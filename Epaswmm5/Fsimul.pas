unit Fsimul;

{-------------------------------------------------------------------}
{                    Unit:    Fsimul.pas                            }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    05/02/22    (5.2.1)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Form unit used to execute the SWMM simulation engine            }
{   (contained in  SWMM5.DLL) and display its progress.             }
{                                                                   }
{   The form contains a Notebook component with two pages - a       }
{   ProgressPage and a ResultsPage. The ProgressPage has a          }
{   ProgressBar that displays the progress of a simulation          }
{   while the ResultsPage displays the continuity error when        }
{   a simulation is successfully completed.                         }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Consts, Math, Gauges, DateUtils,
  Swmm5, Uproject, Uglobals, Uutils;

type
  TSimulationForm = class(TForm)
    Notebook1: TNotebook;
    ProgressLabel: TLabel;
    StopBtn: TButton;
    OKBtn: TButton;
    StatusLabel: TLabel;
    ErrorBox: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ErrLabel1: TLabel;
    ErrLabel2: TLabel;
    ErrLabel3: TLabel;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    PcntCompleteLabel: TLabel;
    Label6: TLabel;
    MinimizeBtn: TButton;
    ProgressBar1: TProgressBar;
    PcntValueLabel: TLabel;
    DaysPanel: TPanel;
    HoursPanel: TPanel;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure OKbtnClick(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
    procedure MinimizeBtnClick(Sender: TObject);
  private
    { Private declarations }
    procedure DisplayRunStatus;
    procedure Execute;
    function  GetDuration:Double;
    procedure RunSimulation;
    procedure UpdateProgressDisplay(ElapsedTime, Duration: Double);
  public
    { Public declarations }
  end;

var
  SimulationForm: TSimulationForm;   // Don't comment this out!

implementation

{$R *.DFM}

uses
  Fmain, Uexport, Uoutput;

const
  TXT_SWMM5 = 'SWMM 5 - ';
  TXT_COMPLETE = ' complete';
  TXT_STATUS_NONE = 'Unable to run simulator.';
  TXT_STATUS_WRONGVERSION = 'Run was unsuccessful.'#10'Wrong version of simulator.';
  TXT_STATUS_FAILED = 'Run was unsuccessful due to system error.';
  TXT_STATUS_ERROR =
    'Run was unsuccessful.'#10'See Status Report for reasons.';
  TXT_STATUS_WARNING =
    'Run was successful with warnings.'#10'See Status Report for details.';
  TXT_STATUS_SUCCESS = 'Run was successful.';
  TXT_STATUS_SHUTDOWN =
   'Simulator performed an illegal operation and was shut down.';
  TXT_STATUS_STOPPED = 'Run was successful but was stopped before completion.';

  TXT_SAVING = 'Saving project data ...';
  TXT_READING = 'Reading project data ...';
  TXT_CHECKING = 'Checking project data ...';
  TXT_COMPUTING = 'Computing ...';
  TXT_CONTINUITY_ERROR = 'Continuity Error';
  TXT_SURF_RUNOFF =  'Surface Runoff:';
  TXT_FLOW_ROUTING = 'Flow Routing:';
  TXT_QUAL_ROUTING = 'Quality Routing:';

  SHORT_TERM_LIMIT = 20;               //Simulation duration (in days) that
                                       //defines a short-term simulation
var
  ErrRunoff: Single;                   //Runoff continuity error
  ErrFlow: Single;                     //Flow routing continuity error
  ErrQual: Single;                     //Quality routing continuity error
  OldDays: LongInt;                    //Old elapsed number of days
  Warnings: Integer;
  Activated: Boolean;                  //True if form has been activated

procedure TSimulationForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
// OnCreate handler for form
//-----------------------------------------------------------------------------
begin
  // Initialize display of continuity errors
  ErrRunoff := 0;
  ErrFlow := 0;
  ErrQual := 0;
  ErrLabel1.Caption := '';
  ErrLabel2.Caption := '';
  ErrLabel3.Caption := '';
  Label1.Caption := '';
  Label2.Caption := '';
  Label3.Caption := '';
  ErrorBox.Caption := '';
  ErrorBox.Visible := False;

  // Initialize placement and status of images on the ResultsPage
  // (Image1 is the information icon and Image2 the error icon)
  Image1.Visible := False;
  Image2.Visible := False;
  Image2.Left := Image1.Left;
  Image2.Top  := Image1.Top;

  // Place the label displaying the percent complete value
  // to the right of the Percent Complete label
  PcntValueLabel.Left := PcntCompleteLabel.Left +
                         PcntCompleteLabel.Width + 4;

  // Make the ProgressPage be the active page
  Notebook1.PageIndex := 0;
  Activated := false;
end;


procedure TSimulationForm.FormActivate(Sender: TObject);
//-----------------------------------------------------------------------------
// OnActivate handler for the form. Executes a simulation when the form
// first appears.
//-----------------------------------------------------------------------------
begin
  if not Activated then
  begin
    Activated := true;
    Execute;
  end;
end;


procedure TSimulationForm.OKbtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick procedure for the OK button.
//-----------------------------------------------------------------------------
begin
  Hide;
  ModalResult := mrOK;
end;


procedure TSimulationForm.StopBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick procedure for the Stop button.
//-----------------------------------------------------------------------------
begin
  RunStatus := rsStopped;
end;


procedure TSimulationForm.MinimizeBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick procedure for the Minimize button.
//-----------------------------------------------------------------------------
begin
  Application.Minimize;
end;


procedure TSimulationForm.Execute;
//-----------------------------------------------------------------------------
// Executes the steps needed to run a simulation.
//-----------------------------------------------------------------------------
var
  OldDir: String;
  AppTitle: String;
begin
  // Change the current directory to the application's temporary directory
  GetDir(0,OldDir);
  ChDir(TempDir);

  // Update the form's display
  Update;

  // Save the title Windows' uses for the application
  AppTitle := Application.Title;

  // Run the simulation
  RunSimulation;

  // Restore the application's title
  Application.Title := AppTitle;

  // Change back to the original directory
  ChDir(OldDir);

  // Display the run's status on the ResultsPage of the form
  DisplayRunStatus;
  Notebook1.PageIndex := 1;
end;


procedure TSimulationForm.RunSimulation;
//-----------------------------------------------------------------------------
// Makes calls to the SWMM DLL engine to perform a simulation.
// The input, report, and binary output files required by the SWMM
// engine have already been created in Fmain.pas' RunSimulation
// procedure through a call to CreateTempFiles.
//-----------------------------------------------------------------------------
var
  Err: Integer;                        // error code (0 = no error)
  S: TStringlist;                      // stringlist used for input data
  Duration: double;                    // simulation duration in days
  ElapsedTime: double;                 // elapsed simulation time in days
  OldTime, NewTime: TDateTime;         // system times for progress meter
  InpFile, RptFile, OutFile: AnsiString;  // Ansi string versions of file names

begin
  // Save the current project input data to a temporary file
  ProgressLabel.Caption := TXT_SAVING;
  ProgressLabel.Refresh;
  S := TStringlist.Create;             // Input will be placed in a stringlist
  try
    Uexport.ExportProject(S, '');      // Write input data to the stringlist
    Uexport.ExportTempDir(S);          // Add temp. directory name to input
    S.SaveToFile(TempInputFile);       // Save input to file
  finally
    S.Free;
  end;

  // Have the SWMM solver read the input data file
  ProgressLabel.Caption := TXT_READING;
  ProgressLabel.Refresh;
  InpFile := AnsiString(TempInputFile);
  RptFile := AnsiString(TempReportFile);
  OutFile := AnsiString(TempOutputFile);
  Err := swmm_open(PAnsiChar(InpFile),PAnsiChar(RptFile), PAnsiChar(OutFile));

  // If there are no input errors, then initialize the simulation
  if Err = 0 then
  begin
    ProgressLabel.Caption := TXT_CHECKING;
    ProgressLabel.Refresh;
    Err := swmm_start(1);
  end;

  // If there are no initialization errors, then...
  if Err = 0 then
  begin

    // Get the simulation duration in days
    OldDays := 1;
    Duration := GetDuration;

    // Gray-out the Hrs:Min display for long-term simulations
    if Duration >= SHORT_TERM_LIMIT then
    begin
      Panel2.Font.Color := clGrayText;
      DaysPanel.Caption := '0';
      HoursPanel.Caption := '';
    end;
    ProgressLabel.Caption := TXT_COMPUTING;

    // Step through each time period until there is no more time left,
    // an error occurs, or the user stops the run
    OldTime := Time;
    repeat
      Application.ProcessMessages;
      Err := swmm_step(ElapsedTime);
      NewTime := Time;
      if MilliSecondsBetween(NewTime, OldTime) > 100 then
      begin
        UpdateProgressDisplay(ElapsedTime, Duration);
        OldTime := NewTime;
      end;
    until (ElapsedTime = 0) or (Err > 0) or (RunStatus = rsStopped);

    // End the simulation and retrieve mass balance errors
    swmm_end;
    swmm_getMassBalErr(ErrRunoff, ErrFlow, ErrQual);
    Warnings := swmm_getWarnings();
  end;

  // Close the SWMM solver
  swmm_close;
end;


function TSimulationForm.GetDuration: Double;
//-----------------------------------------------------------------------------
// Computes the simulation duration in days from the Project's simulation
// options.
//-----------------------------------------------------------------------------
begin
   try
     Result := StrToDate(Project.Options.Data[END_DATE_INDEX], MyFormatSettings) +
               StrToTime(Project.Options.Data[END_TIME_INDEX], MyFormatSettings) -
               StrToDate(Project.Options.Data[START_DATE_INDEX], MyFormatSettings) -
               StrToTime(Project.Options.Data[START_TIME_INDEX], MyFormatSettings);
   except
     on E: Exception do Result := 0;
   end;
end;


procedure TSimulationForm.UpdateProgressDisplay(ElapsedTime, Duration: Double);
//-----------------------------------------------------------------------------
// Updates the percent complete progress bar and the elapsed time displays.
//-----------------------------------------------------------------------------
var
  NewPosition: Integer;
  TimeString: String;

begin
  if Duration > 0 then
  begin

    // Find the new percent completed value (as an integer)
    NewPosition := Floor(ElapsedTime/Duration*100);

    // If this value is greater than the current progress bar position
    if NewPosition > ProgressBar1.Position then
    begin

      // Update the progress bar position
      ProgressBar1.Position := NewPosition;

      // Update the numerical display of the percent completed
      PcntValueLabel.Caption := IntToStr(NewPosition) + '%';

      // Update the SWMM5 application's title (which is what gets
      // displayed in the task bar when the application is minimized)
      Application.Title := TXT_SWMM5 + PcntValueLabel.Caption + TXT_COMPLETE;
    end;
  end;

  // Update the elapsed days display for long-term simulations
  if Duration >= SHORT_TERM_LIMIT then
  begin
    if ElapsedTime >= OldDays then
    begin
      DaysPanel.Caption := IntToStr(OldDays);
      OldDays := Round(ElapsedTime);
    end;
  end

  // Or update the elapsed days and time display for shorter simulations
  else
  begin
    DateTimeToString(TimeString, 'hh:nn', TDateTime(ElapsedTime), MyFormatSettings);
    DaysPanel.Caption := IntToStr(Floor(ElapsedTime));
    HoursPanel.Caption := TimeString;
  end;
end;


procedure TSimulationForm.DisplayRunStatus;
//-----------------------------------------------------------------------------
// Displays the final status of the simulation run.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  // Determine what the final run status is
  if not (RunStatus in [rsShutdown]) then
  begin
    if GetFileSize(TempReportFile) <= 0 then RunStatus := rsFailed
    else RunStatus := Uoutput.CheckRunStatus(TempOutputFile);
  end;
  if (RunStatus = rsSuccess) and (Warnings > 0) then RunStatus := rsWarning;

  // Display the appropriate run status message
  with StatusLabel do case RunStatus of
    rsShutdown:     Caption := TXT_STATUS_SHUTDOWN;
    rsNone:         Caption := TXT_STATUS_NONE;
    rsWrongVersion: Caption := TXT_STATUS_WRONGVERSION;
    rsFailed:       Caption := TXT_STATUS_FAILED;
    rsError:        Caption := TXT_STATUS_ERROR;
    rsWarning:      Caption := TXT_STATUS_WARNING;
    rsSuccess:      Caption := TXT_STATUS_SUCCESS;
    rsStopped:      Caption := TXT_STATUS_STOPPED;
  end;

  // Display mass balance errors if results are available
  if (RunStatus = rsWarning) or (RunStatus = rsSuccess) then
  begin
    // Display the information icon
    Image1.Visible := True;
    ErrorBox.Caption := TXT_CONTINUITY_ERROR;
    ErrorBox.Visible := True;
    I := 0;

    // Runoff continuity error
    if ErrRunoff <> 0.0 then
    begin
      Inc(I);
      with FindComponent('Label' + IntToStr(I)) as TLabel do
        Caption := TXT_SURF_RUNOFF;
      with FindComponent('ErrLabel' + IntToStr(I)) as TLabel do
        Caption := Format('%7.2f %%', [ErrRunoff]);
    end;

    // Flow routing continuity error
    if ErrFlow <> 0.0 then
    begin
      Inc(I);
      with FindComponent('Label' + IntToStr(I)) as TLabel do
        Caption := TXT_FLOW_ROUTING;
      with FindComponent('ErrLabel' + IntToStr(I)) as TLabel do
        Caption := Format('%7.2f %%', [ErrFlow]);
    end;

    // Quality routing continuity error
    if ErrQual <> 0.0 then
    begin
      Inc(I);
      with FindComponent('Label' + IntToStr(I)) as TLabel do
        Caption := TXT_QUAL_ROUTING;
      with FindComponent('ErrLabel' + IntToStr(I)) as TLabel do
        Caption := Format('%7.2f %%', [ErrQual]);
    end;
  end

  // If no results are available then display the error icon
  else
    Image2.Visible := True;
end;

end.
