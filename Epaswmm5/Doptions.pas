unit Doptions;

{-------------------------------------------------------------------}
{                    Unit:    Doptions.pas                          }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    05/02/22     (5.2.1)                  }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form unit that edits a project's simulation options.     }
{                                                                   }
{   The form consists of a PageControl component with 5 pages,      }
{   one for each category of simulation options.                    }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, System.UITypes, Mask, ComCtrls, Math,  NumEdit,
  Buttons, ImgList, Grids, Uproject, Uglobals, Uutils, UpDnEdit;

type
  TAnalysisOptionsForm = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    OKBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    ModelsGroup: TGroupBox;
    RainfallBox: TCheckBox;
    SnowMeltBox: TCheckBox;
    GroundwaterBox: TCheckBox;
    FlowRoutingBox: TCheckBox;
    WaterQualityBox: TCheckBox;
    RDIIBox: TCheckBox;
    MiscGroup: TGroupBox;
    Label8: TLabel;
    Label13: TLabel;
    AllowPondingBox: TCheckBox;
    MinSlopeEdit: TNumEdit;
    InfilModelsGroup: TRadioGroup;
    RoutingMethodsGroup: TRadioGroup;
    Label6: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label7: TLabel;
    Label21: TLabel;
    Label23: TLabel;
    DryDaysEdit: TNumEdit;
    StartDatePicker: TDateTimePicker;
    RptDatePicker: TDateTimePicker;
    EndDatePicker: TDateTimePicker;
    StartTimePicker: TDateTimePicker;
    RptTimePicker: TDateTimePicker;
    EndTimePicker: TDateTimePicker;
    SweepStartPicker: TDateTimePicker;
    SweepEndPicker: TDateTimePicker;
    Label12: TLabel;
    Label11: TLabel;
    Label24: TLabel;
    Label27: TLabel;
    Label10: TLabel;
    Label9: TLabel;
    Label5: TLabel;
    Label16: TLabel;
    RptStepPicker: TDateTimePicker;
    DryStepPicker: TDateTimePicker;
    WetStepPicker: TDateTimePicker;
    DryDaysPicker: TDateTimePicker;
    WetDaysPicker: TDateTimePicker;
    RptDaysPicker: TDateTimePicker;
    RouteStepEdit: TNumEdit;
    Label29: TLabel;
    MinSurfAreaLabel: TLabel;
    Label15: TLabel;
    HeadTolLabel: TLabel;
    LengthenStepEdit: TNumEdit;
    MinSurfAreaEdit: TNumEdit;
    VarTimeStepBox: TCheckBox;
    HeadTolEdit: TNumEdit;
    VarStepEdit: TUpDnEditBox;
    MaxTrialsEdit: TUpDnEditBox;
    Label25: TLabel;
    FileListBox: TListBox;
    AddBtn: TButton;
    EditBtn: TButton;
    DeleteBtn: TButton;
    GroupBox1: TGroupBox;
    SysFlowTolSpinner: TUpDnEditBox;
    LatFlowTolSpinner: TUpDnEditBox;
    SkipSteadyBox: TCheckBox;
    Label14: TLabel;
    Label19: TLabel;
    MinTimeStepEdit: TNumEdit;
    MinTimeStepLabel: TLabel;
    DefaultsLabel: TLinkLabel;
    Label26: TLabel;
    InertialTermsCombo: TComboBox;
    Label28: TLabel;
    NormalFlowCombo: TComboBox;
    Label30: TLabel;
    ForceMainCombo: TComboBox;
    Label22: TLabel;
    RuleStepPicker: TDateTimePicker;
    Label31: TLabel;
    Label2: TLabel;
    SurchargeCombo: TComboBox;
    ThreadsButton: TSpeedButton;
    Label20: TLabel;
    ThreadsEdit: TUpDown;
    EditBox: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure EditChange(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure AddBtnClick(Sender: TObject);
    procedure EditBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure FileListBoxDblClick(Sender: TObject);
    procedure FileListBoxKeyPress(Sender: TObject; var Key: Char);
    procedure VarTimeStepBoxClick(Sender: TObject);
    procedure DefaultsLabelLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure PageControl1Change(Sender: TObject);
    procedure PageControl1Changing(Sender: TObject; var AllowChange: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ThreadsButtonClick(Sender: TObject);
  private
    { Private declarations }
    OldInfilIndex: Integer;
    OldVarTimeStep: Boolean;
    HasChangedTrue: Boolean;
    NumberOfCPUs: String;
    procedure EditFileSelection(var S: String);
    function  GetStepTime(DaysPicker: TDateTimePicker;
      TimePicker: TDateTimePicker): String;
    procedure SetStepTime(const S: String; DaysPicker: TDateTimePicker;
      TimePicker: TDateTimePicker);

  public
    { Public declarations }
    HasChanged: Boolean;
    procedure SetOptions(Page: Integer);
    procedure GetOptions;
  end;

//var
//  AnalysisOptionsForm: TAnalysisOptionsForm;

implementation

{$R *.dfm}

uses
  Fmain, Diface, Uupdate;

const
  TXT_MIN_SURF_AREA = 'Minimum Nodal Surface Area ';
  TXT_SQUARE_FEET = '(sq. feet)';
  TXT_SQUARE_METERS = '(sq. meters)';
  TXT_HEAD_TOL = 'Head Convergence Tolerance ';
  TXT_TOL_FEET = '(feet)';
  TXT_TOL_METERS = '(meters)';

procedure TAnalysisOptionsForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnCreate handler.
//-----------------------------------------------------------------------------
var
  DateFmt: String;
  M: Integer;
begin
  // Set properties of the interface file list box
  FileListBox.Font.Style := Font.Style;
  FileListBox.ScrollWidth := 10*FileListBox.ClientWidth;

  // Set date format for date picker controls
  DateFmt := 'MM' + MyFormatSettings.DateSeparator + 'dd' +
    MyFormatSettings.DateSeparator + 'yyyy';
  StartDatePicker.Format := DateFmt;
  RptDatePicker.Format := DateFmt;
  EndDatePicker.Format := DateFmt;

  if Uglobals.UnitSystem = usUS then
  begin
    MinSurfAreaLabel.Caption := TXT_MIN_SURF_AREA + TXT_SQUARE_FEET;
    HeadTolLabel.Caption := TXT_HEAD_TOL + TXT_TOL_FEET;
  end
  else
  begin
    MinSurfAreaLabel.Caption := TXT_MIN_SURF_AREA + TXT_SQUARE_METERS;
    HeadTolLabel.Caption := TXT_HEAD_TOL + TXT_TOL_METERS;
  end;

  // Set options for number of threads
  M := Uutils.GetProcessorCount;
  ThreadsEdit.Max := M;
  NumberOfCPUs := Format(
    'This machine has %d physical cores and %d logical processors.',
    [Uutils.GetCoreCount, M]);
end;

procedure TAnalysisOptionsForm.FormShow(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnShow handler.
//-----------------------------------------------------------------------------
begin
  case PageControl1.TabIndex of
  1: ActiveControl := StartDatePicker;
  2: ActiveControl := RptDaysPicker;
  3: ActiveControl := InertialTermsCombo;
  4: ActiveControl := AddBtn;
  end;
  HasChanged := False;
end;

procedure TAnalysisOptionsForm.SetStepTime(const S: String;
  DaysPicker: TDateTimePicker; TimePicker: TDateTimePicker);
//-----------------------------------------------------------------------------
//  Sets the values displayed in a pair of DateTimePicker controls to the
//  hrs:min:sec time step specified by string S, where hrs can be > 23.
//-----------------------------------------------------------------------------
var
  StepTime: TDateTime;
  StepDays: Integer;
begin
  StepTime := Uutils.StrHoursToTime(S);
  StepDays := Floor(StepTime);
  StepTime := StepTime - StepDays;
  DaysPicker.Time := EncodeTime(0, StepDays, 0, 0);
  TimePicker.Time := StepTime;
end;

procedure TAnalysisOptionsForm.ThreadsButtonClick(Sender: TObject);
begin
  MsgDlg(NumberOfCPUs, mtInformation, [mbOK]);
end;

function  TAnalysisOptionsForm.GetStepTime(DaysPicker: TDateTimePicker;
  TimePicker: TDateTimePicker): String;
//-----------------------------------------------------------------------------
//  Converts the contents of a pair of DateTimePicker controls
//  into hrs:min:sec format, where hrs can be > 23.
//-----------------------------------------------------------------------------
var
  Days, Hr, Min, Sec, Msec: Word;
begin
  DecodeTime(DaysPicker.Time, Hr, Days, Sec, Msec);
  DecodeTime(TimePicker.Time, Hr, Min, Sec, Msec);
  Hr := Hr + 24*Days;
  Result := Format('%.2d:%.2d:%.2d', [Hr,Min,Sec]);
end;

procedure TAnalysisOptionsForm.SetOptions(Page: Integer);
//-----------------------------------------------------------------------------
//  Loads current simulation options into form and displays the
//  specified page of the PageControl component.
//-----------------------------------------------------------------------------
var
  S: String;
  I: Integer;
begin
  S := Project.Options.Data[INFILTRATION_INDEX];
  I := Uutils.FindKeyword(S, InfilOptions, 10);
  if I < 0 then I := 0;
  InfilModelsGroup.ItemIndex := I;
  OldInfilIndex := I;

  S := Project.Options.Data[ROUTING_MODEL_INDEX];
  I := Uutils.FindKeyWord(S, RoutingOptions, 3);
  if I < 0 then I := 0;
  RoutingMethodsGroup.ItemIndex := I;
  with Project.Options do
  begin
    StartDatePicker.Date  := StrToDate(Data[START_DATE_INDEX], MyFormatSettings);
    StartTimePicker.Time  := StrToTime(Data[START_TIME_INDEX], MyFormatSettings);
    RptDatePicker.Date    := StrToDate(Data[REPORT_START_DATE_INDEX], MyFormatSettings);
    RptTimePicker.Time    := StrToTime(Data[REPORT_START_TIME_INDEX], MyFormatSettings);
    EndDatePicker.Date    := StrToDate(Data[END_DATE_INDEX], MyFormatSettings);
    EndTimePicker.Time    := StrToTime(Data[END_TIME_INDEX], MyFormatSettings);
    SweepStartPicker.Date := StrToDate(Data[SWEEP_START_INDEX] + '/1947', MyFormatSettings);
    SweepEndPicker.Date   := StrToDate(Data[SWEEP_END_INDEX] + '/1947', MyFormatSettings);
    DryDaysEdit.Text      := Data[DRY_DAYS_INDEX];

    RuleStepPicker.Time := StrToTime(Data[RULE_STEP_INDEX], MyFormatSettings);

    SetStepTime(Data[REPORT_STEP_INDEX], RptDaysPicker, RptStepPicker);
    SetStepTime(Data[DRY_STEP_INDEX], DryDaysPicker, DryStepPicker);
    SetStepTime(Data[WET_STEP_INDEX], WetDaysPicker, WetStepPicker);
    RouteStepEdit.Text := Data[ROUTING_STEP_INDEX];

    SysFlowTolSpinner.Spinner.Position := StrToInt(Data[SYS_FLOW_TOL_INDEX]);
    LatFlowTolSpinner.Spinner.Position := StrToInt(Data[LAT_FLOW_TOL_INDEX]);

    AllowPondingBox.Checked := SameText(Data[ALLOW_PONDING_INDEX], 'YES');
    SkipSteadyBox.Checked := SameText(Data[SKIP_STEADY_INDEX], 'YES');

    if Length(Data[NUM_THREADS_INDEX]) = 0 then ThreadsEdit.Position := 1
    else begin
      I := StrToInt(Data[NUM_THREADS_INDEX]);
      I := MIN(I, ThreadsEdit.Max);
      ThreadsEdit.Position := I;
    end;

    if Project.Lists[RAINGAGE].Count > 0
    then RainfallBox.Checked := not SameText(Data[IGNORE_RAINFALL_INDEX], 'YES')
    else RainfallBox.Enabled := False;
    if (Project.Lists[RAINGAGE].Count > 0)
    and (Project.Lists[HYDROGRAPH].Count > 0)
    then RDIIBox.Checked := not SameText(Data[IGNORE_RDII_INDEX], 'YES')
    else RDIIBox.Enabled := False;
    if Project.Lists[SNOWPACK].Count > 0
    then SnowMeltBox.Checked := not SameText(Data[IGNORE_SNOWMELT_INDEX], 'YES')
    else SnowMeltBox.Enabled := False;
    if Project.Lists[AQUIFER].Count > 0
    then GroundwaterBox.Checked := not SameText(Data[IGNORE_GRNDWTR_INDEX], 'YES')
    else GroundwaterBox.Enabled := False;
    if Project.GetLinkCount > 0
    then FlowRoutingBox.Checked := not SameText(Data[IGNORE_ROUTING_INDEX], 'YES')
    else FlowRoutingBox.Enabled := False;
    if Project.Lists[POLLUTANT].Count > 0
    then WaterQualityBox.Checked := not SameText(Data[IGNORE_QUALITY_INDEX], 'YES')
    else WaterQualityBox.Enabled := False;

    with InertialTermsCombo do
    begin
      if SameText(Data[INERTIAL_DAMPING_INDEX], 'NONE')
      then ItemIndex := 0
      else if Sametext(Data[INERTIAL_DAMPING_INDEX], 'FULL')
      then ItemIndex := 2
      else ItemIndex := 1;
    end;

    with NormalFlowCombo do
    begin
      if SameText(Data[NORMAL_FLOW_LTD_INDEX], 'SLOPE')
      then ItemIndex := 0
      else if SameText(Data[NORMAL_FLOW_LTD_INDEX], 'FROUDE')
      then ItemIndex := 1
      else if SameText(Data[NORMAL_FLOW_LTD_INDEX], 'NONE')
      then ItemIndex := 3
      else ItemIndex := 2;
    end;

    with ForceMainCombo do
    begin
      if SameText(Project.Options.Data[FORCE_MAIN_EQN_INDEX], 'H-W')
      then ItemIndex := 0
      else ItemIndex := 1;
    end;

    with SurchargeCombo do
    begin
      if SameText(Project.Options.Data[SURCHARGE_METHOD_INDEX], 'EXTRAN')
      then ItemIndex := 0
      else ItemIndex := 1;
    end;

    MinTimeStepEdit.Text := Data[MIN_ROUTE_STEP_INDEX];
    I := StrToInt(Data[VARIABLE_STEP_INDEX]);
    if I >= VarStepEdit.Spinner.Min then
    begin
      VarTimeStepBox.Checked := True;
      VarStepEdit.Spinner.Position := I;
      VarStepEdit.Enabled := True;
      MinTimeStepEdit.Enabled := True;
      MinTimeStepLabel.Enabled := True;
    end;
    OldVarTimeStep := VarTimeStepBox.Checked;

    LengthenStepEdit.Text := Data[LENGTHEN_STEP_INDEX];
    MinSlopeEdit.Text := Data[MIN_SLOPE_INDEX];

    if StrToFloatDef(Data[MIN_SURFAREA_INDEX], 0) = 0 then
    begin
      if Uglobals.UnitSystem = Uglobals.usUS
      then MinSurfAreaEdit.Text := Uglobals.DefMinSurfAreaUS
      else MinSurfAreaEdit.Text := Uglobals.DefMinSurfAreaSI;
    end
    else MinSurfAreaEdit.Text := Data[MIN_SURFAREA_INDEX];

    if StrToFloatDef(Data[HEAD_TOL_INDEX], 0) = 0 then
    begin
      if Uglobals.UnitSystem = Uglobals.usUS
      then HeadTolEdit.Text := Uglobals.DefHeadTolUS
      else HeadTolEdit.Text := Uglobals.DefHeadTolSI;
    end
    else HeadTolEdit.Text := Data[HEAD_TOL_INDEX];

    if StrToIntDef(Data[MAX_TRIALS_INDEX], 0) = 0
    then MaxTrialsEdit.Spinner.Position := Uglobals.DefMaxTrials
    else MaxTrialsEdit.Spinner.Position := StrToInt(Data[MAX_TRIALS_INDEX]);

  end;

  for I := 0 to Project.IfaceFiles.Count-1 do
    FileListBox.Items.Add(Project.IfaceFiles[I]);
  if FileListBox.Items.Count = 0 then
  begin
    EditBtn.Enabled := False;
    DeleteBtn.Enabled := False;
  end
  else FileListBox.ItemIndex := 0;

  HasChanged := False;
  if Page < 0 then Page := 0;
  if Page >= PageControl1.PageCount
  then Page := PageControl1.PageCount - 1;
  PageControl1.TabIndex := Page;
end;

procedure TAnalysisOptionsForm.GetOptions;
//-----------------------------------------------------------------------------
//  Unloads current contents of form into project's simulation options.
//-----------------------------------------------------------------------------
var
  I: Integer;
  Yr, Mon, Day: Word;
begin
  if  InfilModelsGroup.ItemIndex <> OldInfilIndex then
  begin
    Project.Options.Data[INFILTRATION_INDEX] :=
      InfilOptions[InfilModelsGroup.ItemIndex];
    case InfilModelsGroup.ItemIndex of
    HORTON_INFIL, MOD_HORTON_INFIL:
       CopyStringArray(DefHortonInfil, Uproject.DefInfil);
    GREEN_AMPT_INFIL, MOD_GREEN_AMPT_INFIL:
       CopyStringArray(DefGreenAmptInfil, Uproject.DefInfil);
    CURVE_NUMBER_INFIL:
        CopyStringArray(DefCurveNumInfil, Uproject.DefInfil);
    end;
    Uupdate.UpdateInfilModel(InfilOptions[OldInfilIndex],
      Project.Options.Data[INFILTRATION_INDEX]);
  end;

  Project.Options.Data[ROUTING_MODEL_INDEX] :=
     RoutingOptions[RoutingMethodsGroup.ItemIndex];

  with Project.Options do
  begin
    Data[START_DATE_INDEX] := DateToStr(StartDatePicker.Date, MyFormatSettings);
    Data[START_TIME_INDEX] := TimeToStr(StartTimePicker.Time, MyFormatSettings);
    Data[REPORT_START_DATE_INDEX] := DateToStr(RptDatePicker.Date, MyFormatSettings);
    Data[REPORT_START_TIME_INDEX] := TimeToStr(RptTimePicker.Time, MyFormatSettings);
    Data[END_DATE_INDEX] := DateToStr(EndDatePicker.Date, MyFormatSettings);
    Data[END_TIME_INDEX] := TimeToStr(EndTimePicker.Time, MyFormatSettings);

    DecodeDate(SweepStartPicker.Date, Yr, Mon, Day);
    Data[SWEEP_START_INDEX] := Format('%.2d/%.2d', [Mon, Day]);
    DecodeDate(SweepEndPicker.Date, Yr, Mon, Day);
    Data[SWEEP_END_INDEX] := Format('%.2d/%.2d', [Mon, Day]);

    Data[DRY_DAYS_INDEX] := DryDaysEdit.Text;

    Data[RULE_STEP_INDEX] := TimeToStr(RuleStepPicker.Time, MyFormatSettings);
    Data[REPORT_STEP_INDEX] := GetStepTime(RptDaysPicker, RptStepPicker);
    Data[DRY_STEP_INDEX] := GetStepTime(DryDaysPicker, DryStepPicker);
    Data[WET_STEP_INDEX] := GetStepTime(WetDaysPicker, WetStepPicker);
    Data[ROUTING_STEP_INDEX] := RouteStepEdit.Text;
    Data[SYS_FLOW_TOL_INDEX] := SysFlowTolSpinner.EditBox.Text;
    Data[LAT_FLOW_TOL_INDEX] := LatFlowTolSpinner.EditBox.Text;

    if AllowPondingBox.Checked then
      Data[ALLOW_PONDING_INDEX] := 'YES'
    else
      Data[ALLOW_PONDING_INDEX] := 'NO';

    if SkipSteadyBox.Checked then
      Data[SKIP_STEADY_INDEX] := 'YES'
    else
      Data[SKIP_STEADY_INDEX] := 'NO';

    if RainfallBox.Enabled then
      if not RainfallBox.Checked then
        Data[IGNORE_RAINFALL_INDEX] := 'YES'
      else
        Data[IGNORE_RAINFALL_INDEX] := 'NO';

    if RDIIBox.Enabled then
      if not RDIIBox.Checked then
        Data[IGNORE_RDII_INDEX] := 'YES'
      else
        Data[IGNORE_RDII_INDEX] := 'NO';

    if SnowMeltBox.Enabled then
      if not SnowMeltBox.Checked then
        Data[IGNORE_SNOWMELT_INDEX] := 'YES'
      else
        Data[IGNORE_SNOWMELT_INDEX] := 'NO';

    if GroundwaterBox.Enabled then
      if not GroundwaterBox.Checked then
        Data[IGNORE_GRNDWTR_INDEX] := 'YES'
      else
        Data[IGNORE_GRNDWTR_INDEX] := 'NO';

    if FlowRoutingBox.Enabled then
      if not FlowRoutingBox.Checked then
        Data[IGNORE_ROUTING_INDEX] := 'YES'
      else
        Data[IGNORE_ROUTING_INDEX] := 'NO';

    if WaterQualityBox.Enabled then
      if not WaterQualityBox.Checked then
        Data[IGNORE_QUALITY_INDEX] := 'YES'
      else
        Data[IGNORE_QUALITY_INDEX] := 'NO';

    with InertialTermsCombo do
    begin
      case ItemIndex of
      0: Data[INERTIAL_DAMPING_INDEX] := 'NONE';
      1: Data[INERTIAL_DAMPING_INDEX] := 'PARTIAL';
      2: Data[INERTIAL_DAMPING_INDEX] := 'FULL';
      end;
    end;

    with NormalFlowCombo do
    begin
      case ItemIndex of
      0: Data[NORMAL_FLOW_LTD_INDEX] := 'SLOPE';
      1: Data[NORMAL_FLOW_LTD_INDEX] := 'FROUDE';
      2: Data[NORMAL_FLOW_LTD_INDEX] := 'BOTH';
      3: Data[NORMAL_FLOW_LTD_INDEX] := 'NONE';
      end;
    end;

    with ForceMainCombo do
    begin
      case ItemIndex of
      0: Data[FORCE_MAIN_EQN_INDEX] := 'H-W';
      1: Data[FORCE_MAIN_EQN_INDEX] := 'D-W';
      end;
    end;

    with SurchargeCombo do
    begin
      case ItemIndex of
      0: Data[SURCHARGE_METHOD_INDEX] := 'EXTRAN';
      1: Data[SURCHARGE_METHOD_INDEX] := 'SLOT';
      end;
    end;

    if VarTimeStepBox.Checked then
    begin
      Data[VARIABLE_STEP_INDEX] := IntToStr(VarStepEdit.Spinner.Position);
      Data[MIN_ROUTE_STEP_INDEX] := MinTimeStepEdit.Text;
    end
    else
      Data[VARIABLE_STEP_INDEX] := '0';

    Data[LENGTHEN_STEP_INDEX] := LengthenStepEdit.Text;
    Data[MIN_SURFAREA_INDEX] := MinSurfAreaEdit.Text;
    Data[MIN_SLOPE_INDEX] := MinSlopeEdit.Text;
    Data[MAX_TRIALS_INDEX] := IntToStr(MaxTrialsEdit.Spinner.Position);
    Data[HEAD_TOL_INDEX] := HeadTolEdit.Text;
    Data[NUM_THREADS_INDEX] := IntToStr(ThreadsEdit.Position);
  end;

  Project.IfaceFiles.Clear;
  with FileListBox do
  begin
    for I := 0 to Items.Count-1 do Project.IfaceFiles.Add(Items[I]);
  end;
  Uupdate.UpdateDefOptions;
end;

procedure TAnalysisOptionsForm.VarTimeStepBoxClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the Variable Time Step check box.
//-----------------------------------------------------------------------------
begin
  VarStepEdit.Enabled := VarTimeStepBox.Checked;
  MinTimeStepEdit.Enabled := VarStepEdit.Enabled;
  MinTimeStepLabel.Enabled := VarStepEdit.Enabled;
end;

procedure TAnalysisOptionsForm.OKBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the OK button.
//-----------------------------------------------------------------------------
begin
  if VarTimeStepBox.Checked <> OldVarTimeStep then HasChanged := True;
  ModalResult := mrOK;
end;

//------------------------------------------------------------------------------
//  OnChanging and OnChange handlers for the form's tabbed Page Control.
//  (Prevents the HasChanged flag from being set to True after OnChange
//  events for controls on a user-selected page fire when they get redrawn).
//------------------------------------------------------------------------------

procedure TAnalysisOptionsForm.PageControl1Changing(Sender: TObject;
  var AllowChange: Boolean);
begin
  // Save the current value of the HasChanged flag
  HasChangedTrue := HasChanged;
end;

procedure TAnalysisOptionsForm.PageControl1Change(Sender: TObject);
begin
  // Restore the OnChange flag to its value before page was changed
  HasChanged := HasChangedTrue;
end;

procedure TAnalysisOptionsForm.EditChange(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnChange handler for most of form's controls.
//-----------------------------------------------------------------------------
begin
  HasChanged := True;
end;

procedure TAnalysisOptionsForm.AddBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the Interface File Add button.
//-----------------------------------------------------------------------------
var
  S: String;
begin
  S := '';
  EditFileSelection(S);
  if Length(S) > 0 then
  begin
    FileListBox.Items.Add(S);
    FileListBox.ItemIndex := FileListBox.Items.Count-1;
    FileListBox.SetFocus;
    EditBtn.Enabled := True;
    DeleteBtn.Enabled := True;
    HasChanged := True;
  end;
end;

procedure TAnalysisOptionsForm.EditBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the Interface File Edit button.
//-----------------------------------------------------------------------------
var
  S: String;
begin
  with FileListBox do
  begin
    S := Items[ItemIndex];
    EditFileSelection(S);
    if Length(S) > 0 then
    begin
      Items[ItemIndex] := S;
      HasChanged := True;
    end;
    SetFocus;
  end;
end;

procedure TAnalysisOptionsForm.DefaultsLabelLinkClick(Sender: TObject;
  const Link: string; LinkType: TSysLinkType);
//-----------------------------------------------------------------------------
//  OnClick handler for the Apply Defaults label.
//-----------------------------------------------------------------------------
begin
  InertialTermsCombo.ItemIndex := 1;
  NormalFlowCombo.ItemIndex := 2;
  ForceMainCombo.ItemIndex := 0;
  SurchargeCombo.ItemIndex := 0;
  VarTimeStepBox.Checked := True;
  VarStepEdit.Spinner.Position := 75;
  MinTimeStepEdit.Text := '0.5';
  LengthenStepEdit.Text := '0';
  if Uglobals.UnitSystem = usUS
  then MinSurfAreaEdit.Text := Uglobals.DefMinSurfAreaUS
  else MinSurfAreaEdit.Text := Uglobals.DefMinSurfAreaSI;
  MaxTrialsEdit.Spinner.Position := 8;
  if Uglobals.UnitSystem = usUS
  then HeadTolEdit.Text := Uglobals.DefHeadTolUS
  else HeadTolEdit.Text := Uglobals.DefHeadTolSI;
  ThreadsEdit.Position := 1;
end;

procedure TAnalysisOptionsForm.DeleteBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the Interface File Delete button.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  with FileListBox do
  begin
    I := ItemIndex;
    if ItemIndex >= 0 then
    begin
      Items.Delete(ItemIndex);
      HasChanged := True;
    end;
    if Items.Count = 0 then
    begin
      ItemIndex := -1;
      AddBtn.SetFocus;
      EditBtn.Enabled := False;
      DeleteBtn.Enabled := False;
    end
    else
    begin
      if I >= Items.Count then ItemIndex := Items.Count-1
      else ItemIndex := I;
      SetFocus;
    end;
  end;
end;

procedure TAnalysisOptionsForm.FileListBoxDblClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnDblClick handler for the Interface File list box.
//-----------------------------------------------------------------------------
begin
  if EditBtn.Enabled then EditBtnClick(Sender);
end;

procedure TAnalysisOptionsForm.FileListBoxKeyPress(Sender: TObject;
  var Key: Char);
//-----------------------------------------------------------------------------
//  OnKeyPress handler for the Interface File list box.
//-----------------------------------------------------------------------------
begin
  if (Key = #13) and EditBtn.Enabled then EditBtnClick(Sender);
end;

procedure TAnalysisOptionsForm.EditFileSelection(var S: String);
//-----------------------------------------------------------------------------
//  Launches the Interface File selection dialog form.
//-----------------------------------------------------------------------------
var
  IfaceFileForm: TIfaceFileForm;
begin
  IfaceFileForm := TIfaceFileForm.Create(self);
  with IfaceFileForm do
  try
    SetData(S);
    if ShowModal = mrOK then GetData(S);
  finally
    Free;
  end;
end;

procedure TAnalysisOptionsForm.HelpBtnClick(Sender: TObject);
begin
  with PageControl1 do
    if ActivePage = TabSheet1 then
       Application.HelpCommand(HELP_CONTEXT, 211200)
    else if ActivePage = TabSheet2 then
       Application.HelpCommand(HELP_CONTEXT, 211210)
    else if ActivePage = TabSheet3 then
       Application.HelpCommand(HELP_CONTEXT, 211220)
    else if ActivePage = TabSheet4 then
       Application.HelpCommand(HELP_CONTEXT, 211230)
    else if ActivePage = TabSheet5 then
       Application.HelpCommand(HELP_CONTEXT, 212080);
end;

procedure TAnalysisOptionsForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then HelpBtnClick(Sender);
end;

end.
