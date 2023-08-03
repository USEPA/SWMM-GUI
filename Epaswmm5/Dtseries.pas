unit Dtseries;

{-------------------------------------------------------------------}
{                    Unit:    Dtseries.pas                          }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form unit for editing a Time Series object.              }
{-------------------------------------------------------------------}

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, Grids, ExtCtrls, ClipBrd,
  Menus, GridEdit, NumEdit, Uglobals, Uutils, Uproject;

type
  TTimeseriesForm = class(TForm)
    Label1: TLabel;
    SeriesName: TEdit;
    Comment: TEdit;
    BtnOK: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    BtnView: TButton;
    GridEdit: TGridEditFrame;
    Label2: TLabel;
    Label3: TLabel;
    EditBtn: TBitBtn;
    FileNameEdit: TEdit;
    FindFileBtn: TBitBtn;
    UseTableCheckBox: TCheckBox;
    UseFileCheckBox: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure SeriesNameChange(Sender: TObject);
    procedure SeriesNameKeyPress(Sender: TObject; var Key: Char);
    procedure BtnHelpClick(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure BtnViewClick(Sender: TObject);
    procedure EditBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure UseFileCheckBoxClick(Sender: TObject);
    procedure UseTableCheckBoxClick(Sender: TObject);
    procedure FindFileBtnClick(Sender: TObject);
  private
    { Private declarations }
    SeriesIndex: Integer;
    FileDir: String;
    function  TimeOrdered: Boolean;
    function  ValidateData: Boolean;
  public
    { Public declarations }
    Modified: Boolean;
    procedure SetData(const Index: Integer; const S: String;
              aSeries: TTimeseries);
    procedure GetData(var S: String; aSeries: TTimeseries);
  end;

implementation

{$R *.DFM}

uses
  Dprevplot, Fmain, Uedit;

const
  MAXTIMES = 1000;

  TXT_DESCRIPTION = 'Time Series Description';
  TXT_DATE = 'Date'#13'(M/D/Y)';
  TXT_TIME = 'Time'#13'(H:M)';
  TXT_VALUE = #13'Value';
  TXT_OPEN_TIMESERIES_TITLE = 'Open a Time Series';
  TXT_SAVE_TIMESERIES_TITLE = 'Save Time Series As';
  TXT_TIMESERIES_FILTER = 'Time series files (*.DAT)|*.DAT|All files|*.*';
  TXT_TIMESERIES_HEADER = 'EPASWMM Time Series Data';
  TXT_TIME_SERIES = 'Time Series ';

  MSG_NO_ID = 'No time series name supplied.';
  MSG_DUPLICATE_ID = 'Time series name already in use.';
  MSG_OUT_OF_ORDER = 'Time values are out of order.';
  MSG_ILLEGAL_NUMBER = 'Illegal numeric value.';
  MSG_NO_FILE = 'External data file does not exist.';

procedure TTimeseriesForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
// OnCreate handler for form.
//-----------------------------------------------------------------------------
begin
  FileDir := ProjectDir;

  // Set up GridEdit control used for time series data
  GridEdit.Visible := False;
  GridEdit.CenterHeaders := True;
  with GridEdit.Grid do
  begin
    Ctl3d := False;
    RowCount := MAXTIMES + 1;
    DefaultRowHeight := GridEdit.EditBox.Height;
    RowHeights[0] := 3 * (-Font.Height);
    ColCount := 3;
    ColWidths[0] := (ClientWidth - 3) div 3;
    ColWidths[1] := ColWidths[0];
    ColWidths[2] := ColWidths[0];
    Cells[0,0] := TXT_DATE;
    Cells[1,0] := TXT_TIME;
    Cells[2,0] := TXT_VALUE;
  end;

  with MainForm.ProjectImageList do
  begin
    GetBitmap(GetIndexByName('edit'), EditBtn.Glyph);
    GetBitmap(GetIndexByName('browse'), FindFileBtn.Glyph);
  end;
  FindFileBtn.Visible := False;

  // Make the time series name edit box the active control
  ActiveControl := SeriesName;
end;

procedure TTimeseriesForm.UseFileCheckBoxClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the UseFile button.
//-----------------------------------------------------------------------------
var
  IsChecked: Boolean;
begin
  IsChecked := UseFileCheckBox.Checked;
  UseTableCheckBox.Checked := not IsChecked;
  FileNameEdit.Enabled := IsChecked;
  FindFileBtn.Visible := IsChecked;
  GridEdit.Enabled := not IsChecked;
  BtnView.Enabled := not IsChecked;
  if UseTableCheckBox.Checked then ActiveControl := UseTableCheckBox;
end;

procedure TTimeseriesForm.UseTableCheckBoxClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the DirectInput button.
//-----------------------------------------------------------------------------
var
  IsChecked: Boolean;
begin
  IsChecked := UseTableCheckBox.Checked;
  UseFileCheckBox.Checked := not IsChecked;
  FileNameEdit.Enabled := not IsChecked;
  FindFileBtn.Visible := not IsChecked;
  GridEdit.Enabled := IsChecked;
  BtnView.Enabled := IsChecked;
  if UseFileCheckBox.Checked then ActiveControl := UseFileCheckBox;
end;

procedure TTimeseriesForm.SetData(const Index: Integer;
   const S: String; aSeries: TTimeseries);
//-----------------------------------------------------------------------------
// Loads data for the time series being edited into the form.
// Index = index of time series, S = time series name, aSeries = time
// series object.
//-----------------------------------------------------------------------------
var
  I: Integer;
  Imax: Integer;
begin
  // Change mouse cursor to hourglass
  Screen.Cursor := crHourglass;
  Application.ProcessMessages;

  // Save index of time series
  SeriesIndex := Index;

  // Place time series data in the form's controls
  SeriesName.Text := S;
  Comment.Text := aSeries.Comment;
  if Length(aSeries.Filename) > 0 then
  begin
    FileNameEdit.Text := aSeries.Filename;
    UseFileCheckBox.Checked := True;
    FileNameEdit.Enabled := True;
    FindFileBtn.Visible := True;
    GridEdit.Enabled := False;
    BtnView.Enabled := False;
  end
  else with aSeries, GridEdit.Grid do
  begin
    Imax := Values.Count;
    if Imax > MAXTIMES then RowCount := Imax + 1;
    for I := 1 to Imax do
    begin
      Cells[0,I] := Dates[I-1];
      Cells[1,I] := Times[I-1];
      Cells[2,I] := Values[I-1];
    end;
  end;
  ActiveControl := SeriesName;

  // Set properties of the GridEdit control
  GridEdit.AllowInsert := True;
  GridEdit.Modified := False;
  GridEdit.EditBox.Style := esNoSpace;
  Modified := False;
  GridEdit.Visible := True;

  // Restore mouse cursor to normal shape
  Screen.Cursor := crDefault;
  Application.ProcessMessages;
end;

procedure TTimeseriesForm.GetData(var S: String; aSeries: TTimeseries);
//-----------------------------------------------------------------------------
// Unloads new data from the form into the time series being edited.
// S = edited time series name, aSeries = time series object.
//-----------------------------------------------------------------------------
var
  I: Integer;
  S0, S1, S2: String;
begin
  S := SeriesName.Text;
  if not Modified then Exit;
  aSeries.Comment := Comment.Text;

  if UseFileCheckBox.Checked then
  begin
    S0 := Trim(FileNameEdit.Text);
    if not SameText(S0, aSeries.Filename) then Modified := True;
    aSeries.Filename := S0;
    Exit;
  end
  else
  begin
    if Length(aSeries.Filename) > 0 then Modified := True;
    aSeries.Filename := '';
  end;

  aSeries.Dates.Clear;
  aSeries.Times.Clear;
  aSeries.Values.Clear;
  with GridEdit.Grid do
    begin
    for I := 1 to RowCount-1 do
      begin
      S0 := Trim(Cells[0,I]);
      S1 := Trim(Cells[1,I]);
      S2 := Trim(Cells[2,I]);
      if (Length(S1) > 0) and (Length(S2) > 0) then
        begin
        aSeries.Dates.Add(S0);
        aSeries.Times.Add(S1);
        aSeries.Values.Add(S2);
        end;
      end;
    end;
end;

procedure TTimeseriesForm.SeriesNameChange(Sender: TObject);
//-----------------------------------------------------------------------------
// OnChange handler for the SeriesName edit box.
//-----------------------------------------------------------------------------
begin
  Modified := True;
end;

procedure TTimeseriesForm.SeriesNameKeyPress(Sender: TObject; var Key: Char);
//-----------------------------------------------------------------------------
// OnKeyPress handler for the SeriesName edit box.
// Prevents user from entering a space character
//-----------------------------------------------------------------------------
begin
  if (Key = ' ') or (Key = '"') or (Key = ';') then Key := #0
  else with SeriesName as TEdit do
  begin
    if (Length(Text) = 0) or (SelStart = 0) then
      if Key = '[' then Key := #0;
  end;
end;

procedure TTimeseriesForm.EditBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the Edit button. Launches a comment editor dialog.
//-----------------------------------------------------------------------------
var
  S: String;
  Modified: Boolean;
begin
  S := Comment.Text;
  Uedit.EditComment(TXT_DESCRIPTION, S, Modified);
  if Modified then Comment.Text := S;
end;

procedure TTimeseriesForm.BtnOKClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the OK button.
//-----------------------------------------------------------------------------
begin
  if not Modified then Modified := GridEdit.Modified;
  if not ValidateData then ModalResult := mrNone
  else ModalResult := mrOK;
end;

function TTimeseriesForm.ValidateData: Boolean;
//-----------------------------------------------------------------------------
//  Validates data entered into the form.
//-----------------------------------------------------------------------------
var
  S : String;
  I : Integer;
begin
  // Check for no time series name
  S := Trim(SeriesName.Text);
  if Length(S) = 0 then
    begin
    Uutils.MsgDlg(MSG_NO_ID, mtError, [mbOK]);
    ActiveControl := SeriesName;
    Result := False;
    Exit;
    end;

  // Check for a duplicate time series name
  I := Project.Lists[TIMESERIES].IndexOf(S);
  if (I >= 0) and (I <> SeriesIndex) then
    begin
    Uutils.MsgDlg(MSG_DUPLICATE_ID, mtError, [mbOK]);
    ActiveControl := SeriesName;
    Result := False;
    Exit;
    end;

  // Check that specified external file exists
  if UseFileCheckBox.Checked and not FileExists(Trim(FileNameEdit.Text)) then
  begin
    Uutils.MsgDlg(MSG_NO_FILE, mtError, [mbOK]);
    ActiveControl := FileNameEdit;
    Result := False;
    Exit;
  end;

  // Check for time values out of order
  if not TimeOrdered then
  begin
    ActiveControl := GridEdit.Grid;
    Result := False;
    Exit;
  end;
  Result := True;
end;

function TTimeseriesForm.TimeOrdered: Boolean;
//-----------------------------------------------------------------------------
// Checks if time series data are out of order.
//-----------------------------------------------------------------------------
var
  I     : Integer;
  S     : String;
  T     : TDateTime;
  D0    : TDateTime;
  D1    : TDateTime;
  Dt    : TDateTime;
  Dt1   : TDateTime;
begin

  // Assign a value to starting date D0
  try
    D0 := StrToDate(Project.Options.Data[START_DATE_INDEX], MyFormatSettings);
  except
    on EConvertError do D0 := Date;
  end;

  // Initialize variables
  D1 := D0;                  // Current date = starting date
  Dt1 := -1;                 // Previous elapsed time is negative
  Result := True;            // Assume data are in order

  // Examine each row of the GridEdit control
  with GridEdit.Grid do
  begin
    for I := 1 to RowCount-1 do
    begin

      // Skip row if time series value is blank
      if Length(Trim(Cells[2,I])) = 0 then continue;

      // Convert the date appearing in 1st column to a number,
      // and use this as the starting date if its the first row
      S := Trim(Cells[0,I]);
      if Length(S) > 0 then
      try
        D1 := StrToDate(S, MyFormatSettings);
        if I = 1 then D0 := D1;
      except
        on EConvertError do continue;
      end;

      // Convert the time appearing in the 2nd column to decimal days
      try
        S := Trim(Cells[1,I]);
        T := Uutils.StrHoursToTime(S);
        if T < 0 then continue;

        // Compute elapsed time from start of series
        Dt := (D1 + T - D0);

        // If new elapsed time is <= previous elapsed time then
        // break with data being out of order
        if Dt <= Dt1 then
        begin
          Row := I;
          Col := 1;
          Result := False;
          Uutils.MsgDlg(MSG_OUT_OF_ORDER, mtError, [mbOK]);
          break;
        end;

        // Set previous elapsed time to current elapsed time
        Dt1 := Dt;
      except
        on EConvertError do continue;
      end;

      // Check for valid numerical value
      S := Trim(Cells[2,I]);
      try
        StrToFloat(S);
      except
        on EConvertError do
        begin
          Result := False;
          Uutils.MsgDlg(MSG_ILLEGAL_NUMBER, mtError, [mbOK]);
          Row := I;
          Col := 2;
          break;
        end;
      end;

    end;
  end;
end;

procedure TTimeseriesForm.BtnViewClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for View button. Displays a plot of the time series.
//-----------------------------------------------------------------------------
var
  D0 : TDateTime;
  PreviewPlotForm: TPreviewPlotForm;
begin
  // Get a value for the starting date
  try
    D0 := StrToDate(Project.Options.Data[START_DATE_INDEX], MyFormatSettings);
  except
    on EConvertError do D0 := Date;
  end;

  // Create the plot form and call its PlotTimeSeries function
  PreviewPlotForm := TPreviewPlotForm.Create(self);
  with PreviewPlotForm do
  try
    Width := 619;
    Left := Self.Left + (Self.Width - Width) div 2;
    Top := Self.Top + Self.Height - Height;
    if Left < 0 then Left := 0;
    if Top  < 0 then Top  := 0;
    PlotTimeSeries(GridEdit.Grid, TXT_TIME_SERIES + SeriesName.Text, D0);
    ShowModal;
  finally
    Free;
  end;
end;

procedure TTimeseriesForm.FindFileBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for FindFile button. Opens a File Dialog to get name
//  of Time Series file.
//-----------------------------------------------------------------------------
begin
  with MainForm.OpenTextFileDialog do
  begin
    Title := TXT_OPEN_TIMESERIES_TITLE;
    Filter := TXT_TIMESERIES_FILTER;
    InitialDir := ProjectDir;
    Filename := '*.dat';
    Options := Options + [ofHideReadOnly];
    if Execute then FileNameEdit.Text := Filename;
  end;
end;

procedure TTimeseriesForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
//-----------------------------------------------------------------------------
//  Brings up context-sensitive Help when the F1 key is pressed.
//  (Form's KeyPreview property was set to True).
//-----------------------------------------------------------------------------
begin
  if Key = VK_F1 then BtnHelpClick(Sender);
end;

procedure TTimeseriesForm.BtnHelpClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 211440);
end;

end.
