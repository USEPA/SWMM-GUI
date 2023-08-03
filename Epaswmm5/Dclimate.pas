unit Dclimate;

{-------------------------------------------------------------------}
{                    Unit:    Dclimate.pas                          }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21     (5.2.0)                  }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form unit containing the Climatology properties editor.  }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GridEdit, StdCtrls, ComCtrls, FileCtrl, Grids, NumEdit, Buttons,
  Uproject, Uglobals, Mask, ExtCtrls;

type
  TClimatologyForm = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    OKBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    Label12: TLabel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    TempSeriesCombo: TComboBox;
    TempTseriesBtn: TBitBtn;
    RadioButton3: TRadioButton;
    TempFileEdit: TEdit;
    TempFileBtn: TBitBtn;
    TempStartDateBox: TCheckBox;
    TempStartDateEdit: TMaskEdit;
    EvapGridLabel: TLabel;
    SoilRecoveryLabel: TLabel;
    Bevel1: TBevel;
    Label1: TLabel;
    EvapTempLabel: TLabel;
    EvapValueEdit: TNumEdit;
    EvapSeriesCombo: TComboBox;
    EvapGrid1: TGridEditFrame;
    EvapGrid2: TGridEditFrame;
    EvapSeriesBtn: TBitBtn;
    RecoveryCombo: TComboBox;
    RecoveryBtn1: TBitBtn;
    RecoveryBtn2: TBitBtn;
    DryOnlyCheckBox: TCheckBox;
    EvapSourceCombo: TComboBox;
    WindSpeedLabel: TLabel;
    WindGrid1: TGridEditFrame;
    WindGrid2: TGridEditFrame;
    RadioButton9: TRadioButton;
    RadioButton8: TRadioButton;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label11: TLabel;
    SnowTempUnitsLabel: TLabel;
    ElevUnitsLabel: TLabel;
    Label13: TLabel;
    Label3: TLabel;
    NumEdit1: TNumEdit;
    NumEdit2: TNumEdit;
    NumEdit3: TNumEdit;
    NumEdit4: TNumEdit;
    NumEdit5: TNumEdit;
    NumEdit6: TNumEdit;
    Label10: TLabel;
    ADCGrid: TGridEditFrame;
    Panel1: TPanel;
    NoADBtn1: TButton;
    NatADBtn1: TButton;
    NoADBtn2: TButton;
    NatADBtn2: TButton;
    TabSheet6: TTabSheet;
    AdjustGrid: TGridEditFrame;
    Panel2: TPanel;
    Label2: TLabel;
    ClearBtn: TButton;
    GroupBox1: TGroupBox;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    RadioButton6: TRadioButton;
    TempFileDelBtn: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure TempFileBtnClick(Sender: TObject);
    procedure TempSeriesComboClick(Sender: TObject);
    procedure TempFileEditChange(Sender: TObject);
    procedure NoADBtn1Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure TempTseriesBtnClick(Sender: TObject);
    procedure EvapSeriesBtnClick(Sender: TObject);
    procedure TempSeriesComboChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TempStartDateBoxClick(Sender: TObject);
    procedure RecoveryComboDblClick(Sender: TObject);
    procedure RecoveryBtn2Click(Sender: TObject);
    procedure RecoveryComboChange(Sender: TObject);
    procedure DryOnlyCheckBoxClick(Sender: TObject);
    procedure EvapSourceComboChange(Sender: TObject);
    procedure EditChanged(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure TempFileDelBtnClick(Sender: TObject);
    procedure TempSeriesComboKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    FileDir: String;
    TempFileName: String;
  public
    { Public declarations }
    HasChanged: Boolean;
    procedure SetData(I: Integer);
    procedure GetData;
  end;

//var
//  ClimatologyForm: TClimatologyForm;

implementation

{$R *.dfm}

uses Fmain, Uedit;

const

  MonthLabels: array[0..11] of PChar =
    ('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec');
  AdjustColLabels: array[0..4] of PChar =
    ('Month','Temp','Evap','Rain','Cond');

  AdjustCaption: String =
    ''#13+
    'Temp'#9' temperature adjustment (+- deg F or deg C)'#13+
    'Evap'#9' evaporation adjustment (+- in/day or mm/day)'#13+
    'Rain'#9' rainfall multiplier'#13+
    'Cond'#9' soil conductivity multiplier';

  ADCColLabels: PChar = 'Depth Ratio'#13'Impervious'#13'Pervious';
  ADCRowLabels: PChar = ''#13'0.0'#13'0.1'#13'0.2'#13'0.3'#13'0.4'#13'0.5'#13+
                        '0.6'#13'0.7'#13'0.8'#13'0.9';

  TXT_SELECT_FILE = 'Select a Climatological File';
  TXT_FILE_FILTER = 'Data files (*.DAT)|*.DAT|Text files (*.TXT)|*.TXT' +
                    '|All files|*.*';
  TXT_DAILY_EVAP   = 'Daily Evaporation ';
  TXT_MONTHLY_EVAP = 'Monthly Evaporation ';
  TXT_PAN_COEFFS   = 'Pan Coefficients';
  TXT_EVAP_SERIES  = 'Name of Time Series';

var
  EvapUnits: String;

procedure TClimatologyForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnCreate handler. Sets up the form's grid and combo box entries.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  FileDir := ProjectDir;
  TempFileName := '';

  with MainForm.ProjectImageList do
  begin
    GetBitmap(GetIndexByName('edit'), TempTseriesBtn.Glyph);
    GetBitmap(GetIndexByName('browse'), TempFileBtn.Glyph);
    GetBitmap(GetIndexByName('delete'), TempFileDelBtn.Glyph);
    GetBitmap(GetIndexByName('edit'), RecoveryBtn1.Glyph);
    GetBitmap(GetIndexByName('delete'), RecoveryBtn2.Glyph);
    GetBitmap(GetIndexByName('edit'), EvapSeriesBtn.Glyph);
  end;

  with EvapGrid1.Grid do
  begin
    DefaultRowHeight := EvapGrid1.EditBox.Height;
    DefaultColWidth := ClientWidth div ColCount - 1;
    for I := 0 to 5 do Cells[I,0] := MonthLabels[I];
    ClientWidth := ColCount * (DefaultColWidth + 1);
    ClientHeight := RowCount * (DefaultRowHeight + 1);
  end;
  with EvapGrid2.Grid do
  begin
    DefaultRowHeight := EvapGrid2.EditBox.Height;
    DefaultColWidth := ClientWidth div ColCount - 1;
    for I := 6 to 11 do Cells[I-6,0] := MonthLabels[I];
    ClientWidth := ColCount * (DefaultColWidth + 1);
    ClientHeight := RowCount * (DefaultRowHeight + 1);
  end;
  with WindGrid1.Grid do
  begin
    DefaultRowHeight := WindGrid1.EditBox.Height;
    DefaultColWidth := ClientWidth div ColCount - 1;
    for I := 0 to 5 do Cells[I,0] := MonthLabels[I];
    ClientWidth := ColCount * (DefaultColWidth + 1);
    ClientHeight := RowCount * (DefaultRowHeight + 1);
  end;
  with WindGrid2.Grid do
  begin
    DefaultRowHeight := WindGrid2.EditBox.Height;
    DefaultColWidth := ClientWidth div ColCount - 1;
    for I := 6 to 11 do Cells[I-6,0] := MonthLabels[I];
    ClientWidth := ColCount * (DefaultColWidth + 1);
    ClientHeight := RowCount * (DefaultRowHeight + 1);
  end;
  with ADCGrid.Grid do
  begin
    DefaultRowHeight := ADCGrid.EditBox.Height;
    DefaultColWidth := (Width div ColCount) - 1;
    Height := RowCount * (DefaultRowHeight + 1) + 1;
    Cols[0].SetText(ADCRowLabels);
    Rows[0].SetText(ADCColLabels);
  end;

  with AdjustGrid.Grid do
  begin
    RowCount := 13;
    DefaultRowHeight := AdjustGrid.EditBox.Height;
    Height := RowCount * (DefaultRowHeight + 1) + 1;
    AdjustGrid.ClientHeight := Height;
    for I := 1 to 13 do Cells[0,I] := MonthLabels[I-1];
    for I := 0 to 4 do Cells[I,0] := AdjustColLabels[I];
  end;
  Panel2.Top := AdjustGrid.Top + AdjustGrid.Height + 2;
  Label2.Caption := AdjustCaption;

  Panel1.Top := ADCGrid.Top + ADCGrid.Grid.Height + 2;
  TempSeriesCombo.Items := Project.Lists[TIMESERIES];
  EvapTempLabel.Top := EvapValueEdit.Top;
  EvapSeriesCombo.Top := EvapValueEdit.Top;
  EvapSeriesCombo.Items := Project.Lists[TIMESERIES];
  EvapSeriesBtn.Top := EvapValueEdit.Top;
  RecoveryCombo.Items := Project.Lists[PATTERN];
  EvapUnits := '(in/day)';
  if UnitSystem = usSI then
  begin
    WindSpeedLabel.Caption     := 'Monthly Average Wind Speed (km/hr)';
    SnowTempUnitsLabel.Caption := '(degrees C)';
    ElevUnitsLabel.Caption     := '(meters)';
    EvapUnits := '(mm/day)';
  end;

  EvapSourceCombo.ItemIndex := Project.Climatology.EvapType;
  EvapSourceComboChange(Self);
end;

procedure TClimatologyForm.TempSeriesComboChange(Sender: TObject);
//-----------------------------------------------------------------------------
//  Checks off Time Series radio button when an entry in the Time Series combo
//  box is changed.
//-----------------------------------------------------------------------------
begin
  RadioButton2.Checked := True;
  HasChanged := True;
end;

procedure TClimatologyForm.TempSeriesComboClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  Checks off Time Series radio button when an entry in the Time Series combo
//  box is selected.
//-----------------------------------------------------------------------------
begin
  RadioButton2.Checked := True;
  HasChanged := True;
end;

procedure TClimatologyForm.TempSeriesComboKeyPress(Sender: TObject;
  var Key: Char);
begin
  if (Key = ' ') or (Key = '"') or (Key = ';') then Key := #0
end;

procedure TClimatologyForm.TempTseriesBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  Launches the Time Series Editor when the Temp Tseries button is clicked.
//-----------------------------------------------------------------------------
var
  I: Integer;
  S: String;
begin
  // Extract name of time series from Temp. Time Series combo box
  S := Trim(TempSeriesCombo.Text);

  // Edit the time series with this name
  I := Project.Lists[TIMESERIES].IndexOf(S);
  S := Uedit.EditTimeseries(I);

  // If name was changed, then change it in the Temp. Time Series combo box
  if Length(S) > 0 then
  begin
      TempSeriesCombo.Text := S;
      TempSeriesCombo.Items := Project.Lists[TIMESERIES];
  end;
end;

procedure TClimatologyForm.TempFileBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  Uses the MainForm's OpenTextFileDialog to obtain the name of a
//  temperature data file when the TempFileBtn is clicked.
//-----------------------------------------------------------------------------
begin
  with MainForm.OpenTextFileDialog do
  begin
    Filename := TempFileName;
    InitialDir := FileDir;
    if Length(Filename) = 0 then Filename := '*.dat'
    else InitialDir := ExtractFileDir(Filename);
    Title := TXT_SELECT_FILE;
    Filter := TXT_FILE_FILTER;
    FilterIndex := 1;
    if Execute then
    begin
      TempFileName := Filename;
      TempFileEdit.Text := MinimizeName(TempFileName, self.Canvas,
        TempFileEdit.ClientWidth);
      FileDir := ExtractFileDir(Filename);
      TempFileBtn.SetFocus;
    end;
  end;
end;

procedure TClimatologyForm.TempFileDelBtnClick(Sender: TObject);
begin
  TempFileEdit.Clear;
  TempFileName := '';
  TempFileDelBtn.SetFocus;
end;

procedure TClimatologyForm.TempFileEditChange(Sender: TObject);
//-----------------------------------------------------------------------------
//  Checks off the External Climate File radio button when contents of
//  TempFileEdit edit box is changed.
//-----------------------------------------------------------------------------
begin
  RadioButton3.Checked := True;
  HasChanged := True;
  TempFileEdit.Hint := TempFileEdit.Text;
end;

procedure TClimatologyForm.TempStartDateBoxClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  Enables the Temperature File Start Date edit control when the
//  TempStartDateBox check box is checked.
//-----------------------------------------------------------------------------
begin
  TempStartDateEdit.Enabled := TempStartDateBox.Checked;
end;

procedure TClimatologyForm.EvapSourceComboChange(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnChange handler for the EvapSourceCombo control.
//-----------------------------------------------------------------------------
var
  J: Integer;
begin
  EvapValueEdit.Visible := False;
  EvapSeriesCombo.Visible := False;
  EvapSeriesBtn.Visible := False;
  EvapGridLabel.Visible := False;
  EvapGrid1.Visible := False;
  EvapGrid2.Visible := False;
  EvapTempLabel.Visible := False;

  case EvapSourceCombo.ItemIndex of
  CONSTANT_EVAP:
  begin
    EvapGridLabel.Caption := TXT_DAILY_EVAP + EvapUnits;
    EvapGridLabel.Visible := True;
    EvapValueEdit.Visible := True;
  end;
  TSERIES_EVAP:
  begin
    EvapGridLabel.Caption := TXT_EVAP_SERIES;
    EvapGridLabel.Visible := True;
    EvapSeriesCombo.Visible := True;
    EvapSeriesBtn.Visible := True;
  end;
  FILE_EVAP:
  begin
    EvapGridLabel.Visible := True;
    EvapGrid1.Visible := True;
    EvapGrid2.Visible := True;
    EvapGridLabel.Caption := TXT_PAN_COEFFS;
    EvapGridLabel.Visible := True;
    with Project.Climatology do
    begin
       for J := 0 to 5 do
      begin
        EvapGrid1.Grid.Cells[J,1] := PanData[J];
        EvapGrid2.Grid.Cells[J,1] := PanData[J+6];
      end;
    end;
  end;
  MONTHLY_EVAP:
  begin
    EvapGridLabel.Visible := True;
    EvapGrid1.Visible := True;
    EvapGrid2.Visible := True;
    EvapGridLabel.Caption := TXT_MONTHLY_EVAP + EvapUnits;
    EvapGridLabel.Visible := True;
    with Project.Climatology do
    begin
      for J := 0 to 5 do
      begin
        EvapGrid1.Grid.Cells[J,1] := EvapData[J];
        EvapGrid2.Grid.Cells[J,1] := EvapData[J+6];
      end;
    end;
  end;
  TEMP_EVAP:
    EvapTempLabel.Visible := True;
  end;
end;

procedure TClimatologyForm.EditChanged(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnChange handler for most of form's controls.
//-----------------------------------------------------------------------------
begin
  HasChanged := True;
end;

procedure TClimatologyForm.EvapSeriesBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  Launches the Time Series Editor when the Evap. Tseries button is clicked.
//-----------------------------------------------------------------------------
var
  I: Integer;
  S: String;
begin
  // Extract name of time series from Evap. Time Series combo box
  S := Trim(EvapSeriesCombo.Text);

  // Edit the time series with this name
  I := Project.Lists[TIMESERIES].IndexOf(S);
  S := Uedit.EditTimeseries(I);

  // If name was changed, then change it in the Evap. Time Series combo box
  if Length(S) > 0 then
  begin
      EvapSeriesCombo.Text := S;
      EvapSeriesCombo.Items := Project.Lists[TIMESERIES];
  end;
end;

procedure TClimatologyForm.OKBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the OK button.
//-----------------------------------------------------------------------------
begin
  ModalResult := mrOK;
  if EvapGrid1.Modified then HasChanged := True;
  if EvapGrid2.Modified then HasChanged := True;
  if WindGrid1.Modified then HasChanged := True;
  if WindGrid2.Modified then HasChanged := True;
  if ADCGrid.Modified then HasChanged := True;
  if AdjustGrid.Modified then HasChanged := True;
end;

procedure TClimatologyForm.NoADBtn1Click(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the four Areal Depletion buttons on the Areal
//  Depletion page of the form.
//-----------------------------------------------------------------------------
var
  K: Integer;
begin
  with ADCGrid.Grid do
  begin
    // No areal depletion for impervious area
    if Sender = NoADBtn1 then
      for K := 1 to 10 do Cells[1,K] := DefADCurve[K]
    // No areal depletion for pervious area
    else if Sender = NoADBtn2 then
      for K := 1 to 10 do Cells[2,K] := DefADCurve[K]
    // Natural areal depletion for impervious area
    else if Sender = NatADBtn1 then
      for K := 1 to 10 do Cells[1,K] := NatADCurve[K]
    // Natural area depletion for pervious area
    else if Sender = NatADBtn2 then
      for K := 1 to 10 do Cells[2,K] := NatADCurve[K]
    else exit;
  end;
  HasChanged := True;
end;

procedure TClimatologyForm.SetData(I: Integer);
//-----------------------------------------------------------------------------
//  Loads current climatology data into form and displays page I.
//-----------------------------------------------------------------------------
var
  J: Integer;
  K: Integer;
begin
  with Project.Climatology do
  begin
    // Temperature data
    TempSeriesCombo.Text := TempTseries;
    TempFileName := TempFile;
    TempFileEdit.Text := MinimizeName(TempFileName, self.Canvas, TempFileEdit.ClientWidth);
    if (not SameText(TempStartDate, '*')) then
    begin
      TempStartDateBox.Checked := True;
      TempStartDateEdit.Enabled := True;
      TempStartDateEdit.Text := TempStartDate;
    end;
    case TempUnitsType of
    0: RadioButton4.Checked := True;
    1: RadioButton5.Checked := True;
    2: RadioButton6.Checked := True;
    end;

    FileDir := ExtractFileDir(TempFile);
    if Length(FileDir) = 0 then FileDir := ProjectDir;
    case TempDataSource of
    NO_TEMP     : RadioButton1.Checked := True;
    TSERIES_TEMP: RadioButton2.Checked := True;
    FILE_TEMP   : RadioButton3.Checked   := True;
    end;

    // Evaporation data
    EvapValueEdit.Text := EvapData[0];
    EvapSeriesCombo.Text := EvapTseries;
    EvapSourceCombo.ItemIndex := EvapType;
    RecoveryCombo.Text := RecoveryPat;
    DryOnlyCheckBox.Checked := EvapDryOnly;

    // Wind speed data
    with WindGrid1.Grid do
    begin
      for J := 0 to 5 do Cells[J,1] := WindSpeed[J+1];
    end;
    with WindGrid2.Grid do
    begin
      for J := 0 to 5 do Cells[J,1] := WindSpeed[J+7];
    end;
    if WindType = FILE_WINDSPEED then RadioButton8.Checked := True
    else RadioButton9.Checked := True;

    // Snow melt data
    for J := 1 to 6 do
    begin
      with FindComponent('NumEdit'+IntToStr(J)) as TNumEdit do
        Text := SnowMelt[J];
    end;
    with ADCGrid.Grid do
    begin
      for J := 1 to 2 do
      begin
        for K := 1 to 10 do
          Cells[J,K] := ADCurve[J][K];
      end;
    end;

   // Climate adjustment data
    with AdjustGrid.Grid do
    begin
      for J := 1 to 12 do
        begin
          Cells[1,J] := TempAdjust[J-1];
          Cells[2,J] := EvapAdjust[J-1];
          Cells[3,J] := RainAdjust[J-1];
          Cells[4,J] := CondAdjust[J-1];
        end;
    end;

  end;
  PageControl1.ActivePageIndex := I;
  EvapGrid1.Modified := False;
  EvapGrid2.Modified := False;
  WindGrid1.Modified := False;
  WindGrid2.Modified := False;
  ADCGrid.Modified   := False;
  AdjustGrid.Modified := False;
  HasChanged := False;
end;

procedure TClimatologyForm.GetData;
//-----------------------------------------------------------------------------
//  Retrieves climatology data from the form.
//-----------------------------------------------------------------------------
var
  I: Integer;
  J: Integer;
  S: String;
begin
  with Project.Climatology do
  begin

    // Temperature data
    if RadioButton1.Checked then TempDataSource := NO_TEMP;
    if RadioButton2.Checked then
    begin
      TempDataSource := TSERIES_TEMP;
      TempTseries    := TempSeriesCombo.Text;
    end;
    if RadioButton3.Checked then
    begin
      TempDataSource := FILE_TEMP;
      TempFile := TempFileName;
      if TempStartDateBox.Checked
      then TempStartDate := TempStartDateEdit.Text
      else TempStartDate := '*';
      TempUnitsType := 0;
      if RadioButton5.Checked then TempUnitsType := 1;
      if RadioButton6.Checked then TempUnitsType := 2;
    end;

    // Evaporation data
    if EvapType <> EvapSourceCombo.ItemIndex then
    begin
      EvapType := EvapSourceCombo.ItemIndex;
      HasChanged := True;
    end;
    case EvapType of
    CONSTANT_EVAP:
    begin
      S := Trim(EvapValueEdit.Text);
      if Length(S) > 0 then for I := 0 to 11 do EvapData[I] := S;
    end;
    TSERIES_EVAP:
    begin
      EvapTseries := EvapSeriesCombo.Text;
    end;
    FILE_EVAP:
    begin
      for I := 0 to 5 do
      begin
        S := Trim(EvapGrid1.Grid.Cells[I,1]);
        if Length(S) = 0 then S := '0.0';
        PanData[I] := S;
        S := Trim(EvapGrid2.Grid.Cells[I,1]);
        if Length(S) = 0 then S := '0.0';
        PanData[I+6] := S;
      end;
    end;
    MONTHLY_EVAP:
    begin
      for I := 0 to 5 do
      begin
        S := Trim(EvapGrid1.Grid.Cells[I,1]);
        if Length(S) = 0 then S := '0.0';
        EvapData[I] := S;
        S := Trim(EvapGrid2.Grid.Cells[I,1]);
        if Length(S) = 0 then S := '0.0';
        EvapData[I+6] := S;
      end;
    end;
    end;
    RecoveryPat := Trim(RecoveryCombo.Text);
    EvapDryOnly := DryOnlyCheckBox.Checked;

    // Wind speed data
    with WindGrid1.Grid do
    begin
      for I := 0 to 5 do
      begin
        S := Trim(Cells[I,1]);
        if Length(S) = 0 then S := '0.0';
        WindSpeed[I+1] := S;
      end;
    end;
    with WindGrid2.Grid do
    begin
      for I := 0 to 5 do
      begin
        S := Trim(Cells[I,1]);
        if Length(S) = 0 then S := '0.0';
        WindSpeed[I+7] := S;
      end;
    end;
    if RadioButton8.Checked then WindType := FILE_WINDSPEED
    else WindType := MONTHLY_WINDSPEED;

    // Snow melt data
    for I := 1 to 6 do
    begin
      with FindComponent('NumEdit'+IntToStr(I)) as TNumEdit do
      begin
        S := Trim(Text);
        if Length(S) > 0 then SnowMelt[I] := S;
      end;
    end;
    with ADCGrid.Grid do
    begin
      for I := 1 to 2 do
      begin
        for J := 1 to 10 do
        begin
           S := Trim(Cells[I,J]);
           if Length(S) > 0 then ADCurve[I][J] := S;
        end;
      end;
    end;

    // Climate adjustment data
    with AdjustGrid.Grid do
    begin
      for J := 1 to 12 do
        begin
          S := Trim(Cells[1,J]);
          TempAdjust[J-1] := S;
          S := Trim(Cells[2,J]);
          EvapAdjust[J-1] := S;
          S := Trim(Cells[3,J]);
          RainAdjust[J-1] := S;
          S := Trim(Cells[4,J]);
          CondAdjust[J-1] := S;
        end;
    end;

  end;
end;

procedure TClimatologyForm.RecoveryComboDblClick(Sender: TObject);
var
  I: Integer;
  S: String;
begin
  with RecoveryCombo do
  begin
    I := Project.Lists[PATTERN].IndexOf(Trim(Text));
    S := Uedit.EditPattern(I);
    if Length(S) > 0 then Text := S;
  end;
end;

procedure TClimatologyForm.RecoveryBtn2Click(Sender: TObject);
begin
  RecoveryCombo.Text := '';
end;

procedure TClimatologyForm.RecoveryComboChange(Sender: TObject);
begin
  HasChanged := True;
end;

procedure TClimatologyForm.ClearBtnClick(Sender: TObject);
var
  I,J: Integer;
begin
    with AdjustGrid.Grid do
    begin
      for J := 1 to 12 do
      begin
        for I := 1 to 4 do Cells[I,J] := '';
      end;
    end;
end;

procedure TClimatologyForm.DryOnlyCheckBoxClick(Sender: TObject);
begin
  HasChanged := True;
end;

procedure TClimatologyForm.HelpBtnClick(Sender: TObject);
begin
  with PageControl1 do
    if ActivePage = TabSheet1 then
       Application.HelpCommand(HELP_CONTEXT, 212340)
    else if ActivePage = TabSheet2 then
       Application.HelpCommand(HELP_CONTEXT, 212360)
    else if ActivePage = TabSheet3 then
       Application.HelpCommand(HELP_CONTEXT, 212370)
    else if ActivePage = TabSheet4 then
       Application.HelpCommand(HELP_CONTEXT, 212380)
    else if ActivePage = TabSheet5 then
       Application.HelpCommand(HELP_CONTEXT, 212350)
    else if ActivePage = TabSheet6 then
       Application.HelpCommand(HELP_CONTEXT, 213480)
end;

procedure TClimatologyForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then HelpBtnClick(Sender);
end;

end.
