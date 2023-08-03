unit Dcurve;

{-------------------------------------------------------------------}
{                    Unit:    Dcurve.pas                            }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form unit for editing a Curve object.                    }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, TeeProcs, TeEngine, Chart, Grids, Series, ClipBrd,
  Math, Vcl.Buttons, GridEdit, Menus, NumEdit,
  Uglobals, Uutils, Uproject;

type
  TCurveDataForm = class(TForm)
    CurveName: TEdit;
    CurveTypeCombo: TComboBox;
    Comment: TEdit;
    Label1: TLabel;
    CurveTypeLabel: TLabel;
    Label3: TLabel;
    BtnOK: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    BtnLoad: TButton;
    BtnSave: TButton;
    BtnView: TButton;
    GridEdit: TGridEditFrame;
    EditBtn: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure CurveTypeComboClick(Sender: TObject);
    procedure CurveNameChange(Sender: TObject);
    procedure CurveNameKeyPress(Sender: TObject; var Key: Char);
    procedure BtnOKClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure BtnLoadClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure BtnViewClick(Sender: TObject);
    procedure EditBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    CurveType: Integer;
    CurveIndex: Integer;
    FileDir: String;
    function  ValidateData: Boolean;
  public
    { Public declarations }
    Modified: Boolean;
    procedure SetData(const Ctype: Integer; const Cindex: Integer;
              const S: String; aCurve: TCurve);
    procedure GetData(var S: String; aCurve: TCurve);
  end;

//var
//  CurveDataForm: TCurveDataForm;

implementation

{$R *.DFM}

uses
  Dprevplot, Fmain, Uedit;

const
  MAXITEMS = 100;

  ft2 = '(ft' + #178 + ')';
  ft3 = '(ft' + #179 + ')';
  m2 = '(m' + #178 + ')';
  m3 = '(m' + #179 + ')';

  CurveTypeLabels: array[CONTROLCURVE..WEIRCURVE] of String =
    ('Control', 'Diversion', 'Pump', 'Rating', 'Shape', 'Storage', 'Tidal',
     'Weir');

  Xlabel: array[CONTROLCURVE..WEIRCURVE] of String =
    ('Controller', 'Inflow', '', 'Head', 'Depth /', 'Depth', 'Hour', 'Head');

  Xunits: array[CONTROLCURVE..WEIRCURVE, 0..1] of String =
    (('Value','Value'), ('',''), ('',''), ('(ft)','(m)'),
     ('Full Depth','Full Depth'), ('(ft)', '(m)'),  ('of Day', 'of Day'),
     ('(ft)', '(m)'));

  Ylabel: array[CONTROLCURVE..WEIRCURVE] of String =
    ('Control', 'Outflow', 'Flow', 'Outflow', 'Width /', 'Area', 'Stage',
     'Coefficient');

  Yunits: array[CONTROLCURVE..WEIRCURVE, 0..1] of String =
   (('Setting','Setting'), ('',''), ('',''), ('',''),
    ('Full Depth','Full Depth'), (ft2,m2),  ('(ft)','(m)'),
    ('',''));

  PumpTypeLabels: array[0..4] of String =
    ('TYPE1', 'TYPE2', 'TYPE3', 'TYPE4', 'TYPE5');

  Xpump: array[0..4] of String =
    ('Volume', 'Depth', 'Head', 'Depth', 'Head');

  XPumpUnits: array[0..4, 0..1] of String =
    ((ft3,m3), ('(ft)','(m)'), ('(ft)','(m)'), ('(ft)','(m)'),
     ('(ft)','(m)'));

  MSG_NO_ID        = 'No curve name supplied.';
  MSG_DUPLICATE_ID = 'Curve name already in use.';
  MSG_OUT_OF_ORDER = ' values are not in ascending order. Accept data anyway?';
  TXT_CURVE_EDITOR = ' Curve Editor';
  TXT_OPEN_CURVE_TITLE = 'Open a Curve';
  TXT_SAVE_CURVE_TITLE = 'Save Curve As';
  TXT_CURVE_FILTER = 'Data files (*.dat)|*.DAT|All files|*.*';
  TXT_CURVE_HEADER = 'EPASWMM Curve Data';
  TXT_DESCRIPTION = 'Curve Description';
  TXT_CURVE = ' Curve ';

procedure TCurveDataForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
// Form's OnCreate handler.
//-----------------------------------------------------------------------------
var
  I : Integer;
begin
  FileDir := ProjectDir;
  with MainForm.ProjectImageList do
    GetBitmap(GetIndexByName('edit'), EditBtn.Glyph);

  // Set up the GridEdit control
  with GridEdit.Grid do
  begin
    Ctl3d := False;
    ColWidths[0] := 32;
    ColWidths[1] := (ClientWidth - ColWidths[0] - 3) div 2;
    ColWidths[2] := ColWidths[1];
    DefaultRowHeight := GridEdit.EditBox.Height;
    RowHeights[0] := 3 * (-Font.Height);
    RowCount := MAXITEMS + 1;
    for I := 1 to RowCount-1 do
      Cells[0,I] := IntToStr(I);
  end;
  GridEdit.CenterHeaders := True;

  // Hide pump curve type combobox
  CurveTypeLabel.Visible := False;
  CurveTypeCombo.Visible := False;
  ActiveControl := CurveName;
end;

procedure TCurveDataForm.CurveTypeComboClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for CurveType combobox.
// Changes column headings on the DataGrid grid control
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  I := CurveTypeCombo.ItemIndex;
  GridEdit.Grid.Cells[1,0] := Xpump[I] + #13 + XpumpUnits[I][Ord(UnitSystem)];
end;

procedure TCurveDataForm.SetData(const Ctype: Integer; const Cindex: Integer;
  const S: String; aCurve: TCurve);
//-----------------------------------------------------------------------------
// Loads data for the curve being edited into the form.
// Ctype = type of curve, Cindex = curve index, S = name of curve
// aCurve = curve object.
//-----------------------------------------------------------------------------
var
  I: Integer;
  Hdr: String;
begin
  // Save curve type and index
  CurveType := Ctype;
  CurveIndex := Cindex;

  // Get name associated with current curve
  Caption := CurveTypeLabels[CurveType] + TXT_CURVE_EDITOR;
  CurveName.Text := S;
  Comment.Text := aCurve.Comment;

  // Expand size of data grid if necessary
  if aCurve.Xdata.Count > MAXITEMS
  then GridEdit.Grid.RowCount := aCurve.Xdata.Count + 1;

  // Add header labels to the grid
  Hdr := Xlabel[CurveType];
  if CurveType = DIVERSIONCURVE
  then Hdr := Hdr + #13 + '(' + FlowUnits + ')'
  else Hdr := Hdr + #13 + Xunits[CurveType][Ord(UnitSystem)];
  GridEdit.Grid.Cells[1,0] := Hdr;
  Hdr := Ylabel[CurveType];
  case CurveType of
    DIVERSIONCURVE,
    PUMPCURVE,
    RATINGCURVE: Hdr := Hdr + #13 + '(' + FlowUnits + ')';
    else Hdr := Hdr + #13 + Yunits[CurveType][Ord(UnitSystem)];
  end;
  GridEdit.Grid.Cells[2,0] := Hdr;

  // Enable curve type combo box for pump curves
  if CurveType = PUMPCURVE then
  begin
    CurveTypeLabel.Visible := True;
    CurveTypeCombo.Visible := True;
    for I := 0 to High(PumpTypeLabels) do
      CurveTypeCombo.Items.Add(PumpTypeLabels[I]);
    I := aCurve.CurveCode - 1;
    if I < 0 then I := 0;
    CurveTypeCombo.ItemIndex := I;
    CurveTypeComboClick(self);
  end;

  // Add X-Y values to the GridEdit control
  with aCurve.Xdata do
  begin
    for I := 0 to Count-1 do
      GridEdit.Grid.Cells[1,I+1] := Strings[I];
  end;
  with aCurve.Ydata do
  begin
    for I := 0 to Count-1 do
      GridEdit.Grid.Cells[2,I+1] := Strings[I];
  end;
  GridEdit.AllowInsert := True;
  GridEdit.Modified := False;
  if CurveType = TIDALCURVE
  then GridEdit.EditBox.Style := esNumber
  else GridEdit.EditBox.Style := esPosNumber;
  Modified := False;
end;

procedure TCurveDataForm.GetData(var S: String; aCurve: TCurve);
//-----------------------------------------------------------------------------
// Unloads data from the form to the curve being edited.
// S = edited name of curve, aCurve = the curve object.
//-----------------------------------------------------------------------------
var
  I: Integer;
  Sx,Sy: String;
begin
  S := CurveName.Text;
  if not Modified then Exit;
  aCurve.Comment := Comment.Text;
  aCurve.CurveType := CurveTypeLabels[CurveType];
  if CurveType = PUMPCURVE then
  begin
    aCurve.CurveCode := CurveTypeCombo.ItemIndex + 1;
    aCurve.CurveType := aCurve.CurveType + IntToStr(aCurve.CurveCode);
  end;
  aCurve.Xdata.Clear;
  aCurve.Ydata.Clear;
  with GridEdit.Grid do
  begin
    for I := 1 to RowCount-1 do
    begin
      Sx := Trim(Cells[1,i]);
      Sy := Trim(Cells[2,i]);
      if (Length(Sx) > 0) and (Length(Sy) > 0) then
      begin
        aCurve.Xdata.Add(Sx);
        aCurve.Ydata.Add(Sy);
      end;
    end;
  end;
  HasChanged := True;
end;

procedure TCurveDataForm.CurveNameChange(Sender: TObject);
//-----------------------------------------------------------------------------
// OnChange handler for CurveName edit control.
//-----------------------------------------------------------------------------
begin
  Modified := True;
end;

procedure TCurveDataForm.CurveNameKeyPress(Sender: TObject; var Key: Char);
//-----------------------------------------------------------------------------
// OnKeyPress handler for CurveName edit control. Prevents user from entering
// a space character.
//-----------------------------------------------------------------------------
begin
  if (Key = ' ') or (Key = '"') or (Key = ';') then Key := #0
  else with CurveName as TEdit do
  begin
    if (Length(Text) = 0) or (SelStart = 0) then
      if Key = '[' then Key := #0;
  end;
end;

procedure TCurveDataForm.EditBtnClick(Sender: TObject);
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

procedure TCurveDataForm.BtnOKClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for OK button. Validates and accepts the form.
//-----------------------------------------------------------------------------
begin
  if not Modified then Modified := GridEdit.Modified;
  if not ValidateData then ModalResult := mrNone
  else ModalResult := mrOK;
end;

function TCurveDataForm.ValidateData: Boolean;
//-----------------------------------------------------------------------------
//  Validates data entered into the form's controls.
//-----------------------------------------------------------------------------
var
  I, J : Integer;
  S    : String;
  ID   : String;
  DupID: Boolean;
  Vx, Vx1: Single;
begin
  // Check for no curve name
  S := Trim(CurveName.Text);
  if Length(S) = 0 then
  begin
    Uutils.MsgDlg(MSG_NO_ID, mtError, [mbOK]);
    CurveName.SetFocus;
    Result := False;
    Exit;
  end;

  // Temporarily blank out existing curve's ID in data base
  if CurveIndex >= 0 then
  begin
    ID := Project.Lists[CurveType].Strings[CurveIndex];
    Project.Lists[CurveType].Strings[CurveIndex] := '';
  end;

  // See if another curve with same name exists
  DupID := Project.FindCurve(S, I, J);

  // Restore ID name and display error message if duplicate found
  if CurveIndex >= 0 then Project.Lists[CurveType].Strings[CurveIndex] := ID;
  if DupID then
  begin
    Uutils.MsgDlg(MSG_DUPLICATE_ID, mtError, [mbOK]);
    CurveName.SetFocus;
    Result := False;
    Exit;
  end;

  // Check for X-values out of order
  Vx1 := 0;
  with GridEdit.Grid do for I := 1 to RowCount-1 do
  begin
    if Uutils.GetSingle(Cells[1,I], Vx) then
    begin
      if Vx < Vx1 then
      begin
        Result := True;
        S := Cells[1,0];
        Delete(S, Pos(#13,S), Length(S));
        if Uutils.MsgDlg(S + MSG_OUT_OF_ORDER, mtConfirmation,
          [mbYes, mbNo]) = mrNo then
        begin
          ActiveControl := GridEdit.Grid;
          Result := False;
        end;
        Exit;
      end
      else Vx1 := Vx;
    end;
  end;
  Result := True;
end;

procedure TCurveDataForm.BtnViewClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for View button. Displays a plot of the curve.
//-----------------------------------------------------------------------------
var
  UseStairs: Boolean;
  SwitchXY:  Boolean;
  Title: String;
begin
  // Type 1 and Type 2 pump curves use stair-step display
  // Type 3 pump curve has X & Y axes switched
  SwitchXY  := False;
  UseStairs := False;
  if CurveType = PUMPCURVE then
  begin
    if (CurveTypeCombo.ItemIndex = 0) or (CurveTypeCombo.ItemIndex = 1)
    then UseStairs := True;
    if CurveTypeCombo.ItemIndex = 2 then SwitchXY := True;
  end;

  // Create a form to plot the curve on
  with TPreviewPlotForm.Create(self) do
  try
    Left := Self.Left + (Self.Width - Width) div 2;
    Top := Self.Top + Self.Height - Height;
    if Left < 0 then Left := 0;
    if Top  < 0 then Top  := 0;

    // Storage curves have their x-section shape displayed
    Title := CurveTypeLabels[CurveType] + TXT_CURVE + CurveName.Text;
    if CurveType = STORAGECURVE
    then PlotStorageXsect(GridEdit.Grid, Title)
    else if CurveType = SHAPECURVE
    then PlotShapeXsect(GridEdit.Grid, Title)
    else PlotCurveData(GridEdit.Grid, Title, UseStairs, SwitchXY);
    ShowModal;
  finally
    Free;
  end;
end;

procedure TCurveDataForm.BtnLoadClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for Load button. Loads in curve data from a text file.
//-----------------------------------------------------------------------------
var
  F : Textfile;
  S : String;
  I : Integer;
  P : Integer;
begin
  with MainForm.OpenTextFileDialog do
  begin
    Title := TXT_OPEN_CURVE_TITLE;
    Filter := TXT_CURVE_FILTER;
    InitialDir := FileDir;
    Filename := '*.dat';
    Options := Options + [ofHideReadOnly];
    if Execute then
    begin
      FileDir := ExtractFileDir(Filename);
      AssignFile(F,Filename);
      {$I-}
      Reset(F);
      {$I+}
      if (IOResult = 0) then
      try
        Readln(F, S);
        Readln(F, S);
        Comment.Text := S;
        with GridEdit.Grid do
        begin
          for I := 1 to RowCount-1 do
          begin
            Cells[1,I] := '';
            Cells[2,I] := '';
          end;
          I := 1;
          while not Eof(F) and (I < RowCount) do
          begin
            Readln(F, S);
            P := Pos(' ', Trim(S));
            if P > 0 then
            begin
              Cells[1,I] := Copy(S,1,P-1);
              Cells[2,I] := Trim(Copy(S,P+1,Length(S)));
              Inc(I);
            end;
          end;
        end;
      finally
      end;
      CloseFile(F);
      Modified := True;
      Comment.SetFocus;
    end;
  end;
end;

procedure TCurveDataForm.BtnSaveClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for Save button. Saves curve data to a text file.
//-----------------------------------------------------------------------------
var
  F : Textfile;
  Sx: String;
  Sy: String;
  I : Integer;
begin
  with MainForm.SaveDialog do
  begin
    Title := TXT_SAVE_CURVE_TITLE;
    Filter := TXT_CURVE_FILTER;
    InitialDir := FileDir;
    Filename := '*.dat';
    if Execute then
    begin
      FileDir := ExtractFileDir(Filename);
      AssignFile(F,Filename);
      {$I-}
      Rewrite(F);
      {$I+}
      if (IOResult = 0) then
      try
        Writeln(F, TXT_CURVE_HEADER);
        Writeln(F, Comment.Text);
        with GridEdit.Grid do for I := 1 to RowCount-1 do
        begin
          Sx := Trim(Cells[1,I]);
          Sy := Trim(Cells[2,I]);
          if (Length(Sx) > 0) and (Length(Sy) > 0) then
            Writeln(F, Sx, '  ', Sy);
        end;
      finally
      end;
      CloseFile(F);
    end;
  end;
end;

procedure TCurveDataForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
//-----------------------------------------------------------------------------
//  Brings up context-sensitive Help when the F1 key is pressed.
//  (Form's KeyPreview property was set to True).
//-----------------------------------------------------------------------------
begin
  if Key = VK_F1 then BtnHelpClick(Sender);
end;

procedure TCurveDataForm.BtnHelpClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 211430);
end;

end.
