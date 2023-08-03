unit Dtsect;

{-------------------------------------------------------------------}
{                    Unit:    Dtsect.pas                            }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form unit for editing a natural cross-section transect.  }
{                                                                   }
{   The form contains a GridEdit frame on the left to edit          }
{   Station-Elevation data, and a PropEdit control on the right     }
{   to edit properties of the transect.                             }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, Clipbrd, Menus, ExtCtrls, NumEdit, PropEdit, Math,
  Uglobals, Uutils, Uproject, GridEdit, Buttons;

type
  TTransectForm = class(TForm)
    Label1: TLabel;
    NameEdit: TEdit;
    Label3: TLabel;
    CommentEdit: TEdit;
    BtnOK: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    BtnView: TButton;
    Panel1: TPanel;
    GridEdit: TGridEditFrame;
    EditBtn: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BtnViewClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure NameEditChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditBtnClick(Sender: TObject);

////  New procedure added. (LR - 9/5/05)  ////
    procedure NameEditKeyPress(Sender: TObject; var Key: Char);

  private
    { Private declarations }
    TsectIndex: Integer;
    PropEdit1: TPropEdit;
    PropList: TStringlist;
    function DataGridValid: Boolean;
    function ValidateData: Boolean;
  public
    { Public declarations }
    Modified: Boolean;
    procedure SetData(const I: Integer; const S: String; Tsect: TTransect);
    procedure GetData(var S: String; Tsect: TTransect);
  end;

//var
//  TransectForm: TTransectForm;

implementation

{$R *.dfm}

uses
  fmain, Dprevplot, Uedit;

const
  MAXSTATIONS = 1500;
  TXT_PROPERTY = 'Property';
  TXT_VALUE = 'Value';
  TXT_STATION = 'Station';
  TXT_ELEVATION = 'Elevation';
  TXT_TRANSECT = 'Transect ';

////  Added by LR - 9/5/05  ////
  TXT_DESCRIPTION = 'Transect Description';

  MSG_INVALID_NAME = 'Invalid transect name.';
  MSG_DUPLICATE_NAME = 'Duplicate transect name.';
  MSG_NO_N_VALUE = 'No value supplied for main channel Manning''s N.';
  MSG_INVALID_STATION = 'Invalid Station value.';
  MSG_INVALID_ELEVATION = 'Invalid Elevation value.';
  MSG_INVALID_ORDER = 'Stations are out of order.';

  DefaultProps: array[0..10] of String =
    ('', '0.0', '0.0', '0.0', '', '0.0', '0.0', '', '1.0', '0.0', '1.0');

  LengthUnits: array[0..1] of String = ('(ft)','(m)');

var
  TsectProps: array[0..10] of TPropRecord =
    ((Name: 'Roughness:';    Style: esHeading),
     (Name: '  Left Bank';   Style: esEdit;       Mask: emPosNumber;  Length: 0),
     (Name: '  Right Bank';  Style: esEdit;       Mask: emPosNumber;  Length: 0),
     (Name: '  Channel';     Style: esEdit;       Mask: emPosNumber;  Length: 0),
     (Name: 'Bank Stations:';Style: esHeading),
     (Name: '  Left';        Style: esEdit;       Mask: emNumber;     Length: 0),
     (Name: '  Right';       Style: esEdit;       Mask: emNumber;     Length: 0),
     (Name: 'Modifiers:';    Style: esHeading),
     (Name: '  Stations';    Style: esEdit;       Mask: emPosNumber;  Length: 0),
     (Name: '  Elevations';  Style: esEdit;       Mask: emNumber;     Length: 0),
     (Name: '  Meander';     Style: esEdit;       Mask: emPosNumber;  Length: 0));

procedure TTransectForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnCreate handler.
//-----------------------------------------------------------------------------
var
  I: Integer;
  S: String;
begin
  // Create a PropEdit control
  PropEdit1 := TPropEdit.Create(self);
  with PropEdit1 do
  begin
    Parent := Panel1;
    Align := alClient;
    BorderStyle := bsNone;
    ColHeading1 := TXT_PROPERTY;
    ColHeading2 := TXT_VALUE;
    ValueColor := clNavy;
    TabStop := True;
    TabOrder := 0;
  end;

  // Create a stringlist to hold the property values
  PropList := TStringlist.Create;
  for I := 0 to High(DefaultProps) do
    PropList.Add(DefaultProps[I]);

  // Set up the GridEdit data grid
  with GridEdit.Grid do
  begin
    Ctl3D := False;
    S := TXT_STATION + #13 + LengthUnits[Ord(UnitSystem)];
    Cells[1,0] := S;
    S := TXT_ELEVATION + #13 + LengthUnits[Ord(UnitSystem)];
    Cells[2,0] := S;
    ColWidths[0] := 32;
    ColWidths[1] := (ClientWidth - ColWidths[0] - 3) div 2;
    ColWidths[2] := ColWidths[1];
    DefaultRowHeight := GridEdit.EditBox.Height;
    RowHeights[0] := DefaultRowHeight + (-Font.Height);
  end;
  GridEdit.CenterHeaders := True;
  with MainForm.ProjectImageList do
    GetBitmap(GetIndexByName('edit'), EditBtn.Glyph);

end;

procedure TTransectForm.FormDestroy(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnDestroy handler.
//-----------------------------------------------------------------------------
begin
  PropList.Free;
  PropEdit1.Free;
end;

procedure TTransectForm.FormShow(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnShow handler.
//-----------------------------------------------------------------------------
begin
  PropEdit1.SetProps(TsectProps, PropList);
  PropEdit1.Edit;
  NameEdit.SetFocus;
end;

////  New procdure added. (LR - 9/5/05)  ////
procedure TTransectForm.NameEditKeyPress(Sender: TObject; var Key: Char);
//-----------------------------------------------------------------------------
//  OnKeyPress handler for the NameEdit edit box.
//-----------------------------------------------------------------------------
begin
  if (Key = ' ') or (Key = '"') or (Key = ';') then Key := #0
  else with NameEdit as TEdit do
  begin
    if (Length(Text) = 0) or (SelStart = 0) then
      if Key = '[' then Key := #0;
  end;
end;

procedure TTransectForm.BtnOKClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the OK button.
//-----------------------------------------------------------------------------
begin
  PropEdit1.IsValid;
  if not Modified then Modified := PropEdit1.Modified;
  if not Modified then Modified := GridEdit.Modified;
  if not ValidateData then ModalResult := mrNone
  else ModalResult := mrOK;
end;

procedure TTransectForm.SetData(const I: Integer; const S: String;
  Tsect: TTransect);
//-----------------------------------------------------------------------------
//  Loads data from transect Tsect with index I and ID name S into the form.
//-----------------------------------------------------------------------------
var
  J: Integer;
  N: Integer;
begin
  TsectIndex := I;
  NameEdit.Text := S;
  CommentEdit.Text := Tsect.Comment;
  PropList.Clear;
  with PropList do
  begin
    Add('');
    Add(Tsect.Data[TRANSECT_N_LEFT]);
    Add(Tsect.Data[TRANSECT_N_RIGHT]);
    Add(Tsect.Data[TRANSECT_N_CHANNEL]);
    Add('');
    Add(Tsect.Data[TRANSECT_X_LEFT]);
    Add(Tsect.Data[TRANSECT_X_RIGHT]);
    Add('');
    Add(Tsect.Data[TRANSECT_X_FACTOR]);
    Add(Tsect.Data[TRANSECT_Y_FACTOR]);
    Add(Tsect.Data[TRANSECT_L_FACTOR]);
  end;
  N := Tsect.Xdata.Count;
  if N > MAXSTATIONS then N := MAXSTATIONS;
  if N < 100 then N := 100;
  with GridEdit.Grid do
  begin
    RowCount := N + 1;
    for J := 1 to N do
      Cells[0,J] := IntToStr(J);
  end;
  with Tsect.Xdata do
  begin
    N := MinIntValue([Count, MAXSTATIONS]);
    for J := 0 to N-1 do
      GridEdit.Grid.Cells[1,J+1] := Strings[J];
  end;
  with Tsect.Ydata do
  begin
    N := MinIntValue([Count, MAXSTATIONS]);
    for J := 0 to N-1 do
      GridEdit.Grid.Cells[2,J+1] := Strings[J];
  end;
  GridEdit.AllowInsert := True;
  GridEdit.EditBox.Style := esNumber;
  GridEdit.Modified := False;
  Modified := False;
end;

procedure TTransectForm.GetData(var S: String; Tsect: TTransect);
//-----------------------------------------------------------------------------
//  Retrieves data from the form for transect Tsect.
//-----------------------------------------------------------------------------
var
  I: Integer;
  Sx, Sy: String;
begin
  S := NameEdit.Text;
  Tsect.Comment := CommentEdit.Text;
  for I := 0 to PropList.Count-1 do
    if Length(Trim(PropList[I])) = 0 then PropList[I] := '0.0';
  Tsect.Data[TRANSECT_N_LEFT]    := PropList[1];
  Tsect.Data[TRANSECT_N_RIGHT]   := PropList[2];
  Tsect.Data[TRANSECT_N_CHANNEL] := PropList[3];
  Tsect.Data[TRANSECT_X_LEFT]    := PropList[5];
  Tsect.Data[TRANSECT_X_RIGHT]   := PropList[6];
  Tsect.Data[TRANSECT_X_FACTOR]  := PropList[8];
  Tsect.Data[TRANSECT_Y_FACTOR]  := PropList[9];
  Tsect.Data[TRANSECT_L_FACTOR]  := PropList[10];
  Tsect.Xdata.Clear;
  Tsect.Ydata.Clear;
  with GridEdit.Grid do
  begin
    for I := 1 to RowCount-1 do
    begin
      Sx := Trim(Cells[1,I]);
      Sy := Trim(Cells[2,I]);
      if (Length(Sx) > 0) and (Length(Sy) > 0) then
      begin
        Tsect.Xdata.Add(Sx);
        Tsect.Ydata.Add(Sy);
      end;
    end;
  end;
  Tsect.SetMaxDepth;
  Project.SetTransectConduitDepth(S, Tsect.Data[TRANSECT_MAX_DEPTH]);
end;

function TTransectForm.ValidateData: Boolean;
//-----------------------------------------------------------------------------
//  Validates the data entered into the form.
//-----------------------------------------------------------------------------
var
  S : String;
  I : Integer;
begin
  Result := True;
  S := Trim(NameEdit.Text);
  if (Length(S) = 0) then
  begin
    Uutils.MsgDlg(MSG_INVALID_NAME, mtError, [mbOK]);
    NameEdit.SetFocus;
    Result := False;
    Exit;
  end;
  I := Project.Lists[TRANSECT].IndexOf(S);
  if (I >= 0) and (I <> TsectIndex) then
  begin
    Uutils.MsgDlg(MSG_DUPLICATE_NAME, mtError, [mbOK]);
    NameEdit.SetFocus;
    Result := False;
    Exit;
  end;
  if Length(Trim(PropList[3])) = 0 then
  begin
    Uutils.MsgDlg(MSG_NO_N_VALUE, mtError, [mbOK]);
    PropEdit1.Edit;
    Result := False;
    Exit;
  end;
  if not DataGridValid then
  begin
    Result := False;
    GridEdit.Grid.SetFocus;
    Exit;
  end;
end;

function TTransectForm.DataGridValid: Boolean;
//-----------------------------------------------------------------------------
//  Validates the Station-Elevation data entered into the form.
//-----------------------------------------------------------------------------
var
  Sx, Sy : String;
  I : Integer;
  Vx, Vx1, Vy: Single;
begin
  Result := False;
  Vx1 := -1.0e20;
  with GridEdit.Grid do
  begin
    for I := 1 to RowCount-1 do
    begin
      Sx := Trim(Cells[1,I]);
      Sy := Trim(Cells[2,I]);
      if (Length(Sx) > 0) or (Length(Sy) > 0) then
      begin
        if not Uutils.GetSingle(Sx, Vx) then
        begin
          Row := I;
          Col := 1;
          Uutils.MsgDlg(MSG_INVALID_STATION, mtError, [mbOK]);
          Exit;
        end;
        if not Uutils.GetSingle(Sy, Vy) then
        begin
          Row := I;
          Col := 2;
          Uutils.MsgDlg(MSG_INVALID_ELEVATION, mtError, [mbOK]);
          Exit;
        end;
        if Vx < Vx1 then
        begin
          Row := I;
          Col := 1;
          Uutils.MsgDlg(MSG_INVALID_ORDER, mtError, [mbOK]);
          Exit;
        end;
        Vx1 := Vx;
      end;
    end;
  end;
  Result := True;
end;

procedure TTransectForm.EditBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the Edit button. Launches a comment editor dialog.
//-----------------------------------------------------------------------------
var
  S: String;
  Modified: Boolean;
begin
  S := CommentEdit.Text;
  Uedit.EditComment(TXT_DESCRIPTION, S, Modified);
  if Modified then CommentEdit.Text := S;
end;

procedure TTransectForm.BtnViewClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the View button. Displays a plot of the transect.
//-----------------------------------------------------------------------------
begin
  PropEdit1.IsValid;
  with TPreviewPlotForm.Create(self) do
  try
    Width := 619;
    Left := Self.Left + (Self.Width - Width) div 2;
    Top := Self.Top + Self.Height - Height;
    if Left < 0 then Left := 0;
    if Top  < 0 then Top  := 0;
    PlotTransectData(GridEdit.Grid, PropList[5], PropList[6], PropList[8],
      PropList[9], TXT_TRANSECT + NameEdit.Text, LengthUnits[Ord(UnitSystem)]);
    ShowModal;
  finally
    Free;
  end;
end;

procedure TTransectForm.NameEditChange(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnChange handler shared by the form's controls.
//-----------------------------------------------------------------------------
begin
  Modified := True;
end;

procedure TTransectForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
//-----------------------------------------------------------------------------
//  Brings up context-sensitive Help when the F1 key is pressed.
//  (Form's KeyPreview property was set to True).
//-----------------------------------------------------------------------------
begin
  if Key = VK_F1 then BtnHelpClick(Sender);
end;

procedure TTransectForm.BtnHelpClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 212040);
end;

end.
