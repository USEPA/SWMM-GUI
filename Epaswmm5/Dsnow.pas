unit Dsnow;

{-------------------------------------------------------------------}
{                    Unit:    Dsnow.pas                             }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form unit used to supply the properties of a Snowpack    }
{   object.                                                         }
{                                                                   }
{   The form consists of a Page control with two pages - one with   }
{   a GridEdit frame used for snowpack properties for plowable,     }
{   impervious, and pervious sub-areas, and one for snow removal    }
{   parameters.                                                     }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, NumEdit, GridEdit, Grids,
  Uproject, Uglobals, Uutils;

type
  TSnowpackForm = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    Label1: TLabel;
    NameEdit: TNumEdit;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Label12: TLabel;
    PackGrid: TGridEditFrame;
    FracPlowableEdit: TNumEdit;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label4: TLabel;
    NumEdit1: TNumEdit;
    NumEdit2: TNumEdit;
    NumEdit3: TNumEdit;
    NumEdit4: TNumEdit;
    NumEdit5: TNumEdit;
    SubcatchNameEdit: TEdit;
    NumEdit6: TNumEdit;
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure PackGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure PackGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure NameEditChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure NameEditKeyPress(Sender: TObject; var Key: Char);

  private
    { Private declarations }
    SnowpackIndex: Integer;
    function ValidateInput: Boolean;
    function ValidateRemovalFractions: Boolean;
  public
    { Public declarations }
    Modified: Boolean;
    procedure SetData(const Index: Integer; SP: TSnowpack);
    procedure GetData(var S: String; SP: TSnowpack);
  end;

//var
//  SnowpackForm: TSnowpackForm;

implementation

{$R *.dfm}

const
  PackProps: array[0..7] of String =
    ('Subcatchment Surface Type',
     'Min. Melt Coeff.',
     'Max. Melt Coeff.',
     'Base Temperature',
     'Fraction Free Water Capacity',
     'Initial Snow Depth',
     'Initial Free Water',
     'Depth at 100% Cover');

  PackUnits: array[0..7, 0..1] of String =
    (('', ''),
     (' (in/hr/deg F)', ' (mm/hr/deg C)'),
     (' (in/hr/deg F)', ' (mm/hr/deg C)'),
     (' (deg F)', ' (deg C)'),
     ('', ''),
     (' (in)', ' (mm)'),
     (' (in)', ' (mm)'),
     (' (in)', ' (mm)'));

  TXT_REMOVAL_DEPTH = 'Depth at which snow removal begins (mm)';
  TXT_PLOWABLE = 'Plowable';
  TXT_IMPERVIOUS = 'Impervious';
  TXT_PERVIOUS = 'Pervious';
  TXT_NOT_NEGATIVE = ' cannot be negative.';
  TXT_NOT_GREATER = ' cannot be greater than ';
  MSG_INVALID_NAME = 'Invalid snow pack name.';
  MSG_DUPLICATE_NAME = 'Duplicate snow pack name.';
  MSG_INVALID_PLOWABLE_FRAC = 'Invalid Fraction of Impervious Area Plowable.';
  MSG_NO_SUBCATCH_NAME = 'No subcatchment was named for snow removal.';
  MSG_INVALID_REMOVALS = 'Invalid snow removal fractions.';

procedure TSnowpackForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnCreate handler.
//-----------------------------------------------------------------------------
var
  I: Integer;
  W: Integer;
begin
  // Initialize row & column labels of Snowpack properties grid
  with PackGrid.Grid do
  begin
    W := (241 * Screen.PixelsPerInch) div 96;
    ColWidths[0] := W;
    W := (ClientWidth - ColWidths[0]) div 3;
    for I := 1 to 3 do ColWidths[I] := W - 1;
    DefaultRowHeight := PackGrid.EditBox.Height;
    Height := RowCount * (DefaultRowHeight + 1) + 1;
    Cells[1,0] := TXT_PLOWABLE;
    Cells[2,0] := TXT_IMPERVIOUS;
    Cells[3,0] := TXT_PERVIOUS;
    for I := 0 to 7 do
      Cells[0,I] := PackProps[I] + PackUnits[I][Ord(UnitSystem)];
  end;

  // Display metric units for snow removal depth if required
  if UnitSystem = usSI then Label4.Caption := TXT_REMOVAL_DEPTH;
end;

procedure TSnowpackForm.NameEditChange(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnChange handler for the various edit fields on the form.
//-----------------------------------------------------------------------------
begin
  Modified := True;
end;

procedure TSnowpackForm.NameEditKeyPress(Sender: TObject; var Key: Char);
//-----------------------------------------------------------------------------
// OnKeyPress handler for the NameEdit box.
// Prevents user from entering '[' as first character.
//-----------------------------------------------------------------------------
begin
  with NameEdit as TNumEdit do
  begin
    if (Length(Text) = 0) or (SelStart = 0) then
      if Key = '[' then Key := #0;
  end;
end;

procedure TSnowpackForm.PackGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
//-----------------------------------------------------------------------------
//  OnGridSelectCell handler for the snow pack data grid.
//  Prevents user from entering Depth at 100% Cover for plowable area.
//-----------------------------------------------------------------------------
begin
  if (ACol = 1) and (ARow = 7)
  then CanSelect := False
  else CanSelect := True;
end;

procedure TSnowpackForm.PackGridDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
//-----------------------------------------------------------------------------
//  OnDrawCell handler for the snow pack data grid.
//  Grays out the plowable area's Depth at 100% Cover cell.
//-----------------------------------------------------------------------------
begin

  if (ACol = 1) and (ARow = 7) then with Sender as TStringGrid do
  begin
    Canvas.Brush.Color := clBtnFace;
    Canvas.FillRect(Rect);
  end;

end;

procedure TSnowpackForm.SetData(const Index: Integer; SP: TSnowpack);
//-----------------------------------------------------------------------------
//  Loads data from a Snowpack into the form.
//-----------------------------------------------------------------------------
var
  I, J: Integer;
begin
  // Save the index of the Snowpack object being edited
  SnowpackIndex := Index;

  // Place the name of existing Snowpack object in the Name edit control
  if Index >= 0
  then NameEdit.Text := Project.Lists[SNOWPACK].Strings[Index];

  // Populate the property grid with the Snowpack's parameters
  with PackGrid.Grid do
  begin
    for I := 1 to 3 do
    begin
      for J := 1 to 7 do Cells[I,J] := SP.Data[I][J];
    end;
    Cells[1,7] := '';
  end;

  // Populate the snow removal controls with the Snowpack's removal parameters
  FracPlowableEdit.Text := SP.FracPlowable;
  for I := 1 to 6 do
  begin
    with FindComponent('NumEdit' + IntToStr(I)) as TNumEdit do
      Text := SP.Plowing[I];
  end;
  SubcatchNameEdit.Text := SP.Plowing[7];
  Modified := False;
end;

procedure TSnowpackForm.GetData(var S: String; SP: TSnowpack);
//-----------------------------------------------------------------------------
//  Loads data from the form into a Snowpack object.
//-----------------------------------------------------------------------------
var
  I, J: Integer;
begin
  // Retrieve the name of the Snowpack from the Name edit control
  S := Trim(NameEdit.Text);

  // Retrieve Snowpack parameter values from the properties grid
  with PackGrid.Grid do
  begin
    for I := 1 to 3 do
    begin
      for J := 1 to 7 do
      begin
        if Length(Cells[I,J]) > 0 then SP.Data[I][J] := Cells[I,J]
        else SP.Data[I][J] := '0.0';
      end;
    end;
  end;

  // Retrieve snow removal parameters
  if Length(FracPlowableEdit.Text) > 0
  then SP.FracPlowable := FracPlowableEdit.Text
  else SP.FracPlowable := '0.0';
  for I := 1 to 6 do
  begin
    with FindComponent('NumEdit' + IntToStr(I)) as TNumEdit do
    begin
      if Length(Text) > 0 then SP.Plowing[I] := Text
      else SP.Plowing[I] := '0.0';
    end;
  end;
  SP.Plowing[7] := SubcatchNameEdit.Text;
end;

procedure TSnowpackForm.OKBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the OK button.
//-----------------------------------------------------------------------------
begin
  if not ValidateInput then ModalResult := mrNone
  else ModalResult := mrOK;
end;

function TSnowpackForm.ValidateInput: Boolean;
//-----------------------------------------------------------------------------
//  Validates data entries on the form.
//-----------------------------------------------------------------------------
var
  S : String;
  I : Integer;
  J : Integer;
  X : Single;
  Y : Single;
begin
  // Check that Snowpack's name is non-empty
  Result := True;
  S := Trim(NameEdit.Text);
  if (Length(S) = 0) then
  begin
    NameEdit.SetFocus;
    Uutils.MsgDlg(MSG_INVALID_NAME, mtError, [mbOK]);
    Result := False;
    Exit;
  end;

  // Check that Snowpack name not the same as another Snowpack
  with Project.Lists[SNOWPACK] do
  for I := 0 to Count-1 do
  begin
    if I = SnowpackIndex then continue;
    if SameText(S, Strings[I]) then
    begin
      NameEdit.SetFocus;
      Uutils.MsgDlg(MSG_DUPLICATE_NAME, mtError, [mbOK]);
      Result := False;
      Exit;
    end;
  end;

  // Check for non-negative numbers for Snowpack properties
  with PackGrid.Grid do
  begin
    for I := 1 to 3 do
    begin
      for J := 1 to 7 do
      begin
        if J = 3 then continue;
        if Uutils.GetSingle(Cells[I,J], X) and (X < 0.0) then
        begin
          PageControl1.ActivePageIndex := 0;
          Row := J;
          Col := I;
          SetFocus;
          Uutils.MsgDlg(PackProps[J] + TXT_NOT_NEGATIVE, mtError, [mbOK]);
          Result := False;
          Exit;
        end;
      end;

      // Check if min. melt coeff. > max. melt coeff.
      if Uutils.GetSingle(Cells[I,1], X)
      and Uutils.GetSingle(Cells[I,2], Y)
      and (X > Y) then
      begin
        PageControl1.ActivePageIndex := 0;
        Row := 1;
        Col := I;
        SetFocus;
        Uutils.MsgDlg(PackProps[1] + TXT_NOT_GREATER + PackProps[2], mtError,
          [mbOK]);
        Result := False;
        Exit;
      end;

      // Check if free water fraction > 1
      if Uutils.GetSingle(Cells[I,4], X) and (X > 1.0) then
      begin
        PageControl1.ActivePageIndex := 0;
        Row := 4;
        Col := I;
        SetFocus;
        Uutils.MsgDlg(PackProps[4] + TXT_NOT_GREATER + '1.0', mtError, [mbOK]);
        Result := False;
        Exit;
      end;
    end;
  end;

  // Check if plowable fraction of impervious area is between 0 and 1
  if not Uutils.GetSingle(FracPlowableEdit.Text, X)
  or (X < 0) or (X > 1.0) then
  begin
    PageControl1.ActivePageIndex := 0;
    FracPlowableEdit.SetFocus;
    Uutils.MsgDlg(MSG_INVALID_PLOWABLE_FRAC, mtError, [mbOK]);
    Result := False;
    Exit;
  end;

  // Check if subcatchment name provided if
  Uutils.GetSingle(NumEdit6.Text, X);
  if (X > 0) and (Length(Trim(SubcatchNameEdit.Text)) = 0) then
  begin
    PageControl1.ActivePageIndex := 1;
    SubcatchNameEdit.SetFocus;
    Uutils.MsgDlg(MSG_NO_SUBCATCH_NAME, mtError, [mbOK]);
    Result := False;
    Exit;
  end;

  // Check if snow removal fractions sum to > 1.0
  if not ValidateRemovalFractions then
  begin
    PageControl1.ActivePageIndex := 1;
    NumEdit1.SetFocus;
    Uutils.MsgDlg(MSG_INVALID_REMOVALS, mtError, [mbOK]);
    Result := False;
    Exit;
  end;
end;

function TSnowpackForm.ValidateRemovalFractions: Boolean;
//-----------------------------------------------------------------------------
//  Validates snow removal fractions.
//-----------------------------------------------------------------------------
var
  I: Integer;
  Sum, X: Single;
  S: String;
begin
  Result := True;
  Sum := 0.0;
  for I := 2 to 6 do
  begin
    with FindComponent('NumEdit'+IntToStr(I)) as TNumEdit do
    begin
      S := Text;
      if Length(Trim(S)) = 0 then continue;
      if Uutils.GetSingle(S, X) then
      begin
        if (X < 0.0) or (X > 1.0)
        then Result := False
        else Sum := Sum + X;
      end;
    end;
    if Sum > 1.01 then Result := False;
  end;
end;

procedure TSnowpackForm.HelpBtnClick(Sender: TObject);
begin
  with PageControl1 do
    if ActivePage = TabSheet1 then
       Application.HelpCommand(HELP_CONTEXT, 212420)
    else if ActivePage = TabSheet2 then
       Application.HelpCommand(HELP_CONTEXT, 212430)
end;

procedure TSnowpackForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then HelpBtnClick(Sender);
end;

end.
