unit Dinfil;

{-------------------------------------------------------------------}
{                    Unit:    Dinfil.pas                            }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form unit for editing subcatchment infiltration          }
{   or storage node exfiltration parameters.                        }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Uglobals, Uproject, Uutils, GridEdit;

type
  TInfilForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    ComboBox1: TComboBox;
    Panel3: TPanel;
    OKBtn: TButton;
    CancleBtn: TButton;
    HelpBtn: TButton;
    Panel4: TPanel;
    HintLabel: TLabel;
    GridEdit: TGridEditFrame;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure GridEditGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    OldInfilModel: Integer;
    NewInfilModel: Integer;
    HortonInfil: array[0..MAXINFILPROPS] of String;
    GreenAmptInfil: array[0..MAXINFILPROPS] of String;
    CurveNumInfil: array[0..MAXINFILPROPS] of String;
    procedure SetInfilProperties;
    procedure ShowHint(aRow: Integer);
  public
    { Public declarations }
    HasChanged: Boolean;
    procedure SetData(const InfilData: array of String);
    procedure GetData(var InfilData: array of String);
    procedure SetInfilModel(const S: String);
    procedure GetInfilModelName(var S: String);
  end;

//var
//  InfilForm: TInfilForm;

implementation

{$R *.DFM}

const
  TXT_PROPERTY = 'Property';
  TXT_VALUE = 'Value';

  HortonProps: array[1..5] of String =
   ('Max. Infil. Rate', 'Min. Infil. Rate', 'Decay Constant',
    'Drying Time', 'Max. Volume');

  GreenAmptProps: array[1..3] of String =
  ('Suction Head', 'Conductivity', 'Initial Deficit');

  CurveNumProps: array[1..3] of String =
  ('Curve Number', 'Conductivity', 'Drying Time');

  HortonHint: array[1..5] of String =
  ('Maximum rate on the Horton infiltration curve (in/hr or mm/hr)',
   'Minimum rate on the Horton infiltration curve (in/hr or mm/hr)',
   'Decay constant for the Horton infiltration curve (1/hr)',
   'Time for a fully saturated soil to completely dry (days)',
   'Maximum infiltration volume possible (inches or mm, 0 if not applicable)');

  GreenAmptHint: array[1..3] of String =
  ('Soil capillary suction head (inches or mm)',
   'Soil saturated hydraulic conductivity (in/hr or mm/hr)',
   'Difference between soil porosity and initial moisture content ' +
   '(a fraction)');

   StorageInfilHint: array[1..3] of String =
  ('Capillary suction head (inches or mm).',
   'Saturated hydraulic conductivity (in/hr or mm/hr). Enter ' +
   '0 for no seepage.',
   'Difference between porosity and initial moisture content. ' +
   'Enter 0 for constant seepage equal to conductivity.');

  CurveNumHint: array[1..3] of String =
  ('SCS runoff curve number',
   'This property has been deprecated and its value is ignored.',
   'Time for a fully saturated soil to completely dry (days)');

  ErrMsg: array[1..3] of String =
  ('Min. Infil. Rate cannot be greater than Max. Infil. Rate.',
   'Initial Deficit must be a fraction.',
   'Curve Number cannot exceed 100');

procedure TInfilForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnCreate handler.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  // Load infiltration options into ComboBox1
  for I := 0 to High(InfilOptions) do
    ComboBox1.Items.Add(InfilOptions[I]);
//  ComboBox1.ItemHeight := Uglobals.ItemHeight;

  HasChanged := False;

  // Assign default infil. parameters
  for I := 0 to MAXINFILPROPS do
  begin
    HortonInfil[I] := DefHortonInfil[I];
    GreenAmptInfil[I] := DefGreenAmptInfil[I];
    CurveNumInfil[I] := DefCurveNumInfil[I];
  end;
  NewInfilModel := 0;

  with GridEdit.Grid do
  begin
    DefaultColWidth := (Width - 2) div 2;
    Cells[0,0] := TXT_PROPERTY;
    Cells[1,0] := TXT_VALUE;
  end;
end;

procedure TInfilForm.FormShow(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnShow handler.
//-----------------------------------------------------------------------------
begin
  SetInfilProperties;
  GridEdit.Grid.SetFocus;
end;

procedure TInfilForm.ComboBox1Change(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnChange handler for ComboBox1.
//-----------------------------------------------------------------------------
var
  J: Integer;
begin
  // Save grid entries to proper infil. data array
  with GridEdit.Grid do
  begin
    for J := 1 to RowCount-1 do
    begin
      if Length(Trim(Cells[1,J])) > 0 then
      case NewInfilModel of
      HORTON_INFIL, MOD_HORTON_INFIL:
        HortonInfil[J-1] := Cells[1, J];
      GREEN_AMPT_INFIL, MOD_GREEN_AMPT_INFIL:
        GreenAmptInfil[J-1] := Cells[1, J];
      CURVE_NUMBER_INFIL:
        CurveNumInfil[J-1] := Cells[1, J];
      end;
    end;
  end;

  // Load properties for new choice of infil. model into the Property Editor
  NewInfilModel := ComboBox1.ItemIndex;
  SetInfilProperties;
  GridEdit.Grid.SetFocus;
end;

procedure TInfilForm.SetInfilModel(const S: String);
//-----------------------------------------------------------------------------
//  Determines which Infiltration model is represented by string S.
//  (S will be empty when the dialog form is used to edit exfiltration
//  parameters for a storage node.)
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  if Length(S) = 0 then
  begin
    Caption := 'Storage Seepage Editor';
    OldInfilModel := 3;
    ComboBox1.Visible := False;
    Panel2.Caption := '  Properties of soil beneath storage unit:';
  end
  else begin
    OldInfilModel := 0;
    for I := 0 to High(InfilOptions) do
      if SameText(S, InfilOptions[I]) then OldInfilModel := I;
    ComboBox1.ItemIndex := OldInfilModel
  end;
  NewInfilModel := OldInfilModel;
end;

procedure TInfilForm.SetInfilProperties;
//-----------------------------------------------------------------------------
//  Loads an infiltration model's parameters into the Grid Editor.
//-----------------------------------------------------------------------------
var
  J: Integer;
begin
  with GridEdit.Grid do
  begin
    case NewInfilModel  of
      HORTON_INFIL, MOD_HORTON_INFIL:
      begin
        RowCount := High(HortonProps) + 1;
        for J := 1 to RowCount-1 do
        begin
          Cells[0, J] := HortonProps[J];
          Cells[1, J] := HortonInfil[J-1];
        end;
      end;
      GREEN_AMPT_INFIL, MOD_GREEN_AMPT_INFIL:
      begin
        RowCount := High(GreenAmptProps) + 1;
        for J := 1 to RowCount-1 do
        begin
          Cells[0, J] := GreenAmptProps[J];
          Cells[1, J] := GreenAmptInfil[J-1];
        end;
      end;
      CURVE_NUMBER_INFIL:
      begin
        RowCount := High(CurveNumProps) + 1;
        for J := 1 to RowCount-1 do
        begin
          Cells[0, J] := CurveNumProps[J];
          Cells[1, J] := CurveNumInfil[J-1];
        end;
      end;
    end;
  end;
  ShowHint(GridEdit.Grid.Row);
end;

procedure TInfilForm.SetData(const InfilData: array of String);
//-----------------------------------------------------------------------------
//  Loads a set of infiltration parameters into the form.
//-----------------------------------------------------------------------------
var
  J: Integer;
  N: Integer;
begin
  N := High(InfilData);
  case NewInfilModel of
    HORTON_INFIL, MOD_HORTON_INFIL:
      for J := 0 to N do HortonInfil[J] := InfilData[J];
    GREEN_AMPT_INFIL, MOD_GREEN_AMPT_INFIL:
      for J := 0 to N do GreenAmptInfil[J] := InfilData[J];
    CURVE_NUMBER_INFIL:
      for J := 0 to N do CurveNumInfil[J] := InfilData[J];
  end;
  SetInfilProperties;
end;

procedure TInfilForm.GetData(var InfilData: array of String);
//-----------------------------------------------------------------------------
//  Retrieves a set of infiltration parameters from the form.
//-----------------------------------------------------------------------------
var
  J: Integer;
begin
  for J := 0 to MAXINFILPROPS do InfilData[J] := ' ';
  with GridEdit.Grid do
  begin
    for J := 1 to RowCount-1 do
      if Length(Trim(Cells[1, J])) > 0 then InfilData[J-1] := Cells[1, J];
  end;
  HasChanged := GridEdit.EditBox.Modified;
  if OldInfilModel <> NewInfilModel then HasChanged := true;
end;

procedure TInfilForm.GetInfilModelName(var S: String);
//-----------------------------------------------------------------------------
//  Retrieves the type of infiltration model selected from the form.
//-----------------------------------------------------------------------------
begin
  S := ComboBox1.Text;
end;

procedure TInfilForm.ShowHint(aRow: Integer);
//-----------------------------------------------------------------------------
//  Displays a decription of the infiltration property being edited.
//-----------------------------------------------------------------------------
var
  S: String;
begin
  case ComboBox1.ItemIndex of
  HORTON_INFIL, MOD_HORTON_INFIL:         S := HortonHint[aRow];
  GREEN_AMPT_INFIL, MOD_GREEN_AMPT_INFIL: S := GreenAmptHint[aRow];
  CURVE_NUMBER_INFIL:                     S := CurveNumHint[aRow];
  else                                    S := StorageInfilHint[aRow];
  end;
  HintLabel.Caption := S;
end;

procedure TInfilForm.GridEditGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  ShowHint(ARow);
end;

procedure TInfilForm.OKBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the OK button.
//-----------------------------------------------------------------------------
var
  J: Integer;
  ErrCode: Integer;
  X: array[0 .. MAXINFILPROPS] of Single;
begin
  // Convert grid's string values to numerical values
  with GridEdit.Grid do
  begin
    for J := 1 to RowCount-1 do
      if not Uutils.GetSingle(Cells[1, J], X[J-1]) then
      begin
        Row := J;
        Uutils.MsgDlg('Illegal property value', mtError, [mbOK]);
        exit;
      end;
  end;

  // Check for valid numerical values
  ErrCode := 0;
  case NewInfilModel of
    HORTON_INFIL, MOD_HORTON_INFIL:
      if X[1] > X[0] then ErrCode := 1;
    GREEN_AMPT_INFIL, MOD_GREEN_AMPT_INFIL:
      if X[2] > 1 then ErrCode := 2;
    CURVE_NUMBER_INFIL:
      if X[0] >= 100 then ErrCode := 3;
  end;

  // Display error message if invalid data found
  if ErrCode = 0 then ModalResult := mrOK
  else Uutils.MsgDlg(ErrMsg[ErrCode], mtError, [mbOK]);
end;

procedure TInfilForm.HelpBtnClick(Sender: TObject);
begin
  case NewInfilModel of
  HORTON_INFIL, MOD_HORTON_INFIL:
    Application.HelpCommand(HELP_CONTEXT, 212690);
  GREEN_AMPT_INFIL, MOD_GREEN_AMPT_INFIL:
    Application.HelpCommand(HELP_CONTEXT, 212700);
  CURVE_NUMBER_INFIL:
    Application.HelpCommand(HELP_CONTEXT, 212710);
  else
    Application.HelpCommand(HELP_CONTEXT, 212700);
  end;
end;

procedure TInfilForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then HelpBtnClick(Sender);
end;

end.
