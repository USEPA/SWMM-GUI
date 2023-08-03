unit Daquifer;

{-------------------------------------------------------------------}
{                    Unit:    Daquifer.pas                          }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form unit containing the Groundwater Aquifer properties  }
{   editor.                                                         }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, StdCtrls, ExtCtrls, Uproject, Uglobals, Uutils, PropEdit;

type
  TAquiferForm = class(TForm)
    Panel1: TPanel;
    Splitter1: TSplitter;
    Panel2: TPanel;
    OKBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    Panel3: TPanel;
    HintLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    AquiferProps: array[0..MAXAQUIFERPROPS+1] of TPropRecord;
    PropEdit1: TPropEdit;
    PropList: TStringlist;
    AquiferIndex: Integer;
    procedure ShowPropertyHint(Sender: TObject; aRow: LongInt);
    procedure ValidateEntry(Sender: TObject; Index: Integer; var S: String;
              var Errmsg: String; var IsValid: Boolean);
    function ValidateInput: Boolean;
  public
    { Public declarations }
    procedure SetData(const Index: Integer; A: TAquifer);
    procedure GetData(var S: String; A: TAquifer);
  end;

//var
//  AquiferForm: TAquiferForm;

implementation

{$R *.dfm}

const
  TXT_PROPERTY = 'Property';
  TXT_VALUE = 'Value';
  MSG_NO_DATA = 'This data field cannot be blank.';
  MSG_INVALID_NAME = 'Invalid aquifer name.';
  MSG_DUPLICATE_NAME = 'Duplicate aquifer name.';

  PropNames: array[0..MAXAQUIFERPROPS+1] of String =
    ('Aquifer Name',
     'Porosity',
     'Wilting Point',
     'Field Capacity',
     'Conductivity',
     'Conductivity Slope',
     'Tension Slope',
     'Upper Evap. Fraction',
     'Lower Evap. Depth',
     'Lower GW Loss Rate',
     'Bottom Elevation',
     'Water Table Elevation',
     'Unsat. Zone Moisture ',
     'Upper Evap. Pattern');

  DefaultProps: array[0..MAXAQUIFERPROPS+1] of String =
    ('', '0.5', '0.15', '0.30', '5.0', '10.0', '15.0', '0.35', '14.0', '0.002',
     '0.0', '10.0', '0.30', '');

  AquiferHint: array[0..MAXAQUIFERPROPS+1] of String =
    ('User-assigned aquifer name',
     'Volume of voids / total soil volume',
     'Residual moisture content of a completely dry soil (fraction)',
     'Soil moisture content after all free water has drained off (fraction)',
     'Soil''s saturated hydraulic conductivity (in/hr or mm/hr)',
     'Slope of log(conductivity) v. soil moisture deficit curve',
     'Slope of soil tension v. soil moisture content curve',
     'Fraction of total evaporation available for upper unsaturated zone',
     'Depth into saturated zone over which evaporation can occur (ft or m)',
     'Rate of seepage to deep groundwater when aquifer is completely ' +
     'saturated (in/hr or mm/hr)',
     'Elevation of the bottom of the aquifer (ft or m)',
     'Initial water table elevation (ft or m)',
     'Initial moisture content of the unsaturated upper zone (fraction)',
     'Monthly pattern of adjustments to upper evaporation fraction (optional)');

procedure TAquiferForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnCreate handler.
//-----------------------------------------------------------------------------
var
  I: Integer;
  N: Integer;
begin
  // Create Property Editor
  PropEdit1 := TPropEdit.Create(self);
  with PropEdit1 do
  begin
    Parent := Panel1;
    Align := alClient;
    BorderStyle := bsNone;
    ColHeading1 := TXT_PROPERTY;
    ColHeading2 := TXT_VALUE;
    HeaderSplit := 60;
    ValueColor := clNavy;
    OnValidate := ValidateEntry;
    OnRowSelect := ShowPropertyHint;
  end;

  // Create Property stringlist
  PropList := TStringlist.Create;

  // Initialize contents of each property record
  AquiferProps[0].Name   := PropNames[0];
  AquiferProps[0].Style  := esEdit;
  AquiferProps[0].Mask   := emNoSpace;
  AquiferProps[0].Length := 0;
  N := High(AquiferProps);
  for I := 1 to High(AquiferProps) do
  begin
    AquiferProps[i].Name   := PropNames[i];
    AquiferProps[i].Style  := esEdit;
    AquiferProps[i].Mask   := emPosNumber;
    AquiferProps[i].Length := 0;
  end;
  AquiferProps[10].Mask := emNumber;   // Bottom elev. can be < 0
  AquiferProps[11].Mask := emNumber;   // GW table can be < 0
  AquiferProps[N].Style := esComboEdit;
  AquiferProps[N].List := Project.Lists[PATTERN].Text;
  BorderStyle := bsSizeToolWin;
end;

procedure TAquiferForm.FormDestroy(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnDestroy handler.
//-----------------------------------------------------------------------------
begin
  PropList.Free;
  PropEdit1.Free;
end;

procedure TAquiferForm.FormShow(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnShow handler.
//-----------------------------------------------------------------------------
begin
  PropEdit1.SetProps(AquiferProps, PropList);
  PropEdit1.Edit;
end;

procedure TAquiferForm.OKBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the OKBtn. Validates form's data entries.
//-----------------------------------------------------------------------------
begin
  if (not PropEdit1.IsValid) or (not ValidateInput) then
  begin
    ModalResult := mrNone;
    PropEdit1.Edit;
  end
  else ModalResult := mrOK;
end;

procedure TAquiferForm.ShowPropertyHint(Sender: TObject; aRow: LongInt);
//-----------------------------------------------------------------------------
//  OnRowSelect message handler for the form's PropEdit control. Displays
//  a hint message when a new property row of the PropEdit control is
//  selected.
//-----------------------------------------------------------------------------
begin
  HintLabel.Caption := AquiferHint[aRow];
end;

procedure TAquiferForm.SetData(const Index: Integer; A: TAquifer);
//-----------------------------------------------------------------------------
//  Loads current property values for Aquifer A with index Index into the form.
//-----------------------------------------------------------------------------
var
  K: Integer;
begin
  AquiferIndex := Index;
  if Index < 0 then
  begin
    for K := 0 to High(DefaultProps) do
      PropList.Add(DefaultProps[K]);
  end
  else
  begin
    PropList.Add(Project.Lists[AQUIFER].Strings[Index]);
    for K := 1 to High(DefaultProps) do PropList.Add(A.Data[K-1]);
  end;
end;

procedure TAquiferForm.GetData(var S: String; A: TAquifer);
//-----------------------------------------------------------------------------
//  Retrieves edited name (S) and property values for Aquifer A from the form.
//-----------------------------------------------------------------------------
var
  K: Integer;
begin
  S := PropList[0];
  for K := 1 to High(DefaultProps) do
    A.Data[K-1] := PropList[K];
end;

procedure TAquiferForm.ValidateEntry(Sender: TObject; Index: Integer;
  var S: String; var Errmsg: String; var IsValid: Boolean);
//-----------------------------------------------------------------------------
//  OnValidate handler for the PropEdit control.
//-----------------------------------------------------------------------------
begin
  IsValid := True;
  if Length(Trim(S)) = 0 then
  begin
    Errmsg := MSG_NO_DATA;
    IsValid := False;
  end;
end;

function TAquiferForm.ValidateInput: Boolean;
//-----------------------------------------------------------------------------
//  Validates the user's entry for the Aquifer name in the PropEdit control.
//-----------------------------------------------------------------------------
var
  S : String;
  I : Integer;
begin
  // Check that the field is not blank.
  Result := True;
  S := Trim(PropList[0]);
  if (Length(S) = 0) then
  begin
    Uutils.MsgDlg(MSG_INVALID_NAME, mtError, [mbOK]);
    Result := False;
    Exit;
  end;

  // Check that no other Aquifer has the same name.
  with Project.Lists[AQUIFER] do
  for I := 0 to Count-1 do
  begin
    if I = AquiferIndex then continue;
    if SameText(S, Strings[I]) then
    begin
      Uutils.MsgDlg(MSG_DUPLICATE_NAME, mtError, [mbOK]);
      Result := False;
      Exit;
    end;
  end;
end;

procedure TAquiferForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 212390);
end;

procedure TAquiferForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then HelpBtnClick(Sender);
end;

end.
