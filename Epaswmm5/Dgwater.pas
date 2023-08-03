unit Dgwater;

{-------------------------------------------------------------------}
{                    Unit:    Dgwater.pas                           }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21     (5.2.0)                  }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form unit used to edit the groundwater flow properties   }
{   of a subcatchment.                                              }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Uproject, Uglobals, Uutils, PropEdit;

const
  MAX_GW_PROPS = 14;
  LAT_EQN_INDEX = 13;
  DEEP_EQN_INDEX = 14;


  Default_GW_Props: array[0..MAX_GW_PROPS] of String =
    ('', '*', '0', '0', '0', '0', '0', '0', '0', '',
    '', '', '', 'No', 'No');

type
  TGroundWaterForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Panel4: TPanel;
    HintLabel: TLabel;
    Panel3: TPanel;
    Panel5: TPanel;
    Label4: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Image1: TImage;
    Panel6: TPanel;
    OKBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    procedure OKBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    SubCatchIndex: Integer;
    GwaterProps: array[0..MAX_GW_PROPS] of TPropRecord;
    PropEdit1: TPropEdit;
    PropList: TStringlist;
    LatFlowEqn: String;
    DeepFlowEqn: String;
    procedure ButtonClick(Sender: TObject; Index: Integer;
      var S: String; var Modified: Boolean);
    procedure ShowPropertyHint(Sender: TObject; aRow: LongInt);
    procedure ValidateEntry(Sender: TObject; Index: Integer; var S: String;
              var Errmsg: String; var IsValid: Boolean);
  public
    { Public declarations }
    HasChanged: Boolean;
    procedure SetData(const Index: Integer);
    procedure GetData(const Index: Integer);
    procedure SetGroupData;
    procedure GetGroupData(var GWData: array of String);
  end;

//var
//  GroundWaterForm: TGroundWaterForm;

implementation

{$R *.dfm}

uses
  Dgweqn;

const
  TXT_PROPERTY = 'Property';
  TXT_VALUE = 'Value';
  MSG_NO_DATA = 'This data field cannot be blank.';

  PropNames: array[0..MAX_GW_PROPS] of String =
    ('Aquifer Name',
     'Receiving Node',
     'Surface Elevation',
     'A1 Coefficient',
     'B1 Exponent',
     'A2 Coefficient',
     'B2 Exponent',
     'A3 Coefficient',
     'Surface Water Depth',
     'Threshold Water Table Elev.',
     'Aquifer Bottom Elevation',
     'Initial Water Table Elev.',
     'Unsat. Zone Moisture',
     'Custom Lateral Flow Equation',
     'Custom Deep Flow Equation');

  GroundWaterHint: array[0..MAX_GW_PROPS] of String =
    ('Name of Aquifer object that lies below subcatchment. ' +
     'Leave blank for no groundwater.',
     'Name of node that receives groundwater flow.',
     'Elevation of the ground surface (ft or m).',
     'Groundwater influence multiplier.',
     'Groundwater influence exponent.',
     'Tailwater influence multiplier.',
     'Tailwater influence exponent.',
     'Combined groundwater/tailwater influence multiplier.',
     'Depth of surface water above channel bottom (ft or m). ' +
     'Enter 0 to use depth from flow routing.',
     'Minimum water table elevation for flow to occur (ft or m). ' +
     'Leave blank to use node''s invert elevation.',
     'Elevation of aquifer bottom (ft or m). ' +
     'Leave blank to use Aquifer value.',
     'Initial water table elevation (ft or m). ' +
     'Leave blank to use Aquifer value.',
     'Initial moisture content of the unsaturated upper zone (fraction). ' +
     'Leave blank to use Aquifer value.',
     'Click to supply a custom equation for lateral GW flow.',
     'Click to supply a custom equation for deep GW flow.'
     );

procedure TGroundWaterForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnCreate handler.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  // Create Property Editor
  PropEdit1 := TPropEdit.Create(self);
  with PropEdit1 do
  begin
    Parent := Panel2;
    Align := alClient;
    BorderStyle := bsNone;
    ColHeading1 := TXT_PROPERTY;
    ColHeading2 := TXT_VALUE;
    HeaderSplit := 67;
    ValueColor := clNavy;
    OnButtonClick := ButtonClick;
    OnValidate := ValidateEntry;
    OnRowSelect := ShowPropertyHint;
  end;

  // Create Property stringlist
  PropList := TStringlist.Create;

  // Set the attributes of the Aquifer name property
  GwaterProps[0].Name   := PropNames[0];
  GwaterProps[0].Style  := esComboEdit;
  GwaterProps[0].Mask   := emNoSpace;
  GwaterProps[0].Length := 0;
  GwaterProps[0].List   := Project.Lists[AQUIFER].Text;

  // Set the attributes of the Node name property
  GwaterProps[1].Name   := PropNames[1];
  GwaterProps[1].Style  := esEdit;
  GwaterProps[1].Mask   := emNoSpace;
  GwaterProps[1].Length := 0;

  // Set the attributes of the Surface Elev. & Flow Coeffs. properties
  for I := 2 to MAX_GW_PROPS-2 do
  begin
    GwaterProps[I].Name   := PropNames[I];
    GwaterProps[I].Style  := esEdit;
    GwaterProps[I].Mask   := emNumber;
    GwaterProps[I].Length := 0;
  end;

  // Set the attributes of the Custom Equation properties
  for I := LAT_EQN_INDEX TO DEEP_EQN_INDEX do
  begin
    GwaterProps[I].Name := PropNames[I];
    GwaterProps[I].Style := esButton;
    GwaterProps[I].Mask  := emNone;
    GwaterProps[I].Length := 0;
  end;
  LatFlowEqn := '';
  DeepFlowEqn := '';
end;

procedure TGroundWaterForm.FormShow(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnShow handler.
//-----------------------------------------------------------------------------
begin
  PropEdit1.SetProps(GwaterProps, PropList);
  PropEdit1.Edit;
end;

procedure TGroundWaterForm.FormDestroy(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnDestroy handler.
//-----------------------------------------------------------------------------
begin
  PropList.Free;
  PropEdit1.Free;
end;

procedure TGroundWaterForm.ButtonClick(Sender: TObject; Index: Integer;
      var S: String; var Modified: Boolean);
//-----------------------------------------------------------------------------
//  OnClick handler for the custom GW flow equation properties
//-----------------------------------------------------------------------------
var
  EqnEditor: TGWEqnForm;
  Eqn: String;
  CustomEqn: String;
begin
  if Index = LAT_EQN_INDEX
  then CustomEqn := Copy(LatFlowEqn, 1, MaxInt)
  else CustomEqn := Copy(DeepFlowEqn, 1, MaxInt);

  EqnEditor := TGWEqnForm.Create(self);
  Eqn := Copy(CustomEqn, 1, MaxInt);
  with EqnEditor do
  try
    SetupForm(Index-LAT_EQN_INDEX);
    SetEqn(Eqn);
    if (ShowModal = mrOK) then
    begin
      Eqn := GetEqn;
      if Eqn <> CustomEqn then
      begin
        CustomEqn := Copy(Eqn, 1, MaxInt);
        HasChanged := True;
      end;
      Modified := HasChanged;
    end;
  finally
    Free;
  end;
  if Length(CustomEqn) > 0 then S := 'Yes' else S := 'No';
  if Index = LAT_EQN_INDEX
  then LatFlowEqn := Copy(CustomEqn, 1, MaxInt)
  else DeepFlowEqn := Copy(CustomEqn, 1, MaxInt);

end;

procedure TGroundWaterForm.OKBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the OK button.
//-----------------------------------------------------------------------------
begin
  if (not PropEdit1.IsValid) then
  begin
    ModalResult := mrNone;
    PropEdit1.Edit;
  end
  else ModalResult := mrOK;
end;

procedure TGroundWaterForm.ShowPropertyHint(Sender: TObject; aRow: LongInt);
//-----------------------------------------------------------------------------
//  Property Editor's OnRowSelect handler.
//-----------------------------------------------------------------------------
begin
  HintLabel.Caption := GroundWaterHint[aRow];
end;

procedure TGroundWaterForm.SetData(const Index: Integer);
//-----------------------------------------------------------------------------
//  Loads the current GW flow properties associated with subcatchment
//  of given index into the form's Property Editor control.
//-----------------------------------------------------------------------------
var
  K: Integer;
  S: TSubcatch;
begin
  // Save index of subcatchment
  SubcatchIndex := Index;

  // First load default property values into the editor
  for K := 0 to High(Default_GW_Props) do PropList.Add(Default_GW_Props[K]);
  LatFlowEqn := '';
  DeepFlowEqn := '';

  // If subcatchment exists, load its properties into the editor
  if (Index >= 0) and (Index < Project.Lists[SUBCATCH].Count) then
  begin
    S := Project.GetSubcatch(SUBCATCH, Index);
    for K := 0 to S.Groundwater.Count-1 do
      PropList[K] := S.Groundwater[K];
    LatFlowEqn := S.GwLatFlowEqn;
    if Length(LatFlowEqn) > 0 then PropList[LAT_EQN_INDEX] := 'Yes'
    else PropList[LAT_EQN_INDEX] := 'No';
    DeepFlowEqn := S.GwDeepFlowEqn;
    if Length(DeepFlowEqn) > 0 then PropList[DEEP_EQN_INDEX] := 'Yes'
    else PropList[DEEP_EQN_INDEX] := 'No';
  end;
  HasChanged := False;
end;

procedure TGroundWaterForm.GetData(const Index: Integer);
//-----------------------------------------------------------------------------
//  Unloads the GW flow properties from the form's Property Editor control
//  into subcatchment of given index.
//-----------------------------------------------------------------------------
var
  K: Integer;
  S: TSubcatch;
begin
  if (Index >= 0) and (Index < Project.Lists[SUBCATCH].Count) then
  begin
    S := Project.GetSubcatch(SUBCATCH, Index);
    S.Groundwater.Clear;

    // Note: the last two properties in the editor's list is not part of the
    //       Groundwater string list
    if Length(Trim(PropList[0])) > 0
    then for K := 0 to LAT_EQN_INDEX-1 do S.Groundwater.Add(PropList[K]);
    S.GwLatFlowEqn := LatFlowEqn;
    S.GwDeepFlowEqn := DeepFlowEqn;
    if not HasChanged then HasChanged := PropEdit1.Modified;
  end;
end;

procedure TGroundWaterForm.SetGroupData;
//-----------------------------------------------------------------------------
//  Initializes the Groundwater properties when the form is being
//  used to group edit a collection of subcatchments.
//-----------------------------------------------------------------------------
var
  K: Integer;
begin
  Caption := 'Group Groundwater Editor';
  GwaterProps[MAX_GW_PROPS].Style := esReadOnly;
  for K := 0 to High(Default_GW_Props) do PropList.Add('*');
  PropEdit1.OnRowSelect := nil;
  HintLabel.Caption :=
    'Edit only the properties you wish to change. ' +
    'Leave Aquifer Name blank to remove Groundwater from the ' +
    'subcatchment group.';
end;

procedure TGroundWaterForm.GetGroupData(var GWData: array of String);
//-----------------------------------------------------------------------------
//  Copies the form's Groundwater property values to a string array.
//-----------------------------------------------------------------------------
var
  K: Integer;
begin
  for K := 0 to LAT_EQN_INDEX-1 do GWData[K] := PropList[K];
end;

procedure TGroundWaterForm.ValidateEntry(Sender: TObject; Index: Integer;
 var S: String; var Errmsg: String; var IsValid: Boolean);
begin
  IsValid := True;
end;

procedure TGroundWaterForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 212220);
end;

procedure TGroundWaterForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then HelpBtnClick(Sender);
end;

end.
