unit Ddefault;

{-------------------------------------------------------------------}
{                    Unit:    Ddefault.pas                          }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form unit that selects default settings for the current  }
{   project.                                                        }
{                                                                   }
{   The form contains a Tab control with three tabs. A PropEdit     }
{   control is placed on the Tab control and is used to set         }
{   defaults for ID prefixes, subcatchmnets, and nodes/links.       }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, PropEdit, Spin, ComCtrls, Uproject, Uglobals, Uutils;

type
  TDefaultsForm = class(TForm)
    BtnOK: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    CheckDefault: TCheckBox;
    TabControl1: TTabControl;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    PropList  : array[0..2] of TStringlist;
    DefShape  : array[0..4] of String;
    TmpInfil  : array[0..MAXINFILPROPS] of String;
    TmpUnitSystem: TUnitSystem;
    TmpOffsets: String;
    procedure ButtonClicked(Sender: TObject; Index: Integer;
              var S: String; var Modified: Boolean);
    procedure SetDefaults(const I: Integer);
    procedure GetDefaults;
    procedure ValidateOption(Sender: TObject; Index: Integer; var S: String;
              var Errmsg: String; var IsValid: Boolean);
    procedure EditInfil(var S: String);
    procedure EditXsection(var S: String);
  public
    { Public declarations }
    PropEdit1: TPropEdit;
    Modified: Boolean;
  end;

//var
//  DefaultsForm: TDefaultsForm;

implementation

{$R *.DFM}

uses Dinfil, Dxsect, Uinifile, Uupdate;

const
  MAXPREFIX         = 6; //Max. chars. in an ID prefix
  TXT_OBJECT        = 'Object';
  TXT_ID_PREFIX     = 'ID Prefix';
  TXT_PROPERTY      = 'Property';
  TXT_DEF_VALUE     = 'Default Value';
  TXT_OPTION        = 'Option';
  TXT_ROUTING: array[0..2] of PChar =
    ('Steady Flow', 'Kinematic Wave', 'Dynamic Wave');
  TXT_EQUATION: array[0..1] of PChar =
    ('Hazen-Williams', 'Darcy-Weisbach');

var
// A TPropRecord record determines how properties are displayed
// and edited in the PropEdit control (see PropEdit.pas unit).

  PrefixProps: array[0..9] of TPropRecord =
   ((Name:'Rain Gages';    Style:esEdit;    Mask:emNoSpace;  Length:MAXPREFIX),
    (Name:'Subcatchments'; Style:esEdit;    Mask:emNoSpace;  Length:MAXPREFIX),
    (Name:'Junctions';     Style:esEdit;    Mask:emNoSpace;  Length:MAXPREFIX),
    (Name:'Outfalls';      Style:esEdit;    Mask:emNoSpace;  Length:MAXPREFIX),
    (Name:'Dividers';      Style:esEdit;    Mask:emNoSpace;  Length:MAXPREFIX),
    (Name:'Storage Units'; Style:esEdit;    Mask:emNoSpace;  Length:MAXPREFIX),
    (Name:'Conduits';      Style:esEdit;    Mask:emNoSpace;  Length:MAXPREFIX),
    (Name:'Pumps';         Style:esEdit;    Mask:emNoSpace;  Length:MAXPREFIX),
    (Name:'Regulators';    Style:esEdit;    Mask:emNoSpace;  Length:MAXPREFIX),
    (Name:'ID Increment';  Style:esEdit;    Mask:emPosNumber));

  SubcatchProps: array[0..9] of TPropRecord =
    ((Name:'Area';              Style:esEdit;      Mask:emPosNumber; Length:0),
     (Name:'Width';             Style:esEdit;      Mask:emPosNumber; Length:0),
     (Name:'% Slope';           Style:esEdit;      Mask:emPosNumber; Length:0),
     (Name:'% Imperv';          Style:esEdit;      Mask:emPosNumber; Length:0),
     (Name:'N-Imperv';          Style:esEdit;      Mask:emPosNumber; Length:0),
     (Name:'N-Perv';            Style:esEdit;      Mask:emPosNumber; Length:0),
     (Name:'Dstore-Imperv';     Style:esEdit;      Mask:emPosNumber; Length:0),
     (Name:'Dstore-Perv';       Style:esEdit;      Mask:emPosNumber; Length:0),
     (Name:'%Zero-Imperv';      Style:esEdit;      Mask:emPosNumber; Length:0),
     (Name:'Infiltration Model';Style:esButton;    Mask:emNone;      Length:0));

  NodeLinkProps: array[0..9] of TPropRecord =
   ((Name:'Node Invert';       Style:esEdit;      Mask:emNumber),
    (Name:'Node Max. Depth';   Style:esEdit;      Mask:emPosNumber),
    (Name:'Node Ponded Area';  Style:esEdit;      Mask:emNumber),
    (Name:'Conduit Length';    Style:esEdit;      Mask:emPosNumber),
    (Name:'Conduit Geometry';  Style:esButton;    Mask:emNone;       Length:0),
    (Name:'Conduit Roughness'; Style:esEdit;      Mask:emPosNumber),
    (Name:'Flow Units';        Style:esComboList; Mask:emNone;       Length:0;
     List:'CFS'#13'GPM'#13'MGD'#13'CMS'#13'LPS'#13'MLD'),
    (Name:'Link Offsets';      Style:esComboList; Mask:emNone;       Length:0;
     List:'DEPTH'#13'ELEVATION'),
    (Name:'Routing Method';    Style:esComboList; Mask:emNone;       Length:0;
     List:'Steady Flow'#13'Kinematic Wave'#13'Dynamic Wave'),
    (Name:'Force Main Equation'; Style:esComboList; Mask:emNone;     Length:0;
     List:'Hazen-Williams'#13'Darcy-Weisbach'));

procedure TDefaultsForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
// Form's OnCreate event handler.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  // Create Property Editor
  PropEdit1 := TPropEdit.Create(self);
  with PropEdit1 do
  begin
    Parent := TabControl1;
    Align := alClient;
    BorderStyle := bsNone;
    ColHeading1 := TXT_OBJECT;
    ColHeading2 := TXT_ID_PREFIX;
    HeaderSplit := 50;
    ValueColor := clNavy;
    OnValidate := ValidateOption;
    OnButtonClick := ButtonClicked;
  end;

  // Load current default values into stringlists.
  for I := 0 to 2 do
  begin
    PropList[I] := TStringList.Create;
    SetDefaults(I);
  end;
  TmpUnitSystem := Uglobals.UnitSystem;
  TmpOffsets := Project.DefProp[OPTION].Data[LINK_OFFSETS_INDEX];
  Modified := False;
end;

procedure TDefaultsForm.FormShow(Sender: TObject);
//-----------------------------------------------------------------------------
// Form's OnShow event handler.
//-----------------------------------------------------------------------------
begin
  TabControl1.TabIndex := 0;
  TabControl1Change(Sender);
end;

procedure TDefaultsForm.FormDestroy(Sender: TObject);
//-----------------------------------------------------------------------------
// Form's OnDestroy event handler.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  for I := 0 to 2 do PropList[I].Free;
  PropEdit1.Free;
end;

procedure TDefaultsForm.SetDefaults(const I: Integer);
//-----------------------------------------------------------------------------
// Loads current set of default values into the work array of
// stringlists (PropList).
//-----------------------------------------------------------------------------
var
  J: Integer;
begin
  case I of
  // ID prefixes
  0: begin
       PropList[I].Add(Project.IDPrefix[RAINGAGE]);
       PropList[I].Add(Project.IDPrefix[SUBCATCH]);
       PropList[I].Add(Project.IDPrefix[JUNCTION]);
       PropList[I].Add(Project.IDPrefix[OUTFALL]);
       PropList[I].Add(Project.IDPrefix[DIVIDER]);
       PropList[I].Add(Project.IDPrefix[STORAGE]);
       PropList[I].Add(Project.IDPrefix[CONDUIT]);
       PropList[I].Add(Project.IDPrefix[PUMP]);
       PropList[I].Add(Project.IDPrefix[ORIFICE]);
       PropList[I].Add(IntToStr(Project.IDIncrement));
     end;

  // Subcatch parameters
  1: begin
     PropList[I].Add(Project.DefProp[SUBCATCH].Data[SUBCATCH_AREA_INDEX]);
     PropList[I].Add(Project.DefProp[SUBCATCH].Data[SUBCATCH_WIDTH_INDEX]);
     PropList[I].Add(Project.DefProp[SUBCATCH].Data[SUBCATCH_SLOPE_INDEX]);
     PropList[I].Add(Project.DefProp[SUBCATCH].Data[SUBCATCH_IMPERV_INDEX]);
     PropList[I].Add(Project.DefProp[SUBCATCH].Data[SUBCATCH_IMPERV_N_INDEX]);
     PropList[I].Add(Project.DefProp[SUBCATCH].Data[SUBCATCH_PERV_N_INDEX]);
     PropList[I].Add(Project.DefProp[SUBCATCH].Data[SUBCATCH_IMPERV_DS_INDEX]);
     PropList[I].Add(Project.DefProp[SUBCATCH].Data[SUBCATCH_PERV_DS_INDEX]);
     PropList[I].Add(Project.DefProp[SUBCATCH].Data[SUBCATCH_PCTZERO_INDEX]);
     PropList[I].Add(Project.DefProp[OPTION].Data[INFILTRATION_INDEX]);
     for J := 0 to MAXINFILPROPS do TmpInfil[J] := Uproject.DefInfil[J];
     end;

  // Node/link parameters
  2: begin
       PropList[I].Add(Project.DefProp[JUNCTION].Data[NODE_INVERT_INDEX]);
       PropList[I].Add(Project.DefProp[JUNCTION].Data[JUNCTION_MAX_DEPTH_INDEX]);
       PropList[I].Add(Project.DefProp[JUNCTION].Data[JUNCTION_PONDED_AREA_INDEX]);
       PropList[I].Add(Project.DefProp[CONDUIT].Data[CONDUIT_LENGTH_INDEX]);
       PropList[I].Add(Project.DefProp[CONDUIT].Data[CONDUIT_SHAPE_INDEX]);
       PropList[I].Add(Project.DefProp[CONDUIT].Data[CONDUIT_ROUGHNESS_INDEX]);
       PropList[I].Add(Project.DefProp[OPTION].Data[FLOW_UNITS_INDEX]);
       PropList[I].Add(Project.DefProp[OPTION].Data[LINK_OFFSETS_INDEX]);
       J := Uutils.FindKeyWord(Project.DefProp[OPTION].Data[ROUTING_MODEL_INDEX],
                               RoutingOptions, 2);
       if J < 0 then J := 0;
       PropList[I].Add(TXT_ROUTING[J]);

       if SameText(Project.DefProp[OPTION].Data[FORCE_MAIN_EQN_INDEX], 'H-W')
       then PropList[I].Add(TXT_EQUATION[0])
       else PropList[I].Add(TXT_EQUATION[1]);

       with Project.DefProp[CONDUIT] do
       begin
         DefShape[0] := Data[CONDUIT_SHAPE_INDEX];
         DefShape[1] := Data[CONDUIT_GEOM1_INDEX];
         DefShape[2] := Data[CONDUIT_GEOM2_INDEX];
         DefShape[3] := Data[CONDUIT_GEOM3_INDEX];
         DefShape[4] := Data[CONDUIT_GEOM4_INDEX];
       end;
     end;
  end;
end;

procedure TDefaultsForm.GetDefaults;
//-----------------------------------------------------------------------------
// Transfers values from work array of stringlists to the project's defaults.
//-----------------------------------------------------------------------------
var
  J, Code, V: Integer;
begin
  // ID Prefixes
  Project.IDPrefix[RAINGAGE] := PropList[0].Strings[0];
  Project.IDPrefix[SUBCATCH] := PropList[0].Strings[1];
  Project.IDPrefix[JUNCTION] := PropList[0].Strings[2];
  Project.IDPrefix[OUTFALL]  := PropList[0].Strings[3];
  Project.IDPrefix[DIVIDER]  := PropList[0].Strings[4];
  Project.IDPrefix[STORAGE]  := PropList[0].Strings[5];
  Project.IDPrefix[CONDUIT]  := PropList[0].Strings[6];
  Project.IDPrefix[PUMP]     := PropList[0].Strings[7];
  for J := ORIFICE to OUTLET do
    Project.IDPrefix[J] := PropList[0].Strings[8];
  Val(PropList[0].Strings[9], V, Code);
  if Code = 0 then
  begin
    if V <= 0 then V := 1;
    Project.IDIncrement := V;
    for J := 0 to MAXCLASS do
    begin
      if (Project.NextID[J] <= V)
      or (Project.Lists[J].Count = 0) then Project.NextID[J] := V
      else Project.NextID[J] := Project.NextID[J] + V;
    end;
  end;

  // Subcatch parameters
  Project.DefProp[SUBCATCH].Data[SUBCATCH_AREA_INDEX] := PropList[1].Strings[0];
  Project.DefProp[SUBCATCH].Data[SUBCATCH_WIDTH_INDEX] := PropList[1].Strings[1];
  Project.DefProp[SUBCATCH].Data[SUBCATCH_SLOPE_INDEX] := PropList[1].Strings[2];
  Project.DefProp[SUBCATCH].Data[SUBCATCH_IMPERV_INDEX] := PropList[1].Strings[3];
  Project.DefProp[SUBCATCH].Data[SUBCATCH_IMPERV_N_INDEX] := PropList[1].Strings[4];
  Project.DefProp[SUBCATCH].Data[SUBCATCH_PERV_N_INDEX] := PropList[1].Strings[5];
  Project.DefProp[SUBCATCH].Data[SUBCATCH_IMPERV_DS_INDEX] := PropList[1].Strings[6];
  Project.DefProp[SUBCATCH].Data[SUBCATCH_PERV_DS_INDEX] := PropList[1].Strings[7];
  Project.DefProp[SUBCATCH].Data[SUBCATCH_PCTZERO_INDEX] := PropList[1].Strings[8];
  Project.DefProp[OPTION].Data[INFILTRATION_INDEX] := PropList[1].Strings[9];
  for J := 0 to MAXINFILPROPS do Uproject.DefInfil[J] := TmpInfil[J];

  // Node/Link parameters:
  // Node invert elev.
  for J := JUNCTION to STORAGE do
  begin
    Project.DefProp[J].Data[NODE_INVERT_INDEX] := PropList[2].Strings[0];
  end;

  // Node max. depth
  Project.DefProp[JUNCTION].Data[JUNCTION_MAX_DEPTH_INDEX] := PropList[2].Strings[1];
  Project.DefProp[DIVIDER].Data[DIVIDER_MAX_DEPTH_INDEX] := PropList[2].Strings[1];
  Project.DefProp[STORAGE].Data[STORAGE_MAX_DEPTH_INDEX] := PropList[2].Strings[1];

  // Node ponded area
  Project.DefProp[JUNCTION].Data[JUNCTION_PONDED_AREA_INDEX] := PropList[2].Strings[2];
  Project.DefProp[DIVIDER].Data[DIVIDER_PONDED_AREA_INDEX] := PropList[2].Strings[2];

  // Conduit length
  Project.DefProp[CONDUIT].Data[CONDUIT_LENGTH_INDEX] := PropList[2].Strings[3];

  // Conduit shape
  // (PropList.Strings[4] is merely a placeholder for conduit shape parameters)
  with Project.DefProp[CONDUIT] do
  begin
    Data[CONDUIT_SHAPE_INDEX]   := DefShape[0];
    Data[CONDUIT_GEOM1_INDEX]   := DefShape[1];
    Data[CONDUIT_GEOM2_INDEX]   := DefShape[2];
    Data[CONDUIT_GEOM3_INDEX]   := DefShape[3];
    Data[CONDUIT_GEOM4_INDEX]   := DefShape[4];
  end;

  // Conduit Roughness
  Project.DefProp[CONDUIT].Data[CONDUIT_ROUGHNESS_INDEX] := PropList[2].Strings[5];

  // Project flow units
  Project.DefProp[OPTION].Data[FLOW_UNITS_INDEX] := PropList[2].Strings[6];

  // Link Offsets
  Project.DefProp[OPTION].Data[LINK_OFFSETS_INDEX] := PropList[2].Strings[7];

  // Project flow routing method
  for J := 0 to High(TXT_ROUTING) do
    if SameText(PropList[2].Strings[8], TXT_ROUTING[J]) then break;
  if J <= High(TXT_ROUTING) then
    Project.DefProp[OPTION].Data[ROUTING_MODEL_INDEX] := RoutingOptions[J];

  // Force Main Equation
  if SameText(PropList[2].Strings[9], TXT_EQUATION[0])
  then Project.DefProp[OPTION].Data[FORCE_MAIN_EQN_INDEX] := 'H-W'
  else Project.DefProp[OPTION].Data[FORCE_MAIN_EQN_INDEX] := 'D-W';

  // Update Project options
  Uupdate.UpdateInfilModel(Project.Options.Data[INFILTRATION_INDEX],
    Project.DefProp[OPTION].Data[INFILTRATION_INDEX]);
  Project.Options.Data[INFILTRATION_INDEX] :=
    Project.DefProp[OPTION].Data[INFILTRATION_INDEX];
  Project.Options.Data[FLOW_UNITS_INDEX] :=
    Project.DefProp[OPTION].Data[FLOW_UNITS_INDEX];
  Project.Options.Data[LINK_OFFSETS_INDEX] :=
    Project.DefProp[OPTION].Data[LINK_OFFSETS_INDEX];
  Project.Options.Data[ROUTING_MODEL_INDEX] :=
    Project.DefProp[OPTION].Data[ROUTING_MODEL_INDEX];
  Project.Options.Data[FORCE_MAIN_EQN_INDEX] :=
    Project.DefProp[OPTION].Data[FORCE_MAIN_EQN_INDEX];
end;

procedure TDefaultsForm.BtnOKClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick event handler for the OK button.
//-----------------------------------------------------------------------------
begin
  // Validate last entry in the PropEdit control
  if not PropEdit1.IsValid then
  begin
    PropEdit1.Edit;
    ModalResult := mrNone;
    Exit;
  end;

  // Retrieve updated project defaults
  GetDefaults;

  // Set the global Modified flag if edits were made
  if PropEdit1.Modified then Modified := True;

  // If Default checkbox checked then save defaults to file
  Uupdate.UpdateUnits;
  Uupdate.UpdateLinkHints;
  if not SameText(tmpOffsets, Project.DefProp[OPTION].Data[LINK_OFFSETS_INDEX])
  then Uupdate.UpdateOffsets;
  if CheckDefault.Checked then Uinifile.SaveDefaults;
  ModalResult := mrOK;
end;

procedure TDefaultsForm.BtnCancelClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick event handler for the Cancel button.
//-----------------------------------------------------------------------------
begin
  Uglobals.UnitSystem := TmpUnitSystem;
  ModalResult := mrCancel;
end;

procedure TDefaultsForm.TabControl1Change(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick event handler for the TabControl1.
// Switches the set of default properties edited in PropEdit1.
//-----------------------------------------------------------------------------
begin
  PropEdit1.IsValid;
  case TabControl1.TabIndex of
  0:  begin
        PropEdit1.ColHeading1 := TXT_OBJECT;;
        PropEdit1.ColHeading2 := TXT_ID_PREFIX;
        PropEdit1.SetProps(PrefixProps, PropList[0]);
        PropEdit1.Edit;
      end;
  1:  begin
        PropEdit1.ColHeading1 := TXT_PROPERTY;
        PropEdit1.ColHeading2 := TXT_DEF_VALUE;
        PropEdit1.SetProps(SubcatchProps, PropList[1]);
        PropEdit1.Edit;
      end;
  2:  begin
        PropEdit1.ColHeading1 := TXT_OPTION;
        PropEdit1.ColHeading2 := TXT_DEF_VALUE;
        PropEdit1.SetProps(NodeLinkProps, PropList[2]);
        PropEdit1.Edit;
      end;
  end;
end;

procedure TDefaultsForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
//-----------------------------------------------------------------------------
// OnKeyDown event handler for the Form (KeyPreview was set to True).
// Allows use of Ctrl-Tab keystroke to change tabs.
//-----------------------------------------------------------------------------
begin
  if (ssCtrl in Shift) and (Key = VK_TAB) then with TabControl1 do
  begin
    if TabIndex < Tabs.Count - 1 then TabIndex := TabIndex + 1
    else TabIndex := 0;
    TabControl1Change(Sender);
  end;
  if Key = VK_F1 then BtnHelpClick(Sender);
end;

procedure TDefaultsForm.ValidateOption(Sender: TObject; Index: Integer;
  var S: String; var Errmsg: String; var IsValid: Boolean);
//-----------------------------------------------------------------------------
// OnValidate event handler for the TPropEdit editor component.
//-----------------------------------------------------------------------------
begin
  IsValid := True;
  if (TabControl1.TabIndex > 0) and (Length(Trim(S)) = 0) then
  begin
    Errmsg := 'This field cannot be left blank.';
    IsValid := False;
  end
  else if (TabControl1.TabIndex = 2) and (Index = 6) then
  begin
    if Uutils.FindKeyWord(S, SIFlowUnits, 3) >= 0
    then Uglobals.UnitSystem := usSI
    else Uglobals.UnitSystem := usUS;
  end;
end;

procedure TDefaultsForm.ButtonClicked(Sender: TObject; Index: Integer;
  var S: String; var Modified: Boolean);
//-----------------------------------------------------------------------------
// OnButtonClick event handler for the TPropEdit editor component.
//-----------------------------------------------------------------------------
begin
  if TabControl1.TabIndex = 1 then EditInfil(S) else
  if TabControl1.TabIndex = 2 then EditXsection(S);
end;

procedure TDefaultsForm.EditInfil(var S: String);
//-----------------------------------------------------------------------------
// Launches the Infiltration Editor to set default infiltration parameters.
//-----------------------------------------------------------------------------
var
  InfilForm: TInfilForm;
begin
  InfilForm := TInfilForm.Create(Application);
  try
    InfilForm.SetInfilModel(S);
    InfilForm.SetData(TmpInfil);
    if InfilForm.ShowModal = mrOK then
    begin
      InfilForm.GetInfilModelName(S);
      InfilForm.GetData(TmpInfil);
    end;
  finally
    InfilForm.Free;
  end;
end;

procedure TDefaultsForm.EditXsection(var S: String);
//-----------------------------------------------------------------------------
// Launches the Xsection Editor set default xsection properties.
//-----------------------------------------------------------------------------
var
  S1, S2: String;
  XsectionForm: TXsectionForm;
begin
  XsectionForm := TXsectionForm.Create(Application);
  try
    XsectionForm.SetData(DefShape[0], DefShape[1], DefShape[2],
                         DefShape[3], DefShape[4], '', '');
    if XsectionForm.ShowModal = mrOK then
    begin
      XsectionForm.GetData(DefShape[0], DefShape[1], DefShape[2],
                           DefShape[3], DefShape[4], S1, S2);
      S := DefShape[0];
    end;
  finally
    XsectionForm.Free;
  end;
end;

procedure TDefaultsForm.BtnHelpClick(Sender: TObject);
begin
  Case TabControl1.TabIndex of
    0:  Application.HelpCommand(HELP_CONTEXT, 210930);
    1:  Application.HelpCommand(HELP_CONTEXT, 210940);
    2:  Application.HelpCommand(HELP_CONTEXT, 210950);
  end;
end;

end.
