unit Uinlet;

{-------------------------------------------------------------------}
{                    Unit:    Uinlet.pas                            }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    05/02/22     (5.2.1)                  }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Delphi Pascal unit that manages data related to street and      }
{   and channel inlets.                                             }
{-------------------------------------------------------------------}

interface

uses
  Classes, Forms, SysUtils, Controls, Uproject, Uutils, Uglobals, Dialogs, Math,
  StrUtils;

const

  // Inlet Placement
  AUTOMATIC = 0;
  ON_GRADE  = 1;
  ON_SAG    = 2;

  PlacementKeywords: array[0..2] of String =
  ('AUTOMATIC', 'ON_GRADE', 'ON_SAG');

  // Inlet Types
  GRATE_INLET   = 0;
  CURB_INLET    = 1;
  COMBO_INLET   = 2;
  SLOTTED_INLET = 3;
  DROP_GRATE    = 4;
  DROP_CURB     = 5;
  CUSTOM_INLET  = 6;

  InletTypes: array[0..6] of String =
  ('GRATE', 'CURB OPENING', 'COMBINATION', 'SLOTTED DRAIN',
   'DROP GRATE', 'DROP CURB', 'CUSTOM');

  InletKeywords: array[0..7] of String =
    ('GRATE', 'CURB', '', 'SLOTTED', 'DROP_GRATE', 'DROP_CURB', 'CUSTOM', 'STREET');

  // Inlet Property Indexes
  GRATE_INLET_TYPE            = 0;
  GRATE_INLET_LENGTH          = 1;
  GRATE_INLET_WIDTH           = 2;
  GRATE_INLET_OPEN_FRAC       = 3;
  GRATE_INLET_SPLASH_VELOC    = 4;

  CURB_INLET_LENGTH           = 5;
  CURB_INLET_HEIGHT           = 6;
  CURB_INLET_THROAT           = 7;

  SLOTTED_INLET_LENGTH        = 8;
  SLOTTED_INLET_WIDTH         = 9;

  CUSTOM_INLET_DIVRSN_CURVE   = 10;
  CUSTOM_INLET_RATING_CURVE   = 11;
  MAX_INLET_PROPS             = 11;

  DefInlet: array[0..MAX_INLET_PROPS] of String =
    ('P_BAR-50', '2', '2', '0.8', '0',      // Grate
     '2', '0.5', 'VERTICAL',                // Curb Opening
     '4', '0.5',                            // Slotted
     '', '');                               // Custom

  GrateTypes: array[0..7] of String =
  ('P_BAR-50', 'P_BAR-50x100', 'P_BAR-30',
   'CURVED_VANE', 'TILT_BAR-45', 'TILT_BAR-30',
   'RETICULINE', 'GENERIC');

  ThroatTypes: array[0..2] of String =
  ('VERTICAL', 'INCLINED', 'HORIZONTAL');

  // Inlet Usage Indexes
  USAGE_NUM_INLETS     = 0;
  USAGE_PCNT_CLOGGED   = 1;
  USAGE_FLOW_RESTRICT  = 2;
  USAGE_DEPRESS_HEIGHT = 3;
  USAGE_DEPRESS_WIDTH  = 4;
  USAGE_PLACEMENT      = 5;
  USAGE_MAX_INDEX      = 5;

type

//------------
// Inlet class
//------------
  TInlet = class(TObject)
    InletType  : Integer;
    Data       : array [0..MAX_INLET_PROPS] of String;
    function     GetID: String;
    constructor  Create;
  end;

//------------------
// Inlet Usage class
//------------------
  TInletUsage = class(TObject)
    InletLink : TLink;
    InletNode : TNode;
    Inlet     : TInlet;
    Data      : array [0..USAGE_MAX_INDEX] of String;
  end;

//------------------
// Shared procedures
//------------------
  function  ReadInletDesignData(TokList: TStringList; const Ntoks: Integer): Integer;
  function  EditInletDesign(const I: Integer): String;
  procedure ExportInletDesigns(S: TStringList);
  function  ReadInletUsageData(TokList: TStringList; const Ntoks: Integer): Integer;
  function  EditInletUsage(const Index: Integer; var S: String;
            var Modified: Boolean): String;
  procedure ExportInletUsage(S: TStringList);
  function  GetInletNode(aLink: TLink): TNode;
  function  GetCaptureNode(aLink: TLink): TNode;
  function  DeleteInletsByNode(aNode: TNode): Boolean;
  function  DeleteInletsByType(aInletType: TInlet): Boolean;
  function  CheckForValidInlet(aLink: TLink): Boolean;

implementation

uses
  Fmain, Dinlet, Dinletusage, Ubrowser, Uupdate, Uimport;

//==============================================================================

constructor TInlet.Create;
begin
  inherited Create;
  InletType := CURB_INLET;
  Uutils.CopyStringArray(DefInlet, Data);
end;

function TInlet.GetID: String;
var
  I: Integer;
begin
  Result := '*';
  I := Project.Lists[INLET].IndexOfObject(self);
  if I >= 0 then Result := Project.Lists[INLET].Strings[I];
end;

//==============================================================================

function ReadGrateInletData(TokList: TStringList; const Ntoks: Integer;
  aInlet: TInlet; const InletType: Integer): Integer;
begin
  Result := 0;
  if Ntoks < 5 then Result := Uimport.ErrMsg(ITEMS_ERR, '') else
  begin
    aInlet.Data[GRATE_INLET_TYPE] := TokList[4];
    aInlet.Data[GRATE_INLET_LENGTH] := TokList[2];
    aInlet.Data[GRATE_INLET_WIDTH] := TokList[3];
    if SameText(TokList[4], 'GENERIC') then
    begin
      if Ntoks > 5 then
        aInlet.Data[GRATE_INLET_OPEN_FRAC] := TokList[5];
      if Ntoks > 6 then
        aInlet.Data[GRATE_INLET_SPLASH_VELOC] := TokList[6];
    end;
    if aInlet.InletType = CURB_INLET then
      aInlet.InletType := COMBO_INLET
    else
      aInlet.InletType := InletType;
  end;
end;

//==============================================================================

function ReadCurbInletData(TokList: TStringList; const Ntoks: Integer;
  aInlet: TInlet; const InletType: Integer): Integer;
begin
  Result := 0;
  if Ntoks < 4 then Result := Uimport.ErrMsg(ITEMS_ERR, '') else
  begin
    aInlet.Data[CURB_INLET_LENGTH] := TokList[2];
    aInlet.Data[CURB_INLET_HEIGHT] := TokList[3];
    if Ntoks > 4 then
      aInlet.Data[CURB_INLET_THROAT] := TokList[4];
    if aInlet.InletType = GRATE_INLET then
      aInlet.InletType := COMBO_INLET
    else
      aInlet.InletType := InletType;
  end;
end;

//==============================================================================

function ReadSlottedInletData(TokList: TStringList; const Ntoks: Integer;
  aInlet: TInlet): Integer;
begin
  Result := 0;
  if Ntoks < 4 then Result := Uimport.ErrMsg(ITEMS_ERR, '') else
  begin
    aInlet.Data[SLOTTED_INLET_LENGTH] := TokList[2];
    aInlet.Data[SLOTTED_INLET_WIDTH] := TokList[3];
    aInlet.InletType := SLOTTED_INLET;
  end;
end;

//==============================================================================

function ReadCustomInletData(TokList: TStringList; const Ntoks: Integer;
  aInlet: TInlet): Integer;
begin
  Result := 0;
  aInlet.Data[CUSTOM_INLET_DIVRSN_CURVE] := TokList[2];
  aInlet.Data[CUSTOM_INLET_RATING_CURVE] := TokList[2];
  aInlet.InletType := CUSTOM_INLET;
end;

//==============================================================================

function ReadInletDesignData(TokList: TStringList; const Ntoks: Integer): Integer;
var
  ID: String;
  InletType: Integer;
  Index: Integer;
  aInlet: TInlet;
begin
  Result := 0;
  if nToks < 3 then Result := Uimport.ErrMsg(ITEMS_ERR, '') else
  begin
    ID := TokList[0];
    InletType := AnsiIndexText(TokList[1], InletKeywords);
    if InletType < 0 then Result := Uimport.ErrMsg(KEYWORD_ERR, TokList[1]) else
    begin
      Index := Project.Lists[INLET].IndexOf(ID);
      if Index < 0 then
      begin
        aInlet := TInlet.Create;
        aInlet.InletType := -1;
        Project.Lists[INLET].AddObject(ID, aInlet);
        Project.HasItems[INLET] := True;
      end
      else aInlet := TInlet(Project.Lists[INLET].Objects[Index]);
      case InletType of
      0, 4: Result := ReadGrateInletData(TokList, Ntoks, aInlet, InletType);
      1, 5: Result := ReadCurbInletData(TokList, Ntoks, aInlet, InletType);
      3:    Result := ReadSlottedInletData(TokList, Ntoks, aInlet);
      6:    Result := ReadCustomInletData(TokList, Ntoks, aInlet);
      end;
    end;
  end;
end;

//==============================================================================

function  EditInletDesign(const I: Integer): String;
var
  Oldname : String;
  NewName : String;
  aInlet : TInlet;
  InletEditorForm: TInletEditorForm;
begin
  // If index I >= 0 get corresponding Inlet object
  Result := '';
  if I >= 0 then with Project.Lists[INLET] do
  begin
    aInlet := TInlet(Objects[I]);
    OldName := Strings[I];
  end

  // Otherwise create a new Inlet object
  else
  begin
    aInlet := TInlet.Create;
    OldName := '';
  end;

  // Create the Inlet Design Editor form
  InletEditorForm := TInletEditorForm.Create(Application);
  with InletEditorForm do
  try
    // Set properties in the Inlet Editor form
    SetData(I, OldName, aInlet);

    // If valid data entered
    if ShowModal = mrOK then
    begin
      // Retrieve properties from form
      GetData(NewName, aInlet);
      Result := NewName;

      // For new Inlet, add it to the project
      if I < 0 then
      begin
        Project.Lists[INLET].AddObject(NewName, aInlet);
        Ubrowser.BrowserAddItem(INLET, Project.Lists[INLET].Count-1);
      end

      // For existing Inlet, update the Project Browser
      else
      begin
        if OldName <> NewName then
        begin
          Project.Lists[INLET].Strings[I] := NewName;
        end;
        MainForm.ItemListBox.Refresh;
      end;
      if Modified then MainForm.SetChangeFlags;
    end

    // If editing cancelled, then delete new Inlet
    else if I < 0 then aInlet.Free;
  finally
    Free;
  end;
  Result := NewName;
end;

//==============================================================================

procedure ExportInletDesigns(S: TStringlist);
var
  I, J: Integer;
  Line: String;
  Tab: String;
  aInlet: TInlet;
begin
  if Project.Lists[INLET].Count = 0 then exit;
  if Uglobals.TabDelimited then Tab := #9 else Tab := ' ';
  S.Add('');
  S.Add('[INLETS]');
  Line := ';;Name          ' + Tab + 'Type            ' + Tab + 'Parameters:';
  S.Add(Line);
  Line := ';;--------------' + Tab + '----------------' + Tab + '-----------';
  S.Add(Line);
  with Project.Lists[INLET] do
  begin
    for I := 0 to Count-1 do
    begin
      aInlet := TInlet(Objects[I]);
      if (aInlet.InletType = GRATE_INLET) or
         (aInlet.InletType = COMBO_INLET) or
         (aInlet.InletType = DROP_GRATE) then
      begin
        Line := Format('%-16s', [Strings[I]]);
        if aInlet.InletType = DROP_GRATE
        then J := DROP_GRATE else J := GRATE_INLET;
        Line := Line + Tab + Format('%-16s', [InletKeywords[J]]);
        Line := Line + Tab + Format('%-8s', [aInlet.Data[GRATE_INLET_LENGTH]]);
        Line := Line + Tab + Format('%-8s', [aInlet.Data[GRATE_INLET_WIDTH]]);
        Line := Line + Tab + Format('%-16s', [aInlet.Data[GRATE_INLET_TYPE]]);
        if SameText(aInlet.Data[GRATE_INLET_TYPE], 'GENERIC') then
        begin
          Line := Line + Tab + Format('%-8s', [aInlet.Data[GRATE_INLET_OPEN_FRAC]]);
          Line := Line + Tab + Format('%-8s', [aInlet.Data[GRATE_INLET_SPLASH_VELOC]]);
        end;
        S.Add(Line);
      end;

      if (aInlet.InletType = CURB_INLET) or
         (aInlet.InletType = COMBO_INLET) or
         (aInlet.InletType = DROP_CURB) then
      begin
        Line := Format('%-16s', [Strings[I]]);
        if aInlet.InletType = DROP_CURB
        then J := DROP_CURB else J := CURB_INLET;
        Line := Line + Tab + Format('%-16s', [InletKeywords[J]]);
        Line := Line + Tab + Format('%-8s', [aInlet.Data[CURB_INLET_LENGTH]]);
        Line := Line + Tab + Format('%-8s', [aInlet.Data[CURB_INLET_HEIGHT]]);
        if (aInlet.InletType = CURB_INLET) or
           (aInlet.InletType = COMBO_INLET) then
          Line := Line + Tab + Format('%-8s', [aInlet.Data[CURB_INLET_THROAT]]);
        S.Add(Line);
      end;

      if aInlet.InletType = SLOTTED_INLET then
      begin
        Line := Format('%-16s', [Strings[I]]);
        Line := Line + Tab + Format('%-16s', [InletKeywords[SLOTTED_INLET]]);
        for J := SLOTTED_INLET_LENGTH to SLOTTED_INLET_WIDTH do
          Line := Line + Tab + Format('%-8s', [aInlet.Data[J]]);
        S.Add(Line);
      end;

      if aInlet.InletType = CUSTOM_INLET then
      begin
        Line := Format('%-16s', [Strings[I]]);
        Line := Line + Tab + Format('%-16s', [InletKeywords[CUSTOM_INLET]]);
        if Length(aInlet.Data[CUSTOM_INLET_DIVRSN_CURVE]) > 0 then
          Line := Line + Tab + Format('%-16s', [aInlet.Data[CUSTOM_INLET_DIVRSN_CURVE]])
        else
          Line := Line + Tab + Format('%-16s', [aInlet.Data[CUSTOM_INLET_RATING_CURVE]]);
        S.Add(Line);
      end;
    end;
  end;
end;

//==============================================================================

function ReadInletUsageData(TokList: TStringList; const Ntoks: Integer): Integer;
//
// Reads a tokenized line of data from the [INLET_USAGE] section of
// an input file with following format:
//
//  LinkID  InletID  NodeID  (#Inlets  %Clogged  MaxFlow)
//
var
  I, K, L: Integer;
  X: Single;
  UsageData: array[0..USAGE_MAX_INDEX] of String;
  aLink: TLink;
  aNode: TNode;
  aInlet: TInlet;
  InletUsage: TInletUsage;
begin
  Result := 0;
  if nToks < 3 then
  begin
    Result := Uimport.ErrMsg(ITEMS_ERR, '');
    exit;
  end;

  aLink := nil;
  aNode := nil;
  aInlet := nil;

  // Check that conduit Link exists
  L := Project.Lists[CONDUIT].IndexOf(TokList[0]);
  if L < 0 then Result := Uimport.ErrMsg(LINK_ERR, TokList[0])
  else aLink := TLink(Project.Lists[CONDUIT].Objects[L]);

  // Check that inlet exists
  if Result = 0 then
  begin
    I := Project.Lists[INLET].IndexOf(TokList[1]);
    if I < 0 then Result := Uimport.ErrMsg(INLET_ERR, TokList[1])
    else aInlet := TInlet(Project.Lists[Inlet].Objects[I]);
  end;

  // Check that capture node exists
  if Result = 0 then
  begin
    aNode := Project.GetNode(TokList[2]);
    if aNode = nil then Result := Uimport.ErrMsg(NODE_ERR, TokList[2]);
  end;
  if Result <> 0 then exit;

  // Assign defaults to optional parameters
  UsageData[USAGE_NUM_INLETS]     := '1';
  UsageData[USAGE_PCNT_CLOGGED]   := '0';
  Usagedata[USAGE_FLOW_RESTRICT]  := '0';
  UsageData[USAGE_DEPRESS_HEIGHT] := '0';
  UsageData[USAGE_DEPRESS_WIDTH]  := '0';
  UsageData[USAGE_PLACEMENT]      := 'AUTOMATIC';

  // Read optional parameters
  if (Result = 0) and (nToks > 3) then
  begin
    if (Uutils.GetSingle(TokList[3], X)) and (X > 0) then
      UsageData[USAGE_NUM_INLETS] := TokList[3]
    else
      Result := Uimport.ErrMsg(NUMBER_ERR, TokList[3]);
  end;

  if (Result = 0) and (nToks > 4) then
  begin
    if (Uutils.GetSingle(TokList[4], X)) and (X >= 0) then
      UsageData[USAGE_PCNT_CLOGGED] := TokList[4]
    else
      Result := Uimport.ErrMsg(NUMBER_ERR, TokList[4]);
  end;

  if (Result = 0) and (nToks > 5) then
  begin
    if (Uutils.GetSingle(TokList[5], X)) and (X >= 0) then
      Usagedata[USAGE_FLOW_RESTRICT] := TokList[5]
    else
      Result := Uimport.ErrMsg(NUMBER_ERR, TokList[5]);
  end;

  if (Result = 0) and (nToks > 6) then
  begin
    if nToks < 8 then Result := Uimport.ErrMsg(ITEMS_ERR, '') else
    begin
      if (Uutils.GetSingle(TokList[6], X)) and (X >= 0) then
        Usagedata[USAGE_DEPRESS_HEIGHT] := TokList[6]
      else
        Result := Uimport.ErrMsg(NUMBER_ERR, TokList[6]);
      if Result = 0 then
      begin
        if (Uutils.GetSingle(TokList[7], X)) and (X >= 0) then
          Usagedata[USAGE_DEPRESS_HEIGHT] := TokList[7]
        else
          Result := Uimport.ErrMsg(NUMBER_ERR, TokList[7]);
      end;
    end;
  end;

  if (Result = 0) and (nToks > 8) then
  begin
    if AnsiIndexText(TokList[8], PlacementKeywords) < 0
    then Result := Uimport.ErrMsg(KEYWORD_ERR, TokList[1])
    else UsageData[USAGE_PLACEMENT] := TokList[8];
  end;

  if Result <> 0 then exit;

  // Create and save inlet usage data
  InletUsage := TInletUsage(aLink.Inlet);
  if InletUsage = nil then
  begin
      InletUsage := TInletUsage.Create;
      aLink.Inlet := TClass(InletUsage);
  end;
  InletUsage.InletLink := aLink;
  InletUsage.InletNode := aNode;
  InletUsage.Inlet := aInlet;
  for K := 0 to USAGE_MAX_INDEX do
    InletUsage.Data[K] := UsageData[K];
  aLink.Data[CONDUIT_INLET_INDEX] := 'YES';
end;

//==============================================================================

function EditInletUsage(const Index: Integer; var S: String;
         var Modified: Boolean): String;
var
  I: Integer;
  InletType: Integer;
  XshapeType: Integer;
  XshapeText: String;
  InletDesigns: String;
  L: TLink;
  aInlet: TInlet;
begin
  // Determine conduit's xsection shape
  Result := '';
  L := TLink(Project.Lists[CONDUIT].Objects[Index]);
  XshapeText := L.Data[CONDUIT_SHAPE_INDEX];
  if SameText(XshapeText, 'STREET') then XshapeType := 1
  else if SameText(XshapeText, 'TRAPEZOIDAL') then XshapeType := 2
  else if SameText(XshapeText, 'RECT_OPEN') then XshapeType := 2
  else XshapeType := 3;

  // Find inlet designs compatible with conduit's shape
  InletDesigns := '';
  with Project.Lists[INLET] do
  begin
    for I := 0 to Count-1 do
    begin
      aInlet := TInlet(Objects[I]);
      if (aInlet.InletType = DROP_GRATE) or
         (aInlet.InletType = DROP_CURB) then InletType := 2
      else if aInlet.InletType = CUSTOM_INLET then InletType := 3
      else InletType := 1;
      if (InletType = XshapeType) or (InletType = 3) then
        InletDesigns := InletDesigns + #13 + Strings[I];
    end;
  end;

  // Exit with error message if there are no compatible inlet designs
  if Length(InletDesigns) = 0 then
  begin
    Result :='No compatible inlet designs have been' +
      #10'defined yet for this type of conduit';
    exit;
  end;

  // Otherwise launch Inlet Usage Editor
  with InletUsageForm do
  try
    SetData(Index, InletDesigns);
    Show;
  finally
  end;
end;

//==============================================================================

function FormatInletData(InletUsage: TInletUsage): String;
var
  Tab: String;
begin
  if Uglobals.TabDelimited then Tab := #9 else Tab := ' ';
  Result := Tab + Format('%-16s', [InletUsage.Inlet.GetID]);
  Result := Result + Tab + Format('%-16s', [InletUsage.InletNode.ID]);
  Result := Result + Tab + Format('%-9s', [Inletusage.Data[USAGE_NUM_INLETS]]);
  Result := Result + Tab + Format('%-9s', [Inletusage.Data[USAGE_PCNT_CLOGGED]]);
  Result := Result + Tab + Format('%-9s', [Inletusage.Data[USAGE_FLOW_RESTRICT]]);
  Result := Result + Tab + Format('%-9s', [Inletusage.Data[USAGE_DEPRESS_HEIGHT]]);
  Result := Result + Tab + Format('%-9s', [Inletusage.Data[USAGE_DEPRESS_WIDTH]]);
  if not SameText(InletUsage.Data[USAGE_PLACEMENT], PlacementKeywords[AUTOMATIC]) then
  Result := Result + Tab + Format('%-9s', [Inletusage.Data[USAGE_PLACEMENT]]);
end;

//==============================================================================

procedure ExportInletUsage(S: TStringList);
var
  I: Integer;
  Line: String;
  Tab: String;
  aLink: TLink;
  InletUsage: TInletUsage;
begin
  if Project.Lists[INLET].Count = 0 then exit;
  if Uglobals.TabDelimited then Tab := #9 else Tab := ' ';
  S.Add('');
  S.Add('[INLET_USAGE]');
  Line := ';;Conduit       ' + Tab +
          'Inlet           ' + Tab + 'Node            ' + Tab +
          'Number   ' + Tab + '%Clogged ' + Tab + 'Qmax     ' + Tab +
          'aLocal   ' + Tab + 'wLocal   ' + Tab + 'Placement';
  S.Add(Line);
  Line := ';;--------------' + Tab +
          '----------------' + Tab + '----------------' + Tab +
          '---------' + Tab + '---------' + Tab + '---------' + Tab +
          '---------' + Tab + '---------' + Tab + '---------' + Tab +
          '---------';
  S.Add(Line);
  with Project.Lists[CONDUIT] do
  begin
    for I := 0 to Count - 1 do
    begin
      aLink := TLink(Objects[I]);
      InletUsage := TInletUsage(aLink.Inlet);
      if InletUsage <> nil then
        S.Add(Format('%-16s', [Strings[I]]) + FormatInletData(InletUsage));
    end;
  end;
end;

//==============================================================================

function  GetInletNode(aLink: TLink): TNode;
var
  Inlet: TInletUsage;
begin
  Result := nil;
  Inlet := TInletUsage(aLink.Inlet);
  if Inlet <> nil then Result := Inlet.InletNode;
end;

//==============================================================================

function  GetCaptureNode(aLink: TLink): TNode;
var
  E1, E2: Extended;
begin
  Result := aLink.Node2;
  if Uutils.GetExtended(aLink.Node1.Data[NODE_INVERT_INDEX], E1) and
     Uutils.GetExtended(aLink.Node2.Data[NODE_INVERT_INDEX], E2) and
     (E1 < E2) then
     Result := aLink.Node1;
end;

//==============================================================================

function DeleteInletsByNode(aNode: TNode): Boolean;
//
//  Deletes all conduit inlets that have aNode as their capture node.
//  (Used when user deletes aNode from the project).
var
  I: Integer;
  Inlet: TInletUsage;
  aLink: TLink;
begin
  Result := false;
  for I := 0 to Project.Lists[CONDUIT].Count - 1 do
  begin
    aLink := TLink(Project.Lists[CONDUIT].Objects[I]);
    Inlet := TInletUsage(aLink.Inlet);
    if Inlet = nil then continue;
    if Inlet.InletNode = aNode then
    begin
      Inlet.Free;
      aLink.Inlet := nil;
      aLink.Data[CONDUIT_INLET_INDEX] := 'NO';
      Result := true;
    end;
  end;
end;

//==============================================================================

function DeleteInletsByType(aInletType: TInlet): Boolean;
var
  I: Integer;
  Inlet: TInletUsage;
  aLink: TLink;
begin
  Result := false;
  for I := 0 to Project.Lists[CONDUIT].Count - 1 do
  begin
    aLink := TLink(Project.Lists[CONDUIT].Objects[I]);
    Inlet := TInletUsage(aLink.Inlet);
    if Inlet = nil then continue;
    if Inlet.Inlet = aInletType then
    begin
      Inlet.Free;
      aLink.Inlet := nil;
      aLink.Data[CONDUIT_INLET_INDEX] := 'NO';
      Result := true;
    end;
  end;
end;

//==============================================================================

function  CheckForValidInlet(aLink: TLink): Boolean;
var
  S: String;
  Inlet: TInletUsage;
  InletType: Integer;
begin
  Result := True;
  S := aLink.Data[CONDUIT_SHAPE_INDEX];
  Inlet := TInletUsage(aLink.Inlet);
  if Inlet = nil then
    Result := False
  else begin
    InletType := Inlet.Inlet.InletType ;
    case InletType of
      GRATE_INLET .. SLOTTED_INLET:
        if SameText(S, 'STREET') then Result := False;
      DROP_GRATE .. DROP_CURB:
        if (SameText(S, 'RECT_OPEN')) or (SameText(S, 'TRAPEZOIDAL')) then
          Result := False;
      else Result := False;
    end;
  end;
  if Result then
  begin
    FreeAndNil(TInletUsage(aLink.Inlet));
    aLink.Data[CONDUIT_INLET_INDEX] := 'NO';
  end;
end;

end.
