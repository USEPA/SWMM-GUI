unit Uupdate;

{-------------------------------------------------------------------}
{                    Unit:    Uupdate.pas                           }
{                    Project: EPA SMM                               }
{                    Version: 5.2                                   }
{                    Date:    11/01/21   (5.2.0)                    }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Delphi Pascal unit that updates references to names of data     }
{   objects that change during an editing session.                  }
{-------------------------------------------------------------------}

interface

uses
  SysUtils, Windows, Forms, Messages, Classes, Math, Controls, Dialogs,
  StrUtils, Uglobals, Uproject, Uutils, Uvertex;

procedure RemoveName(const ObjType: Integer; const OldName: String);

procedure UpdateAllLengths;
procedure UpdateAquiferName(const OldName: String; const NewName: String);

procedure UpdateCurveName(const OldName: String; const NewName: String);
procedure UpdateDefOptions;

procedure UpdateHydrographName(const OldName: String; const NewName: String);
procedure UpdateInfilModel(const OldModel: String; const NewModel: String);

procedure UpdateLanduseName(const OldName: String; const NewName: String);
function  UpdateLabelAnchor(L: TMapLabel; const S: String;
          var Errmsg: String): Boolean;
procedure UpdateLinkColor(const Ltype: Integer; const Index: Integer;
          K: Integer);
procedure UpdateLinkHints;
procedure UpdateLinkName(const OldName: String; const NewName: String);
function  UpdateLinkNode(const Ltype: Integer; const Index: Integer;
          const I: LongInt; const S: String; var Errmsg: String): Boolean;

procedure UpdateMapUnits;
procedure UpdateOffsets;
procedure UpdateObjectLength(const ObjType: Integer; const Index: Integer);

procedure UpdateNodeColor(const Ntype: Integer; const Index: Integer;
          K: Integer);
procedure UpdateNodeName(const OldName: String; const NewName: String);

procedure UpdatePatternName(const OldName: String; const NewName: String);
procedure UpdatePollutName(const OldName: String; const NewName: String);

procedure UpdateRainGageName(const OldName: String; const NewName: String);

procedure UpdateSnowpackName(const OldName: String; const NewName: String);
procedure UpdateSubcatchColor(const Index: Integer;  K: Integer);
function  UpdateSubcatchOutlet(C: TSubcatch; const S: String;
          var Errmsg: String): Boolean;

procedure UpdateTransectName(const OldName: String; const NewName: String);
procedure UpdateTseriesName(const OldName: String; const NewName: String);
procedure UpdateUnits;
procedure UpdateWeirShape(const Index: Integer);

implementation

uses
  Fmain, Fmap, Uedit, Umap, Uoutput, Dreporting, Ulid;

const
  MSG_NO_NODE_NAMED = 'There is no node named ';
  MSG_BAD_CONNECTION = 'Node/Subcatchment cannot be connected to itself.';
  MSG_NO_SUBCATCH_NAMED = 'There is no node or subcatchment named ';


procedure RemoveName(const ObjType: Integer; const OldName: String);
//-----------------------------------------------------------------------------
//  Removes all references to Object of type ObjType with name OldName
//-----------------------------------------------------------------------------
begin
  if Project.IsSubcatch(ObjType) then
  begin
    ReportingForm.RemoveItem(SUBCATCHMENTS, OldName);
  end

  else if Project.IsNode(ObjType) then
  begin
    UpdateNodeName(OldName, '');
    ReportingForm.RemoveItem(NODES, OldName);
  end

  else if Project.IsLink(ObjType) then
  begin
    UpdateLinkName(OldName, '');
    ReportingForm.RemoveItem(LINKS, OldName);
  end

  else case ObjType of
  RAINGAGE:   UpdateRainGageName(OldName, '');
  POLLUTANT:  UpdatePollutName(OldName, '');
  AQUIFER:    UpdateAquiferName(OldName, '');
  SNOWPACK:   UpdateSnowpackName(OldName, '');
  HYDROGRAPH: UpdateHydrographName(OldName, '');
  LANDUSE:    UpdateLanduseName(OldName, '');
  PATTERN:    UpdatePatternName(OldName, '');
  LID:        Ulid.UpdateLIDName(OldName, '');
  end;
end;


procedure UpdatePollutName(const OldName: String; const NewName: String);
//-----------------------------------------------------------------------------
//  Replaces all references to Pollutant OldName with NewName
//-----------------------------------------------------------------------------
var
  I: Integer;
  J: Integer;
  K: Integer;
  N: Integer;
  P: TPollutant;
  U: TLanduse;
  aNode: TNode;
  IsBlank: Boolean;
begin
  // Change name for co-pollutants
  IsBlank := (Length(NewName) = 0);
  N := Project.Lists[POLLUTANT].Count-1;
  for I := 0 to N do
  begin
    P := TPollutant(Project.Lists[POLLUTANT].Objects[I]);
    if SameText(P.Data[POLLUT_COPOLLUT_INDEX], OldName) then
    begin
      P.Data[POLLUT_COPOLLUT_INDEX] := NewName;
      if IsBlank then P.Data[POLLUT_FRACTION_INDEX] := '';
    end;
  end;

  // Change name used with Landuse nonpoint source objects
  for J := 0 to Project.Lists[LANDUSE].Count-1 do
  begin
    U := TLanduse(Project.Lists[LANDUSE].Objects[J]);
    for I := 0 to N do
    begin
      if SameText(U.NonPointSources[I], OldName) then
        U.NonpointSources[I] := NewName;
    end;
  end;

  // Change name used with Node's Treatment & Inflows properties
  for I := JUNCTION to STORAGE do
  begin
    N := Project.Lists[I].Count - 1;
    for J := 0 to N do
    begin
      aNode := Project.GetNode(I, J);
      with aNode.Treatment do
      begin
        K := IndexOfName(OldName);
        if K >= 0 then
        begin
          if IsBlank then Delete(K)
          else Strings[K] := NewName + '=' + ValueFromIndex[K];
        end;
      end;
      with aNode.DXInflow do
      begin
        K := IndexOfName(OldName);
        if K >= 0 then
        begin
          if IsBlank then Delete(K)
          else Strings[K] := NewName + '=' + ValueFromIndex[K];
        end;
      end;
      with aNode.DWInflow do
      begin
        K := IndexOfName(OldName);
        if K >= 0 then
        begin
          if IsBlank then Delete(K)
          else Strings[K] := NewName + '=' + ValueFromIndex[K];
        end;
      end;
    end;
  end;

  // Change name used with Subcatchment's initial Loadings
  for J := 0 to Project.Lists[SUBCATCH].Count - 1 do
  begin
    with Project.GetSubcatch(SUBCATCH, J).Loadings do
    begin
      K := IndexOfName(OldName);
      if K >= 0 then
        if IsBlank then Delete(K)
        else Strings[K] := NewName + '=' + Values[OldName];
    end;
  end;

  // Change name associated with LID drain pollutant removals
  for J := 0 to Project.Lists[LID].Count-1 do
  begin
    with TLid(Project.Lists[LID].Objects[J]).DrainRemovals do
    begin
      K := IndexOfName(OldName);
      if K >= 0 then
        if IsBlank then Delete(K)
        else Strings[K] := NewName + '=' + Values[OldName];
    end;
  end;
end;


procedure UpdateTseriesName(const OldName: String; const NewName: String);
//-----------------------------------------------------------------------------
//  Replaces all references to Timeseries OldName with NewName
//-----------------------------------------------------------------------------
var
  I: Integer;
  J: Integer;
  K: Integer;
  S: String;
  aNode: TNode;
begin
  for J := 0 to Project.Lists[RAINGAGE].Count - 1 do
  begin
    with TRaingage(Project.Lists[RAINGAGE].Objects[J]) do
      if SameText(Data[GAGE_SERIES_NAME], OldName) then
        Data[GAGE_SERIES_NAME] := NewName;
  end;
  if SameText(Project.Climatology.TempTseries, OldName) then
    Project.Climatology.TempTseries := NewName;
  if SameText(Project.Climatology.EvapTseries, OldName) then
    Project.Climatology.EvapTseries := NewName;
  for J := 0 to Project.Lists[OUTFALL].Count - 1 do
  begin
    with Project.GetNode(OUTFALL, J) do
      if SameText(Data[OUTFALL_TIME_SERIES_INDEX], OldName) then
        Data[OUTFALL_TIME_SERIES_INDEX] := NewName;
  end;
  for I := JUNCTION to STORAGE do
  begin
    for J := 0 to Project.Lists[I].Count - 1 do
    begin
      aNode := Project.GetNode(I, J);
      for K := 0 to aNode.DXInflow.Count - 1 do
      begin
        S := aNode.DXInflow.ValueFromIndex[K];
        aNode.DXInflow.ValueFromIndex[K] :=
          StringReplace(S, OldName, NewName, [rfIgnoreCase]);
      end;
    end;
  end;
end;


procedure UpdateCurveName(const OldName: String; const NewName: String);
//-----------------------------------------------------------------------------
//  Replaces all references to Curve OldName with NewName
//-----------------------------------------------------------------------------
var
  J: Integer;
begin
  // Storage area curves
  for J := 0 to Project.Lists[STORAGE].Count - 1 do
  begin
    with Project.GetNode(STORAGE, J) do
      if SameText(Data[STORAGE_ATABLE_INDEX], OldName) then
        Data[STORAGE_ATABLE_INDEX] := NewName;
  end;

  // Divider flow curves
  for J := 0 to Project.Lists[DIVIDER].Count - 1 do
  begin
    with Project.GetNode(DIVIDER, J) do
      if SameText(Data[DIVIDER_TABLE_INDEX], OldName) then
        Data[DIVIDER_TABLE_INDEX] := NewName;
  end;

  // Outfall tide curves
  for J := 0 to Project.Lists[OUTFALL].Count - 1 do
  begin
    with Project.GetNode(OUTFALL, J) do
      if SameText(Data[OUTFALL_TIDE_TABLE_INDEX], OldName) then
        Data[OUTFALL_TIDE_TABLE_INDEX] := NewName;
  end;

  // Pump curves
  for J := 0 to Project.Lists[PUMP].Count - 1 do
  begin
    with Project.GetLink(PUMP, J) do
      if SameText(Data[PUMP_CURVE_INDEX], OldName) then
        Data[PUMP_CURVE_INDEX] := NewName;
  end;

  // Outlet rating curves
  for J := 0 to Project.Lists[OUTLET].Count - 1 do
  begin
    with Project.GetLink(OUTLET, J) do
      if SameText(Data[OUTLET_QTABLE_INDEX], OldName) then
        Data[OUTLET_QTABLE_INDEX] := NewName;
  end;
end;


procedure UpdateAquiferName(const OldName: String; const NewName: String);
//-----------------------------------------------------------------------------
//  Replaces all references to Aquifer OldName with NewName
//-----------------------------------------------------------------------------
var
  J: Integer;
  IsBlank: Boolean;
begin
  IsBlank := (Length(NewName) = 0);
  for J := 0 to Project.Lists[SUBCATCH].Count - 1 do
  begin
    with Project.GetSubcatch(SUBCATCH, J).Groundwater do
    begin
      if (Count > 0) and SameText(Strings[0], OldName) then
      begin
        if IsBlank then Clear else Strings[0] := NewName;
      end;
    end;
  end;
end;


procedure UpdateSnowpackName(const OldName: String; const NewName: String);
//-----------------------------------------------------------------------------
//  Replaces all references to Snowpack OldName with NewName
//-----------------------------------------------------------------------------
var
  J: Integer;
begin
  for J := 0 to Project.Lists[SUBCATCH].Count - 1 do
  begin
    with Project.GetSubcatch(SUBCATCH, J) do
    begin
      if SameText(Data[SUBCATCH_SNOWPACK_INDEX], OldName) then
        Data[SUBCATCH_SNOWPACK_INDEX] := NewName;
    end;
  end;
end;


procedure UpdateHydrographName(const OldName: String; const NewName: String);
//-----------------------------------------------------------------------------
//  Replaces all references to Hydrograph OldName with NewName
//-----------------------------------------------------------------------------
var
  I: Integer;
  J: Integer;
  IsBlank: Boolean;
begin
  IsBlank := (Length(NewName) = 0);
  for I := JUNCTION to STORAGE do
  begin
    for J := 0 to Project.Lists[I].Count - 1 do
      with Project.GetNode(I, J).IIInflow do
      begin
        if (Count > 0) and SameText(Strings[0], OldName) then
        begin
          if IsBlank then Clear else Strings[0] := NewName;
        end;
      end;
  end;
end;


procedure UpdateLanduseName(const OldName: String; const NewName: String);
//-----------------------------------------------------------------------------
//  Replaces all references to Landuse OldName with NewName
//-----------------------------------------------------------------------------
var
  J: Integer;
  K: Integer;
  IsBlank: Boolean;
begin
  IsBlank := (Length(NewName) = 0);
  for J := 0 to Project.Lists[SUBCATCH].Count - 1 do
  begin
    with Project.GetSubcatch(SUBCATCH, J).Landuses do
    begin
      K := IndexOfName(OldName);
      if K >= 0 then
      begin
        if IsBlank then Delete(K)
        else Strings[K] := NewName + '=' + ValueFromIndex[K];
      end;
    end;
  end;
end;


procedure UpdatePatternName(const OldName: String; const NewName: String);
//-----------------------------------------------------------------------------
//  Replaces all references to Pattern OldName with NewName
//-----------------------------------------------------------------------------
var
  I, J, K   : Integer;
  S, S1, S2 : String;
begin
  S1 := #13 + OldName + #13;
  S2 := #13 + NewName + #13;
  for I := JUNCTION to STORAGE do
  begin
    for J := 0 to Project.Lists[I].Count - 1 do
      with Project.GetNode(I, J).DWInflow do
      begin
        for K := 0 to Count-1 do
        begin
          S := ValueFromIndex[K];
          S := #13 + S + #13;
          S := StringReplace(S, S1, S2, [rfReplaceAll, rfIgnoreCase]);
          ValueFromIndex[K] := Trim(S);
        end;
      end;
  end;
end;


procedure UpdateTransectName(const OldName: String; const NewName: String);
//-----------------------------------------------------------------------------
//  Replaces all references to Transect OldName with NewName
//-----------------------------------------------------------------------------
var
  J: Integer;
begin
  for J := 0 to Project.Lists[CONDUIT].Count - 1 do
  begin
    with Project.GetLink(CONDUIT, J) do
    begin
      if SameText(Data[CONDUIT_TSECT_INDEX], OldName)
      then Data[CONDUIT_TSECT_INDEX] := NewName;
    end;
  end;
end;


procedure UpdateRainGageName(const OldName: String; const NewName: String);
//-----------------------------------------------------------------------------
//  Replaces all references to Raingage OldName with NewName
//-----------------------------------------------------------------------------
var
  J: Integer;
  S: String;
begin
  if Length(NewName) = 0 then S := '*' else S := NewName;
  for J := 0 to Project.Lists[SUBCATCH].Count - 1 do
  begin
    with Project.GetSubcatch(SUBCATCH, J) do
    begin
      if SameText(Data[SUBCATCH_RAINGAGE_INDEX], OldName)
      then Data[SUBCATCH_RAINGAGE_INDEX] := S;
    end;
  end;
end;


procedure UpdateNodeName(const OldName: String; const NewName: String);
//-----------------------------------------------------------------------------
//  Replaces all references to Node OldName with NewName
//-----------------------------------------------------------------------------
var
  J: Integer;
  S: String;
begin
  if Length(NewName) = 0 then S := '*' else S := NewName;
  for J := 0 to Project.Lists[SUBCATCH].Count - 1 do
  begin
    with Project.GetSubcatch(SUBCATCH, J) do
    begin
      if SameText(Data[SUBCATCH_OUTLET_INDEX], OldName)
      then Data[SUBCATCH_OUTLET_INDEX] := S;
      if (Groundwater.Count > 0)
      and SameText(Groundwater.Strings[1], OldName)
      then Groundwater.Strings[1] := S;
    end;
  end;
end;


procedure UpdateLinkName(const OldName: String; const NewName: String);
//-----------------------------------------------------------------------------
//  Replaces all references to Link OldName with NewName
//-----------------------------------------------------------------------------
var
  J: Integer;
  S: String;
begin
  if Length(NewName) = 0 then S := '*' else S := NewName;
  for J := 0 to Project.Lists[DIVIDER].Count - 1 do
  begin
    with Project.GetNode(DIVIDER, J) do
    begin
      if SameText(Data[DIVIDER_LINK_INDEX], OldName)
      then Data[DIVIDER_LINK_INDEX] := S;
    end;
  end;
end;


function UpdateLinkNode(const Ltype: Integer; const Index: Integer;
  const I: LongInt; const S: String; var Errmsg: String): Boolean;
//-----------------------------------------------------------------------------
//  Updates a link's node I (UP or DOWN) to node S & redraws link.
//-----------------------------------------------------------------------------
var
  Ntype  : Integer;
  Index2 : Integer;
  L      : TLink;
  N      : TNode;
  R      : TRect;

begin
  // New node must be in project's database
  if not Project.FindNode(S, Ntype, Index2) then
  begin
    Errmsg := MSG_NO_NODE_NAMED + S;
    Result := False;
    Exit;
  end;

  // Both end nodes cannot be the same
  L := Project.GetLink(Ltype, Index);
  N := Project.GetNode(Ntype, Index2);
  if ((I = UP_INDEX) and (N = L.Node2))
  or  (N = L.Node1) then
  begin
    ErrMsg := MSG_BAD_CONNECTION;
    Result := False;
    Exit;
  end;

  // Replace old end node with new one.
  // Get bounding rectangle of current link connection.
  MapForm.HiliteOff;
  R := MapForm.Map.GetAdjacencyRect(Ltype, Index, False);
  if I = UP_INDEX then L.Node1 := N
  else                 L.Node2 := N;

  // Union new bounding rectangle with old one & redraw map.
  if UnionRect(R, R, MapForm.Map.GetAdjacencyRect(Ltype, Index, False))
  then MapForm.InvalidateMap(R);
  MapForm.HiliteOn;

  // Update conduit length if AutoLength is on
  if AutoLength and (Ltype = CONDUIT)
  then Uupdate.UpdateObjectLength(Ltype, Index);
  Result := True;
end;


function UpdateSubcatchOutlet(C: TSubcatch; const S: String;
  var Errmsg: String): Boolean;
//-----------------------------------------------------------------------------
//  Updates a subcatchment's outlet node (or subcatchment).
//-----------------------------------------------------------------------------
var
  N2: TNode;
  C2: TSubcatch;
  I, Ntype: Integer;
begin
  // Get location of existing outlet
  N2 := nil;
  C2 := nil;

  // Check that outlet is a Node or a Subcatchment
  if Project.FindNode(S, Ntype, I)
  then N2 := Project.GetNode(Ntype, I)
  else if Project.FindSubcatch(S, Ntype, I)
  then C2 := Project.GetSubcatch(SUBCATCH, I)
  else
  begin
    Errmsg := MSG_NO_SUBCATCH_NAMED + S;
    Result := False;
    Exit;
  end;
  C.OutNode := N2;
  C.OutSubcatch := C2;

  // Redraw outlet connection
  if MapForm.Map.Options.ShowSubcatchLinks then MapForm.RedrawMap;
  Result := True;
end;


procedure UpdateSubcatchColor(const Index: Integer;  K: Integer);
//-----------------------------------------------------------------------------
//  Updates color of subcatchment on map if view theme is an input variable.
//-----------------------------------------------------------------------------
begin
  if Uglobals.CurrentSubcatchVar < SUBCATCHOUTVAR1 then
  with SubcatchVariable[Uglobals.CurrentSubcatchVar] do
  begin
    if SourceIndex = K then
    begin
      Uoutput.SetSubcatchColor(Project.GetSubcatch(SUBCATCH, Index), K);
      MapForm.DrawObject(SUBCATCH, Index);
    end;
  end;
end;


procedure UpdateNodeColor(const Ntype: Integer; const Index: Integer;
  K: Integer);
//-----------------------------------------------------------------------------
//  Updates color of node on map if view theme is an input variable.
//-----------------------------------------------------------------------------
begin
  if Uglobals.CurrentNodeVar < NODEOUTVAR1 then
  with NodeVariable[Uglobals.CurrentNodeVar] do
  begin
    if SourceIndex = K then
    begin
      Uoutput.SetNodeColor(Project.GetNode(Ntype, Index), K);
      MapForm.DrawObject(Ntype, Index);
    end;
  end;
end;


procedure UpdateLinkColor(const Ltype: Integer; const Index: Integer;
  K: Integer);
//-----------------------------------------------------------------------------
//  Updates color of link on map if view theme is an input variable.
//-----------------------------------------------------------------------------
begin
  if CurrentLinkVar < LINKOUTVAR1 then with LinkVariable[CurrentLinkVar] do
  begin
    if SourceIndex = K then
    begin
      Uoutput.SetLinkColor(Project.GetLink(Ltype, Index), K);
      MapForm.DrawObject(Ltype, Index);
    end;
  end;
end;


function  UpdateLabelAnchor(L: TMapLabel; const S: String;
  var Errmsg: String): Boolean;
//-----------------------------------------------------------------------------
//  Makes node with ID name S be the anchor node for map label L.
//-----------------------------------------------------------------------------
var
  Ntype: Integer;
  Index: Integer;
begin
  // Erase current label from map
  MapForm.HiliteOff;
  MapForm.EraseLabel(EditorIndex);

  // No anchor node if S is blank
  Result := True;
  if Length(Trim(S)) = 0 then L.Anchor := nil

  // Otherwise make sure S is in database
  else
  begin
    if not Project.FindNode(S, Ntype, Index) then
    begin
      ErrMsg := MSG_NO_NODE_NAMED + S;
      Result := False;
    end
    else L.Anchor := Project.GetNode(Ntype, Index);
  end;

  // Redraw label on map
  MapForm.DrawObject(MAPLABEL, EditorIndex);
  MapForm.HiliteOn;
end;


procedure UpdateWeirShape(const Index: Integer);
//-----------------------------------------------------------------------------
//  Updates a weir's shape property when choice of weir type changes.
//-----------------------------------------------------------------------------
var
  I, K: Integer;
  aLink: TLink;
begin
  aLink := Project.GetLink(WEIR, Index);
  K := 0;
  for I := 0 to High(WeirTypes) do
  begin
    if SameText(aLink.Data[WEIR_TYPE_INDEX], WeirTypes[I]) then
    begin
      K := I;
      break;
    end;
  end;
  aLink.Data[WEIR_SHAPE_INDEX] := WeirShapes[K];
end;


procedure UpdateLinkHints;
//-----------------------------------------------------------------------------
//  Updates the hints associated with Link properties.
//-----------------------------------------------------------------------------
const
  DEFAULT_OFFSET_TXT1 = ' (can enter * if same as node invert)';
  DEFAULT_OFFSET_TXT2 = ' (can enter * if same as inlet node invert)';
begin
  if SameText(Project.Options.Data[LINK_OFFSETS_INDEX], 'ELEVATION') then
  begin
    ConduitHint[CONDUIT_INLET_HT_INDEX] :=
      'Elevation of conduit invert at inlet end' + DEFAULT_OFFSET_TXT1;
    ConduitHint[CONDUIT_OUTLET_HT_INDEX] :=
      'Elevation of conduit invert at outlet end' + DEFAULT_OFFSET_TXT1;
    OrificeHint[ORIFICE_BOTTOM_HT_INDEX] :=
      'Elevation of orifice opening' + DEFAULT_OFFSET_TXT2;
    WeirHint[WEIR_CREST_INDEX] :=
      'Elevation of weir opening' + DEFAULT_OFFSET_TXT2;
    OutletHint[OUTLET_CREST_INDEX] :=
      'Elevation of outlet' + DEFAULT_OFFSET_TXT2;
    Project.DefProp[CONDUIT].Data[CONDUIT_INLET_HT_INDEX] := '*';
    Project.DefProp[CONDUIT].Data[CONDUIT_OUTLET_HT_INDEX] := '*';
    Project.DefProp[ORIFICE].Data[ORIFICE_BOTTOM_HT_INDEX] := '*';
    Project.DefProp[WEIR].Data[WEIR_CREST_INDEX] := '*';
    Project.DefProp[OUTLET].Data[OUTLET_CREST_INDEX] := '*';
    MainForm.OffsetsBtn.Caption := 'Offsets: Elevation';
  end
  else
  begin
    ConduitHint[CONDUIT_INLET_HT_INDEX] :=
      'Height of conduit invert above node invert at inlet end';
    ConduitHint[CONDUIT_OUTLET_HT_INDEX] :=
      'Height of conduit invert above node invert at outlet end';
    OrificeHint[ORIFICE_BOTTOM_HT_INDEX] :=
      'Height of orifice opening above inlet node invert';
    WeirHint[WEIR_CREST_INDEX] :=
      'Height of weir opening above inlet node invert';
    OutletHint[OUTLET_CREST_INDEX] :=
      'Height of outlet above inlet node invert';
    Project.DefProp[CONDUIT].Data[CONDUIT_INLET_HT_INDEX] := '0';
    Project.DefProp[CONDUIT].Data[CONDUIT_OUTLET_HT_INDEX] := '0';
    Project.DefProp[ORIFICE].Data[ORIFICE_BOTTOM_HT_INDEX] := '0';
    Project.DefProp[WEIR].Data[WEIR_CREST_INDEX] := '0';
    Project.DefProp[OUTLET].Data[OUTLET_CREST_INDEX] := '0';
    MainForm.OffsetsBtn.Caption := 'Offsets: Depth';
  end;
end;


procedure UpdateUnits;
//-----------------------------------------------------------------------------
//  Updates flow units and the unit system for all input design variables.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  Uglobals.FlowUnits := Project.Options.Data[FLOW_UNITS_INDEX];
  Project.DefProp[OPTION].Data[FLOW_UNITS_INDEX] := Uglobals.FlowUnits;
  MainForm.FlowUnitsBtn.Caption := 'Flow Units: ' + Uglobals.FlowUnits;
  if Uutils.FindKeyWord(Uglobals.FlowUnits, SIFlowUnits, 3) >= 0
  then Uglobals.UnitSystem := usSI
  else Uglobals.UnitSystem := usUS;
  for I := 0 to SUBCATCHOUTVAR1 - 1 do
    SubcatchUnits[I].Units := BaseSubcatchUnits[I, UnitSystem];
  for I := 0 to NODEOUTVAR1 - 1 do
    NodeUnits[I].Units := BaseNodeUnits[I, UnitSystem];
  for I := 0 to LINKOUTVAR1 - 1 do
    LinkUnits[I].Units := BaseLinkUnits[I, UnitSystem];
  UpdateMapUnits;
end;


procedure UpdateMapUnits;
//------------------------------------------------------------
//  Updates length units conversion factor
//  (conversion constants are defined in Uglobals.pas).
//------------------------------------------------------------
begin
  with MapForm.Map.Dimensions do
  begin
    LengthUCF := 1.0;

    if (UnitSystem = usUS) then
    begin
      AreaUCF := ACRESperFOOT2;
      if (Units = muMeters) or (Units = muDegrees) then
      begin
        LengthUCF := FEETperMETER;
        AreaUCF := ACRESperMETER2;
      end;
    end;

    if (UnitSystem = usSI) then
    begin
      AreaUCF := HECTARESperMETER2;
      if (Units = muFeet) then
      begin
        LengthUCF := METERSperFOOT;
        AreaUCF := HECTARESperFOOT2;
      end;
    end;

    Digits := Umap.DefMapDimensions.Digits;
    if Units = muDegrees then
    begin
      Digits := MAXDEGDIGITS;
      YperDeg := XperDeg*Cos(DegToRad((LowerLeft.Y + UpperRight.Y)/2));
    end;
  end;
end;


procedure UpdateObjectLength(const ObjType: Integer; const Index: Integer);
//-----------------------------------------------------------------------------
//  Updates a conduit's length or a subcatchment's area, depending on value
//  of ObjType.
//-----------------------------------------------------------------------------
var
  S: TSubcatch;
  L: TLink;
begin
  if ObjType = CONDUIT then
  begin
    L := Project.GetLink(CONDUIT, Index);
    L.Data[CONDUIT_LENGTH_INDEX] := MapForm.Map.GetLinkLengthStr(CONDUIT, Index);
    Uedit.UpdateEditor(ObjType, Index);
  end;
  if ObjType = SUBCATCH then
  begin
    S := Project.GetSubcatch(SUBCATCH, Index);
    S.Data[SUBCATCH_AREA_INDEX] := MapForm.Map.GetSubcatchAreaStr(Index);
    Uedit.UpdateEditor(ObjType, Index);
  end;
end;


procedure UpdateAllLengths;
//-----------------------------------------------------------------------------
//  Updates all conduit lengths and subcatchment areas.
//-----------------------------------------------------------------------------
var
  I: Integer;
  S: TSubcatch;
  L: TLink;
begin
  for I := 0 to Project.Lists[CONDUIT].Count-1 do
  begin
    L := Project.GetLink(CONDUIT, I);
    L.Data[CONDUIT_LENGTH_INDEX] := MapForm.Map.GetLinkLengthStr(CONDUIT, I);
  end;
  for I := 0 to Project.Lists[SUBCATCH].Count-1 do
  begin
    S := Project.GetSubcatch(SUBCATCH, I);
    S.Data[SUBCATCH_AREA_INDEX] := MapForm.Map.GetSubcatchAreaStr(I);
  end;
end;


procedure UpdateDefOptions;
//-----------------------------------------------------------------------------
//  Updates project default options when actual options are changed.
//-----------------------------------------------------------------------------
begin
  Project.DefProp[OPTION].Data[INFILTRATION_INDEX] :=
    Project.Options.Data[INFILTRATION_INDEX];
  Project.DefProp[OPTION].Data[FLOW_UNITS_INDEX] :=
    Project.Options.Data[FLOW_UNITS_INDEX];
  Project.DefProp[OPTION].Data[LINK_OFFSETS_INDEX] :=
    Project.Options.Data[LINK_OFFSETS_INDEX];
  Project.DefProp[OPTION].Data[ROUTING_MODEL_INDEX] :=
    Project.Options.Data[ROUTING_MODEL_INDEX];
  Project.DefProp[OPTION].Data[FORCE_MAIN_EQN_INDEX] :=
    Project.Options.Data[FORCE_MAIN_EQN_INDEX];
end;

procedure UpdateInfilModel(const OldModel: String; const NewModel: String);
var
  S1, S2: String;
  S:  TSubcatch;
  J:  Integer;
  UpdateParams: Boolean;
begin
  if Project.Lists[SUBCATCH].Count = 0 then exit;
  if SameText(OldModel, NewModel) then exit;
  S1 := Format('You have changed the default infiltration method from %s to %s.',
    [OldModel, NewModel]);
  S2 := Format('Should all subcatchments using %s infiltration switch to %s?',
    [OldModel, NewModel]);
  if Uutils.MsgDlg(S1 + #13 + S2, mtConfirmation, [mbYes, mbNo]) = mrYes then
  begin
    for J := 0 to Project.Lists[SUBCATCH].Count-1 do
    begin
      S := Project.GetSubcatch(SUBCATCH, J);
      if SameText(S.Data[SUBCATCH_INFIL_INDEX], OldModel) then
      begin
        S.Data[SUBCATCH_INFIL_INDEX] := NewModel;
        UpdateParams := true;
        if ContainsText(NewModel, 'HORTON') and
          ContainsText(OldModel, 'HORTON') then UpdateParams := false;
        if ContainsText(NewModel, 'GREEN_AMPT') and
          ContainsText(OldModel, 'GREEN_AMPT') then UpdateParams := false;
        if UpdateParams then CopyStringArray(Uproject.DefInfil, S.InfilData);
      end;
    end;
  end;
end;


function GetOffsetDepth(L: TLink; N: TNode; I: Integer): String;
//-----------------------------------------------------------------------------
//  Converts a link's Elevation offset to a Depth offset.
//-----------------------------------------------------------------------------
var
  Z: Single;
  E: Single;
begin
  if SameText(Trim(L.Data[I]), '*') then Result := '0'
  else
  begin
    Uutils.GetSingle(L.Data[I], Z);
    Uutils.GetSingle(N.Data[NODE_INVERT_INDEX], E);
    Z := Z - E;
    if Z <= 0 then Result := '0'
    else Result := Format('%.3f', [Z]);
  end;
end;


function GetOffsetElevation(L: TLink; N: TNode; I: Integer): String;
//-----------------------------------------------------------------------------
//  Converts a link's Depth offset to an Elevation offset.
//-----------------------------------------------------------------------------
var
  Z: Single;
  E: Single;
begin
  Uutils.GetSingle(L.Data[I], Z);
  if Z = 0 then Result := N.Data[NODE_INVERT_INDEX]
  else
  begin
    Uutils.GetSingle(N.Data[NODE_INVERT_INDEX], E);
    Z := Z + E;
    Result := Format('%.3f', [Z]);
  end;
end;


procedure ComputeDepthOffsets;
//-----------------------------------------------------------------------------
//  Converts Elevation offsets to Depth offsets for all links.
//-----------------------------------------------------------------------------
var
  J: Integer;
  L: TLink;
begin
  with Project.Lists[CONDUIT] do
    for J := 0 to Count-1 do
    begin
      L := TLink(Objects[J]);
      L.Data[CONDUIT_INLET_HT_INDEX] :=
        GetOffsetDepth(L, L.Node1, CONDUIT_INLET_HT_INDEX);
      L.Data[CONDUIT_OUTLET_HT_INDEX] :=
        GetOffsetDepth(L, L.Node2, CONDUIT_OUTLET_HT_INDEX);
    end;

  with Project.Lists[ORIFICE] do
    for J := 0 to Count-1 do
    begin
      L := TLink(Objects[J]);
      L.Data[ORIFICE_BOTTOM_HT_INDEX] :=
        GetOffsetDepth(L, L.Node1, ORIFICE_BOTTOM_HT_INDEX);
    end;

  with Project.Lists[WEIR] do
    for J := 0 to Count-1 do
    begin
      L := TLink(Objects[J]);
      L.Data[WEIR_CREST_INDEX] :=
        GetOffsetDepth(L, L.Node1, WEIR_CREST_INDEX);
    end;

  with Project.Lists[OUTLET] do
    for J := 0 to Count-1 do
    begin
      L := TLink(Objects[J]);
      L.Data[OUTLET_CREST_INDEX] :=
        GetOffsetDepth(L, L.Node1, OUTLET_CREST_INDEX);
    end;
end;


procedure ComputeElevationOffsets;
//-----------------------------------------------------------------------------
//  Converts Depth offsets to Elevation offsets for all links.
//-----------------------------------------------------------------------------
var
  J: Integer;
  L: TLink;
begin
  with Project.Lists[CONDUIT] do
    for J := 0 to Count-1 do
    begin
      L := TLink(Objects[J]);
      L.Data[CONDUIT_INLET_HT_INDEX] :=
        GetOffsetElevation(L, L.Node1, CONDUIT_INLET_HT_INDEX);
      L.Data[CONDUIT_OUTLET_HT_INDEX] :=
        GetOffsetElevation(L, L.Node2, CONDUIT_OUTLET_HT_INDEX);
    end;

  with Project.Lists[ORIFICE] do
    for J := 0 to Count-1 do
    begin
      L := TLink(Objects[J]);
      L.Data[ORIFICE_BOTTOM_HT_INDEX] :=
        GetOffsetElevation(L, L.Node1, ORIFICE_BOTTOM_HT_INDEX);
    end;

  with Project.Lists[WEIR] do
    for J := 0 to Count-1 do
    begin
      L := TLink(Objects[J]);
      L.Data[WEIR_CREST_INDEX] :=
        GetOffsetElevation(L, L.Node1, WEIR_CREST_INDEX);
    end;

  with Project.Lists[OUTLET] do
    for J := 0 to Count-1 do
    begin
      L := TLink(Objects[J]);
      L.Data[OUTLET_CREST_INDEX] :=
        GetOffsetElevation(L, L.Node1, OUTLET_CREST_INDEX);
    end;
end;


procedure UpdateOffsets;
const
  Msg1 = 'You have switched from using Depth Offsets to Elevation Offsets.' +
         #13 + 'Should all link offsets be converted to elevations now?';
  Msg2 = 'You have switched form using Elevation Offsets to Depth Offsets.' +
         #13 + 'Should all link offsets be converted to depths now?';
begin
  if SameText(Project.Options.Data[LINK_OFFSETS_INDEX], 'ELEVATION') then
  begin
    if Project.GetLinkCount > 0 then
    begin
      if Uutils.MsgDlg(Msg1, mtConfirmation, [mbYes, mbNo]) = mrYes
      then ComputeElevationOffsets;
    end;
  end
  else
  begin
    if Project.GetLinkCount > 0 then
    begin
      if Uutils.MsgDlg(Msg2, mtConfirmation, [mbYes, mbNo]) = mrYes
      then ComputeDepthOffsets;
    end;
  end;
  UpdateEditor(Uglobals.EditorObject, Uglobals.EditorIndex);
end;

end.
