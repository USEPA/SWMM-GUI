unit Uvalidate;

{-------------------------------------------------------------------}
{                    Unit:    Uvalidate.pas                         }
{                    Project: EPA SMM                               }
{                    Version: 5.2                                   }
{                    Date:    11/01/21   (5.2.0)                    }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Delphi Pascal unit that validates new values for edited         }
{   properties of EPA SWMM objects.                                 }
{-------------------------------------------------------------------}

interface

uses
  SysUtils, Windows, Forms, Messages, Classes, Uglobals,   Dialogs,
  Uproject, Uutils, Uvertex;

function  ValidateEditor(I: Integer; var S: String; var E: String): Boolean;

implementation

uses
  Fmain, Fmap, Fproped, Uupdate, Dreporting;

const
  MSG_DUPLICATE_ID = 'ID name is blank or already in use.';
  MSG_NO_DATA      = 'This data field cannot be blank.';
  MSG_ZERO_VALUE   = 'This property cannot be 0.';

var
  Errmsg: String;


function  ValidateData(const I: Integer; const S: String): Boolean;
//-----------------------------------------------------------------------------
//  General validation function for an object property I with string
//  value S. The global variable EditorObject determines the type of
//  object being edited.
//-----------------------------------------------------------------------------

  function IsNotBlank(const S: String): Boolean;
  begin
    if Length(Trim(S)) > 0 then Result := True else Result := False;
  end;

  function IsZero(const S: String): Boolean;
  var
    X: Single;
  begin
    if Uutils.GetSingle(S, X) and (X = 0.0)
    then Result := True
    else Result := False;
  end;

begin
  Result := True;
  case EditorObject of

  RAINGAGE:
    if I = GAGE_DATA_FREQ then Result := IsNotBlank(S);

  SUBCATCH:
    if I in [SUBCATCH_SNOWPACK_INDEX, SUBCATCH_N_PERV_PAT_INDEX,
             SUBCATCH_DS_PAT_INDEX, SUBCATCH_INFIL_PAT_INDEX]
    then  Result := True
    else if I in [SUBCATCH_AREA_INDEX..SUBCATCH_CURBLENGTH_INDEX] then
      Result := IsNotBlank(S);

  JUNCTION:
    if I in [NODE_INVERT_INDEX,
             JUNCTION_MAX_DEPTH_INDEX..JUNCTION_PONDED_AREA_INDEX] then
      Result := IsNotBlank(S);

  OUTFALL:
    if I in [NODE_INVERT_INDEX, OUTFALL_FIXED_STAGE_INDEX] then
      Result := IsNotBlank(S);

  DIVIDER:
    if I in [NODE_INVERT_INDEX, DIVIDER_LINK_INDEX, DIVIDER_CUTOFF_INDEX,
             DIVIDER_QMIN_INDEX..DIVIDER_QCOEFF_INDEX] then
      Result := IsNotBlank(S);

  STORAGE:
    if I in [NODE_INVERT_INDEX, STORAGE_MAX_DEPTH_INDEX, STORAGE_INIT_DEPTH_INDEX]
             {STORAGE_ACOEFF_INDEX, STORAGE_AEXPON_INDEX]}then
      Result := IsNotBlank(S);

  CONDUIT:
    if (I in [CONDUIT_GEOM1_INDEX..CONDUIT_ROUGHNESS_INDEX]) and IsZero(S)
    then begin
      Result := False;
      ErrMsg := MSG_ZERO_VALUE;
      Exit;
    end
    else if I in [CONDUIT_LENGTH_INDEX..CONDUIT_AVG_LOSS_INDEX]
    then Result := IsNotBlank(S);

  ORIFICE:
    if I in [ORIFICE_HEIGHT_INDEX..ORIFICE_COEFF_INDEX] then
      Result := IsNotBlank(S);

  WEIR:
    if I in [WEIR_HEIGHT_INDEX..WEIR_CONTRACT_INDEX] then
      Result := IsNotBlank(S);

  OUTLET:
    if I in [OUTLET_CREST_INDEX, OUTLET_QCOEFF_INDEX, OUTLET_QEXPON_INDEX] then
      Result := IsNotBlank(S);
  end;
  if not Result then ErrMsg := MSG_NO_DATA;
end;


procedure ReplaceID(const NewName: String; const ObjType: Integer;
  const Index: Integer);
//-----------------------------------------------------------------------------
//  Replaces the ID name of the object of type ObjType and index Index
//  with NewName.
//-----------------------------------------------------------------------------
var
  OldName: String;
begin
  with Project.Lists[ObjType] do
  begin

    // Replace the ID name in the project database
    OldName := Strings[Index];
    Strings[Index] := NewName;

    // Replace the ID name that appears in the MainForm's ItemListBox
    MainForm.ItemListBox.Refresh;

    // Replace the ID reference property of the object
    if ObjType = RAINGAGE then
    begin
      Project.GetGage(Index).ID := PChar(Strings[Index]);
      Uupdate.UpdateRainGageName(Oldname, NewName);
    end
    else if Project.IsSubcatch(ObjType) then
    begin
      Project.GetSubcatch(ObjType, Index).ID := PChar(Strings[Index]);
      ReportingForm.UpdateName(SUBCATCHMENTS, OldName, NewName);
    end
    else if Project.IsNode(ObjType) then
    begin
      Project.GetNode(ObjType, Index).ID := PChar(Strings[Index]);
      Uupdate.UpdateNodeName(Oldname, NewName);
      ReportingForm.UpdateName(NODES, OldName, NewName);
    end
    else if Project.IsLink(ObjType) then
    begin
      Project.GetLink(ObjType, Index).ID := PChar(Strings[Index]);
      Uupdate.UpdateLinkName(OldName, NewName);
      ReportingForm.UpdateName(LINKS, OldName, NewName);
    end
    else if ObjType = MAPLABEL then
      Project.GetMapLabel(Index).Text := PChar(Strings[Index]);

    // Replace the ID name shown on the Property Editor's title bar
    if (ObjType = EditorObject) and (ObjType <> MAPLABEL) then
      PropEditForm.Caption := ObjectLabels[EditorObject] + ' ' + NewName;
  end;

  // Redraw the ID name on the map if these are currently displayed
  if Project.IsSubcatch(ObjType) then
  begin
    if MapForm.Map.Options.ShowSubcatchIDs
    then MapForm.DrawObject(SUBCATCH, Index);
  end
  else if Project.IsNode(ObjType) then
  begin
    if MapForm.Map.Options.ShowNodeIDs
    then MapForm.DrawObject(ObjType, Index);
  end
  else if Project.IsLink(ObjType) then
  begin
    if MapForm.Map.Options.ShowLinkIDs
    then MapForm.DrawObject(ObjType, Index);
  end;
end;


function ValidateRaingage(I: Integer; var S: String): Boolean;
//-----------------------------------------------------------------------------
//  Validates data for a rain gage.
//-----------------------------------------------------------------------------
var
  Rgage: TRaingage;
  V    : Extended;
  X, Y : Extended;
begin
  // For ID name, make sure its not a duplicate
  Result := True;
  Rgage := Project.GetGage(EditorIndex);
  if (I = ID_INDEX) then
  begin
    Result := not Project.DupID(S, RAINGAGE, EditorIndex);
    if   Result = True
    then ReplaceID(S, EditorObject, EditorIndex)
    else ErrMsg := MSG_DUPLICATE_ID;
  end

  // For X or Y coordinate, move gage to new position
  else if I in [X_INDEX..Y_INDEX] then
  begin
    if Uutils.GetExtended(S, V) then
    begin
      X := Rgage.X;
      Y := Rgage.Y;
      case I of
        X_INDEX: X := V;
        Y_INDEX: Y := V;
      end;
      MapForm.HiliteOff;
      MapForm.MoveGage(EditorIndex, X, Y);
      MapForm.HiliteOn;
    end;
  end

  // For all other properties
  else if ValidateData(I, S) then Rgage.Data[I] := S else Result := False;
end;


function ValidateSubcatch(I: Integer; var S: String): Boolean;
//-----------------------------------------------------------------------------
// Validates data for a subcatchment.
//-----------------------------------------------------------------------------
var
  C: TSubcatch;
  V: Extended;
  X: Extended;
  Y: Extended;
begin
  C := Project.GetSubcatch(SUBCATCH, EditorIndex);
  Result := True;

  // For ID name, make sure its not a duplicate
  if (I = ID_INDEX) then
  begin
    Result := not Project.DupID(S, SUBCATCH, EditorIndex);
    if   Result = True
    then ReplaceID(S, EditorObject, EditorIndex)
    else ErrMsg := MSG_DUPLICATE_ID;
  end

  // For X or Y coordinate, move subcatchment to new position
  else if I in [X_INDEX..Y_INDEX] then
  begin
    if Uutils.GetExtended(S, V) then
    begin
      X := C.X;
      Y := C.Y;
      case I of
        X_INDEX: X := V;
        Y_INDEX: Y := V;
      end;
      MapForm.HiliteOff;
      MapForm.MoveSubcatch(EditorIndex, X, Y);
      MapForm.HiliteOn;
    end;
  end

  // For outlet node/subcatchment
  else if I = SUBCATCH_OUTLET_INDEX then
  begin
    Result := Uupdate.UpdateSubcatchOutlet(C, S, Errmsg);
    if Result then C.Data[I] := S;
  end

  // For all other properties
  else if (I >= COMMENT_INDEX) and (I <= High(SubcatchProps)) then
  begin
    if ValidateData(I, S) then
    begin
      C.Data[I] := S;
      Uupdate.UpdateSubcatchColor(EditorIndex, I);
    end
    else Result := False;
  end;
end;


function ValidateNode(I: Integer; var S: String): Boolean;
//-----------------------------------------------------------------------------
// Validates data for a drainage system node.
//-----------------------------------------------------------------------------
var
  Last: Integer;
  N   : TNode;
  V   : Extended;
  X, Y: Extended;
begin
  Result := True;
  N := Project.GetNode(EditorObject, EditorIndex);
  case EditorObject of
    JUNCTION:   Last := High(JunctionProps);
    OUTFALL:    Last := High(OutfallProps);
    DIVIDER:    Last := High(DividerProps);
    STORAGE:    Last := High(StorageProps);
    else        Last := -1;
  end;

  // For ID name, make sure its not a duplicate
  if (I = ID_INDEX) then
  begin
    Result := not Project.DupID(S, EditorObject, EditorIndex);
    if   Result = True
    then ReplaceID(S, EditorObject, EditorIndex)
    else ErrMsg := MSG_DUPLICATE_ID;
  end

  // For X or Y coordinate, move node to new position
  else if I in [X_INDEX..Y_INDEX] then
  begin
    if Uutils.GetExtended(S, V) then
    begin
      X := N.X;
      Y := N.Y;
      case I of
        X_INDEX: X := V;
        Y_INDEX: Y := V;
      end;
      MapForm.HiliteOff;
      MapForm.MoveNode(EditorObject, EditorIndex, X, Y);
      MapForm.HiliteOn;
    end;
  end

  // For all other properties
  else if (I >= COMMENT_INDEX) and (I <= Last) then
  begin
    if ValidateData(I, S) then
    begin
      N.Data[I] := S;
      Uupdate.UpdateNodeColor(EditorObject, EditorIndex, I);
    end
    else Result := False;
  end;
end;


function ValidateLink(I: Integer; var S: String): Boolean;
//-----------------------------------------------------------------------------
// Validates data for a drainage system link.
//-----------------------------------------------------------------------------
var
  Last: Integer;
  L   : TLink;
begin
  L := Project.GetLink(EditorObject, EditorIndex);
  case EditorObject of
    CONDUIT:    Last := High(ConduitProps);
    PUMP:       Last := High(PumpProps);
    ORIFICE:    Last := High(OrificeProps);
    WEIR:       Last := High(WeirProps);
    OUTLET:     Last := High(OutletProps);
    else        Last := -1;
  end;
  Result := True;

  // For ID name, make sure its not a duplicate
  if (I = ID_INDEX) then
  begin
    Result := not Project.DupID(S, EditorObject, EditorIndex);
    if   Result = True
    then ReplaceID(S, EditorObject, EditorIndex)
    else ErrMsg := MSG_DUPLICATE_ID;
  end

  // For start/end node:
  else if (I = UP_INDEX) or (I = DN_INDEX) then
  begin
    Result := Uupdate.UpdateLinkNode(EditorObject, EditorIndex, I, S, Errmsg);
    if Result then L.Data[I] := S;
  end

  // For all other properties
  else if (I >= COMMENT_INDEX) and (I <= Last) then
  begin
    if ValidateData(I, S) then
    begin
      L.Data[I] := S;
      if EditorObject = CONDUIT
      then Uupdate.UpdateLinkColor(EditorObject, EditorIndex, I);
      if (EditorObject = WEIR)
      and (I = WEIR_TYPE_INDEX)
      then Uupdate.UpdateWeirShape(EditorIndex);
    end
    else Result := False;
  end;
end;


function ValidateLabel(I: Integer; var S: String): Boolean;
//-----------------------------------------------------------------------------
// Validation function for map label data.
//-----------------------------------------------------------------------------
var
  V: Extended;
  L: TMapLabel;
begin
  Result := True;
  L := Project.GetMapLabel(EditorIndex);

  // Replace old text with new
  if I = LABEL_TEXT_INDEX then
  begin
    MapForm.ReplaceLabel(EditorIndex, S);
    ReplaceID(S, EditorObject, EditorIndex);
  end

  // Move label to new position
  else if I in [X_INDEX..Y_INDEX] then
  begin
    if Uutils.GetExtended(S, V) then
    begin
      MapForm.HiliteOff;
      MapForm.EraseLabel(EditorIndex);
      case I of
        X_INDEX: L.X := V;
        Y_INDEX: L.Y := V;
      end;
      MapForm.DrawObject(MAPLABEL, EditorIndex);
      MapForm.HiliteOn;
    end;
  end

  //Update connection to an anchor node
  else if I = ANCHOR_NODE_INDEX
  then Result := Uupdate.UpdateLabelAnchor(L, S, Errmsg);

  if Result = True then HasChanged := True;
end;


function ValidateEditor(I: Integer; var S: String; var E: String): Boolean;
//-----------------------------------------------------------------------------
//  Validates data for the current object being edited. I is the index of
//  the property being edited, S is the string value of the property, and
//  E is the text of any error message generated.
//-----------------------------------------------------------------------------
begin
  E := '';
  case EditorObject of
    RAINGAGE:           Result := ValidateRaingage(I, S);
    SUBCATCH:           Result := ValidateSubcatch(I, S);
    JUNCTION..STORAGE:  Result := ValidateNode(I, S);
    CONDUIT..OUTLET:    Result := ValidateLink(I, S);
    MAPLABEL:           Result := ValidateLabel(I, S);
    else                Result := False;
  end;
  if Result = False then E := ErrMsg
  else MainForm.SetChangeFlags;
end;

end.
