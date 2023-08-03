unit Uclipbrd;

{-------------------------------------------------------------------}
{                    Unit:    Uclipbrd.pas                          }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21  (5.2.0)                     }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Delphi Pascal unit containing procedures that copy and paste    }
{   data to and from the program's internal clipboard.              }
{-------------------------------------------------------------------}

interface

uses SysUtils, Windows, Forms, Messages, Classes, Graphics,
     Controls, Dialogs, System.UITypes, Uglobals, Uproject, Uutils, Uvertex;

procedure CopySubcatch(const Index: Integer);
procedure CopyGage(const Index: Integer);
procedure CopyLabel(const Index: Integer);
procedure CopyLink(const Ltype: Integer; const Index: Integer);
procedure CopyNode(const Ntype: Integer; const Index: Integer);
procedure PasteSubcatch(const Index: Integer);
procedure PasteGage(const Index: Integer);
procedure PasteLabel(const Index: Integer);
procedure PasteLink(const Ltype: Integer; const Index: Integer);
procedure PasteNode(const Ntype: Integer; const Index: Integer);

implementation

uses Fmain, Fmap, Uedit, Uupdate;


function GetDataIndexes(const ObjType: Integer;
  var First, Last: Integer):Boolean;
//-----------------------------------------------------------------------------
//  Gets the first and last indexes, depending on object type, of data
//  items that are copied/pasted from the program's internal clipboard.
//-----------------------------------------------------------------------------
begin
  Result := False;
  case ObjType of
    JUNCTION:
    begin
      First := NODE_INVERT_INDEX;
      Last := JUNCTION_PONDED_AREA_INDEX ;
    end;
    OUTFALL:
    begin
      First := NODE_INVERT_INDEX;
      Last := OUTFALL_TIME_SERIES_INDEX;
    end;
    DIVIDER:
    begin
      First := NODE_INVERT_INDEX;
      Last := DIVIDER_QCOEFF_INDEX;
    end;
    STORAGE:
    begin
      First := NODE_INVERT_INDEX;
      Last := STORAGE_ATABLE_INDEX;
    end;
    CONDUIT:
    begin
      First := CONDUIT_SHAPE_INDEX;
      Last := CONDUIT_TSECT_INDEX;
    end;
    PUMP:
    begin
      First := PUMP_CURVE_INDEX;
      Last := PUMP_SHUTOFF_INDEX;
    end;
    ORIFICE:
    begin
      First := ORIFICE_TYPE_INDEX;
      Last := ORIFICE_FLAPGATE_INDEX;
    end;
    WEIR:
    begin
      First := WEIR_TYPE_INDEX;
      Last := WEIR_SHAPE_INDEX;
    end;
    OUTLET:
    begin
      First := OUTLET_CREST_INDEX;
      Last  := OUTLET_QTABLE_INDEX;
    end;
    else Exit;
  end;
  Result := True;
end;


procedure CopyGage(const Index: Integer);
//-----------------------------------------------------------------------------
//  Copies a rain gage's data to the clipboard.
//-----------------------------------------------------------------------------
var
  I : Integer;
  G : TRaingage;
begin
  with Project.Clipboard do
  begin
    Clear;
    G := Project.GetGage(Index);
    for I := GAGE_DATA_FORMAT to GAGE_RAIN_UNITS do Data.Add(G.Data[I]);
    ObjType := RAINGAGE;
  end;
end;


procedure CopySubcatch(const Index: Integer);
//-----------------------------------------------------------------------------
// Copies a subcatchment's data to the clipboard.
//-----------------------------------------------------------------------------
var
  I : Integer;
  S : TSubcatch;
begin
  with Project.Clipboard do
  begin
    Clear;
    S := Project.GetSubcatch(SUBCATCH, Index);
    for I := SUBCATCH_RAINGAGE_INDEX to SUBCATCH_CURBLENGTH_INDEX do
      Data.Add(S.Data[I]);
    for I := 0 to MAXINFILPROPS do List1.Add(S.InfilData[I]);
    Uutils.CopyStringList(S.LandUses, List2);
    ObjType := SUBCATCH;
  end;
end;


procedure CopyNode(const Ntype: Integer; const Index: Integer);
//-----------------------------------------------------------------------------
// Copies a node's data to the clipboard.
//-----------------------------------------------------------------------------
var
  I      : Integer;
  First  : Integer;
  Last   : Integer;
  aNode  : TNode;
begin
  with Project.Clipboard do
  begin
    Clear;
    if not GetDataIndexes(Ntype, First, Last) then Exit;
    aNode := Project.GetNode(Ntype, Index);
    for I := First to Last do Data.Add(aNode.Data[I]);
    Uutils.CopyStringList(aNode.DWInflow, List1);
    Uutils.CopyStringList(aNode.DXInflow, List2);
    ObjType := Ntype;
  end;
end;


procedure CopyLink(const Ltype: Integer; const Index: Integer);
//-----------------------------------------------------------------------------
// Copies a link's data to the clipboard.
//-----------------------------------------------------------------------------
var
  I      : Integer;
  First  : Integer;
  Last   : Integer;
  aLink  : TLink;
begin
  with Project.Clipboard do
  begin
    Clear;
    if not GetDataIndexes(Ltype, First, Last) then Exit;
    aLink := Project.GetLink(Ltype, Index);
    for I := First to Last do Data.Add(aLink.Data[I]);
    ObjType := Ltype;
  end;
end;


procedure CopyLabel(const Index: Integer);
//-----------------------------------------------------------------------------
// Copies a map label's data to the clipboard.
//-----------------------------------------------------------------------------
var
  aLabel: TMapLabel;
begin
  with Project.Clipboard do
  begin
    Clear;
    ObjType := MAPLABEL;
    aLabel := Project.GetMapLabel(Index);
    with Font do
    begin
      Name := aLabel.FontName;
      Size := aLabel.FontSize;
      Style := [];
      if aLabel.FontBold then Style := Style + [fsBold];
      if aLabel.FontItalic then Style := Style + [fsItalic];
    end;
  end;
end;


procedure PasteSubcatch(const Index: Integer);
//-----------------------------------------------------------------------------
// Pastes data from the clipboard to a subcatchment.
//-----------------------------------------------------------------------------
var
  I : Integer;
  S : TSubcatch;
  OutletID: String;
  Emsg: String;
begin
  with Project.Clipboard do
  begin
    S := Project.GetSubcatch(SUBCATCH, Index);
    OutletID := S.Data[SUBCATCH_OUTLET_INDEX];
    for I := SUBCATCH_RAINGAGE_INDEX to SUBCATCH_CURBLENGTH_INDEX do
      S.Data[I] := Data[I-SUBCATCH_RAINGAGE_INDEX];
    for I := 0 to MAXINFILPROPS do S.InfilData[I] := List1[I];
    S.LandUses.Clear;
    Uutils.CopyStringList(List2, S.LandUses);
  end;
  if not SameText(OutletID, S.Data[SUBCATCH_OUTLET_INDEX])
  then Uupdate.UpdateSubcatchOutlet(S, S.Data[SUBCATCH_OUTLET_INDEX], Emsg);
  Uedit.UpdateEditor(SUBCATCH, Index);
  if Uglobals.CurrentSubcatchVar < SUBCATCHOUTVAR1
  then Uupdate.UpdateSubcatchColor(Index,
    SubcatchVariable[CurrentSubcatchVar].SourceIndex);
  MainForm.SetChangeFlags;
end;


procedure PasteGage(const Index: Integer);
//-----------------------------------------------------------------------------
// Pastes data from the clipboard to a rain gage.
//-----------------------------------------------------------------------------
var
  I : Integer;
  G : TRaingage;
begin
  with Project.Clipboard do
  begin
    G := Project.GetGage(Index);
    for I := GAGE_DATA_FORMAT to GAGE_RAIN_UNITS do
      G.Data[I] := Data[I-GAGE_DATA_FORMAT];
  end;
  Uedit.UpdateEditor(RAINGAGE, Index);
  MainForm.SetChangeFlags;
end;


procedure PasteLabel(const Index: Integer);
//-----------------------------------------------------------------------------
// Pastes data from the clipboard to a map label.
//-----------------------------------------------------------------------------
var
  aLabel: TMapLabel;
begin
  with Project.Clipboard do
  begin
    aLabel := Project.GetMapLabel(Index);
    with Font do
    begin
      aLabel.FontName := Name;
      aLabel.FontSize := Size;
      if (fsBold in Style) then aLabel.FontBold := True
      else aLabel.FontBold := False;
      if (fsItalic in Style) then aLabel.FontItalic := True
      else aLabel.FontItalic := False;
    end;
  end;
  MapForm.EraseLabel(Index);
  MapForm.DrawObject(MAPLABEL, Index);
  Uedit.UpdateEditor(MAPLABEL, Index);
  HasChanged := True;
end;


procedure PasteLink(const Ltype: Integer; const Index: Integer);
//-----------------------------------------------------------------------------
// Pastes data from the clipboard to a link.
//-----------------------------------------------------------------------------
var
  I:     Integer;
  First: Integer;
  Last:  Integer;
  aLink: TLink;
begin
  if not GetDataIndexes(Ltype, First, Last) then Exit;
  aLink := Project.GetLink(Ltype, Index);
  with Project.Clipboard do
  begin
    for I := First to Last do aLink.Data[I] := Data[I-First];
  end;
  Uedit.UpdateEditor(Ltype, Index);
  if Uglobals.CurrentLinkVar < LINKOUTVAR1
  then Uupdate.UpdateLinkColor(Ltype, Index,
    LinkVariable[CurrentLinkVar].SourceIndex);
  MainForm.SetChangeFlags;
end;


procedure PasteNode(const Ntype: Integer; const Index: Integer);
//-----------------------------------------------------------------------------
// Pastes data from the clipboard to a node.
//-----------------------------------------------------------------------------
var
  I:     Integer;
  First: Integer;
  Last:  Integer;
  aNode: TNode;
begin
  if not GetDataIndexes(Ntype, First, Last) then Exit;
  aNode := Project.GetNode(Ntype, Index);
  with Project.Clipboard do
  begin
    for I := First to Last do aNode.Data[I] := Data[I-First];
    aNode.DWInflow.Clear;
    Uutils.CopyStringList(List1, aNode.DWInflow);
    aNode.DXInflow.Clear;
    Uutils.CopyStringList(List2, aNode.DXInflow);
  end;
  Uedit.UpdateEditor(Ntype, Index);
  if Uglobals.CurrentNodeVar < NODEOUTVAR1
  then Uupdate.UpdateNodeColor(Ntype, Index,
    NodeVariable[CurrentNodeVar].SourceIndex);
  MainForm.SetChangeFlags;
end;

end.
