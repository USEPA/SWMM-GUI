unit Ubrowser;

{-------------------------------------------------------------------}
{                    Unit:    Ubrowser.pas                          }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/12/22    (5.2.2)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Delphi Pascal unit containing procedures called by event        }
{   handlers for the Browser panel components of the MainForm.      }
{-------------------------------------------------------------------}

interface

uses
 Controls, Classes, SysUtils, Dialogs, Windows, Math, Types, DateUtils,
 Forms, Graphics, ComCtrls, StrUtils, Uglobals, Uproject, Uutils;

type
  TStringCompare = function(S1, S2: String): Integer;

procedure AssignOutputUnits;

procedure BrowserAddItem(const ObjType: Integer; const Index: Integer);
procedure BrowserDeleteObject;
procedure BrowserEditObject;
procedure BrowserNewObject;
procedure BrowserSortObjects;
procedure BrowserUpdate(const ObjType: Integer; const Index: Integer);

procedure ChangeDate(I: Integer);
procedure ChangeMapTheme(const I: Integer; const J: Integer);
procedure ChangeTimePeriod(I: Integer);
function  ConvertLink(const oldType: Integer; const oldIndex: Integer;
          const newType: Integer): Integer;
function  ConvertNode(const oldType: Integer; oldIndex: Integer;
          const newType: Integer): Integer;

procedure DecreaseElapsedTime;
procedure DeleteItem(const ObjType: Integer; const Index: Integer);

procedure EnableDateTimeControls(const State: Boolean);

function  GetCurrentPeriod: LongInt;
function  GetIndexFromObject(const ObjType: Integer): Integer;
function  GetIndexOfVar(const ObjType: Integer; const S: String): Integer;
function  GetObjectFromIndex(const Index: Integer): Integer;

procedure IncreaseElapsedTime;
procedure InitBrowser;
procedure InitDataPage;
procedure InitMapPage;
procedure InitMapViewBoxes;

procedure RefreshObjectImage(const ObjType: Integer; ImageIndex: Integer);
procedure RefreshMap;
procedure RefreshMapColors;
procedure RefreshPollutNames;
procedure RefreshTimeLegend;
procedure RefreshTimeListBox(const Backwards: Boolean);

procedure SetBrowserButtons(const ObjType: Integer; const Index: Integer);
procedure UpdateHasItems(const ObjType: Integer);
procedure UpdateMapPage;
procedure UpdateMapViewBoxes;

var
  CurrentDateIndex: Integer;
  CurrentTimeIndex: Integer;

implementation

uses
  Dquery, Dtimeplot, Dinletusage, Fmain, Fproped, Fmap, Fovmap,
  Uedit, Uoutput, Uupdate;

const
  TXT_DELETE_OBJECT = 'Delete selected object?';
  TXT_TIME = 'Time';
  TXT_QUALITY = 'Quality';

var
  OldSubcatchVar: Integer;
  OldNodeVar: Integer;
  OldLinkVar: Integer;


//=============================================================================
//                         Stringlist Sorting Procedures
//=============================================================================

function CompareIDStrings(S1, S2: String): Integer;
//----------------------------------------------------------------------------
//  Special function used to compare ID strings of two objects. It does
//  a numerical comparison if both ID's are numbers, otherwise it does
//  a normal string comparison.
//----------------------------------------------------------------------------
var
  X1, X2: Extended;
begin
  if Uutils.GetExtended(S1, X1) and Uutils.GetExtended(S2, X2) then
  begin
    if      X1 < X2 then Result := -1
    else if X1 > X2 then Result := 1
    else                 Result := 0;
  end
  else Result := AnsiCompareText(S1, S2);
end;


procedure ExchangeListItems(Slist: TStringlist; Index1, Index2: Integer);
//----------------------------------------------------------------------------
//  Exchanges items at positions Index1 and Index2 in stringlist Slist.
//----------------------------------------------------------------------------
var
  TempObject: TObject;
  TempString: string;
begin
  with Slist do
  begin
    BeginUpdate;
    try
      TempString := Strings[Index1];
      TempObject := Objects[Index1];
      Strings[Index1] := Strings[Index2];
      Objects[Index1] := Objects[Index2];
      Strings[Index2] := TempString;
      Objects[Index2] := TempObject;
    finally
      EndUpdate;
    end;
  end;
end;


procedure SortStringlist(Slist: TStringlist; L, R: Integer;
  Compare: TStringCompare);
//-----------------------------------------------------------------------------
//  Sorts items in stringlist Slist between indexes L and R using
//  the user-supplied function Compare as a string comparison function.
//  Utilizes the QuickSort algorithm recursively, starting with L = 0
//  and R = Slist.Count-1.
//-----------------------------------------------------------------------------
var
  I, J: Integer;
  P: String;
begin
  repeat
    I := L;
    J := R;
    P := Slist[(L+R) Shr 1];
    repeat
      while Compare(Slist[I], P) < 0 do Inc(I);
      while Compare(Slist[J], P) > 0 do Dec(J);
      if I <= J then
      begin
        ExchangeListItems(Slist, I, J);
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then SortStringList(Slist, L, J, Compare);
    L := I;
  until I >= R;
end;


procedure InitBrowser;
//-----------------------------------------------------------------------------
//  Initializes contents of the Browser panel
//-----------------------------------------------------------------------------
begin
  //MainForm.ObjectTreeView.Images := nil;
  CurrentSubcatchVar := NOVIEW;
  CurrentNodeVar := NOVIEW;
  CurrentLinkVar := NOVIEW;
  CurrentObject  := -1;
  InitMapViewBoxes;
end;


//=============================================================================
//                           Data Page Procedures
//=============================================================================

procedure InitDataPage;
//-----------------------------------------------------------------------------
// Initializes data page of Browser panel to begin a new project.
//-----------------------------------------------------------------------------
begin
  with MainForm do
  begin
    Uglobals.CurrentObject := -1;
    with ObjectTreeView do Selected := Items[0];
    ItemListBox.Count := 0;
    ItemListBox.ItemIndex := -1;
    BrowserBtnNew.Enabled := False;
    BrowserBtnDelete.Enabled := False;
    BrowserBtnEdit.Enabled := True;
  end;
end;


procedure BrowserAddItem(const ObjType: Integer; const Index: Integer);
//-----------------------------------------------------------------------------
//  Adds a new item into the Browser panel's ItemListBox display.
//-----------------------------------------------------------------------------
begin
  UpdateHasItems(ObjType);
  MainForm.SetChangeFlags;
  if ObjType = Uglobals.CurrentList then
  begin
    MainForm.ItemListBox.Count := MainForm.ItemListBox.Count + 1;
    BrowserUpdate(ObjType, Index);
    if Project.IsVisual(ObjType) then OVMapForm.NeedsUpdating := True;
  end
  else Project.CurrentItem[ObjType] := Index;
end;


procedure BrowserNewObject;
//-----------------------------------------------------------------------------
//  Creates a new non-visual object in the project's database.
//-----------------------------------------------------------------------------
begin
  MainForm.SelectorButtonClick;
  Uedit.AddNonvisualObject(Uglobals.CurrentList);
end;


procedure BrowserDeleteObject;
//-----------------------------------------------------------------------------
//  Handles a request to remove a selected item from the project's database.
//-----------------------------------------------------------------------------
var
  I : Integer;
begin
  // Activate Select Object toolbar button
  MainForm.SelectorButtonClick;

  // Make sure there's an object to delete
  I := Project.CurrentItem[Uglobals.CurrentList];
  if I < 0 then Exit;

  // Ask for confirmation of deletion
  if Uglobals.ConfirmDelete then
  begin
    if Uutils.MsgDlg(TXT_DELETE_OBJECT,mtConfirmation,[mbYes,mbNo],MainForm)
     = mrNo then Exit;
  end;
  DeleteItem(Uglobals.CurrentList, I);
  UpdateHasItems(Uglobals.CurrentList);
end;


procedure DeleteItem(const ObjType: Integer; const Index: Integer);
//-----------------------------------------------------------------------------
//  Deletes an item from the project's database.
//-----------------------------------------------------------------------------
var
  OldName: String;
begin
  // If object is visual, then erase it from the map
  // (which also deletes it from the database)
  Oldname := Project.Lists[ObjType].Strings[Index];
  if Project.IsVisual(ObjType)
  then MapForm.EraseObject(ObjType, Index)

  // Otherwise delete the non-visual object from database.
  else Project.DeleteItem(ObjType, Index);

  // Redraw map if inlet structure deleted
  if ObjType = INLET then MapForm.RedrawMap;

  // Update the Browser's controls
  BrowserUpdate(ObjType, Project.CurrentItem[ObjType]);

  // Update change flags (SetChangeFlags sees if a new analysis is needed)
  if ObjType = MAPLABEL then Uglobals.HasChanged := True
  else MainForm.SetChangeFlags;
end;


procedure BrowserEditObject;
//-----------------------------------------------------------------------------
//  Edits the currently selected object in the project's database.
//-----------------------------------------------------------------------------
begin
  MainForm.SelectorButtonClick;
  Uedit.EditObject(Uglobals.CurrentList);
end;


procedure BrowserSortObjects;
//-----------------------------------------------------------------------------
//  Sorts the items in the current object category by ID name.
//-----------------------------------------------------------------------------
var
  S: String;
  I: Integer;
begin
  if Project.IsSortable(Uglobals.CurrentList)
  then with Project.Lists[Uglobals.CurrentList] do
  begin
    I := Project.CurrentItem[Uglobals.CurrentList];
    S := Strings[I];
    SortStringList(Project.Lists[Uglobals.CurrentList], 0, Count-1,
                   CompareIDStrings);
    I := IndexOf(S);
    BrowserUpdate(Uglobals.CurrentList, I);
    MainForm.ItemListBox.Refresh;
    Uglobals.HasChanged := True;
  end;
end;


function  GetObjectFromIndex(const Index: Integer): Integer;
//-----------------------------------------------------------------------------
//  Gets the index of the Object category that corresponds to the
//  node selected from the Browser's ObjectTreeView control.
//-----------------------------------------------------------------------------
begin
  case Index of
    0:  Result := NOTES;
    1:  Result := OPTION;
    2:  Result := CLIMATE;

    4:  Result := RAINGAGE;
    5:  Result := SUBCATCH;
    6:  Result := AQUIFER;
    7:  Result := SNOWPACK;
    8:  Result := HYDROGRAPH;
    9:  Result := LID;

    12: Result := JUNCTION;
    13: Result := OUTFALL;
    14: Result := DIVIDER;
    15: Result := STORAGE;

    17: Result := CONDUIT;
    18: Result := PUMP;
    19: Result := ORIFICE;
    20: Result := WEIR;
    21: Result := OUTLET;

    22: Result := STREET;
    23: Result := INLET;
    24: Result := TRANSECT;
    25: Result := CONTROL;

    27: Result := POLLUTANT;
    28: Result := LANDUSE;

    30: Result := CONTROLCURVE;
    31: Result := DIVERSIONCURVE;
    32: Result := PUMPCURVE;
    33: Result := RATINGCURVE;
    34: Result := SHAPECURVE;
    35: Result := STORAGECURVE;
    36: Result := TIDALCURVE;
    37: Result := WEIRCURVE;

    38: Result := TIMESERIES;
    39: Result := PATTERN;
    40: Result := MAPLABEL;

    else Result := -1;
  end;
end;


function GetIndexFromObject(const ObjType: Integer): Integer;
//-----------------------------------------------------------------------------
//  Gets the index of the node in the Browser's ObjectTreeView control
//  that corresponds to a particular object category.
//-----------------------------------------------------------------------------
begin
  case ObjType of
    NOTES:       Result := 0;
    OPTION:      Result := 1;
    CLIMATE:     Result := 2;

    RAINGAGE:    Result := 4;
    SUBCATCH:    Result := 5;
    AQUIFER:     Result := 6;
    SNOWPACK:    Result := 7;
    HYDROGRAPH:  Result := 8;
    LID:         Result := 9;

    JUNCTION:    Result := 12;
    OUTFALL:     Result := 13;
    DIVIDER:     Result := 14;
    STORAGE:     Result := 15;

    CONDUIT:     Result := 17;
    PUMP:        Result := 18;
    ORIFICE:     Result := 19;
    WEIR:        Result := 20;
    OUTLET:      Result := 21;

    STREET:      Result := 22;
    INLET:       Result := 23;
    TRANSECT:    Result := 24;
    CONTROL:     Result := 25;

    POLLUTANT:   Result := 27;
    LANDUSE:     Result := 28;

    CONTROLCURVE:   Result := 30;
    DIVERSIONCURVE: Result := 31;
    PUMPCURVE:      Result := 32;
    RATINGCURVE:    Result := 33;
    SHAPECURVE:     Result := 34;
    STORAGECURVE:   Result := 35;
    TIDALCURVE:     Result := 36;
    WEIRCURVE:      Result := 37;

    TIMESERIES:   Result := 38;
    PATTERN:      Result := 39;
    MAPLABEL:     Result := 40;

    else         Result := 0;
  end;
end;


procedure BrowserUpdate(const ObjType: Integer; const Index: Integer);
//-----------------------------------------------------------------------------
//  Updates the Browser after a new object has been selected from the
//  Browser or on the Study Area Map.
//-----------------------------------------------------------------------------
var
  RefreshFlag: Boolean;
begin
  // If no object type selected then clear the ItemListBox
  if ObjType < 0 then
  begin
    Uglobals.CurrentObject := ObjType;
    MainForm.NotesMemo.Visible := False;
    MainForm.ItemListBox.Visible := True;
    MainForm.ItemListBox.Count := 0;
    MainForm.ItemListBox.ItemIndex := -1;
    SetBrowserButtons(-1, -1);
    PropEditForm.Hide;
    MapForm.ChangeHiliteObject(-1, -1);
    Exit;
  end;

  // If a new object type has been selected then select its node in the
  // ObjectTreeView control
  RefreshFlag := False;
  if Uglobals.CurrentObject <> ObjType then
  begin
    Uglobals.CurrentList := ObjType;
    RefreshFlag := True;
    with MainForm.ObjectTreeView do
    begin
      Selected := Items[GetIndexFromObject(ObjType)];
      MainForm.ItemsLabel.Caption := Selected.Text;
    end;
  end;

  // Select the current item of the current object category in ItemListBox
  if MainForm.ItemListBox.Count <> Project.Lists[Uglobals.CurrentList].Count
  then MainForm.ItemListBox.Count := Project.Lists[Uglobals.CurrentList].Count;
  Project.CurrentItem[Uglobals.CurrentList] := Index;
  MainForm.ItemListBox.ItemIndex := Index;
  if RefreshFlag then MainForm.ItemListBox.Refresh;

  // Switch visiblity of NotesMemo & ItemListBox
  if Uglobals.CurrentList = NOTES then with MainForm.NotesMemo do
  begin
    MainForm.ItemListBox.Visible := False;
    if RefreshFlag then Text := Project.Lists[NOTES].Text;
    Visible := True;
  end
  else
  begin
    MainForm.NotesMemo.Visible := False;
    MainForm.ItemListBox.Visible := True;
  end;

  // Reset the status of the Browser Buttons
  SetBrowserButtons(Uglobals.CurrentList, Index);

  // Hide the Property Editor if no visual item was selected
  if (Index < 0) or not Project.IsVisual(ObjType) then PropEditForm.Hide;

  // Update the Property Editor and highlight the item on the map
  // if a visual object was selected
  if Project.IsVisual(ObjType) then
  begin
    if Index >= 0 then Uedit.UpdateEditor(ObjType, Index);
    MapForm.ChangeHiliteObject(ObjType, Index);
    if TimePlotForm.Visible then TimePlotForm.SetObject;

    if Project.IsNode(ObjType) and Assigned(InletUsageForm)
      and InletUsageForm.Visible then
        InletUsageForm.SetReceiverNode(ObjType, Index);

  end
  else MapForm.ChangeHiliteObject(-1, -1);
  if PropEditForm.Visible then PropEditForm.SetFocus;
end;


procedure SetBrowserButtons(const ObjType: Integer; const Index: Integer);
//-----------------------------------------------------------------------------
//  Resets the state of the Browser's speed buttons depending on the
//  type of object selected.
//-----------------------------------------------------------------------------
var
  Flag: Boolean;
  S: String;
begin
  // Disable New button for link objects if node count < 2
  Flag := True;
  if ObjType < 0 then Flag := False
  else if Project.IsLink(ObjType)
  and (Project.GetNodeCount < 2)
  then Flag := False;
  MainForm.BrowserBtnNew.Enabled := Flag;
  MainForm.MnuAddObject.Visible := Flag;

  // Disable Edit, Delete, Up, Down & Sort buttons if no items exist
  if (ObjType < 0) or (MainForm.ItemListBox.Count = 0)
  then Flag := False
  else Flag := True;
  MainForm.BrowserBtnEdit.Enabled := Flag;
  MainForm.BrowserBtnDelete.Enabled := Flag;
  MainForm.BrowserBtnUp.Enabled := Flag;
  MainForm.BrowserBtnDown.Enabled := Flag;
  MainForm.BrowserBtnSort.Enabled := Flag;
  MainForm.MnuEditObject.Enabled := Flag;
  MainForm.MnuDeleteObject.Enabled := Flag;
  if ObjType < 0 then Exit;

  // Disable Up button if at first item, Down button if at last item
  if Index = 0 then MainForm.BrowserBtnUp.Enabled := False;
  if Index = Project.Lists[ObjType].Count-1 then
    MainForm.BrowserBtnDown.Enabled := False;

  // Disable New button for Labels (they must be added via the Map Toolbar)
  //if ObjType = MAPLABEL then MainForm.BrowserBtnNew.Enabled := False;

  // Disable New, Delete, Up, Down & Sort buttons for
  // Notes, Controls, Climatology & Options
  if ObjType in [NOTES, CONTROL, CLIMATE, OPTION] then
  begin
    MainForm.BrowserBtnNew.Enabled := False;
    MainForm.BrowserBtnDelete.Enabled := False;
    MainForm.BrowserBtnUp.Enabled := False;
    MainForm.BrowserBtnDown.Enabled := False;
    MainForm.BrowserBtnSort.Enabled := False;
    MainForm.BrowserBtnEdit.Enabled := True;
    MainForm.MnuDeleteObject.Enabled := False;
    MainForm.MnuAddObject.Visible := False;
  end;
  if MainForm.MnuAddObject.Visible then
  begin
    S := MainForm.ItemsLabel.Caption;
    if (S <> 'Time Series') and AnsiEndsStr('s', S)
    then S := AnsiLeftStr(S, Length(S)-1);
    MainForm.MnuAddObject.Caption := 'Add a New ' + S;
  end;
end;


procedure UpdateHasItems(const ObjType: Integer);
//-----------------------------------------------------------------------------
//  Updates the HasItems property when an object category gains its
//  first item or looses its last item.
//-----------------------------------------------------------------------------
begin
  if not Project.HasItems[ObjType]
  and (Project.Lists[ObjType].Count > 0) then
  begin
    Project.HasItems[ObjType] := True;
  end
  else if (Project.Lists[ObjType].Count = 0) then
  begin
    Project.HasItems[ObjType] := False;
  end;
end;


procedure RefreshObjectImage(const ObjType: Integer; ImageIndex: Integer);
//-----------------------------------------------------------------------------
//  Updates the bitmap image next to a node of the ObjectTreeView control.
//  (Not currently used.)
//-----------------------------------------------------------------------------
var
  Index: Integer;
  aNode: TTreeNode;
  aParent: TTreeNode;
begin
  Index := GetIndexFromObject(ObjType);
  aNode := MainForm.ObjectTreeView.Items[Index];
  aNode.ImageIndex := ImageIndex;
  aNode.SelectedIndex := ImageIndex;
  aParent := aNode.Parent;
  while aParent <> nil do
  begin
    ImageIndex := 0;
    aNode := aParent.getFirstChild;
    while aNode <> nil do
    begin
      if aNode.ImageIndex = 1 then
      begin
        ImageIndex := 1;
        break;
      end;
      aNode := aParent.GetNextChild(aNode);
    end;
    aParent.ImageIndex := ImageIndex;
    aParent.SelectedIndex := ImageIndex;
    aParent := aParent.Parent;
  end;
end;


//=============================================================================
//                           Map Page Procedures
//=============================================================================

procedure InitMapPage;
//-----------------------------------------------------------------------------
//  Initializes the map page prior to running an analysis.
//-----------------------------------------------------------------------------
begin
  // Save current map theme variables
  OldSubcatchVar := Uglobals.CurrentSubcatchVar;
  OldNodeVar := Uglobals.CurrentNodeVar;
  OldLinkVar := Uglobals.CurrentLinkVar;

  // Re-initialize the theme selection boxes
  InitMapViewBoxes;

  // Disable date & time controls
  EnableDateTimeControls(False);
  MapForm.TimeLegendPanel.Caption := '';
  MapForm.TimeLegendBox.Visible := False;

  // Assign units to output view variables
  AssignOutputUnits;

  // Update the status of the Animator frame
  MainForm.AnimatorFrame.UpdateStatus;
  MainForm.AnimatorFrame.Timer.Enabled := False;
end;


procedure InitMapViewBoxes;
//-----------------------------------------------------------------------------
//  Initializes the theme selection combo boxes on the map page of Browser.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  // Fill the Subcatchment combo box with input variables only
  with MainForm.SubcatchViewBox do
  begin
    Clear;
    for I := 0 to SUBCATCHRPTVAR1 - 1 do Items.Add(SubcatchVariable[I].Name);
    if Uglobals.CurrentSubcatchVar < Items.Count
    then ItemIndex := Uglobals.CurrentSubcatchVar
    else ItemIndex := 0;
    Uglobals.CurrentSubcatchVar := ItemIndex;
  end;

  // Do the same for the Node combo box
  with MainForm.NodeViewBox do
  begin
    Clear;
    for I := 0 to NODERPTVAR1 - 1 do Items.Add(NodeVariable[I].Name);
    if Uglobals.CurrentNodeVar < Items.Count
    then ItemIndex := Uglobals.CurrentNodeVar
    else ItemIndex := 0;
    Uglobals.CurrentNodeVar := ItemIndex;
  end;

  // And the same for the Link combo box
  with MainForm.LinkViewBox do
  begin
    Clear;
    for I := 0 to LINKRPTVAR1 - 1 do Items.Add(LinkVariable[I].Name);
    if Uglobals.CurrentLinkVar < Items.Count
    then ItemIndex := Uglobals.CurrentLinkVar
    else ItemIndex := 0;
    Uglobals.CurrentLinkVar := ItemIndex;
  end;
end;


procedure AssignOutputUnits;
//-----------------------------------------------------------------------------
//  Assigns measurement units to analysis output variables.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  for I := SUBCATCHRPTVAR1 to SUBCATCHVIEWS do
    Uglobals.SubcatchUnits[I].Units := Uglobals.BaseSubcatchUnits[I, UnitSystem];
  for I := NODERPTVAR1 to NODEVIEWS do
    Uglobals.NodeUnits[I].Units := Uglobals.BaseNodeUnits[I, UnitSystem];
  for I := LINKRPTVAR1 to LINKVIEWS do
    Uglobals.LinkUnits[I].Units := Uglobals.BaseLinkUnits[I, UnitSystem];
  Uglobals.SubcatchUnits[RUNOFF].Units := Uglobals.FlowUnits;
  Uglobals.SubcatchUnits[GW_FLOW].Units := Uglobals.FlowUnits;
  Uglobals.NodeUnits[LATFLOW].Units := Uglobals.FlowUnits;
  Uglobals.NodeUnits[INFLOW].Units := Uglobals.FlowUnits;
  Uglobals.NodeUnits[OVERFLOW].Units := Uglobals.FlowUnits;
  Uglobals.LinkUnits[FLOW].Units := Uglobals.FlowUnits;
end;


procedure UpdateMapViewBoxes;
//-----------------------------------------------------------------------------
//  Adds output variables to the theme selection combo boxes after a
//  successful analysis has been made.
//-----------------------------------------------------------------------------
var
  I: Integer;
  N: Integer;
begin
  N := Uoutput.Npolluts - 1;
  with MainForm.SubcatchViewBox do
  begin
    for I := SUBCATCHRPTVAR1 to SUBCATCHQUAL-1 do
      Items.Add(SubcatchVariable[I].Name);
    for I := 0 to N do
      Items.Add(Project.PollutNames[I]);
    if OldSubcatchVar < Items.Count
    then ItemIndex := OldSubcatchVar
    else ItemIndex := 0;
    Uglobals.CurrentSubcatchVar := ItemIndex;
  end;

  with MainForm.NodeViewBox do
  begin
    for I := NODERPTVAR1 to NODEQUAL-1 do
      Items.Add(NodeVariable[I].Name);
    for I := 0 to N do
      Items.Add(Project.PollutNames[I]);
    if OldNodeVar < Items.Count
    then ItemIndex := OldNodeVar
    else ItemIndex := 0;
    Uglobals.CurrentNodeVar := ItemIndex;
  end;

  with MainForm.LinkViewBox do
  begin
    for I := LINKRPTVAR1 to LINKQUAL-1 do
      Items.Add(LinkVariable[I].Name);
    for I := 0 to N do
      Items.Add(Project.PollutNames[I]);
    if OldLinkVar < Items.Count
    then ItemIndex := OldLinkVar
    else ItemIndex := 0;
    Uglobals.CurrentLinkVar := ItemIndex;
  end;
end;


procedure RefreshPollutNames;
//-----------------------------------------------------------------
//  Updates the list of pollutant names & their units.
//-----------------------------------------------------------------
var
  I: Integer;
  N: Integer;
  S: String;
  P: TPollutant;
begin
  Project.PollutNames.Clear;
  Project.PollutUnits.Clear;
  S := TXT_QUALITY;
  N := Project.Lists[POLLUTANT].Count;
  if N > 0 then
  begin
    for I := 0 to N-1 do
    begin
      S := Project.Lists[POLLUTANT].Strings[I];
      Project.PollutNames.Add(S);
      P := TPollutant(Project.Lists[POLLUTANT].Objects[I]);
      Project.PollutUnits.Add(P.Data[POLLUT_UNITS_INDEX]);
    end;
  end;
end;


procedure ChangeMapTheme(const I: Integer; const J: Integer);
//-----------------------------------------------------------------------------
//  Updates the Study Area Map display when a view theme changes.
//-----------------------------------------------------------------------------
begin
  case I of
  SUBCATCHMENTS:
     begin
       Uglobals.CurrentSubcatchVar := J;
       Uoutput.SetSubcatchColors;
       MapForm.RedrawMap;
       MapForm.DrawSubcatchLegend;
     end;
  NODES:
     begin
       Uglobals.CurrentNodeVar := J;
       Uoutput.SetNodeColors;
       MapForm.RedrawMap;
       MapForm.DrawNodeLegend;
     end;
  LINKS:
     begin
       Uglobals.CurrentLinkVar := J;
       if RunFlag then Uoutput.GetFlowDir(Uglobals.CurrentPeriod);
       Uoutput.SetLinkColors;
       MapForm.RedrawMap;
       MapForm.DrawLinkLegend;
     end;
  end;
  MainForm.AnimatorFrame.UpdateStatus;
end;


function  GetIndexOfVar(const ObjType: Integer; const S: String): Integer;
//-----------------------------------------------------------------------------
//  Finds the index of a view variable given its name.
//-----------------------------------------------------------------------------
begin
  case ObjType of
  SUBCATCHMENTS: Result := MainForm.SubcatchViewBox.Items.IndexOf(S);
  NODES: Result := MainForm.NodeViewBox.Items.IndexOf(S);
  LINKS: Result := MainForm.LinkViewBox.Items.IndexOf(S);
  else Result := -1;
  end;
end;


function GetCurrentPeriod: LongInt;
//-----------------------------------------------------------------------------
//  Finds the current output reporting period from the positions of the
//  Date & Time scrollbars on the map page of the Browser panel.
//-----------------------------------------------------------------------------
var
  N1: LongInt;
begin
  if Uglobals.DeltaDateTime >= 1.0 then
    N1 := CurrentDateIndex
  else
    N1 := Uglobals.getPeriod(CurrentDateIndex) + CurrentTimeIndex;
  Uglobals.CurrentDateTime :=
    IncSecond(Uglobals.StartDateTime, N1*Uglobals.ReportStep);
  Result := N1;
end;


procedure ChangeDate(I: Integer);
//----------------------------------------------------------------------------
//  Sets the position of the DateScrollBar on the map page of
//  the Browser panel to I.
//----------------------------------------------------------------------------
var
  Backwards: Boolean;
begin
  if I <> CurrentDateIndex then
  begin
    if I < CurrentDateIndex then Backwards := true else Backwards := false;
    CurrentDateIndex := I;
    MainForm.DateScrollBar.Position := CurrentDateIndex;
    RefreshTimeListBox(Backwards);
  end;
end;


procedure RefreshTimeListBox(const Backwards: Boolean);
//-----------------------------------------------------------------------------
//  Refreshes the entries in the Time of Day listbox on the map page of
//  the Browser panel when a new date is selected.
//-----------------------------------------------------------------------------
var
  T1: TDateTime;
  T2: TDateTime;
  I : Integer;
begin
  // Find dates that bracket the contents of the Time of Day listbox
  if Uglobals.DeltaDateTime >= 1.0 then
  begin
    T1 := IncSecond(Uglobals.StartDateTime, CurrentDateIndex*Uglobals.ReportStep);
    T2 := IncDay(T1);
  end
  else
  begin
    T1 := IncDay(DateOf(StartDateTime), CurrentDateIndex);
    T2 := IncDay(T1);
    if T1 < StartDateTime then T1 := StartDateTime;
  end;

  // With the Time of Day list box
  with MainForm.TimeListBox.Items do
  begin
    // Clear its contents and prevent it from redrawing as new items are added
    Clear;
    BeginUpdate;

    // Add times at successive reporting intervals until end of day reached
    while T1 < T2 do
    begin
      if T1 > EndDateTime then break;
      Add(TimeToStr(T1, MyFormatSettings));
      T1 := IncSecond(T1, ReportStep);
    end;

    // Allow the list box to refresh itself
    EndUpdate;

    // Update the max. value of the TimeScrollBar control to correspond
    // to the number of items in the Time of Day list box
    MainForm.TimeScrollBar.Max := Count-1;
  end;

  // Reset the current time of day index to the start of the day if
  // moving forward in time, or to end of the day if moving backward
  // in time
  CurrentTimeIndex := -1;
  if not Backwards then I := 0
  else I := MainForm.TimeListBox.Items.Count-1;
  MainForm.TimeListBox.ItemIndex := I;
  ChangeTimePeriod(I);
end;


procedure ChangeTimePeriod(I: Integer);
//----------------------------------------------------------------------------
//  Resets the curent time of day index when a new reporting time period
//  is selected.
//----------------------------------------------------------------------------
begin
  if I <> CurrentTimeIndex then
  begin
    CurrentTimeIndex := I;
    MainForm.TimeScrollBar.Position := CurrentTimeIndex;
    Uglobals.CurrentPeriod := GetCurrentPeriod;
    RefreshTimeLegend;
    RefreshMap;
    MainForm.UpdateProfilePlots;
  end;
end;


procedure IncreaseElapsedTime;
//-----------------------------------------------------------------------------
//  Moves the reporting date/time value to the next reporting period.
//-----------------------------------------------------------------------------
begin
  if Uglobals.CurrentPeriod >= Uglobals.Nperiods-1 then Exit;

  // Increment the time of day index by 1 if not at end of current day
  if CurrentTimeIndex < MainForm.TimeScrollBar.Max then
    ChangeTimePeriod(CurrentTimeIndex+1)

  // Otherwise increment the current date index by 1
  else if CurrentDateIndex < MainForm.DateScrollBar.Max then
    ChangeDate(CurrentDateIndex+1);
end;


procedure DecreaseElapsedTime;
//-----------------------------------------------------------------------------
//  Moves the reporting date/time value to the previous reporting period.
//-----------------------------------------------------------------------------
begin
  if Uglobals.CurrentPeriod <= 0 then Exit;

  // Decrease the time of day index by 1 if not at the start of the day
  if CurrentTimeIndex > 0 then ChangeTimePeriod(CurrentTimeIndex-1)

  // Otherwise decrease the current date index by 1
  else if CurrentDateIndex > 0 then ChangeDate(CurrentDateIndex-1);
end;


procedure UpdateMapPage;
//-----------------------------------------------------------------------------
//  Refreshes the controls on the Browser's map page after new simulation
//  run has been made.
//-----------------------------------------------------------------------------
var
  N: Integer;
  T: TDateTime;
  Tend: TDateTime;
begin
  // Refresh the list of pollutant names
  RefreshPollutNames;

  // Update the Map theme selection controls
  UpdateMapViewBoxes;

  // Enable the Date/Time controls
  EnableDateTimeControls(True);

  // Set Max value for ElapsedTimeUpDown control
  MainForm.ElapsedTimeUpDown.Max := Uglobals.Nperiods - 1;

  // Add date labels to the DateListBox
  T := DateOf(RoundTo(Uglobals.StartDateTime, -8));
  Tend := DateOf(RoundTo(Uglobals.EndDateTime, -8));

  while (T <= Tend) do
  begin
    MainForm.DateListBox.Items.Add(DateToStr(T, MyFormatSettings));
    if Uglobals.DeltaDateTime <= 1.0 then
      T := IncDay(T)
    else
      T := IncSecond(T, Uglobals.ReportStep);
  end;
  MainForm.DateListBox.ItemIndex := 0;

  // Set the parameters of the DateScrollBar
  CurrentDateIndex := 0;
  N := MainForm.DateListBox.Items.Count - 1;
  if N >= 0 then
  begin
    MainForm.DateScrollBar.Max := N;
    MainForm.DateScrollBar.Position := 0;
  end;

  // Add time period labels to the TimeListBox
  RefreshTimeListBox(False);

  // Enable the TimeListBox
  MainForm.TimeScrollBar.Enabled := True;
  MainForm.TimeListBox.ItemIndex := 0;

  // Display the Time Legend on the study area map
  Uglobals.CurrentPeriod := 0;
  RefreshTimeLegend;
  MapForm.TimeLegendBox.Visible := MapForm.PopupTimeLegend.Checked;

  // Initialize the Animator frame
  MainForm.AnimatorFrame.UpdateStatus;

  // Update the list of variables that can be queried on the QueryForm
  if Assigned(QueryForm) then QueryForm.UpdateVariables;
end;


procedure EnableDateTimeControls(const State: Boolean);
//-----------------------------------------------------------------------------
//  Enables/disables the date/time controls on the map page of
//  the Browser.
//-----------------------------------------------------------------------------
begin
  with MainForm do
  begin
    DateLabel.Enabled := State;
    DateListBox.Clear;
    DateListBox.Enabled := State;
    DateScrollBar.Enabled := State;
    TimeLabel.Enabled := State;
    TimeListBox.Clear;
    TimeListBox.Enabled := State;
    TimeScrollBar.Enabled := State;
    ElapsedTimeLabel.Enabled := State;
    ElapsedTimePanel.Enabled := State;
    ElapsedTimeUpDown.Enabled := State;
    ElapsedTimePanel.Text := '';
  end;
end;


procedure RefreshTimeLegend;
//-----------------------------------------------------------------------------
//  Refreshes the contents of the Time Legend panel displayed on the
//  study area map.
//-----------------------------------------------------------------------------
var
  ElapsedDateTime : TDateTime;
  ElapsedTime : Int64;
  S : String;
begin
  MapForm.TimeLegendPanel.Caption := MainForm.DateListBox.Text + ' ' +
    MainForm.TimeListBox.Text;

  ElapsedTime := (Uglobals.CurrentPeriod + Uglobals.StartOffset) *
                  Uglobals.ReportStep;
  ElapsedDateTime := IncSecond(0, ElapsedTime);

  S := ' ' + IntToStr(Floor(ElapsedDateTime)) + '.' +
         TimeToStr(TimeOf(ElapsedDateTime), MyFormatSettings);
  MainForm.ElapsedTimePanel.Text := S;
  MainForm.ElapsedTimeUpDown.Position := Uglobals.CurrentPeriod;
end;


procedure RefreshMapColors;
//-----------------------------------------------------------------------------
//  Refreshes the color coding of objects on the study area map.
//-----------------------------------------------------------------------------
begin
  if OldSubcatchVar <> Uglobals.CurrentSubcatchVar
  then Uoutput.SetSubcatchColors;
  if OldNodeVar <> Uglobals.CurrentNodeVar
  then Uoutput.SetNodeColors;
  if OldLinkVar <> Uglobals.CurrentLinkVar
  then Uoutput.SetLinkColors;
end;


procedure RefreshMap;
//-----------------------------------------------------------------------------
//  Refreshes the study area map display when the time period changes.
//-----------------------------------------------------------------------------
var
  RedrawFlag: Boolean;
begin
  // If analysis results are available
  RedrawFlag := False;
  if RunFlag then
  begin

    // Get flow directions
    Uoutput.GetFlowDir(Uglobals.CurrentPeriod);

    // Update subcatchment colors
    if (Uglobals.CurrentSubcatchVar >= SUBCATCHOUTVAR1) then
    begin
      Uoutput.SetSubcatchColors;
      RedrawFlag := True;
    end;

    // Update node colors
    if (Uglobals.CurrentNodeVar >= NODEOUTVAR1) then
    begin
      Uoutput.SetNodeColors;
      RedrawFlag := True;
    end;

    // Update link colors
    // Note: if current link theme is a summary report variable then
    // it must be re-loaded since the GetFlowDir call has overwritten
    // the theme results stored in Link.Z with flow values.
    if (Uglobals.CurrentLinkVar >= LINKRPTVAR1) then
    begin
      Uoutput.SetLinkColors;
      RedrawFlag := True;
    end;

    // Set RedrawFlag to True if displaying flow arrows
    if  (MapForm.Map.Options.ArrowStyle <> asNone)
    and (MapForm.Map.ZoomRatio >= MapForm.Map.Options.ArrowZoom)
    then RedrawFlag := True;
  end;

  // Redraw map if required.
  if RedrawFlag then
  begin
    MapForm.RedrawMap;
    if QueryFlag and Assigned(QueryForm) then QueryForm.UpdateQueryCaption;
  end;
end;


function ConvertNode(const oldType: Integer; oldIndex: Integer;
  const newType: Integer): Integer;
//-----------------------------------------------------------------------------
//  Converts a node from one type to another.
//-----------------------------------------------------------------------------
var
  aSubcatch: TSubcatch;
  oldNode  : TNode;
  newNode  : TNode;
  aLink    : TLink;
  ID       : String;
  I, J     : Integer;
  newIndex : Integer;
begin
  // Create a new Node of type newType
  oldNode := Project.GetNode(oldType, oldIndex);
  ID := Project.Lists[OldType].Strings[oldIndex];
  newNode := TNode.Create;
  newNode.Ntype := newType;

  // Copy properties from old Node to new Node
  newNode.X := oldNode.X;
  newNode.Y := oldNode.Y;
  newNode.OutFileIndex := -1;
  Uutils.CopyStringArray(Project.DefProp[newType].Data, newNode.Data);
  for I := ID_INDEX to NODE_INVERT_INDEX do newNode.Data[I] := oldNode.Data[I];
  if (oldType <> OUTFALL) and (newType <> OUTFALL) then
    for I := JUNCTION_MAX_DEPTH_INDEX to JUNCTION_INIT_DEPTH_INDEX do
      newNode.Data[I] := oldNode.Data[I];

  Uutils.CopyStringList(oldNode.DWInflow, newNode.DWInflow);
  Uutils.CopyStringList(oldNode.DXInflow, newNode.DXInflow);
  Uutils.CopyStringList(oldNode.IIInflow, newNode.IIInflow);
  Uutils.CopyStringList(oldNode.Treatment, newNode.Treatment);

  // Replace reference to old Node with new Node in all attached subcatchments
  for J := Project.Lists[SUBCATCH].Count-1 downto 0 do
  begin
    aSubcatch := Project.GetSubcatch(SUBCATCH, J);
    if aSubcatch.OutNode = oldNode then aSubcatch.OutNode := newNode;
  end;

  // Replace reference to old Node with new Node in all attached links
  for I := CONDUIT to OUTLET do
  begin
    for J := Project.Lists[I].Count-1 downto 0 do
    begin
      aLink := Project.GetLink(I, J);
      if aLink.Node1 = oldNode then aLink.Node1 := newNode;
      if aLink.Node2 = oldNode then aLink.Node2 := newNode;
    end;
  end;

  // Add new node to project
  Project.Lists[newType].AddObject(ID, newNode);
  UpdateHasItems(newType);

  // Attach old Node's ID to new Node
  newIndex := Project.Lists[newType].Count - 1;
  newNode.ID := PChar(Project.Lists[newType].Strings[newIndex]);

  // Assign the new node a map color
  newNode.ColorIndex := -1;
  if (Uglobals.CurrentNodeVar = NOVIEW)
  or (Uglobals.CurrentNodeVar >= NODEOUTVAR1)
  then newNode.ColorIndex := -1
  else Uoutput.SetNodeColor(newNode,
         NodeVariable[Uglobals.CurrentNodeVar].SourceIndex);
  MainForm.SetChangeFlags;
  Result := newIndex;
end;


function ConvertLink(const oldType: Integer; const oldIndex: Integer;
  const newType: Integer): Integer;
//-----------------------------------------------------------------------------
//  Converts a link from one type to another.
//-----------------------------------------------------------------------------
var
  oldLink : TLink;
  newLink : TLink;
  ID      : String;
  I       : Integer;
  newIndex: Integer;
begin
  // Create a new Link of type newType
  oldLink := Project.GetLink(oldType, oldIndex);
  ID := Project.Lists[OldType].Strings[oldIndex];
  newLink := TLink.Create;
  newLink.Ltype := newType;
  newLink.Node1 := oldLink.Node1;
  newLink.Node2 := oldLink.Node2;
  newLink.Vlist.Assign(oldLink.Vlist);
  newLink.OutFileIndex := -1;

  Uutils.CopyStringArray(Project.DefProp[newType].Data, newLink.Data);
  for I := ID_INDEX to TAG_INDEX do newLink.Data[I] := oldLink.Data[I];

  // Add new link to project & delete old one
  Project.Lists[newType].AddObject(ID, newLink);
  UpdateHasItems(newType);

  // Attach old Link's ID to new Link
  newIndex := Project.Lists[newType].Count - 1;
  newLink.ID := PChar(Project.Lists[newType].Strings[newIndex]);

  // Assign the new link a map color
  newLink.ColorIndex := -1;
  if (Uglobals.CurrentLinkVar = NOVIEW)
  or (Uglobals.CurrentLinkVar >= LINKOUTVAR1)
  then newLink.ColorIndex := -1
  else Uoutput.SetLinkColor(newLink,
         LinkVariable[Uglobals.CurrentLinkVar].SourceIndex);
  MainForm.SetChangeFlags;
  Result := newIndex;
end;

end.
