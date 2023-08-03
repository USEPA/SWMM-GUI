unit Uresults;

{-------------------------------------------------------------------}
{                    Unit:    Uresults.pas                          }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    07/11/23    (5.2.4)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Delphi Pascal unit that reads the contents of the summary       }
{   results tables from SWMM's Status Report file.                  }
{-------------------------------------------------------------------}

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, DateUtils,
  Grids, ExtCtrls, StdCtrls, Math, System.StrUtils, Dialogs,
  Uutils, Uglobals, Uproject;

type
// Codes for report topics
  TTopic = (rtRunoff,
            rtLID,
            rtGroundwater,
            rtWashoff,
            rtNodeDepth,
            rtNodeInflow,
            rtNodeSurcharge,
            rtNodeFlooding,
            rtStorageVolume,
            rtOutfallLoading,
            rtStreetFlow,
            rtLinkFlow,
            rtFlowClass,
            rtConduitSurcharge,
            rtPumping,
            rtLinkLoad);

const
  LastTopic = 15;

  TopicLabels: array[0..LastTopic] of String =
    ('Subcatchment Runoff',
     'LID Performance',
     'Groundwater',
     'Subcatchment Washoff',
     'Node Depth',
     'Node Inflow',
     'Node Surcharge',
     'Node Flooding',
     'Storage Volume',
     'Outfall Loading',
     'Street Flow',
     'Link Flow',
     'Flow Classification',
     'Conduit Surcharge',
     'Pumping',
     'Link Pollutant Load');

  // Nominal object types for each report topic
  ObjectType: array[0..LastTopic] of Integer =
    (SUBCATCH, SUBCATCH, SUBCATCH, SUBCATCH,
     JUNCTION, JUNCTION, JUNCTION, JUNCTION, JUNCTION, JUNCTION,
     CONDUIT, CONDUIT, CONDUIT, CONDUIT, CONDUIT, CONDUIT);

var
  Topics: array[0..LastTopic] of TTopic;
  TopicStart: array[0..LastTopic] of Integer;
  TopicSize: array[0..LastTopic] of Integer;
  TopicHeaderLines: array[0..LastTopic, 0..3] of String;
  LineCount: Integer;
  UpdateCount: Boolean;
  F: TextFile;

procedure InitSummaryResults;
procedure PopulateGrid(Topic: Integer; aGrid: TStringGrid);
procedure GetLinkRptValues(LinkVar: Integer; var Z: array of Single);
procedure GetNodeRptValues(NodeVar: Integer; var Z: array of Single);
procedure GetSubcatchRptValues(SubcatchVar: Integer; var Z: array of Single);

implementation

procedure FindTopicsInReportFile; forward;
procedure SetDefaultReportLegends; forward;
function  GetNewTopic(Line: String; Topic: Integer): Integer; forward;
procedure GetHeaderLines(Topic: Integer); forward;
procedure RetrieveValues(Topic: Integer; VarIndex: Integer; var Z: array of Single); forward;
function  GetLinkIndex(LinkID: String): Integer; forward;
function  GetNodeIndex(NodeID: String): Integer; forward;
procedure SetLegend(var Legend: array of TMapLegend; J: Integer; Z: array of Single); forward;

procedure InitSummaryResults;
//-----------------------------------------------------------------------------
// Initializes reading of summary results from report file.
//------------------------------------------------------------------------------
begin
  FindTopicsInReportFile;
//  SetDefaultReportLegends;
end;

procedure FindTopicsInReportFile;
//-----------------------------------------------------------------------------
// Finds starting line and number of lines for each topic in the report file.
//-----------------------------------------------------------------------------
var
  I, J, K: Integer;
  Line : String;
  Topic: Integer;
begin
  // Initialize topic locations and sizes
  for I := 0 to High(Topics) do
  begin
    TopicStart[I] := -1;
    TopicSize[I] := 0;
    for J := 0 to 3 do TopicHeaderLines[I][J] := '';
  end;

  // Open SWMM's Status Report file
  if FileExists(TempReportFile) then
  begin
    AssignFile(F, TempReportFile);

    // Initialize line count and current topic
    LineCount := 0;
    Topic := -1;

    // Turn off updating of topic's size
    UpdateCount := False;

    // Read each line of file
    try
      {$I-}
      Reset(F);
      {$I+}
      if IOResult = 0 then
      while not Eof(F) do
      begin
        Readln(F, Line);
        Inc(LineCount);

        // Check if current line starts a new report topic
        K := GetNewTopic(Line, Topic);

        // New topic found -- parse its header lines
        if K >= 0 then
        begin
          Topic := K;
          GetHeaderLines(Topic);
        end

        // Otherwise, if still updating size of current topic
        else if (Topic >= 0) and UpdateCount then
        begin

          // Line not blank nor end of current topic -
          // add to size of current topic
          Line := Trim(Line);
          if (Length(Line) > 0) and not (AnsiStartsStr('-', Line))
          then Inc(TopicSize[Topic])

          // Otherwise at end of current topic -- stop updating its size
          else
          begin
           UpdateCount := False;
          end;
        end;
      end;

    // Close report file
    finally
      CloseFile(F);
    end;
  end;
end;

procedure SetDefaultReportlegends;
//-----------------------------------------------------------------------------
// Sets default legend intervals for each summary report variable.
// (See viewvars.txt for listing of viewable report variables.)
//------------------------------------------------------------------------------
var
  I, J: Integer;
begin
  for I := SUBCATCHRPTVAR1 to SUBCATCHOUTVAR1 - 1 do
  begin
    with SubcatchLegend[I] do
    begin
      for J := 1 to Nintervals do
        Intervals[J] := SubcatchVariable[I].DefIntervals[J];
    end;
  end;
  for I := NODERPTVAR1 to NODEOUTVAR1 - 1 do
  begin
    with NodeLegend[I] do
    begin
      for J := 1 to Nintervals do
        Intervals[J] := NodeVariable[I].DefIntervals[J];
    end;
  end;
  for I := LINKRPTVAR1 to LINKOUTVAR1 - 1 do
  begin
    with LinkLegend[I] do
    begin
      for J := 1 to Nintervals do
        Intervals[J] := LinkVariable[I].DefIntervals[J];
    end;
  end;
end;

function GetNewTopic(Line: String; Topic: Integer): Integer;
//-----------------------------------------------------------------------------
// Checks if current line from SWMM report file starts a new topic.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  Result := -1;
  for I := Topic+1 to High(TopicLabels) do
  begin
    if ContainsText(Line, TopicLabels[I] + ' Summary') then
    begin
      Result := I;
      break;
    end;
  end;
end;

procedure GetHeaderLines(Topic: Integer);
//-----------------------------------------------------------------------------
// Saves the lines from the SWMM report file that constitute the
// column headings for the current report topic.
// Headings have following format:
//  Separator Line (-----)
//  Heading Line 1
//  . . .
//  Separator Line (------)
//-----------------------------------------------------------------------------
var
  Line: String;
  I: Integer;
begin
  // Turn off updating of topic size
  UpdateCount := False;
  I := 0;

  // Skip next two lines after topic title
  Readln(F, Line);
  Inc(LineCount);
  Readln(F, Line);
  Inc(LineCount);

  // Exit if start of header section not found within next 2 lines
  Readln(F, Line);
  Inc(LineCount);
  if not AnsiStartsStr('  -', Line) then
  begin
    Readln(F, Line);
    Inc(LineCount);
    if not AnsiStartsStr('  -', Line) then Exit;
  end;

  // Keep reading header lines until last header line found
  // (Headers can't have more than 4 lines of text)
  while (I <= 4)  do
  begin
    Readln(F, Line);
    Inc(LineCount);

    // Last line of header section found
    if AnsiStartsStr('  -', Line) then
    begin
      // Make topic start here
      TopicStart[Topic] := LineCount;
      UpdateCount := True;
      break;
    end

    // Otherwise copy line to topic's header
    else if I < 4 then
    begin
      TopicHeaderLines[Topic][I] := Line;
      Inc(I);
    end;
  end;
end;

procedure PopulateGrid(Topic: Integer; aGrid: TStringGrid);
//-----------------------------------------------------------------------------
// Populates a string grid with contents of a summary report topic.
//------------------------------------------------------------------------------
var
  I, J, N: Integer;
  Line : String;
  Tokens: TStringList;
begin

  // Open the SWMM report file
  Tokens := TStringList.Create;
  AssignFile(F, TempReportFile);
  try
    {$I-}
    Reset(F);
    {$I+}
    if IOResult = 0 then
    begin

      // Move file to start of current topic's report
      for I := 1 to TopicStart[Topic] do Readln(F, Line);

      // Tokenize each line of topic's results and insert into grid
      for I := 1 to TopicSize[Topic] do
      begin
        Readln(F, Line);
        Uutils.Tokenize(Line, Tokens, N);

        // Special case for Flow Summary of regulator links
        if (Topic = Integer(rtLinkFlow)) and (N = 6) then
        begin
          for J := 0 to 4 do aGrid.Cells[J,I] := Tokens[J];
          aGrid.Cells[7,I] := Tokens[5];
        end

        else for J := 0 to N - 1 do aGrid.Cells[J,I] := Tokens[J];
      end;
    end;

  // Close SWMM report file
  finally
    Tokens.Free;
    CloseFile(F);
  end;

end;

procedure GetLinkRptValues(LinkVar: Integer; var Z: array of Single);
//-----------------------------------------------------------------------------
// Places values of summary result LinkVar for all links into array Z.
//------------------------------------------------------------------------------
var
  VarIndex: Integer;
  Topic: Integer;
begin
  // Determine to which topic summary result variable belongs
  Topic := Ord(rtLinkFlow);
  if LinkVar in [HRS_SURCH, HRS_CAP_LTD]  then
    Topic := Ord(rtConduitSurcharge);

  // Determine where the variable appears on a line from the report file
  // and then retrieve its value for all links
  if TopicSize[Topic] > 0 then
  begin
    VarIndex := LinkVariable[LinkVar].SourceIndex;
    RetrieveValues(Topic, VarIndex, Z);
  end;

  // Create legend intervals for the variable if none exist
  if (LinkLegend[LinkVar].Intervals[0] = 0) and
     (LinkLegend[LinkVar].Intervals[MAXINTERVALS] = 0) then
      SetLegend(LinkLegend, LinkVar, Z);
end;

procedure GetNodeRptValues(NodeVar: Integer; var Z: array of Single);
//-----------------------------------------------------------------------------
// Places values of summary result NodeVar for all nodes into array Z.
//------------------------------------------------------------------------------
var
  VarIndex: Integer;
  Topic: Integer;
begin
  // Determine to which topic summary result variable belongs
  if NodeVar in [MAX_DEPTH, MAX_HGL] then
    Topic := Ord(rtNodeDepth)
  else if NodeVar in [MAX_LATFLOW, TOT_LATFLOW]  then
    Topic := Ord(rtNodeInflow)
  else if NodeVar in [ MAX_FLOODING, TOT_FLOODING, HRS_FLOODED] then
    Topic := Ord(rtNodeFlooding)
  else exit;

  // Determine where the variable appears on a line from the report file
  // and then retrieve its value for all nodes
  if TopicSize[Topic] > 0 then
  begin
    VarIndex := NodeVariable[NodeVar].SourceIndex;
    RetrieveValues(Topic, VarIndex, Z);
  end;

  // Create legend intervals for the variable if none exist
  if (NodeLegend[NodeVar].Intervals[0] = 0) and
    (NodeLegend[NodeVar].Intervals[MAXINTERVALS] = 0) then
      SetLegend(NodeLegend, NodeVar, Z);
end;

procedure GetSubcatchRptValues(SubcatchVar: Integer; var Z: array of Single);
//-----------------------------------------------------------------------------
// Places values of summary result SubcatchVar for all subcatchments into array Z.
//------------------------------------------------------------------------------
var
  VarIndex: Integer;
  Topic: Integer;
begin
  // Determine where the variable appears on a line from the report file
  // and then retrieve its value for all subcatchments
  Topic := Ord(rtRunoff);
  if TopicSize[Topic] > 0  then
  begin
    VarIndex := SubcatchVariable[SubcatchVar].SourceIndex;
    RetrieveValues(Topic, VarIndex, Z);
  end;

  // Create legend intervals for the variable if none exist
  if (SubcatchLegend[SubcatchVar].Intervals[0] = 0) and
    (SubcatchLegend[SubcatchVar].Intervals[MAXINTERVALS] = 0) then
      SetLegend(SubcatchLegend, SubcatchVar, Z);
end;

procedure RetrieveValues(Topic: Integer; VarIndex: Integer; var Z: array of Single);
//-----------------------------------------------------------------------------
// Retrieves results for a variable appearing in a topic of the report file.
//------------------------------------------------------------------------------
var
  I, K, N: Integer;
  Line : String;
  Tokens: TStringList;
begin
  // Open SWMM report file
  Tokens := TStringList.Create;
  AssignFile(F, TempReportFile);
  try
    {$I-}
    Reset(F);
    {$I+}
    if IOResult = 0 then
    begin

      // Move file to start of topic's results
      for I := 1 to TopicStart[Topic] do Readln(F, Line);

      // Tokenize each line of topic's results
      for I := 0 to TopicSize[Topic]-1 do
      begin
        Readln(F, Line);
        Uutils.Tokenize(Line, Tokens, N);
        if VarIndex < N then
        begin
          // Flooding topic only has lines for nodes that flood
          // so determine the node's index from its ID name (Tokens[0])
          if Topic = Ord(rtNodeFlooding) then
            K := GetNodeIndex(Tokens[0])
          else if Topic = Ord(rtConduitSurcharge) then
            K := GetLinkIndex(Tokens[0])
          else
            K := I;

          // Save the numerical value to array Z
          if K >= 0 then
          begin
            Uutils.GetSingle(Tokens[VarIndex], Z[K]);
          end;
        end;
      end;
    end;

  // Close SWMM report file
  finally
    Tokens.Free;
    CloseFile(F);
  end;
end;

function GetLinkIndex(LinkID: String): Integer;
//-----------------------------------------------------------------------------
// Finds the index of a link in the array of link results given its ID name.
//------------------------------------------------------------------------------
var
  LinkType: Integer;
  LinkIndex: Integer;
begin
  if not Project.FindLink(LinkID, LinkType, LinkIndex) then
    Result := -1
  else
    Result := Project.GetLink(LinkType, LinkIndex).RptFileIndex;
end;

function  GetNodeIndex(NodeID: String): Integer;
//-----------------------------------------------------------------------------
// Finds the index of a node in the array of node results given its ID name.
//------------------------------------------------------------------------------
var
  NodeType: Integer;
  NodeIndex: Integer;
begin
  if not Project.FindNode(NodeID, NodeType, NodeIndex) then
    Result := -1
  else
    Result := Project.GetNode(NodeType, NodeIndex).RptFileIndex;
end;

procedure SetLegend(var Legend: array of TMapLegend; J: Integer; Z: array of Single);
//-----------------------------------------------------------------------------
// Determines map legend intervals for a summary report variable.
//
// J = variable's index
// Z = array of values for the variable
//------------------------------------------------------------------------------
var
  I, N: Integer;
  V, Zmin, Zmax, Zinterval: Double;
begin
  Zmin := -MISSING; //MISSING defined as -1e10 in Uproject.pas
  Zmax := MISSING;

  // Find range of values in array Z
  N := Length(Z);
  for I := 0 to N - 1 do
  begin
    V := Abs(Z[I]);
    if V = MISSING then continue;
    if V < Zmin then Zmin := V;
    if V > Zmax then Zmax := V;
  end;

  // If range is not valid then use a default minimum and interval
  if (Zmax = MISSING) or (Zmax = 0) then
  begin
    Zmin := 0;
    Zinterval := 0.2;
  end

  // Otherwise determine a nicely scaled minimum value and interval size
  else
  begin
    Zinterval := (Zmax - Zmin) / (MAXINTERVALS + 1);
    Uutils.AutoScale(Zmin, Zmax, Zinterval);
  end;

  // Use the scaled minimum and interval size to set legend intervals
  for I := 0 to MAXINTERVALS do
    Legend[J].Intervals[I] := Zmin + I * Zinterval;
end;

end.

