unit Uoutput;

{-------------------------------------------------------------------}
{                    Unit:    Uoutput.pas                           }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Delphi Pascal unit used for retrieving output results from      }
{   a simulation run by EPA SWMM.                                   }
{-------------------------------------------------------------------}

{    Routine           Purpose
    --------------    -----------------------------------------------
    CheckRunStatus    Checks if a successful simulation was made
    ClearOutput       Clears all output results
    CloseOutputFile   Closes binary output file
    FindSubcatchColor Finds color to display a subcatchment value with
    FindLinkColor     Finds color to display a link value with
    FindNodeColor     Finds color to display a node value with
    GetSubcatchMinMax Gets min and max value of a subcatchment variable
    GetSubcatchOutVal Gets value of a variable for a specific subcatchment
    GetSubcatchOutVals Gets values at all subcatchments at specific time period
    GetSubcatchValStr Gets string value of a subcatchment variable
    GetBasicOutput    Stores basic information from output results file
    GetConduitSlope   Gets slope of a conduit
    GetFlowDir        Gets flow direction of each link
    GetLinkInputStr   Gets string value of a link's input property
    GetLinkMinMax     Gets min and max value of a link variable
    GetLinkOutVal     Gets value of a link variable for a specific link
    GetLinkOutVals    Gets values at all links for a specific time period
    GetLinkValStr     Gets string value of a link variable
    GetMeterLabel     Gets string value to display in a meter label
    GetNodeMinMax     Gets min and max value of a node variable
    GetNodeOutVal     Gets value of a node variable for a specific node
    GetNodeOutVals    Gets values at all nodes for a specific time period
    GetNodeValStr     Gets string value of a node variable
    GetRunFlag        Gets RunFlag value when previous output saved to file
    GetString         Reads a fixed-length string from a binary file
    OpenOutputFile    Opens binary output file
    SetSubcatchColor  Sets color of a subcatchment's input value
    SetSubcatchColors Sets colors for all subcatchments
    SetLinkColor      Sets color of a link's input value
    SetLinkColors     Sets colors for all links
    SetNodeColor      Sets color of a node's input value
    SetNodeColors     Sets colors for all nodes
    SetQueryColor     Sets node/link color based on query condition
}

interface

uses
  SysUtils, Dialogs, Classes, Consts, Graphics, Windows, AnsiStrings,
  Math, DateUtils, Uglobals, Uproject, Ulid, Uutils, Swmm5;

var
  Nsubcatchs    : Integer;        // Number of reporting subcatchments
  Nnodes        : Integer;        // Number of reporting nodes
  Nlinks        : Integer;        // Number of reporting links
  Npolluts      : Integer;        // Number of pollutants

function  CheckRunStatus(const Fname: String): TRunStatus;
procedure ClearOutput;
procedure CloseOutputFile;

function  FindSubcatchColor(const Z: Single): Integer;
function  FindLinkColor(const Z: Single): Integer;
function  FindNodeColor(const Z: Single): Integer;

procedure GetSubcatchMinMax(const SubcatchVar: Integer; const Period: LongInt;
          var Xmin: Single; var Xmax: Single);
function  GetSubcatchOutVal(const V: Integer; const Period: LongInt;
          const Zindex: Integer): Single;
procedure GetSubcatchOutVals(const SubcatchVar: Integer; const Period: LongInt;
          var Value: array of Single);
function  GetSubcatchValStr(const SubcatchVar: Integer; const Period: LongInt;
          const SubcatchIndex: Integer): String;

procedure GetBasicOutput;
function  GetConduitSlope(L: TLink): Extended;
procedure GetFlowDir(const TimePeriod: Integer);

procedure GetLinkFixedData(const K: Integer; var X: array of Single);
function  GetLinkInputStr(L: TLink; const Index: Integer): String;
procedure GetLinkMinMax(const LinkVar: Integer; const Period: LongInt;
          var Xmin: Single; var Xmax: Single);
function  GetLinkOutVal(const V: Integer; const Period: LongInt;
          const Zindex: Integer):Single;
procedure GetLinkOutVals(const LinkVar: Integer; const Period: LongInt;
          var Value: array of Single);
function  GetLinkValStr(const LinkVar: Integer; const Period: LongInt;
          const LinkType: Integer; const LinkIndex: Integer): String;

procedure GetMeterLabel(const ObjType, ObjIndex: Integer;
            var IDStr, ValStr: String);

procedure GetNodeFixedData(const K: Integer; var X: array of Single);
procedure GetNodeMinMax(const NodeVar: Integer; const Period: LongInt;
            var Xmin: Single; var Xmax: Single);
function  GetNodeOutVal(const V: Integer; const Period: LongInt;
          const Zindex: Integer):Single;
procedure GetNodeOutVals(const NodeVar: Integer; const Period: LongInt;
          var Value: array of Single);
function  GetNodeValStr(const NodeVar: Integer; const Period: LongInt;
          const NodeType: Integer; const NodeIndex: Integer): String;

function  GetRunFlag(const Fname: String): Boolean;
procedure GetString(var F: File; var S: ShortString);
function  GetSysOutVal(const V: Integer; const Period: LongInt):Single;
function  GetValue(const ObjType: Integer; const VarIndex: Integer;
          const Period: LongInt; theObject: TObject): Double;
function  GetVarIndex(const V: Integer; const ObjType: Integer): Integer;

function  OpenOutputFile(const Fname: String): TRunStatus;

procedure SetSubcatchColor(S: TSubcatch; const K: Integer);
procedure SetSubcatchColors;
procedure SetSubcatchRptColors(const SubcatchVar: Integer);
procedure SetLinkColor(L: TLink; const K: Integer);
procedure SetLinkColors;
procedure SetLinkRptColors(const LinkVar: Integer);
procedure SetNodeColor(N: TNode; const K: Integer);
procedure SetNodeColors;
procedure SetNodeRptColors(const NodeVar: Integer);
function  SetQueryColor(const Z: Single): Integer;

implementation

uses
  Fmain, Ubrowser, Dreporting, Uresults;

const
  MagicNumber = 516114522; //File signature
  RECORDSIZE  = 4;         //Byte size of each record

var
  Fout    : Integer;
  Offset0 : Int64;
  Offset1 : Int64;
  Offset2 : Int64;
  NsubcatchVars : Integer;
  NnodeVars : Integer;
  NlinkVars : Integer;
  NsysVars : Integer;
  SubcatchCount: Integer;
  NodeCount: Integer;
  LinkCount: Integer;

procedure SetupResultsArrays; forward;
procedure SetSubcatchInColors(const SubcatchVar: Integer); forward;
procedure SetSubcatchOutColors(const SubcatchVar: Integer); forward;
procedure SetLinkInColors(const LinkVar: Integer); forward;
procedure SetLinkOutColors(const LinkVar: Integer); forward;
procedure SetNodeInColors(const NodeVar: Integer); forward;
procedure SetNodeOutColors(const NodeVar: Integer); forward;


function CheckRunStatus(const Fname: String): TRunStatus;
//-----------------------------------------------------------------------------
// Checks if a successful simulation run was made.
//-----------------------------------------------------------------------------
var
  Mfirst : Integer;
  Mlast  : Integer;
  Np     : Integer;
  V      : Integer;
  E      : Integer;
  Offset : Integer;
begin
  // Open binary output file
  Result := OpenOutputFile(Fname);
  if Result = rsError then Exit;

  try
    // Starting from end of file, read byte offsets of file's sections
    Offset0 := -6*RecordSize;
    FileSeek(Fout, Offset0, 2);
    FileRead(Fout, Offset, SizeOf(Offset));   // start of ID names (not used)
    FileRead(Fout, Offset, SizeOf(Offset));   // start of input data
    Offset0 := Offset;
    FileRead(Fout, Offset, SizeOf(Offset));   // start of output data
    Offset1 := Offset;

    // Read # time periods, error code & file signature
    FileRead(Fout, Np, SizeOf(Np));
    Nperiods := Np;
    FileRead(Fout, E, SizeOf(E));
    FileRead(Fout, Mlast, SizeOf(Mlast));

    // Read file signature & version number from start of file
    FileSeek(Fout, 0, 0);
    FileRead(Fout, Mfirst, SizeOf(Mfirst));
    FileRead(Fout, V, SizeOf(V));

    // Check if run was completed
    if Mlast <> MagicNumber then Result := rsError

    // Ckeck if results were saved for 1 or more time periods
    else if Np <= 0 then Result := rsError

    // Check if correct version was used
    else if (Mfirst <> MagicNumber)
    or (V < VERSIONID1)
    or (V > VERSIONID2)
    then Result := rsWrongVersion

    // Check if error messages were generated
    else if E <> 0 then Result := rsError
    else Result := rsSuccess;

  except
    Result := rsError;
  end;

  // Close file if run was unsuccessful
  if Result in [rsFailed, rsWrongVersion, rsError]
  then FileClose(Fout);
end;


procedure ClearOutput;
//-----------------------------------------------------------------------------
//  Closes binary output results file and frees all
//  memory allocated to hold simulation output values.
//-----------------------------------------------------------------------------
var
  I, J: Integer;
  S: TSubcatch;
  N: TNode;
  L: TLink;
begin
  if RunFlag then
  begin
    // Close binary output file
    FileClose(Fout);

    // Re-set Zindex values
    for I := 0 to MAXCLASS do
    begin
      if Project.IsSubcatch(I) then
      begin
        for J := 0 to Project.Lists[I].Count - 1 do
        begin
          S := Project.GetSubcatch(SUBCATCH, J);
          S.OutFileIndex := -1;
          S.RptFileIndex := -1;
        end;
      end
      else if Project.IsNode(I) then
      begin
        for J := 0 to Project.Lists[I].Count - 1 do
        begin
          N := Project.GetNode(I, J);
          N.OutFileIndex := -1;
          N.RptFileIndex := -1;
        end;
      end
      else if Project.IsLink(I) then
      begin
        for J := 0 to Project.Lists[I].Count - 1 do
        begin
          L := Project.GetLink(I, J);
          L.OutFileIndex := -1;
          L.RptFileIndex := -1;
        end;
      end
      else continue;
    end;
  end;

  // Re-set status flags
  RunStatus := rsNone;
  RunFlag := False;
  UpdateFlag := False;
end;


procedure CloseOutputFile;
//-----------------------------------------------------------------------------
//  Closes binary output file.
//-----------------------------------------------------------------------------
begin
  FileClose(Fout);
end;


function FindLinkColor(const Z: Single): Integer;
//-----------------------------------------------------------------------------
//  Finds the color index of the value Z in the map legend for Links.
//-----------------------------------------------------------------------------
var
  I: Integer;
  K: Integer;
begin
  if (QueryFlag) then Result := SetQueryColor(Z)
  else
  begin
    if CurrentLinkVar >= LINKQUAL then K := LINKQUAL else K := CurrentLinkVar;
    with LinkLegend[K] do
      for I := Nintervals downto 1 do
        if Z >= Intervals[I] then
        begin
          Result := I;
          Exit;
        end;
    Result := 0;
  end;
end;


function FindNodeColor(const Z: Single): Integer;
//-----------------------------------------------------------------------------
//  Finds the color index of the value Z in the map legend for nodes.
//-----------------------------------------------------------------------------
var
  I: Integer;
  K: Integer;
begin
  if (QueryFlag) then Result := SetQueryColor(Z)
  else
  begin
    if CurrentNodeVar >= NODEQUAL then K := NODEQUAL else K := CurrentNodeVar;
    with NodeLegend[K] do
      for I := Nintervals downto 1 do
        if Z >= Intervals[I] then
        begin
          Result := I;
          Exit;
        end;
    Result := 0;
  end;
end;


function FindSubcatchColor(const Z: Single): Integer;
//-----------------------------------------------------------------------------
//  Finds the color index of the value Z in the map legend for subcatchments.
//-----------------------------------------------------------------------------
var
  I: Integer;
  K: Integer;
begin
  if (QueryFlag) then Result := SetQueryColor(Z)
  else
  begin
    if CurrentSubcatchVar >= SUBCATCHQUAL
    then K := SUBCATCHQUAL
    else K := CurrentSubcatchVar;
    with SubcatchLegend[K] do
      for I := Nintervals downto 1 do
        if Z >= Intervals[I] then
        begin
          Result := I;
          Exit;
        end;
    Result := 0;
  end;
end;


procedure GetBasicOutput;
//-----------------------------------------------------------------------------
//  Retrieves basic information from the binary output file.
//-----------------------------------------------------------------------------
var
  I             : Integer;
  Dummy         : Integer;
  SkipBytes     : Int64;
  ReportStart   : TDateTime;
  StartDate     : Double;

begin
  // Read number of drainage system components
  FileSeek(Fout, 0, 0);
  FileRead(Fout, Dummy, SizeOf(Dummy));             // File signature
  FileRead(Fout, Dummy, SizeOf(Dummy));             // Version number
  FileRead(Fout, Qunits, SizeOf(Qunits));           // Flow units code
  FileRead(Fout, Nsubcatchs, SizeOf(Nsubcatchs));   // # Subcatchments
  FileRead(Fout, Nnodes, SizeOf(Nnodes));           // # Nodes
  FileRead(Fout, Nlinks, SizeOf(Nlinks));           // # Links
  FileRead(Fout, Npolluts, SizeOf(Npolluts));       // # Pollutants

  // Skip over saved subcatch/node/link input values
  SkipBytes := (Nsubcatchs+2) * RecordSize  // Subcatchment area
             + (3*Nnodes+4) * RecordSize  // Node type, invert & max depth
             + (5*Nlinks+6) * RecordSize; // Link type, z1, z2, max depth & length
  SkipBytes := Offset0 + SkipBytes;
  FileSeek(Fout, SkipBytes, 0);

  // Read number & codes of computed variables
  FileRead(Fout, NsubcatchVars, SizeOf(NSubcatchVars)); // # Subcatch variables
  for I := 1 to NSubcatchVars do FileRead(Fout, Dummy, Sizeof(Dummy));
  FileRead(Fout, NnodeVars, SizeOf(NnodeVars));     // # Node variables
  for I := 1 to NnodeVars do FileRead(Fout, Dummy, Sizeof(Dummy));
  FileRead(Fout, NlinkVars, SizeOf(NlinkVars));     // # Link variables
  for I := 1 to NlinkVars do FileRead(Fout, Dummy, Sizeof(Dummy));
  FileRead(Fout, NsysVars, SizeOf(NsysVars));       // # System variables
  for I := 1 to NsysVars do FileRead(Fout, Dummy, Sizeof(Dummy));

  // Make number of system view variables consistent with number available
  NsysViews := NsysVars;
  if NsysVars - 1 > Uglobals.SYSVIEWS
  then NsysViews := Uglobals.SYSVIEWS + 1;

  // Read starting date/time & reporting time step and save as TDateTime objects
  FileRead(Fout, StartDate, SizeOf(StartDate));
  FileRead(Fout, ReportStep, SizeOf(ReportStep));
  StartDateTime := RoundTo(StartDate, -8);
  DeltaDateTime := RoundTo(ReportStep/86400, -8);

  // Check if report start date begins after simulation start date
  ReportStart := StrToDate(Project.Options.Data[REPORT_START_DATE_INDEX]) +
                 StrToTime(Project.Options.Data[REPORT_START_TIME_INDEX]);
  if ReportStart > StartDateTime then
    StartOffset := 0
  else
    StartOffset := 1;

  // Save dates of first and last reported results
  FileRead(Fout, StartDate, SizeOf(StartDate));
  Uglobals.StartDateTime := RoundTo(StartDate, -8);
  Uglobals.EndDateTime := IncSecond(Uglobals.StartDateTime,
     Int64((Nperiods-1))*Int64(ReportStep));

  // Save # bytes used to store results in each reporting period
  Offset2 := Sizeof(TDateTime) + RecordSize*(Nsubcatchs*NsubcatchVars +
             Nnodes*NnodeVars + Nlinks*NlinkVars + NsysVars);

  // Setup arrays that hold saved results in a given time period
  SetupResultsArrays;

  // Check if run used dynamic wave routing
  DynWaveFlag := SameText(Project.Options.Data[ROUTING_MODEL_INDEX], 'DYNWAVE');

  // Initialize Summary Results topics in Status Report
  Uresults.InitSummaryResults;
end;

procedure SetupResultsArrays;
//-----------------------------------------------------------------------------
//  Creates arrays that hold values for computed results.
//-----------------------------------------------------------------------------
var
  I, J, K, M: Integer;
  S: TSubcatch;
  N: TNode;
  L: TLink;
begin
  // Allocate memory for computed results (per period) arrays
  SubcatchCount := Project.Lists[SUBCATCH].Count;
  SetLength(Zsubcatch, SubcatchCount);
  NodeCount := Project.GetNodeCount;
  SetLength(Znode, NodeCount);
  LinkCount := Project.GetLinkCount;
  SetLength(Zlink, LinkCount);
  SetLength(FlowDir, LinkCount);

  // For each subcatchment, assign an index into the results array
  ReportingForm.SetReportedItems(SUBCATCHMENTS);
  K := 0;
  M := 0;
  for J := 0 to Project.Lists[SUBCATCH].Count - 1 do
  begin
    S := Project.GetSubcatch(SUBCATCH, J);
    if S.OutFileIndex >= 0 then
    begin
      S.OutFileIndex := K;
      Inc(K);
    end;
    S.RptFileIndex := M;
    Inc(M);
  end;

  // For each JUNCTION, OUTFALL, DIVIDER, and STORAGE node
  // (in that order) assign an index into the results array
  ReportingForm.SetReportedItems(NODES);
  K := 0;
  M := 0;
  for I := JUNCTION to STORAGE do
  begin
    for J := 0 to Project.Lists[I].Count - 1 do
    begin
      N := Project.GetNode(I, J);
      if N.OutFileIndex >= 0 then
      begin
        N.OutFileIndex := K;
        Inc(K);
      end;
      N.RptFileIndex := M;
      Inc(M);
    end;
  end;

  // For each CONDUIT, PUMP, ORIFICE, WEIR, and OUTLET (in that order)
  // assign an index into the results array
  ReportingForm.SetReportedItems(LINKS);
  K := 0;
  M := 0;
  for I := CONDUIT to OUTLET do
  begin
    for J := 0 to Project.Lists[i].Count - 1 do
    begin
      L := Project.GetLink(I, J);
      if L.OutFileIndex >= 0 then
      begin
        L.OutFileIndex := K;
        Inc(K);
      end;
      L.RptFileIndex := M;
      Inc(M);
    end;
  end;
end;


function GetConduitSlope(L: TLink): Extended;
//-----------------------------------------------------------------------------
//  Computes the slope of a given conduit.
//-----------------------------------------------------------------------------
var
  E1, E2, Len: Extended;
  Delta: Extended;
begin
  try

    // Find elevation of link end points for ELEVATION offsets
    if SameText(Project.Options.Data[LINK_OFFSETS_INDEX], 'ELEVATION') then
    begin
      if L.Data[CONDUIT_INLET_HT_INDEX] = '*'
      then E1 := StrToFloat(L.Node1.Data[NODE_INVERT_INDEX])
      else E1 := StrToFloat(L.Data[CONDUIT_INLET_HT_INDEX]);
      if L.Data[CONDUIT_OUTLET_HT_INDEX] = '*'
      then E2 := StrToFloat(L.Node2.Data[NODE_INVERT_INDEX])
      else E2 := StrToFloat(L.Data[CONDUIT_OUTLET_HT_INDEX]);
    end

    // Find elevation of link end points for DEPTH offsets
    else
    begin
      E1 := StrToFloat(L.Node1.Data[NODE_INVERT_INDEX]) +
            StrToFloat(L.Data[CONDUIT_INLET_HT_INDEX]);
      E2 := StrToFloat(L.Node2.Data[NODE_INVERT_INDEX]) +
            StrToFloat(L.Data[CONDUIT_OUTLET_HT_INDEX]);
    end;

    // Use HEC-RAS definition of slope
    Len := StrToFloat(L.Data[CONDUIT_LENGTH_INDEX]);
    if Len > 0 then
    begin
      Delta := E1 - E2;
      if Abs(Delta) < Len then Len := Sqrt((Len*Len) - (Delta*Delta));
      Result := Delta/Len*100;
    end
    else Result := 0;

  except
    On EConvertError do Result := 0;
  end;
end;


procedure GetFlowDir(const TimePeriod: Integer);
//-----------------------------------------------------------------------------
//  Determines the flow direction in each link of the drainage network.
//-----------------------------------------------------------------------------
var
  I, J, K : Integer;
  F : Single;        // Flow value
begin
  // Use original link orientation if link theme view is design variable
  if Uglobals.CurrentLinkVar < LINKRPTVAR1 then
  begin
    for I := 0 to LinkCount-1 do FlowDir[I] := PLUS;
    exit;
  end;

  // Default direction is NONE
  for I := 0 to LinkCount-1 do FlowDir[I] := NONE;

  // Stay with no flow direction if link theme is a summary variable
  if Uglobals.CurrentLinkVar < LINKOUTVAR1 then exit;

  // Retrieve link flows from the binary output file
  GetLinkOutVals(FLOW, TimePeriod, Zlink);

  // Establish the flow direction of each link.
  for J := CONDUIT to OUTLET do
  begin
    for I := 0 to Project.Lists[J].Count - 1 do
    begin
      K := Project.GetLink(J, I).OutFileIndex;
      if K >= 0 then
      begin
        F := Zlink[K];
        if F < -FLOWTOL then FlowDir[K] := MINUS
        else if F > FLOWTOL then FlowDir[K] := PLUS;
      end;
    end;
  end;
end;


procedure GetLinkFixedData(const K: Integer; var X: array of Single);
//-----------------------------------------------------------------------------
//  Retrieves the following design parameters for Link K that were saved to
//  the binary output file: type code, upstream and downstream invert
//  offsets, max. depth and length.
//-----------------------------------------------------------------------------
var
  BytePos: Integer;
  Y: array[0..3] of Single;
  I: Integer;
begin
  BytePos := Offset0 +                      // prolog records
             (2+Nsubcatchs)*RecordSize +    // subcatchment data
             (4+3*Nnodes)*RecordSize +      // node data
             (6+5*K)*RecordSize +           // link data prior to link K
             RecordSize;                    // link type code
  FileSeek(Fout, BytePos, 0);
  FileRead(Fout, Y, 4*Sizeof(Single));
  for I := 0 to 3 do X[I] := Y[I];
end;


function GetLinkInputStr(L: TLink; const Index: Integer): String;
//-----------------------------------------------------------------------------
//  Gets the string value of a link L's input property of given Index.
//-----------------------------------------------------------------------------
begin
  Result := L.Data[Index];
end;


procedure GetLinkMinMax(const LinkVar: Integer; const Period: LongInt;
  var Xmin: Single; var Xmax: Single);
//-----------------------------------------------------------------------------
//  Gets the min & max values of link variable LinkVar at time period Period.
//-----------------------------------------------------------------------------
var
  J, K, M, N : Integer;
  X : Single;
  Y : array of Single;
begin
  Xmin := -MISSING;
  Xmax := MISSING;
  if Nlinks = 0 then Exit;
  N := Project.Lists[CONDUIT].Count - 1;

  // This is for input design variables
  if (LinkVar < LINKOUTVAR1) then
  begin
    K := LinkVariable[LinkVar].SourceIndex;
    for J := 0 to N do
    begin
      if K = CONDUIT_SLOPE_INDEX
      then X := GetConduitSlope(Project.GetLink(CONDUIT, J))
      else if not Uutils.GetSingle(Project.GetLink(CONDUIT, J).Data[K], X)
      then continue;
      if (X < Xmin) then Xmin := X;
      if (X > Xmax) then Xmax := X;
    end
  end

  // This is for computed output variables
  else
  begin
    SetLength(Y, LinkCount);
    if (Period = CurrentPeriod) then
      for J := 0 to LinkCount - 1 do Y[J] := Zlink[J]
    else
      GetLinkOutVals(LinkVar, Period, Y);
    for J := 0 to N do
    begin
      M := Project.GetLink(CONDUIT, J).OutFileIndex;
      if (M < 0) then continue;
      if Y[M] = MISSING then continue;
      X := Abs(Y[M]);
      if (X < Xmin) then Xmin := X;
      if (X > Xmax) then Xmax := X;
    end;
  end;
end;


function GetLinkOutVal(const V: Integer; const Period: LongInt;
  const Zindex: Integer):Single;
//-----------------------------------------------------------------------------
//  Returns the computed value for variable V at time period Period
//  for link LinkIndex.
//-----------------------------------------------------------------------------
var
  P: Int64;
begin
  Result := MISSING;
  if (Zindex < 0) or (V >= NlinkVars) then Exit;
  P := Offset1 + Period*Offset2 + SizeOf(TDateTime) +
       RecordSize*(Nsubcatchs*NsubcatchVars + Nnodes*NnodeVars +
       Zindex*NlinkVars + V);
  FileSeek(Fout, P, 0);
  FileRead(Fout, Result, SizeOf(Single));
end;


procedure GetLinkOutVals(const LinkVar: Integer; const Period: LongInt;
  var Value: array of Single);
//-----------------------------------------------------------------------------
//  Gets computed results for all links from the output file where:
//  LinkVar = link variable code
//  Period  = time period index
//  Value   = array that stores the retrieved values
//-----------------------------------------------------------------------------
var
  P1, P2: Int64;
  I, K : Integer;
begin
  if (Nlinks > 0) and (LinkVar <> NONE) and (Period < Nperiods) then
  begin
    K := GetVarIndex(LinkVar, LINKS);
    P1 := Offset1 + Period*Offset2 + SizeOf(TDateTime) +
          RecordSize*(Nsubcatchs*NSubcatchVars + Nnodes*NnodeVars + K);
    FileSeek(Fout, P1, 0);
    FileRead(Fout, Value[0], SizeOf(Single));
    P2 := RecordSize*(NlinkVars - 1);
    for I := 1 to Nlinks-1 do
    begin
      FileSeek(Fout, P2, 1);
      FileRead(Fout, Value[I], SizeOf(Single));
    end;
  end;
end;


function  GetLinkValStr(const LinkVar: Integer; const Period: LongInt;
  const LinkType: Integer; const LinkIndex: Integer): String;
//-----------------------------------------------------------------------------
//  Gets the string value of variable LinkVar at time period Period for
//  link of type LinkType with index LinkIndex.
//-----------------------------------------------------------------------------
var
  J: Integer;
  K: Integer;
  Z: Single;
  L: TLink;
begin
  // Default result is N/A
  Result := NA;
  Z := MISSING;
  if (LinkVar = NOVIEW) then Exit;

  // LinkVar is an input variable
  if (LinkVar < LINKRPTVAR1) then
  begin
    // Exit if link is not a conduit
    if LinkType <> CONDUIT then Exit;

    // Find index of variable V in property list
    K := LinkVariable[LinkVar].SourceIndex;
    if (K >= 0) then
    begin
      L := Project.GetLink(LinkType, LinkIndex);
      if K = CONDUIT_SLOPE_INDEX then
        Result := Format('%.2f', [GetConduitSlope(L)])
      else
        Result := GetLinkInputStr(L, K);
    end;
  end

  // LinkVar is a computed variable; make sure output results exist
  else if (RunFlag) and (Period < Nperiods) then
  begin
    L := Project.GetLink(LinkType, LinkIndex);
    if LinkVar < LINKOUTVAR1 then
    begin
      J := L.RptFileIndex;
      if J >= 0 then Z := Zlink[J];
    end
    else if LinkVar >= LINKOUTVAR1 then
    begin
      J := L.OutFileIndex;
      if J >= 0 then
        Z := GetLinkOutVal(GetVarIndex(LinkVar, LINKS), Period, J);
    end;
    if (Z <> MISSING) then
    begin
      if LinkVar > LINKQUAL then
        K := LINKQUAL
      else
        K := LinkVar;
      Result := FloatToStrF(Z, ffFixed, 7, LinkUnits[K].Digits);
    end;
  end;
end;


procedure GetMeterLabel(const ObjType, ObjIndex: Integer;
  var IDStr, ValStr: String);
//-----------------------------------------------------------------------------
//  Retrieves an object's ID label and value of its map view variable.
//  This procedure is used to supply text for flyover map labeling.
//-----------------------------------------------------------------------------
var
  Units: String;
begin
  IDStr := ' ' + ObjectLabels[ObjType] + ' ' +
           Project.GetID(ObjType,ObjIndex) + ' ';
  ValStr := '';
  Units := '';

  if (ObjType = SUBCATCH) and (CurrentSubcatchVar <> NOVIEW) then
  begin
    ValStr := GetSubcatchValStr(CurrentSubcatchVar, CurrentPeriod, ObjIndex);
    if CurrentSubcatchVar < SUBCATCHQUAL then
      Units := ' ' + SubcatchUnits[CurrentSubcatchVar].Units
    else
      Units := ' ' + Project.PollutUnits[CurrentSubcatchVar - SUBCATCHQUAL];
  end;

  if (Project.IsNode(ObjType)) and (CurrentNodeVar <> NOVIEW) then
  begin
    ValStr := GetNodeValStr(CurrentNodeVar, CurrentPeriod, ObjType, ObjIndex);
    if CurrentNodeVar < NODEQUAL then
      Units := ' ' + NodeUnits[CurrentNodeVar].Units
    else
      Units := ' ' + Project.PollutUnits[CurrentNodeVar - NODEQUAL];
  end;

  if (Project.IsLink(ObjType)) and (CurrentLinkVar <> NOVIEW) then
  begin
    ValStr := GetLinkValStr(CurrentLinkVar, CurrentPeriod, ObjType, ObjIndex);
    if CurrentLinkVar < LINKQUAL then
      Units := ' ' + LinkUnits[CurrentLinkVar].Units
    else
      Units := ' ' + Project.PollutUnits[CurrentLinkVar - LINKQUAL];
  end;
  ValStr := ValStr + Units;

end;


procedure GetNodeFixedData(const K: Integer; var X: array of Single);
//-----------------------------------------------------------------------------
//  Retrieves the following design parameters for Node K that were saved to
//  the binary output file: invert elev. and max. depth.
//-----------------------------------------------------------------------------
var
  BytePos: Integer;
  Y: array[0..1] of Single;
  I: Integer;
begin
  BytePos := Offset0 +                      // prolog records
             (2+Nsubcatchs)*RecordSize +    // subcatchment data
             (4+3*K)*RecordSize +           // node data prior to node K
             RecordSize;                    // node type code
  FileSeek(Fout, BytePos, 0);
  FileRead(Fout, Y, 2*Sizeof(Single));
  for I := 0 to 1 do X[I] := Y[I];
end;


procedure GetNodeMinMax(const NodeVar: Integer; const Period: LongInt;
            var Xmin: Single; var Xmax: Single);
//-----------------------------------------------------------------------------
//  Gets the min & max values of node variable NodeVar at time period Period.
//-----------------------------------------------------------------------------
var
  I, J ,K   : Integer;
  X         : Single;
  Y         : array of Single;
begin
  Xmin := -MISSING;
  Xmax := MISSING;

  // This is for input design variables
  if (NodeVar < NODEOUTVAR1) then
  begin
    K := NodeVariable[NodeVar].SourceIndex;
    for I := JUNCTION to STORAGE do
    begin
      for J := 0 to Project.Lists[I].Count - 1 do
      begin
        if not Uutils.GetSingle(Project.GetNode(I, J).Data[K], X)
        then continue;
        X := Abs(X);
        if (X < Xmin) then Xmin := X;
        if (X > Xmax) then Xmax := X;
      end;
    end;
  end

  // This is for computed output variables
  else
  begin
    SetLength(Y, NodeCount);
    if (Period = CurrentPeriod) then
      for J := 0 to NodeCount - 1 do Y[J] := Znode[J]
    else
      GetNodeOutVals(NodeVar, Period, Y);
    for J := 0 to NodeCount-1 do
    begin
      if Y[J] = MISSING then continue;
      X := Abs(Y[J]);
      if (X < Xmin) then Xmin := X;
      if (X > Xmax) then Xmax := X;
    end;
  end;
end;


function  GetNodeOutVal(const V: Integer; const Period: LongInt;
  const Zindex: Integer):Single;
//-----------------------------------------------------------------------------
//  Returns the computed value for variable V at time period Period
//  for node NodeIndex.
//-----------------------------------------------------------------------------
var
  P: Int64;
begin
  Result := MISSING;
  if (Zindex < 0) or (V >= NnodeVars) then Exit;
  P := Offset1 + Period*Offset2 + SizeOf(TDateTime) +
       RecordSize*(Nsubcatchs*NsubcatchVars + Zindex*NnodeVars + V);
  FileSeek(Fout, P, 0);
  FileRead(Fout, Result, SizeOf(Single));
end;


procedure GetNodeOutVals(const NodeVar: LongInt; const Period: Integer;
  var Value: array of Single);
//-----------------------------------------------------------------------------
//  Gets computed results for all nodes from the output file where:
//  NodeVar = node variable code
//  Period  = time period index
//  Value   = array that stores the retrieved values
//-----------------------------------------------------------------------------
var
  P1, P2: Int64;
  I, K: Integer;
begin
  if (Nnodes > 0) and (NodeVar <> NONE) and (Period < Nperiods) then
  begin
    K := GetVarIndex(NodeVar, NODES);
    P1 := Offset1 + Period*Offset2 + SizeOf(TDateTime) +
          RecordSize*(Nsubcatchs*NsubcatchVars + K);
    FileSeek(Fout, P1, 0);
    FileRead(Fout, Value[0], SizeOf(Single));
    P2 := RecordSize*(NnodeVars-1);
    for I := 1 to Nnodes-1 do
    begin
      FileSeek(Fout, P2, 1);
      FileRead(Fout, Value[I], SizeOf(Single));
    end;
  end;
end;


function  GetNodeValStr(const NodeVar: Integer; const Period: LongInt;
  const NodeType: Integer; const NodeIndex: Integer): String;
//-----------------------------------------------------------------------------
//  Gets the string value of variable NodeVar at time period Period for
//  node of type NodeType with index NodeIndex.
//-----------------------------------------------------------------------------
var
  J: Integer;
  K: Integer;
  Z: Single;
  N: TNode;
begin
  // Default result is N/A
  Result := NA;
  Z := MISSING;
  if (NodeVar = NOVIEW) then Exit;

  // NodeVar is an input design variable
  if (NodeVar < NODERPTVAR1) then
  begin
    // Find index of variable NodeVar in property list
    K := NodeVariable[NodeVar].SourceIndex;
    if (K >= 0) then Result := Project.GetNode(NodeType, NodeIndex).Data[K];
  end

  // NodeVar is a computed variable; make sure output results exist
  else if (RunFlag) and (Period < Nperiods) then
  begin
    N := Project.GetNode(NodeType, NodeIndex);
    if NodeVar < NODEOUTVAR1 then
    begin
      J := N.RptFileIndex;
      if J >= 0 then
        Z := Znode[J]
    end
    else if NodeVar >= NODEOUTVAR1 then
    begin
      J := N.OutFileIndex;
      if J >= 0 then
        Z := GetNodeOutVal(GetVarIndex(NodeVar, NODES), Period, J);
    end;
    if (Z <> MISSING) then
    begin
      if NodeVar > NODEQUAL then
        K := NODEQUAL
      else
        K := NodeVar;
      Result := FloatToStrF(Z, ffFixed, 7, NodeUnits[K].Digits);
    end;
  end;
end;

function GetRunFlag(const Fname: String): Boolean;
//-----------------------------------------------------------------------------
//  Activates a previously saved set of results files for the project
//  whose file name is Fname.
//-----------------------------------------------------------------------------
var
  F1: String;
  F2: String;
begin
  // Derive the report and output file names from the input file Fname
  Result := False;
  F1 := ChangeFileExt(Fname, '.rpt');
  F2 := ChangeFileExt(Fname, '.out');

  // Make sure that these file names are not same as the input file name
  // and that the files exist
  if (SameText(F1, Fname)) or (SameText(F1, Fname)) then Exit;
  if not FileExists(F1) or not FileExists(F2) then Exit;

  // See if these files contain valid results
  Uglobals.RunStatus := rsNone;
  if GetFileSize(F1) <= 0
  then Uglobals.RunStatus := rsFailed
  else Uglobals.RunStatus := CheckRunStatus(F2);

  // If the files are valid then rename the current temporary files
  // that are normally used to store results
  if RunStatus in [rsSuccess, rsWarning] then
  begin
    Result := True;
    Uglobals.ResultsSaved := True;
    Uglobals.TempReportFile := F1;
    Uglobals.TempOutputFile := F2;
  end
  else ResultsSaved := False;
end;


procedure GetString(var F: File; var S: ShortString);
//-----------------------------------------------------------------------------
//  Reads a fixed-size string from the current position of file F.
//  (F must be declared as 'var' because BlockRead is used.)
//-----------------------------------------------------------------------------
var
  Buf: PAnsiChar;
  Size: Word;
begin
  Size := SizeOf(S);
  Buf := AnsiStrAlloc(Size);
  BlockRead(F, Buf^, Size-1);
  S := AnsiStrings.StrPas(Buf);
  AnsiStrings.StrDispose(Buf);
end;


procedure GetSubcatchMinMax(const SubcatchVar: Integer; const Period: LongInt;
  var Xmin: Single; var Xmax: Single);
//-----------------------------------------------------------------------------
// Gets the min & max values of a given subcatchment variable.
//-----------------------------------------------------------------------------
var
  J, K : Integer;
  X : Single;
  Y : array of Single;
begin
  Xmin := -MISSING;
  Xmax := MISSING;
  if Nsubcatchs = 0 then Exit;

  // This is for input property variables
  if (SubcatchVar < SUBCATCHOUTVAR1) then
  begin
      K := SubcatchVariable[SubcatchVar].SourceIndex;
      for J := 0 to Project.Lists[SUBCATCH].Count - 1 do
      begin
        if not Uutils.GetSingle(Project.GetSubcatch(SUBCATCH, J).Data[K], X)
        then continue;
        X := Abs(X);
        if (X < Xmin) then Xmin := X;
        if (X > Xmax) then Xmax := X;
      end;
  end

  // This is for computed variables
  else
  begin
    SetLength(Y, SubcatchCount);
    if (Period = CurrentPeriod) then
      for J  := 0 to SubcatchCount - 1 do Y[J] := Zsubcatch[J]
    else
      GetSubcatchOutVals(SubcatchVar, Period, Y);
    for J := 0 to SubcatchCount - 1 do
    begin
      if Y[J] = MISSING then continue;
      X := Abs(Y[J]);
      if (X < Xmin) then Xmin := X;
      if (X > Xmax) then Xmax := X;
    end;
  end;
end;


function GetSubcatchOutVal(const V: Integer; const Period: LongInt;
  const Zindex: Integer): Single;
//-----------------------------------------------------------------------------
//  Returns the computed value for variable V at time period Period
//  for subcatchment with position Zindex in list of reported subcatchments.
//-----------------------------------------------------------------------------
var
  P: Int64;
begin
  Result := MISSING;
  if (Zindex < 0) or (V >= NsubcatchVars) then Exit;
  P := Offset1 + Period*Offset2 + SizeOf(TDateTime) +
       RecordSize*(Zindex*NsubcatchVars + V);
  FileSeek(Fout, P, 0);
  FileRead(Fout, Result, SizeOf(Single));
end;


procedure GetSubcatchOutVals(const SubcatchVar: Integer; const Period: LongInt;
  var Value: array of Single);
//-----------------------------------------------------------------------------
//  Gets computed results for all subcatchments from the output file where:
//  SubcatchVar = subcatchment variable code
//  Period  = time period index
//  Value   = array that stores the retrieved values
//-----------------------------------------------------------------------------
var
  P1, P2: Int64;
  I, K: Integer;
begin
  if (Nsubcatchs > 0) and (SubcatchVar <> NONE) and (Period < Nperiods) then
  begin
    K := GetVarIndex(SubcatchVar, SUBCATCHMENTS);
    P1 := Offset1 + Period*Offset2 + SizeOf(TDateTime) + K*RecordSize;
    FileSeek(Fout, P1, 0);
    FileRead(Fout, Value[0], SizeOf(Single));
    P2 := RecordSize*(NsubcatchVars-1);
    for I := 1 to Nsubcatchs - 1 do
    begin
      FileSeek(Fout, P2, 1);
      FileRead(Fout, Value[I], SizeOf(Single));
    end;
  end;
end;


function GetSubcatchValStr(const SubcatchVar: Integer; const Period: LongInt;
  const SubcatchIndex: Integer): String;
//-----------------------------------------------------------------------------
//  Gets the string value of variable SubcatchVar at time period Period for
//  subcatchment with index SubcatchIndex.
//-----------------------------------------------------------------------------
var
  J, K:  Integer;
  Z: Single;
  S: TSubcatch;
begin
  // Default result is N/A
  Result := NA;
  Z := MISSING;
  if (SubcatchVar = NOVIEW) then Exit;

  // SubcatchVar is an input design variable
  if (SubcatchVar < SUBCATCHRPTVAR1) then
  begin
    // Find index of variable SubcatchVar in property list
    K := SubcatchVariable[SubcatchVar].SourceIndex;
    if (K >= 0) then
    begin
      if K = SUBCATCH_LID_INDEX
      then Result := Format('%0.2f', [Ulid.GetPcntLidArea(SubcatchIndex)])
      else Result := Project.GetSubcatch(SUBCATCH, SubcatchIndex).Data[K];
    end;
  end

  // SubcatchVar is a computed variable; make sure output results exist
  else if (RunFlag) and (Period < Nperiods) then
  begin
    S :=Project.GetSubcatch(SUBCATCH, SubcatchIndex);
    if SubcatchVar < SUBCATCHOUTVAR1 then
    begin
      J := S.RptFileIndex;
      if J >= 0 then
        Z := Zsubcatch[J];
    end
    else if SubcatchVar >= SUBCATCHOUTVAR1 then
    begin
      J := S.OutFileIndex;
      if J >= 0 then
        Z := GetSubcatchOutVal(GetVarIndex(SubcatchVar, SUBCATCHMENTS), Period, J);
    end;
    if (Z <> MISSING) then
    begin
      if SubcatchVar > SUBCATCHQUAL then
        K := SUBCATCHQUAL
      else
        K := SubcatchVar;
      Result := FloatToStrF(Z, ffFixed, 7, SubcatchUnits[K].Digits);
    end;
  end;
end;


function  GetSysOutVal(const V: Integer; const Period: LongInt):Single;
//-----------------------------------------------------------------------------
//  Returns the computed value for system variable V at time period Period.
//-----------------------------------------------------------------------------
var
  P: Int64;
begin
  P := Offset1 + Period*Offset2 + SizeOf(TDateTime) +
       RecordSize*(Nsubcatchs*NsubcatchVars + Nnodes*NnodeVars +
       NLinks*NlinkVars + V);
  FileSeek(Fout, P, 0);
  FileRead(Fout, Result, SizeOf(Single));
end;


function GetValue(const ObjType: Integer; const VarIndex: Integer;
  const Period: LongInt; theObject: TObject): Double;
//-----------------------------------------------------------------------------
//  Returns the value of variable VarIndex at time period Period for
//  an object of type ObjType.
//-----------------------------------------------------------------------------
begin
  Result := MISSING;
  case ObjType of
    SUBCATCHMENTS:
      Result := GetSubcatchOutVal(VarIndex, Period,
                  TSubcatch(theObject).OutFileIndex);
    NODES:
      Result := GetNodeOutVal(VarIndex, Period, TNode(theObject).OutFileIndex);
    LINKS:
      Result := GetLinkOutVal(VarIndex, Period, TLink(theObject).OutFileIndex);
    SYS:
      Result := GetSysOutVal(VarIndex, Period);
  end;
end;


function GetVarIndex(const V: Integer; const ObjType: Integer): Integer;
//-----------------------------------------------------------------------------
//  Gets a lookup index for variable V associated with an object of
//  type ObjType.
//-----------------------------------------------------------------------------
begin
  case ObjType of

  SUBCATCHMENTS:
  begin
    if V < SUBCATCHQUAL
    then Result := SubcatchVariable[V].SourceIndex
    else Result := SubcatchVariable[SUBCATCHQUAL].SourceIndex +
                   V - SUBCATCHQUAL;
  end;

  LINKS:
  begin
    if V < LINKQUAL
    then Result := LinkVariable[V].SourceIndex
    else Result := LinkVariable[LINKQUAL].SourceIndex + V - LINKQUAL;
  end;

  NODES:
  begin
    if V < NODEQUAL
    then Result := NodeVariable[V].SourceIndex
    else Result := NodeVariable[NODEQUAL].SourceIndex + V - NODEQUAL;
  end;

  SYS:
   Result := V;

  else Result := 0;
  end;
end;


function OpenOutputFile(const Fname: String): TRunStatus;
//-----------------------------------------------------------------------------
//  Opens the project's binary output file.
//-----------------------------------------------------------------------------
//var
//  StrFname: String;
begin
  Result := rsSuccess;
  Fout := FileOpen(Fname, fmOpenRead);
  if Fout < 0 then Result := rsError;
end;


procedure SetLinkColor(L: TLink; const K: Integer);
//-----------------------------------------------------------------------------
//  Sets the map color code for input property K of Link L.
//-----------------------------------------------------------------------------
var
  X : Single;
begin
  L.ColorIndex := -1;
  if K >= 0 then
  begin
    if K = CONDUIT_SLOPE_INDEX
    then L.ColorIndex := FindLinkColor(Abs(GetConduitSlope(L)))
    else if Uutils.GetSingle(L.Data[K], X)
    then L.ColorIndex := FindLinkColor(Abs(X));
  end;
end;


procedure SetLinkColors;
//-----------------------------------------------------------------------------
//  Sets the map color coding for all links.
//-----------------------------------------------------------------------------
begin
  if CurrentLinkVar < LINKRPTVAR1 then
    SetLinkInColors(CurrentLinkVar)
  else if (RunFlag = False) then
    SetLinkInColors(NOVIEW)
  else if CurrentLinkVar >= LINKOUTVAR1 then
    SetLinkOutColors(CurrentLinkVar)
  else
    SetLinkRptColors(CurrentLinkVar);
end;


procedure SetLinkInColors(const LinkVar: Integer);
//-----------------------------------------------------------------------------
//  Sets the map color coding for all links displaying input
//  variable LinkVar.
//-----------------------------------------------------------------------------
var
  I, J, K : Integer;
begin
  for I := 0 to MAXCLASS do
  begin
    if not Project.IsLink(I) then continue;
    if LinkVar = NOVIEW then K := -1
    else if I = CONDUIT then K := LinkVariable[LinkVar].SourceIndex
    else K := -1;
    for J := 0 to Project.Lists[I].Count-1 do
      SetLinkColor(Project.GetLink(I, J), K);
  end;
end;


procedure SetLinkRptColors(const LinkVar:Integer);
//-----------------------------------------------------------------------------
//  Sets the map color coding for all links displaying summary report
//  variable LinkVar.
//-----------------------------------------------------------------------------
var
  I, J, K : Integer;
  L : TLink;
begin
  for I := 0 to LinkCount-1 do Zlink[I] := 0;
  Uresults.GetLinkRptValues(LinkVar, Zlink);
  for I := 0 to MAXCLASS do
  begin
    if not Project.IsLink(I) then continue;
    for J := 0 to Project.Lists[I].Count-1 do
    begin
      L := Project.GetLink(I, J);
      K := L.RptFileIndex;
      if K >= 0 then
        L.ColorIndex := FindLinkColor(abs(Zlink[K]))
      else
        L.ColorIndex := -1;
    end;
  end;
end;


procedure SetLinkOutColors(const LinkVar: Integer);
//-----------------------------------------------------------------------------
//  Sets the map color coding for all links displaying output
//  variable LinkVar.
//-----------------------------------------------------------------------------
var
  I, J, K : Integer;
  L : TLink;
begin
  GetLinkOutVals(LinkVar, CurrentPeriod, Zlink);
  for I := 0 to MAXCLASS do
  begin
    if not Project.IsLink(I) then continue;
    for J := 0 to Project.Lists[I].Count-1 do
    begin
      L := Project.GetLink(I, J);
      K := L.OutFileIndex;
      if K >= 0 then
        L.ColorIndex := FindLinkColor(abs(Zlink[K]))
      else
        L.ColorIndex := -1;
    end;
  end;
end;


procedure SetNodeColor(N: TNode; const K: Integer);
//-----------------------------------------------------------------------------
//  Sets the map color code for input property K of node N.
//-----------------------------------------------------------------------------
var
  X : Single;
begin
  N.ColorIndex := -1;
  if K >= 0 then
  begin
    if Uutils.GetSingle(N.Data[K], X) then
      N.ColorIndex := FindNodeColor(X);
  end;
end;


procedure SetNodeColors;
//-----------------------------------------------------------------------------
//  Sets the map color coding for all nodes.
//-----------------------------------------------------------------------------
begin
  if CurrentNodeVar < NODERPTVAR1 then
    SetNodeInColors(CurrentNodeVar)
  else if (RunFlag = False) then
    SetNodeInColors(NOVIEW)
  else if CurrentNodeVar >= NODEOUTVAR1 then
    SetNodeOutColors(CurrentNodeVar)
  else
    SetNodeRptColors(CurrentNodeVar);
end;


procedure SetNodeRptColors(const NodeVar:Integer);
//-----------------------------------------------------------------------------
//  Sets the map color coding for all nodes displaying summary report
//  variable NodeVar.
//-----------------------------------------------------------------------------
var
  I, J, K : Integer;
  N : TNode;
begin
  for I := 0 to NodeCount-1 do Znode[I] := 0;
  Uresults.GetNodeRptValues(NodeVar, Znode);
  for I := 0 to MAXCLASS do
  begin
    if not Project.IsNode(I) then continue;
    for J := 0 to Project.Lists[I].Count-1 do
    begin
      N:= Project.GetNode(I, J);
      K := N.RptFileIndex;
      if K >= 0 then
        N.ColorIndex := FindNodeColor(abs(Znode[K]))
      else
        N.ColorIndex := -1;
    end;
  end;
end;


procedure SetNodeInColors(const NodeVar: Integer);
//-----------------------------------------------------------------------------
//  Sets the map color coding for all nodes displaying input
//  variable NodeVar.
//-----------------------------------------------------------------------------
var
  I, J, K : Integer;
begin
  for I := 0 to MAXCLASS do
  begin
    if not Project.IsNode(I) then continue;
    if NodeVar = NOVIEW then
      K := -1
    else
      K := NodeVariable[NodeVar].SourceIndex;
    for J := 0 to Project.Lists[I].Count-1 do
      SetNodeColor(Project.GetNode(I, J), K);
  end;
end;


procedure SetNodeOutColors(const NodeVar: Integer);
//-----------------------------------------------------------------------------
//  Sets the map color coding for all nodes displaying output
//  variable NodeVar.
//-----------------------------------------------------------------------------
var
  I, J ,K : Integer;
  N : TNode;
begin
  GetNodeOutVals(NodeVar, CurrentPeriod, Znode);
  for I := 0 to MAXCLASS do
  begin
    if not Project.IsNode(I) then continue;
    for J := 0 to Project.Lists[I].Count-1 do
    begin
      N := Project.GetNode(I, J);
      K := N.OutFileIndex;
      if K >= 0 then
        N.ColorIndex := FindNodeColor(Znode[K])
      else
        N.ColorIndex := -1;
    end;
  end;
end;


function SetQueryColor(const Z: Single): Integer;
//-----------------------------------------------------------------------------
//  Sets the color code in a map query for a value Z.
//-----------------------------------------------------------------------------
begin
  Result := -1;
  case QueryRelation of
  rtBelow: if Z < QueryValue then Result := 1;
  rtEquals: if Z = QueryValue then Result := 1;
  rtAbove: if Z > QueryValue then Result := 1;
  end;
end;


procedure SetSubcatchColor(S: TSubcatch; const K: Integer);
//-----------------------------------------------------------------------------
//  Sets the map color code for input property K of subcatchment S.
//-----------------------------------------------------------------------------
var
  X : Single;
begin
  S.ColorIndex := -1;
  if K >= 0 then
  begin
    if Uutils.GetSingle(S.Data[K], X) then
      S.ColorIndex := FindSubcatchColor(Abs(X));
  end;
end;


procedure SetSubcatchColors;
//-----------------------------------------------------------------------------
//  Sets the map color coding for all subcatchments.
//-----------------------------------------------------------------------------
begin
  if CurrentSubcatchVar < SUBCATCHRPTVAR1 then
    SetSubcatchInColors(CurrentSubcatchVar)
  else if (RunFlag = False) then
    SetSubcatchInColors(NOVIEW)
  else if CurrentSubcatchVar >= SUBCATCHOUTVAR1 then
    SetSubcatchOutColors(CurrentSubcatchVar)
  else
    SetSubcatchRptColors(CurrentSubcatchVar);
end;


procedure SetSubcatchInColors(const SubcatchVar: Integer);
//-----------------------------------------------------------------------------
//  Sets the map color coding for all subcatchments displaying input
//  variable SubcatchVar.
//-----------------------------------------------------------------------------
var
  J, K : Integer;
  S: TSubcatch;
begin
  if SubcatchVar = NOVIEW then
    K := -1
  else
    K := SubcatchVariable[SubcatchVar].SourceIndex;
  for J := 0 to Project.Lists[SUBCATCH].Count-1 do
  begin
    S := Project.GetSubcatch(SUBCATCH, J);
    if K = SUBCATCH_LID_INDEX then
      S.ColorIndex := FindSubcatchColor(Ulid.GetPcntLidArea(J))
    else
      SetSubcatchColor(S, K);
  end;
end;


procedure SetSubcatchRptColors(const SubcatchVar: Integer);
//-----------------------------------------------------------------------------
//  Sets the map color coding for all subcatchments displaying summary report
//  variable SubcatchVar.
//-----------------------------------------------------------------------------
var
  I, J, K : Integer;
  S : TSubcatch;
begin
  for I := 0 to SubcatchCount-1 do Zsubcatch[I] := 0;
  Uresults.GetSubcatchRptValues(SubcatchVar, Zsubcatch);
  for J := 0 to SubcatchCount-1 do
  begin
    S:= Project.GetSubcatch(SUBCATCH, J);
    K := S.RptFileIndex;
    if K >= 0 then
      S.ColorIndex := FindSubcatchColor(abs(Zsubcatch[K]))
    else
      S.ColorIndex := -1;
  end;
end;


procedure SetSubcatchOutColors(const SubcatchVar: Integer);
//-----------------------------------------------------------------------------
//  Sets the map color coding for all subcatchments displaying output
//  variable SubcatchVar.
//-----------------------------------------------------------------------------
var
  J, K : Integer;
  S : TSubcatch;
begin
  GetSubcatchOutVals(SubcatchVar, CurrentPeriod, Zsubcatch);
  for J := 0 to Project.Lists[SUBCATCH].Count-1 do
  begin
    S := Project.GetSubcatch(SUBCATCH, j);
    K := S.OutFileIndex;
    if K >= 0 then
      S.ColorIndex := FindSubcatchColor(Zsubcatch[K])
    else
      S.ColorIndex := -1;
  end;
end;

end.
