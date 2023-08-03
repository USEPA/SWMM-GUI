unit Uimport;

{-------------------------------------------------------------------}
{                    Unit:    Uimport.pas                           }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    06/28/23    (5.2.4)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Delphi Pascal unit that imports a SWMM project's data from a    }
{   a formatted text file.                                          }
{-------------------------------------------------------------------}

interface

uses
  Classes, Forms, Controls, Dialogs, SysUtils, Windows, Math, StrUtils,
  Uutils, Uglobals;

const
  ITEMS_ERR    = 1;
  KEYWORD_ERR  = 2;
  SUBCATCH_ERR = 3;
  NODE_ERR     = 4;
  LINK_ERR     = 5;
  LANDUSE_ERR  = 6;
  POLLUT_ERR   = 7;
  NUMBER_ERR   = 8;
  XSECT_ERR    = 9;
  TRANSECT_ERR = 10;
  TIMESTEP_ERR = 11;
  DATE_ERR     = 12;
  LID_ERR      = 13;
  INLET_ERR    = 14;
  MAX_ERRORS   = 50;

type
  TFileType = (ftInput, ftImport);

// These routines can be called from other units
function  ErrMsg(const ErrCode: Integer; const Name: String): Integer;
function  OpenProject(const Fname: String): TInputFileType;
function  ReadInpFile(const Fname: String):Boolean;
function  FullPathName(Fname: String): String;
procedure SetDefaultDates;

implementation

uses
  Fmain, Fmap, Fstatus, Dxsect, Uexport, Uinifile, Uproject, Umap,
  Ucoords, Uvertex, Uupdate, Dreporting, Ulid, Uinlet;

const
  MSG_READING_PROJECT_DATA = 'Reading project data... ';
  TXT_ERROR = 'Error ';
  TXT_AT_LINE = ' at line ';
  TXT_MORE_ERRORS = ' more errors found in file.';
  TXT_ERROR_REPORT = 'Error Report for File ';

  SectionWords : array[0..57] of PChar =
    ('[TITLE',                    //0
     '[OPTION',                   //1
     '[RAINGAGE',                 //2
     '[HYDROGRAPH',               //3
     '[EVAPORATION',              //4
     '[SUBCATCHMENT',             //5
     '[SUBAREA',                  //6
     '[INFILTRATION',             //7
     '[AQUIFER',                  //8
     '[GROUNDWATER',              //9
     '[JUNCTION',                 //10
     '[OUTFALL',                  //11
     '[STORAGE',                  //12
     '[DIVIDER',                  //13
     '[CONDUIT',                  //14
     '[PUMP',                     //15
     '[ORIFICE',                  //16
     '[WEIR',                     //17
     '[OUTLET',                   //18
     '[XSECTION',                 //19
     '[TRANSECT',                 //20
     '[LOSS',                     //21
     '[CONTROL',                  //22
     '[POLLUTANT',                //23
     '[LANDUSE',                  //24
     '[BUILDUP',                  //25
     '[WASHOFF',                  //26
     '[COVERAGE',                 //27
     '[INFLOW',                   //28
     '[DWF',                      //29
     '[PATTERN',                  //30
     '[RDII',                     //31
     '[LOAD',                     //32
     '[CURVE',                    //33
     '[TIMESERIES',               //34
     '[REPORT',                   //35
     '[FILE',                     //36
     '[MAP',                      //37
     '[COORDINATES',              //38
     '[VERTICES',                 //39
     '[POLYGONS',                 //40
     '[SYMBOLS',                  //41
     '[LABELS',                   //42
     '[BACKDROP',                 //43
     '[PROFILE',                  //44
     '[TABLE',                    //45
     '[TEMPERATURE',              //46
     '[SNOWPACK',                 //47
     '[TREATMENT',                //48
     '[TAG',                      //49
     '[LID_CONTROL',              //50
     '[LID_USAGE',                //51
     '[GWF',                      //52
     '[ADJUSTMENTS',              //53
     '[EVENT',                    //54
     '[STREET',                   //55
     '[INLET_USAGE',              //56
     '[INLET');                   //57

var
  FileType     : TFileType;
  InpFile      : String;
  ErrList      : TStringlist;
  SubcatchList : TStringlist;
  NodeList     : TStringlist;
  LinkList     : TStringlist;
  TokList      : TStringlist;
  Ntoks        : Integer;
  Section      : Integer;
  LineCount    : LongInt;
  ErrCount     : LongInt;
  Line         : String;
  Comment      : String;
  TsectComment : String;
  PrevID       : String;
  PrevIndex    : Integer;
  CurveType    : Integer;
  MapExtentSet : Boolean;
  ManningsN    : array[1..3] of String;

  // These are deprecated attributes of a backdrop image
  BackdropX    : Extended;
  BackdropY    : Extended;
  BackdropW    : Extended;
  BackdropH    : Extended;


function ErrMsg(const ErrCode: Integer; const Name: String): Integer;
//-----------------------------------------------------------------------------
//  Adds an error message for a specific error code to the list
//  of errors encountered when reading an input file.
//-----------------------------------------------------------------------------
var
  S: String;
begin
  if ErrCount <= MAX_ERRORS then
  begin
    case ErrCode of
      ITEMS_ERR:    S := 'Too few items ';
      KEYWORD_ERR:  S := 'Unrecognized keyword (' + Name + ') ';
      SUBCATCH_ERR: S := 'Undefined Subcatchment (' + Name + ') referenced ';
      NODE_ERR:     S := 'Undefined Node (' + Name + ') referenced ';
      LINK_ERR:     S := 'Undefined Link (' + Name + ') referenced ';
      LANDUSE_ERR:  S := 'Undefined Land Use (' + Name + ') referenced ';
      POLLUT_ERR:   S := 'Undefined Pollutant (' + Name + ') referenced ';
      NUMBER_ERR:   S := 'Illegal numeric value (' + Name + ') ';
      XSECT_ERR:    S := 'Illegal cross section for Link ' + Name + ' ';
      TRANSECT_ERR: S := 'No Transect defined for these data ';
      TIMESTEP_ERR: S := 'Illegal time step value ';
      DATE_ERR:     S := 'Illegal date/time value ';
      LID_ERR:      S := 'Undefined LID process (' + Name + ') referenced ';
      INLET_ERR:    S := 'Undefined Inlet (' + Name + ') referenced ';
      else          S := 'Unknown error ';
    end;
    S := S + 'at line ' + IntToStr(LineCount) + ':';
    ErrList.Add(S);
    if Section >= 0 then ErrList.Add(SectionWords[Section] + ']');
    ErrList.Add(Line);
    ErrList.Add('');
  end;
  Result := ErrCode;
end;


function FindSubcatch(const ID: String): TSubcatch;
//-----------------------------------------------------------------------------
//  Finds a Subcatchment object given its ID name.
//-----------------------------------------------------------------------------
var
  Index: Integer;
  Atype: Integer;
begin
  Result := nil;
  if (FileType = ftInput) then
  begin
    if SubcatchList.Find(ID, Index)
    then Result := TSubcatch(SubcatchList.Objects[Index]);
  end
  else
  begin
    if (Project.FindSubcatch(ID, Atype, Index))
    then Result := Project.GetSubcatch(Atype, Index);
  end;
end;


function FindNode(const ID: String): TNode;
//-----------------------------------------------------------------------------
// Finds a Node object given its ID name.
//-----------------------------------------------------------------------------
var
  Index: Integer;
  Ntype: Integer;
begin
  Result := nil;
  if (FileType = ftInput) then
  begin
    if NodeList.Find(ID, Index)
    then Result := TNode(NodeList.Objects[Index]);
  end
  else
  begin
    if (Project.FindNode(ID, Ntype, Index))
    then Result := Project.GetNode(Ntype, Index);
  end;
end;


function FindLink(const ID: String): TLink;
//-----------------------------------------------------------------------------
// Finds a Link object given its ID name.
//-----------------------------------------------------------------------------
var
  Index : Integer;
  Ltype : Integer;
begin
  Result := nil;
  if (FileType = ftInput) then
  begin
    if LinkList.Find(ID, Index)
    then Result := TLink(LinkList.Objects[Index]);
  end
  else
  begin
    if (Project.FindLink(ID, Ltype, Index))
    then Result := Project.GetLink(Ltype, Index);
  end;
end;


function  FullPathName(Fname: String): String;
//-----------------------------------------------------------------------------
// Converts a file name to absolute path.
//-----------------------------------------------------------------------------
begin
  if Length(Fname) = 0 then Result := Fname
  else if Fname[1] = PathDelim then Result := Fname
  else if Pos(':', Fname) > 0 then Result := Fname
  else Result := Uglobals.ProjectDir + Fname;
end;


function ReadTitleData(Line: String): Integer;
//-----------------------------------------------------------------------------
// Adds Line of text to a project's title and notes.
//-----------------------------------------------------------------------------
begin
  if Project.Lists[NOTES].Count = 0 then Project.Title := Line;
  Project.Lists[NOTES].Add(Line);
  Project.HasItems[NOTES] := True;
  Result := 0;
end;


procedure ReadOldRaingageData(I: Integer; aGage: TRaingage);
//-----------------------------------------------------------------------------
//  Reads a line of parsed rain gage data using older format.
//-----------------------------------------------------------------------------
begin
  if I = 0 then
  begin
    aGage.Data[GAGE_DATA_SOURCE] := RaingageOptions[0];
    if Ntoks > 2 then aGage.Data[GAGE_SERIES_NAME] := TokList[2];
    if Ntoks > 3 then aGage.Data[GAGE_DATA_FORMAT] := TokList[3];
    if Ntoks > 4 then aGage.Data[GAGE_DATA_FREQ]   := TokList[4];
  end;
  if I = 1 then
  begin
    aGage.Data[GAGE_DATA_SOURCE] := RaingageOptions[1];
    if Ntoks > 2 then aGage.Data[GAGE_FILE_NAME] := TokList[2];
    if Ntoks > 3 then aGage.Data[GAGE_STATION_NUM] := TokList[3];
  end;
end;


procedure ReadNewRaingageData(aGage: TRaingage);
//-----------------------------------------------------------------------------
//  Reads a line of rain gage data using newer format.
//-----------------------------------------------------------------------------
var
  I: Integer;
  Fname: String;
begin
  if Ntoks > 1 then aGage.Data[GAGE_DATA_FORMAT] := TokList[1];
  if Ntoks > 2 then aGage.Data[GAGE_DATA_FREQ]   := TokList[2];
  if Ntoks > 3 then aGage.Data[GAGE_SNOW_CATCH]  := TokList[3];
  if Ntoks > 4 then
  begin
    I := Uutils.FindKeyWord(TokList[4], RaingageOptions, 4);
    if I = 0 then
    begin
      aGage.Data[GAGE_DATA_SOURCE] := RaingageOptions[0];
      if Ntoks > 5 then aGage.Data[GAGE_SERIES_NAME] := TokList[5];
    end;
    if I = 1 then
    begin
      aGage.Data[GAGE_DATA_SOURCE] := RaingageOptions[1];
      if Ntoks > 5 then
      begin
        Fname := TokList[5];
        aGage.Data[GAGE_FILE_NAME] := Fname;
      end;
      if Ntoks > 6 then aGage.Data[GAGE_STATION_NUM] := TokList[6];
      if Ntoks > 7 then aGage.Data[GAGE_RAIN_UNITS]  := TokList[7];
    end;
  end;
end;


function ReadRaingageData: Integer;
//-----------------------------------------------------------------------------
//  Parses rain gage data from the input line.
//-----------------------------------------------------------------------------
var
  aGage   : TRaingage;
  ID      : String;
  I       : Integer;
begin
  if Ntoks < 2 then
  begin
    Result := ErrMsg(ITEMS_ERR, '');
    Exit;
  end;
  ID := TokList[0];
  aGage := TRaingage.Create;
  Uutils.CopyStringArray(Project.DefProp[RAINGAGE].Data, aGage.Data);
  aGage.X := MISSING;
  aGage.Y := MISSING;
  aGage.Data[COMMENT_INDEX ] := Comment;
  Project.Lists[RAINGAGE].AddObject(ID, aGage);
  Project.HasItems[RAINGAGE] := True;
  I := Uutils.FindKeyWord(TokList[1], RaingageOptions, 4);
  if I >= 0
    then ReadOldRaingageData(I, aGage)
    else ReadNewRaingageData(aGage);
  if Length(aGage.Data[GAGE_FILE_NAME]) > 0 then
  begin
    aGage.Data[GAGE_FILE_PATH] := FullPathName(aGage.Data[GAGE_FILE_NAME]);
    aGage.Data[GAGE_FILE_NAME] := ExtractFileName(aGage.Data[GAGE_FILE_PATH]);
  end;
  Result := 0;
end;


function ReadOldHydrographFormat(const I: Integer; UH: THydrograph): Integer;
//-----------------------------------------------------------------------------
//  Reads older format of unit hydrograph parameters from a line of input.
//-----------------------------------------------------------------------------
var
  J, K, N: Integer;
begin
  Result := 0;
  if Ntoks < 2 then Result := ErrMsg(ITEMS_ERR, '')
  else
  begin
    N := 2;
    for K := 1 to 3 do
    begin
      for J := 1 to 3 do
      begin
        UH.Params[I,J,K] := TokList[N];
        Inc(N);
      end;
    end;
    for J := 1 to 3 do
    begin
      UH.InitAbs[I,J,1] := '';
      if Ntoks > N then UH.InitAbs[I,J,1] := TokList[N];
      Inc(N);
      for K := 2 to 3 do UH.InitAbs[I,J,K] := UH.InitAbs[I,J,1];
    end;
  end;
end;


function ReadHydrographData: Integer;
//-----------------------------------------------------------------------------
//  Reads RDII unit hydrograph data from a line of input.
//-----------------------------------------------------------------------------
var
  I, J, K: Integer;
  ID: String;
  aUnitHyd: THydrograph;
  Index: Integer;
begin
  Result := 0;
  if Ntoks < 2 then Result := ErrMsg(ITEMS_ERR, '')
  else
  begin

    // Check if hydrograph ID is same as for previous line
    ID := TokList[0];
    if (ID = PrevID)
    then Index := PrevIndex
    else Index := Project.Lists[HYDROGRAPH].IndexOf(ID);

    // If starting input for a new hydrograph then create it
    if Index < 0 then
    begin
      aUnitHyd := THydrograph.Create;
      Project.Lists[HYDROGRAPH].AddObject(ID, aUnitHyd);
      Project.HasItems[HYDROGRAPH] := True;
      Index := Project.Lists[HYDROGRAPH].Count - 1;
      PrevID := ID;
      PrevIndex := Index;
    end
    else aUnitHyd := THydrograph(Project.Lists[HYDROGRAPH].Objects[Index]);

    // Parse rain gage name for 2-token line
    if Ntoks = 2 then
    begin
      aUnitHyd.Raingage := TokList[1];
    end

    // Extract remaining hydrograph parameters
    else if Ntoks < 6 then Result := ErrMsg(ITEMS_ERR, '')
    else
    begin
      // Determine month of year
      I := Uutils.FindKeyWord(TokList[1], MonthLabels, 3);
      if I < 0 then Result := ErrMsg(KEYWORD_ERR, TokList[1]);

      // Determine if response type present - if not, process old format
      K := Uutils.FindKeyWord(TokList[2], ResponseTypes, 3) + 1;
      if K < 1 then Result := ReadOldHydrographFormat(I, aUnitHyd)
      else
      begin
        // Extract R-T-K values
        for J := 3 to 5 do
        begin
          aUnitHyd.Params[I,J-2,K] := TokList[J];
        end;
        // Extract IA parameters
        for J := 6 to 8 do
        begin
          if J >= Ntoks then break
          else aUnitHyd.InitAbs[I,J-5,K] := TokList[J];
        end;
      end;
    end;
  end;
end;


function ReadTemperatureData: Integer;
//-----------------------------------------------------------------------------
//  Reads description of air temperature data from a line of input.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  Result := 0;
  if Ntoks < 2 then Result := ErrMsg(ITEMS_ERR, '')
  else with Project.Climatology do
  begin
    I := Uutils.FindKeyWord(TokList[0], TempKeywords, 10);
    if I < 0 then Result := ErrMsg(KEYWORD_ERR, TokList[0])
    else case I of
    0:  begin                                              // Time series
          TempDataSource := 1;
          TempTseries := TokList[1];
        end;
    1:  begin                                              // File
          TempDataSource := 2;
          TempFile := FullPathName(TokList[1]);
          if Ntoks >= 3 then TempStartDate := TokList[2];
          if Ntoks >= 4 then
            TempUnitsType := Uutils.FindKeyWord(TokList[3], TempUnitsKeywords, 3);
        end;
    2:  begin                                              // Wind speed
          if SameText(TokList[1], 'MONTHLY') then
          begin
            if Ntoks < 14 then Result := ErrMsg(ITEMS_ERR, '')
            else
            begin
              WindType := MONTHLY_WINDSPEED;
              for I := 1 to 12 do WindSpeed[I] := TokList[I+1];
            end;
          end
          else if SameText(TokList[1], 'FILE') then WindType := FILE_WINDSPEED
          else Result := ErrMsg(KEYWORD_ERR, TokList[1]);
        end;
    3:  begin
          if Ntoks < 7 then Result := ErrMsg(ITEMS_ERR, '')
          else for I := 1 to 6 do SnowMelt[I] := TokList[I];
        end;
    4:  begin
          if Ntoks < 12 then Result := ErrMsg(ITEMS_ERR, '')
          else
          begin
            if  SameText(TokList[1], 'IMPERVIOUS') then
              for I := 1 to 10 do ADCurve[1][I] := TokList[I+1]
            else if SameText(TokList[1], 'PERVIOUS') then
              for I := 1 to 10 do ADCurve[2][I] := TokList[I+1]
            else Result := ErrMsg(KEYWORD_ERR, TokList[1]);
          end;
        end;
    end;
  end;
end;


function ReadEvaporationRates(Etype: Integer): Integer;
var
  J: Integer;
begin
  if (Etype <> TEMP_EVAP) and (Ntoks < 2)
  then  Result := ErrMsg(ITEMS_ERR, '')

  else with Project.Climatology do
  begin
    EvapType := Etype;

    case EvapType of

      CONSTANT_EVAP:
        for J := 0 to 11 do EvapData[J] := TokList[1];

      TSERIES_EVAP:
        EvapTseries := TokList[1];

      FILE_EVAP:
        for J := 1 to 12 do
        begin
          if J >= Ntoks then break;
          PanData[J-1] := TokList[J];
        end;

      MONTHLY_EVAP:
        for J := 1 to 12 do
        begin
          if J >= Ntoks then break;
          EvapData[J-1] := TokList[J];
        end;
    end;

    Result := 0;
  end;

end;


function ReadEvaporationData: Integer;
//-----------------------------------------------------------------------------
//  Reads evaporation data from a line of input.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  Result := 0;
  I := Uutils.FindKeyWord(TokList[0], EvapOptions, 4);
  if I >= 0 then Result := ReadEvaporationRates(I)

  else if (I <> TEMP_EVAP) and (Ntoks < 2)
  then  Result := ErrMsg(ITEMS_ERR, '')

  else with Project.Climatology do
  begin

    if SameText(TokList[0], 'RECOVERY')
    then RecoveryPat := TokList[1]

    else if SameText(TokList[0], 'DRY_ONLY') then
    begin
      if SameText(TokList[1], 'NO') then EvapDryOnly := False
      else if SameText(TokList[1], 'YES') then EvapDryOnly := True
      else Result := ErrMsg(KEYWORD_ERR, TokList[1]);
    end

    else Result := ErrMsg(KEYWORD_ERR, TokList[0]);

  end;
end;


function ReadSubcatchmentData: Integer;
//-----------------------------------------------------------------------------
//  Reads subcatchment data from a line of input.
//-----------------------------------------------------------------------------
var
  S  : TSubcatch;
  ID : String;
begin
  if Ntoks < 8
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin
    ID := TokList[0];
    S := TSubcatch.Create;
    SubcatchList.AddObject(ID, S);
    S.X := MISSING;
    S.Y := MISSING;
    S.OutFileIndex := -1;
    Uutils.CopyStringArray(Project.DefProp[SUBCATCH].Data, S.Data);
    S.Data[SUBCATCH_RAINGAGE_INDEX] := TokList[1];
    S.Data[SUBCATCH_OUTLET_INDEX]   := TokList[2];
    S.Data[SUBCATCH_AREA_INDEX]     := TokList[3];
    S.Data[SUBCATCH_IMPERV_INDEX]   := TokList[4];
    S.Data[SUBCATCH_WIDTH_INDEX]    := TokList[5];
    S.Data[SUBCATCH_SLOPE_INDEX]    := TokList[6];
    S.Data[SUBCATCH_CURBLENGTH_INDEX] := TokList[7];
    if Ntoks >= 9
      then S.Data[SUBCATCH_SNOWPACK_INDEX] := TokList[8];
    S.Data[COMMENT_INDEX ] := Comment;
    Project.Lists[SUBCATCH].AddObject(ID, S);
    Project.HasItems[SUBCATCH] := True;
    Result := 0;
  end;
end;


function ReadSubareaData: Integer;
//-----------------------------------------------------------------------------
//  Reads subcatchment sub-area data from a line of input.
//-----------------------------------------------------------------------------
var
  S  : TSubcatch;
  ID : String;
begin
  if Ntoks < 7
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin
    ID := TokList[0];
    S := FindSubcatch(ID);
    if S = nil
    then Result := ErrMsg(SUBCATCH_ERR, ID)
    else begin
      S.Data[SUBCATCH_IMPERV_N_INDEX]  := TokList[1];
      S.Data[SUBCATCH_PERV_N_INDEX]    := TokList[2];
      S.Data[SUBCATCH_IMPERV_DS_INDEX] := TokList[3];
      S.Data[SUBCATCH_PERV_DS_INDEX]   := TokList[4];
      S.Data[SUBCATCH_PCTZERO_INDEX]   := TokList[5];
      S.Data[SUBCATCH_ROUTE_TO_INDEX]  := TokList[6];
      if Ntoks >= 8
      then S.Data[SUBCATCH_PCT_ROUTED_INDEX] := TokList[7];
      Result := 0;
    end;
  end;
end;


function ReadInfiltrationData: Integer;
//-----------------------------------------------------------------------------
// Reads subcatchment infiltration data from a line of input.
//-----------------------------------------------------------------------------
var
  S     : TSubcatch;
  ID    : String;
  J     : Integer;
  Jmax  : Integer;
begin
  ID := TokList[0];
  S := FindSubcatch(ID);
  if S = nil
  then Result := ErrMsg(SUBCATCH_ERR, ID)
  else begin
    Jmax := Ntoks-1;
    J := Uutils.FindKeyWord(TokList[Jmax], InfilOptions, 10);
    if J >= 0 then
    begin
      S.Data[SUBCATCH_INFIL_INDEX] := InfilOptions[J];
      Dec(Jmax);
    end
    else S.Data[SUBCATCH_INFIL_INDEX] := Project.Options.Data[INFILTRATION_INDEX];
    if Jmax > MAXINFILPROPS then Jmax := MAXINFILPROPS;
    for J := 1 to Jmax do S.InfilData[J-1] := TokList[J];
    Result := 0;
  end;
end;


function ReadAquiferData: Integer;
//-----------------------------------------------------------------------------
//  Reads aquifer data from a line of input.
//-----------------------------------------------------------------------------
var
  ID : String;
  A  : TAquifer;
  I  : Integer;
begin
  if nToks < MAXAQUIFERPROPS then Result := ErrMsg(ITEMS_ERR, '')
  else begin
    ID := TokList[0];
    A := TAquifer.Create;
    Project.Lists[AQUIFER].AddObject(ID, A);
    Project.HasItems[AQUIFER] := True;
    for I := 0 to MAXAQUIFERPROPS-1 do A.Data[I] := TokList[I+1];
    if nToks >= MAXAQUIFERPROPS + 2
    then A.Data[MAXAQUIFERPROPS] := TokList[MAXAQUIFERPROPS+1]
    else A.Data[MAXAQUIFERPROPS] := '';
    Result := 0;
  end;
end;


function ReadGroundwaterData: Integer;
//-----------------------------------------------------------------------------
//  Reads subcatchment groundwater data from a line of input.
//-----------------------------------------------------------------------------
var
  S:  TSubcatch;
  ID: String;
  P:  String;
  J:  Integer;
  K:  Integer;

begin
  // Get subcatchment name
  ID := TokList[0];
  S := FindSubcatch(ID);
  if S = nil
  then Result := ErrMsg(SUBCATCH_ERR, ID)
  else
  begin

    // Line contains GW flow parameters
    if Ntoks < 10 then Result := ErrMsg(ITEMS_ERR, '')
    else begin
      S.Groundwater.Clear;

      // Read required parameters
      for J := 1 to 9 do S.Groundwater.Add(TokList[J]);

      // Read optional parameters
      for K := 10 to 13 do
      begin
        if Ntoks > K then
        begin
          P := TokList[K];
          if P = '*' then P := '';
          S.Groundwater.Add(P);
        end
        else S.Groundwater.Add('');
      end;
      S.Data[SUBCATCH_GWATER_INDEX] := 'YES';
      Result := 0;
    end;
  end;
end;

function ReadGroundwaterFlowEqn(Line: String): Integer;
//-----------------------------------------------------------------------------
//  Reads GW flow math expression from a line of input.
//-----------------------------------------------------------------------------
var
  S:  TSubcatch;
  N:  Integer;
begin
  // Check for enough tokens in line
  if Ntoks < 3 then Result := ErrMsg(ITEMS_ERR, '')
  // Get subcatchment object referred to by name
  else begin
    Result := 0;
    S := FindSubcatch(TokList[0]);
    if S = nil then Result := ErrMsg(SUBCATCH_ERR, TokList[0])
    else begin
      // Find position in Line where second token ends
      N := Pos(TokList[1], Line) + Length(TokList[1]);
      // Save remainder of line to correct type of GW flow equation
      if SameText(TokList[1], 'LATERAL') then
        S.GwLatFlowEqn := Trim(AnsiRightStr(Line, Length(Line)-N))
      else if SameText(TokList[1], 'DEEP') then
        S.GwDeepFlowEqn := Trim(AnsiRightStr(Line, Length(Line)-N))
      else Result := ErrMsg(KEYWORD_ERR, TokList[1]);
    end;
  end;
end;

function ReadSnowpackData: Integer;
//-----------------------------------------------------------------------------
//  Reads snowpack data from a line of input.
//-----------------------------------------------------------------------------
var
  ID : String;
  P  : TSnowpack;
  I  : Integer;
  J  : Integer;
  Index: Integer;
begin
  Result := 0;
  if Ntoks < 8
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin
    I := Uutils.FindKeyWord(TokList[1], SnowpackOptions, 7);
    if I < 0
    then Result := ErrMsg(KEYWORD_ERR, TokList[1])
    else begin

      // Check if snow pack ID is same as for previous line
      ID := TokList[0];
      if (ID = PrevID)
      then Index := PrevIndex
      else Index := Project.Lists[SNOWPACK].IndexOf(ID);

      // If starting input for a new snow pack then create it
      if Index < 0 then
      begin
        P := TSnowpack.Create;
        Project.Lists[SNOWPACK].AddObject(ID, P);
        Project.HasItems[SNOWPACK] := True;
        Index := Project.Lists[SNOWPACK].Count - 1;
        PrevID := ID;
        PrevIndex := Index;
      end
      else P := TSnowpack(Project.Lists[SNOWPACK].Objects[Index]);

      // Parse line depending on data type
      case I of

      // Plowable area
      0:  begin
            if Ntoks < 9 then Result := ErrMsg(ITEMS_ERR, '')
            else
            begin
              for J := 1 to 6 do P.Data[1][J] := TokList[J+1];
              P.FracPlowable := TokList[8];
            end;
          end;

      // Impervious or Pervious area
      1,
      2:  begin
            if Ntoks < 9 then Result := ErrMsg(ITEMS_ERR, '')
            else for J := 1 to 7 do P.Data[I+1][J] := TokList[J+1];
          end;

      // Plowing parameters
      3:  begin
            for J := 1 to 6 do P.Plowing[J] := TokList[J+1];
            if Ntoks >= 9 then P.Plowing[7] := TokList[8];
          end;
      end;
    end;
  end;
end;


function ReadJunctionData: Integer;
//-----------------------------------------------------------------------------
//  Reads junction data from a line of input.
//-----------------------------------------------------------------------------
var
  aNode: TNode;
  ID   : String;
begin
  if Ntoks < 2
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin
    ID := TokList[0];
    aNode := TNode.Create;
    NodeList.AddObject(ID, aNode);
    aNode.Ntype := JUNCTION;
    aNode.X := MISSING;
    aNode.Y := MISSING;
    aNode.OutFileIndex := -1;
    Uutils.CopyStringArray(Project.DefProp[JUNCTION].Data, aNode.Data);
    aNode.Data[NODE_INVERT_INDEX] := TokList[1];
    if Ntoks > 2 then  aNode.Data[JUNCTION_MAX_DEPTH_INDEX]       := TokList[2];
    if Ntoks > 3 then  aNode.Data[JUNCTION_INIT_DEPTH_INDEX]      := TokList[3];
    if Ntoks > 4 then  aNode.Data[JUNCTION_SURCHARGE_DEPTH_INDEX] := TokList[4];
    if Ntoks > 5 then  aNode.Data[JUNCTION_PONDED_AREA_INDEX]     := TokList[5];
    aNode.Data[COMMENT_INDEX ] := Comment;
    Project.Lists[JUNCTION].AddObject(ID, aNode);
    Project.HasItems[JUNCTION] := True;
    Result := 0;
  end;
end;


function ReadOutfallData: Integer;
//-----------------------------------------------------------------------------
//  Reads outfall data from a line of input.
//-----------------------------------------------------------------------------
var
  aNode: TNode;
  ID   : String;
  I    : Integer;
  N    : Integer;
begin
  if Ntoks < 3
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin
    N := 4;
    ID := TokList[0];
    aNode := TNode.Create;
    NodeList.AddObject(ID, aNode);
    aNode.Ntype := OUTFALL;
    aNode.X := MISSING;
    aNode.Y := MISSING;
    aNode.OutFileIndex := -1;
    Uutils.CopyStringArray(Project.DefProp[OUTFALL].Data, aNode.Data);
    aNode.Data[NODE_INVERT_INDEX] := TokList[1];
    I := Uutils.FindKeyWord(TokList[2], OutfallOptions, 4);
    if I < 0 then I := FREE_OUTFALL;
    if (I > NORMAL_OUTFALL) and (Ntoks >= 4) then
    begin
      case I of
      FIXED_OUTFALL:      aNode.Data[OUTFALL_FIXED_STAGE_INDEX] := TokList[3];
      TIDAL_OUTFALL:      aNode.Data[OUTFALL_TIDE_TABLE_INDEX] := TokList[3];
      TIMESERIES_OUTFALL: aNode.Data[OUTFALL_TIME_SERIES_INDEX] := TokList[3];
      end;
      N := 5;
      if Ntoks >= 5 then aNode.Data[OUTFALL_TIDE_GATE_INDEX] := TokList[4];
    end
    else if Ntoks >= 4 then aNode.Data[OUTFALL_TIDE_GATE_INDEX] := TokList[3];
    if Ntoks > N then aNode.Data[OUTFALL_ROUTETO_INDEX] := TokList[N];
    aNode.Data[OUTFALL_TYPE_INDEX] := OutfallOptions[I];
    aNode.Data[COMMENT_INDEX ] := Comment;
    Project.Lists[OUTFALL].AddObject(ID, aNode);
    Project.HasItems[OUTFALL] := True;
    Result := 0;
  end;
end;


function ReadStorageData: Integer;
//-----------------------------------------------------------------------------
//  Reads storage unit data from a line of input.
//-----------------------------------------------------------------------------
var
  aNode: TNode;
  ID   : String;
  N    : Integer;
  X    : Single;
begin
  Result := 0;
  N := 6;
  if Ntoks < 6
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin
    ID := TokList[0];
    aNode := TNode.Create;
    NodeList.AddObject(ID, aNode);
    aNode.Ntype := STORAGE;
    aNode.X := MISSING;
    aNode.Y := MISSING;
    aNode.OutFileIndex := -1;
    Uutils.CopyStringArray(Project.DefProp[STORAGE].Data, aNode.Data);
    Project.Lists[STORAGE].AddObject(ID, aNode);
    aNode.Data[NODE_INVERT_INDEX]        := TokList[1];
    aNode.Data[STORAGE_MAX_DEPTH_INDEX]  := TokList[2];
    aNode.Data[STORAGE_INIT_DEPTH_INDEX] := TokList[3];
    aNode.Data[STORAGE_GEOMETRY_INDEX]   := TokList[4];
    if CompareText(TokList[4], 'TABULAR') = 0 then
    begin
      aNode.Data[STORAGE_ATABLE_INDEX] := TokList[5];
    end
    else begin
      if Ntoks < 8
      then Result := ErrMsg(ITEMS_ERR, '')
      else begin
        aNode.Data[STORAGE_COEFF1_INDEX] := TokList[5];
        aNode.Data[STORAGE_COEFF2_INDEX] := TokList[6];
        aNode.Data[STORAGE_COEFF0_INDEX] := TokList[7];
        N := 8;
      end;
    end;

    // Optional items
    if (Result = 0) and (Ntoks > N) then
    begin
      // Surcharge Depth
      aNode.Data[STORAGE_SURCHARGE_DEPTH_INDEX] := TokList[N];
      // Evaporation factor
      if Ntoks > N+1 then aNode.Data[STORAGE_EVAP_FACTOR_INDEX] := TokList[N+1];
      // Constant seepage rate
      if Ntoks = N+3 then
      begin
        aNode.InfilData[STORAGE_KSAT_INDEX] := TokList[N+2];
      end
      // Green-Ampt seepage parameters
      else if Ntoks = N+5 then
      begin
        aNode.InfilData[STORAGE_SUCTION_INDEX] := TokList[N+2];
        aNode.InfilData[STORAGE_KSAT_INDEX] := TokList[N+3];
        aNode.InfilData[STORAGE_IMDMAX_INDEX] := TokList[N+4];
      end;
    end;
    Uutils.GetSingle(aNode.InfilData[STORAGE_KSAT_INDEX ], X);
    if (X > 0) then aNode.Data[STORAGE_SEEPAGE_INDEX] := 'YES'
    else aNode.Data[STORAGE_SEEPAGE_INDEX] := 'NO';
    aNode.Data[COMMENT_INDEX ] := Comment;
    Project.HasItems[STORAGE] := True;
  end;
end;


function ReadDividerData: Integer;
//-----------------------------------------------------------------------------
//  Reads flow divider data from a line of input.
//  (Corrected on 6/14/05)
//-----------------------------------------------------------------------------
var
  N, J : Integer;
  aNode: TNode;
  ID   : String;
begin
  if Ntoks < 4
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin
    Result := 0;
    ID := TokList[0];
    aNode := TNode.Create;
    NodeList.AddObject(ID, aNode);
    aNode.Ntype := DIVIDER;
    aNode.X := MISSING;
    aNode.Y := MISSING;
    aNode.OutFileIndex := -1;
    Uutils.CopyStringArray(Project.DefProp[DIVIDER].Data, aNode.Data);
    Project.Lists[DIVIDER].AddObject(ID, aNode);
    Project.HasItems[DIVIDER] := True;
    aNode.Data[COMMENT_INDEX ] := Comment;
    aNode.Data[NODE_INVERT_INDEX] := TokList[1];
    aNode.Data[DIVIDER_LINK_INDEX] := TokList[2];
    aNode.Data[DIVIDER_TYPE_INDEX] := TokList[3];
    N := 5;
    if SameText(TokList[3], 'OVERFLOW') then
    begin
      N := 4;
    end
    else if SameText(TokList[3], 'CUTOFF') then
    begin
      aNode.Data[DIVIDER_CUTOFF_INDEX] := TokList[4];
    end
    else if SameText(TokList[3], 'TABULAR') then
    begin
      aNode.Data[DIVIDER_TABLE_INDEX] := TokList[4];
    end
    else if SameText(TokList[3], 'WEIR') and (Ntoks >= 7) then
    begin
      aNode.Data[DIVIDER_QMIN_INDEX]   := TokList[4];
      aNode.Data[DIVIDER_DMAX_INDEX]   := TokList[5];
      aNode.Data[DIVIDER_QCOEFF_INDEX] := TokList[6];
      N := 7;
    end
    else Result := ErrMsg(KEYWORD_ERR, TokList[3]);
    if (Result = 0) and (Ntoks > N) then
    begin
      for J := N to Ntoks-1 do
        aNode.Data[DIVIDER_MAX_DEPTH_INDEX + J - N] := TokList[J];
    end;
  end;
end;


function ReadConduitData: Integer;
//-----------------------------------------------------------------------------
//  Reads conduit data from a line of input.
//-----------------------------------------------------------------------------
var
  aLink : TLink;
  aNode1: TNode;
  aNode2: TNode;
  ID    : String;
begin
  if Ntoks < 7 then Result := ErrMsg(ITEMS_ERR, '')
  else begin
    ID := TokList[0];
    aNode1 := FindNode(TokList[1]);
    aNode2 := FindNode(TokList[2]);
    if (aNode1 = nil) then Result := ErrMsg(NODE_ERR, TokList[1])
    else if (aNode2 = nil) then Result := ErrMsg(NODE_ERR, TokList[2])
    else begin
      aLink := TLink.Create;
      LinkList.AddObject(ID, aLink);
      aLink.Ltype := CONDUIT;
      aLink.Node1 := aNode1;
      aLink.Node2 := aNode2;
      aLink.OutFileIndex := -1;
      Uutils.CopyStringArray(Project.DefProp[CONDUIT].Data, aLink.Data);
      Project.Lists[CONDUIT].AddObject(ID, aLink);
      Project.HasItems[CONDUIT] := True;
      aLink.Data[CONDUIT_LENGTH_INDEX]     := TokList[3];
      aLink.Data[CONDUIT_ROUGHNESS_INDEX]  := TokList[4];
      aLink.Data[CONDUIT_INLET_HT_INDEX]   := TokList[5];
      aLink.Data[CONDUIT_OUTLET_HT_INDEX]  := TokList[6];
      if Ntoks > 7 then aLink.Data[CONDUIT_INIT_FLOW_INDEX] := TokList[7];
      if Ntoks > 8 then aLink.Data[CONDUIT_MAX_FLOW_INDEX] := TokList[8];
      aLink.Data[COMMENT_INDEX ] := Comment;
      Result := 0;
    end;
  end;
end;


function ReadPumpData: Integer;
//-----------------------------------------------------------------------------
//  Reads pump data from a line of input.
//-----------------------------------------------------------------------------
var
  aLink : TLink;
  aNode1: TNode;
  aNode2: TNode;
  ID    : String;
  N     : Integer;
begin
  if nToks < 4
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin
    ID := TokList[0];
    aNode1 := FindNode(TokList[1]);
    aNode2 := FindNode(TokList[2]);
    if (aNode1 = nil) then Result := ErrMsg(NODE_ERR, TokList[1])
    else if (aNode2 = nil) then Result := ErrMsg(NODE_ERR, TokList[2])
    else begin
      aLink := TLink.Create;
      LinkList.AddObject(ID, aLink);
      aLink.Ltype := PUMP;
      aLink.Node1 := aNode1;
      aLink.Node2 := aNode2;
      aLink.OutFileIndex := -1;
      Uutils.CopyStringArray(Project.DefProp[PUMP].Data, aLink.Data);
      Project.Lists[PUMP].AddObject(ID, aLink);
      Project.HasItems[PUMP] := True;

      // Skip over PumpType if line has old format
      if Uutils.FindKeyWord(TokList[3], PumpTypes, 5) >= 0 then N := 4
      else N := 3;
      if Ntoks <= N then Result := ErrMsg(ITEMS_ERR, '')
      else
      begin
        aLink.Data[PUMP_CURVE_INDEX] := TokList[N];
        if nToks > N+1 then aLink.Data[PUMP_STATUS_INDEX] := TokList[N+1];
        if nToks > N+2 then aLink.Data[PUMP_STARTUP_INDEX] := TokList[N+2];
        if nToks > N+3 then aLink.Data[PUMP_SHUTOFF_INDEX] := TokList[N+3];
        aLink.Data[COMMENT_INDEX ] := Comment;
        Result := 0;
      end;
    end;
  end;
end;


function ReadOrificeData: Integer;
//-----------------------------------------------------------------------------
//  Reads orifice data from a line of input.
//-----------------------------------------------------------------------------
var
  aLink : TLink;
  aNode1: TNode;
  aNode2: TNode;
  ID    : String;
begin
  if nToks < 6
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin
    ID := TokList[0];
    aNode1 := FindNode(TokList[1]);
    aNode2 := FindNode(TokList[2]);
    if (aNode1 = nil) then Result := ErrMsg(NODE_ERR, TokList[1])
    else if (aNode2 = nil) then Result := ErrMsg(NODE_ERR, TokList[2])
    else begin
      aLink := TLink.Create;
      LinkList.AddObject(ID, aLink);
      aLink.Ltype := ORIFICE;
      aLink.Node1 := aNode1;
      aLink.Node2 := aNode2;
      aLink.OutFileIndex := -1;
      Uutils.CopyStringArray(Project.DefProp[ORIFICE].Data, aLink.Data);
      Project.Lists[ORIFICE].AddObject(ID, aLink);
      Project.HasItems[ORIFICE] := True;
      aLink.Data[ORIFICE_TYPE_INDEX]      := TokList[3];
      aLink.Data[ORIFICE_BOTTOM_HT_INDEX] := TokList[4];
      aLink.Data[ORIFICE_COEFF_INDEX]     := TokList[5];
      if nToks >= 7
      then aLink.Data[ORIFICE_FLAPGATE_INDEX] := TokList[6];
      if nToks >= 8
      then aLink.Data[ORIFICE_ORATE_INDEX] := TokList[7];
      aLink.Data[COMMENT_INDEX ] := Comment;
      Result := 0;
    end;
  end;
end;


function ReadWeirData: Integer;
//-----------------------------------------------------------------------------
//  Reads weir data from a line of input.
//-----------------------------------------------------------------------------
var
  aLink : TLink;
  aNode1: TNode;
  aNode2: TNode;
  ID    : String;
begin
  if nToks < 6
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin
    ID := TokList[0];
    aNode1 := FindNode(TokList[1]);
    aNode2 := FindNode(TokList[2]);
    if (aNode1 = nil) then Result := ErrMsg(NODE_ERR, TokList[1])
    else if (aNode2 = nil) then Result := ErrMsg(NODE_ERR, TokList[2])
    else begin
      aLink := TLink.Create;
      LinkList.AddObject(ID, aLink);
      aLink.Ltype := WEIR;
      aLink.Node1 := aNode1;
      aLink.Node2 := aNode2;
      aLink.OutFileIndex := -1;
      Uutils.CopyStringArray(Project.DefProp[WEIR].Data, aLink.Data);
      Project.Lists[WEIR].AddObject(ID, aLink);
      Project.HasItems[WEIR] := True;
      aLink.Data[WEIR_TYPE_INDEX]  := TokList[3];
      aLink.Data[WEIR_CREST_INDEX] := TokList[4];
      aLink.Data[WEIR_COEFF_INDEX] := TokList[5];
      if (nToks >= 7) and not SameText(TokList[6], '*') then
        aLink.Data[WEIR_FLAPGATE_INDEX] := TokList[6];
      if (nToks >= 8) and not SameText(TokList[7], '*') then
        aLink.Data[WEIR_CONTRACT_INDEX] := TokList[7];
      if (nToks >= 9) and not SameText(TokList[8], '*') then
        aLink.Data[WEIR_END_COEFF_INDEX] := TokList[8];
      if (nToks >= 10) and not SameText(TokList[9], '*') then
        aLink.Data[WEIR_SURCHARGE_INDEX] := TokList[9];
      if (nToks >= 11) and not SameText(TokList[10], '*') then
        aLink.Data[WEIR_ROAD_WIDTH_INDEX] := TokList[10];
      if (nToks >= 12) and not SameText(TokList[11], '*') then
        aLink.Data[WEIR_ROAD_SURF_INDEX] := TokList[11];
      if (nToks >= 13) and not SameText(TokList[12], '*') then
      aLink.Data[WEIR_COEFF_CURVE_INDEX] := TokList[12];
      aLink.Data[COMMENT_INDEX] := Comment;
      Result := 0;
    end;
  end;
end;


function ReadOutletData: Integer;
//-----------------------------------------------------------------------------
//  Reads outlet data from a line of input.
//-----------------------------------------------------------------------------
var
  aLink : TLink;
  aNode1: TNode;
  aNode2: TNode;
  ID    : String;
  N     : Integer;
begin
  if nToks < 6
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin
    ID := TokList[0];
    aNode1 := FindNode(TokList[1]);
    aNode2 := FindNode(TokList[2]);
    if (aNode1 = nil) then Result := ErrMsg(NODE_ERR, TokList[1])
    else if (aNode2 = nil) then Result := ErrMsg(NODE_ERR, TokList[2])
    else begin
      aLink := TLink.Create;
      LinkList.AddObject(ID, aLink);
      aLink.Ltype := OUTLET;
      aLink.Node1 := aNode1;
      aLink.Node2 := aNode2;
      aLink.OutFileIndex := -1;
      Uutils.CopyStringArray(Project.DefProp[OUTLET].Data, aLink.Data);
      Project.Lists[OUTLET].AddObject(ID, aLink);
      Project.HasItems[OUTLET] := True;
      aLink.Data[OUTLET_CREST_INDEX] := TokList[3];

      //... added for backwards compatibility
      if SameText(TokList[4], 'TABULAR') then TokList[4] := 'TABULAR/DEPTH';
      if SameText(TokList[4], 'FUNCTIONAL') then TokList[4] := 'FUNCTIONAL/DEPTH';
      aLink.Data[OUTLET_TYPE_INDEX]  := TokList[4];

      if AnsiContainsText(TokList[4], 'TABULAR') then
      begin
        aLink.Data[OUTLET_QTABLE_INDEX] := TokList[5];
        N := 6;
      end
      else begin
        if Ntoks < 7 then
        begin
          Result := ErrMsg(ITEMS_ERR, '');
          Exit;
        end
        else
        begin
          aLink.Data[OUTLET_QCOEFF_INDEX] := TokList[5];
          aLink.Data[OUTLET_QEXPON_INDEX] := TokList[6];
          N := 7;
        end;
      end;
      if Ntoks > N then aLink.Data[OUTLET_FLAPGATE_INDEX] := TokList[N];
      aLink.Data[COMMENT_INDEX ] := Comment;
      Result := 0;
    end;
  end;
end;


function GetXsectShape(const S: String): Integer;
//-----------------------------------------------------------------------------
//  Finds the code number corresponding to cross section shape S.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  for I := 0 to High(Dxsect.XsectShapes) do
  begin
    if CompareText(S, Dxsect.XsectShapes[I].Text[1]) = 0 then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;

procedure CheckForStdSize;
//-----------------------------------------------------------------------------
//  Converts from old format for standard size ellipse and arch pipes
//  to new format.
//-----------------------------------------------------------------------------
var
  J: Integer;
  X: Extended;
begin
// Old format had size code as 3rd token and 0 for 4th token
  J := StrToIntDef(TokList[2], 0);
  Uutils.GetExtended(TokList[3], X);
  if (J > 0) and (X = 0) then
  begin
  // New format has 5th token as size code
    TokList[4] := TokList[2];
    TokList[2] := '0';
    TokList[3] := '0';
  end;
end;

function ReadXsectionData: Integer;
//-----------------------------------------------------------------------------
//  Reads cross section data froma line of input.
//-----------------------------------------------------------------------------
var
  I     : Integer;
  ID    : String;
  aLink : TLink;
begin
  if nToks < 3
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin
    ID := TokList[0];
    aLink := FindLink(ID);
    if (aLink = nil)
    then Result := ErrMsg(LINK_ERR, TokList[0])
    else begin
      I := GetXsectShape(TokList[1]);
      if I < 0 then Result := ErrMsg(KEYWORD_ERR, TokList[1])
      else case aLink.Ltype of

      CONDUIT:
        begin
          aLink.Data[CONDUIT_SHAPE_INDEX] := TokList[1];
          if (I = Dxsect.IRREG_SHAPE_INDEX) or
             (I = Dxsect.STREET_SHAPE_INDEX) then
          begin
            aLink.Data[CONDUIT_TSECT_INDEX] := TokList[2];
            aLink.Data[CONDUIT_GEOM1_INDEX] := '';
            Result := 0;
          end

          else begin
            if nToks < 6 then Result := ErrMsg(ITEMS_ERR, '')
            else begin
              aLink.Data[CONDUIT_GEOM1_INDEX] := TokList[2];
              aLink.Data[CONDUIT_GEOM2_INDEX] := TokList[3];
              aLink.Data[CONDUIT_GEOM3_INDEX] := TokList[4];
              aLink.Data[CONDUIT_GEOM4_INDEX] := TokList[5];
              if Ntoks > 6 then aLink.Data[CONDUIT_BARRELS_INDEX] := TokList[6];
              if Ntoks > 7 then aLink.Data[CONDUIT_CULVERT_INDEX] := TokList[7];
              if I = Dxsect.CUSTOM_SHAPE_INDEX then
                aLink.Data[CONDUIT_TSECT_INDEX] := TokList[3];
              Result := 0;
            end;
          end;
        end;

      ORIFICE:
        begin
          if not I in [0, 1] then Result := ErrMsg(XSECT_ERR, TokList[0])
          else begin
            aLink.Data[ORIFICE_SHAPE_INDEX]  := TokList[1];
            aLink.Data[ORIFICE_HEIGHT_INDEX] := TokList[2];
            aLink.Data[ORIFICE_WIDTH_INDEX]  := TokList[3];
            Result := 0;
          end;
        end;

      WEIR:
        begin
          if not I in [2, 3, 4] then Result := ErrMsg(XSECT_ERR, TokList[0])
          else begin
            aLink.Data[WEIR_SHAPE_INDEX]  := TokList[1];
            aLink.Data[WEIR_HEIGHT_INDEX] := TokList[2];
            aLink.Data[WEIR_WIDTH_INDEX]  := TokList[3];
            aLink.Data[WEIR_SLOPE_INDEX]  := TokList[4];
            Result := 0;
          end;
        end;

      else Result := 0;
      end;
    end;
  end;
end;

function ReadStreetData: Integer;
//-----------------------------------------------------------------------------
//  Reads street section data from a line of input.
//-----------------------------------------------------------------------------
var
  ID: String;
  Index: Integer;
  aStreet: TStreet;
  J: Integer;
begin
  if nToks < 8
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin
    ID := TokList[0];
    Index := Project.Lists[STREET].IndexOf(ID);
    if Index < 0 then
    begin
      aStreet := TStreet.Create;
      Project.Lists[STREET].AddObject(ID, aStreet);
      Project.HasItems[STREET] := True;
    end
    else aStreet := TStreet(Project.Lists[STREET].Objects[Index]);
    for J := 1 to nToks - 1 do
      aStreet.Data[J-1] := TokList[J];
    aStreet.SetMaxDepth;
    Result := 0;
  end;
end;

function ReadTransectData: Integer;
//-----------------------------------------------------------------------------
//  Reads transect data from a line of input.
//-----------------------------------------------------------------------------
var
  I     : Integer;
  K     : Integer;
  N     : Integer;
  ID    : String;
  Tsect : TTransect;
begin
  Result := 0;
  if SameText(TokList[0], 'NC') then
  begin
    if nToks < 4 then Result := ErrMsg(ITEMS_ERR, '')
    else for I := 1 to 3 do ManningsN[I] := TokList[I];
    TsectComment := Comment;
    Exit;
  end;

  if SameText(TokList[0], 'X1') then
  begin
    if nToks < 2 then Exit;
    ID := TokList[1];
    Tsect := TTransect.Create;

    if Length(Comment) > 0 then TsectComment := Comment;
    Tsect.Comment := TsectComment;

    Project.Lists[TRANSECT].AddObject(ID, Tsect);
    Project.HasItems[TRANSECT] := True;
    Tsect.Data[TRANSECT_N_LEFT]    := ManningsN[1];
    Tsect.Data[TRANSECT_N_RIGHT]   := ManningsN[2];
    Tsect.Data[TRANSECT_N_CHANNEL] := ManningsN[3];
    if nToks < 10 then Result := ErrMsg(ITEMS_ERR, '')
    else
    begin
      Tsect.Data[TRANSECT_X_LEFT] := TokList[3];
      Tsect.Data[TRANSECT_X_RIGHT] := TokList[4];
      Tsect.Data[TRANSECT_L_FACTOR] := TokList[7];
      Tsect.Data[TRANSECT_X_FACTOR] := TokList[8];
      Tsect.Data[TRANSECT_Y_FACTOR] := TokList[9];
    end;
    Exit;
  end;

  if SameText(TokList[0], 'GR') then
  begin
    N := Project.Lists[TRANSECT].Count;
    if N = 0 then Result := ErrMsg(TRANSECT_ERR, '')
    else begin
      Tsect := TTransect(Project.Lists[TRANSECT].Objects[N-1]);
      K := 1;
      while K + 1 < Ntoks do
      begin
        Tsect.Ydata.Add(TokList[K]);
        Tsect.Xdata.Add(TokList[K+1]);
        K := K + 2;
      end;
    end;
    Exit;
  end;
end;


function ReadLossData: Integer;
//-----------------------------------------------------------------------------
//  Reads conduit loss data from a line of input.
//-----------------------------------------------------------------------------
var
  ID    : String;
  aLink : TLink;
begin
  if nToks < 4
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin
    ID := TokList[0];
    aLink := FindLink(ID);
    if (aLink = nil) then Result := ErrMsg(LINK_ERR, ID)
    else if (aLink.Ltype <> CONDUIT) then Result := 0
    else begin
      aLink.Data[CONDUIT_ENTRY_LOSS_INDEX] := TokList[1];
      aLink.Data[CONDUIT_EXIT_LOSS_INDEX] := TokList[2];
      aLink.Data[CONDUIT_AVG_LOSS_INDEX] := TokList[3];
      if nToks >= 5
      then aLink.Data[CONDUIT_CHECK_VALVE_INDEX] := TokList[4];
      if nToks >= 6
      then aLink.Data[CONDUIT_SEEPAGE_INDEX] := TokList[5];
      Result := 0;
    end;
  end;
end;


function ReadPollutantData: Integer;
//-----------------------------------------------------------------------------
//  Reads pollutant data from a line of input.
//-----------------------------------------------------------------------------
var
  ID      : String;
  aPollut : TPollutant;
  X       : Single;
begin
  if nToks < 5
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin

    // Add new pollutant to project
    ID := TokList[0];
    aPollut := TPollutant.Create;
    Uutils.CopyStringArray(Project.DefProp[POLLUTANT].Data, aPollut.Data);
    Project.Lists[POLLUTANT].AddObject(ID, aPollut);
    Project.HasItems[POLLUTANT] := True;

    // Parse units & concens.
    aPollut.Data[POLLUT_UNITS_INDEX] := TokList[1];
    aPollut.Data[POLLUT_RAIN_INDEX]  := TokList[2];
    aPollut.Data[POLLUT_GW_INDEX]    := TokList[3];

    // This is for old format
    if (Ntoks = 5)
    or ( (Ntoks = 7) and Uutils.GetSingle(TokList[6], X) ) then
    begin
      aPollut.Data[POLLUT_DECAY_INDEX] := TokList[4];
      if nToks >= 7 then
      begin
        aPollut.Data[POLLUT_COPOLLUT_INDEX] := TokList[5];
        aPollut.Data[POLLUT_FRACTION_INDEX] := TokList[6];
      end;
    end

    // This is for new format
    else
    begin
      aPollut.Data[POLLUT_II_INDEX] := TokList[4];
      if Ntoks >= 6 then aPollut.Data[POLLUT_DECAY_INDEX] := TokList[5];
      if nToks >= 7 then aPollut.Data[POLLUT_SNOW_INDEX]  := TokList[6];
      if Ntoks >= 9 then
      begin
        aPollut.Data[POLLUT_COPOLLUT_INDEX] := TokList[7];
        aPollut.Data[POLLUT_FRACTION_INDEX] := TokList[8];
      end;
      if Ntoks >= 10 then aPollut.Data[POLLUT_DWF_INDEX] := TokList[9];
      if Ntoks >= 11 then aPollut.Data[POLLUT_INIT_INDEX] := TokList[10];
    end;
    Result := 0;
  end;
end;


function ReadLanduseData: Integer;
//-----------------------------------------------------------------------------
//  Reads land use data from a line of input.
//-----------------------------------------------------------------------------
var
  ID           : String;
  aLanduse     : TLanduse;
  aNonPtSource : TNonpointSource;
  J            : Integer;
begin
    ID := TokList[0];
    aLanduse := TLanduse.Create;
    for J := 0 to Project.Lists[POLLUTANT].Count - 1 do
    begin
      aNonPtSource := TNonpointSource.Create;
      aLanduse.NonpointSources.AddObject(Project.Lists[POLLUTANT].Strings[J],
        aNonPtSource);
    end;
    Project.Lists[LANDUSE].AddObject(ID, aLanduse);
    Project.HasItems[LANDUSE] := True;
    if Ntoks > 1 then aLanduse.Data[LANDUSE_CLEANING_INDEX]  := TokList[1];
    if Ntoks > 2 then aLanduse.Data[LANDUSE_AVAILABLE_INDEX] := TokList[2];
    if Ntoks > 3 then aLanduse.Data[LANDUSE_LASTCLEAN_INDEX] := TokList[3];
    aLanduse.Data[COMMENT_INDEX ] := Comment;
    Result := 0;
end;


function ReadBuildupData: Integer;
//-----------------------------------------------------------------------------
//  Reads pollutant buildup function data from a line of input.
//-----------------------------------------------------------------------------
var
  LanduseIndex   : Integer;
  PollutIndex    : Integer;
  aLanduse       : TLanduse;
  aNonpointSource: TNonpointSource;
  J              : Integer;
begin
  if nToks < 7
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin
    LanduseIndex := Project.Lists[LANDUSE].IndexOf(TokList[0]);
    PollutIndex := Project.Lists[POLLUTANT].IndexOf(TokList[1]);
    if (LanduseIndex < 0) then Result := ErrMsg(LANDUSE_ERR, TokList[0])
    else if (PollutIndex < 0) then Result := ErrMsg(POLLUT_ERR, TokList[1])
    else begin
      aLanduse := TLanduse(Project.Lists[LANDUSE].Objects[LanduseIndex]);
      aNonpointSource :=
        TNonpointSource(aLanduse.NonpointSources.Objects[PollutIndex]);
      for J := 2 to 6 do
        aNonpointSource.BuildupData[J-2] := TokList[J];
      Result := 0;
    end;
  end;
end;


function ReadWashoffData: Integer;
//-----------------------------------------------------------------------------
//  Reads pollutant washoff function data from a line of input.
//-----------------------------------------------------------------------------
var
  LanduseIndex   : Integer;
  PollutIndex    : Integer;
  aLanduse       : TLanduse;
  aNonpointSource: TNonpointSource;
  J              : Integer;
begin
  if nToks < 7
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin
    LanduseIndex := Project.Lists[LANDUSE].IndexOf(TokList[0]);
    PollutIndex := Project.Lists[POLLUTANT].IndexOf(TokList[1]);
    if (LanduseIndex < 0) then Result := ErrMsg(LANDUSE_ERR, TokList[0])
    else if (PollutIndex < 0) then Result := ErrMsg(POLLUT_ERR, TokList[1])
    else begin
      aLanduse := TLanduse(Project.Lists[LANDUSE].Objects[LanduseIndex]);
      aNonpointSource :=
        TNonpointSource(aLanduse.NonpointSources.Objects[PollutIndex]);
      for J := 2 to 6 do
        aNonpointSource.WashoffData[J-2] := TokList[J];
      Result := 0;
    end;
  end;
end;


function ReadCoverageData: Integer;
//-----------------------------------------------------------------------------
//  Reads land use coverage data from a line of input.
//-----------------------------------------------------------------------------
var
  MaxToks: Integer;
  X      : Single;
  S      : TSubcatch;
  S1     : String;
  I      : Integer;
begin
  Result := 0;
  S := FindSubcatch(TokList[0]);
  if S = nil
  then Result := ErrMsg(SUBCATCH_ERR, TokList[0])
  else begin
    MaxToks := 3;
    while (MaxToks <= nToks) do
    begin
      if not Uutils.GetSingle(TokList[MaxToks-1], X) then
      begin
        Result := ErrMsg(NUMBER_ERR, TokList[MaxToks-1]);
        break;
      end;
      S1 := TokList[MaxToks-2];
      I := S.LandUses.IndexOfName(S1);
      S1 := TokList[MaxToks-2] + '=' + TokList[MaxToks-1];
      if I < 0
      then S.LandUses.Add(S1)
      else S.Landuses[I] := S1;
      MaxToks := MaxToks + 2;
    end;
    S.Data[SUBCATCH_LANDUSE_INDEX] := IntToStr(S.LandUses.Count);
  end;
end;


function ReadTreatmentData(Line: String): Integer;
//-----------------------------------------------------------------------------
//  Reads pollutant treatment function from a line of input.
//-----------------------------------------------------------------------------
var
  S     : String;
  aNode : TNode;
  I     : Integer;
begin
  if Ntoks < 3
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin
    aNode := FindNode(TokList[0]);
    if (aNode = nil) then Result := ErrMsg(NODE_ERR, TokList[0])
    else if Project.Lists[POLLUTANT].IndexOf(TokList[1]) < 0 then
      Result := ErrMsg(POLLUT_ERR, TokList[1])
    else begin
      I := aNode.Treatment.IndexOfName(TokList[1]);
      S := Copy(Line, Pos(TokList[1], Line)+Length(TokList[1]), Length(Line));
      S := TokList[1] + '=' + Trim(S);
      if I < 0 then
        aNode.Treatment.Add(S)
      else
        aNode.Treatment[I] := S;
      aNode.Data[NODE_TREAT_INDEX] := 'YES';
      Result := 0;
    end;
  end;
end;


function ReadExInflowData: Integer;
//-----------------------------------------------------------------------------
//  Reads external inflow data from a line of input.
//-----------------------------------------------------------------------------
var
  S     : array[1..7] of String;
  Inflow: String;
  I     : Integer;
  aNode : TNode;
begin

  if Ntoks < 3
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin
    aNode := FindNode(TokList[0]);
    if (aNode = nil) then Result := ErrMsg(NODE_ERR, TokList[0])
    else begin
      S[1] := TokList[1];                // Constituent name
      S[2] := TokList[2];                // Time Series name
      S[3] := 'FLOW';
      S[4] := '1.0';
      if not SameText(S[1], 'FLOW') then
      begin
        if nToks >= 4 then S[3] := TokList[3] else S[3] := 'CONCEN';
        if nToks >= 5 then S[4] := TokList[4] else S[4] := '1.0';
      end;
      if nToks >= 6 then S[5] := TokList[5] else S[5] := '1.0';
      if nToks >= 7 then S[6] := TokList[6] else S[6] := '';
      if nToks >= 8 then S[7] := TokList[7] else S[7] := '';
      Inflow := S[1] + '=' + S[2] + #13 + S[3] + #13 + S[4] + #13 +
                             S[5] + #13 + S[6] + #13 + S[7];
      I := aNode.DXInflow.IndexOfName(S[1]);
      if I < 0 then aNode.DXInflow.Add(Inflow)
      else aNode.DXInflow[I] := Inflow;
      aNode.Data[NODE_INFLOWS_INDEX] := 'YES';
      Result := 0;
    end;
  end;
end;


function ReadDWInflowData: Integer;
//-----------------------------------------------------------------------------
//  Reads dry weather inflow data from a line of input.
//-----------------------------------------------------------------------------
var
  M    : Integer;
  S    : String;
  S1   : String;
  I    : Integer;
  aNode: TNode;
begin
  if Ntoks < 3
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin
    aNode := FindNode(TokList[0]);
    if (aNode = nil) then Result := ErrMsg(NODE_ERR, TokList[0])
    else begin
      S1 := TokList[1];
      S := S1 + '=' + TokList[2];
      for M := 3 to Ntoks-1 do S := S + #13 + TokList[M];
      I := aNode.DWInflow.IndexOfName(S1);
      if I < 0
      then aNode.DWInflow.Add(S)
      else aNode.DWInflow[I] := S;
      aNode.Data[NODE_INFLOWS_INDEX] := 'YES';
      Result := 0;
    end;
  end;
end;


function ReadPatternData: Integer;
//-----------------------------------------------------------------------------
//  Reads time pattern data from a line of input.
//-----------------------------------------------------------------------------
var
  ID: String;
  Index: Integer;
  aPattern: TPattern;
  PatType: Integer;
  J: Integer;
begin
  if nToks < 2
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin
    J := 1;
    ID := TokList[0];
    Index := Project.Lists[PATTERN].IndexOf(ID);
    if Index < 0 then
    begin
      aPattern := TPattern.Create;
      aPattern.Comment := Comment;
      Project.Lists[PATTERN].AddObject(ID, aPattern);
      Project.HasItems[PATTERN] := True;
      PatType := Uutils.FindKeyWord(TokList[1], PatternTypes, 7);
      if PatType < 0 then
      begin
        Result := ErrMsg(KEYWORD_ERR, TokList[1]);
        exit;
      end;
      aPattern.PatternType := PatType;
      J := 2;
    end
    else aPattern := TPattern(Project.Lists[PATTERN].Objects[Index]);
    while (J < nToks) and (aPattern.Count <= High(aPattern.Data)) do
    begin
      aPattern.Data[aPattern.Count] := TokList[J];
      Inc(J);
      Inc(aPattern.Count);
    end;
    Result := 0;
  end;
end;


function ReadIIInflowData: Integer;
//-----------------------------------------------------------------------------
//  Reads RDII inflow data from a line of input.
//-----------------------------------------------------------------------------
var
  aNode: TNode;

begin
  if Ntoks < 3
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin
    aNode := FindNode(TokList[0]);
    if (aNode = nil) then Result := ErrMsg(NODE_ERR, TokList[0])
    else begin
      aNode.IIInflow.Clear;
      aNode.IIInflow.Add(TokList[1]);
      aNode.IIInflow.Add(TokList[2]);
      aNode.Data[NODE_INFLOWS_INDEX] := 'YES';
      Result := 0;
    end;
  end;
end;


function ReadLoadData: Integer;
//-----------------------------------------------------------------------------
//  Reads initial pollutant loading data from a line of input.
//-----------------------------------------------------------------------------
var
  X  : Single;
  S  : TSubcatch;
  S1 : String;
  I  : Integer;

begin
  S := FindSubcatch(TokList[0]);
  if S = nil
  then Result := ErrMsg(SUBCATCH_ERR, TokList[0])
  else if Project.Lists[POLLUTANT].IndexOf(TokList[1]) < 0
  then Result := ErrMsg(POLLUT_ERR, TokList[1])
  else if not Uutils.GetSingle(TokList[2], X)
  then Result := ErrMsg(NUMBER_ERR, TokList[2])
  else
  begin
      S1 := TokList[1] + '=' + TokList[2];
      I := S.Loadings.IndexOfName(TokList[1]);
      if I < 0
      then S.Loadings.Add(S1)
      else S.Loadings[I] := S1;
      S.Data[SUBCATCH_LOADING_INDEX] := 'YES';
      Result := 0;
  end;
end;


function ReadCurveData: Integer;
//-----------------------------------------------------------------------------
//  Reads curve data from a line of input.
//-----------------------------------------------------------------------------
var
  Index   : Integer;
  K       : Integer;
  L       : Integer;
  M       : Integer;
  ObjType : Integer;
  ID      : String;
  aCurve  : TCurve;
begin
  // Check for too few tokens
  if Ntoks < 2
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin

    // Check if curve ID is same as for previous line
    ID := TokList[0];
    if (ID = PrevID) then
    begin
      Index := PrevIndex;
      ObjType := CurveType;
    end
    else Project.FindCurve(ID, ObjType, Index);

    // Create new curve if ID not in data base
    K := 2;
    if Index < 0 then
    begin

      // Check for valid curve type keyword
      M := -1;
      for L := 0 to High(CurveTypeOptions) do
      begin
        if SameText(TokList[1], CurveTypeOptions[L]) then
        begin
          M := L;
          break;
        end;
      end;
      if M < 0 then
      begin
        Result := ErrMsg(KEYWORD_ERR, TokList[1]);
        Exit;
      end;

      // Convert curve type keyword index to a curve object category
      case M of
      0: ObjType := CONTROLCURVE;
      1: ObjType := DIVERSIONCURVE;
      2..6:
         ObjType := PUMPCURVE;
      7: ObjType := RATINGCURVE;
      8: ObjType := SHAPECURVE;
      9: ObjType := STORAGECURVE;
      10: ObjType := TIDALCURVE;
      11: ObjType := WEIRCURVE;
      end;

      // Create a new curve object
      aCurve := TCurve.Create;
      aCurve.Comment := Comment;
      aCurve.CurveType := TokList[1];
      if ObjType = PUMPCURVE
      then aCurve.CurveCode := M - 1
      else aCurve.CurveCode := 0;
      Project.Lists[ObjType].AddObject(ID, aCurve);
      Project.HasItems[ObjType] := True;
      Index := Project.Lists[ObjType].Count - 1;
      PrevID := ID;
      PrevIndex := Index;
      CurveType := ObjType;
      K := 3;
    end;

    // Add x,y values to the list maintained by the curve
    aCurve := TCurve(Project.Lists[ObjType].Objects[Index]);
    while K <= nToks-1 do
    begin
      aCurve.Xdata.Add(TokList[K-1]);
      aCurve.Ydata.Add(TokList[K]);
      K := K + 2;
    end;
    Result := 0;
  end;
end;


function ReadTimeseriesData: Integer;
//-----------------------------------------------------------------------------
//  Reads time series data from a line of input.
//-----------------------------------------------------------------------------
const
  NEEDS_DATE = 1;
  NEEDS_TIME = 2;
  NEEDS_VALUE = 3;
var
  Index     : Integer;
  State     : Integer;
  K         : Integer;
  ID        : String;
  StrDate   : String;
  aTseries  : TTimeseries;
begin
  // Check for too few tokens
  Result := -1;
  if Ntoks < 3 then Result := ErrMsg(ITEMS_ERR, '');

  // Check if series ID is same as for previous line
  ID := TokList[0];
  if (ID = PrevID) then Index := PrevIndex
  else Index := Project.Lists[TIMESERIES].IndexOf(ID);

  // If starting input for a new series then create it
  if Index < 0 then
  begin
    aTseries := TTimeseries.Create;
    aTseries.Comment := Comment;
    Project.Lists[TIMESERIES].AddObject(ID,aTseries);
    Project.HasItems[TIMESERIES] := True;
    Index := Project.Lists[TIMESERIES].Count - 1;
    PrevID := ID;
    PrevIndex := Index;
  end;
  aTseries := TTimeseries(Project.Lists[TIMESERIES].Objects[Index]);

  // Check if external file name used
  if SameText(TokList[1], 'FILE') then
  begin
    aTseries.Filename := FullPathname(TokList[2]);
    Result := 0;
    Exit;
  end;

  // Add values to the list maintained by the timeseries
  State := NEEDS_DATE;
  K := 1;
  while K < nToks do
  begin
    case State of

    NEEDS_DATE:
      begin
        try
          StrDate := Uutils.ConvertDate(TokList[K]);
          StrToDate(StrDate, MyFormatSettings);
          aTseries.Dates.Add(StrDate);
          Inc(K);
          if K >= nToks then break;
        except
          On EconvertError do aTseries.Dates.Add('');
        end;
        State := NEEDS_TIME;
      end;

    NEEDS_TIME:
      begin
        aTseries.Times.Add(TokList[K]);
        Inc(K);
        if K >= nToks then break;
        State := NEEDS_VALUE;
      end;

    NEEDS_VALUE:
      begin
        aTseries.Values.Add(TokList[K]);
        Result := 0;
        State := NEEDS_DATE;
        Inc(K);
      end;
    end;
  end;
  if Result = -1 then Result := ErrMsg(ITEMS_ERR, '');
end;


function ReadControlData(Line: String): Integer;
//-----------------------------------------------------------------------------
//  Reads a control rule statement from line of input.
//-----------------------------------------------------------------------------
begin
  Project.ControlRules.Add(Line);
  Result := 0;
end;


function ReadLidUsageData: Integer;
//-----------------------------------------------------------------------------
//  Reads LID usage data from line of input.
//-----------------------------------------------------------------------------
var
  aSubcatch: TSubcatch;
begin
  aSubcatch := FindSubcatch(TokList[0]);
  if aSubcatch = nil then Result := ErrMsg(SUBCATCH_ERR, TokList[0])
  else Result := Ulid.ReadLIDUsageData(aSubcatch, TokList, Ntoks);
end;


procedure ReadReportOption(Index: Integer);
begin
  if (Ntoks >= 2) and SameText(TokList[1], 'YES') then
    Project.Options.Data[Index] := 'YES'
  else
    Project.Options.Data[Index] := 'NO';
end;


function ReadReportData: Integer;
//-----------------------------------------------------------------------------
//  Reads reporting options from a line of input.
//-----------------------------------------------------------------------------
begin
  ReportingForm.Import(TokList, Ntoks);
  Result := 0;
end;


function ReadFileData: Integer;
//-----------------------------------------------------------------------------
//  Reads interface file usage from a line of input.
//-----------------------------------------------------------------------------
var
  Fname: String;
begin
  if Ntoks < 3
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin
    Fname := FullPathName(TokList[2]);
    Project.IfaceFiles.Add(TokList[0] + ' ' + TokList[1] + ' ' +
     '"' + Fname + '"');
    Result := 0;
  end;
end;


function ReadAdjustmentData: Integer;
//-----------------------------------------------------------------------------
//  Reads climate adjustments from a line of input.
//-----------------------------------------------------------------------------
var
  I: Integer;
  aSubcatch: TSubcatch;
begin
  Result := 0;
  if Ntoks < 2 then exit;
  if SameText(TokList[0], 'Temperature') then
  begin
    if Ntoks < 13 then Result := ErrMsg(ITEMS_ERR, '')
    else with Project.Climatology do
    begin
      for I := 0 to 11 do
        if StrToFloatDef(TokList[I+1], 0.0) = 0.0
        then TempAdjust[I] := ''
        else TempAdjust[I] := TokList[I+1];
    end;
  end
  else if SameText(TokList[0], 'Evaporation') then
  begin
    if Ntoks < 13 then Result := ErrMsg(ITEMS_ERR, '')
    else with Project.Climatology do
    begin
      for I := 0 to 11 do
        if StrToFloatDef(TokList[I+1], 0.0) = 0.0
        then EvapAdjust[I] := ''
        else EvapAdjust[I] := TokList[I+1];
    end;
  end
  else if SameText(TokList[0], 'Rainfall') then
  begin
    if Ntoks < 13 then Result := ErrMsg(ITEMS_ERR, '')
    else with Project.Climatology do
    begin
      for I := 0 to 11 do
        if StrToFloatDef(TokList[I+1], 1.0) = 1.0
        then RainAdjust[I] := ''
        else RainAdjust[I] := TokList[I+1];
    end;
  end

  else if SameText(TokList[0], 'Conductivity') then
  begin
    if Ntoks < 13 then Result := ErrMsg(ITEMS_ERR, '')
    else with Project.Climatology do
    begin
      for I := 0 to 11 do
        if StrToFloatDef(TokList[I+1], 1.0) = 1.0
        then CondAdjust[I] := ''
        else CondAdjust[I] := TokList[I+1];
    end;
  end

  else if (SameText(TokList[0], 'N-PERV')) and (Ntoks > 2) then
  begin
    aSubcatch := FindSubcatch(TokList[1]);
    if aSubcatch = nil then Result := ErrMsg(SUBCATCH_ERR, TokList[1])
    else aSubcatch.Data[SUBCATCH_N_PERV_PAT_INDEX] := TokList[2];
  end

  else if (SameText(TokList[0], 'DSTORE')) and (Ntoks > 2) then
  begin
    aSubcatch := FindSubcatch(TokList[1]);
    if aSubcatch = nil then Result := ErrMsg(SUBCATCH_ERR, TokList[1])
    else aSubcatch.Data[SUBCATCH_DS_PAT_INDEX] := TokList[2];
  end

  else if (SameText(TokList[0], 'INFIL')) and (Ntoks > 2) then
  begin
    aSubcatch := FindSubcatch(TokList[1]);
    if aSubcatch = nil then Result := ErrMsg(SUBCATCH_ERR, TokList[1])
    else aSubcatch.Data[SUBCATCH_INFIL_PAT_INDEX] := TokList[2];
  end;
end;


function ReadEventData(Line: String): Integer;
//-----------------------------------------------------------------------------
//  Reads hydraulic event data from a line of input.
//-----------------------------------------------------------------------------
begin
  Result := 0;
  if Length(Line) = 0 then exit;
  if LeftStr(Line,2) = ';;' then exit;
  Project.Events.Add(Line);
end;


function ReadOptionData: Integer;
//-----------------------------------------------------------------------------
//  Reads an analysis option from a line of input.
//-----------------------------------------------------------------------------
var
  Index : Integer;
  Keyword : String;
  S : String;
  S2: String;
  I : Integer;
  X : Single;
  T : Extended;
begin
  // Check which keyword applies
  Result := 0;
  if Ntoks < 2 then exit;
  Keyword := TokList[0];
  if SameText(Keyword, 'TEMPDIR') then exit;
  Index := Uutils.FindKeyWord(Keyword, OptionLabels, 17);
  case Index of

    -1:  Result := ErrMsg(KEYWORD_ERR, Keyword);

    ROUTING_MODEL_INDEX:
    begin
      if SameText(TokList[1], 'NONE') then
      begin
         Project.Options.Data[IGNORE_ROUTING_INDEX] := 'YES';
         Exit;
      end;

      I := Uutils.FindKeyWord(TokList[1], OldRoutingOptions, 3);
      if I >= 0 then TokList[1] := RoutingOptions[I];
    end;

    START_DATE_INDEX, REPORT_START_DATE_INDEX, END_DATE_INDEX:
    begin
      S := Uutils.ConvertDate(TokList[1]);
      try
        StrToDate(S, MyFormatSettings);
        TokList[1] := S;
      except
        on EConvertError do Result := ErrMsg(DATE_ERR, '');
      end;
    end;

    START_TIME_INDEX, REPORT_START_TIME_INDEX, END_TIME_INDEX:
    begin
      S := TokList[1];
      try
        StrToTime(S, MyFormatSettings);
        TokList[1] := S;
      except
        on EConvertError do Result := ErrMsg(DATE_ERR, '');
      end;
    end;

    SWEEP_START_INDEX, SWEEP_END_INDEX:
    begin
      S := Uutils.ConvertDate(TokList[1]);
      S2 := S + '/1947';
      try
        StrToDate(S2, MyFormatSettings);
        TokList[1] := S;
      except
        on EConvertError do Result := ErrMsg(DATE_ERR, '');
      end;
    end;

    WET_STEP_INDEX, DRY_STEP_INDEX, REPORT_STEP_INDEX, RULE_STEP_INDEX:
    begin
      S := TokList[1];
      if Uutils.StrHoursToTime(S) = -1 then Result := ErrMsg(TIMESTEP_ERR, '');
    end;

    ROUTING_STEP_INDEX:
    begin
      S := TokList[1];
      T := 0.0;
      if not Uutils.GetExtended(S, T) then
      begin
        T := Uutils.StrHoursToTime(S)*86400.;
        if T <= 0.0 then Result := ErrMsg(TIMESTEP_ERR, '')
        else TokList[1] := Format('%.0f',[T]);
      end;
    end;

    VARIABLE_STEP_INDEX:
    begin
      if Uutils.GetSingle(TokList[1], X) then
        TokList[1] := IntToStr(Round(100.0*X))
      else
        TokList[1] := '0';
    end;

    INERTIAL_DAMPING_INDEX:
    begin
      if Uutils.GetSingle(TokList[1], X) then
      begin
        if X = 0 then TokList[1] := 'NONE'
        else TokList[1] := 'PARTIAL';
      end;
    end;

    // This option is now fixed to SWMM 4.
    COMPATIBILITY_INDEX:
    begin
      TokList[1] := '4';
    end;

    MIN_ROUTE_STEP_INDEX,
    LENGTHEN_STEP_INDEX,
    MIN_SURFAREA_INDEX,
    MIN_SLOPE_INDEX,
    MAX_TRIALS_INDEX,
    HEAD_TOL_INDEX,
    SYS_FLOW_TOL_INDEX,
    LAT_FLOW_TOL_INDEX:
    begin
      Uutils.GetSingle(TokList[1], X);
      if X <= 0 then TokList[1] := '0';
    end;

    NORMAL_FLOW_LTD_INDEX:
    begin
      if SameText(TokList[1], 'NO') or SameText(TokList[1], 'NONE')
      then TokList[1] := 'NONE'
      else if SameText(TokList[1], 'SLOPE')
      then TokList[1] := 'SLOPE'
      else if SameText(TokList[1], 'YES') or SameText(TokList[1], 'FROUDE')
      then TokList[1] := 'FROUDE'
      else if SameText(TokList[1], 'BOTH') then TokList[1] := 'BOTH'
      else Result := ErrMsg(KEYWORD_ERR, TokList[1]);
    end;

    FORCE_MAIN_EQN_INDEX:
    begin
      I := Uutils.FindKeyWord(TokList[1], ForceMainEqnOptions, 3);
      if I < 0 then Result := ErrMsg(KEYWORD_ERR, TokList[1])
      else TokList[1] := ForceMainEqnOptions[I];
    end;

    SURCHARGE_METHOD_INDEX:
    begin
      if (not SameText(TokList[1], 'EXTRAN'))
      and (not SameText(TokList[1], 'SLOT'))
      then Result := ErrMsg(KEYWORD_ERR, TokList[1]);
    end;

    LINK_OFFSETS_INDEX:
    begin
      I := Uutils.FindKeyWord(TokList[1], LinkOffsetsOptions, 10);
      if I < 0 then Result := ErrMsg(KEYWORD_ERR, TokList[1])
      else TokList[1] := LinkOffsetsOptions[I];
    end;

    IGNORE_RAINFALL_INDEX,
    IGNORE_SNOWMELT_INDEX,
    IGNORE_GRNDWTR_INDEX,
    IGNORE_ROUTING_INDEX,
    IGNORE_QUALITY_INDEX:
    begin
      if not SameText(TokList[1], 'YES') and not SameText(TokList[1], 'NO')
      then Result := ErrMsg(KEYWORD_ERR, TokList[1]);
    end;

    NUM_THREADS_INDEX:
    begin
      I := StrToIntDef(TokList[1], 1);
      if I < 0 then I := 1;
      if (I = 0) or (I > Uutils.GetProcessorCount) then
        I := Uutils.GetProcessorCount;
      TokList[1] := IntToStr(I);
    end;

  end;
  if Result = 0 then Project.Options.Data[Index] := TokList[1];
end;


function ReadTagData: Integer;
//-----------------------------------------------------------------------------
//  Reads in tag data from a line of input.
//-----------------------------------------------------------------------------
const
  TagTypes: array[0..3] of PChar = ('Gage', 'Subcatch', 'Node', 'Link');
var
  I, J : Integer;
  aNode: TNode;
  aLink: TLink;
begin
  Result := 0;
  if Ntoks < 3
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin
    I := Uutils.FindKeyWord(TokList[0], TagTypes, 4);
    case I of

    -1: Result := ErrMsg(KEYWORD_ERR, TokList[0]);

     0: begin
          J := Project.Lists[RAINGAGE].IndexOf(TokList[1]);
          if J >= 0 then with Project.GetGage(J) do Data[TAG_INDEX] := TokList[2];
        end;

     1: begin
          J := Project.Lists[SUBCATCH].IndexOf(TokList[1]);
          if J >= 0 then with Project.GetSubcatch(SUBCATCH, J) do
            Data[TAG_INDEX] := TokList[2];
        end;

     2: begin
          aNode := FindNode(TokList[1]);
          if (aNode <> nil) then aNode.Data[TAG_INDEX] := TokList[2];
        end;

     3: begin
          aLink := FindLink(TokList[1]);
          if (aLink <> nil) then aLink.Data[TAG_INDEX] := TokList[2];
        end;
     end;
  end;
end;


function ReadSymbolData: Integer;
//-----------------------------------------------------------------------------
//  Reads rain gage coordinate data from a line of input.
//-----------------------------------------------------------------------------
var
  J     : Integer;
  X, Y  : Extended;
  aGage : TRaingage;
begin
  Result := 0;
  if (Ntoks < 3)
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin

    // Locate the gage ID in the database
    J := Project.Lists[RAINGAGE].IndexOf(TokList[0]);

    // If gage exists then assign it X & Y coordinates
    if (J >= 0) then
    begin
      aGage := Project.GetGage(J);
      if not Uutils.GetExtended(TokList[1], X) then
        Result := ErrMsg(NUMBER_ERR, TokList[1])
      else if not Uutils.GetExtended(TokList[2], Y) then
          Result := ErrMsg(NUMBER_ERR, TokList[2])
      else
      begin
        aGage.X := X;
        aGage.Y := Y;
      end;
    end;
  end;
end;


function ReadMapData: Integer;
//-----------------------------------------------------------------------------
//  Reads map dimensions data from a line of input.
//-----------------------------------------------------------------------------
var
  I       : Integer;
  Index   : Integer;
  Keyword : String;
  X       : array[1..4] of Extended;
begin
  // Check which keyword applies
  Result := 0;
  Keyword := TokList[0];
  Index := Uutils.FindKeyWord(Keyword, MapWords, 4);
  case Index of

    0:  // Map dimensions
    begin
      if Ntoks < 5 then Result := ErrMsg(ITEMS_ERR, '')
      else begin
        for I := 1 to 4 do
        begin
          if not Uutils.GetExtended(TokList[I], X[I]) then
            Result := ErrMsg(NUMBER_ERR, TokList[I]);
        end;
        if (Result = 0) and (X[1] < X[3]) and (X[2] < x[4]) then
        with MapForm.Map.Dimensions do
        begin
          LowerLeft.X := X[1];
          LowerLeft.Y := X[2];
          UpperRight.X := X[3];
          UpperRight.Y := X[4];
          MapExtentSet := True;
        end;
      end;
    end;

    1:  //Map units
    if Ntoks > 1 then
    begin
      I := Uutils.FindKeyWord(Copy(TokList[1], 1, 1), MapUnits, 1);
      if I < 0 then Result := ErrMsg(KEYWORD_ERR, TokList[1])
      else MapForm.Map.Dimensions.Units := TMapUnits(I);
      with MapForm.Map.Dimensions do
      begin
        if Units = muDegrees then Digits := MAXDEGDIGITS
        else Digits := Umap.DefMapDimensions.Digits;
      end;
    end;

    else Result := ErrMsg(KEYWORD_ERR, Keyword);
  end;
end;


function ReadCoordData: Integer;
//-----------------------------------------------------------------------------
//  Reads node coordinate data from a line of input.
//-----------------------------------------------------------------------------
var
  X, Y  : Extended;
  aNode : TNode;
begin
  Result := 0;
  if (Ntoks < 3)
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin

    // Locate the node ID in the database
    aNode := FindNode(TokList[0]);

    // If node exists then assign it X & Y coordinates
    if (aNode <> nil) then
    begin
      if not Uutils.GetExtended(TokList[1], X) then
        Result := ErrMsg(NUMBER_ERR, TokList[1])
      else if not Uutils.GetExtended(TokList[2], Y) then
        Result := ErrMsg(NUMBER_ERR, TokList[2])
      else
      begin
        aNode.X := X;
        aNode.Y := Y;
      end;
    end;
  end;
end;


function ReadVertexData: Integer;
//-----------------------------------------------------------------------------
//  Reads link vertex coordinate data from a line of input.
//-----------------------------------------------------------------------------
var
  X, Y  : Extended;
  aLink : TLink;

begin
  Result := 0;
  if (Ntoks < 3)
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin

    // Locate the link ID in the database
    aLink := FindLink(TokList[0]);;

    // If link exists then assign it X & Y coordinates
    if (aLink <> nil) then
    begin
      if not Uutils.GetExtended(TokList[1], X) then
        Result := ErrMsg(NUMBER_ERR, TokList[1])
      else if not Uutils.GetExtended(TokList[2], Y) then
        Result := ErrMsg(NUMBER_ERR, TokList[2])
      else  aLink.Vlist.Add(X, Y);
    end;
  end;
end;


function ReadPolygonData: Integer;
//-----------------------------------------------------------------------------
//  Reads polygon coordinates associated with subcatchment outlines.
//-----------------------------------------------------------------------------
var
  Index : Integer;
  X, Y  : Extended;
  Vlist : TVertexList;
begin
  Result := 0;
  Vlist := nil;
  if (Ntoks < 3)
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin
    // See if line begins with a subcatchment ID
    Index := Project.Lists[SUBCATCH].IndexOf(TokList[0]);
    if Index >= 0 then
      Vlist := Project.GetSubcatch(SUBCATCH, Index).Vlist

    // See if line begins with a storage node ID
    else begin
      Index := Project.Lists[STORAGE].IndexOf(TokList[0]);
      if Index >= 0 then
        Vlist := Project.GetNode(STORAGE, Index).Vlist;
    end;

    // If vertex list exists then add a new vertex to it
    if (Vlist <> nil) then
    begin
      if not Uutils.GetExtended(TokList[1], X) then
        Result := ErrMsg(NUMBER_ERR, TokList[1])
      else if not Uutils.GetExtended(TokList[2], Y) then
        Result := ErrMsg(NUMBER_ERR, TokList[2])
      else  Vlist.Add(X, Y);
    end;
  end;
end;


function ReadLabelData: Integer;
//-----------------------------------------------------------------------------
//  Reads map label data from a line of input.
//-----------------------------------------------------------------------------
var
  Ntype    : Integer;
  Index    : Integer;
  X, Y     : Extended;
  S        : String;
  aMapLabel: TMapLabel;
begin
  if (Ntoks < 3)
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin
    if not Uutils.GetExtended(TokList[0], X) then
        Result := ErrMsg(NUMBER_ERR, TokList[0])
    else if not Uutils.GetExtended(TokList[1], Y) then
        Result := ErrMsg(NUMBER_ERR, TokList[1])
    else begin
      S := TokList[2];
      aMapLabel := TMapLabel.Create;
      aMapLabel.X := X;
      aMapLabel.Y := Y;
      Project.Lists[MAPLABEL].AddObject(S, aMapLabel);
      Index := Project.Lists[MAPLABEL].Count - 1;
      aMapLabel.Text := PChar(Project.Lists[MAPLABEL].Strings[Index]);
      Project.HasItems[MAPLABEL] := True;
      if Ntoks >= 4 then
      begin
        if (Length(TokList[3]) > 0) and
          Project.FindNode(TokList[3], Ntype, Index) then
            aMapLabel.Anchor := Project.GetNode(Ntype, Index);
      end;
      if Ntoks >= 5 then aMapLabel.FontName := TokList[4];
      if Ntoks >= 6 then aMapLabel.FontSize := StrToInt(TokList[5]);
      if Ntoks >= 7 then
        if StrToInt(TokList[6]) = 1 then aMapLabel.FontBold := True;
      if Ntoks >= 8 then
        if StrToInt(TokList[7]) = 1 then aMapLabel.FontItalic := True;
      Result := 0;
    end;
  end;
end;


function ReadBackdropData: Integer;
//-----------------------------------------------------------------------------
//  Reads map backdrop image information from a line of input.
//-----------------------------------------------------------------------------
var
  Index   : Integer;
  Keyword : String;
  I       : Integer;
  X       : array[1..4] of Extended;
begin
  // Check which keyword applies
  Result := 0;
  Keyword := TokList[0];
  Index := Uutils.FindKeyWord(Keyword, BackdropWords, 4);
  case Index of

    0:  //Backdrop file
    if Ntoks > 1 then
    begin
      MapForm.Map.Backdrop.Filename := FullPathName(TokList[1]);
      MapForm.Map.Backdrop.Source := bdFile;
    end;

    1:  // Backdrop dimensions
    begin
      if Ntoks < 5 then Result := ErrMsg(ITEMS_ERR, '')
      else begin
        for i := 1 to 4 do
          if not Uutils.GetExtended(TokList[I], X[I]) then
            Result := ErrMsg(NUMBER_ERR, TokList[I]);
        if Result = 0 then with MapForm.Map.Backdrop do
        begin
          LowerLeft.X := X[1];
          LowerLeft.Y := X[2];
          UpperRight.X := X[3];
          UpperRight.Y := X[4];
        end;
      end;
    end;

    2:  //Map units - deprecated
    if Ntoks > 1 then
    begin
      i := Uutils.FindKeyWord(Copy(TokList[1], 1, 1), MapUnits, 1);
      if I < 0 then Result := ErrMsg(KEYWORD_ERR, TokList[1])
      else MapForm.Map.Dimensions.Units := TMapUnits(I);
    end;

    3, 4:  //Backdrop offset or scaling -- deprecated
    begin
      if Ntoks < 3 then Result := ErrMsg(ITEMS_ERR, '')
      else if not Uutils.GetExtended(TokList[1], X[1]) then
        Result := ErrMsg(NUMBER_ERR, TokList[1])
      else if not Uutils.GetExtended(TokList[2], X[2]) then
        Result := ErrMsg(NUMBER_ERR, TokList[2])
      else
      begin
        if Index = 3 then
        begin
          BackdropX := X[1];
          BackdropY := X[2];
        end
        else
        begin
          BackdropW := X[1];
          BackdropH := X[2];
        end;
      end;
    end;

    else Result := ErrMsg(KEYWORD_ERR, Keyword);
  end;
end;


function ReadProfileData: Integer;
//-----------------------------------------------------------------------------
//  Reads profile plot data from a line of input.
//-----------------------------------------------------------------------------
var
  I, J: Integer;
  S   : String;
begin
  Result := 0;
  if (Ntoks < 2) then Exit;

  // Locate the profile name in the database
  I := Project.ProfileNames.IndexOf(TokList[0]);

  // If profile does not exist then create it
  if (I < 0) then
  begin
    Project.ProfileNames.Add(TokList[0]);
    I := Project.ProfileNames.Count-1;
    S := '';
    Project.ProfileLinks.Add(S);
  end
  else S := Project.ProfileLinks[I] + #13;

  // Add each remaining token to the list of links in the profile
  S := S + TokList[1];
  for J := 2 to Ntoks-1 do
    S := S + #13 + TokList[J];
  Project.ProfileLinks[I] := S;
end;


procedure SetMapDimensions;
//-----------------------------------------------------------------------------
// Determines map dimensions based on range of object coordinates.
//-----------------------------------------------------------------------------
var
  Xleft, Xright, Yleft, Yright: Extended;
begin
  with MapForm.Map do
  begin
    // Dimensions were provided in [BACKDROP] section
    if (Backdrop.LowerLeft.X < Backdrop.UpperRight.X) and
       (Backdrop.LowerLeft.Y < Backdrop.UpperRight.Y) then
    begin
      if MapExtentSet then with Dimensions do
      begin
        LowerLeft.X  := Min(LowerLeft.X, Backdrop.LowerLeft.X);
        LowerLeft.Y  := Min(LowerLeft.Y, Backdrop.LowerLeft.Y);
        UpperRight.X := Max(UpperRight.X, Backdrop.UpperRight.X);
        UpperRight.Y := Max(UpperRight.Y, Backdrop.UpperRight.Y);
      end
      else with Dimensions do
      begin
        LowerLeft  := Backdrop.LowerLeft;
        UpperRight := Backdrop.UpperRight;
      end;
      MapExtentSet := True;
    end;

    // Get dimensions from map objects
    Ucoords.GetCoordExtents(Xleft, Yleft, Xright, Yright);
    with Dimensions do
    begin
      if MapExtentSet then
      begin
        LowerLeft.X := Min(LowerLeft.X, Xleft);
        LowerLeft.Y := Min(LowerLeft.Y, Yleft);
        UpperRight.X := Max(UpperRight.X, Xright);
        UpperRight.Y := Max(UpperRight.Y, Yright);
      end else
      begin
        LowerLeft.X := Xleft;
        LowerLeft.Y := Yleft;
        UpperRight.X := Xright;
        UpperRight.Y := Yright;
      end;
    end;
  end;
end;


function ParseInpLine(S: String): Integer;
//-----------------------------------------------------------------------------
//  Parses current input line depending on current section of input file.
//-----------------------------------------------------------------------------
begin
  case Section of
    1:    Result := ReadOptionData;
    2:    Result := ReadRaingageData;
    3:    Result := ReadHydrographData;
    4:    Result := ReadEvaporationData;
    5:    Result := ReadSubcatchmentData;
    6:    Result := ReadSubareaData;
    7:    Result := ReadInfiltrationData;
    8:    Result := ReadAquiferData;
    9:    Result := ReadGroundwaterData;
    10:   Result := ReadJunctionData;
    11:   Result := ReadOutfallData;
    12:   Result := ReadStorageData;
    13:   Result := ReadDividerData;
    14:   Result := ReadConduitData;
    15:   Result := ReadPumpData;
    16:   Result := ReadOrificeData;
    17:   Result := ReadWeirData;
    18:   Result := ReadOutletData;
    19:   Result := ReadXsectionData;
    20:   Result := ReadTransectData;
    21:   Result := ReadLossData;
    //22: ReadControlData called directly from ReadFile
    23:   Result := ReadPollutantData;
    24:   Result := ReadLanduseData;
    25:   Result := ReadBuildupData;
    26:   Result := ReadWashoffData;
    27:   Result := ReadCoverageData;
    28:   Result := ReadExInflowData;
    29:   Result := ReadDWInflowData;
    30:   Result := ReadPatternData;
    31:   Result := ReadIIInflowData;
    32:   Result := ReadLoadData;
    33:   Result := ReadCurveData;
    34:   Result := ReadTimeseriesData;
    35:   Result := ReadReportData;
    36:   Result := ReadFileData;
    37:   Result := ReadMapData;
    38:   Result := ReadCoordData;
    39:   Result := ReadVertexdata;
    40:   Result := ReadPolygonData;
    41:   Result := ReadSymbolData;
    42:   Result := ReadLabelData;
    43:   Result := ReadBackdropData;
    44:   Result := ReadProfileData;
    45:   Result := ReadCurveData;
    46:   Result := ReadTemperatureData;
    47:   Result := ReadSnowpackData;
    48:   Result := ReadTreatmentData(S);
    49:   Result := ReadTagData;
    50:   Result := Ulid.ReadLidData(TokList, Ntoks);
    51:   Result := ReadLidUsageData;
    52:   Result := ReadGroundwaterFlowEqn(S);
    53:   Result := ReadAdjustmentData;
    55:   Result := ReadStreetData;
    56:   Result := Uinlet.ReadInletUsageData(TokList, Ntoks);
    57:   Result := Uinlet.ReadInletDesignData(TokList, Ntoks);
    else  Result := 0;
  end;
end;


function ParseImportLine(Line: String): Integer;
//-----------------------------------------------------------------------------
//  Processes line of input from imported scenario or map file.
//  (Not currently used.)
//-----------------------------------------------------------------------------
begin
  Result := 0;
end;


procedure StripComment(const Line: String; var S: String);
//-----------------------------------------------------------------------------
//  Strips comment (text following a ';') from a line of input.
//  Ignores comment if it begins with ';;'.
//-----------------------------------------------------------------------------
var
  P: Integer;
  N: Integer;
  C: String;
begin
  C := '';
  S := Trim(Line);
  P := Pos(';', S);
  if P > 0 then
  begin
    N := Length(S);
    C := Copy(S, P+1, N);
    if (Length(C) >= 1) and (C[1] <> ';') then
    begin
      if Length(Comment) > 0 then Comment := Comment + #13;
      Comment := Comment + C;
    end;
    Delete(S, P, N);
  end;
end;


function FindNewSection(const S: String): Integer;
//-----------------------------------------------------------------------------
//  Checks if S matches any of the section heading key words.
//-----------------------------------------------------------------------------
var
  K: Integer;
begin
  for K := 0 to High(SectionWords) do
  begin
    if Pos(SectionWords[K], S) = 1 then
    begin
      Result := K;
      Exit;
    end;
  end;
  Result := -1;
end;


function StartNewSection(S: String): Integer;
//-----------------------------------------------------------------------------
//  Begins reading a new section of the input file.
//-----------------------------------------------------------------------------
var
  K : Integer;

begin
  // Determine which new section to begin
  K := FindNewSection(UpperCase(S));
  if (K >= 0) then
  begin

    //Update section code
    Section := K;
    PrevID := '';
    Result := 0;
  end
  else Result := ErrMsg(KEYWORD_ERR, S);
  Comment := '';
end;


function ReadFile(var F: Textfile; const Fsize: Int64):Boolean;
//-----------------------------------------------------------------------------
//  Reads each line of a SWMM input file.
//-----------------------------------------------------------------------------
var
  Err         : Integer;
  ByteCount   : Integer;
  StepCount   : Integer;
  StepSize    : Integer;
  S           : String;
  I           : Integer;

begin
  Result := True;
  ErrCount := 0;
  LineCount := 0;
  Comment := '';

  // Initialize progress meter settings
  StepCount := MainForm.ProgressBar.Max div MainForm.ProgressBar.Step;
  StepSize := Fsize div StepCount;
  if StepSize < 1000 then StepSize := 0;
  ByteCount := 0;

  // Read each line of input file
  Reset(F);
  while not Eof(F) do
  begin
    Err := 0;
    Readln(F, Line);

    // Strip off any BOM character at start of file
    if (LineCount = 0) then
    begin
      for I := 1 to Length(Line) do
        if Line[I] < Char(128) then break;
      Delete(Line, 1, I-1);
    end;

    // Update progress monitor
    Inc(LineCount);
    if StepSize > 0 then
    begin
      Inc(ByteCount, Length(Line));
      MainForm.UpdateProgressBar(ByteCount, StepSize);
    end;

    // Strip out trailing spaces, control characters & comment
    StripComment(Line, S);

    // Check if line begins a new input section
    if (Pos('[', S) = 1) then Err := StartNewSection(S)
    else
    begin

      // Check if line contains project title/notes
      if (Section = 0) and (Length(Line) > 0) then
      begin
        if LeftStr(Line,2) <> ';;' then Err := ReadTitleData(Line);
      end

      // Check if line contains a control rule clause
      else if (Section = 22) then Err := ReadControlData(Line)

      // Check if line contains an event start/end dates
      else if (Section = 54) then Err := ReadEventData(Trim(Line))

      // If in some section, then process the input line
      else
      begin
        // Break line into string tokens and parse their contents
        Uutils.Tokenize(S, TokList, Ntoks);
        if (Ntoks > 0) and (Section >= 0) then
        begin
          Err := ParseInpLine(S);
          Comment := '';
        end

        // No current section -- file was probably not an EPA-SWMM file
        else if (Ntoks > 0) then
        begin
          Uutils.MsgDlg(
            'No data read from file - it is likely not a true SWMM project file.',
            mtWarning, [mbOK], MainForm);
          Result := False;
          Exit;
        end;
      end;
    end;

    // Increment error count
    if Err > 0 then Inc(ErrCount);
  end;  //End of file.

  if ErrCount > MAX_ERRORS then ErrList.Add(
    IntToStr(ErrCount-MAX_ERRORS) + TXT_MORE_ERRORS);
end;


procedure DisplayInpErrForm(const Fname: String);
//-----------------------------------------------------------------------------
//  Displays Status Report form that lists any error messages.
//-----------------------------------------------------------------------------
begin
  SysUtils.DeleteFile((TempReportFile));
  TempReportFile := Uutils.GetTempFile(TempDir,'swmm');
  ErrList.Insert(0, TXT_ERROR_REPORT + Fname + #13);
  ErrList.SaveToFile(TempReportFile);
  MainForm.MnuReportStatusClick(MainForm);
end;


procedure ReverseVertexLists;
//-----------------------------------------------------------------------------
//  Reverses list of vertex points for each link in the project.
//-----------------------------------------------------------------------------
var
  I, J: Integer;
begin
  for I := 0 to MAXCLASS do
  begin
    if not Project.IsLink(I) then continue;
    for J := 0 to Project.Lists[I].Count-1 do
      if Project.GetLink(I, J).Vlist <> nil then
        Project.GetLink(I, J).Vlist.Reverse;
  end;
end;


procedure SetSubcatchCentroids;
//-----------------------------------------------------------------------------
//  Determines the centroid of each subcatchment polygon.
//-----------------------------------------------------------------------------
var
  I : Integer;
begin
  for I := 0 to Project.Lists[SUBCATCH].Count - 1 do
  begin
    Project.GetSubcatch(SUBCATCH, I).SetCentroid;
  end;
end;

procedure SetStorageCentroids;
//-----------------------------------------------------------------------------
//  Determines the centroid of each storage node polygon.
//-----------------------------------------------------------------------------
var
  I : Integer;
  aNode : TNode;
begin
  for I := 0 to Project.Lists[STORAGE].Count - 1 do
  begin
    aNode := Project.GetNode(STORAGE, I);
    with aNode do
    begin
      if Vlist.Count = 0 then Vlist.Add(X, Y);
      SetCentroid;
    end;
  end;
end;

procedure SetIDPtrs;
//-----------------------------------------------------------------------------
//  Makes pointers to ID strings the ID property of objects.
//-----------------------------------------------------------------------------
var
  I, J : Integer;
  C : TSubcatch;
  S : String;

begin
  for I := 0 to MAXCLASS do
  begin
    if I = RAINGAGE then with Project.Lists[RAINGAGE] do
    begin
      for J := 0 to Count-1 do TRaingage(Objects[J]).ID := PChar(Strings[J]);
    end
    else if Project.IsSubcatch(I) then with Project.Lists[SUBCATCH] do
    begin
      for J := 0 to Count-1 do
      begin
        C := TSubcatch(Objects[J]);
        C.ID := PChar(Strings[J]);
        S := Trim(C.Data[SUBCATCH_OUTLET_INDEX]);
        C.OutSubcatch := FindSubcatch(S);
        if C.OutSubcatch = nil then C.OutNode := FindNode(S);
      end;
    end
    else if Project.IsNode(I) then with Project.Lists[I] do
    begin
      for J := 0 to Count-1 do TNode(Objects[J]).ID := PChar(Strings[J]);
    end
    else if Project.IsLink(I) then with Project.Lists[I] do
    begin
      for J := 0 to Count-1 do TLink(Objects[J]).ID := PChar(Strings[J]);
    end
    else if I = STREET then with Project.Lists[I] do
    begin
      for J := 0 to Count-1 do
      begin
        TStreet(Objects[J]).SetMaxDepth;
        Project.SetTransectConduitDepth(Strings[J],
          TStreet(Objects[J]).MaxDepth);
     end;
    end
    else if I = TRANSECT then with Project.Lists[I] do
    begin
      for J := 0 to Count-1 do
      begin
        TTransect(Objects[J]).CheckData;
        TTransect(Objects[J]).SetMaxDepth;
        Project.SetTransectConduitDepth(Strings[J],
          TTransect(Objects[J]).Data[TRANSECT_MAX_DEPTH]);
      end;
    end
    else continue;
  end;
end;


function ReadInpFile(const Fname: String):Boolean;
//-----------------------------------------------------------------------------
//  Reads SWMM input data from a text file.
//-----------------------------------------------------------------------------
var
  F : Textfile;
begin
  // Try to open the file
  Result := False;
  AssignFile(F,Fname);
  {$I-}
  Reset(F);
  {$I+}
  if (IOResult = 0) then
  begin

    // Create stringlists
    Screen.Cursor := crHourGlass;
    MapExtentSet := False;
    ErrList := TStringList.Create;
    TokList := TStringList.Create;
    SubcatchList := TStringList.Create;
    NodeList := TStringList.Create;
    LinkList := TStringList.Create;
    FileType := ftInput;
    InpFile := Fname;
    try

      // Read the file
      MainForm.ShowProgressBar(MSG_READING_PROJECT_DATA);
      SubcatchList.Sorted := True;
      NodeList.Sorted := True;
      LinkList.Sorted := True;
      Section := -1;
      Result := ReadFile(F, Uutils.GetFileSize(Fname));
      if (Result = True) then
      begin
        // Establish pointers to ID names
        SetIDPtrs;
      end;

    finally
      // Free the stringlists
      SubcatchList.Free;
      NodeList.Free;
      LinkList.Free;
      TokList.Free;
      MainForm.PageSetupDialog.Header.Text := Project.Title;
      MainForm.HideProgressBar;
      Screen.Cursor := crDefault;

      // Display errors if found & set map dimensions
      if Result = True then
      begin
        if ErrList.Count > 0 then DisplayInpErrForm(Fname);
        SetSubcatchCentroids;
        SetStorageCentroids;
        SetMapDimensions;
      end;
      ErrList.Free;
    end;
  end;

  // Close the input file
  CloseFile(F);
end;


procedure ClearDefaultDates;
//-----------------------------------------------------------------------------
//  Clears project's date/time settings.
//-----------------------------------------------------------------------------
begin
  with Project.Options do
  begin
    Data[START_DATE_INDEX]        := '';
    Data[START_TIME_INDEX]        := '';
    Data[REPORT_START_DATE_INDEX] := '';
    Data[REPORT_START_TIME_INDEX] := '';
    Data[END_DATE_INDEX]          := '';
    Data[END_TIME_INDEX]          := '';
  end;
end;


procedure SetDefaultDates;
//-----------------------------------------------------------------------------
//  Sets default values for project's date/time settings.
//-----------------------------------------------------------------------------
var
  StartTime: TDateTime;
  StartDate: TDateTime;
  T: TDateTime;
  D: TDateTime;
begin
  with Project.Options do
  begin

  // Process starting date/time
    try
      StartDate := StrToDate(Data[START_DATE_INDEX], MyFormatSettings);
    except
      On EConvertError do StartDate := Date;
    end;
    StartTime := Uutils.StrHoursToTime(Data[START_TIME_INDEX]);
    if StartTime < 0 then StartTime := 0;
    D := StartDate + StartTime;
    Data[START_DATE_INDEX] := DateToStr(D, MyFormatSettings);
    Data[START_TIME_INDEX] := TimeToStr(D, MyFormatSettings);

  // Process reporting start date/time
    try
      D := StrToDate(Data[REPORT_START_DATE_INDEX], MyFormatSettings);
    except
      On EConvertError do D := StartDate;
    end;
    T := Uutils.StrHoursToTime(Data[REPORT_START_TIME_INDEX]);
    if T < 0 then T := StartTime;
    D := D + T;
    Data[REPORT_START_DATE_INDEX] := DateToStr(D, MyFormatSettings);
    Data[REPORT_START_TIME_INDEX] := TimeToStr(D, MyFormatSettings);

  // Process ending date/time
    try
      D := StrToDate(Data[END_DATE_INDEX], MyFormatSettings);
    except
      On EConvertError do D := StartDate;
    end;
    T := Uutils.StrHoursToTime(Data[END_TIME_INDEX]);
    if T < 0 then T := StartTime;
    D := D + T;
    Data[END_DATE_INDEX] := DateToStr(D, MyFormatSettings);
    Data[END_TIME_INDEX] := TimeToStr(D, MyFormatSettings);
  end;
end;


function OpenProject(const Fname: String): TInputFileType;
//-----------------------------------------------------------------------------
//  Reads in project data from a file.
//-----------------------------------------------------------------------------
begin
  // Show progress meter
  ClearDefaultDates;
  Screen.Cursor := crHourGlass;
  MainForm.ShowProgressBar(MSG_READING_PROJECT_DATA);

  // Use default map dimensions and backdrop settings
  MapForm.Map.Dimensions := DefMapDimensions;
  MapForm.Map.Backdrop := DefMapBackdrop;

  // Do the following for non-temporary input files
  if not SameText(Fname, Uglobals.TempInputFile) then
  begin

    // Create a backup file
    if AutoBackup
    then CopyFile(PChar(Fname), PChar(ChangeFileExt(Fname, '.bak')), FALSE);

    // Retrieve project defaults from .INI file
    if CompareText(ExtractFileExt(Fname), '.ini') <> 0
    then Uinifile.ReadProjIniFile(ChangeFileExt(Fname, '.ini'));
  end;
  // Read and parse each line of input file
  Result := iftNone;
  if ReadInpFile(Fname) then Result := iftINP;

  // Finish processing the input data
  if Result <> iftNone then
  begin
    SetDefaultDates;                   // set any missing analysis dates
    Uglobals.RegisterCalibData;        // register calibration data files
    Uupdate.UpdateUnits;               // update choice of unit system
    Uupdate.UpdateDefOptions;          // update default analysis options
    Uupdate.UpdateLinkHints;           // update hints used for offsets
    Project.GetControlRuleNames;       // store control rule names in a list
  end;

  // Hide progress meter.
  MainForm.HideProgressBar;
  Screen.Cursor := crDefault;
end;

end.
