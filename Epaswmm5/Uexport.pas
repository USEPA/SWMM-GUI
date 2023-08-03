unit Uexport;

{-------------------------------------------------------------------}
{                    Unit:    Uexport.pas                           }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21   (5.2.0)                    }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Delphi Pascal unit that exports current project data to a       }
{   formatted text file.                                            }
{-------------------------------------------------------------------}

interface

uses SysUtils, Windows, Messages, Classes, Math, Dialogs, StrUtils,
  Uglobals, Uutils, Uproject, Uvertex;

procedure ExportMap(S: TStringlist);
procedure ExportProfiles(S: TStringlist);
procedure ExportProject(S: TStringlist; P: String);
procedure ExportTags(S: TStringlist);
procedure ExportTempDir(S: TStringlist);
procedure SaveProject(Fname: String);
procedure SaveResults(Fname: String);
procedure SaveHotstartFile(Fname: String);
function  RelativePathName(Fname: String): String;

implementation

uses
  Fmap, Uinifile, Uoutput, Dreporting, Ulid, Uinlet;

var
  DXFCount   : Integer;      // # nodes w/ external inflow
  DWFCount   : Integer;      // # nodes w/ dry weather flow
  RDIICount  : Integer;      // # nodes w/ RDII inflow
  TreatCount : Integer;      // # nodes w/ treatment
  Tab        : String;       // tab or space character
  SaveToPath : String;       // path of saved project file


procedure ExportComment(S: TStringlist; Comment: String);
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  if Length(Trim(Comment)) = 0 then exit;
  with Tstringlist.Create do
  try
    Clear;
    SetText(PChar(Comment));
    for I := 0 to Count-1 do S.Add(';' + Strings[I]);
  finally
    Free;
  end;
end;


procedure ExportTitle(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  with Project do
  begin
    S.Add('[TITLE]');
    S.Add(';;Project Title/Notes');
    for I := 0 to Lists[NOTES].Count-1 do
      S.Add(Lists[NOTES].Strings[I]);
  end;
end;


procedure ExportOptions(S: TStringlist);
//-----------------------------------------------------------------------------
var
   I: Integer;
   X: Extended;
   Line: String;
   TmpOptions: array[0..MAXOPTIONS] of String;
begin
  S.Add('');
  S.Add('[OPTIONS]');
  S.Add(';;Option            ' + Tab + 'Value');

  with Project.Options do
  begin
    // Copy current options to a temporary  array
    // (so that blank defaults can be filled in)
    for I := 0 to MAXOPTIONS do TmpOptions[I] := Data[I];

    // Fill in blank defaults
    if Length(Trim(TmpOptions[REPORT_START_DATE_INDEX])) = 0 then
       TmpOptions[REPORT_START_DATE_INDEX] := TmpOptions[START_DATE_INDEX];
    if Length(Trim(TmpOptions[REPORT_START_TIME_INDEX])) = 0 then
       TmpOptions[REPORT_START_TIME_INDEX] := TmpOptions[START_TIME_INDEX];
    if Length(Trim(TmpOptions[END_DATE_INDEX])) = 0 then
      TmpOptions[END_DATE_INDEX] := TmpOptions[START_DATE_INDEX];
    if Length(Trim(TmpOptions[END_TIME_INDEX])) = 0 then
      TmpOptions[END_TIME_INDEX] := TmpOptions[START_TIME_INDEX];
    TmpOptions[VARIABLE_STEP_INDEX] :=
      Format('%.2f', [StrToFloat(Data[VARIABLE_STEP_INDEX])/100]);

    if SameText(Data[SURCHARGE_METHOD_INDEX], 'EXTRAN')
    then TmpOptions[SURCHARGE_METHOD_INDEX] := '';

    // If routing time step is whole number of seconds then export it
    // in Hrs:Min:Sec format for backward compatibility
    if Uutils.GetExtended(Data[ROUTING_STEP_INDEX], X) then
    begin
      if Frac(X) = 0
      then TmpOptions[ROUTING_STEP_INDEX] := Uutils.GetTimeString(Round(X));
    end;

    if Length(Trim(TmpOptions[MIN_SURFAREA_INDEX])) = 0
    then TmpOptions[MIN_SURFAREA_INDEX] := '0';
    if Length(Trim(TmpOptions[HEAD_TOL_INDEX ])) = 0
    then TmpOptions[HEAD_TOL_INDEX ] := '0';

    // Write required options to the data set
    for I := FLOW_UNITS_INDEX to SKIP_STEADY_INDEX do
    begin
      Line := Format('%-20s', [OptionLabels[I]]) + Tab + TmpOptions[I];
      S.Add(Line);
    end;
    S.Add('');

    // Write options that ignore certain process models
    for I := IGNORE_RAINFALL_INDEX to IGNORE_QUALITY_INDEX do
    begin
      if SameText(TmpOptions[I], 'YES') then
      begin
        Line := Format('%-20s', [OptionLabels[I]]) + Tab + TmpOptions[I];
        S.Add(Line);
      end;
    end;

    // Write date & time options
    for I := START_DATE_INDEX to RULE_STEP_INDEX do
    begin
      Line := Format('%-20s', [OptionLabels[I]]) + Tab + TmpOptions[I];
      S.Add(Line);
    end;
    S.Add('');

    // Write dynamic wave flow routing options
    for I := INERTIAL_DAMPING_INDEX to NUM_THREADS_INDEX  do
    begin
      if Length(TmpOptions[I]) = 0 then continue;
      Line := Format('%-20s', [OptionLabels[I]]) + Tab + TmpOptions[I];
      S.Add(Line);
    end;
  end;
end;


procedure ExportRaingages(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I:     Integer;
  Line:  String;
  Fname: String;
  Rgage: TRaingage;
begin
  if Project.Lists[RAINGAGE].Count = 0 then exit;
  S.Add('');
  S.Add('[RAINGAGES]');
  Line := ';;Name          ' + Tab + 'Format   ' + Tab + 'Interval';
  Line := Line + Tab + 'SCF     ' + Tab + 'Source    ';
  S.Add(Line);
  Line := ';;--------------' + Tab + '---------' + Tab + '------' + Tab + '------';
  Line := Line + Tab + '----------';
  S.Add(Line);
  with Project.Lists[RAINGAGE] do
    for I := 0 to Count-1 do
    begin
      Rgage := TRaingage(Objects[I]);
      ExportComment(S, Rgage.Data[COMMENT_INDEX]);
      Line := Format('%-16s', [Strings[I]]);
      Line := Line + Tab + Format('%-9s', [Rgage.Data[GAGE_DATA_FORMAT]]);
      Line := Line + Tab + Format('%-8s', [Rgage.Data[GAGE_DATA_FREQ]]);
      Line := Line + Tab + Format('%-8s', [Rgage.Data[GAGE_SNOW_CATCH]]);
      Line := Line + Tab + Format('%-10s', [Rgage.Data[GAGE_DATA_SOURCE]]);
      if SameText(Rgage.Data[GAGE_DATA_SOURCE], RaingageOptions[0]) then
      begin
        Line := Line + Tab + Format('%-16s', [Rgage.Data[GAGE_SERIES_NAME]]);
      end
      else
      begin
        Fname := '"' + RelativePathName(Rgage.Data[GAGE_FILE_PATH]) + '"';
        Line := Line + Tab + Format('%-16s', [Fname]);
        Line := Line + Tab + Format('%-10s', [Rgage.Data[GAGE_STATION_NUM]]);
        Line := Line + Tab + Format('%-5s',  [Rgage.Data[GAGE_RAIN_UNITS]]);
      end;
      S.Add(Line);
    end;
end;


procedure ExportHydrographs(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I: Integer;
  J: Integer;
  K: Integer;
  M: Integer;
  N: Integer;
  Name: String;
  Line: String;
  RTK : String;
  IA: String;
  H: THydrograph;
begin
  if Project.Lists[HYDROGRAPH].Count = 0 then Exit;
  S.Add('');
  S.Add('[HYDROGRAPHS]');
  Line := ';;Hydrograph    ' + Tab + 'Rain Gage/Month ' + Tab + 'Response';
  Line := Line + Tab + 'R       ' + Tab + 'T       ' + Tab + 'K       ';
  Line := Line + Tab + 'Dmax    ' + Tab + 'Drecov  ' + Tab + 'Dinit   ';
  S.Add(Line);
  Line := ';;--------------' + Tab + '----------------' + Tab + '--------';
  Line := Line + Tab + '--------' + Tab + '--------' + Tab + '--------';
  Line := Line + Tab + '--------' + Tab + '--------' + Tab + '--------';
  S.Add(Line);
  for I := 0 to Project.Lists[HYDROGRAPH].Count-1 do
  begin
    // Add name & rain gage line
    Name := Project.Lists[HYDROGRAPH].Strings[I];
    H := THydrograph(Project.Lists[HYDROGRAPH].Objects[I]);
    Line := Format('%-16s', [Name]);
    Line := Line + Tab + Format('%-16s', [H.Raingage]);
    S.Add(Line);

    // For each month (+ All)
    for M := 0 to 12 do
    begin
      // See if month contains any R-T-K values
      for K := 1 to 3 do
      begin
        N := 0;
        for J := 1 to 3 do
        begin
          if Length(Trim(H.Params[M,J,K])) > 0 then
          begin
            N := 1;
            break;
          end;
        end;

        // Write monthly UH parameters to line
        if N > 0 then
        begin
          Line := Format('%-16s', [Name]);
          Line := Line + Tab + Format('%-16s', [MonthLabels[M]]);
          Line := Line + Tab + Format('%-8s', [ResponseTypes[K-1]]);
          for J := 1 to 3 do
          begin
            RTK := Trim(H.Params[M,J,K]);
            if Length(RTK) = 0 then RTK := '0';
            Line := Line + Tab + Format('%-8s', [RTK]);
          end;
          for J := 1 to 3 do
          begin
            IA := Trim(H.InitAbs[M,J,K]);
            if Length(IA) = 0 then IA := '0';
            Line := Line + Tab + Format('%-8s', [IA]);
          end;
          S.Add(Line);
        end;
      end;
    end;
  end;
  S.Add('');
end;


procedure ExportTemperature(S: TStringlist);
//-----------------------------------------------------------------------------
var
  Line: String;
  Fname: String;
  I: Integer;
begin
  with Project.Climatology do
  begin
    if TempDataSource = NO_TEMP then exit;
    S.Add('');
    S.Add('[TEMPERATURE]');
    S.Add(';;Data Element    ' + Tab + 'Values     ');
    if (TempDataSource = TSERIES_TEMP) and (Length(TempTseries) > 0) then
    begin
      Line := 'TIMESERIES        ' + Tab + TempTseries;
      S.Add(Line);
    end
    else if (TempDataSource = FILE_TEMP) and (Length(TempFile) > 0) then
    begin
      Fname := '"' + RelativePathName(TempFile) + '"';
      Line := 'FILE              ' + Tab + Fname;
      Line := Line + Tab + Format('%-10s', [TempStartDate]);
      Line := Line + Tab + Format('%-3s', [TempUnitsKeywords[TempUnitsType]]);
      S.Add(Line);
    end;
    if WindType = FILE_WINDSPEED then
      Line := 'WINDSPEED FILE    '
    else
    begin
      Line := 'WINDSPEED MONTHLY ';
      for I := 1 to 12 do Line := Line + Tab + WindSpeed[I];
    end;
    S.Add(Line);
    Line := 'SNOWMELT          ';
    for I := 1 to 6 do Line := Line + Tab + SnowMelt[I];
    S.Add(Line);
    Line := 'ADC IMPERVIOUS    ';
    for I := 1 to 10 do Line := Line + Tab + ADCurve[1][I];
    S.Add(Line);
    Line := 'ADC PERVIOUS      ';
    for I := 1 to 10 do Line := Line + Tab + ADCurve[2][I];
    S.Add(Line);
  end;
end;


procedure ExportEvaporation(S: TStringlist);
//-----------------------------------------------------------------------------
var
  Line: String;
  I: Integer;
begin
  with Project.Climatology do
  begin
    S.Add('');
    S.Add('[EVAPORATION]');
    Line := ';;Data Source   ' + Tab + 'Parameters';
    S.Add(Line);
    Line := ';;--------------' + Tab + '----------------';
    S.Add(Line);
    case EvapType of
    CONSTANT_EVAP:
      Line := Format('%-16s', [EvapOptions[CONSTANT_EVAP]]) + Tab + EvapData[0];
    TSERIES_EVAP:
      Line := Format('%-16s', [EvapOptions[TSERIES_EVAP]]) + Tab + EvapTseries;
    FILE_EVAP:
      begin
        Line := Format('%-16s', [EvapOptions[FILE_EVAP]]);
        for I := 0 to 11 do Line := Line + Tab + Format('%-6s', [PanData[I]]);
      end;
    MONTHLY_EVAP:
      begin
        Line := Format('%-16s', [EvapOptions[MONTHLY_EVAP]]);
        for I := 0 to 11 do Line := Line + Tab + Format('%-6s', [EvapData[I]]);
      end;
    TEMP_EVAP:
      Line := 'TEMPERATURE ';
    end;
    S.Add(Line);
    if Length(RecoveryPat) > 0 then S.Add('RECOVERY        ' + Tab + RecoveryPat);
    if EvapDryOnly = True then S.Add('DRY_ONLY        ' + Tab + 'YES')
                          else S.Add('DRY_ONLY        ' + Tab + 'NO');
  end;
end;

procedure ExportSubcatchAdjustments(S: TStringlist; HasClimateAdjustments: Boolean);
//-----------------------------------------------------------------------------
var
  I     : Integer;
  C     : TSubcatch;
  Line  : String;
  Alist : TStringList;
begin
  Alist := TStringList.Create;
  try
    with Project.Lists[SUBCATCH] do
    for I := 0 to Count-1 do
    begin
      C := TSubcatch(Objects[I]);
      if (Length(C.Data[SUBCATCH_N_PERV_PAT_INDEX]) > 0) then
      begin
        Line := 'N-PERV        ' + Tab + Format('%-16s', [C.ID]) +
                                   Tab + C.Data[SUBCATCH_N_PERV_PAT_INDEX];
        AList.Add(Line)
      end;
      if (Length(C.Data[SUBCATCH_DS_PAT_INDEX]) > 0) then
      begin
        Line := 'DSTORE        ' + Tab + Format('%-16s', [C.ID]) +
                                   Tab + C.Data[SUBCATCH_DS_PAT_INDEX];
        Alist.Add(Line)
      end;
      if (Length(C.Data[SUBCATCH_INFIL_PAT_INDEX]) > 0) then
      begin
        Line := 'INFIL         ' + Tab + Format('%-16s', [C.ID]) +
                                   Tab + C.Data[SUBCATCH_INFIL_PAT_INDEX];
        Alist.Add(Line)
      end;
    end;

    if Alist.Count > 0 then
    begin
      if not HasClimateAdjustments then
      begin
        S.Add('');
        S.Add('[ADJUSTMENTS]');
        S.Add(';;Parameter   ' + Tab + 'Subcatchment    ' +
                                 Tab + 'Monthly Adjustments');
      end;
      for I := 0 to Alist.Count-1 do S.Add(Alist[I]);
    end;
  finally
    Alist.Free;
  end;
end;

procedure ExportAdjustments(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I      : Integer;
  Line   : String;
  Value  : String;
  TempAdjusted: Boolean;
  EvapAdjusted: Boolean;
  RainAdjusted: Boolean;
  CondAdjusted: Boolean;
  HasClimateAdjustments: Boolean;
begin
  TempAdjusted := False;
  EvapAdjusted := False;
  RainAdjusted := False;
  CondAdjusted := False;
  HasClimateAdjustments := False;
  with Project.Climatology do
  begin
    for I := 0 to 11 do
    begin
      if Length(TempAdjust[I]) > 0 then TempAdjusted := True;
      if Length(EvapAdjust[I]) > 0 then EvapAdjusted := True;
      if Length(RainAdjust[I]) > 0 then RainAdjusted := True;
      if Length(CondAdjust[I]) > 0 then CondAdjusted := True;
    end;
  end;
  if (TempAdjusted) or (EvapAdjusted) or (RainAdjusted) or (CondAdjusted) then
  begin
    HasClimateAdjustments := True;    S.Add('');
    S.Add('[ADJUSTMENTS]');
    Line := ';;Parameter   ' + Tab + 'Subcatchment    ' +
                               Tab + 'Monthly Adjustments';
    S.Add(Line);
    if TempAdjusted then with Project.Climatology do
    begin
      Line := 'TEMPERATURE   ' + Tab + '            ';
      for I := 0 to 11 do
      begin
        Value := TempAdjust[I];
        if Length(Value) = 0 then Value := '0.0';
        Line := Line + Tab + Format('%-6s', [Value]);
      end;
      S.Add(Line);
    end;
    if EvapAdjusted then with Project.Climatology do
    begin
      Line := 'EVAPORATION   ' + Tab + '                ';
      for I := 0 to 11 do
      begin
        Value := EvapAdjust[I];
        if Length(Value) = 0 then Value := '0.0';
        Line := Line + Tab + Format('%-6s', [Value]);
      end;
      S.Add(Line);
    end;
    if RainAdjusted then with Project.Climatology do
    begin
      Line := 'RAINFALL      ' + Tab + '                ';
      for I := 0 to 11 do
      begin
        Value := RainAdjust[I];
        if Length(Value) = 0 then Value := '1.0';
        Line := Line + Tab + Format('%-6s', [Value]);
      end;
      S.Add(Line);
    end;

    if CondAdjusted then with Project.Climatology do
    begin
      Line := 'CONDUCTIVITY  ' + Tab + '                ';
      for I := 0 to 11 do
      begin
        Value := CondAdjust[I];
        if Length(Value) = 0 then Value := '1.0';
        Line := Line + Tab + Format('%-6s', [Value]);
      end;
      S.Add(Line);
    end;
  end;
  ExportSubcatchAdjustments(S, HasClimateAdjustments);
end;


procedure ExportEvents(S:TStringList);
//-----------------------------------------------------------------------------
var
  I : Integer;
  N : Integer;
  Line: String;
begin
  N := Project.Events.Count;
  if N = 0 then exit;
  S.Add('');
  S.Add('[EVENTS]');
  S.Add(';;Start Date         End Date');
  for I := 0 to N-1 do
  begin
    Line := Project.Events[I];
    S.Add(Line);
  end;
end;


procedure ExportSubcatchments(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I      : Integer;
  Line   : String;
  C      : TSubcatch;
begin
  if Project.Lists[SUBCATCH].Count = 0 then exit;
  S.Add('');
  S.Add('[SUBCATCHMENTS]');
  Line := ';;Name          ' + Tab + 'Rain Gage       ' + Tab + 'Outlet          ';
  Line := Line + Tab + 'Area    ' + Tab + '%Imperv ' + Tab + 'Width   ';
  Line := Line + Tab + '%Slope  ' + Tab + 'CurbLen ' + Tab + 'SnowPack        ';
  S.Add(Line);
  Line := ';;--------------' + Tab + '----------------' + Tab + '----------------';
  Line := Line + Tab + '--------' + Tab + '--------' + Tab + '--------';
  Line := Line + Tab + '--------' + Tab + '--------' + Tab + '----------------';
  S.Add(Line);
  with Project.Lists[SUBCATCH] do
    for I := 0 to Count-1 do
    begin
      C := TSubcatch(Objects[I]);
      ExportComment(S, C.Data[COMMENT_INDEX]);
      Line := Format('%-16s', [Strings[I]]);
      Line := Line + Tab + Format('%-16s', [C.Data[SUBCATCH_RAINGAGE_INDEX]]);
      Line := Line + Tab + Format('%-16s', [C.Data[SUBCATCH_OUTLET_INDEX]]);
      Line := Line + Tab + Format('%-8s',  [C.Data[SUBCATCH_AREA_INDEX]]);
      Line := Line + Tab + Format('%-8s',  [C.Data[SUBCATCH_IMPERV_INDEX]]);
      Line := Line + Tab + Format('%-8s',  [C.Data[SUBCATCH_WIDTH_INDEX]]);
      Line := Line + Tab + Format('%-8s',  [C.Data[SUBCATCH_SLOPE_INDEX]]);
      Line := Line + Tab + Format('%-8s',  [C.Data[SUBCATCH_CURBLENGTH_INDEX]]);
      Line := Line + Tab + Format('%-16s', [C.Data[SUBCATCH_SNOWPACK_INDEX]]);
      S.Add(Line);
    end;
end;


procedure ExportSubAreas(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I     : Integer;
  Line  : String;
  C     : TSubcatch;
begin
  if Project.Lists[SUBCATCH].Count = 0 then exit;
  S.Add('');
  S.Add('[SUBAREAS]');
  Line := ';;Subcatchment  ' + Tab + 'N-Imperv  ' + Tab + 'N-Perv    ';
  Line := Line + Tab + 'S-Imperv  ' + Tab + 'S-Perv    ' + Tab + 'PctZero   ';
  Line := Line + Tab + 'RouteTo   ' + Tab + 'PctRouted ';
  S.Add(Line);
  Line := ';;--------------' + Tab + '----------' + Tab + '----------';
  Line := Line + Tab + '----------' + Tab + '----------' + Tab + '----------';
  Line := Line + Tab + '----------' + Tab + '----------';
  S.Add(Line);
  with Project.Lists[SUBCATCH] do
    for I := 0 to Count-1 do
    begin
      C := TSubcatch(Objects[I]);
      Line := Format('%-16s', [Strings[I]]);
      Line := Line + Tab + Format('%-10s', [C.Data[SUBCATCH_IMPERV_N_INDEX]]);
      Line := Line + Tab + Format('%-10s', [C.Data[SUBCATCH_PERV_N_INDEX]]);
      Line := Line + Tab + Format('%-10s', [C.Data[SUBCATCH_IMPERV_DS_INDEX]]);
      Line := Line + Tab + Format('%-10s', [C.Data[SUBCATCH_PERV_DS_INDEX]]);
      Line := Line + Tab + Format('%-10s', [C.Data[SUBCATCH_PCTZERO_INDEX]]);
      Line := Line + Tab + Format('%-10s', [C.Data[SUBCATCH_ROUTE_TO_INDEX]]);
      if not SameText(C.Data[SUBCATCH_ROUTE_TO_INDEX], 'OUTLET')
      then Line := Line + Tab + Format('%-10s', [C.Data[SUBCATCH_PCT_ROUTED_INDEX]]);
      S.Add(Line);
    end;
end;


procedure ExportInfiltration(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I, J      : Integer;
  InfilType : String;
  Line      : String;
  D         : String;
  C         : TSubcatch;
begin
  if Project.Lists[SUBCATCH].Count = 0 then exit;
  S.Add('');
  S.Add('[INFILTRATION]');
  InfilType := Project.Options.Data[INFILTRATION_INDEX];
  Line := ';;Subcatchment  ' + Tab + 'Param1    ' + Tab + 'Param2    ';
  Line := Line + Tab + 'Param3    ' + Tab + 'Param4    ' + Tab + 'Param5    ';
  S.Add(Line);
  Line := ';;--------------' + Tab + '----------' + Tab + '----------';
  Line := Line + Tab + '----------' + Tab + '----------' + Tab + '----------';
  S.Add(Line);

  with Project.Lists[SUBCATCH] do
    for I := 0 to Count-1 do
    begin
      C := TSubcatch(Objects[I]);
      Line := Format('%-16s', [Strings[I]]);
      for J := 0 to 4 do
      begin
        D := C.InfilData[J];
        if Length(D) = 0 then D := '0';
        Line := Line + Tab + Format('%-10s', [D]);
      end;
       if not SameText(C.Data[SUBCATCH_INFIL_INDEX], InfilType) then
        Line := Line + Tab + C.Data[SUBCATCH_INFIL_INDEX];
      S.Add(Line);
    end;
end;

procedure ExportAquifers(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I    : Integer;
  J    : Integer;
  A    : TAquifer;
  Line : String;
begin
  if Project.Lists[AQUIFER].Count = 0 then exit;
  S.Add('');
  S.Add('[AQUIFERS]');
  Line := ';;Name          ' + Tab + 'Por   ' + Tab + 'WP    ' + Tab + 'FC    ';
  Line := Line + Tab + 'Ksat  ' + Tab + 'Kslope' + Tab + 'Tslope' + Tab + 'ETu   ';
  Line := Line + Tab + 'ETs   ' + Tab + 'Seep  ' + Tab + 'Ebot  ' + Tab + 'Egw   ';
  Line := Line + Tab + 'Umc   ' + Tab + 'ETupat ';
  S.Add(Line);
  Line := ';;--------------';
  for I := 0 to MAXAQUIFERPROPS do Line := Line + Tab + '------';
  S.Add(Line);
  with Project.Lists[AQUIFER] do
  begin
    for I := 0 to Count-1 do
    begin
      A := TAquifer(Objects[I]);
      Line := Format('%-16s',[Strings[I]]);
      for J := 0 to MAXAQUIFERPROPS do
        Line := Line + Tab + Format('%-6s',[A.Data[J]]);
      S.Add(Line);
    end;
  end;
end;


procedure ExportGroundwater(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I, J, N: Integer;
  Line   : String;
  Param  : String;
  C      : TSubcatch;
  OptParams: Boolean;
begin
  if Project.Lists[AQUIFER].Count = 0 then exit;

  S.Add('');
  S.Add('[GROUNDWATER]');
  Line := ';;Subcatchment  ' + Tab + 'Aquifer         ' + Tab + 'Node            ';
  Line := Line + Tab + 'Esurf ' + Tab + 'A1    ' + Tab + 'B1    ' + Tab + 'A2    ';
  Line := Line + Tab + 'B2    ' + Tab + 'A3    ' + Tab + 'Dsw   ' + Tab + 'Egwt  ';
  Line := Line + Tab + 'Ebot  ' + Tab + 'Wgr   ' + Tab + 'Umc   ';
  S.Add(Line);
  Line := ';;--------------' + Tab + '----------------' + Tab + '----------------';
  Line := Line + Tab + '------' + Tab + '------' + Tab + '------' + Tab + '------';
  Line := Line + Tab + '------' + Tab + '------' + Tab + '------' + Tab + '------';
  Line := Line + Tab + '------' + Tab + '------' + Tab + '------';
  S.Add(Line);

  N := 0;
  with Project.Lists[SUBCATCH] do
  begin
    for I := 0 to Count-1 do
    begin
      // Get a reference to Subcatchment I
      C := TSubcatch(Objects[I]);
      // Check that its Goundwater data has at least 2 elements
      if C.Groundwater.Count < 2 then continue;
      // Add the subcatchment's name, the GW aquifer name, and the
      // receiving node's name to the output line
      Line := Format('%-16s', [Strings[I]]);
      Line := Line + Tab + Format('%-16s', [C.Groundwater[0]]);
      Line := Line + Tab + Format('%-16s', [C.Groundwater[1]]);
      // Add the surf. elev, the GW flow coeffs. & surf. water depth to line
      for J := 2 to 8 do
        Line := Line + Tab + Format('%-6s', [C.Groundwater[J]]);

      // Add threshold water table elev. to line
      Param := C.Groundwater[9];
      if Length(Param) = 0 then Param := '*';
      Line := Line + Tab + Format('%-6s', [Param]);

      // Determine if any of the aquifer params. are overridden
      OptParams := False;
      for J := 10 to 12 do
        if Length(C.Groundwater[J]) > 0 then OptParams := True;

      // Write the overridden aquifer params (bottom elev., initial water
      // table elev. & initial upper moisture content) to the line
      if OptParams = True then
      for J := 10 to 12 do
      begin
        Param := C.Groundwater[J];
        if Length(Param) = 0 then Param := '*';
        Line := Line + Tab + Format('%-6s', [Param]);
      end;
      S.Add(Line);

      // Update count of user-supplied GW flow eqns.
      if Length(C.GwLatFlowEqn) > 0 then Inc(N);
      if Length(C.GwDeepFlowEqn) > 0 then Inc(N);

    end;
  end;

  // If the project has user-defined GW flow eqns. then add them to the
  // exported data set
  if N > 0 then
  begin
    S.Add('');
    S.Add('[GWF]');
    Line := ';;Subcatchment  ' + Tab + 'Flow   ' + Tab + 'Equation';
    S.Add(Line);
    Line := ';;--------------' + Tab + '-------' + Tab + '--------';
    S.Add(Line);

    with Project.Lists[SUBCATCH] do
    begin
      for I := 0 to Count-1 do
      begin
        C := TSubcatch(Objects[I]);
        if Length(C.GwLatFlowEqn) > 0 then
        begin
          Line := Format('%-16s', [Strings[I]]);
          Line := Line + Tab + 'LATERAL' + Tab + C.GwLatFlowEqn;
          S.Add(Line);
        end;
        if Length(C.GwDeepFlowEqn) > 0 then
        begin
          Line := Format('%-16s', [Strings[I]]);
          Line := Line + Tab + 'DEEP   ' + Tab + C.GwDeepFlowEqn;
          S.Add(Line);
        end;
      end;
    end;
  end;
end;


procedure ExportSnowpacks(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I    : Integer;
  J    : Integer;
  P    : TSnowpack;
  Line : String;
begin
  if Project.Lists[SNOWPACK].Count = 0 then exit;
  S.Add('');
  S.Add('[SNOWPACKS]');
  S.Add(';;Name          ' + Tab + 'Surface   ' + Tab + 'Parameters');
  S.Add(';;--------------' + Tab + '----------' + Tab + '----------');
  with Project.Lists[SNOWPACK] do
  begin
    for I := 0 to Count-1 do
    begin
      P := TSnowpack(Objects[I]);
      Line := Format('%-16s', [Strings[I]]);
      Line := Line + Tab + 'PLOWABLE  ';
      for J := 1 to 6 do Line := Line + Tab + Format('%-10s', [P.Data[1][J]]);
      Line := Line + Tab + Format('%-10s', [P.FracPlowable]);
      S.Add(Line);
      Line := Format('%-16s', [Strings[I]]);
      Line := Line + Tab + 'IMPERVIOUS';
      for J := 1 to 7 do Line := Line + Tab + Format('%-10s', [P.Data[2][J]]);
      S.Add(Line);
      Line := Format('%-16s', [Strings[I]]);
      Line := Line + Tab + 'PERVIOUS  ';
      for J := 1 to 7 do Line := Line + Tab + Format('%-10s', [P.Data[3][J]]);
      S.Add(Line);
      Line := Format('%-16s', [Strings[I]]);
      Line := Line + Tab + 'REMOVAL   ';
      for J := 1 to 6 do Line := Line + Tab + Format('%-10s', [P.Plowing[J]]);
      Line := Line + Tab + P.Plowing[7];
      S.Add(Line);
    end;
  end;
end;


procedure UpdateNodePropertyCount(N: TNode);
//-----------------------------------------------------------------------------
begin
  if N.DXInflow.Count > 0 then Inc(DXFCount);
  if N.DWInflow.Count > 0 then Inc(DWFCount);
  if N.IIInflow.Count > 0 then Inc(RDIICount);
  if N.Treatment.Count > 0 then Inc(TreatCount);
end;


procedure ExportJunctions(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I     : Integer;
  Line  : String;
  N     : TNode;
begin
  if Project.Lists[JUNCTION].Count = 0 then exit;
  S.Add('');
  S.Add('[JUNCTIONS]');
  Line := ';;Name          ' + Tab + 'Elevation ' + Tab + 'MaxDepth  ';
  Line := Line + Tab + 'InitDepth ' + Tab + 'SurDepth  ' + Tab + 'Aponded   ';
  S.Add(Line);
  Line := ';;--------------' + Tab + '----------' + Tab + '----------';
  Line := Line + Tab + '----------' + Tab + '----------' + Tab + '----------';
  S.Add(Line);
  with Project.Lists[JUNCTION] do
    for I := 0 to Count-1 do
    begin
      N := TNode(Objects[I]);
      ExportComment(S, N.Data[COMMENT_INDEX]);
      UpdateNodePropertyCount(N);
      Line := Format('%-16s', [Strings[I]]);
      Line := Line + Tab + Format('%-10s', [N.Data[NODE_INVERT_INDEX]]);
      Line := Line + Tab + Format('%-10s', [N.Data[JUNCTION_MAX_DEPTH_INDEX]]);
      Line := Line + Tab + Format('%-10s', [N.Data[JUNCTION_INIT_DEPTH_INDEX]]);
      Line := Line + Tab + Format('%-10s', [N.Data[JUNCTION_SURCHARGE_DEPTH_INDEX]]);
      Line := Line + Tab + Format('%-10s', [N.Data[JUNCTION_PONDED_AREA_INDEX]]);
      S.Add(Line);
    end;
end;


procedure ExportOutfalls(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I, J, K: Integer;
  Line   : String;
  N      : TNode;
begin
  if Project.Lists[OUTFALL].Count = 0 then exit;
  S.Add('');
  S.Add('[OUTFALLS]');
  Line := ';;Name          ' + Tab + 'Elevation ' + Tab + 'Type      ';
  Line := Line + Tab + 'Stage Data      ' + Tab + 'Gated   ';
  Line := Line + Tab + 'Route To        ';
  S.Add(Line);
  Line := ';;--------------' + Tab + '----------' + Tab + '----------';
  Line := Line + Tab + '----------------' + Tab + '--------';
  Line := Line + Tab + '----------------';
  S.Add(Line);
  with Project.Lists[OUTFALL] do
    for I := 0 to Count-1 do
    begin
      N := TNode(Objects[I]);
      ExportComment(S, N.Data[COMMENT_INDEX]);
      UpdateNodePropertyCount(N);
      Line := Format('%-16s', [Strings[I]]);
      Line := Line + Tab + Format('%-10s', [N.Data[NODE_INVERT_INDEX]]);
      Line := Line + Tab + Format('%-10s', [N.Data[OUTFALL_TYPE_INDEX]]);
      K := -1;
      for J := 0 to High(OutfallOptions) do
      begin
        if SameText(N.Data[OUTFALL_TYPE_INDEX], OutfallOptions[J]) then
        begin
          K := J;
          break;
        end;
      end;
      case K of
      FIXED_OUTFALL:
        Line := Line + Tab + Format('%-16s', [N.Data[OUTFALL_FIXED_STAGE_INDEX]]);
      TIDAL_OUTFALL:
        Line := Line + Tab + Format('%-16s', [N.Data[OUTFALL_TIDE_TABLE_INDEX]]);
      TIMESERIES_OUTFALL:
        Line := Line + Tab + Format('%-16s', [N.Data[OUTFALL_TIME_SERIES_INDEX]]);
      else Line := Line + Tab + '                ';
      end;
      Line := Line + Tab + Format('%-8s', [N.Data[OUTFALL_TIDE_GATE_INDEX]]);
      Line := Line + Tab + Format('%-16s', [N.Data[OUTFALL_ROUTETO_INDEX]]);
      S.Add(Line);
    end;
end;


procedure ExportDividers(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I, J, K: Integer;
  Line   : String;
  N      : TNode;
begin
  if Project.Lists[DIVIDER].Count = 0 then exit;
  S.Add('');
  S.Add('[DIVIDERS]');
  Line := ';;Name          ' + Tab + 'Elevation ' + Tab + 'Diverted Link   ';
  Line := Line + Tab + 'Type      ' + Tab + 'Parameters';
  S.Add(Line);
  Line := ';;--------------' + Tab + '----------' + Tab + '----------------';
  Line := Line + Tab + '----------' + Tab + '----------';
  S.Add(Line);
  with Project.Lists[DIVIDER] do
    for I := 0 to Count-1 do
    begin
      N := TNode(Objects[I]);
      ExportComment(S, N.Data[COMMENT_INDEX]);
      UpdateNodePropertyCount(N);
      Line := Format('%-16s', [Strings[I]]);
      Line := Line + Tab + Format('%-10s', [N.Data[NODE_INVERT_INDEX]]);
      Line := Line + Tab + Format('%-16s', [N.Data[DIVIDER_LINK_INDEX]]);
      Line := Line + Tab + Format('%-10s', [N.Data[DIVIDER_TYPE_INDEX]]);
      K := -1;
      for J := 0 to High(DividerOptions) do
      begin
        if SameText(N.Data[DIVIDER_TYPE_INDEX], DividerOptions[J]) then
        begin
          K := J;
          break;
        end;
      end;
      case K of
      CUTOFF_DIVIDER:
        Line := Line + Tab + Format('%-10s', [N.Data[DIVIDER_CUTOFF_INDEX]]);
      TABULAR_DIVIDER:
        Line := Line + Tab + Format('%-16s', [N.Data[DIVIDER_TABLE_INDEX]]);
      WEIR_DIVIDER:
        begin
          Line := Line + Tab + Format('%-10s', [N.Data[DIVIDER_QMIN_INDEX]]);
          Line := Line + Tab + Format('%-10s', [N.Data[DIVIDER_DMAX_INDEX]]);
          Line := Line + Tab + Format('%-10s', [N.Data[DIVIDER_QCOEFF_INDEX]]);
        end
      end;
      for J := DIVIDER_MAX_DEPTH_INDEX to DIVIDER_PONDED_AREA_INDEX do
        Line := Line + Tab + Format('%-10s', [N.Data[J]]);
      S.Add(Line);
    end;
end;


procedure ExportStorage(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I    : Integer;
  Line : String;
  N    : TNode;
  X    : Single;
begin
  if Project.Lists[STORAGE].Count = 0 then exit;
  S.Add('');
  S.Add('[STORAGE]');
  Line := ';;Name          ' + Tab + 'Elev.   ' + Tab + 'MaxDepth  ';
  Line := Line + Tab + 'InitDepth ' + Tab + 'Shape     ';
  Line := Line + Tab + 'Curve Type/Params           ';
  Line := Line + Tab + 'SurDepth ' + Tab + 'Fevap   ' + Tab + 'Psi     ';
  Line := Line + Tab + 'Ksat    ' + Tab + 'IMD     ';
  S.Add(Line);
  Line := ';;--------------' + Tab + '--------' + Tab + '----------';
  Line := Line + Tab + '-----------' + Tab + '----------';
  Line := Line + Tab + '----------------------------';
  Line := Line + Tab + '---------' + Tab + '--------' + Tab + '        ';
  Line := Line + Tab + '--------' + Tab + '--------';
  S.Add(Line);
  with Project.Lists[STORAGE] do
    for I := 0 to Count-1 do
    begin
      N := TNode(Objects[I]);
      ExportComment(S, N.Data[COMMENT_INDEX]);
      UpdateNodePropertyCount(N);
      Line := Format('%-16s', [Strings[I]]);
      Line := Line + Tab + Format('%-8s', [N.Data[NODE_INVERT_INDEX]]);
      Line := Line + Tab + Format('%-10s', [N.Data[STORAGE_MAX_DEPTH_INDEX]]);
      Line := Line + Tab + Format('%-10s', [N.Data[STORAGE_INIT_DEPTH_INDEX]]);
      Line := Line + Tab + Format('%-10s', [N.Data[STORAGE_GEOMETRY_INDEX]]);
      if  SameText(N.Data[STORAGE_GEOMETRY_INDEX], 'TABULAR') then
        Line := Line + Tab + Format('%-28s', [N.Data[STORAGE_ATABLE_INDEX]])
      else
      begin
        Line := Line + Tab + Format('%-8s  ', [N.Data[STORAGE_COEFF1_INDEX]]);
        Line := Line + Tab + Format('%-8s  ', [N.Data[STORAGE_COEFF2_INDEX]]);
        Line := Line + Tab + Format('%-8s', [N.Data[STORAGE_COEFF0_INDEX]]);
      end;
      Line := Line + Tab + Format('%-8s ', [N.Data[STORAGE_SURCHARGE_DEPTH_INDEX]]);
      Line := Line + Tab + Format('%-8s', [N.Data[STORAGE_EVAP_FACTOR_INDEX]]);
      Uutils.GetSingle(N.InfilData[STORAGE_KSAT_INDEX ], X);
      if X > 0 then
      begin
        Line := Line + Tab + Format('%-8s', [N.InfilData[STORAGE_SUCTION_INDEX]]);
        Line := Line + Tab + Format('%-8s', [N.InfilData[STORAGE_KSAT_INDEX]]);
        Line := Line + Tab + Format('%-8s', [N.InfilData[STORAGE_IMDMAX_INDEX]]);
      end;
      S.Add(Line);
    end;
end;


procedure ExportConduits(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I     : Integer;
  Line  : String;
  L     : TLink;
begin
  if Project.Lists[CONDUIT].Count = 0 then exit;
  S.Add('');
  S.Add('[CONDUITS]');
  Line := ';;Name          ' + Tab + 'From Node       ' + Tab + 'To Node         ';
  Line := Line + Tab + 'Length    ' + Tab + 'Roughness ' + Tab + 'InOffset  ';
  Line := Line + Tab + 'OutOffset ' + Tab + 'InitFlow  ' + Tab + 'MaxFlow   ';
  S.Add(Line);
  Line := ';;--------------' + Tab + '----------------' + Tab + '----------------';
  Line := Line + Tab + '----------' + Tab + '----------' + Tab + '----------';
  Line := Line + Tab + '----------' + Tab + '----------' + Tab + '----------';
  S.Add(Line);
  with Project.Lists[CONDUIT] do
    for I := 0 to Count-1 do
    begin
      L := TLink(Objects[I]);
      ExportComment(S, L.Data[COMMENT_INDEX]);
      Line := Format('%-16s', [Strings[I]]);
      Line := Line + Tab + Format('%-16s', [L.Node1.ID]);
      Line := Line + Tab + Format('%-16s', [L.Node2.ID]);
      Line := Line + Tab + Format('%-10s', [L.Data[CONDUIT_LENGTH_INDEX]]);
      Line := Line + Tab + Format('%-10s', [L.Data[CONDUIT_ROUGHNESS_INDEX]]);
      Line := Line + Tab + Format('%-10s', [L.Data[CONDUIT_INLET_HT_INDEX]]);
      Line := Line + Tab + Format('%-10s', [L.Data[CONDUIT_OUTLET_HT_INDEX]]);
      Line := Line + Tab + Format('%-10s', [L.Data[CONDUIT_INIT_FLOW_INDEX]]);
      Line := Line + Tab + Format('%-10s', [L.Data[CONDUIT_MAX_FLOW_INDEX]]);
      S.Add(Line);
    end;
end;


procedure ExportPumps(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I     : Integer;
  Line  : String;
  L     : TLink;
begin
  if Project.Lists[PUMP].Count = 0 then exit;
  S.Add('');
  S.Add('[PUMPS]');
  Line := ';;Name          ' + Tab + 'From Node       ' + Tab + 'To Node         ';
  Line := Line + Tab + 'Pump Curve      ' + Tab + 'Status  ';
  Line := Line + Tab + 'Sartup' + Tab + 'Shutoff ';
  S.Add(Line);
  Line := ';;--------------' + Tab + '----------------' + Tab + '----------------';
  Line := Line + Tab + '----------------' + Tab + '------';
  Line := Line + Tab + '--------' + Tab + '--------';
  S.Add(Line);
  with Project.Lists[PUMP] do
    for I := 0 to Count-1 do
    begin
      L := TLink(Objects[I]);
      ExportComment(S, L.Data[COMMENT_INDEX]);
      Line := Format('%-16s', [Strings[I]]);
      Line := Line + Tab + Format('%-16s', [L.Node1.ID]);
      Line := Line + Tab + Format('%-16s', [L.Node2.ID]);
      if Length(L.Data[PUMP_CURVE_INDEX]) = 0 then
        Line := Line + Tab + '*               '
      else
        Line := Line + Tab + Format('%-16s', [L.Data[PUMP_CURVE_INDEX]]);
      Line := Line + Tab + Format('%-8s', [L.Data[PUMP_STATUS_INDEX]]);
      Line := Line + Tab + Format('%-8s', [L.Data[PUMP_STARTUP_INDEX]]);
      Line := Line + Tab + Format('%-8s', [L.Data[PUMP_SHUTOFF_INDEX]]);
      S.Add(Line);
    end;
end;


procedure ExportOrifices(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I     : Integer;
  Line  : String;
  L     : TLink;
begin
  if Project.Lists[ORIFICE].Count = 0 then exit;
  S.Add('');
  S.Add('[ORIFICES]');
  Line := ';;Name          ' + Tab + 'From Node       ' + Tab + 'To Node         ';
  Line := Line + Tab + 'Type        ' + Tab + 'Offset    ' + Tab + 'Qcoeff    ';
  Line := Line + Tab + 'Gated   ' + Tab + 'CloseTime ';
  S.Add(Line);
  Line := ';;--------------' + Tab + '----------------' + Tab + '----------------';
  Line := Line + Tab + '------------' + Tab + '----------' + Tab + '----------';
  Line := Line + Tab + '--------' + Tab + '----------';
  S.Add(Line);
  with Project.Lists[ORIFICE] do
    for I := 0 to Count-1 do
    begin
      L := TLink(Objects[I]);
      ExportComment(S, L.Data[COMMENT_INDEX]);
      Line := Format('%-16s', [Strings[I]]);
      Line := Line + Tab + Format('%-16s', [L.Node1.ID]);
      Line := Line + Tab + Format('%-16s', [L.Node2.ID]);
      Line := Line + Tab + Format('%-12s', [L.Data[ORIFICE_TYPE_INDEX]]);
      Line := Line + Tab + Format('%-10s', [L.Data[ORIFICE_BOTTOM_HT_INDEX]]);
      Line := Line + Tab + Format('%-10s', [L.Data[ORIFICE_COEFF_INDEX]]);
      Line := Line + Tab + Format('%-8s', [L.Data[ORIFICE_FLAPGATE_INDEX]]);
      Line := Line + Tab + Format('%-10s', [L.Data[ORIFICE_ORATE_INDEX]]);
      S.Add(Line);
   end;
end;


procedure ExportWeirs(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I     : Integer;
  Line  : String;
  L     : TLink;
begin
  if Project.Lists[WEIR].Count = 0 then exit;
  S.Add('');
  S.Add('[WEIRS]');
  Line := ';;Name          ' + Tab + 'From Node       ' + Tab + 'To Node         ';
  Line := Line + Tab + 'Type        ' + Tab + 'CrestHt   ' + Tab + 'Qcoeff    ';
  Line := Line + Tab + 'Gated   ' + Tab + 'EndCon  ' + Tab + 'EndCoeff  ';
  Line := Line + Tab + 'Surcharge ';

  Line := Line + Tab + 'RoadWidth ' + Tab + 'RoadSurf  ';
  Line := Line + Tab + 'Coeff. Curve';

  S.Add(Line);
  Line := ';;--------------' + Tab + '----------------' + Tab + '----------------';
  Line := Line + Tab + '------------' + Tab + '----------' + Tab + '----------';
  Line := Line + Tab + '--------' + Tab + '--------' + Tab + '----------';
  Line := Line + Tab + '----------';

  Line := Line + Tab + '----------' + Tab + '----------';
  Line := Line + Tab + '----------------';

  S.Add(Line);
  with Project.Lists[WEIR] do
    for I := 0 to Count-1 do
    begin
      L := TLink(Objects[I]);
      ExportComment(S, L.Data[COMMENT_INDEX]);
      Line := Format('%-16s', [Strings[I]]);
      Line := Line + Tab + Format('%-16s', [L.Node1.ID]);
      Line := Line + Tab + Format('%-16s', [L.Node2.ID]);
      Line := Line + Tab + Format('%-12s', [L.Data[WEIR_TYPE_INDEX]]);
      Line := Line + Tab + Format('%-10s', [L.Data[WEIR_CREST_INDEX]]);
      Line := Line + Tab + Format('%-10s', [L.Data[WEIR_COEFF_INDEX]]);
      Line := Line + Tab + Format('%-8s',  [L.Data[WEIR_FLAPGATE_INDEX]]);
      Line := Line + Tab + Format('%-8s',  [L.Data[WEIR_CONTRACT_INDEX]]);
      Line := Line + Tab + Format('%-10s', [L.Data[WEIR_END_COEFF_INDEX]]);
      Line := Line + Tab + Format('%-10s', [L.Data[WEIR_SURCHARGE_INDEX]]);

      if SameText(L.Data[WEIR_TYPE_INDEX], 'ROADWAY') then
      begin
        Line := Line + Tab + Format('%-10s', [L.Data[WEIR_ROAD_WIDTH_INDEX]]);
        Line := Line + Tab + Format('%-10s', [L.Data[WEIR_ROAD_SURF_INDEX]]);
      end
      else if Length(L.Data[WEIR_COEFF_CURVE_INDEX]) > 0  then
      begin
        Line := Line + Tab + '*         ' + Tab + '*         ' + Tab +
            Format('%-10s', [L.Data[WEIR_COEFF_CURVE_INDEX]]);
      end;
      S.Add(Line);
    end;
end;


procedure ExportOutlets(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I     : Integer;
  Line  : String;
  L     : TLink;
begin
  if Project.Lists[OUTLET].Count = 0 then exit;
  S.Add('');
  S.Add('[OUTLETS]');
  Line := ';;Name          ' + Tab + 'From Node       ' + Tab + 'To Node         ';
  Line := Line + Tab + 'Offset    ' + Tab + 'Type           ';
  Line := Line + Tab + 'QTable/Qcoeff   ' + Tab + 'Qexpon    ' + Tab + 'Gated   ';
  S.Add(Line);
  Line := ';;--------------' + Tab + '----------------' + Tab + '----------------';
  Line := Line + Tab + '----------' + Tab + '---------------';
  Line := Line + Tab + '----------------' + Tab + '----------' + Tab + '--------';
  S.Add(Line);
  with Project.Lists[OUTLET] do
    for I := 0 to Count-1 do
    begin
      L := TLink(Objects[I]);
      ExportComment(S, L.Data[COMMENT_INDEX]);
      Line := Format('%-16s', [Strings[I]]);
      Line := Line + Tab + Format('%-16s', [L.Node1.ID]);
      Line := Line + Tab + Format('%-16s', [L.Node2.ID]);
      Line := Line + Tab + Format('%-10s', [L.Data[OUTLET_CREST_INDEX]]);
      Line := Line + Tab + Format('%-15s', [L.Data[OUTLET_TYPE_INDEX]]);
      if  AnsiContainsText(L.Data[OUTLET_TYPE_INDEX], 'FUNCTIONAL') then
      begin
        Line := Line + Tab + Format('%-16s', [L.Data[OUTLET_QCOEFF_INDEX]]);
        Line := Line + Tab + Format('%-10s', [L.Data[OUTLET_QEXPON_INDEX]]);
      end
      else
      begin
        Line := Line + Tab + Format('%-16s', [L.Data[OUTLET_QTABLE_INDEX]]);
        Line := Line + Tab + '          ';
      end;
      Line := Line + Tab + Format('%-8s', [L.Data[OUTLET_FLAPGATE_INDEX]]);
      S.Add(Line);
    end;
end;


procedure ExportXsections(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I, J  : Integer;
  Line  : String;
  L     : TLink;
begin
  if Project.Lists[CONDUIT].Count + Project.Lists[ORIFICE].Count +
     Project.Lists[WEIR].Count  = 0 then exit;
  S.Add('');
  S.Add('[XSECTIONS]');
  Line := ';;Link          ' + Tab + 'Shape       ' + Tab + 'Geom1           ';
  Line := Line + Tab + 'Geom2     ' + Tab + 'Geom3     ' + Tab + 'Geom4     ';
  Line := Line + Tab + 'Barrels   ' + Tab + 'Culvert   ';
  S.Add(Line);
  Line := ';;--------------' + Tab + '------------' + Tab + '----------------';
  Line := Line + Tab + '----------' + Tab + '----------' + Tab + '----------';
  Line := Line + Tab + '----------' + Tab + '----------';
  S.Add(Line);
  with Project.Lists[CONDUIT] do
    for I := 0 to Count-1 do
    begin
      L := TLink(Objects[I]);
      Line := Format('%-16s', [Strings[I]]);
      Line := Line + Tab + Format('%-12s', [L.Data[CONDUIT_SHAPE_INDEX]]);
      if SameText(L.Data[CONDUIT_SHAPE_INDEX], 'IRREGULAR')
      or SameText(L.Data[CONDUIT_SHAPE_INDEX], 'STREET')
      then Line := Line + Tab + Format('%-16s', [L.Data[CONDUIT_TSECT_INDEX]])
      else begin
        Line := Line + Tab + Format('%-16s', [L.Data[CONDUIT_GEOM1_INDEX]]);
        for J := CONDUIT_GEOM2_INDEX to CONDUIT_BARRELS_INDEX do
          Line := Line + Tab + Format('%-10s', [L.Data[J]]);
        Line := Line + Tab + Format('%-10s', [L.Data[CONDUIT_CULVERT_INDEX]]);
      end;
      S.Add(Line);
    end;
  with Project.Lists[ORIFICE] do
    for I := 0 to Count-1 do
    begin
      L := TLink(Objects[I]);
      Line := Format('%-16s', [Strings[I]]);
      Line := Line + Tab + Format('%-12s', [L.Data[ORIFICE_SHAPE_INDEX]]);
      Line := Line + Tab + Format('%-16s', [L.Data[ORIFICE_HEIGHT_INDEX]]);
      if SameText(L.Data[ORIFICE_SHAPE_INDEX], 'CIRCULAR') then
        Line := Line + Tab + '0         '
      else
        Line := Line + Tab + Format('%-10s', [L.Data[ORIFICE_WIDTH_INDEX]]);
      Line := Line + Tab + '0         ' + Tab + '0';
      S.Add(Line);
    end;
  with Project.Lists[WEIR] do
    for I := 0 to Count-1 do
    begin
      L := TLink(Objects[I]);
      Line := Format('%-16s', [Strings[I]]);
      Line := Line + Tab + Format('%-12s', [L.Data[WEIR_SHAPE_INDEX]]);
      Line := Line + Tab + Format('%-16s', [L.Data[WEIR_HEIGHT_INDEX]]);
      Line := Line + Tab + Format('%-10s', [L.Data[WEIR_WIDTH_INDEX]]);
      Line := Line + Tab + Format('%-10s', [L.Data[WEIR_SLOPE_INDEX]]);
      Line := Line + Tab + Format('%-10s', [L.Data[WEIR_SLOPE_INDEX]]);
      S.Add(Line);
    end;
end;

procedure ExportStreets(S: TStringlist);
var
  I, J: Integer;
  Line: String;
  aStreet: TStreet;
begin
  if Project.Lists[STREET].Count = 0 then exit;
  S.Add('');
  S.Add('[STREETS]');
  Line := ';;Name          ' + Tab + 'Tcrown  ' + Tab + 'Hcurb   ';
  Line := Line + Tab + 'Sx      ' + Tab + 'nRoad   ' + Tab + 'a       ';
  Line := Line + Tab + 'W       ' + Tab + 'Sides   ' + Tab + 'Tback   ';
  Line := Line + Tab + 'Sback   ' + Tab + 'nBack   ';
  ;
  S.Add(Line);
  Line := ';;--------------' + Tab + '--------' + Tab + '--------';
  Line := Line + Tab + '--------' + Tab + '--------' + Tab + '--------';
  Line := Line + Tab + '--------' + Tab + '--------' + Tab + '--------';
  Line := Line + Tab + '--------' + Tab + '--------';
  S.Add(Line);
  with Project.Lists[STREET] do
    for I := 0 to Count-1 do
    begin
      aStreet := TStreet(Objects[I]);
      Line := Format('%-16s', [Strings[I]]);
      for J := 0 to MAXSTREETPROPS do
        Line := Line + Tab + Format('%-8s', [aStreet.Data[J]]);
      S.Add(Line);
    end;
end;

procedure ExportTransects(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I: Integer;
  J: Integer;
  K: Integer;
  L: Integer;
  ID: String;
  Line: String;
  Tsect: TTransect;
begin
  if Project.Lists[TRANSECT].Count = 0 then exit;
  S.Add('');
  S.Add('[TRANSECTS]');
  S.Add(';;Transect Data in HEC-2 format');
  for I := 0 to Project.Lists[TRANSECT].Count-1 do
  begin
    ID := Project.Lists[TRANSECT].Strings[I];
    Tsect := TTransect(Project.Lists[TRANSECT].Objects[I]);
    K := Tsect.Xdata.Count;
    if K = 0 then continue;
    S.Add(';');
    ExportComment(S, Tsect.Comment);

    Line := 'NC';
    Line := Line + Tab + Format('%-8s', [Tsect.Data[TRANSECT_N_LEFT]]);
    Line := Line + Tab + Format('%-8s', [Tsect.Data[TRANSECT_N_RIGHT]]);
    Line := Line + Tab + Format('%-8s', [Tsect.Data[TRANSECT_N_CHANNEL]]);
    S.Add(Line);

    Line := 'X1';
    Line := Line + Tab + Format('%-16s ', [ID]);
    Line := Line + Tab + Format('%-8d', [K]);
    Line := Line + Tab + Format('%-8s', [Tsect.Data[TRANSECT_X_LEFT]]);
    Line := Line + Tab + Format('%-8s', [Tsect.Data[TRANSECT_X_RIGHT]]);
    Line := Line + Tab + '0.0     ' + Tab + '0.0     ';
    Line := Line + Tab + Format('%-8s', [Tsect.Data[TRANSECT_L_FACTOR]]);
    Line := Line + Tab + Format('%-8s', [Tsect.Data[TRANSECT_X_FACTOR]]);
    Line := Line + Tab + Format('%-8s', [Tsect.Data[TRANSECT_Y_FACTOR]]);
    S.Add(Line);

    Line := 'GR';
    L := 0;
    for J := 0 to K-1 do
    begin
      Line := Line + Tab + Format('%-8s', [Tsect.Ydata[J]]);
      Line := Line + Tab + Format('%-8s', [Tsect.Xdata[J]]);
      Inc(L);
      if L = 5 then
      begin
        S.Add(Line);
        Line := 'GR';
        L := 0;
      end;
    end;
    if L > 0 then S.Add(Line);
  end;
end;


procedure ExportLosses(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I     : Integer;
  Line  : String;
  List  : TStringlist;
  L     : TLink;
begin
  if Project.Lists[CONDUIT].Count = 0 then exit;
  List := TStringlist.Create;
  try
    with Project.Lists[CONDUIT] do
    for I := 0 to Count-1 do
    begin
      L := TLink(Objects[I]);
      if  SameText(L.Data[CONDUIT_ENTRY_LOSS_INDEX], '0')
      and SameText(L.Data[CONDUIT_EXIT_LOSS_INDEX], '0')
      and SameText(L.Data[CONDUIT_AVG_LOSS_INDEX], '0')
      and SameText(L.Data[CONDUIT_SEEPAGE_INDEX], '0')
      and SameText(L.Data[CONDUIT_CHECK_VALVE_INDEX], 'NO') then continue;
      Line := Format('%-16s', [Strings[I]]);
      Line := Line + Tab + Format('%-10s', [L.Data[CONDUIT_ENTRY_LOSS_INDEX]]);
      Line := Line + Tab + Format('%-10s', [L.Data[CONDUIT_EXIT_LOSS_INDEX]]);
      Line := Line + Tab + Format('%-10s', [L.Data[CONDUIT_AVG_LOSS_INDEX]]);
      Line := Line + Tab + Format('%-10s', [L.Data[CONDUIT_CHECK_VALVE_INDEX]]);
      Line := Line + Tab + Format('%-10s', [L.Data[CONDUIT_SEEPAGE_INDEX]]);
      List.Add(Line);
    end;
    if List.Count > 0 then
    begin
      S.Add('');
      S.Add('[LOSSES]');
      Line := ';;Link          ' + Tab + 'Kentry    ' + Tab + 'Kexit     ';
      Line := Line + Tab + 'Kavg      ' + Tab + 'Flap Gate ' + Tab + 'Seepage   ';
      S.Add(Line);
      Line := ';;--------------' + Tab + '----------' + Tab + '----------';
      Line := Line + tab + '----------' + Tab + '----------' + Tab + '----------';
      S.Add(Line);
      S.AddStrings(List);
    end;
  finally
    List.Free;
  end;
end;

procedure ExportPollutants(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I      : Integer;
  Line   : String;
  Pollut : TPollutant;
begin
  if Project.Lists[POLLUTANT].Count = 0 then exit;
  S.Add('');
  S.Add('[POLLUTANTS]');
  Line := ';;Name          ' + Tab + 'Units ' + Tab + 'Crain     ';
  Line := Line + Tab + 'Cgw       ' + Tab + 'Crdii     ' + Tab + 'Kdecay    ';
  Line := Line + Tab + 'SnowOnly  ' + Tab + 'Co-Pollutant    ' + Tab + 'Co-Frac   ';
  Line := Line + Tab + 'Cdwf      ' + Tab + 'Cinit     ';
  S.Add(Line);
  Line := ';;--------------' + Tab + '------' + Tab + '----------';
  Line := Line + Tab + '----------' + Tab + '----------' + Tab + '----------';
  Line := Line + Tab + '----------' + Tab + '----------------' + Tab + '----------';
  Line := Line + Tab + '----------' + Tab + '----------';
  S.Add(Line);
  with Project.Lists[POLLUTANT] do
    for I := 0 to Count-1 do
    begin
      Pollut := TPollutant(Objects[I]);
      Line := Format('%-16s',[Strings[I]]);
      Line := Line + Tab + Format('%-6s',[Pollut.Data[POLLUT_UNITS_INDEX]]);
      Line := Line + Tab + Format('%-10s',[Pollut.Data[POLLUT_RAIN_INDEX]]);
      Line := Line + Tab + Format('%-10s',[Pollut.Data[POLLUT_GW_INDEX]]);
      Line := Line + Tab + Format('%-10s',[Pollut.Data[POLLUT_II_INDEX]]);
      Line := Line + Tab + Format('%-10s',[Pollut.Data[POLLUT_DECAY_INDEX]]);
      Line := Line + Tab + Format('%-10s',[Pollut.Data[POLLUT_SNOW_INDEX]]);
      if Length(Trim(Pollut.Data[POLLUT_COPOLLUT_INDEX])) > 0 then
      begin
        Line := Line + Tab + Format('%-16s',[Pollut.Data[POLLUT_COPOLLUT_INDEX]]);
        Line := Line + Tab + Format('%-10s',[Pollut.Data[POLLUT_FRACTION_INDEX]]);
      end
      else Line := Line + Tab + '*               ' + Tab + '0.0       ';
      Line := Line + Tab + Format('%-10s',[Pollut.Data[POLLUT_DWF_INDEX]]);
      Line := Line + Tab + Format('%-10s',[Pollut.Data[POLLUT_INIT_INDEX]]);
      S.Add(Line);
    end;
end;


procedure ExportLanduses(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I      : Integer;
  Line   : String;
  Lnduse : TLanduse;
begin
  if Project.Lists[LANDUSE].Count = 0 then exit;
  S.Add('');
  S.Add('[LANDUSES]');
  Line := ';;              ' + Tab + 'Sweeping  ' + Tab + 'Fraction  ' + Tab + 'Last      ';
  S.Add(Line);
  Line := ';;Name          ' + Tab + 'Interval  ' + Tab + 'Available ' + Tab + 'Swept     ';
  S.Add(Line);
  Line := ';;--------------' + Tab + '----------' + Tab + '----------' + Tab + '----------';
  S.Add(Line);
  with Project.Lists[LANDUSE] do
    for I := 0 to Count-1 do
    begin
      Lnduse := TLanduse(Objects[I]);
      ExportComment(S, Lnduse.Data[COMMENT_INDEX]);
      Line := Format('%-16s',[Strings[I]]);
      Line := Line + Tab + Format('%-10s',[Lnduse.Data[LANDUSE_CLEANING_INDEX]]);
      Line := Line + Tab + Format('%-10s',[Lnduse.Data[LANDUSE_AVAILABLE_INDEX]]);
      Line := Line + Tab + Format('%-10s',[Lnduse.Data[LANDUSE_LASTCLEAN_INDEX]]);
      S.Add(Line);
    end;
end;


procedure ExportBuildup(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I, J, K: Integer;
  Lnduse : String;
  Line   : String;
  npsList: TStringlist;
  nps    : TNonpointSource;
begin
  if Project.Lists[LANDUSE].Count = 0 then exit;
  If Project.Lists[POLLUTANT].Count = 0 then exit;
  S.Add('');
  S.Add('[BUILDUP]');
  Line := ';;Land Use      ' + Tab + 'Pollutant       ' + Tab + 'Function  ';
  Line := Line + Tab + 'Coeff1    ' + Tab + 'Coeff2    ' + Tab + 'Coeff3    ';
  Line := Line + Tab + 'Per Unit  ';
  S.Add(Line);
  Line := ';;--------------' + Tab + '----------------' + Tab + '----------';
  Line := Line + Tab + '----------' + Tab + '----------' + Tab + '----------';
  Line := Line + Tab + '----------';
  S.Add(Line);
  with Project.Lists[LANDUSE] do
  for I := 0 to Count-1 do
  begin
    Lnduse := Strings[I];
    npsList := TLanduse(Objects[I]).NonpointSources;
    for J := 0 to npsList.Count-1 do
    begin
      nps := TNonpointSource(npsList.Objects[J]);
      Line := Format('%-16s', [Lnduse]);
      Line := Line + Tab + Format('%-16s', [npsList[J]]);
      for K := 0 to 4 do
        Line := Line + Tab + Format('%-10s', [nps.BuildupData[K]]);
      S.Add(Line);
    end;
  end;
end;


procedure ExportWashoff(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I, J, K: Integer;
  Lnduse : String;
  Line   : String;
  npsList: TStringlist;
  nps    : TNonpointSource;
begin
  if Project.Lists[LANDUSE].Count = 0 then exit;
  If Project.Lists[POLLUTANT].Count = 0 then exit;
  S.Add('');
  S.Add('[WASHOFF]');
  Line := ';;Land Use      ' + Tab + 'Pollutant       ' + Tab + 'Function  ';
  Line := Line + Tab + 'Coeff1    ' + Tab + 'Coeff2    ' + Tab + 'SweepRmvl ';
  Line := Line + Tab + 'BmpRmvl   ';
  S.Add(Line);
  Line := ';;--------------' + Tab + '----------------' + Tab + '----------';
  Line := Line + Tab + '----------' + Tab + '----------' + Tab + '----------';
  Line := Line + Tab + '----------';
  S.Add(Line);
  with Project.Lists[LANDUSE] do
  for I := 0 to Count-1 do
  begin
    Lnduse := Strings[I];
    npsList := TLanduse(Objects[I]).NonpointSources;
    for J := 0 to npsList.Count-1 do
    begin
      nps := TNonpointSource(npsList.Objects[J]);
      Line := Format('%-16s', [Lnduse]);
      Line := Line + Tab + Format('%-16s', [npsList[J]]);
      for K := 0 to 4 do
        Line := Line + Tab + Format('%-10s', [nps.WashoffData[K]]);
      S.Add(Line);
    end;
  end;
end;


procedure ExportCoverages(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I, J: Integer;
  C: TSubcatch;
  Line: String;
begin
  if Project.Lists[SUBCATCH].Count = 0 then exit;
  if Project.Lists[LANDUSE].Count = 0 then exit;
  S.Add('');
  S.Add('[COVERAGES]');
  Line := ';;Subcatchment  ' + Tab + 'Land Use        ' + Tab + 'Percent   ';
  S.Add(Line);
  Line := ';;--------------' + Tab + '----------------' + Tab + '----------';
  S.Add(Line);
  for I := 0 to Project.Lists[SUBCATCH].Count-1 do
  begin
    C := Project.GetSubcatch(SUBCATCH, I);
    for J := 0 to C.Landuses.Count-1 do
    begin
      Line := Format('%-16s', [C.ID]);
      Line := Line + Tab + Format('%-16s', [C.Landuses.Names[J]]);
      Line := Line + Tab + Format('%-10s', [C.Landuses.ValueFromIndex[J]]);
      S.Add(Line);
    end;
  end;
end;


procedure ExportLoadings(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I, J: Integer;
  C:    TSubcatch;
  Line: String;
begin
  if Project.Lists[SUBCATCH].Count = 0 then exit;
  if Project.Lists[POLLUTANT].Count = 0 then exit;
  S.Add('');
  S.Add('[LOADINGS]');
  Line := ';;Subcatchment  ' + Tab + 'Pollutant       ' + Tab + 'Buildup   ';
  S.Add(Line);
  Line := ';;--------------' + Tab + '----------------' + Tab + '----------';
  S.Add(Line);
  for I := 0 to Project.Lists[SUBCATCH].Count-1 do
  begin
    C := Project.GetSubcatch(SUBCATCH, I);
    for J := 0 to C.Loadings.Count-1 do
    begin
      Line := Format('%-16s', [C.ID]);
      Line := Line + Tab + Format('%-16s', [C.Loadings.Names[J]]);
      Line := Line + Tab + Format('%-10s', [C.Loadings.ValueFromIndex[J]]);
      S.Add(Line);
    end;
  end;
end;


procedure ExportTreatment(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I, J   : Integer;
  K, N   : Integer;
  Pollut : String;
  Func   : String;
  Line   : String;
  aNode  : TNode;
begin
  if TreatCount = 0 then exit;
  S.Add('');
  S.Add('[TREATMENT]');
  Line := ';;Node          ' + Tab + 'Pollutant       ' + Tab + 'Function  ';
  S.Add(Line);
  Line := ';;--------------' + Tab + '----------------' + Tab + '----------';
  S.Add(Line);
  for I := JUNCTION to STORAGE do
  begin
    N := Project.Lists[I].Count - 1;
    for J := 0 to N do
    begin
      aNode := Project.GetNode(I, J);
      with aNode.Treatment do
      begin
        for K := 0 to Count-1 do
        begin
          Func := ValueFromIndex[K];
          if Length(Func) = 0 then continue;
          Pollut := Names[K];
          Line := Format('%-16s',[aNode.ID]);
          Line := Line + Tab + Format('%-16s', [Pollut]);
          Line := Line + Tab + Format('%s', [Func]);
          S.Add(Line);
        end;
      end;
    end;
  end;
end;


procedure ExportInflows(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I, J   : Integer;
  K, M   : Integer;
  Line   : String;
  Param  : String;
  TS     : String;
  Slist  : TStringlist;
  aNode  : TNode;
begin

  if DXFCount = 0 then exit;
  Slist := TStringlist.Create;
  try
    S.Add('');
    S.Add('[INFLOWS]');
    Line := ';;Node          ' + Tab + 'Constituent     ' + Tab + 'Time Series     ';
    Line := Line + Tab + 'Type    ' + Tab + 'Mfactor ' + Tab + 'Sfactor ';
    Line := Line + Tab + 'Baseline' + Tab + 'Pattern';
    S.Add(Line);
    Line := ';;--------------' + Tab + '----------------' + Tab + '----------------';
    Line := Line + Tab + '--------' + Tab + '--------' + Tab + '--------';
    Line := Line + Tab + '--------' + Tab + '--------';
    S.Add(Line);
    for I := JUNCTION to STORAGE do
    begin
      for J := 0 to Project.Lists[I].Count - 1 do
      begin
        aNode := Project.GetNode(I, J);
        with aNode.DXInflow do
        begin
          for K := 0 to Count-1 do
          begin
            Param := Names[K];
            Slist.Clear;
            Slist.SetText(PChar(ValueFromIndex[K]));
            TS := Slist[0];
            if Length(TS) = 0 then TS := '""';
            Line := Format('%-16s', [aNode.ID]);
            Line := Line + Tab + Format('%-16s', [Param]);
            Line := Line + Tab + Format('%-16s',[TS]);
            for M := 1 to Slist.Count-1 do
              Line := Line + Tab + Format('%-8s', [Slist[M]]);
            S.Add(Line);
          end;
        end;
      end;
    end;
  finally
    Slist.Free;
  end;
end;


procedure ExportDWflows(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I, J      : Integer;
  K, M      : Integer;
  Line      : String;
  Param     : String;
  Slist     : TStringlist;
  aNode     : TNode;
begin
  if DWFCount = 0 then exit;
  Slist := TStringlist.Create;
  try
    S.Add('');
    S.Add('[DWF]');
    Line := ';;Node          ' + Tab + 'Constituent     ' + Tab + 'Baseline  ';
    Line := Line + Tab + 'Patterns  ';
    S.Add(Line);
    Line := ';;--------------' + Tab + '----------------' + Tab + '----------';
    Line := Line + Tab + '----------';
    S.Add(Line);
    for I := JUNCTION to STORAGE do
    begin
      for J := 0 to Project.Lists[I].Count - 1 do
      begin
        aNode := Project.GetNode(I, J);
        with aNode.DWInflow do
        begin
          for K := 0 to Count-1 do
          begin
            Param := Names[K];
            Slist.Clear;
            Slist.SetText(PChar(ValueFromIndex[K]));
            Line := Format('%-16s', [aNode.ID]);
            Line := Line + Tab + Format('%-16s', [Param]);
            Line := Line + Tab + Format('%-10s', [Slist[0]]);
            for M := 1 to Slist.Count-1 do
              Line := Line + Tab + '"' + Trim(Slist[M]) + '"';
            S.Add(Line);
          end;
        end;
      end;
    end;
  finally
    Slist.Free;
  end;
end;


procedure ExportIIflows(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I, J, N   : Integer;
  Line      : String;
  aNode     : TNode;
begin
  if RDIICount = 0 then exit;
  S.Add('');
  S.Add('[RDII]');
  Line := ';;Node          ' + Tab + 'Unit Hydrograph ' + Tab + 'Sewer Area';
  S.Add(Line);
  Line := ';;--------------' + Tab + '----------------' + Tab + '----------';
  S.Add(Line);
  for I := JUNCTION to STORAGE do
  begin
    N := Project.Lists[I].Count - 1;
    for J := 0 to N do
    begin
      aNode := Project.GetNode(I, J);
      if aNode.IIInflow.Count >= 2 then
      begin
        Line := Format('%-16s', [aNode.ID]);
        Line := Line + Tab + Format('%-16s', [aNode.IIInflow[0]]);
        Line := Line + Tab + Format('%-10s', [aNode.IIInflow[1]]);
        S.Add(Line);
      end;
    end;
  end;
end;


procedure ExportPatterns(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I, J: Integer;
  K, K1, K2: Integer;
  Line: String;
  aPattern: TPattern;
begin
  if Project.Lists[PATTERN].Count = 0 then exit;
  S.Add('');
  S.Add('[PATTERNS]');
  Line := ';;Name          ' + Tab + 'Type      ' + Tab + 'Multipliers';
  S.Add(Line);
  Line := ';;--------------' + Tab + '----------' + Tab + '-----------';
  S.Add(Line);
  with Project.Lists[PATTERN] do
  begin
    for I := 0 to Count-1 do
    begin
      aPattern := TPattern(Objects[I]);
      ExportComment(S, aPattern.Comment);
      Line := Format('%-16s', [Strings[I]]);
      Line := Line + Tab + Format('%-10s', [PatternTypes[aPattern.PatternType]]);
      if aPattern.PatternType = PATTERN_DAILY then K := 7 else K := 6;
      K2 := K;
      for J := 0 to K2-1 do
        Line := Line + Tab + Format('%-5s', [aPattern.Data[J]]);
      S.Add(Line);
      while K2 < aPattern.Count do
      begin
        K1 := K2;
        K2 := K1 + K;
        if K2 > aPattern.Count then K2 := aPattern.Count;
        Line := Format('%-16s', [Strings[I]]) + Tab + '          ';
        for J := K1 to K2-1 do
          Line := Line + Tab + Format('%-5s', [aPattern.Data[J]]);
        S.Add(Line);
      end;
      if I < Count-1 then S.Add(';');
    end;
  end;
end;


procedure ExportTimeseries(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I, J, N : Integer;
  Name    : String;
  Fname   : String;
  Line    : String;
  Tseries : TTimeseries;
begin
  if Project.Lists[TIMESERIES].Count = 0 then exit;
  S.Add('');
  S.Add('[TIMESERIES]');
  Line := ';;Name          ' + Tab + 'Date      ' + Tab + 'Time      ' + Tab + 'Value     ';
  S.Add(Line);
  Line := ';;--------------' + Tab + '----------' + Tab + '----------' + Tab + '----------';
  S.Add(Line);
  with Project.Lists[TIMESERIES] do
    for I := 0 to Count-1 do
    begin
      Name := Strings[I];
      Tseries := TTimeseries(Objects[I]);
      ExportComment(S, Tseries.Comment);
      if Length(Tseries.Filename) > 0 then
      begin
        Fname := '"' + RelativePathName(Tseries.Filename) + '"';
        Line := Format('%-16s', [Name]) + Tab + 'FILE' + Tab + Fname;
        S.Add(Line);
      end
      else
      begin
        N := MinIntValue([Tseries.Times.Count, Tseries.Values.Count]);
        for J := 0 to N-1 do
        begin
          Line := Format('%-16s', [Name]);
          Line := Line + Tab + Format('%-10s', [Tseries.Dates[J]]);
          Line := Line + Tab + Format('%-10s', [Tseries.Times[J]]);
          Line := Line + Tab + Format('%-10s', [Tseries.Values[J]]);
          S.Add(Line);
        end;
      end;
      if I < Count-1 then S.Add(';');
    end;
end;


procedure ExportCurves(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I,J,K,N,M : Integer;
  Line    : String;
  Name    : String;
  aCurve  : TCurve;
begin
  if Project.GetCurveCount = 0 then exit;
  S.Add('');
  S.Add('[CURVES]');
  Line := ';;Name          ' + Tab + 'Type      ' + Tab + 'X-Value   ' + Tab + 'Y-Value   ';
  S.Add(Line);
  Line := ';;--------------' + Tab + '----------' + Tab + '----------' + Tab + '----------';
  S.Add(Line);
  M := 0;
  for I := 0 to MAXCLASS do
  begin
    if Project.IsCurve(I) then with Project.Lists[I] do
    begin
      for J := 0 to Count-1 do
      begin
        Name := Strings[J];
        aCurve := TCurve(Objects[J]);
        if M = 0 then M := 1 else S.Add(';');
        ExportComment(S, aCurve.Comment);
        N := MinIntValue([aCurve.Xdata.Count, aCurve.Ydata.Count]);
        if N > 0 then
        begin
          Line := Format('%-16s', [Name]) + Tab +
                  Format('%-10s', [aCurve.CurveType]) + Tab +
                  Format('%-10s', [aCurve.Xdata[0]]) + Tab +
                  Format('%-10s', [aCurve.Ydata[0]]);
          S.Add(Line);
          for K := 1 to N-1 do
          begin
            Line := Format('%-16s', [Name]) + Tab + '          ' + Tab +
                    Format('%-10s', [aCurve.Xdata[K]]) + Tab +
                    Format('%-10s', [aCurve.Ydata[K]]);
            S.Add(Line);
          end;
        end;
      end;
    end;
  end;
end;


procedure ExportControls(S: TStringlist);
//-----------------------------------------------------------------------------
begin
  if Project.ControlRules.Count = 0 then Exit;
  S.Add('');
  S.Add('[CONTROLS]');
  S.AddStrings(Project.ControlRules);
end;


procedure ExportReport(S: TStringlist);
//-----------------------------------------------------------------------------
begin
  S.Add('');
  S.Add('[REPORT]');
  S.Add(';;Reporting Options');
  ReportingForm.Export(S, Tab);
end;


procedure ExportFiles(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I, N: Integer;
  Fname: String;
  Line: String;
  TokList : TStringlist;
  Ntoks   : Integer;
begin
  N := Project.IfaceFiles.Count;
  if N = 0 then Exit;
  S.Add('');
  S.Add('[FILES]');
  S.Add(';;Interfacing Files');
  TokList := TStringList.Create;
  try
    for I := 0 to N-1 do
    begin
      Uutils.Tokenize(Project.IfaceFiles[I], TokList, Ntoks);
      if Ntoks < 3 then continue;
      Fname := RelativePathName(TokList[2]);
      Line := TokList[0] + Tab + TokList[1] + Tab + '"' + Fname + '"';
      S.Add(Line);
    end;
  finally
    TokList.Free;
  end;
end;


procedure ExportProfiles(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I, M, N   : Integer;
  aID       : String;
  Line      : String;
  LinksList : TStringlist;
begin
  if Project.ProfileNames.Count = 0 then exit;
  if Uglobals.TabDelimited then Tab := #9 else Tab := ' ';
  S.Add('');
  S.Add('[PROFILES]');
  Line := ';;Name          ' + Tab + 'Links     ';
  S.Add(Line);
  Line := ';;--------------' + Tab + '----------';
  S.Add(Line);
  LinksList := TStringlist.Create;
  with Project do
  try
    for I := 0 to ProfileNames.Count-1 do
    begin
      aID := ProfileNames[I];
      LinksList.Clear;
      LinksList.SetText(PChar(ProfileLinks[I]));
      N := LinksList.Count;
      M := 0;
      Line := '';
      while M < N do
      begin
        if (M mod 5) = 0 then
        begin
          if M > 0 then S.Add(Line);
          Line := Format('"%-16s"', [aID]);
        end;
        Line := Line + Tab + LinksList[M];
        Inc(M);
      end;
      S.Add(Line);
    end;
  finally
    LinksList.Free;
  end;
end;


procedure ExportTags(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I: Integer;
  J: Integer;
  Line: String;
begin
  if Uglobals.TabDelimited then Tab := #9 else Tab := ' ';
  S.Add('');
  S.Add('[TAGS]');
  for J := 0 to Project.Lists[RAINGAGE].Count-1 do
  begin
    with Project.Lists[RAINGAGE].Objects[J] as TRaingage do
      if Length(Data[TAG_INDEX]) > 0 then
      begin
        Line := 'Gage      ' + Tab + Format('%-16s', [ID]) + Tab +
                Format('%-16s',[Data[TAG_INDEX]]);
        S.Add(Line);
      end;
  end;
  for J := 0 to Project.Lists[SUBCATCH].Count-1 do
  begin
    with Project.Lists[SUBCATCH].Objects[J] as TSubcatch do
      if Length(Data[TAG_INDEX]) > 0 then
      begin
        Line := 'Subcatch  ' + Format('%-16s', [ID]) + Tab +
                Format('%-16s', [Data[TAG_INDEX]]);
        S.Add(Line);
      end;
  end;
  for I := JUNCTION to STORAGE do
  begin
    for J := 0 to Project.Lists[I].Count-1 do
    begin
      with Project.GetNode(I, J) do
        if Length(Data[TAG_INDEX]) > 0 then
        begin
          Line := 'Node      ' + Tab + Format('%-16s', [ID]) + Tab +
                  Format('%-16s', [Data[TAG_INDEX]]);
          S.Add(Line);
        end;
    end;
  end;
  for I := CONDUIT to OUTLET do
  begin
    for J := 0 to Project.Lists[I].Count-1 do
    begin
      with Project.GetLink(I, J) do
        if Length(Data[TAG_INDEX]) > 0 then
        begin
          Line := 'Link      ' + Tab + Format('%-16s', [ID]) + Tab +
                  Format('%-16s', [Data[TAG_INDEX]]);
          S.Add(Line);
        end;
    end;
  end;
end;


procedure ExportMap(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I, J, N, D: Integer;
  aID       : String;
  Line      : String;
  Fmt       : String;
  Fname     : String;
  TrueFalse : Integer;                                                          
  Slist     : TStringlist;
  aGage     : TRainGage;
  aNode     : TNode;
  aVertex   : PVertex;
  aMapLabel : TMapLabel;
begin
  // Export map's dimensions
  if Uglobals.TabDelimited then Tab := #9 else Tab := ' ';
  S.Add('');
  S.Add('[MAP]');
  with MapForm.Map.Dimensions do
  begin
    D := Digits;
    Fmt := '%-18.' + IntToStr(Digits) + 'f';
    Line := 'DIMENSIONS' + Tab +
            FloatToStrF(LowerLeft.X,ffFixed,18,D) + Tab +
            FloatToStrF(LowerLeft.Y,ffFixed,18,D) + Tab +
            FloatToStrF(UpperRight.X,ffFixed,18,D) + Tab +
            FloatToStrF(UpperRight.Y,ffFixed,18,D);
    S.Add(Line);
    Line := 'Units     ' + Tab + MapUnits[Ord(Units)];
    S.Add(Line);
  end;

  // Export nodal coordinates
  S.Add('');
  S.Add('[COORDINATES]');
  Line := ';;Node          ' + Tab + 'X-Coord           ' + Tab + 'Y-Coord           ';
  S.Add(Line);
  Line := ';;--------------' + Tab + '------------------' + Tab + '------------------';
  S.Add(Line);
  for I := JUNCTION to STORAGE do
  begin
    Slist := Project.Lists[i];
    N := Slist.Count - 1;
    for J := 0 to N do
    begin
      aNode := Project.GetNode(I, J);
      with aNode do
        if (X <> MISSING) and (Y <> MISSING) then
        begin
          Line := Format('%-16s', [Slist[J]]) + Tab +
                  Format(Fmt, [X]) + Tab +
                  Format(Fmt, [Y]);
          S.Add(Line);
        end;
    end;
  end;
  S.Add('');

  // Export link vertex coordinates
  S.Add('[VERTICES]');
  Line := ';;Link          ' + Tab + 'X-Coord           ' + Tab + 'Y-Coord           ';
  S.Add(Line);
  Line := ';;--------------' + Tab + '------------------' + Tab + '------------------';
  S.Add(Line);
  for I := CONDUIT to OUTLET do
  begin
    Slist := Project.Lists[I];
    for J := 0 to Slist.Count - 1 do
    begin
      aVertex := Project.GetLink(I, J).Vlist.First;
      while aVertex <> nil do
      begin
        Line := Format('%-16s', [Slist[J]]) + Tab +
                Format(Fmt, [aVertex^.X]) + Tab +
                Format(Fmt, [aVertex^.Y]);
        S.Add(Line);
        aVertex := aVertex^.Next;
      end;
    end;
  end;
  S.Add('');

  // Export vertex coordinates of subcatchment polygons
  S.Add('[Polygons]');
  Slist := Project.Lists[SUBCATCH];
  N := Slist.Count - 1;
  if N >= 0 then
  begin
    Line := ';;Subcatchment  ' + Tab + 'X-Coord           ' + Tab + 'Y-Coord           ';
    S.Add(Line);
    Line := ';;--------------' + Tab + '------------------' + Tab + '------------------';
    S.Add(Line);
    for J := 0 to N do
    begin
      aVertex := Project.GetSubcatch(SUBCATCH, J).Vlist.First;
      while aVertex <> nil do
      begin
        Line := Format('%-16s', [Slist[J]]) + Tab +
                Format(Fmt, [aVertex^.X]) + Tab +
                Format(Fmt, [aVertex^.Y]);
        S.Add(Line);
        aVertex := aVertex^.Next;
      end;
    end;
    S.Add('');
  end;

  // Export vertex coordinates of storage node polygons
  Slist := Project.Lists[STORAGE];
  N := Slist.Count - 1;
  if N >= 0 then
  begin
    Line := ';;Storage Node  ' + Tab + 'X-Coord           ' + Tab + 'Y-Coord           ';
    S.Add(Line);
    Line := ';;--------------' + Tab + '------------------' + Tab + '------------------';
    S.Add(Line);
    for J := 0 to N do
    begin
      aVertex := Project.GetNode(Storage, J).Vlist.First;
      while aVertex <> nil do
      begin
        Line := Format('%-16s', [Slist[J]]) + Tab +
                Format(Fmt, [aVertex^.X]) + Tab +
                Format(Fmt, [aVertex^.Y]);
        S.Add(Line);
        aVertex := aVertex^.Next;
      end;
    end;
    S.Add('');
  end;

  // Export rain gage coordinates
  Slist := Project.Lists[RAINGAGE];
  N := Slist.Count - 1;
  if N >= 0 then
  begin
    S.Add( '[SYMBOLS]');
    Line := ';;Gage          ' + Tab + 'X-Coord           ' + Tab + 'Y-Coord           ';
    S.Add(Line);
    Line := ';;--------------' + Tab + '------------------' + Tab + '------------------';
    S.Add(Line);
    for J := 0 to N do
    begin
      aGage := Project.GetGage(J);
      with aGage do
        if (X <> MISSING) and (Y <> MISSING) then
        begin
          Line := Format('%-16s', [Slist[J]]) + Tab +
                  Format(Fmt, [X]) + Tab +
                  Format(Fmt, [Y]);
          S.Add(Line);
        end;
    end;
    S.Add('');
  end;

  // Export map label coordinates
  Slist := Project.Lists[MAPLABEL];
  N := Slist.Count - 1;
  if N >= 0 then
  begin
    S.Add( '[LABELS]');
    Line := ';;X-Coord         ' + Tab + 'Y-Coord           ' + Tab + 'Label           ';
    S.Add(Line);
    for J := 0 to N do
    begin
      aMapLabel := Project.GetMapLabel(J);
      with aMapLabel do
      begin
        aID := '""';
        if Anchor <> nil then aID := Anchor.ID;
        Line := Format(Fmt, [X]) + Tab +
                Format(Fmt, [Y]) + Tab + '"' + Slist[J] + '"' +
                Tab + aID + Tab + '"' + FontName + '"' + Tab +
                Format('%d', [FontSize]);
        if FontBold then TrueFalse := 1 else TrueFalse := 0;
        Line := Line + Tab + IntToStr(TrueFalse);
        if FontItalic then TrueFalse := 1 else TrueFalse := 0;
        Line := Line + Tab + IntToStr(TrueFalse);
        S.Add(Line);
      end;
    end;
    S.Add('');
  end;

  // Export backdrop image information
  with MapForm.Map.Backdrop do
  begin
    if Length(Filename) > 0 then
    begin
      Fname := RelativePathName(Filename);
      S.Add('');
      S.Add('[BACKDROP]');
      S.Add('FILE      ' + Tab + '"' + Fname + '"');
      Line := 'DIMENSIONS' + Tab +
              FloatToStrF(LowerLeft.X,ffFixed,18,D) + Tab +
              FloatToStrF(LowerLeft.Y,ffFixed,18,D) + Tab +
              FloatToStrF(UpperRight.X,ffFixed,18,D) + Tab +
              FloatToStrF(UpperRight.Y,ffFixed,18,D);
      S.Add(Line);
    end;
  end;

end;


procedure ExportTempDir(S: TStringlist);
//-----------------------------------------------------------------------------
begin
  if Uglobals.TabDelimited then Tab := #9 else Tab := ' ';
  S.Add('');
  S.Add('[OPTIONS]');
  S.Add('TEMPDIR   ' + Tab + '"' + TempDir + '"');
end;


procedure ExportProject(S: TStringlist; P: String);
//-----------------------------------------------------------------------------
begin
  SaveToPath := P;
  if Uglobals.TabDelimited then Tab := #9 else Tab := ' ';
  DXFCount    := 0;
  DWFCount    := 0;
  RDIICount   := 0;
  TreatCount  := 0;

  ExportTitle(S);
  ExportOptions(S);
  ExportFiles(S);
  ExportEvaporation(S);
  ExportTemperature(S);
  ExportRaingages(S);
  ExportSubcatchments(S);
  ExportSubAreas(S);
  ExportInfiltration(S);
  Ulid.ExportLIDs(S, Tab);
  Ulid.ExportLIDGroups(S, Tab);
  ExportAquifers(S);
  ExportGroundwater(S);
  ExportSnowpacks(S);

  //*****************************************************
  // These sections must be exported in the order as shown
  //******************************************************
  ExportJunctions(S);
  ExportOutfalls(S);
  ExportDividers(S);
  ExportStorage(S);
  ExportConduits(S);
  ExportPumps(S);
  ExportOrifices(S);
  ExportWeirs(S);
  ExportOutlets(S);
  //******************************************************

  ExportXsections(S);
  ExportStreets(S);
  Uinlet.ExportInletDesigns(S);
  Uinlet.ExportInletUsage(S);
  ExportTransects(S);
  ExportLosses(S);
  ExportControls(S);
  ExportPollutants(S);
  ExportLanduses(S);
  ExportCoverages(S);
  ExportLoadings(S);
  ExportBuildup(S);
  ExportWashoff(S);
  ExportTreatment(S);
  ExportInflows(S);
  ExportDWflows(S);
  ExportHydrographs(S);
  ExportIIflows(S);
  ExportCurves(S);
  ExportTimeseries(S);
  ExportPatterns(S);
  ExportReport(S);
  ExportAdjustments(S);
  ExportEvents(S);
end;


function  RelativePathName(Fname: String): String;
//-----------------------------------------------------------------------------
begin
  if Pos(SaveToPath, Fname) = 1 then
    Result := ExtractRelativePath(SaveToPath, Fname)
  else
    Result := Fname;
end;


function SaveOutput(Fname: String): Boolean;
//-----------------------------------------------------------------------------
var
  F1: String;
  F2: String;
begin
  // Make sure that report & output file don't have same name as input file
  Result := False;
  F1 := ChangeFileExt(Fname, '.rpt');
  F2 := ChangeFileExt(Fname, '.out');
  if (SameText(F1, Fname)) or (SameText(F2, Fname)) then Exit;

  // Exit if current temporary files have same names as permanent ones
  if (SameText(F1, TempReportFile) and SameText(F2, TempOutputFile)) then
  begin
    Result := True;
    Exit;
  end;

  // Rename current temporary report file
  DeleteFile(PChar(F1));
  if not RenameFile(TempReportFile, F1) then Exit;

  // Close & rename current temporary output file
  Uoutput.CloseOutputFile;
  DeleteFile(PChar(F2));
  if not RenameFile(TempOutputFile, F2) then Exit;

  // Reopen output file & clear temporary file names
  Uoutput.OpenOutputFile(F2);
  TempReportFile := '';
  TempOutputFile := '';
  Result := True;
end;


procedure SaveProject(Fname: String);
//-----------------------------------------------------------------------------
var
  S: TStringlist;
begin
  SaveToPath := ExtractFilePath(Fname);
  S := TStringlist.Create;
  try
    ExportProject(S, SaveToPath);
    S.AddStrings(Project.Options.Report);
    ExportTags(S);
    ExportMap(S);
    ExportProfiles(S);
    S.SaveToFile(Fname);
  finally
    S.Free;
  end;
  SaveToPath := '';
  if CompareText(ExtractFileExt(Fname), '.ini') <> 0
  then Uinifile.SaveProjIniFile(ChangeFileExt(Fname, '.ini'));
end;

procedure SaveResults(Fname: String);
//-----------------------------------------------------------------------------
begin
  Uglobals.ResultsSaved := SaveOutput(Fname);
  if CompareText(ExtractFileExt(Fname), '.ini') <> 0
  then Uinifile.SaveProjIniFile(ChangeFileExt(Fname, '.ini'));
end;

procedure SaveHotstartFile(Fname: String);
//-----------------------------------------------------------------------------
const
  //Conversion factors for CFS, GPM, MGD, CMS, LPS, & MLD to CFS
  QCFs: array[0..5] of Single =
    (1.0, 448.831, 0.64632, 0.02832, 28.317, 2.4466);
var
  F: File;
  I, J, K, N: Integer;
  P: Integer;
  V: Integer;
  X: Single;
  S: String;
  QCF: Single;
  LCF: Single;

begin
  // Open a binary output file
  AssignFile(F, Fname);
  try
    Rewrite(F, 1);

    // Determine flow (QCF) and length (LCF) conversion factors
    QCF := QCFs[Uglobals.Qunits];
    if Uglobals.Qunits > 2 then LCF := Uglobals.METERSperFOOT
    else LCF := 1.0;

    // Write title & number of components
    S := 'SWMM5-HOTSTART2';
    N := Length(S);
    for I := 1 to N do BlockWrite(F, S[I], 1);
    N := Project.Lists[SUBCATCH].Count;
    BlockWrite(F, N, SizeOf(N));
    N := Project.GetNodeCount;
    BlockWrite(F, N, SizeOf(N));
    N := Project.GetLinkCount;
    BlockWrite(F, N, SizeOf(N));
    N := Project.Lists[POLLUTANT].Count;
    BlockWrite(F, N, SizeOf(N));
    BlockWrite(F, Qunits, SizeOf(Qunits));

    // Write GW state for each subcatchment at current time period
    for J := 0 to Project.Lists[SUBCATCH].Count-1 do
    begin
      // If no output results available then write MISSING value to file
      K := Project.GetSubcatch(SUBCATCH, J).OutFileIndex;
      if K < 0 then
      begin
        X := MISSING;
        BlockWrite(F, X, SizeOf(X));
        BlockWrite(F, X, SizeOf(X));
      end

      // Otherwise retrieve upper zone theta & water table elev. from output file
      else begin
        V := GetVarIndex(GW_MOIST, SUBCATCHMENTS);
        X := Uoutput.GetSubcatchOutVal(V, CurrentPeriod, K);
        BlockWrite(F, X, SizeOf(X));
        V := GetVarIndex(GW_ELEV, SUBCATCHMENTS);
        X := Uoutput.GetSubcatchOutVal(V, CurrentPeriod, K)/LCF;
        BlockWrite(F, X, SizeOf(X));
      end;
    end;

    // Write selected results for each node at current time period
    for I := 0 to MAXCLASS do
    begin
      if Project.IsNode(I) then for J := 0 to Project.Lists[I].Count-1 do
      begin

        // If no output results for node then write defaults to file
        K := Project.GetNode(I, J).OutFileIndex;
        if K < 0 then
        begin
          X := 0.0;
          BlockWrite(F, X, SizeOf(X));
          BlockWrite(F, X, SizeOf(X));
          for P := 0 to N-1 do BlockWrite(F, X, SizeOf(X));
        end

        // Otherwise retrieve hotstart values from output file
        else
        begin

          // Write node depth to file
          V := GetVarIndex(NODEDEPTH, NODES);
          X := Uoutput.GetNodeOutVal(V, CurrentPeriod, K)/LCF;
          BlockWrite(F, X, SizeOf(X));

          // Write node lateral inflow to file
          V := GetVarIndex(LATFLOW, NODES);
          X := Uoutput.GetNodeOutVal(V, CurrentPeriod, K)/QCF;
          BlockWrite(F, X, SizeOf(X));

          // Write concen. of each pollutant to file
          for P := 0 to N-1 do
          begin
            if P < Npolluts then
            begin
              V := GetVarIndex(NODEQUAL+P, NODES);
              X := Uoutput.GetNodeOutVal(V, CurrentPeriod, K);
            end
            else X := 0.0;
            BlockWrite(F, X, SizeOf(X));
          end;
        end;

        // Write extra set of 0's for backwards compatibility
        X := 0.0;
        for P := 0 to N-1 do BlockWrite(F, X, SizeOf(X));
      end;
    end;

    // Write selected results for each link at current time period
    for I := 0 to MAXCLASS do
    begin
      if Project.IsLink(I) then for J := 0 to Project.Lists[I].Count-1 do
      begin

        // If no output results for link then write defaults to file
        K := Project.GetLink(I, J).OutFileIndex;
        if K < 0 then
        begin
          X := 0.0;
          BlockWrite(F, X, SizeOf(X));
          BlockWrite(F, X, SizeOf(X));
          X := 1.0;
          BlockWrite(F, X, SizeOf(X));
          X := 0.0;
          for P := 0 to Npolluts-1 do BlockWrite(F, X, SizeOf(X));
        end

        // Otherwise retrieve hotstart values from output file
        else
        begin

          // Write flow to file
          V := GetVarIndex(FLOW, LINKS);
          X := Uoutput.GetLinkOutVal(V, CurrentPeriod, K)/QCF;
          BlockWrite(F, X, SizeOf(X));

          // Write flow depth to file
          V := GetVarIndex(LINKDEPTH, LINKS);
          X := Uoutput.GetLinkOutVal(V, CurrentPeriod, K)/LCF;
          BlockWrite(F, X, SizeOf(X));

          // Write control setting to file
          V := GetVarIndex(CAPACITY, LINKS);
          X := Uoutput.GetLinkOutVal(V, CurrentPeriod, K);
          BlockWrite(F, X, SizeOf(X));

          // Write concen. of each pollutant to file
          for P := 0 to N-1 do
          begin
            if P < Npolluts then
            begin
              V := GetVarIndex(LINKQUAL+P, LINKS);
              X := Uoutput.GetLinkOutVal(V, CurrentPeriod, K);
            end
            else X := 0.0;
            BlockWrite(F, X, SizeOf(X));
          end;
        end;
      end;
    end;

  except
    on EInOutError do
      Uutils.MsgDlg('I/O error writing to hotstart file.', mtError, [mbOK]);
  end;
  CloseFile(F);
end;

end.
