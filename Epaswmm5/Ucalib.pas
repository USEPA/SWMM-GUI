unit Ucalib;

{-------------------------------------------------------------------}
{                    Unit:    Ucalib.pas                            }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Delphi Pascal unit that retrieves a time series of measured     }
{   observation data from a calibration file for use in a time      }
{   series plot.                                                    }
{-------------------------------------------------------------------}

//  Mapping of view variable indexes to calibration file indexes:
//  Variable                    Calibration File Index
//  ------------                ----------------------
//  Subcatchment RUNOFF          1
//  Subcatchment SNOWDEPTH       10
//  Subcatchment GW_FLOW         8
//  Subcatchment GW_ELEV         9
//  Subcatchment SUBCATCHQUAL    2
//  Node NODEDEPTH               3
//  Node LATFLOW                 6
//  Node OVERFLOW                7
//  Node NODEQUAL                5
//  Link FLOW                    4
//  Link DEPTH                   11
//  Link VELOCITY                12

interface

uses
  Classes, SysUtils, TeEngine, Series, Chart, Uglobals, Uutils, Dialogs,
  System.DateUtils;

procedure GetCalibData(VarIndex: Integer; ObjType: Integer; LocID: String;
  const aStartDate: TDateTime; const aEndDate: TDateTime;
  const aTimeFactor: Double;  const aDateTimeDisplay: Boolean;
  ChartSeries: TChartSeries);

implementation

var
  Toklist  : TStringList;
  StartDate: TDateTime;
  EndDate  : TDateTime;
  TimeFactor: Double;
  DateTimeDisplay: Boolean;
  SimStartDate: TDateTime;

procedure GetCalibFileIndex(VarIndex: Integer; ObjType: Integer;
  var FileIndex: Integer; var VarOffset: Integer);
begin
  FileIndex := 0;
  VarOffset := 0;

  // Variable is for a subcatchment
  if (ObjType = SUBCATCHMENTS) then
  begin
    if VarIndex = RUNOFF    then FileIndex := 1;
    if VarIndex = SNOWDEPTH then FileIndex := 10;
    if VarIndex = GW_FLOW   then FileIndex := 8;
    if VarIndex = GW_ELEV   then FileIndex := 9;
    if VarIndex >= SUBCATCHQUAL then
    begin
      FileIndex := 2;
      VarOffset := VarIndex - SUBCATCHQUAL;
    end;
  end

  // Variable is for a node
  else if (ObjType = NODES) then
  begin
    if VarIndex = NODEDEPTH then FileIndex := 3;
    if VarIndex = LATFLOW   then FileIndex := 6;
    if VarIndex = OVERFLOW  then FileIndex := 7;
    if VarIndex >= NODEQUAL then
    begin
      FileIndex := 5;
      VarOffset := VarIndex - NODEQUAL;
    end;
  end

  // Variable is for a link
  else if (ObjType = LINKS) then
  begin
    if VarIndex = FLOW then FileIndex := 4;
    if VarIndex = LINKDEPTH then FileIndex := 11;
    if VarIndex = VELOCITY  then FileIndex := 12;
  end;
end;

function AddCalibDataPoint(ChartSeries: TChartSeries; const DataTok: Integer): Boolean;
//-----------------------------------------------------------------------------
// Adds a calibration data value to the chart data series where
// DataTok = index of the token that contains the measurement value.
//-----------------------------------------------------------------------------
var
  Value: Extended;
  Days : Extended;
  Hours: TDateTime;
  X    : TDateTime;
  aDate: String;
begin
  try
    // Assume the data measurement time is a date/time value
    // (If its not, an exception will occur which is handled below).
    Result := False;
    aDate := TokList[0];
    Days := StrToDate(Uutils.ConvertDate(aDate), Uglobals.MyFormatSettings);
    Hours := Uutils.StrHoursToTime(TokList[1]);
    if Hours < 0 then Exit;
    X := Days + Hours;
  except
    // An exception occurs if measurement time is actually elapsed days/hours
    on EconvertError do
    begin
      Result := False;
      if not Uutils.GetExtended(TokList[0], Days) then Exit;
      Hours := Uutils.StrHoursToTime(TokList[1]);
      if Hours < 0 then Exit;

      // Adjust date to simulation start date.
      X := SimStartDate + Days + Hours;

    end;
  end;

  // Check that measurement time falls within plotting interval
  if (X < StartDate) or (X > EndDate) then Exit;

  // If the chart is not using date/time for X-axis,
  // convert measurement time to elapsed time if need be

  //  Adjust date to simulation start date.
  if not DateTimeDisplay then X := (X - SimStartDate)*TimeFactor;

  Result := True;

  // If the DataTok token contains a valid value then add
  // that value and its time to the calibration point series
  if Uutils.GetExtended(TokList[DataTok], Value)
  then with ChartSeries as TPointSeries do AddXY(X, Value, '', clTeeColor);
end;

procedure GetCalibData(VarIndex: Integer; ObjType: Integer; LocID: String;
  const aStartDate: TDateTime; const aEndDate: TDateTime;
  const aTimeFactor: Double;  const aDateTimeDisplay: Boolean;
  ChartSeries: TChartSeries);
var
  FileIndex: Integer;             //Index of calibration file
  VarOffset: Integer;             //Offset of variable in file
  P        : Integer;
  Ntoks    : Integer;
  DataTok  : Integer;
  UseData  : Boolean;
  F        : TextFile;
  Fname    : String;
  Line     : String;
  S        : String;
begin
  // Determine which calibration file corresponds to the variable of interest
  GetCalibFileIndex(VarIndex, ObjType, FileIndex, VarOffset);
  if FileIndex = 0 then Exit;

  // Check if the calibration file for the variable of interest
  // has data for the location of interest
  if (Pos('"'+LocID+'"', CalibData[FileIndex].Locations) = 0) then Exit;

  // Save shared variables
  StartDate := aStartDate;
  EndDate := aEndDate;
  TimeFactor := aTimeFactor;
  DateTimeDisplay := aDateTimeDisplay;

  // Compute simulation start date which is one reporting
  // period before the first reporting date.
  //SimStartDate := StartDateTime - DeltaDateTime;
  SimStartDate := IncSecond(StartDateTime, - ReportStep);

  // Try to open the calibration data file
  Fname := CalibData[FileIndex].FileName;
  if FileExists(Fname) then
  try
    // Create a list of string tokens
    Toklist := TStringList.Create;
    AssignFile(F,Fname);
    {$I-}
    Reset(F);
    {$I+}
    if IOResult = 0 then
    begin

      // Data token index is 2 + index of measurement variable
      DataTok := 2 + VarOffset;
      UseData := False;

      // Read lines from file until reach end of file
      while not EOF(F) do
      begin
        Readln(F, Line);
        S := Line;

        // Remove any comment & tokenize the line
        P := Pos(';', S);
        if (P > 0) then Delete(S, P, 256);
        Uutils.Tokenize(S, Toklist, Ntoks);

        // A single entry marks start of data for a new location.
        // If this is the location of interest, set UseData to True
        if (Ntoks = 1) then
        begin
          if (Toklist[0] = LocID) then
            UseData := True
          else
            UseData := False;
        end

        // If line has enough items then add data point to data lists
        else if (Ntoks > DataTok) and (UseData) then
        begin
          AddCalibDataPoint(ChartSeries, DataTok);
        end;
      end;
    end;

    // After finished reading the file, free the token list.
  finally
    Toklist.Free;
    CloseFile(F);
  end;
end;

end.
