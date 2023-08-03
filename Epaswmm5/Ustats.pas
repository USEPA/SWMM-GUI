unit Ustats;

{-------------------------------------------------------------------}
{                    Unit:    Ustats.pas                            }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   This unit computes frequency statistics on the events that      }
{   have occured during a simulation by calling the procedure       }
{   GetStats(StatsSel, EventList, Results). StatsSel is a           }
{   TStatsSelection record that defines the variable and type of    }
{   event being analyzed. EventList is a TList that will contain    }
{   an ordered listing of TStatsEvent records. Results is a         }
{   TStatsResults record that contains summary event statistics.    }
{-------------------------------------------------------------------}

interface

uses
  Forms, Dialogs, Classes, SysUtils, Math, Uglobals, Uproject, Uutils;


type
  TPlotPosition = (ppFrequency, ppYears, ppMonths);
  TTimePeriod   = (tpVariable, tpDaily, tpMonthly, tpAnnual);
  TStatsType    = (stMean, stPeak, stTotal, stDuration, stDelta,
                   stMeanConcen, stPeakConcen, stMeanLoad, stPeakLoad,
                   stTotalLoad);
  TStatsQuant   = (sqValue, sqLoad);

  // Record which contains information about what kind of report to generate
  TStatsSelection = record
    ObjectID       : String;           // ID of object being analyzed
    ObjectType     : Integer;          // Subcatch, Node or Link
    Variable       : Integer;          // Index of variable analyzed
    MinEventDelta  : Single;           // Min. inter-event hours allowed
    MinEventValue  : Single;           // Min. value for event to exist
    MinEventVolume : Single;           // Min. flow vol. for event to exist
    PlotParameter  : Single;           // e.g., = 0 for Weibull probability
    TimePeriod     : TTimePeriod;      // Event-dependent, Daily, Monthly, Annual
    PlotPosition   : TPlotPosition;    // Frequency, Yearly, Monthly Ret. Per.
    StatsType      : TStatsType;       // Mean, Peak, Total, Duration, Delta
    VarIndex       : Integer;          // Index of variable analyzed in output file
    FlowVarIndex   : Integer;          // Index of flow  variable in output file
    IsQualParam    : Boolean;          // True if variable is quality
    IsRainParam    : Boolean;          // True if variable is rainfall
  end;

  // Record which contains statistical analysis results
  TStatsResults = record
    Duration       : Integer;          // Period of analysis (yrs or months)
    Mean           : Extended;         // Mean value
    StdDev         : Extended;         // Standard deviation
    Skew           : Extended;         // Skewness Coeff.
    Xmin           : Extended;         // Minmimum value
    Xmax           : Extended;         // Maximum value
    EventFreq      : Extended;         // Event frequency
  end;

  PStatsEvent = ^TStatsEvent;          // Pointer to a TStatsEvent object

  // Record which contains the properties of a single event
  TStatsEvent = record
    StartDate      : TDateTime;
    Value          : Single;
    Duration       : Single;
    Rank           : Integer;
  end;

procedure GetStats(var StatsSel: TStatsSelection; EventList: TList;
    var Results: TStatsResults);

implementation

uses
  Fmain, Uoutput;

const
  TXT_FINDING_EVENTS = 'Finding events... ';
  TXT_RANKING_EVENTS = 'Ranking events... ';

  // The following arrays of flow conversion factors are for
  // CFS, GPM, MGD, CMS, LPS, and MLD, respectively.

  // Conversion factors for flow to million liters per day
  QtoMLD: array[0..5] of Single =
    (2.446589, 0.005451, 3.785414, 86.4, 0.0864, 1.0);

  // Conversion from flow time units to days
  QtimeToDays: array[0..5] of Single =
    (86400.0, 1440.0, 1.0, 86400.0, 86400.0, 1.0);

var
  Stats          : TStatsSelection;  // Local copy of stat. event defn.
  TotalPeriods   : Single;           // Total number of event periods
  EventPeriods   : Single;           // Number of periods with events
  WetStart       : TDateTime;        // Start date/time of wet period
  WetEnd         : TDateTime;        // End date/time of wet period
  DryEnd         : TDateTime;        // End date/time of following dry period
  Nwet           : Integer;          // Number of wet periods in an event
  Ysum           : Single;           // Cumulative sum of event variable
  Ymax           : Single;           // Maximum value of event variable
  Vsum           : Single;           // Cumulative flow volume in an event

procedure FindDuration(EventList: TList; var Results: TStatsResults); forward;
procedure FindEvents(EventList: TList); forward;
function  IsNewEventPeriod(const Date1: TDateTime;
          const Date2: TDateTime): Boolean; forward;
procedure InitConditions(const NewDate: TDateTime); forward;
procedure AddNewEvent(EventList: TList; const NewDate: TDateTime); forward;
function  GetEventDelta(EventList: TList): Single; forward;
procedure RankEvents(EventList: TList); forward;
procedure FindStats(EventList: TList; var Results: TStatsResults); forward;


procedure GetStats(var StatsSel: TStatsSelection; EventList: TList;
  var Results: TStatsResults);
//-----------------------------------------------------------------------------
//  Analyzes simulation results to find the frequency of events as
//  defined by the StatsSel argument. EventList contains a rank ordered
//  listing of each event while Results contains summary event statistics.
//-----------------------------------------------------------------------------
begin
  Stats := StatsSel;
  Application.ProcessMessages;
  MainForm.ShowProgressBar(TXT_FINDING_EVENTS);
  FindDuration(EventList, Results);
  FindEvents(EventList);
  MainForm.ShowProgressBar(TXT_RANKING_EVENTS);
  RankEvents(EventList);
  FindStats(EventList, Results);
  StatsSel := Stats;
end;


procedure FindDuration(EventList: TList; var Results: TStatsResults);
//-----------------------------------------------------------------------------
//  Finds the duration of the total simulation period.
//-----------------------------------------------------------------------------
var
  D1, D2: TDateTime;
  Months: Integer;
  Years:  Integer;
begin
  // Set the capacity of the EventList
  EventList.Capacity := Uglobals.Nperiods;

  // Evaluate start and end days
  D1 := Uglobals.StartDateTime;
  D2 := Uglobals.StartDateTime + (Uglobals.Nperiods-1)*Uglobals.DeltaDateTime;

  // Find duration in months & years
  Months := Floor((D2 - D1)/(365/12) + 0.5);
  Years  := Floor((D2 - D1)/365 + 0.5);

  // Assign value to PlotPosition
  if Years < 3 then
  begin
    Stats.PlotPosition := ppMonths;
    Results.Duration := Months;
  end
  else
  begin
    Stats.PlotPosition := ppYears;
    Results.Duration := Years;
  end;
end;


procedure FindEvents(EventList: TList);
//-----------------------------------------------------------------------------
//  Identifies all events that occurred over the simulation period.
//-----------------------------------------------------------------------------
var
  T       : LongInt;
  Q       : Single;
  Y       : Single;
  CF      : Single;
  Date1   : TDateTime;
  Date2   : TDateTime;
  Progress: LongInt;
  ProgStep: LongInt;
  theObject : TObject;          // Reference to object being analyzed

begin
  // Initialize total periods and number with events
  TotalPeriods := 0;
  EventPeriods := 0;

  // Initialize wet/dry period conditions
  InitConditions(Uglobals.StartDateTime);

  // Find step size to use with the MainForm's ProgressBar
  MainForm.ShowProgressBar('Reading Time Series:');
  ProgStep := Nperiods div (MainForm.ProgressBar.Max div MainForm.ProgressBar.Step);
  Progress := 0;
  Application.ProcessMessages;
  MainForm.ShowProgressBar(TXT_FINDING_EVENTS);
  theObject := Uglobals.GetObject(Stats.ObjectType, Stats.ObjectID);

  // Examine each reporting period
  for T := 0 to Uglobals.Nperiods-1 do
  begin
    // Get the current and next reporting dates
    Date1 := Uglobals.StartDateTime + T * Uglobals.DeltaDateTime;
    Date2 := Date1 + Uglobals.DeltaDateTime;

    // See if a new event period has begun
    if IsNewEventPeriod(Date1, Date2) then TotalPeriods := TotalPeriods + 1;

    // If in a wet period, see if a new event has begun
    if Nwet > 0 then AddNewEvent(EventList, Date1);

    // Get current values of the event variable (Y) and flow (Q)
    Y := Uoutput.GetValue(Stats.ObjectType, Stats.VarIndex, T, theObject);
    if Stats.FlowVarIndex = Stats.VarIndex
    then Q := Y
    else if Stats.FlowVarIndex >= 0
    then Q := Uoutput.GetValue(Stats.ObjectType, Stats.FlowVarIndex, T, theObject)
    else Q := 0;

    // Update event properties
    DryEnd := Date2;
    if (Q <> MISSING) and (Y <> MISSING) and (Y > Stats.MinEventValue) then
    begin

      // Convert variables to absolute value
      Q := Abs(Q);
      Y := Abs(Y);

      // Update the event volume (Vsum)
      CF := QtimeToDays[Uglobals.Qunits];
      if Stats.IsRainParam then CF := 24;
      Vsum := Vsum + Q * Uglobals.DeltaDateTime * CF;

      // If computing mass load statistics, convert concentration to load
      if Stats.IsQualParam and (Stats.StatsType >= stMeanLoad)
      then Y := Y * Q * QtoMLD[Uglobals.Qunits];

      // Start a new wet period
      if Nwet = 0 then WetStart := Date1;
      Inc(Nwet);
      WetEnd := Date2;

      // Update sum and max. value
      Ysum := Ysum + Y;
      if Y > Ymax then Ymax := Y;
    end;

    // Update MainForm's progress bar
    MainForm.UpdateProgressBar(Progress, ProgStep);
  end;

  // After processing all time periods, add the last wet period event
  // to the event list
  DryEnd := Stats.MinEventDelta/24.0 + WetEnd + 1.0;
  AddNewEvent(EventList, Uglobals.StartDateTime +
      (Uglobals.Nperiods-1)*Uglobals.DeltaDateTime);

  // Make sure we have at least one event period
  if TotalPeriods = 0 then TotalPeriods := 1;

  // Reclaim any unused capacity in the event list
  EventList.Capacity := EventList.Count;
end;


function IsNewEventPeriod(const Date1: TDateTime;
                          const Date2: TDateTime): Boolean;
//-----------------------------------------------------------------------------
//  Determines if a new event period has begun or not.
//-----------------------------------------------------------------------------
var
  Month1, Month2: Word;
  Year1, Year2  : Word;
  Day1, Day2    : Word;
begin
  Result := False;
  DecodeDate(Date1, Year1, Month1, Day1);
  DecodeDate(Date2, Year2, Month2, Day2);
  case Stats.TimePeriod of

    tpVariable: Result := True;

    tpDaily:
      if Day1 <> Day2 then Result := True;

    tpMonthly:
      if Month1 <> Month2 then Result := True;

    tpAnnual:
      if Year2 > Year1 then Result := True;
  end;
end;


procedure InitConditions(const NewDate: TDateTime);
//-----------------------------------------------------------------------------
//  Initializes the properties of an event.
//-----------------------------------------------------------------------------
begin
  WetStart := NewDate;
  WetEnd   := NewDate;
  DryEnd   := NewDate;
  Ysum   := 0;
  Ymax   := 0;
  Vsum   := 0;
  Nwet   := 0;
end;


procedure AddNewEvent(EventList: TList; const NewDate: TDateTime);
//-----------------------------------------------------------------------------
//  Sees if a new event has occurred and adds it to the event list.
//-----------------------------------------------------------------------------
var
  NewEvent : Boolean;
  AnEvent  : PStatsEvent;
  Year1,
  Year2,
  Day      : Word;
  Month1,
  Month2   : Word;

begin
  // Try to see if a new event has just occurred prior to the new date
  NewEvent := False;
  case Stats.TimePeriod of

    // For a variable event period, the current wet period constitutes
    // a new event if the current time period is dry and the length of
    // the dry period exceeds the stipulated minimum
    tpVariable:
    begin
      if 24*(DryEnd - WetEnd) >= Stats.MinEventDelta then NewEvent := True;
    end;

    // For daily events, a new event occurs if the new date (in integer days)
    // exceeds the start of the current wet period (in integer days)
    tpDaily:
      if Floor(NewDate) > Floor(WetStart) then NewEvent := True;

    // For monthly events, a new event occurs if the new month is different
    // from that of the start of the current wet period
    tpMonthly:
    begin
      DecodeDate(NewDate, Year1, Month1, Day);
      DecodeDate(WetStart, Year2, Month2, Day);
      if Month1 <> Month2 then NewEvent := True;
    end;

    // For annual events, a new event occurs if the new year is different
    // from that of the start of the current wet period
    tpAnnual:
    begin
      DecodeDate(NewDate, Year1, Month1, Day);
      DecodeDate(WetStart, Year2, Month2, Day);
      if Year1 <> Year2 then NewEvent := True;
    end;

  end;

  // If a new event was found to occur, then add it to the event list
  if NewEvent and (Nwet > 0) and (Vsum > Stats.MinEventVolume) then
  begin
    // Create a new event
    New(AnEvent);

    // Assign it a start date and duration
    if Stats.TimePeriod = tpVariable then
    begin
      AnEvent^.StartDate := WetStart;
      AnEvent^.Duration  := 24*(WetEnd - WetStart);   // in hours
      EventPeriods := EventPeriods + (WetEnd - WetStart) / Uglobals.DeltaDateTime;
    end
    else
    begin
      AnEvent^.StartDate := WetStart;
      AnEvent^.Duration := Nwet*24*Uglobals.DeltaDateTime;     // in hours
      EventPeriods := EventPeriods + 1;
    end;

    // Assign the event a value
    case Stats.StatsType of
      stMean:     AnEvent^.Value := Ysum/Nwet;
      stPeak:     AnEvent^.Value := Ymax;
      stTotal:    AnEvent^.Value := Vsum;
      stDuration: AnEvent^.Value := anEvent^.Duration;
      stDelta:    AnEvent^.Value := GetEventDelta(EventList);  // inter-event hours
      stMeanConcen,
      stMeanLoad:   AnEvent^.Value := Ysum/Nwet;
      stPeakConcen,
      stPeakLoad:   AnEvent^.Value := Ymax;
      stTotalLoad:  AnEvent^.Value := Ysum*Uglobals.DeltaDateTime;
      else          AnEvent^.Value := 0.0;
    end;

    // Add the new event to the event list and initiialize the next event
    EventList.Add(AnEvent);
    InitConditions(NewDate);
  end;
end;


function GetEventDelta(EventList: TList): Single;
//-----------------------------------------------------------------------------
//  Computes the time between events for the most recent event and its
//  predeccessor.
//-----------------------------------------------------------------------------
var
  LastEvent: PStatsEvent;
  LastStartDate : TDateTime;
  LastEndDate: TDateTime;
  Year1, Year2, Day, Month1, Month2 : Word;

begin
  // Find the starting and ending dates of the previous event
  if EventList.Count = 0 then
  begin
    LastStartDate := StartDateTime;
    LastEndDate := LastStartDate;
  end
  else
  begin
    LastEvent := EventList.Items[EventList.Count-1];
    LastStartDate := LastEvent^.StartDate;
    LastEndDate := LastStartDate + LastEvent^.Duration/24;
  end;

  // For variable time period events, the inter-event time is the
  // difference between the midpoints of the current event and the
  // previous event
  if Stats.TimePeriod = tpVariable then
    Result := 24*((WetStart - LastStartDate) + (WetEnd - LastEndDate))/2.0

  // For monthly events, the inter-event time is the difference
  // between the start month of the current event and start month
  // of the previous event
  else if Stats.TimePeriod = tpMonthly then
  begin
    DecodeDate(LastStartDate, Year1, Month1, Day);
    DecodeDate(WetStart, Year2, Month2, Day);
    if Month2 >= Month1 then Result := Month2 - Month1
    else Result := Month2 + (12 - Month1);
  end

  // For annual events, the time is the difference between the starting
  // year of the current and previous events
  else if Stats.TimePeriod = tpAnnual then
  begin
    DecodeDate(LastStartDate, Year1, Month1, Day);
    DecodeDate(WetStart, Year2, Month2, Day);
    Result := Year2 - Year1;
  end

  // For daily events, the time is the difference between the starting
  // whole day of the current and previous events
  else Result := Floor(WetStart) - Floor(LastStartDate);
end;


function Compare(Event1, Event2: Pointer): Integer;
//-----------------------------------------------------------------------------
// The comparison function used for sorting events
//-----------------------------------------------------------------------------
var
  V1, V2: Single;
begin
  V1 := TStatsEvent(Event1^).Value;
  V2 := TStatsEvent(Event2^).Value;
  if V1 < V2 then Result := 1
  else if V1 > V2 then Result := -1
  else Result := 0;
end;


procedure RankEvents(EventList: TList);
//-----------------------------------------------------------------------------
//  Rank the events in the event list with respect to the event variable.
//-----------------------------------------------------------------------------
var
  I  : Integer;
  N  : Integer;
  E1 : PStatsEvent;
  E2 : PStatsEvent;
begin
  // Call the TList Sort method to sort the objects in the event list,
  // using the Compare function as the comparison function.
  EventList.Sort(@Compare);

  // Starting from the end of the sorted event list, assign ranks to
  // each event (events with the same value have the same rank)
  N := EventList.Count;
  if N > 0 then
  begin
    E1 := EventList.Items[N-1];
    E1^.Rank := N;
    for I := N-2 downto 0 do
    begin
      E2 := EventList.Items[I];
      if E1^.Value = E2^.Value
      then E2^.Rank := E1^.Rank
      else E2^.Rank := I + 1;
      E1 := E2;
    end;
  end;
end;


procedure FindStats(EventList: TList; var Results: TStatsResults);
//-----------------------------------------------------------------------------
//  Finds summary statistics of the identified events.
//-----------------------------------------------------------------------------
var
  I:  Integer;
  J:  Integer;
  N:  Integer;
  A:  array[1..3] of Extended;
  G:  Extended;
  X:  Extended;
  T:  Extended;
  T1: Extended;
  T2: Extended;
  E:  PStatsEvent;

begin
  // Initialize results
  Results.EventFreq := 0;
  Results.Xmin := 0;
  Results.Xmax := 0;
  Results.StdDev := 0;
  Results.Mean := 0;
  Results.Skew := 0;

  // Exit if too few events
  N := EventList.Count;
  if N < 1 then Exit;

  // Find max. and min. event values
  E := EventList.Items[0];
  Results.Xmax := E^.Value;
  E := EventList.Items[N-1];
  Results.Xmin := E^.Value;

  // Find event frequency
  Results.EventFreq := EventPeriods / TotalPeriods;

  // Find sums of 1st three moments
  for I := 1 to 3 do A[I] := 0;
  for J := 0 to N-1 do
  begin
    E := EventList.Items[J];
    X := E^.Value;
    for I := 1 to 3 do A[I] := A[I] + IntPower(X, I);
  end;

  // Apply formulas to find mean, std. dev., & skewness from
  // sums of first three moments
  G := 0;
  T := N;
  T1 := A[3] - 3*A[1]*A[2]/T + 2/T/T*IntPower(A[1], 3);
  T2 := A[2]/T - IntPower(A[1]/T, 2);
  if T2 < 0 then T2 := 0;
  if T2 > 0 then T2 := Power(T2, 1.5);
  if T2 <> 0 then G := T1/T2/T;
  T1 := A[2] - A[1]*A[1]/T;
  if T1 < 0 then T1 := 0;
  T2 := T;
  if N > 1 then T2 := T - 1;
  Results.StdDev := Sqrt(T1/T2);
  Results.Mean := A[1]/T;
  Results.Skew := G;
end;

end.

