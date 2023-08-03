unit Fstats;

{-------------------------------------------------------------------}
{                    Unit:    Fstats.pas                            }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   An MDI child form that contains a statistical frequency report  }
{   of user-defined events that occur during a simulation. The      }
{   report is created by calling CreateReport(Stats) where Stats    }
{   is a TStatsSelection record that contains reporting options     }
{   collected from a TStatsSelectForm dialog (see Dstats unit).     }
{                                                                   }
{   The actual event statistics are computed by the GetStats        }
{   procedure in the Ustats unit and are returned in both an        }
{   event list TList and a TStatsResults record. The structures     }
{   of the TStatsSelection, TStatsResults, and TStatsEvent          }
{   records are defined in the Ustats unit.                         }
{                                                                   }
{   The report displays results in a tabbed PageControl, with       }
{   one page for summary statistics (StatsMemo), a second page      }
{   for a ranked event table (EventTable), a third page for a       }
{   histogram plot (HistoChart), and a fourth page for an           }
{   exceedance frequency plot (FreqChart). The report can be        }
{   refreshed after a new analysis is made by calling the           }
{   RefreshReport procedure.                                        }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, NumEdit, ExtCtrls, TeeProcs, TeEngine, Chart, TeeGDIPlus,
  ComCtrls, Clipbrd, Math, Grids, Series, Vcl.Themes, XPrinter,
  Uglobals, Uproject, Uutils, Ugraph, Ustats, System.UITypes, VCLTee.TeeSpline,
  System.DateUtils;

type
  TStatsReportForm = class(TForm)
    PageControl1: TPageControl;
    HistogramPage: TTabSheet;
    TablePage: TTabSheet;
    StatsPage: TTabSheet;
    HistoChart: TChart;
    EventTable: TDrawGrid;
    StatsMemo: TMemo;
    HistoSeries: TBarSeries;
    FrequencyPlotPage: TTabSheet;
    FreqChart: TChart;
    FreqSeries: TAreaSeries;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure EventTableDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure EventTableFixedCellClick(Sender: TObject; ACol, ARow: Integer);
    procedure HistoChartMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure HistoSeriesBeforeDrawValues(Sender: TObject);
  private
    { Private declarations }
    Stats          : TStatsSelection;  // Information on what to analyze
    EventList      : TList;            // List of all events
    Results        : TStatsResults;    // Analysis results
    ObjName        : String;           // ID name of object being analyzed
    VarName        : String;           // Name of variable being analyzed
    VarUnits       : String;           // Units for variable
    TimePeriodLabel: String;
    StatsTypeLabel : String;
    StatsUnitsLabel: String;
    ColHeading1    : array[0..5] of String;
    ColHeading2    : array[0..5] of String;
    ColHeading3    : array[0..5] of String;
    procedure ClearEventList;
    procedure CopyEventsPage(const Fname: String);
    procedure CopyStatsPage(const Fname: String);
    procedure CreateHistogram(var Nbins: Integer;  var Xmin: Single;
              var BinWidth:Single; var Contents: array of Integer);
    function  GetRowColEntry(const I: Integer; const J: Integer): String;
    procedure PrintEventsPage(Destination: TDestination);
    procedure RefreshHistoPage;
    procedure RefreshFreqPage;
    procedure RefreshStatsPage;
    procedure RefreshTablePage;
  public
    { Public declarations }
    procedure CopyTo;
    function  CreateReport(StatsSelection: TStatsSelection): Boolean;
    procedure Print(Destination: TDestination);
    procedure RefreshReport;
    procedure SelectAll;
    procedure SetGraphOptions;
  end;

var
  StatsReportForm: TStatsReportForm;

implementation

{$R *.dfm}

uses
  Dcopy, Fmain;

const
  TXT_STATISTICS = 'Statistics - ';
  TXT_CLEARING_EVENTS = 'Clearing events... ';

  TimePeriodText: array[0..3] of String =
    ('Event', 'Daily', 'Monthly', 'Annual');

  EventPeriodText: array[0..3] of String =
    ('Variable', 'Daily', 'Monthly', 'Annual');

  FrequencyNoteText: array[0..3] of String =
    ('  *Fraction of all reporting periods belonging to an event.',
     '  *Fraction of all days containing an event.',
     '  *Fraction of all months containing an event.',
     '  *Fraction of all years containing an event.');

  DeltaTimeUnits: array[0..3] of String =
    ('(hours)', '(days)', '(months)', '(years)');

  StatsTypeText: array[0..9] of String =
    ('Mean', 'Peak', 'Total', 'Duration', 'Inter-Event Time',
     'Mean Concen.', 'Peak Concen.', 'Mean Loading', 'Peak Loading',
     'Total Load');

  RainVolumeText: array[0..5] of String =
    ('(in)', '(in)', '(in)', '(mm)', '(mm)', '(mm)');

  FlowVolumeText: array[0..5] of String =
    ('(ft3)', '(gal)', '(Mgal)', '(m3)', '(liters)', '(Mliters)');

  ColHeadingText1: array[0..5] of String =
    (' ', ' ', ' ', ' ', 'Exceedance', 'Return');

  ColHeadingText2: array[0..5] of String =
    (' ', ' ', 'Duration', ' ', 'Frequency', 'Period');

  ColHeadingText3: array[0..5] of String =
    ('Rank', 'Start Date', '(hours)', ' ', '(percent)', '(years)');

  StatsText: array[0..17] of String =
    ('  S U M M A R Y   S T A T I S T I C S',
     '  ===================================',
     '  Object  .............. %s %s',
     '  Variable ............. %s %s',
     '  Event Period ......... %s',
     '  Event Statistic ...... %s %s',
     '  Event Threshold ...... %s > %.4f %s',
     '  Event Threshold ...... Event Volume > %.4f %s',
     '  Event Threshold ...... Separation Time >= %.1f  (hr)',
     '  Period of Record ..... %s to %s',
     ' ',
     '  Number of Events ..... %d',
     '  Event Frequency*...... %.3f',
     '  Minimum Value ........ %.3f',
     '  Maximum Value ........ %.3f',
     '  Mean Value ........... %.3f',
     '  Std. Deviation ....... %.3f',
     '  Skewness Coeff. ...... %.3f');


procedure TStatsReportForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
// OnCreate event handler for the form.
//-----------------------------------------------------------------------------
var
  H: Integer;
begin
  // Set width/height of event table's columns/rows
  H := Canvas.TextHeight('Xq') + 4;
  with EventTable do
  begin
    DefaultRowHeight := H + 4;
    RowHeights[0] := 3 * H;
    DefaultColWidth := 120;
    ColWidths[0] := DefaultColWidth div 2;
  end;

  // Create an event list
  EventList := TList.Create;

  // Initialize the histogram & frequency plot properties
  HistoChart.Canvas := TGDIPlusCanvas.Create;
  HistoChart.Zoom.KeyShift :=[ssShift];
  FreqChart.Canvas := TGDIPlusCanvas.Create;
  FreqChart.Zoom.KeyShift :=[ssShift];
  Ugraph.InitGraphOptions(HistoChart);
  Ugraph.InitGraphOptions(FreqChart);
  FreqChart.Legend.Visible := False;
  HistoSeries.Marks.BackColor := GraphOptions.LabelsBackColor;
  FreqSeries.Marks.BackColor := GraphOptions.LabelsBackColor;

  // If no default chart panel color then use the current style's color
  if HistoChart.Color = -1 then
  begin
    HistoChart.Color := StyleServices.GetStyleColor(scPanel);
    FreqChart.Color := StyleServices.GetStyleColor(scPanel);
  end;

  PageControl1.ActivePageIndex := 0;
end;


procedure TStatsReportForm.FormClose(Sender: TObject; var Action: TCloseAction);
//-----------------------------------------------------------------------------
// OnClose event handler for the form.
//-----------------------------------------------------------------------------
begin
  ClearEventList;
  EventList.Free;
  Action := caFree;
end;


procedure TStatsReportForm.EventTableDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
//-----------------------------------------------------------------------------
// OnDrawCell handler for the event table.
//-----------------------------------------------------------------------------
var
  x : Integer;
  s : String;
  offset: Integer;
begin
  offset := Canvas.TextWidth('0');
  with Sender as TDrawGrid do
  begin
    // Fill background of cell
    if (ARow < FixedRows) or (ACol < FixedCols) then
    begin
      if StyleServices.Enabled then
      begin
        Canvas.Brush.Color := StyleServices.GetStyleColor(scPanel);
        Canvas.FillRect(Rect);
      end;
    end;

    // Draw contents of header row
    if (ARow = 0) then
    begin
      // Use Windows API DeawText function to draw multi-line header
      s := ColHeading1[ACol] + #13 + ColHeading2[ACol] + #13 + ColHeading3[ACol];
      InflateRect(Rect, 0, -2);
      DrawText(Canvas.Handle, PChar(s), -1, Rect, DT_CENTER or DT_WORDBREAK);
    end

    // Draw cell value for body of table
    else
    begin
      if ACol >= 1 then
      begin
        SetTextAlign(Canvas.Handle, TA_RIGHT);
        x := Rect.Right - offset;
      end
      else
      begin
        x := Rect.Left + offset;
        SetTextAlign(Canvas.Handle, TA_LEFT);
      end;
      s := GetRowColEntry(ARow-1, ACol);
      Canvas.FillRect(Rect);
      SetBkMode(Canvas.Handle, TRANSPARENT);
      Canvas.TextOut(x, Rect.Top+2, s);
    end;
  end;
end;


procedure TStatsReportForm.EventTableFixedCellClick(Sender: TObject; ACol,
  ARow: Integer);
//-----------------------------------------------------------------------------
// OnFixedCellClick handler for the event table - selects an entire column.
//-----------------------------------------------------------------------------
var
  gRect : TGridRect;
begin
  with Sender as TDrawGrid do
  begin

    // Mouse pressed in a column of row 0
    if (ARow = 0) then
    begin
      // Column is > 0, select all rows in the column
      if (ACol > 0) then
      begin
        gRect.Left := ACol;
        gRect.Right := ACol;
        gRect.Top := FixedRows;
        gRect.Bottom := RowCount-FixedRows;
        Selection := gRect;
      end

      // Column is 0, select all columns & rows
      else SelectAll;
    end;
  end;
end;


procedure TStatsReportForm.HistoChartMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
//-----------------------------------------------------------------------------
// OnMouseDown handler for the histogram chart.
//-----------------------------------------------------------------------------
begin
  if (Button = mbRight) then SetGraphOptions;
end;


procedure TStatsReportForm.HistoSeriesBeforeDrawValues(Sender: TObject);
//-----------------------------------------------------------------------------
// Keeps the histogram chart within the chart's area when scrolled.
//-----------------------------------------------------------------------------
begin
  HistoChart.Canvas.ClipRectangle(HistoChart.ChartRect);
end;


procedure TStatsReportForm.ClearEventList;
//-----------------------------------------------------------------------------
// Clears all entries in the EventList.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  for I := EventList.Count-1 downto 0 do Dispose(EventList.Items[I]);
  EventList.Clear;
end;


function TStatsReportForm.CreateReport(StatsSelection: TStatsSelection): Boolean;
//-----------------------------------------------------------------------------
// Creates a new statistics report.
//-----------------------------------------------------------------------------
var
  OrigVarUnits: String;
begin
  // Save statistics analysis options
  Stats := StatsSelection;

  // Identify the object and variable being analyzed
  Uglobals.GetObjVarNames(Stats.ObjectType, Stats.Variable, ObjName,
                          VarName, OrigVarUnits);
  if Length(OrigVarUnits) > 0 then VarUnits := ' (' + OrigVarUnits + ')';
  Caption := TXT_STATISTICS + ObjName + ' ' + Stats.ObjectID + ' ' + VarName;

  // If there is no flow variable associated with the variable being analyzed
  // then there is no min. event volume defined
  if Stats.FlowVarIndex < 0 then Stats.MinEventVolume := -1;

  // Create identifier labels
  TimePeriodLabel := TimePeriodText[Ord(Stats.TimePeriod)]; //Daily, Event, etc.
  StatsTypeLabel := StatsTypeText[Ord(Stats.StatsType)];    //Mean, Peak, etc.
  StatsUnitsLabel := VarUnits;
  case Stats.StatsType of
    stTotal:
      if Stats.IsRainParam
      then StatsUnitsLabel := RainVolumeText[Uglobals.Qunits]
      else StatsUnitsLabel := FlowVolumeText[Uglobals.Qunits];
    stDuration: StatsUnitsLabel := '(hours)';
    stDelta: StatsUnitsLabel := DeltaTimeUnits[ord(Stats.TimePeriod)];
    stMeanLoad, stPeakLoad:
    begin
      StatsUnitsLabel := '(kg/day)';
      if SameText(OrigVarUnits, 'UG/L') then StatsUnitsLabel := '(g/day)';
      if SameText(OrigVarUnits, '#/L')  then StatsUnitsLabel := '(million/day)';
    end;
    stTotalLoad:
    begin
      StatsUnitsLabel := '(kg)';
      if SameText(OrigVarUnits, 'UG/L') then StatsUnitsLabel := '(grams)';
      if SameText(OrigVarUnits, '#/L')  then StatsUnitsLabel := '(millions)';
    end;
  end;
  Result := True;
end;


procedure TStatsReportForm.RefreshReport;
//-----------------------------------------------------------------------------
// Re-computes statistics and displays them when a new report is first
// created or when results from a new simulation are made available.
//-----------------------------------------------------------------------------
var
  Save_Cursor:TCursor;

begin
  Save_Cursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;    { Show hourglass cursor }

  try
    MainForm.ShowProgressBar(TXT_CLEARING_EVENTS);
    ClearEventList;
    Ustats.GetStats(Stats, EventList, Results);
    RefreshStatsPage;
    RefreshTablePage;
    RefreshHistoPage;
    RefreshFreqPage;

  finally
    Screen.Cursor := Save_Cursor;  { Always restore to normal }
    MainForm.HideProgressBar;
  end;
end;


procedure TStatsReportForm.RefreshStatsPage;
//-----------------------------------------------------------------------------
//  Displays the summary statistics page of the form.
//-----------------------------------------------------------------------------
var
  N:  Integer;
  D1: TDateTime;
  D2: TDateTime;

begin
  D1 := StartDateTime;
  //D2 := StartDateTime + (Nperiods-1)*DeltaDateTime;
  D2 := IncSecond(StartDateTime, (Nperiods-1)*ReportStep);
  N := EventList.Count;

  StatsMemo.Clear;
  with StatsMemo.Lines do
  begin

    // List the object & type of statistical analysis performed
    Add('');
    Add(StatsText[0]);
    Add(StatsText[1]);
    Add(Format(StatsText[2], [ObjName, Stats.ObjectID]));
    Add(Format(StatsText[3], [VarName, VarUnits]));
    Add(Format(StatsText[4], [EventPeriodText[Ord(Stats.TimePeriod)]]));
    Add(Format(StatsText[5], [StatsTypeLabel, StatsUnitsLabel]));
    if Stats.MinEventValue >= 0 then
      Add(Format(StatsText[6], [VarName, Stats.MinEventValue, VarUnits]));
    if Stats.MinEventVolume >= 0 then
    begin
      if Stats.IsRainParam then Add(Format(StatsText[7], [Stats.MinEventVolume,
          RainVolumeText[Uglobals.Qunits]]))
      else Add(Format(StatsText[7], [Stats.MinEventVolume,
          FlowVolumeText[Uglobals.Qunits]]));
    end;
    if (Stats.TimePeriod = tpVariable) and (Stats.MinEventDelta > 0) then
      Add(Format(StatsText[8], [Stats.MinEventDelta]));
    Add(Format(StatsText[9], [DateToStr(D1, MyFormatSettings),
      DateToStr(D2, MyFormatSettings)]));

    // List the number & frequency of events
    Add(StatsText[10]);
    Add(Format(StatsText[11], [N]));
    Add(Format(StatsText[12], [Results.EventFreq]));
  end;

  // Display summary statistics
  if N > 0 then with StatsMemo.Lines do
  begin
    Add(Format(StatsText[13], [Results.Xmin]));
    Add(Format(StatsText[14], [Results.Xmax]));
    Add(Format(StatsText[15], [Results.Mean]));
    Add(Format(StatsText[16], [Results.StdDev]));
    Add(Format(StatsText[17], [Results.Skew]));
  end;
  StatsMemo.Lines.Add('');
  StatsMemo.Lines.Add(FrequencyNoteText[Ord(Stats.TimePeriod)]);
end;


procedure TStatsReportForm.RefreshTablePage;
//-----------------------------------------------------------------------------
//  Displays the table of rank-ordered events.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  // Fill in column headings
  for I := 0 to 5 do
  begin
    ColHeading1[I] := ColHeadingText1[I];
    ColHeading2[I] := ColHeadingText2[I];
    ColHeading3[I] := ColHeadingText3[I];
  end;
  ColHeading1[2] := TimePeriodLabel;
  ColHeading1[3] := TimePeriodLabel;
  ColHeading2[3] := StatsTypeLabel;
  ColHeading3[3] := StatsUnitsLabel;
  if Stats.PlotPosition = ppMonths
  then ColHeading3[5] := '(months)'
  else ColHeading3[5] := '(years)';
  if Stats.StatsType = stDelta then
  begin
    ColHeading1[3] := 'Inter-Event';
    ColHeading2[3] := 'Time';
  end;

  // Set number of rows in the table
  with EventTable do
  begin
    ColCount := 6;
    if EventList.Count = 0
    then RowCount := 2
    else RowCount := EventList.Count + 1;
    Refresh;
  end;
end;


function  TStatsReportForm.GetRowColEntry(const I: Integer; const J: Integer):
  String;
//-----------------------------------------------------------------------------
// Finds the string to display for in row I and column J of the event table.
//-----------------------------------------------------------------------------
var
  E: PStatsEvent;
  R: Single;
  F: Single;
  K: Integer;
begin
  if I >= EventList.Count then Result := ''
  else
  begin
    E := EventList.Items[I];
    K := E^.Rank;
    R := (Results.Duration + 1 - 2*Stats.PlotParameter)/(K - Stats.PlotParameter);
    F := 100*K/(EventList.Count + 1);
    case J of
    0: Result := IntToStr(K);
    1: Result := DateToStr(E^.StartDate, MyFormatSettings);
    2: Result := FloatToStrF(E^.Duration, ffFixed, 7, 1);
    3: Result := FloatToStrF(E^.Value, ffFixed, 12, 3);
    4: Result := FloatToStrF(F, ffFixed, 7, 2);
    5: Result := FloatToStrF(R, ffFixed, 7, 2);
    end;
  end;
end;


procedure TStatsReportForm.RefreshHistoPage;
//-----------------------------------------------------------------------------
// Refreshes the contents of the histogram plot.
//-----------------------------------------------------------------------------
var
  S       : String;
  X       : Single;
  Y       : Single;
  I       : Integer;
  N       : Integer;
  Nbins   : Integer;
  BinWidth: Single;
  Xmin    : Single;
  Contents: array[0..50] of Integer;
begin
  with HistoChart do
  begin
    Title.Text.Clear;
    Title.Text.Add(ObjName + ' ' + Stats.ObjectID + ' ' + VarName);
    case Stats.StatsType of
    stDuration, stDelta:
      S := TimePeriodLabel + ' ' + StatsTypeLabel + ' ' + StatsUnitsLabel;
    else
      S := TimePeriodLabel + ' ' + StatsTypeLabel + ' ' + VarName + ' ' +
           StatsUnitsLabel;
    end;
    BottomAxis.Title.Caption := S;
    LeftAxis.Title.Caption := 'Percent of Total';
    HistoSeries.Clear;
    N := EventList.Count;
    if N = 0 then Exit;
    CreateHistogram(Nbins, Xmin, BinWidth, Contents);
    for I := 0 to Nbins-1 do
    begin
      X := Xmin + I*BinWidth + BinWidth/2;
      Y := 100*Contents[I]/N;
      HistoSeries.AddXY(X, Y, '', clTeeColor);
    end;
  end;
end;


procedure TStatsReportForm.CreateHistogram(var Nbins: Integer;
  var Xmin: Single; var BinWidth:Single; var Contents: array of Integer);
//-----------------------------------------------------------------------------
// Creates a histogram of the items in the event list.
//-----------------------------------------------------------------------------
var
  Xmax: Single;
  I   : Integer;
  K   : Integer;
  N   : Integer;
  E   : PStatsEvent;
begin
  N := EventList.Count;
  if N < 10 then Nbins := 10
  else if N < 20 then Nbins := 20
  else Nbins := 50;
  E := EventList.Items[0];
  Xmax := E^.Value;
  E := EventList.Items[N-1];
  Xmin := E^.Value;
  BinWidth := Uutils.RoundToScale((Xmax - Xmin)/Nbins);
  Xmin := BinWidth*Floor(Xmin/BinWidth);
  Nbins := Ceil((Xmax - Xmin)/BinWidth);
  if Xmin + Nbins*BinWidth <= Xmax then Inc(Nbins);
  if Nbins > 50 then Nbins := 50;
  for I := 0 to Nbins-1 do Contents[I] := 0;
  for I := 0 to N-1 do
  begin
    E := EventList.Items[I];
    K := Floor((E^.Value - Xmin)/BinWidth);
    if K < Nbins then Inc(Contents[K]);
  end;
end;

procedure TStatsReportForm.RefreshFreqPage;
//-----------------------------------------------------------------------------
// Refreshes the contents of the frequency plot.
//-----------------------------------------------------------------------------
var
  S       : String;
  X       : Double;
  Y       : Double;
  I       : Integer;
  N       : Integer;
  K       : Integer;
  M       : Integer;
  E       : PStatsEvent;
begin
  with FreqChart do
  begin
    Title.Text.Clear;
    Title.Text.Add(ObjName + ' ' + Stats.ObjectID + ' ' + VarName);
    case Stats.StatsType of
    stDuration, stDelta:
      S := TimePeriodLabel + ' ' + StatsTypeLabel + ' ' + StatsUnitsLabel;
    else
      S := TimePeriodLabel + ' ' + StatsTypeLabel + ' ' + VarName + ' ' +
           StatsUnitsLabel;
    end;
    BottomAxis.Title.Caption := S;
    LeftAxis.Logarithmic := True;
    LeftAxis.Title.Caption := 'Exceedance Frequency (%)';
    FreqSeries.Clear;
    N := EventList.Count;
    if N = 0 then Exit;
    M := N div 50;
    if M = 0 then M := 1;
    for I := 0 to N-1 do
    begin
      if I mod M > 0 then continue;
      E := EventList.Items[I];
      X := E^.Value;
      K := E^.Rank;
      Y := 100*K/(EventList.Count);
      FreqSeries.AddXY(X, Y, '', clTeeColor);
    end;
  end;
end;


procedure TStatsReportForm.SetGraphOptions;
//-----------------------------------------------------------------------------
// Launches the dialog that sets display options for the histogram
// or frequency plot.
//-----------------------------------------------------------------------------
var
  StartPage: Integer;
begin
  StartPage := 0;
  if PageControl1.ActivePage = HistogramPage
  then Ugraph.SetGraphOptions(HistoChart, StartPage, Self)
  else if PageControl1.ActivePage = FrequencyPlotPage
  then Ugraph.SetGraphOptions(FreqChart, StartPage, Self)
end;


procedure TStatsReportForm.CopyTo;
//-----------------------------------------------------------------------------
// Copies the active page of the form to a file or to the Windows Clipboard.
//-----------------------------------------------------------------------------
var
  CopyToForm: TCopyToForm;
begin

  // If active page is the histogram or frequency plot, use the procedure
  // in the Ugraph.pas unit to copy the plot
  if PageControl1.ActivePage = HistogramPage
  then Ugraph.CopyTo(HistoChart)
  else if PageControl1.ActivePage = FrequencyPlotPage
  then Ugraph.CopyTo(FreqChart)

  // Otherwise display the CopyTo dialog form
  else
  begin
    CopyToForm := TCopyToForm.Create(self);
    with CopyToForm do
    try
      FormatGroup.ItemIndex := 2;
      FormatGroup.Enabled := False;
      if ShowModal = mrOK then
      begin
        if PageControl1.ActivePage = StatsPage
        then CopyStatsPage(DestFileName);
        if PageControl1.ActivePage = TablePage
        then CopyEventsPage(DestFileName);
      end;
    finally
      Free;
    end;
  end;
end;


procedure TStatsReportForm.CopyStatsPage(const Fname: String);
//-----------------------------------------------------------------------------
// Copies the Summary Statistics page to a file or to the Windows Clipboard.
//-----------------------------------------------------------------------------
begin
  with StatsMemo do
  begin
    if Length(Fname) > 0 then Lines.SaveToFile(Fname)
    else Clipboard.SetTextBuf(PChar(Lines.Text));
  end;
end;


procedure TStatsReportForm.CopyEventsPage(const Fname: String);
//-----------------------------------------------------------------------------
// Copies the Event Table page to a file or to the Windows Clipboard.
//-----------------------------------------------------------------------------
var
  R: LongInt;
  C: LongInt;
  S: String;
  Slist: TStringlist;

begin
  // Create a stringlist to hold each line of the table
  Slist := TStringlist.Create;
  try
    with EventTable.Selection do
    begin

      // Add title to stringlist
      Slist.Add(Project.Title);
      Slist.Add(Caption);

      // Add column headings to stringlist
      S := Format('%-12s',[ColHeading1[Left]]);
      for C := Left+1 to Right do S := S + #9 + Format('%-12s',[ColHeading1[C]]);
      Slist.Add(S);
      S := Format('%-12s',[ColHeading2[Left]]);
      for C := Left+1 to Right do S := S + #9 + Format('%-12s',[ColHeading2[C]]);
      Slist.Add(S);
      S := Format('%-12s',[ColHeading3[Left]]);
      for C := Left+1 to Right do S := S + #9 + Format('%-12s',[ColHeading3[C]]);
      Slist.Add(S);

      // Iterate through each row of selected block of cells
      for R := Top to Bottom do
      begin
        // Build up a tab-delimited string for entries on current row
        S := GetRowColEntry(R-1, Left);
        for C := Left+1 to Right do
          S := S + #9 + Format('%-12s',[GetRowColEntry(R-1, C)]);

        // Add tab-delimited string to list
        Slist.Add(S);
      end;
    end;

    // Save stringlist to file if file name supplied
    if Length(Fname) > 0 then Slist.SaveToFile(Fname)

    // Otherwise place text of stringlist onto clipboard
    else Clipboard.SetTextBuf(PChar(Slist.Text));

  // Free the stringlist.
  finally
    Slist.Free;
  end;
end;


procedure TStatsReportForm.SelectAll;
//-----------------------------------------------------------------------------
// Selects all rows and columns of the event table.
//-----------------------------------------------------------------------------
var
  gRect : TGridRect;

begin
  with EventTable do
  begin
    gRect.Left := 0;
    gRect.Right := ColCount - 1;
    gRect.Top := FixedRows;
    gRect.Bottom := RowCount - FixedRows;
    Selection := gRect;
  end;
end;


procedure TStatsReportForm.Print(Destination: TDestination);
//-----------------------------------------------------------------------------
// Prints the active page of the form.
//-----------------------------------------------------------------------------
var
  I: Integer;

begin
  if PageControl1.ActivePage = HistogramPage
  then Ugraph.Print(HistoChart, MainForm.thePrinter, Destination)
  else if PageControl1.ActivePage = FrequencyPlotPage
  then Ugraph.Print(FreqChart, MainForm.thePrinter, Destination)
  else if PageControl1.ActivePage = StatsPage
  then with MainForm.thePrinter do
  begin
    BeginJob;
    SetDestination(Destination);
    with StatsMemo.Font do
      SetFontInformation(Name, Size, Style);
    with StatsMemo do
      for I := 0 to Lines.Count - 1 do PrintLine(Lines[I]);
    EndJob;
  end
  else if PageControl1.ActivePage = TablePage
  then PrintEventsPage(Destination);
end;


procedure TStatsReportForm.PrintEventsPage(Destination: TDestination);
//-----------------------------------------------------------------------------
// Prints the event table.
//-----------------------------------------------------------------------------
var
  I, J, StartRow, EndRow: Integer;

begin
  with MainForm.thePrinter do
  begin
    // Initialize the Printer object
    Screen.Cursor := crHourglass;
    BeginJob;
    SetDestination(Destination);
    SetFontInformation('Times New Roman',11,[]);
    PrintCenter(Caption);
    NewLines(2);

    // Create a table with borders and grid lines
    CreateTable(EventTable.ColCount);
    SetTableStyle([sBorder, sVerticalGrid, sHorizontalGrid]);

    // Make the grid's fixed rows be the column headings
    for J := 0 to EventTable.ColCount-1 do
    begin
      SetColumnHeaderText(j+1, 1, ColHeading1[J]);
      SetColumnHeaderText(j+1, 2, ColHeading2[J]);
      SetColumnHeaderText(j+1, 3, ColHeading3[J]);
    end;

    // Begin printing the cells of the table
    BeginTable;
    StartRow := EventTable.FixedRows;
    EndRow := EventTable.RowCount - 1;

    // Process each non-fixed row of the grid
    for I := StartRow to EndRow do
    begin
      // Print the grid cell contents for each column
      for J := 0 to EventTable.ColCount-1 do
        PrintColumnLeft(J+1, GetRowColEntry(I, J));

      // Move to the next row position in the printed table
      NextTableRow( (I = EndRow) );
    end;

    // End printing the table, set the job's destination and end it
    EndTable;
    SetDestination(Destination);
    Screen.Cursor := crDefault;
    EndJob;
  end;
end;

end.
