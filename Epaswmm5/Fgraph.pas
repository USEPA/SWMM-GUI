unit Fgraph;

{-------------------------------------------------------------------}
{                    Unit:    Fgraph.pas                            }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21   (5.2.0)                    }
{                    Author:  L. Rossman                            }
{                                                                   }
{   MDI child form that displays simulation results in the form     }
{   of a time series plot or a scatter plot. The graph is created   }
{   by calling CreateGraph. It gets refreshed (after a new analysis }
{   is made) by calling RefreshGraph. SetGraphOptions is called to  }
{   change display options, CopyTo to copy the graph to a file or   }
{   to the clipboard, and Print to print the graph.                 }
{                                                                   }
{   This unit uses several routines in the Ugraph unit.             }
{-------------------------------------------------------------------}

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, DateUtils,
  Forms, Dialogs, TeEngine, Series, ExtCtrls, TeeProcs, Chart, Math,
  Buttons, TeeGDIPlus, Vcl.Themes, XPrinter, Uglobals, Uproject, Uutils;

type
  TGraphForm = class(TForm)
    Chart1: TChart;
    SpeedButton1: TSpeedButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Chart1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
    OptionPage:   Integer;
    StartPeriod:  LongInt;
    EndPeriod:    LongInt;
    TotalPeriods: LongInt;
    XvarIndex:    Integer;
    YvarIndex:    Integer;
    TimeFactor:   Double;
    StartDate:    TDateTime;
    EndDate:      TDateTime;
    Graph:        TReportSelection;

    function  CreateScatterPlot: Boolean;
    function  CreateTimeSeriesPlot: Boolean;
    procedure AssignTitles;
    procedure GetCalibSeries;
    procedure GetSeriesData(const K: Integer; Dt:TDateTime);
    procedure RefreshScatterPlot;
    procedure RefreshTimeSeriesPlot;
    procedure FormatTimeAxis;
    procedure SetAxisScale(theAxis: TChartAxis);
    procedure SetSpeedBtnGlyph(ImageName: String);

  public
    { Public declarations }
    procedure CopyTo;
    function  CreateGraph(ReportSelection: TReportSelection): Boolean;
    procedure Print(Destination: TDestination);
    procedure RefreshGraph;
    procedure SetGraphOptions;
  end;

var
  GraphForm: TGraphForm;

implementation

{$R *.DFM}

uses
  Fmain, Fmap, Ubrowser, Uoutput, Ugraph, Ucalib;

const
  TXT_GRAPH = 'Graph - ';
  TXT_SERIES = 'Series';
  TXT_COMPUTED = 'Computed';
  TXT_OBSERVED = 'Observed';
  TXT_TIME_HRS = 'Elapsed Time (hours)';
  TXT_TIME_DAYS = 'Elapsed Time (days)';
  TXT_SYS_RESULTS = 'System Results';
  TXT_SCATTER_LINE = 'Scatter Line';

  MSG_RETRIEVING_DATA = 'Retrieving Data Series ';
  MSG_RETRIEVING_CALIB_DATA = 'Retrieving calibration data... ';
  ObjectClasses: array[0..3] of String =
    ('Subcatchment', 'Node', 'Link', 'System');

//=============================================================================
//                        Event handlers
//=============================================================================

procedure TGraphForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
// OnCreate handler for form.
//
// The Quality property for the fonts on all text in Chart1 was
// set to Default at design time through the TeeChart editor.
//-----------------------------------------------------------------------------
begin
  // Create a stringlist to store items being graphed
  Graph.Items := TStringlist.Create;

  // Initialize chart properties
  Ugraph.InitGraphOptions(Chart1);

  // If no default chart panel color then use the current style's color
  if Chart1.Color = -1
  then Chart1.Color := StyleServices.GetStyleColor(scPanel);
  ColorPalettes.ApplyPalette(Chart1, 7);

  // Create a TeeChart GDIplus canvas for antialiasing
  Chart1.Canvas := TGDIPlusCanvas.Create;

  // Make max. axis label round to highest integer value
  with Chart1 do
  begin
    LeftAxis.MaximumRound := True;
    RightAxis.MaximumRound := True;
    BottomAxis.MaximumRound := True;
  end;

  // Make the left mouse zoom when shift key is pressed
  Chart1.Zoom.KeyShift :=[ssShift];
  Chart1.ScrollMouseButton := mbLeft;

  // Set the initial tab page of the Graph Options dialog
  OptionPage := 0;
  SetSpeedBtnGlyph('lock_open');
end;

procedure TGraphForm.FormClose(Sender: TObject; var Action: TCloseAction);
//-----------------------------------------------------------------------------
// OnClose handler for form.
//-----------------------------------------------------------------------------
begin
  Action := caFree;
end;

procedure TGraphForm.FormDestroy(Sender: TObject);
//-----------------------------------------------------------------------------
// OnDestroy handler for form. Frees chart series and stringlists.
//-----------------------------------------------------------------------------
begin
  with Chart1 do
    while SeriesCount > 0 do Series[0].Free;
  Graph.Items.Free;
end;

procedure TGraphForm.Chart1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
//-----------------------------------------------------------------------------
// OnMouseDown handler for Chart1. Launches graph options dialog
// when user right-clicks over the graph.
//-----------------------------------------------------------------------------
begin
  if (Button = mbRight) then SetGraphOptions;
end;

procedure TGraphForm.SetSpeedBtnGlyph(ImageName: String);
//------------------------------------------------------------------------------
//  Sets the bitmap image for the form's speed button in a DPI-aware manner.
//------------------------------------------------------------------------------
var
  bm: TBitmap;
begin
  bm := TBitmap.Create;
  try
    bm.Canvas.Handle;
    with MainForm.ProjectImageList do
      GetBitmap(GetIndexByName(ImageName), bm);
    SpeedButton1.Glyph := bm;
  finally
    bm.Free;
  end;
end;

procedure TGraphForm.SpeedButton1Click(Sender: TObject);
//------------------------------------------------------------------------------
//  Changes the bitmap image for the form's speed button.
//------------------------------------------------------------------------------
begin
  if SpeedButton1.Down then
  begin
    SetSpeedBtnGlyph('lock_closed');
  end
  else begin
    SetSpeedBtnGlyph('lock_open');
  end;
end;

function  TGraphForm.CreateGraph(ReportSelection: TReportSelection): Boolean;
//-----------------------------------------------------------------------------
// Creates a graph based on selections in ReportSelection.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  // Copy graph selection choices
  with ReportSelection do
  begin
    Graph.ReportType := ReportType;
    for I := 0 to VariableCount-1 do
    begin
      Graph.Variables[I] := Variables[I];
      Graph.ReportItems[I] := ReportItems[I];
    end;
    Graph.ObjectType := ObjectType;
    Graph.XObjectType := XObjectType;
    Graph.StartDateIndex := StartDateIndex;
    Graph.EndDateIndex := EndDateIndex;
    Graph.DateTimeDisplay := DateTimeDisplay;
    if Assigned(Items) then Uutils.CopyStringList(Items, Graph.Items)
    else Graph.Items := nil;
    Graph.ItemCount := ItemCount;
    if ObjectType = SYS then Graph.ItemCount := VariableCount;
  end;

  // Determine number of time periods plotted
  StartPeriod := Uglobals.GetPeriod(Graph.StartDateIndex);
  EndPeriod := Uglobals.GetPeriod(Graph.EndDateIndex);
  TotalPeriods := EndPeriod - StartPeriod + 1;

  // Create the called for graph
  case Graph.ReportType of
    TIMESERIESPLOT: Result := CreateTimeSeriesPlot;
    SCATTERPLOT:    Result := CreateScatterPlot;
    else            Result := False;
  end;
end;

procedure TGraphForm.RefreshGraph;
//-----------------------------------------------------------------------------
// Refreshes graph display after data changes.
//-----------------------------------------------------------------------------
var
  K: Integer;
  Save_Cursor: TCursor;
begin
  // Exit if the graph is locked (SpeedButton1 is in the down position)
  if SpeedButton1.Down = True then exit;

  // If no results are available, then clear all previous data and
  // refresh the chart
  if not RunFlag then with Chart1 do
  begin
    for K := 0 to SeriesCount-1 do Series[K].Clear;
    Refresh;
    Exit;
  end;

  // Change mouse cursor shape
  Application.ProcessMessages;
  Save_Cursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;

  // Refresh the graph's data and redraw it
  // (The refresh procedures call MainForm.ShowProgressBar)
  try
    case Graph.ReportType of
      TIMESERIESPLOT: RefreshTimeSeriesPlot;
      SCATTERPLOT:    RefreshScatterPlot;
    end;
  finally
    Application.ProcessMessages;
    Screen.Cursor := Save_Cursor;  { Always restore to normal }
    MainForm.HideProgressBar;      { In case the refresh procedure fails }
  end;
end;


//=============================================================================
//                 Time Series Ploting Functions
//=============================================================================

function TGraphForm.CreateTimeSeriesPlot: Boolean;
//-----------------------------------------------------------------------------
// Creates a time series graph.
//-----------------------------------------------------------------------------
var
  K         : Integer;
  Title     : String;
  ObjName   : String;
  VarName   : String;
  VarUnits  : String;
  ObjType   : Integer;
  VarType   : Integer;
  aSeries   : TChartSeries;

begin
  Result := True;
  with Graph do
  begin
    AssignTitles;
    if ItemCount > 1 then Chart1.Legend.Visible := True;

    // Consider each object being plotted
    Title := TXT_COMPUTED;
    for K := 0 to ItemCount - 1 do
    begin

      // Get the object type and variable type
      ObjType := ReportItems[K].ObjType;
      VarType := ReportItems[K].Variable;

      // Get the legend text of the objects being plotted
      if Length(ReportItems[K].LegendTxt) > 0
      then Title := ReportItems[K].LegendTxt
      else
      begin
        Uglobals.GetObjVarNames(ObjType, VarType, ObjName, VarName, VarUnits);
        if Length(VarUnits) > 0 then VarUnits := '(' + VarUnits + ')';
        Title := Items[K] + ' ' + VarUnits;
      end;

      // Create a Fast Line data series for large time series
      // (more than a year of 15-minute reporting steps)
      if TotalPeriods >= 36000
      then aSeries := Ugraph.CreateFastLineSeries(Chart1, Title)

      // Or an Area data series for rainfall
      else if (ObjType = SUBCATCHMENTS) and (VarType = RAINFALL)
      then aSeries := Ugraph.CreateAreaSeries(Chart1, Title)
      else if (ObjType = SYS) and (VarType = SYS_RAINFALL)
      then aSeries := Ugraph.CreateAreaSeries(Chart1, Title)

      // Or a normal Line series otherwise
      else aSeries := Ugraph.CreateLineSeries(Chart1, Title);

      if Assigned(aSeries) then
      begin
        // Store series index in its Tag property
        aSeries.Tag := K;

        // Assign Marks properties
        aSeries.Marks.Callout.Length := 12;
        aSeries.Marks.Visible := False;
        aSeries.Marks.BackColor := GraphOptions.LabelsBackColor;
        aSeries.Marks.Transparent := GraphOptions.LabelsTransparent;
        aSeries.Marks.Style := TSeriesMarksStyle(0);

        // Set series Y axis
        if ReportItems[K].Axis = 2 then aSeries.VertAxis := aRightAxis;
      end;

      // For speed, do not order the X-values
      Chart1.Series[K].XValues.Order := loNone;

      // Use a DateTime X-variable if so specified
      Chart1.Series[K].XValues.DateTime := DateTimeDisplay;
    end;
    Chart1.Legend.Visible := True;

    // Create a Point series for any calibration data
    if ItemCount = 1 then
    begin
      aSeries := Ugraph.CreatePointSeries(Chart1, TXT_OBSERVED);
      if Assigned(aSeries) then
      begin
        aSeries.Tag := MAXSERIES;
        aSeries.ShowInLegend := False;
      end;
    end;
  end;
end;

procedure TGraphForm.SetAxisScale(theAxis: TChartAxis);
//-----------------------------------------------------------------------------
//  Automatically scales the graph's axes
//-----------------------------------------------------------------------------
var
  zMin, zMax, zInc: Double;
  Rescale: Boolean;
begin
  if theAxis.IsDateTime then Exit;
  Rescale := false;
  with theAxis do
  begin
    if theAxis = Chart1.BottomAxis then
    begin
      zMin := Chart1.MinXValue(theAxis);
      zMax := Chart1.MaxXValue(theAxis);
      if (zMin < Minimum) or (zMax > Maximum) or
      (zMax + Increment < Maximum) then
      Rescale := true;
    end
    else
    begin
      zMin := Chart1.MinYValue(theAxis);
      zMax := Chart1.MaxYValue(theAxis);
      if (zMin < Minimum) or (zMax > Maximum)
      then Rescale := true;
    end;
    if Rescale then
    begin
      Automatic := False;
      AutomaticMinimum := False;
      AutomaticMaximum := False;
      Uutils.AutoScale(zMin, zMax, zInc);
      SetMinMax(zMin, zMax);
      Increment := zInc;
    end;
  end;
end;

procedure TGraphForm.AssignTitles;
//-----------------------------------------------------------------------------
//  Assigns title captions to graph window and axes
//-----------------------------------------------------------------------------
var
  K, I: Integer;
  AxisTitle: array[1..2] of String; // 1 = left axis, 2 = right axis
  NumVars: array[1..2] of Integer;
  LastVar: array[1..2] of Integer;
  LastObj: array[1..2] of Integer;
  ObjName: String;
  VarName: String;
  VarUnits: String;
begin
  // Initialize number of left & right axis variables
  NumVars[1] := 0;
  NumVars[2] := 0;
  LastVar[1] := -1;
  LastVar[2] := -1;
  LastObj[1] := -1;
  LastObj[2] := -1;

  // Examine each data series
  for K := 0 to Graph.ItemCount-1 do
  begin
    // Form axis title from plotted variable name & its units
    Uglobals.GetObjVarNames(Graph.ReportItems[K].ObjType,
           Graph.ReportItems[K].Variable, ObjName, VarName, VarUnits);
    if Length(VarUnits) > 0 then VarUnits := '(' + VarUnits + ')';
    I := Graph.ReportItems[K].Axis;
    AxisTitle[I] := VarName + ' ' + VarUnits;

    // Check if this series is for a new variable
    if (Graph.ReportItems[K].ObjType <> LastObj[I])
    or (Graph.ReportItems[K].Variable <> LastVar[I]) then Inc(NumVars[I]);

    // Save indexes of the data series' object type and variable
    LastObj[I] := Graph.ReportItems[K].ObjType;
    LastVar[I] := Graph.ReportItems[K].Variable;
  end;

  // Remove axis title if it covers more than one variable
  if (NumVars[1] = 0) or (NumVars[1] > 1) then AxisTitle[1] := '';
  if (NumVars[2] = 0) or (NumVars[2] > 1) then AxisTitle[2] := '';

  // Assign the axis titles to the chart
  Chart1.LeftAxis.Title.Caption := AxisTitle[1];
  Chart1.RightAxis.Title.Caption := AxisTitle[2];

  // Construct the window's title caption
  Caption := TXT_GRAPH + Graph.Items[0];
  if Graph.ItemCount > 1 then Caption := Caption + '...';

end;

procedure TGraphForm.FormatTimeAxis;
//-----------------------------------------------------------------------------
// Formats the time axis of a time series plot.
//-----------------------------------------------------------------------------
var
  DeltaDays : Integer;
begin
  with Graph do
  begin
    // Determine Time units
    DeltaDays := Round(EndDate - StartDate);
    TimeFactor := 1.0;

    if not DateTimeDisplay then
    begin
      if DeltaDays <= 7 then
      begin
        Chart1.BottomAxis.Title.Caption := TXT_TIME_HRS;
        TimeFactor := 24.0;
      end
      else Chart1.BottomAxis.Title.Caption := TXT_TIME_DAYS;
    end
    else begin
      Chart1.BottomAxis.Title.Caption := ' ';
      if Length(GraphOptions.DateTimeFormat) > 0 then
        Chart1.BottomAxis.DateTimeFormat := GraphOptions.DateTimeFormat
      else if DeltaDays <= 3 then
        Chart1.BottomAxis.DateTimeFormat := 'h:nn'
      else if DeltaDays <= 10 then
        Chart1.BottomAxis.DateTimeFormat := 'h:nn m/d/yy'
      else if DeltaDays <= 60 then
        Chart1.BottomAxis.DateTimeFormat := 'm/d yyyy'
      else if DeltaDays <= 10*365 then
        Chart1.BottomAxis.DateTimeFormat := 'mmm yyyy'
      else
         Chart1.BottomAxis.DateTimeFormat := 'yyyy';
      Chart1.BottomAxis.ExactDateTime := True;
    end;
  end;
end;


procedure TGraphForm.RefreshTimeSeriesPlot;
//-----------------------------------------------------------------------------
// Refreshes drawing of a time series plot.
//-----------------------------------------------------------------------------
var
  K          : Integer;
  Dt         : TDateTime;
begin
  // Determine number of time periods plotted
  StartPeriod := Uglobals.GetPeriod(Graph.StartDateIndex);
  EndPeriod := Uglobals.GetPeriod(Graph.EndDateIndex);
  TotalPeriods := EndPeriod - StartPeriod + 1;

  // Determine range of dates to plot
  Dt := DeltaDateTime;
  Startdate := IncSecond(StartdateTime, StartPeriod*Uglobals.ReportStep);
  EndDate := IncSecond(StartDateTime, EndPeriod*Uglobals.ReportStep);
  FormatTimeAxis;

  // Inactivate each data series (so that graph doesn't keep redrawing itself)
  for K := 0 to Chart1.SeriesCount-1 do
  begin
    Chart1.Series[K].Active := False;
    Chart1.Series[K].Clear;
  end;

  // Retrieve data for each time series in the graph
  for K := 0 to Graph.ItemCount - 1 do
  begin
    MainForm.ShowProgressBar(MSG_RETRIEVING_DATA + IntToStr(K+1) + '... ');
    GetSeriesData(K, Dt);
  end;

  // Add calibration data for a single-object plot
  if (Graph.ItemCount = 1) then
  begin
   GetCalibSeries;
  end;

  // Activate each series
  for K := 0 to Chart1.SeriesCount-1 do
  begin
    if Chart1.Series[K].Count > 0 then Chart1.Series[K].Active := True;
  end;

  // Scale the axes
  SetAxisScale(Chart1.BottomAxis);
  SetAxisScale(Chart1.LeftAxis);
  SetAxisScale(Chart1.RightAxis);

end;

procedure TGraphForm.GetSeriesData(const K: Integer; Dt: TDateTime);
//-----------------------------------------------------------------------------
//  Loads time series data into chart series where  K = chart series index
//  and Dt = time increment of output values in decimal days.
//-----------------------------------------------------------------------------
var
  J          : LongInt;
  theObject  : TObject;
  ObjType    : Integer;
  X, Y       : Double;
  Progress   : LongInt;
  ProgStep   : LongInt;

begin
  ProgStep := TotalPeriods div (MainForm.ProgressBar.Max div
                                MainForm.ProgressBar.Step);
  Progress := 0;
  with Graph do
  begin
    // Get a reference to the object being plotted
    ObjType := ReportItems[K].ObjType;
    if ObjType <> SYS then
    begin
      theObject := Uglobals.GetObject(ObjType, ReportItems[K].ObjName);
      if theObject = nil then Exit;
    end
    else theObject := nil;

    // Retrieve values to be plotted from the simulation's binary output file
    try
      // Find starting X value for graph
      with Chart1.Series[K] do
      begin

        // Find initial date of graph
        if DateTimeDisplay then
          //X := StartDateTime + StartPeriod*Dt
          X := IncSecond(StartDateTime, StartPeriod*ReportStep)

        // Find initial elapsed time
        else
        begin
          Dt := Dt*TimeFactor;
          X := (StartPeriod + StartOffset)*Dt;
        end;

        // Determine the index of the plotted variable
        // (in the binary output file)
        YvarIndex := Uoutput.GetVarIndex(ReportItems[K].Variable, ObjType);

        // Get Y-value for each reporting period in graph
        for J := StartPeriod to EndPeriod do
        begin
          // If a Y-value found, add it to the graph
          Y := Uoutput.GetValue(ObjType, YvarIndex, J, theObject);
          if Y <> MISSING then AddXY(X, Y, '', clTeeColor);

          // Move to next time X
          if DateTimeDisplay then
            X := IncSecond(X, ReportStep)
          else
            X := X + Dt;
          MainForm.UpdateProgressBar(Progress, ProgStep);
        end;

      end;
    finally
    end;
  end;
end;


procedure TGraphForm.GetCalibSeries;
//-----------------------------------------------------------------------------
//  Loads calibration data into a chart series.
//-----------------------------------------------------------------------------
var
  Nseries  : Integer;
begin
  // Retrieve data points from the calibration file
  MainForm.ShowProgressBar(MSG_RETRIEVING_CALIB_DATA);
  Nseries := Chart1.SeriesCount-1;
  Chart1.Series[Nseries].XValues.Order := loNone;
  if Graph.Items.Count = 0 then exit;
  Ucalib.GetCalibData(Graph.ReportItems[0].Variable,
                      Graph.ReportItems[0].ObjType,
                      Graph.ReportItems[0].ObjName,
                      StartDate, EndDate, TimeFactor,
                      Graph.DateTimeDisplay,
                      Chart1.Series[Nseries]);

  // Activate the chart's calibration data series legend.
  with Chart1.Series[Nseries] do
  begin
    if (Count > 0) then
    begin
      ShowInLegend := True;
      Chart1.Legend.Visible := True;
    end;
  end;
end;

//=============================================================================
//                     Scatter Plot Functions
//=============================================================================

function TGraphForm.CreateScatterPlot: Boolean;
//-----------------------------------------------------------------------------
// Creates a scatter plot graph.
//-----------------------------------------------------------------------------
var
  XObjName  : String;
  YObjName  : String;
  XVarName  : String;
  YVarName  : String;
  XVarUnits : String;
  YVarUnits : String;
  Title     : String;
begin
  Result := True;
  with Graph do
  begin
    // Assign title strings
    XvarIndex := Uoutput.GetVarIndex(Variables[0], XObjectType);
    YvarIndex := Uoutput.GetVarIndex(Variables[1], ObjectType);
    Uglobals.GetObjVarNames(XObjectType, Variables[0], XObjName, XVarName, XVarUnits);
    Uglobals.GetObjVarNames(ObjectType, Variables[1], YObjName, YVarName, YVarUnits);
    if Length(XVarUnits) > 0 then XVarUnits := ' (' + XVarUnits + ')';
    if Length(YVarUnits) > 0 then YVarUnits := ' (' + YVarUnits + ')';
    Title := YObjName + ' ' + Items[1] + ' ' + YVarName + ' v. ' +
             XObjName + ' ' + Items[0] + ' ' + XVarName;
    Caption := 'Graph - ' + Title;
    Chart1.Title.Text.Add(Title);
    Chart1.BottomAxis.Title.Caption := XObjName + ' ' + Items[0] + ' ' +
                                       XVarName + XVarUnits;
    Chart1.LeftAxis.Title.Caption := YObjName + ' ' + Items[1] + ' ' +
                                     YVarName + YVarUnits;
  end;

  // Create a Point series for the plot & set its options
  Ugraph.CreatePointSeries(Chart1, '');
  with Chart1 do
  begin
    Legend.Visible := False;
    with Series[0] as TPointSeries do
    begin
      XValues.Order := loNone;
      Marks.Callout.Length := 12;
      Marks.Visible := False;
      Marks.BackColor := GraphOptions.LabelsBackColor;
      Marks.Transparent := GraphOptions.LabelsTransparent;
      Marks.Style := TSeriesMarksStyle(0);
    end;
  end;
end;


procedure TGraphForm.RefreshScatterPlot;
//-----------------------------------------------------------------------------
// Refreshes the drawing of a scatter plot.
//-----------------------------------------------------------------------------
var
  J          : LongInt;
  Xobject    : TObject;
  Yobject    : TObject;
  X, Y       : Double;
  Progress   : LongInt;
  ProgStep   : LongInt;
begin
  // Determine number of time periods plotted
  StartPeriod := Uglobals.GetPeriod(Graph.StartDateIndex);
  EndPeriod := Uglobals.GetPeriod(Graph.EndDateIndex);
  TotalPeriods := EndPeriod - StartPeriod + 1;
  ProgStep := TotalPeriods div (MainForm.ProgressBar.Max div
                                MainForm.ProgressBar.Step);
  Progress := 0;
  MainForm.ShowProgressBar('Retrieving data... ');

  // Add each pair of X & Y values to the series being plotted
  with Graph do
  begin
    // Get references to the X-object & Y-Object being plotted
    Xobject := Uglobals.GetObject(XObjectType, Items[0]);
    Yobject := Uglobals.GetObject(ObjectType, Items[1]);
    if (Xobject = nil) or (Yobject = nil) then Exit;

     // Add data to the chart series
    with Chart1.Series[0] do
    begin
      Clear;
      Active := False;
      for J := StartPeriod to EndPeriod do
      begin
        X := Uoutput.GetValue(XObjectType, XvarIndex, J, Xobject);
        Y := Uoutput.GetValue(ObjectType, YvarIndex, J, Yobject);
        if (X <> MISSING) and (Y <> MISSING) then AddXY(X, Y, '', clTeeColor);
        MainForm.UpdateProgressBar(Progress, ProgStep);
      end;
    end;
  end;

  with Chart1.Series[0] as TPointSeries do
  begin
    Pointer.Visible := True;
  end;
  Chart1.Series[0].Active := True;

  // Scale the axes
  SetAxisScale(Chart1.BottomAxis);
  SetAxisScale(Chart1.LeftAxis);

end;


procedure TGraphForm.SetGraphOptions;
//-----------------------------------------------------------------------------
// Launches the Graph Options dialog form.
//-----------------------------------------------------------------------------
var
  OldDateTimeFormat: String;
begin
  OldDateTimeFormat := GraphOptions.DateTimeFormat;
  Ugraph.SetGraphOptions(Chart1, OptionPage, self);
  if not SameText(OldDateTimeFormat, GraphOptions.DateTimeFormat)
  then Uglobals.HasChanged := True;
end;


procedure TGraphForm.CopyTo;
//-----------------------------------------------------------------------------
// Launches a CopyTo dialog form to copy the graph to a file or to the
// clipboard.
//-----------------------------------------------------------------------------
begin
  Ugraph.CopyTo(Chart1);
end;


procedure TGraphForm.Print(Destination: TDestination);
//-----------------------------------------------------------------------------
// Prints the graph to the printer or to a preview window.
//-----------------------------------------------------------------------------
begin
  Ugraph.Print(Chart1, MainForm.thePrinter, Destination);
end;

end.
