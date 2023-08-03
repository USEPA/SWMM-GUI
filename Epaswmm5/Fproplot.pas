unit Fproplot;

{-------------------------------------------------------------------}
{                    Unit:    Fproplot.pas                          }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21  (5.2.0)                     }
{                    Author:  L. Rossman                            }
{                                                                   }
{   MDI child form that displays a profile plot.                    }
{   The plot is created by calling CreatePlot. It gets refreshed    }
{   (after a new analysis is made) by calling RefreshPlot.          }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Series, TeEngine, ExtCtrls, TeeProcs, Chart, TeeGDIPlus, Buttons,
  Vcl.Themes, System.Types, Math, System.UITypes, VCLTee.TeCanvas,
  StdCtrls, XPrinter, Uproject, Uglobals, Uutils;

type

  TLinkData = record
    Z1: Single;
    Z2: Single;
    MaxDepth: Single;
    Length: Single;
  end;

  TNodeData = record
    Invert: Single;
    MaxDepth: Single;
    ID: String;
  end;

  TElevPoints = record
    X:    LongInt;
    Y:    LongInt;
    Yinv: LongInt;
    Ycrn: LongInt;
  end;

  TProfilePlotForm = class(TForm)
    Chart1: TChart;
    LinkInvert: TLineSeries;
    LinkCrown: TLineSeries;
    NodeInvert: TPointSeries;
    RefreshBtn: TSpeedButton;
    GroundLine: TLineSeries;

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure Chart1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LinkInvertBeforeDrawValues(Sender: TObject);
    procedure NodeInvertAfterDrawValues(Sender: TObject);
    procedure RefreshBtnClick(Sender: TObject);

  private
    { Private declarations }
    StartNode:  TNode;                 //Node where profile begins
    LinkCount:  Integer;               //Number of links in profile
    PlotPeriod: LongInt;               //Time period being plotted
    LinksList:  TStringList;           //Links that comprise the profile
    LinkData:   TLinkData;             //Data needed to plot a link's profile
    NodeData:   TNodeData;             //Data needed to plot a node
    MainTitle:  String;
    Xtitle:     String;
    Ytitle:     String;
    Yoffset:    Double;
    AutoScale:  Boolean;
    HasResults: Boolean;
    IncludeHGL: Boolean;
    ShowGround: Boolean;
    HGLmax:     Double;
    TheGraphOptions: TGraphOptions;
    TheProfileOptions: TProfileOptions;
    procedure CopyToBitmap(const Fname: String);
    procedure CopyToMetafile(const Fname: String);
    procedure CopyOptions(const GraphOptions1: TGraphOptions;
              const ProfileOptions1: TProfileOptions;
              var GraphOptions2: TGraphOptions;
              var ProfileOptions2: TProfileOptions);
    procedure DrawHGL(const E1, E2: TElevPoints);
    procedure GetLinkData(aLink: TLink);
    procedure GetNodeData(aNode: TNode);
    procedure GetStartNode;
    procedure SetupAxis(Axis: TChartAxis);
    procedure SetupPlot;

    procedure ScaleYaxis;
    function  GetHGL(const TimePeriod: LongInt; const Zindex: Integer): Double;

    procedure GetNodeElevations(const aNode: TNode; const X: Double;
              const Z: Single; const Ylink: Single; const V: Integer;
              var E: TElevPoints);
    procedure DrawNodeElev(const X: Double; const Y: Double; const Z: Double);
    function  GetWaterFilledPoly(var Points: array of TPoint;
              E1, E2: TElevPoints): Integer;
    function  Intersection(const X1: LongInt; const X2: LongInt;
              const Y1: LongInt; const Y2: LongInt; const Y3: LongInt;
              const Y4: LongInt): TPoint;
    function  GetOffset(aLink: TLink; Index: Integer; aNode: TNode): Single;

  public
    { Public declarations }
    procedure CopyTo;
    function  CreatePlot(ReportSelection: TReportSelection): Boolean;
    procedure Print(Destination: TDestination);
    procedure RefreshPlot;
    procedure SetPlotOptions;
    procedure UpdatePlot(TimePeriod: LongInt);
  end;

//var
//  ProfilePlotForm: TProfilePlotForm;

implementation

{$R *.dfm}

uses Dcopy, Dproplot, Fmain, Fmap, Uoutput, Ugraph;

const
  MINLENGTH = 5; //10;
  TXT_DISTANCE = 'Distance';
  TXT_ELEVATION = 'Elevation';
  TXT_HT_ABOVE = 'Height Above';
  TXT_MAIN_TITLE = 'Water Elevation Profile';

procedure TProfilePlotForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
// Form's OnCreate handler.
//-----------------------------------------------------------------------------
begin
  CopyOptions(GraphOptions, ProfileOptions, TheGraphOptions, TheProfileOptions);
  MainTitle := '';
  Xtitle := '';
  Ytitle := '';
  AutoScale := True;
  Yoffset := 0;
  LinksList := TStringList.Create;
  SetupPlot;
  RefreshBtn.Visible := not Uglobals.RunFlag;
  Chart1.AutoRepaint := False;
  Chart1.Walls.Back.Gradient.Visible := False;

  // Create a TeeChart GDIplus canvas for antialiasing
  Chart1.Canvas := TGDIPlusCanvas.Create;

  // Make the left mouse zoom when shift key is pressed
  Chart1.Zoom.KeyShift :=[ssShift];
  Chart1.ScrollMouseButton := mbLeft;
end;


procedure TProfilePlotForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
//-----------------------------------------------------------------------------
// Form's OnClose handler.
//-----------------------------------------------------------------------------
begin
  Action := caFree;
end;


procedure TProfilePlotForm.FormDestroy(Sender: TObject);
//-----------------------------------------------------------------------------
// Form's OnDestroy handler.
//-----------------------------------------------------------------------------
begin
  LinksList.Free;
end;


procedure TProfilePlotForm.Chart1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
//-----------------------------------------------------------------------------
// OnMouseDown handler for the Chart component. Launches the dialog that
// sets plotting options.
//-----------------------------------------------------------------------------
begin
  if Button = mbRight then SetPlotOptions;
end;


procedure TProfilePlotForm.RefreshBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the Refresh button.
//-----------------------------------------------------------------------------
begin
  RefreshPlot;
end;


procedure TProfilePlotForm.SetupPlot;
//-----------------------------------------------------------------------------
// Initializes the properties of the TChart component used to display
// the profile plot.
//-----------------------------------------------------------------------------
begin
  with Chart1 do
  begin
    Walls.Back.Color := TheGraphOptions.BackColor;
    if TheGraphOptions.PanelColor = -1
    then Color := Integer(StyleServices.GetStyleColor(scPanel))
    else Color := TheGraphOptions.PanelColor;

    Title.Text.Clear;
    Title.Font.Name := TheGraphOptions.TitleFontName;
    Title.Font.Size := TheGraphOptions.TitleFontSize;
    Title.Font.Color := TheGraphOptions.TitleFontColor;
    Title.Font.Style := [];
    if TheGraphOptions.TitleFontBold
    then Title.Font.Style := Title.Font.Style + [fsBold];
    if TheGraphOptions.TitleFontItalic
    then Title.Font.Style := Title.Font.Style + [fsItalic];

    SetupAxis(BottomAxis);
    SetupAxis(LeftAxis);
    TopAxis.Grid.Visible := False;
    TopAxis.LabelStyle := talText;
    TopAxis.LabelsAngle := 90;
    TopAxis.LabelsFont.Size := TheProfileOptions.LabelsFontSize;
    TopAxis.LabelsFont.Style := [];
    if TheGraphOptions.AxisFontBold
    then TopAxis.LabelsFont.Style := TopAxis.LabelsFont.Style + [fsBold];
    if TheGraphOptions.AxisFontItalic
    then TopAxis.LabelsFont.Style := TopAxis.LabelsFont.Style + [fsItalic];
    TopAxis.Labels := TheProfileOptions.LabelsOnAxis;
  end;

  with GroundLine.Marks do
  begin
    BackColor := Chart1.BackColor;
    Transparent := False;
    Visible := TheProfileOptions.LabelsOnPlot;
    ArrowLength := TheProfileOptions.LabelsArrowLength;
    Font.Size := TheProfileOptions.LabelsFontSize;
    Font.Style := [];
    if TheGraphOptions.AxisFontBold
    then Font.Style := Font.Style + [fsBold];
    if TheGraphOptions.AxisFontItalic
    then Font.Style := Font.Style + [fsItalic];
  end;

  with NodeInvert.Pointer do
  begin
    HorizSize := 7;
    VertSize := 1;
    Pen.Visible := False;
    InflateMargins := True;
    Brush.Style := bsClear;
  end;
end;


procedure TProfilePlotForm.SetupAxis(Axis: TChartAxis);
//-----------------------------------------------------------------------------
// Initializes the properties of a TeeChart's Axis component.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  with Axis do
  begin
    if Horizontal then I := 0
    else I := 1;
    Grid.Visible := TheGraphOptions.AxisGridStyle[I] > 0;
    Title.Font.Name := TheGraphOptions.AxisFontName;
    Title.Font.Size := TheGraphOptions.AxisFontSize;
    Title.Font.Style := [];
    if TheGraphOptions.AxisFontBold
    then Title.Font.Style := Title.Font.Style + [fsBold];
    if TheGraphOptions.AxisFontItalic
    then Title.Font.Style := Title.Font.Style + [fsItalic];
    LabelsFont.Assign(Title.Font);
  end;
end;


function TProfilePlotForm.CreatePlot(ReportSelection: TReportSelection):
  Boolean;
//-----------------------------------------------------------------------------
// Creates a new profile plot based on the options contained in
// ReportSelection.
//-----------------------------------------------------------------------------
var
  I, J, K: Integer;
  LastNode: TNode;
  aLink: TLink;
begin
  with ReportSelection do
  begin
    LinksList.Assign(Items);
    LinkCount := LinksList.Count;
    if RunFlag then PlotPeriod := CurrentPeriod
    else PlotPeriod := -1;
  end;
  HasResults := False;
  IncludeHGL := True;
  ShowGround := True;
  GetStartNode;

  LastNode := StartNode;
  for K := 0 to LinkCount-1 do
  begin
    if not Project.FindLink(LinksList[K], I, J) then continue;
    aLink := Project.GetLink(I,J);
    if aLink.Node1 <> LastNode then LastNode := aLink.Node1
    else LastNode := aLink.Node2;
  end;
  MainTitle := TXT_MAIN_TITLE;
  if (LastNode <> nil) and (StartNode <> nil) then
  begin
      MainTitle := MainTitle + ': Node ' + StartNode.ID + ' - ' + LastNode.ID;
      Caption := 'Profile: Node ' + StartNode.ID + ' - ' + LastNode.ID;
  end;

  Xtitle := TXT_DISTANCE + ' (' + LinkUnits[DIAMETER].Units + ')';
  Ytitle := TXT_ELEVATION + ' (' + LinkUnits[DIAMETER].Units + ')';
  RefreshPlot;
  Result := True;
end;


procedure TProfilePlotForm.GetStartNode;
//-----------------------------------------------------------------------------
// Finds the start (upstream) node in the profile plot.
//-----------------------------------------------------------------------------
var
  I, J: Integer;
  Link1, Link2: TLink;
begin
  StartNode := nil;
  if LinkCount = 0 then exit;

  // Assume start node is upstream node of first link in profile
  if not Project.FindLink(LinksList[0], I, J) then exit;
  Link1 := Project.GetLink(I, J);
  StartNode := Link1.Node1;
  if (LinkCount = 1) or not Project.FindLink(LinksList[1], I, J) then exit;

  // If this node is shared with the second link in the profile, then make
  // the start node be the downstream node of the first link
  Link2 := Project.GetLink(I, J);
  if (StartNode = Link2.Node1) or (StartNode = Link2.Node2)
  then StartNode := Link1.Node2;
end;

procedure TProfilePlotForm.RefreshPlot;
//-----------------------------------------------------------------------------
// Refreshes the profile plot.
//-----------------------------------------------------------------------------
var
  I: Integer;
  J: Integer;
  K: Integer;
  X: Double;
  Y: Double;
  aLink: TLink;
  Node1: TNode;
  Node2: TNode;
  LastNode: TNode;
  Yinvert: Double;
  Yground: Double;
  Xstart : Double;
  Xfinish: Double;
  Z1: Single;
  Z2: Single;

begin
  X := 0;
  Y := 0;
  Xstart := MISSING;
  Xfinish := MISSING;
  LastNode := StartNode;

  // Hide the Refresh button if computed results are available
  Chart1.AutoRepaint := False;
  if Uglobals.RunFlag then RefreshBtn.Visible := False;

  // Clear all chart series
  for K := 0 to Chart1.SeriesCount-1 do
  begin
    Chart1.Series[K].Clear;
    Chart1.Series[K].Active := False;
  end;
  HasResults := True;

  // For each link in the profile
  for K := 0 to LinkCount-1 do
  begin
    // Retrieve data for link
    if not Project.FindLink(LinksList[K], I, J) then continue;
    aLink := Project.GetLink(I, J);
    GetLinkData(aLink);
    if Uglobals.RunFlag and (aLink.OutFileIndex < 0) then HasResults := False;

    // Determine which node is the upstream node (Node1)
    Node1 := aLink.Node1;
    Node2 := aLink.Node2;
    Z1 := LinkData.Z1;
    Z2 := LinkData.Z2;
    if Node1 <> LastNode then
    begin
      Node1 := aLink.Node2;
      Node2 := aLink.Node1;
      Z1 := LinkData.Z2;
      Z2 := LinkData.Z1;
    end;
    LastNode := Node2;
    if Uglobals.RunFlag then
    begin
      if (Node1.OutFileIndex < 0) or (Node2.OutFileIndex < 0) then HasResults := False;
    end;

    // Retrieve data for upstream node
    if (Xstart = MISSING) and (Node1.X <> MISSING) then Xstart := Node1.X;
    GetNodeData(Node1);

    // Add link invert elev. to plot
    Yinvert := NodeData.Invert + Z1;
    LinkInvert.AddXY(X, Yinvert, '', clTeeColor);
    LinkCrown.AddXY(X, LinkData.MaxDepth + Yinvert, '', clTeeColor);
    NodeInvert.AddXY(X, NodeData.Invert, NodeData.ID, clTeeColor);

    // Compute elev. of link crown
    Y := Yinvert + LinkData.MaxDepth;

    // Find ground elev. of node
    Yground := NodeData.Invert + NodeData.MaxDepth;
    if Yground < Y then Yground := Y;

    // Add elev. of node's max. depth to ground line on plot
    Y := Yground;
    if IncludeHGL then
    begin
      GroundLine.AddXY(X, Y, NodeData.ID, clTeeColor);
    end;

    // Get data for downstream node
    if Node2.X <> MISSING then Xfinish := Node2.X;
    GetNodeData(Node2);
    X := X + LinkData.Length;

    // Update link invert and crown at downstream end
    Yinvert := NodeData.Invert + Z2;
    LinkInvert.AddXY(X, Yinvert, '', clTeeColor);
    LinkCrown.AddXY(X, LinkData.MaxDepth + Yinvert, '', clTeeColor);
    Y := Yinvert + LinkData.MaxDepth;
  end;

  // Add ground elev. of last node to plot
  Yground := NodeData.Invert + NodeData.MaxDepth;
  if Yground < Y then Yground := Y;
  Y := Yground;
  if IncludeHGL then
  begin
    GroundLine.AddXY(X, Y, NodeData.ID, clTeeColor);
  end;
  NodeInvert.AddXY(X, NodeData.Invert, NodeData.ID, clTeeColor);

  // Invert X-axis if starting node is to the right of finishing node
  if Xstart > Xfinish then
  begin
    Chart1.BottomAxis.Inverted := True;
    Chart1.TopAxis.Inverted := True;
  end;

  // Add chart and axis titles
  Chart1.BottomAxis.Title.Caption := Xtitle;
  Chart1.LeftAxis.Title.Caption := Ytitle;
  Chart1.Title.Text.Clear;
  Chart1.Title.Text.Add(MainTitle);

  // Redraw water depths for current time period
  UpdatePlot(PlotPeriod);
  Chart1.AutoRepaint := True;
  Chart1.Refresh;
end;

procedure TProfilePlotForm.UpdatePlot(TimePeriod: LongInt);
//-----------------------------------------------------------------------------
// Updates the water levels shown on the plot for a given time period.
//-----------------------------------------------------------------------------
var
  I: Integer;
  J: Integer;
  K: Integer;
  X: Double;
  LastNode: TNode;
  Node1: TNode;
  Node2: TNode;
  aLink: TLink;

begin

  // No display if no results available
  if Uglobals.RunFlag and not HasResults then
  begin
    Chart1.Foot.Text.Clear;
    Chart1.Foot.Text.Add(
      'Reported results are not available for all objects in the profile.');
    Exit;
  end;

  // Initialize maximum HGL value shown on plot
  HGLmax := -1.0e50;

  // Update plot's time period
  if TimePeriod >= 0 then PlotPeriod := TimePeriod;

  // Begin with the first node of the profile
  LastNode := StartNode;
  X := 0;

  // For each link in the profile
  for K := 0 to LinkCount-1 do
  begin
    // Retrieve data for the link
    if not Project.FindLink(LinksList[K], I, J) then continue;
    aLink := Project.GetLink(I, J);
    GetLinkData(aLink);

    // Determine which node is the upstream node (Node1)
    Node1 := aLink.Node1;
    Node2 := aLink.Node2;
    if Node1 <> LastNode then
    begin
      Node1 := aLink.Node2;
      Node2 := aLink.Node1;
    end;
    LastNode := Node2;
    GetNodeData(Node1);
    HGLmax := Max(HGLmax, GetHGL(TimePeriod, Node1.OutFileIndex));

    // Get data for the downstream node
    X := X + LinkData.Length;
    GetNodeData(Node2);
  end;
  HGLmax := Max(HGLmax, GetHGL(TimePeriod, LastNode.OutFileIndex));

  // Re-scale the Y-axis if automatic scaling is in effect
  if AutoScale then ScaleYaxis;

  // Re-activate all data series
  for K := 0 to Chart1.SeriesCount-1 do
  begin
    Chart1.Series[K].Active := True;
  end;

  if not IncludeHGL or not ShowGround
  then GroundLine.Active := False
  else GroundLine.Active := True;
  GroundLine.LinePen.Width := TheProfileOptions.LineWidth;
  LinkInvert.LinePen.Width := TheProfileOptions.LineWidth;
  LinkCrown.LinePen.Width := TheProfileOptions.LineWidth;

  // Add date/time to the plot's footer
  Chart1.Foot.Text.Clear;
  Chart1.Foot.Text.Add(MapForm.TimeLegendPanel.Caption);
end;

function  TProfilePlotForm.GetHGL(const TimePeriod: LongInt;
                                  const Zindex: Integer): Double;
//-----------------------------------------------------------------------------
//  Finds the larger of a node's ground elevation and hydraulic grade line
//-----------------------------------------------------------------------------
var
  HGL1, HGL2: Double;
  Vnode: Integer;
begin
    Vnode := Uoutput.GetVarIndex(Uglobals.HEAD, NODES);
    HGL1 := NodeData.Invert + NodeData.MaxDepth;
    HGL2 := MISSING;
    if Uglobals.RunFlag then
    begin
      HGL2 := Uoutput.GetNodeOutVal(Vnode, TimePeriod, Zindex);
    end;
    Result := Max(HGL1, HGL2);
end;

procedure TProfilePlotForm.ScaleYaxis;
//-----------------------------------------------------------------------------
//  Automatically scales the Y-axis of the plot.
//-----------------------------------------------------------------------------
var
  Ymin, Ymax: Double;
begin
  with Chart1.LeftAxis do
  begin
    Automatic := True;
    Ymin := Chart1.MinYValue(Chart1.LeftAxis);
    Ymax := Chart1.MaxYValue(Chart1.LeftAxis);
    if HGLmax > Ymax then Ymax := HGLmax;
    Ymax := Ymax + 0.1*(Ymax-Ymin);
    SetMinMax(Ymin, Ymax);
    try
      Ymin := Math.Floor(Ymin);
      Ymax := Math.Ceil(Ymax);
      SetMinMax(Ymin, Ymax);
    except
    end;
  end;
end;


procedure TProfilePlotForm.LinkInvertBeforeDrawValues(Sender: TObject);
//-----------------------------------------------------------------------------
// This is the "BeforeDrawValues" event handler for the LinkInvert data
// series that draws the water filled portion of each link before the
// link boundaries are drawn.
//-----------------------------------------------------------------------------
var
  I: Integer;
  J: Integer;
  K: Integer;
  N: Integer;
  Vnode: Integer;
  Vlink: Integer;
  X: Double;
  Z1, Z2: Single;
  Ylink: Single;
  LastNode: TNode;
  Node1: TNode;
  Node2: TNode;
  aLink: TLink;
  E1, E2, E: TElevPoints;
  Points: array[0..5] of TPoint;

begin
  // Get the index of the node depth variable in the output results
  Vnode := Uoutput.GetVarIndex(Uglobals.NODEDEPTH, NODES);
  Vlink := Uoutput.GetVarIndex(Uglobals.LINKDEPTH, LINKS);
  Ylink := 0.0;

  // Set the chart's pen style to clear and fill style to solid
  Chart1.Canvas.ClipRectangle(Chart1.ChartRect);
  Chart1.Canvas.Pen.Style := psClear;
  Chart1.Canvas.Pen.Color := clBlue;
  Chart1.Canvas.Brush.Style := bsSolid;

    // Begin with the first node of the profile
    LastNode := StartNode;
    X := 0;

    // For each link in the profile
    for K := 0 to LinkCount-1 do
    begin

      // Retrieve data for the link
      if not Project.FindLink(LinksList[K], I, J) then continue;
      aLink := Project.GetLink(I, J);
      GetLinkData(aLink);

      // Determine which node is the upstream node (Node1)
      Node1 := aLink.Node1;
      Node2 := aLink.Node2;
      Z1 := LinkData.Z1;
      Z2 := LinkData.Z2;
      if Node1 <> LastNode then
      begin
        Node1 := aLink.Node2;
        Node2 := aLink.Node1;
        Z1 := LinkData.Z2;
        Z2 := LinkData.Z1;
      end;
      LastNode := Node2;

      // Retrieve water depth for link if not using dynamic wave routing
      if not DynWaveFlag
      then Ylink := Uoutput.GetLinkOutVal(Vlink, PlotPeriod, aLink.OutFileIndex);

      // Get elevations at upstream & downstream nodes
      GetNodeElevations(Node1, X, Z1, Ylink, Vnode, E1);
      X := X + LinkData.Length;
      GetNodeElevations(Node2, X, Z2, Ylink, Vnode, E2);

      // Check if link has no length
      if LinkData.Length = 0 then continue;

      // Make E1 be the node at higher water elevation
      // (Remember that pixel Y-values decrease as real Y-values increase)
      if E2.Y < E1.Y then
      begin
        E := E1;
        E1 := E2;
        E2 := E;
      end;

      // Do not allow water line to drop below conduit invert
      if E1.Y > E1.Yinv then E1.Y := E1.Yinv;
      if E2.Y > E2.Yinv then E2.Y := E2.Yinv;

      // Fill in the link's area with the conduit's background color
      Points[0] := Point(E1.X, E1.Yinv);
      Points[1] := Point(E1.X, E1.Ycrn);
      Points[2] := Point(E2.X, E2.Ycrn);
      Points[3] := Point(E2.X, E2.Yinv);
      with Chart1.Canvas do
      begin
        Brush.Color := TheProfileOptions.ConduitColor;
        Polygon(Slice(Points, 4));
      end;

      // For Dynamic Wave routing runs, find the polygon points that
      // bound the water-filled area of the link
      if DynWaveFlag then N := getWaterFilledPoly(Points, E1, E2)

      // For non-dynamic wave routing, simply use the computed link flow depth
      else
      begin
        Points[1] := Point(E1.X, E1.Y);
        Points[2] := Point(E2.X, E2.Y);
        N := 4;
      end;

      // Fill-in the water-filled area of the link
      if N > 0 then with Chart1.Canvas do
      begin
        Brush.Color := TheProfileOptions.WaterColor;
        Polygon(Slice(Points, N));
      end;

      // Draw the Hydraulic Grade Line
      if IncludeHGL then DrawHGL(E1, E2);
    end;
    Chart1.Canvas.Pen.Style := psSolid;
end;

procedure TProfilePlotForm.NodeInvertAfterDrawValues(Sender: TObject);
//--------------------------------------------------------------------
// This is the "AfterDrawValues" event handler for the NodeInvert
// data series that draws the node cylinders on the profile plot.
//--------------------------------------------------------------------
var
  I: Integer;
  J: Integer;
  K: Integer;
  Vnode: Integer;
  X, Y: Double;
  Z1, Z2: Double;
  LastNode: TNode;
  Node1: TNode;
  Node2: TNode;
  aLink: TLink;

begin
  // Set the chart's pen style to clear and fill style to solid
  Chart1.Canvas.ClipRectangle(Chart1.ChartRect);
  Chart1.Canvas.Pen.Style := psSolid;
  Chart1.Canvas.Pen.Color := clBlack;
  Chart1.Canvas.Brush.Style := bsSolid;
  Vnode := Uoutput.GetVarIndex(Uglobals.NODEDEPTH, NODES);

    // Begin with the first node of the profile
    LastNode := StartNode;
    X := 0;
    Z2 := 0;

    // For each link in the profile
    for K := 0 to LinkCount-1 do
    begin
      // Retrieve data for the link
      if not Project.FindLink(LinksList[K], I, J) then continue;
      aLink := Project.GetLink(I, J);
      GetLinkData(aLink);

      // Determine which node is the upstream node (Node1)
      Node1 := aLink.Node1;
      Node2 := aLink.Node2;
      Z1 := LinkData.Z1;
      Z2 := LinkData.Z2;
      if Node1 <> LastNode then
      begin
        Node1 := aLink.Node2;
        Node2 := aLink.Node1;
        Z1 := LinkData.Z2;
        Z2 := LinkData.Z1;
      end;
      LastNode := Node2;
      GetNodeData(Node1);

      //  No water depth if no simulation was run.
      if (PlotPeriod >= 0) and (Node1.OutFileIndex >= 0)
      then Y := Uoutput.GetNodeOutVal(Vnode, PlotPeriod, Node1.OutFileIndex)
      else Y := 0;
      if Y > NodeData.MaxDepth then Y := NodeData.MaxDepth;

      // Draw node cylinder plus ground line
      if IncludeHGL then DrawNodeElev(X, Y, Z1);

      // Get data for the downstream node
{
      if LinkData.Length = 0 then with Chart1.BottomAxis do
      begin
        X := CalcPosPoint(CalcXPosValue(X) + 4);
      end
      else
}
      X := X + LinkData.Length;
      GetNodeData(Node2);
    end;

    // Add water elev. of the last node to the plot
    if LastNode <> nil then
    begin

      // No water depth if no simulation was run.
      if (PlotPeriod >= 0) and (LastNode.OutFileIndex >= 0)
      then Y := Uoutput.GetNodeOutVal(Vnode, PlotPeriod, LastNode.OutFileIndex)
      else Y := 0;
      if Y > NodeData.MaxDepth then Y := NodeData.MaxDepth;
      if IncludeHGL then DrawNodeElev(X, Y, Z2);
    end;
end;

procedure TProfilePlotForm.DrawNodeElev(const X: Double; const Y: Double;
  const Z: Double);
var
  X1, X2: Integer;
  Y1, Y2, Y3: Integer;
  Yinvert, Ydepth, Ycrown: Double;
begin
  X1 := Chart1.BottomAxis.CalcXPosValue(X) - 3;
  X2 := X1 + 7;
  Yinvert := NodeData.Invert;
  Ydepth := Yinvert + Y;
  Ycrown := Z + LinkData.MaxDepth;
  if NodeData.MaxDepth > Ycrown then Ycrown := NodeData.MaxDepth;
  Ycrown := Yinvert + Ycrown;
  with Chart1.LeftAxis do
  begin
    Y1 := CalcYPosValue(Yinvert);
    Y2 := CalcYPosValue(Ycrown);
    Y3 := CalcYPosValue(Ydepth);
  end;
  with Chart1.Canvas do
  begin
    Brush.Color := theProfileOptions.ConduitColor;
    Rectangle(X1, Y2, X2, Y1);
    Brush.Color := theProfileOptions.WaterColor;
    Rectangle(X1, Y3, X2, Y1);
  end;
end;

procedure TProfilePlotForm.GetNodeElevations(const aNode: TNode;
  const X: Double; const Z: Single; const Ylink: Single; const V: Integer;
  var E: TElevPoints);
//-----------------------------------------------------------------------------
// Converts the link invert and crown elevations for a node as
// well as the node's water surface elevation into pixel locations.
//-----------------------------------------------------------------------------
var
  Y: Double;
begin
  E.X := Chart1.BottomAxis.CalcXPosValue(X);
  GetNodeData(aNode);
  Y := NodeData.Invert - Yoffset;
  E.Yinv := Chart1.LeftAxis.CalcYPosValue(Y + Z);
  E.Ycrn := Chart1.LeftAxis.CalcYPosValue(Y + Z + LinkData.MaxDepth);

  if (PlotPeriod >= 0) and (aNode.OutFileIndex >= 0) then
  begin
    if DynWaveFlag
    then Y := Y + Uoutput.GetNodeOutVal(V, PlotPeriod, aNode.OutFileIndex)
    else Y := Y + Z + Ylink;
  end;

  E.Y := Chart1.LeftAxis.CalcYPosValue(Y);
end;


function TProfilePlotForm.GetWaterFilledPoly(var Points: array of TPoint;
  E1, E2: TElevPoints): Integer;
//-----------------------------------------------------------------------------
// Determines the vertices of the polygon (stored in Points) that defines
// the water-filled area of a link.
//
// **************************** N O T E **********************************
// Elevations have been converted to pixels with origin in the upper left.
// So saying that point Y1 is above point Y2 means that the numerical value
// of Y1 is less than Y2.
//-----------------------------------------------------------------------------
var
  N: Integer;
begin

  // 1. Case where water line is above link crown at both ends
  if (E1.Y <= E1.Ycrn) and (E2.Y <= E2.Ycrn) then
  begin
    Points[0] := Point(E1.X, E1.Yinv);
    Points[1] := Point(E1.X, E1.Ycrn);
    Points[2] := Point(E2.X, E2.Ycrn);
    Points[3] := Point(E2.X, E2.Yinv);
    N := 4
  end

  // 2. Case where water line is below link invert at both ends
  else if (E1.Y >= E1.Yinv) and (E2.Y >= E2.Yinv) then N := 0

  // 3. Case where high end of water line is above link crown
  else if E1.Y < E1.Ycrn then
  begin
    // 3a. Low end of water line is above link invert
    if E2.Y < E2.Yinv then
    begin
      Points[0] := Point(E1.X, E1.Ycrn);
      Points[1] := Intersection(E1.X, E2.X, E1.Y, E2.Y, E1.Ycrn, E2.Ycrn);
      Points[2] := Point(E2.X, E2.Y);
      Points[3] := Point(E2.X, E2.Yinv);
      Points[4] := Point(E1.X, E1.Yinv);
      N := 5;
    end

    // 3b. Low end of water line is below link invert
    else
    begin
      Points[0] := Point(E1.X, E1.Ycrn);
      Points[1] := Intersection(E1.X, E2.X, E1.Y, E2.Y, E1.Ycrn, E2.Ycrn);
      Points[2] := Intersection(E1.X, E2.X, E1.Y, E2.Y, E1.Yinv, E2.Yinv);
      Points[3] := Point(E1.X, E1.Yinv);
      N := 4;
    end;
  end

  // 4. High end of water line is above link invert
  else if E1.Y < E1.Yinv then
  begin
    // 4a. Low end of water line is above link crown
    if E2.Y < E2.Ycrn then
    begin
      Points[0] := Point(E1.X, E1.Y);
      Points[1] := Intersection(E1.X, E2.X, E1.Y, E2.Y, E1.Ycrn, E2.Ycrn);
      Points[2] := Point(E2.X, E2.Ycrn);
      Points[3] := Point(E2.X, E2.Yinv);
      Points[4] := Point(E1.X, E1.Yinv);
      N := 5;
    end

    // 4b. Low end of water line is above link invert
    else if E2.Y < E2.Yinv then
    begin
      Points[0] := Point(E1.X, E1.Y);
      Points[1] := Point(E2.X, E2.Y);
      Points[2] := Point(E2.X, E2.Yinv);
      Points[3] := Point(E1.X, E1.Yinv);
      N := 4;
    end

    // 4c. Low end of water line is below link invert
    else
    begin
      Points[0] := Point(E1.X, E1.Y);
      Points[1] := Intersection(E1.X, E2.X, E1.Y, E2.Y, E1.Yinv, E2.Yinv);
      Points[2] := Point(E1.X, E1.Yinv);
      N := 3;
    end;
  end

  // 5. High end of water line is below link invert
  else
  begin
    // 5a. Low end of water line is above link crown
    if E2.Y < E2.Ycrn then
    begin
      Points[0] := Intersection(E1.X, E2.X, E1.Y, E2.Y, E1.Yinv, E2.Yinv);
      Points[1] := Intersection(E1.X, E2.X, E1.Y, E2.Y, E1.Ycrn, E2.Ycrn);
      Points[2] := Point(E2.X, E2.Ycrn);
      Points[3] := Point(E2.X, E2.Yinv);
      N := 4;
    end

    // 5b. Low end of water line is below link crown
    else
    begin
      Points[0] := Intersection(E1.X, E2.X, E1.Y, E2.Y, E1.Yinv, E2.Yinv);
      Points[1] := Point(E2.X, E2.Y);
      Points[2] := Point(E2.X, E2.Yinv);
      N := 3;
    end;
  end;
  Result := N;
end;

procedure TProfilePlotForm.DrawHGL(const E1, E2: TElevPoints);
//-----------------------------------------------------------------------------
// Draws the Hydraulic Grade Line on the profile plot.
// See note about pixel elevations in GetWaterFilledPoly function above.
//-----------------------------------------------------------------------------
var
  P: TPoint;
begin
  if (E1.Y >= E1.Yinv) and (E2.Y >= E2.Yinv) then Exit;
  with Chart1.Canvas do
  begin
    Pen.Style := psSolid;
    if (E1.Y <= E1.Yinv) and (E2.Y <= E2.Yinv) then
    begin
      MoveTo(E1.X, E1.Y);
      LineTo(E2.X, E2.Y);
    end
    else
    begin
      P := Intersection(E1.X, E2.X, E1.Y, E2.Y, E1.Yinv, E2.Yinv);
      if E1.Y <= E1.Yinv then
      begin
        MoveTo(E1.X, E1.Y);
        LineTo(P.X, P.Y);
      end
      else
      begin
        MoveTo(E2.X, E2.Y);
        LineTo(P.x, P.Y);
      end;
    end;
    Pen.Style := psClear;
  end;
end;

function TProfilePlotForm.Intersection(const X1: LongInt; const X2: LongInt;
  const Y1: LongInt; const Y2: LongInt; const Y3: LongInt;
  const Y4: LongInt): TPoint;
//-----------------------------------------------------------------------------
// Finds the intersection point of lines (X1,Y1)-(X2,Y2) and (X1,Y3)-(X2,Y4).
//-----------------------------------------------------------------------------
var
  D: Double;
  Ua: Double;
  X,Y: Double;
begin
  D := (Y4-Y3) - (Y2-Y1);
  if D = 0.0 then Result := Point(X2, Y2)
  else
  begin
    Ua := (Y1-Y3);
    Ua := Ua / D;
    X := X1 + Ua*(X2-X1);
    Y := Y1 + Ua*(Y2-Y1);
    Result := Point(Round(X), Round(Y));
  end;
end;

function  TProfilePlotForm.GetOffset(aLink: TLink; Index: Integer;
  aNode: TNode): Single;
//-----------------------------------------------------------------------------
// Converts an absolute link offset to a relative one.
//-----------------------------------------------------------------------------
var
  Z, E: Single;
begin
  if SameText(Project.Options.Data[LINK_OFFSETS_INDEX], 'ELEVATION') then
  begin
    if SameText(Trim(aLink.Data[Index]), '*') then Z := 0
    else
    begin
      Uutils.GetSingle(aLink.Data[INDEX], Z);
      Uutils.GetSingle(aNode.Data[NODE_INVERT_INDEX], E);
      Z := Z - E;
      if Z < 0 then Z := 0;
    end;
  end
  else Uutils.GetSingle(aLink.Data[INDEX], Z);
  Result := Z;
end;


procedure TProfilePlotForm.GetLinkData(aLink: TLink);
//-----------------------------------------------------------------------------
// Retrieves selected input data for a link.
//-----------------------------------------------------------------------------
var
  X: array[0..3] of Single;
  K: Integer;
begin
  K := aLink.OutFileIndex;
  if Uglobals.RunFlag and (K >= 0) then
  begin
    Uoutput.GetLinkFixedData(K, X);
    with LinkData do
    begin
      Z1       := X[0];
      Z2       := X[1];
      MaxDepth := X[2];
      Length   := X[3];
    end;
  end
  else with LinkData do
  begin
    Z1 := 0;
    Z2 := 0;
    MaxDepth := 0;
    Length := 0;
    if aLink.Ltype = CONDUIT then
    begin
      Z1 := GetOffset(aLink, CONDUIT_INLET_HT_INDEX, aLink.Node1);
      Z2 := GetOffset(aLink, CONDUIT_OUTLET_HT_INDEX, aLink.Node2);

      Uutils.GetSingle(aLink.Data[CONDUIT_GEOM1_INDEX], MaxDepth);
      Uutils.GetSingle(aLink.Data[CONDUIT_LENGTH_INDEX], Length);
    end;
    if aLink.Ltype = ORIFICE then
      Z1 := GetOffset(aLink, ORIFICE_BOTTOM_HT_INDEX, aLink.Node1);
    if aLink.Ltype = WEIR then
      Z1 := GetOffset(aLink, WEIR_CREST_INDEX, aLink.Node1);
    if aLink.Ltype = OUTLET then
      Z1 := GetOffset(aLink, OUTLET_CREST_INDEX, aLink.Node1);
  end;
  if aLink.Ltype <> CONDUIT then with LinkData do
  begin
    Z2 := 0;
    MaxDepth := 0;
  end;

  if LinkData.Length = 0 then
  begin
    if Uglobals.UnitSystem = usUS
    then LinkData.Length := MINLENGTH
    else LinkData.Length := 0.3048*MINLENGTH;
  end;

end;


procedure TProfilePlotForm.GetNodeData(aNode: TNode);
//-----------------------------------------------------------------------------
// Retrieves selected input data for a node.
//-----------------------------------------------------------------------------
var
  X: array[0..1] of Single;
  K: Integer;
begin
  K := aNode.OutFileIndex;
  if Uglobals.RunFlag and (K >= 0) then
  begin
    Uoutput.GetNodeFixedData(K, X);
    with NodeData do
    begin
      Invert := X[0];
      MaxDepth := X[1];
    end;
  end
  else with NodeData do
  begin
    Uutils.GetSingle(aNode.Data[NODE_INVERT_INDEX], Invert);
    if      aNode.Ntype = JUNCTION then K := JUNCTION_MAX_DEPTH_INDEX
    else if aNode.Ntype = DIVIDER then K := DIVIDER_MAX_DEPTH_INDEX
    else if aNode.Ntype = STORAGE then K := STORAGE_MAX_DEPTH_INDEX
    else K := -1;
    if K >= 0 then Uutils.GetSingle(aNode.Data[K], MaxDepth)
    else MaxDepth := 0;
  end;
  NodeData.ID := aNode.ID;
end;


procedure TProfilePlotForm.SetPlotOptions;
//-----------------------------------------------------------------------------
// Launches the ProfilePlotOptions dialog to obtain new plotting options.
//-----------------------------------------------------------------------------
var
  TmpMainTitle, TmpXtitle, TmpYtitle: String;
  Ymax: Double;
  OptionsForm: TProfilePlotOptionsForm;
begin
  Ymax := Chart1.MaxYValue(Chart1.LeftAxis);
  if HGLmax > Ymax then Ymax := HGLmax;
  OptionsForm := TProfilePlotOptionsForm.Create(self);
  with OptionsForm do
  try
    LoadOptions(TheGraphOptions, TheProfileOptions, MainTitle, Xtitle, Ytitle,
                IncludeHGL, AutoScale, ShowGround);
    SetLeftAxisScaling(Ymax, Chart1);
    if ShowModal = mrOK then
    begin
      Chart1.AutoRepaint := False;
      UnloadOptions(TheGraphOptions, TheProfileOptions, TmpMainTitle,
                    TmpXtitle, TmpYtitle, IncludeHGL, AutoScale, ShowGround);
      if DefaultBox.Checked
      then CopyOptions(TheGraphOptions, TheProfileOptions,
                       GraphOptions, ProfileOptions);
      Hide;

      // Replace chart's main and axis titles
      MainTitle := TmpMainTitle;
      Xtitle := TmpXtitle;
      Ytitle := TmpYtitle;
      Chart1.Title.Text.Clear;
      Chart1.Title.Text.Add(MainTitle);
      Chart1.LeftAxis.Title.Caption := Ytitle;
      Chart1.BottomAxis.Title.Caption := Xtitle;
      if not AutoScale then GetLeftAxisScaling(Chart1);
      SetupPlot;
      RefreshPlot;
    end;
  finally
    Free;
  end;
end;

procedure TProfilePlotForm.CopyOptions(const GraphOptions1: TGraphOptions;
  const ProfileOptions1: TProfileOptions; var GraphOptions2: TGraphOptions;
  var ProfileOptions2: TProfileOptions);
begin
  with GraphOptions2 do
  begin
    PanelColor       := GraphOptions1.PanelColor;
    BackColor        := GraphOptions1.BackColor;
    TitleFontName    := GraphOptions1.TitleFontName;
    TitleFontColor   := GraphOptions1.TitleFontColor;
    TitleFontSize    := GraphOptions1.TitleFontSize;
    TitleFontBold    := GraphOptions1.TitleFontBold;
    TitleFontItalic  := GraphOptions1.TitleFontItalic;
    AxisFontName     := GraphOptions1.AxisFontName;
    AxisFontSize     := GraphOptions1.AxisFontSize;
    AxisFontBold     := GraphOptions1.AxisFontBold;
    AxisFontItalic   := GraphOptions1.AxisFontItalic;
    AxisGridStyle[0] := GraphOptions1.AxisGridStyle[0];
    AxisGridStyle[1] := GraphOptions1.AxisGridStyle[1];
  end;
  with ProfileOptions2 do
  begin
    LabelsOnAxis      := ProfileOptions1.LabelsOnAxis;
    LabelsOnPlot      := ProfileOptions1.LabelsOnPlot;
    LabelsArrowLength := ProfileOptions1.LabelsArrowLength;
    LabelsFontSize    := ProfileOptions1.LabelsFontSize;
    ConduitColor      := ProfileOptions1.ConduitColor;
    WaterColor        := ProfileOptions1.WaterColor;
    LineWidth         := ProfileOptions1.LineWidth;
  end;
end;


procedure TProfilePlotForm.CopyTo;
//-----------------------------------------------------------------------------
// Copies the plot to a file or to the Clipboard.
//-----------------------------------------------------------------------------
var
  CopyToForm: TCopyToForm;
begin
  // Create the CopyTo dialog form
  CopyToForm := TCopyToForm.Create(self);
  with CopyToForm do
  try

    // Add the appropriate Copy To choices to the dialog
    with FormatGroup do
    begin
      Items.Clear;
      Items.Add('Bitmap');
      Items.Add('Metafile');
      ItemIndex := 0;
    end;

    // Show the dialog and retrieve name of file (DestFileName)
    // (If name is empty then plot is copied to the Clipboard)
    if ShowModal = mrOK then
    begin

      // Copy the plot using the selected format
      case FormatGroup.ItemIndex of
      0: CopyToBitmap(DestFileName);
      1: CopyToMetafile(DestFileName);
      end;
    end;
  finally
    Free;
  end;
end;


procedure TProfilePlotForm.CopyToBitmap(const Fname: String);
//-----------------------------------------------------------------------------
// Copies the plot to file Fname (or to the Clipboard) in bitmap format.
//-----------------------------------------------------------------------------
begin
  if Length(Fname) > 0
  then Chart1.SaveToBitmapFile(Fname)
  else Chart1.CopyToClipboardBitmap;
end;


procedure TProfilePlotForm.CopyToMetafile(const Fname: String);
//-----------------------------------------------------------------------------
// Copies the plot to file Fname (or to the Clipboard) in an enhanced
// metafile format.
//-----------------------------------------------------------------------------
begin
  if Length(Fname) > 0
  then Chart1.SaveToMetafileEnh(Fname)
  else Chart1.CopyToClipBoardMetaFile(True);
end;


procedure TProfilePlotForm.Print(Destination: TDestination);
//-----------------------------------------------------------------------------
// Prints the plot to Destination (printer or preview form).
//-----------------------------------------------------------------------------
begin
  Ugraph.Print(Chart1, MainForm.thePrinter, Destination);
end;

end.
