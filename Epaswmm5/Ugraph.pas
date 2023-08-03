unit Ugraph;

{-------------------------------------------------------------------}
{                    Unit:    Ugraph.pas                            }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/12/22   (5.2.2)                    }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Delphi Pascal unit that contains general purpose graphing       }
{   routines used in conjunction with a TChart object.              }
{-------------------------------------------------------------------}

interface

uses
  Dialogs, Classes, SysUtils, Forms, Controls, Windows,
  Graphics, Clipbrd, TeEngine, Series, Chart, TeeProcs, System.UITypes,
  VCLTee.TeCanvas, PgSetup, Xprinter, Uglobals, Uutils, Dchart,
  Math;

procedure CopyTo(Chart: TChart);
function  CreateBarSeries(Chart: TChart; const Stitle: String): Boolean;
function  CreateAreaSeries(Chart: TChart; const Stitle: String): TAreaSeries;
function  CreateFastLineSeries(Chart: TChart; const Stitle: String): TFastLineSeries;
function  CreateLineSeries(Chart: TChart; const Stitle: String): TLineSeries;
function  CreatePointSeries(Chart: TChart; const Stitle: String): TPointSeries;
procedure InitGraphOptions(Chart: TChart);
procedure InitAreaSeriesOptions(Series: TAreaSeries; I: Integer);
procedure InitLineSeriesOptions(Series: TLineSeries; I: Integer);
procedure InitFastLineSeriesOptions(Series: TFastLineSeries; I: Integer);
procedure Print(Chart: TChart; thePrinter: TPrintControl;
          Destination: TDestination);
procedure SetGraphOptions(theChart: TChart; var startPage: Integer; theForm: TForm);

implementation

uses Dcopy;

procedure CopyToBitmap(const Fname: String; Chart: TChart); forward;
procedure CopyToMetafile(const Fname: String; Chart: TChart); forward;
procedure CopyToString(const Fname: String; Chart: TChart); forward;
procedure InitAxisOptions(Axis: TChartAxis); forward;
procedure SaveDefaultOptions(Chart: TChart;
            const UseDefaultPanelColor: Boolean); forward;


procedure CopyTo(Chart: TChart);
//-----------------------------------------------------------------------------
//  Copies the Chart to a file or to the Clipboard.
//-----------------------------------------------------------------------------
var
  CopyToForm: TCopyToForm;
begin
  // Create the CopyTo dialog form
  CopyToForm := TCopyToForm.Create(Application);
  with CopyToForm do
  try
    Caption := 'Copy Chart';
    if Chart.Tag > 0 then RemoveCopyAsData;

    // Show the dialog and retrieve the name of a file (DestFileName)
    // (If name is empty then chart is copied to the Clipboard)
    if ShowModal = mrOK then
    begin

      // Copy the chart using the selected format
      case FormatGroup.ItemIndex of
      0: CopyToBitmap(DestFileName, Chart);
      1: CopyToMetafile(DestFileName, Chart);
      2: CopyToString(DestFileName, Chart);
      end;
    end;
  finally
    Free;
  end;
end;


function CreateBarSeries(Chart: TChart;  const Stitle: String): Boolean;
//-----------------------------------------------------------------------------
//  Creates a bar data series in a Chart.
//-----------------------------------------------------------------------------
var
  aSeries: TBarSeries;
begin
  aSeries := TBarSeries.Create(Chart);
  with aSeries do
  try
    ParentChart := Chart;
    Title := Stitle;
    ShowInLegend := False;
    BarBrush.Color := GraphOptions.AreaFillColor;
    BarBrush.Style := GraphOptions.AreaFillStyle;
    BarWidthPercent := 100;
  finally
    Result := Assigned(aSeries);
  end;
end;


function CreateAreaSeries(Chart: TChart; const Stitle: String): TAreaSeries;
//-----------------------------------------------------------------------------
//  Creates a staired area data series in a Chart (for rainfall).
//-----------------------------------------------------------------------------
var
  aSeries: TAreaSeries;
  I: Integer;
begin
  aSeries := TAreaSeries.Create(Chart);
  with aSeries do
  try
    ParentChart := Chart;
    Title := Stitle;
    ShowInLegend := True;
    I := Chart.SeriesCount - 1;
    InitAreaSeriesOptions(aSeries, I);
    Stairs := True;
  finally
    Result := aSeries;
  end;
end;


function  CreateFastLineSeries(Chart: TChart; const Stitle: String): TFastLineSeries;
//-----------------------------------------------------------------------------
//  Creates a fast line data series in a Chart. (Used when plotting very
//  large time series data sets).
//-----------------------------------------------------------------------------
var
  aSeries: TFastLineSeries;
  I: Integer;
begin
  aSeries := TFastLineSeries.Create(Chart);
  with aSeries do
  try
    ParentChart := Chart;
    Title := Stitle;
    ShowInLegend := True;
    I := Chart.SeriesCount - 1;
    InitFastLineSeriesOptions(aSeries, I);
  finally
    Result := aSeries;
  end;
end;


function CreateLineSeries(Chart: TChart;  const Stitle: String): TLineSeries;
//-----------------------------------------------------------------------------
//  Creates a line data series in a Chart.
//-----------------------------------------------------------------------------
var
  aSeries: TLineSeries;
  I: Integer;
begin
  aSeries := TLineSeries.Create(Chart);
  with aSeries do
  try
    ParentChart := Chart;
    Title := Stitle;
    ShowInLegend := True;
    I := Chart.SeriesCount - 1;
    InitLineSeriesOptions(aSeries, I);
  finally
    Result := aSeries;
  end;
end;


function CreatePointSeries(Chart: TChart;  const Stitle: String): TPointSeries;
//-----------------------------------------------------------------------------
//  Creates a point data series in a Chart.
//-----------------------------------------------------------------------------
var
  aSeries : TPointSeries;
  N       : Integer;
begin
  aSeries := TPointSeries.Create(Chart);
  with aSeries do
  try
    ParentChart := Chart;
    Title := Stitle;
    ShowInLegend := False;
    Pointer.Visible := True;
    N := Chart.SeriesCount - 1;
    if N <= MAXSERIES then
    begin
      Pointer.Style := TSeriesPointerStyle(GraphOptions.PointStyle[N]);
      SeriesColor := GraphOptions.PointColor[N];
      Pointer.HorizSize := GraphOptions.PointSize[N];
      Pointer.VertSize := GraphOptions.PointSize[N];
    end;
  finally
    Result := aSeries;
  end;
end;


procedure InitGraphOptions(Chart: TChart);
//-----------------------------------------------------------------------------
//  Assigns the display options contained in the global variable GraphOptions
//  to a Chart object.
//-----------------------------------------------------------------------------
begin
  with Chart do
  begin
    View3D := GraphOptions.View3D;
    Chart3DPercent := GraphOptions.Percent3D;
    Color := GraphOptions.PanelColor;
    BackColor := GraphOptions.BackColor;
    Chart.BackWall.Gradient.EndColor := GraphOptions.BackGradColor;
    Legend.ColorWidth := GraphOptions.LegendWidth;
    Legend.LegendStyle :=lsSeries;
    Legend.Alignment := TLegendAlignment(GraphOptions.LegendPosition);
    Legend.Visible := GraphOptions.LegendVisible;
    Legend.Transparent := GraphOptions.LegendTransparent;
    Title.Text.Clear;
    Title.Font.Name := GraphOptions.TitleFontName;
    Title.Font.Size := GraphOptions.TitleFontSize;
    Title.Font.Style := [];
    if GraphOptions.TitleFontBold
    then Title.Font.Style := Title.Font.Style + [fsBold];
    if GraphOptions.TitleFontItalic
    then Title.Font.Style := Title.Font.Style + [fsItalic];
    InitAxisOptions(BottomAxis);
    InitAxisOptions(LeftAxis);
    InitAxisOptions(RightAxis);
    Legend.Font.Assign(BottomAxis.Title.Font);
  end;
end;


procedure Print(Chart: TChart; thePrinter: TPrintControl;
  Destination: TDestination);
//-----------------------------------------------------------------------------
// Prints the Chart to Destination (printer or preview form).
//-----------------------------------------------------------------------------
var
  W, H : Single;
  Left, Top : Single;
  Width, Height: Single;
  aRatio: Single;
  aMetafile: TMetafile;
  aPicture : TPicture;
begin
  if Chart.Height > 0
  then aRatio := Chart.Width/Chart.Height
  else aRatio := 1.0;
  with PageLayout, thePrinter do
  begin
    Left := LMargin;
    Width := GetPageWidth - LMargin - RMargin;
    Height := GetPageHeight - TMargin - BMargin;
  end;
  aPicture := TPicture.Create;
  aMetafile := Chart.TeeCreateMetafile(True, Chart.ClientRect);
  with thePrinter do
  try
    BeginJob;
    SetDestination(Destination);
    Uutils.FitFrameToPage(aRatio, Width, Height, W, H);
    Top := GetYPos;
    Left := Left + (Width - W)/2;
    aPicture.Metafile := aMetafile;
    StretchGraphic(Left, Top, Left+W, Top+H, aPicture);
    EndJob;
  finally
    aMetafile.Free;
    aPicture.Free;
    Chart.Refresh;
  end;
end;


procedure SetGraphOptions(theChart: TChart; var startPage: Integer; theForm: TForm);
//-----------------------------------------------------------------------------
//  Launches the Graph Options dialog which allows the user to change
//  theChart's display options.
//-----------------------------------------------------------------------------
var
  ChartOptionsDlg: TChartOptionsDlg;
begin
  ChartOptionsDlg := TChartOptionsDlg.Create(theForm);
  with ChartOptionsDlg do
  try
    PageControl1.ActivePageIndex := startPage;
    LoadOptions(theChart);
    if ShowModal = mrOK then
    begin
      startPage := PageControl1.ActivePageIndex;
      UnloadOptions(theChart);
      if DefaultBox.Checked then
        SaveDefaultOptions(theChart, UseDefaultPanelColor);
    end;
  finally
    ChartOptionsDlg.Free;
  end;
end;


//============================================================================


procedure CopyToBitmap(const Fname: String; Chart: TChart);
//-----------------------------------------------------------------------------
//  Copies Chart to file Fname (or to Clipboard) in bitmap format.
//-----------------------------------------------------------------------------
begin
  if Length(Fname) > 0
  then Chart.SaveToBitmapFile(Fname)
  else Chart.CopyToClipboardBitmap;
end;


procedure CopyToMetafile(const Fname: String; Chart: TChart);
//-----------------------------------------------------------------------------
// Copies Chart to file Fname (or to Clipboard) in enhanced metafile format.
//-----------------------------------------------------------------------------
begin
  if Length(Fname) > 0
  then Chart.SaveToMetafileEnh(Fname)
  else Chart.CopyToClipBoardMetaFile(True);
end;


function GetXstr(const X: Double; const IsDateTime: Boolean): String;
begin
  if IsDateTime then
    Result := DateTimeToStr(X)
  else
    Result := Format('%16.4f', [RoundTo(X, -4)]);
end;

procedure CopyToString(const Fname: String; Chart: TChart);
//-----------------------------------------------------------------------------
// Copies data behind Chart to file Fname or to the Clipboard.
//-----------------------------------------------------------------------------
var
  Slist: TStringList;
  I,J  : Integer;
  S,S1 : String;
  DateTimeAxis: Boolean;
begin
  // Create a stringlist to hold data points
  Slist := TStringList.Create;
  try
    with Chart do
    begin

    // Check for DateTime X-axis
    DateTimeAxis := BottomAxis.IsDateTime;

    // Add chart's title to the stringlist
    if Title.Text.Count > 0 then Slist.Add(Title.Text[0]);

    // Scatter plot or single Time Series plot
    if (SeriesCount = 1)
    or ( (SeriesCount = 2) and not Series[1].Active)
    then
    begin

      // Add column headings to the stringlist
      Slist.Add(Series[0].Title);
      S := Format('%-16s'+#9+'%-16s',
        [BottomAxis.Title.Caption, LeftAxis.Title.Caption]);
      Slist.Add(S);

      // Add each data pair to stringlist
      if Series[0].Active then with Series[0] do
      begin
        for J := 0 to Count-1 do
        begin
          S := GetXstr(XValue[J], DateTimeAxis);
          S := S + #9 + Format('%-16.4f', [YValue[J]]);
          Slist.Add(S);
        end;
      end;
    end

    // Calibration plot
    else if (SeriesCount = 2) and (Series[1] is TPointSeries) then
    begin
      Slist.Add(Series[0].Title);
      S := Format('%-16s'+#9+'%-16s'+#9+'%-16s',
        ['Series', BottomAxis.Title.Caption, LeftAxis.Title.Caption]);
      Slist.Add(S);
      for I := 0 to 1 do
      begin
        if not Series[I].Active then continue;
        if I = 0 then S1 := 'Computed' else S1 := 'Observed';
        for J := 0 to Series[I].Count-1 do
        begin
          S := S1 + #9 + GetXstr(Series[I].XValue[J], DateTimeAxis);
          S := S + #9 + Format('%-16.4f', [Series[I].YValue[J]]);
          Slist.Add(S);
        end;
      end;
    end

    // Multiple time series plot
    else begin
      // Add column headings to stringlist
      S := Format('%-16s',[BottomAxis.Title.Caption]);
      for I := 0 to SeriesCount-1 do
      begin
        if Series[I].Active then
        begin
          if Length(Series[I].Title) > 0
          then S1 := Series[I].Title
          else S1 := 'Series' + IntToStr(I+1);
          S := S + Format(#9+'%-16s', [S1]);
        end;
      end;
      Slist.Add(S);
      // Add each series' data point to stringlist
      for J := 0 to Series[0].Count-1 do
      begin
        // X (Time) value
        S := GetXstr(Series[0].XValue[J], DateTimeAxis);
        // Y-values
        for I := 0 to SeriesCount-1 do
        begin
          if Series[I].Active then
            S := S + Format(#9+'%-16.4f', [Series[I].YValue[J]]);
        end;
        Slist.Add(S);
      end;
    end;
  end;

  // Save the stringlist to file if a file name was supplied,
  // otherwise place text of the stringlist onto the Clipboard
  if Length(Fname) > 0
  then Slist.SaveToFile(Fname)
  else Clipboard.SetTextBuf(PChar(Slist.Text));

  // Free the stringlist.
  finally
    Slist.Free;
  end;
end;


procedure InitAxisOptions(Axis: TChartAxis);
//-----------------------------------------------------------------------------
// Initializes axis properties for the Axis component of a TChart object.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  with Axis do
  begin
    Automatic := True;
    AutomaticMinimum := True;
    AutomaticMaximum := True;
    Grid.Color := clSilver;  //GraphOptions.AxisGridColor;
    Grid.SmallDots := False;
    if Horizontal then I := 0        // BottomAxis
    else if Otherside then I := 2    // RightAxis
    else I := 1;                     // LeftAxis
    Grid.Visible := GraphOptions.AxisGridStyle[I] > 0;
    Title.Font.Name := GraphOptions.AxisFontName;
    Title.Font.Size := GraphOptions.AxisFontSize;
    Title.Font.Style := [];
    if GraphOptions.AxisFontBold
    then Title.Font.Style := Title.Font.Style + [fsBold];
    if GraphOptions.AxisFontItalic
    then Title.Font.Style := Title.Font.Style + [fsItalic];
    LabelsFont.Assign(Title.Font);
  end;
end;


procedure InitFastLineSeriesOptions(Series: TFastLineSeries; I: Integer);
//-----------------------------------------------------------------------------
//  Assigns the display options in the global variable GraphOptions to
//  a fast line data series component of a TChart object.
//-----------------------------------------------------------------------------
begin
  with Series do
  begin
    SeriesColor := GraphOptions.LineColor[I];
    LinePen.Visible := GraphOptions.LineVisible[I];                    
    LinePen.Style := TPenStyle(GraphOptions.LineStyle[I]);
    LinePen.Width := 1;
  end;
end;


procedure InitLineSeriesOptions(Series: TLineSeries; I: Integer);
//-----------------------------------------------------------------------------
//  Assigns the display options in the global variable GraphOptions to
//  a line data series component of a TChart object.
//-----------------------------------------------------------------------------
var
  Psize: Integer;
begin
  with Series do
  begin
    SeriesColor := GraphOptions.LineColor[I];
    LinePen.Color := SeriesColor;
    LinePen.Visible := GraphOptions.LineVisible[I];
    LinePen.Style := TPenStyle(GraphOptions.LineStyle[I]);
    LinePen.Width := GraphOptions.LineWidth[I];
    Pointer.Visible := GraphOptions.PointVisible[I];
    Pointer.Style := TSeriesPointerStyle(GraphOptions.PointStyle[I]);
    Pointer.Brush.Color := GraphOptions.PointColor[I];
    Psize := GraphOptions.PointSize[I];
    if Psize > 0 then
    begin
      Pointer.Size := Psize;
    end;
  end;
end;

procedure InitAreaSeriesOptions(Series: TAreaSeries; I: Integer);
//-----------------------------------------------------------------------------
//  Assigns the display options in the global variable GraphOptions to
//  an area data series component of a TChart object.
//-----------------------------------------------------------------------------
begin
  with Series do
  begin
    AreaColor := GraphOptions.AreaFillColor;
    SeriesColor := AreaColor;
    AreaBrush := GraphOptions.AreaFillStyle;
    LinePen.Visible := False;  //True;
    LinePen.Style := TPenStyle(0);
    LinePen.Width := GraphOptions.LineWidth[I];
    AreaLinesPen.Visible := GraphOptions.LineVisible[I];
    AreaLinesPen.Width := LinePen.Width;
    AreaLinesPen.Style := LinePen.Style;
    AreaLinesPen.Color := LinePen.Color;
    Pointer.Visible := False;
  end;
end;

procedure SaveDefaultOptions(Chart: TChart; const UseDefaultPanelColor: Boolean);
//-----------------------------------------------------------------------------
//  Saves Chart's display options as the default display options.
//-----------------------------------------------------------------------------
var
  I: Integer;
  K: Integer;
begin
  with GraphOptions do
  begin
    View3D := Chart.View3D;
    Percent3D := Chart.Chart3DPercent;
    if UseDefaultPanelColor then PanelColor := -1
    else PanelColor := Chart.Color;
    BackColor := Chart.BackColor;
    BackGradColor := Chart.Walls.Back.Gradient.EndColor;
    LegendPosition := Ord(Chart.Legend.Alignment);
    LegendVisible := Chart.Legend.Visible;
    LegendTransparent := Chart.Legend.Transparent;
    LegendColor := Chart.Legend.Color;
    LegendWidth := Chart.Legend.ColorWidth;
    LegendFramed := Chart.Legend.Frame.Visible;
    with Chart.Title.Font do
    begin
      TitleFontName := Name;
      TitleFontColor := Color;
      TitleFontSize := Size;
      TitleFontBold := (fsBold in Style);
      TitleFontItalic := (fsItalic in Style);
    end;
    with Chart.BottomAxis do
    begin
      AxisGridStyle[0] := Integer(Grid.Visible);
      with Title.Font do
      begin
        AxisFontName := Name;
        AxisFontSize := Size;
        AxisFontBold := (fsBold in Style);
        AxisFontItalic := (fsItalic in Style);
      end;
    end;
    DateTimeFormat := Chart.BottomAxis.DateTimeFormat;
    with Chart.LeftAxis do
    begin
      AxisGridStyle[1] := Integer(Grid.Visible);
    end;
    for I := 0 to Chart.SeriesCount-1 do
    begin
      if Chart.Series[I].Active then
      begin
        if Chart.Series[I] is TAreaSeries then
          with Chart.Series[I] as TAreaSeries do
        begin
          AreaFillColor := SeriesColor;
          AreaFillStyle := AreaBrush;
        end;
        if Chart.Series[I] is TLineSeries then
          with Chart.Series[I] as TLineSeries do
        begin
          K := Tag;
          LineColor[K] := SeriesColor;
          LineStyle[K] := Ord(LinePen.Style);
          LineWidth[K] := LinePen.Width;
          PointVisible[K] := Pointer.Visible;
          PointStyle[K] := Ord(Pointer.Style);
          PointColor[K] := Pointer.Brush.Color;
          PointSize[K] := Pointer.HorizSize;
          LineVisible[K] := LinePen.Visible;
        end;
        if Chart.Series[I] is TPointSeries then
          with Chart.Series[I] as TPointSeries do
        begin
          PointStyle[MAXSERIES] := Ord(Pointer.Style);
          PointColor[MAXSERIES] := SeriesColor;
          PointSize[MAXSERIES] := Pointer.HorizSize;
        end;
      end;
      if I = 0 then with Chart.Series[0] do
      begin
        LabelsBackColor := Marks.BackColor;
        LabelsTransparent := Marks.Transparent;
      end;
    end;
  end;
end;

end.

