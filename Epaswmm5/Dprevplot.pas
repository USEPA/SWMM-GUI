unit Dprevplot;

{-------------------------------------------------------------------}
{                    Unit:    Dprevplot.pas                         }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21   (5.2.0)                    }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form unit that displays a plot of the data contained     }
{   in the Curve editor, the Time Series editor, or the Transect    }
{   editor dialog forms in response to clicking the View button.    }
{                                                                   }
{   The form consists of a TChart Chart component with 4 data       }
{   series:                                                         }
{   Series1 - line series used to draw the top of a Transect from   }
{             one bank to the other                                 }
{   Series2 - line series used to draw the top of the main channel  }
{             portion of a Transect                                 }
{   Series3 - line series used to draw a Curve or Time Series       }
{   Series4 - area series used to draw the filled-in bottom and     }
{             bank areas of a Transect.                             }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TeEngine, Series, ExtCtrls, TeeProcs, Chart, TeeGDIPlus, Grids,
  StdCtrls, Vcl.Themes, Math, Xprinter, Uutils, Uglobals;

type
  TPreviewPlotForm = class(TForm)
    Chart1: TChart;
    Series1: TLineSeries;
    Series2: TLineSeries;
    Series3: TLineSeries;
    Series4: TAreaSeries;
    Panel1: TPanel;
    BtnPanel: TPanel;
    CopyBtn: TButton;
    PrintBtn: TButton;
    CloseBtn: TButton;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CopyBtnClick(Sender: TObject);
    procedure PrintBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    procedure DrawTransect(DataGrid: TStringGrid; const Xf: Single;
              const Yf: Single);
    procedure DrawTransectLine(aSeries: TLineSeries; DataGrid:TStringGrid;
              const I1: Integer; const I2: Integer; const Xf: Single;
              const Yf: Single);
    procedure SetBottomAxisScale;
    function  StripReturns(const S: String): String;
  public
    { Public declarations }
    procedure PlotCurveData(DataGrid: TStringGrid; const Title: String;
              const UseStairs: Boolean; const SwitchXY: Boolean);
    procedure PlotTransectData(DataGrid: TStringGrid; Xleft: String;
              Xright: String; Xfactor: String; Yfactor: String;
              const Title: String; const Units: String);
    procedure PlotTimeSeries(DataGrid: TStringGrid; const Title: String;
              D0: TDateTime);
    procedure PlotStorageXsect(DataGrid: TStringGrid; const Title: String);
    procedure PlotShapeXsect(DataGrid: TStringGrid; const Title: String);
  end;

var
  PreviewPlotForm: TPreviewPlotForm;

implementation

{$R *.dfm}

uses
  Fmain, Ugraph;

const
  TXT_CURVE_VIEWER = 'Curve Viewer';
  TXT_TIMESERIES_VIEWER = 'Time Series Viewer';
  TXT_TRANSECT_VIEWER = 'Transect Viewer';
  TXT_STORAGE_XSECT = 'Storage Curve Viewer';
  TXT_SHAPE_XSECT = 'Shape Curve Viewer';
  TXT_STATION = 'Station ';
  TXT_ELEVATION = 'Elevation ';
  TXT_DEPTH = 'Depth';
  TXT_TIME = 'Elapsed Time (hours)';
  TXT_RETURN = #13;

procedure TPreviewPlotForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnCreate handler.
//-----------------------------------------------------------------------------
begin
  Chart1.Canvas := TGDIPlusCanvas.Create;
  Chart1.Color := StyleServices.GetStyleColor(scPanel);
  PrintBtn.Enabled := MainForm.Mnuprint.Enabled;
end;


procedure TPreviewPlotForm.FormShow(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnShow handler. Right-aligns command button panel on form.
//-----------------------------------------------------------------------------
begin
  BtnPanel.Left := (ClientWidth - BtnPanel.Width);
end;


procedure TPreviewPlotForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
//-----------------------------------------------------------------------------
//  Form's OnKeyDown handler. Closes form when user hits Esc key.
//-----------------------------------------------------------------------------
begin
  if Key = 27 then Close;
end;


procedure TPreviewPlotForm.CopyBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the Copy To button.
//-----------------------------------------------------------------------------
begin
  Ugraph.CopyTo(Chart1);
end;


procedure TPreviewPlotForm.PrintBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the Print button.
//-----------------------------------------------------------------------------
begin
  Ugraph.Print(Chart1, MainForm.thePrinter, Xprinter.dPrinter);
end;


procedure TPreviewPlotForm.CloseBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the Close button.
//-----------------------------------------------------------------------------
begin
  Close;
end;


procedure TPreviewPlotForm.PlotTransectData(DataGrid: TStringGrid; Xleft: String;
  Xright: String; Xfactor: String; Yfactor: String; const Title: String;
  const Units: String);
//-----------------------------------------------------------------------------
//  This procedure displays a transect cross-section plot where:
//  DataGrid = a string grid containing the station-elevation data
//  Xleft = station representing the left overbank location
//  Xright = station representing the right overbank location
//  Xfactor = multiplier for station values
//  Yfactor = elevation offset
//-----------------------------------------------------------------------------
var
  I     : Integer;
  I1    : Integer;
  I2    : Integer;
  Ileft : Integer;
  Iright: Integer;
  Xf, Yf: Single;
begin
  // Set text for chart's captions
  Caption := TXT_TRANSECT_VIEWER;
  Chart1.Title.Text.Add(Title);
  Chart1.BottomAxis.Title.Caption := TXT_STATION + Units;
  Chart1.LeftAxis.Title.Caption := TXT_ELEVATION + Units;

  // Turn off the default chart setting of ascending ordering of X values
  Series1.XValues.Order := loNone;  // Full transect top line
  Series2.XValues.Order := loNone;  // Main channel top line
  Series4.XValues.Order := loNone;  // Full transect filled area

  // Locate left overbank station index if one has been specified
  Ileft := -1;
  Xleft := Trim(Xleft);
  if Length(Xleft) > 0 then for I := 1 to DataGrid.RowCount-1 do
  begin
    if not SameText(Xleft, DataGrid.Cells[1,I]) then continue;
    Ileft := I;
    break;
  end;

  // Locate right overbank station index if one has been specified
  Iright := -1;
  Xright := Trim(Xright);
  if Length(Xright) > 0 then for I := 1 to DataGrid.RowCount-1 do
  begin
    if not SameText(Xright, DataGrid.Cells[1,I]) then continue;
    Iright := I;
    break;
  end;

  // Determine values for transect modifiers if they were specified
  if not Uutils.GetSingle(Xfactor, Xf) then Xf := 1.0;
  if Xf = 0.0 then Xf := 1.0;
  if not Uutils.GetSingle(Yfactor, Yf) then Yf := 0.0;

  // Draw the full transect
  DrawTransect(DataGrid, Xf, Yf);
  DrawTransectLine(Series1, DataGrid, 1, DataGrid.RowCount-1, Xf, Yf);

  // Find data indexes that define the extent of the main channel
  I1 := 1;
  I2 := DataGrid.RowCount-1;
  if (Ileft >= 1) then I1 := Ileft;
  if (Iright >= 1) then I2 := Iright;

  // Draw the main channel portion of the transect
  DrawTransectLine(Series2, DataGrid, I1, I2, Xf, Yf);
end;


procedure TPreviewPlotForm.DrawTransect(DataGrid: TStringGrid;
  const Xf: Single; const Yf: Single);
//-----------------------------------------------------------------------------
//  Draws a natural cross-section transect whose station-elevation data
//  are contained in DataGrid, with station distance multiplier Xf and
//  elevation offset Yf.
//-----------------------------------------------------------------------------
var
  I  : Integer;
  Vx : Single;
  Vy : Single;
begin
  // Series4 is an Area Line series used to draw the full extent
  // of the transect
  with Series4 do
  begin
    with DataGrid do for I := 1 to RowCount-1 do
    begin
      if  (Uutils.GetSingle(Cells[1,I], Vx))
      and (Uutils.GetSingle(Cells[2,I], Vy))
      then AddXY(Xf*Vx, Yf+Vy, '', clTeeColor);
    end;
  end;
end;


procedure TPreviewPlotForm.DrawTransectLine(aSeries: TLineSeries;
  DataGrid:TStringGrid; const I1: Integer;  const I2: Integer;
  const Xf: Single; const Yf: Single);
//-----------------------------------------------------------------------------
//  Draws the top line of a section of a natural cross-section transect
//  using line series aSeries with the station-elevation data in DataGrid
//  extending between index I1 and I2, and with station distance
//  multiplier Xf and elevation offset Yf.
//-----------------------------------------------------------------------------
var
  I  : Integer;
  Vx : Single;
  Vy : Single;
begin
  // Depending on the line series specified, the line color signifies either
  // the main channel or an overbank area
  with aSeries do
  begin
    Active := False;
    with DataGrid do for I := I1 to I2 do
    begin
      if  (Uutils.GetSingle(Cells[1,I], Vx))
      and (Uutils.GetSingle(Cells[2,I], Vy))
      then AddXY(Xf*Vx, Yf+Vy, '', clTeeColor);
    end;
    Active := True;
  end;
end;


procedure TPreviewPlotForm.PlotCurveData(DataGrid: TStringGrid;
  const Title: String; const UseStairs: Boolean; const SwitchXY: Boolean);
//-----------------------------------------------------------------------------
// Plots the data for a Curve contained in DataGrid as a line series,
// either using a stair-step shape or not.
//-----------------------------------------------------------------------------
var
  I: Integer;
  Ix, Iy: Integer;
  Vx, Vy, Vx1: Single;
begin
  // Determine which columns of DataGrid are X and Y
  Ix := 1;
  Iy := 2;
  if SwitchXY then
  begin
    Ix := 2;
    Iy := 1;
  end;

  // Label the chart and its axes
  Series3.Stairs := UseStairs;
  Caption := TXT_CURVE_VIEWER;
  Chart1.Title.Text.Add(Title);
  Chart1.BottomAxis.Title.Caption := StripReturns(DataGrid.Cells[Ix,0]);
  Chart1.LeftAxis.Title.Caption := StripReturns(DataGrid.Cells[Iy,0]);
  Chart1.Legend.Visible := False;

  // Get data values for the chart
  with Series3 do
  begin
    // Extract X-Y values from grid control
    XValues.Order := loNone;
    Vx1 := 0;
    with DataGrid do for I := 1 to RowCount-1 do
    begin
      if (Uutils.GetSingle(Cells[Ix,I], Vx)) and
         (Uutils.GetSingle(Cells[Iy,I], Vy)) then
        begin
          if Stairs = True then AddXY(Vx1, Vy, '', clTeeColor);
          AddXY(Vx, Vy, '', clTeeColor);
          Vx1 := Vx;
        end;
    end;
  end;

  // Scale the X axis
  SetBottomAxisScale;
end;


procedure TPreviewPlotForm.PlotTimeSeries(DataGrid: TStringGrid;
  const Title: String; D0: TDateTime);
//-----------------------------------------------------------------------------
// Plots the time series data contained in DataGrid, using D0 as a
// starting data.
//-----------------------------------------------------------------------------
var
  I     : Integer;
  S     : String;
  V     : Extended;
  T     : TDateTime;
  D1    : TDateTime;
  DT    : Extended;
begin
  // Label the chart and its axes
  Caption := TXT_TIMESERIES_VIEWER;
  Chart1.Title.Text.Add(Title);
  Chart1.BottomAxis.Title.Caption := TXT_TIME;
  Chart1.Legend.Visible := False;
  Series3.XValues.DateTime := True;

  // Initialize charting date to D0
  D1 := D0;

  // Add data to Chart
  with Series3 do
  begin

    // Do not make the Chart automatically re-order the time values
    Clear;
    XValues.Order := loNone;

    // For each row in the DataGrid ...
    with DataGrid do for I := 1 to RowCount-1 do
    begin

      // Skip if no value entered for this row
      S := Trim(Cells[2,I]);
      if Length(S) = 0 then continue;
      if not Uutils.GetExtended(S, V) then continue;

      // Extract date from column 0 if present
      S := Trim(Cells[0,I]);
      if Length(S) > 0 then
      try
        D1 := StrToDate(S, MyFormatSettings);
        if I = 1 then D0 := D1;
      except
        on EConvertError do continue;
      end;
      try
        // Extract time from column 1
        // (can be in decimal hours or hours:minutes)
        S := Trim(Cells[1,I]);
        T := Uutils.StrHoursToTime(S);
        if T < 0 then continue;

        // Add date/time and value to chart series
        DT := (D1 + T - D0) * 24.0;
        AddXY(DT, V, '', clTeeColor);
      except
        on EConvertError do continue;
      end;
    end;

    // Adjust axis scales
    if Count > 0 then
    begin
      with Chart1.LeftAxis do
      begin
        if MinYvalue > 0 then Minimum := 0;
        AutomaticMinimum := False;
        if (MaxYvalue > 100) or (MinYvalue < 0) then
          AutomaticMinimum := True;
      end;
      SetBottomAxisScale;
    end;

  end;
end;


procedure TPreviewPlotForm.PlotStorageXsect(DataGrid: TStringGrid;
  const Title: String);
//-----------------------------------------------------------------------------
//  Draws the cross-section of a Storage Unit as derived from its
//  surface area v. depth curve whose data are in DataGrid.
//-----------------------------------------------------------------------------
var
  I: Integer;
  D, A, R: Single;
begin
  // Label the chart and its axes
  Series3.Stairs := False;
  Caption := TXT_STORAGE_XSECT;
  Chart1.Title.Text.Add(Title);
  Chart1.BottomAxis.Title.Caption := '';
  Chart1.BottomAxis.Labels := False;
  Chart1.BottomAxis.Grid.Style := psClear;
  Chart1.LeftAxis.Title.Caption := StripReturns(DataGrid.Cells[1,0]);
  Chart1.Legend.Visible := False;

  // Get data values for the chart
  with Series3 do
  begin
    // Draw left half of cross section
    XValues.Order := loNone;
    with DataGrid do for I := RowCount-1 downto 1 do
    begin
      if (Uutils.GetSingle(Cells[1,I], D)) and
         (Uutils.GetSingle(Cells[2,I], A)) then
        begin
          R := Sqrt(A/Pi);
          AddXY(-R, D, '', clTeeColor);
        end;
    end;
    if D <> 0.0 then AddXY(0, 0, '', clTeeColor);

    // Draw right half of cross section
    with DataGrid do for I := 1 to RowCount-1 do
    begin
      if (Uutils.GetSingle(Cells[1,I], D)) and
         (Uutils.GetSingle(Cells[2,I], A)) then
        begin
          R := Sqrt(A/Pi);
          AddXY(R, D, '', clTeeColor);
        end;
    end;
  end;
end;


procedure TPreviewPlotForm.PlotShapeXsect(DataGrid: TStringGrid;
  const Title: String);
//-----------------------------------------------------------------------------
//  Draws the cross-section of a Custom Cross Section Shape as derived from
//  its depth v. width curve whose data are in DataGrid.
//-----------------------------------------------------------------------------
var
  I: Integer;
  Y, X: Single;
  Ylast, Xlast: Single;
  Aratio, Xrange, Dy: Double;
begin
  // Setup the chart and its axes
  Series3.Stairs := False;
  Caption := TXT_SHAPE_XSECT;
  Chart1.Title.Text.Add(Title);
  with Chart1 do
  begin
    BottomAxis.Title.Caption := '';
    BottomAxis.Labels := False;
    BottomAxis.Grid.Style := psClear;
    LeftAxis.Title.Caption := '';
    LeftAxis.Labels := False;
    LeftAxis.Grid.Style := psClear;
    Legend.Visible := False;
  end;

  // Get data values for the chart
  Ylast := 0;
  Xlast := 0;
  with Series3 do
  begin
    // Draw left half of cross section
    XValues.Order := loNone;
    with DataGrid do for I := RowCount-1 downto 1 do
    begin
      if  (Uutils.GetSingle(Cells[1,I], Y)) and
          (Uutils.GetSingle(Cells[2,I], X)) then
          AddXY(-X/2, Y, '', clTeeColor);
    end;

    // Add a 0,0 point if curve doesn't start at 0
    if Y > 0.0 then AddXY(0, 0, '', clTeeColor);

    // Draw right half of cross section
    with DataGrid do
    begin
      for I := 1 to RowCount-1 do
      begin
        if (Uutils.GetSingle(Cells[1,I], Y)) and
           (Uutils.GetSingle(Cells[2,I], X)) then
        begin
          AddXY(X/2, Y, '', clTeeColor);
          Ylast := Y;
          Xlast := -X/2;
        end;
      end;
    end;

    // Add a 1,0 point if curve doesn't end at 1
    if Ylast < 1.0 then
    begin
      AddXY(0, 1, '', clTeeColor);
      AddXY(Xlast, Ylast, '', clTeeColor);
    end

    // Or close up top if left open
    else if Ylast <> Xlast then AddXY(Xlast, Ylast, '', clTeeColor);
    Xrange := MaxXValue - MinXValue;
  end;

  // Set axis ranges to maintain 1:1 aspect ratio
  with Chart1 do
  begin
    Aratio := Width/(0.8*Height);
    if Xrange <= Aratio then
    begin
      BottomAxis.Automatic := False;
      BottomAxis.SetMinMax(-Aratio/2, Aratio/2);
      LeftAxis.Automatic := False;
      LeftAxis.SetMinMax(-0.025, 1.025);
    end
    else
    begin
      Dy := (Xrange/Aratio - 1.0)/2.0;
      if Dy < 0.025 then Dy := 0.025;
      LeftAxis.Automatic := False;
      LeftAxis.SetMinMax(-Dy, 1.0+Dy);
    end;
  end;
end;


function TPreviewPlotForm.StripReturns(const S: String): String;
begin
  Result := S;
  while Pos(TXT_RETURN, Result) > 0 do
    Result[Pos(TXT_RETURN, Result)] := ' ';
end;


procedure TPreviewPlotForm.SetBottomAxisScale;
var
  Xmin: Double;
  Xmax: Double;
  Xinc: Double;
begin
  with Chart1.BottomAxis do
  begin
    CalcMinMax(Xmin, Xmax);
    if Xmax < 10 then
    begin
      Uutils.AutoScale(Xmin, Xmax, Xinc);
      Maximum := Xmax;
      Minimum := Xmin;
      Increment := Xinc;
      Automatic := False;
    end;
  end;
end;

end.
