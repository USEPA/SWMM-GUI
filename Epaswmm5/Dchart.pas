unit Dchart;

{-------------------------------------------------------------------}
{                    Unit:    Dchart.pas                            }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form used to set display options for a TChart component. }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, StdCtrls, Spin, ExtCtrls, ComCtrls, Chart, TeEngine,
  Series, Vcl.Themes, Vcl.Grids, UpDnEdit, System.UITypes, VCLTee.TeCanvas,
  Uutils;

const
  LineStyleText: array[0..4] of PChar =
    ('Solid','Dash','Dot','DashDot','DashDotDot');
  LegendPosText: array[0..3] of PChar =
    ('Left','Right','Top','Bottom');
  MarkStyleText: array[0..7] of PChar =
    ('Rectangle','Circle','Up Triangle','Down Triangle','Cross',
     'Diagonal Cross','Star','Diamond');
  FillStyleText: array[0..7] of PChar =
    ('Solid','Clear','Horizontal','Vertical','Foward Diagonal','Back Diagonal',
     'Cross','Diagonal Cross');
  StackStyleText: array[0..3] of PChar =
    ('None','Side','Stacked','Stacked 100%');
  LabelStyleText: array[0..8] of PChar =
    ('Value','Percent','Label','Label & %','Label & Value','Legend','% Total',
     'Label & % Total','X Value');
  DateFormats: array[0..4] of PChar =
    ('h:nn', 'h:nn m/d/yy', 'm/d yyyy', 'mmm yyyy', 'yyyy');

type
//Graph series types
  TSeriesType = (stLine, stFastLine, stPoint, stBar, stHorizBar, stArea, stPie);

//Axis information
  TAxisInfo = record
    Title:   String;
    DataMin: String;
    DataMax: String;
    AxisMin: String;
    AxisMax: String;
    AxisInc: String;
    DateFmt: Integer;
    GridLines: Boolean;
  end;

//Graph series options
  TSeriesOptions = class(TObject)
    Constructor Create;
    public
      SeriesType      : TSeriesType;
      LineVisible     : Boolean;
      LineStyle       : Integer;
      LineColor       : TColor;
      LineWidth       : Integer;
      PointVisible    : Boolean;
      PointStyle      : Integer;
      PointColor      : TColor;
      PointSize       : Integer;
      AreaFillStyle   : Integer;
      AreaFillColor   : TColor;
      AreaStacking    : Integer;
      PieCircled      : Boolean;
      PieUsePatterns  : Boolean;
      PieRotation     : Integer;
      LabelsVisible   : Boolean;
      LabelsTransparent: Boolean;
      LabelsArrows    : Boolean;
      LabelsBackColor : TColor;
      LabelsStyle     : Integer;
    end;

  TChartOptionsDlg = class(TForm)
    DefaultBox: TCheckBox;
    FontDialog1: TFontDialog;
    OkBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    PageControl1: TPageControl;
    GeneralPage: TTabSheet;
    XaxisPage: TTabSheet;
    LegendPage: TTabSheet;
    StylesPage: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label8: TLabel;
    PanelColorBox: TColorBox;
    BackColorBox1: TColorBox;
    View3DBox: TCheckBox;
    GraphTitleBox: TEdit;
    Pct3DUpDown: TUpDown;
    Pct3DEdit: TEdit;
    BackColorBox2: TColorBox;
    XminLabel: TLabel;
    XmaxLabel: TLabel;
    XIncrementLabel: TLabel;
    Label11: TLabel;
    XDataMinLabel: TLabel;
    XDataMaxLabel: TLabel;
    XFormatLabel: TLabel;
    Xmin: TEdit;
    Xmax: TEdit;
    Xinc: TEdit;
    Xtitle: TEdit;
    Xgrid: TCheckBox;
    DateFmtCombo: TComboBox;
    Label18: TLabel;
    Label19: TLabel;
    LegendFrameBox: TCheckBox;
    LegendVisibleBox: TCheckBox;
    LegendPosBox: TComboBox;
    LegendColorBox: TColorBox;
    LegendCheckBox: TCheckBox;
    LegendShadowBox: TCheckBox;
    Label21: TLabel;
    Label22: TLabel;
    SeriesComboBox: TComboBox;
    SeriesTitle: TEdit;
    Panel6: TPanel;
    PageControl2: TPageControl;
    LineOptionsSheet: TTabSheet;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    LineStyleBox: TComboBox;
    LineColorBox: TColorBox;
    LineVisibleBox: TCheckBox;
    LineSizeEdit: TEdit;
    LineSizeUpDown: TUpDown;
    MarkOptionsSheet: TTabSheet;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    MarkVisibleBox: TCheckBox;
    MarkStyleBox: TComboBox;
    MarkColorBox: TColorBox;
    MarkSizeEdit: TEdit;
    MarkSizeUpDown: TUpDown;
    AreaOptionsSheet: TTabSheet;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    AreaFillStyleBox: TComboBox;
    AreaColorBox: TColorBox;
    StackStyleBox: TComboBox;
    PieOptionsSheet: TTabSheet;
    Label32: TLabel;
    PieCircledBox: TCheckBox;
    PiePatternBox: TCheckBox;
    PieRotateEdit: TEdit;
    PieRotateUpDown: TUpDown;
    LabelsOptionsSheet: TTabSheet;
    Label33: TLabel;
    Label34: TLabel;
    LabelsStyleBox: TComboBox;
    LabelsBackColorBox: TColorBox;
    LabelsTransparentBox: TCheckBox;
    LabelsVisibleBox: TCheckBox;
    AxisBtn1: TRadioButton;
    AxisBtn2: TRadioButton;
    AxisBtn3: TRadioButton;
    Bevel1: TBevel;
    InvertedBox: TCheckBox;
    LegendTransparentBox: TCheckBox;
    Label3: TLabel;
    LegendWidthSpinner: TUpDnEditBox;
    GraphTitleFontLabel: TLinkLabel;
    AxisFontLabel: TLinkLabel;
    LegendFontLabel: TLinkLabel;
    Xauto: TLinkLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SeriesComboBoxClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure StylesPageExit(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure AxisBtn1Click(Sender: TObject);
    procedure GraphTitleFontLabelLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure AxisFontLabelLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure LegendFontLabelLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure XautoLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    AxisIndex: Integer;
    AxisInfo:  array[1..3] of TAxisInfo;
    SeriesIndex: Integer;
    theSeries: TStringlist;
    IsPieChart: Boolean;
    IsDateTime: Boolean;
    procedure LoadDateTimeOptions(theChart: TChart);
    procedure SaveSeriesOptions(const Index: Integer);
    procedure SetSeriesOptions(const Index: Integer);
    function  ValidAxisScaling(theAxis: TChartAxis;
              theAxisInfo: TAxisInfo): Boolean;
    procedure SetAxisOptions(theChart: TChart; theAxis: TChartAxis;
              var theAxisInfo: TAxisInfo);
    procedure SaveAxisInfo;
    procedure SetAxisInfo;
    procedure AutoScaleAxis(var theAxisInfo: TAxisInfo);
  public
    { Public declarations }
    UseDefaultPanelColor: Boolean;
    procedure LoadOptions(theChart: TChart);
    procedure UnloadOptions(theChart: TChart);
  end;

//var
//  ChartOptionsDlg: TChartOptionsDlg;

implementation

{$R *.dfm}

uses
  Uglobals;

{Constructor for TSeriesOptions}
Constructor TSeriesOptions.Create;
begin
  Inherited Create;
end;

procedure TChartOptionsDlg.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  //Load options into comboboxes
  for i := 0 to High(LineStyleText) do
    LineStyleBox.Items.Add(LineStyleText[i]);
  for i := 0 to High(LegendPosText) do
    LegendPosBox.Items.Add(LegendPosText[i]);
  for i := 0 to High(FillStyleText) do
    AreaFillStyleBox.Items.Add(FillStyleText[i]);
  for i := 0 to High(MarkStyleText) do
    MarkStyleBox.Items.Add(MarkStyleText[i]);
  for i := 0 to High(StackStyleText) do
    StackStyleBox.Items.Add(StackStyleText[i]);
  for i := 0 to High(LabelStyleText) do
    LabelsStyleBox.Items.Add(LabelStyleText[i]);
  LabelsStyleBox.ItemIndex := 0;
  //LabelsStyleBox.Enabled := False;
  PanelColorBox.DefaultColorColor :=
    Integer(StyleServices.GetStyleColor(scPanel));
  LegendColorBox.DefaultColorColor := PanelColorBox.DefaultColorColor;
  UseDefaultPanelColor := False;
  XFormatLabel.Top := XIncrementLabel.Top;
  DateFmtCombo.Top := Xinc.Top;

  // Make the left and right axis radio buttons invisible
  AxisBtn2.Visible := False;
  AxisBtn3.Visible := False;
  InvertedBox.Visible := False;
  AxisIndex := 0;

  //Create a stringlist to hold data series options
  theSeries := TStringlist.Create;
  PageControl1.ActivePage := GeneralPage;

end;

procedure TChartOptionsDlg.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  with theSeries do
  begin
    for i := 0 to Count - 1 do
      Objects[i].Free;
    Free;
  end;
end;

procedure TChartOptionsDlg.GraphTitleFontLabelLinkClick(Sender: TObject;
  const Link: string; LinkType: TSysLinkType);
begin
  with FontDialog1 do
  begin
    Font.Assign(GraphTitleBox.Font);
    if Execute then GraphTitleBox.Font.Assign(Font);
  end;
end;

procedure TChartOptionsDlg.AxisBtn1Click(Sender: TObject);
var
  NewAxisIndex : Integer;
begin
  if AxisBtn1.Checked then NewAxisIndex := 1
  else if AxisBtn2.Checked then NewAxisIndex := 2
  else NewAxisIndex := 3;
  if NewAxisIndex = AxisIndex then Exit;
  SaveAxisInfo;
  AxisIndex := NewAxisIndex;
  SetAxisInfo;
end;

procedure TChartOptionsDlg.XautoLinkClick(Sender: TObject; const Link: string;
  LinkType: TSysLinkType);
begin
  AutoScaleAxis(AxisInfo[AxisIndex]);
end;

procedure TChartOptionsDlg.AxisFontLabelLinkClick(Sender: TObject;
  const Link: string; LinkType: TSysLinkType);
begin
  with FontDialog1 do
  begin
    Font.Assign(Xtitle.Font);
    if Execute then
    begin
      Xtitle.Font.Assign(Font);
    end;
  end;
end;

procedure TChartOptionsDlg.LegendFontLabelLinkClick(Sender: TObject;
  const Link: string; LinkType: TSysLinkType);
begin
  with FontDialog1 do
  begin
    Font.Assign(SeriesTitle.Font);
    if Execute then SeriesTitle.Font.Assign(Font);
  end;
end;

procedure TChartOptionsDlg.SeriesComboBoxClick(Sender: TObject);
begin
  if (Sender is TComboBox) then
    with Sender as TComboBox do
    begin
      SaveSeriesOptions(SeriesIndex);  //Store options for current series
      SeriesIndex := ItemIndex;        //Update value of current series
      SetSeriesOptions(SeriesIndex);   //Load series options into form
    end;
end;

procedure TChartOptionsDlg.StylesPageExit(Sender: TObject);
begin
  SaveSeriesOptions(SeriesIndex);
end;

procedure TChartOptionsDlg.LoadOptions(theChart: TChart);
//------------------------------------------------------
//  Transfers data from theChart to form.
//-------------------------------------------------------
var
  i: Integer;
  s: String;
  SeriesOptions: TSeriesOptions;
begin
  IsPieChart := False;
  with theChart do
  begin

    //General Page
    View3DBox.Checked := View3D;
    Pct3DUpDown.Position := Chart3DPercent;
    with PanelColorBox do
    begin
      if theChart.Color = DefaultColorColor then
        ItemIndex := Items.IndexOf('Default')
      else Selected := theChart.Color;
    end;
    BackColorBox1.Selected := BackWall.Gradient.StartColor;
    BackColorBox2.Selected := BackWall.Gradient.EndColor;
    GraphTitleBox.Font.Assign(Title.Font);
    if (Title.Text.Count > 0) then
      GraphTitleBox.Text := Title.Text[0];

    //Series Page - do before Axis pages to get value for IsPieChart
    // Save current line series options
    IsDateTime := False;
    SeriesTitle.Font.Assign(Legend.Font);
    for i := 0 to SeriesCount-1 do
    begin
      if Series[i].Active then
      begin
        SeriesOptions := TSeriesOptions.Create;
        s := 'Series' + IntToStr(i+1);
        SeriesComboBox.Items.Add(s);
        if Series[i].XValues.DateTime then IsDateTime := True;
        if Series[i].VertAxis = aLeftAxis then AxisBtn2.Visible := True;
        if Series[i].VertAxis = aRightAxis then AxisBtn3.Visible := True;

        with Series[i], SeriesOptions do
        begin
          LabelsVisible := Marks.Visible;
          LabelsTransparent := Marks.Transparent;
          LabelsBackColor := Marks.BackColor;
          LabelsStyle := Ord(Marks.Style);
        end;
        if Series[i] is TLineSeries then
          with Series[i] as TLineSeries, SeriesOptions do
          begin
            SeriesType := stLine;
            LineVisible := LinePen.Visible;
            LineStyle := Ord(LinePen.Style);
            LineColor := SeriesColor;
            LineWidth := LinePen.Width;
            AreaFillStyle := Ord(AreaBrush);
            PointVisible := Pointer.Visible;
            PointStyle := Ord(Pointer.Style);
            PointColor := Pointer.Brush.Color; //SeriesColor;   ////ValueColor[0];
            PointSize := Pointer.VertSize;
          end
        else if Series[i] is TFastLineSeries then
          with Series[i] as TFastLineSeries, SeriesOptions do
          begin
            SeriesType := stFastLine;
            LineVisible := LinePen.Visible;
            LineStyle := Ord(LinePen.Style);
            LineColor := SeriesColor;
            LineWidth := LinePen.Width;
          end
        else if Series[i] is TPointSeries then
          with Series[i] as TPointSeries, SeriesOptions do
          begin
            SeriesType := stPoint;
            PointVisible := Pointer.Visible;
            PointStyle := Ord(Pointer.Style);
            PointColor := SeriesColor;
            PointSize := Pointer.HorizSize;
          end
        else if Series[i] is TBarSeries then
          with Series[i] as TBarSeries, SeriesOptions do
          begin
            SeriesType := stBar;
            AreaFillStyle := Ord(BarBrush.Style);
            if BarBrush.Style = bsSolid then
            begin
              AreaFillColor := SeriesColor;
            end
            else
            begin
              AreaFillColor := BarBrush.Color;
            end;
            LineColor := BarPen.Color;
            LineWidth := BarPen.Width;
            LineVisible := BarPen.Visible;
            AreaStacking := Ord(MultiBar);
          end
        else if Series[i] is THorizBarSeries then
          with Series[i] as THorizBarSeries, SeriesOptions do
          begin
            SeriesType := stHorizBar;
            AreaFillStyle := Ord(BarBrush.Style);
            if BarBrush.Style = bsSolid then
            begin
              AreaFillColor := SeriesColor;
            end
            else
            begin
              AreaFillColor := BarBrush.Color;
            end;
            LineColor := BarPen.Color;
            LineWidth := BarPen.Width;
            LineVisible := BarPen.Visible;
            AreaStacking := Ord(MultiBar);
          end
        else if Series[i] is TAreaSeries then
          with Series[i] as TAreaSeries, SeriesOptions do
          begin
            SeriesType := stArea;
            LineVisible := AreaLinesPen.Visible;
            LineStyle := Ord(AreaLinesPen.Style);
            LineColor := AreaLinesPen.Color;
            LineWidth := AreaLinesPen.Width;
            if Gradient.Visible then
              AreaFillColor := Gradient.EndColor
            else
              AreaFillColor := SeriesColor;
            AreaFillStyle := Ord(AreaBrush);
          end
        else if Series[i] is TPieSeries then
          with Series[i] as TPieSeries, SeriesOptions do
          begin
            SeriesType := stPie;
            IsPieChart := True;
            LineVisible := PiePen.Visible;
            LineStyle := Ord(PiePen.Style);
            LineColor := PiePen.Color;  //SeriesColor;
            LineWidth := PiePen.Width;
            PieCircled := Circled;
            PieUsePatterns := UsePatterns;
            PieRotation := RotationAngle;
          end;
        if Length(Series[i].Title) > 0 then s := Series[i].Title;
        theSeries.AddObject(s,SeriesOptions);
      end;
    end;

    // Bottom Axis
    if IsPieChart then XaxisPage.TabVisible := False
    else
    begin
      if IsDateTime = True then LoadDateTimeOptions(theChart) else
      begin
        AxisInfo[1].DataMin := Format('%f',[MinXValue(BottomAxis)]);
        AxisInfo[1].DataMax := Format('%f',[MaxXValue(BottomAxis)]);
        with BottomAxis do
        begin
          AxisInfo[1].AxisMin := Format('%f',[Minimum]);
          AxisInfo[1].AxisMax := Format('%f',[Maximum]);
          AxisInfo[1].AxisInc := Format('%f',[Increment]);
        end;
      end;
      with BottomAxis do
      begin
        AxisInfo[1].GridLines := Grid.Visible;
        Xtitle.Font.Assign(Title.Font);
        AxisInfo[1].Title := Title.Caption;
      end;
    end;

    // Left Axis
    if (not IsPieChart) and AxisBtn2.Visible then
    begin
      AxisInfo[2].DataMin := Format('%f',[MinYValue(LeftAxis)]);
      AxisInfo[2].DataMax := Format('%f',[MaxYValue(LeftAxis)]);
      with LeftAxis do
      begin
        AxisInfo[2].AxisMin := Format('%f',[Minimum]);
        AxisInfo[2].AxisMax := Format('%f',[Maximum]);
        AxisInfo[2].AxisInc := Format('%f',[Increment]);
        AxisInfo[2].GridLines := Grid.Visible;
        AxisInfo[2].Title := Title.Caption;
      end;
    end;

    // Right Axis
    if (not IsPieChart) and AxisBtn3.Visible then
    begin
      AxisInfo[3].DataMin := Format('%f',[MinYValue(RightAxis)]);
      AxisInfo[3].DataMax := Format('%f',[MaxYValue(RightAxis)]);
      with RightAxis do
      begin
        AxisInfo[3].AxisMin := Format('%f',[Minimum]);
        AxisInfo[3].AxisMax := Format('%f',[Maximum]);
        AxisInfo[3].AxisInc := Format('%f',[Increment]);
        AxisInfo[3].GridLines := Grid.Visible;
        AxisInfo[3].Title := Title.Caption;
        InvertedBox.Checked := Inverted;
      end;
    end;

    // Legend Page
    LegendPosBox.ItemIndex := Ord(Legend.Alignment);
    LegendColorBox.Selected := Legend.Color;
    LegendCheckBox.Checked := Legend.CheckBoxes;
    LegendShadowBox.Checked := Legend.Shadow.Visible;
    LegendFrameBox.Checked := Legend.Frame.Visible;
    LegendTransparentBox.Checked := Legend.Transparent;
    LegendVisibleBox.Checked := Legend.Visible;
    LegendWidthSpinner.Spinner.Position := Legend.ColorWidth;
  end;

  //Set current series to first series & update dialog entries
  if theChart.SeriesCount > 0 then
  begin
    SeriesIndex := 0;
    SeriesComboBox.ItemIndex := 0;
    SetSeriesOptions(0);
  end
  else StylesPage.TabVisible := False;
  AxisIndex := 1;
  SetAxisInfo;
end;

procedure TChartOptionsDlg.PageControl1Change(Sender: TObject);
begin
  if PageControl1.ActivePage = StylesPage
    then SetSeriesOptions(SeriesIndex);
end;

procedure TChartOptionsDlg.LoadDateTimeOptions(theChart: TChart);
//--------------------------------------------------------------
//   Sets up the X Axis page for using Date/Time formats.
//--------------------------------------------------------------
var
  i      : Integer;
  fType  : Integer;
  s      : String;
  minDate: TDateTime;
  maxDate: TDateTime;
begin
  AxisInfo[1].DataMin := '';
  AxisInfo[1].DataMax := '';
  //AxisInfo[1].AutoScale := True;
  minDate := theChart.MinXValue(theChart.BottomAxis);
  DateTimeToString(s, DateFormats[1], minDate);
  AxisInfo[1].AxisMin := s;
  maxDate := theChart.MaxXValue(theChart.BottomAxis);
  DateTimeToString(s, DateFormats[1], maxDate);
  AxisInfo[1].AxisMax := s;
  s := theChart.BottomAxis.DateTimeFormat;
  fType := 1;
  for i := 0 to High(DateFormats) do
    if SameText(s, DateFormats[i]) then fType := i;
  for i := 0 to High(DateFormats) do
  begin
    DateTimeToString(s, DateFormats[i], minDate);
    DateFmtCombo.Items.Add(s);
  end;
  DateFmtCombo.ItemIndex := fType;
end;

procedure TChartOptionsDlg.UnloadOptions(theChart: TChart);
//--------------------------------------------------------
//   Transfers data from form back to theChart.
//--------------------------------------------------------
var
  i,j: Integer;
  s  : String;
  SeriesOptions: TSeriesOptions;
begin
  with theChart do
  begin

    // General Page
    AutoRepaint := False;
    View3D := View3DBox.Checked;
    Chart3DPercent := Pct3DUpDown.Position;
    BackWall.Gradient.StartColor := BackColorBox1.Selected;
    BackWall.Gradient.EndColor := BackColorBox2.Selected;
    with PanelColorBox do
    begin
      if Items[ItemIndex] = 'Default' then
      begin
        theChart.Color := DefaultColorColor;
        UseDefaultPanelColor := True;
      end
      else theChart.Color := Selected;
    end;
    s := GraphTitleBox.Text;
    Title.Text.Clear;
    if (Length(s) > 0) then Title.Text.Add(s);
    Title.Font.Assign(GraphTitleBox.Font);

    // Axes
    // Transfer form entries to current axis info record
    SaveAxisInfo;

    // Save each axis info settings to chart's axis
    if not IsPieChart then
    begin
      SetAxisOptions(theChart, BottomAxis, AxisInfo[1]);
      if AxisBtn2.Visible then SetAxisOptions(theChart, LeftAxis, AxisInfo[2]);
      if AxisBtn3.Visible then
      begin
        SetAxisOptions(theChart, RightAxis, AxisInfo[3]);
        RightAxis.Inverted := InvertedBox.Checked;
      end;
    end;

    // Legend Page
    Legend.Alignment := TLegendAlignment(LegendPosBox.ItemIndex);
    with LegendColorBox do
    begin
      if Items[ItemIndex] = 'Default' then Legend.Color := DefaultColorColor
      else Legend.Color := LegendColorBox.Selected;
    end;
    Legend.CheckBoxes := LegendCheckBox.Checked;
    Legend.Shadow.Visible := LegendShadowBox.Checked;
    Legend.Frame.Visible := LegendFrameBox.Checked;
    Legend.Transparent := LegendTransparentBox.Checked;
    Legend.Visible := LegendVisibleBox.Checked;
    Legend.ColorWidth := LegendWidthSpinner.Spinner.Position;
    Legend.Font.Assign(SeriesTitle.Font);

    // Series Page
    if SeriesCount > 0 then
    begin
      SaveSeriesOptions(SeriesIndex);
      j := 0;
      for i := 0 to SeriesCount-1 do
      begin
        if Series[i].Active then
        begin
          SeriesOptions := TSeriesOptions(theSeries.Objects[j]);
          Series[i].Title := theSeries.Strings[j];

          with Series[i], SeriesOptions do
          begin
            Marks.Visible := LabelsVisible;
            Marks.Transparent := LabelsTransparent;
            Marks.BackColor := LabelsBackColor;
            Marks.Style := TSeriesMarksStyle(LabelsStyle);
          end;

          if Series[i] is TLineSeries then
          with Series[i] as TLineSeries, SeriesOptions do
          begin
            LinePen.Visible := LineVisible;
            LinePen.Style := TPenStyle(LineStyle);
            LinePen.Color := LineColor;
            SeriesColor := LineColor;
            LinePen.Width := LineWidth;
            Pointer.Visible := PointVisible;
            Pointer.Style := TSeriesPointerStyle(PointStyle);
            Pointer.Color := PointColor;
            Pointer.Size := PointSize;
            Pointer.Brush.Color := PointColor;
            ////LineBrush := TBrushStyle(AreaFillStyle);
            if (not Pointer.Visible) and (not LinePen.Visible) then
              ShowinLegend := False
            else
              ShowinLegend := True;
          end;

          if Series[i] is TFastLineSeries then
          with Series[i] as TFastLineSeries, SeriesOptions do
          begin
            LinePen.Visible := LineVisible;
            LinePen.Style := TPenStyle(LineStyle);
            SeriesColor := LineColor;
            LinePen.Width := LineWidth;
            ShowinLegend := LinePen.Visible;
          end;

          if Series[i] is TPointSeries then
          with Series[i] as TPointSeries, SeriesOptions do
          begin
            Pointer.Visible := PointVisible;
            Pointer.Style := TSeriesPointerStyle(PointStyle);
            SeriesColor := PointColor;
            Pointer.Brush.Color := PointColor;
            Pointer.Size := PointSize;
          end

          else if Series[i] is TBarSeries then
          with Series[i] as TBarSeries, SeriesOptions do
          begin
            BarBrush.Style := TBrushStyle(AreaFillStyle);
            if BarBrush.Style = bsSolid then
            begin
              SeriesColor := AreaFillColor;
              BarBrush.Color := AreaFillColor
            end
            else
            begin
              SeriesColor := LineColor;
              BarBrush.Color := AreaFillColor;
            end;
            BarPen.Color := LineColor;
            BarPen.Width := LineWidth;
            BarPen.Visible := LineVisible;
            MultiBar := TMultiBar(AreaStacking);
          end

          else if Series[i] is THorizBarSeries then
          with Series[i] as THorizBarSeries, SeriesOptions do
          begin
            BarBrush.Style := TBrushStyle(AreaFillStyle);
            if BarBrush.Style = bsSolid then
            begin
              SeriesColor := AreaFillColor;
              BarBrush.Color := AreaFillColor
            end
            else
            begin
              SeriesColor := LineColor;
              BarBrush.Color := AreaFillColor;
            end;
            BarPen.Color := LineColor;
            BarPen.Width := LineWidth;
            BarPen.Visible := LineVisible;
            MultiBar := TMultiBar(AreaStacking);
          end

          else if Series[i] is TAreaSeries then
          with Series[i] as TAreaSeries, SeriesOptions do
          begin
            SeriesColor := AreaFillColor;
            AreaColor := AreaFillColor;
            if Gradient.Visible then Gradient.EndColor := AreaColor;
            AreaBrush := TBrushStyle(AreaFillStyle);
            AreaLinesPen.Visible := LineVisible;
            AreaLinesPen.Style := TPenStyle(LineStyle);
            AreaLinesPen.Color := LineColor;
            AreaLinesPen.Width := LineWidth;
            LinePen.Color := LineColor;
            LinePen.Width := LineWidth;
            Pointer.Visible := False;
          end

          else if Series[i] is TPieSeries then
          with Series[i] as TPieSeries, SeriesOptions do
          begin
            PiePen.Visible := LineVisible;
            PiePen.Style := TPenStyle(LineStyle);
            PiePen.Color := LineColor;
            PiePen.Width := LineWidth;
            Circled := PieCircled;
            UsePatterns := PieUsePatterns;
            RotationAngle := PieRotation;
          end;

          Inc(j);
        end;
      end;
    end;
    AutoRepaint := True;
    Refresh;
  end;
end;

procedure TChartOptionsDlg.AutoScaleAxis(var theAxisInfo: TAxisInfo);
//--------------------------------------------------------
//   Implements auto-scaling for the selected axis.
//--------------------------------------------------------
var
  zMin, zMax, zInc: Double;
begin
  zMin := StrToFloatDef(theAxisInfo.DataMin, 0);
  zMax := StrToFloatDef(theAxisInfo.DataMax, 0);
  Uutils.AutoScale(zMin, zMax, zInc);
  theAxisInfo.AxisMin := Format('%f', [zMin]);
  theAxisInfo.AxisMax := Format('%f', [zMax]);
  theAxisInfo.AxisInc := Format('%f', [zInc]);
  Xmin.Text := theAxisInfo.AxisMin;
  Xmax.Text := theAxisInfo.AxisMax;
  Xinc.Text := theAxisInfo.AxisInc;
end;

procedure TChartOptionsDlg.SaveAxisInfo;
//------------------------------------------------------------------
//   Saves user's choice of options for the currently selected axis.
//------------------------------------------------------------------
begin
  if AxisIndex < 1 then Exit;
  AxisInfo[AxisIndex].AxisMin := XMin.Text;
  AxisInfo[AxisIndex].AxisMax := XMax.Text;
  AxisInfo[AxisIndex].AxisInc := XInc.Text;
  AxisInfo[AxisIndex].GridLines := Xgrid.Checked;
  AxisInfo[AxisIndex].Title := Xtitle.Text;
end;

procedure TChartOptionsDlg.SetAxisInfo;
//--------------------------------------------------------
//   Display user's options when a new axis is selected.
//--------------------------------------------------------
begin
  XDataMinLabel.Caption := '(' + AxisInfo[AxisIndex].DataMin + ')';
  XDataMaxLabel.Caption := '(' + AxisInfo[AxisIndex].DataMax + ')';
  XMin.Text := AxisInfo[AxisIndex].AxisMin;
  XMax.Text := AxisInfo[AxisIndex].AxisMax;
  XInc.Text := AxisInfo[AxisIndex].AxisInc;
  Xgrid.Checked := AxisInfo[AxisIndex].GridLines;
  Xtitle.Text := AxisInfo[AxisIndex].Title;
  InvertedBox.Visible := (AxisIndex = 3);

  if (AxisIndex = 1) and IsDateTime then
  begin
    Xmin.Enabled := False;
    Xmax.Enabled := False;
    Xinc.Visible := False;
    XIncrementLabel.Visible := False;
    XFormatLabel.Visible := True;
    DateFmtCombo.Visible := True;
    Xauto.Enabled := False;
  end
  else
  begin
    Xmin.Enabled := True;
    Xmax.Enabled := True;
    Xinc.Visible := True;
    XIncrementLabel.Visible := True;
    XFormatLabel.Visible := False;
    DateFmtCombo.Visible := False;
    Xauto.Enabled := True;
  end;
end;

procedure TChartOptionsDlg.SetAxisOptions(theChart: TChart; theAxis: TChartAxis;
  var theAxisInfo: TAxisInfo);
//------------------------------------------------------------
//   Assigns the user's choice of options to a particular axis.
//------------------------------------------------------------
begin
  with theAxis do
  begin

    if (theAxis = theChart.BottomAxis) and IsDateTime then
        DateTimeFormat := DateFormats[DateFmtCombo.ItemIndex]
    else
    begin
      Automatic := False;
      AutomaticMinimum := False;
      AutomaticMaximum := False;
      if not ValidAxisScaling(theAxis, theAxisInfo)
      then AutoScaleAxis(theAxisInfo);
      Minimum := StrToFloatDef(theAxisInfo.AxisMin, 0);
      Maximum := StrToFloatDef(theAxisInfo.AxisMax, 0);
      Increment := StrToFloatDef(theAxisInfo.AxisInc, 0);
    end;

    Grid.Visible := theAxisInfo.GridLines;
    Title.Caption := theAxisInfo.Title;
    Title.Font.Assign(Xtitle.Font);
    LabelsFont.Assign(Xtitle.Font);
  end;

end;

function TChartOptionsDlg.ValidAxisScaling(theAxis: TChartAxis;
            theAxisInfo: TAxisInfo): Boolean;
//-------------------------------------------------
//   Checks for valid axis scaling.
//-------------------------------------------------
var
  zMin, zMax, zInc: Double;
begin
  Result := False;
  try
    with theAxis do
    begin
      zMin := StrToFloat(theAxisInfo.AxisMin);
      zMax := StrToFloat(theAxisInfo.AxisMax);
      zInc := StrToFloatDef(theAxisInfo.AxisInc, 0);
      if zMin >= zMax then Exit;
      if zInc < 0 then Exit;
      if zInc > zMax - zMin then Exit;
    end;
    Result := True;
  except
  end;
end;

procedure TChartOptionsDlg.SetSeriesOptions(const Index: Integer);
//------------------------------------------------------
//   Transfer options for data series Index to form.
//------------------------------------------------------
var
  SeriesOptions: TSeriesOptions;
begin
  SeriesTitle.Text := theSeries.Strings[Index];
  SeriesOptions := TSeriesOptions(theSeries.Objects[Index]);
  with SeriesOptions do
  begin
    LineStyleBox.ItemIndex := LineStyle;
    LineColorBox.Selected := LineColor;
    LineSizeUpDown.Position := LineWidth;
    LineVisibleBox.Checked := LineVisible;
    MarkStyleBox.ItemIndex := PointStyle;
    MarkColorBox.Selected := PointColor;
    MarkSizeUpDown.Position := PointSize;
    MarkVisibleBox.Checked := PointVisible;
    AreaFillStyleBox.ItemIndex := AreaFillStyle;
    AreaColorBox.Selected := AreaFillColor;
    StackStyleBox.ItemIndex := AreaStacking;
    PieCircledBox.Checked := PieCircled;
    PiePatternBox.Checked := PieUsePatterns;
    PieRotateUpDown.Position := PieRotation;
    LabelsVisibleBox.Checked := LabelsVisible;
    LabelsTransparentBox.Checked := LabelsTransparent;
    LabelsBackColorBox.Selected := LabelsBackColor;
    LabelsStyleBox.ItemIndex := LabelsStyle;
  end;
  PieOptionsSheet.TabVisible := False;
  case SeriesOptions.SeriesType of
  stLine:
  begin
    LineOptionsSheet.TabVisible := True;
    LineVisibleBox.Visible := False;
    MarkOptionsSheet.TabVisible := True;
    AreaOptionsSheet.TabVisible := False;
    LabelsOptionsSheet.TabVisible := True;
    PageControl2.ActivePage := LineOptionsSheet;
  end;
  stFastLine:
  begin
    LineOptionsSheet.TabVisible := True;
    LineVisibleBox.Visible := False;
    MarkOptionsSheet.TabVisible := False;
    AreaOptionsSheet.TabVisible := False;
    LabelsOptionsSheet.TabVisible := False;
    PageControl2.ActivePage := LineOptionsSheet;
  end;
  stPoint:
  begin
    LineOptionsSheet.TabVisible := False;
    MarkOptionsSheet.TabVisible := True;
    AreaOptionsSheet.TabVisible := False;
    LabelsOptionsSheet.TabVisible := True;
    PageControl2.ActivePage := MarkOptionsSheet;
  end;
  stBar, stHorizBar:
  begin
    LineOptionsSheet.TabVisible := True;
    MarkOptionsSheet.TabVisible := False;
    AreaOptionsSheet.TabVisible := True;
    LabelsOptionsSheet.TabVisible := True;
    PageControl2.ActivePage := AreaOptionsSheet;
  end;
  stArea:
  begin
    LineOptionsSheet.TabVisible := True;
    MarkOptionsSheet.TabVisible := False;
    AreaOptionsSheet.TabVisible := True;
    LabelsOptionsSheet.TabVisible := True;
    AreaFillStyleBox.ItemIndex := 0;
    AreaFillStyleBox.Enabled := False;
    PageControl2.ActivePage := AreaOptionsSheet;
  end;
  stPie:
  begin
    LineOptionsSheet.TabVisible := True;
    MarkOptionsSheet.TabVisible := False;
    AreaOptionsSheet.TabVisible := False;
    PieOptionsSheet.TabVisible := True;
    LabelsOptionsSheet.TabVisible := True;
    PageControl2.ActivePage := PieOptionsSheet;
  end;
  end;
end;

procedure TChartOptionsDlg.SaveSeriesOptions(const Index: Integer);
//------------------------------------------------------
//   Transfer options from form to data series Index.
//------------------------------------------------------
var
  SeriesOptions: TSeriesOptions;
begin
  theSeries.Strings[Index] := SeriesTitle.Text;
  SeriesOptions := TSeriesOptions(theSeries.Objects[Index]);
  with SeriesOptions do
  begin
    if LineOptionsSheet.TabVisible then
    begin
      LineStyle := LineStyleBox.ItemIndex;
      LineColor := LineColorBox.Selected;
      LineWidth := LineSizeUpDown.Position;
      LineVisible := LineVisibleBox.Checked;
    end;
    if MarkOptionsSheet.TabVisible then
    begin
      PointStyle := MarkStyleBox.ItemIndex;
      PointColor := MarkColorBox.Selected;
      PointSize := MarkSizeUpDown.Position;
      PointVisible := MarkVisibleBox.Checked;
    end;
    if AreaOptionsSheet.TabVisible then
    begin
      AreaFillStyle := AreaFillStyleBox.ItemIndex;
      AreaFillColor := AreaColorBox.Selected;
      AreaStacking := StackStyleBox.ItemIndex;
    end;
    if PieOptionsSheet.TabVisible then
    begin
      PieCircled := PieCircledBox.Checked;
      PieUsePatterns := PiePatternBox.Checked;
      PieRotation := PieRotateUpDown.Position;
    end;
    if LabelsOptionsSheet.TabVisible then
    begin
      LabelsVisible := LabelsVisibleBox.Checked;
      LabelsTransparent := LabelsTransparentBox.Checked;
      LabelsBackColor := LabelsBackColorBox.Selected;
      LabelsStyle := LabelsStyleBox.ItemIndex;
    end;
  end;
end;

procedure TChartOptionsDlg.HelpBtnClick(Sender: TObject);
begin
    Application.HelpCommand(HELP_CONTEXT, 211750);
end;

procedure TChartOptionsDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then HelpBtnClick(Sender);
end;

end.
