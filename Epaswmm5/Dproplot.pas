unit Dproplot;

{-------------------------------------------------------------------}
{                    Unit:    Dproplot.pas                          }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form unit for editing the display options of a           }
{   profile plot.                                                   }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Spin, StdCtrls, ExtCtrls, ComCtrls, Vcl.Themes,  Chart, TeEngine,
  UpDnEdit, System.UITypes, NumEdit, Uglobals;

type
  TProfilePlotOptionsForm = class(TForm)
    DefaultBox: TCheckBox;
    OKBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    FontDialog: TFontDialog;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ColorBox1: TColorBox;
    ColorBox2: TColorBox;
    ColorBox3: TColorBox;
    ColorBox4: TColorBox;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    MainTitleEdit: TEdit;
    HorizAxisEdit: TEdit;
    VertAxisEdit: TEdit;
    HorizGridBox: TCheckBox;
    VertGridBox: TCheckBox;
    Label5: TLabel;
    Label6: TLabel;
    LabelsOnAxisBox: TCheckBox;
    LabelsOnPlotBox: TCheckBox;
    LabelSizeSpin: TUpDnEditBox;
    ArrowLengthSpin: TUpDnEditBox;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    YminLabel: TLabel;
    YmaxLabel: TLabel;
    YminEdit: TNumEdit;
    YmaxEdit: TNumEdit;
    YincEdit: TNumEdit;
    MainTitleFontLabel: TLinkLabel;
    AxisFontLabel: TLinkLabel;
    AutoScaleCheckBox: TCheckBox;
    TabSheet5: TTabSheet;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure MainTitleFontLabelClick(Sender: TObject);
    procedure AxisFontLabelClick(Sender: TObject);
    procedure AutoScaleCheckBoxClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
    LeftAxis: TChartAxis;
    procedure LoadOptions(theGraphOptions: TGraphOptions;
      theProfileOptions: TProfileOptions; theMainTitle: String;
      theXtitle: String; theYtitle: String; IncludeHGL: Boolean;
      AutoScale: Boolean; ShowGround: Boolean);
    procedure UnloadOptions(var theGraphOptions: TGraphOptions;
      var theProfileOptions: TProfileOptions; var theMainTitle: String;
      var theXtitle: String; var theYtitle: String; var IncludeHGL: Boolean;
      var AutoScale: Boolean; var ShowGround: Boolean);
    procedure SetLeftAxisScaling(Ymax: Double; theChart: TChart);
    procedure GetLeftAxisScaling(theChart: TChart);
  end;

//var
//  ProfilePlotOptionsForm: TProfilePlotOptionsForm;

implementation

{$R *.dfm}

procedure TProfilePlotOptionsForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnCreate handler.
//-----------------------------------------------------------------------------
//var
//  I: Integer;
begin
  ColorBox1.DefaultColorColor :=
    Integer(StyleServices.GetStyleColor(scPanel));
end;

procedure TProfilePlotOptionsForm.LoadOptions(theGraphOptions: TGraphOptions;
  theProfileOptions: TProfileOptions; theMainTitle: String; theXtitle: String;
  theYtitle: String; IncludeHGL: Boolean; AutoScale: Boolean;
  ShowGround: Boolean);
//-----------------------------------------------------------------------------
//  Loads a set of graph options, profile plot options, main title, and
//  axis titles into the form.
//-----------------------------------------------------------------------------
begin
  with MainTitleEdit do
  begin
    Text := theMainTitle;
    Font.Color := theGraphOptions.TitleFontColor;
    Font.Name := theGraphOptions.TitleFontName;
    Font.Size := theGraphOptions.TitleFontSize;
    Font.Style := [];
    if theGraphOptions.TitleFontBold
    then Font.Style := Font.Style + [fsBold];
    if theGraphOptions.TitleFontItalic
    then Font.Style := Font.Style + [fsItalic];
  end;

  with HorizAxisEdit do
  begin
    Text := theXtitle;
    Font.Color := clBlack;
    Font.Name := theGraphOptions.AxisFontName;
    Font.Size := theGraphOptions.AxisFontSize;
    Font.Style := [];
    if theGraphOptions.AxisFontBold
    then Font.Style := Font.Style + [fsBold];
    if theGraphOptions.AxisFontItalic
    then Font.Style := Font.Style + [fsItalic];
  end;
  VertAxisEdit.Text := theYtitle;
  VertAxisEdit.Font.Assign(HorizAxisEdit.Font);
  HorizGridBox.Checked := (theGraphOptions.AxisGridStyle[1] > 0);
  VertGridBox.Checked  := (theGraphOptions.AxisGridStyle[0] > 0);

  with theProfileOptions do
  begin
    LabelsOnAxisBox.Checked :=  LabelsOnAxis;
    LabelsOnPlotBox.Checked := LabelsOnPlot;
    ArrowLengthSpin.Spinner.Position := LabelsArrowLength;
    LabelSizeSpin.Spinner.Position := LabelsFontSize;
  end;

  if theGraphOptions.PanelColor = -1
  then ColorBox1.ItemIndex := ColorBox1.Items.IndexOf('Default')
  else ColorBox1.Selected := theGraphOptions.PanelColor;

  ColorBox2.Selected := theGraphOptions.BackColor;
  ColorBox3.Selected := theProfileOptions.ConduitColor;
  ColorBox4.Selected := theProfileOptions.WaterColor;

  CheckBox1.Checked := not IncludeHGL;
  AutoScaleCheckBox.Checked := AutoScale;
  CheckBox2.Checked := (theProfileOptions.LineWidth > 1);
  CheckBox3.Checked := ShowGround;
end;

procedure TProfilePlotOptionsForm.SetLeftAxisScaling(Ymax: Double; theChart: TChart);
//-----------------------------------------------------------------------------
//  Sets the entries in the edit boxes used for vertical axis scaling
//-----------------------------------------------------------------------------
begin
  YminEdit.Text := FloatToStr(theChart.LeftAxis.Minimum);
  YmaxEdit.Text := FloatToStr(theChart.LeftAxis.Maximum);
  YincEdit.Text := FloatToStr(theChart.LeftAxis.Increment);
  YminLabel.Caption := '(' + FloatToStrF(theChart.MinYValue(theChart.LeftAxis),
                             ffFixed, 15, 2) + ')';
  YmaxLabel.Caption := '(' + FloatToStrF(Ymax, ffFixed, 15, 2) + ')';
end;

procedure TProfilePlotOptionsForm.UnloadOptions(
  var theGraphOptions: TGraphOptions; var theProfileOptions: TProfileOptions;
  var theMainTitle: String; var theXtitle: String; var theYtitle: String;
  var IncludeHGL: Boolean; var AutoScale: Boolean; var ShowGround: Boolean);
//-----------------------------------------------------------------------------
//  Unloads graph options, profile plot options, main title, and axis
//  titles from the form.
//-----------------------------------------------------------------------------
begin
  theMainTitle := MainTitleEdit.Text;
  theXtitle := HorizAxisEdit.Text;
  theYtitle := VertAxisEdit.Text;
  IncludeHGL := not Checkbox1.Checked;
  AutoScale := AutoScaleCheckBox.Checked;
  ShowGround := CheckBox3.Checked;

  with theProfileOptions do
  begin
    LabelsOnAxis := LabelsOnAxisBox.Checked;
    LabelsOnPlot := LabelsOnPlotBox.Checked;
    LabelsArrowLength := ArrowLengthSpin.Spinner.Position;
    LabelsFontSize := LabelSizeSpin.Spinner.Position;
    ConduitColor := ColorBox3.Selected;
    WaterColor := ColorBox4.Selected;
    if CheckBox2.Checked then LineWidth := 2 else LineWidth := 1;
  end;

  with theGraphOptions do
  begin
    //if ColorBox1.Selected = ColorBox1.DefaultColorColor
    if ColorBox1.ItemIndex = ColorBox1.Items.IndexOf('Default')
    then PanelColor := -1
    else PanelColor := ColorBox1.Selected;
    BackColor  := ColorBox2.Selected;
    AxisGridStyle[1] := Ord(HorizGridBox.Checked);
    AxisGridStyle[0] := Ord(VertGridBox.Checked);
    with MainTitleEdit.Font do
    begin
      TitleFontName := Name;
      TitleFontColor := Color;
      TitleFontSize := Size;
      TitleFontBold := (fsBold in Style);
      TitleFontItalic := (fsItalic in Style);
    end;
    with HorizAxisEdit.Font do
    begin
      AxisFontName := Name;
      AxisFontSize := Size;
      AxisFontBold := (fsBold in Style);
      AxisFontItalic := (fsItalic in Style);
    end;
  end;
end;

procedure TProfilePlotOptionsForm.GetLeftAxisScaling(theChart: TChart);
//-----------------------------------------------------------------------------
//  Retrieves vertical axis scaling options
//-----------------------------------------------------------------------------
var
  YminExt, YmaxExt, YincExt: Extended;
begin
  with theChart.LeftAxis do
  begin
    YminExt := StrToFloatDef(YminEdit.Text, Minimum);
    YmaxExt := StrToFloatDef(YmaxEdit.Text, Maximum);
    YincExt := StrToFloatDef(YincEdit.Text, 0);
    if YmaxExt > YminExt then
    begin
      Automatic := False;
      AutomaticMinimum := False;
      AutomaticMaximum := False;
      Maximum := YmaxExt;
      Minimum :=  YminExt;
      Increment := YincExt;
    end;
  end;
end;

procedure TProfilePlotOptionsForm.MainTitleFontLabelClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the LinkLabel used to change the font properties
//  of the profile plot's main title.
//-----------------------------------------------------------------------------
begin
  with FontDialog do
  begin
    Font.Assign(MainTitleEdit.Font);
    Options := Options + [fdEffects];
    if Execute then MainTitleEdit.Font.Assign(Font);
  end;
end;

procedure TProfilePlotOptionsForm.AutoScaleCheckBoxClick(Sender: TObject);
begin
  YminEdit.Enabled := not AutoScaleCheckBox.Checked;
  YmaxEdit.Enabled := not AutoScaleCheckBox.Checked;
  YincEdit.Enabled := not AutoScaleCheckBox.Checked;
end;

procedure TProfilePlotOptionsForm.AxisFontLabelClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the LinkLabel used to change the font properties
//  of the axes displayed on the profile plot.
//-----------------------------------------------------------------------------
begin
  with FontDialog do
  begin
    Font.Assign(HorizAxisEdit.Font);
    Options := Options - [fdEffects];
    if Execute then
    begin
      HorizAxisEdit.Font.Assign(Font);
      VertAxisEdit.Font.Assign(Font);
    end;
  end;
end;

procedure TProfilePlotOptionsForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 211380);
end;

procedure TProfilePlotOptionsForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then HelpBtnClick(Sender);
end;

end.
