unit Dxsect;

{-------------------------------------------------------------------}
{                    Unit:    Dxsect.pas                            }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21   (5.2.0)                    }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form unit used to edit cross-sectional geometry for      }
{   a conduit.                                                      }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, NumEdit, ImgList, Spin, Buttons, ComCtrls,
  Uproject, Uglobals, Uutils, System.ImageList, Vcl.VirtualImageList,
  Vcl.BaseImageCollection, Vcl.ImageCollection;

type
  TXsectShape = record
    Text : array[1..4] of String;
  end;

const

  // Index numbers of shapes requiring special treatment
  MAX_SHAPE_INDEX = 25;
  CUSTOM_SHAPE_INDEX = 24;
  STREET_SHAPE_INDEX = 5;
  IRREG_SHAPE_INDEX = 6;
  HORIZ_ELLIPSE_SHAPE_INDEX = 11;
  VERT_ELLIPSE_SHAPE_INDEX = 12;
  ARCH_SHAPE_INDEX = 13;
  TRAPEZOIDAL_SHAPE_INDEX = 1;
  CIRC_SHAPE_INDEX = 7;
  FORCE_MAIN_INDEX = 8;
  FILLED_CIRC_INDEX = 9;
  MODBASKET_SHAPE_INDEX = 16;
  RECTANGULAR_SHAPE_INDEX = 0;

  // Full names of available cross-sectional shapes
  ShapeNames: array[0..MAX_SHAPE_INDEX] of String =
  ('Rectangular', 'Trapezoidal', 'Triangular', 'Parabolic', 'Power',
   'Street', 'Irregular', 'Circular', 'Force Main', 'Filled Circular',
   'Closed Rectangular', 'Horizontal Elliptical', 'Vertical Elliptical',
   'Arch', 'Rectangular Triangular', 'Rectangular Round', 'Modified Baskethandle',
   'Egg', 'Horseshoe', 'Gothic', 'Catenary', 'Semi-Elliptical', 'Baskethandle',
   'Semi-Circular', 'Custom', 'Dummy');

  // Short names and data field labels of available shapes
  XsectShapes: array[0..MAX_SHAPE_INDEX] of TXsectShape =
    (
     (Text: ('RECT_OPEN', 'Bottom Width', '', '')),
     (Text: ('TRAPEZOIDAL', 'Bottom Width', 'Left Slope', 'Right Slope')),
     (Text: ('TRIANGULAR', 'Top Width', '', '')),
     (Text: ('PARABOLIC', 'Top Width', '', '')),
     (Text: ('POWER', 'Top Width', 'Power', '')),
     (Text: ('STREET', '', '', '')),
     (Text: ('IRREGULAR', '', '', '')),
     (Text: ('CIRCULAR', '', '', '')),
     (Text: ('FORCE_MAIN', 'Roughness*', '', '')),
     (Text: ('FILLED_CIRCULAR', 'Filled Depth', '', '')),
     (Text: ('RECT_CLOSED', 'Bottom Width', '', '')),
     (Text: ('HORIZ_ELLIPSE', 'Maximum Width', 'Size Code', 'Standard Sizes')),
     (Text: ('VERT_ELLIPSE', 'Maximum Width', 'Size Code', 'Standard Sizes')),
     (Text: ('ARCH', 'Maximum Width', 'Size Code', 'Standard Sizes')),
     (Text: ('RECT_TRIANGULAR', 'Top Width', 'Triangle Height', '')),
     (Text: ('RECT_ROUND', 'Top Width', 'Bottom Radius', '')),
     (Text: ('MODBASKETHANDLE', 'Bottom Width', 'Top Radius', '')),
     (Text: ('EGG', '', '', '')),
     (Text: ('HORSESHOE', '', '', '')),
     (Text: ('GOTHIC', '', '', '')),
     (Text: ('CATENARY', '', '', '')),
     (Text: ('SEMIELLIPTICAL', '', '', '')),
     (Text: ('BASKETHANDLE', '', '', '')),
     (Text: ('SEMICIRCULAR', '', '', '')),
     (Text: ('CUSTOM', '', '', '')),
     (Text: ('DUMMY', '', '', ''))
     );

  // Short description of each available shape
  XsectNotes: array[0..MAX_SHAPE_INDEX] of String =
  (
   'Open rectangular channel. Sidewalls can be removed for 2-D modeling.',
   'Open trapezoidal channel. Slopes are horizontal / vertical.',
   'Open triangular channel.',
   'Open parabolic channel where depth varies with top width squared.',
   'Open channel where depth varies with top width to some power.',
   'Open street cross section, either one or two sided',
   'Open irregular natural channel described by transect coordinates.',
   'Standard circular pipe.',
   'Circular pipe with a special friction loss equation for pressurized flow.',
   'Circular pipe partly filled with sediment.',
   'Closed rectangular box conduit.',
   'Closed horizontal elliptical pipe. '+
   'Select a standard size or "Custom" to enter a custom height and width.',
   'Closed vertical elliptical pipe. '+
   'Select a standard size or "Custom" to enter a custom height and width.',
   'Closed arch pipe. ' +
   'Select a standard size or "Custom" to enter a custom height and width.',
   'Closed rectangular top with triangular bottom.',
   'Closed rectangular top with circular bottom.',
   'Rectangular bottom with closed circular top.',
   'Classic Phillips Standard Egg sewer shape.',
   'Classic Boston Horseshoe sewer shape.',
   'Classic Gothic sewer shape.',
   'Classic Catenary sewer shape (i.e., inverted egg).',
   'Classic Louisville Semi-Elliptical sewer shape.',
   'Classic Baskethandle sewer shape.',
   'Classic Semi-Circular sewer shape.',
   'Closed custom shape described by a user-supplied shape curve.',
   'There are no parameters for a dummy cross-section.');

type
  TXsectionForm = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    Label1: TLabel;
    NumEdit1: TNumEdit;
    Label2: TLabel;
    NumEdit2: TNumEdit;
    Label3: TLabel;
    NumEdit3: TNumEdit;
    Label4: TLabel;
    NumEdit4: TNumEdit;
    TsectCombo: TComboBox;
    BarrelsLabel: TLabel;
    TsectBtn: TBitBtn;
    BarrelsEdit: TEdit;
    BarrelsUpDown: TUpDown;
    TsectLabel: TLabel;
    ForceMainNote: TLabel;
    ShapeListView: TListView;
    ImageList1: TImageList;
    SidewallsLabel: TLabel;
    SidewallsCombo: TComboBox;
    UnitsLabel: TLabel;
    SizesCombo: TComboBox;
    SpecialNote: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure NumEdit1Change(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure TsectBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SizesComboClick(Sender: TObject);
    procedure SizesComboDropDown(Sender: TObject);
    procedure TsectComboKeyPress(Sender: TObject; var Key: Char);
    procedure ShapeListViewSelectItem(Sender: TObject; Item: TListItem;
              Selected: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    SelectedShapeIndex: Integer;
    PriorShapeIndex: Integer;
    OldShapeIndex: Integer;
    PriorStreetShapeName: String;
    PriorTransectName: String;
    procedure DisplayStdArchSize(S: String);
    function  ValidateData: Boolean;
    function  ValidateTextEntry(const N: Integer; const I: Integer): Boolean;
    procedure SetupDataFields;
  public
    { Public declarations }
    HasChanged: Boolean;
    procedure SetData(const Shape: String; const Geom1: String;
              const Geom2: String; const Geom3: String; const Geom4: String;
              const Barrels: String; const Tsect: String);
    procedure GetData(var Shape: String; var Geom1: String;
              var Geom2: String; var Geom3: String; var Geom4: String;
              var Barrels: String; var Tsect: String);
    function  GetArchSize(Index: Integer): String;
  end;

//var
//  XsectionForm: TXsectionForm;

implementation

{$R *.DFM}
{$R Xsects.res}

uses
  Fmain, Darchpipe, Uedit;

const
  TXT_STREET_NOTE = 'Street Section Name';
  TXT_TRANSECT_NOTE = 'Transect Name';
  TXT_CUSTOM_NOTE = 'Shape Curve Name';
  TXT_SIZES_NOTE = 'Standard Sizes';
  TXT_DIMENSIONS_IN = 'Dimensions are ';
  TXT_FEET = 'feet';
  TXT_METERS = 'meters';
  TXT_OTHERWISE = ' unless otherwise stated.';
  TXT_MAX_HEIGHT = 'Maximum Height';
  TXT_INCHES = ' (inches)';
  TXT_MMETERS = ' (mm)';
  TXT_C_FACTOR = '*Hazen-Williams C-factor';
  TXT_ROUGHNESS = '*Darcy-Weisbach roughness height';
  MSG_BAD_DATA = ' is invalid. Please re-enter a value.';
  MSG_BLANK_STREET = 'Street name cannot be blank.';
  MSG_BLANK_TSECT = 'Transect name cannot be blank.';
  MSG_BLANK_CURVE = 'Shape curve name cannot be blank.';
  MSG_TOO_WIDE = 'Shape must have height greater than width.';
  MSG_TOO_TALL = 'Shape must have width greater than height.';
  MSG_TOO_DEEP = 'Filled depth must be less than full height.';

  // Minor and major axis lengths for standard ellipsoid pipes
  // in both inches and mm
  EllipseMinorAxisIN: array[1..23] of Integer =
  (14,19,22,24,27,29,32,34,38,43,48,53,58,63,68,72,77,82,87,92,97,106,116);

  EllipseMajorAxisIN: array[1..23] of Integer =
  (23,30,34,38,42,45,49,53,60,68,76,83,91,98,106,113,121,128,136,143,151,166,180);

  EllipseMinorAxisMM: array[1..23] of Integer =
  (356,483,559,610,686,737,813,864,965,1092,1219,1346,1473,1600,
   1727,1829,1956,2083,2210,2337,2464,2692,2946);

  EllipseMajorAxisMM: array[1..23] of Integer =
  (584,762,864,965,1067,1143,1245,1346,1524,1727,1930,2108,2311,
   2489,2692,2870,3073,3251,3454,3632,3835,4216,4572);

   ArchHeightIN: array[1..102] of String =
   // Concrete
   ('11','13.5','15.5','18','22.5','26.625','31.3125','36','40','45','54','62',
    '72','77.5','87.125','96.875','106.5',
   // Corrugated Steel - 2-2/3 x 1/2" corrugations
    '13','15','18','20','24','29','33','38','43','47','52','57',
   // Corrugated Steel - 3 x 1" corrugations
    '31','36','41','46','51','55','59','63','67','71','75','79','83','87','91',
   // Structural Plate - 18" Corner Radius
    '55','57','59','61','63','65','67','69','71','73','75','77','79','81','83',
    '85','87','89','91','93','95','97','100','101','103','105','107','109',
    '111','113','115','118','119','121',
   // Structural Plate - 31" Corner Radius
    '112','114','116','118','120','122','124','126','128','130','132','134',
    '136','138','140','142','144','146','148','150','152','154','156','158');

    ArchWidthIN: array[1..102] of String =
   // Concrete
    ('18','22','26','28.5','36.25','43.75','51.125','58.5','65','73','88','102',
    '115','122','138','154','168.75',
   // Corrugated Steel - 2-2/3 x 1/2" corrugations
     '17','21','24','28','35','42','49','57','64','71','77','83',
   // Corrugated Steel - 3 x 1" corrugations
     '40','46','53','60','66','73','81','87','95','103','112','117','128',
     '137','142',
   // Structural Plate - 18" Corner Radius
     '73','76','81','84','87','92','95','98','103','106','112','114','117',
     '123','128','131','137','139','142','148','150','152','154','161','167',
     '169','171','178','184','186','188','190','197','199',
   // Structural Plate - 31" Corner Radius
     '159','162','168','170','173','179','184','187','190','195','198','204',
     '206','209','215','217','223','225','231','234','236','239','245','247');

var
  StdSizesUnits: String;
  CF: Single;
  EllipseMinorAxis: array[1..23] of Integer;
  EllipseMajorAxis: array[1..23] of Integer;

  OldShape: String;
  OldGeom1: String;
  OldGeom2: String;
  OldGeom3: String;
  OldGeom4: String;
  OldTsect: String;
  OldBarrels: String;

procedure TXsectionForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
// OnCreate handler for form.
//-----------------------------------------------------------------------------
var
  I: Integer;
  ListItem: TListItem;

begin
  // Load images into the ShapeListView
  with ShapeListView do
  begin
    LargeImages := ImageList1;
    for I := 0 to ImageList1.Count - 1 do
    begin
      ListItem := Items.Add;
      Listitem.Caption := ShapeNames[I];
      ListItem.ImageIndex := I;
    end;
  end;
  SpecialNote.Caption := '';
  SelectedShapeIndex := -1;

  // Set position & visibility of controls
  TsectLabel.Visible := False;
  TsectCombo.Visible := False;
  TsectBtn.Visible := False;
  with MainForm.ProjectImageList do
    GetBitmap(GetIndexByName('edit'), TsectBtn.Glyph);

  SizesCombo.Visible := False;
  SizesCombo.Top := NumEdit4.Top;

  ForceMainNote.Left := Label3.Left;
  ForceMainNote.Top := Label3.Top;
  ForceMainNote.Visible := False;

  SidewallsLabel.Left := Label3.Left;
  SidewallsLabel.Top := Label3.Top;
  SidewallsLabel.Visible := False;
  SidewallsCombo.Left := NumEdit3.Left;
  SidewallsCombo.Top := NumEdit3.Top;
  SidewallsCombo.Visible := False;

  // Initialize variables that depend on unit system
  if Uglobals.UnitSystem = usUS then
  begin
    UnitsLabel.Caption := TXT_DIMENSIONS_IN + TXT_FEET;
    StdSizesUnits := TXT_INCHES;
    for I := 1 to High(EllipseMinorAxis) do
    begin
      EllipseMinorAxis[I] := EllipseMinorAxisIN[I];
      EllipseMajorAxis[I] := EllipseMajorAxisIN[I];
    end;
    CF := 12.0;
  end
  else
  begin
    UnitsLabel.Caption := TXT_DIMENSIONS_IN + TXT_METERS;
    StdSizesUnits := TXT_MMETERS;
    for I := 1 to High(EllipseMinorAxis) do
    begin
      EllipseMinorAxis[I] := EllipseMinorAxisMM[I];
      EllipseMajorAxis[I] := EllipseMajorAxisMM[I];
    end;
    CF := 1000.0;
  end;
  UnitsLabel.Caption := UnitsLabel.Caption + TXT_OTHERWISE;

  // Initialize the description of roughness factor
  if SameText(Project.Options.Data[FORCE_MAIN_EQN_INDEX], 'H-W') then
    ForceMainNote.Caption := TXT_C_FACTOR
  else if Uglobals.UnitSystem = usUS
  then ForceMainNote.Caption := TXT_ROUGHNESS + TXT_INCHES
  else ForceMainNote.Caption := TXT_ROUGHNESS + TXT_MMETERS;
end;

procedure TXsectionForm.FormShow(Sender: TObject);
//-----------------------------------------------------------------------------
// The form's OnShow handler.
//-----------------------------------------------------------------------------
begin
  ShapeListView.Items[SelectedShapeIndex].MakeVisible(False);
  ShapeListView.Items[SelectedShapeIndex].Selected := True;
  ShapeListView.Selected.Focused := True;
end;

procedure TXsectionForm.ShapeListViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
//-----------------------------------------------------------------------------
// OnSelectItem handler for the ShapeListView component.
//-----------------------------------------------------------------------------
begin
  if Selected then
  begin
    if Item.Index = SelectedShapeIndex then Exit;
    PriorShapeIndex := SelectedShapeIndex;
    SelectedShapeIndex := Item.Index;
    if PriorShapeIndex = STREET_SHAPE_INDEX then
      PriorStreetShapeName := TsectCombo.Text;
    if PriorShapeIndex = IRREG_SHAPE_INDEX then
      PriorTransectName := TsectCombo.Text;
    SetupDataFields;
  end;
end;

procedure TXsectionForm.SetupDataFields;
//-----------------------------------------------------------------------------
// Sets up the proper data entry fields for the selected shape.
//-----------------------------------------------------------------------------
var
  I: Integer;
  J: Integer;
  S: String;
begin

  I := SelectedShapeIndex;
  SpecialNote.Caption := XsectNotes[I];

  // Asssign visibility & labels to controls depending on shape selected
  UnitsLabel.Visible := True;
  TsectLabel.Visible := False;
  TsectCombo.Visible := False;
  TsectBtn.Visible := False;
  SizesCombo.Visible := False;

  Label1.Caption := TXT_MAX_HEIGHT;
  Label2.Caption := XsectShapes[I].Text[2];
  Label3.Caption := XsectShapes[I].Text[3];
  Label4.Caption := XsectShapes[I].Text[4];

  NumEdit1.Enabled := True;
  NumEdit2.Enabled := True;
  NumEdit3.Enabled := True;
  NumEdit1.Visible := True;
  NumEdit2.Visible := (Length(XsectShapes[I].Text[2]) > 0);
  NumEdit3.Visible := (Length(XsectShapes[I].Text[3]) > 0);
  NumEdit4.Visible := (Length(XsectShapes[I].Text[4]) > 0);
  ForceMainNote.Visible := False;
  SidewallsLabel.Visible := False;
  SidewallsCombo.Visible := False;

  // Special case for Street and Irregular cross sections
  if (I = STREET_SHAPE_INDEX) or (I = IRREG_SHAPE_INDEX) then
  begin
    if I = IRREG_SHAPE_INDEX then
    begin
      TsectCombo.Items := Project.Lists[TRANSECT];
      TsectCombo.Text := PriorTransectName;
      TsectLabel.Caption := TXT_TRANSECT_NOTE;
    end else
    begin
      TsectCombo.Items := Project.Lists[STREET];
      TsectCombo.Text := PriorStreetShapeName;
      TsectLabel.Caption := TXT_STREET_NOTE;
    end;
    TsectLabel.Top := BarrelsLabel.Top;
    TsectCombo.Top := TsectLabel.Top + TsectLabel.Height + 4;
    TsectBtn.Top := TsectCombo.Top;
    NumEdit1.Visible := False;
    TsectLabel.Visible := True;
    TsectCombo.Visible := True;
    TsectBtn.Visible := True;
  end;

  // Special case for Custom shape
  if I = CUSTOM_SHAPE_INDEX then
  begin
    TsectCombo.Items := Project.Lists[SHAPECURVE];
    TsectLabel.Top := Label2.Top;
    TsectCombo.Top := TsectLabel.Top + TsectLabel.Height + 4;
    TsectBtn.Top := TsectCombo.Top;
    TsectLabel.Caption := TXT_CUSTOM_NOTE;
    TsectLabel.Visible := True;
    TsectCombo.Visible := True;
    TsectBtn.Visible := True;
  end;

  // Special case for Force Main shape
  if I = FORCE_MAIN_INDEX then
  begin
    ForceMainNote.Visible := True;
  end;

  // Special case for Open Rectangular shape
  if I = RECTANGULAR_SHAPE_INDEX then
  begin
    NumEdit3.Visible := False;
    SidewallsLabel.Visible := True;
    SidewallsCombo.Visible := True;
  end;

  // Special case for Ellipse shapes
  if I in [HORIZ_ELLIPSE_SHAPE_INDEX, VERT_ELLIPSE_SHAPE_INDEX] then
  begin
    NumEdit3.Enabled := False;
    Label4.Visible := True;
    Label4.Caption := Label4.Caption + StdSizesUnits;
    NumEdit4.Visible := False;
    SizesCombo.Visible := True;
    SizesCombo.Clear;

    if I = HORIZ_ELLIPSE_SHAPE_INDEX then
    begin
      SizesCombo.Items.Add('Custom');
      for J  := 1 to High(EllipseMinorAxis) do
        SizesCombo.Items.Add(
                             IntToStr(EllipseMinorAxis[J]) + ' x ' +
                             IntToStr(EllipseMajorAxis[J]));
    end;

    if I = VERT_ELLIPSE_SHAPE_INDEX then
    begin
      SizesCombo.Items.Add('Custom');
      for J  := 1 to High(EllipseMajorAxis) do
        SizesCombo.Items.Add(
                             IntToStr(EllipseMajorAxis[J]) + ' x ' +
                             IntToStr(EllipseMinorAxis[J]));
    end;
    J := StrToIntDef(NumEdit3.Text, 0);
    if J >= SizesCombo.Items.Count then J := 0;
    SizesCombo.ItemIndex := J;
    SizesComboClick(self);
  end;

  // Special case for Arch shape
  if I = ARCH_SHAPE_INDEX then
  begin
    NumEdit3.Enabled := False;
    Label4.Visible := True;
    Label4.Caption := Label4.Caption + StdSizesUnits;
    NumEdit4.Visible := False;
    J := StrToIntDef(NumEdit3.Text, 0);
    if J >= High(ArchHeightIN) then
    begin
      J := 0;
      NumEdit3.Text := '0';
    end;
    S := GetArchSize(J);
    SizesCombo.Visible := True;
    DisplayStdArchSize(S);
    NumEdit1.Enabled := (J = 0);
    NumEdit2.Enabled := (J = 0);
  end;

  // Special case for Dummy shape
  if I = MAX_SHAPE_INDEX then
  begin
    Label1.Caption := '';
    NumEdit1.Visible := False;
    UnitsLabel.Visible := False;
  end;

  Label1.Visible := NumEdit1.Visible;
  Label3.Visible := NumEdit3.Visible;
  BarrelsLabel.Visible := NumEdit1.Visible;
  BarrelsEdit.Visible := NumEdit1.Visible;
  BarrelsUpDown.Visible := NumEdit1.Visible;
end;

procedure TXsectionForm.SetData(const Shape: String; const Geom1: String;
  const Geom2: String; const Geom3: String; const Geom4: String;
  const Barrels: String; const Tsect: String);
//-----------------------------------------------------------------------------
//  Loads a given set of cross-section parameters into the form.
//  Shape = name of cross section shape,
//  Geom1 .. Geom4 = shape's dimensional parameters
//  Barrels = number of barrels
//  Tsect = name of transect object for irregular shapes
//-----------------------------------------------------------------------------
var
  I         : Integer;
  K         : Integer;
  MaxShapes : Integer;
  S         : String;
begin
  // If Barrels string empty, then form is being used to set defaults
  if Length(Barrels) = 0 then
  begin
    BarrelsEdit.Enabled := False;
    BarrelsUpDown.Enabled := False;
    MaxShapes := CUSTOM_SHAPE_INDEX - 1;
  end
  else
  begin
    MaxShapes := MAX_SHAPE_INDEX;
    I := StrToIntDef(Barrels, 1);
    if I >= 1 then BarrelsUpDown.Position := I;
  end;

  // Add remaining parameters to form
  S := Shape;
  TsectCombo.Text := Tsect;
  NumEdit1.Text := Geom1;
  NumEdit2.Text := Geom2;
  NumEdit3.Text := Geom3;
  NumEdit4.Text := Geom4;
  if SameText(S, 'STREET') then
    PriorStreetShapeName := Tsect;
  if SameText(S, 'IRREGULAR') then
    PriorTransectName := Tsect;

  // Find index of current shape in shape list view
  I := -1;
  for K := 0 to MaxShapes do
  begin
    if SameText(S, XsectShapes[K].Text[1]) then
    begin
      I := K;
      break;
    end;
  end;

  // Set number of sidewall removed for rectangular shapes
  if I = RECTANGULAR_SHAPE_INDEX then
  begin
    SidewallsCombo.ItemIndex := StrToIntDef(Geom3, 0);
  end;

  // Accommodate older format for std. ellipse & arch shapes
  if I in [HORIZ_ELLIPSE_SHAPE_INDEX, VERT_ELLIPSE_SHAPE_INDEX,
           ARCH_SHAPE_INDEX] then
  begin
    if (StrToIntDef(Geom1, 0) > 0)
    and (StrToFloatDef(Geom2, -1) = 0) then
    begin
      NumEdit1.Text := '';
      NumEdit2.Text := '';
      NumEdit3.Text := Geom1;
    end;
  end;

  // Highlight the current shape in the ShapeListView control
  if I >= 0 then
  begin
    ShapeListView.Selected := ShapeListView.Items[I];
  end;

  HasChanged := False;
  SelectedShapeIndex := I;
  OldShapeIndex := I;
  PriorShapeIndex := I;

  // Save current shape data
  OldShape := Shape;
  OldGeom1 := Geom1;
  OldGeom2 := Geom2;
  OldGeom3 := Geom3;
  OldGeom4 := Geom4;
  OldTsect := Tsect;
  OldBarrels := Barrels;
end;

procedure TXsectionForm.GetData(var Shape: String; var Geom1: String;
  var Geom2: String; var Geom3: String; var Geom4: String;
  var Barrels: String; var Tsect: String);
//-----------------------------------------------------------------------------
//  Retrieves cross-section parameters from the form.
//-----------------------------------------------------------------------------
var
  I: Integer;
  K: Integer;
begin
  K := SelectedShapeIndex;  //ShapeListView.ItemIndex;
  Shape := XsectShapes[K].Text[1];
  if NumEdit1.Visible then Geom1 := NumEdit1.Text else Geom1 := '0';
  if NumEdit2.Visible then Geom2 := NumEdit2.Text else Geom2 := '0';
  if NumEdit3.Visible then Geom3 := NumEdit3.Text else Geom3 := '0';
  if NumEdit4.Visible then Geom4 := NumEdit4.Text else Geom4 := '0';

  if K = STREET_SHAPE_INDEX then
  begin
    Geom1 := '';
    Barrels := '1';
    Tsect := TsectCombo.Text;
    I := Project.Lists[STREET].IndexOf(Tsect);
    if I >= 0
    then Geom1 := TStreet(Project.Lists[STREET].Objects[I]).MaxDepth;
  end

  else if K = IRREG_SHAPE_INDEX then
  begin
    Geom1 := '';
    Barrels := '1';
    Tsect := TsectCombo.Text;
    I := Project.Lists[TRANSECT].IndexOf(Tsect);
    if I >= 0
    then Geom1 := TTransect(Project.Lists[TRANSECT].Objects[I]).Data[TRANSECT_MAX_DEPTH];
  end

  else if K = CUSTOM_SHAPE_INDEX then
  begin
    Geom1 := NumEdit1.Text;
    Geom2 := TsectCombo.Text;
    Barrels := IntToStr(BarrelsUpDown.Position);
    Tsect := TsectCombo.Text;
  end

  else if K = RECTANGULAR_SHAPE_INDEX then
  begin
    Geom3 := IntToStr(SidewallsCombo.ItemIndex);
    Barrels := IntToStr(BarrelsUpDown.Position);
  end

  else if K = MAX_SHAPE_INDEX then  // for Dummy shape
  begin
    Geom1 := '0';
    Barrels := '1';
    Tsect := '';
  end

  else
  begin
    Geom1 := NumEdit1.Text;
    Barrels := IntToStr(BarrelsUpDown.Position);
    Tsect := '';
  end;
  //if K <> OldShapeIndex then HasChanged := True;

  if not SameText(Shape, OldShape) then HasChanged := true;
  if not SameText(Geom1, OldGeom1) then HasChanged := true;
  if not SameText(Geom2, OldGeom2) then HasChanged := true;
  if not SameText(Geom3, OldGeom3) then HasChanged := true;
  if not SameText(Geom4, OldGeom4) then HasChanged := true;
  if not SameText(Tsect, OldTsect) then Haschanged := true;
  if not SameText(Barrels, OldBarrels) then HasChanged := true;
end;

procedure TXsectionForm.NumEdit1Change(Sender: TObject);
//-----------------------------------------------------------------------------
// OnChange handler for all 4 NumEdit controls as well as the TsectCombo
// control.
//-----------------------------------------------------------------------------
begin
  //HasChanged := True;
end;

procedure TXsectionForm.OKBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the OK button.
//-----------------------------------------------------------------------------
begin
  if ValidateData then ModalResult := mrOK;
end;

function  TXsectionForm.ValidateData: Boolean;
//-----------------------------------------------------------------------------
// Validates data entries on the form.
//-----------------------------------------------------------------------------
var
  I: Integer;
  H, W: Extended;
begin
  Result := True;
  I := ShapeListView.ItemIndex;
  // Check for valid numerical entries
  if NumEdit1.Visible then Result := Result and ValidateTextEntry(1, I);
  if NumEdit2.Visible then Result := Result and ValidateTextEntry(2, I);
  if NumEdit3.Visible then Result := Result and ValidateTextEntry(3, I);
  if NumEdit4.Visible then Result := Result and ValidateTextEntry(4, I);

  // For Filled Circular shape filled depth must be < full depth
  if (I = FILLED_CIRC_INDEX) and
    (StrToFloatDef(NumEdit2.Text, 0) >= StrToFloatDef(NumEdit1.Text, 0)) then
  begin
    Uutils.MsgDlg(MSG_TOO_DEEP, mtError, [mbOK]);
    NumEdit2.SetFocus;
    Result := False;
  end;

  // For Street shape, check that a Street name was supplied
  if (I = STREET_SHAPE_INDEX) and (Length(Trim(TsectCombo.Text)) = 0) then
  begin
    Uutils.MsgDlg(MSG_BLANK_STREET, mtError, [mbOK]);
    TsectCombo.SetFocus;
    Result := False;
  end;

  // For Irregular shape, check that a Transect name was supplied
  if (I = IRREG_SHAPE_INDEX) and (Length(Trim(TsectCombo.Text)) = 0) then
  begin
    Uutils.MsgDlg(MSG_BLANK_TSECT, mtError, [mbOK]);
    TsectCombo.SetFocus;
    Result := False;
  end;

  // For Custom closed shape, check that a Shape Curve name was supplied
  if (I = CUSTOM_SHAPE_INDEX) and (Length(Trim(TsectCombo.Text)) = 0) then
  begin
    Uutils.MsgDlg(MSG_BLANK_CURVE, mtError, [mbOK]);
    TsectCombo.SetFocus;
    Result := False;
  end;

  // For the Ellipse and Arch shapes with custom sizes, check for a
  // valid relation between height and width
  if (Result = True)
  and (I in [HORIZ_ELLIPSE_SHAPE_INDEX, VERT_ELLIPSE_SHAPE_INDEX, ARCH_SHAPE_INDEX])
  and SameText(NumEdit3.Text, '0') then // signifies a custom size
  begin

    // Get height and width dimensions
    H := StrToFloatDef(NumEdit1.Text, 0);
    W := StrToFloatDef(NumEdit2.Text, 0);

    // Verical Ellipse must be higher than wide
    if I = VERT_ELLIPSE_SHAPE_INDEX then
    begin
      if W > H then
      begin
          Uutils.MsgDlg(MSG_TOO_WIDE, mtError, [mbOK]);
          Result := False;
      end;
    end

    // Horiz. Ellipse and Arch must be wider than high
    else if H > W then
    begin
      Uutils.MsgDlg(MSG_TOO_TALL, mtError, [mbOK]);
      Result := False;
    end;
  end;

end;

function  TXsectionForm.ValidateTextEntry(const N: Integer;
  const I: Integer): Boolean;
//-----------------------------------------------------------------------------
// Validates the entry made in a NumEdit control for a given shape.
//   N is the index of the NumEdit control being validated and
//   I is the xsection shape index.
//-----------------------------------------------------------------------------
var
  X: Single;
  Entry: String;
  LblTxt: String;
begin
  // Get text of label next to the NumEdit control
  with (FindComponent('Label' + IntToStr(N)) as TLabel) do LblTxt := Caption;

  // Identify the edit control
  with (FindComponent('NumEdit' + IntToStr(N)) as TNumEdit) do
  begin

    // Assume entry is 0 if edit control is blank
    Result := True;
    if not Enabled then Exit;
    Entry := Trim(Text);
    if Length(Entry) = 0 then Entry := '0';

    // Convert the string to a number
    if not Uutils.GetSingle(Entry, X) then Result := False

    // If the number is 0, then check if the type of shape permits it
    else if (X = 0) then
    begin
      if (N = 2) and (I = CIRC_SHAPE_INDEX) then Exit;
      if (N = 3) and
        (I in [MODBASKET_SHAPE_INDEX, TRAPEZOIDAL_SHAPE_INDEX]) then Exit;
      if (N = 4) and (I = TRAPEZOIDAL_SHAPE_INDEX) then Exit;
      Result := False;
    end;

    // If number is invalid, alert user and re-focus on the NumEdit control
    if Result = False then
    begin
      Uutils.MsgDlg(LblTxt + MSG_BAD_DATA, mtError, [mbOK]);
      if Visible then SetFocus;
    end;
  end;
end;

procedure TXsectionForm.TsectBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the TSectBtn control. Launches the Shape Curve editor
// for the Closed Custom shape or the Transect editor for the Irregular shape
// named in the TsectCombo box.
//-----------------------------------------------------------------------------
var
  I: Integer;
  S: String;
begin
  S := Trim(TsectCombo.Text);

  if ShapeListView.ItemIndex = CUSTOM_SHAPE_INDEX then
  begin
    I := Project.Lists[SHAPECURVE].IndexOf(S);
    S := Uedit.EditCurve(SHAPECURVE, I);
    if Length(S) > 0 then
    begin
      TsectCombo.Text := S;
      TsectCombo.Items := Project.Lists[SHAPECURVE];
    end;
    Exit;
  end;

  if ShapeListView.ItemIndex = STREET_SHAPE_INDEX then
  begin
    I := Project.Lists[STREET].IndexOf(S);
    S := Uedit.EditStreet(I);
    if Length(S) > 0 then
    begin
      TsectCombo.Text := S;
      TsectCombo.Items := Project.Lists[STREET];
    end;
    Exit;
  end;

  I := Project.Lists[TRANSECT].IndexOf(S);
  S := Uedit.EditTransect(I);
  if Length(S) > 0 then
  begin
      TsectCombo.Text := S;
      TsectCombo.Items := Project.Lists[TRANSECT];
  end;
end;

procedure TXsectionForm.TsectComboKeyPress(Sender: TObject; var Key: Char);
//-----------------------------------------------------------------------------
// OnKeyPress handler for TsectCombo control. Prevents user from entering
// a space character.
//-----------------------------------------------------------------------------
begin
  if (Key = ' ') or (Key = '"') or (Key = ';') then Key := #0;
end;

procedure TXsectionForm.SizesComboClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the SizesCombo control.
//-----------------------------------------------------------------------------
var
  J: Integer;
  X1, X2, Xtemp: Extended;
begin
  // Only applies to ellipse shapes
  if (SelectedShapeIndex <> HORIZ_ELLIPSE_SHAPE_INDEX)
  and (SelectedShapeIndex <> VERT_ELLIPSE_SHAPE_INDEX) then Exit;

  // Place the size code in NumEdit3
  J := SizesCombo.ItemIndex;
  NumEdit3.Text := IntToStr(J);

  // Enable NumEdit1 (height) and NumEdit2 (width) for Custom size;
  // disable them for a standard size
  NumEdit1.Enabled := (J = 0);
  NumEdit2.Enabled := (J = 0);

  // Place std. height and width values in NumEdits 1 & 2
  if J > 0 then
  begin
    X1 := EllipseMinorAxis[J] / CF;
    X2 := EllipseMajorAxis[J] / CF;
    if ShapeListView.ItemIndex = VERT_ELLIPSE_SHAPE_INDEX then
    begin
      Xtemp := X1;
      X1 := X2;
      X2 := Xtemp;
    end;
    NumEdit1.Text := Format('%.3f', [X1]);
    NumEdit2.Text := Format('%.3f', [X2]);
   end;
end;

procedure TXsectionForm.SizesComboDropDown(Sender: TObject);
//-----------------------------------------------------------------------------
// OnDropDown handler for the SizesCombo control. Launches a dialog form
// to select a standard arch pipe size.
//-----------------------------------------------------------------------------
var
  I: Integer;
  StdSize: String;
begin
  // Only applies to the Arch shape
  I := SelectedShapeIndex;
  if I <> ARCH_SHAPE_INDEX then Exit;

  // Launch the size selection form (SizeCode, MaxHeight & MaxWidth
  // are public variables of that form).
  with TArchPipeForm.Create(self) do
  try
     SizeCode := StrToIntDef(NumEdit3.Text, 0);
     if ShowModal = mrOK then
     begin
       // Place the size code in NumEdit3
       NumEdit3.Text := IntToStr(SizeCode);

       // Place the selected height and width (in ft or m) in NumEdits 1 & 2
       if Length(MaxHeight) > 0
       then NumEdit1.Text := Format('%.3f', [StrToFloatDef(MaxHeight,0)/CF]);
       if Length(MaxWidth) > 0
       then NumEdit2.Text := Format('%.3f', [StrToFloatDef(MaxWidth,0)/CF]);

       // Save the selected size (in in or mm) to StdSize string
       if SizeCode = 0 then StdSize := 'Custom'
       else StdSize := MaxHeight + ' x ' + MaxWidth;

       // Enable NumEdits 1 & 2 if a Custom size was selected
       NumEdit1.Enabled := (SizeCode = 0);
       NumEdit2.Enabled := (SizeCode = 0);
     end;
  finally
    Close;
  end;
  DisplayStdArchSize(StdSize);
end;

procedure TXsectionForm.DisplayStdArchSize(S: String);
begin
  // Make the StdSize string the only entry in the SizesCombo dropdown list
  if Length(S) > 0 then
  begin
    SizesCombo.Clear;
    SizesCombo.Items.Add(S);
    SizesCombo.ItemIndex := 0;
  end;
  PostMessage(SizesCombo.Handle, WM_CHAR, VK_RETURN, 0);
end;

function  TXsectionForm.GetArchSize(Index: Integer): String;
//-----------------------------------------------------------------------------
// Gets the text representation of a std. arch pipe size given its index.
//-----------------------------------------------------------------------------
var
  H, W: Extended;
begin
  Result := '';
  if Index = 0 then Result := 'Custom Size'
  else if Uglobals.UnitSystem = usUS then
    Result := ArchHeightIN[Index] + ' x ' + ArchWidthIN[Index]
  else
  begin
    H := 25.4 * StrToFloat(ArchHeightIN[Index]);
    W := 25.4 * StrToFloat(ArchWidthIN[Index]);
    Result := Format('%.0f x %.0f', [H, W]);
  end;
end;

procedure TXsectionForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 211450);
end;

procedure TXsectionForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then HelpBtnClick(Sender);
end;

end.
