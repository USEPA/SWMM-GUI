unit Dstorage;

{-------------------------------------------------------------------}
{                    Unit:    Dstorage.pas                          }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    05/02/22    (5.2.1)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form used to select the shape of a Storage Unit and      }
{   the parameters that define its surface area as a function       }
{   of height.                                                      }
{-------------------------------------------------------------------}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ExtCtrls, Vcl.StdCtrls, NumEdit, Vcl.Buttons,
  Vcl.WinXPanels, System.ImageList, Vcl.ImgList,
  Vcl.VirtualImageList, Vcl.BaseImageCollection, Vcl.ImageCollection,
  Vcl.ComCtrls, Math,
  Uproject, Uglobals, Uutils;

type
  TStorageForm = class(TForm)
    HelpBtn: TButton;
    CancelBtn: TButton;
    OkBtn: TButton;
    TitleLabel: TLabel;
    ListView1: TListView;
    ImageCollection1: TImageCollection;
    VirtualImageList1: TVirtualImageList;
    UnitsLabel1: TLabel;
    UnitsLabel2: TLabel;
    UnitsLabel3: TLabel;
    AreaUnitsLabel: TLabel;
    CalcLinkLabel: TLinkLabel;
    CardPanel1: TCardPanel;
    FunctionalCard: TCard;
    FuncEdit3: TNumEdit;
    FuncEdit2: TNumEdit;
    FuncEdit1: TNumEdit;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    ShapeCard: TCard;
    ShapeLabel: TLabel;
    ParamEdit1: TNumEdit;
    ParamEdit2: TNumEdit;
    ParamEdit3: TNumEdit;
    ParamLabel1: TLabel;
    ParamLabel2: TLabel;
    ParamLabel3: TLabel;
    NoteLabel: TLabel;
    TabularCard: TCard;
    ComboBox1: TComboBox;
    EditBitBtn: TBitBtn;
    Label2: TLabel;
    Label25: TLabel;
    VolumeCard: TCard;
    DepthLabel: TLabel;
    AreaLabel: TLabel;
    VolumeLabel: TLabel;
    DepthNumEdit: TNumEdit;
    AreaPanel: TPanel;
    VolumePanel: TPanel;
    Label27: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CalcLinkLabelLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure DepthNumEditChange(Sender: TObject);
    procedure DepthNumEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditBitBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    { Private declarations }
    CurrentCard: TCard;
    procedure InitParams;
    procedure FillEmptyFields;
    function  SetShapeParams: Boolean;
    function  GetStorageCurveData: Boolean;
    procedure FindAreaAndVolume(var Area: String; var Volume: String);
    procedure StorageCurveLookup(const D: Single);
  public
    { Public declarations }
    HasChanged: Boolean;
    procedure SetData(const Data: array of String);
    procedure GetData(var Data: array of String);
  end;

//var
//  StorageForm: TStorageForm;

implementation

{$R *.dfm}

uses
  Fmain, Uedit;

const
  ShowVolumeStr = '<a>Show Volume Calculator</a>';
  ShowShapeStr = '<a>Show Shape Selection</a>';

  ShapeStr: array[0..3] of String =
    ('Elliptical or Circular Cylinder',
     'Truncated Elliptical Cone',
     'Elliptical Paraboloid',
     'Rectangular Pyramid or Box (for side slope of 0)');

  Param1Str: array[0..3] of String =
    ('Major Axis Length', 'Base Major Axis Length', 'Major Axis Length',
     'Base Length');
  Param2Str: array[0..3] of String =
    ('Minor Axis Length', 'Base Minor Axis Length', 'Minor Axis Length',
     'Base Width');
  Param3Str: array[0..3] of String =
    ('', 'Side Slope (run/rise)', 'Height of Axis Surface',
     'Side Wall Slope (run/rise)');
  NoteStr: array[0..3] of String =
    ('Use Functional shape if only area is known.',
     'Side slope is in plane of major axis.',
     'Use Functional shape if only area is known.',
     'Each side wall is sloped at same angle from vertical.');

var
  L, W, H, Z, D, V: Single;
  A, A0, A1, A2: Single;
  ShapeIndex: Integer;
  Depth: array of Single;
  Area:  array of Single;
  TmpData: array[0 .. STORAGE_ATABLE_INDEX] of String;

procedure TStorageForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnCreate handler.
//-----------------------------------------------------------------------------
begin
  with MainForm.ProjectImageList do
    GetBitmap(GetIndexByName('edit'), EditBitBtn.Glyph);
  TitleLabel.Caption := 'Select a type of storage unit shape and ' +
                        'provide its parameters:';
  ComboBox1.Items.Text := Project.Lists[STORAGECURVE].Text;
  InitParams;
  if Uglobals.UnitSystem = usSI then
  begin
    UnitsLabel1.Caption := '(m)';
    UnitsLabel2.Caption := '(m)';
    UnitsLabel3.Caption := '(m)';
    AreaUnitsLabel.Caption := 'm'+#178;
    DepthLabel.Caption := 'Depth (m)';
    AreaLabel.Caption := 'Area (m' + #178 +')';
    VolumeLabel.Caption := 'Volume (m' + #179 + ')';
  end
  else
  begin
    UnitsLabel1.Caption := '(ft)';
    UnitsLabel2.Caption := '(ft)';
    UnitsLabel3.Caption := '(ft)';
    AreaUnitsLabel.Caption := 'ft'+#178;
    DepthLabel.Caption := 'Depth (ft)';
    AreaLabel.Caption := 'Area (ft' + #178 + ')';
    VolumeLabel.Caption := 'Volume (ft' + #179 + ')';
  end;
  CalcLinkLabel.Caption := ShowVolumeStr;
  ListView1.ItemIndex := -1;
end;

procedure TStorageForm.FormClose(Sender: TObject; var Action: TCloseAction);
//-----------------------------------------------------------------------------
//  OnClose handler.
//-----------------------------------------------------------------------------
begin
  Depth := nil;
  Area := nil;
end;

procedure TStorageForm.InitParams;
//-----------------------------------------------------------------------------
// Sets parameter values for each storage shape option so
// that surface area at a height of 6 ft or 2 m equals
// 1000 (either sq ft or sq m) for 2:1 side slopes.
//-----------------------------------------------------------------------------
begin
  // FuncEdit1 contains the default surface area for storage
  // unit described with a functional shape.
  FuncEdit1.Text := DefStorage[STORAGE_COEFF0_INDEX];      //'1000';
  FuncEdit2.Text := '0';
  FuncEdit3.Text := '0';

  ParamEdit1.Text := '0';
  ParamEdit2.Text := '0';
  ParamEdit3.Text := '0';
end;

procedure TStorageForm.SetData(const Data: array of String);
//-----------------------------------------------------------------------------
//  Sets form's data entry fields to current storage shape parameters
//-----------------------------------------------------------------------------
begin
  if SameText(Data[STORAGE_GEOMETRY_INDEX], 'FUNCTIONAL') then
  begin
    ShapeIndex := 4;
    FuncEdit1.Text := Data[STORAGE_COEFF0_INDEX];
    FuncEdit2.Text := Data[STORAGE_COEFF1_INDEX];
    FuncEdit3.Text := Data[STORAGE_COEFF2_INDEX];
  end
  else if SameText(Data[STORAGE_GEOMETRY_INDEX], 'TABULAR') then
  begin
    ShapeIndex := 5;
    ComboBox1.Text := Data[STORAGE_ATABLE_INDEX];
  end
  else
  begin
    ParamEdit1.Text := Data[STORAGE_COEFF1_INDEX];
    ParamEdit2.Text := Data[STORAGE_COEFF2_INDEX];
    ParamEdit3.Text := Data[STORAGE_COEFF0_INDEX];
    if SameText(Data[STORAGE_GEOMETRY_INDEX], 'CYLINDRICAL') then
      ShapeIndex := 0
    else if SameText(Data[STORAGE_GEOMETRY_INDEX], 'CONICAL') then
      ShapeIndex := 1
    else if SameText(Data[STORAGE_GEOMETRY_INDEX], 'PARABOLIC') then
    begin
      ShapeIndex := 2;
    end
    else if SameText(Data[STORAGE_GEOMETRY_INDEX], 'PYRAMIDAL') then
      ShapeIndex := 3;
  end;
  ListView1.ItemIndex := ShapeIndex;
end;

procedure TStorageForm.GetData(var Data: array of String);
//-----------------------------------------------------------------------------
//  Retrieves storage shape parameters from form's data entry fields.
//-----------------------------------------------------------------------------
begin
  if ListView1.ItemIndex = 4  then
  begin
    Data[STORAGE_GEOMETRY_INDEX] := 'FUNCTIONAL';
    Data[STORAGE_COEFF0_INDEX]   := FuncEdit1.Text;
    Data[STORAGE_COEFF1_INDEX]   := FuncEdit2.Text ;
    Data[STORAGE_COEFF2_INDEX]   := FuncEdit3.Text;
  end
  else if ListView1.ItemIndex = 5 then
  begin
    Data[STORAGE_GEOMETRY_INDEX] := 'TABULAR';
    Data[STORAGE_ATABLE_INDEX]   := ComboBox1.Text;
  end
  else
  begin
    Data[STORAGE_COEFF1_INDEX]   := ParamEdit1.Text;
    Data[STORAGE_COEFF2_INDEX]   := ParamEdit2.Text;
    Data[STORAGE_COEFF0_INDEX]   := ParamEdit3.Text;
    case ListView1.ItemIndex  of
    0:
      Data[STORAGE_GEOMETRY_INDEX] := 'CYLINDRICAL';
    1:
      Data[STORAGE_GEOMETRY_INDEX] := 'CONICAL';
    2:
      Data[STORAGE_GEOMETRY_INDEX] := 'PARABOLIC';
    3:
      Data[STORAGE_GEOMETRY_INDEX] := 'PYRAMIDAL';
    end;
    if ListView1.ItemIndex = 0 then
      Data[STORAGE_COEFF0_INDEX]   := '0';
  end;
end;

procedure TStorageForm.OkBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the OK button.
//-----------------------------------------------------------------------------
begin
  FillEmptyFields;
  if (ListView1.ItemIndex = 5) and (Length(Trim(ComboBox1.Text)) = 0) then
  begin
    Uutils.MsgDlg('Curve name cannot be blank', mtError, [mbOK], self);
    Exit;
  end;
  ModalResult := mrOK;
end;

procedure TStorageForm.FillEmptyFields;
//-----------------------------------------------------------------------------
//  Fills empty data fields with 0's.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  if ListView1.ItemIndex = 4 then
  begin
    for I := 1 to 3 do
      with FindComponent('FuncEdit' + IntToStr(I)) as TNumEdit do
        if Length(Text) = 0 then Text := '0';
  end
  else
  begin
    for I := 1 to 3 do
      with FindComponent('ParamEdit' + IntToStr(I)) as TNumEdit do
        if Length(Text) = 0 then Text := '0';
  end;
end;

procedure TStorageForm.DepthNumEditChange(Sender: TObject);
//-----------------------------------------------------------------------------
//  Displays the area & volume for a tabular storage curve when depth entry
//  field changes.
//-----------------------------------------------------------------------------
var
  Area, Volume: String;
begin
  if CardPanel1.ActiveCard = VolumeCard then
  begin
    FindAreaAndVolume(Area, Volume);
    AreaPanel.Caption := Area;
    VolumePanel.Caption := Volume;
  end;
end;

procedure TStorageForm.DepthNumEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
//-----------------------------------------------------------------------------
//  Switches from Area/Volume display back to parameter editing when the
//  escape is pressed in the depth entry field.
//-----------------------------------------------------------------------------
begin
  if Key = VK_ESCAPE then
  begin
    CalcLinkLabel.Caption := ShowVolumeStr;
    CardPanel1.ActiveCard := CurrentCard;
  end;
end;

procedure TStorageForm.EditBitBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  Launches the Curve Editor form when the EditBitBtn is pressed.
//-----------------------------------------------------------------------------
var
  I: Integer;
  S, S1: String;
begin
  S := ComboBox1.Text;
  I := Project.Lists[STORAGECURVE].IndexOf(S);
  S1 := Uedit.EditCurve(STORAGECURVE, I);
  if Length(S1) > 0 then
  begin
    ComboBox1.Items.SetStrings(Project.Lists[STORAGECURVE]);
    ComboBox1.Text := S1;
  end;
end;

procedure TStorageForm.CalcLinkLabelLinkClick(Sender: TObject; const Link: string;
  LinkType: TSysLinkType);
//-----------------------------------------------------------------------------
//  OnLinkClick handler for the CalcLinkLabel control.
//-----------------------------------------------------------------------------
begin
  // If in Area/Volume display mode then switch back to parameter editing mode
  if CardPanel1.ActiveCard = VolumeCard then
  begin
    CalcLinkLabel.Caption := ShowVolumeStr;
    CardPanel1.ActiveCard := CurrentCard;
  end else

  // Otherwise switch to Area/Volume display mode
  begin
    FillEmptyFields;
    GetData(TmpData);
    if SetShapeParams then
    begin
      CalcLinkLabel.Caption := ShowShapeStr;
      DepthNumEdit.Text := '';
      AreaPanel.Caption := '';
      VolumePanel.Caption := '';
      CardPanel1.ActiveCard := VolumeCard;
      DepthNumEdit.SetFocus;
    end
    else
      Uutils.MsgDlg('Storage curve does not exist or has invalid data.',
        mtInformation, [mbOK], self);
  end;
end;

procedure TStorageForm.ListView1SelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
//-----------------------------------------------------------------------------
//  Changes the current card panel and its contents when a storage shape is
//  selected.
//-----------------------------------------------------------------------------
begin
  ShapeIndex := ListView1.ItemIndex;
  if ShapeIndex < 0 then exit;
  if ShapeIndex = 5 then
    CardPanel1.ActiveCard := TabularCard
  else if ShapeIndex = 4 then
    CardPanel1.ActiveCard := FunctionalCard
  else
  begin
    ShapeLabel.Caption := ShapeStr[ShapeIndex];
    ParamLabel1.Caption := Param1Str[ShapeIndex];
    ParamLabel2.Caption := Param2Str[ShapeIndex];
    ParamLabel3.Caption := Param3Str[ShapeIndex];
    NoteLabel.Caption := NoteStr[ShapeIndex];
    UnitsLabel3.Visible := False;
    ParamEdit3.Visible := (ShapeIndex <> 0);
    UnitsLabel3.Visible := (ShapeIndex = 2);
    CardPanel1.ActiveCard := ShapeCard;
  end;
  CurrentCard := CardPanel1.ActiveCard;
  CalcLinkLabel.Caption := ShowVolumeStr;
end;

function TStorageForm.SetShapeParams: Boolean;
//-----------------------------------------------------------------------------
//  Retrieves the parameters for the currently selected storage shape.
//-----------------------------------------------------------------------------
begin
  Result := True;
  ShapeIndex := ListView1.ItemIndex;
  case ShapeIndex of
  0:   //Cylindrical
  begin
    Uutils.GetSingle(TmpData[STORAGE_COEFF1_INDEX], L);
    Uutils.GetSingle(TmpData[STORAGE_COEFF2_INDEX], W);
    A0 := PI * L * W / 4.0;
    A1 := 0;
    A2 := 0;
  end;
  1:   //Conical
  begin
    Uutils.GetSingle(TmpData[STORAGE_COEFF1_INDEX], L);
    Uutils.GetSingle(TmpData[STORAGE_COEFF2_INDEX], W);
    Uutils.GetSingle(TmpData[STORAGE_COEFF0_INDEX], Z);
    A0 := PI * L * W / 4.0;
    A1 := 0;
    A2 := 0;
    if L > 0 then
    begin
      A1 := PI * W * Z;
      A2 := PI * W / L * Z * Z;
    end;
  end;
  2:  //Parabolic
  begin
    Uutils.GetSingle(TmpData[STORAGE_COEFF1_INDEX], L);
    Uutils.GetSingle(TmpData[STORAGE_COEFF2_INDEX], W);
    Uutils.GetSingle(TmpData[STORAGE_COEFF0_INDEX], H);
    if H = 0 then A1 := 0
    else A1 := PI * L * W / H / 4.0;
    A2 := 0;
    A0 := 0;
  end;
  3:  //Pyramidal
  begin
    Uutils.GetSingle(TmpData[STORAGE_COEFF1_INDEX], L);
    Uutils.GetSingle(TmpData[STORAGE_COEFF2_INDEX], W);
    Uutils.GetSingle(TmpData[STORAGE_COEFF0_INDEX], Z);
    A1 := 2.0 * (L + W) * Z;
    A2 := 4.0 * Z * Z;
    A0 := L * W;
  end;
  4:  //Functional
  begin
    Uutils.GetSingle(TmpData[STORAGE_COEFF1_INDEX], A1);
    Uutils.GetSingle(TmpData[STORAGE_COEFF2_INDEX], A2);
    Uutils.GetSingle(TmpData[STORAGE_COEFF0_INDEX], A0);
  end;
  5:  //Tabular
    Result := GetStorageCurveData;
  end;
end;

function  TStorageForm.GetStorageCurveData: Boolean;
//-----------------------------------------------------------------------------
//  Retrieves the Depth/Area data for the storage curve assigned to a
//  Tabular storage shape.
//-----------------------------------------------------------------------------
var
  I, J, N: Integer;
  aCurve: TCurve;
begin
  Result := False;
  with Project.Lists[STORAGECURVE] do
  begin
    I := IndexOf(ComboBox1.Text);
    if I < 0 then exit;
    aCurve := TCurve(Objects[I]);
    N := aCurve.Xdata.Count;
    if N = 0 then exit;
    SetLength(Depth, N);
    SetLength(Area, N);
    for J := 0 to N-1 do
    begin
      if not Uutils.GetSingle(aCurve.Xdata[J], Depth[J]) then exit;
      if not Uutils.GetSingle(aCurve.Ydata[J], Area[J]) then exit;
      if (J > 0) and (Depth[J] <= Depth[J-1]) then exit;
    end;
  end;
  Result := True;
end;

procedure TStorageForm.FindAreaAndVolume(var Area: String; var Volume: String);
//-----------------------------------------------------------------------------
//  Finds the Area and Volume of the selected storage shape for the value
//  entered in the depth entry field.
//-----------------------------------------------------------------------------
var
  N: Single;
begin
  Area := '';
  Volume := '';

  // Check that depth value is valid
  if Uutils.GetSingle(DepthNumEdit.Text, D) then
  begin
    // Compute area & volume for a Functional storage shape
    if ShapeIndex = 4 then
    begin
      A := A0 + A1 * power(D,A2);
      N := A2 + 1;
      V := (A0 * D) + A1 / N * power(D,N);
    end

    // Retrieve area & volume for a Tabular storage shape
    else if ShapeIndex = 5 then
    begin
        StorageCurveLookup(D);
    end

    // Compute area & volume for all other storage shapes
    else begin
      A := A0 + D * (A1 + A2 * D);
      V := D * (A0 + D * (A1 / 2.0 + D * A2 / 3.0));
    end;
    Area := Format('%.1f', [A]);
    Volume := Format('%.1f', [V]);
  end;
end;

procedure TStorageForm.StorageCurveLookup(const D: Single);
//-----------------------------------------------------------------------------
//  Looks up the area (A) and volume (V) from the storage curve for a
//  Tabular storage shape obtained from GetStorageCurveData.
//-----------------------------------------------------------------------------
var
  I, N: Integer;
  D1, A1, Slope: Single;
begin
  // Initialize computed area and volume
  A := 0;
  V := 0;

  // Determine number of Depth/Area points on storage curve
  N := Length(Depth);
  if N = 0 then exit;
  D1 := Depth[0];
  A1 := Area[0];

  // Curve has single point
  if N = 1 then
  begin
    A := A1;
    V := A * D;
    exit;
  end;

  // Depth at or below first point
  if D <= D1 then
  begin
    // First depth point is close to 0
    if D1 < 0.000001 then
    begin
      A := A1;
      V := 0;
    end else
    // otherwise interpolate between 0 and first point
    begin
      A := A1 * D / D1;
      V := A * D / 2;
    end;
    exit;
  end;

  // Step through each successive point
  I := 1;
  while I < N do
  begin
    if Depth[I] >= D then
    begin
      Slope := (Area[I] - A1) / (Depth[I] - D1);
      A := A1 + (D - D1) * Slope;
      V := V + (D - D1) * (A1 + A)/2;
      exit;
    end else
    begin
      V := V + (Depth[I] - D1) * (A1 + Area[I]) / 2;
      D1 := Depth[I];
      A1 := Area[I];
      I := I + 1;
    end;
  end;

  // Depth is beyond last point on curve so extrapolate
  Slope := (Area[N-1] - Area[N-2]) / (Depth[N-1] - Depth[N-2]);
  A := Area[N-1];
  D1 := D - Depth[N-1];
  if Slope > 0 then
    A := A + Slope * D1;
  V := V + D1 * (A + Area[N-1]) / 2;
end;

procedure TStorageForm.HelpBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the Help button.
//-----------------------------------------------------------------------------
begin
  Application.HelpCommand(HELP_CONTEXT, 213690);
end;

procedure TStorageForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
//-----------------------------------------------------------------------------
//  Launches Help when the F1 key is pressed.
//-----------------------------------------------------------------------------
begin
  if Key = VK_F1 then HelpBtnClick(Sender);
end;

end.
