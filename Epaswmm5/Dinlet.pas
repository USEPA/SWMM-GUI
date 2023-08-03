unit Dinlet;

{-------------------------------------------------------------------}
{                    Unit:    Dinlet.pas                            }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form used to specify a particular street Inlet design.   }
{-------------------------------------------------------------------}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls,  System.ImageList,
  Vcl.ImgList, Vcl.VirtualImageList, Vcl.BaseImageCollection,
  Vcl.ImageCollection, Vcl.Buttons, NumEdit, Uutils, Uproject, Uglobals, Uinlet;

type
  TInletEditorForm = class(TForm)
    InletTypeCombo: TComboBox;
    Label2: TLabel;
    NameEdit: TNumEdit;
    Label1: TLabel;
    HelpBtn: TButton;
    CancelBtn: TButton;
    OkBtn: TButton;
    Panel1: TPanel;
    Image1: TImage;
    ImageCollection1: TImageCollection;
    VirtualImageList1: TVirtualImageList;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet4: TTabSheet;
    NumEdit3: TNumEdit;
    Label_3: TLabel;
    UnitLabel2: TLabel;
    NumEdit2: TNumEdit;
    Label5: TLabel;
    UnitLabel1: TLabel;
    NumEdit1: TNumEdit;
    Label4: TLabel;
    GrateTypeCombo: TComboBox;
    Label3: TLabel;
    ThroatTypeCombo: TComboBox;
    Label13: TLabel;
    UnitLabel4: TLabel;
    NumEdit6: TNumEdit;
    Label10: TLabel;
    UnitLabel3: TLabel;
    NumEdit5: TNumEdit;
    Label12: TLabel;
    CurveCombo: TComboBox;
    TabSheet3: TTabSheet;
    Label23: TLabel;
    NumEdit8: TNumEdit;
    UnitLabel5: TLabel;
    Label25: TLabel;
    NumEdit9: TNumEdit;
    UnitLabel6: TLabel;
    Label_4: TLabel;
    NumEdit4: TNumEdit;
    SplashUnits: TLabel;
    CurveBtn: TBitBtn;
    DiversionRB: TRadioButton;
    RatingRB: TRadioButton;
    Label6: TLabel;
    Label7: TLabel;
    procedure OkBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure InletTypeComboClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure InletTypeComboChange(Sender: TObject);
    procedure GrateTypeComboChange(Sender: TObject);
    procedure CurveBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DiversionRBClick(Sender: TObject);
    procedure RatingRBClick(Sender: TObject);
    procedure CurveComboKeyPress(Sender: TObject; var Key: Char);
    procedure NameEditKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    InletName: String;
    InletIndex: Integer;
    InletType: Integer;
    InletData: array[0..MAX_INLET_PROPS] of String;
    procedure SetActiveTabSheet;
  public
    { Public declarations }
    Modified: Boolean;
    procedure SetData(const I: Integer; const S: String; aInlet: TInlet);
    procedure GetData(var S: String; aInlet: TInlet);
  end;

//var
//  InletEditorForm: TInletEditorForm;

implementation

{$R *.dfm}

uses
  Fmain, Uedit;


procedure TInletEditorForm.OkBtnClick(Sender: TObject);
var
  I: Integer;
  T: Integer;
  S: String;
  Err: Boolean;
begin
  // Check for blank Inlet name
  S := Trim(NameEdit.Text);
  if (Length(S) = 0) then
  begin
    Uutils.MsgDlg('Inlet name field cannot be left blank.', mtError, [mbOK]);
    NameEdit.SetFocus;
    Exit;
  end;

  // Check for dupliacte Inlet name
  I := Project.Lists[INLET].IndexOf(S);
  if (I >= 0) and (I <> InletIndex) then
  begin
    Uutils.MsgDlg('An Inlet with this name already exists.',
      mtError, [mbOK]);
    NameEdit.SetFocus;
    Exit;
  end;

  // Check for valid Custom inlet capture curve name
  if SameText(InletTypeCombo.Text, 'CUSTOM') then
  begin
    Err := True;
    S := Trim(CurveCombo.Text);

    // Check for blank curve name
    if Length(S) = 0 then
      Uutils.MsgDlg('No Capture Curve name was supplied.',
        mtError, [mbOK])

    // Check if existing curve is of proper type
    else if Project.FindCurve(S, T, I) then
    begin
      if (DiversionRB.Checked) and (T <> DIVERSIONCURVE) then
        Uutils.MsgDlg('Curve ' + S + ' exists but is not a Diversion curve.',
          mtError, [mbOK])
      else if (RatingRB.Checked) and (T <> RATINGCURVE) then
        Uutils.MsgDlg('Curve ' + S + ' exists but is not a Rating curve.',
          mtError, [mbOK])
      else Err := False
    end

    // No error if curve name not in project data base
    else Err := False;
    if Err = True then
    begin
      PageControl1.ActivePage := TabSheet4;
      Exit;
    end;
  end;
  ModalResult := mrOK;
end;

procedure TInletEditorForm.CurveBtnClick(Sender: TObject);
var
  I: Integer;
  S: String;
begin
  S := Trim(CurveCombo.Text);
  if DiversionRB.Checked then
  begin
    I := Project.Lists[DIVERSIONCURVE].IndexOf(S);
    S := Uedit.EditCurve(DIVERSIONCURVE, I);
  end
  else
  begin
    I := Project.Lists[RATINGCURVE].IndexOf(S);
    S := Uedit.EditCurve(RATINGCURVE, I);
  end;
  if Length(S) > 0 then
    CurveCombo.Text := S;
end;

procedure TInletEditorForm.SetData(const I: Integer; const S: String; aInlet: TInlet);
var
  J: Integer;
  CurveName: String;
  CurveType: Integer;
begin
  InletName := S;
  InletType := aInlet.InletType;
  for J := 0 to MAX_INLET_PROPS do
    InletData[J] := aInlet.Data[J];
  InletIndex := I;
  NameEdit.Text := S;
  InletTypeCombo.ItemIndex := aInlet.InletType;

  with GrateTypeCombo do
    ItemIndex := Items.IndexOf(aInlet.Data[GRATE_INLET_TYPE]);
  for J := 1 to 6 do
  begin
    with FindComponent('NumEdit' + IntToStr(J)) as TNumEdit do
      Text := aInlet.Data[J];
  end;

  with ThroatTypeCombo do
    ItemIndex := Items.IndexOf(aInlet.Data[CURB_INLET_THROAT]);

  for J := 8 to 9 do
  begin
    with FindComponent('NumEdit' + IntToStr(J)) as TNumEdit do
      Text := aInlet.Data[J];
  end;

  CurveName := aInlet.Data[CUSTOM_INLET_DIVRSN_CURVE];
  if Length(CurveName) > 0 then CurveCombo.Text := CurveName;
  CurveName := aInlet.Data[CUSTOM_INLET_RATING_CURVE];
  if Length(CurveName) > 0 then CurveCombo.Text := CurveName;
  if Project.FindCurve(CurveName, CurveType, J) then
  begin
    if CurveType = DIVERSIONCURVE then DiversionRB.Checked := True;
    if CurveType = RATINGCURVE then RatingRB.Checked := True;
  end;

  InletTypeComboClick(self);
  Modified := False;
end;

procedure TInletEditorForm.InletTypeComboChange(Sender: TObject);
begin
  SetActiveTabSheet;
end;

procedure TInletEditorForm.InletTypeComboClick(Sender: TObject);
var
  ShowTab: array[1..4] of Boolean;
  I: Integer;
begin
  for I := 1 to 4 do
  begin
    ShowTab[I] := false;
  end;
  I := InletTypeCombo.ItemIndex;
  VirtualImageList1.GetIcon(I, Image1.Picture.Icon);
  if I = GRATE_INLET then
  begin
   ShowTab[1] := true;
   TabSheet1.Caption := 'Grate Inlet';
  end
  else if I = CURB_INLET then
  begin
    Label13.Visible := true;
    ThroatTypeCombo.Visible := true;
    ShowTab[2] := true;
    TabSheet2.Caption := 'Curb Opening Inlet';
  end
  else if I = COMBO_INLET then
  begin
    ShowTab[1] := true;
    ShowTab[2] := true;
    TabSheet1.Caption := 'Grate Inlet';
    TabSheet2.Caption := 'Curb Opening Inlet';
  end
  else if I = SLOTTED_INLET then
  begin
    ShowTab[3] := true;
    TabSheet3.Caption := 'Slotted Drain Inlet';
  end
  else if I = DROP_GRATE then
  begin
    ShowTab[1] := true;
    TabSheet1.Caption := 'Drop Grate Inlet';
  end
  else if I = DROP_CURB then
  begin
    Label13.Visible := false;
    ThroatTypeCombo.Visible := false;
    ShowTab[2] := true;
    TabSheet2.Caption := 'Drop Curb Inlet';
  end
  else if I = CUSTOM_INLET then
  begin
    ShowTab[4] := true;
    TabSheet4.Caption := 'Custom Inlet';
  end;
  for I := 1 to 4 do
  begin
    with FindComponent('TabSheet' + IntToStr(I)) as TTabSheet do
      TabVisible := ShowTab[I];
  end;
end;

procedure TInletEditorForm.NameEditKeyPress(Sender: TObject; var Key: Char);
begin
  with NameEdit as TNumEdit do
  begin
    if (Length(Text) = 0) or (SelStart = 0) then
      if Key = '[' then Key := #0;
  end;
end;

procedure TInletEditorForm.CurveComboKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = ' ') or (Key = '"') or (Key = ';') then Key := #0;
end;

procedure TInletEditorForm.DiversionRBClick(Sender: TObject);
begin
  CurveCombo.Items := Project.Lists[DIVERSIONCURVE];
end;

procedure TInletEditorForm.RatingRBClick(Sender: TObject);
begin
  CurveCombo.Items := Project.Lists[RATINGCURVE];
end;

procedure TInletEditorForm.FormCreate(Sender: TObject);
var
  I: Integer;
  LengthUnits: String;
begin
  LengthUnits := 'ft';
  SplashUnits.Caption := 'ft/s';
  if Uglobals.UnitSystem = usSI then
  begin
    LengthUnits := 'm';
    SplashUnits.Caption := 'm/s';
  end;
  for I := 1 to 6 do
    with FindComponent('UnitLabel' + IntToStr(I)) as TLabel do
      Caption := LengthUnits;
  for I := Low(InletTypes) to High(InletTypes) do
    InletTypeCombo.Items.Add(InletTypes[I]);
  for I := Low(GrateTypes) to High(GrateTypes) do
    GrateTypeCombo.Items.Add(GrateTypes[I]);
  for I := Low(ThroatTypes) to High(ThroatTypes) do
    ThroatTypeCombo.Items.Add(ThroatTypes[I]);
  with MainForm.ProjectImageList do
  begin
    GetBitmap(GetIndexByName('edit'), CurveBtn.Glyph);
  end;
  CurveCombo.Items := Project.Lists[DIVERSIONCURVE];
 end;

procedure TInletEditorForm.FormShow(Sender: TObject);
begin
  GrateTypeComboChange(self);
  SetActiveTabSheet;
end;

procedure TInletEditorForm.SetActiveTabSheet;
begin
  with PageControl1 do
  begin
    case InletTypeCombo.ItemIndex of
    0: ActivePageIndex := 0;
    1: ActivePageIndex := 1;
    2: ActivePageIndex := 0;
    3: ActivePageIndex := 2;
    4: ActivePageIndex := 0;
    5: ActivePageIndex := 1;
    6: ActivePageIndex := 3;
    end;
  end;
end;

procedure TInletEditorForm.GetData(var S: String; aInlet: TInlet);
var
  J: Integer;
begin
  S := Trim(NameEdit.Text);
  aInlet.InletType := InletTypeCombo.ItemIndex;
  aInlet.Data[GRATE_INLET_TYPE] := GrateTypeCombo.Text;
  for J := 1 to 6 do
  begin
    with FindComponent('NumEdit' + IntToStr(J)) as TNumEdit do
      aInlet.Data[J] := Text;
  end;
  aInlet.Data[CURB_INLET_THROAT] := ThroatTypeCombo.Text;
  for J := 8 to 9 do
  begin
    with FindComponent('NumEdit' + IntToStr(J)) as TNumEdit do
      aInlet.Data[J] := Text;
  end;
  if aInlet.InletType = CUSTOM_INLET then
  begin
    if DiversionRB.Checked then
    begin
      aInlet.Data[CUSTOM_INLET_DIVRSN_CURVE] := Trim(CurveCombo.Text);
      aInlet.Data[CUSTOM_INLET_RATING_CURVE] := '';
    end
    else
    begin
      aInlet.Data[CUSTOM_INLET_RATING_CURVE] := Trim(CurveCombo.Text);
      aInlet.Data[CUSTOM_INLET_DIVRSN_CURVE] := '';
    end;
  end;

  if not SameText(Inletname, S) then Modified := true;
  if InletType <> aInlet.InletType then Modified := true;
  for J := 0 to MAX_INLET_PROPS do
    if not SameText(InletData[J], aInlet.Data[J]) then Modified := true;
 end;

procedure TInletEditorForm.GrateTypeComboChange(Sender: TObject);
var
  IsVisible: Boolean;
begin
  IsVisible := SameText(GrateTypeCombo.Text, 'GENERIC');
  Label_3.Visible := IsVisible;
  Label_4.Visible := IsVisible;
  NumEdit3.Visible := IsVisible;
  NumEdit4.Visible := IsVisible;
  SplashUnits.Visible := IsVisible;
end;

procedure TInletEditorForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then HelpBtnClick(Sender);
end;

procedure TInletEditorForm.HelpBtnClick(Sender: TObject);
var
  HC: Integer;
begin
  HC := 213600;
  if (not NameEdit.Focused) and (not InletTypeCombo.Focused) then
  case PageControl1.ActivePageIndex of
  0: HC := 213620;
  1: HC := 213630;
  2: HC := 213640;
  3: HC := 213650;
  end;
  Application.HelpCommand(HELP_CONTEXT, HC);
end;

end.
