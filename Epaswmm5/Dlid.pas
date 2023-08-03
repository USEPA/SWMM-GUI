unit Dlid;

{-------------------------------------------------------------------}
{                    Unit:    Dlid.pas                              }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21       (5.2.0)                }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form unit that describes the design of a generic LID     }
{   control that can be re-used throughout the study area.          }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,  ComCtrls, ExtCtrls, Vcl.Grids, Vcl.ValEdit,
  System.UITypes, System.ImageList, Vcl.ImgList, Vcl.VirtualImageList,
  Vcl.BaseImageCollection, Vcl.ImageCollection, Vcl.Imaging.pngimage,
  NumEdit, Uproject, Uglobals, Ulid, Uutils;

type
  TLidControlDlg = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    NameEdit: TNumEdit;
    TypeCombo: TComboBox;
    OkBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    SlopeLabel1: TLabel;
    Label10: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    SlopeLabel2: TLabel;
    Label34: TLabel;
    SurfaceEdit1: TNumEdit;
    SurfaceEdit2: TNumEdit;
    SurfaceEdit3: TNumEdit;
    SurfaceEdit4: TNumEdit;
    SurfaceEdit5: TNumEdit;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label12: TLabel;
    Label40: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    PavementEdit1: TNumEdit;
    PavementEdit2: TNumEdit;
    PavementEdit3: TNumEdit;
    PavementEdit4: TNumEdit;
    PavementEdit5: TNumEdit;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label6: TLabel;
    Label11: TLabel;
    SoilEdit1: TNumEdit;
    SoilEdit2: TNumEdit;
    SoilEdit3: TNumEdit;
    SoilEdit4: TNumEdit;
    SoilEdit5: TNumEdit;
    SoilEdit6: TNumEdit;
    SoilEdit7: TNumEdit;
    StorageHeightLabel: TLabel;
    Label9: TLabel;
    StorageEdit1: TNumEdit;
    StoragePanel: TPanel;
    StorageVoidLabel: TLabel;
    Label15: TLabel;
    Label33: TLabel;
    Label41: TLabel;
    Label45: TLabel;
    StorageEdit2: TNumEdit;
    StorageEdit3: TNumEdit;
    StorageEdit4: TNumEdit;
    Label35: TLabel;
    Label37: TLabel;
    Label39: TLabel;
    DrainDelayLabel: TLabel;
    Label47: TLabel;
    DrainEdit1: TNumEdit;
    DrainEdit2: TNumEdit;
    DrainEdit3: TNumEdit;
    DrainEdit4: TNumEdit;
    Label3: TLabel;
    Label46: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label50: TLabel;
    DrainMatEdit1: TNumEdit;
    DrainMatEdit2: TNumEdit;
    DrainMatEdit3: TNumEdit;
    TabSheet7: TTabSheet;
    Label32: TLabel;
    Label38: TLabel;
    RoofDrainEdit1: TNumEdit;
    Label44: TLabel;
    OptionalLabel: TLabel;
    Label51: TLabel;
    DrainAdvisorLabel: TLinkLabel;
    DrainEdit5: TNumEdit;
    DrainEdit6: TNumEdit;
    DrainCombo1: TComboBox;
    Label36: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    PavementEdit6: TNumEdit;
    Label54: TLabel;
    Label55: TLabel;
    PavementEdit7: TNumEdit;
    Label56: TLabel;
    Label57: TLabel;
    TabSheet8: TTabSheet;
    RemovalsPanel: TPanel;
    RemovalsHintLabel: TLabel;
    RemovalsEditor: TValueListEditor;
    LIDImageCollection: TImageCollection;
    LIDImageList: TVirtualImageList;
    LIDImage: TImage;
    CoveredCheckBox: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure NameEditChange(Sender: TObject);
    procedure NumEditExit(Sender: TObject);
    procedure TypeComboClick(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure DrainAdvisorLabelLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure RemovalsEditorGetEditText(Sender: TObject; ACol, ARow: Integer;
      var Value: string);
    procedure RemovalsEditorValidate(Sender: TObject; ACol, ARow: Integer;
      const KeyName, KeyValue: string);
    procedure PageControl1Change(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure NameEditKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    LidIndex: Integer;
    RemovalStr: String;
    function LayerDepthsValid: Boolean;
    function GetValue(const Txt: String): String;
  public
    { Public declarations }
    Modified: Boolean;
    procedure SetData(const Index: Integer; aLID: TLid);
    procedure GetData(var S: String; aLID: TLid);
  end;

//var
//  LidControlDlg: TLidControlDlg;

implementation

{$R *.dfm}

const
  MSG_NOZEROVALUE = 'This value cannot be left at 0';
  MSG_LESSTHANONE = 'This value must be less than 1.0.';
  MSG_NOBLANKNAME = 'Control name cannot be left blank.';
  MSG_NAMEINUSE = 'Control name already in use.';
  TXT_REMOVALS_HINT =
    'Enter percent removal for pollutants in underdrain outflow.';


procedure TLidControlDlg.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  // Add process types to ProcessCombo
  for I := 0 to High(Ulid.ProcessTypesLong) do
    TypeCombo.Items.Add(Ulid.ProcessTypesLong[I]);
  TypeCombo.ItemIndex := 0;
  RemovalsHintLabel.Caption := TXT_REMOVALS_HINT;

  // Place rain barrel CoveredCheckBox over StorageVoidLabel
  CoveredCheckBox.Top := 54;
end;

procedure TLidControlDlg.SetData(const Index: Integer; aLID: TLid);
var
  I: Integer;
  S: String;
begin
  LidIndex := Index;

  // Place the name of existing LID object in the NameEdit control
  if Index >= 0
  then NameEdit.Text := Project.Lists[LID].Strings[Index];

  // Place LID Process Type in the ProcessCombo control
  TypeCombo.ItemIndex := aLID.ProcessType;
  LIDImageList.GetIcon(TypeCombo.ItemIndex, LIDImage.Picture.Icon);

  // Fill the layer tab sheets with data
  for I := 1 to 5 do
    with FindComponent('SurfaceEdit' + IntToStr(I)) as TNumEdit do
      Text := aLID.SurfaceLayer[I-1];

  for I := 1 to 7 do
    with FindComponent('PavementEdit' + IntToStr(I)) as TNumEdit do
      Text := aLID.PavementLayer[I-1];

  for I := 1 to 7 do
    with FindComponent('SoilEdit' + IntToStr(I)) as TNumEdit do
      Text := aLID.SoilLayer[I-1];

  for I := 1 to 4 do
    with FindComponent('StorageEdit' + IntToStr(I)) as TNumEdit do
      Text := aLID.StorageLayer[I-1];
  CoveredCheckBox.Checked := SameText(aLID.StorageLayer[4], 'YES');

  for I := 1 to 6 do
    with FindComponent('DrainEdit' + IntToStr(I)) as TNumEdit do
      Text := aLID.DrainLayer[I-1];
  DrainCombo1.Items.Text := Project.Lists[CONTROLCURVE].Text;
  DrainCombo1.Text := aLid.DrainLayer[6];

  for I := 1 to 3 do
    with FindComponent('DrainMatEdit' + IntToStr(I)) as TNumEdit do
      Text := aLID.DrainMatLayer[I-1];

  RoofDrainEdit1.Text := aLID.DrainLayer[0];

  // Load contents of Pollutant Removal property list
    for I := 0 to Project.Lists[POLLUTANT].Count - 1 do
    begin
      S := Project.Lists[POLLUTANT].Strings[I];
      RemovalsEditor.InsertRow(S, aLID.DrainRemovals.Values[S], True);
    end;

  // Display the correct tab sheets for the current process type
  TypeComboClick(self);
  Modified := False;
end;

procedure TLidControlDlg.GetData(var S: String; aLID: TLid);
var
  I: Integer;
  Covered: String;
begin
  // Retrieve the name of the LID from the Name edit control
  S := Trim(NameEdit.Text);

  // Retrieve the process type
  aLID.ProcessType := TypeCombo.ItemIndex;

  // Retrieve data for each layer
  if TabSheet1.TabVisible then
    for I := 1 to 5 do
      with FindComponent('SurfaceEdit' + IntToStr(I)) as TNumEdit do
        aLID.SurfaceLayer[I-1] := GetValue(Text);

  if TabSheet2.TabVisible then
    for I := 1 to 7 do
      with FindComponent('PavementEdit' + IntToStr(I)) as TNumEdit do
        aLID.PavementLayer[I-1] := GetValue(Text);

  if TabSheet3.TabVisible then
    for I := 1 to 7 do
      with FindComponent('SoilEdit' + IntToStr(I)) as TNumEdit do
        aLID.SoilLayer[I-1] := GetValue(Text);

  if TabSheet4.TabVisible then
  begin
    for I := 1 to 4 do
      with FindComponent('StorageEdit' + IntToStr(I)) as TNumEdit do
        aLID.StorageLayer[I-1] := getValue(Text);
      if CoveredCheckBox.Visible then
      begin
        Covered := aLID.StorageLayer[4];
        if CoveredCheckBox.Checked
        then aLID.StorageLayer[4] := 'YES'
        else aLID.StorageLayer[4] := 'NO';
        if not SameText(Covered, aLID.StorageLayer[4])
        then Modified := True;
      end;
  end;

  if TabSheet5.Visible then
  begin
    for I := 1 to 6 do
      with FindComponent('DrainEdit' + IntToStr(I)) as TNumEdit do
        aLID.DrainLayer[I-1] := GetValue(Text);
    aLID.DrainLayer[6] := DrainCombo1.Text;
  end;

  if TabSheet6.Visible then
  begin
    for I := 1 to 3 do
      with FindComponent('DrainMatEdit' + IntToStr(I)) as TNumEdit do
        aLID.DrainMatLayer[I-1] := GetValue(Text);
  end;

  if TabSheet7.Visible then
  begin
    aLID.DrainLayer[0] := GetValue(RoofDrainEdit1.Text);
  end;

  if TabSheet8.Visible then
  begin
    aLID.DrainRemovals.Assign(RemovalsEditor.Strings);
  end;
end;

function TLidControlDlg.GetValue(const Txt: String): String;
//-----------------------------------------------------------------------------
//  Replaces a blank numerical field with 0.
//-----------------------------------------------------------------------------
begin
  if Length(Trim(Txt)) = 0 then Result := '0' else Result := Txt;
end;

procedure TLidControlDlg.NameEditChange(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnChange handler for the various fields on the form.
//-----------------------------------------------------------------------------
begin
  Modified := True;
end;

procedure TLidControlDlg.NameEditKeyPress(Sender: TObject; var Key: Char);
//-----------------------------------------------------------------------------
// OnKeyPress handler for the NameEdit box.
// Prevents user from entering '[' as first character.
//-----------------------------------------------------------------------------
begin
  with NameEdit as TNumEdit do
  begin
    if (Length(Text) = 0) or (SelStart = 0) then
      if Key = '[' then Key := #0;
  end;
end;

//-----------------------------------------------------------------------------
//  OnExit handler for Numerical fields that edit fractions.
//-----------------------------------------------------------------------------
procedure TLidControlDlg.NumEditExit(Sender: TObject);
var
  Pscreen: TPoint;
  Pclient: TPoint;
begin
  with Sender as TNumEdit do
  begin
    if (Length(Text) = 0) or (StrToFloat(Text) >= 1.0) then
    begin
      Pclient.X := 0;
      Pclient.Y := Height;
      Pscreen := ClientToScreen(Pclient);
      MessageDlgPos(MSG_LESSTHANONE, mtError, [mbOK], 0, Pscreen.X, Pscreen.Y);
      SetFocus;
      Exit;
    end;
  end;
end;

procedure TLidControlDlg.TypeComboClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the TypeCombo control.
//-----------------------------------------------------------------------------
var
  ShowTab1: Boolean;  //Surface layer tab
  ShowTab2: Boolean;  //Pavement layer tab
  ShowTab3: Boolean;  //Soil layer tab
  ShowTab4: Boolean;  //Storage layer tab
  ShowTab5: Boolean;  //Drain layer tab
  ShowTab6: Boolean;  //Drainmat layer tab
  Showtab7: Boolean;  //Downspout tab
  ShowTab8: Boolean;  //Pollutant removal tab
  RoofDiscon: Boolean;
  RainBarrel: Boolean;
  VegSwale: Boolean;
  RainGarden: Boolean;
  PollutCount: Integer;

begin
  ShowTab1 := True;
  ShowTab2 := False;
  ShowTab3 := False;
  ShowTab4 := True;
  ShowTab5 := True;
  ShowTab6 := False;
  ShowTab7 := False;
  ShowTab8 := False;
  PollutCount := Project.Lists[POLLUTANT].Count;

  LIDImageList.GetIcon(TypeCombo.ItemIndex, LIDImage.Picture.Icon);
  OptionalLabel.Visible := TypeCombo.ItemIndex in [Ulid.BIO_CELL,
                                                   Ulid.PERM_PAVE,
                                                   Ulid.INFIL_TRENCH];
  case TypeCombo.ItemIndex of
  Ulid.BIO_CELL: ShowTab3 := True;
  Ulid.RAIN_GARDEN:
    begin
      ShowTab3 := True;
      ShowTab4 := True;
      ShowTab5 := False;
    end;
  Ulid.GREEN_ROOF:
    begin
      ShowTab3 := True;
      ShowTab4 := False;
      ShowTab5 := False;
      ShowTab6 := True;
    end;
  Ulid.PERM_PAVE:
    begin
      ShowTab2 := True;
      ShowTab3 := True;
    end;
  Ulid.RAIN_BARREL: ShowTab1 := False;

  Ulid.ROOF_DISCON:
    begin
      ShowTab4 := False;
      ShowTab5 := False;
      ShowTab7 := True;
    end;

  Ulid.VEG_SWALE:
    begin
      ShowTab4 := False;
      ShowTab5 := False;
    end;
  end;

  if (PollutCount > 0) and (ShowTab5) then ShowTab8 := True;

  TabSheet1.TabVisible := ShowTab1;
  TabSheet2.TabVisible := ShowTab2;
  TabSheet3.TabVisible := ShowTab3;
  TabSheet4.TabVisible := ShowTab4;
  TabSheet5.TabVisible := ShowTab5;
  TabSheet6.TabVisible := ShowTab6;
  TabSheet7.TabVisible := ShowTab7;
  TabSheet8.TabVisible := ShowTab8;

  RoofDiscon := (TypeCombo.ItemIndex = Ulid.ROOF_DISCON);
  if RoofDiscon then Label4.Caption := 'Storage Depth'
  else Label4.Caption := 'Berm Height';
  Label5.Visible := not RoofDiscon;
  Label34.Visible := not RoofDiscon;
  SurfaceEdit2.Visible := not RoofDiscon;

  RainBarrel := (TypeCombo.ItemIndex = Ulid.RAIN_BARREL);
  StoragePanel.Visible := not RainBarrel;
  CoveredCheckBox.Visible := RainBarrel;
  DrainEdit4.Visible := RainBarrel;
  DrainDelayLabel.Visible := RainBarrel;
  if RainBarrel then StorageHeightLabel.Caption := 'Barrel Height'
  else StorageHeightLabel.Caption := 'Thickness';

  RainGarden := (TypeCombo.ItemIndex = Ulid.RAIN_GARDEN);
  StorageEdit1.Enabled := not RainGarden;
  StorageEdit2.Enabled := not RainGarden;
  StorageEdit4.Enabled := not RainGarden;
  if RainGarden then StorageEdit1.Text := '0';

  VegSwale := (TypeCombo.ItemIndex = Ulid.VEG_SWALE);
  SlopeLabel1.Visible := VegSwale;
  SlopeLabel2.Visible := VegSwale;
  SurfaceEdit5.Visible := VegSwale;

  if TypeCombo.ItemIndex = Ulid.RAIN_BARREL
  then PageControl1.ActivePage := TabSheet4
  else PageControl1.ActivePage := TabSheet1;

end;

procedure TLidControlDlg.OkBtnClick(Sender: TObject);
var
  S : String;
  ID: String;
  DupID: Integer;
  Pscreen: TPoint;
  Pclient: TPoint;
begin
  // Get coordinates of bottom left of edit control
  Pclient.X := NameEdit.Left;
  Pclient.Y := NameEdit.Top + NameEdit.Height + 2;

  // Check for blank name
  S := Trim(NameEdit.Text);
  if Length(S) = 0 then
  begin
    Pscreen := ClientToScreen(Pclient);
    MessageDlgPos(MSG_NOBLANKNAME, mtError, [mbOK], 0, Pscreen.X, Pscreen.Y);
    NameEdit.SetFocus;
    Exit;
  end;

  // Check for valid layer depths
  if not LayerDepthsValid then Exit;

  // Temporarily blank out existing LID name in data base
  if LidIndex >= 0 then
  begin
    ID := Project.Lists[LID].Strings[LidIndex];
    Project.Lists[LID].Strings[LidIndex] := '';
  end;

  // See if another LID with same name exists
  DupID := Project.Lists[LID].IndexOf(S);

  // Restore ID name and display error message if duplicate found
  if LidIndex >= 0 then Project.Lists[LID].Strings[LidIndex] := ID;
  if DupID >= 0 then
  begin
    Pscreen := ClientToScreen(Pclient);
    MessageDlgPos(MSG_NAMEINUSE, mtError, [mbOK], 0, Pscreen.X, Pscreen.Y);
    NameEdit.SetFocus;
    Exit;
  end;

  // Signal that dialog's contents are OK
  ModalResult := mrOk;
end;

procedure TLidControlDlg.PageControl1Change(Sender: TObject);
begin
  with Sender as TPageControl do
  begin
    case ActivePageIndex of
    0: SurfaceEdit1.SetFocus;
    1: PavementEdit1.SetFocus;
    2: SoilEdit1.SetFocus;
    3: if TypeCombo.ItemIndex = Ulid.RAIN_GARDEN
       then StorageEdit3.SetFocus
       else StorageEdit1.SetFocus;
    4: DrainEdit1.SetFocus;
    5: DrainMatEdit1.SetFocus;
    6: RoofDrainEdit1.SetFocus;
    7: RemovalsEditor.SetFocus;
    end;
  end;
end;

procedure TLidControlDlg.RemovalsEditorGetEditText(Sender: TObject; ACol,
  ARow: Integer; var Value: string);
begin
  RemovalStr := Value;
end;

procedure TLidControlDlg.RemovalsEditorValidate(Sender: TObject; ACol,
  ARow: Integer; const KeyName, KeyValue: string);
var
  X: Single;
begin
  if Length(KeyValue) = 0 then Exit;
  if not Uutils.GetSingle(KeyValue, X) then
  begin
    Uutils.MsgDlg(KeyValue + ' is not a valid number.', mtError, [mbOK], self);
    RemovalsEditor.Cells[ACol, ARow] := RemovalStr;
  end
  else if (X < 0) or (X > 100) then
  begin
    Uutils.MsgDlg('% removal must be between 0 and 100.', mtError, [mbOK], self);
    RemovalsEditor.Cells[ACol, ARow] := RemovalStr;
  end;
end;

function TLidControlDlg.LayerDepthsValid: Boolean;
var
  ErrMsg: String;
begin
  Result := False;
  if TypeCombo.ItemIndex in
    [Ulid.BIO_CELL, Ulid.GREEN_ROOF, Ulid.RAIN_GARDEN] then
  begin
    if StrToFloatDef(SoilEdit1.Text, 0) = 0 then
    begin
      ErrMsg := 'Soil layer thickness must be greater than 0.';
      PageControl1.ActivePage := TabSheet3;
      Uutils.MsgDlg(ErrMsg, mtError, [mbOK], self);
      Exit;
    end;
  end;

  if TypeCombo.ItemIndex = Ulid.PERM_PAVE then
  begin
    if StrToFloatDef(PavementEdit1.Text, 0) = 0 then
    begin
      ErrMsg := 'Pavement layer thickness must be greater than 0.';
      PageControl1.ActivePage := TabSheet2;
      Uutils.MsgDlg(ErrMsg, mtError, [mbOK], self);
      Exit;
    end;
  end;

  if TypeCombo.ItemIndex in
    [Ulid.BIO_CELL, Ulid.PERM_PAVE, Ulid.INFIL_TRENCH] then
  begin
    if StrToFloatDef(StorageEdit1.Text, 0) = 0 then
    begin
      ErrMsg := 'Storage layer thickness must be greater than 0.';
      PageControl1.ActivePage := TabSheet4;
      Uutils.MsgDlg(ErrMsg, mtError, [mbOK], self);
      Exit;
    end;
  end;

  if TypeCombo.ItemIndex = Ulid.GREEN_ROOF then
  begin
     if StrToFloatDef(DrainMatEdit1.Text, 0) = 0 then
    begin
      ErrMsg := 'Drainage mat thickness must be greater than 0.';
      PageControl1.ActivePage := TabSheet6;
      Uutils.MsgDlg(ErrMsg, mtError, [mbOK], self);
      Exit;
    end;
  end;

  if TypeCombo.ItemIndex  = Ulid.VEG_SWALE then
  begin
    if StrToFloatDef(SurfaceEdit1.Text, 0) = 0 then
    begin
      ErrMsg := 'Berm height must be greater than 0.';
      PageControl1.ActivePage := TabSheet1;
      Uutils.MsgDlg(ErrMsg, mtError, [mbOK], self);
      Exit;
    end;
  end;
  Result := True;
end;

procedure TLidControlDlg.DrainAdvisorLabelLinkClick(Sender: TObject;
  const Link: string; LinkType: TSysLinkType);
begin
  Application.HelpCommand(HELP_CONTEXT, 213530);
end;

procedure TLidControlDlg.HelpBtnClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 213280);
end;


procedure TLidControlDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then HelpBtnClick(Sender);
end;

end.
