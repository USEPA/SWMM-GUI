unit Dgrouped;

{-------------------------------------------------------------------}
{                    Unit:    Dgrouped.pas                          }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21     (5.2.0)                  }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form unit used to edit a property of a group of objects  }
{   that lie within a fencelined region of the study area map.      }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, NumEdit, Uproject, Uglobals, Uutils;

type
  TEditType = (etReplace, etMultiply, etAdd);

  TGroupEditForm = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    ClassListbox: TComboBox;
    TagCheckBox: TCheckBox;
    PropertyListBox: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    PropertyNumEdit: TNumEdit;
    PropertyEditBtn: TButton;
    TagEditBox: TNumEdit;
    EditTypeListBox: TComboBox;
    ReplaceWithLabel: TLabel;
    SelectionCombo: TComboBox;
    NoOptionsLabel: TLabel;
    procedure OKBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ClassListboxChange(Sender: TObject);
    procedure TagCheckBoxClick(Sender: TObject);
    procedure PropertyListBoxChange(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure PropertyEditBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditTypeListBoxChange(Sender: TObject);
  private
    { Private declarations }
    OldClassItemIndex: Integer;
    theRegion: HRgn;
    NumObjects: Integer;
    InfilModel: Integer;
    LandUses: TStringlist;
    LUCount: Integer;
    ConduitShape: array[0..6] of String;
    procedure EditConduits(const newValue: String; const EditType: TEditType);
    function  EditConduitShape: Boolean;
    procedure EditJunctions(const newValue: String; const EditType: TEditType);
    procedure EditInfiltration(const newValue: String; const EditType: TEditType);
    function  EditInfilMethod: Boolean;
    function  EditLanduses: Boolean;
    function  EditGroundwater: Boolean;
    function  EditStorageCurve: Boolean;
    procedure EditStorage(const newValue: String; const EditType: TEditType);
    procedure EditSubcatchments(const newValue: String; const EditType: TEditType);
    function  GetNewValue(var Value1: String; const Value2: String;
              const EditType: TEditType): Boolean;
    function  ObjectQualifies(const X: Extended; const Y: Extended;
              const Tag: String): Boolean;
    function  UpdateGroundwater(S: TSubcatch): Boolean;
  public
    { Public declarations }
  end;

//var
//  GroupEditForm: TGroupEditForm;

implementation

{$R *.DFM}

uses
  Dsubland, Dgwater, Dxsect, Dinfil, Dstorage, Fmap, Ubrowser, Uinlet;

const
  TXT_PROPERTY = 'Property';
  TXT_VALUE = 'Value';
  TXT_INFIL_METHOD = 'Method';
  TXT_LANDUSES = 'Subcatchment Land Uses';
  TXT_ASSIGNED = ' Assigned';
  TXT_CLICK_TO_EDIT = '<Click to Edit>';
  TXT_WERE_CHANGED = ' were changed.' + #13 + 'Continue editing?';
  MSG_NO_DATA = 'No new value was entered.';
  MSG_NOLANDUSES = 'No land uses have been defined yet for this project.';

  ClassLabels: array[0..4] of PChar =
    ('Subcatchment', 'Infiltration', 'Junction', 'Storage Unit', 'Conduit');

  HortonInfilProps: array[0..4] of PChar =
    ('Max. Rate', 'Min. Rate', 'Decay Constant', 'Drying Time', 'Max. Volume');

  GreenAmptInfilProps: array[0..2] of PChar =
    ('Suction Head', 'Conductivity', 'Initial Deficit');

  CurveNumInfilProps: array[0..2] of PChar =
    ('Curve Number', 'Conductivity', 'Drying Time');

  EditedSubcatchProps: array[0..14] of Integer =
    (TAG_INDEX,
     SUBCATCH_RAINGAGE_INDEX, SUBCATCH_AREA_INDEX,      SUBCATCH_WIDTH_INDEX,
     SUBCATCH_SLOPE_INDEX,    SUBCATCH_IMPERV_INDEX,    SUBCATCH_IMPERV_N_INDEX,
     SUBCATCH_PERV_N_INDEX,   SUBCATCH_IMPERV_DS_INDEX, SUBCATCH_PERV_DS_INDEX,
     SUBCATCH_PCTZERO_INDEX,  SUBCATCH_LANDUSE_INDEX,
     SUBCATCH_CURBLENGTH_INDEX,
     SUBCATCH_SNOWPACK_INDEX, SUBCATCH_GWATER_INDEX);

  EditedJunctionProps: array[0..5] of Integer =
    (TAG_INDEX,  NODE_INVERT_INDEX,
     JUNCTION_MAX_DEPTH_INDEX,        JUNCTION_INIT_DEPTH_INDEX,
     JUNCTION_SURCHARGE_DEPTH_INDEX,  JUNCTION_PONDED_AREA_INDEX);

  EditedStorageProps: array[0..6] of Integer =
    (TAG_INDEX,                       NODE_INVERT_INDEX,
     STORAGE_MAX_DEPTH_INDEX,         STORAGE_INIT_DEPTH_INDEX,
     STORAGE_SURCHARGE_DEPTH_INDEX,   STORAGE_EVAP_FACTOR_INDEX,
     STORAGE_GEOMETRY_INDEX);

  EditedConduitProps: array[0..11] of Integer =
    (TAG_INDEX,               CONDUIT_SHAPE_INDEX,
     CONDUIT_GEOM1_INDEX,     CONDUIT_LENGTH_INDEX,
     CONDUIT_ROUGHNESS_INDEX, CONDUIT_INLET_HT_INDEX,
     CONDUIT_OUTLET_HT_INDEX, CONDUIT_INIT_FLOW_INDEX,
     CONDUIT_MAX_FLOW_INDEX,  CONDUIT_ENTRY_LOSS_INDEX,
     CONDUIT_EXIT_LOSS_INDEX, CONDUIT_AVG_LOSS_INDEX);

var
  GWData: array[0..Dgwater.MAX_GW_PROPS] of String;

procedure TGroupEditForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnCreate handler.
//-----------------------------------------------------------------------------
var
  I: Integer;
  S: String;
begin
  // Determine which infiltration model is being used
  InfilModel := 0;
  S := Project.Options.Data[INFILTRATION_INDEX];
  for I := 0 to High(InfilOptions) do
  begin
    if SameText(S, InfilOptions[I]) then InfilModel := I;
  end;

  // Create a landuses string list
  LandUses := TStringlist.Create;

  // Assign default values to ConduitShape
  with Project.DefProp[CONDUIT] do
  begin
    ConduitShape[0] := Data[CONDUIT_SHAPE_INDEX];
    ConduitShape[1] := Data[CONDUIT_GEOM1_INDEX];
    ConduitShape[2] := Data[CONDUIT_GEOM2_INDEX];
    ConduitShape[3] := Data[CONDUIT_GEOM3_INDEX];
    ConduitShape[4] := Data[CONDUIT_GEOM4_INDEX];
    ConduitShape[5] := Data[CONDUIT_BARRELS_INDEX];
    ConduitShape[6] := Data[CONDUIT_TSECT_INDEX];
  end;

  // Disable TagListbox control
  TagEditbox.Enabled := False;

  // Place the ReplaceWithLabel on top of the EditTypeListBox
  ReplaceWithLabel.Left := EditTypeListBox.Left;
  ReplaceWithLabel.Visible := False;
  NoOptionsLabel.Top := ReplaceWithLabel.Top;
  NoOptionsLabel.Visible := False;
  EditTypeListBox.Top := PropertyNumEdit.Top;
  SelectionCombo.Top := PropertyNumEdit.Top;

  // Populate ClassListbox with names of classes that can be edited
  for I := Low(ClassLabels) to High(ClassLabels) do
    ClassListbox.Items.Add(ClassLabels[I]);
  ClassListbox.ItemIndex := 0;
  OldClassItemIndex := -1;
  ClassListboxChange(Sender);

  //Create a GDI region from user's fenceline region of the study area map
  theRegion := CreatePolygonRgn(MapForm.Fenceline, MapForm.NumFencePts - 1,
                                WINDING);
end;

procedure TGroupEditForm.FormDestroy(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnDestroy handler.
//-----------------------------------------------------------------------------
begin
  DeleteObject(theRegion);
  LandUses.Clear;
  LandUses.Free;
end;

procedure TGroupEditForm.ClassListboxChange(Sender: TObject);
//-----------------------------------------------------------------------------
//  Updates the form when user selects a different class of object to edit.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  if OldClassItemIndex = ClassListbox.ItemIndex then exit;
  OldClassItemIndex := ClassListbox.ItemIndex;
  PropertyListBox.Clear;
  case ClassListbox.ItemIndex of
  0:   // Subcatchments
    begin
      PropertyListbox.Items.Add(SubcatchProps[TAG_INDEX].Name);
      PropertyListbox.Items.Add(SubcatchProps[SUBCATCH_RAINGAGE_INDEX].Name);
      for I := SUBCATCH_AREA_INDEX to SUBCATCH_PCTZERO_INDEX do
        PropertyListbox.Items.Add(SubcatchProps[I].Name);
      PropertyListbox.Items.Add(SubcatchProps[SUBCATCH_LANDUSE_INDEX].Name);
      PropertyListbox.Items.Add(SubcatchProps[SUBCATCH_CURBLENGTH_INDEX].Name);
      PropertyListbox.Items.Add(SubcatchProps[SUBCATCH_SNOWPACK_INDEX].Name);
      PropertyListbox.Items.Add(SubcatchProps[SUBCATCH_GWATER_INDEX].Name);
    end;

  1:  // Infiltration
    begin
      case InfilModel of
        HORTON_INFIL, MOD_HORTON_INFIL:
          for I := 0 to High(HortonInfilProps) do
            PropertyListBox.Items.Add(HortonInfilProps[I]);
        GREEN_AMPT_INFIL, MOD_GREEN_AMPT_INFIL:
          for I := 0 to High(GreenAmptInfilProps) do
            PropertyListBox.Items.Add(GreenAmptInfilProps[I]);
        CURVE_NUMBER_INFIL:
          for I := 0 to High(CurveNumInfilProps) do
            PropertyListBox.Items.Add(CurveNumInfilProps[I]);
      end;
      PropertyListBox.Items.Add(TXT_INFIL_METHOD);
    end;

  2:  // Junctions
    begin
      PropertyListbox.Items.Add(JunctionProps[TAG_INDEX].Name);
      PropertyListbox.Items.Add(JunctionProps[NODE_INVERT_INDEX].Name);
      for I := JUNCTION_MAX_DEPTH_INDEX to JUNCTION_PONDED_AREA_INDEX do
        PropertyListbox.Items.Add(JunctionProps[I].Name);
    end;

  3:  // Storage
    begin
      PropertyListbox.Items.Add(StorageProps[TAG_INDEX].Name);
      PropertyListbox.Items.Add(StorageProps[NODE_INVERT_INDEX].Name);
      for I := 2 to 6 do
        PropertyListBox.Items.Add(StorageProps[EditedStorageProps[I]].Name);
    end;

  4:  // Conduits
    begin
      for I := TAG_INDEX to CONDUIT_AVG_LOSS_INDEX do
        PropertyListbox.Items.Add(ConduitProps[I].Name);
    end;
  end;
  PropertyListbox.ItemIndex := 0;
  PropertyListBoxChange(Sender);
end;

procedure TGroupEditForm.TagCheckBoxClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  Enables the Tag edit box when the Tag check box is checked.
//-----------------------------------------------------------------------------
begin
  TagEditBox.Enabled := TagCheckBox.Checked;
end;

procedure TGroupEditForm.PropertyListBoxChange(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnChange method for PropertyListBox. Determines what kind of property
//  editing controls are enabled after user selects a particular property
//  to edit.
//-----------------------------------------------------------------------------
var
  K: Integer;
begin
  // Default is to enable the PropertyNumEdit control
  SelectionCombo.Visible := False;
  PropertyNumEdit.Text := '';
  PropertyNumEdit.Style := esPosNumber;
  if TEditType(EditTypeListBox.ItemIndex) = etAdd
  then PropertyNumEdit.Style := esNumber;
  PropertyNumEdit.Visible := True;
  PropertyNumEdit.Enabled := True;
  PropertyEditBtn.Visible := False;
  EditTypeListBox.Visible := True;
  ReplaceWithLabel.Visible := False;
  NoOptionsLabel.Visible := False;
 
  // If Subcatchment class selected,
  if ClassListBox.ItemIndex = 0 then
  begin
    K := EditedSubcatchProps[PropertyListBox.ItemIndex];
    if K = TAG_INDEX then
    begin
      EditTypeListBox.Visible := False;
      ReplaceWithLabel.Visible := True;
      PropertyNumEdit.Style := esNoSpace;
    end;

    if K = SUBCATCH_RAINGAGE_INDEX then
    begin
      EditTypeListBox.Visible := False;
      if Project.Lists[RAINGAGE].Count > 0 then
      begin
        ReplaceWithLabel.Visible := True;
        PropertyNumEdit.Visible := False;
        SelectionCombo.Visible := True;
        SelectionCombo.Items.Text := Project.Lists[RAINGAGE].Text;
        SelectionCombo.ItemIndex := 0;
      end
      else begin
        PropertyNumEdit.Visible := False;
        NoOptionsLabel.Visible := True;
      end;
    end;

    if K = SUBCATCH_SNOWPACK_INDEX then
    begin
      EditTypeListBox.Visible := False;
      if Project.Lists[SNOWPACK].Count > 0 then
      begin
        ReplaceWithLabel.Visible := True;
        PropertyNumEdit.Visible := False;
        SelectionCombo.Visible := True;
        SelectionCombo.Items.Text := Project.Lists[SNOWPACK].Text;
        SelectionCombo.ItemIndex := 0;
      end
      else begin
        PropertyNumEdit.Visible := False;
        NoOptionsLabel.Visible := True;
      end;
    end;

    // If Landuses or Groundwater properties selected,
    // then disable PropertyNumEdit control and enable the
    // PropertyEditBtn button to allow use of specialized editor
    if (K = SUBCATCH_LANDUSE_INDEX) or (K = SUBCATCH_GWATER_INDEX) then
    begin
      PropertyNumEdit.Text := TXT_CLICK_TO_EDIT;
      PropertyNumEdit.Enabled := False;
      PropertyEditBtn.Visible := True;
      EditTypeListBox.Visible := False;
      ReplaceWithLabel.Visible := True;
    end
  end

  // If Infiltration class selected
  else if ClassListBox.ItemIndex = 1 then
  begin
    if PropertyListBox.ItemIndex = PropertyListBox.Items.Count-1 then
    begin
      PropertyNumEdit.Text := TXT_CLICK_TO_EDIT;
      PropertyNumEdit.Enabled := False;
      PropertyEditBtn.Visible := True;
      EditTypeListBox.Visible := False;
      ReplaceWithLabel.Visible := True;
    end;
  end


  // If Junction class selected,
  else if ClassListBox.ItemIndex = 2 then
  begin
    K := EditedJunctionProps[PropertyListBox.ItemIndex];
    if (K = TAG_INDEX) then
    begin
      EditTypeListBox.Visible := False;
      ReplaceWithLabel.Visible := True;
      PropertyNumEdit.Style := esNoSpace;
    end;
  end

  // If Storage class selected
  else if ClassListBox.ItemIndex = 3 then
  begin
    K := EditedStorageProps[PropertyListBox.ItemIndex];
    if (K = TAG_INDEX) or (K = STORAGE_GEOMETRY_INDEX) then
    begin
      EditTypeListBox.Visible := False;
      ReplaceWithLabel.Visible := True;
      if K = TAG_INDEX then PropertyNumEdit.Style := esNoSpace
      else
      begin
        PropertyNumEdit.Text := TXT_CLICK_TO_EDIT;
        PropertyNumEdit.Enabled := False;
        PropertyEditBtn.Visible := True;
      end;
    end;
  end

  // If Conduit class selected, and Shape property selected, then disable
  // PropertyNumEdit control and enable the PropertyEditBtn button to allow
  // use of specialized editor
  else if ClassListBox.ItemIndex = 4 then
  begin
    K := EditedConduitProps[PropertyListBox.ItemIndex];
    if (K = TAG_INDEX) or (K = CONDUIT_SHAPE_INDEX) then
    begin
      EditTypeListBox.Visible := False;
      ReplaceWithLabel.Visible := True;
      if K = TAG_INDEX then PropertyNumEdit.Style := esNoSpace
      else
      begin
        PropertyNumEdit.Text := TXT_CLICK_TO_EDIT;
        PropertyNumEdit.Enabled := False;
        PropertyEditBtn.Visible := True;
      end;
    end;
  end;
end;

procedure TGroupEditForm.EditTypeListBoxChange(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnChange method for EditTypeListBox.
//-----------------------------------------------------------------------------
begin
  case TEditType(EditTypeListBox.ItemIndex) of
    etReplace,
    etMultiply:  PropertyNumEdit.Style := esPosNumber;
    etAdd:       PropertyNumEdit.Style := esNumber;
  end;
end;

procedure TGroupEditForm.PropertyEditBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  Calls a special editing function when the ellipsis button next to
//  the property's NumEdit control is clicked.
//-----------------------------------------------------------------------------
var
  K: Integer;
  WasEdited: Boolean;
begin
  WasEdited := False;

  // Call special editor for subcatchment land uses & groundwater
  if (ClassListBox.ItemIndex = 0) then
  begin
    K := EditedSubcatchProps[PropertyListBox.ItemIndex];
    if K = SUBCATCH_LANDUSE_INDEX then WasEdited := EditLanduses
    else if K = SUBCATCH_GWATER_INDEX then WasEdited := EditGroundwater;
  end

  // Call special editor for infiltration method
  else if ClassListBox.ItemIndex = 1 then
  begin
    // Last item in the Infiltration Listbox is Infiltration Method
    if PropertyListBox.ItemIndex = PropertyListBox.Items.Count-1 then
      WasEdited := EditInfilMethod;
  end

  // Call special editor for storage curve
  else if ClassListBox.ItemIndex = 3 then
  begin
    K := EditedStorageProps[PropertyListBox.ItemIndex];
    if K = STORAGE_GEOMETRY_INDEX then WasEdited := EditStorageCurve;
  end

  // Call special editor for conduit shape
  else if (ClassListBox.ItemIndex = 4) then
  begin
    K := EditedConduitProps[PropertyListBox.ItemIndex];
    if K = CONDUIT_SHAPE_INDEX then WasEdited := EditConduitShape;
  end;

  // Simulate click of OK button
  if WasEdited then OKBtnClick(Sender);
end;


function TGroupEditForm.EditLanduses: Boolean;
//-----------------------------------------------------------------------------
//  Launches the Land Uses editor.
//-----------------------------------------------------------------------------
var
  SubLandusesForm: TSubLandusesForm;
begin
  Result := False;
  if Project.Lists[LANDUSE].Count = 0 then
  begin
    Uutils.MsgDlg(MSG_NOLANDUSES, mtINFORMATION, [mbOK]);
    Exit;
  end;
  SubLandusesForm := TSubLandusesForm.Create(Application);
  try
    SubLandusesForm.SetData(LandUses);
    if SubLandusesForm.ShowModal = mrOK then
    begin
      SubLandusesForm.GetData(LandUses, LUCount);
      Result := True;
    end;
  finally
    SubLandusesForm.Free;
  end;
end;

function TGroupEditForm.EditGroundwater: Boolean;
//-----------------------------------------------------------------------------
//  Launches the Groundwater editor.
//-----------------------------------------------------------------------------
var
  GroundWaterForm: TGroundWaterForm;
begin
  Result := False;
  GroundWaterForm := TGroundWaterForm.Create(Application);
  try
    GroundWaterForm.SetGroupData;
    if GroundWaterForm.ShowModal = mrOK then
    begin
      GroundWaterForm.GetGroupData(GWData);
      Result := True;
    end;
  finally
    GroundWaterForm.Free;
  end;
end;

function TGroupEditForm.EditInfilMethod: Boolean;
//-----------------------------------------------------------------------------
//  Launches the Infiltration editor.
//-----------------------------------------------------------------------------
var
  J: Integer;
  InfilModel: String;
  InfilData: array[0..MAXINFILPROPS] of String;
  InfilForm: TInfilForm;
  S: TSubcatch;
begin
  for J := 0 to MAXINFILPROPS do InfilData[J] := Uproject.DefInfil[J];
  InfilModel := Project.Options.Data[INFILTRATION_INDEX];
  Result := false;
  InfilForm := TInfilForm.Create(Application);
  try
    InfilForm.SetInfilModel(InfilModel);
    InfilForm.SetData(InfilData);
    if InfilForm.ShowModal = mrOK then
    begin
      InfilForm.GetInfilModelName(InfilModel);
      InfilForm.GetData(InfilData);
      Result := true;
    end;
  finally
    InfilForm.Free;
  end;
  if Result then for J := 0 to Project.Lists[SUBCATCH].Count-1 do
  begin
    // See if subcatchment lies within region being edited
    S := Project.GetSubcatch(SUBCATCH, J);
    if ObjectQualifies(S.X, S.Y, S.Data[TAG_INDEX]) then
    begin
      S.Data[SUBCATCH_INFIL_INDEX] := InfilModel;
      Uutils.CopyStringArray(InfilData, S.InfilData);
      Inc(NumObjects);
    end;
  end;
end;

function TGroupEditForm.EditStorageCurve: Boolean;
//-----------------------------------------------------------------------------
//  Launches the storage curve editor.
//-----------------------------------------------------------------------------
var
  J, K: Integer;
  Data: array[0..MAXNODEPROPS] of String;
  StorageForm: TStorageForm;
  N: TNode;
begin
  Result := False;
  StorageForm := TStorageForm.Create(Application);
  with StorageForm do
  try
    Uutils.CopyStringArray(Project.DefProp[STORAGE].Data, Data);
    SetData(Data);
    if ShowModal = mrOK then
    begin
      GetData(Data);
      Result := true;
    end;
  finally
    Free;
  end;
  if Result then for J := 0 to Project.Lists[STORAGE].Count-1 do
  begin
    N := Project.GetNode(STORAGE, J);
    if ObjectQualifies(N.X, N.Y, N.Data[TAG_INDEX]) then
    begin
      for K := STORAGE_GEOMETRY_INDEX to STORAGE_ATABLE_INDEX do
        N.Data[K] := Data[K];
      Inc(NumObjects);
    end;
  end;
end;

function TGroupEditForm.EditConduitShape: Boolean;
//-----------------------------------------------------------------------------
//  Launches the Cross Section editor.
//-----------------------------------------------------------------------------
var
  XsectionForm: TXsectionForm;
begin
  Result := False;
  XsectionForm := TXsectionForm.Create(self);
  try
    XsectionForm.SetData(ConduitShape[0], ConduitShape[1], ConduitShape[2],
                         ConduitShape[3], ConduitShape[4], ConduitShape[5],
                          ConduitShape[6]);
    if XsectionForm.ShowModal = mrOK then
    begin
      XsectionForm.GetData(ConduitShape[0], ConduitShape[1], ConduitShape[2],
                           ConduitShape[3], ConduitShape[4], ConduitShape[5],
                           ConduitShape[6]);
      Result := True;
    end;
  finally
    XsectionForm.Free;
  end;
end;

procedure TGroupEditForm.OKBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the OK button. Calls the appropriate group editing
//  function depending on the class of object being edited.
//-----------------------------------------------------------------------------
var
  EditType: TEditType;
  NewValue: String;
begin
  // Check that a property value was entered.
  if  PropertyNumEdit.Visible
  and (PropertyListBox.ItemIndex > 0)
  and (Length(Trim(PropertyNumEdit.Text)) = 0) then
  begin
    Uutils.MsgDlg(MSG_NO_DATA, mtError, [mbOK]);
    if PropertyNumEdit.Enabled then ActiveControl := PropertyNumEdit;
  end

  // Call the appropriate group editing function
  else begin
    // Check if Infiltration Method was edited
    if (ClassListBox.ItemIndex = 1) and
    SameText(PropertyListBox.Text, TXT_INFIL_METHOD) then
    begin
    // Do nothing
    end

    // Check if Storage Curve was edited
    else if (ClassListBox.ItemIndex = 3) and
    (EditedStorageProps[PropertyListBox.ItemIndex] = STORAGE_GEOMETRY_INDEX)
    then begin
    // Do nothing
    end

    else begin
      NumObjects := 0;
      if EditTypeListBox.Visible
      then EditType := TEditType(EditTypeListBox.ItemIndex)
      else EditType := etReplace;
      if PropertyNumEdit.Visible
      then NewValue := PropertyNumEdit.Text
      else if SelectionCombo.Visible
      then NewValue := SelectionCombo.Text
      else exit;

      try
        case ClassListBox.ItemIndex of
        0: EditSubcatchments(NewValue, EditType);
        1: EditInfiltration(NewValue, EditType);
        2: EditJunctions(NewValue, EditType);
        3: EditStorage(NewValue, EditType);
        4: EditConduits(NewValue, EditType);
        end;
      finally
      end;
    end;

    // See if user wants to edit some more properties.
    if NumObjects > 0 then Uglobals.HasChanged := True;
    if Uutils.MsgDlg(IntToStr(NumObjects) + ' ' +
               ClassLabels[ClassListBox.ItemIndex] + 's' +
               TXT_WERE_CHANGED, mtConfirmation, [mbYes, mbNo]) = mrNo
    then ModalResult := mrOK;
  end;
end;

procedure TGroupEditForm.EditSubcatchments(const newValue: String;
  const EditType: TEditType);
//-----------------------------------------------------------------------------
//  Group edits a subcatchment property.
//-----------------------------------------------------------------------------
var
  I: Integer;
  J: Integer;
  K: Integer;
  S: TSubcatch;
  WasChanged: Boolean;
begin
  // Check each subcatchment
  for J := 0 to Project.Lists[SUBCATCH].Count-1 do
  begin
    // See if subcatchment lies within region being edited
    WasChanged := False;
    S := Project.GetSubcatch(SUBCATCH, J);
    if ObjectQualifies(S.X, S.Y, S.Data[TAG_INDEX]) then

    begin
      // Obtain index of property being edited
      K := EditedSubcatchProps[PropertyListBox.ItemIndex];

      // Special case for land use allocations
      if K = SUBCATCH_LANDUSE_INDEX then
      begin
        S.LandUses.Clear;
        for I := 0 to LandUses.Count-1 do
          S.LandUses.AddObject(LandUses.Strings[I], LandUses.Objects[I]);
        S.Data[SUBCATCH_LANDUSE_INDEX] := IntToStr(LUCount);
        WasChanged := True;
      end

      // Special case for Groundwater properties
      else if K = SUBCATCH_GWATER_INDEX then
      begin
        if UpdateGroundwater(S) then WasChanged := True;
      end

      // All other subcatchment properties get updated here
      else if GetNewValue(S.Data[K], newValue, EditType)
      then WasChanged := True;
      if WasChanged then Inc(NumObjects);
    end;
  end;

  // Update display of subcatch theme values on the map
  if NumObjects > 0
  then Ubrowser.ChangeMapTheme(SUBCATCHMENTS, CurrentSubcatchVar);
end;

function TGroupEditForm.UpdateGroundwater(S: TSubcatch): Boolean;
//-----------------------------------------------------------------------------
//  Updates the Groundwater properties of a subcatchment.
//-----------------------------------------------------------------------------
var
  K: Integer;
begin
  Result := False;
  //Remove GW from subcacthment if Aquifer Name is blank
  if Length(Trim(GWData[0])) = 0 then
  begin
    S.Groundwater.Clear;
    S.GwLatFlowEqn := '';
    S.GwDeepFlowEqn := '';
    Result := True;
    Exit;
  end
  else begin
    if S.Groundwater.Count = 0 then
    begin
      for K := 0 to MAX_GW_PROPS do
        S.Groundwater.Add(Dgwater.Default_GW_Props[K]);
      S.GwLatFlowEqn := '';
      S.GwDeepFlowEqn := '';
    end;
  end;

  //Replace properties that have been edited
  for K := 0 to S.Groundwater.Count-1 do
  begin
    if SameText(Trim(GWData[K]), '*') then continue;
    S.Groundwater[K] := GWData[K];
    Result := True;
  end;
end;

procedure TGroupEditForm.EditInfiltration(const newValue: String;
  const EditType: TEditType);
//-----------------------------------------------------------------------------
//  Group edits an infiltration property.
//-----------------------------------------------------------------------------
var
  I: Integer;
  J: Integer;
  S: TSubcatch;
begin
  // Check each subcatchment
  for J := 0 to Project.Lists[SUBCATCH].Count-1 do
  begin
    // See if subcatchment lies within region being edited
    S := Project.GetSubcatch(SUBCATCH, J);
    if ObjectQualifies(S.X, S.Y, S.Data[TAG_INDEX]) and
      SameText(S.Data[SUBCATCH_INFIL_INDEX],
               Project.Options.Data[INFILTRATION_INDEX]) then
    begin
      I := PropertyListBox.ItemIndex;
      if GetNewValue(S.InfilData[I], newValue, EditType)
      then Inc(NumObjects);
    end;
  end;
end;

procedure TGroupEditForm.EditConduits(const newValue: String;
  const EditType: TEditType);
//-----------------------------------------------------------------------------
//  Group edits a conduit property.
//-----------------------------------------------------------------------------
var
  J: Integer;
  K: Integer;
  L: TLink;
begin
  // Check each conduit
  for J := 0 to Project.Lists[CONDUIT].Count-1 do
  begin
    // See if conduit lies within region being edited
    L := Project.GetLink(CONDUIT, J);
    if ( ObjectQualifies(L.Node1.X, L.Node1.Y, L.Data[TAG_INDEX]) and
         ObjectQualifies(L.Node2.X, L.Node2.Y, L.Data[TAG_INDEX]) ) then
    begin
      K := EditedConduitProps[PropertyListBox.ItemIndex];
      if K = CONDUIT_SHAPE_INDEX then
      begin
        L.Data[CONDUIT_SHAPE_INDEX]    := ConduitShape[0];
        L.Data[CONDUIT_GEOM1_INDEX]    := ConduitShape[1];
        L.Data[CONDUIT_GEOM2_INDEX]    := ConduitShape[2];
        L.Data[CONDUIT_GEOM3_INDEX]    := ConduitShape[3];
        L.Data[CONDUIT_GEOM4_INDEX]    := ConduitShape[4];
        L.Data[CONDUIT_BARRELS_INDEX]  := ConduitShape[5];
        L.Data[CONDUIT_TSECT_INDEX]    := ConduitShape[6];
        Inc(NumObjects);
        Uinlet.CheckForValidInlet(L);
      end
      else if GetNewValue(L.Data[K], newValue, EditType)
      then Inc(NumObjects);
    end;
  end;

  // Update display of link theme values on the map
  if NumObjects > 0 then Ubrowser.ChangeMapTheme(LINKS, CurrentLinkVar);
end;

procedure TGroupEditForm.EditJunctions(const newValue: String;
  const EditType: TEditType);
//-----------------------------------------------------------------------------
//  Group edits a junction property.
//-----------------------------------------------------------------------------
var
  J: Integer;
  K: Integer;
  N: TNode;
begin
  // Check each junction
  for J := 0 to Project.Lists[JUNCTION].Count-1 do
  begin

    // See if junction lies within region being edited
    N := Project.GetNode(JUNCTION, J);
    if ObjectQualifies(N.X, N.Y, N.Data[TAG_INDEX]) then
    begin
      K := EditedJunctionProps[PropertyListBox.ItemIndex];
      if GetNewValue(N.Data[K], newValue, EditType)
      then Inc(NumObjects);
    end;
  end;

  // Update display of node theme values on the map
  if NumObjects > 0 then Ubrowser.ChangeMapTheme(NODES, CurrentNodeVar);
end;

procedure TGroupEditForm.EditStorage(const newValue: String;
  const EditType: TEditType);
//-----------------------------------------------------------------------------
//  Group edits a storage unit property.
//-----------------------------------------------------------------------------
var
  J: Integer;
  K: Integer;
  N: TNode;
begin
  // Check each storage unit node
  for J := 0 to Project.Lists[STORAGE].Count-1 do
  begin

    // See if node lies within region being edited
    N := Project.GetNode(STORAGE, J);
    if ObjectQualifies(N.X, N.Y, N.Data[TAG_INDEX]) then
    begin
      K := EditedStorageProps[PropertyListBox.ItemIndex];
      if GetNewValue(N.Data[K], newValue, EditType)
      then Inc(NumObjects);
    end;
  end;

  // Update display of node theme values on the map
  if NumObjects > 0 then Ubrowser.ChangeMapTheme(NODES, CurrentNodeVar);
end;

function TGroupEditForm.ObjectQualifies(const X: Extended; const Y: Extended;
           const Tag: String): Boolean;
//-----------------------------------------------------------------------------
//  Checks if object located at point X,Y and with given tag should be edited.
//-----------------------------------------------------------------------------
var
  Xp: Integer;
  Yp: Integer;

begin
  if (TagCheckBox.Checked) and (CompareText(Tag, TagEditBox.Text) <> 0)
  then Result := False

  else if MapForm.AllSelected
  then Result := True

  else
  begin
    Xp := MapForm.Map.GetXpix(X);
    Yp := MapForm.Map.GetYpix(Y);
    if not(PtInRegion(theRegion, Xp, Yp))
    then Result := False
    else Result := True;
  end;
end;

function TGroupEditForm.GetNewValue(var Value1: String; const Value2: String;
  const EditType: TEditType): Boolean;
//-----------------------------------------------------------------------------
//  Applies editing operation of type EditType with value Value2 to Value1.
//-----------------------------------------------------------------------------
var
  X1, X2, Z1: Extended;
begin
  Result := False;
  if EditType = etReplace then
  begin
    if SameText(Value1, Value2) then Exit;
    Value1 := Value2;
  end
  else
  begin
    if not Uutils.GetExtended(Value1, X1) then Exit;
    if not Uutils.GetExtended(Value2, X2) then Exit;
    if EditType = etMultiply then X1 := X2*X1;
    if EditType = etAdd then X1 := X1 + X2;
    Z1 := Abs(X1);
    if      Z1 < 0.01 then Value1 := Format('%0.6f', [X1])
    else if Z1 < 1.0  then Value1 := Format('%0.4f', [X1])
    else if Z1 < 10.0 then Value1 := Format('%.3f', [X1])
    else                   Value1 := Format('%.2f', [X1]);
  end;
  Result := True;
end;


procedure TGroupEditForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 211300);
end;

procedure TGroupEditForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then HelpBtnClick(Sender);
end;

end.
