unit Dlanduse;

{-------------------------------------------------------------------}
{                    Unit:    Dlanduse.pas                          }
{                    Project: EPASWMM                               }
{                    Version: 5.2                                   }
{                    Date:    11/01/21     (5.2.0)                  }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form unit that edits properties of a  Landuse object.    }
{                                                                   }
{   The form consists of a TabControl with a PropEdit component     }
{   placed on it. Separate tabs are used for General properties,    }
{   pollutant Buildup properties, and pollutant Washoff properties. }
{   Each tab has an associated TPropRecord that is placed into the  }
{   PropEdit component when the tab is selected. The actual general }
{   properties of the Landuse object are stored in a stringlist     }
{   (LanduseList) while the buildup and washoff properties are      }
{   placed into invisible string grids (BuildupGrid and             }
{   WashoffGrid, respectively).                                     }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, ComCtrls, StdCtrls, Uproject, Uglobals, NumEdit, ExtCtrls, PropEdit,
  Uutils;
type
  TLanduseForm = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    BuildupGrid: TStringGrid;
    WashoffGrid: TStringGrid;
    TabControl1: TTabControl;
    PollutPanel: TPanel;
    PollutCombo: TComboBox;
    HintPanel: TPanel;
    HintLabel: TLabel;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure PollutComboClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    LanduseIndex: Integer;             // Index of land use in project
    Npolluts: Integer;                 // Number of pollutants
    PollutIndex: Integer;              // Current pollut. index
    GeneralProps: TStringlist;
    OnBuildupTab: Boolean;
    BuildupProps1: array[0..4] of TPropRecord;
    BuildupProps2: array[0..4] of TPropRecord;
    WashoffProps: array[0..4] of TPropRecord;
    PropEditor: TPropEdit;
    procedure EditComment(Sender: TObject; Index: Integer;
      var S: String; var Modified: Boolean);
    procedure ShowBuildupProps(S: String);
    procedure ShowPropertyHint(Sender: TObject; aRow: LongInt);
    procedure ValidateData(Sender: TObject; Index: Integer; var S: String;
      var Errmsg: String; var IsValid: Boolean);
  public
    { Public declarations }
    HasChanged: Boolean;
    procedure SetData(const I: Integer);
    procedure GetData(const I: Integer);
  end;

//var
//  LanduseForm: TLanduseForm;

implementation

{$R *.DFM}

uses Uedit;

const
  TXT_DESCRIPTION = 'Land Use Description';
  MSG_INVALID_NAME = 'Invalid land use name.';
  MSG_NAME_EXISTS = 'A land use with that name already exists.';

  DefBuildup: array[0..4] of String =
    ('NONE', '0.0', '0.0', '0.0', 'AREA');

  DefWashoff: array[0..4] of String =
    ('EMC', '0.0', '0.0', '0.0', '0.0');

  GeneralHint: array[0..5] of String =
    ('User assigned name of land use.',
     'Optional comment or description of the land use (click to edit).',
     '',
     'Days between street sweeping within the land use (0 for no sweeping).',
     'Fraction of pollutant buildup that is available for removal by sweeping.',
     'Number of days since land use was last swept at the start of the simulation.');

  BuildupHint1: array[0..4] of String =
    ('Buildup function: POW = power, EXP = exponential, SAT = saturation, ' +
     'EXT = external time series.',
     'Maximum possible buildup (lbs (kg) per unit of normalizer variable).',
     'Rate constant of buildup function (lbs (kg) per normalizer per day for ' +
     'power buildup or 1/days for exponential buildup.',
     'Time exponent for power buildup or half saturation constant (days) for ' +
     'saturation buildup.',
     'Subcatchment variable to which buildup is normalized: area (acres or ' +
     'hectares) or curb length (any units).');

  BuildupHint2: array[0..4] of String =
    ('Buildup function: POW = power, EXP = exponential, SAT = saturation, ' +
     'EXT = external time series.',
     'Maximum possible buildup (lbs (kg) per unit of normalizer variable).',
     'Scaling factor used to modify loading rates by a fixed ratio.',
     'Name of Time Series containing loading rates (lbs (kg) per normalizer per day).',
     'Subcatchment variable to which buildup is normalized: area (acres or ' +
     'hectares) or curb length (any units).');

  WashoffHint: array[0..4] of String =
    ('Washoff function: EXP = exponential, RC = rating curve, EMC = event mean ' +
     'concentration.',
     'Washoff coefficient or Event Mean Concentration (EMC).',
     'Runoff exponent in washoff function.',
     'Street cleaning removal efficiency (percent) for the pollutant.',
     'Removal efficiency (percent) associated with any Best Management Practice ' +
     'utilized.');

var
  // Property records for General, Buildup and Washoff categories
  // (A TPropRecord record determines how properties are displayed
  // and edited in the PropEdit control (see PropEdit.pas unit)).
  DefGeneralProps: array[0..5] of TPropRecord =
    ((Name:'Land Use Name';   Style:esEdit;      Mask:emNoSpace;   Length:0),
     (Name:'Description';     Style:esButton;    Mask:emNone;      Length:0),
     (Name:'STREET SWEEPING'; Style:esHeading;   Mask:emNone),
     (Name:'  Interval';      Style:esEdit;      Mask:emPosNumber; Length:0),
     (Name:'  Availability';  Style:esEdit;      Mask:emPosNumber; Length:0),
     (Name:'  Last Swept';    Style:esEdit;      Mask:emPosNumber; Length:0));

  DefBuildupProps1: array[0..4] of TPropRecord =
    ((Name:'Function';        Style:esComboList; Mask:emNone;      Length:0;
      List:'NONE'#13'POW'#13'EXP'#13'SAT'#13'EXT'),
     (Name:'Max. Buildup';    Style:esEdit;      Mask:emPosNumber; Length:0),
     (Name:'Rate Constant';   Style:esEdit;      Mask:emNumber;    Length:0),
     (Name:'Power/Sat. Constant'; Style:esEdit;  Mask:emNumber;    Length:0),
     (Name:'Normalizer';      Style:esComboList; Mask:emNone;      Length:0;
      List:'AREA'#13'CURB'));

  DefBuildupProps2: array[0..4] of TPropRecord =
    ((Name:'Function';        Style:esComboList; Mask:emNone;      Length:0;
      List:'NONE'#13'POW'#13'EXP'#13'SAT'#13'EXT'),
     (Name:'Max. Buildup';    Style:esEdit;      Mask:emPosNumber; Length:0),
     (Name:'Scaling Factor';  Style:esEdit;      Mask:emPosNumber; Length:0),
     (Name:'Time Series';     Style:esComboEdit; Mask:emNone;      Length:0),
     (Name:'Normalizer';      Style:esComboList; Mask:emNone;      Length:0;
      List:'AREA'#13'CURB'));

  DefWashoffProps: array[0..4] of TPropRecord =
    ((Name:'Function';        Style:esComboList; Mask:emNone;      Length:0;
      List:'NONE'#13'EXP'#13'RC'#13'EMC'),
     (Name:'Coefficient';     Style:esEdit;      Mask:emPosNumber; Length:0),
     (Name:'Exponent';        Style:esEdit;      Mask:emNumber;    Length:0),
     (Name:'Cleaning Effic.'; Style:esEdit;      Mask:emNumber;    Length:0),
     (Name:'BMP Effic.';      Style:esEdit;      Mask:emNumber;    Length:0));

procedure TLanduseForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnCreate event handler.
//-----------------------------------------------------------------------------
begin
  // Create GeneralProps string list
  GeneralProps := TStringlist.Create;

  // Create Property Editor component
  PropEditor := TPropEdit.Create(self);
  with PropEditor do
  begin
    Parent := TabControl1;
    Align := alClient;
    BorderStyle := bsSingle;
    Ctl3D := False;
    ColHeading1 := 'Property';
    ColHeading2 := 'Value';
    ReadOnlyColor := clBtnFace;
    ValueColor := clNavy;
    OnButtonClick := EditComment;
    OnValidate := ValidateData;
    OnRowSelect := ShowPropertyHint;
  end;
  HasChanged := False;
end;

procedure TLanduseForm.FormDestroy(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnDestroy event handler.
//-----------------------------------------------------------------------------
begin
  GeneralProps.Free;
  PropEditor.Free;
end;

procedure TLanduseForm.SetData(const I: Integer);
//-----------------------------------------------------------------------------
//  Loads properties for Landuse I into the form
//-----------------------------------------------------------------------------
var
  C: Integer;
  J: Integer;
  R: Integer;
  S: String;
  NPS: TNonpointSource;
  NPSlist: TStringlist;
begin
  // Save landuse object index
  LanduseIndex := I;
  PollutIndex := 0;

  // Load default values into General properties list
  with GeneralProps do
  begin
    Add('');
    Add('');
    Add('');
    Add('0');
    Add('0');
    Add('0');
  end;

  // Initialize actual Property Records to default ones for buildup/washoff
  for J := 0 to High(BuildupProps1) do BuildupProps1[J] := DefBuildupProps1[J];
  for J := 0 to High(BuildupProps2) do BuildupProps2[J] := DefBuildupProps2[J];
  BuildupProps2[3].List := Project.Lists[TIMESERIES].Text;
  for J := 0 to High(WashoffProps) do WashoffProps[J] := DefWashoffProps[J];

  // Store buildup/washoff properties for each pollutant in a row of the
  // buildup/washoff string grids
  BuildupGrid.ColCount := High(DefBuildup) + 1;
  WashoffGrid.ColCount := High(DefWashoff) + 1;
  Npolluts := Project.Lists[POLLUTANT].Count;
  if Npolluts > 0 then
  begin
    BuildupGrid.RowCount := Npolluts;
    WashoffGrid.RowCount := Npolluts;
    for R := 0 to Npolluts - 1 do
    begin
      S := Project.Lists[POLLUTANT].Strings[R];
      PollutCombo.Items.Add(S);
      for C := 0 to High(DefBuildup) do
        BuildupGrid.Cells[C,R] := DefBuildup[C];
      for C := 0 to High(DefWashoff) do
        WashoffGrid.Cells[C,R] := DefWashoff[C];
    end;
    PollutCombo.ItemIndex := 0;
  end;

  // Fill in data for existing land use
  if I >= 0 then with Project.Lists[LANDUSE] do
  begin

    // Name & street cleaning data (property 2 is blank)
    GeneralProps[0] := Strings[I];
    Generalprops[1] := TLanduse(Objects[I]).Data[COMMENT_INDEX];
    GeneralProps[3] := TLanduse(Objects[I]).Data[LANDUSE_CLEANING_INDEX];
    GeneralProps[4] := TLanduse(Objects[I]).Data[LANDUSE_AVAILABLE_INDEX];
    GeneralProps[5] := TLanduse(Objects[I]).Data[LANDUSE_LASTCLEAN_INDEX];

    // Get the Buildup/Washoff data associated with the Landuse
    NPSlist := TLanduse(Objects[I]).NonpointSources;

    // Use a row of the string grids for each pollutant
    for R := 0 to Npolluts - 1 do
    begin

      // Find the position of the pollutant in the buildup/washoff lists
      S := Project.Lists[POLLUTANT].Strings[R];
      J := NPSlist.IndexOf(S);

      // Fill the string grids with the buildup/washoff properties
      if J >= 0 then
      begin
        NPS := TNonpointSource(NPSlist.Objects[J]);
        for C := 0 to MAXBUPROPS do
          BuildupGrid.Cells[C,R] := NPS.BuildupData[C];
        for C := 0 to MAXWOPROPS do
          WashoffGrid.Cells[C,R] := NPS.WashoffData[C];
      end;
    end;
  end;
end;

procedure TLanduseForm.GetData(const I: Integer);
//-----------------------------------------------------------------------------
//  Unloads data from form into properties of Landuse I
//-----------------------------------------------------------------------------
var
  J : Integer;
  R : Integer;
  S : String;
  aLanduse: TLanduse;
  aNPS: TNonpointSource;
begin
  // Get name of landuse from form
  S := Trim(GeneralProps[0]);

  // If the landuse currently doesn't exist, then create it
  if I < 0 then
  begin
    aLanduse := TLanduse.Create;
    Project.Lists[LANDUSE].AddObject(S, aLanduse);
  end

  // Otherwise retrieve it from the project database
  else
  begin
    aLanduse := TLanduse(Project.Lists[LANDUSE].Objects[I]);
    Project.Lists[LANDUSE].Strings[I] := S;
  end;

  // Copy the general properties into the landuse object
  with Project.Lists[LANDUSE] do
  begin
    aLanduse.Data[COMMENT_INDEX] := GeneralProps[1];
    aLanduse.Data[LANDUSE_CLEANING_INDEX] := GeneralProps[3];
    aLanduse.Data[LANDUSE_AVAILABLE_INDEX] := GeneralProps[4];
    aLanduse.Data[LANDUSE_LASTCLEAN_INDEX] := GeneralProps[5];
  end;

  // Unload the nonpoint source buildup/washoff data
  if Npolluts > 0 then
  begin

    // First delete the landuse's exisitng nonpoint source data
    aLanduse.ClearNonpointSources;

    // Then for each pollutant:
    for R := 0 to Npolluts - 1 do
    begin
      // Get the pollutant name and create a nonpoint source object
      with PollutCombo do S := Items[R];
      aNPS := TNonpointSource.Create;

      // Copy the buildup/washoff data from the form's string grids to the
      // new nonpoint source object
      for J := 0 to MAXBUPROPS do
        aNPS.BuildupData[J] := BuildupGrid.Cells[J,R];
      for J := 0 to MAXWOPROPS do
        aNPS.WashoffData[J] := WashoffGrid.Cells[J,R];

      // Add the new nonpoint source object to the landuse
      aLanduse.NonpointSources.AddObject(S, aNPS);
    end;
  end;
end;

procedure TLanduseForm.OKBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick event handler for the OK button.
//-----------------------------------------------------------------------------
var
  I: Integer;
  S: String;
  Err: Boolean;
begin
  // Validate the entry to the Property Editor last made by the user
  Err := False;
  PropEditor.IsValid;

  // Check that valid name supplied for the landuse
  S := Trim(GeneralProps[0]);
  if Length(S) = 0 then
  begin
    Uutils.MsgDlg(MSG_INVALID_NAME, mtError, [mbOK]);
    Err := True;
  end;
  I := Project.Lists[LANDUSE].IndexOf(S);
  if (I >= 0) then
  begin
    if (LanduseIndex < 0) or (LanduseIndex <> I) then
    begin
      Uutils.MsgDlg(MSG_NAME_EXISTS, mtError, [mbOK]);
      Err := True;
    end;
  end;
  if Err = True then
  begin
    ModalResult := mrNone;
  end;
end;

procedure TLanduseForm.FormShow(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnShow event handler.
//-----------------------------------------------------------------------------
begin
  // Begin by showing the General properties editor
  OnBuildupTab := False;
  TabControl1.TabIndex := 0;
  TabControl1Change(Sender);
end;

procedure TLanduseForm.TabControl1Change(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick event handler for the TabControl.
//  Switches the set of properties displayed in the PropEditor control.
//-----------------------------------------------------------------------------
begin
  // Handle the last entry the user made in the editor
  PropEditor.IsValid;
  PollutPanel.Visible := False;
  PollutPanel.Caption := '';
  PollutCombo.Visible := False;
  OnBuildupTab := False;

  // User selected the General properties tab
  if TabControl1.TabIndex = 0 then
  begin
    PropEditor.Visible := True;
    PropEditor.SetProps(DefGeneralProps, GeneralProps);
    PropEditor.Edit;
  end

  // User selected the Buildup or Washoff tabs
  else
  begin

    // If there are no pollutants then do not display the property editor
    PollutPanel.Visible := True;
    if Npolluts = 0 then
    begin
      PollutPanel.Caption := 'There are no pollutants for this project.';
      PropEditor.Visible := False;

      HintLabel.Caption := '';
    end

    // Otherwise display the appropriate set of properties in the editor
    else
    begin
      PollutPanel.Caption := '  Pollutant';
      PollutCombo.Visible := True;
      PropEditor.Visible := True;
      if TabControl1.TabIndex = 1 then
      begin
        showBuildupProps(BuildupGrid.Cells[0,PollutIndex]);
        OnBuildupTab := True;
      end
      else
        PropEditor.SetProps(WashoffProps, WashoffGrid.Rows[PollutIndex]);
      PropEditor.Edit;
    end;
  end;
end;

procedure TLanduseForm.ShowBuildupProps(S: String);
//-----------------------------------------------------------------------------
// Displays a different set of buildup properties on the Buildup page
// of the form depending on whether the External Time Series buildup
// option was selected or not.
//-----------------------------------------------------------------------------
begin
  if SameText(S, 'EXT') then
    PropEditor.SetProps(BuildupProps2, BuildupGrid.Rows[PollutIndex])
  else
    PropEditor.SetProps(BuildupProps1, BuildupGrid.Rows[PollutIndex]);
end;

procedure TLanduseForm.EditComment(Sender: TObject; Index: Integer;
  var S: String; var Modified: Boolean);
//-----------------------------------------------------------------------------
// OnButtonClick event for ellipsis button fields in the property editor.
// Launches a Comment Editor dialog.
//-----------------------------------------------------------------------------
begin
  if TabControl1.TabIndex = 0 then
    Uedit.EditComment(TXT_DESCRIPTION, S, Modified);
end;

procedure TLanduseForm.ValidateData(Sender: TObject; Index: Integer;
      var S: String; var Errmsg: String; var IsValid: Boolean);
//-----------------------------------------------------------------------------
//  OnValidate event handler for the PropEditor component.
//-----------------------------------------------------------------------------
begin
  HasChanged := True;
  IsValid := True;
  if OnBuildupTab and (Index = 0) then ShowBuildupProps(S);
end;

procedure TLanduseForm.PollutComboClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick event handler for the PollutCombo component.
//-----------------------------------------------------------------------------
begin
  // If currently editing buildup properties
  PropEditor.IsValid;
  PollutIndex := PollutCombo.ItemIndex;
  if (TabControl1.TabIndex = 1) then
  begin

    // Update pollutant index for buildup editing and load properties
    // for this pollutant into the editor
    ShowBuildupProps(BuildupGrid.Cells[0,PollutIndex]);
    PropEditor.Edit;
  end

  // If currently editing washoff properties and a pollutant was selected
  else if (TabControl1.TabIndex = 2) then
  begin

    // Update pollutant index for washoff editing and load properties
    // for this pollutant into the editor
    PropEditor.SetProps(WashoffProps, WashoffGrid.Rows[PollutIndex]);
    PropEditor.Edit;
  end
end;

procedure TLanduseForm.ShowPropertyHint(Sender: TObject; aRow: LongInt);
//-----------------------------------------------------------------------------
// OnRowSelect handler for the PropEdit component. Displays a context-
// sensitive hint message in the panel below the property editor.
//-----------------------------------------------------------------------------
begin
  case TabControl1.TabIndex of
  0: HintLabel.Caption := GeneralHint[aRow];
  1: begin
       if SameText(BuildupGrid.Cells[0,PollutIndex], 'EXT') then
         HintLabel.Caption := BuildupHint2[aRow]
       else
         HintLabel.Caption := BuildupHint1[aRow];
     end;
  2: HintLabel.Caption := WashoffHint[aRow];
  end;
end;

procedure TLanduseForm.HelpBtnClick(Sender: TObject);
begin
  Case TabControl1.TabIndex of
    0:  Application.HelpCommand(HELP_CONTEXT, 211650);
    1:  Application.HelpCommand(HELP_CONTEXT, 211660);
    2:  Application.HelpCommand(HELP_CONTEXT, 211670);
  end;
end;

procedure TLanduseForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then HelpBtnClick(Sender);
end;

end.
