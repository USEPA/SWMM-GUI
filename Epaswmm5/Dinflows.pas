unit Dinflows;

{-------------------------------------------------------------------}
{                    Unit:    Dinflow.pas                           }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form unit that edits the properties of user-supplied     }
{   external inflows to a drainage system node.                     }
{                                                                   }
{   The form consists of a Page Control with three pages; one for   }
{   Direct inflows, one for Dry Weather Flows, and one for RDII     }
{   flows. Invisible String Grids hold the actual flow and          }
{   pollutant inflow data for Direct and Dry Weather inflows.       }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Uproject, Uglobals, Grids, StdCtrls, NumEdit, ComCtrls, Buttons,
  Uutils, ExtCtrls;

type
  TInflowsForm = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    PageControl1: TPageControl;
    TimeSeriesPage: TTabSheet;
    DryWeatherPage: TTabSheet;
    RDIIPage: TTabSheet;
    HintPanel: TPanel;
    HintLabel: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label6: TLabel;
    DxParamCombo: TComboBox;
    DxSeriesCombo: TComboBox;
    DxTypeCombo: TComboBox;
    DxCFactorEdit: TNumEdit;
    DxInflowDataGrid: TStringGrid;
    TseriesBtn1: TBitBtn;
    TseriesBtn2: TBitBtn;
    DxSFactorEdit: TNumEdit;
    DxBaseEdit: TNumEdit;
    DxBaseDelBtn: TBitBtn;
    DxPatCombo: TComboBox;
    DxPatBtn1: TBitBtn;
    DxPatBtn2: TBitBtn;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    DwUnitsLabel: TLabel;
    Label7: TLabel;
    Label18: TLabel;
    DwParamCombo: TComboBox;
    DwAvgEdit: TNumEdit;
    DwInflowDataGrid: TStringGrid;
    DwPatCombo1: TComboBox;
    DwPatCombo2: TComboBox;
    DwPatCombo3: TComboBox;
    DwPatCombo4: TComboBox;
    PatternBtn1: TBitBtn;
    PatternBtn2: TBitBtn;
    PatternBtn3: TBitBtn;
    PatternBtn4: TBitBtn;
    PatternBtn5: TBitBtn;
    PatternBtn6: TBitBtn;
    PatternBtn7: TBitBtn;
    PatternBtn8: TBitBtn;
    DwAvgDelBtn: TBitBtn;
    Label11: TLabel;
    AreaUnitsLabel: TLabel;
    UHGroupCombo: TComboBox;
    SewerAreaEdit: TNumEdit;
    UHBtn: TBitBtn;
    UHDelBtn: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DxParamComboChange(Sender: TObject);
    procedure DxSeriesComboChange(Sender: TObject);
    procedure DxSeriesComboDblClick(Sender: TObject);
    procedure DxBaseDelBtnClick(Sender: TObject);
    procedure TseriesBtn1Click(Sender: TObject);
    procedure TseriesBtn2Click(Sender: TObject);
    procedure DwParamComboChange(Sender: TObject);
    procedure DwChange(Sender: TObject);
    procedure DwPatCombo1DblClick(Sender: TObject);
    procedure DwAvgDelBtnClick(Sender: TObject);
    procedure PatternBtnDelClick(Sender: TObject);
    procedure RDIIChange(Sender: TObject);
    procedure UHGroupComboDblClick(Sender: TObject);
    procedure UHBtnClick(Sender: TObject);
    procedure UHDelBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure DxPatComboDblClick(Sender: TObject);
    procedure DxPatBtn2Click(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

   private
    { Private declarations }
    DxParamIndex: Integer;
    DwParamIndex: Integer;
    procedure SetBitBtnGlyphs;
    procedure SetDefaultDwConcen;
    procedure UpdateDxInflowDataGrid;
    procedure UpdateDxInflowPage;
    procedure UpdateDwInflowDataGrid;
    procedure UpdateDwInflowPage;
  public
    { Public declarations }
    HasChanged: Boolean;
    procedure SetData(const NodeType: Integer; const NodeIndex: Integer);
    procedure GetData(const NodeType: Integer; const NodeIndex: Integer);
  end;

//var
//  InflowEditor: TInflowEditor;

implementation

{$R *.dfm}

uses
  Fmain, Uedit;

const
  TXT_NODE_INFLOWS = 'Inflows for Node ';
  TXT_FLOW = 'FLOW';
  TXT_DIRECT = 'If Baseline or Time Series is left blank ' +
               'its value is 0. If Baseline Pattern is left ' +
               'blank its value is 1.0.';
  TXT_DRY_WEATHER = 'If Average Value is left blank its value is 0. ' +
                    'Any Time Pattern left blank defaults to a ' +
                    'constant value of 1.0.';
  TXT_RDII = 'Leave the Unit Hydrograph Group field blank to remove ' +
             'any RDII inflow at this node.';

procedure TInflowsForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnCreate handler.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  // Set hint text
  HintLabel.Caption := TXT_DIRECT;

  // Make data grids invisible
  DxInflowDataGrid.Visible := False;
  DwInflowDataGrid.Visible := False;
  HasChanged := False;

  // Assign items to combo boxes
  DxSeriesCombo.Items := Project.Lists[TIMESERIES];
  for I := 1 to 4 do
  begin
    with FindComponent('DwPatCombo' + IntToStr(I)) as TComboBox do
      Items := Project.Lists[PATTERN];
  end;
  UHGroupCombo.Items := Project.Lists[HYDROGRAPH];

  if UnitSystem = usSI
  then AreaUnitsLabel.Caption := 'Sewershed Area (hectares)'
  else AreaUnitsLabel.Caption := 'Sewershed Area (acres)';

  DxBaseEdit.Style := esNumber;
  DwAvgEdit.Style := esNumber;
  SetBitBtnGlyphs;
end;

procedure TInflowsForm.SetBitBtnGlyphs;
begin
  with MainForm.ProjectImageList do
  begin
    GetBitmap(GetIndexByName('delete'), DxBaseDelBtn.Glyph);
    GetBitmap(GetIndexByName('edit'), DxPatBtn1.Glyph);
    GetBitmap(GetIndexByName('delete'), DxPatBtn2.Glyph);
    GetBitmap(GetIndexByName('edit'), TseriesBtn1.Glyph);
    GetBitmap(GetIndexByName('delete'), TseriesBtn2.Glyph);
    GetBitmap(GetIndexByName('delete'), DwAvgDelBtn.Glyph);
    GetBitmap(GetIndexByName('edit'), UHBtn.Glyph);
    GetBitmap(GetIndexByName('delete'), UHDelBtn.Glyph);
    GetBitmap(GetIndexByName('edit'), PatternBtn1.Glyph);
    GetBitmap(GetIndexByName('edit'), PatternBtn2.Glyph);
    GetBitmap(GetIndexByName('edit'), PatternBtn3.Glyph);
    GetBitmap(GetIndexByName('edit'), PatternBtn4.Glyph);
    GetBitmap(GetIndexByName('delete'), PatternBtn5.Glyph);
    GetBitmap(GetIndexByName('delete'), PatternBtn6.Glyph);
    GetBitmap(GetIndexByName('delete'), PatternBtn7.Glyph);
    GetBitmap(GetIndexByName('delete'), PatternBtn8.Glyph);
  end;

end;

procedure TInflowsForm.FormShow(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnShow handler.
//-----------------------------------------------------------------------------
begin

  // Start with Flow as inflow parameter being edited
  DxParamIndex := 0;
  DwParamIndex := 0;

  // Update page displays
  UpdateDxInflowPage;
  UpdateDwInflowPage;

  // Make the Constant Inflow page the active page
  PageControl1.ActivePageIndex := 0;
end;

procedure TInflowsForm.DxParamComboChange(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnChange handler for the DxParamCombo box. Changes the parameter
//  whose Direct Inflow is being edited.
//-----------------------------------------------------------------------------
begin
  // Save data on the Time Series page to its hidden data grid
  UpdateDxInflowDataGrid;

  // Change the Time Series Inflow parameter being edited
  DxParamIndex := DxParamCombo.ItemIndex;
  UpdateDxInflowPage;
end;

procedure TInflowsForm.DxSeriesComboChange(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnChange handler for the DxSeriesCombo box.
//-----------------------------------------------------------------------------
begin
  HasChanged := True;
end;

procedure TInflowsForm.DWChange(Sender: TObject);
//-----------------------------------------------------------------------------
// OnChange handler for all Time Pattern combo boxes.
//-----------------------------------------------------------------------------
begin
  HasChanged := True;
end;

procedure TInflowsForm.DwParamComboChange(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnChange handler for the DwParamCombo box. Changes the parameter whose
//  dry weather inflow is being edited.
//-----------------------------------------------------------------------------
begin
  // Save data on DW Inflow page to its hidden data grid
  UpdateDwInflowDataGrid;

  // Change DW Inflow parameter
  DwParamIndex := DwParamCombo.ItemIndex;
  UpdateDwInflowPage;
end;

procedure TInflowsForm.DxSeriesComboDblClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnDblClick handler for the Direct Inflow time series combo box.
//  Launches the Time Series Editor for the specified time series.
//-----------------------------------------------------------------------------
var
  I: Integer;
  S: String;
begin
  with Sender as TComboBox do
  begin

    // Extract name of time series from combo box & launch Editor
    I := Project.Lists[TIMESERIES].IndexOf(Trim(Text));
    S := Uedit.EditTimeseries(I);

    // Update name of time series in the combo box
    if Length(S) > 0 then
    begin
      Text := S;
      DxSeriesCombo.Items := Project.Lists[TIMESERIES];
    end;
  end;
end;

procedure TInflowsForm.DxBaseDelBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the Direct Inflow baseline value remove button.
//-----------------------------------------------------------------------------
begin
  DxBaseEdit.Text := '';
end;

procedure TInflowsForm.DxPatComboDblClick(Sender: TObject);
var
  I: Integer;
  S: String;
begin
  with DxPatCombo do
  begin
    I := Project.Lists[PATTERN].IndexOf(Trim(Text));
    S := Uedit.EditPattern(I);
    if Length(S) > 0 then Text := S;
  end;
end;

procedure TInflowsForm.DxPatBtn2Click(Sender: TObject);
begin
  DxPatCombo.Text := '';
end;

procedure TInflowsForm.TseriesBtn1Click(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the Direct Inflow time series edit button.
//-----------------------------------------------------------------------------
begin
  DxSeriesComboDblClick(DxSeriesCombo);
end;

procedure TInflowsForm.TseriesBtn2Click(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the Direct Inflow time series remove button.
//-----------------------------------------------------------------------------
begin
  DxSeriesCombo.Text := '';
end;

procedure TInflowsForm.DwAvgDelBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the Average Dry Weather remove button.
//-----------------------------------------------------------------------------
begin
  DwAvgEdit.Text := '';
end;

procedure TInflowsForm.PageControl1Change(Sender: TObject);
begin
  case PageControl1.ActivePageIndex of
  0: HintLabel.Caption := TXT_DIRECT;
  1: HintLabel.Caption := TXT_DRY_WEATHER;
  2: HintLabel.Caption := TXT_RDII;
  end;
end;

procedure TInflowsForm.PatternBtnDelClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the Dry Weather Pattern remove buttons.
//-----------------------------------------------------------------------------
var
  N: Integer;
begin
  if Sender is TBitBtn then with Sender as TBitBtn do N := Tag
  else exit;
  with FindComponent('DwPatCombo' + IntToStr(N)) as TComboBox do
    Text := '';
end;

procedure TInflowsForm.UHDelBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the RDII Unit Hydrographs remove button.
//-----------------------------------------------------------------------------
begin
  UHGroupCombo.Text := '';
end;

procedure TInflowsForm.DwPatCombo1DblClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnDblClick handler for all of the Dry Weather Pattern combo boxes and
//  OnClick handler for their corresponding edit buttons.
//  Launches the Time Pattern Editor for the specified pattern.
//
//  Note: each of the Pattern combo boxes and buttons identifies its index
//        (1 to 4) by its Tag property.
//-----------------------------------------------------------------------------
var
  N: Integer;
  I: Integer;
  S: String;
begin
  // Identify which combo box or button was clicked from its Tag property
  if Sender is TComboBox then with Sender as TComboBox do N := Tag
  else if Sender is TBitBtn then with Sender as TBitBtn do N := Tag
  else exit;

  // Identify the name of the corresponding combo box
  with FindComponent('DwPatCombo' + IntToStr(N)) as TComboBox do
  begin

    // Edit the pattern named in the combo box
    I := Project.Lists[PATTERN].IndexOf(Trim(Text));
    S := Uedit.EditPattern(I);

    // Update the name of the pattern in the combo box
    if Length(S) > 0 then Text := S;
  end;
end;

procedure TInflowsForm.UHGroupComboDblClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnDblClick handler for the RDII Unit Hydrograph Group combo box.
//  Launches the Unit Hydrograph Editor for the specified unit
//  hydrograph group.
//-----------------------------------------------------------------------------
var
  I: Integer;
  S: String;
begin
  with Sender as TComboBox do
  begin
    I := Project.Lists[HYDROGRAPH].IndexOf(Trim(Text));
    S := Uedit.EditHydrograph(I);
    if Length(S) > 0 then
    begin
      Text := S;
      UHGroupCombo.Items := Project.Lists[HYDROGRAPH];
    end;
  end;
end;

procedure TInflowsForm.UHBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the RDII Unit Hydrograph Group edit button.
//-----------------------------------------------------------------------------
begin
  UHGroupComboDblClick(UHGroupCombo);
end;

procedure TInflowsForm.OKBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the OK button.
//-----------------------------------------------------------------------------
begin
  // Save current set of inflow edits to their hidden data grids
  UpdateDxInflowDataGrid;
  UpdateDwInflowDataGrid;
end;

procedure TInflowsForm.SetData(const NodeType: Integer;
  const NodeIndex: Integer);
//-----------------------------------------------------------------------------
//  Loads a node's current external inflow data into the form.
//-----------------------------------------------------------------------------
var
  aNode: TNode;
  I, J : Integer;
  S    : String;
begin
  // Get a reference to the node being edited
  if NodeIndex < 0 then
  begin
    aNode := nil;
    Caption := 'Group Inflows Editor';
  end
  else
  begin
    aNode := Project.GetNode(NodeType, NodeIndex);
    Caption := TXT_NODE_INFLOWS + aNode.ID;
  end;

  // Copy the node's Direct inflow data to the hidden data grid
  with DxInflowDataGrid do
  begin
    // Rows are for Inflow properties:
    //   0 - Constituent Name
    //   1 - Time Series Scaling Factor
    //   2 - Inflow Type
    //   3 - Mass Units Conversion Factor
    //   4 - Inflow Time Series Name
    //   5 - Baseline Inflow Value
    //   6 - Baseline Time Pattern
    RowCount := 7;

    // Columns are for Flow + pollutants
    ColCount := Project.Lists[POLLUTANT].Count + 1;

    // Add constituent names to row 0
    Cells[0, 0] := TXT_FLOW;
    for I := 0 to Project.Lists[POLLUTANT].Count-1 do
      Cells[I+1,0] := Project.Lists[POLLUTANT].Strings[I];

    // Add current Inflow properties for each constituent.
    // Inflow properties are stored in the node's DxInflow
    // string list in Name=Value format, where Name is
    // a constituent name and Value is the list of
    // properties separated by carriage returns.
    for I := 0 to ColCount-1 do
    begin
      S := Cells[I,0];
      if aNode <> nil then J := aNode.DxInflow.IndexOfName(S) else J := -1;
      if J >= 0 then
      begin
        S := S + #13 + aNode.DxInflow.ValueFromIndex[J];
        Cols[I].SetText(PChar(S));
      end;
    end;

    // Add default entries to Inflow Type & Conversion Factor
    // cells of the data grid in case no values were provided
    for I := 0 to ColCount-1 do
    begin
      if Length(Trim(Cells[I,2])) = 0 then  // Constituent Type
      begin
        if I = 0 then Cells[I,2] := 'FLOW'
        else Cells[I,2] := 'CONCEN';
      end;
      if Length(Trim(Cells[I,3])) = 0       // Unit Conversion Factor
      then Cells[I,3] := '1.0';
      if Length(Trim(Cells[I,4])) = 0       // Scale Factor
      then Cells[I,4] := '1.0';
    end;
  end;

  // Populate the Parameter combo box on the Direct Inflow page
  DxParamCombo.Items := DxInflowDataGrid.Rows[0];
  DxParamCombo.ItemIndex := 0;
  DxPatCombo.Items := Project.Lists[PATTERN];

  // Copy the node's Dry Weather inflow property data to the hidden
  // Dry Weather data grid
  with DwInflowDataGrid do
  begin

    // Rows are for DW Inflow properties, columns for Flow + pollutants
    RowCount := 6;
    ColCount := Project.Lists[POLLUTANT].Count + 1;

    // Add constituent names to row 0
    Cells[0, 0] := TXT_FLOW;
    for I := 0 to Project.Lists[POLLUTANT].Count-1 do
      Cells[I+1,0] := Project.Lists[POLLUTANT].Strings[I];

    // Add current DW Inflow properties for each constituent.
    // Inflow properties are stored in the node's DWInflow
    // string list in Name=Value format, where Name is
    // a constituent name and Value consists of average inflow
    // value plus the names of up to 4 time patterns.
    for I := 0 to ColCount-1 do
    begin
      S := Cells[I,0];
      if aNode <> nil then J := aNode.DWInflow.IndexOfName(S) else J := -1;
      if J >= 0 then
      begin
        S := S + #13 + aNode.DWInflow.ValueFromIndex[J];
        Cols[I].SetText(PChar(S));
      end;
    end;
    SetDefaultDwConcen;

    // Populate the Parameter combo box on the Dry Weather Inflow page
    DwParamCombo.Items := DwInflowDataGrid.Rows[0];
    DwParamCombo.ItemIndex := 0;
  end;

  // Add node's RDII inflow properties to RDII page
  if (aNode <> nil) and (aNode.IIInflow.Count >= 2) then
  begin
    UHGroupCombo.Text := aNode.IIInflow[0];
    SewerAreaEdit.Text := aNode.IIInflow[1];
  end;
end;

procedure TInflowsForm.SetDefaultDwConcen;
//--------------------------------------------------------------------
//  Places default DWF pollutant concentration in DW Inflows data grid
//--------------------------------------------------------------------
var
  I: Integer;
  C: Single;
  S: String;
  Pollut: TPollutant;
begin
  with DwInflowDataGrid do
  begin
    for I := 1 to ColCount-1 do
    begin
      S := Cells[I,1];
      if Uutils.GetSingle(S, C) then continue;
      Pollut := TPollutant(Project.Lists[POLLUTANT].Objects[I-1]);
      S := Pollut.Data[POLLUT_DWF_INDEX];
      if not Uutils.GetSingle(S, C) then S := ''
      else if C = 0.0 then S := '';
      Cells[I,1] := S;
    end;
  end;
end;

procedure TInflowsForm.GetData(const NodeType: Integer;
  const NodeIndex: Integer);
//-----------------------------------------------------------------------------
//  Unloads the form's data into a specified node's external inflow data.
//-----------------------------------------------------------------------------
var
  aNode: TNode;
  I, J : Integer;
  S    : String;

begin
  // Get a reference to the node being edited
  aNode := Project.GetNode(NodeType, NodeIndex);

  // Copy contents of hidden Direct Inflow data grid to the node's
  // Direct Inflow property (stored in a string list in 'Name=Value' format)
  aNode.DxInflow.Clear;
  with DxInflowDataGrid do
  begin
    for I := 0 to ColCount-1 do
    begin
      if (Length(Trim(Cells[I,1])) = 0) and
         (Length(Trim(Cells[I,5])) = 0) then continue;
      S := Cells[I,0] + '=' + Trim(Cells[I,1]);
      for J := 2 to RowCount-1 do S := S + #13 + Trim(Cells[I,J]);
      aNode.DxInflow.Add(S);
    end;
  end;

  // Copy contents of hidden Dry Weather Inflow data grid to the node's
  // Dry Weather Inflow property (also stored in 'Name=Value' format)
  aNode.DWInflow.Clear;
  with DwInflowDataGrid do
  begin
    for I := 0 to ColCount-1 do
    begin
      if Length(Trim(Cells[I,1])) = 0 then continue;
      S := Cells[I,0] + '=' + Trim(Cells[I,1]);
      for J := 2 to RowCount-1 do S := S + #13 + Trim(Cells[I,J]);
      aNode.DWInflow.Add(S);
    end;
  end;

  // Copy data on RDII page to node's RDII Inflow properties
  aNode.IIInflow.Clear;
  S := Trim(UHGroupCombo.Text);
  if Length(S) > 0 then with aNode.IIInflow do
  begin
    Add(S);
    Add(SewerAreaEdit.Text);
 end;
end;

procedure TInflowsForm.UpdateDxInflowDataGrid;
//-----------------------------------------------------------------------------
//  Updates contents of hidden Direct External data grid with values
//  from visible components on the Direct External Inflow page.
//-----------------------------------------------------------------------------
begin
  with DxInflowDataGrid do
  begin
    Cells[DxParamIndex,5] := DxBaseEdit.Text;         //Baseline
    if Length(Trim(DxBaseEdit.Text)) > 0              //Baseline Pattern
    then Cells[DxParamIndex,6] := DxPatCombo.Text
    else Cells[DxParamIndex,6] := '';

    Cells[DxParamIndex,1] := DxSeriesCombo.Text;      //Time Series
    Cells[DxParamIndex,4] := DxSFactorEdit.Text;      //Scaling Factor
    if DxParamIndex > 0 then
    begin
      Cells[DxParamIndex,2] := DxTypeCombo.Text;      //Inflow Type
      Cells[DxParamIndex,3] := DxCFactorEdit.Text;    //Units Conv. Factor
    end;
  end;
end;

procedure TInflowsForm.UpdateDxInflowPage;
//-----------------------------------------------------------------------------
//  Updates components on the Direct External Inflow page when a new
//  constituent is selected.
//-----------------------------------------------------------------------------
var
  IsVisible: Boolean;
  ChangedFlag: Boolean;
  J: Integer;

begin
  // Save HasChanged value since we will be updating contents of some controls
  ChangedFlag := HasChanged;

  // Show the Parameter Type and Conversion Factor data fields for
  // pollutant parameters (i.e., not for Flow)
  IsVisible := (DxParamIndex > 0);
  DxTypeCombo.Visible := IsVisible;
  DxCFactorEdit.Visible := IsVisible;
  Label3.Visible := IsVisible;
  Label4.Visible := IsVisible;

  // Copy values for the newly selected constituent from the hidden
  // data grid to the page's visible controls
  with DxInflowDataGrid do
  begin
    DxBaseEdit.Text := Cells[DxParamIndex,5];         //Baseline
    DxPatCombo.Text := Cells[DxParamIndex,6];         //Baseline Pattern
    DxSeriesCombo.Text := Cells[DxParamIndex,1];      //Time Series
    DxSfactorEdit.Text := Cells[DxParamIndex,4];      //Scale Factor
    if IsVisible then
    begin
      J := DxTypeCombo.Items.IndexOf(Cells[DxParamIndex,2]);
      if J < 0 then J := 0;
      DxTypeCombo.ItemIndex := J;
      DxCFactorEdit.Text := Cells[DxParamIndex,3];
    end;
  end;

  // Re-set HasChanged to original value
  HasChanged := ChangedFlag;
end;

procedure TInflowsForm.UpdateDwInflowDataGrid;
//-----------------------------------------------------------------------------
//  Updates contents of hidden Dry Weather Inflow data grid with values
//  from visible components on the Dry Weather Inflow page.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  DwInflowDataGrid.Cells[DwParamIndex, 1] := DwAvgEdit.Text;
  for I := 1 to 4 do
  begin
    with FindComponent('DwPatCombo' + IntToStr(I)) as TComboBox do
    begin
      DwInflowDataGrid.Cells[DwParamIndex, I+1] := Text;
    end;
  end;
end;

procedure TInflowsForm.UpdateDwInflowPage;
//-----------------------------------------------------------------------------
//  Updates components on the Dry Weather Inflow page when a new constituent
//  is selected.
//-----------------------------------------------------------------------------
var
  I: Integer;
  ChangedFlag: Boolean;
begin
  // Save HasChanged value since we will be updating contents of some controls
  ChangedFlag := HasChanged;

  // Copy values for selected constituent from the hidden data grid
  // to the page's visible controls
  DwAvgEdit.Text := DwInflowDataGrid.Cells[DwParamIndex, 1];
  for I := 1 to 4 do
  begin
    with FindComponent('DwPatCombo' + IntToStr(I)) as TComboBox do
    begin
        Text := DwInflowDataGrid.Cells[DwParamIndex, I+1];
    end;
  end;

  // Set text for DWF units
  if DwParamIndex = 0 then
    DWUnitsLabel.Caption := '(' + Project.Options.Data[FLOW_UNITS_INDEX] + ')'
  else with Project.Lists[POLLUTANT].Objects[DwParamIndex-1] as TPollutant do
    DWUnitsLabel.Caption := '(' + Data[POLLUT_UNITS_INDEX] + ')';

  // Re-set HasChanged to original value
  HasChanged := ChangedFlag;
end;

procedure TInflowsForm.RDIIChange(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnChange handler for components on the RDII page.
//-----------------------------------------------------------------------------
begin
  HasChanged := True;
end;

procedure TInflowsForm.HelpBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
begin
  case PageControl1.ActivePageIndex of
  0: Application.HelpCommand(HELP_CONTEXT, 211290);
  1: Application.HelpCommand(HELP_CONTEXT, 211270);
  2: Application.HelpCommand(HELP_CONTEXT, 212250);
  end;
end;

procedure TInflowsForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then HelpBtnClick(Sender);
end;

end.
