unit Dstats;

{-------------------------------------------------------------------}
{                    Unit:    Dstats.pas                            }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Stay-on-top dialog form used to specify statistical analysis    }
{   options for a variable at a given location. Works with the      }
{   TStatsSelection data structure defined in the Ustats unit       }
{   and launches the TStatsReportForm defined in the Fstats unit.   }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, NumEdit, StdCtrls, Buttons, Uglobals, Uproject,
  Ustats, Uutils;

type
  TStatsSelectForm = class(TForm)
    Label103: TLabel;
    Label104: TLabel;
    Label102: TLabel;
    Label101: TLabel;
    Label105: TLabel;

    ObjectTypeCombo: TComboBox;
    ObjectIDEdit: TEdit;
    VariableCombo: TComboBox;
    TimePeriodCombo: TComboBox;
    StatsTypeCombo: TComboBox;

    BtnOK: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    ObjectIDBtn: TBitBtn;
    GroupBox1: TGroupBox;
    Label109: TLabel;
    MinValueEdit: TNumEdit;
    Label110: TLabel;
    MinVolEdit: TNumEdit;
    Label111: TLabel;
    MinDeltaEdit: TNumEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BtnOKClick(Sender: TObject);
    procedure ObjectTypeComboClick(Sender: TObject);
    procedure VariableComboClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure ObjectIDBtnClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure TimePeriodComboClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    StatsTypeIndex: Integer;
    function  GetStatsSelection(var Stats: TStatsSelection): Boolean;
    procedure GetVariableTypes(var Stats: TStatsSelection);
  public
    { Public declarations }
  end;

var
  StatsSelectForm: TStatsSelectForm;

implementation

{$R *.dfm}

uses
  Fmain, Fstats, Ubrowser, Uoutput;

const
  TXT_NO_AREA_SELECTED = 'Must select a subcatchment.';
  TXT_NO_NODE_SELECTED = 'Must select a node.';
  TXT_NO_LINK_SELECTED = 'Must select a link.';
  TXT_NO_OBJECT_SELECTED = 'No object was selected.';
  TXT_INVALID_DATES = 'End date comes before start date.';
  TXT_INVALID_PLOT_PARAM = 'Plotting parameter must be between 0 and 1.';

  BASICSTATS = 0;
  FLOWSTATS  = 1;
  QUALSTATS  = 2;

  StatsTypeText: array[0..2] of PChar =
    ('Mean'#13'Peak',
     'Mean'#13'Peak'#13'Total'#13'Duration'#13'Inter-Event Time',
     'Mean Concen.'#13'Peak Concen.'#13'Mean Load'#13'Peak Load'#13'Total Load');


procedure TStatsSelectForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnCreate handler.
//-----------------------------------------------------------------------------
begin
  // Place current type of object in the Object Type combo box
  if Project.IsSubcatch(CurrentList)
  then ObjectTypeCombo.ItemIndex := SUBCATCH
  else if Project.IsNode(CurrentList)
  then ObjectTypeCombo.ItemIndex := NODES
  else if Project.IsLink(CurrentList)
  then ObjectTypeCombo.ItemIndex := LINKS
  else ObjectTypeCombo.ItemIndex := 0;
  ObjectTypeComboClick(ObjectTypeCombo);
  TimePeriodComboClick(Sender);
  with MainForm.ProjectImageList do
    GetBitmap(GetIndexByName('blue_plus'), ObjectIDBtn.Glyph);
end;


procedure TStatsSelectForm.FormClose(Sender: TObject; var Action: TCloseAction);
//-----------------------------------------------------------------------------
//  OnClose handler.
//-----------------------------------------------------------------------------
begin
  Action := caFree;
end;


procedure TStatsSelectForm.BtnOKClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the OK button.
//-----------------------------------------------------------------------------
var
  Stats: TStatsSelection;
  StatsReportForm: TStatsReportForm;
begin
  // Place form's selections into a StatsSelection data structure
  if GetStatsSelection(Stats) then
  begin

    // Create a statistical report
    Hide;
    StatsReportForm := TStatsReportForm.Create(MainForm);
    with StatsReportForm do
    try
      if CreateReport(Stats) then
      begin
        Application.ProcessMessages;
        RefreshReport;
      end
      else Close;
    finally
    end;
  end;
end;


procedure TStatsSelectForm.ObjectTypeComboClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the Object Type combo box. Loads the proper set of
//  variables into the Variable combo box when a new category of object
//  is selected.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  with Sender as TComboBox do
  begin
    ObjectIDEdit.Enabled := True;
    ObjectIDBtn.Visible := True;
    Label102.Enabled := True;
    VariableCombo.Clear;
    Case ItemIndex of

    // Subcatchments were selected
    SUBCATCHMENTS:
      begin
        // Load subcatchment variables into the Variable combo box
        for I := SUBCATCHOUTVAR1 to MainForm.SubcatchViewBox.Items.Count-1 do
          VariableCombo.Items.Add(MainForm.SubcatchViewBox.Items[I]);
        VariableCombo.ItemIndex := 0;

        // Place ID of currently selected subcatchment into Object edit box
        if Project.IsSubcatch(CurrentList) then with Project do
          ObjectIDEdit.Text := GetID(CurrentList, CurrentItem[CurrentList])
        else ObjectIDEdit.Clear;
      end;

    // Nodes were selected
    NODES:
      begin
        // Load node variables into the Variable combo box
        for I := NODEOUTVAR1 to MainForm.NodeViewBox.Items.Count-1 do
          VariableCombo.Items.Add(MainForm.NodeViewBox.Items[I]);
        VariableCombo.ItemIndex := 0;

        // Place ID of currently selected node into Object edit box
        if Project.IsNode(CurrentList) then with Project do
          ObjectIDEdit.Text := GetID(CurrentList, CurrentItem[CurrentList])
        else ObjectIDEdit.Clear;
      end;

    // Links were selected
    LINKS:
      begin
        // Load link variables into the Variable combo box
        for I := LINKOUTVAR1 to MainForm.LinkViewBox.Items.Count-1 do
          VariableCombo.Items.Add(MainForm.LinkViewBox.Items[I]);
        VariableCombo.ItemIndex := 0;

        // Place ID of currently selected link into Object edit box
        if Project.IsLink(CurrentList) then with Project do
          ObjectIDEdit.Text := GetID(CurrentList, CurrentItem[CurrentList])
        else ObjectIDEdit.Clear;
      end;

    // System was selected
    SYS:
      begin
        // Load system variables into the Variable combo box
        for I := 0 to Uglobals.NsysViews-1 do
          VariableCombo.Items.Add(Uglobals.SysViewNames[I]);
        VariableCombo.ItemIndex := 0;

        // Disable the ID editing controls
        ObjectIDEdit.Clear;
        ObjectIDEdit.Enabled := False;
        ObjectIDBtn.Visible := False;
        Label102.Enabled := False;
      end;
    end;
  end;

  // Activate response to a change in variable
  VariableComboClick(Sender);
end;


procedure TStatsSelectForm.VariableComboClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the Variable combo box. Adjusts the type of
//  statistic that can be selected from the StatsType combo box when
//  a new choice of variable is made.
//-----------------------------------------------------------------------------
var
  K: Integer;
  EnabledFlag: Boolean;
begin
  // Update choices in the StatsTypeCombo box
  EnabledFlag := True;
  with StatsTypeCombo do
  begin
    K := ItemIndex;
    case ObjectTypeCombo.ItemIndex of

    SUBCATCHMENTS:
      if SUBCATCHOUTVAR1 + VariableCombo.ItemIndex >= SUBCATCHQUAL
      then StatsTypeIndex := QUALSTATS
      else StatsTypeIndex := FLOWSTATS;

    NODES:
      begin
        EnabledFlag := False;
        if NODEOUTVAR1 + VariableCombo.ItemIndex in [NODEDEPTH, HEAD]
        then StatsTypeIndex := BASICSTATS
        else if NODEOUTVAR1 + VariableCombo.ItemIndex >= NODEQUAL
        then StatsTypeIndex := BASICSTATS
        else
        begin
          StatsTypeIndex := FLOWSTATS;
          EnabledFlag := True;
        end;
      end;

    LINKS:
      if LINKOUTVAR1 + VariableCombo.ItemIndex >= LINKQUAL
      then StatsTypeIndex := QUALSTATS
      else if LINKOUTVAR1 + VariableCombo.ItemIndex = FLOW
      then StatsTypeIndex := FLOWSTATS
      else StatsTypeIndex := BASICSTATS;

    SYS:
      if VariableCombo.ItemIndex in [SYS_SNOWDEPTH, SYS_EVAP] then
        StatsTypeIndex := BASICSTATS
      else
        StatsTypeIndex := FLOWSTATS;
    end;
    Items.SetText(StatsTypeText[StatsTypeIndex]);
    if (K < 0) or (K >= Items.Count) then K := 0;
    ItemIndex := K;
  end;
  Label109.Caption := VariableCombo.Text;
  Label110.Enabled := EnabledFlag;
  MinVolEdit.Enabled := EnabledFlag;
end;


procedure TStatsSelectForm.TimePeriodComboClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the Time Period combo box. Enables the Inter-Event
//  hours edit box (MinDeltaEdit) only if event-based statistics are
//  selected.
//-----------------------------------------------------------------------------
var
  EnabledFlag : Boolean;
begin
  EnabledFlag := (TimePeriodCombo.ItemIndex = 0);
  Label111.Enabled := EnabledFlag;
  MinDeltaEdit.Enabled := EnabledFlag;
end;


procedure TStatsSelectForm.BtnCancelClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the Cancel button.
//-----------------------------------------------------------------------------
begin
  Hide;
end;


procedure TStatsSelectForm.ObjectIDBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the Object ID button. Loads the ID of the currently
//  selected object from the Data Browser into the Object ID edit box.
//-----------------------------------------------------------------------------
begin
  // Check that proper type of object is selected in the Browser
  case ObjectTypeCombo.ItemIndex of
  SUBCATCHMENTS: if not Project.IsSubcatch(CurrentList) then
            begin
             Uutils.MsgDlg(TXT_NO_AREA_SELECTED, mtError, [mbOK]);
             Exit;
            end;
  NODES: if not Project.IsNode(CurrentList) then
         begin
           Uutils.MsgDlg(TXT_NO_NODE_SELECTED, mtError, [mbOK]);
           Exit;
         end;
  LINKS: if not Project.IsLink(CurrentList) then
         begin
           Uutils.MsgDlg(TXT_NO_LINK_SELECTED, mtError, [mbOK]);
           Exit;
         end;
  end;

  // Load the object's ID into the edit box
  with Project do
  begin
    if CurrentItem[CurrentList] >= 0
    then ObjectIDEdit.Text := GetID(CurrentList, CurrentItem[CurrentList]);
  end;
end;


function TStatsSelectForm.GetStatsSelection(var Stats: TStatsSelection): Boolean;
//-----------------------------------------------------------------------------
// Places user's selections into a TStatsSelection data structure
//-----------------------------------------------------------------------------
begin
  Result := False;
  with Stats do
  begin
    ObjectType := ObjectTypeCombo.ItemIndex;
    if ObjectType = SYS
    then Variable := VariableCombo.ItemIndex
    else Variable := Ubrowser.GetIndexOfVar(ObjectType, VariableCombo.Text);

    ObjectID := ObjectIDEdit.Text;
    if Length(Trim(MinDeltaEdit.Text)) = 0 then MinEventDelta := 0
    else MinEventDelta := StrToFloat(MinDeltaEdit.Text);
    if Length(Trim(MinVolEdit.Text)) = 0 then MinEventVolume := -1
    else MinEventVolume := StrToFloat(MinVolEdit.Text);
    if Length(Trim(MinValueEdit.Text)) = 0 then MinEventValue := -1
    else MinEventValue := StrToFloat(MinValueEdit.Text);

    TimePeriod := TTimePeriod(TimePeriodCombo.ItemIndex);
    if StatsTypeIndex = QUALSTATS
    then StatsType := TstatsType(Ord(stMeanConcen) + StatsTypeCombo.ItemIndex)
    else StatsType := TstatsType(StatsTypeCombo.ItemIndex);

    PlotParameter := 0;
    PlotPosition := ppFrequency;
    if PlotParameter > 1
    then Uutils.MsgDlg(TXT_INVALID_PLOT_PARAM, mtError, [mbOK])
    else if (ObjectType <> SYS)
    and (Uglobals.GetObject(ObjectType, ObjectID) = nil)
    then Uutils.MsgDlg(TXT_NO_OBJECT_SELECTED, mtError, [mbOK])
    else Result := True;
  end;
  if Result = True then GetVariableTypes(Stats);
end;


procedure TStatsSelectForm.GetVariableTypes(var Stats: TStatsSelection);
//-----------------------------------------------------------------------------
// Determines if the variable being analyzed is rainfall, losses, or quality.
//-----------------------------------------------------------------------------
begin
  Stats.IsQualParam := False;
  Stats.IsRainParam := False;
  Stats.VarIndex := Uoutput.GetVarIndex(Stats.Variable, Stats.ObjectType);
  Stats.FlowVarIndex := -1;
  case Stats.ObjectType of

    SUBCATCHMENTS:
    begin
      if Stats.Variable >= SUBCATCHQUAL
      then Stats.IsQualParam := True
      else
      begin
        if (Stats.Variable = RAINFALL)
        or (Stats.Variable = EVAP)
        or (Stats.Variable = INFIL)
        then Stats.IsRainParam := True;
      end;
      if Stats.IsQualParam
      then Stats.FlowVarIndex := Uoutput.GetVarIndex(RUNOFF, SUBCATCHMENTS)
      else Stats.FlowVarIndex := Stats.VarIndex;
    end;

    NODES:
    begin
      if Stats.Variable >= NODEQUAL then Stats.IsQualParam := True;
      if Stats.Variable = INFLOW
      then Stats.FlowVarIndex := Uoutput.GetVarIndex(INFLOW, NODES);
      if Stats.Variable = OVERFLOW
      then Stats.FlowVarIndex := Uoutput.GetVarIndex(OVERFLOW, NODES);
    end;

    LINKS:
    begin
      if Stats.Variable >= LINKQUAL then Stats.IsQualParam := True;
      Stats.FlowVarIndex := Uoutput.GetVarIndex(FLOW, LINKS);
    end;

    SYS:
    begin
      if (Stats.Variable = SYS_RAINFALL)
      or (Stats.Variable = SYS_INFIL)
      or (Stats.Variable = SYS_EVAP)
      then Stats.IsRainParam := True;
      Stats.FlowVarIndex := Stats.VarIndex;
    end;
  end;
end;


procedure TStatsSelectForm.BtnHelpClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 212050);
end;

procedure TStatsSelectForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then BtnHelpClick(Sender);
end;

end.
