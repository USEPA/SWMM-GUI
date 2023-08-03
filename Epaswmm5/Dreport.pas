unit Dreport;

{-------------------------------------------------------------------}
{                    Unit:    Dreport.pas                           }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Stay-on-top form unit used to define the contents of scatter    }
{   plots and tabular reports. It consists of a Notebook control    }
{   with separate pages for each type of report. When the user      }
{   clicks the OK button, the selections made on the form are       }
{   placed into a TReportSelection record (see Uglobals.pas for     }
{   description) and the MainForm's CreateReport procedure is       }
{   called.                                                         }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Buttons, CheckLst, Contnrs,
  Spin, NumEdit, Uglobals, Uproject, Uutils;

type
  TReportSelectForm = class(TForm)
    BtnOK: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;

    Notebook1: TNotebook;
    Label1: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    StartDateCombo1: TComboBox;
    EndDateCombo1: TComboBox;
    Label3: TLabel;
    VariableListBox: TCheckListBox;
    CategoryCombo: TComboBox;

    Label2: TLabel;
    Label6: TLabel;
    StartDateCombo2: TComboBox;
    EndDateCombo2: TComboBox;
    XVariableBox: TGroupBox;
    Label9: TLabel;
    XCategoryCombo: TComboBox;
    Label10: TLabel;
    XObjectEdit: TEdit;
    XObjectBtn: TBitBtn;
    Label11: TLabel;
    XVariableCombo: TComboBox;
    Label15: TLabel;
    TimeAxisCombo: TComboBox;
    YVariableBox: TGroupBox;
    Label12: TLabel;
    YCategoryCombo: TComboBox;
    Label13: TLabel;
    YObjectEdit: TEdit;
    YObjectBtn: TBitBtn;
    Label14: TLabel;
    YVariableCombo: TComboBox;
    ObjectsPanel: TPanel;
    ObjectsLabel: TLabel;
    ItemsListBox: TListBox;
    BtnAdd: TBitBtn;
    BtnDelete: TBitBtn;
    BtnMoveUp: TBitBtn;
    BtnMoveDown: TBitBtn;
    procedure BtnOKClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure BtnAddClick(Sender: TObject);
    procedure BtnDeleteClick(Sender: TObject);
    procedure BtnMoveUpClick(Sender: TObject);
    procedure BtnMoveDownClick(Sender: TObject);
    procedure CategoryComboClick(Sender: TObject);
    procedure VariableListBoxClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

  private
    { Private declarations }
    ReportTypeChoice: Integer;         // Type of report being generated
    MaxObjects: Integer;               // Max. number of items being reported on
    MultiVariable: Boolean;            // True if > 1 variable being reported on
    procedure AddToItemList;
    procedure CreateReport;
    function  GetObjectType(const ItemIndex: Integer): Integer;
    procedure Initialize;

  public
    { Public declarations }
    procedure SetReportType(const aReportType: Integer);
  end;

var
  ReportSelectForm: TReportSelectForm;      // Do not comment this out.

implementation

{$R *.DFM}

uses
  Dprofile, Fmain, Fmap, Ubrowser, Uoutput;

const
  TXT_SYSTEM = 'System';
  TXT_TIME_SERIES_PLOT = 'Time Series Plot';
  TXT_SCATTER_PLOT = 'Scatter Plot Selection';
  TXT_PROFILE_PLOT = 'Profile Plot Selection';
  TXT_TABLE_BY_VARIABLE = 'Table by Variable Selection';
  TXT_TABLE_BY_OBJECT = 'Table by Object Selection';
  TXT_OBJECTS_LABEL1 =
  'Select an object from the Map or the Browser. ';
  TXT_OBJECTS_LABEL2 =
  'Then click the "+" button to add it to the ';
  TXT_OBJECTS_LABEL_PLOT = 'plot.';
  TXT_OBJECTS_LABEL_TABLE = 'table.';

  MSG_NO_VARIABLE = 'No variable was selected';
  MSG_NO_ITEMS = 'No objects to display';
  MSG_TOO_FEW_ITEMS = 'Too few objects to display';
  MSG_TOO_MANY_ITEMS = 'Too many objects to display';
  MSG_NO_RESULTS = 'No results available to display';
  MSG_NO_SUBCATCH_SELECTED = 'Must select a subcatchment.';
  MSG_NO_NODE_SELECTED = 'Must select a node.';
  MSG_NO_LINK_SELECTED = 'Must select a link.';
  MSG_INVALID_DATES = 'End date comes before start date.';
  MSG_NO_RESULTS_AVAILABLE = 'There are no results available for this object.';

  Categories: String = 'Subcatchments'#13'Nodes'#13'Links';


procedure TReportSelectForm.Initialize;
//-----------------------------------------------------------------------------
// Initializes the contents of the form.
//-----------------------------------------------------------------------------
begin
  // Add items to the object category combo box
  CategoryCombo.Items.Clear;
  CategoryCombo.Items.Text := Categories;

  // Select default type of object to report on
  if Project.IsSubcatch(CurrentList) then
  begin
    CategoryCombo.ItemIndex := 0;
    AddToItemList;
  end
  else if Project.IsNode(CurrentList) then
  begin
    CategoryCombo.ItemIndex := 1;
    AddToItemList;
  end
  else if Project.IsLink(CurrentList) then
  begin
    CategoryCombo.ItemIndex := 2;
    AddToItemList;
  end
  else CategoryCombo.ItemIndex := 0;

  // Select to report by elapsed time or by date/time
  if RptElapsedTime then TimeAxisCombo.ItemIndex := 0
  else TimeAxisCombo.ItemIndex := 1;
  MultiVariable := False;

  // Activate item selection buttons
  BtnDelete.Visible := True;
  BtnMoveUp.Visible := True;
  BtnMoveDown.Visible := True;

  with MainForm.ProjectImageList do
  begin
    GetBitmap(GetIndexByName('blue_plus'), BtnAdd.Glyph);
    GetBitmap(GetIndexByName('blue_minus'), BtnDelete.Glyph);
    GetBitmap(GetIndexByName('uparrow2'), BtnMoveUp.Glyph);
    GetBitmap(GetIndexByName('dnarrow2'), BtnMoveDown.Glyph);
    GetBitmap(GetIndexByName('blue_plus'), XObjectBtn.Glyph);
    GetBitmap(GetIndexByName('blue_plus'), YobjectBtn.Glyph);
  end;
end;


procedure TReportSelectForm.SetReportType(const aReportType: Integer);
//-----------------------------------------------------------------------------
// Sets up the form to display choices for a particular type of report.
//-----------------------------------------------------------------------------
var
  S: String;
begin
  // Initialize the contents of the form
  Initialize;

  // Save the type of report being created
  ReportTypeChoice := aReportType;
  S := TXT_OBJECTS_LABEL1 + TXT_OBJECTS_LABEL2;

  // Report is a time series plot
  if ReportTypeChoice = TIMESERIESPLOT then
  begin
    Caption := TXT_TIME_SERIES_PLOT;
    Notebook1.PageIndex := 0;
    MaxObjects := MAXSERIES + 1;
    CategoryCombo.Items.Add(TXT_SYSTEM);
    CategoryComboClick(CategoryCombo);
  end;

  // Report is a scatter plot
  if ReportTypeChoice = SCATTERPLOT then
  begin
    Caption := TXT_SCATTER_PLOT;
    Notebook1.PageIndex := 1;
    XCategoryCombo.Items := CategoryCombo.Items;
    XCategoryCombo.ItemIndex := CategoryCombo.ItemIndex;
    CategoryComboClick(XCategoryCombo);
    YCategoryCombo.Items := CategoryCombo.Items;
    YCategoryCombo.ItemIndex := CategoryCombo.ItemIndex;
    CategoryComboClick(YCategoryCombo);
  end;

  // Report is a time series table for a specific variable
  if ReportTypeChoice = TABLEBYVARIABLE then
  begin
    Caption := TXT_TABLE_BY_VARIABLE;
    Notebook1.PageIndex := 0;
    MaxObjects := MAXCOLS;  //Largest number of columns allowed in the table
    CategoryComboClick(CategoryCombo);
  end;

  if ReportTypeChoice = TABLEBYOBJECT then
  begin
    Caption := TXT_TABLE_BY_OBJECT;
    Notebook1.PageIndex := 0;
    MultiVariable := True;
    BtnDelete.Visible := False;
    BtnMoveUp.Visible := False;
    BtnMoveDown.Visible := False;
    MaxObjects := 1;
    CategoryCombo.Items.Add(TXT_SYSTEM);
    CategoryComboClick(CategoryCombo);
  end;

  // Assign dates to date combo boxes
  if Notebook1.PageIndex = 0 then
  begin
    StartDateCombo1.Items := MainForm.DateListBox.Items;
    EndDateCombo1.Items := MainForm.DateListBox.Items;
    StartDateCombo1.ItemIndex := 0;
    EndDateCombo1.ItemIndex := EndDateCombo1.Items.Count - 1;
  end;
  if Notebook1.PageIndex = 1 then
  begin
    StartDateCombo2.Items := MainForm.DateListBox.Items;
    EndDateCombo2.Items := MainForm.DateListBox.Items;
    StartDateCombo2.ItemIndex := 0;
    EndDateCombo2.ItemIndex := EndDateCombo2.Items.Count - 1;
  end;
end;


procedure TReportSelectForm.CategoryComboClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick event handler for CategoryCombo control. Changes entries in the
// VariableCombo control depending on whether Subcatchments, Nodes, Links
// or System was selected.
//-----------------------------------------------------------------------------
var
  I: Integer;
  Receiver: TComboBox;
begin
  // Identify which combobox needs to be updated
  if      Sender = CategoryCombo  then Receiver := XVariableCombo
  else if Sender = XCategoryCombo then Receiver := XVariableCombo
  else if Sender = YCategoryCombo then Receiver := YVariableCombo
  else Exit;

  // Check which class of object was selected
  with Sender as TComboBox do
  begin
    Receiver.Items.Clear;
    Case ItemIndex of
    // Subcatchments were selected
    0: begin
         // Load subcatchment variables into Receiver combobox
         for I := Uglobals.SUBCATCHOUTVAR1 to
                  MainForm.SubcatchViewBox.Items.Count-1 do
           Receiver.Items.Add(MainForm.SubcatchViewBox.Items[I]);
         Receiver.ItemIndex := 0;

         // Place current subcatchment from Browser into the ItemsList listbox
         if Notebook1.PageIndex = 0 then
         begin
           ItemsListBox.Clear;
           if ItemsListBox.Enabled then
             if Project.IsSubcatch(CurrentList) then AddToItemList;
         end;
       end;

    // Nodes were selected
    1: begin
         // Load node variables into the Variable combobox
         for I := Uglobals.NODEOUTVAR1 to MainForm.NodeViewBox.Items.Count-1 do
           Receiver.Items.Add(MainForm.NodeViewBox.Items[I]);
         Receiver.ItemIndex := 0;

         // Place current node from Browser into the ItemsList listbox
         if Notebook1.PageIndex = 0 then
         begin
           ItemsListBox.Clear;
           if ItemsListBox.Enabled then
             if Project.IsNode(CurrentList) then AddToItemList;
         end;
       end;

    // Links were selected
    2: begin
         // Load link variables into the Variable combobox
         for I := Uglobals.LINKOUTVAR1 to MainForm.LinkViewBox.Items.Count-1 do
           Receiver.Items.Add(MainForm.LinkViewBox.Items[I]);
         Receiver.ItemIndex := 0;

         // Place current link from Browser into the ItemsList listbox
         if Notebook1.PageIndex = 0 then
         begin
           ItemsListBox.Clear;
           if ItemsListBox.Enabled then
             if Project.IsLink(CurrentList) then AddToItemList;
         end;
       end;

    // System was selected
    3: begin
         // Load system variables into Variable combobox
         for I := 0 to Uglobals.NsysViews-1 do
           Receiver.Items.Add(Uglobals.SysViewNames[I]);
         Receiver.ItemIndex := 0;
       end;
    end;
  end;

  // Finish setting up the time series plot/table page
  if Notebook1.PageIndex = 0 then
  begin
    if CategoryCombo.ItemIndex = SYS then ObjectsPanel.Visible := False
    else ObjectsPanel.Visible := True;
    ObjectsLabel.Caption := CategoryCombo.Text;
    VariableListBox.Items.Assign(XVariableCombo.Items);
    VariableListBox.ItemIndex := 0;
    if CategoryCombo.ItemIndex <> SYS then VariableListBox.Checked[0] := True;
  end;
end;


procedure TReportSelectForm.VariableListBoxClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the check list box used to select which variable(s)
//  to report on. Makes sure that only one variable is selected if the report
//  is not multi-variable.
//-----------------------------------------------------------------------------
var
  I: Integer;
  isMultiVariable: Boolean;
begin
  if CategoryCombo.ItemIndex = SYS
  then isMultiVariable := True
  else isMultiVariable := Multivariable;
  if not isMultiVariable then with VariableListBox do
  begin
    for I := 0 to Items.Count-1 do Checked[I] := (I = ItemIndex);
  end;
end;


procedure TReportSelectForm.BtnOKClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the OK button.
//-----------------------------------------------------------------------------
var
  I : Integer;
  VariableCount: Integer;
begin
  // Check that one or more objects were selected to report on
  // for scatter plots
  if ReportTypeChoice = SCATTERPLOT then
  begin
    if (Length(Trim(XObjectEdit.Text)) = 0)
    or (Length(Trim(YObjectEdit.Text)) = 0) then
    begin
      Uutils.MsgDlg(MSG_TOO_FEW_ITEMS, mtError, [mbOK]);
      Exit;
    end;
  end

  // For time series plots/tables,
  else
  begin
    // Check that >= 1 objects were selected
    if  (ItemsListBox.Items.Count <= 0)
    and (CategoryCombo.ItemIndex <> SYS) then
    begin
      Uutils.MsgDlg(MSG_NO_ITEMS, mtError, [mbOK]);
      Exit;
    end

    // Check that too many objects were not selected
    else if ItemsListBox.Items.Count > MaxObjects then
    begin
      Uutils.MsgDlg(MSG_TOO_MANY_ITEMS, mtError, [mbOK]);
      Exit;
    end;

    // Check that >= 1 variable was selected
    VariableCount := 0;
    with VariableListBox do
      for I := 0 to Items.Count-1 do
       if Checked[I] then Inc(VariableCount);
    if VariableCount = 0 then
    begin
      Uutils.MsgDlg(MSG_NO_VARIABLE, mtError, [mbOK]);
      Exit;
    end;
  end;

  // Check that End Date > Start Date
  if ( (Notebook1.PageIndex = 0) and
     (StartDateCombo1.ItemIndex > EndDateCombo1.ItemIndex) )
  or ( (Notebook1.PageIndex = 1) and
     (StartDateCombo2.ItemIndex > EndDateCombo2.ItemIndex) )
  then
  begin
    Uutils.MsgDlg(MSG_INVALID_DATES, mtError, [mbOK]);
    Exit;
  end;

  // Save report selections and create the report
  Hide;
  Application.ProcessMessages;
  CreateReport;
end;


procedure TReportSelectForm.BtnCancelClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the Cancel button.
//-----------------------------------------------------------------------------
begin
  Hide;
end;


procedure TReportSelectForm.BtnAddClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the Add button. Adds the currently selected object
// appearing in the Data Browser to the items to be plotted.
//-----------------------------------------------------------------------------
var
  theCategory: Integer;
begin
  // Determine the class of object to be reported on
  if      Sender = BtnAdd     then theCategory := CategoryCombo.ItemIndex
  else if Sender = XObjectBtn then theCategory := XCategoryCombo.ItemIndex
  else if Sender = YObjectBtn then theCategory := YCategoryCombo.ItemIndex
  else Exit;

  // Check that results are available for the object
  if not Project.HasResults(CurrentList, Project.CurrentItem[CurrentList]) then
  begin
    Uutils.MsgDlg(MSG_NO_RESULTS_AVAILABLE, mtInformation, [mbOK]);
    Exit;
  end;

  // Check that the current object appearing in the Browser is of this class
  case theCategory of
  0: if not Project.IsSubcatch(CurrentList) then
     begin
       Uutils.MsgDlg(MSG_NO_SUBCATCH_SELECTED, mtError, [mbOK]);
       Exit;
     end;
  1: if not Project.IsNode(CurrentList) then
     begin
       Uutils.MsgDlg(MSG_NO_NODE_SELECTED, mtError, [mbOK]);
       Exit;
     end;
  2: if not Project.IsLink(CurrentList) then
     begin
       Uutils.MsgDlg(MSG_NO_LINK_SELECTED, mtError, [mbOK]);
       Exit;
     end;
  end;

  // Add the object to the list of items to be plotted or tabulated
  // ...this is for time series
  if Sender = BtnAdd then
  begin
    with ItemsListBox do
      if (MaxObjects = 1) and (Items.Count = MaxObjects) then Items.Delete(0);
    AddToItemList;
  end
  // ...this is for the X-variable of a scatter plot
  else if Sender = XObjectBtn then with Project do
  begin
    if CurrentItem[CurrentList] >= 0 then
      XObjectEdit.Text := GetID(CurrentList, CurrentItem[CurrentList]);
  end
  // ...this is for the Y-variable of a scatter plot
  else if Sender = YObjectBtn then with Project do
  begin
    if CurrentItem[CurrentList] >= 0 then
      YObjectEdit.Text := GetID(CurrentList, CurrentItem[CurrentList]);
  end;
end;


procedure TReportSelectForm.BtnDeleteClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for Delete button. Deletes selected item from Items listbox.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  with ItemsListBox do
  begin
    I := ItemIndex;
    if ItemIndex >= 0 then
      Items.Delete(ItemIndex);
    if Items.Count > 0 then
    begin
      if I < Items.Count then
        ItemIndex := I
      else
        ItemIndex := Items.Count-1;
    end;
  end;
end;


procedure TReportSelectForm.BtnMoveUpClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for Move Up button. Moves the selected item in the Items
// list box up one level.
//-----------------------------------------------------------------------------
begin
  with ItemsListBox do
  begin
    if ItemIndex > 0 then Items.Exchange(ItemIndex,ItemIndex-1);
  end;
end;


procedure TReportSelectForm.BtnMoveDownClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for Move Down button. Moves the selected item in the Items
// list box down one level.
//-----------------------------------------------------------------------------
begin
  with ItemsListBox do
  begin
    if ItemIndex < Items.Count-1 then Items.Exchange(ItemIndex,ItemIndex+1);
  end;
end;


procedure TReportSelectForm.AddToItemList;
//-----------------------------------------------------------------------------
// Adds a new item (subcatchment, node or link ID) to the Items listbox.
//-----------------------------------------------------------------------------
begin
  with Project do
  begin
    if not HasResults(CurrentList, CurrentItem[CurrentList]) then Exit;
    if CurrentItem[CurrentList] >= 0 then with ItemsListBox do
    begin
      Items.Add(GetID(CurrentList, CurrentItem[CurrentList]));
      ItemIndex := Items.Count-1;
    end;
  end;
end;


procedure TReportSelectForm.CreateReport;
//-----------------------------------------------------------------------------
// Creates a graph or table based on the selections made on the form.
//-----------------------------------------------------------------------------
var
  I : Integer;
  ReportSelection: TReportSelection;
begin
  with ReportSelection do
  begin
    StartDateIndex := 0;
    EndDateIndex   := 0;
    if ReportTypeChoice = SCATTERPLOT then
    begin
      StartDateIndex := StartDateCombo2.ItemIndex;
      EndDateIndex   := EndDateCombo2.ItemIndex + 1;
      XObjectType    := GetObjectType(XCategoryCombo.ItemIndex);
      ObjectType     := GetObjectType(YCategoryCombo.ItemIndex);
      ItemsListBox.Clear;
      ItemsListBox.Items.Add(XObjectEdit.Text);
      ItemsListBox.Items.Add(YObjectEdit.Text);
      Items := ItemsListBox.Items;
      ItemCount := ItemsListBox.Items.Count;
      Variables[0] := Ubrowser.GetIndexOfVar(XObjectType, XVariableCombo.Text);
      Variables[1] := Ubrowser.GetIndexOfVar(ObjectType,  YVariableCombo.Text);
      VariableCount := 2;
      ReportType := ReportTypeChoice;
    end

    else
    begin
      StartDateIndex  := StartDateCombo1.ItemIndex;
      EndDateIndex    := EndDateCombo1.ItemIndex + 1;
      ObjectType      := GetObjectType(CategoryCombo.ItemIndex);
      Items           := ItemsListBox.Items;
      ItemCount       := ItemsListBox.Items.Count;
      ReportType      := ReportTypeChoice;
      DateTimeDisplay := (TimeAxisCombo.ItemIndex > 0);

      if  (ReportType = TABLEBYVARIABLE)
      or  ( (ReportType = TIMESERIESPLOT) and (ObjectType <> SYS) ) then
      begin
        Variables[0] := Ubrowser.GetIndexOfVar(ObjectType,
                        VariableListBox.Items[VariableListBox.ItemIndex]);
        VariableCount := 1;
      end;

      if (ReportType = TABLEBYOBJECT)
      or ( (ReportType = TIMESERIESPLOT) and (ObjectType = SYS) )
      then with VariableListBox do
      begin
        VariableCount := 0;
        for I := 0 to Items.Count-1 do
        begin
          if Checked[I] then
          begin
            if ObjectType = SYS then Variables[VariableCount] := I
            else Variables[VariableCount] :=
              Ubrowser.GetIndexOfVar(ObjectType, Items[I]);
            Inc(VariableCount);
            if VariableCount > MAXCOLS then break;
          end;
        end;
      end;
    end;
  end;
  MainForm.CreateReport(ReportSelection);
end;


function TReportSelectForm.GetObjectType(const ItemIndex: Integer): Integer;
//-----------------------------------------------------------------------------
//  Determines the class of object selected in one of the object category
//  combo boxes.
//-----------------------------------------------------------------------------
begin
  if ItemIndex = 0 then Result := SUBCATCHMENTS
  else if ItemIndex = 1 then Result := NODES
  else if ItemIndex = 2 then Result := LINKS
  else Result := SYS;
end;

procedure TReportSelectForm.BtnHelpClick(Sender: TObject);
var
  HC: Integer;
begin
  case ReportTypeChoice of
    TIMESERIESPLOT:  HC := 211390;
    SCATTERPLOT:     HC := 212460;
    TABLEBYOBJECT:   HC := 211400;
    TABLEBYVARIABLE: HC := 211410;
    else HC := 0;
  end;
  if HC > 0 then Application.HelpCommand(HELP_CONTEXT, HC);
end;

procedure TReportSelectForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then BtnHelpClick(Sender);
end;

end.
