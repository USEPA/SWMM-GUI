unit Dquery;

{-------------------------------------------------------------------}
{                    Unit:    Dquery.pas                            }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{  Stay on top form unit that performs a Map Query (such as         }
{  locate all nodes with flooding > 0)                              }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Uglobals, Uutils, ExtCtrls, Buttons;

type
  TQueryForm = class(TForm)
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    Edit1: TEdit;
    Panel1: TPanel;
    Button1: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure ComboBox2MeasureItem(Control: TWinControl; Index: Integer;
      var Height: Integer);
    procedure ComboBox2DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
  private
    { Private declarations }
    OldSubcatchVar: Integer;
    OldLinkVar: Integer;
    OldNodeVar: Integer;
    OldShowSubcatchs: Boolean;
    OldShowNodes: Boolean;
    OldShowLinks: Boolean;

    procedure SetInflowNodeColors;
    procedure SetLIDSubcatchColors;
    function  NeedsSeparator(Index: Integer): Boolean;

  public
    { Public declarations }
    procedure Clear;
    procedure UpdateVariables;
    procedure UpdateQueryCaption;
  end;

var
  QueryForm: TQueryForm;

implementation

{$R *.DFM}

uses
  Fmain, Fmap, Ubrowser, Uoutput, Uproject, Ulid;

const
  TXT_SUBCATCH_WITH = 'Subcatchments';
  TXT_NODES_WITH = 'Nodes';
  TXT_LINKS_WITH = 'Links';
  TXT_ITEMS_FOUND = ' items found';

  TXT_INFLOWS_WITH = 'Inflow Nodes';
  TXT_INFLOW_TYPE : String =
    ''#13'Direct Inflow'#13'DW Inflow'#13'RDII Inflow'#13'GW Inflow';

  TXT_LIDS_WITH = 'LID Subcatchments';

var
  TxtLIDType : String;

procedure TQueryForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
// OnCreate handler for form.
//-----------------------------------------------------------------------------
var
  I: Integer;
  P: TPoint;
begin
  // Load items into Subcatch/Node/Link selection combo box
  ComboBox1.Items.Add(TXT_SUBCATCH_WITH);
  ComboBox1.Items.Add(TXT_NODES_WITH);
  ComboBox1.Items.Add(TXT_LINKS_WITH);
  ComboBox1.Items.Add(TXT_LIDS_WITH);
  ComboBox1.Items.Add(TXT_INFLOWS_WITH);
  ComboBox1.ItemIndex := 0;

  //Add names of LID types to TxtLIDType string
  TxtLIDType := ''#13'Any LIDs';
  for I := 0 to High(Ulid.ProcessTypesLong) do
  begin
    TxtLIDType := TxtLIDType + #13 + Ulid.ProcessTypesLong[I];
  end;

  // Load the choices of relations to query on (below, equals, above)
  for I := Low(FilterRelation) to High(FilterRelation) do
    ComboBox3.Items.Add(FilterRelation[I]);
  ComboBox3.ItemIndex := 0;
  OldSubcatchVar := -1;
  OldNodeVar := -1;
  OldLinkVar := -1;

  // Position form at top left of Main form
  with MainForm do
  begin
    P.x := Left + (Width - ClientWidth) - 2;
    P.Y := Top + (Height-ClientHeight) - 2;
  end;
  Top := P.Y;
  Left := P.X;
end;

procedure TQueryForm.FormShow(Sender: TObject);
//-----------------------------------------------------------------------------
// OnShow handler for the form.
//-----------------------------------------------------------------------------
begin
  // Set display panel's font color
  Panel1.Font.Color := clRed;

  // Save show settings for each class of object
  with MapForm.Map.Options do
  begin
    OldShowSubcatchs := ShowSubcatchs;
    OldShowNodes := ShowNodes;
    OldShowLinks := ShowLinks;
  end;
  ComboBox1Change(self);
end;

procedure TQueryForm.FormClose(Sender: TObject; var Action: TCloseAction);
//-----------------------------------------------------------------------------
// OnClose handler for form. Restores the map display to the state it had
// before last query was made.
//-----------------------------------------------------------------------------
begin
  Panel1.Caption := '';
  if (QueryFlag) then
  begin
    QueryFlag := False;
    CurrentSubcatchVar := OldSubcatchVar;
    CurrentNodeVar := OldNodeVar;
    CurrentLinkVar := OldLinkVar;
    if not RunFlag then
    begin
      if CurrentSubcatchVar >= SUBCATCHOUTVAR1
      then CurrentSubcatchVar := NOVIEW;
      if CurrentNodeVar >= NODEOUTVAR1 then CurrentNodeVar := NOVIEW;
      if CurrentLinkVar >= LINKOUTVAR1 then CurrentLinkVar := NOVIEW;
    end;
    OldSubcatchVar := -1;
    OldNodeVar := -1;
    OldLinkVar := -1;
    Uoutput.SetSubcatchColors;
    Uoutput.SetNodeColors;
    Uoutput.SetLinkColors;
    MapForm.Map.Options.ShowSubcatchs := OldShowSubcatchs;
    MapForm.Map.Options.ShowNodes := OldShowNodes;
    MapForm.Map.Options.ShowLinks := OldShowLinks;
    MapForm.RedrawMap;
    MapForm.DrawSubcatchLegend;
    MapForm.DrawNodeLegend;
    MapForm.DrawLinkLegend;
  end;

  // Enable selection of map view theme from main form's Browser panel
  MainForm.SubcatchViewBox.Enabled := True;
  MainForm.NodeViewBox.Enabled := True;
  MainForm.LinkViewBox.Enabled := True;
  Action := caHide;
end;

procedure TQueryForm.Clear;
//-----------------------------------------------------------------------------
// Clears the Query Value and Results fields of the form.
//-----------------------------------------------------------------------------
begin
  Edit1.Text := '';
  Panel1.Caption := '';
end;

procedure TQueryForm.UpdateVariables;
//-----------------------------------------------------------------------------
// Changes the names of the view variables listed in the Variable combo box.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  with ComboBox2 do
  begin
    I := ItemIndex;
    Clear;
    case ComboBox1.ItemIndex of
    0: Items.Assign(MainForm.SubcatchViewBox.Items);
    1: Items.Assign(MainForm.NodeViewBox.Items);
    2: Items.Assign(MainForm.LinkViewBox.Items);
    3: Items.Text := TxtLIDType;
    4: Items.Text := TXT_INFLOW_TYPE;
    end;
    Items.Delete(0);
    if I >= Items.Count then ItemIndex := 0 else ItemIndex := I;
  end;
end;

procedure TQueryForm.ComboBox1Change(Sender: TObject);
//-----------------------------------------------------------------------------
// OnChange handler for the Area/Node/Link combo box. Changes list of map
// view variables to choose from when user switches between Subcatchment,
// Node & Link.
//-----------------------------------------------------------------------------
begin
  UpdateVariables;
  ComboBox2.ItemIndex := 0;
  ComboBox3.Visible := ComboBox1.ItemIndex < 3;
  Edit1.Visible := ComboBox3.Visible;
  Panel1.Caption := '';
end;

procedure TQueryForm.ComboBox2Change(Sender: TObject);
begin
  Panel1.Caption := '';
end;

procedure TQueryForm.ComboBox2DrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  ItemText: String;
  DrawSeparator: Boolean;
  TxtRect: TRect;
begin
  DrawSeparator := NeedsSeparator(Index);

  with (Control as TComboBox).Canvas do
  begin

    if odSelected In State then
    begin
      Pen.Color := clHighlightText;
      Brush.Color := clHighlight;
      DrawSeparator := False;
    end;

    TxtRect := Rect;
    if DrawSeparator and (Control as TComboBox).DroppedDown then
      Inc(TxtRect.Top, 3);
    FillRect(Rect);

    ItemText := (Control as TComboBox).Items[Index];
    TextOut(TxtRect.Left + 3,
      (TxtRect.Top + TxtRect.Bottom -  TextHeight(ItemText)) div 2, ItemText);

    if DrawSeparator and (Control as TComboBox).DroppedDown then
    begin
      Pen.Color := Font.Color;
      MoveTo(Rect.Left, Rect.Top + 1);
      LineTo(Rect.Right, Rect.Top + 1);
    end;

  end;
end;

procedure TQueryForm.ComboBox2MeasureItem(Control: TWinControl; Index: Integer;
  var Height: Integer);
begin
  with Control as TComboBox do
  begin
    Height := ItemHeight;
    if (Index <> -1) and NeedsSeparator(Index) then
      Height := Height + 3;
  end;
end;

function TQueryForm.NeedsSeparator(Index: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;

  // ComboBox2 doesn't include NOVIEW as item 0
  I := Index + 1;

  if ComboBox1.ItemIndex = 0 then
  begin
    if (I = SUBCATCHRPTVAR1) or (I = SUBCATCHOUTVAR1) then Result := True;
  end
  else if ComboBox1.ItemIndex = 1 then
  begin
    if (I = NODERPTVAR1) or (I = NODEOUTVAR1) then Result := True;
  end
  else if ComboBox1.ItemIndex = 2 then
  begin
    if (I = LINKRPTVAR1) or (I = LINKOUTVAR1) then Result := True;
  end
end;

procedure TQueryForm.Button1Click(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the Go button that executes the query.
//-----------------------------------------------------------------------------
begin
  // Check for valid numerical entry for query comparison value
  if not Edit1.Visible
  or Uutils.IsValidNumber(Edit1.Text,QueryValue) then
  begin

    // Set a flag & save query relation type
    QueryFlag := True;
    QueryRelation := TRelationType(ComboBox3.ItemIndex);

    // Disable selection of map view theme from Main form Browser panel
    MainForm.SubcatchViewBox.Enabled := False;
    MainForm.NodeViewBox.Enabled := False;
    MainForm.LinkViewBox.Enabled := False;

    // If this is first query since form was last shown then save
    // current status of map view
    if (OldSubcatchVar < 0) then OldSubcatchVar := CurrentSubcatchVar;
    if (OldNodeVar < 0) then OldNodeVar := CurrentNodeVar;
    if (OldLinkVar < 0) then OldLinkVar := CurrentLinkVar;

  // Select the query variable for viewing on the map
    CurrentSubcatchVar := NOVIEW;
    CurrentLinkVar := NOVIEW;
    CurrentNodeVar := NOVIEW;
    if (ComboBox1.ItemIndex = 0) then
    begin
      CurrentSubcatchVar := ComboBox2.ItemIndex+1;
      Uoutput.SetSubcatchColors;
    end
    else if (ComboBox1.ItemIndex = 1) then
    begin
      CurrentNodeVar := ComboBox2.ItemIndex+1;
      Uoutput.SetNodeColors;
    end
    else if ComboBox1.ItemIndex = 2 then
    begin
      CurrentLinkVar := ComboBox2.ItemIndex+1;
      Uoutput.SetLinkColors;
    end

    else if ComboBox1.ItemIndex = 3 then SetLIDSubcatchColors

    else if ComboBox1.ItemIndex = 4 then SetInflowNodeColors;

  // Display number of items matching the query
    UpdateQueryCaption;

  // Redraw the map
    MapForm.RedrawMap;
    MapForm.DrawSubcatchLegend;
    MapForm.DrawNodeLegend;
    MapForm.DrawLinkLegend;
  end
  else Edit1.SetFocus;
end;

procedure TQueryForm.SetInflowNodeColors;
//-----------------------------------------------------------------------------
// Identifies which nodes have a particular type of inflow assigned to them.
//-----------------------------------------------------------------------------
var
  I, J, Ntype: Integer;
  N: TNode;
  SC: TSubcatch;
begin
  // Remove color from all nodes
  for I := 0 to MAXCLASS do
  begin
    if not Project.IsNode(I) then continue;
    for J := 0 to Project.Lists[I].Count-1 do
      Project.GetNode(I, J).ColorIndex := -1;
  end;
  CurrentNodeVar := ComboBox2.ItemIndex+1;

  // Identify nodes receiving GW inflow from subcatchments
  if CurrentNodeVar = 4 then
  for J := 0 to Project.Lists[SUBCATCH].Count-1 do
  begin
    SC := Project.GetSubcatch(SUBCATCH, J);
    if SC.Groundwater.Count < 2 then continue;
    if not Project.FindNode(SC.Groundwater[1], Ntype, I) then continue;
    Project.GetNode(Ntype, I).ColorIndex := 1;
  end

  // Identify nodes receiving other types of inflow
  else for I := 0 to MAXCLASS do
  begin
    if not Project.IsNode(I) then continue;
    for J := 0 to Project.Lists[I].Count-1 do
    begin
      N := Project.GetNode(I, J);
      case CurrentNodeVar of
      1: if N.DXInflow.Count > 0 then N.ColorIndex := 1;
      2: if N.DWInflow.Count > 0 then N.ColorIndex := 1;
      3: if N.IIInflow.Count > 0 then N.ColorIndex := 1;
      end;
    end;
  end;
end;

procedure TQueryForm.SetLIDSubcatchColors;
var
  I: Integer;
  LIDType: Integer;
  SC: TSubcatch;
begin
  for I := 0 to Project.Lists[SUBCATCH].Count-1 do
    Project.GetSubcatch(SUBCATCH, I).ColorIndex := -1;
  CurrentSubcatchVar := LID_USAGE;
  LIDType := ComboBox2.ItemIndex - 1;
  for I := 0 to Project.Lists[SUBCATCH].Count-1 do
  begin
    SC := Project.GetSubcatch(SUBCATCH, I);
    if SC.LIDs.Count < 1 then Continue;
    if (LIDType < 0) or (Ulid.HasLIDType(SC, LIDType)) then SC.ColorIndex := 1;
  end;
end;

procedure TQueryForm.Edit1KeyPress(Sender: TObject; var Key: Char);
//-----------------------------------------------------------------------------
// OnKeyPress handler for the Edit box where user enters a value to compare
// against. Causes the query to be submitted if the user hits the Enter key.
//-----------------------------------------------------------------------------
begin
  if Key = #13 then
  begin
    Button1Click(Sender);
    Key := #0;
  end;
end;

procedure TQueryForm.FormKeyPress(Sender: TObject; var Key: Char);
//-----------------------------------------------------------------------------
// OnKeyPress handler for the form. Closes form when user hits Esc.
//-----------------------------------------------------------------------------
begin
  if Key = #27 then
  begin
    Key := #0;
    Close;
  end;
end;

procedure TQueryForm.UpdateQueryCaption;
//-----------------------------------------------------------------------------
// Updates the display of number of items matching the query.
//-----------------------------------------------------------------------------
var
  I,J,N: Integer;
begin
  Panel1.Caption := '';
  N := 0;
  if CurrentSubcatchVar <> NOVIEW then
  begin
    for J := 0 to Project.Lists[SUBCATCH].Count - 1 do
      if Project.GetSubcatch(SUBCATCH, J).ColorIndex > 0 then Inc(N);
  end
  else if CurrentNodeVar <> NOVIEW then
  begin
      for I := 0 to MAXCLASS do
      begin
        if Project.IsNode(I) then
          for J := 0 to Project.Lists[I].Count-1 do
            if Project.GetNode(I, J).ColorIndex > 0 then Inc(N);
      end;
  end
  else if CurrentLinkVar <> NOVIEW then
  begin
      for I := 0 to MAXCLASS do
      begin
        if Project.IsLink(I) then
          for J := 0 to Project.Lists[I].Count-1 do
            if Project.GetLink(I, J).ColorIndex > 0 then Inc(N);
      end;
  end;
  Panel1.Caption := IntToStr(N) + TXT_ITEMS_FOUND;
end;

end.

