unit Dfind;

{-------------------------------------------------------------------}
{                    Unit:    Dfind.pas                             }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form unit used to find a specific object on the study    }
{   area map.                                                       }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Uglobals, Uproject, Uutils;

type
  TFindForm = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Label2: TLabel;
    ListBox1: TListBox;
    Label3: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure Edit1Change(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
  private
    { Private declarations }
    procedure GetAdjacentObjects;
    procedure UpdateMapDisplay;
  public
    { Public declarations }
    FoundObject: Integer;
    FoundIndex:  Integer;
    procedure Clear;
    procedure SearchFor(const ObjType: Integer; const ObjIndex: Integer;
              ObjName: String = '');
  end;

var
  FindForm: TFindForm;

implementation

{$R *.DFM}

uses Fmain, Fmap, Fovmap, Ubrowser, Umap;

const
  TXT_NO_SUCH_OBJECT = 'There is no such object on the map';
  TXT_SUBCATCH_NOT_ON_MAP = 'Subcatchment exists but is not on the map.';
  TXT_LINK_NOT_ON_MAP = 'Link exists but is not on the map.';
  TXT_NODE_NOT_ON_MAP = 'Node exists but is not on the map.';
  GBCaption: array[0..2] of PChar =
    ('Outlet', 'Adjacent Links', 'Adjacent Nodes');


procedure TFindForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
// Form's OnCreate event handler. Positions form at top left of Main form.
//-----------------------------------------------------------------------------
begin
  Left := MainForm.Left + (MainForm.Width - MainForm.ClientWidth) - 2;
  Top := MainForm.Top + (MainForm.Height-MainForm.ClientHeight) - 2;
end;

procedure TFindForm.FormShow(Sender: TObject);
//-----------------------------------------------------------------------------
// Form's OnShow event handler. Updates font to current global font.
//-----------------------------------------------------------------------------
begin
  ComboBox1Change(Sender);
end;

procedure TFindForm.FormClose(Sender: TObject; var Action: TCloseAction);
//-----------------------------------------------------------------------------
// Form's OnClose event handler.
//-----------------------------------------------------------------------------
begin
  Action := caHide;
end;

procedure TFindForm.Clear;
//-----------------------------------------------------------------------------
// Clears ID field and Adjacent Nodes/Links fields
//-----------------------------------------------------------------------------
begin
  Edit1.Text := '';
  ListBox1.Clear;
end;

procedure TFindForm.Button1Click(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick event handler for the Find button.
//-----------------------------------------------------------------------------
var
  S     : String;
  Found : Boolean;

begin
  // Place map in Selection mode
  MainForm.SelectorButtonClick;

  // Search project for specified subcatch/node/link ID
  // and save object type and index if found.
  S := Trim(Edit1.Text);
  ListBox1.Clear;
  if (ComboBox1.ItemIndex = 0) then
    Found := Project.FindSubcatch(S, FoundObject, FoundIndex)
  else if (ComboBox1.ItemIndex = 1) then
    Found := Project.FindNode(S, FoundObject, FoundIndex)
  else
    Found := Project.FindLink(S, FoundObject, FoundIndex);

  // If ID found then highlight object on the map and
  // make it the current item shown in the Browser.
  if Found then
  begin
    GetAdjacentObjects;
    UpdateMapDisplay;
    Ubrowser.BrowserUpdate(FoundObject,FoundIndex)
  end

  // If not found then issue a message.
  else
    Uutils.MsgDlg(TXT_NO_SUCH_OBJECT, mtInformation, [mbOK]);

  Edit1.SetFocus;
  Edit1.SelectAll;
end;

procedure TFindForm.GetAdjacentObjects;
//-----------------------------------------------------------------------------
// Lists the outlet subcatch/node for a found subcatchment, the links
// connected to a found node or the nodes connected to a found link.
//-----------------------------------------------------------------------------
var
  aSubcatch: TSubcatch;
  aLink: TLink;
  aNode: TNode;
  K, M : Integer;
begin
  if Project.IsSubcatch(FoundObject) then
  begin
    aSubcatch := Project.GetSubcatch(FoundObject, FoundIndex);
    ListBox1.Items.Add(aSubcatch.Data[SUBCATCH_OUTLET_INDEX]);
  end
  else if Project.IsLink(FoundObject) then
  begin
    aLink := Project.GetLink(FoundObject, FoundIndex);
    ListBox1.Items.Add(aLink.Node1.ID);
    ListBox1.Items.Add(aLink.Node2.ID);
  end
  else
  begin
    aNode := Project.GetNode(FoundObject, FoundIndex);
    for K := 0 to MAXCLASS do
    begin
      if Project.IsLink(K) then
      begin
        for M := Project.Lists[K].Count-1 downto 0 do
        begin
          aLink := Project.GetLink(K, M);
          if (aLink.Node1 = aNode) or (aLink.Node2 = aNode)
          then ListBox1.Items.Add(Project.GetID(K, M));
        end;
      end;
    end;
  end;
end;

procedure TFindForm.UpdateMapDisplay;
//-----------------------------------------------------------------------------
// Highlights found object on the study area map, panning the map into
// position if necessary.
//-----------------------------------------------------------------------------
var
  P1, P2, P: TPoint;
  Xf, Yf   : Single;
  aNode1   : TNode;
  aNode2   : TNode;
  aSubcatch: TSubcatch;
begin
  with MapForm do
  begin

    // If found object is a link then get coords. of midpoint
    if Project.IsLink(FoundObject) then
    begin
      aNode1 := Project.GetLink(FoundObject, FoundIndex).Node1;
      aNode2 := Project.GetLink(FoundObject, FoundIndex).Node2;
      if not (Map.GetNodePixPos(aNode1, P1))
      or not (Map.GetNodePixPos(aNode2, P2)) then
      begin
        Uutils.MsgDlg(TXT_LINK_NOT_ON_MAP, mtInformation, [mbOK], MainForm);
        Exit;
      end;
      P.X := (P1.X + P2.X) div 2;
      P.Y := (P1.Y + P2.Y) div 2;
      Xf := (aNode1.X + aNode2.X) / 2;
      Yf := (aNode1.Y + aNode2.Y) / 2;
    end

    // Otherwise get found node's coords.
    else if Project.IsNode(FoundObject) then
    begin
      aNode1 := Project.GetNode(FoundObject, FoundIndex);
      if not Map.GetNodePixPos(aNode1, P) then
      begin
        Uutils.MsgDlg(TXT_NODE_NOT_ON_MAP, mtInformation, [mbOK], MainForm);
        Exit;
      end;
      Xf := aNode1.X;
      Yf := aNode1.Y;
    end

    // Otherwise get found subcatch's centroid
    else if Project.IsSubcatch(FoundObject) then
    begin
      aSubcatch := Project.GetSubcatch(FoundObject, FoundIndex);
      Xf := aSubcatch.X;
      Yf := aSubcatch.Y;
      if (Xf = MISSING) or (Yf = MISSING) then
      begin
        Uutils.MsgDlg(TXT_SUBCATCH_NOT_ON_MAP, mtInformation, [mbOK], MainForm);
        Exit;
      end;
      P := Map.GetSubcatchCentroid(aSubcatch);
    end
    else Exit;

  // If object within current map view window then exit
    if PtInRect(Map.Window.MapRect, P) then Exit;

  // Adjust map offset to position object in center of map
    with Map.Window do
    begin
      Woffset.X := Xf - Pwidth*WperP/2;
      Woffset.Y := Yf - Pheight*WperP/2;
    end;

  // Redraw the map
    Map.RedrawBackdrop;
    RedrawMap;
    OVMapForm.ShowMapExtent;
  end;
end;

procedure TFindForm.ComboBox1Change(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for ComboBox that selects a Subcatchment, Node, or Link.
// Changes caption on list of adjacent objects.
//-----------------------------------------------------------------------------
begin
  Label3.Caption := GBCaption[ComboBox1.ItemIndex];
  ListBox1.Clear;
end;

procedure TFindForm.FormKeyPress(Sender: TObject; var Key: Char);
//-----------------------------------------------------------------------------
// OnKeyPress handler for form (including the ID Edit box).
//-----------------------------------------------------------------------------
begin
  // Submit search if Enter key was pressed.
  if Key = #13 then
  begin
    Key := #0;
    Button1Click(Sender);
  end;

  // Hide form if Escape key was pressed.
  if Key = #27 then
  begin
    Key := #0;
    Hide;
  end;
end;

procedure TFindForm.Edit1Change(Sender: TObject);
//-----------------------------------------------------------------------------
// OnChange handler for ID Edit box.
//-----------------------------------------------------------------------------
begin
  ListBox1.Clear;
end;

procedure TFindForm.ListBox1Click(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for listbox that displays adjacent links/nodes
// to the found node/link. Highlights the selected object both on
// the map and in the Data Browser.
//-----------------------------------------------------------------------------
var
  s: String;
  Found: Boolean;
begin
  // Get ID of adjacent object selected
  with ListBox1 do
    s := Items[ItemIndex];

  // Search for object in Subcatchment, Node or Link database
  if ComboBox1.ItemIndex = 0 then
  begin
    Found := Project.FindSubcatch(s, FoundObject, FoundIndex);
    if not Found then Found := Project.FindNode(s, FoundObject, FoundIndex);
  end
  else if ComboBox1.ItemIndex = 1 then
    Found := Project.FindLink(s, FoundObject, FoundIndex)
  else
    Found := Project.FindNode(s, FoundObject, FoundIndex);

  // If object found then highlight it on the map and
  // make it the current item shown in the Browser.
  if Found then
  begin
    UpdateMapDisplay;
    Ubrowser.BrowserUpdate(FoundObject, FoundIndex)
  end;
end;

procedure TFindForm.SearchFor(const ObjType: Integer; const ObjIndex: Integer;
  ObjName: String = '');
//-----------------------------------------------------------------------------
// Loads a specific object into the dialog when the item is selected from
// the MainForm's Browser panel.
//-----------------------------------------------------------------------------
begin
  if      Project.IsSubcatch(ObjType) then ComboBox1.ItemIndex := 0
  else if Project.IsNode(ObjType) then ComboBox1.ItemIndex := 1
  else if Project.IsLink(ObjType) then ComboBox1.ItemIndex := 2
  else Exit;
  if Length(ObjName) = 0
  then Edit1.Text := Project.GetID(ObjType, ObjIndex)
  else Edit1.Text := ObjName;
end;

end.
