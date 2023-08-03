unit Dproselect;

{-------------------------------------------------------------------}
{                    Unit:    Dproselect.pas                        }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Stay-on-top form unit used to define the links appearing in     }
{   a profile plots.                                                }
{-------------------------------------------------------------------}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.Buttons, System.UITypes, Contnrs,
  Uglobals, Uproject, Uutils;

type
  TProfileSelectForm = class(TForm)
    GroupBox1: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    StartNodeEdit: TEdit;
    StartNodeBtn: TBitBtn;
    EndNodeEdit: TEdit;
    EndNodeBtn: TBitBtn;
    FindPathBtn: TButton;
    UseProfileBtn: TButton;
    SaveProfileBtn: TButton;
    ProfileLinksBox: TGroupBox;
    BtnAddLink: TBitBtn;
    BtnDelLink: TBitBtn;
    BtnLinkUp: TBitBtn;
    BtnLinkDown: TBitBtn;
    LinksListBox: TListBox;
    BtnClearLinks: TBitBtn;
    BtnOK: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    procedure FormCreate(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure StartEndNodeEditChange(Sender: TObject);
    procedure StartEndNodeBtnClick(Sender: TObject);
    procedure BtnAddLinkClick(Sender: TObject);
    procedure BtnDelLinkClick(Sender: TObject);
    procedure BtnLinkDownClick(Sender: TObject);
    procedure BtnLinkUpClick(Sender: TObject);
    procedure BtnClearLinksClick(Sender: TObject);
    procedure FindPathBtnClick(Sender: TObject);
    procedure SaveProfileBtnClick(Sender: TObject);
    procedure UseProfileBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BtnHelpClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    procedure CreateAdjLists;
    procedure CreatePlot;
    procedure FreeAdjLists;
    procedure GetMinPath(StartNode: TNode; EndNode: TNode;
              NodeStack: TQueue; LinkStack: TStack);
    function  CheckProfile: Integer;
  public
    { Public declarations }
  end;

var
  ProfileSelectForm: TProfileSelectForm;

implementation

{$R *.dfm}

uses
  Dprofile, Fmain;

const
  MSG_NO_ITEMS = 'No objects to display';
  MSG_NO_LINK_SELECTED = 'Must select a link.';
  MSG_START_NODE_NOT_EXIST = 'Starting node does not exist.';
  MSG_END_NODE_NOT_EXIST = 'Ending node does not exist.';
  MSG_NO_VALID_PATH = 'Cannot find a path between start and end nodes.';
  MSG_PROFILE_EXISTS = 'A profile with that name already exists.';
  TXT_LINKS_IN_PROFILE = 'Links in Profile';
  TXT_PROFILE_NAME = 'Profile Name';


procedure TProfileSelectForm.FormCreate(Sender: TObject);
begin
  Left := MainForm.Left + (MainForm.Width - MainForm.ClientWidth) - 2;
  Top := MainForm.Top + (MainForm.Height-MainForm.ClientHeight) - 2;
  UseProfileBtn.Enabled := (Project.ProfileNames.Count > 0);
  SaveProfileBtn.Enabled := False;
  with MainForm.ProjectImageList do
  begin
    GetBitmap(GetIndexByName('blue_plus'), StartNodeBtn.Glyph);
    GetBitmap(GetIndexByName('blue_plus'), EndNodeBtn.Glyph);
    GetBitmap(GetIndexByName('blue_plus'), BtnAddLink.Glyph);
    GetBitmap(GetIndexByName('blue_minus'), BtnDelLink.Glyph);
    GetBitmap(GetIndexByName('uparrow2'), BtnLinkUp.Glyph);
    GetBitmap(GetIndexByName('dnarrow2'), BtnLinkDown.Glyph);
    GetBitmap(GetIndexByName('delete'), BtnClearLinks.Glyph);
  end;

end;

procedure TProfileSelectForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TProfileSelectForm.BtnOKClick(Sender: TObject);
var
  I: Integer;
begin
  if LinksListBox.Items.Count <= 0 then
  begin
    Uutils.MsgDlg(MSG_NO_ITEMS, mtError, [mbOK]);
    Exit;
  end;
  I := CheckProfile;
  if I >= 0 then
  begin
    Uutils.MsgDlg('Link ' + LinksListBox.Items[I] +
               ' either does not exist or is not connected' +
               ' to the preceding link.', mtError, [mbOK]);
    Exit;
  end;
  Hide;
  CreatePlot;
  Close;
end;

procedure TProfileSelectForm.BtnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TProfileSelectForm.StartEndNodeEditChange(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnChange handler for the Start/End Node edit box.
//-----------------------------------------------------------------------------
begin
  ProfileLinksBox.Caption := TXT_LINKS_IN_PROFILE;
  LinksListBox.Clear;
end;

procedure TProfileSelectForm.StartEndNodeBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the Start/End Node button. Makes the currently
//  selected node in the Data Browser the starting/ending node for the
//  profile plot.
//-----------------------------------------------------------------------------
var
  S: String;
begin
  with Project do
  begin
    if IsNode(CurrentList) then
    begin
      S := GetID(CurrentList, CurrentItem[CurrentList]);
      if Sender = StartNodeBtn then StartNodeEdit.Text := S;
      if Sender = EndNodeBtn then  EndNodeEdit.Text := S;
    end;
  end;
end;


procedure TProfileSelectForm.BtnAddLinkClick(Sender: TObject);
var
  I: Integer;
begin
  if not Project.IsLink(CurrentList) then
  begin
    Uutils.MsgDlg(MSG_NO_LINK_SELECTED, mtError, [mbOK]);
    Exit;
  end;
  I := LinksListBox.ItemIndex + 1;
  with Project do
  begin
    if CurrentItem[CurrentList] >= 0 then with LinksListBox do
    begin
      Items.Insert(I, GetID(CurrentList, CurrentItem[CurrentList]));
      ItemIndex := I;
    end;
  end;
  SaveProfileBtn.Enabled := LinksListBox.Items.Count > 0;
end;

procedure TProfileSelectForm.BtnDelLinkClick(Sender: TObject);
var
  I: Integer;
begin
  with LinksListBox do
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
  SaveProfileBtn.Enabled := LinksListBox.Items.Count > 0;
end;

procedure TProfileSelectForm.BtnLinkDownClick(Sender: TObject);
begin
  with LinksListBox do
  begin
    if ItemIndex < Items.Count-1 then Items.Exchange(ItemIndex,ItemIndex+1);
  end;
end;

procedure TProfileSelectForm.BtnLinkUpClick(Sender: TObject);
begin
  with LinksListBox do
  begin
    if ItemIndex > 0 then Items.Exchange(ItemIndex,ItemIndex-1);
  end;
end;

procedure TProfileSelectForm.BtnClearLinksClick(Sender: TObject);
begin
  LinksListBox.Clear;
end;

procedure TProfileSelectForm.CreatePlot;
var
  ReportSelection: TReportSelection;
begin
  with ReportSelection do
  begin
    StartDateIndex := 0;
    EndDateIndex   := 0;
    ObjectType     := LINKS;
    Items          := LinksListBox.Items;
    ItemCount      := LinksListBox.Items.Count;
    VariableCount  := 0;
    ReportType     := PROFILEPLOT;
  end;
  MainForm.CreateReport(ReportSelection);
end;


procedure TProfileSelectForm.FindPathBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the Find Path button. Finds a path of links between
//  the start and end nodes specified for the profile plot.
//-----------------------------------------------------------------------------
var
  PathStack:  TQueue;   // Delphi's queue & stack class are
  FinalStack: TStack;   // defined in the Contrns unit
  StartNode:  TNode;
  EndNode:    TNode;
  aLink:      TLink;
  I, J:       Integer;
begin
  // Get the node objects associated with the names of the start & end nodes
  if not Project.FindNode(StartNodeEdit.Text, I, J) then
  begin
    Uutils.MsgDlg(MSG_START_NODE_NOT_EXIST, mtError, [mbOK]);
    exit;
  end;
  StartNode := Project.GetNode(I, J);
  if not Project.FindNode(EndNodeEdit.Text, I, J) then
  begin
    Uutils.MsgDlg(MSG_END_NODE_NOT_EXIST, mtError, [mbOK]);
    exit;
  end;
  EndNode := Project.GetNode(I, J);

  // Populate link adjacency lists for each node of the project
  CreateAdjLists;
  LinksListBox.Clear;
  SaveProfileBtn.Enabled := False;

  // Create two stack objects to use in finding the profile plot path
  PathStack := TQueue.Create();
  FinalStack := TStack.Create();

  try
    // Trace a path between the start and end nodes
    GetMinPath(StartNode, EndNode, PathStack, FinalStack);

    // Check that a path was found
    if FinalStack.Count = 0
    then Uutils.MsgDlg(MSG_NO_VALID_PATH, mtError, [mbOK])

    // Unwind the FinalStack to extract the links on the path and add
    // their ID's to the Links On Path list box
    else
    begin
      while FinalStack.Count > 0 do
      begin
        aLink := FinalStack.Pop;
        //LinksListBox.Lines.Add(aLink.ID);
        LinksListBox.Items.Add(aLink.ID);
      end;
      SaveProfileBtn.Enabled := True;
    end;
  finally
    FreeAdjLists;
    PathStack.Free;
    FinalStack.Free;
  end;
end;


procedure TProfileSelectForm.CreateAdjLists;
//-----------------------------------------------------------------------------
//  Loops through each link in the project, adding it to the link adjacency
//  list of the node on either end.
//-----------------------------------------------------------------------------
var
  K, L:   Integer;
  aLink:  TLink;
  aNode:  TNode;
begin
  for K := 0 to MAXCLASS do
  begin
    if Project.IsLink(K) then
    begin
      for L := 0 to Project.Lists[K].Count-1 do
      begin
        aLink := Project.GetLink(K,L);
        aLink.Marked := False;
        aNode := aLink.Node1;
        aNode.AdjList.Add(aLink);
        aNode := aLink.Node2;
        aNode.AdjList.Add(aLink);
      end;
    end;
  end;
end;


procedure TProfileSelectForm.FreeAdjLists;
//-----------------------------------------------------------------------------
//  Clears the link adjacency list for each node of the project.
//-----------------------------------------------------------------------------
var
  I, J: Integer;
  aNode: TNode;
begin
  for I := 0 to MAXCLASS do
  begin
    if Project.IsNode(I) then
    begin
      for J := 0 to Project.Lists[I].Count-1 do
      begin
        aNode := Project.GetNode(I,J);
        aNode.AdjList.Clear;
      end;
    end;
  end;
end;


procedure TProfileSelectForm.GetMinPath(StartNode: TNode; EndNode: TNode;
  NodeStack: TQueue; LinkStack: TStack);
//-----------------------------------------------------------------------------
//  Finds a path between StartNode and EndNode that consist of the
//  fewest links.
//-----------------------------------------------------------------------------
var
  I, J, L  : Integer;
  N1, N2   : TNode;
  aLink    : TLink;
  MovedBack: Boolean;
begin
  // Assign a high path length to each node
  for I := 0 to MAXCLASS do
  begin
    if Project.IsNode(I) then
    begin
      for J := 0 to Project.Lists[I].Count-1 do
        Project.GetNode(I,J).PathLen := MAXINT;
    end;
  end;

  // Place the start node on the node stack
  StartNode.PathLen := 0;
  NodeStack.Push(StartNode);

  // Repeat until the node stack is empty
  while NodeStack.Count > 0 do
  begin

    // Remove the top node from the stack and examine each connecting link
    N1 := NodeStack.Pop;
    for I := 0 to N1.AdjList.Count-1 do
    begin

      // Mark the link and find the node at its other end
      aLink := N1.AdjList.Items[I];
      if aLink.Marked then continue;
      aLink.Marked := true;
      N2 := aLink.Node2;
      if N1 = N2 then N2 := aLink.Node1;

      // Update the path length to that node
      L := N1.PathLen + 1;

      // If a shorter path results, place the node on the stack
      if L < N2.PathLen then
      begin
        if (N2.PathLen = MAXINT) and (N2 <> EndNode) then NodeStack.Push(N2);
        N2.PathLen := L;
      end;
    end;
  end;

  // If the end node was never reached then exit
  if EndNode.PathLen = MAXINT then Exit;

  // Start with the end node and trace back to the start node
  N1 := EndNode;
  while N1 <> StartNode do
  begin

    // Examine each link connected to the current node
    MovedBack := false;
    for I := 0 to N1.AdjList.Count-1 do
    begin

      // Unmark the link
      aLink := N1.AdjList.Items[I];
      if not aLink.Marked then continue;
      aLink.Marked := false;

      // Identify the node at the other end of the link
      N2 := aLink.Node2;
      if N2 = N1 then N2 := aLink.Node1;

      // If this node is on the shortest path then place the
      // link on the link stack and make this node the next one
      // to examine
      if N2.PathLen = N1.PathLen - 1 then
      begin
        LinkStack.Push(aLink);
        N1 := N2;
        MovedBack := true;
        break;
      end;
    end;
    if not MovedBack then break;
  end;
end;


procedure TProfileSelectForm.SaveProfileBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the Save Profile button. Saves the current profile
//  path to the project's list of profile paths.
//-----------------------------------------------------------------------------
var
  S: String;
  I: Integer;
begin
  S := InputBox(TXT_PROFILE_NAME, '', '');
  if Project.ProfileNames.Find(S, I)
  then MessageDlg(MSG_PROFILE_EXISTS, mtError, [mbOK], 0)
  else
  begin
    Project.ProfileNames.Add(S);
    ProfileLinksBox.Caption := S;
    with LinksListBox do
    begin
      S := Items[0];
      for I := 1 to Items.Count-1 do
        S := S + #13 + Items[I];
    end;
    Project.ProfileLinks.Add(S);
    SaveProfileBtn.Enabled := False;
    UseProfileBtn.Enabled := True;
    Uglobals.HasChanged := True;
  end;
end;


procedure TProfileSelectForm.UseProfileBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the Use Profile button. Retrieves one of the
//  project's previously saved profile paths.
//-----------------------------------------------------------------------------
var
  ProfileIndex: Integer;
  ProfileSelectionForm : TProfileSelectionForm;
begin
  ProfileIndex := -1;
  if Project.ProfileNames.Count > 0 then
  begin
    ProfileSelectionForm := TProfileSelectionForm.Create(self);
    try
      if ProfileSelectionForm.ShowModal = mrOK
      then ProfileIndex := ProfileSelectionForm.SelectedProfile;
    finally
      ProfileSelectionForm.Free;
    end;
  end;
  if ProfileIndex >= 0 then
  begin
    StartNodeEdit.Text := '';
    EndNodeEdit.Text := '';
    ProfileLinksBox.Caption := Project.ProfileNames[ProfileIndex];
    LinksListBox.Clear;
    LinksListBox.Items.SetText(PChar(Project.ProfileLinks[ProfileIndex]));
    SaveProfileBtn.Enabled := False;
  end;
  if Project.ProfileNames.Count = 0 then UseProfileBtn.Enabled := False;
end;


function TProfileSelectForm.CheckProfile: Integer;
//-----------------------------------------------------------------------------
//  Checks that the links selected for a profile plot form a connected path.
//-----------------------------------------------------------------------------
var
  I, J, K: Integer;
  Link1: TLink;
  Link2: TLink;
  LastNode: TNode;
begin
  Result := 0;
  with LinksListBox do
  begin

  //Check that first two links are connected
    if not Project.FindLink(LinksListBox.Items[0], I, J) then Exit;
    if Items.Count = 1 then
    begin
      Result := -1;
      Exit;
    end;
    Result := 1;
    Link1 := Project.GetLink(I, J);
    if not Project.FindLink(LinksListBox.Items[1], I, J) then Exit;
    Link2 := Project.GetLink(I, J);
    if (Link2.Node1 = Link1.Node1) or (Link2.Node1 = Link1.Node2)
    then LastNode := Link2.Node2
    else if (Link2.Node2 = Link1.Node1) or (Link2.Node2 = Link1.Node2)
    then LastNode := Link2.Node1
    else Exit;

  //Check that remaining links are connected
    for K := 2 to Items.Count-1 do
    begin
      Result := K;
      if not Project.FindLink(LinksListBox.Items[K], I, J) then Exit;
      Link2 := Project.GetLink(I, J);
      if Link2.Node1 = LastNode then LastNode := Link2.Node2
      else if Link2.Node2 = LastNode then LastNode := Link2.Node1
      else Exit;
    end;
    Result := -1;
  end;
end;

procedure TProfileSelectForm.BtnHelpClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 211370);
end;

procedure TProfileSelectForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then BtnHelpClick(Sender);
end;

end.
