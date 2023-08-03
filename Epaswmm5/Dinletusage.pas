unit Dinletusage;

{-------------------------------------------------------------------}
{                    Unit:    Dinletusage.pas                       }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form used to assign an Inlet design to a conduit and     }
{   designate to which node the inlet's captured flow is sent.      }
{-------------------------------------------------------------------}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Buttons, System.ImageList, Vcl.ImgList,
  Vcl.VirtualImageList, Vcl.BaseImageCollection, Vcl.ImageCollection,
  Uproject, Uglobals, Uutils, PropEdit;

type
  TInletUsageForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Image1: TImage;
    ImageCollection1: TImageCollection;
    VirtualImageList1: TVirtualImageList;
    Panel3: TPanel;
    CancelBtn: TButton;
    OkBtn: TButton;
    Panel4: TPanel;
    HintLabel: TLabel;
    HelpBtn: TButton;
    procedure OkBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure HelpBtnClick(Sender: TObject);
  private
    { Private declarations }
    PropEdit1: TPropEdit;
    PropList: TStringlist;
    LinkIndex: Integer;
    Modified: Boolean;
    function GetData(aNode: TNode): String;
    procedure ShowPropertyHint(Sender: TObject; aRow: LongInt);
  public
    { Public declarations }
    procedure SetData(Index: Integer; InletDesigns: String);
    procedure SetReceiverNode(ObjType: Integer; ObjIndex: Integer);
  end;

var
  InletUsageForm: TInletUsageForm;

implementation

{$R *.dfm}

uses
  Fmain, Fmap, Fproped, Ubrowser, Uedit, Uinlet;

const
  TXT_PROPERTY = 'Property';
  TXT_VALUE = 'Value';

  PropNames: array[0..7] of String =
    ('Inlet Structure',
     'Capture Node',
     'Number of Inlets',
     'Percent Clogged',
     'Flow Restriction',
     'Depression Height',
     'Depression Width',
     'Inlet Placement');

  DefaultProps: String = ''#13''#13'1'#13'0'#13'0'#13'0'#13'0'#13'AUTOMATIC';

  PropHints: array[0..7] of String =
    ('Name of inlet structure to use.'#13 +
     'Select blank entry to remove inlet.',
     'Node that receives flow captured by the inlet. ' +
     'Click on Map or Browser to select it.',
     'Number of inlets placed on each side of street or in channel.',
     'Percentage to which the inlet is clogged.',
     'Maximum flow that can be captured per inlet (flow units). ' +
     'Enter 0 for no flow restriction.',
     'Height of local gutter depression (ft or m). ' +
     'Enter 0 for no local depression.',
     'Width of local gutter depression (ft or m). ' +
     'Enter 0 for no local depression.',
     'Whether inlet is placed on-grade or on-sag.' + #10 +
     'AUTOMATIC lets network topography decide.');

  Inlet_Number: String = '1'#13'2'#13'3'#13'4'#13'5';
  Inlet_Placement: String = 'AUTOMATIC'#13'ON_GRADE'#13'ON_SAG';

var
  InletProps: array[0..7] of TPropRecord;
  TheLink: TLink;
  TheNode: TNode;
  LinkID: String;
  TheUsedInlet: TInletUsage;

//==============================================================================

procedure TInletUsageForm.OkBtnClick(Sender: TObject);
var
  S: String;
  aNode: TNode;
begin
  // No current inlet usage & inlet name supplied
  S := Trim(PropList[0]);
  if (Length(S) = 0) and (TheUsedInlet = nil) then Close
  else
  begin
    // Invalid capture node
    PropEdit1.EndEditing;
    S := Trim(PropList[1]);
    aNode := Project.GetNode(S);
    if aNode = nil then
    begin
      Uutils.MsgDlg('Invalid capture node.', mtError, [mbOK]);
      PropEdit1.Edit;
      PropEdit1.SetFocus;
    end
    else
    begin
      // Transfer form's data to inlet usage object
      PropEdit1.EndEditing;
      if self.Modified or PropEdit1.Modified then MainForm.SetChangeFlags;
      S := GetData(aNode);
      TheLink.Data[CONDUIT_INLET_INDEX] := S;
      Close;
    end;
  end;
end;

//==============================================================================

procedure TInletUsageForm.CancelBtnClick(Sender: TObject);
begin
  Close;
end;

//==============================================================================

procedure TInletUsageForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caHide;
  Ubrowser.BrowserUpdate(CONDUIT, LinkIndex);
  Uedit.EditObject(CONDUIT);
end;

//==============================================================================

procedure TInletUsageForm.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  // Load inlet usage image
  VirtualImageList1.GetIcon(0, Image1.Picture.Icon);

// Create a Property Editor
  PropEdit1 := TPropEdit.Create(self);
  with PropEdit1 do
  begin
    Parent := Panel2;
    Align := alClient;
    BorderStyle := bsNone;
    ColHeading1 := TXT_PROPERTY;
    ColHeading2 := TXT_VALUE;
    HeaderSplit := 50;
    ValueColor := clNavy;
    OnRowSelect := ShowPropertyHint;
  end;

  // Create a Property stringlist
  PropList := TStringlist.Create;

  // Initialize contents of each property record
  for I := 0 to High(PropNames) do
    InletProps[I].Name   := PropNames[I];

  InletProps[0].Style := esComboList;
  InletProps[0].Mask := emNoSpace;
  InletProps[0].List := '';

  InletProps[1].Style  := esEdit;
  InletProps[1].Mask   := emNoSpace;
  InletProps[1].Length := 0;

  InletProps[2].Style := esComboList;
  InletProps[2].List := Inlet_Number;

  for I := 3 to 6 do
  begin
    InletProps[I].Style  := esEdit;
    InletProps[I].Mask   := emPosNumber;
    InletProps[I].Length := 0;
  end;

  InletProps[7].Style := esComboList;
  InletProps[7].List := Inlet_Placement;

end;

//==============================================================================

procedure TInletUsageForm.FormDestroy(Sender: TObject);
begin
  PropList.Free;
  PropEdit1.Free;
end;

//==============================================================================

procedure TInletUsageForm.FormShow(Sender: TObject);
begin
  Left := PropEditForm.Left;
  Top := PropEditForm.Top;
  PropEdit1.SetProps(InletProps, PropList);
  PropEdit1.Edit;
end;

//==============================================================================

procedure TInletUsageForm.ShowPropertyHint(Sender: TObject; aRow: LongInt);
begin
  HintLabel.Caption := PropHints[aRow];
end;

//==============================================================================

procedure TInletUsageForm.SetData(Index: Integer; InletDesigns: String);
var
  I: Integer;
  S: String;
begin
  PropList.Clear;
  LinkIndex := Index;
  LinkID := Project.Lists[CONDUIT].Strings[Index];
  Caption := 'Inlet for Conduit ' + LinkID;
  TheLink := TLink(Project.Lists[CONDUIT].Objects[Index]);
  TheUsedInlet := nil;
  TheNode := nil;
  InletProps[0].List := InletDesigns;

  if TheLink.Inlet = nil then
    PropList.Text := DefaultProps

  else
  begin
    TheUsedInlet := TInletUsage(TheLink.Inlet);
    if TheUsedInlet.Inlet <> nil
    then S := TheUsedInlet.Inlet.GetID
    else S := '';
    PropList.Add(S);

    if TheUsedInlet.InletNode <> nil then
    begin
      S := TheUsedInlet.InletNode.ID;
      theNode := TheUsedInlet.InletNode;
    end
    else S := '';
    PropList.Add(S);

    for I := 0 to USAGE_MAX_INDEX do
      PropList.Add(TheUsedInlet.Data[I]);
  end;
  Modified := false;
end;

//==============================================================================

procedure TInletUsageForm.SetReceiverNode(ObjType: Integer; ObjIndex: Integer);
begin
  with PropEdit1 do
  begin
    if Row = 1 then
    begin
      EndEditing;
      SetProp(1, Project.GetID(ObjType, ObjIndex));
      self.Modified := true;
      Edit;
    end;
  end;
end;

//==============================================================================

function TInletUsageForm.GetData(aNode:TNode): String;
var
  I: Integer;
  RedrawMap: Boolean;
begin
  RedrawMap := false;
  for I := 0 to PropList.Count-1 do PropList[I] := Trim(PropList[I]);

  if Length(PropList[0]) = 0 then
  begin
    if TheUsedInlet <> nil then
    begin
      TheUsedInlet.Free;
      RedrawMap := true;
    end;
    TheLink.Inlet := nil;
    Result := 'NO';
    if RedrawMap then MapForm.RedrawMap;
    exit;
  end;

  if TheUsedInlet = nil then
  begin
    TheUsedInlet := TInletUsage.Create;
    TheLink.Inlet := TClass(TheUsedInlet);
    RedrawMap := true;
  end;

  I := Project.Lists[INLET].IndexOf(PropList[0]);
  TheUsedInlet.Inlet := TInlet(Project.Lists[INLET].Objects[I]);
  TheUsedInlet.InletNode := aNode;
  if aNode <> TheNode then RedrawMap := true;
  for I := 2 to PropList.Count-1 do
  begin
    if Length(PropList[I]) = 0 then PropList[I] := '0';
    TheUsedInlet.Data[I-2] := PropList[I];
  end;

  if RedrawMap then MapForm.RedrawMap;
  Result := 'YES';
end;

//==============================================================================

procedure TInletUsageForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then HelpBtnClick(Sender);
end;

procedure TInletUsageForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 213610);
end;

end.

