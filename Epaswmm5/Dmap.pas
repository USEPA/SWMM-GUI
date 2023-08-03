unit Dmap;

{-------------------------------------------------------------------}
{                    Unit:    Dmap.pas                              }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form unit for changing Study Area Map display options.   }
{                                                                   }
{   The form consists of a listbox on the left and a notebook       }
{   next to it. The notebook has 8 pages, one for each category     }
{   of map display options, which are selected from the listbox.    }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, ExtCtrls, ComCtrls, Math, UpDnEdit, Grids, CheckLst,
  Vcl.Themes, Uglobals, Umap;

type
  TMapOptionsForm = class(TForm)
    BtnOK: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    Panel1: TPanel;
    Notebook1: TNotebook;
    NodesBySize: TCheckBox;
    NodeBorder: TCheckBox;
    GroupBox2: TGroupBox;
    NodeShape: TShape;
    LinksBySize: TCheckBox;
    GroupBox3: TGroupBox;
    LinkShape: TShape;
    Label4: TLabel;
    Label7: TLabel;
    NotationTransparent: TCheckBox;
    Label3: TLabel;
    LinkSymbols: TCheckBox;
    Label6: TLabel;
    Label8: TLabel;
    LinkArrows: TRadioGroup;
    ListBox1: TListBox;
    LabelsTransparent: TCheckBox;
    Label1: TLabel;
    SubcatchFill: TRadioGroup;
    SubcatchLink: TCheckBox;
    Label2: TLabel;
    LinkBorder: TCheckBox;
    Label10: TLabel;
    NodeSymbols: TCheckBox;
    NotationListBox: TCheckListBox;
    SubcatchSymbol: TUpDnEditBox;
    SubcatchLine: TUpDnEditBox;
    NodeSpin: TUpDnEditBox;
    LinkSpin: TUpDnEditBox;
    ZoomForLabels: TUpDnEditBox;
    NotationFontSize: TUpDnEditBox;
    ZoomForNotation: TUpDnEditBox;
    ZoomForSymbols: TUpDnEditBox;
    ArrowSpin: TUpDnEditBox;
    ZoomForArrows: TUpDnEditBox;
    Label9: TLabel;
    Label5: TLabel;
    ColorListBox1: TColorListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure NodeSpinChange(Sender: TObject);
    procedure LinkSpinChange(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure NodeBorderClick(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox1DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ColorListBox1GetColors(Sender: TCustomColorListBox;
      Items: TStrings);
  private
    { Private declarations }
    procedure ResizeNodeShape;
    procedure ResizeLinkShape;
    procedure UpdateBoolean(var Value: Boolean; const NewValue: Boolean);
    procedure UpdateInteger(var Value: Integer; const NewValue: Integer);
  public
    { Public declarations }
    HasChanged: Boolean;
    procedure GetActivePage(var aPage: Integer);
    procedure SetOptions(const Options: TMapOptions);
    procedure SetActivePage(aPage: Integer);
    procedure GetOptions(var Options: TMapOptions);
  end;

//var
//  MapOptionsForm: TMapOptionsForm;

implementation

{$R *.DFM}

procedure TMapOptionsForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
// OnCreate handler for form.
//----------------------------------------------------------------------------
begin
  with ListBox1 do
  begin
    ItemHeight := (ClientHeight) div Items.Count;
    ItemIndex := 0;
  end;
  ListBox1Click(Sender);
end;

procedure TMapOptionsForm.ColorListBox1GetColors(Sender: TCustomColorListBox;
  Items: TStrings);
begin
  Items.Clear;
  Items.AddObject('White', TObject(Uglobals.MapBackColor[1]));
  Items.AddObject('Yellow', TObject(Uglobals.MapBackColor[2]));
  Items.AddObject('Blue', TObject(Uglobals.MapBackColor[3]));
  Items.AddObject('Panel', TObject(Uglobals.MapBackColor[4]));
  Items.AddObject('Black', TObject(Uglobals.MapBackColor[5]));
  Items.AddObject('Cyan', TObject(Uglobals.MapBackColor[6]));
  Items.AddObject('Green', TObject(Uglobals.MapBackColor[7]));
  Items.AddObject('Pink', TObject(Uglobals.MapBackColor[8]));
end;

procedure TMapOptionsForm.SetOptions(const Options: TMapOptions);
//-----------------------------------------------------------------------------
// Loads current map display options into the form.
//-----------------------------------------------------------------------------
var
  I: Integer;
  Bstyle: TBrushStyle;
begin
  with Options do
  begin
    SubcatchLink.Checked := ShowSubcatchLinks;
    Bstyle := TBrushStyle(SubcatchFillStyle);
    if      Bstyle = bsDiagCross then I := 3
    else if Bstyle = bsBDiagonal then I := 2
    else if Bstyle = bsSolid     then I := 1
    else                              I := 0;
    SubcatchFill.ItemIndex := I;
    SubcatchSymbol.Spinner.Position := SubcatchSize;
    SubcatchLine.Spinner.Position := SubcatchLineSize;

    NodeSpin.Spinner.Position := NodeSize;
    NodesBySize.Checked := ShowNodesBySize;
    NodeBorder.Checked := ShowNodeBorder;

    LinkSpin.Spinner.Position := LinkSize;
    LinksBySize.Checked := ShowLinksBySize;
    LinkBorder.Checked := ShowLinkBorder;

    NodeSymbols.Checked := ShowNodeSymbols;
    LinkSymbols.Checked := ShowLinkSymbols;

    LinkArrows.ItemIndex := Ord(ArrowStyle);
    ArrowSpin.Spinner.Position := ArrowSize;

    LabelsTransparent.Checked := LabelsTranspar;

    with NotationListBox do
    begin
      Checked[0] := ShowGageIDs;
      Checked[1] := ShowSubcatchIDs;
      Checked[2] := ShowNodeIDs;
      Checked[3] := ShowLinkIDs;
      Checked[4] := ShowSubcatchValues;
      Checked[5] := ShowNodeValues;
      Checked[6] := ShowLinkValues;
    end;

    NotationTransparent.Checked := NotationTranspar;
    NotationFontSize.Spinner.Position := NotationSize;

    ZoomForLabels.Spinner.Position := LabelZoom;
    ZoomForSymbols.Spinner.Position := SymbolZoom;
    ZoomForArrows.Spinner.Position := ArrowZoom;
    ZoomForNotation.Spinner.Position := NotationZoom;

    ColorListBox1.ItemIndex := ColorIndex - 1;
  end;
  ResizeNodeShape;
  ResizeLinkShape;
  NodeBorderClick(self);
  NodeSymbols.Enabled := not QueryFlag;
  HasChanged := False;
end;

procedure TMapOptionsForm.GetOptions(var Options: TMapOptions);
//-----------------------------------------------------------------------------
// Unloads contents of form into map display options.
//-----------------------------------------------------------------------------
var
  I : Integer;
  Astyle: TArrowStyle;
begin
  with Options do
  begin
    UpdateBoolean(ShowSubcatchLinks, SubcatchLink.Checked);
    I := 0;
    case SubcatchFill.ItemIndex of
    0: I := Ord(bsClear);
    1: I := Ord(bsSolid);
    2: I := Ord(bsBDiagonal);
    3: I := Ord(bsDiagCross);
    end;
    UpdateInteger(SubcatchFillStyle, I);
    UpdateInteger(SubcatchSize, SubcatchSymbol.Spinner.Position);
    UpdateInteger(SubcatchLineSize, SubcatchLine.Spinner.Position);

    UpdateInteger(NodeSize, NodeSpin.Spinner.Position);
    UpdateBoolean(ShowNodesBySize, NodesBySize.Checked);
    UpdateBoolean(ShowNodeBorder,NodeBorder.Checked);

    UpdateInteger(LinkSize, LinkSpin.Spinner.Position);
    UpdateBoolean(ShowLinksBySize, LinksBySize.Checked);
    UpdateBoolean(ShowLinkBorder, LinkBorder.Checked);

    UpdateBoolean(ShowNodeSymbols, NodeSymbols.Checked);
    UpdateBoolean(ShowLinkSymbols, LinkSymbols.Checked);

    Astyle := TArrowStyle(LinkArrows.ItemIndex);
    if Astyle <> ArrowStyle then HasChanged := true;
    ArrowStyle := Astyle;
    UpdateInteger(ArrowSize, ArrowSpin.Spinner.Position);

    UpdateBoolean(LabelsTranspar, LabelsTransparent.Checked);

    with NotationListBox do
    begin
      ShowGageIDs := Checked[0];
      ShowSubcatchIDs := Checked[1];
      ShowNodeIDs := Checked[2];
      ShowLinkIDs := Checked[3];
      ShowSubcatchValues := Checked[4];
      ShowNodeValues := Checked[5];
      ShowLinkValues := Checked[6];
    end;
    UpdateBoolean(NotationTranspar, NotationTransparent.Checked);
    UpdateInteger(NotationSize, NotationFontSize.Spinner.Position);

    LabelZoom := ZoomForLabels.Spinner.Position;
    SymbolZoom := ZoomForSymbols.Spinner.Position;
    ArrowZoom := ZoomForArrows.Spinner.Position;
    NotationZoom := ZoomForNotation.Spinner.Position;

    ColorIndex := ColorListBox1.ItemIndex + 1;
  end;
end;

procedure TMapOptionsForm.UpdateBoolean(var Value: Boolean;
  const NewValue: Boolean);
//-----------------------------------------------------------------------------
// Utility function which changes the value of a boolean variable.
//-----------------------------------------------------------------------------
begin
  if Value <> NewValue then HasChanged := true;
  Value := NewValue;
end;

procedure TMapOptionsForm.UpdateInteger(var Value: Integer;
  const NewValue: Integer);
//-----------------------------------------------------------------------------
// Utility function which changes the value of an integer variable.
//-----------------------------------------------------------------------------
begin
  if Value <> NewValue then HasChanged := true;
  Value := NewValue;
end;

procedure TMapOptionsForm.GetActivePage(var aPage: Integer);
//-----------------------------------------------------------------------------
// Retrieves index of notebook page of options currently displayed.
//-----------------------------------------------------------------------------
begin
  aPage := ListBox1.ItemIndex;
end;

procedure TMapOptionsForm.SetActivePage(aPage: Integer);
//-----------------------------------------------------------------------------
// Displays a specific page of map options.
//-----------------------------------------------------------------------------
begin
  with ListBox1 do
  begin
    if aPage < Items.Count then
    begin
      ItemIndex := aPage;
      ListBox1Click(Self);
    end;
  end;
end;

procedure TMapOptionsForm.ListBox1Click(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the option category listbox.
//-----------------------------------------------------------------------------
begin
  NoteBook1.PageIndex := ListBox1.ItemIndex;
end;

procedure TMapOptionsForm.ListBox1DrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
//-----------------------------------------------------------------------------
// OnDrawItem handler for the item category listbox.
// Centers the text within the item's drawing rectangle.
//-----------------------------------------------------------------------------
var
  ht: Integer;
  dy: Integer;
  s:  String;
begin
  with Control as TListBox do
  begin
    s := Items[Index];
    ht := Canvas.TextHeight(s);
    Canvas.FillRect(Rect);
    dy := (Rect.Bottom - Rect.Top - ht) div 2;
    Canvas.TextOut(0,Rect.Top+dy,s);
  end;
end;

procedure TMapOptionsForm.NodeSpinChange(Sender: TObject);
//-----------------------------------------------------------------------------
// OnChange handler for Node Size SpinEdit control.
//-----------------------------------------------------------------------------
begin
  ResizeNodeShape;
end;

procedure TMapOptionsForm.ResizeNodeShape;
//-----------------------------------------------------------------------------
// Resizes the NodeShape control.
//-----------------------------------------------------------------------------
var
  newsize : Integer;
  aRect   : TRect;
begin
  newsize := 3*NodeSpin.Spinner.Position+2;
  aRect := NodeShape.BoundsRect;
  aRect.Top := NodeSpin.Top + (NodeSpin.Height div 2)
               - (newsize div 2);
  aRect.Bottom := aRect.Top + newsize;// + 1;
  aRect.Right := aRect.Left + newsize;// + 1;
  NodeShape.BoundsRect := aRect;
end;

procedure TMapOptionsForm.LinkSpinChange(Sender: TObject);
//-----------------------------------------------------------------------------
// OnChange handler for Link Size SpinEdit control.
//-----------------------------------------------------------------------------
begin
  ResizeLinkShape;
end;

procedure TMapOptionsForm.ResizeLinkShape;
//-----------------------------------------------------------------------------
// Resizes the LinkShape control.
//-----------------------------------------------------------------------------
begin
  LinkShape.Top := LinkSpin.Top + (LinkSpin.Height -
                                   LinkSpin.Spinner.Position) div 2;
  LinkShape.Pen.Width := LinkSpin.Spinner.Position;
  LinkShape.Height := LinkSpin.Spinner.Position;
end;

procedure TMapOptionsForm.NodeBorderClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for NodeBorder checkbox. Causes the node shape symbol
// to be drawn with/without a border.
//-----------------------------------------------------------------------------
begin
  if NodeBorder.Checked then
    NodeShape.Pen.Color := clBlack
  else
    NodeShape.Pen.Color := clRed;
end;

procedure TMapOptionsForm.BtnHelpClick(Sender: TObject);
begin
  case NoteBook1.PageIndex of
    0: Application.HelpCommand(HELP_CONTEXT, 211120);
    1: Application.HelpCommand(HELP_CONTEXT, 211130);
    2: Application.HelpCommand(HELP_CONTEXT, 211140);
    3: Application.HelpCommand(HELP_CONTEXT, 211150);
    4: Application.HelpCommand(HELP_CONTEXT, 211160);
    5: Application.HelpCommand(HELP_CONTEXT, 211170);
    6: Application.HelpCommand(HELP_CONTEXT, 211180);
    7: Application.HelpCommand(HELP_CONTEXT, 211190);
  end;
end;

procedure TMapOptionsForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then BtnHelpClick(Sender);
end;

end.
