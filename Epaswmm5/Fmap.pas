unit Fmap;

{-------------------------------------------------------------------}
{                    Unit:    Fmap.pas                              }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    06/27/23   (5.2.4)                    }
{                    Author:  L. Rossman                            }
{                                                                   }
{    MDI child form that displays a map of the study area           }
{    being analyzed. The map is an object of class TMap             }
{    whose display is handled by methods in the Umap.pas unit.      }
{    Methods contained in this form handle user interaction         }
{    with the map, such as zooming, panning, and adding             }
{    or moving map objects. The form is created on startup          }
{    and remains active throughout the session.                     }
{-------------------------------------------------------------------}

//=============================================================================
//    The main components on this form include:
//      PopupMenu1 - popup menu for editing map objects
//      PopupMenu2 - popup menu for toggling display of
//        legends, backdrop image, and options dialog
//      PopupMenu3 - popup menu for editing subcatchment & link vertices
//      SubcatchLegendPanel - panel containing a Paintbox that displays
//        the legend for subcatchment areas
//      NodeLegendPanel - panel containing a Paintbox that displays
//        the legend for nodes
//      LinkLegendPanel - panel containing a Paintbox that displays
//        the legend for links
//      TimePanel - panel displaying the simulation elapsed time
//      HintPanel - panel displaying flyover information on the object
//        under the mouse pointer
//      Timer1 - timer used to make a highlighted object blink
//      Timer2 - timer used for displaying flyover map information
//      Timer3 - timer used to display X,Y coords. in MainForm's status panel
//=============================================================================

interface

uses
  SysUtils, Windows, Types, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, Spin, StdCtrls, ExtCtrls, Tabs, Buttons, Menus,
  Clipbrd, Math, Printers, System.ImageList, System.UITypes, ImgList,
  Xprinter, Umap, Uvertex, Ulegend, Uglobals, Uproject, Uutils,
  Vcl.VirtualImageList, Vcl.BaseImageCollection, Vcl.ImageCollection,
  Vcl.ComCtrls, Vcl.ToolWin;

const
  MAXVERTICES  = 100;        //Max. number of vertices in a fenceline

  TXT_EDIT_VERTEX =
    '  Select a vertex to move (with the mouse pressed) or to add/delete '+
    '(right-click).';
  TXT_GROUP_SELECT =
    '  Enclose the objects to select '+
    '(left-click to change direction, right-click to close).';
  TXT_PAN_MAP =
    '  Pan the map by moving the mouse with the left button pressed.';
  TXT_ZOOM_IN =
    '  Zoom to an area by moving the mouse with the left button pressed '+
    '(or left-click to zoom 2x).';
  TXT_MEASURE_LENGTH =
    '  Left-click along a path to measure, '+
    'right-click to display its distance and area.';

type
  TLinkInfo = record         //Data structure for drawing a link
    Node1, Node2  : TNode;   // End nodes of link
    Point1, Point2: TPoint;  // Pixel location of link's end points
  end;

  TMapForm = class(TForm)

    PopupMenu1: TPopupMenu;
      PopupCopy: TMenuItem;
      PopupPaste: TMenuItem;
      PopupDelete: TMenuItem;
      PopupReverse: TMenuItem;
      N4: TMenuItem;
      PopupVertices: TMenuItem;
      PopupProperties: TMenuItem;

    PopupMenu2: TPopupMenu;
      PopupOptions: TMenuItem;

    PopupMenu3: TPopupMenu;
      PopupAddVertex: TMenuItem;
      PopupDeleteVertex: TMenuItem;
      N2: TMenuItem;
      PopupQuitEditing: TMenuItem;

    SubcatchLegendPanel: TPanel;
    SubcatchLegendBox: TPaintBox;
    NodeLegendPanel: TPanel;
    NodeLegendBox: TPaintBox;
    LinkLegendPanel: TPanel;
    LinkLegendBox: TPaintBox;
    TimeLegendBox: TScrollBox;
    TimeLegendPanel: TPanel;

    Timer1: TTimer;
    Timer2: TTimer;

    PopupObjects: TMenuItem;
    PopupLegends: TMenuItem;
    PopupSubcatchLegend: TMenuItem;
    PopupNodeLegend: TMenuItem;
    PopupLinkLegend: TMenuItem;
    PopupTimeLegend: TMenuItem;
    PopupShowGages: TMenuItem;
    PopupShowSubcatch: TMenuItem;
    PopupShowNodes: TMenuItem;
    PopupShowLinks: TMenuItem;
    PopupShowBackdrop: TMenuItem;
    PopupShowLabels: TMenuItem;
    PopupConvert: TMenuItem;
    ConvertToType1: TMenuItem;
    ConvertToType2: TMenuItem;
    ConvertToType3: TMenuItem;
    ConvertToType4: TMenuItem;
    ConvertToType5: TMenuItem;
    HintPanel: TPanel;
    HintLabel: TLabel;
    Timer3: TTimer;
    ImageCollection1: TImageCollection;
    VirtualImageList1: TVirtualImageList;
    MapToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton20: TToolButton;
    PopupDeleteAllVertices: TMenuItem;

    procedure CreateParams(var Params: TCreateParams); override;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormDblClick(Sender: TObject);
    procedure FormDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure FormDragDrop(Sender, Source: TObject; X, Y: Integer);

    procedure SubcatchLegendBoxPaint(Sender: TObject);
    procedure SubcatchLegendBoxDblClick(Sender: TObject);
    procedure NodeLegendBoxPaint(Sender: TObject);
    procedure NodeLegendBoxDblClick(Sender: TObject);
    procedure LinkLegendBoxPaint(Sender: TObject);
    procedure LinkLegendBoxDblClick(Sender: TObject);
    procedure TimeLegendPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TimeLegendPanelDblClick(Sender: TObject);
    procedure DragLegend(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);

    procedure PopupMenu1Popup(Sender: TObject);
    procedure PopupMenu3Popup(Sender: TObject);
    procedure PopupCopyClick(Sender: TObject);
    procedure PopupPasteClick(Sender: TObject);
    procedure PopupDeleteClick(Sender: TObject);
    procedure PopupReverseClick(Sender: TObject);
    procedure PopupPropertiesClick(Sender: TObject);
    procedure PopupOptionsClick(Sender: TObject);
    procedure PopupSubcatchLegendClick(Sender: TObject);
    procedure PopupNodeLegendClick(Sender: TObject);
    procedure PopupLinkLegendClick(Sender: TObject);
    procedure PopupTimeLegendClick(Sender: TObject);
    procedure PopupAddVertexClick(Sender: TObject);
    procedure PopupDeleteVertexClick(Sender: TObject);
    procedure PopupVerticesClick(Sender: TObject);
    procedure PopupQuitEditingClick(Sender: TObject);
    procedure PopupLegendsClick(Sender: TObject);
    procedure PopupObjectsClick(Sender: TObject);
    procedure PopupShowBackdropClick(Sender: TObject);
    procedure PopupShowObjectsClick(Sender: TObject);
    procedure ConvertToTypeClick(Sender: TObject);
    procedure Timer3Timer(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure ObjButtonClick(Sender: TObject);
    procedure MapButtonClick(Sender: TObject);
    procedure ToolButton7Click(Sender: TObject);
    procedure PopupDeleteAllVerticesClick(Sender: TObject);

  private
    { Private declarations }
    FlyOverObject: Integer;       //Type of object under the mouse pointer
    FlyOverIndex: Integer;        //Item index of object under mouse pointer
    MapOptionsPage: Integer;      //Current page of map options dialog form
    SubcatchHiliteSize: Integer;  //Size of highlighting rectangle for subcatchments
    NodeHiliteSize: Integer;      //Size of highlighting rectangle for nodes
    LinkHiliteSize: Integer;      //Size of highlighting rectangle for links
    HiliteObject: Integer;        //Type of object highlighted on map
    HiliteIndex: Integer;         //Index of item highlighted on map
    SelectedObject: Integer;      //Type of object selected on map
    SelectedIndex : Integer;      //Index of item selected on map
    SelectedSubcatch: TSubcatch;  //Pointer to selected subcatchment area
    SelectedNode: TNode;          //Pointer to selected node
    SelectedLink: TLink;          //Pointer to selected link
    SelectedVlist: TVertexList;   //Pointer to selected vertex list
    SelectedVertex: PVertex;      //Pointer to selected link vertex
    LinkInfo : TLinkInfo;         //Data structure used for drawing links
    Moving   : Boolean;           //True if user is moving an object
    Aligning : Boolean;           //True if user is aligning the backdrop
    Resizing : Boolean;           //True if user is resizing the backdrop
    Fencing  : Boolean;           //True if user is drawing a fenceline
    Panning  : Boolean;           //True if user is panning the map
    Vertexing: Boolean;           //True if user is editing a link's vertices
    Zooming  : Boolean;           //True if user if zooming the map
    ZoomRect : TRect;             //Rectangle that defines zoom-in extent
    OldTickCount: DWORD;          //Used to measure a small time delay

    procedure AddVertex;
    procedure BeginFencing(X: Integer; Y: Integer);
    function  BeginLinking(const X: Integer; const Y: Integer): Boolean;
    procedure BeginPanning(X: Integer; Y: Integer);
    procedure BeginSelecting(X: Integer; Y: Integer);
    procedure BeginVertexing;
    procedure BeginZooming(X: Integer; Y: Integer);

    procedure CopyToBitmap(const Fname: String);
    procedure DeleteVertex;
    procedure DisplayFlyOver(const X,Y: Integer);
    procedure DrawDotLink(const P1: TPoint; const P2: TPoint);
    procedure DrawDotRect(aRect: TRect);
    procedure DrawFenceline;
    procedure DrawFilledRect(const P: TPoint);
    procedure DrawToMetafile(MyMetafile: TMetafile);
    function  DrawVertices(DrawFlag: Boolean): TRect;
    procedure DrawZoomRect;

    procedure EndFencing;
    procedure EndLinking(const X: Integer; const Y: Integer);
    procedure EndPanning;
    procedure EndSelecting;
    procedure EndVertexing;
    procedure EraseFenceline;

    function  FindSubcatch(const X: Integer; const Y: Integer): Boolean;
    function  FindGage(const X: Integer; const Y: Integer): Boolean;
    function  FindLabel(const X: Integer; const Y: Integer): Boolean;
    function  FindLink(const X: Integer; const Y: Integer): Boolean;
    function  FindNode(const X: Integer; const Y: Integer): Boolean;
    function  FindObject(const X: Integer; const Y: Integer): Boolean;
    function  FindVertex(P: TPoint): Boolean;

    function  GetMapClientWidth: Integer;
    function  GetVertexRect(V: PVertex): TRect;
    procedure GoFlyOver(const X,Y: Integer);
    procedure GoLabeling(X: Integer; Y: Integer);
    procedure GoLinking(X: Integer; Y: Integer);
    procedure GoMeasuring;
    procedure GoPanning(X: Integer; Y: Integer);
    procedure GoVertexing(X: Integer; Y: Integer);
    procedure GoZoomIn(X: Integer; Y: Integer);
    procedure GoZoomOut;

    procedure MoveLabel(const Index: Integer);
    procedure MoveLabelRect(const X,Y: Integer);
    procedure MoveVertex(const X,Y: Extended);
    procedure PlaceControl(aControl: TWinControl; X, Y, W, H: Integer);

    procedure UpdateZoomFactor;
    procedure WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo);
      message WM_GETMINMAXINFO;

  public
    { Public declarations }
    Map           : TMap;              //Study area map object
    CurrentTool   : Integer;           //Current map drawing tool
    Hilited       : Boolean;           //True if an object is highlighted
    HiliteRect    : TRect;             //Highlighted rectangle
    HiliteBitmap  : TBitmap;           //Bitmap storing highlighted rectangle
    Fenceline     : array[0..MAXVERTICES] of TPoint; //Fenceline points
    Linking       : Boolean;           //True if user is drawing a link
    NumFencePts   : Integer;           //Number of fenceline points
    RedrawOnResize: Boolean;           //True if map redrawn when form resized
    AllSelected   : Boolean;           //True if entire study area selected

    procedure BeginAligning(Sender: TObject);
    procedure BeginResizing(Sender: TObject);

    procedure ChangeHiliteObject(const ObjType, Index: Integer);
    procedure ClearMap;
    procedure CopyTo;
    procedure CopyToMetafile(const Fname: String);

    procedure DrawFullExtent;
    procedure DrawLinkLegend;
    procedure DrawNodeLegend;
    procedure DrawObject(const ObjType: Integer; const Index: Integer);
    procedure DrawSubcatchLegend;

    procedure EraseGage(const Index: Integer);
    procedure EraseLabel(const Index: Integer);
    procedure EraseObject(const ObjType: Integer; const Index: Integer);

    procedure GetSelectedObject(var Obj: Integer; var Index: Integer);
    procedure HiliteOff;
    procedure HiliteOn;
    procedure InvalidateMap(var aRect: TRect);

    procedure ModifyLinkLegend;
    procedure ModifyMapDimensions;
    procedure ModifyNodeLegend;
    procedure ModifySubcatchLegend;
    procedure MoveGage(const Index: Integer; const X,Y: Extended);
    procedure MoveNode(const ObjType, ObjIndex: Integer; const X, Y: Extended);
    procedure MoveSubcatch(const Index: Integer; const X, Y: Extended);

    procedure OpenBackdropFile;
    procedure PlaceLegends;
    procedure Print(Destination: TDestination);

    procedure RedrawMap;
    procedure ReplaceLabel(const Index: Integer; const S: String);

    procedure SelectAll;
    procedure SetMapOptions;

    procedure ToggleLinkLegend;
    procedure ToggleNodeLegend;
    procedure ToggleSubcatchLegend;
    procedure ToggleTimeLegend;
    procedure ToolButtonClick(ButtonTag: Integer);
    procedure UpdateOVmap;
  end;

var
  MapForm: TMapForm;

implementation

{$R *.DFM}

uses
  Fmain, Fovmap, Dcopy, Dmapdim, Dlabel, Dmap,
  Ubrowser, Uedit, Uoutput, Uupdate, Uclipbrd, Uinlet, Dfind;

const
  MAXZOOMRATIO = 1000000;    //Max. zoom-in ratio
  TICKDELAY    = 100;        //Delay before object can be moved

  MSG_NO_FIND_BACKDROP = 'Could not find backdrop file ';
  MSG_NO_READ_BACKDROP = 'Could not read backdrop file ';
  TXT_RESTORE_JUNCS    = 'Restoring display of junctions on the map.';
  TXT_RESTORE_LABELS   = 'Restoring display of labels on the map.';
  TXT_NODE             = 'Node ';

var
  FlyOverX  : Integer;
  FlyOverY  : Integer;
  AnchorX   : Integer;
  AnchorY   : Integer;
  ScrollRect: TRect;


//=============================================================================
//             Form Creation, Resizing and Destruction Methods
//=============================================================================

procedure TMapForm.CreateParams(var Params: TCreateParams);
//-----------------------------------------------------------------------------
// Positions the MapForm on creation so that it fills the
// MainForm's client area to right of the Browser panel.
//-----------------------------------------------------------------------------
begin
  Inherited CreateParams(Params);
  with Params do
  begin
    X := 0;
    Y := 0;
    Width := MainForm.ClientWidth -
             MainForm.BrowserPageControl.Width -
             MainForm.Splitter1.Width - 4;
    Height := MainForm.ClientHeight - MainForm.ToolbarPanel.Height -
              MainForm.StatusBar.Height - 5;
  end;
end;


procedure TMapForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
// Form's OnCreate event handler.
//-----------------------------------------------------------------------------
var
  Y: Integer;
  Ypos: Single;

begin
  // Create a map object
  Map := TMap.Create;

  // Save image of rain gage (stored as bitmap 0 in ImageList1)
  MainForm.ProjectImageList.GetBitmap(8, Map.GageBM);
  VirtualImageList1.GetBitmap(0, Map.InletBM);
  RedrawOnResize := False;

  // Assign background color choices
  Uglobals.SetMapBackColors;

  // Set background and foreground colors
  Color := MapBackColor[Map.Options.ColorIndex];
  Canvas.Pen.Color := MapForeColor[Map.Options.ColorIndex];
  MapOptionsPage := 0;

  // Initialize object selector & highlighting variables
  HiliteObject := -1;
  Hilited := False;
  HiliteRect := Rect(0,0,0,0);
  SubcatchHiliteSize := MaxIntValue([PIXTOL, Map.Options.SubcatchSize+1]);
  NodeHiliteSize := MaxIntValue([PIXTOL, Map.Options.NodeSize+1]);
  LinkHiliteSize := MaxIntValue([PIXTOL, Map.Options.LinkSize+1]);
  HiliteBitmap := TBitmap.Create;
  Timer1.Enabled := False;
  FlyOverObject := -1;
  FlyOverIndex  := -1;

  // Size the legend boxes
  with NodeLegendBox.Canvas do
  begin
    Y := TextHeight('[');
    Y := (MAXINTERVALS + 2) * ((3*Y) div 2) + 2*Y;
  end;
  SubcatchLegendPanel.Height := Y;
  NodeLegendPanel.Height := Y;
  LinkLegendPanel.Height := Y;
  Ypos := (Y+1) / ClientHeight;

  // Initialize the legend frames
  with SubcatchLegendFrame do
  begin
    X := 0;
    Y := 0;
    Framed := True;
  end;
  with NodeLegendFrame do
  begin
    X := 0;
    Y := Ypos;
    Framed := True;
  end;
  with LinkLegendFrame do
  begin
    X := 0;
    Y := 2*Ypos;
    Framed := True;
  end;
  with TimeLegendFrame do
  begin
    X := 1;
    Y := 0;
  end;

  // Make legends visible, hide hint panel
  SubcatchLegendPanel.Visible := True;
  NodeLegendPanel.Visible := True;
  LinkLegendPanel.Visible := True;
  TimeLegendBox.Visible := True;
  HintPanel.Visible := False;

  // Disable replacement of '&' with '_' in flyover labels
  HintLabel.ShowAccelChar := False;

  // Set checked status of legend menu items
  PopupSubcatchLegend.Checked := SubcatchLegendPanel.Visible;
  PopupNodeLegend.Checked := NodeLegendPanel.Visible;
  PopupLinkLegend.Checked := LinkLegendPanel.Visible;
  PopupTimeLegend.Checked := TimeLegendBox.Visible;
  MainForm.MnuSubcatchLegend.Checked := PopupSubcatchLegend.Checked;
  MainForm.MnuNodeLegend.Checked := PopupNodeLegend.Checked;
  MainForm.MnuLinkLegend.Checked := PopupLinkLegend.Checked;
  MainForm.MnuTimeLegend.Checked := PopupTimeLegend.Checked;
  TimeLegendBox.Visible := False;
  ToolButton1.Down := True;
  if Uglobals.StyleName = 'Windows' then
    MapToolbar.DrawingStyle := TTBDrawingStyle(dsNormal);
end;


procedure TMapForm.FormClose(Sender: TObject; var Action: TCloseAction);
//-----------------------------------------------------------------------------
// OnClose handler for the MapForm.
// Minimizes window when the close icon is clicked.
//-----------------------------------------------------------------------------
begin
  if (WindowState = wsMinimized)
  then Action := caNone
  else Action := caMinimize;
end;


procedure TMapForm.FormDestroy(Sender: TObject);
//-----------------------------------------------------------------------------
// Frees the Map object when the form is destroyed.
//-----------------------------------------------------------------------------
begin
  Map.Free;
  HiliteBitmap.Free;
end;


procedure TMapForm.FormResize(Sender: TObject);
//-----------------------------------------------------------------------------
// Form's OnResize event handler.
//-----------------------------------------------------------------------------
begin
  if Assigned(Map) then
  try
    Map.Resize(Rect(0,0,GetMapClientWidth,ClientHeight));
    if RedrawOnResize then
    begin
      Map.Rescale;
      if Visible then PlaceLegends;
      Map.RedrawBackdrop;
      RedrawMap;
      UpdateOVmap;
    end;
  finally
  end;
end;


procedure TMapForm.WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo);
//-----------------------------------------------------------------------------
// Message handler for WM_GETMINMAXINFO that restricts the
// smallest size that a user can re-size the MapForm to.
//-----------------------------------------------------------------------------
var
  MinMax: PMinMaxInfo;
begin
  inherited;
  MinMax := Msg.MinMaxInfo;
  MinMax^.ptMinTrackSize.X := MINMAPSIZE;
  MinMax^.ptMinTrackSize.Y := MINMAPSIZE;
end;


procedure TMapForm.PlaceLegends;
//-----------------------------------------------------------------------------
// Re-locates legends on the MapForm after its been re-sized.
//-----------------------------------------------------------------------------
begin
  if LinkLegendFrame.Y = MISSING
  then LinkLegendFrame.Y := (NodeLegendPanel.Height+1)/MapForm.ClientHeight;
  with SubcatchLegendPanel do
    PlaceControl(SubcatchLegendPanel,
                 Round(SubcatchLegendFrame.X*MapForm.GetMapClientWidth),
                 Round(SubcatchLegendFrame.Y*MapForm.ClientHeight),
                 Width, Height);
  with NodeLegendPanel do
    PlaceControl(NodeLegendPanel,
                 Round(NodeLegendFrame.X*MapForm.GetMapClientWidth),
                 Round(NodeLegendFrame.Y*MapForm.ClientHeight),
                 Width, Height);
  with LinkLegendPanel do
    PlaceControl(LinkLegendPanel,
                 Round(LinkLegendFrame.X*MapForm.GetMapClientWidth),
                 Round(LinkLegendFrame.Y*MapForm.ClientHeight),
                 Width, Height);
  with TimeLegendBox do
    PlaceControl(TimeLegendBox,
                 Round(TimeLegendFrame.X*MapForm.GetMapClientWidth-Width)-2,
                 Round(TimeLegendFrame.Y*MapForm.ClientHeight)+2,
                 Width, Height);
end;


procedure TMapForm.PlaceControl(aControl: TWinControl; X, Y, W, H: Integer);
//-----------------------------------------------------------------------------
// Positions a control so that it falls within the bounds of the
// MapForm window.
//-----------------------------------------------------------------------------
begin
  if X < 0 then X := 0;
  if X + W > GetMapClientWidth then X := GetMapClientWidth - W;
  if Y < 0 then Y := 0;
  if Y + H > ClientHeight then Y := ClientHeight - H;
  aControl.SetBounds(X,Y,W,H);
end;


procedure TMapForm.UpdateOVmap;
//-----------------------------------------------------------------------------
// Updates dimensions & scaling of the Overview map
//-----------------------------------------------------------------------------
begin
  OVMapForm.OVmap.Dimensions := Map.Dimensions;
  OVMapForm.Rescale;
end;

function TMapForm.GetMapClientWidth;
begin
  Result := ClientWidth - MapToolBar.Width;
end;


//=============================================================================
//                          Map Drawing Procedures
//=============================================================================

procedure TMapForm.FormPaint(Sender: TObject);
//-----------------------------------------------------------------------------
// Form's OnPaint event handler. Copies a bitmap image of the Map object
// to the form's canvas when the form needs repainting.
//-----------------------------------------------------------------------------
begin

if Panning then
begin
    Uutils.Cls(Canvas, Map.Window.MapRect, clWhite);
    Canvas.CopyRect(ScrollRect, Map.Canvas, Map.Window.MapRect);
    exit;
end;


  Canvas.Draw(0, 0, Map.Bitmap);
  DrawFenceline;
  HiliteOn;
end;


procedure TMapForm.InvalidateMap(var aRect: TRect);
//-----------------------------------------------------------------------------
// Redraws the portion of the map contained in the rectangle aRect.
//-----------------------------------------------------------------------------
var
  FullRect: TRect;
begin
  FullRect := Map.Window.MapRect;  //Save map window's bounding rectangle
  InflateRect(aRect, 50, 50);      //Inflate aRect a little
  Map.Window.MapRect := aRect;     //Replace the map window with aRect
  Map.DrawMap;                     //Draw the map into aRect
  Canvas.CopyRect(aRect,           //Copy aRect to the form's canvas
    Map.Bitmap.Canvas, aRect);
  Map.Window.MapRect := FullRect;  //Restore map's original rectangle
  OVMapForm.NeedsUpdating := True; //Mark the Overview Map for updating
end;


procedure TMapForm.RedrawMap;
//-----------------------------------------------------------------------------
// Redraws the study area map at the current zoom extent.
//-----------------------------------------------------------------------------
begin
  UpdateZoomFactor;
  HiliteOff;
  Map.DrawMap;
  Canvas.Draw(0, 0, Map.Bitmap);
  DrawFenceline;
  HiliteOn;
end;


procedure TMapForm.DrawFullExtent;
//-----------------------------------------------------------------------------
// Redraws the study area map at full extent.
//-----------------------------------------------------------------------------
begin
  Map.ZoomIndex := 0;
  Map.Rescale;
  Map.RedrawBackdrop;
  RedrawMap;
  UpdateOVmap;
  OVMapForm.ShowMapExtent;
end;


procedure TMapForm.ClearMap;
//-----------------------------------------------------------------------------
// Clears the study area map display.
//-----------------------------------------------------------------------------
begin
  Fencing := False;
  Vertexing := False;
  Linking := False;
  HiliteOff;
  HiliteObject := -1;
  SelectedLink := nil;
  SelectedVertex := nil;
  EraseFenceline;
  Map.Backdrop := DefMapBackdrop;
  Map.Dimensions := DefMapDimensions;
  Map.ZoomIndex := 0;
  Map.Rescale;
  RedrawMap;
  OVMapForm.OVmap.Backdrop := DefMapBackdrop;
  OVMapForm.Rescale;
  OVMapForm.Hide;
end;


procedure TMapForm.SelectAll;
//-----------------------------------------------------------------------------
// Draws a fence line around the entire study area map.
//-----------------------------------------------------------------------------
begin
  DrawFenceline;
  NumFencePts := 5;
  Fenceline[0] := Point(1, 1);
  Fenceline[1] := Point(1, ClientHeight-1);
  Fenceline[2] := Point(GetMapClientWidth-1, ClientHeight-1);
  Fenceline[3] := Point(GetMapClientWidth-1, 0);
  Fenceline[4] := Point(1, 1);
  DrawFenceline;
  AllSelected := True;
  MainForm.SelectorButtonClick;
end;


procedure TMapForm.DrawObject(const ObjType: Integer; const Index: Integer);
//-----------------------------------------------------------------------------
// Draws an object on the map by determining the area it occupies
// and invalidating this area.
//-----------------------------------------------------------------------------
var
  aRect: TRect;
begin
  HiliteOff;
  aRect := Map.GetBoundingRect(ObjType, Index);
  InvalidateMap(aRect);
  HiliteOn;
end;


procedure TMapForm.DrawDotLink(const P1: TPoint; const P2: TPoint);
//-----------------------------------------------------------------------------
// Draws a dotted line between pixel points P1 and P2.
//-----------------------------------------------------------------------------
begin
  with Canvas do
  begin
    Pen.Mode := pmXor;   {pmNotXor;}
    Pen.Style := psDot;
    Pen.Color := MapForeColor[Map.Options.ColorIndex];
    MoveTo(P1.X, P1.Y);
    LineTo(P2.X, P2.Y);
    Pen.Style := psSolid;
    Pen.Mode := pmCopy;
  end;
end;


procedure TMapForm.DrawFilledRect(const P: TPoint);
//-------------------------------------------------
// Draws a small filled rectangle on the map.
//-------------------------------------------------
var
  oldColor: TColor;
begin
  with Canvas do
  begin
    oldColor := Brush.Color;
    Brush.Style := bsSolid;
    Brush.Color := clRed;
    Pen.Color := MapForeColor[Map.Options.ColorIndex];
    Rectangle(Rect(P.X-4, P.Y-4, P.X+4, P.Y+4));
    Brush.Color := oldColor;
  end;
end;


procedure TMapForm.DrawDotRect(aRect: TRect);
//-----------------------------------------------------------------------------
// Draws a dotted outline of the rectangle aRect
//-----------------------------------------------------------------------------
begin
  with Canvas do
  begin
    Pen.Mode := pmXor;   {pmNotXor;}
    Pen.Style := psDot;
    with aRect do
    begin
      PolyLine([Point(Left,Top), Point(Right,Top), Point(Right,Bottom),
        Point(Left,Bottom), Point(Left,Top)]);
    end;
    Pen.Style := psSolid;
    Pen.Mode := pmCopy;
  end;
end;


procedure TMapForm.DrawFenceline;
//-----------------------------------------------------------------------------
// Draws a fence line around a user-selected portion of the map.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  if NumFencePts >= 2 then
    for I := 0 to NumFencePts-2 do
      DrawDotLink(Fenceline[I], Fenceline[I+1]);
end;


procedure TMapForm.EraseFenceline;
//-----------------------------------------------------------------------------
// Erases the map's current fence line.
//-----------------------------------------------------------------------------
begin
  Canvas.Draw(0, 0, Map.Bitmap);
  NumFencePts := 0;
  Fencing := False;
  AllSelected := False;
end;


procedure TMapForm.ModifyMapDimensions;
//-----------------------------------------------------------------------------
// Invokes the Map Dimensions dialog to modify the map's dimensions.
//-----------------------------------------------------------------------------
var
  MapDimensionsForm: TMapDimensionsForm;
begin
  // Create map dimensions dialog form
  MainForm.SelectorButtonClick;
  MapDimensionsForm := TMapDimensionsForm.Create(self);
  with MapDimensionsForm do
  try

    // Load current dimensions into the form
    SetData(Map.Dimensions);

    // Retrieve new extents from the form
    if ShowModal = mrOK then
    begin
      GetData(Map.Dimensions);
      HasChanged := True;

      // Re-scale & redraw the map
      DrawFullExtent;

      // Update contents of Property Editor
      Uedit.UpdateEditor(Uglobals.EditorObject, Uglobals.EditorIndex);
    end;

  finally
    Free;
  end;
end;


//=============================================================================
//                      Backdrop Image Procedures
//=============================================================================

procedure TMapForm.OpenBackdropFile;
//-----------------------------------------------------------------------------
// Opens a map backdrop image file.
//-----------------------------------------------------------------------------
var
  Opened: Boolean;
begin
  if Length(Map.Backdrop.Filename) = 0 then Exit;
  Opened := False;
  if not FileExists(Map.Backdrop.Filename)
  then Uutils.MsgDlg(MSG_NO_FIND_BACKDROP + Map.Backdrop.Filename, mtError, [mbOK])
  else if not Map.RedrawBackdrop
  then Uutils.MsgDlg(MSG_NO_READ_BACKDROP + Map.Backdrop.Filename, mtError, [mbOK])
  else Opened := True;
  if not Opened then
  begin
    Map.Backdrop := DefMapBackdrop;
    HasChanged := True;
  end
  else
  begin
    Map.Backdrop.Visible := True;
    OVmapForm.OVmap.Backdrop := Map.Backdrop;
    OVmapForm.OVmap.Backdrop.Watermark := True;
    OVmapForm.OVmap.Backdrop.Grayscale := False;
  end;
end;


//=============================================================================
//                       User Interaction Procedures
//=============================================================================

procedure TMapForm.FormDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
//-----------------------------------------------------------------------------
// Form's OnDragOver handler. Determines if an object dragged from the
// MainForm's ItemListBox can be dragged onto the map.
//-----------------------------------------------------------------------------
begin
  Accept := False;
  if Source = MainForm.ItemListBox then
  begin
    if CurrentList in [RAINGAGE, SUBCATCH, JUNCTION, OUTFALL, DIVIDER,
       STORAGE, MAPLABEL]
    then Accept := True;
  end;
end;

procedure TMapForm.FormDragDrop(Sender, Source: TObject; X, Y: Integer);
//-----------------------------------------------------------------------------
// Form's OnDragDrop handler. Repositions an object which has dragged from the
// MainForm's ItemListBox and dropped onto the map.
//-----------------------------------------------------------------------------
var
  Index: Integer;
  Xmap : Extended;
  Ymap : Extended;
begin
  Index := Project.CurrentItem[CurrentList];
  Xmap := Map.GetX(X);
  Ymap := Map.GetY(Y);
  if Source = MainForm.ItemListBox then
  begin
    if CurrentList = RAINGAGE then
    begin
      EraseGage(Index);
      MoveGage(Index, Xmap, Ymap);
    end
    else if Project.IsSubcatch(CurrentList)
    then MoveSubcatch(Index, Xmap, Ymap)
    else if Project.IsNode(CurrentList)
    then MoveNode(CurrentList, Index, Xmap, Ymap)
    else if CurrentList = MAPLABEL then
    begin
      ScrollRect := Rect(X, Y, X, Y);
      MoveLabel(Index);
    end;
    Uedit.UpdateEditor(CurrentList, Index);
    ChangeHiliteObject(CurrentList, Index);
  end;
end;

procedure TMapForm.MapButtonClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the Map Toolbar buttons - invokes the
// ToolButtonClick procedure. The Tag property of each
// MapButton stores the button's action code:
// MapButton   Tag   Action
// ----------  ----  -----------------------------
//    1        101    Activate Map Selection tool
//    2        102    Activate Vertex Selection tool
//    3        103    Activate Group Selection tool
//    4        104    Activate Map Panning tool
//    5        105    Activate Map Zoom In tool
//    6        106    Activate Map Zoom Out tool
//    7        107    (Activated by MapButon7Click)
//    8        108    Activate Map Ruler tool
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  // Stop adding new objects to the map
  MainForm.ShowStatusHint('');
  MainForm.BrowserBtnNew.Down := False;

  // Place all buttons on the Map & Object toolbars in the UP position
  for I := 0 to MapToolbar.ButtonCount-1 do
    MapToolbar.Buttons[I].Down := False;

  // Place the selected Map Toolbar button in the DOWN position
  TToolButton(Sender).Down := True;

  // Show action hint in main form status bar
  case TToolButton(Sender).Tag of
  102: MainForm.ShowStatusHint(TXT_EDIT_VERTEX);
  103: MainForm.ShowStatusHint(TXT_GROUP_SELECT);
  104: MainForm.ShowStatusHint(TXT_PAN_MAP);
  105: MainForm.ShowStatusHint(TXT_ZOOM_IN);
  108: MainForm.ShowStatusHint(TXT_MEASURE_LENGTH);
  end;

  // Pass the button click on to the MapForm
  ToolButtonClick(TToolButton(Sender).Tag);

  // Put Zoom Out button back in Up position
  if TToolButton(Sender).Tag = 106 then
  begin
    TToolButton(Sender).Down := False;
    MainForm.ShowStatusHint('');
    MainForm.BrowserBtnNew.Down := False;
  end;

end;

procedure TMapForm.ObjButtonClick(Sender: TObject);
begin
  with Sender as TToolButton do
  begin
    Uglobals.CurrentList := Tag;
  end;
  MainForm.BrowserBtnNewClick(Sender);
end;

procedure TMapForm.ToolButton7Click(Sender: TObject);
begin
  DrawFullExtent;
end;

procedure TMapForm.ToolButtonClick(ButtonTag: Integer);
//-----------------------------------------------------------------------------
// Processes OnClick events for the toolbar buttons. The ButtonTag parameter
// determines which toolbutton was selected. See the notes in the
// MapButtonClick procedure for a listing of which Tags go with which buttons.
//-----------------------------------------------------------------------------
begin
  // Determine which tool button was pressed
  CurrentTool := ButtonTag;

  // End vertex editing if the Vertex Selection tool was not selected
  if (CurrentTool <> VERTEXSELECT) and Vertexing then
  begin
    EndVertexing;
    SelectedLink := nil;
    SelectedSubcatch := nil;
    SelectedNode := nil;
  end;

  // End measuring if the Ruler tool was not selected
  if (CurrentTool <> RULER) and Fencing then
  begin
    EraseFenceline;
  end;

  // Turn off Hiliting if a map object tool was selected
  if Project.IsVisual(CurrentTool) then HiliteOff;

  // Turn off all action flags
  if Fencing or Linking then EraseFenceline;
  Linking := False;
  Moving := False;
  Aligning := False;
  Resizing := False;
  Panning := False;
  Fencing := False;
  Zooming := False;

  // Update the object listed in the Browser panel
  if Project.IsVisual(CurrentTool)
  then Ubrowser.BrowserUpdate(CurrentTool, Project.CurrentItem[CurrentTool]);

  // Change the mouse cursor shape depending on the tool button pressed
  case CurrentTool of

  RAINGAGE:
    Cursor := crXHAIR;

  SUBCATCH, STORAGE:
    Cursor := crPENCIL;

  JUNCTION..DIVIDER:
    Cursor := crXHAIR;

  CONDUIT..OUTLET:
    Cursor := -3;  //crCROSS

  MAPLABEL:
    // Restore the labels display if it was turned off
    begin
      if not Map.Options.ShowLabels
      or (Map.ZoomRatio < Map.Options.LabelZoom)
      then begin
        Uutils.MsgDlg(TXT_RESTORE_LABELS, mtInformation, [mbOK]);
        Map.Options.ShowLabels := True;
        Map.Options.LabelZoom := Map.ZoomRatio;
        RedrawMap;
      end;
      Cursor := -4;  //crlBEAM
    end;

  SELECT:
    Cursor := -2;  //crARROW

  GROUPSELECT:
    begin
      Cursor := -3;
    end;

  PAN:
    Cursor := crMOVE;

  ZOOMIN:
    Cursor := crZOOMIN;

  ZOOMOUT:
    GoZoomOut;

  RULER:
    Cursor := -3;

  VERTEXSELECT:
    begin
      BeginVertexing;
      Cursor := crARROWTIP;
    end;
  end;

  // Make the Hint Panel invisible if the Selection Tool was not selected
  if CurrentTool <> SELECT then HintPanel.Visible := False;
end;


procedure TMapForm.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
//-----------------------------------------------------------------------------
// Form's OnMouseDown event handler.
//-----------------------------------------------------------------------------
var
  P: TPoint;
begin
  // The left button is used for object selection, panning & zooming
  if Button = mbLeft then
  begin

    // Erase the current fence line if not Fencing or Linking
    if not Fencing and not Linking then EraseFenceline;

    // Begin executing the called-for action
    case CurrentTool of
    SUBCATCH,
    STORAGE:       BeginFencing(X, Y);
    SELECT:        BeginSelecting(X, Y);
    GROUPSELECT:   BeginFencing(X, Y);
    VERTEXSELECT:  GoVertexing(X, Y);
    PAN:           BeginPanning(X, Y);
    ZOOMIN:        BeginZooming(X, Y);
    RULER:         BeginFencing(X, Y);
    end;
  end

  // The right button either cancels a linking operation, completes a
  // fence lining operation, or activates one of the popup menus.
  else if Button = mbRight then
  begin
    // Get screen coords. of mouse position for use with popup menus
    P := ClientToScreen(Point(X, Y));

    // If linking, then cancel
    if Linking then EndLinking(-1, -1)

    // If drawing a fence line, then complete the operation
    else if Fencing then EndFencing

    // If vertex editing, then display the vertex popup menu
    else if Vertexing then PopupMenu3.Popup(P.X, P.Y)

    // If an object was selected, then display the object's popup menu
    else if (CurrentTool = SELECT) and FindObject(X, Y) then
    begin
      Ubrowser.BrowserUpdate(SelectedObject, SelectedIndex);
      PopupMenu1.Popup(P.X, P.Y);
    end

    // Otherwise invoke the map's popup menu.
    else
    begin
      PopupMenu2.Popup(P.X, P.Y);
    end;
  end

  // The middle button begins a panning operation
  else if Button = mbMiddle then
  begin
    if not Fencing and not Linking then EraseFenceline;
    Cursor := crFIST;
    HiliteOff;
    Panning := True;
    AnchorX := X;
    AnchorY := Y;
    ScrollRect := Map.Window.MapRect;
  end;
end;


procedure TMapForm.FormMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
//-----------------------------------------------------------------------------
// Form's OnMouseMove event handler.
//-----------------------------------------------------------------------------
begin
  // Do nothing if the mouse is not in the map's window
  if not (PtInRect(Map.Window.MapRect,Point(X,Y))) then Exit;

  // Save the curent position of the flyover hint label
  FlyOverX := X;
  FlyOverY := Y;
  CurrentX := Map.GetX(X);
  CurrentY := Map.GetY(Y);

  // Draw a dotted line if fence lining or linking is in progress
  if (Fencing or Linking) then
  begin
    DrawDotLink(Fenceline[NumFencePts-2], Fenceline[NumFencePts-1]);
    Fenceline[NumFencePts-1] := Point(X,Y);
    DrawDotLink(Fenceline[NumFencePts-2], Fenceline[NumFencePts-1]);
  end

  // Scroll the map if panning is in progress
  else if Panning = True then GoPanning(X,Y)

  // Draw a dotted rectangle if zooming is in progress
  else if Zooming = True then
  begin
    DrawZoomRect;
    ZoomRect.BottomRight := Point(X, Y);
    DrawZoomRect;
  end

  // Move the currently selected object if a move is in progress
  // (Add slight delay between time object first selected
  // and when it can be moved)
  else if (Moving = True)
  and (GetTickCount - OldTickCount > TICKDELAY)
  and (ssLeft in Shift)
  then begin
    OldTickCount := 0;
    HiliteOff;
    if Vertexing
    then MoveVertex(CurrentX, CurrentY)
    else if HiliteObject = MAPLABEL
    then MoveLabelRect(X, Y)
    else if HiliteObject = RAINGAGE
    then MoveGage(HiliteIndex, CurrentX, CurrentY)
    else if HiliteObject = SUBCATCH
    then MoveSubcatch(HiliteIndex, CurrentX, CurrentY)
    else if Project.IsNode(HiliteObject) then
    begin
      MoveNode(HiliteObject, HiliteIndex, CurrentX, CurrentY);
    end;
  end;
end;


procedure TMapForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
//-----------------------------------------------------------------------------
// Form's OnMouseUp event handler.
// Completes the operation begun by the MouseDown action.
//-----------------------------------------------------------------------------
begin
  if Button = mbLeft then
  begin
    case CurrentTool of
    RAINGAGE:           Uedit.AddGage(CurrentX, CurrentY);
    JUNCTION..DIVIDER:  Uedit.AddNode(CurrentTool, CurrentX, CurrentY);
    MAPLABEL:           GoLabeling(X, Y);
    CONDUIT..OUTLET:    GoLinking(X, Y);
    SELECT,
    VERTEXSELECT:       EndSelecting;
    PAN:                EndPanning;
    ZOOMIN:             GoZoomIn(X, Y);
    end;
  end

  else if (Button = mbMiddle) and Panning then
  begin
    EndPanning;
    Cursor := crDefault;
  end;
end;


procedure TMapForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  Pos: TPoint;
begin
  Pos := ScreenToClient(MousePos);
  if WheelDelta > 0 then
  begin
    BeginZooming(Pos.X, Pos.Y);
    GoZoomIn(Pos.X, Pos.Y);
  end
  else GoZoomOut;
  Handled := true;
end;

procedure TMapForm.FormDblClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Form's OnDoubleClick event handler.
// Invokes the Property Editor for the currently selected object.
//-----------------------------------------------------------------------------
begin
  if (CurrentTool = SELECT)
  and Project.IsVisual(HiliteObject)
  then MainForm.BrowserBtnEditClick(Sender);
end;


procedure TMapForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
//-----------------------------------------------------------------------------
// Form's OnKeyDown event handler.
// Used to complete or cancel a fence lining operation, to delete a selected
// object, or to pass a keystroke on to the Property Editor.
//-----------------------------------------------------------------------------
begin
  if CurrentTool = VERTEXSELECT then
  begin
    if (Key = VK_DELETE) then DeleteVertex;
    if (Key = VK_INSERT) then AddVertex;
  end

  else if (Key = VK_RETURN) and Fencing then EndFencing

  else if (Key = VK_ESCAPE) then
  begin
    if Fencing then EraseFenceline
    else if Linking then EndLinking(-1, -1)
    else MainForm.SelectorButtonClick;
  end

  else if (Key = VK_DELETE) then
  begin
    if (Project.IsVisual(HiliteObject))
    or (NumFencePts > 0)
    then MainForm.BrowserBtnDeleteClick(Sender)
  end;
end;


procedure TMapForm.Timer2Timer(Sender: TObject);
//-----------------------------------------------------------------------------
// OnTimer handler for Timer2.
// Determines if the flyover hint feature can be displayed.
//-----------------------------------------------------------------------------
begin
  if FlyOvers and (CurrentTool = SELECT)
  and not Moving then GoFlyOver(FlyOverX, FlyOverY);
end;


procedure TMapForm.Timer3Timer(Sender: TObject);
//-----------------------------------------------------------------------------
// OnTimer handler for Timer3.
// Displays real world map coordinates in the MainForm's status panel
//-----------------------------------------------------------------------------
begin
  MainForm.XYLabel.Caption :=
    'X,Y: ' + FloatToStrF(CurrentX,ffFixed,18,Map.Dimensions.Digits) +
    ', ' + FloatToStrF(CurrentY,ffFixed,18,Map.Dimensions.Digits) +
    ' ' + MapUnitsAbbrev[Ord(Map.Dimensions.Units)];
end;


procedure TMapForm.GoFlyover(const X,Y: Integer);
//-----------------------------------------------------------------------------
// Implements the flyover hint feature. This displays an object's ID and
// view theme value when the mouse rests over the object on the map.
//-----------------------------------------------------------------------------
begin
  // Check if the mouse is over a map object
  if FindObject(X, Y) and Project.IsVisual(SelectedObject)
  and (SelectedObject <> MAPLABEL) then
  begin

    // Check if a new object has been selected
    if (SelectedObject <> FlyOverObject) or
       (SelectedIndex <> FlyOverIndex) then
    begin

      // Save the new flyover object type and its index
      FlyOverObject := SelectedObject;
      FlyOverIndex := SelectedIndex;

      // Display the flyover hint
      DisplayFlyOver(X,Y);
    end;
  end

  // Otherwise hide the flyover hint panel
  else
  begin
    HintPanel.Visible := False;
    FlyOverObject := -1;
    FlyOverIndex := -1;
  end;
end;


procedure TMapForm.DisplayFlyOver(const X,Y: Integer);
//-----------------------------------------------------------------------------
// Displays a flyover hint label on the map.
//-----------------------------------------------------------------------------
var
  S1, S2  : String;
  L, T    : Integer;
  W, H    : Integer;

begin
  // Use the GetMeterLabel procedure to obtain text of object's ID & value.
  Uoutput.GetMeterLabel(FlyOverObject, FlyOverIndex, S1, S2);

  // Build the hint panel's caption and determine its width & height
  Hint := S1;
  W := Canvas.TextWidth(Hint);
  H := Canvas.TextHeight(Hint);
  if (Length(S2) > 0) then
  begin
    S2 := ' ' + S2 + ' ';
    W := MaxIntValue([W, Canvas.TextWidth(S2)]);
    H := H + Canvas.TextHeight(S2);
    Hint := Hint + #10 + S2;
  end;

  // Position hint panel so it does not fall outside of map window
  HintLabel.Caption := Hint;
  Inc(W, 2);
  Inc(H, 2);
  L := X;
  T := Y - H - NodeHiliteSize;
  if T < 0 then T := Y + 20; //GetSystemMetrics(SM_CYCURSOR);
  PlaceControl(HintPanel, L, T, W, H);

  // Set color of hint panel & display it
  if      FlyoverObject = RAINGAGE      then HintPanel.Color := clYellow
  else if FlyoverObject = SUBCATCH      then HintPanel.Color := clLime
  else if Project.IsNode(FlyoverObject) then HintPanel.Color := clAqua
  else if Project.IsLink(FlyoverObject) then HintPanel.Color := clInfoBk;
  HintPanel.Visible := True;
end;


procedure TMapForm.GoLabeling(X: Integer; Y: Integer);
//-----------------------------------------------------------------------------
// Displays a LabelForm form to obtain user entry of a map label.
//-----------------------------------------------------------------------------
var
  S: String;
  P: TPoint;
  LabelForm: TLabelForm;
begin
  // Create a borderless LabelForm that contains an Edit control
  S := '';
  P := Point(X, Y);
  LabelForm := TLabelForm.Create(self);
  with LabelForm do
  try

    // Position the Edit control at the current mouse location
    Top := MapForm.ClientToScreen(P).Y;
    Left := MapForm.ClientToScreen(P).X;

    // Retrieve the text from the Edit control
    if ShowModal = mrOK then S := Edit1.Text;

  finally
    Free;
  end;

  // Add a new map label to the project
  if Length(S) > 0 then
  begin
    Repaint;
    Uedit.AddLabel(CurrentX, CurrentY, S);
  end;
end;


procedure TMapForm.GoLinking(X: Integer; Y: Integer);
//-----------------------------------------------------------------------------
// Begins/ends linking of one node to another when a link object is drawn.
//-----------------------------------------------------------------------------
begin
  // If linking already begun then end it, otherwise begin linking
  if Linking = True
  then EndLinking(X,Y)
  else Linking := BeginLinking(X,Y);
end;


function TMapForm.BeginLinking(const X: Integer; const Y: Integer): Boolean;
//-----------------------------------------------------------------------------
// Initializes the process that draws a link object on the map.
//-----------------------------------------------------------------------------
begin
  // Locate the start node clicked on
  Result := False;
  if not FindNode(X,Y) then Exit;

  // Highlight & save info about this node
  ChangeHiliteObject(SelectedObject, SelectedIndex);
  LinkInfo.Node1 := Project.GetNode(SelectedObject, SelectedIndex);
  LinkInfo.Point1 := Map.GetNodePoint(LinkInfo.Node1);
  LinkInfo.Point2 := LinkInfo.Point1;

  // Create the fence line used to draw the link
  Fenceline[0] := LinkInfo.Point1;
  NumFencePts := 1;
  Inc(NumFencePts);
  Fenceline[NumFencePts-1] := Point(X,Y);

  // Switch to Pencil cursor
  Cursor := crPENCIL;
  Result := True;
end;


procedure TMapForm.EndLinking(const X: Integer; const Y: Integer);
//-----------------------------------------------------------------------------
// Completes (or continues) the process that draws a link object on the map.
//-----------------------------------------------------------------------------
var
  N: Integer;
begin
  // Cancel linking if called for
  if (X < 0) and (Y < 0) then Linking := False

  // Check if an ending node was clicked on
  else
  begin

    // Node not clicked on, so add point to fence line
    if not FindNode(X,Y) then
    begin
      if NumFencePts < MAXVERTICES then
      begin
        Inc(NumFencePts);
        Fenceline[NumFencePts-1] := Point(X,Y);
      end;
      Exit;
    end

    // Node clicked on, so save it to LinkInfo structure
    else LinkInfo.Node2 := Project.GetNode(SelectedObject, SelectedIndex);
  end;

  // Erase fence line and change mouse cursor
  N := NumFencePts;
  EraseFenceLine;
  HiliteOff;
  Cursor := -3;  // crCross

  // Add new link to project database and turn off linking
  if Linking and (LinkInfo.Node1 <> LinkInfo.Node2)
  then Uedit.AddLink(CurrentTool, LinkInfo.Node1, LinkInfo.Node2, FenceLine, N-2);
  Linking := False;
end;

procedure TMapForm.BeginFencing(X: Integer; Y: Integer);
//-----------------------------------------------------------------------------
// Begins or continues the process that draws a fence line on the map.
//-----------------------------------------------------------------------------
begin
  // If this is first fence point then begin a new fence line
  AllSelected := False;
  if not Fencing then
  begin
    Fenceline[0] := Point(X, Y);
    Fenceline[1] := Point(X, Y);
    NumFencePts := 2;
    Fencing := True;
    if CurrentTool = RULER then DrawFilledRect(Fenceline[0]);
    Exit;
  end;

  // Otherwise add a new vertex point to the fence line
  if NumFencePts < MAXVERTICES then
  begin
    Inc(NumFencePts);
    Fenceline[NumFencePts-1] := Point(X, Y);
  end;
end;


procedure TMapForm.EndFencing;
//-----------------------------------------------------------------------------
// Completes the drawing of an enclosed fence line on the map.
//-----------------------------------------------------------------------------
var
  N: Integer;
begin
  // Turn off fence lining
  Fencing := False;

  // Create a new subcatchment area if the subcatchment tool is selected
  if CurrentTool = SUBCATCH then
  begin
    N := NumFencePts - 1;
    EraseFenceline;
    Uedit.AddSubcatch(Fenceline, N);
    Exit;
  end;

  if CurrentTool = STORAGE then
  begin
    N := NumFencePts - 1;
    EraseFenceline;
    Uedit.AddStorage(Fenceline, N);
    Exit;
  end;

  // Measure the fenceline if the Ruler tool is selected
  if CurrentTool = RULER then
  begin
    GoMeasuring;
    EraseFenceLine;
    Exit;
  end;

  // Otherwise close up the fence line polygon
  DrawDotLink(Fenceline[NumFencePts-1], Fenceline[0]);
  Fenceline[NumFencePts-1] := Fenceline[0];

  // And activate the Selection tool on Mainform Toolbar
  MainForm.SelectorButtonClick;
end;


procedure TMapForm.GoMeasuring;
//-----------------------------------------------------------------------------
// Measures a distance (or area) covered by a drawn fenceline on the map.
//-----------------------------------------------------------------------------
var
  Distance, Area: Extended;
  DistUnits: String;
  AreaUnits: String;
  Msg: String;
begin
  if UnitSystem = usUS then DistUnits := ' feet'
  else DistUnits := ' meters';
  Distance := Map.GetDistance(Fenceline, NumFencePts);
  Msg := 'Distance = ' + Format('%.3f', [Distance]) + DistUnits;
  if  (Abs(Fenceline[NumFencePts-1].X - Fenceline[0].X) < 5)
  and (Abs(Fenceline[NumFencePts-1].Y - Fenceline[0].Y) < 5)
  then begin
    Area := Map.GetArea(Fenceline, NumFencePts);
    if UnitSystem = usUS then AreaUnits := ' ac'
    else DistUnits := ' ha';
    Msg := Msg + #10 + 'Area = ' + Format('%.3f', [Area]) + AreaUnits;
  end;
  Uutils.MsgDlg(Msg, mtInformation, [mbOK]);
end;


procedure TMapForm.BeginZooming(X: Integer; Y: Integer);
//-----------------------------------------------------------------------------
// Processes the MouseDown action that begins a Zoom In.
//-----------------------------------------------------------------------------
begin
  // Initialize the zoom rectangle that will be drawn by the user
  ZoomRect.TopLeft := Point(X, Y);
  ZoomRect.BottomRight := Point(X, Y);

  // Prepare form's canvas for drawing the zoom rectangle
  Canvas.Pen.Mode := pmXor;
  Canvas.Pen.Style := psDot;

  // Turn Zooming on
  Zooming := True;
end;


procedure TMapForm.DrawZoomRect;
//-----------------------------------------------------------------------------
// Draws a rubberbanding rectangle around the current selected zoom-in area.
//-----------------------------------------------------------------------------
begin
  with Canvas, ZoomRect do
  begin
    PolyLine([Point(Left,Top), Point(Right,Top), Point(Right,Bottom)]);
    PolyLine([Point(Left,Top), Point(Left,Bottom), Point(Right,Bottom)]);
  end;
end;


procedure TMapForm.GoZoomIn(X: Integer; Y: Integer);
//-----------------------------------------------------------------------------
// Implements the Zoom In on the zoom rectangle which ends at location X,Y.
//-----------------------------------------------------------------------------
var
  Xc  : Extended;
  Yc  : Extended;
  ZFx : Extended;
  ZFy : Extended;
  ZF  : Extended;
  Rx  : Integer;
  Ry  : Integer;
  Temp: Integer;

begin
  // Erase the zoom rectangle from map
  Zooming := False;
  DrawZoomRect;
  Canvas.Pen.Mode := pmCopy;
  Canvas.Pen.Style := psSolid;

  // Make sure the zoom rectangle is oriented correctly
  with ZoomRect do
  begin
    if Left > Right then
    begin
      Temp := Left;
      Left := Right;
      Right := Temp;
    end;
    if Top > Bottom then
    begin
      Temp := Bottom;
      Bottom := Top;
      Top := Temp;
    end;
  end;

  // Exit if can't zoom in anymore
  if Map.ZoomIndex = High(Map.ZoomState) then Exit;

  // Adjust map scaling and offsets
  with Map.Window do
  begin

    // If user simply clicked mouse without drawing a
    // a zoom rectangle, then create a zoom rectangle
    // which equals 1/2 of the view window centered at X,Y.
    if IsRectEmpty(ZoomRect) = True
    then ZoomRect := Bounds(X-(Pwidth div 4), Y-(Pheight div 4),
                           (Pwidth div 2), (Pheight div 2));

    // Get the world coords. of the center of the zoom rectangle
    with ZoomRect do
    begin
      Rx := (Right + Left) div 2;
      Ry := (Top + Bottom) div 2;
    end;
    Xc := Map.GetX(Rx);
    Yc := Map.GetY(Ry);

    // Get a scale factor for zoom rectangle
    ZFx := (ZoomRect.Right - ZoomRect.Left)/Pwidth;
    ZFy := (ZoomRect.Bottom - ZoomRect.Top)/Pheight;
    if ZFy > ZFx then ZF := ZFy else ZF := ZFx;
    ZF := ZF * Map.ZoomState[Map.ZoomIndex].ZoomFactor;
  end;

  // Save new zoom state
  with Map do
  begin
    Inc(ZoomIndex);
    ZoomState[ZoomIndex].ZoomFactor := ZF;
    ZoomState[ZoomIndex].Xcenter := Xc;
    ZoomState[ZoomIndex].Ycenter := Yc;
  end;

  // Assign new map scaling
  with Map.Window do
  begin
    WperP := Map.Sfactor * ZF;
    Woffset.X := Map.ZoomState[Map.ZoomIndex].Xcenter - WperP * (Pwidth/2.0);
    Woffset.Y := Map.ZoomState[Map.ZoomIndex].Ycenter - WperP * (Pheight/2.0);
  end;

  // Redraw the study area map
  Map.RedrawBackdrop;
  RedrawMap;

  // Show the new map extent on the Overview map
  OVMapForm.ShowMapExtent;
end;

procedure TMapForm.GoZoomOut;
//-----------------------------------------------------------------------------
// Implements a Zoom Out on the map.
//-----------------------------------------------------------------------------
var
  Xc, Yc: Extended;
begin
  with Map do
  begin
    // Exit if can't zoom out any more
    if ZoomIndex = 0 then exit;

    // Find coordinates of current map center
    with Map.Window do
    begin
    Xc := Map.GetX(Pwidth div 2);;
    Yc := Map.GetY(Pheight div 2);
    end;

    // Restore previous zoom factor
    Dec(ZoomIndex);

    // Reference previous zoom to current map center
    ZoomState[ZoomIndex].Xcenter := Xc;
    ZoomState[ZoomIndex].Ycenter := Yc;

    // Rescale the map at previous zoom factor
    Rescale;

    // Redraw study area map
    Map.RedrawBackdrop;
    RedrawMap;

    // Show the new map extent on the Overview map
    OVMapForm.ShowMapExtent;
  end;
end;


procedure TMapForm.UpdateZoomFactor;
//-----------------------------------------------------------------------------
// Updates global MapZoomRatio variable and checks if further zooming
// can occur.
//-----------------------------------------------------------------------------
var
  Zoominflag:  Boolean;
  Zoomoutflag: Boolean;
begin
  with Map do
  begin
    // Determine if further zooming allowed
    Zoominflag  := not (ZoomIndex = High(ZoomState));
    Zoomoutflag := not (ZoomIndex = Low(ZoomState));

    // Determine current zoom ratio
    ZoomRatio := Round(100/ZoomState[ZoomIndex].ZoomFactor);
    if ZoomRatio >= MAXZOOMRATIO then Zoominflag := False;
  end;

  with MainForm do
  begin

    // Switch to Selection tool if cannot zoom anymore
    if ((CurrentTool = ZOOMIN) and (Zoominflag = FALSE))
    or ((CurrentTool = ZOOMOUT) and (Zoomoutflag = FALSE))
    then SelectorButtonClick;

    // Adjust status of menus & toolbar buttons on Main form
    MnuZoomIn.Enabled := Zoominflag;
    MnuZoomOut.Enabled := Zoomoutflag;
    ToolButton5.Enabled := Zoominflag;
    ToolButton6.Enabled := Zoomoutflag;

    // Update display of zoom ratio on Main form's status panel
    ZoomLevelLabel.Caption := 'Zoom Level: ' +
      IntToStr(Map.ZoomRatio) + '%  ';
  end;
end;

procedure TMapForm.BeginPanning(X: Integer; Y: Integer);
//-----------------------------------------------------------------------------
// Processes the MouseDown action that begins panning the map.
//-----------------------------------------------------------------------------
begin
  // Change cursor shape (by forcing ButtonUp message)
  SendMessage(Handle,WM_LBUTTONUP,0,0);
  Cursor := crFIST;
  HiliteOff;

  // Set panning flag & save anchor coordinates & map window rectangle
  Panning := True;
  AnchorX := X;
  AnchorY := Y;
  ScrollRect := Map.Window.MapRect;
end;


procedure TMapForm.GoPanning(X: Integer; Y: Integer);
//-----------------------------------------------------------------------------
// Processes MouseMove action that pans the map.
//-----------------------------------------------------------------------------
var
  Xscroll, Yscroll: Integer;
begin
  if Panning = True then
  begin

    // Determine scrolling distance
    Xscroll := X - AnchorX;
    Yscroll := Y - AnchorY;

    // Save current position
    AnchorX := X;
    AnchorY := Y;

    // Update world coordinate offsets of map
    with Map.Window do
    begin
      if not Aligning then
      begin
        Woffset.X := Woffset.X - Xscroll*WperP;
        Woffset.Y := Woffset.Y + Yscroll*WperP;
      end
      else with Map.Backdrop do
      begin
        LowerLeft.X  := LowerLeft.X + Xscroll*WperP;
        UpperRight.X := UpperRight.X + Xscroll*WperP;
        LowerLeft.Y  := LowerLeft.Y - Yscroll*WperP;
        UpperRight.Y := UpperRight.Y - Yscroll*WperP;
      end;

    end;

    with Map do
    begin
      ClearMap;
      if Backdrop.Visible then
      begin
        OffsetRect(ScrollRect, Xscroll, Yscroll);
        Bitmap.Canvas.CopyRect(ScrollRect, BackBM.Canvas, Window.MapRect);
      end;
      DrawForeground;
    end;
    Canvas.Draw(0, 0, Map.Bitmap);

  end;

end;


procedure TMapForm.EndPanning;
//-----------------------------------------------------------------------------
// Processes the MouseUp action that completes panning of map.
//-----------------------------------------------------------------------------
begin
  if Panning = True then
  begin
    // Redraw map
    Map.RedrawBackdrop;
    RedrawMap;

    // If Aligning the backdrop, then also redraw the Overview map
    if Aligning then
    begin
      OVmapForm.OVmap.Backdrop.LowerLeft  := Map.Backdrop.LowerLeft;
      OVmapForm.OVmap.Backdrop.UpperRight := Map.Backdrop.UpperRight;
      OVMapForm.Redraw;
    end;

    // Update the map extent rectangle on the Overview map.
    OVMapForm.ShowMapExtent;

    // Restore the Panning mouse cursor.
    Panning := False;
    Cursor := crMove;
  end;
end;


procedure TMapForm.BeginAligning(Sender: TObject);
//-----------------------------------------------------------------------------
// Begins the process of aligning a backdrop image with the project's map.
//-----------------------------------------------------------------------------
begin
  // Display backdrop if not visible
  if not Map.Backdrop.Visible then PopupShowBackdropClick(Sender);

  // Put map into Aligning mode
  Aligning := True;
end;


procedure TMapForm.BeginResizing(Sender: TObject);
//-----------------------------------------------------------------------------
// Begins the process of resizing a backdrop image.
//-----------------------------------------------------------------------------
begin
  // Display backdrop if not visible
  if not Map.Backdrop.Visible then PopupShowBackdropClick(Sender);

  // Put map into Resizing mode & make ZOOMIN be the current tool
  Resizing := True;
  CurrentTool := ZOOMIN;
end;


procedure TMapForm.BeginSelecting(X: Integer; Y: Integer);
//-----------------------------------------------------------------------------
// Processes the MouseDown action when the Select tool is active.
//-----------------------------------------------------------------------------
begin
  // Check if mouse is over a currently selected, moveable object
  if (Hilited)
  and InRect(X, Y, HiliteRect)
  and Project.IsVisual(SelectedObject) then
  begin

    // Hide hint panel and allow object to be moved
    HintPanel.Visible := False;
    Moving := True;
    OldTickCount := GetTickCount;

    // For labels and gages, copy HiliteRect to a moveable ScrollRect
    if SelectedObject in [RAINGAGE, MAPLABEL] then
    begin
      ScrollRect := HiliteRect;
      AnchorX := X;
      AnchorY := Y;
    end;
    Exit;
  end;

  // If mouse clicked over an object then update the listing in the Browser
  // panel (Hiliting will be turned on in UpdateBrowser() procedure)
  if FindObject(X, Y) then
  begin
    Ubrowser.BrowserUpdate(SelectedObject, SelectedIndex);
    if Assigned(FindForm)
    then FindForm.SearchFor(SelectedObject, SelectedIndex);
  end

  // Otherwise no object selected so cancel any highlighting
  else ChangeHiliteObject(-1, -1);
end;


procedure TMapForm.EndSelecting;
//-----------------------------------------------------------------------------
// Processes the MouseUp action when the Select tool is active.
//-----------------------------------------------------------------------------
begin
  if Moving then
  begin
    // Move map label if it is currently selected
    if HiliteObject = MAPLABEL then MoveLabel(HiliteIndex)

    // Otherwise mark the Overview map for updating
    else if not Vertexing then OVMapForm.NeedsUpdating := True;

    // Re-compute link lengths & subcatch areas if AutoLength is on
    if Vertexing and Uglobals.AutoLength then
    begin
      Uupdate.UpdateObjectLength(HiliteObject, HiliteIndex);
      MainForm.SetChangeFlags;
    end;
    HiliteOn;

    // Display new information in the Property Editor
    Uedit.UpdateEditor(HiliteObject, HiliteIndex);
  end;
  Moving := False;
end;


//=============================================================================
//                   Link or Subcatchment Vertex Editing Routines
//=============================================================================

procedure TMapForm.BeginVertexing;
//-----------------------------------------------------------------------------
// Places the map into Vertex Editing mode.
//-----------------------------------------------------------------------------
begin
  if HiliteObject = SUBCATCH then
  begin
    SelectedLink := nil;
    SelectedNode := nil;
    SelectedSubcatch := Project.GetSubcatch(SUBCATCH, HiliteIndex);
    SelectedVlist := SelectedSubcatch.Vlist;
    SelectedVertex := SelectedSubcatch.Vlist.First;
  end
  else if HiliteObject = STORAGE then
  begin
    SelectedLink := nil;
    SelectedSubcatch := nil;
    SelectedNode := Project.GetNode(HiliteObject, HiliteIndex);
    SelectedVlist := SelectedNode.Vlist;
    SelectedVertex := SelectedNode.Vlist.First;
  end
  else if Project.IsLink(HiliteObject) then
  begin
    SelectedSubcatch := nil;
    SelectedLink := Project.GetLink(HiliteObject, HiliteIndex);
    SelectedVlist := SelectedLink.Vlist;
    SelectedVertex := SelectedLink.Vlist.First;
  end
  else Exit;
  Vertexing := True;
  HiliteOff;
  HiliteOn;
end;


procedure TMapForm.GoVertexing(X: Integer; Y: Integer);
//-----------------------------------------------------------------------------
// Processes the OnMouseDown action for vertex editing.
//-----------------------------------------------------------------------------
begin
  // If the mouse is over a selected vertex, then allow the vertex to be moved
  if not Vertexing then Exit;
  if (Hilited) and InRect(X, Y, HiliteRect) then
  begin
    Moving := True;
    OldTickCount := GetTickCount;
  end

  // If the mouse is over a new vertex, then select it
  else if FindVertex(Point(X, Y)) then Exit

  // If the mouse is over another link, then begin editing its vertexes
  else if FindLink(X, Y) then
  begin
    EndVertexing;
    Ubrowser.BrowserUpdate(SelectedObject, SelectedIndex);
    BeginVertexing;
  end

  // If the mouse is over a subcatchment, then begin editing its vertexes
  else if FindSubcatch(X, Y) then
  begin
    EndVertexing;
    Ubrowser.BrowserUpdate(SelectedObject, SelectedIndex);
    BeginVertexing;
  end

  else if FindNode(X, Y) and (SelectedObject = STORAGE) then
  begin
    EndVertexing;
    Ubrowser.BrowserUpdate(SelectedObject, SelectedIndex);
    BeginVertexing;
  end;

end;


procedure TMapForm.EndVertexing;
//-----------------------------------------------------------------------------
// Places the map out of vertex editing mode.
//-----------------------------------------------------------------------------
begin
  HiliteOff;
  if Vertexing then DrawVertices(False);
  SelectedVlist := nil;
  SelectedVertex := nil;
  Vertexing := False;
  HiliteOn;
end;


procedure TMapForm.AddVertex;
//-----------------------------------------------------------------------------
// Adds a new vertex to the object being edited.
//-----------------------------------------------------------------------------
begin
  if SelectedSubcatch <> nil then
    SelectedVertex := SelectedSubcatch.AddVertex
  else if SelectedNode <> nil then
    SelectedVertex := SelectedNode.AddVertex
  else
    SelectedVertex := SelectedLink.AddVertex;
  HiliteOff;
  HiliteOn;
  HasChanged := True;
end;


procedure TMapForm.DeleteVertex;
//-----------------------------------------------------------------------------
// Deletes the currently selected vertex from the object being edited.
//-----------------------------------------------------------------------------
var
  R : TRect;
begin
  if SelectedVertex <> nil then
  begin
    R := Map.GetBoundingRect(HiliteObject, HiliteIndex);
    SelectedVertex := SelectedVlist.Delete(SelectedVertex);
    HiliteOff;
    InvalidateMap(R);
    if Uglobals.AutoLength
    then Uupdate.UpdateObjectLength(HiliteObject, HiliteIndex);
    HiliteOn;
    HasChanged := True;
  end;
end;


function TMapForm.FindVertex(P: TPoint): Boolean;
//-----------------------------------------------------------------------------
// Finds which vertex of the object being edited contains the point P.
// If a vertex is found, then it becomes the currently highlighted vertex.
//-----------------------------------------------------------------------------
var
  X1, X2: Extended;
  Y1, Y2: Extended;
  D     : Integer;
  V     : PVertex;
begin
  // Determine world coordinates of the 'hit' region rectangle
  // that surrounds the point P.
  D := LinkHiliteSize - 1;
  with Map do
  begin
    X1 := GetX(P.X-D);  //To left of P
    X2 := GetX(P.X+D);  //To right of P
    Y1 := GetY(P.Y+D);  //Below P
    Y2 := GetY(P.Y-D);  //Above P
  end;
  V := SelectedVlist.Find(X1,X2,Y1,Y2);
  if V <> nil then
  begin
    HiliteOff;
    SelectedVertex := V;
    HiliteOn;
    Result := True;
  end
  else Result := False;
end;


function TMapForm.GetVertexRect(V: PVertex): TRect;
//-----------------------------------------------------------------------------
// Determines a bounding rectangle for a vertex V.
//-----------------------------------------------------------------------------
var
  X, Y, Size: Integer;
begin
  if V = nil then Result := NORECT
  else
  begin
    Size := LinkHiliteSize - 1;
    X := Map.GetXpix(V^.X);
    Y := Map.GetYpix(V^.Y);
    Result := Rect(X-Size, Y-Size, X+Size, Y+Size);
  end;
end;


function TMapForm.DrawVertices(DrawFlag: Boolean): TRect;
//-----------------------------------------------------------------------------
// Draws or erases the vertexes of the object being edited,
// depending on whether DrawFlag is True or not. Returns
// the bounding rectangle of the currently selected vertex.
//-----------------------------------------------------------------------------
var
  R: TRect;
  V: PVertex;
  BrushColor: TColor;
  PenColor: TColor;
begin
  Result := NORECT;
  if not DrawFlag then
  begin
    R := Map.GetBoundingRect(HiliteObject, HiliteIndex);
    if SelectedSubcatch <> nil then SelectedSubcatch.SetCentroid
    else if SelectedNode <> nil then SelectedNode.SetCentroid;
    InvalidateMap(R);
  end

  else with Canvas do
  begin
    BrushColor  := Brush.Color;
    PenColor    := Pen.Color;
    Brush.Color := MapBackColor[Map.Options.ColorIndex];
    Pen.Color   := MapForeColor[Map.Options.ColorIndex];
    V := SelectedVlist.Vfirst;
    while V <> nil do
    begin
      R := GetVertexRect(V);
      if V = SelectedVertex then Result := R;
      Rectangle(R);
      V := V^.Next;
    end;
    Brush.Color := BrushColor;
    Pen.Color := PenColor;
  end;
end;


procedure TMapForm.MoveVertex(const X,Y: Extended);
//-----------------------------------------------------------------------------
// Moves the currently selected vertex to new world coordinate position X,Y.
//-----------------------------------------------------------------------------
var
  R: TRect;
begin
  if SelectedVertex <> nil then
  begin
    HiliteOff;
    R := Map.GetBoundingRect(HiliteObject, HiliteIndex);
    SelectedVertex^.X := X;
    SelectedVertex^.Y := Y;
    if UnionRect(R, R, Map.GetBoundingRect(HiliteObject, HiliteIndex))
    then InvalidateMap(R);
    HasChanged := True;
    HiliteOn;
  end;
end;


//=============================================================================
//                     Map Selection Methods
// These functions determine what map object the mouse pointer is
// currently on by finding values for the following variables:
//    SelectedObject   (e.g., subcatchment, junction, conduit, etc.)
//    SelectedIndex    (index of selected object)
//=============================================================================

function TMapForm.FindObject(const X: Integer; const Y: Integer): Boolean;
//-----------------------------------------------------------------------------
// Determines which object on the map is located at pixels X,Y.
//-----------------------------------------------------------------------------
begin
  SelectedObject := -1;
  Result := False;
  if (Map.Options.ShowNodes and FindNode(X, Y))
  or (Map.Options.ShowLabels and FindLabel(X, Y))
  or (Map.Options.ShowLinks and FindLink(X, Y))
  or (Map.Options.ShowSubcatchs and FindSubcatch(X, Y))
  or (Map.Options.ShowGages and FindGage(X, Y))
  then Result := True;
end;


function TMapForm.FindNode(const X: Integer; const Y: Integer): Boolean;
//-----------------------------------------------------------------------------
// Determines which node on the map is located at pixels X,Y.
//-----------------------------------------------------------------------------
var
  I, J, B: Integer;
  P      : TPoint;
  R      : TRect;
  N      : TNode;
begin
  for I := MAXCLASS downto 0 do
  begin
    if not Project.IsNode(I) then continue;
    for J := 0 to Project.Lists[I].Count - 1 do
    begin
      B := Map.Options.NodeSize;
      try
        N := Project.GetNode(I, J);
        P := Map.GetNodePoint(N);
        R := Rect(P.X-B, P.Y-B, P.X+B+1, P.Y+B+1);
        if PtInRect(R, Point(X, Y)) then
        begin
          SelectedObject := I;
          SelectedIndex := J;
          Result := True;
          Exit;
        end;
      finally
      end;
    end;
  end;
  Result := False;
end;


function TMapForm.FindSubcatch(const X: Integer; const Y: Integer): Boolean;
//-----------------------------------------------------------------------------
// Determines which subcatchment on the map is located at pixels X,Y.
//-----------------------------------------------------------------------------
var
  I: Integer;
  B: Integer;
  P: TPoint;
  R: TRect;
begin
  for I := 0 to Project.Lists[SUBCATCH].Count-1 do
  begin
    B := Map.Options.SubcatchSize;
    P := Map.GetSubcatchCentroid(Project.GetSubcatch(SUBCATCH, I));
    R := Rect(P.X-B, P.Y-B, P.X+B+1, P.Y+B+1);
    if PtInRect(R, Point(X, Y)) then
    begin
      SelectedObject := SUBCATCH;
      SelectedIndex := I;
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;


function TMapForm.FindGage(const X: Integer; const Y: Integer): Boolean;
//-----------------------------------------------------------------------------
// Determines which raingage on the map is located at pixels X,Y.
//-----------------------------------------------------------------------------
var
  J : Integer;
  R : TRect;
begin
  for J := 0 to Project.Lists[RAINGAGE].Count - 1 do
  begin
    R := Map.GetBoundingRect(RAINGAGE, J);
    if PtInRect(R, Point(X, Y)) then
    begin
      SelectedObject := RAINGAGE;
      SelectedIndex := J;
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function TMapForm.FindLabel(const X: Integer; const Y: Integer): Boolean;
//-----------------------------------------------------------------------------
// Determines which label on the map is located at pixels X,Y.
//-----------------------------------------------------------------------------
var
  I: Integer;
  R: TRect;
begin
  for I := 0 to Project.Lists[MAPLABEL].Count - 1 do
  begin
    R := Map.GetBoundingRect(MAPLABEL, I);
    if PtInRect(R, Point(X, Y)) then
    begin
      SelectedObject := MAPLABEL;
      SelectedIndex := I;
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;


function TMapForm.FindLink(const X: Integer; const Y: Integer): Boolean;
//-----------------------------------------------------------------------------
// Determines which link on the map is located at pixels X,Y.
//-----------------------------------------------------------------------------
var
  I, J    : Integer;
  P1, P2  : TPoint;
  P3, P   : TPoint;
  L       : TLink;
  V       : PVertex;
  Hit     : Boolean;
begin
  // Check each type of link
  for I := 0 to MAXCLASS do
  begin
    if not Project.IsLink(I) then continue;
    Hit := False;
    P := Point(X, Y);

    // Check each link of this type
    for J := 0 to Project.Lists[I].Count - 1 do
    begin

      // Get pixel coords. of link's end points (p1 & p2)
      L := Project.GetLink(I, J);
      if  (Map.GetNodePixPos(L.Node1, P1))
      and (Map.GetNodePixPos(L.Node2, P2)) then
      begin

        // If X,Y close to line between p1 & p2 then select link
        V := L.Vlist.First;
        while V <> nil do
        begin
          P3 := Point(Map.GetXPix(V^.X), Map.GetYpix(V^.Y));
          if Uutils.PtOnLine(P1, P3, P, PIXTOL) then
          begin
            Hit := True;
            Break;
          end;
          P1 := P3;
          V := V^.Next;
        end;
        if not Hit then Hit := Uutils.PtOnLine(P1, P2, P, PIXTOL);
        if Hit then
        begin
          SelectedObject := I;
          SelectedIndex := J;
          Result := True;
          Exit;
        end;
      end;
    end;
  end;
  Result := False;
end;


procedure TMapForm.GetSelectedObject(var Obj: Integer; var Index: Integer);
//-----------------------------------------------------------------------------
//  Retrieves the object class code and index of the currently selected object.
//-----------------------------------------------------------------------------
begin
  Obj := SelectedObject;
  Index := SelectedIndex;
end;


//=============================================================================
//                Methods That Erase, Move & Highlight Objects
//=============================================================================

procedure TMapForm.EraseObject(const ObjType: Integer; const Index: Integer);
//-----------------------------------------------------------------------------
// Erases a specific object from both the map and the project database.
//-----------------------------------------------------------------------------
var
  R : TRect;
  N : TNode;
  L : TLink;
begin
  // If object is a node
  if Project.IsNode(ObjType) then
  begin

    // Delete any references to node as a subcatchment outlet or
    // as a label's anchor point
    N := Project.GetNode(ObjType, Index);
    Project.DeleteSubcatchOutNode(N);
    Project.DeleteLabelAnchors(N);

    // Get bounding map area of all links adjacent to this node & delete them
    R := Map.GetAdjacencyRect(ObjType, Index, True);

    if Uinlet.DeleteInletsByNode(N) then R := Map.Window.MapRect;
  end

  // If object is not a node
  else
  begin
    // If object is a subcatchment, remove it as the outlet of any subcatchment
    if ObjType = SUBCATCH then
      Project.DeleteSubcatchOutSubcatch(Project.GetSubcatch(SUBCATCH, Index));

    // Get the object's bounding rectangle
    R := Map.GetBoundingRect(ObjType, Index);

    // Expand rectangle to include a conduit inlet's capture node
    if Project.IsLink(ObjType) then
    begin
      L := Project.GetLink(ObjType, Index);
      N := Uinlet.GetInletNode(L);
      if N <> nil then UnionRect(R, R, Map.GetNodeRect(N));
    end;
   end;

  // Delete the object and redraw the area it occupied on the map
  HiliteOff;
  Project.DeleteItem(ObjType, Index);
  InvalidateMap(R);
end;


procedure TMapForm.MoveSubcatch(const Index: Integer; const X,Y: Extended);
//-----------------------------------------------------------------------------
// Moves a subcatchment to position X,Y.
//-----------------------------------------------------------------------------
var
  S : TSubcatch;
  R : TRect;
  Dx, Dy: Extended;
begin
  // Get bounding area of subcatchment
  S := Project.GetSubcatch(SUBCATCH, Index);
  if S.Vlist.Count = 0 then
  begin
    S.X := X;
    S.Y := Y;
    S.Vlist.Add(X, Y);
  end;
  R := Map.GetBoundingRect(SUBCATCH, Index);
  if S.OutNode <> nil then UnionRect(R, R, Map.GetNodeRect(S.OutNode));
  if S.OutSubcatch <> nil then UnionRect(R, R, Map.GetSubcatchRect(S.OutSubcatch));

  // Update subcatchment's coordinates
  Dx := X - S.X;
  Dy := Y - S.Y;
  S.X := X;
  S.Y := Y;
  S.Vlist.Move(Dx, Dy);

  // Union bounding area for new position with that of
  // old position and redraw that portion of the map
  if UnionRect(R, R, Map.GetBoundingRect(SUBCATCH, Index))
  then InvalidateMap(R);
  HasChanged := True;
end;


procedure TMapForm.MoveNode(const ObjType, ObjIndex: Integer;
  const X,Y: Extended);
//-----------------------------------------------------------------------------
// Moves a node of type ObjType with index ObjIndex to position X,Y.
//-----------------------------------------------------------------------------
var
  N : TNode;
  L : TLink;
  R : TRect;
  I : Integer;
  Dx, Dy: Extended;
begin
  // Get the bounding area of all links adjacent to the node without
  // deleting them
  N := Project.GetNode(ObjType, ObjIndex);
  R := Map.GetAdjacencyRect(ObjType, ObjIndex, False);
  if (ObjType = STORAGE) and (N.Vlist.Count > 0) then
    UnionRect(R, R, Map.GetBoundingRect(ObjType, ObjIndex));

  // Check if node is the outlet of a subcatchment
  for I := 0 to Project.Lists[SUBCATCH].Count-1 do
  begin
    if Project.GetSubcatch(SUBCATCH, I).OutNode = N then
    begin
      UnionRect(R, R, Map.GetBoundingRect(SUBCATCH, I));
      break;
    end;
  end;

  // Update node's coordinates
  Dx := X - N.X;
  Dy := Y - N.Y;
  N.X := X;
  N.Y := Y;
  if N.Vlist.Count > 0 then N.Vlist.Move(Dx, Dy);

  // Union bounding area for new position with that of
  // old position and redraw that portion of the map
  if UnionRect(R, R, Map.GetBoundingRect(ObjType, ObjIndex))
  then InvalidateMap(R);

  // Update conduit lengths if necessary
  if Uglobals.AutoLength then
  begin
    for I := 0 to Project.Lists[CONDUIT].Count-1 do
    begin
      L := Project.GetLink(CONDUIT, I);
      if (L.Node1 = N) or (L.Node2 = N) then
      begin
        L.Data[CONDUIT_LENGTH_INDEX] := Map.GetLinkLengthStr(CONDUIT, I);
        MainForm.SetChangeFlags;
      end;
    end;
  end;
  Uglobals.HasChanged := True;
end;


procedure TMapForm.MoveLabelRect(const X,Y: Integer);
//-----------------------------------------------------------------------------
// Offsets a label's outline rectangle from its current position
// to position given by X,Y.
//-----------------------------------------------------------------------------
begin
  DrawDotRect(ScrollRect);
  OffsetRect(ScrollRect, X-AnchorX, Y-AnchorY);
  DrawDotRect(ScrollRect);
  AnchorX := X;
  AnchorY := Y;
end;


procedure TMapForm.MoveLabel(const Index: Integer);
//-----------------------------------------------------------------------------
// Moves a map label to the position to which its outline rectangle was moved.
//-----------------------------------------------------------------------------
var
  L : TMapLabel;
  Xp, Yp : Integer;
begin
  DrawDotRect(ScrollRect);
  EraseLabel(Index);
  Xp := ScrollRect.Left;
  Yp := ScrollRect.Top;
  L  := Project.GetMapLabel(Index);
  with L do
  begin
    if (Anchor <> nil) and (Map.ZoomIndex > 0) then
    begin
      X := Anchor.X + (Xp - Map.GetXpix(Anchor.X))*Map.Sfactor;
      Y := Anchor.Y - (Yp - Map.GetYpix(Anchor.Y))*Map.Sfactor;
    end
    else
    begin
      X := Map.GetX(Xp);
      Y := Map.GetY(Yp);
    end;
  end;
  DrawObject(MAPLABEL, Index);
  HasChanged := True;
end;


procedure TMapForm.EraseLabel(const Index: Integer);
//-----------------------------------------------------------------------------
// Erases a map label without deleting it.
//-----------------------------------------------------------------------------
var
  R: TRect;
  L: TMapLabel;
  X: Single;
begin
  HiliteOff;
  L := Project.GetMapLabel(Index);
  R := Map.GetBoundingRect(MAPLABEL, Index);
  X := L.X;
  L.X := MISSING;
  InvalidateMap(R);
  L.X := X;
end;


procedure TMapForm.ReplaceLabel(const Index: Integer; const S: String);
//-----------------------------------------------------------------------------
// Replaces the text of a map label.
//-----------------------------------------------------------------------------
var
  R: TRect;
begin
  HiliteOff;
  R := Map.GetBoundingRect(MAPLABEL, Index);
  with Project.Lists[MAPLABEL] do
  begin
    Strings[Index] := S;
    Project.GetMapLabel(Index).Text := PChar(Strings[Index]);
  end;
  if UnionRect(R, R, Map.GetBoundingRect(MAPLABEL, Index)) then
    InvalidateMap(R);
  HiliteOn;
end;


procedure TMapForm.MoveGage(const Index: Integer; const X,Y: Extended);
//-----------------------------------------------------------------------------
// Moves a rain gage to the position to which its outline rectangle was moved.
//-----------------------------------------------------------------------------
var
  G : TRaingage;
  R : TRect;
begin
  // Get bounding area of gage at its current position
  R := Map.GetBoundingRect(RAINGAGE, Index);

  // Update gage's coordinates
  G := Project.GetGage(Index);
  G.X := X;
  G.Y := Y;

  // Union bounding area for new position with that of
  // old position and redraw that portion of the map
  if UnionRect(R, R, Map.GetBoundingRect(RAINGAGE, Index))
  then InvalidateMap(R);
  HasChanged := True;
end;


procedure TMapForm.EraseGage(const Index: Integer);
//-----------------------------------------------------------------------------
// Erases a map rain gage without deleting it.
//-----------------------------------------------------------------------------
var
  R: TRect;
  G: TRaingage;
  X: Extended;
begin
  HiliteOff;
  G := Project.GetGage(Index);
  R := Map.GetBoundingRect(RAINGAGE, Index);
  X := G.X;
  G.X := MISSING;
  InvalidateMap(R);
  G.X := X;
end;


procedure TMapForm.ChangeHiliteObject(const ObjType, Index: Integer);
//-----------------------------------------------------------------------------
// Changes the currently highlighted object on the map to the object
// of type ObjType whose index is Index.
//-----------------------------------------------------------------------------
begin
  if Vertexing then EndVertexing;
  HiliteObject := ObjType;
  HiliteIndex := Index;
  SelectedLink := nil;
  SelectedSubcatch := nil;
  SelectedNode := nil;
  if HiliteIndex >= 0 then
  begin
    if Project.IsLink(HiliteObject)
    then SelectedLink := Project.GetLink(HiliteObject, HiliteIndex);
    if HiliteObject = SUBCATCH
    then SelectedSubcatch := Project.GetSubcatch(SUBCATCH, HiliteIndex);
    if HiliteObject = STORAGE
    then SelectedNode := Project.GetNode(STORAGE, HiliteIndex);
  end;

  // Enable Vertex Selection button on Map Toolbar if a subcatchment
  // or a link was selected
  ToolButton2.Enabled := (SelectedLink <> nil) or
                         (SelectedSubcatch <> nil) or (SelectedNode <> nil);
  HiliteOff;
  HiliteOn;
end;


procedure TMapForm.HiliteOff;
//-----------------------------------------------------------------------------
// Turns off highlighting of currently selected object.
//-----------------------------------------------------------------------------
begin
  // Disable the highlighting timer.
  if not Hilited then Exit;
  Timer1.Enabled := False;

  // Copy over the highlighted rectangle with the corresponding area
  // of the Map's bitmap.
  Canvas.CopyRect(HiliteRect, Map.Bitmap.Canvas, HiliteRect);

  // Zero-out the highlight rectangle
  HiliteRect := Rect(0,0,0,0);
  Hilited := False;
end;


procedure TMapForm.HiliteOn;
//-----------------------------------------------------------------------------
// Turns on highlighting of the currently selected object.
//-----------------------------------------------------------------------------
var
  P1    : TPoint;
  Hrect : TRect;
  OldColor: TColor;

begin
  // Check that there is something to highlight
  if (HiliteObject < 0) then Exit;
  if (HiliteIndex < 0) then Exit;
  Hrect := NORECT;

  // Highlighted object is a subcatchment:
  if (Project.IsSubcatch(HiliteObject)) and (Map.Options.ShowSubcatchs) then
  begin
    if Vertexing and (SelectedVertex <> nil) then
    begin
      Hrect := DrawVertices(True);
    end
    else
    begin
      P1 := Map.GetSubcatchCentroid(Project.GetSubcatch(SUBCATCH, HiliteIndex));
      Hrect := Rect(P1.X - SubcatchHiliteSize, P1.Y - SubcatchHiliteSize,
                    P1.X + SubcatchHiliteSize+1, P1.Y + SubcatchHiliteSize+1);
    end;
  end

  // Highlighted object is a node:
  else if (Project.IsNode(HiliteObject)) and (Map.Options.ShowNodes) then
  begin
    if (HiliteObject = STORAGE) and Vertexing and (SelectedVertex <> nil) then
    begin
      Hrect := DrawVertices(True);
    end
    else
    begin
      P1 := Map.GetNodePoint(Project.GetNode(HiliteObject, HiliteIndex));
      Hrect := Rect(P1.X - NodeHiliteSize, P1.Y - NodeHiliteSize,
                  P1.X + NodeHiliteSize+1, P1.Y + NodeHiliteSize+1);
    end;
  end

  // Highlighted object is a label:
  else if (HiliteObject = MAPLABEL) and (Map.Options.ShowLabels)
  then Hrect := Map.GetBoundingRect(HiliteObject, HiliteIndex)

  // Highlighted object is a rain gage
  else if (HiliteObject = RAINGAGE) and (Map.Options.ShowGages)
  then Hrect := Map.GetBoundingRect(HiliteObject, HiliteIndex)

  // Highlighted object is a link:
  else if (Project.IsLink(HiliteObject)) and (Map.Options.ShowLinks) then
  begin
    if Vertexing and (SelectedVertex <> nil) then
    begin
      Hrect := DrawVertices(True);
    end
    else if
      Map.GetLinkMidPoint(Project.GetLink(HiliteObject, HiliteIndex), P1) then
        Hrect := Rect(P1.X - LinkHiliteSize, P1.Y - LinkHiliteSize,
                    P1.X + LinkHiliteSize+1, P1.Y + LinkHiliteSize+1);
  end;

  // Highlight the selection rectangle and enable  the highlighting
  // timer if the Blinking option is on.
  if not IsRectEmpty(Hrect) then
  begin
    Hilited := True;
    HiliteRect := Hrect;
    if Vertexing then
    begin
      OldColor := Canvas.Brush.Color;
      Canvas.Brush.Color := MapForeColor[Map.Options.ColorIndex];
      Canvas.FillRect(HiliteRect);
      Canvas.Brush.Color := OldColor;
    end
    else with HiliteBitmap do
    begin
      Width  := HiliteRect.Right - HiliteRect.Left;
      Height := HiliteRect.Bottom - HiliteRect.Top;
      Canvas.CopyRect(Rect(0, 0, Width, Height), Map.Canvas, hRect);
      Timer1Timer(self);
      Timer1.Enabled := (not Vertexing) and Blinking;
    end;
  end;
end;


procedure TMapForm.Timer1Timer(Sender: TObject);
//-----------------------------------------------------------------------------
// Causes the highlighted object to flash by inverting its highlighting
// rectangle.
//-----------------------------------------------------------------------------
begin
  with HiliteBitmap do
  begin
    InvertRect(Canvas.Handle, Rect(0, 0, Width, Height));
    self.Canvas.CopyRect(HiliteRect, Canvas, Rect(0, 0, Width, Height));
  end;
end;


//=============================================================================
//                        Event handlers for PopupMenu1
//                      (Options for editng a map object)
//=============================================================================

procedure TMapForm.PopupMenu1Popup(Sender: TObject);
//-----------------------------------------------------------------------------
// OnPopup event handler for PopupMenu1. Determines which items appear
// in the menu.
//-----------------------------------------------------------------------------
var
  I, J: Integer;
begin
  // Reverse option appears only if current object is a link
  PopupReverse.Visible := Project.IsLink(CurrentList);

  // Vertices option appears if current object is a link,
  // subcatchment or storage node
  PopupVertices.Visible := (PopupReverse.Visible) or (CurrentList = SUBCATCH)
    or (CurrentList = STORAGE);

  // Paste option appears only if project clipboard contains the same
  // class of object
  PopupPaste.Enabled := (CurrentList = Project.Clipboard.ObjType);

  // Fill in the options of the Convert To menu if a node is selected
  if Project.IsNode(CurrentList) then
  begin
    PopupConvert.Visible := True;
    for I := 1 to 5 do
    begin
      J := I + JUNCTION - 1;
      with TMenuItem(FindComponent('ConvertToType' + IntToStr(I))) do
      begin
        Visible := False;
        if (J <= STORAGE) and (J <> CurrentList) then
        begin
          Caption := ObjectLabels[J];
          Visible := True;
        end;
      end;
    end;
  end

  // Fill in the options of the Convert To menu if a link is selected
  else if Project.IsLink(CurrentList) then
  begin
    PopupConvert.Visible := True;
    for I := 1 to 5 do
    begin
      J := I + CONDUIT - 1;
      with TMenuItem(FindComponent('ConvertToType' + IntToStr(I))) do
      begin
        Visible := False;
        if (J <= OUTLET) and (J <> CurrentList) then
        begin
          Caption := ObjectLabels[J];
          Visible := True;
        end;
      end;
    end;
  end
  else PopupConvert.Visible := False;
  HiliteOff;
end;


procedure TMapForm.PopupCopyClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the PopUpCopy menu option. Copies the selected object's
// properties to the internal clipboard.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  I := Project.CurrentItem[CurrentList];
  case CurrentList of
  RAINGAGE:      Uclipbrd.CopyGage(I);
  SUBCATCH:      Uclipbrd.CopySubcatch(I);
  JUNCTION..
  STORAGE:       Uclipbrd.CopyNode(CurrentList, I);
  CONDUIT..
  OUTLET:        Uclipbrd.CopyLink(CurrentList, I);
  MAPLABEL:      Uclipbrd.CopyLabel(I);
  end;
  HiliteOn;
end;


procedure TMapForm.PopupPasteClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the PopUpPaste menu option. Pastes copied properties
// of an object into the selected object.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  I := Project.CurrentItem[CurrentList];
  case CurrentList of
  RAINGAGE:      Uclipbrd.PasteGage(I);
  SUBCATCH:      Uclipbrd.PasteSubcatch(I);
  JUNCTION..
  STORAGE:       Uclipbrd.PasteNode(CurrentList, I);
  CONDUIT..
  OUTLET:        Uclipbrd.PasteLink(CurrentList, I);
  MAPLABEL:      Uclipbrd.PasteLabel(I);
  end;
  //Hilite turned back on in Paste... procedures
end;


procedure TMapForm.PopupDeleteClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the PopupDelete menu option. Deletes the currently
// selected map item from the project.
//-----------------------------------------------------------------------------
begin
  MainForm.BrowserBtnDeleteClick(Sender);
end;


procedure TMapForm.PopupReverseClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for PopupReverse menu option. Reverses the direction
// of the currently selected link.
//-----------------------------------------------------------------------------
begin
  if SelectedLink <> nil then
  begin
    SelectedLink.ReverseNodes;
    SelectedLink.Vlist.Reverse;
    MainForm.SetChangeFlags;
    UpdateEditor(HiliteObject, HiliteIndex);
    Uupdate.UpdateLinkColor(HiliteObject, HiliteIndex, CONDUIT_SLOPE_INDEX);
    HiliteOn;
  end;
end;


procedure TMapForm.ConvertToTypeClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the Convert To popup menu options.
//-----------------------------------------------------------------------------
var
  oldType: Integer;
  oldIndex: Integer;
  newType: Integer;
  newIndex: Integer;
begin
  // Save reference to old object
  oldType := CurrentList;
  oldIndex := Project.CurrentItem[CurrentList];

  // Convert from old to new object type
  if Project.IsNode(CurrentList) then
  begin
    with Sender as TMenuItem do newType := JUNCTION + Tag - 1;
    newIndex := Ubrowser.ConvertNode(CurrentList, oldIndex, newType);
  end
  else if Project.IsLink(CurrentList) then
  begin
    with Sender as TMenuItem do newType := CONDUIT + Tag - 1;
    newIndex := Ubrowser.ConvertLink(CurrentList, oldIndex, newType);
  end
  else exit;

  // Select the new object on the map
  SelectedObject := newType;
  SelectedIndex := newIndex;
  Project.CurrentItem[newType] := newIndex;
  Ubrowser.BrowserUpdate(SelectedObject, SelectedIndex);

  // Delete the old object (without changing any references to its ID)
  Project.DeleteItemOnly(oldType, oldIndex);

  // Redraw the new object
  if oldType = STORAGE then MapForm.RedrawMap
  else DrawObject(newType, newIndex);
end;


procedure TMapForm.PopupVerticesClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the PopupVertices popup menu option. Puts the map
// into Vertex Editing mode.
//-----------------------------------------------------------------------------
begin
  MapButtonClick(ToolButton2);
end;


procedure TMapForm.PopupPropertiesClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the PopupProperties menu option. Displays the property
// editor for currently selected map object.
//-----------------------------------------------------------------------------
begin
  MainForm.BrowserBtnEditClick(Sender);
end;


//=============================================================================
//                      Event Handlers for PopUpMenu2.
//     (Show/hide the various object layers, the legends & backdrop map
//     and invoke the Map Display Options dialog.)
//=============================================================================

procedure TMapForm.PopupObjectsClick(Sender: TObject);
begin
  PopupShowGages.Checked := Map.Options.ShowGages;
  PopupShowSubcatch.Checked := Map.Options.ShowSubcatchs;
  PopupShowNodes.Checked := Map.Options.ShowNodes;
  PopupShowLinks.Checked := Map.Options.ShowLinks;
  PopupShowLabels.Checked := Map.Options.ShowLabels;
  PopupShowBackdrop.Checked := Map.Backdrop.Visible;
  PopupShowBackdrop.Enabled := (Length(Map.Backdrop.Filename) > 0);
end;


procedure TMapForm.PopupLegendsClick(Sender: TObject);
begin
  PopupSubcatchLegend.Enabled := (not QueryFlag) and
                                 (CurrentSubcatchVar <> NOVIEW);
  PopupNodeLegend.Enabled := (not QueryFlag) and (CurrentNodeVar <> NOVIEW);
  PopupLinkLegend.Enabled := (not QueryFlag) and (CurrentLinkVar <> NOVIEW);
  PopupTimeLegend.Enabled := RunFlag;
  PopupSubcatchLegend.Checked := MainForm.MnuSubcatchLegend.Checked;
  PopupNodeLegend.Checked := MainForm.MnuNodeLegend.Checked;
  PopupLinkLegend.Checked := MainForm.MnuLinkLegend.Checked;
  PopupTimeLegend.Checked := MainForm.MnuTimeLegend.Checked;
end;


procedure TMapForm.PopupOptionsClick(Sender: TObject);
begin
  SetMapOptions;
  HiliteOn;
end;


procedure TMapForm.PopupShowObjectsClick(Sender: TObject);
begin
  if (Sender = PopupShowGages)
  or (Sender = MainForm.MnuShowGages)
  then Map.Options.ShowGages := not Map.Options.ShowGages
  else if (Sender = PopupShowSubcatch)
  or (Sender = MainForm.MnuShowSubcatch)
  then Map.Options.ShowSubcatchs := not Map.Options.ShowSubcatchs
  else if (Sender = PopupShowNodes)
  or (Sender = MainForm.MnuShowNodes)
  then Map.Options.ShowNodes := not Map.Options.ShowNodes
  else if (Sender = PopupShowLinks)
  or (Sender = MainForm.MnuShowLinks)
  then Map.Options.ShowLinks := not Map.Options.ShowLinks
  else if (Sender = PopupShowLabels)
  or (Sender = MainForm.MnuShowLabels)
  then Map.Options.ShowLabels := not Map.Options.ShowLabels;
  RedrawMap;
end;


procedure TMapForm.PopupShowBackdropClick(Sender: TObject);
begin
  Map.Backdrop.Visible := not Map.Backdrop.Visible;
  OVMapForm.OVmap.Backdrop.Visible := Map.Backdrop.Visible;
  RedrawMap;
  OVmapForm.Redraw;
end;


procedure TMapForm.PopupSubcatchLegendClick(Sender: TObject);
begin
  ToggleSubcatchLegend;
  HiliteOn;
end;


procedure TMapForm.PopupNodeLegendClick(Sender: TObject);
begin
  ToggleNodeLegend;
  HiliteOn;
end;


procedure TMapForm.PopupLinkLegendClick(Sender: TObject);
begin
  ToggleLinkLegend;
  HiliteOn;
end;


procedure TMapForm.PopupTimeLegendClick(Sender: TObject);
begin
  ToggleTimeLegend;
  HiliteOn;
end;


procedure TMapForm.SetMapOptions;
//-----------------------------------------------------------------------------
// Invokes the MapOptions dialog form to change map display options.
//-----------------------------------------------------------------------------
var
  OldColor: TColor;
  TmpOptions: TMapOptions;
  MapOptionsForm: TMapOptionsForm;
begin
  HiliteOff;
  Uglobals.SetMapBackColors;
  MapOptionsForm := TMapOptionsForm.Create(self);
  with MapOptionsForm do
  try
    OldColor := MapForm.Color;
    TmpOptions := Map.Options;
    SetOptions(TmpOptions);
    SetActivePage(MapOptionsPage);
    if ShowModal = mrOK then
    begin
      GetOptions(TmpOptions);
      if HasChanged then Uglobals.HasChanged := True;
      Map.Options := TmpOptions;
      MapForm.Color := MapBackColor[Map.Options.ColorIndex];
      MapForm.Canvas.Pen.Color := MapForeColor[Map.Options.ColorIndex];
      SubcatchHiliteSize := MaxIntValue([PIXTOL,Map.Options.SubcatchSize+1]);
      NodeHiliteSize := MaxIntValue([PIXTOL,Map.Options.NodeSize+1]);
      LinkHiliteSize := MaxIntValue([PIXTOL,Map.Options.LinkSize+1]);
      if OldColor <> MapForm.Color then Map.RedrawBackdrop;
      RedrawMap;
      DrawSubcatchLegend;
      DrawNodeLegend;
      DrawLinkLegend;
    end;
    GetActivePage(MapOptionsPage);
  finally
    Free;
  end;
end;


//=============================================================================
//                      Event Handlers for PopUpMenu3.
//                         (Vertex editing options)
//=============================================================================

procedure TMapForm.PopupMenu3Popup(Sender: TObject);
begin
  if SelectedSubcatch <> nil then
  begin
    PopupDeleteVertex.Enabled := (SelectedSubcatch.Vlist.Count > 3);
    PopupDeleteAllVertices.Enabled := (SelectedSubcatch.Vlist.Count > 1);
  end
  else if SelectedNode <> nil then
  begin
    PopupDeleteVertex.Enabled := (SelectedNode.Vlist.Count > 3);
    PopupDeleteAllVertices.Enabled := (SelectedNode.Vlist.Count > 1);
  end
  else if SelectedLink <> nil then
  begin
    PopupDeleteVertex.Enabled := (SelectedLink.Vlist.Count > 0);
    PopupDeleteAllVertices.Enabled := (SelectedLink.Vlist.Count > 0);
  end;
end;


procedure TMapForm.PopupAddVertexClick(Sender: TObject);
begin
  AddVertex;
end;


procedure TMapForm.PopupDeleteVertexClick(Sender: TObject);
begin
  DeleteVertex;
end;

procedure TMapForm.PopupDeleteAllVerticesClick(Sender: TObject);
var
  R : TRect;
begin
  R := Map.GetBoundingRect(HiliteObject, HiliteIndex);
  if SelectedLink <> nil then      // Delete all vertices for polylines
    SelectedVlist.Clear
  else                             // Delete all but first vertex for polygons
    SelectedVlist.ClearPolygon;
  HiliteOff;
  InvalidateMap(R);
  if Uglobals.AutoLength
    then Uupdate.UpdateObjectLength(HiliteObject, HiliteIndex);
  HiliteOn;
  HasChanged := True;
end;


procedure TMapForm.PopupQuitEditingClick(Sender: TObject);
begin
  MainForm.SelectorButtonClick;
end;


//=============================================================================
//                       Map Legend Procedures
//=============================================================================

procedure TMapForm.SubcatchLegendBoxDblClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnDoubleClick event handler for the SubcatchLegendBox control.
// Makes legend invisible through call to ToggleSubcatchLegend.
//-----------------------------------------------------------------------------
begin
  ToggleSubcatchLegend;
end;


procedure TMapForm.LinkLegendBoxDblClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnDoubleClick event handler for the LinkLegendBox control.
// Makes legend invisible through call to ToggleLinkLegend.
//-----------------------------------------------------------------------------
begin
  ToggleLinkLegend;
end;


procedure TMapForm.NodeLegendBoxDblClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnDoubleClick event handler for the NodeLegendBox control.
// Makes legend invisible through call to ToggleNodeLegend.
//-----------------------------------------------------------------------------
begin
  ToggleNodeLegend;
end;


procedure TMapForm.ToggleSubcatchLegend;
//-----------------------------------------------------------------------------
// Toggles the display of the Subcatchment legend on/off.
//-----------------------------------------------------------------------------
begin
  with MainForm.MnuSubcatchLegend do
  begin
    Checked := not Checked;
    SubcatchLegendPanel.Visible := Checked;
    PopupSubcatchLegend.Checked := Checked;
  end;
end;


procedure TMapForm.ToggleLinkLegend;
//-----------------------------------------------------------------------------
// Toggles the display of the Link legend on/off.
//-----------------------------------------------------------------------------
begin
  with MainForm.MnuLinkLegend do
  begin
    Checked := not Checked;
    LinkLegendPanel.Visible := Checked;
    PopupLinkLegend.Checked := Checked;
  end;
end;


procedure TMapForm.ToggleNodeLegend;
//-----------------------------------------------------------------------------
// Toggles the display of the Node legend on/off.
//-----------------------------------------------------------------------------
begin
  with MainForm.MnuNodeLegend do
  begin
    Checked := not Checked;
    NodeLegendPanel.Visible := Checked;
    PopupNodeLegend.Checked := Checked;
  end;
end;


procedure TMapForm.ModifySubcatchLegend;
//-----------------------------------------------------------------------------
// Invokes the dialog that modifies the properties of the Subcatchment legend.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  // Invoke dialog through general purpose procedure in Ulegend unit
  if CurrentSubcatchVar >= SUBCATCHQUAL
  then I := SUBCATCHQUAL
  else I := CurrentSubcatchVar;
  if Ulegend.EditLegend(SubcatchLegend[I], CurrentSubcatchVar,
    SubcatchUnits[I].Digits, CurrentPeriod, MapSubcatchColor,
    SubcatchLegendFrame.Framed) then
  begin
    // Redraw map with new set of node colors
    Uoutput.SetSubcatchColors;
    RedrawMap;
    DrawSubcatchLegend;
    HasChanged := True;
  end;
end;


procedure TMapForm.ModifyNodeLegend;
//-----------------------------------------------------------------------------
// Invokes the dialog that modifies the properties of the Node legend.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  // Invoke dialog through general purpose procedure in Umap unit
  if CurrentNodeVar >= NODEQUAL then I := NODEQUAL else I := CurrentNodeVar;
  if Ulegend.EditLegend(NodeLegend[I], CurrentNodeVar, NodeUnits[I].Digits,
    CurrentPeriod, MapNodeColor, NodeLegendFrame.Framed) then
  begin
    // Redraw map with new set of node colors
    Uoutput.SetNodeColors;
    RedrawMap;
    DrawNodeLegend;
    HasChanged := True;
  end;
end;

procedure TMapForm.ModifyLinkLegend;
//-----------------------------------------------------------------------------
// Invokes the dialog that modifies the properties of the Link legend.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  // Invoke dialog through general purpose procedure in Umap unit
  if CurrentLinkVar >= LINKQUAL then I := LINKQUAL else I := CurrentLinkVar;
  if Ulegend.EditLegend(LinkLegend[I], CurrentLinkVar, LinkUnits[I].Digits,
    CurrentPeriod, MapLinkColor, LinkLegendFrame.Framed) then
  begin
    // Redraw map with new set of link colors
    Uoutput.SetLinkColors;
    RedrawMap;
    DrawLinkLegend;
    HasChanged := True;
  end;
end;


procedure TMapForm.DragLegend(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
//-----------------------------------------------------------------------------
// OnMouseDown event handler for the legend Paintboxes. Left button drags
// legend to new position, right button invokes the Legend Editor form.
//-----------------------------------------------------------------------------
const
  SC_DragMove = $F012;  // Magic number for the drag operation.
begin
  // Pressing left button begins a drag event.
  if Button = mbLeft then
  begin
    ReleaseCapture;
    if Sender = SubcatchLegendBox then
    begin
      SubcatchLegendPanel.Perform(WM_SysCommand, SC_DragMove, 0);
      SubcatchLegendFrame.X := SubcatchLegendPanel.Left / MapForm.GetMapClientWidth;
      SubcatchLegendFrame.Y := SubcatchLegendPanel.Top / MapForm.ClientHeight;
    end
    else if Sender = NodeLegendBox then
    begin
      NodeLegendPanel.Perform(WM_SysCommand, SC_DragMove, 0);
      NodeLegendFrame.X := NodeLegendPanel.Left / MapForm.GetMapClientWidth;
      NodeLegendFrame.Y := NodeLegendPanel.Top / MapForm.ClientHeight;
    end
    else if Sender = LinkLegendBox then
    begin
      LinkLegendPanel.Perform(WM_SysCommand, SC_DragMove, 0);
      LinkLegendFrame.X := LinkLegendPanel.Left / MapForm.GetMapClientWidth;
      LinkLegendFrame.Y := LinkLegendPanel.Top / MapForm.ClientHeight;
    end;
  end

  // Pressing right button modifies the map legend.
  else
  begin
    if Sender = SubcatchLegendBox
    then ModifySubcatchLegend
    else if Sender = NodeLegendBox
    then ModifyNodeLegend
    else if Sender = LinkLegendBox
    then ModifyLinkLegend;
  end;
end;


procedure TMapForm.SubcatchLegendBoxPaint(Sender: TObject);
//-----------------------------------------------------------------------------
// OnPaint handler for the SubcatchLegendBox. Redraws the Subcatchment legend.
//-----------------------------------------------------------------------------
begin
  DrawSubcatchLegend;
end;


procedure TMapForm.NodeLegendBoxPaint(Sender: TObject);
//-----------------------------------------------------------------------------
// OnPaint handler for the NodeLegendBox. Redraws the Node legend.
//-----------------------------------------------------------------------------
begin
  DrawNodeLegend;
end;


procedure TMapForm.LinkLegendBoxPaint(Sender: TObject);
//-----------------------------------------------------------------------------
// OnPaint handler for the LinkLegendBox. Redraws the Link legend.
//-----------------------------------------------------------------------------
begin
  DrawLinkLegend;
end;


procedure TMapForm.DrawSubcatchLegend;
//-----------------------------------------------------------------------------
// Oversees the drawing of the Subcatchment legend
// (actual drawing is done by Ulegend.DrawLegend).
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  if (CurrentSubcatchVar = NOVIEW) or (QueryFlag)
  then SubcatchLegendPanel.Visible := False
  else if PopupSubcatchLegend.Checked then
  begin
    SubcatchLegendPanel.Color := MapBackColor[Map.Options.ColorIndex];
    if CurrentSubcatchVar >= SUBCATCHQUAL
    then I := SUBCATCHQUAL
    else I := CurrentSubcatchVar;
    Ulegend.DrawLegend(SubcatchLegendBox.BoundsRect, SubcatchLegendBox.Canvas,
      SubcatchLegendPanel.Color, SubcatchLegendFrame.Framed, SubcatchLegend[I],
      CurrentSubcatchVar, SubcatchUnits[I].Digits, MapSubcatchColor);
    SubcatchLegendPanel.Visible := True;
  end;
end;


procedure TMapForm.DrawNodeLegend;
//-----------------------------------------------------------------------------
// Oversees the drawing of the Node legend
// (actual drawing is done by Ulegend.DrawLegend).
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  if (CurrentNodeVar = NOVIEW) or (QueryFlag)
  then NodeLegendPanel.Visible := False
  else if PopupNodeLegend.Checked then
  begin
    NodeLegendPanel.Color := MapBackColor[Map.Options.ColorIndex];
    if CurrentNodeVar >= NODEQUAL
    then I := NODEQUAL
    else I := CurrentNodeVar;
    Ulegend.DrawLegend(NodeLegendBox.BoundsRect, NodeLegendBox.Canvas,
      NodeLegendPanel.Color, NodeLegendFrame.Framed, NodeLegend[I],
      CurrentNodeVar, NodeUnits[I].Digits, MapNodeColor);
    NodeLegendPanel.Visible := True;
  end;
end;


procedure TMapForm.DrawLinkLegend;
//-----------------------------------------------------------------------------
// Oversees the drawing of the Link legend
// (actual drawing is done by Ulegend.DrawLegend).
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  if (CurrentLinkVar = NOVIEW) or (QueryFlag)
  then LinkLegendPanel.Visible := False
  else if PopupLinkLegend.Checked then
  begin
    LinkLegendPanel.Color := MapBackColor[Map.Options.ColorIndex];
    if CurrentLinkVar >= LINKQUAL
    then I := LINKQUAL
    else I := CurrentLinkVar;
    Ulegend.DrawLegend(LinkLegendBox.BoundsRect, LinkLegendBox.Canvas,
      LinkLegendPanel.Color, LinkLegendFrame.Framed, LinkLegend[I],
      CurrentLinkVar, LinkUnits[I].Digits, MapLinkColor);
    LinkLegendPanel.Visible := True;
  end;
end;


//=============================================================================
//                           Time Panel Procedures
//=============================================================================

procedure TMapForm.TimeLegendPanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
//-----------------------------------------------------------------------------
// OnMouseDown event handler for the TimeLegendPanel.
//-----------------------------------------------------------------------------
const
  SC_DragMove = $F012;  // Magic number for the drag operation.
begin
  // Pressing left button begins a drag event.
  if Button = mbLeft then
  begin
    ReleaseCapture;
    if Sender = TimeLegendPanel then with TimeLegendBox do
    begin
      Perform(WM_SysCommand, SC_DragMove, 0);
      TimeLegendFrame.X := (Left + Width) / MapForm.GetMapClientWidth;
      TimeLegendFrame.Y := Top / MapForm.ClientHeight;
    end;
  end
end;


procedure TMapForm.TimeLegendPanelDblClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnDoubleClick event handler for the TimeLegendPanel.
//-----------------------------------------------------------------------------
begin
  ToggleTimeLegend;
end;


procedure TMapForm.ToggleTimeLegend;
//-----------------------------------------------------------------------------
// Toggles display of the time legend on/off.
//-----------------------------------------------------------------------------
begin
  with MainForm.MnuTimeLegend do
  begin
    Checked := not Checked;
    TimeLegendBox.Visible := Checked;
    PopupTimeLegend.Checked := Checked;
  end;
end;


//=============================================================================
//                      Map Copying & Printing Procedures
//=============================================================================

procedure TMapForm.CopyTo;
//-----------------------------------------------------------------------------
// Copies map to file or to Windows Clipboard in variety of formats.
//-----------------------------------------------------------------------------
var
  CopyToForm: TCopyToForm;
begin
// Create a CopyTo dialog box
  CopyToForm := TCopyToForm.Create(self);
  with CopyToForm do
  try
    // Remove 'Text' from list of format choices
    FormatGroup.Items.Delete(2);

  // Get name of destination file
  // (Empty name means we copy to the Clipboard)
    if ShowModal = mrOK then
    begin

    // Copy map in the selected format
      case FormatGroup.ItemIndex of
        0: CopyToBitmap(DestFileName);
        1: CopyToMetafile(DestFileName);
      end;
    end;
  finally
    Free;
  end;
end;


procedure TMapForm.CopyToBitmap(const Fname: String);
//-----------------------------------------------------------------------------
// Copies the study area map in bitmap format to either a file or
// to the clipboard.
//-----------------------------------------------------------------------------
begin
  if Length(Fname) > 0 then Map.Bitmap.SaveToFile(Fname)
  else Clipboard.Assign(Map.Bitmap);
end;


procedure TMapForm.CopyToMetafile(const Fname: String);
//-----------------------------------------------------------------------------
// Copies the study area map in the Windows enhanced metafile format
// to either a file or to the clipboard.
//-----------------------------------------------------------------------------
var
  MyMetafile: TMetafile;
begin
  MyMetafile := TMetaFile.Create;   // Create the metafile
  try
    DrawToMetafile(MyMetafile);     // Draw map on metafile
    if Length(Fname) > 0 then
      MyMetafile.SaveToFile(Fname)  // Save metafile to file
    else
      ClipBoard.Assign(MyMetafile); // Copy metafile to clipboard
  finally
    MyMetafile.Free;                // Free the metafile
  end;
end;


procedure TMapForm.DrawToMetafile(MyMetafile: TMetafile);
//-----------------------------------------------------------------------------
// Draws study area map on the canvas of MyMetafile.
//-----------------------------------------------------------------------------
var
  MyMetafileCanvas : TMetafileCanvas;
  WidthInMM        : Integer;
  HeightInMM       : Integer;
  WidthInPixels    : Integer;
  HeightInPixels   : Integer;
  MMPerPixelHorz   : Integer;
  MMPerPixelVert   : Integer;
  ClippingRgn      : HRGN;
  DC               : HDC;
  SFx              : Single;
  SFy              : Single;
  R                : TRect;
  I                : Integer;
begin
  //Set dimensions of metafile
  DC := Canvas.Handle;
  WidthInMM := GetDeviceCaps(DC, HORZSIZE);
  HeightInMM := GetDeviceCaps(DC, VERTSIZE);
  WidthInPixels := GetDeviceCaps(DC, HORZRES) - MapToolBar.Width;
  HeightInPixels := GetDeviceCaps(DC, VERTRES);
  MMPerPixelHorz := (WidthInMM*100) div WidthInPixels;
  MMPerPixelVert := (HeightInMM*100) div HeightInPixels;
  MyMetafile.MMWidth := Map.Window.Pwidth*MMPerPixelHorz;
  MyMetafile.MMHeight := Map.Window.Pheight*MMPerPixelVert;

  //Create a canvas for the metafile & a clipping region for the canvas
  MyMetafileCanvas := TMetafileCanvas.Create(MyMetafile, 0);
  ClippingRgn := CreateRectRgn(0, 0, Map.Window.Pwidth, Map.Window.Pheight);

  //Make the metafile canvas the map's canvas & redraw map on it
  try
    with Map do
    begin
      Canvas := TCanvas(MyMetafileCanvas);
      SelectClipRgn(Canvas.Handle, ClippingRgn);
      if Backdrop.Visible then DrawBackdrop(Canvas);
      DrawMap;
      SFx := Window.Pwidth/GetMapClientWidth - 1.0;
      SFy := Window.Pheight/ClientHeight - 1.0;
      if SubcatchLegendPanel.Visible then
      begin
        if CurrentSubcatchVar >= SUBCATCHQUAL
        then I := SUBCATCHQUAL
        else I := CurrentSubcatchVar;
        R := SubcatchLegendPanel.BoundsRect;
        if OffsetRect(R, Round(R.Left*SFx), Round(R.Top*SFy)) then
          Ulegend.DrawLegend(R, Canvas, MapBackColor[Options.ColorIndex],
            SubcatchLegendFrame.Framed, SubcatchLegend[I], CurrentSubcatchVar,
            SubcatchUnits[I].Digits, MapSubcatchColor);
      end;
      if NodeLegendPanel.Visible then
      begin
        if CurrentNodeVar >= NODEQUAL
        then I := NODEQUAL
        else I := CurrentNodeVar;
        R := NodeLegendPanel.BoundsRect;
        if OffsetRect(R, Round(R.Left*SFx), Round(R.Top*SFy)) then
          Ulegend.DrawLegend(R, Canvas, MapBackColor[Options.ColorIndex],
            NodeLegendFrame.Framed, NodeLegend[I], CurrentNodeVar,
            NodeUnits[I].Digits, MapNodeColor);
      end;
      if LinkLegendPanel.Visible then
      begin
        if CurrentLinkVar >= LINKQUAL
        then I := LINKQUAL
        else I := CurrentLinkVar;
        R := LinkLegendPanel.BoundsRect;
        if OffsetRect(R, Round(R.Left*SFx), Round(R.Top*SFy)) then
          Ulegend.DrawLegend(R, Canvas, MapBackColor[Options.ColorIndex],
            LinkLegendFrame.Framed, LinkLegend[I], CurrentLinkVar,
            LinkUnits[I].Digits, MapLinkColor);
      end;
      if TimeLegendBox.Visible then
      begin
        R := TimeLegendBox.BoundsRect;
        if OffsetRect(R, Round(R.Left*SFx), Round(R.Top*SFy))
        then DrawTimeLegend(R, Canvas, MapBackColor[Options.ColorIndex],
                            TimeLegendPanel.Caption);
      end;
    end;

  //Restore map's canvas & free the metafile canvas & clipping region
  finally
    Map.Canvas := Map.Bitmap.Canvas;
    DeleteObject(ClippingRgn);
    MyMetafileCanvas.Free;
  end;
end;


procedure TMapForm.Print(Destination: TDestination);
//-----------------------------------------------------------------------------
// Prints the study area map to Destination (printer or preview form)
//-----------------------------------------------------------------------------
var
  MyPicture     : TPicture;
  MyMetafile    : TMetafile;
  W, H          : Single;
  Left, Top     : Single;
  Width, Height : Single;
  aRatio        : Single;
  OldColorIndex : Integer;

begin
  // Begin a new job on thePrinter's destination (printer or print preview)
  Screen.Cursor := crHourglass;
  with MainForm.thePrinter do
  begin
    BeginJob;
    SetDestination(Destination);
    SetFontInformation('Times New Roman',11,[]);

    // Determine printable area of page
    with PageLayout do
    begin
      Left := LMargin;
      Width := GetPageWidth - LMargin - RMargin;
      Height := GetPageHeight - TMargin - BMargin;
    end;

    // Find width (W) and height (H) of map on printable area
    aRatio := GetMapClientWidth/ClientHeight;
    Uutils.FitFrameToPage(aRatio, Width, Height, W, H);
    Top := GetYPos;
    Left := Left + (Width - W)/2;

    // Temporarily give the map a white background
    OldColorIndex := Map.Options.ColorIndex;
    Map.Options.ColorIndex := 1;

    // Create a metafile to draw the map on
    MyMetafile := TMetaFile.Create;
    try
      // Draw the metafile
      DrawToMetafile(MyMetafile);

      // Create a Picture object to hold the metafile
      MyPicture := TPicture.Create;
      try
        // Assign the metafile to the picture and draw it on the printer
        MyPicture.Assign(MyMetafile);
        StretchGraphic(Left, Top, Left+W, Top+H, MyPicture);
      finally
        MyPicture.Free;
      end;

    finally
      MyMetafile.Free;
    end;

    // Restore map properties
    Map.Options.ColorIndex := OldColorIndex;
    EndJob;
  end;
  Screen.Cursor := crDefault;
end;

end.
