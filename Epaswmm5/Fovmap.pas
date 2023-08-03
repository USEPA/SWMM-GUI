unit Fovmap;

{-------------------------------------------------------------------}
{                    Unit:    Fovmap.pas                            }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Form unit that contains a full-scale outline of the study       }
{   area map with a focus rectangle drawn around the current view   }
{   area. This form is created at start up and remains active       }
{   throughout the session.                                         }
{                                                                   }
{   Draging the rectangle causes the view area on the main map      }
{   (drawn on the MapForm form) to shift accordingly.               }
{-------------------------------------------------------------------}

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, System.Types, Uglobals, Umap, Uutils;

type
  TOVMapForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
    FocusRect: TRect;
    DragRect : TRect;
    Draging  : Boolean;
    DragX    : Integer;
    DragY    : Integer;
    procedure PaintDragRect;
    procedure PaintFocusRect;
    procedure SizeFocusRect;
    procedure WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo);
      message WM_GETMINMAXINFO;
  public
    { Public declarations }
    OVmap    : Tmap;
    NeedsUpdating: Boolean;
    procedure ShowMapExtent;
    procedure Rescale;
    procedure Redraw;
  end;

var
  OVMapForm: TOVMapForm;

implementation

{$R *.DFM}

uses Fmain, Fmap;

procedure TOVMapForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
// OnCreate event handler for the Overview map form.
// Creates an OVmap object of class TMap (see Umap.pas for the definition
// of TMap).
//-----------------------------------------------------------------------------
begin
  OVmap := TMap.Create;
  with OVmap do
  begin
    Options.ShowLabels := False;
    Options.ColorIndex := MAXMAPCOLORINDEX;
    Options.NodeSize := 0;
  end;
  FocusRect := Rect(-1,-1,-1,-1);
  Draging := False;
  NeedsUpdating := False;
  Position := poDefaultPosOnly;
  Left := MainForm.Left + MainForm.ClientWidth - Width;
  Top := MainForm.Top + MainForm.Height - Height;
end;

procedure TOVMapForm.FormClose(Sender: TObject; var Action: TCloseAction);
//-----------------------------------------------------------------------------
// Form's OnClose event handler. Hides, but does not close, the form.
//-----------------------------------------------------------------------------
begin
  MainForm.MnuOVMap.Checked := False;
  Action := caHide;
end;

procedure TOVMapForm.FormDestroy(Sender: TObject);
//-----------------------------------------------------------------------------
// OnDestroy handler for the form. Frees the OVmap object.
//-----------------------------------------------------------------------------
begin
  OVmap.Free;
end;

procedure TOVMapForm.FormResize(Sender: TObject);
//-----------------------------------------------------------------------------
// OnResize handler for the form.  Re-scales the map to fit the window.
//-----------------------------------------------------------------------------
begin
  Rescale;
end;

procedure TOVMapForm.WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo);
//-----------------------------------------------------------------------------
// Message handler for WM_GETMINMAXINFO that restricts the
// smallest size that user can re-size the form to.
//-----------------------------------------------------------------------------
var
  MinMax: PMinMaxInfo;
begin
  inherited;
  MinMax := Msg.MinMaxInfo;
  MinMax^.ptMinTrackSize.X := MINMAPSIZE;
  MinMax^.ptMinTrackSize.Y := MINMAPSIZE;
end;

procedure TOVMapForm.FormPaint(Sender: TObject);
//-----------------------------------------------------------------------------
// OnPaint handler for the form. Copies the study area map outline from
// an internal bitmap to the form's canvas and redraws the focus rectangle.
//-----------------------------------------------------------------------------
begin
  Canvas.Draw(0,0,OVmap.Bitmap);
  PaintFocusRect;
end;

procedure TOVMapForm.FormActivate(Sender: TObject);
//-----------------------------------------------------------------------------
// OnActivate handler for the form.
//-----------------------------------------------------------------------------
begin
  if NeedsUpdating then Redraw;
end;

procedure TOVMapForm.Redraw;
//-----------------------------------------------------------------------------
// Redraws an outline of the study area on the form.
//-----------------------------------------------------------------------------
var
  Color: TColor;
begin
  with OVmap do
  begin
    Color := MapGrayColor[Options.ColorIndex];
    if Backdrop.Visible then Color := clBlack;
    ClearMap;
    RedrawBackdrop;
    if Backdrop.Visible
    then Canvas.CopyRect(Window.MapRect, BackBM.Canvas, Window.MapRect);
    DrawOutline(1,Color);
  end;
  ShowMapExtent;
  NeedsUpdating := False;
end;

procedure TOVMapForm.Rescale;
//-----------------------------------------------------------------------------
// Updates the size and scaling of the Overview map.
//-----------------------------------------------------------------------------
begin
  OVmap.Resize(Rect(0, 0, ClientWidth, ClientHeight));
  OVmap.Rescale;
  Redraw;
end;

procedure TOVMapForm.ShowMapExtent;
//-----------------------------------------------------------------------------
// Displays a rectangle on the Overview map that bounds the current
// viewing extent of the study area shown on the MapForm's map.
// Called from the Fmap unit.
//-----------------------------------------------------------------------------
begin
  SizeFocusRect;
  Refresh;
end;

procedure TOVMapForm.SizeFocusRect;
//-----------------------------------------------------------------------------
// Finds the bounding rectangle of the current viewing extent of MapForm's map.
//-----------------------------------------------------------------------------
var
  X1, X2, Y1, Y2: Single;
  L, T, R, B    : Integer;
begin
  // If MapForm map is at full scale, then don't display the focus rectangle.
  if MapForm.Map.ZoomIndex = 0 then FocusRect := Rect(-1,-1,-1,-1)
  else
  begin
    // Determine world coordinates of zoomed-in area.
    with MapForm.Map.Window do
    begin
      X1 := Woffset.X;
      Y1 := Woffset.Y;
      X2 := X1 + Pwidth*WperP;
      Y2 := Y1 + Pheight*WperP;
    end;

    // Translate these coordinates to the Overview map's scaling.
    with OVmap.Window do
    begin
      L := Round((X1-Woffset.X)/WperP) + Poffset.X;
      T := Poffset.Y - Round((Y2-Woffset.Y)/WperP);
      R := Round((X2-Woffset.X)/WperP) + Poffset.X;
      B := Poffset.Y - Round((Y1-Woffset.Y)/WperP);
    end;

    // Make these the coordinates of the focus rectangle
    FocusRect := Rect(L,T,R,B);
  end;
end;

procedure TOVMapForm.PaintFocusRect;
//-----------------------------------------------------------------------------
// Draws the focus rectangle on the OVMapForm's canvas.
//-----------------------------------------------------------------------------
begin
  with Canvas do
  begin
    Pen.Width := 3;
    Pen.Color := clRed;
    Brush.Style := bsClear;
    with FocusRect do
      Rectangle(Left,Top,Right,Bottom);
  end;
end;

procedure TOVMapForm.PaintDragRect;
//-----------------------------------------------------------------------------
// Draws an outline of the focus rectangle while its being dragged.
//-----------------------------------------------------------------------------
begin
  with Canvas do
  begin
    Pen.Width := 1;
    Pen.Style := psDot;
    Pen.Color := clBlack;
    Brush.Style := bsSolid;
    DrawFocusRect(DragRect);
    Pen.Style := psSolid;
  end;
end;

procedure TOVMapForm.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
//-----------------------------------------------------------------------------
// OnMouseDown event handler. Initiates dragging of the focus rectangle.
//-----------------------------------------------------------------------------
begin
  if (Button = mbLeft) and
  not IsRectEmpty(FocusRect) and
  PtInRect(FocusRect, Point(X,Y)) then
  begin
    Draging := True;
    DragX := X;
    DragY := Y;
    DragRect := FocusRect;
    PaintDragRect;
  end;
end;

procedure TOVMapForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
//-----------------------------------------------------------------------------
// OnMouseUp event handler. Relocates the focus rectangle after dragging ends.
//-----------------------------------------------------------------------------
var
  X1,Y1: Single;
  aRect: TRect;
begin
  if Draging then
  begin
    // Stop dragging
    PaintDragRect;
    Draging := False;

    // Exit if focus rectangle dragged off of form
    if not IntersectRect(aRect,DragRect,ClientRect) then Exit;

    // Relocate the focus rectangle
    FocusRect := DragRect;
    Refresh;

    // Pan the MapForm's map to the position of the focus rectangle
    with OVMap.Window do
    begin
      X1 := Woffset.X + (FocusRect.Left-Poffset.X)*WperP;
      Y1 := Woffset.Y - (FocusRect.Bottom-Poffset.Y)*WperP;
    end;
    with MapForm do
    begin
      Map.Window.Woffset.X := X1;
      Map.Window.Woffset.Y := Y1;
      Map.RedrawBackdrop;
      RedrawMap;
    end;
  end;
end;

procedure TOVMapForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
//-----------------------------------------------------------------------------
// OnMouseMove event handler. Drags an outline of the focus rectangle to
// the current mouse position.
//-----------------------------------------------------------------------------
begin
  if Draging then
  begin
    PaintDragRect;
    OffsetRect(DragRect, X-DragX, Y-DragY);
    PaintDragRect;
    DragX := X;
    DragY := Y;
  end;
end;

end.
