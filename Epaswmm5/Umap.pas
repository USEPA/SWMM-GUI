unit Umap;

{-------------------------------------------------------------------}
{                    Unit:    Umap.pas                              }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21   (5.2.0)                    }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Delphi Pascal unit that defines the TMap object. This object    }
{   contains drawing methods for rendering the Study Area Map on a  }
{   memory bitmap. It also draws a backdrop image, draws the map    }
{   legends, identifies the bounding rectangle for an object, and   }
{   handles map re-scaling.                                         }
{-------------------------------------------------------------------}

interface

uses
  Windows, Graphics, SysUtils, Dialogs, Forms, Classes,
  Controls, Math, Jpeg, System.Types, System.UITypes,
  Uglobals, Uproject, Uutils, Uvertex;

const
  MAX_INT_COORD = 32767;
  MAX_POINTS    = 1000;

type
  // Map physical dimensions
  TMapDimensions = record
    LowerLeft    : TExtendedPoint; //Lower left corner of map
    UpperRight   : TExtendedPoint; //Upper right corner of map
    XperDeg      : Extended;       //Meters per degree longitude
    YperDeg      : Extended;       //Meters per degree latitude
    LengthUCF    : Extended;       //Length units conversion factor
    AreaUCF      : Extended;       //Area units conversion factor
    Units        : TMapUnits;      //Length units
    Digits       : Integer;        //Decimal digits in XY-coords.
  end;

  // Map display window scaling
  TMapWindow = record
    WperP      : Extended;         //World coord. per pixel
    Woffset    : TExtendedPoint;   //World coord. offset
    Poffset    : TPoint;           //Pixel coord. offset
    Pwidth     : Integer;          //Full window width
    Pheight    : Integer;          //Full window height
    MapRect    : TRect;            //Current window rect
  end;

  // Map zoom-in state
  TMapZoomState = record
    ZoomFactor : Extended;
    Xcenter    : Extended;
    Ycenter    : Extended;
  end;

  TBackdropSource = (bdNone, bdFile, bdWebStreet, bdWebTopo, bdWebAerial);

  // Map backdrop image
  TMapBackDrop = record
    Source     : TBackdropSource;  //Source of backdrop map
    Filename   : String;           //File containing backdrop image
    LowerLeft  : TExtendedPoint;   //World coords. of lower-left of image
    UpperRight : TExtendedPoint;   //World coords. of upper-right of image
    Watermark  : Boolean;          //True if backdrop drawn as watermark
    Grayscale  : Boolean;          //True if backdrop drawn in grayscale
    Visible    : Boolean;          //True if backdrop is visible
  end;

  // Map display options
  TMapOptions   = record
    ShowGageIDs       : Boolean;

    ShowSubcatchIDs   : Boolean;
    ShowSubcatchValues: Boolean;
    ShowSubcatchLinks : Boolean;
    SubcatchFillStyle : Integer;
    SubcatchLineSize  : Integer;
    SubcatchSnapTol   : Integer;
    SubcatchSize      : Integer;

    ShowNodeIDs       : Boolean;
    ShowNodeValues    : Boolean;
    ShowNodesBySize   : Boolean;
    ShowNodeBorder    : Boolean;
    NodeSize          : Integer;

    ShowLinkIDs       : Boolean;
    ShowLinkValues    : Boolean;
    ShowLinksBySize   : Boolean;
    ShowLinkBorder    : Boolean;
    LinkSize          : Integer;

    ShowGages         : Boolean;
    ShowSubcatchs     : Boolean;
    ShowNodes         : Boolean;
    ShowLinks         : Boolean;
    ShowNodeSymbols   : Boolean;
    ShowLinkSymbols   : Boolean;

    ShowLabels        : Boolean;
    LabelsTranspar    : Boolean;
    NotationTranspar  : Boolean;
    NotationSize      : Integer;

    ArrowStyle        : TArrowStyle;
    ArrowSize         : Integer;
    ColorIndex        : Integer;

    NotationZoom      : Integer;
    LabelZoom         : Integer;
    SymbolZoom        : Integer;
    ArrowZoom         : Integer;
  end;

const

  DefMapDimensions: TMapDimensions =
    (LowerLeft : (X:0.00; Y:0.00);
     UpperRight: (X:10000.00; Y:10000.00);
     XperDeg   : 111195; //Meters per degree of longitude on spherical earth
     YperDeg   : 111195;
     LengthUCF : 1.0;
     AreaUCF   : 2.2956e-5;  // acres per sq. foot
     Units     : muNone;
     Digits    : 3);

  DefMapBackdrop: TMapBackdrop =
    (Source    : bdNone;
     Filename  : '';
     LowerLeft : (X:0.00; Y:0.00);
     UpperRight: (X:0.00; Y:0.00);
     Watermark : False;
     Grayscale : False;
     Visible   : False);

  DefMapOptions: TMapOptions =
    (ShowGageIDs        : False;
     ShowSubcatchIDs    : False;
     ShowSubcatchValues : False;
     ShowSubcatchLinks  : True;
     SubcatchFillStyle  : Ord(bsBDiagonal);
     SubcatchLineSize   : 1;
     SubcatchSnapTol    : 0;
     SubcatchSize       : 5;
     ShowNodeIDs        : False;
     ShowNodeValues     : False;
     ShowNodesBySize    : False;
     ShowNodeBorder     : True;
     NodeSize           : 3;
     ShowLinkIDs        : False;
     ShowLinkValues     : False;
     ShowLinksBySize    : False;
     ShowLinkBorder     : False;
     LinkSize           : 1;
     ShowGages          : True;
     ShowSubcatchs      : True;
     ShowNodes          : True;
     ShowLinks          : True;
     ShowNodeSymbols    : True;
     ShowLinkSymbols    : True;
     ShowLabels         : True;
     LabelsTranspar     : True;
     NotationTranspar   : False;
     NotationSize       : 7;
     ArrowStyle         : asNone;
     ArrowSize          : 2;
     ColorIndex         : 1;
     NotationZoom       : 100;
     LabelZoom          : 100;
     SymbolZoom         : 100;
     ArrowZoom          : 100);

var
  Points  : array[0..MAX_POINTS] of TPoint;

type

  TLinkSymbol = (lsArrow, lsPump, lsValve, lsCheckValve);

  TMap = class(TObject)          // Map object
    Canvas    : TCanvas;         // Display canvas
    Bitmap    : TBitmap;         // Bitmap containing the drawn map
    BackBM    : TBitmap;         // Bitmap containing backdrop image
    GageBM    : TBitmap;         // Bitmap containing raingage image
    InletBM   : TBitmap;         // Bitmap containing inlet symbol
    Window    : TMapWindow;      // Display window sizing info
    Dimensions: TMapDimensions;  // Physical map dimensions
    Options   : TMapOptions;     // Display options
    Backdrop  : TMapBackdrop;    // Backdrop image info
    Sfactor   : Extended;        // scale factor at 0 zoom
    ZoomState : array [0..10] of TMapZoomState;
    ZoomIndex : Integer;         // Current zoom-in level
    ZoomRatio : Integer;
    constructor Create;
    destructor  Destroy; override;

    procedure ClearMap;
    procedure ConvertMapUnits(var Dx, Dy: Extended);

    procedure DrawArrow(L: TLink; P1: TPoint; P2: TPoint);
    procedure DrawArrowHead(const X: Integer; const Y: Integer;
              const Style: TArrowStyle; const Size: Integer;
              const Aratio: Single; const Asin: Extended;
              const Acos: Extended);
    function  DrawBackdrop(BDCanvas: TCanvas): Boolean;
    procedure DrawDivider(const X: Integer; const Y: Integer; const Size: Integer);
    procedure DrawForeground;
    procedure DrawGages;
    procedure DrawGageIDLabel(const Index: Integer; const P: TPoint);

    procedure DrawInletSymbol(L: TLink; P1: TPoint; P2: TPoint);
    procedure DrawLabels;
    procedure DrawLink(const P1: TPoint; const P2: TPoint; L: TLink);
    procedure DrawLinkIDLabel(const ObjType: Integer; const Index: Integer;
              const P1: TPoint; const P2: TPoint; const Size: Integer);
    procedure DrawLinks;
    procedure DrawLinkSymbol(const P1: TPoint; const P2: TPoint;
              const Symbol: TLinkSymbol);
    procedure DrawLinkValueLabel(const ObjType: Integer; const Index: Integer;
              const P1: TPoint; const P2: TPoint; const Size: Integer;
              const S: String);

    procedure DrawMap;
    procedure DrawNode(const X: Integer; const Y: Integer;
              const Size: Integer);
    procedure DrawNodeIDLabel(const ObjType: Integer; const Index: Integer;
              const P: TPoint; const Size: Integer);
    procedure DrawNodes;
    procedure DrawNodeValueLabel(const ObjType: Integer; const Index: Integer;
              const P: TPoint; const Size: Integer; const S: String);

    procedure DrawObject(const ObjType: Integer; const Index: Integer);
    procedure DrawOutfall(const X: Integer; const Y: Integer;
              const Size: Integer);
    procedure DrawOutline(const LineWidth: Integer; const LineColor: TColor);
    procedure DrawPumpSymbol(const X: Integer; const Y: Integer;
              const Size: Integer; const Direction: Integer;
              const Asin: Extended; const Acos: Extended);

    procedure DrawStorage(const X: Integer; const Y: Integer;
              const Size: Integer);
    procedure DrawStoragePolygon(const Index: Integer);
    procedure DrawSubcatch(const Index: Integer);
    procedure DrawSubcatchments;

    function  GetAdjacencyRect(const ObjType: Integer; const Index: Integer;
              const DeleteLinks: Boolean): TRect;
    function  GetArea(Points: array of TPoint; N: Integer): Extended;
    procedure GetBackdropBounds(const PicWidth: Integer;
              const PicHeight: Integer);
    function  GetBoundingRect(const ObjType: Integer;
              const Index: Integer): TRect;
    function  GetDistance(Points: array of TPoint; N: Integer): Extended;
    function  GetGageRect(G: TRaingage): TRect;
    function  GetLabelRect(L: TMapLabel): TRect;
    function  GetLinkLengthStr(const LinkType: Integer;
              const Index: Integer): String;
    function  GetLinkMidpoint(L: TLink; var P: TPoint): Boolean;
    procedure GetLinkMidsegment(L: TLink; var Pa, Pb: TPoint);
    function  GetLinkRect(L: TLink): TRect;
    function  GetPolygonPoints(Vlist: TVertexList):Integer;
    function  GetPolygonRect(X, Y: Extended; Pbuf: Integer; Vlist: TVertexList): TRect;
    function  GetNodePixPos(N: TNode; var P: TPoint): Boolean;
    function  GetNodePoint(N: TNode): TPoint;
    function  GetNodeRect(N: TNode): TRect;
    function  GetSubcatchAreaStr(const Index: Integer): String;
    function  GetSubcatchCentroid(S: TSubcatch): TPoint;
    function  GetSubcatchRect(S: TSubcatch): TRect;
    function  GetX(const X: Integer): Extended;
    function  GetY(const Y: Integer): Extended;
    function  GetXpix(const X: Extended): Integer;
    function  GetYpix(const Y: Extended): Integer;

    function  RedrawBackdrop: Boolean;
    procedure Rescale;
    procedure Resize(const Rect: TRect);

    procedure ResizeBitmap(var Bmap: TBitmap; const W,H: Integer);
    procedure ResizeWindow(const Rect: TRect);

    procedure SetLinkColor(const ObjType: Integer; const Index: Integer);
    function  SetLinkSize: Integer;
    procedure SetNodeColor(const ObjType: Integer; const Index: Integer);
    function  SetNodeSize: Integer;
    procedure SetSubcatchColor(const Index: Integer);
    procedure SnapSubcatch(const Index: Integer);
    function  SnapVertex(V1: PVertex; V2: PVertex; const X1: Integer;
                         const Y1: Integer): Boolean;
    procedure UpdateBounds(var R: TRect; const P: TPoint);

  end;

implementation

uses
  Uoutput, Uinlet;

var
  P1, P2: TPoint;
  LastColorIndex: Integer;
  CharHeight: Integer;
  BackColor: TColor;
  ForeColor: TColor;
  GrayColor: TColor;


//=============================================================================
//                         TMap Constructor & Destructor
//=============================================================================

constructor TMap.Create;
begin
  inherited Create;
  Bitmap := TBitmap.Create;
  if Bitmap <> nil then Canvas := Bitmap.Canvas;
  BackBM := TBitmap.Create;
  Bitmap.PixelFormat := pf24Bit;
  BackBM.PixelFormat := pf24Bit;
  GageBM := TBitmap.Create;
  GageBM.Transparent := False;
  InletBM := TBitmap.Create;
  InletBM.Transparent := False;
  Dimensions := DefMapDimensions;
  Options := DefMapOptions;
  Backdrop := DefMapBackdrop;
  ZoomIndex := 0;
  ZoomState[0].ZoomFactor := 1.0;
end;

destructor TMap.Destroy;
begin
  Bitmap.Free;
  BackBM.Free;
  GageBM.Free;
  InletBM.Free;
  inherited Destroy;
end;


//=============================================================================
//                         Backdrop Drawing Methods
//=============================================================================

function TMap.RedrawBackdrop: Boolean;
//-----------------------------------------------------------------------------
//  Redraws backdrop image on the Map object's backdrop bitmap.
//-----------------------------------------------------------------------------
begin
  if Backdrop.Visible
  then Result := DrawBackdrop(BackBM.Canvas)
  else Result := True;
end;


procedure TMap.GetBackdropBounds(const PicWidth: Integer;
  const PicHeight: Integer);
//-----------------------------------------------------------------------------
//  Finds bounding rectangle of backdrop that fills map extent
//  while preserving its aspect ratio.
//-----------------------------------------------------------------------------
var
  Wpic   : Extended;
  Hpic   : Extended;
  Wwin   : Extended;
  Hwin   : Extended;
  R      : Extended;
begin
  // Compute picture & window width & height in world coords.
  Wpic := PicWidth * Sfactor;
  Hpic := PicHeight * Sfactor;
  Wwin := Window.Pwidth * Sfactor;
  Hwin := Window.Pheight * Sfactor;

  // Re-scale if picture is wider or taller than map window
  if Wpic > Wwin then
  begin
    R := Wwin / Wpic;
    Wpic := R * Wpic;
    Hpic := R * Hpic;
  end;
  if Hpic > Hwin then
  begin
    R := Hwin / Hpic;
    Wpic := R * Wpic;
    Hpic := R * Hpic;
  end;

  // Set bottom left & top right coords. of backdrop
  with Backdrop do
  begin
    LowerLeft.X  := Dimensions.LowerLeft.X;
    UpperRight.X := LowerLeft.X + Wpic;
    UpperRight.Y := Dimensions.UpperRight.Y;
    LowerLeft.Y  := UpperRight.Y - Hpic;
  end;
end;


function TMap.DrawBackdrop(BDCanvas: TCanvas): Boolean;
//-----------------------------------------------------------------------------
//  Draws map's backdrop image on specified Canvas.
//-----------------------------------------------------------------------------
var
  Picture     : TPicture;
  R           : TRect;
begin
  // Create a TPicture object to hold the backdrop image
  Result := False;
  Picture := TPicture.Create;

  try
    if Backdrop.Source = bdFile then
    begin
      // Retrieve the backdrop picture
      if not FileExists(Backdrop.FileName) then exit;
      Picture.LoadFromFile(Backdrop.FileName);

      // Find coords. of bounding rectangle if none exist
      with Backdrop do
      begin
        if LowerLeft.X = UpperRight.X
        then GetBackdropBounds(Picture.Width, Picture.Height);
        R := Rect(GetXpix(LowerLeft.X),  GetYpix(UpperRight.Y),
                  GetXpix(UpperRight.X), GetYpix(LowerLeft.Y));
      end;
    end;

    // Draw the picture in the canvas
    Uutils.Cls(BDCanvas, Window.MapRect, MapBackColor[Options.ColorIndex]);
    BDCanvas.StretchDraw(R, Picture.Graphic);
    if Backdrop.Grayscale then Uutils.AdjustBitmap(BackBM, atGrayscale);
    if Backdrop.Watermark then Uutils.AdjustBitmap(BackBM, atWatermark);
    Result := True;

  // Free the Picture
  finally
    Picture.Free;
  end;
end;


//=============================================================================
//                          Map Drawing Methods
//=============================================================================

procedure TMap.ClearMap;
//-----------------------------------------------------------------------------
//  Clears map to background color.
//-----------------------------------------------------------------------------
begin
  Uutils.Cls(Canvas,Window.MapRect,MapBackColor[Options.ColorIndex]);
end;


procedure TMap.DrawMap;
//-----------------------------------------------------------------------------
//  Draws entire map (backdrop + foreground) on current canvas.
//-----------------------------------------------------------------------------
begin
  ClearMap;
  if Backdrop.Visible
  then Canvas.CopyRect(Window.MapRect, BackBM.Canvas, Window.MapRect);
  DrawForeground;
end;


procedure TMap.DrawForeground;
//-----------------------------------------------------------------------------
//  Draws foreground of map (i.e., SWMM's visual objects) on current canvas.
//-----------------------------------------------------------------------------
begin
  // Assign values to global colors
  ForeColor := MapForeColor[Options.ColorIndex];
  BackColor := MapBackColor[Options.ColorIndex];
  GrayColor := MapGrayColor[Options.ColorIndex];

  // Setup pen & brush to draw on map's Canvas
  Canvas.Pen.Color := ForeColor;
  Canvas.Brush.Color := ForeColor;
  Canvas.Brush.Style := bsSolid;

  // Set Canvas's font for ID/Value labeling
  Canvas.Font.Name := 'Arial';
  Canvas.Font.Size := Options.NotationSize;
  Canvas.Font.Style := [];
  CharHeight := Canvas.TextHeight('[');

  // Draw subcatchments, links, nodes, rain gages & labels
  DrawSubcatchments;
  DrawLinks;
  DrawNodes;
  DrawGages;
  DrawLabels;
  LastColorIndex := -999;
end;


procedure TMap.DrawGages;
//-----------------------------------------------------------------------------
//  Draws all rain gage objects.
//-----------------------------------------------------------------------------
var
  J: Integer;
begin
  if Options.ShowGages then
    for J := 0 to Project.Lists[RAINGAGE].Count - 1 do
      DrawObject(RAINGAGE, J);
  SetBkMode(Canvas.Handle, OPAQUE);
end;


procedure TMap.DrawSubcatchments;
//-----------------------------------------------------------------------------
//  Draws all subcatchment polygons.
//-----------------------------------------------------------------------------
var
  J: Integer;
begin
  LastColorIndex := -999;
  if Options.ShowSubcatchs then
    for J := 0 to Project.Lists[SUBCATCH].Count - 1 do
    begin
      DrawObject(SUBCATCH, J);
    end;
end;


procedure TMap.DrawLinks;
//-----------------------------------------------------------------------------
//  Draws all conduits, pumps, orifices, weirs, & outlets.
//-----------------------------------------------------------------------------
var
  I, J : Integer;
begin
  if Options.NotationTranspar then SetBkMode(Canvas.Handle, TRANSPARENT);
  LastColorIndex := -999;
  Canvas.Pen.Width := Options.LinkSize;
  if Options.ShowLinks then for I := CONDUIT to OUTLET do
  begin
    for J := 0 to Project.Lists[I].Count - 1 do DrawObject(I, J);
  end;
  Canvas.Pen.Width := 1;
  SetBkMode(Canvas.Handle, OPAQUE);
end;


procedure TMap.DrawNodes;
//-----------------------------------------------------------------------------
//  Draws all junctions, outfalls, dividers & storage units.
//-----------------------------------------------------------------------------
var
  I, J : Integer;
begin
  if Options.NotationTranspar then SetBkMode(Canvas.Handle, TRANSPARENT);
  LastColorIndex := -999;
  if Options.ShowNodes then for I := JUNCTION to STORAGE do
  begin
    for J := 0 to Project.Lists[I].Count - 1 do DrawObject(I, J);
  end;
  SetBkMode(Canvas.Handle, OPAQUE);
end;


procedure TMap.DrawLabels;
//-----------------------------------------------------------------------------
//  Draws all map labels.
//-----------------------------------------------------------------------------
var
  J: Integer;
  ZoomFlag: Boolean;
begin
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := BackColor;
  Canvas.Pen.Color := ForeColor;
  Canvas.Font.Color := ForeColor;
  if (Options.ShowLabels) then
  begin
    ZoomFlag := (ZoomRatio >= Options.LabelZoom);
    if Options.LabelsTranspar then SetBkMode(Canvas.Handle, TRANSPARENT);
    for J := 0 to Project.Lists[MAPLABEL].Count - 1 do
      if ZoomFlag then
      begin
        if Project.GetMapLabel(J).X <> MISSING then DrawObject(MAPLABEL, J);
      end;
    SetBkMode(Canvas.Handle, OPAQUE);
  end;
end;


procedure TMap.DrawOutline(const LineWidth: Integer; const LineColor: TColor);
//-----------------------------------------------------------------------------
//  Draws map outline for display of the Overview map.
//-----------------------------------------------------------------------------
var
  I, J    : Integer;
  L       : TLink;
  R       : TRect;
begin
  // Setup canvas's pen
  LastColorIndex := -999;
  Canvas.Pen.Color := LineColor;
  Canvas.Pen.Width := 1;

  // Draw outline of subcatchment polygons
  for J := 0 to Project.Lists[SUBCATCH].Count - 1 do
  begin
    R := GetBoundingRect(SUBCATCH, J);
    if not IntersectRect(R, R, Window.MapRect) then continue;
    I := GetPolygonPoints(Project.GetSubcatch(SUBCATCH, J).Vlist);
    if I > 0 then Canvas.PolyLine(Slice(Points, I));
  end;

  // Draw all link objects
  for I := 0 to MAXCLASS do
  begin
    if not Project.IsLink(I) then continue;
    for J := 0 to Project.Lists[I].Count - 1 do
    begin
      L := Project.GetLink(I, J);
      if not GetNodePixPos(L.Node1, P1) then Continue;
      if not GetNodePixPos(L.Node2, P2) then Continue;
      if not PtInRect(Window.MapRect, P1)
      and not PtInRect(Window.MapRect, P2) then Continue;
      DrawLink(P1, P2, L);
    end;
  end;
  Canvas.Pen.Width := 1;
end;


//=============================================================================
//                        Object Drawing Methods
//=============================================================================

procedure TMap.DrawObject(const ObjType: Integer; const Index: Integer);
//-----------------------------------------------------------------------------
//  Draws the object identified by ObjType and Index on map.
//-----------------------------------------------------------------------------
var
  R : TRect;
  L : TLink;
  Size  : Integer;
  Offset: Integer;
  Color: TColor;
begin
  // Check if object falls within current display window
  // (GetBoundingRect also gets global variables P1 & P2
  // that define the object's bounding rectangle).
  R := GetBoundingRect(ObjType, Index);
  if not IntersectRect(R, R, Window.MapRect) then Exit;

  // Object is a rain gage
  if (ObjType = RAINGAGE) then
  begin
    Canvas.Draw(P1.X, P1.Y, GageBM);
    if (ZoomRatio >= Options.NotationZoom)
    and (Options.ShowGageIDs)
    then DrawGageIDLabel(Index, P1);
  end

  // Object is a subcatchment
  else if (ObjType = SUBCATCH) then
  begin
    // Set color & draw subcatcment
    SetSubcatchColor(Index);
    DrawSubcatch(Index);

    // Add notation if called for
    if (ZoomRatio >= Options.NotationZoom) then
    begin
      Size := Options.SubcatchSize;
      if (Options.ShowSubcatchIDs)
      then DrawNodeIDLabel(ObjType, Index, P1, Size);
      if (Options.ShowSubcatchValues) and (CurrentSubcatchVar > NOVIEW)
      then DrawNodeValueLabel(ObjType, Index, P1, Size,
           Uoutput.GetSubcatchValStr(CurrentSubcatchVar, CurrentPeriod, Index));
    end;
  end

  // Object is a Node
  else if Project.IsNode(ObjType) then
  begin
    // Determine color & size
    SetNodeColor(ObjType, Index);
    Size := SetNodeSize;

    // Draw the node
    if ObjType = STORAGE then
    begin
     DrawStoragePolygon(Index);
     DrawStorage(P1.X, P1.Y, Size);
    end
    else if ObjType = DIVIDER then DrawDivider(P1.X, P1.Y, Size)
    else if ObjType = OUTFALL then DrawOutfall(P1.X, P1.Y, Size)
    else DrawNode(P1.X, P1.Y, Size);

    // Add notation if called for
    if (ZoomRatio >= Options.NotationZoom) then
    begin
      if (Options.ShowNodeIDs) then DrawNodeIDLabel(ObjType, Index, P1, Size);
      if (Options.ShowNodeValues) and (CurrentNodeVar > NOVIEW)
      then DrawNodeValueLabel(ObjType, Index, P1, Size,
             Uoutput.GetNodeValStr(CurrentNodeVar, CurrentPeriod,
                                   ObjType, Index));
    end;
  end

  // Object is a Link
  else if Project.IsLink(ObjType) then
  begin
    // Determine color & size and draw the link
    L := Project.GetLink(ObjType, Index);
    SetLinkColor(ObjType, Index);
    Size := SetLinkSize;

    // Draw border if called for
    // (Line size adjusted to insure that border gets displayed)
    if Options.ShowLinkBorder then
    begin
      Color := Canvas.Pen.Color;
      if Color <> ForeColor then
      begin
        Canvas.Pen.Color := ForeColor;
        if Size = 4 then Size := 3
        else if Size = 5 then Size := 6;
        Canvas.Pen.Width := Size + 2;
        DrawLink(P1, P2, L);
        Canvas.Pen.Color := Color;
      end;
    end;

    // Draw link in the proper color
    Offset := Size;
    Canvas.Pen.Width := Size;
    DrawLink(P1, P2, L);
    Canvas.Pen.Width := 1;
    GetLinkMidSegment(L, P1, P2);
    Canvas.Pen.Color := ForeColor;

    // Draw inlet if link is a conduit with an inlet
    if (ObjType = CONDUIT) and (Options.ShowLinkSymbols)
    and (L.Inlet <> nil) then DrawInletSymbol(L, P1, P2);

    // Draw object's symbol & flow direction arrow if called for
    case ObjType of
    CONDUIT:
      if (Options.ArrowStyle <> asNone) then
      begin
        DrawArrow(L, P1, P2);
        Offset := Offset + Options.ArrowSize div 2;
      end;
    PUMP:
      if (Options.ShowLinkSymbols) and (ZoomRatio >= Options.SymbolZoom) then
      begin
        DrawLinkSymbol(P1, P2, lsPump);
        Offset := Offset + SYMBOLSIZE;
      end
      else if (Options.ArrowStyle <> asNone) then
      begin
        DrawArrow(L, P1, P2);
        Offset := Offset + Options.ArrowSize div 2;
      end;
    ORIFICE, WEIR, OUTLET:
      if (Options.ShowLinkSymbols) and (ZoomRatio >= Options.SymbolZoom) then
      begin
        DrawLinkSymbol(P1, P2, lsValve);
        Offset := Offset + SYMBOLSIZE;
      end
      else if (Options.ArrowStyle <> asNone) then
      begin
        DrawArrow(L, P1, P2);
        Offset := Offset + Options.ArrowSize div 2;
      end;
    end;

    // Add link notation if called for
    if (ZoomRatio >= Options.NotationZoom) then
    begin
      if (Options.ShowLinkIDs)
      then DrawLinkIDLabel(ObjType, Index, P1, P2, Offset);
      if (Options.ShowLinkValues) and (CurrentLinkVar > NOVIEW)
      then DrawLinkValueLabel(ObjType, Index, P1, P2, Offset,
             Uoutput.GetLinkValStr(CurrentLinkVar, CurrentPeriod, ObjType, Index));
    end;
  end

  // Object is a Label
  else if (ObjType = MAPLABEL) then
  begin
    with Project.GetMapLabel(Index), Canvas do
    begin
      Font.Name := FontName;
      Font.Size := FontSize;
      Font.Style := [];
      if FontBold then Font.Style := Font.Style + [fsBold];
      if FontItalic then Font.Style := Font.Style + [fsItalic];
      Canvas.TextOut(P1.X, P1.Y, Text);
    end;
  end;
end;


procedure TMap.DrawSubcatch(const Index: Integer);
//-----------------------------------------------------------------------------
// Draws a subcatchment on the map.
//-----------------------------------------------------------------------------
var
  OldStyle: TBrushStyle;
  OldWidth: Integer;
  OldColor: TColor;
  S       : TSubcatch;
  B       : Integer;
  C       : TPoint;
  P       : TPoint;
  R       : TRect;
  N       : Integer;
begin

  // Save current pen & brush attributes
  OldStyle := Canvas.Brush.Style;
  OldWidth := Canvas.Pen.Width;
  OldColor := Canvas.Pen.Color;

  // Retrieve the vertex points of the subcatchment's polygon
  S := Project.GetSubcatch(SUBCATCH, Index);
  N := GetPolygonPoints(S.Vlist);
  if N = 0 then Exit;
  Canvas.Pen.Width := 1;
  if Options.SubcatchLineSize > 0 then
  begin
    // Draw the polygon
    Canvas.Brush.Style := TBrushStyle(Options.SubcatchFillStyle);
    Canvas.Polygon(Slice(Points, N-1));

    // Draw the polygon's frame
    Canvas.Pen.Width := Options.SubcatchLineSize;
    Canvas.Pen.Color := ForeColor;
    Canvas.PolyLine(Slice(Points, N));
  end;

  // Draw the subcatchment's centroid symbol
  Canvas.Pen.Width := 1;
  Canvas.Pen.Color := ForeColor;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := OldColor;
  B := Options.SubcatchSize;
  C := GetSubcatchCentroid(S);
  R := Rect(C.X - B, C.Y - B, C.X + B + 1, C.Y + B + 1);
  Canvas.Rectangle(R);

  // Draw outlet connection
  if Options.ShowSubcatchLinks then
  begin
    N := 1;
    if S.OutNode <> nil then P := GetNodePoint(S.OutNode)
    else if S.OutSubcatch <> nil then P := GetSubcatchCentroid(S.OutSubcatch)
    else N := 0;
    if N > 0 then with Canvas do
    begin
      Brush.Style := bsClear;
      Pen.Width := 1;
      Pen.Style := psDot;
      Pen.Color := ForeColor;
      MoveTo(C.X, C.Y);
      LineTo(P.X, P.Y);
      Pen.Style := psSolid;
    end;
  end;

  // Restore current pen & brush
  Canvas.Brush.Style := OldStyle;
  Canvas.Brush.Color := OldColor;
  Canvas.Pen.Color := OldColor;
  Canvas.Pen.Width := OldWidth;
end;


function TMap.GetPolygonPoints(Vlist: TVertexList):Integer;
//-----------------------------------------------------------------------------
//  Retrieves the vertex points of a subcatchment's outline.
//-----------------------------------------------------------------------------
var
  V: PVertex;
  I: Integer;
begin
  Result := 0;
  if Vlist = nil then exit;
  I := 0;
  with Vlist do
  begin
    V := First;
    while (V <> nil) and (I < MAX_POINTS) do
    begin
      Points[I] := Point(GetXpix(V^.X), GetYpix(V^.Y));
      Inc(I);
      V := V^.Next;
    end;
  end;
  if I > 0 then
  begin
    V := Vlist.First;
    Points[I] := Point(GetXpix(V^.X), GetYpix(V^.Y));
    Inc(I);
  end;
  Result := I;
end;


procedure TMap.DrawNode(const X: Integer; const Y: Integer;
  const Size: Integer);
//-----------------------------------------------------------------------------
//  Draws a node at location X,Y with size Size.
//-----------------------------------------------------------------------------
begin
  Canvas.Ellipse(X-Size, Y-Size, X+Size+1, Y+Size+1);
end;


procedure TMap.DrawOutfall(const X: Integer; const Y: Integer;
  const Size: Integer);
//-----------------------------------------------------------------------------
//  Draws an Outfall node at location X,Y with size Size.
//-----------------------------------------------------------------------------
var
  Poly: array[0..2] of TPoint;
  W: Integer;
begin
  W := Size;
  Poly[0] := Point(X-W, Y-W);
  Poly[1] := Point(X, Y+W);
  Poly[2] := Point(X+W, Y-W);
  Canvas.Polygon(Poly);
end;


procedure TMap.DrawDivider(const X: Integer; const Y: Integer;
                           const Size: Integer);
//-----------------------------------------------------------------------------
//  Draws a Divider node at location X,Y with size Size.
//-----------------------------------------------------------------------------
var
  Poly: array[0..3] of TPoint;
  W: Integer;
begin
  W := Size + 1;
  Poly[0] := Point(X, Y-W);
  Poly[1] := Point(X+W, Y);
  Poly[2] := Point(X, Y+W);
  Poly[3] := Point(X-W, Y);
  Canvas.Polygon(Poly);
end;


procedure TMap.DrawStorage(const X: Integer; const Y: Integer;
                           const Size: Integer);
//-----------------------------------------------------------------------------
//  Draws a Storage node at location X,Y with size Size.
//-----------------------------------------------------------------------------
var
  Poly: array[0..3] of TPoint;
  W: Integer;
begin
  if (Options.ShowNodeSymbols) and (ZoomRatio >= Options.SymbolZoom) then
  begin
    W := 2*Size;
    Poly[0] := Point(X-W, Y-W);
    Poly[1] := Point(X-W, Y+Size+1);
    Poly[2] := Point(X+W+1, Y+Size+1);
    Poly[3] := Point(X+W+1, Y-W-1);
    Canvas.PolyLine(Poly);
    Canvas.Rectangle(X-W, Y-Size, X+W+2, Y+Size+2);
  end
  else
    Canvas.Rectangle(X-Size, Y-Size, X+Size+1, Y+Size+1);
end;


procedure TMap.DrawStoragePolygon(const Index: Integer);
//-----------------------------------------------------------------------------
//  Draws the polygon associated with Storage node Index.
//-----------------------------------------------------------------------------
var
  S: TNode;
  N: Integer;
  OldStyle: TBrushStyle;
  OldWidth: Integer;
  OldPenColor: TColor;
  OldBrushColor: TColor;
begin
  // Save current pen & brush attributes
  OldStyle := Canvas.Brush.Style;
  OldBrushColor := Canvas.Brush.Color;
  OldPenColor := Canvas.Pen.Color;
  OldWidth := Canvas.Pen.Width;

  // Retrieve the vertex points of the subcatchment's polygon
  S := Project.GetNode(STORAGE, Index);
  N := GetPolygonPoints(S.Vlist);
  if N < 3 then Exit;

  // Draw the polygon
  Canvas.Brush.Style := TBrushStyle(bsSolid);
  Canvas.Brush.Color := $00F2D9BF;  //Uutils.LightenColor(OldBrushColor);
  Canvas.Polygon(Slice(Points, N-1));

  // Draw the polygon's frame
  Canvas.Pen.Width := 1;
  Canvas.Pen.Color := ForeColor;
  Canvas.PolyLine(Slice(Points, N));

  // Restore current pen & brush
  Canvas.Brush.Style := OldStyle;
  Canvas.Brush.Color := OldBrushColor;
  Canvas.Pen.Color := OldPenColor;
  Canvas.Pen.Width := OldWidth;

end;


procedure TMap.DrawLink(const P1: TPoint; const P2: TPoint; L: TLink);
//-----------------------------------------------------------------------------
//  Draws a link between points P1 and P2.
//-----------------------------------------------------------------------------
var
  V: PVertex;
begin
  Canvas.MoveTo(P1.X, P1.Y);
  if L <> nil then
  begin
    V := L.Vlist.First;
    while V <> nil do
    begin
      Canvas.LineTo(GetXPix(V^.X), GetYpix(V^.Y));
      V := V^.Next;
    end;
  end;
  Canvas.LineTo(P2.X, P2.Y);
end;


//=============================================================================
//                       Symbol Drawing Methods
//=============================================================================

procedure TMap.DrawInletSymbol(L: TLink; P1: TPoint; P2: TPoint);
//-----------------------------------------------------------------------------
//  Draws inlet symbol and capture node connection for a conduit.
//-----------------------------------------------------------------------------
var
  LinkPt: TPoint;
  NodePt: TPoint;
  N, N2: TNode;
begin
  LinkPt := Point((P1.X + P2.X) div 2, (P1.Y + P2.Y) div 2);
  Canvas.Draw(LinkPt.X, LinkPt.Y - 4, InletBM);
  N := Uinlet.GetInletNode(L);
  N2 := Uinlet.GetCaptureNode(L);
  if (N <> nil) and GetNodePixPos(N, NodePt) and GetNodePixPos(N2, P2)
  then with Canvas do
  begin
    Brush.Style := bsClear;
    Pen.Style := psDot;
    Pen.Width := 1;
    Pen.Color := ForeColor;
    MoveTo(P2.X, P2.Y);
    LineTo(NodePt.X, NodePt.Y);
    Pen.Style := psSolid;
  end;
end;


procedure TMap.DrawArrow(L: TLink; P1: TPoint; P2: TPoint);
//-----------------------------------------------------------------------------
//  Draws flow direction arrow on link between points P1 and P2.
//-----------------------------------------------------------------------------
var
  K: Integer;
  Ptmp: TPoint;
begin
  // Check that zoom in scale is high enough to show arrows
  if (ZoomRatio < Options.ArrowZoom) then exit;

  // If results of a simulation run exist, reverse P1 and P2 for negative
  // flow direction
  if RunFlag then
  begin
    K := L.OutFileIndex;
    if K < 0 then Exit
    else
    case FlowDir[K] of
    NONE:
      Exit;
    MINUS:
      begin
        Ptmp := P1;
        P1 := P2;
        P2 := Ptmp;
      end;
    end;
  end;

  // Draw the arrow symbol on the link
  DrawLinkSymbol(P1, P2, lsArrow);
end;


procedure TMap.DrawLinkSymbol(const P1: TPoint; const P2: TPoint;
  const Symbol: TLinkSymbol);
//-----------------------------------------------------------------------------
//  Draws a symbol on the link between points P1 and P2.
//-----------------------------------------------------------------------------
var
  Asin,
  Acos   : Extended;
  Dx, Dy : Extended;
  X, Y   : Integer;
  Size   : Integer;
  Width  : Integer;
begin
  // Determine angle of inclination of symbol
  Dy := P2.Y - P1.Y;
  Dx := P2.X - P1.X;
  SinCos(arctan2(Dy, Dx), Asin, Acos);

  // Determine location & size of symbol
  X := (P1.X + P2.X) div 2;
  Y := (P1.Y + P2.Y) div 2;
  Width := Canvas.Pen.Width;
  Size := SYMBOLSIZE + Width;

  // Call symbol-specific drawing procedure
  case Symbol of

  lsArrow:
    begin
      with Options do
        DrawArrowHead(X, Y, ArrowStyle, ArrowSize, 0.5, Asin, Acos);
    end;

  lsValve:
    begin
      // Draw first half of valve
      DrawArrowHead(X, Y, asFilled, Size, 1.0, Asin, Acos);

      // Draw second half of valve
      Asin := -Asin;
      Acos := -Acos;
      DrawArrowHead(X, Y, asFilled, Size, 1.0, Asin, Acos);
    end;

  lsPump:
    begin
      if (Dx >= 0)
      then DrawPumpSymbol(X, Y, Size, 1, Asin, Acos)
      else DrawPumpSymbol(X, Y, Size, -1, Asin, Acos);
    end;

  end;
  Canvas.Pen.Width := Width;
end;


procedure TMap.DrawArrowHead(const X: Integer; const Y: Integer;
  const Style: TArrowStyle; const Size: Integer; const Aratio: Single;
  const Asin: Extended; const Acos: Extended);
//-----------------------------------------------------------------------------
//  Draws arrowhead symbol starting from position X,Y.
//  Style = arrowhead style,
//  Size  = arrowhead style,
//  Aratio = ratio of arrowhead width to length
//  Asin, Acos = sine & cosine of angle of inclination of arrowhead.
//-----------------------------------------------------------------------------
var
  X1, X2: Integer;
  Y1, Y2: Integer;
  Poly  : array[0..3] of TPoint;
begin
  X1 := X + Round((-Acos + Aratio*Asin)*Size);
  Y1 := Y - Round((Asin + Aratio*Acos)*Size);
  X2 := X + Round((-Acos - Aratio*Asin)*Size);
  Y2 := Y - Round((Asin - Aratio*Acos)*Size);
  case Style of

  asOpen:
    begin
      Poly[0] := Point(X1, Y1);
      Poly[1] := Point(X, Y);
      Poly[2] := Point(X2, Y2);
      Canvas.PolyLine(Slice(Poly, 3));
    end;

  asFilled:
    begin
      Poly[0] := Point(X, Y);
      Poly[1] := Point(X1, Y1);
      Poly[2] := Point(X2, Y2);
      Canvas.Pen.Width := 1;
      Canvas.Polygon(Slice(Poly, 3));
    end;

  asFancy:
    begin
      Poly[0] := Point(X, Y);
      Poly[1] := Point(X1, Y1);
      X1 := X + Round((0.67*Acos)*Size);
      Y1 := Y - Round((-0.67*Asin)*Size);
      Poly[2] := Point(X1, Y1);
      Poly[3] := Point(X2, Y2);
      Canvas.Pen.Width := 1;
      Canvas.Polygon(Slice(Poly, 4));
    end;

  end;
end;


procedure TMap.DrawPumpSymbol(const X: Integer; const Y: Integer;
  const Size: Integer; const Direction: Integer; const Asin: Extended;
  const Acos: Extended);
//-----------------------------------------------------------------------------
//  Draws pump symbol centered at position X,Y.
//  Size  = arrowhead style,
//  Direction = orientation of pump
//  Asin, Acos = sine & cosine of angle of inclination of pump.
//-----------------------------------------------------------------------------
var
  Xi, Yi, R : Integer;
  Acolor    : TColor;
  Poly      : array[0..3] of TPoint;
begin
  Acolor := Canvas.Pen.Color;
  if Options.ShowNodeBorder then Canvas.Pen.Color := ForeColor;
  R := 2*Size;
  Poly[0] := Point(X, Y);
  Xi := X + Round(Acos*R);
  Yi := Y + Round(Asin*R);
  Poly[1] := Point(Xi, Yi);
  Xi := X + Round((Acos + 0.5*Asin*Direction)*R);
  Yi := Y + Round((Asin - 0.5*Acos*Direction)*R);
  Poly[2] := Point(Xi, Yi);
  Xi := X + Round(+0.5*Asin*R*Direction);
  Yi := Y + Round(-0.5*Acos*R*Direction);
  Poly[3] := Point(Xi, Yi);
  Canvas.Polygon(Slice(Poly, 4));
  Canvas.Ellipse(X-Size, Y-Size, X+Size, Y+Size);
  Canvas.Pen.Color := Acolor;
end;


//=============================================================================
//                        Label Drawing Methods
//=============================================================================

procedure TMap.DrawGageIDLabel(const Index: Integer; const P: TPoint);
//-----------------------------------------------------------------------------
//  Draws text of a rain gage ID label underneath the gage's symbol.
//-----------------------------------------------------------------------------
var
  Acolor : TColor;
  Xoffset: Integer;
  Yoffset: Integer;
  S: String;
begin
  S := Project.GetID(RAINGAGE, Index);
  with Canvas do
  begin
    Acolor := Brush.Color;
    Brush.Color := BackColor;
    Font.Color := GrayColor;
    if Options.NotationTranspar then SetBkMode(Handle, TRANSPARENT);
    Yoffset := CharHeight + 1;
    Xoffset := (GageBM.Width - TextWidth(S)) div 2;
    TextOut(P.X+Xoffset, P.Y-Yoffset, S);
    Brush.Color := Acolor;
  end;
end;


procedure TMap.DrawNodeIDLabel(const ObjType: Integer; const Index: Integer;
  const P: TPoint; const Size: Integer);
//-----------------------------------------------------------------------------
// Draws text of node ID label next to node.
//-----------------------------------------------------------------------------
var
  Acolor : TColor;
  Offset: Integer;
begin
  // Increase offset distance from node for tanks & reservoirs
  Offset := Size + CharHeight + 1;
  if (ObjType = STORAGE)
  and (Options.ShowNodeSymbols)
  and (ZoomRatio >= Options.SymbolZoom)
  then Offset := Offset + Size + 1;
  with Canvas do
  begin
    Acolor := Brush.Color;
    Brush.Color := BackColor;
    Font.Color := GrayColor;
    if Options.NotationTranspar then SetBkMode(Handle, TRANSPARENT);
    TextOut(P.X, P.Y-Offset, Project.GetID(ObjType, Index));
    Brush.Color := Acolor;
  end;
end;


procedure TMap.DrawNodeValueLabel(const ObjType: Integer; const Index: Integer;
  const P: TPoint; const Size: Integer; const S: String);
//-----------------------------------------------------------------------------
//  Draws text of value of current node view variable next to node.
//-----------------------------------------------------------------------------
var
  Acolor : TColor;
  Offset : Integer;
begin
  if (Length(S) = 0) then Exit;
  Offset := Size + 2;
  if (ObjType = STORAGE)
  and (Options.ShowNodeSymbols)
  and (ZoomRatio >= Options.SymbolZoom)
  then Offset := Offset + Size;
  with Canvas do
  begin
    Acolor := Brush.Color;
    Brush.Color := BackColor;
    Font.Color := GrayColor;  //Pen.Color;
    Font.Style := Font.Style + [fsItalic];
    if Options.NotationTranspar then SetBkMode(Handle,TRANSPARENT);
    TextOut(P.X, P.Y+Offset, S);
    Brush.Color := Acolor;
    Font.Style := [];
  end;
end;


procedure TMap.DrawLinkIDLabel(const ObjType: Integer; const Index: Integer;
  const P1: TPoint; const P2: TPoint; const Size: Integer);
//-----------------------------------------------------------------------------
//  Draws text of link ID label at midpoint of link.
//-----------------------------------------------------------------------------
var
  Acolor : TColor;
  S      : String;
  X, Y   : Integer;
begin
  S := Project.GetID(ObjType, Index);
  X := (P1.X + P2.X) div 2;
  Y := (P1.Y + P2.Y) div 2;
  if (Abs(P2.Y - P1.Y) < Size) then
  begin
    X := X - (Canvas.TextWidth(S) div 2);
    Y := Y - Size - CharHeight;
  end
  else
  begin
    X := X - Canvas.TextWidth(S) - Size;
    if ((P1.Y < P2.Y) and (P1.X < P2.X))
    then Y := Y + Size
    else Y := Y - Size - CharHeight;
  end;
  with Canvas do
  begin
    Acolor := Brush.Color;
    Brush.Color := BackColor;
    Font.Color := GrayColor;
    if Options.NotationTranspar then SetBkMode(Handle, TRANSPARENT);
    TextOut(X, Y, S);
    Brush.Color := Acolor;
  end;
end;


procedure TMap.DrawLinkValueLabel(const ObjType: Integer; const Index: Integer;
  const P1: TPoint; const P2: TPoint; const Size: Integer; const S: String);
//-----------------------------------------------------------------------------
//  Draws text of value of current link view variable at link midpoint.
//-----------------------------------------------------------------------------
var
  Acolor : TColor;
  X, Y   : Integer;
begin
  if (Length(S) = 0) then Exit;
  X := (P1.X + P2.X) div 2;
  Y := (P1.Y + P2.Y) div 2;
  if (Abs(P2.Y - P1.Y) < Size) then
  begin
    X := X - (Canvas.TextWidth(S) div 2);
    Y := Y + Size;
  end
  else
  begin
    X := X + Size;
    if ((P1.Y < P2.Y) and (P1.X < P2.X))
    then Y := Y - Size - CharHeight
    else Y := Y + Size;
  end;
  with Canvas do
  begin
    Acolor := Brush.Color;
    Brush.Color := BackColor;
    Font.Color := GrayColor;  //Pen.Color;
    Font.Style := Font.Style + [fsItalic];
    if Options.NotationTranspar then SetBkMode(Handle, TRANSPARENT);
    TextOut(X, Y, S);
    Brush.Color := Acolor;
    Font.Style := [];
  end;
end;


//=============================================================================
//                    Object Sizing & Coloring Methods
//=============================================================================

procedure TMap.SetSubcatchColor(const Index: Integer);
//-----------------------------------------------------------------------------
//  Sets the color to be used for drawing a subcatchment.
//-----------------------------------------------------------------------------
var
  K      : Integer;
  Color  : TColor;
begin
  // Retrieve the color stored for the subcatchment in the project database
  K := -1;
  if (CurrentSubcatchVar = NOVIEW)
  then Color := MapGrayColor[Options.ColorIndex]
  else begin
    K := Project.GetSubcatch(SUBCATCH, Index).ColorIndex;
    if K < 0 then Color := MapGrayColor[Options.ColorIndex]
    else if QueryFlag then Color := QueryColor
    else Color := MapSubcatchColor[K];
  end;

  // If this color is different than the last one used to draw with
  // then update the color used to draw with.
  if K <> LastColorIndex then
  begin
    Canvas.Pen.Color := Color;
    Canvas.Brush.Color := Color;
    LastColorIndex := K;
  end;
end;


procedure TMap.SetLinkColor(const ObjType: Integer; const Index: Integer);
//-----------------------------------------------------------------------------
//  Sets the color to be used for drawing a link.
//-----------------------------------------------------------------------------
var
  K      : Integer;
  Color : TColor;
begin
  // Retrieve the color stored for the link in the project database
  K := -1;
  if (CurrentLinkVar = NOVIEW)
  then Color := MapGrayColor[Options.ColorIndex]
  else begin
    K := Project.GetLink(ObjType, Index).ColorIndex;
    if K < 0 then Color := MapGrayColor[Options.ColorIndex]
    else if QueryFlag then Color := QueryColor
    else Color := MapLinkColor[K];
  end;

  // Update the color used to draw with.
  Canvas.Pen.Color := Color;
  Canvas.Brush.Color := Color;
  LastColorIndex := K;
end;


procedure TMap.SetNodeColor(const ObjType: Integer; const Index: Integer);
//-----------------------------------------------------------------------------
//  Sets the color to be used for drawing a node.
//-----------------------------------------------------------------------------
var
  K      : Integer;
  Color : TColor;
begin
  // Retrieve the color stored for the node in the project database
  K := -1;
  if (CurrentNodeVar = NOVIEW)
  then Color := MapGrayColor[Options.ColorIndex]
  else begin
    K := Project.GetNode(ObjType, Index).ColorIndex;
    if K < 0 then Color := MapGrayColor[Options.ColorIndex]
    else if QueryFlag then Color := QueryColor
    else Color := MapNodeColor[K];
  end;

  // If this color is different than the last one used to draw with
  // then update the color used to draw with.
  if K <> LastColorIndex then
  begin
    if Options.ShowNodeBorder then Canvas.Pen.Color := ForeColor
    else Canvas.Pen.Color := Color;
    Canvas.Brush.Color := Color;
    LastColorIndex := K;
  end;
end;


function TMap.SetLinkSize: Integer;
//-----------------------------------------------------------------------------
//  Sets the size used to draw a link with.
//-----------------------------------------------------------------------------
begin
  with Options do
    if ShowLinksBySize
    then Result := LinkSize + 2*LastColorIndex
    else Result := LinkSize;
end;


function TMap.SetNodeSize: Integer;
//-----------------------------------------------------------------------------
//  Sets the size used to draw a node with.
//-----------------------------------------------------------------------------
begin
  with Options do
    if ShowNodesBySize
    then Result := NodeSize + LastColorIndex
    else Result := NodeSize;
end;


//=============================================================================
//                    Object Bounding & Location Methods
//=============================================================================

procedure TMap.UpdateBounds(var R: TRect; const P: TPoint);
//-----------------------------------------------------------------------------
//  Updates the size of rectangle R to include point P.
//-----------------------------------------------------------------------------
begin
  if P.X < R.Left   then R.Left := P.X;
  if P.X > R.Right  then R.Right := P.X;
  if P.Y < R.Top    then R.Top := P.Y;
  if P.Y > R.Bottom then R.Bottom := P.Y;
end;


function TMap.GetBoundingRect(const ObjType: Integer;
  const Index: Integer): TRect;
//-----------------------------------------------------------------------------
//  Returns the bounding rectangle for item Index of type ObjType.
//  Also saves the pixel locations of the top-left & bottom-right
//  points in global variables P1 and P2.
//-----------------------------------------------------------------------------
begin
  if ObjType = RAINGAGE
  then Result := GetGageRect(Project.GetGage(Index))
  else if ObjType = SUBCATCH
  then Result := GetSubcatchRect(Project.GetSubcatch(ObjType, Index))
  else if Project.IsNode(ObjType)
  then Result := GetNodeRect(Project.GetNode(ObjType, Index))
  else if Project.IsLink(ObjType)
  then Result := GetLinkRect(Project.GetLink(ObjType, Index))
  else if ObjType = MAPLABEL
  then Result := GetLabelRect(Project.GetMapLabel(Index))
  else Result := NORECT;
end;


function TMap.GetGageRect(G: TRaingage): TRect;
//-----------------------------------------------------------------------------
//  Gets the bounding rectangle for a rain gage object.
//-----------------------------------------------------------------------------
var
  X, Y : Extended;
begin
  X := G.X;
  Y := G.Y;
  if (X = MISSING) or (Y = MISSING) or (GageBM = nil)
  then Result := NORECT
  else begin
    P1.X := GetXpix(X) - (GageBM.Width div 2) - 1;
    P1.Y := GetYpix(Y) - (GageBM.Height div 2) - 1;
    P2.X := P1.X + GageBM.Width + 1;
    P2.Y := P1.Y + GageBM.Height + 1;
    Result := Rect(P1.X, P1.Y, P2.X, P2.Y);
  end;
end;


function TMap.GetPolygonRect(X, Y: Extended; Pbuf: Integer;
  Vlist: TVertexList): TRect;
//-----------------------------------------------------------------------------
//  Gets the bounding rectangle for a polygon.
//-----------------------------------------------------------------------------
var
  P      : TPoint;
  R      : TRect;
  V      : PVertex;
begin
  P := Point(GetXpix(X), GetYpix(Y));
  P1 := P;
  R := Rect(P.X, P.Y, P.X+1, P.Y+1);
  V := Vlist.First;
  while V <> nil do
  begin
    P := Point(GetXPix(V^.X), GetYpix(V^.Y));
    UpdateBounds(R, P);
    V := V^.Next;
  end;
  OffsetRect(R, -Pbuf, -Pbuf);
  InflateRect(R, 2*Pbuf, 2*Pbuf);
  Result := R;
end;

function TMap.GetSubcatchRect(S: TSubcatch): TRect;
//-----------------------------------------------------------------------------
//  Gets the bounding rectangle for a subcatchment object.
//-----------------------------------------------------------------------------
var
  Pbuf   : Integer;
  X, Y   : Extended;
begin
  Pbuf := Options.NodeSize + CharHeight;
  X := S.X;
  Y := S.Y;
  if (X = MISSING) or (Y = MISSING)
  then Result := NORECT
  else Result := GetPolygonRect(X, Y, Pbuf, S.Vlist);
end;


function TMap.GetNodeRect(N: TNode): TRect;
//-----------------------------------------------------------------------------
//  Gets the bounding rectangle for a node. The rectangle is centered
//  at node's pixel position and extends a distance on all sides
//  equal to the node size option plus the height of notation
//  labels (CharHeight).
//-----------------------------------------------------------------------------
var
  Pbuf : Integer;
begin
   Pbuf := Options.NodeSize + CharHeight;
   if (N.Ntype = STORAGE) and (N.Vlist.Count > 1) then
     Result := GetPolygonRect(N.X, N.Y, Pbuf, N.Vlist)
   else
   begin
     P1 := GetNodePoint(N);
     Result := Rect(P1.X-Pbuf, P1.Y-Pbuf, P1.X+Pbuf+1, P1.Y+Pbuf+1);
   end;
end;


function TMap.GetLinkRect(L: TLink): TRect;
//-----------------------------------------------------------------------------
//  Gets the bounding rectangle for a link. The rectangle equals the
//  pixel extent of all nodes & vertices that comprise the link and is
//  then enlarged by the size of link symbols (or flow arrows)
//  plus the link's width.
//-----------------------------------------------------------------------------
var
  Pbuf : Integer;
  P    : TPoint;
  R    : TRect;
  V    : PVertex;
begin
  Pbuf := MaxIntValue([SYMBOLSIZE,Options.ArrowSize]) + Options.LinkSize;
  Result := NORECT;
  if not GetNodePixPos(L.Node1, P1) then Exit;
  if not GetNodePixPos(L.Node2, P2) then Exit;
  R := Rect(P1.x, P1.y, P1.x+1, P1.y+1);
  V := L.Vlist.First;
  while V <> nil do
  begin
    P := Point(GetXPix(V^.X), GetYpix(V^.Y));
    UpdateBounds(R, P);
    V := V^.Next;
  end;
  UpdateBounds(R,P2);
  OffsetRect(R, -Pbuf, -Pbuf);
  InflateRect(R, 2*Pbuf, 2*Pbuf);
  Result := R;
end;


function TMap.GetLabelRect(L: TMapLabel): TRect;
//-----------------------------------------------------------------------------
//  Gets the bounding rectangle for a label.
//-----------------------------------------------------------------------------
var
  H, W      : Integer;
  Xa, Ya    : Single;
  F         : TFont;
  S         : TSize;
begin
    with L do
    begin

      // Find world coordinates of label's anchor node
      // (if no anchor node then these are same coordinates as label)
      if Anchor = nil then
      begin
        Xa := X;
        Ya := Y;
      end
      else
      begin
        Xa := Anchor.X;
        Ya := Anchor.Y;
      end;

      // Find pixel coordinates of upper left of label's bounding rectangle
      with Window do
      begin
        if ZoomIndex = 0 then
        begin
          P1.X := GetXpix(X);
          P1.Y := GetYpix(Y);
        end
        else
        begin
          P1.X := GetXpix(Xa) + Round((X-Xa)/Sfactor);
          P1.Y := GetYpix(Ya) + Round((Ya-Y)/Sfactor);
        end;
      end;
    end;

    // Create a Font object to determine label's width & height
    F := TFont.Create;
    try
      F.Assign(Canvas.Font);
      L.GetFont(Canvas.Font);
      S := Canvas.TextExtent(L.Text);
      W := S.cx;
      H := S.cy;
      Canvas.Font.Assign(F);
    finally
      F.Free;
    end;

    // Construct the label's bounding rectangle
    Result := Rect(P1.X, P1.Y, P1.X + W, P1.Y + H);
end;


procedure TMap.GetLinkMidSegment(L: TLink; var Pa, Pb: TPoint);
//-----------------------------------------------------------------------------
//  Gets the pixel coordinates of the vertices that define the
//  midpoint segment of the link (aLink).
//-----------------------------------------------------------------------------
var
  M, N   : Integer;
  Va, Vb : PVertex;
begin
  N := L.Vlist.Count;
  if N > 0 then
  begin
    N := (N div 2) + 1;
    Va := nil;
    Vb := L.Vlist.First;
    for M := 2 to N do
    begin
      Va := Vb;
      Vb := Vb^.Next;
    end;
    if Va <> nil then
      Pa := Point(GetXPix(Va^.X), GetYpix(Va^.Y));
    if Vb <> nil then
      Pb := Point(GetXPix(Vb^.X), GetYpix(Vb^.Y));
  end;
end;


function TMap.GetLinkMidpoint(L: TLink; var P: TPoint): Boolean;
//-----------------------------------------------------------------------------
//  Gets the pixel coordinates (P) of the midpoint of a link (aLink).
//-----------------------------------------------------------------------------
var
  Pa, Pb: TPoint;
begin
  Result := False;
  if not GetNodePixPos(L.Node1, Pa)
  or not GetNodePixPos(L.Node2, Pb) then Exit;
  GetLinkMidSegment(L, Pa, Pb);
  P.X := (Pa.X + Pb.X) div 2;
  P.Y := (Pa.Y + Pb.Y) div 2;
  Result := True;
end;


function TMap.GetAdjacencyRect(const ObjType: Integer; const Index: Integer;
  const DeleteLinks: Boolean): TRect;
//-----------------------------------------------------------------------------
//  Finds the rectangle encompassing all nodes adjacent to a given node.
//  If DeleteLinks = True, then the connecting links are deleted (as
//  when the node is being deleted from the project).
//-----------------------------------------------------------------------------
var
  I, J   : Integer;
  R      : TRect;
  Links  : TStringList;
  N      : TNode;
  L      : TLink;
begin
  Result := GetBoundingRect(ObjType, Index);
  N := Project.GetNode(ObjType, Index);
  for I := CONDUIT to OUTLET do
  begin
    Links := Project.Lists[I];
    for J := Links.Count-1 downto 0 do
    begin
      L := Project.GetLink(I, J);

      if Options.ShowLinkSymbols and (Uinlet.GetInletNode(L) = N)
      then Result := Window.MapRect;

      if (L.Node1 = N) or (L.Node2 = N) then
      begin
        if Options.ShowLinkSymbols and (L.Inlet <> nil)
        then Result := Window.MapRect else
        begin
          R := GetBoundingRect(I, J);
          if IntersectRect(R, R, Window.MapRect)
          then UnionRect(Result, Result, R);
        end;
        if DeleteLinks then Project.DeleteItem(I, J);
      end;

    end;
  end;
end;


function TMap.GetXpix(const X: Extended):Integer;
//-----------------------------------------------------------------------------
// Converts world coordinate X to a screen pixel value.
//-----------------------------------------------------------------------------
var
  P: Extended;
begin
  with Window do
  try
    P := (X-Woffset.X) / WperP;
    Result := Round(P) + Poffset.X;
  except
    On EInvalidOp do Result := MAX_INT_COORD;
  end;
end;


function TMap.GetYpix(const Y: Extended):Integer;
//-----------------------------------------------------------------------------
// Converts world coordinate Y to a screen pixel value.
//-----------------------------------------------------------------------------
var
  P: Extended;
begin
  with Window do
  try
    P := (Y-Woffset.Y) / WperP;
    Result := Poffset.Y - Round(P);
  except
    On EInvalidOp do Result := MAX_INT_COORD;
  end;
end;


function  TMap.GetX(const X: Integer): Extended;
//-----------------------------------------------------------------------------
// Converts a screen pixel location to an X world coordinate value.
//-----------------------------------------------------------------------------
begin
  with Window do
    Result := (X - Poffset.X) * WperP + Woffset.X;
end;


function  TMap.GetY(const Y: Integer): Extended;
//-----------------------------------------------------------------------------
// Converts a screen pixel location to a Y world coordinate value.
//-----------------------------------------------------------------------------
begin
  with Window do
    Result := (Poffset.Y - Y) * WperP + Woffset.Y;
end;


function TMap.GetSubcatchCentroid(S: TSubcatch): TPoint;
//-----------------------------------------------------------------------------
//  Returns the X,Y pixel location of subcatchment's centroid.
//-----------------------------------------------------------------------------
var
  X,Y : Extended;
begin
  X := S.X;
  Y := S.Y;
  if (X = MISSING) or (Y = MISSING)
  then Result := NOPOINT
  else Result := Point(GetXpix(X),GetYpix(Y));
end;


function TMap.GetNodePoint(N: TNode): TPoint;
//-----------------------------------------------------------------------------
//  Returns the X,Y pixel location of a node object.
//-----------------------------------------------------------------------------
var
  X,Y : Extended;
begin
  X := N.X;
  Y := N.Y;
  if (X = MISSING) or (Y = MISSING)
  then Result := NOPOINT
  else Result := Point(GetXpix(X),GetYpix(Y));
end;


function TMap.GetNodePixPos(N: TNode; var P: TPoint): Boolean;
//-----------------------------------------------------------------------------
//  Finds the pixel coordinates (P) of a node (N).
//  Returns False if the node's coordinates are missing.
//-----------------------------------------------------------------------------
begin
  P := NOPOINT;
  Result := False;
  if N = nil then Exit;
  if (N.X = MISSING) or (N.Y = MISSING) then Exit;
  P := Point(GetXpix(N.X), GetYpix(N.Y));
  Result := True;
end;

{
function TMap.IsBounded(const A, B, C: Extended): Boolean;
//-----------------------------------------------------------------------------
//  Determines if C is between A and B
//-----------------------------------------------------------------------------
begin
  if (C < A) or (C > B) then Result := False
  else Result := True;
end;
}

//=============================================================================
//                Map Resizing & Re-scaling Methods
//=============================================================================

procedure TMap.Resize(const Rect: TRect);
//-----------------------------------------------------------------------------
//  Resizes the map's display window & its bitmaps.
//-----------------------------------------------------------------------------
begin
  ResizeWindow(Rect);
  ResizeBitmap(Bitmap, Window.Pwidth, Window.Pheight);
  ResizeBitmap(BackBM, Window.Pwidth, Window.Pheight);
end;


procedure TMap.ResizeWindow(const Rect: TRect);
//-----------------------------------------------------------------------------
//  Resizes the map's display window.
//-----------------------------------------------------------------------------
begin
  with Window do
  begin
    MapRect := Rect;
    Pwidth := MapRect.Right - MapRect.Left;
    Pheight := MapRect.Bottom - MapRect.Top;
    Poffset.X := MapRect.Left;
    Poffset.Y := MapRect.Bottom;
  end;
end;


procedure TMap.ResizeBitmap(var Bmap: TBitmap; const W,H: Integer);
//-----------------------------------------------------------------------------
//  Resizes the map's bitmap.
//-----------------------------------------------------------------------------
begin
  if Bmap <> nil then
  begin
    Bmap.Width := W;
    Bmap.Height := H;
  end;
end;


procedure TMap.Rescale;
//-----------------------------------------------------------------------------
//  Resets the map's dimensions and scale factors.
//-----------------------------------------------------------------------------
var
  Dx, Dy   : Extended;
  SFx, SFy : Extended;
begin
  // Compute world distance units per pixel in the X & Y directions
  Dx := Dimensions.UpperRight.X - Dimensions.LowerLeft.X;
  Dy := Dimensions.UpperRight.Y - Dimensions.LowerLeft.Y;
  SFx := Dx/Window.Pwidth;
  SFy := Dy/Window.Pheight;

  // Adjust scaling to maintain a 1:1 aspect ratio
  if SFy > SFx then Sfactor := SFy
  else Sfactor := SFx;

  // Compute the location of the map center at full scale
  ZoomState[0].Xcenter := Dimensions.LowerLeft.X + Dx/2.0;
  ZoomState[0].Ycenter := Dimensions.LowerLeft.Y + Dy/2.0;

  // Compute a scaling for the current zoom state
  with Window do
  begin
    WperP := Sfactor * ZoomState[ZoomIndex].ZoomFactor;
    Woffset.X := ZoomState[ZoomIndex].Xcenter - WperP * (Pwidth/2.0);
    Woffset.Y := ZoomState[ZoomIndex].Ycenter - WperP * (Pheight/2.0);
  end;
end;

function TMap.GetSubcatchAreaStr(const Index: Integer): String;
//-----------------------------------------------------------------------------
//  Returns string value of a subcatchment's area.
//-----------------------------------------------------------------------------
var
  S    : TSubcatch;
  Area : Extended;
begin
  S := Project.GetSubcatch(SUBCATCH, Index);
  Area := S.GetArea;
  if Area = 0.0 then Result := S.Data[SUBCATCH_AREA_INDEX]
  else with Dimensions do
  begin
    if Units = muDegrees then Area := Area*XperDeg*YperDeg;
    Area := Area*AreaUCF;
    Result := Format('%.2f',[Area]);
  end;
end;


procedure TMap.ConvertMapUnits(var Dx, Dy: Extended);
begin
  with Dimensions do
    if Units = muDegrees then
    begin
      Dx := Dx*XperDeg;
      Dy := Dy*YperDeg;
    end;
end;


function TMap.GetLinkLengthStr(const LinkType: Integer;
  const Index: Integer): String;
//-----------------------------------------------------------------------------
// Returns a string with the length of a particular link.
//-----------------------------------------------------------------------------
var
  L      : TLink;
  V      : PVertex;
  Length : Extended;
  X1, X2 : Extended;
  Y1, Y2 : Extended;
  Dx, Dy : Extended;
begin
  L := Project.GetLink(LinkType, Index);
  Length := 0;
  X1 := L.Node1.X;
  Y1 := L.Node1.Y;
  V  := L.Vlist.First;
  while V <> nil do
  begin
    X2 := V^.X;
    Y2 := V^.Y;
    Dx := X2 - X1;
    Dy := Y2 - Y1;
    ConvertMapUnits(Dx, Dy);
    Length := Length + Sqrt(Sqr(Dx) + Sqr(Dy));
    X1 := X2;
    Y1 := Y2;
    V  := V^.Next;
  end;
  X2 := L.Node2.X;
  Y2 := L.Node2.Y;
  Dx := X2 - X1;
  Dy := Y2 - Y1;
  ConvertMapUnits(Dx, Dy);
  Length := Length + Sqrt(Sqr(Dx) + Sqr(Dy));
  Length := Dimensions.LengthUCF*Length;
  Result := Format('%.2f',[Length]);
end;


function TMap.GetDistance(Points: array of TPoint; N: Integer): Extended;
//-----------------------------------------------------------------------------
// Finds the distance in project units of a polyline given in pixel units.
//-----------------------------------------------------------------------------
var
  I: Integer;
  X, Y, X1, Y1: Extended;
  D, Dx, Dy: Extended;
begin
  D := 0.0;
  if N >= 2 then
  begin
    X1 := GetX(Points[0].X);
    Y1 := GetY(Points[0].Y);
    for I := 1 to N-1 do
    begin
      X := GetX(Points[I].X);
      Y := GetY(Points[I].Y);
      Dx := X - X1;
      Dy := Y - Y1;
      ConvertMapUnits(Dx, Dy);
      D := D + Sqrt(Sqr(Dx) + Sqr(Dy));
      X1 := X;
      Y1 := Y;
    end;
  end;
  Result := D * Dimensions.LengthUCF;
end;


function TMap.GetArea(Points: array of TPoint; N: Integer): Extended;
//-----------------------------------------------------------------------------
//  Computes the area in project units of a polygon region whose
//  coordinates are in pixels.
//-----------------------------------------------------------------------------
var
  I:  Integer;
  X:  Extended;
  Y:  Extended;
  A:  Extended;
  X1, Y1: Extended;
  Xmin, Ymin: Extended;
begin
  // There must be at least 3 points to compute an area
  A := 0.0;
  if N >= 3 then
  begin

    // Find min. X & Y of all points
    X := GetX(Points[0].X);
    Y := GetY(Points[0].Y);
    Xmin := X;
    Ymin := Y;
    for I := 1 to N-1 do
    begin
      X := GetX(Points[I].X);
      Y := GetY(Points[I].Y);
      if X < Xmin then Xmin := X;
      if Y < Ymin then Ymin := Y;
    end;

    // Compute terms of the area formula using scaled X & Y values
    X := GetX(Points[0].X) - Xmin;
    Y := GetY(Points[0].Y) - Ymin;
    for I := 1 to N-1 do
    begin
      X1 := GetX(Points[I].X) - Xmin;
      Y1 := GetY(Points[I].Y) - Ymin;
      A := A + X*Y1 - Y*X1;
      X := X1;
      Y := Y1;
    end;
    A := Abs(A)/2.0;

    // Convert from map units to project units
    with Dimensions do
    begin
      if Units = muDegrees then A := A*XperDeg*YperDeg;
      A := A * AreaUCF;
    end;
  end;
  Result := A;
end;


//-----------------------------------------------------------------------------
//  These next two methods are used to snap together the vertexes of two
//  adjoining subcatchment polygons.
//-----------------------------------------------------------------------------

function TMap.SnapVertex(V1: PVertex; V2: PVertex; const X1: Integer;
  const Y1: Integer): Boolean;
begin
  Result := False;
  while V2 <> nil do
  begin
    if Abs(X1 - GetXpix(V2^.X)) <= Options.SubcatchSnapTol then
      if Abs(Y1 - GetYpix(V2^.Y)) <= Options.SubcatchSnapTol then
      begin
        V1^.X := V2^.X;
        V1^.Y := V2^.Y;
        Result := True;
        break;
      end;
    V2 := V2.Next;
  end;
end;


procedure TMap.SnapSubcatch(const Index: Integer);
var
  I: Integer;
  V1: PVertex;
  V2: PVertex;
  X1: Integer;
  Y1: Integer;
begin
  V1 := Project.GetSubcatch(SUBCATCH, Index).Vlist.Vfirst;
  while V1 <> nil do
  begin
    X1 := GetXpix(V1^.X);
    Y1 := GetYpix(V1^.Y);
    for I := 0 to Project.Lists[SUBCATCH].Count - 1 do
    begin
      if I <> Index then
      begin
        V2 := Project.GetSubcatch(SUBCATCH, I).Vlist.Vfirst;
        if SnapVertex(V1, V2, X1, Y1) then break;
      end;
    end;
    V1 := V1.Next;
  end;
end;

end.
