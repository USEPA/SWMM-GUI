unit Udxf;

{-------------------------------------------------------------------}
{                    Unit:    Udxf.pas                              }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    06/01/22  (5.2.1)                     }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Delphi Pascal unit that exports the SWMM Study Area Map to      }
{   a file in DXF format by calling the DXFexport procedure.        }
{-------------------------------------------------------------------}

interface

uses SysUtils, Windows, Graphics, Math, Uglobals, Uproject, Uvertex;

procedure DXFexport(const fname: String; const Jstyle: Integer);

implementation

uses Fmap, Umap;

const
  DXFcolors  : array[1..7] of TColor =
               (clRed, clYellow, clLime, clAqua,
                clBlue, clFuchsia, clBlack);
  StartTable : array[1..31] of String =
               ('0','SECTION','2','TABLES','0','TABLE',
                '2','LTYPE','70','1','0','LTYPE','2',
                'CONTINUOUS','70','64','3','SOLID LINE',
                '72','65','73','0','40','0.0','0','ENDTAB',
                '0','TABLE','2','LAYER','70');
  EndTable   : array[1..16] of String =
               ('0','ENDTAB','0','TABLE','2','VIEW','70','0',
                '0','ENDTAB','0','ENDSEC','0','SECTION','2','ENTITIES');

var
  DXFfile   : TextFile;
  JuncStyle : Integer;
  SubcatchSize: Single;
  LinkSize  : Single;
  NodeSize  : Single;
  LabelSize : Single;
  ArrowSize : Single;
  Map       : TMap;
  MapOptions: TMapOptions;
  PixPerMapExt: Single;
  DXFLinkColor: array[0..MAXINTERVALS] of Integer;
  DXFNodeColor: array[0..MAXINTERVALS] of Integer;
  DXFSubcatchColor: array[0..MAXINTERVALS] of Integer;


function GetDXFColor(aColor : TColor): Integer;
var
  diff    : LongInt;
  maxdiff : LongInt;
  i, imax : Integer;
begin
  imax := 1;
  maxdiff := High(LongInt);
  for i := Low(DXFColors) to High(DXFColors) do
  begin
    diff := Abs(aColor - DXFColors[i]);
    if diff < maxdiff then
    begin
      maxdiff := diff;
      imax := i;
    end
  end;
  Result := imax;
end;


procedure AddHeader(const xmin,ymin,xmax,ymax: Single);
begin
  Writeln(DXFfile,'0');
  Writeln(DXFfile,'SECTION');
  Writeln(DXFfile,'2');
  Writeln(DXFfile,'HEADER');
  Writeln(DXFfile,'9');
  Writeln(DXFfile,'$EXTMIN');
  Writeln(DXFfile,'10');
  Writeln(DXFfile,xmin);
  Writeln(DXFfile,'20');
  Writeln(DXFfile,ymin);
  Writeln(DXFfile,'9');
  Writeln(DXFfile,'$EXTMAX');
  Writeln(DXFfile,'10');
  Writeln(DXFfile,xmax);
  Writeln(DXFfile,'20');
  Writeln(DXFfile,ymax);
  Writeln(DXFfile,'0');
  Writeln(DXFfile,'ENDSEC');
end;


procedure StartTables(Nlayers: Integer);
var
  i : Integer;
begin
  for i := 1 to High(StartTable) do
    Writeln(DXFfile, StartTable[i]);
  Writeln(DXFfile, Nlayers);
end;


procedure AddLayer(layer: String; color: Integer);
begin
  Writeln(DXFfile, '0');
  Writeln(DXFfile, 'LAYER');
  Writeln(DXFfile, '2');
  Writeln(DXFfile, layer);
  Writeln(DXFfile, '70');
  Writeln(DXFfile, '0');
  Writeln(DXFfile, '62');
  Writeln(DXFfile, color);
  Writeln(DXFfile, '6');
  Writeln(DXFfile, 'CONTINUOUS');
end;


procedure EndTables;
var
  i : Integer;
begin
  for i := 1 to High(EndTable) do
    Writeln(DXFfile, EndTable[i]);
end;


procedure EndDXF;
begin
  Writeln(DXFfile, '0');
  Writeln(DXFfile, 'ENDSEC');
  Writeln(DXFfile, '0');
  Writeln(DXFfile, 'EOF');
end;


procedure AddCircle(const x,y   : Single;
                    const radius: Single;
                    const color : Integer;
                    const layer : String);
begin
  Writeln(DXFfile, '0');
  Writeln(DXFfile, 'CIRCLE');
  Writeln(DXFfile, '8');
  Writeln(DXFfile, layer);
  Writeln(DXFfile, '10');
  Writeln(DXFfile, x);
  Writeln(DXFfile, '20');
  Writeln(DXFfile, y);
  Writeln(DXFfile, '40');
  Writeln(DXFfile, radius);
  if color > 0 then
  begin
    Writeln(DXFfile, '62');
    Writeln(DXFfile, color);
  end;
end;


procedure AddVertex(const x, y, bulge: Single; const layer: String);
begin
  Writeln(DXFfile, '0');
  Writeln(DXFfile, 'VERTEX');
  Writeln(DXFfile, '8');
  Writeln(DXFfile, layer);
  Writeln(DXFfile, '10');
  Writeln(DXFfile, x);
  Writeln(DXFfile, '20');
  Writeln(DXFfile, y);
  Writeln(DXFfile, '30');
  Writeln(DXFfile, '0.0');
  Writeln(DXFfile, '42');
  Writeln(DXFfile, bulge);
end;


procedure AddLine(const x1,y1,x2,y2: Single;
                  const thickness,
                        bulge      : Single;
                  const color      : Integer;
                  const layer      : String);
begin
  Writeln(DXFfile, '0');
  Writeln(DXFfile, 'POLYLINE');   //LWPOLYLINE??
  Writeln(DXFfile, '8');
  Writeln(DXFfile, layer);
  Writeln(DXFfile, '66');
  Writeln(DXFfile, '1');

  if bulge > 0 then
  begin
    Writeln(DXFfile, '70');
    Writeln(DXFfile, '1');
  end;

  Writeln(DXFfile, '40');
  Writeln(DXFfile, thickness);
  Writeln(DXFfile, '41');
  Writeln(DXFfile, thickness);

  if color > 0 then
  begin
    Writeln(DXFfile, '62');
    Writeln(DXFfile, color);
  end;

  AddVertex(x1, y1, bulge, layer);
  AddVertex(x2, y2, bulge, layer);
  Writeln(DXFfile, '0');
  Writeln(DXFfile, 'SEQEND');
end;

procedure AddSubcatchPolyLine(aSubcatch: TSubcatch;
  const thickness, bulge: Single; const color: Integer; const layer: String);
var
  aVertex: PVertex;
begin
  Writeln(DXFfile, '0');
  Writeln(DXFfile, 'POLYLINE');   //LWPOLYLINE??
  Writeln(DXFfile, '8');
  Writeln(DXFfile, layer);
  Writeln(DXFfile, '66');
  Writeln(DXFfile, '1');
  Writeln(DXFfile, '70');
  Writeln(DXFfile, '1');

  Writeln(DXFfile, '40');
  Writeln(DXFfile, thickness);
  Writeln(DXFfile, '41');
  Writeln(DXFfile, thickness);

  if color > 0 then
  begin
    Writeln(DXFfile, '62');
    Writeln(DXFfile, color);
  end;

  aVertex := aSubcatch.Vlist.First;
  while aVertex <> nil do
  begin
    AddVertex(aVertex^.X, aVertex^.Y, bulge, layer);
    aVertex := aVertex^.Next;
  end;
  aVertex := aSubcatch.Vlist.First;
  if aVertex <> nil then
    AddVertex(aVertex^.X, aVertex^.Y, bulge, layer);
  Writeln(DXFfile, '0');
  Writeln(DXFfile, 'SEQEND');

end;

procedure AddLinkPolyLine(aLink: TLink; const  thickness, bulge: Single;
  const color: Integer; const layer: String);
var
  aVertex: PVertex;
begin
  Writeln(DXFfile, '0');
  Writeln(DXFfile, 'POLYLINE');   //LWPOLYLINE??
  Writeln(DXFfile, '8');
  Writeln(DXFfile, layer);
  Writeln(DXFfile, '66');
  Writeln(DXFfile, '1');

  if bulge > 0 then
  begin
    Writeln(DXFfile, '70');
    Writeln(DXFfile, '1');
  end;

  Writeln(DXFfile, '40');
  Writeln(DXFfile, thickness);
  Writeln(DXFfile, '41');
  Writeln(DXFfile, thickness);

  if color > 0 then
  begin
    Writeln(DXFfile, '62');
    Writeln(DXFfile, color);
  end;

  AddVertex(aLink.Node1.X, aLink.Node1.Y, bulge, layer);
  aVertex := aLink.Vlist.First;
  while aVertex <> nil do
  begin
    AddVertex(aVertex^.X, aVertex^.Y, bulge, layer);
    aVertex := aVertex^.Next;
  end;
  AddVertex(aLink.Node2.X, aLink.Node2.Y, bulge, layer);
  Writeln(DXFfile, '0');
  Writeln(DXFfile, 'SEQEND');
end;


procedure AddSolid(const x: array of Single;
                   const y: array of Single;
                   const color: Integer;
                   const layer: String);
var
  i : Integer;
begin
  Writeln(DXFfile, '0');
  Writeln(DXFfile, 'SOLID');
  Writeln(DXFfile, '8');
  Writeln(DXFfile, layer);
  for i := 0 to 3 do
  begin
    Writeln(DXFfile, IntToStr(10+i));
    Writeln(DXFfile, x[i]);
    Writeln(DXFfile, IntToStr(20+i));
    Writeln(DXFfile, y[i]);
    Writeln(DXFfile, IntToStr(30+i));
    Writeln(DXFfile, '0.0');
  end;
  if color > 0 then
  begin
    Writeln(DXFfile, '62');
    Writeln(DXFfile, color);
  end;
end;


procedure AddText(const x,y,ht,rot : Single;
                  const txt,layer  : String);
begin
  Writeln(DXFfile, '0');
  Writeln(DXFfile, 'TEXT');
  Writeln(DXFfile, '8');
  Writeln(DXFfile, layer);
  Writeln(DXFfile, '10');
  Writeln(DXFfile, x);
  Writeln(DXFfile, '20');
  Writeln(DXFfile, y);
  Writeln(DXFfile, '30');
  Writeln(DXFfile, '0.0');
  Writeln(DXFfile, '40');
  Writeln(DXFfile, ht);
  Writeln(DXFfile, '1');
  Writeln(DXFfile, txt);
  Writeln(DXFfile, '50');
  Writeln(DXFfile, rot);
end;


procedure AddArrow(const x1,y1,size: Single;
                   const asin,acos: Extended;
                   const color: Integer;
                   const layer: String);
var
  asize  : Single;
  x      : array[0..2] of Single;
  y      : array[0..2] of Single;
begin
  asize := size*PixPerMapExt*ArrowSize/2.0;
  x[0] := x1;
  y[0] := y1;
  x[1] := x1 + (-acos - 0.5*asin)*asize;
  y[1] := y1 + (-asin + 0.5*acos)*asize;
  x[2] := x1 + (-acos + 0.5*asin)*asize;
  y[2] := y1 + (-asin - 0.5*acos)*asize;
  AddLine(x[0], y[0], x[1], y[1], size, 0.0, color, layer);
  AddLine(x[0], y[0], x[2], y[2], size, 0.0, color, layer);
end;

procedure AddSubcatch(const J: Integer);
var
  color, k  : Integer;
  size      : Single;
  aSubcatch : TSubcatch;
begin
  aSubcatch := Project.GetSubcatch(SUBCATCH, J);
  if CurrentSubcatchVar = NOVIEW then k := -1
  else k := aSubcatch.ColorIndex;
  if k < 0 then color := 7
  else color := DXFLinkColor[k];
  size := SubcatchSize;
  AddSubcatchPolyLine(aSubcatch, size, 0.0, color, 'Subcatchments');
end;

procedure AddLink(const I: Integer; const J: Integer);
var
  color, k  : Integer;
  symbolflag: Boolean;
  x1, y1    : Single;
  x2, y2    : Single;
  asin,acos : Extended;
  size      : Single;
  P1, P2    : TPoint;
  aLink     : TLink;
begin
  aLink := Project.GetLink(I,J);
  with aLink do
  begin
    x1 := Node1.X;
    y1 := Node1.Y;
    if (x1 = MISSING) or (y1 = MISSING) then exit;
    x2 := Node2.X;
    y2 := Node2.Y;
    if (x2 = MISSING) or (y2 = MISSING) then exit;
  end;
  if CurrentLinkVar = NOVIEW then k := -1
  else k := Project.GetLink(I,J).ColorIndex;
  if k < 0 then color := 7
  else color := DXFLinkColor[k];
  size := LinkSize;
  if (MapOptions.ShowLinksBySize) then
  begin
    if (k >= 0) then size := size + k/PixPerMapExt;
  end;
  AddLinkPolyLine(aLink, size, 0.0, color, 'Links');
  symbolflag := False;
  if (MapOptions.ArrowStyle <> asNone)
  and (MapOptions.ArrowZoom = 100)
  and (not symbolflag) then
  begin
    Map.GetNodePixPos(aLink.Node1, P1);
    Map.GetNodePixPos(aLink.Node2, P2);
    if aLink.Vlist.Count > 0 then with MapForm.Map do
    begin
      GetLinkMidsegment(aLink, P1, P2);
      x1 := GetX(P1.X);
      y1 := GetY(P1.Y);
      x2 := GetX(P2.X);
      y2 := GetY(P2.Y);
    end;
    SinCos(arctan2(y2 - y1, x2 - x1), asin, acos);
    if RunFlag then
    begin
      k := aLink.OutFileIndex;
      if (k >= 0) then
      case FlowDir[k] of
        PLUS:  AddArrow((x1+x2)/2.0, (y1+y2)/2.0, size, asin, acos, color, 'Arrows');
        MINUS: AddArrow((x1+x2)/2.0, (y1+y2)/2.0, size, -asin, -acos, color, 'Arrows');
      end;
    end
    else
      AddArrow((x1+x2)/2.0, (y1+y2)/2.0, size, asin, acos, color, 'Arrows');
  end;
end;


procedure AddNode(const I: Integer; const J: Integer);
var
  color, k : Integer;
  x1, y1,
  radius   : Single;
  x, y     : array[0..3] of Single;
begin
  x1 := Project.GetNode(I, J).X;
  y1 := Project.GetNode(I, J).Y;
  if (x1 = MISSING) or (y1 = MISSING) then Exit;

  if CurrentNodeVar = NOVIEW then k := -1
  else k := Project.GetNode(I, J).ColorIndex;
  if k < 0 then color := 7
  else color := DXFLinkColor[k];

  radius := NodeSize;
  if (MapOptions.ShowNodesBySize) then
  begin
    if (k >= 0) then radius := radius +  k/PixPerMapExt;
  end;
  case JuncStyle of
    0: AddCircle(x1, y1, radius, color, 'Nodes');
    1: AddLine(x1-radius, y1, x1+radius, y1, 2*radius, 1, color, 'Nodes');
    2: begin
         x[0] := x1 - radius;
         y[0] := y1 - radius;
         x[1] := x1 + radius;
         y[1] := y[0];
         x[2] := x[0];
         y[2] := y1 + radius;
         x[3] := x[1];
         y[3] := y[2];
         AddSolid(x, y, color, 'Nodes');
       end;
  end;
end;


procedure AddLabels;
var
  I   : Integer;
  X,Y : Single;
  aLabel: TMapLabel;
begin
  with Project.Lists[MAPLABEL] do
    for I := 0 to Count-1 do
    begin
      X := Project.GetMapLabel(I).X;
      Y := Project.GetMapLabel(I).Y;
      if (X <> MISSING) and (Y <> MISSING) then
      begin
        with Map.Bitmap.Canvas do
        begin
          aLabel := Project.GetMapLabel(I);
          Font.Name := aLabel.FontName;
          Font.Size := aLabel.FontSize;
          LabelSize := 0.75*TextHeight(Strings[I])/PixPerMapExt;
          Y := Y - LabelSize;
          AddText(X, Y, 0.75*LabelSize, 0.0, Strings[I], 'Labels');
        end;
      end;
    end;
end;


procedure DXFexport(const Fname: String; const Jstyle: Integer);
var
  I,J,N : Integer;
begin
  AssignFile(DXFfile,Fname);
  {$I-}
  Rewrite(DXFfile);
  {$I+}
  if (IOResult = 0) then
  begin

    Map := MapForm.Map;
    MapOptions := Map.Options;
    PixPerMapExt := 1.0 / MapForm.Map.Window.WperP;
    JuncStyle := Jstyle;
    SubcatchSize := MapOptions.SubcatchLineSize/PixPerMapExt;
    LinkSize := MapOptions.LinkSize/PixPerMapExt;
    NodeSize := MapOptions.NodeSize/PixPerMapExt;
    LabelSize := 8.0/PixPerMapExt;
    ArrowSize := 2*MapOptions.ArrowSize/PixPerMapExt;
    for I := 0 to MAXINTERVALS do
    begin
      DXFLinkColor[I] := GetDXFColor(MapLinkColor[I]);
      DXFNodeColor[I] := GetDXFColor(MapNodeColor[I]);
      DXFSubcatchColor[I] := GetDXFColor(MapSubcatchColor[I]);
    end;

    try
      with MapForm.Map.Dimensions do
        AddHeader(LowerLeft.X, LowerLeft.Y, UpperRight.X, UpperRight.Y);
      StartTables(5);             // #Layers = 5
      AddLayer('Subcatchments',7);
      AddLayer('Links',7);
      AddLayer('Nodes',7);
      AddLayer('Arrows',7);
      AddLayer('Labels',7);
      EndTables;

      if MapOptions.ShowSubcatchs then
      begin
        N := Project.Lists[SUBCATCH].Count-1;
        for J := 0 to N do  AddSubcatch(J);
      end;

      for I := 0 to MAXCLASS do
      begin
        if not Project.IsLink(I) then continue;
        N := Project.Lists[I].Count - 1;
        for J := 0 to N do AddLink(I, J);
      end;

      if (MapOptions.ShowNodes) then
      begin
        for I := 0 to MAXCLASS do
        begin
          if not Project.IsNode(I) then continue;
          N := Project.Lists[I].Count - 1;
          for J := 0 to N do AddNode(I, J);
        end;
      end;

      if MapOptions.ShowLabels then AddLabels;
      EndDXF;

    finally
    end;
  end;
  CloseFile(DXFfile);
end;

end.
