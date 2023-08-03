unit Uvertex;

{-------------------------------------------------------------------}
{                    Unit:    Uvertex.pas                           }
{                    Project: EPA SMM                               }
{                    Version: 5.2                                   }
{                    Date:    06/27/21   (5.2.4)                    }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Delphi Pascal unit that defines and implements the              }
{   TVertexList class. This is a linked list of two-dimensional     }
{   points that define the vertices of a polyline or polygon shape. }
{-------------------------------------------------------------------}

interface

uses
  SysUtils, Classes, Dialogs;

type

//----------------------
// Vertex data structure
//----------------------
  PVertex = ^TVertex;
  TVertex = record
    X    : Extended;
    Y    : Extended;
    Next : PVertex;
  end;

//-------------------
// Vertex List class
//-------------------
  TVertexList = class(TObject)
    Vfirst   : PVertex;
    Vcurrent : PVertex;
    constructor Create;
    destructor  Destroy; override;
    function    Count: Integer;
    function    First: PVertex;
    function    Next:  PVertex;
    function    Find(X1,X2,Y1,Y2: Extended): PVertex;
    function    Add(X,Y: Extended): PVertex;
    function    Delete(aVertex: PVertex): PVertex;
    procedure   Clear;
    procedure   ClearPolygon;
    procedure   Update(X,Y: Extended);
    procedure   Reverse;
    procedure   Move(DX,DY: Extended);
    procedure   Assign(aVlist: TVertexList);
    function    AddPolygonVertex: PVertex;
    function    PolygonCentroid(var X: Extended; var Y: Extended): Boolean;
    function    PolygonArea: Extended;
  end;

implementation

//-------------------------
// VertexList class methods
//-------------------------

constructor TVertexList.Create;
//-----------------------------------------------------------------------------
// Constructor method.
//-----------------------------------------------------------------------------
begin
  inherited Create;
  Vfirst := nil;
  Vcurrent := nil;
end;

destructor TVertexList.Destroy;
//-----------------------------------------------------------------------------
// Destructor method.
//-----------------------------------------------------------------------------
begin
  Clear;
  inherited Destroy;
end;

procedure TVertexList.Clear;
//-----------------------------------------------------------------------------
// Clears and deletes all vertices in the list.
//-----------------------------------------------------------------------------
begin
  Vcurrent := Vfirst;
  while Vcurrent <> nil do
  begin
    Vfirst := Vcurrent^.Next;
    Dispose(Vcurrent);
    Vcurrent := Vfirst;
  end;
end;

procedure  TVertexList.ClearPolygon;
//-----------------------------------------------------------------------------
// Removes a polygon's vertices.
//-----------------------------------------------------------------------------
var
  X, Y: Extended;
begin
  PolygonCentroid(X, Y);
  Clear;
  Add(X, Y);
end;

function TVertexList.Count: Integer;
//-----------------------------------------------------------------------------
// Counts the number of vertices in the list.
//-----------------------------------------------------------------------------
var
  V: PVertex;
begin
  Result := 0;
  V := Vfirst;
  while V <> nil do
  begin
    Inc(Result);
    V := V^.Next;
  end;
end;

function TVertexList.First: PVertex;
//-----------------------------------------------------------------------------
//  Returns the first vertex in the list.
//-----------------------------------------------------------------------------
begin
  Vcurrent := Vfirst;
  Result := Vcurrent;
end;

function TVertexList.Next: PVertex;
//-----------------------------------------------------------------------------
// Returns the next vertex in the list.
//-----------------------------------------------------------------------------
begin
  if (Vfirst = nil) or (Vcurrent^.Next = nil) then Result := nil
  else
  begin
    Vcurrent := Vcurrent^.Next;
    Result := Vcurrent;
  end;
end;

function TVertexList.Find(X1,X2,Y1,Y2: Extended): PVertex;
//-----------------------------------------------------------------------------
// Finds the vertex that lies within a rectangular region.
//-----------------------------------------------------------------------------
var
  V: PVertex;
begin
  V := Vfirst;
  while V <> nil do
  begin
    if (V^.X >= X1) and (V^.X <= X2)
    and (V^.Y >= Y1) and (V^.Y <= Y2) then break;
    V := V^.Next;
  end;
  if V <> nil then Vcurrent := V;
  Result := V;
end;

function TVertexList.Add(X,Y: Extended): PVertex;
//-----------------------------------------------------------------------------
// Adds a new vertex to the list.
//-----------------------------------------------------------------------------
var
  V: PVertex;
begin
  New(V);
  V^.X := X;
  V^.Y := Y;
  if Vcurrent = nil then Vcurrent := Vfirst;
  if Vfirst = nil then
  begin
    V^.Next := nil;
    Vfirst := V;
  end
  else
  begin
    V^.Next := Vcurrent^.Next;
    Vcurrent^.Next := V;
  end;
  Vcurrent := V;
  Result := V;
end;

function TVertexList.Delete(aVertex: PVertex): PVertex;
//-----------------------------------------------------------------------------
// Deletes a vertex form the list.
//-----------------------------------------------------------------------------
var
  V: PVertex;
begin
  Result := nil;
  if (aVertex = nil) then exit;
  if Vfirst = aVertex then
  begin
    Vfirst := Vfirst^.Next;
    Dispose(aVertex);
    Vcurrent := Vfirst;
  end
  else
  begin
    V := Vfirst;
    while V <> nil do
    begin
      if V^.Next = aVertex then
      begin
        V^.Next := aVertex^.Next;
        Dispose(aVertex);
        Vcurrent := V;
        break;
      end;
      V := V^.Next;
    end;
  end;
  Result := Vcurrent;
end;

procedure TVertexList.Update(X,Y: Extended);
//-----------------------------------------------------------------------------
// Updates the X,Y coordinates of the current vertex.
//-----------------------------------------------------------------------------
begin
  if Vcurrent <> nil then
  begin
    Vcurrent^.X := X;
    Vcurrent^.Y := Y;
  end;
end;

procedure TVertexList.Reverse;
//-----------------------------------------------------------------------------
// Reverses the order of the vertices in the list.
//-----------------------------------------------------------------------------
var
  V, Vprior, Vnext: PVertex;
begin
  Vprior := nil;
  V := Vfirst;
  while V <> nil do
  begin
    Vfirst := V;
    Vnext := V^.Next;
    V^.Next := Vprior;
    Vprior := V;
    V := Vnext;
  end;
end;

procedure TVertexList.Move(DX,DY: Extended);
//-----------------------------------------------------------------------------
// Moves each vertex in the list by the same amount.
//-----------------------------------------------------------------------------
var
  V: PVertex;
begin
  V := Vfirst;
  while V <> nil do
  begin
    V^.X := V^.X + DX;
    V^.Y := V^.Y + DY;
    V := V^.Next;
  end;
end;

procedure TVertexList.Assign(aVlist: TVertexList);
//-----------------------------------------------------------------------------
// Replaces the list's vertices with those from another list.
//-----------------------------------------------------------------------------
var
  V: PVertex;
begin
  Clear;
  V := aVlist.First;
  while V <> nil do
  begin
    Add(V^.X, V^.Y);
    V := aVlist.Next;
  end;
end;

function TVertexList.AddPolygonVertex: PVertex;
//
// Adds a new vertex to a polygon.
//
var
  Vx, Vy : Extended;
  Vnext  : PVertex;
begin
  Vnext := Vcurrent^.Next;
  if Vnext = nil then Vnext := Vfirst;
  if (Vcurrent = nil) or (Vnext = nil) then Result := nil
  else
  begin
    Vx := (Vcurrent^.X + (Vnext)^.X)/2;
    Vy := (Vcurrent^.Y + (Vnext)^.Y)/2;
    Result := Add(Vx,Vy);
  end;
end;

function TVertexList.PolygonCentroid(var X: Extended; var Y: Extended): Boolean;
//
//  Computes the centroid coordinates of a polygon outline.
//  Uses the method described in the book Graphics Gems IV.
//
var
  X1, Y1: Extended;
  Xmin, Ymin: Extended;
  Xsum, Ysum: Extended;
  A, Z: Extended;
  V: PVertex;

begin
  // Case of no vertices
  Result := True;
  if Count = 0 then
  begin
    X := -1e10;
    Y := -1e10;
    Result := False;
    Exit;
  end;

  // Case of just a single vertex
  Xsum := 0.0;
  Ysum := 0.0;
  A := 0.0;
  V := First;
  X := V^.X;
  Y := V^.Y;
  if Count = 1 then Exit;

  // For 2 vertices, use average X and Y
  V := V^.Next;
  if Count = 2 then
  begin
    X := (X + V^.X) / 2.;
    Y := (Y + V^.Y) / 2.;
    Exit;
  end;

  // Otherwise use the centroid formula
  try

    // First find the min. X & Y for scaling purposes
    Xmin := X;
    Ymin := Y;
    while V <> nil do
    begin
      if V^.X < Xmin then Xmin := V^.X;
      if V^.Y < Ymin then Ymin := V^.Y;
      V := V^.Next;
    end;
    X := X - Xmin;
    Y := Y - Ymin;

    // Now compute the sums used in the centroid formula
    // using scaled X & Y vertex points
    V := First;
    while V <> nil do
    begin
      X1 := V^.X - Xmin;
      Y1 := V^.Y - Ymin;
      Z := (X*Y1) - (Y*X1);
      A := A + Z;
      Xsum := Xsum + (X + X1)*Z;
      Ysum := Ysum + (Y + Y1)*Z;
      X := X1;
      Y := Y1;
      V := V^.Next;
    end;
    V := First;
    X1 := V^.X - Xmin;
    Y1 := V^.Y - Ymin;
    Z := (X*Y1) - (Y*X1);
    A := (A + Z) / 2.0;

    // Raise an exception if the area is too small
    if Abs(A) < 1.0e-100 then raise EMathError.Create('');

    // Otherwise compute the centroid X & Y
    Xsum := Xsum + (X + X1)*Z;
    Ysum := Ysum + (Y + Y1)*Z;
    A := 6.0 * A;
    X := Xmin + Xsum/A;
    Y := Ymin + Ysum/A;

  // Alternate backup method in case of a math exception
  // (If we get this far into the procedure, Vlist.Count
  //  will always be > 2).
  except
    on EMathError do
    begin
      Xsum := 0.0;
      Ysum := 0.0;
      V := First;
      while V <> nil do
      begin
        Xsum := Xsum + V^.X;
        Ysum := Ysum + V^.Y;
        V := V^.Next;
      end;
      X := Xsum / Count;
      Y := Ysum / Count;
    end;
  end;
end;


function TVertexList.PolygonArea: Extended;
//
//  Finds the area encompassed by a polygon outline.
//  Uses the method described in the book Graphics Gems IV.
//
var
  X:  Extended;
  Y:  Extended;
  A:  Extended;
  V:  PVertex;
  X1, Y1: Extended;
  Xmin, Ymin: Extended;
begin

  // There must be at least 3 vertices to compute an area
  A := 0.0;
  if Count >= 3 then
  begin

    // Find min. X & Y of all vertices
    V := First;
    X := V^.X;
    Y := V^.Y;
    Xmin := X;
    Ymin := Y;
    V := V^.Next;
    while V <> nil do
    begin
      if V^.X < Xmin then Xmin := V^.X;
      if V^.Y < Ymin then Ymin := V^.Y;
      V := V^.Next;
    end;

    // Compute terms of the area formula using scaled X & Y values
    V := First;
    X := V^.X - Xmin;
    Y := V^.Y - Ymin;
    V := V^.Next;
    while V <> nil do
    begin
      X1 := V^.X - Xmin;
      Y1 := V^.Y - Ymin;
      A := A + X*Y1 - Y*X1;
      X := X1;
      Y := Y1;
      V := V^.Next;
    end;
    V := First;
    X1 := V^.X - Xmin;
    Y1 := V^.Y - Ymin;
    A := A + X*Y1 - Y*X1;
    A := Abs(A)/2.0;
  end;
  Result := A;
end;

end.
