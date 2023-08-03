unit Ulegend;

{-------------------------------------------------------------------}
{                    Unit:    Ulegend.pas                           }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21  (5.2.0)                     }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Delphi Pascal unit that handles drawing and editing of the      }
{   study area map legends.                                         }
{-------------------------------------------------------------------}

interface

uses
  Windows, Graphics, SysUtils, Dialogs, Forms, Classes,
  Controls, Math, System.UITypes, Uglobals, Uutils;

procedure DrawLegend(const R: TRect;  const C: TCanvas; const BackColor: TColor;
  const Framed: Boolean; const Legend: TMapLegend; const VarIndex: Integer;
  const Digits: Integer; const MapColor: array of TColor);
procedure DrawTimeLegend(R: TRect; C: TCanvas; const BackColor: TColor;
  const S: String);
function EditLegend(var Legend: TMapLegend; const VarIndex: Integer;
  const Digits: Integer; const TimePeriod: Integer; var Colors: array of TColor;
  var Framed: Boolean): Boolean;

implementation

uses
  Dlegend;

procedure DrawLegend(const R: TRect;  const C: TCanvas; const BackColor: TColor;
  const Framed: Boolean; const Legend: TMapLegend; const VarIndex: Integer;
  const Digits: Integer; const MapColor: array of TColor);
//-----------------------------------------------------------------------------
//  Draws map legend Legend with colors MapColor in rectangle
//  R on canvas C with background color BackColor.
//-----------------------------------------------------------------------------
var
  D, X, Y  : Integer;
  I, Dy    : Integer;
  W, N     : Integer;
  Units    : String;         // Measurement units of the legend's values
  ObjTxt   : String;         // Name of object class (node, link, etc.)
  VarTxt   : String;         // Name of quantity in legend (flow, depth, etc.)
  Bordercolor: TColor;
begin
  // Retrieve the name of the object type, the quantity, and
  // its units to display in the legend
  Uglobals.GetObjVarNames(Legend.Ltype, VarIndex, ObjTxt, VarTxt, Units);

  with C, Legend do
  begin
    // Determine legend width
    Font.Name := 'Arial';
    Font.Size := 8;
    if BoldFonts then Font.Style := [fsBold];
    Dy := TextHeight('[');
    D := Dy div 2;
    W := TextWidth(VarTxt);
    for I := 1 to Nintervals do
    begin
      N := TextWidth(FloatToStrF(Intervals[I], ffFixed, 7, Digits));
      if N > W then W := N;
    end;

    // Clear canvas background.
    Brush.Color := BackColor;
    Brush.Style := bsSolid;
    Bordercolor := clBlack;
    if BackColor = clBlack then Bordercolor := clWhite;
    Pen.Color := Bordercolor;
    Font.Color := Bordercolor;
    if Framed then Rectangle(R.Left, R.Top, R.Right, R.Bottom)
    else FillRect(R);

    // Draw title
    SetBkMode(Handle, TRANSPARENT);
    X := R.Left + 3*D;
    Y := R.Top + D div 2;
    TextOut(X, Y, ObjTxt);
    Y := Y + Dy;
    TextOut(X, Y, VarTxt);
    Y := Y + D;

    // Draw color bars
    Pen.Color := clBlack;
    for I := 0 to Nintervals do
    begin
      X := R.Left + D;
      Brush.Color := MapColor[I];
      Rectangle(X, Y, X+D, Y+D+Dy);
      Y := Y + D + Dy - 1;
    end;

    // Draw scale labels
    Brush.Style := bsClear;
    X := R.Left + D + 2*D;
    Y := R.Top + 2*Dy + D + (D div 2);
    for I := 1 to Nintervals do
    begin
      TextOut(X, Y, FloatToStrF(Intervals[I], ffFixed, 7, Digits));
      Y := Y + Dy + D - 1;
    end;

    // Draw units label
    TextOut(X, Y, Units);
  end;
end;


function EditLegend(var Legend: TMapLegend; const VarIndex: Integer;
  const Digits: Integer; const TimePeriod: Integer; var Colors: array of TColor;
  var Framed: Boolean): Boolean;
//-----------------------------------------------------------------------------
//  Launches the Legend Editor dialog box to modify map colors and intervals.
//-----------------------------------------------------------------------------
var
  ObjTxt:   String;
  VarTxt:   String;
  VarUnits: String;
  LegendForm: TLegendForm;
begin
  // Determine variable type & its units displayed in legend
  Result := True;
  Uglobals.GetObjVarNames(Legend.Ltype, VarIndex, ObjTxt, VarTxt, VarUnits);

  // Launch the Legend Editor dialog
  LegendForm := TLegendForm.Create(Application);
  with LegendForm do
  try
    SetData(VarIndex, VarTxt, VarUnits, Digits, TimePeriod, Legend, Colors, Framed);
    if ShowModal = mrOK then GetData(Legend, Colors, Framed)
    else Result := False;
  finally
    Free;
  end;
end;


procedure DrawTimeLegend(R: TRect; C: TCanvas; const BackColor: TColor;
  const S: String);
//-----------------------------------------------------------------------------
//  Displays the Time Legend text S in rectangle R on map canvas C.
//-----------------------------------------------------------------------------
var
  Buff: array[0..255] of Char;
  BorderColor: TColor;
begin
  with C do
  begin
    // Set brush & pen properties
    Brush.Color := BackColor;
    Brush.Style := bsSolid;
    BorderColor := clBlack;
    if BackColor = clBlack then BorderColor := clWhite;
    Pen.Color := BorderColor;

    // Draw time legend text
    SetBkMode(Handle,TRANSPARENT);
    Font.Name := 'Arial';
    Font.Style := [fsBold];
    Font.Size := 8;
    Font.Color := Pen.Color;
    StrPCopy(Buff, S);
    DrawText(Handle, PChar(S), -1, R, DT_CENTER or DT_VCENTER);
  end;
end;

end.
