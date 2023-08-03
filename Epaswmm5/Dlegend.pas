unit Dlegend;

{-------------------------------------------------------------------}
{                    Unit:    Dlegend.pas                           }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form unit for editing a map legend.                      }
{                                                                   }
{   NOTE: built to accommodate only 5 colors.                       }
{-------------------------------------------------------------------}

interface

uses
  SysUtils, Types, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ColorGrd, ExtCtrls, Grids, StdCtrls, Buttons,
  System.UITypes, Uglobals, Uutils;

type
  TLegendForm = class(TForm)
    Box0: TShape;
    Box1: TShape;
    Box2: TShape;
    Box3: TShape;
    Box4: TShape;

    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    BtnAutoScale: TButton;
    BtnColorRamp: TButton;
    BtnReverse: TButton;
    Panel1: TPanel;
    BtnOK: TButton;
    BtnCancel: TButton;
    ColorDialog1: TColorDialog;
    Hiliter: TShape;
    NameLabel: TLabel;
    UnitsLabel: TLabel;
    Bevel1: TBevel;
    BtnHelp: TButton;
    CheckFramed: TCheckBox;

    procedure FormCreate(Sender: TObject);
    procedure BtnAutoScaleClick(Sender: TObject);
    procedure BtnColorRampClick(Sender: TObject);
    procedure BtnReverseClick(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BtnHelpClick(Sender: TObject);
    procedure CheckFramedClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

  private
    { Private declarations }
    LegendType     : Integer;
    LegendVarIndex : Integer;
    LegendPeriod   : Integer;
    LegendDigits   : Integer;
    function  ValidateInput: Boolean;

  public
    { Public declarations }
    Modified: Boolean;
    procedure SetData(const VarIndex: Integer; const VarName: String;
      const VarUnits: String; const Digits: Integer; const TimePeriod: Integer;
      const Legend: TMapLegend; const Colors: array of TColor;
      const Framed: Boolean);
    procedure GetData(var Legend: TMapLegend; var Colors: array of TColor;
      var Framed: Boolean);
  end;

implementation

{$R *.DFM}

uses
  Fmain, Dcolramp, Uoutput;

const
  MSG_RANGE_TOO_SMALL = 'Range too small.';
  MSG_NO_INTERVALS = 'No intervals specified.';

procedure TLegendForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnCreate handler for form.
//-----------------------------------------------------------------------------
var
  P: TPoint;
begin
  // Adjust size of the highlighting box to fit around the color boxes
  Hiliter.Height := Box0.Height + 4;
  Hiliter.Width := Box0.Width + 4;

  // Position form at top left of Main form
  with MainForm do
  begin
    P.x := Left + (Width - ClientWidth) - 2;
    P.Y := Top + (Height-ClientHeight) - 2;
  end;
  Top := P.Y;
  Left := P.X;
  Modified := False;
end;


function TLegendForm.ValidateInput: Boolean;
//-----------------------------------------------------------------------------
// Checks that form's text boxes contain valid numbers.
//-----------------------------------------------------------------------------
var
  i       : Integer;
  count   : Integer;  //Count of non-blank edit boxes
  number  : Single;   //Numeric value in an edit box
begin
  Result := False;
  count := 0;

  // Check each edit component
  for i := 1 to MAXINTERVALS do
    with FindComponent('Edit' + IntToStr(i)) as TEdit do
    begin

      // If not blank and not a valid number then exit
      if Text <> '' then
      begin
        if not Uutils.IsValidNumber(Text,number) then
        begin
          SetFocus;
          Exit;
        end
        else Inc(count);
      end;
      if Modified then self.Modified := True;
    end;

  // Issue warning message if no edit boxes filled in
  if count = 0 then
  begin
    Uutils.MsgDlg(MSG_NO_INTERVALS, mtWarning, [mbOK]);
    Edit1.SetFocus;
  end
  else
    Result := True;
end;


procedure TLegendForm.SetData(const VarIndex: Integer; const VarName: String;
  const VarUnits: String; const Digits: Integer; const TimePeriod: Integer;
  const Legend: TMapLegend; const Colors: array of TColor; const Framed: Boolean);
//-----------------------------------------------------------------------------
//  Loads legend information into form.
//    VarIndex = index of legend variable
//    VarName = legend variable's name
//    VarUnits = legend variable's measurement units
//    Digits = number of decimal digits to display numbers with
//    TimePeriod = time period legend applies to
//    Legend = legend record
//    Colors = legend's colors
//    Framed = true if legend framed
//-----------------------------------------------------------------------------
var
  I: Integer;
  S: String;
begin
  // Save legend information
  NameLabel.Caption := VarName;
  UnitsLabel.Caption := VarUnits;
  LegendPeriod := TimePeriod;
  LegendType := Legend.Ltype;
  LegendVarIndex := VarIndex;
  LegendDigits := Digits;
  CheckFramed.Checked := Framed;

  // Assign legend colors to the color boxes
  for I := 0 to MAXINTERVALS do
    with FindComponent('Box' + IntToStr(I)) as TShape do
      Brush.Color := Colors[I];

  // Assign legend interval values to edit controls
  for I := 1 to MAXINTERVALS do
  begin
    if Legend.Intervals[I] = MISSING
    then S := ''
    else S := FloatToStrF(Legend.Intervals[I], ffFixed, 7, LegendDigits);
    with FindComponent('Edit' + IntToStr(I)) as TEdit do
      Text := S;
  end;
end;


procedure TLegendForm.GetData(var Legend: TMapLegend;
  var Colors: array of TColor; var Framed: Boolean);
//-----------------------------------------------------------------------------
//  Unloads contents of form into Legend data structure, consolidating any
//  gaps left by blank entries.
//-----------------------------------------------------------------------------
var
  I, N  : Integer;
  Lasti : Integer;
  Code  : Integer;
  Number: Single;
  S     : String;
begin
  // Initialize count of non-blank intervals
  N := 0;
  Lasti := 1;

  // Examine each interval's edit control
  for I := 1 to MAXINTERVALS do
  begin

    // Initialize legend interval to MISSING (equivalent to blank)
    Legend.Intervals[I] := MISSING;

    // Retrieve text from edit control
    with FindComponent('Edit' + IntToStr(I)) as TEdit do
      S := Text;

    // If text not blank, then update Legend structure
    if S <> '' then
    begin

      // Save index of this edit control
      Lasti := I;

      // Store color of color box appearing above edit control
      with FindComponent('Box' + IntToStr(I-1)) as TShape do
        Colors[N] := Brush.Color;

      // Increment non-blank interval count & store new interval value
      Inc(N);
      val(S, Number, Code);
      Legend.Intervals[N] := Number;
    end;
  end;

  // Store color of color box appearing below last non-blank interval
  with FindComponent('Box' + IntToStr(Lasti)) as TShape do
    Colors[N] := Brush.Color;

  // Store number of non-blank intervals
  Legend.Nintervals := N;
  Framed := CheckFramed.Checked;
end;


procedure TLegendForm.BtnAutoScaleClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for Auto Scale button. Divides range of Legend variable
// values into equal intervals.
//-----------------------------------------------------------------------------
var
  I, N        : Integer;
  Dx, X       : Single;
  Xmin, Xmax  : Single;
begin
  Xmin := 1e20;
  Xmax := 0;
  if (LegendType = SUBCATCHMENTS) then
    Uoutput.GetSubcatchMinMax(LegendVarIndex, LegendPeriod, Xmin, Xmax)
  else if (LegendType = NODES) then
    Uoutput.GetNodeMinMax(LegendVarIndex, LegendPeriod, Xmin, Xmax)
  else
    Uoutput.GetLinkMinMax(LegendVarIndex, LegendPeriod, Xmin, Xmax);
  if (Xmax = MISSING) then
    Uutils.MsgDlg(MSG_RANGE_TOO_SMALL, mtWarning, [mbOK])
  else
  begin
    try
      N := Trunc(Xmin/10);
      Xmin := 10*N;
      if (Xmax < 10) then Xmax := Trunc(Xmax) + 1
      else
      begin
        N := Trunc(Xmax/10) + 1;
        Xmax := 10*N;
      end;
      Dx := (Xmax - Xmin)/(MAXINTERVALS+1);
    except
      Uutils.MsgDlg(MSG_RANGE_TOO_SMALL, mtWarning, [mbOK]);
      Exit;
    end;
    for i := 1 to MAXINTERVALS do
    begin
      X := Xmin + I*Dx;
      with FindComponent('Edit' + IntToStr(i)) as TEdit do
        Text := FloatToStrF(X, ffFixed, 7, LegendDigits);
    end;
    Modified := True;
  end;
end;


procedure TLegendForm.BtnColorRampClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for Color Ramp button. Selects colors from a set of
// pre-defined color schemes.
//-----------------------------------------------------------------------------
var
  I : Integer;
  N : Integer;
  ColorRampDlg : TColorRampForm;
begin

  // Create the color ramp dialog form and display modally
  ColorRampDlg := TColorRampForm.Create(self);
  try
    if ColorRampDlg.ShowModal = mrOK then
    begin

      // Transfer colors from dialog's color boxes to Legend Editor's
      Modified := True;
      N := High(ColorRampDlg.Colors);
      if N > MAXINTERVALS then N := MAXINTERVALS;
      for I := 0 to N do
      try
        with self.FindComponent('Box' + IntToStr(I)) as TShape do
          Brush.Color := ColorRampDlg.Colors[I];
      finally
      end;
    end;

  // Free the dialog form
  finally
    ColorRampDlg.Free;
  end;
end;


procedure TLegendForm.BtnReverseClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for Reverse Colors button. Reverses color ordering of
// legend's color boxes.
//-----------------------------------------------------------------------------
var
  TmpColor: array [0..MAXINTERVALS] of TColor;
  I : Integer;
begin
  for I := 0 to MAXINTERVALS do
    with FindComponent('Box' + IntToStr(I)) as TShape do
      TmpColor[I] := Brush.Color;
  for I := 0 to MAXINTERVALS do
    with FindComponent('Box' + IntToStr(I)) as TShape do
      Brush.Color := TmpColor[MAXINTERVALS-I];
  Modified := True;
end;


procedure TLegendForm.BtnOKClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for OK button. Validates numeric entries in text boxes.
//-----------------------------------------------------------------------------
begin
  if not ValidateInput then ModalResult := mrNone
  else if not Modified then ModalResult := mrCancel
  else ModalResult := mrOK;
end;


procedure TLegendForm.BtnCancelClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for Cancel button.
//-----------------------------------------------------------------------------
begin
  ModalResult := mrCancel;
end;


procedure TLegendForm.CheckFramedClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for Framed checkbox.
//-----------------------------------------------------------------------------
begin
  Modified := True;
end;


procedure TLegendForm.BoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
//-----------------------------------------------------------------------------
// OnMouseDown handler for all of the color boxes (Shape controls).
// Highlights the box and opens up a color selection dialog form.
//-----------------------------------------------------------------------------
var
  aBox : TShape;
begin

// Highlight box clicked on
  aBox := TShape(Sender);
  Hiliter.Left := aBox.Left - 2;
  Hiliter.Top := aBox.Top - 2;
  Hiliter.Visible := True;

// Execute standard color dialog to get new color for box
  ColorDialog1.Color := TShape(Sender).Brush.Color;
  try
    if ColorDialog1.Execute then
    begin
      TShape(Sender).Brush.Color := ColorDialog1.Color;
      Modified := True;
    end;
  finally
    Hiliter.Visible := False;
  end;
end;


procedure TLegendForm.BtnHelpClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 211610);
end;

procedure TLegendForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then BtnHelpClick(Sender);
end;

end.
