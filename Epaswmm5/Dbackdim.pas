unit Dbackdim;

{-------------------------------------------------------------------}
{                    Unit:    Dbackdim.pas                          }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21     (5.2.0)                  }
{                    Author:  L. Rossman                            }
{                                                                   }
{  Dialog form unit that allows the user to set the dimensions of   }
{  the backdrop picture for the study area map.                     }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, NumEdit, Uproject, Uglobals, Uutils;

type
  TBackdropDimensionsForm = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    LLYEdit: TNumEdit;
    LLXEdit: TNumEdit;
    Label3: TLabel;
    Label4: TLabel;
    GroupBox2: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    URYEdit: TNumEdit;
    URXEdit: TNumEdit;
    OKBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    ScaleBackdropBtn: TRadioButton;
    ScaleMapBtn: TRadioButton;
    ResizeOnlyBtn: TRadioButton;
    MapLLX: TEdit;
    MapLLY: TEdit;
    MapURX: TEdit;
    MapURY: TEdit;
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure OKBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure ResizeOnlyBtnClick(Sender: TObject);
    procedure ScaleBackdropBtnClick(Sender: TObject);
    procedure ScaleMapBtnClick(Sender: TObject);
    procedure LLXEditChange(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
  private
    { Private declarations }
    BackLL: TExtendedPoint;
    BackUR: TExtendedPoint;
    MapLL : TExtendedPoint;
    MapUR : TExtendedPoint;
    procedure EnableEditFields(Enable: Boolean);
    procedure ScaleMapToBackdrop;
    procedure DisplayBackdropDimensions(LL, UR: TExtendedPoint; D: Integer);
    procedure DisplayMapDimensions(LL, UR: TExtendedPoint; D: Integer);
  public
    { Public declarations }
    procedure SetData;
  end;

var
  BackdropDimensionsForm: TBackdropDimensionsForm;

implementation

{$R *.dfm}

uses Fmap, Fovmap, Ucoords;

const
  MSG_BLANK_FIELD = 'Blank data field not allowed.';
  MSG_ILLEGAL_DIMENSIONS = 'Illegal dimensions.';

procedure TBackdropDimensionsForm.FormShow(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnShow handler.
//-----------------------------------------------------------------------------
begin
  SetData;
  ResizeOnlyBtn.Checked := True;
  LLXEdit.SetFocus;
end;

procedure TBackdropDimensionsForm.OKBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the OKBtn.
//-----------------------------------------------------------------------------
var
  x1,x2,y1,y2: Extended;
  BadNumEdit: TNumEdit;
begin
  // Find which NumEdit control has an invalid numerical entry.
  BadNumEdit := nil;
  if not Uutils.GetExtended(LLXEdit.Text, x1) then BadNumEdit := LLXEdit;
  if not Uutils.GetExtended(LLYEdit.Text, y1) then BadNumEdit := LLYEdit;
  if not Uutils.GetExtended(URXEdit.Text, x2) then BadNumEdit := URXEdit;
  if not Uutils.GetExtended(URYEdit.Text, y2) then BadNumEdit := URYEdit;

  // Display an error message for the control and reset the focus to it.
  if BadNumEdit <> nil then
  begin
    Uutils.MsgDlg(MSG_BLANK_FIELD, mtError, [mbOK]);
    BadNumEdit.SetFocus;
    Exit;
  end;

  // Check that the X and Y ranges are not 0.
  if (x1 = x2) or (y1 = y2) then
  begin
    Uutils.MsgDlg(MSG_ILLEGAL_DIMENSIONS, mtError, [mbOK]);
    LLXEdit.SetFocus;
    Exit;
  end;

  // Set the dimensions of the Map's Backdrop image
  with MapForm.Map.Backdrop do
  begin
    LowerLeft.X := x1;
    LowerLeft.Y := y1;
    UpperRight.X := x2;
    UpperRight.Y := y2;
    OVmapForm.OVmap.Backdrop.LowerLeft := LowerLeft;
    OVmapForm.OVmap.Backdrop.UpperRight := UpperRight;
  end;

  // Scale the Map to the Backdrop if called for
  if ScaleMapBtn.Checked then ScaleMapToBackdrop;

  // Redraw the Map on the MapForm at full extent
  Hide;
  MapForm.DrawFullExtent;
  Uglobals.HasChanged := True;
  Close;
end;

procedure TBackdropDimensionsForm.CancelBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the CancelBtn.
//-----------------------------------------------------------------------------
begin
  ModalResult := mrCancel;
  Close;
end;

procedure TBackdropDimensionsForm.ResizeOnlyBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the Resize Backdrop Only radio button. Displays
//  the map's dimensions in the Map panel and the backdrop's diemsnions
//  in the Backdrop edit boxes. Allows the user to edit dimensions for
//  the Backdrop.
//-----------------------------------------------------------------------------
var
  D: Integer;
begin
  D := MapForm.Map.Dimensions.Digits;
  DisplayMapDimensions(MapLL, MapUR, D);
  DisplayBackdropDimensions(BackLL, BackUR, D);
  EnableEditFields(True);
end;

procedure TBackdropDimensionsForm.ScaleBackdropBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the Scale Backdrop to Map radio button. Displays
//  the map's dimensions in both the Map panels and the Backdrop edit
//  boxes. Does not allow the user to edit the Backdrop's dimensions.
//-----------------------------------------------------------------------------
var
  D: Integer;
begin
  D := MapForm.Map.Dimensions.Digits;
  DisplayMapDimensions(MapLL, MapUR, D);
  DisplayBackdropDimensions(MapLL, MapUR, D);
  EnableEditFields(False);
end;

procedure TBackdropDimensionsForm.ScaleMapBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the Scale Map to Backdrop radio button. Displays
//  the Backdrop's dimensions in the Map panels. Allows the user to edit
//  the Backdrop's dimensions.
//-----------------------------------------------------------------------------
var
  D: Integer;
begin
  D := MapForm.Map.Dimensions.Digits;
  DisplayBackdropDimensions(BackLL, BackUR, D);
  MapLLX.Text := LLXEdit.Text;
  MapLLY.Text := LLYEdit.Text;
  MapURX.Text := URXEdit.Text;
  MapURY.Text := URYEdit.Text;
  EnableEditFields(True);
end;

procedure TBackdropDimensionsForm.LLXEditChange(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnChange handler for all of the Backdrop edit boxes. Sets the map
//  dimension displayed in the Map panel to that shown in the Backdrop
//  edit box when the Scale Map to Backdrop button is selected.
//-----------------------------------------------------------------------------
begin
  if ScaleMapBtn.Checked then
  begin
    if Sender = LLXEdit then MapLLX.Text := LLXEdit.Text
    else if Sender = LLYEdit then MapLLY.Text := LLYEdit.Text
    else if Sender = URXEdit then MapURX.Text := URXEdit.Text
    else if Sender = URYEdit then MapURY.Text := URYEdit.Text;
  end;
end;

procedure TBackdropDimensionsForm.EnableEditFields(Enable: Boolean);
//-----------------------------------------------------------------------------
//  Enables/disables the Backdrop dimension edit boxes.
//-----------------------------------------------------------------------------
begin
  LLXEdit.Enabled := Enable;
  LLYEdit.Enabled := Enable;
  URXEdit.Enabled := Enable;
  URYEdit.Enabled := Enable;
end;

procedure TBackdropDimensionsForm.SetData;
//-----------------------------------------------------------------------------
//  Saves original Map and Backdrop dimensions and displays them on the form.
//-----------------------------------------------------------------------------
var
  D: Integer;
begin
  // Save original map & backdrop dimensions
  with MapForm.Map.Dimensions do
  begin
    MapLL := LowerLeft;
    MapUR := UpperRight;
  end;
  with MapForm.Map.Backdrop do
  begin
    BackLL := LowerLeft;
    BackUR := UpperRight;
  end;

  // Display original map dimensions
  D := MapForm.Map.Dimensions.Digits;
  DisplayMapDimensions(MapLL, MapUR, D);
  DisplayBackdropDimensions(BackLL, BackUR, D);
end;

procedure TBackdropDimensionsForm.DisplayMapDimensions(
          LL, UR: TExtendedPoint; D: Integer);
//-----------------------------------------------------------------------------
//  Displays Map dimensions on the form's Map dimension panels.
//-----------------------------------------------------------------------------
begin
  MapLLX.Text := FloatToStrF(LL.X, ffFixed, 18, D);
  MapLLY.Text := FloatToStrF(LL.Y, ffFixed, 18, D);
  MapURX.Text := FloatToStrF(UR.X, ffFixed, 18, D);
  MapURY.Text := FloatToStrF(UR.Y, ffFixed, 18, D);
end;

procedure TBackdropDimensionsForm.DisplayBackdropDimensions(
          LL, UR: TExtendedPoint; D: Integer);
//-----------------------------------------------------------------------------
//  Displays dimensions in the form's Backdrop dimensions edit boxes.
//-----------------------------------------------------------------------------
begin
  LLXEdit.Text := FloatToStrF(LL.X, ffFixed, 18, D);
  LLYEdit.Text := FloatToStrF(LL.Y, ffFixed, 18, D);
  URXEdit.Text := FloatToStrF(UR.X, ffFixed, 18, D);
  URYEdit.Text := FloatToStrF(UR.Y, ffFixed, 18, D);
end;

procedure TBackdropDimensionsForm.ScaleMapToBackdrop;
//-----------------------------------------------------------------------------
//  Transforms the coordinates of all map objects so that they scale
//  proportionately to the dimensions of the Backdrop image.
//-----------------------------------------------------------------------------
begin
  with MapForm.Map do
  begin
    Ucoords.TransformCoords(Dimensions.LowerLeft, Dimensions.UpperRight,
                            Backdrop.LowerLeft, Backdrop.UpperRight);
    Dimensions.LowerLeft  := Backdrop.LowerLeft;
    Dimensions.UpperRight := Backdrop.UpperRight;
  end;
end;

procedure TBackdropDimensionsForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 212960);
end;

procedure TBackdropDimensionsForm.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_F1 then HelpBtnClick(Sender);
end;

end.
