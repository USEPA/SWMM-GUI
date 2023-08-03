unit Dmapdim;

{-------------------------------------------------------------------}
{                    Unit:    Dmapdim.pas                           }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21   (5.2.0)                    }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form unit that sets dimensions of the Study Area Map.    }
{-------------------------------------------------------------------}

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, Math,
  Uglobals, Uutils, Umap, NumEdit;

type
  TMapDimensionsForm = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    BtnOK: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    MapUnits: TRadioGroup;
    BtnAuto: TButton;
    URYEdit: TNumEdit;
    URXEdit: TNumEdit;
    LLYEdit: TNumEdit;
    LLXEdit: TNumEdit;
    AutoLengths: TCheckBox;
    procedure BtnOKClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure BtnAutoClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetData(const Dimensions: TMapDimensions);
    procedure GetData(var Dimensions: TMapDimensions);
  end;

//var
//  MapDimensionsForm: TMapDimensionsForm;

implementation

{$R *.DFM}

uses
  Fmap, Ucoords, Uupdate;

const
  MSG_BLANK_FIELD = 'Blank data field not allowed.';
  MSG_ILLEGAL_MAP_LIMITS = 'Lower and upper limits cannot be equal.';

procedure TMapDimensionsForm.SetData(const Dimensions: TMapDimensions);
//-----------------------------------------------------------------------------
// Loads current map dimensions into form.
//-----------------------------------------------------------------------------
begin
  with Dimensions do
  begin
    LLXEdit.Text := FloatToStrF(LowerLeft.X, ffFixed, 18, Digits);
    LLYEdit.Text := FloatToStrF(LowerLeft.Y, ffFixed, 18, Digits);
    URXEdit.Text := FloatToStrF(UpperRight.X, ffFixed, 18, Digits);
    URYEdit.Text := FloatToStrF(UpperRight.Y, ffFixed, 18, Digits);
    MapUnits.ItemIndex := Ord(Units);
  end;
  AutoLengths.Visible := AutoLength;
end;

procedure TMapDimensionsForm.GetData(var Dimensions: TMapDimensions);
//----------------------------------------------------------------------------
// Updates map dimensions with contents of form.
//----------------------------------------------------------------------------
begin
  Uutils.GetExtended(LLXEdit.Text, Dimensions.LowerLeft.X);
  Uutils.GetExtended(LLYEdit.Text, Dimensions.LowerLeft.Y);
  Uutils.GetExtended(URXEdit.Text, Dimensions.UpperRight.X);
  Uutils.GetExtended(URYEdit.Text, Dimensions.UpperRight.Y);
  Dimensions.Units := TMapUnits(MapUnits.ItemIndex);
  Uupdate.UpdateMapUnits;
  if AutoLengths.Checked then Uupdate.UpdateAllLengths;
end;

procedure TMapDimensionsForm.BtnAutoClick(Sender: TObject);
//----------------------------------------------------------------------------
// OnClick handler for Auto Scale button.
//----------------------------------------------------------------------------
var
  Dimensions: TMapDimensions;
begin
  // Set map dimensions to default values
  Dimensions := DefMapDimensions;
  Dimensions.Units := TMapUnits(MapUnits.ItemIndex);
  if Dimensions.Units = muDegrees then Dimensions.Digits := MAXDEGDIGITS
  else Dimensions.Digits := Umap.DefMapDimensions.Digits;
  with Dimensions do
  begin
    // Get coord. extents of objects on map
    Ucoords.GetCoordExtents(LowerLeft.X,  LowerLeft.Y,
                            UpperRight.X, UpperRight.Y);

    // Merge these with coords. of any backdrop image
    with MapForm.Map do
    begin
      if Length(Backdrop.Filename) > 0 then
      begin
        LowerLeft.X  := Min(LowerLeft.X, Backdrop.LowerLeft.X);
        LowerLeft.Y  := Min(LowerLeft.Y, Backdrop.LowerLeft.Y);
        UpperRight.X := Max(UpperRight.X, Backdrop.UpperRight.X);
        UpperRight.Y := Max(UpperRight.Y, Backdrop.UpperRight.Y);
      end;
    end;

    // If no objects or backdrop, then use default dimensions
    if LowerLeft.X > UpperRight.X then
    begin
      LowerLeft := DefMapDimensions.LowerLeft;
      UpperRight := DefMapDimensions.UpperRight;
    end;

    LLXEdit.Text := FloatToStrF(LowerLeft.X, ffFixed, 18, Digits);
    LLYEdit.Text := FloatToStrF(LowerLeft.Y, ffFixed, 18, Digits);
    URXEdit.Text := FloatToStrF(UpperRight.X, ffFixed, 18, Digits);
    URYEdit.Text := FloatToStrF(UpperRight.Y,ffFixed, 18, Digits);
  end;
end;

procedure TMapDimensionsForm.BtnOKClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for OK button. Checks for valid map dimensions.
//-----------------------------------------------------------------------------
var
  x1,x2,y1,y2: Single;
  BadNumEdit: TNumEdit;
begin
  BadNumEdit := nil;
  if not Uutils.GetSingle(LLXEdit.Text, x1) then BadNumEdit := LLXEdit;
  if not Uutils.GetSingle(LLYEdit.Text, y1) then BadNumEdit := LLYEdit;
  if not Uutils.GetSingle(URXEdit.Text, x2) then BadNumEdit := URXEdit;
  if not Uutils.GetSingle(URYEdit.Text, y2) then BadNumEdit := URYEdit;
  if BadNumEdit <> nil then
  begin
    Uutils.MsgDlg(MSG_BLANK_FIELD, mtError, [mbOK]);
    BadNumEdit.SetFocus;
    Exit;
  end;
  if (x1 = x2) or (y1 = y2) then
  begin
    Uutils.MsgDlg(MSG_ILLEGAL_MAP_LIMITS, mtError, [mbOK]);
    LLXEdit.SetFocus;
  end
  else ModalResult := mrOK;
end;

procedure TMapDimensionsForm.BtnHelpClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 211330);
end;

procedure TMapDimensionsForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then BtnHelpClick(Sender);
end;

end.
