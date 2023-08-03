unit Dbackdrp;

{-------------------------------------------------------------------}
{                    Unit:    Dbackdrp.pas                          }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21      (5.2.0)                 }
{                    Author:  L. Rossman                            }
{                                                                   }
{  Dialog form unit that allows the user to select an image to use  }
{  as a backdrop picture for the study area map.                    }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, FileCtrl, Math, Uglobals, Uproject, Uutils;

type
  TBackdropFileForm = class(TForm)
    Label1: TLabel;
    ImageFileEdit: TEdit;
    ImageFileBtn: TBitBtn;
    Label2: TLabel;
    WorldFileEdit: TEdit;
    WorldFileBtn: TBitBtn;
    OKBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    ScaleMapCheckBox: TCheckBox;
    WorldFileDelBtn: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure ImageFileBtnClick(Sender: TObject);
    procedure WorldFileBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure WorldFileEditChange(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure WorldFileDelBtnClick(Sender: TObject);
  private
    { Private declarations }
    XtopLeft  : Extended;              // top left X coordinate
    YtopLeft  : Extended;              // top left Y coordinate
    XbotRight : Extended;              // bottom right X coordinate
    YbotRight : Extended;              // bottom right Y coordinate
    Xwpp      : Extended;              // world X distance per pixel
    Ywpp      : Extended;              // world Y distance per pixel
    Pwidth    : Integer;               // image width in pixels
    Pheight   : Integer;               // image height in pixels
    Scaling   : Integer;               // type of image scaling to use
    FileDir   : String;                // name of image file directory
    ImageFile : String;
    WorldFile : String;
    function  ReadImageFile(const Fname: String): Boolean;
    function  ReadWorldFile(const Fname: String): Boolean;
    procedure ScaleBackdrop;
    procedure ScaleMapToBackdrop;
    function  SetScaling: Boolean;
  public
    { Public declarations }
    function  GetBackdropFileName: String;
    procedure GetBackdropCoords(var LowerLeft, UpperRight: TExtendedPoint);
  end;

var
  BackdropFileForm: TBackdropFileForm;

implementation

{$R *.dfm}

uses
  Fmain, Fmap, Ucoords;

const
  TXT_FILE_DLG_TITLE = 'Select a World File';
  TXT_FILE_DLG_FILTER = 'World files (*.*w)|*.*w|All files|*.*';
  MSG_NO_IMAGE_FILE  = 'Cannot find image file ';
  MSG_BAD_IMAGE_FILE = 'Cannot read image file ';
  MSG_NO_WORLD_FILE  = 'Cannot find world file ';
  MSG_BAD_WORLD_FILE = 'Invalid world file ';
  MSG_NO_OVERLAP     =
  'The position of the backdrop image falls outside the map''s boundaries.' + #10 +
  'Should the backdrop image be shifted and re-scaled to fit within the map?';
  SCALE_TO_NOTHING   = 0;
  SCALE_TO_WINDOW    = 1;
  SCALE_TO_MAP       = 2;
  SCALE_TO_BACKDROP  = 3;

procedure TBackdropFileForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnCreate handler.
//-----------------------------------------------------------------------------
begin
  Scaling := SCALE_TO_WINDOW;
  FileDir := ProjectDir;
  ImageFile := '';
  WorldFile := '';
  with MainForm.ProjectImageList do
  begin
    GetBitmap(GetIndexByName('browse'), ImageFileBtn.Glyph);
    GetBitmap(GetIndexByName('browse'), WorldFileBtn.Glyph);
    GetBitmap(GetIndexByName('delete'), WorldFileDelBtn.Glyph);
  end;
end;

procedure TBackdropFileForm.ImageFileBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the ImageFileBtn BitButton. Uses the MainForm's
//  OpenPictureDialog to select a backdrop image file.
//-----------------------------------------------------------------------------
begin
  with MainForm.OpenPictureDialog do
  begin
    InitialDir := FileDir;
    if Execute then
    begin
      ImageFile := Filename;
      ImageFileEdit.Text := MinimizeName(Filename, self.Canvas, ImageFileEdit.ClientWidth);
      FileDir := ExtractFileDir(Filename);
    end;
  end;
  ImageFileBtn.SetFocus;
end;

procedure TBackdropFileForm.WorldFileBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the WorldFileBtn BitButton. Uses the MainForm's
//  OpenTextFileDialog to select a World file for the backdrop image.
//-----------------------------------------------------------------------------
begin
  with MainForm.OpenTextFileDialog do
  begin
    Title := TXT_FILE_DLG_TITLE;
    Filter := TXT_FILE_DLG_FILTER;
    Filename := '*.*w';
    InitialDir := FileDir;
    if Execute then
    begin
      WorldFile := Filename;
      WorldFileEdit.Text := MinimizeName(Filename, self.Canvas, WorldFileEdit.ClientWidth);
      FileDir := ExtractFileDir(Filename);
    end;
  end;
  WorldFileBtn.SetFocus;
end;

procedure TBackdropFileForm.WorldFileDelBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the WorldFileDelBtn BitButton.
//-----------------------------------------------------------------------------
begin
  WorldFile := '';
  WorldFileEdit.Clear;
  WorldFileBtn.SetFocus;
end;

procedure TBackdropFileForm.WorldFileEditChange(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnChange handler for the WorldFileEdit edit box. Enables the Scale Map
//  to Backdrop CheckBox if a World file is specified.
//-----------------------------------------------------------------------------
begin
  ScaleMapCheckBox.Enabled := (Length(Trim(WorldFileEdit.Text)) > 0);
end;

procedure TBackdropFileForm.OKBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the OKBtn. Processes the entries made in the form.
//-----------------------------------------------------------------------------
begin
  if not ReadImageFile(ImageFile)
  then ImageFileBtn.SetFocus
  else if not ReadWorldFile(WorldFile)
  then WorldFileBtn.SetFocus
  else begin
    if not SetScaling then Exit;
    Hide;
    ModalResult := mrOK;
  end;
end;

function TBackdropFileForm.GetBackdropFileName: String;
//-----------------------------------------------------------------------------
//  Public function that returns the name of the backdrop image file.
//  Called from the MainForm when processing the View >> Backdrop >>
//  Load menu selection.
//-----------------------------------------------------------------------------
begin
  Result := ImageFile;
end;

function TBackdropFileForm.ReadImageFile(const Fname: String): Boolean;
//-----------------------------------------------------------------------------
//  Determines pixel width and height of backdrop image file.
//-----------------------------------------------------------------------------
var
  Picture: TPicture;
begin
  XtopLeft  := 0.0;
  YtopLeft  := 0.0;
  XbotRight := 0.0;
  YbotRight := 0.0;
  Xwpp      := MapForm.Map.Window.WperP;
  Ywpp      := MapForm.Map.Window.WperP;
  Result    := False;
  if not FileExists(Fname) then
  begin
    Uutils.MsgDlg(MSG_NO_IMAGE_FILE + Fname, mtWarning, [mbOK]);
    Exit;
  end;
  Picture := TPicture.Create;
  try
    Picture.LoadFromFile(Fname);
    Pwidth := Picture.Width;
    Pheight := Picture.Height;
    Result := True;
  finally
    Picture.Free;
  end;
  if Result = False
  then Uutils.MsgDlg(MSG_BAD_IMAGE_FILE + Fname, mtWarning, [mbOK]);
end;

function TBackdropFileForm.ReadWorldFile(const Fname: String): Boolean;
//-----------------------------------------------------------------------------
//  Reads the contents of a "world" file for the backdrop image.
//-----------------------------------------------------------------------------
var
  F: TextFile;
  S: String;
  I: Integer;
  X: array[0..5] of Extended;
begin
  // Check that world file exists
  I := 0;
  Result := True;
  if Length(Trim(Fname)) = 0 then Exit;
  Result := False;
  if not FileExists(Fname) then
  begin
    Uutils.MsgDlg(MSG_NO_WORLD_FILE + Fname, mtWarning, [mbOK]);
    Exit;
  end;

  // Try to open the file
  AssignFile(F, Fname);
  try
    {$I-}
    Reset(F);
    {$I+}
    if (IOResult = 0) then
    begin
      // Read 6 lines of data from the file
      while not Eof(F) and (I < 6) do
      begin
        Readln(F, S);
        if Uutils.GetExtended(S, X[I]) then Inc(I);
      end;
    end;
  finally
    CloseFile(F);
  end;

  // Check for valid data
  if (I < 6) or (X[0] = 0.0) or (X[3] = 0.0) then
  begin
    Uutils.MsgDlg(MSG_BAD_WORLD_FILE + Fname, mtWarning, [mbOK]);
    Exit;
  end;

  // Save each data value
  Xwpp      := X[0];                        // world x per pixel
  Ywpp      := -X[3];                       // world y per pixel
  XtopLeft  := X[4];                        // world x of top left
  YtopLeft  := X[5];                        // world y of top left
  XbotRight := XtopLeft + Xwpp*Pwidth;
  YbotRight := YtopLeft - Ywpp*Pheight;
  Result := True;
end;

procedure TBackdropFileForm.GetBackdropCoords(var LowerLeft, UpperRight:
  TExtendedPoint);
//-----------------------------------------------------------------------------
//  Determines coordinates of lower left and upper right corners of the
//  backdrop image.
//-----------------------------------------------------------------------------
begin
  case Scaling of
  SCALE_TO_WINDOW:   ScaleBackdrop;
  SCALE_TO_MAP:      ScaleBackdrop;
  SCALE_TO_BACKDROP: ScaleMapToBackdrop;
  end;
  LowerLeft.X  := XtopLeft;
  LowerLeft.Y  := YbotRight;
  UpperRight.X := XbotRight;
  UpperRight.Y := YtopLeft;
end;

function TBackdropFileForm.SetScaling: Boolean;
//-----------------------------------------------------------------------------
//  Determines how the backdrop image and the study area map should be
//  scaled relative to one another.
//-----------------------------------------------------------------------------
begin
  // Default is to scale backdrop to map window extent
  Result := True;
  Scaling := SCALE_TO_WINDOW;

  // Check if world coordinates were supplied for backdrop image
  if XtopLeft <> XbotRight then
  begin

    // Default is no scaling
    Scaling := SCALE_TO_NOTHING;

    // Scale map to backdrop if check box checked
    if ScaleMapCheckBox.Checked then Scaling := SCALE_TO_BACKDROP
    else begin

      // Check if there is no overlap between map and backdrop
      with MapForm.Map.Dimensions do
      begin
        if (XtopLeft  > UpperRight.X)
        or (XbotRight < LowerLeft.X)
        or (YtopLeft  < LowerLeft.Y)
        or (YbotRight > UpperRight.Y) then

        // See if user wants to scale backdrop to map
          case Uutils.MsgDlg(MSG_NO_OVERLAP, mtConfirmation,
                          [mbYes, mbNo, mbCancel]) of
          mrYES: Scaling := SCALE_TO_MAP;
          mrCANCEL: Result := False;
          end;
      end;
    end;
  end;
end;

procedure TBackdropFileForm.ScaleMapToBackdrop;
//-----------------------------------------------------------------------------
//  Scales the study area map so that it fits within the backdrop image.
//-----------------------------------------------------------------------------
var
  LL2    : TExtendedPoint;             // Lower left corner coords.
  UR2    : TExtendedPoint;             // Upper right corner coords.
begin
  with MapForm.Map do
  begin

    // Determine lower left and upper right coords. of backdrop
    LL2.X := XtopLeft;
    LL2.Y := YbotRight;
    UR2.X := LL2.X + Pwidth * Xwpp;
    UR2.Y := LL2.Y + Pheight * Ywpp;

    // Transform object coords. & map dimensions to those of backdrop
    with Dimensions do
    begin
      Ucoords.TransformCoords(LowerLeft, UpperRight, LL2, UR2);
      LowerLeft  := LL2;
      UpperRight := UR2;
    end;

    // Rescale the map
    Rescale;
  end;
end;

procedure TBackdropFileForm.ScaleBackdrop;
//-----------------------------------------------------------------------------
//  Scales the backdrop image to fit either within the display window
//  or within the extents of the study area map.
//-----------------------------------------------------------------------------
var
  Wback  : Extended;
  Hback  : Extended;
  W      : Extended;
  H      : Extended;
  R      : Extended;
  Rx     : Extended;
  Ry     : Extended;
begin
  with MapForm.Map do
  begin

    // Compute backdrop width & height in world coords.
    Wback := Pwidth * Sfactor;
    Hback := Pheight * Sfactor;

    // If scaling to window, compute width & height of window
    if Scaling = SCALE_TO_WINDOW then
    begin
      W := Window.Pwidth * Sfactor;
      H := Window.Pheight * Sfactor;
    end

    // Otherwise find width & height of map
    else with Dimensions do
    begin
      W := UpperRight.X - LowerLeft.X;
      H := UpperRight.Y - LowerLeft.Y;
    end;

    // Expand picture, if necessary, to fill display extent without distortion
    Rx := W / Wback;
    Ry := H / Hback;
    R := Min(Rx, Ry);
    if R > 1.0 then
    begin
      Wback := R * Wback;
      Hback := R * Hback;
    end;

    // Reduce picture if it's wider or taller than display extent
    if Wback > W then
    begin
      R := W / Wback;
      Wback := R * Wback;
      Hback := R * Hback;
    end;
    if Hback > H then
    begin
      R := H / Hback;
      Wback := R * Wback;
      Hback := R * Hback;
    end;

    // Save corner coords. of centered backdrop image
    XtopLeft := ZoomState[0].Xcenter - Wback/2.0;
    YtopLeft := ZoomState[0].Ycenter + Hback/2.0;
    XbotRight := XtopLeft + Wback;
    YbotRight := YtopLeft - Hback;

    // Re-dimension map to include backdrop image's extent
    with Dimensions do
    begin
      LowerLeft.X := Min(LowerLeft.X, XtopLeft);
      LowerLeft.Y := Min(LowerLeft.Y, YbotRight);
      UpperRight.X := Max(UpperRight.X, XbotRight);
      UpperRight.Y := Max(UpperRight.Y, YtopLeft);
    end;
    Rescale;
  end;
end;

procedure TBackdropFileForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 212970);
end;

procedure TBackdropFileForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then HelpBtnClick(Sender);
end;

end.
