unit Uutils;

{-------------------------------------------------------------------}
{                    Unit:    Uutils.pas                            }
{                    Project: EPA SMM                               }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Delphi Pascal unit containing general purpose utility           }
{   routines.                                                       }
{-------------------------------------------------------------------}

interface

uses
  SysUtils, Windows, Graphics, Math, Classes, Consts, Dialogs, Forms,
  StrUtils, Grids, System.UITypes;

const
  MAXSIZE = 536870910;  // = largest Integer / SizeOf(Integer) - 1

  TheSeparator = '@';   // Separator used for SortStringGrid

type
  // This is used by the CompareStrVals function
  TRelationType   = (rtBelow, rtEquals, rtAbove);

  // Bitmap adjustment types
  TAdjustmentType = (atWatermark, atGrayscale);
  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array[WORD] of TRGBTriple;

  // These are used for dynamically allocated arrays.
  TByteArray    = Array[0..MAXSIZE] of Byte;
  PByteArray    = ^TByteArray;
  TIntegerArray = Array[0..MAXSIZE] of Integer;
  PIntegerArray = ^TIntegerArray;
  TLongintArray = Array[0..MAXSIZE] of Longint;
  PLongintArray = ^TLongintArray;
  TSingleArray  = Array[0..MAXSIZE] of Single;
  PSingleArray  = ^TSingleArray;
  TPointArray   = Array[0..16000] of TPoint;
  PPointArray   = ^TPointArray;

procedure AdjustBitmap(var aBitmap: TBitmap; AdjustmentType: TAdjustmentType);
procedure AutoScale(var Zmin: Double; var Zmax: Double; var T: Double);

procedure Cls(Canvas: TCanvas; aRect: TRect; aColor: TColor);
function  CompareSingles(a, b: Pointer): Integer;
function  CompareStrVals(const S1,S2: String; const R: TRelationType): Boolean;
function  ConvertDate(S: String): String;
procedure CopyStringArray(const Source: array of String;
                          var Dest: array of String);
procedure CopyStringList(List1, List2: TStrings);

procedure DrawTextCentered(C: TCanvas; R: TRect; const S: String);

function  FileDateTime(const FileName: string): TDateTime;
function  FindKeyWord(const S: String; const Words: array of PChar;
          const N: Integer): Integer;
procedure FitFrameToPage(aRatio: Single; const PageWidth: Single;
          const PageHeight: Single; var FrameWidth: Single;
          var FrameHeight: Single);

function  GetAppDataDir(const AppName: String; const AppDir: String): String;
function  GetCoreCount(): Integer;
function  GetDecimalChar:Char;
function  GetExtended(const S: String; var X: Extended): Boolean;
function  GetFileSize(const FileName: string): Int64;
function  GetHomeDir: String;
function  GetLocaleMeasurements: Integer;
function  GetProcessorCount(): Integer;
function  GetSingle(const S: String; var X: Single): Boolean;
function  GetTempFile(const Folder, Prefix: String): String;
function  GetTempFolder: String;
function  GetTimeString(const Seconds: Longint): String;
function  GetWindowsDir: String;

function  HasAttr(const FileName: string; Attr: Word): Boolean;
function  InRect(const X: Integer; const Y: Integer;
          const aRect: TRect): Boolean;
function  InvertColor(C: TColor): TColor;
function  IsReadOnly(const DirName: String): Boolean;
function  IsValidNumber(const Txt: String; var V: Single): Boolean;

procedure LatLongToMeters(var X, Y: Extended);
function  LightenColor(aColor: TColor): TColor;

function  MsgDlg(const Msg: string; DlgType: TMsgDlgType;
          Buttons: TMsgDlgButtons): Integer; overload;
function  MsgDlg(const Msg: string; DlgType: TMsgDlgType;
          Buttons: TMsgDlgButtons; F: TForm): Integer; overload;

function  PtOnLine(const P1: TPoint; const P2: TPoint;
          const P: TPoint; const Ptol: Integer): Boolean;

function  RoundToScale(X: Extended): Extended;

procedure SortStringGrid(var GenStrGrid: TStringGrid; ThatCol: Integer;
          Numeric: Boolean; Direction: Integer);

function  StrHoursToTime(S: String): TDateTime;
procedure Swap(var x: Integer; var y: Integer);

procedure TextOutRotate(Canvas: TCanvas; X, Y: Integer; S: String;
                        fAngle: Longint);
procedure Tokenize(S: String; T: TStringlist; var N: Integer);

function  WinExecAndWait(CmdLine: String; WorkDir: String; ShowCmd: Integer;
                         Wait: Boolean): Integer;

implementation

const
  SInvalidNum = ' is not a valid number.';

var
  SortDirection: Integer;


procedure AdjustBitmap(var aBitmap: TBitmap; AdjustmentType: TAdjustmentType);
//-----------------------------------------------------------------------------
//  Brightens backdrop bitmap to create a watermark effect.
//-----------------------------------------------------------------------------
const
  BRIGHTNESS = 70;           // relative brightness setting
var
  Row : PRGBTripleArray;     // ptr. to array of R,G,B color values
  V   : Integer;             // new R, G, or B color value
  X, Y: Integer;             // pixel row & column
  newBitmap: TBitmap;        // copy of original bitmap
begin
  // Create a copy of the original bitmap
  newBitmap := TBitmap.Create;
  try
    newBitmap.PixelFormat := pf24Bit;  // need this format for ScanLine function
    newBitmap.Assign(aBitmap);

    // Process each pixel of new bitmap
    with newBitmap do
    begin
      for Y := 0 to Height-1 do
      begin
        Row := ScanLine[Y];            // fast access to rows of pixels
        for X := 0 to Width-1 do
        begin
          with Row[X] do
          begin
            case AdjustmentType of

              atWatermark:
              begin
                V := rgbtRed;
                V := V + (BRIGHTNESS*(255 - V) div 100);
                rgbtRed := Min(255, V);
                V := rgbtBlue;
                V := V + (BRIGHTNESS*(255 - V) div 100);
                rgbtBlue := Min(255, V);
                V := rgbtGreen;
                V := V + (BRIGHTNESS*(255 - V) div 100);
                rgbtGreen := Min(255, V);
              end;

              atGrayscale:
              begin
                V := Round(0.2125 * rgbtRed + 0.7154 * rgbtGreen + 0.0721 * rgbtBlue);
                rgbtRed := V;
                rgbtBlue := V;
                rgbtGreen := V;
              end;
            end;
          end;
        end;
      end;
    end;

    // Transfer adjusted bitmap to original bitmap
    aBitmap.Canvas.Draw(0, 0, newBitmap);

  // Free the bitmap copy
  finally
    newBitmap.Free;
  end;


end;

procedure AutoScale(var Zmin: Double; var Zmax: Double; var T: Double);
//-----------------------------------------------------------------------------
//  Scales the range between Zmin and Zmax at intervals of T.
//-----------------------------------------------------------------------------
var
  m        : Integer;
  z        : Longint;
  d, z1, z2: Double;
begin
  z1 := Zmin;
  z2 := Zmax;
  try
    d := Abs(Zmax-Zmin);
    if (d = 0.0) and (Zmin = 0.0) then
    begin
      Zmin := -1.0;
      Zmax := 1.0;
      T := 1.0;
      Exit;
    end
    else if d < 0.01 then
    begin
      Zmin := Zmin - 0.5*Abs(Zmin);
      Zmax := Zmax + 0.5*Abs(Zmax);
    end;
    d := Abs(Zmax - Zmin);
    m := Trunc(Ln(d)/Ln(10.0));
    T := IntPower(10.,m);
    if T > 0.5*d then T := 0.2*T
    else if T > 0.2*d then T := 0.5*T;
    z := Trunc(Zmax/T) + 1;
    Zmax := z*T;
    z := Trunc(Zmin/T);
    if Zmin < 0 then z := z - 1;
    Zmin := z*T;
    if Zmin = Zmax then Zmax := Zmin + T;
    if Abs(Zmin-Zmax)/T > 10.0 then T := 2.0*T;
  except
    Zmin := z1;
    Zmax := z2;
    T := z2 - z1;
  end;
end;


function RoundToScale(X: Extended): Extended;
//-----------------------------------------------------------------------------
//  Rounds X down to a nice scaling factor (e.g., RoundToScale(879) = 800).
//-----------------------------------------------------------------------------
const
  ScaleValues: array[0..4] of Extended = (2, 4, 5, 8, 10);
var
  OrderOfMagnitude: Integer;
  Exponent: Extended;
  R: Extended;
  I: Integer;
begin
  if X <= 0 then Result := 1
  else
  begin
    OrderOfMagnitude := Floor(Ln(X)/Ln(10));
    Exponent := IntPower(10, OrderOfMagnitude);
    R := X/Exponent;
    for I := 0 to High(ScaleValues) do
    begin
      if R <= ScaleValues[I] then
      begin
        Result := ScaleValues[I]*Exponent;
        Exit;
      end;
    end;
    Result := Exponent;
  end;
end;


procedure Cls(Canvas: TCanvas; aRect: TRect; aColor: TColor);
//-----------------------------------------------------------------------------
//  Fills the rectangle aRect with the color aColor on the canvas Canvas.
//-----------------------------------------------------------------------------
var
  oldbrushstyle : TBrushStyle;
  oldbrushcolor : TColor;
begin
  with Canvas do
  begin
    oldbrushcolor := Brush.Color;
    oldbrushstyle := Brush.Style;
    Brush.Color := aColor;
    Brush.Style := bsSolid;
    FillRect(aRect);
    Brush.Style := oldbrushstyle;
    Brush.Color := oldbrushcolor;
  end;
end;


function CompareSingles(a, b: Pointer): Integer;
//-----------------------------------------------------------------------------
//  Compares values of pointers to two variables of type Single.
//-----------------------------------------------------------------------------
begin
  if (Single(a^) < Single(b^)) then Result := -1
  else if (Single(a^) > Single(b^)) then Result := 1
  else Result := 0;
end;


function CompareStrVals(const S1,S2: String; const R: TRelationType): Boolean;
//-----------------------------------------------------------------------------
//  Compares numerical values contained in strings S1 & S2 with respect
//  to the relational operator R. If neither S1 or S2 are numbers then
//  a case-sensitive string comparison is made.
//-----------------------------------------------------------------------------
var
  v1, v2: Single;
  c: Integer;
begin
  Result := False;
  if (Uutils.GetSingle(S1,v1) and Uutils.GetSingle(S2,v2)) then
  begin
    case R of
      rtBelow: if (v1 >= v2) then exit;
      rtEquals: if (v1 <> v2) then exit;
      rtAbove: if (v1 <= v2) then exit;
    end;
    Result := True;
  end
  else
  begin
    c := CompareStr(S1, S2);
    case R of
      rtBelow:  if (c < 0) then Result := True;
      rtEquals: if (c = 0) then Result := True;
      rtAbove:  if (c > 0) then Result := True;
    end;
  end;
end;


function ConvertDate(S: String): String;
//-----------------------------------------------------------------------------
//  Converts alternate types of date separators found in a date string
//  into the date separator used by EPA SWMM.
//-----------------------------------------------------------------------------
const
  Months: array[1..12] of PChar =
  ('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN',
   'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC');
var
  I, M: Integer;
  S1, S2: String;
begin
  // Check for a date with format '4-JUL-1947'
  I := Pos('-', S);
  if I > 0 then
  begin
    S1 := Copy(S, 1, I-1);               //the day number
    S2 := Copy(S, I+1, 3);               //the month text
    M := FindKeyword(S2, Months, 3) + 1; //the month number
    if M > 0 then                        //date as mm/dd-yyyy
      S := IntToStr(M) +
        '/' + S1 + Copy(S, I+4, Length(S));
  end;

  // Examine each character of date string
  for I := 1 to Length(S) do
  begin
    // Check if character is not part of a number nor a time separator
    if not CharInSet(S[I], [' ', '.', '0'..'9'])
    and (S[I] <> ':')
    then S[I] := '/';
  end;
  Result := S;
end;


procedure CopyStringArray(const Source: array of String;
  var Dest: array of String);
//-----------------------------------------------------------------------------
//  Copies an array of strings from Source to the array Dest.
//-----------------------------------------------------------------------------
var
  i: Integer;
  first, last: Integer;
begin
  first := MaxIntValue([Low(Source),Low(Dest)]);
  last :=  MinIntValue([High(Source),High(Dest)]);
  for i := first to last do Dest[i] := Source[i];
end;


procedure CopyStringList(List1, List2: TStrings);
//-----------------------------------------------------------------------------
//  Copies the text in StringList List1 to stringlist List2.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  List2.Clear;
  for I := 0 to List1.Count-1 do List2.Add(List1[I]);
end;


procedure DrawTextCentered(C: TCanvas; R: TRect; const S: String);
//-----------------------------------------------------------------------------
//  Draws text S centered in rectangle R on canvas C.
//-----------------------------------------------------------------------------
var
  left, top: Integer;
begin
  with C do
  begin
    top := R.Top + (R.Bottom - R.Top - TextExtent(S).cy) div 2;
    left := R.Left + (R.Right - R.Left - TextExtent(S).cx) div 2;
    TextOut(left, top, S);
  end;
end;


function FileDateTime(const FileName: string): System.TDateTime;
//-----------------------------------------------------------------------------
//  Returns the date/time stamp of file FileName as a TDateTime value.
//-----------------------------------------------------------------------------
begin
  SysUtils.FileAge(FileName, Result);
end;


function FindKeyWord(const S: String; const Words: array of PChar;
  const N: Integer): Integer;
//-----------------------------------------------------------------------------
//  Sees if the first N characters of any of the keywords in the
//  array Words matches the string S. Returns the index of the keyword
//  if a match is found or -1 for no match. Not case sensitive.
//-----------------------------------------------------------------------------
var
  k : Integer;
  s1, s2: String;
begin
  s2 := UpperCase(S);
  for k := 0 to High(Words) do
  begin
    s1 := UpperCase(Copy(Words[k], 1, N));
    if Pos(s1, s2) = 1 then // > 0 then
    begin
      Result := k;
      Exit;
    end;
  end;
  Result := -1;
end;


procedure FitFrameToPage(aRatio: Single; const PageWidth: Single;
  const PageHeight: Single; var FrameWidth: Single; var FrameHeight: Single);
//-----------------------------------------------------------------------------
//  Fits a frame with aspect ratio aRatio to the printed page.
//  PageWidth and PageHeight are the width and height of the printable area
//  of the page and FrameWidth and FrameHeight are returned as the width
//  and height of the frame on the printed page.
//-----------------------------------------------------------------------------
var
  h,w: Single;
begin
  if aRatio <= 0.0 then aRatio := 1.0;
  w := PageWidth;
  h := w/aRatio;
  if h > PageHeight then
  begin
    h := PageHeight;
    w := h * aRatio;
  end;
  if w > PageWidth then
  begin
    w := PageWidth;
    h := w/aRatio;
  end;
  FrameWidth := w;
  FrameHeight := h;
end;


function  GetAppDataDir(const AppName: String; const AppDir: String): String;
//-----------------------------------------------------------------------------
//  Returns a directory in a user's space where data files for
//  an application named AppName can be safely saved.
//-----------------------------------------------------------------------------
var
  S: String;
begin
  // Determine system's Application Data directory
  S := GetEnvironmentVariable('APPDATA');

  // If it doesn't exist, then get the user's home directory
  if Length(S) = 0 then S := GetEnvironmentVariable('USERPROFILE');

  // If we found the app or home directory, then create an AppName
  // directory under it
  if Length(S) > 0 then
  begin
    S := S + '\' + AppName;
    if not DirectoryExists(S) then
      if not CreateDir(S) then S := '';
  end;

  // If we still don't have a directory, then use the directory
  // where the application is installed
  if Length(S) = 0 then S := AppDir;

  // Append a backslash to the directory name
  Result := S + '\';
end;


function  GetCoreCount(): Integer;
//-----------------------------------------------------------------------------
//  Retrieves the number of physical CPU cores.
//  (Deprecated by Delphi's System.CPUCount property.)
//-----------------------------------------------------------------------------
var
  i: Integer;
  ReturnLength: DWORD;
  Buffer: array of TSystemLogicalProcessorInformation;
begin
  Result := 0;
  SetLength(Buffer, 256);
  if not GetLogicalProcessorInformation(@Buffer[0], ReturnLength) then
  begin
    if GetLastError = ERROR_INSUFFICIENT_BUFFER then
    begin
      SetLength(Buffer,
        ReturnLength div SizeOf(TSystemLogicalProcessorInformation) + 1);
      if not GetLogicalProcessorInformation(@Buffer[0], ReturnLength) then exit;
    end;
  end;
  SetLength(Buffer, ReturnLength div SizeOf(TSystemLogicalProcessorInformation));
  for i := 0 to High(Buffer) do begin
    if Buffer[i].Relationship = RelationProcessorCore then Inc(Result);
  end;
end;


function GetProcessorCount(): Integer;
//-----------------------------------------------------------------------------
//  Retrieves the number of processing units.
//-----------------------------------------------------------------------------
var
  MySystem:  _SYSTEM_INFO;
begin
  try
    GetSystemInfo(MySystem);
    Result := MySystem.dwNumberOfProcessors;
  finally
  end;
  if Result = 0 then Result := 1;
end;

function GetDecimalChar:Char;
//-----------------------------------------------------------------------------
//  Retrieves the International decimal point setting in use by Windows.
//-----------------------------------------------------------------------------
var
  Buffer: PChar;
begin
  Buffer := StrAlloc(2);
  GetLocaleInfo(GetSystemDefaultLCID, LOCALE_SDECIMAL, Buffer, 2);
  Result := Buffer[0];
  StrDispose(Buffer);
end;


function  GetExtended(const S: String; var X: Extended): Boolean;
//-----------------------------------------------------------------------------
//  Converts the string S to an extended float X. Returns True if  S is a
//  valid number and False if not.
//-----------------------------------------------------------------------------
begin
  X := 0;
  Result := True;
  try
    X := StrToFloat(S);
  except
    On EConvertError do Result := False;
  end;
end;


function GetFileSize(const FileName: string): Int64;
//-----------------------------------------------------------------------------
//  Returns the size of the named file without opening the file.
//  If the file doesn't exist, returns -1.
//-----------------------------------------------------------------------------
var
  Fhandle: Integer;
  Fname: String;
begin
  Result := -1;
  Fhandle := -1;
  try
    Fname := ExpandFileName(FileName);
    Fhandle := FileOpen(Fname, fmOpenRead or fmShareDenyNone);
    if Fhandle < 0 then Exit;
    Result := FileSeek(Fhandle, 0, 2);
  finally
    if Fhandle >= 0 then FileClose(Fhandle);
  end;
end;
{
var
  SearchRec: TSearchRec;
begin
  if FindFirst(ExpandFileName(FileName), faAnyFile, SearchRec) = 0
  then Result := SearchRec.Size
  else Result := -1;
end;
}

function GetHomeDir: String;
//-----------------------------------------------------------------------------
//  Returns user's home directory with a '\' appended to it.
//-----------------------------------------------------------------------------
var
  Str : String;
begin
  Str := GetEnvironmentVariable('USERPROFILE');
  if Length(Str) > 0 then Result := Str + '\'
  else Result := '';
end;


function GetLocaleMeasurements: Integer;
//-----------------------------------------------------------------------------
//  Gets units of measurement in use by Windows.
//  0 = Metric (Millimeters), 1 = US (inches)
//-----------------------------------------------------------------------------
var
  Buffer: PChar;
begin
  Buffer := StrAlloc(2);
  GetLocaleInfo(GetUserDefaultLCID, LOCALE_IMEASURE, Buffer, 2);
  if Buffer[0] = '0' then
    Result := 0
  else
    Result := 1;
  StrDispose(Buffer);
end;


function  GetSingle(const S: String; var X: Single): Boolean;
//-----------------------------------------------------------------------------
//  Converts the string S to a Single float X. Returns True if S is a valid
//  number and False if its not.
//-----------------------------------------------------------------------------
begin
  X := 0;
  Result := True;
  try
    X := StrToFloat(S);
  except
    On EConvertError do Result := False;
  end;
end;


function GetTempFile(const Folder, Prefix: String): String;
//-----------------------------------------------------------------------------
//  Returns a unique filename guaranteed not to be in use. Folder = name of
//  folder where file will be created (use '.' for current folder). Prefix =
//  string whose first 3 characters form beginning of the filename.
//-----------------------------------------------------------------------------
var
  zFileName: array[0..MAX_PATH] of char;
  zFolder: array[0..MAX_PATH] of char;
  zPrefix: array[0..3] of char;
begin
  FillChar(zFileName, SizeOf(zFileName), #0);
  StrPLCopy(zFolder, Folder, MAX_PATH);
  StrPLCopy(zPrefix, Prefix, 3);
  if GetTempFileName(zFolder, zPrefix, 0, zFilename) <> 0
  then Result := zFileName
  else Result := String('');
end;


function GetTempFolder: String;
//-----------------------------------------------------------------------------
//  Returns the file path Windows has designated for temporary files.
//-----------------------------------------------------------------------------
var
  zTmpFolder: array[0..MAX_PATH] of char;
begin
  FillChar(zTmpFolder, SizeOf(zTmpFolder), #0);
  if GetTempPath(MAX_PATH, zTmpFolder) <> 0
  then Result := zTmpFolder
  else Result := '';
end;


function GetTimeString(const Seconds: Longint): String;
//-----------------------------------------------------------------------------
//  Converts seconds into Hrs:Mins format.
//-----------------------------------------------------------------------------
var
  hours, minutes, secs: Longint;
begin
  hours := Seconds div 3600;
  minutes := (Seconds - (3600*hours)) div 60;
  secs := Seconds - 3600*hours - 60*minutes;
  Result := Format('%d:%.2d:%.2d ',[hours,minutes,secs]);
end;


function GetWindowsDir: String;
//-----------------------------------------------------------------------------
//  Returns the Windows directory with a '\' appended.
//-----------------------------------------------------------------------------
var
  WinDir  : array[0..MAX_PATH] of Char;
begin
  if (GetWindowsDirectory(WinDir, MAX_PATH) > 0) then
  begin
    if WinDir[StrLen(WinDir)-1] <> '\' then StrCat(WinDir,'\');
    Result := String(WinDir);
  end
  else Result := '';
end;


function HasAttr(const FileName: string; Attr: Word): Boolean;
//-----------------------------------------------------------------------------
//  Determines if file FileName has attribute Attr.
//-----------------------------------------------------------------------------
begin
  Result := (FileGetAttr(FileName) and Attr) > 0;
end;


function InRect(const X: Integer; const Y: Integer; const aRect: TRect):
  Boolean;
//-----------------------------------------------------------------------------
//  Determines if the point X,Y falls within the rectangle aRect.
//-----------------------------------------------------------------------------
begin
  with aRect do
  begin
    if (X >= Left) and (X <= Right)
    and (Y >= Top) and (Y <= Bottom)
    then Result := True
    else Result := False;
  end;
end;


function  InvertColor(C: TColor): TColor;
//-----------------------------------------------------------------------------
//  Returns the inverse of color C.
//-----------------------------------------------------------------------------
var
  rgb1, rgb2: LongInt;
  r,g,b: Byte;
begin
  rgb1 := ColorToRGB(C);
  r := GetRValue(rgb1) mod 255;
  g := GetGValue(rgb1) mod 255;
  b := GetBValue(rgb1) mod 255;
  rgb2 := RGB(r,g,b);
  if rgb2 = rgb1 then rgb2 := 0;
  Result := TColor(rgb2);
end;


function  IsReadOnly(const DirName: String): Boolean;
//-----------------------------------------------------------------------------
//  Determines if directory DirName is read-only or not.
//-----------------------------------------------------------------------------
var
  Attributes: Word;
begin
  Attributes := FileGetAttr(DirName);
  Result := (Attributes and SysUtils.faReadOnly) = faReadOnly;
end;


function IsValidNumber(const Txt: String; var V: Single): Boolean;
//-----------------------------------------------------------------------------
//  Checks if the string Txt is a valid number. If so, the number is
//  returned in V otherwise an error message is displayed.
//-----------------------------------------------------------------------------
begin
  if not GetSingle(Txt, V) then
  begin
    MsgDlg('''' + Txt + '''' + SInvalidNum, mtError,[mbOK]);
    Result := False;
  end
  else Result := True;
end;


procedure LatLongToMeters(var X, Y: Extended);
//-----------------------------------------------------------------------------
// Converts longitude (X) and latitude (Y) to Cartesian coordinates in
// meters. Assumes a spherical earth with radius of 6,370,997 m so 1 degree
// = 111194.87 m at the equator.
//-----------------------------------------------------------------------------
begin
  X := 111194.87*X;
  Y := 111194.87*Y*cos(DegToRad(Y));
end;

function  LightenColor(aColor: TColor): TColor;
//-----------------------------------------------------------------------------
// Lightens a given color.
//-----------------------------------------------------------------------------
const
  BRIGHTNESS = 70;           // relative brightness setting
var
  rgbColor: LongInt;
  r, b, g: Integer;
begin
  rgbColor := ColorToRGB(aColor);
  r := GetRValue(rgbColor);
  r := r + (BRIGHTNESS*(255 - r) div 100);
  r := Min(255, r);
  b := GetBValue(rgbColor);
  b := b + (BRIGHTNESS*(255 - b) div 100);
  b := Min(255, b);
  g := GetGValue(rgbColor);
  g := g + (BRIGHTNESS*(255 - g) div 100);
  g := Min(255, g);
  Result := RGB(r, g, b);
  end;

function  MsgDlg(const Msg: string; DlgType: TMsgDlgType;
          Buttons: TMsgDlgButtons): Integer; overload;
//-----------------------------------------------------------------------------
//  Displays a message dialog in center of currently active form.
//-----------------------------------------------------------------------------
begin
  Result :=  MsgDlg(Msg, DlgType, Buttons, Screen.ActiveForm);
end;

function  MsgDlg(const Msg: string; DlgType: TMsgDlgType;
          Buttons: TMsgDlgButtons; F: TForm): Integer; overload;
//-----------------------------------------------------------------------------
//  Displays a message dialog in center of a specific form.
//-----------------------------------------------------------------------------
var
  R: TRect;
begin
  if not Assigned(F) then
  begin
    Result := MessageDlg(Msg, DlgType, Buttons, 0);
  end else
  begin
    with CreateMessageDialog(Msg, DlgType, Buttons) do
    try
      GetWindowRect(F.Handle, R);
      Left := R.Left + ((R.Right - R.Left) div 2) - (Width div 2);
      Top := R.Top + ((R.Bottom - R.Top) div 2) - (Height div 2);
      Result := ShowModal;
    finally
      Free;
    end;
  end;
{
begin
  with CreateMessageDialog(Msg, DlgType, Buttons) do
    try
      Position := poOwnerFormCenter;
      Result := ShowModal;
    finally
      Free
    end
}
end;


function PtOnLine(const P1: TPoint; const P2: TPoint; const P: TPoint;
  const Ptol: Integer): Boolean;
//-----------------------------------------------------------------------------
//  Checks if the point P is on the line between points P1 & P2.
//-----------------------------------------------------------------------------
var
  dx,  dy  : Integer;
  dx1, dy1 : Integer;
  a, b, c  : Integer;
begin
  Result := False;
  dx := P2.X - P1.X;
  dy := P2.Y - P1.Y;
  dx1 := P.X - P1.X;
  dy1 := P.Y - P1.Y;
  if (Abs(dx) > 0) and (Abs(dy) < Abs(dx)) then
  begin
    if (dx*dx1 >= 0) and (Abs(dx1) <= Abs(dx)) then
    begin
      a := (dy*dx1);
      b := (dx*dy1);
      c := Abs(dx*Ptol);
      if Abs(a-b) <= c then Result := True;
    end;
  end
  else if Abs(dy) > 0 then
  begin
    if (dy*dy1 >= 0) and (Abs(dy1) <= Abs(dy)) then
    begin
      a := (dx*dy1);
      b := (dy*dx1);
      c := Abs(dy*Ptol);
      if Abs(a-b) <= c then Result := True;
    end;
  end;
end;


function  StrHoursToTime(S: String): TDateTime;
//-----------------------------------------------------------------------------
//  Converts time in Hours:Mins:Secs to decimal days.
//-----------------------------------------------------------------------------
var
  T: Extended;
  N: Integer;
  Hr: Extended;
  Min: Extended;
  Sec: Extended;
  TS: String;
begin
  try
    // If no time separator then assume that string is a decimal number
    TS := ':';
    N := Pos(TS, S);
    if (N = 0) then
    begin
      if not GetExtended(S, T) then Result := -1
      else Result := T/24.0;
    end

    // Otherwise parse the string into hours, minutes, & seconds
    else
    begin
      Hr := 0;
      Min := 0;
      Sec := 0;
      if (N > 1) then Hr := StrToFloat(Copy(S,1,N-1));
      Delete(S,1,N);
      N := Pos(TS, S);
      if (N = 0) then Min := StrToFloat(S)
      else
      begin
        if (N > 1) then Min := StrToFloat(Copy(S,1,N-1));
        Delete(S,1,N);
        if (Length(S) > 0) then Sec := StrToFloat(S);
      end;
      Result := Hr + (Min)/60 + (Sec)/3600;
      Result := Result/24.;
    end;
  except
    on EConvertError do Result := -1;
  end;
end;


procedure Swap(var X: Integer; var Y: Integer);
//-----------------------------------------------------------------------------
//  Swaps the values of integers X & Y.
//-----------------------------------------------------------------------------
var
  Z : Integer;
begin
  Z := X;
  X := Y;
  Y := Z;
end;


procedure TextOutRotate(Canvas: TCanvas; X, Y: Integer; S: String;
  fAngle: Longint);
//-----------------------------------------------------------------------------
//  Draws the string S at location X,Y on the canvas Canvas rotated
//  fAngle degrees.
//-----------------------------------------------------------------------------
var
  LogRec            : TLOGFONT;
  OldFont, NewFont  : HFONT;
  H, W              : Integer;
  fDegToRad         : Double;
  fCosAngle         : Double;
  fSinAngle         : Double;
begin
  with Canvas do
  begin
    fDegToRad := PI/180.0;
    fCosAngle := cos(fAngle*fDegToRad);
    fSinAngle := sin(fAngle*fDegToRad);
    {Brush.Style := bsClear;}
    GetObject(Font.Handle, SizeOf(LogRec), @LogRec);
    LogRec.lfEscapement := fAngle*10;
    LogRec.lfOutPrecision := OUT_TT_ONLY_PRECIS;
    NewFont := CreateFontIndirect(LogRec);
    OldFont := SelectObject(Canvas.Handle,NewFont);
    W := TextWidth(S);
    H := TextHeight(S);
    X := X - trunc(W/2*fCosAngle) - trunc(H/2*fSinAngle);
    Y := Y + trunc(W/2*fSinAngle) - trunc(H/2*fCosAngle);
    TextOut(X, Y, S);
    NewFont := SelectObject(Canvas.Handle,OldFont);
    DeleteObject(NewFont);
  end;
end;


procedure Tokenize(S: String; T: TStringList; var N: Integer);
//-----------------------------------------------------------------------------
//  Converts a string S into a set of N tokens saved in T.
//  Words between " " are stored as a single token. Characters to right
//  of ';' are ignored.
//-----------------------------------------------------------------------------
const
  // Spaces, tabs, & line feeds are used to separate tokens
  separators: TSysCharSet = [' ',Chr(9),Chr(10),Chr(13)];
var
  instring:  Boolean;   // True if currently in a string
  intoken:   Boolean;   // True if currently in a token
  i:         Integer;   // Current position in S
  start:     Integer;   // Start position of item
  len:       Integer;   // Length of item
  c:         Char;      // Current character in S
begin
  // Initialize variables
  T.Clear;
  N := 0;
  intoken := False;
  instring := False;
  start := 1;
  len := 0;

  // Examine each character in S
  i := 1;
  while (i <= Length(S)) do
  begin
    c := S[i];
    if c = ';' then break;     // a comment follows

    if  CharInSet(c, separators)
    and (not instring) then    // a separator has been found
    begin
      if intoken then          // finish the current token
      begin
        T.Add(Copy(S,start,len));
        intoken := False;
        Inc(N);
      end;
    end

    else if c = Chr(34) then   // a quote has been found
    begin
      if intoken then          // finish current token
      begin
        T.Add(Copy(S,start,len));
        intoken := False;
        Inc(N);
      end;
      if instring then         // finish the current string
      begin
        T.Add(Copy(S,start,len));
        instring := False;
        Inc(N);
      end
      else                     // start new string
      begin
        instring := True;
        start := i+1;
        len := 0;
      end;
    end

    else                       // neither a separator or a quote was found
    begin
      if (not intoken)         // if no current token or string
      and (not instring) then  // then start new token
      begin
        intoken := True;
        start := i;
        len := 1;
      end
      else Inc(len);           // otherwise add on to the current one
    end;

    Inc(i);                    // move to the next character
  end;

  if (intoken)
  or (instring) then           // finish the last token
  begin
    T.Add(Copy(S,start,len));
    Inc(N);
  end;
end;


function WinExecAndWait(CmdLine: String; WorkDir: String; ShowCmd: Integer;
   Wait: Boolean): Integer;
//-----------------------------------------------------------------------------
//  Executes the program command given by CmdLine.
//  The window state is given by ShowCmd.
//  If Wait is true then the calling program is halted until the called
//  program ends.
//-----------------------------------------------------------------------------
var
  zCmdLine: array[0..512] of char;
  zCurDir: array[0..255] of char;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  ExitCode: DWORD;
begin
  Result := 0;
  StrPCopy(zCmdLine, CmdLine);
  if Length(WorkDir) = 0 then WorkDir := GetCurrentDir;
  StrPCopy(zCurDir,WorkDir);
  FillChar(StartupInfo,SizeOf(StartupInfo),#0);
  StartupInfo.cb := Sizeof(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := ShowCmd;
  if not CreateProcess(nil,
    zCmdLine,
    nil,
    nil,
    False,
    CREATE_NEW_CONSOLE or
    NORMAL_PRIORITY_CLASS,
    nil,
    zCurDir,
    StartupInfo,
    ProcessInfo) then Result := -1;
  if Wait = true then
  begin
    WaitForSingleObject(ProcessInfo.hProcess,INFINITE);
    GetExitCodeProcess(ProcessInfo.hProcess,ExitCode);
    Result := ExitCode;
  end;
end;

function CompareNumbers(List: TStringList; Index1, Index2: Integer): Integer;
//-----------------------------------------------------------------------------
//  Numerical comparison function for the SortStringGrid procedure.
//-----------------------------------------------------------------------------
var
  S1, S2: String;
  V1, V2: Extended;
begin
  S1 := List[Index1];
  S1 := AnsiLeftStr(S1, AnsiPos(TheSeparator, S1)-1);
  S2 := List[Index2];
  S2 := AnsiLeftStr(S2, AnsiPos(TheSeparator, S2)-1);
  V1 := StrToFloatDef(S1, 0);
  V2 := StrToFloatDef(S2, 0);
  if V1 < V2 then Result := -1
  else if V1 > V2 then Result := 1
  else Result := 0;
  Result := Result * SortDirection;
end;

procedure SortStringGrid(var GenStrGrid: TStringGrid; ThatCol: Integer;
  Numeric: Boolean; Direction: Integer);
//-----------------------------------------------------------------------------
//  Sorts the contents of a StringGrid by the entries in ThatCol column
//  which can be numbers (if Numeric is true) in direction given by
//  Direction (+1 = ascending, -1 = decending).
//-----------------------------------------------------------------------------
var
  CountItem, I, J, K, ThePosition: integer;
  MyList: TStringList;
  MyString, TempString: string;
begin
  // Give the number of rows in the StringGrid
  CountItem := GenStrGrid.RowCount;
  SortDirection := Direction;

  //Create the List
  MyList        := TStringList.Create;
  MyList.Sorted := False;
  try
    begin
      for I := 1 to (CountItem - 1) do
        MyList.Add(GenStrGrid.Rows[I].Strings[ThatCol] + TheSeparator +
          GenStrGrid.Rows[I].Text);

      //Sort the List
      if Numeric then Mylist.CustomSort(@CompareNumbers)
      else Mylist.Sort;

      for K := 1 to Mylist.Count do
      begin
        //Take the String of the line (K – 1)
        MyString := MyList.Strings[(K - 1)];
        //Find the position of the Separator in the String
        ThePosition := Pos(TheSeparator, MyString);
        TempString  := '';
        {Eliminate the Text of the column on which we have sorted the StringGrid}
        TempString := Copy(MyString, (ThePosition + 1), Length(MyString));
        MyList.Strings[(K - 1)] := '';
        MyList.Strings[(K - 1)] := TempString;
      end;

      // Refill the StringGrid
      for J := 1 to (CountItem - 1) do
        GenStrGrid.Rows[J].Text := MyList.Strings[(J - 1)];
    end;
  finally
    //Free the List
    MyList.Free;
  end;
end;


end.
