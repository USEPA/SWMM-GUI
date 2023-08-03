unit Dcombine;

{-------------------------------------------------------------------}
{                    Unit:    Dcombine.pas                          }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21     (5.2.0)                  }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form unit that obtains names of two interface files      }
{   to combine together into a third file.                          }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, OpenDlg, Grids, Uglobals;

type
  TFileCombineForm = class(TForm)
    BtnCombine: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    SaveFileDlg: TSaveDialog;
    StringGrid1: TStringGrid;
    SpeedButton1: TSpeedButton;
    Label1: TLabel;
    OpenFileDlg: TOpenTxtFileDialog;
    Label2: TLabel;

    procedure FormCreate(Sender: TObject);
    procedure StringGrid1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure SpeedButton1Click(Sender: TObject);
    procedure BtnCombineClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    Fdir:  String;
  public
    { Public declarations }
  end;

//var
//  FileCombineForm: TFileCombineForm;

implementation

{$R *.dfm}

uses
  Fmain, Ucombine;

const
  TXT_ROW_LABELS = 'Interface File 1'#13+
                   'Interface File 2'#13+
                   'Interface File 3'#13+
                   'Description';
  TXT_FILE_DLG_FILTER = 'Text files (*.TXT)|*.TXT|All files|*.*';
  TXT_SELECT_FILE = 'Select File ';
  TXT_SAVE_FILE = 'Save Combined File to';
  TXT_STAND_BY = 'Combining files, please stand by ...';


procedure TFileCombineForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnCreate handler.
//-----------------------------------------------------------------------------
begin
  // Initialize contents of grid control
  with StringGrid1 do
  begin
    ColWidths[1] := ClientWidth - ColWidths[0];
    ClientHeight := (DefaultRowHeight+1)*(RowCount);
    Cols[0].SetText(TXT_ROW_LABELS);
  end;

  // Initialize file directory and file dialog controls
  Fdir := ProjectDir;
  with OpenFileDlg do
  begin
    Filter := TXT_FILE_DLG_FILTER;
  end;
  with SaveFileDlg do
  begin
    Filter := TXT_FILE_DLG_FILTER;
  end;

  // Initialize Label2
  Label2.Caption := TXT_STAND_BY;
  Label2.Visible := False;

  with MainForm.ProjectImageList do
      GetBitmap(GetIndexByName('browse'), SpeedButton1.Glyph);

end;

procedure TFileCombineForm.StringGrid1KeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
//-----------------------------------------------------------------------------
//  Clicks file Browse button when user presses Shift-Enter in a grid cell.
//-----------------------------------------------------------------------------
begin
  if (Key = VK_RETURN) and (ssCtrl in Shift) then SpeedButton1Click(Sender);
end;

procedure TFileCombineForm.StringGrid1SelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
//-----------------------------------------------------------------------------
//  Enables the file Browse button if the selected grid cell is for a file
//  name.
//-----------------------------------------------------------------------------
begin
  SpeedButton1.Enabled := (ARow < 3);
end;

procedure TFileCombineForm.SpeedButton1Click(Sender: TObject);
//-----------------------------------------------------------------------------
//  Launches a file selection dialog when the Browse button is clicked.
//-----------------------------------------------------------------------------
begin
  with StringGrid1 do
  begin

    // Launch an Open File dialog for the files being combined
    if Row < 2 then with OpenFileDlg do
    begin
      Title := TXT_SELECT_FILE + IntToStr(Row+1);
      Options := Options + [ofFileMustExist];
      DefaultExt := 'txt';
      InitialDir := Fdir;
      FileName := Cells[1,Row];
      if Execute then
      begin
        Cells[1,Row] := FileName;
        Fdir := ExtractFileDir(FileName);
      end;
      DefaultExt := '';
    end

    // Launch a Save File dialog for the file being created
    else if Row = 2 then with SaveFileDlg do
    begin
      Title := TXT_SAVE_FILE;
      DefaultExt := 'txt';
      InitialDir := Fdir;
      FileName := Cells[1,Row];
      if Execute then
      begin
        Cells[1,Row] := FileName;
        Fdir := ExtractFileDir(FileName);
      end;
      DefaultExt := '';
    end;
  end;
end;

procedure TFileCombineForm.BtnCombineClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  Launches the file combine utility when the OK button is clicked.
//-----------------------------------------------------------------------------
var
  I: Integer;
  S: String;
  Fname: array[1..3] of String;
begin
  // Extract the file names & description from the grid control
  with StringGrid1 do
  begin
    for I := 1 to 3 do Fname[I] := Cells[1,I-1];
    S := Cells[1,3];
  end;

  // Pass these on as arguments to the CombineFiles function
  Label2.Visible := True;
  Refresh;
  Screen.Cursor := crHourGlass;
  if UCombine.CombineFiles(Fname[1], Fname[2], Fname[3], S)
  then ModalResult := mrOK;
  Label2.Visible := False;
  Screen.Cursor := crDefault;
end;

procedure TFileCombineForm.BtnHelpClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 212270);
end;

procedure TFileCombineForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then BtnHelpClick(Sender);
end;

end.
