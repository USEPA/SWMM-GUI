unit Dcalib1;

{-------------------------------------------------------------------}
{                    Unit:    Dcalib1.pas                           }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form unit that obtains names of calibration data files   }
{   for different measurement variables.                            }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, ExtCtrls, StdCtrls, FileCtrl, Buttons, Uglobals, OpenDlg;

type
  TCalibDataForm = class(TForm)
    StringGrid1: TStringGrid;
    BtnOK: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    BtnBrowse: TBitBtn;
    BtnEdit: TBitBtn;
    BtnDelete: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure BtnBrowseClick(Sender: TObject);
    procedure BtnEditClick(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure BtnDeleteClick(Sender: TObject);
  private
    { Private declarations }
    FileDir: String;
    FileNames: array[1..High(Uglobals.CalibVariables)] of String;
  public
    { Public declarations }
  end;

//var
//  CalibDataForm: TCalibDataForm;

implementation

{$R *.DFM}

uses
  Fmain;

const
  TXT_PARAMETER = 'Calibration Variable';
  TXT_NAME_OF_FILE = 'Name of Calibration File';
  TXT_SELECT_FILE = 'Select a Calibration File';
  TXT_FILE_FILTER = 'Data files (*.DAT)|*.DAT|All files|*.*';


procedure TCalibDataForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
// OnCreate handler. Loads current calibration file names into the
// form's grid control.
//-----------------------------------------------------------------------------
var
  I: Integer;

begin
// Set default file directory
  FileDir := ProjectDir;

// Initialize the grid control
  with StringGrid1 do
  begin
    RowCount := High(CalibData) + 1;
    ColWidths[1] := ClientWidth - ColWidths[0];
    ClientHeight := (DefaultRowHeight+1)*(RowCount);

    Cells[0,0] := TXT_PARAMETER;
    for I := 1 to RowCount-1 do
    begin
      Cells[0,I] := Uglobals.CalibVariables[I];
    end;

    Cells[1,0] := TXT_NAME_OF_FILE;
    for I := 1 to RowCount-1 do
    begin
      Cells[1,I] := MinimizeName(CalibData[I].FileName, self.Canvas, ColWidths[1]);
      FileNames[I] := CalibData[I].FileName;
    end;
  end;

  with MainForm.ProjectImageList do
  begin
    GetBitmap(GetIndexByName('browse'), BtnBrowse.Glyph);
    GetBitmap(GetIndexByName('edit'), BtnEdit.Glyph);
    GetBitmap(GetIndexByName('delete'), BtnDelete.Glyph);
  end;

end;

procedure TCalibDataForm.BtnBrowseClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for "Browse" button. Uses the MainForm's
// OpenTextFileDialog control to obtain the name of a calibration file.
//-----------------------------------------------------------------------------
var
  InitDir: String;
begin
  with StringGrid1 do InitDir := ExtractFileDir(Cells[Col,Row]);
  if Length(InitDir) = 0 then InitDir := ProjectDir;
  with MainForm.OpenTextFileDialog do
  begin
    Title := TXT_SELECT_FILE;
    Filter := TXT_FILE_FILTER;
    InitialDir := InitDir;
    Filename := '*.dat';
    if Execute then
    begin
      with StringGrid1 do
      begin
        FileNames[Row] := FileName;
        Cells[Col,Row] := MinimizeName(FileName, self.Canvas, ColWidths[1]);
      end;
      FileDir := ExtractFileDir(Filename);
    end;
  end;
  BtnBrowse.SetFocus;
end;

procedure TCalibDataForm.BtnDeleteClick(Sender: TObject);
begin
  with StringGrid1 do
  begin
    Filenames[Row] := '';
    Cells[Col,Row] := '';
  end;
end;

procedure TCalibDataForm.BtnEditClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for "Edit" button. Launches Windows NotePad editor for
// the file in current cell of grid control.
//-----------------------------------------------------------------------------
var
  Fname: String;
  CmdLine: AnsiString;

begin
  with StringGrid1 do Fname := FileNames[Row];
  if Length(Fname) > 0 then
  begin
    CmdLine := AnsiString('Notepad ' + Fname);
    WinExec(PAnsiChar(CmdLine),SW_SHOWNORMAL);
  end;
end;

procedure TCalibDataForm.BtnOKClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for "OK" button. Updates names of project's calibration
// files with the entries in the grid control.
//-----------------------------------------------------------------------------
var
  I  : Integer;
begin
  for I := 1 to StringGrid1.RowCount-1 do
  begin
    if not SameText(CalibData[I].FileName, FileNames[I]) then
    begin
      CalibData[I].FileName := FileNames[I];
      HasChanged := True;
    end;
  end;
end;

procedure TCalibDataForm.BtnHelpClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 209960);
end;

procedure TCalibDataForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then BtnHelpClick(Sender);
end;

end.
