unit Dcopy;

{-------------------------------------------------------------------}
{                    Unit:    Dcopy.pas                             }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/12/22     (5.2.2)                  }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form unit that gets choice of format and destination     }
{   that a view should be copied to.                                }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Uglobals;

type
  TCopyToForm = class(TForm)
    DestGroup: TRadioGroup;
    FormatGroup: TRadioGroup;
    BtnOK: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    procedure BtnOKClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
    DestFileName: String;
    procedure RemoveCopyAsData;
  end;

//var
//  CopyToForm: TCopyToForm;

implementation

{$R *.DFM}

uses FMain;

const
  TXT_COPY = 'Copy ';
  TXT_SAVE_AS = 'Save As';

  FilterTxt: array[0..2] of PChar =
   ('Bitmap files (*.BMP)|*.BMP|All files|*.*',
    'EMF files (*.EMF)|*.EMF|All files|*.*',
    'Text files (*.TXT)|*.TXT|All files|*.*');

  ExtensionTxt: array[0..2] of PChar =
    ('.bmp','.emf','.txt');

procedure TCopyToForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
// OnCreate handler for form.
//-----------------------------------------------------------------------------
var
  s: String;
  n: Integer;
begin
  // Extract the type of view being copied from the view form's caption
  // and use this as the dialog's caption
  s := '';
  with MainForm do
  begin
    s := ActiveMDIChild.Caption;
    n := Pos(' -',s);
    if n > 0 then s := Copy(s, 1, n-1);
  end;
  Caption := TXT_COPY + s;
end;

procedure TCopyToForm.RemoveCopyAsData;
//-----------------------------------------------------------------------------
// Removes Copy As Data option.
//-----------------------------------------------------------------------------
begin
  FormatGroup.Items.Delete(2);
end;

procedure TCopyToForm.BtnOKClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for OK button. Retrieves name of file to copy to.
//-----------------------------------------------------------------------------
var
  Ftype: Integer;
begin
  // Use the MainForm's SaveDialog control to obtain the file name
  DestFileName := '';
  if DestGroup.ItemIndex = 1 then
  with MainForm.SaveDialog do
  begin
    Title := TXT_SAVE_AS;
    Ftype := FormatGroup.ItemIndex;
    Filter := FilterTxt[Ftype];
    InitialDir := ProjectDir;
    DefaultExt := Copy(ExtensionTxt[Ftype],2,3);
    Filename := '*' + ExtensionTxt[Ftype];
    if Execute then
    begin
      DestFileName := Filename;
      ModalResult := mrOK;
    end
    else ModalResult := mrCancel;
    DefaultExt := '';
  end
  else ModalResult := mrOK;
  Hide;
end;


procedure TCopyToForm.BtnCancelClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the Cancel button.
//-----------------------------------------------------------------------------
begin
  ModalResult := mrCancel;
  Hide;
end;


procedure TCopyToForm.BtnHelpClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the Help button.
//-----------------------------------------------------------------------------
begin
  Application.HelpCommand(HELP_CONTEXT, 211260);
end;

procedure TCopyToForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then BtnHelpClick(Sender);
end;

end.
