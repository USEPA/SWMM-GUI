unit Dcontrol;

{-------------------------------------------------------------------}
{                    Unit:    Dcontrol.pas                          }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21     (5.2.0)                  }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Form unit with a memo control that edits Rule-Based Controls.   }
{-------------------------------------------------------------------}

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls,
  System.UITypes,
  Uproject, Uglobals;

type
  TControlsForm = class(TForm)
    Memo1: TMemo;
    StatusBar1: TStatusBar;
    Panel1: TPanel;
    Panel2: TPanel;
    BtnOK: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    procedure BtnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    StartRule: String;
    Modified: Boolean;
  end;

//var
//  ControlsForm: TControlsForm;

implementation

{$R *.DFM}

procedure TControlsForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnCreate handler.
//-----------------------------------------------------------------------------
begin
  Memo1.Font.Style := Font.Style;
  Memo1.Lines.Assign(Project.ControlRules);
  Modified := false;
end;

procedure TControlsForm.FormShow(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnShow handler.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  if Length(StartRule) > 0 then
  begin
    I:= Pos(StartRule, Memo1.Text);
    if I > 0 then begin
      Memo1.SelStart := I - 1;
      Memo1.SelLength := Length(StartRule);
      Memo1.SetFocus;
    end;
  end;
end;

procedure TControlsForm.BtnOKClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the OK button.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  if Memo1.Modified then
  begin
    Modified := True;
    Project.ControlRules.Assign(Memo1.Lines);

    // Remove blank lines from end of control rules list
    with Project.ControlRules do
    begin
      for I := Count-1 downto 0 do
        if Length(Strings[I]) > 0 then break else Delete(I);
    end;

  end;
end;

procedure TControlsForm.BtnHelpClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 210740);
end;

procedure TControlsForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then BtnHelpClick(Sender);
end;

end.
