unit Dtools2;

{-------------------------------------------------------------------}
{                    Unit:    Dtools2.pas                           }
{                    Project: EPA SMM                               }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Delphi Pascal dialog unit that edits the properties of an       }
{   add-on tool made available through the Tools item on the        }
{   Main Menu.                                                      }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, FileCtrl, Uglobals, Uutils;

type
  TToolPropertiesForm = class(TForm)
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    ToolNameEdit: TEdit;
    ToolExeEdit: TEdit;
    ToolDirEdit: TEdit;
    ToolParamsEdit: TEdit;
    FindProgBtn: TBitBtn;
    FindDirBtn: TBitBtn;
    OKBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    DisableSWMMCheckBox: TCheckBox;
    MacrosListBox: TListBox;
    Label1: TLabel;
    OpenDialog1: TOpenDialog;
    AddMacroBtn1: TBitBtn;
    AddMacroBtn2: TBitBtn;
    UpdateSWMMCheckBox: TCheckBox;

    procedure OKBtnClick(Sender: TObject);
    procedure AddMacroBtn1Click(Sender: TObject);
    procedure FindDirBtnClick(Sender: TObject);
    procedure FindProgBtnClick(Sender: TObject);
    procedure ToolExeEditChange(Sender: TObject);
    procedure AddMacroBtn2Click(Sender: TObject);
    procedure DisableSWMMCheckBoxClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MacrosListBoxClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);

  private
    { Private declarations }
    ToolIndex: Integer;

  public
    { Public declarations }
    function  GetToolName: String;
    procedure LoadTool(I: Integer);
  end;

var
  ToolPropertiesForm: TToolPropertiesForm;

implementation

{$R *.dfm}

uses
  fmain, Utools;

function TToolPropertiesForm.GetToolName: String;
begin
  Result := ToolNameEdit.Text;
end;

procedure TToolPropertiesForm.LoadTool(I: Integer);
var
  Tool: TTool;
begin
  ToolIndex := I;
  if (I >= 0) and (I < ToolList.Count) then
  begin
    Tool := TTool(ToolList.Objects[I]);
    ToolNameEdit.Text := ToolList[I];
    ToolExeEdit.Text  := Tool.ExeName;
    ToolDirEdit.Text  := Tool.DirName;
    ToolParamsEdit.Text := Tool.Params;
    DisableSWMMCheckBox.Checked := Tool.DisableSWMM;
    if DisableSWMMCheckBox.Checked
    then UpdateSWMMCheckBox.Checked := Tool.UpdateSWMM;
  end;
end;

procedure TToolPropertiesForm.MacrosListBoxClick(Sender: TObject);
begin
  AddMacroBtn1.Visible := (MacrosListBox.ItemIndex < 2);
end;

procedure TToolPropertiesForm.OKBtnClick(Sender: TObject);
var
  S1, S2, S3, S4: String;
begin
  ModalResult := mrNone;
  S1 := Trim(ToolNameEdit.Text);
  S2 := Trim(ToolExeEdit.Text);
  S3 := Trim(ToolDirEdit.Text);
  S4 := ToolParamsEdit.Text;
  if Length(S1) = 0 then
  begin
    Uutils.MsgDlg('Tool Name cannot be empty.', mtWarning, [mbOK]);
    ToolNameEdit.SetFocus;
  end
  else if Length(S2) = 0 then
  begin
    Uutils.MsgDlg('Program name cannot be empty.', mtWarning, [mbOK]);
    ToolExeEdit.SetFocus;
  end
  else
  begin
    Utools.UpdateTool(ToolIndex, S1, S2, S3, S4, DisableSWMMCheckBox.Checked,
      UpdateSWMMCheckBox.Checked);
    ModalResult := mrOK;
  end;
end;

procedure TToolPropertiesForm.AddMacroBtn1Click(Sender: TObject);
var
  I: Integer;
begin
  I := MacrosListBox.ItemIndex;
  if I >= 0 then ToolDirEdit.Text := Macros[I];
end;

procedure TToolPropertiesForm.AddMacroBtn2Click(Sender: TObject);
var
  I: Integer;
begin
  I := MacrosListBox.ItemIndex;
  if I >= 0 then
    ToolParamsEdit.Text := ToolParamsEdit.Text + Macros[I] + ' ';
end;

procedure TToolPropertiesForm.DisableSWMMCheckBoxClick(Sender: TObject);
begin
  UpdateSWMMCheckBox.Enabled := DisableSWMMCheckBox.Checked;
  if not DisableSWMMCheckBox.Checked
  then UpdateSWMMCheckBox.Checked := False;

end;

procedure TToolPropertiesForm.FindProgBtnClick(Sender: TObject);
begin
  with OpenDialog1 do
  begin
    Title := 'Find Executable';
    Filter := 'Executable files (*.exe)|*.exe|All files|*.*';
    DefaultExt := 'exe';
    InitialDir := EpaSwmmDir;
    Options := Options + [ofFileMustExist];
    if Execute then ToolExeEdit.Text := Filename;
  end;
end;

procedure TToolPropertiesForm.FindDirBtnClick(Sender: TObject);
var
  D: String;
begin
  D := GetCurrentDir;
  if SelectDirectory(D, [], 0)
  then ToolDirEdit.Text := D;
end;

procedure TToolPropertiesForm.ToolExeEditChange(Sender: TObject);
begin
  with Sender as TEdit do Hint := Text;
end;

procedure TToolPropertiesForm.FormCreate(Sender: TObject);
begin
  with MainForm.ProjectImageList do
  begin
    GetBitmap(GetIndexByName('browse'), FindProgBtn.Glyph);
    GetBitmap(GetIndexByName('browse'), FindDirBtn.Glyph);
    GetBitmap(GetIndexByName('green_plus'), AddMacroBtn1.Glyph);
    GetBitmap(GetIndexByName('green_plus'), AddMacroBtn2.Glyph);
  end;
end;

procedure TToolPropertiesForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then HelpBtnClick(Sender);
end;

procedure TToolPropertiesForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 213170);
end;

end.
