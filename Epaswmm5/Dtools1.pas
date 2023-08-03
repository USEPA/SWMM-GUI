unit Dtools1;

{-------------------------------------------------------------------}
{                    Unit:    Dtools1.pas                           }
{                    Project: EPA SMM                               }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Delphi Pascal dialog unit that allows user to update the list   }
{   of add-on tools made available through the Tools item on the    }
{   Main Menu.                                                      }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TToolOptionsForm = class(TForm)
    ToolsListBox: TListBox;
    AddBtn: TButton;
    DeleteBtn: TButton;
    EditBtn: TButton;
    MoveUpBtn: TBitBtn;
    MoveDownBtn: TBitBtn;
    Label1: TLabel;
    CloseBtn: TButton;
    HelpBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure AddBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure EditBtnClick(Sender: TObject);
    procedure MoveUpBtnClick(Sender: TObject);
    procedure MoveDownBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
  private
    { Private declarations }
    procedure EnableBtns;
  public
    { Public declarations }
  end;

//var
//  ToolOptionsForm: TToolOptionsForm;

implementation

{$R *.dfm}

uses
  Fmain, DTools2, Utools, Uglobals;

procedure TToolOptionsForm.EnableBtns;
var
  Show: Boolean;
begin
  if ToolsListBox.Items.Count = 0
  then Show := False
  else Show := True;
  EditBtn.Enabled := Show;
  DeleteBtn.Enabled := Show;
  MoveUpBtn.Enabled := Show;
  MoveDownBtn.Enabled := Show;
end;

procedure TToolOptionsForm.FormCreate(Sender: TObject);
begin
  ToolsListBox.Items.AddStrings(Utools.ToolList);
  if ToolsListBox.Items.Count > 0
  then ToolsListBox.ItemIndex := 0;
  with MainForm.ProjectImageList do
  begin
    GetBitmap(GetIndexByName('uparrow1'), MoveUpBtn.Glyph);
    GetBitmap(GetIndexByName('dnarrow1'), MoveDownBtn.Glyph);
  end;
  EnableBtns;
end;

procedure TToolOptionsForm.AddBtnClick(Sender: TObject);
var
  ToolPropertiesForm: TToolPropertiesForm;
begin
  ToolPropertiesForm := TToolPropertiesForm.Create(self);
  with ToolPropertiesForm do
  try
    LoadTool(-1);
    if ShowModal = mrOK then
    begin
      ToolsListBox.Items.Add(GetToolName);
      ToolsListBox.ItemIndex := ToolsListBox.Items.Count-1;
    end;
  finally
    Free;
  end;
  EnableBtns;
end;

procedure TToolOptionsForm.DeleteBtnClick(Sender: TObject);
var
  I: Integer;
begin
  I := ToolsListBox.ItemIndex;
  Utools.DeleteTool(I);
  ToolsListBox.DeleteSelected;
  if I >= ToolsListBox.Items.Count then Dec(I);
  if I >= 0 then ToolsListBox.ItemIndex := I;
  EnableBtns;
end;

procedure TToolOptionsForm.EditBtnClick(Sender: TObject);
var
  I: Integer;
  ToolPropertiesForm: TToolPropertiesForm;
begin
  I := ToolsListBox.ItemIndex;
  ToolPropertiesForm := TToolPropertiesForm.Create(self);
  with ToolPropertiesForm do
  try
    LoadTool(I);
    if ShowModal = mrOK then
    begin
      ToolsListBox.Items[I] := GetToolName;
    end;
  finally
    Free;
  end;
end;

procedure TToolOptionsForm.MoveUpBtnClick(Sender: TObject);
var
  I: Integer;
begin
  I := ToolsListBox.ItemIndex;
  if I > 0 then
  begin
    Utools.ExchangeTools(I, I-1);
    ToolsListBox.Items.Clear;
    ToolsListBox.Items.AddStrings(Utools.ToolList);
    ToolsListBox.ItemIndex := I-1;
  end;
end;

procedure TToolOptionsForm.MoveDownBtnClick(Sender: TObject);
var
  I: Integer;
begin
  I := ToolsListBox.ItemIndex;
  if I < Utools.ToolList.Count-1 then
  begin
    Utools.ExchangeTools(I, I+1);
    ToolsListBox.Items.Clear;
    ToolsListBox.Items.AddStrings(Utools.ToolList);
    ToolsListBox.ItemIndex := I+1;
  end;
end;

procedure TToolOptionsForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then HelpBtnClick(Sender);
end;

procedure TToolOptionsForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 213160);
end;

end.
