unit Dpattern;

{-------------------------------------------------------------------}
{                    Unit:    Dpattern.pas                          }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form unit that edits a time pattern.                     }
{                                                                   }
{   The form utilizes a GridEdit frame to edit the time pattern's   }
{   multiplier values.                                              }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TeEngine, Series, ExtCtrls, TeeProcs, Chart, Grids,
  NumEdit, Uproject, Uglobals, Uutils, GridEdit, Buttons;

type
  TPatternForm = class(TForm)
    NameEdit: TNumEdit;
    Label1: TLabel;
    CommentEdit: TEdit;
    Label2: TLabel;
    PatternTypeCombo: TComboBox;
    Label3: TLabel;
    OKBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    Label4: TLabel;
    GridEdit1: TGridEditFrame;
    EditBtn: TBitBtn;
    procedure PatternTypeComboClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure EditBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure NameEditKeyPress(Sender: TObject; var Key: Char);

  private
    { Private declarations }
    PatternIndex: Integer;
    function ValidateData: Boolean;
  public
    { Public declarations }
    procedure SetData(const Index: Integer; aPattern: TPattern);
    procedure GetData(var S: String; aPattern: TPattern);
  end;

//var
//  PatternForm: TPatternForm;

implementation

{$R *.dfm}

uses
  Fmain, Uedit;

const
  TXT_DESCRIPTION = 'Time Pattern Description';
  MSG_INVALID_NAME = 'Invalid pattern name.';
  MSG_DUPLICATE_NAME = 'Duplicate pattern name.';
  MSG_INVALID_VALUE = 'Invalid multiplier value.';

  MonthlyLabels: array[0..11] of String =
    ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
     'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');

  DailyLabels: array[0..6] of String =
    ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');

  HourlyLabels: array[0..11] of String =
    ('12 ', '1 ', '2 ', '3 ', '4 ', '5 ',
     '6 ', '7 ', '8 ', '9 ', '10 ', '11 ');

  AM: string = 'AM';
  PM: string = 'PM';

procedure TPatternForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnCreate handler.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  // Load names of pattern types into combo box
  for I := 0 to High(PatternTypes) do
    PatternTypeCombo.Items.Add(PatternTypes[I]);

  // Make the grid fill its container's width
  with GridEdit1.Grid do
  begin
    DefaultRowHeight := GridEdit1.EditBox.Height;
    ColWidths[1] := ClientWidth - DefaultColWidth - 1;
  end;

  with MainForm.ProjectImageList do
    GetBitmap(GetIndexByName('edit'), EditBtn.Glyph);
end;

procedure TPatternForm.PatternTypeComboClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the time pattern type combo box. Modifies the size
//  and labels on the GridEdit control according to the type of pattern
//  selected.
//-----------------------------------------------------------------------------
var
  I: Integer;
  S: String;
begin
  case PatternTypeCombo.ItemIndex of
    PATTERN_MONTHLY:
      begin
        with GridEdit1.Grid do
        begin
          RowCount := 12;
          for I := 0 to 11 do
            Cells[0,I] := MonthlyLabels[I];
        end;
      end;
    PATTERN_DAILY:
      begin
        with GridEdit1.Grid do
        begin
          RowCount := 7;
          for I := 0 to 6 do
            Cells[0,I] := DailyLabels[I];
        end;
      end;
    else
      begin
        with GridEdit1.Grid do
        begin
          RowCount := 24;
          for I := 0 to 23 do
          begin
            if I < 12 then S := HourlyLabels[I] + AM
            else S := HourlyLabels[I-12] + PM;
            Cells[0,I] := S;
          end;
        end;
      end;
  end;
  with GridEdit1.Grid do
  begin
    for I := 0 to RowCount-1 do
      if Length(Cells[1,I]) = 0 then Cells[1,I] := '1.0';
  end;
end;

procedure TPatternForm.EditBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the Description edit button. Launches the Notes
//  Editor to edit a descriptive comment.
//-----------------------------------------------------------------------------
var
  S: String;
  Modified: Boolean;
begin
  S := CommentEdit.Text;
  Uedit.EditComment(TXT_DESCRIPTION, S, Modified);
  if Modified then CommentEdit.Text := S;
end;

procedure TPatternForm.OKBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the OK button.
//-----------------------------------------------------------------------------
begin
  if not ValidateData then ModalResult := mrNone
  else ModalResult := mrOK;
end;

procedure TPatternForm.SetData(const Index: Integer; aPattern: TPattern);
//-----------------------------------------------------------------------------
//  Loads data for a given time pattern into the form.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  PatternIndex := Index;
  if Index >= 0 then NameEdit.Text := Project.Lists[PATTERN].Strings[Index];
  PatternTypeCombo.ItemIndex := aPattern.PatternType;
  CommentEdit.Text := aPattern.Comment;
  PatternTypeComboClick(self);
  with GridEdit1.Grid do
    for I := 0 to RowCount-1 do
      Cells[1,I] := aPattern.Data[I];
end;

procedure TPatternForm.GetData(var S: String; aPattern: TPattern);
//-----------------------------------------------------------------------------
//  Unloads data from the form into a specific time pattern.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  S := NameEdit.Text;
  aPattern.PatternType := PatternTypeCombo.ItemIndex;
  aPattern.Comment := CommentEdit.Text;
  aPattern.Count := GridEdit1.Grid.RowCount;
  with GridEdit1.Grid do
    for I := 0 to RowCount-1 do
      aPattern.Data[I] := Cells[1,I];
end;

function TPatternForm.ValidateData: Boolean;
//-----------------------------------------------------------------------------
//  Validates the data entries in the form.
//-----------------------------------------------------------------------------
var
  S : String;
  I : Integer;
  X : Single;
begin
  Result := False;
  S := Trim(NameEdit.Text);
  if Length(S) = 0 then
  begin
    Uutils.MsgDlg(MSG_INVALID_NAME, mtError, [mbOK]);
    NameEdit.SetFocus;
    Exit;
  end;
  I := Project.Lists[PATTERN].IndexOf(S);
  if (I >= 0) and (I <> PatternIndex) then
  begin
    Uutils.MsgDlg(MSG_DUPLICATE_NAME, mtError, [mbOK]);
    NameEdit.SetFocus;
    Exit;
  end;
  with GridEdit1.Grid do
    for I := 0 to RowCount-1 do
      if not Uutils.GetSingle(Cells[1,I], X) then
      begin
        Uutils.MsgDlg(MSG_INVALID_VALUE, mtError, [mbOK]);
        GridEdit1.SetFocus;
        Exit;
      end;
  Result := True;
end;

procedure TPatternForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 211350);
end;

procedure TPatternForm.NameEditKeyPress(Sender: TObject; var Key: Char);
begin
  with NameEdit as TNumEdit do
  begin
    if (Length(Text) = 0) or (SelStart = 0) then
      if Key = '[' then Key := #0;
  end;
end;

procedure TPatternForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then HelpBtnClick(Sender);
end;

end.
