unit Dnotes;

{-------------------------------------------------------------------}
{                    Unit:    Dnotes.pas                            }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21     (5.2.0)                  }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form unit that edits either a project's Title/Notes      }
{   or the Comment property of an object.                           }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, System.UITypes, Uglobals, ExtCtrls;

type
  TNotesEditorForm = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    CheckHeader: TCheckBox;
    OKBtn: TButton;
    CancelBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure CheckHeaderClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    HasChanged: Boolean;
    procedure SetComment(const Title: String; var S: String);
    procedure GetComment(var S: String);
    procedure SetProjectNotes(Slist: TStringlist);
    procedure GetProjectNotes(Slist: TStringlist);
  end;

//var
//  NotesEditorForm: TNotesEditorForm;

implementation

{$R *.dfm}

procedure TNotesEditorForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnCreate handler.
//-----------------------------------------------------------------------------
begin
  // CheckHeader button only visible when editing project Titel/Notes
  CheckHeader.Visible := False;
end;

procedure TNotesEditorForm.SetComment(const Title: String; var S: String);
//-----------------------------------------------------------------------------
//  Loads a comment string S into the editor, using Title as the form's caption.
//-----------------------------------------------------------------------------
begin
  Caption := Title;
  with Memo1 do
  begin
    Lines.SetText(PChar(S));
    SelStart := 0;
  end;
  HasChanged := False;
end;

procedure TNotesEditorForm.GetComment(var S: String);
//-----------------------------------------------------------------------------
//  Unloads the contents of the editor into the comment string S.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  S := '';
  with Memo1 do
  begin
    if Lines.Count > 0 then
    begin
      S := Lines[0];
      for I := 1 to Lines.Count-1 do
        S := S + #13 + Lines[I];
    end;
    HasChanged := Modified;
  end;
end;

procedure TNotesEditorForm.SetProjectNotes(Slist: TStringlist);
//-----------------------------------------------------------------------------
//  Loads the contents of the stringlist Slist into the editor.
//  Used when editing the project's Title/Notes.
//-----------------------------------------------------------------------------
begin
  CheckHeader.Checked := Uglobals.TitleAsHeader;
  CheckHeader.Visible := True;
  Memo1.Lines.Assign(Slist);
  Memo1.SelStart := 0;
  HasChanged := False;
end;

procedure TNotesEditorForm.GetProjectNotes(Slist: TStringlist);
//-----------------------------------------------------------------------------
//  Unloads the contents of the editor into the stringlist Slist.
//  Used when editing the project's Title/Notes.
//-----------------------------------------------------------------------------
begin
  Uglobals.TitleAsHeader := CheckHeader.Checked;
  Slist.Assign(Memo1.Lines);
end;

procedure TNotesEditorForm.CheckHeaderClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnChange handler for the CheckHeader check box.
//-----------------------------------------------------------------------------
begin
  HasChanged := True;
end;

end.
