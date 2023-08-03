unit Dprofile;

{-------------------------------------------------------------------}
{                    Unit:    Dprofile.pas                          }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form unit for selecting previously saved profile plot    }
{   node lists.                                                     }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, System.UITypes, Uproject, Uglobals;

type
  TProfileSelectionForm = class(TForm)
    ProfilesListBox: TListBox;
    RenameBtn: TButton;
    RemoveBtn: TButton;
    OKBtn: TButton;
    CancelBtn: TButton;
    Label1: TLabel;
    procedure RenameBtnClick(Sender: TObject);
    procedure RemoveBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
  private
    { Private declarations }
    TmpProfiles: TStringList;
    HasChanged: Boolean;
  public
    { Public declarations }
    SelectedProfile: Integer;
  end;

//var
//  ProfileSelectionForm: TProfileSelectionForm;

implementation

{$R *.dfm}

const
  TXT_NEW_NAME = 'New Profile Name';
  MSG_NAME_EXISTS = 'A profile with that name already exists.';

procedure TProfileSelectionForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
// OnCreate handler for form.
//-----------------------------------------------------------------------------
begin
  TmpProfiles := TStringlist.Create;
  HasChanged := False;
end;

procedure TProfileSelectionForm.FormShow(Sender: TObject);
//-----------------------------------------------------------------------------
// OnShow handler for form.
//-----------------------------------------------------------------------------
begin
  ProfilesListBox.Items.Assign(Project.ProfileNames);
  TmpProfiles.Assign(Project.ProfileLinks);
  ProfilesListBox.ItemIndex := 0;
end;

procedure TProfileSelectionForm.FormDestroy(Sender: TObject);
//-----------------------------------------------------------------------------
// OnDestroy handler for form.
//-----------------------------------------------------------------------------
begin
  TmpProfiles.Free;
end;

procedure TProfileSelectionForm.RenameBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the Rename button.
//-----------------------------------------------------------------------------
var
  S: String;
  I, J: Integer;
begin
  I := ProfilesListBox.ItemIndex;
  S := InputBox(TXT_NEW_NAME, '', '');
  J := ProfilesListBox.Items.IndexOf(S);
  if (J >= 0) and (J <> I)
  then MessageDlg(MSG_NAME_EXISTS, mtError, [mbOK], 0)
  else if Length(Trim(S)) > 0 then
  begin
    ProfilesListBox.Items[I] := S;
    HasChanged := True;
  end;
end;

procedure TProfileSelectionForm.RemoveBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the Remove button.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  with ProfilesListBox do
  begin
    I := ItemIndex;
    if ItemIndex >= 0 then
    begin
      Items.Delete(ItemIndex);
      TmpProfiles.Delete(I);
      HasChanged := True;
    end;
    if Items.Count > 0 then
    begin
      if I < Items.Count then
        ItemIndex := I
      else
        ItemIndex := Items.Count-1;
    end
    else
    begin
      RemoveBtn.Enabled := False;
      RenameBtn.Enabled := False;
    end;
  end;
end;

procedure TProfileSelectionForm.OKBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the OK button.
//-----------------------------------------------------------------------------
begin
  Project.ProfileNames.Clear;
  Project.ProfileLinks.Clear;
  Project.ProfileNames.Assign(ProfilesListBox.Items);
  Project.ProfileLinks.Assign(TmpProfiles);
  SelectedProfile := ProfilesListBox.ItemIndex;
  if HasChanged then Uglobals.HasChanged := True;
end;

end.
