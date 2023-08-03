unit Dsubland;

{-------------------------------------------------------------------}
{                    Unit:    Dsubland.pas                          }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form unit for editing the distribution of land uses      }
{   within a subcatchment.                                          }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Uproject, Uglobals, Uutils, StdCtrls, Grids, PropEdit, ExtCtrls;

type
  TSubLandUsesForm = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    PropEdit1: TPropEdit;
    PropCount: Integer;
    PropList: TStringlist;
    PropRecd: array of TPropRecord;
    function ValidateInput: Boolean;
  public
    { Public declarations }
    HasChanged: Boolean;
    LanduseCount: Integer;
    procedure SetData(LandUses: TStringlist);
    procedure GetData(LandUses: TStringlist; var Count: Integer);
  end;

//var
//  SubLandUsesForm: TSubLandUsesForm;

implementation

{$R *.DFM}

const
  TXT_CAPTION = 'Land Use Assignment';
  TXT_LANDUSE = 'Land Use';
  TXT_PERCENT = '% of Area';
  MSG_EXCEED100 = 'Percentages exceed 100%.';

procedure TSubLandUsesForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnCreate handler.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  Caption := TXT_CAPTION;

  // Create a Property Editor
  PropEdit1 := TPropEdit.Create(self);
  with PropEdit1 do
  begin
    Parent := Panel1;
    Align := alClient;
    BorderStyle := bsNone;
    ColHeading1 := TXT_LANDUSE;
    ColHeading2 := TXT_PERCENT;
    ValueColor := clNavy;
  end;

  // Create a PropList stringlist to store property values
  PropList := TStringlist.Create;

  // Use Delphi's SetLength function to set the size of the PropRecd array
  // to the number of land uses in the project
  PropCount := Project.Lists[LANDUSE].Count;
  SetLength(PropRecd, PropCount);

  // Fill in the attributes of each property record displayed by the editor
  for I := 0 to PropCount-1 do
  begin
    PropRecd[I].Name := Project.Lists[LANDUSE].Strings[I];
    PropRecd[I].Style := esEdit;
    PropRecd[I].Mask := emPosNumber;
    PropRecd[I].Length := 0;
  end;
end;

procedure TSubLandUsesForm.FormShow(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnShow handler.
//-----------------------------------------------------------------------------
begin
  // Load the land use names and their current allocations into the property
  // editor
  PropEdit1.SetProps(PropRecd, PropList);
  PropEdit1.Edit;
end;

procedure TSubLandUsesForm.FormDestroy(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnDestroy handler.
//-----------------------------------------------------------------------------
begin
  // Use Delphi's Finalize procedure to free the memory allocated for PropRecd
  Finalize(PropRecd);
  PropList.Free;
  PropEdit1.Free;
end;

procedure TSubLandUsesForm.SetData(LandUses: TStringlist);
//-----------------------------------------------------------------------------
//  Loads a set of land use allocations for a subcatchment into the editor.
//  These appear in the stringlist LandUses in landuse=percent format.
//-----------------------------------------------------------------------------
var
  I, J: Integer;
begin
  for I := 0 to Project.Lists[LANDUSE].Count-1 do
  begin
    J := LandUses.IndexOfName(Project.Lists[LANDUSE].Strings[I]);
    if J >= 0 then PropList.Add(LandUses.ValueFromIndex[J])
    else PropList.Add('');
  end;
  HasChanged := False;
end;

procedure TSubLandUsesForm.GetData(LandUses: TStringlist; var Count: Integer);
//-----------------------------------------------------------------------------
//  Retrieves the land use allocation contained in the editor, placing it
//  in the LandUses stringlist, and counts the number of land uses with a
//  non-zero allocation.
//-----------------------------------------------------------------------------
var
  I: Integer;
  P: Single;
  S: String;
begin
  LandUses.Clear;
  Count := 0;
  for I := 0 to PropCount-1 do
  begin
    S := Trim(PropList[I]);
    Uutils.GetSingle(S, P);
    if P > 0.0 then
    begin
      Inc(Count);
      LandUses.Add(PropRecd[I].Name + '=' + S);
    end;
  end;
  HasChanged := PropEdit1.Modified;
end;

procedure TSubLandUsesForm.OKBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the OK button.
//-----------------------------------------------------------------------------
begin
  if (not PropEdit1.IsValid) or (not ValidateInput) then
  begin
    PropEdit1.Edit;
    ModalResult := mrNone;
  end
  else ModalResult := mrOK;
end;

function TSubLandusesForm.ValidateInput: Boolean;
//-----------------------------------------------------------------------------
//  Validates the percentage land use allocations entered into the editor.
//-----------------------------------------------------------------------------
var
  I: Integer;
  P: Single;
  Psum: Single;
  S: String;
begin
  Result := False;
  Psum := 0;
  for I := 0 to PropCount-1 do
  begin
    S := Trim(PropList[I]);
    if Length(S) = 0 then continue;
    Uutils.GetSingle(S, P);
    Psum := Psum + P;
  end;
  if (Psum > 101) then
    Uutils.MsgDlg(MSG_EXCEED100, mtERROR, [mbOK])
  else Result := True;
end;

procedure TSubLandUsesForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 211900);
end;

procedure TSubLandUsesForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then HelpBtnClick(Sender);
end;

end.
