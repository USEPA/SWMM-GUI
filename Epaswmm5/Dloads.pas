unit Dloads;

{-------------------------------------------------------------------}
{                    Unit:    Dloads.pas                            }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form unit for editing initial pollutant loadings over    }
{   a subcatchment.                                                 }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Uproject, Uglobals, Uutils, StdCtrls, ExtCtrls, PropEdit;

type
  TInitLoadingsForm = class(TForm)
    Panel1: TPanel;
    OKBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    HintPanel: TPanel;
    HintLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    PropEdit1: TPropEdit;
    PropCount: Integer;
    PropList: TStringlist;
    PropRecd: array of TPropRecord;
  public
    { Public declarations }
    HasChanged: Boolean;
    procedure SetData(aSubcatch: TSubcatch);
    procedure GetData(aSubcatch: TSubcatch; var Count: Integer);
  end;

//var
//  InitLoadingsForm: TInitLoadingsForm;

implementation

{$R *.dfm}

const
  TXT_POLLUTANT = 'Pollutant';
  TXT_BUILDUP = 'Initial Buildup';
  TXT_HINT = 'Enter initial buildup of pollutants on subcatchment ';
  TXT_LBS = ' (lbs/ac)';
  TXT_KG = ' (kg/ha)';

procedure TInitLoadingsForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnCreate handler.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  // Create Property Editor
  PropEdit1 := TPropEdit.Create(self);
  with PropEdit1 do
  begin
    Parent := Panel1;
    Align := alClient;
    BorderStyle := bsSingle;
    Ctl3D := False;
    ColHeading1 := TXT_POLLUTANT;
    if Uglobals.UnitSystem = usUS
    then ColHeading2 := TXT_BUILDUP + TXT_LBS
    else ColHeading2 := TXT_BUILDUP + TXT_KG;
    ValueColor := clNavy;
  end;

  // Create Property Record array and Property stringlist
  PropList := TStringlist.Create;
  PropCount := Project.Lists[POLLUTANT].Count;
  SetLength(PropRecd, PropCount);
  for I := 0 to PropCount-1 do
  begin
    PropRecd[I].Name := Project.Lists[POLLUTANT].Strings[I];
    PropRecd[I].Style := esEdit;
    PropRecd[I].Mask := emPosNumber;
    PropRecd[I].Length := 0;
  end;
end;

procedure TInitLoadingsForm.FormShow(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnShow handler.
//-----------------------------------------------------------------------------
begin
  PropEdit1.SetProps(PropRecd, PropList);
  PropEdit1.Edit;
end;

procedure TInitLoadingsForm.FormDestroy(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnDestroy handler.
//-----------------------------------------------------------------------------
begin
  Finalize(PropRecd);
  PropList.Free;
  PropEdit1.Free;
end;

procedure TInitLoadingsForm.SetData(aSubcatch: TSubcatch);
//-----------------------------------------------------------------------------
//  Loads loading data from subcatchment aSubcatch into the form.
//-----------------------------------------------------------------------------
var
  I: Integer;
  S: String;
begin
  HintLabel.Caption := TXT_HINT + aSubcatch.ID;
  for I := 0 to Project.Lists[POLLUTANT].Count-1 do
  begin
    S := Project.Lists[POLLUTANT].Strings[I];
    PropList.Add(aSubcatch.Loadings.Values[S]);
  end;
  HasChanged := False;
end;

procedure TInitLoadingsForm.GetData(aSubcatch: TSubcatch; var Count: Integer);
//-----------------------------------------------------------------------------
//  Retrieves loading data for subcatchment aSubcatch from the form.
//-----------------------------------------------------------------------------
var
  I: Integer;
  X: Single;
  S: String;
begin
  aSubcatch.Loadings.Clear;
  Count := 0;
  for I := 0 to PropCount-1 do
  begin
    S := Trim(PropList[I]);
    if not Uutils.GetSingle(S, X) then X := 0;
    if X > 0 then
    begin
      Inc(Count);
      aSubcatch.Loadings.Add(PropRecd[I].Name + '=' + S);
    end;
  end;
  HasChanged := PropEdit1.Modified;
end;

procedure TInitLoadingsForm.OKBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick hander for the OK button.
//-----------------------------------------------------------------------------
begin
  if (not PropEdit1.IsValid) then
  begin
    PropEdit1.Edit;
    ModalResult := mrNone;
  end
  else ModalResult := mrOK;
end;

procedure TInitLoadingsForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 212910);
end;

procedure TInitLoadingsForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then HelpBtnClick(Sender);
end;

end.
