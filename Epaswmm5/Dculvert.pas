unit Dculvert;

{-------------------------------------------------------------------}
{                    Unit:    Dculvert.pas                          }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form used to select a Culvert type code based on its     }
{   material and inlet shape.                                       }
{-------------------------------------------------------------------}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TCulvertSelectorForm = class(TForm)
    TreeView1: TTreeView;
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    CancelBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetSelection(S: String);
    function GetSelection: String;
  end;

//var
//  CulvertSelectorForm: TCulvertSelectorForm;

implementation

{$R *.dfm}

uses
  Fproped, Uutils;

var
  Codes: array[0..76] of String;

procedure TCulvertSelectorForm.Button1Click(Sender: TObject);
begin
  if Length(Edit1.Text) = 0 then
    ModalResult := mrOk
  else if StrToInt(Edit1.Text) > 57 then
      Uutils.MsgDlg('Invalid culvert code.', mtError, [mbOk], self)
  else
    ModalResult := mrOk;
end;

procedure TCulvertSelectorForm.FormCreate(Sender: TObject);
var
  I: Integer;
  K: Integer;
begin
  K := 1;
  for I := 0 to TreeView1.Items.Count-1 do
  begin
    if TreeView1.Items[I].Level = 0 then Codes[I] := ''
    else
    begin
      Codes[I] := IntToStr(K);
      Inc(K);
    end;
  end;
end;

procedure TCulvertSelectorForm.SetSelection(S: String);
var
  I: Integer;
begin
  Edit1.Text := S;
  if Length(S) = 0 then with TreeView1 do
    Selected := Items[0]
  else
  begin
    for I := Low(Codes) to High(Codes) do
    begin
      if SameText(S, Codes[I]) then
      begin
        TreeView1.Selected := TreeView1.Items[I];
        exit;
      end;
    end;
  end;
end;

procedure TCulvertSelectorForm.FormShow(Sender: TObject);
begin
  Left := PropEditForm.Left;
  Top := PropEditForm.Top;
end;

function TCulvertSelectorForm.GetSelection: String;
begin
  Result := Edit1.Text;
end;

procedure TCulvertSelectorForm.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  Edit1.Text := Codes[Node.AbsoluteIndex];
end;

end.
