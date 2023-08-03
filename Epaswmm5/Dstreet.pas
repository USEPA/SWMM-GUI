unit Dstreet;

{-------------------------------------------------------------------}
{                    Unit:    Dstreet.pas                           }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form used to specify the properties of a Street cross    }
{   section.                                                        }
{-------------------------------------------------------------------}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, NumEdit, Vcl.ExtCtrls, System.ImageList, Vcl.ImgList,
  Vcl.VirtualImageList, Vcl.BaseImageCollection, Vcl.ImageCollection,
  Uutils, Uproject, Uglobals;

type
  TStreetEditorForm = class(TForm)
    Panel1: TPanel;
    Image1: TImage;
    Label5: TLabel;
    NumEdit0: TNumEdit;
    Label2: TLabel;
    NumEdit1: TNumEdit;
    Label3: TLabel;
    NumEdit2: TNumEdit;
    Label6: TLabel;
    NumEdit3: TNumEdit;
    NumEdit5: TNumEdit;
    Label4: TLabel;
    NumEdit4: TNumEdit;
    Label7: TLabel;
    NumEdit7: TNumEdit;
    Label1: TLabel;
    NumEdit8: TNumEdit;
    Label8: TLabel;
    NumEdit9: TNumEdit;
    Label9: TLabel;
    RadioButton2: TRadioButton;
    RadioButton1: TRadioButton;
    Label10: TLabel;
    Label11: TLabel;
    Button3: TButton;
    Button2: TButton;
    Button1: TButton;
    UnitLabel1: TLabel;
    UnitLabel2: TLabel;
    Label12: TLabel;
    UnitLabel4: TLabel;
    UnitLabel3: TLabel;
    Label13: TLabel;
    UnitLabel5: TLabel;
    NameEdit: TNumEdit;
    Label15: TLabel;
    ImageCollection1: TImageCollection;
    VirtualImageList1: TVirtualImageList;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure NameEditChange(Sender: TObject);
    procedure NameEditKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    StreetIndex: Integer;
    RadioButton1Checked: Boolean;
  public
    { Public declarations }
    Modified: Boolean;
    procedure SetData(const I: Integer; const S: String; aStreet: TStreet);
    procedure GetData(var S: String; aStreet: TStreet);
  end;

//var
//  StreetEditorForm: TStreetEditorForm;

implementation

{$R *.dfm}

procedure TStreetEditorForm.Button1Click(Sender: TObject);
var
  I: Integer;
  X: Single;
  S: String;
begin
  S := Trim(NameEdit.Text);
  if (Length(S) = 0) then
  begin
    Uutils.MsgDlg('Name field cannot be left blank.', mtError, [mbOK]);
    NameEdit.SetFocus;
    Exit;
  end;
  I := Project.Lists[STREET].IndexOf(S);
  if (I >= 0) and (I <> StreetIndex) then
  begin
    Uutils.MsgDlg('A Street Section with this name already exists.',
      mtError, [mbOK]);
    NameEdit.SetFocus;
    Exit;
  end;
  for I := 0 to 3 do
    with FindComponent('NumEdit' + IntToStr(I)) as TNumEdit do
    begin
      S := Trim(Text);
      if Length(S) = 0 then
      begin
        Uutils.MsgDlg('Required field cannot be left blank.', mtError, [mbOK]);
        SetFocus;
        Exit;
      end;
      Uutils.GetSingle(S, X);
      if X = 0 then
      begin
        Uutils.MsgDlg('Required value cannot be 0.', mtError, [mbOK]);
        SetFocus;
        Exit;
      end;
    end;
  ModalResult := mrOK;
end;

procedure TStreetEditorForm.FormCreate(Sender: TObject);
var
  LengthUnits: String;
begin
  LengthUnits := 'ft';
  if Uglobals.UnitSystem = usSI then LengthUnits := 'm';
  UnitLabel1.Caption := LengthUnits;
  UnitLabel2.Caption := LengthUnits;
  UnitLabel3.Caption := LengthUnits;
  UnitLabel4.Caption := LengthUnits;
  UnitLabel5.Caption := LengthUnits;
  VirtualImageList1.GetIcon(0, Image1.Picture.Icon);
end;

procedure TStreetEditorForm.SetData(const I: Integer; const S: String;
  aStreet: TStreet);
var
  J: Integer;
begin
  StreetIndex := I;
  NameEdit.Text := S;
  for J := 0 to MAXSTREETPROPS do
  begin
    if J = STREET_SIDES then
    begin
      if SameText(aStreet.Data[STREET_SIDES], '1') then
        RadioButton1.Checked := True;
    end
    else with FindComponent('NumEdit' + IntToStr(J)) as TNumEdit do
      Text := aStreet.Data[J];
  end;
  RadioButton1Checked := RadioButton1.Checked;
  Modified := false;
end;

procedure TStreetEditorForm.GetData(var S: String; aStreet: TStreet);
var
  J: Integer;
begin
  S := NameEdit.Text;
  for J := 0 to MAXSTREETPROPS do
  begin
    if J = STREET_SIDES then
    begin
      if RadioButton1.Checked then
        aStreet.DATA[STREET_SIDES] := '1'
      else
        aStreet.DATA[STREET_SIDES] := '2';
    end
    else with FindComponent('NumEdit' + IntToStr(J)) as TNumEdit do
    begin
      if Length(Trim(Text)) > 0 then aStreet.Data[J] := Trim(Text)
      else aStreet.Data[J] := '0';
    end;
  end;
  if RadioButton1Checked <> RadioButton1.Checked then Modified := true;
  aStreet.SetMaxDepth;
end;

procedure TStreetEditorForm.NameEditChange(Sender: TObject);
begin
  Modified := true;
end;

procedure TStreetEditorForm.NameEditKeyPress(Sender: TObject; var Key: Char);
begin
  with NameEdit as TNumEdit do
  begin
    if (Length(Text) = 0) or (SelStart = 0) then
      if Key = '[' then Key := #0;
  end;
end;

procedure TStreetEditorForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then Button3Click(Sender);
end;

procedure TStreetEditorForm.Button3Click(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 213580);
end;

end.
