unit Dpollut;

{-------------------------------------------------------------------}
{                    Unit:    Dpollut.pas                           }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form unit used to edit a pollutant's name and its        }
{   properties.                                                     }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, StdCtrls, ExtCtrls, Uproject, Uglobals, Uutils, PropEdit;

type
  TPollutantForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    OKBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    Panel3: TPanel;
    HintLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    PropEdit1: TPropEdit;
    PropList: TStringlist;
    PollutIndex: Integer;
    procedure ShowPropertyHint(Sender: TObject; aRow: LongInt);
    procedure ValidateEntry(Sender: TObject; Index: Integer; var S: String;
              var Errmsg: String; var IsValid: Boolean);
    function ValidateName: Boolean;
  public
    { Public declarations }
    HasChanged: Boolean;
    procedure SetData(const Index: Integer; Pollut: TPollutant);
    procedure GetData(var S: String; Pollut: TPollutant);
  end;

//var
//  PollutantForm: TPollutantForm;

implementation

{$R *.DFM}

const
  TXT_PROPERTY = 'Property';
  TXT_VALUE = 'Value';
  MSG_NO_DATA = 'This data field cannot be blank.';
  MSG_INVALID_NAME = 'Invalid pollutant name.';
  MSG_DUPLICATE_NAME = 'Duplicate pollutant name.';

  DefaultProps: array[0..10] of String =
    ('', 'MG/L', '0.0', '0.0', '0.0', '0.0', '0.0', '0.0', 'NO', '', '');

   PollutHint: array[0..10] of String =
    ('User-assigned name of the pollutant.',
     'Concentration units for the pollutant.',
     'Concentration of the pollutant in rain water.',
     'Concentration of the pollutant in ground water.',
     'Concentration of the pollutant in infiltration/inflow flow.',
     'Concentration of the pollutant in dry weather sanitary flow.',
     'Initial concentration of the pollutant throughout the conveyance system.',
     'First-order decay coefficient of the pollutant (1/days).',
     'Does the pollutant build up only during snowfall events?',
     'Name of another pollutant to whose runoff concentration ' +
     'the current pollutant is dependent on.',
     'Fraction of the co-pollutant''s runoff concentration that ' +
     'becomes the current pollutant''s runoff concentration.');

var
  PollutProps: array[0..10] of TPropRecord =
    ((Name: 'Name';         Style: esEdit;       Mask: emNoSpace;    Length: 0),
     (Name: 'Units';        Style: esComboList;  Mask: emNone;       Length: 0;
      List: 'MG/L'#13'UG/L'#13'#/L'),
     (Name: 'Rain Concen.'; Style: esEdit;       Mask: emPosNumber;  Length: 0),
     (Name: 'GW Concen.';   Style: esEdit;       Mask: emPosNumber;  Length: 0),
     (Name: 'I&I Concen.';  Style: esEdit;       Mask: emPosNumber;  Length: 0),
     (Name: 'DWF Concen.';  Style: esEdit;       Mask: emPosNumber;  Length: 0),
     (Name: 'Init. Concen.';Style: esEdit;       Mask: emPosNumber;  Length: 0),
     (Name: 'Decay Coeff.'; Style: esEdit;       Mask: emPosNumber;  Length: 0),
     (Name: 'Snow Only';    Style: esComboList;  Mask: emNone;       Length:0;
      List:'NO'#13'YES'),
     (Name: 'Co-Pollutant'; Style: esEdit;       Mask: emNoSpace;    Length: 0),
     (Name: 'Co-Fraction';  Style: esEdit;       Mask: emPosNumber;  Length: 0));

procedure TPollutantForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnCreate handler.
//-----------------------------------------------------------------------------
begin
  // Create a Property Editor
  PropEdit1 := TPropEdit.Create(self);
  with PropEdit1 do
  begin
    Parent := Panel1;
    Align := alClient;
    BorderStyle := bsNone;
    ColHeading1 := TXT_PROPERTY;
    ColHeading2 := TXT_VALUE;
    ValueColor := clNavy;
    OnValidate := ValidateEntry;
    OnRowSelect := ShowPropertyHint;
  end;

  // Create a Property stringlist
  PropList := TStringlist.Create;
end;

procedure TPollutantForm.FormDestroy(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnDestroy handler.
//-----------------------------------------------------------------------------
begin
  PropList.Free;
  PropEdit1.Free;
end;

procedure TPollutantForm.FormShow(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnShow handler.
//-----------------------------------------------------------------------------
begin
  PropEdit1.SetProps(PollutProps, PropList);
  PropEdit1.Edit;
end;

procedure TPollutantForm.OKBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the OK button.
//-----------------------------------------------------------------------------
begin
  if (not PropEdit1.IsValid) or (not ValidateName) then
  begin
    ModalResult := mrNone;
    PropEdit1.Edit;
  end
  else
  begin
    HasChanged := PropEdit1.Modified;
    ModalResult := mrOK;
  end;
end;

procedure TPollutantForm.SetData(const Index: Integer; Pollut: TPollutant);
//-----------------------------------------------------------------------------
//  Loads data for a specific Pollutant object into the form.
//-----------------------------------------------------------------------------
var
  K: Integer;
begin
  PollutIndex := Index;
  if Index < 0 then
  begin
    for K := 0 to High(DefaultProps) do
      PropList.Add(DefaultProps[K]);
  end
  else
  begin
    PropList.Add(Project.Lists[POLLUTANT].Strings[Index]);
    for K := 1 to High(DefaultProps) do
      PropList.Add(Pollut.Data[K-1]);
  end;
  HasChanged := False;
end;

procedure TPollutantForm.GetData(var S: String; Pollut: TPollutant);
//-----------------------------------------------------------------------------
//  Unloads data from the form into a specific Pollutant object.
//-----------------------------------------------------------------------------
var
  K: Integer;
begin
  S := PropList[0];
  for K := 1 to High(DefaultProps) do
    Pollut.Data[K-1] := PropList[K];
end;

procedure TPollutantForm.ValidateEntry(Sender: TObject; Index: Integer;
  var S: String; var Errmsg: String; var IsValid: Boolean);
//-----------------------------------------------------------------------------
//  Property Editor's OnValidate handler.
//-----------------------------------------------------------------------------
begin
  // Concentration & decay coeff. fields cannot be blank
  IsValid := True;
  if Index in [2, 3, 4, 5, 6] then
  begin
    if Length(Trim(S)) = 0 then
    begin
      Errmsg := MSG_NO_DATA;
      IsValid := False;
    end;
  end;
end;

function TPollutantForm.ValidateName: Boolean;
//-----------------------------------------------------------------------------
//  Checks for a valid pollutant name.
//-----------------------------------------------------------------------------
var
  S : String;
  I : Integer;
begin
  // Retrieve pollutant name from 1st entry of property list
  Result := True;
  S := Trim(PropList[0]);

  // Check that a unique name was entered
  if Length(S) = 0 then
  begin
    Uutils.MsgDlg(MSG_INVALID_NAME, mtError, [mbOK]);
    Result := False;
    Exit;
  end;
  with Project.Lists[POLLUTANT] do
  for I := 0 to Count-1 do
  begin
    if I = PollutIndex then continue;
    if CompareText(S, Strings[I]) = 0 then
    begin
      Uutils.MsgDlg(MSG_DUPLICATE_NAME, mtError, [mbOK]);
      Result := False;
      Exit;
    end;
  end;
end;

procedure TPollutantForm.ShowPropertyHint(Sender: TObject; aRow: LongInt);
//-----------------------------------------------------------------------------
//  Property Editor's OnRowSelect handler. Displays a context-sensitive
//  hint in the form's hint panel when a new row (property) is selected. 
//-----------------------------------------------------------------------------
begin
  HintLabel.Caption := PollutHint[aRow];
end;

procedure TPollutantForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 211360);
end;

procedure TPollutantForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then HelpBtnClick(Sender);
end;

end.
