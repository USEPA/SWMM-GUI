unit Dgweqn;

{-------------------------------------------------------------------}
{                    Unit:    Dgweqn.pas                            }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21     (5.2.0)                  }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form unit used to edit custom groundwater flow math      }
{   expressions.                                                    }
{-------------------------------------------------------------------}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TGWEqnForm = class(TForm)
    Label1: TLabel;
    Memo1: TMemo;
    OkBtn: TButton;
    CancelBtn: TButton;
    Label2: TLabel;
    Edit1: TMemo;
    HelpBtn: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure HelpBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetupForm(EqnType: Integer);
    procedure SetEqn(Eqn: String);
    function  GetEqn: String;
  end;

var
  GWEqnForm: TGWEqnForm;

implementation

{$R *.dfm}

const
  Label1Text: array[0..1] of String =
    ('Enter an expression to use in addition to the standard equation ' +
     'for lateral groundwater flow',
     'Enter an expression to use for flow to deep groundwater');

  Label2Text: array[0..1] of String =
    ('(leave blank to use only the standard equation):',
     '(leave blank to use the aquifer''s lower groundwater loss rate):');

  Memo1Text1: array[0..1] of String =
    ('The result of evaluating your custom equation will be added onto ' +
     'the result of the standard equation. To replace the standard equation ' +
     'completely set all of its coefficients to 0. Remember that groundwater ' +
     'flow units are cfs/acre for US units and cms/ha for metric units.',
     'The units of flow to deep groundwater are in/hr for US units and ' +
     'mm/hr for metric units.');

  Memo1Text2: array[0..12] of String =
    ('',
    'You can use the following symbols in your expression:',
    '    Hgw'#9'(height of the groundwater table above aquifer bottom, ft or m)',
    '    Hsw'#9'(height of the surface water above aquifer bottom, ft or m)',
    '    Hcb'#9'(height of the channel bottom above aquifer bottom, ft or m)',
    '    Hgs'#9'(height of the ground surface above aquifer bottom, ft or m)',
    '    Ks'#9'(saturated hydraulic conductivity, in/hr or mm/hr)',
    '    K'#9'(unsaturated hydraulic conductivity, in/hr or mm/hr)',
    '    Theta'#9'(moisture content of unsaturated upper zone, fraction)',
    '    Phi'#9'(soil porosity, fraction)',
    '    Fi'#9'(surface infiltration rate, in/hr or mm/hr)',
    '    Fu'#9'(upper soil zone percolation rate, in/hr or mm/hr)',
    '    A'#9'(subcatchment area, ac or ha)'
    );

  Memo1Text3: array[0..3] of String =
    ('',
    'Use the STEP function to have flow only when the groundwater ' +
    'level is above a certain threshold. For example, the expression:',
    '    0.001 * (Hgw - 5) * STEP(Hgw - 5)',
    'would generate flow only when Hgw was above 5.');

procedure TGWEqnForm.FormShow(Sender: TObject);
begin
  Edit1.SetFocus;
end;

procedure TGWEqnForm.SetupForm(EqnType: Integer);
var
  I: Integer;
begin
  if (EqnType >= 0) and (EqnType <= 1) then
  begin
    Label1.Caption := Label1Text[EqnType];
    Label2.Caption := Label2Text[EqnType];
    with Memo1 do
    begin
      Lines.Append(Memo1Text1[EqnType]);
      for I := 0 to High(Memo1Text2) do
        Lines.Append(Memo1Text2[I]);
      if EqnType = 0 then for I := 0 to High(Memo1Text3) do
        Lines.Append(Memo1Text3[I]);
    end;
  end;
end;

procedure TGWEqnForm.SetEqn(Eqn: String);
begin
  Edit1.Text := Eqn;
end;

function TGWEqnForm.GetEqn: String;
begin
  Result := Trim(Edit1.Text);
end;

procedure TGWEqnForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 213440);
end;

procedure TGWEqnForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then HelpBtnClick(Sender);
end;

end.
