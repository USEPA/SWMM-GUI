unit Darchpipe;

{-------------------------------------------------------------------}
{                    Unit:    Darchpipe.pas                         }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    03/24/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form used to select a standard size of arch pipe.        }
{                                                                   }
{   The dialog uses two identical Tree View components (one for     }
{   US units and the other for SI units) to list standard arch      }
{   pipe sizes (height x width). Pipe material categories form      }
{   the root nodes of the tree with the pipe sizes for each         }
{   category listed beneath them as sub-nodes.                      }
{-------------------------------------------------------------------}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls, System.Types, StrUtils;

type
  TArchPipeForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    TreeView1: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    SizeCode:  Integer;   // Size code of the standard size pipe
    MaxHeight: String;    // Length of minor axis
    MaxWidth:  String;    // Length of major axis
  end;

var
  ArchPipeForm: TArchPipeForm;

implementation

{$R *.dfm}

uses
  Dxsect, Uglobals, Uutils;

const
  // Indexes of the last sub-nodes within each pipe material
  // category of the Tree View listing of standard sizes.
  MaxIndex: array[1..5] of Integer = (19, 32, 48, 83, 108);

  MSG_NO_SELECTION = 'You must select a pipe size or "Custom"';

procedure TArchPipeForm.FormCreate(Sender: TObject);
//----------------------------------------------------------------------------
//  OnCreate handler for the form.
//----------------------------------------------------------------------------
var
  TreeIndex,            // Index of TreeView node
  CatIndex,             // Index of pipe material category
  SizeIndex: Integer;   // Index of std. pipe size
begin
  // Set form's caption
  if Uglobals.UnitSystem = Uglobals.usUS then
  begin
    Caption := Caption + ' (Rise x Span - inches)';
  end
  else
  begin
    Caption := Caption + ' (Rise x Span - mm)';
  end;

  // Fill in the TreeView items with std. arch pipe sizes, skipping
  // the nodes used for 'Custom' and names of material categories
  CatIndex := 1;
  SizeIndex := 0;
  for TreeIndex := 2 to TreeView1.Items.Count-1 do
  begin
    if TreeIndex = MaxIndex[CatIndex] then Inc(CatIndex) else
    begin
      Inc(SizeIndex);
      with Parent as TXsectionForm do
        TreeView1.Items[TreeIndex].Text := GetArchSize(SizeIndex);
    end;
  end;
  TreeView1.Items[0].Selected := True;
end;

procedure TArchPipeForm.FormShow(Sender: TObject);
//----------------------------------------------------------------------------
//  Form's OnShow Handler. Selects the TreeView node corresponding to
//  the currently selected standard arch size code.
//----------------------------------------------------------------------------
var
  J, K: Integer;
begin
  if SizeCode = 0 then TreeView1.Items[0].Selected := True;
  for J := 1 to High(MaxIndex) do
  begin
    K := SizeCode + J;
    if K < MaxIndex[J] then
    begin
      TreeView1.Items[K].Selected := True;
      Exit;
    end;
  end;
end;

procedure TArchPipeForm.Button1Click(Sender: TObject);
//----------------------------------------------------------------------------
//  OnClick handler for the OK button.
//----------------------------------------------------------------------------
var
  I, J: Integer;
  theNode: TTreeNode;
  Sizes: TStringDynArray;
begin
  // Get the selected tree view node and its index
  theNode := TreeView1.Selected;
  I := theNode.AbsoluteIndex;

  // Node is top root node ('Custom')
  if I = 0 then
  begin
    SizeCode := I;
    MaxHeight := '';
    MaxWidth := '';
  end

  // Node is another root node (i.e., a pipe category, not a size)
  else if theNode.Parent = nil then
  begin
    Uutils.MsgDlg(MSG_NO_SELECTION, mtError, [mbOK]);
    Exit;
  end

  // Node contains a pipe size
  else
  begin
    // Find the size code for the selected size
    for J := 1 to High(MaxIndex) do
    begin
      if I < MaxIndex[J] then
      begin
        SizeCode := I - J;
        break;
      end;
    end;
    // Split out the height and width from the node's text
    Sizes := StrUtils.SplitString(theNode.Text, 'x');
    if Length(Sizes) > 1 then
    begin
      MaxHeight := Trim(Sizes[0]);
      MaxWidth := Trim(Sizes[1]);
    end;
  end;
  ModalResult := mrOK;
end;

end.
