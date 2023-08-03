unit Dreporting;

{-------------------------------------------------------------------}
{                    Unit:    Dreporting.pas                        }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Stay-on-top form unit used to select which objects will have    }
{   detailed time series results saved to the binary output file.   } 
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Uglobals, Uproject, Buttons, TabNotBk,
  ExtCtrls, Uutils;

type
  TReportingForm = class(TForm)
    Label1: TLabel;
    PageControl1: TPageControl;
    SubcatchTabSheet: TTabSheet;
    NodesTabSheet: TTabSheet;
    LinksTabSheet: TTabSheet;
    ListBox1: TListBox;
    ListBox2: TListBox;
    ListBox3: TListBox;
    AllCheckBox1: TCheckBox;
    AllCheckBox2: TCheckBox;
    AllCheckBox3: TCheckBox;
    BtnAdd: TButton;
    BtnRemove: TButton;
    BtnClear: TButton;
    BtnClose: TButton;
    BtnHelp: TButton;
    ReportInputBox: TCheckBox;
    ReportControlsBox: TCheckBox;
    ReportAveragesBox: TCheckBox;
    procedure BtnAddClick(Sender: TObject);
    procedure BtnRemoveClick(Sender: TObject);
    procedure BtnClearClick(Sender: TObject);
    procedure BtnCloseClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure AllCheckBox1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    procedure ExportItems(Category: String; Items: TStrings; S: TStringlist;
              Tab: String);
  public
    { Public declarations }
    procedure Clear;
    procedure Export(S: TStringlist; Tab: String);
    procedure Import(TokList: TStringlist; Ntoks: Integer);
    procedure SetReportedItems(Category: Integer);
    procedure UpdateName(Category: Integer; const OldName: String;
              const NewName: String);
    procedure RemoveItem(Category: Integer; const OldName: String);
  end;

var
  ReportingForm: TReportingForm;

implementation

{$R *.dfm}

uses
  Fmain, Fmap, Ubrowser;

const
  MSG_NO_SUBCATCH_SELECTED = 'Must select a subcatchment.';
  MSG_NO_NODE_SELECTED = 'Must select a node.';
  MSG_NO_LINK_SELECTED = 'Must select a link.';

procedure TReportingForm.Clear;
var
  I: Integer;
begin
  for I := 1 to 3 do
  begin
    with FindComponent('AllCheckBox' + IntToStr(I)) as TCheckBox do
      Checked := True;
    with FindComponent('ListBox' + IntToStr(I)) as TListBox do Clear;
  end;
  ReportInputBox.Checked := False;
  ReportControlsBox.Checked := False;
  ReportAveragesBox.Checked := False;
end;

procedure TReportingForm.AllCheckBox1Click(Sender: TObject);
begin
  MainForm.SetChangeFlags;
end;

procedure TReportingForm.BtnAddClick(Sender: TObject);
var
  I: Integer;
  ItemsListBox: TListBox;
begin
  I := PageControl1.TabIndex;
  case I of
  0: if not Project.IsSubcatch(CurrentList) then
     begin
       Uutils.MsgDlg(MSG_NO_SUBCATCH_SELECTED, mtError, [mbOK]);
       Exit;
     end
     else ItemsListBox := ListBox1;
  1: if not Project.IsNode(CurrentList) then
     begin
       Uutils.MsgDlg(MSG_NO_NODE_SELECTED, mtError, [mbOK]);
       Exit;
     end
     else ItemsListBox := ListBox2;
  2: if not Project.IsLink(CurrentList) then
     begin
       Uutils.MsgDlg(MSG_NO_LINK_SELECTED, mtError, [mbOK]);
       Exit;
     end
     else ItemsListBox := ListBox3;
  else Exit;
  end;
  with Project do
  begin
    if CurrentItem[CurrentList] >= 0 then
    begin
      with ItemsListBox do
      begin
        Items.Add(GetID(CurrentList, CurrentItem[CurrentList]));
        ItemIndex := Items.Count-1;
      end;
      with FindComponent('AllCheckBox' + IntToStr(I+1)) as TCheckBox do
        if not Checked then MainForm.SetChangeFlags;
    end;
  end;
end;

procedure TReportingForm.BtnRemoveClick(Sender: TObject);
var
  I, J: Integer;
begin
  I := PageControl1.TabIndex + 1;
  with FindComponent('ListBox' + IntToStr(I)) as TListBox do
  begin
    J := ItemIndex;
    if ItemIndex >= 0 then Items.Delete(ItemIndex);
    if Items.Count > 0 then
    begin
      if J < Items.Count then ItemIndex := J
      else ItemIndex := Items.Count-1;
    end;
  end;
  if J >= 0 then
    with FindComponent('AllCheckBox' + IntToStr(I)) as TCheckBox do
      if not Checked then MainForm.SetChangeFlags;
end;

procedure TReportingForm.BtnClearClick(Sender: TObject);
var
  I: Integer;
  Cleared: Boolean;
begin
  Cleared := False;
  I := PageControl1.TabIndex+1;
  with FindComponent('ListBox' + IntToStr(I)) as TListBox do
  begin
    if Items.Count > 0 then
    begin
      Clear;
      Cleared := True;
    end;
  end;
  if Cleared then
    with FindComponent('AllCheckBox' + IntToStr(I)) as TCheckBox do
      if not Checked then MainForm.SetChangeFlags;
end;

procedure TReportingForm.BtnCloseClick(Sender: TObject);
begin
  Hide;
end;

procedure TReportingForm.Export(S: TStringlist; Tab: String);
begin
  if ReportInputBox.Checked     then S.Add('INPUT     ' + Tab + 'YES');
  if ReportControlsBox.Checked  then S.Add('CONTROLS  ' + Tab + 'YES');
  if ReportAveragesBox.Checked  then S.Add('AVERAGES  ' + Tab + 'YES');

  if AllCheckBox1.Checked
  then S.Add('SUBCATCHMENTS' + Tab + 'ALL')
  else ExportItems('SUBCATCHMENTS', ListBox1.Items, S, Tab);
  if AllCheckBox2.Checked
  then S.Add('NODES' + Tab + 'ALL')
  else ExportItems('NODES', ListBox2.Items, S, Tab);
  if AllCheckBox3.Checked
  then S.Add('LINKS' + Tab + 'ALL')
  else ExportItems('LINKS', ListBox3.Items, S, Tab);
end;

procedure TReportingForm.ExportItems(Category: String; Items: TStrings;
  S: TStringlist; Tab: String);
var
  Line: String;
  I, J: Integer;
begin
  if Items.Count = 0 then
  begin
    S.Add(Category + Tab + 'NONE');
    Exit;
  end;
  Line := Category;
  J := 0;
  for I := 0 to Items.Count-1 do
  begin
    Line := Line + Tab + Items[I];
    Inc(J);
    if J < 4 then continue;
    S.Add(Line);
    Line := Category;
    J := 0;
  end;
  if J > 0 then S.Add(Line);
end;

procedure TReportingForm.Import(TokList: TStringlist; Ntoks: Integer);
const
  Category: array[1 .. 3] of String = ('SUBCATCH', 'NODE', 'LINK');
var
  I, K, N: Integer;
  Tok: String;
begin
  if Ntoks < 2 then exit;
  Tok := UpperCase(TokList[0]);

  if SameText(Tok, 'INPUT')
  then ReportInputBox.Checked := SameText(TokList[1], 'YES')
  else if SameText(Tok, 'CONTROLS')
  then ReportControlsBox.Checked := SameText(TokList[1], 'YES')
  else if SameText(Tok, 'AVERAGES')
  then ReportAveragesBox.Checked := SameText(TokList[1], 'YES')
  else begin
    K := 0;
    for I := 1 to 3 do
      if Pos(Category[I], Tok) = 1 then K := I;
    if K = 0 then Exit;
    if SameText(TokList[1], 'ALL') then
    with FindComponent('AllCheckBox' + IntToStr(K)) as TCheckBox do
      Checked := True
    else if SameText(TokList[1], 'NONE') then
    with FindComponent('AllCheckBox' + IntToStr(K)) as TCheckBox do
      Checked := False
    else
    begin
      N := 0;
      with FindComponent('ListBox' + IntToStr(K)) as TListBox do
      begin
        for I := 1 to Ntoks-1 do
        begin
          Items.Add(TokList[I]);
          Inc(N);
        end;
      end;
      if N > 0 then
      with FindComponent('AllCheckBox' + IntToStr(K)) as TCheckBox do
        Checked := False;
    end;

  end;
end;

procedure TReportingForm.SetReportedItems(Category: Integer);
var
  I: Integer;
  J: Integer;
  ObjType: Integer;
  ObjIndex: Integer;
begin
  Category := Category + 1;
  with FindComponent('AllCheckBox' + IntToStr(Category)) as TCheckBox do
  begin
    if Checked then
    begin
      case Category of
      1: for J := 0 to Project.Lists[SUBCATCH].Count - 1 do
           Project.GetSubcatch(SUBCATCH, J).OutFileIndex := 1;
      2: for I := JUNCTION to STORAGE do
         begin
           for J := 0 to Project.Lists[I].Count - 1 do
             Project.GetNode(I, J).OutFileIndex := 1;
         end;
      3: for I := CONDUIT to OUTLET do
         begin
           for J := 0 to Project.Lists[I].Count - 1 do
             Project.GetLink(I, J).OutFileIndex := 1;
         end;
      end;
      Exit;
    end;
  end;
  with FindComponent('ListBox' + IntToStr(Category)) as TListBox do
  begin
    for I := 0 to Items.Count - 1 do
    begin
      case Category of
      1: if (Project.FindSubcatch(Items[I], ObjType, ObjIndex))
         then Project.GetSubcatch(ObjType, ObjIndex).OutFileIndex := 1;
      2: if (Project.FindNode(Items[I], ObjType, ObjIndex))
         then Project.GetNode(ObjType, ObjIndex).OutFileIndex := 1;
      3: if (Project.FindLink(Items[I], ObjType, ObjIndex))
         then Project.GetLink(ObjType, ObjIndex).OutFileIndex := 1;
      end;
    end;
  end;
end;

procedure TReportingForm.UpdateName(Category: Integer; const OldName: String;
  const NewName: String);
var
  I: Integer;
begin
  Category := Category + 1;
  with FindComponent('ListBox' + IntToStr(Category)) as TListBox do
  begin
    I := Items.IndexOf(OldName);
    if I >= 0 then Items[I] := NewName;
  end;
end;

procedure TReportingForm.RemoveItem(Category: Integer; const OldName: String);
var
  I: Integer;
begin
  Category := Category + 1;
  with FindComponent('ListBox' + IntToStr(Category)) as TListBox do
  begin
    I := Items.IndexOf(OldName);
    if I >= 0 then Items.Delete(I);
  end;
end;

procedure TReportingForm.BtnHelpClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 213240);
end;

procedure TReportingForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then BtnHelpClick(Sender);
end;

end.
