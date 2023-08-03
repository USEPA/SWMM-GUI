unit Dtimeplot;

{-------------------------------------------------------------------}
{                    Unit:    Dtimeplot.pas                         }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21     (5.2.0)                  }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Stay-on-top form unit used to define the contents of a time     }
{   series plot of simulation results. It consists of a Notebook    }
{   control with one page used to list the data series assigned     }
{   for plotting and another page used to identify an object and    }
{   variable for a particular data series.                          }
{-------------------------------------------------------------------}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, UpDnEdit, Vcl.Buttons, Vcl.ExtCtrls,
  Fgraph, Uglobals, Uproject, Vcl.ComCtrls, Uutils;

type
  TTimePlotForm = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Panel2: TPanel;
    OkBtn: TButton;
    CancelBtn1: TButton;
    HelpBtn1: TButton;
    GroupBox1: TGroupBox;
    BtnAdd: TBitBtn;
    SeriesListBox: TListBox;
    BtnDelete: TBitBtn;
    BtnMoveUp: TBitBtn;
    BtnMoveDown: TBitBtn;
    BtnEdit: TBitBtn;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label5: TLabel;
    StartDateCombo1: TComboBox;
    EndDateCombo1: TComboBox;
    ElapsedTimeBtn: TRadioButton;
    DateTimeBtn: TRadioButton;
    Panel1: TPanel;
    Label2: TLabel;
    ObjNameLabel: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label3: TLabel;
    ObjTypeCombo: TComboBox;
    ObjNameEdit: TEdit;
    VariableCombo: TComboBox;
    LegendLabelEdit: TEdit;
    AcceptBtn: TButton;
    CancelBtn2: TButton;
    HelpBtn2: TButton;
    LeftAxisBtn: TRadioButton;
    RightAxisBtn: TRadioButton;
    procedure OkBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BtnAddClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure AcceptBtnClick(Sender: TObject);
    procedure BtnEditClick(Sender: TObject);
    procedure CancelBtn2Click(Sender: TObject);
    procedure ObjTypeComboChange(Sender: TObject);
    procedure CancelBtn1Click(Sender: TObject);
    procedure BtnDeleteClick(Sender: TObject);
    procedure BtnMoveUpClick(Sender: TObject);
    procedure BtnMoveDownClick(Sender: TObject);
    procedure HelpBtn1Click(Sender: TObject);
    procedure HelpBtn2Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    ReportSelection: TReportSelection;
    SeriesIndex: Integer;
    procedure AddSelectedObject;
    procedure ClearReportSelection;
    procedure ClearReportItem(const I: Integer);
    procedure UpdateSeriesBtns;
    procedure UpdateVariableCombo;
    function  ObjectExists: Boolean;
  public
    { Public declarations }
    procedure Setup; overload;
    procedure SetObject;
  end;

var
  TimePlotForm: TTimePlotForm;

implementation

{$R *.dfm}

uses
  Fmain;

const

  Caption1 = 'Time Series Plot Selection';
  Caption2 = 'Data Series Selection';

  ObjTypeNames: array[0..3] of PChar =
    ('Subcatchment', 'Node', 'Link', 'System');

  MSG_NO_SUCH_OBJECT = 'There is no such object in your project.';

//-----------------------------------------------------------------------------

procedure TTimePlotForm.FormCreate(Sender: TObject);
begin
  Caption := Caption1;
  ObjTypeCombo.Items.Add('Subcatchment');
  ObjTypeCombo.Items.Add('Node');
  ObjTypeCombo.Items.Add('Link');
  ObjTypeCombo.Items.Add('System');
  ObjTypeCombo.ItemIndex := 0;

  with MainForm.ProjectImageList do
  begin
    GetBitmap(GetIndexByName('blue_plus'), BtnAdd.Glyph);
    GetBitmap(GetIndexByName('edit'), BtnEdit.Glyph);
    GetBitmap(GetIndexByName('blue_minus'), BtnDelete.Glyph);
    GetBitmap(GetIndexByName('uparrow2'), BtnMoveUp.Glyph);
    GetBitmap(GetIndexByName('dnarrow2'), BtnMoveDown.Glyph);
  end;

end;

//-----------------------------------------------------------------------------

procedure TTimePlotForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Hide;
end;

//-----------------------------------------------------------------------------

procedure TTimePlotForm.Setup;
begin
  StartDateCombo1.Items := MainForm.DateListBox.Items;
  EndDateCombo1.Items := MainForm.DateListBox.Items;
  StartDateCombo1.ItemIndex := 0;
  EndDateCombo1.ItemIndex := EndDateCombo1.Items.Count - 1;
  if RptElapsedTime then ElapsedTimeBtn.Checked := True
  else DateTimeBtn.Checked := True;
  SeriesListBox.Clear;
  ObjTypeCombo.ItemIndex := SYS;
  UpdateVariableCombo;
  ClearReportSelection;
  SeriesListBox.Clear;
  AddSelectedObject;
  if Length(ObjNameEdit.Text) > 0 then
  begin
    SeriesIndex := 0;
    AcceptBtnClick(self);
  end;
  UpdateSeriesBtns;
  PageControl1.ActivePageIndex := 0;
  Caption := Caption1;
  if SeriesListBox.Items.Count > 0 then ActiveControl := BtnEdit
  else ActiveControl := BtnAdd;
end;

//-----------------------------------------------------------------------------

procedure TTimePlotForm.SetObject;
begin
  AddSelectedObject;
end;

//-----------------------------------------------------------------------------

procedure TTimePlotForm.BtnAddClick(Sender: TObject);
begin
  ObjNameEdit.Clear;
  LegendLabelEdit.Clear;
  LeftAxisBtn.Checked := True;
  SeriesIndex := SeriesListBox.Items.Count;
  AddSelectedObject;
  PageControl1.ActivePageIndex := 1;
  Caption := Caption2;
end;

//-----------------------------------------------------------------------------

procedure TTimePlotForm.BtnDeleteClick(Sender: TObject);
var
  I, Imax: Integer;
begin
  SeriesIndex := SeriesListBox.ItemIndex;
  Imax := SeriesListBox.Items.Count - 1;
  if SeriesIndex < Imax then
  begin
    for I := SeriesIndex to Imax-1 do
    begin
      SeriesListBox.Items[I] := SeriesListBox.Items[I+1];
      ReportSelection.ReportItems[I] := ReportSelection.ReportItems[I+1];
    end;
  end;
  SeriesListBox.Items.Delete(Imax);
  ClearReportItem(Imax);
  if SeriesIndex >= SeriesListBox.Items.Count then Dec(SeriesIndex);
  SeriesListBox.ItemIndex := SeriesIndex;
  UpdateSeriesBtns;
end;

//-----------------------------------------------------------------------------

procedure TTimePlotForm.BtnEditClick(Sender: TObject);
begin
  SeriesIndex := SeriesListBox.ItemIndex;
  with ReportSelection.ReportItems[SeriesIndex] do
  begin
    ObjTypeCombo.ItemIndex := ObjType;
    UpdateVariableCombo;
    ObjNameEdit.Text := ObjName;
    VariableCombo.ItemIndex := Variable;
    if Axis = 1 then LeftAxisBtn.Checked := True
    else RightAxisBtn.Checked := True;
    LegendLabelEdit.Text := LegendTxt;
    PageControl1.ActivePageIndex := 1;
    Caption := Caption2;
  end;
end;

//-----------------------------------------------------------------------------

procedure TTimePlotForm.BtnMoveDownClick(Sender: TObject);
var
  I, N: Integer;
  TmpReportItem: TReportItem;
begin
  with SeriesListBox do
  begin
    I := ItemIndex;
    N := Items.Count-1;
    if (I < N) then
    begin
      TmpReportItem := ReportSelection.ReportItems[I];
      ReportSelection.ReportItems[I] :=
        ReportSelection.ReportItems[I+1];
      ReportSelection.ReportItems[I+1] := TmpReportItem;
      Items.Exchange(I, I+1);
    end;
  end;
end;

//-----------------------------------------------------------------------------

procedure TTimePlotForm.BtnMoveUpClick(Sender: TObject);
var
  I: Integer;
  TmpReportItem: TReportItem;
begin
  with SeriesListBox do
  begin
    I := ItemIndex;
    if (I > 0) then
    begin
      TmpReportItem := ReportSelection.ReportItems[I];
      ReportSelection.ReportItems[I] :=
        ReportSelection.ReportItems[I-1];
      ReportSelection.ReportItems[I-1] := TmpReportItem;
      Items.Exchange(I, I-1);
    end;
  end;
end;

//-----------------------------------------------------------------------------

procedure TTimePlotForm.CancelBtn1Click(Sender: TObject);
begin
  Hide;
end;

//-----------------------------------------------------------------------------

procedure TTimePlotForm.CancelBtn2Click(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;
  Caption := Caption1;
end;

//-----------------------------------------------------------------------------

procedure TTimePlotForm.AcceptBtnClick(Sender: TObject);
var
  S: String;
begin
  if not ObjectExists then
  begin
    Uutils.MsgDlg(MSG_NO_SUCH_OBJECT, mtError, [mbOK]);
    Exit;
  end;

  with ReportSelection.ReportItems[SeriesIndex] do
  begin
    ObjType := ObjTypeCombo.ItemIndex;
    ObjName := ObjNameEdit.Text;
    Variable := VariableCombo.ItemIndex;

    if LeftAxisBtn.Checked then Axis := 1 else Axis := 2;
    S := Trim(LegendLabelEdit.Text);
    LegendTxt := S;
    if Length(S) = 0 then
    begin
      S := ObjTypeNames[ObjType];
      if ObjType <> SYS then S := S + ' ' + ObjName;
      S := S + ' ' + VariableCombo.Text
    end;
  end;
  if SeriesIndex < SeriesListBox.Items.Count then
    SeriesListBox.Items[SeriesIndex] := S
  else if SeriesIndex = SeriesListBox.Items.Count then
    SeriesListBox.Items.Add(S);
  SeriesListBox.ItemIndex := SeriesListBox.Items.Count - 1;
  UpdateSeriesBtns;
  PageControl1.ActivePageIndex := 0;
  Caption := Caption1;
end;

//-----------------------------------------------------------------------------

function TTimePlotForm.ObjectExists: Boolean;
var
  S: String;
  I, J: Integer;
begin
  Result := False;
  S := Trim(ObjNameEdit.Text);
  case ObjTypeCombo.ItemIndex of
  SUBCATCHMENTS: if Project.FindSubcatch(S, I, J) then Result := True;
  NODES:         if Project.FindNode(S, I, J) then Result := True;
  LINKS:         if Project.FindLink(S, I, J) then Result := True;
  SYS:           Result := True;
  end;
end;

//-----------------------------------------------------------------------------

procedure TTimePlotForm.ClearReportSelection;
var
  I: Integer;
begin
  with ReportSelection do
  begin
    ReportType := TIMESERIESPLOT;
    StartDateIndex := 0;
    EndDateIndex := 0;
    DateTimeDisplay := False;
    for I := 0 to High(ReportItems) do ClearReportItem(I);
  end;
end;

//-----------------------------------------------------------------------------

procedure TTimePlotForm.ClearReportItem(const I: Integer);
begin
  with ReportSelection do
  begin
    ReportItems[I].ObjType := 0;
    ReportItems[I].ObjName := '';
    ReportItems[I].LegendTxt := '';
    ReportItems[I].Variable := -1;
  end;
end;

//-----------------------------------------------------------------------------

procedure TTimePlotForm.AddSelectedObject;
var
  OldObjType: Integer;
  SelectedItemIndex: Integer;
begin
  OldObjType := ObjTypeCombo.ItemIndex;
  if Project.IsSubcatch(Uglobals.CurrentList)
  then ObjTypeCombo.ItemIndex := SUBCATCHMENTS
  else if Project.IsNode(Uglobals.CurrentList)
  then ObjTypeCombo.ItemIndex := NODES
  else if Project.IsLink(Uglobals.CurrentList)
  then ObjTypeCombo.ItemIndex := LINKS
  else Exit;
  if ObjTypeCombo.ItemIndex <> OldObjType then UpdateVariableCombo;

  SelectedItemIndex := Project.CurrentItem[Uglobals.CurrentList];
  if SelectedItemIndex >= 0 then
    ObjNameEdit.Text := Project.GetID(Uglobals.CurrentList, SelectedItemIndex);

  LegendLabelEdit.Clear;
  LeftAxisBtn.Checked := True;
end;

//-----------------------------------------------------------------------------

procedure TTimePlotForm.ObjTypeComboChange(Sender: TObject);
begin
  UpdateVariableCombo;
end;

//-----------------------------------------------------------------------------

procedure TTimePlotForm.OkBtnClick(Sender: TObject);
var
  I: Integer;
begin
  Hide;
  if SeriesListBox.Items.Count > 0 then with ReportSelection do
  begin
    ReportType := TIMESERIESPLOT;
    StartDateIndex := StartDateCombo1.ItemIndex;
    EndDateIndex := EndDateCombo1.ItemIndex + 1;
    DateTimeDisplay := DateTimeBtn.Checked;

    Items := SeriesListBox.Items;
    ItemCount := SeriesListBox.Items.Count;

    VariableCount := ItemCount;
    for I := 0 to VariableCount do
    begin
      with ReportItems[I] do
      begin
        case ObjType of
        SUBCATCHMENTS: Variable := Variable + Uglobals.SUBCATCHOUTVAR1;
        NODES:         Variable := Variable + Uglobals.NODEOUTVAR1;
        LINKS:         Variable := Variable + Uglobals.LINKOUTVAR1;
        SYS:           Variable := Variable + Uglobals.SYSOUTVAR1;
        end;
      end;
    end;

//    if theGraph = nil then
    MainForm.CreateReport(ReportSelection);
  end;
end;

//-----------------------------------------------------------------------------

procedure TTimePlotForm.UpdateSeriesBtns;
begin
  with SeriesListBox do
  begin
    BtnAdd.Enabled := Items.Count < 6;
    BtnEdit.Enabled := Items.Count > 0;
    BtnDelete.Enabled := Items.Count > 0;
    BtnMoveUp.Visible := Items.Count > 1;
    BtnMoveDown.Visible := Items.Count > 1;
  end;
end;

//-----------------------------------------------------------------------------

procedure TTimePlotForm.UpdateVariableCombo;
var
  I: Integer;
begin
  ObjNameEdit.Clear;
  LegendLabelEdit.Clear;
  VariableCombo.Clear;
  case ObjTypeCombo.ItemIndex of
  SUBCATCHMENTS:
    begin
      for I := Uglobals.SUBCATCHOUTVAR1 to
               MainForm.SubcatchViewBox.Items.Count-1 do
        VariableCombo.Items.Add(MainForm.SubcatchViewBox.Items[I]);
      VariableCombo.ItemIndex := RUNOFF - SUBCATCHOUTVAR1;
    end;
  NODES:
    begin
      for I := Uglobals.NODEOUTVAR1 to
               MainForm.NodeViewBox.Items.Count-1 do
        VariableCombo.Items.Add(MainForm.NodeViewBox.Items[I]);
      VariableCombo.ItemIndex := NODEDEPTH - NODEOUTVAR1;
    end;
  LINKS:
    begin
      for I := Uglobals.LINKOUTVAR1 to
               MainForm.LinkViewBox.Items.Count-1 do
        VariableCombo.Items.Add(MainForm.LinkViewBox.Items[I]);
      VariableCombo.ItemIndex := FLOW - LINKOUTVAR1;
    end;
  SYS:
    begin
      for I := 0 to Uglobals.NsysViews-1 do
        VariableCombo.Items.Add(Uglobals.SysViewNames[I]);
      VariableCombo.ItemIndex := SYS_RAINFALL;
    end;
  end;
  ObjNameEdit.Enabled := not (ObjTypeCombo.ItemIndex = SYS);
  ObjNameLabel.Enabled := ObjNameEdit.Enabled;
end;

procedure TTimePlotForm.HelpBtn1Click(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 211390);
end;

procedure TTimePlotForm.HelpBtn2Click(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 213450);
end;

procedure TTimePlotForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then
  begin
    if PageControl1.ActivePageIndex = 0 then
       HelpBtn1Click(Sender)
    else
      HelpBtn2Click(Sender);
  end;
end;

end.
