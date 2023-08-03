unit Dlidusage;

{-------------------------------------------------------------------}
{                    Unit:    Dlidusage.pas                         }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21   (5.2.0)                    }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form unit that defines how a specific LID control        }
{   should be deployed within a given subcatchment.                 }
{   This dialog is launched by the LID Group dialog form            }
{   (Dlidgroup.pas).                                                }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, NumEdit, FileCtrl, Uproject, Uglobals, Uutils, Buttons,
  Spin, ExtCtrls, ComCtrls;

type
  TLidUsageDlg = class(TForm)
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ComboBox1: TComboBox;
    OkBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    UnitAreaEdit: TNumEdit;
    InitSatEdit: TNumEdit;
    FromImpervEdit: TNumEdit;
    RptFileEdit: TEdit;
    Label7: TLabel;
    BrowseBtn: TBitBtn;
    SaveDialog1: TSaveDialog;
    ClearBtn: TBitBtn;
    Label2a: TLabel;
    UnitWidthEdit: TNumEdit;
    Label2: TLabel;
    Label8: TLabel;
    Label5: TLabel;
    PcntAreaLabel: TLabel;
    FullAreaCheckBox: TCheckBox;
    NumUnitsUpDn: TUpDown;
    NumUnitsEdit: TEdit;
    Notebook1: TNotebook;
    VegSwaleImage: TImage;
    RainBarrelImage: TImage;
    InfilTrenchImage: TImage;
    BioCellImage: TImage;
    Bevel1: TBevel;
    Bevel2: TBevel;
    PermPaveImage: TImage;
    DrainOutletCheckBox: TCheckBox;
    Label6: TLabel;
    Label9: TLabel;
    DrainOutletEdit: TEdit;
    Label10: TLabel;
    FromPervEdit: TNumEdit;
    procedure FormCreate(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure BrowseBtnClick(Sender: TObject);
    procedure ControlChange(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure NumUnitsEditChange(Sender: TObject);
    procedure FullAreaCheckBoxClick(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    RptFileName: String;     //Name of optional report file
    procedure DisplayLidImage;
    procedure DisplayFullUnitArea;
    procedure SetPcntArea;
  public
    { Public declarations }
    Modified: Boolean;
    SubcatchName: String;
    procedure SetData(Data: array of String);
    procedure GetData(var Data: array of String);
  end;

//var
//  LidUsageDlg: TLidUsageDlg;

implementation

{$R *.dfm}

uses
 Fmain, Ulid;

procedure TLidUsageDlg.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnCreate handler for the dialog.
//-----------------------------------------------------------------------------
var
  I: Integer;
  N: Integer;
  S: String;
begin
  // Add existing names of LID controls to combo box
  N := Project.Lists[LID].Count;
  for I := 0 to N-1 do
  begin
    S := Project.Lists[LID].Strings[I];
    ComboBox1.Items.Add(S);
  end;
 with MainForm.ProjectImageList do
 begin
    GetBitmap(GetIndexByName('browse'), BrowseBtn.Glyph);
    GetBitmap(GetIndexByName('delete'), ClearBtn.Glyph);
 end;

end;

procedure TLidUsageDlg.SetData(Data: array of String);
//-----------------------------------------------------------------------------
//  Loads LID usage for the subcatchment into the dialog.
//-----------------------------------------------------------------------------
begin
// The contents of the Data argument passed into this routine are:
// 0 - LID name
// 1 - number of replicate units
// 2 - unit area
// 3 - unit width
// 4 - % initially saturated
// 5 - % of Impervious area treated
// 6 - outflow routing (0 = Outlet, 1 = Pervious)
// 7 - name of report file
// 8 - path of report file
// 9 - underdrain outlet
// 10 - % of Pervious area treated

  ComboBox1.ItemIndex := ComboBox1.Items.IndexOf(Data[0]);
  NumUnitsUpDn.Position := StrToInt(Data[1]);
  UnitAreaEdit.Text := Data[2];
  UnitWidthEdit.Text := Data[3];
  InitSatEdit.Text := Data[4];
  FromImpervEdit.Text := Data[5];
  FromPervEdit.Text := Data[10];
  DrainOutletCheckBox.Checked := SameText(Data[6], '1');
  RptFileName := Data[7];
  RptFileEdit.Text := MinimizeName(RptFileName, self.Canvas, RptFileEdit.ClientWidth);
  DrainOutletEdit.Text := Data[9];
  SetPcntArea;
  if ComboBox1.ItemIndex >= 0 then DisplayLidImage;
  Modified := False;
end;

procedure TLidUsageDlg.GetData(var Data: array of String);
//-----------------------------------------------------------------------------
//  Retrieves LID usage data from the dialog
//-----------------------------------------------------------------------------
begin
  Data[0] := Trim(ComboBox1.Text);
  Data[1] := IntToStr(NumUnitsUpDn.Position);
  Data[2] := UnitAreaEdit.Text;
  Data[3] := UnitWidthEdit.Text;
  Data[4] := InitSatEdit.Text;
  Data[5] := FromImpervEdit.Text;
  Data[6] := '0';
  if (DrainOutletCheckBox.Checked = True) then Data[6] := '1';
  Data[7] := RptFileName;
  Data[9] := Trim(DrainOutletEdit.Text);
  Data[10] := FromPervEdit.Text;
end;

procedure TLidUsageDlg.OkBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  On Click handler for the OK button
//-----------------------------------------------------------------------------
var
  V: Extended;
begin
  // Check that an LID control was specified
  with ComboBox1 do
  begin
    if Length(Trim(Text)) = 0 then
    begin
      Uutils.MsgDlg('LID Control Name cannot be left blank.', mtError, [mbOK]);
      SetFocus();
      Exit;
    end;
  end;

  // Check that LID area <= subcatchment area
  Uutils.GetExtended(PcntAreaLabel.Caption, V);
  if (V < 0.0) or (V > 100.001) then
  begin
    Uutils.MsgDlg('LID area exceeds subcatchment area', mtError, [mbOK]);
    Exit;
  end;

  // Signal that the data are Ok
  ModalResult := mrOk;
end;

procedure TLidUsageDlg.BrowseBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  On Click handler for the Report File Browse button
//-----------------------------------------------------------------------------
begin
  with SaveDialog1 do
  begin
    Title := 'LID Report File';
    Filter := 'LID Report Files (*.txt)|*.txt|All files|*.*';;
    InitialDir := ProjectDir;
    DefaultExt := 'txt';
    if Length(RptFileEdit.Text) > 0
    then Filename := ChangeFileExt(ExtractFileName(RptFileName),'.txt')
    else Filename := '*.txt';
    if Execute then
    begin
      RptFileName := Filename;
      RptFileEdit.Text :=
        MinimizeName(RptFileName, self.Canvas, RptFileEdit.ClientWidth);
    end;
  end;
end;

procedure TLidUsageDlg.CancelBtnClick(Sender: TObject);
begin
   ModalResult := mrCancel;
end;

procedure TLidUsageDlg.ClearBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  On Click handler for the Report File Clear button
//-----------------------------------------------------------------------------
begin
  RptFileName := '';
  RptFileEdit.Text := '';
end;

procedure TLidUsageDlg.ComboBox1Change(Sender: TObject);
//-----------------------------------------------------------------------------
//  On Click handler for the LID Control Name combo box
//-----------------------------------------------------------------------------
begin
  Modified := True;
  DisplayLidImage;
end;

procedure TLidUsageDlg.ControlChange(Sender: TObject);
//-----------------------------------------------------------------------------
//  On Change handler for the dialog's controls
//-----------------------------------------------------------------------------
begin
  Modified := True;
end;

procedure TLidUsageDlg.DisplayLidImage;
//-----------------------------------------------------------------------------
//  Displays a sketch of the LID type associated with a selected LID control
//-----------------------------------------------------------------------------
var
  J : Integer;
  P : Integer;
  S : String;
begin
  S := ComboBox1.Text;
  J := Project.Lists[LID].IndexOf(S);
  if J < 0 then Notebook1.ActivePage := 'BlankPage' else
  begin
    P := TLid(Project.Lists[LID].Objects[J]).ProcessType;
    if      P = Ulid.PERM_PAVE then Notebook1.ActivePage := 'PermPavePage'
    else if P = Ulid.INFIL_TRENCH then Notebook1.ActivePage := 'InfilTrenchPage'
    else if P = Ulid.RAIN_BARREL then Notebook1.ActivePage := 'RainBarrelPage'
    else if P = Ulid.VEG_SWALE then Notebook1.ActivePage := 'VegSwalePage'
    else Notebook1.ActivePage := 'BioCellPage';
  end;
end;

procedure TLidUsageDlg.DisplayFullUnitArea;
//-----------------------------------------------------------------------------
//  Displays area per LID unit when LID occupies full subcatchment
//-----------------------------------------------------------------------------
var
  A: Extended;
begin
    A := Ulid.GetSubcatchArea;
    UnitAreaEdit.Text := Format('%0.2f', [A / NumUnitsUpDn.Position]);
end;

procedure TLidUsageDlg.NumUnitsEditChange(Sender: TObject);
//-----------------------------------------------------------------------------
//  On Change handler for controls that affect LID's area
//-----------------------------------------------------------------------------
begin
  SetPcntArea;
  if FullAreaCheckBox.Checked then DisplayFullUnitArea;
  Modified := True;
end;

procedure TLidUsageDlg.FullAreaCheckBoxClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  On Click handler for LID Occupies Full Subcatchment check box
//-----------------------------------------------------------------------------
begin
  if FullAreaCheckBox.Checked then
  begin
    DisplayFullUnitArea;
    PcntAreaLabel.Caption := '100.0';
    UnitAreaEdit.Enabled := False;
  end
  else UnitAreaEdit.Enabled := True;
  Modified := True;
end;

procedure TLidUsageDlg.SetPcntArea;
//-----------------------------------------------------------------------------
//  Computes percent of subcatchment area occupied by the LID unit
//-----------------------------------------------------------------------------
begin
  PcntAreaLabel.Caption := Ulid.GetPcntArea(NumUnitsUpDn.Position, UnitAreaEdit.Text);
end;

procedure TLidUsageDlg.HelpBtnClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 213340);
end;

procedure TLidUsageDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then HelpBtnClick(Sender);
end;

end.
