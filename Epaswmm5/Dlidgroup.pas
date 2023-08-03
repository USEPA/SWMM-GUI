unit Dlidgroup;

{-------------------------------------------------------------------}
{                    Unit:    Dlidgroup.pas                         }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    05/02/22       (5.2.1)                }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form unit that adds or deletes LID units to a sub-       }
{   catchment.                                                      }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, StdCtrls, Buttons, Uproject, Uglobals, Uutils;

type
  TLidGroupDlg = class(TForm)
    OkBtn: TButton;
    CancelBtn: TButton;
    StringGrid1: TStringGrid;
    HelpBtn: TButton;
    BtnAdd: TButton;
    BtnEdit: TButton;
    BtnDelete: TButton;
    procedure FormCreate(Sender: TObject);
    procedure BtnAddClick(Sender: TObject);
    procedure BtnEditClick(Sender: TObject);
    procedure BtnDeleteClick(Sender: TObject);
    procedure StringGrid1Click(Sender: TObject);
    procedure StringGrid1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure HelpBtnClick(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    procedure SetBtnState(const State: Boolean);
    procedure FillGridRow(const I: Integer; Data: array of String);
  public
    { Public declarations }
    HasChanged: Boolean;
    theSubcatchName: String;
    procedure SetData(theSubcatch: TSubcatch);
    procedure GetData(theSubcatch: TSubcatch);
  end;

var
  LidGroupDlg: TLidGroupDlg;

implementation

{$R *.dfm}

uses
  Dlidusage, Ulid;

const
  ColLabels: array[0..5] of String =
    ('Control Name', 'LID Type', '% of Area', '% From Imperv',
     '% From Perv', 'Report File');

procedure TLidGroupDlg.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnCreate handler for the dialog.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  // Column labels
  with StringGrid1 do
  begin
    RowCount := 2;
    ColCount := High(ColLabels) + 1 + Ulid.MAX_LID_UNIT_PARAMS;
    DefaultColWidth := Width div (High(ColLabels)+1);
    for I := 0 to High(ColLabels) do Cells[I,0] := ColLabels[I];
  end;

  // Initial button states
  SetBtnState(False);
  HasChanged := False;
end;

procedure TLidGroupDlg.SetBtnState(const State: Boolean);
//-----------------------------------------------------------------------------
//  Adjusts the Enabled state of the editing buttons.
//-----------------------------------------------------------------------------
begin
  BtnEdit.Enabled := State;
  BtnDelete.Enabled := State;
end;

procedure TLidGroupDlg.SetData(theSubcatch: TSubcatch);
//-----------------------------------------------------------------------------
//  Loads LID data for the subcatchment into the dialog's data grid.
//-----------------------------------------------------------------------------
var
  I: Integer;
  J: Integer;
  N: Integer;
  LidList: TStringList;
  LidUnit: TLidUnit;
  Data: array[0..Ulid.MAX_LID_UNIT_PARAMS] of String;
begin
  // Add name of subcatchment to dialog's title caption
  theSubcatchName := theSubcatch.ID;
  Caption := 'LID Controls for Subcatchment ' + theSubcatchName;

  // Get number of LID units in the group
  LidList := theSubcatch.Lids;
  N := LidList.Count;
  with StringGrid1 do
  begin

    // Leave room for fixed row header and one blank row in data grid
    if RowCount < N+2 then RowCount := N+2;

    // Activate the Edit & Delete buttons if grid contains LID units
    if RowCount > 2 then SetBtnState(True);

    // Add data for each LID unit to each row of the grid
    for I := 0 to N-1 do
    begin
      LidUnit := TLidUnit(LidList.Objects[I]);
      Data[0] := LidList.Strings[I];
      for J := 1 to Ulid.MAX_LID_UNIT_PARAMS do Data[J] := LidUnit.Data[J-1];
      FillGridRow(I+1, Data);
    end;
  end;
end;

procedure TLidGroupDlg.GetData(theSubcatch: TSubcatch);
//-----------------------------------------------------------------------------
//  Retrieves LID data from the dialog's data grid
//-----------------------------------------------------------------------------
var
  I: Integer;
  J: Integer;
  K: Integer;
  LidName: String;
  LidList: TStringList;
  LidUnit: TLidUnit;
begin
  // First remove existing LID units from the subcatchment
  LidList := theSubcatch.LIDs;
  Ulid.FreeLidUnits(LidList);

  // Then create an LID unit for each row of the data grid
  with StringGrid1 do
  begin
    for I := 1 to RowCount-1 do
    begin
      LidName := Cells[0,I];
      if Length(LidName) > 0 then
      begin
        LidUnit := TLidUnit.Create;
        K := High(ColLabels) + 1;
        for J := K to ColCount - 1 do LidUnit.Data[J-K] := Cells[J, I];
        LidList.AddObject(LidName, LidUnit);
      end;
    end;
  end;
end;

procedure TLidGroupDlg.BtnAddClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  Adds an LID unit to the data grid.
//-----------------------------------------------------------------------------
var
  R: Integer;
  Data: array[0..Ulid.MAX_LID_UNIT_PARAMS] of String;
  LidUsageDlg: TLidUsageDlg;
begin
  // Launch the LID Unit dialog form
  LidUsageDlg := TLidUsageDlg.Create(self);
  try
    LidUsageDlg.SubcatchName := theSubcatchName;
    if LidUsageDlg.ShowModal = mrOK then
    begin

      // Retrieve data from the form
      LidUsageDlg.Hide;
      LidUsageDlg.GetData(Data);
      with StringGrid1 do
      begin

        // Add the LID unit's data to last row of grid
        R := RowCount - 1;
        FillGridRow(R, Data);

        // Add a new row to the grid
        RowCount := RowCount + 1;
        Row := R;
        HasChanged := True;

        // Activate the Edit & Delete buttons
        SetBtnState(True);
      end;
    end;
  finally
    LidUsageDlg.Free;
  end;
end;

procedure TLidGroupDlg.BtnEditClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  Edits an LID unit listed in the data grid.
//-----------------------------------------------------------------------------
var
  J, K: Integer;
  Data: array[0..Ulid.MAX_LID_UNIT_PARAMS] of String;
  LidUsageDlg: TLidUsageDlg;
begin

  K := High(ColLabels) + 1;
  with StringGrid1 do
  begin
    Data[0] := Cells[0, Row];
    for J := K to ColCount-1 do Data[J-K+1] := Cells[J, Row];
  end;

  LidUsageDlg := TLidUsageDlg.Create(self);
  try
    LidUsageDlg.SubcatchName := theSubcatchName;
    LidUsageDlg.SetData(Data);
    if LidUsageDlg.ShowModal = mrOK then
    begin
      LidUsageDlg.Hide;
      LidUsageDlg.GetData(Data);
      FillGridRow(StringGrid1.Row, Data);
      HasChanged := LidUsageDlg.Modified;
    end;
  finally
    LidUsageDlg.Free;
  end;
end;

procedure TLidGroupDlg.BtnDeleteClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  Deletes an LID unit from the data grid.
//-----------------------------------------------------------------------------
var
  I: Integer;
  J: Integer;
begin
  with StringGrid1 do
  begin
    if Row < RowCount-1 then
      for I := Row+1 to RowCount-1 do
        for J := 0 to ColCount-1 do
          Cells[J, I-1] := Cells[J, I];
    RowCount := RowCount - 1;
    if RowCount = 2 then SetBtnState(False);
    HasChanged := True;
  end;
end;

procedure TLidGroupDlg.OkBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the OK button.
//-----------------------------------------------------------------------------
var
  I: Integer;
  TotalArea: Extended;
  TotalImperv: Extended;
  TotalPerv: Extended;
begin
  // Sum area and treated imperv. area of each LID
  TotalArea := 0;
  TotalImperv := 0;
  TotalPerv := 0;
  with StringGrid1 do
  begin
    for I := 1 to RowCount-1 do
    begin
      if Length(Cells[0,I]) > 0 then
      try
        TotalArea := TotalArea + StrToFloat(Cells[2,I]);
        TotalImperv := TotalImperv + StrToFloat(Cells[3,I]);
        TotalPerv := TotalPerv + StrToFloat(Cells[15,I]);
      finally
      end;
    end;
  end;

  // Check that totals are less than 100
  if TotalArea > 100.01 then
    Uutils.MsgDlg('Total LID area exceeds that of subcatchment.',
      mtError, [mbOK])
  else if TotalImperv > 100.01 then
    Uutils.MsgDlg('Percent of impervious area treated exceeds 100.',
      mtError, [mbOK])
  else if TotalPerv > 100.01 then
    Uutils.MsgDlg('Percent of pervious area treated exceeds 100.',
      mtError, [mbOK])
  else ModalResult := mrOk;
end;

procedure TLidGroupDlg.StringGrid1Click(Sender: TObject);
//-----------------------------------------------------------------------------
//  Updates state of editing buttons when user selects a row in the grid.
//-----------------------------------------------------------------------------
var
  State: Boolean;
begin
  with StringGrid1 do
  begin
    if Row = RowCount-1 then State := False
    else State := True;
  end;
  SetBtnState(State);
end;

procedure TLidGroupDlg.StringGrid1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
//-----------------------------------------------------------------------------
//  Interprets data grid keystrokes as editing commands.
//-----------------------------------------------------------------------------
begin
  if Key = VK_Insert then BtnAddClick(Sender)
  else if (StringGrid1.Row < StringGrid1.RowCount-1) then
  begin
    if      Key = VK_Return then BtnEditClick(Sender)
    else if Key = VK_Delete then BtnDeleteClick(Sender);
  end;
end;

procedure TLidGroupDlg.FillGridRow(const I: Integer; Data: array of String);
//-----------------------------------------------------------------------------
//  Fills row I of the grid with data from an LID unit.
//-----------------------------------------------------------------------------
var
  J: Integer;
  K: Integer;
  P: Integer;
begin
  K := High(ColLabels) + 1;
  with StringGrid1 do
  begin
    // Copy unit's parameters to invisible columns of grid
    for J := K to ColCount-1 do Cells[J, I] := Data[J-K+1];

    // Place LID name in column 0
    Cells[0,I] := Data[0];

    // Place LID Type in column 1
    J := Project.Lists[LID].IndexOf(Data[0]);
    if J >= 0 then
    begin
      P := TLid(Project.Lists[LID].Objects[J]).ProcessType;
      Cells[1,I] := Ulid.ProcessTypesMedium[P];
    end
    else Cells[1,I] := 'N/A';

    // Place percent of area used by LID in column 2
    Cells[2, I] := Ulid.GetPcntArea(StrToInt(Data[1]), Data[2]);

    // Place percent treated & report file name in columns 3-4
    Cells[3, I] := Data[5];
    Cells[4, I] := Data[10];
    Cells[5, I] := ExtractFileName(Data[7]);
  end;
end;

procedure TLidGroupDlg.HelpBtnClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 213330);
end;

procedure TLidGroupDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then HelpBtnClick(Sender);
end;

end.
