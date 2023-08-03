unit Dunithyd;

{-------------------------------------------------------------------}
{                    Unit:    Dunithyd.pas                          }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form unit for editing the properties of an RDII Unit     }
{   Hydrograph group.                                               }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, ClipBrd, Menus, CheckLst, ExtCtrls,
  GridEdit, NumEdit, Uglobals, Uutils, Uproject, ComCtrls;

type
  TUnitHydForm = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    Label1: TLabel;
    Label2: TLabel;
    UHName: TEdit;
    RGname: TComboBox;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    UHGridEdit: TGridEditFrame;
    IAGridEdit: TGridEditFrame;
    MonthsCombo: TComboBox;
    Bevel1: TBevel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Panel2: TPanel;
    IALabel1: TLabel;
    IALabel2: TLabel;
    IALabel3: TLabel;

    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure UHDataChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure UHNameKeyPress(Sender: TObject; var Key: Char);
    procedure MonthsComboClick(Sender: TObject);

  private
    { Private declarations }
    UHIndex: Integer;                               // Unit hydrograph index
    UHMonth: Integer;                               // Month being edited
    UHParams: array[0..12, 1..3, 1..3] of String;   // UH params for each month
    IAParams: array[0..12, 1..3, 1..3] of String;   // IA params for each month
    function ValidateData: Boolean;
  public
    { Public declarations }
    Modified: Boolean;
    procedure SetData(const Index: Integer; aUnitHyd: THydrograph);
    procedure GetData(var S: String; aUnitHyd: THydrograph);
  end;

//var
//  UnitHydForm: TUnitHydForm;

implementation

{$R *.dfm}

const
  MSG_NO_ID = 'No hydrograph name supplied.';
  MSG_DUPLICATE_ID = 'Hydrograph name already in use.';
  MSG_NO_RAINGAGE = 'No rain gage name supplied.';

  Months: array[0..12] of String =
    ('All Months', 'January', 'February', 'March', 'April', 'May', 'June',
     'July', 'August', 'September', 'October', 'November', 'December');
  RowLabels: array[0..3] of String =
    ('Response', 'Short-Term', 'Medium-Term', 'Long-Term');
  ColLabels: array[1..3] of String = ('R', 'T', 'K');
  IAColLabels: array[1..3] of String = ('Dmax', 'Drec', 'Do');
  IAUnits: array[1..3, 0..1] of String =
  (('(inches)', '(mm)'), ('(in/day)', '(mm/day)'), ('(inches)', '(mm)'));

procedure TUnitHydForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
// OnCreate handler for form.
//-----------------------------------------------------------------------------
var
  I: Integer;
  W: Integer;
begin
  // Set up the Unit Hydrograph Grid Editor.
  with UHGridEdit.Grid do
  begin
    W := (100 * Screen.PixelsPerInch) div 96;
    DefaultColWidth := ((ClientWidth - W) div 3) - 2;
    ColWidths[0] := W;
    DefaultRowHeight := UHGridEdit.EditBox.Height;
    Height := RowCount * (DefaultRowHeight+1) + 1;
    Cells[0,0] := RowLabels[0];
    Cells[0,1] := RowLabels[1];
    Cells[0,2] := RowLabels[2];
    Cells[0,3] := RowLabels[3];
    Cells[1,0] := ColLabels[1];
    Cells[2,0] := ColLabels[2];
    Cells[3,0] := ColLabels[3];
  end;
  UHGridEdit.CenterHeaders := True;

  // Populate months check list box control with names of months
  with MonthsCombo do
  begin
    for I := 0 to 12 do Items.Add(Months[I]);
    ItemIndex := 0;
  end;

  // Set up the Initial Abstraction Grid Editor
  with IAGridEdit.Grid do
  begin
    DefaultColWidth := ((ClientWidth - W) div 3) - 2;
    ColWidths[0] := W;
    DefaultRowHeight := IAGridEdit.EditBox.Height;
    Height := RowCount * (DefaultRowHeight+1) + 1;
    Cells[0,0] := RowLabels[0];
    Cells[0,1] := RowLabels[1];
    Cells[0,2] := RowLabels[2];
    Cells[0,3] := RowLabels[3];
    Cells[1,0] := IAColLabels[1];
    Cells[2,0] := IAColLabels[2];
    Cells[3,0] := IAColLabels[3];
  end;
  IAGridEdit.CenterHeaders := True;
  IALabel1.Caption := IALabel1.Caption + IAUnits[1][Ord(Uglobals.UnitSystem)];
  IALabel2.Caption := IALabel2.Caption + IAUnits[2][Ord(Uglobals.UnitSystem)];
  IALabel3.Caption := IALabel3.Caption + IAUnits[3][Ord(Uglobals.UnitSystem)];
end;

procedure TUnitHydForm.SetData(const Index: Integer; aUnitHyd: THydrograph);
//-----------------------------------------------------------------------------
//  Loads data from unit hydrograph object aUnitHyd whose index is Index
//  into the form's controls.
//-----------------------------------------------------------------------------
var
  I, J, K: Integer;
begin
  // Save unit hydrograph index
  UHIndex := Index;
  UHMonth := -1;

  // Get name associated with current hydrograph
  if Index >= 0 then  UHName.Text := Project.Lists[HYDROGRAPH].Strings[Index];

  // Assign existing rain gage names to the rain gage name combo box
  RGname.Items := Project.Lists[RAINGAGE];

  with aUnitHyd do
  begin

    // Set rain gage name combo box's text to UH's rain gage
    RGname.Text := Raingage;

    // For each month (including All Months index 0)
    for I := 0 to 12 do
    begin

      // Load the UH parameters into the UHParams array
      for J := 1 to 3 do
      begin
        for K := 1 to 3 do
        begin
          if  Length(Trim(Params[I,J,K])) > 0 then
          begin
            MonthsCombo.Items[I] := Months[I] + ' (*)';
            if UHMonth = -1 then UHMonth := I;
          end;
          UHParams[I,J,K] := Params[I,J,K];
          IAParams[I,J,K] := InitAbs[I,J,K];
        end;
      end;
    end;
  end;

  // The month selected in the month combo box is the first one with data
  if UHMonth = -1 then UHMonth := 0;
  MonthsCombo.ItemIndex := UHMonth;

  // Load the UH grid with parameters for the selected month
  with UHGridEdit.Grid do
  begin
    for I := 1 to 3 do
    begin
      for J := 1 to 3 do Cells[J,I] := UHParams[UHMonth,J,I];
    end;
  end;
  UHGridEdit.AllowInsert := False;
  UHGridEdit.Modified := False;
  UHGridEdit.EditBox.Style := esPosNumber;

  // Load the IA grid with parameters for the selected month
  with IAGridEdit.Grid do
  begin
    for I := 1 to 3 do
    begin
      for J := 1 to 3 do Cells[J,I] := IAParams[UHMonth,J,I];
    end;
  end;
  IAGridEdit.AllowInsert := False;
  IAGridEdit.Modified := False;
  IAGridEdit.EditBox.Style := esPosNumber;
  Modified := False;
end;

procedure TUnitHydForm.GetData(var S: String; aUnitHyd: THydrograph);
//-----------------------------------------------------------------------------
//  Retrieves data from the form to the unit hydrograph object aUnitHyd
//  whose edited ID is S.
//-----------------------------------------------------------------------------
var
  I: Integer;
  J: Integer;
  M: Integer;
begin
  S := UHName.Text;
  if not Modified then Exit;

  //Save current entries being edited in the UH grid
  with UHGridEdit.Grid do
  begin
    for I := 1 to 3 do
    begin
      for J := 1 to 3 do UHParams[UHMonth,J,I] := Cells[J,I];
    end;
  end;

  //Save current entries being edited in the IA grid
  with IAGridEdit.Grid do
  begin
    for I := 1 to 3 do
    begin
      for J := 1 to 3 do IAParams[UHMonth,J,I] := Cells[J,I];
    end;
  end;

  //Load UH and IA values for each month into the aUnitHyd object
  with aUnitHyd do
  begin
    Raingage := RGname.Text;
    for M := 0 to 12 do
    begin
      for I := 1 to 3 do
      begin
        for J := 1 to 3 do
        begin
          Params[M,J,I] := UHParams[M,J,I];
          InitAbs[M,J,I] := IAParams[M,J,I];
        end;
      end;
    end;
  end;
end;

procedure TUnitHydForm.UHNameKeyPress(Sender: TObject; var Key: Char);
//-----------------------------------------------------------------------------
//  OnKeyPress handler for the UHName edit box.
//-----------------------------------------------------------------------------
begin
  if (Key = ' ') or (Key = '"') or (Key = ';') then Key := #0
  else with UHName as TEdit do
  begin
    if (Length(Text) = 0) or (SelStart = 0) then
      if Key = '[' then Key := #0;
  end;
end;

procedure TUnitHydForm.MonthsComboClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the month combo box.
//-----------------------------------------------------------------------------
var
  I, J: Integer;
  HasData: Boolean;
begin
  // Transfer UH parameters from the GridEdit control to the UHParams array
  HasData := False;
  with UHGridEdit.Grid do
  begin
    for I:= 1 to 3 do
    begin
      for J := 1 to 3 do
      begin
        if Length(Trim(Cells[I,J])) > 0 then HasData := True;
        UHParams[UHMonth,I,J] := Cells[I,J];
      end;
    end;
  end;

  // Transfer IA parameters
  with IAGridEdit.Grid do
  begin
    for I:= 1 to 3 do
    begin
      for J := 1 to 3 do
      begin
        IAParams[UHMonth,J,I] := Cells[J,I];
      end;
    end;
  end;

  // Update the current month to the one that was selected
  if MonthsCombo.ItemIndex <> UHMonth then
  begin
    if HasData then MonthsCombo.Items[UHMonth] := Months[UHMonth] + ' (*)'
    else MonthsCombo.Items[UHMonth] := Months[UHMonth];
  end;
  UHMonth := MonthsCombo.ItemIndex;

  // Copy the contents of the UHParams array for the current month
  // into the GridEdit control
  with UHGridEdit.Grid do
  begin
    for I := 1 to 3 do
    begin
      for J := 1 to 3 do Cells[J,I] := UHParams[UHMonth,J,I];
    end;
  end;

  // Copy IAParams into IA Grid
  with IAGridEdit.Grid do
  begin
    for I := 1 to 3 do
    begin
      for J := 1 to 3 do Cells[J,I] := IAParams[UHMonth,J,I];
    end;
  end;
end;

procedure TUnitHydForm.OKBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the OK button.
//-----------------------------------------------------------------------------
begin
  // Check for valid data
  if not Modified then Modified := UHGridEdit.Modified;
  if not Modified then Modified := IAGridEdit.Modified;
  if not ValidateData then ModalResult := mrNone
  else ModalResult := mrOK;
end;

function TUnitHydForm.ValidateData: Boolean;
//-----------------------------------------------------------------------------
//  Validates data entered into form.
//-----------------------------------------------------------------------------
var
  S : String;
  I : Integer;
begin
  // Check for no hydrograph name
  S := Trim(UHname.Text);
  if (Length(S) = 0) then
  begin
    Uutils.MsgDlg(MSG_NO_ID, mtError, [mbOK]);
    UHname.SetFocus;
    Result := False;
    Exit;
  end;

  // Check for duplicate hydrograph name
  I := Project.Lists[HYDROGRAPH].IndexOf(S);
  if (I >= 0) and (I <> UHIndex) then
  begin
    Uutils.MsgDlg(MSG_DUPLICATE_ID, mtError, [mbOK]);
    UHName.SetFocus;
    Result := False;
    Exit;
  end;

  // Check for no rain gage
  S := Trim(RGname.Text);
  if Length(S) = 0 then
  begin
    Uutils.MsgDlg(MSG_NO_RAINGAGE, mtError, [mbOK]);
    RGname.SetFocus;
    Result := False;
    Exit;
  end;
  Result := True;
end;

procedure TUnitHydForm.UHDataChange(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnChange handler shared by the form's controls.
//-----------------------------------------------------------------------------
begin
  Modified := True;
end;

procedure TUnitHydForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
//-----------------------------------------------------------------------------
//  Brings up context-sensitive Help when the F1 key is pressed.
//  (Form's KeyPreview property was set to True).
//-----------------------------------------------------------------------------
begin
  if Key = VK_F1 then HelpBtnClick(Sender);
end;

procedure TUnitHydForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 212260);
end;

end.
