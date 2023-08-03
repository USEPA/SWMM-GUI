unit Dprefers;

{-------------------------------------------------------------------}
{                    Unit:    Dprefers.pas                          }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    05/02/22     (5.2.1)                  }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form unit for setting program preferences.               }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Spin, StdCtrls, ComCtrls, FileCtrl, ExtCtrls, CheckLst, Buttons, Vcl.Themes,
  UpDnEdit, Uglobals, Uutils;

type
  TPreferencesForm = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    BtnOK: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label6: TLabel;
    NodeVarBox: TComboBox;
    LinkVarBox: TComboBox;
    SubcatchVarBox: TComboBox;
    SubcatchVarSpin: TUpDnEditBox;
    NodeVarSpin: TUpDnEditBox;
    LinkVarSpin: TUpDnEditBox;
    Label5: TLabel;
    StylesCombo: TComboBox;
    CheckBox9: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure SubcatchVarBoxChange(Sender: TObject);
    procedure LinkVarBoxChange(Sender: TObject);
    procedure NodeVarBoxChange(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure SubcatchVarSpinEditBoxChange(Sender: TObject);
    procedure NodeVarSpinEditBoxChange(Sender: TObject);
    procedure LinkVarSpinEditBoxChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    SubcatchDigits: array[0..SUBCATCHVIEWS] of Integer;
    NodeDigits: array[0..NODEVIEWS] of Integer;
    LinkDigits: array[0..LINKVIEWS] of Integer;
    function SetPreferences: Boolean;
  public
    { Public declarations }
    function RetrieveStyleName: String;
  end;

//var
//  PreferencesForm: TPreferencesForm;

implementation

{$R *.DFM}

uses
  Fmain;

const
  MSG_NO_DIRECTORY = ' - directory does not exist.';
  MSG_DECIMALS = 'Select number of decimal places for computed results:';

  PrefersList: array[1..9] of PChar =
    ('Blinking Map Highlighter', 'Flyover Map Labeling', 'Confirm Deletions',
     'Automatic Backup File', 'Tab Delimited Project File',
     'Report Elapsed Time by Default', 'Prompt to Save Results',
     'Show Welcome Screen at Startup', 'Clear Recent Project List');

procedure TPreferencesForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
// OnCreate handler for form.
//-----------------------------------------------------------------------------
var
  I: Integer;
  J: Integer;
  S: String;
begin
  // Assign OnChange handlers to the TUpDnEditBox controls
  SubcatchVarSpin.EditBox.OnChange := SubcatchVarSpinEditBoxChange;
  NodeVarSpin.EditBox.OnChange := NodeVarSpinEditBoxChange;
  LinkVarSpin.EditBox.OnChange := LinkVarSpinEditBoxChange;

  // Initialize general preferences
  for I := 1 to High(PrefersList) do
  begin
      with FindComponent('CheckBox' + IntToStr(I)) as TCheckBox do
        Caption := PrefersList[I];
  end;
  CheckBox1.Checked := Uglobals.Blinking;
  CheckBox2.Checked := Uglobals.FlyOvers;
  CheckBox3.Checked := Uglobals.ConfirmDelete;
  CheckBox4.Checked := Uglobals.AutoBackup;
  CheckBox5.Checked := Uglobals.TabDelimited;
  CheckBox6.Checked := Uglobals.RptElapsedTime;
  CheckBox7.Checked := not Uglobals.AutoSave;
  CheckBox8.Checked := Uglobals.ShowStartPage;

  // Assign UI styles to StylesCombo box
  StylesCombo.Items.BeginUpdate;
  try
    StylesCombo.Items.Clear;
    for S in TStyleManager.StyleNames do
       StylesCombo.Items.Add(s);
    StylesCombo.Sorted := True;
    // Select the style that's currently in use in the combobox
    StylesCombo.ItemIndex :=
      StylesCombo.Items.IndexOf(TStyleManager.ActiveStyle.Name);
  finally
    StylesCombo.Items.EndUpdate;
  end;

  // Assign items to node & link variable combo boxes
  J := 0;
  for I := SUBCATCHOUTVAR1 to SUBCATCHVIEWS do
  begin
    SubcatchVarBox.Items.Add(SubcatchVariable[I].Name);
    SubcatchDigits[J] := SubcatchUnits[I].Digits;
    Inc(J);
  end;
  J := 0;
  for I := NODEOUTVAR1 to NODEVIEWS do
  begin
    NodeVarBox.Items.Add(NodeVariable[I].Name);
    NodeDigits[J] := NodeUnits[I].Digits;
    Inc(J);
  end;
  J := 0;
  for I := LINKOUTVAR1 to LINKVIEWS do
  begin
    LinkVarBox.Items.Add(LinkVariable[I].Name);
    LinkDigits[J] := LinkUnits[I].Digits;
    Inc(J);
  end;
  SubcatchVarBox.ItemIndex := 0;
  SubcatchVarSpin.Spinner.Position := SubcatchDigits[0];
  NodeVarBox.ItemIndex := 0;
  NodeVarSpin.Spinner.Position := NodeDigits[0];
  LinkVarBox.ItemIndex := 0;
  LinkVarSpin.Spinner.Position := LinkDigits[0];

  Label6.Caption := MSG_DECIMALS;
  PageControl1.ActivePage := TabSheet1;
end;

procedure TPreferencesForm.SubcatchVarSpinEditBoxChange(Sender: TObject);
//-----------------------------------------------------------------------------
// OnChange handler for SpinEdit control that sets the number of
// decimal places for a subcatchment variable.
//-----------------------------------------------------------------------------
begin
  SubcatchDigits[SubcatchVarBox.ItemIndex] := SubcatchVarSpin.Spinner.Position;
end;

procedure TPreferencesForm.NodeVarSpinEditBoxChange(Sender: TObject);
//-----------------------------------------------------------------------------
// OnChange handler for SpinEdit control that sets the number of
// decimal places for a node variable.
//-----------------------------------------------------------------------------
begin
  NodeDigits[NodeVarBox.ItemIndex] := NodeVarSpin.Spinner.Position;
end;

procedure TPreferencesForm.LinkVarSpinEditBoxChange(Sender: TObject);
//-----------------------------------------------------------------------------
// OnChange handler for SpinEdit control that sets the number of
// decimal places for a link variable.
//-----------------------------------------------------------------------------
begin
  LinkDigits[LinkVarBox.ItemIndex] := LinkVarSpin.Spinner.Position;
end;

procedure TPreferencesForm.SubcatchVarBoxChange(Sender: TObject);
//-----------------------------------------------------------------------------
// OnChange handler for the combo box that selects a subcatchment variable.
//-----------------------------------------------------------------------------
begin
  SubcatchVarSpin.Spinner.Position := SubcatchDigits[SubcatchVarBox.ItemIndex];
end;

procedure TPreferencesForm.NodeVarBoxChange(Sender: TObject);
//-----------------------------------------------------------------------------
// OnChange handler for the combo box that selects a node variable.
//-----------------------------------------------------------------------------
begin
  NodeVarSpin.Spinner.Position := NodeDigits[NodeVarBox.ItemIndex];
end;

procedure TPreferencesForm.LinkVarBoxChange(Sender: TObject);
//-----------------------------------------------------------------------------
// OnChange handler for the combo box that selects a link variable.
//-----------------------------------------------------------------------------
begin
  LinkVarSpin.Spinner.Position := LinkDigits[LinkVarBox.ItemIndex];
end;

procedure TPreferencesForm.BtnOKClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for OK button.
//-----------------------------------------------------------------------------
begin
  if SetPreferences then ModalResult := mrOK;
end;

procedure TPreferencesForm.BtnCancelClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for Cancel button.
//-----------------------------------------------------------------------------
begin
  ModalResult := mrCancel;
end;

function TPreferencesForm.SetPreferences: Boolean;
//-----------------------------------------------------------------------------
// Transfers contents of form to program preference variables.
//-----------------------------------------------------------------------------
var
  I: Integer;
  J: Integer;
begin
  // Save preferences to their respective global variables.
  Uglobals.Blinking       := CheckBox1.Checked;
  Uglobals.FlyOvers       := CheckBox2.Checked;
  Uglobals.ConfirmDelete  := CheckBox3.Checked;
  Uglobals.AutoBackup     := CheckBox4.Checked;
  Uglobals.TabDelimited   := CheckBox5.Checked;
  Uglobals.RptElapsedTime := CheckBox6.Checked;
  Uglobals.AutoSave       := not CheckBox7.Checked;
  Uglobals.ShowStartPage  := CheckBox8.Checked;

  // Clear Most Recently Used file list
  if CheckBox9.Checked then
    for I := 0 to Uglobals.MRUList.Count-1 do
      Uglobals.MRUList[I] := '';

  // Save number of display decimal places for output view variables
  J := 0;
  for I := SUBCATCHOUTVAR1 to SUBCATCHVIEWS do
  begin
    SubcatchUnits[I].Digits := SubcatchDigits[J];
    Inc(J);
  end;
  J := 0;
  for I := NODEOUTVAR1 to NODEVIEWS do
  begin
    NodeUnits[I].Digits := NodeDigits[J];
    Inc(J);
  end;
  J := 0;
  for I := LINKOUTVAR1 to LINKVIEWS do
  begin
    LinkUnits[I].Digits := LinkDigits[J];
    Inc(J);
  end;
  Result := True;
end;

function TPreferencesForm.RetrieveStyleName: String;
begin
  Result := StylesCombo.Text;
end;

procedure TPreferencesForm.BtnHelpClick(Sender: TObject);
begin
   with PageControl1 do
     if ActivePage = TabSheet1 then
       Application.HelpCommand(HELP_CONTEXT, 210910)
     else
       Application.HelpCommand(HELP_CONTEXT, 210920);
end;

procedure TPreferencesForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then BtnHelpClick(Sender);
end;

end.
