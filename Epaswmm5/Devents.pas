unit Devents;

{-------------------------------------------------------------------}
{                    Unit:    Devents.pas                           }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21     (5.2.0)                  }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form unit that selects the start and end dates of        }
{   hydraulic event periods.                                        }
{-------------------------------------------------------------------}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Uutils, Uglobals,
  Vcl.ComCtrls, Vcl.ExtCtrls, StrUtils, DateUtils;

type
  TEventsForm = class(TForm)
    OkBtn: TButton;
    CancelBtn: TButton;
    Label1: TLabel;
    Label2: TLabel;
    HelpBtn: TButton;
    ListView1: TListView;
    GroupBox1: TGroupBox;
    Label5: TLabel;
    StartDatePicker: TDateTimePicker;
    Label7: TLabel;
    EndDatePicker: TDateTimePicker;
    Label6: TLabel;
    StartTimePicker: TDateTimePicker;
    Label8: TLabel;
    EndTimePicker: TDateTimePicker;
    ReplaceBtn: TButton;
    DeleteBtn: TButton;
    DeleteAllBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure DeleteAllBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ReplaceBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    procedure AddNewItem;
    function Validate: Boolean;
  public
    { Public declarations }
    HasChanged: Boolean;
    procedure SetData(Events: TStringList);
    procedure GetData(Events: TStringList);
  end;

var
  EventsForm: TEventsForm;

implementation

{$R *.dfm}

procedure TEventsForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnCreate handler.
//-----------------------------------------------------------------------------
var
  DateFmt: String;
begin
  // Set format for date picker controls
  DateFmt := 'MM' + MyFormatSettings.DateSeparator + 'dd' +
    MyFormatSettings.DateSeparator + 'yyyy';
  StartDatePicker.Format := DateFmt;
  EndDatePicker.Format := DateFmt;
  StartTimePicker.Format := 'HH:mm';
  EndTimePicker.Format := 'HH:mm';

  // Add a blank event to the list view's items
  AddNewItem;
 end;

procedure TEventsForm.FormShow(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnShow handler.
//-----------------------------------------------------------------------------
begin
  with ListView1 do
  begin
    Selected := Items[0];
    Items[0].Checked := True;
  end;
end;

procedure TEventsForm.DeleteAllBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the Delete All button.
//-----------------------------------------------------------------------------
begin
  ListView1.Items.Clear;
  AddNewItem;
  ListView1.ItemIndex := ListView1.Items.Count-1;
  HasChanged := True;
end;

procedure TEventsForm.DeleteBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the Delete Event button.
//-----------------------------------------------------------------------------
begin
  if ListView1.Selected = nil then exit;
  ListView1.DeleteSelected;
  if ListView1.Items.Count = 0 then AddNewItem;
  ListView1.ItemIndex := ListView1.Items.Count-1;
  HasChanged := True;
end;

procedure TEventsForm.OkBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the OK button.
//-----------------------------------------------------------------------------
begin
  if Validate then ModalResult := mrOK;
end;

procedure TEventsForm.ReplaceBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the Replace Event button.
//-----------------------------------------------------------------------------
var
  I: Integer;
  H: Integer;
  M: Integer;
begin
  if ListView1.Selected = nil then exit;
  I := ListView1.Selected.Index;
  if I >= 0 then with ListView1.Items[I] do
  begin
    SubItems[0] := DateToStr(StartDatePicker.DateTime);
    H := HourOf(TimeOf(StartTimePicker.Time));
    M := MinuteOf(TimeOf(StartTimePicker.Time));
    SubItems[1] := Format('%.2d:%.2d', [H,M]);
    SubItems[2] := DateToStr(EndDatePicker.DateTime);
    H := HourOf(TimeOf(EndTimePicker.Time));
    M := MinuteOf(TimeOf(EndTimePicker.Time));
    SubItems[3] := Format('%.2d:%.2d', [H,M]);
  end;
  if I = ListView1.Items.Count-1 then
  begin
    AddNewItem;
    ListView1.ItemIndex := ListView1.Items.Count-1;
  end;
  HasChanged := True;
end;

procedure TEventsForm.ListView1SelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
//-----------------------------------------------------------------------------
//  OnSelectItem handler for the ListView control.
//-----------------------------------------------------------------------------
begin
  if ListView1.Selected = nil then exit;
  with ListView1 do
  begin
    if Selected.Index = Items.Count-1 then exit;
  end;
  with Item do
  begin
    try
      StartDatePicker.DateTime := StrToDate(SubItems[0]);
      StartTimePicker.DateTime := StrToTime(SubItems[1]);
      EndDatePicker.DateTime := StrToDate(SubItems[2]);
      EndTimePicker.DateTime := StrToTime(SubItems[3]);
    except
      On EConvertError do exit;
    end;
  end;
end;

procedure TEventsForm.SetData(Events: TStringList);
//-----------------------------------------------------------------------------
//  Loads current set of hydraulic events into the form's ListView.
//-----------------------------------------------------------------------------
var
  I        : Integer;
  J        : Integer;
  N        : Integer;
  Ntoks    : Integer;
  TokList  : TStringlist;
  Item     : TListItem;
  UseItem  : Boolean;
  S        : String;
begin
  N := Events.Count;
  if N = 0 then exit;
  TokList := TStringList.Create;
  try
    for I := 0 to N-1 do
    begin
      UseItem := True;
      S := Trim(Events[I]);
      if LeftStr(S,1) = ';' then
      begin
        S := RightStr(S, Length(S)-1);
        UseItem := False;
      end;
      Uutils.Tokenize(S, TokList, Ntoks);
      if Ntoks < 4 then continue;
      Item := ListView1.Items[I];
      for J := 0 to 3 do Item.SubItems[J] := TokList[J];
      Item.Checked := UseItem;
      AddNewItem;
    end;
  finally
    TokList.Free;
  end;
  with ListView1 do Selected := Items[0];
  HasChanged := False;
end;

procedure TEventsForm.GetData(Events: TStringList);
//-----------------------------------------------------------------------------
//  Replaces the current set of hydraulic events with those in the ListView.
//-----------------------------------------------------------------------------
var
  I: Integer;
  J: Integer;
  N: Integer;
  Item: TListItem;
  S: String;
begin
  Events.Clear;
  N := ListView1.Items.Count;
  if N < 2 then exit;
  for I := 0 to N-2 do
  begin
    Item := ListView1.Items[I];
    if not Item.Checked then S := ';' else S := '';
    for J := 0 to 3 do S := S + '  ' + Item.SubItems[J];
    Events.Add(S);
  end;
end;

procedure TEventsForm.AddNewItem;
//-----------------------------------------------------------------------------
//  Adds a new blank row to the events list.
//-----------------------------------------------------------------------------
var
  I: Integer;
  Item: TListItem;
begin
  Item := ListView1.Items.Add;
  Item.Checked := True;
  Item.Caption := '';
  for I := 0 to 3 do Item.SubItems.Add('');
end;

function TEventsForm.Validate: Boolean;
//-----------------------------------------------------------------------------
//  Checks for valid dates and times in the list view control.
//-----------------------------------------------------------------------------
var
  I: Integer;
  Item: TListItem;
  aDate1, aDate2: TDateTime;
begin
  Result := False;

  // Examine each event (last event is a blank one)
  for I := 0 to ListView1.Items.Count - 2 do
  begin
    Item := ListView1.Items[I];
    with Item do
    begin
      // Check that event has valid date/time values
      try
        aDate1 := StrToDate(SubItems[0]) + StrToTime(SubItems[1]);
        aDate2 := StrToDate(SubItems[2]) + StrToTime(SubItems[3]);
      except
        On EConvertError do
        begin
          with ListView1 do Selected := Items[I];
          Uutils.MsgDlg('This event contains an invalid date/time.',
                         mtError, [mbOK]);
          exit;
        end;
      end;

      // Check that start date precedes end date
      if aDate1 >= aDate2 then
      begin
        with ListView1 do Selected := Items[I];
        Uutils.MsgDlg('End date precedes start date for this event.',
                       mtError, [mbOK]);
        exit;
      end;
    end;
  end;
  Result := True;
end;

procedure TEventsForm.HelpBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the Help button.
//-----------------------------------------------------------------------------
begin
  Application.HelpCommand(HELP_CONTEXT, 213540);
end;

procedure TEventsForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then HelpBtnClick(Sender);
end;

end.
