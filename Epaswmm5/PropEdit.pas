//********************************************************************
//  PropEdit.Pas
//
//  Property Editor Component for Delphi
//
//  Version 1.5
//  Written by L. Rossman
//
//  This is a TCustomPanel descendant that is used for editing an
//  array of property values displayed in a 2-column grid. One
//  column displays the property names and the other their values.
//  All property values are stored and displayed as string values.
//
//  Information about each property is contained in a TPropRecord
//  structure, which consists of the following items:
//    Name   -- a string containing the property's name
//    Style  -- one of the following editing styles:
//              esHeading   (displays only the property name as a heading)
//              esReadOnly  (cannot be edited)
//              esEdit      (edited in an Edit control)
//              esComboList (edited in a csDropDownList ComboBox)
//              esComboEdit (edited in a csDropDown ComboBox)
//              esButton    (edited via an OnClick event handler)
//    Mask   -- an edit mask that applies when Style is esEdit:
//              emNone      (any character is accepted)
//              emNumber    (text must be a number)
//              emPosNumber (text must be a number >= 0)
//              emNoSpace   (text cannot have any spaces in it)
//    Length -- maximum length of text (applies to Style esEdit)
//    List   -- string that contains a list of possible choices for
//              esComboList or esComboEdit styles, where each item is
//              separated by #13.
//
//  The editor is populated via the SetProps method which takes
//  as arguments an array of property records, a stringlist with
//  values for each property, and an integer index representing
//  the property to highlight first in the editor.
//
//  The OnValidate event can be used to validate and record the
//  property values immediately after they are edited.
//
//********************************************************************

unit PropEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Math,
  ExtCtrls, StdCtrls, ComCtrls, Grids, System.Types, System.UITypes, Vcl.Themes;

type
  // Types of edit field styles
  TEditStyle = (esHeading, esReadOnly, esEdit, esComboList, esComboEdit,
                esButton);

  // Types of restrictions on an esEdit entry
  TEditMask = (emNone, emNumber, emPosNumber, emNoSpace);

  // Information associated with each property
  TPropRecord = record
    Name    : String;      // property name
    Style   : TEditStyle;  // style of edit field
    Mask    : TEditMask;   // restrictions on field entry
    Length  : Integer;     // max. length of text entry
    List    : String;      // list of option choices
  end;

  // An array of Property records and a pointer to such an array
  TPropArray = Array [0..0] of TPropRecord;
  PPropArray = ^TPropArray;

  // Event procedures used to validate an edited property value
  TValidateEvent = procedure(Sender: TObject; Index: Integer;
    var S: String; var Errmsg: String; var IsValid: Boolean) of Object;

  // Event procedure launched when an esButton field is clicked
  TButtonClickEvent = procedure(Sender: TObject; Index: Integer;
    var S: String; var Modified: Boolean) of Object;

  // Event procedure launched when a new row (i.e., property) is selected
  TRowSelectEvent = procedure(Sender: TObject; aRow: LongInt) of Object;

  // Special exception class for invalid property values
  EInvalidProperty = class(Exception);

  // Declaration of the TPropEdit class
  TPropEdit = class(TCustomPanel)
  private
    { Private declarations }
    // These components are used for the editor's header
    FPanel1        : TPanel;
    FHeader1       : TPanel;
    FHeader2       : TPanel;
    FSplitter      : TSplitter;

    FGrid          : TStringGrid;         // StringGrid to display values
    FEdit          : TEdit;               // Edit control for editing values
    FCombo         : TComboBox;           // Combobox for selecting choices

    FModified      : Boolean;             // Modified flag
    FRow           : Integer;             // Current property index
    FReadOnlyColor : TColor;              // Backgnd color of read-only values
    FValueColor    : TColor;              // Foregnd color of property values
    FHeadBackColor : TColor;              // Backgnd color of heading row
    FHeadForeColor : TColor;              // Foregnd color of heading row
    FColHeading1   : String;              // Heading of property name column
    FColHeading2   : String;              // Heading of property value column
    FHeaderSplit   : Integer;             // Location of heading splitter
    FProps         : PPropArray;          // Pointer to array of properties
    FValues        : TStrings;            // Stringlist with property values
    FOnValidate    : TValidateEvent;      // Property value validation event
    FOnButtonClick : TButtonClickEvent;   // Editor button OnClick event
    FOnRowSelect   : TRowSelectEvent;     // Row selection event
    ButtonPressed  : Boolean;             // True if editor button pressed
    ButtonVisible  : Boolean;             // True if editor button visible
    VisibleRows    : Integer;             // Number of rows showing in editor
    RowHeight      : Integer;             // Row height of StringGrid display

//    DecimalChar    : Char;                // Character used for decimal point

    ComponentsCreated: Boolean;           // True if sub-components created
    EditMask       : TEditMask;           // Edit mask for current property
    procedure CreateComponents;
    procedure DrawButton(aCanvas: TCanvas; aRect: TRect);
    procedure EditProperty(CurCol, CurRow: LongInt; Key: Char);
    procedure SetDropDownWidth;
    procedure SetEditBounds(aControl : TWinControl);
    procedure GoButtonClick(aRow: Integer);
    function  GoValidate(S: String): Boolean;
  protected
    { Protected declarations }
    procedure Resize; override;
    procedure ResizeGrid(Sender: TObject);

    procedure EditKeyPress(Sender: TObject; var Key: Char);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditExit(Sender: TObject);

    procedure ComboChange(Sender: TObject);
    procedure ComboDblClick(Sender: TObject);

    procedure GridKeyPress(Sender: TObject; var Key: Char);
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GridClick(Sender: TObject);
    procedure GridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GridDrawCell(Sender: TObject; vCol, vRow: Longint;
      Rect: TRect; State: TGridDrawState);
    procedure GridTopLeftChanged(Sender: TObject);
    procedure GridSelectCell(Sender: TObject; ACol, ARow: Longint;
      var CanSelect: Boolean);

    procedure SetReadOnlyColor(Value: TColor);
    procedure SetValueColor(Value: TColor);
    procedure SetHeadBackColor(Value: TColor);
    procedure SetHeadForeColor(Value: TColor);
    procedure SetColHeading1(Value: String);
    procedure SetColHeading2(Value: String);
    procedure SetHeaderSplit(Value: Integer);
    procedure SplitterMoved(Sender: TObject);

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    { Published declarations }
    procedure SetProp(Index: Integer; Value: String);
    function  GetProp(Index: Integer): String;
    procedure SetProps(var PropArray: array of TPropRecord; Values: TStrings);
    procedure GetProps(Plist, Vlist: TStringlist);
    procedure Edit;
    procedure EndEditing;
    function  IsValid: Boolean;
    property  Modified: Boolean read FModified;
    property  Row: Integer read FRow;
    property  ReadOnlyColor: TColor read FReadOnlyColor write SetReadOnlyColor;
    property  ValueColor: TColor read FValueColor write SetValueColor;
    property  HeadBackColor: TColor read FHeadBackColor write SetHeadBackColor;
    property  HeadForeColor: TColor read FHeadForeColor write SetHeadForeColor;
    property  ColHeading1: String read FColHeading1 write SetColHeading1;
    property  ColHeading2: String read FColHeading2 write SetColHeading2;
    property  HeaderSplit: Integer read FHeaderSplit write SetHeaderSplit;
    property  OnValidate: TValidateEvent read FOnValidate write FOnValidate;
    property  OnButtonClick: TButtonClickEvent read FOnButtonClick
                 write FOnButtonClick;
    property  OnRowSelect: TRowSelectEvent read FOnRowSelect write FOnRowSelect;
    property  Align;
    property  BevelInner default bvNone;
    property  BevelOuter default bvLowered;
    property  BevelWidth default 1;
    property  BorderStyle default bsNone;
    property  BorderWidth default 0;
    property  Ctl3D default True;
    property  Font;
    property  Height default 100;
    property  Left;
    property  ParentFont;
    property  Top;
    property  Visible;
    property  Width default 100;
    property  OnClick;
    property  OnDblClick;
    property  OnEnter;
    property  OnExit;
    property  OnKeyDown;
    property  OnKeyPress;
    property  OnKeyUp;
  end;

implementation


constructor TPropEdit.Create(AOwner:TComponent);
//-----------------------------------------------------------------------------
// Component constructor
//-----------------------------------------------------------------------------
begin
  inherited Create(AOwner);
  Width := 100;
  Height := 100;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  BevelKind := bkNone;
  BevelWidth := 1;
  BorderStyle := bsNone; //bsSingle;
  BorderWidth := 0;
  Ctl3D := False; //True;
  ParentColor := False;
  Color := clBtnFace;
  FReadOnlyColor := StyleServices.GetStyleColor(scPanel);
  FValueColor := clNavy;
  FHeadBackColor := StyleServices.GetStyleColor(scPanel);
  FHeadForeColor := clBlack;
  FColHeading1 := 'Property';
  FColHeading2 := 'Value';
  FHeaderSplit := 50;
  FModified := False;
  FRow := 0;
  ButtonPressed := False;
  ButtonVisible := False;
  ComponentsCreated := False;

  //DecimalChar := DecimalSeparator;
end;


destructor TPropEdit.Destroy;
//-----------------------------------------------------------------------------
// Component destructor. The sub-components must be freed in the order listed.
//-----------------------------------------------------------------------------
begin
  ComponentsCreated := False;
  inherited Destroy;
end;


procedure TPropEdit.CreateComponents;
//-----------------------------------------------------------------------------
// Creates the sub-components that populate the PropEdit component.
//-----------------------------------------------------------------------------
begin
  // FGrid displays properties & values
  FGrid := TStringGrid.Create(self);
  FGrid.Parent := self;
  with FGrid do
  begin
    BorderStyle := bsNone;
    ColCount := 2;
    Ctl3D := False;
    Color := clWindow; //clBtnFace;
    Options := Options - [goRangeSelect] + [goThumbTracking];
    DrawingStyle := gdsClassic;
    FixedCols := 0;
    FixedRows := 0;
    RowCount := 1;
    ScrollBars := ssVertical;
    Visible := True;
    OnKeypress := GridKeyPress;
    OnKeyDown := GridKeyDown;
    OnClick := GridClick;
    OnMouseDown := GridMouseDown;
    OnMouseUp := GridMouseUp;
    OnDrawCell := GridDrawCell;
    OnTopLeftChanged := GridTopLeftChanged;
    OnSelectCell := GridSelectCell;
  end;

  // Panel containing grid's headers
  FPanel1 := TPanel.Create(self);
  with FPanel1 do
  begin
    Parent := self;
    Align := alTop;
    Caption := '';
  end;
  FGrid.Align := alClient;

  // Header for first column of grid
  FHeader1 := TPanel.Create(FPanel1);
  with FHeader1 do
  begin
    Parent := FPanel1;
    Align := alLeft;
    Alignment := taLeftJustify;
    Caption := ' ' + ColHeading1;
    ParentBackground := False;
    BevelOuter := bvNone;
    Ctl3D := False;
  end;

  // Splitter between grid's column headers
  FSplitter := TSplitter.Create(FPanel1);
  with FSplitter do
  begin
    Parent := FPanel1;
    Left := self.ClientWidth div 2;
    Beveled := True;
    Width := 3;
    OnMoved := SplitterMoved;
  end;

  // Header for second column of grid
  FHeader2 := TPanel.Create(FPanel1);
  with FHeader2 do
  begin
    Parent := FPanel1;
    Align := alClient;
    Alignment := taLeftJustify;
    Caption := ' ' + ColHeading2;
    ParentBackground := False;
    BevelOuter := bvNone;
    Ctl3D := False;
  end;

  // Create the edit & combobox controls last so that
  // they can appear in front of the grid when activated.

  // FEdit is used to edit numbers and strings
  FEdit := TEdit.Create(self);
  with FEdit do
  begin
    Parent := self;
    Ctl3D := False;  //True;
    BorderStyle := bsSingle;
    BevelKind := bkNone;   //bkFlat;
    AutoSelect := True;
    Visible := False;
    OnKeyPress := EditKeyPress;
    OnKeyDown := EditKeyDown;
  end;

  // FCombo is used to edit choices
  FCombo := TComboBox.Create(self);
  with FCombo do
  begin
    Parent := self;
    Ctl3D := False;  //True;
    BevelKind := bkNone;  //bkFlat;
    Style := csDropDown;
    Visible := False;
    OnKeyPress := EditKeyPress;
    OnKeyDown := EditKeyDown;
    OnChange := ComboChange;
    OnDblClick := ComboDblClick;
  end;

  RowHeight := FCombo.Height;
  FGrid.DefaultRowHeight := RowHeight;
  FEdit.Height := RowHeight;

  FPanel1.Height := RowHeight + 2;

  ComponentsCreated := True;
  Resize;
end;


//============================================================================
//                       Property Servers
//============================================================================

procedure TPropEdit.SetReadOnlyColor(Value: TColor);
begin
  if FReadOnlyColor <> Value then FReadOnlyColor := Value;
end;

procedure TPropEdit.SetValueColor(Value: TColor);
begin
  if FValueColor <> Value then FValueColor := Value;
end;

procedure TPropEdit.SetHeadBackColor(Value: TColor);
begin
  if FHeadBackColor <> Value then FHeadBackColor := Value;
end;

procedure TPropEdit.SetHeadForeColor(Value: TColor);
begin
  if FHeadForeColor <> Value then FHeadForeColor := Value;
end;

procedure TPropEdit.SetColHeading1(Value: String);
begin
  if FColHeading1 <> Value then
  begin
    FColHeading1 := Value;
    if ComponentsCreated then FHeader1.Caption := ' ' + Value;
  end;
end;

procedure TPropEdit.SetColHeading2(Value: String);
begin
  if FColHeading2 <> Value then
  begin
    FColHeading2 := Value;
    if ComponentsCreated then FHeader2.Caption := ' ' + Value;
  end;
end;

procedure TPropEdit.SetHeaderSplit(Value: Integer);
begin
  if (FHeaderSplit <> Value) then
    FHeaderSplit := Value;
{  if ComponentsCreated then
  begin
    Value := Trunc(FHeaderSplit * FPanel1.Width / 100);
    if (Value < FSplitter.MinSize) or
       (Value > FPanel1.Width - FSplitter.MinSize) then Exit;
    //FSplitter.Left := Value;
    FHeader1.Width := Value;
    SplitterMoved(self);
  end;}
end;

function TPropEdit.IsValid;
begin
  Result := True;
  if not ComponentsCreated then Exit;
  if (FCombo.Visible) then Result := GoValidate(FCombo.Text);
  if (FEdit.Visible) then Result := GoValidate(FEdit.Text);
end;


//=============================================================================
//                         Re-sizing procedures
//=============================================================================

procedure TPropEdit.Resize;
//var
//  S: String;
begin
  inherited Resize;
  if ComponentsCreated then
  begin
    if (FEdit.Visible) then EditExit(FEdit);
    if (FCombo.Visible) then EditExit(FCombo);
    ResizeGrid(Self);
  end;
end;


procedure TPropEdit.SplitterMoved(Sender: TObject);
begin
  // Save new HeaderSplit value
  FHeaderSplit := FHeader1.Width * 100 div FPanel1.Width;

  //Resize Grid columns
  with FGrid do
  begin
    ColWidths[0] := FHeader1.ClientWidth;
    ColWidths[1] := ClientWidth - ColWidths[0] - 2 * GridLineWidth;
  end;

  //Resize active edit control
  if (FEdit.Visible) then SetEditBounds(FEdit);
  if (FCombo.Visible) then SetEditBounds(FCombo);
end;


procedure TPropEdit.ResizeGrid(Sender: TObject);
begin
  VisibleRows := (ClientHeight - FPanel1.Height) div (RowHeight + 1);
  with FGrid do
  begin
    //Determine number of visible rows
    DefaultRowheight := RowHeight;
    if VisibleRows > RowCount then VisibleRows := RowCount;
    Height := VisibleRows*(RowHeight + 1);

    // Set header section widths
    FHeader1.Width := (FHeaderSplit * FPanel1.Width) div 100;
    if FHeader1.ClientWidth  > ClientWidth - FSplitter.MinSize then
    begin
      FHeader1.ClientWidth := ClientWidth - FSplitter.MinSize;
      FHeaderSplit := FHeader1.Width * 100 div FPanel1.Width;
    end;

    // Set grid's column widths
    ColWidths[0] := FHeader1.ClientWidth;
    ColWidths[1] := ClientWidth - ColWidths[0] - 2 * GridLineWidth;
  end;
end;


//=============================================================================
//                    Edit Controls Event Handlers
//=============================================================================

procedure TPropEdit.EditKeyPress(Sender: TObject; var Key: Char);
//-----------------------------------------------------------------------------
// OnKeyPress event handler for the edit and combo box controls.
//-----------------------------------------------------------------------------
var
  S: String;
  X: Extended;
begin
  if Key = #13 then   {Enter key}
  begin
    if (Sender = FCombo)
    and (FCombo.DroppedDown) then Exit;
    EditExit(Sender);
    Key := #0;
  end

  else if Key = #27 then   {Escape key}
  begin
    with Sender as TwinControl do Visible := False;
    FGrid.SetFocus;
    Key := #0;
  end

  else if (Sender = FEdit) then with Sender as TEdit do
  begin
    // Allow backspace key press
    if Key = #8 then Exit;

    // If a number is required
    if (EditMask in [emNumber, emPosNumber]) then
    begin

      // Insert the key character into the current text
      S := Text;
      Delete(S, SelStart+1, SelLength);
      Insert(Key, S, SelStart+1);

      // Add a 1 to complete a partial numeric entry
      S := S + '1';

      // Check that this creates a valid number
      try
        X := StrToFloat(S);

        // Check if a positive number is required
        if (EditMask = emPosNumber) and (X < 0.0) then Key := #0;

      // Ignore the key if we don't have a valid number
      except
        on EConvertError do Key := #0;
      end;
    end

    // For the NoSpace style, ignore spaces, double quotes, semicolons
    // and left bracket as first character
    else if (EditMask = emNoSpace) then
    begin
      if Key = '[' then
      begin
        if (Length(Text) = 0) or (SelStart = 0) then  Key := #0;
      end
      else if CharInSet(Key, [' ', '"', ';']) then Key := #0;
    end;
  end;
end;


procedure TPropEdit.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
//-----------------------------------------------------------------------------
// OnKeyDown event handler for the edit and combo box controls.
//-----------------------------------------------------------------------------
begin
  if Key in [VK_UP,VK_DOWN,VK_PRIOR,VK_NEXT] then
  begin
  { Check if Combobox is dropped down }
    if (Sender = FCombo) and (FCombo.DroppedDown) then Exit
    else EditExit(Sender);

  { Pass keystroke onto Grid }
    SendMessage(FGrid.Handle,WM_KEYDOWN,Key,0);
  end;
  if (Sender = FCombo) and (Key = VK_RETURN) and (ssCtrl in Shift) then
    ComboDblClick(Sender);
  if Key = VK_F1 then SendMessage(FGrid.Handle,WM_KEYDOWN,Key,0);
end;


procedure TPropEdit.EditExit(Sender: TObject);
//-----------------------------------------------------------------------------
// OnExit event handler for the edit and combo box controls.
//-----------------------------------------------------------------------------
var
  S : String;
  C: TWinControl;
begin
  // Extract text from the active edit control
  C := TWinControl(Sender);
  if C.Visible then
  begin
    if C = FEdit then
      S := Trim(FEdit.Text)
    else if C = FCombo then
      S := Trim(FCombo.Text)
    else S := FGrid.Cells[1,FRow];

    // Validate text if it has changed
    GoValidate(S);

    // Return focus to grid control
    C.Visible := False;
    FGrid.SetFocus;
  end;
end;


procedure TPropEdit.ComboDblClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnDblClick event handler for the combo box control. Allows a special
// editor (or other procedure) to be launched when a combo box field
// is double clicked.
//-----------------------------------------------------------------------------
var
  S: String;
  Modified: Boolean;
begin
  if Assigned(FOnButtonClick) then
  begin
    S := FCombo.Text;
    Modified := False;
    FOnButtonClick(self, FRow, S, Modified);
    FCombo.Text := S;
    EditExit(Sender);
  end;
end;


procedure TPropEdit.ComboChange(Sender: TObject);
//-----------------------------------------------------------------------------
// OnChange event handler for the combo box control.
//-----------------------------------------------------------------------------
begin
  if FCombo.Visible then GoValidate(Trim(FCombo.Text));
end;


procedure TPropEdit.GoButtonClick(aRow: Integer);
//-----------------------------------------------------------------------------
// Calls the OnButtonClick event procedure.
//-----------------------------------------------------------------------------
var
  S: String;
  Modified: Boolean;
begin
  if Assigned(FOnButtonClick) then
  begin
    S := FGrid.Cells[1, aRow];
    Modified := False;
    FOnButtonClick(self, aRow, S, Modified);
    GoValidate(S);
  end;
end;


function TPropEdit.GoValidate(S: String): Boolean;
//-----------------------------------------------------------------------------
// Validates an edited property value and updates the value if its valid.
// S is a string holding the newly edited value.
//-----------------------------------------------------------------------------
var
  IsValid: Boolean;
  Errmsg:  String;
begin
  // Check if S is different from current value
  IsValid := True;
  if (S <> FGrid.Cells[1,FRow]) then
  try

    // Call the validation procedure if it exists
    if (Assigned(FOnValidate)) then
      FOnValidate(self,FRow,S,Errmsg,IsValid);

    // If the new value is valid then update the StringGrid display
    // and the StringList that holds the property values
    if (IsValid) then
    begin
      FGrid.Cells[1,FRow] := S;
      FValues[FRow] := S;
      FModified := True;
    end

    // If not valid then raise an exception
    else raise EInvalidProperty.Create('Invalid Property Value');

  except
    // Specific error messages should be displayed in FOnValidate
    on E:EInvalidProperty do
    begin
      if Length(Errmsg) = 0 then
        MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
  end;
  Result := IsValid;
end;

//=============================================================================
//                      StringGrid Event Handlers
//=============================================================================

procedure TPropEdit.GridSelectCell(Sender: TObject; ACol, ARow: Longint;
  var CanSelect: Boolean);
//-----------------------------------------------------------------------------
// OnSelectCell event handler for grid control.
//-----------------------------------------------------------------------------
begin
  if (Assigned(FOnRowSelect)) then FOnRowSelect(self, ARow);
  CanSelect := True;
end;


procedure TPropEdit.GridClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick event handler for grid control.
//-----------------------------------------------------------------------------
begin
  // Exit any active edit control
  if (FEdit.Visible) then EditExit(FEdit);
  if (FCombo.Visible) then EditExit(FCombo);

  // Prevent selection of cell in column 0
  with FGrid do
  begin
    if Col = 0 then Col := 1;   { Can't select column 0 }
    FRow := Row;                { Save current row value }
  end;

  // Set button status if row's edit style is esButton
  if (FProps^[FRow].Style = esButton) then
    ButtonVisible := True
  else
    ButtonVisible := False;
end;


procedure TPropEdit.GridKeyPress(Sender: TObject; var Key: Char);
//-----------------------------------------------------------------------------
// OnKeyPress event handler for the grid control.
//-----------------------------------------------------------------------------
begin
  if FGrid.Col <> 1 then Exit;
  if (Key = #13) or CharInSet(Key, [#43..#122]) then
  begin
    with FGrid do EditProperty(Col,Row,Key);
  end;
end;


procedure TPropEdit.GridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
//-----------------------------------------------------------------------------
// OnKeyDown event handler for the grid control.
//-----------------------------------------------------------------------------
var
  aRow: LongInt;
begin
  // Identify the current row
  aRow := FGrid.Row;

  // Fire the OnButtonClick event if Enter was pressed for a row
  // whose edit style is esButton
  if (Key = VK_RETURN)
  and (FProps^[aRow].Style = esButton)
  then GoButtonClick(aRow)

  // Let the grid process the Up & Down Arrow key press
  else if Key in [VK_UP, VK_DOWN] then exit

  // Pass all other keys to the parent form
  else
  begin
    SendMessage(Parent.Handle,WM_KEYDOWN,Key,0);
    Key := 0;
  end;
end;


procedure TPropEdit.GridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
//-----------------------------------------------------------------------------
// OnMouseDown event handler for the grid control.
//-----------------------------------------------------------------------------
var
  aCol,aRow: LongInt;
  W: Integer;
  aRect, R: TRect;
begin
  with Sender as TStringGrid do
  begin
    MouseToCell(X,Y,aCol,aRow);

    // If   current row's edit style is esButton, and  button is visible,
    // and  mouse is over the button, then re-draw button in pressed state.
    if  (aCol = 1) and (aRow = Row)
    and (FProps^[aRow].Style = esButton)
    and (ButtonVisible) then
    begin
      aRect := CellRect(aCol,aRow);
      W := aRect.Bottom - aRect.Top;
      SetRect(R, aRect.Right - W, aRect.Top, aRect.Right, aRect.Bottom);
      if PtInRect(R, Point(X,Y)) then
      begin
        ButtonPressed := True;
        DrawButton(Canvas, aRect);
      end;
    end;
  end;
end;


procedure TPropEdit.GridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
//-----------------------------------------------------------------------------
// OnMouseUp event handler for the grid control.
//-----------------------------------------------------------------------------
var
  aCol,aRow: LongInt;
  aRect: TRect;
begin
  with Sender as TStringGrid do
  begin
    MouseToCell(X,Y,aCol,aRow);
    if (aCol = 0) then aCol := 1;

    // If button is pressed and current row's edit style is esButton
    // then redraw button in unpressed state and fire the OnButtonClick event.
    if (ButtonPressed) then
    begin
      if (FProps^[aRow].Style = esButton) then
      begin
        aRect := CellRect(1,aRow);
        ButtonPressed := not ButtonPressed;
        DrawButton(Canvas, aRect);
        GoButtonClick(aRow);
      end
      else
      begin
        ButtonPressed := not ButtonPressed;
      end;
    end;
  end;
  EditProperty(1,FGrid.Row,#13);
end;


procedure TPropEdit.GridTopLeftChanged(Sender: TObject);
//-----------------------------------------------------------------------------
// OnTopLeftChanged event handler for the grid control.
//-----------------------------------------------------------------------------
begin
  FEdit.Visible := False;
  FCombo.Visible := False;
  FGrid.SetFocus;
end;


procedure TPropEdit.GridDrawCell(Sender: TObject; vCol,
  vRow: Longint; Rect: TRect; State: TGridDrawState);
//-----------------------------------------------------------------------------
// OnDrawCell event handler for the grid control.
//-----------------------------------------------------------------------------
begin
  if (FProps = nil) then Exit;
  //Rect.Left := Rect.Left - 4;
  with Sender as TStringGrid do
  begin
    with Canvas do
    begin

      if vCol = 0 then
      begin
        if (FProps^[vRow].Style = esHeading) then
        begin
          Brush.Color := StyleServices.GetStyleColor(scPanel); //FHeadBackColor;
          Font.Color := FHeadForeColor;
        end
        else Brush.Color := Color;
        FillRect(Rect);
        SetBkMode(Handle,TRANSPARENT);
        TextOut(Rect.Left+2,Rect.Top+2,Cells[vCol,vRow]);
        exit;
      end;

      if FProps^[vRow].Style = esHeading then
      begin
        Brush.Color := StyleServices.GetStyleColor(scPanel); //FHeadBackColor;
        Font.Color := FHeadForeColor;
      end
      else if (FProps^[vRow].Style = esReadOnly) then
      begin
        Brush.Color := FReadOnlyColor;
        Font.Color := FValueColor;
      end
      else if (gdSelected in State) then
      begin
        Brush.Color := clWhite;
        Font.Color := clBlack;
      end
      else Font.Color := FValueColor;
      FillRect(Rect);
      SetBkMode(Handle,TRANSPARENT);
      TextOut(Rect.Left+2,Rect.Top+2,Cells[vCol,vRow]);
      if (gdSelected in State) and
         (FProps^[vRow].Style = esButton) then
        DrawButton(Canvas,Rect);
    end;
  end;
end;


procedure TPropEdit.DrawButton(aCanvas: TCanvas; aRect: TRect);
//-----------------------------------------------------------------------------
// Draws button in the cell of the grid control.
//-----------------------------------------------------------------------------
var
  Flags: Integer;
  W, H: Integer;
  R: TRect;
begin
  with aRect do
  begin
    Height := Bottom - Top;
    Width := Right - Left;
    W := Height;
    H := Height div 2;
    SetRect(R, Left + Width - W, Top+1, Left + Width-1, Top + Height-1);
  end;
  Flags := 0;
  if ButtonPressed then Flags := BF_FLAT;
  DrawEdge(aCanvas.Handle, R, EDGE_RAISED, BF_RECT or BF_MIDDLE or Flags);
  Flags := ((R.Right - R.Left) shr 1);
  PatBlt(aCanvas.Handle, R.Left+Flags, R.Top + H, 2, 2, BLACKNESS);
  PatBlt(aCanvas.Handle, R.Left+Flags-5, R.Top + H, 2, 2, BLACKNESS);
  PatBlt(aCanvas.Handle, R.Left+Flags+5, R.Top + H, 2, 2, BLACKNESS);
  ButtonVisible := True;
end;


//=============================================================================
//                         General Procedures
//=============================================================================

procedure TPropEdit.SetDropDownWidth;
//-----------------------------------------------------------------------------
// Sets the width of the dropdown list box associated with the combo control.
//-----------------------------------------------------------------------------
var
  I, ItemWidth, MaxWidth: Integer;
begin
  MaxWidth := 0;
  with FCombo do
  begin
    for I := 0 to Items.Count-1 do
    begin
      ItemWidth := FGrid.Canvas.TextWidth(Items[I]);
      if ItemWidth > MaxWidth then MaxWidth := ItemWidth;
    end;
    MaxWidth := MaxWidth + 10;
    if MaxWidth > Fgrid.ClientWidth then MaxWidth := Fgrid.ClientWidth;
    Perform(CB_SETDROPPEDWIDTH, MaxWidth, 0);
  end;
end;


procedure TPropEdit.EditProperty(CurCol, CurRow: LongInt; Key: Char);
//-----------------------------------------------------------------------------
// Activates the appropriate editor for the current grid cell.
//-----------------------------------------------------------------------------
begin
  // No editor used for following conditions
  if CurCol <> 1 then Exit;
  if CurRow <> FGrid.Row then Exit;
  if FProps^[CurRow].Style = esReadOnly then Exit;
  if FProps^[CurRow].Style = esButton then Exit;

  // Activate the single line text edit control
  if (FProps^[CurRow].Style = esEdit) then
  begin

    // Determine max. length and edit mask
    FEdit.MaxLength := FProps^[CurRow].Length;
    EditMask := FProps^[CurRow].Mask;

    // Place property value in edit control
    if (Key = #13) then
      FEdit.Text := FGrid.Cells[1,CurRow]
    else
      FEdit.Text := '';

    // Activate the control
    SetEditBounds(TwinControl(FEdit));
    if (Key <> #13) then
      PostMessage(FEdit.Handle,WM_KeyDown,VkKeyScan(Key),0);
  end

  // Activate the combo box edit control
  else if (FProps^[CurRow].Style in [esComboList, esComboEdit]) then
  with FCombo do
  begin
    Items.Clear;
    Items.SetText(PChar(FProps^[CurRow].List));
    if FProps^[CurRow].Style = esComboList then
    begin
      Style := csOwnerDrawFixed; //csDropDownList;
      ItemIndex := Items.IndexOf(FGrid.Cells[1,CurRow]);
    end
    else
    begin
      Style := csDropDown;
      Text := FGrid.Cells[1,CurRow];
    end;
    SetEditBounds(TwinControl(FCombo));
    SetDropDownWidth;
  end;
end;


procedure TPropEdit.SetEditBounds(aControl : TWinControl);
//-----------------------------------------------------------------------------
// Sets the bounds on the active edit control.
//-----------------------------------------------------------------------------
var
  aRect : TRect;
begin
  if aControl <> Nil then with aControl do
  begin
    aRect  := FGrid.CellRect(1,FGrid.Row);
    Left   := FGrid.Left + aRect.Left;
    Top    := FGrid.Top + aRect.Top;
    Width  := FGrid.ColWidths[1];
    Height := FGrid.DefaultRowHeight;
    if Parent.Visible then
    begin
      Visible := True;
      aControl.SetFocus;
    end;
  end;
end;


procedure TPropEdit.SetProp(Index: Integer; Value: String);
//-----------------------------------------------------------------------------
// Assigns Value to the property in row Index of the editor.
//-----------------------------------------------------------------------------
begin
  with FGrid do
  begin
    if (Index >= 0) and (Index < RowCount) and (Fvalues <> nil) then
    begin
      Cells[1,Index] := Value;
      FValues[Index] := Value;
    end;
  end;
end;


function  TPropEdit.GetProp(Index: Integer): String;
//-----------------------------------------------------------------------------
// Retrieves the string value of the property in row Index of the editor.
//-----------------------------------------------------------------------------
begin
  if (Fvalues <> nil) and (Index < FValues.Count) then
    Result := Fvalues[Index]
  else
    Result := '';
end;


procedure TPropEdit.SetProps(var PropArray: array of TPropRecord;
  Values: TStrings);
//-----------------------------------------------------------------------------
// Updates the property editor with a new set of properties.
//-----------------------------------------------------------------------------
var
  i,j: Integer;
begin
  // If editing components have not been created then create them
  if not ComponentsCreated then CreateComponents;

  // Initialize status of editor
  FEdit.Visible := False;
  FCombo.Visible := False;
  ButtonPressed := False;
  ButtonVisible := False;
  FModified := False;

  // Assign pointers to the property array & initial values
  FProps := @PropArray;
  FValues := Values;

  // Re-size the editor's grid and populate its cells
  with FGrid do
  begin
    RowCount := high(PropArray) - low(PropArray) + 1;
    ResizeGrid(self);
    for i := 0 to RowCount - 1 do
    begin
      j := low(PropArray) + i;
      Cells[0,i] := PropArray[j].Name;
    end;
    Cols[1].Assign(Values);
    Col := 1;
    if FRow >= RowCount then FRow := 0;
    Row := FRow;
  end;

  // Call OnRowSelect handler
  if (Assigned(FOnRowSelect)) then FOnRowSelect(self, FRow);
end;


procedure TPropEdit.Edit;
//-----------------------------------------------------------------------------
// Activates the Property Editor.
//-----------------------------------------------------------------------------
begin
  if Parent.Visible then
  begin
    FEdit.Visible := False;
    FCombo.Visible := False;
    FGrid.SetFocus;
  end;
end;


procedure TPropEdit.EndEditing;
//-----------------------------------------------------------------------------
// Ends editing with the Property Editor.
//-----------------------------------------------------------------------------
var
  S : String;
begin
  // Save contents of an active TEdit control to the property grid
  if FEdit.Visible then
  begin
    S := Trim(FEdit.Text);
    GoValidate(S);
  end;
end;

procedure TPropEdit.GetProps(Plist, Vlist: TStringlist);
//-----------------------------------------------------------------------------
// Retrieves property names & values from the Editor.
//-----------------------------------------------------------------------------
begin
  Plist.Assign(FGrid.Cols[0]);  //List of property names
  Vlist.Assign(Fgrid.Cols[1]);  //List of property values
end;

end.
