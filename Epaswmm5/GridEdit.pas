unit GridEdit;

{-------------------------------------------------------------------}
{                    Unit:    GridEdit.pas                          }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Frame unit containing a string grid control with enhancements   }
{   for editing numerical data and a popup edit menu. An NumEdit    }
{   component (named EditBox), placed over the current cell of the  }
{   grid, is used for editing numerical data.                       }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, NumEdit, ExtCtrls, Grids, Menus, Clipbrd, Vcl.Themes,
  System.Types;

type
  TGridEditFrame = class(TFrame)
    Grid: TStringGrid;
    EditPanel: TPanel;
    EditBox: TNumEdit;
    PopupMenu: TPopupMenu;
    MnuCut: TMenuItem;
    MnuCopy: TMenuItem;
    MnuPaste: TMenuItem;
    MnuInsert: TMenuItem;
    N1: TMenuItem;
    MnuInsertRow: TMenuItem;
    MnuDeleteRow: TMenuItem;
    procedure EditBoxKeyPress(Sender: TObject; var Key: Char);
    procedure EditBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditBoxExit(Sender: TObject);
    procedure GridClick(Sender: TObject);
    procedure GridDblClick(Sender: TObject);
    procedure GridKeyPress(Sender: TObject; var Key: Char);
    procedure MenuItemClick(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
    procedure GridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
  private
    { Private declarations }
    AutoMove: Boolean;
    procedure ShowEditor(Key: Char);
    procedure SetEditBounds;
    procedure GoAutoMove(Key: Word);
    procedure ClearCells;
    procedure CopyCells;
    procedure DeleteOneRow;
    procedure InsertCells;
    procedure InsertRows(Nrows: Integer);
    procedure PasteCells;
  public
    { Public declarations }
    AllowInsert: Boolean;
    CenterHeaders: Boolean;
    Modified: Boolean;
  end;

implementation

{$R *.dfm}

const
  MNU_CUT        = 0;
  MNU_COPY       = 1;
  MNU_PASTE      = 2;
  MNU_INSERT     = 3;
  MNU_N1         = 4;
  MNU_INSERT_ROW = 5;
  MNU_DELETE_ROW = 6;


procedure TGridEditFrame.EditBoxKeyPress(Sender: TObject; var Key: Char);
//-----------------------------------------------------------------------------
// OnKeyPress handler for the EditBox control.
//-----------------------------------------------------------------------------
begin
  // Stop editing and move to next cell when Enter key is pressed
  if Key = #13 then
  begin
    EditBoxExit(Sender);
    Grid.SetFocus;
    Key := #0;
    if AutoMove then GoAutoMove(VK_RIGHT);
  end

  // Stop editing and ignore new text when Escape key pressed
  else if Key = #27 then
  begin
    EditPanel.Visible := False;
    Grid.SetFocus;
    Key := #0;
  end;
end;

procedure TGridEditFrame.EditBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
//-----------------------------------------------------------------------------
// OnKeyDown handler for the EditBox control. Handles pressing of arrow keys.
//-----------------------------------------------------------------------------
begin
  // Up or Down arrow keys
  if Key in [VK_UP,VK_DOWN] then
  begin
    EditBoxExit(Sender);
    Grid.SetFocus;
    SendMessage(Grid.Handle, WM_KEYDOWN, Key, 0);
  end;

  // F1 key
  if Key = VK_F1 then SendMessage(Grid.Handle, WM_KEYDOWN, Key, 0);
end;

procedure TGridEditFrame.EditBoxExit(Sender: TObject);
//-----------------------------------------------------------------------------
// OnExit handler for the EditBox control.
//-----------------------------------------------------------------------------
var
  S : String;
begin
  // Extract text from the edit control and place it in the grid
  if EditPanel.Visible then
  begin
    S := EditBox.Text;
    with Grid do
    begin
      if CompareStr(S, Cells[Col,Row]) <> 0 then Modified := True;
      Cells[Col,Row] := S;
    end;
    EditPanel.Visible := False;
  end;
end;

procedure TGridEditFrame.GridClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the Grid control.
//-----------------------------------------------------------------------------
begin
  // Stop any editing in progress
  if (EditPanel.Visible) then EditBoxExit(Sender);
end;

procedure TGridEditFrame.GridDblClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnDblClick handler for the Grid control.
//-----------------------------------------------------------------------------
begin
  // Stop any editing in progress
  //if (EditPanel.Visible) then EditBoxExit(Sender);

  // Re-start editing in selected cell
//  with Grid do
//    if (Row >= FixedRows) and (Col >= FixedCols) then ShowEditor(#13);
end;

procedure TGridEditFrame.GridKeyPress(Sender: TObject; var Key: Char);
//-----------------------------------------------------------------------------
// OnKeyPress handler for the Grid control. Activates the EditBox control
// if the Enter key or a character key is pressed.
//-----------------------------------------------------------------------------
begin
  if (Key = #13) or CharInSet(Key, [#43..#122]) then ShowEditor(Key);
end;

procedure TGridEditFrame.GridDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
//-----------------------------------------------------------------------------
//  OnDrawCell handler for the Grid control.
//-----------------------------------------------------------------------------
var
  DeltaX: Integer;
begin
  with Sender as TStringGrid do
  begin
    DeltaX := 0;
    if (ARow < FixedRows) or (ACol < FixedCols) then
    begin
      // Use panel color to draw fixed cells
      if StyleServices.Enabled then
      begin
        Canvas.Brush.Color := StyleServices.GetStyleColor(scPanel);
        DeltaX := 4;
      end;
      Rect.Left := Rect.Left - DeltaX;

    // Use Win API DrawText function to enable word-wraping
      Canvas.FillRect(Rect);
      if (ARow < FixedRows) and (ACol >= FixedCols) then
      begin
        if CenterHeaders then
          DrawText(Canvas.Handle, Cells[ACol,ARow], -1, Rect,
                            DT_CENTER OR DT_VCENTER OR DT_WORDBREAK)
        else Canvas.TextOut(Rect.Left + DeltaX + 2, Rect.Top + 2,
                            Cells[ACol,ARow]);
      end
      else Canvas.TextOut(Rect.Left + DeltaX + 2, Rect.Top + 2,
                          Cells[ACol,ARow]);
    end;
  end;
end;

procedure TGridEditFrame.PopupMenuPopup(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnPopup handler for the popup menu.
//-----------------------------------------------------------------------------
begin
  // Hide the Insert/Delete options if insertion is not allowed
  MnuInsert.Visible := AllowInsert;
  N1.Visible := AllowInsert;
  MnuInsertRow.Visible := AllowInsert;
  MnuDeleteRow.Visible := AllowInsert;

  // Disable Pasting/Inserting if no text is on the clipboard
  MnuPaste.Enabled := Clipboard.HasFormat(CF_TEXT);
  MnuInsert.Enabled := MnuPaste.Enabled;
end;

procedure TGridEditFrame.MenuItemClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler shared among all of the popup edit menu's items.
//-----------------------------------------------------------------------------
begin
  with Sender as TMenuItem do
  begin
    case Tag of

    MNU_CUT:
      begin
        CopyCells;
        ClearCells;
      end;

    MNU_COPY:
      CopyCells;

    MNU_PASTE:
      PasteCells;

    MNU_INSERT:
      InsertCells;

    MNU_INSERT_ROW:
      InsertRows(1);

    MNU_DELETE_ROW:
      DeleteOneRow;
    end;
    if Tag <> MNU_COPY then Modified := True;
  end;
end;

procedure TGridEditFrame.ShowEditor(Key: Char);
//-----------------------------------------------------------------------------
// Makes the EditBox control visible and initializes its contents.
//-----------------------------------------------------------------------------
begin
  // When Enter key pressed, load the Grid cell's text into the EditBox
  // control and do not move to new cell once editing is done
  if (Key = #13) then with Grid do
  begin
    EditBox.Text := Cells[Col,Row];
    AutoMove := False;
  end

  // For any other key pressed, set the EditBox's text to empty and
  // allow movement to new cell once editing is done
  else
  begin
    EditBox.Text := '';
    AutoMove := True;
  end;

  // Place the editing panel, which contains the EditBox, over the
  // current grid cell and make it visible
  SetEditBounds;
  if Visible then
  begin
    EditPanel.Visible := True;
    EditBox.SetFocus;
  end;

  // Pass the key pressed on to the EditBox control
  if (Key = #13) then Key := #0;
  PostMessage(EditBox.Handle,WM_CHAR,Integer(Key),0);
end;

procedure TGridEditFrame.SetEditBounds;
//-----------------------------------------------------------------------------
//  Makes the editing panel fit over grid cell being edited.
//-----------------------------------------------------------------------------
var
  aRect : TRect;
begin
  // First determine the position and size of the panel
  with Grid do aRect := CellRect(Col,Row);
  with EditPanel do
  begin
    Left   := aRect.Left + 1;
    Top    := aRect.Top + 1;
    Width  := aRect.Right - aRect.Left;
    Height := aRect.Bottom - aRect.Top;
  end;

  // Then place EditBox control at a slight offset within the panel
  // to make it look like the grid's text is being edited in place
  with EditBox do
  begin
    Left := 5;
    Top := 1;
    Width := EditPanel.Width - 5;
    Height := EditPanel.Height - 1;
  end;
end;

procedure TGridEditFrame.GoAutoMove(Key: Word);
//-----------------------------------------------------------------------------
//  Moves focus to new grid cell after arrow key was pressed.
//-----------------------------------------------------------------------------
begin
  // For the right arrow key, move to the next column in the grid.
  // If in the last column then move to the first editable column
  // of the next row.
  if (Key = VK_RIGHT) then with Grid do
  begin
    if Col < ColCount-1 then Col := Col + 1
    else if Row < RowCount-1 then
    begin
      Row := Row + 1;
      Col := FixedCols;
    end;
  end;

  // For the left arrow key, simply move to the previous column.
  if Key = VK_LEFT then with Grid do
  begin
    if Col > FixedCols then Col := Col - 1;
  end;
end;

procedure TGridEditFrame.ClearCells;
//-----------------------------------------------------------------------------
//  Clears the contents of selected cells.
//-----------------------------------------------------------------------------
var
  R,C: LongInt;
begin
  with Grid.Selection do
  begin
    for R := Top to Bottom do
    begin
      for C := Left to Right do
        Grid.Cells[C,R] := '';
    end;
  end;
end;

procedure TGridEditFrame.CopyCells;
//-----------------------------------------------------------------------------
//  Copies the contents of selected cells to the clipboard.
//-----------------------------------------------------------------------------
var
  Slist: TStringlist;
  S: String;
  R,C: LongInt;
begin
  // Create a stringlist to hold the selected text
  Slist := TStringList.Create;
  try
    with Grid.Selection do
    begin

      // Loop through each row of the selected cells
      for R := Top to Bottom do
      begin

        // Add the text from each column to a string, separated by tabs
        S := '';
        for C := Left to Right-1 do
          S := S + Grid.Cells[C,R] + #9;
        S := S + Grid.Cells[Right,R];

        // Add the row string to the stringlist
        Slist.Add(S);
      end;
    end;

    // Add the text from the stringlist to the clipboard
    Clipboard.SetTextBuf(PChar(Slist.Text));
  finally
    Slist.Free;
  end;
end;

procedure TGridEditFrame.DeleteOneRow;
//-----------------------------------------------------------------------------
//  Deletes a row from the grid
//-----------------------------------------------------------------------------
var
  Rstart, R, C: LongInt;
begin
  Rstart := Grid.Row;
  for R := Rstart + 1 to Grid.RowCount - 1 do
  begin
    for C := Grid.FixedCols to Grid.ColCount - 1 do
      Grid.Cells[C,R-1] := Grid.Cells[C,R];
  end;
  Grid.RowCount := Grid.RowCount - 1;
  Grid.Row := Rstart;
end;

function CountRows(var T: String): Integer;
//-----------------------------------------------------------------------------
//  Counts the number of rows, denoted by carriage returns, in string T.
//-----------------------------------------------------------------------------
var
  P, N: Integer;
begin
  N := 0;
  while (Length(T) > 0) do
  begin
    P := Pos(Chr(13), T);
    if P > 0 then
    begin
      if T[P+1] = Chr(10) then Inc(P);
      T := Copy(T, P+1, Length(T));
    end
    else T := '';
    Inc(N);
  end;
  Result := N;
end;

procedure TGridEditFrame.InsertCells;
//-----------------------------------------------------------------------------
//  Inserts the contents of the clipboard into new rows of the grid.
//-----------------------------------------------------------------------------
var
  N: Integer;
  T: String;
begin
  if Clipboard.HasFormat(CF_TEXT) then
  begin
    T := Clipboard.AsText;   // Retrieve text from Clipboard
    N := CountRows(T);       // Count rows in text
    InsertRows(N);           // Insert N new rows into aGrid
    PasteCells();            // Paste text into original cells
  end;
end;

procedure TGridEditFrame.InsertRows(Nrows: Integer);
//-----------------------------------------------------------------------------
//  Inserts new rows into the grid.
//-----------------------------------------------------------------------------
var
  Rstart, R, C: LongInt;
begin
  if Nrows > 0 then with Grid do
  begin
    Rstart := Row;
    RowCount := RowCount + Nrows;
    if FixedCols = 1 then
      for R := RowCount - Nrows to RowCount - 1 do
        Cells[0,R] := IntToStr(R);
    for R := RowCount - 1 downto Rstart + Nrows do
    begin
      for C := FixedCols to ColCount - 1 do
        Cells[C,R] := Cells[C,R-Nrows];
    end;
    for R := Rstart to Rstart + Nrows - 1 do
    begin
      for C := FixedCols to ColCount - 1 do Cells[C,R] := '';
    end;
  end;
end;

procedure TGridEditFrame.PasteCells;
//-----------------------------------------------------------------------------
//  Pastes text from the clipboard into the grid's cells.
//-----------------------------------------------------------------------------
var
  R, C, P: LongInt;
  T, Line, Value: String;
begin
  if Clipboard.HasFormat(CF_TEXT) then with Grid do
  begin

    // Retrieve text from Clipboard into string T
    T := Clipboard.AsText;

    // Begin at current row
    R := Row;

    // Process the text
    while (Length(T) > 0) do
    begin
      if R = RowCount then
      begin
        RowCount := RowCount + 1;
        Cells[0,RowCount-1] := IntToStr(RowCount-1);
      end;

      // Search for end of current row
      P := Pos(Chr(13),T);
      if (P > 0) then
      begin

        // Copy row from T to Line
        Line := Copy(T,1,P-1);

        // Make T contain only remaining text
        if (T[P+1] = Chr(10)) then Inc(P);
        T := Copy(T,P+1,Length(T));
      end
      else
      begin
        Line := T;
        T := '';
      end;

      // Parse the contents of each column from the row string
      C := Col;
      while (Length(Line) > 0) and (C < ColCount) do
      begin
        P := Pos(Chr(9),Line);
        if (P > 0) then
        begin
          Value := Copy(Line,1,P-1);
          Line := Copy(Line,P+1,Length(Line));
        end
        else
        begin
          Value := Line;
          Line := '';
        end;
        Cells[C,R] := Value;
        Inc(C);
      end;

      // Move to next row
      Inc(R);
    end;
  end;
end;

end.
