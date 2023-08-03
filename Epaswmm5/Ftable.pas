unit Ftable;

{-------------------------------------------------------------------}
{                    Unit:    Ftable.pas                            }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   MDI child form that displays a tabular time series listing of   }
{   selected variables for a single object or values of a single    }
{   variable for selected objects in a DrawGrid control.            }
{                                                                   }
{   The selection of what to display in the table is contained in   }
{   in a TReportSelection data structure, which is defined in the   }
{   Uglobals.pas unit. The table is created by calling CreateTable. }
{   It gets refreshed (after a new analysis is made) by calling     }
{   RefreshTable.                                                   }
{-------------------------------------------------------------------}

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, DateUtils,
  Forms, Dialogs, Grids, ClipBrd, ExtCtrls, StdCtrls, Math, Vcl.Themes,
  Xprinter, Uglobals, Uutils, Uproject, Printers;

type
  TTableForm = class(TForm)
    Grid1: TDrawGrid;
    Edit1: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Grid1DrawCell(Sender: TObject; vCol, vRow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure Grid1FixedCellClick(Sender: TObject; ACol, ARow: Integer);
  private
    { Private declarations }
    ColLabel   : array[0..MAXCOLS+2] of String;  // Column headings
    ObjTypes   : array[0..MAXCOLS] of Integer;  // Type of each object
    ObjIndexes : array[0..MAXCOLS] of Integer;  // Index of each item
    Table: TReportSelection;                    // Table options
    StartPeriod: LongInt;
    procedure CopyToString(const Fname: String);
    procedure CreatePrintTable(const FirstCol: Integer; var Ncol: Integer);
    procedure GetHeading(const S: String; var H1, H2: String);
    function  GetRowColEntry(const R: LongInt; const C: LongInt): String;
    function  GetRowCount: LongInt;
    function  GetVariableName(const V: Integer): String;
    procedure PrintRow(const R, FirstCol, Ncol: Integer);
  public
    { Public declarations }
    procedure CopyTo;
    function  CreateTable(ReportSelection: TReportSelection): Boolean;
    procedure Print(Destination: TDestination);
    procedure RefreshTable;
    procedure SelectAll;
  end;

//var
//  TableForm: TTableForm;

implementation

{$R *.DFM}

uses Dcopy, Fmain, Fmap, Uoutput;

const
  TXT_TABLE = 'Table - ';
  TXT_TABLE_SYS_RESULTS = 'Table - System Results';
  TXT_DATE = 'Date';
  TXT_TIME = 'Time';
  TXT_DAYS = 'Days';
  TXT_HOURS = 'Hours';

  ObjectTypeLabel: array[SUBCATCHMENTS..LINKS] of String =
    ('Subcatch', 'Node', 'Link');


procedure TTableForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
// Form's OnCreate handler.
//-----------------------------------------------------------------------------
begin
  // Set row heights and column widths for the display grid
  with Grid1 do
  begin
    DefaultRowHeight := Edit1.Height - 2;
    RowHeights[0] := 2 * DefaultRowHeight;
    DefaultColWidth := 100;
  end;

  // Create a stringlist to hold the ID labels of the objects
  // being tabulated in a Table by Variable
  Table.Items := TStringlist.Create;
end;

procedure TTableForm.FormClose(Sender: TObject; var Action: TCloseAction);
//-----------------------------------------------------------------------------
// Form's OnClose handler.
//-----------------------------------------------------------------------------
begin
  Table.Items.Free;
  Action := caFree;
end;

procedure TTableForm.Grid1DrawCell(Sender: TObject; vCol, vRow: Integer;
  Rect: TRect; State: TGridDrawState);
//-----------------------------------------------------------------------------
// OnDrawCell handler for the Grid1 DrawGrid component.
//-----------------------------------------------------------------------------
var
  x : Integer;
  s : String;
  N : Integer;
  T : TDateTime;
  offset: Integer;

begin
  with Sender as TDrawGrid do
  begin

    // Fill background of fixed cell
    offset := Canvas.TextWidth('0');
    if (vRow < FixedRows) or (vCol < FixedCols) then
    begin
      if StyleServices.Enabled then
      begin
        Canvas.Brush.Color := StyleServices.GetStyleColor(scPanel);
        Canvas.FillRect(Rect);
      end;
    end;

    // Draw column headings in row 0
    if (vRow = 0) then
    begin
      // Add some vertical margin in the cell's Rect
      InflateRect(Rect, 0, -2);

      // Use Win API DrawText function to enable word-wraping
      DrawText(Canvas.Handle, PChar(ColLabel[vCol]), -1, Rect,
        DT_CENTER or DT_VCENTER or DT_WORDBREAK);
    end

    // Draw cell value for body of table
    else
    begin
       // Right align contents of all columns
        SetTextAlign(Canvas.Handle, TA_RIGHT);
        x := Rect.Right - offset;

      // Place date (or days) in column 0
      N := StartPeriod + vRow - 1;
      if vCol = 0 then
      begin
        T := IncSecond(StartDateTime, N*ReportStep);
        if Table.DateTimeDisplay then
          s := DateToStr(T, MyFormatSettings)
        else
        begin
          T := (N + StartOffset) * DeltaDateTime;
          s := IntToStr(Floor(T));
        end;
      end

      // Place time in column 1
      else if vCol = 1 then
      begin
        if Table.DateTimeDisplay then
        begin
          T := IncSecond(StartDateTime, N*ReportStep);
          s := TimeToStr(T, MyFormatSettings);
        end
        else
        begin
          T := IncSecond(0, (N + StartOffset) * ReportStep);
          s := TimeToStr(T, MyFormatSettings);
        end;
      end

      // For other columns, get string representation of value to display
      else s := GetRowColEntry(vRow, vCol);
{
      // Get colors to use if cell is selected
      if (gdSelected in State) then
      begin
        Canvas.Brush.Color := clWindow; //clHighlight;
        //Canvas.Font.Color := clHighlightText;
      end;
}
      // Draw the string value in the cell
      Canvas.FillRect(Rect);
      SetBkMode(Canvas.Handle, TRANSPARENT);
      Canvas.TextOut(x, Rect.Top+2, s);
    end;
  end;
end;

procedure TTableForm.Grid1FixedCellClick(Sender: TObject; ACol, ARow: Integer);
var
  gRect : TGridRect;
begin
  with Sender as TDrawGrid do
  begin

    // Mouse pressed in a column of row 0
    if (ARow = 0) then
    begin

      // Column is > 1, select all rows in the column
      if (ACol > 1) then
      begin
        gRect.Left := ACol;
        gRect.Right := ACol;
        gRect.Top := FixedRows;
        gRect.Bottom := RowCount-FixedRows;
        Selection := gRect;
      end

      // Column is 0, select all columns & rows
      else SelectAll;
    end;

  end;
end;

function TTableForm.GetRowColEntry(const R: LongInt; const C: LongInt): String;
//-----------------------------------------------------------------------------
// Constructs the string representation of the value to display in row R
// and column C of the table.
//-----------------------------------------------------------------------------
var
  V: Integer;      // Variable index
  I: Integer;      // Object type index
  J: Integer;      // Item index
  K: LongInt;
  N: LongInt;      // Time period
begin
   N := StartPeriod + R - 1;
   with Table do
   begin

     // Determine which variable to retrieve
     if ReportType = TABLEBYOBJECT
     then K := C - Grid1.FixedCols
     else K := 0;
     V := Variables[K];

     // Determine which object to retrieve a value for
     if ReportType = TABLEBYVARIABLE
     then K := C - Grid1.FixedCols
     else K := 0;

     // Display 'N/A' if no simulation result is available
     if not RunFlag then Result := 'N/A'

     // Retrieve system result if a system variable is being displayed
     else if ObjectType = SYS
     then Result := FloatToStrF(Uoutput.GetSysOutVal(V, N), ffFixed, 7, 2)

     // Retrieve object result for other types of objects
     else
     begin
       I := ObjTypes[K];
       J := ObjIndexes[K];
       case ObjectType of
       SUBCATCHMENTS: Result := Uoutput.GetSubcatchValStr(V, N, J);
       NODES:         Result := Uoutput.GetNodeValStr(V, N, I, J);
       LINKS:         Result := Uoutput.GetLinkValStr(V, N, I, J);
       else           Result := 'N/A';
       end;
     end;
  end;
end;

function TTableForm.CreateTable(ReportSelection: TReportSelection): Boolean;
//-----------------------------------------------------------------------------
// Creates a table to display the selections specified in ReportSelection.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  // Transfer the report selections to the form's Table variable
  with ReportSelection do
  begin
    Table.ReportType := ReportType;
    Table.VariableCount := VariableCount;
    if VariableCount >= MAXCOLS then Table.VariableCount := MAXCOLS;
    for I := 0 to Table.VariableCount-1 do
    begin
      Table.Variables[I] := Variables[I];
    end;
    Table.ObjectType := ObjectType;
    Table.StartDateIndex := StartDateIndex;
    Table.EndDateIndex := EndDateIndex;
    if Assigned(Items)
    then Uutils.CopyStringList(Items, Table.Items)
    else Table.Items := nil;
    if   ObjectType = SYS
    then Table.ItemCount := 1
    else Table.ItemCount := ItemCount;
    if ItemCount >= MAXCOLS then Table.ItemCount := MAXCOLS;
    Table.DateTimeDisplay := DateTimeDisplay;
  end;

  // Reconstruct and display the table.
  RefreshTable;
  Result := True;
end;

procedure TTableForm.RefreshTable;
//-----------------------------------------------------------------------------
// Reconstructs the table
//-----------------------------------------------------------------------------
var
  Flag: Boolean;
  I: Integer;           // Object type index
  J: Integer;           // Item index
  K: Integer;           // Table item index
  N: Integer;           // Table item index
  ID: String;           // Object ID name
  S1, S2: String;
begin
  // Set labels for the date/time column headings
  if Table.DateTimeDisplay then
  begin
    ColLabel[0] := '' + #13 + TXT_DATE;
    ColLabel[1] := '' + #13 + TXT_TIME;
    Grid1.ColWidths[0] := Grid1.DefaultColWidth;
  end
  else
  begin
    ColLabel[0] := 'Elapsed' + #13 + TXT_DAYS;
    ColLabel[1] := 'Elapsed' + #13 + TXT_HOURS;
    Grid1.ColWidths[0] := 2 * Grid1.DefaultColWidth div 3;
  end;

  // Set heading labels for remaining columns
  N := 0;
  with Table do
  begin
    for K := 0 to ItemCount - 1 do
    begin

      // Get ID name, type and index of k-th object being displayed
      if K > MAXCOLS then break;
      Flag := False;
      if (ObjectType <> SYS) and (Items <> nil) then
      begin
        ID := Items[K];
        if ObjectType = SUBCATCHMENTS
        then Flag := Project.FindSubcatch(ID, I, J)
        else if ObjectType = NODES
        then Flag := Project.FindNode(ID, I, J)
        else if ObjectType = LINKS
        then Flag := Project.FindLink(ID, I, J);
      end;

      // If object exists ...
      if (Flag = True) then
      begin

        // For Table By Variable, use object ID as column headings
        if (ReportType = TABLEBYVARIABLE)
        then ColLabel[N+2] := ObjectTypeLabel[ObjectType] + #13 + ID;
        ObjTypes[N]   := I;
        ObjIndexes[N] := J;
        Inc(N);
      end;
    end;

    // For Table by Object, use variable names as column headings
    if ReportType = TABLEBYOBJECT then
    begin
      for I := 0 to VariableCount-1 do
        ColLabel[I+2] := GetVariableName(I);
    end;

    // Set number of columns in the table's DrawGrid component
    if ReportType = TABLEBYVARIABLE then Grid1.ColCount := N + 2;
    if ReportType = TABLEBYOBJECT   then Grid1.ColCount := VariableCount + 2;

    // Set the form's caption
    if ReportType = TABLEBYVARIABLE then
    begin
      GetHeading(GetVariableName(0), S1, S2);
      Caption := TXT_TABLE + ObjectTypeLabel[ObjectType] + ' ' + S1;
    end;
    if ReportType = TABLEBYOBJECT then
    begin
      if ObjectType = SYS
      then Caption := TXT_TABLE_SYS_RESULTS
      else Caption := TXT_TABLE + ObjectTypeLabel[ObjectType] + ' ' + ID;
    end;
  end;

  // Set width of form to match that of the table
   Self.ClientWidth := (Grid1.ColCount-1) * (Grid1.DefaultColWidth + 2) +
     Grid1.ColWidths[0] + 2 + GetSystemMetrics(SM_CXVSCROLL);

  // Refresh the table's display in the Grid1 control
  Grid1.RowCount := GetRowCount;
  Grid1.Refresh;
  Grid1.SetFocus;
end;

function TTableForm.GetVariableName(const V: Integer): String;
//-----------------------------------------------------------------------------
// Finds the name and units of variable V that is included in the table.
//-----------------------------------------------------------------------------
var
  ObjName: String;
  VarName: String;
  VarUnits: String;
begin
  Uglobals.GetObjVarNames(Table.ObjectType, Table.Variables[V], ObjName,
    VarName, VarUnits);
  if Length(VarUnits) > 0 then VarName := VarName + #13 + '(' + VarUnits + ')';
  Result := VarName;
end;

function  TTableForm.GetRowCount: LongInt;
//-----------------------------------------------------------------------------
// Determines the number of rows in the table.
//-----------------------------------------------------------------------------
var
  LastPeriod : LongInt;
  N          : LongInt;
begin
  with Table do
  begin
    StartPeriod := Uglobals.GetPeriod(StartDateIndex);
    LastPeriod := Uglobals.GetPeriod(EndDateIndex);
    N := LastPeriod - StartPeriod + 1;
    if N > Nperiods then N := Nperiods;
  end;
  Result := N + 1;
end;

procedure TTableForm.GetHeading(const S: String; var H1, H2: String);
//-----------------------------------------------------------------------------
// Converts a column heading S into two separate lines of text
//-----------------------------------------------------------------------------
var
  p: Integer;
begin
  H1 := ' ';
  H2 := ' ';
  p := Pos(#13, S);
  if (p = 0) then
  begin
    H1 := S;
  end
  else
  begin
    H1 := Copy(S,1,p-1);
    if (Length(H1) = 0) then H1 := ' ';
    if p < Length(S) then
      H2 := Copy(S,p+1,Length(S)-p);
  end;
end;

procedure TTableForm.SelectAll;
//-----------------------------------------------------------------------------
// Selects all cells within the table.
//-----------------------------------------------------------------------------
var
  gRect : TGridRect;
begin
  with Grid1 do
  begin
    gRect.Left := FixedCols;
    gRect.Right := ColCount - FixedCols + 1;
    gRect.Top := FixedRows;
    gRect.Bottom := RowCount - FixedRows;
    Selection := gRect;
  end;
end;

procedure TTableForm.CopyTo;
//-----------------------------------------------------------------------------
// Copies the selected cells of the table to a file or to the Clipboard.
//-----------------------------------------------------------------------------
var
  CopyToForm: TCopyToForm;
begin
  // Launch the CopyTo dialog form
  CopyToForm := TCopyToForm.Create(self);
  with CopyToForm do
  try

    // Fix the format type to text
    FormatGroup.ItemIndex := 2;
    FormatGroup.Enabled := False;

    // Call CopyToString to do the actual copying
    if ShowModal = mrOK
    then CopyToString(DestFileName);
  finally
    Free;
  end;
end;

procedure TTableForm.CopyToString(const Fname: String);
//-----------------------------------------------------------------------------
// Copies the current selection of cells from the table into a stringlist
// which is then saved to file Fname or to the Clipboard if Fname is blank.
//-----------------------------------------------------------------------------
var
  Slist: TStringList;
  C1,C2,C,R  : LongInt;
  h1,h2: String;
  S,S1,S2: String;

  N: LongInt;
  T: TDateTime;

begin
  // Create a stringlist to hold each row of selected cells
  Slist := TStringList.Create;
  try
    with Grid1.Selection do
    begin

      // Add titles to the stringlist
      Slist.Add(Project.Title);
      Slist.Add(Caption);

      // Add column headings to the stringlist
      GetHeading(ColLabel[0], h1, h2);
      S1 := Format('%-10s',[h1]);
      S2 := Format('%-10s',[h2]);
      GetHeading(ColLabel[1], h1, h2);
      S1 := S1 + #9 + Format('%-10s',[h1]);
      S2 := S2 + #9 + Format('%-10s',[h2]);
      C1 := Left;
      if C1 = 0 then C1 := 2;
      C2 := Right;
      if C2 >= Grid1.ColCount then C2 := Grid1.ColCount-1;
      for C := C1 to C2 do
      begin
        GetHeading(ColLabel[C], h1, h2);
        S1 := S1 + #9 + Format('%-16s',[h1]);
        S2 := S2 + #9 + Format('%-16s',[h2]);
      end;
      Slist.Add(S1);
      Slist.Add(S2);

      // Iterate through each row of the selected block of cells
      for R := Top to Bottom do
      begin

        // Convert Date/Time or Elapsed Time entries to a string (S1)
        N := StartPeriod + R - 1;
        if Table.DateTimeDisplay then
        begin

          T := IncSecond(StartDateTime, N*ReportStep);
          S := DateToStr(T, MyFormatSettings);
          S1 := Format('%-10s', [S]);
          S := TimeToStr(T, MyFormatSettings);
          S1 := S1 + #9 + Format('%-10s',[S]);
        end
        else
        begin
          T := (N + StartOffset) * DeltaDateTime;
          S := IntToStr(Floor(T));
          S1 := Format('%-10s', [S]);
          T := IncSecond(0, (N + StartOffset) * ReportStep);
          S := TimeToStr(T, MyFormatSettings);
          S1 := S1 + #9 + Format('%-10s', [S]);
        end;

        // Build up a tab-delimited string of entries in the selected columns
        for C := C1 to C2 do
          S1 := S1 + #9 + Format('%-16s',[GetRowColEntry(R,C)]);

        // Add the tab-delimited string to the stringlist
        Slist.Add(S1);
      end;
    end;

    // Save the stringlist to file Fname if file name supplied
    if Length(Fname) > 0 then Slist.SaveToFile(Fname)

    // Otherwise place the text of the stringlist onto the clipboard
    else Clipboard.SetTextBuf(PChar(Slist.Text));

  // Free the stringlist.
  finally
    Slist.Free;
  end;
end;

procedure TTableForm.Print(Destination: TDestination);
//-----------------------------------------------------------------------------
// Prints the table to Destination (the printer or the preview form).
//-----------------------------------------------------------------------------
var
  FirstCol,
  Ncol      : Integer;
  r         : LongInt;
begin
  with MainForm.thePrinter do
  begin

    // Initialize the Printer object
    Screen.Cursor := crHourglass;
    BeginJob;
    SetDestination(Destination);
    SetFontInformation('Times New Roman',11,[]);
    PrintCenter(Caption);
    NewLines(2);

    // Continue until all columns are printed
    FirstCol := 2;
    while FirstCol < Grid1.ColCount do
    begin

      // Create a table with enough columns that fit across page
      CreatePrintTable(FirstCol,Ncol);
      BeginTable;

      // Transfer contents of the Grid1 table to the printed table
      with Grid1 do
      begin
        for r := 1 to RowCount - 1 do
        begin
          PrintRow(r,FirstCol,Ncol);
          NextTableRow( (r >= RowCount-1) );
        end;
      end;
      EndTable;

      // Continue creating a new table for remaining columns
      FirstCol := FirstCol + Ncol - 2;
      if (FirstCol < Grid1.ColCount) then
      begin
        NewPage;
        PrintCenter(Caption);
        NewLines(2);
      end;
    end;
    Screen.Cursor := crDefault;
    EndJob;
  end;
end;

procedure TTableForm.CreatePrintTable(const FirstCol: Integer;
  var Ncol: Integer);
//-----------------------------------------------------------------------------
// Creates a printed table with Ncol columns starting from column FirstCol.
//-----------------------------------------------------------------------------
const
  ColWidth = 1.0;
  ColWidth1 = 1.75;
var
  i, j      : Integer;
  LastCol   : Integer;
  W, L,
  Left,
  Width,
  Width1    : Single;
  h1,h2     : String;
begin
  // Assign width to column 1
  Width1 := ColWidth;

  // Determine how many remaining columns fit on a page
  with MainForm.ThePrinter do
  begin

    // Get left margin & printable page width
    with PageLayout do
    begin
      Left := LMargin;
      Width := GetPageWidth - LMargin - RMargin;
    end;

    // Start with 2 columns
    Ncol := 2;
    W := 2*Width1;
    LastCol := FirstCol;

    // Add columns until we run out of space or columns
    while LastCol < Grid1.ColCount do
    begin
      if W + ColWidth > Width then break;
      W := W + ColWidth;
      Inc(Ncol);
      Inc(LastCol);
    end;

    // Create a table on thePrinter object and define its column properties
    CreateTable(Ncol);
    L := Left + (Width - W)/2;
    GetHeading(ColLabel[0], h1, h2);
    SetColumnHeaderText(1,1,h1);
    SetColumnHeaderText(1,2,h2);

    //SetColumnHeaderAlignment(1,jLeft);
    SetColumnHeaderAlignment(1,jCenter);
    SetColumnDimensions(1,L,Width1);
    L := L + Width1;
    GetHeading(ColLabel[1], h1, h2);
    SetColumnHeaderText(2,1,h1);
    SetColumnHeaderText(2,2,h2);
    SetColumnHeaderAlignment(2,jCenter);

    SetColumnDimensions(2,L,Width1);
    L := L + Width1;
    for i := 3 to Ncol do
    begin
      j := FirstCol + i - 3;
      GetHeading(ColLabel[j], h1, h2);
      SetColumnHeaderText(i,1,h1);
      SetColumnHeaderText(i,2,h2);
      SetColumnHeaderAlignment(i,jCenter);
      SetColumnDimensions(i,L,ColWidth);
      L := L + ColWidth;
    end;
    SetTableStyle([sBorder, sVerticalGrid, sHorizontalGrid]);
  end;
end;

procedure TTableForm.PrintRow(const R: LongInt; const FirstCol, Ncol: Integer);
//-----------------------------------------------------------------------------
// Prints the Date/Time (or Elapsed Time) column and the Ncol columns
// starting with FirstCol for row R to the current page.
//-----------------------------------------------------------------------------
var
  i,j: LongInt;
  ElapsedTime : TDateTime;
  T: TdateTime;
begin
  with MainForm.thePrinter do
  begin
    if Table.DateTimeDisplay then
    begin
      T := IncSecond(StartDateTime, (R-1)*ReportStep);
      PrintColumnCenter(1, DateToStr(T, MyFormatSettings));
      PrintColumnCenter(2, TimeToStr(T, MyFormatSettings));
    end
    else
    begin
      ElapsedTime := (StartPeriod + R) * DeltaDateTime;
      PrintColumnCenter(1, IntToStr(Floor(ElapsedTime)));
      PrintColumnCenter(2, FormatDateTime('hh:nn:ss',
                      ElapsedTime - Floor(ElapsedTime)));
    end;
    for i := 3 to Ncol do
    begin
      j := FirstCol + i - 3;
      PrintColumnRight(i,GetRowColEntry(r,j));
    end;
  end;
end;

end.
