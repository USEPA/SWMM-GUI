unit Fresults;

{-------------------------------------------------------------------}
{                    Unit:    Fresults.pas                          }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    10/08/22 (5.2.2)                      }
{                    Author:  L. Rossman                            }
{                                                                   }
{   MDI child form that displays summary results generated from     }
{   a run of SWMM.                                                  }
{-------------------------------------------------------------------}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.Grids, Vcl.StdCtrls, Vcl.CheckLst, Vcl.ExtCtrls, Vcl.Themes,
  System.StrUtils, ClipBrd, Xprinter, Printers, Uresults;

type
  TResultsForm = class(TForm)
    TopPanel: TPanel;
    Panel1: TPanel;
    TopicsListBox: TComboBox;
    TheGrid: TStringGrid;
    Panel3: TPanel;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TopicsListBoxClickCheck(Sender: TObject);
    procedure TheGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
    procedure TheGridFixedCellClick(Sender: TObject; ACol, ARow: Integer);
  private
    { Private declarations }
    function  ColumnIsText(aCol: Integer; Topic: TTopic): Boolean;
    procedure CopyToString(const Fname: String);
    procedure CreatePrintTable(const FirstCol: Integer; var Ncol: Integer);
    procedure ParseColHeader(const S: String; var H: array of String);
    procedure PrintRow(const R: Integer; const FirstCol, Ncol: Integer);
    procedure RefreshTopic;
    procedure SetColHeaders(Topic: TTopic);
  public
    { Public declarations }
    procedure CopyTo;
    function  RefreshReport: Boolean;
    procedure Print(Destination: TDestination);
  end;

var
  ResultsForm: TResultsForm;

implementation

{$R *.dfm}

uses
  Dcopy, Fmain, Uproject, Ubrowser, Uglobals, Uutils;

const
  Caption1: String =
    'Click a column header to sort the column. ';
  Caption2: String =
    'Flow class frequencies are fraction of all time steps. ';
  TypeLabel: String = #10#10#10'Type';
  LastTopic = 15;

  TopicLabels: array[0..LastTopic] of String =
    ('Subcatchment Runoff',
     'LID Performance',
     'Groundwater',
     'Subcatchment Washoff',
     'Node Depth',
     'Node Inflow',
     'Node Surcharge',
     'Node Flooding',
     'Storage Volume',
     'Outfall Loading',
     'Street Flow',
     'Link Flow',
     'Flow Classification',
     'Conduit Surcharge',
     'Pumping',
     'Link Pollutant Load');

  // Nominal object types for each report topic
  ObjectType: array[0..LastTopic] of Integer =
    (SUBCATCH, SUBCATCH, SUBCATCH, SUBCATCH,
     JUNCTION, JUNCTION, JUNCTION, JUNCTION, JUNCTION, JUNCTION,
     CONDUIT, CONDUIT, CONDUIT, CONDUIT, CONDUIT, CONDUIT);

var
  CopiedHeaders: array[0..3] of String;
  ColHeaders: array of String;
  ColSorted: array of Integer;
  SortedCol: Integer;
  CurrentTopic: TTopic;
  FixedCellColor: TColor;

procedure TResultsForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
// Form's OnCreate handler.
//-----------------------------------------------------------------------------
var
  W: Integer;
begin
  Label1.Caption := Caption1;
  W := Canvas.TextWidth('W');
  with TheGrid do
  begin
    Align := alClient;
    ColCount := 11;
    FixedCellColor := FixedColor;
    DefaultColWidth := 8 * W;
    ColWidths[0] := 12 * W;
    ColWidths[1] := 8 * W;
    DefaultRowHeight := Edit1.Height - 2;
    RowHeights[0] := 4 * (DefaultRowHeight - 4);
  end;
  TheGrid.Visible := False;
//  TopPanel.Visible := False;
  CurrentTopic := rtRunoff;
end;

procedure TResultsForm.FormClose(Sender: TObject; var Action: TCloseAction);
//-----------------------------------------------------------------------------
// Form's OnClose handler.
//-----------------------------------------------------------------------------
begin
  Action := caFree;
end;

function TResultsForm.RefreshReport: Boolean;
//-----------------------------------------------------------------------------
// Refreshes the form when new simulation results are obtained.
//-----------------------------------------------------------------------------
var
  I, K: Integer;
begin

  // Clear all report topic info
  Result := False;
  TopicsListBox.Clear;

  // Add labels for results topics to TopicsListBox
  K := -1;
  for I := 0 to High(Topics) do
  begin
    if TopicStart[I] >= 0 then
    begin
      Inc(K);
      Topics[K] := TTopic(I);
      TopicsListBox.Items.Add(TopicLabels[I]);
    end;
  end;

  if TopicsListBox.Items.Count = 0 then
  begin
    Label1.Caption := 'There are no results to display.';
    TopicsListBox.Visible := False;
    TheGrid.Visible := False;
    exit;
  end;

  // Set the current selected topic
  if Ord(CurrentTopic) > K  then CurrentTopic := rtRunoff;
  if K >= 0 then
  begin
   TopicsListBox.ItemIndex := Ord(CurrentTopic);
   RefreshTopic;
  end;

  TopPanel.Visible := True;
  TheGrid.Visible := True;
  Result := True;
end;

procedure TResultsForm.TheGridDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
//-----------------------------------------------------------------------------
// String grid's OnDrawCell handler.
//-----------------------------------------------------------------------------
var
  x : Integer;
  s : String;
  offset: Integer;

begin
  offset := Canvas.TextWidth('0');
  with Sender as TStringGrid do
  begin
    // Fill background of cell
    if (ARow < FixedRows) or (ACol < FixedCols) then
    begin
      if StyleServices.Enabled then
      begin
        Canvas.Brush.Color := StyleServices.GetStyleColor(scPanel);
        //Rect.Left := Rect.Left - 4;
      end;
    end;
    Canvas.FillRect(Rect);

    // Draw contents of header row
    if (ARow = 0) then
    begin

      // Use bold font for sorted column
      Canvas.Font.Style := [];
      if (ACol = SortedCol) and (ColSorted[ACol] <> 0)
      then Canvas.Font.Style := [fsBold];

      // Use Windows API DrawText function to draw multi-line header
      InflateRect(Rect, 0, -2);
      DrawText(Canvas.Handle, PChar(ColHeaders[ACol]), -1, Rect,
        DT_CENTER or DT_WORDBREAK);
      Canvas.Font.Style := [];
    end

    // Draw cell value for body of table
    else
    begin
      // Use left alignment if column is non-numeric
      if ColumnIsText(ACol, CurrentTopic) then
      begin
        SetTextAlign(Canvas.Handle, TA_LEFT);
        x := Rect.Left + offset;
      end

      // Other columns are aligned right
      else
      begin
        SetTextAlign(Canvas.Handle, TA_RIGHT);
        x := Rect.Right - offset;
      end;

      // Draw the string value in the cell
      SetBkMode(Canvas.Handle, TRANSPARENT);
      s := Cells[ACol, ARow];
      Canvas.TextOut(x, Rect.Top+2, s);
    end;
  end;
end;

procedure TResultsForm.TheGridFixedCellClick(Sender: TObject; ACol, ARow: Integer);
//-----------------------------------------------------------------------------
// OnFixedCellClick handler for the StringGrid.
//-----------------------------------------------------------------------------
var
  Numeric: Boolean;
  ObjType: Integer;
  ObjName: String;
  FoundObject: Integer;
  FoundIndex:  Integer;
  Found: Boolean;
begin
  // Detect if an object name was clicked
  if (ACol = 0) and (Arow > 0) then
  begin
    ObjType := ObjectType[Integer(Topics[Ord(CurrentTopic)])];
    ObjName := TheGrid.Cells[ACol, ARow];

    Found := False;
    if Project.IsSubcatch(ObjType)
    then Found := Project.FindSubcatch(ObjName, FoundObject, FoundIndex)
    else if Project.IsNode(ObjType)
    then Found := Project.FindNode(ObjName, FoundObject, FoundIndex)
    else if Project.IsLink(ObjType)
    then Found := Project.FindLink(ObjName, FoundObject, FoundIndex);
    if Found then
    begin
      Ubrowser.BrowserUpdate(FoundObject, FoundIndex);
      TheGrid.Row := Arow;
    end;
    exit;
  end;

  // Otherwise sort the column clicked on.
  // Determine if column holds numeric or text values.
  Numeric := True;
  if ColumnIsText(ACol, CurrentTopic) then
    Numeric := False;

  // Update the sorted state of the column
  if ColSorted[ACol] = 0 then
    ColSorted[ACol] := 1
  else
    ColSorted[ACol] := - ColSorted[ACol];
  SortedCol := ACol;

  // Sort the grid by the entries in the selected column
  Uutils.SortStringGrid(TheGrid, ACol, Numeric, ColSorted[ACol]);
end;

procedure TResultsForm.TopicsListBoxClickCheck(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClickCheck handler for the Topics combobox - displays the summary
// results for the selected topic.
//-----------------------------------------------------------------------------
begin
  with TopicsListBox do
  begin
    // Display report if new topic selected
    if ItemIndex <> Integer(CurrentTopic) then
    begin
      CurrentTopic := TTopic(ItemIndex);
      RefreshTopic;
      TheGrid.Row := 1;
      TheGrid.Col := 1;
    end;
  end;
end;

procedure TResultsForm.RefreshTopic;
//-----------------------------------------------------------------------------
// Displays summary results for the currently selected topic.
//-----------------------------------------------------------------------------
var
  I: Integer;
  Topic: TTopic;
  Caption: String;
begin
  // Convert from index of available topics to absolute topic index
  Topic := Topics[Ord(CurrentTopic)];
  SortedCol := -1;

  // Clear contents of the report table grid
  for I := 0 to TheGrid.RowCount - 1 do TheGrid.Rows[I].Clear;

  // Assign caption text for topic
  Caption := Caption1;
  if Topic = rtFlowClass then Caption := Caption + Caption2;  // Flow Classiifcation topic
  Label1.Caption := Caption;

  // Provide a minimum size grid for topics with no results
  if TopicSize[Ord(Topic)] = 0 then
  begin
    TheGrid.FixedRows := 1;
    TheGrid.RowCount := 2;
  end

  // Otherwise open the SWMM report file
  else if FileExists(Uglobals.TempReportFile) then
  begin
    // Set number of rows in the grid
    TheGrid.FixedRows := 1;
    TheGrid.RowCount := TopicSize[Ord(Topic)] + TheGrid.FixedRows;

    // Set text of column headers
    SetColHeaders(Topic);

    // Re-size and refresh the grid
    TheGrid.ColCount := Length(ColHeaders);
    SetLength(ColSorted, TheGrid.ColCount);
    for I := 0 to TheGrid.ColCount - 1 do ColSorted[I] := 0;

    Uresults.PopulateGrid(Ord(Topic), TheGrid);
    TheGrid.Refresh;
  end;
end;

procedure TResultsForm.SetColHeaders(Topic: TTopic);
//-----------------------------------------------------------------------------
//  Forms the multi-line column headers that appear in the grid table
//  for a specific report topic.
//-----------------------------------------------------------------------------
var
  I, N, T: Integer;
  Tokens: TStringList;
  Units1, Units2, Units3, Text1: String;
begin
  T := Ord(Topic);
  case Topic of
    rtRunoff: // Subcatchment Runoff (3 lines with 11 cols)
    begin
      // Get units appearing on 3rd header line
      Units1 := Copy(TopicHeaderLines[T][2], 32, 2);
      Units2 := Copy(TopicHeaderLines[T][2], 104, 9);
      Units3 := Copy(TopicHeaderLines[T][2], 118, 4);

      // Build up each column heading
      SetLength(ColHeaders, 11);
      ColHeaders[0] := #10#10#10'Subcatchment';
      ColHeaders[1] := #10'Total'#10'Precip'#10 + Units1;
      ColHeaders[2] := #10'Total'#10'Runon'#10 + Units1;
      ColHeaders[3] := #10'Total'#10'Evap'#10 + Units1;
      ColHeaders[4] := #10'Total'#10'Infil'#10 + Units1;
      ColHeaders[5] := #10'Imperv'#10'Runoff'#10 + Units1;
      ColHeaders[6] := #10'Perv'#10'Runoff'#10 + Units1;
      ColHeaders[7] := #10'Total'#10'Runoff'#10 + Units1;
      ColHeaders[8] := #10'Total'#10'Runoff'#10 + Units2;
      ColHeaders[9] := #10'Peak'#10'Runoff'#10 + Units3;
      ColHeaders[10] := #10#10'Runoff'#10'Coeff';
      CopiedHeaders[0] := Format('%-16s',[' ']);
    end;

    rtLID: // LID Performance
    begin
      Units1 := Trim(Copy(TopicHeaderLines[T][2], 44, 3));
      SetLength(ColHeaders, 10);
      ColHeaders[0] := #10#10#10'Subcatchment';
      ColHeaders[1] := #10#10#10'LID Control';
      ColHeaders[2] := #10'Total'#10'Inflow'#10 + Units1;
      ColHeaders[3] := #10'Evap'#10'Loss'#10 + Units1;
      ColHeaders[4] := #10'Infil'#10'Loss'#10 + Units1;
      ColHeaders[5] := #10'Surface'#10'Outflow'#10 + Units1;
      ColHeaders[6] := #10'Drain'#10'Outflow'#10 + Units1;
      ColHeaders[7] := #10'Initial'#10'Storage'#10 + Units1;
      ColHeaders[8] := #10'Final'#10'Storage'#10 + Units1;
      ColHeaders[9] := #10'Continuity'#10'Error'#10'%';
    end;

    rtGroundwater:  // Groundwater statistics
    begin
      Units1 := Copy(TopicHeaderLines[T][3], 30, 2);
      Units2 := Copy(TopicHeaderLines[T][3], 65, 3);
      Units3 := Copy(TopicHeaderLines[T][3], 84, 2);
      SetLength(ColHeaders, 10);
      ColHeaders[0] := #10#10#10'Subcatchment';
      ColHeaders[1] := #10'Total'#10'Infil'#10 + Units1;
      ColHeaders[2] := #10'Total'#10'Evap'#10 + Units1;
      ColHeaders[3] := 'Total'#10'Lower'#10'Seepage'#10 + Units1;
      ColHeaders[4] := 'Total'#10'Lateral'#10'Outflow'#10 + Units1;
      ColHeaders[5] := 'Maximum'#10'Lateral'#10'Outflow'#10 + Units2;
      ColHeaders[6] := 'Average'#10'Upper'#10'Moisture'#10;
      ColHeaders[7] := 'Average'#10'Water'#10'Table'#10 + Units3;
      ColHeaders[8] := 'Final'#10'Upper'#10'Moisture'#10;
      ColHeaders[9] := 'Final'#10'Water'#10'Table'#10 + Units3;
    end;

    rtWashoff: // Washoff Loads
    begin
      Tokens := TStringList.Create;
      try
        // Tokenize header with pollutant names
        Uutils.Tokenize(TopicHeaderLines[T][0], Tokens, N);

        // Set number of header columns to number of pollutant names
        // + one column for subcatchment name
        SetLength(ColHeaders, N+1);

        // Fill in first line of grid column headers
        ColHeaders[0] := #10#10#10;
        for I := 0 to N-1 do ColHeaders[I+1] := #10#10 + Tokens[I] + #10;

        // Fill in second line of column headers (with mass units)
        Uutils.Tokenize(TopicHeaderLines[T][1], Tokens, N);
        for I := 0 to N-1 do ColHeaders[I] := ColHeaders[I] + Tokens[I];
      finally
        Tokens.Free;
      end;
    end;

    rtNodeDepth: // Node Depth
    begin
      Units1 := Trim(Copy(TopicHeaderLines[T][2], 35, 6));
      SetLength(ColHeaders, 8);
      ColHeaders[0] := #10#10#10'Node';
      ColHeaders[1] := TypeLabel;
      ColHeaders[2] := #10'Average'#10'Depth'#10 + Units1;
      ColHeaders[3] := #10'Maximum'#10'Depth'#10 + Units1;
      ColHeaders[4] := #10'Maximum'#10'HGL'#10 + Units1;
      ColHeaders[5] := #10'Day of'#10'Maximum'#10'Depth';
      ColHeaders[6] := #10'Hour of'#10'Maximum'#10'Depth';
      ColHeaders[7] := 'Maximum'#10'Reported'#10'Depth'#10 + Units1;
    end;

    rtNodeInflow: // Node Inflows
    begin
      Units1 := Trim(Copy(TopicHeaderLines[T][3], 38, 4));
      Units2 := Trim(Copy(TopicHeaderLines[T][3], 68, 8));
      SetLength(ColHeaders, 9);
      ColHeaders[0] := #10#10#10'Node';
      ColHeaders[1] := TypeLabel;
      ColHeaders[2] := 'Maximum'#10'Lateral'#10'Inflow'#10 + Units1;
      ColHeaders[3] := 'Maximum'#10'Total'#10'Inflow'#10 + Units1;
      ColHeaders[4] := #10'Day of'#10'Maximum'#10'Inflow';
      ColHeaders[5] := #10'Hour of'#10'Maximum'#10'Inflow';
      ColHeaders[6] := 'Lateral'#10'Inflow'#10'Volume'#10 + Units2;
      ColHeaders[7] := 'Total'#10'Inflow'#10'Volume'#10 + Units2;
      ColHeaders[8] := 'Flow'#10'Balance'#10'Error'#10'%';
    end;

    rtNodeSurcharge: // Node Surcharge
    begin
      Units1 := Trim(Copy(TopicHeaderLines[T][2], 53, 6));
      SetLength(ColHeaders, 5);
      ColHeaders[0] := #10#10#10'Node';
      ColHeaders[1] := TypeLabel;
      ColHeaders[2] := #10#10'Hours'#10'Surcharged';
      ColHeaders[3] := 'Max Height'#10'Above'#10'Crown'#10 + Units1;
      ColHeaders[4] := 'Min Depth'#10'Below'#10'Rim'#10 + Units1;
    end;

    rtNodeFlooding: // Node Flooding
    begin
      Units1 := Trim(Copy(TopicHeaderLines[T][3], 38, 4));
      Units2 := Trim(Copy(TopicHeaderLines[T][3], 59, 9));
      Units3 := Trim(Copy(TopicHeaderLines[T][3], 69, 9));
      Text1 := Trim(AnsiRightStr(TopicHeaderLines[T][2], 7));
      SetLength(ColHeaders, 7);
      ColHeaders[0] := #10#10#10'Node';
      ColHeaders[1] := #10#10'Hours'#10'Flooded';
      ColHeaders[2] := #10'Maximum'#10'Rate'#10 + Units1;
      ColHeaders[3] := #10'Day of'#10'Maximum'#10'Flooding';
      ColHeaders[4] := #10'Hour of'#10'Maximum'#10'Flooding';
      ColHeaders[5] := 'Total'#10'Flood'#10'Volume'#10 + Units2;
      ColHeaders[6] := 'Maximum'#10'Ponded'#10 + Text1 + #10 + Units3;
    end;

    rtStorageVolume: // Storage Volume
    begin
      Units1 := Trim(Copy(TopicHeaderLines[T][2], 25, 9));
      Units2 := Trim(Copy(TopicHeaderLines[T][2], 93, 6));
      SetLength(ColHeaders, 10);
      ColHeaders[0] := #10#10'Storage'#10'Unit';
      ColHeaders[1] := #10'Average'#10'Volume'#10+Units1;
      ColHeaders[2] := #10'Average'#10'Percent'#10'Full';
      ColHeaders[3] := #10'Evaporation'#10'Loss'#10'%';
      ColHeaders[4] := #10'Exfiltration'#10'Loss'#10'%';
      ColHeaders[5] := #10'Maximum'#10'Volume'#10+Units1;
      ColHeaders[6] := #10'Maximum'#10'Percent'#10'Full';
      ColHeaders[7] := #10'Day of'#10'Maximum'#10'Volume';
      ColHeaders[8] := #10'Hour of'#10'Maximum'#10'Volume';
      ColHeaders[9] := #10'Maximum'#10'Outflow'#10+Units2;
    end;

    rtOutfallLoading: // Outfall Loading
    begin
       Tokens := TStringList.Create;
      try
        // Tokenize header with pollutant names
        Uutils.Tokenize(TopicHeaderLines[T][1], Tokens, N);

        // Set number of header columns to number of tokens found
        // + one column for outfall name
        SetLength(ColHeaders, N+1);

        // Parse flow and volume units from 3rd header line
        Units1 := Trim(Copy(TopicHeaderLines[T][2], 36, 4));
        Units2 := Trim(Copy(TopicHeaderLines[T][2], 54, 9));

        // Set headers for non-pollutant columns
        ColHeaders[0] := #10#10#10'Outfall Node';
        ColHeaders[1] := #10'Flow'#10'Frequency'#10'%';
        ColHeaders[2] := #10'Avgerage'#10'Flow'#10 + Units1;
        ColHeaders[3] := #10'Maximum'#10'Flow'#10 + Units1;
        ColHeaders[4] := #10'Total'#10'Volume'#10 + Units2;

        if N > 4 then
        begin
          // Add pollutant names to column headers
          for I := 5 to N do
            ColHeaders[I] := #10'Total'#10 + Tokens[I-1] + #10;

          // Trim non-pollutant units text from last header line
          Text1 := AnsiMidStr(TopicHeaderLines[T][2], 63,
            Length(TopicHeaderLines[T][2]));

          // Tokenize the pollutant units from this text
          Uutils.Tokenize(Text1, Tokens, N);

          // Add the pollutant units to the column headers
          for I := 0 to N-1 do
            ColHeaders[I+5] := ColHeaders[I+5] + Tokens[I];
        end;
      finally
        Tokens.Free;
      end;
    end;

    rtStreetFlow:
    begin
      Units1 := Trim(Copy(TopicHeaderLines[T][3], 25, 4));
      Units2 := Trim(Copy(TopicHeaderLines[T][3], 37, 2));
      SetLength(ColHeaders, 13);
      ColHeaders[0] := #10#10#10'Street Conduit';
      ColHeaders[1] := #10'Peak'#10'Flow'#10 + Units1;
      ColHeaders[2] := #10'Max'#10'Spread'#10 + Units2;
      ColHeaders[3] := #10'Max'#10'Depth'#10 + Units2;
      ColHeaders[4] := #10#10'Inlet'#10'Structure';
      ColHeaders[5] := #10#10'Inlet'#10'Location';
      ColHeaders[6] := #10#10'Inlet'#10'Count';
      ColHeaders[7] := #10'Peak Flow'#10'Capture'#10'Percent';
      ColHeaders[8] := #10'Avg. Flow'#10'Capture'#10'Percent';
      ColHeaders[9] := #10'Bypass'#10'Frequency'#10'Percent';
      ColHeaders[10] := #10'Backflow'#10'Frequency'#10'Percent';
      ColHeaders[11] := 'Peak'#10'Capture'#10'Per Inlet'#10 + Units1;
      ColHeaders[12] := 'Peak'#10'Bypass'#10'Flow'#10 + Units1;
    end;

    rtLinkFlow: // Link Flow
    begin
      Units1 := Trim(Copy(TopicHeaderLines[T][2], 38, 4));
      Units2 := Trim(Copy(TopicHeaderLines[T][2], 58, 6));
      SetLength(ColHeaders, 8);
      ColHeaders[0] := #10#10#10'Link';
      ColHeaders[1] := TypeLabel;
      ColHeaders[2] := #10'Maximum'#10'|Flow|'#10 + Units1;
      ColHeaders[3] := #10'Day of'#10'Maximum'#10'Flow';
      ColHeaders[4] := #10'Hour of'#10'Maximum'#10'Flow';
      ColHeaders[5] := #10'Maximum'#10'|Velocity|'#10 + Units2;
      ColHeaders[6] := #10'Max /'#10'Full'#10'Flow';
      ColHeaders[7] := #10'Max /'#10'Full'#10'Depth';
    end;

    rtFlowClass: // Flow Classification
    begin
      SetLength(ColHeaders, 11);
      ColHeaders[0] := #10#10#10'Conduit';
      ColHeaders[1] := #10'Adjusted/'#10'Actual'#10'Length';
      ColHeaders[2] := #10#10'Fully'#10'Dry';
      ColHeaders[3] := #10#10'Upstrm'#10'Dry';
      ColHeaders[4] := #10#10'Dnstrm'#10'Dry';
      ColHeaders[5] := #10#10'Sub'#10'Critical';
      ColHeaders[6] := #10#10'Super'#10'Critical';
      ColHeaders[7] := #10#10'Upstrm'#10'Critical';
      ColHeaders[8] := #10#10'Dnstrm'#10'Critical';
      ColHeaders[9] := #10'Normal'#10'Flow'#10'Limited';
      ColHeaders[10] := #10#10'Inlet'#10'Control';
    end;

    rtConduitSurcharge: // Conduit Surcharge
    begin
      SetLength(ColHeaders, 6);
      ColHeaders[0] := #10#10#10'Conduit';
      ColHeaders[1] := #10'Hours'#10'Both Ends'#10'Full';
      ColHeaders[2] := #10'Hours'#10'Upstream'#10'Full';
      ColHeaders[3] := #10'Hours'#10'Downstream'#10'Full';
      ColHeaders[4] := 'Hours'#10'Above'#10'Normal'#10'Flow';
      ColHeaders[5] := #10'Hours'#10'Capacity'#10'Limited';
    end;

    rtPumping: // Pumping
    begin
      Units1 := Trim(Copy(TopicHeaderLines[T][2], 51, 4));
      Units2 := Trim(Copy(TopicHeaderLines[T][2], 76, 9));
      Units3 := Trim(Copy(TopicHeaderLines[T][2], 89, 6));
      SetLength(ColHeaders, 10);
      ColHeaders[0] := #10#10#10'Pump';
      ColHeaders[1] := #10#10'Percent'#10'Utilized';
      ColHeaders[2] := #10#10'Number of'#10'Start-Ups';
      ColHeaders[3] := #10'Minimum'#10'Flow'#10 + Units1;
      ColHeaders[4] := #10'Average'#10'Flow'#10 + Units1;
      ColHeaders[5] := #10'Maximum'#10'Flow'#10 + Units1;
      ColHeaders[6] := #10'Total'#10'Volume'#10 + Units2;
      ColHeaders[7] := #10'Power'#10'Usage'#10 + Units3;
      ColHeaders[8] := '% Time'#10'Below'#10'Pump'#10'Curve';
      ColHeaders[9] := '% Time'#10'Above'#10'Pump'#10'Curve';
    end;

    rtLinkLoad:  // Link Loadings
    begin
      Tokens := TStringList.Create;
      try
        // Tokenize header with pollutant names
        Uutils.Tokenize(TopicHeaderLines[T][0], Tokens, N);

        // Set number of header columns to number of tokens found
        // + one column for outfall name
        SetLength(ColHeaders, N+1);

        // Add pollutant names to column headers
        ColHeaders[0] := #10#10#10;
        for I := 0 to N-1 do
            ColHeaders[I+1] := #10#10 + Tokens[I] + #10;

        // Tokenize header with pollutant units
        Uutils.Tokenize(TopicHeaderLines[T][1], Tokens, N);

        // Add the pollutant units to the column headers
        for I := 0 to N-1 do
          ColHeaders[I] := ColHeaders[I] + Tokens[I];
      finally
        Tokens.Free;
      end;
    end;
  end;
end;

procedure TResultsForm.CopyTo;
//-----------------------------------------------------------------------------
// Copies the current topic's table to a file or to the Clipboard.
//-----------------------------------------------------------------------------
var
  CopyToForm: TCopyToForm;
begin
  if not TheGrid.Visible then
  begin
    Uutils.MsgDlg('There are no results to copy.', mtInformation, [mbOK]);
    exit;
  end;

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

procedure TResultsForm.CopyToString(const Fname: String);
//-----------------------------------------------------------------------------
// Copies the current table into a stringlist which is then
// saved to file Fname or to the Clipboard if Fname is blank.
//-----------------------------------------------------------------------------
var
  Slist: TStringList;
  S: String;
  Hrows: array[0..3] of String;
  Hcols: array[0..3] of String;
  I: Integer;
  J: Integer;
begin
  // Create a stringlist to hold each row of table
  Slist := TStringList.Create;
  try
    // Add titles to the stringlist
    Slist.Add(Project.Title);
    Slist.Add(TopicsListBox.Text);

    // Add column headings to the string list
    for I := 0 to 3 do Hrows[I] := '';
    for I := 0 to TheGrid.ColCount-1 do
    begin
      ParseColHeader(ColHeaders[I], Hcols);
      if I = 0 then
      begin
        for J := 0 to 3 do Hrows[J] := Format('%-16s', [Hcols[J]]);
      end
      else
      begin
        for J := 0 to 3 do Hrows[J] := Hrows[J] + #9 +
          Format('%-10s', [Hcols[J]]);
      end;
    end;
    for I := 0 to 3 do Slist.Add(Hrows[I]);

    // Add body of table to the string list
    for I := 1 to TheGrid.RowCount-1 do
    begin
      S := Format('%-16s',[TheGrid.Cells[0,I]]);
      for J := 1 to TheGrid.ColCount-1 do
        S := S + #9 + Format('%-10s', [TheGrid.Cells[J, I]]);
      Slist.Add(S);
    end;

    // Save the string list to file Fname if file name supplied
    if Length(Fname) > 0 then Slist.SaveToFile(Fname)

    // Otherwise place the text of the string list onto the clipboard
    else Clipboard.SetTextBuf(PChar(Slist.Text));

  // Free the string list
  finally
    Slist.Free;
  end;
end;

procedure TResultsForm.ParseColHeader(const S: String; var H: array of String);
//-----------------------------------------------------------------------------
// Separates the multiple lines of header text in S into separate strings
// returned in H.
//-----------------------------------------------------------------------------
var
  I, J, K, N, P: Integer;
  S1: String;
begin
  // Make a copy of the multi-line header
  S1 := S;

  // Initialize index into S1 (K), number of lines in the header (N),
  // and default contents of H
  K := 0;
  N := High(H);
  for I := 0 to N do H[I] := '';

  // For the first N-1 lines of the header
  for J := 0 to N-1 do
  begin
    // Locate the next line feed character
    p := Pos(#10, S1);
    if P = 0 then break;

    // If line feeds are successive, use just a space for the next line
    if P <= K+1 then H[J] := ' '

    // Otherwise copy the text between line feeds into the next line in H
    else H[J] := Copy(S1, K+1, P-K);

    // Move the index to the line feed and replace it with a space
    K := P;
    S1[K] := ' ';
  end;

  // Copy the remaining characters of S1 to the last header line
  H[N] := Copy(S1, K+1, 100);
end;

procedure TResultsForm.Print(Destination: TDestination);
//-----------------------------------------------------------------------------
// Prints the table to Destination (the printer or the preview form).
//-----------------------------------------------------------------------------
var
  FirstCol,
  Ncol      : Integer;
  R         : Integer;
begin
  if not TheGrid.Visible then
  begin
    Uutils.MsgDlg('There are no results to print.', mtInformation, [mbOK]);
    exit;
  end;

  with MainForm.thePrinter do
  begin

    // Initialize the Printer object
    Screen.Cursor := crHourglass;
    BeginJob;
    SetDestination(Destination);
    SetFontInformation('Times New Roman',11,[]);
    PrintCenter(TopicsListBox.Text + ' Summary');
    NewLines(2);

    // Continue until all columns are printed
    FirstCol := 1;
    while FirstCol < TheGrid.ColCount do
    begin

      // Create a table with enough columns that fit across page
      CreatePrintTable(FirstCol, Ncol);
      BeginTable;

      // Transfer contents of the Grid1 table to the printed table
      with TheGrid do
      begin
        for r := 1 to RowCount - 1 do
        begin
          PrintRow(r, FirstCol, Ncol);
          NextTableRow( (r >= RowCount-1) );
        end;
      end;
      EndTable;

      // Continue creating a new table for remaining columns
      FirstCol := FirstCol + Ncol - 1;
      if (FirstCol < TheGrid.ColCount) then
      begin
        NewPage;
        PrintCenter(TopicsListBox.Text + ' Summary');
        NewLines(2);
      end;
    end;
    Screen.Cursor := crDefault;
    EndJob;
  end;
end;

procedure TResultsForm.CreatePrintTable(const FirstCol: Integer;
  var Ncol: Integer);
//-----------------------------------------------------------------------------
//  Creates a printed table with Ncol columns starting from column FirstCol.
//-----------------------------------------------------------------------------
const
  ColWidth = 1.0;
  ColWidth1 = 1.75;
var
  I, J, K   : Integer;
  LastCol   : Integer;
  W, L,
  Left,
  Width,
  Width1    : Single;
  HdrLines  : array [0..3] of String;
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

    // Start with 1 column
    Ncol := 1;
    W := Width1;
    LastCol := FirstCol;

    // Add columns until we run out of space or columns
    while LastCol < TheGrid.ColCount do
    begin
      if W + ColWidth > Width then break;
      W := W + ColWidth;
      Inc(Ncol);
      Inc(LastCol);
    end;

    // Create a table on thePrinter object and define its column properties
    CreateTable(Ncol);
    L := Left + (Width - W)/2;
    ParseColHeader(ColHeaders[0], HdrLines);
    for K := 0 to 3 do SetColumnHeaderText(1, K+1, HdrLines[K]);
    SetColumnHeaderAlignment(1, jCenter);
    SetColumnDimensions(1, L, Width1);
    L := L + Width1;
    for I := 2 to Ncol do
    begin
      J := FirstCol + i - 2;
      ParseColHeader(ColHeaders[J], HdrLines);
      for K := 0 to 3 do SetColumnHeaderText(I, K+1, HdrLines[K]);
      SetColumnHeaderAlignment(I, jCenter);
      SetColumnDimensions(I, L, ColWidth);
      L := L + ColWidth;
    end;
    SetTableStyle([sBorder, sVerticalGrid, sHorizontalGrid]);
  end;
end;

procedure TResultsForm.PrintRow(const R: Integer; const FirstCol, Ncol: Integer);
//-----------------------------------------------------------------------------
// Prints a row of a printed table.
//-----------------------------------------------------------------------------
var
  I, J: Integer;
begin
  with MainForm.thePrinter do
  begin
    PrintColumnLeft(1, TheGrid.Cells[0, R]);
    for I := 2 to Ncol do
    begin
      J := FirstCol + I - 2;
      PrintColumnRight(I, TheGrid.Cells[J,R]);
    end;
  end;
end;

function TResultsForm.ColumnIsText(aCol: Integer; Topic: TTopic): Boolean;
//-----------------------------------------------------------------------------
// Determines if Topic has numeric data in column aCol of string grid.
//-----------------------------------------------------------------------------
begin
  if aCol = 0 then Result := true

  else if (aCol = 1)
  and (Topics[Ord(Topic)] in
    [rtLID, rtNodeDepth, rtNodeInflow, rtNodeSurcharge, rtLinkFlow])
  then Result := true

  else if (Topics[Ord(Topic)] = rtStreetFlow)
  and ((aCol = 4) or (aCol = 5))
  then Result := true

  else Result := false;
end;

end.
