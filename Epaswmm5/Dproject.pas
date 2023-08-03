unit Dproject;

{-------------------------------------------------------------------}
{                    Unit:    Dproject.pas                          }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21     (5.2.0)                  }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Form unit used to view a project's input file.                  }
{-------------------------------------------------------------------}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.ExtCtrls,
  Vcl.StdCtrls, StrUtils, Math;

type
  TProjectForm = class(TForm)
    Panel1: TPanel;
    StringGrid1: TStringGrid;
    Panel2: TPanel;
    Panel3: TPanel;
    ListBox1: TListBox;
    Splitter1: TSplitter;
    Edit1: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Panel1Resize(Sender: TObject);
    procedure StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
  private
    { Private declarations }
    procedure RefreshGrid;
  public
    { Public declarations }
  end;

var
  ProjectForm: TProjectForm;

implementation

{$R *.dfm}

uses
  Uglobals, Uexport, Fmain;

var
  S: TStringList;
  Section: Integer;

procedure TProjectForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnCreate handler.
//-----------------------------------------------------------------------------
var
  Rect: TRect;
begin
  // S stores the project's data in input file format
  S := TStringList.Create;

  // Edit1 is an invisible Text Edit control used to
  // accomodate different DPI settings
  Panel3.Height := Edit1.Height;
  StringGrid1.DefaultRowHeight := Edit1.Height;
  ListBox1.ItemHeight := Edit1.Height;

  // Position form over main form's client rectangle
  GetWindowRect(MainForm.ClientHandle, Rect);
  SetBounds(Rect.Left, Rect.Top, Rect.Width, Rect.Height);
end;

procedure TProjectForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
//-----------------------------------------------------------------------------
//  Form's OnKeyDown handler.
//-----------------------------------------------------------------------------
begin
  if Key = VK_ESCAPE then ModalResult := mrOK;
end;

procedure TProjectForm.FormClose(Sender: TObject; var Action: TCloseAction);
//-----------------------------------------------------------------------------
//  Form's OnClose handler.
//-----------------------------------------------------------------------------
begin
  S.Free;
end;

procedure TProjectForm.FormShow(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnShow handler.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  // Export the project's data to string list S
  Uexport.ExportProject(S, Uglobals.ProjectDir);

  // Add name of each data section in S to the data sections list box
  for I := 0 to S.Count-1 do
  begin
    if AnsiLeftStr(S[I], 1) = '[' then ListBox1.Items.Add(S[I]);
  end;

  // Initialize the current data section and list box selection
  Section := -1;
  ListBox1.ItemIndex := 0;

  // Force the list box to display data for the first section
  ListBox1Click(Self);
  ListBox1.SetFocus;
end;

procedure TProjectForm.ListBox1Click(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the data sections list box.
//-----------------------------------------------------------------------------
begin
  with ListBox1 do
  begin
    // Display selected section of project data in the string grid
    if ItemIndex <> Section then
    begin
      Section := ItemIndex;
      RefreshGrid;
      StringGrid1.Row := 1; //StringGrid1.FixedRows;
      StringGrid1.Col := 0;
    end;
  end;
end;

procedure TProjectForm.Panel1Resize(Sender: TObject);
//-----------------------------------------------------------------------------
//  Makes the string grid's column width fill its parent panel's width.
//-----------------------------------------------------------------------------
begin
  StringGrid1.DefaultColWidth := Panel1.Width;
end;

procedure TProjectForm.RefreshGrid;
//-----------------------------------------------------------------------------
//  Displays project data from the current section in the string grid.
//-----------------------------------------------------------------------------
var
  I, J, K, N: Integer;
  Txt: String;

begin
  // Clear the string grid
  for I := 0 to StringGrid1.RowCount - 1 do StringGrid1.Rows[I].Clear;

  // Find the currently selected section header in the input string list
  Txt := Listbox1.Items[Section];
  I := -1;
  for J := 0 to S.Count-1 do
  begin
    if AnsiStartsStr(Txt, S[J]) then
    begin
      I := J;
      break;
    end;
  end;

  // Section was found
  N := 0;
  if (I >= 0) then
  begin
    // Find out how many lines are in the section
    N := 0;
    for K := I+1 to S.Count-1 do
    begin
      if AnsiLeftStr(S[K], 2) = ';;' then continue;
      if Length(S[K]) = 0 then continue;
      if AnsiLeftStr(S[K], 1) = '[' then break;
      Inc(N);
    end;
  end;

  // Re-size the number of rows in the grid
  StringGrid1.RowCount := StringGrid1.FixedRows + Math.Max(N, 1);

  // Display each line of the data section in the grid
  if N > 0 then
  begin
    N := 0;
    for K := I+1 to S.Count-1 do
    begin
      // Stop when next section encountered
      if AnsiLeftStr(S[K], 1) = '[' then break;

      // Skip blank lines
      if Length(S[K]) = 0 then continue;

      // Add to header line
      if AnsiLeftStr(S[K], 2) = ';;' then
      begin
        if Length(StringGrid1.Cells[0,0]) > 0 then continue
        else StringGrid1.Cells[0,0] := AnsiReplaceStr(S[K], ';;', '  ');
      end
      else
      begin
        Inc(N);
        StringGrid1.Cells[0, N] := S[K];
      end;
    end;
  end;
end;

procedure TProjectForm.StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
//-----------------------------------------------------------------------------
//  Avoids having string grid highlighted when it's not the active control.
//-----------------------------------------------------------------------------
var
  GridRect: TRect;
begin
  if Sender = ActiveControl then exit;
  if not (gdSelected in State) then exit;
  with Sender as TStringGrid do
  begin
    Canvas.Brush.Color := Color;
    Canvas.Font := Font;
    GridRect := Rect;
    InflateRect(GridRect, 4, 1);
    Canvas.FillRect(GridRect);
    Canvas.TextRect(Rect, Rect.Left+2, Rect.Top+2, Cells[ACol, ARow]);
  end;
end;

end.
