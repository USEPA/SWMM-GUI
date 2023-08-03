unit Utools;

{-------------------------------------------------------------------}
{                    Unit:    Utools.pas                            }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    06/01/22    (5.2.1)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Delphi Pascal unit that manages the list of external tools      }
{   made available through the Tools item on the Main Menu.         }
{                                                                   }
{-------------------------------------------------------------------}

interface

uses
  Classes, Dialogs, Menus, SysUtils, IniFiles, Windows, StrUtils,
  Forms, Uglobals, Uproject;

// Data structure that records info for each external SWMM tool
type
  TTool = class(TObject)
    ExeName:     String;
    DirName:     String;
    Params:      String;
    DisableSWMM: Boolean;
    UpdateSWMM:  Boolean;
    MnuItem:     TMenuItem;
  end;

const

  // Name of INI file where tool info is stored
  TOOLSFILE = 'swmm5tools.ini';

  // Names used to designate specific project attributes when used
  // as arguments in the command that launches a tool
  Macros: array[0..5] of String =
    ('$PROJDIR',        // Current project directory
     '$SWMMDIR',        // SWMM's program directory
     '$INPFILE',        // Text file containing current project data
     '$RPTFILE',        // Text file to contain a status report
     '$OUTFILE',        // Binary file to contain analysis results
     '$RIFFILE');       // SWMM runoff interface file

var
  ToolList: TStrings;   // Collection of registered tools

procedure OpenToolList;
procedure CloseToolList;
procedure UpdateTool(I: Integer; S1, S2, S3, S4: String; Flag1: Boolean;
          Flag2: Boolean);
procedure DeleteTool(I: Integer);
procedure ExchangeTools(const I1: Integer; const I2: Integer);
function  GetToolName(I: Integer): String;
procedure RunTool(S: String);
function  GetTmpInpFileName: String;

implementation

uses
  Fmain, Fmap, Fproped, Ubrowser, Uimport, Uexport, Uoutput, Uutils;

var
  Tool: TTool;


procedure ReadToolsIniFile;
//-----------------------------------------------------------------------------
//  Reads the collection of tools that were last registered by the user
//  from the swmm5tools.ini file.
//-----------------------------------------------------------------------------
var
  I: Integer;
  Sections: TStringList;
  S1, S2, S3, S4: String;
  Flag1: Boolean;
  Flag2: Boolean;
begin
  // Info on each tool is written to its own section of the ini file
  Sections := TStringlist.Create;
  with TIniFile.Create(IniFileDir + TOOLSFILE) do
  try
    // Read all sections of the ini file (this is a Delphi function)
    ReadSections(Sections);

    // Parse the info for each tool from its particular section
    for I := 0 to Sections.Count-1 do
    begin
      S1 := Sections[I];                     // This is the tool's name
      S2 := ReadString(S1, 'ExeName', '');
      S3 := ReadString(S1, 'DirName', '');
      S4 := ReadString(S1, 'Params', '');
      Flag1 := ReadBool(S1, 'Disabled', False);
      Flag2 := ReadBool(S1, 'Update', False);

      // Add this tool to the collection (-1 as first argument to
      // UpdateTool means we are adding rather than updating a tool)
      if Length(S2) > 0
      then UpdateTool(-1, S1, S2, S3, S4, Flag1, Flag2);
    end;
  finally
    Free;
    Sections.Free;
  end;
end;


procedure WriteToolsIniFile;
//-----------------------------------------------------------------------------
//  Writes the current collection of registered tools to the swmm5tools.ini
//  file.
//-----------------------------------------------------------------------------
var
  I: Integer;
  S1: String;
begin
  with TMemIniFile.Create(IniFileDir + TOOLSFILE) do
  try
    Clear;
    for I := 0 to ToolList.Count-1 do
    begin
      S1 := ToolList[I];
      Tool := TTool(ToolList.Objects[I]);
      WriteString(S1, 'ExeName', Tool.ExeName);
      WriteString(S1, 'DirName', Tool.DirName);
      WriteString(S1, 'Params', Tool.Params);
      WriteBool(S1, 'Disabled', Tool.DisableSWMM);
      WriteBool(S1, 'Update', Tool.UpdateSWMM);
    end;
    UpdateFile;
  finally
    Free;
  end;
end;


function GetNewTool: TTool;
//-----------------------------------------------------------------------------
//  Creates a new tool and its associated menu item under the Main Menu's
//  Tools menu.
//-----------------------------------------------------------------------------
begin
  Tool := TTool.Create;
  Tool.MnuItem := TMenuItem.Create(MainForm);
  Tool.MnuItem.OnClick := MainForm.ToolItemClick;
  MainForm.MnuTools.Add(Tool.MnuItem);
  Result := Tool;
end;


procedure OpenToolList;
//-----------------------------------------------------------------------------
//  Creates a collection of tools and populates it with the tools
//  registered at the end of the user's last session.
//-----------------------------------------------------------------------------
begin
  ToolList := TStringlist.Create;
  ReadToolsIniFile;
end;


procedure CloseToolList;
//-----------------------------------------------------------------------------
//  Saves the current collection of tools to file and then deletes them.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  WriteToolsIniFile;
  with ToolList do
  try
   for I := (Count - 1) downto 0 do DeleteTool(I);
  finally
    ToolList.Free;
  end;
end;


function GetToolName(I: Integer): String;
//-----------------------------------------------------------------------------
//  Retrieves the name of the I-th tool in the tools collection.
//-----------------------------------------------------------------------------
begin
  if (I < 0) or (I >= ToolList.Count)
  then Result := ''
  else Result := ToolList[I];
end;


procedure UpdateTool(I: Integer; S1, S2, S3, S4: String; Flag1: Boolean;
  Flag2: Boolean);
//-----------------------------------------------------------------------------
//  Updates the properties of the tool whose index is I (if I is -1 then a
//  new tool is created and added to the tools collection).
//-----------------------------------------------------------------------------
begin
  if I < 0 then Tool := GetNewTool
  else Tool := TTool(ToolList.Objects[I]);
  Tool.ExeName := S2;
  Tool.DirName := S3;
  Tool.Params := S4;
  Tool.DisableSWMM := Flag1;
  Tool.UpdateSWMM := Flag2;
  Tool.MnuItem.Caption := S1;
  if I < 0 then ToolList.AddObject(S1, Tool)
  else ToolList[I] := S1;
end;


procedure DeleteTool(I: Integer);
//-----------------------------------------------------------------------------
//  Deletes tool I from the tools collection.
//-----------------------------------------------------------------------------
begin
  Tool := TTool(ToolList.Objects[I]);
  MainForm.MnuTools.Remove(Tool.MnuItem);
  Tool.MnuItem.Free;
  Tool.Free;
  ToolList.Delete(I);
end;


procedure ExchangeTools(const I1: Integer; const I2: Integer);
//-----------------------------------------------------------------------------
//  Exchanges the positions of tools I1 and I2 in the tools collection.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  ToolList.Exchange(I1, I2);
  for I := 0 to ToolList.Count-1 do
  begin
    Tool := TTool(ToolList.Objects[I]);
    MainForm.MnuTools.Remove(Tool.MnuItem);
  end;
  for I := 0 to ToolList.Count-1 do
  begin
    Tool := TTool(ToolList.Objects[I]);
    MainForm.MnuTools.Add(Tool.MnuItem);
  end;
end;


function GetDir: String;
//-----------------------------------------------------------------------------
//  Replaces a directory macro with the actual directory name (which can
//  either be the current project directory or the SWMM program directory).
//-----------------------------------------------------------------------------
var
  S: String;
begin
  S := Tool.DirName;
  S := AnsiReplaceText(S, Macros[0], ProjectDir);
  S := AnsiReplaceText(S, Macros[1], EpaSwmmDir);
  Result := S;
end;


function GetTmpInpFileName: String;
//-----------------------------------------------------------------------------
//  Replaces the SWMM input file name macro with the name of a temporary
//  file to which the current project's input data has been written.
//-----------------------------------------------------------------------------
var
  S: TStringlist;
begin
  // Create a new temporary input file
  SysUtils.DeleteFile(Uglobals.TempInputFile);
  Uglobals.TempInputFile  := Uutils.GetTempFile(Uglobals.TempDir, 'swmm');

  // Export the current SWMM project data to this file
  S := TStringlist.Create;
  try
    Uexport.ExportProject(S, '');
    S.AddStrings(Project.Options.Report);
    Uexport.ExportTags(S);
    Uexport.ExportMap(S);
    Uexport.ExportProfiles(S);
    S.SaveToFile(TempInputFile);
  finally
    S.Free;
  end;
  Result := Uglobals.TempInputFile;
end;


function GetTmpRptFileName: String;
//-----------------------------------------------------------------------------
//  Replaces the SWMM report file name macro with the name of a temporary
//  report file.
//-----------------------------------------------------------------------------
begin
  if not FileExists(Uglobals.TempReportFile)
  then Uglobals.TempReportFile := Uutils.GetTempFile(Uglobals.TempDir, 'swmm');
  Result := Uglobals.TempReportFile;
end;


function GetTmpOutFileName: String;
//-----------------------------------------------------------------------------
//  Replaces the SWMM output file name macro with the name of a temporary
//  output file.
//-----------------------------------------------------------------------------
begin
  if not FileExists(Uglobals.TempOutputFile)
  then Uglobals.TempOutputFile := Uutils.GetTempFile(Uglobals.TempDir, 'swmm');
  Result := Uglobals.TempOutputFile;
end;


function GetRIFFileName: String;
//-----------------------------------------------------------------------------
//  Replaces the runoff interface file name macro with the name of
//  the current runoff interface file (or a blank string if no such file
//  exists).
//-----------------------------------------------------------------------------
var
  I: Integer;
  N: Integer;
  S: String;
  Tokens: TStringlist;
begin
  Result := '';
  Tokens := TStringlist.Create;
  try
    for I := 0 to Project.IfaceFiles.Count-1 do
    begin
      S := Project.IfaceFiles[I];
      Uutils.Tokenize(S, Tokens, N);
      if N >= 3 then
      begin
        if SameText(Tokens[1], 'RUNOFF') then Result := Tokens[2];
      end;
    end;
  finally
    Tokens.Free;
  end;
end;


function GetParams: String;
//-----------------------------------------------------------------------------
//  Replaces any macros appearing in a tool's command line parameters
//  with their actual names.
//-----------------------------------------------------------------------------
var
  S: String;
begin
  S := Tool.Params;
  S := AnsiReplaceText(S, Macros[0], Uglobals.ProjectDir);
  S := AnsiReplaceText(S, Macros[1], Uglobals.EpaSwmmDir);
  S := AnsiReplaceText(S, Macros[2], GetTmpInpFileName);
  S := AnsiReplaceText(S, Macros[3], GetTmpRptFileName);
  S := AnsiReplaceText(S, Macros[4], GetTmpOutFileName);
  S := AnsiReplaceText(S, Macros[5], GetRIFFileName);
  Result := S;
end;


procedure UpdateSWMMOutput;
//-----------------------------------------------------------------------------
//  Allows a new set of simulation results that were generated by the
//  tool to be displayed by the program's user interface.
//-----------------------------------------------------------------------------
begin
  Uoutput.ClearOutput;
  Ubrowser.InitMapPage;
  if GetFileSize(Uglobals.TempReportFile) <= 0
  then Uglobals.RunStatus := rsFailed
  else Uglobals.RunStatus := Uoutput.CheckRunStatus(Uglobals.TempOutputFile);
  if Uglobals.RunStatus in [rsSuccess, rsWarning]
  then Uglobals.RunFlag := True
  else Uglobals.RunFlag := False;
  if Uglobals.RunStatus = rsError
  then MainForm.MnuReportStatusClick(MainForm);
  MainForm.RefreshResults;
  MainForm.RefreshForms;
end;


procedure UpdateSWMMInput(const OutputUpdated: Boolean);
//-----------------------------------------------------------------------------
//  Replaces the current project data with that contained in the temporary
//  input file that was updated by a particular tool.
//-----------------------------------------------------------------------------
begin
  PropEditForm.Hide;
  Project.Clear;
  Project.InitCurrentItems;
  Ubrowser.InitDataPage;
  Ubrowser.InitMapPage;
  MainForm.ReadInpFile(Uglobals.TempInputFile);
  Uglobals.HasChanged := True;
  Uglobals.UpdateFlag := True;
  if RunFlag and not OutputUpdated then UpdateSWMMOutput;
  MainForm.RefreshMapForm;
  MainForm.ShowRunStatus;
end;


procedure RunTool(S: String);
//-----------------------------------------------------------------------------
//  Launches the tool whose name is S.
//-----------------------------------------------------------------------------
var
  I, R: Integer;
  CmdLine: String;
  DirName: String;
  Params:  String;
  OutputUpdated: Boolean;
  OldInpTimeStamp: TDateTime;
  NewInpTimeStamp: TDateTime;
begin
  //Remove any menu accelerator key symbol from the tool's name
  Delete(S, Pos('&', S), 1);

  // Locate the index of the tool in the tool collection
  I := ToolList.IndexOf(S);
  if (I < 0)
  then Uutils.MsgDlg(S + ' is not a registered tool.', mtWarning, [mbOK])
  else
  begin

    // Generate the command line for launching the tool
    Tool := TTool(ToolList.Objects[I]);
    DirName := GetDir;
    Params  := GetParams;
    CmdLine := Tool.ExeName + ' ' + Params;

    // Save SWMM input file's time stamp
    if AnsiContainsText(Params, Uglobals.TempInputFile) then
      FileAge(Uglobals.TempInputFile, OldInpTimeStamp)
    else OldInpTimeStamp := 0;

    // Launch the tool and disable SWMM if called for
    try
      if Tool.DisableSWMM then MainForm.Hide;
      R := Uutils.WinExecAndWait(CmdLine, DirName, SW_SHOWNORMAL,
                                 Tool.DisableSWMM);
    finally
      MainForm.Show;
    end;
    if R < 0 then
    begin
      Uutils.MsgDlg('Could not launch ' + ToolList[I], mtWarning, [mbOK]);
      Exit;
    end;

    // Update SWMM if called for
    if Tool.UpdateSWMM then
    begin

      // See if tool updates the simulation results
      OutputUpdated := False;
      if AnsiContainsText(Params, Uglobals.TempOutputFile)
      then OutputUpdated := True;

      // Update project's input data if called for
      if AnsiContainsText(Params, Uglobals.TempInputFile) then
      begin
        FileAge(Uglobals.TempInputFile, NewInpTimeStamp);
        if (OldInpTimeStamp > 0)
        and (OldInpTimeStamp < NewInpTimeStamp)
        then UpdateSWMMInput(OutputUpdated);
      end;

      // Update simulation results if called for
      if OutputUpdated then UpdateSWMMOutput;
    end;

  end;
end;

end.
