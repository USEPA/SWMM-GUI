unit Ucombine;

{-------------------------------------------------------------------}
{                    Unit:    Ucombine.pas                          }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21   (5.2.0)                    }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Delphi Pascal unit that combines two interface files into       }
{   a third file. The function CombineFiles is called from the      }
{   the FileCombineForm dialog in unit Dcombine, passing in the     }
{   names of the three files and returning True if the combine      }
{   operation was performed successfully.                           }
{-------------------------------------------------------------------}

interface

uses Windows, Classes, SysUtils, Dialogs, DateUtils, Uglobals, Uutils,
  Forms, Controls;

function CombineFiles(const F1, F2, F3, Title: String): Boolean;

implementation

uses Fmain;

type
  Array2D = array of array of Single;
  TFileStatus = (fsNone, fsBegun, fsDone);

const
  NO_DATE = -1.0e20;

var
  // The following arrays use index 1 for the first file, index 2 for the
  // second file, and index 3 for the combined file.
  F: array[1..3] of TextFile;               // Text file objects
  Fname: array[1..3] of String;             // Names of files
  Nparams: array[1..3] of Integer;          // Number of parameters
  Nnodes: array[1..3] of Integer;           // Number of nodes (locations)
  Tstep: array[1..3] of Integer;            // Time step between records (sec)
  Params: array[1..3] of TStringlist;       // Names of parameters
  ParamUnits: array[1..3] of TStringlist;   // Names of parameter units
  Nodes: array[1..3] of TStringlist;        // Names of nodes
  ParamIndex2: array of Integer;            // Indexes of File 2 parameters
                                            // in merged list of parameters
  NodeIndex2: array of Integer;             // Indexes of File 2 node names
                                            // in merged list of node names

  // The following variables record values for the primary (1) and secondary
  // (2) files. The primary file is the input file with the smaller time step.
  FF: array[1..2] of Integer;               // Index of primary & secondary files
  FileStatus: array[1..2] of TFileStatus;   // State of reading each file
  BeforeDate: array[1..2] of TDateTime;     // Lower bound on current date
  AfterDate: array[1..2] of TDateTime;      // Upper bound on current date
  V1: Array2D;                              // Values from primary file
  V2: Array2D;                              // Values from secondary file
  V2before: Array2D;                        // BeforeDate values from 2nd file
  V2after: Array2D;                         // AfterDate values from 2nd file
  V3: Array2D;                              // Combined values at current date
  CurrentDate: TDateTime;                   // Current date for combined file
  Ndates: Integer;                          // Number of records written to file

  TmpFname: String;                         // Name of temporary file
  ErrorTxt: String;                         // Error message text
  Tokens: TStringlist;                      // String tokens extracted from a line

procedure CheckFileNames;
//-----------------------------------------------------------------------------
//  Checks for valid file names.
//-----------------------------------------------------------------------------
begin
  ErrorTxt := '';
  if not FileExists(Fname[1]) then
    ErrorTxt := 'File 1 does not exist.'
  else if not FileExists(Fname[2]) then
    ErrorTxt := 'File 2 does not exist.'
  else if Length(Fname[3]) = 0 then
    ErrorTxt := 'No name supplied for combined file.'
  else if SameText(Fname[1], Fname[2]) then
    ErrorTxt := 'Files 1 and 2 are the same file.'
  else if SameText(Fname[1], Fname[3]) then
    ErrorTxt := 'Combined file cannot have same name as File 1.'
  else if SameText(Fname[2], Fname[3]) then
    ErrorTxt := 'Combined file cannot have same name as File 2.';
end;


procedure OpenFiles;
//-----------------------------------------------------------------------------
//  Opens files.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  // Open the two input files
  if Length(ErrorTxt) > 0 then exit;
  for I := 1 to 2 do
  begin
    AssignFile(F[I],Fname[I]);
    {$I-}
    if I < 3 then Reset(F[I]) else Rewrite(F[I]);
    if IOResult <> 0 then
    begin
      ErrorTxt := 'Cannot open File ' + IntToStr(I) + '.';
      Exit;
    end;
    {$I+}
  end;

  // Open a temporary file for the combined file
  TmpFname := Uutils.GetTempFile(TempDir, 'swmm');
  AssignFile(F[3],TmpFname);
  {$I-}
  Rewrite(F[3]);
  if IOResult <> 0 then
  begin
    ErrorTxt := 'Cannot open temporary combined file.';
    Exit;
  end;
  {$I+}
end;


procedure CheckFileFormat(const I: Integer);
//-----------------------------------------------------------------------------
//  Checks for valid format for input file I
//-----------------------------------------------------------------------------
var
  J: Integer;
  N: Integer;
  S1: String;
  S2: String;
begin
  // Assume incorrect format for quick exit in case this occurs
  if Length(ErrorTxt) > 0 then exit;
  ErrorTxt := 'File ' + IntToStr(I) + ' has incorrect format.';

  // Read header records from the file
  try
    // Read title lines
    Readln(F[I], S1);
    if Pos('SWMM5', S1) <> 1 then exit;
    Readln(F[I], S1);

    // Read time step
    Readln(F[I], Tstep[I]);

    // Read number & names of parameters
    Readln(F[I], Nparams[I]);
    for J := 1 to Nparams[I] do
    begin
      Readln(F[I], S1);
      N := Pos(' ', S1);
      if N = 0 then S2 := ''
      else
      begin
        S2 := Trim(Copy(S1, N+1, 255));
        S1 := Copy(S1, 1, N-1);
      end;
      Params[I].Add(S1);
      ParamUnits[I].Add(S2);
    end;

    // Read number & names of locations
    Readln(F[I], Nnodes[I]);
    for J := 1 to Nnodes[I] do
    begin
      Readln(F[I], S1);
      Nodes[I].Add(S1);
    end;

    // Re-set error text to empty
    ErrorTxt := '';
  finally
  end;
end;


procedure MergeParamLists;
//-----------------------------------------------------------------------------
//  Merge the parameter lists for files 1 and 2 into one list.
//-----------------------------------------------------------------------------
var
  I: Integer;
  J: Integer;
  S: String;
begin
  // Check for valid parameter lists
  if Length(ErrorTxt) > 0 then exit;
  if Params[1].Count = 0 then
    ErrorTxt := 'File 1 has no parameters.'
  else if Params[2].Count = 0 then
    ErrorTxt := 'File 2 has no parameters.'
  else if not SameText(Params[1].Strings[0], 'FLOW') then
    ErrorTxt := 'Flow is not first parameter on File 1'
  else if not SameText(Params[2].Strings[0], 'FLOW') then
    ErrorTxt := 'Flow is not first parameter on File 2'
  else
  begin
    // Identify which file is the primary file (one with smallest time step)
    if Tstep[1] <= Tstep[2] then
    begin
      FF[1] := 1;
      FF[2] := 2;
      Tstep[3] := Tstep[1];
    end
    else
    begin
      FF[1] := 2;
      FF[2] := 1;
      Tstep[3] := Tstep[2];
    end;

    // First add all parameters from File 1 to the combined list
    for I := 0 to Nparams[1]-1 do
    begin
      Params[3].Add(Params[1].Strings[I]);
      ParamUnits[3].Add(ParamUnits[1].Strings[I]);
    end;
    Nparams[3] := Nparams[1];

    // Then examine each parameter in File 2
    SetLength(ParamIndex2, Nparams[2]);
    for I := 0 to Nparams[2]-1 do
    begin

      // See if the parameter is already on the combined list
      S := Params[2].Strings[I];
      J := Params[3].IndexOf(S);

      // If already on combined list then record its position
      if J >= 0 then
      begin
        if not SameText(ParamUnits[3].Strings[J], ParamUnits[2].Strings[I]) then
        begin
          ErrorTxt := S + ' units differ on Files 1 and 2.';
          break;
        end;
        ParamIndex2[I] := J;
      end

      // Otherwise add parameter to combined list & record its position
      else
      begin
        Inc(Nparams[3]);
        Params[3].Add(S);
        ParamUnits[3].Add(ParamUnits[2].Strings[I]);
        ParamIndex2[I] := Params[3].Count-1;
      end;
    end;
  end;
end;


procedure MergeNodeLists;
//-----------------------------------------------------------------------------
//  Merge the node lists for files 1 and 2 into one list.
//-----------------------------------------------------------------------------
var
  I: Integer;
  J: Integer;
  S: String;
begin
  // First add all nodes from File 1 to the combined list
  if Length(ErrorTxt) > 0 then exit;
  for I := 0 to Nodes[1].Count-1 do Nodes[3].Add(Nodes[1].Strings[I]);

  // Examine each node from File 2
  Nnodes[3] := Nnodes[1];
  SetLength(NodeIndex2, Nnodes[2]);
  for I := 0 to Nnodes[2]-1 do
  begin
    S := Nodes[2].Strings[I];
    J := Nodes[3].IndexOf(S);

    // If node already on combined list, then record its position
    if J >= 0 then
    begin
      NodeIndex2[I] := J;
    end

    // Otherwise add the node to the combined list & record its position
    else
    begin
      Inc(Nnodes[3]);
      Nodes[3].Add(S);
      NodeIndex2[I] := Nodes[3].Count - 1;
    end;
  end;
end;


procedure CreateArrays;
//-----------------------------------------------------------------------------
//  Allocates memory for arrays that store values read from files.
//  V1 stores values from primary file at every time step.
//  V2before & V2after store values from secondary file on either side
//  of the current time for the primary file.
//  V2 holds interpolated values from V2before & V2after.
//  V3 holds combined values from V1 & V2.
//  Each of these 2D arrays has parameter values stored in columns
//  and node locations stored in rows.
//-----------------------------------------------------------------------------
begin
  if Length(ErrorTxt) > 0 then exit;
  SetLength(V1, Nnodes[FF[1]], Nparams[FF[1]]);
  SetLength(V2, Nnodes[FF[2]], Nparams[FF[2]]);
  SetLength(V2before, Nnodes[FF[2]], Nparams[FF[2]]);
  SetLength(V2after, Nnodes[FF[2]], Nparams[FF[2]]);
  SetLength(V3, Nnodes[3], Nparams[3]);
end;


procedure WriteHeader(const Title: String);
//-----------------------------------------------------------------------------
//  Writes the header lines to the combined file.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  if Length(ErrorTxt) > 0 then exit;
  Writeln(F[3], 'SWMM5 Interface File');
  Writeln(F[3], Title);
  Writeln(F[3], Tstep[3]);
  Writeln(F[3], Nparams[3]);
  for I := 0 to Params[3].Count-1 do
    Writeln(F[3], Params[3].Strings[I], ' ', ParamUnits[3].Strings[I]);
  Writeln(F[3], Nnodes[3]);
  for I := 0 to Nodes[3].Count-1 do
    Writeln(F[3], Nodes[3].Strings[I]);
  Writeln(F[3],
  ';Node             Year Mon Day  Hr Min Sec  Flow');
end;


procedure SetToZero(const I: Integer; var V: Array2D);
//-----------------------------------------------------------------------------
//  Sets entries in value matrix V to zero. I is the file index (1, 2 or 3)
//-----------------------------------------------------------------------------
var
  J, K: Integer;
begin
  for J := 0 to Nnodes[I]-1 do
  begin
    for K := 0 to Nparams[I]-1 do
      V[J,K] := 0.0;
  end;
end;


procedure SetEndOfFile(const I: Integer);
//-----------------------------------------------------------------------------
//  Sets value array to all zeros once end of file I is reached.
//-----------------------------------------------------------------------------
begin
  FileStatus[FF[I]] := fsDone;
  if FF[I] = 1 then SetToZero(I, V1) else SetToZero(I, V2);
end;


function ReadValues(const I: Integer; var V: Array2D): TDateTime;
//-----------------------------------------------------------------------------
//  Reads parameter values for each node from File I into array V.
//-----------------------------------------------------------------------------
var
  S: String;
  Ntoks: Integer;
  J, K: Integer;
  N: array[1..6] of Integer;
begin
  Result := NO_DATE;
  //if EOF(F[I]) then exit;
  try
    // Read a line for the first node from the file
    Readln(F[I], S);

    // Extract string tokens form the line
    Uutils.Tokenize(S, Tokens, Ntoks);
    if Ntoks <> Nparams[I]+7 then raise EInOutError.Create('');

    // Convert Yr, Mon, Day, Hr, Min, Sec tokens to integers
    for K := 1 to 6 do N[K] := StrToInt(Tokens[K]);

    // Encode the date in TDateTime format
    Result := EncodeDateTime(N[1], N[2], N[3], N[4], N[5], N[6], 0);

    // Retrieve parameter values from remaining tokens
    for K := 0 to Nparams[I]-1 do V[0,K] := StrToFloat(Tokens[7+K]);

    // Read in the data lines for addtional nodes
    for J := 1 to Nnodes[I]-1 do
    begin
      Readln(F[I], S);
      Uutils.Tokenize(S, Tokens, Ntoks);
      if Ntoks <> Nparams[I]+7 then raise EInOutError.Create('');
      for K := 0 to Nparams[I]-1 do V[J,K] := StrToFloat(Tokens[7+K]);
    end;

  except
    On EInOutError do SetEndOfFile(I);
    On EConvertError do SetEndOfFile(I);
  end;
end;


procedure CopyValues(const I: Integer; VA: Array2D; var VB: Array2D);
//-----------------------------------------------------------------------------
//  Copies all values from array VA of File I to VB.
//-----------------------------------------------------------------------------
var
  J, K: Integer;
begin
  for J := 0 to Nnodes[I]-1 do
  begin
    for K := 0 to Nparams[I]-1 do
      VB[J,K] := VA[J,K];
  end;
end;


procedure InterpValues;
//-----------------------------------------------------------------------------
//  Interpolates values for array V2 from arrays V2before and V2after.
//-----------------------------------------------------------------------------
var
  I: Integer;
  J: Integer;
  K: Integer;
  F: Double;
begin
  // Find fraction of difference between before and after date
  if AfterDate[2] > BeforeDate[2] then
    F := (CurrentDate - BeforeDate[2]) / (AfterDate[2] - BeforeDate[2])
  else F := 1.0;

  // Find index of file (1 or 2) whose values are being interpolated
  I := FF[2];

  // Perform interpolation for all values
  for J := 0 to Nnodes[I]-1 do
  begin
    for K := 0 to Nparams[I]-1 do
      V2[J,K] := (1.0-F)*V2Before[J,K] + F*V2After[J,K];
  end;
end;


procedure CombineValues(const I: Integer; V: Array2D);
//-----------------------------------------------------------------------------
//  Combines values V from File I with those for the combined file.
//-----------------------------------------------------------------------------
var
  J: Integer;
  K: Integer;
  J3: Integer;
  K3: Integer;
  Q: Single;
begin
  // For each node in file I
  for J := 0 to Nnodes[I]-1 do
  begin
    // Obtain index of node J in combined file
    if I = 1 then J3 := J else J3 := NodeIndex2[J];

    // Add flow to combined file values
    Q := V[J,0];
    V3[J3,0] := V3[J3,0] + Q;

    // For each WQ parameter in file I
    for K := 1 to Nparams[I]-1 do
    begin
      // Obtain index of param K in combined file
      if I = 1 then K3 := K else K3 := ParamIndex2[K];

      // Add mass flow to combined file values
      V3[J3,K3] := V3[J3,K3] + Q*V[J,K];
    end;
  end;
end;


procedure ConvertValues;
//-----------------------------------------------------------------------------
//  Converts combined parameter values from mass flow to concentration.
//-----------------------------------------------------------------------------
var
  J: Integer;
  K: Integer;
  Q: Single;
begin
  for J := 0 to Nnodes[3]-1 do
  begin
    Q := Abs(V3[J,0]);
    // Set concen. to 0 for very small flow
    // (remember that param 0 is flow so start param count from 1)
    if (Q <= 0.000001 ) then for K := 1 to Nparams[3]-1 do V3[J,K] := 0.0
    else for K := 1 to Nparams[3]-1 do V3[J,K] := V3[J,K]/Q;
  end;
end;


procedure SaveValues;
//-----------------------------------------------------------------------------
//  Saves current date and set of combined values to combined file.
//-----------------------------------------------------------------------------
var
  I: Integer;
  J: Integer;
  aYr, aMon, aDay, aHr, aMin, aSec, mSec: Word;
  Sdate: String;
  Line: String;
begin
  // Decode current date into its components
  DecodeDateTime(CurrentDate, aYr, aMon, aDay, aHr, aMin, aSec, mSec);

  // Drop milliseconds from current date to avoid roundoff problems
  CurrentDate := EncodeDateTime(aYr, aMon, aDay, aHr, aMin, aSec, 0);

  // Format date components into a string
  Sdate := Format('%4d  %2.2d  %2.2d  %2.2d  %2.2d  %2.2d', [aYr, aMon, aDay, aHr, aMin, aSec]);
  Inc(Ndates);

  // Combine values in V1 and V2 into V3 & convert to concentrations
  SetToZero(3, V3);
  CombineValues(FF[1], V1);
  CombineValues(FF[2], V2);
  ConvertValues;

  // Write values for each node to a line of file
  for I := 0 to Nnodes[3]-1 do
  begin
    // Write name of node & date to line
    Line := Format('%-16s  %s', [Nodes[3].Strings[I], Sdate]);

    // Write parameter values to line
    for J := 0 to Nparams[3]-1 do
      Line := Line + Format('  %.6f',[V3[I,J]]);

    // Write line to file
    Writeln(F[3], Line);
  end;

end;


procedure InitCombine;
//-----------------------------------------------------------------------------
//  Initialize variables used in the combine process.
//-----------------------------------------------------------------------------
var
  DiffSeconds: Int64;
  Nsteps: Int64;
  DateDiff: Integer;
begin
  // Set values from each file to zero
  if Length(ErrorTxt) > 0 then exit;
  SetToZero(FF[1], V1);
  SetToZero(FF[2], V2);

  // Assume each file begins at same time
  FileStatus[1] := fsBegun;
  FileStatus[2] := fsBegun;

  // Skip the data heading line in each file
  Readln(F[1]);
  Readln(F[2]);

  // Read & compare starting date from each file
  BeforeDate[1] := ReadValues(FF[1], V1);
  BeforeDate[2] := ReadValues(FF[2], V2Before);

  // Assume initial current date is starting date of primary file
  CurrentDate := BeforeDate[1];
  DateDiff := CompareDateTime(BeforeDate[1], BeforeDate[2]);

  case DateDiff of
    1:     // primary file FF[1] starts later than interpolated file FF[2]
    begin
      // Shift initial current date back an even number of time steps
      DiffSeconds := SecondsBetween(BeforeDate[1], BeforeDate[2]);
      Nsteps := DiffSeconds div Tstep[3];
      DiffSeconds := Nsteps * Tstep[3];
      CurrentDate := IncSecond(BeforeDate[1], -DiffSeconds);

      // Transfer initial set of values from secondary file
      CopyValues(FF[2], V2Before, V2After);
      AfterDate[2] := BeforeDate[2];
      FileStatus[1] := fsNone;
    end;
    -1:    // primary file FF[1] starts before interpolated file FF[2]
    begin
      AfterDate[1] := BeforeDate[1];
      FileStatus[2] := fsNone;
    end;
    0:     // both files start at same time
    begin
      // Read in initial sets of values for both files
      AfterDate[1] := BeforeDate[1];
      CopyValues(FF[2], V2Before, V2After);
      AfterDate[2] := BeforeDate[2];
    end;
  end;
end;


procedure CombineAtCurrentDate;
//-----------------------------------------------------------------------------
//  Combines a data record from secondary file with current data record
//  from primary file and saves result to combined file.
//-----------------------------------------------------------------------------
begin
  // If 2nd file begins after primary file does, see if we have reached
  // starting date of 2nd file.
  if (FileStatus[2] = fsNone) and (CurrentDate > BeforeDate[2]) then
  begin
    CopyValues(FF[2], V2Before, V2After);
    AfterDate[2] := BeforeDate[2];
    FileStatus[2] := fsBegun;
  end;

  // Find the data records on 2nd file that bracket the current date
  while (FileStatus[2] = fsBegun) and (AfterDate[2] < CurrentDate) do
  begin
    BeforeDate[2] := AfterDate[2];
    CopyValues(FF[2], V2After, V2Before);
    AfterDate[2] := ReadValues(FF[2], V2After);
  end;

  // Interpolate the before & after data records for 2nd file
  if (FileStatus[2] = fsBegun) then
  begin
    if AfterDate[2] = CurrentDate then CopyValues(FF[2], V2After, V2)
    else InterpValues;
  end;

  // Combine & save values at current date
  if (FileStatus[1] <> fsDone) or (FileStatus[2] <> fsDone) then SaveValues;
end;


procedure AdvanceToNextDate;
//-----------------------------------------------------------------------------
//  Advances the date at which data are being combined.
//-----------------------------------------------------------------------------
var
  OldDate: TDateTime;
begin
  // Save current date
  OldDate := Currentdate;

  // If first date on primary file hasn't been reached yet, then increase
  // current date by primary file's time step & check whether start of
  // primary file has been reached
  if (FileStatus[1] = fsNone) and (CurrentDate < BeforeDate[1]) then
  begin
    CurrentDate := IncSecond(CurrentDate, Tstep[3]);

    // If new current date is within 1 second of start date for
    // primary file, then begin reading from the file
    if Abs(SecondsBetween(BeforeDate[1], CurrentDate)) < 1 then
    begin
      CurrentDate := BeforeDate[1];
      FileStatus[1] := fsBegun;
    end;

  end

  // Else if beyond end of primary file then increase current time
  // by primary file time step
  else if FileStatus[1] = fsDone then
    CurrentDate := IncSecond(CurrentDate, Tstep[3])

  // Otherwise, read next current date & values from primary file.
  else if FileStatus[1] = fsBegun then
  begin
    CurrentDate := ReadValues(FF[1], V1);
    if FileStatus[1] = fsDone then CurrentDate := IncSecond(OldDate, Tstep[3]);
  end;
end;


procedure CloseFiles;
//-----------------------------------------------------------------------------
//  Closes all files.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  {$I-}
  for I := 1 to 3 do CloseFile(F[I]);
  {$I+}
end;


function CombineFiles(const F1, F2, F3, Title: String): Boolean;
//-----------------------------------------------------------------------------
//  Combines values from interface files F1 and F2 into file F3.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  // Save file names
  ErrorTxt := '';
  Fname[1] := F1;
  Fname[2] := F2;
  Fname[3] := F3;

  // Create string lists to hold parameter & node names
  for I := 1 to 3 do
  begin
    Params[I] := TStringlist.Create;
    ParamUnits[I] := TStringlist.Create;
    Nodes[I]  := TStringlist.Create;
  end;
  Ndates := 0;

  // Create a string list to hold tokens extracted from a line of data
  Tokens := TStringlist.Create;

  // Start the combining process
  try
    // Check for valid input files
    CheckFileNames;
    OpenFiles;
    CheckFileFormat(1);
    CheckFileFormat(2);

    // Merge names on files 1 and 2
    MergeParamLists;
    MergeNodeLists;

    // Create data arrays used to move & combine values from the files
    CreateArrays;

    // Write header records for combined file
    WriteHeader(Title);
    Screen.Cursor := crHourGlass;

    // Initialize the combine process
    InitCombine;

    // Keep combining data records from the input files until end of
    // both files reached
    if Length(ErrorTxt) = 0 then
    begin
      while (FileStatus[1] <> fsDone) or (FileStatus[2] <> fsDone) do
      begin
        CombineAtCurrentDate;
        AdvanceToNextDate;
      end;
    end;

  except
    on E: Exception do ErrorTxt := E.Message;
  end;

  // Close all files and free allocated memory
  Screen.Cursor := crDefault;
  CloseFiles;
  for I := 1 to 3 do
  begin
    Params[I].Free;
    ParamUnits[I].Free;
    Nodes[I].Free;
  end;
  Tokens.Free;

  ParamIndex2 := nil;
  NodeIndex2 := nil;
  V1 := nil;
  V2 := nil;
  V2before := nil;
  V2after := nil;
  V3 := nil;

  // If error occurred then display it & delete temporary combined file
  if Length(ErrorTxt) > 0 then
  begin
    Uutils.MsgDlg(ErrorTxt, mtError, [mbOK]);
    Result := False;
    DeleteFile(TmpFname);
  end

  // Otherwise rename temporary file to user-supplied name of combined file
  else
  begin
    DeleteFile(Fname[3]);
    if not RenameFile(TmpFname, Fname[3]) then
      Uutils.MsgDlg('Could not save combined file ' + Fname[3] + '.',
        mtError, [mbOK], MainForm)
    else Uutils.MsgDlg('A total of ' + IntToStr(Ndates) +
      ' records were combined into file ' + ExtractFileName(Fname[3]) + '.',
      mtInformation, [mbOK], MainForm);
    Result := True;
  end;
end;

end.
