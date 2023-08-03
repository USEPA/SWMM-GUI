unit Dsummary;

{-------------------------------------------------------------------}
{                    Unit:    Fsummary.pas                          }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form that lists the number of each type of SWMM object   }
{   within the current project.                                     }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, Uproject, Uglobals;

type
  TProjectSummaryForm = class(TForm)
    StringGrid1: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    TSFcount: Integer;                 //Number of time series inflow nodes
    DWFcount: Integer;                 //Number of dry weather inflow nodes
    GWFcount: Integer;                 //Number of groundwater inflow nodes
    IIFcount: Integer;                 //Number of RDII inflow nodes
    TUcount:  Integer;                 //Number of WQ treatment nodes
    LIDcount: Integer;                 //Number of LID controls
    procedure UpdateNodePropertyCount(N: TNode);
    procedure GetInflowsCount;
  public
    { Public declarations }
  end;

//var
//  ProjectSummaryForm: TProjectSummaryForm;

implementation

{$R *.dfm}

const
  S: array[0..25] of String =
    (
     ' Raingages              ',
     ' Subcatchments          ',
     ' Aquifers               ',
     ' Snowpacks              ',
     ' RDII Hydrographs       ',
     ' Infiltration Model     ',
     ' Junction Nodes         ',
     ' Outfall Nodes          ',
     ' Divider Nodes          ',
     ' Storage Nodes          ',
     ' Conduit Links          ',
     ' Pump Links             ',
     ' Orifice Links          ',
     ' Weir Links             ',
     ' Outlet Links           ',
     ' Flow Units             ',
     ' Flow Routing           ',
     ' Control Rules          ',
     ' Pollutants             ',
     ' Land Uses              ',
     ' Time Series Inflows    ',
     ' Dry Weather Inflows    ',
     ' Groundwater Inflows    ',
     ' RDII Inflows           ',
     ' LID Controls           ',
     ' Treatment Units        '
     );


procedure TProjectSummaryForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
// Form's OnCreate handler. Compiles the number of each type of object
// in the current project and displays them in a Memo control.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  // Display project info in the string grid
  with StringGrid1 do
  begin
    ColWidths[1] := ClientWidth - DefaultColWidth;
    RowCount := High(S) + 1;
    for I := Low(S) to High(S) do Cells[0,I] := S[I];
    Cells[1,0] := IntToStr(Project.Lists[RAINGAGE].Count);
    Cells[1,1] := IntToStr(Project.Lists[SUBCATCH].Count);
    Cells[1,2] := IntToStr(Project.Lists[AQUIFER].Count);
    Cells[1,3] := IntToStr(Project.Lists[SNOWPACK].Count);
    Cells[1,4] := IntToStr(Project.Lists[HYDROGRAPH].Count);
    Cells[1,5] := Project.Options.Data[INFILTRATION_INDEX];
//    Cells[1,6] := '';
    Cells[1,6] := IntToStr(Project.Lists[JUNCTION].Count);
    Cells[1,7] := IntToStr(Project.Lists[OUTFALL].Count);
    Cells[1,8] := IntToStr(Project.Lists[DIVIDER].Count);
    Cells[1,9] := IntToStr(Project.Lists[STORAGE].Count);
//    Cells[1,11] := '';
    Cells[1,10] := IntToStr(Project.Lists[CONDUIT].Count);
    Cells[1,11] := IntToStr(Project.Lists[PUMP].Count);
    Cells[1,12] := IntToStr(Project.Lists[ORIFICE].Count);
    Cells[1,13] := IntToStr(Project.Lists[WEIR].Count);
    Cells[1,14] := IntToStr(Project.Lists[OUTLET].Count);
//    Cells[1,17] := '';
    Cells[1,15] := Project.Options.Data[FLOW_UNITS_INDEX];
    Cells[1,16] := Project.Options.Data[ROUTING_MODEL_INDEX];
    Cells[1,17] := IntToStr(Project.Lists[CONTROL].Count);
//    Cells[1,18] := '';
    Cells[1,18] := IntToStr(Project.Lists[POLLUTANT].Count);
    Cells[1,19] := IntToStr(Project.Lists[LANDUSE].Count);
    GetInflowsCount();
//    Cells[1,24] := '';
    Cells[1,20] := IntToStr(TSFcount);
    Cells[1,21] := IntToStr(DWFcount);
    Cells[1,22] := IntToStr(GWFcount);
    Cells[1,23] := IntToStr(IIFcount);
    Cells[1,24] := IntToStr(LIDcount);
    Cells[1,25] := IntToStr(TUcount);
  end;
end;

procedure TProjectSummaryForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then Close;
end;

procedure TProjectSummaryForm.GetInflowsCount;
//-----------------------------------------------------------------------------
// Counts the numbers of inflows objects of various types and the
// number of treatment nodes.
//-----------------------------------------------------------------------------
var
  I, J: Integer;
begin
  TSFcount := 0;
  DWFcount := 0;
  GWFcount := 0;
  IIFcount := 0;
  TUcount  := 0;
  LIDcount := 0;

  // Count number of groundwater inflow objects
  with Project.Lists[SUBCATCH] do
  begin
    for I := 0 to Count-1 do
    begin
      if TSubcatch(Objects[I]).Groundwater.Count > 1 then Inc(GWFcount);
      Inc(LIDCount, TSubcatch(Objects[I]).LIDs.Count);
    end;
  end;

  // Count numbers of nodes with various types of inflows or treatment
  for J := JUNCTION to STORAGE do
  begin
    with Project.Lists[J] do
      for I := 0 to Count-1 do UpdateNodePropertyCount(TNode(Objects[I]));
  end;
end;

procedure TProjectSummaryForm.UpdateNodePropertyCount(N: TNode);
//-----------------------------------------------------------------------------
// Adds to the count of nodal inflows and treatment if the node N
// has such objects.
//-----------------------------------------------------------------------------
begin
  if N.DXInflow.Count > 0 then Inc(TSFcount);
  if N.DWInflow.Count > 0 then Inc(DWFcount);
  if N.IIInflow.Count > 0 then Inc(IIFcount);
  if N.Treatment.Count > 0 then Inc(TUcount);
end;

end.
