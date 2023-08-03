unit Dgrpdel;

{-------------------------------------------------------------------}
{                    Unit:    Dgrpdel.pas                           }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21    (5.2.0)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Dialog form for deleting a group of objects bounded by a user-  }
{   drawn fenceline.                                                }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Uproject, Uglobals, ExtCtrls;

type
  TGroupDeleteForm = class(TForm)
    RainGageCheckBox: TCheckBox;
    SubcatchCheckBox: TCheckBox;
    NodeCheckBox: TCheckBox;
    LabelCheckBox: TCheckBox;
    OkBtn: TButton;
    CancelBtn: TButton;
    RainGageTagEdit: TEdit;
    SubcatchTagEdit: TEdit;
    NodeTagEdit: TEdit;
    Label1: TLabel;
    RainGageTagCheck: TCheckBox;
    SubcatchTagCheck: TCheckBox;
    NodeTagCheck: TCheckBox;
    procedure OkBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    theRegion: HRgn;
    NumDeletions: Integer;
    procedure GroupDelete;
    procedure DeleteRaingages;
    procedure DeleteSubcatchments;
    procedure DeleteNodes;
    procedure DeleteLabels;
  public
    { Public declarations }
  end;

//var
//  GroupDeleteForm: TGroupDeleteForm;

implementation

{$R *.dfm}

uses Fmain, Fmap, Ubrowser;

procedure TGroupDeleteForm.OkBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the OK button.
//-----------------------------------------------------------------------------
begin
  GroupDelete;
end;

procedure TGroupDeleteForm.GroupDelete;
//-----------------------------------------------------------------------------
//  Deletes all objects of a selected type that lie within the user-drawn
//  fenceline.
//-----------------------------------------------------------------------------
begin
  //Create a GDI region from user's fenceline region
  theRegion := CreatePolygonRgn(MapForm.Fenceline, MapForm.NumFencePts - 1,
                                WINDING);
  try
    // Delete selected classes of objects
    if RainGageCheckBox.Checked then DeleteRaingages;
    if SubcatchCheckBox.Checked then DeleteSubcatchments;
    if NodeCheckBox.Checked then DeleteNodes;
    if LabelCheckBox.Checked then DeleteLabels;

    // Update browser item list box and map
    if NumDeletions > 0 then with MainForm do
    begin
      SetChangeFlags;
      ItemListBox.Count := Project.Lists[CurrentList].Count;
      ItemListBox.Refresh;
      Ubrowser.BrowserUpdate(CurrentList, Project.CurrentItem[CurrentList]);
      MapForm.NumFencePts := 0;
      MapForm.RedrawMap;
    end;
  finally
    DeleteObject(theRegion);
  end;
end;

procedure TGroupDeleteForm.DeleteRaingages;
//-----------------------------------------------------------------------------
//  Deletes all rain gages that lie within the deletion polygon area.
//-----------------------------------------------------------------------------
var
  J: Integer;
  Xp: Integer;
  Yp: Integer;
  Tag: String;
  Checked: Boolean;
  G: TRaingage;
begin
  // Try to delete raingages from highest to lowest list index
  Checked := RainGageTagCheck.Checked;
  Tag := RainGageTagEdit.Text;
  for J := Project.Lists[RAINGAGE].Count-1 downto 0 do
  begin
    G := Project.GetGage(J);
    Xp := MapForm.Map.GetXpix(G.X);
    Yp := MapForm.Map.GetYpix(G.Y);
    if (PtInRegion(theRegion, Xp, Yp)) then
    begin
      if Checked and not SameText(Tag, G.Data[TAG_INDEX]) then continue;
      Inc(NumDeletions);
      Project.DeleteItem(RAINGAGE, J);
    end;
  end;
end;

procedure TGroupDeleteForm.DeleteSubcatchments;
//-----------------------------------------------------------------------------
//  Deletes all subcatchments that lie within the deletion polygon area.
//-----------------------------------------------------------------------------
var
  J: Integer;
  Xp: Integer;
  Yp: Integer;
  Tag: String;
  Checked: Boolean;
  S: TSubcatch;
begin
  // Try to delete subcatchments from highest to lowest list index
  Checked := SubcatchTagCheck.Checked;
  Tag := SubcatchTagEdit.Text;
  for J := Project.Lists[SUBCATCH].Count-1 downto 0 do
  begin
    S := Project.GetSubcatch(SUBCATCH, J);
    Xp := MapForm.Map.GetXpix(S.X);
    Yp := MapForm.Map.GetYpix(S.Y);
    if (PtInRegion(theRegion, Xp, Yp)) then
    begin
      if Checked and not SameText(Tag, S.Data[TAG_INDEX]) then continue;
      Inc(NumDeletions);
      // Remove this subcatchment as the outlet of any other subcatchment
      Project.DeleteSubcatchOutSubcatch(S);
      Project.DeleteItem(SUBCATCH, J);
    end;
  end;
end;

procedure TGroupDeleteForm.DeleteNodes;
//-----------------------------------------------------------------------------
//  Deletes all nodes and their connecting links that lie within the
//  deletion polygon area.
//-----------------------------------------------------------------------------
var
  I, J: Integer;
  P: TPoint;
  N: TNode;
  Tag: String;
  Checked: Boolean;
begin
  Checked := NodeTagCheck.Checked;
  Tag := NodeTagEdit.Text;
  for I := 0 to MAXCLASS do
  begin
    if Project.IsNode(I) then
    begin
      // Must delete nodes from highest to lowest list index
      for J := Project.Lists[I].Count-1 downto 0 do
      begin
        N := Project.GetNode(I, J);
        P := MapForm.Map.GetNodePoint(N);
        if (PtInRegion(theRegion, P.X, P.Y)) then
        begin
          if Checked and not SameText(Tag, N.Data[TAG_INDEX]) then continue;
          Inc(NumDeletions);

          // Delete any references to node as a subcatchment outlet or
          // as a label's anchor point
          Project.DeleteSubcatchOutNode(N);
          Project.DeleteLabelAnchors(N);

          // Delete all links adjacent to deleted node
          MapForm.Map.GetAdjacencyRect(I, J, True);
          Project.DeleteItem(I, J);
        end;
      end;
    end;
  end;
end;

procedure TGroupDeleteForm.DeleteLabels;
//-----------------------------------------------------------------------------
//  Deletes all map labels that lie within the deletion polygon area.
//-----------------------------------------------------------------------------
var
  J: Integer;
  Xp: Integer;
  Yp: Integer;
begin
  for J := Project.Lists[MAPLABEL].Count-1 downto 0 do
  begin
    Xp := MapForm.Map.GetXpix(Project.GetMapLabel(J).X);
    Yp := MapForm.Map.GetYpix(Project.GetMapLabel(J).Y);
    if (PtInRegion(theRegion, Xp, Yp)) then
    begin
      Inc(NumDeletions);
      Project.DeleteItem(MAPLABEL, J);
    end;
  end;
end;

procedure TGroupDeleteForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then Application.HelpCommand(HELP_CONTEXT, 211080);
end;

end.
