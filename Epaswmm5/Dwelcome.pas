unit Dwelcome;

{-------------------------------------------------------------------}
{                    Unit:    Dwelcome.pas                          }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    05/02/22    (5.2.1)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Welcome form that can appear when SWMM is first started.        }
{-------------------------------------------------------------------}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, System.IOUtils, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, System.ImageList, Vcl.ImgList, Vcl.ComCtrls, Vcl.StdCtrls,
  Vcl.ExtCtrls, CommCtrl, Uglobals, Vcl.VirtualImageList,
  Vcl.BaseImageCollection, Vcl.ImageCollection, ShellAPI, Vcl.Buttons;

type
  TWelcomeForm = class(TForm)
    Panel1: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    GetStartedListView: TListView;
    SamplesListView: TListView;
    ShowStartPageCB: TCheckBox;
    DevelopListView: TListView;
    ProjectsListView: TListView;
    Panel2: TPanel;
    ImageCollection1: TImageCollection;
    VirtualImageList1: TVirtualImageList;
    CloseButton: TSpeedButton;
    ClearProjectsLinkLabel: TLinkLabel;
    Label1: TLabel;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GetStartedListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure DevelopListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure ProjectsListViewInfoTip(Sender: TObject; Item: TListItem;
      var InfoTip: string);
    procedure ProjectsListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure SamplesListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure CloseButtonClick(Sender: TObject);
    procedure ClearProjectsLinkLabelLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
  private
    { Private declarations }
    procedure LoadRecentProjects;
    procedure LoadSampleProjects;
  public
    { Public declarations }
    SelectedFile: String;
  end;

var
  WelcomeForm: TWelcomeForm;

implementation

{$R *.dfm}

const
  // Sample projects in user's Documents\EPA SWMM Projects\Samples folder
  SampleFiles: array[0..6] of String =
    ('Site_Drainage_Model.inp', 'Detention_Pond_Model.inp', 'LID_Model.inp',
     'Inlet_Drains_Model.inp', 'Pump_Control_Model.inp',
     'Groundwater_Model.inp', 'Culvert_Model.inp');

  SampleCaptions: array[0..6] of String =
  ('Site Runoff Drainage', 'Detention Pond Design', 'Low Impact Development',
   'Inlets & Street Drainage', 'Pump Control Rules',
   'Groundwater Model', 'Culvert Under Roadway');

var
  RecentFileNames: array [0..7] of String;
  SampleFileNames: array [0..6] of String;


procedure TWelcomeForm.FormCreate(Sender: TObject);
//
//  Loads most recently used project and sample project file names into form.
//
begin
  Label1.Left := ProjectsListView.Left + 10;
  Label1.Top := ProjectsListView.Top + 10;
  Label1.Visible := False;
  LoadRecentProjects;
  LoadSampleProjects;
end;

procedure TWelcomeForm.CloseButtonClick(Sender: TObject);
//
//  Closes form with no action to be taken.
//
begin
  ModalResult := Uglobals.saNoAction;
end;

procedure TWelcomeForm.LoadSampleProjects;
//
//  Loads names of sample projects into the SamplesListView.
//
var
  I: Integer;
  S: String;
  Item: TListItem;
  SamplesPath: String;
begin
  SamplesListView.Clear;
  SamplesPath := TPath.GetDocumentsPath + '\EPA SWMM Projects\Samples\';
  for I := 0 to High(SampleFiles) do
  begin
    S := SamplesPath + SampleFiles[I];
    if not FileExists(S) then continue;
    with SamplesListView do
    begin
      Item := Items.Add;
      Item.Caption := SampleCaptions[I];
      Item.ImageIndex := 1;
      SampleFileNames[Item.Index] := S;
    end;
  end;
  with SamplesListView do
  begin
    Item := Items.Add;
    Item.Caption := 'OpenSwmm.org Examples';
    Item.ImageIndex := 1;
  end;
end;

procedure TWelcomeForm.LoadRecentProjects;
//
//  Loads names of most recently used project files into the ProjectsListView.
//
var
  I: Integer;
  S: String;
  Item: TListItem;
begin
  // Retrieve names of most recent project files from the MRUList array
  ProjectsListView.Clear;
  for I := 0 to Uglobals.MAXMRUINDEX do
  begin
    S := Uglobals.MRUList[I];
    if Length(S) = 0 then break;
    if not FileExists(S) then continue;
    with ProjectsListView do
    begin
      Item := Items.Add;
      Item.Caption := ExtractFilename(S);
      Item.ImageIndex := 3;
      RecentFileNames[Item.Index] := S;
      if Item.Index = High(RecentFileNames) then break;
    end;
  end;

  // Indicate on form if there were no recent projects available
  if ProjectsListView.Items.Count = 0 then
  begin
    ProjectsListView.Visible := False;
    Label1.Visible := True;
    ClearProjectsLinkLabel.Visible := False;
  end
  else
  begin
    ProjectsListView.Visible := True;
    Label1.Visible := False;
    ClearProjectsLinkLabel.Visible := True;
  end;
end;

procedure TWelcomeForm.ProjectsListViewInfoTip(Sender: TObject; Item: TListItem;
  var InfoTip: string);
//
//  Sets the hint shown when mouse is over an item in the ProjectsListView.
//
begin
  InfoTip := RecentFileNames[Item.Index];
end;

procedure TWelcomeForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
//
//  Closes form with no action to be taken when Esc key pressed.
//
begin
  if Key = VK_ESCAPE then ModalResult := Uglobals.saNoAction;
end;

procedure TWelcomeForm.ClearProjectsLinkLabelLinkClick(Sender: TObject; const Link: string;
  LinkType: TSysLinkType);
//
//  Clears the MRUList and ProjectsListView when the ClearProjectsLinkLabel
//  is clicked.
//
var
  I: Integer;
begin
  for I := 0 to Uglobals.MRUList.Count-1 do
    Uglobals.MRUList[I] := '';
  ProjectsListView.Clear;
  ProjectsListView.Visible := False;
  Label1.Visible := True;
  ClearProjectsLinkLabel.Visible := False;
end;

procedure TWelcomeForm.GetStartedListViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
//
//  Closes the form when an item from the GetStartedListView is selected.
//
begin
  if Item.Selected then
  begin
    if Item.Index = 0 then
      ModalResult := Uglobals.saShowTutorial
    else if Item.Index = 1 then
      ModalResult := Uglobals.saShowUsersGuide;
  end;
end;

procedure TWelcomeForm.SamplesListViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
//
//  Processes a selection from the SampleListView and closes the form.
//
var
  S: String;
  Url: String;
begin
  if Item.Selected then
  begin
    if Item.Index = SamplesListView.Items.Count-1 then
    begin
      Url := 'https://www.openswmm.org/SWMMExamples';
      ShellAPI.ShellExecute(0, 'Open', PChar(Url), PChar(''), nil, SW_SHOWNORMAL);
      ModalResult := Uglobals.saNoAction;
    end
    else
    begin
      S := SampleFileNames[Item.Index];
      Uglobals.ProjectDir := ExtractFileDir(S);
      SelectedFile := S;
      ModalResult := Uglobals.saLoadSample;
    end;
  end;
end;

procedure TWelcomeForm.DevelopListViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
//
//  Processes a selection from the DevelopListView and closes the form.
//
begin
  if Item.Selected then
  begin
    if Item.Index = 0 then
      ModalResult := Uglobals.saNewproject
    else if Item.Index = 1 then
      ModalResult := Uglobals.saOpenProject;
  end;

end;

procedure TWelcomeForm.ProjectsListViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
//
//  Processes a selection from the ProjectsListView and closes the form.
//
var
  S: String;
begin
  if Item.Selected then
  begin
    S := RecentFileNames[Item.Index];
    Uglobals.ProjectDir := ExtractFileDir(S);
    SelectedFile := S;
    ModalResult := Uglobals.saLoadRecent;
  end;
end;

end.
