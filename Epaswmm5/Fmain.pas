unit Fmain;

{-------------------------------------------------------------------}
{                    Unit:    Fmain.pas                             }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    06/01/22    (5.2.1)                   }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Delphi form unit containing the main MDI form for EPA SWMM.     }
{                                                                   }
{   This unit contains the main MDI parent form, MainForm.          }
{   It contains the program's main menu, its tool bars, a           }
{   status panel and a Browser (i.e., control) panel.               }
{                                                                   }
{   The MDI Child forms consist of:                                 }
{     MapForm         - study area map                              }
{     StatusForm      - simulation status report                    }
{     ResultsForm     - tables of summary results                   }
{     GraphForm       - time series graph                           }
{     ProfilePlotForm - water elevation profile plot                }
{     TableForm       - tabular listing of time series results      }
{     StatsReportForm - statistical analysis report                 }
{                                                                   }
{   The following stay-on-top forms are included in the project's   }
{   list of auto-create forms and are made visible when needed:     }
{     OVMapForm         - displays a birds eye overview map         }
{     ReportingForm     - selects objects that can be reported on   }
{     ReportSelectForm  - defines contents of a report              }
{     TimePlotForm      - defines contents of a time series plot    }
{     FindForm          - locates an object on the study area map   }
{     QueryForm         - locates objects meeting a criterion       }
{                                                                   }
{   Consult the files Uproject.pas, Uglobals.pas, Objprops.txt,     }
{   and Viewvars.txt for a description of the constants, classes,   }
{   and global variables used throughout the program.               }
{-------------------------------------------------------------------}

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, Menus, ExtCtrls, Buttons, StdCtrls, ComCtrls, Types,
  Printers, Chart, ExtDlgs, ImgList, Grids, ToolWin,
  HTMLHelpViewer, Spin, ShellAPI, Vcl.Themes, Vcl.Styles, Vcl.AppEvnts,
  Uglobals, Uproject, Uutils, Animator, PgSetup, OpenDlg, Xprinter,
  System.ImageList, Vcl.VirtualImageList, Vcl.BaseImageCollection,
  Vcl.ImageCollection, Vcl.Imaging.pngimage, System.Win.TaskbarCore, Vcl.Taskbar;

const
  MSG_NO_MAP_FILE = 'Could not read map file ';
  MSG_NO_CALIB_DATA = 'No calibration data has been registered for this project.';
  MSG_NO_INPUT_FILE = 'Input file no longer exists.';
  MSG_NOT_EPASWMM_FILE = 'Not an EPA-SWMM file.';
  MSG_READONLY = ' is read-only.'#10+
    'Use File >> Save As command to save it under a different name.';
  MSG_NO_BACKDROP = 'Could not find backdrop file ';
  MSG_FIND_BACKDROP = '. Do you want to search for it?';

  TXT_MAIN_CAPTION = 'SWMM 5.2';
  TXT_STATUS_REPORT = 'Status Report';
  TXT_SAVE_CHANGES = 'Save changes made to current project?';
  TXT_SAVE_RESULTS = 'Save current simulation results?';
  TXT_WARNING = '  WARNING:';
  TXT_TITLE = 'TITLE:';
  TXT_NOTES = 'NOTES:';
  TXT_HIDE = '&Hide';
  TXT_SHOW = '&Show';
  TXT_HOW_DO_I = 'How do I';
  TXT_OPEN_PROJECT_TITLE = 'Open a Project';
  TXT_OPEN_MAP_TITLE = 'Open a Map';
  TXT_SAVE_PROJECT_TITLE = 'Save Project As';
  TXT_OPEN_PROJECT_FILTER =
   'Input file (*.INP)|*.INP|' + 'Backup files (*.BAK)|*.BAK|All files|*.*';
  TXT_SAVE_PROJECT_FILTER = 'Input files (*.INP)|*.INP|All files|*.*';
  TXT_SCENARIO_FILTER = 'Scenario files (*.SCN)|*.SCN|All files|*.*';
  TXT_MAP_FILTER  = 'Map files (*.MAP)|*.MAP|All files|*.*';
  TXT_ADD_RAINGAGE = '  Click the map where the new Rain Gage should be placed.';
  TXT_ADD_SUBCATCH = '  Draw the outline of the new Subcatchment on the map ' +
                     '(left-click adds a vertex, right-click closes the outline).';
  TXT_ADD_STORAGE = '  Draw the outline of the new Storage unit on the map ' +
                     '(left-click adds a vertex, right-click closes the outline).';
  TXT_ADD_NODE = '  Click the map where the new Node should be placed.';
  TXT_ADD_LINK = '  Click on the new Link''s start node and then on its end node.';
  TXT_ADD_LABEL = '  Click the map where the new Label should be placed.';

  TXT_INP = 'inp';
  TXT_RPT = 'rpt';
  TXT_HSF = 'hsf';
  TXT_BAK = 'bak';
  MSG_NO_STATUS_RPT = 'A Status Report is not available.';
  MSG_NO_EXPORT_RPT = 'Could not export Status Report.';
  TXT_SAVE_STATUS_RPT = 'Save Status and Summary Report As';
  TXT_SAVE_RPT_FILTER = 'Report files (*.rpt)|*.rpt|All files|*.*';
  TXT_SAVE_HOTSTART = 'Do you wish to save the current state of '#10+
                      'the conveyance system to a Hotstart file?';
  TXT_SAVE_HOTSTART_AS = 'Save Hotstart File As';
  TXT_HOTSTART_FILTER = 'Hotstart files (*.HSF)|*.HSF|All files|*.*';
  TXT_AUTO_LENGTH_ON = 'Auto-Length: On';
  TXT_AUTO_LENGTH_OFF = 'Auto-Length: Off';

  RunStatusHint: array[0..3] of String =
    ('No Results Available','Results Are Current','Results Need Updating',
     'No Results - Last Run Failed');

  // Max. index of Map Toolbar buttons
  MAXMAPTOOLBARINDEX = 8;

type
  TMainForm = class(TForm)
    MainMenu1: TMainMenu;

    // File Menu
    MnuFile: TMenuItem;
      MnuNew: TMenuItem;
      MnuOpen: TMenuItem;
      MnuReopen: TMenuItem;
      N13: TMenuItem;
      MnuSave: TMenuItem;
      MnuSaveAs: TMenuItem;
      N1: TMenuItem;
      MnuExport: TMenuItem;
        MnuExportMap: TMenuItem;
        MnuExportHotstart: TMenuItem;
        MnuExportStatusRpt: TMenuItem;
      MnuCombine: TMenuItem;
      N10: TMenuItem;
      MnuPageSetup: TMenuItem;
      MnuPrintPreview: TMenuItem;
      MnuPrint: TMenuItem;
      N11: TMenuItem;
      N2: TMenuItem;
      MnuExit: TMenuItem;

    // Edit Menu
    MnuEdit: TMenuItem;
      MnuCopy: TMenuItem;
      N3: TMenuItem;
      MnuSelectObject: TMenuItem;
      MnuSelectVertex: TMenuItem;
      MnuSelectRegion: TMenuItem;
      MnuSelectAll: TMenuItem;
      N16: TMenuItem;
      MnuFindObject: TMenuItem;
      N21: TMenuItem;
      MnuEditObject: TMenuItem;
      MnuDeleteObject: TMenuItem;
      N17: TMenuItem;
      MnuGroupEdit: TMenuItem;
      MnuGroupDelete: TMenuItem;

    // View Menu
    MnuView: TMenuItem;
      MnuDimensions: TMenuItem;
      MnuBackdrop: TMenuItem;
        MnuBackdropLoad: TMenuItem;
        MnuBackdropUnload: TMenuItem;
        N15: TMenuItem;
        MnuBackdropAlign: TMenuItem;
        MnuBackdropResize: TMenuItem;
        N18: TMenuItem;
        MnuBackdropWatermark: TMenuItem;
      N5: TMenuItem;
      MnuPan: TMenuItem;
      MnuZoomIn: TMenuItem;
      MnuZoomOut: TMenuItem;
      MnuFullExtent: TMenuItem;
      N14: TMenuItem;
      MnuQuery: TMenuItem;
      N12: TMenuItem;
      MnuOVMap: TMenuItem;
      MnuObjects: TMenuItem;
        MnuShowGages: TMenuItem;
        MnuShowSubcatch: TMenuItem;
        MnuShowNodes: TMenuItem;
        MnuShowLinks: TMenuItem;
        MnuShowLabels: TMenuItem;
        MnuShowBackdrop: TMenuItem;
      MnuLegends: TMenuItem;
        MnuSubcatchLegend: TMenuItem;
        MnuNodeLegend: TMenuItem;
        MnuLinkLegend: TMenuItem;
        MnuTimeLegend: TMenuItem;
        N6: TMenuItem;
        MnuModifyLegend: TMenuItem;
          MnuModifySubcatchLegend: TMenuItem;
          MnuModifyNodeLegend: TMenuItem;
          MnuModifyLinkLegend: TMenuItem;
      MnuToolbars: TMenuItem;

    // Project Menu
    MnuProject: TMenuItem;
      MnuProjectSummary: TMenuItem;
      MnuProjectDetails: TMenuItem;
      MnuProjectDefaults: TMenuItem;
      MnuProjectCalibData: TMenuItem;
      N9: TMenuItem;
      MnuAddObject: TMenuItem;
      N22: TMenuItem;
      MnuProjectRunSimulation: TMenuItem;

    // Report Menu
    MnuReport: TMenuItem;
      MnuReportStatus: TMenuItem;
      MnuReportSummary: TMenuItem;
      MnuReportGraph: TMenuItem;
        MnuGraphTimeSeries: TMenuItem;
        MnuGraphScatter: TMenuItem;
        MnuGraphProfile: TMenuItem;
      MnuReportTable: TMenuItem;
        MnuTableByObject: TMenuItem;
        MnuTableByVariable: TMenuItem;
      MnuReportStatistics: TMenuItem;
      N8: TMenuItem;
      MnuReportOptions: TMenuItem;

    // Tools Menu
    MnuTools: TMenuItem;
      MnuProgramOptions: TMenuItem;
      MnuMapDisplayOptions: TMenuItem;
      N7: TMenuItem;
      MnuConfigureTools: TMenuItem;

    // Window Menu
    MnuWindow: TMenuItem;
      MnuWindowTile: TMenuItem;
      MnuWindowCascade: TMenuItem;
      MnuWindowCloseAll: TMenuItem;

    // Help Menu
    MnuHelp: TMenuItem;
      MnuHelpTopics: TMenuItem;
      MnuHowdoI: TMenuItem;
      MnuHelpUnits: TMenuItem;
      MnuHelpErrors: TMenuItem;
      N19: TMenuItem;
      MnuHelpTutorials: TMenuItem;
        MnuIntroTutorial: TMenuItem;
        MnuInletsTutorial: TMenuItem;
      N4: TMenuItem;
      MnuAbout: TMenuItem;

    // Popup Menus
    TablePopupMenu: TPopupMenu;
      PopupTableByObject: TMenuItem;
      PopupTableByVariable: TMenuItem;
    AutoLengthMnu: TPopupMenu;
      AutoLengthOnMnu: TMenuItem;
      AutoLengthOffMnu: TMenuItem;
    FlowUnitsMnu: TPopupMenu;
      CFSMnuItem: TMenuItem;
      GPMMnuItem: TMenuItem;
      MGDMnuItem: TMenuItem;
      N20: TMenuItem;
      CMSMnuItem: TMenuItem;
      LPSMnuItem: TMenuItem;
      MLDMnuItem: TMenuItem;
    OffsetsMnu: TPopupMenu;
      OffsetsMnuDepth: TMenuItem;
      OffsetsMnuElev: TMenuItem;

    // System Dialogs
    OpenTextFileDialog: TOpenDialog;
    OpenPictureDialog: TOpenPictureDialog;
    SaveDialog: TSaveDialog;
    FontDialog: TFontDialog;
    thePrinter: TPrintControl;

    // Progress Bar
    ProgressPanel: TPanel;
      ProgressBar: TProgressBar;

    // Status Bar
    StatusBar: TToolBar;
      AutoLengthBtn: TToolButton;
      OffsetsBtn: TToolButton;
      FlowUnitsBtn: TToolButton;
      RunStatusButton: TToolButton;
      ZoomLevelLabel: TToolButton;
      XYLabel: TToolButton;
      StatusHint: TToolButton;
      StatusHintSep: TToolButton;
      ToolButton1: TToolButton;
      ToolButton3: TToolButton;
      ToolButton4: TToolButton;
      ToolButton6: TToolButton;
      ToolButton7: TToolButton;

    // Popup Menus
    ReportPopupMenu: TPopupMenu;
    PopupReportStatus: TMenuItem;
    PopupReportSummary: TMenuItem;

    // Browser Panel
    BrowserPageControl: TPageControl;
    BrowserDataPage: TTabSheet;
      ObjectTreeView: TTreeView;
      BrowserDataSplitter: TSplitter;
      ItemsPanel: TPanel;
        ItemsLabel: TLabel;
        ItemListBox: TListBox;
        NotesMemo: TMemo;
        BrowserToolBar: TToolBar;
          BrowserBtnNew: TToolButton;
          BrowserBtnDelete: TToolButton;
          BrowserBtnEdit: TToolButton;
          BrowserBtnUp: TToolButton;
          BrowserBtnDown: TToolButton;
          BrowserBtnSort: TToolButton;
    BrowserMapPage: TTabSheet;
      MapScrollBox: TScrollBox;
      MapThemesBox: TGroupBox;
      Label1: TLabel;
      Label2: TLabel;
      Label3: TLabel;
      SubcatchViewBox: TComboBox;
      NodeViewBox: TComboBox;
      LinkViewBox: TComboBox;
      MapTimePeriodBox: TGroupBox;
      DateLabel: TLabel;
      TimeLabel: TLabel;
      ElapsedTimeLabel: TLabel;
      DateListBox: TComboBox;
      DateScrollBar: TScrollBar;
      TimeListBox: TComboBox;
      TimeScrollBar: TScrollBar;
    ElapsedTimePanel: TEdit;
      Splitter1: TSplitter;
    ApplicationEvents1: TApplicationEvents;
    MnuWhatsNew: TMenuItem;
    PageSetupDialog: TPageSetupDialog;
    BtnImageCollection: TImageCollection;
    BtnImageList: TVirtualImageList;
    ProjectImageCollection: TImageCollection;
    ProjectImageList: TVirtualImageList;
    RunImageCollection: TImageCollection;
    RunImageList: TVirtualImageList;
    AnimatorImageCollection: TImageCollection;
    AnimatorImageList: TVirtualImageList;
    AnimatorFrame: TAnimatorFrame;
    ProjectImageList1: TVirtualImageList;
    BtnImageList1: TVirtualImageList;
    ToolbarPanel: TPanel;
    ToolBar1: TToolBar;
    TBNew: TToolButton;
    TBOpen: TToolButton;
    TBSave: TToolButton;
    TBPrint: TToolButton;
    Sep1: TToolButton;
    TBCopy: TToolButton;
    TBFind: TToolButton;
    TBQuery: TToolButton;
    TBOverview: TToolButton;
    Sep2: TToolButton;
    TBRun: TToolButton;
    ToolButton21: TToolButton;
    TBReport: TToolButton;
    TBProfile: TToolButton;
    TBGraph: TToolButton;
    TBTable: TToolButton;
    TBScatter: TToolButton;
    TBStats: TToolButton;
    Sep3: TToolButton;
    TBOptions: TToolButton;
    TBArrange: TToolButton;
    AnimatorImageList1: TVirtualImageList;
    ElapsedTimeUpDown: TUpDown;
    MnuWelcomeScreen: TMenuItem;
    MnuHelpShortcuts: TMenuItem;
    MnuBackdropGrayscale: TMenuItem;

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure MnuFileClick(Sender: TObject);
    procedure MnuNewClick(Sender: TObject);
    procedure MnuReopenClick(Sender: TObject);
    procedure MnuOpenClick(Sender: TObject);
    procedure MnuSaveClick(Sender: TObject);
    procedure MnuSaveAsClick(Sender: TObject);
    procedure MnuExportMapClick(Sender: TObject);
    procedure MnuExportHotstartClick(Sender: TObject);
    procedure MnuExportStatusRptClick(Sender: TObject);
    procedure MnuCombineClick(Sender: TObject);
    procedure MnuPageSetupClick(Sender: TObject);
    procedure MnuPrintPreviewClick(Sender: TObject);
    procedure MnuPrintClick(Sender: TObject);
    procedure MnuPreferencesClick(Sender: TObject);
    procedure MnuExitClick(Sender: TObject);

    procedure MnuEditClick(Sender: TObject);
    procedure MnuCopyClick(Sender: TObject);
    procedure MnuSelectObjectClick(Sender: TObject);
    procedure MnuSelectVertexClick(Sender: TObject);
    procedure MnuSelectRegionClick(Sender: TObject);
    procedure MnuSelectAllClick(Sender: TObject);
    procedure MnuGroupEditClick(Sender: TObject);
    procedure MnuGroupDeleteClick(Sender: TObject);
    procedure MnuFindObjectClick(Sender: TObject);

    procedure MapActionClick(Sender: TObject);
    procedure MnuBackdropClick(Sender: TObject);
    procedure MnuBackdropLoadClick(Sender: TObject);
    procedure MnuDimensionsClick(Sender: TObject);
    procedure MnuBackdropUnloadClick(Sender: TObject);
    procedure MnuBackdropAlignClick(Sender: TObject);
    procedure MnuBackdropResizeClick(Sender: TObject);
    procedure MnuBackdropWatermarkClick(Sender: TObject);
    procedure MnuQueryClick(Sender: TObject);
    procedure MnuOVMapClick(Sender: TObject);
    procedure MnuObjectsClick(Sender: TObject);
    procedure MnuShowObjectsClick(Sender: TObject);
    procedure MnuShowBackdropClick(Sender: TObject);
    procedure MnuLegendsClick(Sender: TObject);
    procedure MnuSubcatchLegendClick(Sender: TObject);
    procedure MnuNodeLegendClick(Sender: TObject);
    procedure MnuLinkLegendClick(Sender: TObject);
    procedure MnuTimeLegendClick(Sender: TObject);
    procedure MnuModifyLegendClick(Sender: TObject);
    procedure MnuModifySubcatchLegendClick(Sender: TObject);
    procedure MnuModifyLinkLegendClick(Sender: TObject);
    procedure MnuModifyNodeLegendClick(Sender: TObject);

    procedure MnuProjectSummaryClick(Sender: TObject);
    procedure MnuProjectDefaultsClick(Sender: TObject);
    procedure MnuProjectDetailsClick(Sender: TObject);
    procedure MnuProjectCalibDataClick(Sender: TObject);
    procedure MnuProjectRunSimulationClick(Sender: TObject);

    procedure MnuReportClick(Sender: TObject);
    procedure MnuReportStatusClick(Sender: TObject);
    procedure MnuReportSummaryClick(Sender: TObject);
    procedure MnuReportCustomizeClick(Sender: TObject);
    procedure MnuReportGraphClick(Sender: TObject);
    procedure MnuGraphTimeSeriesClick(Sender: TObject);
    procedure MnuGraphProfileClick(Sender: TObject);
    procedure MnuGraphScatterClick(Sender: TObject);
    procedure MnuTableByVariableClick(Sender: TObject);
    procedure MnuTableByObjectClick(Sender: TObject);
    procedure MnuReportStatisticsClick(Sender: TObject);

    procedure MnuConfigureToolsClick(Sender: TObject);
    procedure MnuViewOptionsClick(Sender: TObject);

    procedure MnuWindowClick(Sender: TObject);
    procedure MnuWindowCascadeClick(Sender: TObject);
    procedure MnuWindowTileClick(Sender: TObject);
    procedure MnuWindowCloseAllClick(Sender: TObject);

    procedure MnuHelpTopicsClick(Sender: TObject);
    procedure MnuHowdoIClick(Sender: TObject);
    procedure MnuHelpUnitsClick(Sender: TObject);
    procedure MnuHelpErrorsClick(Sender: TObject);
    procedure MnuIntroTutorialClick(Sender: TObject);
    procedure MnuAboutClick(Sender: TObject);

    procedure AutoLengthOnMnuClick(Sender: TObject);
    procedure AutoLengthOffMnuClick(Sender: TObject);
    procedure FlowUnitsMnuItemClick(Sender: TObject);
    procedure OffsetsMnuItemClick(Sender: TObject);

    procedure TBGraphClick(Sender: TObject);
    procedure TBOptionsClick(Sender: TObject);
    procedure BrowserBtnNewClick(Sender: TObject);
    procedure BrowserBtnDeleteClick(Sender: TObject);
    procedure BrowserBtnEditClick(Sender: TObject);
    procedure BrowserBtnUpClick(Sender: TObject);
    procedure BrowserBtnDownClick(Sender: TObject);
    procedure BrowserBtnSortClick(Sender: TObject);

    procedure ObjectTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure ObjectTreeViewClick(Sender: TObject);

    procedure ItemListBoxClick(Sender: TObject);
    procedure ItemListBoxDblClick(Sender: TObject);
    procedure ItemListBoxKeyPress(Sender: TObject; var Key: Char);
    procedure ItemListBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ItemListBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ItemListBoxData(Control: TWinControl; Index: Integer;
      var Data: string);
    procedure ItemListBoxDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);

    procedure MapViewBoxChange(Sender: TObject);
    procedure TimeListBoxClick(Sender: TObject);
    procedure TimeScrollBarChange(Sender: TObject);
    procedure TimeScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure DateScrollBarChange(Sender: TObject);
    procedure DateScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure DateListBoxClick(Sender: TObject);
    procedure ElapsedTimeUpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure BrowserPageControlResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ObjButtonClick(Sender: TObject);

    procedure ApplicationEvents1Restore(Sender: TObject);
    procedure ApplicationEvents1Minimize(Sender: TObject);
    procedure MnuWhatsNewClick(Sender: TObject);
    procedure MnuToolbarsClick(Sender: TObject);
    procedure ElapsedTimePanelKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MapViewBoxDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure MapViewBoxMeasureItem(Control: TWinControl; Index: Integer;
      var Height: Integer);
    procedure MnuInletsTutorialClick(Sender: TObject);
    procedure MnuWelcomeScreenClick(Sender: TObject);
    procedure MnuHelpShortcutsClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ObjectTreeViewKeyPress(Sender: TObject; var Key: Char);
    procedure MnuBackdropGrayscaleClick(Sender: TObject);
    procedure MnuBackdropStreetClick(Sender: TObject);
    procedure FormResize(Sender: TObject);

  private
    { Private declarations }

    // Most recently used files menu
    MnuMRU: array[0..Uglobals.MAXMRUINDEX] of TMenuItem;

    procedure ClearAll;
    procedure CreateTempFiles;
    procedure DeleteTempFiles;
    procedure EnableReportButtons(IsEnabled: Boolean);
    procedure FindBackdropFile;
    procedure InitPageLayout;
    procedure OpenFile(Sender: TObject; const Fname: String);
    procedure Print(Dest: TDestination);
    procedure RecenterControl(aControl: TControl);
    procedure ResizeControl(aControl: TControl);
    procedure RunSimulation;
    procedure SaveFile(Fname: String);
    function  SaveFileDlg(Sender: TObject): Integer;
    procedure SetAllUp(Toolbar: TToolbar);

    // MRU file support
    procedure MRUClick(Sender: TObject);
    procedure MRUDisplay(Sender: TObject);
    procedure MRUUpdate(Sender: TObject; const AddFileName: String);

    // Drag and drop file support
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;

    // Custom OnHelp handler (avoids bug in XE2's HtmlHelpViewer unit)
    function ApplicationHelp(Command: Word; Data: THelpEventData;
                             var CallHelp: Boolean): Boolean;
    procedure ShowProjectInfo(FileName: String);

 public
    { Public declarations }
    procedure CloseForms;
    procedure CreateReport(ReportSelection: TReportSelection);
    function  FormExists(const Name: String): Boolean;
    procedure HideProgressBar;
    procedure PageSetup;
    procedure ReadInpFile(const Fname: String);
    procedure RefreshForms;
    procedure RefreshMapForm;
    procedure RefreshResults;
    procedure SelectorButtonClick;
    procedure SetChangeFlags;
    procedure ShowProgressBar(const Msg: String);
    procedure ShowRunStatus;
    procedure ShowStatusHint(const Msg: String);
    procedure ToolItemClick(Sender:TObject);
    procedure UpdateProfilePlots;
    procedure UpdateProgressBar(var Count: Integer; const StepSize: Integer);
end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}
{$R mycurs32.res}  // Resource file containing custom cursors

uses
  Fmap, Fovmap, Fproped, Fstatus, Fresults, Fgraph, Fproplot, Ftable, Fstats,
  Fsimul, Dsummary, Dabout, Dcalib1, Dcombine, Ddefault, Dgrpdel, Dprefers,
  Dproject, Dreport, Dstats, Dgrouped, Dfind, Dquery, Dmapexp, Dbackdrp,
  Dbackdim, Dtools1, Ubrowser, Uinifile, Umap, Uimport, Uexport, Uoutput,
  Utools, Uupdate, Dreporting, Dproselect, Dtimeplot;

//============================================================================
//            Form Creation, Resizing, & Closing Handlers
//============================================================================

procedure TMainForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
// Main form's OnCreate handler.
// (Uglobals.Startup is called from Epaswmm5.dpr before the main form is
//  created. It sets the program's directories, reads command line switches,
//  creates the most recently used file list, and launches a Welcome screen.)
//-----------------------------------------------------------------------------
var
  I  : Integer;
begin
  // Load custom cursors from the mycurs32.res file
  Screen.Cursors[crXHAIR]   := LoadCursor(HInstance, PChar('xhair'));
  Screen.Cursors[crZOOMIN]  := LoadCursor(HInstance, PChar('zoomin'));
  Screen.Cursors[crZOOMOUT] := LoadCursor(HInstance, PChar('zoomout'));
  Screen.Cursors[crFIST]    := LoadCursor(HInstance, PChar('fist'));
  Screen.Cursors[crMOVE]    := LoadCursor(HInstance, PChar('move'));
  Screen.Cursors[crPENCIL]  := LoadCursor(HInstance, PChar('pencil'));
  Screen.Cursors[crARROWTIP]:= LoadCursor(HInstance, PChar('arrowtip'));
  Screen.Cursors[crSQUARE]  := LoadCursor(HInstance, PChar('square'));

// Maintain floating point compatibility between 32-bit GUI and CLE
{$IFDEF WIN32}
  Set8087CW($133F);
{$ENDIF}

  // Identify various directories
  Uglobals.SetDirectories;

  // Set the user interface theme style
  Uinifile.ReadStyleName;
  TStyleManager.TrySetStyle(Uglobals.StyleName);
  if Uglobals.StyleName = 'Windows' then
  begin
    Toolbar1.DrawingStyle := TTBDrawingStyle(dsNormal);
    StatusBar.DrawingStyle := TTBDrawingStyle(dsNormal);
  end;

  // Set OnHelp handler
  Application.OnHelp := ApplicationHelp;

  // Set main window caption
  Caption := TXT_MAIN_CAPTION;

  // Register ability to accept input files dragged from Explorer
  DragAcceptFiles(Self.Handle, True);

  // Process command line switches
  ReadCmdLine;

  // Set format settings
  Uglobals.SetFormatSettings;

  // Create most-recently-used (MRU) file list
  MRUList := TStringList.Create;

  // Create MRU file menu items
  for I := 0 to Uglobals.MAXMRUINDEX do
  begin
    MnuMRU[I] := TMenuItem.Create(self);
    MnuMRU[I].Tag     := I;
    MnuMRU[I].OnClick := MRUClick;
    MnuMRU[I].Name    := 'MRU' + IntToStr(I);
    MnuMRU[I].Visible := False;
    MnuReopen.Add(MnuMRU[I]);
  end;

  // Create list of add-on tools
  Utools.OpenToolList;

  // Use default number of decimal places
  for I := 0 to SUBCATCHVIEWS do SubcatchUnits[I].Digits := 2;
  for I := 0 to NODEVIEWS do NodeUnits[I].Digits := 2;
  for I := 0 to LINKVIEWS do LinkUnits[I].Digits := 2;

  // Create Project database object
  Project := TProject.Create;

  // Disable printing options if there are no printers
  thePrinter.SetShowProgress(True);
  if Printer.Printers.Count = 0 then
  begin
    MnuPageSetup.Enabled := False;
    MnuPrintPreview.Enabled := False;
    MnuPrint.Enabled := False;
    TBPrint.Enabled := False;
  end;

  // Set status flags
  RunStatus := rsNone;
  RunFlag := False;
  QueryFlag := False;
  QueryColor := clRed;
  HasChanged := False;
  ReadOnlyFlag := False;

  // Create the Property Editor Form
  PropEditForm := TPropEditForm.Create(self);

  // Retrieve preferences from .INI file
  Uinifile.ReadMainFormSize;
  Uinifile.ReadIniFile;

  // Align NotesMemo & ItemListBox on top of one another
  NotesMemo.Visible := False;
  NotesMemo.Align := alClient;
  ItemListBox.Align := alClient;

  // Set enough spacing between items in the Browser's ItemListBox
  ItemListBox.ItemHeight := ItemsLabel.Height;

  // Set the item height of the owner drawn combo boxes
  I := (18 * Screen.PixelsPerInch) div 96;
  Uglobals.ItemHeight := I;

  // Display status panel, hide progress meter panel
  ProgressBar.Visible := False;
  ProgressPanel.Visible := False;
  ProgressPanel.Height := StatusBar.Height - 1;

  // Initialize Browser panel
  Ubrowser.InitBrowser;

  // Prevent form from repainting itself for now
  LockWindowUpdate(Handle);

  // Enable only for testing
  //ReportMemoryLeaksOnShutdown := True;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
//-----------------------------------------------------------------------------
// Main form's OnDestroy handler. Frees objects previously created.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  // Free add-on tools list
  Utools.CloseToolList;

  // Free the most-recently-used file menu items
  for I := 0 to Uglobals.MAXMRUINDEX do MnuMRU[I].Free;

  // Free the most-recently-used file list
  MRUList.Free;

  // Free the MapForm
  MapForm.Free;

  // Free the memory allocated for the project's data
  Project.Free;
end;

procedure TMainForm.FormShow(Sender: TObject);
//-----------------------------------------------------------------------------
// Main form's OnShow handler.
// Creates the Map form and opens project specified on command line.
//-----------------------------------------------------------------------------
begin
   // Do nothing if the map form already exists
   if Assigned(MapForm) then exit;

  // Create a Map child form
  MapForm := TMapForm.Create(self);
  with MapForm do
  begin
    FormResize(Sender);
    PlaceLegends;
    RedrawOnResize := True;
  end;

  // Set Browser to show Data page
  BrowserPageControl.ActivePage := BrowserDataPage;

  // Allow main form to repaint itself
  LockWindowUpdate(0);

  // Open a project file if one is provided on the command line,
  if (Length(StartupFile) > 0) and FileExists(StartupFile) then
  begin
    OpenFile(Sender, InputFileName);
  end

  // Show the Welcome start page if that option is in effect
  else
  begin
    MnuNewClick(Sender);
    if Uglobals.ShowStartPage then MnuWelcomeScreenClick(Sender);
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
//-----------------------------------------------------------------------------
// Main form's OnClose handler.
//-----------------------------------------------------------------------------
begin
  // Save preferences to the .INI file
  if not Uutils.IsReadOnly(IniFileDir) then
  begin
    Uinifile.SaveIniFile;
    Uinifile.SaveMainFormSize;
  end;

  // Clear any current simulation output results
  CloseForms;
  Uoutput.ClearOutput;
  DeleteTempFiles;
  Project.Clear;

  // Un-register ability to accept dragged files from Explorer
  DragAcceptFiles(Self.Handle, False);
  Action := caFree;
end;


procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
//-----------------------------------------------------------------------------
// OnCloseQuery handler for Main form.
// Checks if user wants to save the current project or cancel the
// Close request.
//-----------------------------------------------------------------------------
begin
  if SaveFileDlg(Sender) = mrCancel
  then CanClose := False
  else CanClose := True;
end;


//=============================================================================
//                        File Menu Handlers
//=============================================================================

procedure TMainForm.MnuFileClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for File menu
//-----------------------------------------------------------------------------
var
  I: Integer;
  Enable: Boolean;
begin
  // Enable Reopen item if MRU list not empty
  Enable := False;
  for I := 0 to MRUList.Count-1 do
  begin
    if Length(MRUList[I]) > 0 then Enable := True;
  end;
  MnuReopen.Enabled := Enable;

  // Enable/disable printing (if printing is allowed)
  if MnuPageSetup.Enabled then
  begin
    MnuPrint.Enabled :=
      (ActiveMDIChild is TTableForm) or
      (ActiveMDIChild is TGraphForm) or
      (ActiveMDIChild is TProfilePlotForm) or
      (ActiveMDIChild is TStatsReportForm) or
      (ActiveMDIChild is TStatusForm) or
      (ActiveMDIChild is TResultsForm) or
      (ActiveMDIChild is TMapForm);
    MnuPrintPreview.Enabled := MnuPrint.Enabled;
  end;

  // Enable/disable Export | Hotstart File and Export | Status Report
  MnuExportHotStart.Enabled := RunFlag;
  MnuExportStatusRpt.Enabled := RunFlag;
end;


procedure TMainForm.MnuNewClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Creates a new project when File|New selected from main menu.
//-----------------------------------------------------------------------------
begin
  // Save current project data if it has changed
  if SaveFileDlg(Sender) = mrCancel then Exit;

  // Close any simulation output display forms
  CloseForms;

  // Re-set name of input project file
  InputFileName := '';
  InputFileType := iftINP;
  Caption := TXT_MAIN_CAPTION;
  ReadOnlyFlag := False;

  // Clear current project data
  ClearAll;             // Clears all project data
  ShowRunStatus;        // Resets run status icon
  PageSetup;            // Resets the printer page options

  // Make Title/Notes be the Browser's current data category
  Ubrowser.BrowserUpdate(NOTES, Project.CurrentItem[NOTES]);
  BrowserPageControl.ActivePage := BrowserDataPage;
  ObjectTreeView.SetFocus;
end;


procedure TMainForm.MnuOpenClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Opens a project file when File|Open selected from main menu.
//-----------------------------------------------------------------------------
var
  Fname: String;
begin
  // Prompt the user to either save the current project data or to cancel
  if SaveFileDlg(Sender) = mrCancel then Exit;

  // Execute the Open File dialog
  with OpenTextFileDialog do
  begin
    // Set options for the Open File dialog control
    Title := TXT_OPEN_PROJECT_TITLE;
    Filter := TXT_OPEN_PROJECT_FILTER;
    InitialDir := ProjectDir;
    Filename := InputFileName;
    Options := Options - [ofHideReadOnly] + [ofFileMustExist];

    // If the user selects a file, then open it
    if Execute then
    begin
      ReadOnlyFlag := (ofReadOnly in Options) or
                      (HasAttr(FileName, faReadOnly));
      Fname := Filename;
    end;
    FilterIndex := 1;
  end;
  Application.ProcessMessages;
  if Length(Fname) > 0 then
  begin
    OpenFile(Sender, Fname);
  end;
end;


procedure TMainForm.MnuReopenClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for File|Reopen menu item.
//-----------------------------------------------------------------------------
begin
  MRUDisplay(Sender);
end;


procedure TMainForm.MnuSaveClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for File|Save menu item.
//-----------------------------------------------------------------------------
begin
  // For a new, un-named input file, implement the Save As command
  if (Length(InputFileName) = 0) then MnuSaveAsClick(Sender)

  // Otherwise save project data under the current input file name
  else SaveFile(InputFileName);
end;


procedure TMainForm.MnuSaveAsClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Saves project to a new file when File|Save As selected from main menu.
//-----------------------------------------------------------------------------
var
  Fname: String;
begin
  with SaveDialog do
  begin
    Title := TXT_SAVE_PROJECT_TITLE;
    Filter := TXT_SAVE_PROJECT_FILTER;
    InitialDir := ProjectDir;
    DefaultExt := TXT_INP;
    if Length(InputFileName) > 0
    then Filename := ChangeFileExt(ExtractFileName(InputFileName), '.' + TXT_INP)
    else Filename := '*.' + TXT_INP;
    if Execute then Fname := Filename;;
    DefaultExt := '';
  end;
  Application.ProcessMessages;
  if Length(Fname) > 0 then SaveFile(Fname);
end;


procedure TMainForm.MnuExportMapClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Launches the Map Export dialog when File|Export|Map selected.
// Allows the study area map image to be saved to file.
//-----------------------------------------------------------------------------
begin
  with TMapExportForm.Create(self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;


procedure TMainForm.MnuExportStatusRptClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Save a run's Status and Summary Reports to file.
//-----------------------------------------------------------------------------
var
  Source: String;
begin
  if not FileExists(Uglobals.TempReportFile) then
    Uutils.MsgDlg(MSG_NO_STATUS_RPT, mtInformation, [mbOK], self)
  else with SaveDialog do
  begin
    Title := TXT_SAVE_STATUS_RPT;
    Filter := TXT_SAVE_RPT_FILTER;
    DefaultExt := TXT_RPT;
    Filename := '*.' + TXT_RPT;
    InitialDir := ProjectDir;
    if Execute then
    begin
      Source := Copy(Uglobals.TempReportFile, 1, MaxInt);
      if not Windows.CopyFile(PChar(Source), PChar(Filename), False)
      then Uutils.MsgDlg(MSG_NO_EXPORT_RPT, mtInformation, [mbOK], self);
    end;
    DefaultExt := '';
  end;
end;

procedure TMainForm.MnuExportHotstartClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Allows output results at the current time period to be saved to a
// Hotstart file.
//-----------------------------------------------------------------------------
var
  Fname: String;
begin
  with SaveDialog do
  begin
    Title := TXT_SAVE_HOTSTART_AS;
    Filter := TXT_HOTSTART_FILTER;
    DefaultExt := TXT_HSF;
    Filename := '*.' + TXT_HSF;
    InitialDir := ProjectDir;
    if Execute then Fname := Filename;
    DefaultExt := '';
  end;
  Application.ProcessMessages;
  if Length(Fname) > 0 then Uexport.SaveHotstartFile(Fname);
end;


procedure TMainForm.MnuCombineClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Launches the Combine dialog form when File|Combine selected.
// Allows two SWMM Routing Interface results files to be combined into one
// (see Help file or Users Manual for description of a Routing Interface file.)
//-----------------------------------------------------------------------------
begin
  with TFileCombineForm.Create(self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;


procedure TMainForm.MnuPageSetupClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Displays the Page Setup dialog when File|Page Setup selected.
//-----------------------------------------------------------------------------
begin
  // Transfer current contents of the PageLayout structure to
  // the PageSetupDialog component
  PageSetupDialog.PageMargins.Left   := PageLayout.LMargin;
  PageSetupDialog.PageMargins.Right  := PageLayout.RMargin;
  PageSetupDialog.PageMargins.Top    := PageLayout.TMargin;
  PageSetupDialog.PageMargins.Bottom := PageLayout.BMargin;
  PageSetupDialog.BoldFont := BoldFonts;
  Printer.Orientation := TPrinterOrientation(Orientation);

  // Execute the dialog
  if PageSetupDialog.Execute then
  begin

    // Transfer new margins to the PageLayout structure
    PageLayout.LMargin := PageSetupDialog.PageMargins.Left;
    PageLayout.RMargin := PageSetupDialog.PageMargins.Right;
    PageLayout.TMargin := PageSetupDialog.PageMargins.Top;
    PageLayout.BMargin := PageSetupDialog.PageMargins.Bottom;

    // Setup the printer's page options
    Orientation := Ord(Printer.Orientation);
    PageSetup;
    HasChanged := True;
  end;
end;


procedure TMainForm.MnuPrintPreviewClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Prints the active window to a preview form when File|Print Preview selected.
//-----------------------------------------------------------------------------
begin
  Print(Xprinter.dPreview);
end;


procedure TMainForm.MnuPrintClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Prints the active window to the printer when File|Print selected.
//-----------------------------------------------------------------------------
begin
  Print(Xprinter.dPrinter);
end;


procedure TMainForm.Print(Dest: TDestination);
//-----------------------------------------------------------------------------
// Prints the active window to Dest (Preview window or printer).
//-----------------------------------------------------------------------------
begin
    if ActiveMDIChild is TMapForm
    then TMapForm(ActiveMDIChild).Print(Dest)

    else if ActiveMDIChild is TGraphForm
    then TGraphForm(ActiveMDIChild).Print(Dest)

    else if ActiveMDIChild is TProfilePlotForm
    then TProfilePlotForm(ActiveMDIChild).Print(Dest)

    else if ActiveMDIChild is TStatsReportForm
    then TStatsReportForm(ActiveMDIChild).Print(Dest)

    else if ActiveMDIChild is TTableForm
    then TTableForm(ActiveMDIChild).Print(Dest)

    else if ActiveMDIChild is TStatusForm
    then TStatusForm(ActiveMDIChild).Print(Dest)

    else if ActiveMDIChild is TResultsForm
    then TResultsForm(ActiveMDIChild).Print(Dest);
end;


procedure TMainForm.MnuExitClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Shuts down program when File|Exit selected from main menu.
//-----------------------------------------------------------------------------
begin
  Close;
end;


//=============================================================================
//                         Edit Menu Handlers
//=============================================================================

procedure TMainForm.MnuEditClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for Edit menu.
//-----------------------------------------------------------------------------
begin
  // Menu options Select Object, Select Vertex, & Select Region apply
  // only when the MapForm window is active
  MnuSelectObject.Enabled := ActiveMDIChild is TMapForm;
  MnuSelectVertex.Enabled := (ActiveMDIChild is TMapForm) and
                             MapForm.ToolButton2.Enabled;
  MnuSelectRegion.Enabled := ActiveMDIChild is TMapForm;

  // Select All applies when either the MapForm window, a TableForm window
  // or a StatsReportForm window is active
  MnuSelectAll.Enabled := (ActiveMDIChild is TMapForm) or
                          (ActiveMDIChild is TTableForm) or
                          (ActiveMDIChild is TStatusForm) or
                          ((ActiveMDIChild is TStatsReportForm) and
                           (TStatsReportForm(ActiveMDIChild).
                            PageControl1.ActivePageIndex = 1));

  // Group editing applies only if a fenceline has been drawn on the
  // MapForm's map
  MnuGroupEdit.Enabled := (not MapForm.Linking) and
                          (MapForm.NumFencePts > 0);
  MnuGroupDelete.Enabled := MnuGroupEdit.Enabled;

  // Only allow certain windows to be copied
  MnuCopy.Enabled := (ActiveMDIChild is TMapForm) or
                     (ActiveMDIChild is TGraphForm) or
                     (ActiveMDIChild is TProfilePlotForm) or
                     (ActiveMDIChild is TStatusForm) or
                     (ActiveMDIChild is TResultsForm) or
                     (ActiveMDIChild is TTableForm) or
                     (ActiveMDIChild is TStatsReportForm);
end;


procedure TMainForm.MnuCopyClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Copies active window to the clipboard or to a file when Edit|Copy To
// is selected.
//-----------------------------------------------------------------------------
begin
  if ActiveMDIChild is TMapForm
  then TMapForm(ActiveMDIChild).CopyTo

  else if ActiveMDIChild is TStatusForm
  then TStatusForm(ActiveMDIChild).CopyTo

  else if ActiveMDIChild is TResultsForm
  then TResultsForm(ActiveMDIChild).CopyTo

  else if ActiveMDIChild is TGraphForm
  then TGraphForm(ActiveMDIChild).CopyTo

  else if ActiveMDIChild is TProfilePlotForm
  then TProfilePlotForm(ActiveMDIChild).CopyTo

  else if ActiveMDIChild is TTableForm
  then TTableForm(ActiveMDIChild).CopyTo

  else if ActiveMDIChild is TStatsReportForm
  then TStatsReportForm(ActiveMDIChild).CopyTo;
end;


procedure TMainForm.MnuSelectObjectClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Puts the MapForm into Object Selection mode when Edit|Select Object
// is selected.
//-----------------------------------------------------------------------------
begin
  SelectorButtonClick;
end;


procedure TMainForm.MnuSelectVertexClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Puts the MapForm into Vertex Selection mode when Edit|Select Vertex
// is selected.
//-----------------------------------------------------------------------------
begin
  MapForm.MapButtonClick(MapForm.ToolButton2);
end;


procedure TMainForm.MnuSelectRegionClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Puts the MapForm into Region Selection mode when Edit|Select Region
// is selected.
//-----------------------------------------------------------------------------
begin
  MapForm.MapButtonClick(MapForm.ToolButton3);
end;


procedure TMainForm.MnuSelectAllClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Selects all objects on a given form when Edit|Select All is selected.
//-----------------------------------------------------------------------------
begin
  if ActiveMDIChild is TMapForm then TMapForm(ActiveMDIChild).SelectAll;
  if ActiveMDIChild is TTableForm then TTableForm(ActiveMDIChild).SelectAll;
  if ActiveMDIChild is TStatsReportForm
  then TStatsReportForm(ActiveMDIChild).SelectAll;
end;


procedure TMainForm.MnuFindObjectClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Activates the FindForm to locate a visual object by name.
//-----------------------------------------------------------------------------
begin
  FindForm.Visible := True;
end;


procedure TMainForm.MnuGroupEditClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Displays the Group Edit dialog when Edit|Group Edit selected.
//-----------------------------------------------------------------------------
begin
  with TGroupEditForm.Create(self) do
  try
    PropEditForm.Hide;
    ShowModal;
  finally
    Free;
  end;
  if PropEditForm.Visible then Ubrowser.BrowserEditObject;
end;


procedure TMainForm.MnuGroupDeleteClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Deletes all objects in the selected region of the study area map when
// Edit|Group Delete is selected.
//-----------------------------------------------------------------------------
begin
  with TGroupDeleteForm.Create(self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;


//=============================================================================
//                         View Menu Handlers
//=============================================================================

procedure TMainForm.MnuDimensionsClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for View|Dimensions menu item.
//-----------------------------------------------------------------------------
begin
  MapForm.ModifyMapDimensions;
end;


procedure TMainForm.MnuBackdropClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for View|Backdrop menu item.
//-----------------------------------------------------------------------------
var
  EnableFlag: Boolean;
begin
  // EnableFlag is true if the project has a map backdrop file
  EnableFlag := (Length(MapForm.Map.Backdrop.Filename) > 0);

  // Enable the Load Backdrop option if there is no current backdrop file
  MnuBackdropLoad.Enabled := not EnableFlag;

  // Enable the other options if there is a current backdrop file
  MnuBackdropUnload.Enabled := EnableFlag;
  MnuBackdropAlign.Enabled := EnableFlag;
  MnuBackdropResize.Enabled := EnableFlag;
  MnuBackdropWatermark.Enabled := EnableFlag;
  MnuBackdropGrayscale.Enabled := EnableFlag;

  // Check the Watermark & Grayscale options if the backdrop is currently
  // displayed as such
  MnuBackdropWatermark.Checked := EnableFlag and MapForm.Map.Backdrop.Watermark;
  MnuBackdropGrayscale.Checked := EnableFlag and MapForm.Map.Backdrop.Grayscale;
end;


procedure TMainForm.MnuBackdropLoadClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the View|Backdrop|Load menu item.
// Loads a backdrop image file into the project's map display.
//-----------------------------------------------------------------------------
var
  BackdropFileForm : TBackdropFileForm;
begin
  BackdropFileForm := TBackdropFileForm.Create(self);
  try
    // Use the Backdrop File dialog to get the name of a backdrop file
    if BackdropFileForm.ShowModal = mrOK then with MapForm do
    begin

      // Retrieve the backdrop's file name and coordinates from the dialog
      Application.ProcessMessages;
      with Map.Backdrop do
      begin
        Source := bdFile;
        Filename := BackdropFileForm.GetBackdropFilename;
        BackdropFileForm.GetBackdropCoords(LowerLeft, UpperRight);
        Visible  := True;
      end;

      // Display the backdrop image on the study area map
      OpenBackdropFile;
      RedrawMap;

      // Update the Overview Map
      UpdateOVmap;
      if OVmapForm.Visible then OVmapForm.Redraw;
      Uglobals.HasChanged := True;
    end;

  finally
    BackdropFileForm.Free;
  end;
end;


procedure TMainForm.MnuBackdropStreetClick(Sender: TObject);
begin
  showmessage('Web map option ' + IntToStr((Sender As TMenuItem).Tag));
end;


procedure TMainForm.MnuBackdropUnloadClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for View|Backdrop|Unload menu item.
// Resets the project's backdrop image to a no-image default.
//-----------------------------------------------------------------------------
begin
  MapForm.Map.Backdrop := UMap.DefMapBackdrop;
  OVmapForm.OVmap.Backdrop := MapForm.Map.Backdrop;
  MapForm.RedrawMap;
  OVmapForm.Redraw;
  Uglobals.HasChanged := True;
end;


procedure TMainForm.MnuBackdropAlignClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for View|Backdrop|Align menu item.
// Allows the user to reposition the backdrop image over the study area map.
//-----------------------------------------------------------------------------
begin
  MapActionClick(MnuPan);
  MapForm.BeginAligning(Sender);
end;


procedure TMainForm.MnuBackdropResizeClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for View|Backdrop|Resize menu item.
// Displays the Backdrop Dimensions form to resize the backdrop image's
// bounding area.
//-----------------------------------------------------------------------------
begin
  if FormExists('BackdropDimensionsForm') then Exit;
  with TBackdropDimensionsForm.Create(self) do Show;
end;

procedure TMainForm.MnuBackdropWatermarkClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for View|Backdrop|Watermark menu item.
//-----------------------------------------------------------------------------
begin
  with MnuBackdropWatermark do
  begin
    Checked := not Checked;
    with MapForm do
    begin
      Map.Backdrop.Watermark := Checked;
      Map.RedrawBackdrop;
      RedrawMap;
    end;
  end;
  Uglobals.HasChanged := True;
end;


procedure TMainForm.MnuBackdropGrayscaleClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for View|Backdrop|Grayscale menu item.
//-----------------------------------------------------------------------------
begin
  with MnuBackdropGrayscale do
  begin
    Checked := not Checked;
    with MapForm do
    begin
      Map.Backdrop.Grayscale := Checked;
      Map.RedrawBackdrop;
      RedrawMap;
    end;
  end;
  Uglobals.HasChanged := True;
end;


procedure TMainForm.MapActionClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for Map menu items Full Extent, Rescale, Pan, and Zoom.
//-----------------------------------------------------------------------------
begin
  with MapForm do
  begin
    // Restore the Map window to its normal display state
    Show;
    SetFocus;
    WindowState := wsNormal;

    // The Tag property of each menu item was set to a
    // constant corresponding to the action it controls.
    if Sender is TMenuItem then with TMenuItem(Sender) do
    case Tag of

      // Display map at full extent
      FULLEXTENT:
      begin
        MapForm.DrawFullExtent;
        SelectorButtonClick;
      end;

      // Panning & zooming handled by MapButtons 4-6
      PAN:     MapForm.MapButtonClick(MapForm.ToolButton4);
      ZOOMIN:  MapForm.MapButtonClick(MapForm.ToolButton5);
      ZOOMOUT: MapForm.MapButtonClick(MapForm.ToolButton6);
    end;
  end;
end;


procedure TMainForm.MnuQueryClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Activates the Query dialog to highlight items on the map that
// meet a specific criteria when View|Query is selected.
//-----------------------------------------------------------------------------
begin
  QueryForm.Visible := True;
end;


procedure TMainForm.MnuOVMapClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Toggles display of the Overview Map when View|Overview Map selected.
//-----------------------------------------------------------------------------
begin
  MnuOVMap.Checked := not MnuOVMap.Checked;
  OVMapForm.Visible := MnuOVMap.Checked;
end;


procedure TMainForm.MnuObjectsClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for View|Objects menu item.
// Allows the user to toggle the display of certain categories of map objects.
//-----------------------------------------------------------------------------
begin
  MnuShowGages.Checked := MapForm.Map.Options.ShowGages;
  MnuShowSubcatch.Checked := MapForm.Map.Options.ShowSubcatchs;
  MnuShowNodes.Checked := MapForm.Map.Options.ShowNodes;
  MnuShowLinks.Checked := MapForm.Map.Options.ShowLinks;
  MnuShowLabels.Checked := MapForm.Map.Options.ShowLabels;
  MnuShowBackdrop.Checked := MapForm.Map.Backdrop.Visible;
  MnuShowBackdrop.Enabled := (Length(MapForm.Map.Backdrop.Filename) > 0);
end;

procedure TMainForm.MnuShowObjectsClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Common OnClick handler for View/Objects/Gages..Labels.
//-----------------------------------------------------------------------------
begin
  MapForm.PopupShowObjectsClick(Sender);
end;

procedure TMainForm.MnuShowBackdropClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for View/Objects/Backdrop.
//-----------------------------------------------------------------------------
begin
  MapForm.PopupShowBackdropClick(Sender);
end;


procedure TMainForm.MnuLegendsClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Enables/disables submenu items when View|Legends selected.
//-----------------------------------------------------------------------------
begin
  // Legend items are disabled if no view variable is selected
  // or if map is in Query mode
  MnuSubcatchLegend.Enabled   := (not Uglobals.QueryFlag) and
                                 (Uglobals.CurrentSubcatchVar <> NOVIEW);
  MnuNodeLegend.Enabled   := (not Uglobals.QueryFlag) and
                             (Uglobals.CurrentNodeVar <> NOVIEW);
  MnuLinkLegend.Enabled   := (not Uglobals.QueryFlag) and
                             (Uglobals.CurrentLinkVar <> NOVIEW);
  MnuTimeLegend.Enabled   := Uglobals.RunFlag;
  MnuModifyLegend.Enabled := (not Uglobals.QueryFlag);
end;


procedure TMainForm.MnuSubcatchLegendClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Toggles display of Map's Subcatchment legend when
// View|Legends|Subcatch selected.
//-----------------------------------------------------------------------------
begin
  MapForm.ToggleSubcatchLegend;
end;


procedure TMainForm.MnuLinkLegendClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Toggles display of Map's Link legend when View|Legends|Link selected.
//-----------------------------------------------------------------------------
begin
  MapForm.ToggleLinkLegend;
end;


procedure TMainForm.MnuNodeLegendClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Toggles display of Map's Node legend when View|Legends|Node selected.
//-----------------------------------------------------------------------------
begin
  MapForm.ToggleNodeLegend;
end;


procedure TMainForm.MnuTimeLegendClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Toggles display of Map's Time legend when View|Legends|Time selected.
//-----------------------------------------------------------------------------
begin
  MapForm.ToggleTimeLegend;
end;


procedure TMainForm.MnuToolbarsClick(Sender: TObject);
begin
  with Sender as TMenuItem do
    Toolbar1.Visible := Checked;
end;

procedure TMainForm.MnuModifyLegendClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Enables/disables submenu items when View|Legends|Modify selected.
//-----------------------------------------------------------------------------
begin
  MnuModifySubcatchLegend.Enabled := (Uglobals.CurrentSubcatchVar <> NOVIEW);
  MnuModifyNodeLegend.Enabled := (Uglobals.CurrentNodeVar <> NOVIEW);
  MnuModifyLinkLegend.Enabled := (Uglobals.CurrentLinkVar <> NOVIEW);
end;


procedure TMainForm.MnuModifySubcatchLegendClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Displays Legend Editor form when View|Legends|Modify|Subcatch is selected.
//-----------------------------------------------------------------------------
begin
  MapForm.ModifySubcatchLegend;
end;


procedure TMainForm.MnuModifyNodeLegendClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Displays Legend Editor form when View|Legends|Modify|Node is selected.
//-----------------------------------------------------------------------------
begin
  MapForm.ModifyNodeLegend;
end;


procedure TMainForm.MnuModifyLinkLegendClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Displays Legend Editor form when View|Legends|Modify|Link is selected.
//-----------------------------------------------------------------------------
begin
  MapForm.ModifyLinkLegend;
end;


//=============================================================================
//                        Project Menu Handlers
//=============================================================================

procedure TMainForm.MnuProjectSummaryClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Displays the Project Summary form when Project|Summary is selected.
//-----------------------------------------------------------------------------
begin
  with TProjectSummaryForm.Create(self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;


procedure TMainForm.MnuProjectDefaultsClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Displays the Project Defaults dialog when Project|Defaults is selected.
//-----------------------------------------------------------------------------
var
  DefaultsForm : TDefaultsForm;
begin
  PropEditForm.Hide;
  DefaultsForm := TDefaultsForm.Create(self);
  try
    if (DefaultsForm.ShowModal = mrOK)
    and (DefaultsForm.Modified = True)
    then SetChangeFlags;
  finally
    DefaultsForm.Free;
  end;
end;


procedure TMainForm.MnuProjectDetailsClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Displays current project data in the EPA SWMM input file format
// when Project|Details is selected.
//-----------------------------------------------------------------------------
var
  OldTabDelimited: Boolean;
  ProjectForm: TProjectForm;
begin
  OldTabDelimited := Uglobals.TabDelimited;
  Uglobals.TabDelimited := False;
  ProjectForm := TProjectForm.Create(Self);
  try
    ProjectForm.ShowModal;
  finally
    ProjectForm.Free;
  end;
  Uglobals.TabDelimited := OldTabDelimited;
end;


procedure TMainForm.MnuProjectCalibDataClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Displays the Calibration Data dialog to register calibration data
// when Project|Calibration Data is selected.
//-----------------------------------------------------------------------------
begin
  with TCalibDataForm.Create(self) do
  try
    if (ShowModal = mrOK) then Uglobals.RegisterCalibData;
  finally
    Free;
  end;
end;


procedure TMainForm.MnuProjectRunSimulationClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Runs a simulation when Project|Run Simulation is selected.
//-----------------------------------------------------------------------------
begin
  RunSimulation;
end;


//=============================================================================
//                         Report Menu Handlers
//=============================================================================

procedure TMainForm.MnuReportClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Controls what kind of reports can be generated when Report menu is selected.
//-----------------------------------------------------------------------------
begin
  // Reports are available only if a successful analysis run has been made
  MnuReportStatus.Enabled := True;
  MnuReportSummary.Enabled := Uglobals.RunFlag;
  MnuReportStatistics.Enabled := Uglobals.RunFlag;
  MnuReportGraph.Enabled := True;
  MnuReportTable.Enabled := Uglobals.RunFlag;

  // The Options menu item is enabled depending on type of active window
  MnuReportOptions.Enabled :=
    (ActiveMDIChild is TProfilePlotForm) or
    (ActiveMDIChild is TGraphForm) or
    ((ActiveMDIChild is TStatsReportForm) and
    (TStatsReportForm(ActiveMDIChild).PageControl1.ActivePageIndex = 2));
end;

procedure TMainForm.MnuReportStatusClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Displays a run's Status Report when Report|Status is selected.
//-----------------------------------------------------------------------------
var
  StatusForm : TStatusForm;
begin
  // Check if Status Report form already exists
  if FormExists('StatusForm') then Exit;

  // Otherwise create it
  StatusForm := TStatusForm.Create(self);
  try
    StatusForm.Caption := TXT_STATUS_REPORT;
    StatusForm.RefreshStatusReport;
    StatusForm.SetFocus;
  finally
  end;
end;

procedure TMainForm.MnuReportSummaryClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Displays a run's Summary Report when Report|Summary is selected.
//-----------------------------------------------------------------------------
var
  ResultsForm: TResultsForm;
begin
  // Check if Summary Results form already exists
  if FormExists('ResultsForm') then Exit;

  // Otherwise create it
  ResultsForm := TResultsForm.Create(self);
  try
    ResultsForm.RefreshReport;
    ResultsForm.SetFocus;
  finally
  end;
end;

procedure TMainForm.MnuReportGraphClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Enables Time Series and Scatter plot options only if results are
// available when Report|Graph is selected.
//-----------------------------------------------------------------------------
begin
  MnuGraphScatter.Enabled := Uglobals.RunFlag;
  MnuGraphTimeSeries.Enabled := Uglobals.RunFlag;
end;


procedure TMainForm.TBGraphClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the graphing toolbar buttons.
//-----------------------------------------------------------------------------
begin
  if Sender = TBGraph then MnuGraphTimeSeriesClick(Sender);
  if Sender = TBProfile then MnuGraphProfileClick(Sender);
  if Sender = TBScatter then MnuGraphScatterClick(Sender);
end;


procedure TMainForm.MnuGraphTimeSeriesClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Displays a Time Series Selection Plot dialog form when
// Report|Graph|Time Series is selected.
//-----------------------------------------------------------------------------
begin
  ReportSelectForm.Visible := False;
  TimePlotForm.Setup;
  TimePlotForm.Show;
end;


procedure TMainForm.MnuGraphScatterClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Displays the Report Selection dialog form for a Scatter Plot when
// Report|Graph|Scatter is selected.
//-----------------------------------------------------------------------------
begin
  if Assigned(ReportSelectForm) then with ReportSelectForm do
  try
    SetReportType(SCATTERPLOT);
    Show;
  finally
  end;
end;


procedure TMainForm.MnuGraphProfileClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Displays the Report Selection dialog form for a Profile Plot when
// Report|Graph|Profile is selected.
//-----------------------------------------------------------------------------
begin
  if not FormExists('ProfileSelectForm')
  then ProfileSelectForm := TProfileSelectForm.Create(self);
  with ProfileSelectForm do
  try
    Show;
  finally
  end;
end;


procedure TMainForm.MnuTableByObjectClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Displays the Report Selection dialog form for a Table by Object when
// Report|Table|By Object is selected.
//-----------------------------------------------------------------------------
begin
  if Assigned(ReportSelectForm) then with ReportSelectForm do
  try
    SetReportType(TABLEBYOBJECT);
    Show;
  finally
  end;
end;


procedure TMainForm.MnuTableByVariableClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Displays the Report Selection dialog form for a Table by Variable when
// Report|Table|By Variable is selected.
//-----------------------------------------------------------------------------
begin
  if Assigned(ReportSelectForm) then with ReportSelectForm do
  try
    SetReportType(TABLEBYVARIABLE);
    Show;
  finally
  end;
end;


procedure TMainForm.MnuReportStatisticsClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Displays the Statistics Selection dialog form when
// Report|Statistics is selected.
//-----------------------------------------------------------------------------
begin
  if not FormExists('StatsSelectForm')
  then with TStatsSelectForm.Create(self) do
  try
    Show;
  finally
  end;
end;


procedure TMainForm.MnuReportCustomizeClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for Report|Customize menu option.
//-----------------------------------------------------------------------------
begin
  if (ActiveMDIChild is TGraphForm)
  then with ActiveMDIChild as TGraphForm do SetGraphOptions
  else if (ActiveMDIChild is TProfilePlotForm)
  then with ActiveMDIChild as TProfilePlotForm do SetPlotOptions
  else if (ActiveMDIChild is TStatsReportForm)
  then with ActiveMDIChild as TStatsReportForm do SetGraphOptions;
end;


//=============================================================================
//                       Tool Menu Handlers
//=============================================================================

procedure TMainForm.MnuPreferencesClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Displays Preferences dialog form when Tools|Program Preferences selected.
//-----------------------------------------------------------------------------
var
  S: String;
begin
  with TPreferencesForm.Create(self) do
  try
    if ShowModal = mrOK then
    begin
      S := RetrieveStyleName;
    end;
  finally
    Free;
  end;
  if (Length(S) > 0) and (S <> TStyleManager.ActiveStyle.Name) then
  begin
    Uglobals.StyleName := S;
    if S = 'Windows' then
    begin
      Toolbar1.DrawingStyle := TTBDrawingStyle(dsNormal);
      StatusBar.DrawingStyle := TTBDrawingStyle(dsNormal);
      MapForm.MapToolBar.DrawingStyle := TTBDrawingStyle(dsNormal);
      TStyleManager.TrySetStyle(S);
    end
    else begin
      Toolbar1.DrawingStyle := TTBDrawingStyle(dsGradient);
      StatusBar.DrawingStyle := TTBDrawingStyle(dsGradient);
      MapForm.MapToolBar.DrawingStyle := TTBDrawingStyle(dsGradient);
      TStyleManager.TrySetStyle(S);
      Application.ProcessMessages;
      MnuWindowCascadeClick(self);
    end;
  end;
end;


procedure TMainForm.MnuViewOptionsClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Displays Map Options dialog when Tools|Map Display Options selected.
//-----------------------------------------------------------------------------
begin
  MapForm.SetMapOptions;
end;


procedure TMainForm.MnuConfigureToolsClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Displays a Tool Options dialog when Tools | Configure Tools selected.
//-----------------------------------------------------------------------------
begin
  with TToolOptionsForm.Create(Self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;


procedure TMainForm.ToolItemClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Launches an application when it is selected from the Tools menu.
//-----------------------------------------------------------------------------
begin
  Utools.RunTool(TMenuItem(Sender).Caption);
end;


//=============================================================================
//                       Window Menu Handlers
//=============================================================================

procedure TMainForm.MnuWindowClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the Window menu - disables the Close All & Tile
// menu items if the MapForm is the only MDI child window.
//-----------------------------------------------------------------------------
var
  Flag: Boolean;
begin
  if MDIChildCount <= 1 then Flag := False else Flag := True;
  MnuWindowCloseAll.Enabled := Flag;
  MnuWindowTile.Enabled := Flag;
end;


procedure TMainForm.MnuWindowCascadeClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Cascades MDI child windows when Window|Cascade is selected.
//-----------------------------------------------------------------------------
var
  Rect: TRect;
  I: Integer;
begin
  // Cascade the child windows
  if (MDIChildCount >= 1) then
  begin
    // First prevent windows from re-drawing and cascade them
    LockWindowUpdate(Handle);
    MapForm.RedrawOnResize := False;
    Cascade;
    MapForm.RedrawOnResize := True;

    // Make the Map form occupy the full MDI client area
    if Assigned(MapForm) then with MapForm do
    begin
      GetWindowRect(MainForm.ClientHandle,Rect);
      SetBounds(0, 0, Rect.Width-4, Rect.Height-5);
    end;

    // Allow windows to re-draw themselves
    LockWindowUpdate(0);
  end;

  // Place each child window in front of the Map window
  for I := MDIChildCount - 1 downto 0 do
  begin
   if MDIChildren[I] is TMapForm then continue;
   MDIChildren[I].Show;
  end;

end;


procedure TMainForm.MnuWindowTileClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Tiles MDI child windows when Window|Cascade is selected.
//-----------------------------------------------------------------------------
begin
  MapForm.WindowState := wsMinimized;
  Tile;
end;


procedure TMainForm.MnuWindowCloseAllClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Closes all MDI children (except the Map) when Window|Close All is selected.
//-----------------------------------------------------------------------------
begin
  CloseForms;
end;


procedure TMainForm.MnuAboutClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Displays About form when Help|About is selected.
//-----------------------------------------------------------------------------
begin
  with TAboutBoxForm.Create(self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;


//=============================================================================
//                        Toolbar Button Handlers
//=============================================================================

procedure TMainForm.TBOptionsClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the Options toolbar button.
//-----------------------------------------------------------------------------
begin
  if (ActiveMDIChild is TMapForm)
  then with ActiveMDIChild as TMapForm do SetMapOptions
  else MnuReportCustomizeClick(Sender);
end;


procedure TMainForm.SelectorButtonClick;
//-----------------------------------------------------------------------------
// Activates the Select tool button on Map Toolbar.
//-----------------------------------------------------------------------------
begin
  //HideProgressBar;
  ShowStatusHint('');
  BrowserBtnNew.Down := False;
  //MapForm.ToolButtonClick(101);
  MapForm.MapButtonClick(MapForm.ToolButton1);
end;


procedure TMainForm.SetAllUp(Toolbar: TToolbar);
//-----------------------------------------------------------------------------
// Places all buttons on a toolbar in the up position.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  for I := 0 to Toolbar.ButtonCount-1 do Toolbar.Buttons[I].Down := False;
end;

procedure TMainForm.EnableReportButtons(IsEnabled: Boolean);
//-----------------------------------------------------------------------------
// Enables/disables the toolbar buttons used for reporting results.
//-----------------------------------------------------------------------------
begin
  TBGraph.Enabled := IsEnabled;
  TBTable.Enabled := IsEnabled;
  TBStats.Enabled := IsEnabled;
  TBScatter.Enabled := IsEnabled;
  PopupReportSummary.Enabled := IsEnabled;
end;

//=============================================================================
//                     Data Browser Procedures
//=============================================================================

procedure TMainForm.BrowserBtnNewClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the New button on the Browser's Data page.
//-----------------------------------------------------------------------------
var
  I: Integer;
  aToolButton: TToolButton;
begin
  // To add a new visual object, simulate a click on their toolbar button
  if Uglobals.CurrentList in
    [RAINGAGE, SUBCATCH, JUNCTION..STORAGE, CONDUIT..OUTLET, MAPLABEL]
  then
  begin

    for I := 9 to 20 do
    begin
      aToolButton := TToolButton(MapForm.FindComponent('ToolButton' + IntToStr(I)));
      if aToolButton.Tag = Uglobals.CurrentList then
      begin
        MapForm.MapButtonClick(aToolButton);
        BrowserBtnNew.Down := True;
        break;
      end;
    end;

    case Uglobals.CurrentList of
    RAINGAGE: ShowStatusHint(TXT_ADD_RAINGAGE);
    SUBCATCH: ShowStatusHint(TXT_ADD_SUBCATCH);
    JUNCTION..DIVIDER: ShowStatusHint(TXT_ADD_NODE);
    STORAGE: ShowStatusHint(TXT_ADD_STORAGE);
    CONDUIT..OUTLET: ShowStatusHint(TXT_ADD_LINK);
    MAPLABEL: ShowStatusHint(TXT_ADD_LABEL);
    end;
  end

  // For non-visual objects add a new instance directly
  else Ubrowser.BrowserNewObject;
end;


procedure TMainForm.BrowserBtnDeleteClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the Delete button on the Browser's Data page.
//-----------------------------------------------------------------------------
begin
  Ubrowser.BrowserDeleteObject;
end;


procedure TMainForm.BrowserBtnEditClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the Edit button on the Browser's Data page.
//-----------------------------------------------------------------------------
begin
  Ubrowser.BrowserEditObject;
end;


procedure TMainForm.BrowserBtnUpClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the Up button on the Browser's Data page.
// Moves the current selection in the ItemListBox up one position
// in the list of objects of that type.
//-----------------------------------------------------------------------------
var
  ItemIndex: Integer;
begin
  if Uglobals.CurrentList < 0 then Exit;
  ItemIndex := Project.CurrentItem[Uglobals.CurrentList];
  with Project.Lists[Uglobals.CurrentList] do
    if ItemIndex > 0 then
    begin
      Exchange(ItemIndex, ItemIndex-1);
      ItemListBox.ItemIndex := ItemIndex-1;
      Project.CurrentItem[Uglobals.CurrentList] := ItemListBox.ItemIndex;
      Uglobals.HasChanged := True;
    end;
end;


procedure TMainForm.BrowserBtnDownClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the Down button on the Browser's Data page.
// Moves the current selection in the ItemListBox down one position
// in the list of objects of that type.
//-----------------------------------------------------------------------------
var
  ItemIndex: Integer;
begin
  if Uglobals.CurrentList < 0 then Exit;
  ItemIndex := Project.CurrentItem[Uglobals.CurrentList];
  with Project.Lists[Uglobals.CurrentList] do
    if ItemIndex < Count-1 then
    begin
      Exchange(ItemIndex, ItemIndex+1);
      ItemListBox.ItemIndex := ItemIndex+1;
      Project.CurrentItem[Uglobals.CurrentList] := ItemListBox.ItemIndex;
      Uglobals.HasChanged := True;
    end;
end;


procedure TMainForm.BrowserBtnSortClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the Sort button on the Browser's Data page.
// Sorts the items of the current object category by ID name.
//-----------------------------------------------------------------------------
begin
  Ubrowser.BrowserSortObjects;
end;


procedure TMainForm.ObjectTreeViewClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the ObjectTreeView on the Browser's Data page.
//-----------------------------------------------------------------------------
begin
  MainForm.SelectorButtonClick;
end;


procedure TMainForm.ObjectTreeViewKeyPress(Sender: TObject; var Key: Char);
//-----------------------------------------------------------------------------
// Prevents beeping when ObjectTreeView is focused & a key is pressed.
//-----------------------------------------------------------------------------
begin
  Key := #0;
end;


procedure TMainForm.ObjButtonClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the Object toolbar buttons.
//-----------------------------------------------------------------------------
begin
  SetAllUp(Toolbar1);
  with Sender as TToolButton do
  begin
    Down := True;
    Uglobals.CurrentList := Tag;
  end;
  BrowserBtnNewClick(Sender);
end;


procedure TMainForm.ObjectTreeViewChange(Sender: TObject; Node: TTreeNode);
//-----------------------------------------------------------------------------
// OnChange handler for the ObjectTreeView on the Browser's Data page.
//-----------------------------------------------------------------------------
var
  ObjType: Integer;
  ItemIndex: Integer;
  Caption: String;
begin
  // Determine which object category corresponds to the tree node selected
  // in the ObjectTreeView control
  ObjType := Ubrowser.GetObjectFromIndex(Node.AbsoluteIndex);

  // A general category (with no specific items) was selected
  if ObjType < 0 then
  begin
    ItemIndex := -1;
    Caption := '';
  end

  // A category with specific items was selected
  else
  begin
    ItemIndex := Project.CurrentItem[ObjType];
    Caption := Node.Text;
  end;

  // Update the display of items for the selected category
  ItemsLabel.Caption := Caption;
  Ubrowser.BrowserUpdate(ObjType, ItemIndex);
end;


procedure TMainForm.ItemListBoxData(Control: TWinControl; Index: Integer;
  var Data: string);
begin
  // Check for valid item index
  Data := '';
  //aColor := clVLB;
  if (Index < 0)
  or (Index >= Project.Lists[Uglobals.CurrentList].Count)
  then exit;
{
  // Gray-out display of item name if its a node with no coordinates
  if Project.IsNode(Uglobals.CurrentList) then
  begin
    if (Project.GetNode(Uglobals.CurrentList, Index).X = MISSING)
    or (Project.GetNode(Uglobals.CurrentList, Index).Y = MISSING)
    then aColor := clGray;
  end;
}
  // Get database ID label of item at current index
  Data := Project.GetID(Uglobals.CurrentList, Index);

end;


procedure TMainForm.ItemListBoxClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the Browser panel's ItemListBox.
//-----------------------------------------------------------------------------
begin
  with ItemListBox do
  begin
    if ItemIndex >= 0 then
    begin
      Ubrowser.BrowserUpdate(Uglobals.CurrentList, ItemIndex);
      if Assigned(FindForm)
      then FindForm.SearchFor(Uglobals.CurrentList, ItemIndex);
    end;
  end;
end;


procedure TMainForm.ItemListBoxDblClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnDblClick handler for the Browser panel's ItemListBox --
// edits the item double-clicked on.
//-----------------------------------------------------------------------------
begin
  // End any drag operation begun on a MouseDown action
  ItemListBox.EndDrag(False);

  // Select & edit the list box item
  ItemListBoxClick(Sender);
  if ItemListBox.ItemIndex >= 0 then Ubrowser.BrowserEditObject;
end;


procedure TMainForm.ItemListBoxDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
//-----------------------------------------------------------------------------
// OnDrawItem handler for the Browser panel's ItemListBox.
//-----------------------------------------------------------------------------
Var
 LListBox : TListBox;
begin
  // For native Windows style, draw item in the usual fashion
  LListBox := TListBox(Control);
  if not StyleServices.Enabled then with LListBox.Canvas do
  begin
    FillRect(Rect);
    TextRect(Rect, Rect.Left+2, Rect.Top, LListBox.Items[Index]);
    Exit;
  end;

  // Check the state
  if odSelected in State then
  begin
    LListBox.Canvas.Brush.Color := StyleServices.GetSystemColor(clHighlight);
  end;

  // Draw the background and text
  LListBox.Canvas.FillRect(Rect);
  SetBkMode(LListBox.Canvas.Handle, TRANSPARENT);
  LListBox.Canvas.TextOut(Rect.Left + 2, Rect.Top+2, LListBox.Items[Index]);

  // Draw the Highlight rect using the vcl styles colors
  if odFocused In State then
  begin
    LListBox.Canvas.Brush.Color := StyleServices.GetSystemColor(clHighlight);
    LListBox.Canvas.DrawFocusRect(Rect);
  end;
end;


procedure TMainForm.ItemListBoxKeyPress(Sender: TObject; var Key: Char);
//-----------------------------------------------------------------------------
// OnKeyPress handler for the Browser panel's ItemListBox --
// edits the item if the Enter key was pressed.
//-----------------------------------------------------------------------------
begin
  if Key = #13 then
  begin
    ItemListBoxDblClick(Sender);
    Key := #0;
  end;
end;


procedure TMainForm.ItemListBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
//-----------------------------------------------------------------------------
// OnKeyDown handler for the Browser panel's ItemListBox --
// deletes the item if the Delete key was pressed or inserts a new item if
// the Insert key was pressed.
//-----------------------------------------------------------------------------
begin
  if Project.IsSortable(Uglobals.CurrentList) then case Key of
  VK_DELETE:  BrowserBtnDeleteClick(Sender);
  VK_INSERT:  BrowserBtnNewClick(Sender);
  end;
end;


procedure TMainForm.ItemListBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
//-----------------------------------------------------------------------------
// OnMouseDown handler for the Browser panel's ItemListBox. Allows an
// item to be dragged from the ItemListBox.
//-----------------------------------------------------------------------------
begin
  // Only visual objects can be dragged
  if (Button = mbLeft)
  and (Uglobals.CurrentList in
      [RAINGAGE, SUBCATCH, JUNCTION, OUTFALL, DIVIDER, STORAGE, MAPLABEL])
  //then with Sender as TVirtualListBox do
  then with Sender as TListBox do
  begin
    if ItemAtPos(Point(X, Y), True) >= 0 then BeginDrag(False);
  end;
end;

//=============================================================================
//                        Map Browser Procedures
//=============================================================================

procedure TMainForm.MapViewBoxChange(Sender: TObject);
//-----------------------------------------------------------------------------
// Generic OnChange handler for the combo boxes that select a
// map theme on the Browser panel's Map page.
//-----------------------------------------------------------------------------
begin
  if (Sender = SubcatchViewBox)
  and (SubcatchViewBox.ItemIndex <> Uglobals.CurrentSubcatchVar)
  then Ubrowser.ChangeMapTheme(SUBCATCHMENTS, SubcatchViewBox.ItemIndex)

  else if (Sender = NodeViewBox)
  and (NodeViewBox.ItemIndex <> Uglobals.CurrentNodeVar)
  then Ubrowser.ChangeMapTheme(NODES, NodeViewBox.ItemIndex)

  else if (Sender = LinkViewBox)
  and (LinkViewBox.ItemIndex <> Uglobals.CurrentLinkVar)
  then Ubrowser.ChangeMapTheme(LINKS, LinkViewBox.ItemIndex);
end;


function NeedsSeparator(Control: TWinControl; Index: Integer): Boolean;
//-----------------------------------------------------------------------------
// Determines if an item in a map theme combo box needs a separator line.
//-----------------------------------------------------------------------------
begin
  Result := False;
  if SameText(Control.Name, 'SubcatchViewBox') then
  begin
    if (Index = SUBCATCHRPTVAR1) or (Index = SUBCATCHOUTVAR1) then Result := True;
  end
  else if SameText(Control.Name, 'NodeViewBox') then
  begin
    if (Index = NODERPTVAR1) or (Index = NODEOUTVAR1) then Result := True;
  end
  else if SameText(Control.Name, 'LinkViewBox') then
  begin
    if (Index = LINKRPTVAR1) or (Index = LINKOUTVAR1) then Result := True;
  end
end;


procedure TMainForm.MapViewBoxDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
//-----------------------------------------------------------------------------
// Draws an item in a map theme combo box.
//-----------------------------------------------------------------------------
var
  ItemText: String;
  DrawSeparator: Boolean;
  TxtRect: TRect;
begin
  DrawSeparator := NeedsSeparator(Control, Index);

  with (Control as TComboBox).Canvas do
  begin

    if odSelected In State then
    begin
      Pen.Color := clHighlightText;
      Brush.Color := clHighlight;
      DrawSeparator := False;
    end;

    TxtRect := Rect;
    if DrawSeparator and (Control as TComboBox).DroppedDown then
      Inc(TxtRect.Top, 3);
    FillRect(Rect);

    ItemText := (Control as TComboBox).Items[Index];
    TextOut(TxtRect.Left + 3,
      (TxtRect.Top + TxtRect.Bottom -  TextHeight(ItemText)) div 2, ItemText);

    if DrawSeparator and (Control as TComboBox).DroppedDown then
    begin
      Pen.Color := Font.Color;
      MoveTo(Rect.Left, Rect.Top + 1);
      LineTo(Rect.Right, Rect.Top + 1);
    end;

  end;
end;


procedure TMainForm.MapViewBoxMeasureItem(Control: TWinControl;
  Index: Integer; var Height: Integer);
//-----------------------------------------------------------------------------
// Determines the height of an item in a map theme combo box.
//-----------------------------------------------------------------------------
begin
  with Control as TComboBox do
  begin
    Height := ItemHeight;
    if (Index <> -1) and NeedsSeparator(Control, Index) then
      Height := Height + 3;
  end;
end;


//=============================================================================
//  The following procedures are event handlers for the Time/Date/ElapsedTime
//  controls on the Map page of the Browser panel.
//=============================================================================

procedure TMainForm.DateListBoxClick(Sender: TObject);
begin
  Ubrowser.ChangeDate(DateListBox.ItemIndex);
end;

procedure TMainForm.DateScrollBarChange(Sender: TObject);
begin
  DateListBox.ItemIndex := DateScrollBar.Position;
end;

procedure TMainForm.DateScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  if ScrollCode in
  [scLineUp, scLineDown, scPageUp, scPageDown, scPosition] then
  begin
    DateListBox.ItemIndex := ScrollPos;
    DateListBoxClick(Sender);
  end;
end;


procedure TMainForm.TimeListBoxClick(Sender: TObject);
begin
  Ubrowser.ChangeTimePeriod(TimeListBox.ItemIndex);
end;


procedure TMainForm.TimeScrollBarChange(Sender: TObject);
begin
  TimeListBox.ItemIndex := TimeScrollBar.Position;
end;


procedure TMainForm.TimeScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  if ScrollCode in
    [scLineUp, scLineDown, scPageUp, scPageDown, scPosition] then
  begin
    TimeListBox.ItemIndex := ScrollPos;
    TimeListBoxClick(Sender);
  end;
end;


procedure TMainForm.ElapsedTimePanelKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_UP) or (Key = VK_RIGHT) then
    Ubrowser.IncreaseElapsedTime
  else if (Key = VK_DOWN) or (Key = VK_LEFT) then
    Ubrowser.DecreaseElapsedTime;
  Key := 0;
end;

procedure TMainForm.ElapsedTimeUpDownClick(Sender: TObject; Button: TUDBtnType);
begin
  if Button = btNext then
    Ubrowser.IncreaseElapsedTime
  else if Button = btPrev then
    Ubrowser.DecreaseElapsedTime;
end;

//=============================================================================
//  The following procedures handle resizing of the Browser panel's
//  controls when the panel is re-sized by the user.
//=============================================================================

procedure TMainForm.BrowserPageControlResize(Sender: TObject);
var
  S: String;
begin
  ResizeControl(MapThemesBox);
  ResizeControl(MapTimePeriodBox);
  ResizeControl(SubcatchViewBox);
  ResizeControl(NodeViewBox);
  ResizeControl(LinkViewBox);
  ResizeControl(DateListBox);
  ResizeControl(DateScrollBar);
  ResizeControl(TimeListBox);
  ResizeControl(TimeScrollBar);
  DateScrollBar.Top := DateListBox.Top + DateListBox.Height + 2;
  TimeScrollBar.Top := TimeListBox.Top + TimeListBox.Height + 2;
  with ElapsedTimePanel do
  begin
    // Need to save & redisplay panel's display text since associating
    // the panel with ElapsedTimeUpDown causes it to display the
    // the latter's Position property
    if Enabled then S := Text else S := '';
    Width := Parent.ClientWidth - 2*Left - ElapsedTimeUpDown.Width;
    Text := S;
  end;
end;


procedure TMainForm.ResizeControl(aControl: TControl);
begin
  with aControl do
    Width := Parent.ClientWidth - 2*Left;
end;


procedure TMainForm.RecenterControl(aControl: TControl);
begin
  with aControl do
    Left := (Parent.ClientWidth - Width) div 2;
end;

//=============================================================================
//              Most Recently Used (MRU) File Procedures
//              (MRUList is declared in Uglobals.pas.)
//=============================================================================

procedure TMainForm.MRUUpdate(Sender: TObject; const AddFileName: String);
//-----------------------------------------------------------------------------
// Updates the MRU list when a new file (AddFileName) is opened.
//-----------------------------------------------------------------------------
var
  Index: Integer;
begin
  Index := 0;
  while Index < (MRUList.Count - 1) do
    if AddFileName = MRUList[Index]
    then MRUList.Delete(Index)
    else Index := Index + 1;
  while MRUList.Count > Uglobals.MAXMRUINDEX do
    MRUList.Delete(MRUList.Count - 1);
  while MRUList.Count < Uglobals.MAXMRUINDEX do
    MRUList.Add('');
  MRUList.Insert(0, AddFileName);
end;


procedure TMainForm.MRUDisplay(Sender: TObject);
//-----------------------------------------------------------------------------
// Displays the MRU file list on the File menu.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  for I := 0 to Uglobals.MAXMRUINDEX do
  begin
    MnuMRU[I].Caption := IntToStr(I) + ' ' + MRUList[I];
    MnuMRU[I].Visible := (MRUList[I] <> '');
  end;
end;


procedure TMainForm.MRUClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the File|MRU File menu item - opens the selected file.
//-----------------------------------------------------------------------------
var
  Index: Integer;
  Fname: String;
begin
  Index := TMenuItem(Sender).Tag;
  Fname := MRUList[Index];
  if not FileExists(Fname)
  then Uutils.MsgDlg(MSG_NO_INPUT_FILE, mtInformation, [mbOK], self)
  else if SaveFileDlg(Sender) <> mrCancel then
  begin
    ReadOnlyFlag := (HasAttr(Fname, faReadOnly));
    OpenFile(Sender, Fname);
  end;
end;


//=============================================================================
//                      File Open & Save Procedures
//=============================================================================

procedure TMainForm.OpenFile(Sender: TObject; const Fname: String);
//-----------------------------------------------------------------------------
// Opens an existing project file named Fname.
//-----------------------------------------------------------------------------
begin
  // Re-set file names
  Uglobals.InputFileName := Fname;
  ProjectDir := ExtractFilePath(Fname);
  SetCurrentDir(ProjectDir);
  MRUUpdate(Self, Uglobals.InputFileName);
  Caption := TXT_MAIN_CAPTION + ' - ' + ExtractFileName(Fname);

  // Close all output display forms
  CloseForms;

  // Clear all existing data
  ClearAll;
  ShowRunStatus;

  // Read contents of project file
  ReadInpFile(Fname);

  // If import was unsuccessful, then open a new, blank project
  if (Uglobals.InputFileType = iftNone) then
  begin
    MnuNewClick(Self);
    Exit;
  end;

  // If previous results were saved then prepare the Browser to display them
  if Uglobals.ResultsSaved then
  begin
    Uglobals.RunFlag := Uoutput.GetRunFlag(Uglobals.InputFileName);
    if Uglobals.RunFlag then
    begin
      Ubrowser.InitMapPage;
      RefreshResults;
      ShowRunStatus;
    end;
  end;
  RefreshMapForm;
end;

procedure TMainForm.ReadInpFile(const Fname: String);
//-----------------------------------------------------------------------------
// Reads project data from an input file.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  // Read project data from input file
  Uglobals.InputFileType := Uimport.OpenProject(Fname);
  StatusBar.Refresh;

  // Exit if unsuccessful
  if (Uglobals.InputFileType = iftNone) then Exit;

  // Check that any backdrop file named in the input file actually exists
  FindBackdropFile;

  // Reset printer's page properties
  PageSetup;

  // Initialize current item in each object category
  for I := 0 to MAXCLASS do
  begin
    if Project.Lists[I].Count > 0 then Project.CurrentItem[I] := 0;
  end;
  Uglobals.CurrentList := -1;

  // Re-scale and redraw the study area & overview maps
  MapForm.Map.Rescale;
  MapForm.OpenBackdropFile;
  MapForm.UpdateOVmap;

  // Display Title/Notes as the current object category in the Browser
  Ubrowser.BrowserUpdate(NOTES, Project.CurrentItem[NOTES]);
  BrowserPageControl.ActivePageIndex := 0;
  ObjectTreeView.SetFocus;
  Uglobals.HasChanged := False;
end;


procedure TMainForm.ShowProjectInfo(FileName: String);
//-----------------------------------------------------------------------------
//  Displays meta-information about a project stored in a text file.
//-----------------------------------------------------------------------------
var
  CmdLine: AnsiString;
  S: String;
begin
  S := ChangeFileExt(FileName, '.txt');
  if FileExists(S) then
  begin
    CmdLine := AnsiString('Notepad ' + S);
    WinExec(PAnsiChar(CmdLine),SW_SHOWNORMAL);
  end;
end;


procedure TMainForm.FindBackdropFile;
//-----------------------------------------------------------------------------
// Let's the user search for a map backdrop file.
//-----------------------------------------------------------------------------
begin
  with MapForm.Map do
  begin
    if  (Length(Backdrop.Filename) > 0) then
    begin
      if not FileExists(Backdrop.Filename) then
      begin
        if Uutils.MsgDlg(MSG_NO_BACKDROP + Backdrop.Filename +
         MSG_FIND_BACKDROP, mtWarning, [mbYes,mbNo], self) = mrYes then
        begin
          with OpenPictureDialog do
          begin
            Filename := ExtractFileName(Backdrop.Filename);
            if Execute then
            begin
              Backdrop.Filename := Filename;
              Backdrop.Visible := True;
            end
            else Backdrop := Umap.DefMapBackdrop;
          end;
        end
        else Backdrop := Umap.DefMapBackdrop;
      end
      else Backdrop.Visible := True;
    end;
  end;
end;


function TMainForm.SaveFileDlg(Sender: TObject): Integer;
//-----------------------------------------------------------------------------
// Checks if user wants to save current project to file.
//-----------------------------------------------------------------------------
begin
  // If project data has changed then ask user to save input & results
  Result := mrNo;
  if (not Uglobals.ReadOnlyFlag) and Uglobals.HasChanged then
  begin

    // See if input data should be saved
    Result := Uutils.MsgDlg(TXT_SAVE_CHANGES, mtConfirmation, mbYesNoCancel,
                            self);
    if Result = mrYes then
    begin
      MnuSaveClick(Sender);

      // See if most current results should be saved
      if Uglobals.RunFlag and not Uglobals.ResultsSaved then
      begin
        if Uglobals.AutoSave
        or (Uutils.MsgDlg(TXT_SAVE_RESULTS, mtConfirmation, [mbYes, mbNo],
                          self)
         = mrYes)
        then Uexport.SaveResults(Uglobals.InputFileName);
      end;
    end;
  end

  // If project data not changed made, see if results should be saved
  else if Uglobals.RunFlag and (not Uglobals.ResultsSaved) then
  begin
    if Uglobals.AutoSave
    or (Uutils.MsgDlg(TXT_SAVE_RESULTS, mtConfirmation, [mbYes, mbNo], self)
        = mrYes)
    then Uexport.SaveResults(Uglobals.InputFileName);
  end;
end;


procedure TMainForm.SaveFile(Fname: String);
//-----------------------------------------------------------------------------
// Saves project data in text format to file Fname.
//-----------------------------------------------------------------------------
begin
  // Append .inp extension to file name if none exists
  if ExtractFileExt(Fname) = '' then Fname := Fname + '.' + TXT_INP;

  // Check if project file is read-only
  if ReadOnlyFlag
  and (CompareText(Fname, Uglobals.InputFileName) = 0)
  then Uutils.MsgDlg(ExtractFileName(Uglobals.InputFileName) + MSG_READONLY,
                  mtInformation, [mbOK], self)

  // Save project under new name
  else
  begin
    Screen.Cursor := crHourGlass;
    Uexport.SaveProject(Fname);
    Uglobals.InputFileName := Fname;
    Uglobals.InputFileType := iftINP;
    Uglobals.ProjectDir := ExtractFilePath(Fname);
    SetCurrentDir(ProjectDir);
    Caption := Txt_MAIN_CAPTION + ' - ' +
               ExtractFileName(Uglobals.InputFileName);
    MRUUpdate(Self, Uglobals.InputFileName);
    Uglobals.HasChanged := False;
    Uglobals.ReadOnlyFlag := False;
    if AutoBackup then
      CopyFile(PChar(Fname), PChar(ChangeFileExt(Fname, '.' + TXT_BAK)), FALSE);
    Screen.Cursor := crDefault;
  end;
end;


procedure TMainForm.ClearAll;
//-----------------------------------------------------------------------------
// Clears the entire project database.
// (Called when File|New or File|Open selected)
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  // Clear all output and input data
  Uoutput.ClearOutput;
  DeleteTempFiles;
  Uglobals.TempInputFile  := '';
  Uglobals.TempReportFile := '';
  Uglobals.TempOutputFile := '';
  Project.Clear;

  // Hide the Property Editor
  PropEditForm.Hide;

  // Read project defaults from the INI file
  Uinifile.ReadDefaults;
  Uimport.SetDefaultDates;

  // Reset the Auto-Length feature
  Uglobals.AutoLength := False;
  AutoLengthBtn.Caption := 'Auto-Length: Off';

  // Reset the Link Offsets option
  Uupdate.UpdateOffsets;

  // Initialize the current item and next ID number for each object category
  Project.InitCurrentItems;

  // Reset the Browser panel and clear the MapForm
  Ubrowser.InitDataPage;
  Ubrowser.InitMapPage;
  MapForm.Map.Options := Umap.DefMapOptions;
  MapForm.ClearMap;
  MapForm.DrawSubcatchLegend;
  MapForm.DrawNodeLegend;
  MapForm.DrawLinkLegend;
  MapForm.WindowState := wsNormal;

  // Reset the printer's page layout
  InitPageLayout;

  // Clear all calibration file information
  for I := Low(CalibData) to High(CalibData) do
    Uglobals.CalibData[I].FileName := '';

  // Disable output reporting toolbar buttons
  EnableReportButtons(False);

  // Reset status flags
  Uglobals.HasChanged := False;
  Uglobals.UpdateFlag := False;
  Uglobals.ResultsSaved := False;
  MainForm.SelectorButtonClick;
end;


procedure TMainForm.CreateTempFiles;
//-----------------------------------------------------------------------------
// Creates temporary files that begin with the letters 'swmm'.
//-----------------------------------------------------------------------------
begin
  Uglobals.TempInputFile  := Uutils.GetTempFile(Uglobals.TempDir, 'swmm');
  Uglobals.TempReportFile := Uutils.GetTempFile(Uglobals.TempDir, 'swmm');
  Uglobals.TempOutputFile := Uutils.GetTempFile(Uglobals.TempDir, 'swmm');
end;


procedure TMainForm.DeleteTempFiles;
//-----------------------------------------------------------------------------
// Deletes temporary files.
//-----------------------------------------------------------------------------
begin
  SysUtils.DeleteFile(Uglobals.TempInputFile);
  if not Uglobals.ResultsSaved then
  begin
    SysUtils.DeleteFile(Uglobals.TempReportFile);
    SysUtils.DeleteFile(Uglobals.TempOutputFile);
  end;
end;


procedure TMainForm.WMDropFiles(var Msg: TWMDropFiles);
//-----------------------------------------------------------------------------
// Opens a project file dragged from Explorer and dropped on main window.
//-----------------------------------------------------------------------------
var
  DropH: HDROP;               // drop handle
  FileNameLength: Integer;    // length of a dropped file name
  FileName: string;           // a dropped file name
begin
  inherited;

  // Store drop handle from the message
  DropH := Msg.Drop;
  try

    // Get length of file name
    FileNameLength := DragQueryFile(DropH, 0, nil, 0);

    // Create string large enough to store file
    // (Delphi allows for #0 terminating character automatically)
    SetLength(FileName, FileNameLength);

    // Get the file name
    DragQueryFile(DropH, 0, PChar(FileName), FileNameLength + 1);

    // Ask user to save current file
    Application.BringToFront;
    if SaveFileDlg(Self) <> mrCancel then
    begin
      // Open new input file
      ReadOnlyFlag := (HasAttr(FileName, faReadOnly));
      OpenFile(Self, Filename);
    end;
  finally
    // Tidy up - release the drop handle
    // don't use DropH again after this
    DragFinish(DropH);
  end;
  // Note we handled message
  Msg.Result := 0;
end;


//=============================================================================
//                     Form-Related Procedures
//=============================================================================

function TMainForm.FormExists(const Name: String): Boolean;
//-----------------------------------------------------------------------------
// Checks if form with given name already exists.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Screen.FormCount - 1 do
  begin
    if Screen.Forms[I].Name = Name then
    begin
      with Screen.Forms[I] do
      begin
        WindowState := wsNormal;
        Show;
        SetFocus;
      end;
      Result := True;
      Exit;
    end;
  end;
end;


procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
//-----------------------------------------------------------------------------
//  Form's KeyDown handler.
//-----------------------------------------------------------------------------
begin
  // Turn off adding objects when Escape is pressed
  if (Key = VK_ESCAPE) then SelectorButtonClick;
end;


procedure TMainForm.FormResize(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's Resize handler.
//-----------------------------------------------------------------------------
begin
  if Self.WindowState = wsMaximized then
    ItemsPanel.Height := (BrowserPageControl.ClientHeight div 2) +
                         (BrowserToolbar.Height + ItemsLabel.Height) div 2;
end;


procedure TMainForm.CreateReport(ReportSelection: TReportSelection);
//-----------------------------------------------------------------------------
// Creates a new graph or table form.
//-----------------------------------------------------------------------------
var
  GraphForm: TGraphForm;
  ProfilePlotForm: TProfilePlotForm;
  TableForm: TTableForm;
begin
  if ReportSelection.ReportType = TIMESERIESPLOT then
  begin
    GraphForm := TGraphForm.Create(self);
    try
      if GraphForm.CreateGraph(ReportSelection)
      then GraphForm.RefreshGraph
      else GraphForm.Close;
    finally
    end;
  end

  else if ReportSelection.ReportType = SCATTERPLOT then
  begin
    GraphForm := TGraphForm.Create(self);
    try
      if GraphForm.CreateGraph(ReportSelection)
      then GraphForm.RefreshGraph
      else GraphForm.Close;
    finally
    end;
  end

  else if ReportSelection.ReportType = PROFILEPLOT then
  begin
    ProfilePlotForm := TProfilePlotForm.Create(self);
    try
      if ProfilePlotForm.CreatePlot(ReportSelection)
      then ProfilePlotForm.Show
      else ProfilePlotForm.Close;
    finally
    end;
  end

  else if ReportSelection.ReportType in [TABLEBYVARIABLE, TABLEBYOBJECT] then
  begin
    TableForm := TTableForm.Create(self);
    try
      if TableForm.CreateTable(ReportSelection)
      then TableForm.Show
      else TableForm.Close;
    finally
    end;
  end;
  StatusBar.Refresh;
end;


procedure TMainForm.CloseForms;
//-----------------------------------------------------------------------------
// Closes all forms (except the MapForm).
//-----------------------------------------------------------------------------
var
  I : Integer;
begin

  // Close MDI child forms
  for I := MDIChildCount - 1 downto 0 do
  begin
    if (MDIChildren[I] is TMapForm) then continue;
    MDIChildren[I].Close;
  end;

  // Hide overview map if visible
  if MnuOVMap.Checked then
  begin
    MnuOVMap.Checked := False;
    OVMapForm.Hide;
  end;

  // Hide Map Query and Find forms if visible
  if Assigned(QueryForm) then with QueryForm do
  begin
    Close;
    Clear;
  end;
  if Assigned(FindForm) then with FindForm do
  begin
    Close;
    Clear;
  end;
  if Assigned(ReportingForm) then with ReportingForm do
  begin
    Close;
    Clear;
  end;

  if Assigned(ReportSelectForm) then ReportSelectForm.Close;
  if Assigned(StatsSelectForm) then StatsSelectForm.Close;
  if Assigned(BackdropDimensionsForm) then BackdropDimensionsForm.Close;

end;


procedure TMainForm.RefreshMapForm;
//-----------------------------------------------------------------------------
// Refreshes the MapForm after new project data is retreived.
//-----------------------------------------------------------------------------
begin
  Uoutput.SetSubcatchColors;
  Uoutput.SetNodeColors;
  Uoutput.SetLinkColors;
  MapForm.RedrawMap;
end;


procedure TMainForm.RefreshForms;
//-----------------------------------------------------------------------------
// Refreshes all open output display forms after new analysis is made.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  Application.ProcessMessages;
  LockWindowUpdate(Handle);
  for I := 0 to MDIChildCount - 1 do
  begin

    if (MDIChildren[I] is TStatusForm) then
      with MDIChildren[I] as TStatusForm do RefreshStatusReport

    else if (MDIChildren[I] is TResultsForm) then
      with MDIChildren[I] as TResultsForm do RefreshReport

    else if (MDIChildren[I] is TGraphForm) then
      with MDIChildren[I] as TGraphForm do RefreshGraph

    else if (MDIChildren[I] is TProfilePlotForm) then
      with MDIChildren[I] as TProfilePlotForm do RefreshPlot

    else if (MDIChildren[I] is TTableForm) then
      with MDIChildren[I] as TTableForm do RefreshTable

    else if (MDIChildren[I] is TStatsReportForm) then
      with MDIChildren[I] as TStatsReportForm do RefreshReport;
  end;
  LockWindowUpdate(0);
end;


procedure TMainForm.UpdateProfilePlots;
//-----------------------------------------------------------------------------
// Updates all profile plots when a new time period is selected.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  LockWindowUpdate(Handle);
  for I := MDIChildCount - 1 downto 0 do
  begin
   if MDIChildren[I] is TProfilePlotForm then
     with MDIChildren[I] as TProfilePlotForm do
       UpdatePlot(Uglobals.CurrentPeriod);
  end;
  LockWindowUpdate(0);
end;

//=============================================================================
//                       Printer Page Setup Procedures
//=============================================================================

procedure TMainForm.InitPageLayout;
//-----------------------------------------------------------------------------
// Initializes the printer's page layout.
//-----------------------------------------------------------------------------
begin
  with Uglobals.PageLayout do
  begin
    LMargin := 1.0;
    TMargin := 1.5;
    RMargin := 1.0;
    BMargin := 1.0;
  end;
  with PageSetupDialog do
  begin
    Header.Text := '';
    Header.Alignment := taCenter;
    Header.Enabled := True;
    Footer.Text := TXT_MAIN_CAPTION;
    Footer.Alignment := taLeftJustify;
    Footer.Enabled := True;
    PageNumbers := pnLowerRight;
  end;
  TitleAsHeader := True;
  Orientation := Ord(poPortrait);
end;


procedure TMainForm.PageSetup;
//-----------------------------------------------------------------------------
// Transfers current page margins & header/footer options to the
// Printer object.
//-----------------------------------------------------------------------------
var
  Y: Single;
  Justify: TJustify;
begin
  if Printer.Printers.Count > 0 then with thePrinter do
  begin
    // Set printer orientation
    SetOrientation(TPrinterOrientation(Orientation));

    // Set page margins
    with PageLayout do
      SetMargins(TMargin,BMargin,LMargin,RMargin);

    with PageSetupDialog do
    begin
      // Define header line (0.5 inches above top margin)
      Justify := TJustify(Ord(Header.Alignment));
      SetHeaderInformation(1,PageLayout.TMargin-0.5,Header.Text,Justify,
        'Arial',14,[fsBold]);
      SetHeaders(Header.Enabled);

      // Define footer line (0.5 inches from bottom of page)
      Justify := TJustify(Ord(Footer.Alignment));
      SetFooterInformation(1,GetPageHeight-0.5,Footer.Text,Justify,
        'Arial',10,[fsBold, fsItalic]);
      SetFooters(Footer.Enabled);

      // Set page number location
      Justify := jRight;
      if PageNumbers in [pnUpperLeft, pnLowerLeft] then Justify := jLeft;
      if PageNumbers in [pnUpperCenter, pnLowerCenter] then Justify := jCenter;
      Y := 0.5;
      if PageNumbers in [pnLowerLeft, pnLowerCenter, pnLowerRight] then
        Y := GetPageHeight-0.5;
      SetPageNumberInformation(Y,'Page ',Justify,'Arial',10,[]);
      SetPageNumbers(not (PageNumbers = pnNone));
    end;
  end;
end;


//=============================================================================
//                        Progress Bar Procedures
//=============================================================================

procedure TMainForm.ShowStatusHint(const Msg: String);
begin
  if Length(Msg) = 0 then
  begin
    StatusHint.Visible := False;
    StatusHintSep.Visible := False;
  end
  else
  begin
    StatusHint.Caption := Msg;
    StatusHint.Visible := True;
    StatusHintSep.Visible := True;
  end;
end;

procedure TMainForm.ShowProgressBar(const Msg: String);
//-----------------------------------------------------------------------------
// Activates the ProgressBar by hiding the StatusPanel panel and making
// the ProgressPanel panel visible.
//-----------------------------------------------------------------------------
var
  W: Integer;
begin
  // Display a message on the ProgressBar's panel
  W := Canvas.TextWidth(Msg) + 2;
  ProgressPanel.Caption := Msg;

  // Position the ProgressBar to the right of the message
  ProgressBar.Left := W;
  ProgressBar.Position := 0;

  // Switch the visibility of the StatusPanel and the ProgressPanel panels
  StatusBar.Visible := False;
  ProgressPanel.Visible := True;
  ProgressPanel.Refresh;
end;


procedure TMainForm.HideProgressBar;
//-----------------------------------------------------------------------------
// Hides the ProgressBar by switching the visibility of the ProgressPanel
// panel and the StatusPanel panel.
//-----------------------------------------------------------------------------
begin
  ProgressBar.Visible := False;
  ProgressPanel.Visible := False;
  StatusBar.Visible := True;
  //StatusBar.Refresh;
end;


procedure TMainForm.UpdateProgressBar(var Count: Integer;
  const StepSize: Integer);
//-----------------------------------------------------------------------------
// Updates the display of the ProgressBar's meter.
//-----------------------------------------------------------------------------
begin
  Inc(Count);
  if Count >= StepSize then
  begin
    Application.ProcessMessages;
    Count := 0;
    ProgressBar.Visible := True;
    ProgressBar.StepIt;
  end;
end;


//=============================================================================
//                          Status Bar Procedures
//=============================================================================

procedure TMainForm.AutoLengthOnMnuClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the 'On' menu choice for the Auto-Length button
//-----------------------------------------------------------------------------
begin
  AutoLength := True;
  AutoLengthBtn.Caption := TXT_AUTO_LENGTH_ON;
end;


procedure TMainForm.AutoLengthOffMnuClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the 'Off' menu choice for the Auto-Length button
//-----------------------------------------------------------------------------
begin
  AutoLength := False;
  AutoLengthBtn.Caption := TXT_AUTO_LENGTH_OFF;
end;


procedure TMainForm.FlowUnitsMnuItemClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the menu attached to the Flow Units button
//-----------------------------------------------------------------------------
begin
  with Sender as TMenuItem do
  begin
    if not SameText(Caption, Project.Options.Data[FLOW_UNITS_INDEX]) then
    begin
      Project.Options.Data[FLOW_UNITS_INDEX] := Caption;
      Uupdate.UpdateUnits;
      SetChangeFlags;
    end;
  end;
end;


procedure TMainForm.OffsetsMnuItemClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the menu attached to the Offsets button
//-----------------------------------------------------------------------------
begin
  with Sender as TMenuItem do
  begin
    if Pos(Project.Options.Data[LINK_OFFSETS_INDEX], Caption) = 0 then
    begin
      OffsetsBtn.Caption := Caption;
      Project.Options.Data[LINK_OFFSETS_INDEX] := LinkOffsetsOptions[Tag];
      SetChangeFlags;
      Uupdate.UpdateDefOptions;
      Uupdate.UpdateLinkHints;
      if PropEditForm.Visible then PropEditForm.RefreshPropertyHint;
      Uupdate.UpdateOffsets;
    end;
  end;
end;


procedure TMainForm.ShowRunStatus;
//-----------------------------------------------------------------------------
// Displays the analysis success or failure icon in the StatusPanel.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  if RunStatus = rsNone then I := 0
  else if RunStatus in [rsSuccess, rsWarning] then
  begin
    if Uglobals.UpdateFlag then I := 2
    else I := 1;
  end
  else I := 3;
  RunStatusButton.ImageIndex := I;
  RunStatusButton.Hint := RunStatusHint[I];
  StatusBar.Refresh;
end;


procedure TMainForm.SetChangeFlags;
//-----------------------------------------------------------------------------
// Updates change flags after a change is made to the project.
// HasChanged: True if database has changed.
// UpdateFlag: True if analysis results need updating.
//-----------------------------------------------------------------------------
begin
  Uglobals.HasChanged := True;
  if Uglobals.RunFlag and not Uglobals.UpdateFlag then
  begin
    Uglobals.UpdateFlag := True;
    ShowRunStatus;
  end;
end;


//=============================================================================
//                    Procedures for Running a Simulation
//=============================================================================

procedure Execute;
//-----------------------------------------------------------------------------
// Executes the command line (console) version of the SWMM engine.
// (Not currently used.)
//-----------------------------------------------------------------------------
var
  OldDir: String;
  CmdLine: String;
  S: TStringlist;
begin
  GetDir(0, OldDir);
  ChDir(TempDir);
  S := TStringlist.Create;
  try
    Uexport.ExportProject(S, '');
    Uexport.ExportTempDir(S);
    S.SaveToFile(TempInputFile);
  finally
    S.Free;
  end;
  CmdLine := EpaSwmmDir + 'swmm5.exe ' +
             ExtractFileName(TempInputFile) + ' ' +
             ExtractFileName(TempReportFile) + ' ' +
             ExtractFileName(TempOutputFile);
  Uutils.WinExecAndWait(CmdLine, '', SW_SHOWNORMAL, false);
  if GetFileSize(Uglobals.TempReportFile) <= 0
  then Uglobals.RunStatus := rsFailed
  else Uglobals.RunStatus := Uoutput.CheckRunStatus(Uglobals.TempOutputFile);
  if not (Uglobals.RunStatus in [rsSuccess, rsWarning])
  then Uutils.MsgDlg('Run was unsuccessful.', mtInformation, [mbOK]);
  ChDir(OldDir);
end;


procedure TMainForm.RunSimulation;
//-----------------------------------------------------------------------------
// Uses the external SWMM engine to run a simulation.
//-----------------------------------------------------------------------------
var
  I: Integer;

begin
  // Clear all previous results
  Uoutput.ClearOutput;
  Ubrowser.InitMapPage;
  Uglobals.RunStatus := rsNone;

  // Clear contents of Status Report (so file can be deleted).
  for I := 0 to MDIChildCount - 1 do
  begin
    if (MDIChildren[I] is TStatusForm) then
      with MDIChildren[I] as TStatusForm do ClearReport;
  end;

  // Create a new set of temporary files
  Uglobals.ResultsSaved := False;
  DeleteTempFiles;
  CreateTempFiles;

  // Call the command line version of the engine (not currently used).
  //Execute;

  // Display the Simulation dialog form (which will call the DLL
  // version of the engine)
  with TSimulationForm.Create(self) do
  try
    ShowModal;
  finally
    Free;
  end;

  // Delete temporary files if the run ended prematurely
  if (Uglobals.RunStatus = rsShutdown) then DeleteTempFiles;

  // Set RunFlag if the run produced results
  if Uglobals.RunStatus in [rsSuccess, rsWarning]
  then Uglobals.RunFlag := True
  else Uglobals.RunFlag := False;
  ShowRunStatus;

  // Display the Status Report if the run produced errors
  if Uglobals.RunStatus = rsError then MnuReportStatusClick(Self);

  // Refresh output results and any existing reporting forms
  RefreshResults;
  RefreshForms;
end;


procedure TMainForm.RefreshResults;
//-----------------------------------------------------------------------------
// Prepares the system to display the results of a simulation run.
//-----------------------------------------------------------------------------
begin
  // Enable reporting toolbar buttons
  EnableReportButtons(Uglobals.RunFlag);

  // Do following if output results exist
  if Uglobals.RunFlag then
  begin

    // Read prolog portion of binary results file
    Uoutput.GetBasicOutput;

    // Update Browser panel's map page and the MapForm's display
    Ubrowser.UpdateMapPage;
    Ubrowser.RefreshMapColors;
    Ubrowser.RefreshMap;
  end

  // Otherwise re-set map display themes to none
  // (xxxOUTVAR1 is index of the first map display variable that
  // comes from simulation results).
  else
  begin
    if   Uglobals.CurrentSubcatchVar >= SUBCATCHOUTVAR1
    then Uglobals.CurrentSubcatchVar := NOVIEW;
    if   Uglobals.CurrentNodeVar >= NODEOUTVAR1
    then Uglobals.CurrentNodeVar := NOVIEW;
    if   Uglobals.CurrentLinkVar >= LINKOUTVAR1
    then Uglobals.CurrentLinkVar := NOVIEW;
    if Assigned(QueryForm) then
    begin
      QueryForm.UpdateVariables;
      QueryForm.Clear;
    end;
    MapForm.RedrawMap;
  end;

// Refresh map legends
  MapForm.DrawSubcatchLegend;
  MapForm.DrawNodeLegend;
  MapForm.DrawLinkLegend;
end;


//-----------------------------------------------------------------------------
//  Application event handlers that prevents problems when the minimized
//  form is restored when using a non-Windows style theme.
//-----------------------------------------------------------------------------

procedure TMainForm.ApplicationEvents1Minimize(Sender: TObject);
begin
  if Assigned(TStyleManager.ActiveStyle) and
    (TStyleManager.ActiveStyle.Name <> 'Windows') then
  begin
    ToolBar1.AutoSize := False;
    MapForm.MapToolBar.AutoSize := False;
  end;
end;

procedure TMainForm.ApplicationEvents1Restore(Sender: TObject);
begin
  if Assigned(TStyleManager.ActiveStyle) and
    (TStyleManager.ActiveStyle.Name <> 'Windows') then
  begin
    ToolBar1.AutoSize := True;
    MapForm.MapToolBar.AutoSize := True;
  end;
end;

//=============================================================================
//                     Help System Procedures
//=============================================================================

procedure TMainForm.MnuWelcomeScreenClick(Sender: TObject);
//-----------------------------------------------------------------------------
// Shows SWMM's Welcome screen when Help | Welcome Screen menu item selected.
//-----------------------------------------------------------------------------
begin
  // Show the Welcome screen and obtain user's choice of action to take
  Uglobals.GetStartupAction;

  // Take the user's choice of action
  if StartupAction = saShowTutorial then
     MnuIntroTutorialClick(Sender)

  else if StartupAction = saShowUsersGuide then
     MnuHelpTopicsClick(Sender)

  else
  begin
    if SaveFileDlg(Sender) = mrCancel then Exit;
    case StartupAction of
      saNewProject:     MnuNewClick(Sender);
      saOpenProject:    MnuOpenClick(Sender);
      saLoadRecent:     OpenFile(Sender, StartupFile);
      saLoadSample:
      begin
        OpenFile(Sender, StartupFile);
        ReadOnlyFlag := True;
        ShowProjectInfo(StartupFile);
      end;
    end;
  end;
end;

function TMainForm.ApplicationHelp(Command: Word; Data: THelpEventData;
                             var CallHelp: Boolean): Boolean;
begin
  CallHelp := False;
  Result := True;
  case Command of
  HELP_CONTEXT,HELP_CONTEXTPOPUP:
    HtmlHelp(GetDesktopWindow, Application.HelpFile, HH_HELP_CONTEXT, Data);
  HELP_FINDER:
    HtmlHelp(GetDesktopWIndow, Application.HelpFile, HH_HELP_FINDER, Data);
  end;
end;

procedure TMainForm.MnuHelpTopicsClick(Sender: TObject);
begin
  HtmlHelp(GetDesktopWindow, Application.HelpFile, HH_DISPLAY_TOC, 0);
end;

procedure TMainForm.MnuHelpUnitsClick(Sender: TObject);
begin
  if Uglobals.UnitSystem = usUS then
    Application.HelpCommand(HELP_CONTEXT, 211800)
  else
    Application.HelpCommand(HELP_CONTEXT, 211810);
end;

procedure TMainForm.MnuHelpErrorsClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 212990);
end;

procedure TMainForm.MnuHelpShortcutsClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 213720);
end;

procedure TMainForm.MnuInletsTutorialClick(Sender: TObject);
begin
  HtmlHelp(GetDesktopWindow, EpaSwmmDir + INLETSTUTORFILE, HH_DISPLAY_TOC, 0);
end;

procedure TMainForm.MnuIntroTutorialClick(Sender: TObject);
begin
  HtmlHelp(GetDesktopWindow, EpaSwmmDir + BASICTUTORFILE, HH_DISPLAY_TOC, 0);
end;

procedure TMainForm.MnuHowdoIClick(Sender: TObject);
begin
  HtmlHelp(GetDesktopWindow, Application.HelpFile, HH_DISPLAY_INDEX,
    DWORD(PWideChar(TXT_HOW_DO_I)));
end;

procedure TMainForm.MnuWhatsNewClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 213560);
end;

end.
