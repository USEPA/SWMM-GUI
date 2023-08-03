unit Uproject;

{-------------------------------------------------------------------}
{                    Unit:    Uproject.pas                          }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21   (5.2.0)                    }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Delphi Pascal unit that defines the TProject class used to      }
{   represent a project's data objects and their properties.        }
{-------------------------------------------------------------------}

interface

uses
  SysUtils, Windows, Forms, Messages, Classes, Graphics,
  Controls, Dialogs, Math, System.UITypes, Propedit, Uutils, Uvertex;

const
  MISSING          = -1.0e10;  //Missing value
  MAXGAGEPROPS     = 15;       //Max. index for raingage data array
  MAXSUBCATCHPROPS = 27;       //Max. index for subcatch data array
  MAXNODEPROPS     = 21;       //Max. index for node data array
  MAXLINKPROPS     = 24;       //Max. index for link data array
  MAXINFILPROPS    = 5;        //Max. index for infil. data array
  MAXPOLLUTPROPS   = 9;        //Max. index for pollutant data array
  MAXLANDUSEPROPS  = 3;        //Max. index for landuse data array
  MAXAQUIFERPROPS  = 12;       //Max. index for aquifer data array
  MAXBUPROPS       = 4;        //Max. index for pollutant buildup data array
  MAXWOPROPS       = 4;        //Max. index for pollutant washoff data array
  MAXOPTIONS       = 42;       //Max. index for options array
  MAXPROPS         = 42;       //Max. index of all data arrays; must be as
                               //large as largest of all preceeding constants
  MAXCLASS         = 34;       //Max. index for object classes

//----------------------
// Object category codes
//----------------------
  NOTES        = 0;
  OPTION       = 1;
  RAINGAGE     = 2;
  SUBCATCH     = 3;
  JUNCTION     = 4;
  OUTFALL      = 5;
  DIVIDER      = 6;
  STORAGE      = 7;
  CONDUIT      = 8;
  PUMP         = 9;
  ORIFICE      = 10;
  WEIR         = 11;
  OUTLET       = 12;
  MAPLABEL     = 13;
  CONTROLCURVE   = 14;
  DIVERSIONCURVE = 15;
  PUMPCURVE      = 16;
  RATINGCURVE    = 17;
  SHAPECURVE     = 18;
  STORAGECURVE   = 19;
  TIDALCURVE     = 20;
  WEIRCURVE      = 21;
  TIMESERIES     = 22;
  PATTERN        = 23;
  TRANSECT       = 24;
  HYDROGRAPH     = 25;
  POLLUTANT      = 26;
  LANDUSE        = 27;
  AQUIFER        = 28;
  CONTROL        = 29;
  CLIMATE        = 30;
  SNOWPACK       = 31;
  LID            = 32;
  STREET         = 33;
  INLET          = 34;

//---------------------------
// Climatology category codes
//---------------------------
  TEMPERATURE    = 0;
  EVAPORATION    = 1;
  WINDSPEED      = 2;
  SNOWMELT       = 3;

//----------------------
// Shared property codes
//----------------------
  ID_INDEX            = 0;  //ID index
  X_INDEX             = 1;  //Node's X-coord.
  Y_INDEX             = 2;  //Node's Y-coord.
  UP_INDEX            = 1;  //Link's upstream node
  DN_INDEX            = 2;  //Link's downstream node
  COMMENT_INDEX       = 3;  //Comment index
  TAG_INDEX           = 4;  //Tag index
  PROP_INDEX_OFFSET   = 4;  //Offset in Property Editor

//-------------------------
// Rain gage property codes
//-------------------------
  GAGE_DATA_FORMAT       = 5;
  GAGE_DATA_FREQ         = 6;
  GAGE_SNOW_CATCH        = 7;
  GAGE_DATA_SOURCE       = 8;
  GAGE_TIME_SERIES       = 9;
  GAGE_SERIES_NAME       = 10;
  GAGE_DATA_FILE         = 11;
  GAGE_FILE_NAME         = 12;
  GAGE_STATION_NUM       = 13;
  GAGE_RAIN_UNITS        = 14;
  GAGE_FILE_PATH         = 15;

//----------------------------
// Subcatchment property codes
//----------------------------
  SUBCATCH_RAINGAGE_INDEX    = 5;
  SUBCATCH_OUTLET_INDEX      = 6;
  SUBCATCH_AREA_INDEX        = 7;
  SUBCATCH_WIDTH_INDEX       = 8;
  SUBCATCH_SLOPE_INDEX       = 9;
  SUBCATCH_IMPERV_INDEX      = 10;
  SUBCATCH_IMPERV_N_INDEX    = 11;
  SUBCATCH_PERV_N_INDEX      = 12;
  SUBCATCH_IMPERV_DS_INDEX   = 13;
  SUBCATCH_PERV_DS_INDEX     = 14;
  SUBCATCH_PCTZERO_INDEX     = 15;
  SUBCATCH_ROUTE_TO_INDEX    = 16;
  SUBCATCH_PCT_ROUTED_INDEX  = 17;
  SUBCATCH_INFIL_INDEX       = 18;
  SUBCATCH_GWATER_INDEX      = 19;
  SUBCATCH_SNOWPACK_INDEX    = 20;
  SUBCATCH_LID_INDEX         = 21;
  SUBCATCH_LANDUSE_INDEX     = 22;
  SUBCATCH_LOADING_INDEX     = 23;
  SUBCATCH_CURBLENGTH_INDEX  = 24;
  SUBCATCH_N_PERV_PAT_INDEX  = 25;
  SUBCATCH_DS_PAT_INDEX      = 26;
  SUBCATCH_INFIL_PAT_INDEX   = 27;

//---------------------------
// Groundwater property codes
//---------------------------
//  MAX_GW_PROPS = 10;
//  GW_EQN_INDEX = 10;

//---------------------------
// Shared node property codes
//---------------------------
  NODE_INFLOWS_INDEX     = 5;  //External inflow
  NODE_TREAT_INDEX       = 6;  //Treatment
  NODE_INVERT_INDEX      = 7;  //Invert

//------------------------
// Junction property codes
//------------------------
  JUNCTION_MAX_DEPTH_INDEX       = 8;
  JUNCTION_INIT_DEPTH_INDEX      = 9;
  JUNCTION_SURCHARGE_DEPTH_INDEX = 10;
  JUNCTION_PONDED_AREA_INDEX     = 11;

//-----------------------
// Outfall property codes
//-----------------------
  OUTFALL_TIDE_GATE_INDEX     = 8;
  OUTFALL_ROUTETO_INDEX       = 9;
  OUTFALL_TYPE_INDEX          = 10;
  OUTFALL_FIXED_STAGE_HEADING = 11;
  OUTFALL_FIXED_STAGE_INDEX   = 12;
  OUTFALL_TIDAL_HEADING       = 13;
  OUTFALL_TIDE_TABLE_INDEX    = 14;
  OUTFALL_TIME_SERIES_HEADING = 15;
  OUTFALL_TIME_SERIES_INDEX   = 16;

//-----------------------
// Divider property codes
//-----------------------
  DIVIDER_MAX_DEPTH_INDEX       = 8;
  DIVIDER_INIT_DEPTH_INDEX      = 9;
  DIVIDER_SURCHARGE_DEPTH_INDEX = 10;
  DIVIDER_PONDED_AREA_INDEX     = 11;
  DIVIDER_LINK_INDEX            = 12;
  DIVIDER_TYPE_INDEX            = 13;
  DIVIDER_CUTOFF_HEADING        = 14;
  DIVIDER_CUTOFF_INDEX          = 15;
  DIVIDER_TABULAR_HEADING       = 16;
  DIVIDER_TABLE_INDEX           = 17;
  DIVIDER_WEIR_HEADING          = 18;
  DIVIDER_QMIN_INDEX            = 19;
  DIVIDER_DMAX_INDEX            = 20;
  DIVIDER_QCOEFF_INDEX          = 21;

//----------------------------
// Storage unit property codes
//----------------------------
  STORAGE_MAX_DEPTH_INDEX    = 8;
  STORAGE_INIT_DEPTH_INDEX   = 9;
  STORAGE_SURCHARGE_DEPTH_INDEX  = 10;
  STORAGE_EVAP_FACTOR_INDEX  = 11;
  STORAGE_SEEPAGE_INDEX      = 12;
  STORAGE_GEOMETRY_INDEX     = 13;
  STORAGE_COEFF0_INDEX       = 14;
  STORAGE_COEFF1_INDEX       = 15;
  STORAGE_COEFF2_INDEX       = 16;
  STORAGE_ATABLE_INDEX       = 17;

  STORAGE_SUCTION_INDEX      = 0;
  STORAGE_KSAT_INDEX         = 1;
  STORAGE_IMDMAX_INDEX       = 2;

//-----------------------
// Conduit property codes
//-----------------------
  CONDUIT_SHAPE_INDEX        = 5;
  CONDUIT_GEOM1_INDEX        = 6;
  CONDUIT_LENGTH_INDEX       = 7;
  CONDUIT_ROUGHNESS_INDEX    = 8;
  CONDUIT_INLET_HT_INDEX     = 9;
  CONDUIT_OUTLET_HT_INDEX    = 10;
  CONDUIT_INIT_FLOW_INDEX    = 11;
  CONDUIT_MAX_FLOW_INDEX     = 12;
  CONDUIT_ENTRY_LOSS_INDEX   = 13;
  CONDUIT_EXIT_LOSS_INDEX    = 14;
  CONDUIT_AVG_LOSS_INDEX     = 15;
  CONDUIT_SEEPAGE_INDEX      = 16;
  CONDUIT_CHECK_VALVE_INDEX  = 17;
  CONDUIT_CULVERT_INDEX      = 18;
  CONDUIT_INLET_INDEX        = 19;
  CONDUIT_GEOM2_INDEX        = 20;
  CONDUIT_GEOM3_INDEX        = 21;
  CONDUIT_GEOM4_INDEX        = 22;
  CONDUIT_BARRELS_INDEX      = 23;
  CONDUIT_TSECT_INDEX        = 24;
  CONDUIT_SLOPE_INDEX        = 120;

//--------------------
// Pump property codes
//--------------------
  PUMP_CURVE_INDEX        = 5;
  PUMP_STATUS_INDEX       = 6;
  PUMP_STARTUP_INDEX      = 7;
  PUMP_SHUTOFF_INDEX      = 8;

//-----------------------
// Orifice property codes
//-----------------------
  ORIFICE_TYPE_INDEX      = 5;
  ORIFICE_SHAPE_INDEX     = 6;
  ORIFICE_HEIGHT_INDEX    = 7;
  ORIFICE_WIDTH_INDEX     = 8;
  ORIFICE_BOTTOM_HT_INDEX = 9;
  ORIFICE_COEFF_INDEX     = 10;
  ORIFICE_FLAPGATE_INDEX  = 11;
  ORIFICE_ORATE_INDEX     = 12;

//--------------------
// Weir property codes
//--------------------
  WEIR_TYPE_INDEX      = 5;
  WEIR_HEIGHT_INDEX    = 6;
  WEIR_WIDTH_INDEX     = 7;
  WEIR_SLOPE_INDEX     = 8;
  WEIR_CREST_INDEX     = 9;
  WEIR_COEFF_INDEX     = 10;
  WEIR_FLAPGATE_INDEX  = 11;
  WEIR_CONTRACT_INDEX  = 12;
  WEIR_END_COEFF_INDEX = 13;
  WEIR_SURCHARGE_INDEX = 14;
  WEIR_COEFF_CURVE_INDEX = 15;
  WEIR_ROADWAY_HEADING  = 16;
  WEIR_ROAD_WIDTH_INDEX = 17;
  WEIR_ROAD_SURF_INDEX  = 18;
  WEIR_SHAPE_INDEX     = 19;

//----------------------
// Outlet property codes
//----------------------
  OUTLET_CREST_INDEX        = 5;
  OUTLET_FLAPGATE_INDEX     = 6;
  OUTLET_TYPE_INDEX         = 7;
  OUTLET_FUNCTIONAL_HEADING = 8;
  OUTLET_QCOEFF_INDEX       = 9;
  OUTLET_QEXPON_INDEX       = 10;
  OUTLET_TABULAR_HEADING    = 11;
  OUTLET_QTABLE_INDEX       = 12;

//-------------------------
// Pollutant property codes
//-------------------------
  POLLUT_UNITS_INDEX    = 0;
  POLLUT_RAIN_INDEX     = 1;
  POLLUT_GW_INDEX       = 2;
  POLLUT_II_INDEX       = 3;
  POLLUT_DWF_INDEX      = 4;
  POLLUT_INIT_INDEX     = 5;
  POLLUT_DECAY_INDEX    = 6;
  POLLUT_SNOW_INDEX     = 7;
  POLLUT_COPOLLUT_INDEX = 8;
  POLLUT_FRACTION_INDEX = 9;

//------------------------
// Land use property codes
//------------------------
  LANDUSE_CLEANING_INDEX  = 0;
  LANDUSE_AVAILABLE_INDEX = 1;
  LANDUSE_LASTCLEAN_INDEX = 2;

//-----------------------
// Temperature data types
//-----------------------
  NO_TEMP      = 0;
  TSERIES_TEMP = 1;
  FILE_TEMP    = 2;

//-----------------------
// Evaporation data types
//-----------------------
  CONSTANT_EVAP = 0;
  TSERIES_EVAP  = 1;
  FILE_EVAP     = 2;
  MONTHLY_EVAP  = 3;
  TEMP_EVAP     = 4;
  EVAP_DRY_ONLY = 5;
  RECOVERY      = 6;

//----------------------
// Wind speed data types
//----------------------
  FILE_WINDSPEED    = 0;
  MONTHLY_WINDSPEED = 1;

//-------------------------
// Infiltration model types
//-------------------------
  HORTON_INFIL         = 0;
  MOD_HORTON_INFIL     = 1;
  GREEN_AMPT_INFIL     = 2;
  MOD_GREEN_AMPT_INFIL = 3;
  CURVE_NUMBER_INFIL   = 4;

//------------------------
// Time Pattern type codes
//------------------------
  PATTERN_MONTHLY  = 0;
  PATTERN_DAILY    = 1;
  PATTERN_HOURLY   = 2;
  PATTERN_WEEKEND  = 3;

//-----------------
// Table type codes
//-----------------
  TABLE_STORAGE  = 0;
  TABLE_DIVIDER  = 1;
  TABLE_TIDAL    = 2;
  TABLE_PUMP     = 3;
  TABLE_OUTLET   = 4;

//------------------------
// Transect property codes
//------------------------
  TRANSECT_N_LEFT    = 0;
  TRANSECT_N_RIGHT   = 1;
  TRANSECT_N_CHANNEL = 2;
  TRANSECT_X_LEFT    = 3;
  TRANSECT_X_RIGHT   = 4;
  TRANSECT_X_FACTOR  = 5;
  TRANSECT_Y_FACTOR  = 6;
  TRANSECT_L_FACTOR  = 7;
  TRANSECT_MAX_DEPTH = 8;
  MAXTRANSECTPROPS   = 8;

//-----------------------------
// Street Section property code
//-----------------------------
  STREET_CROWN_WIDTH    = 0;
  STREET_CURB_HEIGHT    = 1;
  STREET_CROSS_SLOPE    = 2;
  STREET_ROUGHNESS      = 3;
  STREET_DEPRESSION     = 4;
  STREET_GUTTER_WIDTH   = 5;
  STREET_SIDES          = 6;
  STREET_BACK_WIDTH     = 7;
  STREET_BACK_SLOPE     = 8;
  STREET_BACK_ROUGHNESS = 9;
  MAXSTREETPROPS        = 9;

//-------------------------
// Map Label property codes
//-------------------------
  LABEL_TEXT_INDEX    = 0;
  ANCHOR_NODE_INDEX   = 3;
  METER_TYPE_INDEX    = 4;
  METER_ID_INDEX      = 5;

//---------------------------
// Analysis option type codes
//---------------------------
  COMPATIBILITY_INDEX     = 0;
  REPORT_CONTROLS_INDEX   = 1;
  REPORT_INPUT_INDEX      = 2;

  FLOW_UNITS_INDEX        = 3;
  INFILTRATION_INDEX      = 4;
  ROUTING_MODEL_INDEX     = 5;
  LINK_OFFSETS_INDEX      = 6;
  MIN_SLOPE_INDEX         = 7;
  ALLOW_PONDING_INDEX     = 8;
  SKIP_STEADY_INDEX       = 9;

  IGNORE_RAINFALL_INDEX   = 10;
  IGNORE_RDII_INDEX       = 11;
  IGNORE_SNOWMELT_INDEX   = 12;
  IGNORE_GRNDWTR_INDEX    = 13;
  IGNORE_ROUTING_INDEX    = 14;
  IGNORE_QUALITY_INDEX    = 15;

  START_DATE_INDEX        = 16;
  START_TIME_INDEX        = 17;
  REPORT_START_DATE_INDEX = 18;
  REPORT_START_TIME_INDEX = 19;
  END_DATE_INDEX          = 20;
  END_TIME_INDEX          = 21;
  SWEEP_START_INDEX       = 22;
  SWEEP_END_INDEX         = 23;
  DRY_DAYS_INDEX          = 24;
  REPORT_STEP_INDEX       = 25;
  WET_STEP_INDEX          = 26;
  DRY_STEP_INDEX          = 27;
  ROUTING_STEP_INDEX      = 28;
  RULE_STEP_INDEX         = 29;

  INERTIAL_DAMPING_INDEX  = 30;
  NORMAL_FLOW_LTD_INDEX   = 31;
  FORCE_MAIN_EQN_INDEX    = 32;
  SURCHARGE_METHOD_INDEX  = 33;
  VARIABLE_STEP_INDEX     = 34;
  LENGTHEN_STEP_INDEX     = 35;
  MIN_SURFAREA_INDEX      = 36;
  MAX_TRIALS_INDEX        = 37;
  HEAD_TOL_INDEX          = 38;
  SYS_FLOW_TOL_INDEX      = 39;
  LAT_FLOW_TOL_INDEX      = 40;
  MIN_ROUTE_STEP_INDEX    = 41;
  NUM_THREADS_INDEX       = 42;

 
//---------------------
// Interface file codes
//---------------------
  AS_INPUT       = 0;
  AS_OUTPUT      = 1;
  RAINFALL_FILE  = 0;
  RUNOFF_FILE    = 1;
  HOTSTART_FILE  = 2;
  RDII_FILE      = 3;
  INFLOWS_FILE   = 4;
  OUTFLOWS_FILE  = 5;
  ROUTING_FILE   = 6;

{$I objprops.txt}  //File containing object property definitions

type

//------------------------
// X,Y coordinate position
//------------------------
  TSinglePoint = record
    X : Single;
    Y : Single;
  end;

  TExtendedPoint = record
    X : Extended;
    Y : Extended;
  end;

//------------------------
// Default property values
//------------------------
  TDefProp = record
    Data: array[0..MAXPROPS] of String;
  end;

//-------------------
// Subcatchment class
//-------------------
  TNode = class;
  TSubcatch = class(TObject)
    ID: PChar;                  // Pointer to ID label
    X, Y: Extended;             // Centroid coordinates
    OutNode: TNode;             // Outlet node
    OutSubcatch: TSubcatch;     // Outlet subcatchment
    Vlist: TVertexList;         // Vertices of bounding polygon
    OutFileIndex: Integer;      // Index for output file results
    RptFileIndex: Integer;      // Index for report file results
    ColorIndex: Integer;        // Index in array of map display colors
    Data: array[0..MAXSUBCATCHPROPS] of String;       // Property data
    InfilData: array[0..MAXINFILPROPS] of String; // Infiltration parameters
    Groundwater: TStringlist;   // Groundwater parameters
    GwLatFlowEqn: String;       // Custom GW lateral flow equation
    GwDeepFlowEqn: String;      // Custom GW deep flow equation
    LandUses: TStringlist;      // Land use data
    Loadings: TStringlist;      // Initial pollutant buildup
    LIDs: TStringlist;          // Group of LIDs
    constructor Create;
    destructor  Destroy; override;
    procedure   SetCentroid;
    function    GetArea: Extended;
    function    AddVertex: PVertex;
  end;

//-----------
// Node class
//-----------
  TNode = class(TObject)
    ID: PChar;                  // Pointer to ID label
    Ntype: Integer;             // Node type
    X, Y : Extended;            // X,Y map coordinates
    Vlist: TVertexList;         // Vertices of storage node polygon
    OutFileIndex: Integer;      // Index in array of computed results
    RptFileIndex: Integer;      // Index for report file results
    ColorIndex: Integer;        // Index in array of map display colors
    Data : array [0..MAXNODEPROPS] of String; // Node-specific data
    DWInflow: TStringlist;      // Dry weather inflow data
    DXInflow: TStringlist;      // Direct external inflow data
    IIInflow: TStringlist;      // I&I inflow data
    Treatment: TStringlist;     // Treatment expressions
    AdjList: TList;             // Link adjacency list
    InfilData: array [0..2] of String; //Green-Ampt infiltration data
    PathLen: Integer;           // Path length to node

    constructor Create;
    destructor  Destroy; override;
    procedure   SetCentroid;
    function    AddVertex: PVertex;
  end;

//-----------
// Link class
//-----------
  TLink = class(TObject)
    ID: PChar;                  // Pointer to ID label
    Ltype: Integer;             // Link type
    Node1,Node2: TNode;         // Start & end nodes
    Vlist: TVertexList;         // List of vertex points
    Inlet: TClass;              // conduit inlet (nil if not present)
    OutFileIndex: Integer;      // Index in array of computed results
    RptFileIndex: Integer;      // Index for report file results
    ColorIndex: Integer;        // Index in array of map display colors
    Marked: Boolean;
    Data : array [0..MAXLINKPROPS] of String; // Link-specific data
    constructor Create;
    destructor  Destroy; override;
    function    AddVertex: PVertex;
    procedure   ReverseNodes;
  end;

//----------------
// Rain gage class
//----------------
  TTimeSeries = class;
  TRaingage = class(TObject)
    ID: PChar;                  // Pointer to ID label
    X, Y: Extended;             // Map coordinates
    Data : array [0..MAXGAGEPROPS] of String;
  end;

//----------------
// Pollutant class
//----------------
  TPollutant = class(TObject)
    Data : array [0..MAXPOLLUTPROPS] of String;
  end;

//---------------
// Land use class
//---------------
  TLanduse = class(TObject)
    Data : array [0..MAXLANDUSEPROPS] of String; // Cleaning params.
    NonpointSources : TStringlist;               // Pollut buildup/washoff
    constructor Create;
    destructor  Destroy; override;
    procedure ClearNonpointSources;
  end;

//----------------------
// Nonpoint source class
//----------------------
  TNonpointSource = class(TObject)
    BuildupData : array [0..MAXBUPROPS] of String;
    WashoffData : array [0..MAXWOPROPS] of String;
  end;

//--------------
// Aquifer class
//--------------
  TAquifer = class(TObject)
    Data : array [0..MAXAQUIFERPROPS] of String;
  end;

//------------------
// Climatology class
//------------------
  TClimatology = class(TObject)
    TempDataSource: Integer;
    TempTseries   : String;
    TempFile      : String;
    TempStartDate : String;
    TempUnitsType : Integer;
    EvapType      : Integer;
    EvapTseries   : String;
    EvapData      : array[0..11] of String;
    PanData       : array[0..11] of String;
    RecoveryPat   : String;
    EvapDryOnly   : Boolean;
    WindType      : Integer;
    WindSpeed     : array[1..12] of String;
    SnowMelt      : array[1..6] of String;
    ADCurve       : array[1..2, 1..10] of String;
    TempAdjust    : array[0..11] of String;
    EvapAdjust    : array[0..11] of String;
    RainAdjust    : array[0..11] of String;
    CondAdjust    : array[0..11] of String;
    constructor Create;
    procedure Init;
  end;

//----------------------
// RDII Hydrograph class
//----------------------
  THydrograph = class(TObject)
    Raingage: String;
    Params:   array[0..12, 1..3, 1..3] of String;
    InitAbs:  array[0..12, 1..3, 1..3] of String;
    constructor Create;
  end;

//---------------
// Snowpack class
//---------------
  TSnowpack = class(TObject)
    FracPlowable: String;
    Data:    array[1..3, 1..7] of String;
    Plowing: array[1..7] of String;
    constructor Create;
  end;

//-------------------
// Time Pattern class
//-------------------
  TPattern = class(TObject)
    PatternType : Integer;
    Comment     : String;
    Count       : Integer;
    Data        : array [0..23] of String;
    constructor Create;
  end;

//------------------
// Time series class
//------------------
  TTimeseries = class(TObject)
    Comment : String;
    Filename: String;
    Dates   : TStringlist;
    Times   : TStringlist;
    Values  : TStringlist;
    constructor Create;
    destructor  Destroy; override;
  end;

//------------
// Curve class
//------------
  TCurve = class(TObject)
    Comment  : String;
    CurveType: String;
    CurveCode: Integer;
    Xdata    : TStringList;
    Ydata    : TStringList;
    constructor Create;
    destructor  Destroy; override;
  end;

//---------------------
// Street Section class
//---------------------
  TStreet = class(TObject)
    Data     : array[0..MAXSTREETPROPS] of String;
    MaxDepth : String;
    constructor Create;
    procedure   SetMaxDepth;
  end;

//---------------
// Transect class
//---------------
  TTransect = class(TObject)
    Comment  : String;
    Data     : array [0..MAXTRANSECTPROPS] of String;
    Xdata    : TStringList;
    Ydata    : TStringList;
    constructor Create;
    destructor  Destroy; override;
    procedure   SetMaxDepth;
    procedure   CheckData;
    procedure   SwitchData;
  end;

//----------------
// Map label class
//----------------
  TMapLabel = class(Tobject)
    Text      : PChar;       // Pointer to label text
    X         : Extended;    // X,Y coordinates
    Y         : Extended;
    Anchor    : TNode;       // Anchor node
    FontName  : String;      // Font properties
    FontSize  : Integer;
    FontBold  : Boolean;
    FontItalic: Boolean;
    constructor Create;
    procedure getFont(aFont: TFont);
  end;

//-----------------------
// Analysis options class
//-----------------------
  TOptions = class(TObject)
    Data  : array[0..MAXOPTIONS] of String;  // Analysis options
    Report: TStringlist;                     // Unused reporting options
    constructor  Create;
    destructor   Destroy; override;
  end;

//------------------------
// Project clipboard class
//------------------------
  TProjectClipboard = class
    ObjType    : Integer;
    Data       : TStringlist;
    List1      : TStringlist;
    List2      : TStringlist;
    Font       : TFont;
    constructor  Create;
    destructor   Destroy; override;
    procedure Clear;
  end;

//--------------
// Project class
//--------------
  TProject = class(TObject)
  public
    Title        : String;
    Lists        : array [0..MAXCLASS] of TStringList;
    DefProp      : array [0..MAXCLASS] of TDefProp;
    HasItems     : array [0..MAXCLASS] of Boolean;
    CurrentItem  : array [0..MAXCLASS] of Integer;
    NextID       : array [0..MAXCLASS] of Integer;
    IDPrefix     : array [0..MAXCLASS] of String;
    IDincrement  : Integer;
    PropList     : TStringList;       // Property list (for Property Editor)
    ProfileNames : TStringList;       // Names of saved profile plots
    ProfileLinks : TStringList;       // Link sequences of saved profile plots
    PollutNames  : TStringList;       // Pollutant names
    PollutUnits  : TStringList;       // Pollutant units
    ControlRules : TStringList;       // Pump/regulator controls
    IfaceFiles   : TStringList;       // Interface file specs
    Events       : TStringList;       // Routing event periods
    Options      : TOptions;          // Analysis options
    Climatology  : TClimatology;      // Climatological data
    Clipboard    : TProjectClipboard; // Project clipboard

    constructor Create;
    destructor  Destroy; override;

    procedure   Clear;
    procedure   ClearList(const ObjType: Integer);
    procedure   ClearMapData;

    procedure   DeleteItem(const ObjType: Integer; const Index: Integer);
    procedure   DeleteItemOnly(const ObjType: Integer; const Index: Integer);
    procedure   DeleteLabelAnchors(aNode: TNode);
    procedure   DeleteNonpointSource(const Pollut: String);
    procedure   DeleteSubcatchOutNode(aNode: TNode);
    procedure   DeleteSubcatchOutSubcatch(aSubcatch: TSubcatch);
    function    DupID(const ID: String; const Obj: Integer;
                const Indx: Integer): Boolean;

    function    FindCurve(const S: String; var Ctype: Integer;
                  var Index: Integer): Boolean;
    function    FindLink(const S: String; var Ltype: Integer;
                  var Index: Integer): Boolean;
    function    FindNode(const S: String; var Ntype: Integer;
                  var Index: Integer): Boolean;
    function    FindSubcatch(const S: String; var Atype: Integer;
                  var Index: Integer): Boolean;

    procedure   GetControlRuleNames;
    function    GetCurveCount: Integer;
    function    GetGage(const Index: Integer): TRaingage;
    function    GetID(const ObjType: Integer; const Index: Integer): String;
    function    GetLastNodes(var N1, N2: TNode): Boolean;
    function    GetLink(const Ltype: Integer; const Index: Integer): TLink;
    function    GetLinkCount: Integer;
    function    GetMapLabel(const Index: Integer): TMapLabel;
    function    GetNextID(const ObjType : Integer): String;
    function    GetNode(const Ntype: Integer; const Index: Integer):TNode; Overload;
    function    GetNode(const S: String): TNode; Overload;
    function    GetNodeCount: Integer;
    function    GetSubcatch(const Atype: Integer; const Index: Integer): TSubcatch;

    function    HasResults(const ObjType: Integer; const Index: Integer): Boolean;

    procedure   InitCurrentItems;
    function    IsCurve(const ObjType: Integer): Boolean;
    function    IsNode(const ObjType: Integer): Boolean;
    function    IsLink(const ObjType: Integer): Boolean;
    function    IsSortable(const ObjType: Integer): Boolean;
    function    IsSubcatch(const ObjType: Integer): Boolean;
    function    IsVisual(const ObjType: Integer): Boolean;

    procedure   SetStreetConduitDepth(const StreetName: string;
                const DepthStr: string);
    procedure   SetTransectConduitDepth(const TsectName: String;
                const DepthStr: String);
  private
    function    IDExists(const ObjType: Integer; const ID: string): Boolean;
  end;

var
  DefInfil : array [0..MAXINFILPROPS] of String;

implementation

uses
  Uupdate, Ulid, Uinlet;

//-----------------------------------------------------------------------------
// Project class methods
//-----------------------------------------------------------------------------

constructor TProject.Create;
var
  I: Integer;
begin
  inherited Create;
  for I := 0 to MAXCLASS do
  begin
    Lists[I] := TStringList.Create;
  end;
  PropList     := TStringList.Create;
  ProfileNames := TStringlist.Create;
  ProfileLinks := TStringlist.Create;
  PollutNames  := TStringlist.Create;
  PollutUnits  := TStringlist.Create;
  ControlRules := TStringlist.Create;
  IfaceFiles   := TStringlist.Create;
  Events       := TStringList.Create;
  Options      := TOptions.Create;
  Climatology  := TClimatology.Create;
  Clipboard    := TProjectClipboard.Create;
end;


destructor TProject.Destroy;
var
  I : Integer;
begin
  for I := 0 to MAXCLASS do
  begin
    Lists[I].Free;
  end;
  PropList.Free;
  ProfileNames.Free;
  ProfileLinks.Free;
  PollutNames.Free;
  PollutUnits.Free;
  ControlRules.Free;
  IfaceFiles.Free;
  Events.Free;
  Options.Free;
  Climatology.Free;
  Clipboard.Free;
  inherited Destroy;
end;


procedure TProject.DeleteItem(const ObjType: Integer; const Index: Integer);
//-----------------------------------------------------------------------------
//  Deletes a specific object from the project.
//-----------------------------------------------------------------------------
var
  S: String;
begin
  if (Lists[ObjType].Count > 0) then
  begin
    S := Lists[ObjType].Strings[Index];

    // For pollutants, free any associated buildup/washoff objects
    if ObjType = POLLUTANT
    then DeleteNonpointSource(Lists[POLLUTANT].Strings[Index]);

    // For inlet structures, remove their usage by conduits
    if ObjType = INLET
    then Uinlet.DeleteInletsByType(TInlet(Lists[ObjType].Objects[Index]));

    // Free the object & delete it from its object list
    Lists[ObjType].Objects[Index].Free;
    Lists[ObjType].Delete(Index);

    // Remove any references to object's name in other objects
    Uupdate.RemoveName(ObjType, S);

    // Update the CurrentItem pointer for the object class
    if Index >= Lists[ObjType].Count
    then CurrentItem[ObjType] := Lists[ObjType].Count - 1
    else CurrentItem[ObjType] := Index;

  end;
  if Lists[ObjType].Count = 0 then HasItems[ObjType] := False;
end;


procedure TProject.DeleteItemOnly(const ObjType: Integer; const Index: Integer);
//-----------------------------------------------------------------------------
//  Deletes a specific object from the project without removing references
//  to object's name in other objects
//-----------------------------------------------------------------------------
begin
  Lists[ObjType].Objects[Index].Free;
  Lists[ObjType].Delete(Index);
  if Index >= Lists[ObjType].Count
  then CurrentItem[ObjType] := Lists[ObjType].Count - 1
  else CurrentItem[ObjType] := Index;
  if Lists[ObjType].Count = 0 then HasItems[ObjType] := False;
end;


procedure TProject.DeleteLabelAnchors(aNode: TNode);
//-----------------------------------------------------------------------------
//  Deletes references to node aNode in the Anchor property of all map labels.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  for I := 0 to Lists[MAPLABEL].Count - 1 do
  begin
    with GetMapLabel(I) do
      if Anchor = aNode then Anchor := nil;
  end;
end;


procedure TProject.DeleteNonpointSource(const Pollut: String);
//-----------------------------------------------------------------------------
//  Deletes NonpointSource entries associated with pollutant Pollut
//  in all Landuse objects.
//-----------------------------------------------------------------------------
var
  I: Integer;
  J: Integer;
  aLanduse: TLanduse;
begin
  for I := 0 to Lists[LANDUSE].Count-1 do
  begin
    aLanduse := TLanduse(Lists[LANDUSE].Objects[I]);
    J := aLanduse.NonpointSources.IndexOf(Pollut);
    if J >= 0 then with aLanduse.NonpointSources do
    begin
      Objects[J].Free;
      Delete(J);
    end;
  end;
end;


procedure TProject.DeleteSubcatchOutNode(aNode: TNode);
//-----------------------------------------------------------------------------
//  Deletes references to node aNode in the OutNode property of all
//  subcatchments.
//-----------------------------------------------------------------------------
var
  I: Integer;
  S: TSubcatch;
begin
  for I := 0 to Lists[SUBCATCH].Count-1 do
  begin
    S := GetSubcatch(SUBCATCH, I);
    if S.OutNode = aNode then S.OutNode := nil;
  end;
end;


procedure TProject.DeleteSubcatchOutSubcatch(aSubcatch: TSubcatch);
//-----------------------------------------------------------------------------
//  Deletes references to subcatchment aSubcatch in the OutSubcatch
//  property of all subcatchments.
//-----------------------------------------------------------------------------
var
  I: Integer;
  S: TSubcatch;
begin
  for I := 0 to Lists[SUBCATCH].Count-1 do
  begin
    S := GetSubcatch(SUBCATCH, I);
    if S.OutSubcatch = aSubcatch then S.OutSubcatch := nil;
  end;
end;


procedure TProject.ClearMapData;
//-----------------------------------------------------------------------------
//  Clears the display of all objects on the study area map.
//-----------------------------------------------------------------------------
var
  I, J     : Integer;
  aGage    : TRaingage;
  aNode    : TNode;
  aLink    : TLink;
  aSubcatch: TSubcatch;
  aLabel   : TMapLabel;
begin
  for I := 0 to MAXCLASS do
  begin
    if I = RAINGAGE then
    begin
      for J := 0 to Lists[RAINGAGE].Count-1 do
      begin
        aGage := GetGage(J);
        aGage.X := MISSING;
        aGage.Y := MISSING;
      end;
    end
    else if IsSubcatch(I) then
    begin
      for J := 0 to Lists[I].Count-1 do
      begin
        aSubcatch := GetSubcatch(I, J);
        aSubcatch.X := MISSING;
        aSubcatch.Y := MISSING;
        aSubcatch.Vlist.Free;
        aSubcatch.Vlist := TVertexList.Create;
      end;
    end
    else if IsNode(I) then
    begin
      for J := 0 to Lists[I].Count-1 do
      begin
        aNode := GetNode(I,J);
        aNode.X := MISSING;
        aNode.Y := MISSING;
        aNode.Vlist.Free;
        aNode.Vlist := TVertexList.Create;
       end;
    end
    else if IsLink(I) then
    begin
      for J := 0 to Lists[I].Count-1 do
      begin
        aLink := GetLink(I, J);
        aLink.Vlist.Free;
        aLink.Vlist := TVertexList.Create;
      end;
    end
    else if I = MAPLABEL then
    begin
      for J := 0 to Lists[MAPLABEL].Count-1 do
      begin
        aLabel := GetMapLabel(J);
        aLabel.X := MISSING;
        aLabel.Y := MISSING;
      end;
    end;
  end;
end;


procedure TProject.ClearList(const ObjType: Integer);
//-----------------------------------------------------------------------------
//  Deletes all objects of a given type from the project.
//-----------------------------------------------------------------------------
var
  J: Integer;
begin
  with Lists[ObjType] do
  begin
    J := Count-1;
    while J >= 0 do
    begin
     Objects[J].Free;
     Delete(J);
     Dec(J);
    end;
  end;
end;


procedure TProject.Clear;
//-----------------------------------------------------------------------------
//  Deletes all objects from the project.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  // Delete links first ( otherwise their end nodes will point nowhere)
  for I := 0 to MAXCLASS do
    if IsLink(I) then ClearList(I);

  // Then delete all other objects
  for I := 0 to MAXCLASS do
    if not IsLink(I) then ClearList(I);

  // Reset the current item and next ID number for each class of object
  for I := 0 to MAXCLASS do
  begin
    CurrentItem[I] := -1;
    NextID[I] := IDIncrement;
    HasItems[I] := False;
  end;

  // Clear all remaining project data
  Title := '';
  ControlRules.Clear;
  ProfileNames.Clear;
  ProfileLinks.Clear;
  IfaceFiles.Clear;
  Events.Clear;
  Options.Report.Clear;
end;


procedure TProject.GetControlRuleNames;
//-----------------------------------------------------------------------------
//  Places the name of each control rule in the Lists[CONTROL] stringlist.
//-----------------------------------------------------------------------------
var
  I: Integer;
  K: Integer;
  S: String;
begin
  K := CurrentItem[CONTROL];
  Lists[CONTROL].Clear;
  for I := 0 to ControlRules.Count-1 do
  begin
    S := Trim(ControlRules[I]);
    if Pos('RULE', UpperCase(S)) = 1 then
    begin
      Delete(S, 1, 4);
      Lists[CONTROL].Add(Trim(S));
    end;
  end;
  if Lists[CONTROL].Count = 0 then CurrentItem[CONTROL] := -1
  else if K > Lists[CONTROL].Count - 1 then
    CurrentItem[CONTROL] := Lists[CONTROL].Count - 1
  else CurrentItem[CONTROL] := K;
  HasItems[CONTROL] := (Lists[CONTROL].Count > 0);
end;


function TProject.GetCurveCount: Integer;
//-----------------------------------------------------------------------------
//  Counts the number of curve objects in the project.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to MAXCLASS do
    if IsCurve(I) then Result := Result + Lists[I].Count;
end;


function TProject.GetNodeCount: Integer;
//-----------------------------------------------------------------------------
//  Counts the number of node objects in the project.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to MAXCLASS do
    if IsNode(I) then Result := Result + Lists[I].Count;
end;


function TProject.GetLastNodes(var N1, N2: TNode): Boolean;
//-----------------------------------------------------------------------------
// Retrieves last two nodes added to the database
// (used to supply default end nodes for a new link).
//-----------------------------------------------------------------------------
var
  I, J: Integer;
begin
  N1 := nil;
  N2 := nil;
  for I := 0 to MAXCLASS do
  begin
    if not IsNode(I) then continue;
    for J := Lists[I].Count-1 downto 0 do
    begin
      if N2 = nil then N2 := GetNode(I, J)
      else
      begin
        N1 := GetNode(I, J);
        Result := True;
        Exit;
      end;
    end;
  end;
  Result := False;
end;


function TProject.GetLinkCount: Integer;
//-----------------------------------------------------------------------------
//  Counts the number of link objects in the project.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to MAXCLASS do
    if IsLink(I) then Result := Result + Lists[I].Count;
end;


procedure TProject.InitCurrentItems;
//-----------------------------------------------------------------------------
//  Initializes the CurrentItem pointer for each list of object type.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  Lists[OPTION].Add('General');
  Lists[OPTION].Add('Dates');
  Lists[OPTION].Add('Time Steps');
  Lists[OPTION].Add('Dynamic Wave');
  Lists[OPTION].Add('Interface Files');
  Lists[OPTION].Add('Reporting');
  Lists[OPTION].Add('Events');
  Lists[CLIMATE].Add('Temperature');
  Lists[CLIMATE].Add('Evaporation');
  Lists[CLIMATE].Add('Wind Speed');
  Lists[CLIMATE].Add('Snow Melt');
  Lists[CLIMATE].Add('Areal Depletion');
  Lists[CLIMATE].Add('Adjustments');

  Climatology.Init;
  for I := 0 to MAXCLASS do
  begin
    if Lists[I].Count = 0 then
    begin
      CurrentItem[I] := -1;
      HasItems[I] := False;
    end
    else
    begin
      CurrentItem[I] := 0;
      HasItems[I] := True;
    end;
    HasItems[OPTION] := True;
    HasItems[CLIMATE] := True;
    NextID[I] := IDIncrement;
  end;
  Uupdate.UpdateLinkHints;
end;

//-----------------------------------------------------------------------------
//  The following functions test what category an object belongs to.
//-----------------------------------------------------------------------------

function TProject.IsSubcatch(const ObjType: Integer): Boolean;
begin
  if ObjType = SUBCATCH then Result := True
  else Result := False;
end;

function TProject.IsNode(const ObjType: Integer): Boolean;
begin
  if ObjType in [JUNCTION..STORAGE] then Result := True
  else Result := False;
end;

function TProject.IsLink(const ObjType: Integer): Boolean;
begin
  if ObjType in [CONDUIT..OUTLET] then Result := True
  else Result := False;
end;

function TProject.IsCurve(const ObjType: Integer): Boolean;
begin
  if ObjType in [CONTROLCURVE..WEIRCURVE]
  then Result := True
  else Result := False;
end;

function TProject.IsVisual(const ObjType: Integer): Boolean;
begin
  if ObjType in [RAINGAGE..MAPLABEL] then Result := True
  else Result := False;
end;

function TProject.IsSortable(const ObjType: Integer): Boolean;
begin
  if ObjType in [RAINGAGE..AQUIFER] then Result := True
  else Result := False;
end;

//-----------------------------------------------------------------------------
//  The following functions return a pointer to the object of a given type.
//-----------------------------------------------------------------------------

function TProject.GetGage(const Index: Integer): TRaingage;
begin
  Result := TRaingage(Lists[RAINGAGE].Objects[Index]);
end;

function TProject.GetSubcatch(const Atype: Integer;
  const Index: Integer): TSubcatch;
begin
  Result := TSubcatch(Lists[Atype].Objects[Index]);
end;

function TProject.GetNode(const Ntype: Integer; const Index: Integer): TNode;
begin
  Result := TNode(Lists[Ntype].Objects[Index]);
end;

function TProject.GetNode(const S: String): TNode;
var
  Ntype, Index: Integer;
begin
  if FindNode(S, Ntype, Index) then Result := GetNode(Ntype, Index)
  else Result := nil;
end;

function TProject.GetLink(const Ltype: Integer; const Index: Integer): TLink;
begin
  Result := TLink(Lists[Ltype].Objects[Index]);
end;

function TProject.GetMapLabel(const Index: Integer): TMapLabel;
begin
  Result := TMapLabel(Lists[MAPLABEL].Objects[Index]);
end;

function TProject.GetID(const ObjType: Integer; const Index: Integer): String;
//-----------------------------------------------------------------------------
// Returns ID label of object of type ObjType and index Index.
//-----------------------------------------------------------------------------
begin
  Result := Lists[ObjType].Strings[Index];
end;

function TProject.HasResults(const ObjType: Integer;
  const Index: Integer): Boolean;
//-----------------------------------------------------------------------------
// Determines if computed results exist for a given object.
//-----------------------------------------------------------------------------
begin
  if IsSubcatch(ObjType) and (GetSubcatch(SUBCATCH, Index).OutFileIndex >= 0)
  then Result := True
  else if IsNode(ObjType) and (GetNode(ObjType, Index).OutFileIndex >= 0)
  then Result := True
  else if IsLink(ObjType) and (GetLink(ObjType, Index).OutFileIndex >= 0)
  then Result := True
  else Result := False;
end;

//-----------------------------------------------------------------------------
//  The following functions locate an object in the data base given its ID name.
//-----------------------------------------------------------------------------

function TProject.FindSubcatch(const S: String; var Atype: Integer;
  var Index: Integer): Boolean;
begin
  Result := True;
  Atype := SUBCATCH;
  Index := Lists[SUBCATCH].IndexOf(S);
  if Index >= 0 then Exit;
  Result := False;
end;

function TProject.FindNode(const S: String; var Ntype: Integer;
  var Index: Integer): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to MAXCLASS do
  begin
    if not IsNode(I) then continue;
    Ntype := I;
    Index := Lists[Ntype].IndexOf(S);
    if Index >= 0 then Exit;
  end;
  Result := False;
end;

function TProject.FindLink(const S: String; var Ltype: Integer;
  var Index: Integer): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to MAXCLASS do
  begin
    if not IsLink(I) then continue;
    Ltype := I;
    Index := Lists[Ltype].IndexOf(S);
    if Index >= 0 then Exit;
  end;
  Result := False;
end;

function TProject.FindCurve(const S: String; var Ctype: Integer;
  var Index: Integer): Boolean;
var
  I: Integer;
begin
  Result := True;
  Index := -1;
  for I := 0 to MAXCLASS do
  begin
    if not IsCurve(I) then continue;
    Ctype := I;
    Index := Lists[Ctype].IndexOf(S);
    if Index >= 0 then Exit;
  end;
  Result := False;
end;


procedure TProject.SetStreetConduitDepth(const StreetName: string;
  const DepthStr: string);
//-----------------------------------------------------------------------------
//  Sets max. depth parameter of a Street conduit DepthStr.
//-----------------------------------------------------------------------------
var
  I: Integer;
  L: TLink;
begin
  for I := 0 to Lists[CONDUIT].Count-1 do
  begin
    L := GetLink(CONDUIT, I);
    if SameText(StreetName, L.Data[CONDUIT_TSECT_INDEX]) then
    begin
      L.Data[CONDUIT_GEOM1_INDEX] := DepthStr;
    end;
  end;
end;


procedure TProject.SetTransectConduitDepth(const TsectName: String;
  const DepthStr: String);
//-----------------------------------------------------------------------------
//  Sets max. depth parameter of a transect with ID TsectName to DepthStr.
//-----------------------------------------------------------------------------
var
  I: Integer;
  L: TLink;
begin
  for I := 0 to Lists[CONDUIT].Count-1 do
  begin
    L := GetLink(CONDUIT, I);
    if SameText(TsectName, L.Data[CONDUIT_TSECT_INDEX]) then
    begin
      L.Data[CONDUIT_GEOM1_INDEX] := DepthStr;
    end;
  end;
end;


//=============================================================================
//                    Object ID management functions
//=============================================================================

function TProject.DupID(const ID: String; const Obj: Integer;
  const Indx: Integer): Boolean;
//-----------------------------------------------------------------------------
// Checks if ID for object Indx of type Obj already exists.
//-----------------------------------------------------------------------------
var
  I : Integer;
  J : Integer;
  S : String;
  P : PChar;
begin
  // Temporarily blank out ID
  Result := False;
  S := GetID(Obj, Indx);
  Lists[Obj].Strings[Indx] := '';

  // Check if ID already in use by another object of same type
  // NOTE: subcatchments and nodes cannot share same IDs
  if IsSubcatch(Obj) or IsNode(Obj) then
  begin
    if FindSubcatch(ID, I, J) or FindNode(ID, I, J) then Result := True;
  end
  else if IsLink(Obj) then
  begin
    if FindLink(ID, I, J) then Result := True;
  end
  else if isCurve(Obj) then
  begin
    if FindCurve(ID, I, J) then Result := True;
  end
  else
  begin
   J := Lists[Obj].IndexOf(ID);
   if J >= 0 then Result := True;
  end;

  // Restore ID property for objects that have one
  Lists[Obj].Strings[Indx] := S;
  P := PChar(Lists[Obj].Strings[Indx]);
  if Obj = RAINGAGE   then GetGage(Indx).ID := P
  else if IsSubcatch(Obj) then GetSubcatch(Obj, Indx).ID := P
  else if IsNode(Obj) then GetNode(Obj, Indx).ID := P
  else if IsLink(Obj) then GetLink(Obj, Indx).ID := P;
end;

function TProject.IDExists(const ObjType: Integer; const ID: string): Boolean;
//-----------------------------------------------------------------------------
// Determines if object already has ID (used by GetNextID function).
//-----------------------------------------------------------------------------
var
  I, J: Integer;
begin
  Result := False;

  // Object is a subcatchment
  // NOTE: subcatchments and nodes cannot share same IDs
  if IsSubcatch(ObjType) then
  begin
    if FindSubcatch(ID, I, J) then Result := True
    else if FindNode(ID, I, J) then Result := True;
  end

  // Object is a node
  // NOTE: subcatchments and nodes cannot share same IDs
  else if IsNode(ObjType) then
  begin
    if FindNode(ID, I, J) then Result := True
    else if FindSubcatch(ID, I, J) then Result := True;
  end

  // Object is a link
  else if IsLink(ObjType) then
  begin
    if FindLink(ID, I, J) then Result := True;
  end

  // Object is a curve
  else if IsCurve(ObjType) then
  begin
    if FindCurve(ID, I, J) then Result := True;
  end

  // Object is anything else
  else if Lists[ObjType].IndexOf(ID) >= 0 then
    Result := True;
end;

function TProject.GetNextID(const ObjType : Integer): String;
//-----------------------------------------------------------------------------
// Returns next available default ID tag for network object.
//-----------------------------------------------------------------------------
var
  N : LongInt;
  S : String;
  V : LongInt;
  Code: Integer;
begin
  // Get ID prefix and next ID number.
  S := IDPrefix[ObjType];
  Val(S, V, Code);
  N := NextID[ObjType];

  // Keep incrementing ID number until a unique ID is created.
  // If prefix is a number, then ID = Prefix + Next ID Number.
  if (Code = 0) then
  begin
    while IDExists(ObjType, IntToStr(V+N)) do Inc(N, IDIncrement);
    Result := IntToStr(V+N);
  end

  // Otherwise ID = concatonation of Prefix & ID number.
  else
  begin
    while IDExists(ObjType, S + IntToStr(N)) do Inc(N, IDIncrement);
    Result := S + IntToStr(N);
  end;

  // Save last ID number used.
  NextID[ObjType] := N;
end;

//-----------------------------------------------------------------------------
// Subcatchment class methods
//-----------------------------------------------------------------------------
constructor TSubcatch.Create;
begin
  inherited Create;
  OutNode := nil;
  OutSubcatch := nil;
  GwLatFlowEqn := '';
  GwDeepFlowEqn := '';
  LIDs := TStringlist.Create;
  Groundwater := TStringlist.Create;
  Landuses := TStringlist.Create;
  Loadings := TStringlist.Create;
  Vlist := TVertexList.Create;
  Uutils.CopyStringArray(DefInfil, InfilData);
end;

destructor TSubcatch.Destroy;
begin
  Ulid.FreeLIDUnits(LIDs);
  LIDs.Free;
  Groundwater.Free;
  Landuses.Free;
  Loadings.Free;
  Vlist.Free;
  inherited Destroy;
end;

procedure TSubcatch.SetCentroid;
//
//  Finds the centroid coordinates of a subcatchment's polygon outline.
//
begin
  X := MISSING;
  Y := MISSING;
  if Vlist.Count > 0 then Vlist.PolygonCentroid(X, Y);
end;

function TSubcatch.GetArea: Extended;
//
//  Finds the area encompassed by a subcatchment's polygon outline.
//
begin
  Result := Vlist.PolygonArea;
end;

function TSubcatch.AddVertex: PVertex;
//
// Adds a new vertex to a subcatchment's polygon outline.
//
begin
  Result := Vlist.AddPolygonVertex;
end;

//-----------------------------------------------------------------------------
// Node class methods
//-----------------------------------------------------------------------------
constructor TNode.Create;
begin
  inherited Create;
  DWInflow := TStringlist.Create;
  DXInflow := TStringlist.Create;
  IIInflow := TStringlist.Create;
  Treatment := TStringlist.Create;
  Vlist := TVertexList.Create;
  AdjList := TList.Create;
  InfilData[STORAGE_SUCTION_INDEX] := '0';
  InfilData[STORAGE_KSAT_INDEX] := '0';
  InfilData[STORAGE_IMDMAX_INDEX] := '0';
end;

destructor TNode.Destroy;
begin
  DWInflow.Free;
  DXInflow.Free;
  IIInflow.Free;
  Treatment.Free;
  Vlist.Free;
  AdjList.Clear;
  AdjList.Free;
  inherited Destroy;
end;

procedure TNode.SetCentroid;
//
//  Finds the centroid coordinates of a Storage node's polygon outline.
//
begin
  if Ntype = STORAGE then
  begin
    X := MISSING;
    Y := MISSING;
    if Vlist.Count > 0 then Vlist.PolygonCentroid(X, Y);
  end;
end;

function TNode.AddVertex: PVertex;
//
// Adds a new vertex to a storage node's polygon outline.
//
begin
  if Ntype = STORAGE then
  begin
    Result := Vlist.AddPolygonVertex
  end
  else
    Result := nil;
end;

//-----------------------------------------------------------------------------
// Link class methods
//-----------------------------------------------------------------------------
constructor TLink.Create;
begin
  inherited Create;
  Vlist := TVertexList.Create;
end;

destructor TLink.Destroy;
begin
  Vlist.Free;
  TInletUsage(Inlet).Free;
  inherited Destroy;
end;

function TLink.AddVertex: PVertex;
//
// Adds a new vertex to a link's polyline.
//
var
  Vx, Vy : Extended;
begin
  with Vlist do
  begin
    if Vcurrent = nil then
    begin
      Vx := (Node1.X + Node2.X)/2;
      Vy := (Node1.Y + Node2.Y)/2;
    end
    else if Vcurrent^.Next = nil then
    begin
      Vx := (Vcurrent^.X + Node2.X)/2;
      Vy := (Vcurrent^.Y + Node2.Y)/2;
    end
    else
    begin
      Vx := (Vcurrent^.X + (Vcurrent^.Next)^.X)/2;
      Vy := (Vcurrent^.Y + (Vcurrent^.Next)^.Y)/2;
    end;
    Result := Add(Vx,Vy);
  end;
end;

procedure TLink.ReverseNodes;
//
// Reverses the upstream and downstream nodes of a link.
//
var
  Ntemp: TNode;
  S: String;
begin
  Ntemp := Node2;
  Node2 := Node1;
  Node1 := Ntemp;
  if Ltype = CONDUIT then
  begin
    S := Data[CONDUIT_OUTLET_HT_INDEX];
    Data[CONDUIT_OUTLET_HT_INDEX] := Data[CONDUIT_INLET_HT_INDEX];
    Data[CONDUIT_INLET_HT_INDEX] := S;
  end;
end;


//-----------------------------------------------------------------------------
// MapLabel class methods
//-----------------------------------------------------------------------------
constructor TMapLabel.Create;
begin
  inherited Create;
  X := MISSING;
  Y := MISSING;
  Anchor := nil;
  FontName := 'Arial';
  FontSize := 10;
  FontBold := False;
  FontItalic := False;
end;

procedure TMapLabel.GetFont(aFont: TFont);
//
// Places the font properties of a map label into aFont.
//
begin
  aFont.Name := FontName;
  aFont.Size := FontSize;
  aFont.Style := [];
  if FontBold then aFont.Style := aFont.Style + [fsBold];
  if FontItalic then aFont.Style := aFont.Style + [fsItalic];
end;


//-----------------------------------------------------------------------------
// Climatology class methods
//-----------------------------------------------------------------------------
constructor TClimatology.Create;
begin
  inherited Create;
  Init;
end;

procedure TClimatology.Init;
//
// Initializes the properties of the Climatology object.
//
var
  I: Integer;
begin
  TempDataSource := NO_TEMP;
  TempTseries := '';
  TempFile := '';
  TempStartDate := '*';
  TempUnitsType := 0;

  EvapType := CONSTANT_EVAP;
  EvapTseries  := '';
  for I := 0 to 11 do
  begin
    EvapData[I] := '0.0';
    PanData[I] := '1.0';
    TempAdjust[I] := '';
    EvapAdjust[I] := '';
    RainAdjust[I] := '';
    CondAdjust[I] := '';
  end;
  RecoveryPat := '';
  EvapDryOnly := False;

  WindType := MONTHLY_WINDSPEED;
  for I := 1 to 12 do WindSpeed[I] := '0.0';
  for I := 1 to 6 do SnowMelt[I] := DefSnowMelt[I];
  for I := 1 to 10 do
  begin
    ADCurve[1][I] := Uproject.DefADCurve[I];
    ADCurve[2][I] := Uproject.DefADCurve[I];
  end;
end;


//-----------------------------------------------------------------------------
// Hydrograph class methods
//-----------------------------------------------------------------------------
constructor THydrograph.Create;
begin
  inherited Create;
  Raingage := '';
end;


//-----------------------------------------------------------------------------
// Snowpack class methods
//-----------------------------------------------------------------------------
constructor TSnowpack.Create;
var
  I, J: Integer;
begin
  inherited Create;
  FracPlowable := '0.0';
  for I := 1 to 3 do
  begin
    for J := 1 to 8 do Data[I][J] := DefSnowPack[J];
  end;
  Plowing[1] := '1.0';
  for I := 2 to 6 do Plowing[I] := '0.0';
  Plowing[7] := '';
end;


//-----------------------------------------------------------------------------
// Landuse class methods
//-----------------------------------------------------------------------------
constructor TLanduse.Create;
begin
  inherited Create;
  NonpointSources := TStringlist.Create;
end;

destructor TLanduse.Destroy;
begin
  ClearNonpointSources;
  NonpointSources.Free;
  inherited Destroy;
end;

procedure TLanduse.ClearNonpointSources;
//
// Removes all pollutant buildup/washoff functions associated with a land use.
//
var
  I: Integer;
begin
  for I := 0 to NonpointSources.Count - 1 do
    TNonpointSource(NonpointSources.Objects[I]).Free;
  NonpointSources.Clear;
end;


//-----------------------------------------------------------------------------
// Timeseries class methods
//-----------------------------------------------------------------------------
constructor TTimeseries.Create;
begin
  inherited Create;
  Filename := '';
  Dates := TStringlist.Create;
  Times := TStringlist.Create;
  Values := TStringlist.Create;
end;

destructor TTimeseries.Destroy;
begin
  Dates.Free;
  Times.Free;
  Values.Free;
  inherited Destroy;
end;


//-----------------------------------------------------------------------------
// Curve class methods
//-----------------------------------------------------------------------------
constructor TCurve.Create;
begin
  inherited Create;
  CurveType := '';
  CurveCode := 0;
  Xdata := TStringList.Create;
  Ydata := TStringList.Create;
end;

destructor TCurve.Destroy;
begin
  Xdata.Free;
  Ydata.Free;
  inherited Destroy;
end;


//-----------------------------------------------------------------------------
// Street class methods
//-----------------------------------------------------------------------------
constructor TStreet.Create;
begin
  inherited Create;
  MaxDepth := '';
  Uutils.CopyStringArray(DefStreet, Data);
end;

procedure TStreet.SetMaxDepth;
var
  D, D1, D2: Single;
  Ycurb, Ydep, Wcrown, Wgut, Wback, Scross, Sback: Single;
begin
  MaxDepth := Data[STREET_CURB_HEIGHT];
  if Uutils.GetSingle(Data[STREET_CURB_HEIGHT], Ycurb)  and
     Uutils.GetSingle(Data[STREET_DEPRESSION], Ydep)    and
     Uutils.GetSingle(Data[STREET_CROWN_WIDTH], Wcrown) and
     Uutils.GetSingle(Data[STREET_GUTTER_WIDTH], Wgut)  and
     Uutils.GetSingle(Data[STREET_BACK_WIDTH], Wback)   and
     Uutils.GetSingle(Data[STREET_CROSS_SLOPE], Scross) and
     Uutils.GetSingle(Data[STREET_BACK_SLOPE], Sback)   then
  begin
    D1 := Ydep + Ycurb + Wback * Sback / 100.0;
    D2 := Ydep + Wcrown * Scross / 100.0;
    D := Max(D1, D2);
    MaxDepth := Format('%.3f', [D]);
  end;
end;


//-----------------------------------------------------------------------------
// Transect class methods
//-----------------------------------------------------------------------------
constructor TTransect.Create;
begin
  inherited Create;
  Xdata := TStringList.Create;
  Ydata := TStringList.Create;
  Uutils.CopyStringArray(DefTransect, Data);
end;

destructor TTransect.Destroy;
begin
  Xdata.Free;
  Ydata.Free;
  inherited Destroy;
end;

procedure TTransect.CheckData;
//
// Checks if the station distances for a transect are in increasing order.
//
var
  I: Integer;
  N: Integer;
  InOrder: Boolean;
  X   : Single;
  Xold: Single;
begin
  N := Xdata.Count;
  if (N > 1) and Uutils.GetSingle(Xdata[0], X) then
  begin
    InOrder := True;
    Xold := X;
    for I := 1 to N-1 do
    begin
      if Uutils.GetSingle(Xdata[I], X) then
      begin
        if X < Xold then
        begin
          InOrder := False;
          break;
        end
        else Xold := X;
      end;
    end;
    if not InOrder then SwitchData;
  end;
end;

procedure TTransect.SwitchData;
//
// Switches the X and Y data for a transect's stations.
// (Used only for backward compatibility with beta releases).
//
var
  I: Integer;
  N: Integer;
  S: String;
begin
  N := Xdata.Count;
  if Ydata.Count < N then N := Ydata.Count;
  for I := 0 to N-1 do
  begin
    S := Ydata[I];
    Ydata[I] := Xdata[I];
    Xdata[I] := S;
  end;
end;

procedure TTransect.SetMaxDepth;
//
// Determines the max. depth of a transect.
//
var
  I: Integer;
  N: Integer;
  Ymax: Single;
  Ymin: Single;
  Y: Single;
begin
  Data[TRANSECT_MAX_DEPTH] := '';
  N := Ydata.Count;
  if N < 3 then exit;
  Ymax := -1e10;
  Ymin := 1e10;
  for I := 0 to N-1 do
  begin
    if Uutils.GetSingle(Ydata[I], Y) then
    begin
      Ymin := Min(Ymin, Y);
      Ymax := Max(Ymax, Y);
    end;
  end;
  Y := Ymax - Ymin;
  if Y > 0 then Data[TRANSECT_MAX_DEPTH] := Format('%.2f', [Y]);
end;


//-----------------------------------------------------------------------------
// Time pattern class methods
//-----------------------------------------------------------------------------
constructor TPattern.Create;
var
  I: Integer;
begin
  inherited Create;
  PatternType := 0;
  Count := 0;
  for I := Low(Data) to High(Data) do Data[I] := '1.0';
end;


//------------------------------------------------------------------------------
// Options class methods
//------------------------------------------------------------------------------
constructor TOptions.Create;
begin
  inherited Create;
  Report := TStringlist.Create;
end;

destructor TOptions.Destroy;
begin
  Report.Free;
  inherited Destroy;
end;


//-----------------------------------------------------------------------------
// ProjectClipboard class methods
//-----------------------------------------------------------------------------
constructor TProjectClipboard.Create;
begin
  inherited Create;
  Data := TStringlist.Create;
  List1 := TStringlist.Create;
  List2 := TStringlist.Create;
  Font := TFont.Create;
  ObjType := -1;
end;

destructor TProjectClipboard.Destroy;
begin
  Data.Free;
  List1.Free;
  List2.Free;
  Font.Free;
  inherited Destroy;
end;

procedure TProjectClipboard.Clear;
begin
  Data.Clear;
  List1.Clear;
  List2.Clear;
  ObjType := -1;
end;

end.
