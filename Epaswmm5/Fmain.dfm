object MainForm: TMainForm
  Left = 328
  Top = 128
  Caption = 'EPA SWMM 5'
  ClientHeight = 656
  ClientWidth = 961
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clBlack
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsMDIForm
  Icon.Data = {
    0000010001002020100000000400E80200001600000028000000200000004000
    0000010004000000000000020000000000000000000010000000100000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000444
    444444444444444444444444444404EEEEEEEEEE4444E4444EEEEEEEEEE404EE
    EEEEEE444444E444444EEEEEEEE404EEEEEE44444444E44444444EEEEEE404EE
    EE4444444444E4444444444EEEE404EEE4444444444EEE4444444444EEE404EE
    E4444444444EEE44444444444EE404EE44444444444EEEE4444444444EE404E4
    4444444444EEEEE44444444444E404E44444444444EEEEEE4444444444E40444
    444444444EEEEEEE444444444444044444444444EEEEEEEEE444444444440444
    4444444EE4444444EE4444444444044444444EE44444444444EE444444440444
    4444EE4444444444444EE444444404444EEEE444444444444444EEEE444404EE
    EEEE44444444444444444EEEEEE404EEEEEE44444444444444444EEEEEE404EE
    EEE4444444444444444444EEEEE404EEEEE4EEE4EEE4EE4EEE4EE4EEEEE404EE
    EEEEEEEEEEEEEEEEEEEEEEEEEEE404EEEEE4444444EEEEE4444444EEEEE404EE
    EEE4444444EEEEE4444444EEEEE404EEEEE4444444EEEEE4444444EEEEE404EE
    EEEE444444EEEEE444444EEEEEE404EEEEEE4444444EEE4444444EEEEEE404EE
    EEEEE444444444444444EEEEEEE404EEEEEEEE4444444444444EEEEEEEE404EE
    EEEEEEE44444444444EEEEEEEEE404EEEEEEEEEE444444444EEEEEEEEEE40444
    4444444444444444444444444444000000000000000000000000000000008000
    0000800000008000000080000000800000008000000080000000800000008000
    0000800000008000000080000000800000008000000080000000800000008000
    0000800000008000000080000000800000008000000080000000800000008000
    0000800000008000000080000000800000008000000080000000FFFFFFFF}
  KeyPreview = True
  Menu = MainMenu1
  OldCreateOrder = False
  WindowMenu = MnuWindow
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object Splitter1: TSplitter
    Left = 173
    Top = 28
    Width = 5
    Height = 568
    AutoSnap = False
    Beveled = True
    Color = clBtnShadow
    ParentColor = False
    ExplicitTop = 30
    ExplicitHeight = 566
  end
  object ProgressPanel: TPanel
    Left = 0
    Top = 596
    Width = 961
    Height = 30
    Align = alBottom
    Alignment = taLeftJustify
    BevelOuter = bvLowered
    ParentBackground = False
    ParentColor = True
    TabOrder = 1
    object ProgressBar: TProgressBar
      Left = 158
      Top = 6
      Width = 189
      Height = 15
      Step = 1
      TabOrder = 0
    end
  end
  object StatusBar: TToolBar
    Left = 0
    Top = 626
    Width = 961
    Height = 30
    Align = alBottom
    AutoSize = True
    ButtonHeight = 30
    ButtonWidth = 119
    DrawingStyle = dsGradient
    EdgeInner = esNone
    EdgeOuter = esNone
    Images = RunImageList
    List = True
    ShowCaptions = True
    TabOrder = 2
    Transparent = True
    Wrapable = False
    object StatusHint: TToolButton
      Left = 0
      Top = 0
      AutoSize = True
      Style = tbsTextButton
      Visible = False
    end
    object StatusHintSep: TToolButton
      Left = 14
      Top = 0
      Width = 8
      Caption = 'StatusHintSep'
      ImageIndex = 0
      ImageName = 'Item1'
      Style = tbsSeparator
    end
    object AutoLengthBtn: TToolButton
      Left = 22
      Top = 0
      Hint = 'Automatically calculate lengths and areas'
      AutoSize = True
      Caption = 'AutoLength: Off'
      DropdownMenu = AutoLengthMnu
      ParentShowHint = False
      ShowHint = True
      Style = tbsDropDown
    end
    object ToolButton4: TToolButton
      Left = 140
      Top = 0
      Width = 8
      Caption = 'ToolButton4'
      ImageIndex = 0
      ImageName = 'Item1'
      Style = tbsSeparator
    end
    object OffsetsBtn: TToolButton
      Left = 148
      Top = 0
      Hint = 'Link offset convention.'
      AutoSize = True
      Caption = 'Offsets: Depth'
      DropdownMenu = OffsetsMnu
      ParentShowHint = False
      ShowHint = True
      Style = tbsDropDown
    end
    object ToolButton3: TToolButton
      Left = 255
      Top = 0
      Width = 8
      Caption = 'ToolButton3'
      ImageIndex = 0
      ImageName = 'Item1'
      Style = tbsSeparator
    end
    object FlowUnitsBtn: TToolButton
      Left = 263
      Top = 0
      Hint = 'Choice of flow units (and unit system)'
      AutoSize = True
      Caption = 'Flow Units: CFS'
      DropdownMenu = FlowUnitsMnu
      ParentShowHint = False
      ShowHint = True
      Style = tbsDropDown
    end
    object ToolButton6: TToolButton
      Left = 376
      Top = 0
      Width = 8
      Caption = 'ToolButton6'
      ImageIndex = 0
      ImageName = 'Item1'
      Style = tbsSeparator
    end
    object RunStatusButton: TToolButton
      Left = 384
      Top = 0
      Hint = 'No Results Available'
      AutoSize = True
      ImageIndex = 0
      ImageName = 'Item1'
      ParentShowHint = False
      ShowHint = True
    end
    object ToolButton7: TToolButton
      Left = 418
      Top = 0
      Width = 8
      Caption = 'ToolButton7'
      ImageIndex = 0
      ImageName = 'Item1'
      Style = tbsSeparator
    end
    object ZoomLevelLabel: TToolButton
      Left = 426
      Top = 0
      AutoSize = True
      Caption = 'Zoom Level: 100'
      Style = tbsTextButton
    end
    object ToolButton1: TToolButton
      Left = 523
      Top = 0
      Width = 8
      Caption = 'ToolButton1'
      ImageIndex = 0
      ImageName = 'Item1'
      Style = tbsSeparator
    end
    object XYLabel: TToolButton
      Left = 531
      Top = 0
      AutoSize = True
      Caption = 'X, Y:'
      Style = tbsTextButton
    end
  end
  object BrowserPageControl: TPageControl
    Left = 0
    Top = 28
    Width = 173
    Height = 568
    ActivePage = BrowserMapPage
    Align = alLeft
    DoubleBuffered = False
    ParentDoubleBuffered = False
    TabOrder = 0
    OnResize = BrowserPageControlResize
    object BrowserDataPage: TTabSheet
      Caption = 'Project'
      object BrowserDataSplitter: TSplitter
        Left = 0
        Top = 335
        Width = 165
        Height = 5
        Cursor = crVSplit
        Align = alBottom
        AutoSnap = False
        Beveled = True
        Color = clBtnShadow
        ParentColor = False
      end
      object ItemsPanel: TPanel
        Left = 0
        Top = 340
        Width = 165
        Height = 198
        Align = alBottom
        BevelOuter = bvNone
        ParentBackground = False
        TabOrder = 1
        object ItemsLabel: TLabel
          Left = 0
          Top = 22
          Width = 165
          Height = 18
          Align = alTop
          AutoSize = False
          Caption = 'Title/Notes'
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlue
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          Layout = tlCenter
          ExplicitTop = 13
          ExplicitWidth = 168
        end
        object BrowserToolBar: TToolBar
          Left = 0
          Top = 0
          Width = 165
          Height = 22
          AutoSize = True
          ButtonWidth = 26
          Caption = 'BrowserToolBar'
          DisabledImages = ProjectImageList1
          DrawingStyle = dsGradient
          Images = ProjectImageList
          GradientDrawingOptions = [gdoHotTrack]
          TabOrder = 0
          Transparent = True
          object BrowserBtnNew: TToolButton
            Left = 0
            Top = 0
            Hint = 'Add Object'
            ImageIndex = 0
            ImageName = 'green_plus'
            ParentShowHint = False
            ShowHint = True
            OnClick = BrowserBtnNewClick
          end
          object BrowserBtnDelete: TToolButton
            Left = 26
            Top = 0
            Hint = 'Delete Object'
            ImageIndex = 1
            ImageName = 'red_minus'
            ParentShowHint = False
            ShowHint = True
            OnClick = BrowserBtnDeleteClick
          end
          object BrowserBtnEdit: TToolButton
            Left = 52
            Top = 0
            Hint = 'Edit Selection'
            ImageIndex = 2
            ImageName = 'edit'
            ParentShowHint = False
            ShowHint = True
            OnClick = BrowserBtnEditClick
          end
          object BrowserBtnUp: TToolButton
            Left = 78
            Top = 0
            Hint = 'Move Up'
            ImageIndex = 3
            ImageName = 'uparrow1'
            ParentShowHint = False
            ShowHint = True
            OnClick = BrowserBtnUpClick
          end
          object BrowserBtnDown: TToolButton
            Left = 104
            Top = 0
            Hint = 'Move Down'
            Caption = 'BrowserBtnDown'
            ImageIndex = 4
            ImageName = 'dnarrow1'
            ParentShowHint = False
            ShowHint = True
            OnClick = BrowserBtnDownClick
          end
          object BrowserBtnSort: TToolButton
            Left = 130
            Top = 0
            Hint = 'Sort Objects'
            Caption = 'BrowserBtnSort'
            ImageIndex = 5
            ImageName = 'sort'
            ParentShowHint = False
            ShowHint = True
            OnClick = BrowserBtnSortClick
          end
        end
        object ItemListBox: TListBox
          Left = 0
          Top = 128
          Width = 165
          Height = 70
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Style = lbVirtual
          DoubleBuffered = False
          ParentDoubleBuffered = False
          TabOrder = 1
          OnClick = ItemListBoxClick
          OnData = ItemListBoxData
          OnDblClick = ItemListBoxDblClick
          OnDrawItem = ItemListBoxDrawItem
          OnKeyDown = ItemListBoxKeyDown
          OnKeyPress = ItemListBoxKeyPress
          OnMouseDown = ItemListBoxMouseDown
        end
        object NotesMemo: TMemo
          Left = 10
          Top = 46
          Width = 141
          Height = 65
          ReadOnly = True
          TabOrder = 2
        end
      end
      object ObjectTreeView: TTreeView
        Left = 0
        Top = 0
        Width = 165
        Height = 335
        Align = alClient
        Ctl3D = True
        DoubleBuffered = True
        HideSelection = False
        Indent = 19
        ParentCtl3D = False
        ParentDoubleBuffered = False
        ReadOnly = True
        TabOrder = 0
        OnChange = ObjectTreeViewChange
        OnClick = ObjectTreeViewClick
        OnKeyPress = ObjectTreeViewKeyPress
        Items.NodeData = {
          030A000000340000000000000000000000FFFFFFFFFFFFFFFF00000000000000
          0000000000010B5400690074006C0065002F004E006F007400650073002C0000
          000100000001000000FFFFFFFFFFFFFFFF00000000000000000000000001074F
          007000740069006F006E007300340000000200000002000000FFFFFFFFFFFFFF
          FF000000000000000000000000010B43006C0069006D00610074006F006C006F
          0067007900300000000300000003000000FFFFFFFFFFFFFFFF00000000000000
          0006000000010948007900640072006F006C006F0067007900320000000A0000
          000A000000FFFFFFFFFFFFFFFF000000000000000000000000010A5200610069
          006E00200047006100670065007300380000000B0000000B000000FFFFFFFFFF
          FFFFFF000000000000000000000000010D530075006200630061007400630068
          006D0065006E00740073002E0000001500000016000000FFFFFFFFFFFFFFFF00
          0000000000000000000000010841007100750069006600650072007300320000
          001500000016000000FFFFFFFFFFFFFFFF000000000000000000000000010A53
          006E006F00770020005000610063006B0073003E0000001500000016000000FF
          FFFFFFFFFFFFFF000000000000000000000000011055006E0069007400200048
          007900640072006F006700720061007000680073003600000015000000160000
          00FFFFFFFFFFFFFFFF000000000000000000000000010C4C0049004400200043
          006F006E00740072006F006C007300320000000400000004000000FFFFFFFFFF
          FFFFFF000000000000000006000000010A4800790064007200610075006C0069
          0063007300280000001500000016000000FFFFFFFFFFFFFFFF00000000000000
          000400000001054E006F00640065007300300000000C0000000C000000FFFFFF
          FFFFFFFFFF00000000000000000000000001094A0075006E006300740069006F
          006E0073002E0000000D0000000D000000FFFFFFFFFFFFFFFF00000000000000
          000000000001084F0075007400660061006C006C0073002E0000000E0000000E
          000000FFFFFFFFFFFFFFFF000000000000000000000000010844006900760069
          006400650072007300380000000F0000000F000000FFFFFFFFFFFFFFFF000000
          000000000000000000010D530074006F007200610067006500200055006E0069
          0074007300280000001500000016000000FFFFFFFFFFFFFFFF00000000000000
          000500000001054C0069006E006B0073002E0000001000000010000000FFFFFF
          FFFFFFFFFF000000000000000000000000010843006F006E0064007500690074
          007300280000001100000011000000FFFFFFFFFFFFFFFF000000000000000000
          0000000105500075006D00700073002E0000001200000012000000FFFFFFFFFF
          FFFFFF00000000000000000000000001084F0072006900660069006300650073
          00280000001300000013000000FFFFFFFFFFFFFFFF0000000000000000000000
          000105570065006900720073002C0000001400000014000000FFFFFFFFFFFFFF
          FF00000000000000000000000001074F00750074006C006500740073002C0000
          001500000016000000FFFFFFFFFFFFFFFF000000000000000000000000010753
          007400720065006500740073002A000000000000001500000016000000FFFFFF
          FF000000000000000000000000010649006E006C006500740073003000000015
          00000016000000FFFFFFFFFFFFFFFF0000000000000000000000000109540072
          0061006E00730065006300740073002E0000001500000016000000FFFFFFFFFF
          FFFFFF000000000000000000000000010843006F006E00740072006F006C0073
          002C0000000500000005000000FFFFFFFFFFFFFFFF0000000000000000020000
          0001075100750061006C00690074007900320000001500000016000000FFFFFF
          FFFFFFFFFF000000000000000000000000010A50006F006C006C007500740061
          006E0074007300300000001500000016000000FFFFFFFFFFFFFFFF0000000000
          0000000000000001094C0061006E006400200055007300650073002A00000006
          00000006000000FFFFFFFFFFFFFFFF0000000000000000080000000106430075
          0072007600650073003A0000001500000016000000FFFFFFFFFFFFFFFF000000
          000000000000000000010E43006F006E00740072006F006C0020004300750072
          007600650073003E0000001500000016000000FFFFFFFFFFFFFFFF0000000000
          00000000000000011044006900760065007200730069006F006E002000430075
          007200760065007300340000001500000016000000FFFFFFFFFFFFFFFF000000
          000000000000000000010B500075006D00700020004300750072007600650073
          00380000001500000016000000FFFFFFFFFFFFFFFF0000000000000000000000
          00010D52006100740069006E0067002000430075007200760065007300360000
          001500000016000000FFFFFFFFFFFFFFFF000000000000000000000000010C53
          00680061007000650020004300750072007600650073003A0000001500000016
          000000FFFFFFFFFFFFFFFF000000000000000000000000010E530074006F0072
          0061006700650020004300750072007600650073003600000015000000160000
          00FFFFFFFFFFFFFFFF000000000000000000000000010C54006900640061006C
          002000430075007200760065007300340000001500000016000000FFFFFFFFFF
          FFFFFF000000000000000000000000010B570065006900720020004300750072
          00760065007300340000000700000007000000FFFFFFFFFFFFFFFF0000000000
          00000000000000010B540069006D006500200053006500720069006500730038
          0000000800000008000000FFFFFFFFFFFFFFFF00000000000000000000000001
          0D540069006D00650020005000610074007400650072006E0073003200000009
          00000009000000FFFFFFFFFFFFFFFF000000000000000000000000010A4D0061
          00700020004C006100620065006C007300}
      end
    end
    object BrowserMapPage: TTabSheet
      Caption = 'Map'
      ImageIndex = 1
      object MapScrollBox: TScrollBox
        Left = 0
        Top = 0
        Width = 165
        Height = 538
        Align = alClient
        BorderStyle = bsNone
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 0
        object MapThemesBox: TGroupBox
          Left = 5
          Top = 8
          Width = 129
          Height = 174
          Caption = 'Themes'
          Color = clBtnFace
          ParentColor = False
          TabOrder = 0
          object Label1: TLabel
            Left = 8
            Top = 24
            Width = 82
            Height = 15
            Caption = 'Subcatchments'
          end
          object Label2: TLabel
            Left = 8
            Top = 73
            Width = 34
            Height = 15
            Caption = 'Nodes'
          end
          object Label3: TLabel
            Left = 8
            Top = 121
            Width = 27
            Height = 15
            Caption = 'Links'
          end
          object SubcatchViewBox: TComboBox
            Left = 8
            Top = 45
            Width = 108
            Height = 24
            Style = csOwnerDrawVariable
            Ctl3D = True
            DropDownCount = 20
            ItemHeight = 18
            ParentCtl3D = False
            ParentShowHint = False
            ShowHint = False
            TabOrder = 0
            OnChange = MapViewBoxChange
            OnDrawItem = MapViewBoxDrawItem
            OnMeasureItem = MapViewBoxMeasureItem
          end
          object NodeViewBox: TComboBox
            Left = 8
            Top = 92
            Width = 108
            Height = 24
            Style = csOwnerDrawVariable
            DropDownCount = 20
            ItemHeight = 18
            ParentShowHint = False
            ShowHint = False
            TabOrder = 1
            OnChange = MapViewBoxChange
            OnDrawItem = MapViewBoxDrawItem
            OnMeasureItem = MapViewBoxMeasureItem
          end
          object LinkViewBox: TComboBox
            Left = 8
            Top = 142
            Width = 108
            Height = 24
            BevelInner = bvNone
            Style = csOwnerDrawVariable
            Ctl3D = True
            DropDownCount = 20
            ItemHeight = 18
            ParentCtl3D = False
            ParentShowHint = False
            ShowHint = False
            TabOrder = 2
            OnChange = MapViewBoxChange
            OnDrawItem = MapViewBoxDrawItem
            OnMeasureItem = MapViewBoxMeasureItem
          end
        end
        object MapTimePeriodBox: TGroupBox
          Left = 5
          Top = 192
          Width = 129
          Height = 222
          Caption = 'Time Period'
          Color = clBtnFace
          ParentColor = False
          TabOrder = 1
          DesignSize = (
            129
            222)
          object DateLabel: TLabel
            Left = 8
            Top = 24
            Width = 24
            Height = 15
            Caption = 'Date'
          end
          object TimeLabel: TLabel
            Left = 8
            Top = 96
            Width = 63
            Height = 15
            Caption = 'Time of Day'
          end
          object ElapsedTimeLabel: TLabel
            Left = 8
            Top = 167
            Width = 69
            Height = 15
            Caption = 'Elapsed Time'
          end
          object DateListBox: TComboBox
            Left = 8
            Top = 43
            Width = 108
            Height = 22
            Style = csOwnerDrawFixed
            ParentShowHint = False
            ShowHint = False
            TabOrder = 0
            OnClick = DateListBoxClick
          end
          object DateScrollBar: TScrollBar
            Left = 8
            Top = 67
            Width = 108
            Height = 17
            Ctl3D = True
            PageSize = 0
            ParentCtl3D = False
            TabOrder = 1
            OnChange = DateScrollBarChange
            OnScroll = DateScrollBarScroll
          end
          object TimeListBox: TComboBox
            Left = 8
            Top = 115
            Width = 108
            Height = 22
            Style = csOwnerDrawFixed
            ParentShowHint = False
            ShowHint = False
            TabOrder = 2
            OnClick = TimeListBoxClick
          end
          object TimeScrollBar: TScrollBar
            Left = 8
            Top = 139
            Width = 108
            Height = 17
            Ctl3D = True
            PageSize = 0
            ParentCtl3D = False
            TabOrder = 3
            OnChange = TimeScrollBarChange
            OnScroll = TimeScrollBarScroll
          end
          object ElapsedTimePanel: TEdit
            Left = 10
            Top = 186
            Width = 88
            Height = 21
            AutoSize = False
            Ctl3D = True
            Enabled = False
            ParentCtl3D = False
            ReadOnly = True
            TabOrder = 4
            Text = '0'
            OnKeyDown = ElapsedTimePanelKeyDown
          end
          object ElapsedTimeUpDown: TUpDown
            Left = 103
            Top = 186
            Width = 17
            Height = 21
            Anchors = [akTop, akRight]
            TabOrder = 5
            OnClick = ElapsedTimeUpDownClick
          end
        end
        inline AnimatorFrame: TAnimatorFrame
          Left = 5
          Top = 424
          Width = 129
          Height = 89
          AutoSize = True
          TabOrder = 2
          ExplicitLeft = 5
          ExplicitTop = 424
          inherited AnimatorBox: TGroupBox
            inherited SpeedBar: TTrackBar
              Top = 48
              ExplicitTop = 48
            end
            inherited AnimatorToolBar: TToolBar
              Left = 1
              Top = 16
              Width = 127
              DisabledImages = AnimatorImageList1
              Images = AnimatorImageList
              ExplicitLeft = 1
              ExplicitTop = 16
              ExplicitWidth = 127
              inherited RewindBtn: TToolButton
                ImageName = 'vcrfirst'
              end
              inherited BackBtn: TToolButton
                ImageName = 'vcrback'
              end
              inherited PauseBtn: TToolButton
                ImageName = 'vcrstop'
              end
              inherited FwdBtn: TToolButton
                ImageName = 'vcrfwd'
              end
            end
          end
        end
      end
    end
  end
  object ToolbarPanel: TPanel
    Left = 0
    Top = 0
    Width = 961
    Height = 28
    Align = alTop
    AutoSize = True
    BevelEdges = []
    BevelOuter = bvLowered
    Padding.Top = 2
    Padding.Bottom = 2
    TabOrder = 3
    object ToolBar1: TToolBar
      Left = 1
      Top = 3
      Width = 959
      Height = 22
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      AutoSize = True
      DisabledImages = BtnImageList1
      DoubleBuffered = False
      DrawingStyle = dsGradient
      Images = BtnImageList
      Indent = 2
      GradientDrawingOptions = [gdoHotTrack]
      ParentDoubleBuffered = False
      TabOrder = 0
      Transparent = True
      object TBNew: TToolButton
        Left = 2
        Top = 0
        Hint = 'Start a new project'
        ImageIndex = 0
        ImageName = 'StdToolbar\Item1'
        ParentShowHint = False
        ShowHint = True
        OnClick = MnuNewClick
      end
      object TBOpen: TToolButton
        Left = 25
        Top = 0
        Hint = 'Open an existing project'
        ImageIndex = 1
        ImageName = 'StdToolbar\Item2'
        ParentShowHint = False
        ShowHint = True
        OnClick = MnuOpenClick
      end
      object TBSave: TToolButton
        Left = 48
        Top = 0
        Hint = 'Save the current project'
        ImageIndex = 2
        ImageName = 'StdToolbar\Item3'
        ParentShowHint = False
        ShowHint = True
        OnClick = MnuSaveClick
      end
      object TBPrint: TToolButton
        Left = 71
        Top = 0
        Hint = 'Print the current window'
        ImageIndex = 3
        ImageName = 'StdToolbar\Item4'
        ParentShowHint = False
        ShowHint = True
        OnClick = MnuPrintClick
      end
      object Sep1: TToolButton
        Left = 94
        Top = 0
        Width = 8
        Caption = 'ToolButton5'
        ImageIndex = 4
        ImageName = 'StdToolbar\Item5'
        Style = tbsSeparator
      end
      object TBCopy: TToolButton
        Left = 102
        Top = 0
        Hint = 'Copy the contents of the current window'
        ImageIndex = 4
        ImageName = 'StdToolbar\Item5'
        ParentShowHint = False
        ShowHint = True
        OnClick = MnuCopyClick
      end
      object TBFind: TToolButton
        Left = 125
        Top = 0
        Hint = 'Find an object on the map'
        ImageIndex = 5
        ImageName = 'StdToolbar\Item6'
        ParentShowHint = False
        ShowHint = True
        OnClick = MnuFindObjectClick
      end
      object TBQuery: TToolButton
        Left = 148
        Top = 0
        Hint = 'Find all objects meeting a given criterion'
        ImageIndex = 6
        ImageName = 'StdToolbar\Item7'
        ParentShowHint = False
        ShowHint = True
        OnClick = MnuQueryClick
      end
      object TBOverview: TToolButton
        Left = 171
        Top = 0
        Hint = 'Toggle the display of the Overview Map'
        ImageIndex = 7
        ImageName = 'StdToolbar\Item8'
        ParentShowHint = False
        ShowHint = True
        OnClick = MnuOVMapClick
      end
      object Sep2: TToolButton
        Left = 194
        Top = 0
        Width = 8
        Caption = 'ToolButton10'
        ImageIndex = 8
        ImageName = 'StdToolbar\Item9'
        Style = tbsSeparator
      end
      object TBRun: TToolButton
        Left = 202
        Top = 0
        Hint = 'Run a simulation'
        ImageIndex = 8
        ImageName = 'StdToolbar\Item9'
        ParentShowHint = False
        ShowHint = True
        OnClick = MnuProjectRunSimulationClick
      end
      object ToolButton21: TToolButton
        Left = 225
        Top = 0
        Width = 8
        Caption = 'ToolButton12'
        ImageIndex = 9
        ImageName = 'StdToolbar\Item10'
        Style = tbsSeparator
      end
      object TBReport: TToolButton
        Left = 233
        Top = 0
        Hint = 'View the Status Report'
        DropdownMenu = ReportPopupMenu
        ImageIndex = 9
        ImageName = 'StdToolbar\Item10'
        ParentShowHint = False
        ShowHint = True
        OnClick = MnuReportStatusClick
      end
      object TBProfile: TToolButton
        Left = 256
        Top = 0
        Hint = 'Create a Profile Plot'
        ImageIndex = 10
        ImageName = 'StdToolbar\Item11'
        ParentShowHint = False
        ShowHint = True
        OnClick = MnuGraphProfileClick
      end
      object TBGraph: TToolButton
        Left = 279
        Top = 0
        Hint = 'Create a Time Series Plot'
        ImageIndex = 11
        ImageName = 'StdToolbar\Item12'
        ParentShowHint = False
        ShowHint = True
        OnClick = TBGraphClick
      end
      object TBTable: TToolButton
        Left = 302
        Top = 0
        Hint = 'Create a Time Series Table'
        DropdownMenu = TablePopupMenu
        ImageIndex = 13
        ImageName = 'StdToolbar\Item14'
        ParentShowHint = False
        ShowHint = True
      end
      object TBScatter: TToolButton
        Left = 325
        Top = 0
        Hint = 'Create a Scatter Plot'
        ImageIndex = 12
        ImageName = 'StdToolbar\Item13'
        ParentShowHint = False
        ShowHint = True
        OnClick = TBGraphClick
      end
      object TBStats: TToolButton
        Left = 348
        Top = 0
        Hint = 'Create a Statistics Report'
        ImageIndex = 14
        ImageName = 'StdToolbar\Item15'
        ParentShowHint = False
        ShowHint = True
        OnClick = MnuReportStatisticsClick
      end
      object Sep3: TToolButton
        Left = 371
        Top = 0
        Width = 8
        Caption = 'ToolButton19'
        ImageIndex = 15
        ImageName = 'StdToolbar\Item16'
        Style = tbsSeparator
      end
      object TBOptions: TToolButton
        Left = 379
        Top = 0
        Hint = 'Select viewing options for the current window'
        ImageIndex = 15
        ImageName = 'StdToolbar\Item16'
        ParentShowHint = False
        ShowHint = True
        OnClick = TBOptionsClick
      end
      object TBArrange: TToolButton
        Left = 402
        Top = 0
        Hint = 'Arrange all windows'
        ImageIndex = 17
        ImageName = 'StdToolbar\Item18'
        ParentShowHint = False
        ShowHint = True
        OnClick = MnuWindowCascadeClick
      end
    end
  end
  object MainMenu1: TMainMenu
    Left = 312
    Top = 72
    object MnuFile: TMenuItem
      Caption = '&File'
      OnClick = MnuFileClick
      object MnuNew: TMenuItem
        Caption = '&New'
        ShortCut = 16462
        OnClick = MnuNewClick
      end
      object MnuOpen: TMenuItem
        Caption = '&Open...'
        ShortCut = 16463
        OnClick = MnuOpenClick
      end
      object MnuReopen: TMenuItem
        Caption = '&Reopen'
        OnClick = MnuReopenClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MnuSave: TMenuItem
        Caption = '&Save'
        ShortCut = 16467
        OnClick = MnuSaveClick
      end
      object MnuSaveAs: TMenuItem
        Caption = 'Save &As...'
        ShortCut = 49235
        OnClick = MnuSaveAsClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object MnuExport: TMenuItem
        Caption = '&Export'
        object MnuExportMap: TMenuItem
          Caption = '&Map...'
          OnClick = MnuExportMapClick
        end
        object MnuExportHotstart: TMenuItem
          Caption = '&Hot Start File...'
          OnClick = MnuExportHotstartClick
        end
        object MnuExportStatusRpt: TMenuItem
          Caption = '&Status/Summary Report...'
          OnClick = MnuExportStatusRptClick
        end
      end
      object MnuCombine: TMenuItem
        Caption = '&Combine...'
        OnClick = MnuCombineClick
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object MnuPageSetup: TMenuItem
        Caption = 'Pa&ge Setup...'
        OnClick = MnuPageSetupClick
      end
      object MnuPrintPreview: TMenuItem
        Caption = 'Print Pre&view'
        OnClick = MnuPrintPreviewClick
      end
      object MnuPrint: TMenuItem
        Caption = '&Print'
        OnClick = MnuPrintClick
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object MnuExit: TMenuItem
        Caption = 'E&xit'
        ShortCut = 32883
        OnClick = MnuExitClick
      end
    end
    object MnuEdit: TMenuItem
      Caption = '&Edit'
      OnClick = MnuEditClick
      object MnuCopy: TMenuItem
        Caption = '&Copy To...'
        Enabled = False
        ShortCut = 16451
        OnClick = MnuCopyClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object MnuSelectObject: TMenuItem
        Caption = '&Select Object'
        OnClick = MnuSelectObjectClick
      end
      object MnuSelectVertex: TMenuItem
        Caption = 'Select &Vertex'
        Enabled = False
        OnClick = MnuSelectVertexClick
      end
      object MnuSelectRegion: TMenuItem
        Caption = 'Select &Region'
        OnClick = MnuSelectRegionClick
      end
      object MnuSelectAll: TMenuItem
        Caption = 'Select &All'
        ShortCut = 16449
        OnClick = MnuSelectAllClick
      end
      object N16: TMenuItem
        Caption = '-'
      end
      object MnuFindObject: TMenuItem
        Caption = '&Find Object...'
        ShortCut = 16454
        OnClick = MnuFindObjectClick
      end
      object N21: TMenuItem
        Caption = '-'
      end
      object MnuEditObject: TMenuItem
        Caption = '&Edit Object...'
        ShortCut = 113
        OnClick = BrowserBtnEditClick
      end
      object MnuDeleteObject: TMenuItem
        Caption = '&Delete Object'
        ShortCut = 16430
        OnClick = BrowserBtnDeleteClick
      end
      object N17: TMenuItem
        Caption = '-'
      end
      object MnuGroupEdit: TMenuItem
        Caption = '&Group Edit...'
        Enabled = False
        ShortCut = 8305
        OnClick = MnuGroupEditClick
      end
      object MnuGroupDelete: TMenuItem
        Caption = 'G&roup Delete...'
        Enabled = False
        OnClick = MnuGroupDeleteClick
      end
    end
    object MnuView: TMenuItem
      Caption = '&View'
      object MnuDimensions: TMenuItem
        Tag = 16
        Caption = '&Dimensions...'
        OnClick = MnuDimensionsClick
      end
      object MnuBackdrop: TMenuItem
        Caption = '&Backdrop'
        OnClick = MnuBackdropClick
        object MnuBackdropLoad: TMenuItem
          Caption = '&Load'
          OnClick = MnuBackdropLoadClick
        end
        object MnuBackdropUnload: TMenuItem
          Caption = '&Unload'
          OnClick = MnuBackdropUnloadClick
        end
        object N15: TMenuItem
          Caption = '-'
        end
        object MnuBackdropAlign: TMenuItem
          Caption = '&Align'
          Enabled = False
          OnClick = MnuBackdropAlignClick
        end
        object MnuBackdropResize: TMenuItem
          Caption = '&Resize...'
          Enabled = False
          OnClick = MnuBackdropResizeClick
        end
        object N18: TMenuItem
          Caption = '-'
        end
        object MnuBackdropWatermark: TMenuItem
          Caption = '&Watermark'
          Enabled = False
          OnClick = MnuBackdropWatermarkClick
        end
        object MnuBackdropGrayscale: TMenuItem
          Caption = '&Grayscale'
          Enabled = False
          OnClick = MnuBackdropGrayscaleClick
        end
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object MnuPan: TMenuItem
        Tag = 104
        Caption = '&Pan'
        OnClick = MapActionClick
      end
      object MnuZoomIn: TMenuItem
        Tag = 105
        Caption = 'Zoom &In'
        OnClick = MapActionClick
      end
      object MnuZoomOut: TMenuItem
        Tag = 106
        Caption = 'Zoom &Out'
        OnClick = MapActionClick
      end
      object MnuFullExtent: TMenuItem
        Tag = 107
        Caption = 'Full &Extent'
        OnClick = MapActionClick
      end
      object N14: TMenuItem
        Caption = '-'
      end
      object MnuQuery: TMenuItem
        Caption = '&Query...'
        ShortCut = 16465
        OnClick = MnuQueryClick
      end
      object MnuOVMap: TMenuItem
        Caption = 'Over&view'
        OnClick = MnuOVMapClick
      end
      object N12: TMenuItem
        Caption = '-'
      end
      object MnuObjects: TMenuItem
        Caption = '&Layers'
        OnClick = MnuObjectsClick
        object MnuShowGages: TMenuItem
          Caption = '&Rain Gages'
          Checked = True
          OnClick = MnuShowObjectsClick
        end
        object MnuShowSubcatch: TMenuItem
          Caption = '&Subcatchments'
          Checked = True
          OnClick = MnuShowObjectsClick
        end
        object MnuShowNodes: TMenuItem
          Caption = '&Nodes'
          Checked = True
          OnClick = MnuShowObjectsClick
        end
        object MnuShowLinks: TMenuItem
          Caption = '&Links'
          Checked = True
          OnClick = MnuShowObjectsClick
        end
        object MnuShowLabels: TMenuItem
          Caption = 'Lab&els'
          Checked = True
          OnClick = MnuShowObjectsClick
        end
        object MnuShowBackdrop: TMenuItem
          Caption = '&Backdrop'
          OnClick = MnuShowBackdropClick
        end
      end
      object MnuLegends: TMenuItem
        Caption = 'Le&gends'
        OnClick = MnuLegendsClick
        object MnuSubcatchLegend: TMenuItem
          Caption = '&Subcatchment'
          Checked = True
          OnClick = MnuSubcatchLegendClick
        end
        object MnuNodeLegend: TMenuItem
          Caption = '&Node'
          Checked = True
          OnClick = MnuNodeLegendClick
        end
        object MnuLinkLegend: TMenuItem
          Caption = '&Link'
          Checked = True
          OnClick = MnuLinkLegendClick
        end
        object MnuTimeLegend: TMenuItem
          Caption = '&Time'
          Checked = True
          OnClick = MnuTimeLegendClick
        end
        object N6: TMenuItem
          Caption = '-'
        end
        object MnuModifyLegend: TMenuItem
          Caption = '&Modify'
          OnClick = MnuModifyLegendClick
          object MnuModifySubcatchLegend: TMenuItem
            Caption = '&Subcatchment'
            OnClick = MnuModifySubcatchLegendClick
          end
          object MnuModifyNodeLegend: TMenuItem
            Caption = '&Node'
            OnClick = MnuModifyNodeLegendClick
          end
          object MnuModifyLinkLegend: TMenuItem
            Caption = '&Link'
            OnClick = MnuModifyLinkLegendClick
          end
        end
      end
      object MnuToolbars: TMenuItem
        AutoCheck = True
        Caption = '&Toolbar'
        Checked = True
        OnClick = MnuToolbarsClick
      end
    end
    object MnuProject: TMenuItem
      Caption = '&Project'
      object MnuProjectSummary: TMenuItem
        Caption = '&Summary'
        OnClick = MnuProjectSummaryClick
      end
      object MnuProjectDetails: TMenuItem
        Caption = '&Details'
        OnClick = MnuProjectDetailsClick
      end
      object MnuProjectDefaults: TMenuItem
        Caption = 'De&faults...'
        OnClick = MnuProjectDefaultsClick
      end
      object MnuProjectCalibData: TMenuItem
        Caption = '&Calibration Data...'
        OnClick = MnuProjectCalibDataClick
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object MnuAddObject: TMenuItem
        Caption = 'Add Object'
        ShortCut = 16429
        OnClick = BrowserBtnNewClick
      end
      object N22: TMenuItem
        Caption = '-'
      end
      object MnuProjectRunSimulation: TMenuItem
        Caption = '&Run Simulation'
        ShortCut = 120
        OnClick = MnuProjectRunSimulationClick
      end
    end
    object MnuReport: TMenuItem
      Caption = '&Report'
      OnClick = MnuReportClick
      object MnuReportStatus: TMenuItem
        Caption = '&Status'
        OnClick = MnuReportStatusClick
      end
      object MnuReportSummary: TMenuItem
        Caption = 'S&ummary'
        OnClick = MnuReportSummaryClick
      end
      object MnuReportGraph: TMenuItem
        Caption = '&Graph'
        OnClick = MnuReportGraphClick
        object MnuGraphProfile: TMenuItem
          Caption = '&Profile'
          OnClick = MnuGraphProfileClick
        end
        object MnuGraphTimeSeries: TMenuItem
          Caption = '&Time Series'
          ShortCut = 16455
          OnClick = MnuGraphTimeSeriesClick
        end
        object MnuGraphScatter: TMenuItem
          Caption = '&Scatter'
          OnClick = MnuGraphScatterClick
        end
      end
      object MnuReportTable: TMenuItem
        Caption = '&Table'
        object MnuTableByObject: TMenuItem
          Caption = 'By &Object'
          OnClick = MnuTableByObjectClick
        end
        object MnuTableByVariable: TMenuItem
          Caption = 'By &Variable'
          OnClick = MnuTableByVariableClick
        end
      end
      object MnuReportStatistics: TMenuItem
        Caption = 'St&atistics'
        OnClick = MnuReportStatisticsClick
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object MnuReportOptions: TMenuItem
        Caption = '&Customize...'
        OnClick = MnuReportCustomizeClick
      end
    end
    object MnuTools: TMenuItem
      Caption = '&Tools'
      object MnuProgramOptions: TMenuItem
        Caption = '&Program Preferences...'
        OnClick = MnuPreferencesClick
      end
      object MnuMapDisplayOptions: TMenuItem
        Caption = '&Map Display Options...'
        OnClick = MnuViewOptionsClick
      end
      object MnuConfigureTools: TMenuItem
        Caption = '&Configure Tools...'
        OnClick = MnuConfigureToolsClick
      end
      object N7: TMenuItem
        Caption = '-'
      end
    end
    object MnuWindow: TMenuItem
      Caption = '&Window'
      OnClick = MnuWindowClick
      object MnuWindowCascade: TMenuItem
        Caption = '&Cascade'
        ShortCut = 8308
        OnClick = MnuWindowCascadeClick
      end
      object MnuWindowTile: TMenuItem
        Caption = '&Tile'
        ShortCut = 8307
        OnClick = MnuWindowTileClick
      end
      object MnuWindowCloseAll: TMenuItem
        Caption = 'C&lose All'
        ShortCut = 41075
        OnClick = MnuWindowCloseAllClick
      end
    end
    object MnuHelp: TMenuItem
      Caption = '&Help'
      object MnuHelpTopics: TMenuItem
        Caption = '&User Guide'
        ShortCut = 16496
        OnClick = MnuHelpTopicsClick
      end
      object MnuHowdoI: TMenuItem
        Caption = 'How do I...'
        OnClick = MnuHowdoIClick
      end
      object MnuWhatsNew: TMenuItem
        Caption = 'What'#39's New ...'
        OnClick = MnuWhatsNewClick
      end
      object N13: TMenuItem
        Caption = '-'
      end
      object MnuHelpShortcuts: TMenuItem
        Caption = '&Keyboard Shortcuts'
        OnClick = MnuHelpShortcutsClick
      end
      object MnuHelpUnits: TMenuItem
        Caption = '&Measurement Units'
        OnClick = MnuHelpUnitsClick
      end
      object MnuHelpErrors: TMenuItem
        Caption = '&Error Messages'
        OnClick = MnuHelpErrorsClick
      end
      object N19: TMenuItem
        Caption = '-'
      end
      object MnuHelpTutorials: TMenuItem
        Caption = 'Tutorials'
        object MnuIntroTutorial: TMenuItem
          Caption = 'Basic Tutorial'
          OnClick = MnuIntroTutorialClick
        end
        object MnuInletsTutorial: TMenuItem
          Caption = 'Inlets Tutorial'
          OnClick = MnuInletsTutorialClick
        end
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object MnuWelcomeScreen: TMenuItem
        Caption = '&Welcome Screen'
        OnClick = MnuWelcomeScreenClick
      end
      object MnuAbout: TMenuItem
        Caption = '&About...'
        OnClick = MnuAboutClick
      end
    end
  end
  object SaveDialog: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly]
    Left = 216
    Top = 240
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Options = [fdAnsiOnly]
    Left = 216
    Top = 296
  end
  object OpenPictureDialog: TOpenPictureDialog
    Filter = 
      'All (*.bmp;*.emf;*.wmf;*.jpg;*.jpeg;*.png)|*.bmp;*.emf;*.wmf;*.j' +
      'pg;*.jpeg;*.png|Bitmaps (*.bmp)|*.bmp|Enhanced Metafiles (*.emf)' +
      '|*.emf|Metafiles (*.wmf)|*.wmf|JPEG Files (*.jpg)|*.jpg|PNG File' +
      's (*.png)|*.png'
    Options = [ofHideReadOnly, ofFileMustExist]
    Title = 'Open a Backdrop Map'
    Left = 216
    Top = 184
  end
  object TablePopupMenu: TPopupMenu
    Left = 312
    Top = 296
    object PopupTableByObject: TMenuItem
      Caption = 'by &Object'
      OnClick = MnuTableByObjectClick
    end
    object PopupTableByVariable: TMenuItem
      Caption = 'by &Variable'
      OnClick = MnuTableByVariableClick
    end
  end
  object AutoLengthMnu: TPopupMenu
    AutoHotkeys = maManual
    Left = 312
    Top = 128
    object AutoLengthOnMnu: TMenuItem
      Caption = 'Auto-Length: On'
      OnClick = AutoLengthOnMnuClick
    end
    object AutoLengthOffMnu: TMenuItem
      Caption = 'Auto-Length: Off'
      OnClick = AutoLengthOffMnuClick
    end
  end
  object FlowUnitsMnu: TPopupMenu
    AutoHotkeys = maManual
    Left = 312
    Top = 184
    object CFSMnuItem: TMenuItem
      Caption = 'CFS'
      OnClick = FlowUnitsMnuItemClick
    end
    object GPMMnuItem: TMenuItem
      Caption = 'GPM'
      OnClick = FlowUnitsMnuItemClick
    end
    object MGDMnuItem: TMenuItem
      Caption = 'MGD'
      OnClick = FlowUnitsMnuItemClick
    end
    object N20: TMenuItem
      Caption = '-'
    end
    object CMSMnuItem: TMenuItem
      Caption = 'CMS'
      OnClick = FlowUnitsMnuItemClick
    end
    object LPSMnuItem: TMenuItem
      Caption = 'LPS'
      OnClick = FlowUnitsMnuItemClick
    end
    object MLDMnuItem: TMenuItem
      Caption = 'MLD'
      OnClick = FlowUnitsMnuItemClick
    end
  end
  object OffsetsMnu: TPopupMenu
    AutoHotkeys = maManual
    Left = 312
    Top = 240
    object OffsetsMnuDepth: TMenuItem
      Caption = 'Depth Offsets'
      OnClick = OffsetsMnuItemClick
    end
    object OffsetsMnuElev: TMenuItem
      Tag = 1
      Caption = 'Elevation Offsets'
      OnClick = OffsetsMnuItemClick
    end
  end
  object thePrinter: TPrintControl
    Left = 400
    Top = 352
  end
  object ReportPopupMenu: TPopupMenu
    Left = 312
    Top = 352
    object PopupReportStatus: TMenuItem
      Caption = '&Status Report'
      OnClick = MnuReportStatusClick
    end
    object PopupReportSummary: TMenuItem
      Caption = 'S&ummary Results'
      OnClick = MnuReportSummaryClick
    end
  end
  object ApplicationEvents1: TApplicationEvents
    OnMinimize = ApplicationEvents1Minimize
    OnRestore = ApplicationEvents1Restore
    Left = 512
    Top = 352
  end
  object PageSetupDialog: TPageSetupDialog
    BoldFont = False
    HelpContext = 0
    Left = 216
    Top = 352
  end
  object BtnImageCollection: TImageCollection
    Images = <
      item
        Name = 'StdToolbar\Item1'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000464944415478DA63FCCFF09F8112C008338011C4C40380EA18091A00
              04D86D6164C46908D10680E4C0349A2124B9009B77883200C335A3060C6B0308
              EA8602AC06900B28360000F85E6AF1B6DC41EF0000000049454E44AE426082}
          end>
      end
      item
        Name = 'StdToolbar\Item2'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000734944415478DABD93510A002108057D37EFE66E864A41ABE5C2FA53
              9133BC8AC0C4F4A5702280B469F57E5C09049E215BFB58398225724114313B8E
              3433AB6F6C07F84EAE02B6E9EB886E0E0414C296EC3801C25B481208DC5A0E4A
              CF748935784950815D601BB7F02238A9ED2BFCF21BA37A0074EA5FF2B1FA52A3
              0000000049454E44AE426082}
          end>
      end
      item
        Name = 'StdToolbar\Item3'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000005A4944415478DA63FCCFF09F8112C008328011449101807A19E10634
              34303080F0810307F06A72707000AB01D1641B40B10B606A87A3012000A37101
              6475580D201690ED02981A920D80850DD961405B0340809C0C856200B90000A4
              7791F17030F3370000000049454E44AE426082}
          end>
      end
      item
        Name = 'StdToolbar\Item4'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000008B4944415478DAA5925D1280200884E56472743C59092543FEA2ED4B
              0D23DFAE205CE10A7F043D007079A1DC075D003767CD5D01D6003ED4B8BEF5D2
              DC0076DD8780997B3D070578DD892820A27E05E0993AAB6E7E8C73122E78649B
              35815C6553C55001A304B65E225B7719E20C1023EA7F4AF401E9164EAFA009CA
              163C83ACE3CB16EC3BF02618BEC413DD39E17CF5C0682CAA0000000049454E44
              AE426082}
          end>
      end
      item
        Name = 'StdToolbar\Item5'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000784944415478DAA592810AC0200844BD2FAFBEDC59845BD3A6A38328
              2E7A774260623A1164B90401230D10AD2690866C01DD1B7B005180A6EAB9BA0F
              980A5C8021A3C94531DE1B621ADCAA2EA07B639F90B0C13A5A33E3FC6A60029E
              80A8C1923CDB28C0CF4836D87D6550E354830870D480027D02B23A065C406F74
              F1B639E9590000000049454E44AE426082}
          end>
      end
      item
        Name = 'StdToolbar\Item6'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000006D4944415478DAB593DB0E80200C43D7FFFF68042364C3168CCE3E10
              28CBD9858062C5BE081E8076BC547DEC7C0AA832003700F3732BF0413E58F914
              D0CB9C01CC0F009665A70EC1B96F8BC8CEEE62CC5F8010F804A0FA5CCD670056
              531ED396AF9401504F3403680BA9BFF18D0ECF0A83F1A2245EE4000000004945
              4E44AE426082}
          end>
      end
      item
        Name = 'StdToolbar\Item7'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000006C4944415478DAB593510EC0200843E9FD0FDD4D4D3F8648646E7C68
              4CECB355018D7652D805C0C6C67B009AAC026862F649EB0900472132808B200B
              12811E1201E4E20B80BF832720B31F00F6C58183D97A065900B8382D7D85FA3F
              2845F80D7014A1DC0B82747F6F9A29AB0BB74159FB45AF3B930000000049454E
              44AE426082}
          end>
      end
      item
        Name = 'StdToolbar\Item8'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000984944415478DAAD93490EC0200845E1E4E2C9A9432032B930852E9A
              A6EFE5E3800C0CBE10307C1CFF212485A7404166A0DE818E9E4F2652C18279BF
              7BD83491912C81C21D6BB09058C12C91B416E69564F351C11ECBC1D243A250B3
              09456204D7D98B31EE09929624EF09646D061A13240B96C2A720EC825432CE8A
              7FC0F936160914CE04FE24565BEAE1EB5D08870AF17E177EB98D2FF501B3BBC5
              F167850F4B0000000049454E44AE426082}
          end>
      end
      item
        Name = 'StdToolbar\Item9'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000007D4944415478DAA5915112C0100C44774F2E3757B4A32A1131CD8F0C
              F31E59CCC8F853B404ACDB9B2A1C5D41A9D6A19FDF3D49880892245BF0C2F880
              AD9B60478007625F0BAB6025388517020D5BE1298105475EF211A8AF1AC60965
              B0CA229C81954538037DFBF81B888F30675165C7238C2FF1E0ADA0AE1EEC0AA2
              750189667FF1F71FE6360000000049454E44AE426082}
          end>
      end
      item
        Name = 'StdToolbar\Item10'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000003E4944415478DA63FCCFF09F8112C048350318414C1201502F238A01
              2001A26D86AAC730801897C0D4D1DF05C8160C8C0B46C3801E6140AC66149750
              9A1B0168C476F1EA3E1BC70000000049454E44AE426082}
          end>
      end
      item
        Name = 'StdToolbar\Item11'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000704944415478DACDD0410EC020080440F6FF8FA621150B0A1A38D51B
              12471610113331A879F00F408B0802E1F8C10BF030800DAA01F31613E901669A
              2C9AE239B040D97EBE25668800D233B11CC0E3BD1421A440308D03ACEA200B04
              FBD9800DBAC44B8174A22A70842A40087580157A0056135AFBFFB3FD39000000
              0049454E44AE426082}
          end>
      end
      item
        Name = 'StdToolbar\Item12'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000A14944415478DAA5525116C320080B27174ECEA21BCA6A756BCB87C0
              03F242441C8E2726A594866066607C1D20180843C6720B408B7A6510007C0886
              2D58F49C32F80590EB9306664AAFDD8729EBFAD1A8D69CF3359F18746A5F2CD8
              D6D8091F6FC308B65903B4E2A03E400220836D7F613370D067D220763DFAD83D
              EBB26030F67DB7AC7439BD03008B635A7DEDF34BCC1ADCB1CEA0AEA1A672D54F
              00FDF2FECC5F0CC8BD6CD319F01E0000000049454E44AE426082}
          end>
      end
      item
        Name = 'StdToolbar\Item13'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000644944415478DAB593510EC0200843E9C9C5933334F1C36DA860E0C7
              4A487D6904424237856650B90A33AB12840CBA501936581168438D09E6FD8FE0
              3D64F542044719EC08269D42E0C9E143B04B3DFF1F8C17ADD07209BCD5890741
              33295CE03D71BBCE0F4CEA97F141F8EA3E0000000049454E44AE426082}
          end>
      end
      item
        Name = 'StdToolbar\Item14'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000004E4944415478DA63FCCFF09F8112C048B101404C91096003F6EFDF0F
              E6383A3A10A569FFFE0350F58E301790EB0846880140409E76462403401C0608
              87201B993FEA82E1E302B274C30CA134370200388A64FE2F4728370000000049
              454E44AE426082}
          end>
      end
      item
        Name = 'StdToolbar\Item15'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000554944415478DA63FCCFF09F8112C008338011C4241200F530621880
              6C08B20214DBB0C833A27B811843F01A40C810A20C20C625040D20D6104642D1
              48304C68E6028AC280A258A0281D509412A99217C801141B000096BA58F16575
              607E0000000049454E44AE426082}
          end>
      end
      item
        Name = 'StdToolbar\Item16'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000854944415478DAD5934B0EC0200844E1661EADBD1937B3104B3A2A98
              7E5665A1129C17062357AAF4257805602B07A11A66DAEB12606211096BA5145D
              B7BC0317DBC51C22E75D9A0928CE201D40A325EA2C128F90CE8203503CE6BE87
              434400B5E49A450035713F2FB010BE8689C0E263C004CB0068C1CF18AF2DFCA4
              835B6AEF64047CFDCE077C078DF7FD1DD3900000000049454E44AE426082}
          end>
      end
      item
        Name = 'StdToolbar\Item17'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000002614944415478DAA5935F485A511CC7BFD7589A932048185347A9B818
              AE17875B6B44303670E47AE929D69099B5C9CA8784F9B482925134D8A30E1A41
              C33DC450A8087A689094AE11D583212968C41C947F9AE8BDA1E9D9BDC26097EE
              DBCE793A9C2F9FC3F97D7E3F8A80E07F16F51730333D4332990CA45229C627C6
              29A1B063D4418E8F8F2197CBE1FDE8A57800E79893180C0604834164B359C1D7
              689A86D96C46381CC6DCA7393E60E4F50891482428140A68E87B0B89488C5285
              A054055E347BA0BC0A58C64250A954884422D8086EF00153935344AFD7636767
              076B0FEB60A87F0E71450DA6023C92F96196AF6178E21C3D3D3D08040258F8BC
              C007B8DEB8486363238AC5227E3C89E0177D813B75EF21AADC441D1BB9268AE3
              68611262B118A9540ACB2BCB97012D2D2D482693F8D917C50D990467A55314E8
              76E47F3F468E9643F9F5035A5B5BB1BBBB2B0CE08AB8BDBD8D5BA355D453525C
              B0BB583965530CAA5419412785AEAE2EACAEAE5E06D85FD96B45641806B95C4E
              D0422C168342A1403A9DC656688B0FE01C1B8DC69A2251FF2C1A580B65B68025
              F6DADAEC855246C16A5F41777737969696B0FE6D9D0F18B20D11AD568B783C8E
              FDFEEB82167A5F26A1D3E96ADF0C85437CC0C0B301D2D4D4847C3E8F94ED4CD0
              C2E6BBE15AA772DDB8B7BFC7070C5A0789C964C2E2E222AE38CF052D94671DB0
              582C989F9FC7E6D6261FD079BF93B4B5B5E1E0E000B62F77590B0D28FF6B813D
              4D3FF80EEE119FCF079AA1F9808E7B1DC4E572C1ED7643AD560B5AE0E6C4E3F1
              C06AB5E2E4F4840FE87DDA4B128904341A0DFC01BFE034B6DF6E27D168145CC3
              1DC60E6B993FAF903600A133F7640000000049454E44AE426082}
          end>
      end
      item
        Name = 'StdToolbar\Item18'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000005C4944415478DACDD2410A00210840D1FEFD0F6DB6686158A96D1A08
              82E2259F419AB4970F5D61411FC305C60157883DF03E81DC0DE00C8C0B95490C
              5069916AE04E30FF03745B69E102991E5B20DAC300D9166682D58DF6F8188836
              E835FC54F8652E50100000000049454E44AE426082}
          end>
      end
      item
        Name = 'MapToolbar\Item19'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000544944415478DABDD2C10A0020080350F7FF1F6D790B52B788DA4510
              7D07116E6E6B60F0D9838941064455911250911650100A3044023A44062AE408
              C89077378825E527C03E91211BB00DDC020CF903B4F82D3000C6C34FF14511DE
              5A0000000049454E44AE426082}
          end>
      end
      item
        Name = 'MapToolbar\Item20'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000614944415478DAB5924B0A00210C439BFB1FBA7E6060446D52C56E84
              421F3109DCDCFE0383D71D4C1CAC00ED5521D829509520FA820201F380412880
              412440B8CFC4B85497EDC1E4D1499186A4941476A0DE97673DB86AE2278F1D1F
              9B1802B25300C2336DF1F2902A320000000049454E44AE426082}
          end>
      end
      item
        Name = 'MapToolbar\Item21'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000544944415478DA63FCCFF09F8112C048750318414250009463C4AA09
              490D5E03E8EF0290042E4DB8D430822D2260233E0B081A40C84B941B000A037C
              7EA7D8008261408A0114C702366FA118400EA0DC0594E64600CF5B4FF730D51D
              BD0000000049454E44AE426082}
          end>
      end
      item
        Name = 'MapToolbar\Item22'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000005E4944415478DAB5913B0E00200843E1FE874619340EE5D310596A6C
              785454139349E957806E7BFB4A03BCD1D59BDF730B70A62245200AF02A045471
              D15E2E209A4003CA6FCB0055FC14D049817C6A8925204B11DE2340F4841680AD
              316001340E73F13DAFE3570000000049454E44AE426082}
          end>
      end
      item
        Name = 'MapToolbar\Item23'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000614944415478DABDD35B12001008055077FF8B465F425D798CFE1467
              92819C72BA096800B21CA2D61102E4B0B5D9CB77C07213A94B722A5A07FC0E5F
              026C887F3A3802A2AFA0AF3301BCC536170AB0213204D1BFE02161C043B6000B
              D9063422400183566FF15CB75B260000000049454E44AE426082}
          end>
      end
      item
        Name = 'MapToolbar\Item24'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000604944415478DABDD35B12001008055077FF8B8EBEE4D195C7E88F72
              26199024E9266001E8B28B924708D0C3B3626FBF01964524AF9BB26A9377F812
              6043FCD3C111107D057B9D01E02DD6B950800D912188FE050F09031EB205CC90
              6DC0220A64399363F1C29130780000000049454E44AE426082}
          end>
      end
      item
        Name = 'MapToolbar\Item25'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000574944415478DAED923B0E00200843E9FD0FED2F0CD0800EE8661742
              A48FC680264D2AC2046016D5E8B135D0AC039CCC0C7180D5683D99ED2CF80F6C
              C448BC20046429A2B70F7805908D4240F990AE9C721631333B40451D5E4E79F1
              4BE5505B0000000049454E44AE426082}
          end>
      end
      item
        Name = 'MapToolbar\Item26'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000724944415478DAAD92DB0EC0200843E99FE397BB41B2C539D0EAD617
              BCC0911451A54A2408EE8B3307920811C08ADBA27EBF041815A700E6E517204A
              66CE1C7019C602DA5CF83A506F6296F3BD8391078CB1C329304A3D60E41E588C
              3ECD6C6D5155E501586BFD4FC0AE070EB029142DDB9003D3B97176FB06AACB00
              00000049454E44AE426082}
          end>
      end
      item
        Name = 'ObjectToolbar\Item27'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000714944415478DABD925D0E80200C83E9CD767439193A13C8184B0589
              F691AC1FDD0F4A2A6947F000E853A0AB0E8F806AD6E27CE40E242221B0019839
              9202B5F606A879D668D36C01BA1656E2DBDF8719CC02EC46C22D30F975821D52
              4DC58021C01B19089F9C729BF0DB165666F14F02A613FF6573F10FC7D35C0000
              000049454E44AE426082}
          end>
      end
      item
        Name = 'ObjectToolbar\Item28'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000944944415478DAA5930B0E84300844E1E4C0C9D169C4D0A2BBA824A6
              D8F40D4C3FECE4F42518028CE145EC2C9F02AA4AF8DC9DCC6C2C101536358F1C
              EB008DCA473E7580090043E8C8338C79448C934054EEC0035C3B382DFC818B50
              16B8F2BC0261B15A785AF9CA4217CE1B5D2CDCC1EBB1168168AB03E73B31DD83
              5FADAEF0ED2676E07CE4C542071611E2FDAF5878FD98BEC40606D61400A1F68B
              060000000049454E44AE426082}
          end>
      end
      item
        Name = 'ObjectToolbar\Item29'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000004B4944415478DA63FCCFF09F8112C04813031841C25800502D234103
              409AB129C4258762003ECDB8D4C00D2046333643460DA0A601C41A82331A8931
              846042425688CD00A29232A980620300FF0F61F186E9FC0D0000000049454E44
              AE426082}
          end>
      end
      item
        Name = 'ObjectToolbar\Item30'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000004C4944415478DA63FCCFF09F8112C048350318414C2201500F235603
              902570DA88A60EC50B840CC1264F5D03F01982539CEA06600D283C5EA38D01C8
              9A08062CCD0C20E474FA18400CA0D80000DAED5EF19BDC500F0000000049454E
              44AE426082}
          end>
      end
      item
        Name = 'ObjectToolbar\Item31'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000005B4944415478DAAD92490E00200803E1FF8F563C680CB20970B47652
              2B38604065D00220C9A4630AB0CD1E44047093057900DA65F5FC06B87105FD00
              228589CF5B80A85982F42468E9A0E517D4983F7BC04DA94D8C76E2022253064C
              E73967F11DDDED020000000049454E44AE426082}
          end>
      end
      item
        Name = 'ObjectToolbar\Item32'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000404944415478DA63FCCFF09F8112C0481503188114906624492354CF
              2032801CFF0F322FC00CC0E51D740BE8130BD85C83ECD2411688A46846F60E75
              3213250000E72F67F1603EB3800000000049454E44AE426082}
          end>
      end
      item
        Name = 'ObjectToolbar\Item33'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000002F4944415478DA63FCCFF09F8112C0386A00C200462013C866245A23
              543D750D20D5F9D477014506900B460DA0820100647E3AF137488FE200000000
              49454E44AE426082}
          end>
      end
      item
        Name = 'ObjectToolbar\Item34'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000004B4944415478DA63FCCFF09F8112C0487503184142E41A00D20CE433
              12AD19A81E6E00A99A510C204733C906E00A1B920CC0A6669018402816F0CA11
              4A07845C47302512F4DAE0CB4CA402004E5651F626EB0B810000000049454E44
              AE426082}
          end>
      end
      item
        Name = 'ObjectToolbar\Item35'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000514944415478DA63FCCFF09F8112C048350318414C2201500F235603
              902570DA88A60EA701C82E421727680036C370CB3130A0D884CB2B307174970D
              0203280E039AC402D9E9809066AC2E1A3C99895C0000B02379F1E4087C550000
              000049454E44AE426082}
          end>
      end
      item
        Name = 'ObjectToolbar\Item36'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000554944415478DA63FCCFF09F8112C038780C600432816C46821AD0D4
              51D700108DCF106C6A18C17C280049E072094C1C6608423D9A01589D89C427CA
              00741BD1C5510CC01788C4880DC26824066035805C30F0060000A64767F19266
              8A840000000049454E44AE426082}
          end>
      end
      item
        Name = 'ObjectToolbar\Item37'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000005A4944415478DA63FCCFF09F8112C048350318414C2201500F234E03
              9025316CC3A206C50090048CC6A6199B3C8601E86CC2720C8850C4A6099B61C8
              EA711A80CD36920C20DA05148701556381E274C04024C06A00B960E00D000027
              907CF1D8E66F3A0000000049454E44AE426082}
          end>
      end
      item
        Name = 'ObjectToolbar\Item38'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000384944415478DA63FCCFF09F8112C048550318415C2200500F235E03
              600A08F1097A019B0692C260D480216F007AAAC4650875F3C280180000629A46
              F103D3C3D30000000049454E44AE426082}
          end>
      end>
    Left = 512
    Top = 128
  end
  object BtnImageList: TVirtualImageList
    AutoFill = True
    DisabledOpacity = 100
    DisabledGrayscale = False
    DisabledSuffix = '_Disabled'
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'StdToolbar\Item1'
        Disabled = False
        Name = 'StdToolbar\Item1'
      end
      item
        CollectionIndex = 1
        CollectionName = 'StdToolbar\Item2'
        Disabled = False
        Name = 'StdToolbar\Item2'
      end
      item
        CollectionIndex = 2
        CollectionName = 'StdToolbar\Item3'
        Disabled = False
        Name = 'StdToolbar\Item3'
      end
      item
        CollectionIndex = 3
        CollectionName = 'StdToolbar\Item4'
        Disabled = False
        Name = 'StdToolbar\Item4'
      end
      item
        CollectionIndex = 4
        CollectionName = 'StdToolbar\Item5'
        Disabled = False
        Name = 'StdToolbar\Item5'
      end
      item
        CollectionIndex = 5
        CollectionName = 'StdToolbar\Item6'
        Disabled = False
        Name = 'StdToolbar\Item6'
      end
      item
        CollectionIndex = 6
        CollectionName = 'StdToolbar\Item7'
        Disabled = False
        Name = 'StdToolbar\Item7'
      end
      item
        CollectionIndex = 7
        CollectionName = 'StdToolbar\Item8'
        Disabled = False
        Name = 'StdToolbar\Item8'
      end
      item
        CollectionIndex = 8
        CollectionName = 'StdToolbar\Item9'
        Disabled = False
        Name = 'StdToolbar\Item9'
      end
      item
        CollectionIndex = 9
        CollectionName = 'StdToolbar\Item10'
        Disabled = False
        Name = 'StdToolbar\Item10'
      end
      item
        CollectionIndex = 10
        CollectionName = 'StdToolbar\Item11'
        Disabled = False
        Name = 'StdToolbar\Item11'
      end
      item
        CollectionIndex = 11
        CollectionName = 'StdToolbar\Item12'
        Disabled = False
        Name = 'StdToolbar\Item12'
      end
      item
        CollectionIndex = 12
        CollectionName = 'StdToolbar\Item13'
        Disabled = False
        Name = 'StdToolbar\Item13'
      end
      item
        CollectionIndex = 13
        CollectionName = 'StdToolbar\Item14'
        Disabled = False
        Name = 'StdToolbar\Item14'
      end
      item
        CollectionIndex = 14
        CollectionName = 'StdToolbar\Item15'
        Disabled = False
        Name = 'StdToolbar\Item15'
      end
      item
        CollectionIndex = 15
        CollectionName = 'StdToolbar\Item16'
        Disabled = False
        Name = 'StdToolbar\Item16'
      end
      item
        CollectionIndex = 16
        CollectionName = 'StdToolbar\Item17'
        Disabled = False
        Name = 'StdToolbar\Item17'
      end
      item
        CollectionIndex = 17
        CollectionName = 'StdToolbar\Item18'
        Disabled = False
        Name = 'StdToolbar\Item18'
      end
      item
        CollectionIndex = 18
        CollectionName = 'MapToolbar\Item19'
        Disabled = False
        Name = 'MapToolbar\Item19'
      end
      item
        CollectionIndex = 19
        CollectionName = 'MapToolbar\Item20'
        Disabled = False
        Name = 'MapToolbar\Item20'
      end
      item
        CollectionIndex = 20
        CollectionName = 'MapToolbar\Item21'
        Disabled = False
        Name = 'MapToolbar\Item21'
      end
      item
        CollectionIndex = 21
        CollectionName = 'MapToolbar\Item22'
        Disabled = False
        Name = 'MapToolbar\Item22'
      end
      item
        CollectionIndex = 22
        CollectionName = 'MapToolbar\Item23'
        Disabled = False
        Name = 'MapToolbar\Item23'
      end
      item
        CollectionIndex = 23
        CollectionName = 'MapToolbar\Item24'
        Disabled = False
        Name = 'MapToolbar\Item24'
      end
      item
        CollectionIndex = 24
        CollectionName = 'MapToolbar\Item25'
        Disabled = False
        Name = 'MapToolbar\Item25'
      end
      item
        CollectionIndex = 25
        CollectionName = 'MapToolbar\Item26'
        Disabled = False
        Name = 'MapToolbar\Item26'
      end
      item
        CollectionIndex = 26
        CollectionName = 'ObjectToolbar\Item27'
        Disabled = False
        Name = 'ObjectToolbar\Item27'
      end
      item
        CollectionIndex = 27
        CollectionName = 'ObjectToolbar\Item28'
        Disabled = False
        Name = 'ObjectToolbar\Item28'
      end
      item
        CollectionIndex = 28
        CollectionName = 'ObjectToolbar\Item29'
        Disabled = False
        Name = 'ObjectToolbar\Item29'
      end
      item
        CollectionIndex = 29
        CollectionName = 'ObjectToolbar\Item30'
        Disabled = False
        Name = 'ObjectToolbar\Item30'
      end
      item
        CollectionIndex = 30
        CollectionName = 'ObjectToolbar\Item31'
        Disabled = False
        Name = 'ObjectToolbar\Item31'
      end
      item
        CollectionIndex = 31
        CollectionName = 'ObjectToolbar\Item32'
        Disabled = False
        Name = 'ObjectToolbar\Item32'
      end
      item
        CollectionIndex = 32
        CollectionName = 'ObjectToolbar\Item33'
        Disabled = False
        Name = 'ObjectToolbar\Item33'
      end
      item
        CollectionIndex = 33
        CollectionName = 'ObjectToolbar\Item34'
        Disabled = False
        Name = 'ObjectToolbar\Item34'
      end
      item
        CollectionIndex = 34
        CollectionName = 'ObjectToolbar\Item35'
        Disabled = False
        Name = 'ObjectToolbar\Item35'
      end
      item
        CollectionIndex = 35
        CollectionName = 'ObjectToolbar\Item36'
        Disabled = False
        Name = 'ObjectToolbar\Item36'
      end
      item
        CollectionIndex = 36
        CollectionName = 'ObjectToolbar\Item37'
        Disabled = False
        Name = 'ObjectToolbar\Item37'
      end
      item
        CollectionIndex = 37
        CollectionName = 'ObjectToolbar\Item38'
        Disabled = False
        Name = 'ObjectToolbar\Item38'
      end>
    ImageCollection = BtnImageCollection
    Left = 400
    Top = 128
  end
  object ProjectImageCollection: TImageCollection
    Images = <
      item
        Name = 'green_plus'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000544944415478DA63FCCFF09F8112C038B80D606C61844BFEAFF9CF48
              9E01D540462BB906C0A4493100D9D96000750118D400710BAA61D80DA8C6136A
              ADC41880CB05580294C681481503B0389B68038801C3C00000E0A64BF1E18955
              680000000049454E44AE426082}
          end>
      end
      item
        Name = 'red_minus'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000002F4944415478DA63FCCFF09F8112C0386AC06034A011244400D433FC
              67C46B403D1ECD8DC41840910B063E1047A001009A3129F120F5B75B00000000
              49454E44AE426082}
          end>
      end
      item
        Name = 'edit'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000007D4944415478DAA592510EC0200843E9FD0FCD5037250242263F2486
              3E280826A69B40150029955AFC020C71CB443B240574B19400569C0232F111A0
              C53A4A1676715F008D2DCCB71764004D1C59625E1017303AAF7D4719423080C8
              B3EEAC6DB880CF5B368999C0FB657A17A509A24BEC13A457C82E322D560095B8
              063CA1B979F149D5929F0000000049454E44AE426082}
          end>
      end
      item
        Name = 'uparrow1'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000004F4944415478DA63FCCFF09F8112C038020C60042A01AA6124CB0046
              B034509E9191019721380D806B860B603704AB01189AF11882610058330CA0B9
              002E8C64086DBC30880C40032419402C187803001F6248F134F9829400000000
              49454E44AE426082}
          end>
      end
      item
        Name = 'dnarrow1'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000484944415478DA63FCCFF09F8112C0388C0D600449A101A05A46D20C
              F88F24C7C838540C40F13B9A01D8C202AB0B306CC7E30ADA85018A2138341334
              0066082ECD441940080CBC01001F6248F1A1246BAF0000000049454E44AE4260
              82}
          end>
      end
      item
        Name = 'sort'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000914944415478DAAD92810A80300844CF2F9FFB724BC2BA865A5007D1
              C6E6F3D489C1C0124C330C412181ECE7769E0B03A64E5305FC1B9A435A80678F
              75E5A20444760F7450E5A20470F6CE450AE0EC5C4EE62205ACB577BD689BF846
              7909FE4B34E8E223802F07F0358055054BE2D241F797D8643E26A5E7DED73EA1
              EB2135C1998B28E3D6C4B50F2B2C5C44F6FFA6F0451BFEDA96F10215BE200000
              000049454E44AE426082}
          end>
      end
      item
        Name = 'lock_open'
        SourceImages = <
          item
            Image.Data = {
              47494638396110001000F700000000005231006B4A006B73736B7B8C73737B73
              7B7B7B52007B848C7B8C9C8C8C8C8C8C948C94948C949C94949494949C949C9C
              9C9CA59CA5A5A56B10AD6B10AD7310B57310B57B00B5B5BDB5BDCEBD7310BD7B
              10BDBDBDBDBDC6C67B10C68418CE8418CE8C00CE8C18CECED6CED6D6D68C00D6
              8C10D68C18D68C21D69400D69408D69C10DE8408DE8C10DE9C10E7E7E7E7EFEF
              F7BD42F7C64AF7C652F7CE63F7D66BF7DE84FFBD39FFBD42FFC642FFC64AFFD6
              5AFFD66BFFDE84FFE78CFFEFA5FFEFADFFF7B5FFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFF21F90401000000002C00000000100010000008
              A50001081418A1A0C10803130278C0E185C3171C204050088001861111162C88
              300203038A0A220E105880C102031411706840B125810C095A2A4461A2A60914
              3873A24808E207909F407FFE00C1D34688102A2E5C50717468C20F3562E47011
              20808B1C317C7C784A43850A170204B860DA6343C20D33A4BA3870E06A0C1E1A
              125A90E175850B172BBCEEB02057EAD4BB5773E8E03B7042050A142A283E9C78
              82CCC7000202003B}
          end>
      end
      item
        Name = 'lock_closed'
        SourceImages = <
          item
            Image.Data = {
              47494638396110001000F7000000000080000000800080800000008080008000
              8080C0C0C0C0DCC0A6CAF0402000602000802000A02000C02000E02000004000
              204000404000604000804000A04000C04000E040000060002060004060006060
              00806000A06000C06000E06000008000208000408000608000808000A08000C0
              8000E0800000A00020A00040A00060A00080A000A0A000C0A000E0A00000C000
              20C00040C00060C00080C000A0C000C0C000E0C00000E00020E00040E00060E0
              0080E000A0E000C0E000E0E000000040200040400040600040800040A00040C0
              0040E00040002040202040402040602040802040A02040C02040E02040004040
              204040404040604040804040A04040C04040E040400060402060404060406060
              40806040A06040C06040E06040008040208040408040608040808040A08040C0
              8040E0804000A04020A04040A04060A04080A040A0A040C0A040E0A04000C040
              20C04040C04060C04080C040A0C040C0C040E0C04000E04020E04040E04060E0
              4080E040A0E040C0E040E0E040000080200080400080600080800080A00080C0
              0080E00080002080202080402080602080802080A02080C02080E02080004080
              204080404080604080804080A04080C04080E040800060802060804060806060
              80806080A06080C06080E06080008080208080408080608080808080A08080C0
              8080E0808000A08020A08040A08060A08080A080A0A080C0A080E0A08000C080
              20C08040C08060C08080C080A0C080C0C080E0C08000E08020E08040E08060E0
              8080E080A0E080C0E080E0E0800000C02000C04000C06000C08000C0A000C0C0
              00C0E000C00020C02020C04020C06020C08020C0A020C0C020C0E020C00040C0
              2040C04040C06040C08040C0A040C0C040C0E040C00060C02060C04060C06060
              C08060C0A060C0C060C0E060C00080C02080C04080C06080C08080C0A080C0C0
              80C0E080C000A0C020A0C040A0C060A0C080A0C0A0A0C0C0A0C0E0A0C000C0C0
              20C0C040C0C060C0C080C0C0A0C0C0FFFBF0A0A0A4808080FF000000FF00FFFF
              000000FFFF00FF00FFFFFFFFFF21F90401000000002C00000000100010000008
              830001081478AFA0C18108091EB0C7D0DE817B09153E2445EADE818711493D1C
              25B0E2015219310ED40831E1A88F084F824478A2A5CB972D119AF845B3A64D13
              327F9930F16267CF993807CEBC73E7C584092F88EA9479EB85D3A34E81CA245A
              940387A477960A2DEAB4ABD33F41059AA0EA956A58001D4A74485BA2ED5AB511
              E3260C08003B}
          end>
      end
      item
        Name = 'raingage'
        SourceImages = <
          item
            Image.Data = {
              424D460500000000000036000000280000001200000012000000010020000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
              FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
              FF000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFF
              FF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
              FF00FFFFFF00FFFFFF000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
              FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFF
              FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFFFF00FFFF
              FF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF0000000000FFFFFF00FFFF
              FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
              0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
              FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
              FF000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
              0000FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFF
              FF00FFFFFF00FFFFFF000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
              FF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFF
              FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFFFF00FFFF
              FF00FFFFFF000000000000000000000000000000000000000000000000000000
              00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
              0000FFFFFF00FFFFFF0000000000C0C0C000FFFFFF00C0C0C000FFFFFF00C0C0
              C000FFFFFF00C0C0C000FFFFFF00C0C0C00000000000FFFFFF00FFFFFF00FFFF
              FF000000000000000000FFFFFF000000000000000000FFFFFF00C0C0C000FFFF
              FF00C0C0C000FFFFFF00C0C0C000FFFFFF00C0C0C000FFFFFF00000000000000
              0000FFFFFF00FFFFFF000000000000000000FFFFFF0000000000FFFFFF00C0C0
              C000FFFFFF00C0C0C000FFFFFF00C0C0C000FFFFFF00C0C0C000FFFFFF00C0C0
              C000FFFFFF0000000000FFFFFF00FFFFFF000000000000000000FFFFFF000000
              0000C0C0C000FFFFFF00C0C0C000FFFFFF00C0C0C000FFFFFF00C0C0C000FFFF
              FF00C0C0C000FFFFFF00C0C0C0000000000000000000FFFFFF00000000000000
              0000FFFFFF00FFFFFF000000000000000000FFFFFF00C0C0C000FFFFFF00C0C0
              C000FFFFFF00C0C0C000FFFFFF00C0C0C000FFFFFF00C0C0C00000000000FFFF
              FF000000000000000000FFFFFF00FFFFFF00FFFFFF000000000000000000FFFF
              FF00C0C0C000FFFFFF00C0C0C000FFFFFF000000000000000000000000000000
              0000FFFFFF00FFFFFF000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
              FF00000000000000000000000000000000000000000000000000FFFFFF00FFFF
              FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFFFF00FFFF
              FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
              FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              000000000000}
          end>
      end
      item
        Name = 'blue_plus'
        SourceImages = <
          item
            Image.Data = {
              4749463839610D000D00B30000000000BF000000BF00BFBF000000BFBF00BF00
              BFBFC0C0C0808080FF000000FF00FFFF000000FFFF00FF00FFFFFFFFFF21F904
              01000007002C000000000D000D00000420F0C849ABBD13682C0103DCE18196A6
              7D26E5316C4B7626BA955F38DA73A86311003B}
          end>
      end
      item
        Name = 'blue_minus'
        SourceImages = <
          item
            Image.Data = {
              4749463839610D000D00B30000000000BF000000BF00BFBF000000BFBF00BF00
              BFBFC0C0C0808080FF000000FF00FFFF000000FFFF00FF00FFFFFFFFFF21F904
              01000007002C000000000D000D00000417F0C849ABBD38EB7D80FF1F053064C9
              002208726CEBB211003B}
          end>
      end
      item
        Name = 'uparrow2'
        SourceImages = <
          item
            Image.Data = {
              4749463839610D000D00B30000000000BF000000BF00BFBF000000BFBF00BF00
              BFBFC0C0C0808080FF000000FF00FFFF000000FFFF00FF00FFFFFFFFFF21F904
              01000007002C000000000D000D00000422F0C8492B05B60283F3D9069781A178
              9568F7016CC9B29AEB916A1CD6D78B7BBC1701003B}
          end>
      end
      item
        Name = 'dnarrow2'
        SourceImages = <
          item
            Image.Data = {
              4749463839610D000D00B30000000000BF000000BF00BFBF000000BFBF00BF00
              BFBFC0C0C0808080FF000000FF00FFFF000000FFFF00FF00FFFFFFFFFF21F904
              01000007002C000000000D000D00000422F0C849AB0418DB6B3AD807D019DF26
              7A557692D9B48E640AC7D65A9A3418EA7C2F45003B}
          end>
      end
      item
        Name = 'Delete'
        SourceImages = <
          item
            Image.Data = {
              47494638396110001000B30000000000C6C6C6FFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF21F904
              01000001002C000000001000100000042830C849ABBD15001CF4D45EB651E028
              8516787267C9B62657C669888AF10DA735BD66AFE04F388900003B}
          end>
      end
      item
        Name = 'browse'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000012000000120403000000A4E093
              6400000001735247420240C07DC50000000F504C5445000000000000808080FF
              FF00FFFFFFDC68C2B80000000174524E530040E6D86600000009704859730000
              0EC400000EC401952B0E1B0000001974455874536F667477617265004D696372
              6F736F6674204F66666963657FED35710000004F4944415418D36D8CD1098040
              0C437B4ED0A75DE4C0058AFBEF64EF72E28706028F24ADD92B7C41BB12F64967
              A92B2BE527E3489445F47C6E19D90649E869C311911EB3B65A2DAA51A1481EAD
              FCA31B25E209EFF8480C5A0000000049454E44AE426082}
          end>
      end>
    Left = 512
    Top = 184
  end
  object ProjectImageList: TVirtualImageList
    AutoFill = True
    DisabledOpacity = 100
    DisabledGrayscale = False
    DisabledSuffix = '_Disabled'
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'green_plus'
        Disabled = False
        Name = 'green_plus'
      end
      item
        CollectionIndex = 1
        CollectionName = 'red_minus'
        Disabled = False
        Name = 'red_minus'
      end
      item
        CollectionIndex = 2
        CollectionName = 'edit'
        Disabled = False
        Name = 'edit'
      end
      item
        CollectionIndex = 3
        CollectionName = 'uparrow1'
        Disabled = False
        Name = 'uparrow1'
      end
      item
        CollectionIndex = 4
        CollectionName = 'dnarrow1'
        Disabled = False
        Name = 'dnarrow1'
      end
      item
        CollectionIndex = 5
        CollectionName = 'sort'
        Disabled = False
        Name = 'sort'
      end
      item
        CollectionIndex = 6
        CollectionName = 'lock_open'
        Disabled = False
        Name = 'lock_open'
      end
      item
        CollectionIndex = 7
        CollectionName = 'lock_closed'
        Disabled = False
        Name = 'lock_closed'
      end
      item
        CollectionIndex = 8
        CollectionName = 'raingage'
        Disabled = False
        Name = 'raingage'
      end
      item
        CollectionIndex = 9
        CollectionName = 'blue_plus'
        Disabled = False
        Name = 'blue_plus'
      end
      item
        CollectionIndex = 10
        CollectionName = 'blue_minus'
        Disabled = False
        Name = 'blue_minus'
      end
      item
        CollectionIndex = 11
        CollectionName = 'uparrow2'
        Disabled = False
        Name = 'uparrow2'
      end
      item
        CollectionIndex = 12
        CollectionName = 'dnarrow2'
        Disabled = False
        Name = 'dnarrow2'
      end
      item
        CollectionIndex = 13
        CollectionName = 'Delete'
        Disabled = False
        Name = 'Delete'
      end
      item
        CollectionIndex = 14
        CollectionName = 'browse'
        Disabled = False
        Name = 'browse'
      end>
    ImageCollection = ProjectImageCollection
    Left = 400
    Top = 184
  end
  object RunImageCollection: TImageCollection
    Images = <
      item
        Name = 'Item1'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000002084944415478DA63FCCFF09F8112C088CB80371FBEFC6F597F81E1E2
              670E06863FFF1934F97E32F8B2EC67B8B86336838A913383A16B2C83B2A11323
              4E03787B5EFD77D5166458E7C9CA0813D39F72E3FFD964098603BDEE0C1FD8B4
              1842CAE63332FEFDF7EF7FE4BA5F0C5B6EFD65F805328B8981E13F104FF06067
              C8D167664436F4E09685FF95389E311CDAB68241D7B78A41CF319C91F1DB8F5F
              FF7DD7FF67D813C9C6C84000CCC833FBEF1B1CC5B07ADE248694FEE30C3C42E2
              8C8C0F5E7EF81F7E808BE144382B5E034A0E7EFC6FB9C39541DFCC9261CDCAF5
              0C152B1E81D533EEBAF4FC7FF94D118673A12C780D9875F8D67FF593990CF2DA
              D60C2B162E4018D0BEF3E9FF0A772982CE9FB07CDD7FF7DF2B193E7EF8C070E5
              D15F86949E3D100342577CFCBF2A820FA701D1DB3FFFFFF69F9D21F6CF2606CD
              0FAB196EDD79C220A81DC860175E0231C07EC9CFFF07625003D06CD5AFFF371E
              7F6648947FC390E5ACC670E4DE47866901EA0C8B1A6D198E9DB9C1E091339741
              56CB1C6240EDCA33FF17DDE46678C3220A8CBF7F0CBC8CEF191C789E302CCF73
              821BFAE0F2D1FF7B67643058996B316CDC761CEE7FBC2911191C58DAFEFFD3ED
              6D0CAA4A52E41930ABC8F1BFA12A2F030F372779067444C8FDF7F7B26478F1EA
              3DC3DB7FD2E0244C9601D76E3E6290368D65B008C822CD809D73ABFF9FDFBD18
              9C031D222A18D8B979E1060000C787DFF1AD55EAA70000000049454E44AE4260
              82}
          end>
      end
      item
        Name = 'Item2'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000027F4944415478DAA5D36B4893611407F0FF33DDF6CE39CB4B9AD7D434
              71D245294DC568459A86D85A347548444251411025525154047DE802425F86AB
              75A3A94948264892E1A591261245E9301A2BC7BCCCBD2EDB746E7B7A7925EB8B
              41F67C7BE09CDF73CEE13C8482E27F0E590A307D35D32BFA0BF048A7302B0D80
              C81D01954F0C737F2B52B27622735715D666EE204B022A7D364D8B4A4771D621
              CCCC3BD067EFC4FB6E130C870D7875A308AC488EFD357709F1F9FDF4487D3D6C
              522D578F0F844FA7288EDB874DE90A080522CC78597071081C1843326345579B
              01EB4BCF6283424D886BD6430F6A8FE2B85A03AF7F9E4F160706414884B0B92D
              081186C1EAFE028F2F0473753751AAAA44D39D3A54DF3222382C8A10F3184B6B
              9A3550951CC0A0A39B7F3F469288505138229868EE26C0E49C15DF5917221B1F
              6363762E9E343C45ADC1C2174B5ABAFA69D3D0799415693038D5C3850B102989
              43B8380AAB9858B8BC4E4CCFDB41261824743FC29A8C7C18EEE97F03579B7B68
              CABA11381916961F2610C2014C0C07AC4662701A9C1E07FCF062B8F7330ADD6F
              30CDB2F860F1A1FA7AC70250A16BA5EA42178C13EDF0512F577E12926469080A
              94C139C74D7FB4175EC14AE44F6E86DCD108D3C837846628B14D7D7A01D8A3BB
              4C2B15D1DCB419BE7F710083BEF14EBCB3F623CE9D878A826A7C1A7542AB54E2
              FEA502BC7E3B84DD27748897E72C009ADB27E978C800042BDCE057624608A135
              1ECFCE34925F3B31647C4E7B1FD6222F478E9636E362FF7FDDC43F4F87FE2275
              995F223539667980F6948266A6CA102C952C0FB8569E40CB4A72611B77C0EE8F
              E5577859C0C7610B62B75461EBDE63FF06B4EBCED1C1170FF81FB8BDBC1662A9
              6C11F8092BE50D00279FC9E20000000049454E44AE426082}
          end>
      end
      item
        Name = 'Item3'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000027C4944415478DAA5936D4853511CC69FB3EDCE97CD895908A995E925
              D874A635CD172C0B73049251A044868484989F22C4CACFD5875EA008A2B09430
              662F564463BED1104A419D44A695627127B9B4A59B6DEEAAF79E4E93AC2F06D9
              FFE3E19C1FCFFF799E432828FE67C84A00F7944007EC75080FF4429664B89405
              D04831786DBB85A4F43D482B284362DA6EB222C0715D49E38C0644EAAF41F28F
              607AF00446058A9CA393B05F2AC48C5A8F43357708912489763EAB824E688042
              1641144C1601F8BD75088D2D8642158D45F123533187FE9E296C527F4697D582
              94A23330E69710E2F77B698FAD0CA6D40CF6580DD00528C392A05047439404A8
              4379F83D1DA0D22CEE5FEE42D1C1C37870FB2A2AAE7443BB268610A773888E3B
              2A919C5C88C04453503E17950B55380F12B9E5A74D107DBDF07E73A0CFF205A9
              195978D8FC18B51681044DECEB7D4239D769241ACA3137718FA95041A5DB0E4E
              6B60000316E79D981787E1F168E1B4BDC046430E2C8D0DBF011DB673745B02DB
              9D5D5CF40DB3230538DD56A8B4C950ADCDC3827F1032FD8EB1D1AF508FF4C333
              3383414142C5C58E25406B4B39CD36F208B8987C39C0E4EF0417990165681C7C
              730EB85DCD5028DD987D1B8E1062C287D17144190E20AFE4D412C0FEC84CD38D
              2628B968502A33F3D6B1C88EC33526424E388298941A0C4D4C427C6E46627C11
              5EF5BD83B9BA1EF1FACC25405B4B35D58C3782A33E066021B024BCDA5C141C6B
              27BF3AF1E9CD4BDA79A312D9997A3CB5762FEFFFD726FE39F6A6F3D43B6205BF
              79FDEA00374FE6D3343E025A4DD8EA00174A37D0FDFBB2E09A9C865B8E0D5678
              5580A1F702624D65D8515CF56F80D6FAB374A0FD6EF007EE2AAD4588266219F0
              031FE814000D7C0F920000000049454E44AE426082}
          end>
      end
      item
        Name = 'Item4'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000026F4944415478DAA5935948D4511487BFBFFB36A6518A991A6E95A265
              6166626E592148459956F8205984084145C98444B418A114F522856946A41454
              505399894A650F5269292EE1366E4D536E318E63E3EDA264BD186407EEC33D9C
              DF77EFF9DD731581E07F42990BF05DAF17EF2F9EC5AEA901610913FE2B190BF5
              A0E1E975FCD7241096988E5F58BC322740E7A312AED189581D5583B69BA9FC73
              B44F9AF1AFAAA1BA600BC33641EC3A5EAC285366B3301DDE83B9EA11984C6096
              6AB3C036F73296319BC0D90506FA61D2448DAE155FBB7E6A35658424AB098D4B
              5514D3B841888C646C92D2C0D24A16FE80A55EE0228543DFC0C3131ADEC2B881
              C2C6129277EEE5EE8D2B645EAAC369A1BBA20C7777098783A958EFCB840A0D08
              D9526818787A835F005828D0D6C2485333954A2DABD64572AFFC3E39653DCAB4
              8903D51562D1A91358EDCF86CA2752201D0B0A86657E10B802BEE8A04F4B9B49
              30305A8E4F701465374B7E03FAAEE6090F7B6794CF83D0D3293316E0BF5C2E79
              FAEA70E8EE00A391C64E2DD60E558C0C0FF3B1C74C667EE50C60242B4538476C
              96D77F2C0B27A4682D0487809B3B666D1786CA0A6C8D06EA9C9C718B32D2F6A9
              17D7E01D6C4C3D360398D81D23AC135350540BE4563EAA747DB2E00C631D2DE8
              5332083C90C5C89B9724A8D5949E8EE6757D0B5BB38BF00A8A9801D49FCF158E
              0F4A593CA4674A2686AC55F446C4125F7C47F935135D1F5E89178587D81011C4
              434DDD6CFF7F9DC43FA3FA769E186DD710E0BB647E806B47E24458800A2747FB
              F9012EA4798B6D49910CEA86F83AE5393DC2F30234B7F6E0199ECEFAED59FF06
              78567452BC7B7E6BFA07C6A6E560EBA89A05FC04A04A05003B72D0BC00000000
              49454E44AE426082}
          end>
      end>
    Left = 512
    Top = 240
  end
  object RunImageList: TVirtualImageList
    AutoFill = True
    DisabledGrayscale = False
    DisabledSuffix = '_Disabled'
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'Item1'
        Disabled = False
        Name = 'Item1'
      end
      item
        CollectionIndex = 1
        CollectionName = 'Item2'
        Disabled = False
        Name = 'Item2'
      end
      item
        CollectionIndex = 2
        CollectionName = 'Item3'
        Disabled = False
        Name = 'Item3'
      end
      item
        CollectionIndex = 3
        CollectionName = 'Item4'
        Disabled = False
        Name = 'Item4'
      end>
    ImageCollection = RunImageCollection
    Left = 400
    Top = 240
  end
  object OpenTextFileDialog: TOpenDialog
    Title = 'EPA SWMM 5'
    Left = 216
    Top = 128
  end
  object AnimatorImageCollection: TImageCollection
    Images = <
      item
        Name = 'vcrfirst'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000010000000100403000000EDDDE2
              5200000030504C5445000000800000008000808000000080800080008080C0C0
              C0808080FF000000FF00FFFF000000FFFF00FF00FFFFFFFFFF4F132649000000
              1074524E53FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00E0235D1900000034494441
              5478DA63F8FF1F0218FE63321838FEFFFFC001653040191F600C0628E3030603
              2E85500CD70E31F04707D0820E2C960200173354FC02319FFB0000000049454E
              44AE426082}
          end>
      end
      item
        Name = 'vcrback'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000010000000100403000000EDDDE2
              5200000030504C5445000000800000008000808000000080800080008080C0C0
              C0808080FF000000FF00FFFF000000FFFF00FF00FFFFFFFFFF4F132649000000
              1074524E53FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00E0235D190000002C494441
              5478DA63F8FF1F0218FE63657CE0803218A08C0F30060394F101830197422886
              6B4718F8BF039BA500B82A69161140BC360000000049454E44AE426082}
          end>
      end
      item
        Name = 'vcrstop'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000010000000100403000000EDDDE2
              5200000030504C5445000000800000008000808000000080800080008080C0C0
              C0808080FF000000FF00FFFF000000FFFF00FF00FFFFFFFFFF4F132649000000
              1074524E53FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00E0235D1900000034494441
              5478DA6378F70E0CDE333C60000320E33F087C0032BE970341030AA380818178
              0686F61F1D40D08064D73B98EDFFA100006AAB5103F29046050000000049454E
              44AE426082}
          end>
      end
      item
        Name = 'vcrfwd'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000010000000100403000000EDDDE2
              5200000030504C5445000000800000008000808000000080800080008080C0C0
              C0808080FF000000FF00FFFF000000FFFF00FF00FFFFFFFFFF4F132649000000
              1074524E53FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00E0235D1900000032494441
              5478DA63F8FF1F0218FE63617CE0873118E00C7E188301CEE0873138608C7E28
              8303A6A61FCAE0809AF3A31FBBA50089466BEB527B13860000000049454E44AE
              426082}
          end>
      end>
    Left = 512
    Top = 296
  end
  object AnimatorImageList: TVirtualImageList
    AutoFill = True
    DisabledGrayscale = False
    DisabledSuffix = '_Disabled'
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'vcrfirst'
        Disabled = False
        Name = 'vcrfirst'
      end
      item
        CollectionIndex = 1
        CollectionName = 'vcrback'
        Disabled = False
        Name = 'vcrback'
      end
      item
        CollectionIndex = 2
        CollectionName = 'vcrstop'
        Disabled = False
        Name = 'vcrstop'
      end
      item
        CollectionIndex = 3
        CollectionName = 'vcrfwd'
        Disabled = False
        Name = 'vcrfwd'
      end>
    ImageCollection = AnimatorImageCollection
    Left = 400
    Top = 296
  end
  object ProjectImageList1: TVirtualImageList
    DisabledOpacity = 100
    DisabledGrayscale = False
    DisabledSuffix = '_Disabled'
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'green_plus'
        Disabled = True
        Name = 'green_plus_Disabled'
      end
      item
        CollectionIndex = 1
        CollectionName = 'red_minus'
        Disabled = True
        Name = 'red_minus_Disabled'
      end
      item
        CollectionIndex = 2
        CollectionName = 'edit'
        Disabled = True
        Name = 'edit_Disabled'
      end
      item
        CollectionIndex = 3
        CollectionName = 'uparrow1'
        Disabled = True
        Name = 'uparrow1_Disabled'
      end
      item
        CollectionIndex = 4
        CollectionName = 'dnarrow1'
        Disabled = True
        Name = 'dnarrow1_Disabled'
      end
      item
        CollectionIndex = 5
        CollectionName = 'sort'
        Disabled = True
        Name = 'sort_Disabled'
      end
      item
        CollectionIndex = 6
        CollectionName = 'lock_open'
        Disabled = True
        Name = 'lock_open_Disabled'
      end
      item
        CollectionIndex = 7
        CollectionName = 'lock_closed'
        Disabled = True
        Name = 'lock_closed_Disabled'
      end
      item
        CollectionIndex = 8
        CollectionName = 'raingage'
        Disabled = True
        Name = 'raingage_Disabled'
      end
      item
        CollectionIndex = 9
        CollectionName = 'blue_plus'
        Disabled = True
        Name = 'blue_plus_Disabled'
      end
      item
        CollectionIndex = 10
        CollectionName = 'blue_minus'
        Disabled = True
        Name = 'blue_minus_Disabled'
      end
      item
        CollectionIndex = 11
        CollectionName = 'uparrow2'
        Disabled = True
        Name = 'uparrow2_Disabled'
      end
      item
        CollectionIndex = 12
        CollectionName = 'dnarrow2'
        Disabled = True
        Name = 'dnarrow2_Disabled'
      end
      item
        CollectionIndex = 13
        CollectionName = 'Delete'
        Disabled = True
        Name = 'Delete_Disabled'
      end
      item
        CollectionIndex = 14
        CollectionName = 'browse'
        Disabled = True
        Name = 'browse_Disabled'
      end>
    ImageCollection = ProjectImageCollection
    Left = 624
    Top = 184
  end
  object BtnImageList1: TVirtualImageList
    DisabledOpacity = 100
    DisabledGrayscale = False
    DisabledSuffix = '_Disabled'
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'StdToolbar\Item1'
        Disabled = True
        Name = 'Item1_Disabled'
      end
      item
        CollectionIndex = 1
        CollectionName = 'StdToolbar\Item2'
        Disabled = True
        Name = 'Item2_Disabled'
      end
      item
        CollectionIndex = 2
        CollectionName = 'StdToolbar\Item3'
        Disabled = True
        Name = 'Item3_Disabled'
      end
      item
        CollectionIndex = 3
        CollectionName = 'StdToolbar\Item4'
        Disabled = True
        Name = 'Item4_Disabled'
      end
      item
        CollectionIndex = 4
        CollectionName = 'StdToolbar\Item5'
        Disabled = True
        Name = 'Item5_Disabled'
      end
      item
        CollectionIndex = 5
        CollectionName = 'StdToolbar\Item6'
        Disabled = True
        Name = 'Item6_Disabled'
      end
      item
        CollectionIndex = 6
        CollectionName = 'StdToolbar\Item7'
        Disabled = True
        Name = 'Item7_Disabled'
      end
      item
        CollectionIndex = 7
        CollectionName = 'StdToolbar\Item8'
        Disabled = True
        Name = 'Item8_Disabled'
      end
      item
        CollectionIndex = 8
        CollectionName = 'StdToolbar\Item9'
        Disabled = True
        Name = 'Item9_Disabled'
      end
      item
        CollectionIndex = 9
        CollectionName = 'StdToolbar\Item10'
        Disabled = True
        Name = 'Item10_Disabled'
      end
      item
        CollectionIndex = 10
        CollectionName = 'StdToolbar\Item11'
        Disabled = True
        Name = 'Item11_Disabled'
      end
      item
        CollectionIndex = 11
        CollectionName = 'StdToolbar\Item12'
        Disabled = True
        Name = 'Item12_Disabled'
      end
      item
        CollectionIndex = 12
        CollectionName = 'StdToolbar\Item13'
        Disabled = True
        Name = 'Item13_Disabled'
      end
      item
        CollectionIndex = 13
        CollectionName = 'StdToolbar\Item14'
        Disabled = True
        Name = 'Item14_Disabled'
      end
      item
        CollectionIndex = 14
        CollectionName = 'StdToolbar\Item15'
        Disabled = True
        Name = 'Item15_Disabled'
      end
      item
        CollectionIndex = 15
        CollectionName = 'StdToolbar\Item16'
        Disabled = True
        Name = 'Item16_Disabled'
      end
      item
        CollectionIndex = 16
        CollectionName = 'StdToolbar\Item17'
        Disabled = True
        Name = 'Item17_Disabled'
      end
      item
        CollectionIndex = 17
        CollectionName = 'StdToolbar\Item18'
        Disabled = True
        Name = 'Item18_Disabled'
      end
      item
        CollectionIndex = 18
        CollectionName = 'MapToolbar\Item19'
        Disabled = True
        Name = 'Item19_Disabled'
      end
      item
        CollectionIndex = 19
        CollectionName = 'MapToolbar\Item20'
        Disabled = True
        Name = 'Item20_Disabled'
      end
      item
        CollectionIndex = 20
        CollectionName = 'MapToolbar\Item21'
        Disabled = True
        Name = 'Item21_Disabled'
      end
      item
        CollectionIndex = 21
        CollectionName = 'MapToolbar\Item22'
        Disabled = True
        Name = 'Item22_Disabled'
      end
      item
        CollectionIndex = 22
        CollectionName = 'MapToolbar\Item23'
        Disabled = True
        Name = 'Item23_Disabled'
      end
      item
        CollectionIndex = 23
        CollectionName = 'MapToolbar\Item24'
        Disabled = True
        Name = 'Item24_Disabled'
      end
      item
        CollectionIndex = 24
        CollectionName = 'MapToolbar\Item25'
        Disabled = True
        Name = 'Item25_Disabled'
      end
      item
        CollectionIndex = 25
        CollectionName = 'MapToolbar\Item26'
        Disabled = True
        Name = 'Item26_Disabled'
      end
      item
        CollectionIndex = 26
        CollectionName = 'ObjectToolbar\Item27'
        Disabled = True
        Name = 'Item27_Disabled'
      end
      item
        CollectionIndex = 27
        CollectionName = 'ObjectToolbar\Item28'
        Disabled = True
        Name = 'Item28_Disabled'
      end
      item
        CollectionIndex = 28
        CollectionName = 'ObjectToolbar\Item29'
        Disabled = True
        Name = 'Item29_Disabled'
      end
      item
        CollectionIndex = 29
        CollectionName = 'ObjectToolbar\Item30'
        Disabled = True
        Name = 'Item30_Disabled'
      end
      item
        CollectionIndex = 30
        CollectionName = 'ObjectToolbar\Item31'
        Disabled = True
        Name = 'Item31_Disabled'
      end
      item
        CollectionIndex = 31
        CollectionName = 'ObjectToolbar\Item32'
        Disabled = True
        Name = 'Item32_Disabled'
      end
      item
        CollectionIndex = 32
        CollectionName = 'ObjectToolbar\Item33'
        Disabled = True
        Name = 'Item33_Disabled'
      end
      item
        CollectionIndex = 33
        CollectionName = 'ObjectToolbar\Item34'
        Disabled = True
        Name = 'Item34_Disabled'
      end
      item
        CollectionIndex = 34
        CollectionName = 'ObjectToolbar\Item35'
        Disabled = True
        Name = 'Item35_Disabled'
      end
      item
        CollectionIndex = 35
        CollectionName = 'ObjectToolbar\Item36'
        Disabled = True
        Name = 'Item36_Disabled'
      end
      item
        CollectionIndex = 36
        CollectionName = 'ObjectToolbar\Item37'
        Disabled = True
        Name = 'Item37_Disabled'
      end
      item
        CollectionIndex = 37
        CollectionName = 'ObjectToolbar\Item38'
        Disabled = True
        Name = 'Item38_Disabled'
      end>
    ImageCollection = BtnImageCollection
    Left = 624
    Top = 128
  end
  object AnimatorImageList1: TVirtualImageList
    DisabledGrayscale = False
    DisabledSuffix = '_Disabled'
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'vcrfirst'
        Disabled = True
        Name = 'vcrfirst_Disabled'
      end
      item
        CollectionIndex = 1
        CollectionName = 'vcrback'
        Disabled = True
        Name = 'vcrback_Disabled'
      end
      item
        CollectionIndex = 2
        CollectionName = 'vcrstop'
        Disabled = True
        Name = 'vcrstop_Disabled'
      end
      item
        CollectionIndex = 3
        CollectionName = 'vcrfwd'
        Disabled = True
        Name = 'vcrfwd_Disabled'
      end>
    ImageCollection = AnimatorImageCollection
    Left = 624
    Top = 296
  end
end
