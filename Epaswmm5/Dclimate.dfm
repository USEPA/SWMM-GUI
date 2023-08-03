object ClimatologyForm: TClimatologyForm
  Left = 273
  Top = 25
  BorderStyle = bsDialog
  Caption = 'Climatology Editor'
  ClientHeight = 477
  ClientWidth = 395
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 15
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 377
    Height = 417
    ActivePage = TabSheet1
    MultiLine = True
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Temperature'
      object Label12: TLabel
        Left = 16
        Top = 16
        Width = 149
        Height = 15
        Caption = 'Source of Temperature Data:'
      end
      object RadioButton1: TRadioButton
        Left = 32
        Top = 51
        Width = 112
        Height = 17
        Caption = 'No Data'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = EditChanged
      end
      object RadioButton2: TRadioButton
        Left = 32
        Top = 88
        Width = 144
        Height = 17
        Caption = 'Time Series'
        TabOrder = 1
        OnClick = EditChanged
      end
      object TempSeriesCombo: TComboBox
        Left = 32
        Top = 111
        Width = 133
        Height = 23
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 2
        OnChange = TempSeriesComboChange
        OnClick = TempSeriesComboClick
        OnDblClick = TempTseriesBtnClick
        OnKeyPress = TempSeriesComboKeyPress
      end
      object TempTseriesBtn: TBitBtn
        Left = 166
        Top = 112
        Width = 23
        Height = 22
        Hint = 'Edit the Time Series'
        ParentShowHint = False
        ShowHint = True
        Spacing = 0
        TabOrder = 3
        OnClick = TempTseriesBtnClick
      end
      object RadioButton3: TRadioButton
        Left = 32
        Top = 156
        Width = 176
        Height = 17
        Caption = 'External Climate File'
        TabOrder = 4
        OnClick = EditChanged
      end
      object TempFileEdit: TEdit
        Left = 32
        Top = 178
        Width = 244
        Height = 23
        TabStop = False
        Ctl3D = True
        ParentCtl3D = False
        ParentShowHint = False
        ReadOnly = True
        ShowHint = True
        TabOrder = 5
        OnChange = TempFileEditChange
      end
      object TempFileBtn: TBitBtn
        Left = 277
        Top = 179
        Width = 23
        Height = 22
        Hint = 'Select a file'
        ParentShowHint = False
        ShowHint = True
        Spacing = 0
        TabOrder = 6
        OnClick = TempFileBtnClick
      end
      object TempStartDateBox: TCheckBox
        Left = 32
        Top = 211
        Width = 149
        Height = 21
        Caption = 'Start Reading File at'
        TabOrder = 8
        OnClick = TempStartDateBoxClick
      end
      object TempStartDateEdit: TMaskEdit
        Left = 187
        Top = 211
        Width = 86
        Height = 23
        Ctl3D = True
        Enabled = False
        EditMask = '!99/99/0000;1;_'
        MaxLength = 10
        ParentCtl3D = False
        TabOrder = 9
        Text = '  /  /    '
      end
      object GroupBox1: TGroupBox
        Left = 32
        Top = 246
        Width = 241
        Height = 105
        Caption = 'Temperature Units for GHCN Files'
        TabOrder = 10
        object RadioButton4: TRadioButton
          Left = 20
          Top = 24
          Width = 181
          Height = 17
          Caption = 'Tenths of Degrees Celsius'
          Checked = True
          TabOrder = 0
          TabStop = True
        end
        object RadioButton5: TRadioButton
          Left = 20
          Top = 47
          Width = 156
          Height = 17
          Caption = 'Degrees Celsius'
          TabOrder = 1
        end
        object RadioButton6: TRadioButton
          Left = 20
          Top = 70
          Width = 156
          Height = 17
          Caption = 'Degrees Fahrenheit'
          TabOrder = 2
        end
      end
      object TempFileDelBtn: TBitBtn
        Tag = 1
        Left = 300
        Top = 179
        Width = 23
        Height = 22
        Hint = 'Clear File Selection'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 7
        OnClick = TempFileDelBtnClick
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Evaporation'
      ImageIndex = 4
      object EvapGridLabel: TLabel
        Left = 16
        Top = 54
        Width = 156
        Height = 15
        Caption = 'Monthly Evaporation (in/day)'
      end
      object SoilRecoveryLabel: TLabel
        Left = 17
        Top = 235
        Width = 137
        Height = 35
        AutoSize = False
        Caption = 'Monthly Soil Recovery Pattern (Optional)'
        WordWrap = True
      end
      object Bevel1: TBevel
        Left = 16
        Top = 218
        Width = 328
        Height = 3
        Shape = bsBottomLine
      end
      object Label1: TLabel
        Left = 16
        Top = 16
        Width = 150
        Height = 15
        Caption = 'Source of Evaporation  Rates'
      end
      object EvapTempLabel: TLabel
        AlignWithMargins = True
        Left = 16
        Top = 298
        Width = 320
        Height = 50
        AutoSize = False
        Caption = 
          'Evaporation rates will be computed using temperatures from the C' +
          'limate File selected on the Temperature page.'
        Visible = False
        WordWrap = True
      end
      object EvapValueEdit: TNumEdit
        Left = 193
        Top = 51
        Width = 73
        Height = 23
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 1
        Visible = False
        OnChange = EditChanged
        Style = esPosNumber
        Modified = False
        SelLength = 0
        SelStart = 0
      end
      object EvapSeriesCombo: TComboBox
        Left = 159
        Top = 325
        Width = 156
        Height = 23
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 2
        Visible = False
        OnChange = EditChanged
        OnDblClick = EvapSeriesBtnClick
        OnKeyPress = TempSeriesComboKeyPress
      end
      inline EvapGrid1: TGridEditFrame
        Left = 16
        Top = 80
        Width = 324
        Height = 56
        AutoSize = True
        TabOrder = 3
        ExplicitLeft = 16
        ExplicitTop = 80
        ExplicitWidth = 324
        ExplicitHeight = 56
        inherited Grid: TStringGrid
          Left = 1
          Width = 323
          Height = 56
          ColCount = 6
          DefaultColWidth = 46
          FixedCols = 0
          RowCount = 2
          ScrollBars = ssNone
          ExplicitLeft = 1
          ExplicitWidth = 323
          ExplicitHeight = 56
        end
        inherited EditPanel: TPanel
          Left = 0
          Top = 0
          Height = 40
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitHeight = 40
          inherited EditBox: TNumEdit
            Width = 120
            Style = esPosNumber
            ExplicitWidth = 120
          end
        end
        inherited PopupMenu: TPopupMenu
          Top = 16
        end
      end
      inline EvapGrid2: TGridEditFrame
        Left = 16
        Top = 142
        Width = 324
        Height = 56
        AutoSize = True
        TabOrder = 4
        ExplicitLeft = 16
        ExplicitTop = 142
        ExplicitWidth = 324
        ExplicitHeight = 56
        inherited Grid: TStringGrid
          Left = 1
          Width = 323
          Height = 56
          ColCount = 6
          DefaultColWidth = 46
          FixedCols = 0
          RowCount = 2
          ScrollBars = ssNone
          ExplicitLeft = 1
          ExplicitWidth = 323
          ExplicitHeight = 56
        end
        inherited EditPanel: TPanel
          Left = 0
          ExplicitLeft = 0
          inherited EditBox: TNumEdit
            Width = 120
            Style = esPosNumber
            ExplicitWidth = 120
          end
        end
        inherited PopupMenu: TPopupMenu
          Top = 16
        end
      end
      object EvapSeriesBtn: TBitBtn
        Left = 316
        Top = 326
        Width = 23
        Height = 22
        Hint = 'Edit the time series'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 5
        Visible = False
        OnClick = EvapSeriesBtnClick
      end
      object RecoveryCombo: TComboBox
        Tag = 1
        Left = 159
        Top = 238
        Width = 137
        Height = 23
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 6
        OnChange = RecoveryComboChange
        OnDblClick = RecoveryComboDblClick
        OnKeyPress = TempSeriesComboKeyPress
      end
      object RecoveryBtn1: TBitBtn
        Tag = 1
        Left = 297
        Top = 239
        Width = 23
        Height = 22
        Hint = 'Edit the recovery pattern'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 7
        OnClick = RecoveryComboDblClick
      end
      object RecoveryBtn2: TBitBtn
        Tag = 1
        Left = 320
        Top = 239
        Width = 23
        Height = 22
        Hint = 'Remove Pattern'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 8
        OnClick = RecoveryBtn2Click
      end
      object DryOnlyCheckBox: TCheckBox
        Left = 16
        Top = 286
        Width = 288
        Height = 17
        Caption = 'Evaporate Only During Dry Periods'
        TabOrder = 9
        OnClick = DryOnlyCheckBoxClick
      end
      object EvapSourceCombo: TComboBox
        Left = 193
        Top = 13
        Width = 145
        Height = 24
        Style = csOwnerDrawFixed
        ItemHeight = 18
        TabOrder = 0
        OnChange = EvapSourceComboChange
        Items.Strings = (
          'Constant Value'
          'Time Series'
          'Climate File'
          'Monthly Averages'
          'Temperatures')
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Wind Speed'
      ImageIndex = 1
      object WindSpeedLabel: TLabel
        Left = 32
        Top = 102
        Width = 147
        Height = 15
        Caption = 'Monthly Wind Speed (mph)'
      end
      inline WindGrid1: TGridEditFrame
        Left = 32
        Top = 122
        Width = 298
        Height = 56
        AutoSize = True
        TabOrder = 2
        ExplicitLeft = 32
        ExplicitTop = 122
        ExplicitWidth = 298
        ExplicitHeight = 56
        inherited Grid: TStringGrid
          Left = 1
          Width = 297
          Height = 56
          ColCount = 6
          DefaultColWidth = 47
          FixedCols = 0
          RowCount = 2
          ScrollBars = ssNone
          ExplicitLeft = 1
          ExplicitWidth = 297
          ExplicitHeight = 56
        end
        inherited EditPanel: TPanel
          Left = 0
          ExplicitLeft = 0
          inherited EditBox: TNumEdit
            Top = 0
            Width = 120
            Style = esPosNumber
            ExplicitTop = 0
            ExplicitWidth = 120
          end
        end
        inherited PopupMenu: TPopupMenu
          Top = 16
        end
      end
      inline WindGrid2: TGridEditFrame
        Left = 32
        Top = 181
        Width = 298
        Height = 56
        AutoSize = True
        TabOrder = 3
        ExplicitLeft = 32
        ExplicitTop = 181
        ExplicitWidth = 298
        ExplicitHeight = 56
        inherited Grid: TStringGrid
          Left = 1
          Width = 297
          Height = 56
          ColCount = 6
          DefaultColWidth = 47
          FixedCols = 0
          RowCount = 2
          ScrollBars = ssNone
          ExplicitLeft = 1
          ExplicitWidth = 297
          ExplicitHeight = 56
        end
        inherited EditPanel: TPanel
          Left = 0
          Ctl3D = True
          ExplicitLeft = 0
          inherited EditBox: TNumEdit
            Width = 120
            Style = esPosNumber
            ExplicitWidth = 120
          end
        end
        inherited PopupMenu: TPopupMenu
          Top = 16
        end
      end
      object RadioButton9: TRadioButton
        Left = 32
        Top = 64
        Width = 144
        Height = 17
        Caption = 'Use Monthly Averages'
        TabOrder = 1
        OnClick = EditChanged
      end
      object RadioButton8: TRadioButton
        Left = 32
        Top = 24
        Width = 280
        Height = 16
        Caption = 'Use Climate File Data (see Temperature Page)'
        TabOrder = 0
        TabStop = True
        OnClick = EditChanged
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Snow Melt'
      ImageIndex = 2
      object Label4: TLabel
        Left = 52
        Top = 24
        Width = 113
        Height = 15
        Caption = 'Dividing Temperature'
      end
      object Label5: TLabel
        Left = 52
        Top = 83
        Width = 109
        Height = 15
        Caption = 'ATI Weight (fraction)'
      end
      object Label6: TLabel
        Left = 52
        Top = 120
        Width = 104
        Height = 15
        Caption = 'Negative Melt Ratio'
      end
      object Label7: TLabel
        Left = 52
        Top = 164
        Width = 109
        Height = 15
        Caption = 'Elevation above MSL'
      end
      object Label8: TLabel
        Left = 52
        Top = 207
        Width = 95
        Height = 15
        Caption = 'Latitude (degrees)'
      end
      object Label9: TLabel
        Left = 52
        Top = 248
        Width = 113
        Height = 15
        Caption = 'Longitude Correction'
      end
      object Label11: TLabel
        Left = 52
        Top = 40
        Width = 126
        Height = 15
        Caption = 'Between Snow and Rain'
      end
      object SnowTempUnitsLabel: TLabel
        Left = 52
        Top = 56
        Width = 58
        Height = 15
        Caption = '(degrees F)'
      end
      object ElevUnitsLabel: TLabel
        Left = 52
        Top = 180
        Width = 28
        Height = 15
        Caption = '(feet)'
      end
      object Label13: TLabel
        Left = 52
        Top = 264
        Width = 72
        Height = 15
        Caption = '(+/- minutes)'
      end
      object Label3: TLabel
        Left = 52
        Top = 135
        Width = 49
        Height = 15
        Caption = '(fraction)'
      end
      object NumEdit1: TNumEdit
        Left = 211
        Top = 37
        Width = 73
        Height = 23
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 0
        OnChange = EditChanged
        Style = esNumber
        Modified = False
        SelLength = 0
        SelStart = 0
      end
      object NumEdit2: TNumEdit
        Left = 211
        Top = 78
        Width = 73
        Height = 23
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 1
        OnChange = EditChanged
        Style = esPosNumber
        Modified = False
        SelLength = 0
        SelStart = 0
      end
      object NumEdit3: TNumEdit
        Left = 211
        Top = 117
        Width = 73
        Height = 23
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 2
        OnChange = EditChanged
        Style = esPosNumber
        Modified = False
        SelLength = 0
        SelStart = 0
      end
      object NumEdit4: TNumEdit
        Left = 211
        Top = 161
        Width = 73
        Height = 23
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 3
        OnChange = EditChanged
        Style = esNumber
        Modified = False
        SelLength = 0
        SelStart = 0
      end
      object NumEdit5: TNumEdit
        Left = 211
        Top = 203
        Width = 73
        Height = 23
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 4
        OnChange = EditChanged
        Style = esNumber
        Modified = False
        SelLength = 0
        SelStart = 0
      end
      object NumEdit6: TNumEdit
        Left = 211
        Top = 245
        Width = 73
        Height = 23
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 5
        OnChange = EditChanged
        Style = esNumber
        Modified = False
        SelLength = 0
        SelStart = 0
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Areal Depletion'
      ImageIndex = 3
      object Label10: TLabel
        Left = 95
        Top = 1
        Width = 179
        Height = 15
        Caption = 'Fraction of Area Covered by Snow'
      end
      inline ADCGrid: TGridEditFrame
        Left = 35
        Top = 21
        Width = 297
        Height = 280
        AutoSize = True
        TabOrder = 0
        ExplicitLeft = 35
        ExplicitTop = 21
        ExplicitWidth = 297
        ExplicitHeight = 280
        inherited Grid: TStringGrid
          Left = 1
          Width = 296
          Height = 280
          ColCount = 3
          DefaultColWidth = 97
          RowCount = 11
          ScrollBars = ssVertical
          ExplicitLeft = 1
          ExplicitWidth = 296
          ExplicitHeight = 280
        end
        inherited EditPanel: TPanel
          Left = 0
          ExplicitLeft = 0
          inherited EditBox: TNumEdit
            Width = 120
            Style = esPosNumber
            ExplicitWidth = 120
          end
        end
      end
      object Panel1: TPanel
        Left = 134
        Top = 301
        Width = 198
        Height = 51
        AutoSize = True
        BevelOuter = bvNone
        TabOrder = 1
        object NoADBtn1: TButton
          Left = 0
          Top = 0
          Width = 97
          Height = 25
          Caption = 'No Depletion'
          TabOrder = 0
          OnClick = NoADBtn1Click
        end
        object NatADBtn1: TButton
          Left = 0
          Top = 26
          Width = 97
          Height = 25
          Caption = 'Natural Area'
          TabOrder = 1
          OnClick = NoADBtn1Click
        end
        object NoADBtn2: TButton
          Left = 101
          Top = 0
          Width = 97
          Height = 25
          Caption = 'No Depletion'
          TabOrder = 2
          OnClick = NoADBtn1Click
        end
        object NatADBtn2: TButton
          Left = 101
          Top = 26
          Width = 97
          Height = 25
          Caption = 'Natural Area'
          TabOrder = 3
          OnClick = NoADBtn1Click
        end
      end
    end
    object TabSheet6: TTabSheet
      Caption = 'Adjustments'
      ImageIndex = 5
      inline AdjustGrid: TGridEditFrame
        Left = 0
        Top = 0
        Width = 369
        Height = 289
        Align = alTop
        TabOrder = 0
        ExplicitWidth = 369
        ExplicitHeight = 289
        inherited Grid: TStringGrid
          Width = 369
          Height = 330
          DefaultColWidth = 72
          DefaultRowHeight = 21
          RowCount = 13
          ScrollBars = ssNone
          ExplicitWidth = 369
          ExplicitHeight = 330
        end
      end
      object Panel2: TPanel
        Left = 0
        Top = 289
        Width = 369
        Height = 78
        Align = alClient
        Alignment = taLeftJustify
        BevelOuter = bvNone
        Ctl3D = False
        Padding.Left = 4
        Padding.Right = 4
        ParentCtl3D = False
        TabOrder = 1
        VerticalAlignment = taAlignTop
        DesignSize = (
          369
          78)
        object Label2: TLabel
          Left = 4
          Top = 0
          Width = 361
          Height = 78
          Align = alClient
          AutoSize = False
          Caption = 'Label2'
          WordWrap = True
          ExplicitLeft = 0
          ExplicitWidth = 337
          ExplicitHeight = 30
        end
        object ClearBtn: TButton
          Left = 284
          Top = 48
          Width = 75
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = 'Clear All'
          TabOrder = 0
          OnClick = ClearBtnClick
        end
      end
    end
  end
  object OKBtn: TButton
    Left = 73
    Top = 440
    Width = 75
    Height = 24
    Caption = 'OK'
    TabOrder = 1
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 160
    Top = 440
    Width = 75
    Height = 24
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object HelpBtn: TButton
    Left = 248
    Top = 440
    Width = 75
    Height = 24
    Caption = '&Help'
    TabOrder = 3
    OnClick = HelpBtnClick
  end
end
