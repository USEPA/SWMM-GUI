object InflowsForm: TInflowsForm
  Left = 209
  Top = 191
  BorderStyle = bsDialog
  Caption = 'Inflows Editor'
  ClientHeight = 471
  ClientWidth = 353
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object PageControl1: TPageControl
    Left = 10
    Top = 8
    Width = 329
    Height = 329
    ActivePage = TimeSeriesPage
    TabOrder = 0
    OnChange = PageControl1Change
    object TimeSeriesPage: TTabSheet
      Caption = 'Direct'
      object Label1: TLabel
        Left = 16
        Top = 71
        Width = 62
        Height = 15
        Caption = 'Constituent'
      end
      object Label2: TLabel
        Left = 16
        Top = 171
        Width = 59
        Height = 15
        Caption = 'Time Series'
      end
      object Label3: TLabel
        Left = 16
        Top = 237
        Width = 60
        Height = 15
        Caption = 'Inflow Type'
      end
      object Label4: TLabel
        Left = 16
        Top = 271
        Width = 63
        Height = 15
        Caption = 'Units Factor'
        WordWrap = True
      end
      object Label5: TLabel
        Left = 16
        Top = 204
        Width = 63
        Height = 15
        Caption = 'Scale Factor'
      end
      object Label13: TLabel
        Left = 16
        Top = 104
        Width = 43
        Height = 15
        Caption = 'Baseline'
      end
      object Label14: TLabel
        Left = 16
        Top = 137
        Width = 84
        Height = 15
        Caption = 'Baseline Pattern'
      end
      object Label15: TLabel
        Left = 16
        Top = 39
        Width = 289
        Height = 15
        AutoSize = False
        Caption = '                (Time Series Value) x (Scale Factor)'
      end
      object Label6: TLabel
        Left = 16
        Top = 16
        Width = 289
        Height = 15
        AutoSize = False
        Caption = 'Inflow  =  (Baseline Value) x (Baseline Pattern) +'
      end
      object DxParamCombo: TComboBox
        Left = 120
        Top = 68
        Width = 129
        Height = 24
        Style = csOwnerDrawFixed
        ItemHeight = 18
        TabOrder = 0
        OnChange = DxParamComboChange
      end
      object DxSeriesCombo: TComboBox
        Left = 120
        Top = 168
        Width = 129
        Height = 23
        TabOrder = 6
        OnChange = DxSeriesComboChange
        OnDblClick = DxSeriesComboDblClick
      end
      object DxTypeCombo: TComboBox
        Left = 120
        Top = 234
        Width = 129
        Height = 24
        Style = csOwnerDrawFixed
        ItemHeight = 18
        ItemIndex = 0
        TabOrder = 10
        Text = 'CONCEN'
        OnChange = DxSeriesComboChange
        Items.Strings = (
          'CONCEN'
          'MASS')
      end
      object DxCFactorEdit: TNumEdit
        Left = 122
        Top = 268
        Width = 127
        Height = 23
        TabOrder = 11
        Text = '1.0'
        OnChange = DWChange
        Style = esPosNumber
        Modified = False
        SelLength = 0
        SelStart = 0
      end
      object DxInflowDataGrid: TStringGrid
        Left = 273
        Top = 57
        Width = 32
        Height = 32
        Color = clWhite
        ColCount = 4
        DefaultColWidth = 92
        FixedCols = 0
        RowCount = 15
        FixedRows = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goRowSelect]
        ScrollBars = ssVertical
        TabOrder = 12
        Visible = False
      end
      object TseriesBtn1: TBitBtn
        Left = 252
        Top = 169
        Width = 23
        Height = 22
        Hint = 'Edit Time Series'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 7
        OnClick = TseriesBtn1Click
      end
      object TseriesBtn2: TBitBtn
        Left = 277
        Top = 169
        Width = 23
        Height = 22
        Hint = 'Remove Time Series'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 8
        OnClick = TseriesBtn2Click
      end
      object DxSFactorEdit: TNumEdit
        Left = 120
        Top = 201
        Width = 129
        Height = 23
        TabOrder = 9
        Text = '1.0'
        OnChange = DWChange
        Style = esNumber
        Modified = False
        SelLength = 0
        SelStart = 0
      end
      object DxBaseEdit: TNumEdit
        Left = 120
        Top = 101
        Width = 129
        Height = 23
        TabOrder = 1
        OnChange = DWChange
        Style = esNumber
        Modified = False
        SelLength = 0
        SelStart = 0
      end
      object DxBaseDelBtn: TBitBtn
        Left = 252
        Top = 102
        Width = 23
        Height = 22
        Hint = 'Remove Value'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        OnClick = DxBaseDelBtnClick
      end
      object DxPatCombo: TComboBox
        Left = 120
        Top = 134
        Width = 129
        Height = 23
        TabOrder = 3
        OnChange = DxSeriesComboChange
        OnDblClick = DxPatComboDblClick
      end
      object DxPatBtn1: TBitBtn
        Left = 252
        Top = 135
        Width = 23
        Height = 22
        Hint = 'Edit Time Pattern'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
        OnClick = DxPatComboDblClick
      end
      object DxPatBtn2: TBitBtn
        Left = 277
        Top = 135
        Width = 23
        Height = 22
        Hint = 'Remove Pattern'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 5
        OnClick = DxPatBtn2Click
      end
    end
    object DryWeatherPage: TTabSheet
      Caption = 'Dry Weather'
      ImageIndex = 1
      object Label8: TLabel
        Left = 16
        Top = 76
        Width = 62
        Height = 15
        Caption = 'Constituent'
      end
      object Label9: TLabel
        Left = 16
        Top = 101
        Width = 74
        Height = 15
        Caption = 'Average Value'
      end
      object Label10: TLabel
        Left = 16
        Top = 140
        Width = 72
        Height = 15
        Caption = 'Time Patterns'
      end
      object DwUnitsLabel: TLabel
        Left = 16
        Top = 117
        Width = 72
        Height = 15
        Caption = 'DwUnitsLabel'
      end
      object Label7: TLabel
        Left = 16
        Top = 16
        Width = 289
        Height = 15
        AutoSize = False
        Caption = 'Inflow = (Average Value) x (Pattern 1) x'
        WordWrap = True
      end
      object Label18: TLabel
        Left = 48
        Top = 39
        Width = 257
        Height = 15
        AutoSize = False
        Caption = '     (Pattern 2) x (Pattern 3) x (Pattern 4)'
        WordWrap = True
      end
      object DwParamCombo: TComboBox
        Left = 120
        Top = 73
        Width = 129
        Height = 24
        Style = csOwnerDrawFixed
        ItemHeight = 18
        TabOrder = 0
        OnChange = DwParamComboChange
      end
      object DwAvgEdit: TNumEdit
        Left = 120
        Top = 105
        Width = 129
        Height = 23
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 1
        OnChange = DWChange
        Style = esNumber
        Modified = False
        SelLength = 0
        SelStart = 0
      end
      object DwInflowDataGrid: TStringGrid
        Left = 273
        Top = 54
        Width = 32
        Height = 32
        Color = clWhite
        ColCount = 4
        DefaultColWidth = 92
        FixedCols = 0
        RowCount = 15
        FixedRows = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goRowSelect]
        ScrollBars = ssVertical
        TabOrder = 15
        Visible = False
      end
      object DwPatCombo1: TComboBox
        Tag = 1
        Left = 120
        Top = 137
        Width = 129
        Height = 23
        TabOrder = 3
        OnChange = DWChange
        OnDblClick = DwPatCombo1DblClick
      end
      object DwPatCombo2: TComboBox
        Tag = 2
        Left = 120
        Top = 170
        Width = 129
        Height = 23
        TabOrder = 6
        OnChange = DWChange
        OnDblClick = DwPatCombo1DblClick
      end
      object DwPatCombo3: TComboBox
        Tag = 3
        Left = 120
        Top = 202
        Width = 129
        Height = 23
        TabOrder = 9
        OnChange = DWChange
        OnDblClick = DwPatCombo1DblClick
      end
      object DwPatCombo4: TComboBox
        Tag = 4
        Left = 120
        Top = 235
        Width = 129
        Height = 23
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 12
        OnChange = DWChange
        OnDblClick = DwPatCombo1DblClick
      end
      object PatternBtn1: TBitBtn
        Tag = 1
        Left = 252
        Top = 138
        Width = 23
        Height = 22
        Hint = 'Edit Pattern'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
        OnClick = DwPatCombo1DblClick
      end
      object PatternBtn2: TBitBtn
        Tag = 2
        Left = 252
        Top = 171
        Width = 23
        Height = 22
        Hint = 'Edit Pattern'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 7
        OnClick = DwPatCombo1DblClick
      end
      object PatternBtn3: TBitBtn
        Tag = 3
        Left = 252
        Top = 203
        Width = 23
        Height = 22
        Hint = 'Edit Pattern'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 10
        OnClick = DwPatCombo1DblClick
      end
      object PatternBtn4: TBitBtn
        Tag = 4
        Left = 252
        Top = 236
        Width = 23
        Height = 22
        Hint = 'Edit Pattern'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 13
        OnClick = DwPatCombo1DblClick
      end
      object PatternBtn5: TBitBtn
        Tag = 1
        Left = 277
        Top = 138
        Width = 23
        Height = 22
        Hint = 'Remove Pattern'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 5
        OnClick = PatternBtnDelClick
      end
      object PatternBtn6: TBitBtn
        Tag = 2
        Left = 277
        Top = 171
        Width = 23
        Height = 22
        Hint = 'Remove Pattern'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 8
        OnClick = PatternBtnDelClick
      end
      object PatternBtn7: TBitBtn
        Tag = 3
        Left = 277
        Top = 203
        Width = 23
        Height = 22
        Hint = 'Remove Pattern'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 11
        OnClick = PatternBtnDelClick
      end
      object PatternBtn8: TBitBtn
        Tag = 4
        Left = 277
        Top = 236
        Width = 23
        Height = 22
        Hint = 'Remove Pattern'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 14
        OnClick = PatternBtnDelClick
      end
      object DwAvgDelBtn: TBitBtn
        Tag = 1
        Left = 252
        Top = 106
        Width = 23
        Height = 22
        Hint = 'Remove Value'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        OnClick = DwAvgDelBtnClick
      end
    end
    object RDIIPage: TTabSheet
      Caption = 'RDII'
      ImageIndex = 2
      object Label11: TLabel
        Left = 61
        Top = 24
        Width = 125
        Height = 15
        Caption = 'Unit Hydrograph Group'
      end
      object AreaUnitsLabel: TLabel
        Left = 61
        Top = 102
        Width = 108
        Height = 43
        AutoSize = False
        Caption = 'AreaUnitsLabel'
        WordWrap = True
      end
      object UHGroupCombo: TComboBox
        Left = 61
        Top = 44
        Width = 153
        Height = 23
        TabOrder = 0
        OnChange = RDIIChange
        OnDblClick = UHGroupComboDblClick
      end
      object SewerAreaEdit: TNumEdit
        Left = 195
        Top = 99
        Width = 65
        Height = 23
        TabOrder = 3
        Text = '0'
        OnChange = RDIIChange
        Style = esPosNumber
        Modified = False
        SelLength = 0
        SelStart = 0
      end
      object UHBtn: TBitBtn
        Left = 216
        Top = 45
        Width = 23
        Height = 22
        Hint = 'Edit UH Group'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnClick = UHBtnClick
      end
      object UHDelBtn: TBitBtn
        Tag = 1
        Left = 240
        Top = 45
        Width = 23
        Height = 22
        Hint = 'Remove UH Group'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        OnClick = UHDelBtnClick
      end
    end
  end
  object OKBtn: TButton
    Left = 48
    Top = 432
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 1
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 136
    Top = 432
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object HelpBtn: TButton
    Left = 224
    Top = 432
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 3
    OnClick = HelpBtnClick
  end
  object HintPanel: TPanel
    Left = 14
    Top = 353
    Width = 321
    Height = 64
    BevelInner = bvRaised
    BevelOuter = bvLowered
    BorderWidth = 1
    Ctl3D = True
    Padding.Left = 2
    Padding.Top = 2
    Padding.Right = 2
    Padding.Bottom = 2
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 4
    object HintLabel: TLabel
      Left = 5
      Top = 5
      Width = 311
      Height = 54
      Align = alClient
      Alignment = taCenter
      AutoSize = False
      Caption = 
        'If Baseline or Time Series is left blank its value is 0. If Base' +
        'line Pattern is left blank its value is 1.0.'
      Layout = tlCenter
      WordWrap = True
      ExplicitWidth = 312
    end
  end
end
