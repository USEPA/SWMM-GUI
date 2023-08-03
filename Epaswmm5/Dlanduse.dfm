object LanduseForm: TLanduseForm
  Left = 192
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Land Use Editor'
  ClientHeight = 344
  ClientWidth = 297
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
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object OKBtn: TButton
    Left = 17
    Top = 304
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 0
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 111
    Top = 304
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object HelpBtn: TButton
    Left = 205
    Top = 304
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 2
    OnClick = HelpBtnClick
  end
  object BuildupGrid: TStringGrid
    Left = 0
    Top = 296
    Width = 41
    Height = 33
    ColCount = 6
    DefaultColWidth = 84
    FixedCols = 0
    RowCount = 11
    FixedRows = 0
    GridLineWidth = 0
    Options = [goFixedHorzLine, goRangeSelect, goTabs, goRowSelect]
    ScrollBars = ssVertical
    TabOrder = 3
    Visible = False
    RowHeights = (
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24)
  end
  object WashoffGrid: TStringGrid
    Left = 256
    Top = 296
    Width = 41
    Height = 33
    DefaultColWidth = 84
    FixedCols = 0
    RowCount = 11
    FixedRows = 0
    GridLineWidth = 0
    Options = [goFixedHorzLine, goRangeSelect, goColSizing, goTabs, goRowSelect]
    ScrollBars = ssVertical
    TabOrder = 4
    Visible = False
  end
  object TabControl1: TTabControl
    Left = 8
    Top = 8
    Width = 279
    Height = 281
    TabOrder = 5
    Tabs.Strings = (
      'General'
      'Buildup'
      'Washoff')
    TabIndex = 0
    OnChange = TabControl1Change
    object Splitter1: TSplitter
      Left = 4
      Top = 204
      Width = 271
      Height = 4
      Cursor = crVSplit
      Align = alBottom
      Color = clBtnShadow
      ParentColor = False
      ExplicitTop = 205
    end
    object PollutPanel: TPanel
      Left = 4
      Top = 26
      Width = 271
      Height = 24
      Align = alTop
      Alignment = taLeftJustify
      BevelOuter = bvNone
      Caption = '  Pollutant'
      TabOrder = 0
      object PollutCombo: TComboBox
        Left = 136
        Top = 0
        Width = 132
        Height = 24
        Style = csOwnerDrawFixed
        ItemHeight = 18
        TabOrder = 0
        OnClick = PollutComboClick
      end
    end
    object HintPanel: TPanel
      Left = 4
      Top = 208
      Width = 271
      Height = 69
      Align = alBottom
      BevelOuter = bvNone
      BorderWidth = 1
      Ctl3D = False
      Padding.Left = 2
      Padding.Right = 2
      Padding.Bottom = 2
      ParentBackground = False
      ParentColor = True
      ParentCtl3D = False
      TabOrder = 1
      object HintLabel: TLabel
        Left = 3
        Top = 1
        Width = 3
        Height = 15
        Align = alClient
        Transparent = True
        WordWrap = True
      end
    end
  end
end
