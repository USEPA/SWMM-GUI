object ChartOptionsDlg: TChartOptionsDlg
  Left = 410
  Top = 182
  BorderStyle = bsDialog
  BorderWidth = 1
  Caption = 'Graph Options'
  ClientHeight = 407
  ClientWidth = 366
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
  PixelsPerInch = 96
  TextHeight = 15
  object DefaultBox: TCheckBox
    Left = 20
    Top = 331
    Width = 213
    Height = 21
    Caption = 'Make these the default options '
    TabOrder = 1
  end
  object OkBtn: TButton
    Left = 54
    Top = 367
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object CancelBtn: TButton
    Left = 147
    Top = 367
    Width = 74
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object HelpBtn: TButton
    Left = 239
    Top = 367
    Width = 74
    Height = 25
    Caption = 'Help'
    TabOrder = 4
    OnClick = HelpBtnClick
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 4
    Width = 351
    Height = 312
    ActivePage = StylesPage
    TabOrder = 0
    OnChange = PageControl1Change
    object GeneralPage: TTabSheet
      Caption = 'General'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label1: TLabel
        Left = 8
        Top = 24
        Width = 61
        Height = 15
        Caption = 'Panel Color'
      end
      object Label2: TLabel
        Left = 8
        Top = 67
        Width = 123
        Height = 15
        Caption = 'Start Background Color'
      end
      object Label4: TLabel
        Left = 8
        Top = 184
        Width = 90
        Height = 15
        Caption = '3D Effect Percent'
      end
      object Label5: TLabel
        Left = 8
        Top = 219
        Width = 52
        Height = 15
        Caption = 'Main Title'
      end
      object Label8: TLabel
        Left = 8
        Top = 111
        Width = 119
        Height = 15
        Caption = 'End Background Color'
      end
      object PanelColorBox: TColorBox
        Left = 155
        Top = 21
        Width = 147
        Height = 22
        DefaultColorColor = clBtnFace
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbPrettyNames]
        Ctl3D = True
        ItemHeight = 20
        ParentCtl3D = False
        TabOrder = 0
      end
      object BackColorBox1: TColorBox
        Left = 155
        Top = 64
        Width = 147
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbPrettyNames]
        ItemHeight = 20
        TabOrder = 1
      end
      object View3DBox: TCheckBox
        Left = 8
        Top = 145
        Width = 161
        Height = 21
        Alignment = taLeftJustify
        Caption = 'View in 3-D'
        TabOrder = 3
      end
      object GraphTitleBox: TEdit
        Left = 8
        Top = 239
        Width = 294
        Height = 23
        TabOrder = 6
      end
      object Pct3DUpDown: TUpDown
        Left = 204
        Top = 181
        Width = 16
        Height = 23
        Associate = Pct3DEdit
        Min = 1
        Increment = 5
        Position = 15
        TabOrder = 5
        TabStop = True
      end
      object Pct3DEdit: TEdit
        Left = 155
        Top = 181
        Width = 49
        Height = 23
        NumbersOnly = True
        TabOrder = 4
        Text = '15'
      end
      object BackColorBox2: TColorBox
        Left = 155
        Top = 108
        Width = 147
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbPrettyNames]
        ItemHeight = 20
        TabOrder = 2
      end
      object GraphTitleFontLabel: TLinkLabel
        Left = 219
        Top = 217
        Width = 81
        Height = 19
        Caption = '<a>Change Font...</a>'
        TabOrder = 7
        TabStop = True
        OnLinkClick = GraphTitleFontLabelLinkClick
      end
    end
    object XaxisPage: TTabSheet
      Caption = 'Axes'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object XminLabel: TLabel
        Left = 8
        Top = 92
        Width = 53
        Height = 15
        Caption = 'Minimum'
      end
      object XmaxLabel: TLabel
        Left = 8
        Top = 132
        Width = 55
        Height = 15
        Caption = 'Maximum'
      end
      object XIncrementLabel: TLabel
        Left = 8
        Top = 172
        Width = 54
        Height = 15
        Caption = 'Increment'
      end
      object Label11: TLabel
        Left = 8
        Top = 214
        Width = 47
        Height = 15
        Caption = 'Axis Title'
      end
      object XDataMinLabel: TLabel
        Left = 228
        Top = 93
        Width = 80
        Height = 15
        Caption = 'XDataMinLabel'
      end
      object XDataMaxLabel: TLabel
        Left = 228
        Top = 133
        Width = 82
        Height = 15
        Caption = 'XDataMaxLabel'
      end
      object XFormatLabel: TLabel
        Left = 8
        Top = 152
        Width = 38
        Height = 15
        Caption = 'Format'
      end
      object Bevel1: TBevel
        Left = 16
        Top = 42
        Width = 300
        Height = 3
        Shape = bsBottomLine
      end
      object Xmin: TEdit
        Left = 118
        Top = 89
        Width = 86
        Height = 23
        TabOrder = 6
      end
      object Xmax: TEdit
        Left = 118
        Top = 129
        Width = 86
        Height = 23
        TabOrder = 7
      end
      object Xinc: TEdit
        Left = 118
        Top = 169
        Width = 86
        Height = 23
        TabOrder = 8
      end
      object Xtitle: TEdit
        Left = 8
        Top = 233
        Width = 313
        Height = 23
        TabOrder = 10
      end
      object Xgrid: TCheckBox
        Left = 8
        Top = 54
        Width = 87
        Height = 21
        Caption = 'Grid Lines'
        TabOrder = 3
      end
      object DateFmtCombo: TComboBox
        Left = 118
        Top = 205
        Width = 86
        Height = 24
        Style = csOwnerDrawFixed
        ItemHeight = 18
        TabOrder = 9
      end
      object AxisBtn1: TRadioButton
        Left = 6
        Top = 16
        Width = 109
        Height = 17
        Caption = 'Bottom Axis'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = AxisBtn1Click
      end
      object AxisBtn2: TRadioButton
        Left = 118
        Top = 16
        Width = 109
        Height = 17
        Caption = 'Left Axis'
        TabOrder = 1
        OnClick = AxisBtn1Click
      end
      object AxisBtn3: TRadioButton
        Left = 228
        Top = 16
        Width = 109
        Height = 17
        Caption = 'Right Axis'
        TabOrder = 2
        OnClick = AxisBtn1Click
      end
      object InvertedBox: TCheckBox
        Left = 228
        Top = 56
        Width = 100
        Height = 17
        Caption = 'Inverted'
        TabOrder = 5
      end
      object AxisFontLabel: TLinkLabel
        Left = 228
        Top = 212
        Width = 81
        Height = 19
        Caption = '<a>Change Font...</a>'
        TabOrder = 11
        TabStop = True
        OnLinkClick = AxisFontLabelLinkClick
      end
      object Xauto: TLinkLabel
        Left = 118
        Top = 58
        Width = 62
        Height = 19
        Caption = '<a>Auto-Scale</a>'
        TabOrder = 4
        TabStop = True
        OnLinkClick = XautoLinkClick
      end
    end
    object LegendPage: TTabSheet
      Caption = 'Legend'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label18: TLabel
        Left = 8
        Top = 35
        Width = 43
        Height = 15
        Caption = 'Position'
      end
      object Label19: TLabel
        Left = 8
        Top = 75
        Width = 29
        Height = 15
        Caption = 'Color'
      end
      object Label3: TLabel
        Left = 8
        Top = 216
        Width = 176
        Height = 15
        Caption = 'Symbol Width (% of Label Width)'
      end
      object LegendFrameBox: TCheckBox
        Left = 8
        Top = 143
        Width = 121
        Height = 21
        Alignment = taLeftJustify
        Caption = 'Framed'
        TabOrder = 3
      end
      object LegendVisibleBox: TCheckBox
        Left = 168
        Top = 176
        Width = 121
        Height = 21
        Alignment = taLeftJustify
        Caption = 'Visible'
        TabOrder = 6
      end
      object LegendPosBox: TComboBox
        Left = 116
        Top = 32
        Width = 146
        Height = 24
        Style = csOwnerDrawFixed
        ItemHeight = 18
        TabOrder = 0
      end
      object LegendColorBox: TColorBox
        Left = 116
        Top = 72
        Width = 146
        Height = 22
        DefaultColorColor = clBtnFace
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbPrettyNames]
        ItemHeight = 20
        TabOrder = 1
      end
      object LegendCheckBox: TCheckBox
        Left = 8
        Top = 112
        Width = 121
        Height = 21
        Alignment = taLeftJustify
        Caption = 'Check Boxes'
        TabOrder = 2
      end
      object LegendShadowBox: TCheckBox
        Left = 168
        Top = 143
        Width = 121
        Height = 21
        Alignment = taLeftJustify
        Caption = 'Shadowed'
        TabOrder = 4
      end
      object LegendTransparentBox: TCheckBox
        Left = 8
        Top = 176
        Width = 121
        Height = 21
        Alignment = taLeftJustify
        Caption = 'Transparent'
        TabOrder = 5
      end
      inline LegendWidthSpinner: TUpDnEditBox
        Left = 235
        Top = 212
        Width = 54
        Height = 23
        AutoSize = True
        TabOrder = 7
        ExplicitLeft = 235
        ExplicitTop = 212
        ExplicitWidth = 54
        ExplicitHeight = 23
        inherited Spinner: TUpDown
          Left = 38
          Height = 23
          Min = 5
          Increment = 5
          Position = 20
          ExplicitLeft = 38
          ExplicitHeight = 23
        end
        inherited EditBox: TEdit
          Width = 38
          Height = 23
          Text = '20'
          ExplicitWidth = 38
          ExplicitHeight = 23
        end
      end
    end
    object StylesPage: TTabSheet
      Caption = 'Styles'
      ImageIndex = 4
      OnExit = StylesPageExit
      object Label21: TLabel
        Left = 8
        Top = 19
        Width = 30
        Height = 15
        Caption = 'Series'
      end
      object Label22: TLabel
        Left = 8
        Top = 62
        Width = 64
        Height = 15
        Caption = 'Legend Title'
      end
      object SeriesComboBox: TComboBox
        Left = 96
        Top = 16
        Width = 91
        Height = 24
        Style = csOwnerDrawFixed
        ItemHeight = 18
        TabOrder = 0
        OnClick = SeriesComboBoxClick
      end
      object SeriesTitle: TEdit
        Left = 96
        Top = 56
        Width = 227
        Height = 23
        TabOrder = 1
      end
      object Panel6: TPanel
        Left = 8
        Top = 96
        Width = 319
        Height = 178
        BevelOuter = bvNone
        Caption = 'Panel1'
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 3
        object PageControl2: TPageControl
          Left = 0
          Top = 0
          Width = 319
          Height = 178
          ActivePage = AreaOptionsSheet
          Align = alClient
          TabOrder = 0
          object LineOptionsSheet: TTabSheet
            Caption = 'Lines'
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object Label23: TLabel
              Left = 55
              Top = 14
              Width = 25
              Height = 15
              Caption = 'Style'
            end
            object Label24: TLabel
              Left = 55
              Top = 51
              Width = 29
              Height = 15
              Caption = 'Color'
            end
            object Label25: TLabel
              Left = 55
              Top = 94
              Width = 20
              Height = 15
              Caption = 'Size'
            end
            object LineStyleBox: TComboBox
              Left = 111
              Top = 11
              Width = 146
              Height = 24
              Style = csOwnerDrawFixed
              ItemHeight = 18
              TabOrder = 0
            end
            object LineColorBox: TColorBox
              Left = 111
              Top = 48
              Width = 146
              Height = 22
              Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
              ItemHeight = 20
              TabOrder = 1
            end
            object LineVisibleBox: TCheckBox
              Left = 55
              Top = 125
              Width = 69
              Height = 21
              Alignment = taLeftJustify
              Caption = 'Visible'
              TabOrder = 4
            end
            object LineSizeEdit: TEdit
              Left = 111
              Top = 91
              Width = 32
              Height = 23
              Ctl3D = True
              NumbersOnly = True
              ParentCtl3D = False
              TabOrder = 2
              Text = '1'
            end
            object LineSizeUpDown: TUpDown
              Left = 143
              Top = 91
              Width = 16
              Height = 23
              Associate = LineSizeEdit
              Min = 1
              Max = 10
              Position = 1
              TabOrder = 3
            end
          end
          object MarkOptionsSheet: TTabSheet
            Caption = 'Markers'
            ImageIndex = 1
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object Label26: TLabel
              Left = 55
              Top = 14
              Width = 25
              Height = 15
              Caption = 'Style'
            end
            object Label27: TLabel
              Left = 55
              Top = 51
              Width = 29
              Height = 15
              Caption = 'Color'
            end
            object Label28: TLabel
              Left = 55
              Top = 94
              Width = 20
              Height = 15
              Caption = 'Size'
            end
            object MarkVisibleBox: TCheckBox
              Left = 55
              Top = 125
              Width = 69
              Height = 21
              Alignment = taLeftJustify
              Caption = 'Visible'
              TabOrder = 4
            end
            object MarkStyleBox: TComboBox
              Left = 111
              Top = 11
              Width = 146
              Height = 24
              Style = csOwnerDrawFixed
              ItemHeight = 18
              TabOrder = 0
            end
            object MarkColorBox: TColorBox
              Left = 111
              Top = 48
              Width = 146
              Height = 22
              Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
              ItemHeight = 20
              TabOrder = 1
            end
            object MarkSizeEdit: TEdit
              Left = 111
              Top = 91
              Width = 32
              Height = 23
              Ctl3D = True
              NumbersOnly = True
              ParentCtl3D = False
              TabOrder = 2
              Text = '1'
            end
            object MarkSizeUpDown: TUpDown
              Left = 143
              Top = 91
              Width = 16
              Height = 23
              Associate = MarkSizeEdit
              Min = 1
              Max = 10
              Position = 1
              TabOrder = 3
            end
          end
          object AreaOptionsSheet: TTabSheet
            Caption = 'Patterns'
            ImageIndex = 2
            object Label29: TLabel
              Left = 47
              Top = 14
              Width = 45
              Height = 15
              AutoSize = False
              Caption = 'Style'
            end
            object Label30: TLabel
              Left = 47
              Top = 51
              Width = 29
              Height = 15
              Caption = 'Color'
            end
            object Label31: TLabel
              Left = 47
              Top = 90
              Width = 57
              Height = 15
              AutoSize = False
              Caption = 'Stacking'
            end
            object AreaFillStyleBox: TComboBox
              Left = 118
              Top = 11
              Width = 147
              Height = 24
              Style = csOwnerDrawFixed
              ItemHeight = 18
              TabOrder = 0
            end
            object AreaColorBox: TColorBox
              Left = 118
              Top = 48
              Width = 147
              Height = 26
              Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
              ItemHeight = 20
              TabOrder = 1
            end
            object StackStyleBox: TComboBox
              Left = 118
              Top = 87
              Width = 147
              Height = 24
              Style = csOwnerDrawFixed
              ItemHeight = 18
              TabOrder = 2
            end
          end
          object PieOptionsSheet: TTabSheet
            Caption = 'Pie Options'
            ImageIndex = 3
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object Label32: TLabel
              Left = 80
              Top = 96
              Width = 79
              Height = 15
              Caption = 'Rotation Angle'
            end
            object PieCircledBox: TCheckBox
              Left = 80
              Top = 24
              Width = 114
              Height = 21
              Alignment = taLeftJustify
              Caption = 'Circular'
              TabOrder = 0
            end
            object PiePatternBox: TCheckBox
              Left = 80
              Top = 56
              Width = 114
              Height = 21
              Alignment = taLeftJustify
              Caption = 'Use Patterns'
              TabOrder = 1
            end
            object PieRotateEdit: TEdit
              Left = 185
              Top = 91
              Width = 32
              Height = 23
              Ctl3D = True
              NumbersOnly = True
              ParentCtl3D = False
              TabOrder = 2
              Text = '0'
            end
            object PieRotateUpDown: TUpDown
              Left = 217
              Top = 91
              Width = 16
              Height = 23
              Associate = PieRotateEdit
              Max = 360
              Increment = 10
              TabOrder = 3
            end
          end
          object LabelsOptionsSheet: TTabSheet
            Caption = 'Labels'
            ImageIndex = 4
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object Label33: TLabel
              Left = 43
              Top = 14
              Width = 25
              Height = 15
              Caption = 'Style'
            end
            object Label34: TLabel
              Left = 43
              Top = 51
              Width = 29
              Height = 15
              Caption = 'Color'
            end
            object LabelsStyleBox: TComboBox
              Left = 122
              Top = 11
              Width = 147
              Height = 24
              Style = csOwnerDrawFixed
              ItemHeight = 18
              TabOrder = 0
            end
            object LabelsBackColorBox: TColorBox
              Left = 122
              Top = 48
              Width = 147
              Height = 22
              Style = [cbStandardColors, cbExtendedColors, cbPrettyNames]
              ItemHeight = 20
              TabOrder = 1
            end
            object LabelsTransparentBox: TCheckBox
              Left = 43
              Top = 83
              Width = 96
              Height = 21
              Alignment = taLeftJustify
              Caption = 'Transparent'
              TabOrder = 2
            end
            object LabelsVisibleBox: TCheckBox
              Left = 43
              Top = 110
              Width = 96
              Height = 21
              Alignment = taLeftJustify
              Caption = 'Visible'
              TabOrder = 3
            end
          end
        end
      end
      object LegendFontLabel: TLinkLabel
        Left = 242
        Top = 35
        Width = 81
        Height = 19
        Caption = '<a>Change Font...</a>'
        TabOrder = 2
        TabStop = True
        OnLinkClick = LegendFontLabelLinkClick
      end
    end
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 440
    Top = 432
  end
end
