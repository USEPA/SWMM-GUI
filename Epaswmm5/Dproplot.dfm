object ProfilePlotOptionsForm: TProfilePlotOptionsForm
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Profile Plot Options'
  ClientHeight = 335
  ClientWidth = 361
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
  object OKBtn: TButton
    Left = 55
    Top = 296
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 2
  end
  object CancelBtn: TButton
    Left = 143
    Top = 296
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object HelpBtn: TButton
    Left = 231
    Top = 296
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 4
    OnClick = HelpBtnClick
  end
  object DefaultBox: TCheckBox
    Left = 30
    Top = 261
    Width = 209
    Height = 25
    Caption = 'Make these the default options'
    TabOrder = 1
    WordWrap = True
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 345
    Height = 249
    ActivePage = TabSheet1
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Colors'
      object Label1: TLabel
        Left = 16
        Top = 32
        Width = 53
        Height = 15
        Caption = 'Plot Panel'
      end
      object Label2: TLabel
        Left = 16
        Top = 72
        Width = 88
        Height = 15
        Caption = 'Plot Background'
      end
      object Label3: TLabel
        Left = 16
        Top = 112
        Width = 84
        Height = 15
        Caption = 'Conduit Interior'
      end
      object Label4: TLabel
        Left = 16
        Top = 152
        Width = 66
        Height = 15
        Caption = 'Water Depth'
      end
      object ColorBox1: TColorBox
        Left = 168
        Top = 24
        Width = 145
        Height = 24
        DefaultColorColor = clBtnFace
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbPrettyNames]
        BevelKind = bkFlat
        Ctl3D = True
        ItemHeight = 18
        ParentCtl3D = False
        TabOrder = 0
      end
      object ColorBox2: TColorBox
        Left = 168
        Top = 64
        Width = 145
        Height = 24
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbPrettyNames]
        BevelKind = bkFlat
        Ctl3D = True
        ItemHeight = 18
        ParentCtl3D = False
        TabOrder = 1
      end
      object ColorBox3: TColorBox
        Left = 168
        Top = 104
        Width = 145
        Height = 24
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbPrettyNames]
        BevelKind = bkFlat
        Ctl3D = True
        ItemHeight = 18
        ParentCtl3D = False
        TabOrder = 2
      end
      object ColorBox4: TColorBox
        Left = 168
        Top = 144
        Width = 145
        Height = 24
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbPrettyNames]
        BevelKind = bkFlat
        Ctl3D = True
        ItemHeight = 18
        ParentCtl3D = False
        TabOrder = 3
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Styles'
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object CheckBox1: TCheckBox
        Left = 16
        Top = 112
        Width = 159
        Height = 21
        Alignment = taLeftJustify
        Caption = 'Display Conduits Only'
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 2
      end
      object CheckBox2: TCheckBox
        Left = 16
        Top = 24
        Width = 159
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Use Thick Lines'
        TabOrder = 0
      end
      object CheckBox3: TCheckBox
        Left = 16
        Top = 68
        Width = 159
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Display Ground Profile'
        TabOrder = 1
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Axes'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label7: TLabel
        Left = 16
        Top = 15
        Width = 52
        Height = 15
        Caption = 'Main Title'
      end
      object Label8: TLabel
        Left = 16
        Top = 71
        Width = 80
        Height = 15
        Caption = 'Horizontal Axis'
      end
      object Label9: TLabel
        Left = 16
        Top = 127
        Width = 63
        Height = 15
        Caption = 'Vertical Axis'
      end
      object MainTitleEdit: TEdit
        Left = 16
        Top = 34
        Width = 305
        Height = 23
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 0
      end
      object HorizAxisEdit: TEdit
        Left = 16
        Top = 90
        Width = 305
        Height = 23
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 2
      end
      object VertAxisEdit: TEdit
        Left = 16
        Top = 146
        Width = 305
        Height = 23
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 4
      end
      object HorizGridBox: TCheckBox
        Left = 16
        Top = 192
        Width = 151
        Height = 21
        Caption = 'Horizontal Grid Lines'
        TabOrder = 5
      end
      object VertGridBox: TCheckBox
        Left = 184
        Top = 192
        Width = 151
        Height = 21
        Caption = 'Vertical Grid Lines'
        TabOrder = 6
      end
      object MainTitleFontLabel: TLinkLabel
        Left = 258
        Top = 15
        Width = 62
        Height = 19
        Caption = '<a>Title Font...</a>'
        TabOrder = 1
        TabStop = True
        OnClick = MainTitleFontLabelClick
      end
      object AxisFontLabel: TLinkLabel
        Left = 257
        Top = 127
        Width = 65
        Height = 19
        Caption = '<a>Axes Font...</a>'
        TabOrder = 3
        TabStop = True
        OnClick = AxisFontLabelClick
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Vertical Scale'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label10: TLabel
        Left = 32
        Top = 38
        Width = 53
        Height = 15
        Caption = 'Minimum'
      end
      object Label11: TLabel
        Left = 32
        Top = 78
        Width = 55
        Height = 15
        Caption = 'Maximum'
      end
      object Label12: TLabel
        Left = 32
        Top = 118
        Width = 54
        Height = 15
        Caption = 'Increment'
      end
      object YminLabel: TLabel
        Left = 232
        Top = 38
        Width = 27
        Height = 15
        Caption = 'Ymin'
      end
      object YmaxLabel: TLabel
        Left = 232
        Top = 78
        Width = 29
        Height = 15
        Caption = 'Ymax'
      end
      object YminEdit: TNumEdit
        Left = 112
        Top = 35
        Width = 97
        Height = 23
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 0
        Style = esNumber
        Modified = False
        SelLength = 0
        SelStart = 0
      end
      object YmaxEdit: TNumEdit
        Left = 112
        Top = 72
        Width = 97
        Height = 23
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 1
        Style = esNumber
        Modified = False
        SelLength = 0
        SelStart = 0
      end
      object YincEdit: TNumEdit
        Left = 112
        Top = 112
        Width = 97
        Height = 23
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 2
        Style = esNumber
        Modified = False
        SelLength = 0
        SelStart = 0
      end
      object AutoScaleCheckBox: TCheckBox
        Left = 32
        Top = 158
        Width = 93
        Height = 17
        Alignment = taLeftJustify
        Caption = 'AutoScale'
        TabOrder = 3
        OnClick = AutoScaleCheckBoxClick
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Node Labels'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label5: TLabel
        Left = 16
        Top = 116
        Width = 72
        Height = 15
        Caption = 'Arrow Length'
      end
      object Label6: TLabel
        Left = 16
        Top = 160
        Width = 47
        Height = 15
        Caption = 'Font Size'
      end
      object LabelsOnAxisBox: TCheckBox
        Left = 16
        Top = 32
        Width = 145
        Height = 21
        Alignment = taLeftJustify
        Caption = 'Display on Top Axis'
        TabOrder = 0
      end
      object LabelsOnPlotBox: TCheckBox
        Left = 16
        Top = 72
        Width = 145
        Height = 21
        Alignment = taLeftJustify
        Caption = 'Display on Plot'
        TabOrder = 1
      end
      inline LabelSizeSpin: TUpDnEditBox
        Left = 148
        Top = 153
        Width = 57
        Height = 23
        AutoSize = True
        TabOrder = 3
        ExplicitLeft = 148
        ExplicitTop = 153
        ExplicitWidth = 57
        ExplicitHeight = 23
        inherited Spinner: TUpDown
          Left = 41
          Height = 23
          Min = 6
          Max = 12
          Position = 9
          TabOrder = 1
          ExplicitLeft = 41
          ExplicitHeight = 23
        end
        inherited EditBox: TEdit
          Width = 41
          Height = 23
          ParentCtl3D = False
          TabOrder = 0
          Text = '9'
          ExplicitWidth = 41
          ExplicitHeight = 23
        end
      end
      inline ArrowLengthSpin: TUpDnEditBox
        Left = 148
        Top = 112
        Width = 57
        Height = 23
        AutoSize = True
        TabOrder = 2
        ExplicitLeft = 148
        ExplicitTop = 112
        ExplicitWidth = 57
        ExplicitHeight = 23
        inherited Spinner: TUpDown
          Left = 41
          Height = 23
          TabOrder = 1
          ExplicitLeft = 41
          ExplicitHeight = 23
        end
        inherited EditBox: TEdit
          Width = 41
          Height = 23
          ParentCtl3D = False
          TabOrder = 0
          ExplicitWidth = 41
          ExplicitHeight = 23
        end
      end
    end
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Options = [fdAnsiOnly, fdEffects]
    Left = 424
    Top = 352
  end
end
