object AnalysisOptionsForm: TAnalysisOptionsForm
  Left = 465
  Top = 171
  BorderStyle = bsDialog
  Caption = 'Simulation Options'
  ClientHeight = 503
  ClientWidth = 423
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
    Left = 8
    Top = 8
    Width = 403
    Height = 441
    ActivePage = TabSheet4
    TabOrder = 0
    OnChange = PageControl1Change
    OnChanging = PageControl1Changing
    object TabSheet1: TTabSheet
      Caption = 'General'
      object ModelsGroup: TGroupBox
        Left = 16
        Top = 32
        Width = 169
        Height = 208
        Caption = 'Process Models'
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 0
        object RainfallBox: TCheckBox
          Left = 8
          Top = 24
          Width = 145
          Height = 21
          Caption = 'Rainfall/Runoff'
          TabOrder = 0
          OnClick = EditChange
        end
        object SnowMeltBox: TCheckBox
          Left = 8
          Top = 85
          Width = 145
          Height = 21
          Caption = 'Snow Melt'
          TabOrder = 1
          OnClick = EditChange
        end
        object GroundwaterBox: TCheckBox
          Left = 8
          Top = 115
          Width = 145
          Height = 21
          Caption = 'Groundwater'
          TabOrder = 2
          OnClick = EditChange
        end
        object FlowRoutingBox: TCheckBox
          Left = 8
          Top = 146
          Width = 145
          Height = 21
          Caption = 'Flow Routing'
          TabOrder = 3
          OnClick = EditChange
        end
        object WaterQualityBox: TCheckBox
          Left = 8
          Top = 177
          Width = 145
          Height = 21
          Caption = 'Water Quality'
          TabOrder = 4
          OnClick = EditChange
        end
        object RDIIBox: TCheckBox
          Left = 8
          Top = 54
          Width = 145
          Height = 21
          Caption = 'Rainfall Dependent I/I'
          TabOrder = 5
          OnClick = EditChange
        end
      end
      object MiscGroup: TGroupBox
        Left = 200
        Top = 258
        Width = 177
        Height = 121
        Caption = 'Routing Options'
        TabOrder = 3
        object Label8: TLabel
          Left = 8
          Top = 63
          Width = 131
          Height = 15
          Caption = 'Minimum Conduit Slope'
        end
        object Label13: TLabel
          Left = 80
          Top = 87
          Width = 18
          Height = 15
          Caption = '(%)'
        end
        object AllowPondingBox: TCheckBox
          Left = 8
          Top = 28
          Width = 157
          Height = 21
          Caption = 'Allow Ponding'
          TabOrder = 0
          OnClick = EditChange
        end
        object MinSlopeEdit: TNumEdit
          Left = 7
          Top = 83
          Width = 65
          Height = 23
          TabOrder = 1
          Text = '0'
          OnChange = EditChange
          Style = esPosNumber
          Modified = False
          SelLength = 0
          SelStart = 0
        end
      end
      object InfilModelsGroup: TRadioGroup
        Left = 200
        Top = 32
        Width = 177
        Height = 208
        Caption = 'Infiltration Model'
        Items.Strings = (
          'Horton'
          'Modified Horton'
          'Green-Ampt'
          'Modified Green-Ampt'
          'Curve Number')
        TabOrder = 1
        OnClick = EditChange
      end
      object RoutingMethodsGroup: TRadioGroup
        Left = 16
        Top = 258
        Width = 169
        Height = 121
        Caption = 'Routing Model'
        Items.Strings = (
          'Steady Flow'
          'Kinematic Wave'
          'Dynamic Wave')
        TabOrder = 2
        OnClick = EditChange
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Dates'
      ImageIndex = 1
      object Label6: TLabel
        Left = 40
        Top = 254
        Width = 61
        Height = 15
        Caption = 'Antecedent'
      end
      object Label17: TLabel
        Left = 40
        Top = 62
        Width = 87
        Height = 15
        Caption = 'Start Analysis on'
      end
      object Label18: TLabel
        Left = 40
        Top = 102
        Width = 96
        Height = 15
        Caption = 'Start Reporting on'
      end
      object Label1: TLabel
        Left = 40
        Top = 142
        Width = 83
        Height = 15
        Caption = 'End Analysis on'
      end
      object Label3: TLabel
        Left = 160
        Top = 32
        Width = 71
        Height = 15
        Caption = 'Date (M/D/Y)'
      end
      object Label4: TLabel
        Left = 272
        Top = 32
        Width = 60
        Height = 15
        Caption = 'Time (H:M)'
      end
      object Label7: TLabel
        Left = 40
        Top = 270
        Width = 46
        Height = 15
        Caption = 'Dry Days'
      end
      object Label21: TLabel
        Left = 40
        Top = 184
        Width = 95
        Height = 15
        Caption = 'Start Sweeping on'
      end
      object Label23: TLabel
        Left = 40
        Top = 224
        Width = 91
        Height = 15
        Caption = 'End Sweeping on'
      end
      object DryDaysEdit: TNumEdit
        Left = 160
        Top = 256
        Width = 41
        Height = 23
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 8
        Text = '0'
        OnChange = EditChange
        Style = esPosNumber
        Modified = False
        SelLength = 0
        SelStart = 0
      end
      object StartDatePicker: TDateTimePicker
        Left = 160
        Top = 56
        Width = 97
        Height = 23
        Date = 37914.000000000000000000
        Time = 0.027740509249269960
        DateMode = dmUpDown
        TabOrder = 0
        OnChange = EditChange
      end
      object RptDatePicker: TDateTimePicker
        Left = 160
        Top = 96
        Width = 97
        Height = 23
        Date = 37914.000000000000000000
        Time = 0.027740509249269960
        DateMode = dmUpDown
        TabOrder = 2
        OnChange = EditChange
      end
      object EndDatePicker: TDateTimePicker
        Left = 160
        Top = 136
        Width = 97
        Height = 23
        Date = 37914.000000000000000000
        Time = 0.027740509249269960
        DateMode = dmUpDown
        TabOrder = 4
        OnChange = EditChange
      end
      object StartTimePicker: TDateTimePicker
        Left = 272
        Top = 56
        Width = 65
        Height = 23
        Date = 37914.000000000000000000
        Format = 'HH:mm'
        Time = 37914.000000000000000000
        Kind = dtkTime
        TabOrder = 1
        OnChange = EditChange
      end
      object RptTimePicker: TDateTimePicker
        Left = 272
        Top = 96
        Width = 65
        Height = 23
        Date = 37914.000000000000000000
        Format = 'HH:mm'
        Time = 37914.000000000000000000
        Kind = dtkTime
        TabOrder = 3
        OnChange = EditChange
      end
      object EndTimePicker: TDateTimePicker
        Left = 272
        Top = 136
        Width = 65
        Height = 23
        Date = 37914.000000000000000000
        Format = 'HH:mm'
        Time = 37914.000000000000000000
        Kind = dtkTime
        TabOrder = 5
        OnChange = EditChange
      end
      object SweepStartPicker: TDateTimePicker
        Left = 160
        Top = 176
        Width = 65
        Height = 23
        Date = 17168.000000000000000000
        Format = 'MM/dd'
        Time = 0.027740509249269960
        DateMode = dmUpDown
        TabOrder = 6
        OnChange = EditChange
      end
      object SweepEndPicker: TDateTimePicker
        Left = 160
        Top = 216
        Width = 65
        Height = 23
        Date = 17532.000000000000000000
        Format = 'MM/dd'
        Time = 0.027740509249269960
        DateMode = dmUpDown
        TabOrder = 7
        OnChange = EditChange
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Time Steps'
      ImageIndex = 4
      object Label12: TLabel
        Left = 56
        Top = 60
        Width = 78
        Height = 15
        Caption = 'Reporting Step'
      end
      object Label11: TLabel
        Left = 56
        Top = 218
        Width = 122
        Height = 15
        Caption = 'Routing Step (seconds)'
      end
      object Label24: TLabel
        Left = 160
        Top = 32
        Width = 25
        Height = 15
        Caption = 'Days'
      end
      object Label27: TLabel
        Left = 224
        Top = 32
        Width = 58
        Height = 15
        Caption = 'Hr:Min:Sec'
      end
      object Label10: TLabel
        Left = 56
        Top = 105
        Width = 65
        Height = 15
        Caption = 'Dry Weather'
      end
      object Label9: TLabel
        Left = 56
        Top = 146
        Width = 68
        Height = 15
        Caption = 'Wet Weather'
      end
      object Label5: TLabel
        Left = 56
        Top = 90
        Width = 65
        Height = 15
        Caption = 'Runoff Step:'
      end
      object Label16: TLabel
        Left = 56
        Top = 132
        Width = 65
        Height = 15
        Caption = 'Runoff Step:'
      end
      object Label31: TLabel
        Left = 56
        Top = 182
        Width = 92
        Height = 15
        Caption = 'Control Rule Step'
      end
      object RptStepPicker: TDateTimePicker
        Left = 224
        Top = 53
        Width = 81
        Height = 23
        Date = 37914.000000000000000000
        Format = 'HH:mm:ss'
        Time = 37914.000000000000000000
        Kind = dtkTime
        TabOrder = 1
        OnChange = EditChange
      end
      object DryStepPicker: TDateTimePicker
        Left = 224
        Top = 94
        Width = 81
        Height = 23
        Date = 37914.000000000000000000
        Format = 'HH:mm:ss'
        Time = 37914.000000000000000000
        Kind = dtkTime
        TabOrder = 3
        OnChange = EditChange
      end
      object WetStepPicker: TDateTimePicker
        Left = 224
        Top = 134
        Width = 81
        Height = 23
        Date = 37914.000000000000000000
        Format = 'HH:mm:ss'
        Time = 37914.000000000000000000
        Kind = dtkTime
        TabOrder = 5
        OnChange = EditChange
      end
      object DryDaysPicker: TDateTimePicker
        Left = 160
        Top = 93
        Width = 49
        Height = 23
        Date = 37914.000000000000000000
        Format = 'm'
        Time = 37914.000000000000000000
        Kind = dtkTime
        TabOrder = 2
        OnChange = EditChange
      end
      object WetDaysPicker: TDateTimePicker
        Left = 160
        Top = 133
        Width = 49
        Height = 23
        Date = 37914.000000000000000000
        Format = 'm'
        Time = 37914.000000000000000000
        Kind = dtkTime
        TabOrder = 4
        OnChange = EditChange
      end
      object RptDaysPicker: TDateTimePicker
        Left = 160
        Top = 53
        Width = 49
        Height = 23
        Date = 37914.000000000000000000
        Format = 'm'
        Time = 37914.000000000000000000
        Kind = dtkTime
        TabOrder = 0
        OnChange = EditChange
      end
      object RouteStepEdit: TNumEdit
        Left = 224
        Top = 215
        Width = 81
        Height = 23
        TabOrder = 7
        Text = '0'
        OnChange = EditChange
        Style = esPosNumber
        Modified = False
        SelLength = 0
        SelStart = 0
      end
      object GroupBox1: TGroupBox
        Left = 56
        Top = 248
        Width = 265
        Height = 129
        Caption = 'Steady Flow Periods'
        TabOrder = 8
        object Label14: TLabel
          Left = 16
          Top = 93
          Width = 137
          Height = 15
          Caption = 'Lateral Flow Tolerance (%)'
        end
        object Label19: TLabel
          Left = 16
          Top = 61
          Width = 140
          Height = 15
          Caption = 'System Flow Tolerance (%)'
        end
        inline SysFlowTolSpinner: TUpDnEditBox
          Left = 191
          Top = 58
          Width = 57
          Height = 23
          AutoSize = True
          TabOrder = 1
          ExplicitLeft = 191
          ExplicitTop = 58
          ExplicitWidth = 57
          ExplicitHeight = 23
          inherited Spinner: TUpDown
            Left = 41
            Height = 23
            Position = 5
            ExplicitLeft = 41
            ExplicitHeight = 23
          end
          inherited EditBox: TEdit
            Width = 41
            Height = 23
            Text = '5'
            OnChange = EditChange
            ExplicitWidth = 41
            ExplicitHeight = 23
          end
        end
        inline LatFlowTolSpinner: TUpDnEditBox
          Left = 191
          Top = 90
          Width = 57
          Height = 23
          AutoSize = True
          TabOrder = 2
          ExplicitLeft = 191
          ExplicitTop = 90
          ExplicitWidth = 57
          ExplicitHeight = 23
          inherited Spinner: TUpDown
            Left = 41
            Height = 23
            Position = 5
            ExplicitLeft = 41
            ExplicitHeight = 23
          end
          inherited EditBox: TEdit
            Width = 41
            Height = 23
            Text = '5'
            OnChange = EditChange
            ExplicitWidth = 41
            ExplicitHeight = 23
          end
        end
        object SkipSteadyBox: TCheckBox
          Left = 16
          Top = 28
          Width = 146
          Height = 21
          Caption = 'Skip Steady Flow Periods'
          TabOrder = 0
          OnClick = EditChange
        end
      end
      object RuleStepPicker: TDateTimePicker
        Left = 224
        Top = 175
        Width = 81
        Height = 23
        Date = 37914.000000000000000000
        Format = 'HH:mm:ss'
        Time = 37914.000000000000000000
        Kind = dtkTime
        TabOrder = 6
        OnChange = EditChange
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Dynamic Wave'
      ImageIndex = 3
      object Label29: TLabel
        Left = 16
        Top = 218
        Width = 216
        Height = 15
        Caption = 'Time Step For Conduit Lengthening (sec)'
      end
      object MinSurfAreaLabel: TLabel
        Left = 16
        Top = 251
        Width = 226
        Height = 15
        Caption = 'Minimum Nodal Surface Area (square feet)'
      end
      object Label15: TLabel
        Left = 16
        Top = 316
        Width = 159
        Height = 15
        Caption = 'Maximum Trials per Time Step'
      end
      object HeadTolLabel: TLabel
        Left = 16
        Top = 284
        Width = 185
        Height = 15
        Caption = 'Head Convergence Tolerance (feet)'
      end
      object MinTimeStepLabel: TLabel
        Left = 16
        Top = 185
        Width = 180
        Height = 15
        Caption = 'Minimum Variable Time Step (sec)'
      end
      object Label26: TLabel
        Left = 16
        Top = 16
        Width = 70
        Height = 15
        Caption = 'Inertial Terms'
      end
      object Label28: TLabel
        Left = 16
        Top = 50
        Width = 117
        Height = 15
        Caption = 'Normal Flow Criterion'
      end
      object Label30: TLabel
        Left = 16
        Top = 84
        Width = 109
        Height = 15
        Caption = 'Force Main Equation'
      end
      object Label22: TLabel
        Left = 350
        Top = 152
        Width = 10
        Height = 15
        Caption = '%'
      end
      object Label2: TLabel
        Left = 16
        Top = 118
        Width = 98
        Height = 15
        Caption = 'Surcharge Method'
      end
      object ThreadsButton: TSpeedButton
        Left = 200
        Top = 346
        Width = 23
        Height = 22
        Cursor = crHandPoint
        Flat = True
        Glyph.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FDFDFD02F1F1F10EE7E7E718E5E5E51AE5E5E51AE5E5E51AE5E5E51AE5E5
          E51AE5E5E51AE6E6E619EFEFEF10FCFCFC03FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FBFBFB04E4E4E41BCDC2BF46D1948599ED7E63D4EF6C4BF5EF6C4BF5ED7E
          63D4D1948599CCC1BE47E0E0E01FF9F9F906FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00E2BCB260F07A5DEBF5A28FFFFAD1CAFFFCE7E5FFFCE6E5FFF9D1
          C9FFF4A18EFFF0795CEBE2BCB260FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00F9C5B960EF795DF9F7D4CEFFF7E9E9FFF7E8E8FFFCF4F4FFFCF4F4FFF7E8
          E8FFF7E9E9FFF6D1CAFFEF795CF9F9C5B960FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FBEB
          E720E87658EBF3D3CDFFF0E5E5FFEFE4E4FFEFE4E4FFE5613FFFE5613FFFEFE4
          E4FFEFE4E4FFEFE4E4FFEFCDC7FFE87658EBFBEBE720FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00E9A7
          9587E59D8AFFEAE2E2FFE7DFDFFFE7DFDFFFE7DFDFFFD65937FFD65937FFE7DF
          DFFFE7DFDFFFE7DFDFFFE8E0E0FFE19784FFE9A79587FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00D070
          55D0EAD0C8FFE1DADAFFE0D9D9FFE0D9D9FFE0D9D9FFC5502EFFC5502EFFE0D9
          D9FFE0D9D9FFE0D9D9FFE0D9D9FFE1C4BDFFD07055D0FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00BA4F
          2FF5F4EEEDFFE7E5E5FFDAD6D6FFD8D4D4FFD8D4D4FFB74826FFB74826FFD8D4
          D4FFD8D4D4FFD8D4D4FFD8D4D4FFE1D8D6FFBA4F2FF5FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B04B
          2BF5F4EFEDFFEEEEEEFFEAEAEAFFDCDBDBFFD3D1D1FFAD4422FFAD4422FFD2D0
          D0FFD2D0D0FFD2D0D0FFD2D0D0FFDFD8D7FFB04B2BF5FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00BA68
          4CD0EAD5CDFFF0F0F0FFF0F0F0FFF0F0F0FFEBEBEBFFE9E9E9FFE7E6E6FFD2D1
          D1FFCECECEFFCECECEFFD2D1D1FFDCC7C0FFBA684CD0FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00D7A2
          9087D49D8AFFF5F5F5FFF3F3F3FFF3F3F3FFF3F3F3FFB34F2DFFB34F2DFFF3F3
          F3FFF3F3F3FFF3F3F3FFF5F5F5FFD49C89FFD7A29087FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7EB
          E620CA7355EBF2DFD9FFF8F8F8FFF7F7F7FFF7F7F7FFC15D3BFFC15D3BFFF7F7
          F7FFF7F7F7FFF8F8F8FFF2DFD9FFCA7355EBF7EBE620FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00EDC7BA60D88265F9F6E3DCFFFCFCFCFFFBFBFBFFFBFBFBFFFBFBFBFFFBFB
          FBFFFCFCFCFFF6E3DCFFD88265F9EDC7BA60FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00F3CDC060E58E70EBEFB7A4FFFAE4DDFFFEF9F7FFFEF9F7FFFAE4
          DDFFEFB7A4FFE58E70EBF3CDC060FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FCF0EB20F5BFAD87F09C81D0ED8B6AF5ED8B6AF5F09C
          81D0F5BFAD87FCF0EB20FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00}
        Layout = blGlyphRight
        ParentShowHint = False
        ShowHint = False
        OnClick = ThreadsButtonClick
      end
      object Label20: TLabel
        Left = 16
        Top = 349
        Width = 179
        Height = 15
        Caption = 'Number of Parallel Threads to Use'
      end
      object LengthenStepEdit: TNumEdit
        Left = 281
        Top = 215
        Width = 81
        Height = 23
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 7
        Text = '0'
        OnChange = EditChange
        Style = esPosNumber
        Modified = False
        SelLength = 0
        SelStart = 0
      end
      object MinSurfAreaEdit: TNumEdit
        Left = 281
        Top = 248
        Width = 81
        Height = 23
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 8
        Text = '0'
        OnChange = EditChange
        Style = esPosNumber
        Modified = False
        SelLength = 0
        SelStart = 0
      end
      object VarTimeStepBox: TCheckBox
        Left = 16
        Top = 150
        Width = 230
        Height = 22
        Caption = 'Use Variable Time Steps Adjusted By:'
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 4
        OnClick = VarTimeStepBoxClick
      end
      object HeadTolEdit: TNumEdit
        Left = 281
        Top = 281
        Width = 81
        Height = 22
        AutoSize = False
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 9
        Text = '0'
        OnChange = EditChange
        Style = esPosNumber
        Modified = False
        SelLength = 0
        SelStart = 0
      end
      inline VarStepEdit: TUpDnEditBox
        Left = 281
        Top = 149
        Width = 58
        Height = 23
        AutoSize = True
        TabOrder = 5
        ExplicitLeft = 281
        ExplicitTop = 149
        ExplicitWidth = 58
        ExplicitHeight = 23
        inherited Spinner: TUpDown
          Left = 42
          Height = 23
          Min = 25
          Max = 400
          Increment = 5
          Position = 75
          TabOrder = 1
          ExplicitLeft = 42
          ExplicitHeight = 23
        end
        inherited EditBox: TEdit
          Width = 42
          Height = 23
          TabOrder = 0
          Text = '75'
          OnChange = EditChange
          ExplicitWidth = 42
          ExplicitHeight = 23
        end
      end
      inline MaxTrialsEdit: TUpDnEditBox
        Left = 281
        Top = 313
        Width = 58
        Height = 23
        AutoSize = True
        TabOrder = 10
        ExplicitLeft = 281
        ExplicitTop = 313
        ExplicitWidth = 58
        ExplicitHeight = 23
        inherited Spinner: TUpDown
          Left = 42
          Height = 23
          Min = 3
          Max = 20
          Position = 4
          TabOrder = 1
          ExplicitLeft = 42
          ExplicitHeight = 23
        end
        inherited EditBox: TEdit
          Width = 42
          Height = 23
          TabOrder = 0
          Text = '4'
          OnChange = EditChange
          ExplicitWidth = 42
          ExplicitHeight = 23
        end
      end
      object MinTimeStepEdit: TNumEdit
        Left = 281
        Top = 182
        Width = 81
        Height = 23
        Ctl3D = True
        Enabled = False
        ParentCtl3D = False
        TabOrder = 6
        Text = '0.5'
        OnChange = EditChange
        Style = esPosNumber
        Modified = False
        SelLength = 0
        SelStart = 0
      end
      object DefaultsLabel: TLinkLabel
        Left = 16
        Top = 382
        Width = 81
        Height = 19
        Caption = '<a>Apply Defaults</a>'
        TabOrder = 13
        TabStop = True
        OnLinkClick = DefaultsLabelLinkClick
      end
      object InertialTermsCombo: TComboBox
        Left = 223
        Top = 13
        Width = 139
        Height = 24
        Style = csOwnerDrawFixed
        ItemHeight = 18
        ItemIndex = 1
        TabOrder = 0
        Text = 'Dampen'
        OnChange = EditChange
        Items.Strings = (
          'Keep'
          'Dampen'
          'Ignore')
      end
      object NormalFlowCombo: TComboBox
        Left = 223
        Top = 47
        Width = 139
        Height = 24
        Style = csOwnerDrawFixed
        ItemHeight = 18
        ItemIndex = 2
        TabOrder = 1
        Text = 'Slope & Froude'
        OnChange = EditChange
        Items.Strings = (
          'Slope'
          'Froude No.'
          'Slope & Froude'
          'None')
      end
      object ForceMainCombo: TComboBox
        Left = 223
        Top = 81
        Width = 139
        Height = 24
        Style = csOwnerDrawFixed
        ItemHeight = 18
        ItemIndex = 0
        TabOrder = 2
        Text = 'Hazen-Williams'
        OnChange = EditChange
        Items.Strings = (
          'Hazen-Williams'
          'Darcy-Weisbach')
      end
      object SurchargeCombo: TComboBox
        Left = 223
        Top = 115
        Width = 139
        Height = 24
        Style = csOwnerDrawFixed
        ItemHeight = 18
        ItemIndex = 0
        TabOrder = 3
        Text = 'Extran'
        OnChange = EditChange
        Items.Strings = (
          'Extran'
          'Slot')
      end
      object ThreadsEdit: TUpDown
        Left = 321
        Top = 346
        Width = 16
        Height = 23
        Associate = EditBox
        Min = 1
        Position = 1
        TabOrder = 12
      end
      object EditBox: TEdit
        Left = 280
        Top = 346
        Width = 41
        Height = 23
        NumbersOnly = True
        ReadOnly = True
        TabOrder = 11
        Text = '1'
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Files'
      ImageIndex = 2
      object Label25: TLabel
        Left = 8
        Top = 32
        Width = 189
        Height = 15
        Caption = 'Specify interface files to use or save:'
      end
      object FileListBox: TListBox
        Left = 8
        Top = 56
        Width = 375
        Height = 177
        Style = lbOwnerDrawFixed
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        ItemHeight = 18
        ParentFont = False
        TabOrder = 0
        OnDblClick = FileListBoxDblClick
        OnKeyPress = FileListBoxKeyPress
      end
      object AddBtn: TButton
        Left = 74
        Top = 248
        Width = 75
        Height = 25
        Caption = 'Add'
        TabOrder = 1
        OnClick = AddBtnClick
      end
      object EditBtn: TButton
        Left = 158
        Top = 248
        Width = 75
        Height = 25
        Caption = 'Edit'
        TabOrder = 2
        OnClick = EditBtnClick
      end
      object DeleteBtn: TButton
        Left = 242
        Top = 248
        Width = 75
        Height = 25
        Caption = 'Delete'
        TabOrder = 3
        OnClick = DeleteBtnClick
      end
    end
  end
  object OKBtn: TButton
    Left = 80
    Top = 464
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 1
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 174
    Top = 464
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object HelpBtn: TButton
    Left = 268
    Top = 464
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 3
    OnClick = HelpBtnClick
  end
end
