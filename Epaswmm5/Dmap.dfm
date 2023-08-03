object MapOptionsForm: TMapOptionsForm
  Left = 454
  Top = 118
  BorderStyle = bsDialog
  Caption = 'Map Options'
  ClientHeight = 341
  ClientWidth = 329
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 15
  object BtnOK: TButton
    Left = 37
    Top = 294
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object BtnCancel: TButton
    Left = 127
    Top = 294
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object BtnHelp: TButton
    Left = 217
    Top = 294
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 4
    OnClick = BtnHelpClick
  end
  object Panel1: TPanel
    Left = 130
    Top = 8
    Width = 193
    Height = 265
    BevelKind = bkFlat
    BevelOuter = bvNone
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 1
    object Notebook1: TNotebook
      Left = 0
      Top = 0
      Width = 189
      Height = 261
      Align = alClient
      PageIndex = 7
      TabOrder = 0
      object TPage
        Left = 0
        Top = 0
        Caption = 'Subcatchments'
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Label2: TLabel
          Left = 16
          Top = 179
          Width = 58
          Height = 15
          Caption = 'Border Size'
          WordWrap = True
        end
        object Label10: TLabel
          Left = 16
          Top = 139
          Width = 63
          Height = 15
          Caption = 'Symbol Size'
          WordWrap = True
        end
        object SubcatchFill: TRadioGroup
          Left = 16
          Top = 12
          Width = 161
          Height = 109
          Caption = 'Fill Style'
          ItemIndex = 0
          Items.Strings = (
            'Clear'
            'Solid'
            'Diagonal'
            'Cross Hatch')
          TabOrder = 0
        end
        object SubcatchLink: TCheckBox
          Left = 16
          Top = 216
          Width = 161
          Height = 21
          Caption = 'Display link to outlet'
          TabOrder = 3
        end
        inline SubcatchSymbol: TUpDnEditBox
          Left = 120
          Top = 136
          Width = 57
          Height = 23
          AutoSize = True
          TabOrder = 1
          ExplicitLeft = 120
          ExplicitTop = 136
          ExplicitWidth = 57
          ExplicitHeight = 23
          inherited Spinner: TUpDown
            Left = 41
            Height = 23
            Min = 1
            Max = 20
            Position = 10
            TabOrder = 1
            ExplicitLeft = 41
            ExplicitHeight = 23
          end
          inherited EditBox: TEdit
            Width = 41
            Height = 23
            TabOrder = 0
            Text = '10'
            ExplicitWidth = 41
            ExplicitHeight = 23
          end
        end
        inline SubcatchLine: TUpDnEditBox
          Left = 120
          Top = 176
          Width = 57
          Height = 23
          AutoSize = True
          TabOrder = 2
          ExplicitLeft = 120
          ExplicitTop = 176
          ExplicitWidth = 57
          ExplicitHeight = 23
          inherited Spinner: TUpDown
            Left = 41
            Height = 23
            Max = 5
            Position = 1
            TabOrder = 1
            ExplicitLeft = 41
            ExplicitHeight = 23
          end
          inherited EditBox: TEdit
            Width = 41
            Height = 23
            TabOrder = 0
            Text = '1'
            ExplicitWidth = 41
            ExplicitHeight = 23
          end
        end
      end
      object TPage
        Left = 0
        Top = 0
        Caption = 'Nodes'
        ExplicitWidth = 0
        ExplicitHeight = 0
        object NodesBySize: TCheckBox
          Left = 20
          Top = 94
          Width = 170
          Height = 21
          Caption = 'Proportional to Value'
          TabOrder = 1
        end
        object NodeBorder: TCheckBox
          Left = 20
          Top = 134
          Width = 170
          Height = 21
          Caption = 'Display Border'
          TabOrder = 2
          OnClick = NodeBorderClick
        end
        object GroupBox2: TGroupBox
          Left = 16
          Top = 12
          Width = 161
          Height = 61
          Caption = 'Node Size'
          TabOrder = 0
          object NodeShape: TShape
            Left = 88
            Top = 29
            Width = 7
            Height = 6
            Brush.Color = clRed
            Pen.Style = psInsideFrame
            Shape = stCircle
          end
          inline NodeSpin: TUpDnEditBox
            Left = 8
            Top = 24
            Width = 47
            Height = 23
            AutoSize = True
            TabOrder = 0
            ExplicitLeft = 8
            ExplicitTop = 24
            ExplicitWidth = 47
            ExplicitHeight = 23
            inherited Spinner: TUpDown
              Left = 31
              Height = 23
              Min = 1
              Max = 9
              Position = 1
              ExplicitLeft = 31
              ExplicitHeight = 23
            end
            inherited EditBox: TEdit
              Width = 31
              Height = 23
              Text = '1'
              OnChange = NodeSpinChange
              ExplicitWidth = 31
              ExplicitHeight = 23
            end
          end
        end
      end
      object TPage
        Left = 0
        Top = 0
        Caption = 'Links'
        ExplicitWidth = 0
        ExplicitHeight = 0
        object LinksBySize: TCheckBox
          Left = 20
          Top = 94
          Width = 170
          Height = 21
          Caption = 'Proportional to Value'
          TabOrder = 1
        end
        object GroupBox3: TGroupBox
          Left = 16
          Top = 12
          Width = 161
          Height = 61
          Caption = 'Link Size'
          TabOrder = 0
          object LinkShape: TShape
            Left = 88
            Top = 22
            Width = 39
            Height = 2
            Brush.Color = clRed
            Pen.Color = clRed
          end
          inline LinkSpin: TUpDnEditBox
            Left = 8
            Top = 24
            Width = 47
            Height = 23
            AutoSize = True
            TabOrder = 0
            ExplicitLeft = 8
            ExplicitTop = 24
            ExplicitWidth = 47
            ExplicitHeight = 23
            inherited Spinner: TUpDown
              Left = 31
              Height = 23
              Min = 1
              Max = 9
              Position = 1
              ExplicitLeft = 31
              ExplicitHeight = 23
            end
            inherited EditBox: TEdit
              Width = 31
              Height = 23
              Text = '1'
              OnChange = LinkSpinChange
              ExplicitWidth = 31
              ExplicitHeight = 23
            end
          end
        end
        object LinkBorder: TCheckBox
          Left = 20
          Top = 134
          Width = 170
          Height = 21
          Caption = 'Display Border'
          TabOrder = 2
        end
      end
      object TPage
        Left = 0
        Top = 0
        Caption = 'Labels'
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Label4: TLabel
          Left = 16
          Top = 52
          Width = 61
          Height = 15
          Caption = 'At Zoom of'
        end
        object LabelsTransparent: TCheckBox
          Left = 16
          Top = 12
          Width = 170
          Height = 21
          Caption = 'Use Transparent Text'
          TabOrder = 0
        end
        inline ZoomForLabels: TUpDnEditBox
          Tag = 1
          Left = 98
          Top = 49
          Width = 67
          Height = 23
          AutoSize = True
          TabOrder = 1
          ExplicitLeft = 98
          ExplicitTop = 49
          ExplicitWidth = 67
          ExplicitHeight = 23
          inherited Spinner: TUpDown
            Left = 51
            Height = 23
            Min = 100
            Max = 10000
            Increment = 100
            Position = 100
            ExplicitLeft = 51
            ExplicitHeight = 23
          end
          inherited EditBox: TEdit
            Width = 51
            Height = 23
            Text = '100'
            ExplicitWidth = 51
            ExplicitHeight = 23
          end
        end
      end
      object TPage
        Left = 0
        Top = 0
        Caption = 'Notation'
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Label7: TLabel
          Left = 16
          Top = 229
          Width = 61
          Height = 15
          Caption = 'At Zoom of'
        end
        object Label1: TLabel
          Left = 16
          Top = 195
          Width = 47
          Height = 15
          Caption = 'Font Size'
        end
        object Label9: TLabel
          Left = 16
          Top = 12
          Width = 41
          Height = 15
          Caption = 'Display:'
        end
        object NotationTransparent: TCheckBox
          Left = 16
          Top = 165
          Width = 170
          Height = 21
          Caption = 'Use Transparent Text'
          TabOrder = 1
        end
        object NotationListBox: TCheckListBox
          Left = 16
          Top = 30
          Width = 161
          Height = 121
          Flat = False
          ItemHeight = 15
          Items.Strings = (
            'Rain Gage IDs'
            'Subcatch IDs'
            'Node IDs'
            'Link IDs'
            'Subcatch Values'
            'Node Values'
            'Link Values')
          TabOrder = 0
        end
        inline NotationFontSize: TUpDnEditBox
          Left = 98
          Top = 192
          Width = 67
          Height = 23
          AutoSize = True
          TabOrder = 2
          ExplicitLeft = 98
          ExplicitTop = 192
          ExplicitWidth = 67
          ExplicitHeight = 23
          inherited Spinner: TUpDown
            Left = 51
            Height = 23
            Min = 4
            Max = 16
            Position = 7
            TabOrder = 1
            ExplicitLeft = 51
            ExplicitHeight = 23
          end
          inherited EditBox: TEdit
            Width = 51
            Height = 23
            TabOrder = 0
            Text = '7'
            ExplicitWidth = 51
            ExplicitHeight = 23
          end
        end
        inline ZoomForNotation: TUpDnEditBox
          Tag = 4
          Left = 98
          Top = 226
          Width = 67
          Height = 23
          AutoSize = True
          TabOrder = 3
          ExplicitLeft = 98
          ExplicitTop = 226
          ExplicitWidth = 67
          ExplicitHeight = 23
          inherited Spinner: TUpDown
            Left = 51
            Height = 23
            Min = 100
            Max = 10000
            Increment = 100
            Position = 100
            TabOrder = 1
            ExplicitLeft = 51
            ExplicitHeight = 23
          end
          inherited EditBox: TEdit
            Width = 51
            Height = 23
            TabOrder = 0
            Text = '100'
            ExplicitWidth = 51
            ExplicitHeight = 23
          end
        end
      end
      object TPage
        Left = 0
        Top = 0
        Caption = 'Symbols'
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Label3: TLabel
          Left = 16
          Top = 83
          Width = 61
          Height = 15
          Caption = 'At Zoom of'
        end
        object LinkSymbols: TCheckBox
          Left = 16
          Top = 46
          Width = 170
          Height = 21
          Caption = 'Display Link Symbols'
          Ctl3D = True
          ParentCtl3D = False
          TabOrder = 1
        end
        object NodeSymbols: TCheckBox
          Left = 16
          Top = 12
          Width = 170
          Height = 21
          Caption = 'Display Node Symbols'
          TabOrder = 0
        end
        inline ZoomForSymbols: TUpDnEditBox
          Tag = 2
          Left = 98
          Top = 80
          Width = 67
          Height = 23
          AutoSize = True
          TabOrder = 2
          ExplicitLeft = 98
          ExplicitTop = 80
          ExplicitWidth = 67
          ExplicitHeight = 23
          inherited Spinner: TUpDown
            Left = 51
            Height = 23
            Min = 100
            Max = 10000
            Increment = 100
            Position = 100
            ExplicitLeft = 51
            ExplicitHeight = 23
          end
          inherited EditBox: TEdit
            Width = 51
            Height = 23
            Text = '100'
            ExplicitWidth = 51
            ExplicitHeight = 23
          end
        end
      end
      object TPage
        Left = 0
        Top = 0
        Caption = 'Arrows'
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Label6: TLabel
          Left = 16
          Top = 194
          Width = 61
          Height = 15
          Caption = 'At Zoom of'
        end
        object Label8: TLabel
          Left = 16
          Top = 150
          Width = 55
          Height = 15
          Caption = 'Arrow Size'
        end
        object LinkArrows: TRadioGroup
          Left = 16
          Top = 12
          Width = 161
          Height = 120
          Caption = 'Arrow Style'
          Items.Strings = (
            'None'
            'Open'
            'Filled'
            'Fancy')
          TabOrder = 0
        end
        inline ArrowSpin: TUpDnEditBox
          Left = 98
          Top = 147
          Width = 67
          Height = 23
          AutoSize = True
          TabOrder = 1
          ExplicitLeft = 98
          ExplicitTop = 147
          ExplicitWidth = 67
          ExplicitHeight = 23
          inherited Spinner: TUpDown
            Left = 51
            Height = 23
            Min = 5
            Max = 20
            Position = 10
            ExplicitLeft = 51
            ExplicitHeight = 23
          end
          inherited EditBox: TEdit
            Width = 51
            Height = 23
            Text = '10'
            ExplicitWidth = 51
            ExplicitHeight = 23
          end
        end
        inline ZoomForArrows: TUpDnEditBox
          Tag = 3
          Left = 96
          Top = 192
          Width = 67
          Height = 23
          AutoSize = True
          TabOrder = 2
          ExplicitLeft = 96
          ExplicitTop = 192
          ExplicitWidth = 67
          ExplicitHeight = 23
          inherited Spinner: TUpDown
            Left = 51
            Height = 23
            Min = 100
            Max = 10000
            Increment = 100
            Position = 100
            ExplicitLeft = 51
            ExplicitHeight = 23
          end
          inherited EditBox: TEdit
            Width = 51
            Height = 23
            Text = '100'
            ExplicitWidth = 51
            ExplicitHeight = 23
          end
        end
      end
      object TPage
        Left = 0
        Top = 0
        Caption = 'Background'
        object Label5: TLabel
          Left = 46
          Top = 12
          Width = 96
          Height = 15
          Caption = 'Background Color'
        end
        object ColorListBox1: TColorListBox
          Left = 18
          Top = 33
          Width = 153
          Height = 192
          Selected = clScrollBar
          Style = [cbCustomColors]
          ItemHeight = 21
          TabOrder = 0
          OnGetColors = ColorListBox1GetColors
        end
      end
    end
  end
  object ListBox1: TListBox
    Left = 8
    Top = 8
    Width = 121
    Height = 265
    Style = lbOwnerDrawFixed
    ItemHeight = 32
    Items.Strings = (
      '  Subcatchments'
      '  Nodes'
      '  Links'
      '  Labels'
      '  Annotation'
      '  Symbols'
      '  Flow Arrows'
      '  Background')
    TabOrder = 0
    OnClick = ListBox1Click
    OnDrawItem = ListBox1DrawItem
  end
end
