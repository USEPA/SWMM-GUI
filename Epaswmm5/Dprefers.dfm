object PreferencesForm: TPreferencesForm
  Left = 260
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Preferences'
  ClientHeight = 398
  ClientWidth = 291
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
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
  object PageControl1: TPageControl
    Left = 12
    Top = 8
    Width = 266
    Height = 321
    ActivePage = TabSheet1
    TabOrder = 0
    object TabSheet1: TTabSheet
      HelpContext = 137
      Caption = 'General Options'
      object Label5: TLabel
        Left = 24
        Top = 229
        Width = 67
        Height = 15
        Caption = 'Style Theme:'
      end
      object CheckBox1: TCheckBox
        Left = 24
        Top = 17
        Width = 195
        Height = 21
        Caption = 'Blinking Map Highlighter'
        TabOrder = 0
      end
      object CheckBox2: TCheckBox
        Left = 24
        Top = 40
        Width = 195
        Height = 21
        Caption = 'Blinking Map Highlighter'
        TabOrder = 1
      end
      object CheckBox3: TCheckBox
        Left = 24
        Top = 63
        Width = 195
        Height = 21
        Caption = 'Blinking Map Highlighter'
        TabOrder = 2
      end
      object CheckBox4: TCheckBox
        Left = 24
        Top = 86
        Width = 195
        Height = 21
        Caption = 'Blinking Map Highlighter'
        TabOrder = 3
      end
      object CheckBox5: TCheckBox
        Left = 24
        Top = 109
        Width = 195
        Height = 21
        Caption = 'Blinking Map Highlighter'
        TabOrder = 4
      end
      object CheckBox6: TCheckBox
        Left = 24
        Top = 132
        Width = 195
        Height = 21
        Caption = 'Blinking Map Highlighter'
        TabOrder = 5
      end
      object CheckBox7: TCheckBox
        Left = 24
        Top = 155
        Width = 195
        Height = 21
        Caption = 'Blinking Map Highlighter'
        TabOrder = 6
      end
      object CheckBox8: TCheckBox
        Left = 24
        Top = 178
        Width = 195
        Height = 21
        Caption = 'Blinking Map Highlighter'
        TabOrder = 7
      end
      object StylesCombo: TComboBox
        Left = 24
        Top = 250
        Width = 169
        Height = 24
        Style = csOwnerDrawFixed
        ItemHeight = 18
        TabOrder = 8
      end
      object CheckBox9: TCheckBox
        Left = 24
        Top = 202
        Width = 195
        Height = 21
        Caption = 'Blinking Map Highlighter'
        TabOrder = 9
      end
    end
    object TabSheet2: TTabSheet
      HelpContext = 142
      Caption = 'Numerical Precision'
      object Label1: TLabel
        Left = 30
        Top = 150
        Width = 86
        Height = 15
        Caption = 'Node Parameter'
      end
      object Label2: TLabel
        Left = 173
        Top = 150
        Width = 48
        Height = 15
        Caption = 'Decimals'
      end
      object Label3: TLabel
        Left = 30
        Top = 211
        Width = 79
        Height = 15
        Caption = 'Link Parameter'
      end
      object Label4: TLabel
        Left = 173
        Top = 211
        Width = 48
        Height = 15
        Caption = 'Decimals'
      end
      object Label7: TLabel
        Left = 30
        Top = 91
        Width = 106
        Height = 15
        Caption = 'Subcatch Parameter'
      end
      object Label8: TLabel
        Left = 173
        Top = 91
        Width = 48
        Height = 15
        Caption = 'Decimals'
      end
      object Label6: TLabel
        Left = 30
        Top = 34
        Width = 195
        Height = 49
        AutoSize = False
        Caption = 'Label6'
        WordWrap = True
      end
      object NodeVarBox: TComboBox
        Left = 30
        Top = 167
        Width = 107
        Height = 24
        Style = csOwnerDrawFixed
        Ctl3D = True
        ItemHeight = 18
        ParentCtl3D = False
        TabOrder = 2
        OnChange = NodeVarBoxChange
      end
      object LinkVarBox: TComboBox
        Left = 30
        Top = 229
        Width = 107
        Height = 24
        Style = csOwnerDrawFixed
        Ctl3D = True
        ItemHeight = 18
        ParentCtl3D = False
        TabOrder = 4
        OnChange = LinkVarBoxChange
      end
      object SubcatchVarBox: TComboBox
        Left = 30
        Top = 110
        Width = 107
        Height = 24
        Style = csOwnerDrawFixed
        Ctl3D = True
        ItemHeight = 18
        ParentCtl3D = False
        TabOrder = 0
        OnChange = SubcatchVarBoxChange
      end
      inline SubcatchVarSpin: TUpDnEditBox
        Left = 173
        Top = 110
        Width = 55
        Height = 23
        AutoSize = True
        TabOrder = 1
        ExplicitLeft = 173
        ExplicitTop = 110
        ExplicitWidth = 55
        ExplicitHeight = 23
        inherited Spinner: TUpDown
          Left = 39
          Height = 23
          Max = 6
          Position = 2
          TabOrder = 1
          ExplicitLeft = 39
          ExplicitHeight = 23
        end
        inherited EditBox: TEdit
          Width = 39
          Height = 23
          ParentCtl3D = False
          TabOrder = 0
          Text = '2'
          ExplicitWidth = 39
          ExplicitHeight = 23
        end
      end
      inline NodeVarSpin: TUpDnEditBox
        Left = 173
        Top = 167
        Width = 55
        Height = 23
        AutoSize = True
        TabOrder = 3
        ExplicitLeft = 173
        ExplicitTop = 167
        ExplicitWidth = 55
        ExplicitHeight = 23
        inherited Spinner: TUpDown
          Left = 39
          Height = 23
          Max = 6
          Position = 2
          TabOrder = 1
          ExplicitLeft = 39
          ExplicitHeight = 23
        end
        inherited EditBox: TEdit
          Width = 39
          Height = 23
          ParentCtl3D = False
          TabOrder = 0
          Text = '2'
          ExplicitWidth = 39
          ExplicitHeight = 23
        end
      end
      inline LinkVarSpin: TUpDnEditBox
        Left = 173
        Top = 229
        Width = 55
        Height = 23
        AutoSize = True
        TabOrder = 5
        ExplicitLeft = 173
        ExplicitTop = 229
        ExplicitWidth = 55
        ExplicitHeight = 23
        inherited Spinner: TUpDown
          Left = 39
          Height = 23
          Max = 6
          Position = 2
          TabOrder = 1
          ExplicitLeft = 39
          ExplicitHeight = 23
        end
        inherited EditBox: TEdit
          Width = 39
          Height = 23
          ParentCtl3D = False
          TabOrder = 0
          Text = '2'
          ExplicitWidth = 39
          ExplicitHeight = 23
        end
      end
    end
  end
  object BtnOK: TButton
    Left = 14
    Top = 352
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = BtnOKClick
  end
  object BtnCancel: TButton
    Left = 108
    Top = 352
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = BtnCancelClick
  end
  object BtnHelp: TButton
    Left = 202
    Top = 352
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 3
    OnClick = BtnHelpClick
  end
end
