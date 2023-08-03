object FindForm: TFindForm
  Left = 234
  Top = 108
  HelpContext = 191
  BorderStyle = bsDialog
  Caption = 'Map Finder'
  ClientHeight = 161
  ClientWidth = 193
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 8
    Top = 14
    Width = 23
    Height = 15
    Caption = 'Find'
  end
  object Label2: TLabel
    Left = 8
    Top = 48
    Width = 39
    Height = 15
    Caption = 'Named'
  end
  object Label3: TLabel
    Left = 64
    Top = 72
    Width = 77
    Height = 15
    Caption = 'Adjacent Links'
  end
  object Button1: TButton
    Left = 7
    Top = 88
    Width = 42
    Height = 25
    Caption = '&Go'
    Default = True
    TabOrder = 2
    OnClick = Button1Click
  end
  object ComboBox1: TComboBox
    Left = 64
    Top = 8
    Width = 113
    Height = 24
    Style = csOwnerDrawFixed
    Ctl3D = True
    ItemHeight = 18
    ItemIndex = 0
    ParentCtl3D = False
    TabOrder = 0
    Text = 'Subcatchment'
    OnChange = ComboBox1Change
    Items.Strings = (
      'Subcatchment'
      'Node'
      'Link')
  end
  object Edit1: TEdit
    Left = 64
    Top = 41
    Width = 113
    Height = 23
    TabOrder = 1
    OnChange = Edit1Change
  end
  object ListBox1: TListBox
    Left = 64
    Top = 89
    Width = 113
    Height = 56
    Ctl3D = True
    ItemHeight = 15
    ParentCtl3D = False
    TabOrder = 3
    OnClick = ListBox1Click
  end
end
