object QueryForm: TQueryForm
  Left = 470
  Top = 103
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'Map Query'
  ClientHeight = 189
  ClientWidth = 185
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
    Width = 25
    Height = 15
    Caption = 'With'
  end
  object ComboBox1: TComboBox
    Left = 48
    Top = 8
    Width = 121
    Height = 24
    Style = csOwnerDrawFixed
    ItemHeight = 18
    TabOrder = 0
    OnChange = ComboBox1Change
  end
  object ComboBox2: TComboBox
    Left = 48
    Top = 44
    Width = 121
    Height = 24
    Style = csOwnerDrawVariable
    DropDownCount = 20
    ItemHeight = 18
    TabOrder = 1
    OnChange = ComboBox2Change
    OnDrawItem = ComboBox2DrawItem
    OnMeasureItem = ComboBox2MeasureItem
  end
  object ComboBox3: TComboBox
    Left = 48
    Top = 80
    Width = 121
    Height = 24
    Style = csOwnerDrawFixed
    ItemHeight = 18
    TabOrder = 2
    OnChange = ComboBox2Change
  end
  object Edit1: TEdit
    Left = 48
    Top = 116
    Width = 73
    Height = 23
    TabOrder = 3
    OnChange = ComboBox2Change
    OnKeyPress = Edit1KeyPress
  end
  object Panel1: TPanel
    Left = 48
    Top = 152
    Width = 121
    Height = 23
    BevelOuter = bvNone
    BorderWidth = 1
    BorderStyle = bsSingle
    Color = 16776176
    Ctl3D = False
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 5
  end
  object Button1: TBitBtn
    Left = 128
    Top = 116
    Width = 41
    Height = 25
    Caption = '&Go'
    Default = True
    TabOrder = 4
    OnClick = Button1Click
  end
end
