object ColorRampForm: TColorRampForm
  Left = 200
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Color Ramp Selector'
  ClientHeight = 113
  ClientWidth = 202
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object Shape0: TShape
    Left = 16
    Top = 8
    Width = 33
    Height = 17
  end
  object Shape1: TShape
    Left = 48
    Top = 8
    Width = 33
    Height = 17
  end
  object Shape2: TShape
    Left = 80
    Top = 8
    Width = 33
    Height = 17
  end
  object Shape3: TShape
    Left = 112
    Top = 8
    Width = 33
    Height = 17
  end
  object Shape4: TShape
    Left = 144
    Top = 8
    Width = 33
    Height = 17
  end
  object ComboBox1: TComboBox
    Left = 16
    Top = 40
    Width = 161
    Height = 24
    Style = csOwnerDrawFixed
    Ctl3D = True
    DropDownCount = 9
    ItemHeight = 18
    ParentCtl3D = False
    TabOrder = 0
    OnChange = ComboBox1Change
  end
  object Button1: TButton
    Left = 16
    Top = 80
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 104
    Top = 80
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
