object MapExportForm: TMapExportForm
  Left = 200
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Map Export'
  ClientHeight = 265
  ClientWidth = 339
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
  object GroupBox1: TGroupBox
    Left = 16
    Top = 8
    Width = 217
    Height = 241
    Caption = 'Export Map To:'
    TabOrder = 0
    object RadioButton1: TRadioButton
      Left = 16
      Top = 24
      Width = 185
      Height = 25
      Caption = 'Text File (.map)'
      TabOrder = 0
      OnClick = RadioButton1Click
    end
    object RadioButton2: TRadioButton
      Left = 16
      Top = 56
      Width = 185
      Height = 25
      Caption = 'Enhanced Metafile (.emf)'
      TabOrder = 1
      OnClick = RadioButton1Click
    end
    object RadioButton3: TRadioButton
      Left = 16
      Top = 88
      Width = 185
      Height = 25
      Caption = 'Drawing Exchange File (.dxf)'
      TabOrder = 2
      OnClick = RadioButton1Click
    end
    object RadioGroup1: TRadioGroup
      Left = 32
      Top = 120
      Width = 145
      Height = 105
      Caption = 'Draw Junctions As:'
      Items.Strings = (
        'Open circles'
        'Filled circles'
        'Filled squares')
      TabOrder = 3
    end
  end
  object Button1: TButton
    Left = 248
    Top = 16
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 248
    Top = 56
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 1
    TabOrder = 2
  end
  object Button3: TButton
    Left = 248
    Top = 96
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 3
    OnClick = Button3Click
  end
  object SaveDialog1: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly]
    Left = 272
    Top = 160
  end
end
