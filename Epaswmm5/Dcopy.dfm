object CopyToForm: TCopyToForm
  Left = 200
  Top = 108
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Copy'
  ClientHeight = 183
  ClientWidth = 289
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
  object DestGroup: TRadioGroup
    Left = 16
    Top = 16
    Width = 105
    Height = 97
    Caption = 'Copy To'
    ItemIndex = 0
    Items.Strings = (
      'Clipboard'
      'File')
    TabOrder = 0
  end
  object FormatGroup: TRadioGroup
    Left = 152
    Top = 16
    Width = 113
    Height = 97
    Caption = 'Copy As'
    ItemIndex = 0
    Items.Strings = (
      'Bitmap'
      'Metafile'
      'Data (Text)')
    TabOrder = 1
  end
  object BtnOK: TButton
    Left = 16
    Top = 136
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = BtnOKClick
  end
  object BtnCancel: TButton
    Left = 104
    Top = 136
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = BtnCancelClick
  end
  object BtnHelp: TButton
    Left = 192
    Top = 136
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 4
    OnClick = BtnHelpClick
  end
end
