object DefaultsForm: TDefaultsForm
  Left = 295
  Top = 75
  BorderStyle = bsDialog
  Caption = 'Project Defaults'
  ClientHeight = 406
  ClientWidth = 277
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
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object BtnOK: TButton
    Left = 10
    Top = 367
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 1
    OnClick = BtnOKClick
  end
  object BtnCancel: TButton
    Left = 98
    Top = 367
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = BtnCancelClick
  end
  object BtnHelp: TButton
    Left = 186
    Top = 367
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 3
    OnClick = BtnHelpClick
  end
  object CheckDefault: TCheckBox
    Left = 7
    Top = 332
    Width = 250
    Height = 21
    Caption = 'Save as defaults for all new projects'
    TabOrder = 0
  end
  object TabControl1: TTabControl
    Left = 7
    Top = 7
    Width = 257
    Height = 306
    TabOrder = 4
    Tabs.Strings = (
      'ID Labels'
      'Subcatchments'
      'Nodes/Links')
    TabIndex = 0
    OnChange = TabControl1Change
  end
end
