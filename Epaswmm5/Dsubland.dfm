object SubLandUsesForm: TSubLandUsesForm
  Left = 511
  Top = 286
  BorderStyle = bsDialog
  Caption = 'SubLandUsesForm'
  ClientHeight = 251
  ClientWidth = 272
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
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object OKBtn: TButton
    Left = 8
    Top = 216
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 0
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 96
    Top = 216
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object HelpBtn: TButton
    Left = 184
    Top = 216
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 2
    OnClick = HelpBtnClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 272
    Height = 201
    Align = alTop
    BevelKind = bkFlat
    BevelOuter = bvNone
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 3
  end
end
