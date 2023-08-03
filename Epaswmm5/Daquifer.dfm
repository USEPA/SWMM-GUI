object AquiferForm: TAquiferForm
  Left = 332
  Top = 110
  BorderStyle = bsDialog
  Caption = 'Aquifer Editor'
  ClientHeight = 468
  ClientWidth = 285
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
  object Splitter1: TSplitter
    Left = 0
    Top = 351
    Width = 285
    Height = 4
    Cursor = crVSplit
    Align = alTop
    Color = clBtnShadow
    ParentColor = False
    ExplicitWidth = 265
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 285
    Height = 351
    Align = alTop
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 0
  end
  object Panel2: TPanel
    Left = 0
    Top = 411
    Width = 285
    Height = 57
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object OKBtn: TButton
      Left = 19
      Top = 16
      Width = 75
      Height = 25
      Caption = 'OK'
      TabOrder = 0
      OnClick = OKBtnClick
    end
    object CancelBtn: TButton
      Left = 105
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object HelpBtn: TButton
      Left = 191
      Top = 16
      Width = 75
      Height = 25
      Caption = '&Help'
      TabOrder = 2
      OnClick = HelpBtnClick
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 355
    Width = 285
    Height = 56
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 1
    Ctl3D = False
    Padding.Left = 2
    Padding.Top = 2
    Padding.Right = 2
    Padding.Bottom = 2
    ParentBackground = False
    ParentColor = True
    ParentCtl3D = False
    TabOrder = 2
    object HintLabel: TLabel
      Left = 3
      Top = 3
      Width = 279
      Height = 50
      Align = alClient
      Caption = 'HintLabel'
      Color = clBtnFace
      ParentColor = False
      Transparent = True
      WordWrap = True
      ExplicitWidth = 51
      ExplicitHeight = 15
    end
  end
end
