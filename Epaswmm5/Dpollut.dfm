object PollutantForm: TPollutantForm
  Left = 507
  Top = 115
  BorderStyle = bsDialog
  Caption = 'Pollutant Editor'
  ClientHeight = 411
  ClientWidth = 265
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 265
    Height = 290
    Align = alTop
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 0
  end
  object Panel2: TPanel
    Left = 0
    Top = 352
    Width = 265
    Height = 59
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object OKBtn: TButton
      Left = 9
      Top = 16
      Width = 75
      Height = 25
      Caption = 'OK'
      TabOrder = 0
      OnClick = OKBtnClick
    end
    object CancelBtn: TButton
      Left = 95
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object HelpBtn: TButton
      Left = 181
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
    Top = 290
    Width = 265
    Height = 62
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
      Width = 51
      Height = 15
      Align = alClient
      Caption = 'HintLabel'
      Transparent = True
      WordWrap = True
    end
  end
end
