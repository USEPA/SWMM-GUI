object InitLoadingsForm: TInitLoadingsForm
  Left = 516
  Top = 174
  BorderStyle = bsDialog
  Caption = 'Initial Buildup Editor'
  ClientHeight = 251
  ClientWidth = 265
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 265
    Height = 201
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object HintPanel: TPanel
      Left = 0
      Top = 160
      Width = 265
      Height = 41
      Align = alBottom
      BevelOuter = bvNone
      BorderWidth = 1
      Ctl3D = False
      Padding.Left = 2
      Padding.Top = 2
      Padding.Right = 2
      Padding.Bottom = 2
      ParentBackground = False
      ParentCtl3D = False
      TabOrder = 0
      object HintLabel: TLabel
        Left = 3
        Top = 3
        Width = 3
        Height = 15
        Align = alClient
        Transparent = True
        WordWrap = True
      end
    end
  end
  object OKBtn: TButton
    Left = 8
    Top = 216
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 1
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 96
    Top = 216
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object HelpBtn: TButton
    Left = 184
    Top = 216
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 3
    OnClick = HelpBtnClick
  end
end
