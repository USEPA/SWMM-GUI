object GWEqnForm: TGWEqnForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Custom Groundwater Flow Equation Editor'
  ClientHeight = 462
  ClientWidth = 527
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Padding.Left = 8
  Padding.Top = 8
  Padding.Right = 8
  Padding.Bottom = 8
  OldCreateOrder = False
  Position = poMainFormCenter
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 483
    Height = 15
    Caption = 
      'Enter an expression to use in addition to the standard equation ' +
      'for lateral groundwater flow '
  end
  object Label2: TLabel
    Left = 8
    Top = 27
    Width = 250
    Height = 15
    Caption = '(leave blank to use only the standard equation):'
  end
  object Memo1: TMemo
    Left = 8
    Top = 143
    Width = 505
    Height = 266
    TabStop = False
    Color = clBtnFace
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 4
  end
  object OkBtn: TButton
    Left = 245
    Top = 426
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelBtn: TButton
    Left = 341
    Top = 426
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object Edit1: TMemo
    Left = 8
    Top = 48
    Width = 505
    Height = 81
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'Edit1')
    ParentFont = False
    TabOrder = 0
  end
  object HelpBtn: TButton
    Left = 438
    Top = 426
    Width = 75
    Height = 25
    Caption = 'Help'
    TabOrder = 3
    OnClick = HelpBtnClick
  end
end
