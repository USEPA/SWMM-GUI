object TreatmentForm: TTreatmentForm
  Left = 192
  Top = 107
  BorderStyle = bsSizeToolWin
  Caption = 'Treatment Editor'
  ClientHeight = 319
  ClientWidth = 485
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
  OnKeyDown = FormKeyDown
  DesignSize = (
    485
    319)
  PixelsPerInch = 96
  TextHeight = 15
  object OKBtn: TButton
    Left = 222
    Top = 284
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    TabOrder = 1
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 311
    Top = 284
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object HelpBtn: TButton
    Left = 400
    Top = 284
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 3
    OnClick = HelpBtnClick
  end
  object Panel1: TPanel
    Left = 16
    Top = 16
    Width = 461
    Height = 249
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 0
    OnResize = Panel1Resize
    object Splitter1: TSplitter
      Left = 0
      Top = 131
      Width = 459
      Height = 3
      Cursor = crVSplit
      Align = alTop
      Beveled = True
      ExplicitWidth = 461
    end
    object HintMemo: TMemo
      AlignWithMargins = True
      Left = 3
      Top = 137
      Width = 453
      Height = 107
      Align = alClient
      BorderStyle = bsNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Courier New'
      Font.Style = []
      Lines.Strings = (
        'HintMemo')
      ParentColor = True
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 1
    end
    object Grid: TStringGrid
      Left = 0
      Top = 0
      Width = 459
      Height = 131
      Align = alTop
      BorderStyle = bsNone
      ColCount = 2
      Ctl3D = False
      DefaultColWidth = 128
      DrawingStyle = gdsClassic
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Default'
      Font.Style = []
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
      ParentCtl3D = False
      ParentFont = False
      TabOrder = 0
      OnSetEditText = GridSetEditText
    end
  end
end
