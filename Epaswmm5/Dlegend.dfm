object LegendForm: TLegendForm
  Left = 218
  Top = 116
  BorderStyle = bsDialog
  Caption = 'Legend Editor'
  ClientHeight = 199
  ClientWidth = 338
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poDefault
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 15
  object Box0: TShape
    Left = 4
    Top = 6
    Width = 11
    Height = 29
    OnMouseDown = BoxMouseDown
  end
  object Box1: TShape
    Tag = 1
    Left = 4
    Top = 34
    Width = 11
    Height = 30
    OnMouseDown = BoxMouseDown
  end
  object Box2: TShape
    Tag = 2
    Left = 4
    Top = 63
    Width = 11
    Height = 30
    OnMouseDown = BoxMouseDown
  end
  object Box3: TShape
    Tag = 3
    Left = 4
    Top = 92
    Width = 11
    Height = 31
    OnMouseDown = BoxMouseDown
  end
  object Box4: TShape
    Tag = 4
    Left = 4
    Top = 122
    Width = 11
    Height = 29
    OnMouseDown = BoxMouseDown
  end
  object Hiliter: TShape
    Left = 92
    Top = 145
    Width = 15
    Height = 13
    Brush.Style = bsClear
    Pen.Style = psDot
    Shape = stRoundRect
    Visible = False
  end
  object NameLabel: TLabel
    Left = 22
    Top = 7
    Width = 60
    Height = 15
    Caption = 'NameLabel'
  end
  object UnitsLabel: TLabel
    Left = 22
    Top = 141
    Width = 55
    Height = 15
    Caption = 'UnitsLabel'
  end
  object Bevel1: TBevel
    Left = 232
    Top = 8
    Width = 2
    Height = 185
    Shape = bsLeftLine
  end
  object Edit1: TEdit
    Left = 22
    Top = 25
    Width = 71
    Height = 23
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 0
  end
  object Edit2: TEdit
    Left = 22
    Top = 54
    Width = 71
    Height = 23
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 1
  end
  object Edit3: TEdit
    Left = 22
    Top = 85
    Width = 71
    Height = 23
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 2
  end
  object Edit4: TEdit
    Left = 22
    Top = 115
    Width = 71
    Height = 23
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 3
  end
  object BtnAutoScale: TButton
    Left = 112
    Top = 25
    Width = 105
    Height = 23
    Caption = '&Auto-Scale'
    TabOrder = 4
    OnClick = BtnAutoScaleClick
  end
  object BtnColorRamp: TButton
    Left = 112
    Top = 57
    Width = 105
    Height = 23
    Caption = '&Color Ramp ...'
    TabOrder = 5
    OnClick = BtnColorRampClick
  end
  object BtnReverse: TButton
    Left = 112
    Top = 89
    Width = 105
    Height = 23
    Caption = '&Reverse Colors'
    TabOrder = 6
    OnClick = BtnReverseClick
  end
  object Panel1: TPanel
    Left = 7
    Top = 163
    Width = 210
    Height = 24
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Caption = 'Click on color you wish to change'
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 11
  end
  object BtnOK: TButton
    Left = 250
    Top = 9
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 8
    OnClick = BtnOKClick
  end
  object BtnCancel: TButton
    Left = 251
    Top = 49
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 9
    OnClick = BtnCancelClick
  end
  object BtnHelp: TButton
    Left = 251
    Top = 89
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 10
    OnClick = BtnHelpClick
  end
  object CheckFramed: TCheckBox
    Left = 112
    Top = 120
    Width = 81
    Height = 17
    Caption = 'Framed'
    TabOrder = 7
    OnClick = CheckFramedClick
  end
  object ColorDialog1: TColorDialog
    Options = [cdFullOpen, cdSolidColor]
    Left = 264
    Top = 128
  end
end
