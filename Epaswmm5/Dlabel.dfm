object LabelForm: TLabelForm
  Left = 200
  Top = 99
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 24
  ClientWidth = 115
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poDefault
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Edit1: TEdit
    Left = 1
    Top = 2
    Width = 107
    Height = 20
    AutoSize = False
    Ctl3D = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 0
    OnKeyPress = Edit1KeyPress
  end
end
