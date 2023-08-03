object GroupDeleteForm: TGroupDeleteForm
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Group Deletion'
  ClientHeight = 320
  ClientWidth = 304
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 281
    Height = 25
    Alignment = taCenter
    AutoSize = False
    Caption = 'Delete which objects in the selected region? '
    WordWrap = True
  end
  object RainGageCheckBox: TCheckBox
    Left = 16
    Top = 48
    Width = 145
    Height = 21
    Caption = 'All Rain Gages'
    TabOrder = 0
  end
  object SubcatchCheckBox: TCheckBox
    Left = 16
    Top = 112
    Width = 153
    Height = 21
    Caption = 'All Subcatchments'
    TabOrder = 3
  end
  object NodeCheckBox: TCheckBox
    Left = 16
    Top = 176
    Width = 257
    Height = 21
    Caption = 'All Nodes (and attached Links)'
    TabOrder = 6
    WordWrap = True
  end
  object LabelCheckBox: TCheckBox
    Left = 16
    Top = 240
    Width = 121
    Height = 21
    Caption = 'All Map Labels'
    TabOrder = 9
  end
  object OkBtn: TButton
    Left = 67
    Top = 280
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 10
    OnClick = OkBtnClick
  end
  object CancelBtn: TButton
    Left = 155
    Top = 280
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 11
  end
  object RainGageTagEdit: TEdit
    Left = 160
    Top = 72
    Width = 121
    Height = 23
    TabOrder = 2
  end
  object SubcatchTagEdit: TEdit
    Left = 160
    Top = 128
    Width = 121
    Height = 23
    TabOrder = 5
  end
  object NodeTagEdit: TEdit
    Left = 160
    Top = 200
    Width = 121
    Height = 23
    TabOrder = 8
  end
  object RainGageTagCheck: TCheckBox
    Left = 16
    Top = 72
    Width = 137
    Height = 21
    Caption = 'with Tag equal to '
    TabOrder = 1
  end
  object SubcatchTagCheck: TCheckBox
    Left = 16
    Top = 136
    Width = 137
    Height = 21
    Caption = 'with Tag equal to '
    TabOrder = 4
  end
  object NodeTagCheck: TCheckBox
    Left = 16
    Top = 200
    Width = 137
    Height = 21
    Caption = 'with Tag equal to '
    TabOrder = 7
  end
end
