object ToolOptionsForm: TToolOptionsForm
  Left = 486
  Top = 225
  BorderStyle = bsDialog
  Caption = 'Tool Options'
  ClientHeight = 239
  ClientWidth = 311
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
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 27
    Height = 15
    Caption = 'Tools'
  end
  object ToolsListBox: TListBox
    Left = 16
    Top = 32
    Width = 185
    Height = 193
    ItemHeight = 15
    TabOrder = 0
  end
  object AddBtn: TButton
    Left = 216
    Top = 32
    Width = 73
    Height = 25
    Caption = 'Add'
    TabOrder = 1
    OnClick = AddBtnClick
  end
  object DeleteBtn: TButton
    Left = 216
    Top = 64
    Width = 73
    Height = 25
    Caption = 'Delete'
    TabOrder = 2
    OnClick = DeleteBtnClick
  end
  object EditBtn: TButton
    Left = 216
    Top = 96
    Width = 73
    Height = 25
    Caption = 'Edit'
    TabOrder = 3
    OnClick = EditBtnClick
  end
  object MoveUpBtn: TBitBtn
    Left = 225
    Top = 127
    Width = 25
    Height = 25
    TabOrder = 4
    OnClick = MoveUpBtnClick
  end
  object MoveDownBtn: TBitBtn
    Left = 256
    Top = 128
    Width = 25
    Height = 25
    TabOrder = 5
    OnClick = MoveDownBtnClick
  end
  object CloseBtn: TButton
    Left = 216
    Top = 168
    Width = 73
    Height = 25
    Caption = 'Close'
    ModalResult = 1
    TabOrder = 6
  end
  object HelpBtn: TButton
    Left = 216
    Top = 200
    Width = 73
    Height = 25
    Caption = '&Help'
    TabOrder = 7
    OnClick = HelpBtnClick
  end
end
