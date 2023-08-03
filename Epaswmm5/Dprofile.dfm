object ProfileSelectionForm: TProfileSelectionForm
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Profile Selection'
  ClientHeight = 175
  ClientWidth = 286
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 73
    Height = 15
    Caption = 'Saved Profiles'
  end
  object ProfilesListBox: TListBox
    Left = 16
    Top = 24
    Width = 153
    Height = 97
    ItemHeight = 15
    TabOrder = 0
  end
  object RenameBtn: TButton
    Left = 192
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Rename'
    TabOrder = 1
    OnClick = RenameBtnClick
  end
  object RemoveBtn: TButton
    Left = 192
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Remove'
    TabOrder = 2
    OnClick = RemoveBtnClick
  end
  object OKBtn: TButton
    Left = 59
    Top = 136
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 3
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 147
    Top = 136
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
end
