object ProfileSelectForm: TProfileSelectForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'Profile Plot Selection'
  ClientHeight = 307
  ClientWidth = 329
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 15
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 145
    Height = 169
    Caption = 'Create Profile'
    TabOrder = 0
    object Label7: TLabel
      Left = 8
      Top = 24
      Width = 56
      Height = 15
      Caption = 'Start Node'
    end
    object Label8: TLabel
      Left = 8
      Top = 72
      Width = 52
      Height = 15
      Caption = 'End Node'
    end
    object StartNodeEdit: TEdit
      Left = 8
      Top = 40
      Width = 105
      Height = 23
      TabOrder = 0
      OnChange = StartEndNodeEditChange
    end
    object StartNodeBtn: TBitBtn
      Left = 114
      Top = 41
      Width = 23
      Height = 22
      TabOrder = 1
      OnClick = StartEndNodeBtnClick
    end
    object EndNodeEdit: TEdit
      Left = 8
      Top = 88
      Width = 105
      Height = 23
      TabOrder = 2
      OnChange = StartEndNodeEditChange
    end
    object EndNodeBtn: TBitBtn
      Left = 114
      Top = 89
      Width = 23
      Height = 22
      TabOrder = 3
      OnClick = StartEndNodeBtnClick
    end
    object FindPathBtn: TButton
      Left = 24
      Top = 128
      Width = 89
      Height = 25
      Caption = '&Find Path'
      TabOrder = 4
      OnClick = FindPathBtnClick
    end
  end
  object UseProfileBtn: TButton
    Left = 16
    Top = 184
    Width = 133
    Height = 25
    Caption = 'Use Saved Profile'
    TabOrder = 2
    OnClick = UseProfileBtnClick
  end
  object SaveProfileBtn: TButton
    Left = 16
    Top = 216
    Width = 133
    Height = 25
    Caption = 'Save Current Profile'
    TabOrder = 3
    OnClick = SaveProfileBtnClick
  end
  object ProfileLinksBox: TGroupBox
    Left = 168
    Top = 8
    Width = 145
    Height = 233
    Caption = 'Links in Profile'
    TabOrder = 1
    object BtnAddLink: TBitBtn
      Left = 9
      Top = 199
      Width = 23
      Height = 22
      Hint = 'Insert link'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = BtnAddLinkClick
    end
    object BtnDelLink: TBitBtn
      Left = 35
      Top = 199
      Width = 23
      Height = 22
      Hint = 'Remove link'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = BtnDelLinkClick
    end
    object BtnLinkUp: TBitBtn
      Left = 61
      Top = 199
      Width = 23
      Height = 22
      Hint = 'Move link up'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = BtnLinkUpClick
    end
    object BtnLinkDown: TBitBtn
      Left = 87
      Top = 199
      Width = 23
      Height = 22
      Hint = 'Move link down'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      OnClick = BtnLinkDownClick
    end
    object LinksListBox: TListBox
      Left = 8
      Top = 24
      Width = 129
      Height = 173
      ItemHeight = 15
      TabOrder = 0
    end
    object BtnClearLinks: TBitBtn
      Left = 113
      Top = 199
      Width = 23
      Height = 22
      Hint = 'Clear all links'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      OnClick = BtnClearLinksClick
    end
  end
  object BtnOK: TButton
    Left = 37
    Top = 265
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 4
    OnClick = BtnOKClick
  end
  object BtnCancel: TButton
    Left = 127
    Top = 265
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 5
    OnClick = BtnCancelClick
  end
  object BtnHelp: TButton
    Left = 217
    Top = 265
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 6
    OnClick = BtnHelpClick
  end
end
