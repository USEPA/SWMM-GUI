object TransectForm: TTransectForm
  Left = 211
  Top = 109
  BorderStyle = bsDialog
  Caption = 'Transect Editor'
  ClientHeight = 425
  ClientWidth = 456
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
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
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 78
    Height = 15
    Caption = 'Transect Name'
  end
  object Label3: TLabel
    Left = 184
    Top = 7
    Width = 60
    Height = 15
    Caption = 'Description'
  end
  object NameEdit: TEdit
    Left = 16
    Top = 23
    Width = 145
    Height = 23
    TabOrder = 0
    OnChange = NameEditChange
    OnKeyPress = NameEditKeyPress
  end
  object CommentEdit: TEdit
    Left = 184
    Top = 23
    Width = 233
    Height = 23
    TabOrder = 1
    OnChange = NameEditChange
  end
  object BtnOK: TButton
    Left = 184
    Top = 384
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 6
    OnClick = BtnOKClick
  end
  object BtnCancel: TButton
    Left = 271
    Top = 384
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 7
  end
  object BtnHelp: TButton
    Left = 358
    Top = 384
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 8
    OnClick = BtnHelpClick
  end
  object BtnView: TButton
    Left = 16
    Top = 384
    Width = 75
    Height = 25
    Caption = '&View...'
    TabOrder = 5
    OnClick = BtnViewClick
  end
  object Panel1: TPanel
    Left = 240
    Top = 64
    Width = 193
    Height = 288
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 4
  end
  inline GridEdit: TGridEditFrame
    Left = 16
    Top = 64
    Width = 201
    Height = 302
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 3
    ExplicitLeft = 16
    ExplicitTop = 64
    ExplicitWidth = 201
    ExplicitHeight = 302
    inherited Grid: TStringGrid
      Width = 198
      Height = 288
      ColCount = 3
      RowCount = 101
      ExplicitWidth = 198
      ExplicitHeight = 288
    end
    inherited EditPanel: TPanel
      inherited EditBox: TNumEdit
        Style = esNumber
      end
    end
  end
  object EditBtn: TBitBtn
    Tag = 1
    Left = 418
    Top = 23
    Width = 21
    Height = 21
    Hint = 'Edit'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnClick = EditBtnClick
  end
end
