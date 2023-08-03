object BackdropFileForm: TBackdropFileForm
  Left = 194
  Top = 254
  BorderStyle = bsDialog
  Caption = 'Backdrop Image Selector'
  ClientHeight = 209
  ClientWidth = 312
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
    Width = 107
    Height = 15
    Caption = 'Backdrop Image File'
  end
  object Label2: TLabel
    Left = 16
    Top = 72
    Width = 175
    Height = 15
    Caption = 'World Coordinates File (optional)'
  end
  object ImageFileEdit: TEdit
    Left = 16
    Top = 32
    Width = 224
    Height = 23
    TabStop = False
    ReadOnly = True
    TabOrder = 0
  end
  object ImageFileBtn: TBitBtn
    Left = 241
    Top = 30
    Width = 24
    Height = 24
    Hint = 'Select File'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    OnClick = ImageFileBtnClick
  end
  object WorldFileEdit: TEdit
    Left = 16
    Top = 87
    Width = 224
    Height = 23
    TabStop = False
    ReadOnly = True
    TabOrder = 2
    OnChange = WorldFileEditChange
  end
  object WorldFileBtn: TBitBtn
    Left = 241
    Top = 85
    Width = 24
    Height = 25
    Hint = 'Select File'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    OnClick = WorldFileBtnClick
  end
  object OKBtn: TButton
    Left = 16
    Top = 168
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 5
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 104
    Top = 168
    Width = 74
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object HelpBtn: TButton
    Left = 191
    Top = 168
    Width = 75
    Height = 25
    Caption = 'Help'
    TabOrder = 7
    OnClick = HelpBtnClick
  end
  object ScaleMapCheckBox: TCheckBox
    Left = 16
    Top = 128
    Width = 224
    Height = 21
    Caption = 'Scale Map to Backdrop Image'
    Ctl3D = True
    Enabled = False
    ParentCtl3D = False
    TabOrder = 4
  end
  object WorldFileDelBtn: TBitBtn
    Left = 266
    Top = 85
    Width = 24
    Height = 25
    Hint = 'Clear File Selection'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 8
    OnClick = WorldFileDelBtnClick
  end
end
