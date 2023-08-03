object IfaceFileForm: TIfaceFileForm
  Left = 571
  Top = 128
  BorderStyle = bsDialog
  Caption = 'Interface File Selector'
  ClientHeight = 187
  ClientWidth = 369
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 16
    Top = 20
    Width = 48
    Height = 15
    Caption = 'File Type:'
  end
  object Label3: TLabel
    Left = 16
    Top = 80
    Width = 56
    Height = 15
    Caption = 'File Name:'
  end
  object FileTypeCombo: TComboBox
    Left = 16
    Top = 40
    Width = 157
    Height = 24
    Style = csOwnerDrawFixed
    ItemHeight = 18
    TabOrder = 0
    OnChange = FileTypeComboChange
  end
  object UseBtn: TRadioButton
    Left = 285
    Top = 42
    Width = 75
    Height = 17
    Caption = 'Use File'
    TabOrder = 2
  end
  object SaveBtn: TRadioButton
    Left = 193
    Top = 42
    Width = 75
    Height = 17
    Caption = 'Save File'
    TabOrder = 1
  end
  object FileNameEdit: TEdit
    Left = 16
    Top = 96
    Width = 305
    Height = 23
    TabStop = False
    ParentShowHint = False
    ReadOnly = True
    ShowHint = True
    TabOrder = 3
    OnChange = FileNameEditChange
  end
  object OKBtn: TButton
    Left = 53
    Top = 144
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 5
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 144
    Top = 144
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object HelpBtn: TButton
    Left = 235
    Top = 144
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 7
    OnClick = HelpBtnClick
  end
  object BrowseBtn: TBitBtn
    Left = 324
    Top = 97
    Width = 23
    Height = 22
    Hint = 'Select File'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    OnClick = BrowseBtnClick
  end
  object OpenDialog: TOpenDialog
    Left = 248
  end
  object SaveDialog: TSaveDialog
    Left = 184
  end
end
