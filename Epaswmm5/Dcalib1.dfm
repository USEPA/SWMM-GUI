object CalibDataForm: TCalibDataForm
  Left = 210
  Top = 199
  BorderStyle = bsDialog
  Caption = 'Calibration Data'
  ClientHeight = 406
  ClientWidth = 617
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 15
  object BtnOK: TButton
    Left = 345
    Top = 363
    Width = 74
    Height = 26
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 4
    OnClick = BtnOKClick
  end
  object BtnCancel: TButton
    Left = 435
    Top = 363
    Width = 74
    Height = 26
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object BtnHelp: TButton
    Left = 524
    Top = 363
    Width = 75
    Height = 26
    Caption = '&Help'
    TabOrder = 6
    OnClick = BtnHelpClick
  end
  object StringGrid1: TStringGrid
    Left = 16
    Top = 16
    Width = 583
    Height = 328
    ColCount = 2
    Ctl3D = False
    DefaultColWidth = 180
    DrawingStyle = gdsClassic
    RowCount = 13
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected]
    ParentCtl3D = False
    ScrollBars = ssNone
    TabOrder = 0
  end
  object BtnBrowse: TBitBtn
    Left = 16
    Top = 363
    Width = 75
    Height = 25
    Caption = 'Add'
    TabOrder = 1
    OnClick = BtnBrowseClick
  end
  object BtnEdit: TBitBtn
    Left = 106
    Top = 363
    Width = 75
    Height = 25
    Caption = 'Edit'
    TabOrder = 2
    OnClick = BtnEditClick
  end
  object BtnDelete: TBitBtn
    Left = 196
    Top = 363
    Width = 75
    Height = 25
    Caption = 'Delete'
    TabOrder = 3
    OnClick = BtnDeleteClick
  end
end
