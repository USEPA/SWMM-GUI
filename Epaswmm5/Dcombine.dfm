object FileCombineForm: TFileCombineForm
  Left = 237
  Top = 198
  BorderStyle = bsDialog
  Caption = 'Interface File Combine Utility'
  ClientHeight = 257
  ClientWidth = 514
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
  object SpeedButton1: TSpeedButton
    Left = 16
    Top = 216
    Width = 81
    Height = 25
    Caption = '&Browse'
    OnClick = SpeedButton1Click
  end
  object Label1: TLabel
    Left = 32
    Top = 16
    Width = 441
    Height = 40
    Alignment = taCenter
    AutoSize = False
    Caption = 
      'This utility will combine the contents of Routing Interface File' +
      '1 with those of Routing Interface File 2 to produce Routing Inte' +
      'rface File 3.'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 16
    Top = 184
    Width = 183
    Height = 15
    Caption = 'Combining files, please stand by ...'
    Visible = False
  end
  object BtnCombine: TButton
    Left = 240
    Top = 216
    Width = 75
    Height = 25
    Caption = 'Combine'
    TabOrder = 1
    OnClick = BtnCombineClick
  end
  object BtnCancel: TButton
    Left = 328
    Top = 216
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object BtnHelp: TButton
    Left = 416
    Top = 216
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 3
    OnClick = BtnHelpClick
  end
  object StringGrid1: TStringGrid
    Left = 16
    Top = 64
    Width = 473
    Height = 105
    ColCount = 2
    Ctl3D = False
    DefaultColWidth = 120
    DrawingStyle = gdsClassic
    RowCount = 4
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
    ParentCtl3D = False
    ScrollBars = ssNone
    TabOrder = 0
    OnDblClick = SpeedButton1Click
    OnKeyDown = StringGrid1KeyDown
    OnSelectCell = StringGrid1SelectCell
  end
  object SaveFileDlg: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly]
    Left = 176
    Top = 200
  end
  object OpenFileDlg: TOpenTxtFileDialog
    Options = [ofHideReadOnly]
    ShowPreview = True
    WordWrap = False
    Left = 120
    Top = 200
  end
end
