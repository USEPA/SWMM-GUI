object TimeseriesForm: TTimeseriesForm
  Left = 543
  Top = 172
  BorderStyle = bsDialog
  Caption = 'Time Series Editor'
  ClientHeight = 488
  ClientWidth = 392
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
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 94
    Height = 15
    Caption = 'Time Series Name'
  end
  object Label2: TLabel
    Left = 16
    Top = 55
    Width = 60
    Height = 15
    Caption = 'Description'
  end
  object Label3: TLabel
    Left = 16
    Top = 190
    Width = 293
    Height = 15
    Caption = 'No dates means times are relative to start of simulation.'
  end
  object SeriesName: TEdit
    Left = 16
    Top = 23
    Width = 177
    Height = 23
    TabOrder = 0
    OnChange = SeriesNameChange
    OnKeyPress = SeriesNameKeyPress
  end
  object Comment: TEdit
    Left = 16
    Top = 71
    Width = 331
    Height = 23
    TabOrder = 1
    OnChange = SeriesNameChange
  end
  object BtnOK: TButton
    Left = 296
    Top = 368
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 9
    OnClick = BtnOKClick
  end
  object BtnCancel: TButton
    Left = 296
    Top = 408
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 10
  end
  object BtnHelp: TButton
    Left = 296
    Top = 448
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 11
    OnClick = BtnHelpClick
  end
  object BtnView: TButton
    Left = 296
    Top = 208
    Width = 75
    Height = 25
    Caption = '&View'
    TabOrder = 8
    OnClick = BtnViewClick
  end
  inline GridEdit: TGridEditFrame
    Left = 16
    Top = 208
    Width = 259
    Height = 265
    AutoSize = True
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 7
    ExplicitLeft = 16
    ExplicitTop = 208
    ExplicitWidth = 259
    ExplicitHeight = 265
    inherited Grid: TStringGrid
      Width = 259
      Height = 265
      Align = alClient
      ColCount = 3
      FixedCols = 0
      RowCount = 101
      ExplicitWidth = 259
      ExplicitHeight = 265
    end
  end
  object EditBtn: TBitBtn
    Tag = 1
    Left = 349
    Top = 72
    Width = 23
    Height = 22
    Hint = 'Edit'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnClick = EditBtnClick
  end
  object FileNameEdit: TEdit
    Left = 16
    Top = 127
    Width = 331
    Height = 23
    Enabled = False
    TabOrder = 4
    OnChange = SeriesNameChange
  end
  object FindFileBtn: TBitBtn
    Tag = 1
    Left = 349
    Top = 128
    Width = 23
    Height = 22
    Hint = 'Edit'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 5
    OnClick = FindFileBtnClick
  end
  object UseTableCheckBox: TCheckBox
    Left = 16
    Top = 164
    Width = 329
    Height = 21
    Caption = 'Enter time series data in the table below'
    Checked = True
    State = cbChecked
    TabOrder = 6
    OnClick = UseTableCheckBoxClick
  end
  object UseFileCheckBox: TCheckBox
    Left = 16
    Top = 108
    Width = 329
    Height = 17
    Caption = 'Use external data file named below'
    TabOrder = 3
    OnClick = UseFileCheckBoxClick
  end
end
