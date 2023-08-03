object CurveDataForm: TCurveDataForm
  Left = 412
  Top = 135
  BorderStyle = bsDialog
  Caption = 'Curve Editor'
  ClientHeight = 374
  ClientWidth = 337
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
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
    Width = 66
    Height = 15
    Caption = 'Curve Name'
  end
  object Label3: TLabel
    Left = 16
    Top = 55
    Width = 60
    Height = 15
    Caption = 'Description'
  end
  object CurveTypeLabel: TLabel
    Left = 200
    Top = 8
    Width = 59
    Height = 15
    Caption = 'Pump Type'
  end
  object CurveName: TEdit
    Left = 16
    Top = 24
    Width = 149
    Height = 23
    TabOrder = 0
    OnChange = CurveNameChange
    OnKeyPress = CurveNameKeyPress
  end
  object Comment: TEdit
    Left = 16
    Top = 71
    Width = 273
    Height = 23
    TabOrder = 2
    OnChange = CurveNameChange
  end
  object BtnOK: TButton
    Left = 240
    Top = 250
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 8
    OnClick = BtnOKClick
  end
  object BtnCancel: TButton
    Left = 240
    Top = 290
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 9
  end
  object BtnHelp: TButton
    Left = 240
    Top = 330
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 10
    OnClick = BtnHelpClick
  end
  object BtnLoad: TButton
    Left = 240
    Top = 144
    Width = 75
    Height = 25
    Caption = '&Load...'
    TabOrder = 6
    OnClick = BtnLoadClick
  end
  object BtnSave: TButton
    Left = 240
    Top = 184
    Width = 75
    Height = 25
    Caption = '&Save...'
    TabOrder = 7
    OnClick = BtnSaveClick
  end
  object BtnView: TButton
    Left = 240
    Top = 104
    Width = 75
    Height = 25
    Caption = '&View...'
    TabOrder = 5
    OnClick = BtnViewClick
  end
  object CurveTypeCombo: TComboBox
    Left = 200
    Top = 24
    Width = 113
    Height = 24
    Style = csOwnerDrawFixed
    ItemHeight = 18
    TabOrder = 1
    OnChange = CurveNameChange
    OnClick = CurveTypeComboClick
  end
  inline GridEdit: TGridEditFrame
    Left = 16
    Top = 104
    Width = 201
    Height = 253
    TabOrder = 4
    ExplicitLeft = 16
    ExplicitTop = 104
    ExplicitWidth = 201
    ExplicitHeight = 253
    inherited Grid: TStringGrid
      Width = 201
      Height = 253
      ColCount = 3
      RowCount = 101
      ExplicitWidth = 201
      ExplicitHeight = 253
    end
  end
  object EditBtn: TBitBtn
    Left = 292
    Top = 72
    Width = 23
    Height = 22
    TabOrder = 3
    OnClick = EditBtnClick
  end
end
