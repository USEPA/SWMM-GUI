object MapDimensionsForm: TMapDimensionsForm
  Left = 237
  Top = 176
  BorderStyle = bsDialog
  Caption = 'Map Dimensions'
  ClientHeight = 264
  ClientWidth = 410
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poMainFormCenter
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 15
  object GroupBox1: TGroupBox
    Left = 8
    Top = 12
    Width = 190
    Height = 93
    Caption = 'Lower Left'
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 0
    object Label1: TLabel
      Left = 15
      Top = 26
      Width = 72
      Height = 15
      Caption = 'X-coordinate:'
    end
    object Label2: TLabel
      Left = 15
      Top = 58
      Width = 72
      Height = 15
      Caption = 'Y-coordinate:'
    end
    object LLYEdit: TNumEdit
      Left = 96
      Top = 56
      Width = 73
      Height = 23
      Ctl3D = True
      ParentCtl3D = False
      TabOrder = 1
      Style = esNumber
      Modified = False
      SelLength = 0
      SelStart = 0
    end
    object LLXEdit: TNumEdit
      Left = 96
      Top = 24
      Width = 73
      Height = 23
      Ctl3D = True
      ParentCtl3D = False
      TabOrder = 0
      Style = esNumber
      Modified = False
      SelLength = 0
      SelStart = 0
    end
  end
  object GroupBox2: TGroupBox
    Left = 206
    Top = 12
    Width = 190
    Height = 93
    Caption = 'Upper Right'
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 1
    object Label3: TLabel
      Left = 15
      Top = 26
      Width = 72
      Height = 15
      Caption = 'X-coordinate:'
    end
    object Label4: TLabel
      Left = 15
      Top = 58
      Width = 72
      Height = 15
      Caption = 'Y-coordinate:'
    end
    object URYEdit: TNumEdit
      Left = 96
      Top = 56
      Width = 73
      Height = 23
      Ctl3D = True
      ParentCtl3D = False
      TabOrder = 1
      Style = esNumber
      Modified = False
      SelLength = 0
      SelStart = 0
    end
    object URXEdit: TNumEdit
      Left = 96
      Top = 24
      Width = 73
      Height = 23
      Ctl3D = True
      ParentCtl3D = False
      TabOrder = 0
      Style = esNumber
      Modified = False
      SelLength = 0
      SelStart = 0
    end
  end
  object BtnOK: TButton
    Left = 140
    Top = 224
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 5
    OnClick = BtnOKClick
  end
  object BtnCancel: TButton
    Left = 230
    Top = 224
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object BtnHelp: TButton
    Left = 320
    Top = 224
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 7
    OnClick = BtnHelpClick
  end
  object MapUnits: TRadioGroup
    Left = 8
    Top = 116
    Width = 387
    Height = 61
    Caption = 'Map Units'
    Columns = 4
    ItemIndex = 3
    Items.Strings = (
      'Feet'
      'Meters'
      'Degrees'
      'None')
    TabOrder = 2
  end
  object BtnAuto: TButton
    Left = 8
    Top = 224
    Width = 87
    Height = 25
    Hint = 'Sizes the map to fill the display window'
    Caption = '&Auto-Size'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    OnClick = BtnAutoClick
  end
  object AutoLengths: TCheckBox
    Left = 23
    Top = 192
    Width = 345
    Height = 21
    Caption = 'Auto-Length is ON. Re-compute all lengths and areas?'
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 3
  end
end
