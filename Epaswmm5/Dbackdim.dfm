object BackdropDimensionsForm: TBackdropDimensionsForm
  Left = 295
  Top = 140
  BorderStyle = bsDialog
  Caption = 'Backdrop Dimensions'
  ClientHeight = 377
  ClientWidth = 345
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object GroupBox1: TGroupBox
    Left = 16
    Top = 12
    Width = 313
    Height = 101
    Caption = 'Lower Left'
    TabOrder = 0
    object Label1: TLabel
      Left = 15
      Top = 34
      Width = 72
      Height = 15
      Caption = 'X-coordinate:'
    end
    object Label2: TLabel
      Left = 15
      Top = 66
      Width = 72
      Height = 15
      Caption = 'Y-coordinate:'
    end
    object Label3: TLabel
      Left = 104
      Top = 16
      Width = 50
      Height = 15
      Caption = 'Backdrop'
    end
    object Label4: TLabel
      Left = 208
      Top = 16
      Width = 24
      Height = 15
      Caption = 'Map'
    end
    object LLYEdit: TNumEdit
      Left = 104
      Top = 64
      Width = 89
      Height = 23
      TabOrder = 1
      OnChange = LLXEditChange
      Style = esNumber
      Modified = False
      SelLength = 0
      SelStart = 0
    end
    object LLXEdit: TNumEdit
      Left = 104
      Top = 32
      Width = 89
      Height = 23
      TabOrder = 0
      OnChange = LLXEditChange
      Style = esNumber
      Modified = False
      SelLength = 0
      SelStart = 0
    end
    object MapLLX: TEdit
      Left = 208
      Top = 32
      Width = 89
      Height = 23
      TabStop = False
      Enabled = False
      ReadOnly = True
      TabOrder = 2
    end
    object MapLLY: TEdit
      Left = 208
      Top = 64
      Width = 89
      Height = 23
      TabStop = False
      Enabled = False
      ReadOnly = True
      TabOrder = 3
    end
  end
  object GroupBox2: TGroupBox
    Left = 16
    Top = 124
    Width = 313
    Height = 101
    Caption = 'Upper Right'
    TabOrder = 1
    object Label5: TLabel
      Left = 15
      Top = 34
      Width = 72
      Height = 15
      Caption = 'X-coordinate:'
    end
    object Label6: TLabel
      Left = 15
      Top = 66
      Width = 72
      Height = 15
      Caption = 'Y-coordinate:'
    end
    object Label7: TLabel
      Left = 104
      Top = 16
      Width = 50
      Height = 15
      Caption = 'Backdrop'
    end
    object Label8: TLabel
      Left = 208
      Top = 16
      Width = 24
      Height = 15
      Caption = 'Map'
    end
    object URYEdit: TNumEdit
      Left = 104
      Top = 64
      Width = 89
      Height = 23
      TabOrder = 1
      OnChange = LLXEditChange
      Style = esNumber
      Modified = False
      SelLength = 0
      SelStart = 0
    end
    object URXEdit: TNumEdit
      Left = 104
      Top = 32
      Width = 89
      Height = 23
      TabOrder = 0
      OnChange = LLXEditChange
      Style = esNumber
      Modified = False
      SelLength = 0
      SelStart = 0
    end
    object MapURX: TEdit
      Left = 208
      Top = 32
      Width = 89
      Height = 23
      TabStop = False
      Enabled = False
      ReadOnly = True
      TabOrder = 2
    end
    object MapURY: TEdit
      Left = 208
      Top = 64
      Width = 89
      Height = 23
      TabStop = False
      Enabled = False
      ReadOnly = True
      TabOrder = 3
    end
  end
  object OKBtn: TButton
    Left = 47
    Top = 336
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 5
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 135
    Top = 336
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
    OnClick = CancelBtnClick
  end
  object HelpBtn: TButton
    Left = 223
    Top = 336
    Width = 75
    Height = 25
    Caption = 'Help'
    TabOrder = 7
    OnClick = HelpBtnClick
  end
  object ScaleBackdropBtn: TRadioButton
    Left = 16
    Top = 272
    Width = 209
    Height = 17
    Caption = 'Scale Backdrop Image to Map'
    TabOrder = 3
    OnClick = ScaleBackdropBtnClick
  end
  object ScaleMapBtn: TRadioButton
    Left = 16
    Top = 304
    Width = 209
    Height = 17
    Caption = 'Scale Map to Backdrop Image'
    TabOrder = 4
    OnClick = ScaleMapBtnClick
  end
  object ResizeOnlyBtn: TRadioButton
    Left = 16
    Top = 240
    Width = 209
    Height = 17
    Caption = 'Resize Backdrop Image Only'
    Checked = True
    TabOrder = 2
    TabStop = True
    OnClick = ResizeOnlyBtnClick
  end
end
