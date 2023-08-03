object NotesEditorForm: TNotesEditorForm
  Left = 277
  Top = 242
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'Title/Notes Editor'
  ClientHeight = 193
  ClientWidth = 416
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 416
    Height = 140
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
    OnChange = CheckHeaderClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 140
    Width = 416
    Height = 53
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      416
      53)
    object CheckHeader: TCheckBox
      Left = 8
      Top = 18
      Width = 217
      Height = 21
      Anchors = [akLeft, akBottom]
      Caption = 'Use title line as header for printing'
      TabOrder = 0
      OnClick = CheckHeaderClick
    end
    object OKBtn: TButton
      Left = 250
      Top = 14
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 1
    end
    object CancelBtn: TButton
      Left = 331
      Top = 14
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 2
    end
  end
end
