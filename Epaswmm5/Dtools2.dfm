object ToolPropertiesForm: TToolPropertiesForm
  Left = 205
  Top = 145
  BorderStyle = bsDialog
  Caption = 'Tool Properties'
  ClientHeight = 387
  ClientWidth = 442
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
  object Label2: TLabel
    Left = 16
    Top = 19
    Width = 57
    Height = 15
    Caption = 'Tool Name'
  end
  object Label3: TLabel
    Left = 16
    Top = 59
    Width = 46
    Height = 15
    Caption = 'Program'
  end
  object Label4: TLabel
    Left = 16
    Top = 92
    Width = 45
    Height = 15
    Caption = 'Working'
  end
  object Label5: TLabel
    Left = 16
    Top = 108
    Width = 48
    Height = 15
    Caption = 'Directory'
  end
  object Label6: TLabel
    Left = 16
    Top = 139
    Width = 59
    Height = 15
    Caption = 'Parameters'
  end
  object Label1: TLabel
    Left = 16
    Top = 160
    Width = 42
    Height = 15
    Caption = 'Macros:'
  end
  object ToolNameEdit: TEdit
    Left = 104
    Top = 16
    Width = 265
    Height = 23
    TabOrder = 0
  end
  object ToolExeEdit: TEdit
    Left = 104
    Top = 56
    Width = 265
    Height = 23
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    OnChange = ToolExeEditChange
  end
  object ToolDirEdit: TEdit
    Left = 104
    Top = 96
    Width = 265
    Height = 23
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    OnChange = ToolExeEditChange
  end
  object ToolParamsEdit: TEdit
    Left = 104
    Top = 136
    Width = 288
    Height = 23
    ParentShowHint = False
    ShowHint = True
    TabOrder = 6
    OnChange = ToolExeEditChange
  end
  object FindProgBtn: TBitBtn
    Left = 372
    Top = 57
    Width = 23
    Height = 22
    Hint = 'Browse'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnClick = FindProgBtnClick
  end
  object FindDirBtn: TBitBtn
    Left = 372
    Top = 97
    Width = 23
    Height = 22
    Hint = 'Browse'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    OnClick = FindDirBtnClick
  end
  object OKBtn: TButton
    Left = 174
    Top = 344
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 11
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 262
    Top = 344
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 12
  end
  object HelpBtn: TButton
    Left = 350
    Top = 344
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 13
    OnClick = HelpBtnClick
  end
  object DisableSWMMCheckBox: TCheckBox
    Left = 104
    Top = 272
    Width = 249
    Height = 21
    Caption = 'Disable SWMM while executing'
    TabOrder = 9
    OnClick = DisableSWMMCheckBoxClick
  end
  object MacrosListBox: TListBox
    Left = 104
    Top = 161
    Width = 312
    Height = 97
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ItemHeight = 14
    Items.Strings = (
      '$PROJDIR        Project directory'
      '$SWMMDIR        SWMM directory'
      '$INPFILE        SWMM input file'
      '$RPTFILE        SWMM report file'
      '$OUTFILE        SWMM output file'
      '$RIFFILE        SWMM runoff interface file')
    ParentFont = False
    TabOrder = 8
    TabWidth = 10
    OnClick = MacrosListBoxClick
  end
  object AddMacroBtn1: TBitBtn
    Left = 397
    Top = 97
    Width = 23
    Height = 22
    Hint = 'Add Selected Macro'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 5
    OnClick = AddMacroBtn1Click
  end
  object AddMacroBtn2: TBitBtn
    Left = 396
    Top = 137
    Width = 23
    Height = 22
    Hint = 'Add Selected Macro'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 7
    OnClick = AddMacroBtn2Click
  end
  object UpdateSWMMCheckBox: TCheckBox
    Left = 104
    Top = 304
    Width = 249
    Height = 21
    Caption = 'Update SWMM after closing'
    Enabled = False
    TabOrder = 10
  end
  object OpenDialog1: TOpenDialog
    Left = 384
    Top = 8
  end
end
