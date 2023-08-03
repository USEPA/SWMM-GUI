object PatternForm: TPatternForm
  Left = 201
  Top = 127
  BorderStyle = bsDialog
  Caption = 'Time Pattern Editor'
  ClientHeight = 338
  ClientWidth = 273
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
    Width = 32
    Height = 15
    Caption = 'Name'
  end
  object Label2: TLabel
    Left = 16
    Top = 72
    Width = 60
    Height = 15
    Caption = 'Description'
  end
  object Label3: TLabel
    Left = 160
    Top = 16
    Width = 24
    Height = 15
    Caption = 'Type'
  end
  object Label4: TLabel
    Left = 16
    Top = 128
    Width = 56
    Height = 15
    Caption = 'Multipliers'
  end
  object NameEdit: TNumEdit
    Left = 17
    Top = 35
    Width = 121
    Height = 21
    BevelKind = bkFlat
    BorderStyle = bsNone
    TabOrder = 0
    OnKeyPress = NameEditKeyPress
    Style = esNoSpace
    Modified = False
    SelLength = 0
    SelStart = 0
  end
  object CommentEdit: TEdit
    Left = 16
    Top = 88
    Width = 217
    Height = 21
    BevelKind = bkFlat
    BorderStyle = bsNone
    TabOrder = 2
  end
  object PatternTypeCombo: TComboBox
    Left = 160
    Top = 35
    Width = 97
    Height = 24
    Style = csOwnerDrawFixed
    ItemHeight = 18
    TabOrder = 1
    OnClick = PatternTypeComboClick
  end
  object OKBtn: TButton
    Left = 182
    Top = 144
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 5
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 182
    Top = 184
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object HelpBtn: TButton
    Left = 182
    Top = 224
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 7
    OnClick = HelpBtnClick
  end
  inline GridEdit1: TGridEditFrame
    Left = 16
    Top = 144
    Width = 185
    Height = 173
    AutoSize = True
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 4
    ExplicitLeft = 16
    ExplicitTop = 144
    ExplicitWidth = 185
    ExplicitHeight = 173
    inherited Grid: TStringGrid
      Left = 1
      Width = 150
      Height = 173
      ColCount = 2
      DefaultColWidth = 60
      RowCount = 24
      FixedRows = 0
      ScrollBars = ssVertical
      ExplicitLeft = 1
      ExplicitWidth = 150
      ExplicitHeight = 173
    end
    inherited EditPanel: TPanel
      Left = 0
      ExplicitLeft = 0
      inherited EditBox: TNumEdit
        Top = 2
        Width = 122
        Height = 21
        BevelInner = bvRaised
        BevelOuter = bvLowered
        ParentCtl3D = False
        Style = esNumber
        ExplicitTop = 2
        ExplicitWidth = 122
        ExplicitHeight = 21
      end
    end
  end
  object EditBtn: TBitBtn
    Tag = 1
    Left = 234
    Top = 88
    Width = 23
    Height = 22
    Hint = 'Edit'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    OnClick = EditBtnClick
  end
end
