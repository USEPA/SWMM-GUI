object InfilForm: TInfilForm
  Left = 198
  Top = 110
  BorderStyle = bsDialog
  Caption = 'Infiltration Editor'
  ClientHeight = 305
  ClientWidth = 328
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 41
    Width = 328
    Height = 156
    Align = alTop
    BevelOuter = bvNone
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 1
    inline GridEdit: TGridEditFrame
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 322
      Height = 161
      Align = alTop
      TabOrder = 0
      ExplicitLeft = 3
      ExplicitTop = 3
      ExplicitWidth = 322
      ExplicitHeight = 161
      inherited Grid: TStringGrid
        Width = 322
        Height = 161
        Align = alClient
        ColCount = 2
        DefaultColWidth = 156
        RowCount = 6
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goThumbTracking]
        ScrollBars = ssNone
        OnSelectCell = GridEditGridSelectCell
        ExplicitWidth = 322
        ExplicitHeight = 161
        RowHeights = (
          24
          24
          24
          24
          24
          24)
      end
      inherited EditPanel: TPanel
        inherited EditBox: TNumEdit
          Style = esPosNumber
        end
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 328
    Height = 41
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvNone
    Caption = '  Infiltration Method'
    Padding.Left = 4
    Padding.Top = 4
    Padding.Right = 4
    TabOrder = 0
    object ComboBox1: TComboBox
      Left = 136
      Top = 10
      Width = 177
      Height = 24
      Style = csOwnerDrawFixed
      ItemHeight = 18
      TabOrder = 0
      OnChange = ComboBox1Change
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 249
    Width = 328
    Height = 56
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object OKBtn: TButton
      Left = 38
      Top = 16
      Width = 75
      Height = 25
      Caption = 'OK'
      TabOrder = 0
      OnClick = OKBtnClick
    end
    object CancleBtn: TButton
      Left = 126
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object HelpBtn: TButton
      Left = 214
      Top = 16
      Width = 75
      Height = 25
      Caption = '&Help'
      TabOrder = 2
      OnClick = HelpBtnClick
    end
  end
  object Panel4: TPanel
    Left = 0
    Top = 197
    Width = 328
    Height = 52
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 1
    Ctl3D = False
    Padding.Left = 2
    Padding.Top = 2
    Padding.Right = 2
    Padding.Bottom = 2
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 3
    object HintLabel: TLabel
      Left = 3
      Top = 3
      Width = 322
      Height = 46
      Align = alClient
      Caption = 'HintLabel'
      Transparent = True
      WordWrap = True
      ExplicitWidth = 51
      ExplicitHeight = 15
    end
  end
end
