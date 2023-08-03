object SnowpackForm: TSnowpackForm
  Left = 205
  Top = 82
  BorderStyle = bsDialog
  Caption = 'Snow Pack Editor'
  ClientHeight = 470
  ClientWidth = 519
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
    Left = 20
    Top = 24
    Width = 92
    Height = 15
    Caption = 'Snow Pack Name'
  end
  object OKBtn: TButton
    Left = 240
    Top = 427
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 2
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 323
    Top = 427
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object HelpBtn: TButton
    Left = 418
    Top = 427
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 4
    OnClick = HelpBtnClick
  end
  object NameEdit: TNumEdit
    Left = 148
    Top = 16
    Width = 147
    Height = 23
    TabOrder = 0
    OnChange = NameEditChange
    OnKeyPress = NameEditKeyPress
    Style = esNoSpace
    Modified = False
    SelLength = 0
    SelStart = 0
  end
  object PageControl1: TPageControl
    Left = 20
    Top = 57
    Width = 477
    Height = 349
    ActivePage = TabSheet1
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Snow Pack Parameters'
      object Label12: TLabel
        Left = 5
        Top = 247
        Width = 237
        Height = 15
        Caption = 'Fraction of Impervious Area That is Plowable:'
      end
      inline PackGrid: TGridEditFrame
        Left = 4
        Top = 16
        Width = 459
        Height = 210
        AutoSize = True
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 0
        ExplicitLeft = 4
        ExplicitTop = 16
        ExplicitWidth = 459
        ExplicitHeight = 210
        inherited Grid: TStringGrid
          Left = 1
          Width = 458
          Height = 210
          ColCount = 4
          DefaultColWidth = 71
          RowCount = 8
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goThumbTracking]
          ScrollBars = ssNone
          ExplicitLeft = 1
          ExplicitWidth = 458
          ExplicitHeight = 210
        end
        inherited EditPanel: TPanel
          Left = 0
          Top = 0
          ExplicitLeft = 0
          ExplicitTop = 0
          inherited EditBox: TNumEdit
            Style = esNumber
          end
        end
      end
      object FracPlowableEdit: TNumEdit
        Left = 274
        Top = 243
        Width = 53
        Height = 23
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 1
        OnChange = NameEditChange
        Style = esPosNumber
        Modified = False
        SelLength = 0
        SelStart = 0
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Snow Removal Parameters'
      ImageIndex = 1
      object Label5: TLabel
        Left = 56
        Top = 56
        Width = 215
        Height = 15
        Caption = 'Fraction transferred out of the watershed'
      end
      object Label6: TLabel
        Left = 56
        Top = 96
        Width = 224
        Height = 15
        Caption = 'Fraction transferred to the impervious area'
      end
      object Label7: TLabel
        Left = 56
        Top = 136
        Width = 210
        Height = 15
        Caption = 'Fraction transferred to the pervious area'
      end
      object Label8: TLabel
        Left = 56
        Top = 176
        Width = 210
        Height = 15
        Caption = 'Fraction converted into immediate melt'
      end
      object Label9: TLabel
        Left = 56
        Top = 216
        Width = 220
        Height = 15
        Caption = 'Fraction moved to another subcatchment'
      end
      object Label10: TLabel
        Left = 56
        Top = 253
        Width = 118
        Height = 15
        Caption = '(Subcatchment name)'
      end
      object Label11: TLabel
        Left = 56
        Top = 286
        Width = 219
        Height = 15
        Caption = 'Note: sum of all fractions must be <= 1.0.'
      end
      object Label4: TLabel
        Left = 56
        Top = 16
        Width = 216
        Height = 15
        Caption = 'Depth at which snow removal begins (in)'
      end
      object NumEdit1: TNumEdit
        Left = 328
        Top = 14
        Width = 67
        Height = 23
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 0
        OnChange = NameEditChange
        Style = esPosNumber
        Modified = False
        SelLength = 0
        SelStart = 0
      end
      object NumEdit2: TNumEdit
        Left = 328
        Top = 54
        Width = 67
        Height = 23
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 1
        OnChange = NameEditChange
        Style = esPosNumber
        Modified = False
        SelLength = 0
        SelStart = 0
      end
      object NumEdit3: TNumEdit
        Left = 328
        Top = 94
        Width = 67
        Height = 23
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 2
        OnChange = NameEditChange
        Style = esPosNumber
        Modified = False
        SelLength = 0
        SelStart = 0
      end
      object NumEdit4: TNumEdit
        Left = 328
        Top = 134
        Width = 67
        Height = 23
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 3
        OnChange = NameEditChange
        Style = esPosNumber
        Modified = False
        SelLength = 0
        SelStart = 0
      end
      object NumEdit5: TNumEdit
        Left = 328
        Top = 174
        Width = 67
        Height = 23
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 4
        OnChange = NameEditChange
        Style = esPosNumber
        Modified = False
        SelLength = 0
        SelStart = 0
      end
      object SubcatchNameEdit: TEdit
        Left = 218
        Top = 250
        Width = 177
        Height = 23
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 6
        OnChange = NameEditChange
      end
      object NumEdit6: TNumEdit
        Left = 328
        Top = 214
        Width = 67
        Height = 23
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 5
        OnChange = NameEditChange
        Style = esPosNumber
        Modified = False
        SelLength = 0
        SelStart = 0
      end
    end
  end
end
