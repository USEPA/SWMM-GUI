object UnitHydForm: TUnitHydForm
  Left = 236
  Top = 136
  BorderStyle = bsDialog
  Caption = 'Unit Hydrograph Editor'
  ClientHeight = 451
  ClientWidth = 344
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
    Top = 20
    Width = 102
    Height = 15
    Caption = 'Name of UH Group'
  end
  object Label2: TLabel
    Left = 16
    Top = 60
    Width = 82
    Height = 15
    Caption = 'Rain Gage Used'
  end
  object Bevel1: TBevel
    Left = 16
    Top = 96
    Width = 305
    Height = 4
    Shape = bsTopLine
  end
  object Label3: TLabel
    Left = 16
    Top = 118
    Width = 92
    Height = 15
    Caption = 'Hydrographs For:'
  end
  object Label4: TLabel
    Left = 16
    Top = 376
    Width = 240
    Height = 15
    Caption = 'Months with UH data have a (*) next to them.'
  end
  object UHName: TEdit
    Left = 152
    Top = 16
    Width = 169
    Height = 23
    MaxLength = 15
    TabOrder = 0
    OnChange = UHDataChange
    OnKeyPress = UHNameKeyPress
  end
  object RGname: TComboBox
    Left = 152
    Top = 56
    Width = 169
    Height = 23
    TabOrder = 1
    OnChange = UHDataChange
  end
  object OKBtn: TButton
    Left = 44
    Top = 408
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 4
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 132
    Top = 408
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object HelpBtn: TButton
    Left = 220
    Top = 408
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 6
    OnClick = HelpBtnClick
  end
  object PageControl1: TPageControl
    Left = 16
    Top = 152
    Width = 305
    Height = 209
    ActivePage = TabSheet1
    TabOrder = 3
    object TabSheet1: TTabSheet
      Caption = 'Unit Hydrographs'
      inline UHGridEdit: TGridEditFrame
        Left = 0
        Top = 0
        Width = 297
        Height = 105
        Align = alTop
        AutoSize = True
        TabOrder = 0
        ExplicitWidth = 297
        ExplicitHeight = 105
        inherited Grid: TStringGrid
          Width = 297
          Height = 105
          ColCount = 4
          DefaultColWidth = 62
          RowCount = 4
          ScrollBars = ssNone
          ExplicitWidth = 297
          ExplicitHeight = 105
        end
        inherited EditPanel: TPanel
          Left = 40
          Top = 40
          ExplicitLeft = 40
          ExplicitTop = 40
          inherited EditBox: TNumEdit
            Top = 16
            ExplicitTop = 16
          end
        end
      end
      object Panel1: TPanel
        Left = 0
        Top = 105
        Width = 297
        Height = 74
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object Label5: TLabel
          Left = 8
          Top = 6
          Width = 209
          Height = 15
          Caption = 'R = fraction of rainfall that becomes I&&I'
        end
        object Label6: TLabel
          Left = 8
          Top = 27
          Width = 192
          Height = 15
          Caption = 'T = time to hydrograph peak (hours)'
        end
        object Label7: TLabel
          Left = 8
          Top = 48
          Width = 244
          Height = 15
          Caption = 'K = falling limb duration / rising limb duration'
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Initial Abstraction Depth'
      ImageIndex = 1
      inline IAGridEdit: TGridEditFrame
        Left = 0
        Top = 0
        Width = 297
        Height = 105
        Align = alTop
        AutoSize = True
        TabOrder = 0
        ExplicitWidth = 297
        ExplicitHeight = 105
        inherited Grid: TStringGrid
          Width = 297
          Height = 105
          ColCount = 4
          DefaultColWidth = 62
          RowCount = 4
          ScrollBars = ssNone
          ExplicitWidth = 297
          ExplicitHeight = 105
        end
        inherited EditPanel: TPanel
          Left = 48
          Top = 24
          ExplicitLeft = 48
          ExplicitTop = 24
          inherited EditBox: TNumEdit
            Top = 16
            Style = esNumber
            ExplicitTop = 16
          end
        end
      end
      object Panel2: TPanel
        Left = 0
        Top = 105
        Width = 297
        Height = 74
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object IALabel1: TLabel
          Left = 8
          Top = 6
          Width = 137
          Height = 15
          Caption = 'Dmax = maximum depth '
        end
        object IALabel2: TLabel
          Left = 8
          Top = 25
          Width = 109
          Height = 15
          Caption = 'Drec = recovery rate '
        end
        object IALabel3: TLabel
          Left = 8
          Top = 46
          Width = 106
          Height = 15
          Caption = 'Do = starting depth '
        end
      end
    end
  end
  object MonthsCombo: TComboBox
    Left = 152
    Top = 114
    Width = 169
    Height = 24
    Style = csOwnerDrawFixed
    ItemHeight = 18
    TabOrder = 2
    OnClick = MonthsComboClick
  end
end
