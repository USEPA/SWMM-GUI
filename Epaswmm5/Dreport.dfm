object ReportSelectForm: TReportSelectForm
  Left = 529
  Top = 316
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'Report Selection'
  ClientHeight = 304
  ClientWidth = 329
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = True
  Position = poMainFormCenter
  ShowHint = True
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 15
  object BtnOK: TButton
    Left = 37
    Top = 265
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 1
    OnClick = BtnOKClick
  end
  object BtnCancel: TButton
    Left = 127
    Top = 265
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = BtnCancelClick
  end
  object BtnHelp: TButton
    Left = 217
    Top = 265
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 3
    OnClick = BtnHelpClick
  end
  object Notebook1: TNotebook
    Left = 0
    Top = 0
    Width = 329
    Height = 249
    Align = alTop
    TabOrder = 0
    object TPage
      Left = 0
      Top = 0
      Caption = 'TimeSeriesPage'
      object Label4: TLabel
        Left = 16
        Top = 8
        Width = 51
        Height = 15
        Caption = 'Start Date'
      end
      object Label5: TLabel
        Left = 160
        Top = 8
        Width = 47
        Height = 15
        Caption = 'End Date'
      end
      object Label1: TLabel
        Left = 160
        Top = 64
        Width = 86
        Height = 15
        Caption = 'Object Category'
      end
      object Label3: TLabel
        Left = 16
        Top = 118
        Width = 46
        Height = 15
        Caption = 'Variables'
      end
      object Label15: TLabel
        Left = 16
        Top = 64
        Width = 67
        Height = 15
        Caption = 'Time Format'
      end
      object StartDateCombo1: TComboBox
        Left = 16
        Top = 27
        Width = 121
        Height = 24
        Style = csOwnerDrawFixed
        ItemHeight = 18
        TabOrder = 0
      end
      object EndDateCombo1: TComboBox
        Left = 160
        Top = 27
        Width = 121
        Height = 24
        Style = csOwnerDrawFixed
        ItemHeight = 18
        TabOrder = 1
      end
      object CategoryCombo: TComboBox
        Left = 160
        Top = 84
        Width = 121
        Height = 24
        Style = csOwnerDrawFixed
        ItemHeight = 18
        TabOrder = 3
        OnClick = CategoryComboClick
      end
      object VariableListBox: TCheckListBox
        Left = 16
        Top = 136
        Width = 121
        Height = 94
        IntegralHeight = True
        ItemHeight = 18
        Style = lbOwnerDrawFixed
        TabOrder = 4
        OnClick = VariableListBoxClick
      end
      object TimeAxisCombo: TComboBox
        Left = 16
        Top = 84
        Width = 121
        Height = 24
        Style = csOwnerDrawFixed
        ItemHeight = 18
        ItemIndex = 0
        TabOrder = 2
        Text = 'Elapsed Time'
        Items.Strings = (
          'Elapsed Time'
          'Date/Time')
      end
      object ObjectsPanel: TPanel
        Left = 160
        Top = 120
        Width = 151
        Height = 121
        BevelOuter = bvNone
        TabOrder = 5
        object ObjectsLabel: TLabel
          Left = 0
          Top = -2
          Width = 40
          Height = 15
          Caption = 'Objects'
        end
        object ItemsListBox: TListBox
          Left = 0
          Top = 16
          Width = 121
          Height = 94
          Style = lbOwnerDrawFixed
          IntegralHeight = True
          ItemHeight = 18
          TabOrder = 0
        end
        object BtnAdd: TBitBtn
          Left = 123
          Top = 15
          Width = 23
          Height = 22
          Hint = 'Add Item'
          TabOrder = 1
          OnClick = BtnAddClick
        end
        object BtnDelete: TBitBtn
          Left = 123
          Top = 40
          Width = 23
          Height = 22
          Hint = 'Remove Item'
          TabOrder = 2
          OnClick = BtnDeleteClick
        end
        object BtnMoveUp: TBitBtn
          Left = 123
          Top = 64
          Width = 23
          Height = 22
          Hint = 'Move Item Up'
          TabOrder = 3
          OnClick = BtnMoveUpClick
        end
        object BtnMoveDown: TBitBtn
          Left = 123
          Top = 89
          Width = 23
          Height = 22
          Hint = 'Move Item Down'
          TabOrder = 4
          OnClick = BtnMoveDownClick
        end
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'ScatterPage'
      object Label2: TLabel
        Left = 16
        Top = 16
        Width = 51
        Height = 15
        Caption = 'Start Date'
      end
      object Label6: TLabel
        Left = 168
        Top = 16
        Width = 47
        Height = 15
        Caption = 'End Date'
      end
      object StartDateCombo2: TComboBox
        Left = 16
        Top = 32
        Width = 121
        Height = 22
        Style = csOwnerDrawFixed
        TabOrder = 0
      end
      object EndDateCombo2: TComboBox
        Left = 168
        Top = 32
        Width = 121
        Height = 22
        Style = csOwnerDrawFixed
        TabOrder = 1
      end
      object XVariableBox: TGroupBox
        Left = 16
        Top = 72
        Width = 145
        Height = 169
        Caption = 'X-Variable'
        TabOrder = 2
        object Label9: TLabel
          Left = 16
          Top = 24
          Width = 86
          Height = 15
          Caption = 'Object Category'
        end
        object Label10: TLabel
          Left = 16
          Top = 72
          Width = 35
          Height = 15
          Caption = 'Object'
        end
        object Label11: TLabel
          Left = 16
          Top = 120
          Width = 41
          Height = 15
          Caption = 'Variable'
        end
        object XCategoryCombo: TComboBox
          Left = 16
          Top = 40
          Width = 113
          Height = 22
          Style = csOwnerDrawFixed
          TabOrder = 0
          OnClick = CategoryComboClick
        end
        object XObjectEdit: TEdit
          Left = 16
          Top = 88
          Width = 91
          Height = 23
          TabOrder = 1
        end
        object XObjectBtn: TBitBtn
          Left = 109
          Top = 89
          Width = 23
          Height = 22
          TabOrder = 2
          TabStop = False
          OnClick = BtnAddClick
        end
        object XVariableCombo: TComboBox
          Left = 16
          Top = 136
          Width = 113
          Height = 22
          Style = csOwnerDrawFixed
          TabOrder = 3
        end
      end
      object YVariableBox: TGroupBox
        Left = 168
        Top = 72
        Width = 145
        Height = 169
        Caption = 'Y-Variable'
        TabOrder = 3
        object Label12: TLabel
          Left = 16
          Top = 24
          Width = 86
          Height = 15
          Caption = 'Object Category'
        end
        object Label13: TLabel
          Left = 16
          Top = 72
          Width = 35
          Height = 15
          Caption = 'Object'
        end
        object Label14: TLabel
          Left = 16
          Top = 120
          Width = 41
          Height = 15
          Caption = 'Variable'
        end
        object YCategoryCombo: TComboBox
          Left = 16
          Top = 40
          Width = 113
          Height = 22
          Style = csOwnerDrawFixed
          TabOrder = 0
          OnClick = CategoryComboClick
        end
        object YObjectEdit: TEdit
          Left = 16
          Top = 88
          Width = 91
          Height = 23
          TabOrder = 1
        end
        object YObjectBtn: TBitBtn
          Left = 109
          Top = 89
          Width = 23
          Height = 22
          TabOrder = 2
          TabStop = False
          OnClick = BtnAddClick
        end
        object YVariableCombo: TComboBox
          Left = 16
          Top = 136
          Width = 113
          Height = 22
          Style = csOwnerDrawFixed
          TabOrder = 3
        end
      end
    end
  end
end
