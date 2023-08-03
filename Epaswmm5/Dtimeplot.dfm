object TimePlotForm: TTimePlotForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'Time Series Plot Selection'
  ClientHeight = 340
  ClientWidth = 297
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 15
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 297
    Height = 340
    ActivePage = TabSheet2
    Align = alClient
    Style = tsButtons
    TabHeight = 1
    TabOrder = 0
    TabStop = False
    TabWidth = 1
    object TabSheet1: TTabSheet
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 289
        Height = 329
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object OkBtn: TButton
          Left = 23
          Top = 297
          Width = 75
          Height = 25
          Caption = 'OK'
          TabOrder = 2
          OnClick = OkBtnClick
        end
        object CancelBtn1: TButton
          Left = 107
          Top = 297
          Width = 75
          Height = 25
          Cancel = True
          Caption = 'Cancel'
          TabOrder = 3
          OnClick = CancelBtn1Click
        end
        object HelpBtn1: TButton
          Left = 192
          Top = 297
          Width = 75
          Height = 25
          Caption = 'Help'
          TabOrder = 4
          OnClick = HelpBtn1Click
        end
        object GroupBox1: TGroupBox
          Left = 14
          Top = 128
          Width = 259
          Height = 153
          Caption = 'Data Series'
          TabOrder = 1
          object BtnAdd: TBitBtn
            Left = 8
            Top = 24
            Width = 57
            Height = 25
            Hint = 'Add a new data series'
            Caption = 'Add'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            OnClick = BtnAddClick
          end
          object SeriesListBox: TListBox
            Left = 8
            Top = 50
            Width = 241
            Height = 95
            ItemHeight = 15
            TabOrder = 5
          end
          object BtnDelete: TBitBtn
            Left = 126
            Top = 24
            Width = 67
            Height = 25
            Hint = 'Delete the selected data series'
            Caption = 'Delete'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 2
            OnClick = BtnDeleteClick
          end
          object BtnMoveUp: TBitBtn
            Left = 195
            Top = 24
            Width = 25
            Height = 25
            Hint = 'Move selected series up'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 3
            OnClick = BtnMoveUpClick
          end
          object BtnMoveDown: TBitBtn
            Left = 224
            Top = 24
            Width = 25
            Height = 25
            Hint = 'Move selected series down'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 4
            OnClick = BtnMoveDownClick
          end
          object BtnEdit: TBitBtn
            Left = 67
            Top = 24
            Width = 57
            Height = 25
            Hint = 'Edit the selected data series'
            Caption = 'Edit'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 1
            OnClick = BtnEditClick
          end
        end
        object GroupBox2: TGroupBox
          Left = 16
          Top = 8
          Width = 257
          Height = 106
          Caption = 'Time Periods'
          TabOrder = 0
          object Label1: TLabel
            Left = 8
            Top = 25
            Width = 51
            Height = 15
            Caption = 'Start Date'
          end
          object Label5: TLabel
            Left = 136
            Top = 25
            Width = 47
            Height = 15
            Caption = 'End Date'
          end
          object StartDateCombo1: TComboBox
            Left = 8
            Top = 43
            Width = 113
            Height = 24
            Style = csOwnerDrawFixed
            ItemHeight = 18
            TabOrder = 0
          end
          object EndDateCombo1: TComboBox
            Left = 136
            Top = 43
            Width = 113
            Height = 24
            Style = csOwnerDrawFixed
            ItemHeight = 18
            TabOrder = 1
          end
          object ElapsedTimeBtn: TRadioButton
            Left = 8
            Top = 81
            Width = 113
            Height = 17
            Caption = 'Elapsed Time'
            TabOrder = 2
            TabStop = True
          end
          object DateTimeBtn: TRadioButton
            Left = 136
            Top = 81
            Width = 113
            Height = 17
            Caption = 'Date/Time'
            TabOrder = 3
            TabStop = True
          end
        end
      end
    end
    object TabSheet2: TTabSheet
      ImageIndex = 1
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 289
        Height = 329
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object Label2: TLabel
          Left = 16
          Top = 83
          Width = 62
          Height = 15
          Caption = 'Object Type'
        end
        object ObjNameLabel: TLabel
          Left = 16
          Top = 123
          Width = 70
          Height = 15
          Caption = 'Object Name'
        end
        object Label4: TLabel
          Left = 16
          Top = 163
          Width = 41
          Height = 15
          Caption = 'Variable'
        end
        object Label6: TLabel
          Left = 16
          Top = 202
          Width = 70
          Height = 15
          Caption = 'Legend Label'
        end
        object Label7: TLabel
          Left = 16
          Top = 238
          Width = 22
          Height = 15
          Caption = 'Axis'
        end
        object Label8: TLabel
          Left = 44
          Top = 16
          Width = 202
          Height = 15
          Caption = 'Specify the object and variable to plot:'
        end
        object Label3: TLabel
          Left = 42
          Top = 37
          Width = 207
          Height = 15
          Caption = '(Click an object on the map to select it)'
        end
        object ObjTypeCombo: TComboBox
          Left = 112
          Top = 80
          Width = 156
          Height = 24
          Style = csOwnerDrawFixed
          ItemHeight = 18
          TabOrder = 0
          OnChange = ObjTypeComboChange
        end
        object ObjNameEdit: TEdit
          Left = 112
          Top = 120
          Width = 155
          Height = 23
          TabOrder = 1
        end
        object VariableCombo: TComboBox
          Left = 112
          Top = 160
          Width = 156
          Height = 24
          Style = csOwnerDrawFixed
          ItemHeight = 18
          TabOrder = 2
        end
        object LegendLabelEdit: TEdit
          Left = 112
          Top = 199
          Width = 156
          Height = 23
          TabOrder = 3
        end
        object AcceptBtn: TButton
          Left = 23
          Top = 297
          Width = 75
          Height = 25
          Caption = 'Accept'
          TabOrder = 4
          OnClick = AcceptBtnClick
        end
        object CancelBtn2: TButton
          Left = 107
          Top = 297
          Width = 75
          Height = 25
          Caption = 'Cancel'
          TabOrder = 5
          OnClick = CancelBtn2Click
        end
        object HelpBtn2: TButton
          Left = 192
          Top = 297
          Width = 75
          Height = 25
          Caption = 'Help'
          TabOrder = 6
          OnClick = HelpBtn2Click
        end
        object LeftAxisBtn: TRadioButton
          Left = 112
          Top = 238
          Width = 49
          Height = 17
          Caption = 'Left'
          TabOrder = 7
        end
        object RightAxisBtn: TRadioButton
          Left = 178
          Top = 238
          Width = 57
          Height = 17
          Caption = 'Right'
          TabOrder = 8
        end
      end
    end
  end
end
