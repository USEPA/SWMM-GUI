object EventsForm: TEventsForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Events Editor'
  ClientHeight = 569
  ClientWidth = 430
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 417
    Height = 17
    AutoSize = False
    Caption = 
      'Use this form to restrict hydraulic analysis to particular time ' +
      'periods. '
    WordWrap = True
  end
  object Label2: TLabel
    Left = 16
    Top = 39
    Width = 417
    Height = 17
    AutoSize = False
    Caption = 'Hydraulics will remain constant outside of these periods. '
    WordWrap = True
  end
  object OkBtn: TButton
    Left = 87
    Top = 527
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 2
    OnClick = OkBtnClick
  end
  object CancelBtn: TButton
    Left = 178
    Top = 527
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object HelpBtn: TButton
    Left = 269
    Top = 527
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 4
    OnClick = HelpBtnClick
  end
  object ListView1: TListView
    Left = 16
    Top = 66
    Width = 398
    Height = 281
    Checkboxes = True
    Columns = <
      item
        Caption = 'Use'
        Width = 40
      end
      item
        AutoSize = True
        Caption = 'Start Date'
      end
      item
        AutoSize = True
        Caption = 'Start Time'
      end
      item
        AutoSize = True
        Caption = 'End Date'
      end
      item
        AutoSize = True
        Caption = 'End Time'
      end>
    ColumnClick = False
    Ctl3D = False
    GridLines = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnSelectItem = ListView1SelectItem
  end
  object GroupBox1: TGroupBox
    Left = 16
    Top = 367
    Width = 398
    Height = 145
    Caption = 'Modify the Selected Event'
    TabOrder = 1
    object Label5: TLabel
      Left = 8
      Top = 33
      Width = 51
      Height = 15
      Caption = 'Start Date'
    end
    object Label7: TLabel
      Left = 206
      Top = 33
      Width = 47
      Height = 15
      Caption = 'End Date'
    end
    object Label6: TLabel
      Left = 8
      Top = 69
      Width = 53
      Height = 15
      Caption = 'Start Time'
    end
    object Label8: TLabel
      Left = 206
      Top = 69
      Width = 49
      Height = 15
      Caption = 'End Time'
    end
    object StartDatePicker: TDateTimePicker
      Left = 87
      Top = 28
      Width = 105
      Height = 25
      Date = 42551.000000000000000000
      Time = 0.418314687500242100
      DateMode = dmUpDown
      TabOrder = 0
    end
    object EndDatePicker: TDateTimePicker
      Left = 276
      Top = 28
      Width = 105
      Height = 25
      Date = 42551.000000000000000000
      Time = 0.418314687500242100
      DateMode = dmUpDown
      TabOrder = 2
    end
    object StartTimePicker: TDateTimePicker
      Left = 87
      Top = 64
      Width = 105
      Height = 25
      Date = 42551.000000000000000000
      Time = 42551.000000000000000000
      Kind = dtkTime
      TabOrder = 1
    end
    object EndTimePicker: TDateTimePicker
      Left = 279
      Top = 64
      Width = 105
      Height = 25
      Date = 42551.000000000000000000
      Time = 42551.000000000000000000
      DateMode = dmUpDown
      Kind = dtkTime
      TabOrder = 3
    end
    object ReplaceBtn: TButton
      Left = 55
      Top = 104
      Width = 89
      Height = 25
      Caption = 'Replace Event'
      TabOrder = 4
      OnClick = ReplaceBtnClick
    end
    object DeleteBtn: TButton
      Left = 154
      Top = 104
      Width = 89
      Height = 25
      Caption = 'Delete Event'
      TabOrder = 5
      OnClick = DeleteBtnClick
    end
    object DeleteAllBtn: TButton
      Left = 254
      Top = 104
      Width = 89
      Height = 25
      Caption = 'Delete All'
      TabOrder = 6
      OnClick = DeleteAllBtnClick
    end
  end
end
