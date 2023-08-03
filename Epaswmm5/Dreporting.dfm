object ReportingForm: TReportingForm
  Left = 999
  Top = 201
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'Reporting Options'
  ClientHeight = 387
  ClientWidth = 283
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 249
    Height = 25
    AutoSize = False
    Caption = 'Select objects for detailed reporting:'
    WordWrap = True
  end
  object PageControl1: TPageControl
    Left = 16
    Top = 40
    Width = 161
    Height = 249
    ActivePage = SubcatchTabSheet
    MultiLine = True
    TabOrder = 0
    object SubcatchTabSheet: TTabSheet
      Caption = 'Subcatchments'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object ListBox1: TListBox
        Left = 0
        Top = 0
        Width = 153
        Height = 169
        Align = alTop
        ItemHeight = 15
        TabOrder = 0
      end
      object AllCheckBox1: TCheckBox
        Left = 8
        Top = 176
        Width = 133
        Height = 21
        Caption = 'All Subcatchments'
        TabOrder = 1
        OnClick = AllCheckBox1Click
      end
    end
    object NodesTabSheet: TTabSheet
      Caption = 'Nodes'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object ListBox2: TListBox
        Left = 0
        Top = 0
        Width = 153
        Height = 169
        Align = alTop
        ItemHeight = 15
        TabOrder = 0
      end
      object AllCheckBox2: TCheckBox
        Left = 8
        Top = 176
        Width = 81
        Height = 17
        Caption = 'All Nodes'
        TabOrder = 1
        OnClick = AllCheckBox1Click
      end
    end
    object LinksTabSheet: TTabSheet
      Caption = 'Links'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object ListBox3: TListBox
        Left = 0
        Top = 0
        Width = 153
        Height = 169
        Align = alTop
        ItemHeight = 15
        TabOrder = 0
      end
      object AllCheckBox3: TCheckBox
        Left = 8
        Top = 176
        Width = 73
        Height = 17
        Caption = 'All Links'
        TabOrder = 1
        OnClick = AllCheckBox1Click
      end
    end
  end
  object BtnHelp: TButton
    Left = 192
    Top = 345
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 8
    OnClick = BtnHelpClick
  end
  object BtnAdd: TButton
    Left = 192
    Top = 86
    Width = 75
    Height = 25
    Caption = 'Add'
    TabOrder = 1
    OnClick = BtnAddClick
  end
  object BtnRemove: TButton
    Left = 192
    Top = 118
    Width = 75
    Height = 25
    Caption = 'Remove'
    TabOrder = 2
    OnClick = BtnRemoveClick
  end
  object BtnClear: TButton
    Left = 192
    Top = 150
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 3
    OnClick = BtnClearClick
  end
  object BtnClose: TButton
    Left = 192
    Top = 313
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 7
    OnClick = BtnCloseClick
  end
  object ReportInputBox: TCheckBox
    Left = 16
    Top = 300
    Width = 157
    Height = 21
    Caption = 'Report Input Summary'
    TabOrder = 4
    OnClick = AllCheckBox1Click
  end
  object ReportControlsBox: TCheckBox
    Left = 16
    Top = 327
    Width = 157
    Height = 21
    Caption = 'Report Control Actions'
    TabOrder = 5
    OnClick = AllCheckBox1Click
  end
  object ReportAveragesBox: TCheckBox
    Left = 16
    Top = 354
    Width = 157
    Height = 21
    Caption = 'Report Average Results'
    TabOrder = 6
    OnClick = AllCheckBox1Click
  end
end
