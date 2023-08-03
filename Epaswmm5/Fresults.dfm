object ResultsForm: TResultsForm
  Left = 0
  Top = 0
  Caption = 'Summary Results'
  ClientHeight = 283
  ClientWidth = 629
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsMDIChild
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    000000000000044444444444444404FFFFFFFFFFFFF404F000F000F000F404FF
    FFFFFFFFFFF404F000F000F000F404FFFFFFFFFFFFF404F000F000F000F404FF
    FFFFFFFFFFF404F000F000F000F404FFFFFFFFFFFFF4044444444444444404F4
    44F444F444F4044444444444444400000000000000000000000000000000FFFF
    0000800000008000000080000000800000008000000080000000800000008000
    00008000000080000000800000008000000080000000FFFF0000FFFF0000}
  OldCreateOrder = False
  Position = poDefault
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object TopPanel: TPanel
    Left = 0
    Top = 0
    Width = 629
    Height = 33
    Align = alTop
    AutoSize = True
    BevelOuter = bvNone
    Padding.Left = 4
    Padding.Right = 4
    TabOrder = 0
    object Panel1: TPanel
      Left = 4
      Top = 0
      Width = 239
      Height = 33
      Align = alLeft
      AutoSize = True
      BevelOuter = bvNone
      Padding.Left = 4
      TabOrder = 0
      object Label2: TLabel
        Left = 4
        Top = 9
        Width = 31
        Height = 15
        Caption = 'Topic:'
      end
      object TopicsListBox: TComboBox
        Left = 42
        Top = 6
        Width = 197
        Height = 24
        Style = csOwnerDrawFixed
        Ctl3D = True
        ItemHeight = 18
        ParentCtl3D = False
        TabOrder = 0
        OnSelect = TopicsListBoxClickCheck
      end
    end
    object Panel3: TPanel
      Left = 243
      Top = 0
      Width = 382
      Height = 33
      Align = alClient
      Alignment = taLeftJustify
      BevelOuter = bvNone
      Padding.Left = 12
      Padding.Right = 4
      TabOrder = 1
      object Label1: TLabel
        Left = 12
        Top = 0
        Width = 366
        Height = 33
        Align = alClient
        Caption = 'Label1'
        Layout = tlCenter
        WordWrap = True
        ExplicitWidth = 34
        ExplicitHeight = 15
      end
    end
  end
  object TheGrid: TStringGrid
    Left = 144
    Top = 80
    Width = 320
    Height = 120
    Ctl3D = False
    DrawingStyle = gdsClassic
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goThumbTracking, goFixedColClick, goFixedRowClick]
    ParentCtl3D = False
    TabOrder = 1
    OnDrawCell = TheGridDrawCell
    OnFixedCellClick = TheGridFixedCellClick
  end
  object Edit1: TEdit
    Left = 408
    Top = 224
    Width = 121
    Height = 23
    TabOrder = 2
    Text = 'Edit1'
    Visible = False
  end
end
