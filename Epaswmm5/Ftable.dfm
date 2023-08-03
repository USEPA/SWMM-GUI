object TableForm: TTableForm
  Left = 264
  Top = 149
  Caption = 'TableForm'
  ClientHeight = 165
  ClientWidth = 286
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clBlack
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
  OldCreateOrder = True
  Position = poDefault
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object Grid1: TDrawGrid
    Left = 0
    Top = 0
    Width = 286
    Height = 165
    Align = alClient
    Ctl3D = False
    DefaultColWidth = 72
    DrawingStyle = gdsClassic
    FixedCols = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goThumbTracking, goFixedColClick, goFixedRowClick]
    ParentCtl3D = False
    TabOrder = 0
    OnDrawCell = Grid1DrawCell
    OnFixedCellClick = Grid1FixedCellClick
    RowHeights = (
      24
      24
      24
      24
      24)
  end
  object Edit1: TEdit
    Left = 104
    Top = 80
    Width = 121
    Height = 23
    ReadOnly = True
    TabOrder = 1
    Text = 'Edit1'
    Visible = False
  end
end
