object ProjectSummaryForm: TProjectSummaryForm
  Left = 192
  Top = 114
  BorderStyle = bsDialog
  Caption = 'Project Summary'
  ClientHeight = 346
  ClientWidth = 336
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Padding.Left = 6
  Padding.Top = 6
  Padding.Right = 6
  Padding.Bottom = 6
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 15
  object StringGrid1: TStringGrid
    Left = 6
    Top = 6
    Width = 324
    Height = 334
    Align = alClient
    ColCount = 2
    Ctl3D = False
    DefaultColWidth = 180
    DrawingStyle = gdsClassic
    RowCount = 30
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine]
    ParentCtl3D = False
    TabOrder = 0
  end
end
