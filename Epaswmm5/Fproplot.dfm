object ProfilePlotForm: TProfilePlotForm
  Left = 191
  Top = 107
  Caption = 'Profile Plot'
  ClientHeight = 339
  ClientWidth = 528
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIChild
  Icon.Data = {
    0000010001001010000000001800680300001600000028000000100000002000
    0000010018000000000000030000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000FFFF000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000FFFF00FFFF00FFFF00000000000000000000000000000000000000000000
    000000000000000000000000FFFF00FFFF00FFFF00FFFF00FFFF000000000000
    00000000000000000000000000000000000000000000000000FFFF00FFFF00FF
    FF00FFFF00FFFF00FFFF00000000000000000000000000000000000000000000
    FFFF00FFFF00000000FFFF00FFFF00FFFF00FFFF000000000000000000000000
    00000000000000000000FFFF00FFFF00FFFF00FFFF00000000FFFF00FFFF0000
    0000000000000000000000000000000000000000FFFF00FFFF00FFFF00FFFF00
    FFFF00FFFF00000000FFFF000000000000000000000000000000000000000000
    00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF0000000000000000000000
    0000000000000000000000000000000000FFFF00FFFF00FFFF00FFFF00FFFF00
    0000000000000000000000000000000000000000000000000000000000000000
    00FFFF00FFFF00FFFF00FFFF0000000000000000000000000000000000000000
    0000000000000000000000000000000000FFFF00FFFF00000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000FFFC
    0000FFF00000FFC00000FF000000FC000000F0000000C0060000001E0000003E
    0000007E0000037F0000077F00001F7F00007FFF00007FFF00007FFF0000}
  OldCreateOrder = False
  Position = poDefault
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Chart1: TChart
    Left = 0
    Top = 0
    Width = 528
    Height = 339
    BackWall.Brush.Color = clAqua
    BackWall.Color = clWhite
    BackWall.Transparent = False
    Foot.Alignment = taRightJustify
    Foot.Font.Color = clBlue
    Foot.Text.Strings = (
      'Date / Time')
    Legend.Visible = False
    MarginBottom = 5
    MarginLeft = 2
    MarginRight = 2
    MarginTop = 5
    Title.Text.Strings = (
      'TChart')
    BottomAxis.Axis.Visible = False
    BottomAxis.LabelsOnAxis = False
    BottomAxis.LabelStyle = talValue
    Hover.Visible = False
    LeftAxis.Axis.Visible = False
    TopAxis.Axis.Visible = False
    TopAxis.Grid.SmallDots = True
    TopAxis.LabelStyle = talText
    TopAxis.MinorTicks.Visible = False
    TopAxis.TicksInner.Visible = False
    TopAxis.Title.Angle = 90
    TopAxis.Title.Font.Charset = ANSI_CHARSET
    TopAxis.Title.Font.Height = -8
    TopAxis.Title.Font.Name = 'Small Fonts'
    View3D = False
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    TabOrder = 0
    OnMouseDown = Chart1MouseDown
    DefaultCanvas = 'TGDIPlusCanvas'
    ColorPaletteIndex = 13
    object RefreshBtn: TSpeedButton
      Left = 0
      Top = 0
      Width = 24
      Height = 24
      Hint = 'Refresh'
      Glyph.Data = {
        4E010000424D4E01000000000000760000002800000012000000120000000100
        040000000000D800000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333444444
        3333330000003243342222224433330000003224422222222243330000003222
        222AAAAA222433000000322222A33333A222430000003222223333333A224300
        00003222222333333A44430000003AAAAAAA3333333333000000333333333333
        3333330000003333333333334444440000003A444333333A2222240000003A22
        43333333A2222400000033A22433333442222400000033A22244444222222400
        0000333A2222222222AA240000003333AA222222AA33A3000000333333AAAAAA
        333333000000333333333333333333000000}
      ParentShowHint = False
      ShowHint = True
      OnClick = RefreshBtnClick
    end
    object LinkInvert: TLineSeries
      SeriesColor = clBlack
      Title = 'LinkInvert'
      BeforeDrawValues = LinkInvertBeforeDrawValues
      Brush.BackColor = clDefault
      LinePen.Width = 2
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object LinkCrown: TLineSeries
      SeriesColor = clBlack
      Title = 'LinkCrown'
      Brush.BackColor = clDefault
      LinePen.Width = 2
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object NodeInvert: TPointSeries
      HorizAxis = aBothHorizAxis
      Title = 'NodeInvert'
      AfterDrawValues = NodeInvertAfterDrawValues
      ClickableLine = False
      Pointer.Brush.Color = clBlack
      Pointer.Brush.Gradient.EndColor = 10708548
      Pointer.Gradient.EndColor = 10708548
      Pointer.HorizSize = 8
      Pointer.InflateMargins = True
      Pointer.Pen.Visible = False
      Pointer.Style = psRectangle
      Pointer.VertSize = 1
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object GroundLine: TLineSeries
      Marks.Brush.Style = bsClear
      Marks.Font.Shadow.Visible = False
      Marks.Frame.Visible = False
      Marks.Visible = True
      Marks.Arrow.Visible = False
      Marks.BackColor = clWhite
      Marks.Callout.Arrow.Visible = False
      Marks.Color = clWhite
      SeriesColor = clGreen
      Title = 'GroundLine'
      Brush.BackColor = clDefault
      ClickableLine = False
      LinePen.Width = 2
      Pointer.Brush.Gradient.EndColor = clGreen
      Pointer.Gradient.EndColor = clGreen
      Pointer.HorizSize = 1
      Pointer.InflateMargins = True
      Pointer.Style = psSmallDot
      Pointer.VertSize = 1
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
  end
end
