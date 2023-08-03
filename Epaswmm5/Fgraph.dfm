object GraphForm: TGraphForm
  Left = 193
  Top = 164
  Caption = 'GraphForm'
  ClientHeight = 337
  ClientWidth = 528
  Color = clBtnFace
  ParentFont = True
  FormStyle = fsMDIChild
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FF8F
    FF8FFF8FFF8FFF8F8F8F8F8F8F8F8800000000000000FF0FFFFFFFFFFFFFF809
    FFFFFF4FFFFFFF0F9F99F4F4FFFF8808989898848888FF0FF9FF9FFF4FFFF804
    FFF4F9FF4FF4FF0F4F4FF9FF4F4F8808484888988488FF0FF4FFFFF9FFFFF80F
    F4FFFFFF9FFFFF0FFFFFFFFFF9998808888888888888FFFFFFFFFFFFFFFF0000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  OldCreateOrder = True
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
    Height = 337
    BackWall.Brush.Gradient.Balance = 67
    BackWall.Brush.Gradient.EndColor = 15461355
    BackWall.Brush.Gradient.Visible = True
    BackWall.Color = clWhite
    BackWall.Transparent = False
    Foot.Alignment = taRightJustify
    Foot.Font.Height = -12
    Legend.Font.Height = -12
    Legend.Symbol.Squared = False
    Legend.Title.Font.Height = -12
    Legend.TopPos = 0
    Legend.Visible = False
    MarginBottom = 5
    MarginLeft = 2
    MarginRight = 2
    MarginTop = 5
    ScrollMouseButton = mbLeft
    SubFoot.Font.Height = -12
    SubTitle.Font.Height = -12
    Title.Font.Height = -12
    Title.Text.Strings = (
      'TChart')
    BottomAxis.Grid.Visible = False
    BottomAxis.LabelsFormat.Font.Height = -12
    BottomAxis.LabelsMultiLine = True
    BottomAxis.MinorTickCount = 0
    BottomAxis.Title.Caption = 'X Axis'
    BottomAxis.Title.Font.Height = -12
    Chart3DPercent = 25
    DepthAxis.LabelsFormat.Font.Height = -12
    DepthAxis.Title.Font.Height = -12
    DepthTopAxis.LabelsFormat.Font.Height = -12
    DepthTopAxis.Title.Font.Height = -12
    Hover.Visible = False
    LeftAxis.AxisValuesFormat = '0.0#'
    LeftAxis.Grid.Visible = False
    LeftAxis.LabelsFormat.Font.Height = -12
    LeftAxis.MinorTickCount = 0
    LeftAxis.Title.Caption = 'Y Axis'
    LeftAxis.Title.Font.Height = -12
    RightAxis.Axis.Visible = False
    RightAxis.AxisValuesFormat = '0.0#'
    RightAxis.Grid.Visible = False
    RightAxis.LabelsFormat.Font.Height = -12
    RightAxis.MinorTickCount = 0
    RightAxis.Title.Font.Height = -12
    TopAxis.Axis.Visible = False
    TopAxis.LabelsFormat.Font.Height = -12
    TopAxis.Title.Font.Height = -12
    View3D = False
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    OnMouseDown = Chart1MouseDown
    DefaultCanvas = 'TGDIPlusCanvas'
    ColorPaletteIndex = 7
    object SpeedButton1: TSpeedButton
      Left = 1
      Top = 1
      Width = 23
      Height = 22
      Hint = 'Lock/Unlock'
      AllowAllUp = True
      GroupIndex = 1
      Flat = True
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton1Click
    end
  end
end
