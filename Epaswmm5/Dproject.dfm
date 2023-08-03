object ProjectForm: TProjectForm
  Left = 0
  Top = 0
  Caption = 'Project Data'
  ClientHeight = 422
  ClientWidth = 584
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poDefault
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object Splitter1: TSplitter
    Left = 116
    Top = 0
    Height = 422
    AutoSnap = False
    Beveled = True
    ExplicitLeft = 152
    ExplicitTop = 104
    ExplicitHeight = 100
  end
  object Panel1: TPanel
    Left = 119
    Top = 0
    Width = 465
    Height = 422
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    OnResize = Panel1Resize
    object StringGrid1: TStringGrid
      Left = 0
      Top = 0
      Width = 465
      Height = 422
      Align = alClient
      ColCount = 1
      Ctl3D = True
      DefaultColWidth = 1000
      DrawingStyle = gdsClassic
      FixedCols = 0
      RowCount = 500
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Courier New'
      Font.Style = []
      Options = [goFixedHorzLine, goThumbTracking]
      ParentCtl3D = False
      ParentFont = False
      TabOrder = 0
      OnDrawCell = StringGrid1DrawCell
    end
    object Edit1: TEdit
      Left = 232
      Top = 352
      Width = 121
      Height = 23
      TabStop = False
      ReadOnly = True
      TabOrder = 1
      Text = 'Edit1'
      Visible = False
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 116
    Height = 422
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 116
      Height = 25
      Align = alTop
      Caption = 'Data Category'
      TabOrder = 1
    end
    object ListBox1: TListBox
      Left = 0
      Top = 25
      Width = 116
      Height = 397
      Style = lbOwnerDrawFixed
      Align = alClient
      Ctl3D = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Courier New'
      Font.Style = []
      ItemHeight = 18
      ParentCtl3D = False
      ParentFont = False
      TabOrder = 0
      OnClick = ListBox1Click
    end
  end
end
