object MapForm: TMapForm
  Left = 242
  Top = 132
  Caption = 'Study Area Map'
  ClientHeight = 445
  ClientWidth = 509
  Color = clBtnFace
  DoubleBuffered = True
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
    00000000000000007000000700000000700000070000000AAA0000DDD000077A
    AA7777DDD770000AAA0000DDD000000070000007000000007000000700000000
    7000000700000000700000070000000EEE0000BBB000077EEE7777BBB770000E
    EE0000BBB000000070000007000000007000000700000000000000000000FFFF
    0000F7EF0000F7EF0000E3C7000080010000E3C70000F7EF0000F7EF0000F7EF
    0000F7EF0000E3C7000080010000E3C70000F7EF0000F7EF0000FFFF0000}
  KeyPreview = True
  OldCreateOrder = True
  Position = poDefault
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDblClick = FormDblClick
  OnDestroy = FormDestroy
  OnDragDrop = FormDragDrop
  OnDragOver = FormDragOver
  OnKeyDown = FormKeyDown
  OnMouseDown = FormMouseDown
  OnMouseMove = FormMouseMove
  OnMouseUp = FormMouseUp
  OnMouseWheel = FormMouseWheel
  OnPaint = FormPaint
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 15
  object SubcatchLegendPanel: TPanel
    Left = 292
    Top = 50
    Width = 100
    Height = 180
    BevelOuter = bvNone
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 2
    Visible = False
    object SubcatchLegendBox: TPaintBox
      Left = 0
      Top = 0
      Width = 100
      Height = 180
      Cursor = crDrag
      Align = alClient
      OnDblClick = SubcatchLegendBoxDblClick
      OnMouseDown = DragLegend
      OnPaint = SubcatchLegendBoxPaint
    end
  end
  object NodeLegendPanel: TPanel
    Left = 53
    Top = 50
    Width = 100
    Height = 180
    BevelOuter = bvNone
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 0
    Visible = False
    object NodeLegendBox: TPaintBox
      Left = 0
      Top = 0
      Width = 100
      Height = 180
      Cursor = crDrag
      Align = alClient
      OnDblClick = NodeLegendBoxDblClick
      OnMouseDown = DragLegend
      OnPaint = NodeLegendBoxPaint
    end
  end
  object LinkLegendPanel: TPanel
    Left = 172
    Top = 50
    Width = 100
    Height = 180
    BevelOuter = bvNone
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 1
    Visible = False
    object LinkLegendBox: TPaintBox
      Left = 0
      Top = 0
      Width = 100
      Height = 180
      Cursor = crDrag
      Align = alClient
      OnDblClick = LinkLegendBoxDblClick
      OnMouseDown = DragLegend
      OnPaint = LinkLegendBoxPaint
    end
  end
  object TimeLegendBox: TScrollBox
    Left = 256
    Top = 8
    Width = 161
    Height = 25
    HorzScrollBar.Visible = False
    VertScrollBar.Visible = False
    BorderStyle = bsNone
    Color = clBlack
    Ctl3D = False
    ParentColor = False
    ParentCtl3D = False
    TabOrder = 3
    object TimeLegendPanel: TPanel
      Left = 0
      Top = 0
      Width = 161
      Height = 25
      BevelInner = bvRaised
      BevelOuter = bvLowered
      Caption = 'TimeLegendPanel'
      Color = clBlack
      Ctl3D = False
      Font.Charset = ANSI_CHARSET
      Font.Color = clLime
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentCtl3D = False
      ParentFont = False
      TabOrder = 0
      OnDblClick = TimeLegendPanelDblClick
      OnMouseDown = TimeLegendPanelMouseDown
    end
  end
  object HintPanel: TPanel
    Left = 40
    Top = 272
    Width = 49
    Height = 17
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Color = clInfoBk
    Ctl3D = False
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 4
    object HintLabel: TLabel
      Left = 0
      Top = 0
      Width = 47
      Height = 15
      Align = alClient
      Caption = 'HintLabel'
      ExplicitWidth = 51
    end
  end
  object MapToolBar: TToolBar
    Left = 482
    Top = 0
    Width = 27
    Height = 445
    Align = alRight
    AutoSize = True
    Caption = 'ToolBar1'
    Color = clBtnFace
    DisabledImages = MainForm.BtnImageList1
    DrawingStyle = dsGradient
    EdgeBorders = [ebLeft]
    Images = MainForm.BtnImageList
    Indent = 2
    ParentColor = False
    TabOrder = 5
    Transparent = False
    object ToolButton1: TToolButton
      Tag = 101
      Left = 2
      Top = 0
      Hint = 'Select an object'
      Caption = 'ToolButton1'
      ImageIndex = 18
      ImageName = 'MapToolbar\Item19'
      ParentShowHint = False
      Wrap = True
      ShowHint = True
      OnClick = MapButtonClick
    end
    object ToolButton2: TToolButton
      Tag = 102
      Left = 2
      Top = 22
      Hint = 'Select a vertex'
      Caption = 'ToolButton2'
      ImageIndex = 19
      ImageName = 'MapToolbar\Item20'
      ParentShowHint = False
      Wrap = True
      ShowHint = True
      OnClick = MapButtonClick
    end
    object ToolButton3: TToolButton
      Tag = 103
      Left = 2
      Top = 44
      Hint = 'Select a region'
      Caption = 'ToolButton3'
      ImageIndex = 20
      ImageName = 'MapToolbar\Item21'
      ParentShowHint = False
      Wrap = True
      ShowHint = True
      OnClick = MapButtonClick
    end
    object ToolButton4: TToolButton
      Tag = 104
      Left = 2
      Top = 66
      Hint = 'Pan the map'
      Caption = 'ToolButton4'
      ImageIndex = 21
      ImageName = 'MapToolbar\Item22'
      ParentShowHint = False
      Wrap = True
      ShowHint = True
      OnClick = MapButtonClick
    end
    object ToolButton5: TToolButton
      Tag = 105
      Left = 2
      Top = 88
      Hint = 'Zoom in'
      Caption = 'ToolButton5'
      ImageIndex = 22
      ImageName = 'MapToolbar\Item23'
      ParentShowHint = False
      Wrap = True
      ShowHint = True
      OnClick = MapButtonClick
    end
    object ToolButton6: TToolButton
      Tag = 106
      Left = 2
      Top = 110
      Hint = 'Zoom out'
      Caption = 'ToolButton6'
      ImageIndex = 23
      ImageName = 'MapToolbar\Item24'
      ParentShowHint = False
      Wrap = True
      ShowHint = True
      OnClick = MapButtonClick
    end
    object ToolButton7: TToolButton
      Tag = 107
      Left = 2
      Top = 132
      Hint = 'View at full extent'
      Caption = 'ToolButton7'
      ImageIndex = 24
      ImageName = 'MapToolbar\Item25'
      ParentShowHint = False
      Wrap = True
      ShowHint = True
      OnClick = ToolButton7Click
    end
    object ToolButton8: TToolButton
      Tag = 108
      Left = 2
      Top = 154
      Hint = 'Measure a distance or area'
      Caption = 'ToolButton8'
      ImageIndex = 25
      ImageName = 'MapToolbar\Item26'
      ParentShowHint = False
      Wrap = True
      ShowHint = True
      OnClick = MapButtonClick
    end
    object ToolButton9: TToolButton
      Tag = 2
      Left = 2
      Top = 176
      Hint = 'Add a rain gage'
      Caption = 'ToolButton9'
      ImageIndex = 26
      ImageName = 'ObjectToolbar\Item27'
      ParentShowHint = False
      Wrap = True
      ShowHint = True
      OnClick = ObjButtonClick
    end
    object ToolButton10: TToolButton
      Tag = 3
      Left = 2
      Top = 198
      Hint = 'Add a subcatchment'
      Caption = 'ToolButton10'
      ImageIndex = 27
      ImageName = 'ObjectToolbar\Item28'
      ParentShowHint = False
      Wrap = True
      ShowHint = True
      OnClick = ObjButtonClick
    end
    object ToolButton11: TToolButton
      Tag = 4
      Left = 2
      Top = 220
      Hint = 'Add a junction node'
      Caption = 'ToolButton11'
      ImageIndex = 28
      ImageName = 'ObjectToolbar\Item29'
      ParentShowHint = False
      Wrap = True
      ShowHint = True
      OnClick = ObjButtonClick
    end
    object ToolButton12: TToolButton
      Tag = 5
      Left = 2
      Top = 242
      Hint = 'Add an outfall node'
      Caption = 'ToolButton12'
      ImageIndex = 29
      ImageName = 'ObjectToolbar\Item30'
      ParentShowHint = False
      Wrap = True
      ShowHint = True
      OnClick = ObjButtonClick
    end
    object ToolButton13: TToolButton
      Tag = 6
      Left = 2
      Top = 264
      Hint = 'Add a flow divider node'
      Caption = 'ToolButton13'
      ImageIndex = 30
      ImageName = 'ObjectToolbar\Item31'
      ParentShowHint = False
      Wrap = True
      ShowHint = True
      OnClick = ObjButtonClick
    end
    object ToolButton14: TToolButton
      Tag = 7
      Left = 2
      Top = 286
      Hint = 'Add a storage node'
      Caption = 'ToolButton14'
      ImageIndex = 31
      ImageName = 'ObjectToolbar\Item32'
      ParentShowHint = False
      Wrap = True
      ShowHint = True
      OnClick = ObjButtonClick
    end
    object ToolButton15: TToolButton
      Tag = 8
      Left = 2
      Top = 308
      Hint = 'Add a conduit link'
      Caption = 'ToolButton15'
      ImageIndex = 32
      ImageName = 'ObjectToolbar\Item33'
      ParentShowHint = False
      Wrap = True
      ShowHint = True
      OnClick = ObjButtonClick
    end
    object ToolButton16: TToolButton
      Tag = 9
      Left = 2
      Top = 330
      Hint = 'Add a pump link'
      Caption = 'ToolButton16'
      ImageIndex = 33
      ImageName = 'ObjectToolbar\Item34'
      ParentShowHint = False
      Wrap = True
      ShowHint = True
      OnClick = ObjButtonClick
    end
    object ToolButton17: TToolButton
      Tag = 10
      Left = 2
      Top = 352
      Hint = 'Add an orifice link'
      Caption = 'ToolButton17'
      ImageIndex = 34
      ImageName = 'ObjectToolbar\Item35'
      ParentShowHint = False
      Wrap = True
      ShowHint = True
      OnClick = ObjButtonClick
    end
    object ToolButton18: TToolButton
      Tag = 11
      Left = 2
      Top = 374
      Hint = 'Add a weir link'
      Caption = 'ToolButton18'
      ImageIndex = 35
      ImageName = 'ObjectToolbar\Item36'
      ParentShowHint = False
      Wrap = True
      ShowHint = True
      OnClick = ObjButtonClick
    end
    object ToolButton19: TToolButton
      Tag = 12
      Left = 2
      Top = 396
      Hint = 'Add an outlet link'
      Caption = 'ToolButton19'
      ImageIndex = 36
      ImageName = 'ObjectToolbar\Item37'
      ParentShowHint = False
      Wrap = True
      ShowHint = True
      OnClick = ObjButtonClick
    end
    object ToolButton20: TToolButton
      Tag = 13
      Left = 2
      Top = 418
      Hint = 'Add a map label'
      Caption = 'ToolButton20'
      ImageIndex = 37
      ImageName = 'ObjectToolbar\Item38'
      ParentShowHint = False
      ShowHint = True
      OnClick = ObjButtonClick
    end
  end
  object PopupMenu1: TPopupMenu
    AutoPopup = False
    OnPopup = PopupMenu1Popup
    Left = 258
    Top = 241
    object PopupCopy: TMenuItem
      Caption = 'Copy'
      OnClick = PopupCopyClick
    end
    object PopupPaste: TMenuItem
      Caption = 'Paste'
      OnClick = PopupPasteClick
    end
    object PopupDelete: TMenuItem
      Caption = 'Delete'
      OnClick = PopupDeleteClick
    end
    object PopupReverse: TMenuItem
      Caption = 'Reverse'
      OnClick = PopupReverseClick
    end
    object PopupConvert: TMenuItem
      Caption = 'Convert to ...'
      object ConvertToType1: TMenuItem
        Tag = 1
        OnClick = ConvertToTypeClick
      end
      object ConvertToType2: TMenuItem
        Tag = 2
        OnClick = ConvertToTypeClick
      end
      object ConvertToType3: TMenuItem
        Tag = 3
        OnClick = ConvertToTypeClick
      end
      object ConvertToType4: TMenuItem
        Tag = 4
        OnClick = ConvertToTypeClick
      end
      object ConvertToType5: TMenuItem
        Tag = 5
        OnClick = ConvertToTypeClick
      end
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object PopupVertices: TMenuItem
      Caption = 'Vertices'
      OnClick = PopupVerticesClick
    end
    object PopupProperties: TMenuItem
      Caption = 'Properties'
      OnClick = PopupPropertiesClick
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 500
    OnTimer = Timer1Timer
    Left = 108
    Top = 241
  end
  object PopupMenu2: TPopupMenu
    Left = 328
    Top = 242
    object PopupObjects: TMenuItem
      Caption = 'L&ayers'
      OnClick = PopupObjectsClick
      object PopupShowGages: TMenuItem
        Caption = '&Rain Gages'
        OnClick = PopupShowObjectsClick
      end
      object PopupShowSubcatch: TMenuItem
        Caption = '&Subcatchments'
        OnClick = PopupShowObjectsClick
      end
      object PopupShowNodes: TMenuItem
        Caption = '&Nodes'
        OnClick = PopupShowObjectsClick
      end
      object PopupShowLinks: TMenuItem
        Caption = '&Links'
        OnClick = PopupShowObjectsClick
      end
      object PopupShowLabels: TMenuItem
        Caption = 'Lab&els'
        OnClick = PopupShowObjectsClick
      end
      object PopupShowBackdrop: TMenuItem
        Caption = '&Backdrop'
        OnClick = PopupShowBackdropClick
      end
    end
    object PopupLegends: TMenuItem
      Caption = '&Legends'
      OnClick = PopupLegendsClick
      object PopupSubcatchLegend: TMenuItem
        Caption = '&Subcatchment'
        OnClick = PopupSubcatchLegendClick
      end
      object PopupNodeLegend: TMenuItem
        Caption = '&Node'
        OnClick = PopupNodeLegendClick
      end
      object PopupLinkLegend: TMenuItem
        Caption = '&Link'
        OnClick = PopupLinkLegendClick
      end
      object PopupTimeLegend: TMenuItem
        Caption = '&Time'
        OnClick = PopupTimeLegendClick
      end
    end
    object PopupOptions: TMenuItem
      Caption = 'O&ptions...'
      OnClick = PopupOptionsClick
    end
  end
  object PopupMenu3: TPopupMenu
    OnPopup = PopupMenu3Popup
    Left = 328
    Top = 283
    object PopupAddVertex: TMenuItem
      Caption = 'Add Vertex'
      OnClick = PopupAddVertexClick
    end
    object PopupDeleteVertex: TMenuItem
      Caption = 'Delete Vertex'
      OnClick = PopupDeleteVertexClick
    end
    object PopupDeleteAllVertices: TMenuItem
      Caption = 'Delete All'
      OnClick = PopupDeleteAllVerticesClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object PopupQuitEditing: TMenuItem
      Caption = 'Quit Editing'
      OnClick = PopupQuitEditingClick
    end
  end
  object Timer2: TTimer
    Interval = 500
    OnTimer = Timer2Timer
    Left = 152
    Top = 241
  end
  object Timer3: TTimer
    Interval = 50
    OnTimer = Timer3Timer
    Left = 200
    Top = 240
  end
  object ImageCollection1: TImageCollection
    Images = <
      item
        Name = 'inlet9'
        SourceImages = <
          item
            Image.Data = {
              424D7A0100000000000036000000280000000900000009000000010020000000
              0000000000000000000000000000000000000000000000000000000000000000
              000000000000000000000000000000000000000000000000000023232300FFFF
              FF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00000000000000
              0000FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF000000
              000037373700FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFF
              FF00000000004444440000000000000000000000000000000000000000000000
              0000000000000000000045454500FFFFFF00FFFFFF00FFFFFF0000000000FFFF
              FF00FFFFFF00FFFFFF000000000036363600FFFFFF00FFFFFF00FFFFFF000000
              0000FFFFFF00FFFFFF00FFFFFF000000000000000000FFFFFF00FFFFFF00FFFF
              FF0000000000FFFFFF00FFFFFF00FFFFFF000000000023232300000000003737
              3700454545004545450036363600000000002323230000000000}
          end>
      end>
    Left = 32
    Top = 8
  end
  object VirtualImageList1: TVirtualImageList
    DisabledGrayscale = False
    DisabledSuffix = '_Disabled'
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'inlet9'
        Disabled = False
        Name = 'inlet9'
      end>
    ImageCollection = ImageCollection1
    Width = 9
    Height = 9
    Left = 136
    Top = 8
  end
end
