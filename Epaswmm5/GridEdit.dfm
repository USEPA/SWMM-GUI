object GridEditFrame: TGridEditFrame
  Left = 0
  Top = 0
  Width = 320
  Height = 154
  TabOrder = 0
  object Grid: TStringGrid
    Left = 0
    Top = 0
    Width = 320
    Height = 154
    Ctl3D = False
    DrawingStyle = gdsClassic
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goThumbTracking]
    ParentCtl3D = False
    PopupMenu = PopupMenu
    TabOrder = 0
    OnClick = GridClick
    OnDblClick = GridDblClick
    OnDrawCell = GridDrawCell
    OnKeyPress = GridKeyPress
  end
  object EditPanel: TPanel
    Left = 1
    Top = 1
    Width = 185
    Height = 41
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Color = clWindow
    Ctl3D = False
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 1
    Visible = False
    object EditBox: TNumEdit
      Left = 0
      Top = 1
      Width = 121
      Height = 19
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      TabOrder = 0
      OnExit = EditBoxExit
      OnKeyDown = EditBoxKeyDown
      OnKeyPress = EditBoxKeyPress
      Style = esNone
      Modified = False
      SelLength = 0
      SelStart = 0
    end
  end
  object PopupMenu: TPopupMenu
    OnPopup = PopupMenuPopup
    Left = 208
    Top = 40
    object MnuCut: TMenuItem
      Caption = 'Cu&t'
      OnClick = MenuItemClick
    end
    object MnuCopy: TMenuItem
      Tag = 1
      Caption = '&Copy'
      OnClick = MenuItemClick
    end
    object MnuPaste: TMenuItem
      Tag = 2
      Caption = '&Paste'
      OnClick = MenuItemClick
    end
    object MnuInsert: TMenuItem
      Tag = 3
      Caption = '&Insert'
      OnClick = MenuItemClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object MnuInsertRow: TMenuItem
      Tag = 5
      Caption = 'I&nsert Row'
      OnClick = MenuItemClick
    end
    object MnuDeleteRow: TMenuItem
      Tag = 6
      Caption = '&Delete Row'
      OnClick = MenuItemClick
    end
  end
end
