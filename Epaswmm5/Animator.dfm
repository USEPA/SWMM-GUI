object AnimatorFrame: TAnimatorFrame
  Left = 0
  Top = 0
  Width = 129
  Height = 89
  AutoSize = True
  TabOrder = 0
  object AnimatorBox: TGroupBox
    Left = 0
    Top = 0
    Width = 129
    Height = 89
    Caption = 'Animator'
    TabOrder = 0
    object SpeedBar: TTrackBar
      Left = 10
      Top = 46
      Width = 108
      Height = 33
      Hint = 'Animation Speed'
      ParentShowHint = False
      PageSize = 1
      Position = 5
      ShowHint = True
      TabOrder = 0
      OnChange = SpeedBarChange
    end
    object AnimatorToolBar: TToolBar
      Left = 2
      Top = 15
      Width = 125
      Height = 28
      AutoSize = True
      ButtonHeight = 28
      ButtonWidth = 26
      DrawingStyle = dsGradient
      Indent = 12
      GradientDrawingOptions = [gdoHotTrack]
      TabOrder = 1
      Transparent = True
      Wrapable = False
      object RewindBtn: TToolButton
        Left = 12
        Top = 0
        Hint = 'Rewind'
        AllowAllUp = True
        Enabled = False
        ImageIndex = 0
        ParentShowHint = False
        ShowHint = True
        OnClick = RewindBtnClick
      end
      object BackBtn: TToolButton
        Left = 38
        Top = 0
        Hint = 'Back'
        AllowAllUp = True
        Enabled = False
        Grouped = True
        ImageIndex = 1
        ParentShowHint = False
        ShowHint = True
        Style = tbsCheck
        OnClick = BackBtnClick
      end
      object PauseBtn: TToolButton
        Left = 64
        Top = 0
        Hint = 'Pause'
        AllowAllUp = True
        Enabled = False
        Grouped = True
        ImageIndex = 2
        ParentShowHint = False
        ShowHint = True
        Style = tbsCheck
        OnClick = PauseBtnClick
      end
      object FwdBtn: TToolButton
        Left = 90
        Top = 0
        Hint = 'Foward'
        AllowAllUp = True
        Enabled = False
        Grouped = True
        ImageIndex = 3
        ParentShowHint = False
        ShowHint = True
        Style = tbsCheck
        OnClick = FwdBtnClick
      end
    end
  end
  object Timer: TTimer
    Interval = 500
    OnTimer = TimerTimer
    Left = 16
    Top = 56
  end
end
