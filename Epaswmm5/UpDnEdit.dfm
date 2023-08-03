object UpDnEditBox: TUpDnEditBox
  Left = 0
  Top = 0
  Width = 74
  Height = 21
  AutoSize = True
  TabOrder = 0
  object Spinner: TUpDown
    Left = 58
    Top = 0
    Width = 16
    Height = 21
    Associate = EditBox
    TabOrder = 0
  end
  object EditBox: TEdit
    Left = 0
    Top = 0
    Width = 58
    Height = 21
    NumbersOnly = True
    TabOrder = 1
    Text = '0'
  end
end
