object GroupEditForm: TGroupEditForm
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Group Editor'
  ClientHeight = 239
  ClientWidth = 321
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 16
    Top = 23
    Width = 98
    Height = 15
    Caption = 'For objects of type'
  end
  object Label2: TLabel
    Left = 16
    Top = 103
    Width = 88
    Height = 15
    Caption = 'edit the property'
  end
  object ReplaceWithLabel: TLabel
    Left = 16
    Top = 147
    Width = 101
    Height = 15
    Caption = 'by replacing it with'
  end
  object NoOptionsLabel: TLabel
    Left = 16
    Top = 168
    Width = 160
    Height = 15
    Caption = 'There are no options available.'
  end
  object OKBtn: TButton
    Left = 31
    Top = 192
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 7
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 119
    Top = 192
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 8
  end
  object HelpBtn: TButton
    Left = 207
    Top = 192
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 9
    OnClick = HelpBtnClick
  end
  object ClassListbox: TComboBox
    Left = 176
    Top = 20
    Width = 121
    Height = 24
    Style = csOwnerDrawFixed
    Ctl3D = True
    ItemHeight = 18
    ParentCtl3D = False
    TabOrder = 0
    OnChange = ClassListboxChange
  end
  object TagCheckBox: TCheckBox
    Left = 16
    Top = 62
    Width = 137
    Height = 17
    Caption = 'with Tag equal to'
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 1
    OnClick = TagCheckBoxClick
  end
  object PropertyListBox: TComboBox
    Left = 176
    Top = 100
    Width = 121
    Height = 24
    Style = csOwnerDrawFixed
    Ctl3D = True
    ItemHeight = 18
    ParentCtl3D = False
    TabOrder = 3
    OnChange = PropertyListBoxChange
  end
  object PropertyNumEdit: TNumEdit
    Left = 176
    Top = 144
    Width = 121
    Height = 23
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 5
    Style = esNone
    Modified = False
    SelLength = 0
    SelStart = 0
  end
  object PropertyEditBtn: TButton
    Left = 278
    Top = 144
    Width = 19
    Height = 21
    Caption = '...'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 6
    Visible = False
    OnClick = PropertyEditBtnClick
  end
  object TagEditBox: TNumEdit
    Left = 176
    Top = 60
    Width = 121
    Height = 23
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 2
    Style = esNoSpace
    Modified = False
    SelLength = 0
    SelStart = 0
  end
  object EditTypeListBox: TComboBox
    Left = 16
    Top = 124
    Width = 137
    Height = 24
    Style = csOwnerDrawFixed
    Ctl3D = True
    ItemHeight = 18
    ItemIndex = 0
    ParentCtl3D = False
    TabOrder = 4
    Text = 'by replacing it with'
    OnChange = EditTypeListBoxChange
    Items.Strings = (
      'by replacing it with'
      'by multiplying it by'
      'by adding to it')
  end
  object SelectionCombo: TComboBox
    Left = 177
    Top = 113
    Width = 121
    Height = 23
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 10
    Visible = False
  end
end
