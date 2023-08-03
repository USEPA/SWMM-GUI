object WelcomeForm: TWelcomeForm
  Left = 0
  Top = 0
  BorderStyle = bsNone
  ClientHeight = 572
  ClientWidth = 558
  Color = clWhite
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 558
    Height = 572
    Align = alClient
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Color = clWhite
    Ctl3D = False
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 0
    StyleElements = [seFont, seBorder]
    object Label2: TLabel
      Left = 32
      Top = 79
      Width = 103
      Height = 30
      Caption = 'Get Started'
      Color = clWhite
      Font.Charset = ANSI_CHARSET
      Font.Color = clGrayText
      Font.Height = -21
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label3: TLabel
      Left = 32
      Top = 203
      Width = 147
      Height = 30
      Caption = 'Sample Projects'
      Color = clWhite
      Font.Charset = ANSI_CHARSET
      Font.Color = clGrayText
      Font.Height = -21
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label4: TLabel
      Left = 288
      Top = 203
      Width = 207
      Height = 30
      Caption = 'Open a Recent Project'
      Color = clWhite
      Font.Charset = ANSI_CHARSET
      Font.Color = clGrayText
      Font.Height = -21
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label5: TLabel
      Left = 288
      Top = 79
      Width = 76
      Height = 30
      Caption = 'Develop'
      Color = clWhite
      Font.Charset = ANSI_CHARSET
      Font.Color = clGrayText
      Font.Height = -21
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label1: TLabel
      Left = 392
      Top = 553
      Width = 146
      Height = 15
      Caption = 'There are no recent projects'
    end
    object GetStartedListView: TListView
      Left = 32
      Top = 120
      Width = 210
      Height = 89
      Cursor = crHandPoint
      BorderStyle = bsNone
      Columns = <>
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      HideSelection = False
      IconOptions.WrapText = False
      Items.ItemData = {
        05640000000200000002000000FFFFFFFFFFFFFFFF00000000FFFFFFFF000000
        000E4200610073006900630020005400750074006F007200690061006C000200
        0000FFFFFFFFFFFFFFFF00000000FFFFFFFF000000000A550073006500720020
        0047007500690064006500}
      ReadOnly = True
      ParentFont = False
      SmallImages = VirtualImageList1
      TabOrder = 3
      TabStop = False
      ViewStyle = vsList
      OnSelectItem = GetStartedListViewSelectItem
    end
    object SamplesListView: TListView
      Left = 32
      Top = 243
      Width = 240
      Height = 282
      Cursor = crHandPoint
      BorderStyle = bsNone
      Columns = <>
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      IconOptions.WrapText = False
      ReadOnly = True
      RowSelect = True
      ParentFont = False
      SmallImages = VirtualImageList1
      TabOrder = 0
      TabStop = False
      ViewStyle = vsList
      OnSelectItem = SamplesListViewSelectItem
    end
    object ShowStartPageCB: TCheckBox
      Left = 43
      Top = 538
      Width = 209
      Height = 17
      TabStop = False
      Caption = 'Always show at startup'
      TabOrder = 4
    end
    object ProjectsListView: TListView
      Left = 288
      Top = 243
      Width = 240
      Height = 282
      Cursor = crHandPoint
      BorderStyle = bsNone
      Columns = <>
      ColumnClick = False
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      HideSelection = False
      IconOptions.WrapText = False
      ReadOnly = True
      RowSelect = True
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      SmallImages = VirtualImageList1
      TabOrder = 2
      TabStop = False
      ViewStyle = vsList
      OnInfoTip = ProjectsListViewInfoTip
      OnSelectItem = ProjectsListViewSelectItem
    end
    object Panel2: TPanel
      Left = 0
      Top = 0
      Width = 556
      Height = 57
      Align = alTop
      Caption = 'WELCOME TO SWMM'
      Color = clWhite
      Font.Charset = ANSI_CHARSET
      Font.Color = clGrayText
      Font.Height = -27
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentBackground = False
      ParentFont = False
      TabOrder = 5
      StyleElements = [seFont, seBorder]
      object CloseButton: TSpeedButton
        Left = 521
        Top = 17
        Width = 23
        Height = 22
        Caption = #61647
        Flat = True
        Font.Charset = SYMBOL_CHARSET
        Font.Color = clGrayText
        Font.Height = -21
        Font.Name = 'Wingdings 2'
        Font.Style = []
        ParentFont = False
        OnClick = CloseButtonClick
      end
    end
    object ClearProjectsLinkLabel: TLinkLabel
      Left = 299
      Top = 538
      Width = 125
      Height = 19
      Caption = '<a>Clear recent project list</a>'
      TabOrder = 6
      UseVisualStyle = True
      OnLinkClick = ClearProjectsLinkLabelLinkClick
    end
    object DevelopListView: TListView
      Left = 288
      Top = 119
      Width = 240
      Height = 89
      Cursor = crHandPoint
      BorderStyle = bsNone
      Columns = <>
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      IconOptions.WrapText = False
      Items.ItemData = {
        058C0000000200000000000000FFFFFFFFFFFFFFFF00000000FFFFFFFF000000
        00144300720065006100740065002000610020006E0065007700200070007200
        6F006A0065006300740000000000FFFFFFFFFFFFFFFF00000000FFFFFFFF0000
        0000184F00700065006E00200061006E0020006500780069007300740069006E
        0067002000700072006F006A00650063007400}
      ReadOnly = True
      RowSelect = True
      ParentFont = False
      SmallImages = VirtualImageList1
      TabOrder = 1
      TabStop = False
      ViewStyle = vsList
      OnSelectItem = DevelopListViewSelectItem
    end
  end
  object ImageCollection1: TImageCollection
    Images = <
      item
        Name = 'Item1'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F4000002CA4944415478DAEDD55D4853611CC7F1AFAB4DA5B6A9CD56348DD4CC
              6C286566526691545A9437BD484111759150140462109251D10B05469078616F
              17414550994E444C34157AF1A5D55633255BD932CDCD339D73676D5E7455A070
              228AFDCECD81E7E1F97F9EF3FC794E90D717FE628202800020000800260BF83E
              FC0DE3A7E7B4F5B4F8DEFB19133D84C8A7B15897CEE2E83434D3B57F0EF0D6D6
              8EC1788FF0B048126625132A57322A8EF265C84AD3BB5A5C829B6D297B583277
              B9F4007FF1076DB7488E49277E661216BB99DE612B6ED18D5AA16666E81C1ACD
              062CDD5DECCB38C8B2792BA403F83FFBCDE612E2748B88D72653F7D9805370E2
              F68E1112128CD7F7B844170B557A9E9A6AB0D9BE53BCE5025AD56C69000D1603
              1D7DCF58B36033CD5FEB191A1510048182B413E3E3A75E14322CD891C9A79236
              2B831BB5A5E42EDA455EDA6E690057EA8A99179D884B364A67DF3BDFB9BB7138
              EC9C595B323E9E7F7727CA30B9AF21C788F7F586CB21F0D26CA27CEF6D690027
              2B0EB122690366879181A1018E2D3BFDCB7987ABF2508586A1D72CA5BCFA0695
              479E4803287A90CF527D266FECAF18F68C509472FEB700A54245923695D2CA72
              6A8E364A03385B55803A720662B0978FF60F38DD2308363B655BEFFC2CEC8F2C
              48C67C4D22CE4127CDA676EEE43F94066030DEE7FEEB5B6C4AD94E634F35231E
              370A3188B359D77E02448F17856C2AABE372B8F4A8841D29BBD9BFEA803480BE
              211BE7AA8E13AC9C426AEC2A1ABBABC61BCEE3BB05BD1EFF0C2F72D914326373
              30B456D069EDA56CCF7574E151D200FC79D6DDC4D5BA8B68346A3212B2E819EC
              C23AD0855714D145C4A053CDA5B2F521ED964E8A724F9195B86E22CB4EEE2A6E
              79DF4059FD65FA5C363217AE44AB9EEDDF3C3DFD1F30BCAC45C1348E66174EB8
              F8A401FEF40E7EA2D6548DA1E33136876DFC2854A1E164EB37B25E9F435444F4
              6496FB077FC70140001000FC77801F357798B049D14A490000000049454E44AE
              426082}
          end>
      end
      item
        Name = 'Item2'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F4000002124944415478DAED94CF4B545114C7BFF739E38CD6546004A9913A0F
              4AFA034454282A23C11622D408B9291719884B91DAB56DABB670930643D15092
              081A548311BA535A9AE350819534CFF9C59BF7EB76DE7B930A81B8B8E1E65E38
              9C7BDF3B9CEFE79C73DF639C160E7131092001248004900007014827FBA0B010
              1853F63CE5B0AD3CEA5B27C094CA7F1307AA28BE420CC0DA6C3BA25D494AC85C
              5D4F9C0EB0F42DFC5C7904CE76632B2A8FD1AB10AA4FB6205277550CC0973797
              D0747D0EC6E6186C83AA525C9108C2B557681F241E378543DEF1E0987214E977
              B771F6725C1040E2221ABB67A07F7B4A1A1C4EE937CCC20FD8349260F834699A
              A46F108745D11622E747909EEF45F44652D0085EB6A2A1EB15F2EB2FC08D3CCC
              EC2F449A6F225CD3ECA6F047E27580FB1D089CC05AA2036ACFA22080781BD45B
              8B5E957F85A80F74FC0E4B5B00B7B7C12DD7B2B4CF235CFF00A9993B50631F05
              014CB521DAF7164E71950434B21C896970CC8C274E143E84ED021450D5F018A9
              C420D47E510093ED68EA9F839D7BEF57EA55AC95F79972E5D91D806A75121BF1
              21A803A200C6A903776761665E9785B5DDB67B3EB723CE1D1D47CE3DC3C6F430
              D47B8200D69F5CC399D8048CAD25BA0245BA023A7932F2DC2E9177BF80927F47
              B88D505D0C5F9F3F847AFF831880CCF214F4CDCF28A43ED1DC8BFB07BBFFA840
              10C72F74E354E7A81880FFB9248004900012E0D001FE00B52A87B0BB8D4AB800
              00000049454E44AE426082}
          end>
      end
      item
        Name = 'Item3'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F4000002A84944415478DAED966D48535118C7FF7B7F71B35CDB72AE3295C040
              D124A20C32D0F549C922656183FA12F4860C1323EA431491248821157D9168D1
              CA0843216825296411619A4256A49996CE2D8773DEB9DD7B676797B8B53E4DB8
              11C17DBE3CF03FCF79CEEF9EE73EE71CC91231FC4393880022800820022C17C0
              3FC7A2F74D04C39F6878032CA7AD4E93212F47811D9B5430AE90FD3D809EFE10
              3A7A82D8BC518B6DF929C830C939FD9B8FC18BA105BC7E47A1B2241525453AE1
              013C2F67D1EEF1A1E1D03AACB728F1E734894482CF535134B67D4195CD04DB56
              83700053FE45D45E1AC285E3B9C8FC6DF1B2C3AF38FFE4C6161E629C409C691D
              41CBA97C588C6A61005A6E8D204AB33866CF422C16E3F5E203CF38DFE7DAC96B
              52A914AD77C6A052CA50EBC81506A0E28807E74E1422C7AAE4359AA63918968D
              C1E7F311CF70BA4C264780D2A2E9E6283AAFD98401C82B7F885E5719098E8261
              18CCCD05415161ECA9FBC08D3F68DA9010AF52EBB1AF6E10C35DBB850128ACF4
              A0BBAD188BE179B2F83C5F86BD27C77E026425C44BE51AEC6F788F810E8176A0
              DA3988BA83193068C3097F7F55C357CEB7375A13E2277D125CBF1FC0BDE60261
              00AEBA67B04031B0DB14A4D62CAFDB4F0738EFBE98F62B21E904D7A308D256AA
              71D46E160660DACFC079F93BEA1D294837B0FC2ED49CA5387FFBBC968F9DF092
              AEB91B4173FD2AA41BE5C200C4ED715F14AE2E0ACE1A35AC26203E4BA7537063
              A1104DBE1C189F5EC21577048E722D76152B9349BBBCA3B8F3E904DABB535194
              ABC0F60205AC66292189D79CC5F3011AFD230CAA4B83A8285D9B6CCAE401E2ED
              E7F57AF171D48BBEB7728CCEACC1F4AC941B4B37C4906D9E24F7038DEC4C232C
              160B944A817720DE7AA15088F43F95701A26242375D06834D0EBF5E4404AEE56
              FCFFDE032280082002086D3F00553B76B028749A750000000049454E44AE4260
              82}
          end>
      end
      item
        Name = 'Item4'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F4000002994944415478DAEDD67B4853511C07F0EF5E779BC574DE4D6C3A750C
              15AB3D748595A10542D00BC31E0609660C0912022390E8AF8820EB9F305B0FF2
              8F048929884992266A1142120913E70CD9DC12C15C5666CB2DB5E3DDB22EFD13
              F38004FBC1FDE377388F0FE777CEBD57B04C02EB18821820068801FE1B80DFD1
              8885CF9E7F9A541A9F01D6709A2E60F2C56524A46CC4FCFBD195418853E9204F
              488550C4F03B8AC598748E20A5F00A5D80AFEF12B479162C2F2D2138378DF929
              17663C63C8B494F02794CBE11B7341BBE72A5DC0446F2DB4E65C2C8742E1810C
              83E1761B32CC87112767201408C2ED3219BCEF5C48DF7B8D2EC0FDFC22D2FE04
              482470B43780CD3EC0E59AA4F855C084CB095DF175BA80F16735482780A55090
              CB851206AF5B6E21D5182EC1A608402895C24300FA7D37E902C63ACF23C364C2
              62300C1091120CD8EB09EAC85F00B76B04C9A6722834167A00E79373D099CC04
              B0100148F1AAE53674B97C808800C4FA2AB83AAA9173A89E1E60B8ED2CF46407
              7E44006202E8B73720735B291F4076E65B6A0526BB6AB0B5E40E3DC050AB1599
              46230FD06BBF8BECFCA361805AB10A985295E14B7F2DCCA5F7E901DE3CAE4496
              D18050042021809E967BC8D9718C071013C0C1AFD5A8739F81E5C4437A808147
              A7B039D74400DF230019BAED0F602838CEE5C9BF00E47AEEF259619BA9C2CEF2
              A6B503028100F70C3657207F770182C100D7CE1040576B23F28A4EF200429108
              2FFB7A909C674596B91872F2665C13607676167EBF1F834F6F402319459ADE00
              456212B75093AD0E4CE216AE9F52F1FB9BB0B8210BDBF75F00CBB2502A95744A
              B0129EA136781D9D585CF808B59A85C3E144516533D42A963B13D14454FF031F
              BC43187FDB81B94FD3282CAB83541ADDE2510368460C1003C400EB0EF809A1C2
              47B0CAD7E8510000000049454E44AE426082}
          end>
      end>
    Left = 224
    Top = 72
  end
  object VirtualImageList1: TVirtualImageList
    AutoFill = True
    DisabledGrayscale = False
    DisabledSuffix = '_Disabled'
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'Item1'
        Disabled = False
        Name = 'Item1'
      end
      item
        CollectionIndex = 1
        CollectionName = 'Item2'
        Disabled = False
        Name = 'Item2'
      end
      item
        CollectionIndex = 2
        CollectionName = 'Item3'
        Disabled = False
        Name = 'Item3'
      end
      item
        CollectionIndex = 3
        CollectionName = 'Item4'
        Disabled = False
        Name = 'Item4'
      end>
    ImageCollection = ImageCollection1
    Width = 32
    Height = 32
    Left = 224
    Top = 144
  end
end
