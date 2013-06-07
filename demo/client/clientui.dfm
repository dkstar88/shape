object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 379
  ClientWidth = 548
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 323
    Width = 98
    Height = 13
    Caption = 'Total Message Size: '
  end
  object txtName: TLabeledEdit
    Left = 8
    Top = 32
    Width = 121
    Height = 21
    EditLabel.Width = 31
    EditLabel.Height = 13
    EditLabel.Caption = 'Name:'
    TabOrder = 0
  end
  object txtDob: TLabeledEdit
    Left = 280
    Top = 32
    Width = 121
    Height = 21
    EditLabel.Width = 115
    EditLabel.Height = 13
    EditLabel.Caption = 'DOB (e.g. 1980-01-01):'
    TabOrder = 1
  end
  object txtAge: TLabeledEdit
    Left = 144
    Top = 32
    Width = 121
    Height = 21
    EditLabel.Width = 23
    EditLabel.Height = 13
    EditLabel.Caption = 'Age:'
    TabOrder = 2
  end
  object txtHeight: TLabeledEdit
    Left = 416
    Top = 32
    Width = 121
    Height = 21
    EditLabel.Width = 91
    EditLabel.Height = 13
    EditLabel.Caption = 'Height (e.g. 1.78):'
    TabOrder = 3
  end
  object btnSend: TButton
    Left = 465
    Top = 158
    Width = 75
    Height = 25
    Caption = 'Send'
    TabOrder = 4
    OnClick = btnSendClick
  end
  object txtPort: TLabeledEdit
    Left = 402
    Top = 160
    Width = 47
    Height = 21
    EditLabel.Width = 20
    EditLabel.Height = 13
    EditLabel.Caption = 'Port'
    LabelPosition = lpLeft
    TabOrder = 5
    Text = '48585'
  end
  object txtServer: TLabeledEdit
    Left = 258
    Top = 160
    Width = 103
    Height = 21
    EditLabel.Width = 32
    EditLabel.Height = 13
    EditLabel.Caption = 'Server'
    LabelPosition = lpLeft
    TabOrder = 6
    Text = '127.0.0.1'
  end
  object hexViewer: TMPHexEditor
    Left = 8
    Top = 197
    Width = 529
    Height = 120
    Cursor = crIBeam
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
    BytesPerRow = 16
    Translation = tkAsIs
    OffsetFormat = '-!10:0x|'
    Colors.Background = clWindow
    Colors.ChangedBackground = 11075583
    Colors.ChangedText = clMaroon
    Colors.CursorFrame = clNavy
    Colors.Offset = clBlack
    Colors.OddColumn = clBlue
    Colors.EvenColumn = clNavy
    Colors.CurrentOffsetBackground = clBtnShadow
    Colors.OffsetBackground = clBtnFace
    Colors.CurrentOffset = clBtnHighlight
    Colors.Grid = clBtnFace
    Colors.NonFocusCursorFrame = clAqua
    Colors.ActiveFieldBackground = clWindow
    FocusFrame = True
    DrawGridLines = False
    ReadOnlyView = True
    Version = 'september 30, 2007; ?markus stephany, vcl[at]mirkes[dot]de'
  end
  object chkCompressed: TCheckBox
    Left = 8
    Top = 74
    Width = 97
    Height = 17
    Caption = 'Compressed'
    TabOrder = 8
  end
  object chkNamedKey: TCheckBox
    Left = 144
    Top = 74
    Width = 97
    Height = 17
    Caption = 'Named Key'
    TabOrder = 9
  end
  object WSocket1: TWSocket
    LineMode = False
    LineLimit = 65536
    LineEnd = #13#10
    LineEcho = False
    LineEdit = False
    Port = '48585'
    Proto = 'udp'
    LocalAddr = '0.0.0.0'
    LocalPort = '0'
    MultiThreaded = False
    MultiCast = False
    MultiCastIpTTL = 1
    FlushTimeout = 60
    SendFlags = wsSendNormal
    LingerOnOff = wsLingerOn
    LingerTimeout = 0
    KeepAliveOnOff = wsKeepAliveOff
    KeepAliveTime = 0
    KeepAliveInterval = 0
    SocksLevel = '5'
    SocksAuthentication = socksNoAuthentication
    LastError = 0
    ReuseAddr = False
    ComponentOptions = []
    ListenBacklog = 5
    ReqVerLow = 1
    ReqVerHigh = 1
    OnDataSent = WSocket1DataSent
    Left = 384
    Top = 136
  end
end
