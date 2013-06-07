object frmShapeSrv: TfrmShapeSrv
  Left = 0
  Top = 0
  Caption = 'Shape Message Server'
  ClientHeight = 420
  ClientWidth = 554
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 400
    Width = 98
    Height = 13
    Caption = 'Total Message Size: '
  end
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 538
    Height = 225
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
  object btnListen: TButton
    Left = 8
    Top = 243
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 1
    OnClick = btnListenClick
  end
  object btnStop: TButton
    Left = 89
    Top = 243
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 2
    OnClick = btnStopClick
  end
  object txtPort: TLabeledEdit
    Left = 425
    Top = 239
    Width = 121
    Height = 21
    EditLabel.Width = 20
    EditLabel.Height = 13
    EditLabel.Caption = 'Port'
    LabelPosition = lpLeft
    TabOrder = 3
    Text = '48585'
  end
  object txtSenderIP: TLabeledEdit
    Left = 272
    Top = 239
    Width = 121
    Height = 21
    EditLabel.Width = 27
    EditLabel.Height = 13
    EditLabel.Caption = 'Client'
    LabelPosition = lpLeft
    TabOrder = 4
    Text = '0.0.0.0'
  end
  object hexViewer: TMPHexEditor
    Left = 8
    Top = 274
    Width = 529
    Height = 120
    Cursor = crIBeam
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
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
  object WSocket: TWSocket
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
    OnDataAvailable = WSocketDataAvailable
    OnSessionClosed = WSocketSessionClosed
    OnSessionConnected = WSocketSessionConnected
    Left = 200
    Top = 243
  end
  object IcsLogger1: TIcsLogger
    TimeStampFormatString = 'hh:nn:ss:zzz'
    TimeStampSeparator = ' '
    LogFileOption = lfoOverwrite
    LogFileName = 'socket.log'
    LogOptions = [loDestEvent, loDestFile, loDestOutDebug, loAddStamp, loWsockErr, loWsockInfo, loWsockDump]
    Left = 168
    Top = 243
  end
  object WSocketServer1: TWSocketServer
    LineMode = False
    LineLimit = 65536
    LineEnd = #13#10
    LineEcho = False
    LineEdit = False
    Proto = 'tcp'
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
    Banner = 'Welcome to OverByte ICS TcpSrv'
    BannerTooBusy = 'Sorry, too many clients'
    MaxClients = 0
    Left = 272
    Top = 384
  end
end
