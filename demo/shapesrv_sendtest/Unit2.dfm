object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Shape Server Send'
  ClientHeight = 141
  ClientWidth = 492
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 104
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object Button1: TButton
    Left = 8
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Send 10'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 97
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Send 100'
    TabOrder = 1
    OnClick = Button2Click
  end
  object txtPort: TLabeledEdit
    Left = 194
    Top = 16
    Width = 47
    Height = 21
    EditLabel.Width = 20
    EditLabel.Height = 13
    EditLabel.Caption = 'Port'
    LabelPosition = lpLeft
    TabOrder = 2
    Text = '54493'
  end
  object txtServer: TLabeledEdit
    Left = 50
    Top = 16
    Width = 103
    Height = 21
    EditLabel.Width = 32
    EditLabel.Height = 13
    EditLabel.Caption = 'Server'
    LabelPosition = lpLeft
    TabOrder = 3
    Text = '127.0.0.1'
  end
  object Button3: TButton
    Left = 178
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Send 10K'
    TabOrder = 4
    OnClick = Button3Click
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
    Left = 336
    Top = 72
  end
  object Timer1: TTimer
    Interval = 500
    OnTimer = Timer1Timer
    Left = 336
    Top = 104
  end
end
