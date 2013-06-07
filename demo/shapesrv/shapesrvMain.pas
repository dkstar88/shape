unit shapesrvMain;

interface

uses
  {$IFDEF logger}
  CodeSiteLogging,
  {$ENDIF}
  Classes, WinSock, SysUtils, shapeCodec, shapeElement, cFileUtils,
  shapeConsts, shapeIntf, shapeTypes, OverbyteIcsWSocket;


type

  TShapeFormatsDir = class(TShapeFormats)
  private
    FDirectory: String;
    procedure SetDirectory(const Value: String);
    procedure AddDir(ADir: String);
    procedure AddShapeFile(AShapeFile: String);
  public
    constructor Create(ADirectoryBase: String);
    destructor Destroy; override;

    procedure Reload;

    property Directory: String read FDirectory write SetDirectory;
  end;

  TMessageReceivedEvent = procedure (ASender: TObject; ASenderAddr: TInAddr;
    AMessage: IShapeMessage) of object;

  TShapeServer = class(TObject)
  private
    FPort: String;
    FAddress: String;
    FFormats: TShapeFormatsDir;
    FActive: Boolean;
    FUDPServer: TWSocket;
    FSenderAddr: TInAddr;
    FOnMessageReceived: TMessageReceivedEvent;
    procedure SetAddress(const Value: String);
    procedure SetPort(const Value: String);

    procedure WSocketDataAvailable(Sender: TObject; ErrCode: Word);
    procedure WSocketSessionConnected(Sender: TObject; ErrCode: Word);
    procedure WSocketSessionClosed(Sender: TObject; ErrCode: Word);
    procedure DoMessageReceived(ASenderAddr: TInAddr; AMessage: IShapeMessage);
    procedure SetOnMessageReceived(const Value: TMessageReceivedEvent);
    
  public
    constructor Create;
    destructor Destroy; override;

    procedure Open;
    procedure Close;

    property Address: String read FAddress write SetAddress;
    property Port: String read FPort write SetPort;
    property Active: Boolean read FActive;
    property OnMessageReceived: TMessageReceivedEvent read FOnMessageReceived write SetOnMessageReceived;
  end;

implementation

const
  INT_BUFFER_SIZE = 2048;

{ TShapeFormatsDir }

constructor TShapeFormatsDir.Create(ADirectoryBase: String);
begin
  inherited Create;
  fDirectory := ADirectoryBase;
  Reload;
end;

destructor TShapeFormatsDir.Destroy;
begin

  inherited;
end;

procedure TShapeFormatsDir.AddShapeFile(AShapeFile: String);
var
  shape: IShapeMessage;
begin
  shape := TShapeMessage.CreateFromSpec(AShapeFile);
  Add(ExtractFilename(AShapeFile), shape);
end;

procedure TShapeFormatsDir.AddDir(ADir: String);
var SRec : TSearchRec;
    Path : String;
    FileMask: String;
    Res: Boolean;
begin
  FileMask := ADir + PathSeperator + '*.shape';
  Res := FindFirst(FileMask, faAnyFile, SRec) = 0;
  if not Res then
    exit;
  try
    Path := ExtractFilePath(FileMask);
    Repeat
      if (SRec.Name <> '') and (SRec.Name  <> '.') and (SRec.Name <> '..') then
      begin
        // Is Directory
        if ((SRec.Attr and faDirectory) = faDirectory) then
          AddDir(ADir + PathSeperator + SRec.Name)
        else
          AddShapeFile(SRec.Name);
//        if not Result then
//          break;
      end;
    Until FindNext(SRec) <> 0;
  finally
    FindClose(SRec);
  end;
end;

procedure TShapeFormatsDir.Reload;
begin
  Clear;
  AddDir(FilePath('*.shape', FDirectory, ExtractFilePath(ParamStr(0)), PathSeperator));
end;

procedure TShapeFormatsDir.SetDirectory(const Value: String);
begin
  FDirectory := Value;
end;

{ TShapeServer }

procedure TShapeServer.Close;
begin
  FUDPServer.Close;
end;

constructor TShapeServer.Create;
begin
  inherited Create;
  FUDPServer := TWSocket.Create(nil);
  FUDPServer.OnDataAvailable := WSocketDataAvailable;
  FUDPServer.OnSessionClosed := WSocketSessionClosed;
  FUDPServer.OnSessionConnected := WSocketSessionConnected;
  FUDPServer.Proto := 'udp';
  FAddress := '127.0.0.1';
  FPort := '54493'; // SHAPE
  FFormats := TShapeFormatsDir.Create('formats');
end;

destructor TShapeServer.Destroy;
begin
  FUDPServer.Free;
  FFormats.Free;
  inherited;
end;

procedure TShapeServer.DoMessageReceived(ASenderAddr: TInAddr; AMessage: IShapeMessage);
begin
  if Assigned(FOnMessageReceived) then
    FOnMessageReceived(Self, ASenderAddr, AMessage);
end;

procedure TShapeServer.Open;
begin
  FUDPServer.Addr := FAddress;
  FUDPServer.Port := FPort;
  FUDPServer.Listen;
end;

procedure TShapeServer.SetAddress(const Value: String);
begin
  FAddress := Value;
end;

procedure TShapeServer.SetOnMessageReceived(const Value: TMessageReceivedEvent);
begin
  FOnMessageReceived := Value;
end;

procedure TShapeServer.SetPort(const Value: String);
begin
  FPort := Value;
end;

procedure TShapeServer.WSocketDataAvailable(Sender: TObject; ErrCode: Word);
var
  AMessage: IShapeMessage;
  Len    : Integer;
  Src    : TSockAddrIn;
  SrcLen : Integer;
  stream: TMemoryStream;
begin
  SrcLen := SizeOf(Src);
  stream := TMemoryStream.Create;
  stream.SetSize(INT_BUFFER_SIZE);
  try
    Len    := FUDPServer.ReceiveFrom(stream.Memory, stream.size, Src, SrcLen);
    if (FSenderAddr.S_addr = INADDR_ANY) or
       (FSenderAddr.S_addr = Src.Sin_addr.S_addr) then
    begin

      if Len > 0 then
      begin
        AMessage := fFormats.ReadFromStream(stream);
        DoMessageReceived(FSenderAddr, AMessage);
      end;

//      Memo1.Lines.Append(String(StrPas(inet_ntoa(Src.sin_addr))) +
//                                    ':'  + IntToStr(ntohs(Src.sin_port)));
//
//      if Len >= 0 then
//        fUDPMan.Receive(Src, @Buffer, Len, True)
//      else
//        Memo1.Lines.Append(Format('Winsock Error: %d', [WSocket.LastError]));

    end;

  finally
    stream.Free;
  end;
end;

procedure TShapeServer.WSocketSessionClosed(Sender: TObject; ErrCode: Word);
begin
  fActive := False;
end;

procedure TShapeServer.WSocketSessionConnected(Sender: TObject; ErrCode: Word);
begin
  fActive := True;
end;

end.
