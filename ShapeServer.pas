unit ShapeServer;

interface

uses
  {$IFDEF logger}
  CodeSiteLogging,
  {$ENDIF}
  Classes, SysUtils, shapeCodec, shapeElement, cFileUtils,
  shapeConsts, shapeIntf, shapeTypes, shapeServer_Intf
  {$IFDEF ICS}
  ,Winsock, OverbyteIcsWSocket
  {$ELSE}
  ,synsock, synautil, synacode, BlckSock
  {$ENDIF}
  ;


type

  TShapeServerThread = class;
  
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


  TShapeServerWrap = class(TComponent, IShapeServer)
  private
    fShapeServerThread: TShapeServerThread;
    FOnMessageReceived: TMessageReceivedEvent;
    FShapeServerEvent: IShapeServerEvent;

    procedure ServerOnMessageReceived(ASender: TObject; ASenderAddr: TInAddr; AMessage: IShapeMessage);

  public
    procedure Open; stdcall;
    procedure Close; stdcall;

    procedure SetShapeServerEvent(const Value: IShapeServerEvent); stdcall;
    function GetShapeServerEvent: IShapeServerEvent; stdcall;

    function GetActive: Boolean; stdcall;
    function GetAddress: String; stdcall;
    function GetMessageReceived: UInt64; stdcall;
    function GetPort: String;stdcall;
    procedure SetAddress(const Value: String);stdcall;
    procedure SetMessageReceived(const Value: UInt64); stdcall;
    procedure SetPort(const Value: String);stdcall;

    function GetOnMessageReceived: TMessageReceivedEvent;
    procedure SetOnMessageReceived(const Value: TMessageReceivedEvent);
    
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property ShapeServerEvent: IShapeServerEvent read GetShapeServerEvent write SetShapeServerEvent;
    property Address: String read GetAddress write SetAddress;
    property Port: String read GetPort write SetPort;
    property Active: Boolean read GetActive;
    property MessageReceived: UInt64 read GetMessageReceived write SetMessageReceived;
    property OnMessageReceived: TMessageReceivedEvent read GetOnMessageReceived write SetOnMessageReceived;

  end;

  TShapeServerThread = class({$IFDEF ICS}TObject{$ELSE}TThread{$ENDIF})
  private
    FPort: String;
    FAddress: String;
    FFormats: TShapeFormatsDir;
    FActive: Boolean;
    {$IFDEF ICS}
    FUDPServer: TWSocket;
    {$ELSE}
    FUDPServer: TUDPBlockSocket;
    {$ENDIF}
//    FTCPServer: TTCPBlockSocket;
    FSenderAddr: TInAddr;
    fMessageReceived: UInt64;

    FOnMessageReceived: TMessageReceivedEvent;
    FOnBeforeClose: TNotifyEvent;
    FOnAfterBind: TNotifyEvent;

    CurrMessage: IShapeMessage;
    CurrSenderAddr: TInAddr;

    procedure SetAddress(const Value: String);
    procedure SetPort(const Value: String);

    {$IFDEF ICS}
    procedure WSocketDataAvailable(Sender: TObject; ErrCode: Word);
    procedure WSocketSessionConnected(Sender: TObject; ErrCode: Word);
    procedure WSocketSessionClosed(Sender: TObject; ErrCode: Word);
    {$ENDIF}
    
    procedure DoMessageReceived(ASenderAddr: TInAddr; AMessage: IShapeMessage);
    procedure SetOnMessageReceived(const Value: TMessageReceivedEvent);
    procedure SetMessageReceived(const Value: UInt64);
    procedure SetOnAfterBind(const Value: TNotifyEvent);
    procedure SetOnBeforeClose(const Value: TNotifyEvent);
    procedure SyncDoMessageReceived;
  protected
    {$IFNDEF ICS} procedure Execute; override; {$ENDIF}
     
  public
    constructor Create;
    destructor Destroy; override;

    procedure Open;
    procedure Close;

    property Address: String read FAddress write SetAddress;
    property Port: String read FPort write SetPort;
    property Active: Boolean read FActive;
    property MessageReceived: UInt64 read FMessageReceived write SetMessageReceived;
    property OnMessageReceived: TMessageReceivedEvent read FOnMessageReceived write SetOnMessageReceived;
    property OnAfterBind: TNotifyEvent read FOnAfterBind write SetOnAfterBind;
    property OnBeforeClose: TNotifyEvent read FOnBeforeClose write SetOnBeforeClose;
  end;

  function newShapeServer: IShapeServer;

implementation

const
  INT_BUFFER_SIZE = 1000;
	INT_MIN_PORT = 54000;
  INT_MAX_PORT = 58000;

function newShapeServer: IShapeServer;
begin
  Result := TShapeServerWrap.Create(nil);
end;
  
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
  Add(ChangeFileExt(ExtractFilename(AShapeFile), ''), shape);
end;

procedure TShapeFormatsDir.AddDir(ADir: String);
var SRec : TSearchRec;
    Path : String;
    FileMask: String;
    Res: Boolean;
begin
  FileMask := PathInclSuffix(ADir, PathSeperator) + '*.shape';
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
          AddShapeFile(PathInclSuffix(ADir, PathSeperator) + SRec.Name);
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
  AddDir(DirectoryExpand(FDirectory, ExtractFilePath(ParamStr(0)), PathSeperator));
end;

procedure TShapeFormatsDir.SetDirectory(const Value: String);
begin
  FDirectory := Value;
end;

{ TShapeServerThread }

procedure TShapeServerThread.Close;
begin
	if Assigned(FOnBeforeClose) then FOnBeforeClose(Self);
  {$IFDEF ICS}
  FUDPServer.Close;  
  {$ELSE}
  FUDPServer.CloseSocket;
  {$ENDIF}
end;

constructor TShapeServerThread.Create;
begin
  {$IFDEF ICS}
  inherited Create;
  fMessageReceived := 0;
  FUDPServer := TWSocket.Create(nil);
  FUDPServer.OnDataAvailable := WSocketDataAvailable;
  FUDPServer.OnSessionClosed := WSocketSessionClosed;
  FUDPServer.OnSessionConnected := WSocketSessionConnected;
  FUDPServer.Proto := 'udp';
  {$ELSE}
  inherited Create(True);
  fMessageReceived := 0;
  FUDPServer := TUDPBlockSocket.Create;
  // 4MB Buffer
  FUDPServer.SizeRecvBuffer := 4096 * 1024;
  {$ENDIF}

//  FTCPServer := TTCPBlockSocket.Create;
  CurrMessage := TShapeMessage.Create;
  FAddress := '127.0.0.1';
  FPort := '54493'; // SHAPE
  FFormats := TShapeFormatsDir.Create('formats');

end;

destructor TShapeServerThread.Destroy;
begin
//  FTCPServer.Free;
  FUDPServer.Free;
  FFormats.Free;
  inherited;
end;

procedure TShapeServerThread.SyncDoMessageReceived;
begin
  {$IFDEF logger}
  CodeSite.EnterMethod('SyncDoMessageReceived');
  {$ENDIF}
  Inc(fMessageReceived);
  CurrMessage.AddInternal(SHAPE_SENDER_IP, CurrSenderAddr.S_addr);
  if Assigned(FOnMessageReceived) then
  begin
    {$IFDEF logger}
    CodeSite.Send('FOnMessageReceived Send Start');
    {$ENDIF}
    FOnMessageReceived(Self, CurrSenderAddr, CurrMessage);
    {$IFDEF logger}
    CodeSite.Send('FOnMessageReceived Send End');
    {$ENDIF}
    
  end;
  {$IFDEF logger}
  CodeSite.ExitMethod('SyncDoMessageReceived');
  {$ENDIF}
end;

procedure TShapeServerThread.DoMessageReceived(ASenderAddr: TInAddr; AMessage: IShapeMessage);
begin
  Inc(fMessageReceived);
  AMessage.AddInternal(SHAPE_SENDER_IP, ASenderAddr.S_addr);
  if Assigned(FOnMessageReceived) then
    FOnMessageReceived(Self, ASenderAddr, AMessage);
end;

{$IFNDEF ICS}
procedure TShapeServerThread.Execute;
var
  sz: Integer;
  mem: TMemoryStream;

  procedure ReceiveUDP;
  begin
    sz := FUDPServer.WaitingData;
    if sz > 0 then
    begin
    	try
        {$IFDEF logger}
        CodeSite.Send('UDPServer Read: %d', [sz]);
        {$ENDIF}
        Mem.SetSize(Sz);
        mem.Seek(0, 0);
        {$IFDEF logger}
        CodeSite.Send('RecvBuffer');
        {$ENDIF}      
        FUDPServer.RecvBuffer(mem.Memory, sz);
        mem.Seek(0, 0);
        {$IFDEF logger}
        CodeSite.Send('fFormats.ReadFromStream(mem)');
        {$ENDIF}
        CurrMessage.Clear;
        CurrMessage.ReadFromStream(mem);
        CurrSenderAddr := FUDPServer.RemoteSin.sin_addr;
        {$IFDEF logger}
        CodeSite.Send('Synchronize(SyncDoMessageReceived)');
        {$ENDIF}
        Synchronize(SyncDoMessageReceived);
      except
      	On E: Exception do
        {$IFDEF logger}
        CodeSite.SendError(E.Message);
        {$ENDIF}
      end;
    end;
  end;

//  procedure ReceiveTCP;
//  var
//    AMessage: IShapeMessage;
//  begin
//    sz := FTCPServer.WaitingData;
//    if sz > 0 then
//    begin
//      FTCPServer.RecvBuffer(mem.Memory, sz);
//      mem.Seek(0, 0);
//      AMessage := fFormats.ReadFromStream(mem);
//      DoMessageReceived(FTCPServer.RemoteSin.sin_addr, AMessage);
//    end;
//  end;

  procedure ReceiveData;
  var
  	printed: Boolean;
  begin
    {$IFDEF logger}
    CodeSite.EnterMethod('ReceiveData');
    {$ENDIF}

  	printed := False;
    while not Terminated do
    begin
      try


        ReceiveUDP;
//        ReceiveTCP;
        Sleep(10);
      finally

      end;
    end;

    {$IFDEF logger}
    CodeSite.ExitMethod('ReceiveData');
    {$ENDIF}
    
  end;

begin
  inherited;
  
  mem := TMemoryStream.Create;
  try
    mem.SetSize(INT_BUFFER_SIZE);
    ReceiveData;
  finally
    mem.Free;
  end;
end;
{$ENDIF}

procedure TShapeServerThread.Open;
var
	i: Integer;
begin
  {$IFDEF ICS}
  FUDPServer.Addr := FAddress;
  FUDPServer.Port := FPort;
  FUDPServer.Listen;
  {$ELSE}
  if (FPort='0') or (FPort='') then
    for I := INT_MIN_PORT to INT_MAX_PORT do
    begin
      try
        FUDPServer.Bind(FAddress, IntToStr(I));
        FPort := IntToStr(i);
        Break;
      except

      end;
    end
  else
  begin
		FUDPServer.Bind(FAddress, FPort);  
  end;
	if Assigned(FOnAfterBind) then FOnAfterBind(Self);
  Resume;
//  FUDPServer.Listen;
  {$ENDIF}
//  FTCPServer.Bind(FAddress, IntToStr(StrToInt(FPort)+1));
//  FTCPServer.CreateSocket;
//  FTCPServer.setLinger(true,10000);
//  FTCPServer.Listen;
end;

procedure TShapeServerThread.SetAddress(const Value: String);
begin
  FAddress := Value;
end;

procedure TShapeServerThread.SetMessageReceived(const Value: UInt64);
begin
  FMessageReceived := Value;
end;

procedure TShapeServerThread.SetOnAfterBind(const Value: TNotifyEvent);
begin
  FOnAfterBind := Value;
end;

procedure TShapeServerThread.SetOnBeforeClose(const Value: TNotifyEvent);
begin
  FOnBeforeClose := Value;
end;

procedure TShapeServerThread.SetOnMessageReceived(const Value: TMessageReceivedEvent);
begin
  FOnMessageReceived := Value;
end;

procedure TShapeServerThread.SetPort(const Value: String);
begin
  FPort := Value;
end;

{$IFDEF ICS}
procedure TShapeServerThread.WSocketDataAvailable(Sender: TObject; ErrCode: Word);
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
  Inc(fMessageReceived);  
  try
    Len    := FUDPServer.ReceiveFrom(stream.Memory, stream.size, Src, SrcLen);
    if (FSenderAddr.S_addr = INADDR_ANY) or
       (FSenderAddr.S_addr = Src.Sin_addr.S_addr) then
    begin

      while stream.Position < Len do
      begin
        try
          AMessage := fFormats.ReadFromStream(stream);
          
          DoMessageReceived(Src.Sin_addr, AMessage);
        except
          break;
        end;
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

procedure TShapeServerThread.WSocketSessionClosed(Sender: TObject; ErrCode: Word);
begin
  fActive := False;
end;

procedure TShapeServerThread.WSocketSessionConnected(Sender: TObject; ErrCode: Word);
begin
  fActive := True;
end;

{$ENDIF}

{ TShapeServerWrap }

procedure TShapeServerWrap.AfterConstruction;
begin
  inherited;
  fShapeServerThread := TShapeServerThread.Create;
  fShapeServerThread.OnMessageReceived := ServerOnMessageReceived;
  fShapeServerThread.Resume;
end;

procedure TShapeServerWrap.BeforeDestruction;
begin
  inherited;
  FShapeServerThread.Terminate;
  FShapeServerThread.Free;
end;

procedure TShapeServerWrap.Close;
begin
//	DeleteFile(Pchar(AppPath('liveupdsrv.dat')));
  FShapeServerThread.Close;
end;

function TShapeServerWrap.GetActive: Boolean;
begin
  Result := FShapeServerThread.Active;
end;

function TShapeServerWrap.GetAddress: String;
begin
  Result := FShapeServerThread.Address;
end;

function TShapeServerWrap.GetMessageReceived: UInt64;
begin
  Result := FShapeServerThread.MessageReceived;
end;

function TShapeServerWrap.GetOnMessageReceived: TMessageReceivedEvent;
begin
  Result := FOnMessageReceived;
end;

function TShapeServerWrap.GetPort: String;
begin
  Result := FShapeServerThread.Port;
end;

function TShapeServerWrap.GetShapeServerEvent: IShapeServerEvent;
begin
  Result := fShapeServerEvent;
end;

procedure TShapeServerWrap.Open;
begin
  FShapeServerThread.Open;
end;

procedure TShapeServerWrap.ServerOnMessageReceived(ASender: TObject;
  ASenderAddr: TInAddr; AMessage: IShapeMessage);
var
  p: PAnsiChar;
begin
  if Assigned(FShapeServerEvent) then
  begin
    p := synsock.inet_ntoa(ASenderAddr);
    FShapeServerEvent.OnMessageReceived(p, '', AMessage);
  end;
end;

procedure TShapeServerWrap.SetAddress(const Value: String);
begin
  FShapeServerThread.Address := Value;
end;

procedure TShapeServerWrap.SetMessageReceived(const Value: UInt64);
begin
  FShapeServerThread.MessageReceived := Value;
end;

procedure TShapeServerWrap.SetOnMessageReceived(
  const Value: TMessageReceivedEvent);
begin
  FShapeServerThread.OnMessageReceived := Value;
end;

procedure TShapeServerWrap.SetPort(const Value: String);
begin
  FShapeServerThread.Port := Value;
end;

procedure TShapeServerWrap.SetShapeServerEvent(const Value: IShapeServerEvent);
begin
  FShapeServerEvent := Value;
end;

end.
