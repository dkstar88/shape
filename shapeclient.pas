unit shapeclient;

interface

uses Windows, Classes, SysUtils, BlckSock, interfaceQueue, shapeCodec, shapeConsts,
  shapeIntf, shapeclient_intf;

type
  TClientQueueData = class(TInterfacedObject, IClientQueueData)
  private
    fMessage: ISHapeMessage;
    fServerAddress: String;
    fServerPort: String;
    fProtocol: String;
  public
    function getMessage: IShapeMessage; stdcall;
    function getProtocol: String; stdcall;
    function getServerAddress: String; stdcall;
    function getServerPort: String; stdcall;
    procedure setProtocol(const Value: String); stdcall;
    procedure setServerAddress(const Value: String); stdcall;
    procedure setServerPort(const Value: String); stdcall;
    procedure setMessage(const Value: IShapeMessage); stdcall;
    
    procedure BeforeDestruction; override;
    procedure AfterConstruction; override;

  public
    property ServerAddress: String read getServerAddress write setServerAddress;
    property ServerPort: String read getServerPort write setServerPort;
    property Message: IShapeMessage read getMessage write setMessage;
    property Protocol: String read getProtocol write setProtocol;
  end;


  TClientSendQueue = class(TInterfacedObject, IClientSendQueue)
  private
    fList: TInterfaceQueue;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
      
  public
    function Count: Integer;  stdcall;
    function AtLeast(ACount: Integer): Boolean; stdcall;
    function Push(AItem: IClientQueueData): IClientQueueData;  stdcall;
    function Pop: IClientQueueData; stdcall;
    function Peek: IClientQueueData; stdcall;
  end;

  TShapeClientThread = class;

  TShapeClientWrap = class(TComponent, IShapeClient)
  private
    fShapeClient: TShapeClientThread;
  public

    procedure SetServerAddress(const Value: String); stdcall;
    procedure SetServerPort(const Value: String); stdcall;
    procedure SetSendCount(const Value: UInt64); stdcall;
    procedure SetMPS(const Value: Integer); stdcall;
    procedure SetProt(const Value: String); stdcall;
    
    function getActive: Boolean; stdcall;
    function getMPS: Integer; stdcall;
    function getProt: String; stdcall;
    function getSendCount: UInt64; stdcall;
    function getSendQueue: IClientSendQueue; stdcall;
    function getServerAddress: String; stdcall;
    function getServerPort: String; stdcall;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  public

		function Send(ADestAddr, APort: String; AMessage: IShapeMessage): Boolean; stdcall;
    procedure Queue(ADestAddr, APort: String; AMessage: IShapeMessage); stdcall;

    property SendQueue: IClientSendQueue read getSendQueue;
    property Active: Boolean read getActive;
    property SendCount: UInt64 read getSendCount write SetSendCount;
    


    property ServerAddress: String read getServerAddress write SetServerAddress;
    property ServerPort: String read getServerPort write SetServerPort;
    property MPS: Integer read getMPS write SetMPS;
    property Prot: String read getProt write SetProt;

  end;

  TShapeClientThread = class(TThread)
  private
    fActive: Boolean;
    FSendQueue: IClientSendQueue;
    FSender: TBlockSocket;
    FServerPort: String;
    FServerAddress: String;
    FSendCount: UInt64;
    FMPS: Integer;
    FProt: String;
    procedure SetSendQueue(const Value: IClientSendQueue);
    procedure SetServerAddress(const Value: String);
    procedure SetServerPort(const Value: String);
    procedure SetSendCount(const Value: UInt64);
    procedure SetMPS(const Value: Integer);
    procedure SetProt(const Value: String);
    function getActive: Boolean;
    function getMPS: Integer;
    function getProt: String;
    function getSendCount: UInt64;
    function getSendQueue: IClientSendQueue;
    function getServerAddress: String;
    function getServerPort: String;
  protected
    procedure SendPeek;
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    property SendQueue: IClientSendQueue read getSendQueue write SetSendQueue;
    property Active: Boolean read getActive;
    property ServerAddress: String read getServerAddress write SetServerAddress;
    property ServerPort: String read getServerPort write SetServerPort;
    property SendCount: UInt64 read getSendCount write SetSendCount;
    property MPS: Integer read getMPS write SetMPS;
    property Prot: String read getProt write SetProt;
  end;

  function NewShapeClient: IShapeClient; stdcall;



implementation

uses Dateutils;

function NewShapeClient: IShapeClient; stdcall;
begin
  Result := TShapeClientWrap.Create(nil);
end;

{ TShapeClientThread }

constructor TShapeClientThread.Create;
begin
  inherited Create(True);
  fSendQueue := TClientSendQueue.Create;
  FSendCount := 0;
  FMPS := 150;
  {$IFDEF TCP_CLIENT}
  fSender := TTCPBlockSocket.Create;
  {$ELSE}
  fSender := TUDPBlockSocket.Create;
  {$ENDIF}
  fSender.SizeSendBuffer := 4096*1024;
end;

destructor TShapeClientThread.Destroy;
begin
  fSender.Free;
//  fSendQueue.Free;
  inherited;
end;

procedure TShapeClientThread.Execute;
var
  fCurrMPS: Integer;
  fStart, fDelay: Int64;
begin
  inherited;
  while not Terminated do
  begin
    try
//      if (ServerAddress <> '') and (ServerPort <> '') then
//      begin

        fStart := GetTickCount;
        fCurrMPS := 0;
        while (fSendQueue.AtLeast(1)) and (GetTickCount-fStart < 1000) and
          ((fCurrMPS < fMPS) or (fMPS=0)) do
        begin
          SendPeek;
          Inc(fCurrMPS);
          Sleep(0);
        end;

        fDelay := (1000 - (GetTickCount-fStart));
        if fDelay > 0 then
          Sleep(fDelay);

//      end else
//      begin
//        Sleep(50);
//      end;
    finally

    end;
  end;
end;

function TShapeClientThread.getActive: Boolean;
begin
  Result := fActive;
end;

function TShapeClientThread.getMPS: Integer;
begin
  Result := fMPS;
end;

function TShapeClientThread.getProt: String;
begin
  Result := fProt;
end;

function TShapeClientThread.getSendCount: UInt64;
begin
  Result := fSendCount;
end;

function TShapeClientThread.getSendQueue: IClientSendQueue;
begin
  Result := fSendQueue;
end;

function TShapeClientThread.getServerAddress: String;
begin
  Result := fServerAddress;
end;

function TShapeClientThread.getServerPort: String;
begin
  Result := fServerPort;
end;

procedure TShapeClientThread.SendPeek;
var
  data: IClientQueueData;
  mem: TMemoryStream;
  szSent: Integer;
begin
  mem := TMemoryStream.Create;
  try
    data := IClientQueueData(fSendQueue.Pop);
    data.Message.AddInternal(SHAPE_TIMESTAMP, DateTimeToUnix(Now));
    data.Message.WriteToStream(mem);
    fSender.Connect(data.ServerAddress, data.ServerPort);
    szSent := fSender.SendBufferTo(mem.Memory, mem.Size);
    if szSent = mem.Size then
    begin
      Inc(FSendCount);
    end;
  finally
    mem.Free;
  end;
end;

procedure TShapeClientThread.SetMPS(const Value: Integer);
begin
  FMPS := Value;
end;

procedure TShapeClientThread.SetProt(const Value: String);
begin
  FProt := Value;
end;

procedure TShapeClientThread.SetSendCount(const Value: UInt64);
begin
  FSendCount := Value;
end;

procedure TShapeClientThread.SetSendQueue(const Value: IClientSendQueue);
begin
  FSendQueue := Value;
end;

procedure TShapeClientThread.SetServerAddress(const Value: String);
begin
  FServerAddress := Value;
end;

procedure TShapeClientThread.SetServerPort(const Value: String);
begin
  FServerPort := Value;
end;

{ TShapeClientWrap }

procedure TShapeClientWrap.AfterConstruction;
begin
  inherited;
  fShapeClient := TShapeClientThread.Create;
  fShapeClient.Resume;
end;

procedure TShapeClientWrap.BeforeDestruction;
begin
  fShapeClient.Terminate;
  fShapeClient.Free;
  inherited;

end;

function TShapeClientWrap.getActive: Boolean;
begin
  Result := fShapeClient.Active;
end;

function TShapeClientWrap.getMPS: Integer;
begin
  Result := fShapeClient.MPS;
end;

function TShapeClientWrap.getProt: String;
begin
  Result := fShapeClient.Prot;
end;

function TShapeClientWrap.getSendCount: UInt64;
begin
  Result := fShapeClient.SendCount;
end;

function TShapeClientWrap.getSendQueue: IClientSendQueue;
begin
  Result := fShapeClient.SendQueue;
end;

function TShapeClientWrap.getServerAddress: String;
begin
  Result := fShapeClient.ServerAddress;
end;

function TShapeClientWrap.getServerPort: String;
begin
  Result := fShapeClient.ServerPort;
end;

function TShapeClientWrap.Send(ADestAddr, APort: String;
  AMessage: IShapeMessage): Boolean;
var
	Client: TUDPBlockSocket;
  mem: TMemoryStream;
  szSent: Integer;  
begin
	Client := TUDPBlockSocket.Create;
  mem := TMemoryStream.Create;
  try
    AMessage.AddInternal(SHAPE_TIMESTAMP, DateTimeToUnix(Now));
    AMessage.WriteToStream(mem);
    Client.Connect(ADestAddr, APort);
    szSent := Client.SendBufferTo(mem.Memory, mem.Size);
    Result := (szSent = mem.Size);
  finally
    Client.Free;
    mem.Free;
  end;
end;

procedure TShapeClientWrap.Queue(ADestAddr, APort: String;
  AMessage: IShapeMessage);
var
  fData: IClientQueueData;
begin
  fData := TClientQueueData.Create;
  fData.ServerAddress := ADestAddr;
  fData.ServerPort := APort;
  fData.Message := AMessage;
  fShapeClient.SendQueue.Push(fData);
//  fShapeClient.SendQueue.Push(AMessage);
end;

procedure TShapeClientWrap.SetMPS(const Value: Integer);
begin
  fShapeClient.MPS := Value;
end;

procedure TShapeClientWrap.SetProt(const Value: String);
begin
  fShapeClient.Prot := Value;
end;

procedure TShapeClientWrap.SetSendCount(const Value: UInt64);
begin
  fShapeClient.SendCount := Value;
end;

procedure TShapeClientWrap.SetServerAddress(const Value: String);
begin
  fShapeClient.ServerAddress := Value;
end;

procedure TShapeClientWrap.SetServerPort(const Value: String);
begin
  fShapeClient.ServerPort := Value;
end;

{ TClientQueueData }

procedure TClientQueueData.AfterConstruction;
begin
  inherited;
  fMessage := TShapeMessage.Create;
end;

procedure TClientQueueData.BeforeDestruction;
begin
//  fMessage
  inherited;
end;

function TClientQueueData.getMessage: IShapeMessage;
begin
  Result := fMessage;
end;

function TClientQueueData.getProtocol: String;
begin
  Result := fProtocol;
end;

function TClientQueueData.getServerAddress: String;
begin
  Result := fServerAddress;
end;

function TClientQueueData.getServerPort: String;
begin
  Result := fServerPort;
end;

procedure TClientQueueData.setMessage(const Value: IShapeMessage);
begin
  fMessage.Assign(Value);
end;

procedure TClientQueueData.setProtocol(const Value: String);
begin
  fProtocol := Value;
end;

procedure TClientQueueData.setServerAddress(const Value: String);
begin
  fServerAddress := Value;
end;

procedure TClientQueueData.setServerPort(const Value: String);
begin
  fServerPort := Value;
end;



{ TClientSendQueue }

procedure TClientSendQueue.AfterConstruction;
begin
  inherited;
  fList := TInterfaceQueue.Create;
end;

function TClientSendQueue.AtLeast(ACount: Integer): Boolean;
begin
  Result := fList.AtLeast(ACount);
end;

procedure TClientSendQueue.BeforeDestruction;
begin
  fList.Free;
  inherited;
end;

function TClientSendQueue.Count: Integer;
begin
  Result := fList.Count;
end;

function TClientSendQueue.Peek: IClientQueueData;
begin
  Result := IClientQueueData(fList.Peek);
end;

function TClientSendQueue.Pop: IClientQueueData;
begin
  Result := IClientQueueData(fList.Pop);
end;

function TClientSendQueue.Push(AItem: IClientQueueData): IClientQueueData;
begin
  Result := IClientQueueData(FList.Push(AItem));
end;


end.
