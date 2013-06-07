unit stormSender;

interface

uses Windows, Messages, Classes, WinSock, SysUtils, shapeCodec, shapeElement, cFileUtils,
  shapeConsts, shapeIntf, shapeTypes, OverbyteIcsWSocket, StompClient,
  StompTypes, messageQueue;

const
  WM_RESTART_SENDER = WM_USER + 10;
    
type
  TMessageSender = class(TObject)
  private
    FTargetPort: String;
    FTargetServer: String;
    FMessageQueue: TMessageQueue;

    procedure SetTargetPort(const Value: String);
    procedure SetTargetServer(const Value: String);
    function GetMessageQueue: TMessageQueue;
    function GetTargetPort: String;
    function GetTargetServer: String;
  protected
    procedure Start; virtual; abstract;
    procedure Stop; virtual; abstract;
    procedure SendPeek; virtual; abstract;
    procedure SendBatch(const ACount: Integer); virtual;
    procedure SendBatchStart; virtual; abstract;
    procedure SendBatchEnd; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    property TargetServer: String read GetTargetServer write SetTargetServer;
    property TargetPort: String read GetTargetPort write SetTargetPort;
    property MessageQueue: TMessageQueue read GetMessageQueue;
  end;

  TStormSender = class(TMessageSender)
  private
    fStomp: IStompClient;

  protected
    procedure SendBatchStart; override;
    procedure SendBatchEnd; override;
    procedure SendPeek; override;
    procedure Start; override;
    procedure Stop; override;
  public
    constructor Create;
    destructor Destroy; override;

  end;

  TMessageSendThread = class(TThread)
  private
    FSender: TMessageSender;
    procedure SetSender(const Value: TMessageSender);
  protected
    procedure Execute; override;
  published
  public
    constructor Create(ASender: TMessageSender);
    property Sender: TMessageSender read FSender write SetSender;
  end;


implementation

{ TStormSender }
constructor TStormSender.Create;
begin
  inherited Create;
end;

destructor TStormSender.Destroy;
begin

  inherited;
end;

procedure TStormSender.SendBatchEnd;
begin
  fStomp.CommitTransaction('batch');
end;

procedure TStormSender.SendBatchStart;
begin
  fStomp.BeginTransaction('batch');
end;

procedure TStormSender.SendPeek;
var
  Msg: IShapeMessage;
  Ele: IShapeElement;
  Headers: IStompHeaders;
  i: Integer;
  TargetQueue: String;
begin
  Msg := IShapeMessage(FMessageQueue.Pop);
  try
    Headers := StompUtils.NewHeaders;
    TargetQueue := '/queue/' + Msg.Values['_command'];
    for I := 0 to Msg.Count - 1 do
    begin
      Ele := Msg.Elements[i];
      if (Ele.ElementType <> SHAPE_DESTINATION)
        and (Ele.ElementType <> SHAPE_COMMAND) then
        Headers.Add(Ele.Name, Ele.AsString);
    end;
    fStomp.Send(TargetQueue, '', Headers);
  finally
    Msg := nil;
  end;
end;

procedure TStormSender.Start;
begin
  fStomp := StompUtils.NewStomp(TargetServer, StrToInt(TargetPort));
end;

procedure TStormSender.Stop;
begin
  fStomp := nil;
end;

{ TMessageSender }

constructor TMessageSender.Create;
begin
  inherited Create;
  fTargetServer := '127.0.0.1';
  fTargetPort := '61613';
  FMessageQueue := TMessageQueue.Create;
end;

destructor TMessageSender.Destroy;
begin
  FMessageQueue.Free;
  inherited;
end;

function TMessageSender.GetMessageQueue: TMessageQueue;
begin
  Result := fMessageQueue;
end;

function TMessageSender.GetTargetPort: String;
begin
  Result := fTargetPort;
end;

function TMessageSender.GetTargetServer: String;
begin
  Result := fTargetServer;
end;

procedure TMessageSender.SendBatch(const ACount: Integer);
var
  i: Integer;
begin
  SendBatchStart;
  try
    for I := 1 to ACount do
    begin
      if MessageQueue.AtLeast(1) then
      begin
        SendPeek
      end else
        Break;
    end;
  finally
    SendBatchEnd;
  end;
end;

procedure TMessageSender.SetTargetPort(const Value: String);
begin
  fTargetPort := Value;
end;

procedure TMessageSender.SetTargetServer(const Value: String);
begin
  fTargetServer := Value;
end;

{ TMessageSendThread }

constructor TMessageSendThread.Create(ASender: TMessageSender);
begin
  inherited Create(True);
  fSender := ASender;
end;

procedure TMessageSendThread.Execute;
var
  msg: tagMsg;
begin
  inherited;
  fSender.Start;
  try
    while not Terminated do
    begin

      if PeekMessage(msg, 0, 0, 0, PM_NOREMOVE) then
      begin
        if msg.message = WM_RESTART_SENDER then
        begin
          fSender.Stop;
          fSender.Start;
        end;
      end;

      if fSender.MessageQueue.AtLeast(1) then
      begin
        fSender.SendBatch(100);
      end
      else
        Sleep(0);
    end;
  finally
    fSender.Stop;
  end;
end;


procedure TMessageSendThread.SetSender(const Value: TMessageSender);
begin
  FSender := Value;
end;

end.
