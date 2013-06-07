unit msgbuf;

interface

uses SysUtils, Classes, Contnrs, SyncObjs;

const
  MESSAGE_BUFFER_SIZE = 1024;
  INT_DEFAULT_MAXBUFFERLIST = 10;
  STR_ERROR_EXCEED_MAXBUFFER = 'Max buffer size exceeded';
  
type
  EMessageBufferException = Exception;

  PMessage = ^TMessage;
  TMessage = packed record
    wSize: Word;
    bOption: Byte;
    Data: array[0..1024-3] of Char;
  end;

  TMessageBuffer = class
  private
    fMessages: array[0..MESSAGE_BUFFER_SIZE-1] of TMessage;
    fIndex, fCount: Integer;
    FOwner: Integer;
    procedure SetOwner(const Value: Integer);
    procedure ClearAll;
    procedure Clear(AIndex: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    function IsFull: Boolean;
    function IsEmpty: Boolean;
    // Push/Pop a message to/from buffer, return true if more buffer available
    function Push(ABuffer: Pointer; ASize: Cardinal): Boolean;
    function Pop(ABuffer: Pointer; ASize: Cardinal): Boolean;

    property Owner: Integer read FOwner write SetOwner;
    property Count: Integer read fCount;
    property Index: Integer read fIndex write fIndex;
  end;

  TMessageBuffers = class
  private
    fBuffers: TObjectList;
    FMaxBufferList: Cardinal;
    FLock: TCriticalSection;

    function NewMessageBuffer: TMessageBuffer;
    procedure SetMaxBufferList(const Value: Cardinal);

  public
    constructor Create;
    destructor Destroy; override;

    function TotalMessages: Int64;
    // Get a unused message buffer
    function BeginWrite(AOwner: Integer): TMessageBuffer;

    // Get a nonempty message buffer
    function BeginRead(AOwner: Integer): TMessageBuffer;

    // Return when message buffer is full
    procedure ExitBuffer(ABuffer: TMessageBuffer; AOwner: Integer);
    
//    procedure Push(ABuffer: Pointer; ASize: Cardinal);
//    procedure Pop(ABuffer: Pointer; ASize: Cardinal);
    property MaxBufferList: Cardinal read FMaxBufferList write SetMaxBufferList;

  end;




implementation

{ TMessageBuffer }

procedure TMessageBuffer.Clear(AIndex: Integer);
begin
  fMessages[AIndex].wSize := 0;
end;

procedure TMessageBuffer.ClearAll;
var
  i: Integer;
begin
  for I := Low(fMessages) to High(fMessages) do
    Clear(i);
    
end;

constructor TMessageBuffer.Create;
begin
  inherited Create;
  fCount := 0;
  fIndex := 0;
  fOwner := 0;
  ClearAll;
end;

destructor TMessageBuffer.Destroy;
begin
  inherited;
end;

function TMessageBuffer.IsEmpty: Boolean;
begin
  Result := fCount = 0;
end;

function TMessageBuffer.IsFull: Boolean;
begin
  Result := fCount >= MESSAGE_BUFFER_SIZE;
end;

function TMessageBuffer.Pop(ABuffer: Pointer; ASize: Cardinal): Boolean;
begin
  if fCount > 0 then
  begin
    Move(fMessages[fIndex], ABuffer^, ASize);
    Inc(fIndex);
    Dec(fCount);
    Result := True;
//    Result := fCount > 0;
  end else
    Result := False;
end;

function TMessageBuffer.Push(ABuffer: Pointer; ASize: Cardinal): Boolean;
begin
  if fIndex < MESSAGE_BUFFER_SIZE then
  begin
    Move(ABuffer^, fMessages[fIndex], ASize);
    Inc(fIndex);
    Inc(fCount);
    Result := True;    
//    Result := fIndex < MESSAGE_BUFFER_SIZE;
  end else
    Result := False;
end;

procedure TMessageBuffer.SetOwner(const Value: Integer);
begin
  FOwner := Value;
end;

{ TMessageBuffers }

constructor TMessageBuffers.Create;
begin
  inherited Create;
  fBuffers := TObjectList.Create;
  fBuffers.OwnsObjects := True;
  FMaxBufferList := INT_DEFAULT_MAXBUFFERLIST;
  FLock := TCriticalSection.Create;  
end;

destructor TMessageBuffers.Destroy;
begin
  FLock.Enter;
  try
    fBuffers.Free;
  finally
    FLock.Leave;
  end;
  FLock.Free;
  inherited;
end;

function TMessageBuffers.BeginRead(AOwner: Integer): TMessageBuffer;
var
  i: Integer;
begin
  FLock.Enter;
  try
    Result := nil;  
    for I := 0 to fBuffers.Count - 1 do
    begin
      if (TMessageBuffer(fBuffers[i]).Owner = 0) and (not TMessageBuffer(fBuffers[i]).IsEmpty) then
      begin
        Result := TMessageBuffer(fBuffers[i]);
        Result.Owner := AOwner;
        Result.Index := 0;
        Break;        
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

function TMessageBuffers.BeginWrite(AOwner: Integer): TMessageBuffer;
var
  i: Integer;
begin
  Result := nil;
  FLock.Enter;
  try
    for I := 0 to fBuffers.Count - 1 do
    begin
      if (TMessageBuffer(fBuffers[i]).Owner = 0) and (not TMessageBuffer(fBuffers[i]).IsFull) then
      begin
        Result := TMessageBuffer(fBuffers[i]);
        Result.Index := Result.Count;
      end;
    end;
    if Result = nil then
    begin
      Result := NewMessageBuffer;
    end;
    Result.Owner := AOwner;
  finally
    FLock.Leave;
  end;
end;

procedure TMessageBuffers.ExitBuffer(ABuffer: TMessageBuffer; AOwner: Integer);
begin
//  FLock.Enter;
  try
    if ABuffer.Owner = AOwner then ABuffer.Owner := 0;
  finally
//    FLock.Leave;
  end;
end;

function TMessageBuffers.NewMessageBuffer: TMessageBuffer;
begin
  if fBuffers.Count < fMaxBufferList then
  begin
    Result := TMessageBuffer.Create;
    fBuffers.Add(Result);
  end
  else
    raise EMessageBufferException.Create(STR_ERROR_EXCEED_MAXBUFFER);
end;
//
//procedure TMessageBuffers.Pop(ABuffer: Pointer; ASize: Cardinal);
//begin
//
//end;
//
//procedure TMessageBuffers.Push(ABuffer: Pointer; ASize: Cardinal);
//begin
//
//end;

procedure TMessageBuffers.SetMaxBufferList(const Value: Cardinal);
begin
  FLock.Enter;
  try
    FMaxBufferList := Value;
  finally
    FLock.Leave;
  end;
end;

function TMessageBuffers.TotalMessages: Int64;
var
  i: Integer;
begin
  Result := 0;
  FLock.Enter;
  try
    for I := 0 to fBuffers.Count - 1 do
    begin
      Inc(Result, TMessageBuffer(fBuffers[i]).Count);
    end;
  finally
    fLock.Leave;
  end;
end;

end.
