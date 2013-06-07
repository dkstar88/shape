program msgbuftest;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  msgbuf in 'msgbuf.pas';

const
  INT_WRITE_COUNT = 10000;
  INT_WRITE_DELAY = 1;
  INT_READ_DELAY = 1;
  INT_WRITE_SWITCH = 1000;

var
  BufferList: TMessageBuffers;

procedure WriteBuffer;
var
  msg: TMessage;
  Buffer: TMessageBuffer;
  i: Integer;
begin
  FillChar(msg, SizeOf(msg), #0);
  msg.bOption := 0;
  msg.Data := 'this is a test';
  msg.wSize := Length('this is a test');
  try
    Buffer := BufferList.BeginWrite(1);
    for I := 0 to INT_WRITE_COUNT do
    begin
      if Buffer.Push(@msg, SizeOf(msg)) then
        // Do nothing
      else
      begin
        BufferList.ExitBuffer(Buffer);
        Buffer := BufferList.BeginWrite(1);
      end;
//      Sleep(INT_WRITE_DELAY);
    end;
  finally
    BufferList.ExitBuffer(Buffer);
  end;
end;

procedure ReadBuffer;
var
  msg: TMessage;
  msgbuf: Pointer;
  Buffer: TMessageBuffer;
  i: Integer;
begin
  FillChar(msg, SizeOf(msg), #0);
  msg.bOption := 0;
  msg.Data := 'this is a test';
  msg.wSize := Length('this is a test');
  try
    repeat
      Buffer := BufferList.BeginRead(1);
      try
        msgbuf := @msg;
        while Buffer.Pop(msgbuf, SizeOf(msg)) do
        begin
        
        end;
      finally
        BufferList.ExitBuffer(Buffer);
      end;
    until Buffer <> nil;
  finally

  end;
end;


begin
  try
    try
      BufferList := TMessageBuffers.Create;
      WriteBuffer;
      ReadBuffer;
    finally
      BufferList.Free;
    end;
  except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
end.
