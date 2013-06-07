unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, MsgBuf, ExtCtrls;

type
  TWriteThread = class(TThread)
  private
    FMPS: Integer;
    FActualMPS: Integer;
    FTotalWrites: Int64;
    procedure SetMPS(const Value: Integer);
    function WriteMessages(ACount: Integer): Integer;
    procedure SetActualMPS(const Value: Integer);
    procedure SetTotalWrites(const Value: Int64);
  protected
    procedure Execute; override;
  public
    constructor Create;
    property MPS: Integer read FMPS write SetMPS;
    property ActualMPS: Integer read FActualMPS write SetActualMPS;
    property TotalWrites: Int64 read FTotalWrites write SetTotalWrites;
  end;

  TReadThread = class(TThread)
  private
    FActualMPS: Integer;
    FMPS: Integer;
    FTotalReads: Int64;
    function ReadMessages(ACount: Integer): Integer;
    procedure SetActualMPS(const Value: Integer);
    procedure SetMPS(const Value: Integer);
    procedure SetTotalReads(const Value: Int64);
  protected
    procedure Execute; override;
  public
    constructor Create;

    property MPS: Integer read FMPS write SetMPS;
    property ActualMPS: Integer read FActualMPS write SetActualMPS;
    property TotalReads: Int64 read FTotalReads write SetTotalReads;
  end;
  
  TForm2 = class(TForm)
    Label1: TLabel;
    lblTotal: TLabel;
    Label3: TLabel;
    lblTotalWrites: TLabel;
    Label5: TLabel;
    lblTotalReads: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    cboWriteSpeed: TComboBox;
    cboReadSpeed: TComboBox;
    btnStart: TButton;
    btnStop: TButton;
    Timer1: TTimer;
    lblReadDelays: TLabel;
    Label4: TLabel;
    lblWriteMPS: TLabel;
    lblReadMPS: TLabel;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    fWriteThread: TWriteThread;
    fReadThread: TReadThread;
    procedure Start;
    procedure Stop;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}
uses
  CodeSiteLogging,
  shapeCodec, shapeElement, shapeConsts, shapeTypes, Math, DateUtils;

const
  TM_SETSPEED = WM_USER + 1;

var
  BufferList: TMessageBuffers;
  TotalDelay: DWord;
  
procedure TForm2.btnStartClick(Sender: TObject);
begin
  Start;
  btnStop.Enabled := True;
  btnStart.Enabled := False;
end;

procedure TForm2.btnStopClick(Sender: TObject);
begin
  Stop;
  btnStop.Enabled := False;
  btnStart.Enabled := True;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  BufferList := TMessageBuffers.Create;
  fWriteThread := TWriteThread.Create;
  fWriteThread.MPS := 100;
  fReadThread := TReadThread.Create;
  fReadThread.MPS := 2000;
  TotalDelay := 0;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  fWriteThread.Terminate;
  fReadThread.Terminate;
  BufferList.Free;
end;

procedure TForm2.Start;
begin
  fWriteThread.Resume;
  fReadThread.Resume;
end;

procedure TForm2.Stop;
begin
  fWriteThread.Suspend;
  fReadThread.Suspend;
end;

procedure TForm2.Timer1Timer(Sender: TObject);
begin
  lblTotal.Caption := FormatFloat('#,0', BufferList.TotalMessages);
  lblTotalWrites.Caption := FormatFloat('#,0', TWriteThread(fWriteThread).TotalWrites);
  lblTotalReads.Caption := FormatFloat('#,0',  TReadThread(fReadThread).TotalReads);
  if TReadThread(fReadThread).TotalReads > 0 then
    lblReadDelays.Caption := FormatFloat('#,0',  TotalDelay / TReadThread(fReadThread).TotalReads);

  lblWriteMPS.Caption := FormatFloat('#,0', TWriteThread(fWriteThread).ActualMPS);
  lblReadMPS.Caption := FormatFloat('#,0',  TReadThread(fReadThread).ActualMPS);

end;

{ TWriteThread }

constructor TWriteThread.Create;
begin
  inherited Create(True);
  fMPS := 1000;
  fTotalWrites := 0;
  fActualMPS := 0;
end;

procedure TWriteThread.Execute;
var
  CurrMPS: Integer;
  msg: tagMSG;
  msdiff, i: TDateTime;
begin
  while not Terminated do
  begin
//    if PeekMessage(msg, 0, 0, 0, PM_REMOVE) then
//    begin
//      if msg.message = TM_SETSPEED then
//        MPS := msg.LParam;
//    end;
    i := Now;
    CurrMPS := 0;
    while ((Now - i) < OneSecond) and (CurrMPS < MPS) and  (not Terminated) do
    begin
      Inc(CurrMPS, WriteMessages(1));
//      Sleep(0);
    end;
    msdiff := (OneSecond - (Now-i)) / OneMillisecond;
    if msdiff > 0 then Sleep(Trunc(msdiff));
    FActualMPS := CurrMPS;
  end;
end;

procedure TWriteThread.SetActualMPS(const Value: Integer);
begin
  FActualMPS := Value;
end;

procedure TWriteThread.SetMPS(const Value: Integer);
begin
  FMPS := Value;
end;

procedure TWriteThread.SetTotalWrites(const Value: Int64);
begin
  FTotalWrites := Value;
end;

function TWriteThread.WriteMessages(ACount: Integer): Integer;
var
  msg: TShapeMessage;
  mem: TMemoryStream;
  Buffer: TMessageBuffer;
  i, c: Integer;
begin
  msg := TShapeMessage.Create;
  msg.Add('timestamp', SHAPE_DOUBLE);
  msg.Add('id', SHAPE_UINT32);
  mem := TMemoryStream.Create;
  c := 0;
  try
    Buffer := BufferList.BeginWrite(1);
    for I := 1 to ACount do
    begin
      msg.Values['timestamp'] := Now;
      msg.Values['id'] := fTotalWrites+1;
      mem.Clear;
      msg.WriteToStream(mem);
      if Buffer.Push(mem.Memory, mem.Size) then
      begin
        // Do nothing
        Inc(fTotalWrites);
        Inc(c);
      end
      else
      begin
        BufferList.ExitBuffer(Buffer, 1);
        Buffer := BufferList.BeginWrite(1);
      end;
      if Terminated then Exit; 
//      Sleep(INT_WRITE_DELAY);
    end;
  finally
    if Buffer <> nil then BufferList.ExitBuffer(Buffer, 1);
    msg.Free;
    mem.Free;
    Result := c;    
  end;

end;

{ TReadThread }

constructor TReadThread.Create;
begin
  inherited Create(True);
  fMPS := 1000;
  fTotalReads := 0;
  fActualMPS := 0;
end;

procedure TReadThread.Execute;
var
  CurrMPS: Integer;
  msg: tagMSG;
  i: TDateTime;
  msdiff: TDateTime;
begin
  while not Terminated do
  begin
    if PeekMessage(msg, 0, 0, 0, PM_REMOVE) then
    begin
      if msg.Message = TM_SETSPEED then
        MPS := msg.LParam;
    end;
    i := Now;
    CurrMPS := 0;
    while ((Now - i) < OneSecond) and (CurrMPS < MPS) and (not Terminated) do
    begin
      Inc(CurrMPS, ReadMessages(100));
//      Sleep(0);
    end;
    FActualMPS := CurrMPS;    
    msdiff := (OneSecond - (Now-i)) / OneMillisecond;
    if msdiff > 0 then Sleep(Trunc(msdiff));
  end;
end;

function TReadThread.ReadMessages(ACount: Integer): Integer;
var
  msg: TShapeMessage;
  mem: TMemoryStream;
  Buffer: TMessageBuffer;
  i: Integer;
  msgDelay: Integer;
begin
  msg := TShapeMessage.Create;
  msg.Add('timestamp', SHAPE_DOUBLE);
  msg.Add('id', SHAPE_UINT32);
  mem := TMemoryStream.Create;
  mem.SetSize(1000);
  i := 0;
  try
    Buffer := BufferList.BeginRead(2);
    while (Buffer <> nil) and (i < ACount) do
    begin
      try
        while Buffer.Pop(mem.Memory, mem.Size) do
        begin
          mem.Seek(0, 0);
          msg.ReadFromStream(mem);
          msgDelay := Trunc((Now-msg.ElementByName['timestamp'].AsDouble)/OneMillisecond);
          if msgDelay > 0 then
            Inc(TotalDelay, msgDelay);
//          CodeSite.SendFmtMsg('ID=%d, Timestamp=%s', [msg.ElementByName['id'].AsUInt32,
//            DateTimeToSTr(msg.ElementByName['timestamp'].AsDouble)]);

          Inc(fTotalReads);
          Inc(i);
          if i >= ACount then Exit;
          if Terminated then Exit;                   
        end;
      finally
        if Buffer <> nil then BufferList.ExitBuffer(Buffer, 2);
      end;
      Buffer := BufferList.BeginRead(2);
    end
  finally
    if Buffer <> nil then BufferList.ExitBuffer(Buffer, 2);
    
    msg.Free;
    mem.Free;
    Result := i;    
  end;

end;

procedure TReadThread.SetActualMPS(const Value: Integer);
begin
  FActualMPS := Value;
end;

procedure TReadThread.SetMPS(const Value: Integer);
begin
  FMPS := Value;
end;

procedure TReadThread.SetTotalReads(const Value: Int64);
begin
  FTotalReads := Value;
end;

end.
