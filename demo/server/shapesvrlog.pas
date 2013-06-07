unit shapesvrlog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, shapeIntf, shapeCodec, shapeConsts, shapeTypes,
  OverbyteIcsWndControl, OverbyteIcsWSocket, OverbyteIcsWSocketS,
  OverbyteIcsLogger, WinSock, udpdata, ExtCtrls, Grids, MPHexEditor;

type
  TfrmShapeSrv = class(TForm)
    Memo1: TMemo;
    WSocket: TWSocket;
    IcsLogger1: TIcsLogger;
    btnListen: TButton;
    btnStop: TButton;
    txtPort: TLabeledEdit;
    txtSenderIP: TLabeledEdit;
    hexViewer: TMPHexEditor;
    Label1: TLabel;
    WSocketServer1: TWSocketServer;
    procedure FormCreate(Sender: TObject);
    procedure WSocketDataAvailable(Sender: TObject; ErrCode: Word);
    procedure WSocketSessionConnected(Sender: TObject; ErrCode: Word);
    procedure WSocketSessionClosed(Sender: TObject; ErrCode: Word);
    procedure btnListenClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    shapemessage: IShapeMEssage;
    FSenderAddr    : TInAddr;
    fUDPMan: TUDPDataManager;

    procedure UDPDataReady(ASender: TSockAddrIn; AData: TStream; var ARemove: Boolean);
  public
    { Public declarations }
  end;

var
  frmShapeSrv: TfrmShapeSrv;

implementation

{$R *.dfm}

procedure TfrmShapeSrv.btnListenClick(Sender: TObject);
begin
  FSenderAddr               := WSocketResolveHost(AnsiString(txtSenderIP.Text));
  if FSenderAddr.S_addr = htonl(INADDR_LOOPBACK) then begin
      { Replace loopback address by real localhost IP addr }
      FSenderAddr           := WSocketResolveHost(LocalHostName);
  end;
  WSocket.Proto             := 'udp';
  WSocket.Addr              := '0.0.0.0';
  WSocket.Port              := txtPort.Text;
  WSocket.Listen;
end;

procedure TfrmShapeSrv.btnStopClick(Sender: TObject);
begin
  btnListen.Enabled := True;
  btnStop.Enabled := False;
  WSocket.Close;
end;

procedure TfrmShapeSrv.FormCreate(Sender: TObject);
begin
  fUDPMan := TUDPDataManager.Create;
  fUDPMan.OnDataReady := UDPDataReady;
  shapemessage := TShapeMessage.Create;
  shapemessage.Add('Name', SHAPE_STRING);
  shapemessage.Add('Age', SHAPE_UINT16);
  shapemessage.Add('DOB', SHAPE_DOUBLE);
  shapemessage.Add('Height', SHAPE_SINGLE);
end;

procedure TfrmShapeSrv.FormDestroy(Sender: TObject);
begin
  fUDPMan.Free;
end;

procedure TfrmShapeSrv.UDPDataReady(ASender: TSockAddrIn; AData: TStream;
  var ARemove: Boolean);
var
  i: Integer;
begin
  hexViewer.LoadFromStream(AData);
  AData.Seek(0, 0);
  shapemessage.ReadFromStream(AData);
  for I := 0 to shapemessage.Count - 1 do
  begin
    Memo1.Lines.Append(Format('  %s=%s', [shapemessage.Elements[i].Name,
      shapemessage.Elements[i].AsString]));
  end;
  Memo1.Lines.Append('---------------------------');
  Memo1.Lines.Append('');
end;

procedure TfrmShapeSrv.WSocketDataAvailable(Sender: TObject; ErrCode: Word);
var
    Buffer : array [0..1023] of AnsiChar;
    i, Len    : Integer;
    Src    : TSockAddrIn;
    SrcLen : Integer;
    stream: TMemoryStream;
    msg: String;
begin
    SrcLen := SizeOf(Src);
    stream := TMemoryStream.Create;
    try
      Len    := WSocket.ReceiveFrom(@Buffer, SizeOf(Buffer), Src, SrcLen);
      if (FSenderAddr.S_addr = INADDR_ANY) or
         (FSenderAddr.S_addr = Src.Sin_addr.S_addr) then
      begin
        Memo1.Lines.Append(String(StrPas(inet_ntoa(Src.sin_addr))) +
                                      ':'  + IntToStr(ntohs(Src.sin_port)));

        if Len >= 0 then
          fUDPMan.Receive(Src, @Buffer, Len, True)
        else
          Memo1.Lines.Append(Format('Winsock Error: %d', [WSocket.LastError]));


//        repeat
//          Len    := WSocket.ReceiveFrom(@Buffer, SizeOf(Buffer), Src, SrcLen);
//          begin
//            else
//              fUDPMan.Receive(Src, @Buffer, SizeOf(Buffer), False);
//          end;
//        until len = 0;
      end;

    finally
      stream.Free;
    end;
end;

procedure TfrmShapeSrv.WSocketSessionClosed(Sender: TObject; ErrCode: Word);
begin
  Memo1.Lines.Append('Socket closed');
  btnListen.Enabled := True;
  btnStop.Enabled := False;
 
end;

procedure TfrmShapeSrv.WSocketSessionConnected(Sender: TObject; ErrCode: Word);
begin
  Memo1.Lines.Append('Start listening');
  btnListen.Enabled := False;
  btnStop.Enabled := True;

end;

end.
