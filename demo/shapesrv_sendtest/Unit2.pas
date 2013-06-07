unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ShapeTypes, shapeCodec, shapeIntf, OverbyteIcsWndControl,
  OverbyteIcsWSocket, OverbyteIcsWinSock, shapeClient, ExtCtrls;

type
  TWSocket2 = class(TWSocket)
  public
    property ReadCount;
    property WriteCount;

  end;

  TForm2 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    txtPort: TLabeledEdit;
    txtServer: TLabeledEdit;
    WSocket1: TWSocket;
    Button3: TButton;
    Label1: TLabel;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure WSocket1DataSent(Sender: TObject; ErrCode: Word);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    fShapeClient: TShapeClient;
    fMessage: IShapeMessage;
    procedure UpdateSocket;
    procedure SendMsg(ACount: Integer);
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}
uses DateUtils, ShapeConsts, blcksock;

procedure TForm2.Button1Click(Sender: TObject);
begin
  SendMsg(10);
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  SendMsg(100);
end;


procedure TForm2.Button3Click(Sender: TObject);
begin
  SendMsg(10000);
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  fMessage := TShapeMessage.Create;
  fMessage.LoadSpecFromFile('E:\Delphi\2007\UDP\shapesrv\formats\status.shape');
  fMessage.AddInternal(SHAPE_COMMAND, 'status');
  fMessage.Values['user_id'] := '';
  fMessage.Values['game_id'] := 15;

  fShapeClient := TShapeClient.Create;
  fShapeClient.MPS := 0;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  fShapeClient.Terminate;
  fShapeClient.Free;
//  fMessage.Free;
end;

procedure TForm2.FormShow(Sender: TObject);
begin
  fShapeClient.Resume;
end;

procedure TForm2.SendMsg(ACount: Integer);
var
  msg: IShapeMessage;
  i: Integer;
begin
  UpdateSocket;
  msg := TShapeMessage.Create;
  msg.Assign(fMessage);
  try
//    fMessage.WriteToStream(stream);
//    WSocket1.Connect;
    for I := 1 to ACount do
    begin
      fShapeClient.SendQueue.Push(msg);
//      WSocket1.Connect;
//      WSocket1.SendTo(WSocket1.sin, SizeOf(TSockAddr), stream.Memory, stream.Size);
////      WSocket1.SendTo(stream.Memory, stream.Size);
//      WSocket1.Close;
    end;

  finally
//    stream.Free;
  end;
end;

procedure TForm2.Timer1Timer(Sender: TObject);
begin
  Label1.Caption := Format('Send Count: %d', [fShapeClient.SendCount]);
end;

procedure TForm2.UpdateSocket;
begin
  WSocket1.Proto      := 'udp';
  WSocket1.Addr       := txtServer.Text;     { That's a broadcast  ! }
  WSocket1.Port       := txtPort.Text;
  fShapeClient.ServerAddress := txtServer.Text;
  fShapeClient.ServerPort := txtPort.Text;
end;

procedure TForm2.WSocket1DataSent(Sender: TObject; ErrCode: Word);
begin
//  WSocket1.Close;
end;

end.
