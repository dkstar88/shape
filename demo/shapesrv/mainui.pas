unit mainui;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, main, stormSender, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  protected
    procedure CreateHandle; override;
  private
    { Private declarations }
    Transender: TShapeTransender;
//    procedure WMLogMessage(var Msg: TMessage); message WM_COPYDATA;
//    procedure WMLogMessage(var Msg: TMessage); message WM_LOGDATA;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.CreateHandle;
begin
  inherited;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Transender := TShapeTransender.Create;
  Transender.Resume;
  Transender.Start;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Transender.Terminate;
  Transender.Free;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
//  PostThreadMessage(Transender.ThreadID, WM_SETLOGWINDOW, 0, Self.Handle);
end;

//procedure TForm1.WMLogMessage(var Msg: TMessage);
////var
////  s: String;
//begin
////  s := String(Msg.LParam);
////  WriteLog(s);
////  //Memo1.Lines.Add(s);
////  Label1.Caption := 'Message Received: ' + IntToStr(Transender.Server.MessageReceived);
//end;

end.
