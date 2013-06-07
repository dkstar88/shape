unit main;

interface

uses Windows, SysUtils, Messages, Classes,
  ShapeServer, StormSender, ShapeIntf, synsock;

const
  WM_RELOAD_CONFIG = WM_USER + 1;
  WM_START_SERVER = WM_USER + 2;
  WM_STOP_SERVER  = WM_USER + 3;
  WM_SETLOGWINDOW = WM_USER + 5;

  WM_LOGDATA = WM_USER + 100;

type
  TShapeTransender = class(TThread)
  private
    FSender: TMessageSender;
    FServer: TShapeServer;
    FSenderThread: TMessageSendThread;
    FLogHWnd: THandle;
    
    procedure SetSender(const Value: TMessageSender);
    procedure SetServer(const Value: TShapeServer);
    procedure ReloadConfig;

    procedure LogMessage(ASection: String; AMEssage: IShapeMEssage);
    procedure SetLogHWnd(const Value: THandle);

  protected
    procedure OnMessageReceived(ASender: TObject; ASenderAddr: TInAddr;
      AMessage: IShapeMessage);
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadConfig;
    procedure Start;
    procedure Stop;
    
    property Server: TShapeServer read FServer write SetServer;
    property Sender: TMessageSender read FSender write SetSender;
    property LogHWnd: THandle read FLogHWnd write SetLogHWnd;
  end;


implementation

uses IniFiles;

var
  tmplog : TSTrings;

procedure WriteLog(strlog:string);
begin
  if tmplog = nil then
    tmplog := TStringList.Create;
  tmplog.Append(FormatDateTime('hh:nn:sss', Now) + #9 + strlog);

//  strFile := ExtractFilePath(paramstr(0))+'\log\';
//  if not DirectoryExists(strFile) then
//    CreateDir(strFile);
//  try
//    strfile := strFile + FormatDateTime('yyyy-mm-dd', Now)+'.log';
//    AssignFile(Hfile,strFile);
//    try
//      if FileExists(strfile) then
//        Append(Hfile)
//      else
//        ReWrite(HFile);
//      writeln(Hfile, FormatDateTime('hh:nn:sss', Now) + #9 + strlog);
//    finally
//      closefile(Hfile);
//    end;
//  except
//    on E: Exception do
////    Application.MessageBox('写入日志错误','提示',MB_OK);
//  end;
end;


{ TShapeTransender }

constructor TShapeTransender.Create;
begin
  inherited Create(True);
  fServer := TShapeServer.Create;
  fServer.Priority := tpTimeCritical;
  fServer.OnMessageReceived := OnMessageReceived;
  fServer.Resume;
  fLogHWnd := 0;
  fSender := TStormSender.Create;
  LoadConfig;  
  FSenderThread := TMessageSendThread.Create(Sender);
  FSenderThread.Resume;
end;

destructor TShapeTransender.Destroy;
begin
  FSenderThread.Terminate;
  FSenderThread.Free;
  fServer.Terminate;
  fServer.Free;
  fSender.Free;
  inherited;
end;

procedure TShapeTransender.Execute;
var
  msg: tagMsg;
begin
  inherited;
  while not Terminated do
  begin
    if PeekMessage(msg, 0, 0, 0, PM_NOREMOVE) then
    begin
      if msg.message = WM_QUIT then
      begin
        Stop;
        Break;
      end else if msg.message = WM_RELOAD_CONFIG then
      begin
        ReloadConfig;
      end else if msg.message = WM_START_SERVER then
      begin
        Start;
      end else if msg.message = WM_STOP_SERVER then
      begin
        Stop;
      end else if msg.message = WM_SETLOGWINDOW then
      begin
        fLogHWND := msg.lParam;
      end;
    end;
    Sleep(100);
  end;
end;

procedure TShapeTransender.LoadConfig;
var
  fIni: TIniFile;
begin
  fIni := TIniFile.Create('config\default.ini');
  try
    Sender.TargetServer := fIni.ReadString('target', 'address', '127.0.0.1');
    Sender.TargetPort := fIni.ReadString('target', 'port', '61613');

    Server.Address := fIni.ReadString('shape_server', 'address', '0.0.0.0');
    Server.Port := fIni.ReadString('shape_server', 'port', '54493');

//    Sender.TargetServer := fIni.ReadString('target', 'address', '127.0.0.1');
  finally
    fIni.Free;
  end;
end;

procedure TShapeTransender.LogMessage(ASection: String;
  AMEssage: IShapeMEssage);
var
  s: String;
  i: Integer;
  ele: IShapeElement;
begin
//  s := '';
//  for I := 0 to AMessage.Count - 1 do
//  begin
//    ele := AMessage.Elements[i];
//    s := s + Format('%s=%s', [ele.Name, ele.AsString]) + ',';
//  end;
  s := IntToStr(Server.MessageReceived);
  WriteLog(s);
//  SendMessage(fLogHWnd, WM_LOGDATA, 0, LongInt(s));
end;

procedure TShapeTransender.OnMessageReceived(ASender: TObject;
  ASenderAddr: TInAddr; AMessage: IShapeMessage);
begin
  LogMessage('MessageReceived: ', AMessage);
  Sender.MessageQueue.Push(AMessage);
end;

procedure TShapeTransender.ReloadConfig;
begin
  FSenderThread.Suspend;
  LoadConfig;
  FSenderThread.Resume;
  PostThreadMessage(FSenderThread.Handle, WM_RESTART_SENDER, 0, 0); 
end;

procedure TShapeTransender.SetLogHWnd(const Value: THandle);
begin
  FLogHWnd := Value;
end;

procedure TShapeTransender.SetSender(const Value: TMessageSender);
begin
  FSender := Value;
end;

procedure TShapeTransender.SetServer(const Value: TShapeServer);
begin
  FServer := Value;
end;

procedure TShapeTransender.Start;
begin
  Server.Open;
//  FSenderThread.Resume;
end;

procedure TShapeTransender.Stop;
begin
//  FSenderThread.Suspend;
  Server.Close;
end;

initialization

finalization
  if tmplog <> nil then
  begin
    tmplog.SaveToFile(ExtractFilePath(paramstr(0))+'\log\'+FormatDateTime('yyyy-mm-dd', Now)+'.log');
    tmplog.Free;
  end;


end.
