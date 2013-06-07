// JCL_DEBUG_EXPERT_GENERATEJDBG ON
// JCL_DEBUG_EXPERT_INSERTJDBG ON
program shapesrv;

{%AQtime 'Profiling\AQtimeModule1.aqt'}

uses
  Forms,
  SysUtils,
  ShapeServer in 'ShapeServer.pas',
  shapeCodec in '..\Shape\shapeCodec.pas',
  shapeConsts in '..\Shape\shapeConsts.pas',
  shapeElement in '..\Shape\shapeElement.pas',
  shapeEncodings in '..\Shape\shapeEncodings.pas',
  shapeIntf in '..\Shape\shapeIntf.pas',
  shapeTypes in '..\Shape\shapeTypes.pas',
  messageQueue in 'messageQueue.pas',
  interfaceQueue in 'interfaceQueue.pas',
  stormSender in 'stormSender.pas',
  main in 'main.pas',
  mainui in 'mainui.pas' {Form1};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
