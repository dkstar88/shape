program threadtest;

uses
  Forms,
  Unit2 in 'Unit2.pas' {Form2},
  msgbuf in '..\msgbuf.pas',
  shapeCodec in '..\..\Shape\shapeCodec.pas',
  shapeConsts in '..\..\Shape\shapeConsts.pas',
  shapeElement in '..\..\Shape\shapeElement.pas',
  shapeEncodings in '..\..\Shape\shapeEncodings.pas',
  shapeIntf in '..\..\Shape\shapeIntf.pas',
  shapeTypes in '..\..\Shape\shapeTypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
