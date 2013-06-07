program shapeedit;

uses
  Forms,
  Editor in 'Editor.pas' {Form2},
  shapeCodec in 'shapeCodec.pas',
  shapeConsts in 'shapeConsts.pas',
  shapeIntf in 'shapeIntf.pas',
  shapeElement in 'shapeElement.pas',
  shapeTypes in 'shapeTypes.pas',
  shapeEncodings in 'shapeEncodings.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
