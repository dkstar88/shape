program shapesrv;

uses
  Forms,
  shapesvrlog in 'shapesvrlog.pas' {frmShapeSrv},
  shapeCodec in '..\Shape\shapeCodec.pas',
  shapeConsts in '..\Shape\shapeConsts.pas',
  shapeElement in '..\Shape\shapeElement.pas',
  shapeEncodings in '..\Shape\shapeEncodings.pas',
  shapeIntf in '..\Shape\shapeIntf.pas',
  shapeTypes in '..\Shape\shapeTypes.pas',
  udpdata in 'udpdata.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmShapeSrv, frmShapeSrv);
  Application.Run;
end.
