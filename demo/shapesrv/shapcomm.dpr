library shapcomm;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  SysUtils,
  Classes,
  messagequeue_intf in 'messagequeue_intf.pas',
  messageQueue in 'messageQueue.pas',
  ShapeServer in 'ShapeServer.pas',
  shapeclient in '..\shapeclient\shapeclient.pas',
  shapeCodec in '..\Shape\shapeCodec.pas',
  shapeConsts in '..\Shape\shapeConsts.pas',
  shapeElement in '..\Shape\shapeElement.pas',
  shapeEncodings in '..\Shape\shapeEncodings.pas',
  shapeIntf in '..\Shape\shapeIntf.pas',
  shapeTypes in '..\Shape\shapeTypes.pas',
  shapeclient_intf in '..\shapeclient\shapeclient_intf.pas',
  interfaceQueue in 'interfaceQueue.pas',
  shapereg in '..\package\shapereg.pas',
  shapeServer_intf in 'shapeServer_intf.pas';

{$R *.res}

exports
  newMessage, cloneMessage, newMessageQueue, NewShapeClient, newMessageFromSpec;

begin
end.
