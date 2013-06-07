unit shapeServer_intf;

interface

uses shapeIntf;

type
  IShapeServerEvent = interface
    procedure OnMessageReceived(ASenderAddr: String; ASenderPort: String; AMessage: IShapeMessage); stdcall;
  end;

  IShapeServer = interface
    function GetActive: Boolean; stdcall;
    function GetAddress: String; stdcall;
    function GetMessageReceived: UInt64; stdcall;
//    function GetOnMessageReceived: TMessageReceivedEvent; stdcall;
    function GetPort: String; stdcall;
    procedure SetAddress(const Value: String); stdcall;
    procedure SetMessageReceived(const Value: UInt64); stdcall;
//    procedure SetOnMessageReceived(const Value: TMessageReceivedEvent); stdcall;
    procedure SetPort(const Value: String); stdcall;
    

    procedure Open;  stdcall;
    procedure Close; stdcall;

  end;

implementation

end.
