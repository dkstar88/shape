unit shapeclient_intf;

interface

uses Classes, shapeIntf;

type
  IClientQueueData = interface

    function getMessage: IShapeMessage; stdcall;
    function getProtocol: String; stdcall;
    function getServerAddress: String; stdcall;
    function getServerPort: String; stdcall;
    procedure setProtocol(const Value: String); stdcall;
    procedure setServerAddress(const Value: String); stdcall;
    procedure setServerPort(const Value: String); stdcall;
    procedure setMessage(const Value: IShapeMessage); stdcall;

    property ServerAddress: String read getServerAddress write setServerAddress;
    property ServerPort: String read getServerPort write setServerPort;
    property Message: IShapeMessage read getMessage write setMessage;
    property Protocol: String read getProtocol write setProtocol;

  end;

  IClientSendQueue = interface
    function Count: Integer;  stdcall;
    function AtLeast(ACount: Integer): Boolean; stdcall;
    function Push(AItem: IClientQueueData): IClientQueueData;  stdcall;
    function Pop: IClientQueueData; stdcall;
    function Peek: IClientQueueData; stdcall;
  end;
  

  IShapeClient = interface

    procedure Queue(ADestAddr, APort: String; AMessage: IShapeMessage); stdcall;  
		function Send(ADestAddr, APort: String; AMessage: IShapeMessage): Boolean; stdcall;

    procedure SetServerAddress(const Value: String); stdcall;
    procedure SetServerPort(const Value: String); stdcall;
    procedure SetSendCount(const Value: UInt64); stdcall;
    procedure SetMPS(const Value: Integer); stdcall;
    procedure SetProt(const Value: String); stdcall;
    
    function getActive: Boolean; stdcall;
    function getMPS: Integer; stdcall;
    function getProt: String; stdcall;
    function getSendCount: UInt64; stdcall;
    function getSendQueue: IClientSendQueue; stdcall;
    function getServerAddress: String; stdcall;
    function getServerPort: String; stdcall;
    
    property SendQueue: IClientSendQueue read getSendQueue;
    property Active: Boolean read getActive;
    property SendCount: UInt64 read getSendCount write SetSendCount;
    property MPS: Integer read getMPS write SetMPS;
  end;

implementation

end.
