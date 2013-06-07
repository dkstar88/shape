unit shapeIntf;

interface

uses Classes, shapeConsts;

type
  IShapeElement = interface

    function GetData: Variant; stdcall;
    function GetElementType: TShapeElementType; stdcall;
    function GetName: String;  stdcall;
    function GetAsDouble: Double; stdcall;
    function getAsInt16: Int16; stdcall;
    function GetAsInt64: Int64; stdcall;
    function GetAsInteger: Integer; stdcall;
    function GetAsSingle: Single; stdcall;
    function GetAsString: String; stdcall;
    function getAsUInt16: UInt16; stdcall;
    function getAsUInt32: UInt32; stdcall;
    function GetAsUTF8: WideString; stdcall;
    procedure SetAsDouble(const Value: Double); stdcall;
    procedure SetAsInt16(const Value: Int16); stdcall;
    procedure SetAsInt64(const Value: Int64); stdcall;
    procedure SetAsInteger(const Value: Integer); stdcall;
    procedure SetAsSingle(const Value: Single); stdcall;
    procedure SetAsString(const Value: String); stdcall;
    procedure setAsUInt16(const Value: UInt16);  stdcall;
    procedure setAsUInt32(const Value: UInt32);  stdcall;
    procedure SetAsUTF8(const Value: WideString); stdcall;

    procedure SetData(const Value: Variant); stdcall;
    procedure SetElementType(const Value: TShapeElementType); stdcall;
    procedure SetName(const Value: String); stdcall;

    function getLength: Integer;  stdcall;
    function getIsEmpty: Boolean; stdcall;
    procedure setIsEmpty(const Value: Boolean);  stdcall;

    function Read(AStream: TStream): Boolean; stdcall;
    function Write(AStream: TStream): Boolean; stdcall;
    
    procedure Assign(ADest: IShapeElement); stdcall;    

    // Name of element
    property Name: String read GetName write SetName;
    // Element type
    property ElementType: TShapeElementType read GetElementType write SetElementType;
    // Element value in various types
    property AsInt16: Int16 read getAsInt16 write SetAsInt16;
    property AsUInt16: UInt16 read getAsUInt16 write setAsUInt16;
    property AsUInt32: UInt32 read getAsUInt32 write setAsUInt32;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    property AsDouble: Double read GetAsDouble write SetAsDouble;
    property AsSingle: Single read GetAsSingle write SetAsSingle;
    property AsString: String read GetAsString write SetAsString;
    property AsUTF8: WideString read GetAsUTF8 write SetAsUTF8;

    property Data: Variant read GetData write SetData;
    // Data length, not including Element type
    property Length: Integer read getLength;
    // Is data is empty
    property IsEmpty: Boolean read getIsEmpty write setIsEmpty;
  end;

  IShapeMessage = interface

    function GetElements(AIndex: Integer): IShapeElement; stdcall;
    procedure SetElements(AIndex: Integer; const AValue: IShapeElement); stdcall;
    function getCount: Integer; stdcall;
    function GetValues(AName: String): Variant; stdcall;
    procedure SetValues(AName: String; const Value: Variant); stdcall;
    procedure SetEncoding(const Value: Byte); stdcall;
    function GetElementByName(AName: String): IShapeElement; stdcall;
    procedure SetElementByName(AName: String; const Value: IShapeElement);  stdcall;
    function GetEncoding: Byte; stdcall;

    function GetAsBoolean(AName: String): Boolean;  stdcall;
    function GetAsInteger(AName: String): Integer;  stdcall;
    function GetAsString(AName: String): String;stdcall;
    procedure SetAsBoolean(AName: String; const Value: Boolean);stdcall;
    procedure SetAsInteger(AName: String; const Value: Integer);stdcall;
    procedure SetAsString(AName: String; const Value: String);stdcall;    

    procedure LoadSpecFromFile(AFilename: String); stdcall;
    procedure SaveSpecToFile(AFilename: String); stdcall;

    function ReadFromStream(AStream: TStream): Boolean; stdcall;
    function WriteToStream(AStream: TStream): Boolean; stdcall;

    function ReadFromBuffer(ABuffer: Pointer; ASize: Word): Boolean; stdcall;
    function WriteToBuffer(ABuffer: Pointer; var ASize: Word): Boolean; stdcall;

    function AsText: WideString; stdcall;

    procedure LoadFromFile(AFilename: String); stdcall;
    procedure SaveToFile(AFilename: String); stdcall;

    procedure Assign(ASource: IShapeMessage); stdcall;
    procedure Clear; stdcall;
    function CalcSize: Integer;  stdcall;

    function Add(const AName: String;
      const AValueType: TShapeElementType = SHAPE_STRING): IShapeElement; stdcall;

    function AddInternal(const AValueType: TShapeElementType;
      AValue: Variant): IShapeElement; stdcall;

    procedure Remove(AElement: IShapeElement); stdcall;
    procedure Delete(AIndex: Integer); stdcall;

    property Count: Integer read getCount;
    property Elements[AIndex: Integer]:IShapeElement read GetElements write SetElements;
    property ElementByName[AName: String]:IShapeElement read GetElementByName write SetElementByName;
    property Values[AName: String]: Variant read GetValues write SetValues;default;
    property AsString[AName: String]: String read GetAsString write SetAsString;
    property AsInteger[AName: String]: Integer read GetAsInteger write SetAsInteger;
    property AsBoolean[AName: String]: Boolean read GetAsBoolean write SetAsBoolean;
    property Encoding: Byte read GetEncoding write SetEncoding;

  end;


  IShapeFormats = interface
  
    function DetectMessageFormat(AStream: TStream): String; stdcall;
    function ReadFromStream(AStream: TStream): IShapeMessage; stdcall;
    function ReadFromBuffer(ABuffer: Pointer; ASize: Word): IShapeMessage; stdcall;
  
    function GetFormatByName(AName: String): IShapeMessage; stdcall;
    function GetFormats(AIndex: Integer): IShapeMessage; stdcall;

    procedure SetFormatByName(AName: String; const Value: IShapeMessage); stdcall;
    procedure SetFormats(AIndex: Integer; const Value: IShapeMessage); stdcall;
    function GetCount: Integer; stdcall;

    procedure Add(AFormatName: String; AFormat: IShapeMessage); stdcall;
    procedure Remove(AFormatName: String); stdcall;
    procedure Clear; stdcall;
    procedure Delete(AIndex: Integer); stdcall;
  end;


implementation

end.
