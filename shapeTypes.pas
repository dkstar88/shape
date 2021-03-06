unit shapeTypes;

interface

uses SysUtils, Classes, shapeConsts, Variants;

type
  TShapeElementRead = function(AStream: TStream; var AOut: Variant): Boolean;
  TShapeElementWrite = function(AStream: TStream; AIn: Variant): Boolean;
  TShapeElementLength = function (AIn: Variant): Integer;
  TShapeElementTypeHandler = class
    ElementType: TShapeElementType;
    ReadHandler: TShapeElementRead;
    WriteHandler: TShapeElementWrite;
    GetLength: TShapeElementLength;
    DefaultLength: Integer;
  end;

  TShapeElementTypeHandlers = class(TObject)
  private
    fHandlers: array[$00..$FF] of TShapeElementTypeHandler;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const AElementType: TShapeElementType;
      const AReadHandler: TShapeElementRead;
      const AWriteHandler: TShapeElementWrite;
      const ADefaultLength: Integer = 0;
      const AGetLengthHandler: TShapeElementLength = nil
      );
    function Read(const AElementType: TShapeElementType;
      const AStream: TStream;
      var AOut: Variant): Boolean;
    function Write(const AElementType: TShapeElementType;
      const AStream: TStream;
      AIn: Variant): Boolean;
    function GetSize(const AElementType: TShapeElementType; AIn: Variant): Integer;
  end;

  { TShapeElementUtil }
  TShapeElementUtil = class
    class function ElementTypeToString (AType: TShapeElementType): String;
    class function StringToElementType (AString: String): TShapeElementType;
  end;

  procedure RegisterShapeElementTypeHandler(const AElementType: TShapeElementType;
      const AReadHandler: TShapeElementRead;
      const AWriteHandler: TShapeElementWrite;
      const ADefaultLength: Integer = 0;
      const AGetLengthHandler: TShapeElementLength = nil);


  function ShapeGetElementLength(const AElementType: TShapeElementType;
     AIn: Variant): Integer;

  function ShapeReadElement(const AElementType: TShapeElementType;
      const AStream: TStream;
      var AOut: Variant): Boolean;

  function ShapeWriteElement(const AElementType: TShapeElementType;
      const AStream: TStream;
      AIn: Variant): Boolean;


implementation

{ TShapeElementUtil }
class function TShapeElementUtil.ElementTypeToString(AType: TShapeElementType
  ): String;
begin
  case AType of
  SHAPE_EMPTY         : Result := STR_SHAPE_EMPTY;
  SHAPE_BOOL_FALSE    : Result := STR_SHAPE_BOOL;
  SHAPE_BOOL_TRUE     : Result := STR_SHAPE_BOOL;
  SHAPE_INT16         : Result := STR_SHAPE_INT16;
  SHAPE_INT32         : Result := STR_SHAPE_INT32;
  SHAPE_INT64         : Result := STR_SHAPE_INT64;
  SHAPE_UINT16        : Result := STR_SHAPE_UINT16;
  SHAPE_UINT32        : Result := STR_SHAPE_UINT32;
  SHAPE_UINT64        : Result := STR_SHAPE_UINT64;
  SHAPE_SINGLE        : Result := STR_SHAPE_SINGLE;
  SHAPE_DOUBLE        : Result := STR_SHAPE_DOUBLE;
  SHAPE_STRING        : Result := STR_SHAPE_STRING;
  SHAPE_BINARY        : Result := STR_SHAPE_BINARY;
  SHAPE_UTF8          : Result := STR_SHAPE_UTF8;
  SHAPE_UTF16         : Result := STR_SHAPE_UTF16;

  // Special routing info
  SHAPE_COMMAND       : Result := STR_SHAPE_COMMAND;
  SHAPE_DESTINATION   : Result := STR_SHAPE_DESTINATION;
  SHAPE_SENDER_IP     : Result := STR_SHAPE_SENDER_IP;

  // Special IDs
  SHAPE_MESSAGE_ID    : Result := STR_SHAPE_MESSAGE_ID;
  SHAPE_TIMESTAMP     : Result := STR_SHAPE_TIMESTAMP;
  SHAPE_SESSION_ID    : Result := STR_SHAPE_SESSION_ID;
  SHAPE_PRIORITY      : Result := STR_SHAPE_PRIORITY;
  SHAPE_EXPIRATION    : Result := STR_SHAPE_EXPIRATION;

  else
    Result := STR_UNKOWN;
  end;
end;

class function TShapeElementUtil.StringToElementType(AString: String
  ): TShapeElementType;
var
  lstr: String;
begin
  lstr := Lowercase(AString);
  if lstr = STR_SHAPE_BOOL then Result := SHAPE_BOOL
  else if lstr = STR_SHAPE_INT16 then Result := SHAPE_INT16
  else if lstr = STR_SHAPE_INT32 then Result := SHAPE_INT32
  else if lstr = STR_SHAPE_INT64 then Result := SHAPE_INT64
  else if lstr = STR_SHAPE_UINT16 then Result := SHAPE_UINT16
  else if lstr = STR_SHAPE_UINT32 then Result := SHAPE_UINT32
  else if lstr = STR_SHAPE_UINT16 then Result := SHAPE_UINT64
  else if lstr = STR_SHAPE_SINGLE then Result := SHAPE_SINGLE
  else if lstr = STR_SHAPE_DOUBLE then Result := SHAPE_DOUBLE
  else if lstr = STR_SHAPE_STRING then Result := SHAPE_STRING
  else if lstr = STR_SHAPE_BINARY then Result := SHAPE_BINARY
  else if lstr = STR_SHAPE_UTF8 then Result := SHAPE_UTF8
  else if lstr = STR_SHAPE_UTF16 then Result := SHAPE_UTF16
  else if lstr = STR_SHAPE_COMMAND then Result := SHAPE_COMMAND
  else if lstr = STR_SHAPE_DESTINATION then Result := SHAPE_DESTINATION
  else if lstr = STR_SHAPE_MESSAGE_ID then Result := SHAPE_MESSAGE_ID
  else if lstr = STR_SHAPE_TIMESTAMP then Result := SHAPE_TIMESTAMP
  else if lstr = STR_SHAPE_SESSION_ID then Result := SHAPE_SESSION_ID
  else if lstr = STR_SHAPE_SENDER_IP then Result := SHAPE_SENDER_IP
  else if lstr = STR_SHAPE_EXPIRATION then Result := SHAPE_EXPIRATION
  else if lstr = STR_SHAPE_PRIORITY then Result := SHAPE_PRIORITY
  ;
end;

{ TShapeElementTypeHandlers }

procedure TShapeElementTypeHandlers.Add(const AElementType: TShapeElementType;
  const AReadHandler: TShapeElementRead;
  const AWriteHandler: TShapeElementWrite; const ADefaultLength: Integer = 0;
      const AGetLengthHandler: TShapeElementLength = nil);
begin
  if fHandlers[AElementType]=nil then
  begin
    fHandlers[AElementType] := TShapeElementTypeHandler.Create;
    fHandlers[AElementType].ElementType := AElementType;
  end;
  fHandlers[AElementType].ReadHandler := AReadHandler;
  fHandlers[AElementType].WriteHandler := AWriteHandler;
  fHandlers[AElementType].GetLength :=  AGetLengthHandler;
  fHandlers[AElementType].DefaultLength := ADefaultLength;
end;

constructor TShapeElementTypeHandlers.Create;
begin
  inherited Create;
  FillChar(fHandlers, SizeOf(Fhandlers), #0);
end;

destructor TShapeElementTypeHandlers.Destroy;
var
  i: Integer;
begin
  for I := Low(fHandlers) to High(fHandlers) do
  begin
    if fHandlers[i] <> nil then
      FreeAndNil(fHandlers[i]);
  end;
  inherited;
end;

function TShapeElementTypeHandlers.GetSize(
  const AElementType: TShapeElementType; AIn: Variant): Integer;
begin
  if (fHandlers[AElementType] <> nil) then
  begin
    if Assigned(fHandlers[AElementType].GetLength) then
      Result := fHandlers[AElementType].GetLength(AIn)
    else
      Result := fHandlers[AElementType].DefaultLength;
  end;
end;

function TShapeElementTypeHandlers.Read(const AElementType: TShapeElementType;
  const AStream: TStream;
  var AOut: Variant): Boolean;
begin
  if ((fHandlers[AElementType] <> nil) and
    (Assigned(fHandlers[AElementType].ReadHandler))) then
  begin
    Result := fHandlers[AElementType].ReadHandler(AStream, AOut);
  end;
end;

function TShapeElementTypeHandlers.Write(const AElementType: TShapeElementType;
      const AStream: TStream;
      AIn: Variant): Boolean;
begin
  if ((fHandlers[AElementType] <> nil) and
    (Assigned(fHandlers[AElementType].WriteHandler))) then
  begin
    Result := fHandlers[AElementType].WriteHandler(AStream, AIn);
  end;
end;

// Shape Read Functions
function ReadInt16(AStream: TStream; var AValue: Variant): Boolean;
var
  data: Int16;
begin
  data := 0;
  Result := AStream.Read(data, SizeOf(data)) > 0;
  AValue := data;
end;

function ReadInt32(AStream: TStream; var AValue: Variant): Boolean;
var
  data: Int32;
begin
  data := 0;
  Result := AStream.Read(data, SizeOf(data)) > 0;
  AValue := data;
end;

function ReadUInt16(AStream: TStream; var AValue: Variant): Boolean;
var
  data: UInt16;
begin
  data := 0;
  Result := AStream.Read(data, SizeOf(data)) > 0;
  AValue := data;
end;

function ReadUInt32(AStream: TStream; var AValue: Variant): Boolean;
var
  data: UInt32;
begin
  data := 0;
  Result := AStream.Read(data, SizeOf(data)) > 0;
  AValue := data;
end;

function ReadIPV4(AStream: TStream; var AValue: Variant): Boolean;
var
  data: UInt32;
begin
  data := 0;
  Result := AStream.Read(data, SizeOf(data)) > 0;
  AValue := data;
end;

function ReadInt64(AStream: TStream; var AValue: Variant): Boolean;
var
  data: Int64;
begin
  data := 0;
  Result := AStream.Read(data, SizeOf(data)) > 0;
  AValue := data;
end;

function ReadUInt64(AStream: TStream; var AValue: Variant): Boolean;
var
  data: UInt64;
begin
  data := 0;
  Result := AStream.Read(data, SizeOf(data)) > 0;
  AValue := data;
end;


function ReadSingle(AStream: TStream; var AValue: Variant): Boolean;
var
  data: Single;
begin
  data := 0;
  Result := AStream.Read(data, SizeOf(data)) > 0;
  AValue := data;
end;

function ReadDouble(AStream: TStream; var AValue: Variant): Boolean;
var
  data: Double;
begin
  data := 0;
  Result := AStream.Read(data, SizeOf(data)) > 0;
  AValue := data;
end;

function ReadGUID(AStream: TStream; var AValue: Variant): Boolean;
var
  data: TGUID;
begin
  FillChar(data, SizeOf(data), #0);
  Result := AStream.Read(data, SizeOf(data)) > 0;
  AValue := GUIDToString(data);
end;

function ReadString(AStream: TStream; var AValue: Variant): Boolean;
var
  data: String;
  c: Char;
begin
  data := '';
  Result := False;  
  while AStream.Read(c, SizeOf(c))>0 do
  begin
    if (c = #0) then Break;
    data := data + c;
    Result := True;
  end;
  AValue := data;
end;

function ReadUTF8(AStream: TStream; var AValue: Variant): Boolean;
var
  datasize: UInt16;
  data: UTF8String;
begin

  Result := AStream.Read(datasize, SizeOf(datasize)) > 0;
  if datasize > 0 then
  begin
    SetLength(data, datasize);
    AStream.Read(data[1], datasize);
  end;
  AValue := UTF8Decode(data);
end;

function ReadUTF16(AStream: TStream; var AValue: Variant): Boolean;
var
  datasize: Int16;
  data: WideString;
begin
  Result := AStream.Read(datasize, SizeOf(datasize)) > 0;
  if datasize > 0 then
  begin
    SetLength(data, datasize);
    AStream.Read(data, datasize);
  end;
  AValue := data;
end;

function ReadBool(AStream: TStream; var AValue: Variant): Boolean;
var
  data: Byte;
begin
  data := 0;
  Result := AStream.Read(data, SizeOf(data)) > 0;
  AValue := Boolean(data);
end;

// Shape write Functions
function WriteInt16(AStream: TStream; AValue: Variant): Boolean;
var
  data: Int16;
begin
  data := AValue;
  Result := AStream.Write(data, SizeOf(data)) = SizeOf(data);
end;

function WriteInt32(AStream: TStream; AValue: Variant): Boolean;
var
  data: Int32;
begin
  data := AValue;
  Result := AStream.Write(data, SizeOf(data)) = SizeOf(data);
end;

function WriteUInt16(AStream: TStream; AValue: Variant): Boolean;
var
  data: UInt16;
begin
  data := AValue;
  Result := AStream.Write(data, SizeOf(data)) = SizeOf(data);
end;

function WriteUInt32(AStream: TStream; AValue: Variant): Boolean;
var
  data: UINt32;
begin
  data := AValue;
  Result := AStream.Write(data, SizeOf(data)) = SizeOf(data);
end;

function WriteIPV4(AStream: TStream; AValue: Variant): Boolean;
var
  data: UINt32;
begin
  data := AValue;
  Result := AStream.Write(data, SizeOf(data)) = SizeOf(data);
end;

//function WriteIPV6(AStream: TStream; AValue: Variant): Boolean;
//var
//  data: UINt32;
//begin
//  data := AValue;
//  Result := AStream.Write(data, SizeOf(data)) = SizeOf(data);
//end;

function WriteInt64(AStream: TStream; AValue: Variant): Boolean;
var
  data: Int64;
begin
  data := AValue;
  Result := AStream.Write(data, SizeOf(data)) = SizeOf(data);
end;

function WriteUInt64(AStream: TStream; var AValue: Variant): Boolean;
var
  data: UInt64;
begin
  data := AValue;
  Result := AStream.Write(data, SizeOf(data)) = SizeOf(data);
end;


function WriteSingle(AStream: TStream; AValue: Variant): Boolean;
var
  data: Single;
begin
  data := AValue;
  Result := AStream.Write(data, SizeOf(data)) = SizeOf(data);
end;

function WriteDouble(AStream: TStream; AValue: Variant): Boolean;
var
  data: Double;
begin
  data := AValue;
  Result := AStream.Write(data, SizeOf(data)) = SizeOf(data);
end;

function WriteGUID(AStream: TStream; AValue: Variant): Boolean;
var
  data: TGUID;
begin
  if VarIsStr(AValue) then
  begin
    data := StringToGUID(AValue);
    Result := AStream.Write(data, SizeOf(data)) = SizeOf(data);
  end else
    Result := False;
end;

function WriteString(AStream: TStream; AValue: Variant): Boolean;
var
  data: AnsiString;
begin
  data := AnsiString(AValue) + #0;
  Result := AStream.Write(data[1], Length(data)) = SizeOf(data);
end;

function WriteUTF8(AStream: TStream; AValue: Variant): Boolean;
var
  data: UTF8String;
  datasize: UInt16;
begin
  data := UTF8Encode(WideString(AValue));
  datasize := Length(data);
  AStream.Write(datasize, SizeOf(datasize));
  Result := AStream.Write(data[1], datasize) = datasize;
end;

function WriteUTF16(AStream: TStream; AValue: Variant): Boolean;
//var
//  data: Int16;
begin
//  data := AValue;
//  Result := AStream.Write(data, SizeOf(data)) = SizeOf(data);
end;

function WriteBool(AStream: TStream; AValue: Variant): Boolean;
var
  data: Byte;
begin
  data := AValue;
  Result := AStream.Write(data, SizeOf(data)) = SizeOf(data);
end;

function GetStrLen(AIn: Variant): Integer;
var
  Str: String;
begin
  Str := AIn;
  Result := Length(Str);
end;

function GetUTF8StrLen(AIn: Variant): Integer;
var
  Str: UTF8String;
begin
  Str := String(AIn);
  Result := Length(Str) + SizeOf(Word);
end;

function GetUTF16StrLen(AIn: Variant): Integer;
var
  Str: UTF16String;
begin
  Str := AIn;
  Result := Length(Str) + SizeOf(Word);
end;


var
  ShapeElementTypeHandlers: TShapeElementTypeHandlers;

function ShapeReadElement(const AElementType: TShapeElementType;
    const AStream: TStream;
    var AOut: Variant): Boolean;
begin
  Result := ShapeElementTypeHandlers.Read(AElementType, AStream, AOut);
end;

function ShapeWriteElement(const AElementType: TShapeElementType;
    const AStream: TStream;
    AIn: Variant): Boolean;
begin
  Result := ShapeElementTypeHandlers.Write(AElementType, AStream, AIn);
end;

function ShapeGetElementLength(const AElementType: TShapeElementType;
   AIn: Variant): Integer;
begin
  Result := ShapeElementTypeHandlers.GetSize(AElementType, AIn);
end;

procedure RegisterShapeElementTypeHandler(const AElementType: TShapeElementType;
      const AReadHandler: TShapeElementRead;
      const AWriteHandler: TShapeElementWrite;
      const ADefaultLength: Integer = 0;
      const AGetLengthHandler: TShapeElementLength = nil);
begin
  ShapeElementTypeHandlers.Add(AElementType, AReadHandler,
    AWriteHandler, ADefaultLength, AGetLengthHandler);
end;

initialization
  ShapeElementTypeHandlers := TShapeElementTypeHandlers.Create;

  RegisterShapeElementTypeHandler(SHAPE_INT16, @ReadInt16, @WriteInt16, SizeOf(Int16));
  RegisterShapeElementTypeHandler(SHAPE_INT32, @ReadInt32, @WriteInt32, SizeOf(Int32));
  RegisterShapeElementTypeHandler(SHAPE_INT64, @ReadInt64, @WriteInt64, SizeOf(Int64));
  RegisterShapeElementTypeHandler(SHAPE_UINT16, @ReadUInt16, @WriteUInt16, SizeOf(UInt16));
  RegisterShapeElementTypeHandler(SHAPE_UINT32, @ReadUInt32, @WriteUInt32, SizeOf(UInt32));
  RegisterShapeElementTypeHandler(SHAPE_UINT64, @ReadUInt64, @WriteUInt64, SizeOf(UInt64));

  RegisterShapeElementTypeHandler(SHAPE_Single, @ReadSingle, @WriteSingle, SizeOf(Single));
  RegisterShapeElementTypeHandler(SHAPE_Double, @ReadDouble, @WriteDouble, SizeOf(Double));
  
  RegisterShapeElementTypeHandler(SHAPE_String, @ReadString, @WriteString, 0, @GetStrLen);
  RegisterShapeElementTypeHandler(SHAPE_NAMEDINDEX, @ReadString, @WriteString, 0, @GetStrLen);
  RegisterShapeElementTypeHandler(SHAPE_UTF8, @ReadUTF8, @WriteUTF8, 0, @GetUTF8StrLen);

  // Special elements
  RegisterShapeElementTypeHandler(SHAPE_COMMAND, @ReadString, @WriteString, 0, @GetStrLen);
  RegisterShapeElementTypeHandler(SHAPE_DESTINATION, @ReadString, @WriteString, 0, @GetStrLen);
  RegisterShapeElementTypeHandler(SHAPE_MESSAGE_ID, @ReadGUID, @WriteGUID, 16);
  RegisterShapeElementTypeHandler(SHAPE_TIMESTAMP, @ReadUInt64, @WriteUInt64, SizeOf(UInt64));

  RegisterShapeElementTypeHandler(SHAPE_SENDER_IP, @ReadIPV4, @WriteIPV4, SizeOf(UInt32));
  RegisterShapeElementTypeHandler(SHAPE_PRIORITY, @ReadUInt16, @WriteUInt16, SizeOf(UInt16));
  RegisterShapeElementTypeHandler(SHAPE_EXPIRATION, @ReadUInt32, @WriteUInt32, SizeOf(UInt32));


finalization
  ShapeElementTypeHandlers.Free;

end.
