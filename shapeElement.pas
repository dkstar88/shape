unit shapeElement;

interface

uses Classes, Contnrs, shapeTypes, shapeConsts, shapeIntf;

type
  TShapeElement = class(TInterfacedObject, IShapeElement)
  private
    fName: String;
//    fIndex: Integer;
    fElementType: TShapeElementType;
    fData: Variant;
  public

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

//    procedure AssignShapeElement(ADest:TShapeElement);
  public
    constructor Create(const AName: String; AElementType: TShapeElementType); overload;
    constructor Create(const AName: String; AElementTypeString: String); overload;

    procedure Assign(ADest: IShapeElement); stdcall;

    function Read(AStream: TStream): Boolean; stdcall;
    function Write(AStream: TStream): Boolean; stdcall;

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

    // Name of element
    property Name: String read GetName write SetName;
    // Element type
    property ElementType: TShapeElementType read GetElementType write SetElementType;
    // Element value in variant type
    property Data: Variant read GetData write SetData;
    // Data length, not including Element type
    property Length: Integer read getLength;
    // Is data is empty
    property IsEmpty: Boolean read getIsEmpty write setIsEmpty;
  end;


implementation

uses Variants;

{ TShapeElement }

constructor TShapeElement.Create(const AName: String;
  AElementType: TShapeElementType);
begin
  inherited Create;
  fName := AName;
  fElementType := AElementType;
//  fData := Null;
end;


procedure TShapeElement.Assign(ADest: IShapeElement); stdcall;
begin
  inherited;
  with ADest do
  begin
    fName := Name;
    fElementType := ElementType;
    fData := Data;
  end;
end;

constructor TShapeElement.Create(const AName: String;
  AElementTypeString: String);
begin
  Create(AName, TSHapeElementUtil.StringToElementType(AElementTypeString));
end;

function TShapeElement.GetAsDouble: Double;  stdcall;
begin
  Result := fData;
end;

function TShapeElement.getAsInt16: Int16; stdcall;
begin
  Result := fData;
end;

function TShapeElement.GetAsInt64: Int64; stdcall;
begin
  Result := fData;
end;

function TShapeElement.GetAsInteger: Integer;  stdcall;
begin
  Result := fData;
end;

function TShapeElement.GetAsSingle: Single; stdcall;
begin
  Result := fData;
end;

function TShapeElement.GetAsString: String; stdcall;
begin
  if VarIsNull(fData) then
    Result := ''
  else
    Result := fData;
end;

function TShapeElement.getAsUInt16: UInt16; stdcall;
begin
  Result := fData;
end;

function TShapeElement.getAsUInt32: UInt32; stdcall;
begin
  Result := fData;
end;

function TShapeElement.GetAsUTF8: WideString; stdcall;
begin
  Result := UTF8Decode(String(fData));
end;

function TShapeElement.GetData: Variant; stdcall;
begin
  Result := fData;
end;

function TShapeElement.GetElementType: TShapeElementType;  stdcall;
begin
  Result := fElementType;
end;

function TShapeElement.getIsEmpty: Boolean;  stdcall;
begin
  if (fElementType in SHAPE_STRINGS) or (VarIsStr(fData)) then
    Result := (fData = '')
  else if (fElementType in SHAPE_ORDINALS) or (VarIsOrdinal(fData)) then
    Result := (fData = 0)
  else
    Result := VarIsEmpty(fData);
end;

function TShapeElement.getLength: Integer; stdcall;
begin
  if IsEmpty then
    Result := 0
  else
    Result := ShapeGetElementLength(fElementType, fData);
end;

function TShapeElement.GetName: String; stdcall;
begin
  Result := fName;
end;

function TShapeElement.Read(AStream: TStream): Boolean; stdcall;
begin
  Result := ShapeReadElement(fElementType, AStream, fData);
end;

procedure TShapeElement.SetAsDouble(const Value: Double); stdcall;
begin
  fData := Value;
end;

procedure TShapeElement.SetAsInt16(const Value: Int16);stdcall;
begin
  fData := Value
end;

procedure TShapeElement.SetAsInt64(const Value: Int64); stdcall;
begin
  fData := Value
end;

procedure TShapeElement.SetAsInteger(const Value: Integer);stdcall;
begin
  fData := Value
end;

procedure TShapeElement.SetAsSingle(const Value: Single);stdcall;
begin
  fData := Value
end;

procedure TShapeElement.SetAsString(const Value: String);stdcall;
begin
  fData := Value
end;

procedure TShapeElement.setAsUInt16(const Value: UInt16);stdcall;
begin
  fData := Value
end;

procedure TShapeElement.setAsUInt32(const Value: UInt32);stdcall;
begin
  fData := Value
end;

procedure TShapeElement.SetAsUTF8(const Value: WideString); stdcall;
begin
  fData := UTF8Encode(Value);
end;

procedure TShapeElement.SetData(const Value: Variant); stdcall;
begin
  FData := Value;
end;

procedure TShapeElement.SetElementType(const Value: TShapeElementType);stdcall;
begin
  FElementType := Value;
end;

procedure TShapeElement.setIsEmpty(const Value: Boolean);stdcall;
begin
  fData := Unassigned;
end;

procedure TShapeElement.SetName(const Value: String);stdcall;
begin
  FName := Value;
end;

function TShapeElement.Write(AStream: TStream): Boolean;stdcall;
begin
  Result := ShapeWriteElement(fElementType, AStream, fData);
end;

end.
