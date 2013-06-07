unit shapeCodec;

interface

uses SysUtils, Classes, Variants, Contnrs, shapeConsts, WinSock,
  shapeTypes, shapeElement, shapeIntf, cFileUtils, cDictionaries;

type
  EShapeMessageException = Exception;

  { TShapeMessage }
  TShapeMessageEncoding = (smeCompressed, smeEncrypted, smeNamedIndex);
  TShapeMessageEncodings = set of TShapeMessageEncoding;

  TShapeMessage = class(TInterfacedObject, IShapeMessage)
  private
    fElements: TInterfaceList;
    FEncoding: Byte;

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

  public
    constructor Create; virtual;
    constructor CreateFromSpec(AFilename: String);

    destructor Destroy; override;

    procedure Assign(ASource: IShapeMessage); stdcall;
    
    procedure LoadSpecFromFile(AFilename: String); stdcall;
    procedure SaveSpecToFile(AFilename: String); stdcall;

    function ReadFromStream(AStream: TStream): Boolean; stdcall;
    function WriteToStream(AStream: TStream): Boolean; stdcall;

    function ReadFromBuffer(ABuffer: Pointer; ASize: Word): Boolean; stdcall;
    function WriteToBuffer(ABuffer: Pointer; var ASize: Word): Boolean; stdcall;

    procedure LoadFromFile(AFilename: String); stdcall;
    procedure SaveToFile(AFilename: String); stdcall;
    
    function AsText: WideString; stdcall;

    function CalcSize: Integer;  stdcall;
    procedure Clear; stdcall;

    function Add(const AName: String;
      const AValueType: TShapeElementType = SHAPE_STRING): IShapeElement; stdcall;

    function AddInternal(const AValueType: TShapeElementType;
      AValue: Variant): IShapeElement; stdcall;

    function NameExists(AName: String): Boolean; stdcall;
    function NameIndex(AName: String): Integer; stdcall;

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

  TShapeFormats = class(TInterfacedObject, IShapeFormats)
  private
    fFormats: TInterfaceDictionary;
    FCount: Integer;
    
    function GetFormatByName(AName: String): IShapeMessage; stdcall;
    function GetFormats(AIndex: Integer): IShapeMessage; stdcall;
    procedure SetFormatByName(AName: String; const Value: IShapeMessage); stdcall;
    procedure SetFormats(AIndex: Integer; const Value: IShapeMessage); stdcall;
    function GetCount: Integer; stdcall;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(AFormatName: String; AFormat: IShapeMessage); stdcall;
    procedure Remove(AFormatName: String); stdcall;
    procedure Clear; stdcall;
    procedure Delete(AIndex: Integer); stdcall;

    function DetectMessageFormat(AStream: TStream): String; stdcall;
    function ReadFromStream(AStream: TStream): IShapeMessage; stdcall;
    function ReadFromBuffer(ABuffer: Pointer; ASize: Word): IShapeMessage; stdcall;

//    procedure MessageFromStream(AStream: TStream): IShapeMessage;
    
    property Formats[AIndex: Integer]: IShapeMessage read GetFormats write SetFormats;
    property FormatByName[AName: String]: IShapeMessage read GetFormatByName write SetFormatByName;
    property Count: Integer read GetCount;
  end;

  function newMessage: IShapeMessage; stdcall;
  function cloneMessage(AMessage: IShapeMessage): IShapeMessage; stdcall;
  function newMessageFromSpec(ASpecFile: String): IShapeMessage; stdcall;

implementation

uses IniFiles, shapeEncodings;

function cloneMessage(AMessage: IShapeMessage): IShapeMessage; stdcall;
begin
  Result := TShapeMessage.Create;
  Result.Assign(AMessage);
end;

function newMessageFromSpec(ASpecFile: String): IShapeMessage; stdcall;
begin
  Result := TShapeMessage.CreateFromSpec(ASpecFile);
end;

function newMessage: IShapeMessage; stdcall;
begin
  Result := TShapeMessage.Create;
end;

function EncodingToByte(AEncodings:TShapeMessageEncodings): Byte;
begin
  Result := 0;
  if smeCompressed in AEncodings then
    Result := Result and SHAPE_ENCODING_COMPRESSED;

  if smeEncrypted in AEncodings then
    Result := Result and SHAPE_ENCODING_ENCRYPTED;

  if smeEncrypted in AEncodings then
    Result := Result and SHAPE_ENCODING_NAMEDINDEX;

end;

function ByteToEncoding(AEncodings: Byte): TShapeMessageEncodings;
begin
  Result := [];
  if (AEncodings and SHAPE_ENCODING_COMPRESSED) = SHAPE_ENCODING_COMPRESSED then
    Result := Result + [smeCompressed];
  if (AEncodings and SHAPE_ENCODING_ENCRYPTED) = SHAPE_ENCODING_ENCRYPTED then
    Result := Result + [smeEncrypted];
  if (AEncodings and SHAPE_ENCODING_NAMEDINDEX) = SHAPE_ENCODING_NAMEDINDEX then
    Result := Result + [smeNamedIndex];

end;

{ TShapeDescriptor }

function TShapeMessage.AddInternal(const AValueType: TShapeElementType;
  AValue: Variant): IShapeElement;
var
  fName: String;
begin
  fName := STR_INTERNAL_NAME_PREFIX + TShapeElementUtil.ElementTypeToString(AValueType);
  if ElementByName[fName]=nil then
  begin
    Add(fName, AValueType);
  end;
  Values[fName] := AValue;
end;

procedure TShapeMessage.Assign(ASource: IShapeMessage);
var
  i: Integer;
  SrcEle, Ele: IShapeElement;
begin
  if ASource = nil then
  	Clear
  else
  begin
    for I := 0 to ASource.Count - 1 do
    begin
      SrcEle := ASource.Elements[i];
      Ele := Add(SrcEle.Name, SrcEle.ElementType);
      Ele.Data := SrcEle.Data;
    end;
    FEncoding := ASource.Encoding;
  end;
end;

function TShapeMessage.AsText: WideString;
var
	i: Integer;
begin
	Result := '';
	for I := 0 to Count - 1 do
  begin
  	if VarIsNull(Elements[i].Data) or VarIsEmpty(Elements[i].Data) then
	    Result := Result + Format('%s=', [Elements[i].Name])
    else
	    Result := Result + Format('%s=%s', [Elements[i].Name, Elements[i].Data]);

    if i < Count then Result := Result + ';';
    
  end;
end;

function TShapeMessage.CalcSize: Integer; stdcall;
var
  i: Integer;
begin
  Result := 3;
  for I := 0 to Count - 1 do
  begin
    // Special elements Empty, Boolean takes 1 byte
    if Elements[i].ElementType in [$00, $01, $FF] then
      Inc(Result)
    else // Other types are calculated, with size of element type added
      Inc(Result, Elements[i].Length+ELEMENT_TYPE_SIZE);

    if (SHAPE_ENCODING_NAMEDINDEX and fEncoding) = SHAPE_ENCODING_NAMEDINDEX then
      Inc(Result, ShapeGetElementLength(SHAPE_NAMEDINDEX, Elements[i].Name));
    
  end;
end;

procedure TShapeMessage.Clear;
begin
	fElements.Clear;
end;

constructor TShapeMessage.Create;
begin
  inherited Create;
  fElements := TInterfaceList.Create;
//  fElements.OwnsObjects := True;
end;

constructor TShapeMessage.CreateFromSpec(AFilename: String);
begin
  Create;
  LoadSpecFromFile(AFilename);
end;

function TShapeMessage.Add(const AName: String;
  const AValueType: TShapeElementType): IShapeElement;  stdcall;
begin
  Result := ElementByName[AName];
  if Result = nil then
    Result := TShapeElement.Create(AName, AValueType);
  fElements.Add(Result);
end;

procedure TShapeMessage.Delete(AIndex: Integer); stdcall;
begin
  fElements.Delete(AIndex);
end;

destructor TShapeMessage.Destroy;
begin
  fElements.Free;
  inherited;
end;

function TShapeMessage.GetAsBoolean(AName: String): Boolean;
var
  v: Variant;
begin
  v := GetValues(AName);  
  if VarIsNull(v) then
    Result := False
  else
    Result := GetValues(AName);
end;

function TShapeMessage.GetAsInteger(AName: String): Integer;
var
  v: Variant;
begin
  v := GetValues(AName);  
  if VarIsNull(v) then
    Result := 0
  else
    Result := GetValues(AName);
end;
function TShapeMessage.GetAsString(AName: String): String;
var
  v: Variant;
begin
  v := GetValues(AName);  
  if VarIsNull(v) then
    Result := ''
  else
    Result := GetValues(AName);
end;

function TShapeMessage.getCount: Integer; stdcall;
begin
  Result := fElements.Count;
end;

function TShapeMessage.GetElementByName(AName: String): IShapeElement;stdcall;
var
  i: Integer;
begin
  i := NameIndex(AName);
  if i >= 0 then
    Result := Elements[i]
  else
    Result := nil;
end;

function TShapeMessage.GetElements(AIndex: Integer): IShapeElement;stdcall;
begin
  Result := IShapeElement(fElements[AIndex]);
end;

function TShapeMessage.GetEncoding: Byte; stdcall;
begin
  Result := FEncoding;
end;

function TShapeMessage.GetValues(AName: String): Variant; stdcall;
var
  ele: IShapeElement;
begin
  ele := ElementByName[AName];
  if ele = nil then
    Result := Unassigned
  else
    Result := ele.Data;
end;

procedure TShapeMessage.SetAsBoolean(AName: String; const Value: Boolean);
begin
  SetValues(AName, Value);
end;

procedure TShapeMessage.SetAsInteger(AName: String; const Value: Integer);
begin
  SetValues(AName, Value);
end;

procedure TShapeMessage.SetAsString(AName: String; const Value: String);
begin
  SetValues(AName, Value);
end;

procedure TShapeMessage.SetElementByName(AName: String;
  const Value: IShapeElement);  stdcall;
begin
  ElementByName[AName].Assign(Value);
end;

procedure TShapeMessage.SetElements(AIndex: Integer;
  const AValue: IShapeElement);  stdcall;
begin
  Elements[Aindex].Assign(AValue);
end;

procedure TShapeMessage.SetEncoding(const Value: Byte); stdcall;
begin
  FEncoding := Value;
end;

procedure TShapeMessage.SetValues(AName: String; const Value: Variant);
var
  ele: IShapeElement;
begin
  ele := ElementByName[AName];
  if ele <> nil then
    ele.Data := Value;
end;

type
  TShapeStreamProcess = procedure(inpStream, outStream: TStream; const ASize: Cardinal = 0);

function _StreamProcess(Processor: TShapeStreamProcess; AStream: TMemoryStream): Integer;
var
  tmp2: TMemoryStream;
begin
  Result := 0;
  tmp2 := TMemoryStream.Create;
  try
    AStream.Seek(0, 0);
    Processor(AStream, tmp2);
    tmp2.Seek(0, 0);
    AStream.Clear;
    AStream.LoadFromStream(tmp2);
    Result := AStream.Size;
  finally
    tmp2.Free;
  end;
end;

function TShapeMessage.WriteToBuffer(ABuffer: Pointer; var ASize: Word): Boolean; stdcall;
var
  Mem: TMemoryStream;
begin
  if ABuffer = nil then
  begin
    ASize := CalcSize;
    Result := False;
  end else
  begin
    Mem := TMemoryStream.Create;
    try
      Result := WriteToStream(Mem);
      Move(ABuffer, Mem.Memory^, Mem.Size);
    finally
      Mem.Free;
    end;
  end;
end;

function TShapeMessage.WriteToStream(AStream: TStream): Boolean; stdcall;
var
  i: Integer;
  msgsize: Word;
  option: Byte;
  eletype, namedKeyType, emptyelement, booltrue, boolfalse: Byte;
  tmpStream, tmp2: TMemoryStream;
  IsNamedKey: Boolean;

  procedure PostWrite;
  begin
    {$IFDEF ENCODING_COMPRESSION}
    if (FEncoding and SHAPE_ENCODING_COMPRESSED) = SHAPE_ENCODING_COMPRESSED then
    begin
      _StreamProcess(@CompressStream, tmpStream);
    end;
    {$ENDIF}

    tmpStream.Seek(0, 0);
    if (FEncoding and SHAPE_ENCODING_ENCRYPTED) = SHAPE_ENCODING_ENCRYPTED then
    begin
      _StreamProcess(@EncryptStream, tmpStream);
    end;

    // Calculate message size
    msgsize := tmpStream.Size;
    // Write message size
    AStream.Write(msgsize, SizeOf(msgSize));

    // Write encoding options
    option := fEncoding;
    AStream.Write(option, SizeOf(option));

    tmpStream.Seek(0, 0);
    AStream.Write(tmpStream.Memory^, msgsize);  
  end;

  procedure WriteAllElements;
  var
    I: Integer;
  begin
    // Write all elements
    for I := 0 to Count - 1 do
    begin

      if IsNamedKey and (Elements[i].ElementType < SHAPE_SPECIAL_START) then
      begin
        tmpStream.Write(namedKeyType, ELEMENT_TYPE_SIZE);      
        // Write Key name
        ShapeWriteElement(namedKeyType, tmpStream, Elements[i].Name);
      end;

      // Handle empty elements
      if Elements[i].IsEmpty then
      begin
        tmpStream.Write(emptyelement, ELEMENT_TYPE_SIZE);
      end
      else
      begin
        // Handle special boolean, takes 1 byte
        if Elements[i].ElementType in [$00, $01] then
        begin
          if Boolean(Elements[i].Data) then
            tmpStream.Write(booltrue, ELEMENT_TYPE_SIZE)
          else
            tmpStream.Write(boolfalse, ELEMENT_TYPE_SIZE);
        end
        else
        begin
          // Normal element types
          eletype := Elements[i].ElementType;
          tmpStream.Write(eletype, ELEMENT_TYPE_SIZE);
          Elements[i].Write(tmpStream);
        end;
      end;
    end;  
  end;

begin
  booltrue := SHAPE_BOOL_TRUE; boolfalse := SHAPE_BOOL_FALSE;
  emptyelement := SHAPE_EMPTY;
  namedKeyType := SHAPE_NAMEDINDEX;
  tmpStream := TMemoryStream.Create;
  tmp2 := TMemoryStream.Create;
  IsNamedKey := (FEncoding and SHAPE_ENCODING_NAMEDINDEX) = SHAPE_ENCODING_NAMEDINDEX;
  Result := False;
  try

    // Write all elements to stream;
    WriteAllElements;

    // Encode message and write message size, encoding
    PostWrite;

    Result := True;
  finally
    tmpStream.Free;
    tmp2.Free;
  end;

end;

procedure TShapeMessage.LoadFromFile(AFilename: String); stdcall;
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFilename, fmOpenRead);
  try
    ReadFromStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TShapeMessage.LoadSpecFromFile(AFilename: String); stdcall;
var
  fIni: TIniFile;
  fNames: TStrings;
  i:Integer;
  fSection: String;
begin
  fIni := TIniFile.Create(AFilename);
  fNames := TStringList.Create;
  try
    fSection := 'format';
    fIni.ReadSection(fSection, fNames);
    for i := 0 to fNames.Count-1 do
    begin
      Add(fNames[i],
        TShapeElementUtil.StringToElementType(
          fIni.ReadString(fSection, fNames[i], 'string'))
      );
    end;
  finally
    fNames.Free;
    fIni.Free;
  end;
end;

function TShapeMessage.NameExists(AName: String): Boolean;
begin
  Result := NameIndex(AName) >= 0;
end;

function TShapeMessage.NameIndex(AName: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
  begin
    if CompareStr(Elements[i].Name, AName)=0 then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TShapeMessage.ReadFromBuffer(ABuffer: Pointer; ASize: Word): Boolean; stdcall;
var
  Mem: TMemoryStream;
begin
  Mem := TMemoryStream.Create;
  try
    Mem.Read(ABuffer^, ASize);
    Result := ReadFromStream(Mem);
  finally
    Mem.Free;
  end;
end;

function TShapeMessage.ReadFromStream(AStream: TStream): Boolean;stdcall;

  procedure ReadMetaInfo(AStream: TStream;
    AElementType: TShapeElementType);
  var
//    fName: String;
    fValue: Variant;
  begin
    if (AStream.Position < AStream.Size) and ShapeReadElement(AElementType, AStream, fValue) then
    begin
      AddInternal(AElementType, fValue);
//      Values[fName] := value;
    end;
  end;

var
  fItemIndex: Integer;
  msgsize: Word;
  elementtype: TShapeElementType;
  option: Byte;
  tmpStream, tmp2: TMemoryStream;
  fnamedKey: Variant;
  fKeyNamed: Boolean;
  ele: IShapeElement;

  procedure _ReadElement;
  begin
    if elementtype=SHAPE_EMPTY then
      ele.IsEmpty := True
    else if elementtype=SHAPE_BOOL_TRUE then
    begin
      ele.Data := True;
      ele.ElementType := SHAPE_BOOL;
    end else if elementtype=SHAPE_BOOL_FALSE then
    begin
      ele.Data := False;
      ele.ElementType := SHAPE_BOOL;      
    end else if ele.ElementType = elementtype then
    begin
      ele.Read(tmpStream);
    end else
    begin
      // Element type mismatch
      raise EShapeMessageException.Create('Element ' + ele.Name + ' type mismatch');
    end;
  end;
  
  procedure ReadElement;
  var
    val: Variant;
  begin
    if fItemIndex < Count then
    begin
      ele := Elements[fItemIndex];
      _ReadElement;
      Inc(fItemIndex);
    end else
    begin
      // Read dummy data, make sure stream cursor move on correctly
      ShapeReadElement(elementtype, tmpStream, val);
    end;
  end;

  procedure ReadNamedElement;
  var
    i: Integer;
  begin
    i := NameIndex(fnamedKey);
    if i >= 0 then
      Ele := Elements[i]
    else
      Ele := Add(fnamedKey, elementtype);
    _ReadElement;
  end;

  procedure Preprocess;
  begin
    {$IFDEF ENCODING_COMPRESSION}
    if (FEncoding and SHAPE_ENCODING_COMPRESSED) = SHAPE_ENCODING_COMPRESSED then
    begin
      _StreamProcess(@DecompressStream, tmpStream);
    end;
    {$ENDIF}

    tmpStream.Seek(0, 0);
    if (FEncoding and SHAPE_ENCODING_ENCRYPTED) = SHAPE_ENCODING_ENCRYPTED then
    begin
      _StreamProcess(@DecryptStream, tmpStream);
    end;

    fKeyNamed := (FEncoding and SHAPE_ENCODING_NAMEDINDEX) = SHAPE_ENCODING_NAMEDINDEX;

  end;  

begin
  tmpStream := TMemoryStream.Create;
  tmp2 := TMemoryStream.Create;
  Result := False;
  try
    // Write all elements
    AStream.Read(msgsize, sizeof(msgsize));
    AStream.Read(option, SizeOf(Option));
    tmpStream.SetSize(msgSize);
    AStream.Read(tmpStream.Memory^, msgsize);

    FEncoding := option;

    Preprocess;
    fNamedKey := '';
    fItemIndex := 0;
    while tmpStream.Position < tmpStream.Size do
    begin
      tmpStream.Read(elementtype, SizeOf(elementtype));
      if (elementtype = SHAPE_NAMEDINDEX) then
      begin
        ShapeReadElement(SHAPE_NAMEDINDEX, tmpStream, fNamedKey);
      end else if (elementtype >= SHAPE_SPECIAL_START) and (elementtype <= SHAPE_SPECIAL_END) then
      begin
        ReadMetaInfo(tmpStream, elementtype);
      end else
      begin
        if (fKeyNamed and (fNamedKey <> '')) then
          ReadNamedElement
        else
          ReadElement;
      end;
    end;
    Result := True;
  finally
    tmpStream.Free;
    tmp2.Free;
  end;

end;


procedure TShapeMessage.Remove(AElement: IShapeElement);  stdcall;
begin
  fElements.Remove(AElement)
end;

procedure TShapeMessage.SaveSpecToFile(AFilename: String); stdcall;
var
  fIni: TIniFile;
  i:Integer;
  fSection: String;
begin
  fIni := TIniFile.Create(AFilename);
  try
    fSection := '';
//    fIni.ReadSection(fSection, fNames);
    for i := 0 to Count-1 do
    begin
      fIni.WriteString(fSection, Elements[i].Name,
        TShapeElementUtil.ElementTypeToString(Elements[i].ElementType));
    end;
  finally
    fIni.Free;
  end;
end;


procedure TShapeMessage.SaveToFile(AFilename: String); stdcall;
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFilename, fmCreate);
  try
    WriteToStream(fs);
  finally
    fs.Free;
  end;
end;


{ TShapeFormats }

procedure TShapeFormats.Add(AFormatName: String; AFormat: IShapeMessage);
begin
  fFormats.Add(AFormatName, AFormat);
end;

procedure TShapeFormats.Clear;
begin
  fFormats.Clear;
end;

constructor TShapeFormats.Create;
begin
  inherited Create;
  fFormats := TInterfaceDictionary.Create;
end;

procedure TShapeFormats.Delete(AIndex: Integer);
begin
  fFormats.DeleteItemByIndex(AIndex);
end;

destructor TShapeFormats.Destroy;
begin
  fFormats.Free;
  inherited Destroy;
end;

function TShapeFormats.DetectMessageFormat(AStream: TStream): String;
begin
  Result := '';
  with TShapeMessage.Create do
  begin
    try
      // Read internal elements
      ReadFromStream(AStream);
      
      // Use command name as format name
      Result := ElementByName['_command'].AsString;

    finally
      Free;
    end;
  end;

end;


function TShapeFormats.GetCount: Integer;
begin
  Result := fCount;
end;

function TShapeFormats.GetFormatByName(AName: String): IShapeMessage;
//var
//  v: IInterface;
begin
  Result := IShapeMessage(fFormats[AName]);
//  Result := v as IShapeMessage;
end;

function TShapeFormats.GetFormats(AIndex: Integer): IShapeMessage;
begin
  Result := IShapeMessage(fFormats.GetItemByIndex(AIndex));
end;


function TShapeFormats.ReadFromBuffer(ABuffer: Pointer; ASize: Word): IShapeMessage;
var
  Mem: TMemoryStream;
begin
  Mem := TMemoryStream.Create;
  try
    Mem.Read(ABuffer^, ASize);
    Result := ReadFromStream(Mem);
  finally
    Mem.Free;
  end;
end;

function TShapeFormats.ReadFromStream(AStream: TStream): IShapeMessage;
var
  fFormatname: String;
  fPos: Int64;
begin
  fPos := AStream.Position;
  fFormatname := DetectMessageFormat(AStream);
  Result := TShapeMessage.Create;
  Result.Assign(FormatByName[fFormatname]);
  AStream.Position := fPos;  
  Result.ReadFromStream(AStream);
end;

procedure TShapeFormats.Remove(AFormatName: String);
begin
  fFormats.Delete(AFormatName);
end;

procedure TShapeFormats.SetFormatByName(AName: String;
  const Value: IShapeMessage);
begin
  fFormats[AName] := Value;
end;

procedure TShapeFormats.SetFormats(AIndex: Integer; const Value: IShapeMessage);
begin
  fFormats.SetItemByIndex(AIndex, Value);
end;

end.
