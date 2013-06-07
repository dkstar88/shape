unit shapeConsts;

interface

const

  SHAPE_ENCODING_COMPRESSED = $01;
  SHAPE_ENCODING_ENCRYPTED = $02;
  SHAPE_ENCODING_NAMEDINDEX = $04;

  ELEMENT_TYPE_SIZE = 1;
  // Shape Element Type IDs
  SHAPE_EMPTY         = $FF;
  SHAPE_BOOL          = $01;
  SHAPE_BOOL_FALSE    = $00;
  SHAPE_BOOL_TRUE     = $01;
  SHAPE_INT16         = $02;
  SHAPE_INT32         = $03;
  SHAPE_INT64         = $04;
  SHAPE_UINT16        = $05;
  SHAPE_UINT32        = $07;
  SHAPE_UINT64        = $08;
  SHAPE_SINGLE        = $09;
  SHAPE_DOUBLE        = $0A;
  SHAPE_STRING        = $10;
  SHAPE_BINARY        = $11;
  SHAPE_UTF8          = $12;
  SHAPE_UTF16         = $13;

  SHAPE_ORDINALS      = [SHAPE_UINT16, SHAPE_INT16, SHAPE_INT32,
      SHAPE_UINT32, SHAPE_UINT64, SHAPE_SINGLE, SHAPE_DOUBLE];

  SHAPE_STRINGS      = [SHAPE_STRING, SHAPE_UTF8, SHAPE_UTF16 ];

  SHAPE_NAMEDINDEX   = $20;  

  // Special routing info
  SHAPE_SPECIAL_START = $40;
  SHAPE_COMMAND       = $40;
  SHAPE_DESTINATION   = $41;
  SHAPE_SENDER_IP     = $42;

  // Special IDs
  SHAPE_MESSAGE_ID    = $50;
  SHAPE_TIMESTAMP     = $51;
  SHAPE_SESSION_ID    = $52;
  SHAPE_PRIORITY      = $53;
  SHAPE_EXPIRATION    = $54;

  SHAPE_SPECIAL_END   = $D0;

  // Custom IDs should start from $80..$DF


  // String Equivlents
  STR_UNKOWN = 'unkown';
  STR_SHAPE_EMPTY = 'empty';
  // Shape Element Types
  STR_SHAPE_BOOL          = 'boolean';
  //STR_SHAPE_BOOL_TRUE     = '';
  STR_SHAPE_INT16         = 'int16';
  STR_SHAPE_INT32         = 'int32';
  STR_SHAPE_INT64         = 'int64';
  STR_SHAPE_UINT16        = 'uint16';
  STR_SHAPE_UINT32        = 'uint32';
  STR_SHAPE_UINT64        = 'uint64';
  STR_SHAPE_SINGLE        = 'single';
  STR_SHAPE_DOUBLE        = 'double';
  STR_SHAPE_STRING        = 'string';
  STR_SHAPE_BINARY        = 'binary';
  STR_SHAPE_UTF8          = 'utf8';
  STR_SHAPE_UTF16         = 'utf16';


  // Special routing info
  STR_SHAPE_COMMAND       = 'command';
  STR_SHAPE_DESTINATION   = 'destination';
  STR_SHAPE_SENDER_IP     = 'sender_ip';

  // Special IDs
  STR_SHAPE_MESSAGE_ID    = 'message_id';
  STR_SHAPE_TIMESTAMP     = 'timestamp';
  STR_SHAPE_SESSION_ID    = 'session_id';

  STR_SHAPE_PRIORITY      = 'priority';
  STR_SHAPE_EXPIRATION    = 'expiration';

  STR_INTERNAL_NAME_PREFIX = '_';  

type
  Int16 = SmallInt;
  Int32 = LongInt;
  UInt16 = Word;
  UInt32 = LongWord;
  UTF16String = WideString;
//  UInt64 = QWord;

  TShapeElementType = byte;


implementation

end.
