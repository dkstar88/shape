unit shapeEncodings;

interface

uses Classes;

{$IFDEF ENCODING_COMPRESSION}
{ Compress a stream }
procedure CompressStream(inpStream, outStream: TStream; const ASize: Cardinal = 0);
{ Decompress a stream }
procedure DecompressStream(inpStream, outStream: TStream; const ASize: Cardinal = 0);
{$ENDIF}

{ Encrypt a stream }
procedure EncryptStream(inpStream, outStream: TStream; const ASize: Cardinal = 0);

{ Decrypt a stream }
procedure DecryptStream(inpStream, outStream: TStream; const ASize: Cardinal = 0);


implementation


{$IFDEF ENCODING_COMPRESSION}
uses Zlib;
{$ENDIF}


{ Compress a stream }
procedure EncryptStream(inpStream, outStream: TStream; const ASize: Cardinal = 0);
begin

end;

{ Decompress a stream }
procedure DecryptStream(inpStream, outStream: TStream; const ASize: Cardinal = 0);
begin

end;

{$IFDEF ENCODING_COMPRESSION}
{ Compress a stream }
procedure CompressStream(inpStream, outStream: TStream; const ASize: Cardinal = 0);
var
  InpBuf, OutBuf: Pointer;
  InpBytes, OutBytes: Integer;
  inSize: Int64;
begin
  InpBuf := nil;
  OutBuf := nil;
  try
    if ASize=0 then
      inSize := inpStream.Size-inpStream.Position
    else
      inSize := ASize;
    GetMem(InpBuf, inSize);
//    inpStream.Position := 0;
    InpBytes := inpStream.Read(InpBuf^, inSize);
    CompressBuf(InpBuf, InpBytes, OutBuf, OutBytes);
    outStream.Write(OutBuf^, OutBytes);
  finally
    if InpBuf <> nil then FreeMem(InpBuf);
    if OutBuf <> nil then FreeMem(OutBuf);
  end;
end;


{ Decompress a stream }
procedure DecompressStream(inpStream, outStream: TStream; const ASize: Cardinal = 0);
var
  InpBuf, OutBuf: Pointer;
  OutBytes, sz: Integer;
begin
  InpBuf := nil;
  OutBuf := nil;
  if ASize=0 then
    sz := inpStream.Size - inpStream.Position
  else
    sz := ASize;
  if sz > 0 then
    try
      GetMem(InpBuf, sz);
      inpStream.Read(InpBuf^, sz);
      DecompressBuf(InpBuf, sz, 0, OutBuf, OutBytes);
      outStream.Write(OutBuf^, OutBytes);
    finally
      if InpBuf <> nil then FreeMem(InpBuf);
      if OutBuf <> nil then FreeMem(OutBuf);
    end;
  outStream.Position := 0;
end;

{$ENDIF}

end.
