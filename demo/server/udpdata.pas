unit udpdata;

interface

uses SysUtils, Classes, Contnrs, WinSock;

type
  TUDPData = class(TObject)
  private
    FSender: TSockAddrIn;
    FData: TStream;
    procedure SetSender(const Value: TSockAddrIn);
  public
    constructor Create(ASender: TSockAddrIn);
    destructor Destroy; override;

    property Sender: TSockAddrIn read FSender write SetSender;
    property Data: TStream read FData;
  end;

  TOnDataReady = procedure (ASender: TSockAddrIn; AData: TStream; var ARemove: Boolean) of Object;

  TUDPDataManager = class(TObject)
  private
    fDatas: TObjectList;
    FOnDataReady: TOnDataReady;
    function GetDataBySender(ASender: TSockAddrIn): TUDPData;
    function GetDatas(Aindex: Integer): TUDPData;
    procedure SetOnDataReady(const Value: TOnDataReady);
    function getCount: Cardinal;
  protected
    procedure DoDataReady(AUDPData: TUDPData);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Receive(ASender: TSockAddrIn;
      ABuffer: Pointer; ABufsize: Integer; const ALast: Boolean = False);

    property Datas[Aindex: Integer]: TUDPData read GetDatas;
    property DataBySender[ASender: TSockAddrIn]: TUDPData read GetDataBySender;
    property Count: Cardinal read getCount;
    property OnDataReady: TOnDataReady read FOnDataReady write SetOnDataReady;

  end;

implementation

{ TUDPDataManager }

constructor TUDPDataManager.Create;
begin
  inherited Create;
  fDatas := TObjectList.Create;
  fDatas.OwnsObjects := True;
end;

destructor TUDPDataManager.Destroy;
begin
  fDatas.Free;
  inherited;
end;

procedure TUDPDataManager.DoDataReady(AUDPData: TUDPData);
var
  DeleteData: Boolean;
begin
  DeleteData := True;
  if Assigned(fOnDataReady) then
  begin
    fOnDataReady(AUDPData.Sender, AUDPData.Data, DeleteData);
  end;
  if DeleteData then fDatas.Remove(AUDPData);  
end;

function TUDPDataManager.getCount: Cardinal;
begin
  Result := fDatas.Count;
end;

function TUDPDataManager.GetDataBySender(ASender: TSockAddrIn): TUDPData;
var
  i: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    if (Datas[i].Sender.sin_addr.S_addr = ASender.sin_addr.S_addr)
      and (Datas[i].Sender.sin_port = ASender.sin_port) then
    begin
      Result := Datas[i];
      Break;
    end;
  end;
end;

function TUDPDataManager.GetDatas(Aindex: Integer): TUDPData;
begin
  Result := fDatas[AIndex] as TUDPData;
end;

procedure TUDPDataManager.Receive(ASender: TSockAddrIn; ABuffer: Pointer;
  ABufsize: Integer; const ALast: Boolean = False);
var
  Data: TUDPData;
begin
  Data := DataBySender[ASender];
  if Data = nil then
  begin
    Data := TUDPData.Create(ASender);
    fDatas.Add(Data);
  end;
  Data.Data.Write(ABuffer^, ABufSize);
  if ALast then
  begin
    DoDataReady(Data);
  end;

end;

procedure TUDPDataManager.SetOnDataReady(const Value: TOnDataReady);
begin
  FOnDataReady := Value;
end;

{ TUDPData }

constructor TUDPData.Create(ASender: TSockAddrIn);
begin
  inherited Create;
  FSender := ASender;
  fData := TMemoryStream.Create;
end;

destructor TUDPData.Destroy;
begin
  fData.Free;
  inherited;
end;

procedure TUDPData.SetSender(const Value: TSockAddrIn);
begin
  FSender := Value;
end;

end.
