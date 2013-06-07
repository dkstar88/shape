unit interfaceQueue;

interface

uses Classes, Contnrs;

type
  TInterfaceQueue = class(TObject)
  private
    FList: TInterfaceList;
  protected
    procedure PushItem(AItem: IInterface); virtual;
    function PopItem: IInterface; virtual;
    function PeekItem: IInterface; virtual;
    property List: TInterfaceList read FList;
  public
    constructor Create;
    destructor Destroy; override;

    function Count: Integer;
    function AtLeast(ACount: Integer): Boolean;
    function Push(AItem: IInterface): IInterface;
    function Pop: IInterface;
    function Peek: IInterface;
  end;

implementation

{ TInterfaceQueue }

function TInterfaceQueue.AtLeast(ACount: Integer): Boolean;
begin
  Result := List.Count >= ACount;
end;

function TInterfaceQueue.Count: Integer;
begin
  Result := List.Count;
end;

constructor TInterfaceQueue.Create;
begin
  inherited Create;
  fList := TInterfaceList.Create;
end;

destructor TInterfaceQueue.Destroy;
begin
  fList.Free;
  inherited;
end;

function TInterfaceQueue.Peek: IInterface;
begin
  Result := PeekItem;
end;

function TInterfaceQueue.PeekItem: IInterface;
begin
  Result := List[List.Count-1];
end;

function TInterfaceQueue.Pop: IInterface;
begin
  Result := PopItem;
end;

function TInterfaceQueue.PopItem: IInterface;
begin
  Result := PeekItem;
  List.Delete(List.Count-1);
end;

function TInterfaceQueue.Push(AItem: IInterface): IInterface;
begin
  PushItem(AItem);
  Result := AItem;
end;

procedure TInterfaceQueue.PushItem(AItem: IInterface);
begin
  inherited;
  List.Add(AItem);
end;

end.
