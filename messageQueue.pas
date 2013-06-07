unit messageQueue;

interface

uses Classes, Contnrs, shapeIntf, interfaceQueue, shapeclient_intf, messagequeue_intf;

type
  TMessageQueue = class(TInterfacedObject, IMessageQueue)
  private
    fList: TInterfaceQueue;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
      
  public
    function Count: Integer;  stdcall;
    function AtLeast(ACount: Integer): Boolean; stdcall;
    function Push(AItem: IShapeMessage): IShapeMessage;  stdcall;
    function Pop: IShapeMessage; stdcall;
    function Peek: IShapeMessage; stdcall;
  end;

  function newMessageQueue: IMessageQueue; stdcall;


implementation

function newMessageQueue: IMessageQueue; stdcall;
begin
  Result := TMessageQueue.Create;
end;

{ TMessageQueue }

procedure TMessageQueue.AfterConstruction;
begin
  inherited;
  fList := TInterfaceQueue.Create;
end;

function TMessageQueue.AtLeast(ACount: Integer): Boolean;
begin
  Result := fList.AtLeast(ACOunt);
end;

procedure TMessageQueue.BeforeDestruction;
begin
  fList.Free;
  inherited;

end;

function TMessageQueue.Count: Integer;
begin
  Result := fList.Count;
end;

function TMessageQueue.Peek: IShapeMessage;
begin
  Result := IShapeMessage(fList.Peek);
end;

function TMessageQueue.Pop: IShapeMessage;
begin
  Result := IShapeMessage(fList.Pop);
end;

function TMessageQueue.Push(AItem: IShapeMessage): IShapeMessage;
begin
  Result := IShapeMessage(fList.Push(AItem));
end;

end.
