unit messagequeue_intf;

interface

uses shapeIntf;

type
  IMessageQueue = interface
    function Count: Integer;  stdcall;
    function AtLeast(ACount: Integer): Boolean; stdcall;
    function Push(AItem: IShapeMessage): IShapeMessage;  stdcall;
    function Pop: IShapeMessage; stdcall;
    function Peek: IShapeMessage; stdcall;
  end;


implementation


end.
