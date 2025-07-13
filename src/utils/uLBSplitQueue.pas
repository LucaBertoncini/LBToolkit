unit uLBSplitQueue;

{$mode objfpc}{$H+}

interface

type
  TLBSplitQueueDisposeProc = procedure(Data: Pointer) of object;

  TLBSplitQueue = class;

  PLBSplitQueueNode = ^TLBSplitQueueNode;
  TLBSplitQueueNode = record
    Data: Pointer;
    Prev, Next: PLBSplitQueueNode;
  end;



  { TLBSplitQueue }
  TLBSplitQueueClassType = class of TLBSplitQueue;

  TLBSplitQueue = class(TObject)
  strict private
    FHead, FTail: PLBSplitQueueNode;
    FDisposeProc: TLBSplitQueueDisposeProc;
    FCount : Integer;

    procedure InternalDispose(var Node: PLBSplitQueueNode);
    procedure Remove(Node: PLBSplitQueueNode);

  public
    constructor Create(); virtual;
    destructor Destroy; override;

    procedure Clear();
    function isEmpty(): Boolean;

    procedure Enqueue(AData: Pointer);
    function Dequeue: Pointer;

    function SplitFrom(Node: PLBSplitQueueNode; aQueueType: TLBSplitQueueClassType): TLBSplitQueue;

    property DisposeProcedure: TLBSplitQueueDisposeProc write FDisposeProc;
    property FirstNode: PLBSplitQueueNode read FHead;
    property LastNode: PLBSplitQueueNode read FTail;

    property Count: Integer read FCount;
  end;

implementation

constructor TLBSplitQueue.Create();
begin
  FHead := nil;
  FTail := nil;
  FDisposeProc := nil;
end;

destructor TLBSplitQueue.Destroy;
begin
  Self.Clear();

  inherited Destroy;
end;

procedure TLBSplitQueue.Clear();
var
  Temp: PLBSplitQueueNode;

begin
  while FHead <> nil do
  begin
    Temp := FHead;
    FHead := FHead^.Next;
    Self.InternalDispose(Temp);
  end;
  FTail := nil;
  FCount := 0;
end;

function TLBSplitQueue.isEmpty(): Boolean;
begin
  Result := FTail = nil;
end;

procedure TLBSplitQueue.InternalDispose(var Node: PLBSplitQueueNode);
begin
  if (Node <> nil) and Assigned(FDisposeProc) then
    FDisposeProc(Node^.Data);
  Dispose(Node);
  Node := nil;
end;

procedure TLBSplitQueue.Remove(Node: PLBSplitQueueNode);
begin
  if Node = nil then Exit;

  if Node^.Prev <> nil then
    Node^.Prev^.Next := Node^.Next
  else
    FHead := Node^.Next;

  if Node^.Next <> nil then
    Node^.Next^.Prev := Node^.Prev
  else
    FTail := Node^.Prev;

  Self.InternalDispose(Node);
  Dec(FCount);
end;

procedure TLBSplitQueue.Enqueue(AData: Pointer);
var
  NewNode: PLBSplitQueueNode;

begin
  New(NewNode);
  NewNode^.Data := AData;
  NewNode^.Prev := nil;
  NewNode^.Next := FHead;

  if FHead <> nil then
    FHead^.Prev := NewNode
  else
    FTail := NewNode;

  FHead := NewNode;

  Inc(FCount);
end;

function TLBSplitQueue.Dequeue: Pointer;
var
  Temp: PLBSplitQueueNode;

begin
  if FTail = nil then
    Exit(nil);

  Result := FTail^.Data;
  Temp := FTail;
  FTail := FTail^.Prev;

  if FTail <> nil then
    FTail^.Next := nil
  else
    FHead := nil;

  Temp^.Data := nil;
  Self.InternalDispose(Temp);
  Dec(FCount);
end;

function TLBSplitQueue.SplitFrom(Node: PLBSplitQueueNode; aQueueType: TLBSplitQueueClassType): TLBSplitQueue;
var
  _NewCount : Integer;
  _Current : PLBSplitQueueNode;

begin
  if aQueueType = nil then
    Result := TLBSplitQueue.Create()
  else
    Result := aQueueType.Create();

  if (Node = nil) or (Node^.Next = nil) then
    Exit;

  Result.FHead := Node^.Next;
  Result.FTail := FTail;

  Node^.Next^.Prev := nil;
  FTail := Node;
  Node^.Next := nil;


  // Conta i nodi spostati
  _NewCount := 0;
  _Current := Result.FirstNode;
  while _Current <> nil do
  begin
    Inc(_NewCount);
    _Current := _Current^.Next;
  end;

  Result.FCount := _NewCount;
  Dec(Self.FCount, _NewCount);
end;

end.

