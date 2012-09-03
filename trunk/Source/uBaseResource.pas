unit uBaseResource;

interface

uses Classes, SysUtils, uMiscUtils;

const
  cSetEmptyState    = 1000;
  cSetWaitingState  = 1010;
  cSetQueuedState   = 1020;
  cSetLoadingState  = 1030;
  cSetReadyState    = 1040;
  cSetRemovedState  = 1050;
  cSetParamsUpdated = 1060;

type

  TResourceState = (rsEmpty, rsWaiting, rsQueued, rsLoading, rsReady, rsRemoved, rsUpdated);

  TPersistentResource = class (TPersistent)
  protected
    procedure WriteString(const s: WideString; const stream: TStream);
    function ReadString(const Stream: TStream): WideString;
    procedure WriteInt(const Value: integer; const stream: TStream);
    function ReadInt(const stream: TStream): integer;
    procedure WriteBool(const Value: boolean; const stream: TStream);
    function ReadBool(const stream: TStream): boolean;
    procedure WriteGUID(const Value: TGUID; const stream: TStream);
    function ReadGUID(const stream: TStream): TGUID;
    procedure WriteFloat(const Value: single; const stream: TStream);
    function ReadFloat(const stream: TStream): single;
  public
    GUID: TGUID;
    Version: integer;
    Storage: TObject;
    ResourceId: string;
    FriendlyName: string;
    procedure SaveToStream(s: TStream); virtual;
    procedure LoadFromStream(s: TStream); virtual;
    procedure SetGUID(GUIDString: string);
    constructor Create;
    destructor Destroy; override;
  end;

  TPersistentResClass = class of TPersistentResource;

  TResourceObject = class
  private
    FResource: TObject;
    FHandler: TObject;
    FState: TResourceState;
  public
    property Resource: TObject read FResource;
    property Handler: TObject read FHandler;
    property State: TResourceState read FState;
  
    procedure Notify(Sender: TObject; Msg: cardinal); virtual;

    constructor Create(aResource: TObject; aHandler: TObject);
  end;  

  TNotifiableObject = class (TPersistentResource)
    procedure Notify(Sender: TObject; Msg: Cardinal; Params: pointer = nil); virtual; abstract;
  end;


  PStackElement = ^TStackElement;
  TStackElement = record
    Data: pointer;
    Prev: PStackElement;
    Next: PStackElement;
  end;

  TStackEnumProc = procedure (Data: Pointer; var Break: boolean);

  TStack = class
  private
    FRoot: PStackElement;
    FTop: PStackElement;
  public
    constructor Create;
    destructor Destroy;override;

    function Push(Value: pointer): PStackElement;
    function Pop: Pointer;

    function Find(Value: pointer): PStackElement;
    procedure Remove(Element: PStackElement);
    procedure Enum(EnumProc: TStackEnumProc);

    property Root: PStackElement read FRoot;
    property Top: PStackElement read FTop;
  end;

  TResourceList = class (TList)
  public
    function Add(Resource: TPersistentResource): integer;
    function ResourceByGUID(const aGUID: TGUID): TPersistentResource;
  end;

  TFixUpData = record
    addr: pointer;
    GUID: TGUID;
  end;
  PFixUpData = ^TFixUpData;
  PObject = ^TObject;
  TFixUpList = class (TList)
  private
   function Get(Index: Integer): PFixUpData;
   procedure Put(Index: Integer; Item: PFixUpData);
  public
    function Add(const addr: pointer; const aGUID: TGUID): integer;
    property Items[Index: Integer]: PFixUpData read Get write Put; default;
  end;

var
  vResourceList: TResourceList;
  vFixUpList: TFixUpList;
implementation

{ TStack }

function TStack.Pop: Pointer;
begin
  if assigned(FTop) then begin
    result:=FTop.Data; Remove(FTop);
  end else result:=nil;
end;

function TStack.Push(Value: pointer): PStackElement;
var Element: PStackElement;
begin
  new(Element); result:=Element;
  Element.Data:=Value; Element.Next:=nil;
  if not assigned(FRoot) then begin
    FRoot:=Element;
    FTop:=FRoot; FRoot.Prev:=nil;
  end else begin
    FTop.Next:=Element;
    Element.Prev:=FTop;
    FTop:=Element;
  end;
end;

constructor TStack.Create;
begin
  inherited;
  FRoot:=nil; FTop:=FRoot;
end;

destructor TStack.Destroy;
begin
  while assigned(FRoot) do Remove(FTop);
  inherited;
end;

procedure TStack.Enum(EnumProc: TStackEnumProc);
var curr: PStackElement;
    br: boolean;
begin
  curr:=FRoot; br:=false;
  while Assigned(curr) and (not br) do begin
    EnumProc(Curr.Data,br);
    curr:=curr.Next;
  end;
end;

function TStack.Find(Value: pointer): PStackElement;
var curr: PStackElement;
begin
  curr:=FRoot; result:=nil;
  while assigned(curr) do begin
    if curr.Data=Value then begin result:=curr; exit; end
    else curr:=curr.Next;
  end;
end;

procedure TStack.Remove(Element: PStackElement);
begin
  if assigned(Element.Next) then Element.Next.Prev:=Element.Prev else FTop:=Element.Prev;
  if assigned(Element.Prev) then Element.Prev.Next:=Element.Next else FRoot:=Element.Next;
  if FRoot=FTop then FRoot:=nil; FTop:=nil;
  Dispose(Element);
end;

{TResourceObject}

constructor TResourceObject.Create(aResource: TObject; aHandler: TObject);
begin
  assert(assigned(aResource),'Resource is not assigned.');
  assert(assigned(aHandler),'Resource handler is not assigned.');
  FResource:=aResource; FHandler:=aHandler; FState:=rsWaiting;  
end;

procedure TResourceObject.Notify(Sender: TObject; Msg: cardinal);
begin
  case Msg of
    cSetEmptyState  : FState:=rsEmpty; 
    cSetWaitingState: FState:=rsWaiting;
    cSetQueuedState : FState:=rsQueued;
    cSetLoadingState: Fstate:=rsLoading;
    cSetReadyState  : FState:=rsReady;
    else assert(false, 'Unknown state '+inttostr(Msg));
  end;
end;

{ TPersistentResource }

constructor TPersistentResource.Create;
begin
  CreateGuid(GUID); Version:=1;
  vResourceList.Add(Self);
end;

destructor TPersistentResource.Destroy;
var i: integer;
begin
  i:=vResourceList.IndexOf(Self);
  if i>=0 then vResourceList.Delete(i);
  inherited;
end;

procedure TPersistentResource.LoadFromStream(s: TStream);
var l: integer;
    t: string;
begin
  t:=ReadString(s);
  GUID:=ReadGUID(s);
  Version:=ReadInt(s);
  ResourceId:=ReadString(s);
  FriendlyName:=ReadString(s);
end;

function TPersistentResource.ReadBool(const stream: TStream): boolean;
begin
  Stream.ReadBuffer(Result,sizeof(boolean));
end;

function TPersistentResource.ReadFloat(const stream: TStream): single;
begin
  Stream.ReadBuffer(result,4);
end;

function TPersistentResource.ReadGUID(const stream: TStream): TGUID;
begin
  stream.ReadBuffer(Result.D1,4);
  stream.ReadBuffer(Result.D2,2);
  stream.ReadBuffer(Result.D3,2);
  stream.ReadBuffer(Result.D4,8);
end;

function TPersistentResource.ReadInt(const stream: TStream): integer;
begin
  Stream.ReadBuffer(Result,4);
end;

function TPersistentResource.ReadString(const Stream: TStream): WideString;
var n: integer;
    x: WideString;
begin
  result:='';
  stream.ReadBuffer(n,4);
  if n=0 then exit; SetLength(x, n);
  Stream.ReadBuffer(x[1], Length(x) * SizeOf(x[1]));
  result:=x;
end;

procedure TPersistentResource.SaveToStream(s: TStream);
var l: integer;
begin
  WriteString(ClassName,s);
  WriteGUID(GUID,s);
  WriteInt(Version,s);
  WriteString(ResourceId,s);
  WriteString(FriendlyName,s);
end;

procedure TPersistentResource.SetGUID(GUIDString: string);
begin
  GUID:=StringToGUID(GUIDString);
end;

procedure TPersistentResource.WriteBool(const Value: boolean;
  const stream: TStream);
begin
  stream.WriteBuffer(Value,sizeof(boolean));
end;

procedure TPersistentResource.WriteFloat(const Value: single;
  const stream: TStream);
begin
  stream.WriteBuffer(Value,4);
end;

procedure TPersistentResource.WriteGUID(const Value: TGUID;
  const stream: TStream);
begin
  stream.WriteBuffer(Value.D1,4);
  stream.WriteBuffer(Value.D2,2);
  stream.WriteBuffer(Value.D3,2);
  stream.WriteBuffer(Value.D4,8);
end;

procedure TPersistentResource.WriteInt(const Value: integer;
  const stream: TStream);
begin
  Stream.WriteBuffer(Value,4);
end;

procedure TPersistentResource.WriteString(const s: WideString;
  const stream: TStream);
var n: integer;
begin
  if s<>'' then begin
    n:=Length(s);
    stream.WriteBuffer(n,4);
    stream.WriteBuffer(s[1],n*SizeOf(s[1]));
  end else begin
    n:=0; stream.WriteBuffer(n,4);
  end;
end;

{ TResourceList }

function TResourceList.Add(Resource: TPersistentResource): integer;
begin
  result:=inherited Add(Resource);
end;

function TResourceList.ResourceByGUID(const aGUID: TGUID): TPersistentResource;
var i: integer;
    res: TPersistentResource;
begin
  result:=nil;
  for i:=0 to Count-1 do begin
    res:=Items[i];
    if IsEqualGUID(res.GUID,aGUID) then begin
      result:=res; exit;
    end;
  end;
end;

{ TFixUpList }

function TFixUpList.Add(const addr: pointer; const aGUID: TGUID): integer;
var p: PFixUpData;
begin
  new(p); p.addr:=addr; p.GUID:=aGUID;
  result:=inherited Add(p);
end;

function TFixUpList.Get(Index: Integer): PFixUpData;
begin
  result := inherited Get(Index);
end;

procedure TFixUpList.Put(Index: Integer; Item: PFixUpData);
begin
  inherited Put(Index, Item);
end;

initialization
  vResourceList:=TResourceList.Create;
  vFixUpList:=TFixUpList.Create;
finalization
  vResourceList.Free;
  vFixUpList.Free;
end.
