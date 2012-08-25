unit uStorage;

interface

uses Classes, SysUtils, uMiscUtils, uBaseResource, zlib;

Type

  TStorageHeader = class;

  TDataHeader = class
  private
    FDataStream: TMemoryStream;
    FSourceObject: TDataHeader;
  public
    Offset,Size: int64;
    Compressed: boolean;
    GUID: TGUID;
    ObjClassName: ansistring;
    Handler: TStorageHeader;
    DataObject: TPersistentResource;
    FriendlyName: string;

    procedure Assign(DataHeader: TDataHeader);
    procedure GrabObject;
    function HeaderSize: integer;

    destructor Destroy;override;
  end;

  TStorageHeader = class
    Next: int64;
    Offset: int64;
    ObjList: TList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetObject(const GUID: TGUID): TDataHeader; overload;
    function GetObject(const Name: string): TDataHeader; overload;
    function Size: integer;
    procedure SaveToStream(const s: TStream);
    procedure LoadFromStream(const s: TStream);
  end;

  TDataStorage = class
  private
    FHeaders: TList;
    FFileStream: TFileStream;
    FMemHeader: TStorageHeader;
    FActiveHeader: TStorageHeader;
    FHeaderStream: TMemoryStream;
    FModified: boolean;
    FFileName: string;
    procedure LoadFromFile(FileName: string);
    function GetObject(const GUID: TGUID): TDataHeader;
    function ObjectByName(const FriendlyName: string): TDataHeader;
  public
    function AddObject(const Obj: TPersistentResource; Compress: boolean = true;
      const FriendlyName: string=''): TDataHeader; overload;
    function AddObject(const Obj: TPersistentResource; const aGUID: TGUID;
      Compress: boolean = true; const FriendlyName: string=''): TDataHeader; overload;
    function GetOrCreateObject(const GUID: TGUID): TPersistentResource;
    procedure RemoveObject(const GUID: TGUID);
    procedure SaveToFile(FileName: string);
    procedure Save;

    constructor Create(FileName: string='');
    destructor Destroy; override;
  end;

procedure CompressStream(inStream, outStream: TStream; const Level: TCompressionLevel = clMax);
procedure DecompressStream(inStream, outStream: TStream; Offs: Int64 = 0);

implementation

procedure CompressStream(inStream, outStream: TStream; const Level: TCompressionLevel = clMax);
begin
 with TCompressionStream.Create(TCompressionLevel(Level), outStream) do
  try
   inStream.Position := 0;
   CopyFrom(inStream, inStream.Size);
  finally
   Free;
  end;
end;

procedure DecompressStream(inStream, outStream: TStream; Offs: Int64 = 0);
var unzip: TDecompressionStream;
begin
  outStream.Position:=0; inStream.Position:=Offs;
  unzip:=TDecompressionStream.Create(instream);
  try
    outStream.CopyFrom(UnZip, UnZip.Size);
  finally
   unzip.Free; outStream.Position:=0;
  end;
end;

{ TStorageHeader }

constructor TStorageHeader.Create;
begin
  inherited Create;
  ObjList:=TList.Create;
  Next:=-1; Offset:=0;
end;

destructor TStorageHeader.Destroy;
begin
  FreeList(ObjList);
  inherited;
end;

function TStorageHeader.GetObject(const Name: string): TDataHeader;
var i: integer;
    obj: TDataHeader;
begin
  result:=nil;
  for i:=0 to ObjList.Count-1 do begin
    obj:=ObjList[i];
    if obj.FriendlyName=Name then begin result:=obj; exit; end;
  end;
end;

function TStorageHeader.GetObject(const GUID: TGUID): TDataHeader;
var i: integer;
    obj: TDataHeader;
begin
  result:=nil;
  for i:=0 to ObjList.Count-1 do begin
    obj:=ObjList[i];
    if obj.GUID=GUID then begin result:=obj; exit; end;
  end;
end;

procedure TStorageHeader.LoadFromStream(const s: TStream);
var ObjCount,i,n: integer;
    Obj: TDataHeader;
begin
  s.ReadBuffer(Next,sizeof(int64));
  s.ReadBuffer(ObjCount,4);
  ObjList.Count:=ObjCount;
  for i:=0 to ObjCount-1 do begin
    obj:=TDataHeader.Create; ObjList[i]:=obj;
    s.ReadBuffer(obj.Offset,sizeof(int64));
    s.ReadBuffer(obj.Size,sizeof(int64));
    s.ReadBuffer(obj.Compressed,sizeof(boolean));
    s.ReadBuffer(obj.GUID.D1,4);
    s.ReadBuffer(obj.GUID.D2,2);
    s.ReadBuffer(obj.GUID.D3,2);
    s.ReadBuffer(obj.GUID.D4,8);
    s.ReadBuffer(n,sizeof(integer));
    setlength(Obj.ObjClassName,n);
    s.ReadBuffer(Obj.ObjClassName[1],n);
    s.ReadBuffer(n,sizeof(integer));
    if n>0 then begin
      setlength(Obj.FriendlyName,n);
      s.ReadBuffer(Obj.FriendlyName[1],n);
    end;
    obj.Handler:=self;
    obj.DataObject:=nil;
  end;
end;

procedure TStorageHeader.SaveToStream(const s: TStream);
var i,n: integer;
    BaseOffs: Int64;
    obj: TDataHeader;
    t: ansistring;
begin
  BaseOffs:=s.Size+Size;
  s.WriteBuffer(Next,sizeof(int64));
  n:=ObjList.Count; s.WriteBuffer(n,4);
  for i:=0 to ObjList.Count-1 do begin
    obj:=ObjList[i];
    if not assigned(obj.FDataStream) then obj.GrabObject;
    obj.Offset:=BaseOffs; BaseOffs:=BaseOffs+obj.Size;
    s.WriteBuffer(obj.Offset,sizeof(int64));
    s.WriteBuffer(obj.Size,sizeof(int64));
    s.WriteBuffer(obj.Compressed,sizeof(boolean));
    s.WriteBuffer(obj.GUID.D1,4);
    s.WriteBuffer(obj.GUID.D2,2);
    s.WriteBuffer(obj.GUID.D3,2);
    s.WriteBuffer(obj.GUID.D4,8);
    t:=obj.DataObject.ClassName; n:=length(t);
    s.WriteBuffer(n,sizeof(integer));
    s.WriteBuffer(t[1],n);
    t:=obj.FriendlyName; n:=length(t);
    s.WriteBuffer(n,sizeof(integer));
    if n>0 then s.WriteBuffer(t[1],n);
  end;
end;

function TStorageHeader.Size: integer;
var i:integer;
    obj: TDataHeader;
begin
  result:=Sizeof(int64)+sizeof(integer);
  for i:=0 to ObjList.Count-1 do begin
    obj:=ObjList[i]; result:=result+obj.HeaderSize;
  end;
end;

{ TDataHeader }

procedure TDataHeader.Assign(DataHeader: TDataHeader);
begin
  Offset:=DataHeader.Offset;
  Size:=DataHeader.Size;
  Compressed:=DataHeader.Compressed;
  GUID:=DataHeader.GUID;
  ObjClassName:=DataHeader.ObjClassName;
  Handler:=DataHeader.Handler;
  DataObject:=DataHeader.DataObject;
  FriendlyName:=DataHeader.FriendlyName;
  FSourceObject:=DataHeader;
end;

destructor TDataHeader.Destroy;
begin
  if assigned(FDataStream) then FDataStream.Free;
  inherited;
end;

procedure TDataHeader.GrabObject;
var Temp,Comp: TMemoryStream;
begin
  if Compressed then begin
    Temp:=TMemoryStream.Create;
    DataObject.SaveToStream(Temp);
    Comp:=TMemoryStream.Create;
    CompressStream(Temp,Comp,clMax);
    size:=Comp.Size;
    FDataStream:=Comp;
    Temp.Free;
  end else begin
    FDataStream:=TMemoryStream.Create;
    DataObject.SaveToStream(FDataStream);
    size:=FDataStream.Size;
  end;
end;

function TDataHeader.HeaderSize: integer;
var n: integer;
begin
  n:=length(DataObject.ClassName);
  result:=2*sizeof(int64)+sizeof(boolean)+16+sizeof(integer)+n;
  n:=length(FriendlyName); result:=result+n;
end;

{ TDataStorage }

function TDataStorage.AddObject(const Obj: TPersistentResource; Compress: boolean;
  const FriendlyName: string): TDataHeader;
var dh: TDataHeader;
begin
  result:=GetObject(Obj.GUID);
  if assigned(result) then exit;
  result:=TDataHeader.Create;
  result.DataObject:=Obj;
  result.Handler:=FActiveHeader;
  result.Compressed:=Compress;
  result.FDataStream:=nil;
  result.GUID:=obj.GUID;
  result.ObjClassName:=obj.ClassName;
  if FriendlyName<>'' then begin
    dh:=ObjectByName(FriendlyName);
    assert((not assigned(dh)),'Object with name: "'+FriendlyName+'" allready in storage');
  end;
  result.FriendlyName:=FriendlyName;
  FActiveHeader.ObjList.Add(result);
  FModified:=true;
end;

function TDataStorage.AddObject(const Obj: TPersistentResource;
  const aGUID: TGUID; Compress: boolean; const FriendlyName: string): TDataHeader;
var dh: TDataHeader;
begin
  result:=GetObject(aGUID);
  if assigned(result) then exit;
  result:=TDataHeader.Create;
  result.DataObject:=Obj;
  result.Handler:=FActiveHeader;
  result.Compressed:=Compress;
  result.FDataStream:=nil;
  result.GUID:=aGUID;
  result.ObjClassName:=obj.ClassName;
  if FriendlyName<>'' then begin
    dh:=ObjectByName(FriendlyName);
    assert((not assigned(dh)),'Object with name: "'+FriendlyName+'" allready in storage');
  end;
  result.FriendlyName:=FriendlyName;
  FActiveHeader.ObjList.Add(result);
  FModified:=true;
end;

constructor TDataStorage.Create(FileName: string);
begin
  FFileName:=FileName;   FFileStream:=nil;
  FHeaders:=TList.Create;
  FMemHeader:=TStorageHeader.Create;
  FActiveHeader:=FMemHeader;
  if (FileName<>'') and FileExists(FileName) then LoadFromFile(FileName);
  FHeaders.Add(FActiveHeader);
  FModified:=false;
  FHeaderStream:=TMemoryStream.Create;
end;

destructor TDataStorage.Destroy;
begin
  FreeObjectList(FHeaders);
  if assigned(FFileStream) then FFileStream.Free;
  inherited;
end;

function TDataStorage.GetObject(const GUID: TGUID): TDataHeader;
var i: integer;
    sh: TStorageHeader;
    dh: TDataHeader;
begin
  result:=nil;
  for i:=0 to FHeaders.Count-1 do begin
    sh:=FHeaders[i];
    dh:=sh.GetObject(GUID);
    if assigned(dh) then begin result:=dh; exit; end;
  end;
end;

function TDataStorage.GetOrCreateObject(const GUID: TGUID): TPersistentResource;
var dh: TDataHeader;
    ResClass: TPersistentResClass;
    inStream, outStream: TMemoryStream;
begin
  dh:=GetObject(GUID);
  assert(assigned(dh),'Object '+GuidToString(GUID)+' not found in the storage '+FFileName);
  if assigned(dh.DataObject) then result:=dh.DataObject
  else begin
    ResClass:=TPersistentResClass(GetClass(dh.ObjClassName));
    assert(assigned(ResClass),'Class "'+dh.ObjClassName+'" in not registered');
    dh.DataObject:=ResClass.Create;
    FFileStream.Seek(dh.Offset,soBeginning);
    if dh.Compressed then begin
      inStream:=TMemoryStream.Create;
      outStream:=TMemoryStream.Create;
      inStream.CopyFrom(FFileStream,dh.Size);
      DecompressStream(inStream,outStream);
      dh.DataObject.LoadFromStream(outStream);
      dh.DataObject.Storage:=Self; 
      inStream.Free; outStream.Free;
    end else dh.DataObject.LoadFromStream(FFileStream);
    result:=dh.DataObject;
  end;
end;

procedure TDataStorage.LoadFromFile(FileName: string);
var fs: TFileStream;
    Header: TStorageHeader;
begin
  fs:=TFileStream.Create(FileName,fmOpenReadWrite);
  FFileStream:=fs;
  FreeObjectList(FHeaders); FHeaders:=TList.Create;
  if fs.Size=0 then exit; fs.Position:=0;
  repeat
    Header:=TStorageHeader.Create;
    Header.LoadFromStream(fs);
    FHeaders.Add(Header);
    if Header.Next<>-1 then fs.Seek(Header.Next,soBeginning);
  until Header.Next=-1;
end;

function TDataStorage.ObjectByName(const FriendlyName: string): TDataHeader;
var i: integer;
    sh: TStorageHeader;
    dh: TDataHeader;
begin
  result:=nil;
  for i:=0 to FHeaders.Count-1 do begin
    sh:=FHeaders[i];
    dh:=sh.GetObject(FriendlyName);
    if assigned(dh) then begin result:=dh; exit; end;
  end;
end;

procedure TDataStorage.RemoveObject(const GUID: TGUID);
var i,j: integer;
    sh: TStorageHeader;
    dh: TDataHeader;
begin
  for i:=0 to FHeaders.Count-1 do begin
    sh:=FHeaders[i];
    for j:=0 to sh.ObjList.Count-1 do begin
      dh:=sh.ObjList[i];
      if assigned(dh) and (dh.GUID=GUID) then begin
        sh.ObjList.Delete(j); dh.Free; exit;
      end;
    end;
  end;
end;

procedure TDataStorage.Save;
var fs: TFileStream;
    PrevHeader: TStorageHeader;
    Next: int64;
    i: integer;
    obj: TDataHeader;
begin
  if not FModified then exit;

  if not assigned(FFileStream) then begin
    if FileExists(FFileName) then
      fs:=TFileStream.Create(FFileName,fmOpenReadWrite)
    else fs:=TFileStream.Create(FFileName,fmCreate);
  end else fs:=FFileStream;
  next:=fs.Size;
  //Перезаписываем указатель на следующий заголовок
  if FHeaders.Count>1 then begin
    PrevHeader:=FHeaders[FHeaders.Count-2];
    fs.Seek(PrevHeader.Offset, soBeginning);
    fs.WriteBuffer(next,sizeof(int64));
    fs.Seek(Next,soBeginning);
  end;
  for i:=0 to FActiveHeader.ObjList.Count-1 do begin
    obj:=FActiveHeader.ObjList[i];
    if not assigned(obj.FDataStream) then obj.GrabObject;
  end;
  FActiveHeader.Offset:=Next;
  FActiveHeader.SaveToStream(fs);
  for i:=0 to FActiveHeader.ObjList.Count-1 do begin
    obj:=FActiveHeader.ObjList[i];
    obj.FDataStream.Position:=0;
    fs.CopyFrom(obj.FDataStream,obj.Size);
    obj.FDataStream.Free; obj.FDataStream:=nil;
  end;
  FActiveHeader.Next:=fs.Size;
  FActiveHeader:=TStorageHeader.Create;
  FHeaders.Add(FActiveHeader);

  FModified:=false;
  //fs.Free;
end;

procedure TDataStorage.SaveToFile(FileName: string);
var fs: TFileStream;
    i,j, BaseOffs: integer;
    obj,so: TDataHeader;
    sh,th: TStorageHeader;
begin
  assert(FFileName<>FileName, 'You can''t ovewrite opened storage. Select another FileName');
  fs:=TFileStream.Create(FileName,fmCreate); fs.Position:=0;
  sh:=TStorageHeader.Create;
  for i:=0 to FHeaders.Count-1 do begin
    th:=FHeaders[i];
    for j:=0 to th.ObjList.Count-1 do begin
      so:=th.ObjList[j];
      obj:=TDataHeader.Create;
      obj.Assign(so); obj.FDataStream:=TMemoryStream.Create;
      sh.ObjList.Add(obj);
    end;
  end;
  sh.SaveToStream(fs);
  for i:=0 to sh.ObjList.Count-1 do begin
    obj:=sh.ObjList[i];
    FFileStream.Seek(obj.FSourceObject.Offset,soBeginning);
    fs.CopyFrom(FFileStream,obj.size);
  end;
  fs.Free; sh.Free;
end;

end.
