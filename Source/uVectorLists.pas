unit uVectorLists;

interface

uses VectorTypes, VectorGeometry;

type

  PExtents = ^TExtents;
  TExtents = record
    emin, emax: TVector3f
  end;

  // Vector components count (1-4)
  TVectorType = (vtInteger, vtSingle, vtDouble, vtVector, vtPoint, vtMat3, vtMat4);
  TSingleArray = array of single;

  TBaseAttribList = class
  private
    FData: pointer;
    FVectorType: TVectorType;
    FCount: integer;
    FLastIndex: integer;
    FGrowCount: integer;
    FCapacity: integer;

    function GetData: pointer;
    function GetSize: integer;
    procedure setCapacity(const Value: integer);virtual; abstract;
    procedure setCount(const Value: integer);virtual; abstract;
    function getItemsCount: integer;
  public
    constructor Create;

    procedure Clear;virtual; abstract;
    procedure Delete(Index: Integer);virtual; abstract;
    procedure Exchange(index1, index2: Integer);virtual; abstract;

    property Data: pointer read GetData;
    property DataSize: integer read GetSize;
    property Count: integer read FCount write setCount;
    property Capacity: integer read FCapacity write setCapacity;
    property ItemsCount: integer read getItemsCount;

    property VectorType: TVectorType read FVectorType;
  end;

  TIntegerAttribList = class (TBaseAttribList)
  private
    FList: array of integer;
    FMin, FMax: integer;
    procedure GrowList;
    procedure setCapacity(const Value: integer);override;
    procedure setCount(const Value: integer);override;
    function getItem(Index: integer): integer;
    procedure setItem(Index: integer; const Value: integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(a,b,c,d: byte); overload;
    procedure Add(index: integer); overload;
    procedure Add(index1,index2: integer); overload;
    procedure Add(index1,index2, index3: integer); overload;
    procedure Add(index1,index2, index3, index4: integer); overload;

    procedure Clear; override;
    procedure Delete(Index: Integer);override;
    procedure Exchange(index1, index2: Integer);override;

    property Min: integer read FMin;
    property Max: integer read FMax;

    property Items[Index: integer]: integer read getItem write setItem; default;
  end;

  TVectorAttribList = class (TBaseAttribList)
  private
    FList: TSingleArray;
    FMin, FMax: TVector3f;
    procedure GrowList;
    function getExtents: TExtents;
    procedure setCapacity(const Value: integer);override;
    procedure setCount(const Value: integer);override;
    function getItem(Index: integer): single;
    procedure setItem(Index: integer; const Value: single);
  public
    constructor Create(VectorType: TVectorType = vtVector);
    destructor Destroy; override;

    procedure Add(const x: single); overload;
    procedure Add(const x,y: single); overload;
    procedure Add(const x,y,z: single); overload;
    procedure Add(const x,y,z,w: single); overload;

    procedure Add(const v: TVector2f); overload;
    procedure Add(const v1,v2: TVector2f); overload;
    procedure Add(const v1,v2,v3: TVector2f); overload;
    procedure Add(const v1,v2,v3,v4: TVector2f); overload;

    procedure Add(const v: TVector3f); overload;
    procedure Add(const v1,v2: TVector3f); overload;
    procedure Add(const v1,v2,v3: TVector3f); overload;
    procedure Add(const v1,v2,v3,v4: TVector3f); overload;

    procedure Add(const v: TVector4f); overload;
    procedure Add(const v1,v2: TVector4f); overload;
    procedure Add(const v1,v2,v3: TVector4f); overload;
    procedure Add(const v1,v2,v3,v4: TVector4f); overload;

    procedure Add(const m: TMatrix3f); overload;
    procedure Add(const m: TMatrix4f); overload;

    function GetAsSingle(Index: integer): single;
    function GetAsVector2f(Index: integer): TVector2f;
    function GetAsVector3f(Index: integer): TVector3f;
    function GetAsVector4f(Index: integer): TVector4f;

    procedure SetAsSingle(Index: integer; const Value: single);
    procedure SetAsVector2f(Index: integer; const Value: TVector2f);
    procedure SetAsVector3f(Index: integer; const Value: TVector3f);
    procedure SetAsVector4f(Index: integer; const Value: TVector4f);

    function GetSingle(Index: integer): single; overload;
    function GetVector2f(Index: integer): TVector2f; overload;
    function GetVector3f(Index: integer): TVector3f; overload;
    function GetVector4f(Index: integer): TVector4f; overload;

    procedure SetVector(Index: integer; const Value: single); overload;
    procedure SetVector(Index: integer; const Value: TVector2f); overload;
    procedure SetVector(Index: integer; const Value: TVector3f); overload;
    procedure SetVector(Index: integer; const Value: TVector4f); overload;

    property Items[Index: integer]: single read getItem write setItem; default;
    procedure Clear; override;
    procedure Delete(Index: Integer);override;
    procedure Exchange(index1, index2: Integer);override;

    property Extents: TExtents read getExtents;
  end;

  _TBaseVectorList = class (TVectorAttribList)
  public
    procedure GetExtents(var min, max: TAffineVector);
    procedure Normalize; virtual;
    procedure Normalize3f;
    procedure Lerp(const list1, list2: _TBaseVectorList; lerpFactor: Single);
    procedure Combine(const list2: _TBaseVectorList; factor: Single);
  end;

  _TAffineVectorList = class (_TBaseVectorList)
  private
    function Get(Index: Integer): TAffineVector;
    procedure Put(Index: Integer; const Value: TAffineVector);
    function getList: PAffineVectorArray;
  public
    constructor Create;

    procedure Normalize; override;
    procedure Translate(const delta: TAffineVector);
    procedure TranslateItem(Index: Integer; const delta: TAffineVector);
    procedure CombineItem(Index: Integer; const vector: TAffineVector; const f: Single);
    procedure TransformAsPoints(const matrix: TMatrix);
    procedure TransformAsVectors(const matrix: TMatrix); overload;
    procedure TransformAsVectors(const matrix: TAffineMatrix); overload;

    property List: PAffineVectorArray read getList;
    property Items[Index: Integer]: TAffineVector read Get write Put; default;
  end;

  _TVectorList = class (_TBaseVectorList)
  private
    function Get(Index: Integer): TVector;
    procedure Put(Index: Integer; const Value: TVector);
    function getList: PVectorArray;
  public
    constructor Create;

    procedure Normalize; override;
    procedure Translate(const delta: TVector); overload;
    procedure Translate(const delta: TAffineVector); overload;
    procedure TranslateItem(Index: Integer; const delta: TVector); overload;
    procedure TranslateItem(Index: Integer; const delta: TAffineVector); overload;
    procedure CombineItem(Index: Integer; const vector: TAffineVector; const f: Single);
    procedure TransformAsPoints(const matrix: TMatrix);
    procedure TransformAsVectors(const matrix: TMatrix); overload;
    procedure TransformAsVectors(const matrix: TAffineMatrix); overload;

    property List: PVectorArray read getList;
    property Items[Index: Integer]: TVector read Get write Put; default;
  end;

  _TIntegerList = class(TIntegerAttribList)
  private
    function Get(Index: Integer): Integer;
    function getList: PIntegerArray;
    procedure Put(Index: Integer; const Value: Integer);
  public
    property Items[Index: Integer]: Integer read Get write Put; default;
    property List: PIntegerArray read getList;
  end;

implementation

const CVectorSize: array [vtInteger..vtMat4] of byte = (1,1,2,3,4,9,16);

{ TVectorAttribList }

procedure TVectorAttribList.Add(const v: TVector2f);
begin
  Add(v[0],v[1]);
end;

procedure TVectorAttribList.Add(const v1, v2: TVector2f);
begin
  Add(v1);Add(v2);
end;

procedure TVectorAttribList.Add(const v1, v2, v3: TVector2f);
begin
  Add(v1);Add(v2);Add(v3);
end;

procedure TVectorAttribList.Add(const v1, v2, v3, v4: TVector2f);
begin
  Add(v1);Add(v2);Add(v3);Add(v4);
end;

procedure TVectorAttribList.Add(const x: single);
begin
  if FCount=0 then begin
    FMin[0]:=x; FMax[0]:=x;
  end else begin
    if x<FMin[0] then FMin[0]:=x;
    if x>FMax[0] then FMax[0]:=x;
  end;
  assert(FVectorType=vtSingle,'Invalid type of vector, expected vtSingle');
  inc(FLastIndex);
  if FCapacity<=FCount then GrowList;
  case FVectorType of
    vtInteger,vtSingle: begin FList[FLastIndex+1]:=x; inc(FLastIndex,1); end;
    vtDouble: begin FList[FLastIndex+1]:=x; FList[FLastIndex+2]:=0; inc(FLastIndex,2); end;
    vtVector: begin
      FList[FLastIndex+1]:=x;
      FList[FLastIndex+2]:=0;
      FList[FLastIndex+3]:=0;
      inc(FLastIndex,3);
    end;
    vtPoint: begin
      FList[FLastIndex+1]:=x;
      FList[FLastIndex+2]:=0;
      FList[FLastIndex+3]:=0;
      FList[FLastIndex+4]:=0;
      inc(FLastIndex,4);
    end;
  end; inc(FCount);
end;

procedure TVectorAttribList.Add(const x, y: single);
begin
  if FCount=0 then begin
    FMin[0]:=x; FMax[0]:=x;
    FMin[1]:=y; FMax[1]:=y;
  end else begin
    if x<FMin[0] then FMin[0]:=x;
    if x>FMax[0] then FMax[0]:=x;
    if y<FMin[1] then FMin[1]:=y;
    if y>FMax[1] then FMax[1]:=y;
  end;
//  assert(FVectorType=vtDouble,'Invalid type of vector, expected vtDouble');
  if FCapacity<=FCount then GrowList;
  case FVectorType of
    vtInteger,vtSingle: begin FList[FLastIndex+1]:=x; inc(FLastIndex,1); end;
    vtDouble: begin FList[FLastIndex+1]:=x; FList[FLastIndex+2]:=y; inc(FLastIndex,2); end;
    vtVector: begin
      FList[FLastIndex+1]:=x;
      FList[FLastIndex+2]:=y;
      FList[FLastIndex+3]:=0;
      inc(FLastIndex,3);
    end;
    vtPoint: begin
      FList[FLastIndex+1]:=x;
      FList[FLastIndex+2]:=y;
      FList[FLastIndex+3]:=0;
      FList[FLastIndex+4]:=0;
      inc(FLastIndex,4);
    end;
  end; inc(FCount);
end;

procedure TVectorAttribList.Add(const x, y, z: single);
begin
  if FCount=0 then begin
    FMin[0]:=x; FMax[0]:=x;
    FMin[1]:=y; FMax[1]:=y;
    FMin[2]:=z; FMax[2]:=z;
  end else begin
    if x<FMin[0] then FMin[0]:=x;
    if x>FMax[0] then FMax[0]:=x;
    if y<FMin[1] then FMin[1]:=y;
    if y>FMax[1] then FMax[1]:=y;
    if z<FMin[2] then FMin[2]:=z;
    if z>FMax[2] then FMax[2]:=z;
  end;
//  assert(FVectorType=vtVector,'Invalid type of vector, expected vtVector');
  if FCapacity<=FCount then GrowList;
  case FVectorType of
    vtInteger,vtSingle: begin FList[FLastIndex+1]:=x; inc(FLastIndex,1); end;
    vtDouble: begin FList[FLastIndex+1]:=x; FList[FLastIndex+2]:=y; inc(FLastIndex,2); end;
    vtVector: begin
      FList[FLastIndex+1]:=x;
      FList[FLastIndex+2]:=y;
      FList[FLastIndex+3]:=z;
      inc(FLastIndex,3);
    end;
    vtPoint: begin
      FList[FLastIndex+1]:=x;
      FList[FLastIndex+2]:=y;
      FList[FLastIndex+3]:=z;
      FList[FLastIndex+4]:=0;
      inc(FLastIndex,4);
    end;
  end; inc(FCount);
end;

procedure TVectorAttribList.Add(const x, y, z, w: single);
begin
  if FCount=0 then begin
    FMin[0]:=x; FMax[0]:=x;
    FMin[1]:=y; FMax[1]:=y;
    FMin[2]:=z; FMax[2]:=z;
  end else begin
    if x<FMin[0] then FMin[0]:=x;
    if x>FMax[0] then FMax[0]:=x;
    if y<FMin[1] then FMin[1]:=y;
    if y>FMax[1] then FMax[1]:=y;
    if z<FMin[2] then FMin[2]:=z;
    if z>FMax[2] then FMax[2]:=z;
  end;
//  assert(FVectorType=vtPoint,'Invalid type of vector, expected vtPoint');
  if FCapacity<=FCount then GrowList;
  case FVectorType of
    vtInteger,vtSingle: begin FList[FLastIndex+1]:=x; inc(FLastIndex,1); end;
    vtDouble: begin FList[FLastIndex+1]:=x; FList[FLastIndex+2]:=y; inc(FLastIndex,2); end;
    vtVector: begin
      FList[FLastIndex+1]:=x;
      FList[FLastIndex+2]:=y;
      FList[FLastIndex+3]:=z;
      inc(FLastIndex,3);
    end;
    vtPoint: begin
      FList[FLastIndex+1]:=x;
      FList[FLastIndex+2]:=y;
      FList[FLastIndex+3]:=z;
      FList[FLastIndex+4]:=w;
      inc(FLastIndex,4);
    end;
  end; inc(FCount);
end;

procedure TVectorAttribList.Add(const v: TVector3f);
begin
  Add(v[0],v[1],v[2]);
end;

procedure TVectorAttribList.Add(const v1, v2: TVector3f);
begin
  Add(v1);Add(v2);
end;

procedure TVectorAttribList.Add(const v1, v2, v3, v4: TVector3f);
begin
  Add(v1);Add(v2);Add(v3);Add(v4);
end;

procedure TVectorAttribList.Add(const v1, v2, v3: TVector3f);
begin
  Add(v1);Add(v2);Add(v3);
end;

procedure TVectorAttribList.Add(const v: TVector4f);
begin
  Add(v[0],v[1],v[2],v[3]);
end;

procedure TVectorAttribList.Add(const v1, v2: TVector4f);
begin
  Add(v1);Add(v2);
end;

procedure TVectorAttribList.Add(const v1, v2, v3: TVector4f);
begin
  Add(v1);Add(v2);Add(v3);
end;

procedure TVectorAttribList.Add(const v1, v2, v3, v4: TVector4f);
begin
  Add(v1);Add(v2);Add(v3);Add(v4);
end;

procedure TVectorAttribList.Clear;
begin
  FGrowCount:=1; FCapacity:=1;
  setlength(FList,FCapacity*CVectorSize[FVectorType]);
  FLastIndex:=-1;   FCount:=0;
  VectorGeometry.SetVector(Fmin,0,0,0);
  VectorGeometry.SetVector(Fmax,0,0,0);
end;

constructor TVectorAttribList.Create(VectorType: TVectorType);
begin
  inherited Create;
  FVectorType:=VectorType;
  setlength(FList,FCapacity*CVectorSize[FVectorType]);
  FData:=@FList[0];
  VectorGeometry.SetVector(Fmin,0,0,0);
  VectorGeometry.SetVector(Fmax,0,0,0);
end;

procedure TVectorAttribList.Delete(Index: Integer);
var i: integer;
begin
  if Index>=FCount then exit;
  for i:= Index+1 to FCount-1 do FList[i-1]:=FList[i];
  dec(FCount); FLastIndex:=FCount;
end;

destructor TVectorAttribList.Destroy;
begin
  FList:=nil;
  inherited;
end;

procedure TVectorAttribList.Exchange(index1, index2: Integer);
var i,i1,i2: integer;
    t: single;
begin
  i1:=index1*CVectorSize[FVectorType];
  i2:=index2*CVectorSize[FVectorType];
  for i:=0 to CVectorSize[FVectorType]-1 do begin
    t:=FList[i1+i]; FList[i1+i]:=FList[i2+i]; FList[i2+i]:=t;
  end;
end;

function TVectorAttribList.getExtents: TExtents;
begin
  result.emin:=FMin;
  result.emax:=FMax;
end;

function TVectorAttribList.getItem(Index: integer): single;
begin
  result:=FList[Index];
end;

function TVectorAttribList.GetAsSingle(Index: integer): single;
begin
  result:=FList[Index];
end;


function TVectorAttribList.GetAsVector2f(Index: integer): TVector2f;
begin
  result[0]:=FList[Index*2];
  result[1]:=FList[Index*2+1];
end;

function TVectorAttribList.GetAsVector3f(Index: integer): TVector3f;
begin
  result[0]:=FList[Index*3];
  result[1]:=FList[Index*3+1];
  result[2]:=FList[Index*3+2];
end;

function TVectorAttribList.GetAsVector4f(Index: integer): TVector4f;
begin
  result[0]:=FList[Index*4];
  result[1]:=FList[Index*4+1];
  result[2]:=FList[Index*4+2];
  result[3]:=FList[Index*4+3];
end;

function TVectorAttribList.GetVector2f(Index: integer): TVector2f;
var idx: integer;
begin
  idx:=Index*CVectorSize[FVectorType];
  result[0]:=FList[idx];
  if CVectorSize[FVectorType]>=2 then result[1]:=FList[idx+1];
end;

function TVectorAttribList.GetSingle(Index: integer): single;
var idx: integer;
begin
  idx:=Index*CVectorSize[FVectorType];
  result:=FList[idx];
end;

function TVectorAttribList.GetVector4f(Index: integer): TVector4f;
var idx: integer;
begin
  idx:=Index*CVectorSize[FVectorType];
  result[0]:=FList[idx];
  if CVectorSize[FVectorType]>=2 then result[1]:=FList[idx+1];
  if CVectorSize[FVectorType]>=3 then result[2]:=FList[idx+2];
  if CVectorSize[FVectorType]>=4 then result[3]:=FList[idx+3];
end;

function TVectorAttribList.GetVector3f(Index: integer): TVector3f;
var idx: integer;
begin
  idx:=Index*CVectorSize[FVectorType];
  result[0]:=FList[idx];
  if CVectorSize[FVectorType]>=2 then result[1]:=FList[idx+1];
  if CVectorSize[FVectorType]>=3 then result[2]:=FList[idx+2];
end;

procedure TVectorAttribList.GrowList;
begin
  FCapacity:=FCapacity+FGrowCount;
  FGrowCount:=FCapacity;
  setlength(FList,FCapacity*CVectorSize[FVectorType]);
  FData:=@FList[0];
end;

procedure TVectorAttribList.SetAsSingle(Index: integer; const Value: single);
begin
  FList[Index]:=Value;
end;

procedure TVectorAttribList.SetAsVector2f(Index: integer; const Value: TVector2f);
var idx: integer;
begin
  idx:=Index*2;
  FList[idx]:=Value[0];
  FList[idx+1]:=Value[1];
end;

procedure TVectorAttribList.SetAsVector3f(Index: integer; const Value: TVector3f);
var idx: integer;
begin
  idx:=Index*3;
  FList[idx]:=Value[0];
  FList[idx+1]:=Value[1];
  FList[idx+2]:=Value[2];
end;

procedure TVectorAttribList.SetAsVector4f(Index: integer; const Value: TVector4f);
var idx: integer;
begin
  idx:=Index*4;
  FList[idx]:=Value[0];
  FList[idx+1]:=Value[1];
  FList[idx+2]:=Value[2];
  FList[idx+3]:=Value[3];
end;

procedure TVectorAttribList.setCapacity(const Value: integer);
begin
  FCapacity:=Value;
  FGrowCount:=FCapacity;
  setlength(FList,FCapacity*CVectorSize[FVectorType]);
  FData:=@FList[0];
end;

procedure TVectorAttribList.setCount(const Value: integer);
begin
  FCount:=Value; setCapacity(Value);
  FLastIndex:=FCount-1;
end;

procedure TVectorAttribList.setItem(Index: integer; const Value: single);
begin
  FList[Index]:=Value;
end;

procedure TVectorAttribList.SetVector(Index: integer; const Value: single);
var idx: integer;
begin
  idx:=Index*CVectorSize[FVectorType];
  FList[idx]:=Value;
end;

procedure TVectorAttribList.SetVector(Index: integer; const Value: TVector2f);
var idx: integer;
begin
  idx:=Index*CVectorSize[FVectorType];
  FList[idx]:=Value[0];
  if CVectorSize[FVectorType]>=2 then FList[idx+1]:=Value[1];
end;

procedure TVectorAttribList.SetVector(Index: integer; const Value: TVector3f);
var idx: integer;
begin
  idx:=Index*CVectorSize[FVectorType];
  FList[idx]:=Value[0];
  if CVectorSize[FVectorType]>=2 then FList[idx+1]:=Value[1];
  if CVectorSize[FVectorType]>=3 then FList[idx+2]:=Value[2];
end;

procedure TVectorAttribList.SetVector(Index: integer; const Value: TVector4f);
var idx: integer;
begin
  idx:=Index*CVectorSize[FVectorType];
  FList[idx]:=Value[0];
  if CVectorSize[FVectorType]>=2 then FList[idx+1]:=Value[1];
  if CVectorSize[FVectorType]>=3 then FList[idx+2]:=Value[2];
  if CVectorSize[FVectorType]>=4 then FList[idx+3]:=Value[3];
end;

procedure TVectorAttribList.Add(const m: TMatrix3f);
begin
  Add(m[0],m[1],m[2]);
end;

procedure TVectorAttribList.Add(const m: TMatrix4f);
begin
  Add(m[0],m[1],m[2],m[3]);
end;

{ TIntegerAttribList }

procedure TIntegerAttribList.Add(index: integer);
begin
  if FCount=0 then begin
    FMin:=Index; FMax:=Index;
  end else begin
    if index<FMin then FMin:=index;
    if index>FMax then FMax:=index;
  end;
  inc(FLastIndex);
  if FCapacity<=FCount then GrowList;
  FList[FLastIndex]:=Index; inc(FCount);
end;

procedure TIntegerAttribList.Add(index1, index2: integer);
begin
  Add(Index1); Add(Index2);
end;

procedure TIntegerAttribList.Add(index1, index2, index3: integer);
begin
  Add(Index1); Add(Index2); Add(Index3);
end;

procedure TIntegerAttribList.Add(index1, index2, index3, index4: integer);
begin
  Add(Index1); Add(Index2); Add(Index3); Add(Index4);
end;

procedure TIntegerAttribList.Add(a, b, c, d: byte);
var i: integer;
begin
  i:=a or (b shl 8) or (c shl 16) or (d shl 24); Add(i);
end;

procedure TIntegerAttribList.Clear;
begin
  FGrowCount:=1;
  FCapacity:=FGrowCount;
  setlength(FList,FCapacity);
  FLastIndex:=-1;   FCount:=0;
  FMin:=0; FMax:=0;
end;

constructor TIntegerAttribList.Create;
begin
  inherited Create;
  setlength(FList,FCapacity);
  FData:=@FList[0];
  FMin:=0; FMax:=0;
  FVectorType:=vtInteger;
end;

procedure TIntegerAttribList.Delete(Index: Integer);
var i: integer;
begin
  if Index>=FCount then exit;
  for i:= Index+1 to FCount-1 do FList[i-1]:=FList[i];
  dec(FCount); FLastIndex:=FCount;
end;

destructor TIntegerAttribList.Destroy;
begin
  FList:=nil;
  inherited;
end;

procedure TIntegerAttribList.Exchange(index1, index2: Integer);
var i,i1,i2: integer;
    t: integer;
begin
  i1:=index1*CVectorSize[FVectorType];
  i2:=index2*CVectorSize[FVectorType];
  for i:=0 to CVectorSize[FVectorType]-1 do begin
    t:=FList[i1+i]; FList[i1+i]:=FList[i2+i]; FList[i2+i]:=t;
  end;
end;

function TIntegerAttribList.getItem(Index: integer): integer;
begin
  result:=FList[Index];
end;

procedure TIntegerAttribList.GrowList;
begin
  FCapacity:=FCapacity+FGrowCount;
  FGrowCount:=FCapacity;
  setlength(FList,FCapacity);
  FData:=@FList[0];
end;

procedure TIntegerAttribList.setCapacity(const Value: integer);
begin
  FCapacity:=Value;
  FGrowCount:=FCapacity;
  setlength(FList,FCapacity);
  FData:=@FList[0];
end;

procedure TIntegerAttribList.setCount(const Value: integer);
begin
  FCount:=Value; setCapacity(Value);
  FLastIndex:=FCount-1;
end;

procedure TIntegerAttribList.setItem(Index: integer; const Value: integer);
begin
  FList[Index]:=Value;
end;

{ TBaseAttribList }

constructor TBaseAttribList.Create;
begin
  FGrowCount:=1;
  FCapacity:=FGrowCount;
  FLastIndex:=-1; FCount:=0;
end;

function TBaseAttribList.GetData: pointer;
begin
  result:=FData;
end;

function TBaseAttribList.getItemsCount: integer;
begin
  result:=FCount*CVectorSize[FVectorType];
end;

function TBaseAttribList.GetSize: integer;
begin
  result:=FCount*CVectorSize[FVectorType]*4;
end;

{ _TBaseVectorList }

procedure _TBaseVectorList.Combine(const list2: _TBaseVectorList;
  factor: Single);
var I: Integer;
begin
  Assert(list2.Count = Count,'Sizes of the lists is  differ');
  for I := 0 to ItemsCount - 1 do FList[i]:=FList[i]+list2[i]*factor;
end;

procedure _TBaseVectorList.GetExtents(var min, max: TAffineVector);
begin
  min:=Extents.emin; max:=Extents.emax;
end;

procedure _TBaseVectorList.Lerp(const list1, list2: _TBaseVectorList;
  lerpFactor: Single);
var i,n: integer;
begin
  Assert(list1.Count = list2.Count, 'Sizes of the lists is  differ');
  Count := list1.Count;
  n:=FCount*CVectorSize[FVectorType];
  for i:=0 to n-1 do
    FList[i]:=PSingleArray(list1.Data)[i]+(PSingleArray(list2.Data)[i]-
      PSingleArray(list1.Data)[i])*lerpFactor;
end;

procedure _TBaseVectorList.Normalize;
var i,j,idx: integer;
    sum,invLen: single;
begin
  for i:=0 to Count-1 do begin
    idx:=i*CVectorSize[FVectorType]; Sum:=0;
    for j:=0 to CVectorSize[FVectorType]-1 do Sum:=Sum+sqr(FList[idx+j]);
    if sum<>0 then begin
      invLen:=1/sqrt(sum);
      for j:=0 to CVectorSize[FVectorType]-1 do FList[idx+j]:=FList[idx+j]*invLen;
    end;
  end;
end;

procedure _TBaseVectorList.Normalize3f;
var i,j,idx,n: integer;
    sum,invLen: single;
begin
  n:=CVectorSize[FVectorType]; if n>3 then n:=3;
  for i:=0 to Count-1 do begin
    idx:=i*CVectorSize[FVectorType]; Sum:=0;
    for j:=0 to n-1 do Sum:=Sum+sqr(FList[idx+j]);
    if sum<>0 then begin
      invLen:=1/sqrt(sum);
      for j:=0 to n-1 do FList[idx+j]:=FList[idx+j]*invLen;
    end;
  end;
end;

{ _TAffineVectorList }

procedure _TAffineVectorList.CombineItem(Index: Integer;
  const vector: TAffineVector; const f: Single);
var v: TAffineVector;
begin
  v:=Items[Index]; CombineVector(v,vector,@f); Items[Index]:=v;
end;

constructor _TAffineVectorList.Create;
begin
  inherited Create(vtVector);
end;

function _TAffineVectorList.Get(Index: Integer): TAffineVector;
begin
  Result:=GetAsVector3f(index);
end;

function _TAffineVectorList.getList: PAffineVectorArray;
begin
  result:=PAffineVectorArray(FData);
end;

procedure _TAffineVectorList.Normalize;
begin
  inherited Normalize3f;
end;

procedure _TAffineVectorList.Put(Index: Integer; const Value: TAffineVector);
begin
  SetAsVector3f(index,value);
end;

procedure _TAffineVectorList.TransformAsPoints(const matrix: TMatrix);
var i: integer;
begin
  for i:=0 to Count-1 do Items[i]:=VectorTransform(Items[I], matrix);
end;

procedure _TAffineVectorList.TransformAsVectors(const matrix: TMatrix);
var m: TAffineMatrix;
begin
  if FCount > 0 then begin SetMatrix(m, matrix); TransformAsVectors(m); end;
end;

procedure _TAffineVectorList.TransformAsVectors(const matrix: TAffineMatrix);
var i: integer;
begin
  for i:=0 to Count-1 do Items[i]:=VectorTransform(Items[I], matrix);
end;

procedure _TAffineVectorList.Translate(const delta: TAffineVector);
var v: TVector3f;
    i: integer;
begin
  for i:=0 to Count do begin
    v:=GetAsVector3f(i); AddVector(v,delta); SetAsVector3f(i,v);
  end;
end;

procedure _TAffineVectorList.TranslateItem(Index: Integer;
  const delta: TAffineVector);
var v: TVector3f;
begin
  v:=GetAsVector3f(index); AddVector(v,delta); SetAsVector3f(Index,v);
end;

{ _TVectorList }

procedure _TVectorList.CombineItem(Index: Integer; const vector: TAffineVector;
  const f: Single);
var v: TAffineVector;
begin
  v:=AffineVectorMake(Items[Index]); CombineVector(v,vector,@f);
  Items[Index]:=VectorMake(v);
end;

constructor _TVectorList.Create;
begin
  inherited Create(vtPoint);
end;

function _TVectorList.Get(Index: Integer): TVector;
begin
  Result:=GetAsVector4f(index);
end;

function _TVectorList.getList: PVectorArray;
begin
  result:=PVectorArray(FData);
end;

procedure _TVectorList.Normalize;
begin
  inherited Normalize3f;
end;

procedure _TVectorList.Put(Index: Integer; const Value: TVector);
begin
  SetAsVector4f(Index,Value);
end;

procedure _TVectorList.TransformAsPoints(const matrix: TMatrix);
var i: integer;
begin
  for i:=0 to Count-1 do Items[i]:=VectorTransform(Items[i], matrix);
end;

procedure _TVectorList.TransformAsVectors(const matrix: TAffineMatrix);
var i: integer;
    v: TAffineVector;
begin
  for i:=0 to Count-1 do begin
    v:=affineVectorMake(Items[i]);
    v:=VectorTransform(v, matrix); Items[i]:=VectorMake(v);
  end;
end;

procedure _TVectorList.TransformAsVectors(const matrix: TMatrix);
var m: TAffineMatrix;
begin
  if FCount > 0 then begin SetMatrix(m, matrix); TransformAsVectors(m); end;
end;

procedure _TVectorList.Translate(const delta: TAffineVector);
var v: TVector3f;
    i: integer;
begin
  for i:=0 to Count do begin
    v:=AffineVectorMake(Items[i]); AddVector(v,delta);
    Items[i]:=VectorMake(v);
  end;
end;

procedure _TVectorList.Translate(const delta: TVector);
var v: TVector;
    i: integer;
begin
  for i:=0 to Count do begin
    v:=Items[i]; AddVector(v,delta); Items[i]:=v;
  end;
end;

procedure _TVectorList.TranslateItem(Index: Integer;
  const delta: TAffineVector);
var v: TVector3f;
begin
  v:=AffineVectorMake(Items[Index]); AddVector(v,delta);
  Items[Index]:=VectorMake(v);
end;

procedure _TVectorList.TranslateItem(Index: Integer; const delta: TVector);
var v: TVector;
begin
  v:=Items[index]; AddVector(v,delta); Items[index]:=v;
end;

{ _TIntegerList }

function _TIntegerList.Get(Index: Integer): Integer;
begin
  result:=FList[Index];
end;

function _TIntegerList.getList: PIntegerArray;
begin
  result:=PIntegerArray(FData)
end;

procedure _TIntegerList.Put(Index: Integer; const Value: Integer);
begin
  FList[Index]:=Value;
end;

end.
