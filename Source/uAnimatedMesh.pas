unit uAnimatedMesh;

interface

uses classes, File3DS, Types3DS, VectorLists, VectorGeometry,
     SysUtilsLite, uMeshObjects, uVBO, uMiscUtils, uBaseClasses,
     uMaterials, uTextures, uMaterialObjects, OpenGL1x, OGLStateEmul;

Type
  TKeyFrameHeader = record
    Value: TVector;
    Visible: boolean;
    Time: single;
    vType: (kfPosition,kfRotation,kfScale,kfVisible);
  end;
  PKeyFrameHeader = ^TKeyFrameHeader;

  TKeyFrame = record
    Position: TVector;
    Rotation: TVector;
    Scale: TVector;
    Time: single;
    Visible: boolean;
  end;

  TKeyFrameList = class
    private
       FPFrames: TList;  //Ключевые кадры положения
       FRFrames: TList;  //Ключевые кадры поворота
       FSFrames: TList;  //Ключевые кадры масштабирования
       FHFrames: TList;  //Ключевые кадры видимости
       FPivot: TVector;
       FLocalMatrix: TMatrix;
       FCurrentModelMatrix: TMatrix;
       FFrameRate: single;
       FCurrentFrame: integer;
       FAnimLength: integer;

       function GetCount: integer;
       function GetInterpolatedValue(const v1,v2: PKeyFrameHeader; Time: single): TKeyFrameHeader;
       function GetFrame(Time: single): TKeyFrame;
    public
       constructor Create;
       destructor Destroy; override;

       function AddPosKey(const Pos: TVector; Time: integer): integer;
       function AddRotKey(const Rot: TVector; Time: integer): integer;
       function AddScaleKey(const Scale: TVector; Time: integer): integer;
       function AddVisKey(Hide: boolean; Time: integer): integer;
       function GetModelMatrix(Time: single): TMatrix;

       procedure Clear;

       property Frames[Time: single]: TKeyFrame read GetFrame; default;
       property FrameRate: single read FFrameRate write FFrameRate;
       property Count: integer read FAnimLength;

  end;

  THashArray = class
  private
    FCount: integer;
    FList: array of pointer;
    function getItem(index: integer): pointer;
    procedure SetCount(const Value: integer);
    procedure setItem(index: integer; const Value: pointer);
  public
    constructor Create;
    destructor Destroy; override;
    property Count: integer read FCount write SetCount;
    property Items[index: integer]: pointer read getItem write setItem; default;
    function Add(const Value: pointer): integer;
    procedure Clear;
  end;

  TAnimatedMesh = class(TVBOMeshObject)
    private
      FKeyFrameList: TList;
      FPlaying: boolean;
      FPlayLoop: boolean;
      FStartTime: double;
      FCurrentFrame: single;
      FCache: TIntegerBits;
      FTime: double;
      Fhash : array[0..65535] of THashArray;
      function Mat4x3To4x4(const M: TMeshMatrix):TMatrix;
      function Indexing3dsMesh(Buff: PVBOBuffer): PVBOBuffer;
      procedure IndexingHT(const HashTable: TVertexHashArray; Res: TList; Ind: TIntegerList);
      procedure setStartPlay(const Value: boolean);
      procedure RenderAnimatedList(const ViewMatrix: TMatrix; MatList: TList=nil);
      procedure RenderAnimatedBuffer(Index: integer; const ViewMatrix: TMatrix; MatList: TList=nil);
      procedure SetFrame(const Value: single);
      procedure BuildVBOSubMeshes(const Mesh3ds: TFile3DS);
    public
      constructor Create;
      destructor Destroy; override;

//      property KeyFrames: TKeyFrameList read FKeyFrames;
      property Play: boolean read FPlaying write setStartPlay;
      property PlayLoop: boolean read FPlayLoop write FPlayLoop;

      property StartTime: double read FStartTime;
      property CurrentTime: double read FTime;
     // property Frame: single read FCurrentFrame write SetFrame;

      Procedure RenderObject(const ViewMatrix: TMatrix);override;
      Procedure Load3dsMesh(Filename: string; TexPath: string = '');

  end;

implementation

function maxof(const V: array of integer): integer;
var i: integer;
begin
  result:=v[0];
  for i:=1 to high(v) do if v[i]>result then result:=v[i];
end;

procedure FreeList (var List:TList);
var i:integer;
    p:pointer;
begin
   if not assigned(List) then exit;
   for i:=0 to List.Count-1 do begin
       p:=list[i];dispose(p);end;
   List.Free; List:=nil;
end;

{ TKeyFrameList }

function TKeyFrameList.AddPosKey(const Pos: TVector; Time: integer): integer;
var P: PKeyFrameHeader;
begin
  new(p); p.Value:=pos; p.Time:=Time;
  result:=FPFrames.Add(p);
end;

function TKeyFrameList.AddRotKey(const Rot: TVector; Time: integer): integer;
var P: PKeyFrameHeader;
begin
  new(p); p.Value:=rot; p.Time:=Time;
  result:=FRFrames.Add(p);
end;

function TKeyFrameList.AddScaleKey(const Scale: TVector; Time: integer): integer;
var P: PKeyFrameHeader;
begin
  new(p); p.Value:=Scale; p.Time:=Time;
  result:=FSFrames.Add(p);
end;

function TKeyFrameList.AddVisKey(hide: boolean; Time: integer): integer;
var P: PKeyFrameHeader;
begin
  new(p); p.Visible:=not hide; p.Time:=Time;
  result:=FHFrames.Add(p);
end;

procedure TKeyFrameList.Clear;
var i:integer;
begin
//
end;

constructor TKeyFrameList.Create;
begin
  inherited;
  FPFrames:=TList.Create;
  FRFrames:=TList.Create;
  FSFrames:=TList.Create;
  FHFrames:=TList.Create;

  FFrameRate:=25;
  FCurrentFrame:=0;
end;

destructor TKeyFrameList.Destroy;
begin
  FreeList(FPFrames); FreeList(FRFrames);
  FreeList(FSFrames); FreeList(FHFrames);
  inherited;
end;

function TKeyFrameList.GetCount: integer;
begin
//  result:=FFrameList.Count;
end;

function TKeyFrameList.GetFrame(Time: single): TKeyFrame;
var i: integer;
    v1,v2: PKeyFrameHeader;
    v: TKeyFrameHeader;
begin
  with result do begin
    Scale:=vectormake(1,1,1,1);
    Position:=NullHmgPoint;
    Rotation:=NullHmgVector;
    Visible:=true;
  end;

  //Интерполируем положение между кадрами
  if FPFrames.Count=1 then begin
    v1:=FPFrames[0]; result.Position:=v1.Value;
  end else begin
    for i:=0 to FPFrames.Count-2 do begin
      v1:=FPFrames[i]; v2:=FPFrames[i+1];
      if (Time>=v1.Time) and (Time<v2.Time) then begin
        v:=GetInterpolatedValue(v1,v2,Time);
        result.Position:=v.Value; Break;
      end;
    end;
    if FPFrames.Count>0 then begin
      v1:=FPFrames[0]; v2:=FPFrames[FPFrames.Count-1];
      if Time<v1.Time then result.Position:=v1.Value;
      if Time>=v2.Time then result.Position:=v2.Value;
    end;
  end;

  //Интерполируем поворот между кадрами
  if FRFrames.Count=1 then begin
    v1:=FRFrames[0]; result.Rotation:=v1.Value;
  end else begin
    for i:=0 to FRFrames.Count-2 do begin
      v1:=FRFrames[i]; v2:=FRFrames[i+1];
      if (Time>=v1.Time) and (Time<v2.Time) then begin
        v:=GetInterpolatedValue(v1,v2,Time);
        result.Rotation:=v.Value; Break;
      end;
    end;
    if FRFrames.Count>0 then begin
      v1:=FRFrames[0]; v2:=FRFrames[FRFrames.Count-1];
      if Time<v1.Time then result.Rotation:=v1.Value;
      if Time>=v2.Time then result.Rotation:=v2.Value;
    end;
  end;

  //Интерполируем масштаб между кадрами
  if FSFrames.Count=1 then begin
    v1:=FSFrames[0]; result.Scale:=v1.Value;
  end else begin
    for i:=0 to FSFrames.Count-2 do begin
      v1:=FSFrames[i]; v2:=FSFrames[i+1];
      if (Time>=v1.Time) and (Time<v2.Time) then begin
        v:=GetInterpolatedValue(v1,v2,Time);
        result.Scale:=v.Value; Break;
      end;
    end;
    if FSFrames.Count>0 then begin
      v1:=FSFrames[0]; v2:=FSFrames[FSFrames.Count-1];
      if Time<v1.Time then result.Scale:=v1.Value;
      if Time>=v2.Time then result.Scale:=v2.Value;
    end;
  end;

  //Интерполируем видимость между кадрами
  if FHFrames.Count=1 then begin
    v1:=FHFrames[0]; result.Visible:=v1.Visible;
  end else begin
    for i:=0 to FHFrames.Count-2 do begin
      v1:=FHFrames[i]; v2:=FHFrames[i+1];
      if (Time>=v1.Time) and (Time<v2.Time) then begin
        v:=GetInterpolatedValue(v1,v2,Time);
        result.Visible:=v.Visible; Break;
      end;
    end;
    if FHFrames.Count>0 then begin
      v1:=FHFrames[0]; v2:=FHFrames[FHFrames.Count-1];
      if Time<v1.Time then result.Visible:=v1.Visible;
      if Time>=v2.Time then result.Visible:=v2.Visible;
    end;
  end;

  Result.time:=time;
end;

function TKeyFrameList.GetInterpolatedValue(const v1,
  v2: PKeyFrameHeader; Time: single): TKeyFrameHeader;
var k: double;
begin
  assert(v1.vType=v2.vType,'KeyFrame header has different type');
  result.vType:=v1.vType;
  result.Time:=Time;
  if v1.vType=kfVisible then begin
    if v1.Time<Time then result.Visible:=false else result.Visible:=true;
  end else begin
    if v1.Time=v2.Time then k:=0
    else k:=(Time-v1.Time)/(v2.Time-v1.Time);
    result.Value:=VectorLerp(v1.Value,v2.Value,k);
  end;
end;

function TKeyFrameList.GetModelMatrix(Time: single): TMatrix;
var P: TKeyFrame;
    m,ms,mt,mr,mp,mnp: TMatrix;
    np: TVector;
begin
  P:=GetFrame(Time);

  ms:=CreateScaleMatrix(P.Scale);
  mt:=CreateTranslationMatrix(P.Position);
  mr:=CreateRotationMatrix(AffineVectorMake(p.Rotation),p.Rotation[3]);

  np:=VectorNegate(FPivot);
  mp:=CreateTranslationMatrix(FPivot);
  mnp:=CreateTranslationMatrix(np);

  m:=FLocalMatrix;

  //Поворот вокруг заданной точки
  m:=MatrixMultiply(m,mp);
  m:=MatrixMultiply(m,mr);
  m:=MatrixMultiply(m,mnp);
  //Масштаб
  m:=MatrixMultiply(m,ms);
  //Позиция
  m:=MatrixMultiply(m,mt);

  result:=m;
end;

{ TAnimatedMesh }

procedure CalcVertexHash(Data: PVertexHashData);
var hBuff: array[0..10] of single;
    x: double;
begin
{  x:=Data.Vertex.T[0];
  Data.Vertex.T[0]:=Data.Vertex.T[1];
  Data.Vertex.T[0]:=x;
}
  hBuff[0]:=Data.Vertex.V[0];
  hBuff[1]:=Data.Vertex.V[1];
  hBuff[2]:=Data.Vertex.V[2];
  hBuff[3]:=Data.Vertex.T[0];
  hBuff[4]:=Data.Vertex.T[1];
  hBuff[5]:=Data.Vertex.T[2];
  hBuff[6]:=Data.Vertex.N[0];
  hBuff[7]:=Data.Vertex.N[1];
  hBuff[8]:=Data.Vertex.N[2];
  hBuff[9]:=Data.Vertex.SG;
  hBuff[10]:=Data.Vertex.matId;

  Data.Hash:=BufferHash(hBuff,44);
  with Data^ do begin
    Vertex.VHash:=BufferHash(Vertex.V,sizeof(TAffineVector));
    Vertex.THash:=BufferHash(Vertex.T,sizeof(TAffineVector));
    Vertex.NHash:=0;
  end;
  with Data.Vertex do begin
    x:=V[0]+V[1]+V[2]+T[0]+T[1]+T[2]+N[0]+N[1]+N[2]+SG+MatId;
    Data.Summ:=x;
  end;
end;

procedure TAnimatedMesh.BuildVBOSubMeshes(const Mesh3ds: TFile3DS);
var VBO: PVBOBuffer;
    obj: TObjectList;
    PMesh: PMesh3DS;
    SubMesh: PSubMesh;
    i,j,k,l,t,idx: integer;
    locmat: TMatrix;
    lmIdent: boolean;
    F: PFace3DS;
    FaceCount: integer;
    IndexList: array of integer;
    TexCoordsPres: boolean;
    MHT: array of record
      HashTable: TVertexHashArray;
      VertexCount: integer;
      TexCoords: boolean;
    end;

  procedure VertexToVBO(var Vertex: uVBO.TVertex; var vbuff: PVBOBuffer; index: integer);
  begin
    vbuff.Vertexes[index]:=Vertex.V;
    if TexCoordsPres then vbuff.TexCoords[index]:=Vertex.T;
  end;

  procedure Indexing(var vbuff: PVBOBuffer; var HashTable: TVertexHashArray);
  var i,j,n: integer;
      p: PVertexHashData;
      f: boolean;
      rescount: integer;
      h: word;
  begin
    FCache.ResetBits; for i:=0 to high(FHash) do FHash[i].Clear;
    n:=(length(HashTable) shr 16)+1;
    vbuff.Indices.Count:=length(HashTable); vbuff.Indices[0]:=0;
    vbuff.Vertexes.Count:=vbo.indices.Count;
    if TexCoordsPres then vbuff.TexCoords.Count:=vbo.indices.Count;
    VertexToVBO(HashTable[0].Vertex, vbuff,0);
    //vbuff.Vertexes[0]:=HashTable[0].Vertex.V;
    rescount:=1; n:=1;
    FCache.Bits[HashTable[0].Hash]:=true;
    HashTable[0].NewIndex:=0;
    Fhash[HashTable[0].Hash].Add(@HashTable[0]);
    for i:=1 to high(HashTable) do begin
      h:=HashTable[i].Hash; f:=false;
      if FCache.Bits[h] then begin
        for j:=0 to Fhash[h].Count-1 do begin
          p:=Fhash[h].Items[j];
          if p.Summ=HashTable[i].Summ then
            if  (p.Vertex.VHash=HashTable[i].Vertex.VHash)
            and (p.Vertex.THash=HashTable[i].Vertex.THash)
            and (p.Vertex.SG=HashTable[i].Vertex.SG)
            then begin
              if (VectorEquals(p.Vertex.V,HashTable[i].Vertex.V))
              and(VectorEquals(p.Vertex.T,HashTable[i].Vertex.T))
              then begin
                vbuff.Indices[n]:=p.NewIndex; f:=true; Break;
              end;
            end;
        end;
      end;
      if not f then begin
        vbuff.Indices[n]:=rescount;
        //vbuff.Vertexes[rescount]:=HashTable[i].Vertex.V;
        VertexToVBO(HashTable[i].Vertex, vbuff, rescount);
        Fhash[h].Add(@HashTable[i]); HashTable[i].NewIndex:=rescount;
        inc(rescount); FCache.Bits[h]:=true;
      end; inc(n);
    end;
    vbuff.Vertexes.Count:=rescount;
    if TexCoordsPres then vbuff.TexCoords.Count:=vbuff.Vertexes.Count;
    vbuff.Indices.Count:=n;
  end;

begin
  Obj:=Mesh3ds.Objects;
  FaceCount:=0; TexCoordsPres:=false;
  Materials.Clear;
  //Извлекаем список материалов
  for i:=0 to obj.MeshCount-1 do begin
    PMesh:=obj.Mesh[i]; FaceCount:=FaceCount+PMesh.NFaces;
    with pMesh^ do for j:=0 to NMats-1 do begin
      l:=Materials.IndexOf(MatArray[j].NameStr);
      if l<0 then Materials.Add(MatArray[j].NameStr);
    end;
  end;
  if Materials.Count=0 then Materials.Add('');

  //Резервируем место под массивы вершин
  SetLength(MHT,Materials.Count);
  for i:=0 to high(MHT) do begin
    FaceCount:=0;
    for j:=0 to obj.MeshCount-1 do begin
      PMesh:=obj.Mesh[j];
      for k:=0 to PMesh.NMats-1 do begin
        l:=Materials.IndexOf(PMesh.MatArray[k].NameStr);
        if l<0 then l:=0;
        if l=i then FaceCount:=FaceCount+PMesh.MatArray[k].NFaces;
      end;
    end;
    SetLength(MHT[i].HashTable,FaceCount*3);
    MHT[i].VertexCount:=0; MHT[i].TexCoords:=false;
  end;

  //Заполняем массивы вершин согласно материалам
  for i:=0 to obj.MeshCount-1 do begin
    PMesh:=obj.Mesh[i]; locmat:=Mat4x3To4x4(PMesh.LocMatrix);
    lmIdent:=MatrixEquals(locmat,IdentityHmgMatrix);
    with PMesh^ do
    for j:=0 to NMats-1 do begin
      l:=Materials.IndexOf(MatArray[j].NameStr);
      if l<0 then l:=0;
      for k:=0 to MatArray[j].NFaces-1 do begin
        f:=@FaceArray[MatArray[j].FaceIndex[k]];
        with MHT[l] do begin
          idx:=MHT[l].VertexCount; HashTable[idx].Index:=idx;
          HashTable[Idx].Vertex.V:=TAffineVector(VertexArray[f.V1]);
          //if lmIdent then with HashTable[Idx].Vertex do V:=VectorTransform(V,locMat);
          HashTable[Idx].Vertex.N:=NullVector;
          if assigned(SmoothArray) then
            HashTable[Idx].Vertex.SG:=SmoothArray[k]
          else HashTable[Idx].Vertex.SG:=0;
          HashTable[Idx].Vertex.matId:=l;
          if assigned(TextArray) then begin
            HashTable[Idx].Vertex.T:=AffineVectorMake(TextArray[f.V1].U,TextArray[f.V1].V,0);
            TexCoords:=true;
          end else HashTable[Idx].Vertex.T:=AffineVectorMake(0,0,0);
          CalcVertexHash(@HashTable[Idx]);
          MHT[l].VertexCount:=MHT[l].VertexCount+1;

          idx:=MHT[l].VertexCount; HashTable[idx].Index:=idx;
          HashTable[Idx].Vertex.V:=TAffineVector(VertexArray[f.V2]);
          //if lmIdent then with HashTable[Idx].Vertex do V:=VectorTransform(V,locMat);
          HashTable[Idx].Vertex.N:=NullVector;
          if assigned(SmoothArray) then
            HashTable[Idx].Vertex.SG:=SmoothArray[k]
          else HashTable[Idx].Vertex.SG:=0;
          HashTable[Idx].Vertex.matId:=l;
          if assigned(TextArray) then begin
            HashTable[Idx].Vertex.T:=AffineVectorMake(TextArray[f.V2].U,TextArray[f.V2].V,0);
            TexCoords:=true;
          end else HashTable[Idx].Vertex.T:=AffineVectorMake(0,0,0);
          CalcVertexHash(@HashTable[Idx]);
          MHT[l].VertexCount:=MHT[l].VertexCount+1;

          idx:=MHT[l].VertexCount; HashTable[idx].Index:=idx;
          HashTable[Idx].Vertex.V:=TAffineVector(VertexArray[f.V3]);
          //if lmIdent then with HashTable[Idx].Vertex do V:=VectorTransform(V,locMat);
          HashTable[Idx].Vertex.N:=NullVector;
          if assigned(SmoothArray) then
            HashTable[Idx].Vertex.SG:=SmoothArray[k]
          else HashTable[Idx].Vertex.SG:=0;
          HashTable[Idx].Vertex.matId:=l;
          if assigned(TextArray) then begin
            HashTable[Idx].Vertex.T:=AffineVectorMake(TextArray[f.V3].U,TextArray[f.V3].V,0);
            TexCoords:=true;
          end else HashTable[Idx].Vertex.T:=AffineVectorMake(0,0,0);
          CalcVertexHash(@HashTable[Idx]);
          MHT[l].VertexCount:=MHT[l].VertexCount+1;
        end;
      end;
    end;
  end;

  //Индексируем вершины и формируем буфер VBO
  for i:=0 to high(MHT) do begin
    if length(MHT[i].HashTable)>0 then begin
      new(vbo); InitVBOBuff(vbo^,GL_TRIANGLES,DrawElements);
      vbo.RenderBuffs:=[uNormals,uIndices];
      vbo.MaterialFunc:=MaterialSetter;
      vbo.MatName:=Materials[i];
      if MHT[i].TexCoords then include(vbo.RenderBuffs,uTexCoords);
      TexCoordsPres:=MHT[i].TexCoords;
      Indexing(vbo,MHT[i].HashTable);
{     //Debug mode (Triangles)
      for j:=0 to high(MHT[i].HashTable) do begin
        vbo.Vertexes.Add(MHT[i].HashTable[j].Vertex.V);
        if MHT[i].TexCoords then
          vbo.TexCoords.Add(MHT[i].HashTable[j].Vertex.T);
        vbo.Indices.Add(j);
      end;}
      vbo.Normals.Count:=vbo.Vertexes.Count;
      RebuildNormals(vbo);
  //    SaveVBOAsText(vbo^,'e:\vbo_'+Materials[i]+'.txt');
      GenVBOBuff(vbo^,false);
      Exclude(vbo.RenderBuffs,uVAO);
      MeshList.Add(vbo);
    end;
  end;
end;

constructor TAnimatedMesh.Create;
var i: integer;
begin
  inherited;
  FCache:=TIntegerBits.Create; FCache.Size:=65536;
  for i:=0 to high(FHash) do FHash[i]:=THashArray.Create;
  FKeyFrameList:= TList.Create;
  FStartTime:=-1;
  FPlaying := false;
end;

destructor TAnimatedMesh.Destroy;
var i: integer;
begin
  FreeObjectList(FKeyFrameList);
  FCache.Free;
  for i:=0 to high(Fhash) do Fhash[i].Free;
  inherited;
end;

function TAnimatedMesh.Mat4x3To4x4(const M: TMeshMatrix):TMatrix;
var i,j: integer;
begin
   for i:=0 to 3 do begin
      for j:=0 to 2 do result[i,j]:=M[i*3+j];
      result[i,3]:=0;
   end; result[3,3]:=1;
   InvertMatrix(Result);
end;

function TAnimatedMesh.Indexing3dsMesh(Buff: PVBOBuffer): PVBOBuffer;
var i,j, vh1,vh2,th1,th2: integer;
    Temp: PVBOBuffer;
    v1,v2,v3,t1,t2,N,N1: TAffineVector;
    sg1,sg2: integer;
    found: boolean;
    HashTable: array of record
      VertexHash, TCHash: integer;
    end;
    HashSize: integer;
   function HashKey(const v : TAffineVector; hashSize : Integer) : Integer;
   begin
      Result:=((    Integer(PIntegerArray(@v)[0])
                xor Integer(PIntegerArray(@v)[1])
                xor Integer(PIntegerArray(@v)[2])) shr 16) and hashSize;
   end;

begin
  hashSize:=(1 shl MaxInteger(Integer(0), Integer(Trunc(log2(Buff.Vertexes.Count/48)))))-1;
  if hashSize<7 then hashSize:=7;
  if hashSize>65535 then hashSize:=65535;
  setlength(HashTable, Buff.Vertexes.Count);
  for i:=0 to Buff.Vertexes.Count-1 do begin
    HashTable[i].VertexHash:=HashKey(Buff.Vertexes[i],HashSize);
     if uTexCoords in Buff.RenderBuffs then begin
       HashTable[i].TCHash:=HashKey(Buff.TexCoords[i],HashSize)
     end else HashTable[i].TCHash:=0;
  end;
    new(Temp); InitVBOBuff(Temp^,GL_TRIANGLES,DrawElements);
    for i:=0 to Buff.Indices.Count-1 do begin
       v1:=Buff.Vertexes[Buff.Indices[i]];
//       vh1:=HashTable[Buff.Indices[i]].VertexHash;
//       th1:=HashTable[Buff.Indices[i]].TCHash;
       if uTexCoords in Buff.RenderBuffs then
          t1:=Buff.TexCoords[Buff.Indices[i]];
          sg1:=round(t1[2]); t1[2]:=0;
       found:=false; j:=0;
       while (not found) and (j<Temp.Vertexes.Count) do begin
          v2:=Temp.Vertexes[j]; found:=false;
//          vh2:=HashTable[j].VertexHash;
//          th2:=HashTable[j].TCHash;
          found:=VectorDistance2(v1,v2)<1E-5;
          if uTexCoords in Buff.RenderBuffs then
             t2:=Temp.TexCoords[j];
//          if (vh1=vh2) and (th1=th2) then begin
            if uTexCoords in Buff.RenderBuffs then begin
             sg2:=round(t2[2]); t2[2]:=0;
             found:=found and (VectorDistance2(t1,t2)<1E-5);
             found:=found and (sg1=sg2);
            end;
//          end;
          if not found then inc(j);
       end;
       if found then Temp.Indices.Add(j) else begin
          Temp.Indices.Add(Temp.Vertexes.Add(v1)); t1[2]:=sg1;
          if uTexCoords in Buff.RenderBuffs then
             Temp.TexCoords.Add(t1);
       end;
    end;
    if uNormals in Buff.RenderBuffs then begin
       Temp.Normals.Count:=Temp.Vertexes.Count;
       for i:=0 to Temp.Normals.Count-1 do Temp.Normals[i]:=NullVector;
       for i:=0 to (Temp.Indices.Count div 3)-1 do begin
         v1:=Temp.Vertexes[Temp.Indices[i*3]];
         v2:=Temp.Vertexes[Temp.Indices[i*3+1]];
         v3:=Temp.Vertexes[Temp.Indices[i*3+2]];
         N:=VectorCrossProduct(VectorSubtract(v2,v1),VectorSubtract(v3,v1));
         //NegateVector(N);
         N1:=Temp.Normals[Temp.Indices[i*3]];
         Temp.Normals[Temp.Indices[i*3]]:=VectorAdd(N1,N);
         N1:=Temp.Normals[Temp.Indices[i*3+1]];
         Temp.Normals[Temp.Indices[i*3+1]]:=VectorAdd(N1,N);
         N1:=Temp.Normals[Temp.Indices[i*3+2]];
         Temp.Normals[Temp.Indices[i*3+2]]:=VectorAdd(N1,N);
       end;
    end;
    Temp.RenderBuffs:=Buff.RenderBuffs;
    result:=temp;
end;

procedure TAnimatedMesh.IndexingHT(const HashTable: TVertexHashArray;
  Res: TList; Ind: TIntegerList);
var i,j,n: integer;
    p: PVertexHashData;
    f: boolean;
    rescount: integer;
    h: word;
begin
  FCache.ResetBits; n:=(length(HashTable) shr 16)+1;
  ind.Count:=length(HashTable); Ind[0]:=0;
  Res.Count:=ind.Count;
  Res[0]:=@HashTable[0]; rescount:=1; n:=1;
  FCache.Bits[HashTable[0].Hash]:=true;
  for i:=0 to high(FHash) do FHash[i].Clear;
  HashTable[0].NewIndex:=0;
  Fhash[HashTable[0].Hash].Add(@HashTable[0]);
  for i:=1 to high(HashTable) do begin
    h:=HashTable[i].Hash; f:=false;
    if FCache.Bits[h] then begin
      for j:=0 to Fhash[h].Count-1 do begin
        p:=Fhash[h].Items[j];
        if p.Summ=HashTable[i].Summ then
          if  (p.Vertex.VHash=HashTable[i].Vertex.VHash)
          and (p.Vertex.THash=HashTable[i].Vertex.THash)
          and (p.Vertex.SG=HashTable[i].Vertex.SG)
          then begin
//            if (VectorEquals(p.Vertex.V,HashTable[i].Vertex.V))
//            and(VectorEquals(p.Vertex.T,HashTable[i].Vertex.T))
            //then begin
              Ind.List[n]:=p.NewIndex; f:=true; Break;
            //end;
          end;
      end;
    end;
    if not f then begin
      Ind.List[n]:=rescount; Res.List[rescount]:=@HashTable[i];
      Fhash[h].Add(@HashTable[i]); HashTable[i].NewIndex:=rescount;
      inc(rescount); FCache.Bits[h]:=true;
    end; inc(n);
  end; Res.Count:=rescount; ind.Count:=n;
{var i,j: integer;
    p: PVertexHashData;
    f: boolean;
    rescount: integer;
begin
  FCache.ResetBits;
  ind.Clear; ind.Capacity:=length(HashTable); Ind.Add(0);
  Res.Capacity:=ind.Capacity; Res.Clear;
  Res.Add(@HashTable[0]); rescount:=1;
  FCache.Bits[HashTable[0].Hash]:=true;
  for i:=1 to high(HashTable) do begin
    j:=0; f:=false;
    if FCache.Bits[HashTable[i].Hash] then begin
    repeat
      if (j<=rescount) then begin
        p:=Res[j];
        if (p.Hash=HashTable[i].Hash) and (p.Summ=HashTable[i].Summ)
        then begin
          if (VectorEquals(p.Vertex.V,HashTable[i].Vertex.V))
          //and(VectorEquals(p.Vertex.T,HashTable[i].Vertex.T))
          then
          f:=true; end else inc(j);
      end;
    until f or (j=rescount);
    end else j:=rescount;
    if j=ResCount then begin
      Ind.Add(rescount); Res.Add(@HashTable[i]); inc(rescount);
      FCache.Bits[HashTable[i].Hash]:=true;
    end else Ind.Add(j);
  end;
}
end;

procedure AddVertexToHashList(var HashTable: TVertexHashArray; Index,SG: integer;
  const V: TPoint3DS; const T: TTexVert3DS);
var hBuff: array[0..8] of single;
    x: double;
begin
  HashTable[index].Vertex.V:=TAffineVector(V);
  HashTable[index].Vertex.T:=AffineVectorMake(t.u,t.v,0);
  HashTable[index].Vertex.N:=NullVector;
  HashTable[index].Vertex.SG:=SG;
  hBuff[0]:=HashTable[index].Vertex.V[0];
  hBuff[1]:=HashTable[index].Vertex.V[1];
  hBuff[2]:=HashTable[index].Vertex.V[2];
  hBuff[3]:=HashTable[index].Vertex.T[0];
  hBuff[4]:=HashTable[index].Vertex.T[1];
  hBuff[5]:=HashTable[index].Vertex.T[2];
  hBuff[6]:=HashTable[index].Vertex.N[0];
  hBuff[7]:=HashTable[index].Vertex.N[1];
  hBuff[8]:=HashTable[index].Vertex.N[2];
  HashTable[index].Hash:=BufferHash(hBuff,36);
  with HashTable[index] do begin
    Vertex.VHash:=BufferHash(Vertex.V,sizeof(TAffineVector));
    Vertex.THash:=BufferHash(Vertex.T,sizeof(TAffineVector));
    Vertex.NHash:=0;
  end;
  with HashTable[index].Vertex do begin
    x:=V[0]+V[1]+V[2]+T[0]+T[1]+T[2]+N[0]+N[1]+N[2]+SG;
    HashTable[index].Summ:=x;
  end; HashTable[index].Index:=index;
end;

procedure TAnimatedMesh.Load3dsMesh(Filename,TexPath: string);
var Mesh3ds: TFile3DS;
//    obj: TObjectList;
//    PMesh: PMesh3DS;
    i,j,k,l,c:integer;
{    buff, temp: PVBOBuffer;
    V: TPoint3DS;
    T: TTexVert3DS;
    F,f2,ft: TFace3DS;
    v1,v2,v3, d1,d2: TAffineVector;
    v4: TVector;
}
    mat: TObjMat3DS;
    mat3ds: PMaterial3DS;
//    locmat: TMatrix;
    mId: integer;
//    Materials: TStringList;
    glMat: TMaterial;
    glTex: TTexture;
    matObj: TMaterialObject;
    MMName: string;
    path: string;
    maxKeys: integer;
{    HashTable: TVertexHashArray;

    VertexList: TList;
    Vertex: PVertexHashData;
    FG1,FG2: PFaceGroup;
}
    FS: TFileStream;
begin
  FS:=TFileStream.Create(FileName,fmOpenRead);
  Mesh3ds:=TFile3DS.Create; Mesh3ds.LoadFromStream(fs);

  //Materials:=TStringList.Create;
  FreeVBOList(MeshList);
  BuildVBOSubMeshes(Mesh3ds);

//-------------------------
//-------Создаем материалы и загружаем текстуры
//-------------------------
  for i:=0 to Materials.Count-1 do begin
     mat3ds:=Mesh3ds.Materials.MaterialByName[Materials[i]];
     matObj:=FMatObjLib.MaterialByName(Materials[i]);
     if not assigned(matObj) then
       matObj:=FMatObjLib.AddNewMaterialObject(Materials[i]);
     glMat:=FMaterials.MaterialByName(Materials[i]);
     if not assigned(glMat) then glMat:=FMaterials.AddNewMaterial(Materials[i]);
     matObj.AttachMaterial(glMat);

     with glMat do begin
        Properties.AmbientColor.ColorVector:=
          vectormake(mat3ds.Ambient.R,mat3ds.Ambient.G,mat3ds.Ambient.B);
        Properties.DiffuseColor.ColorVector:=
          vectormake(mat3ds.Diffuse.R,mat3ds.Diffuse.G,mat3ds.Diffuse.B, 1 - Abs(mat3ds.Transparency));
        Properties.SpecularColor.ColorVector:=
          vectormake(mat3ds.Specular.R,mat3ds.Specular.G,mat3ds.Specular.B);
        VectorScale(Properties.SpecularColor.ColorVector, mat3ds.ShinStrength);
        Properties.Shininess:=trunc(mat3ds.Shininess);
     end;
     if abs(mat3ds.Transparency) <> 0 then matObj.Blending.SetByMode(bmTransparency)
     else matObj.Blending.SetByMode(bmOpaque);
     if Trim(mat3ds.Texture.Map.NameStr) <> '' then begin
       path:=ExtractFilePath(FileName);
       if pos(':',TexPath)>0 then path:=texPath else begin
         if (path<>'') and (path[Length(path)]<>'\') then path:=path+'\';
         if texpath<>'' then begin
           path:=path+texpath;
           if path[Length(path)]<>'\' then path:=path+'\';
         end;
       end;

       glTex:=FTextures.TextureByLocation(path+mat3ds.Texture.Map.NameStr);
//       FTextures.TextureByName(Materials[i]);
       if not assigned(glTex) then begin
         gltex:=FTextures.AddNewTexture(Materials[i]);
         with glTex do begin
            SetTarget(ttTexture2D);
            SetFilters(mnLinearMipmapLinear, mgLinear);
            LoadFromFile(path+mat3ds.Texture.Map.NameStr);
            if not assigned (TexDesc.Data) then begin
              glTex.Free; glTex:=nil;
            end else begin
              TextureMode := tcModulate;
              //TextureMode := tcReplace;
              TwoSides:=mat3ds.TwoSided;
              with mat3ds.Texture.Map do begin
                //if (UScale<>1) or (VScale<>1) or (UOffset<>0) or (VOffset<>0)then begin
                  TextureMatrix:=CreateScaleMatrix(AffineVectorMake(UScale, VScale, 1));
                  TextureMatrix:=MatrixMultiply(TextureMatrix,
                    CreateTranslationMatrix(AffineVectorMake((1-frac(UOffset))*UScale, (frac(VOffset))*VScale, 0)));
  //              end;
              end;
            end;
         end;
       end;
       matObj.AttachTexture(glTex);
     end;
  end;
(*
  //Materials.Free;
  //KeyFrame Animations
  FKeyFrameList.Count:=MeshList.Count;
  for k:=0 to MeshList.Count-1 do FKeyFrameList[k]:=TKeyFrameList.Create;
  with Mesh3ds.KeyFramer do begin
    for j:=0 to MeshMotionCount-1 do begin
      MMName:=MeshMotion[j].NameStr;
      for k:=0 to MeshList.Count-1 do begin
        Temp:=MeshList[k];
        if (Temp.Name=MMName) or
           ((copy(Temp.Name,1,length(MMName))=MMName) and ((pos('_|_',Temp.Name)>0)))
        then begin
          for i:=0 to MeshMotion[j].NPKeys-1 do with MeshMotion[j]^ do
            TKeyFrameList(FKeyFrameList[k]).AddPosKey(Vectormake(Pos[i].X,Pos[i].Y,Pos[i].Z,1),PKeys[i].Time);
          for i:=0 to MeshMotion[j].NRKeys-1 do with MeshMotion[j]^ do
            TKeyFrameList(FKeyFrameList[k]).AddRotKey(Vectormake(Rot[i].X,Rot[i].Y,Rot[i].Z,Rot[i].Angle),RKeys[i].Time);
          for i:=0 to MeshMotion[j].NSKeys-1 do with MeshMotion[j]^ do
            TKeyFrameList(FKeyFrameList[k]).AddScaleKey(Vectormake(Scale[i].X,Scale[i].Y,Scale[i].Z,1),SKeys[i].Time);
          for i:=0 to MeshMotion[j].NHKeys-1 do with MeshMotion[j]^ do
            TKeyFrameList(FKeyFrameList[k]).AddVisKey(true,PKeys[i].Time);
          with MeshMotion[j]^ do
            TKeyFrameList(FKeyFrameList[k]).FPivot:=Vectormake(Pivot.X,Pivot.Y,Pivot.Z,1);
          TKeyFrameList(FKeyFrameList[k]).FLocalMatrix:=temp.LocalMatrix;
          TKeyFrameList(FKeyFrameList[k]).FAnimLength:=Settings.Anim.Length;
        end;
      end;
    end;
  end;
*)
  FS.Free; Mesh3ds.Free;
end;

procedure TAnimatedMesh.RenderAnimatedBuffer(Index: integer; const ViewMatrix: TMatrix; MatList: TList);
var i:integer;
    p: PVBOBuffer;
    dt,FrameN:double;
    FrameTime: double;
    KeyFrames: TKeyFrameList;
    m: TMatrix;
begin
  if FStartTime=-1 then FStartTime:=FTime;
  dt:=FTime-fStartTime; //время в секундах
  KeyFrames:=FKeyFrameList[Index];
  FrameTime:=1/KeyFrames.FFrameRate;
  FrameN:=dt/FrameTime;
  if FrameN>=KeyFrames.FAnimLength then begin
    if FPlayLoop then FrameN:=FrameN-trunc(FrameN/KeyFrames.Count)*KeyFrames.FAnimLength
    else FrameN:=KeyFrames.FAnimLength-1;
  end;
  m:=KeyFrames.GetModelMatrix(FrameN);
  KeyFrames.FCurrentModelMatrix:=m;
  m:=MatrixMultiply(m, Matrices.WorldMatrix);
  m:=MatrixMultiply(m,ViewMatrix);
  p:=MeshList[Index];
  if assigned(MatList) then RenderVBOBuffer(p^,MatList)
  else begin
    glLoadMatrixf(PGLFloat(@m));
    RenderVBOBuffer(p^);
  end;
end;

procedure TAnimatedMesh.RenderAnimatedList(const ViewMatrix: TMatrix; MatList: TList);
var i:integer;
    p: PVBOBuffer;
    dt,FrameN:double;
    KeyFrames: TKeyFrameList;
    m: TMatrix;
begin
  dt:=FTime-fStartTime;
  for i:=0 to MeshList.Count-1 do begin
     KeyFrames:=FKeyFrameList[i];
     FrameN:=dt/KeyFrames.FFrameRate;
     if FPlayLoop then FrameN:=FrameN-trunc(FrameN/KeyFrames.Count)*KeyFrames.FAnimLength
     else FrameN:=KeyFrames.FAnimLength-1;
     m:=KeyFrames.GetModelMatrix(FrameN);
     m:=MatrixMultiply(m, Matrices.WorldMatrix);
     m:=MatrixMultiply(m,ViewMatrix);
     p:=MeshList[i];
     if assigned(MatList) then RenderVBOBuffer(p^,MatList)
     else begin
       glLoadMatrixf(PGLFloat(@m));
       RenderVBOBuffer(p^);
     end;
  end;
end;

procedure TAnimatedMesh.RenderObject(const ViewMatrix: TMatrix);
var m,mv,ProjMatrix: TMatrix;
    F: TFrustum;
    i,rcount: integer;
    ProxyObj: TVBOMeshObject;
    pm: PMatrix;
    singleMat: boolean;
    CMName: string;
    P: PVBOBuffer;
    ActiveMaterial: string;
    mat: TMaterialObject;
begin
  if FMeshType = mtInstance then exit;
  if (not WorldMatrixUpdated) then UpdateWorldMatrix;
  m:=MatrixMultiply(Matrices.WorldMatrix,ViewMatrix);
  if FProxyList.Count>0 then RebuildProxyList(ViewMatrix, m);

  glPushMatrix;

    glLoadMatrixf(PGLFloat(@m));

    if FBO.Active then FBO.Apply;

    if NoZWrite then glDepthMask(False) else glDepthMask(True);
    if NoDepthTest then glDisable(GL_DEPTH_TEST) else glEnable(GL_DEPTH_TEST);
      if FUseRenderList then rcount:=FRenderList.Count else rcount:=MeshList.Count;
      if rcount>0 then begin
      singleMat:=false; i:=0;
      if FMeshType<>mtParticles then begin
         if TextureId<>0 then begin
               ResetBlending;
               glActiveTexture(GL_TEXTURE0);
               glEnable(GL_TEXTURE_2D);
               glBindTexture(GL_TEXTURE_2D, TextureId);
               singleMat:=true;
         end else begin
           if FMaterialObject.Active then begin
           FMaterialObject.Apply(onMaterialApply);
           singleMat:=true;
           end else FMaterialObject.Blending.Apply;
         end;
      end else singleMat:=true;
      if singleMat then begin
        if assigned(onBeforeRender) then onBeforeRender(self);
        if FPlaying then begin
          if FProxyMatrixList.Count>0 then
             RenderAnimatedList(ViewMatrix,FProxyMatrixList)
          else RenderAnimatedList(ViewMatrix);
        end else begin
          if FProxyMatrixList.Count>0 then
             RenderVBOList(MeshList,FProxyMatrixList)
          else RenderVBOList(MeshList);
        end;

        if assigned(onAfterRender) then onAfterRender(self);
        FMaterialObject.UnApply(onMaterialUnApply);
//          if assigned(FMaterial) then FMaterial.UnApply;
//          if assigned(FTexture) then FTexture.UnApply;
        ResetBlending;
        glDisable(GL_TEXTURE_2D);
      end else begin
      //MultiMaterial
        P := MeshList[0]; CMName:=P.MatName;
        ActiveMaterial:=''; i:=0;
        mat:=SetMaterialName(CMName); ActiveMaterial:=CMName;
        if assigned(mat) then begin
          if mat.Active then mat.Apply(onMaterialApply);
        end else begin
          mat.Blending.Apply;
          GLStateCache.MaterialCache.Reset;
          glDisable(GL_TEXTURE_2D);
        end;
        repeat
           //Render+++++++++++++++++
           if assigned(onBeforeRender) then onBeforeRender(self);
           P := MeshList[i];
{
           if FProxyMatrixList.Count>0 then
              RenderAnimatedBuffer(i,ViewMatrix,FProxyMatrixList)
           else RenderAnimatedBuffer(i,ViewMatrix);
}
           if FProxyMatrixList.Count>0 then
              RenderVBOBuffer(p^,FProxyMatrixList)
           else RenderVBOBuffer(p^);

           if assigned(onAfterRender) then onAfterRender(self);
           //Render-----------------
           inc(i);
           if i<rcount then begin
               P := MeshList[i]; CMName:=P.MatName;
           end else CMName:='';
           if ((i=rcount) or (CMName<>ActiveMaterial)) then
           if assigned(mat) then begin
             if FMaterialObject.Active then begin
               FMaterialObject.UnApply(onMaterialUnApply);
               ResetBlending;
             end else begin
               ResetBlending;
               GLStateCache.MaterialCache.Reset;
               glDisable(GL_TEXTURE_2D);
             end;
           end;
           if CMName<>ActiveMaterial then begin
             if CMName<>'' then begin
               mat:=SetMaterialName(CMName); ActiveMaterial:=CMName;
               if assigned(mat) then begin
                 if mat.Active then begin
                   mat.Apply(onMaterialApply);
                 end else mat.Blending.Apply;
               end else glDisable(GL_TEXTURE_2D);
             end else begin
               mat:=nil; ResetBlending;
               OGLStateEmul.GLStateCache.MaterialCache.Reset;
               glDisable(GL_TEXTURE_2D);
             end;
           end;
        until i=rcount;
      end;
      ResetBlending;
    end;//RCount<=0
    if NoZWrite then glDepthMask(true);
    if NoDepthTest then glEnable(GL_DEPTH_TEST);

    if FBO.Active then FBO.UnApply;
  glPopMatrix;
end;

procedure TAnimatedMesh.SetFrame(const Value: single);
var i:integer;
    p: PVBOBuffer;
    dt,FrameN:double;
    FrameTime: double;
    KeyFrames: TKeyFrameList;
    m: TMatrix;
begin
  FCurrentFrame := Value;
{  if FStartTime=-1 then FStartTime:=FTime;
  dt:=FTime-fStartTime; //время в секундах
  KeyFrames:=FKeyFrameList[Index];
  FCurrentFrame := Value;
  if Value>=KeyFrames.FAnimLength then begin
    if FPlayLoop then FrameN:=FrameN-trunc(FrameN/KeyFrames.Count)*KeyFrames.FAnimLength
    else FrameN:=KeyFrames.FAnimLength-1;
  end;
  m:=KeyFrames.GetModelMatrix(FrameN);
  m:=MatrixMultiply(m, Matrices.WorldMatrix);
  m:=MatrixMultiply(m,ViewMatrix);
}
end;

procedure TAnimatedMesh.setStartPlay(const Value: boolean);
begin
  FPlaying := Value;
  FStartTime:=-1;
  FCurrentFrame:=0;
end;

{ THashArray }

function THashArray.Add(const Value: pointer): integer;
begin
  FList[FCount]:=Value; SetCount(FCount+1);
end;

procedure THashArray.Clear;
begin
  FCount:=0;
end;

constructor THashArray.Create;
begin
  inherited;
  setlength(FList,1); FCount:=0;
end;

destructor THashArray.Destroy;
begin
  FList:=nil;
  inherited;
end;

function THashArray.getItem(index: integer): pointer;
begin
  result:=FList[Index]
end;

procedure THashArray.SetCount(const Value: integer);
begin
  FCount := Value;
  if length(FList)<=FCount then SetLength(FList,FCount*2);
end;

procedure THashArray.setItem(index: integer; const Value: pointer);
begin
  FList[index]:=Value;
end;

end.
