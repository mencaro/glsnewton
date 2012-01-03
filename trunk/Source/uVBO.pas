{ TODO : Закончить тесселяцию: добавить переиндексацию новой геометрии
         Тесселяция для сабмешей }
unit uVBO;

interface

uses VectorTypes, VectorGeometry, VectorLists, uMiscUtils,
     Classes, MeshUtils, Types, OpenGL1x, OGLStateEmul;

type
  TVBORenderType = (DrawArrays, DrawElements, DrawRangedElements, DrawMultiElements);
  TRenderBuff = set of (uNormals, uTexCoords, uIndices, uSecondaryTexure,
                        uColors, uMultitexture, uExTexEnvMode, uVAO);
  TBufferStruct = (bsSeparated, bsInterleaved, bsMultiBuffer);
  TBindState = set of (sActivate, sDeactivate, sUnchanged, sCheck);
  TUpdateBuff = set of (upVertex, upColor, upNormal, upTexCoord, upSecTexCoord);


  TQuadIndices = record
    v1,v2,v3,v4: integer;
  end;
  TQuadVertices = record
    v1,v2,v3,v4: TAffineVector;
  end;

  PExtents = ^TExtents;
  TExtents = record
     emin,emax: TAffineVector;
  end;

  PVBOBuffer = ^TVBOBuffer;

// Types for TVBOIndiceAdapter
//====================================================
  TVertexAttrib = (vaVertex, vaTexCoords, vaNormals);
  TVertexAttribs = set of TVertexAttrib;

  // Vector components count (1-4)
  TVectorType = (vtSingle, vtDouble, vtVector, vtPoint, vtMat3, vtMat4);
  TSingleArray = array of single;

  TAttribType = (atVertex,atNormal,atTexCoord,atColor,atUserAttrib,atInterleaved,
    atTexCoord0,atTexCoord1,atTexCoord2,atTexCoord3,atTexCoord4,atTexCoord5,
    atTexCoord6,atTexCoord7, atIndices);
  TAttribTypes = set of TAttribType;

  TInterleavedAttrib = record
    offset: cardinal;
    size: cardinal;
    stride: cardinal;
    Name: string;
    vaType: TAttribType;
  end;

  TAttribListType = (ltVectorList, ltIntegerList);

  PVertexAttribData = ^TVertexAttribData;
  TVertexAttribData = record
    Data: pointer;
    Size: integer;
    Count: integer;
    List: TObject;
    Name: Ansistring;
    AttribLoc: integer;
    vaType: TAttribType;
    ListType: TAttribListType;
  end;


  TVertex = record
    V,T,N: TaffineVector;
    SG: integer;
    VHash,THash,NHash: word;
  end;

  TVertexHashData = record
    Vertex: TVertex;
    Index: integer;
    NewIndex: integer;
    Hash: word;
    Summ: double;
  end;
  PVertexHashData = ^TVertexHashData;

  TVertexHashArray = array of TVertexHashData;

  TFaceGroup = record
    vOffset, tOffset, nOffset: integer;
    vCount,tCount,nCount: integer;
    GroupName: string[80];
    MaterialName: string[80];
    MatNameHash: integer;
    SmoothingGroup: integer;
    Processed: boolean;
  end; PFaceGroup = ^TFaceGroup;

//======================================================
  TMatAction = (maApply, maUnApply);
  TMaterialSetter = procedure (const MatName: string; Action: TMatAction) of object;

  TSubMesh = record
    Name: string;
    PrimType: Integer;
    VertexCount: Integer;
    StartingVertex: Integer;
    StartingIndex: Integer;
    PrimitiveCount: Integer;
    Extents: TExtents;
    MaterialName: string;
  end;
  PSubMesh = ^TSubMesh;

  TMultiObjectMesh = class;

  TBuiltinMeshObject = class
  private
    FSourceMesh: TMultiObjectMesh;
    FIndicesOffset: integer;
    FElementsCount: integer;
    FMaterialName: string;
    FGroupName: string;

    function getIndices(index: integer): integer;
    function getNormal(index: integer): TAffineVector;
    function getTexCoord(index: integer): TAffineVector;
    function getVertex(index: integer): TAffineVector;
    procedure setIndices(index: integer; const Value: integer);
    procedure setNormal(index: integer; const Value: TAffineVector);
    procedure setTexCoord(index: integer; const Value: TAffineVector);
    procedure setVertex(index: integer; const Value: TAffineVector);
  public
    constructor Create(Owner: TMultiObjectMesh);

    property Vertices[index: integer]: TAffineVector read getVertex write setVertex;
    property Normals[index: integer]: TAffineVector read getNormal write setNormal;
    property TexCoords[index: integer]: TAffineVector read getTexCoord write setTexCoord;
    property Indices[index: integer]: integer read getIndices write setIndices;
    property MaterialName: string read FMaterialName write FMaterialName;
    property GroupName: string read FGroupName write FGroupName;
    property Count: integer read FElementsCount;
    property IndiceOffset: integer read FIndicesOffset;
  end;

  TMultiObjectMesh = class
  private
    FVertices: TAffineVectorList;
    FNormals: TAffineVectorList;
    FTexCoords: TAffineVectorList;
    FIndices: TIntegerList;
    FMeshObjects: TList;
    FVBOBuff: PVBOBuffer;

    function getMeshObject(index: integer): TBuiltinMeshObject;
    function getObjCount: integer;
  public
    constructor Create;
    destructor Destroy; override;
    function BuildVBO: PVBOBuffer;

    property VBOBufer: PVBOBuffer read FVBOBuff;
    property Vertices: TAffineVectorList read FVertices;
    property Normals: TAffineVectorList read FNormals;
    property TexCoords: TAffineVectorList read FTexCoords;
    property Indices: TIntegerList read FIndices;
    property MeshObjects[index: integer]: TBuiltinMeshObject read getMeshObject; default;
    property MeshObjectsList: TList read FMeshObjects;
    property ObjectsCount: integer read getObjCount;
  end;

  TVBOIndiceAdapter = class
  private
    FAttached: TVertexAttribs;
    FVertices: TAffineVectorList;
    FNormals: TAffineVectorList;
    FTexCoords: TAffineVectorList;
    FVIndices: TIntegerList;
    FNIndices: TIntegerList;
    FTIndices: TIntegerList;
    FFaceGroups: TList;
    FCache: TIntegerBits;
    procedure IndexingHT(const HashTable: TVertexHashArray; Res: TList;
      Ind: TIntegerList; sIndex: integer=0);
    procedure AddNewMeshObject(var MOMesh: TMultiObjectMesh; VTNHashList: TList;
      Ind: TIntegerList; MName:string=''; GName: string='');
    procedure QuickSort(var A: TVertexHashArray);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AttachVertices(Vert: TAffineVectorList; Ind: TIntegerList);
    procedure AttachTexCoords(TexC: TAffineVectorList; Ind: TIntegerList);
    procedure AttachNormals(Norm: TAffineVectorList; Ind: TIntegerList);
    procedure AttachFaceGroups(FG: TList);
    procedure ExtractTriangles(var V: TAffineVectorList; var FaceGr: TList;
      var T, N:TAffineVectorList; MergeByMaterials: boolean=true);
    procedure BuildMultiObjectMesh(var MOMesh: TMultiObjectMesh);
    procedure BuildVBOMeshList(var MeshList: TList);
  end;

  TIntegerAttribList = class
  private
    FList: array of integer;
    FLastIndex: integer;
    FCount: integer;
    FGrowCount: integer;
    FCapacity: integer;
    FMin, FMax: integer;
    procedure GrowList;
    function GetData: pointer;
    function GetSize: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(a,b,c,d: byte); overload;
    procedure Add(index: integer); overload;
    procedure Add(index1,index2: integer); overload;
    procedure Add(index1,index2, index3: integer); overload;
    procedure Add(index1,index2, index3, index4: integer); overload;
    procedure Clear;

    property Data: pointer read GetData;
    property Size: integer read GetSize;
    property Count: integer read FCount;
    property Min: integer read FMin;
    property Max: integer read FMax;
  end;

  TVectorAttribList = class
  private
    FVectorType: TVectorType;
    FList: TSingleArray;
    FLastIndex: integer;
    FCount: integer;
    FGrowCount: integer;
    FCapacity: integer;
    FMin, FMax: TVector3f;
    procedure GrowList;
    function GetData: pointer;
    function GetSize: integer;
    function getExtents: TExtents;
  public
    constructor Create(VectorType: TVectorType = vtVector);
    destructor Destroy; override;

    procedure Add(x: single); overload;
    procedure Add(x,y: single); overload;
    procedure Add(x,y,z: single); overload;
    procedure Add(x,y,z,w: single); overload;

    procedure Add(v: TVector2f); overload;
    procedure Add(v1,v2: TVector2f); overload;
    procedure Add(v1,v2,v3: TVector2f); overload;
    procedure Add(v1,v2,v3,v4: TVector2f); overload;

    procedure Add(v: TVector3f); overload;
    procedure Add(v1,v2: TVector3f); overload;
    procedure Add(v1,v2,v3: TVector3f); overload;
    procedure Add(v1,v2,v3,v4: TVector3f); overload;

    procedure Add(v: TVector4f); overload;
    procedure Add(v1,v2: TVector4f); overload;
    procedure Add(v1,v2,v3: TVector4f); overload;
    procedure Add(v1,v2,v3,v4: TVector4f); overload;

    procedure Add(m: TMatrix3f); overload;
    procedure Add(m: TMatrix4f); overload;

    function GetSingle(Index: integer): single;
    function GetVector2f(Index: integer): TVector2f;
    function GetVector3f(Index: integer): TVector3f;
    function GetVector4f(Index: integer): TVector4f;

    procedure Clear;

    property Data: pointer read GetData;
    property Size: integer read GetSize;
    property Count: integer read FCount;
    property Extents: TExtents read getExtents;
  end;

  TVBOAttribute = record
    Id: cardinal;
    Data: pointer;
    Size: cardinal;
    CCount: integer;
    CSize: integer;
    CType: cardinal;
    Name: string;
    DataHandler: TObject;
    AttrType: TAttribType;
    Location: integer;
    tag: string;
  end;
  PVBOAttribute = ^TVBOAttribute;

  TVBOBuffer = packed record
    Visible: boolean;
    Name: string;
    Struct: TBufferStruct;
    RenderBuffs: TRenderBuff;
    Vertexes: TAffineVectorList;
    Normals: TAffineVectorList;
    TexCoords: TAffineVectorList;
    Colors: TVectorList;
    Indices: TintegerList;
    UseTwoTexturesCoord: boolean;
    ExTexCoords: TList;
    AttribList: TList;
    ExTexEnvMode: TIntegerList;
    SubMeshes: TList;

    VertexCount: integer;
    MaxElements: integer;
    ElementsCount: integer;
    FaceType: GLUInt; {GL_TRIANGLES, GL_TRIANGLE_STRIP, GL_QUADS...}
    RenderType: TVBORenderType;
    vId, nId, tId, iId, stId, cId: GLUint;
    VAO: GLUInt;
    vccount, nccount: integer; //coponents count
    emin, emax: TAffineVector;
    LocalMatrix: TMatrix;
    WorldMatrix: TMatrix;
    Builded: boolean;
    idxBuffApply: boolean;
    idxBindOnce: boolean;
    ChildBuff: boolean;
    MatName: string[80];
    FaceGroupeName: string[80];
    TextureHandle: GLUInt;
    ShaderProg: GLUInt;
    Cleared: boolean;
    PrevLOD, NextLOD: PVBOBuffer;
    LODLevel:integer;
    AdditionalInfo:pointer;
    solid: boolean;
    ccount: integer;
    MaterialFunc: TMaterialSetter;
  end;

  TVertexArrayObject = record
    vao: GLUInt;
    SourceVBO: PVBOBuffer;
  end;

  TObjIndices = packed record
    MatName: string;
    Indices: TintegerList;
    IndicesId: GLUint;
  end; PObjIndices = ^TObjIndices;

  TPackedVBO = packed record
    Buff: TVBOBuffer;
    IndicesLists: TList;
    UseExIndices: boolean;
    OptimizeToPostTnLVCache: integer;
    Builded: boolean;
  end;

  TPointSpriteData = packed record
     v,c: TVector;
     mass:single;
     vel,acc:TAffineVector;
  end;

  TInterleaved = packed record
    v: TAffineVector;
    n: TAffineVector;
    s, t: single;
  end;
  TInterleavedBuff = record
    Data: array of TInterleaved;
    BuffId: GLUInt;
  end;

  PMultiBuffInfo = ^TMultiBuffInfo;
  TMultiBuffInfo = record
     IndicePtr: pointer;
     BatchIdx: integer;
     BuffIdx: integer;
  end;
  TMultiBuffer = array of record
      Buff: TVBOBuffer;
      ExtentsList: TList;
      IndiceList: TList;
      CountList: TIntegerList;
      ElementsCount: integer;
      RenderCount: array of integer;
      RenderIndice: array of pointer;
      CurrentIdx: integer;
  end;
  PMultiRenderDescr = ^TMultiRenderDescr;
  TMultiRenderDescr = record
     MaterialName: string;
     FaceType: integer;
     ElementsCount: integer;
     RenderBuff: PVBOBuffer;
     RenderCount: array of integer;
     RenderIndice: array of pointer;
  end;
  PMultiPackBuff=^TMultiPackBuff;
  TMultiPackBuff = array of record
     Buff: TVBOBuffer;
     CountList: TIntegerList;
     IndiceList: TList;
     ExtentsList: TList;
     FaceTypes: TIntegerList;
     MaterialList: TStringList;
  end;
  TGetHeightFunc = Function (X,Y: Integer):single;

  TInterleavedBuffer = class
  private
    FDescriptors: array of TInterleavedAttrib;
    FList: TSingleList;
    FCount: integer;
    FBaseIndex: integer;
    FStructureLocked: boolean;
    FRecordSize: integer;
    FOffset: integer;
    function GetAttribPos(DescrIndex: byte): integer;
    function getBuffData: pointer;
    function getDescr(Index: byte): TInterleavedAttrib;
    procedure SetDescr(Index: byte; const Value: TInterleavedAttrib);
  public
    constructor Create;
    destructor Destroy; override;

    procedure BuildVBO(var VBO: TVBOBuffer);
    procedure BindToVBO(var VBO: TVBOBuffer);
    procedure UnBindVBO(var VBO: TVBOBuffer);

    function CreateDescriptor(aSize: cardinal; aType: TAttribType; aName: string=''): integer;
    function AddVertex: integer;
    procedure AttributeValue(DescrIndex: byte; x,y,z: single); overload;
    procedure AttributeValue(DescrIndex: byte; s,t: single); overload;
    procedure AttributeValue(DescrIndex: byte; r,g,b,a: single); overload;
    procedure AttributeValue(DescrIndex: byte; v: TAffineVector); overload;
    procedure AttributeValue(DescrIndex: byte; st: TTexPoint); overload;
    procedure AttributeValue(DescrIndex: byte; v: TVector); overload;

    function AddFloatValue(Value: single): integer;
    procedure LockBuffer;
    procedure Clear;

    property Count: integer read FCount;
    property RecordSize: integer read FRecordSize;
    property BufferData: pointer read getBuffData;
    property Descriptors[Index: byte]: TInterleavedAttrib read getDescr write SetDescr;
  end;

procedure InitVBOBuff(var VBuff: TVBOBuffer; FType: GLUInt; RType: TVBORenderType);
procedure GenVBOBuff(var VBuff: TVBOBuffer; FreeBuffMem: boolean = true);
procedure GenVBOBuffers(List: TList; FreeBuffMem: boolean = true);
procedure GenVBOSolidBuff(var VBuff: TVBOBuffer; FreeBuffMem: boolean = true);
procedure GenVBOPackBuff(var VBuff: TPackedVBO; FreeBuffMem: boolean = true);
Procedure RebuildVBOBuff(var VBuff: TVBOBuffer; FreeBuffMem: boolean = true);
Procedure UpdateVBOBuff(BuffId:integer; data: pointer; offset, size:integer; MapBuffer:Boolean=false);
Procedure BindVBO(var Buff: TVBOBuffer);
Procedure UnBindVBO(var Buff: TVBOBuffer);

Procedure RebuildNormals(var vbo: PVBOBuffer);
Function GenVAO(VBuff: PVBOBuffer): TVertexArrayObject;overload;
Function GenVAO(var VBuff: TVBOBuffer): GLUInt; overload;

function  GenIndices(var Indices: TIntegerList): GLUInt;
function  GenSingleVBOBuffer(var List: TAffineVectorList; target: GLUInt = GL_STATIC_DRAW): GLUInt; overload;
function  GenSingleVBOBuffer(var List: TVectorList; target: GLUInt = GL_STATIC_DRAW): GLUInt; overload;
function  GenSingleVBOBuffer(var buff: PVBOAttribute; target: GLUInt = GL_STATIC_DRAW): GLUInt; overload;
function  GetAttribByName(var buff: TVBOBuffer; const Name: string): PVBOAttribute;
procedure AttributeFromList(list: TAffineVectorList; var attr: TVBOAttribute);overload;
procedure AttributeFromList(list: TVectorList; var attr: TVBOAttribute);overload;
procedure AttributeFromList(list: TIntegerList; var attr: TVBOAttribute);overload;
procedure AttributeFromList(list: TTexPointList; var attr: TVBOAttribute);overload;

procedure SaveVBOBuff(var Buff:TVBOBuffer;FileName:string);overload;
procedure SaveVBOBuff(Const Buff:TVBOBuffer;FS:TStream);overload;
procedure LoadVBOBuff(var Buff:TVBOBuffer;FileName:string);overload;
procedure LoadVBOBuff(var Buff:TVBOBuffer;FS:TStream);overload;
procedure RenderVBOBuffer(var VBuff: TVBOBuffer);overload;
procedure RenderVAO(const VAO: TVertexArrayObject);
procedure RenderVBOBuffer(var VBuff: TVBOBuffer; MatList: TList);overload;
procedure RenderVBOMultiBuffer(var Buffs: TMultiBuffer; SingleCall:boolean=true);
procedure RenderVBOMultiList(RenderList: TList; SingleCall:boolean=true);
procedure RenderVBOMultiPart(Description: PMultiRenderDescr; BindBuffer:TBindState; SingleCall:boolean=true);
procedure RenderVBOList(List: TList);overload;
procedure RenderVBOList(List: TList; MatList: TList);overload;

function  RenderVBOListWithCulling(List: TList; var WorldMatrix: TMatrix; bounds: boolean = false): integer;
procedure RenderVBOPackBuff(var VBuff: TPackedVBO);

procedure FreeVBOBuffer(var VBuff: TVBOBuffer; ClearBuffs: boolean = true);
procedure FreeVBOMem(var VBuff: TVBOBuffer);
procedure FreeVBOList(var List:TList;ClearBuffs: boolean = true);

procedure AttachBuffer(var FromBuff, ToBuff: TVBOBuffer; AttachIndice: boolean = true);
procedure AttachBufferInPos(var FromBuff, ToBuff: TVBOBuffer; vPos,iPos:integer);
procedure PackListIntoBuffer(List:TList; var Buffs:TMultiBuffer; FreeRAM:boolean);overload;
procedure ListToMultiBuffer(var List:TList; var Buffs:TMultiPackBuff;
                            RenderList:TList; FreeRAM:boolean=false; size:integer=-1);
function  CreatePlane(Width, Height: single; TilesX, TilesY: integer; HFunc: TGetHeightFunc=nil;
                      AsLine:boolean=false;GenBuff:Boolean=true): PVBOBuffer;
function CreateCubicOccluder(GenVBO: boolean=true): PVBOBuffer;

procedure SortListByMaterial(List:TList);

procedure CreateBBox(var buff: TVBOBuffer);

procedure IndexingBuff(var buff: TVBOBuffer);
procedure RestripifyFaces(var buff: TVBOBuffer; Indexing: boolean = true);

procedure Tesselate(var buff: TVBOBuffer; TessType: integer = 1; Smooth: boolean = false);

function GetMaxIndicesCount: GLUInt;
function GetMaxVertexCount: GLUInt;
function GetFrustum: TFrustum;overload;
function GetFrustum(const projMat, mvMat: TMatrix): TFrustum;overload;
procedure OptimizeIndices(CacheSize: integer; BuffSize: integer; var Buff: TVBOBuffer; var BuffList: TList; GenBuff: boolean = true);
procedure ExtractTriangles(var Buff: TVBOBuffer; var Triangles: TAffineVectorList);
//Procedure CreateInterleavedArray(var VBuff:TVBOBuffer;var IBuff:TInterleavedBuff;FreeBuffMem:boolean=true);

procedure RenderBounds(emin, emax: TAffinevector; w, r, g, b: single);
function isVolumeClipped(const emin, emax: TAffineVector): boolean; overload;
function isVolumeClipped(const Extents: TExtents; const Frustum:TFrustum): boolean;overload;

procedure Col2RowMatrix(mm: TMatrix; var m: TMatrix);
function CreateViewMatrix(const ModelMatrix: TMatrix): TMatrix;
Function GetViewPort:TVector4i;

function GetViewMatrix:TMatrix;
function GetProjectionMatrix:TMatrix;
function ProjectPoint(aPoint: TVector; const modelMatrix: TMatrix;
	      const projMatrix:TMatrix; const viewport: array of integer;
        var ScreenPos:TVector): boolean;
function GetModelViewMatrix(WorldMatrix, ViewMatrix: TMatrix): PGLFloat;
function GetMinExtents(v1, v2: TAffineVector): TAffinevector;
function GetMaxExtents(v1, v2: TAffineVector): TAffinevector;
function GetSubMeshExtents(subMesh: PSubMesh; const VBO: TVBOBuffer): TExtents;
procedure ExtentsToTranslateAndScale(var Extents: TExtents; var pos, sc: TVector);
function GetExtentsOfList(List:TList):TExtents;

function LookAt(const eyePosition3D, center3D, upVector3D: TAffineVector): TMatrix;
function QuaternionRotate(const Q: TQuaternion; const P: TAffineVector): TAffineVector;overload
function QuaternionRotate(const Q: TVector; const P: TAffineVector): TAffineVector;overload;
function QuatToVector(const Q: TQuaternion): TVector;
function VectorToQuat(const Q: TVector): TQuaternion;

function HashKey(const v : TAffineVector; hashSize : Integer) : Integer;

function RandomVector(Scale: single=1): TAffineVector; overload;
//function RandomVector(Scale: single=1): TVector; overload;




var
  vCubicOccluder: PVBOBuffer = nil;

implementation

const CVectorSize: array [vtSingle..vtMat4] of byte = (1,2,3,4,9,16);
      CDefAttribName: array[atVertex..atColor] of string =
                     ('Vertex','Normal','TexCoord','Color');

function GetMinExtents(v1, v2: TAffineVector): TAffinevector;
begin
  Result := V1;
  if v1[0] > v2[0] then result[0] := v2[0];
  if v1[1] > v2[1] then result[1] := v2[1];
  if v1[2] > v2[2] then result[2] := v2[2];
end;

function GetMaxExtents(v1, v2: TAffineVector): TAffinevector;
begin
  Result := V1;
  if v1[0] < v2[0] then result[0] := v2[0];
  if v1[1] < v2[1] then result[1] := v2[1];
  if v1[2] < v2[2] then result[2] := v2[2];
end;

function GetExtentsOfList(List:TList):TExtents;
var i:integer;
    P:PVBOBuffer;
begin
  if (not assigned(list)) or (List.Count=0) then exit;
  with Result do begin
    if not assigned(List[0]) then exit;
    P:=List[0];emin:=P.emin; emax:=p.emax;
    for i:=1 to List.Count-1 do begin
       P:=List[i];
       emin:=GetMinExtents(emin,P.emin);
       emax:=GetMaxExtents(emax,P.emax);
    end;
  end;
end;

function GetSubMeshExtents(subMesh: PSubMesh; const VBO: TVBOBuffer): TExtents;
var i,n: integer;
    mx,my,mz: single;
    nx,ny,nz: single;
    v: TAffineVector;
begin
  if subMesh.PrimitiveCount=0 then begin
    Result.emin:=NullVector; result.emax:=NullVector; exit;
  end;
  n:=VBO.Indices.List[SubMesh.StartingIndex];
  v:=VBO.Vertexes[n];
  mx:=v[0]; my:=v[1]; mz:=v[2]; nx:=mx; ny:=my; nz:=mz;
  for i:=1 to SubMesh.PrimitiveCount*3-1 do begin
    n:=VBO.Indices.List[i+SubMesh.StartingIndex];
    v:=VBO.Vertexes[n];
    if v[0]>mx then mx:=v[0]; if v[0]<nx then nx:=v[0];
    if v[1]>my then my:=v[1]; if v[1]<ny then ny:=v[1];
    if v[2]>mz then mz:=v[2]; if v[2]<nz then nz:=v[2];
  end;
  result.emin:=AffineVectorMake(nx,ny,nz);
  result.emax:=AffineVectorMake(mx,my,mz);
end;

function GetMaxIndicesCount: GLUInt;
begin
  glGetintegerv(GL_MAX_ELEMENTS_INDICES, @result);
end;
function GetMaxVertexCount: GLUInt;
begin
  glGetintegerv(GL_MAX_ELEMENTS_VERTICES, @result);
end;

function GetFrustum: TFrustum;overload;
var projMat, mvMat, MVProj: TMatrix;
begin
  glGetFloatv(GL_PROJECTION_MATRIX, @projMat);
  glGetFloatv(GL_MODELVIEW_MATRIX, @mvMat);
  MVProj := MatrixMultiply(mvMat, projMat);
  Result := ExtractFrustumFromModelViewProjection(MVProj);
end;

function GetFrustum(const projMat, mvMat: TMatrix): TFrustum;overload;
var MVProj: TMatrix;
begin
  MVProj := MatrixMultiply(mvMat, projMat);
  Result := ExtractFrustumFromModelViewProjection(MVProj);
end;

function isVolumeClipped(const emin, emax: TAffineVector): boolean;overload;
var F: TFrustum;
  objPos: TAffineVector;
  Radius: single;
begin
  F := GetFrustum;
  objPos := VectorScale(VectorAdd(emin, emax), 0.5);
  Radius := VectorDistance(emin, emax) * 0.5;
  Result := isVolumeClipped(objPos, Radius, F);
end;

function isVolumeClipped(const Extents: TExtents; const Frustum: TFrustum): boolean;overload;
var objPos: TAffineVector;
    Radius: single;
begin
  with Extents do begin
    objPos := VectorScale(VectorAdd(emin, emax), 0.5);
    Radius := VectorDistance(emin, emax) * 0.5;
    Result := isVolumeClipped(objPos, Radius, Frustum);
  end;
end;


procedure FreeAndNil(var Obj);
var
  Temp: TObject;
begin
  Temp := TObject(Obj);
  Pointer(Obj) := nil;
  Temp.Free;
end;


Procedure SaveVBOBuff(var Buff:TVBOBuffer;FileName:string);
var fs: TFileStream;
begin
  fs := TFileStream.Create(FileName,fmCreate);
  SaveVBOBuff(Buff,fs);
  fs.free;
end;

Procedure SaveVBOBuff(Const Buff:TVBOBuffer;FS:TStream);overload;
var count:integer;
    signature:string[14];
begin
  signature := 'VBO Buffer BEG';
  fs.Write(signature, sizeof(signature));
  fs.WriteBuffer(Buff,sizeof(TVBOBuffer));
  with Buff do begin
    count:=Vertexes.count;fs.Write(Count,4);
    Vertexes.SaveToStream(fs);
    count:=Normals.count;fs.Write(Count,4);
    Normals.SaveToStream(fs);
    count:=TexCoords.count;fs.Write(Count,4);
    TexCoords.SaveToStream(fs);
    count:=Indices.count;fs.Write(Count,4);
    Indices.SaveToStream(fs);
  end;
  signature := 'VBO Buffer END';
  fs.Write(signature, sizeof(signature));
end;

Procedure LoadVBOBuff(var Buff:TVBOBuffer;FileName:string);
var fs: TFileStream;
begin
  fs := TFileStream.Create(FileName,0);
  LoadVBOBuff(Buff,fs);
  fs.free;
end;

Procedure LoadVBOBuff(var Buff:TVBOBuffer;FS:TStream);overload;
var count:integer;
    signature:string[14];
begin
  fs.Read(signature, sizeof(signature));
  Assert (signature<>'VBO Buffer BEG','This is not VBO Buffers!');
  fs.ReadBuffer(Buff,sizeof(TVBOBuffer));
  with Buff do begin
    fs.Read(Count,4); FreeAndNil(Vertexes);
    Vertexes:=TAffineVectorList.Create;
    Vertexes.Count:=count;
    Vertexes.LoadFromStream(fs);
    fs.Read(Count,4); FreeAndNil(Normals);
    Normals:=TAffineVectorList.Create;
    Normals.Count:=count;
    Normals.LoadFromStream(fs);
    fs.Read(Count,4); FreeAndNil(TexCoords);
    TexCoords:=TAffineVectorList.Create;
    TexCoords.Count:=count;
    TexCoords.LoadFromStream(fs);
    fs.Read(Count,4); FreeAndNil(Indices);
    Indices:=TIntegerList.Create;
    Indices.Count:=count;
    Indices.LoadFromStream(fs);
  end;
  fs.Read(signature, sizeof(signature));
  Assert (signature<>'VBO Buffer END','VBO Buffers "END" not found!');
end;

procedure RebuildNormals(var vbo: PVBOBuffer);
var i,i1,i2,i3: integer;
    nm,v1,v2,v3: TAffineVector;
begin
  for i:=0 to vbo.Normals.Count-1 do vbo.Normals[i]:=NullVector;
  for i:=0 to (vbo.Indices.Count div 3)-1 do begin
    i1:=vbo.Indices[i*3]; i2:=vbo.Indices[i*3+1]; i3:=vbo.Indices[i*3+2];
    v1:=vbo.Vertexes[i1]; v2:=vbo.Vertexes[i2]; v3:=vbo.Vertexes[i3];
    nm:=CalcPlaneNormal(v1,v2,v3);
    vbo.Normals.TranslateItem(i1,nm);
    vbo.Normals.TranslateItem(i2,nm);
    vbo.Normals.TranslateItem(i3,nm);
  end; vbo.Normals.Normalize;
end;

procedure InitVBOBuff;
begin
  with VBuff do begin
    Vertexes := TAffineVectorList.Create;
    Normals := TAffineVectorList.Create;
    TexCoords := TAffineVectorList.Create;
    Colors := TVectorList.Create;
    Indices := TintegerList.Create;
    SubMeshes := TList.Create;
    AttribList:=TList.Create;
    FaceType := FType;
    RenderType := RType;
    Builded := false;
    RenderBuffs := [];
    vId := 0; nId := 0; tId := 0; iId := 0; stId := 0;
    vccount:=0; nccount:=0; vao:=0;
    WorldMatrix := IdentityHmgMatrix;
    VertexCount := 0;
    idxBuffApply := false;
    idxBindOnce := false;
    ChildBuff := false;
    UseTwoTexturesCoord := True;
    ExTexCoords := TList.Create;
    ExTexEnvMode := TIntegerList.Create;
    solid:=false;
    Visible := true;
    MaterialFunc:=nil;
    Struct:=bsSeparated;
  end;
end;

function  GetAttribByName(var buff: TVBOBuffer; const Name: string): PVBOAttribute;
var i: integer;
    attr: PVBOAttribute;
begin
  result:=nil;
  for i:=0 to buff.AttribList.Count-1 do begin
    attr:=buff.AttribList[i];
    if attr.Name=Name then begin result:=attr; exit; end;
  end;
end;

procedure AttributeFromList(list: TAffineVectorList; var attr: TVBOAttribute);overload;
begin
  attr.Data:=list.List;
  attr.Size:=list.DataSize;
  attr.CCount:=3;
  attr.CSize:=Sizeof(TAffineVector);
  attr.CType:=GL_FLOAT;
  attr.DataHandler:=list;
end;

procedure AttributeFromList(list: TVectorList; var attr: TVBOAttribute);overload;
begin
  attr.Data:=list.List;
  attr.Size:=list.DataSize;
  attr.CCount:=4;
  attr.CSize:=Sizeof(TVector);
  attr.CType:=GL_FLOAT;
  attr.DataHandler:=list;
end;

procedure AttributeFromList(list: TIntegerList; var attr: TVBOAttribute);overload;
begin
  attr.Data:=list.List;
  attr.Size:=list.DataSize;
  attr.CCount:=1;
  attr.CSize:=Sizeof(integer);
  attr.CType:=GL_INT;
  attr.DataHandler:=list;
end;

procedure AttributeFromList(list: TTexPointList; var attr: TVBOAttribute);overload;
begin
  attr.Data:=list.List;
  attr.Size:=list.DataSize;
  attr.CCount:=2;
  attr.CSize:=Sizeof(single)*2;
  attr.CType:=GL_FLOAT;
  attr.DataHandler:=list;
end;


function GenIndices(var Indices: TIntegerList): GLUInt;
begin
  if Indices.Count > 0 then begin
    glGenBuffers(1, @Result);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, Result);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(GLUint) * Indices.Count, Indices.list, GL_STATIC_DRAW);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
  end else Result := 0;
end;

function GenUShortIndices(var Indices: TIntegerList): GLUInt;
var i:integer;
    u_short_Indices:TIntegerList;
    count:integer;
begin
  if Indices.Count > 0 then begin
    u_short_Indices := TIntegerList.Create;
    count:=Indices.Count div 2;
    count:=count+Indices.Count mod 2;
    u_short_Indices.Count:=count;
    for i:=0 to count-1 do begin
       if i*2+1<Indices.Count then
          u_short_Indices[i]:=(Indices[i*2] and $FFFF)+(Indices[i*2+1] and $FFFF) shl 16
       else u_short_Indices[i]:=(Indices[i*2] and $FFFF);
    end;
    glGenBuffers(1, @Result);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, Result);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, 2 * Indices.Count, u_short_Indices.list, GL_STATIC_DRAW);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
    u_short_Indices.Free;
  end else Result := 0;
end;


function GenSingleVBOBuffer(var List: TAffineVectorList; target: GLUInt = GL_STATIC_DRAW): GLUInt;
begin
  if List.Count > 0 then begin
    glGenBuffers(1, @Result);
    glBindBuffer(GL_ARRAY_BUFFER, Result);
    glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * 3 * List.Count, List.list, target);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
  end else Result := 0;
end;

function GenSingleVBOBuffer(var List: TVectorList; target: GLUInt = GL_STATIC_DRAW): GLUInt;
begin
  if List.Count > 0 then begin
    glGenBuffers(1, @Result);
    glBindBuffer(GL_ARRAY_BUFFER, Result);
    glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * 4 * List.Count, List.list, target);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
  end else Result := 0;
end;

function GenSingleVBOBuffer(var Buff: PVBOAttribute; target: GLUInt = GL_STATIC_DRAW): GLUInt;
begin
  with Buff^ do begin
    glGenBuffers(1, @Id); result:=Id;
    glBindBuffer(GL_ARRAY_BUFFER, Id);
    glBufferData(GL_ARRAY_BUFFER, Size, Data, target);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
  end;
end;



procedure GenVBOBuff(var VBuff: TVBOBuffer; FreeBuffMem: boolean = true);
var i: integer;
    attr: PVBOAttribute;
begin
  with VBuff do begin
    Vertexes.GetExtents(emin, emax);
    vId := GenSingleVBOBuffer(Vertexes);
    nId := GenSingleVBOBuffer(Normals);
    tId := GenSingleVBOBuffer(TexCoords);
    cId := GenSingleVBOBuffer(Colors);
    iId := GenIndices(Indices);
    if ExTexCoords.Count>0 then begin
      for i := 0 to ExTexCoords.Count - 1 do begin
        attr:=ExTexCoords[i]; GenSingleVBOBuffer(attr);
        if i=0 then stId:=attr.Id;
      end;
    end;
    for i:=0 to AttribList.Count-1 do begin
      attr:=AttribList[i]; attr.Id:=GenSingleVBOBuffer(attr);
    end;

    RenderBuffs:=[];
    if nId > 0 then RenderBuffs := RenderBuffs + [uNormals];
    if tId > 0 then RenderBuffs := RenderBuffs + [uTexCoords];
    if iId > 0 then RenderBuffs := RenderBuffs + [uIndices];
    if cId > 0 then RenderBuffs := RenderBuffs + [uColors];
    if ExTexCoords.Count>0 then RenderBuffs := RenderBuffs + [uMultitexture];
    //assert(iId>0,'Indices not found');
    if iId > 0 then ElementsCount := Indices.Count
    else ElementsCount := Vertexes.Count;
    if iId=0 then RenderType:=DrawArrays;

    MaxElements := ElementsCount;
    VertexCount := Vertexes.Count;
    if FreeBuffMem then begin
      Vertexes.Clear; Normals.Clear; TexCoords.Clear; Colors.Clear;
      for i := 0 to ExTexCoords.Count - 1 do begin
        attr:=ExTexCoords[i];
        if assigned(attr.DataHandler) and (pointer(attr.DataHandler)<>attr)
        then attr.DataHandler.Free else dispose(attr.Data);
      end;
      for i := 0 to AttribList.Count - 1 do begin
        attr:=AttribList[i];
        if attr.AttrType=atInterleaved then
          TInterleavedBuffer(attr.DataHandler).Clear
        else begin
          if assigned(attr.DataHandler) and (pointer(attr.DataHandler)<>attr)
          then begin
            attr.DataHandler.Free; attr.DataHandler:=nil;
          end else dispose(attr.Data);
        end;
      end;
      Cleared := true
    end else Cleared := false;
    Builded := true;
  end;
  //if GL_ARB_vertex_array_object then GenVAO(VBuff);
  if (GL_ARB_vertex_array_object) and (VBuff.AttribList.Count=0) then begin
    GenVAO(VBuff); include(VBuff.RenderBuffs,uVAO);
  end;
end;

procedure GenVBOBuffers(List: TList; FreeBuffMem: boolean = true);
var i,j: integer;
    attr: PVBOAttribute;
    VBuff: PVBOBuffer;
begin
  for j:=0 to List.Count-1 do begin
    VBuff:=List[j]; GenVBOBuff(VBuff^,FreeBuffMem);
  end;
end;

procedure GenVBOSolidBuff(var VBuff: TVBOBuffer; FreeBuffMem: boolean = true);
var i: integer;
    temp: TAffineVectorList;
    buffCount, buffSize, offs: integer;
begin
  buffCount:=0; buffSize:=0; offs:=0;
  with VBuff do begin
    Vertexes.GetExtents(emin, emax);
    if Vertexes.Count>0 then begin
       inc(buffCount); buffSize:=buffSize+Vertexes.DataSize;
    end;
    if Normals.Count>0 then begin
       inc(buffCount); buffSize:=buffSize+Normals.DataSize;
    end;
    if TexCoords.Count>0 then begin
       inc(buffCount); buffSize:=buffSize+TexCoords.DataSize;
    end;
    if Colors.Count>0 then begin
       inc(buffCount); buffSize:=buffSize+Colors.DataSize;
    end;

    iId := GenIndices(Indices);

    if Vertexes.Count > 0 then begin
       glGenBuffers(1, @vId);
       glBindBuffer(GL_ARRAY_BUFFER, vId);
       glBufferData(GL_ARRAY_BUFFER, buffSize, nil, GL_STATIC_DRAW);
       glBufferSubData(GL_ARRAY_BUFFER,0,Vertexes.DataSize,Vertexes.List);
       offs:=offs+Vertexes.DataSize;
    end;
    if Normals.Count>0 then begin
       glBufferSubData(GL_ARRAY_BUFFER,offs,Normals.DataSize,Normals.List);
       nId:=offs; offs:=offs+Vertexes.DataSize;
    end;
    if TexCoords.Count>0 then begin
       glBufferSubData(GL_ARRAY_BUFFER,offs,TexCoords.DataSize,TexCoords.List);
       tId:=offs; offs:=offs+TexCoords.DataSize;
    end;
    if Colors.Count>0 then begin
       glBufferSubData(GL_ARRAY_BUFFER,offs,Colors.DataSize,Colors.List);
       cId:=offs; offs:=offs+Colors.DataSize;
    end;
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    
    RenderBuffs:=[];
    if nId > 0 then RenderBuffs := RenderBuffs + [uNormals];
    if tId > 0 then RenderBuffs := RenderBuffs + [uTexCoords];
    if cId > 0 then RenderBuffs := RenderBuffs + [uColors];
    if iId > 0 then RenderBuffs := RenderBuffs + [uIndices];
    if iId > 0 then ElementsCount := Indices.Count
    else ElementsCount := Vertexes.Count;
    if iId=0 then RenderType:=DrawArrays;

    MaxElements := ElementsCount;
    VertexCount := Vertexes.Count;
    if FreeBuffMem then begin
      Vertexes.Clear; Normals.Clear;
      TexCoords.Clear;Colors.Clear;
      Cleared := true
    end else Cleared := false;
    Builded := true; solid:=true;
  end;
end;

Procedure RebuildVBOBuff(var VBuff: TVBOBuffer; FreeBuffMem: boolean = true);
Begin
  if VBuff.Cleared then exit;
  FreeVBOBuffer(VBuff,false);
  GenVBOBuff(VBuff,FreeBuffMem);
end;

Procedure UpdateVBOBuff(BuffId:integer; data: pointer; offset, size:integer; MapBuffer:Boolean=false);
Var p:pointer;
    i:integer;
begin
glBindBuffer(GL_ARRAY_BUFFER, BuffId);
if MapBuffer then begin
  p := glMapBuffer(GL_ARRAY_BUFFER, GL_WRITE_ONLY);
  for i:=0 to Size-1 do
      PByteArray(p)[offset+i]:=PByteArray(data)[i];
  glUnMapBuffer(GL_ARRAY_BUFFER);
end else begin
  glBufferSubData(GL_ARRAY_BUFFER, offset, Size, data);
end;
glBindBuffer(GL_ARRAY_BUFFER,0);
end;

procedure GenVBOPackBuff(var VBuff: TPackedVBO; FreeBuffMem: boolean = true);
var i: integer;
  pidx: PObjIndices;
begin
  with VBuff.Buff do begin
    vId := GenSingleVBOBuffer(Vertexes);
    nId := GenSingleVBOBuffer(Normals);
    tId := GenSingleVBOBuffer(TexCoords);

    if (VBuff.IndicesLists.Count > 0) and (VBuff.UseExIndices) then begin
      for i := 0 to VBuff.IndicesLists.Count - 1 do begin
        pidx := VBuff.IndicesLists[i];
        if VBuff.OptimizeToPostTnLVCache <> 0 then
          IncreaseCoherency(pidx.Indices, VBuff.OptimizeToPostTnLVCache);
        pidx.IndicesId := GenIndices(pidx.Indices);
      end;
    end else begin
      if VBuff.OptimizeToPostTnLVCache <> 0 then
        IncreaseCoherency(Indices, VBuff.OptimizeToPostTnLVCache);
      iId := GenIndices(Indices);
    end;

    if nId > 0 then RenderBuffs := RenderBuffs + [uNormals];
    if tId > 0 then RenderBuffs := RenderBuffs + [uTexCoords];
    if iId > 0 then RenderBuffs := RenderBuffs + [uIndices];
    if iId > 0 then ElementsCount := Indices.Count
    else ElementsCount := Vertexes.Count;
    MaxElements := ElementsCount;
    VertexCount := Vertexes.Count;
    if FreeBuffMem then begin
      Vertexes.Clear; Normals.Clear; TexCoords.Clear;
      Cleared := True
    end else Cleared := false;
    Builded := true;
  end;
end;

procedure CreateInterleavedArray(var VBuff: TVBOBuffer; var IBuff: TInterleavedBuff; FreeBuffMem: boolean = true);
var i, count: integer;
  vSize, nSize, tSize: integer;
begin
  with VBuff, IBuff do begin
    count := Vertexes.Count;
    setlength(Data, Count);
    vsize := 12 * Normals.Count;
    nsize := 12 * Normals.Count;
    tsize := 8 * TexCoords.Count;
    for i := 0 to Count - 1 do begin
      Data[i].v := Vertexes[i];
      Data[i].n := Normals[i];
      Data[i].s := TexCoords[i][0];
      Data[i].t := TexCoords[i][1];
    end;
    glGenBuffers(1, @BuffId);
    glBindBuffer(GL_ARRAY_BUFFER, BuffId);
    glBufferData(GL_ARRAY_BUFFER, vsize + nsize + tsize, nil, GL_STATIC_DRAW);
    glBufferSubData(GL_ARRAY_BUFFER, 0, vsize, Vertexes.List);
    if nSize > 0 then
      glBufferSubData(GL_ARRAY_BUFFER, vsize, nsize, Normals.List);
    if tSize > 0 then
      glBufferSubData(GL_ARRAY_BUFFER, vsize + nsize, tsize, TexCoords.List);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
  end;
end;


procedure FreeVBOBuffer(var VBuff: TVBOBuffer; ClearBuffs: boolean = true);
var i:integer;
    attr: PVBOAttribute;
begin
  with VBuff do begin
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    if GL_ARB_vertex_array_object then begin
      glBindVertexArray(0);
      glDeleteVertexArrays(1,@vao);
    end;
    Builded := false;idxBuffApply := false;
    if vId <> 0 then glDeleteBuffers(1, @vId);
    if nId <> 0 then glDeleteBuffers(1, @nId);
    if tId <> 0 then glDeleteBuffers(1, @tId);
    if iId <> 0 then glDeleteBuffers(1, @iId);
    if cId <> 0 then glDeleteBuffers(1, @cId);
    vId := 0; nId := 0; tId := 0;
    iId := 0; cId := 0; vao := 0;

    if ClearBuffs then begin
      FreeAndNil(Vertexes);
      FreeAndNil(Normals);
      FreeAndNil(TexCoords);
      FreeAndNil(Indices);
      FreeAndNil(Colors);
      FreeList(SubMeshes);
      for i:=0 to ExTexCoords.Count - 1 do begin
        attr:=ExTexCoords[i];
        if assigned(attr.DataHandler) and (pointer(attr.DataHandler)<>attr)
        then attr.DataHandler.Free else dispose(attr.Data);
        glDeleteBuffers(1, @attr.Id); dispose(attr);
      end;
      for i:= 0 to AttribList.Count - 1 do begin
        attr:=AttribList[i];
        if assigned(attr.DataHandler) and (pointer(attr.DataHandler)<>attr)
        then attr.DataHandler.Free else dispose(attr.Data);
        glDeleteBuffers(1, @attr.Id); dispose(attr);
      end;
      FreeAndNil(AttribList);
      FreeAndNil(ExTexCoords);
      FreeAndNil(ExTexEnvMode);

      Cleared:=true;
    end;
  end;
end;

procedure FreeVBOMem(var VBuff: TVBOBuffer);
var i:integer;
    attr: PVBOAttribute;
begin
  with VBuff do begin
     if assigned(Vertexes) then begin
        Vertexes.Clear; //Vertexes.Free;
     end;
     if assigned(Normals) then begin
        Normals.Clear; //Normals.Free;
     end;
     if assigned(TexCoords) then begin
        TexCoords.Clear; //TexCoords.Free;
     end;
     if assigned(Indices) then begin
        Indices.Clear; //Indices.Free;
     end;
     if assigned(Colors) then begin
        Colors.Clear; //Colors.Free;
     end;
     if assigned(ExTexCoords) then begin
        for i:= 0 to ExTexCoords.Count - 1 do begin
          attr:=ExTexCoords[i];
          if assigned(attr.DataHandler) and (pointer(attr.DataHandler)<>attr)
          then attr.DataHandler.Free else dispose(attr.Data);
          //dispose(attr);
          //FreeAndNil(temp);
        end;
     end;
     if assigned(AttribList) then begin
      for i:= 0 to AttribList.Count - 1 do begin
        attr:=AttribList[i];
        if assigned(attr.DataHandler) and (pointer(attr.DataHandler)<>attr)
        then attr.DataHandler.Free else dispose(attr.Data);
        //dispose(attr);
      end;
     end;

     //FreeAndNil(ExTexCoords);
     //FreeAndNil(ExTexCoordsId);
     //FreeAndNil(ExTexEnvMode);
     Cleared:=true;
  end;
end;

Procedure PackListIntoBuffer(List:TList; var Buffs:TMultiBuffer; FreeRAM:boolean);overload;
var p,t:PVBOBuffer;
    i,k,ii:integer;
    rb:TRenderBuff;
    ft:GLUint;
    rt:TVBORenderType;
    Error:boolean;
    mi:integer;
    n:cardinal;
    E:PExtents;
    mbi:PMultiBuffInfo;
    vcount,icount,vPos,iPos:integer;
    icList,vcList:TIntegerList;
begin
//Check list compatibility
mi:=GetMaxIndicesCount;
icList:=TIntegerList.Create;
vcList:=TIntegerList.Create;
P:=List[0];vcount:=0;icount:=0;
assert(not P.Cleared,'Buffers is Cleared.');
rb:=P.RenderBuffs;ft:=P.FaceType; rt:=P.RenderType;
Error:=false; i:=0;
repeat
   P:=List[i];
   if icount+P.Indices.Count<mi then begin
     inc(vcount,P.Vertexes.Count);
     inc(icount,P.Indices.Count);
   end else begin
     icList.Add(iCount);
     vcList.Add(vCount);
     vcount:=P.Vertexes.Count;
     icount:=P.Indices.Count;
   end;
   if (P.RenderBuffs<>rb) or (P.FaceType<>ft) then Error:=true;
   i:=i+1;
until Error or (i=List.Count);
icList.Add(iCount); vcList.Add(vCount);

Assert(not Error,'Buffers in List is not identical.');
//Create List with Buff size optimized to MaxIndices/MaxVertexes
setlength(Buffs,icList.count);
i:=0;t:=List[0];k:=0;
repeat
  with Buffs[k] do begin
    InitVBOBuff(Buff,ft,rt);Buff.RenderBuffs:=rb;
    icount:=icList[k];vcount:=vcList[k];
    Buff.Vertexes.Count:=vcount;
    Buff.Indices.Count:=icount;
    if uNormals in rb then Buff.Normals.Count:=vcount;
    if uTexCoords in rb then Buff.TexCoords.Count:=vcount;
    ii:=i;
    ExtentsList:=TList.Create;
    IndiceList:=TList.Create;
    CountList:=TIntegerList.Create;
    ElementsCount:=-1; vPos:=0; iPos:=0;
    While (i<List.count) and (iPos<Buff.Indices.Count) do
    begin
      New(E);E.emin:=t.emin;E.emax:=t.emax;
      ExtentsList.Add(E);
      CountList.Add(t.Indices.Count);
      n:=iPos*4;
      AttachBufferInPos(t^,Buff,vPos,iPos);
      vPos:=vPos+T.Vertexes.Count;
      iPos:=iPos+T.Indices.Count;
//      FreeVBOBuffer(T^);
      new(mbi); mbi.BatchIdx:=k; mbi.BuffIdx:=i-ii;
      mbi.IndicePtr:=pointer(n);
      t.AdditionalInfo:=mbi;
      IndiceList.Add(mbi.IndicePtr);
      i:=i+1;
      if i<List.count then t:=List[i];
    end;
    GenVBOBuff(Buff,FreeRAM);
  end;
  k:=k+1;
until i=List.Count;
end;

procedure ListToMultiBuffer(var List: TList; var Buffs: TMultiPackBuff;
                            RenderList: TList; FreeRAM: boolean; size: integer);
var Lists:TList;
    MeshList:TList;
    i,j,k,n:integer;
    P,T:PVBOBuffer;
    matname:string;
    mi,buffsSize,
    buffCount, buffidx,
    vCount,iCount: integer;
    extents:PExtents;
    rb:TRenderBuff;
    ft:integer;
    RenderDescr:PMultiRenderDescr;
begin
  if (not assigned(List)) or (List.Count=0) or (size<-1) then exit;
  case size of
    -1: mi:=GetMaxIndicesCount;
     0: mi:=high(integer);
     else mi:=size;
  end;
  SortListByMaterial(List);
  Lists:=TList.Create;//Store combined meshes
  //Combine buffs by materials
  i:=0;P:=List[0]; rb:=[];
  while i<List.Count do begin
    MeshList:=TList.Create;
    matname:=p.MatName;
    repeat
      if p.Indices.Count>0 then begin
{        P.Indices.Clear;
        P.Indices:=BuildVectorCountOptimizedIndices(P.Vertexes, p.Normals, p.TexCoords);
        RemapReferences(p.Normals, P.Indices);
        RemapReferences(p.TexCoords, P.Indices);
        RemapAndCleanupReferences(P.Vertexes, P.Indices);}
{        P.Indices.Clear;
        WeldVertices(p.Vertexes,p.Indices,0.00001);}
        MeshList.Add(p);
      end;
      inc(i); if i<List.Count then P:=List[i];
    until (i=List.Count) or (p.MatName<>matname);
    Lists.Add(MeshList);
  end;
  //Sort Mesh Lists by Face Types
  for k:=0 to Lists.Count-1 do begin
      MeshList:=Lists[k]; i:=0;
      while i<MeshList.Count-1 do begin
         P:=MeshList[i]; j:=i+1;
         while j<MeshList.Count do begin
             T:=MeshList[j];
             if P.FaceType=T.FaceType then begin
                if j<>i+1 then begin MeshList.Exchange(i+1,j); inc(i); inc(j);end
                else begin inc(i); j:=i+1; end;
             end else inc(j);
         end;
         inc(i);
      end;
  end;
  //Buff count after splits by MaxIndices, needed to allocate memory for buffs
  buffsSize:=0; buffCount:=0;
  for k:=0 to Lists.Count-1 do begin
      MeshList:=Lists[k];
      for i:=0 to MeshList.Count-1 do begin
         P:=MeshList[i]; rb:=rb+p.RenderBuffs;
         if buffsSize+p.Indices.Count<mi then
            buffsSize:=buffsSize+p.Indices.Count
         else begin
            inc(buffCount); buffsSize:=p.Indices.Count;
         end;
      end;
  end;
  if buffCount=0 then inc(buffCount);
  setlength(buffs,buffCount);
  buffidx:=-1;
  vCount:=0; iCount:=0;
  for k:=0 to Lists.Count-1 do begin
      MeshList:=Lists[k];
      for i:=0 to MeshList.Count-1 do begin
        P:=MeshList[i];
        if p.Indices.Count>0 then begin
          if (buffidx=-1) or (iCount+p.Indices.Count>=mi)
          then begin
             inc(buffidx);
             with Buffs[buffidx] do begin
               InitVBOBuff(Buff,P.FaceType,DrawElements);
               Buff.RenderBuffs:=rb;
               CountList := TIntegerList.Create;
               IndiceList := TList.Create;
               ExtentsList := TList.Create;
               FaceTypes := TIntegerList.Create;
               MaterialList := TStringList.Create;
             end;
             iCount:=0; vCount:=0;
          end;
          AttachBufferInPos(P^,Buffs[buffidx].Buff,vCount,iCount);
          with Buffs[buffidx] do begin
            CountList.Add(p^.Indices.Count);
            IndiceList.Add(pointer(iCount*4));
            new(extents); extents.emin:=p.emin; extents.emax:=p.emax;
            ExtentsList.Add(extents);
            FaceTypes.Add(p.FaceType);
            MaterialList.Add(p.MatName);
          end;
          iCount:=iCount+p.Indices.Count;
          vCount:=vCount+p.Vertexes.Count;
        end;
      end;
  end;
  for k:=0 to buffCount-1 do begin
   ft:=-1;n:=0;matname:='';
   RenderDescr:=nil;
   For i:=0 to Buffs[k].FaceTypes.Count-1 do begin
     if (Buffs[k].FaceTypes[i]<>ft) or (Buffs[k].MaterialList[i]<>matname) then begin
        if RenderDescr<>nil then begin
           setlength(RenderDescr.RenderCount,n);
           setlength(RenderDescr.RenderIndice,n);
           RenderDescr.ElementsCount:=n;
           RenderList.add(RenderDescr);
        end;
        new(RenderDescr); ft:=Buffs[k].FaceTypes[i];
        matname:=Buffs[k].MaterialList[i];
        setlength(RenderDescr.RenderCount,Buffs[k].FaceTypes.Count);
        setlength(RenderDescr.RenderIndice,Buffs[k].FaceTypes.Count);
        RenderDescr.RenderBuff:=@Buffs[k].Buff;
        RenderDescr.MaterialName:=matname;
        RenderDescr.FaceType:=ft; n:=0;
     end;
     RenderDescr.RenderCount[n]:=Buffs[k].CountList[i];
     RenderDescr.RenderIndice[n]:=Buffs[k].IndiceList[i];
     inc(n);
   end;
   if n<>0 then begin
      setlength(RenderDescr.RenderCount,n);
      setlength(RenderDescr.RenderIndice,n);
      RenderDescr.ElementsCount:=n;
      RenderList.add(RenderDescr);
   end;
   Buffs[k].Buff.UseTwoTexturesCoord:=true;
   GenVBOBuff(Buffs[k].Buff,FreeRAM);
  end;
end;

Function CreatePlane(Width, Height: single; TilesX, TilesY:integer;
                     HFunc: TGetHeightFunc=nil;
                     AsLine:boolean=false;GenBuff:Boolean=true): PVBOBuffer;
var R:TRect;
  i, j, ox, oy, offs: integer;
  LW: integer;
  h,kx,ky:single;
  v, v1, v2, v3, nm: TAffineVector;
  Temp: PVBOBuffer;
begin
  new(Temp);R:=Rect(0,0,TilesX,TilesY);
  kx:=(width)/(tilesx); ky:=(height)/(tilesy);
  if AsLine then begin
    InitVBOBuff(Temp^, GL_LINE_STRIP, DrawElements);
    Temp.RenderBuffs:=[uIndices];
  end else begin
    InitVBOBuff(Temp^, GL_TRIANGLE_STRIP, DrawElements);
    Temp.RenderBuffs:=[uNormals,uIndices,uTexCoords];
  end;
  with Temp^ do begin
//    indices.Add(0);//
    LW := R.Right - R.Left + 1;
    for i := R.Top to R.Bottom do begin
      oy := i - R.Top;
      for j := R.Left to R.Right do begin
        if assigned(HFunc) then H:=HFunc(TilesY-i,j)
        else H := 0;
        SetVector(v, j*kx-width/2, H, i*ky-height/2);
        Vertexes.Add(v);
        if uTexCoords in RenderBuffs then begin
          v[0] := j*kx / (width);
          v[1] := 1 - i*ky / height;
          v[2] := (H+1)/2; TexCoords.Add(v);
        end;
        if (i < R.Bottom) then begin
          ox := j - R.Left; offs := oy * LW + ox;
          Indices.Add(offs, offs + LW);
        end;
      end;
      j := Indices[Indices.Count - 1]; offs := (oy + 1) * LW;
      Indices.Add(j, offs);
    end;
    if AsLine then begin
     Indices.Count:=Indices.Count-1;
     Indices.Add(0);
     for i:=0 to LW-1 do Indices.Add(i);
    end else Indices.Count:=Indices.Count-4;

    if uNormals in RenderBuffs then begin
      Normals.Count := Vertexes.Count;
      for i := 0 to Normals.Count-1 do Normals[i]:=NullVector; 
      for i := 0 to Indices.Count - 1 do begin
        v1 := Vertexes[Indices[i]];
        if i<=Indices.Count-3 then begin
          v2 := Vertexes[Indices[i + 1]];
          v3 := Vertexes[Indices[i + 2]];
        end else begin
          v3 := Vertexes[Indices[i - 1]];
          v2 := Vertexes[Indices[i - 2]];
        end;

        if (not VectorEquals(v1,v2)) and (not VectorEquals(v2,v3))
           and (not VectorEquals(v1,v3)) then begin
//           nm := CalcPlaneNormal(v1, v2, v3);
           if odd(i) then nm := CalcPlaneNormal(v3, v2, v1)
           else nm := CalcPlaneNormal(v1, v2, v3);
//           NegateVector(nm);
           Normals[Indices[i]]:=Vectoradd(Normals[Indices[i]],nm);
//           Normals[Indices[i]] := nm;
        end else begin
          v2 := Vertexes[Indices[i - 1]];
          v3 := Vertexes[Indices[i - 2]];
          if odd(i) then nm := CalcPlaneNormal(v1, v2, v3)
          else nm := CalcPlaneNormal(v3, v2, v1);
//          NegateVector(nm);
          Normals[Indices[i]] := Vectoradd(Normals[Indices[i]],nm);//nm;
        end;
      end;
    end;
    Vertexes.GetExtents(emin, emax);
    Normals.Normalize;
  end;
  if assigned(HFunc) then Temp^.UseTwoTexturesCoord:=false;
  if GenBuff then GenVBOBuff(Temp^, False);
  Temp.MatName:=''; Result:=Temp;
end;

function CreateCubicOccluder(GenVBO: boolean=true): PVBOBuffer;
const
  BoxVert: array[0..7] of TAffineVector = (
    (-1,1,-1),(-1,-1,-1),(1,-1,-1),(1,1,-1),
    (-1,1,1), (-1,-1,1), (1,-1, 1),(1,1,1));
  BoxFaces: array[0..5,0..3] of integer = (
    (0,1,2,3), (4,0,3,7), (5,4,7,6),
    (1,5,6,2), (4,5,1,0), (3,2,6,7));
var P: PVBOBuffer;
    i,j: integer;
begin
  new(P); InitVBOBuff(P^,GL_QUADS,DrawElements);
  with P^ do begin
     for i:=0 to 7 do Vertexes.Add(BoxVert[i]);
     for i:=0 to 5 do for j:=3 downto 0 do Indices.Add(BoxFaces[i,j]);
     RenderBuffs:=[uIndices];
  end;
  if GenVBO then GenVBOBuff(P^); result:=P;
end;


procedure SortListByMaterial(List:TList);
var i,j,k:integer;
    s:string;
    P:PVBOBuffer;
begin
   if not assigned(List) or (List.Count<2) then exit;
   for j:=0 to List.Count-2 do begin
      P:=List[j];s:=P.MatName;k:=j+1;
      for i:=j+1 to List.Count-1 do begin
         P:=List[i];
         if (P.MatName=s) and (i<>k) then begin
            List.Exchange(i,k);inc(k);end;
      end;
   end;
end;

procedure FreeVBOList(var List:TList;ClearBuffs: boolean = true);
var i:integer;
    p:PVBOBuffer;
begin
if not assigned(List) then exit;
  for i:=0 to List.Count-1 do begin
     P:=List[i];
     if P<>nil then begin
        FreeVBOBuffer(P^,ClearBuffs); Dispose(P);
     end;
  end;
  List.Clear;
end;

procedure BindVBO(var Buff: TVBOBuffer);
var i: integer;
    BaseTex: cardinal;
    Attr: PVBOAttribute;
    IB: TInterleavedBuffer;
begin
  with Buff do begin
    if (vao<>0) and (uVAO in RenderBuffs) then glBindVertexArray(vao)
    else begin
      case Buff.Struct of
      bsSeparated: begin
          if (vId = 0) or (not Visible) then exit;
          glEnableClientState(GL_VERTEX_ARRAY);
          if (nId <> 0) and (uNormals in RenderBuffs) then begin
            glEnableClientState(GL_NORMAL_ARRAY);
            glBindBuffer(GL_ARRAY_BUFFER, nId);
            glNormalPointer(GL_FLOAT, 0, nil);
          end else glDisableClientState(GL_NORMAL_ARRAY);
          BaseTex:=GL_TEXTURE0;
          if (tId <> 0) and (uTexCoords in RenderBuffs) then begin
            glClientActiveTexture(GL_TEXTURE0);
            BaseTex:=GL_TEXTURE1;
            glEnableClientState(GL_TEXTURE_COORD_ARRAY);
            glBindBuffer(GL_ARRAY_BUFFER, tId);
            if UseTwoTexturesCoord then
               glTexCoordPointer(2, GL_FLOAT, SizeOf(TAffineVector), nil)
            else glTexCoordPointer(3, GL_FLOAT, SizeOf(TAffineVector), nil);
          end else glDisableClientState(GL_TEXTURE_COORD_ARRAY);

          if (stId <> 0) and (uSecondaryTexure in RenderBuffs) and (not (uMultitexture in RenderBuffs))
          then begin
            glClientActiveTexture(GL_TEXTURE1);
            glEnableClientState(GL_TEXTURE_COORD_ARRAY);
            glBindBuffer(GL_ARRAY_BUFFER, stId);
            glTexCoordPointer(3, GL_FLOAT, 0, nil);
          end else if (uMultitexture in RenderBuffs) then begin
            for i:= 0 to ExTexCoords.Count - 1 do begin
              glClientActiveTexture(BaseTex+i);
              glEnableClientState(GL_TEXTURE_COORD_ARRAY);
              attr:=ExTexCoords[i];
              glBindBuffer(GL_ARRAY_BUFFER, attr.Id);
              glTexCoordPointer(attr.CCount, attr.CType, attr.CSize, nil);
              if ( uExTexEnvMode in RenderBuffs) then
                 if i<ExTexEnvMode.Count then
                    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, ExTexEnvMode[i]);
            end;
          end;

          if (cId<>0) and (uColors in RenderBuffs) then begin
            glEnableClientState(GL_COLOR_ARRAY);
            glBindBuffer(GL_ARRAY_BUFFER, cId);
            glColorPointer(4,GL_FLOAT, 0, nil);
            glEnable(GL_COLOR_MATERIAL);
            glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
          end;

          for i:=0 to AttribList.Count-1 do begin
            attr:=AttribList[i];
            if (attr.Location>0) and (attr.Id>0) then begin
              glBindBuffer(GL_ARRAY_BUFFER, attr.Id);
              glEnableVertexAttribArray(attr.Location);
              glVertexAttribPointer(attr.Location, attr.CCount, attr.CType, false, 0, nil);
            end;
          end;

          glBindBuffer(GL_ARRAY_BUFFER, vId);
          glVertexPointer(3, GL_FLOAT, 0, nil);

          if ((iId <> 0) and (uIndices in RenderBuffs)) then
            glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, iId);
        end;
      bsInterleaved: begin
          if (not Visible) then exit;
          assert(AttribList.Count>0,'Interleaved attributes not found.');
          attr:=AttribList[0];
          assert(Attr.AttrType=atInterleaved,'Not interleaved attribute.');
          assert(assigned(attr.DataHandler),'Data hendler is not assigned');
          assert(attr.DataHandler is TInterleavedBuffer, 'Data handler is not TInterleavedBuffer.');
          glBindBuffer(GL_ARRAY_BUFFER, attr.Id);
          IB:=TInterleavedBuffer(attr.DataHandler);
          IB.BindToVBO(Buff);
          if ((iId <> 0) and (uIndices in RenderBuffs)) then
            glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, iId);
        end;
      end;
    end;
  end;
end;


Procedure UnBindVBO(var Buff: TVBOBuffer);
var i: integer;
    Attr: PVBOAttribute;
    IB: TInterleavedBuffer;
begin
  with Buff do begin
    if (vao=0) or (not(uVAO in RenderBuffs)) then begin
      case Buff.Struct of
        bsSeparated: begin
          glDisableClientState(GL_VERTEX_ARRAY);
          if nId <> 0 then glDisableClientState(GL_NORMAL_ARRAY);
          if tId <> 0 then glDisableClientState(GL_TEXTURE_COORD_ARRAY);
          if cId <> 0 then begin
             glDisableClientState(GL_COLOR_ARRAY);
             glDisable(GL_COLOR_MATERIAL);
          end;
          if (uMultitexture in RenderBuffs) then begin
            for i:= 0 to ExTexCoords.Count - 1 do begin
                glClientActiveTexture(GL_TEXTURE0+i);
                glDisableClientState(GL_TEXTURE_COORD_ARRAY);
            end;
          end;
          glBindBuffer(GL_ARRAY_BUFFER, 0);
          if (iId <> 0) then glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
        end;
        bsInterleaved: begin
          assert(AttribList.Count>0,'Interleaved attributes not found.');
          attr:=AttribList[0];
          assert(Attr.AttrType=atInterleaved,'Not interleaved attribute.');
          assert(attr.DataHandler is TInterleavedBuffer,'Data handler is not TInterleavedBuffer.');
          glBindBuffer(GL_ARRAY_BUFFER, 0);
          glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
          IB:=TInterleavedBuffer(attr.DataHandler);
          IB.UnBindVBO(Buff);
        end;
      end;
    end else glBindVertexArray(0);
  end;
end;

function GenVAO(VBuff: PVBOBuffer): TVertexArrayObject;overload;
var i: integer;
    attr: PVBOAttribute;
begin
  result.vao:=0; result.SourceVBO:=VBuff;
  if (not assigned(VBuff)) or (not VBuff.Builded) then exit;

  glGenVertexArrays(1, @Result.vao);
  glBindVertexArray(Result.vao);
  with VBuff^ do begin

    glEnableClientState( GL_VERTEX_ARRAY );
    glBindBuffer(GL_ARRAY_BUFFER, vId );
    glVertexPointer( 3, GL_FLOAT, 0, nil );

    if (cId<>0) and (uColors in RenderBuffs) then begin
      glEnableClientState( GL_COLOR_ARRAY );
      glBindBuffer(GL_ARRAY_BUFFER, cId );
      glColorPointer(4,GL_FLOAT, 0, nil);
    end;

    if (nId <> 0) and (uNormals in RenderBuffs) then begin
      glEnableClientState( GL_NORMAL_ARRAY );
      glBindBuffer(GL_ARRAY_BUFFER, nId );
      glNormalPointer(GL_FLOAT, 0, nil );
    end;

    if (tId <> 0) and (uTexCoords in RenderBuffs) then begin
      glEnableClientState( GL_TEXTURE_COORD_ARRAY );
      glBindBuffer(GL_ARRAY_BUFFER, tId );
      if UseTwoTexturesCoord then
        glTexCoordPointer(2, GL_FLOAT, SizeOf(TAffineVector), nil)
      else glTexCoordPointer(3, GL_FLOAT, SizeOf(TAffineVector), nil);
    end;

    for i:=0 to AttribList.Count-1 do begin
      attr:=AttribList[i];
      if (attr.Location>0) and (attr.Id>0) then begin
        glBindBuffer(GL_ARRAY_BUFFER, attr.Id);
        glEnableVertexAttribArray(attr.Location);
        glVertexAttribPointer(attr.Location, attr.CCount, attr.CType, false, 0, nil);
      end;
    end;


    if ((iId <> 0) and (uIndices in RenderBuffs)) then
      glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, iId);
  end;

  glBindVertexArray(0);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);

end;

function GenVAO(var VBuff: TVBOBuffer): GLUInt; overload;
var vao: GLUint;
    i: integer;
    attr: PVBOAttribute;
begin
  if (not VBuff.Builded) then exit;
  glGenVertexArrays(1, @vao);
  glBindVertexArray(vao);
  BindVBO(VBuff);
  VBuff.VAO:=vao; result:=vao;
  glBindVertexArray(0);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
end;

procedure RenderMeshes(const VBuff: TVBOBuffer; SubMeshes: TList);
var i: integer;
    SubMesh: PSubMesh;
begin
  for i:=0 to SubMeshes.Count-1 do begin
    SubMesh:=SubMeshes[i];
    assert(SubMesh.PrimType>=0,'Unknown Pimitive type');
    if assigned(VBuff.MaterialFunc) then VBuff.MaterialFunc(SubMesh.MaterialName,maApply);
    glDrawElements(SubMesh.PrimType, SubMesh.PrimitiveCount*3,
      GL_UNSIGNED_INT, pointer(SubMesh.StartingIndex*4));
    if assigned(VBuff.MaterialFunc) then VBuff.MaterialFunc(SubMesh.MaterialName,maUnApply);
  end;
end;

procedure RenderVAO(const VAO: TVertexArrayObject);
var RCount: integer;
    vbo: PVBOBuffer;
begin
  if (vao.vao=0) or (not assigned(vao.SourceVBO)) then exit;
  vbo:= VAO.SourceVBO;
  glBindVertexArray(vao.vao);
  with vao.SourceVBO^ do begin
    if ElementsCount>MaxElements then RCount:=MaxElements else
    RCount:=ElementsCount;

    if SubMeshes.Count=0 then begin
      case RenderType of
        DrawArrays: glDrawArrays(FaceType, 0, RCount);
        DrawElements: glDrawElements(FaceType, RCount, GL_UNSIGNED_INT, nil);
      end;
    end else RenderMeshes(VBO^,VBO^.SubMeshes);
{
    case RenderType of
      DrawArrays: glDrawArrays(FaceType, 0, RCount);
      DrawElements: glDrawElements(FaceType, RCount, GL_UNSIGNED_INT, nil);
    end;
}
  end;
  glBindVertexArray(0);
end;

procedure RenderVBOBuffer(var VBuff: TVBOBuffer);
var i,RCount:integer;
    BaseTex: cardinal;
    Attr: PVBOAttribute;
begin
  if (not VBuff.Builded) then GenVBOBuff(VBuff,false);
  BindVBO(VBuff);
  with VBuff do begin
    if ElementsCount>MaxElements then RCount:=MaxElements else
    RCount:=ElementsCount;
    if SubMeshes.Count=0 then begin
      case RenderType of
        DrawArrays: glDrawArrays(FaceType, 0, RCount);
        DrawElements: glDrawElements(FaceType, RCount, GL_UNSIGNED_INT, nil);
      end;
    end else RenderMeshes(VBuff,VBuff.SubMeshes);
  end;
  UnBindVBO(VBuff);
end;

procedure RenderVBOBuffer(var VBuff: TVBOBuffer; MatList: TList);overload;
var i,RCount:integer;
    m: PMatrix;
    Attr: PVBOAttribute;
begin
  if (not assigned(MatList)) or (MatList.Count=0) then exit;
  BindVBO(VBuff);
  with VBuff do begin
    if ElementsCount>MaxElements then RCount:=MaxElements
    else RCount:=ElementsCount;
    for i:=0 to MatList.Count-1 do begin
        m:=MatList[i]; glLoadMatrixf(PGLFloat(m));
        case RenderType of
          DrawArrays: glDrawArrays(FaceType, 0, RCount);
          DrawElements: glDrawElements(FaceType, RCount, GL_UNSIGNED_INT, nil);
        end;
    end;
  end;
  UnBindVBO(VBuff);
end;

procedure RenderVBOMultiBuffer(var Buffs: TMultiBuffer; SingleCall:boolean=true);
var i,j:integer;
begin
for i:=0 to high(Buffs) do begin
 with Buffs[i], Buffs[i].Buff do begin
    if (not Builded) or (vId = 0) or (ElementsCount=-1) then exit;
    glEnableClientState(GL_VERTEX_ARRAY);
    if (nId <> 0) and (uNormals in RenderBuffs) then begin
      glEnableClientState(GL_NORMAL_ARRAY);
      glBindBuffer(GL_ARRAY_BUFFER, nId);
      glNormalPointer(GL_FLOAT, 0, nil);
    end else glDisableClientState(GL_NORMAL_ARRAY);

    if (tId <> 0) and (uTexCoords in RenderBuffs) then begin
      glClientActiveTexture(GL_TEXTURE0);
      glEnableClientState(GL_TEXTURE_COORD_ARRAY);
      glBindBuffer(GL_ARRAY_BUFFER, tId);
      if UseTwoTexturesCoord then
         glTexCoordPointer(2, GL_FLOAT, SizeOf(TAffineVector), nil)
      else glTexCoordPointer(3, GL_FLOAT, SizeOf(TAffineVector), nil);
    end else glDisableClientState(GL_TEXTURE_COORD_ARRAY);
    if (stId <> 0) and (uSecondaryTexure in RenderBuffs) then begin
      glClientActiveTexture(GL_TEXTURE1);
      glEnableClientState(GL_TEXTURE_COORD_ARRAY);
      glBindBuffer(GL_ARRAY_BUFFER, stId);
      glTexCoordPointer(3, GL_FLOAT, 0, nil);
    end;

    glBindBuffer(GL_ARRAY_BUFFER, vId);
    glVertexPointer(3, GL_FLOAT, 0, nil);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, iId);

    if SingleCall then
       glMultiDrawElements (FaceType, @RenderCount[0], GL_UNSIGNED_INT, RenderIndice[0], ElementsCount)
    else for j:=0 to ElementsCount-1 do
             glDrawElements(FaceType, RenderCount[j],GL_UNSIGNED_INT,RenderIndice[j]);

    glDisableClientState(GL_VERTEX_ARRAY);
    if nId <> 0 then glDisableClientState(GL_NORMAL_ARRAY);
    if tId <> 0 then glDisableClientState(GL_TEXTURE_COORD_ARRAY);
    if (iId <> 0) and (not idxBindOnce) then glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);

 end;
end;
end;

procedure RenderVBOMultiList(RenderList: TList; SingleCall:boolean=true);
var i,j:integer;
    PMB:PMultiRenderDescr;
    binded:pointer;
begin
  binded:=nil;
  for i:=0 to RenderList.Count-1 do begin
   PMB:=RenderList[i];
   with PMB.RenderBuff^ do begin
   if binded<>PMB.RenderBuff then begin
    if (not Builded) or (vId = 0) then exit;
    glEnableClientState(GL_VERTEX_ARRAY);
    if (nId <> 0) and (uNormals in RenderBuffs) then begin
      glEnableClientState(GL_NORMAL_ARRAY);
      glBindBuffer(GL_ARRAY_BUFFER, nId);
      glNormalPointer(GL_FLOAT, 0, nil);
    end else glDisableClientState(GL_NORMAL_ARRAY);

    if (tId <> 0) and (uTexCoords in RenderBuffs) then begin
      glClientActiveTexture(GL_TEXTURE0);
      glEnableClientState(GL_TEXTURE_COORD_ARRAY);
      glBindBuffer(GL_ARRAY_BUFFER, tId);
      if UseTwoTexturesCoord then
         glTexCoordPointer(2, GL_FLOAT, SizeOf(TAffineVector), nil)
      else glTexCoordPointer(3, GL_FLOAT, SizeOf(TAffineVector), nil);
    end else glDisableClientState(GL_TEXTURE_COORD_ARRAY);
    if (stId <> 0) and (uSecondaryTexure in RenderBuffs) then begin
      glClientActiveTexture(GL_TEXTURE1);
      glEnableClientState(GL_TEXTURE_COORD_ARRAY);
      glBindBuffer(GL_ARRAY_BUFFER, stId);
      glTexCoordPointer(3, GL_FLOAT, 0, nil);
    end;
    if (cId<>0) and (uColors in RenderBuffs) then begin
      glEnableClientState(GL_COLOR_ARRAY);
      glBindBuffer(GL_ARRAY_BUFFER, cId);
      glColorPointer(4,GL_FLOAT, 0, nil);
      glEnable(GL_COLOR_MATERIAL);
    end;

    glBindBuffer(GL_ARRAY_BUFFER, vId);
    glVertexPointer(3, GL_FLOAT, 0, nil);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, iId);
    binded:=PMB.RenderBuff;
    end;
    if SingleCall then
       glMultiDrawElements (PMB^.FaceType,@PMB^.RenderCount[0], GL_UNSIGNED_INT, PMB^.RenderIndice[0], PMB^.ElementsCount)
    else for j:=0 to PMB.ElementsCount-1 do begin
             glDrawElements(PMB.FaceType, PMB.RenderCount[j],GL_UNSIGNED_INT,PMB^.RenderIndice[j]);
    end;
   end;
    glDisableClientState(GL_VERTEX_ARRAY);
    glDisableClientState(GL_NORMAL_ARRAY);
    glDisableClientState(GL_TEXTURE_COORD_ARRAY);
    glDisableClientState(GL_COLOR_ARRAY);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
  end;
end;

procedure RenderVBOMultiPart(Description: PMultiRenderDescr; BindBuffer:TBindState; SingleCall:boolean=true);
var j:integer;
    PMB:PMultiRenderDescr;
begin
   PMB:=Description;
   with PMB.RenderBuff^ do begin
   if sActivate in BindBuffer then begin
    if (not Builded) or (vId = 0) then exit;
    glEnableClientState(GL_VERTEX_ARRAY);
    if (nId <> 0) and (uNormals in RenderBuffs) then begin
      glEnableClientState(GL_NORMAL_ARRAY);
      glBindBuffer(GL_ARRAY_BUFFER, nId);
      glNormalPointer(GL_FLOAT, nccount*4, nil);
    end else glDisableClientState(GL_NORMAL_ARRAY);
    if (tId <> 0) and (uTexCoords in RenderBuffs) then begin
      glClientActiveTexture(GL_TEXTURE0);
      glEnableClientState(GL_TEXTURE_COORD_ARRAY);
      glBindBuffer(GL_ARRAY_BUFFER, tId);
      if UseTwoTexturesCoord then
         glTexCoordPointer(2, GL_FLOAT, SizeOf(TAffineVector), nil)
      else glTexCoordPointer(3, GL_FLOAT, SizeOf(TAffineVector), nil);
    end else glDisableClientState(GL_TEXTURE_COORD_ARRAY);
    if (stId <> 0) and (uSecondaryTexure in RenderBuffs) then begin
      glClientActiveTexture(GL_TEXTURE1);
      glEnableClientState(GL_TEXTURE_COORD_ARRAY);
      glBindBuffer(GL_ARRAY_BUFFER, stId);
      glTexCoordPointer(3, GL_FLOAT, 0, nil);
    end;
    if (cId<>0) and (uColors in RenderBuffs) then begin
      glEnableClientState(GL_COLOR_ARRAY);
      glBindBuffer(GL_ARRAY_BUFFER, cId);
      glColorPointer(4,GL_FLOAT, 0, nil);
      glEnable(GL_COLOR_MATERIAL);
    end;

    glBindBuffer(GL_ARRAY_BUFFER, vId);
    glVertexPointer(3, GL_FLOAT, vccount*4, nil);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, iId);
   end;
    if SingleCall then
       glMultiDrawElements (PMB^.FaceType,@PMB^.RenderCount[0], GL_UNSIGNED_INT, PMB^.RenderIndice[0], PMB^.ElementsCount)
    else for j:=0 to PMB.ElementsCount-1 do begin
             glDrawElements(PMB.FaceType, PMB.RenderCount[j],GL_UNSIGNED_INT,PMB^.RenderIndice[j]);
    end;
   end;
   if sDeactivate in BindBuffer then begin
    glDisableClientState(GL_VERTEX_ARRAY);
    glDisableClientState(GL_NORMAL_ARRAY);
    glDisableClientState(GL_TEXTURE_COORD_ARRAY);
    glDisableClientState(GL_COLOR_ARRAY);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
   end;
end;
procedure RenderVBOPackBuff(var VBuff: TPackedVBO);
var i: integer;
    pidx: PObjIndices;
begin
  with VBuff.Buff do begin
    if (not Builded) or (vId = 0) then exit;
    glEnableClientState(GL_VERTEX_ARRAY);
    if (nId <> 0) and (uNormals in RenderBuffs) then begin
      glEnableClientState(GL_NORMAL_ARRAY);
      glBindBuffer(GL_ARRAY_BUFFER, nId);
      glNormalPointer(GL_FLOAT, 0, nil);
    end else glDisableClientState(GL_NORMAL_ARRAY);

    if (tId <> 0) and (uTexCoords in RenderBuffs) then begin
      glClientActiveTexture(GL_TEXTURE0);
      glEnableClientState(GL_TEXTURE_COORD_ARRAY);
      glBindBuffer(GL_ARRAY_BUFFER, tId);
      glTexCoordPointer(2, GL_FLOAT, SizeOf(TAffineVector), nil);
    end else glDisableClientState(GL_TEXTURE_COORD_ARRAY);
    if (stId <> 0) and (uSecondaryTexure in RenderBuffs) then begin
      glClientActiveTexture(GL_TEXTURE1);
      glEnableClientState(GL_TEXTURE_COORD_ARRAY);
      glBindBuffer(GL_ARRAY_BUFFER, stId);
      glTexCoordPointer(3, GL_FLOAT, 0, nil);
    end;

    glBindBuffer(GL_ARRAY_BUFFER, vId);
    glVertexPointer(3, GL_FLOAT, 0, nil);

    if VBuff.UseExIndices and (VBuff.IndicesLists.Count > 0) then begin
      for i := 0 to VBuff.IndicesLists.Count - 1 do begin
        pidx := VBuff.IndicesLists[i];
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, pidx.IndicesId);
        glDrawElements(FaceType, pidx.Indices.Count, GL_UNSIGNED_INT, nil);
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
      end;
    end else begin
      if idxBindOnce then begin
        if (not idxBuffApply) and ((iId <> 0) and (uIndices in RenderBuffs)) then begin
          glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, iId);
          glDrawElements(FaceType, Indices.Count, GL_UNSIGNED_INT, nil);
          idxBuffApply := true; end;
      end else begin
        if ((iId <> 0) and (uIndices in RenderBuffs)) then
          glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, iId);
        glDrawElements(FaceType, Indices.Count, GL_UNSIGNED_INT, nil);
      end;
    end;

    glDisableClientState(GL_VERTEX_ARRAY);
    if nId <> 0 then glDisableClientState(GL_NORMAL_ARRAY);
    if tId <> 0 then glDisableClientState(GL_TEXTURE_COORD_ARRAY);
    if (iId <> 0) and (not idxBindOnce) then glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
  end;
end;

procedure RenderVBOList(List: TList);
var i: integer;
    P: PVBOBuffer;
begin
  if List.Count=0 then exit;
  for i := 0 to List.Count - 1 do begin
    P := List[i]; RenderVBOBuffer(p^);
  end;
end;

procedure RenderVBOList(List: TList; MatList: TList);overload;
var i: integer;
    P: PVBOBuffer;
begin
  if List.Count=0 then exit;
  for i := 0 to List.Count - 1 do begin
    P := List[i]; RenderVBOBuffer(p^, MatList);
  end;
end;
procedure Col2RowMatrix(mm: TMatrix; var m: TMatrix);
begin
  m[0, 0] := mm[0, 0]; m[1, 0] := mm[0, 1]; m[2, 0] := mm[0, 2]; m[3, 0] := mm[0, 3];
  m[0, 1] := mm[1, 0]; m[1, 1] := mm[1, 1]; m[2, 1] := mm[1, 2]; m[3, 1] := mm[1, 3];
  m[0, 2] := mm[2, 0]; m[1, 2] := mm[2, 1]; m[2, 2] := mm[2, 2]; m[3, 2] := mm[2, 3];
  m[0, 3] := mm[3, 0]; m[1, 3] := mm[3, 1]; m[2, 3] := mm[3, 2]; m[3, 3] := mm[3, 3];
end;

function RenderVBOListWithCulling(List: TList; var WorldMatrix: TMatrix; bounds: boolean = false): integer;
var i: integer;
  P: PVBOBuffer;
  F: TFrustum;
  objPos: TAffineVector;
  Radius: single;
  emin, emax: TAffineVector;
  Temp: TMatrix;
  PatchesCount: integer;
begin
  F := GetFrustum; PatchesCount := 0;
  for i := 0 to List.Count - 1 do begin
    p := List[i];
    if bounds then RenderBounds(P^.emin, P^.emax, 1, 0, 0, 1);
    Col2RowMatrix(WorldMatrix, Temp);
    emin := VectorTransform(P^.emin, Temp);
    emax := VectorTransform(P^.emax, Temp);
//    emin:=P^.emin; emax:=P^.emax;
    objPos := VectorScale(VectorAdd(emin, emax), 0.5);
    Radius := VectorDistance(emin, emax) * 0.5;
    if not isVolumeClipped(objPos, Radius, F) then begin
      RenderVBOBuffer(p^); inc(PatchesCount);
    end;
  end;
  Result := PatchesCount;
end;

//       

procedure AttachBuffer(var FromBuff, ToBuff: TVBOBuffer; AttachIndice: boolean = true);
var i, idx: integer;
  idxoffs: integer;
begin
  with ToBuff do begin
    idxoffs := Vertexes.Count;
    Vertexes.Add(FromBuff.Vertexes);
    Normals.Add(FromBuff.Normals);
    TexCoords.Add(FromBuff.TexCoords);
    if FaceType=GL_TRIANGLE_STRIP then begin
       if Indices.Count>0 then begin
          i:=Indices.Count-1;
          indices.Add(Indices[i]);
          indices.Add(FromBuff.Indices[0]+idxoffs);
       end;
    end;
    if AttachIndice then begin
      for i := 0 to FromBuff.Indices.Count - 1 do begin
        idx := FromBuff.Indices[i] + idxoffs;
        Indices.Add(idx);
      end;
    end;
  end;
end;

procedure AttachBufferInPos(var FromBuff, ToBuff: TVBOBuffer; vPos,iPos:integer);
var i, idx: integer;
  idxoffs: integer;
begin
  idxoffs := vPos;//ToBuff.Vertexes.Count;
  with ToBuff do begin
    if Vertexes.Count<vPos+FromBuff.Vertexes.Count then
      Vertexes.Count:=FromBuff.Vertexes.Count+vPos;
    for i:=0 to FromBuff.Vertexes.Count-1 do
      Vertexes[vPos+i]:=FromBuff.Vertexes[i];
    if uNormals in ToBuff.RenderBuffs then begin
      if Normals.Count<vPos+FromBuff.Normals.Count then
        Normals.Count:=FromBuff.Normals.Count+vPos;
      for i:=0 to FromBuff.Normals.Count-1 do
        Normals[vPos+i]:=FromBuff.Normals[i];
    end;
    if uColors in ToBuff.RenderBuffs then begin
       if Colors.Count<vPos+FromBuff.Colors.Count then
          Colors.Count:=FromBuff.Colors.Count+vPos;
       for i:=0 to FromBuff.Colors.Count-1 do
          Colors[vPos+i]:=FromBuff.Colors[i];
    end;
    if uTexCoords in ToBuff.RenderBuffs then begin
       if TexCoords.Count<vPos+FromBuff.TexCoords.Count then
          TexCoords.Count:=FromBuff.TexCoords.Count+vPos;
       for i:=0 to FromBuff.TexCoords.Count-1 do
          TexCoords[vPos+i]:=FromBuff.TexCoords[i];
    end;
    if uIndices in ToBuff.RenderBuffs then begin
       if Indices.Count<iPos+FromBuff.Indices.Count then
          Indices.Count:=FromBuff.Indices.Count+iPos;
       for i := 0 to FromBuff.Indices.Count - 1 do begin
           idx := FromBuff.Indices[i] + idxoffs;
           Indices[iPos+i]:=idx;
       end;
    end;
  end;
end;

procedure OptimizeIndices(CacheSize: integer; BuffSize: integer; var Buff: TVBOBuffer; var BuffList: TList; GenBuff: boolean = true);
var i, j, k, n, c, d, ic, id, idn: integer;
  tempIndices: TIntegerList;
  TempBuff: PVBOBuffer;
  v, nm, t: TaffineVector;
  PatchLists: TList;
begin
  with Buff do begin
    n := (BuffSize div 3) * 3; //Align to triangles
    c := Indices.Count; ic := c div n;
    tempIndices := TIntegerList.Create;
    PatchLists := TList.Create;
    for i := 0 to ic do begin
      tempIndices.Clear;
      if i <> ic then d := n else d := c - (ic) * n;
      tempIndices.Count := d;
      for j := 0 to d - 1 do tempIndices[j] := Indices[i * n + j];
      New(TempBuff); InitVBOBuff(TempBuff^, GL_TRIANGLES, DrawElements);
      for j := 0 to tempIndices.Count - 1 do begin
        id := tempIndices[j];
        if id > 0 then begin
          v := Vertexes[id];
          TempBuff.Vertexes.Add(v);
          if Normals.Count > 0 then begin
            nm := Normals[id];
            TempBuff.Normals.Add(nm);
          end;
          if TexCoords.Count > 0 then begin
            t := TexCoords[id];
            TempBuff.TexCoords.Add(t);
          end;
          idn := TempBuff.Vertexes.Count - 1;
          TempBuff.Indices.Add(idn);
          for k := j to tempIndices.Count - 1 do
            if tempIndices[k] = id then tempIndices[k] := -idn;
        end else begin
          if (id = 0) and (TempBuff.Vertexes.Count = 0) then begin
            v := Vertexes[id];
            TempBuff.Vertexes.Add(v);
            if Normals.Count > 0 then begin
              nm := Normals[id];
              TempBuff.Normals.Add(nm);
            end;
            if TexCoords.Count > 0 then begin
              t := TexCoords[id];
              TempBuff.TexCoords.Add(t);
            end;
          end;
          TempBuff.Indices.Add(abs(id));
        end;
      end;
      PatchLists.Add(TempBuff);
      if cachesize > 0 then
        IncreaseCoherency(TempBuff.Indices, cachesize);
      if GenBuff then GenVBOBuff(TempBuff^, false);
      TempBuff.RenderBuffs := TempBuff.RenderBuffs;
    end;
  end;
  BuffList := PatchLists;
end;

procedure ExtractTriangles(var Buff: TVBOBuffer; var Triangles: TAffineVectorList);
var i,n: integer;
  v, v1, v2, v3: TAffineVector;
begin
  if not assigned(Triangles) then Triangles := TAffineVectorList.Create;
  with Buff do begin
   if (Vertexes.Count = 0)then exit;
   if uIndices in RenderBuffs then begin
    if Indices.Count=0 then exit;
    case FaceType of
      GL_TRIANGLES: begin
          n:=Triangles.Count;
          Triangles.Count:=n+Indices.Count;
          for i := 0 to Indices.Count - 1 do begin
            v := Vertexes[Indices[i]];
            Triangles[n+i]:=v;//.Add(v);
          end;
        end;
      GL_TRIANGLE_STRIP: begin
          v1 := Vertexes[Indices[0]];
          v2 := Vertexes[Indices[1]];
          for i := 2 to Indices.Count - 1 do begin
            v := Vertexes[Indices[i]];
            if odd(i) then Triangles.Add(v2, v1, v)
            else Triangles.Add(v1, v2, v);
            v1 := v2; v2 := v;
          end;
        end;
      GL_QUADS: begin
          for i := 0 to (Indices.Count div 4) - 1 do begin
            v  := Vertexes[Indices[i*4]];
            v1 := Vertexes[Indices[i*4+1]];
            v2 := Vertexes[Indices[i*4+2]];
            v3 := Vertexes[Indices[i*4+3]];
            Triangles.Add(v,v1,v2);
            Triangles.Add(v2,v3,v);
          end;
      end;
    end;
   end else begin
    case FaceType of
      GL_TRIANGLES: begin
          Triangles.Add(Vertexes);
        end;
      GL_TRIANGLE_STRIP: begin
          v1 := Vertexes[Indices[0]];
          v2 := Vertexes[Indices[1]];
          for i := 2 to Vertexes.Count - 1 do begin
            v := Vertexes[i];
            if odd(i) then Triangles.Add(v2, v1, v)
            else Triangles.Add(v1, v2, v);
            v1 := v2; v2 := v;
          end;
        end;
      GL_QUADS: begin
          for i := 0 to (Vertexes.Count div 4) - 1 do begin
            v  := Vertexes[i*4];
            v1 := Vertexes[i*4+1];
            v2 := Vertexes[i*4+2];
            v3 := Vertexes[i*4+3];
            Triangles.Add(v,v1,v2);
            Triangles.Add(v2,v3,v);
          end;
      end;
    end;
   end;
  end;
end;

procedure RenderBounds(emin, emax: TAffinevector; w, r, g, b: single);
begin
  glColor3f(r, g, b);
  glLineWidth(w);

  glBegin(GL_LINE_STRIP);
  glVertex3f(emin[0], emin[1], emin[2]);
  glVertex3f(emin[0], emax[1], emin[2]);
  glVertex3f(emax[0], emax[1], emin[2]);
  glVertex3f(emax[0], emin[1], emin[2]);
  glVertex3f(emin[0], emin[1], emin[2]);

  glVertex3f(emin[0], emin[1], emax[2]);
  glVertex3f(emin[0], emax[1], emax[2]);
  glVertex3f(emax[0], emax[1], emax[2]);
  glVertex3f(emax[0], emin[1], emax[2]);
  glVertex3f(emin[0], emin[1], emax[2]);
  glEnd;

  glBegin(GL_LINES);
  glVertex3f(emin[0], emax[1], emin[2]);
  glVertex3f(emin[0], emax[1], emax[2]);

  glVertex3f(emax[0], emax[1], emin[2]);
  glVertex3f(emax[0], emax[1], emax[2]);

  glVertex3f(emax[0], emin[1], emin[2]);
  glVertex3f(emax[0], emin[1], emax[2]);
  glEnd;
end;

Function GetViewPort:TVector4i;
begin
  glGetIntegerv(GL_VIEWPORT, @Result);
end;

Function GetViewMatrix:TMatrix;
begin
  glGetFloatv(GL_MODELVIEW_MATRIX, @Result);
end;

Function GetProjectionMatrix:TMatrix;
begin
  glGetFloatv(GL_PROJECTION_MATRIX, @Result);
end;

function ProjectPoint(aPoint: TVector;
	      const modelMatrix: TMatrix;
	      const projMatrix:TMatrix;
              const viewport: array of integer;
              var ScreenPos:TVector): boolean;
var iPos, oPos: TVector;
begin
    iPos:=aPoint; iPos[3]:=1;
    oPos:=VectorTransform(iPos,ModelMatrix);
    iPos:=VectorTransform(oPos,ProjMatrix);
    if (iPos[3] = 0) then begin
      Result:=false; exit; end;
    ScaleVector(iPos,1/iPos[3]);

    iPos[0] := iPos[0] * 0.5 + 0.5;
    iPos[1] := iPos[1] * 0.5 + 0.5;
    iPos[2] := iPos[2] * 0.5 + 0.5;


    iPos[0] := iPos[0] * viewport[2] + viewport[0];
    iPos[1] := viewport[3]-(iPos[1] * viewport[3] + viewport[1]);

    ScreenPos:=iPos;
    result:=true;
end;


function GetModelViewMatrix(WorldMatrix, ViewMatrix: TMatrix): PGLFloat;
var mv:TMatrix;
    V:PVector;
    M:PMatrix;
    i:integer;
begin
  mv:=MatrixMultiply(WorldMatrix,ViewMatrix);
  new(M);v:=PVector(M);
  for i:=0 to 3 do begin v^:=mv[i];inc(v);end;
  result:=PGLFloat(M);
end;
//|R d|-1  |Rt -Rt*d|
//|0 1|  = |0     1 |
function CreateViewMatrix(const ModelMatrix: TMatrix): TMatrix;
var d: TVector;
    mat3: TAffineMatrix;
begin
  d:=ModelMatrix[3];
  Setmatrix(mat3,ModelMatrix);
  TransposeMatrix(mat3);
  SetMatrix(Result,mat3);
  d:=VectorTransform(d,mat3);
  NegateVector(d); d[3]:=1;
  result[3]:=d;
end;

procedure CreateBBox(var buff: TVBOBuffer);
begin
  InitVBOBuff(Buff,GL_TRIANGLES,DrawElements);
  Buff.Vertexes.Add(-1,1,-1);
  Buff.Vertexes.Add(-1,1, 1);
  Buff.Vertexes.Add( 1,1, 1);
  Buff.Vertexes.Add( 1,1,-1);

  Buff.Vertexes.Add(-1,-1,-1);
  Buff.Vertexes.Add(-1,-1, 1);
  Buff.Vertexes.Add( 1,-1, 1);
  Buff.Vertexes.Add( 1,-1,-1);

  Buff.Indices.Add (0,4,1);
  Buff.Indices.Add (1,4,5);
  Buff.Indices.Add (1,5,2);
  Buff.Indices.Add (2,5,6);
  Buff.Indices.Add (2,6,3);
  Buff.Indices.Add (3,6,7);
  Buff.Indices.Add (3,7,0);
  Buff.Indices.Add (0,7,4);
  Buff.Indices.Add (0,1,3);
  Buff.Indices.Add (3,1,2);
  Buff.Indices.Add (5,4,6);
  Buff.Indices.Add (6,4,7);

  buff.RenderBuffs:=[];
end;

procedure ExtentsToTranslateAndScale(var Extents: TExtents; var pos, sc: TVector);
var size: TAffineVector;
begin
  size:=VectorSubtract(Extents.emax,Extents.emin);
  ScaleVector(size,0.5);
  pos:=VectorMake(VectorAdd(Extents.emin,size),1);
  sc:=vectormake(size,1);
end;

function HashKey(const v : TAffineVector; hashSize : Integer) : Integer;
begin
  Result:=((    Integer(PIntegerArray(@v)[0])
            xor Integer(PIntegerArray(@v)[1])
            xor Integer(PIntegerArray(@v)[2])) shr 16) and hashSize;
end;

function AttributeEquals(var buff1, buff2: TVBOBuffer; i1,i2: integer): boolean;
begin
  if (i1>=buff1.Vertexes.Count) or (i2>=buff2.Vertexes.Count) then begin
    result:=false; exit; end;
  result:=VectorEquals(buff1.Vertexes[i1],buff2.Vertexes[i2]);
  if uNormals in buff1.RenderBuffs then
    result:=result and VectorEquals(buff1.Normals[i1],buff2.Normals[i2]);
  if uTexCoords in buff1.RenderBuffs then
    result:=result and VectorEquals(buff1.TexCoords[i1],buff2.TexCoords[i2]);
  if uColors in buff1.RenderBuffs then
    result:=result and VectorEquals(buff1.Colors[i1],buff2.Colors[i2]);
end;

procedure IndexingBuff(var buff: TVBOBuffer);
var i,j: integer;
    Temp: TAffineVectorList;
    Indices: TIntegerList;
    TriList: TAffineVectorList;
    pBuff: PVBOBuffer;
begin
  assert(Buff.Indices.Count=0,'Only for unIndexinx buffer');
  new(pBuff); InitVBOBuff(pBuff^,GL_TRIANGLES,DrawElements);
  pBuff.RenderBuffs:=buff.RenderBuffs;
  Temp:=TAffineVectorList.Create;
  Indices:=TIntegerList.Create;
  for i:=0 to buff.Vertexes.Count-1 do begin
    j:=-1;
    while (j+1<Temp.Count) and (not AttributeEquals(buff, pBuff^,i,j+1)) do inc(j);
    if j<>Temp.Count then pBuff.Indices.Add(j+1)
    else begin
      pBuff.Vertexes.Add(buff.Vertexes[i]);
      if uNormals in buff.RenderBuffs then
        pBuff.Normals.Add(buff.Normals[i]);
      if uTexCoords in buff.RenderBuffs then
        pBuff.TexCoords.Add(buff.TexCoords[i]);
      if uColors in buff.RenderBuffs then
        pBuff.Colors.Add(buff.Colors[i]);
    end;
  end;
  FreeVBOBuffer(buff);
  buff:=pBuff^;
end;

procedure TriStripToTri(var buff: TVBOBuffer);
var NInd: TIntegerList;
    i,i1,i2,i3,n: integer;
begin
  assert(buff.FaceType=GL_TRIANGLE_STRIP,'Need Triangle strip buffer');
  if buff.Indices.Count>0 then begin
    NInd:=TIntegerList.Create; n:=0;
    NInd.Count:=(buff.Indices.Count-2)*3;
    for i:=0 to buff.Indices.Count-3 do begin
      if not odd(i) then begin
        i1:=buff.Indices[i];
        i2:=buff.Indices[i+1];
        i3:=buff.Indices[i+2];
      end else begin
        i3:=buff.Indices[i];
        i2:=buff.Indices[i+1];
        i1:=buff.Indices[i+2];
      end;
      if (i1=i2) or (i2=i3) or (i1=i3) then inc(n)
      else begin
        NInd[(i-n)*3]:=i1;
        NInd[(i-n)*3+1]:=i2;
        NInd[(i-n)*3+2]:=i3;
      end;
    end;
    buff.Indices.Free; buff.Indices:=NInd;
    buff.Indices.Count:=buff.Indices.Count-n*3;
  end else begin
    NInd:=buff.Indices; n:=0;
    NInd.Count:=(buff.Vertexes.Count-2)*3;
    for i:=0 to buff.Vertexes.Count-3 do begin
      with buff do begin
        if VectorEquals(Vertexes[i],Vertexes[i+1])
        or VectorEquals(Vertexes[i+1],Vertexes[i+2])
        or VectorEquals(Vertexes[i],Vertexes[i+2]) then inc(n)
        else begin
          if not odd(i) then begin
            NInd[(i-n)*3]:=i;
            NInd[(i-n)*3+1]:=i+1;
            NInd[(i-n)*3+2]:=i+2;
          end else begin
            NInd[(i-n)*3+2]:=i;
            NInd[(i-n)*3+1]:=i+1;
            NInd[(i-n)*3]:=i+2;
          end;
        end;
      end;
    end;
    buff.Indices.Count:=buff.Indices.Count-n*3;
  end;
  buff.FaceType:=GL_TRIANGLES;
end;

procedure RestripifyFaces(var buff: TVBOBuffer; Indexing: boolean = true);
begin
  case buff.FaceType of
    GL_TRIANGLE_STRIP: TriStripToTri(buff);
    GL_QUAD_STRIP: assert(false,'Not Ready');
    GL_TRIANGLE_FAN: assert(false,'Not Ready');
  end;
exit;
  if Indexing then begin
    assert(false,'Not Ready');
  end;
end;


function SplitPoint (const attr: array of TAffineVector; aCount: integer): TAffineVector;overload;
var i: integer;
begin
  result:=NullVector;
  for i:=0 to aCount-1 do AddVector(result,attr[i]);
  ScaleVector(result,1/aCount);
end;

function SplitPoint (const attr: array of TVector; aCount: integer): TVector;overload;
var i: integer;
begin
  result:=NullHmgVector;
  for i:=0 to aCount-1 do AddVector(result,attr[i]);
  ScaleVector(result,1/aCount);
end;

procedure Tesselate(var buff: TVBOBuffer; TessType: integer = 1; Smooth: boolean = false);
var i,j: integer;
    ind: array[0..3] of integer;
    vert: array[0..3] of TAffineVector;
    norm: array[0..3] of TAffineVector;
    tc: array[0..3] of TAffineVector;
    col: array[0..3] of TVector;
    np: array [0..3] of TAffineVector;
    cp: array [0..3] of TVector;
    mi: integer;
    l,tl: single;
    ni: integer;
    nv: TAffineVector;
    cv: TVector;
    vc: integer;
begin
  case buff.FaceType of
    GL_TRIANGLES: vc:=2;
    GL_QUADS: vc:=3;
    else begin RestripifyFaces(buff); vc:=2; end;
  end;
  i:=0;
  while i<buff.indices.count-vc do begin
    for j:=0 to vc do begin
      ind[j]:=buff.indices[i+j];
      vert[j]:=buff.vertexes[ind[j]];
      if buff.Normals.Count>0 then norm[j]:=buff.Normals[ind[j]];
      if buff.TexCoords.Count>0 then tc[j]:=buff.TexCoords[ind[j]];
      if buff.Colors.Count>0 then col[j]:=buff.Colors[ind[j]];
    end;
    if TessType=0 then begin
      //ищем большую сторону треугольника
      l:=VectorDistance2(vert[0],vert[vc]); mi:=vc;
      for j:=0 to vc-1 do begin
        tl:=VectorDistance2(vert[j],vert[j+1]);
        if tl>l then begin l:=tl; mi:=j; end;
      end;
      if mi=vc then with buff do begin
        nv:=vectorlerp(vert[0],vert[vc],0.5);
        if Normals.Count>0 then Normals.Add(vectorlerp(norm[0],norm[vc],0.5));
        if TexCoords.Count>0 then TexCoords.Add(vectorlerp(tc[0],tc[vc],0.5));
        if Colors.Count>0 then Colors.Add(vectorlerp(col[0],col[vc],0.5));
      end else with buff do begin
        nv:=vectorlerp(vert[mi],vert[mi+1],0.5);
        if Normals.Count>0 then Normals.Add(vectorlerp(norm[mi],norm[mi+1],0.5));
        if TexCoords.Count>0 then TexCoords.Add(vectorlerp(tc[mi],tc[mi+1],0.5));
        if Colors.Count>0 then Colors.Add(vectorlerp(col[mi],col[mi+1],0.5));
      end;
      ni:=buff.vertexes.add(nv);
      buff.indices[i+2]:=ni;
      buff.indices.insert(i+3,ni);
      buff.indices.insert(i+4,ind[2]);
      buff.indices.insert(i+5,ind[0]);
      i:=i+6;
    end;
    if TessType=1 then begin
      //Type=1 - точка в центре полигона
      nv:=SplitPoint(vert,vc+1);
      ni:=buff.vertexes.add(nv);
      if buff.Normals.Count>0 then begin
        nv:=SplitPoint(norm,vc+1); NormalizeVector(nv);
        buff.Normals.Add(nv);
      end;
      if buff.TexCoords.Count>0 then buff.TexCoords.Add(SplitPoint(tc,vc+1));
      if buff.Colors.Count>0 then buff.Colors.Add(SplitPoint(col,vc+1));
      if buff.FaceType=GL_TRIANGLES then begin
        //Создаем три треугольника
        //Заменяем третью точку первого треугольника на новую и добавляем еще два треугольника
        buff.indices[i+2]:=ni;
        buff.indices.insert(i+3,ind[0]);
        buff.indices.insert(i+4,ni);
        buff.indices.insert(i+5,ind[2]);
        buff.indices.insert(i+6,ind[2]);
        buff.indices.insert(i+7,ni);
        buff.indices.insert(i+8,ind[1]);
        i:=i+9;
      end else if buff.FaceType=GL_QUADS then begin
        assert(false,'Not Ready');
      end;
    end;
    if TessType=2 then begin
      //Type=2 - точка на каждом ребре треугольника
      if buff.FaceType=GL_TRIANGLES then begin
        //Добавлям к буферу новые точки с их вершинными атрибутами
        np[0]:=vectorLerp(vert[0],vert[1],0.5);
        np[1]:=vectorLerp(vert[1],vert[2],0.5);
        np[2]:=vectorLerp(vert[0],vert[2],0.5);
        ni:=buff.Vertexes.Add(np[0]);
        buff.Vertexes.Add(np[1]); buff.Vertexes.Add(np[2]);
        if buff.Normals.Count>0 then begin
          np[0]:=vectorLerp(norm[0],norm[1],0.5);
          np[1]:=vectorLerp(norm[1],norm[2],0.5);
          np[2]:=vectorLerp(norm[0],norm[2],0.5);
          buff.Normals.Add(np[0]);
          buff.Normals.Add(np[1]);
          buff.Normals.Add(np[2]);
        end;
        if buff.TexCoords.Count>0 then begin
          np[0]:=vectorLerp(tc[0],tc[1],0.5);
          np[1]:=vectorLerp(tc[1],tc[2],0.5);
          np[2]:=vectorLerp(tc[0],tc[2],0.5);
          buff.TexCoords.Add(np[0]);
          buff.TexCoords.Add(np[1]);
          buff.TexCoords.Add(np[2]);
        end;
        if buff.Colors.Count>0 then begin
          cp[0]:=vectorLerp(col[0],col[1],0.5);
          cp[1]:=vectorLerp(col[1],col[2],0.5);
          cp[2]:=vectorLerp(col[0],col[2],0.5);
          buff.Colors.Add(cp[0]);
          buff.Colors.Add(cp[1]);
          buff.Colors.Add(cp[2]);
        end;
        //создаем 4 треугольника, в первом заменяем две вершины
        buff.indices[i+1]:=ni;
        buff.indices[i+2]:=ni+2;
        //2
        buff.Indices.Insert(i+3,ni);
        buff.Indices.Insert(i+4,ind[1]);
        buff.Indices.Insert(i+5,ni+1);
        //3
        buff.Indices.Insert(i+6,ni);
        buff.Indices.Insert(i+7,ni+1);
        buff.Indices.Insert(i+8,ni+2);
        //4
        buff.Indices.Insert(i+9,ni+2);
        buff.Indices.Insert(i+10,ni+1);
        buff.Indices.Insert(i+11,ind[2]);
        i:=i+12;
      end else if buff.FaceType=GL_QUADS then begin
        assert(false,'Not Ready');
      end;

    end;

  end;
end;

function RandomVector(Scale: single=1): TAffineVector; overload;
begin
  result[0]:=low(Integer)+random(High(Cardinal));
  result[1]:=low(Integer)+random(High(Cardinal));
  result[2]:=low(Integer)+random(High(Cardinal));
  NormalizeVector(Result); ScaleVector(Result,Scale);
end;


{ TBuiltinMeshObject }

constructor TBuiltinMeshObject.Create(Owner: TMultiObjectMesh);
begin
  inherited Create;
  FSourceMesh:=Owner; FIndicesOffset:=Owner.FIndices.Count;
  FMaterialName:=''; FGroupName:='';
end;

function TBuiltinMeshObject.getIndices(index: integer): integer;
begin
  result:=FSourceMesh.Indices[FIndicesOffset + index];
end;

function TBuiltinMeshObject.getNormal(index: integer): TAffineVector;
begin
  result:=FSourceMesh.Normals[index];
end;

function TBuiltinMeshObject.getTexCoord(index: integer): TAffineVector;
begin
  result:=FSourceMesh.TexCoords[index];
end;

function TBuiltinMeshObject.getVertex(index: integer): TAffineVector;
begin
  result:=FSourceMesh.Vertices[index];
end;

procedure TBuiltinMeshObject.setIndices(index: integer; const Value: integer);
begin
  FSourceMesh.Indices[FIndicesOffset + index]:=Value;
end;

procedure TBuiltinMeshObject.setNormal(index: integer;
  const Value: TAffineVector);
begin
  FSourceMesh.Normals[index]:=Value;
end;

procedure TBuiltinMeshObject.setTexCoord(index: integer;
  const Value: TAffineVector);
begin
  FSourceMesh.TexCoords[index]:=Value;
end;

procedure TBuiltinMeshObject.setVertex(index: integer;
  const Value: TAffineVector);
begin
  FSourceMesh.Vertices[index]:=Value;
end;

{ TMultiObjectMesh }


function TMultiObjectMesh.BuildVBO: PVBOBuffer;
begin
  if assigned(FVBOBuff) then result:=FVBOBuff
  else begin
    new(FVBOBuff);
    FVBOBuff.Vertexes:=FVertices;
    FVBOBuff.Normals:=FNormals;
    FVBOBuff.TexCoords:=FTexCoords;
    FVBOBuff.Indices:=FIndices;

    with FVBOBuff^ do begin
      ExTexCoords:=TList.Create;
      ExTexEnvMode:=TIntegerList.Create;
      Colors:=TVectorList.Create;
      FaceType:=4;
      RenderType:=DrawElements;
      Builded:=false;
      RenderBuffs:=[];
      vId := 0; nId := 0; tId := 0; iId := 0; stId := 0;
      vccount:=0; nccount:=0; vao:=0;
      WorldMatrix := IdentityHmgMatrix;
      VertexCount := 0;
      idxBuffApply := false;
      idxBindOnce := false;
      ChildBuff := false;
      UseTwoTexturesCoord := True;
      solid:=false;
      Visible := true;
    end;
    FVBOBuff.RenderBuffs:=[uNormals,uTexCoords,uIndices];
    GenVBOBuff(FVBOBuff^,false);

    Result:=FVBOBuff;
  end;
end;

constructor TMultiObjectMesh.Create;
begin
  inherited;
  FMeshObjects:=TList.Create;
  FVertices:=TAffineVectorList.Create;
  FNormals:=TAffineVectorList.Create;
  FTexCoords:=TAffineVectorList.Create;
  FIndices:=TIntegerList.Create;
  FVBOBuff:=nil;
end;

destructor TMultiObjectMesh.Destroy;
begin
  FreeObjectList(FMeshObjects);
  FMeshObjects.Free;
  FVertices.Free; FNormals.Free;
  FTexCoords.Free;FIndices.Free;
  if assigned(FVBOBuff) then FreeVBOBuffer(FVBOBuff^);
  inherited;
end;

function TMultiObjectMesh.getMeshObject(index: integer): TBuiltinMeshObject;
begin
  result:=FMeshObjects[index];
end;

function TMultiObjectMesh.getObjCount: integer;
begin
  result:=FMeshObjects.Count;
end;

{ TVBOIndiceAdapter }

procedure TVBOIndiceAdapter.AddNewMeshObject(var MOMesh: TMultiObjectMesh;
  VTNHashList: TList; Ind: TIntegerList; MName, GName: string);
var i, vOffs: integer;
    MeshObject: TBuiltinMeshObject;
    Pvhd,pv2,pv3: PVertexHashData;
    Nm: TAffineVector;
begin
  MeshObject:=TBuiltinMeshObject.Create(MOMesh);
  MOMesh.MeshObjectsList.Add(MeshObject);
  vOffs:=MOMesh.FVertices.Count;

  MOMesh.Vertices.Count:=vOffs+VTNHashList.Count;
  MOMesh.TexCoords.Count:=vOffs+VTNHashList.Count;
  MOMesh.Normals.Count:=vOffs+VTNHashList.Count;
  MOMesh.Indices.Count:=MOMesh.Indices.Count+ind.Count;
  MeshObject.FElementsCount:=ind.Count;
  MeshObject.FMaterialName:=MName;
  MeshObject.FGroupName:=GName;

  for i:=0 to ind.Count-1 do MeshObject.Indices[i]:=Ind[i]+vOffs;

  for i:=0 to VTNHashList.Count-1 do begin
    Pvhd:=VTNHashList[i]; Pvhd.Vertex.N:=NullVector;
  end;
  for i:=0 to (ind.Count div 3)-1 do begin
    Pvhd:=VTNHashList[Ind[i*3]];
    pv2:=VTNHashList[Ind[i*3+1]];
    pv3:=VTNHashList[Ind[i*3+2]];
    nm:=CalcPlaneNormal(Pvhd.Vertex.V,pv2.Vertex.V,pv3.Vertex.V);
    AddVector(Pvhd.Vertex.N,nm); AddVector(pv2.Vertex.N,nm);
    AddVector(pv3.Vertex.N,nm);
  end;

  for i:=0 to VTNHashList.Count-1 do begin
    Pvhd:=VTNHashList[i];
    MOMesh.Vertices[i+vOffs]:=Pvhd.Vertex.V;
    MOMesh.TexCoords[i+vOffs]:=Pvhd.Vertex.T;
    MOMesh.Normals[i+vOffs]:=VectorNormalize(Pvhd.Vertex.N);
  end;
end;

procedure TVBOIndiceAdapter.AttachFaceGroups(FG: TList);
begin
  FFaceGroups:=FG;
end;

procedure TVBOIndiceAdapter.AttachNormals(Norm: TAffineVectorList;
  Ind: TIntegerList);
begin
  include(FAttached, vaNormals); FNormals:=Norm; FNIndices:=Ind;
end;

procedure TVBOIndiceAdapter.AttachTexCoords(TexC: TAffineVectorList;
  Ind: TIntegerList);
begin
  include(FAttached, vaTexCoords); FTexCoords:=TexC; FTIndices:=Ind;
end;

procedure TVBOIndiceAdapter.AttachVertices(Vert: TAffineVectorList;
  Ind: TIntegerList);
begin
  include(FAttached, vaVertex); FVertices:=Vert; FVIndices:=Ind;
end;

procedure TVBOIndiceAdapter.BuildMultiObjectMesh(var MOMesh: TMultiObjectMesh);
var V,T,N: TAffineVectorList;
    FGs: TList;
    VertexHashTable: TVertexHashArray;
    FG1,FG2: PFaceGroup;
    i,j,k,c: integer;
    hCount: integer;
    MeshObjectIndices: TIntegerList;
    VertexList: TList;
    Buff: array[0..8] of single;
    sIndex: integer;
begin
  V:=TAffineVectorList.Create;
  T:=TAffineVectorList.Create;
  N:=TAffineVectorList.Create;
  FGs:=TList.Create;
  MeshObjectIndices:=TIntegerList.Create;
  VertexList:=TList.Create;
  MOMesh:=TMultiObjectMesh.Create;

  ExtractTriangles(V,FGs,T,N);
  for i:=0 to FGs.Count-1 do begin
    FG1:=FGs[i]; FG2:=FG1; hCount:=0;
    if assigned(FG1) then begin
      //sIndex:=Length(VertexHashTable);
      for k:=i to FGs.Count-1 do begin
        FG2:=FGs[k];
        if (FG1.GroupName=FG2.GroupName) and (FG1.MaterialName=FG2.MaterialName)
        then begin
          setlength(VertexHashTable,hCount+FG2.vCount);
          for j:=0 to FG2.vCount-1 do begin
            c:=j+FG1.vOffset;
            VertexHashTable[hCount+j].Vertex.V:=V[c];
            VertexHashTable[hCount+j].Vertex.T:=T[c];
            VertexHashTable[hCount+j].Vertex.N:=N[c];
            VertexHashTable[hCount+j].Vertex.SG:=FG1.SmoothingGroup;
            Buff[0]:=V[c][0]; Buff[1]:=V[c][1]; Buff[2]:=V[c][2];
            Buff[3]:=T[c][0]; Buff[4]:=T[c][1]; Buff[5]:=T[c][2];
            Buff[6]:=N[c][0]; Buff[7]:=N[c][1]; Buff[8]:=N[c][2];
            VertexHashTable[hCount+j].Hash:=BufferHash(buff,36);
//              VertexHash(VertexHashTable[hCount+j].Vertex,sizeof(TVertex));
            VertexHashTable[hCount+j].Summ:=
              V[c][0]+V[c][1]+V[c][2]+T[c][0]+T[c][1]+T[c][2]+
              N[c][0]+N[c][1]+N[c][2]+FG1.SmoothingGroup;
              VertexHashTable[hCount+j].Index:=c;
          end; FGs[k]:=nil; hCount:=hCount+FG2.vCount;
          if FG1<>FG2 then Dispose(FG2);
        end;
      end;
      //Indexing VertexHashTable
      VertexList.Clear; MeshObjectIndices.Clear;
      IndexingHT(VertexHashTable,VertexList,MeshObjectIndices);
      AddNewMeshObject(MOMesh,VertexList,MeshObjectIndices,FG1.MaterialName,FG1.GroupName);
      if assigned(FG1) then Dispose(FG1);
    end;
  end;
  V.Free; T.Free; N.Free; FGs.Free;
  MeshObjectIndices.Free; VertexList.Free;
end;

procedure TVBOIndiceAdapter.BuildVBOMeshList(var MeshList: TList);
var V,T,N: TAffineVectorList;
    tv,nv: TAffineVector;
    FGs: TList;
    VertexHashTable: TVertexHashArray;

    FG1: PFaceGroup;
    i,j,k,c: integer;
    hCount: integer;
    MeshObjectIndices: TIntegerList;
    VertexList: TList;
    Buff: array[0..8] of single;
    sIndex: integer;
    VBOBuff: PVBOBuffer;
    p: PVertexHashData;
begin
  V:=TAffineVectorList.Create;
  T:=TAffineVectorList.Create;
  N:=TAffineVectorList.Create;
  FGs:=TList.Create;
  MeshObjectIndices:=TIntegerList.Create;
  VertexList:=TList.Create;
  MeshList:=TList.Create;

  ExtractTriangles(V,FGs,T,N);
  MeshList.Count:=FGs.Count;
  for i:=0 to FGs.Count-1 do begin
    FG1:=FGs[i]; hCount:=0;
    if assigned(FG1) then begin
      hCount:=0; setlength(VertexHashTable,hCount+FG1.vCount);
      for j:=0 to FG1.vCount-1 do begin
        c:=j+FG1.vOffset;
        if vaTexCoords in FAttached then tv:=T[c] else tv:=NullVector;
        if vaNormals in FAttached then nv:=N[c] else nv:=NullVector;

        VertexHashTable[hCount+j].Vertex.V:=V[c];
        VertexHashTable[hCount+j].Vertex.T:=tv;
        VertexHashTable[hCount+j].Vertex.N:=nv;
        VertexHashTable[hCount+j].Vertex.SG:=FG1.SmoothingGroup;
        Buff[0]:=V[c][0]; Buff[1]:=V[c][1]; Buff[2]:=V[c][2];
        Buff[3]:=tv[0]; Buff[4]:=tv[1]; Buff[5]:=tv[2];
        Buff[6]:=nv[0]; Buff[7]:=nv[1]; Buff[8]:=nv[2];
        VertexHashTable[hCount+j].Hash:=BufferHash(buff,36);
        VertexHashTable[hCount+j].Summ:=
          V[c][0]+V[c][1]+V[c][2]+tv[0]+tv[1]+tv[2]+
          nv[0]+nv[1]+nv[2]+FG1.SmoothingGroup;
          VertexHashTable[hCount+j].Index:=c;
      end;

      //Indexing VertexHashTable
      VertexList.Clear; MeshObjectIndices.Clear;
      IndexingHT(VertexHashTable,VertexList,MeshObjectIndices);
      //Create and fill VBO Buffers
      new(VBOBuff); InitVBOBuff(VBOBuff^,4,DrawElements);
      VBOBuff.Indices.Capacity:=MeshObjectIndices.Count;//.Capacity;
      VBOBuff.Indices.Assign(MeshObjectIndices);
      VBOBuff.Vertexes.Count:=VertexList.Count;
      if vaTexCoords in FAttached then
        VBOBuff.TexCoords.Count:=VertexList.Count
      else VBOBuff.TexCoords.Count:=0;
      if vaNormals in FAttached then
        VBOBuff.Normals.Count:=VertexList.Count
      else VBOBuff.Normals.Count:=0;
      for k:=0 to VertexList.Count-1 do begin
        p:=VertexList[k];
        VBOBuff.Vertexes[k]:=p.Vertex.V;
        if vaTexCoords in FAttached then
          VBOBuff.TexCoords[k]:=p.Vertex.T;
        if vaNormals in FAttached then
          VBOBuff.Normals[k]:=p.Vertex.N;
      end;
      VBOBuff.RenderBuffs:=[uIndices];
      if vaTexCoords in FAttached then include(VBOBuff.RenderBuffs,uTexCoords);
      if vaNormals in FAttached then include(VBOBuff.RenderBuffs,uNormals);
      VBOBuff.MatName:=FG1.MaterialName;
      VBOBuff.FaceGroupeName:=FG1.GroupName;
      VBOBuff.Name:=FG1.GroupName;
      if not (vaNormals in FAttached) then
        RebuildNormals(VBOBuff);
      MeshList.List[i]:=VBOBuff;
      if assigned(FG1) then begin Dispose(FG1); FGs[i]:=nil; end;
    end;
  end;

  V.Free; T.Free; N.Free; FreeList(FGs);
  MeshObjectIndices.Free; VertexList.Free;
  VertexHashTable:=nil;
end;

constructor TVBOIndiceAdapter.Create;
begin
  FAttached:=[];
  FVertices:=nil; FNormals:=nil; FTexCoords:=nil;
  FVIndices:=nil; FNIndices:=nil; FTIndices:=nil;
  FFaceGroups:=nil;
  FCache:=TIntegerBits.Create; FCache.Size:=65536;
end;

destructor TVBOIndiceAdapter.Destroy;
begin
  FCache.Free;
  inherited;
end;

procedure TVBOIndiceAdapter.ExtractTriangles(var V: TAffineVectorList; var FaceGr: TList;
  var T, N: TAffineVectorList; MergeByMaterials: boolean);
var i,j,k,idx,fgCount: integer;
    FG, NextFG, NewFG, tFG: PFaceGroup;
    //vCount, tCount, nCount: integer;
    va: TAffineVector;
begin
  assert(vaVertex in FAttached, 'Vertex list is not attached');
  assert(assigned(FFaceGroups), 'FaceGroups is not attached');
  V.Clear; //MergeByMaterials:=false;
  if assigned(T) then T.Clear;
  if assigned(N) then N.Clear;
  for i:=0 to FFaceGroups.Count-1 do begin
    FG:=FFaceGroups[i]; FG.Processed:=false;
    if i+1<FFaceGroups.Count then begin
      NextFG:=FFaceGroups[i+1];
      FG.vCount:=NextFG.vOffset-FG.vOffset;
      if vaTexCoords in FAttached then
        FG.tCount:=NextFG.tOffset-FG.tOffset
      else FG.tCount:=0;
      if vaNormals in FAttached then
        FG.nCount:=NextFG.nOffset-FG.nOffset
      else FG.nCount:=0;
    end else begin
      FG.vCount:=FVIndices.Count-FG.vOffset;
      if vaTexCoords in FAttached then
        FG.tCount:=FTIndices.Count-FG.tOffset
      else FG.tCount:=0;
      if vaNormals in FAttached then
        FG.nCount:=FNIndices.Count-FG.nOffset
      else FG.nCount:=0;
    end;
  end;
  for i:=0 to FFaceGroups.Count-1 do begin
    FG:=FFaceGroups[i];
    if assigned(FG) and (not FG.Processed) then begin
      new(NewFG); NewFG.vCount:=0;
      NewFG.vOffset:=V.Count; NewFG.tOffset:=V.Count;
      NewFG.nOffset:=V.Count; NewFG.SmoothingGroup:=FG.SmoothingGroup;
      NewFG.GroupName:=CutString(FG.GroupName,80);
      NewFG.MaterialName:=CutString(FG.MaterialName,80);
      NewFG.MatNameHash:=StringHashKey(FG.MaterialName);
      FaceGr.Add(NewFG);
      if MergeByMaterials then fgCount:=FFaceGroups.Count-1 else fgCount:=i;
      for k:=i to fgCount do begin
        tFG:=FFaceGroups[k];
        if tFG.MatNameHash=FG.MatNameHash then begin
          NewFG.vCount:=NewFG.vCount+tFG.vCount;
          V.Capacity:=V.Count+tFG.vCount;
          if assigned(T) and (vaTexCoords in FAttached) then T.Capacity:=V.Count+tFG.vCount;
          if assigned(N) and (vaNormals in FAttached) then N.Capacity:=V.Count+tFG.vCount;
          for j:=0 to tFG.vCount-1 do begin
            va:=FVertices[FVIndices[tFG.vOffset+j]]; V.Add(va);
            if (vaTexCoords in FAttached) and (assigned(T)) then
              if (tFG.tCount=tFG.vCount) then begin
                idx:=FTIndices[tFG.tOffset+j];
                if idx<0 then va:=NullVector else va:=FTexCoords[idx]; T.Add(va);
              end else T.Add(NullVector);
            if (vaNormals in FAttached) and (assigned(N)) then
              if (tFG.nCount=tFG.vCount) then begin
                idx:=FNIndices[tFG.nOffset+j];
                if idx<0 then va:=NullVector else va:=FNormals[idx]; N.Add(va);
              end else N.Add(NullVector);
          end;
          tFG.Processed:=true; if tFG<>FG then NewFG.GroupName:=FG.MaterialName;
        end;
      end;
    end;
  end;
end;

procedure TVBOIndiceAdapter.IndexingHT(const HashTable: TVertexHashArray; Res: TList;
  Ind: TIntegerList; sIndex: integer);
var i,j,n: integer;
    p: PVertexHashData;
    f: boolean;
    rescount: integer;
    hash : array of Tlist;
    h: word;
begin
  FCache.ResetBits; n:=(length(HashTable) shr 16)+1;
  setlength(hash, 65536);
  for i:=0 to high(hash) do begin
    hash[i]:=TList.Create;
    hash[i].Clear;
    hash[i].Capacity:=n;
  end;
  ind.Count:=length(HashTable)-sIndex; Ind[0]:=0;
  Res.Count:=ind.Count;
  Res[0]:=@HashTable[sIndex]; rescount:=1; n:=1;
  FCache.Bits[HashTable[sIndex].Hash]:=true;
  HashTable[sIndex].NewIndex:=0;
  hash[HashTable[sIndex].Hash].Add(@HashTable[sIndex]);
  for i:=sIndex+1 to high(HashTable) do begin
    h:=HashTable[i].Hash; f:=false;
    if FCache.Bits[h] then begin
      for j:=0 to hash[h].Count-1 do begin
        p:=hash[h].List[j];
        if p.Summ=HashTable[i].Summ then begin
          Ind.List[n]:=p.NewIndex; f:=true; Break;
        end;
      end;
    end;
    if not f then begin
      Ind.List[n]:=rescount; Res.List[rescount]:=@HashTable[i];
      hash[h].Add(@HashTable[i]); HashTable[i].NewIndex:=rescount;
      inc(rescount); FCache.Bits[h]:=true;
    end; inc(n);

{    j:=rescount-1; f:=false; p:=Res.List[0];
    if FCache.Bits[HashTable[i].Hash] then
      repeat
        p:=Res.List[j];
        if (p.Hash=HashTable[i].Hash) and (p.Summ=HashTable[i].Summ)
        then begin f:=true; break; end else dec(j);
      until (j<0);
    if not f then begin
      Ind.List[n]:=rescount; Res.List[rescount]:=@HashTable[i];
      inc(rescount); FCache.Bits[HashTable[i].Hash]:=true;
    end else Ind.List[n]:=j; inc(n);}
  end; Res.Count:=rescount; ind.Count:=n;
  for i:=0 to high(hash) do hash[i].Free;
  hash:=nil;
end;


procedure TVBOIndiceAdapter.QuickSort(var A: TVertexHashArray);
  procedure Sort(var A: TVertexHashArray; iLo, iHi: Integer);
  var Lo, Hi: Integer;
      Mid, T: TVertexHashData;
  begin
    Lo := iLo; Hi := iHi;
    Mid := A[(Lo + Hi) div 2];
    repeat
      while A[Lo].Hash < Mid.Hash do Inc(Lo);
      while A[Hi].Hash > Mid.Hash do Dec(Hi);
      if Lo <= Hi then
      begin
        T := A[Lo]; A[Lo] := A[Hi]; A[Hi] := T;
        Inc(Lo); Dec(Hi);
      end;
    until Lo > Hi;
    if Hi > iLo then Sort(A, iLo, Hi);
    if Lo < iHi then Sort(A, Lo, iHi);
  end;
begin
  Sort(A, Low(A), High(A));
end;

{ TVectorAttribList }

procedure TVectorAttribList.Add(v: TVector2f);
begin
  Add(v[0],v[1]);
end;

procedure TVectorAttribList.Add(v1, v2: TVector2f);
begin
  Add(v1);Add(v2);
end;

procedure TVectorAttribList.Add(v1, v2, v3: TVector2f);
begin
  Add(v1);Add(v2);Add(v3);
end;

procedure TVectorAttribList.Add(v1, v2, v3, v4: TVector2f);
begin
  Add(v1);Add(v2);Add(v3);Add(v4);
end;

procedure TVectorAttribList.Add(x: single);
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
  FList[FLastIndex]:=x; inc(FCount);
end;

procedure TVectorAttribList.Add(x, y: single);
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
  assert(FVectorType=vtDouble,'Invalid type of vector, expected vtDouble');
  if FCapacity<=FCount then GrowList;
  FList[FLastIndex+1]:=x;
  FList[FLastIndex+2]:=y;
  inc(FCount); inc(FLastIndex,2);
end;

procedure TVectorAttribList.Add(x, y, z: single);
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
  assert(FVectorType=vtVector,'Invalid type of vector, expected vtVector');
  if FCapacity<=FCount then GrowList;
  FList[FLastIndex+1]:=x;
  FList[FLastIndex+2]:=y;
  FList[FLastIndex+3]:=z;
  inc(FCount); inc(FLastIndex,3);
end;

procedure TVectorAttribList.Add(x, y, z, w: single);
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
  assert(FVectorType=vtPoint,'Invalid type of vector, expected vtPoint');
  if FCapacity<=FCount then GrowList;
  FList[FLastIndex+1]:=x;
  FList[FLastIndex+2]:=y;
  FList[FLastIndex+3]:=z;
  FList[FLastIndex+4]:=w;
  inc(FCount); inc(FLastIndex,4);
end;

procedure TVectorAttribList.Add(v: TVector3f);
begin
  Add(v[0],v[1],v[2]);
end;

procedure TVectorAttribList.Add(v1, v2: TVector3f);
begin
  Add(v1);Add(v2);
end;

procedure TVectorAttribList.Add(v1, v2, v3, v4: TVector3f);
begin
  Add(v1);Add(v2);Add(v3);Add(v4);
end;

procedure TVectorAttribList.Add(v1, v2, v3: TVector3f);
begin
  Add(v1);Add(v2);Add(v3);
end;

procedure TVectorAttribList.Add(v: TVector4f);
begin
  Add(v[0],v[1],v[2],v[3]);
end;

procedure TVectorAttribList.Add(v1, v2: TVector4f);
begin
  Add(v1);Add(v2);
end;

procedure TVectorAttribList.Add(v1, v2, v3: TVector4f);
begin
  Add(v1);Add(v2);Add(v3);
end;

procedure TVectorAttribList.Add(v1, v2, v3, v4: TVector4f);
begin
  Add(v1);Add(v2);Add(v3);Add(v4);
end;

procedure TVectorAttribList.Clear;
begin
  FGrowCount:=1; FCapacity:=1;
  setlength(FList,FCapacity*CVectorSize[FVectorType]);
  FLastIndex:=-1;   FCount:=0;
  SetVector(Fmin,0,0,0);
  SetVector(Fmax,0,0,0);
end;

constructor TVectorAttribList.Create(VectorType: TVectorType);
begin
  inherited Create;
  FVectorType:=VectorType;
  FGrowCount:=1; FCapacity:=1;
  setlength(FList,FCapacity*CVectorSize[FVectorType]);
  FLastIndex:=-1;   FCount:=0;
  SetVector(Fmin,0,0,0);
  SetVector(Fmax,0,0,0);
end;

destructor TVectorAttribList.Destroy;
begin
  FList:=nil;
  inherited;
end;

function TVectorAttribList.GetData: pointer;
begin
  result:=@FList[0];
end;

function TVectorAttribList.getExtents: TExtents;
begin
  result.emin:=FMin;
  result.emax:=FMax;
end;

function TVectorAttribList.GetSingle(Index: integer): single;
begin
  result:=FList[Index];
end;

function TVectorAttribList.GetSize: integer;
begin
  result:=FCount*4*CVectorSize[FVectorType];
end;

function TVectorAttribList.GetVector2f(Index: integer): TVector2f;
begin
  result[0]:=FList[Index*2];
  result[1]:=FList[Index*2+1];
end;

function TVectorAttribList.GetVector3f(Index: integer): TVector3f;
begin
  result[0]:=FList[Index*3];
  result[1]:=FList[Index*3+1];
  result[2]:=FList[Index*3+2];
end;

function TVectorAttribList.GetVector4f(Index: integer): TVector4f;
begin
  result[0]:=FList[Index*4];
  result[1]:=FList[Index*4+1];
  result[2]:=FList[Index*4+2];
  result[3]:=FList[Index*4+3];
end;

procedure TVectorAttribList.GrowList;
begin
  FCapacity:=FCapacity+FGrowCount;
  FGrowCount:=FCapacity;
  setlength(FList,FCapacity*CVectorSize[FVectorType]);
end;

procedure TVectorAttribList.Add(m: TMatrix3f);
begin
  Add(m[0],m[1],m[2]);
end;

procedure TVectorAttribList.Add(m: TMatrix4f);
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
  FGrowCount:=1;
  FCapacity:=FGrowCount;
  setlength(FList,FCapacity);
  FLastIndex:=-1;   FCount:=0;
  FMin:=0; FMax:=0;
end;

destructor TIntegerAttribList.Destroy;
begin
  FList:=nil;
  inherited;
end;

function TIntegerAttribList.GetData: pointer;
begin
  result:=@FList[0];
end;

function TIntegerAttribList.GetSize: integer;
begin
  result:=FCount*4;
end;

procedure TIntegerAttribList.GrowList;
begin
  FCapacity:=FCapacity+FGrowCount;
  FGrowCount:=FCapacity;
  setlength(FList,FCapacity);
end;


function LookAt(const eyePosition3D, center3D, upVector3D: TAffineVector): TMatrix;
var dir, side, up: TAffineVector;
begin
   //------------------
   dir[0] := center3D[0] - eyePosition3D[0];
   dir[1] := center3D[1] - eyePosition3D[1];
   dir[2] := center3D[2] - eyePosition3D[2];
   NormalizeVector(dir);
   //------------------
   //Side = dir x up
   side:=VectorCrossProduct(dir, upVector3D);
   NormalizeVector(side);
   //------------------
   //Recompute up as: up = side x dir
   up:=VectorCrossProduct(side, dir);
   //------------------
   result[0]:=vectormake(side,0.0);
   result[1]:=vectormake(up,0.0);
   result[2]:=vectormake(VectorNegate(dir),0.0);
   result[3]:=VectorMake(-eyePosition3D[0], -eyePosition3D[1], -eyePosition3D[2], 1);
end;

function QuaternionRotate(const Q: TQuaternion; const P: TAffineVector): TAffineVector; overload;
var t,v: TAffineVector;
begin
  t:=VectorCrossProduct(q.ImagPart,p);
  v:=VectorScale(p,q.RealPart); AddVector(t,v);
  t:=VectorCrossProduct(q.ImagPart,t); ScaleVector(t,2);
  result:=VectorAdd(p,t);
end;

function QuaternionRotate(const Q: TVector; const P: TAffineVector): TAffineVector; overload;
var t,v,a: TAffineVector;
begin
  a:=AffineVectorMake(Q);
  t:=VectorCrossProduct(a,p);
  v:=VectorScale(p,q[3]); AddVector(t,v);
  t:=VectorCrossProduct(a,t); ScaleVector(t,2);
  result:=VectorAdd(p,t);
end;

function QuatToVector(const Q: TQuaternion): TVector;
begin
  result:=VectorMake(q.ImagPart,q.RealPart);
end;

function VectorToQuat(const Q: TVector): TQuaternion;
begin
  result.ImagPart:=affineVectorMake(Q);
  result.RealPart:=Q[3];
end;

{ TInterleavedBuffer }

function TInterleavedBuffer.AddFloatValue(Value: single): integer;
begin
  FList.Add(Value);
end;

function TInterleavedBuffer.AddVertex: integer;
var i,n: integer;
begin
  if not FStructureLocked then begin
    FRecordSize:=0;
    for i:=0 to high(FDescriptors) do
      FRecordSize:=FRecordSize+FDescriptors[i].size;
    for i:=0 to high(FDescriptors) do
      FDescriptors[i].stride:=FRecordSize*4;
    FStructureLocked:=true;
  end;
  inc(FCount); FBaseIndex:=FList.Count;
  FList.Count:=FList.Count+FRecordSize;
  result:=FBaseIndex;
end;

procedure TInterleavedBuffer.AttributeValue(DescrIndex: byte; s,
  t: single);
var n: integer;
begin
  assert(DescrIndex<=high(FDescriptors),'Descriptor index out of range.');
  n:=GetAttribPos(DescrIndex);
  FList[n]:=s; FList[n+1]:=t;
end;

procedure TInterleavedBuffer.AttributeValue(DescrIndex: byte; x, y,
  z: single);
var n: integer;
begin
  assert(DescrIndex<=high(FDescriptors),'Descriptor index out of range.');
  n:=GetAttribPos(DescrIndex);
  FList[n]:=x; FList[n+1]:=y; FList[n+2]:=z;
end;

procedure TInterleavedBuffer.AttributeValue(DescrIndex: byte; r, g, b,
  a: single);
var n: integer;
begin
  assert(DescrIndex<=high(FDescriptors),'Descriptor index out of range.');
  n:=GetAttribPos(DescrIndex);
  FList[n]:=r; FList[n+1]:=g; FList[n+2]:=b; FList[n+3]:=a;
end;

procedure TInterleavedBuffer.BindToVBO(var VBO: TVBOBuffer);
var i: integer;
    texpos: cardinal;
begin
  for i:=0 to high(FDescriptors) do begin
    with FDescriptors[i] do
    case vaType of
      atVertex: begin
        glEnableClientState(GL_VERTEX_ARRAY);
        glVertexPointer( size, GL_FLOAT, stride, pointer(offset*4));
      end;
      atNormal: begin
        if uNormals in VBO.RenderBuffs then begin
          glEnableClientState(GL_NORMAL_ARRAY);
          glNormalPointer( GL_FLOAT, stride, pointer(offset*4));
        end;
      end;
      atColor: begin
        if uColors in VBO.RenderBuffs then begin
          glEnableClientState(GL_COLOR_ARRAY);
          glEnable(GL_COLOR_MATERIAL);
          glColorPointer( size, GL_FLOAT, stride, pointer(offset*4));
        end;
      end;
      atTexCoord: begin
        if uTexCoords in VBO.RenderBuffs then begin
          glClientActiveTexture(GL_TEXTURE0);
          glEnableClientState(GL_TEXTURE_COORD_ARRAY);
          glTexCoordPointer( size, GL_FLOAT, stride, pointer(offset*4));
        end;
      end;
      atTexCoord0..atTexCoord7: begin
        if uMultitexture in VBO.RenderBuffs then begin
          texpos:=byte(vaType)-byte(atTexCoord0)+GL_TEXTURE0;
          glClientActiveTexture(texpos);
          glEnableClientState(GL_TEXTURE_COORD_ARRAY);
          glTexCoordPointer( size, GL_FLOAT, stride, pointer(offset*4));
        end;
      end;
    end;
  end;
end;

procedure TInterleavedBuffer.BuildVBO(var VBO: TVBOBuffer);
var attr: PVBOAttribute;
    i:integer;
begin
  vbo.Struct:=bsInterleaved;
  new(attr); vbo.AttribList.Add(attr);
  attr.Id:=0; attr.Data:=FList.List;
  attr.Size:=FList.DataSize;
  attr.CCount:=1;
  attr.CSize:=FRecordSize;
  attr.CType:=GL_FLOAT;
  attr.Name:='Interleaved buffer';
  attr.DataHandler:=self;
  Attr.AttrType:=atInterleaved;
  attr.Location:=-1;
  attr.tag:='';
  GenVBOBuff(VBO,false);
  for i:=0 to high(FDescriptors) do begin
    case FDescriptors[i].vaType of
      atNormal: Include(VBO.RenderBuffs,uNormals);
      atColor: Include(VBO.RenderBuffs,uColors);
      atTexCoord: if not (uMultitexture in VBO.RenderBuffs)
                  then Include(VBO.RenderBuffs,uTexCoords);
      atTexCoord0..atTexCoord7: begin
        exclude(VBO.RenderBuffs,uTexCoords);
        Include(VBO.RenderBuffs,uMultitexture);
      end;
    end;
  end;
end;

procedure TInterleavedBuffer.Clear;
begin
  FList.Clear;
end;

constructor TInterleavedBuffer.Create;
begin
  inherited;
  FList:=TSingleList.Create;
  FCount:=0; FBaseIndex:=0;
  FOffset:=0;
  FStructureLocked:=false;
  FRecordSize:=-1;
end;

function TInterleavedBuffer.CreateDescriptor(aSize: cardinal;
  aType: TAttribType; aName: string): integer;
var i: integer;
begin
  assert(FStructureLocked=false,'You can''t add new Descriptor after adding records.');
  setlength(FDescriptors,length(FDescriptors)+1);
  i:=high(FDescriptors); result:=i;
  with FDescriptors[i] do begin
    Offset:=FOffset;
    Size:=aSize; Stride:=0;
    FOffset:=FOffset+size;
    vaType:=aType;
    if aName<>'' then Name:=aName else begin
       if aType in [atVertex..atColor] then
         Name:=CDefAttribName[aType]
       else Name:='';
    end;
  end;
end;

destructor TInterleavedBuffer.Destroy;
begin
  FList.Free;
  inherited;
end;

function TInterleavedBuffer.GetAttribPos(DescrIndex: byte): integer;
begin
  result:=FBaseIndex+FDescriptors[DescrIndex].offset;
end;

function TInterleavedBuffer.getBuffData: pointer;
begin
  result:=FList.List;
end;

function TInterleavedBuffer.getDescr(Index: byte): TInterleavedAttrib;
begin
  result:=FDescriptors[Index];
end;

procedure TInterleavedBuffer.LockBuffer;
var i: integer;
begin
  assert(length(FDescriptors)>0,'You need to describe attributes first.');
  if not FStructureLocked then begin
    FRecordSize:=0;
    for i:=0 to high(FDescriptors) do
      FRecordSize:=FRecordSize+FDescriptors[i].size;
    for i:=0 to high(FDescriptors) do
      FDescriptors[i].stride:=FRecordSize*4;
    FStructureLocked:=true;
  end;
end;

procedure TInterleavedBuffer.SetDescr(Index: byte;
  const Value: TInterleavedAttrib);
begin
  FDescriptors[Index]:=Value;
end;

procedure TInterleavedBuffer.UnBindVBO(var VBO: TVBOBuffer);
var i: integer;
    texpos: cardinal;
begin
  for i:=0 to high(FDescriptors) do begin
    with FDescriptors[i] do
    case vaType of
      atVertex: glDisableClientState(GL_VERTEX_ARRAY);
      atNormal: if uNormals in VBO.RenderBuffs then
          glDisableClientState(GL_NORMAL_ARRAY);
      atColor:
        if uColors in VBO.RenderBuffs then begin
          glDisableClientState(GL_COLOR_ARRAY);
          glDisable(GL_COLOR_MATERIAL);
        end;
      atTexCoord: begin
        if uTexCoords in VBO.RenderBuffs then begin
          glClientActiveTexture(GL_TEXTURE0);
          glDisableClientState(GL_TEXTURE_COORD_ARRAY);
        end;
      end;
      atTexCoord0..atTexCoord7: begin
        if uMultitexture in VBO.RenderBuffs then begin
          texpos:=byte(vaType)-byte(atTexCoord0)+GL_TEXTURE0;
          glClientActiveTexture(texpos);
          glDisableClientState(GL_TEXTURE_COORD_ARRAY);
        end;
      end;
    end;
  end;
end;

procedure TInterleavedBuffer.AttributeValue(DescrIndex: byte; v: TAffineVector);
begin
  AttributeValue(DescrIndex,v[0],v[1],v[2]);
end;

procedure TInterleavedBuffer.AttributeValue(DescrIndex: byte; st: TTexPoint);
begin
  AttributeValue(DescrIndex,st.S,st.T);
end;

procedure TInterleavedBuffer.AttributeValue(DescrIndex: byte; v: TVector);
begin
  AttributeValue(DescrIndex,v[0],v[1],v[2],v[3]);
end;

initialization
  vCubicOccluder:=CreateCubicOccluder(false);

finalization
  FreeVBOBuffer(vCubicOccluder^);
  dispose(vCubicOccluder);
end.

