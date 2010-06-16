unit uVBO;

interface
uses VectorTypes, VectorGeometry, VectorLists, OpenGL1x, Classes, MeshUtils, Types;

type
  TVBORenderType = (DrawArrays, DrawElements, DrawRangedElements, DrawMultiElements);
  TRenderBuff = set of (uNormals, uTexCoords, uIndices, uSecondaryTexure,
                        uColors, uMultitexture, uExTexEnvMode);
  TBindState = set of (sActivate, sDeactivate, sUnchanged, sCheck);
  TUpdateBuff = set of (upVertex, upColor, upNormal, upTexCoord, upSecTexCoord);

  PVBOBuffer = ^TVBOBuffer;
  TVBOBuffer = packed record
    Visible: boolean;
    Name: string;
    RenderBuffs: TRenderBuff;
    Vertexes: TAffineVectorList;
    Normals: TAffineVectorList;
    TexCoords: TAffineVectorList;
    Colors: TVectorList;
    Indices: TintegerList;
    UseTwoTexturesCoord: boolean;
    ExTexCoords: TList;
    ExTexCoordsId: TIntegerList;
    ExTexEnvMode: TIntegerList;

    VertexCount: integer;
    MaxElements: integer;
    ElementsCount: integer;
    FaceType: GLUInt; {GL_TRIANGLES, GL_TRIANGLE_STRIP, GL_QUADS...}
    RenderType: TVBORenderType;
    vId, nId, tId, iId, stId, cId: GLUint;
    vccount, nccount: integer; //coponents count
    emin, emax: TAffineVector;
    WorldMatrix: TMatrix;
    Builded: boolean;
    idxBuffApply: boolean;
    idxBindOnce: boolean;
    ChildBuff: boolean;
    MatName: string[80];
    TextureHandle: GLUInt;
    ShaderProg: GLUInt;
    Cleared: boolean;
    PrevLOD, NextLOD: PVBOBuffer;
    LODLevel:integer;
    AdditionalInfo:pointer;
    ccount: integer;
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
  PExtents = ^TExtents;
  TExtents = record
     emin,emax:TAffineVector;
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

procedure InitVBOBuff(var VBuff: TVBOBuffer; FType: GLUInt; RType: TVBORenderType);
procedure GenVBOBuff(var VBuff: TVBOBuffer; FreeBuffMem: boolean = true);
procedure GenVBOPackBuff(var VBuff: TPackedVBO; FreeBuffMem: boolean = true);
Procedure RebuildVBOBuff(var VBuff: TVBOBuffer; FreeBuffMem: boolean = true);
Procedure UpdateVBOBuff(BuffId:integer; data: pointer; offset, size:integer; MapBuffer:Boolean=false);

function  GenIndices(var Indices: TIntegerList): GLUInt;
function  GenSingleVBOBuffer(var List: TAffineVectorList; target: GLUInt = GL_STATIC_DRAW): GLUInt; overload;
function  GenSingleVBOBuffer(var List: TVectorList; target: GLUInt = GL_STATIC_DRAW): GLUInt; overload;
Procedure SaveVBOBuff(var Buff:TVBOBuffer;FileName:string);overload;
Procedure SaveVBOBuff(Const Buff:TVBOBuffer;FS:TStream);overload;
Procedure LoadVBOBuff(var Buff:TVBOBuffer;FileName:string);overload;
Procedure LoadVBOBuff(var Buff:TVBOBuffer;FS:TStream);overload;
procedure RenderVBOBuffer(var VBuff: TVBOBuffer);
procedure RenderVBOMultiBuffer(var Buffs: TMultiBuffer; SingleCall:boolean=true);
procedure RenderVBOMultiList(RenderList: TList; SingleCall:boolean=true);
procedure RenderVBOMultiPart(Description: PMultiRenderDescr; BindBuffer:TBindState; SingleCall:boolean=true);
procedure RenderVBOList(List: TList);
function  RenderVBOListWithCulling(List: TList; WorldMatrix: TMatrix; bounds: boolean = false): integer;
procedure RenderVBOPackBuff(var VBuff: TPackedVBO);

procedure FreeVBOBuffer(var VBuff: TVBOBuffer; ClearBuffs: boolean = true);
procedure FreeVBOMem(var VBuff: TVBOBuffer);
procedure FreeVBOList(var List:TList;ClearBuffs: boolean = true);

procedure AttachBuffer(var FromBuff, ToBuff: TVBOBuffer; AttachIndice: boolean = true);
procedure AttachBufferInPos(var FromBuff, ToBuff: TVBOBuffer; vPos,iPos:integer);
procedure PackListIntoBuffer(List:TList; var Buffs:TMultiBuffer; FreeRAM:boolean);overload;
procedure ListToMultiBuffer(var List:TList; var Buffs:TMultiPackBuff;
                            RenderList:TList; FreeRAM:boolean=false; size:integer=-1);
function  CreatePlane(Width, Height: single; TilesX, TilesY: integer;AsLine:boolean=false;GenBuff:Boolean=true): PVBOBuffer;
procedure SortListByMaterial(List:TList);

function GetMaxIndicesCount: GLUInt;
function GetMaxVertexCount: GLUInt;
function GetFrustum: TFrustum;
procedure OptimizeIndeces(CacheSize: integer; BuffSize: integer; var Buff: TVBOBuffer; var BuffList: TList; GenBuff: boolean = true);
procedure ExtractTriangles(var Buff: TVBOBuffer; var Triangles: TAffineVectorList);
//Procedure CreateInterleavedArray(var VBuff:TVBOBuffer;var IBuff:TInterleavedBuff;FreeBuffMem:boolean=true);

procedure RenderBounds(emin, emax: TAffinevector; w, r, g, b: single);
function isVolumeClipped(emin, emax: TAffineVector): boolean; overload;
function isVolumeClipped(const Extents: TExtents; const Frustum:TFrustum): boolean;overload;

procedure Col2RowMatrix(mm: TMatrix; var m: TMatrix);
function CreateViewMatrix(const ModelMatrix: TMatrix): TMatrix;
Function GetViewPort:TVector4i;
function GetViewMatrix:TMatrix;
function GetProjectionMatrix:TMatrix;
function ProjectPoint(aPoint: TVector;
	      const modelMatrix: TMatrix;
	      const projMatrix:TMatrix;
              const viewport: array of integer;
              var ScreenPos:TVector): boolean;
function GetModelViewMatrix(WorldMatrix, ViewMatrix: TMatrix): PGLFloat;
function GetMinExtents(v1, v2: TAffineVector): TAffinevector;
function GetMaxExtents(v1, v2: TAffineVector): TAffinevector;
function GetExtentsOfList(List:TList):TExtents;

implementation

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
    P:=List[0]; emin:=P.emin; emax:=p.emax;
    for i:=1 to List.Count-1 do begin
       P:=List[i];
       emin:=GetMinExtents(emin,P.emin);
       emax:=GetMaxExtents(emax,P.emax);
    end;
  end;
end;

function GetMaxIndicesCount: GLUInt;
begin
  glGetintegerv(GL_MAX_ELEMENTS_INDICES, @result);
end;
function GetMaxVertexCount: GLUInt;
begin
  glGetintegerv(GL_MAX_ELEMENTS_VERTICES, @result);
end;

function GetFrustum: TFrustum;
var projMat, mvMat, MVProj: TMatrix;
begin
  glGetFloatv(GL_PROJECTION_MATRIX, @projMat);
  glGetFloatv(GL_MODELVIEW_MATRIX, @mvMat);
  MVProj := MatrixMultiply(mvMat, projMat);
  Result := ExtractFrustumFromModelViewProjection(MVProj);
end;

function isVolumeClipped(emin, emax: TAffineVector): boolean;overload;
var F: TFrustum;
  objPos: TAffineVector;
  Radius: single;
begin
  F := GetFrustum;
  objPos := VectorScale(VectorAdd(emin, emax), 0.5);
  Radius := VectorDistance(emin, emax) * 0.5;
  Result := isVolumeClipped(objPos, Radius, F);
end;

function isVolumeClipped(const Extents: TExtents; const Frustum:TFrustum): boolean;overload;
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

procedure InitVBOBuff;
begin
  with VBuff do begin
    Vertexes := TAffineVectorList.Create;
    Normals := TAffineVectorList.Create;
    TexCoords := TAffineVectorList.Create;
    Colors := TVectorList.Create;
    Indices := TintegerList.Create;
    FaceType := FType;
    RenderType := RType;
    Builded := false;
    RenderBuffs := [];
    vId := 0; nId := 0; tId := 0; iId := 0; stId := 0;
    vccount:=0; nccount:=0;
    WorldMatrix := IdentityHmgMatrix;
    VertexCount := 0;
    idxBuffApply := false;
    idxBindOnce := false;
    ChildBuff := false;
    UseTwoTexturesCoord := True;
    ExTexCoords := TList.Create;
    ExTexCoordsId := TIntegerList.Create;
    ExTexEnvMode := TIntegerList.Create;
    Visible := true;
  end;
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


procedure GenVBOBuff(var VBuff: TVBOBuffer; FreeBuffMem: boolean = true);
var i: integer;
    temp: TAffineVectorList;
begin
{  if VBuff.Indices.count=0 then begin
     VBuff.Builded := false;
     exit;
  end;}
  with VBuff do begin
    Vertexes.GetExtents(emin, emax);
    vId := GenSingleVBOBuffer(Vertexes);
    nId := GenSingleVBOBuffer(Normals);
    tId := GenSingleVBOBuffer(TexCoords);
    cId := GenSingleVBOBuffer(Colors);
    iId := GenIndices(Indices);
    if ExTexCoords.Count>0 then begin
      for i := 0 to ExTexCoords.Count - 1 do begin
          temp:=ExTexCoords[i];
          ExTexCoordsId.Add(GenSingleVBOBuffer(temp));
      end;
      stId:=ExTexCoordsId[0];
    end;

    RenderBuffs:=[];
    if nId > 0 then RenderBuffs := RenderBuffs + [uNormals];
    if tId > 0 then RenderBuffs := RenderBuffs + [uTexCoords];
    if iId > 0 then RenderBuffs := RenderBuffs + [uIndices];
    if cId > 0 then RenderBuffs := RenderBuffs + [uColors];
    //assert(iId>0,'Indices not found');
    if iId > 0 then ElementsCount := Indices.Count
    else ElementsCount := Vertexes.Count;
    if iId=0 then RenderType:=DrawArrays;

    MaxElements := ElementsCount;
    VertexCount := Vertexes.Count;
    if FreeBuffMem then begin
      Vertexes.Clear; Normals.Clear; TexCoords.Clear;Colors.Clear;
      for i := 0 to ExTexCoords.Count - 1 do begin
          temp:=ExTexCoords[i]; temp.Clear;
      end;
      Cleared := true
    end else Cleared := false;
    Builded := true;
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
    temp: TAffineVectorList;
begin
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  with VBuff do begin
    Builded := false;idxBuffApply := false;
    if vId <> 0 then glDeleteBuffers(1, @vId);
    if nId <> 0 then glDeleteBuffers(1, @nId);
    if tId <> 0 then glDeleteBuffers(1, @tId);
    if iId <> 0 then glDeleteBuffers(1, @iId);
    if cId <> 0 then glDeleteBuffers(1, @cId);
    if ClearBuffs then begin
      FreeAndNil(Vertexes);
      FreeAndNil(Normals);
      FreeAndNil(TexCoords);
      FreeAndNil(Indices);
      FreeAndNil(Colors);
      for i:= 0 to ExTexCoords.Count - 1 do begin
        temp:=ExTexCoords[i]; FreeAndNil(temp);
      end;
      FreeAndNil(ExTexCoords);
      FreeAndNil(ExTexCoordsId);
      FreeAndNil(ExTexEnvMode);
      Cleared:=true;
    end;
  end;
end;

procedure FreeVBOMem(var VBuff: TVBOBuffer);
var i:integer;
    temp: TAffineVectorList;
begin
  with VBuff do begin
     if assigned(Vertexes) then begin
        Vertexes.Clear; Vertexes.Free;
     end;
     if assigned(Normals) then begin
        Normals.Clear; Normals.Free;
     end;
     if assigned(TexCoords) then begin
        TexCoords.Clear; TexCoords.Free;
     end;
     if assigned(Indices) then begin
        Indices.Clear; Indices.Free;
     end;
     if assigned(Colors) then begin
        Colors.Clear; Colors.Free;
     end;
     if assigned(ExTexCoords) then begin
        for i:= 0 to ExTexCoords.Count - 1 do begin
          temp:=ExTexCoords[i]; FreeAndNil(temp);
        end;
     end;
     FreeAndNil(ExTexCoords);
     FreeAndNil(ExTexCoordsId);
     FreeAndNil(ExTexEnvMode);
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

procedure ListToMultiBuffer(var List:TList; var Buffs:TMultiPackBuff;
                            RenderList:TList; FreeRAM:boolean; size:integer);
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
     ELSE mi:=size;
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

Function CreatePlane(Width, Height: single; TilesX, TilesY:
  integer;AsLine:boolean=false;GenBuff:Boolean=true): PVBOBuffer;
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
        H := 0; SetVector(v, j*kx-width/2, H, i*ky-height/2);
        Vertexes.Add(v);
        if uTexCoords in RenderBuffs then begin
          v[0] := j*kx / (width);
          v[1] := 1 - i*ky / height;
          v[2] := 0; TexCoords.Add(v);
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
      for i := 0 to Indices.Count - 1 do begin
        v1 := Vertexes[Indices[i]];
        if i<=Indices.Count-3 then begin
          v2 := Vertexes[Indices[i + 1]];
          v3 := Vertexes[Indices[i + 2]];
        end else begin
          v3 := Vertexes[Indices[i - 1]];
          v2 := Vertexes[Indices[i - 2]];
        end;

        if (not VectorEquals(v2,v3)) and (not VectorEquals(v2,v3))
           and (not VectorEquals(v1,v3)) then begin
           nm := CalcPlaneNormal(v1, v2, v3);
           if odd(i) then NegateVector(nm);
           Normals[Indices[i]] := nm;
        end else begin
          v2 := Vertexes[Indices[i - 1]];
          v3 := Vertexes[Indices[i - 2]];
          nm := CalcPlaneNormal(v1, v2, v3);
          NegateVector(nm);
          Normals[Indices[i]] := nm;
        end;
      end;
    end;
    Vertexes.GetExtents(emin, emax);
  end;
  if GenBuff then GenVBOBuff(Temp^, False);
  Temp.MatName:=''; Result:=Temp;
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

procedure RenderVBOBuffer(var VBuff: TVBOBuffer);
Var i,RCount:integer;
begin
  with VBuff do begin
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

    if (stId <> 0) and (uSecondaryTexure in RenderBuffs) and (not (uMultitexture in RenderBuffs))
    then begin
      glClientActiveTexture(GL_TEXTURE1);
      glEnableClientState(GL_TEXTURE_COORD_ARRAY);
      glBindBuffer(GL_ARRAY_BUFFER, stId);
      glTexCoordPointer(3, GL_FLOAT, 0, nil);
    end else if (uMultitexture in RenderBuffs) then begin
      for i:= 0 to ExTexCoordsId.Count - 1 do begin
          glClientActiveTexture(GL_TEXTURE1+i);
          glEnableClientState(GL_TEXTURE_COORD_ARRAY);
          glBindBuffer(GL_ARRAY_BUFFER, ExTexCoordsId[i]);
          glTexCoordPointer(3, GL_FLOAT, SizeOf(TAffineVector), nil);
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
//      glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
    end;

    glBindBuffer(GL_ARRAY_BUFFER, vId);
    glVertexPointer(3, GL_FLOAT, 0, nil);

    if idxBindOnce then begin
      if (not idxBuffApply) and ((iId <> 0) and (uIndices in RenderBuffs)) then begin
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, iId);
        idxBuffApply := true; end;
    end else begin
      if ((iId <> 0) and (uIndices in RenderBuffs)) then
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, iId);
    end;
    if ElementsCount>MaxElements then RCount:=MaxElements else
    RCount:=ElementsCount;
    case RenderType of
      DrawArrays: glDrawArrays(FaceType, 0, RCount);
      DrawElements: glDrawElements(FaceType, RCount, GL_UNSIGNED_INT, nil);
    end;
    glDisableClientState(GL_VERTEX_ARRAY);
    if nId <> 0 then glDisableClientState(GL_NORMAL_ARRAY);
    if tId <> 0 then glDisableClientState(GL_TEXTURE_COORD_ARRAY);
    if cId <> 0 then begin
       glDisableClientState(GL_COLOR_ARRAY);
       glDisable(GL_COLOR_MATERIAL);
    end;
    if (uMultitexture in RenderBuffs) then begin
      for i:= 0 to ExTexCoordsId.Count - 1 do begin
          glClientActiveTexture(GL_TEXTURE1+i);
          glDisableClientState(GL_TEXTURE_COORD_ARRAY);
      end;
    end;
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    if (iId <> 0) and (not idxBindOnce) then glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
  end;
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

procedure Col2RowMatrix(mm: TMatrix; var m: TMatrix);
begin
  m[0, 0] := mm[0, 0]; m[1, 0] := mm[0, 1]; m[2, 0] := mm[0, 2]; m[3, 0] := mm[0, 3];
  m[0, 1] := mm[1, 0]; m[1, 1] := mm[1, 1]; m[2, 1] := mm[1, 2]; m[3, 1] := mm[1, 3];
  m[0, 2] := mm[2, 0]; m[1, 2] := mm[2, 1]; m[2, 2] := mm[2, 2]; m[3, 2] := mm[2, 3];
  m[0, 3] := mm[3, 0]; m[1, 3] := mm[3, 1]; m[2, 3] := mm[3, 2]; m[3, 3] := mm[3, 3];
end;

function RenderVBOListWithCulling(List: TList; WorldMatrix: TMatrix; bounds: boolean = false): integer;
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

procedure OptimizeIndeces(CacheSize: integer; BuffSize: integer; var Buff: TVBOBuffer; var BuffList: TList; GenBuff: boolean = true);
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
          Triangles.Count:=n+(Indices.Count div 3);
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

end.

