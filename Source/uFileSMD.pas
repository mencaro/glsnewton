unit uFileSMD;

interface
uses Classes, SysUtils, VectorGeometry,
   {$IFNDEF DIRECTGL}
     OpenGL1x, VectorLists,
   {$ELSE}
     dglOpenGL, uVectorLists,
   {$ENDIF}
     uVBO, uMiscUtils, OGLStateEmul;

Type
  TSMDNode = record
     index: integer;
     name:string;
     parent: integer;
  end;

  TSMDNodePos = record
     index: integer;
     name:string;
     parent: integer;
     x,z,y: single;
     rx,rz,ry: single;
     LocalMatrix:TMatrix;
     GlobalMatrix: TMatrix;
     Quaternion: TQuaternion;
     GlobalPos: TVector;
     LocalPos: TVector;
  end;
  PSMDNodePos = ^TSMDNodePos;

  TPointWeight = record
     NodeIndex: integer;
     Weight: single;
  end;

  TSMDVertex = record
    BoneIndex: integer;
    p,n: TAffineVector;
    u,v: single;
    WeightCount: integer;
    Weight: array of TPointWeight;
  end;

  TSMDTriangle = record
     TextureId: integer;
     v1,v2,v3: TSMDVertex;
     i1,i2,i3:integer;
  end;

  TSMDMesh = record
     Textures: TStringList;
     Triangles: array of TSMDTriangle;
     MaxWeights: integer;
  end;

  TSMDNodes = array of TSMDNodePos;

  TSMDFile = record
     Version: string;
     NodesCount: integer;
     Nodes: array of TSMDNode;
     FramesCount: integer;
     Frames: array of TSMDNodes;//array of TSMDNodePos;
     TrianglesCount: integer;
     Mesh: TSMDMesh;
     Name: string;
  end;
  PSMDFile = ^TSMDFile;

  TAnimations = record
     Mesh: TSMDFile;
     Animations: TList;//PSMDFile
     TextureId: GLUint;
     Bones: integer;
     MaxFrames: integer;
  end;
  PAnimations = ^TAnimations;


Function  SMDLoad(FileName: string): TSMDFile;
Procedure DecreaseWeights(var Mesh: TSMDMesh; WeightsCount: integer = 4);

Function  GetMeshFormSMD(var SMD: TSMDFile; var VBOList:TList; Inverted:boolean=false): integer;
Procedure UpdateSkeleton(var Source,Dest: array of TSMDNodePos);
Procedure CalcSkeleton(var Source,Dest: array of TSMDNodePos);
Procedure CreateSkeleton(var SMD:TSMDFile; FrameNo:integer; var List:TList);
Procedure MakeSceletonStatic(var SMD: TSMDFile);
Procedure TransformModel(Model:TList; var anim: TSMDFile; FrameNo:integer);
Procedure GetBonesAtFrame(smd: PSMDFile; var Bones: array of TVector; FrameNo: single);
Function GetInterpolatedBones(smd: PSMDFile;
  var Bones: array of TVector; FrameNo: single): TSMDNodes;
Procedure BonesToNodes(const Bones: array of TVector; var Nodes: TSMDNodes);
Procedure BlendAnimation( var Nodes: TSMDNodes; Anim1,Anim2: PSMDFile;
                         Frame1,Frame2: single; BlendFactor: single);
Function BlendFrames(const Frame1,Frame2: TSMDNodes; factor: single;
                     var Bones: array of TVector): TSMDNodes;
Function InterpolateFrame(const Frame1,Frame2: TSMDNodes; factor: single): TSMDNodes;
Procedure NodesToBones(const Nodes: TSMDNodes; var Bones: array of TVector);


Function AddAnimation(var anim: TAnimations; FileName: string): TSMDFile;
Function AttachAnimation(var Animations: TList; FileName: string): integer;
Function GetTextureFromAnim(var SMD: TSMDFile):GLUint;overload;
Function GetTextureFromAnim(var anim: TAnimations):GLUint;overload;

implementation

function StringToFloatArray(const s: ansistring; var FloatCount: integer): pointer;
var fa: PSingle;
    i,sign,n,l: integer;
    P: PAnsiChar;
    decimal: single;
    dot: boolean;
    dc: set of char;
    df: boolean;
begin
  dc:=[' ',#9,#13,#10];
  P:=PAnsiChar(s); l:=length(s)-1;
  getmem(fa,FloatCount*4); result:=fa; n:=0;
  decimal:=1; dot:=false; fa^:=0; sign:=1; df:=false;
  for i:=0 to L do begin
    if P^='-' then sign:=-1 else
      if (P^='.') or (P^=',') then dot:=true else
       if (P^ in dc) and (not df) then begin
         if sign=-1 then fa^:=-fa^; df:=true;
         inc(n); inc(fa); decimal:=1; dot:=false; fa^:=0; sign:=1;
       end else if not (P^ in dc) then begin
         df:=false;
         if not dot then fa^:=fa^*10+byte(p^)-48
         else begin decimal:=decimal*0.1; fa^:=fa^+decimal*(byte(p^)-48); end;
       end;
    inc(p);
  end; inc(n);
  FloatCount:=n;
 end;

Function GetNodeById(var Bones: array of TSMDNode; Id: integer): TSMDNode;
var i:integer;
begin
  for i:=0 to high(Bones) do begin
     if Bones[i].index=Id then result:=Bones[i];
  end
end;

Function GetNodePosById(var Frame: array of TSMDNodePos; Id: integer): TSMDNodePos;
var i:integer;
begin
  for i:=0 to high(Frame) do begin
     if Frame[i].index=Id then begin
       result:=Frame[i]; exit;end;
  end
end;

Procedure MakeSceletonStatic(var SMD: TSMDFile);
var
   delta : TVector;
   i : Integer;
   f : Single;
   vs, ve : TVector;
begin
   with SMD do begin
     ve:=vectormake(Frames[FramesCount-1,0].x,Frames[FramesCount-1,0].y,Frames[FramesCount-1,0].z);
     vs:=vectormake(Frames[0,0].x,Frames[0,0].y,Frames[0,0].z);
     delta:=VectorSubtract(ve, vs);
     f:=-1/(FramesCount-1);
     for i:=0 to FramesCount-1 do begin
        vs:=vectormake(Frames[i,0].x,Frames[i,0].y,Frames[i,0].z);
        ve:=VectorCombine(vs, delta, 1, i*f);
        Frames[i,0].x:=ve[0]; Frames[i,0].y:=ve[1]; Frames[i,0].z:=ve[2];
     end;
   end;
end;

procedure UpdateSkeleton(var Source,Dest: array of TSMDNodePos);
var i,count: integer;
    np,pp: TSMDNodePos;
    mat: TMatrix;
begin
   count:=length(Source);
   for i:=0 to Count-1 do begin
     np:=Source[i];
     mat:=CreateTranslationMatrix(np.LocalPos);
     np.LocalMatrix:=MatrixMultiply(np.LocalMatrix,mat);
     if np.parent<>-1 then begin
        pp:=GetNodePosById(dest,np.parent);
        np.GlobalMatrix:=MatrixMultiply(np.LocalMatrix,pp.GlobalMatrix);
     end else begin
        np.GlobalMatrix:=np.LocalMatrix;
     end;
     np.Quaternion:=QuaternionFromMatrix(np.globalMatrix);
     np.GlobalPos:=np.globalMatrix[3];
     dest[i]:=np;
   end;
end;

procedure CalcSkeleton(var Source,Dest: array of TSMDNodePos);
var i,count:integer;
    np,pp:TSMDNodePos;
    mat,RotX,RotY,RotZ,Trans:TMatrix;
begin
   count:=high(Source)+1;
   for i:=0 to Count-1 do begin
     np:=Source[i];
     np.LocalPos:=vectormake(np.x,np.y,np.z,0);
     mat:=IdentityHmgMatrix;
     RotX:=CreateRotationMatrixX(np.rx);
     RotY:=CreateRotationMatrixY(np.ry);
     RotZ:=CreateRotationMatrixZ(np.rz);
     Trans:=CreateTranslationMatrix(np.LocalPos);
     mat:=MatrixMultiply(mat,RotX);
     mat:=MatrixMultiply(mat,RotY);
     mat:=MatrixMultiply(mat,RotZ);
     mat:=MatrixMultiply(mat,Trans);
     np.LocalMatrix:=mat;
     if np.parent<>-1 then begin
        pp:=GetNodePosById(dest,np.parent);
        np.GlobalMatrix:=MatrixMultiply(np.LocalMatrix,pp.GlobalMatrix);
     end else begin
        np.GlobalMatrix:=np.LocalMatrix;
     end;
     np.Quaternion:=QuaternionFromMatrix(np.globalMatrix);
     np.GlobalPos:=np.globalMatrix[3];
     dest[i]:=np;
   end;
end;

function ParseNode(s:string): TSMDNode;
var t:string;
    i:integer;
begin
  i:=pos('"',s);
  t:=copy(s,1,i-1);
  result.index:=StrToInt(TrimLeft(t));
  t:=s;delete(t,1,i);
  i:=pos('"',t);
  result.name:=copy(t,1,i-1);
  delete(t,1,i); t:=TrimLeft(t);
  result.parent:=StrToInt(t);
end;

function ParsePos(ps:string): TSMDNodePos;
var i: integer;
    t: string;
begin
  t:=ps; i:=1;
  while t[i]=' ' do inc(i); delete(t,1,i-1);
  i:=pos(' ',t);
  result.index:=StrToInt(TrimLeft(copy(t,1,i-1)));Delete(t,1,i);
  i:=1; while t[i]=' ' do inc(i); delete(t,1,i-1);
  i:=pos(' ',t);
  result.x:=StrToFloat(TrimLeft(copy(t,1,i-1)));Delete(t,1,i);
  i:=pos(' ',t);
  result.y:=StrToFloat(TrimLeft(copy(t,1,i-1)));Delete(t,1,i);
  i:=pos(' ',t);
  result.z:=StrToFloat(TrimLeft(copy(t,1,i-1)));Delete(t,1,i);
  i:=pos(' ',t);
  result.rx:=StrToFloat(TrimLeft(copy(t,1,i-1)));Delete(t,1,i);
  i:=pos(' ',t);
  result.ry:=StrToFloat(TrimLeft(copy(t,1,i-1)));Delete(t,1,i);
  result.rz:=StrToFloat(TrimLeft(t));
end;

function ParseVertex(s: string): TSMDVertex;
var i,j: integer;
    t: string;
    p,pp: PSingle;
begin
  t:=s; i:=1;
  while t[i]=' ' do inc(i); delete(t,1,i-1);
  t:=t+' '; j:=length(t);
  p:=StringToFloatArray(t,j); pp:=p;
  result.BoneIndex:=trunc(p^);inc(p);
  result.p[0]:=p^; inc(p);
  result.p[1]:=p^; inc(p);
  result.p[2]:=p^; inc(p);
  result.n[0]:=p^; inc(p);
  result.n[1]:=p^; inc(p);
  result.n[2]:=p^; inc(p);
  result.u:=p^; inc(p);
  result.v:=p^; inc(p);
  if j>9 then begin
    result.WeightCount:=trunc(p^); inc(p);
    setlength(result.Weight,result.WeightCount);
    for i:=0 to result.WeightCount-1 do begin
      result.Weight[i].NodeIndex:=trunc(p^); inc(p);
      result.Weight[i].Weight:=p^; inc(p);
    end;
  end else begin
     result.WeightCount:=0;
     setlength(result.Weight,result.WeightCount);
  end;
  p:=pp; freemem(p,length(t)*4);
{

  t:=s; i:=1;
  while t[i]=' ' do inc(i); delete(t,1,i-1);
  i:=pos(' ',t);
  result.BoneIndex:=StrToInt(copy(t,1,i-1));Delete(t,1,i); i:=1;
  while t[i]=' ' do inc(i); delete(t,1,i-1); i:=pos(' ',t);
  result.p[0]:=StrToFloat(copy(t,1,i-1));Delete(t,1,i); i:=1;
  while t[i]=' ' do inc(i); delete(t,1,i-1); i:=pos(' ',t);
  result.p[1]:=StrToFloat(copy(t,1,i-1));Delete(t,1,i); i:=1;
  while t[i]=' ' do inc(i); delete(t,1,i-1); i:=pos(' ',t);
  result.p[2]:=StrToFloat(copy(t,1,i-1));Delete(t,1,i); i:=1;
  while t[i]=' ' do inc(i); delete(t,1,i-1); i:=pos(' ',t);
  result.n[0]:=StrToFloat(copy(t,1,i-1));Delete(t,1,i); i:=1;
  while t[i]=' ' do inc(i); delete(t,1,i-1); i:=pos(' ',t);
  result.n[1]:=StrToFloat(copy(t,1,i-1));Delete(t,1,i); i:=1;
  while t[i]=' ' do inc(i); delete(t,1,i-1); i:=pos(' ',t);
  result.n[2]:=StrToFloat(copy(t,1,i-1));Delete(t,1,i); i:=1;
  while t[i]=' ' do inc(i); delete(t,1,i-1); i:=pos(' ',t);
  result.u:=StrToFloat(copy(t,1,i-1));Delete(t,1,i); i:=1;
  while t[i]=' ' do inc(i); delete(t,1,i-1); i:=pos(' ',t);
  if i=0 then begin
     result.v:=StrToFloat(t);
     result.WeightCount:=0;
     setlength(result.Weight,result.WeightCount);
  end else begin
    result.v:=StrToFloat(copy(t,1,i-1));Delete(t,1,i); i:=1;
    while t[i]=' ' do inc(i); delete(t,1,i-1); i:=pos(' ',t);
    result.WeightCount:=StrToInt(copy(t,1,i-1));Delete(t,1,i); i:=1;
    setlength(result.Weight,result.WeightCount);
    for j:=0 to result.WeightCount-1 do begin
        while t[i]=' ' do inc(i); delete(t,1,i-1); i:=pos(' ',t);
        result.Weight[j].NodeIndex:=StrToInt(copy(t,1,i-1));Delete(t,1,i); i:=1;
        while t[i]=' ' do inc(i); delete(t,1,i-1); i:=pos(' ',t);
        if i>0 then begin
          result.Weight[j].Weight:=StrToFloat(copy(t,1,i-1));Delete(t,1,i); i:=1;
        end else result.Weight[j].Weight:=StrToFloat(t);
    end;
  end;
}
{
  if result.WeightCount>0 then begin
    result.BoneIndex:=result.Weight[0].NodeIndex;
//    result.Weight[0].Weight:=result.Weight[0].Weight;
    for j:=1 to result.WeightCount-1 do begin
      if result.Weight[j].Weight>result.Weight[0].Weight then begin
        result.Weight[0].Weight:=result.Weight[j].Weight;
        result.BoneIndex:=result.Weight[j].NodeIndex;
      end;
    end;
  end;
}
//  if (result.WeightCount=1) then
//     result.BoneIndex:=result.Weight[0].NodeIndex;
end;

Procedure DecreaseWeights(var Mesh: TSMDMesh; WeightsCount: integer = 4);
var i,j,k,n: integer;
    tr: TSMDTriangle;
    tempi: array of integer;
    temps: array of single;
procedure RebuildWeights(var V: TSMDVertex);
  var i,j,k: integer;
      sum: single;
      w: array of TPointWeight;
      p: TPointWeight;
  begin
    if V.WeightCount>0 then v.BoneIndex:=v.Weight[0].NodeIndex;
    if V.WeightCount<=WeightsCount then exit;
    sum:=0; setlength(w,V.WeightCount);
    for i:=0 to V.WeightCount-1 do begin
      w[i]:=V.Weight[i];
      for j:=i+1 to V.WeightCount-1 do
        if V.Weight[j].Weight>w[i].Weight then begin
          p:=w[i]; w[i]:=V.Weight[j]; v.Weight[j]:=p;
        end;
    end;
    setlength(V.Weight,WeightsCount);
    for i:=0 to WeightsCount-1 do sum:=sum+w[i].Weight;
    for i:=0 to WeightsCount-1 do begin
      V.Weight[i].Weight:=w[i].Weight/sum;
      V.Weight[i].NodeIndex:=w[i].NodeIndex;
    end;
    v.BoneIndex:=v.Weight[0].NodeIndex;
//    if WeightsCount=1 then v.BoneIndex:=v.Weight[0].NodeIndex;


  end;
begin
  if Mesh.MaxWeights<=WeightsCount then exit;
  setlength(tempi,WeightsCount);
  setlength(temps,WeightsCount);
  for i:=0 to length(Mesh.Triangles)-1 do begin
    tr:=Mesh.Triangles[i];
    RebuildWeights(tr.v1);
    RebuildWeights(tr.v2);
    RebuildWeights(tr.v3);
    Mesh.Triangles[i]:=tr;
  end; Mesh.MaxWeights:=WeightsCount;
end;

Function SMDLoad(FileName: string): TSMDFile;
var f: TStringList;
    i,j,n,fi,ni,ti: integer;
    s: string;
    node: TSMDNode;
begin
  f:=TStringList.Create;
  f.LoadFromFile(FileName);
  Result.Version:=f[0];
  result.Name:=ExtractFileName(FileName);
  i:=pos('.smd',lowercase(result.Name));
  if i>0 then delete(result.Name,i,4);
  
  n:=f.IndexOf('nodes');i:=n;
  assert(n>=0,'Nodes not found');
  repeat n:=n+1;
  until (n=f.Count) or (f[n]='end');
  assert(n<f.Count,'"End" not found');
  n:=n-i-1;assert(n>0);Result.NodesCount:=n;
  setlength(result.Nodes,n);
  for j:=0 to n-1 do result.Nodes[j]:=ParseNode(f[j+i+1]);
  n:=f.IndexOf('skeleton');i:=n;
  assert(n>=0,'Skeleton not found');
  Result.FramesCount:=0;
  repeat n:=n+1;
    if pos('time',f[n])>0 then inc(Result.FramesCount);
  until (n=f.Count) or (f[n]='end');
  n:=n-i-1;assert(n>0); fi:=-1; ni:=0;
  setlength(result.Frames,Result.FramesCount,Result.NodesCount);
  for j:=0 to n-1 do begin
     if pos('time',f[j+i+1])>0 then begin inc(fi);ni:=0;end
     else begin
       result.Frames[fi,ni]:=ParsePos(f[j+i+1]);inc(ni);
       node:=GetNodeById(result.Nodes, result.Frames[fi,ni-1].Index);
       result.Frames[fi,ni-1].name:=node.name;
       result.Frames[fi,ni-1].parent:=node.parent;
     end;
  end;
  n:=f.IndexOf('triangles');i:=n;
  Result.TrianglesCount:=0;
  if n>0 then begin
    repeat n:=n+1;
    until (n=f.Count) or (f[n]='end');
    n:=n-i-1; Result.TrianglesCount:=n div 4;
    setlength(Result.Mesh.Triangles, Result.TrianglesCount);
    Result.Mesh.MaxWeights:=0;
    with Result.Mesh do begin
      Textures:=TStringList.Create;
      for j:=0 to Result.TrianglesCount-1 do begin
        s:=f[j*4+i+1]; ti:=Textures.IndexOf(s);
        if ti>=0 then
          Triangles[j].TextureId:=ti
        else Triangles[j].TextureId:=Textures.Add(s);
        s:=f[j*4+i+2]; Triangles[j].v1:=ParseVertex(s);
        MaxWeights:=max(MaxWeights,Triangles[j].v1.WeightCount);
        s:=f[j*4+i+3]; Triangles[j].v2:=ParseVertex(s);
        MaxWeights:=max(MaxWeights,Triangles[j].v1.WeightCount);
        s:=f[j*4+i+4]; Triangles[j].v3:=ParseVertex(s);
        MaxWeights:=max(MaxWeights,Triangles[j].v1.WeightCount);
      end;
    end;
  end;
  f.Free;
//  DecreaseWeights(Result.Mesh,1);
//  Result.Mesh.MaxWeights:=0;

  if Result.FramesCount>1 then MakeSceletonStatic(Result);
  for i:=0 to Result.FramesCount-1 do
      CalcSkeleton(Result.Frames[i],Result.Frames[i]);
end;

Function AddAnimation(var Anim: TAnimations; FileName: string): TSMDFile;
var prev,smd: PSMDFile;
begin
  new(smd); smd^:=SMDLoad(FileName); result:=smd^;
  if not assigned(Anim.Animations) then
     Anim.Animations:=TList.Create;
  if Anim.Animations.Count=0 then begin
     Anim.Animations.Add(smd);
     Anim.Bones:=smd.NodesCount;
  end else begin
     prev:=Anim.Animations[0];
     assert(prev.NodesCount=smd.NodesCount,'Different skeletons');
     Anim.Animations.Add(smd);
  end;
end;

Function AttachAnimation(var Animations: TList; FileName: string): integer;
var smd: PSMDFile;
begin
  new(smd); smd^:=SMDLoad(FileName);
  if not assigned(Animations) then Animations:=TList.Create;
  result:=Animations.Add(smd);
end;

Procedure IndexingMesh(buff:PVBOBuffer; var iVBO: PVBOBuffer);
var i,j:integer;
    v,t,n, vt,tt,nt: TAffineVector;
    b,w: TVector;
    Joints: PVBOAttribute;
    Weights: PVBOAttribute;
    f:boolean;
begin
   InitVBOBuff(ivbo^,buff.FaceType,DrawElements);
   ivbo.MatName:=buff.MatName;
   ivbo.RenderBuffs:=buff.RenderBuffs+[uIndices];
   ivbo.UseTwoTexturesCoord:=buff.UseTwoTexturesCoord;
   for i:=0 to buff.Vertexes.Count-1 do begin
     v:=buff.Vertexes[i];
     n:=buff.Normals[i];
     t:=buff.TexCoords[i];
     j:=0; f:=false;
     while (not f) and (j<ivbo.Vertexes.Count) do begin
        vt:=ivbo.Vertexes[j];
        nt:=ivbo.Normals[j];
        tt:=ivbo.TexCoords[j];
        f:=(VectorEquals(v,vt) and VectorEquals(n,nt) and VectorEquals(t,tt));
        if not f then j:=j+1;
     end;
     if not f then begin
        ivbo.Vertexes.Add(v);
        ivbo.Normals.Add(n);
        ivbo.TexCoords.Add(t);
        ivbo.Indices.Add(ivbo.Vertexes.Count-1);
     end else ivbo.Indices.Add(j);
   end;
end;

Procedure MakeInvertedVertex(var V:TSMDVertex; const bone: TSMDNodePos);
var p: TVector;
    invMat : TMatrix;
begin
   // transform point
   MakePoint(p, V.p);
   invMat:=bone.GlobalMatrix;
   InvertMatrix(invMat);
   p:=VectorTransform(p, invMat);
   v.p:=PAffineVector(@p)^;
   // transform normal
   SetVector(p, v.n);
   invMat:=bone.GlobalMatrix;
   invMat[3]:=NullHmgPoint;
   InvertMatrix(invMat);
   p:=VectorTransform(p, invMat);
   v.n:=PAffineVector(@p)^;
end;

function GetMeshFormSMD(var SMD: TSMDFile; var VBOList:TList; inverted:boolean=false): integer;
var buff,tempb:PVBOBuffer;
    i,j:integer;
    tr,t,temp:TSMDTriangle;
    b1,b2,b3: TSMDNodePos;
    t1,t2,t3:TSMDVertex;
    emin,emax: TAffineVector;
    Joints: PVBOAttribute;
    Weights: PVBOAttribute;
    BoneWeightCount: integer;
begin
  assert(SMD.TrianglesCount>0,'SMD don''t have meshes');
  //Sort By Texture
  if not assigned(VBOList) then VBOList:=TList.Create; i:=0;
  repeat
     tr:=SMD.Mesh.Triangles[i];
     for j:=i+1 to SMD.TrianglesCount-1 do begin
         t:=SMD.Mesh.Triangles[j];
         if (tr.TextureId=t.TextureId) then begin
            if j<>i+1 then begin
              temp:=t;SMD.Mesh.Triangles[j]:=SMD.Mesh.Triangles[i+1];
              SMD.Mesh.Triangles[i+1]:=temp; inc(i);
            end else inc(i);
         end;
     end; inc(i);
  until i=SMD.TrianglesCount;
  BoneWeightCount:=min(SMD.Mesh.MaxWeights,4);
  //Create buffers
  new(buff);InitVBOBuff(buff^,GL_TRIANGLES,DrawArrays);
  buff.RenderBuffs:=[uNormals, uTexCoords];
  buff.UseTwoTexturesCoord:=false;
{  if SMD.Mesh.MaxWeights>0 then begin
    new(Joints); Joints.Name:='Joints';
//    Joints.Size:=BoneWeightCount*Sizeof(TAffineVector);
    Joints.CCount:=BoneWeightCount;
    Joints.CSize:=Sizeof(TVector); Joints.CType:=$1406;
    Joints.AttrType:=atUserAttrib; Joints.Location:=-1;
    Joints.DataHandler:=TVectorList.Create;
    if SMD.Mesh.MaxWeights>2 then begin
      new(Weights); Weights.Name:='Weights';
  //    Weights.Size:=BoneWeightCount*Sizeof(TAffineVector);
      Weights.CCount:=BoneWeightCount;
      Weights.CSize:=Sizeof(TVector); Weights.CType:=$1406;
      Weights.AttrType:=atUserAttrib; Weights.Location:=-1;
      Weights.DataHandler:=TVectorList.Create;
    end else begin
      Joints.CCount:=4; Buff.CompactWeights:=true;
    end;
  end;
}
  t:=SMD.Mesh.Triangles[0]; i:=0;
  repeat
     tr:=SMD.Mesh.Triangles[i];
     if t.TextureId<>tr.TextureId then begin
        buff.MatName:=SMD.Mesh.Textures[t.TextureId];
        delete(buff.MatName,length(buff.MatName)-4,4);
        new(tempb); IndexingMesh(buff,tempb);
        FreeVBOMem(buff^); dispose(buff);
        GenVBOBuff(tempb^,false); t:=tr;
        result:=VBOList.Add(tempb); new(buff);

        InitVBOBuff(buff^,GL_TRIANGLES,DrawArrays);
        buff.RenderBuffs:=[uNormals, uTexCoords];
        buff.UseTwoTexturesCoord:=false;
     end;
     with SMD.Mesh.Triangles[i], Buff^ do begin
        t1:=v3; t2:=v2; t3:=v1;
        if i=0 then begin emin:=v1.p;emax:=v1.p; end;
        emin:=GetMinExtents(emin,v1.p);
        emin:=GetMinExtents(emin,v2.p);
        emin:=GetMinExtents(emin,v3.p);
        emax:=GetMaxExtents(emax,v1.p);
        emax:=GetMaxExtents(emax,v2.p);
        emax:=GetMaxExtents(emax,v3.p);
//        Normals.Add(t2.n,t3.n,t1.n);
        b3:=SMD.Frames[0,v1.BoneIndex];
        b2:=SMD.Frames[0,v2.BoneIndex];
        b1:=SMD.Frames[0,v3.BoneIndex];
        if Inverted then begin
          MakeInvertedVertex(t3,b3);
          MakeInvertedVertex(t2,b2);
          MakeInvertedVertex(t1,b1);
        end;
        Vertexes.Add(t3.p,t2.p,t1.p);
        Normals.Add(t3.n,t2.n,t1.n);
        if SMD.Mesh.MaxWeights=0 then begin
          TexCoords.Add(affinevectormake(t3.u, t3.v, b3.index{/(SMD.NodesCount-1)}));
          TexCoords.Add(affinevectormake(t2.u, t2.v, b2.index{/(SMD.NodesCount-1)}));
          TexCoords.Add(affinevectormake(t1.u, t1.v, b1.index{/(SMD.NodesCount-1)}));
        end else begin
          TexCoords.Add(affinevectormake(t3.u, t3.v, 0));
          TexCoords.Add(affinevectormake(t2.u, t2.v, 0));
          TexCoords.Add(affinevectormake(t1.u, t1.v, 0));
        end;
     end; inc(i);
  until i=SMD.TrianglesCount;
  buff.MatName:=SMD.Mesh.Textures[t.TextureId];
  delete(buff.MatName,length(buff.MatName)-4,4);
  new(tempb);IndexingMesh(buff,tempb);
  FreeVBOMem(buff^); dispose(buff);
  GenVBOBuff(tempb^,false);
  tempb.emin:=emin; tempb.emax:=emax;
  result:=VBOList.Add(tempb);
end;


Function GetTextureFromAnim(var SMD: TSMDFile): GLUint;
var q: TQuaternion;
    i,j, w,h: integer;
    node: TSMDNodePos;
    p1,p2: TVector;
    data: array of TVector;
    TextureId: cardinal;
    TrMat:TMatrix;
    function IndexFromPos(x,y,w,h:integer):integer;
    begin {result:=(h-y-1)*w+x;} result:=(y)*w+x; end;
const Target:GLEnum = GL_TEXTURE_RECTANGLE;
begin
  w:=smd.FramesCount*2;//<-------------//*2
  h:=smd.NodesCount;
  setlength(data,w*h);
  for i:=0 to smd.NodesCount-1 do
  for j:=0 to smd.FramesCount-1 do begin
      node:=smd.Frames[j,i];
      with node do begin
//        q:=QuaternionFromMatrix(LocalMatrix);//Quaternion;
//        setvector(p1,x,y,z,index);
        p1:=node.GlobalPos;p1[3]:=index;
//        setvector(p2,q.ImagPart,q.RealPart);
//        q:=node.Quaternion;
//        setvector(p2,q.ImagPart,q.RealPart);
//        data[(h-1-i)*w+j*2]:=p1; data[(h-1-i)*w+j*2+1]:=p2;
        TrMat:=GlobalMatrix;
        q:=QuaternionFromMatrix(TrMat);
        NormalizeQuaternion(q);
        setvector(p2,q.ImagPart,q.RealPart);
//        TransposeMatrix(TrMat);
//        TrMat:=IdentityHmgMatrix;

//        data[IndexFromPos(j*4+0,i,w,h)]:=TrMat[0];//vectormake(1.1,2.2,3.3,0.5);
        data[i*w+j*2+0]:=p1;//vectormake(1.1,2.2,3.3,0.5);
        data[i*w+j*2+1]:=p2;//vectormake(1.1,2.2,3.3,0.5);
      end;
  end;
  glGenTextures(1, @TextureId);
  glBindTexture(Target, TextureId);
//  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

  glTexParameteri ( Target, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri ( Target, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexParameteri ( Target, GL_TEXTURE_MIN_FILTER, GL_NEAREST );
  glTexParameteri ( Target, GL_TEXTURE_MAG_FILTER, GL_NEAREST );

  //glTexImage2D    ( GL_TEXTURE_2D, 0,GL_RGBA32F, w,h, 0, GL_RGBA, GL_FLOAT, @Data[0]);
  glTexImage2D    ( Target, 0,GL_RGBA_FLOAT32_ATI, w,h, 0, GL_RGBA, GL_FLOAT, @Data[0]);
  glBindTexture   ( Target, 0 );

  result:=TextureId;

end;

Function GetTextureFromAnim(var anim: TAnimations):GLUint;overload;
var q: TQuaternion;
    a,i,j, w,h,dh,fc: integer;
    node: TSMDNodePos;
    p1,p2: TVector;
    data: array of TVector;
    TextureId: cardinal;
    TrMat:TMatrix;
    smd,ta: PSMDFile;

const Target:GLEnum = GL_TEXTURE_RECTANGLE;
begin
  result:=0;
  if (not assigned(anim.Animations)) or (anim.Animations.Count=0) then exit;
  smd:=anim.Animations[0];

  fc:=smd.FramesCount;
  for i:=0 to anim.Animations.Count-1 do begin
    ta:=anim.Animations[i]; if ta.FramesCount>fc then fc:=ta.FramesCount;
  end;
  anim.MaxFrames:=fc;
  w:=fc*2;//w:=smd.FramesCount*2;
  dh:=smd.NodesCount;

  h:=dh*anim.Animations.Count;
  setlength(data,w*h);
  //for a:=anim.Animations.Count-1 to 0 do begin
  for a:=0 to anim.Animations.Count-1 do begin
    smd:=anim.Animations[a];
    //for i:=0 to smd.NodesCount-1 do
    for i:=smd.NodesCount-1 downto 0 do
    for j:=0 to smd.FramesCount-1 do begin
        node:=smd.Frames[j,i];
        with node do begin
          p1:=node.GlobalPos;p1[3]:=index;
          TrMat:=GlobalMatrix;
          q:=QuaternionFromMatrix(TrMat);
          NormalizeQuaternion(q);
          setvector(p2,q.ImagPart,q.RealPart);
          data[(dh*a+i)*w+j*2+0]:=p1;
          data[(dh*a+i)*w+j*2+1]:=p2;
        end;
    end;
  end;
  glGenTextures(1, @TextureId);
  glBindTexture(Target, TextureId);
  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

  glTexParameteri ( Target, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri ( Target, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexParameteri ( Target, GL_TEXTURE_MIN_FILTER, GL_NEAREST );
  glTexParameteri ( Target, GL_TEXTURE_MAG_FILTER, GL_NEAREST );

  glTexImage2D    ( Target, 0,GL_RGBA_FLOAT32_ATI, w,h, 0, GL_RGBA, GL_FLOAT, @Data[0]);
  glBindTexture   ( Target, 0 );
  result:=TextureId;
end;

Procedure CreateSkeleton(var SMD:TSMDFile; FrameNo:integer; var List:TList);
var p,t:PVBOBuffer;
    i:integer;
    np,pp:TSMDNodePos;
    bones:array of TSMDNodePos;
begin
   setlength(bones,smd.NodesCount);
   for i:=0 to SMD.NodesCount-1 do bones[i]:=SMD.Frames[FrameNo,i];
   new(p); InitVBOBuff(p^,GL_LINES,DrawArrays);
   new(t); InitVBOBuff(t^,GL_POINTS,DrawArrays);
   for i:=0 to SMD.NodesCount-1 do begin
     np:=bones[i];
     if np.parent>=0 then begin
       pp:=GetNodePosById(bones,np.parent);
         p.Vertexes.Add(np.GlobalPos);
         p.Vertexes.Add(pp.GlobalPos);
     end;
     t.Vertexes.Add(np.GlobalPos);
     t.Colors.Add(vectormake(1,0,0,1));
   end;
   p.RenderBuffs:=[];
   t.RenderBuffs:=[uColors];
   GenVBOBuff(p^);GenVBOBuff(t^);
   if not assigned (list) then List:=TList.Create;
   List.Add(p); List.Add(t);
end;

Procedure TransformModel(Model:TList; var anim: TSMDFile; FrameNo:integer);
var i,j,bindex:integer;
    P:PVBOBuffer;
    V,T,N:TAffineVector;
    bone:TSMDNodePos;
    AMat:TAffineMatrix;
    mat:TMatrix;
    x:single;
begin
  for i:=0 to Model.Count-1 do begin
    P:=Model[i];
    for j:=0 to P.Vertexes.Count-1 do begin
        v:=P.Vertexes[j];
        t:=p.TexCoords[j];
        n:=p.Normals[j];
        bindex:=system.round(t[2]*(anim.NodesCount-1));
        bone:=anim.Frames[FrameNo,bindex];
        mat:=bone.GlobalMatrix;
        SetMatrix(AMat,mat);
        v:=VectorTransform(v,Mat);
        x:=v[1];v[1]:=v[2];v[2]:=x;
        P.Vertexes[j]:=v;
        n:=VectorTransform(n,AMat); NormalizeVector(n);
        x:=n[1];n[1]:=n[2];n[2]:=x;
        P.Normals[j]:=n;
    end;
    RebuildVBOBuff(p^,false);
  end;
end;

//Ћинейна€ интерпол€ци€ (без учета иерархии костей) между кадрами анмиации
Procedure GetBonesAtFrame(smd: PSMDFile; var Bones: array of TVector; FrameNo: single);
var q,q2: TQuaternion;
    i, f1,f2: integer;
    node,node2: TSMDNodePos;
    p1,p2: TVector;
    TrMat:TMatrix;
    factor: single;
begin
  f1:=trunc(FrameNo); f2:=f1+1; factor:=frac(FrameNo);
  if f2>=smd.FramesCount then f2:=f2-smd.FramesCount;

  for i:=0 to smd.NodesCount-1 do begin
    node:=smd.Frames[f1,i];
    if factor>0 then begin
      node2:=smd.Frames[f2,i];
      p1:=VectorLerp(node.GlobalPos,node2.GlobalPos,factor);
      p1[3]:=node.index;
      TrMat:=node.GlobalMatrix;
      q:=QuaternionFromMatrix(TrMat);
      TrMat:=node2.GlobalMatrix;
      q2:=QuaternionFromMatrix(TrMat);
      NormalizeQuaternion(q);
      NormalizeQuaternion(q2);
      q:=QuaternionSlerp(q,q2,factor);
      NormalizeQuaternion(q);
      setvector(p2,q.ImagPart,q.RealPart);
      Bones[i*2]:=p1; Bones[i*2+1]:=p2;
    end else begin
      with node do begin
        p1:=node.GlobalPos;p1[3]:=index;
        TrMat:=GlobalMatrix;
        q:=QuaternionFromMatrix(TrMat);
        NormalizeQuaternion(q);
        setvector(p2,q.ImagPart,q.RealPart);
        Bones[i*2]:=p1; Bones[i*2+1]:=p2;
      end;
    end;
  end;
end;

function GetInterpolatedBones(smd: PSMDFile;
  var Bones: array of TVector; FrameNo: single): TSMDNodes;
var q,q2: TQuaternion;
    i, f1,f2: integer;
    node,node2: TSMDNodes;
    p1,p2: TVector;
    TrMat:TMatrix;
    factor: single;
begin
  f1:=trunc(FrameNo); f2:=f1+1; factor:=frac(FrameNo);
  if f2>=smd.FramesCount then f2:=f2-smd.FramesCount;
  node:=smd.Frames[f1];
  if factor>0 then begin
    node2:=smd.Frames[f2];
    result:=BlendFrames(node,node2,factor,Bones);
  end else begin
    result:=node; NodesToBones(node,Bones);
  end;
end;

procedure BonesToNodes(const Bones: array of TVector; var Nodes: TSMDNodes);
var Pos:TVector;
    Q:TQuaternion;
    M,InvM:TMatrix;
    I,Par:integer;
begin
  if Length(Bones)<>Length(Nodes)*2 then Exit; // хот€ тут можно и выдать сообщение о несоответствии костей
  for i:=0 to Length(Nodes)-1 do begin
    Par:=Nodes[i].parent;
    if Par=-1 then begin
      Pos:=Bones[Nodes[i].Index*2];
      Q.ImagPart:=AffineVectorMake(Bones[Nodes[i].Index*2+1]);
      Q.RealPart:=Bones[Nodes[i].Index*2+1][3];
      M:=QuaternionToMatrix(Q);
      Pos[3]:=1; M[3]:=Pos;
      Nodes[i].GlobalMatrix:=M;
      Nodes[i].LocalMatrix:=M;
      Nodes[i].LocalPos:=pos;
    end else begin
      Pos:=Bones[Nodes[i].Index*2]; Pos[3]:=1;
      Q.ImagPart:=AffineVectorMake(Bones[Nodes[i].Index*2+1]);
      Q.RealPart:=Bones[Nodes[i].Index*2+1][3];
      M:=QuaternionToMatrix(Q); M[3]:=Pos;
      InvM:=MatrixInvert(Nodes[Par].GlobalMatrix);
      Nodes[i].GlobalMatrix:=M;
      Nodes[i].LocalMatrix:=MatrixMultiply(M,InvM);
      Nodes[i].LocalPos:=VectorTransform(Pos,InvM);
    end;
  end;
end;

//—мешиваем кадры двух анимаций
procedure BlendAnimation( var Nodes: TSMDNodes; Anim1,Anim2: PSMDFile;
  Frame1,Frame2: single; BlendFactor: single);
var i:integer;
    p,p1,p2: TVector;
    q,q1,q2: TQuaternion;
begin
  Assert(Anim1.NodesCount=Anim2.NodesCount,'Number of bones is differs!');
  setlength(nodes,Anim1.NodesCount);
  for i:=0 to Anim1.NodesCount-1 do begin
    p1:=Anim1.Frames[Trunc(Frame1),i].LocalPos;
    p2:=Anim2.Frames[Trunc(Frame2),i].LocalPos;
    q1:=QuaternionFromMatrix(Anim1.Frames[Trunc(Frame1),i].LocalMatrix);
    NormalizeQuaternion(q1);
    q2:=QuaternionFromMatrix(Anim2.Frames[Trunc(Frame2),i].LocalMatrix);
    NormalizeQuaternion(q2);
    q:=QuaternionSlerp(q1,q2,BlendFactor);
    p:=VectorLerp(p1,p2,BlendFactor);
    nodes[i].LocalMatrix:=QuaternionToMatrix(q);
    nodes[i].LocalPos:=p;
    nodes[i].index:=Anim1.Frames[trunc(Frame1),i].index;
    nodes[i].parent:=Anim1.Frames[trunc(Frame1),i].parent;
  end;

  UpdateSkeleton(nodes,nodes);
end;


function InterpolateFrame(const Frame1,Frame2: TSMDNodes; factor: single): TSMDNodes;
var q,q1,q2: TQuaternion;
    i, f1,f2: integer;
    p,p1,p2: TVector;
    TrMat:TMatrix;
    ncount: integer;
begin
  ncount:=length(Frame1);
  for i:=0 to ncount-1 do begin
    p1:=Frame1[i].LocalPos; p2:=Frame2[i].LocalPos;
    q1:=QuaternionFromMatrix(Frame1[i].LocalMatrix);
    NormalizeQuaternion(q1);
    q2:=QuaternionFromMatrix(Frame2[i].LocalMatrix);
    NormalizeQuaternion(q2);
    q:=QuaternionSlerp(q1,q2,Factor);
    p:=VectorLerp(p1,p2,Factor);
    result[i].LocalMatrix:=QuaternionToMatrix(q);
    result[i].LocalPos:=p;
    result[i].index:=Frame1[i].index;
    result[i].parent:=Frame1[i].parent;
  end;
  UpdateSkeleton(result,result);
end;

//»нтерпол€ци€ между кадрами анимации
function BlendFrames(const Frame1,Frame2: TSMDNodes; factor: single;
  var Bones: array of TVector): TSMDNodes;
var q,q1,q2: TQuaternion;
    i, f1,f2: integer;
    p,p1,p2: TVector;
    TrMat:TMatrix;
    ncount: integer;
    Nodes: TSMDNodes;
begin
  Assert(length(Frame1)=length(Frame2),'Number of nodes is differs!');
  ncount:=length(Frame1);
  if length(Bones)<>ncount*2 then exit;

 // assert(length(Bones)=ncount*2,'Size of Bones does not match Nodes count!');
  setlength(Nodes,ncount);
  for i:=0 to ncount-1 do begin
    p1:=Frame1[i].LocalPos; p2:=Frame2[i].LocalPos;
    q1:=QuaternionFromMatrix(Frame1[i].LocalMatrix);
    NormalizeQuaternion(q1);
    q2:=QuaternionFromMatrix(Frame2[i].LocalMatrix);
    NormalizeQuaternion(q2);
    q:=QuaternionSlerp(q1,q2,Factor);
    p:=VectorLerp(p1,p2,Factor);
    nodes[i].LocalMatrix:=QuaternionToMatrix(q);
    nodes[i].LocalPos:=p;
    nodes[i].index:=Frame1[i].index;
    nodes[i].parent:=Frame1[i].parent;
  end;
  UpdateSkeleton(nodes,nodes);
  if length(Result)=ncount then result:=Nodes;

  for i:=0 to ncount-1 do begin
    with nodes[i] do begin
      p1:=GlobalPos; p1[3]:=index;
      TrMat:=GlobalMatrix;
      q:=QuaternionFromMatrix(TrMat);
      NormalizeQuaternion(q);
      setvector(p2,q.ImagPart,q.RealPart);
      Bones[i*2]:=p1; Bones[i*2+1]:=p2;
    end;
  end;
end;

//ѕересчет костей в пару позици€-кватернион
procedure NodesToBones(const Nodes: TSMDNodes; var Bones: array of TVector);
var q: TQuaternion;
    i: integer;
    p,p1,p2: TVector;
    TrMat:TMatrix;
    ncount: integer;
begin
  ncount:=length(Nodes);
  if length(Bones)<>ncount*2 then exit;
  for i:=0 to ncount-1 do begin
    with nodes[i] do begin
      p1:=GlobalPos; p1[3]:=index;
      TrMat:=GlobalMatrix;
      q:=QuaternionFromMatrix(TrMat);
      NormalizeQuaternion(q);
      setvector(p2,q.ImagPart,q.RealPart);
      Bones[i*2]:=p1; Bones[i*2+1]:=p2;
    end;
  end;
end;


end.
