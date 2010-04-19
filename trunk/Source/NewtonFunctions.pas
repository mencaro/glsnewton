unit NewtonFunctions;

interface

uses
  // стандартные
  Windows, SysUtils, GLObjects, Classes,GLScene, GLVectorFileObjects,
  GLKeyBoard, VectorGeometry, VectorTypes, VectorLists,OpenGL1X,
  // дополнительные
  CalculatePhysics,uVBO,VBOMesh,NewtonImport;

type byte0_2=0..2;

type TObjectExceptionsVBO=array of byte0_2;

type TCamMoveResult=record
  isMove:bool;
  dX,dY:integer;
end;

var NewtonGravity:real;

procedure Debug_ShowGeometryCollision(const Body : PNewtonBody;
        VertexCount : Integer;const FaceArray : PFloat; FaceId : int); cdecl;
procedure NewtonApplyForceAndTorque
       ( const body : PNewtonBody; timestep : Float; threadIndex : int ); cdecl;
procedure ApplyMatrixesVBO
       ( const body : PNewtonBody; const matrix : PFloat; threadIndex : int ); cdecl;
procedure ApplyMatrixes
       ( const body : PNewtonBody; const matrix : PFloat; threadIndex : int ); cdecl;
procedure NullNewtonContactsProcess(
         const contact : PNewtonJoint; timestep : Float; threadIndex : int ); cdecl;
procedure ApplyMatrixes2Bot
       ( const body : PNewtonBody; const matrix : PFloat; threadIndex : int ); cdecl;
procedure ApplyMatrixes2BotVBO
       ( const body : PNewtonBody; const matrix : PFloat; threadIndex : int ); cdecl;

type TSceneNewtonWorld=class
  public
     JumpSpeed,PlayerSpeed:real;
     NewtonBodyList:TList;
     NewtonBodyPlayer:PNewtonBody;
     constructor Create(Friction:integer;Solver:integer;
                        WorldSizeFrom,WorldSizeTo:TVector3f); overload;
     destructor Destroy; override;
     procedure CreateTreeBody(Mesh:TGLFreeForm;Matrix:TMatrix);
     procedure CreateTreeBodyVBO(VBOMesh:TVBOMesh;
                                 Exceptions:TObjectExceptionsVBO=nil);
     procedure CreateConvexBodyVBO(VBOMesh:TVBOMesh;Weight:real;
                                   Exceptions:TObjectExceptionsVBO=nil);
     procedure CreatePlayerSphere(Rx,Ry,Rz:real;PlayerMatrix:TMatrix);
     procedure SetPlayerPosAndAngle(Pos,Angle:TVector4f);
     procedure Debug_ShowBodyCollision(const Body : PNewtonBody); cdecl;
     procedure AddDynamicBox(X,Y,Z:real;Obj:TGLBaseSceneObject);
     procedure AddDynamicSphere(X,Y,Z:real;Obj:TGLBaseSceneObject);
     procedure AddBox4Bot(X,Y,Z:real;Obj:TGLBaseSceneObject);
     procedure AddSphere4Bot(X,Y,Z:real;Obj:TGLBaseSceneObject);
     procedure CreateConvexBody(Mesh:TGLFreeForm;Matrix:TMatrix);
     function  FPSApplyCamMove(Px,Py:integer;
                              GLSCamera:TGLCamera;Player:TGLBaseSceneObject;CamSpeed:real;MaxCamAngle:real):TCamMoveResult;
     function  FPSApplyKeyMove(Player:TGLBaseSceneObject;
                              KeyUP,KeyDOWN,KeyLEFT,KeyRIGHT:integer;Jump:boolean):bool;
     function  Pi2Gr(Gr:single):single;
     procedure UpdateWorld(time:real);
  private
     PlayerID:integer;
     DefaultID:integer;
     SceneNewtonWorld:PNewtonWorld;
     procedure DestroySceneNewtonWorld;
     procedure InitMaterials;
     procedure ClearBodyList;
end;

 {procedure GameInitSceneNewtonWorld(Friction:integer;Solver:integer;
  WorldSizeFrom,WorldSizeTo:TVector3f);}

implementation

procedure TSceneNewtonWorld.UpdateWorld(time:real);
begin
  NewtonUpdate(SceneNewtonWorld,time);
end;

destructor TSceneNewtonWorld.Destroy;
begin
  DestroySceneNewtonWorld;
end;

procedure ApplyMatrixes2Bot
( const body : PNewtonBody; const matrix : PFloat; threadIndex : int ); cdecl;
var
  Obj:TGLBaseSceneObject;
  Mat:TMatrix;
begin
  Obj:=TGLBaseSceneObject(NewtonBodyGetUserData(Body));
  NewtonBodyGetMatrix(body,@Mat);
  mat[0]:=Obj.Matrix[0];
  mat[1]:=Obj.Matrix[1];
  mat[2]:=Obj.Matrix[2];
  NewtonBodySetMatrix(Body,@mat);
  Obj.Matrix:=Mat;
end;

procedure ApplyMatrixes2BotVBO
( const body : PNewtonBody; const matrix : PFloat; threadIndex : int ); cdecl;
var
  Obj:TVboMesh;
  Mat:TMatrix;
begin
  Obj:=TVBOMESH(NewtonBodyGetUserData(Body));
  NewtonBodyGetMatrix(body,@Mat);
  mat[0]:=Obj.ObjectsList[0].Matrices.WorldMatrix[0];
  mat[1]:=Obj.ObjectsList[0].Matrices.WorldMatrix[1];
  mat[2]:=Obj.ObjectsList[0].Matrices.WorldMatrix[2];
  NewtonBodySetMatrix(Body,@mat);
  Obj.ObjectsList[0].ResetMatrices;
  Obj.ObjectsList[0].Matrices.ModelMatrix:=Mat;
  Obj.ObjectsList[0].UpdateWorldMatrix;
end;

procedure ApplyMatrixesVBO
( const body : PNewtonBody; const matrix : PFloat; threadIndex : int ); cdecl;
var
 Mat:TMatrix;
 Mesh:TVBOMeshObject;
begin
  Mesh:=TVBOMeshObject(NewtonBodyGetUserData(Body));
  NewtonBodyGetMatrix(Body,@mat);
  Mesh.ResetMatrices;
  Mesh.Matrices.ModelMatrix:=mat;
  Mesh.UpdateWorldMatrix
end;

procedure ApplyMatrixes
( const body : PNewtonBody; const matrix : PFloat; threadIndex : int ); cdecl;
var
 Obj:TGLBaseSceneObject;
 Mat:TMatrix;
begin
  Obj:=TGLBaseSceneObject(NewtonBodyGetUserData(Body));
  NewtonBodyGetMatrix(Body,@mat);
  Obj.Matrix:=mat;
end;

function TSceneNewtonWorld.FPSApplyCamMove(Px,Py:integer;
GLSCamera:TGLCamera;Player:TGLBaseSceneObject;CamSpeed:real;MaxCamAngle:real):TCamMoveResult;
var cPos:TPoint;
begin
 GetCursorPos(cPos);
 if (CPos.X=Px) and (Cpos.Y=Py) then begin
   result.isMove:=false;
   exit;
 end;
 result.dX:=-(cPos.X-pX);
 result.dY:=(pY-cPos.Y);
 GLSCamera.PitchAngle:=GLSCamera.PitchAngle+(result.dY*CamSpeed);
 Player.TurnAngle:=Player.TurnAngle+(result.dX*CamSpeed);
 if GLSCamera.PitchAngle>MaxCamAngle then
     GLSCamera.PitchAngle:=MaxCamAngle;
 if GLSCamera.PitchAngle<-MaxCamAngle then
     GLSCamera.PitchAngle:=-MaxCamAngle;
 SetCursorPos(Px,Py);
 result.isMove:=true;
end;

function TSceneNewtonWorld.FPSApplyKeyMove(Player:TGLBaseSceneObject;
KeyUP,KeyDOWN,KeyLEFT,KeyRIGHT:integer;Jump:boolean):bool;
label l1;
var
 FI,F,FJ:TVector4f;
 Mat:TMatrix;
begin
 if (not isKeyDown(KeyUP)) and (not isKeyDown(KeyDOWN)) and (not isKeyDown(KeyLEFT))
       and (not isKeyDown(KeyRIGHT)) and (not Jump) then
        begin
          result:=false;
          goto l1;
        end;
        result:=true;
 F:=vectormake(0,0,0);
 if isKeyDown(KeyUP) then begin
    FI:=Player.LocalToAbsolute(vectormake(0,0,-PlayerSpeed));
    AddVector(F,FI);
  end;
  if isKeyDown(KeyDOWN) then begin
    FI:=Player.LocalToAbsolute(vectormake(0,0,PlayerSpeed));
    AddVector(F,FI);
  end;
  if isKeyDown(KeyLEFT) then begin
    FI:=Player.LocalToAbsolute(vectormake(PlayerSpeed,0,0));
    AddVector(F,FI);
  end;
  if isKeyDown(KeyRIGHT) then begin
    FI:=Player.LocalToAbsolute(vectormake(-PlayerSpeed,0,0));
    AddVector(F,FI);
  end;
  if Jump then begin 
  FI:=Player.AbsolutePosition;
  if (F[0]>0) or (F[2]>0) then
   FJ:=vectormake(0,PlayerSpeed*JumpSpeed*2,0)
  else
   FJ:=vectormake(0,JumpSpeed,0);
  NewtonBodyAddImpulse(NewtonBodyPlayer,@FJ,@FI);
  end;
  if (not isKeyDown(KeyUP)) and (not isKeyDown(KeyDOWN)) and (not isKeyDown(KeyLEFT))
       and (not isKeyDown(KeyRIGHT)) then
  begin
   NewtonBodyGetVelocity(NewtonBodyPlayer,@FI);
   F:=vectormake(0,FI[1],0);
   NewtonBodySetVelocity(NewtonBodyPlayer,@F);
  end else begin
   l1:
   NewtonBodyGetVelocity(NewtonBodyPlayer,@FI);
   if FI[1]>JumpSpeed then F[1]:=JumpSpeed else F[1]:=FI[1];
   NewtonBodySetVelocity(NewtonBodyPlayer,@F);
  end;
  NewtonBodyGetMatrix(NewtonBodyPlayer,@Mat);
  mat[0]:=Player.Matrix[0];
  mat[1]:=Player.Matrix[1];
  mat[2]:=Player.Matrix[2];
  NewtonBodySetMatrix(NewtonBodyPlayer,@mat);
  Player.Matrix:=Mat;
end;

procedure TSceneNewtonWorld.DestroySceneNewtonWorld;
begin
 ClearBodyList;
 FreeAndNil(NewtonBodyList);
 NewtonDestroy(SceneNewtonWorld);
end;

function TSceneNewtonWorld.Pi2Gr(Gr:single):single;
begin
 result:=(pi*Gr)/180;
end;

procedure Debug_ShowGeometryCollision
(const Body : PNewtonBody; VertexCount : Integer;
 const FaceArray : PFloat; FaceId : int); cdecl;
var
   i: Integer;
   v0,v1: array[0..2] of Single;
   vA: array of Single;
begin
   if VertexCount = 0 then exit;
   SetLength(vA, VertexCount*3);
   Move(FaceArray^, vA[0], VertexCount*3*SizeOf(Single));
   v0[0] := vA[(VertexCount-1)*3];
   v0[1] := vA[(VertexCount-1)*3+1];
   v0[2] := vA[(VertexCount-1)*3+2];
   for i := 0 to VertexCount-1 do begin
     v1[0] := vA[i*3];
     v1[1] := vA[i*3+1];
     v1[2] := vA[i*3+2];
     glVertex3f(v0[0], v0[1], v0[2]);
     glVertex3f(v1[0], v1[1], v1[2]);
     v0:=v1;
   end;
end;

procedure TSceneNewtonWorld.Debug_ShowBodyCollision(const Body : PNewtonBody); cdecl;
var
m: Tmatrix;
begin
 NewtonBodyGetMatrix(body, @M[0,0]);
 NewtonCollisionForEachPolygonDo(NewtonBodyGetCollision(body),@m,
  @Debug_ShowGeometryCollision,nil);
end;

constructor TSceneNewtonWorld.Create(Friction:integer;Solver:integer;
                                WorldSizeFrom,WorldSizeTo:TVector3f);
begin
 if Assigned(SceneNewtonWorld) then
  DestroySceneNewtonWorld;
 NewtonGravity:=-10;
 JumpSpeed:=4;
 PlayerSpeed:=4;
 SceneNewtonWorld:=NewtonCreate(nil,nil);
 NewtonSetWorldSize(SceneNewtonWorld,@WorldSizeFrom,@WorldSizeTo);
 NewtonSetSolverModel(SceneNewtonWorld,Solver);
 NewtonSetFrictionModel(SceneNewtonWorld,Friction);
 NewtonBodyList:=TList.Create;
 InitMaterials;
end;

procedure TSceneNewtonWorld.ClearBodyList;
begin
 NewtonMaterialDestroyAllGroupID(SceneNewtonWorld);
 NewtonDestroyAllBodies(SceneNewtonWorld);
 NewtonBodyList.Clear;
end;

procedure NewtonApplyForceAndTorque
( const body : PNewtonBody; timestep : Float; threadIndex : int ); cdecl;
var
 M : Single;
 F : TVector4f;
 I:Tvector4f;
begin
 NewtonBodyGetVelocity(body,@I);
 NewtonBodySetVelocity(body,@I);
 NewtonBodyGetMassMatrix(Body, @M, @I[0], @I[1], @I[2]);
 F:=vectormake(0, NewtonGravity*m, 0);
 NewtonBodyAddForce(Body, @F[0]);
end;

procedure TSceneNewtonWorld.CreateConvexBody(Mesh:TGLFreeForm;Matrix:TMatrix);
var
 Cloud:array of TAffineVector;
 i:integer;
 col:pnewtoncollision;
 BBox,Inertia:TAffineVector;
 CM:TVector4f;
 Weight:Single;
begin
 SetLength(Cloud,Mesh.MeshObjects[0].Vertices.Count);
    for i:=0 to Mesh.MeshObjects[0].Vertices.Count-1 do begin
      Cloud[i][0]:=Mesh.MeshObjects[0].Vertices[i][0];
      Cloud[i][1]:=Mesh.MeshObjects[0].Vertices[i][1];
      Cloud[i][2]:=Mesh.MeshObjects[0].Vertices[i][2];
    end;
 col:=NewtonCreateConvexHull(SceneNewtonWorld,
 Mesh.MeshObjects[0].Vertices.Count,@Cloud[0],SizeOf(TAffineVector),0,0,nil);
 NewtonBodyList.Add(NewtonCreateBody(SceneNewtonWorld,col));
 NewtonReleaseCollision(SceneNewtonWorld,Col);
 BBox:=CalculateBBox(Mesh);
 Weight:=BBox[0]*BBox[1]*BBox[2];
 Inertia[0]:= Weight * (BBox[1] * BBox[1] + BBox[2] * BBox[2]) / 12;
 Inertia[1]:= Weight * (BBox[0] * BBox[0] + BBox[2] * BBox[2]) / 12;
 Inertia[2]:= Weight * (BBox[0] * BBox[0] + BBox[1] * BBox[1]) / 12;
 NewtonBodySetMassMatrix(PNewtonBody(NewtonBodyList.Last),
 Weight,Inertia[0],Inertia[1],Inertia[2]);
 NewtonBodySetMatrix(PNewtonBody(NewtonBodyList.Last),@Matrix);
 NewtonBodySetForceAndTorqueCallback(PNewtonBody(NewtonBodyList.Last)
   ,NewtonApplyForceAndTorque);
 NewtonBodySetLinearDamping(PNewtonBody(NewtonBodyList.Last),1);
 CM:=CalculateVerticesMass(Mesh);
 NewtonBodySetCentreOfMass(PNewtonBody(NewtonBodyList.Last),@CM);
 NewtonBodySetUserData(PNewtonBody(NewtonBodyList.Last),Mesh);
 NewtonBodySetTransformCallback(PNewtonBody(NewtonBodyList.Last),@ApplyMatrixes);
end;

procedure TSceneNewtonWorld.CreateTreeBody(Mesh:TGLFreeForm;Matrix:TMatrix);
var
 Col:PNewtonCollision;
 List:TAffineVectorList;
 Faces:TMatrix3f;
 i:integer;
begin
 Col:=NewtonCreateTreeCollision(SceneNewtonWorld,0);
 NewtonTreeCollisionBeginBuild(col);
 list:=Mesh.MeshObjects.ExtractTriangles;
 if list.Count > 0 then
  begin
   i:=0;
   repeat
    Faces[0]:=list[i];
    Faces[1]:=list[i+1];
    Faces[2]:=list[i+2];
    NewtonTreeCollisionAddFace(Col, 3, @Faces[0], SizeOf(TAffineVector), 1);
    inc(i,3);
   until i> list.Count-1;
  end;
 list.Free;
 NewtonTreeCollisionEndBuild(col,1);
 NewtonBodyList.Add(NewtonCreateBody(SceneNewtonWorld, Col));
 NewtonReleaseCollision(SceneNewtonWorld, Col);
 NewtonbodySetMatrix(PNewtonBody(NewtonBodyList.Last),@Matrix);
end;

procedure NullNewtonContactsProcess(
 const contact : PNewtonJoint; timestep : Float; threadIndex : int ); cdecl;
var
 body1,body2:Pnewtonbody;
 ID1,ID2:integer;
begin
 body1:=NewtonJointGetBody0(contact);
 body2:=NewtonJointGetBody1(contact);
 ID1:=NewtonBodyGetMaterialGroupID(body1);
 ID2:=NewtonBodyGetMaterialGroupID(body2);
{if ID1=PlayerID then
так например можно получить тела сталкивающиеся,
 потом уже можно получить все что хочеш
можно например получить материал NewtonBodyGetMaterialGroupID(body1)}
end;

procedure TSceneNewtonWorld.CreateConvexBodyVBO(VBOMesh:TVBOMesh;Weight:real;
  Exceptions:TObjectExceptionsVBO=nil);
var
 Cloud:array of TAffineVector;
 i,j:integer;
 MeshMemory:TList;
 Matrix:TMatrix;
 col:pnewtoncollision;
 BBox,Inertia:TAffineVector;
 CM:TVector4f;
 buff:PVBOBuffer;
begin
 for j:=0 to VBOMesh.Count-1 do begin
  if Exceptions<>nil then
   if Exceptions[j]<>2 then Continue;
  MeshMemory:=VBOMesh.ObjectsList[j].MeshList;
  Matrix:=VBOMesh.ObjectsList[j].Matrices.WorldMatrix;
  buff:=MeshMemory[0];
  SetLength(Cloud,buff.VertexCount);
  for i:=0 to buff.VertexCount-1 do begin
      Cloud[i][0]:=buff.Vertexes[i][0];
      Cloud[i][1]:=buff.Vertexes[i][1];
      Cloud[i][2]:=buff.Vertexes[i][2];
  end;
  col:=NewtonCreateConvexHull(SceneNewtonWorld,
  buff.VertexCount,@Cloud[0],SizeOf(TAffineVector),0,0,nil);
  NewtonBodyList.Add(NewtonCreateBody(SceneNewtonWorld,col));
  NewtonReleaseCollision(SceneNewtonWorld,Col);
  Inertia[0]:= Weight * (BBox[1] * BBox[1] + BBox[2] * BBox[2]) / 12;
  Inertia[1]:= Weight * (BBox[0] * BBox[0] + BBox[2] * BBox[2]) / 12;
  Inertia[2]:= Weight * (BBox[0] * BBox[0] + BBox[1] * BBox[1]) / 12;
  NewtonBodySetMassMatrix(PNewtonBody(NewtonBodyList.Last),
  Weight,Inertia[0],Inertia[1],Inertia[2]);
  NewtonBodySetMatrix(PNewtonBody(NewtonBodyList.Last),@Matrix);
  NewtonBodySetForceAndTorqueCallback(PNewtonBody(NewtonBodyList.Last)
   ,NewtonApplyForceAndTorque);
  NewtonBodySetLinearDamping(PNewtonBody(NewtonBodyList.Last),1);
  CM:=CalculateVerticesMassVBO(VBOMesh.ObjectsList[j]);
  NewtonBodySetCentreOfMass(PNewtonBody(NewtonBodyList.Last),@CM);
  NewtonBodySetUserData(PNewtonBody(NewtonBodyList.Last),vbomesh.ObjectsList[j]);
  NewtonBodySetTransformCallback(PNewtonBody(NewtonBodyList.Last),@ApplyMatrixesVBO);
 end;
end;

procedure TSceneNewtonWorld.CreateTreeBodyVBO(VBOMesh:TVBOMesh;
Exceptions:TObjectExceptionsVBO=nil);
var
 Col:PNewtonCollision;
 List:TAffineVectorList;
 Faces:TMatrix3f;
 i,j:integer;
 buff:PVBOBuffer;
 MeshList:TList;
 Matrix:TMatrix;
begin
 for j:=0 to VBOMesh.Count-1 do begin
  if Exceptions<>nil then
   if Exceptions[j]<>1 then Continue;
   MeshList:=VBOMesh.ObjectsList[j].MeshList;
  Matrix:=VBOMesh.ObjectsList[j].Matrices.WorldMatrix;
  Col:=NewtonCreateTreeCollision(SceneNewtonWorld,0);
  NewtonTreeCollisionBeginBuild(col);
  List:=TAffineVectorList.Create;
  for i:=0 to MeshList.Count-1 do begin
   buff:=MeshList[i];
   ExtractTriangles(buff^,List);
  end;
  if List.Count > 0 then
  begin
   i:=0;
   repeat
    Faces[0]:=list[i];
    Faces[1]:=list[i+1];
    Faces[2]:=list[i+2];
    NewtonTreeCollisionAddFace(Col, 3, @Faces[0], SizeOf(TAffineVector), 1);
    inc(i,3);
   until i> list.Count-1;
  end;
  list.Free;
  NewtonTreeCollisionEndBuild(col,1);
  NewtonBodyList.Add(NewtonCreateBody(SceneNewtonWorld, Col));
  NewtonReleaseCollision(SceneNewtonWorld, Col);
  NewtonbodySetMatrix(PNewtonBody(NewtonBodyList.Last),@Matrix);
 end;
end;

procedure TSceneNewtonWorld.CreatePlayerSphere(Rx,Ry,Rz:real;PlayerMatrix:TMatrix);
var
 Col:PNewtonCollision;
begin
 if NewtonBodyPlayer<>nil then
   NewtonDestroyBody(SceneNewtonWorld,NewtonBodyPlayer);
 Col:=NewtonCreateSphere(SceneNewtonWorld,Rx,Ry,Rz,0,nil);
 NewtonBodyPlayer:=NewtonCreateBody(SceneNewtonWorld, Col);
 NewtonReleaseCollision(SceneNewtonWorld,Col);
 NewtonbodySetMassMatrix(NewtonBodyPlayer,1000,0,0,0);
 NewtonbodySetMatrix(NewtonBodyPlayer,@PlayerMatrix);
 NewtonBodySetLinearDamping(NewtonbodyPlayer,1);
 NewtonBodySetForceAndTorqueCallback(NewtonBodyPlayer
  ,NewtonApplyForceAndTorque);
 NewtonBodySetMaterialGroupID(NewtonbodyPlayer,PlayerID);
end;

procedure TSceneNewtonWorld.InitMaterials;
begin
 DefaultID:=NewtonMaterialGetDefaultGroupID(SceneNewtonWorld);
 PlayerID:=NewtonMaterialCreateGroupID(SceneNewtonWorld);
 NewtonMaterialSetDefaultCollidable(SceneNewtonWorld,PlayerID,DefaultID,1);
 NewtonMaterialSetDefaultSoftness(SceneNewtonWorld,PlayerID,DefaultID,0);
 NewtonMaterialSetDefaultFriction(SceneNewtonWorld,PlayerID,DefaultID,0.01,0.01);
 NewtonMaterialSetDefaultElasticity(SceneNewtonWorld,PlayerID,DefaultID,0);
 NewtonMaterialSetCollisionCallback(SceneNewtonWorld,PlayerID,
  DefaultID,nil, nil,@NullNewtonContactsProcess);
end;

procedure TSceneNewtonWorld.AddDynamicBox(X,Y,Z:real;Obj:TGLBaseSceneObject);
var
 Col:PNewtonCollision;
 Matrix:TMatrix;
begin
 Matrix:=Obj.Matrix;
 Col:=NewtonCreateBox(SceneNewtonWorld,X,Y,Z,0,nil);
 NewtonBodyList.Add(NewtonCreateBody(SceneNewtonWorld, Col));
 NewtonReleaseCollision(SceneNewtonWorld,Col);
 NewtonbodySetMassMatrix(PNewtonBody(NewtonBodyList.Last),10,1,1,1);
 NewtonbodySetMatrix(PNewtonBody(NewtonBodyList.Last),@Matrix);
 NewtonBodySetLinearDamping(PNewtonBody(NewtonBodyList.Last),1);
 NewtonBodySetForceAndTorqueCallback(PNewtonBody(NewtonBodyList.Last)
  ,NewtonApplyForceAndTorque);
 NewtonBodySetUserData(PNewtonBody(NewtonBodyList.Last),Obj);
 NewtonBodySetTransformCallback(PNewtonBody(NewtonBodyList.Last),@ApplyMatrixes);
end;

procedure TSceneNewtonWorld.AddDynamicSphere(X,Y,Z:real;Obj:TGLBaseSceneObject);
var
 Col:PNewtonCollision;
 Matrix:TMatrix;
begin
 Matrix:=Obj.Matrix;
 Col:=NewtonCreateSphere(SceneNewtonWorld,X,Y,Z,0,nil);
 NewtonBodyList.Add(NewtonCreateBody(SceneNewtonWorld, Col));
 NewtonReleaseCollision(SceneNewtonWorld,Col);
 NewtonbodySetMassMatrix(PNewtonBody(NewtonBodyList.Last),10,1,1,1);
 NewtonbodySetMatrix(PNewtonBody(NewtonBodyList.Last),@Matrix);
 NewtonBodySetLinearDamping(PNewtonBody(NewtonBodyList.Last),1);
 NewtonBodySetForceAndTorqueCallback(PNewtonBody(NewtonBodyList.Last)
  ,NewtonApplyForceAndTorque);
 NewtonBodySetUserData(PNewtonBody(NewtonBodyList.Last),Obj);
 NewtonBodySetTransformCallback(PNewtonBody(NewtonBodyList.Last),@ApplyMatrixes);
end;

procedure TSceneNewtonWorld.AddBox4Bot(X,Y,Z:real;Obj:TGLBaseSceneObject);
var
 Col:PNewtonCollision;
 Matrix:TMatrix;
begin
 Matrix:=Obj.Matrix;
 Col:=NewtonCreateBox(SceneNewtonWorld,X,Y,Z,0,nil);
 NewtonBodyList.Add(NewtonCreateBody(SceneNewtonWorld, Col));
 NewtonReleaseCollision(SceneNewtonWorld,Col);
 NewtonbodySetMassMatrix(PNewtonBody(NewtonBodyList.Last),10,1,1,1);
 NewtonbodySetMatrix(PNewtonBody(NewtonBodyList.Last),@Matrix);
 NewtonBodySetLinearDamping(PNewtonBody(NewtonBodyList.Last),1);
 NewtonBodySetForceAndTorqueCallback(PNewtonBody(NewtonBodyList.Last)
  ,NewtonApplyForceAndTorque);
 NewtonBodySetUserData(PNewtonBody(NewtonBodyList.Last),Obj);
 NewtonBodySetTransformCallback(PNewtonBody(NewtonBodyList.Last),@ApplyMatrixes2Bot);
end;

procedure TSceneNewtonWorld.AddSphere4Bot(X,Y,Z:real;Obj:TGLBaseSceneObject);
var
 Col:PNewtonCollision;
 Matrix:TMatrix;
begin
 Matrix:=Obj.Matrix;
 Col:=NewtonCreateSphere(SceneNewtonWorld,X,Y,Z,0,nil);
 NewtonBodyList.Add(NewtonCreateBody(SceneNewtonWorld, Col));
 NewtonReleaseCollision(SceneNewtonWorld,Col);
 NewtonbodySetMassMatrix(PNewtonBody(NewtonBodyList.Last),10,1,1,1);
 NewtonbodySetMatrix(PNewtonBody(NewtonBodyList.Last),@Matrix);
 NewtonBodySetLinearDamping(PNewtonBody(NewtonBodyList.Last),1);
 NewtonBodySetForceAndTorqueCallback(PNewtonBody(NewtonBodyList.Last)
  ,NewtonApplyForceAndTorque);
 NewtonBodySetUserData(PNewtonBody(NewtonBodyList.Last),Obj);
 NewtonBodySetTransformCallback(PNewtonBody(NewtonBodyList.Last),@ApplyMatrixes2Bot);
end;

procedure TSceneNewtonWorld.SetPlayerPosAndAngle(Pos,Angle:TVector4f);
var
 TmpObj:TGLCube;
 M:TMatrix;
begin
 TmpObj:=TGLCube.Create(nil);
 TmpObj.Position.AsVector:=Pos;
 TmpObj.TurnAngle:=Angle[0];
 TmpObj.PitchAngle:=Angle[1];
 TmpObj.RollAngle:=Angle[2];
 M:=TmpObj.AbsoluteMatrix;
 NewtonBodySetMatrix(NewtonBodyPlayer,@M);
 TmpObj.Free;
end;

end.
