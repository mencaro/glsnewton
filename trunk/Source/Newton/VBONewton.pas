//  VBONewton by For[)

unit VBONewton;

interface

uses
  Windows, Classes, GLScene, GLObjects, GLRenderContextInfo, OpenGL1X,
  VectorGeometry, VectorTypes, VectorLists,
  CalculatePhysics, uVBO, VBOMesh, NewtonImport;

type
  TCamMoveResult = record
    isMove: bool;
    dX, dY: integer;
  end;

type TIntBool=0..1;

type TNewtonMaterial = record
   Softness, StaticFriction, KineticFriction, Elasticity: real;
   isCollidable: TIntBool;
   CollisionCallBack: PNewtonContactsProcess;
  end;

const maxBodes=10000;

var NewtonGravity:real=-10;

function isKeyDown(Key:Byte): boolean;
function Gr2Rad(Gr: single): single;
procedure Debug_ShowBodyCollision(const Body: PNewtonBody); cdecl;
procedure Debug_ShowGeometryCollision(const Body: PNewtonBody;
  VertexCount: integer; const FaceArray: PFloat; FaceId: int); cdecl;
function NewtonMaterialMake(isCollidable:TIntBool;Softness,StaticFriction,
      KineticFriction,Elasticity:real;CollisionCallBack:PNewtonContactsProcess=nil):TNewtonMaterial;
procedure DefaultNewtonContactsProcess(const contact: PNewtonJoint;
  timestep: Float; threadIndex: int); cdecl;
//

type TVBONewtonMesh = class
   private
    function GetBodyMatrix:TMatrix;
    procedure SetBodyMatrix(Matrix:TMatrix);
    function GetBodyDirection:TVector4f;
    procedure SetBodyDirection(Direction:TVector4f);
    function GetBodyPosition:TVector4f;
    procedure SetBodyPosition(Position:TVector4f);
    function GetBodyMass:Real;
    procedure SetBodyMass(Mass:Real);
   public
    NewtonBody: PNewtonBody;
    NewtonWorld : PNewtonWorld;
    MeshObject : TVBOMeshObject;
    MaterialID : Integer;
    property BodyMass:Real read GetBodyMass write SetBodyMass;
    property BodyDirection:TVector4f read GetBodyDirection write SetBodyDirection;
    property BodyPosition:TVector4f read GetBodyPosition write SetBodyPosition;
    property BodyMatrix:TMatrix read GetBodyMatrix write SetBodyMatrix;
    constructor Create; overload;
    destructor Destroy; override;
  end;

type TVBONewtonWorld = class
  private
    FDummy : TGLDummyCube;
    DirectOpenGL: TGLDirectOpenGL;
    FMeshBodyList: TList;
    SceneNewtonWorld: PNewtonWorld;
    function GetObjectsCount:Integer;
    function GetObject(Index: Integer):TVBONewtonMesh;
    procedure DebugRender(Sender: TObject; var rci: TRenderContextInfo);
  public
    Player : TVBONewtonMesh;
    Camera: TGLCamera;
    CamSpeed: real;
    CamMaxAngle: real;
    Px, Py: integer;
    JumpSpeed, PlayerSpeed: real;
    DebugGeometry: boolean;
    DefaultWorldMaterialID : Integer;
    property NewtonObjects[Index: Integer]:TVBONewtonMesh read GetObject;
    property NewtonObjectsCount:integer read GetObjectsCount;
    // Dynamic
    function AddConvexBody(VBOMeshObject:TVBOMeshObject;Weight:real=1;
             noUpdateRMatrix:boolean=false):TVBONewtonMesh;
    // Static
    function AddTreeBody(VBOMeshObject:TVBOMeshObject):TVBONewtonMesh;
    procedure CreatePlayer(VBOMeshObject:TVBOMeshObject);
    function FPSApplyCamMove: TCamMoveResult;
    function FPSApplyKeyMove(KeyUP, KeyDOWN, KeyLEFT, KeyRIGHT: Byte;
      Jump: boolean): bool;
    procedure SetMaterialBetweenWorldAndPlayer(NewtonMaterial:TNewtonMaterial);
    procedure SetMaterialBetween2Meshes(Mesh1,Mesh2:TVBONewtonMesh;NewtonMaterial:TNewtonMaterial);
    procedure UpdateWorld(time:real);
    constructor Create(MeshPlayer:TVBOMeshObject; GLScene: TGLScene; Friction: integer; Solver: integer;
      WorldSizeFrom, WorldSizeTo: TVector3f); overload;
    destructor Destroy; override;
  end;

implementation

function TVBONewtonMesh.GetBodyMass:Real;
begin
  NewtonBodyGetMassMatrix(NewtonBody, @result, nil, nil, nil);
end;

procedure TVBONewtonMesh.SetBodyMass(Mass:Real);
begin
  NewtonBodySetMassMatrix(NewtonBody, Mass, 1, 1, 1);
end;

function TVBONewtonMesh.GetBodyMatrix:TMatrix;
begin
  NewtonBodyGetMatrix(NewtonBody,@result);
end;

procedure TVBONewtonMesh.SetBodyMatrix(Matrix:TMatrix);
begin
  NewtonBodySetMatrix(NewtonBody,@Matrix);
end;

function TVBONewtonMesh.GetBodyDirection:TVector4f;
var M:TMatrix;
begin
  NewtonBodyGetMatrix(NewtonBody,@M);
  result:=M[2];
end;

function TVBONewtonMesh.GetBodyPosition:TVector4f;
var M:TMatrix;
begin
  NewtonBodyGetMatrix(NewtonBody,@M);
  result:=M[3];
end;

procedure TVBONewtonMesh.SetBodyDirection(Direction: TVector4f);
var M:TMatrix;
begin
  NewtonBodyGetMatrix(NewtonBody,@M);
  M[2]:=Direction;
  NewtonBodySetMatrix(NewtonBody,@M);
end;

procedure TVBONewtonMesh.SetBodyPosition(Position: TVector4f);
var M:TMatrix;
begin
  NewtonBodyGetMatrix(NewtonBody,@M);
  M[3]:=Position;
  NewtonBodySetMatrix(NewtonBody,@M);
end;

constructor TVBONewtonMesh.Create;
begin
  inherited;
end;

destructor TVBONewtonMesh.Destroy;
begin
  NewtonDestroyBody(NewtonWorld,NewtonBody);
  inherited;
end;

//

function isKeyDown(Key:Byte):boolean;
begin
  if GetAsyncKeyState(Key)<0 then
   result:=true
  else
   result:=false;
end;

procedure DefaultNewtonContactsProcess(const contact: PNewtonJoint;
  timestep: Float; threadIndex: int); cdecl;
var
  body1, body2: PNewtonBody;
  ID1, ID2: integer;
begin
  body1 := NewtonJointGetBody0(contact);
  body2 := NewtonJointGetBody1(contact);
  ID1 := NewtonBodyGetMaterialGroupID(body1);
  ID2 := NewtonBodyGetMaterialGroupID(body2);
  { так например можно получить тела сталкивающиеся,
    потом уже можно получить все что хочеш
    можно например получить материал NewtonBodyGetMaterialGroupID(body1)
    if ID1=PlayerID then ... }
end;

function NewtonMaterialMake(isCollidable:TIntBool;Softness,StaticFriction,
      KineticFriction,Elasticity:real;CollisionCallBack:PNewtonContactsProcess=nil):TNewtonMaterial;
begin
  result.isCollidable:= isCollidable;
  result.Softness:= Softness;
  result.StaticFriction:= StaticFriction;
  result.KineticFriction:= KineticFriction;
  result.Elasticity:= Elasticity;
  result.CollisionCallBack:= CollisionCallBack;
end;

function Gr2Rad(Gr: single): single;
begin
  result := (pi * Gr) / 180;
end;

procedure Debug_ShowGeometryCollision(const Body: PNewtonBody;
  VertexCount: integer; const FaceArray: PFloat; FaceId: int); cdecl;
var
  i: integer;
  v0, v1: array [0 .. 2] of single;
  vA: array of single;
begin
  if VertexCount = 0 then
    exit;
  SetLength(vA, VertexCount * 3);
  Move(FaceArray^, vA[0], VertexCount * 3 * SizeOf(single));
  v0[0] := vA[(VertexCount - 1) * 3];
  v0[1] := vA[(VertexCount - 1) * 3 + 1];
  v0[2] := vA[(VertexCount - 1) * 3 + 2];
  for i := 0 to VertexCount - 1 do
  begin
    v1[0] := vA[i * 3];
    v1[1] := vA[i * 3 + 1];
    v1[2] := vA[i * 3 + 2];
    glVertex3f(v0[0], v0[1], v0[2]);
    glVertex3f(v1[0], v1[1], v1[2]);
    v0 := v1;
  end;
end;

procedure Debug_ShowBodyCollision(const Body: PNewtonBody); cdecl;
var
  m: TMatrix;
begin
  NewtonBodyGetMatrix(Body, @m[0, 0]);
  NewtonCollisionForEachPolygonDo(NewtonBodyGetCollision(Body), @m,
    @Debug_ShowGeometryCollision, nil);
end;

procedure ApplyMatrixes2BotVBO(const Body: PNewtonBody; const matrix: PFloat;
  threadIndex: int); cdecl;
var
  Obj: TVBOMeshObject;
  Mat: TMatrix;
begin
  Obj := TVBOMeshObject(NewtonBodyGetUserData(Body));
  NewtonBodyGetMatrix(Body, @Mat);
  Mat[0] := Obj.Matrices.WorldMatrix[0];
  Mat[1] := Obj.Matrices.WorldMatrix[1];
  Mat[2] := Obj.Matrices.WorldMatrix[2];
  NewtonBodySetMatrix(Body, @Mat);
  Obj.ResetMatrices;
  Obj.Matrices.ModelMatrix:= Mat;
  Obj.UpdateWorldMatrix;
end;

procedure ApplyMatrixesVBO(const Body: PNewtonBody; const matrix: PFloat;
  threadIndex: int); cdecl;
var
  Mat: TMatrix;
  Mesh: TVBOMeshObject;
begin
  Mesh := TVBOMeshObject(NewtonBodyGetUserData(Body));
  NewtonBodyGetMatrix(Body, @Mat);
  Mesh.ResetMatrices;
  Mesh.Matrices.ModelMatrix:= Mat;
  Mesh.UpdateWorldMatrix;
end;

procedure NewtonApplyForceAndTorque(const Body: PNewtonBody; timestep: Float;
  threadIndex: int); cdecl;
var
  m: single;
  F: TVector4f;
  i: TVector4f;
begin
  NewtonBodyGetMassMatrix(Body, @m, @i[0], @i[1], @i[2]);
  F := vectormake(0, NewtonGravity * m, 0);
  NewtonBodyAddForce(Body, @F[0]);
end;

//

constructor TVBONewtonWorld.Create(MeshPlayer:TVBOMeshObject; GLScene: TGLScene; Friction: integer; Solver: integer;
      WorldSizeFrom, WorldSizeTo: TVector3f);
begin
  FMeshBodyList:= TList.Create;
  SceneNewtonWorld := NewtonCreate(nil, nil);
  NewtonSetWorldSize(SceneNewtonWorld, @WorldSizeFrom, @WorldSizeTo);
  NewtonSetSolverModel(SceneNewtonWorld, Solver);
  NewtonSetFrictionModel(SceneNewtonWorld, Friction);
  DirectOpenGL := TGLDirectOpenGL.CreateAsChild(GLScene.Objects);
  DirectOpenGL.OnRender := DebugRender;
  DebugGeometry := false;
  CamSpeed := 0.2;
  CamMaxAngle := 70;
  JumpSpeed := 4;
  PlayerSpeed := 4;
  CreatePlayer(MeshPlayer);
  DefaultWorldMaterialID := NewtonMaterialGetDefaultGroupID(SceneNewtonWorld);
  FDummy := TGLDummyCube.CreateAsChild(GLScene.Objects);
  Camera := TGLCamera.CreateAsChild(FDummy);
end;

destructor TVBONewtonWorld.Destroy;
var t: 0 .. maxBodes;
begin
  Player.Destroy;
  for t:=0 to NewtonObjectsCount-1 do
   NewtonObjects[t].Free;
  NewtonDestroyAllBodies(SceneNewtonWorld);
  NewtonMaterialDestroyAllGroupID(SceneNewtonWorld);
  NewtonDestroy(SceneNewtonWorld);
  FMeshBodyList.Clear;
  FreeAndNil(FMeshBodyList);
  FreeAndNil(DirectOpenGL);
  inherited;
end;

function TVBONewtonWorld.GetObject(Index: Integer):TVBONewtonMesh;
begin
  result:=FMeshBodyList[Index];
end;

function TVBONewtonWorld.FPSApplyCamMove: TCamMoveResult;
var cPos: TPoint;
begin
  GetCursorPos(cPos);
  if (cPos.X = Px) and (cPos.Y = Py) then
  begin
    result.isMove := false;
    exit;
  end;
  result.dX := Px - cPos.X;
  result.dY := Py - cPos.Y;
  FDummy.TurnAngle := FDummy.TurnAngle + (result.dX * CamSpeed);
  Camera.PitchAngle := Camera.PitchAngle + (result.dY * CamSpeed);
  if Camera.PitchAngle > CamMaxAngle then
    Camera.PitchAngle := CamMaxAngle;
  if Camera.PitchAngle < -CamMaxAngle then
    Camera.PitchAngle := -CamMaxAngle;
  SetCursorPos(Px, Py);
  result.isMove := true;
end;

function TVBONewtonWorld.FPSApplyKeyMove
  (KeyUP, KeyDOWN, KeyLEFT, KeyRIGHT: Byte; Jump: boolean): bool;
label l1;
var
  FI, F, FJ: TVector4f;
begin
  if (not isKeyDown(KeyUP)) and (not isKeyDown(KeyDOWN)) and
    (not isKeyDown(KeyLEFT)) and (not isKeyDown(KeyRIGHT)) and (not Jump) then
  begin
    result := false;
    goto l1;
  end;
  result := true;
  F := vectormake(0, 0, 0);
  if isKeyDown(KeyUP) then
  begin
    FI := VectorTransform(vectormake(0,0,-PlayerSpeed),
             FDummy.Matrix);
    AddVector(F, FI);
  end;
  if isKeyDown(KeyDOWN) then
  begin
    FI := VectorTransform(vectormake(0,0,PlayerSpeed),
             FDummy.Matrix);
    AddVector(F, FI);
  end;
  if isKeyDown(KeyLEFT) then
  begin
    FI := VectorTransform(vectormake(PlayerSpeed,0,0),
             FDummy.Matrix);
    AddVector(F, FI);
  end;
  if isKeyDown(KeyRIGHT) then
  begin
    FI := VectorTransform(vectormake(-PlayerSpeed,0,0),
             FDummy.Matrix);
    AddVector(F, FI);
  end;
  if Jump then
  begin
    FI := Player.MeshObject.Position;
    if (F[0] > 0) or (F[2] > 0) then
      FJ := vectormake(0, PlayerSpeed * JumpSpeed * 2, 0)
    else
      FJ := vectormake(0, JumpSpeed, 0);
    NewtonBodyAddImpulse(Player.NewtonBody, @FJ, @FI);
  end; l1 :
  if (not isKeyDown(KeyUP)) and (not isKeyDown(KeyDOWN)) and
    (not isKeyDown(KeyLEFT)) and (not isKeyDown(KeyRIGHT)) then
  begin
    NewtonBodyGetVelocity(Player.NewtonBody, @FI);
    F := vectormake(0, FI[1], 0);
    NewtonBodySetVelocity(Player.NewtonBody, @F);
  end
  else
  begin
    NewtonBodyGetVelocity(Player.NewtonBody, @FI);
    if FI[1] > JumpSpeed then
      F[1] := JumpSpeed
    else
      F[1] := FI[1];
    NewtonBodySetVelocity(Player.NewtonBody, @F);
  end;
end;

procedure TVBONewtonWorld.DebugRender(Sender: TObject;
  var rci: TRenderContextInfo);
var
  t: 0 .. maxBodes;
begin
  if not DebugGeometry then
    exit;
  glDisable(GL_LIGHTING);
  glBegin(GL_LINES);
   for t := 0 to FMeshBodyList.Count - 1 do
     Debug_ShowBodyCollision(NewtonObjects[t].NewtonBody);
  glEnd;
  glEnable(GL_LIGHTING);
end;

function TVBONewtonWorld.AddTreeBody(VBOMeshObject:TVBOMeshObject):TVBONewtonMesh;
var
  col: pnewtoncollision;
  List: TAffineVectorList;
  Faces: TMatrix3f;
  i: integer;
  buff: PVBOBuffer;
  MeshList: TList;
  matrix: TMatrix;
begin
    MeshList := VBOMeshObject.MeshList;
    matrix := VBOMeshObject.Matrices.WorldMatrix;
    col := NewtonCreateTreeCollision(SceneNewtonWorld, 0);
    NewtonTreeCollisionBeginBuild(col);
    List := TAffineVectorList.Create;
    for i := 0 to MeshList.Count - 1 do
    begin
      buff := MeshList[i];
      ExtractTriangles(buff^, List);
    end;
    if List.Count > 0 then
    begin
      i := 0;
      repeat
        Faces[0] := List[i];
        Faces[1] := List[i + 1];
        Faces[2] := List[i + 2];
        NewtonTreeCollisionAddFace(col, 3, @Faces[0], SizeOf(TAffineVector), 1);
        inc(i, 3);
      until i > List.Count - 1;
    end;
    List.Free;
    NewtonTreeCollisionEndBuild(col, 1);
    FMeshBodyList.Add(TVBONewtonMesh.Create);
    NewtonObjects[FMeshBodyList.Count-1].NewtonBody:=NewtonCreateBody(SceneNewtonWorld, col);
    NewtonObjects[FMeshBodyList.Count-1].NewtonWorld:=SceneNewtonWorld;
    NewtonObjects[FMeshBodyList.Count-1].MeshObject:=VBOMeshObject;
    NewtonReleaseCollision(SceneNewtonWorld, col);
    NewtonBodySetMatrix(NewtonObjects[FMeshBodyList.Count-1].NewtonBody, @matrix);
    result:=NewtonObjects[FMeshBodyList.Count-1];
end;

function TVBONewtonWorld.AddConvexBody(VBOMeshObject:TVBOMeshObject;
 Weight:real=1;noUpdateRMatrix:boolean=false):TVBONewtonMesh;
var
  Cloud: array of TAffineVector;
  i: integer;
  MeshMemory: TList;
  matrix: TMatrix;
  col: pnewtoncollision;
  BBox, Inertia, tmpV,tmpV2: TAffineVector;
  CM: TVector4f;
  buff: PVBOBuffer;
begin
    MeshMemory := VBOMeshObject.MeshList;
    matrix := VBOMeshObject.Matrices.WorldMatrix;
    buff := MeshMemory[0];
    SetLength(Cloud, buff.VertexCount);
    for i := 0 to buff.VertexCount - 1 do
    begin
      Cloud[i][0] := buff.Vertexes[i][0];
      Cloud[i][1] := buff.Vertexes[i][1];
      Cloud[i][2] := buff.Vertexes[i][2];
    end;
    tmpV:=VBOMeshObject.Extents.emax;
    tmpV2:=VBOMeshObject.Extents.emin;
    VectorSubtract(tmpV,tmpV2,BBox);
    if Weight=1 then
     Weight:=BBox[0]*BBox[1]*BBox[2];
    col := NewtonCreateConvexHull(SceneNewtonWorld, buff.VertexCount,
      @Cloud[0], SizeOf(TAffineVector), 0, 0, nil);
    FMeshBodyList.Add(TVBONewtonMesh.Create);
    NewtonObjects[FMeshBodyList.Count-1].NewtonBody:=NewtonCreateBody(SceneNewtonWorld, col);
    NewtonObjects[FMeshBodyList.Count-1].NewtonWorld:=SceneNewtonWorld;
    NewtonObjects[FMeshBodyList.Count-1].MeshObject:=VBOMeshObject;
    NewtonReleaseCollision(SceneNewtonWorld, col);
    Inertia[0] := Weight * (BBox[1] * BBox[1] + BBox[2] * BBox[2]) / 12;
    Inertia[1] := Weight * (BBox[0] * BBox[0] + BBox[2] * BBox[2]) / 12;
    Inertia[2] := Weight * (BBox[0] * BBox[0] + BBox[1] * BBox[1]) / 12;
    NewtonBodySetMassMatrix(NewtonObjects[FMeshBodyList.Count-1].NewtonBody, Weight,
      Inertia[0], Inertia[1], Inertia[2]);
    NewtonBodySetMatrix(NewtonObjects[FMeshBodyList.Count-1].NewtonBody, @matrix);
    NewtonBodySetForceAndTorqueCallback(NewtonObjects[FMeshBodyList.Count-1].NewtonBody,
      NewtonApplyForceAndTorque);
    NewtonBodySetLinearDamping(NewtonObjects[FMeshBodyList.Count-1].NewtonBody, 1);
    CM := CalculateVerticesMassVBO(VBOMeshObject);
    NewtonBodySetCentreOfMass(NewtonObjects[FMeshBodyList.Count-1].NewtonBody, @CM);
    NewtonBodySetUserData(NewtonObjects[FMeshBodyList.Count-1].NewtonBody,
      VBOMeshObject);
    if noUpdateRMatrix then
      NewtonBodySetTransformCallback(NewtonObjects[FMeshBodyList.Count-1].NewtonBody,
        ApplyMatrixes2BotVBO)
    else
      NewtonBodySetTransformCallback(NewtonObjects[FMeshBodyList.Count-1].NewtonBody,
        ApplyMatrixesVBO);
     NewtonObjects[FMeshBodyList.Count-1].MaterialID:=NewtonMaterialCreateGroupID(SceneNewtonWorld);
     NewtonBodySetMaterialGroupID(NewtonObjects[FMeshBodyList.Count-1].NewtonBody,
         NewtonObjects[FMeshBodyList.Count-1].MaterialID);
     result:=NewtonObjects[FMeshBodyList.Count-1];
end;

procedure TVBONewtonWorld.UpdateWorld(time:real);
var M:TMatrix;
begin
  NewtonUpdate(SceneNewtonWorld,time);
  NewtonBodyGetMatrix(Player.NewtonBody, @M);
  M[0]:= FDummy.Matrix[0];
  M[1]:= FDummy.Matrix[1];
  M[2]:= FDummy.Matrix[2];
  Player.MeshObject.ResetMatrices;
  Player.MeshObject.Matrices.ModelMatrix:= M;
  Player.MeshObject.UpdateWorldMatrix;
  NewtonBodySetMatrix(Player.NewtonBody, @M);
  FDummy.Matrix:=M;
end;

procedure TVBONewtonWorld.SetMaterialBetweenWorldAndPlayer(NewtonMaterial:TNewtonMaterial);
begin
  NewtonMaterialSetDefaultCollidable(SceneNewtonWorld, Player.MaterialID,
      DefaultWorldMaterialID, NewtonMaterial.isCollidable);
  NewtonMaterialSetDefaultSoftness(SceneNewtonWorld, Player.MaterialID,
      DefaultWorldMaterialID, NewtonMaterial.Softness);
  NewtonMaterialSetDefaultFriction(SceneNewtonWorld, Player.MaterialID,
      DefaultWorldMaterialID, NewtonMaterial.StaticFriction, NewtonMaterial.KineticFriction);
  NewtonMaterialSetDefaultElasticity(SceneNewtonWorld, Player.MaterialID,
      DefaultWorldMaterialID, NewtonMaterial.Elasticity);
  if NewtonMaterial.CollisionCallBack=nil then
   NewtonMaterialSetCollisionCallback(SceneNewtonWorld, Player.MaterialID,
      DefaultWorldMaterialID, nil, nil, @DefaultNewtonContactsProcess)
  else
   NewtonMaterialSetCollisionCallback(SceneNewtonWorld, Player.MaterialID,
      DefaultWorldMaterialID, nil, nil, NewtonMaterial.CollisionCallBack);

end;

procedure TVBONewtonWorld.SetMaterialBetween2Meshes(Mesh1,Mesh2:TVBONewtonMesh;
            NewtonMaterial:TNewtonMaterial);
begin
  NewtonMaterialSetDefaultCollidable(SceneNewtonWorld, Mesh1.MaterialID,
      Mesh2.MaterialID, NewtonMaterial.isCollidable);
  NewtonMaterialSetDefaultSoftness(SceneNewtonWorld, Mesh1.MaterialID,
      Mesh2.MaterialID, NewtonMaterial.Softness);
  NewtonMaterialSetDefaultFriction(SceneNewtonWorld, Mesh1.MaterialID,
      Mesh2.MaterialID, NewtonMaterial.StaticFriction, NewtonMaterial.KineticFriction);
  NewtonMaterialSetDefaultElasticity(SceneNewtonWorld, Mesh1.MaterialID,
      Mesh2.MaterialID, NewtonMaterial.Elasticity);
  if NewtonMaterial.CollisionCallBack=nil then
   NewtonMaterialSetCollisionCallback(SceneNewtonWorld, Mesh1.MaterialID,
      Mesh2.MaterialID, nil, nil, @DefaultNewtonContactsProcess)
  else
   NewtonMaterialSetCollisionCallback(SceneNewtonWorld, Mesh1.MaterialID,
      Mesh2.MaterialID, nil, nil, NewtonMaterial.CollisionCallBack);
end;

function TVBONewtonWorld.GetObjectsCount:Integer;
begin
 result:=FMeshBodyList.Count;
end;

procedure TVBONewtonWorld.CreatePlayer(VBOMeshObject:TVBOMeshObject);
var
  Cloud: array of TAffineVector;
  i: integer;
  Weight:real;
  MeshMemory: TList;
  matrix: TMatrix;
  col: pnewtoncollision;
  CM: TVector4f;
  buff: PVBOBuffer;
begin
    MeshMemory := VBOMeshObject.MeshList;
    matrix := VBOMeshObject.Matrices.WorldMatrix;
    buff := MeshMemory[0];
    SetLength(Cloud, buff.VertexCount);
    for i := 0 to buff.VertexCount - 1 do
    begin
      Cloud[i][0] := buff.Vertexes[i][0];
      Cloud[i][1] := buff.Vertexes[i][1];
      Cloud[i][2] := buff.Vertexes[i][2];
    end;
    Weight:=100;
    col := NewtonCreateConvexHull(SceneNewtonWorld, buff.VertexCount,
      @Cloud[0], SizeOf(TAffineVector), 0, 0, nil);
    Player := TVBONewtonMesh.Create;
    Player.NewtonBody:=NewtonCreateBody(SceneNewtonWorld, col);
    Player.NewtonWorld:=SceneNewtonWorld;
    Player.MeshObject:=VBOMeshObject;
    NewtonReleaseCollision(SceneNewtonWorld, col);
    NewtonBodySetMassMatrix(Player.NewtonBody, Weight,
      1, 1, 1);
    NewtonBodySetMatrix(Player.NewtonBody, @matrix);
    NewtonBodySetForceAndTorqueCallback(Player.NewtonBody,
      NewtonApplyForceAndTorque);
    NewtonBodySetLinearDamping(Player.NewtonBody, 1);
    CM := CalculateVerticesMassVBO(VBOMeshObject);
    NewtonBodySetCentreOfMass(Player.NewtonBody, @CM);
    NewtonBodySetUserData(Player.NewtonBody,
      VBOMeshObject);
    Player.MaterialID:=NewtonMaterialCreateGroupID(SceneNewtonWorld);
    NewtonBodySetMaterialGroupID(Player.NewtonBody,
         Player.MaterialID);
end;

end.
