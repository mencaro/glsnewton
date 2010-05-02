//  VBONewton by For[)

unit VBONewton;

interface

uses
  Windows, Classes, GLScene, GLObjects, GLRenderContextInfo, OpenGL1X,
  VectorGeometry, VectorTypes, VectorLists, uVBO, VBOMesh, NewtonImport;

type
  TCamMoveResult = record
    isMove: bool;
    dX, dY: integer;
  end;

type TIntBool = 0..1;

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
function CalculateVerticesMassVBO(Obj:TVBOMeshObject):TVector4f;
//

type TVBONewtonJoint = class
  private
   procedure SetJointCallback(CallBack:NewtonBallCallBack);
  public
   NewtonJoint : PNewtonJoint;
   NewtonWorld : PNewtonWorld;
   property NewtonJointCallback:NewtonBallCallBack write SetJointCallback;
   destructor Destroy; override;
end;

type TVBONewtonMesh = class
   private
    function GetBodyForce:TVector4f;
    procedure SetBodyForce(Force:TVector4f);
    function GetBodyState:VBONewton.TIntBool;
    procedure SetBodyState(State:VBONewton.TIntBool);
    function GetBodyInertion:TVector4f;
    procedure SetBodyInertion(Inertion:TVector4f);
    function GetBodySpeed:TVector4f;
    procedure SetBodySpeed(Speed:TVector4f);
    function GetBodyMatrix:TMatrix;
    procedure SetBodyMatrix(Matrix:TMatrix);
    function GetBodyDirection:TVector4f;
    procedure SetBodyDirection(Direction:TVector4f);
    function GetBodyPosition:TVector4f;
    procedure SetBodyPosition(Position:TVector4f);
    function GetBodyMass:Single;
    procedure SetBodyMass(Mass:Single);
   public
    NewtonBody: PNewtonBody;
    NewtonWorld : PNewtonWorld;
    MeshObject : TVBOMeshObject;
    MaterialID : Integer;
    //  не рекомендуется использовать, т.к. при пользовании проявляются баги
    //  по крайней мере лучше не читать, масса пишется нормально :)
    property BodyMass:Single read GetBodyMass write SetBodyMass;
    property BodyInertion:TVector4f read GetBodyInertion write SetBodyInertion;
    //
    property BodyFrizeed: TIntBool read GetBodyState write SetBodyState;
    property BodySpeed:TVector4f read GetBodySpeed write SetBodySpeed;
    property BodyForce:TVector4f read GetBodyForce write SetBodyForce;
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
    FJoints : TList;
    SceneNewtonWorld: PNewtonWorld;
    function GetObjectsCount:Integer;
    function GetLastObject : TVBONewtonMesh;
    function GetObject(Index: Integer):TVBONewtonMesh;
    function GetJointsCount : Integer;
    function GetLastJoint : TVBONewtonJoint;
    function GetJointObject(Index: Integer) : TVBONewtonJoint;
    procedure DebugRender(Sender: TObject; var rci: TRenderContextInfo);
    procedure CreatePlayer(VBOMeshObject:TVBOMeshObject);
  public
    Player : TVBONewtonMesh;
    Camera: TGLCamera;
    CamSpeed: real;
    CamMaxAngle: real;
    Px, Py: integer;
    JumpSpeed, PlayerSpeed: real;
    DebugGeometry: boolean;
    DefaultWorldMaterialID : Integer;
    // Objects
    property LastObject : TVBONewtonMesh read GetLastObject;
    property NewtonObjects[Index: Integer] : TVBONewtonMesh read GetObject;
    property NewtonObjectsCount:integer read GetObjectsCount;
    // Joints
    property LastJointObject : TVBONewtonJoint read GetLastJoint;
    property NewtonJoints[Index: Integer] : TVBONewtonJoint read GetJointObject;
    property NewtonJointsCount:integer read GetJointsCount;
    // Dynamic
    function AddConvexBody(VBOMeshObject : TVBOMeshObject; Weight:real=1;
             noUpdateRMatrix:boolean=false) : TVBONewtonMesh;
    // Static
    function AddTreeBody(VBOMeshObject:TVBOMeshObject):TVBONewtonMesh;
    // Joints
    function AddHingeJoint(Mesh1,Mesh2:TVBONewtonMesh):TVBONewtonJoint; deprecated;
    function AddBallSocketJoint(Parent,Child:TVBONewtonMesh;
         PivotPoint:TVector4f;State:TINTBool=0):TVBONewtonJoint;
    //
    function FPSApplyCamMove: TCamMoveResult;
    function FPSApplyKeyMove(KeyUP, KeyDOWN, KeyLEFT, KeyRIGHT: Byte;
      Jump: boolean): bool;
    procedure SetMaterialBetweenWorldAndPlayer(NewtonMaterial:TNewtonMaterial);
    procedure SetMaterialBetween2Meshes(Mesh1,Mesh2:TVBONewtonMesh;NewtonMaterial:TNewtonMaterial);
    procedure UpdateWorld(time:real;isApplyForce:boolean=true);
    procedure NewtonObjectsClear;
    constructor Create(MeshPlayer:TVBOMeshObject; GLScene: TGLScene;
          Owner:TGLBaseSceneObject; Friction: integer; Solver: integer;
          WorldSizeFrom, WorldSizeTo: TVector3f); overload;
    destructor Destroy; override;
  end;

implementation

destructor TVBONewtonJoint.Destroy;
begin
  NewtonDestroyJoint(NewtonWorld, NewtonJoint);
  inherited;
end;

function TVBONewtonMesh.GetBodyForce:TVector4f;
begin
  NewtonBodyGetForce(NewtonBody,@result);
end;

procedure TVBONewtonMesh.SetBodyForce(Force:TVector4f);
begin
  NewtonBodySetForce(NewtonBody,@Force);
end;

function TVBONewtonMesh.GetBodyState:TIntBool;
begin
  result:=NewtonBodyGetFreezeState(NewtonBody);
end;

procedure TVBONewtonMesh.SetBodyState(State:TIntBool);
begin
  NewtonBodySetFreezeState(NewtonBody, State);
end;

function TVBONewtonMesh.GetBodyMass:Single;
var I1, I2, I3:real;
begin
  NewtonBodyGetMassMatrix(NewtonBody, @result, @I1, @I2, @I3);
end;

procedure TVBONewtonMesh.SetBodyMass(Mass:Single);
var I1, I2, I3 , Weight:real;
begin
  NewtonBodyGetMassMatrix(NewtonBody, @Weight, @I1, @I2, @I3);
  NewtonBodySetMassMatrix(NewtonBody, Mass, I1, I2, I3);
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
begin
  NewtonBodyGetMassMatrix(Body, @m, @F[0], @F[1], @F[2]);
  F := VectorMake(0, NewtonGravity * m, 0);
  NewtonBodyAddForce(Body, @F);
end;

function CalculateVerticesMassVBO(Obj:TVBOMeshObject):TVector4f;
var i:integer; tx,ty,tz:real; buff:PVBOBuffer;
begin
 buff:=Obj.MeshList[0]; result:=VectorMake(0,0,0); tx:=0; ty:=0; tz:=0;
 for i:=0 to buff.VertexCount-1 do begin
   tx:=tx+buff.Vertexes[i][0];
   ty:=ty+buff.Vertexes[i][1];
   tz:=tz+buff.Vertexes[i][2];
 end;
 result:=VectorMake(tx/buff.VertexCount,ty/buff.VertexCount,tz/buff.VertexCount)
end;

//

constructor TVBONewtonWorld.Create(MeshPlayer:TVBOMeshObject; GLScene: TGLScene;
          Owner:TGLBaseSceneObject; Friction: integer; Solver: integer;
          WorldSizeFrom, WorldSizeTo: TVector3f);
begin
  FMeshBodyList := TList.Create;
  FJoints := TList.Create;
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
  FDummy := TGLDummyCube.CreateAsChild(Owner);
  Camera := TGLCamera.CreateAsChild(FDummy);
end;

destructor TVBONewtonWorld.Destroy;
begin
  FreeAndNil(Player);
  NewtonObjectsClear;
  NewtonDestroy(SceneNewtonWorld);
  FreeAndNil(Camera);
  FreeAndNil(FDummy);
  FreeAndNil(FJoints);
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
  F := VectorMake(0, 0, 0);
  if (not isKeyDown(KeyUP)) and (not isKeyDown(KeyDOWN)) and
    (not isKeyDown(KeyLEFT)) and (not isKeyDown(KeyRIGHT)) and (not Jump) then
  begin
    result := false;
    goto l1;
  end;
  result := true;
  if isKeyDown(KeyUP) then
  begin
    FI := VectorTransform(VectorMake(0,0,-PlayerSpeed),
             FDummy.Matrix);
    AddVector(F, FI);
  end;
  if isKeyDown(KeyDOWN) then
  begin
    FI := VectorTransform(VectorMake(0,0,PlayerSpeed),
             FDummy.Matrix);
    AddVector(F, FI);
  end;
  if isKeyDown(KeyLEFT) then
  begin
    FI := VectorTransform(VectorMake(PlayerSpeed,0,0),
             FDummy.Matrix);
    AddVector(F, FI);
  end;
  if isKeyDown(KeyRIGHT) then
  begin
    FI := VectorTransform(VectorMake(-PlayerSpeed,0,0),
             FDummy.Matrix);
    AddVector(F, FI);
  end;
  if Jump then
  begin
    FI := Player.BodyPosition;
    if (F[0] <> 0) or (F[2] <> 0) then
      FJ := VectorTransform(VectorMake(0, PlayerSpeed * JumpSpeed * 2, 0),
            FDummy.Matrix)
    else
      FJ := VectorTransform(VectorMake(0, JumpSpeed, 0),
            FDummy.Matrix);
    NewtonBodyAddImpulse(Player.NewtonBody, @FJ, @FI);
  end;
   l1 :
    NewtonBodyGetVelocity(Player.NewtonBody, @FJ);
    if FJ[1] > JumpSpeed then
      F[1] := JumpSpeed
    else
      F[1]:=FJ[1];
    NewtonBodySetVelocity(Player.NewtonBody, @F);
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
    NewtonObjects[FMeshBodyList.Count-1].MaterialID:=NewtonMaterialCreateGroupID(SceneNewtonWorld);
     NewtonBodySetMaterialGroupID(NewtonObjects[FMeshBodyList.Count-1].NewtonBody,
         NewtonObjects[FMeshBodyList.Count-1].MaterialID);
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

procedure TVBONewtonWorld.UpdateWorld(time:real;isApplyForce:boolean=true);
var M:TMatrix;
begin
  if isApplyForce then
   NewtonBodySetForceAndTorqueCallback(Player.NewtonBody,
      NewtonApplyForceAndTorque)
  else
   NewtonBodySetForceAndTorqueCallback(Player.NewtonBody,
      nil);
  NewtonUpdate(SceneNewtonWorld, time);
  NewtonBodyGetMatrix(Player.NewtonBody, @M);
  M[0]:= FDummy.Matrix[0];
  M[1]:= FDummy.Matrix[1];
  M[2]:= FDummy.Matrix[2];
  Player.MeshObject.ResetMatrices;
  Player.MeshObject.Matrices.ModelMatrix:= M;
  Player.MeshObject.UpdateWorldMatrix;
  NewtonBodySetMatrix(Player.NewtonBody, @M);
  FDummy.Matrix:= M;
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
    NewtonBodySetMassMatrix(Player.NewtonBody, Weight, 1, 1, 1);
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

procedure TVBONewtonWorld.NewtonObjectsClear;
var t: 0 .. maxBodes;
begin
  if NewtonJointsCount>0 then
   for t:=0 to NewtonJointsCount-1 do
     NewtonJoints[t].Free;
  if NewtonObjectsCount>0 then
   for t:=0 to NewtonObjectsCount-1 do
     NewtonObjects[t].Free;
  NewtonMaterialDestroyAllGroupID(SceneNewtonWorld);
  if Player<>nil then begin
   DefaultWorldMaterialID:=NewtonMaterialGetDefaultGroupID(SceneNewtonWorld);
   Player.MaterialID:=NewtonMaterialCreateGroupID(SceneNewtonWorld);
  end;
  FMeshBodyList.Clear;
  FJoints.Clear;
end;

function TVBONewtonWorld.GetLastObject : TVBONewtonMesh;
begin
  result:=TVBONewtonMesh(FMeshBodyList.Last);
end;

function TVBONewtonMesh.GetBodySpeed:TVector4f;
begin
  NewtonBodyGetVelocity(NewtonBody, @result);
end;

procedure TVBONewtonMesh.SetBodySpeed(Speed:TVector4f);
begin
  NewtonBodySetVelocity(NewtonBody, @Speed)
end;

function TVBONewtonMesh.GetBodyInertion:TVector4f;
var Weight:real;
begin
  NewtonBodyGetMassMatrix(NewtonBody, @Weight, @result[0], @result[1], @result[2]);
end;

procedure TVBONewtonMesh.SetBodyInertion(Inertion:TVector4f);
var I1, I2, I3 , Weight : Real;
begin
  NewtonBodyGetMassMatrix(NewtonBody, @Weight, @I1, @I2, @I3);
  NewtonBodySetMassMatrix(NewtonBody, Weight, Inertion[0], Inertion[1], Inertion[2]);
end;

function TVBONewtonWorld.GetJointsCount : Integer;
begin
  result:=FJoints.Count;
end;

function TVBONewtonWorld.GetLastJoint : TVBONewtonJoint;
begin
  result:=TVBONewtonJoint(FJoints.Last);
end;

function TVBONewtonWorld.GetJointObject(Index: Integer) : TVBONewtonJoint;
begin
  result:=FJoints[Index];
end;

function TVBONewtonWorld.AddHingeJoint(Mesh1: TVBONewtonMesh; Mesh2: TVBONewtonMesh):TVBONewtonJoint;
begin
 //
end;

function TVBONewtonWorld.AddBallSocketJoint(Parent: TVBONewtonMesh;
      Child: TVBONewtonMesh; PivotPoint:TVector4f ; State:TINTBool=0):TVBONewtonJoint;
begin
  FJoints.Add(TVBONewtonJoint.Create);
  LastJointObject.NewtonWorld:=SceneNewtonWorld;
  LastJointObject.NewtonJoint:=NewtonConstraintCreateBall(SceneNewtonWorld,
        @PivotPoint,Child.NewtonBody,Parent.NewtonBody);
  NewtonJointSetCollisionState(LastJointObject.NewtonJoint,State);
  result:=LastJointObject;
end;

procedure TVBONewtonJoint.SetJointCallback(CallBack:NewtonBallCallBack);
begin
  NewtonBallSetUserCallback(NewtonJoint,CallBack);
end;

end.
