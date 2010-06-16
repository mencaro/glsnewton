// VBONewton by For[)
// updated 8.06.10
// added :
// FCopy
// SDK_Version
// PlatformArchitecture ( 0 - default, 1 - medium, n - best)
//

unit VBONewton;

interface

uses
  Windows, Classes, GLScene, GLObjects, GLRenderContextInfo, OpenGL1X,
  VectorGeometry, VectorTypes, VectorLists, uVBO, VBOMesh, NewtonImport,
  Controls;

type
  TCamMoveResult = record
    isMove: bool;
    dX, dY: integer;
  end;

type
  TIntBool = 0 .. 1;

type
  TNewtonMaterial = record
    Softness, StaticFriction, KineticFriction, Elasticity, Thickness: real;
    isCollidable: TIntBool;
    CollisionCallBack: PNewtonContactsProcess;
  end;

const
  maxBodes = 10000;

var
  NewtonGravity: real = -10;

function FCopy(str: string; N, K: integer): string;
function isKeyDown(Key: Byte): boolean;
function Gr2Rad(Gr: single): single;
procedure Debug_ShowBodyCollision(const Body: PNewtonBody); cdecl;
procedure Debug_ShowGeometryCollision(const Body: PNewtonBody;
  VertexCount: integer; const FaceArray: PFloat; FaceId: int); cdecl;
function NewtonMaterialMake(isCollidable: TIntBool; Softness, StaticFriction,
  KineticFriction, Elasticity, Thickness: real;
  CollisionCallBack: PNewtonContactsProcess = nil): TNewtonMaterial;
procedure DefaultNewtonContactsProcess(const contact: PNewtonJoint;
  timestep: Float; threadIndex: int); cdecl;
function CalculateVerticesMassVBO(Obj: TVBOMeshObject): TVector4f;
//

type
  TVBONewtonJoint = class
  private
    procedure SetJointCallback(CallBack: NewtonBallCallBack);
  public
    NewtonJoint: PNewtonJoint;
    NewtonWorld: PNewtonWorld;
    property NewtonJointCallback: NewtonBallCallBack write SetJointCallback;
    destructor Destroy; override;
  end;

type
  TVBONewtonMesh = class
  private
    function GetBodyForce: TVector4f;
    procedure SetBodyForce(Force: TVector4f);
    function GetBodyState: VBONewton.TIntBool;
    procedure SetBodyState(State: VBONewton.TIntBool);
    function GetBodyInertion: TVector4f;
    procedure SetBodyInertion(Inertion: TVector4f);
    function GetBodySpeed: TVector4f;
    procedure SetBodySpeed(Speed: TVector4f);
    function GetBodyMatrix: TMatrix;
    procedure SetBodyMatrix(Matrix: TMatrix);
    function GetBodyDirection: TVector4f;
    procedure SetBodyDirection(Direction: TVector4f);
    function GetBodyPosition: TVector4f;
    procedure SetBodyPosition(Position: TVector4f);
    function GetBodyMass: single;
    procedure SetBodyMass(Mass: single);
  public
    NewtonBody: PNewtonBody;
    NewtonWorld: PNewtonWorld;
    MeshObject: TVBOMeshObject;
    MaterialID: integer;
    BBox: TVector3f;
    // не рекомендуется использовать, т.к. при пользовании проявляются баги
    // по крайней мере лучше не читать, масса пишется нормально :)
    property BodyMass: single read GetBodyMass write SetBodyMass;
    property BodyInertion: TVector4f read GetBodyInertion write SetBodyInertion;
    //
    property BodyFrizeed: TIntBool read GetBodyState write SetBodyState;
    property BodySpeed: TVector4f read GetBodySpeed write SetBodySpeed;
    property BodyForce: TVector4f read GetBodyForce write SetBodyForce;
    property BodyDirection: TVector4f read GetBodyDirection write
      SetBodyDirection;
    property BodyPosition: TVector4f read GetBodyPosition write SetBodyPosition;
    property BodyMatrix: TMatrix read GetBodyMatrix write SetBodyMatrix;
    constructor Create; overload;
    destructor Destroy; override;
  end;

type
  TVBONewtonWorld = class
  private
    FMeshBodyList: TList;
    FJoints: TList;
    function GetObjectsCount: integer;
    function GetLastObject: TVBONewtonMesh;
    function GetObject(Index: integer): TVBONewtonMesh;
    function GetJointsCount: integer;
    function GetLastJoint: TVBONewtonJoint;
    function GetJointObject(Index: integer): TVBONewtonJoint;
    function GetSDKVersion: String;
    procedure DebugRender(Sender: TObject; var rci: TRenderContextInfo);
  public
    DirectOpenGL: TGLDirectOpenGL;
    SceneNewtonWorld: PNewtonWorld;
    Player: TVBONewtonMesh;
    Camera: TGLCamera;
    CamSpeed: real;
    CamMaxAngle: real;
    Px, Py: integer;
    JumpSpeed, PlayerSpeed: real;
    DebugGeometry: boolean;
    DebugMode: Cardinal;
    DebugColor: TVector4f;
    DefaultWorldMaterialID: integer;
    FDummy: TGLDummyCube;
    property SDK_Version: String read GetSDKVersion;
    // Objects
    property LastObject: TVBONewtonMesh read GetLastObject;
    property NewtonObjects[Index: integer]: TVBONewtonMesh read GetObject;
    property NewtonObjectsCount: integer read GetObjectsCount;
    // Joints
    property LastJointObject: TVBONewtonJoint read GetLastJoint;
    property NewtonJoints[Index: integer]: TVBONewtonJoint read GetJointObject;
    property NewtonJointsCount: integer read GetJointsCount;
    // Dynamic
    function AddConvexBody(VBOMeshObject: TVBOMeshObject; Weight: real = 1;
      noUpdateRMatrix: boolean = false): TVBONewtonMesh;
    // Static
    function AddTreeBody(VBOMeshObject: TVBOMeshObject): TVBONewtonMesh;
    // Joints
    function AddHingeJoint(Mesh1, Mesh2: TVBONewtonMesh): TVBONewtonJoint;
    deprecated;
    function AddBallSocketJoint(Parent, Child: TVBONewtonMesh;
      PivotPoint: TVector4f; Pin: TVector4f; maxCone, maxTwist: real;
      State: TIntBool = 0): TVBONewtonJoint;
    //
    procedure CreatePlayer(VBOMeshObject: TVBOMeshObject);
    procedure CreatePlayerSphere(X, Y, Z: real; Matrix: TMatrix);
    procedure CreatePlayerBox(X, Y, Z: real; Matrix: TMatrix);
    function FPSApplyCamMove: TCamMoveResult;
    function FPSApplyKeyMove(KeyUP, KeyDOWN, KeyLEFT, KeyRIGHT: Byte;
      Jump: boolean; DistanceToGround: real): bool;
    procedure SetMaterialBetweenWorldAndPlayer(NewtonMaterial: TNewtonMaterial);
    procedure SetMaterialBetween2Meshes(Mesh1, Mesh2: TVBONewtonMesh;
      NewtonMaterial: TNewtonMaterial);
    procedure UpdateWorld(time: real; Distance2Ground: real = 0);
    procedure NewtonObjectsClear;
    constructor Create(MeshPlayer: TVBOMeshObject; GLScene: TGLScene;
      Owner: TGLBaseSceneObject; Friction: integer; Solver: integer;
      WorldSizeFrom, WorldSizeTo: TVector3f;PlatformArchitecture:Byte=0); overload;
    destructor Destroy; override;
  end;

implementation

destructor TVBONewtonJoint.Destroy;
begin
  NewtonDestroyJoint(NewtonWorld, NewtonJoint);
  inherited;
end;

function TVBONewtonMesh.GetBodyForce: TVector4f;
begin
  NewtonBodyGetForce(NewtonBody, @result);
end;

procedure TVBONewtonMesh.SetBodyForce(Force: TVector4f);
begin
  NewtonBodySetForce(NewtonBody, @Force);
end;

function TVBONewtonMesh.GetBodyState: TIntBool;
begin
  result := NewtonBodyGetFreezeState(NewtonBody);
end;

procedure TVBONewtonMesh.SetBodyState(State: TIntBool);
begin
  NewtonBodySetFreezeState(NewtonBody, State);
end;

function TVBONewtonMesh.GetBodyMass: single;
var
  I1, I2, I3: real;
begin
  NewtonBodyGetMassMatrix(NewtonBody, @result, @I1, @I2, @I3);
end;

procedure TVBONewtonMesh.SetBodyMass(Mass: single);
var
  I1, I2, I3, Weight: real;
begin
  NewtonBodyGetMassMatrix(NewtonBody, @Weight, @I1, @I2, @I3);
  NewtonBodySetMassMatrix(NewtonBody, Mass, I1, I2, I3);
end;

function TVBONewtonMesh.GetBodyMatrix: TMatrix;
begin
  NewtonBodyGetMatrix(NewtonBody, @result);
end;

procedure TVBONewtonMesh.SetBodyMatrix(Matrix: TMatrix);
begin
  NewtonBodySetMatrix(NewtonBody, @Matrix);
end;

function TVBONewtonMesh.GetBodyDirection: TVector4f;
var
  M: TMatrix;
begin
  NewtonBodyGetMatrix(NewtonBody, @M);
  result := M[2];
end;

function TVBONewtonMesh.GetBodyPosition: TVector4f;
var
  M: TMatrix;
begin
  NewtonBodyGetMatrix(NewtonBody, @M);
  result := M[3];
end;

procedure TVBONewtonMesh.SetBodyDirection(Direction: TVector4f);
var
  M: TMatrix;
  up, left, right, dir: TVector;
begin
  NewtonBodyGetMatrix(NewtonBody, @M);
  up := M[1];
  NormalizeVector(up);
  dir := VectorNormalize(Direction);
  right := VectorCrossProduct(dir, up);
  if VectorLength(right) < 1E-5 then
  begin
    right := VectorCrossProduct(ZHmgVector, up);
    if VectorLength(right) < 1E-5 then
      right := VectorCrossProduct(XHmgVector, up);
  end;
  NormalizeVector(right);
  up := VectorCrossProduct(right, dir);
  NormalizeVector(up);
  left := VectorCrossProduct(up, dir);
  NormalizeVector(left);
  M[0] := left;
  M[1] := up;
  M[2] := dir;
  NewtonBodySetMatrix(NewtonBody, @M);
end;

procedure TVBONewtonMesh.SetBodyPosition(Position: TVector4f);
var
  M: TMatrix;
begin
  NewtonBodyGetMatrix(NewtonBody, @M);
  M[3] := Position;
  NewtonBodySetMatrix(NewtonBody, @M);
end;

constructor TVBONewtonMesh.Create;
begin
  inherited;
end;

destructor TVBONewtonMesh.Destroy;
begin
  NewtonDestroyBody(NewtonWorld, NewtonBody);
  inherited;
end;

//

function isKeyDown(Key: Byte): boolean;
begin
  if GetAsyncKeyState(Key) < 0 then
    result := true
  else
    result := false;
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

function NewtonMaterialMake(isCollidable: TIntBool; Softness, StaticFriction,
  KineticFriction, Elasticity, Thickness: real;
  CollisionCallBack: PNewtonContactsProcess = nil): TNewtonMaterial;
begin
  result.isCollidable := isCollidable;
  result.Softness := Softness;
  result.StaticFriction := StaticFriction;
  result.KineticFriction := KineticFriction;
  result.Elasticity := Elasticity;
  result.CollisionCallBack := CollisionCallBack;
  result.Thickness := Thickness;
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
  M: TMatrix;
begin
  NewtonBodyGetMatrix(Body, @M[0, 0]);
  NewtonCollisionForEachPolygonDo(NewtonBodyGetCollision(Body), @M,
    @Debug_ShowGeometryCollision, nil);
end;

procedure ApplyMatrixes2BotVBO(const Body: PNewtonBody; const Matrix: PFloat;
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
  Obj.Matrices.ModelMatrix := Mat;
  Obj.UpdateWorldMatrix;
end;

procedure ApplyMatrixesVBO(const Body: PNewtonBody; const Matrix: PFloat;
  threadIndex: int); cdecl;
var
  Mat: TMatrix;
  Mesh: TVBOMeshObject;
begin
  Mesh := TVBOMeshObject(NewtonBodyGetUserData(Body));
  NewtonBodyGetMatrix(Body, @Mat);
  Mesh.ResetMatrices;
  Mesh.Matrices.ModelMatrix := Mat;
  Mesh.UpdateWorldMatrix;
end;

procedure NewtonApplyForceAndTorque(const Body: PNewtonBody; timestep: Float;
  threadIndex: int); cdecl;
var
  //F_,
  F: TVector4f;
begin
  NewtonBodyGetMassMatrix(Body, @F[3], @F[0], @F[1], @F[2]);
  //NewtonBodyGetForce(Body, @F_);
  F := VectorMake(0, NewtonGravity * F[3], 0);
  //VectorAdd(F, F_);
  NewtonBodyAddForce(Body, @F);
end;

function CalculateVerticesMassVBO(Obj: TVBOMeshObject): TVector4f;
var
  i: integer;
  tx, ty, tz: real;
  buff: PVBOBuffer;
begin
  buff := Obj.MeshList[0];
  tx := 0;
  ty := 0;
  tz := 0;
  for i := 0 to buff.VertexCount - 1 do
  begin
    tx := tx + buff.Vertexes[i][0];
    ty := ty + buff.Vertexes[i][1];
    tz := tz + buff.Vertexes[i][2];
  end;
  result := VectorMake(tx / buff.VertexCount, ty / buff.VertexCount,
    tz / buff.VertexCount)
end;

//

constructor TVBONewtonWorld.Create(MeshPlayer: TVBOMeshObject;
  GLScene: TGLScene; Owner: TGLBaseSceneObject; Friction: integer;
  Solver: integer; WorldSizeFrom, WorldSizeTo: TVector3f;
  PlatformArchitecture: Byte);
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
  JumpSpeed := 7;
  PlayerSpeed := 7;
  if Assigned(MeshPlayer) then
    CreatePlayer(MeshPlayer);
  DefaultWorldMaterialID := NewtonMaterialGetDefaultGroupID(SceneNewtonWorld);
  FDummy := TGLDummyCube.CreateAsChild(Owner);
  Camera := TGLCamera.CreateAsChild(FDummy);
  DirectOpenGL.MoveLast;
  DebugMode := GL_LINES;
  DebugColor := VectorMake(1, 1, 1, 1);
  NewtonSetPlatformArchitecture(SceneNewtonWorld, PlatformArchitecture);
  NewtonSetMultiThreadSolverOnSingleIsland(SceneNewtonWorld, 1);
  //NewtonSetMinimumFrameRate(SceneNewtonWorld, 1)
end;

destructor TVBONewtonWorld.Destroy;
begin
  if Assigned(Player) then
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

function TVBONewtonWorld.GetObject(Index: integer): TVBONewtonMesh;
begin
  result := FMeshBodyList[Index];
end;

function TVBONewtonWorld.FPSApplyCamMove: TCamMoveResult;
begin
  if (Mouse.CursorPos.X = Px) and (Mouse.CursorPos.Y = Py) then
  begin
    result.isMove := false;
    exit;
  end;
  result.dX := Px - Mouse.CursorPos.X;
  result.dY := Py - Mouse.CursorPos.Y;
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
  (KeyUP, KeyDOWN, KeyLEFT, KeyRIGHT: Byte; Jump: boolean;
  DistanceToGround: real): bool;
label l1;
var
  FI, F, FJ: TVector4f;
begin
  F := VectorMake(0, 0, 0);
  if (not isKeyDown(KeyUP)) and (not isKeyDown(KeyDOWN)) and
    (not isKeyDown(KeyLEFT)) and (not isKeyDown(KeyRIGHT)) and (not Jump) or
    (DistanceToGround > Player.BBox[1]) then
  begin
    result := false;
    goto l1;
  end;
  result := true;
  if isKeyDown(KeyUP) then
  begin
    FI := VectorTransform(VectorMake(0, 0, -PlayerSpeed), FDummy.Matrix);
    AddVector(F, FI);
  end;
  if isKeyDown(KeyDOWN) then
  begin
    FI := VectorTransform(VectorMake(0, 0, PlayerSpeed), FDummy.Matrix);
    AddVector(F, FI);
  end;
  if isKeyDown(KeyLEFT) then
  begin
    FI := VectorTransform(VectorMake(PlayerSpeed, 0, 0), FDummy.Matrix);
    AddVector(F, FI);
  end;
  if isKeyDown(KeyRIGHT) then
  begin
    FI := VectorTransform(VectorMake(-PlayerSpeed, 0, 0), FDummy.Matrix);
    AddVector(F, FI);
  end;
  if Jump then
  begin
    FI := Player.BodyPosition;
    FJ := VectorTransform(VectorMake(0, JumpSpeed, 0), FDummy.Matrix);
    NewtonBodyAddImpulse(Player.NewtonBody, @FJ, @FI);
  end;
l1 :
  NewtonBodyGetVelocity(Player.NewtonBody, @FJ);
  if DistanceToGround < Player.BBox[1] then
    F[1] := FJ[1]
  else
    F := FJ;
  if F[1] > JumpSpeed then
    F[1] := JumpSpeed;
  NewtonBodySetVelocity(Player.NewtonBody, @F);
end;

procedure TVBONewtonWorld.DebugRender(Sender: TObject;
  var rci: TRenderContextInfo);
var
  t: 0 .. maxBodes;
begin
  if not DebugGeometry then
    exit;
  // DirectOpenGL.MoveLast;
  glDisable(GL_LIGHTING);
  glBegin(DebugMode);
  glColor4f(DebugColor[0], DebugColor[1], DebugColor[2], DebugColor[3]);
  if FMeshBodyList.Count > 0 then
    for t := 0 to FMeshBodyList.Count - 1 do
      Debug_ShowBodyCollision(NewtonObjects[t].NewtonBody);
  if Assigned(Player) then
    Debug_ShowBodyCollision(Player.NewtonBody);
  glEnd;
  glEnable(GL_LIGHTING);
end;

function TVBONewtonWorld.AddTreeBody(VBOMeshObject: TVBOMeshObject)
  : TVBONewtonMesh;
var
  col: pnewtoncollision;
  List: TAffineVectorList;
  Faces: TMatrix3f;
  i: integer;
  buff: PVBOBuffer;
  MeshList: TList;
  Matrix: TMatrix;
begin
  MeshList := VBOMeshObject.MeshList;
  Matrix := VBOMeshObject.Matrices.WorldMatrix;
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
  NewtonObjects[FMeshBodyList.Count - 1].NewtonBody := NewtonCreateBody
    (SceneNewtonWorld, col);
  NewtonObjects[FMeshBodyList.Count - 1].NewtonWorld := SceneNewtonWorld;
  NewtonObjects[FMeshBodyList.Count - 1].MeshObject := VBOMeshObject;
  NewtonReleaseCollision(SceneNewtonWorld, col);
  NewtonBodySetMatrix(NewtonObjects[FMeshBodyList.Count - 1].NewtonBody,
    @Matrix);
  NewtonObjects[FMeshBodyList.Count - 1].MaterialID :=
    NewtonMaterialCreateGroupID(SceneNewtonWorld);
  NewtonBodySetMaterialGroupID
    (NewtonObjects[FMeshBodyList.Count - 1].NewtonBody,
    NewtonObjects[FMeshBodyList.Count - 1].MaterialID);
  result := NewtonObjects[FMeshBodyList.Count - 1];
end;

function TVBONewtonWorld.AddConvexBody(VBOMeshObject: TVBOMeshObject;
  Weight: real = 1; noUpdateRMatrix: boolean = false): TVBONewtonMesh;
var
  Cloud: array of TAffineVector;
  i: integer;
  MeshMemory: TList;
  Matrix: TMatrix;
  col: pnewtoncollision;
  BoBox, Inertia, tmpV, tmpV2: TAffineVector;
  CM: TVector4f;
  buff: PVBOBuffer;
begin
  MeshMemory := VBOMeshObject.MeshList;
  Matrix := VBOMeshObject.Matrices.WorldMatrix;
  buff := MeshMemory[0];
  SetLength(Cloud, buff.VertexCount);
  for i := 0 to buff.VertexCount - 1 do
  begin
    Cloud[i][0] := buff.Vertexes[i][0];
    Cloud[i][1] := buff.Vertexes[i][1];
    Cloud[i][2] := buff.Vertexes[i][2];
  end;
  tmpV := VBOMeshObject.Extents.emax;
  tmpV2 := VBOMeshObject.Extents.emin;
  VectorSubtract(tmpV, tmpV2, BoBox);
  if Weight = 1 then
    Weight := BoBox[0] * BoBox[1] * BoBox[2];
  col := NewtonCreateConvexHull(SceneNewtonWorld, buff.VertexCount, @Cloud[0],
    SizeOf(TAffineVector), 0, 0, nil);
  FMeshBodyList.Add(TVBONewtonMesh.Create);
  NewtonObjects[FMeshBodyList.Count - 1].NewtonBody := NewtonCreateBody
    (SceneNewtonWorld, col);
  NewtonObjects[FMeshBodyList.Count - 1].NewtonWorld := SceneNewtonWorld;
  NewtonObjects[FMeshBodyList.Count - 1].MeshObject := VBOMeshObject;
  NewtonReleaseCollision(SceneNewtonWorld, col);
  Inertia[0] := Weight * (BoBox[1] * BoBox[1] + BoBox[2] * BoBox[2]) / 12;
  Inertia[1] := Weight * (BoBox[0] * BoBox[0] + BoBox[2] * BoBox[2]) / 12;
  Inertia[2] := Weight * (BoBox[0] * BoBox[0] + BoBox[1] * BoBox[1]) / 12;
  NewtonBodySetMassMatrix(NewtonObjects[FMeshBodyList.Count - 1].NewtonBody,
    Weight, Inertia[0], Inertia[1], Inertia[2]);
  NewtonBodySetMatrix(NewtonObjects[FMeshBodyList.Count - 1].NewtonBody,
    @Matrix);
  NewtonBodySetForceAndTorqueCallback
    (NewtonObjects[FMeshBodyList.Count - 1].NewtonBody,
    NewtonApplyForceAndTorque);
  NewtonBodySetLinearDamping(NewtonObjects[FMeshBodyList.Count - 1].NewtonBody,
    1);
  CM := CalculateVerticesMassVBO(VBOMeshObject);
  NewtonBodySetCentreOfMass(NewtonObjects[FMeshBodyList.Count - 1].NewtonBody,
    @CM);
  NewtonBodySetUserData(NewtonObjects[FMeshBodyList.Count - 1].NewtonBody,
    VBOMeshObject);
  NewtonBodySetAutoSleep(LastObject.NewtonBody, 1);
  if noUpdateRMatrix then
    NewtonBodySetTransformCallback
      (NewtonObjects[FMeshBodyList.Count - 1].NewtonBody, ApplyMatrixes2BotVBO)
  else
    NewtonBodySetTransformCallback
      (NewtonObjects[FMeshBodyList.Count - 1].NewtonBody, ApplyMatrixesVBO);
  NewtonObjects[FMeshBodyList.Count - 1].MaterialID :=
    NewtonMaterialCreateGroupID(SceneNewtonWorld);
  NewtonBodySetMaterialGroupID
    (NewtonObjects[FMeshBodyList.Count - 1].NewtonBody,
    NewtonObjects[FMeshBodyList.Count - 1].MaterialID);
  LastObject.BBox := BoBox;
  result := NewtonObjects[FMeshBodyList.Count - 1];
end;

procedure TVBONewtonWorld.UpdateWorld(time: real; Distance2Ground: real = 0);
var
  M: TMatrix;
  V: TVector4f;
begin
  NewtonUpdate(SceneNewtonWorld, time);
  if not Assigned(Player) then
    exit;
  if Distance2Ground < Player.BBox[1] then
  begin
    NewtonBodySetForceAndTorqueCallback(Player.NewtonBody, nil);
    if Player.BodySpeed[1] > 0 then
      Player.BodySpeed := VectorMake
        (Player.BodySpeed[0], 0, Player.BodySpeed[2]);
  end
  else
    NewtonBodySetForceAndTorqueCallback(Player.NewtonBody,
      NewtonApplyForceAndTorque);
  NewtonBodyGetMatrix(Player.NewtonBody, @M);
  M[0] := FDummy.Matrix[0];
  M[1] := FDummy.Matrix[1];
  M[2] := FDummy.Matrix[2];
  if Assigned(Player.MeshObject) then
  begin
    Player.MeshObject.ResetMatrices;
    Player.MeshObject.Matrices.ModelMatrix := M;
    Player.MeshObject.UpdateWorldMatrix;
  end;
  NewtonBodySetMatrix(Player.NewtonBody, @M);
  FDummy.Matrix := M;
end;

procedure TVBONewtonWorld.SetMaterialBetweenWorldAndPlayer
  (NewtonMaterial: TNewtonMaterial);
begin
  NewtonMaterialSetDefaultCollidable(SceneNewtonWorld, Player.MaterialID, 0,
    NewtonMaterial.isCollidable);
  NewtonMaterialSetDefaultSoftness(SceneNewtonWorld, Player.MaterialID, 0,
    NewtonMaterial.Softness);
  NewtonMaterialSetDefaultFriction(SceneNewtonWorld, Player.MaterialID, 0,
    NewtonMaterial.StaticFriction, NewtonMaterial.KineticFriction);
  NewtonMaterialSetDefaultElasticity(SceneNewtonWorld, Player.MaterialID, 0,
    NewtonMaterial.Elasticity);
  if NewtonMaterial.CollisionCallBack = nil then
    NewtonMaterialSetCollisionCallback(SceneNewtonWorld, Player.MaterialID, 0,
      nil, nil, @DefaultNewtonContactsProcess)
  else
    NewtonMaterialSetCollisionCallback(SceneNewtonWorld, Player.MaterialID, 0,
      nil, nil, NewtonMaterial.CollisionCallBack);
end;

procedure TVBONewtonWorld.SetMaterialBetween2Meshes
  (Mesh1, Mesh2: TVBONewtonMesh; NewtonMaterial: TNewtonMaterial);
begin
  NewtonMaterialSetDefaultCollidable(SceneNewtonWorld, Mesh1.MaterialID,
    Mesh2.MaterialID, NewtonMaterial.isCollidable);
  NewtonMaterialSetDefaultSoftness(SceneNewtonWorld, Mesh1.MaterialID,
    Mesh2.MaterialID, NewtonMaterial.Softness);
  NewtonMaterialSetDefaultFriction(SceneNewtonWorld, Mesh1.MaterialID,
    Mesh2.MaterialID, NewtonMaterial.StaticFriction,
    NewtonMaterial.KineticFriction);
  NewtonMaterialSetDefaultElasticity(SceneNewtonWorld, Mesh1.MaterialID,
    Mesh2.MaterialID, NewtonMaterial.Elasticity);
  if NewtonMaterial.CollisionCallBack = nil then
    NewtonMaterialSetCollisionCallback(SceneNewtonWorld, Mesh1.MaterialID,
      Mesh2.MaterialID, nil, nil, @DefaultNewtonContactsProcess)
  else
    NewtonMaterialSetCollisionCallback(SceneNewtonWorld, Mesh1.MaterialID,
      Mesh2.MaterialID, nil, nil, NewtonMaterial.CollisionCallBack);
  NewtonMaterialSetSurfaceThickness(SceneNewtonWorld, Mesh1.MaterialID,
    Mesh2.MaterialID, NewtonMaterial.Thickness);
end;

function TVBONewtonWorld.GetObjectsCount: integer;
begin
  result := FMeshBodyList.Count;
end;

procedure TVBONewtonWorld.CreatePlayer(VBOMeshObject: TVBOMeshObject);
var
  Cloud: array of TAffineVector;
  i: integer;
  Weight: real;
  MeshMemory: TList;
  Matrix: TMatrix;
  col: pnewtoncollision;
  CM: TVector4f;
  buff: PVBOBuffer;
  Inertia, BBox: TVector3f;
begin
  MeshMemory := VBOMeshObject.MeshList;
  Matrix := VBOMeshObject.Matrices.WorldMatrix;
  buff := MeshMemory[0];
  SetLength(Cloud, buff.VertexCount);
  for i := 0 to buff.VertexCount - 1 do
  begin
    Cloud[i][0] := buff.Vertexes[i][0];
    Cloud[i][1] := buff.Vertexes[i][1];
    Cloud[i][2] := buff.Vertexes[i][2];
  end;
  Weight := 1000;
  col := NewtonCreateConvexHull(SceneNewtonWorld, buff.VertexCount, @Cloud[0],
    SizeOf(TAffineVector), 0, 0, nil);
  Player := TVBONewtonMesh.Create;
  Player.NewtonBody := NewtonCreateBody(SceneNewtonWorld, col);
  Player.NewtonWorld := SceneNewtonWorld;
  Player.MeshObject := VBOMeshObject;
  VectorSubtract(VBOMeshObject.Extents.emax, VBOMeshObject.Extents.emin,
    Player.BBox);
  Player.BBox[1] := Player.BBox[1] + 0.1;
  BBox := Player.BBox;
  NewtonReleaseCollision(SceneNewtonWorld, col);
  Inertia[0] := Weight * (BBox[1] * BBox[1] + BBox[2] * BBox[2]) / 12;
  Inertia[1] := Weight * (BBox[0] * BBox[0] + BBox[2] * BBox[2]) / 12;
  Inertia[2] := Weight * (BBox[0] * BBox[0] + BBox[1] * BBox[1]) / 12;
  NewtonBodySetMassMatrix(Player.NewtonBody, Weight, Inertia[0], Inertia[1],
    Inertia[2]);
  NewtonBodySetMatrix(Player.NewtonBody, @Matrix);
  NewtonBodySetForceAndTorqueCallback(Player.NewtonBody,
    NewtonApplyForceAndTorque);
  NewtonBodySetLinearDamping(Player.NewtonBody, 1);
  CM := CalculateVerticesMassVBO(VBOMeshObject);
  NewtonBodySetCentreOfMass(Player.NewtonBody, @CM);
  NewtonBodySetUserData(Player.NewtonBody, VBOMeshObject);
  Player.MaterialID := NewtonMaterialCreateGroupID(SceneNewtonWorld);
  NewtonBodySetMaterialGroupID(Player.NewtonBody, Player.MaterialID);
end;

procedure TVBONewtonWorld.NewtonObjectsClear;
var
  t: 0 .. maxBodes;
begin
  if NewtonJointsCount > 0 then
    for t := 0 to NewtonJointsCount - 1 do
      NewtonJoints[t].Free;
  if NewtonObjectsCount > 0 then
    for t := 0 to NewtonObjectsCount - 1 do
      NewtonObjects[t].Free;
  NewtonMaterialDestroyAllGroupID(SceneNewtonWorld);
  // DefaultWorldMaterialID:=NewtonMaterialGetDefaultGroupID(SceneNewtonWorld);
  // Player.MaterialID:=NewtonMaterialCreateGroupID(SceneNewtonWorld);
  FMeshBodyList.Clear;
  FJoints.Clear;
end;

function TVBONewtonWorld.GetLastObject: TVBONewtonMesh;
begin
  result := TVBONewtonMesh(FMeshBodyList.Last);
end;

function TVBONewtonMesh.GetBodySpeed: TVector4f;
begin
  NewtonBodyGetVelocity(NewtonBody, @result);
end;

procedure TVBONewtonMesh.SetBodySpeed(Speed: TVector4f);
begin
  NewtonBodySetVelocity(NewtonBody, @Speed)
end;

function TVBONewtonMesh.GetBodyInertion: TVector4f;
var
  Weight: real;
begin
  NewtonBodyGetMassMatrix(NewtonBody, @Weight, @result[0], @result[1],
    @result[2]);
end;

procedure TVBONewtonMesh.SetBodyInertion(Inertion: TVector4f);
var
  I1, I2, I3, Weight: real;
begin
  NewtonBodyGetMassMatrix(NewtonBody, @Weight, @I1, @I2, @I3);
  NewtonBodySetMassMatrix(NewtonBody, Weight, Inertion[0], Inertion[1],
    Inertion[2]);
end;

function TVBONewtonWorld.GetJointsCount: integer;
begin
  result := FJoints.Count;
end;

function TVBONewtonWorld.GetLastJoint: TVBONewtonJoint;
begin
  result := TVBONewtonJoint(FJoints.Last);
end;

function TVBONewtonWorld.GetJointObject(Index: integer): TVBONewtonJoint;
begin
  result := FJoints[Index];
end;

function TVBONewtonWorld.AddHingeJoint(Mesh1: TVBONewtonMesh;
  Mesh2: TVBONewtonMesh): TVBONewtonJoint;
deprecated;
begin
  //
end;

function TVBONewtonWorld.AddBallSocketJoint(Parent: TVBONewtonMesh;
  Child: TVBONewtonMesh; PivotPoint: TVector4f; Pin: TVector4f;
  maxCone, maxTwist: real; State: TIntBool = 0): TVBONewtonJoint;
begin
  FJoints.Add(TVBONewtonJoint.Create);
  LastJointObject.NewtonWorld := SceneNewtonWorld;
  LastJointObject.NewtonJoint := NewtonConstraintCreateBall
    (SceneNewtonWorld, @PivotPoint, Child.NewtonBody, Parent.NewtonBody);
  NewtonJointSetCollisionState(LastJointObject.NewtonJoint, State);
  NewtonBallSetConeLimits(LastJointObject.NewtonJoint, @Pin, maxCone, maxTwist);
  result := LastJointObject;
end;

procedure TVBONewtonJoint.SetJointCallback(CallBack: NewtonBallCallBack);
begin
  NewtonBallSetUserCallback(NewtonJoint, CallBack);
end;

procedure TVBONewtonWorld.CreatePlayerSphere(X: real; Y: real; Z: real;
  Matrix: TMatrix);
var
  Weight: real;
  col: pnewtoncollision;
  Inertia, BBox: TAffineVector;
begin
  BBox := AffineVectorMake(X, Y + 0.2, Z);
  Inertia[0] := Weight * (BBox[1] * BBox[1] + BBox[2] * BBox[2]) / 12;
  Inertia[1] := Weight * (BBox[0] * BBox[0] + BBox[2] * BBox[2]) / 12;
  Inertia[2] := Weight * (BBox[0] * BBox[0] + BBox[1] * BBox[1]) / 12;
  Weight := 1000;
  col := NewtonCreateSphere(SceneNewtonWorld, X, Y, Z, 0, nil);
  Player := TVBONewtonMesh.Create;
  Player.NewtonBody := NewtonCreateBody(SceneNewtonWorld, col);
  Player.NewtonWorld := SceneNewtonWorld;
  Player.BBox := BBox;
  NewtonReleaseCollision(SceneNewtonWorld, col);
  NewtonBodySetMassMatrix(Player.NewtonBody, Weight, Inertia[0], Inertia[1],
    Inertia[2]);
  NewtonBodySetMatrix(Player.NewtonBody, @Matrix);
  NewtonBodySetForceAndTorqueCallback(Player.NewtonBody,
    NewtonApplyForceAndTorque);
  NewtonBodySetLinearDamping(Player.NewtonBody, 1);
  Player.MaterialID := NewtonMaterialCreateGroupID(SceneNewtonWorld);
  NewtonBodySetMaterialGroupID(Player.NewtonBody, Player.MaterialID);
  NewtonBodySetContinuousCollisionMode(Player.NewtonBody, 1);
end;

procedure TVBONewtonWorld.CreatePlayerBox(X: real; Y: real; Z: real;
  Matrix: TMatrix4f);
var
  Weight: real;
  col: pnewtoncollision;
  Inertia, BBox: TAffineVector;
begin
  BBox := AffineVectorMake(X, Y + 0.2, Z);
  Inertia[0] := Weight * (BBox[1] * BBox[1] + BBox[2] * BBox[2]) / 12;
  Inertia[1] := Weight * (BBox[0] * BBox[0] + BBox[2] * BBox[2]) / 12;
  Inertia[2] := Weight * (BBox[0] * BBox[0] + BBox[1] * BBox[1]) / 12;
  Weight := 1000;
  col := NewtonCreateBox(SceneNewtonWorld, X, Y, Z, 0, nil);
  Player := TVBONewtonMesh.Create;
  Player.NewtonBody := NewtonCreateBody(SceneNewtonWorld, col);
  Player.NewtonWorld := SceneNewtonWorld;
  Player.BBox := BBox;
  NewtonReleaseCollision(SceneNewtonWorld, col);
  NewtonBodySetMassMatrix(Player.NewtonBody, Weight, Inertia[0], Inertia[1],
    Inertia[2]);
  NewtonBodySetMatrix(Player.NewtonBody, @Matrix);
  NewtonBodySetForceAndTorqueCallback(Player.NewtonBody,
    NewtonApplyForceAndTorque);
  NewtonBodySetLinearDamping(Player.NewtonBody, 1);
  Player.MaterialID := NewtonMaterialCreateGroupID(SceneNewtonWorld);
  NewtonBodySetMaterialGroupID(Player.NewtonBody, Player.MaterialID);
  NewtonBodySetContinuousCollisionMode(Player.NewtonBody, 1);
end;

function TVBONewtonWorld.GetSDKVersion: String;
begin
  result := IntToStr(NewtonWorldGetVersion(SceneNewtonWorld));
  result := result[1] + '.' + FCopy(result, 2, length(result));
end;

function FCopy(str: string; N, K: integer): string;
var
  i: integer;
begin
  result := '';
  for i := N to K do
    result := result + str[i];
end;

end.
