// VBONewton by For[)
// updated 28.07.10

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

  TIntBool = 0 .. 1;

  TNewtonMaterial = record
    Softness, StaticFriction, KineticFriction, Elasticity, Thickness: real;
    isCollidable: TIntBool;
    CollisionCallBack: PNewtonContactsProcess;
  end;

function ScaleVector3f(V: TVector3f; Scale: Single): TVector3f;
function FCopy(str: string; N, K: integer): string;
function isKeyDown(Key: Byte): boolean;
procedure SimulateKeyDown(Key: Byte);
procedure SimulateKeyUP(Key: Byte);
function Gr2Rad(Gr: single): single;
procedure Debug_ShowBodyCollision(const Body: PNewtonBody); cdecl;
procedure Debug_ShowGeometryCollision(const Body: PNewtonBody;
  VertexCount: integer; const FaceArray: PFloat; FaceId: int); cdecl;
function NewtonMaterialMake(isCollidable: TIntBool; Softness, StaticFriction,
  KineticFriction, Elasticity, Thickness: real;
  CollisionCallBack: PNewtonContactsProcess = nil): TNewtonMaterial;
procedure DefaultNewtonContactsProcess(const contact: PNewtonJoint;
  timestep: Float; threadIndex: int); cdecl;
procedure dNewtonApplyForceAndTorque(const Body: PNewtonBody; timestep: Float;
  threadIndex: int); cdecl;
function CalculateVerticesMassVBO(Obj: TVBOMeshObject): TVector4f;

type
  TVBONewtonJoint = class
  private
    //
  public
    NewtonJoint: PNewtonJoint;
    NewtonWorld: PNewtonWorld;
    destructor Destroy; override;
  end;

type
  TVBONewtonMesh = class
  private
    procedure SetCallBack(C: NewtonApplyForceAndTorque);
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
    property CallBack: NewtonApplyForceAndTorque write SetCallBack;
    property BodyMass: single read GetBodyMass write SetBodyMass;
    property BodyInertion: TVector4f read GetBodyInertion write SetBodyInertion;
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

  TNewtonRagDoll = record
    Body: TVBONewtonMesh;
    Body2: TVBONewtonMesh;
    Head: TVBONewtonMesh;
    LHand: TVBONewtonMesh;
    RHand: TVBONewtonMesh;
    LHand2: TVBONewtonMesh;
    RHand2: TVBONewtonMesh;
    LFoot: TVBONewtonMesh;
    RFoot: TVBONewtonMesh;
    LFoot2: TVBONewtonMesh;
    RFoot2: TVBONewtonMesh;
  end;

  TNewtonRagDollParams = record
    BodySize: TVector3f;
    Body2Size: TVector3f;
    HandSize: TVector3f;
    Hand2Size: TVector3f;
    FootSize: TVector3f;
    Foot2Size: TVector3f;
    HeadSize: TVector3f;
  end;

type
  TVBONewtonWorld = class
  private
    FMeshBodyList: TList;
    FJoints: TList;
    function AddSphere(RadX, RadY, RadZ, Weight : Single; Matrix:TMatrix):TVBONewtonMesh;
    function AddBox(X, Y, Z, Weight : Single; Matrix:TMatrix):TVBONewtonMesh;
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
    function AddVboRagDoll(BodyPosition: TVector4f; RagDollVBOMesh: TVBOMesh;
      Params: TNewtonRagDollParams; Scale:Single = 1): TNewtonRagDoll;
    function AddRagDoll(BodyPosition: TVector4f; Params: TNewtonRagDollParams): TNewtonRagDoll; deprecated;
    function AddHingeJoint(Parent: TVBONewtonMesh; Child: TVBONewtonMesh;
      PivotPoint: TVector4f; Pin: TVector4f;
      CallBack: NewtonHingeCallBack = nil): TVBONewtonJoint;
    function AddBallSocketJoint(Parent, Child: TVBONewtonMesh;
      PivotPoint: TVector4f; Pin: TVector4f; maxCone, maxTwist: real;
      CallBack: NewtonBallCallBack = nil; State: TIntBool = 0): TVBONewtonJoint;
    //
    procedure CreatePlayer(VBOMeshObject: TVBOMeshObject);
    procedure CreatePlayerSphere(X, Y, Z: real; Matrix: TMatrix);
    procedure CreatePlayerBox(X, Y, Z: real; Matrix: TMatrix);
    function FPSApplyCamMove: TCamMoveResult;
    function FPSApplyKeyMove(KeyUP, KeyDOWN, KeyLEFT, KeyRIGHT: Byte;
      Jump: boolean; DistanceToGround: real): bool;
    procedure SetMaterialBetween2Meshes(Mesh1, Mesh2: TVBONewtonMesh;
      NewtonMaterial: TNewtonMaterial);
    procedure UpdateWorld(time: real; Distance2Ground: real = 10);
    procedure NewtonObjectsClear;
    constructor Create(MeshPlayer: TVBOMeshObject; GLScene: TGLScene;
      Owner: TGLBaseSceneObject; Friction: integer; Solver: integer;
      WorldSizeFrom, WorldSizeTo: TVector3f; PlatformArchitecture: Byte = 1);
      overload;
    destructor Destroy; override;
  end;

const
{$J+}
  maxBodes = 10000;
  NewtonGravity: real = -10;
  PinUP: TVector4f = (0, 1.57, 0, 1);
  PinDOWN: TVector4f = (0, -1.57, 0, 1);
  PinLEFT: TVector4f = (-1.57, 0, 0, 1);
  PinRIGHT: TVector4f = (1.57, 0, 0, 1);
  DefaultRagDollParams: TNewtonRagDollParams = ();
{$J-}

implementation

function ScaleVector3f(V: TVector3f; Scale: Single): TVector3f;
begin
  Result[0] := V[0] * Scale;
  Result[1] := V[1] * Scale;
  Result[2] := V[2] * Scale;
end;

procedure SimulateKeyDown(Key: Byte);
begin
  KeyBD_Event(Key, 0, 0, 0);
end;

procedure SimulateKeyUP(Key: Byte);
begin
  KeyBD_Event(Key, 0, KEYEVENTF_KEYUP, 0);
end;

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

procedure TVBONewtonMesh.SetCallBack(C: NewtonApplyForceAndTorque);
begin
  NewtonBodySetForceAndTorqueCallback(NewtonBody, C);
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
  if GetKeyState(Key) < 0 then
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
  PlatformArchitecture: Byte = 1);
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
  with DefaultRagDollParams do
  begin
    BodySize := AffineVectorMake(1, 1, 0.4);
    Body2Size := AffineVectorMake(0.8, 0.8, 0.4);
    HandSize := AffineVectorMake(1, 0.3, 0.25);
    Hand2Size := AffineVectorMake(1.25, 0.3, 0.25);
    FootSize := AffineVectorMake(0.3, 1.25, 0.3);
    Foot2Size := AffineVectorMake(0.3, 1.2, 0.3);
    HeadSize := AffineVectorMake(0.3, 0.3, 0.3);
  end;
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
    dNewtonApplyForceAndTorque);
  CM := CalculateVerticesMassVBO(VBOMeshObject);
  NewtonBodySetCentreOfMass(NewtonObjects[FMeshBodyList.Count - 1].NewtonBody,
    @CM);
  NewtonBodySetUserData(NewtonObjects[FMeshBodyList.Count - 1].NewtonBody,
    VBOMeshObject);
  NewtonBodySetAutoSleep(LastObject.NewtonBody, 1);
  NewtonObjects[FMeshBodyList.Count - 1].MaterialID :=
    NewtonMaterialCreateGroupID(SceneNewtonWorld);
  NewtonBodySetMaterialGroupID
    (NewtonObjects[FMeshBodyList.Count - 1].NewtonBody,
    NewtonObjects[FMeshBodyList.Count - 1].MaterialID);
  LastObject.BBox := BoBox;
  if noUpdateRMatrix then
    NewtonBodySetTransformCallback
      (NewtonObjects[FMeshBodyList.Count - 1].NewtonBody, ApplyMatrixes2BotVBO)
  else
    NewtonBodySetTransformCallback
      (NewtonObjects[FMeshBodyList.Count - 1].NewtonBody, ApplyMatrixesVBO);
  result := NewtonObjects[FMeshBodyList.Count - 1];
end;

procedure TVBONewtonWorld.UpdateWorld(time: real; Distance2Ground: real = 10);
var
  M: TMatrix;
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
      dNewtonApplyForceAndTorque);
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
    dNewtonApplyForceAndTorque);
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

function TVBONewtonWorld.AddHingeJoint(Parent: TVBONewtonMesh;
  Child: TVBONewtonMesh; PivotPoint: TVector4f; Pin: TVector4f;
  CallBack: NewtonHingeCallBack = nil): TVBONewtonJoint;
begin
  FJoints.Add(TVBONewtonJoint.Create);
  LastJointObject.NewtonWorld := SceneNewtonWorld;
  LastJointObject.NewtonJoint := NewtonConstraintCreateHinge
    (SceneNewtonWorld, @PivotPoint, @Pin, Child.NewtonBody, Parent.NewtonBody);
  NewtonHingeSetUserCallback(LastJointObject.NewtonJoint, CallBack);
  NewtonJointSetStiffness(LastJointObject.NewtonJoint, 0);
  result := LastJointObject;
end;

function TVBONewtonWorld.AddBallSocketJoint(Parent: TVBONewtonMesh;
  Child: TVBONewtonMesh; PivotPoint: TVector4f; Pin: TVector4f;
  maxCone, maxTwist: real; CallBack: NewtonBallCallBack = nil;
  State: TIntBool = 0): TVBONewtonJoint;
begin
  FJoints.Add(TVBONewtonJoint.Create);
  LastJointObject.NewtonWorld := SceneNewtonWorld;
  LastJointObject.NewtonJoint := NewtonConstraintCreateBall
    (SceneNewtonWorld, @PivotPoint, Child.NewtonBody, Parent.NewtonBody);
  NewtonJointSetCollisionState(LastJointObject.NewtonJoint, State);
  NewtonBallSetConeLimits(LastJointObject.NewtonJoint, @Pin, maxCone, maxTwist);
  NewtonBallSetUserCallback(LastJointObject.NewtonJoint, CallBack);
  NewtonJointSetStiffness(LastJointObject.NewtonJoint, 0);
  result := LastJointObject;
end;

procedure TVBONewtonWorld.CreatePlayerSphere(X: real; Y: real; Z: real;
  Matrix: TMatrix);
var
  col: pnewtoncollision;
  Inertia, BBox: TAffineVector;
  Weight: real;
begin
  BBox := AffineVectorMake(X, Y + 0.2, Z);
  Weight := 1000;
  Inertia[0] := Weight * (BBox[1] * BBox[1] + BBox[2] * BBox[2]) / 12;
  Inertia[1] := Weight * (BBox[0] * BBox[0] + BBox[2] * BBox[2]) / 12;
  Inertia[2] := Weight * (BBox[0] * BBox[0] + BBox[1] * BBox[1]) / 12;
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
    dNewtonApplyForceAndTorque);
  NewtonBodySetLinearDamping(Player.NewtonBody, 0.5);
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
  Weight := 1000;
  Inertia[0] := Weight * (BBox[1] * BBox[1] + BBox[2] * BBox[2]) / 12;
  Inertia[1] := Weight * (BBox[0] * BBox[0] + BBox[2] * BBox[2]) / 12;
  Inertia[2] := Weight * (BBox[0] * BBox[0] + BBox[1] * BBox[1]) / 12;
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
    dNewtonApplyForceAndTorque);
  NewtonBodySetLinearDamping(Player.NewtonBody, 0.5);
  Player.MaterialID := NewtonMaterialCreateGroupID(SceneNewtonWorld);
  NewtonBodySetMaterialGroupID(Player.NewtonBody, Player.MaterialID);
  NewtonBodySetContinuousCollisionMode(Player.NewtonBody, 1);
end;

function TVBONewtonWorld.GetSDKVersion: String;
begin
  result := IntToStr(NewtonWorldGetVersion(SceneNewtonWorld));
  result := result[1] + '.' + FCopy(result, 2, length(result));
end;

function TVBONewtonWorld.AddVboRagDoll(BodyPosition: TVector4f;
  RagDollVBOMesh: TVBOMesh; Params: TNewtonRagDollParams; Scale:Single = 1): TNewtonRagDoll;
var
  HeadPos, LHandPos, RHandPos, LFootPos, RFootPos, LHandPos2, RHandPos2,
    LFootPos2, RFootPos2, Body2Pos: TVector4f;
begin
  Params.BodySize := ScaleVector3f(Params.BodySize, Scale);
  Params.Body2Size := ScaleVector3f(Params.Body2Size, Scale);
  Params.HandSize := ScaleVector3f(Params.HandSize, Scale);
  Params.Hand2Size := ScaleVector3f(Params.Hand2Size, Scale);
  Params.FootSize := ScaleVector3f(Params.FootSize, Scale);
  Params.Foot2Size := ScaleVector3f(Params.Foot2Size, Scale);
  Params.HeadSize := ScaleVector3f(Params.HeadSize, Scale);

  Body2Pos := VectorAdd(BodyPosition, VectorMake(0,
    -0.5 * (Params.Body2Size[1] + Params.BodySize[1]), 0));
  // голова
  RagDollVBOMesh.AddSphere(Params.HeadSize[0], 16, 16);
  HeadPos := VectorAdd(BodyPosition, VectorMake
      (0, 0.5 * (Params.BodySize[1] + Params.HeadSize[0]), 0));
  RagDollVBOMesh[RagDollVBOMesh.Count - 1].Position := HeadPos;
  // тело
  RagDollVBOMesh.AddBox(Params.BodySize[0], Params.BodySize[1],
    Params.BodySize[2], 4, 4, 4);
  RagDollVBOMesh[RagDollVBOMesh.Count - 1].Position := BodyPosition;
  // л.рука
  RagDollVBOMesh.AddBox(Params.HandSize[0], Params.HandSize[1],
    Params.HandSize[2], 4, 4, 4);
  LHandPos := VectorAdd(BodyPosition, VectorMake
      (-0.5 * (Params.BodySize[0] + Params.HandSize[0]),
      0.25 * Params.BodySize[1], 0));
  RagDollVBOMesh[RagDollVBOMesh.Count - 1].Position := LHandPos;
  // п.рука
  RagDollVBOMesh.AddBox(Params.HandSize[0], Params.HandSize[1],
    Params.HandSize[2], 4, 4, 4);
  RHandPos := VectorAdd(BodyPosition, VectorMake
      (0.5 * (Params.BodySize[0] + Params.HandSize[0]),
      0.25 * Params.BodySize[1], 0));
  RagDollVBOMesh[RagDollVBOMesh.Count - 1].Position := RHandPos;
  // л.нога
  RagDollVBOMesh.AddBox(Params.FootSize[0], Params.FootSize[1],
    Params.FootSize[2], 4, 4, 4);
  LFootPos := VectorAdd(Body2Pos, VectorMake(-0.25 * Params.Body2Size[0],
      -0.5 * (Params.Body2Size[1] + Params.FootSize[1]), 0));
  RagDollVBOMesh[RagDollVBOMesh.Count - 1].Position := LFootPos;
  // п.нога
  RagDollVBOMesh.AddBox(Params.FootSize[0], Params.FootSize[1],
    Params.FootSize[2], 4, 4, 4);
  RFootPos := VectorAdd(Body2Pos, VectorMake(0.25 * Params.Body2Size[0],
      -0.5 * (Params.Body2Size[1] + Params.FootSize[1]), 0));
  RagDollVBOMesh[RagDollVBOMesh.Count - 1].Position := RFootPos;
  // л. рука2
  RagDollVBOMesh.AddBox(Params.Hand2Size[0], Params.Hand2Size[1],
    Params.Hand2Size[2], 4, 4, 4);
  LHandPos2 := VectorAdd(LHandPos, VectorMake
      (-0.5 * (Params.HandSize[0] + Params.Hand2Size[0]), 0, 0));
  RagDollVBOMesh[RagDollVBOMesh.Count - 1].Position := LHandPos2;
  // п.рука 2
  RagDollVBOMesh.AddBox(Params.Hand2Size[0], Params.Hand2Size[1],
    Params.Hand2Size[2], 4, 4, 4);
  RHandPos2 := VectorAdd
    (RHandPos, VectorMake(0.5 * (Params.HandSize[0] + Params.Hand2Size[0]), 0,
      0));
  RagDollVBOMesh[RagDollVBOMesh.Count - 1].Position := RHandPos2;
  // л.нога 2
  RagDollVBOMesh.AddBox(Params.Foot2Size[0], Params.Foot2Size[1],
    Params.Foot2Size[2], 4, 4, 4);
  LFootPos2 := VectorAdd(LFootPos, VectorMake
      (0, -0.5 * (Params.Foot2Size[1] + Params.FootSize[1]), 0));
  RagDollVBOMesh[RagDollVBOMesh.Count - 1].Position := LFootPos2;
  // п.нога 2
  RagDollVBOMesh.AddBox(Params.Foot2Size[0], Params.Foot2Size[1],
    Params.Foot2Size[2], 4, 4, 4);
  RFootPos2 := VectorAdd(RFootPos, VectorMake
      (0, -0.5 * (Params.Foot2Size[1] + Params.FootSize[1]), 0));
  RagDollVBOMesh[RagDollVBOMesh.Count - 1].Position := RFootPos2;
  // тело 2
  RagDollVBOMesh.AddBox(Params.Body2Size[0], Params.Body2Size[1],
    Params.Body2Size[2], 4, 4, 4);
  RagDollVBOMesh[RagDollVBOMesh.Count - 1].Position := Body2Pos;
  // физика
  AddConvexBody(RagDollVBOMesh[RagDollVBOMesh.Count - 1], 35 * Scale);
  AddConvexBody(RagDollVBOMesh[RagDollVBOMesh.Count - 2], 100 * Scale);
  AddConvexBody(RagDollVBOMesh[RagDollVBOMesh.Count - 3], 50 * Scale);
  AddConvexBody(RagDollVBOMesh[RagDollVBOMesh.Count - 4], 50 * Scale);
  AddConvexBody(RagDollVBOMesh[RagDollVBOMesh.Count - 5], 85 * Scale);
  AddConvexBody(RagDollVBOMesh[RagDollVBOMesh.Count - 6], 85 * Scale);
  AddConvexBody(RagDollVBOMesh[RagDollVBOMesh.Count - 7], 40 * Scale);
  AddConvexBody(RagDollVBOMesh[RagDollVBOMesh.Count - 8], 40 * Scale);
  AddConvexBody(RagDollVBOMesh[RagDollVBOMesh.Count - 9], 45 * Scale);
  AddConvexBody(RagDollVBOMesh[RagDollVBOMesh.Count - 10], 45 * Scale);
  AddConvexBody(RagDollVBOMesh[RagDollVBOMesh.Count - 11], 90 * Scale);
  // джоинты
  HeadPos := VectorAdd(BodyPosition, VectorMake(0, 0.5 * Params.BodySize[1], 0)
    );
  AddBallSocketJoint(NewtonObjects[NewtonObjectsCount - 2],
    NewtonObjects[NewtonObjectsCount - 1], HeadPos, PinUP, Gr2Rad(10), Gr2Rad
      (45));
  LHandPos := VectorAdd(BodyPosition, VectorMake(-0.5 * Params.BodySize[0],
      0.25 * Params.BodySize[1], 0));
  AddBallSocketJoint(NewtonObjects[NewtonObjectsCount - 2],
    NewtonObjects[NewtonObjectsCount - 3], LHandPos, PinLEFT, Gr2Rad(90), Gr2Rad
      (10));
  RHandPos := VectorAdd(BodyPosition, VectorMake(0.5 * Params.BodySize[0],
      0.25 * Params.BodySize[1], 0));
  AddBallSocketJoint(NewtonObjects[NewtonObjectsCount - 2],
    NewtonObjects[NewtonObjectsCount - 4], RHandPos, PinRIGHT, Gr2Rad(90),
    Gr2Rad(10));
  LFootPos := VectorAdd(Body2Pos, VectorMake(-0.25 * Params.Body2Size[0],
      0.5 * -Params.Body2Size[1], 0));
  AddBallSocketJoint(NewtonObjects[NewtonObjectsCount - 11],
    NewtonObjects[NewtonObjectsCount - 5], LFootPos, PinDOWN, Gr2Rad(45), Gr2Rad
      (5));
  RFootPos := VectorAdd(Body2Pos, VectorMake(0.25 * Params.Body2Size[0],
      0.5 * -Params.Body2Size[1], 0));
  AddBallSocketJoint(NewtonObjects[NewtonObjectsCount - 11],
    NewtonObjects[NewtonObjectsCount - 6], RFootPos, PinDOWN, Gr2Rad(45), Gr2Rad
      (5));
  LHandPos2 := VectorAdd(LHandPos, VectorMake(-Params.HandSize[0], 0, 0));
  AddBallSocketJoint(NewtonObjects[NewtonObjectsCount - 3],
    NewtonObjects[NewtonObjectsCount - 7], LHandPos2, PinLEFT, Gr2Rad(90),
    Gr2Rad(10));
  RHandPos2 := VectorAdd(RHandPos, VectorMake(Params.HandSize[0], 0, 0));
  AddBallSocketJoint(NewtonObjects[NewtonObjectsCount - 4],
    NewtonObjects[NewtonObjectsCount - 8], RHandPos2, PinRIGHT, Gr2Rad(90),
    Gr2Rad(10));
  LFootPos2 := VectorAdd(LFootPos, VectorMake(0, -Params.FootSize[1], 0));
  AddBallSocketJoint(NewtonObjects[NewtonObjectsCount - 5],
    NewtonObjects[NewtonObjectsCount - 9], LFootPos2, PinDOWN, Gr2Rad(45),
    Gr2Rad(10));
  RFootPos2 := VectorAdd(RFootPos, VectorMake(0, -Params.FootSize[1], 0));
  AddBallSocketJoint(NewtonObjects[NewtonObjectsCount - 6],
    NewtonObjects[NewtonObjectsCount - 10], RFootPos2, PinDOWN, Gr2Rad(45),
    Gr2Rad(10));
  Body2Pos := VectorAdd(BodyPosition, VectorMake(0,
      -0.5 * Params.BodySize[1], 0));
  AddBallSocketJoint(NewtonObjects[NewtonObjectsCount - 11],
    NewtonObjects[NewtonObjectsCount - 1], Body2Pos, PinUP, Gr2Rad(10), Gr2Rad
      (10));
  result.Head := NewtonObjects[NewtonObjectsCount - 1];
  result.Body := NewtonObjects[NewtonObjectsCount - 2];
  result.LHand := NewtonObjects[NewtonObjectsCount - 3];
  result.RHand := NewtonObjects[NewtonObjectsCount - 4];
  result.LFoot := NewtonObjects[NewtonObjectsCount - 5];
  result.RFoot := NewtonObjects[NewtonObjectsCount - 6];
  result.LHand2 := NewtonObjects[NewtonObjectsCount - 7];
  result.RHand2 := NewtonObjects[NewtonObjectsCount - 8];
  result.LFoot2 := NewtonObjects[NewtonObjectsCount - 9];
  result.RFoot2 := NewtonObjects[NewtonObjectsCount - 10];
  result.Body2 := NewtonObjects[NewtonObjectsCount - 11];
end;

function TVBONewtonWorld.AddRagDoll(BodyPosition: TVector4f; Params: TNewtonRagDollParams):TNewtonRagDoll;
var
  M : TMatrix;
  HeadPos, LHandPos, RHandPos, LFootPos, RFootPos, LHandPos2, RHandPos2, LFootPos2, RFootPos2: TVector4f;
  Head, Body, LHand, RHand, LFoot, RFoot, LHand2, RHand2, LFoot2, RFoot2 :TVBONewtonMesh;
begin
  M[0] := VectorMake(1, 0, 0, 0);
  M[1] := VectorMake(0, 1, 0, 0);
  M[2] := VectorMake(0, 0, 1, 0);
  HeadPos := VectorAdd(BodyPosition, VectorMake(0, 0.5 * (Params.BodySize[1] + Params.HeadSize[0]), 0));
  M[3] := HeadPos;
  Head := AddSphere(Params.HeadSize[0], Params.HeadSize[1], Params.HeadSize[2], 250, M);
  M[3] := BodyPosition;
  Body := AddBox(Params.BodySize[0], Params.BodySize[1], Params.BodySize[2], 500, M);
  LHandPos := VectorAdd(BodyPosition,
   VectorMake(-0.5 * (Params.BodySize[0] + Params.HandSize[0]), 0.25 * Params.BodySize[1], 0));
  M[3] := LHandPos;
  LHand := AddBox(Params.HandSize[0], Params.HandSize[1],
    Params.HandSize[2], 150, M);
  RHandPos := VectorAdd(BodyPosition, VectorMake
      (0.5 * (Params.BodySize[0] + Params.HandSize[0]),
       0.25 * Params.BodySize[1], 0));
  M[3] := RHandPos;
  RHand := AddBox(Params.HandSize[0], Params.HandSize[1], Params.HandSize[2], 150, M);
  LFootPos := VectorAdd(BodyPosition, VectorMake(-0.25 * Params.BodySize[0],
      -0.5 * (Params.BodySize[1] + Params.FootSize[1]), 0));
  M[3] := LFootPos;
  LFoot := AddBox(Params.FootSize[0], Params.FootSize[1], Params.FootSize[2], 300, M);
  RFootPos := VectorAdd(BodyPosition, VectorMake(0.25 * Params.BodySize[0],
      -0.5 * (Params.BodySize[1] + Params.FootSize[1]), 0));
  M[3] := RFootPos;
  RFoot := AddBox(Params.FootSize[0], Params.FootSize[1], Params.FootSize[2], 300, M);
  LHandPos2 := VectorAdd(LHandPos, VectorMake
      (-0.5 * (Params.HandSize[0] + Params.Hand2Size[0]), 0, 0));
  M[3] := LHandPos2;
  LHand2 := AddBox(Params.Hand2Size[0], Params.Hand2Size[1], Params.Hand2Size[2], 100, M);
  RHandPos2 := VectorAdd
    (RHandPos, VectorMake(0.5 * (Params.HandSize[0] + Params.Hand2Size[0]), 0, 0));
  M[3] := RHandPos2;
  RHand2 := AddBox(Params.Hand2Size[0], Params.Hand2Size[1],
    Params.Hand2Size[2], 100, M);
  LFootPos2 := VectorAdd(LFootPos, VectorMake
      (0, -0.5 * (Params.Foot2Size[1] + Params.FootSize[1]), 0));
  M[3] := LFootPos2;
  LFoot2 := AddBox(Params.Foot2Size[0], Params.Foot2Size[1], Params.Foot2Size[2], 180, M);
  RFootPos2 := VectorAdd(RFootPos, VectorMake
      (0, -0.5 * (Params.Foot2Size[1] + Params.FootSize[1]), 0));
  M[3] := RFootPos2;
  RFoot2 := AddBox(Params.Foot2Size[0], Params.Foot2Size[1], Params.Foot2Size[2], 180, M);

  HeadPos := VectorAdd(BodyPosition, VectorMake(0, 0.5 * Params.BodySize[1], 0));
  AddBallSocketJoint(Body, Head, HeadPos, PinUP, Gr2Rad(10), Gr2Rad
      (45));
  LHandPos := VectorAdd(BodyPosition, VectorMake(-0.5 * Params.BodySize[0],
      0.25 * Params.BodySize[1], 0));
  AddBallSocketJoint(Body, LHand, LHandPos, PinLEFT, Gr2Rad(90), Gr2Rad(10));
  RHandPos := VectorAdd(BodyPosition, VectorMake(0.5 * Params.BodySize[0],
      0.25 * Params.BodySize[1], 0));
  AddBallSocketJoint(Body, RHand, RHandPos, PinRIGHT, Gr2Rad(90),
    Gr2Rad(10));
  LFootPos := VectorAdd(BodyPosition, VectorMake(-0.25 * Params.BodySize[0],
      0.5 * -Params.BodySize[1], 0));
  AddBallSocketJoint(Body, LFoot, LFootPos, PinDOWN, Gr2Rad(45), Gr2Rad
      (5));
  RFootPos := VectorAdd(BodyPosition, VectorMake(0.25 * Params.BodySize[0],
      0.5 * -Params.BodySize[1], 0));
  AddBallSocketJoint(Body, RFoot, RFootPos, PinDOWN, Gr2Rad(45), Gr2Rad
      (5));
  LHandPos2 := VectorAdd(LHandPos, VectorMake(-Params.HandSize[0], 0, 0));
  AddBallSocketJoint(Body, LHand2, LHandPos2, PinLEFT, Gr2Rad(90),
    Gr2Rad(10));
  RHandPos2 := VectorAdd(RHandPos, VectorMake(Params.HandSize[0], 0, 0));
  AddBallSocketJoint(Body, RHand2, RHandPos2, PinRIGHT, Gr2Rad(90),
    Gr2Rad(10));
  LFootPos2 := VectorAdd(LFootPos, VectorMake(0, -Params.FootSize[1], 0));
  AddBallSocketJoint(Body, LFoot2, LFootPos2, PinDOWN, Gr2Rad(45),
    Gr2Rad(10));
  RFootPos2 := VectorAdd(RFootPos, VectorMake(0, -Params.FootSize[1], 0));
  AddBallSocketJoint(Body, RFoot2, RFootPos2, PinDOWN, Gr2Rad(45),
    Gr2Rad(10));
  result.Head := Head;
  result.Body := Body;
  result.LHand := LHand;
  result.RHand := RHand;
  result.LFoot := LFoot;
  result.RFoot := RFoot;
  result.LHand2 := LHand2;
  result.RHand2 := RHand2;
  result.LFoot2 := LFoot2;
  result.RFoot2 := RFoot2;
end;

function TVBONewtonWorld.AddSphere(RadX, RadY, RadZ, Weight : Single; Matrix:TMatrix):TVBONewtonMesh;
var
  col: pnewtoncollision;
  Inertia, BBox: TAffineVector;
begin
  BBox := AffineVectorMake(RadX, RadY, RadZ);
  Inertia[0] := Weight * (BBox[1] * BBox[1] + BBox[2] * BBox[2]) / 12;
  Inertia[1] := Weight * (BBox[0] * BBox[0] + BBox[2] * BBox[2]) / 12;
  Inertia[2] := Weight * (BBox[0] * BBox[0] + BBox[1] * BBox[1]) / 12;
  col := NewtonCreateSphere(SceneNewtonWorld, RadX, RadY, RadZ, 0, nil);
  FMeshBodyList.Add(TVBONewtonMesh.Create);
  LastObject.NewtonBody := NewtonCreateBody(SceneNewtonWorld, col);
  LastObject.NewtonWorld := SceneNewtonWorld;
  LastObject.MeshObject := nil;
  LastObject.BBox := BBox;
  NewtonReleaseCollision(SceneNewtonWorld, col);
  NewtonBodySetMassMatrix(LastObject.NewtonBody, Weight, Inertia[0], Inertia[1], Inertia[2]);
  NewtonBodySetMatrix(LastObject.NewtonBody, @Matrix);
  NewtonBodySetForceAndTorqueCallback(LastObject.NewtonBody, dNewtonApplyForceAndTorque);
  NewtonBodySetLinearDamping(LastObject.NewtonBody, 1);
  result := LastObject;
end;

function TVBONewtonWorld.AddBox(X: Single; Y: Single; Z: Single; Weight: Single; Matrix: TMatrix4f):TVBONewtonMesh;
var
  col: pnewtoncollision;
  Inertia, BBox: TAffineVector;
begin
  BBox := AffineVectorMake(X, Y, Z);
  Inertia[0] := Weight * (BBox[1] * BBox[1] + BBox[2] * BBox[2]) / 12;
  Inertia[1] := Weight * (BBox[0] * BBox[0] + BBox[2] * BBox[2]) / 12;
  Inertia[2] := Weight * (BBox[0] * BBox[0] + BBox[1] * BBox[1]) / 12;
  col := NewtonCreateBox(SceneNewtonWorld, X, Y, Z, 0, nil);
  FMeshBodyList.Add(TVBONewtonMesh.Create);
  LastObject.NewtonBody := NewtonCreateBody(SceneNewtonWorld, col);
  LastObject.NewtonWorld := SceneNewtonWorld;
  LastObject.MeshObject := nil;
  LastObject.BBox := BBox;
  NewtonReleaseCollision(SceneNewtonWorld, col);
  NewtonBodySetMassMatrix(LastObject.NewtonBody, Weight, Inertia[0], Inertia[1], Inertia[2]);
  NewtonBodySetMatrix(LastObject.NewtonBody, @Matrix);
  NewtonBodySetForceAndTorqueCallback(LastObject.NewtonBody, dNewtonApplyForceAndTorque);
  NewtonBodySetLinearDamping(LastObject.NewtonBody, 1);
  result := LastObject;
end;

function FCopy(str: string; N, K: integer): string;
var
  i: integer;
begin
  for i := N to K do
    result := result + str[i];
end;

procedure dNewtonApplyForceAndTorque(const Body: PNewtonBody; timestep: Float;
  threadIndex: int); cdecl;
var
  F: TVector4f;
begin
  NewtonBodyGetMassMatrix(Body, @F[3], @F[0], @F[1], @F[2]);
  F := VectorMake(0, NewtonGravity * F[3], 0);
  NewtonBodyAddForce(Body, @F);
end;

end.
