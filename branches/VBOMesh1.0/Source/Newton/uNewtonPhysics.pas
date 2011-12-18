unit uNewtonPhysics;

interface
uses
  VectorGeometry, NewtonImport,
  uBasePhysics;

type
  TNewtonWorld = class;
//==============================================================================
  TNewtonPhysicObject = class(TBasePhysicObject)
  protected
    fNewtonBody: PNewtonBody;
    fNewtonCollision: PNewtonCollision;
  protected
  // от TBaseScreenObject
    procedure SetPosition(const aPosition: TVector); override;
    procedure SetRotation(const aRotation: TMatrix); override;
    procedure SetSizes   (const aSizes   : TVector); override;

    function GetPosition: TVector; override;
    function GetRotation: TMatrix; override;
    function GetSizes   : TVector; override;
  protected
  // от TBasePhysicObject
    procedure SetMass(const aMass: Single);            override;
    procedure SetLinearVel(const aVelocity: TVector);  override;
    procedure SetAngularVel(const aVelocity: TVector); override;
    procedure SetStatic(const aStatic: Boolean);       override;

    function GetMass    : Single;    override;
    function GetLinearVel: TVector;  override;
    function GetAngularVel: TVector; override;
    function GetStatic  : Boolean;   override;
  public
  // от TBaseScreenObject
    Function TestHitPoint(const aPoint: TVector): boolean;   override;
    Function P_LocalToWorld(const aPoint: TVector): TVector; override;
    Function P_WorldToLocal(const aPoint: TVector): TVector; override;
  public
  // от TBasePhysicObject
    Procedure AddForce         (const aForce: TVector);            override;
    Procedure AddForceAtPos    (const aPosition, aForce: TVector); override;
    Procedure AddTorque        (const aTorque: TVector);           override;
    Procedure ApplyImpulse     (const aImpulse: TVector);          override;
    Procedure ApplyImpulseAtPos(const aImpulse, aPos: TVector);    override;
  public
  // своё
    Procedure DoProgress(const DeltaTime: Single); override;
    Procedure InitNewtonBody(aNewtonWorld: TNewtonWorld; aNewtonCollision: PNewtonCollision); virtual;
//    Constructor Create(aNewtonWorld: TNewtonWorld; aNewtonCollision: PNewtonCollision);
    Destructor Destroy; override;
  end;
//==============================================================================
  TNewtonWorld = class(TPhysicWorld)
  protected
    fNewtonWorld: PNewtonWorld;
    fGravity: TVector;
    
    Procedure PhysicUpdate(const FixedDeltaTime: single); override;

    Function CreateNewtonObject(aNewtonCollision: PNewtonCollision): TNewtonPhysicObject;
  public
    property NewtonWorld: PNewtonWorld read fNewtonWorld;
    Function CreateSimplePhysicSphere(const aSizes: TVector): TBasePhysicObject; override;
    Function CreateSimplePhysicBox   (const aSizes: TVector): TBasePhysicObject; override;

    Constructor Create(const aPhysicStep: Single; const aGravity: TVector);
    Destructor Destroy; override;
  end;
//==============================================================================
implementation

//==============================================================================

{TNewtonPhysicObject}

procedure TNewtonPhysicObject.SetPosition(const aPosition: TVector);
var
  BodyMatrix: TMatrix;
begin
  NewtonBodyGetMatrix(fNewtonBody, @BodyMatrix);
  BodyMatrix[3] := aPosition;
  NewtonBodySetMatrix(fNewtonBody, @BodyMatrix);
end;

procedure TNewtonPhysicObject.SetRotation(const aRotation: TMatrix);
begin
  NewtonBodySetMatrix(fNewtonBody, @aRotation);
end;

procedure TNewtonPhysicObject.SetSizes   (const aSizes   : TVector);
begin
end;

function TNewtonPhysicObject.GetPosition: TVector;
var
  BodyMatrix: TMatrix;
begin
  NewtonBodyGetMatrix(fNewtonBody, @BodyMatrix);
  result := BodyMatrix[3];
end;

function TNewtonPhysicObject.GetRotation: TMatrix;
begin
  NewtonBodyGetMatrix(fNewtonBody, @Result);
//  NewtonBodyGetRotation
end;

function TNewtonPhysicObject.GetSizes   : TVector;
begin
  result := VectorMake(1, 1, 1, 1);
end;

procedure TNewtonPhysicObject.SetMass(const aMass: Single);
begin
  NewtonBodySetMassMatrix(fNewtonBody, aMass, 1, 1, 1); // <--todo: надо бы посчитать тензор инерции
end;

procedure TNewtonPhysicObject.SetLinearVel(const aVelocity: TVector);
begin
  NewtonBodySetVelocity(fNewtonBody, @aVelocity[0]);
end;

procedure TNewtonPhysicObject.SetAngularVel(const aVelocity: TVector);
begin
  NewtonBodySetOmega(fNewtonBody, @aVelocity[0]);
end;

procedure TNewtonPhysicObject.SetStatic(const aStatic: Boolean);
begin
  if aStatic then
    SetMass(0)
  else
    SetMass(1); // наверно, стоит кешировать последнее значение внутри класса, но пока не критично
end;

function TNewtonPhysicObject.GetMass    : Single;
var
  I: Float;
begin
  NewtonBodyGetMassMatrix(fNewtonBody, @Result, @I, @I, @I);
end;

function TNewtonPhysicObject.GetLinearVel: TVector;
begin
  NewtonBodyGetVelocity(fNewtonBody, @Result);
end;

function TNewtonPhysicObject.GetAngularVel: TVector;
begin
  NewtonBodyGetOmega(fNewtonBody, @Result);
end;

function TNewtonPhysicObject.GetStatic  : Boolean;
begin
  result := GetMass = 0;
end;

Function TNewtonPhysicObject.TestHitPoint(const aPoint: TVector): boolean;
begin
  result := false;
end;

Function TNewtonPhysicObject.P_LocalToWorld(const aPoint: TVector): TVector;
begin
end;

Function TNewtonPhysicObject.P_WorldToLocal(const aPoint: TVector): TVector;
begin
end;

Procedure TNewtonPhysicObject.AddForce(const aForce: TVector);
begin
  NewtonBodyAddForce(fNewtonBody, @aForce[0]);
end;

Procedure TNewtonPhysicObject.AddForceAtPos(const aPosition, aForce: TVector);
begin
end;

Procedure TNewtonPhysicObject.AddTorque     (const aTorque: TVector);
begin
  NewtonBodyAddTorque(fNewtonBody, @aTorque[0]);
end;

Procedure TNewtonPhysicObject.ApplyImpulse     (const aImpulse: TVector);
begin
//  NewtonBodyAddImpulse();
end;

Procedure TNewtonPhysicObject.ApplyImpulseAtPos(const aImpulse, aPos: TVector);
begin
  NewtonBodyAddImpulse(fNewtonBody, @aImpulse[0], @aPos[0]);
end;

Procedure TNewtonPhysicObject.DoProgress(const DeltaTime: Single);
begin
  inherited;
  PositionGraphObject;
end;

Procedure TNewtonPhysicObject.InitNewtonBody(aNewtonWorld: TNewtonWorld; aNewtonCollision: PNewtonCollision);
begin
  fNewtonCollision := aNewtonCollision;
  fNewtonBody := NewtonCreateBody(aNewtonWorld.NewtonWorld, aNewtonCollision);
  NewtonBodySetUserData(fNewtonBody, Self);
end;

Destructor TNewtonPhysicObject.Destroy;
begin
  NewtonDestroyBody(NewtonBodyGetWorld(fNewtonBody), fNewtonBody);
  inherited;
end;

//==============================================================================

{CallBack's}

Procedure CallBackAddForce(const aNewtonBody: PNewtonBody; aTimeStep: Float; aThreadIndex: int); cdecl; // <-- CallBack
var
  aForce: TVector;
  Mass, I: Float;
begin
  //узнаем массу объекта
  NewtonBodyGetMassMatrix(aNewtonBody, @Mass, @I, @I, @I);
  // добираемся до класса мира
  aForce := TNewtonWorld(NewtonWorldGetUserData(NewtonBodyGetWorld(aNewtonBody))).fGravity;
  // масса * гравитацию
  aForce := VectorScale(aForce, Mass);
  // прикладываем силу
  NewtonBodyAddForce(aNewtonBody, @aForce);
end;

//==============================================================================

{TNewtonWorld}

Procedure TNewtonWorld.PhysicUpdate(const FixedDeltaTime: single);
begin
  NewtonUpdate(fNewtonWorld, FixedDeltaTime);
end;

Function TNewtonWorld.CreateNewtonObject(aNewtonCollision: PNewtonCollision): TNewtonPhysicObject;
begin
  result := TNewtonPhysicObject.Create;
  result.InitNewtonBody(self, aNewtonCollision);
  NewtonBodySetForceAndTorqueCallBack(Result.fNewtonBody, CallBackAddForce);
end;

Function TNewtonWorld.CreateSimplePhysicSphere(const aSizes: TVector): TBasePhysicObject;
begin
  result := CreateNewtonObject(NewtonCreateSphere(fNewtonWorld, aSizes[0], aSizes[1], aSizes[2], 0, nil));
end;

Function TNewtonWorld.CreateSimplePhysicBox  (const aSizes: TVector): TBasePhysicObject;
begin
  result := CreateNewtonObject(NewtonCreateBox(fNewtonWorld, aSizes[0], aSizes[1], aSizes[2], 0, nil));
end;

Constructor TNewtonWorld.Create(const aPhysicStep: Single; const aGravity: TVector);
begin
  inherited Create(aPhysicStep, aGravity);
  fNewtonWorld := NewtonCreate(nil, nil);
  NewtonWorldSetUserData(fNewtonWorld, self);
  fGravity := aGravity;
end;

Destructor TNewtonWorld.Destroy;
begin
  NewtonDestroy(fNewtonWorld);
  inherited;
end;

//==============================================================================
end.
