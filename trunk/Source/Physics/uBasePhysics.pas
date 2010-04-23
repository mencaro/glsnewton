unit uBasePhysics;

interface
uses
  VectorGeometry, VectorLists, GLRenderContextInfo,
  uUtils;
type
//==============================================================================
// базовый объект, сгодится для всего
  TBaseGameObject = class
  public
    Function  DoCommand(const aCommand: cardinal; const aData: array of Single): boolean; virtual;
    Function  ExecuteCommand(const aCommand: String; const aValue: String): boolean; virtual;
    Procedure DoProgress(const DeltaTime: single); virtual; 
  end;
//==============================================================================
// объект, отображаемый на экране, от него идет разделение на физику и графику
  TBaseScreenObject = class(TBaseGameObject)
  protected
    procedure SetPosition(aPosition: TVector); virtual; abstract;
    procedure SetRotation(aRotation: TMatrix); virtual; abstract;
    procedure SetSizes   (aSizes   : TVector); virtual; abstract;

    function GetPosition: TVector; virtual; abstract;
    function GetRotation: TMatrix; virtual; abstract;
    function GetSizes   : TVector; virtual; abstract;
  public
    property Position : TVector read GetPosition write SetPosition;
    property Rotation : TMatrix read GetRotation write SetRotation;
    property Sizes    : TVector read GetSizes write SetSizes; // по сути Scale, но это пока не важно

    // в мировых координатах
    Function TestHitPoint(const aPoint: TVector): boolean; virtual; abstract;  // рэйкаст из камеры
    Function P_LocalToWorld(const aPoint: TVector): TVector; virtual; abstract;// Point from Local To World
    Function P_WorldToLocal(const aPoint: TVector): TVector; virtual; abstract;// Point from World To Local
  end;
//==============================================================================
// граф. объект
  TBaseGraphObject = class(TBaseScreenObject)
  protected
    fParent: TBaseGraphObject;
    fTriMesh: TAffineVectorList;
  protected
//    fGlObject: TGlCustomSceneObject;  // или же лучше сделать надстройку над TVBOMeshObject
    function GetVisible: boolean; virtual; abstract;
    procedure SetVisible(const aVisible: boolean);virtual; abstract;
    procedure SetParent(aNewParent: TBaseGraphObject);virtual; abstract;
    function GetExtents: TExtents; virtual; abstract;
    function GetMatrix: TMatrix; virtual; abstract;
    procedure SetMatrix(const aMatrix: TMatrix);virtual; abstract;
    function GLVisible: boolean; virtual; abstract;
  public
//    property GlObject: TGlCustomSceneObject read fGlObject;
    property Parent: TBaseGraphObject read fParent write SetParent;
    property IsVisible: Boolean read GetVisible write SetVisible;
    property Extents: TExtents read GetExtents;
    property Matrix: TMatrix read GetMatrix write SetMatrix;
    function WantGraphUpdate: boolean; virtual; abstract;

    Procedure RenderObject(var aRenderInfo: TRenderContextInfo); virtual; abstract;

    Function TestHitPoint(const aPoint: TVector): boolean; override; abstract;
    Function P_LocalToWorld(const aPoint: TVector): TVector; override; abstract; // Point from Local To World
    Function P_WorldToLocal(const aPoint: TVector): TVector; override; abstract; // Point from World To Local
    Function GetTriMesh: TAffineVectorList;virtual; abstract;
    Constructor Create(aParent: TBaseGraphObject);
    Destructor Destroy; override;
  end;
//==============================================================================
// тип для сочленений
  TBaseJointObject = class(TBaseGameObject)
  protected
    fGraphObject: TBaseGraphObject;
    fJointType: Byte;
    Function GetAnchor1: TVector; virtual; abstract;
    Function GetAnchor2: TVector; virtual; abstract;
  public
    property GraphObject: TBaseGraphObject read fGraphObject write fGraphObject;
    property JointType: byte read fJointType;
    property Anchor1: TVector read GetAnchor1;
    property Anchor2: TVector read GetAnchor2;
    Constructor Create;
    Destructor Destroy; override;
  end;
//==============================================================================
// физический объект
  TBasePhysicObject = class(TBaseScreenObject)
  protected
    fGraphObject: TBaseGraphObject;
  protected
    procedure SetMass(const aMass: Single);            virtual; abstract;
    procedure SetLinearVel(const aVelocity: TVector);  virtual; abstract;
    procedure SetAngularVel(const aVelocity: TVector); virtual; abstract;
    procedure SetStatic(const aStatic: Boolean);       virtual; abstract;

    function GetMass    : Single;    virtual; abstract;
    function GetLinearVel: TVector;  virtual; abstract;
    function GetAngularVel: TVector; virtual; abstract;
    function GetStatic  : Boolean;   virtual; abstract;
  public
    property LinearVel: TVector read GetLinearVel write SetLinearVel;
    property AngularVel: TVector read GetLinearVel write SetAngularVel;
    property Mass: Single read GetMass write SetMass;
    property Static: boolean read GetStatic write SetStatic;
  public
    Procedure AddForce         (const aForce: TVector);            virtual; abstract;
    Procedure AddForceAtPos    (const aPosition, aForce: TVector); virtual; abstract;
    Procedure AddTorque        (const aTorque: TVector);           virtual; abstract;
    Procedure ApplyImpulse     (const aImpulse: TVector);          virtual; abstract;
    Procedure ApplyImpulseAtPos(const aImpulse, aPos: TVector);    virtual; abstract;
  public
    property GraphObject: TBaseGraphObject read fGraphObject;
    Procedure AttachGraphObject(aGraphObject: TBaseGraphObject); virtual;
    Procedure PositionGraphObject; virtual;
  end;
//==============================================================================
// физический мир
  TPhysicWorld = class(TBaseGameObject)
  protected
    fGlobalTime, fPhysicTime, fPhysicStep: Single;
    
    Procedure PhysicUpdate(const FixedDeltaTime: single); virtual; abstract;
  public
    Procedure DoProgress(const DeltaTime: Single); override; // Делаем шаг симуляции физики
    Function  CreateSimplePhysicSphere(const aSizes: TVector): TBasePhysicObject; virtual; abstract;
    Function  CreateSimplePhysicBox   (const aSizes: TVector): TBasePhysicObject; virtual; abstract;

    Constructor Create(const aPhysicStep: Single);
    Destructor Destroy; override;
  end;
//==============================================================================
implementation

Function  TBaseGameObject.DoCommand(const aCommand: cardinal; const aData: array of Single): boolean;
begin
  result := false;
end;

Function  TBaseGameObject.ExecuteCommand(const aCommand: String; const aValue: String): boolean;
begin
  result := false;
end;

Procedure TBaseGameObject.DoProgress(const DeltaTime: single);
begin
end;

{ TBaseGraphObject }

constructor TBaseGraphObject.Create(aParent: TBaseGraphObject);
begin
  fParent:=aParent;
  fTriMesh:=TAffineVectorList.Create;
end;

destructor TBaseGraphObject.Destroy;
begin
  fTriMesh.Free;
  inherited;
end;

{ TBaseJointObject }

constructor TBaseJointObject.Create;
begin
  inherited;
end;

destructor TBaseJointObject.Destroy;
begin
  inherited;
end;

{TBasePhysicObject}

Procedure TBasePhysicObject.AttachGraphObject(aGraphObject: TBaseGraphObject);
begin
  fGraphObject := aGraphObject;
end;

Procedure TBasePhysicObject.PositionGraphObject;
begin
  fGraphObject.Matrix := Rotation; 
end;

{TPhysicWorld}

Procedure TPhysicWorld.DoProgress(const DeltaTime: single);
begin
  fGlobalTime := fGlobalTime + DeltaTime;
  while fPhysicTime < fGlobalTime do
  begin
    fPhysicTime := fPhysicTime + fPhysicStep;
    PhysicUpdate(fPhysicStep);
  end;
end;

Constructor TPhysicWorld.Create(const aPhysicStep: Single);
begin
  inherited Create;
  fPhysicStep := aPhysicStep;
  fPhysicTime := 0;
  fGlobalTime := 0;
end;

Destructor TPhysicWorld.Destroy;
begin
  inherited;
end;

end.
