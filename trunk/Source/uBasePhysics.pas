unit uBasePhysics;

interface
uses
  VectorGeometry;
type
//==============================================================================
// базовый объект, сгодится для всего
  TBaseGameObject = class
  public
    Function  DoCommand(const aCommand: cardinal; const aData: array of Single): boolean; virtual;
    Function  ExecuteCommand(const aCommand: String; const aValue: String): boolean;
    Procedure DoProgress(const DeltaTime: single); virtual;
  end;
//==============================================================================
// объект, отображаемый на экране, от него идет разделение на физику и графику
  TBaseScreenObject = class(TBaseGameObject)
  protected
    procedure SetPosition(aPosition: TVector); virtual;
    procedure SetRotation(aRotation: TMatrix); virtual;
    procedure SetSizes   (aSizes   : TVector); virtual;

    function GetPosition: TVector; virtual;
    function GetRotation: TMatrix; virtual;
    function GetSizes   : TVector; virtual;
  public
    property Position : TVector read GetPosition write SetPosition;
    property Rotation : TMatrix read GetRotation write SetRotation;
    property Sizes    : TVector read GetSizes write SetSizes; // по сути Scale, но это пока не важно

    // в мировых координатах
    Function TestHitPoint(const aPoint: TVector): boolean; virtual; // рэйкаст из камеры
    Function P_LocalToWorld(const aPoint: TVector): TVector; virtual; // Point from Local To World
    Function P_WorldToLocal(const aPoint: TVector): TVector; virtual; // Point from World To Local
  end;
//==============================================================================
// граф. объект
  TBaseGraphObject = class(TBaseScreenObject)
  protected
    fGlObject: TGlCustomSceneObject;  // или же лучше сделать надстройку над TVBOMeshObject
    function GetVisible: boolean; virtual;
    procedure SetVisible(const aVisible: boolean);virtual;
    procedure SetParent(aNewParent: TBaseGraphObject);
    function GLVisible: boolean; virtual;
  public
    property GlObject: TGlCustomSceneObject read fGlObject;
    property Parent: TBaseGraphObject read fParent write SetParent;
    property IsVisible: Boolean read GetVisible write SetVisible;
    property ActionRegion: TActionRegion read fActionRegion;
    function WantGraphUpdate: boolean; virtual;

    Procedure PrepareBeforeRender; virtual; // для различных манипуляций перед рендером
    Procedure RenderObject(var aRenderInfo: TRenderContextInfo); virtual;

    Function TestHitPoint(const aPoint: TVector): boolean; override;
    Function P_LocalToWorld(const aPoint: TVector): TVector; override; // Point from Local To World
    Function P_WorldToLocal(const aPoint: TVector): TVector; override; // Point from World To Local
    Constructor Create(aParent: TBaseGraphObject);
    Destructor Destroy; override;
  end;
//==============================================================================
// тип дя сочленений
  TBaseJointObject = class(TBaseGameObject)
  protected
    fGraphObject: TBaseGraphObject;
    fJointType: Byte;
    Function GetAnchor1: TVector; virtual;
    Function GetAnchor2: TVector; virtual;
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
    procedure SetMass(const aMass: Single);  virtual;
    procedure SetLinearVel(const aVelocity: TVector); virtual;
    procedure SetAngularVel(const aVelocity: TVector); virtual;
    procedure SetStatic(const aStatic: Boolean); virtual;

    function GetMass    : Single;  virtual;
    function GetLinearVel: TVector; virtual;
    function GetAngularVel: TVector; virtual;
    function GetStatic  : Boolean; virtual;
  public
    property LinearVel: TVector read GetLinearVel write SetLinearVel;
    property AngularVel: TVector read GetLinearVel write SetAngularVel;
    property Mass: Single read GetMass write SetMass;
    property Static: boolean read GetStatic write SetStatic;
  public
    Procedure AddForce(const aForce: TVector); virtual;
    Procedure AddForceAtPos(const aPosition, aForce: TVector); virtual;
    Procedure AddTorque     (const aTorque: Single);       virtual;
    Procedure ApplyImpulse     (const aImpulse: TVector);       virtual;
    Procedure ApplyImpulseAtPos(const aImpulse, aPos: TVector); virtual;
  end;
//==============================================================================
implementation

end.
