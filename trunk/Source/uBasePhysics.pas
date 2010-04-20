unit uBasePhysics;

interface
uses
  VectorGeometry, VectorLists, GLRenderContextInfo, GeometryBB,
  VBOMesh, uVBO,
  GLScene, GLVectorFileObjects;
type
//==============================================================================
// базовый объект, сгодится для всего
  TBaseGameObject = class
  public
    Function  DoCommand(const aCommand: cardinal; const aData: array of Single): boolean; virtual; abstract;
    Function  ExecuteCommand(const aCommand: String; const aValue: String): boolean; virtual; abstract;
    Procedure DoProgress(const DeltaTime: single); virtual; abstract;
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
  private
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
//    property ActionRegion: TActionRegion read fActionRegion;
    function WantGraphUpdate: boolean; virtual; abstract;

    Procedure PrepareBeforeRender; virtual; abstract; // для различных манипуляций перед рендером
    Procedure RenderObject(var aRenderInfo: TRenderContextInfo); virtual; abstract;

    Function TestHitPoint(const aPoint: TVector): boolean; override; abstract;
    Function P_LocalToWorld(const aPoint: TVector): TVector; override; abstract; // Point from Local To World
    Function P_WorldToLocal(const aPoint: TVector): TVector; override; abstract; // Point from World To Local
    Function GetTriMesh: TAffineVectorList;virtual; abstract;
    Constructor Create(aParent: TBaseGraphObject);
    Destructor Destroy; override;
  end;
//==============================================================================
// тип дя сочленений
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
    procedure SetMass(const aMass: Single);  virtual; abstract;
    procedure SetLinearVel(const aVelocity: TVector); virtual; abstract;
    procedure SetAngularVel(const aVelocity: TVector); virtual; abstract;
    procedure SetStatic(const aStatic: Boolean); virtual; abstract;

    function GetMass    : Single;  virtual; abstract;
    function GetLinearVel: TVector; virtual; abstract;
    function GetAngularVel: TVector; virtual; abstract;
    function GetStatic  : Boolean; virtual; abstract;
  public
    property LinearVel: TVector read GetLinearVel write SetLinearVel;
    property AngularVel: TVector read GetLinearVel write SetAngularVel;
    property Mass: Single read GetMass write SetMass;
    property Static: boolean read GetStatic write SetStatic;
  public
    Procedure AddForce(const aForce: TVector); virtual; abstract;
    Procedure AddForceAtPos(const aPosition, aForce: TVector); virtual; abstract;
    Procedure AddTorque     (const aTorque: Single);       virtual; abstract;
    Procedure ApplyImpulse     (const aImpulse: TVector);       virtual; abstract;
    Procedure ApplyImpulseAtPos(const aImpulse, aPos: TVector); virtual; abstract;
  end;
//==============================================================================

// Графический объект TVBOMeshObject 
  TVBOGraphObject = class(TBaseGraphObject)
  private
    fVBOObject: TVBOMeshObject;
  protected
    function GetVisible: boolean; override;
    procedure SetVisible(const aVisible: boolean);override;
    procedure SetParent(aNewParent: TBaseGraphObject);override;
    function GetExtents: TExtents;override;
    function GetMatrix: TMatrix;override;
    procedure SetMatrix(const aMatrix: TMatrix);override;

  public
//    property GlObject: TGlCustomSceneObject read fGlObject;
    property Parent: TBaseGraphObject read fParent write SetParent;
    property IsVisible: Boolean read GetVisible write SetVisible;
    property Extents: TExtents read GetExtents;
    property Matrix: TMatrix read GetMatrix write SetMatrix;

//    property ActionRegion: TActionRegion read fActionRegion;
//    function WantGraphUpdate: boolean; virtual; abstract;

//    Procedure PrepareBeforeRender; virtual; abstract; // для различных манипуляций перед рендером
    Procedure RenderObject(var aRenderInfo: TRenderContextInfo); override;

    Function TestHitPoint(const aPoint: TVector): boolean; override;
    Function P_LocalToWorld(const aPoint: TVector): TVector; override;  // Point from Local To World
    Function P_WorldToLocal(const aPoint: TVector): TVector; override;  // Point from World To Local
    Function GetTriMesh: TAffineVectorList;override;    
    Constructor Create(aParent: TBaseGraphObject);
    Destructor Destroy; override;
  end;

// Графический объект TGLSceneObject
  TSceneGraphObject = class(TBaseGraphObject)
  private
    fSceneObject: TGLBaseSceneObject;
  protected
    function GetVisible: boolean; override;
    procedure SetVisible(const aVisible: boolean);override;
    procedure SetParent(aNewParent: TBaseGraphObject);override;
    function GetExtents: TExtents;override;
    function GetMatrix: TMatrix;override;
    procedure SetMatrix(const aMatrix: TMatrix);override;
  public
//    property GlObject: TGlCustomSceneObject read fGlObject;
    property Parent: TBaseGraphObject read fParent write SetParent;
    property IsVisible: Boolean read GetVisible write SetVisible;
    property Extents: TExtents read GetExtents;
    property Matrix: TMatrix read GetMatrix write SetMatrix;

//    property ActionRegion: TActionRegion read fActionRegion;
//    function WantGraphUpdate: boolean; virtual; abstract;

//    Procedure PrepareBeforeRender; virtual; abstract; // для различных манипуляций перед рендером
    Procedure RenderObject(var aRenderInfo: TRenderContextInfo); override;

    Function TestHitPoint(const aPoint: TVector): boolean; override;
    Function P_LocalToWorld(const aPoint: TVector): TVector; override;  // Point from Local To World
    Function P_WorldToLocal(const aPoint: TVector): TVector; override;  // Point from World To Local
    Function GetTriMesh: TAffineVectorList;override;
    Constructor Create(aParent: TBaseGraphObject);
    Destructor Destroy; override;
  end;


implementation

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

{ TVBOGraphObject }

constructor TVBOGraphObject.Create(aParent: TBaseGraphObject);
begin
  inherited;
  fParent:=aParent;
end;

destructor TVBOGraphObject.Destroy;
begin
  inherited;
end;

function TVBOGraphObject.GetExtents: TExtents;
begin
  with fVBOObject do
  if not WorldMatrixUpdated then UpdateWorldMatrix;
  Result:= fVBOObject.Extents;
end;

function TVBOGraphObject.GetMatrix: TMatrix;
begin
  with fVBOObject do begin
    if not WorldMatrixUpdated then UpdateWorldMatrix;
    Result:= Matrices.WorldMatrix;
  end;
end;

function TVBOGraphObject.GetTriMesh: TAffineVectorList;
begin
   if fTriMesh.Count=0 then fVBOObject.GetTriMesh(fTriMesh);
   result:=fTriMesh;
end;

function TVBOGraphObject.GetVisible: boolean;
begin
  Result:= fVBOObject.Visible;
end;

function TVBOGraphObject.P_LocalToWorld(const aPoint: TVector): TVector;
begin
  result:=VectorTransform(aPoint,Matrix);
end;

function TVBOGraphObject.P_WorldToLocal(const aPoint: TVector): TVector;
begin
  result:=fVBOObject.AbsoluteToLocal(aPoint);
end;

procedure TVBOGraphObject.RenderObject(
  var aRenderInfo: TRenderContextInfo);
var view, proj: TMatrix;
begin
  view:=GetViewMatrix;
  proj:=GetProjectionMatrix;
  fVBOObject.Matrices.ProjectionMatrix:=proj;
  fVBOObject.RenderObject(aRenderInfo,view);
end;

procedure TVBOGraphObject.SetMatrix(const aMatrix: TMatrix);
begin
  with fVBOObject do begin
     ResetMatrices;
     Matrices.ModelMatrix:=aMatrix;
     UpdateWorldMatrix;
  end;
end;

procedure TVBOGraphObject.SetParent(aNewParent: TBaseGraphObject);
begin
  fParent:=aNewParent;
end;

procedure TVBOGraphObject.SetVisible(const aVisible: boolean);
begin
  fVBOObject.Visible:=aVisible;
end;

function TVBOGraphObject.TestHitPoint(const aPoint: TVector): boolean;
var aabb: TAABB;
begin
  aabb:=TAABB(Extents);
  result:=PointInAABB(aPoint,aabb);
end;

{ TSceneGraphObject }

constructor TSceneGraphObject.Create(aParent: TBaseGraphObject);
begin
  inherited;
  fParent:=aParent;
end;

destructor TSceneGraphObject.Destroy;
begin
  inherited;
end;

function TSceneGraphObject.GetExtents: TExtents;
var aabb: TAABB;
begin
  aabb:=fSceneObject.AxisAlignedBoundingBox;
  Result:=TExtents(aabb);
end;

function TSceneGraphObject.GetMatrix: TMatrix;
begin
  result:=fSceneObject.Matrix;
end;

function TSceneGraphObject.GetTriMesh: TAffineVectorList;
begin
  //Возвращается пустой список, так как нет информации о геометрии
   result:=fTriMesh;
end;

function TSceneGraphObject.GetVisible: boolean;
begin
  result:=fSceneObject.Visible;
end;

function TSceneGraphObject.P_LocalToWorld(const aPoint: TVector): TVector;
begin
  result:=fSceneObject.LocalToAbsolute(aPoint);
end;

function TSceneGraphObject.P_WorldToLocal(const aPoint: TVector): TVector;
begin
  result:=fSceneObject.AbsoluteToLocal(aPoint);
end;

procedure TSceneGraphObject.RenderObject(
  var aRenderInfo: TRenderContextInfo);
begin
  fSceneObject.Render(aRenderInfo);
end;

procedure TSceneGraphObject.SetMatrix(const aMatrix: TMatrix);
begin
  fSceneObject.Matrix:=aMatrix;
end;

procedure TSceneGraphObject.SetParent(aNewParent: TBaseGraphObject);
begin
  fParent:=aNewParent;
end;

procedure TSceneGraphObject.SetVisible(const aVisible: boolean);
begin
  fSceneObject.Visible:=aVisible;
end;

function TSceneGraphObject.TestHitPoint(const aPoint: TVector): boolean;
var aabb:TAABB;
begin
  aabb:=fSceneObject.AxisAlignedBoundingBox;
  result:=PointInAABB(aPoint,aabb);
end;

end.
