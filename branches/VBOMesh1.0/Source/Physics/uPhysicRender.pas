unit uPhysicRender;

interface
uses
  GlScene, VectorGeometry, GLRenderContextInfo, VectorLists, GeometryBB,
  uVbo, VBOMesh,
  uBasePhysics;
type
//==============================================================================
// Графический объект TVBOMeshObject
  TVBOGraphObject = class(TBaseGraphObject)
  protected
    fVBOObject: TVBOMeshObject;
  protected
    function GetVisible: boolean; override;
    procedure SetVisible(const aVisible: boolean);override;
    procedure SetParent(aNewParent: TBaseGraphObject);override;
    function GetExtents: TExtents;override;
    function GetMatrix: TMatrix;override;
    procedure SetMatrix(const aMatrix: TMatrix);override;
  public
    property Parent: TBaseGraphObject read fParent write SetParent;
    property IsVisible: Boolean read GetVisible write SetVisible;
    property Extents: TExtents read GetExtents;
    property Matrix: TMatrix read GetMatrix write SetMatrix;

    Procedure RenderObject(var aRenderInfo: TRenderContextInfo); override;

    Function TestHitPoint(const aPoint: TVector): boolean; override;
    Function P_LocalToWorld(const aPoint: TVector): TVector; override;  // Point from Local To World
    Function P_WorldToLocal(const aPoint: TVector): TVector; override;  // Point from World To Local
    Function GetTriMesh: TAffineVectorList;override;
    Constructor Create(aParent: TBaseGraphObject);
    Destructor Destroy; override;
  end;
//==============================================================================
// Графический объект TGLSceneObject
  TSceneGraphObject = class(TBaseGraphObject)
  private
    fSceneObject: TGLBaseSceneObject;
  protected
    procedure SetParent(aNewParent: TBaseGraphObject);override;

    procedure SetVisible (const aVisible: boolean);  override;
    procedure SetPosition(const aPosition: TVector); override;
    procedure SetMatrix  (const aMatrix: TMatrix);   override;
    procedure SetSizes   (const aSizes   : TVector); override;

    function GetVisible: boolean;  override;
    function GetPosition: TVector; override;
    function GetExtents: TExtents; override;
    function GetMatrix: TMatrix;   override;
    function GetSizes   : TVector; override;
  public
    property GlObject: TGLBaseSceneObject read fSceneObject write fSceneObject;
    property Parent: TBaseGraphObject read fParent write SetParent;
    property IsVisible: Boolean read GetVisible write SetVisible;
    property Extents: TExtents read GetExtents;
    property Matrix: TMatrix read GetMatrix write SetMatrix;

    Procedure RenderObject(var aRenderInfo: TRenderContextInfo); override;

    Function TestHitPoint(const aPoint: TVector): boolean; override;
    Function P_LocalToWorld(const aPoint: TVector): TVector; override;  // Point from Local To World
    Function P_WorldToLocal(const aPoint: TVector): TVector; override;  // Point from World To Local
    Function GetTriMesh: TAffineVectorList;override;
    Constructor Create(aParent: TBaseGraphObject);
    Destructor Destroy; override;
  end;
//==============================================================================
  TGlSceneGraphicWorld = class(TGraphicWorld)
  protected
    Function GetParentAsSceneObject: TSceneGraphObject;
    property ParentAsSceneObject: TSceneGraphObject read GetParentAsSceneObject;
  public
    Function  CreateSimpleGraphSphere(const aSizes: TVector): TBaseGraphObject; override;
    Function  CreateSimpleGraphBox   (const aSizes: TVector): TBaseGraphObject; override;
  end;
//==============================================================================
implementation
uses
  GlObjects, 
  uUtils;
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
  aabb:=FromExtentsToAABB(Extents);
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
  Result:=FromAABBToExtents(aabb);
end;

function TSceneGraphObject.GetMatrix: TMatrix;
begin
  result:=fSceneObject.Matrix;
end;

function TSceneGraphObject.GetSizes   : TVector;
begin
  result := fSceneObject.Scale.AsVector;
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

procedure TSceneGraphObject.SetSizes   (const aSizes   : TVector); 
begin
  fSceneObject.Scale.SetVector(aSizes);
end;

procedure TSceneGraphObject.SetPosition(const aPosition: TVector);
begin
  fSceneObject.Position.SetPoint(aPosition);
end;

function TSceneGraphObject.GetPosition: TVector;
begin
  result := fSceneObject.Position.AsVector;
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

//==============================================================================

{TGlSceneGraphicWorld}

Function TGlSceneGraphicWorld.GetParentAsSceneObject: TSceneGraphObject;
begin
  result := TSceneGraphObject(fGraphParent);
end;

Function  TGlSceneGraphicWorld.CreateSimpleGraphSphere(const aSizes: TVector): TBaseGraphObject;
var
  Obj: TSceneGraphObject;
begin
  Obj := TSceneGraphObject.Create(fGraphParent);
  Obj.GlObject := TGlSphere.CreateAsChild(GetParentAsSceneObject.GlObject);
  TGlSphere(Obj.GlObject).Radius := 1;
  Obj.Sizes := aSizes;
  result := Obj;
end;

Function  TGlSceneGraphicWorld.CreateSimpleGraphBox   (const aSizes: TVector): TBaseGraphObject;
var
  Obj: TSceneGraphObject;
begin
  Obj := TSceneGraphObject.Create(fGraphParent);
  Obj.GlObject := TGlCube.CreateAsChild(GetParentAsSceneObject.GlObject);
  Obj.Sizes := aSizes;
  result := Obj;
end;

//==============================================================================
end.
