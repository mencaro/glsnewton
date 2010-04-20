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

    Procedure RenderObject(var aRenderInfo: TRenderContextInfo); override;

    Function TestHitPoint(const aPoint: TVector): boolean; override;
    Function P_LocalToWorld(const aPoint: TVector): TVector; override;  // Point from Local To World
    Function P_WorldToLocal(const aPoint: TVector): TVector; override;  // Point from World To Local
    Function GetTriMesh: TAffineVectorList;override;
    Constructor Create(aParent: TBaseGraphObject);
    Destructor Destroy; override;
  end;
//==============================================================================
implementation
uses
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
