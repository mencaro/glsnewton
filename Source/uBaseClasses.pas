unit uBaseClasses;

interface

uses Classes,VectorGeometry, uMiscUtils;

Type
  TTransformsTypes = (ttPosition, ttScale, ttRotation, ttModel, ttParent, ttFollow, ttAll);
  TTransforms = set of TTransformsTypes;
  TMeshCollectionItem = (mcMeshObject, mcContainer, mcCollection, mcEffect, mcCommands, mcRender, mcUnknown);
  TProcessChilds = (pcNone, pcBefore, pcAfter);
  TSortDirection = (sdFrontToBack, sdBackToFront, sdNone);
  TNotification = (ntTransformationsChanged, ntRemoved, ntUnsibscribe);

  TViewerSettings = record
    ViewPort: THomogeneousIntVector;
    ViewMatrix: TMatrix;
    ProjectionMatrix: TMatrix;
    Frustum: TFrustum;
    CurrentTime: double;
  end;
  PViewerSettings = ^TViewerSettings;

  TMatrixStack = record
    //Матрицы трансформации
    ModelMatrix: TMatrix; //модельная матрица, хранит базовые трансформации объекта
    ScaleMatrix: TMatrix; //масштабная матрица
    RotationMatrix: TMatrix; //матрица поворота
    TranslationMatrix: TMatrix; //матрица переноса
    WorldMatrix: TMatrix; //результирующая мировая матрица
    WorldMatrixT: TMatrix; //транспонированная мировая матрица
    InvWorldMatrix: TMatrix;//обратная мировая матрица
    ProjectionMatrix: TMatrix; //Проекционная матрица, заполняется при рендеринге
    ViewMatrix: TMatrix; //Видовая матрица, заполняется при рендеринге
  end; PMatrixStack = ^TMatrixStack;

  TObjectRenderEvents = procedure (MeshObject: TObject) of object;
  TVBOMeshRenderEvents = procedure of object;
  TVBOObjectClickEvents = procedure (X,Y:integer; NearPos,inObjectPos,dir: TAffineVector; MeshObject: TObject) of object;
  TVBOVisibilityEvents = procedure (var Visible: boolean) of object;

  TVBOMeshItem = class
  private
    procedure setParent(const Value: TVBOMeshItem);
    procedure setChilde(const Value: TVBOMeshItem);
  protected
    FUseParentViewer: boolean;
    FParentViewer: PViewerSettings;
    FItemType: TMeshCollectionItem;
    FParent: TVBOMeshItem;
    FOwner: TVBOMeshItem;
    FName: string;
    FChilde: TVBOMeshItem;
    FProcessChilds: TProcessChilds;
    FSubscribers: TList;
    procedure Subscribe(aItem: TVBOMeshItem); virtual;
    procedure Notification(Sender: TVBOMeshItem; aMessage: TNotification); virtual;
    procedure DispatchNotification(aMessage: TNotification); virtual;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Process; virtual;abstract;
    property MeshItemType: TMeshCollectionItem read FItemType;
    property Name: string read FName write FName;
    property ProcessChilds: TProcessChilds read FProcessChilds write FProcessChilds;
    property UseParentViewer: boolean read FUseParentViewer write FUseParentViewer;
    property ParentViewer: PViewerSettings read FParentViewer write FParentViewer;
    property Childe: TVBOMeshItem read FChilde write setChilde;
    property Parent: TVBOMeshItem read FParent write setParent;
    property Owner: TVBOMeshItem read FOwner write FOwner;
  end;

  TRenderEventItem = class (TVBOMeshItem)
  private
    FRenderEvent: TObjectRenderEvents;
  public
    property RenderEvent: TObjectRenderEvents read FRenderEvent write FRenderEvent;
    procedure Process; override;
  end;

  TMovableObject = class (TVBOMeshItem)
  Private
    //координатный базис
    FAbsolutePosition: TVector;
    FPosition: TVector; //глобальные координаты объекта
    FScale: TVector;    //масштаб объекта, совместно с положением - только для чтения
    FUp: TVector; // OY
    FDirection: TVector; //OZ
    FLeft: TVector;
  Protected
    FRollAngle: single;
    FTurnAngle: single;
    FPitchAngle: single;
    FXRotationAngle: single;
    FYRotationAngle: single;
    FZRotationAngle: single;
    FParentMatrix: TMatrix;
    function getParent: TMovableObject;
    procedure SetParent(const Value: TMovableObject);
    procedure SetPosition(const Value: TVector);
    procedure SetScale(const Value: TVector);
    //Ориентирует объект в заданном направлении
    procedure SetDirection(const Direction: TVector);

    procedure Notification(Sender: TVBOMeshItem; aMessage: TNotification); override;
  Public
    FriendlyName: string; //храните любой текст или комментарии тут
    Tag: integer; //для нужд пользователя
    DirectingAxis: TVector; //Хранит направляющую ось Axis
    Matrices:TMatrixStack;

    WorldMatrixUpdated: boolean; //false=требуется перестроить мировую матрицу

    Constructor Create;
    Destructor Destroy;override;

    Procedure Process; override;

    //установка родителя, из которого будет браться базовая матрица трансформаций
    Property Parent: TMovableObject read getParent write SetParent;
    //Прямая установка родительской матрицы
    Property ParentMatrix: TMatrix read FParentMatrix write FParentMatrix;
    //Установка/чтение локального положения
    Property Position: TVector read FPosition write SetPosition;
    //Чтение абсолютного положения
    Property AbsolutePosition: TVector read FAbsolutePosition;
    //Установка/чтение масштаба объекта
    Property Scale: TVector read FScale write SetScale;
    //Угол поворота в плоскости экрана
    Property RollAngle: single read FRollAngle write FRollAngle;
    //Установка/чтение ориентации объекта
    Property Direction: TVector read Matrices.WorldMatrix[2] write SetDirection;
    Property Left: TVector read Matrices.WorldMatrix[0];
    Property UP: TVector read Matrices.WorldMatrix[1];

    //Вращение относительно локальных осей
    Procedure TurnObject(Angle:single);  //Вокруг локальной оси Y
    Procedure RollObject(Angle:single);  //Вокруг локальной оси Z
    Procedure PitchObject(Angle:single); //Вокруг локальной оси X
    //Передвигает объект вдоль оси Direction
    Procedure MoveForward(Step:single);
    //Передвигает объект вдоль оси Left
    Procedure MoveLeft(Step:single);
    //Передвигает объект вдоль оси Up
    Procedure MoveUp(Step:single);
    //формирует матрицу поворота, при AbsoluteRotation=false модифицируется существующая
    Procedure RotateObject(const Axis: TVector; Angle: single; AbsoluteRotation: boolean=true);
    procedure RotateAround(const Pivot, Axis: TVector; Angle: single);
    Procedure RotateAroundX(Angle: single; AbsoluteRotation: boolean=true);
    Procedure RotateAroundY(Angle: single; AbsoluteRotation: boolean=true);
    Procedure RotateAroundZ(Angle: single; AbsoluteRotation: boolean=true);
    //Накопленные углы при абсолютном повороте
    property XRotationAngle: single read FXRotationAngle;
    property YRotationAngle: single read FYRotationAngle;
    property ZRotationAngle: single read FZRotationAngle;

    //формирует матрицу масштабирования, при AbsoluteScale=false модифицируется существующая
    Procedure ScaleObject(Scale: TVector; AbsoluteScale: boolean=true);overload;
    Procedure ScaleObject(ScaleX,ScaleY,ScaleZ: single; AbsoluteScale: boolean=true);overload;
    //формирует матрицу переноса, при AbsolutePos=false модифицируется существующая
    Procedure MoveObject(Pos: TVector; AbsolutePos: boolean=true);overload;
    Procedure MoveObject(x,y,z: single; AbsolutePos: boolean=true);overload;
    //перестраивается мировая матрица
    Procedure UpdateWorldMatrix(UseMatrix: TTransforms=[ttAll]);virtual;
    //Заменяет все матрицы трансформаций на единичные
    Procedure ResetMatrices;
    //Заменяет модельную матрицу текущей мировой матрицей
    Procedure StoreTransforms(ToStore: TTransforms);
    //Переводит точку из глобальной системы координат в систему координат объекта
    Function AbsoluteToLocal(P: TVector):TVector;
    //Переводит вектор из глобальной системы координат в локальную
    Function VectorToLocal(V: TAffineVector; Norm: boolean=true):TAffineVector;
    //Переводит точку из локальной системы координат в глобальную
    Function LocalToAbsolute(P: TVector): TVector;
  end;

  TJoint = class (TVBOMeshItem)
  private
    FNode: TMovableObject;
    FParentNode: TMovableObject;
    FName: string;
    FIndex: integer;
    procedure SetNode(const Value: TMovableObject);
    procedure setParent(const Value: TMovableObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Notification(Sender: TVBOMeshItem; aMessage: TNotification); override;
    procedure Process; override;

    property Name: string read FName write FName;
    property Index: integer read FIndex;
    property Node: TMovableObject read FNode write SetNode;
    property ParentNode: TMovableObject read FParentNode write setParent;
  end;

  TLinkedObjects = class (TVBOMeshItem)
  private
    FJoints: TList;
    function getJoint(index: integer): TJoint;
    procedure setJoint(index: integer; const Value: TJoint);
  public
    constructor Create;
    destructor Destroy; override;

    function NewJoint(aNode,aParent: TMovableObject; aName: string=''): TJoint;
    function JointByName(aName: string): TJoint;
    function JointIndex(aName: string): integer;

    property Joints[index: integer]: TJoint read getJoint write setJoint; default;
  end;

implementation

{ TVBOMeshItem }

constructor TVBOMeshItem.Create;
begin
  inherited;
  FItemType:=mcUnknown;
  FParent:=nil; FOwner:=nil;
  FName:=''; FChilde:=nil;
  FProcessChilds:=pcAfter;
  FSubscribers:=TList.Create;
end;

destructor TVBOMeshItem.Destroy;
begin
  DispatchNotification(ntRemoved);
  if assigned(FParent) then FParent.Notification(self,ntRemoved);
  if assigned(FChilde) and (FChilde.FOwner=self) then FreeAndNil(FChilde);
  FSubscribers.Free;
  inherited;
end;


procedure TVBOMeshItem.DispatchNotification(aMessage: TNotification);
var mi: TVBOMeshItem;
    i: integer;
begin
  for i:=0 to FSubscribers.Count-1 do begin
    mi:=FSubscribers[i]; if assigned(mi) then mi.Notification(self,aMessage);
  end;
end;

procedure TVBOMeshItem.Notification(Sender: TVBOMeshItem; aMessage: TNotification);
var i: integer;
begin
  case aMessage of
    ntUnsibscribe, ntRemoved: begin
      i:=FSubscribers.IndexOf(Sender);
      if i>=0 then FSubscribers.Delete(i);
      if Sender=FChilde then FChilde:=nil;
      if Sender=FParent then FParent:=nil;
    end;
  end;
end;

procedure TVBOMeshItem.setChilde(const Value: TVBOMeshItem);
begin
  FChilde := Value;
  if assigned(Value) then FChilde.Subscribe(self);
end;

procedure TVBOMeshItem.setParent(const Value: TVBOMeshItem);
begin
  if assigned(FParent) and (FParent<>Value)
  then FParent.Notification(self,ntUnsibscribe);
  FParent := Value;
  if assigned(FParent) then FParent.Subscribe(self);
end;

procedure TVBOMeshItem.Subscribe(aItem: TVBOMeshItem);
begin
  if assigned(aItem) and (FSubscribers.IndexOf(aItem)<0)
  then FSubscribers.Add(aItem);
end;

{ TRenderEventItem }

procedure TRenderEventItem.Process;
begin
  if assigned(FRenderEvent) then FRenderEvent(self);
end;

{ TMovableObject }

function TMovableObject.AbsoluteToLocal(P: TVector): TVector;
begin
    if not WorldMatrixUpdated then UpdateWorldMatrix;p[3]:=1;
    Result:=VectorTransform(P,Matrices.InvWorldMatrix);
end;

function TMovableObject.VectorToLocal(V: TAffineVector; Norm: boolean=true): TAffineVector;
begin
    if not WorldMatrixUpdated then UpdateWorldMatrix;
    Result:=affinevectormake(VectorTransform(vectormake(V,0),Matrices.InvWorldMatrix));
    if Norm then NormalizeVector(Result);
end;

constructor TMovableObject.Create;
begin
  inherited Create;
  FItemType:=mcUnknown;
  with Matrices do begin
    ModelMatrix:=IdentityHmgMatrix;
    ScaleMatrix:=IdentityHmgMatrix;
    RotationMatrix:=IdentityHmgMatrix;
    TranslationMatrix:=IdentityHmgMatrix;
    WorldMatrix:=IdentityHmgMatrix;
    WorldMatrixT:=IdentityHmgMatrix;
    InvWorldMatrix:=IdentityHmgMatrix;
  end;
  FParentMatrix:=IdentityHmgMatrix;

  FRollAngle:=0;
  FTurnAngle:=0;
  FPitchAngle:=0;
  FXRotationAngle:=0;
  FYRotationAngle:=0;
  FZRotationAngle:=0;

  FPosition:=vectormake(0,0,0,0);
  FScale:=vectormake(1,1,1,1);
  Parent:=nil;
  UpdateWorldMatrix;
end;

destructor TMovableObject.Destroy;
begin
  inherited;
end;

function TMovableObject.getParent: TMovableObject;
begin
  result:=inherited Parent as TMovableObject;
end;

procedure TMovableObject.MoveObject(Pos:TVector; AbsolutePos:boolean=true);
var mt:TMatrix;
begin
  mt:=CreateTranslationMatrix(Pos);
  with Matrices do begin
   if AbsolutePos then begin
     TranslationMatrix:=mt;
     //FPosition:=Pos;
   end else begin
     //AddVector(FPosition,VectorTransform(Pos,TranslationMatrix));
     TranslationMatrix:=MatrixMultiply(TranslationMatrix,mt);
   end;
  end;
  UpdateWorldMatrix;
end;

procedure TMovableObject.ResetMatrices;
begin
  with Matrices do begin
    ModelMatrix:=IdentityHmgMatrix;
    ScaleMatrix:=IdentityHmgMatrix;
    RotationMatrix:=IdentityHmgMatrix;
    TranslationMatrix:=IdentityHmgMatrix;
    WorldMatrix:=IdentityHmgMatrix;
  end;
end;

procedure TMovableObject.RotateObject(const Axis: TVector; Angle: single;
  AbsoluteRotation: boolean);
var mr:TMatrix;
begin
 with Matrices do begin
  mr:=CreateRotationMatrix(Axis,Angle);
  if AbsoluteRotation then RotationMatrix:=mr
  else RotationMatrix:=MatrixMultiply(RotationMatrix,mr);
 end;
 UpdateWorldMatrix;
end;

procedure TMovableObject.ScaleObject(Scale:TVector;AbsoluteScale:boolean=true);
var ms:TMatrix;
begin
 with Matrices do begin
  ms:=CreateScaleMatrix(Scale);
  if AbsoluteScale then begin
     ScaleMatrix:=ms;
     FScale:=Scale;
  end else begin
     FScale:=VectorTransform(Scale,ScaleMatrix);
     ScaleMatrix:=MatrixMultiply(ScaleMatrix,ms);
  end;
 end;
 UpdateWorldMatrix;
end;


procedure TMovableObject.UpdateWorldMatrix;
var wm: TMatrix;
begin
 with Matrices do begin

  if (FParent<>nil) and ((ttParent in UseMatrix) or (ttAll in UseMatrix)) then begin
   if not Parent.WorldMatrixUpdated then parent.UpdateWorldMatrix;
   FParentMatrix:=parent.Matrices.WorldMatrix;
  end;

  wm:=IdentityHmgMatrix;
  if (FParent<>nil) and ((ttParent in UseMatrix) or (ttAll in UseMatrix)) then begin
     if not Parent.WorldMatrixUpdated then parent.UpdateWorldMatrix;
     wm:=parent.Matrices.WorldMatrix;
     wm:=MatrixMultiply(wm, ModelMatrix);
  end else wm := ModelMatrix;

  if (not (ttModel in UseMatrix)) and (not(ttAll in UseMatrix))
  then wm:=IdentityHmgMatrix;

  if (ttScale in UseMatrix) or (ttAll in UseMatrix) then wm := MatrixMultiply(wm, ScaleMatrix);
  if (ttRotation in UseMatrix) or (ttAll in UseMatrix) then wm := MatrixMultiply(wm, RotationMatrix);
  if (ttPosition in UseMatrix) or (ttAll in UseMatrix) then wm := MatrixMultiply(wm, TranslationMatrix);

  wm:=MatrixMultiply(wm, FParentMatrix);

  WorldMatrix:=wm;

  FLeft:=WorldMatrix[0];NormalizeVector(FLeft);
  FUp:=WorldMatrix[1];  NormalizeVector(FUp);
  FDirection:=WorldMatrix[2]; NormalizeVector(FDirection);
  FAbsolutePosition:=WorldMatrix[3];
  FPosition:=TranslationMatrix[3];
  TransposeMatrix(wm);WorldMatrixT:=wm;
  InvWorldMatrix:=matrixInvert(WorldMatrix);
  DirectingAxis:=vectormake(WorldMatrix[0,0],WorldMatrix[1,1],WorldMatrix[2,2]);
  NormalizeVector(DirectingAxis);
  WorldMatrixUpdated:=true;
  DispatchNotification(ntTransformationsChanged);
 end;
end;

procedure TMovableObject.PitchObject(Angle: single);
begin
  //вокруг оси X в YZ
  if not WorldMatrixUpdated then UpdateWorldMatrix;
  with Matrices do RotationMatrix:=Pitch(RotationMatrix,Angle);
  UpdateWorldMatrix;
  FPitchAngle:=FPitchAngle+Angle;
end;

procedure TMovableObject.RollObject(Angle: single);
begin
  //вокруг оси Z в XY
  if not WorldMatrixUpdated then UpdateWorldMatrix;
  with Matrices do RotationMatrix:=Roll(RotationMatrix,Angle);
  UpdateWorldMatrix;
  FRollAngle:=FRollAngle+Angle;
end;

procedure TMovableObject.TurnObject(Angle: single);
begin
  //вокруг оси Y в XZ
  if not WorldMatrixUpdated then UpdateWorldMatrix;
  with Matrices do RotationMatrix:=Turn(RotationMatrix,Angle);
  UpdateWorldMatrix;
  FTurnAngle:=FTurnAngle+Angle;
end;

procedure TMovableObject.StoreTransforms(ToStore: TTransforms);
var wm,mm:TMatrix;
    ms:TMatrixStack;
begin
  ms:=Matrices;
  with Matrices do begin
    if ttModel in toStore then wm := ModelMatrix else wm:=IdentityHmgMatrix;
    if ttScale in toStore then wm := MatrixMultiply(wm, ScaleMatrix);
    if ttRotation in toStore then wm := MatrixMultiply(wm, RotationMatrix);
    if ttposition in toStore then wm := MatrixMultiply(wm, TranslationMatrix);
    mm:=ModelMatrix; ResetMatrices;
    Matrices.ModelMatrix:=MatrixMultiply(mm, wm);
    if not (ttScale in toStore) then ScaleMatrix:=ms.ScaleMatrix;
    if not (ttRotation in toStore) then RotationMatrix:=ms.RotationMatrix;
    if not (ttPosition in toStore) then TranslationMatrix:=ms.TranslationMatrix;
    UpdateWorldMatrix;
  end;
end;

procedure TMovableObject.RotateAroundX(Angle: single;
  AbsoluteRotation: boolean);
var rm:TMatrix;
begin
 with Matrices do begin
  //вокруг глобальной оси X
  if not WorldMatrixUpdated then UpdateWorldMatrix;
  if AbsoluteRotation then begin
     RotationMatrix:=CreateRotationMatrixX(Angle);
  end else begin
     FXRotationAngle:=FXRotationAngle+Angle;
     rm:=CreateRotationMatrixX(Angle);
     RotationMatrix:=MatrixMultiply(RotationMatrix,rm);
  end;
  UpdateWorldMatrix;
 end;
end;

procedure TMovableObject.RotateAroundY(Angle: single;
  AbsoluteRotation: boolean);
var rm:TMatrix;
begin
 with Matrices do begin
  //вокруг глобальной оси Y
  if not WorldMatrixUpdated then UpdateWorldMatrix;
  if AbsoluteRotation then begin
     RotationMatrix:=CreateRotationMatrixY(Angle);
  end else begin
     FYRotationAngle:=FYRotationAngle+Angle;
     rm:=CreateRotationMatrixY(Angle);
     RotationMatrix:=MatrixMultiply(RotationMatrix,rm);
  end;
  UpdateWorldMatrix;
 end;
end;

procedure TMovableObject.RotateAroundZ(Angle: single;
  AbsoluteRotation: boolean);
var rm:TMatrix;
begin
 with Matrices do begin
  //вокруг глобальной оси Z
  if not WorldMatrixUpdated then UpdateWorldMatrix;
  rm:=CreateRotationMatrixZ(Angle);
  if AbsoluteRotation then begin
     RotationMatrix:=CreateRotationMatrixZ(Angle);
  end else begin
     FZRotationAngle:=FZRotationAngle+Angle;
     rm:=CreateRotationMatrixZ(Angle);
     RotationMatrix:=MatrixMultiply(RotationMatrix,rm);
  end;
  UpdateWorldMatrix;
 end;
end;

procedure TMovableObject.RotateAround(const Pivot, Axis: TVector; Angle: single);
var np: TVector;
    mr,mp,mnp,m: TMatrix;
begin
  mr:=CreateRotationMatrix(Axis,Angle);

  np:=VectorNegate(Pivot); np[3]:=1;
  mp:=CreateTranslationMatrix(Pivot);
  mnp:=CreateTranslationMatrix(np);

  m:=Matrices.ModelMatrix;

  //Поворот вокруг заданной точки
  m:=MatrixMultiply(m,mp);
  m:=MatrixMultiply(m,mr);
  m:=MatrixMultiply(m,mnp);
  Matrices.ModelMatrix:=m;
  UpdateWorldMatrix;
end;

procedure TMovableObject.SetParent(const Value: TMovableObject);
begin
  inherited Parent:=Value;
end;

procedure TMovableObject.SetPosition(const Value: TVector);
begin
  MoveObject(Value);
end;

procedure TMovableObject.SetScale(const Value: TVector);
begin
  ScaleObject(Value);
end;

procedure TMovableObject.MoveForward(Step: single);
begin
  with Matrices do begin
    TranslationMatrix[3,0]:=TranslationMatrix[3,0]+FDirection[0]*Step;
    TranslationMatrix[3,1]:=TranslationMatrix[3,1]+FDirection[1]*Step;
    TranslationMatrix[3,2]:=TranslationMatrix[3,2]+FDirection[2]*Step;
  end; UpdateWorldMatrix;
end;

procedure TMovableObject.MoveLeft(Step: single);
begin
  with Matrices do begin
    TranslationMatrix[3,0]:=TranslationMatrix[3,0]+FLeft[0]*Step;
    TranslationMatrix[3,1]:=TranslationMatrix[3,1]+FLeft[1]*Step;
    TranslationMatrix[3,2]:=TranslationMatrix[3,2]+FLeft[2]*Step;
  end; UpdateWorldMatrix;
end;

procedure TMovableObject.MoveUp(Step: single);
begin
  with Matrices do begin
    TranslationMatrix[3,0]:=TranslationMatrix[3,0]+FUp[0]*Step;
    TranslationMatrix[3,1]:=TranslationMatrix[3,1]+FUp[1]*Step;
    TranslationMatrix[3,2]:=TranslationMatrix[3,2]+FUp[2]*Step;
  end; UpdateWorldMatrix;
end;

procedure TMovableObject.Notification(Sender: TVBOMeshItem;
  aMessage: TNotification);
begin
  inherited;
  if aMessage=ntTransformationsChanged then WorldMatrixUpdated:=false;
end;

procedure TMovableObject.MoveObject(x, y, z: single; AbsolutePos: boolean);
begin
   MoveObject(vectormake(x,y,z,1),AbsolutePos);
end;

procedure TMovableObject.ScaleObject(ScaleX, ScaleY, ScaleZ: single;
  AbsoluteScale: boolean);
begin
  ScaleObject(vectormake(ScaleX, ScaleY, ScaleZ, 0),AbsoluteScale);
end;

procedure QuadFromCount (count: integer; var size: integer);
const pow2:array[0..12] of integer =
      (1,2,4,8,16,32,64,128,256,512,1024,2048,4096);
      sq2: array[0..12] of integer =
      (1,4,16,64,256,1024,4096,16384,65536,262144,1048576,4194304,16777216);
var i:integer;
begin
  i:=0;
  while (i<=12) and (sq2[i]<count) do inc(i);
  assert(i<=12,'To many vertexes');
  size:=pow2[i];
end;

procedure TMovableObject.SetDirection(const Direction: TVector);
var up,left,right,dir: TVector;
begin
  with Matrices do begin
    up:=ModelMatrix[1];
    NormalizeVector(up);
    dir:=VectorNormalize(direction);
    right:=VectorCrossProduct(Dir, Up);
    if VectorLength(right)<1e-5  then begin
       right:=VectorCrossProduct(ZHmgVector, Up);
       if VectorLength(right)<1e-5 then
          right:=VectorCrossProduct(XHmgVector, Up);
    end;
    NormalizeVector(right);
    Up:=VectorCrossProduct(right, Dir);
    NormalizeVector(Up);
    Left:=VectorCrossProduct(Up, Dir);
    NormalizeVector(Left);
    ModelMatrix[0]:=Left;
    ModelMatrix[1]:=Up;
    ModelMatrix[2]:=Dir;
    RotationMatrix:=IdentityHmgMatrix;
  end;
  UpdateWorldMatrix;
end;

function TMovableObject.LocalToAbsolute(P: TVector): TVector;
begin
  if not WorldMatrixUpdated then UpdateWorldMatrix;
  Result:=VectorTransform(P,Matrices.WorldMatrix);
end;

procedure TMovableObject.Process;
begin
  if not WorldMatrixUpdated then UpdateWorldMatrix;
end;

{ TLinkedObjects }

constructor TLinkedObjects.Create;
begin
  inherited;
  FJoints:=TList.Create;
end;

destructor TLinkedObjects.Destroy;
begin
  FreeObjectList(FJoints);
  inherited;
end;

function TLinkedObjects.getJoint(index: integer): TJoint;
begin
  if (index<FJoints.Count) and (index>=0)
  then result:=FJoints[index] else result:=nil;
end;

function TLinkedObjects.JointByName(aName: string): TJoint;
var i: integer;
begin
  i:=JointIndex(aName);
  if i>=0 then result:=FJoints[i] else result:=nil;

end;

function TLinkedObjects.JointIndex(aName: string): integer;
var i: integer;
    J: TJoint;
begin
  result:=-1;
  for i:=0 to FJoints.Count-1 do begin
    J:=FJoints[i];
    if assigned(J) then
      if J.Name=aName then begin result:=i; exit; end;
  end;
end;

function TLinkedObjects.NewJoint(aNode, aParent: TMovableObject;
  aName: string): TJoint;
var J: TJoint;
begin
  J:=TJoint.Create;
  J.Name:=aName;
  J.ParentNode:=aParent;
  J.Node:=aNode;
  J.Owner:=self;
  J.FIndex:=FJoints.Add(J);
  result:=J;
end;

procedure TLinkedObjects.setJoint(index: integer; const Value: TJoint);
var J: TJoint;
begin
  if index>=FJoints.Count then exit;
  J:=FJoints[index];
  if Value=J then exit;
  if assigned(J) and (J.Owner=self) then J.Free;
  FJoints[index]:=J;
end;

{ TJoint }
constructor TJoint.Create;
begin
  inherited;
  FNode:=nil;
  FParentNode:=nil;
  FName:='';
  FIndex:=0;
end;

destructor TJoint.Destroy;
begin
  if assigned(FParent) then FParent.Notification(self,ntRemoved);
  inherited;
end;

procedure TJoint.Notification(Sender: TVBOMeshItem; aMessage: TNotification);
begin
  if Sender=FParentNode then begin
    case aMessage of
      ntUnsibscribe, ntRemoved: begin
        FParentNode:=nil;
      end;
      ntTransformationsChanged: begin
        FNode.FParentMatrix:=FParentNode.Matrices.WorldMatrix;
        FNode.UpdateWorldMatrix;
      end;
    end;
  end;
  inherited;
end;

procedure TJoint.Process;
begin
  if assigned(FNode) then begin
    if assigned(FParentNode) then
       FNode.FParentMatrix:=FParentNode.Matrices.WorldMatrix
    else FNode.FParentMatrix:=IdentityHmgMatrix;
    FNode.UpdateWorldMatrix;
  end;
end;

procedure TJoint.SetNode(const Value: TMovableObject);
begin
  if assigned(FNode) and assigned(FParentNode) then FParentNode.Notification(self,ntUnsibscribe);
  FNode := Value;
  if assigned(FNode) and assigned(FParentNode) then FParentNode.Subscribe(self);
end;

procedure TJoint.setParent(const Value: TMovableObject);
begin
  if assigned(FNode) and assigned(FParentNode) then FParentNode.Notification(self,ntUnsibscribe);
  FParentNode := Value;
  if assigned(FNode) and assigned(FParentNode) then FParentNode.Subscribe(self);
end;

end.
