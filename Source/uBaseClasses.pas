unit uBaseClasses;

interface

uses Classes,VectorGeometry, uMiscUtils;

Type
  TTransformsTypes = (ttPosition, ttScale, ttRotation, ttModel, ttParent, ttFollow, ttAll);
  TTransforms = set of TTransformsTypes;
  TMeshCollectionItem = (mcMeshObject, mcContainer, mcCollection, mcEffect, mcCommands, mcRender, mcUnknown);
  TProcessChilds = (pcNone, pcBefore, pcAfter);
  TSortDirection = (sdFrontToBack, sdBackToFront, sdNone);

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
  protected
    FUseParentViewer: boolean;
    FParentViewer: PViewerSettings;
    FItemType: TMeshCollectionItem;
    FParent: TVBOMeshItem;
    FOwner: TVBOMeshItem;
    FName: string;
    FChilde: TVBOMeshItem;
    FProcessChilds: TProcessChilds;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Process; virtual;abstract;
    property MeshItemType: TMeshCollectionItem read FItemType;
    property Name: string read FName write FName;
    property ProcessChilds: TProcessChilds read FProcessChilds write FProcessChilds;
    property UseParentViewer: boolean read FUseParentViewer write FUseParentViewer;
    property ParentViewer: PViewerSettings read FParentViewer write FParentViewer;
    property Childe: TVBOMeshItem read FChilde write FChilde;
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
    FParent: TMovableObject;
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
    procedure SetParent(const Value: TMovableObject);
    procedure SetPosition(const Value: TVector);
    procedure SetScale(const Value: TVector);
    //Ориентирует объект в заданном направлении
    procedure SetDirection(const Direction: TVector);
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
    Property Parent: TMovableObject read FParent write SetParent;
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
    Procedure RotateObject(Axis: TVector; Angle: single; AbsoluteRotation: boolean=true);
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
implementation


{ TVBOMeshItem }

constructor TVBOMeshItem.Create;
begin
  inherited;
  FItemType:=mcUnknown;
  FParent:=nil; FOwner:=nil;
  FName:=''; FChilde:=nil;
  FProcessChilds:=pcAfter;
end;

destructor TVBOMeshItem.Destroy;
begin
  if assigned(FChilde) and (FChilde.FOwner=self) then FreeAndNil(FChilde);
  inherited;
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

procedure TMovableObject.RotateObject(Axis: TVector; Angle: single;
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
  wm:=IdentityHmgMatrix;
  if (FParent<>nil) and ((ttParent in UseMatrix) or (ttAll in UseMatrix)) then begin
     if not FParent.WorldMatrixUpdated then Fparent.UpdateWorldMatrix;
     wm:=Fparent.Matrices.WorldMatrix;
     wm:=MatrixMultiply(wm, ModelMatrix);
  end else wm := ModelMatrix;

  if (not (ttModel in UseMatrix)) and (not(ttAll in UseMatrix))
  then wm:=IdentityHmgMatrix;


  if (ttScale in UseMatrix) or (ttAll in UseMatrix) then wm := MatrixMultiply(wm, ScaleMatrix);
  if (ttRotation in UseMatrix) or (ttAll in UseMatrix) then wm := MatrixMultiply(wm, RotationMatrix);
  if (ttPosition in UseMatrix) or (ttAll in UseMatrix) then wm := MatrixMultiply(wm, TranslationMatrix);

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

procedure TMovableObject.SetParent(const Value: TMovableObject);
begin
  FParent := Value; UpdateWorldMatrix;
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

end.
