unit uCamera;

interface

uses VectorGeometry, uBaseClasses;

type
  TCameraController = class(TMovableObject)
     private
       FViewMatrix: TMatrix;
       FProjectionMatrix: TMatrix;
       FLeft, FUp, FDirection, FPosition: TVector;
       FUseLookAtMatrix: boolean;
       function CreateViewMatrix: TMatrix;
       function GetMatrixAdr: pointer;
       procedure SetPosition(const Value: TVector);
     public
       constructor Create;
       destructor Destroy; override;

       function SetProjectionMatrix(left, right, bottom, top,
         zNear, zFar: double): TMatrix; overload;
       function SetProjectionMatrix(width, height, zNear, zFar: double): TMatrix; overload;
       function SetPerspective(fovy, aspect, zNear, zFar: double): TMatrix;
       function SetOrtogonal(width, height, zNear, zFar: double): TMatrix;

       procedure UpdateWorldMatrix;
       procedure UpdateViewMatrix;
       procedure LookAt(aDirection: TVector);
       procedure PointTo(aPosition: TVector);overload;
       procedure PointTo(Obj: TMovableObject);overload;
       //Перемещение камеры в плоскости
       function MoveForward2D(step: single): TVector;
       function MoveLeft2D(step: single): TVector;
       //Подъем камеры
       function LiftUp(step: single): TVector;
       //Задает вертикальный наклон камеры
       procedure LookUp(Angle: single);
       //Задает горизонтальный поворот камеры
       procedure TurnLeft(Angle: single);

       property ViewMatrix: TMatrix read CreateViewMatrix write FViewMatrix;
       property ViewMatrixAsAddress: pointer read GetMatrixAdr;
       property ProjectionMatrix: TMatrix read FProjectionMatrix write FProjectionMatrix;


       property Left: TVector read Matrices.WorldMatrix[0];
       property Up: TVector read Matrices.WorldMatrix[1];
       //property Direction: TVector read Matrices.WorldMatrix[2];
       property Position: TVector read Matrices.WorldMatrix[3] write SetPosition;

  end;

implementation

{ TCameraController }

constructor TCameraController.Create;
begin
   inherited Create;
   FUseLookAtMatrix:=false;
//   FParent:=aParent;
end;

destructor TCameraController.Destroy;
begin
  inherited Destroy;
end;

function TCameraController.CreateViewMatrix: TMatrix;
var d: TVector;
    mat3: TAffineMatrix;
begin
  if not WorldMatrixUpdated then UpdateWorldMatrix;
  d:=Matrices.WorldMatrix[3];
  Setmatrix(mat3,Matrices.WorldMatrix);
  TransposeMatrix(mat3);
  SetMatrix(Result,mat3);
  d:=VectorTransform(d,mat3);
  NegateVector(d); d[3]:=1;
  result[3]:=d;
end;

procedure TCameraController.UpdateWorldMatrix;
var wm:TMatrix;
begin
 FUseLookAtMatrix:=false;
 with Matrices do begin
  if (FParent<>nil) then begin
     if not Parent.WorldMatrixUpdated then parent.UpdateWorldMatrix;
     wm:=parent.Matrices.WorldMatrix;
     wm:=MatrixMultiply(wm, ModelMatrix);
  end else wm := ModelMatrix;
  wm := MatrixMultiply(wm, ScaleMatrix);
  wm := MatrixMultiply(wm, RotationMatrix);
  wm := MatrixMultiply(wm, TranslationMatrix);
  WorldMatrix:=wm;

  FLeft:=WorldMatrix[0];NormalizeVector(FLeft);
  FUp:=WorldMatrix[1];  NormalizeVector(FUp);
  FDirection:=WorldMatrix[2]; NormalizeVector(FDirection);
  FPosition:=WorldMatrix[3];
  TransposeMatrix(wm);WorldMatrixT:=wm;
  InvWorldMatrix:=matrixInvert(WorldMatrix);
  DirectingAxis:=vectormake(WorldMatrix[0,0],WorldMatrix[1,1],WorldMatrix[2,2]);
  NormalizeVector(DirectingAxis);
  FViewMatrix:=CreateViewMatrix;
  WorldMatrixUpdated:=true;
 end;
end;

procedure TCameraController.UpdateViewMatrix;
begin
  updateWorldMatrix;
end;

function TCameraController.MoveForward2D(step: single): TVector;
begin
  if not WorldMatrixUpdated then updateWorldMatrix;
  with Matrices do begin
    TranslationMatrix[3,0]:=TranslationMatrix[3,0]+FDirection[0]*Step;
    TranslationMatrix[3,2]:=TranslationMatrix[3,2]+FDirection[2]*Step;
  end; UpdateWorldMatrix;
  result:=Matrices.WorldMatrix[3];
end;

function TCameraController.MoveLeft2D(step: single): TVector;
begin
  if not WorldMatrixUpdated then updateWorldMatrix;
  with Matrices do begin
    TranslationMatrix[3,0]:=TranslationMatrix[3,0]+FLeft[0]*Step;
    TranslationMatrix[3,2]:=TranslationMatrix[3,2]+FLeft[2]*Step;
  end; UpdateWorldMatrix;
  result:=Matrices.WorldMatrix[3];
end;

procedure TCameraController.PointTo(Obj: TMovableObject);
begin
  PointTo(obj.Position);
end;

procedure TCameraController.PointTo(aPosition: TVector);
var F,absUp,s,u: TVector;
    rM,trM: TMatrix;
begin
  F:=VectorSubtract(aPosition,Position);
  NormalizeVector(F);
  absUp:=vectormake(0,1,0,0);
  s:=VectorCrossProduct(F,absUp);
  if VectorLength(s)<1e-5  then begin
     s:=VectorCrossProduct(ZHmgVector, Up);
     if VectorLength(s)<1e-5 then
        s:=VectorCrossProduct(XHmgVector, Up);
  end;

  u:=VectorCrossProduct(s,F);
  rm:=IdentityHmgMatrix;
  rm[0]:=VectorMake(s[0],u[0],-f[0]);
  rm[1]:=VectorMake(s[1],u[1],-f[1]);
  rm[2]:=VectorMake(s[2],u[2],-f[2]);
  rm[3]:=Vectormake(0,0,0,1);
//  trM:=CreateTranslationMatrix(VectorMake(-Position[0],-Position[1],-Position[2],1));
//  FViewMatrix:=MatrixMultiply(trm,rm);
//  FUseLookAtMatrix:=true;
  TransposeMatrix(rm);
  Matrices.RotationMatrix:=rm;
  Matrices.TranslationMatrix:=CreateTranslationMatrix(VectorMake(Position[0],Position[1],Position[2],1));
  UpdateViewMatrix;
end;

function TCameraController.SetOrtogonal(width, height, zNear,
  zFar: double): TMatrix;
var r,t: double;
begin
  r:=width/2; t:=height/2;
  FProjectionMatrix[0]:=VectorMake(1/r,0,0,0);
  FProjectionMatrix[1]:=VectorMake(0,1/t,0,0);
  FProjectionMatrix[2]:=VectorMake(0, 0, -2/(zFar-zNear), 0);
  FProjectionMatrix[3]:=VectorMake(0,0,-(zFar+zNear)/(zFar-zNear),0);
  result:=FProjectionMatrix;
end;

function TCameraController.SetPerspective(fovy, aspect, zNear,
  zFar: double): TMatrix;
var top, bottom, left, right: double;
begin
  top := zNear * tan(pi/180*fovy/2);
  bottom := -top;
  right := aspect*top;
  left := -right;
  result:=SetProjectionMatrix(left,right,bottom,top,zNear,zFar);
end;

procedure TCameraController.SetPosition(const Value: TVector);
begin
  MoveObject(Value); UpdateWorldMatrix;
end;

function TCameraController.SetProjectionMatrix(width, height, zNear,
  zFar: double): TMatrix;
var r,t: double;
begin
  r:=width/2; t:=height/2;
  FProjectionMatrix[0]:=VectorMake(zNear/r,0,0,0);
  FProjectionMatrix[1]:=VectorMake(0,zNear/t,0,0);
  FProjectionMatrix[2]:=VectorMake(0, 0, -(zFar+zNear)/(zFar-zNear), -1);
  FProjectionMatrix[3]:=VectorMake(0,0,-2*zFar*zNear/(zFar-zNear),0);
  result:=FProjectionMatrix;
end;

function TCameraController.SetProjectionMatrix(left, right, bottom, top, zNear,
  zFar: double): TMatrix;
begin
  FProjectionMatrix[0]:=VectorMake((2*zNear)/(right-left),0,0,0);
  FProjectionMatrix[1]:=VectorMake(0,(2*zNear)/(top-bottom),0,0);
  FProjectionMatrix[2]:=VectorMake((right+left)/(right-left),
    (top+bottom)/(top-bottom), -(zFar+zNear)/(zFar-zNear), -1);
  FProjectionMatrix[3]:=VectorMake(0,0,-2*zFar*zNear/(zFar-zNear),0);
  result:=FProjectionMatrix;
end;

function TCameraController.LiftUp(step: single): TVector;
begin
  if not WorldMatrixUpdated then updateWorldMatrix;
  with Matrices do begin
    TranslationMatrix[3,1]:=TranslationMatrix[3,1]+FUp[1]*Step;
  end; UpdateWorldMatrix;
  result:=Matrices.WorldMatrix[3];
end;

procedure TCameraController.LookAt(aDirection: TVector);
var F,absUp,s,u: TVector;
    rM,trM: TMatrix;
begin
  F:=aDirection; NormalizeVector(F);
  absUp:=vectormake(0,1,0,0);
  s:=VectorCrossProduct(F,absUp);
  if VectorLength(s)<1e-5  then begin
     s:=VectorCrossProduct(ZHmgVector, Up);
     if VectorLength(s)<1e-5 then
        s:=VectorCrossProduct(XHmgVector, Up);
  end;

  u:=VectorCrossProduct(s,F);
  rm:=IdentityHmgMatrix;
  rm[0]:=VectorMake(s[0],u[0],-f[0]);
  rm[1]:=VectorMake(s[1],u[1],-f[1]);
  rm[2]:=VectorMake(s[2],u[2],-f[2]);
  rm[3]:=Vectormake(0,0,0,1);
//  trM:=CreateTranslationMatrix(VectorMake(-Position[0],-Position[1],-Position[2],1));
//  FViewMatrix:=MatrixMultiply(trm,rm);
//  FUseLookAtMatrix:=true;
  TransposeMatrix(rm);
  Matrices.RotationMatrix:=rm;
  Matrices.TranslationMatrix:=CreateTranslationMatrix(VectorMake(Position[0],Position[1],Position[2],1));
  UpdateViewMatrix;
end;

procedure TCameraController.LookUp(Angle: single);
var rm: TMatrix;
begin
  rm:=CreateRotationMatrix(FLeft,Angle);
  Matrices.RotationMatrix:=MatrixMultiply(Matrices.RotationMatrix,rm);
  UpdateWorldMatrix;
end;


procedure TCameraController.TurnLeft(Angle: single);
var rm:TMatrix;
begin
 with Matrices do begin
  if not WorldMatrixUpdated then UpdateWorldMatrix;
  rm:=CreateRotationMatrixY(Angle);
  RotationMatrix:=MatrixMultiply(RotationMatrix,rm);
  UpdateWorldMatrix;
 end;
end;

function TCameraController.GetMatrixAdr: pointer;
begin
  //if not WorldMatrixUpdated then
  if not FUseLookAtMatrix then UpdateViewMatrix;
  result:=@FViewMatrix;
end;

end.