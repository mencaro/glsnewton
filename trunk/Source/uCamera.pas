unit uCamera;

interface

uses VectorGeometry, vboMesh;

type
  TCameraController = class(TVBOMeshObject)
     private
       FViewMatrix: TMatrix;
       FParent: TVBOMeshObject;
       FLeft, FUp, FDirection, FPosition: TVector;
       function CreateViewMatrix: TMatrix;

     public
       constructor Create(aParent: TVBOMeshObject=nil);
       destructor Destroy; override;

       procedure UpdateWorldMatrix;
       procedure UpdateViewMatrix;
//       procedure LookAt(aDirection: TVector);
//       procedure PointTo(aPosition: TVector);overload;
//       procedure PointTo(Obj: TVBOMeshObject);overload;
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
       property Parent: TVBOMeshObject read FParent write FParent;
  end;

implementation

{ TCameraController }

constructor TCameraController.Create(aParent: TVBOMeshObject);
begin
   inherited Create;
   FParent:=aParent;
end;

destructor TCameraController.Destroy;
begin
  inherited;
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
 with Matrices do begin
  if (FParent<>nil) then begin
     if not FParent.WorldMatrixUpdated then Fparent.UpdateWorldMatrix;
     wm:=Fparent.Matrices.WorldMatrix;
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

function TCameraController.LiftUp(step: single): TVector;
begin
  if not WorldMatrixUpdated then updateWorldMatrix;
  with Matrices do begin
    TranslationMatrix[3,1]:=TranslationMatrix[3,1]+FUp[1]*Step;
  end; UpdateWorldMatrix;
  result:=Matrices.WorldMatrix[3];
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

end.