unit DemoUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLCoordinates, GLScene, GLCadencer, GLWin32Viewer, GLCrossPlatform,
  BaseClasses, GLObjects, ExtCtrls, GLSkydome, GLMaterial, 
  VectorGeometry,

  uBasePhysics, uNewtonPhysics, uPhysicObjectList, uPhysicRender;

type
  TfrmMain = class(TForm)
    glMainScene: TGLScene;
    ViewPort: TGLSceneViewer;
    glCadencer: TGLCadencer;
    glMainCamera: TGLCamera;
    glWorldDummy: TGLDummyCube;
    glMainLight: TGLLightSource;
    MatLib: TGLMaterialLibrary;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure glCadencerProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormActivate(Sender: TObject);
    procedure ViewPortMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ViewPortMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    
  private
    GrObj: TSceneGraphObject;
    FirstBox: TBasePhysicObject;

    fNewtonWorld: TNewtonWorld;
    fPhysicList: TPhysicGameList;
    fGraphWorld: TGlSceneGraphicWorld;
    Started: Boolean;
  public
    Function AddSimpleBox(aSizes: TVector; aMass: Single; aPosition: TVector): TBasePhysicObject;
    Procedure InitFirstDemo;
    Procedure InitSecondDemo;
    Procedure PhysicInit;
  end;

var
  frmMain: TfrmMain;

implementation
Uses
  GlKeyboard;
  
{$R *.dfm}

Function TfrmMain.AddSimpleBox(aSizes: TVector; aMass: Single; aPosition: TVector): TBasePhysicObject;
begin
  result := fPhysicList.AddSimplePhysicBox(aSizes);
  result.Mass := aMass;
  result.Position := aPosition;
end;

Procedure TfrmMain.InitFirstDemo;
begin
  AddSimpleBox(VectorMake(1, 1, 1), 0, VectorMake(0, 0, 0));
  AddSimpleBox(VectorMake(2, 0.1, 1), 1, VectorMake(0.8, 2, 0));
  AddSimpleBox(VectorMake(0.1, 1, 1), 1, VectorMake(0.0, 4, 0));
end;

Procedure TfrmMain.InitSecondDemo;
const
  aHeight = 1.1;
  aCount = 60;
  aRadius = 3;
var
  aCenter, Pos: TVector;
  i: Integer;
  Angle: Single;
  aMatrix: TMatrix;
//  Box: TBasePhysicObject;
begin
  aCenter := VectorMake(0, -aHeight / 2,  -1);

  // самое первое основание
  AddSimpleBox(VectorMake(7, 0.1, 7), 0, VectorMake(aCenter[0], aCenter[1] * 2, aCenter[2]));

  // лестница наверх
{  Pos := VectorMake(-2.5, -0.9, aCenter[2] - 1.7);
  Box := AddSimpleBox(VectorMake(2, 0.1, 4), 0, Pos);
  aMatrix := CreateRotationMatrixX(0.1);
  aMatrix := MatrixMultiply(aMatrix, CreateTranslationMatrix(Pos));
  Box.Rotation := aMatrix;}

  for i := 0 to aCount - 1 do
  begin
    Angle := i / aCount * 4 * pi - pi/2;
    Pos := VectorAdd(aCenter, VectorScale(VectorMake(cos(Angle), 0, sin(Angle)), aRadius * (1 - 0.5 * (i / aCount))));
    aMatrix := CreateRotationMatrixY(Pi - Angle);
    aMatrix := MatrixMultiply(aMatrix, CreateTranslationMatrix(Pos));
    if i = 0 then
    begin
      FirstBox := AddSimpleBox(VectorMake(0.5, aHeight, 0.2), 1, Pos);
      FirstBox.Rotation := aMatrix;
    end
    else
      AddSimpleBox(VectorMake(0.5, aHeight, 0.2), 1, Pos).Rotation := aMatrix;
  end;
end;

Procedure TfrmMain.PhysicInit;
begin
  GrObj := TSceneGraphObject.Create(nil);
  GrObj.GlObject := TGlDummyCube.CreateAsChild(glWorldDummy);

  fNewtonWorld := TNewtonWorld.Create(0.01, VectorMake(0, -10, 0));
  fGraphWorld := TGlSceneGraphicWorld.Create(GrObj);
  fPhysicList := TPhysicGameList.Create(fNewtonWorld, fGraphWorld);

//  InitFirstDemo;
  InitSecondDemo;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  glCadencer.Enabled := false;
  FreeAndNil(GrObj);
  FreeAndNil(fGraphWorld);
  FreeAndNil(fPhysicList);
  FreeAndNil(fNewtonWorld);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  PhysicInit;
  Started := false;
end;

procedure TfrmMain.glCadencerProgress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  fPhysicList.DoProgress(deltaTime);

  if IsKeyDown(' ') and not Started then
  begin
    Started := True;
    FirstBox.ApplyImpulseAtPos(VectorMake(3, 0, 0), VectorMake(0, 0.6, -3));
  end;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  glCadencer.Enabled := true;
end;

var
  Mx, My: Integer;

procedure TfrmMain.ViewPortMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Mx := x;
  My := y;
end;

procedure TfrmMain.ViewPortMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if ssLeft in Shift then
  begin
    glMainCamera.MoveAroundTarget(My - y, Mx - x);
    Mx := x;
    My := y;
  end;
end;

end.
