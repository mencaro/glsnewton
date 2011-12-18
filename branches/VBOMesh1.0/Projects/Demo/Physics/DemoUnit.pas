unit DemoUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLCoordinates, GLScene, GLCadencer, GLWin32Viewer, GLCrossPlatform,
  BaseClasses, GLObjects, ExtCtrls, GLSkydome, GLMaterial, TGA;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    GLEarthSkyDome1: TGLEarthSkyDome;
    matlib: TGLMaterialLibrary;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses VboMesh,uVBO,NewtonFunctions,VectorTypes,VectorGeometry,GLKeyBoard,NewtonImport;

var SceneDynamic:TVBOMesh;
    SceneStatic:TVBOMesh;
    GUI:TVBOMesh;
    GamePhysics:TSceneNewtonWorld;
    Exceptions:TObjectExceptionsVBO;
    Px,Py:128..1024;

{$R *.dfm}

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   GLCadencer1.Enabled:=false;
   GamePhysics.Destroy;
   SceneStatic.Free;
   SceneDynamic.Free;
end;

procedure AddSphere;
var rx,ry,rz:-5..5;
begin
  Randomize;
  SceneDynamic.AddSphere(0.5,20,20);
  rx:=random(10)-5;
  ry:=random(5);
  rz:=random(10)-5;
  SceneDynamic.ObjectsList[SceneDynamic.Count-1].Position:=VectorMake(rx,ry,rz);
  Exceptions:=nil;
  SetLength(Exceptions,SceneDynamic.Count);
  Exceptions[SceneDynamic.Count-1]:=2;
  GamePhysics.CreateConvexBodyVBO(SceneDynamic,5,Exceptions);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  v1,v2:TVector3f;
begin
//  BlockInput(true);
  Width:=Screen.Width;
  Height:=Screen.Height;
  Left:=0;
  Top:=0;
  ShowCursor(false);
  GLSceneViewer1.Width:=Width;
  GLSceneViewer1.Height:=Height;
  GLCamera1.FocalLength:=GLSceneViewer1.Width/14.2;
  Px:=GLSceneViewer1.Width div 2;
  Py:=GLSceneViewer1.Height div 2;
  GLSceneViewer1.Enabled:=true;
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  SceneDynamic:=TVBOMesh.CreateAsChild(GLScene1.Objects);
  SceneStatic:=TVBOMesh.CreateAsChild(GLScene1.Objects);
  GUI:=TVBOMesh.CreateAsChild(GLScene1.Objects);
  SceneStatic.AddPlane(10,10,4,4);
  SceneDynamic.AddSphere(0.5,20,20);
  SceneDynamic.AddBox(1,1,1,1,1,1);
  GUI.AddHUDSprite(0.1,0.1).Material:=matlib.Materials[0].Material;
  SetLength(Exceptions,1);
  v1:=AffineVectorMake(-100,-100,-100);
  v2:=AffineVectorMake(100,100,100);
  GamePhysics:=TSceneNewtonWorld.Create(2,2,v1,v2);
  GamePhysics.CreateTreeBodyVBO(SceneStatic);
  GamePhysics.CreateConvexBodyVBO(SceneDynamic,5);
  GamePhysics.CreatePlayerSphere(1,1,1,GLDummyCube1.Matrix);
//  BlockInput(false);
  ShowMessage('жми Enter для добавления сфер');
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  if isKeyDown(VK_SPACE) then
    GamePhysics.FPSApplyKeyMove(GLDummyCube1,87,83,68,65,true)
  else
    GamePhysics.FPSApplyKeyMove(GLDummyCube1,87,83,68,65,false);
  GamePhysics.FPSApplyCamMove(Px,PY,GLCamera1,GLDummyCube1,0.2,70);
  GamePhysics.UpdateWorld(1);
  if isKeyDown(VK_ESCAPE) then Close;
  if isKeyDown(13) then AddSphere;
end;

end.
