unit DemoUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLCoordinates, GLScene, GLCadencer, GLWin32Viewer, GLCrossPlatform,
  BaseClasses, GLObjects, ExtCtrls, GLSkydome;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLLightSource1: TGLLightSource;
    GLEarthSkyDome1: TGLEarthSkyDome;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses VBOMesh, VBONewton, VectorTypes, VectorGeometry;

var SceneDynamic:TVboMesh;
    SceneStatic:TVboMesh;
    GamePhysics:TVBONewtonWorld;
    Px,Py:256..1024;

{$R *.dfm}

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   GLSceneViewer1.Free;
   SceneStatic.Free;
   SceneDynamic.Free;
   GamePhysics.Destroy;
end;

procedure AddSphere;
var rx,ry,rz:-5..5;
    i:integer;
begin
  Randomize;
  SceneDynamic.AddSphere(0.5,20,20);
  rx:=random(10)-5;
  ry:=random(5);
  rz:=random(10)-5;
  SceneDynamic.ObjectsList[SceneDynamic.Count-1].Position:=VectorMake(rx,ry,rz);
  GamePhysics.AddConvexBody(SceneDynamic.ObjectsList[SceneDynamic.Count-1],105);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  v1,v2:TVector3f;
begin
  Caption:='жми Enter для добавления сфер';
  Width:=Screen.Width;
  Height:=Screen.Height;
  Left:=0;
  Top:=0;
  ShowCursor(false);
  GLSceneViewer1.Width:=Width;
  GLSceneViewer1.Height:=Height;
  //GLCamera1.FocalLength:=GLSceneViewer1.Width/14.2;
  Px:=GLSceneViewer1.Width div 2;
  Py:=GLSceneViewer1.Height div 2;
  GLSceneViewer1.Enabled:=true;
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  // Graphics
  SceneDynamic:=TVboMesh.CreateAsChild(GLScene1.Objects);
  SceneStatic:=TVboMesh.CreateAsChild(GLScene1.Objects);
  SceneStatic.AddPlane(10,10,4,4);
  SceneDynamic.AddSphere(1,20,20);
  SceneDynamic.AddSphere(0.5,20,20);
  SceneDynamic.ObjectsList[1].Position:=VectorMake(0,20,0);
  SceneDynamic.AddBox(1,1,1,1,1,1);
  // Physics
  v1:=AffineVectorMake(-100,-100,-100);
  v2:=AffineVectorMake(100,100,100);
  GamePhysics:=TVBONewtonWorld.Create(SceneDynamic.ObjectsList[2],
     GLScene1,2,2,v1,v2);
  GLSceneViewer1.Camera:=GamePhysics.Camera;
  GamePhysics.Camera.Position.Z:=3;
  GamePhysics.Camera.Position.Y:=2;
  GamePhysics.AddTreeBody(SceneStatic.ObjectsList[0]);
  GamePhysics.AddConvexBody(SceneDynamic.ObjectsList[1],100);
  GamePhysics.AddConvexBody(SceneDynamic.ObjectsList[0],100);
  GamePhysics.Px:=Px;
  GamePhysics.Py:=Py;
  GamePhysics.CamSpeed:=0.2;
  GamePhysics.SetMaterialBetweenWorldAndPlayer(
   NewtonMaterialMake(1,0,0.01,0.01,0));
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLSceneViewer1.Invalidate;
  GamePhysics.FPSApplyCamMove;
  if isKeyDown(VK_SPACE) then
    GamePhysics.FPSApplyKeyMove(87,83,68,65,true)
  else
    GamePhysics.FPSApplyKeyMove(87,83,68,65,false);
  GamePhysics.UpdateWorld(1);
  if isKeyDown(VK_ESCAPE) then begin
   GLCadencer1.Enabled:=false;
   Close;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if isKeyDown(13) then AddSphere;
end;

end.
