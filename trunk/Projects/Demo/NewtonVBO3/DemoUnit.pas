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
    Timer2: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses VBOMesh, VBONewton, VectorTypes, VectorGeometry, GLMaterial, OpenGL1x;

var
  SceneDynamic: TVboMesh;
  SceneStatic: TVboMesh;
  GamePhysics: TVBONewtonWorld;
  Px, Py: 256 .. 1024;
{$R *.dfm}

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  GLSceneViewer1.Free;
  SceneStatic.Free;
  SceneDynamic.Free;
  GamePhysics.Destroy;
end;

function CreateRandomMaterial: TGLMaterial;
var
  r1, r2, r3: 1 .. 10;
begin
  Randomize;
  r1 := random(10);
  r2 := random(10);
  r3 := random(10);
  result := TGLMaterial.Create(nil);
  result.FrontProperties.Diffuse.Color := VectorMake(r1 / 10, r2 / 10, r3 / 10);
end;

procedure AddSphere;
var
  rx, ry, rz: - 15 .. 15;
  rs: 1 .. 10;
  i: integer;
begin
  Randomize;
  rs := random(10) + 1;
  SceneDynamic.AddSphere(rs / 10, 16, 16);
  rx := random(20) - 5;
  ry := random(5) + 10;
  rz := random(20) - 5;
  SceneDynamic.ObjectsList[SceneDynamic.Count - 1].Position := VectorMake
    (rx, ry, rz);
  SceneDynamic.ObjectsList[SceneDynamic.Count - 1].Material :=
    CreateRandomMaterial;
  GamePhysics.AddConvexBody(SceneDynamic.ObjectsList[SceneDynamic.Count - 1],
    55);
  GamePhysics.SetMaterialBetween2Meshes(GamePhysics.Player,
    GamePhysics.LastObject, NewtonMaterialMake(1, 0, 0.01, 0.01, 0, 1));
  for i := 0 to GamePhysics.NewtonObjectsCount - 1 do
    if GamePhysics.NewtonObjects[i].BodyPosition[1] < -5 then
      GamePhysics.NewtonObjects[i].BodyPosition := VectorMake(0, 10, 0);
  //GamePhysics.LastObject.MeshObject.BuildOctreeList(3);
end;

procedure AddBoxes;
var
  i: integer;
begin
  for i := -50 to 50 do
  begin
    SceneDynamic.AddBox(1, 2, 0.2, 4, 4, 4);
    SceneDynamic.ObjectsList[SceneDynamic.Count - 1].TurnObject(Gr2Rad(90));
    GamePhysics.AddConvexBody(SceneDynamic.ObjectsList[SceneDynamic.Count - 1],
      100);
    GamePhysics.LastObject.BodyPosition := VectorMake(-(i + 0.5), 1, 0);
    GamePhysics.SetMaterialBetween2Meshes
      (GamePhysics.Player, GamePhysics.LastObject, NewtonMaterialMake
        (1, 0, 0.01, 0.01, 0, 1));
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  v1, v2: TVector3f;
begin
  Caption :=
    'VBONewton Demo v.1 | Жми Enter для включения/выключения таймера сфер | F1,F2 - камера';
  Width := Screen.Width;
  Height := Screen.Height;
  Left := 0;
  Top := 0;
  ShowCursor(false);
  GLSceneViewer1.Width := Width;
  GLSceneViewer1.Height := Height;
  Px := GLSceneViewer1.Width div 2;
  Py := GLSceneViewer1.Height div 2;
  GLSceneViewer1.Enabled := true;
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  // Graphics
  SceneDynamic := TVboMesh.CreateAsChild(GLScene1.Objects);
  SceneStatic := TVboMesh.CreateAsChild(GLScene1.Objects);
  SceneStatic.AddPlane(110, 35, 10, 10);
  SceneStatic.BuildOctree(3);
  SceneDynamic.AddSphere(1, 16, 16, 4, 4);
  // Physics
  v1 := AffineVectorMake(-100, -20, -100);
  v2 := AffineVectorMake(100, 80, 100);
  GamePhysics := TVBONewtonWorld.Create(nil, GLScene1, GLScene1.Objects, 0, 0,
    v1, v2);
  GamePhysics.CreatePlayerSphere(1, 1, 1, SceneDynamic.ObjectsList[0]
      .Matrices.WorldMatrix);
  GamePhysics.Player.MeshObject := SceneDynamic.ObjectsList[0];
  GamePhysics.Player.BodyPosition := VectorMake(0, 0, 2);
  GLSceneViewer1.Camera := GamePhysics.Camera;
  GLSceneViewer1.Camera.FocalLength := GLSceneViewer1.Width / 16;
  GLSceneViewer1.Camera.Position.AsVector := VectorMake(0, 2, 4);
  GamePhysics.AddTreeBody(SceneStatic.ObjectsList[0]);
  GamePhysics.SetMaterialBetween2Meshes(GamePhysics.Player,
    GamePhysics.LastObject, NewtonMaterialMake(1, 0.15, 1, 1, 0, 1));
  GamePhysics.Px := Px;
  GamePhysics.Py := Py;
  GamePhysics.DebugColor := VectorMake(0, 1, 0, 1);
  GamePhysics.DebugGeometry := true;
  AddBoxes;
  //SceneDynamic.BuildOctree(3);
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
var
  i: integer;
  pos: TVector4f;
  rast: real;
begin
  rast := 0;
  GamePhysics.FPSApplyCamMove;

  if isKeyDown(VK_F1) then
    GLSceneViewer1.Camera.Position.AsVector := VectorMake(0, 2, 4);
  if isKeyDown(VK_F2) then
    GLSceneViewer1.Camera.Position.AsVector := VectorMake(0, 0.5, 0);

  for i := 0 to SceneStatic.Count - 1 do
    if SceneStatic.ObjectsList[i].OctreeRayCastIntersect
      (GamePhysics.Player.BodyPosition, VectorMake(0, -1, 0), @pos) then
    begin
      rast := GamePhysics.Player.BodyPosition[1] - pos[1];
      break;
    end;

  {if rast<=0 then
  for i := 0 to SceneDynamic.Count - 1 do
    if SceneDynamic.ObjectsList[i].OctreeRayCastIntersect
      (GamePhysics.Player.BodyPosition, VectorMake(0, -1, 0), @pos) then
    begin
      rast := GamePhysics.Player.BodyPosition[1] - pos[1];
      break;
    end;}

  GamePhysics.FPSApplyKeyMove(87, 83, 68, 65, false, rast);
  GamePhysics.UpdateWorld(1, rast);

  if isKeyDown(VK_ESCAPE) then
  begin
    GLCadencer1.Enabled := false;
    Close;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  AddSphere;
  if SceneDynamic.Count >= 150 then
  begin
    Timer1.Enabled := false;
    Timer2.Enabled := false;
  end;
end;

procedure TForm1.Timer2Timer(Sender: TObject);
begin
  if isKeyDown(13) then
    if Timer1.Enabled then
      Timer1.Enabled := false
    else
      Timer1.Enabled := true;
end;

end.
