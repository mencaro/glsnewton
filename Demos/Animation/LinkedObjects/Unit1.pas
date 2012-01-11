unit Unit1;

interface

uses
  Windows, Messages, SysUtilsLite, Controls, Forms, Classes,
  Dialogs, GLSimpleNavigation, GLScene, GLCoordinates, GLObjects,
  GLCadencer, GLWin32Viewer, GLCrossPlatform, BaseClasses,
  vboMesh, uBaseClasses, uMeshObjects, uTextures, uMaterialObjects,
  OpenGL1x, OGLStateEmul;
type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLDummyCube1: TGLDummyCube;
    GLSimpleNavigation1: TGLSimpleNavigation;
    procedure FormCreate(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  Meshes: TVBOMesh;
  Links: TLinkedObjects;
  sun, earth, moon: TVBOMeshObject;
  tSun, tEarth, tMoon, tSpace: TTexture;
implementation


{$R *.dfm}


procedure TForm1.FormCreate(Sender: TObject);
begin
  GLSceneViewer1.Buffer.RenderingContext.Activate;

  GLStateCache.CheckStates;

  tSun:=TTexture.CreateFromFile('Media\sun.bmp');
  tSun.TextureMode:=tcReplace;
  tSun.BlendingMode:=tbmAdditive;
  tEarth:=TTexture.CreateFromFile('Media\earth.jpg');
  tMoon:=TTexture.CreateFromFile('Media\moon.jpg');
  tSpace:=TTexture.CreateFromFile('Media\Space.jpg');
  tSpace.TextureMode:=tcReplace;

  Meshes:=TVBOMesh.CreateAsChild(GLScene1.Objects);
  Meshes.OldRender:=false;
  with Meshes.AddScreenQuad do begin
    NoZWrite:=true;
    MeshPlacement:=mpBackground;
    Texture:=tSpace;
  end;

  sun:=Meshes.AddSprite(stSpherical,1,1);
  sun.Texture:=tSun;
  sun.MoveObject(0,0,0);

  earth:=Meshes.AddSphere(1,16,32);
  with earth do begin
    Texture:=tEarth;
    MoveObject(0,0,10);
  end;

  moon:=Meshes.AddSphere(0.1,16,32);
  with moon do begin
    Texture:=tMoon;
    MoveObject(0,0,3);
  end;

  Links:=TLinkedObjects.Create;

  Links.NewJoint(Sun,nil,'Sun');
  Links.NewJoint(Earth,sun,'Earth');
  Links.NewJoint(Moon,Earth,'Moon');

end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLSceneViewer1.Invalidate;
  if assigned(Meshes) then begin
    sun.RotateAroundY(deltaTime/20,false);
    sun.RollObject(deltaTime/20);
    //Земля наследует вращение солнца, поворот земли наследуется луной
    Earth.RotateAroundY(deltaTime/5,false);
  end;

end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  GLCadencer1.Enabled:=false;
  GLSceneViewer1.Buffer.RenderingContext.activate;
  Meshes.Free;
  GLSceneViewer1.Buffer.RenderingContext.Deactivate;
end;

end.
