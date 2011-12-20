unit Unit1;

interface

uses
  Windows, Messages, SysUtilsLite, Controls, Forms, Classes,
  Dialogs, GLSimpleNavigation, GLScene, GLCoordinates, GLObjects,
  GLCadencer, GLWin32Viewer, GLCrossPlatform, BaseClasses,
  uVBO, vboMesh, OpenGL1x, uAnimatedMesh, OGLStateEmul;
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
  anim: TAnimatedMesh;

implementation


{$R *.dfm}


procedure TForm1.FormCreate(Sender: TObject);
begin
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  GLStateCache.CheckStates;
  Meshes:=TVBOMesh.CreateAsChild(GLScene1.Objects);
  anim:=TAnimatedMesh.Create;
  Meshes.AddMeshObject(anim);
  anim.PlayLoop:=true;
  anim.Visible:=true;
  anim.Load3dsMesh('Media\planet.3ds');
  anim.ScaleObject(100,100,100);
  anim.Play:=true;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  GLSceneViewer1.Buffer.RenderingContext.activate;
  GLCadencer1.Enabled:=false;
  Meshes.Visible:=false; Meshes.Free;
  GLSceneViewer1.Buffer.RenderingContext.Deactivate;
end;

end.
