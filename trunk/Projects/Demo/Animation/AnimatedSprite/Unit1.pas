unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLSimpleNavigation, GLScene, GLObjects, GLCoordinates,
  GLMaterial, GLCadencer, GLWin32Viewer, GLCrossPlatform, BaseClasses,
  uVBO, vboMesh, VectorLists, VectorGeometry, StdCtrls, OpenGL1x, Jpeg,
  ComCtrls, ExtCtrls, Math, TGA;
type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLDummyCube1: TGLDummyCube;
    GLSimpleNavigation1: TGLSimpleNavigation;
    procedure FormCreate(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  Meshes:TVBOMesh;
  AnimatedSprite:TVBOAnimatedSprite;

implementation

uses GLTexture;

{$R *.dfm}



procedure TForm1.FormCreate(Sender: TObject);
var i,j:integer;
    v:taffinevector;
    c:TVector;
begin
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  with GLMaterialLibrary1.AddTextureMaterial('Frames','..\Media\Anim32x4.jpg').Material do
  begin
    BlendingMode:=bmAdditive;
    Texture.ImageAlpha:=tiaTopLeftPointColorTransparent;
    Texture.TextureMode:=tmReplace;
    Texture.Disabled := False;
  end;
  //добавляем к сцене наш контейнер
  Meshes:=TVBOMesh.CreateAsChild(GLScene1.Objects);
  Meshes.MaterialLibrary:=GLMaterialLibrary1;
  for i:=0 to 10 do for j:=0 to 10 do
  with Meshes.AddAnimatedSprite(stSpherical,1,1) as TVBOAnimatedSprite do begin
       MoveObject(vectormake(i*2-10,j*2-10,0));
       Material:=GLMaterialLibrary1.LibMaterialByName('Frames').Material;
       FramesDirection:=fdHorizontal;
       FramesCount:=128;
       HorFramesCount:=32;
       VertFramesCount:=4;
       FrameRate:=25;
       NoZWrite:=true;
       ToFrame(i*11+j);
//       FirstFrame;
       Play;
  end;

end;

procedure TForm1.FormResize(Sender: TObject);
begin
   GLSceneViewer1.Buffer.ClearBuffers;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
var i,j:integer;
begin
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  GLCadencer1.Enabled:=false;
  Meshes.Visible:=false;
  GLSceneViewer1.Buffer.RenderingContext.Deactivate;
end;

end.
