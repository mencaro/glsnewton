unit Unit1;

interface

uses
  Windows, Messages, SysUtilsLite, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLSimpleNavigation, GLScene, GLObjects, GLCoordinates,
  GLMaterial, GLCadencer, GLWin32Viewer, GLCrossPlatform, BaseClasses,
  uVBO, vboMesh, uMeshObjects, uTextures, VectorGeometry, StdCtrls, OpenGL1x,
  OGLStateEmul;
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
  Meshes: TVBOMesh;
  AnimatedSprite: TVBOAnimatedSprite;
  Frames: TTexture;

implementation

{$R *.dfm}



procedure TForm1.FormCreate(Sender: TObject);
var i,j:integer;
    v:taffinevector;
    c:TVector;

begin
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  GLStateCache.CheckStates;
  //Создаем текстуру из файла и устанавливаем режимы смешивания
  Frames:=TTexture.CreateFromFile('Media\Anim32x4.jpg');
  Frames.BlendingMode:=tbmAdditive;
  Frames.TextureMode:=tcReplace;

  //добавляем к сцене наш контейнер
  Meshes:=TVBOMesh.CreateAsChild(GLScene1.Objects);
  for i:=0 to 10 do for j:=0 to 10 do
  with Meshes.AddAnimatedSprite(stSpherical,1,1) as TVBOAnimatedSprite do begin
       MoveObject(vectormake(i*2-10,j*2-10,0));
       Texture:=Frames;
       FramesDirection:=fdHorizontal;
       FramesCount:=128;
       HorFramesCount:=32;
       VertFramesCount:=4;
       FrameRate:=25;
       NoZWrite:=true;
       ToFrame(random(128)); //Переходим к случайному кадру анимации
       //FirstFrame;
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
  Frames.Free; Meshes.Free;
  GLSceneViewer1.Buffer.RenderingContext.Deactivate;
end;

end.
