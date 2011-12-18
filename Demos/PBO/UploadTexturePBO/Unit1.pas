unit Unit1;

interface

uses
  Windows, Messages, Variants, Classes, Graphics, Controls, Forms, ExtCtrls,
  Dialogs, Math, GLScene, GLObjects, GLCoordinates, GLCadencer, GLWin32Viewer,
  GLCrossPlatform, BaseClasses, vboMesh, StdCtrls, vectorgeometry, OpenGL1x,
  GLSimpleNavigation, uTextures, uMiscUtils, OGLStateEmul;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLDummyCube1: TGLDummyCube;
    GLSimpleNavigation1: TGLSimpleNavigation;
    Button1: TButton;
    Button2: TButton;
    Panel1: TPanel;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  mesh:TVBOMesh;
  bw_tex: TTexture;
  queries: gluint;
  available:gluint;
  timeElapsed: GLuint64;
  t: pointer;

implementation

{$R *.dfm}

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.FormCreate(Sender: TObject);
var i: integer;
begin
GLSceneViewer1.Buffer.RenderingContext.Activate;
OGLStateEmul.GLStateCache.CheckStates;

  bw_tex:= TTexture.CreateFromFile('Media\rgb24_4k.jpg');
  bw_tex.SetWraps(twClampToEdge,twClampToEdge);
  bw_tex.SetFilters(mnLinearMipmapLinear, mgLinear);

  glGenQueries(1, @queries);
  getmem(t,2048*2048*16);
  for i:=0 to 2048*2048*8-1 do PByteArray(t)[i]:=random(255);//random(255)/255;

  bw_tex.SetWraps(twClampToEdge,twClampToEdge);
  bw_tex.SetFilters(mnLinearMipmapLinear, mgLinear);

  Mesh:=TVBOMesh.CreateAsChild(GLScene1.Objects);
  with Mesh.AddSphere(2,16,32) do begin
     Texture:=bw_tex;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
available:=0;
timeElapsed:=0;

  GLSceneViewer1.Buffer.RenderingContext.Activate;
  glBeginQuery(GL_TIME_ELAPSED, queries);
  bw_tex.UploadData(t,false);
  glEndQuery(GL_TIME_ELAPSED);
  while available=0 do begin
    glGetQueryObjectiv(queries, GL_QUERY_RESULT_AVAILABLE, @available);
  end;
  glGetQueryObjectui64vEXT(queries, GL_QUERY_RESULT, @timeElapsed);
  showmessage(floattostr(timeElapsed/1e6,5,2)+'ms');
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
available:=0;
timeElapsed:=0;

  GLSceneViewer1.Buffer.RenderingContext.Activate;
  glBeginQuery(GL_TIME_ELAPSED, queries);
  bw_tex.UploadData(t,true);
  glEndQuery(GL_TIME_ELAPSED);
  while available=0 do begin
    glGetQueryObjectiv(queries, GL_QUERY_RESULT_AVAILABLE, @available);
  end;
  glGetQueryObjectui64vEXT(queries, GL_QUERY_RESULT, @timeElapsed);
  showmessage(floattostr(timeElapsed/1e6,5,2)+'ms');
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
GLSceneViewer1.Buffer.RenderingContext.Activate;
  GLCadencer1.Enabled:=false;
  Mesh.Free; bw_tex.Free; freemem(t);
end;

end.

