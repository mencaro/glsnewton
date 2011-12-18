unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLObjects, GLCoordinates, GLCadencer, GLWin32Viewer,
  GLCrossPlatform, BaseClasses, vboMesh, StdCtrls, vectorgeometry,
  GLSimpleNavigation, uTextures, uFBO, uMaterialObjects, OGLStateEmul;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLDummyCube1: TGLDummyCube;
    GLSimpleNavigation1: TGLSimpleNavigation;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RadioButton2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  mesh:TVBOMesh;
  bw_tex, fbo_tex, depth_tex: TTexture;
implementation

{$R *.dfm}

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  GLStateCache.CheckStates;
  //Грузим текстуру объекта
  bw_tex:= TTexture.CreateFromFile('Media\Tile.bmp');
  bw_tex.TextureMode:=tcModulate;

  //Создаем текстуру в которую будет осуществляться рендеринг
  fbo_tex:= TTexture.Create;
  with fbo_tex do begin
    SetFilters(mnLinearMipmapLinear, mgLinear);
    CreateBGRA8Texture2D(256,256);
    TextureMode:=tcModulate;
  end;
  //Создаем текстуру в которой будет сохранен буфер глубины
  depth_tex:=TTexture.Create;
  with depth_tex do begin
    CreateDepth32FTexture2D(256,256);
    TextureMode:=tcModulate;
  end;

  Mesh:=TVBOMesh.CreateAsChild(GLScene1.Objects);
  //Добавляем на сцену объект и настраиваем буфер FBO
  with Mesh.AddSphere(2,16,32) do begin
     MoveObject(0,0,0);
     Texture:=bw_tex;
     //говорим что нужно использовать буфер глубины
     fbo.ConfigFBO([rbDepth]);
     //присоединяем к FBO текстуру для буфера цвета
     fbo.AttachTexture(fbo_tex);
     //говорим что буфер глубины будет иметь точность 32 бита и будет соранен в текстуру
     fbo.ConfigDepthBuffer(bmTexture,dp32);
     //Присоединяем к FBO текстуру для буфера глубины
     fbo.AttachDepthTexture(depth_tex);
     //инициализируем FBO с заданными размерами текстуры
     fbo.InitFBO(fbo_tex.Width,fbo_tex.Height);
     //говорим рендеру что это FBO активно
     fbo.Active:=true;
  end;
  //добавляем на сцену скринквад и применяем к нему полученную текстуру
  with Mesh.AddScreenQuad do begin
     Texture:=fbo_tex;
     MaterialObject.Blending.SetByMode(bmOpaque);
  end;

end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  Mesh.Free; bw_tex.Free; fbo_tex.Free;
end;

procedure TForm1.RadioButton2Click(Sender: TObject);
begin
  case TRadioButton(sender).tag of
     0: Mesh[1].Texture:=fbo_tex;
     1: Mesh[1].Texture:=depth_tex;
  end;
end;

end.

