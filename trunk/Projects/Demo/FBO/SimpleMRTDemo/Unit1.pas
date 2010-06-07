unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLObjects, GLCoordinates, GLCadencer, GLWin32Viewer,
  GLCrossPlatform, BaseClasses, vboMesh, StdCtrls, vectorgeometry, OpenGL1x,
  GLSimpleNavigation, MTLLoader, uTextures, uFBO, uShaders, GLGraphics,
  GLRenderContextInfo;

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
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RadioButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ApplyShader(rci:TRenderContextInfo;  mo:TObject);
    procedure UnApplyShader(rci:TRenderContextInfo; mo:TObject);
  end;

var
  Form1: TForm1;
  mesh:TVBOMesh;
  bw_tex: TGLTexture;
  Shaders: TShaders;
  spId: integer;
  mrt: array[0..3] of TGLTexture;
  
implementation

{$R *.dfm}

Function CreateMRTShader(Vertex,Fragment: string): integer;
var vsId, fsId: integer;
    v,f: TStringList;
begin
  v:=TstringList.Create;
  f:=TstringList.Create;
  v.LoadFromFile(Vertex);
  f.LoadFromFile(Fragment);
  Shaders:= TShaders.Create;
  with Shaders do begin
    vsId := AddShaderObject(v.Text,GL_VERTEX_SHADER);
    fsId := AddShaderObject(f.Text,GL_FRAGMENT_SHADER);
    vsId := ShaderObjectsList[vsId];
    fsId := ShaderObjectsList[fsId];

    result:=CreateShaderProgram;

    AttachShaderObjectToProgram(vsId,result);
    AttachShaderObjectToProgram(fsId,result);

    LinkShaderProgram(result);
  end;
  v.Free; f.Free;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.FormCreate(Sender: TObject);
var i:integer;
    bmp:Tbitmap;
    bm32: TGLBitmap32;
begin
GLSceneViewer1.Buffer.RenderingContext.Activate;
  bm32:= TGLBitmap32.Create;
  bmp:=Tbitmap.Create;
  bmp.LoadFromFile('..\Media\Tile.bmp');
  bm32.AssignFromBitmap24WithoutRGBSwap(bmp);

  bw_tex:= TGLTexture.Create;
  with bw_tex do begin
    SetFilters(mnLinearMipmapLinear, mgLinear);
    CreateBGRA8Texture2D(bmp.Width,bmp.Height,bm32.Data);
  end; bm32.Free; bmp.Free;

  spId:=CreateMRTShader('..\Project\Demo\FBO\SimpleMRTDemo\Shader.Vert',
                        '..\Project\Demo\FBO\SimpleMRTDemo\Shader.Frag');

  for i:=0 to 3 do begin
    mrt[i]:=TGLTexture.Create;
    with mrt[i] do begin
      SetFilters(mnLinearMipmapLinear, mgLinear);
      CreateRGBA8Texture2D(256,256);
    end;
  end;
  
  Mesh:=TVBOMesh.CreateAsChild(GLScene1.Objects);
  with Mesh.AddSphere(2,16,32) do begin
     MoveObject(0,0,0);
     TextureId:=bw_tex.Handle;
     with FBO do begin
       for i:=0 to 3 do AttachTexture(mrt[i]);
       SetReadBackBuffer([0,1,2,3]);
       InitFBO(256,256);
       Active:=true;
     end;
     onBeforeRender:=ApplyShader;
     onAfterRender:=UnApplyShader;
  end;

  with Mesh.AddScreenQuad do begin
     TextureId:=mrt[0].Handle;
  end;

end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
var i:integer;
begin
GLSceneViewer1.Buffer.RenderingContext.Activate;
  Mesh.Free;
  bw_tex.Free;
  for i:=0 to 3 do mrt[i].Free;
end;

procedure TForm1.RadioButton1Click(Sender: TObject);
begin
  Mesh[1].TextureId:=mrt[TRadioButton(sender).tag].Handle;
end;

procedure TForm1.ApplyShader(rci: TRenderContextInfo; mo: TObject);
begin
  Shaders.UseProgramObject(spid);
end;

procedure TForm1.UnApplyShader(rci: TRenderContextInfo; mo: TObject);
begin
  Shaders.UseProgramObject(0);
end;

end.

