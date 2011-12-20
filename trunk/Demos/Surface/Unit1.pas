unit Unit1;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms,
  Dialogs, GLCadencer, GLScene, GLObjects, GLCoordinates,
  GLWin32Viewer, GLCrossPlatform, BaseClasses, Vectorgeometry,
  StdCtrls, ExtCtrls, GLSimpleNavigation,
  //VBOMesh libs:
  VBOMesh, uTextures, uShaders, OGLStateEmul;
type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLLightSource1: TGLLightSource;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLSimpleNavigation1: TGLSimpleNavigation;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function CreateShader: cardinal;
    procedure ApplyShader(mo: TObject);
    procedure unApplyShader(mo: TObject);
  end;

var
  Form1: TForm1;
  world: TVBOMesh;
  tex,pal: TTexture;
  Shaders: TShaders;
  spId: cardinal;
implementation

{$R *.dfm}

function GetHeight(X,Y: integer): single;
var xt,yt: single;
begin
  xt:=(x-7)/10; yt:=(y-7)/10;
  result:=xt*xt*xt-3*xt*yt*yt;
end;

procedure TForm1.ApplyShader(mo: TObject);
var texScale: TVector;
begin
  Tex.Apply(0); pal.Apply(1);
  Shaders.UseProgramObject(spId);
  Shaders.SetUniforms(spId,'TileTex',0);
  Shaders.SetUniforms(spId,'IntensTex',1);
  texScale[0]:=20; TexScale[1]:=20;
  TexScale[2]:=1/4; TexScale[3]:=0;
  Shaders.SetUniforms(spId,'TexScale',texScale);
end;

procedure TForm1.unApplyShader(mo: TObject);
begin
  Shaders.EndProgram;
  Pal.UnApply(1); Tex.UnApply(0);
end;

function TForm1.CreateShader: cardinal;
var VS,FS: ansistring;
begin
VS:=
'varying vec2 TexCoord;                 '+#13#10+
'varying float intens;                 '+#13#10+
'void main(void)                        '+#13#10+
'{                                      '+#13#10+
'  gl_Position = ftransform();          '+#13#10+
'  vec4 ecPosition = gl_ModelViewMatrix * gl_Vertex;'+#13+#10+
'  vec3 normal = gl_NormalMatrix * gl_Normal;'+#13+#10+
'  normal = normalize(normal);'+#13+#10+
'  vec3 p = gl_LightSource[0].position.xyz - ecPosition.xyz;'+#13+#10+
'  gl_FrontColor.rgb = ((0.8 * max(dot(normal, p), 0.0)) + 0.2) * vec3(1.0);'+#13+#10+
'  gl_FrontColor.a = 1.0;'+#13+#10+
'  intens = gl_Vertex.y;'+#13+#10+
'  TexCoord = gl_MultiTexCoord0.xy;'+#13+#10+
'}';

FS:=
'uniform sampler2D TileTex;             '+#13#10+
'uniform sampler2D IntensTex;           '+#13#10+
'uniform vec4 TexScale;                 '+#13#10+
'varying vec2 TexCoord;                 '+#13#10+
'varying float intens;                  '+#13#10+
'void main(void)                        '+#13#10+
'{                                      '+#13#10+
'  vec4 color = texture2D(TileTex,TexCoord*TexScale.xy);'+#13+#10+
'  vec4 icolor = texture2D(IntensTex,vec2(TexScale.w+intens*TexScale.z,0));'+#13+#10+
'  gl_FragColor = color*gl_Color*icolor;'+#13+#10+
'}';

result:=Shaders.CreateShader(VS,FS);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  //Активируем контекст OGL и делаем сохраняем состояния переменных OGL
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  OGLStateEmul.GLStateCache.CheckStates;
  //Создаем текстуру из файла
  tex:=TTexture.CreateFromFile('Media\tex16bw.bmp');
  tex.SetFilters(mnLinearMipmapLinear, mgLinear);
  tex.TwoSides:=true;
  pal:=TTexture.CreateFromFile('Media\Palette.bmp');

  //Добавляем к сцене контейнер с примитивами
  world:=TVBOMesh.CreateAsChild(GLScene1.Objects);

  //Добавляем к контейнеру параметрическую поверхность
  with world.AddPlane(10,10,20,20,GetHeight) do begin
    Texture:=tex;
    onBeforeRender:=ApplyShader;
    onAfterRender:=unApplyShader;
  end;
  //Создаем шейдер для наложения интенсивности
  Shaders:=TShaders.Create;
  spid:=CreateShader;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

end.
