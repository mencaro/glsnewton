unit Unit1;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms,
  Dialogs, GLCadencer, GLScene, GLObjects, GLCoordinates,
  GLWin32Viewer, GLCrossPlatform, BaseClasses, Vectorgeometry,
  StdCtrls, ExtCtrls, GLSimpleNavigation,
  //VBOMesh libs:
  VBOMesh, uMeshObjects, uTextures, uShaders, PerlinNoise2D, OGLStateEmul;
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
  Ter: TVBOTerrain;
  Map2D: TByteArray2d;
  data: pointer;

implementation

{$R *.dfm}

function GetHeight(X,Y: integer): single;
begin
  result:=Map2D[Y,X];
end;

procedure TForm1.ApplyShader(mo: TObject);
var texScale: TVector;
begin
  Tex.Apply(0); pal.Apply(1);
  Shaders.UseProgramObject(spId);
  Shaders.SetUniforms(spId,'TileTex',0);
  Shaders.SetUniforms(spId,'IntensTex',1);
  texScale[0]:=256; TexScale[1]:=256;
  TexScale[2]:=1/256; TexScale[3]:=0;
  Shaders.SetUniforms(spId,'TexScale',texScale);
end;

procedure TForm1.unApplyShader(mo: TObject);
begin
  Shaders.EndProgram;
  Pal.UnApply; Tex.UnApply;
end;

function TForm1.CreateShader: cardinal;
var VS,FS: ansistring;
begin
VS:=
'varying vec2 TexCoord;                 '+#13#10+
'varying float intens;                  '+#13#10+
'varying vec3 snm, normal, lightDir, eyeVec; '+#13#10+
'void main(void)                        '+#13#10+

'{                                      '+#13#10+
'  gl_Position = ftransform();          '+#13#10+
'  normal = gl_NormalMatrix * gl_Normal; '+#13+#10+
'  snm = gl_Normal;'+#13+#10+
'  vec3 vVertex = vec3(gl_ModelViewMatrix * gl_Vertex);'+#13+#10+
'  lightDir = vec3(gl_LightSource[0].position.xyz - vVertex);'+#13+#10+
'  eyeVec = -vVertex;'+#13+#10+
'  intens = gl_Vertex.y;'+#13+#10+
'  TexCoord = gl_MultiTexCoord0.xy;'+#13+#10+
'}';

FS:=
'uniform sampler2D TileTex;             '+#13#10+
'uniform sampler2D IntensTex;           '+#13#10+
'uniform vec4 TexScale;                 '+#13#10+
'varying vec2 TexCoord;                 '+#13#10+
'varying float intens;                  '+#13#10+
'varying vec3 snm, normal, lightDir, eyeVec; '+#13#10+
'void main(void)                        '+#13#10+
'{                                      '+#13#10+
'  vec4 final_color ='+#13+#10+
'    (gl_FrontLightModelProduct.sceneColor * gl_FrontMaterial.ambient) +'+#13+#10+
'    (gl_LightSource[0].ambient * gl_FrontMaterial.ambient);'+#13+#10+
'  vec3 N = normalize(normal);'+#13+#10+
'  vec3 L = normalize(lightDir);'+#13+#10+
'  float lambertTerm = max (dot(N,L), 0.0);'+#13+#10+
'  final_color += gl_LightSource[0].diffuse * gl_FrontMaterial.diffuse * lambertTerm;'+#13+#10+
'  vec4 color = texture2D(TileTex,TexCoord*TexScale.xy);'+#13+#10+
'  vec4 icolor = texture2D(IntensTex,vec2(TexScale.w+intens*TexScale.z,0.0));'+#13+#10+

'  gl_FragColor = color*final_color*icolor;'+#13+#10+
//'  gl_FragColor = final_color;'+#13+#10+
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
  world.OldRender:=false;

  RandSeed:=157894; Randomize;
  GenNoise(Map2D,256);

  Ter:=TVBOTerrain.Create;
  with Ter do begin
    //FaceMode:=fmLines;
    BuildTerrain(256,256,GetHeight,32);
    RebuildNormals;
    Scale:=vectormake(4,1,4);
    MoveObject(0,-128,0);
    TwoSides:=true;
//    MaterialObject.TwoSideLighting:=true;
    onBeforeRender:=ApplyShader;
    onAfterRender:=UnApplyShader;
    Visible:=true;
  end;
  World.AddMeshObject(ter);

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
