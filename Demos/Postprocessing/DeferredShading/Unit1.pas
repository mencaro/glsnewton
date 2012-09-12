unit Unit1;

interface

uses
  Windows, Messages, SysUtilsLite, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLObjects, GLCoordinates, GLCadencer, GLWin32Viewer,
  GLCrossPlatform, BaseClasses, StdCtrls, vectorgeometry, GLSimpleNavigation,
  vboMesh, uTextures, uFBO, uShaders, uTBN, uVBO, OpenGL1x, OGLStateEmul;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLDummyCube1: TGLDummyCube;
    GLSimpleNavigation1: TGLSimpleNavigation;
    CheckBox1: TCheckBox;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ApplyShader(ShaderProgram: TShaderProgram);
    procedure UnApplyShader(ShaderProgram: TShaderProgram);
    procedure ApplyPass2(mo: TObject);
    procedure UnApplyPass2(mo: TObject);
  end;

var
  Form1: TForm1;
  mesh: TVBOMesh;
  mrt: array[0..2] of TTexture;
  NormalMap: TTexture;
  DiffMap: TTexture;
  Shader1,Shader2: TShaderProgram;
  Pass1: TMeshContainer;
  ObjCol: TMeshCollection;

implementation

{$R *.dfm}

procedure SetTBNBufferLoc(Buffer: PVBOBuffer; SP : TShaderProgram);
begin
  CalculateTangentsAndBinormals(Buffer^);
  RebuildVBOBuff(Buffer^, false);
  SP.GetAttribLocation('Tangent', Buffer^);
  SP.GetAttribLocation('Binormal', Buffer^);
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  if CheckBox1.Checked then
    GLLightSource1.Position.SetPoint(5*sin(newTime/1),5*cos(newTime/1),5*(sin(newTime/10)+cos(newtime/1)));
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.FormCreate(Sender: TObject);
var i: integer;
    buff: PVBOBuffer;
begin
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  OGLStateEmul.GLStateCache.CheckStates;
  //Создаем шейдер первого прохода и назначаем ему события Apply/UnApply
  Shader1:=TShaderProgram.Create('Media\Shader.Vert', 'Media\Shader.Frag');
  Shader1.onApplyShader:=ApplyShader;
  Shader1.onUnApplyShader:=UnApplyShader;
  //Создаем шейдер второго прохода
  Shader2:=TShaderProgram.Create('Media\Shader2.Vert', 'Media\Shader2.Frag');
  //Грузим дифузную текстуру и нормал мап к ней
  NormalMap:=TTexture.CreateFromFile('Media\NormalMap.bmp');
  DiffMap:=TTexture.CreateFromFile('Media\DiffMap.bmp');
  //Создаем 3 рендертаргета под G-Buffer
  for i:=0 to 2 do begin
    mrt[i]:=TTexture.Create;
    with mrt[i] do begin
      SetFilters(mnNearest, mgNearest);
      CreateRGBA32FTexture2D(256,256);
    end;
  end;

  //Создаем рендер
  Mesh:=TVBOMesh.CreateAsChild(GLScene1.Objects);
  Mesh.OldRender:=false;
  //Добавляем на сцену контейнер для рендеринга первого прохода отложенного освещения
  Pass1:=Mesh.AddNewContainer;
  with Pass1 do begin
    Render.Active:=true; Priority:=-1;
    //Указываем что рендеринг будет производиться в текстуру
    Render.RenderBuffer:=rtFrameBuffer;
    //Конфигурируем FBO и прикрепляем к нему наш G-Buffer
    Render.FBO.ConfigFBO([rbDepth]);
    Render.FBO.ConfigDepthBuffer(bmBuffer,dp24);
    Render.AttachTexture(mrt[0],tgMRT0);
    Render.AttachTexture(mrt[1],tgMRT1);
    Render.AttachTexture(mrt[2],tgMRT2);
    //Задаем шейдер для первого прохода отложенного освещения
    Shader:=Shader1;
  end;

  //Формируем сцену (в нашем случае один шарик)
  ObjCol:=Pass1.Collection;
  with ObjCol.AddSphere(2,16,32) do begin
     MoveObject(0,0,0); buff:=MeshList[0];
     //генерируем TBN для сферы
     SetTBNBufferLoc(buff,Shader1);
  end;

  //Добавляем на сцену скринквад для второго прохода
  with Mesh.AddScreenQuad do begin
    //задаем обработчики событий, в которых будут передаваться юниформы
    //для втрого прохода шейдера (альтернативный вариант установки параметров шейдера)
    onBeforeRender:=ApplyPass2;
    onAfterRender:=UnApplyPass2;
  end;

end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
var i:integer;
begin
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  Mesh.Free; for i:=0 to 2 do mrt[i].Free;
  NormalMap.Free; DiffMap.Free; Shader1.Free; Shader2.Free;
end;

procedure TForm1.ApplyPass2(mo: TObject);
var lpos: TVector;
begin
  lpos:=VectorTransform(GLLightSource1.AbsolutePosition,Mesh.ViewMatrix);
  mrt[0].Apply(0); mrt[1].Apply(1); mrt[2].Apply(2);
  Shader2.Apply;
  Shader2.SetUniforms('NormZTex',0);
  Shader2.SetUniforms('ColorTex',1);
  Shader2.SetUniforms('PosTex',2);
  Shader2.SetUniforms('LightPos',lpos);
end;

procedure TForm1.ApplyShader(ShaderProgram: TShaderProgram);
begin
  DiffMap.Apply(0); NormalMap.Apply(1);
  Shader1.SetUniforms('DiffuseMap',0);
  Shader1.SetUniforms('NormalMap',1);
end;

procedure TForm1.UnApplyPass2(mo: TObject);
begin
  Shader2.UnApply;
end;

procedure TForm1.UnApplyShader(ShaderProgram: TShaderProgram);
begin
  NormalMap.UnApply; DiffMap.UnApply;
end;

end.

