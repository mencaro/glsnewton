unit Unit1;

interface

uses
  Windows, Messages, SysUtilsLite, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLObjects, GLCoordinates, GLCadencer, GLWin32Viewer,
  GLCrossPlatform, BaseClasses, StdCtrls, vectorgeometry,
  GLSimpleNavigation, vboMesh, uTextures, uFBO, uShaders, OGLStateEmul;

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
    procedure ApplyShader(mo:TObject);
    procedure UnApplyShader(mo:TObject);
  end;

var
  Form1: TForm1;
  mesh:TVBOMesh;
  Shaders: TShaders;
  spId: integer;
  mrt: array[0..3] of TTexture;
  
implementation

{$R *.dfm}

Function CreateMRTShader(Vertex,Fragment: string): integer;
var v,f: TStringList;
begin
  v:=TstringList.Create;
  f:=TstringList.Create;
  v.LoadFromFile(Vertex);
  f.LoadFromFile(Fragment);
  Shaders:= TShaders.Create;
  result:=Shaders.CreateShader(v.Text,f.Text);
  v.Free; f.Free;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.FormCreate(Sender: TObject);
var i:integer;
begin
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  OGLStateEmul.GLStateCache.CheckStates;

  //Создаем шейдер осуществляющий вывод в 4 буфера цвета
  spId:=CreateMRTShader('Media\Shader.Vert', 'Media\Shader.Frag');

  //Создаем 4 текстуры под буферы цвета
  for i:=0 to 3 do begin
    mrt[i]:=TTexture.Create;
    with mrt[i] do begin
      SetFilters(mnLinearMipmapLinear, mgLinear);
      CreateRGBA8Texture2D(256,256);
    end;
  end;

  Mesh:=TVBOMesh.CreateAsChild(GLScene1.Objects);
  with Mesh.AddSphere(2,16,32) do begin
     MoveObject(0,0,0);
     with FBO do begin
       //Инициализируем FBO
       InitFBO(256,256);
       //Присоединяем к буферу кадра 4 текстуры
       for i:=0 to 3 do AttachTexture(mrt[i]);
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
  Mesh.Free; for i:=0 to 3 do mrt[i].Free;
end;

procedure TForm1.RadioButton1Click(Sender: TObject);
begin
  Mesh[1].TextureId:=mrt[TRadioButton(sender).tag].Handle;
end;

procedure TForm1.ApplyShader(mo: TObject);
begin
  Shaders.UseProgramObject(spid);
end;

procedure TForm1.UnApplyShader(mo: TObject);
begin
  Shaders.UseProgramObject(0);
end;

end.

