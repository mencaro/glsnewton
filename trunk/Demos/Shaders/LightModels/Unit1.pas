unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, GLScene, GLCoordinates, GLWin32Viewer, GLCrossPlatform,
  BaseClasses, GLCadencer, GLObjects, StdCtrls, GLSimpleNavigation,
  //VBOMesh Lib
  vboMesh, uShaders, uAnimatedMesh, OGLStateEmul;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLDummyCube1: TGLDummyCube;
    GLCadencer1: TGLCadencer;
    GLSimpleNavigation1: TGLSimpleNavigation;
    GLSphere1: TGLSphere;
    RadioGroup1: TRadioGroup;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ApplyShader(mo: TObject);
    procedure unApplyShader(mo: TObject);
  end;

var
  Form1: TForm1;
  Shaders: TShaders;
  Mesh: TVBOMesh;
  BlinnId, LambId, PhongId: integer;
implementation

{$R *.dfm}

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.ApplyShader(mo: TObject);
begin
  with Shaders do begin
    case RadioGroup1.ItemIndex of
      0: exit; //отменяем шейдер
      1: UseProgramObject(LambId);  //Ламберта
      2: UseProgramObject(BlinnId); //Блинна
      3: UseProgramObject(PhongId); //Фонга
    end;
  end;
end;


procedure TForm1.unApplyShader(mo: TObject);
begin
  //Деактивируем шейдер
  if RadioGroup1.ItemIndex>0 then Shaders.EndProgram;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  OGLStateEmul.GLStateCache.CheckStates;
  mesh:=TVBOMesh.CreateAsChild(glscene1.Objects);
  with mesh.AddMeshFromFile('Media\teapot.3ds') do begin
    with MaterialObject.AddNewMaterial('Teapot').Properties do begin
      DiffuseColor.SetColor(233,111,52,255);
      SpecularColor.SetColor(255,255,255,255);
      Shininess:=32;
    end;
    RotateAroundX(-pi/2);
    MoveObject(0,-0.5,0);
    ScaleObject(3,3,3);
    onBeforeRender:=ApplyShader;
    onAfterRender:=unApplyShader;
  end;

  //Создаем контейнер шейдеров и грузим в него шейдера
  Shaders:=TShaders.Create;
  LambId:=Shaders.LoadShader('Media\Lambert.Vert','Media\Lambert.Frag');
  BlinnId:=Shaders.LoadShader('Media\Blinn.Vert','Media\Blinn.Frag');
  PhongId:=Shaders.LoadShader('Media\Phong.Vert','Media\Phong.Frag');
end;

end.
