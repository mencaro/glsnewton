unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLCoordinates, GLWin32Viewer, GLCrossPlatform, OpenGL1x,
  BaseClasses, GLCadencer, GLObjects, StdCtrls, VectorGeometry, GLSimpleNavigation,
  ExtCtrls,
  //VBOMesh Lib
  vboMesh, uShaders, OGLStateEmul;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLDummyCube1: TGLDummyCube;
    GLCadencer1: TGLCadencer;
    GLSimpleNavigation1: TGLSimpleNavigation;
    Panel1: TPanel;
    Memo1: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ApplyShader1(mo: TObject);
    procedure ApplyShader2(mo: TObject);
    procedure unApplyShader(mo: TObject);
  end;

Const
  EmptyVP: string=
'#version 120                                   '+#13#10+
'uniform vec4[2] color;                         '+#13#10+
'void SendNormal(void);                 '+#13#10+
'void SendEmpty(void);                 '+#13#10+
'void main(void)                        '+#13#10+
'{                                      '+#13#10+
'  gl_Position = ftransform();          '+#13#10+
'  SendNormal();                        '+#13#10+
'}';
  SendNormToFS:string =
'#version 120                                   '+#13#10+
'uniform vec4[2] color;                         '+#13#10+
'varying vec3 normal;                   '+#13#10+
'void SendNormal(void)                  '+#13#10+
'{                                      '+#13#10+
'  normal = normalize(gl_NormalMatrix * gl_Normal);  '+#13#10+
'}';

  EmptyFP:string=
'#version 120                                   '+#13#10+
'varying vec3 normal;                           '+#13#10+
'uniform vec4[2] color;                         '+#13#10+
'void main(void)                                '+#13#10+
'{                                              '+#13#10+
'  float c = dot(color[1].rgb,normal);          '+#13#10+
'  gl_FragColor = color[0]*c;                   '+#13#10+
'}';


var
  Form1: TForm1;
  Shaders: TShaders;
  spId:integer;
  Mesh: TVBOMesh;

implementation

{$R *.dfm}

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.ApplyShader1(mo: TObject);
var colors:array[0..1] of TVector;
begin
  label1.Caption:='';
  colors[0]:=vectormake(0.5,1,0.7,1);
  colors[1]:=vectormake(1,1,1,1);
  with Shaders do begin
    //Активируем шейдерную программу
    UseProgram('TestShader');
    //Передаем в шейдер массив colors (2 элемента)
    SetUniforms('TestShader','color',colors[0],2);
  end;
end;

procedure TForm1.ApplyShader2(mo: TObject);
var colors:array[0..1] of TVector;
begin
    colors[0]:=vectormake(0.0,1,1,1);
    colors[1]:=vectormake(1,1,0.5,1);
    with Shaders do begin
    //Активируем шейдерную программу
    UseProgram('TestShader');
    //Передаем в шейдер массив colors (2 элемента)
    SetUniforms('TestShader','color',colors[0],2);
    //Пытаемся передать данные в несуществующую юниформу
    SetUniforms('TestShader','Test',colors[0],2);
  end;
end;

procedure TForm1.unApplyShader(mo: TObject);
begin
  //Деактивируем шейдер
  Shaders.EndProgram;
  //Выводим список полученных предупреждений
  label1.Caption:=label1.Caption+Shaders.UniformsWarnings;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  OGLStateEmul.GLStateCache.CheckStates;
  mesh:=TVBOMesh.CreateAsChild(glscene1.Objects);
  //Добавляем на сцену два кубика и прописываем события применения шейдера
  with mesh.AddBox(1,1,1,1,1,1)do begin
    MoveObject(2,0,0);
    onBeforeRender:=ApplyShader1; //вызывается перед рендерингом
    onAfterRender:=unApplyShader; //вызывается после рендеринга
  end;
  with mesh.AddBox(1,1,1,1,1,1) do begin
    MoveObject(-2,0,0);
    onBeforeRender:=ApplyShader2;
    onAfterRender:=unApplyShader;
  end;

  //Создаем контейнер шейдеров
  Shaders:=TShaders.Create;
  with Shaders do begin
    //Создаем шейдерную программу
    CreateShaderProgram('TestShader');
    //Добавляем к контейнеру шейдеров код первого шейдерного объекта EmptyVP,
    //сохраняем его под именем 'EmptyVP'
    AddShaderObject(EmptyVP,GL_VERTEX_SHADER,'EmptyVP');
    //Добавляем код второго шейдерного объекта SendNormToFS
    AddShaderObject(SendNormToFS,GL_VERTEX_SHADER,'SendNormal');
    //Добавляем код фрагметного шейдерного объекта EmptyFP
    AddShaderObject(EmptyFP,GL_FRAGMENT_SHADER,'EmptyFP');

    //Присоединяем к шейдерной программе 'TestShader' объект 'SendNormal'
    AttachShaderToProgram('SendNormal','TestShader');
    //Присоединяем к шейдерной программе 'TestShader' объект 'EmptyVP'
    AttachShaderToProgram('EmptyVP','TestShader');
    //Присоединяем к шейдерной программе 'TestShader' объект 'EmptyFP'
    AttachShaderToProgram('EmptyFP','TestShader');
    //Компилируем шейдерную программу
    LinkShaderProgram('TestShader');
    //выводим в мемо лог компиляции
    memo1.Lines.Add(Logs);
  end;
end;

end.
