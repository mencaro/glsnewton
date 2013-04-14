unit Unit1;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms, ComCtrls,
  Dialogs, GLCadencer, GLScene, GLObjects, GLCoordinates,
  GLWin32Viewer, GLCrossPlatform, BaseClasses, Vectorgeometry,
  StdCtrls, ExtCtrls, GLSimpleNavigation,
  //VBOMesh libs:
  VBOMesh, uMeshObjects, uTextures, uFBO, uShaders, OGLStateEmul;
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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    procedure ApplyShader(ShaderProgram: TShaderProgram);
    { Public declarations }
  end;

var
  Form1: TForm1;
  world: TVBOMesh;
  tex: TTexture;
  fbotex,fbotex2,fbotex3: TTexture;
  ObjCol: TMeshCollection;
  Pass1,Pass2,Pass3: TMeshContainer;
implementation

{$R *.dfm}

procedure TForm1.ApplyShader(ShaderProgram: TShaderProgram);
begin
  ShaderProgram.SetUniforms('TexSize',Vectormake(fbotex.Width,fbotex.Height,0,0));
  ShaderProgram.SetUniforms('image',0);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  GLCadencer1.Enabled:=false;
  tex.Free; fbotex.Free; world.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
var Sphere: TVBOMeshObject;
    color: TVector;
    h: single;
begin
  //Активируем контекст OGL и делаем сохраняем состояния переменных OGL
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  OGLStateEmul.GLStateCache.CheckStates;
  //Создаем текстуру из файла
  tex:=TTexture.CreateFromFile('Media\tile.bmp');

  //Добавляем к сцене основной рендер
  world:=TVBOMesh.CreateAsChild(GLScene1.Objects);
  world.OldRender:=false;
  //Добавляем к сцене контейнер с примитивами
  Pass1:=world.AddNewContainer;
  ObjCol:=Pass1.Collection;
  Pass1.Render.Active:=true;
  Pass2:=world.AddNewContainer;
  //Задаем шейдер для второго прохода
  with Pass2.CreateShader do begin
    onApplyShader:=ApplyShader;
    CreateFromFile('Media\passthrough.vert','Media\linear_horiz.frag');
  end;
  Pass2.Render.Active:=true;
  Pass3:=world.AddNewContainer;
  //Задаем шейдер для третьего прохода
  with Pass3.CreateShader do begin
    onApplyShader:=ApplyShader;
    CreateFromFile('Media\passthrough.vert','Media\linear_vert.frag');
  end;
  Pass3.Render.Active:=true;

  //создаем текстуру для основного рендертаргета FBO
  fbotex:=TTexture.Create;
  fbotex2:=TTexture.Create;
  fbotex3:=TTexture.Create;
  //Размер можно впоследствии изменить
  fbotex.CreateRGBA8Texture2D(256,256);
  fbotex2.CreateRGBA8Texture2D(256,256);
  fbotex3.CreateRGBA8Texture2D(256,256);

  //Конфигурируем FBO контейнера, расставляем приоритеты для
  //правильной очередности обработки контейнеров,
  // (меньшее значение имеет больший приоритет)
  with Pass1 do begin
    Priority:=-3;
    Render.FBO.ConfigFBO([rbDepth]);
    Render.FBO.ConfigDepthBuffer(bmBuffer,dp16);
    Render.AttachTexture(fbotex,tgTexture);
    Render.RenderBuffer:=rtFrameBuffer;
  end;

  with Pass2 do begin
    Priority:=-2;
    Collection.AddScreenQuad.Texture:=fbotex;
    Render.AttachTexture(fbotex2,tgTexture);
    Render.RenderBuffer:=rtFrameBuffer;
  end;

  with Pass3 do begin
    Priority:=-1;
    Collection.AddScreenQuad.Texture:=fbotex2;
    Render.AttachTexture(fbotex3,tgTexture);
    Render.RenderBuffer:=rtFrameBuffer;
  end;

  //добавляем скринквад, в котором будет отображаться отрендеренна сцена
  with world.AddScreenQuad do begin
    Texture:=fbotex;
  end;

//формируем сцену, добалвяя соответствующие примитивы в коллекцию
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //Добавляем к контейнеру плоскость и смещаем ее вниз
  ObjCol.AddPlane(10,10,1,1).MoveObject(0,-2,0);
  //Для последнего объекта в контейнере (нашего плэйна) задаем двустороннее отображение
  TVBOMeshObject(ObjCol.Last).TwoSides:=true;
  TVBOMeshObject(ObjCol.Last).MaterialObject.TwoSideLighting:=true;
  //Добавим к контейнеру кубик, сместим его влево и растянем по вертикали
  with ObjCol.AddBox(1,1,1,1,1,1) do begin
    MoveObject(-2,0,0); ScaleObject(1,1.5,1);
    //Создадим новый материал для куба и сделаем его синего цвета
    MaterialObject.AddNewMaterial('Blue').Properties.DiffuseColor.SetColor(0,0,1.0,1.0);
  end;
  //Добавим к контейнеру сферу и присвоим ей нашу текстуру
  Sphere:=ObjCol.AddSphere(1,16,32);
  Sphere.MoveObject(2,0,0);
  Sphere.Texture:=tex;
  //Явно укажем первому объекту контейнера (плэйну) созданную текстуру
  TVBOMeshObject(ObjCol[0]).Texture:=tex;
  //Добавим к контейнеру сетку зеленого цвета
  color:=VectorMake(0,1,0,1);
  ObjCol.AddGrid(10,10,10,10,@Color).MoveObject(0,2,0);
  //Выведем вокруг сферы окаймляющий бокс красного цвета:
  color:=VectorMake(1,0,0,1);
  ObjCol.AddBBox(Sphere.Extents,color);

  //Добавим к контейнеру фриформу и загрузим в нее модельку из obj
  with ObjCol.AddMeshFromFile('Media\Column.obj') do begin
    h:=Extents.emax[1]-Extents.emin[1];
    ScaleObject(1/400,1/h*4,1/400);
    MoveObject(0,-2,-3);
  end;

  //Добавим к контейнеру фриформу и загрузим в нее модельку из 3ds
  with ObjCol.AddMeshFromFile('Media\Column.3ds') do begin
    Name:='3DS Mesh';
    h:=Extents.emax[1]-Extents.emin[1];
    ScaleObject(1/400,1/h*4,1/400);
    MoveObject(0,-2,3);
  end;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

end.
