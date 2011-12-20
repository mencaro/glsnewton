unit Unit1;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms,
  Dialogs, GLCadencer, GLScene, GLObjects, GLCoordinates,
  GLWin32Viewer, GLCrossPlatform, BaseClasses, Vectorgeometry,
  StdCtrls, ExtCtrls, GLSimpleNavigation,
  //VBOMesh libs:
  VBOMesh, uMeshObjects, uTextures, OGLStateEmul;
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
  end;

var
  Form1: TForm1;
  world: TVBOMesh;
  tex: TTexture;
implementation

{$R *.dfm}

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

  //Добавляем к сцене контейнер с примитивами
  world:=TVBOMesh.CreateAsChild(GLScene1.Objects);

  //Добавляем к контейнеру плоскость и смещаем ее вниз
  world.AddPlane(10,10,1,1).MoveObject(0,-2,0);
  //Для последнего объекта в контейнере (нашего плэйна) задаем двустороннее отображение
  world.Last.TwoSides:=true;
  //Добавим к контейнеру кубик, сместим его влево и растянем по вертикали
  with world.AddBox(1,1,1,1,1,1) do begin
    MoveObject(-2,0,0); ScaleObject(1,1.5,1);
    //Создадим новый материал для куба и сделаем его синего цвета
    MaterialObject.AddNewMaterial('Blue').Properties.DiffuseColor.SetColor(0,0,1.0,1.0);
  end;
  //Добавим к контейнеру сферу и присвоим ей нашу текстуру
  Sphere:=world.AddSphere(1,16,32);
  Sphere.MoveObject(2,0,0);
  Sphere.Texture:=tex;
  //Явно укажем первому объекту контейнера (плэйну) созданную текстуру
  World[0].Texture:=tex;
  //Добавим к контейнеру сетку зеленого цвета
  color:=VectorMake(0,1,0,1);
  World.AddGrid(10,10,10,10,@Color).MoveObject(0,2,0);
  //Выведем вокруг сферы окаймляющий бокс красного цвета:
  color:=VectorMake(1,0,0,1);
  World.AddBBox(Sphere.Extents,color);

  //Добавим к контейнеру фриформу и загрузим в нее модельку из obj
  with World.AddMeshFromFile('Media\Column.obj') do begin
    h:=Extents.emax[1]-Extents.emin[1];
    ScaleObject(1/400,1/h*4,1/400);
    MoveObject(0,-2,-3);
  end;

  //Добавим к контейнеру фриформу и загрузим в нее модельку из 3ds
  with World.AddMeshFromFile('Media\Column.3ds') do begin
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
