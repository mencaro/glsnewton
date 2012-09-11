unit Unit1;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms,
  Dialogs, GLCadencer, GLScene, GLObjects, GLCoordinates,
  GLWin32Viewer, GLCrossPlatform, BaseClasses, Vectorgeometry,
  StdCtrls, ExtCtrls, GLSimpleNavigation,
  //VBOMesh libs:
  VBOMesh, uMeshObjects, uTextures, uFBO, OGLStateEmul, ComCtrls;
type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLLightSource1: TGLLightSource;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLSimpleNavigation1: TGLSimpleNavigation;
    Panel1: TPanel;
    CheckBox1: TCheckBox;
    TrackBar1: TTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  world: TVBOMesh;
  tex,fbotex: TTexture;
  ObjCol: TMeshCollection;
  Pass1: TMeshContainer;
  Size: integer = 256;
implementation

{$R *.dfm}

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  Pass1.Render.ViewerToTextureSize:=not CheckBox1.Checked;
  if not CheckBox1.Checked then
    fbotex.SetDimensions(size,size);
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
  ObjCol:=Pass1.Collection; //ссылка на коллекцию объектов
  Pass1.Render.Active:=true; //активируем рендер контейнера

  //создаем текстуру для основного рендертаргета FBO
  fbotex:=TTexture.Create;
  //Размер можно впоследствии изменить
  fbotex.CreateRGBA8Texture2D(256,256);

  //Конфигурируем FBO контейнера
  Pass1.Render.FBO.ConfigFBO([rbDepth]);
  Pass1.Render.FBO.ConfigDepthBuffer(bmBuffer,dp32);
  //прикрепляем нашу текстуру на основной рендертаргет
  Pass1.Render.AttachTexture(fbotex,tgTexture);
  //указываем что рендеринг должен осуществляться в текстуру
  Pass1.Render.RenderBuffer:=rtFrameBuffer;
  //указываем что вьювер должен подгоняться под размер текстуры
  Pass1.Render.ViewerToTextureSize:=true;

  //добавляем скринквад, в котором будет отображаться отрендеренна сцена
  world.AddScreenQuad.Texture:=fbotex;

//формируем сцену, добалвяя соответствующие примитивы в коллекцию
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //Добавляем к контейнеру плоскость и смещаем ее вниз
  ObjCol.AddPlane(10,10,1,1).MoveObject(0,-2,0);
  //Для последнего объекта в контейнере (нашего плэйна) задаем двустороннее отображение
  TVBOMeshObject(ObjCol.Last).TwoSides:=true;
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
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  fbotex.SetDimensions(size,size);
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
const tsize: array[0..5] of integer = (128,256,512,1024,2048,4096);
begin
  size:=tsize[TrackBar1.Position];
  label2.Caption:=inttostr(size);
end;

end.
