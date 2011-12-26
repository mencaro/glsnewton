unit Unit1;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms,
  Dialogs, GLCadencer, GLScene, GLObjects, GLCoordinates,
  GLWin32Viewer, GLCrossPlatform, BaseClasses, Vectorgeometry,
  StdCtrls, ExtCtrls, GLSimpleNavigation,
  //VBOMesh libs:
  VBOMesh, uVBO, uMeshObjects, uTextures, OGLStateEmul;
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
    { Public declarations }
  end;

var
  Form1: TForm1;
  world: TVBOMesh;
  tex: TTexture;
  usr: TVBOMeshObject;
  Buff: PVBOBuffer;
  IntBuff: TInterleavedBuffer;
implementation

{$R *.dfm}

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  GLCadencer1.Enabled:=false;
  world.Free; tex.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
var vi,ni,ti,ci: integer;
begin
  //Активируем контекст OGL и сохраняем состояния переменных OGL
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  OGLStateEmul.GLStateCache.CheckStates;

  //Добавляем к сцене контейнер с примитивами
  world:=TVBOMesh.CreateAsChild(GLScene1.Objects);

  tex:=TTexture.CreateFromFile('Media\Tile.bmp');
  tex.TextureMode:=tcModulate;

  //Создаем и инициализируем буфер VBO
  new(Buff); InitVBOBuff(buff^,4,DrawElements);
  IntBuff:=TInterleavedBuffer.Create;
  //Создаем дескрипторы вершинных атрибутов
  vi:=IntBuff.CreateDescriptor(3,atVertex);
  ni:=IntBuff.CreateDescriptor(3,atNormal);
  ti:=IntBuff.CreateDescriptor(2,atTexCoord);
  ci:=IntBuff.CreateDescriptor(4,atColor);
  //Добавляем вершину и устанавливаем значения ее атрибутов
  IntBuff.AddVertex;
  IntBuff.AttributeValue(vi,-1,1,0);
  IntBuff.AttributeValue(ni,0,0,1);
  IntBuff.AttributeValue(ti,0,1);
  IntBuff.AttributeValue(ci,1,0,0,1);

  IntBuff.AddVertex;
  IntBuff.AttributeValue(vi,-1,-1,0);
  IntBuff.AttributeValue(ni,0,0,1);
  IntBuff.AttributeValue(ti,0,0);
  IntBuff.AttributeValue(ci,0,1,0,1);

  IntBuff.AddVertex;
  IntBuff.AttributeValue(vi,1,1,0);
  IntBuff.AttributeValue(ni,0,0,1);
  IntBuff.AttributeValue(ti,1,1);
  IntBuff.AttributeValue(ci,0,0,1,1);

  IntBuff.AddVertex;
  IntBuff.AttributeValue(vi,1,-1,0);
  IntBuff.AttributeValue(ni,0,0,1);
  IntBuff.AttributeValue(ti,1,0);
  IntBuff.AttributeValue(ci,1,1,0,1);

  //Добавляем индексы
  buff.Indices.Add(0,1,2);
  buff.Indices.Add(2,1,3);
  //Генерируем полученный буфер VBO
  IntBuff.BuildVBO(buff^);
  //Добавляем на сцену созданный объект и прикрепляем к нему текстуру
  usr:=world.AddUserObject('Plane',Buff);
  usr.Texture:=tex;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

end.
