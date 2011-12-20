unit Unit1;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms,
  Dialogs, GLCadencer, GLScene, GLObjects, GLCoordinates,
  GLWin32Viewer, GLCrossPlatform, BaseClasses, Vectorgeometry,
  GLGeomObjects, StdCtrls, ExtCtrls, GLSimpleNavigation,
  //VBOMesh libs:
  VBOMesh, uVBO, uMeshObjects, OGLStateEmul;
type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLLightSource1: TGLLightSource;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLSimpleNavigation1: TGLSimpleNavigation;
    CheckBox1: TCheckBox;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  world: TVBOMesh;
  box: TVBOMeshObject;

implementation

{$R *.dfm}

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then box.FaceMode:=fmFill
  else box.FaceMode:=fmLines;
end;

procedure TForm1.FormCreate(Sender: TObject);
var buff: PVBOBuffer;
    i,j: integer;
    vert: TVBOMeshObject;
begin
  //Активируем контекст OGL и делаем сохраняем состояния переменных OGL
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  OGLStateEmul.GLStateCache.CheckStates;

  //Добавляем к сцене контейнер с примитивами
  world:=TVBOMesh.CreateAsChild(GLScene1.Objects);
  world.Lights.AddNewLight.ImportLight(0);
  world.Lights[0].Enabled:=true;
  //Добавляем на сцену кубик
  box:=world.AddBox(1,1,1,10,10,10);
  //Создаем маркер вершины
  vert:=world.AddBox(0.015,0.015,0.015,1,1,1);
  with vert do begin
    FriendlyName:='Vertex Marker';
    MaterialObject.AddNewMaterial('VColor');
    Material.Properties.DiffuseColor.SetColor(105,178,255,255);
    //Привязываем маркер вершины к объекту
    Parent:=box; visible:=false;
  end;

  //Перебираем все меши объекта
  for i:=0 to box.MeshList.count-1 do begin
    buff:=box.MeshList[i];
    //перебираем все вершины текущего меша
    for j:=0 to buff.Vertexes.Count-1 do begin
      //Созадем кубик в положении каждой вершины
      with world.AddInstanceToObject(vert) do begin
        MoveObject(vectormake(buff.Vertexes[j]));
        Parent:=box;
      end;
    end;
  end;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  if assigned(box) then box.MoveObject(sin(newTime),cos(newTime),sin(newTime));
  if assigned(world) then world.Lights[0].Position:=GLLightSource1.AbsolutePosition;

  GLSceneViewer1.Invalidate;
end;

end.
