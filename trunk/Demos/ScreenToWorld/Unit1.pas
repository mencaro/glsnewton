unit Unit1;

interface

uses
  Windows, Messages, SysUtilsLite, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLSimpleNavigation, GLCadencer, GLWin32Viewer, vboMesh, VectorGeometry,
  GLScene, GLObjects, GLCoordinates, GLCrossPlatform, BaseClasses, OGLStateEmul;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLDummyCube1: TGLDummyCube;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLSimpleNavigation1: TGLSimpleNavigation;
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  world: TVBOMesh;
implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var color: TVector;
begin
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  OGLStateEmul.GLStateCache.CheckStates;
  world:=TVBOMesh.CreateAsChild(GLScene1.Objects);
  //Создаем сетку зеленого цвета
  color:=vectormake(0,1,0,1);
  world.AddGrid(10,10,80,80,@color);
  //Повернем ее на 90 градусов, чтоб она оказалась в плоскости экрана
  world.Last.RotateAroundX(-pi/2);
  //Добавим на сцену шарик, который будет указывать положение клика
  world.AddSphere(0.5,16,32).visible:=false;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var pos, dir: TVector;
    d: single;
begin
  if not (ssLeft in shift) then exit;
  //Получаем точку пересечения на ближней плоскости отсечения и
  //направление вектора "вглубь" экрана
  World.ScreenToWorld(X,Y,@Pos,@Dir);
  //Зная начальную точку и направление - получаем точку
  //пересечения на заданной плоскости (на сетке с Z=0)
  d:=-Pos[2]/dir[2];
  CombineVector(pos,dir,d);
  //Помещаем шарик в найденную точку пеерсечения и делаем его видимым
  world[1].Position:=pos;
  world[1].Visible:=true;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

end.
