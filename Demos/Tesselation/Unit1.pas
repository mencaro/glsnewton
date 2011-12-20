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
    Button1: TButton;
    CheckBox1: TCheckBox;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  world: TVBOMesh;
  tex: TTexture;
  box: TVBOMeshObject;
implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  box.Tesselate(1);
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then box.FaceMode:=fmFill
  else box.FaceMode:=fmLines;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  //Активируем контекст OGL и делаем сохраняем состояния переменных OGL
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  OGLStateEmul.GLStateCache.CheckStates;

  //Добавляем к сцене контейнер с примитивами
  world:=TVBOMesh.CreateAsChild(GLScene1.Objects);

  tex:=TTexture.CreateFromFile('Media\Tile.bmp');

  //Добавим к контейнеру кубик
  box:=world.AddBox(1,1,1,1,1,1);
//  box:=world.AddPlane(2,2,2,2);
//  box:=world.AddSphere(1,15,32);
//  box.TwoSides:=true;
  box.Texture:=tex;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

end.
