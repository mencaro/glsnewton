unit Unit1;

interface

uses
  Windows, Messages, SysUtilsLite, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uCamera, GLCadencer, GLScene, GLObjects, GLCoordinates,
  GLWin32Viewer, GLCrossPlatform, BaseClasses, GLKeyboard, Vectorgeometry,
  GLGeomObjects;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCube1: TGLCube;
    GLCadencer1: TGLCadencer;
    GLLightSource1: TGLLightSource;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLDummyCube2: TGLDummyCube;
    GLArrowLine1: TGLArrowLine;
    GLPlane1: TGLPlane;
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
  cam: TCameraController;
implementation

{$R *.dfm}

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
var Speed: single;
begin
  GLSceneViewer1.Invalidate;
  speed:=0.01;
  if IsKeyDown(VK_Shift) then Speed:=Speed*10;
  if IsKeyDown(VK_Left) then begin
     cam.TurnLeft(Speed*pi/180);
  end;
  if IsKeyDown(VK_Right) then begin
     cam.TurnLeft(-Speed*pi/180);
  end;
  if IsKeyDown(VK_Up) then begin
     cam.LookUp(-Speed*pi/180);
  end;
  if IsKeyDown(VK_Down) then begin
     cam.LookUp(Speed*pi/180);
  end;

  if IsKeyDown('W') then cam.MoveForward2D(Speed/10);
  if IsKeyDown('S') then cam.MoveForward2D(-Speed/10);
  if IsKeyDown('A') then cam.MoveLeft2D(Speed/10);
  if IsKeyDown('D') then cam.MoveLeft2D(-Speed/10);

  glcamera1.Matrix:=cam.Matrices.WorldMatrix;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  cam:=TCameraController.Create;
  cam.Position:=glcamera1.Position.AsVector;
end;

end.
