unit Unit1;

interface

uses
  Windows, Messages, SysUtilsLite, Classes, Controls, Forms, ComCtrls, ExtCtrls,
  Dialogs, GLScene, GLCadencer, GLWin32Viewer, GLCoordinates, GLObjects,
  GLCrossPlatform, BaseClasses, StdCtrls, OpenGL1x, GLSimpleNavigation,
  vboMesh, uShaders, uMeshObjects, uFileSMD, OGLStateEmul;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLDummyCube1: TGLDummyCube;
    GLSimpleNavigation1: TGLSimpleNavigation;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    ComboBox1: TComboBox;
    TrackBar1: TTrackBar;
    ComboBox2: TComboBox;
    TrackBar2: TTrackBar;
    TrackBar3: TTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  mesh:TVBOMesh;
  frame: single=0;
  fspeed: single=1;
  ffreq: single=1/25;
  oldframe: single=-1;
  smd,fleft,fright: TUniformSMDRender;
  dt: double=0;

implementation

{$R *.dfm}

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
var i,a1,a2: integer;
    f1,f2,bf: single;
    fc: integer;
begin
 smd.BlendAnimation(FLeft.AnimationNum,FRight.AnimationNum,
   FLeft.FramePos,FRight.FramePos,TrackBar1.Position/100);
 GLSceneViewer1.Invalidate;
end;

procedure TForm1.TrackBar2Change(Sender: TObject);
begin
  FLeft.FramePos:=TrackBar2.Position/100*(FLeft.FramesCount-1);
end;

procedure TForm1.TrackBar3Change(Sender: TObject);
begin
  FRight.FramePos:=TrackBar3.Position/100*(FRight.FramesCount-1);
end;

procedure TForm1.FormCreate(Sender: TObject);
var i,j: integer;
begin
GLSceneViewer1.Buffer.RenderingContext.Activate;
OGLStateEmul.GLStateCache.CheckStates;

  Mesh:=TVBOMesh.CreateAsChild(GLScene1.Objects);
  smd:=Mesh.AddUniformSMD('Media\TRINITYrage.smd',
    ['Media\walk.smd','Media\run.smd','Media\jump.smd','Media\long_jump.smd',
     'Media\look_left_right.smd']);
  fleft:=Mesh.AddUniformSMD('Media\TRINITYrage.smd',
    ['Media\walk.smd','Media\run.smd','Media\jump.smd','Media\long_jump.smd',
     'Media\look_left_right.smd']);
  fright:=Mesh.AddUniformSMD('Media\TRINITYrage.smd',
    ['Media\walk.smd','Media\run.smd','Media\jump.smd','Media\long_jump.smd',
     'Media\look_left_right.smd']);
  with smd do begin
    AnimationNum:=0; FramePos:=0;
    ScaleObject(0.1,0.1,0.1);
    RotateAroundZ(pi/2);
    RotateAroundX(-pi/2,false);
    visible:=true;
  end;
  with fleft do begin
    AnimationNum:=0; FramePos:=0;
    ScaleObject(0.1,0.1,0.1);
    RotateAroundZ(pi/2);
    RotateAroundX(-pi/2,false);
    Smoothed:=true;
    MoveObject(-9,0,0);
    visible:=true;
  end;
  with fright do begin
    AnimationNum:=0; FramePos:=0;
    ScaleObject(0.1,0.1,0.1);
    RotateAroundZ(pi/2);
    RotateAroundX(-pi/2,false);
    MoveObject(9,0,0);
    Smoothed:=true;
    visible:=true;
  end;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  if combobox1.ItemIndex<>-1 then
    FLeft.AnimationNum:=combobox1.ItemIndex;
  TrackBar2Change(nil);
end;

procedure TForm1.ComboBox2Change(Sender: TObject);
begin
  if combobox2.ItemIndex<>-1 then
    FRight.AnimationNum:=combobox2.ItemIndex;
  TrackBar3Change(nil);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  GLCadencer1.Enabled:=false;
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  Mesh.Free;

end;


end.

