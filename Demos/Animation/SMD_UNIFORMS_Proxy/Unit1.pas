unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls,
  ExtCtrls, Dialogs, GLScene, GLObjects, GLCoordinates, GLCadencer,
  GLWin32Viewer, GLCrossPlatform, BaseClasses, StdCtrls, GLSimpleNavigation,
  OpenGL1x, vboMesh, uMeshObjects, uFileSMD,  OGLStateEmul;

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
    TrackBar1: TTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    TrackBar2: TTrackBar;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TrackBar2Change(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  mesh: TVBOMesh;
  frame: single=0;
  fspeed: single=1;
  ffreq: single=1/25;
  oldframe: single=-1;
  smd: TUniformSMDRender;
  dt: double=0;
implementation

{$R *.dfm}

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
var i:integer;
begin
  if not assigned(Mesh) then exit;

  if dt>ffreq then begin
     frame:=frac(newTime)*(smd.FramesCount);
     if abs(trunc(oldframe-frame))>0 then begin
        for i:=0 to mesh.Count-1 do begin
          if Mesh[i] is TUniformSMDRender then begin
             smd:=Mesh[i] as TUniformSMDRender;
             if assigned(smd) then smd.NextFrame(fspeed);
                //smd.FramePos:=frame;
          end;
        end;
        oldframe:=trunc(frame);
     end;
     dt:=0;
  end else dt:=dt+deltaTime;
  label10.Caption:=inttostr(Mesh.PolygonsCount);
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.FormCreate(Sender: TObject);
var i,j: integer;
begin
GLSceneViewer1.Buffer.RenderingContext.Activate;
OGLStateEmul.GLStateCache.CheckStates;

  Mesh:=TVBOMesh.CreateAsChild(GLScene1.Objects);
  smd:=Mesh.AddUniformSMD('Media\leet.smd',['Media\lrun.smd']);
  with smd do begin
    AnimationNum:=0; FramePos:=random(FramesCount);
    ScaleObject(0.1,0.1,0.1);
    visible:=false;
    TrackBar1.Max:=smd.FramesCount*2;
    TrackBar1.Position:=smd.FramesCount;
    label6.Caption:=inttostr(smd.FramesCount);
  end;
  for i:=0 to 9 do for j:=0 to 9 do
  with Mesh.AddUniformSMD(smd) do begin
    AnimationNum:=0; FramePos:=random(FramesCount);
    ScaleObject(0.1,0.1,0.1);
    MoveObject(i*3-15,0,j*3-15);
    Smoothed:=true;
    visible:=true;
  end;
  label8.Caption:=inttostr(mesh.Count-1);

end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  GLCadencer1.Enabled:=false;
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  Mesh.Free;
end;


procedure TForm1.TrackBar2Change(Sender: TObject);
begin
  fspeed:=0.25+TrackBar2.Position*0.25;
  label4.Caption:=floattostr(fspeed);
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  label1.Caption:=inttostr(TrackBar1.Position);
  ffreq:=1/TrackBar1.Position;
end;

end.

