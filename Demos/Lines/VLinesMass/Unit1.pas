unit Unit1;

interface

uses
  Windows, Messages, SysUtilsLite, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLSimpleNavigation, GLCadencer, GLWin32Viewer, VectorGeometry,
  GLScene, GLObjects, GLCoordinates, GLCrossPlatform, BaseClasses, StdCtrls,
  //VBOMesh Libs
  vboMesh, uTextures, uMeshObjects, uMaterialObjects, OGLStateEmul, ExtCtrls,
  ComCtrls;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLDummyCube1: TGLDummyCube;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLSimpleNavigation1: TGLSimpleNavigation;
    Timer1: TTimer;
    Panel2: TPanel;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    TrackBar2: TTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    LifeTime: TLabel;
    Label3: TLabel;
    TrackBar3: TTrackBar;
    Label4: TLabel;
    TrackBar1: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure RadioButton1Click(Sender: TObject);
    procedure RadioButton2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TBeam = record
    p: TVector;
    st: double;
    speed: double;
  end;
  PBeam = ^TBeam;

var
  Form1: TForm1;
  world: TVBOMesh;
  Beam: TTexture;
  Glow: TTexture;
  VL: TVolumetricLines;
  Beams: TList;
implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  OGLStateEmul.GLStateCache.CheckStates;
  world:=TVBOMesh.CreateAsChild(GLScene1.Objects);
  world.OldRender:=false;
  Beam:=TTexture.CreateFromFile('Media\red_laser.bmp');
  Glow:=TTexture.CreateFromFile('Media\blue_glow.bmp');;

  Beams:=TList.Create;
  Beams.Capacity:=1000;

  VL:=TVolumetricLines.Create(5000);
  VL.Texture:=Beam;
  vl.Blending:=bmAdditive;

  VL.LineWidth:=0.02;
  VL.Visible:=true;
  world.AddMeshObject(VL);
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
var i: integer;
    p: PBeam;
begin
  for i:=0 to Beams.Count-1 do begin
    p:=Beams[i];
    p.p[0]:=p.p[0]+p.speed*deltatime*0.1;
  end;
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.RadioButton1Click(Sender: TObject);
begin
  VL.Texture:=Beam;
end;

procedure TForm1.RadioButton2Click(Sender: TObject);
begin
  VL.Texture:=Glow;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var i: integer;
    p: PBeam;
    t: double;
    s,lt,l: single;
begin
  VL.Clear; t:=GetTime;
  s:=TrackBar3.Position;
  lt:=TrackBar2.Position/10;
  l:=TrackBar1.Position/100;
  for i:=0 to Beams.Count-1 do begin
    p:=Beams[i];
    if t-p.st>lt then begin
      dispose(p); Beams[i]:=nil;
    end;
  end; Beams.Pack;
  for i:=0 to 3 do begin
    new(p); p.st:=t;
    p.p:=VectorMake(-3,(random(500)-250)/200,(random(500)-250)/200);
    p.speed:=s; Beams.Add(p);
  end;
  for i:=0 to Beams.Count-1 do begin
    p:=Beams[i];
    VL.AddNode(p.p[0],p.p[1],p.p[2]);
    VL.AddNode(p.p[0]+l,p.p[1],p.p[2]);
    VL.BreakLine;
  end;
  Label2.Caption:=inttostr(Beams.Count);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Timer1.Enabled:=false; GLCadencer1.Enabled:=false;
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  world.Free; Beam.Free; Glow.Free; FreeList(Beams);
end;

end.
