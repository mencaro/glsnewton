unit Unit1;

interface

uses
  Windows, Messages, SysUtilsLite, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLSimpleNavigation, GLCadencer, GLWin32Viewer, VectorGeometry,
  GLScene, GLObjects, GLCoordinates, GLCrossPlatform, BaseClasses, StdCtrls,
  //VBOMesh Libs
  vboMesh, uTextures, uMeshObjects, uMaterialObjects, OGLStateEmul;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLDummyCube1: TGLDummyCube;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLSimpleNavigation1: TGLSimpleNavigation;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure RadioButton1Click(Sender: TObject);
    procedure RadioButton2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  world: TVBOMesh;
  Beam: TTexture;
  Glow: TTexture;
  VL: TVolumetricLines;
implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  OGLStateEmul.GLStateCache.CheckStates;
  world:=TVBOMesh.CreateAsChild(GLScene1.Objects);
  Beam:=TTexture.CreateFromFile('Media\red_laser.bmp');
  Glow:=TTexture.CreateFromFile('Media\blue_glow.bmp');;

  VL:=TVolumetricLines.Create;
  VL.Texture:=Beam;
  vl.Blending:=bmAdditive;

  VL.AddNode(-1,1,0);
  VL.AddNode( 1,1,0);
  VL.BreakLine;
  VL.AddNode(-1,0,0);
  VL.AddNode( 1,0,0);
  VL.BreakLine;
  VL.AddNode(-1,-1,0);
  VL.AddNode( 1,-1,0);
  VL.BreakLine;


  VL.AddNode(-2,-2,0);
  VL.AddNode( 2,-2,0);
  VL.AddNode( 2, 2,0);
  VL.AddNode(-2, 2,0);
  VL.AddNode(-2,-2,0);

  VL.LineWidth:=0.2;
  VL.Visible:=true;
  world.AddMeshObject(VL);
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
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

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  world.Free; Beam.Free; Glow.Free;
end;

end.
