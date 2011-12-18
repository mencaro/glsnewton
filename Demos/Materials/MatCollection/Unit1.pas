unit Unit1;

interface

uses
  Windows, Messages, SysUtilsLite, Classes, Controls, Forms,
  GLCadencer, GLScene, GLObjects, GLCoordinates, GLWin32Viewer, GLCrossPlatform,
  BaseClasses, Vectorgeometry, GLSimpleNavigation, OpenGL1x,
  VBOMesh, uMaterials, uTextLayer, uMeshObjects, OGLStateEmul;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLLightSource1: TGLLightSource;
    GLCamera1: TGLCamera;
    GLSimpleNavigation1: TGLSimpleNavigation;
    GLDummyCube1: TGLDummyCube;
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
  world: TVBOMesh;
  mcol: TMaterialsCollection;
  matList: TList;
  tr: TVBOTextRect;
  F: TTextureFont;
implementation

{$R *.dfm}

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.FormCreate(Sender: TObject);
var i,j,n: integer;
    mat: TMaterial;
    v,p: TVector;
    x: single;
    msp: TVBOMeshObject;
begin
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  OGLStateEmul.GLStateCache.CheckStates;

  world:=TVBOMesh.CreateAsChild(glscene1.Objects);
  mcol:=TMaterialsCollection.Create;
  mcol.LoadCollection(extractfilepath(Application.ExeName)+'Materials.ini');
  matList:=TList.Create;
  //Showmessage(mcol.MaterialList);
  n:=0;
  msp:=world.AddSphere(0.18,64,128); msp.Visible:=false;
  for i:=0 to 3 do for j:=0 to 4 do begin
    with world.AddProxyObject(msp) do begin
      MoveObject((j-2)*0.4,(i-1.5)*0.4,0);
      if n<mcol.count then begin
        mat:=TMaterial.Create;
        matList.Add(mat); mat.Name:=mcol[n].Name;
        mat.Properties.Assign(mcol[n]);
        Material:=mat; inc(n);
      end;
    end;
  end;
  n:=0; World.UpdateSceneMatrix;

  F:=TTextureFont.Create;
  F.BuildTexture('Times New Roman',10);
  for i:=0 to 3 do for j:=0 to 4 do begin
      if n<mcol.count then begin
        tr:=TVBOTextRect.Create;
        tr.HUD:=false; tr.Text(mcol[n].Name);
        tr.Font.Assign(F);
        p:=world.WorldToPlane(v);
        x:=0.02*length(mcol[n].Name);
        tr.Position:=vectormake((j-2)*0.4-x/2,((i-1.5)*0.4-0.04),0);
        tr.ScaleObject(x,0.08,1,true);
        tr.AlphaTest:=0.1; tr.Alpha:=0.999;
        tr.BlendSourceFunc:=GL_SRC_ALPHA;
        tr.BlendDestFunc:=GL_ONE_MINUS_SRC_ALPHA;
        tr.Visible:=true;
        world.AddMeshObject(tr);
        inc(n);
      end;
  end;
end;

end.
