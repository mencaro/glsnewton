unit Unit1;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms, ComCtrls,
  Dialogs, GLCadencer, GLScene, GLObjects, GLCoordinates,
  GLWin32Viewer, GLCrossPlatform, BaseClasses, Vectorgeometry,
  StdCtrls, ExtCtrls, GLSimpleNavigation, OpenGL1x,
  //VBOMesh libs:
  VBOMesh, uMeshObjects, uTextures, uFBO, uVBO, uShaders, uCamera, uVectorLists,
  OGLStateEmul;
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
    procedure ApplyShader(ShaderProgram: TShaderProgram);
    procedure SetupCameras;
    { Public declarations }
  end;

var
  Form1: TForm1;
  world: TVBOMesh;
  cam,lcam: TCameraController;

  tex: TTexture;
  fbotex,fbotex2,fbotex3,depthtex, dpt2: TTexture;
  ObjCol: TMeshCollection;
  Pass1,Pass2: TMeshContainer;
  DVShader: TShaderProgram;
implementation

{$R *.dfm}

procedure TForm1.ApplyShader(ShaderProgram: TShaderProgram);
begin
  ShaderProgram.SetUniforms('image',0);
  ShaderProgram.SetUniforms('zNear',0.1);
  ShaderProgram.SetUniforms('zFar',50.0);
end;


procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  GLCadencer1.Enabled:=false;
  tex.Free; fbotex.Free; world.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
var Sphere: TVBOMeshObject;
    color: TVector;
    h: single;
begin
  //���������� �������� OGL � ������ ��������� ��������� ���������� OGL
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  OGLStateEmul.GLStateCache.CheckStates;

  //������� �������� �� �����
  tex:=TTexture.CreateFromFile('Media\tile.bmp');

  //��������� � ����� �������� ������
  world:=TVBOMesh.CreateAsChild(GLScene1.Objects);
  world.OldRender:=false;

  cam:=TCameraController.Create;
  lcam:=TCameraController.Create;

  //��������� � ����� ��������� � �����������
  Pass1:=world.AddNewContainer;
  ObjCol:=Pass1.Collection;
  Pass2:=world.AddNewContainer;

  //������� �������� ��� ��������� ������������� FBO
  fbotex:=TTexture.Create;
  fbotex2:=TTexture.Create;
  depthtex:=TTexture.Create;
  dpt2:=TTexture.Create;

  fbotex.CreateRGBA8Texture2D(256,256);
  fbotex2.CreateRGBA8Texture2D(256,256);
  depthtex.CreateDepth24FTexture2D(256,256);
  //DepthTex.CompareMode:=true;
  dpt2.CreateDepth24FTexture2D(256,256);


  //������������� FBO ����������, ����������� ��������� ���
  //���������� ����������� ��������� ������������
  // (������� �������� ����� ������� ���������)
  with Pass1 do begin
    Camera:=lcam;
    Priority:=-3;
    Render.FBO.ConfigDepthBuffer(bmTexture,dp24);
    Render.AttachTexture(depthtex,tgDepth);
    Render.RenderBuffer:=rtFrameBuffer;
    Render.Active:=true;
  end;

  with Pass2 do begin
    Camera:=cam;
    Priority:=-2;
    Render.FBO.ConfigDepthBuffer(bmTexture,dp24);
    Render.AttachTexture(fbotex2,tgTexture);
    Render.AttachTexture(dpt2,tgDepth);
    Render.RenderBuffer:=rtFrameBuffer;
    Render.Active:=true;
  end;

  //��������� ���������, � ������� ����� ������������ ������������ �����
  with world.AddScreenQuad do begin
    Texture:=fbotex2; //�������� ����� � ������� �������� ������
  end;

  with world.AddHUDSprite(0.5,0.5) do begin
    Texture:=dpt2; //�������� ������� ����� � ������� �������� ������
    MoveObject(-0.75,+0.75,0);
  end;

  DVShader:=TShaderProgram.Create;
  DVShader.CreateFromFile('Media\passthrough.vert','Media\DepthView.frag');
  DVShader.onApplyShader:=ApplyShader;
  with world.AddHUDSprite(0.5,0.5) do begin
    Texture:=dpt2; //�������������� ������� ����� � �������� ������
    Shader:=DVShader;
    MoveObject(-0.75,+0.25,0);
  end;

  with world.AddHUDSprite(0.5,0.5) do begin
    Texture:=depthtex; //�������� ������� ����� � ������� ��������� �����
    MoveObject(-0.75,-0.25,0);
  end;


//��������� �����, �������� ��������������� ��������� � ���������
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //��������� � ���������� ��������� � ������� �� ����
  ObjCol.AddPlane(10,10,1,1).MoveObject(0,-2,0);
  //��� ���������� ������� � ���������� (������ ������) ������ ������������ �����������
  TVBOMeshObject(ObjCol.Last).TwoSides:=true;
  TVBOMeshObject(ObjCol.Last).MaterialObject.TwoSideLighting:=true;
  //������� � ���������� �����, ������� ��� ����� � �������� �� ���������
  with ObjCol.AddBox(1,1,1,1,1,1) do begin
    MoveObject(-2,0,0); ScaleObject(1,1.5,1);
    //�������� ����� �������� ��� ���� � ������� ��� ������ �����
    MaterialObject.AddNewMaterial('Blue').Properties.DiffuseColor.SetColor(0,0,1.0,1.0);
  end;
  //������� � ���������� ����� � �������� �� ���� ��������
  Sphere:=ObjCol.AddSphere(1,16,32);
  Sphere.MoveObject(2,0,0);
  Sphere.Texture:=tex;
  //���� ������ ������� ������� ���������� (������) ��������� ��������
  TVBOMeshObject(ObjCol[0]).Texture:=tex;
  //������� � ���������� ����� �������� �����
  color:=VectorMake(0,1,0,1);
  ObjCol.AddGrid(10,10,10,10,@Color).MoveObject(0,2,0);
  //������� ������ ����� ����������� ���� �������� �����:
  color:=VectorMake(1,0,0,1);
  ObjCol.AddBBox(Sphere.Extents,color);

  //������� � ���������� �������� � �������� � ��� �������� �� obj
  with ObjCol.AddMeshFromFile('Media\Column.obj') do begin
    h:=Extents.emax[1]-Extents.emin[1];
    ScaleObject(1/400,1/h*4,1/400);
    MoveObject(0,-2,-3);
  end;

  //������� � ���������� �������� � �������� � ��� �������� �� 3ds
  with ObjCol.AddMeshFromFile('Media\Column.3ds') do begin
    Name:='3DS Mesh';
    h:=Extents.emax[1]-Extents.emin[1];
    ScaleObject(1/400,1/h*4,1/400);
    MoveObject(0,-2,3);
  end;

  Pass2.Collection.AddCollection(ObjCol,true);
  SetupCameras;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
var v: tvector;
begin
  GLLightSource1.MoveObjectAround(GLDummyCube1,0,deltatime*100);
  if assigned(world) then SetupCameras;
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.SetupCameras;
var v: TVector;
    ext: TExtents;
    sizes: TAffineVector;
    a: single;
begin
  a:=GLSceneViewer1.Width/GLSceneViewer1.Height;
  v:=GLLightSource1.Position.AsVector; NegateVector(v);
  GLLightSource1.SpotDirection.AsVector:=v;

  cam.Position:=GLCamera1.AbsolutePosition;
  cam.PointTo(vectormake(0,0,0,0));
  cam.SetPerspective(60,a,0.1,50);
  cam.WorldMatrixUpdated:=false;

  lcam.Position:=v;
  lcam.PointTo(vectormake(0,0,0,0));
  lcam.SetOrtogonal(15,15/a,0.1,50);
  lcam.WorldMatrixUpdated:=false;
end;

end.
