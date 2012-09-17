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
    procedure ApplyShadowShader(ShaderProgram: TShaderProgram);
    procedure UnApplyShadowShader(ShaderProgram: TShaderProgram);
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
  Pass1,Pass2,Pass3: TMeshContainer;
  DVShader: TShaderProgram;

var vp,vm,imvp: TMatrix;
    vpb: TMatrix;
    bias: TMatrix;
implementation

{$R *.dfm}

procedure TForm1.ApplyShader(ShaderProgram: TShaderProgram);
begin
  ShaderProgram.SetUniforms('image',0);
end;

procedure TForm1.ApplyShadowShader(ShaderProgram: TShaderProgram);
begin
  fbotex2.Apply(0); depthtex.Apply(1); dpt2.Apply(2);

  ShaderProgram.SetUniforms('MainTex',0);
  ShaderProgram.SetUniforms('ShadowTex',1);
  ShaderProgram.SetUniforms('DepthTex',2);
  ShaderProgram.SetUniforms('imvp',imvp);
  ShaderProgram.SetUniforms('shadowMatrix',vpb);

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
  bias[0] := VectorMake(0.5, 0.0, 0.0, 0.0);
  bias[1] := VectorMake(0.0, 0.5, 0.0, 0.0);
  bias[2] := VectorMake(0.0, 0.0, 0.5, 0.0);
  bias[3] := VectorMake(0.5, 0.5, 0.5, 1.0);

  //Активируем контекст OGL и делаем сохраняем состояния переменных OGL
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  OGLStateEmul.GLStateCache.CheckStates;

  //Создаем текстуру из файла
  tex:=TTexture.CreateFromFile('Media\tile.bmp');

  //Добавляем к сцене основной рендер
  world:=TVBOMesh.CreateAsChild(GLScene1.Objects);
  world.OldRender:=false;

  cam:=TCameraController.Create;
  lcam:=TCameraController.Create;

  //Добавляем к сцене контейнер с примитивами
  Pass1:=world.AddNewContainer;
  ObjCol:=Pass1.Collection;
  Pass2:=world.AddNewContainer;
  Pass3:=world.AddNewContainer;

  //создаем текстуру для основного рендертаргета FBO
  fbotex:=TTexture.Create;
  fbotex2:=TTexture.Create;
  fbotex3:=TTexture.Create;
  depthtex:=TTexture.Create;
  dpt2:=TTexture.Create;

  fbotex.CreateRGBA8Texture2D(256,256);
  fbotex2.CreateRGBA8Texture2D(256,256);
  fbotex3.CreateRGBA8Texture2D(256,256);
  depthtex.CreateDepth32FTexture2D(256,256);
  //DepthTex.CompareMode:=true;
  dpt2.CreateDepth32FTexture2D(256,256);


  //Конфигурируем FBO контейнера, расставляем приориеты для
  //правильной очередности обработки контейнеровб
  // (меньшее значение имеет больший приоритет)
  with Pass1 do begin
    Camera:=lcam;
    Priority:=-3;
    Render.FBO.ConfigDepthBuffer(bmTexture,dp32);
    Render.AttachTexture(depthtex,tgDepth);
    Render.RenderBuffer:=rtFrameBuffer;
    Render.Active:=true;
  end;

  with Pass2 do begin
    Camera:=cam;
    Priority:=-2;
    Render.FBO.ConfigDepthBuffer(bmTexture,dp32);
    Render.AttachTexture(fbotex2,tgTexture);
    Render.AttachTexture(dpt2,tgDepth);
    Render.RenderBuffer:=rtFrameBuffer;
    Render.Active:=true;
  end;


  with Pass3 do begin
    Priority:=-1;
    Collection.AddScreenQuad;
    Render.AttachTexture(fbotex3,tgTexture);
    Render.RenderBuffer:=rtFrameBuffer;
    Camera:=cam;
    Render.Active:=true;
  end;
  with Pass3.CreateShader do begin
    onApplyShader:=ApplyShadowShader;
    CreateFromFile('Media\passthrough.vert','Media\ShadowPass.frag');
  end;


  //добавляем скринквад, в котором будет отображаться отрендеренна сцена
  with world.AddScreenQuad do begin
    Texture:=fbotex3;
  end;

  with world.AddHUDSprite(0.5,0.5) do begin
    Texture:=dpt2;
    MoveObject(-0.75,+0.75,0);
  end;

  DVShader:=TShaderProgram.Create;
  DVShader.CreateFromFile('Media\passthrough.vert','Media\DepthView.frag');
  DVShader.onApplyShader:=ApplyShader;
  with world.AddHUDSprite(0.5,0.5) do begin
    Texture:=depthtex;
    Shader:=DVShader;
    MoveObject(-0.75,+0.25,0);
  end;


//формируем сцену, добалвяя соответствующие примитивы в коллекцию
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //Добавляем к контейнеру плоскость и смещаем ее вниз
  ObjCol.AddPlane(10,10,1,1).MoveObject(0,-2,0);
  //Для последнего объекта в контейнере (нашего плэйна) задаем двустороннее отображение
  TVBOMeshObject(ObjCol.Last).TwoSides:=true;
  TVBOMeshObject(ObjCol.Last).MaterialObject.TwoSideLighting:=true;
  //Добавим к контейнеру кубик, сместим его влево и растянем по вертикали
  with ObjCol.AddBox(1,1,1,1,1,1) do begin
    MoveObject(-2,0,0); ScaleObject(1,1.5,1);
    //Создадим новый материал для куба и сделаем его синего цвета
    MaterialObject.AddNewMaterial('Blue').Properties.DiffuseColor.SetColor(0,0,1.0,1.0);
  end;
  //Добавим к контейнеру сферу и присвоим ей нашу текстуру
  Sphere:=ObjCol.AddSphere(1,16,32);
  Sphere.MoveObject(2,0,0);
  Sphere.Texture:=tex;
  //Явно укажем первому объекту контейнера (плэйну) созданную текстуру
  TVBOMeshObject(ObjCol[0]).Texture:=tex;
  //Добавим к контейнеру сетку зеленого цвета
  color:=VectorMake(0,1,0,1);
  ObjCol.AddGrid(10,10,10,10,@Color).MoveObject(0,2,0);
  //Выведем вокруг сферы окаймляющий бокс красного цвета:
  color:=VectorMake(1,0,0,1);
  ObjCol.AddBBox(Sphere.Extents,color);

  //Добавим к контейнеру фриформу и загрузим в нее модельку из obj
  with ObjCol.AddMeshFromFile('Media\Column.obj') do begin
    h:=Extents.emax[1]-Extents.emin[1];
    ScaleObject(1/400,1/h*4,1/400);
    MoveObject(0,-2,-3);
  end;

  //Добавим к контейнеру фриформу и загрузим в нее модельку из 3ds
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
begin
  GLLightSource1.MoveObjectAround(GLDummyCube1,0,deltatime*100);
  if assigned(world) then SetupCameras;
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.SetupCameras;
var v: TVector;
    smvp: TMatrix;
    a: single;
begin
  a:=GLSceneViewer1.Width/GLSceneViewer1.Height;
  v:=GLLightSource1.Position.AsVector; NegateVector(v);
  GLLightSource1.SpotDirection.AsVector:=v;
  vm:=cam.LookAt(GLCamera1.AbsoluteAffinePosition,affinevectormake(0,0,0));
  cam.WorldMatrixUpdated:=true;
  cam.SetPerspective(60,a,0.1,200);
  vp:=cam.ProjectionMatrix;
  imvp:=MatrixMultiply(vm,vp); InvertMatrix(imvp);

  lcam.LookAt(affineVectorMake(v),affinevectormake(0,0,0));
  lcam.SetPerspective(60,a,0.1,200);//SetOrtogonal(15,15/a,0.1,50);
  lcam.WorldMatrixUpdated:=true;

  smvp:=MatrixMultiply(lcam.ViewMatrix,lcam.ProjectionMatrix);
  vpb:=MatrixMultiply(smvp,bias);

end;

procedure TForm1.UnApplyShadowShader(ShaderProgram: TShaderProgram);
begin
  dpt2.UnApply(2); depthtex.UnApply(1); fbotex2.UnApply(0);
end;

end.


