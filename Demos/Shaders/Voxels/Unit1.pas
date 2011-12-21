unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls,
  //GLScene Units
  GLScene, GLCoordinates, GLWin32Viewer, GLCrossPlatform, GLObjects,
  BaseClasses, GLCadencer, VectorGeometry, GLSimpleNavigation, OpenGL1x,
  VectorLists,
  //VBOMesh Lib
  vboMesh, uShaders, uMeshObjects, uVBO, OGLStateEmul;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLCadencer1: TGLCadencer;
    GLSimpleNavigation1: TGLSimpleNavigation;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    GLLightSource1: TGLLightSource;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ApplyShader(mo: TObject);
    procedure unApplyShader(mo: TObject);
  end;

var
  Form1: TForm1;
  Mesh: TVBOMesh;
  Voxels: TShaderProgram;
  spId: cardinal;
  vox: PVBOBuffer;
  mo: TVBOMeshObject;
  sph: array[0..255,0..255,0..255] of byte;
  attrList: TIntegerList;
  pCounts: integer = 0;
  vCounts: integer = 0;
implementation

{$R *.dfm}

procedure FillSphere(R2: single);
var i,j,k: integer;
    rt: single;
begin
  for i:=0 to 255 do for j:=0 to 255 do for k:=0 to 255 do begin
    rt:=sqr(i-127)+sqr(j-127)+sqr(k-127);
    if rt<=r2 then sph[i,j,k]:=1 else sph[i,j,k]:=0;
  end;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  label2.Caption:=inttostr(vCounts);
  label4.Caption:=inttostr(pCounts);
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.ApplyShader(mo: TObject);
var cubeSize: TAffineVector;
    MVP: TMatrix;
    LPos: TVector;
    NM: TAffineMatrix;
begin
  Voxels.Apply;
  cubeSize:=AffineVectorMake(1,1,1);
  Voxels.SetUniforms('cubeSize',cubesize);
  MVP:=TVBOMeshObject(mo).Matrices.WorldMatrix;
  Voxels.SetUniforms('ModelMatrix',MVP);
  MVP:=MatrixMultiply(MVP,TVBOMeshObject(mo).Matrices.ViewMatrix);
  LPos:=VectorTransform(GLLightSource1.AbsolutePosition,MVP);
  Voxels.SetUniforms('LightPos',LPos);
  Voxels.SetUniforms('ModelViewMatrix',MVP);
  SetMatrix(NM,MVP); InvertMatrix(NM); TransposeMatrix(NM);
  Voxels.SetUniforms('NormalMatrix',NM);
  MVP:=MatrixMultiply(MVP,TVBOMeshObject(mo).Matrices.ProjectionMatrix);
  Voxels.SetUniforms('ModelViewProjection',MVP);
end;


procedure TForm1.unApplyShader(mo: TObject);
begin
  Voxels.UnApply;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  GLCadencer1.Enabled:=false;
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  Voxels.Free; Mesh.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
var i,j,k, bits: integer;
begin
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  OGLStateEmul.GLStateCache.CheckStates;
  mesh:=TVBOMesh.CreateAsChild(glscene1.Objects);
  //Создадим наш шейдер
  Voxels:=TShaderProgram.Create;
  spId:=Voxels.CreateFromFile('Media\Voxel.Vert','Media\Voxel.Frag', 'Media\Voxel.Geom');
  //Заполним массив вокселями (сферой радиусом 50)
  FillSphere(2500);
  //Создаем буфер VBO для заданных вокселей
  new(vox); InitVBOBuff(vox^,GL_POINTS,DrawArrays);
  for i:=0 to 255 do for j:=0 to 255 do for k:=0 to 255 do begin
    if sph[i,j,k]<>0 then begin
      bits:=255; inc(vCounts);
      vox.Vertexes.Add(i-127,j-127,k-127);
      //Отмечаем какие грани отрисовывать не нужно
      if (k-1>=0) and (sph[i,j,k-1]>0) then bits:=bits-1;
      if (k+1<=255) and (sph[i,j,k+1]>0) then bits:=bits-2;

      if (j-1>=0) and (sph[i,j-1,k]>0) then bits:=bits-8;
      if (j+1<=255) and (sph[i,j+1,k]>0) then bits:=bits-4;

      if (i-1>=0) and (sph[i-1,j,k]>0) then bits:=bits-32;
      if (i+1<=255) and (sph[i+1,j,k]>0) then bits:=bits-16;
      //Посчитатаем количество выводимых полигонов (для статистики)
      if bits and 1=1 then inc(pCounts,2);
      if bits and 2=2 then inc(pCounts,2);
      if bits and 4=4 then inc(pCounts,2);
      if bits and 8=8 then inc(pCounts,2);
      if bits and 16=16 then inc(pCounts,2);
      if bits and 32=32 then inc(pCounts,2);
      //сохраним биты в нормалях
      vox.Normals.Add(bits/255,bits/255,bits/255);
    end;
  end;
  vox.RenderBuffs:=[uNormals]; GenVBOBuff(vox^,false);
  //Создаем меш из вокселей
  with Mesh.AddUserObject('Voxels',vox) do begin
    onBeforeRender:=ApplyShader;
    onAfterRender:=UnApplyShader;
  end;
end;

end.
