unit Unit1;

interface

uses
  Windows, Messages, SysUtilsLite, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLObjects, GLCoordinates, GLCadencer, GLWin32Viewer,
  GLCrossPlatform, BaseClasses, vboMesh, StdCtrls, vectorgeometry,
  GLSimpleNavigation, uTextures, uBaseClasses, OGLStateEmul;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLDummyCube1: TGLDummyCube;
    GLSimpleNavigation1: TGLSimpleNavigation;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  mesh: TVBOMesh;

  user_tex: TTexture;
  bmp_tex: TTexture;
  tga_tex: TTexture;
implementation

{$R *.dfm}

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.FormCreate(Sender: TObject);
var i,j:integer;
    m:array[0..127,0..127] of record r,g,b,a: byte;end;
    bmp: Tbitmap;
    r: single;
    c: byte;
begin
GLSceneViewer1.Buffer.RenderingContext.Activate;
OGLStateEmul.GLStateCache.CheckStates;
  //Формируем массив пикселей текстуры
  for i:=0 to 127 do for j:=0 to 127 do begin
     r:=1-sqrt(sqr(i-64)+sqr(j-64))/64;
     if r<0 then c:=0 else c:=trunc(r*255);
     m[i,j].r:=c;m[i,j].g:=c;m[i,j].b:=c;m[i,j].a:=c;
  end;
  //Создание текстуры из пользовательского массива данных
  user_Tex:=TTexture.Create;
  user_Tex.CreateBGRA8Texture2D(128,128,@m);
    //установка режима смешивания
  user_Tex.BlendingMode:=tbmAdditive;
  user_tex.TextureMode:=tcModulate;

  //Создание текстуры из bmp
  bmp:=Tbitmap.Create;
  bmp.LoadFromFile('Media\tile.bmp');
  bmp_tex:=TTexture.CreateFromBitmap(bmp);
    //установка переноса текстурных координат
  bmp_tex.SetWraps(twRepeat,twRepeat);
    //Установка текстурной матрицы
  bmp_tex.TextureMatrix:=CreateScaleMatrix(affinevectormake(2,0.5,1));
  bmp.Free;

  //Загрузка текстуры из файла
  tga_tex:= TTexture.CreateFromFile('Media\leaf.tga');
  //Установка свойств текстуры
    //установка зажатия текстурных координат
  tga_tex.SetWraps(twClamp,twClamp);
    //установка режима фильтрации
  tga_tex.SetFilters(mnLinearMipmapLinear,mgLinear);
    //установка режима смешивания
  tga_tex.BlendingMode:=tbmAlphaTest50;
  tga_tex.TextureMode:=tcReplace;

  Mesh:=TVBOMesh.CreateAsChild(GLScene1.Objects);
  Mesh.SortDirection:=sdBackToFront;
  //выводим 3 куба с разными текстурами
  Mesh.AddBox(1,1,1,1,1,1).MoveObject(0,0.5,0);
  Mesh.AddBox(1,1,1,1,1,1).MoveObject(-1,-0.5,0);
  Mesh.AddBox(1,1,1,1,1,1).MoveObject( 1,-0.5,0);
  Mesh[0].Texture:=user_tex;
  Mesh[1].Texture:=bmp_tex;
  Mesh[2].Texture:=tga_tex;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  Mesh.Free;
  user_tex.Free; bmp_tex.Free; tga_tex.Free;
end;

end.

