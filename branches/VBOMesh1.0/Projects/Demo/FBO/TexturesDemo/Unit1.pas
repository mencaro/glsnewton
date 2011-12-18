unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLObjects, GLCoordinates, GLCadencer, GLWin32Viewer,
  GLCrossPlatform, BaseClasses, vboMesh, StdCtrls, vectorgeometry,
  GLSimpleNavigation, MTLLoader, uTextures, GLGraphics;

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
  mesh:TVBOMesh;
  bw_tex: TGLTexture;
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
    bmp:Tbitmap;
    bm32: TGLBitmap32;
begin
GLSceneViewer1.Buffer.RenderingContext.Activate;
{  for i:=0 to 127 do for j:=0 to 127 do begin
     m[i,j].r:=i;m[i,j].g:=j;m[i,j].b:=i+j;m[i,j].a:=1;
  end;}
  bm32:= TGLBitmap32.Create;
  bmp:=Tbitmap.Create;
  bmp.LoadFromFile('..\Media\tile.bmp');
  bm32.AssignFromBitmap24WithoutRGBSwap(bmp);

  bw_tex:= TGLTexture.Create;
  //bw_tex.SetWraps(twClampToEdge,twClampToEdge);
  bw_tex.SetFilters(mnLinearMipmapLinear, mgLinear);
  bw_tex.CreateBGRA8Texture2D(bmp.Width,bmp.Height);
  bw_tex.UploadData(bm32.Data);

  bm32.Free; bmp.Free;

  Mesh:=TVBOMesh.CreateAsChild(GLScene1.Objects);
  with Mesh.AddSphere(2,16,32) do begin
     TextureId:=bw_tex.Handle;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
GLSceneViewer1.Buffer.RenderingContext.Activate;
  Mesh.Free;
  bw_tex.Free;
end;

end.

