unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLObjects, GLCoordinates, GLCadencer, GLWin32Viewer,
  GLCrossPlatform, BaseClasses, vboMesh, StdCtrls, vectorgeometry,
  GLSimpleNavigation, MTLLoader, uTextures, uFBO, GLGraphics;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLDummyCube1: TGLDummyCube;
    GLSimpleNavigation1: TGLSimpleNavigation;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RadioButton2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  mesh:TVBOMesh;
  bw_tex, fbo_tex, depth_tex: TGLTexture;
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
  bmp.LoadFromFile('..\Media\Tile.bmp');
  bm32.AssignFromBitmap24WithoutRGBSwap(bmp);

  bw_tex:= TGLTexture.Create;
  with bw_tex do begin
  //SetWraps(twClampToEdge,twClampToEdge);
    SetFilters(mnLinearMipmapLinear, mgLinear);
    CreateBGRA8Texture2D(bmp.Width,bmp.Height);
    UploadData(bm32.Data);
  end; bm32.Free; bmp.Free;

  fbo_tex:= TGLTexture.Create;
  with fbo_tex do begin
    SetFilters(mnLinearMipmapLinear, mgLinear);
    CreateBGRA8Texture2D(256,256);
  end;

  depth_tex:=TGLTexture.Create;
  with depth_tex do begin
    CreateDepth32FTexture2D(256,256);
  end;

  Mesh:=TVBOMesh.CreateAsChild(GLScene1.Objects);
  with Mesh.AddSphere(2,16,32) do begin
     MoveObject(0,0,0);
     TextureId:=bw_tex.Handle;
     fbo.ConfigFBO([rbDepth]);
     fbo.AttachTexture(fbo_tex);
     fbo.ConfigDepthBuffer(bmTexture,dp32);
     fbo.AttachDepthTexture(depth_tex);
     fbo.InitFBO(fbo_tex.Width,fbo_tex.Height);
     fbo.Active:=true;
  end;
  with Mesh.AddScreenQuad do begin
     TextureId:=fbo_tex.Handle;
  end;

end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
GLSceneViewer1.Buffer.RenderingContext.Activate;
  Mesh.Free;
  bw_tex.Free; fbo_tex.Free;
end;

procedure TForm1.RadioButton2Click(Sender: TObject);
begin
  case TRadioButton(sender).tag of
     0: begin
          Mesh[1].TextureId:=fbo_tex.Handle;
        end;
     1: begin
          Mesh[1].TextureId:=depth_tex.Handle;
        end;
  end;
end;

end.

