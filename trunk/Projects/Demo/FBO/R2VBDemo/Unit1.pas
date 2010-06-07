unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLObjects, GLCoordinates, GLCadencer, GLWin32Viewer,
  GLCrossPlatform, BaseClasses, vboMesh, StdCtrls, vectorgeometry, OpenGL1x,
  GLSimpleNavigation, MTLLoader, uTextures, uFBO, uVBO, uShaders, GLGraphics,
  GLRenderContextInfo;

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
    procedure ApplyShader(rci:TRenderContextInfo;  mo:TObject);
    procedure UnApplyShader(rci:TRenderContextInfo; mo:TObject);
  end;

var
  Form1: TForm1;
  mesh:TVBOMesh;
  bw_tex: TGLTexture;
  Shaders: TShaders;
  spId: integer;
  data: array of integer;
  mrt: array[0..3] of TGLTexture;
  vbo: PVBOBuffer;
implementation

{$R *.dfm}

Function createIndexBuffer(texWidth,texHeight: integer):GLUInt;
var
    i,j,k,trCount: integer;
begin
  trCount:=(texWidth)*(texHeight)*6;
  setlength(data,trCount);  k := 0;
  for j := 0 to texHeight - 2 do
	for i := 0 to texWidth - 2 do begin
            data [k]   := i + texWidth*j;
            data [k+1] := i + texWidth*j + 1;
            data [k+2] := i + texWidth*j + texWidth;

            data [k+3] := i + texWidth*j + 1;
            data [k+4] := i + texWidth*j + texWidth + 1;
            data [k+5] := i + texWidth*j + texWidth;
            k:=k+6;
	end;
   glGenBuffers(1, @Result);
   glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, Result);
   glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(GLUint) * trCount,
        @data[0], GL_STATIC_DRAW);
   glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
end;

function CreateVBOFromPBO(width, height: integer): PVBOBuffer;
var vbo: PVBOBuffer;
begin
  new(vbo); InitVBOBuff(vbo^,GL_TRIANGLES,DrawElements);

  with vbo^ do begin
    vId:=mrt[0].PBOReadBuffer;
    nId:=mrt[1].PBOReadBuffer;
    tId:=mrt[2].PBOReadBuffer;

    iId:=createIndexBuffer(width, height);
    RenderBuffs:=[uIndices,uNormals,uTexCoords];
    MaxElements:=width*height*6;
    ElementsCount:=MaxElements;
    emin:=affinevectormake(-1,-1,-1);
    emax:=affinevectormake(1,1,1);
    Builded:=true;
  end;
  result:=vbo;
end;

Function CreateMRTShader(Vertex,Fragment: string): integer;
var vsId, fsId: integer;
    v,f: TStringList;
begin
  v:=TstringList.Create;
  f:=TstringList.Create;
  v.LoadFromFile(Vertex);
  f.LoadFromFile(Fragment);
  Shaders:= TShaders.Create;
  with Shaders do begin
    vsId := AddShaderObject(v.Text,GL_VERTEX_SHADER);
    fsId := AddShaderObject(f.Text,GL_FRAGMENT_SHADER);
    vsId := ShaderObjectsList[vsId];
    fsId := ShaderObjectsList[fsId];

    result:=CreateShaderProgram;

    AttachShaderObjectToProgram(vsId,result);
    AttachShaderObjectToProgram(fsId,result);

    LinkShaderProgram(result);
  end;
  v.Free; f.Free;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.FormCreate(Sender: TObject);
var i:integer;
    bmp:Tbitmap;
    bm32: TGLBitmap32;
begin
GLSceneViewer1.Buffer.RenderingContext.Activate;
  bm32:= TGLBitmap32.Create;
  bmp:=Tbitmap.Create;
  bmp.LoadFromFile('..\Media\NVHmap.bmp');
  bm32.AssignFromBitmap24WithoutRGBSwap(bmp);

  bw_tex:= TGLTexture.Create;
  with bw_tex do begin
    SetFilters(mnLinearMipmapLinear, mgLinear);
    CreateBGRA8Texture2D(bmp.Width,bmp.Height,bm32.Data);
  end; bm32.Free; bmp.Free;

  spId:=CreateMRTShader('..\Project\Demo\FBO\R2VBDemo\Shader.Vert',
                        '..\Project\Demo\FBO\R2VBDemo\Shader.Frag');

  for i:=0 to 2 do begin
    mrt[i]:=TGLTexture.Create;
    mrt[i].CreateRGB32FTexture2D(256,256);
  end;
  
  Mesh:=TVBOMesh.CreateAsChild(GLScene1.Objects);

  with Mesh.AddScreenQuad do begin
     TextureId:=bw_tex.Handle;
     with FBO do begin
       for i:=0 to 2 do AttachTexture(mrt[i]);
       SetReadBackBuffer([0,1,2]);
       InitFBO(256,256);
       Active:=true;
     end;
     onBeforeRender:=ApplyShader;
     onAfterRender:=UnApplyShader;
  end;

  vbo:=CreateVBOFromPBO(256,256);

  with Mesh.AddUserObject('r2vb',vbo) do begin
     TextureId:=bw_tex.Handle;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
var i:integer;
begin
GLSceneViewer1.Buffer.RenderingContext.Activate;
  Mesh.Free;
  bw_tex.Free;
  for i:=0 to 2 do mrt[i].Free;
end;

procedure TForm1.ApplyShader(rci: TRenderContextInfo; mo: TObject);
begin
  Shaders.UseProgramObject(spid);
  Shaders.SetUniforms(spid,'NormalMap',0);
  Shaders.SetUniforms(spid,'scalefactor',0.1);
  Shaders.SetUniforms(spid,'invtexsize',1/256);
end;

procedure TForm1.UnApplyShader(rci: TRenderContextInfo; mo: TObject);
begin
  Shaders.UseProgramObject(0);
  mesh[0].Visible:=false;
end;

end.

