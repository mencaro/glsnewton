{: vboMesh
	Historique:
  17/12/11 - Fantom - Переписан алгоритм прикрепления текстур, добавлена прямая установка таргета, включая глубину и трафарет
  16/12/11 - Fantom - При повторной инициализации FBO с отличающимися размерами происходит переинициализация
}
unit uFBO;

interface

Uses Classes, OpenGL1x, uTextures, OGLStateEmul;

Type
  TRenderBuffer = (rbDepth, rbStencil);
  TRenderBuffers = set of TRenderBuffer;
  TBufferMode = (bmNone, bmBuffer, bmTexture);
  TDepthPrecision = (dpDefault, dp16, dp24, dp32);
  TStencilPrecision = (spDefault, sp1bit, sp4bits, sp8bits, sp16bits);
  TMultisampleFormat = (MSAA2, MSAA4, MSAA8);
  TMRTTarget = (tgTexture,tgDepth,tgDepthStencil,tgMRT0,tgMRT1,tgMRT2,tgMRT3);

  TFBORenderTarget = record
    Texture: TTexture;
    BuffId: GLUInt;
    Mode: TBufferMode;
    Precision: GLEnum;
  end;

  TAttachments = record
     Textures: TList;
     DepthBuffer: TFBORenderTarget;
     StencilBuffer: TFBORenderTarget;
     DepthStencilBuffer: TFBORenderTarget;
  end;

  TFBOTarget = record
    Texture: TTexture;
    TargetTo: TMRTTarget;
  end;

  TFrameBufferObject = class
  private
    FBOId: GLUInt;
    FMSFBOId: GLUInt;
    FAttachments: TAttachments;
    FRenderBuffers: TRenderBuffers;
    FReadBackBuffers: TList;
    FWidth, FHeight: integer;
    FViewport: array[0..3] of integer;
    FInit: boolean;
    FActive: boolean;
    FDeactivate: boolean;
    FMultisample: TMultisampleFormat;

    procedure AttachTextureTarget(tex: TTexture; attachement: GLEnum);
    function OGLDBPrecision(Precision: TDepthPrecision): GLEnum;
    function OGLSBPrecision(Precision: TStencilPrecision): GLEnum;
    function GetTexture(index: integer): TTexture;
    procedure SetTexture(index: integer; const Value: TTexture);
    function GetAttachmentsCount: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure InitFBO(Width, Height: integer);
    procedure ResetFBO(ResetConfig: boolean = true);

    procedure ConfigFBO(RenderBuffers: TRenderBuffers);
    procedure ConfigDepthBuffer(mode: TBufferMode; precision: TDepthPrecision=dpDefault);
    procedure ConfigStencilBuffer(mode: TBufferMode; precision: TStencilPrecision=spDefault);
    procedure ConfigDepthStencilBuffer(mode: TBufferMode);
    procedure AttachTexture(tex: TTexture; aTarget: TMRTTarget=tgTexture);
    procedure AttachDepthTexture(tex: TTexture);
    procedure AttachStencilTexture(tex: TTexture);
    procedure AttachDepthStencilTexture(tex: TTexture);
    procedure DetachDepthTexture;
    procedure DetachStencilTexture;
    procedure DetachDepthStencilTexture;

    procedure DetachTexture(index:integer);
    procedure DetachAllTextures;

    procedure Apply(ClearBuffers: boolean = true);
    procedure UnApply;
    procedure SetReadBackBuffer(const ColorBufers: array of GLUint);

    property Textures[index: integer]:TTexture read GetTexture write SetTexture;
    property AttachmentsCount: integer read GetAttachmentsCount;
    property Multisample: TMultisampleFormat read FMultisample write FMultisample;

    property Active: boolean read FActive write FActive;
    property DeactivateAfter: boolean read FDeactivate write FDeactivate;
    property Handle: cardinal read FBOId;
  end;

implementation

{ TFrameBufferObject }

procedure TFrameBufferObject.AttachTexture(tex: TTexture; aTarget: TMRTTarget);
var i,n,m: integer;
begin
   glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, fboId); n:=-1;
   if aTarget=tgTexture then begin
     AttachTextureTarget(tex,GL_COLOR_ATTACHMENT0_EXT+FAttachments.Textures.count);
     FAttachments.Textures.Add(tex);
   end else begin
     case aTarget of
       tgDepth: AttachDepthTexture(tex);
       tgDepthStencil: AttachDepthStencilTexture(tex);
       tgMRT0: begin AttachTextureTarget(tex,GL_COLOR_ATTACHMENT0_EXT); n:=0; end;
       tgMRT1: begin AttachTextureTarget(tex,GL_COLOR_ATTACHMENT1_EXT); n:=1; end;
       tgMRT2: begin AttachTextureTarget(tex,GL_COLOR_ATTACHMENT2_EXT); n:=2; end;
       tgMRT3: begin AttachTextureTarget(tex,GL_COLOR_ATTACHMENT3_EXT); n:=3; end;
     end;
     if n>=0 then begin
       if n>FAttachments.Textures.Count-1 then begin
         m:=FAttachments.Textures.Count;
         FAttachments.Textures.Count:=n+1;
         for i:=m to n do FAttachments.Textures[i]:=nil;
       end; FAttachments.Textures[n]:=tex;
     end;
   end;
   glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
end;

procedure TFrameBufferObject.DetachTexture(index: integer);
var tex: TTexture;
begin
  if (index<FAttachments.Textures.count) and (index>=0) then begin
    tex:=FAttachments.Textures[index];
//  assert(index<FAttachments.Textures.count,'Not enough attached texture units');
    glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, fboId);
    glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT+index, tex.GLTarget, 0, 0);
    FAttachments.Textures.Delete(index);
    glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
  end;
end;

procedure TFrameBufferObject.ConfigFBO(RenderBuffers: TRenderBuffers);
begin
  FRenderBuffers:=RenderBuffers;
end;

constructor TFrameBufferObject.Create;
begin
  inherited;
  with FAttachments do begin
    Textures:=TList.Create;
    DepthBuffer.Mode:=bmNone;
    StencilBuffer.Mode:=bmNone;
    DepthStencilBuffer.Mode:=bmNone;
    glGenRenderbuffersEXT(1, @DepthBuffer.buffId);
    glGenRenderbuffersEXT(1, @StencilBuffer.buffId);
    glGenRenderbuffersEXT(1, @DepthStencilBuffer.buffId);
    FReadBackBuffers:=TList.Create;
  end;
  glGenFramebuffersEXT(1, @FBOId);
  glGenFramebuffersEXT(1, @FMSFBOId);
  FRenderBuffers:=[];
  FActive:=False;
  FDeactivate:=false;
  FInit:=false;
end;


destructor TFrameBufferObject.Destroy;
var FBTarget: GLEnum;
    i: integer;
begin
  FBTarget:=GL_FRAMEBUFFER_EXT;
//  FBTarget:=GL_DRAW_FRAMEBUFFER_EXT;

  for i:=0 to FAttachments.Textures.Count-1 do DetachTexture(i);
  FAttachments.Textures.Destroy;
  glBindFramebufferEXT(FBTarget, 0);
  glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, 0);
  glDeleteFramebuffersEXT(1,@FBOId);
  glDeleteFramebuffersEXT(1, @FMSFBOId);
  FReadBackBuffers.Free;
  with FAttachments do begin
    glDeleteRenderbuffersEXT(1, @DepthBuffer.buffId);
    glDeleteRenderbuffersEXT(1, @StencilBuffer.buffId);
    glDeleteRenderbuffersEXT(1, @DepthStencilBuffer.buffId);
  end;
  inherited;
end;

function TFrameBufferObject.GetTexture(index: integer): TTexture;
begin
  assert(index<FAttachments.Textures.count,'Not enough attached texture units');
  result:=FAttachments.Textures[index];
end;

procedure TFrameBufferObject.SetTexture(index: integer;
  const Value: TTexture);
begin
  assert(index<FAttachments.Textures.count,'Not enough attached texture units');
  FAttachments.Textures[index]:=Value;
end;

procedure TFrameBufferObject.ConfigDepthBuffer(mode: TBufferMode;
  precision: TDepthPrecision);
begin
  FAttachments.DepthBuffer.Mode:=mode;
  FAttachments.DepthBuffer.Precision:=OGLDBPrecision(precision);
end;

procedure TFrameBufferObject.ConfigStencilBuffer(mode: TBufferMode;
  precision: TStencilPrecision);
begin
  FAttachments.StencilBuffer.Mode:=mode;
  FAttachments.StencilBuffer.Precision:=OGLSBPrecision(precision);
end;

procedure TFrameBufferObject.ConfigDepthStencilBuffer(mode: TBufferMode);
begin
  FAttachments.DepthStencilBuffer.Mode:=mode;
  FAttachments.DepthStencilBuffer.Precision:=0;
end;

procedure TFrameBufferObject.InitFBO(Width, Height: integer);
var i:integer;
    FBTarget: GLEnum;
begin
  if (Width=FWidth) and (Height=FHeight) and FInit then exit else
    if FInit then ResetFBO;
  FWidth:=width; FHeight:=height;
  FBTarget:=GL_FRAMEBUFFER_EXT;
//  FBTarget:=GL_DRAW_FRAMEBUFFER_EXT;

  with FAttachments do begin
    if (rbDepth in FRenderBuffers) and (not (rbStencil in FRenderBuffers)) then
    begin
      if DepthBuffer.Mode<>bmNone then begin
          glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, DepthBuffer.buffId);
          glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT, DepthBuffer.Precision, FWidth, FHeight);
          glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, 0);
      end;
      case DepthBuffer.Mode of
        bmBuffer: begin
          glBindFramebufferEXT(FBTarget, fboId);
          glFramebufferRenderbufferEXT(FBTarget, GL_DEPTH_ATTACHMENT_EXT,
                        GL_RENDERBUFFER_EXT, DepthBuffer.buffId);
          glBindFramebufferEXT(FBTarget, 0);
        end;
      end;
    end;
    if (not (rbDepth in FRenderBuffers)) and (rbStencil in FRenderBuffers) then
    begin
      if StencilBuffer.Mode<>bmNone then begin
          glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, StencilBuffer.buffId);
          glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT, StencilBuffer.Precision, FWidth, FHeight);
          glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, 0);
      end;
      case StencilBuffer.Mode of
        bmBuffer: begin
          glBindFramebufferEXT(FBTarget, fboId);
          glFramebufferRenderbufferEXT(FBTarget, GL_STENCIL_ATTACHMENT_EXT,
                        GL_RENDERBUFFER_EXT, StencilBuffer.buffId);
          glBindFramebufferEXT(FBTarget, 0);
        end;
      end;
    end;

    if (rbDepth in FRenderBuffers) and (rbStencil in FRenderBuffers) then
    begin
      glBindFramebufferEXT(FBTarget, fboId);
      glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, DepthStencilBuffer.buffId);
      glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT, GL_DEPTH24_STENCIL8_EXT, FWidth, FHeight);
      glBindRenderbufferEXT( GL_RENDERBUFFER_EXT, 0 );
      case DepthStencilBuffer.Mode of
        bmBuffer: begin
          glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT,GL_DEPTH_ATTACHMENT_EXT,GL_RENDERBUFFER_EXT,DepthStencilBuffer.buffId);
          glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT,GL_STENCIL_ATTACHMENT_EXT,GL_RENDERBUFFER_EXT,DepthStencilBuffer.buffId);
        end;
      end;
      glBindFramebufferEXT(FBTarget, 0);
    end;
  end;
  FInit:=true;
end;

procedure TFrameBufferObject.AttachDepthTexture(tex: TTexture);
begin
  if FAttachments.DepthBuffer.Mode=bmTexture then begin
    if tex<>FAttachments.DepthBuffer.Texture then begin
      glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, fboId);
      AttachTextureTarget(tex, GL_DEPTH_ATTACHMENT_EXT);
      glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
      FAttachments.DepthBuffer.Texture:=tex;
    end;
  end;
end;

procedure TFrameBufferObject.AttachStencilTexture(tex: TTexture);
begin
  if FAttachments.StencilBuffer.Mode=bmTexture then begin
    if tex<>FAttachments.StencilBuffer.Texture then begin
      glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, fboId);
      AttachTextureTarget(tex, GL_STENCIL_ATTACHMENT_EXT);
      glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
      FAttachments.StencilBuffer.Texture:=tex;
    end;
  end;
end;

procedure TFrameBufferObject.AttachDepthStencilTexture(tex: TTexture);
begin
  if FAttachments.DepthStencilBuffer.Mode=bmTexture then begin
    if tex<>FAttachments.DepthStencilBuffer.Texture then begin
      glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, fboId);
      AttachTextureTarget(tex, GL_DEPTH_ATTACHMENT);
      AttachTextureTarget(tex, GL_STENCIL_ATTACHMENT);
      glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
      FAttachments.DepthStencilBuffer.Texture:=tex;
    end;
  end;
end;

procedure TFrameBufferObject.DetachAllTextures;
var tex: TTexture;
    i: integer;
begin
  glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, fboId);
  for i:=0 to FAttachments.Textures.count-1 do begin
    tex:=FAttachments.Textures[i];
    glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT+i, tex.GLTarget, 0, 0);
  end;
  FAttachments.Textures.Clear;
  glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
end;

procedure TFrameBufferObject.DetachDepthStencilTexture;
begin
  glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, fboId);
  AttachTextureTarget(nil, GL_DEPTH_ATTACHMENT_EXT);
  AttachTextureTarget(nil, GL_STENCIL_ATTACHMENT_EXT);
  glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
  FAttachments.DepthStencilBuffer.Texture:=nil;
end;

procedure TFrameBufferObject.DetachDepthTexture;
begin
  glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, fboId);
  AttachTextureTarget(nil, GL_DEPTH_ATTACHMENT_EXT);
  glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
  FAttachments.DepthBuffer.Texture:=nil;
end;

procedure TFrameBufferObject.DetachStencilTexture;
begin
  glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, fboId);
  AttachTextureTarget(nil, GL_STENCIL_ATTACHMENT_EXT);
  glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
  FAttachments.StencilBuffer.Texture:=nil;
end;

function TFrameBufferObject.OGLDBPrecision(
  Precision: TDepthPrecision): GLEnum;
begin
  case Precision of
    dpDefault: result:= GL_DEPTH_COMPONENT;
    dp16: result:= GL_DEPTH_COMPONENT16;
    dp24: result:= GL_DEPTH_COMPONENT24;
    dp32: result:= GL_DEPTH_COMPONENT32;
    else result:= GL_DEPTH_COMPONENT;
  end;
end;

function TFrameBufferObject.OGLSBPrecision(
  Precision: TStencilPrecision): GLEnum;
begin
  case Precision of
    spDefault: result:=GL_STENCIL_INDEX;
    sp1bit: result:=GL_STENCIL_INDEX1;
    sp4bits: result:=GL_STENCIL_INDEX4;
    sp8bits: result:=GL_STENCIL_INDEX8;
    sp16bits: result:=GL_STENCIL_INDEX16;
    else result:=GL_STENCIL_INDEX;
  end;
end;

procedure TFrameBufferObject.AttachTextureTarget(tex: TTexture;
   attachement: GLEnum);
var FBTarget: GLEnum;
    th: integer;
begin
  FBTarget:=GL_FRAMEBUFFER_EXT;
  if assigned(tex) then th:=tex.Handle else begin
    glFramebufferTexture2DEXT(FBTarget, attachement, GL_TEXTURE_2D, 0, 0);
    exit;
  end;
//  FBTarget:=GL_DRAW_FRAMEBUFFER_EXT;
  case tex.TextureTarget of
    ttTexture1D: glFramebufferTexture1DEXT(FBTarget, attachement,
                 GL_TEXTURE_1D, th, 0);
    ttTexture2D: glFramebufferTexture2DEXT(FBTarget, attachement,
                 GL_TEXTURE_2D, th, 0);
    ttTexture3D: glFramebufferTexture3DEXT(FBTarget, attachement,
                 GL_TEXTURE_3D, th, 0, 0);
  end;
end;

procedure TFrameBufferObject.Apply(ClearBuffers: boolean);
var buffers: array of GLEnum;
    i,n:integer;
    cb: GLUInt;
    FBTarget: GLEnum;
begin
//  FBTarget:=GL_DRAW_FRAMEBUFFER_EXT;
  FBTarget:=GL_FRAMEBUFFER_EXT;
  if ClearBuffers then begin
    glGetIntegerv(GL_VIEWPORT, @Fviewport);
    if (FViewport[2]<>FWidth) or (FViewport[2]<>FHeight) then
      glViewport(0,0,FWidth,FHeight);
  end;
  glBindFramebufferEXT(FBTarget, fboId);
  with FAttachments do begin
    if Textures.Count>0 then begin
       n:=0; setlength(buffers,Textures.Count);
       for i:=0 to Textures.Count-1 do begin
         if assigned(Textures[i]) then begin
           buffers[n]:=GL_COLOR_ATTACHMENT0_EXT+n; inc(n);
         end;
       end; setlength(buffers,n);
       glDrawBuffers(n,@buffers[0]);
    end else begin glDrawBuffer(GL_NONE);  glReadBuffer(GL_NONE); end;
    if ClearBuffers then begin
      if Textures.Count>0 then cb:= GL_COLOR_BUFFER_BIT else cb:=0;
      if rbDepth in FRenderBuffers then cb:= cb or GL_DEPTH_BUFFER_BIT;
      if rbStencil in FRenderBuffers then cb:= cb or GL_STENCIL_BUFFER_BIT;
      if cb<>0 then glClear(cb);
    end;
  end;
end;

procedure TFrameBufferObject.UnApply;
var tex: TTexture;
    i,n:integer;
    FBTarget: GLEnum;
begin
//  FBTarget:=GL_DRAW_FRAMEBUFFER_EXT;
  FBTarget:=GL_FRAMEBUFFER_EXT;
  for i:=0 to FReadBackBuffers.Count-1 do begin
    n:=integer(FReadBackBuffers[i]);
    if n<FAttachments.Textures.Count then begin
      tex:=FAttachments.Textures[n];
      if assigned(tex) then begin
        glReadBuffer(GL_COLOR_ATTACHMENT0_EXT+n);
        glBindBuffer(GL_PIXEL_PACK_BUFFER, tex.PBOReadBuffer);
        glReadPixels(0,0, FWidth, FHeight, tex.PixelOGLFormat, tex.PrecisionOGLFormat, nil);
      end;
    end;
  end;

  glBindFramebufferEXT(FBTarget, 0);
  glReadBuffer(GL_BACK); glDrawBuffer(GL_BACK);
  if (FViewport[2]<>FWidth) or (FViewport[2]<>FHeight) then
    glViewport(Fviewport[0],Fviewport[1],Fviewport[2],Fviewport[3]);

  with FAttachments do begin
    for i:=0 to Textures.Count-1 do begin
      tex:=Textures[i];
      if assigned(tex) and tex.Mipmapping then begin
        glBindTexture(cTexTargets[tex.TextureTarget], tex.Handle);
        glGenerateMipmapEXT(cTexTargets[tex.TextureTarget]);
        glBindTexture(cTexTargets[tex.TextureTarget],0);
      end;
    end;
    if DepthBuffer.Mode=bmTexture then begin
      tex:=DepthBuffer.Texture;
      if assigned(tex) and tex.Mipmapping then begin
        glBindTexture(cTexTargets[tex.TextureTarget], tex.Handle);
        glGenerateMipmapEXT(cTexTargets[tex.TextureTarget]);
        glBindTexture(cTexTargets[tex.TextureTarget],0);
      end;
    end;
    if StencilBuffer.Mode=bmTexture then begin
      tex:=StencilBuffer.Texture;
      if assigned(tex) and tex.Mipmapping then begin
        glBindTexture(cTexTargets[tex.TextureTarget], tex.Handle);
        glGenerateMipmapEXT(cTexTargets[tex.TextureTarget]);
        glBindTexture(cTexTargets[tex.TextureTarget],0);
      end;
    end;
  end;
  if FDeactivate then FActive:=false;
end;

procedure TFrameBufferObject.SetReadBackBuffer(
  const ColorBufers: array of GLUint);
var i:integer;
begin
  FReadBackBuffers.Clear;
  for i:=0 to length(ColorBufers)-1 do FReadBackBuffers.Add(pointer(ColorBufers[i]));
end;

function TFrameBufferObject.GetAttachmentsCount: integer;
begin
   result:=FAttachments.Textures.Count;
end;

procedure TFrameBufferObject.ResetFBO(ResetConfig: boolean);
var i: integer;
begin
  FInit:=false;

  for i:=0 to FAttachments.Textures.Count-1 do DetachTexture(i);
  if FAttachments.DepthBuffer.Mode=bmTexture then DetachDepthTexture;
  if FAttachments.StencilBuffer.Mode=bmTexture then DetachStencilTexture;
  if FAttachments.DepthStencilBuffer.Mode=bmTexture then DetachDepthStencilTexture;

  glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
  glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, 0);
  glDeleteFramebuffersEXT(1,@FBOId);
  glDeleteFramebuffersEXT(1, @FMSFBOId);
  FReadBackBuffers.Clear;
  with FAttachments do begin
    Textures.Clear;
    glDeleteRenderbuffersEXT(1, @DepthBuffer.buffId);
    glDeleteRenderbuffersEXT(1, @StencilBuffer.buffId);
    glDeleteRenderbuffersEXT(1, @DepthStencilBuffer.buffId);
  end;
  with FAttachments do begin
    if ResetConfig then begin
      DepthBuffer.Mode:=bmNone;
      StencilBuffer.Mode:=bmNone;
      DepthStencilBuffer.Mode:=bmNone;
      FRenderBuffers:=[];
    end;
    glGenRenderbuffersEXT(1, @DepthBuffer.buffId);
    glGenRenderbuffersEXT(1, @StencilBuffer.buffId);
    glGenRenderbuffersEXT(1, @DepthStencilBuffer.buffId);
  end;
  glGenFramebuffersEXT(1, @FBOId);
  glGenFramebuffersEXT(1, @FMSFBOId);
  FActive:=False;
  FDeactivate:=false;

end;

end.
