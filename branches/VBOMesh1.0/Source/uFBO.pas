unit uFBO;

interface

Uses Classes, OpenGL1x, uTextures;

Type
  TRenderBuffer = (rbDepth, rbStencil);
  TRenderBuffers = set of TRenderBuffer;
  TBufferMode = (bmNone, bmBuffer, bmTexture);
  TDepthPrecision = (dpDefault, dp16, dp24, dp32);
  TStencilPrecision = (spDefault, sp1bit, sp4bits, sp8bits, sp16bits);
  TAttachments = record
     Textures: TList;
     DepthBuffer: record
         Texture: TGLTexture;
         RBId: GLUInt;
         Mode: TBufferMode;
         Precision: GLEnum;
     end;
     StencilBuffer: record
         Texture: TGLTexture;
         SBId: GLUInt;
         Mode: TBufferMode;
         Precision: GLEnum;
     end;
  end;
  TFrameBufferObject = class (TObject)
    private
      FBOId: GLUInt;
      FAttachments: TAttachments;
      FRenderBuffers: TRenderBuffers;
      FReadBackBuffers: TList;
      FWidth, FHeight: integer;
      FViewport: array[0..3] of integer;
      FActive: boolean;

      procedure AttachTextureTarget(tex: TGLTexture; attachement: GLEnum);
      function OGLDBPrecision(Precision: TDepthPrecision): GLEnum;
      function OGLSBPrecision(Precision: TStencilPrecision): GLEnum;
      function GetTexture(index: integer): TGLTexture;
      procedure SetTexture(index: integer; const Value: TGLTexture);
    public
      constructor Create;
      destructor Destroy; override;
      procedure InitFBO(Width, Height: integer);

      procedure ConfigFBO(RenderBuffers: TRenderBuffers);
      procedure ConfigDepthBuffer(mode: TBufferMode; precision: TDepthPrecision=dpDefault);
      procedure ConfigStencilBuffer(mode: TBufferMode; precision: TStencilPrecision=spDefault);
      procedure AttachTexture(tex: TGLTexture);
      procedure AttachDepthTexture(tex: TGLTexture);
      procedure AttachStencilTexture(tex: TGLTexture);
      procedure DetachDepthTexture;
      procedure DetachStencilTexture;
      procedure DetachTexture(index:integer);

      procedure Apply;
      procedure UnApply;overload;
      procedure SetReadBackBuffer(const ColorBufers: array of GLUint);

      property Textures[index: integer]:TGLTexture read GetTexture write SetTexture;

      property Active: boolean read FActive write FActive;

  end;

implementation

{ TFrameBufferObject }

procedure TFrameBufferObject.AttachTexture(tex: TGLTexture);
begin
   FAttachments.Textures.Add(tex);
end;

procedure TFrameBufferObject.DetachTexture(index: integer);
begin
  assert(index<FAttachments.Textures.count,'Not enough attached texture units');
  FAttachments.Textures.Delete(index);
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
    glGenRenderbuffersEXT(1, @DepthBuffer.rbId);
    glGenRenderbuffersEXT(1, @StencilBuffer.sbId);
    FReadBackBuffers:=TList.Create;
  end;
  glGenFramebuffersEXT(1, @FBOId);
  FRenderBuffers:=[];
  FActive:=False;
end;


destructor TFrameBufferObject.Destroy;
var FBTarget: GLEnum;
begin
  FBTarget:=GL_FRAMEBUFFER_EXT;
//  FBTarget:=GL_DRAW_FRAMEBUFFER_EXT;

  FAttachments.Textures.Destroy;
  glBindFramebufferEXT(FBTarget, 0);
  glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, 0);
  glDeleteFramebuffersEXT(1,@FBOId);
  FReadBackBuffers.Free;
  with FAttachments do begin
    glDeleteRenderbuffersEXT(1, @DepthBuffer.rbId);
    glDeleteRenderbuffersEXT(1, @StencilBuffer.sbId);
  end;
  inherited;
end;

function TFrameBufferObject.GetTexture(index: integer): TGLTexture;
begin
  assert(index<FAttachments.Textures.count,'Not enough attached texture units');
  result:=FAttachments.Textures[index];
end;

procedure TFrameBufferObject.SetTexture(index: integer;
  const Value: TGLTexture);
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

procedure TFrameBufferObject.InitFBO(Width, Height: integer);
var i:integer;
    FBTarget: GLEnum;
begin
  FWidth:=width; FHeight:=height;
  FBTarget:=GL_FRAMEBUFFER_EXT;
//  FBTarget:=GL_DRAW_FRAMEBUFFER_EXT;

  with FAttachments do begin
        if (rbDepth in FRenderBuffers) and (not (rbStencil in FRenderBuffers)) then
        begin
          if DepthBuffer.Mode<>bmNone then begin
              glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, DepthBuffer.rbId);
              glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT, DepthBuffer.Precision, FWidth, FHeight);
              glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, 0);
          end;
          case DepthBuffer.Mode of
            bmBuffer: begin
              glBindFramebufferEXT(FBTarget, fboId);
              glFramebufferRenderbufferEXT(FBTarget, GL_DEPTH_ATTACHMENT_EXT,
                            GL_RENDERBUFFER_EXT, DepthBuffer.rbId);
              glBindFramebufferEXT(FBTarget, 0);
            end;
            bmTexture: begin
              glBindFramebufferEXT(FBTarget, fboId);
              AttachTextureTarget(DepthBuffer.Texture, GL_DEPTH_ATTACHMENT_EXT);
              glBindFramebufferEXT(FBTarget, 0);
            end;
          end;
        end;
        if (not (rbDepth in FRenderBuffers)) and (rbStencil in FRenderBuffers) then
        begin
          if StencilBuffer.Mode<>bmNone then begin
              glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, StencilBuffer.sbId);
              glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT, StencilBuffer.Precision, FWidth, FHeight);
              glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, 0);
          end;
          case StencilBuffer.Mode of
            bmBuffer: begin
              glBindFramebufferEXT(FBTarget, fboId);
              glFramebufferRenderbufferEXT(FBTarget, GL_STENCIL_ATTACHMENT_EXT,
                            GL_RENDERBUFFER_EXT, StencilBuffer.sbId);
              glBindFramebufferEXT(FBTarget, 0);
            end;
            bmTexture: begin
              glBindFramebufferEXT(FBTarget, fboId);
              AttachTextureTarget(StencilBuffer.Texture, GL_STENCIL_ATTACHMENT_EXT);
              glBindFramebufferEXT(FBTarget, 0);
            end;
          end;
        end;
        if Textures.Count>0 then begin
          glBindFramebufferEXT(FBTarget, fboId);
          for i:=0 to Textures.Count-1 do
             AttachTextureTarget(TGLTexture(Textures[i]),GL_COLOR_ATTACHMENT0_EXT+i);
          glBindFramebufferEXT(FBTarget, 0);
        end;
  end;
end;

procedure TFrameBufferObject.AttachDepthTexture(tex: TGLTexture);
begin
  FAttachments.DepthBuffer.Texture:=tex;
end;

procedure TFrameBufferObject.AttachStencilTexture(tex: TGLTexture);
begin
  FAttachments.StencilBuffer.Texture:=tex;
end;

procedure TFrameBufferObject.DetachDepthTexture;
begin
  FAttachments.DepthBuffer.Texture:=nil;
end;

procedure TFrameBufferObject.DetachStencilTexture;
begin
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

procedure TFrameBufferObject.AttachTextureTarget(tex: TGLTexture;
   attachement: GLEnum);
var FBTarget: GLEnum;
begin
  FBTarget:=GL_FRAMEBUFFER_EXT;
//  FBTarget:=GL_DRAW_FRAMEBUFFER_EXT;
  case tex.Target of
    ttTexture1D: glFramebufferTexture1DEXT(FBTarget, attachement,
                 GL_TEXTURE_1D, Tex.Handle, 0);
    ttTexture2D: glFramebufferTexture2DEXT(FBTarget, attachement,
                 GL_TEXTURE_2D, Tex.Handle, 0);
    ttTexture3D: glFramebufferTexture3DEXT(FBTarget, attachement,
                 GL_TEXTURE_3D, Tex.Handle, 0, 0);
  end;
end;

procedure TFrameBufferObject.Apply;
var buffers: array of GLEnum;
    i:integer;
    cb: GLUInt;
    FBTarget: GLEnum;
begin
//  FBTarget:=GL_DRAW_FRAMEBUFFER_EXT;
  FBTarget:=GL_FRAMEBUFFER_EXT;
  glGetIntegerv(GL_VIEWPORT, @Fviewport);
  glViewport(0,0,FWidth,FHeight);
  glBindFramebufferEXT(FBTarget, fboId);
  with FAttachments do begin
    setlength(buffers,Textures.Count);
    if Textures.Count>0 then begin
       for i:=0 to Textures.Count-1 do buffers[i]:=GL_COLOR_ATTACHMENT0_EXT+i;
       glDrawBuffers(Textures.Count,@buffers[0]);
    end else begin glDrawBuffer(GL_NONE);  glReadBuffer(GL_NONE); end;
    if Textures.Count>0 then cb:= GL_COLOR_BUFFER_BIT else cb:=0;
    if rbDepth in FRenderBuffers then cb:= cb or GL_DEPTH_BUFFER_BIT;
    if rbStencil in FRenderBuffers then cb:= cb or GL_STENCIL_BUFFER_BIT;
    if cb<>0 then glClear(cb);
  end;
end;

procedure TFrameBufferObject.UnApply;
var tex: TGLTexture;
    i,n:integer;
    FBTarget: GLEnum;
begin
//  FBTarget:=GL_DRAW_FRAMEBUFFER_EXT;
  FBTarget:=GL_FRAMEBUFFER_EXT;
  for i:=0 to FReadBackBuffers.Count-1 do begin
    n:=integer(FReadBackBuffers[i]);
    if n<FAttachments.Textures.Count then begin
      glReadBuffer(GL_COLOR_ATTACHMENT0_EXT+n);
      tex:=FAttachments.Textures[n];
      glBindBuffer(GL_PIXEL_PACK_BUFFER, tex.PBOReadBuffer);
      glReadPixels(0,0, FWidth, FHeight, tex.PixelOGLFormat, tex.PrecisionOGLFormat, nil);
    end;
  end;

  glBindFramebufferEXT(FBTarget, 0);
  glReadBuffer(GL_BACK); glDrawBuffer(GL_BACK);
  glViewport(Fviewport[0],Fviewport[1],Fviewport[2],Fviewport[3]);
  with FAttachments do begin
    for i:=0 to Textures.Count-1 do begin
      tex:=Textures[i];
      if tex.Mipmapping then begin
        glBindTexture(cTexTargets[tex.Target], tex.Handle);
        glGenerateMipmapEXT(cTexTargets[tex.Target]);
        glBindTexture(cTexTargets[tex.Target],0);
      end;
    end;
    if DepthBuffer.Mode=bmTexture then begin
      tex:=DepthBuffer.Texture;
      if tex.Mipmapping then begin
        glBindTexture(cTexTargets[tex.Target], tex.Handle);
        glGenerateMipmapEXT(cTexTargets[tex.Target]);
        glBindTexture(cTexTargets[tex.Target],0);
      end;
    end;
    if StencilBuffer.Mode=bmTexture then begin
      tex:=StencilBuffer.Texture;
      if tex.Mipmapping then begin
        glBindTexture(cTexTargets[tex.Target], tex.Handle);
        glGenerateMipmapEXT(cTexTargets[tex.Target]);
        glBindTexture(cTexTargets[tex.Target],0);
      end;
    end;
  end;
end;

procedure TFrameBufferObject.SetReadBackBuffer(
  const ColorBufers: array of GLUint);
var i:integer;
begin
  FReadBackBuffers.Clear;
  for i:=0 to high(ColorBufers) do FReadBackBuffers.Add(pointer(ColorBufers[i]));
end;

end.
