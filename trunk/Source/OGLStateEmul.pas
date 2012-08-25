{: OGLStateEmul
	Historique:
        27/09/10 - Fantom - Исправлена критическая ошибка в работе glMaterial*
}

{TODO:
      - Доделать эмуляцию glBegin/End
      - Добавить в TFFPVertexAttrib.PutAttrib создание новго атрибута при пустом списке
      + Закешировать работу с матрицами (Push/Pop,Multiple,Load,Identity)
      ----убрать в Pop/Scale/Translate/Rotate явную загрузку через glLoadMatrix,
      ----перенести проверку загруженности матрицы на команды рисования
      - Закешировать функции glGet*,glIs*, glEnable/Disable
      - Закешировать атрибуты glPush/PopAttrib
      - Закешировать настройки примитивов (толщина линии, направление обхода вершин и т.д.)
      - Заменить glBegin/End на glBeginVBO/glEndVBO
      - Реализовать стек контекстов, привязаться к активному
      - Привязать ClientState к вызову gl*Pointer
      - Добавить специальные атрибуты к существующим функциям (glGet*(SE_Texture_Target))
      - Возможно стоит распределить работу с кешем между менеджерами?
      - Сделать список обновляемых стэйтов (при первом обращении к функции заданной групы
        помещаем в список константу говорящую что в дальнейшем требуется обновление.
        Если обновления небыло - принудительно прочитать(или установить) стэйты данной
        группы с занесением в список обновления. При последующем обновлении вызывать
        обновление лишь нужных стэйтов
}

unit OGLStateEmul;

interface

Uses   OpenGL1x, VectorTypes, VectorGeometry, Classes, SysUtilsLite, uLogs;

Type
   TEnStates = (sAlphaTest,sAutoNormal,sBlend,sColorLogicOp,sColorMaterial,sColorSum,
                sCullFace,sDepthTest,sDither,
                sFog,sIndexLogicOp,sLighting,sLineSmooth,sLineStipple,
                sMultisample,sNormalize,sPointSmooth,sPointSprite,sRescaleNormal,
                sSampleAlphaToCoverage, sScissorTest,sStencilTest,sVertexProgramPointSize,
                sVertexProgramTwoSide,sNone);
   TIntegerVector = array[0..3] of integer;
   PIntegerVector = ^TIntegerVector;

   TGLStateCache = class;

   TGLDepthCache = class
     private
       FChecked: boolean;
     public
       DepthMask: boolean;
       DepthFunc: GLEnum;
       DepthRange: GLEnum;
       DepthNear: single;
       DepthFar: single;
       DepthScale: single;
       DepthBias: single;
       DepthBits: integer;
       DepthClearValue: single;
       Enabled: boolean;
       procedure Reset;
       procedure GetCurrentGLStates;
       procedure Assign(DepthCache: TGLDepthCache);
       procedure StoreTo(DepthCache: TGLDepthCache);

   end;

   TGLAlphaCache = class
     private
       FChecked: boolean;
     public
       Enabled: boolean;
       Func: TGLEnum;
       Ref: single;
       procedure Reset;
       procedure Assign(AlphaCache: TGLAlphaCache);
       procedure StoreTo(AlphaCache: TGLAlphaCache);
       procedure GetCurrentGLStates;
   end;

   TGLBlendingCache = class
     private
       FChecked: boolean;
     public
       Enabled: boolean;
       SFactor: TGLEnum;
       DFactor: TGLEnum;
       BlendColor: TVector;
       BlendDstAlpha: TGLEnum;
       BlendDstRGB: TGLEnum;
       BlendSrcAlpha: TGLEnum;
       BlendSrcRGB: TGLEnum;
       BlendEquation: TGLEnum;
       BlendEquationAlpha: TGLEnum;
       BlendEquationRGB: TGLEnum;
       procedure Reset;
       procedure Assign(BlendingCache: TGLBlendingCache);
       procedure StoreTo(BlendingCache: TGLBlendingCache);
       procedure GetCurrentGLStates;
   end;

   TGLMaterialCache = class
     private
       FChecked: boolean;
     public
       Faces:array[GL_FRONT..GL_BACK] of record
         AMBIENT  : TVector;
         iAMBIENT : TIntegerVector;
         DIFFUSE  : TVector;
         iDIFFUSE : TIntegerVector;
         SPECULAR : TVector;
         iSPECULAR: TIntegerVector;
         EMISSION : TVector;
         iEMISSION: TIntegerVector;
         SHININESS: single;
         iSHININESS: integer;
         ColorMaterialMode: GLEnum;
       end;
       FaceMode: GLEnum;
       CurrentFace: GLEnum;
       procedure Reset;
       procedure Assign(MaterialCache: TGLMaterialCache);
       procedure StoreTo(MaterialCache: TGLMaterialCache);
       procedure GetCurrentGLStates;
   end;

   TGLLightingCache = class
     private
       FChecked: boolean;
     public
       MaxLights: integer;
       Enabled: boolean;
       Lights: array of record
          Enabled: boolean;
          SPOT_EXPONENT: single;
          SPOT_CUTOFF: single;
          CONSTANT_ATTENUATION: single;
          LINEAR_ATTENUATION: single;
          QUADRATIC_ATTENUATION: single;
          AMBIENT: TVector;
          DIFFUSE: TVector;
          SPECULAR: TVector;
          POSITION: TVector;
          SPOT_DIRECTION: TVector;
       end;
       LM_AMBIENT: TVector;
       LM_LOCAL_VIEWER: boolean;
       LM_COLOR_CONTROL: GLInt;
       LM_TWO_SIDE: boolean;
       procedure Reset;
       procedure Assign(LightingCache: TGLLightingCache);
       procedure StoreTo(LightingCache: TGLLightingCache);
       procedure GetCurrentGLStates;
   end;

   TGLTextureCache = class
     private
       FChecked: boolean;
     public
       Texture1D: boolean;
       Texture2D: boolean;
       Texture3D: boolean;
       TextureCubeMap: boolean;
       TextureRectangle: boolean;
       Units: array of record
          Target: TGLEnum;
          ID: GLUint;
       end;
       CurrentUnit: byte;
       CurrentClientUnit: byte;
       procedure Reset;
       procedure Assign(TextureCache: TGLTextureCache);
       procedure StoreTo(TextureCache: TGLTextureCache);
       procedure GetCurrentGLStates;
   end;

   TGLMatrixCache = class
     private
       FChecked: boolean;
     public
       ModelMatrix: TList;
       ProjectionMatrix: TList;
       TextureMatrix: TList;
       ColorMatrix: TList;
       MatrixMode: GLEnum;
       Current: record
          ModelViewMatrix: TMatrix;
          MVLoaded: boolean;
          TextureMatrix: TMatrix;
          TMLoaded: boolean;
          ProjectionMatrix: TMatrix;
          PMLoaded: boolean;
          ColorMatrix: TMatrix;
          CMLoaded: boolean;
       end;
       constructor Create;
       destructor Destroy; override;
       procedure Reset;
       procedure GetCurrentGLStates;

   end;

   TGLShaderCache = class
   private
     FShadersStack: array [0..32] of cardinal;
     FStackTop: integer;
     procedure Clear;
   public
     procedure Push(ShaderId: Cardinal);
     function Pop: cardinal;
     function Last: cardinal;
     constructor Create;
     procedure Reset;
     procedure GetCurrentGLStates;
     procedure Assign(ShaderCache: TGLShaderCache);
     procedure StoreTo(ShaderCache: TGLShaderCache);
   end;


   TGLStencilCache = record
     StencilMask: GLUint;
     StencilFunc: GLEnum;
     StencilRef : GLInt;
     StencilANDMask: GLUint;
   end;
   TGLClipPlanesCache = record
     ClipPlane: array of record
        Enabled: boolean;
        Equation: GLEnum;
     end;
   end;
   TGLFrontFaceCache = record
      Dir: (ffCW,ffCCW);
   end;
   TGLLineChache = record
     Smooth: boolean;
     Stripple: boolean;
     Mask: word;
   end;
   TGLMapCache = record
     Maps: array[0..1] of record
       Enabled: boolean;
       Color4: boolean;
       Index: boolean;
       Normal: boolean;
       TexCoords: array[1..4] of boolean;
       Vertex3: boolean;
       Vertex4: boolean;
     end;
   end;
   TGLPolygonChache = record
     PolygonOffsetFill: boolean;
     PolygonOffsetLine: boolean;
     PolygonOffsetPoint: boolean;
     Smooth: boolean;
     Stripple: boolean;
   end;

   TGLTextureGenCache = record
     TextureGenQ: boolean;
     TextureGenR: boolean;
     TextureGenS: boolean;
     TextureGenT: boolean;
   end;


   TFFPAttrType = (ffVertex, ffNormal,ffColor,ffTexCoord0,ffTexCoord1,ffTexCoord2,
     ffTexCoord3,ffTexCoord4,ffTexCoord5,ffTexCoord6,ffTexCoord7);
   TFFPAttrTypes = set of TFFPAttrType;

   TFFPVBOAttr = record
    CCount: integer;
    CSize: integer;
    CType: cardinal;
    COffs: cardinal;
    Loc: integer;
    AttrType: TFFPAttrType;
   end;

   TGLFFPBuffer = record
     DrawCallId: cardinal;
     PrimType: cardinal;
     VBOId: cardinal;
     BuffData: pointer;
     BuffSize: integer;
     ActiveFFPAttr: TFFPAttrTypes;
     Attribs: array of TFFPVBOAttr;
   end;
   PGLFFPBuffer = ^TGLFFPBuffer;

   TFFPVertexAttrib = class
   private
     fUsed: TFFPAttrTypes;
     fVertex, fNormal: TVector3f;
     fColor: TVector4f;
     fTexCoords: array[0..7] of TVector4f;
     vcCount: byte;
     tcCount: array[0..7] of byte;
     function FindAttr (const buff: PGLFFPBuffer; AType: TFFPAttrType): integer;
     procedure PutFloat(const SourceBuff; var DestBuff: pointer; Count: byte);
   public
     constructor Create;
     procedure PutAttrib(var Buff: PGLFFPBuffer);

     procedure Vertex3f(x,y,z: single);
     procedure Vertex3fv(v: PAffineVector);
     procedure Vertex2f(x,y: single);
     procedure Vertex2fv(v: PTexPoint);

     procedure Normal3f(x,y,z: single);
     procedure Normal3fv(v: PAffineVector);

     procedure Color3f(r,g,b: single);
     procedure Color3fv(c: PAffineVector);
     procedure Color4f(r,g,b,a: single);
     procedure Color4fv(c: PVector);

     procedure TexCoord1f(s: single; Target: byte=0);
     procedure TexCoord2f(s,t: single; Target: byte=0);
     procedure TexCoord3f(s,t,p: single; Target: byte=0);
     procedure TexCoord4f(s,t,p,q: single; Target: byte=0);
     procedure TexCoord1fv(s: Psingle; Target: byte=0);
     procedure TexCoord2fv(v: PTexPoint; Target: byte=0);
     procedure TexCoord3fv(v: PAffineVector; Target: byte=0);
     procedure TexCoord4fv(v: PVector; Target: byte=0);

   end;

   TGLVBOManager = class
   private
     FVBOList: TList;
     FCurrentAttrib: TFFPVertexAttrib;
   public
     constructor Create;
     destructor Destroy;override;
     function GetBuffer(CallId: cardinal): PGLFFPBuffer;
   end;


   TGLStateCache = class
     private
      FChecked: boolean;
      FActiveProgram: cardinal;
      States: set of TEnStates;
      MatrixCache  : TGLMatrixCache;
      FMaterialCache: TGLMaterialCache;
      FBlendingCache: TGLBlendingCache;
      FAlphaCache   : TGLAlphaCache;
      FDepthCache   : TGLDepthCache;
      FTextureCache : TGLTextureCache;
      FLightingCache: TGLLightingCache;
      FShaderCache  : TGLShaderCache;
      StencilCache : TGLStencilCache;
      LineChache   : TGLLineChache;
      MapCache     : TGLMapCache;
      PolygonChache: TGLPolygonChache;
      ClipPlanesCache: TGLClipPlanesCache;
      TextureGenCache: TGLTextureGenCache;
      FGLVBOManager: TGLVBOManager;
     public
      procedure CheckStates;
      procedure ResetStates(CheckGLStates: boolean=false);
      procedure ReplaceStates(const StateCache: TGLStateCache);
      procedure PushStates;
      procedure PopStates;
      procedure Assign(StateCache: TGLStateCache);

      constructor Create;
      destructor Destroy;override;

      property MaterialCache: TGLMaterialCache read FMaterialCache;
      property BlendingCache: TGLBlendingCache read FBlendingCache;
      property AlphaCache   : TGLAlphaCache read FAlphaCache;
      property DepthCache   : TGLDepthCache read FDepthCache;
      property TextureCache : TGLTextureCache read FTextureCache;
      property LightingCache: TGLLightingCache read FLightingCache;
      property ShaderCache : TGLShaderCache read FShaderCache;
      property ActiveProgram: cardinal read FActiveProgram;
      property GLVBOManager: TGLVBOManager read FGLVBOManager;
    end;

Const
   CEnStates: array[sAlphaTest..sNone] of GLEnum =
   (GL_Alpha_Test,GL_Auto_Normal,GL_Blend,GL_Color_Logic_Op,GL_Color_Material,GL_Color_Sum,
    GL_Cull_Face,GL_Depth_Test,GL_Dither,
    GL_Fog,GL_Index_Logic_Op,GL_Lighting,GL_Line_Smooth,GL_Line_Stipple,
    GL_Multisample,GL_Normalize,GL_Point_Smooth,GL_Point_Sprite,GL_Rescale_Normal,
    GL_Sample_Alpha_To_Coverage,GL_Scissor_Test,GL_Stencil_Test,GL_Vertex_Program_Point_Size,
    GL_Vertex_Program_Two_Side,0);

   cDiffuseColor: TVector = (0.8,0.8,0.8,1);
   cAmbientColor: TVector = (0.2,0.2,0.2,1);
   cSpecularColor: TVector = (0,0,0,1);
   cEmissiveColor: TVector = (0,0,0,1);
   cShininess: single = 0;

   cMultiTexCoords: array[0..7] of TFFPAttrType =
     (ffTexCoord0,ffTexCoord1,ffTexCoord2,ffTexCoord3,ffTexCoord4,ffTexCoord5,
      ffTexCoord6,ffTexCoord7);
   cMultiTexIndex: array[ffTexCoord0..ffTexCoord7] of byte =
     (0,1,2,3,4,5,6,7);

function GetActiveContext: LongInt;

function glIsEnabled(cap: GLEnum): boolean;
procedure glEnable(cap: TGLEnum);
procedure glDisable(cap: TGLEnum);

procedure glAlphaFunc(func: TGLEnum; ref: TGLclampf);

procedure glBlendFunc(sfactor: TGLEnum; dfactor: TGLEnum);
procedure glBlendColor(red, green, blue, alpha: TGLclampf);
procedure glBlendEquation(mode: TGLEnum);
procedure glBlendFuncSeparate(sfactorRGB, dfactorRGB, sfactorAlpha, dfactorAlpha: TGLenum);
procedure glBlendEquationSeparate(modeRGB: TGLenum; modeAlpha: TGLenum);

procedure glClearDepth(depth: TGLclampd);
procedure glDepthFunc(func: TGLEnum);
procedure glDepthRange(zNear, zFar: TGLclampd);
procedure glDepthMask(flag: TGLboolean);

procedure glPixelTransferf(pname: TGLEnum; param: TGLfloat);
procedure glPixelTransferi(pname: TGLEnum; param: TGLint);

procedure glGetMaterialfv(face, pname: TGLEnum; params: PGLfloat);
procedure glGetMaterialiv(face, pname: TGLEnum; params: PGLint);
procedure glMateriali(face, pname: TGLEnum; param: TGLint);
procedure glMaterialiv(face, pname: TGLEnum; params: PGLint);
procedure glMaterialfv(face, pname: TGLEnum; params: PGLfloat);
procedure glMaterialf(face, pname: TGLEnum; param: TGLfloat);

procedure glColorMaterial(face: TGLEnum; mode: TGLEnum);

procedure glLightModelf(pname: TGLEnum; param: TGLfloat);
procedure glLightModelfv(pname: TGLEnum; params: PGLfloat);
procedure glLightModeli(pname: TGLEnum; param: TGLint);
procedure glLightModeliv(pname: TGLEnum; params: PGLint);
procedure glLightf(light, pname: TGLEnum; param: TGLfloat);
procedure glLightfv(light, pname: TGLEnum; params: PGLfloat);
procedure glLighti(light, pname: TGLEnum; param: TGLint);
procedure glLightiv(light, pname: TGLEnum; params: PGLint);
procedure glGetLightfv(light, pname: TGLEnum; params: PGLfloat);
procedure glGetLightiv(light, pname: TGLEnum; params: PGLint);

procedure glBindTexture(target: TGLEnum; texture: TGLuint);
procedure glActiveTexture(target: TGLenum);
procedure glClientActiveTexture(target: TGLenum);

procedure glPushMatrix;
procedure glPopMatrix;
procedure glPushShader;
procedure glPopShader;

procedure glUseProgram(_program: TGLuint);

procedure glBegin(mode: TGLEnum);
procedure glVertex3f(x, y, z: TGLfloat);

{
procedure glScalef(x, y, z: TGLfloat);
procedure glTranslatef(x, y, z: TGLfloat);
procedure glRotatef(angle, x, y, z: TGLfloat);

procedure glMultMatrixf(m: PGLfloat);
procedure glLoadIdentity;
procedure glLoadMatrixf(m: PGLfloat);
}
{
   procedure glGetBooleanv(pname: TGLEnum; params: PGLboolean);
   procedure glGetDoublev(pname: TGLEnum; params: PGLdouble);
   procedure glGetFloatv(pname: TGLEnum; params: PGLfloat);
   procedure glGetPointerv(pname: TGLEnum; var params);
}
procedure glGetIntegerv(pname: TGLEnum; params: PGLint);

var GLStateCache: TGLStateCache;
    StateStack: TList;
    MatrixStackDepth: integer =0;

implementation

var CurrentFFPVBO: PGLFFPBuffer;

procedure glPushMatrix;
begin
  inc(MatrixStackDepth);
  Opengl1x.glPushMatrix;
end;
procedure glPopMatrix;
begin
  dec(MatrixStackDepth);
  assert(MatrixStackDepth>=0,'Matrix Stack is Empty');
  Opengl1x.glPopMatrix;
end;

procedure glPushShader;
begin
  GLStateCache.ShaderCache.Push(GLStateCache.FActiveProgram);
end;

procedure glPopShader;
var spId: cardinal;
begin
  spId:=GLStateCache.ShaderCache.Pop;
  if spId<>GLStateCache.FActiveProgram then begin
    OpenGL1x.glUseProgram(spId); GLStateCache.FActiveProgram:=spId;
  end;
end;

function GetActiveContext: LongInt;
begin
  result:=OpenGL1x.wglGetCurrentContext;
end;

procedure FreeStack(var Stack: TList);
var i:integer;
    temp: TGLStateCache;
begin
   if not assigned(Stack) then exit;
   while Stack.Count>0 do begin
      temp:=Stack.Last; temp.Free;
      Stack.Delete(Stack.Count-1);
//      GLStateCache.PopStates;
   end;
   Stack.Free;
{   for i:=0 to Stack.Count-1 do begin
      //GLStateCache.PopStates;
      temp:=Stack[i]; temp.Free;
   end; Stack.Free;}
end;

procedure FreeList(var List: TList);
var p:pointer;
    i:integer;
begin
   if not assigned(List) then exit;
   for i:=0 to List.count-1 do begin
      p:=List[i]; dispose(p);
   end; List.Free; List:=nil;
end;

function FloatToIntVector(const V: TVector): TIntegerVector;
begin
  result[0]:=trunc(v[0]*255); result[1]:=trunc(v[1]*255);
  result[2]:=trunc(v[2]*255); result[3]:=trunc(v[3]*255);
end;

function IntToFloatVector(const V: TIntegerVector): TVector;
begin
  result[0]:=(v[0]/255); result[1]:=(v[1]/255);
  result[2]:=(v[2]/255); result[3]:=(v[3]/255);
end;

function VectorEquals(v1,v2: TIntegerVector):boolean; overload;
begin
  result:=(v1[0]=v2[0]) and (v1[1]=v2[1]) and
          (v1[2]=v2[2]) and (v1[3]=v2[3]);
end;
procedure TGLDepthCache.Assign(DepthCache: TGLDepthCache);
begin
   FChecked:=DepthCache.FChecked;
   if DepthCache.Enabled then glEnable(GL_DEPTH_TEST)
   else glDisable(GL_DEPTH_TEST);
   glDepthMask(DepthCache.DepthMask);
   glDepthFunc(DepthCache.DepthFunc);
   glDepthRange(DepthCache.DepthNear,DepthCache.DepthFar);
   Opengl1x.glGetIntegerv(GL_DEPTH_RANGE, @DepthRange);
   glPixelTransferf(GL_DEPTH_SCALE,DepthCache.DepthScale);
   glPixelTransferf(GL_DEPTH_BIAS,DepthCache.DepthBias);
   glClearDepth(DepthCache.DepthClearValue);
   DepthBits:=DepthCache.DepthBits;
end;

procedure TGLDepthCache.GetCurrentGLStates;
begin
  Enabled:=Opengl1x.glisEnabled(GL_DEPTH_TEST);
  if Enabled then GLStateCache.States:=GLStateCache.States+[sDepthTest];
  Opengl1x.glGetIntegerv(GL_DEPTH_WRITEMASK, @DepthMask);
  Opengl1x.glGetIntegerv(GL_DEPTH_FUNC, @DepthFunc);
  Opengl1x.glGetIntegerv(GL_DEPTH_RANGE, @DepthRange);
  Opengl1x.glGetFloatv(GL_DEPTH_SCALE, @DepthScale);
  Opengl1x.glGetFloatv(GL_DEPTH_BIAS, @DepthBias);
  Opengl1x.glGetFloatv(GL_DEPTH_CLEAR_VALUE, @DepthClearValue);
  Opengl1x.glGetIntegerv(GL_DEPTH_BITS, @DepthBits);
  FChecked:=true;
end;

procedure TGLDepthCache.Reset;
begin
  glDisable(GL_DEPTH_TEST);
  glDepthMask(DepthMask);
  glDepthFunc(GL_LESS);
  glDepthRange(0,1);
  glPixelTransferf(GL_DEPTH_SCALE,1);
  glPixelTransferf(GL_DEPTH_BIAS,0);
  glClearDepth(1);
  DepthNear:=0;
  DepthFar:=1;
  DepthBits:=24;
  FChecked:=true;
end;

procedure TGLStateCache.Assign(StateCache: TGLStateCache);
begin
  DepthCache.Assign(StateCache.DepthCache);
  AlphaCache.Assign(StateCache.AlphaCache);
  BlendingCache.Assign(StateCache.BlendingCache);
  MaterialCache.Assign(StateCache.MaterialCache);
  LightingCache.Assign(StateCache.LightingCache);
  TextureCache.Assign(StateCache.TextureCache);
  ShaderCache.Assign(StateCache.ShaderCache);
  glUseProgram(StateCache.FActiveProgram);
end;

procedure TGLStateCache.CheckStates;
var state: TEnStates;
begin
  states:=[];
  for state := sAlphaTest to sVertexProgramTwoSide do begin
      if Opengl1x.glisEnabled(CEnStates[state]) then
         States:=states+[state];
  end;
  DepthCache.GetCurrentGLStates;
  AlphaCache.GetCurrentGLStates;
  BlendingCache.GetCurrentGLStates;
  MaterialCache.GetCurrentGLStates;
  LightingCache.GetCurrentGLStates;
  TextureCache.GetCurrentGLStates;
  ShaderCache.GetCurrentGLStates;
  FChecked:=true;
end;

procedure TGLDepthCache.StoreTo(DepthCache: TGLDepthCache);
begin
  DepthCache.FChecked:=FChecked;
  DepthCache.DepthMask:=DepthMask;
  DepthCache.DepthFunc:=DepthFunc;
  DepthCache.DepthRange:=DepthRange;
  DepthCache.DepthNear:=DepthNear;
  DepthCache.DepthFar:=DepthFar;
  DepthCache.DepthScale:=DepthScale;
  DepthCache.DepthBias:=DepthBias;
  DepthCache.DepthBits:=DepthBits;
  DepthCache.DepthClearValue:=DepthClearValue;
  DepthCache.Enabled:=Enabled;
end;

function StateByEnum(state: GLEnum):TEnStates;
begin
  for result:=sAlphaTest to sNone do
    if CEnStates[result]=state then exit;
  result:=sNone;
end;

function GetTextureStates(state: GLEnum): boolean;
begin
  with GLStateCache.TextureCache do begin
   case state  of
     GL_Texture_1D: result:=Texture1D;
     GL_Texture_2D: result:=Texture2D;
     GL_Texture_3D: result:=Texture3D;
     GL_TEXTURE_CUBE_MAP: result:=TextureCubeMap;
     GL_TEXTURE_RECTANGLE: result:=TextureRectangle;
     else result:=Opengl1x.glIsEnabled(state);
   end;
  end;
end;
procedure SetTextureStates(state: GLEnum; value:boolean);
begin
  if GetTextureStates(state)<>Value then
  with GLStateCache.TextureCache do begin
    case state  of
     GL_Texture_1D: Texture1D:=value;
     GL_Texture_2D: Texture2D:=value;
     GL_Texture_3D: Texture3D:=value;
     GL_TEXTURE_CUBE_MAP: TextureCubeMap:=value;
     GL_TEXTURE_RECTANGLE: TextureRectangle:=value;
    end;
    if Value then Opengl1x.glEnable(state) else Opengl1x.glDisable(state);
  end;
end;

function glIsEnabled(cap: GLEnum): boolean;
var state: TEnStates;
begin
  state:=StateByEnum(cap);
  if state<>sNone then begin
     result:=state in GLStateCache.States;
     exit;
  end;
  case cap of
    GL_Texture_1D,GL_Texture_2D,GL_Texture_3D,GL_TEXTURE_CUBE_MAP,
    GL_TEXTURE_RECTANGLE: begin
       result:=GetTextureStates(cap);exit;
    end;
  end;
  result:=Opengl1x.glIsEnabled(cap);
end;

procedure glEnable(cap: TGLEnum);
var state: TEnStates;
begin
  state:=StateByEnum(cap);
  if state<>sNone then begin
     if not (state in GLStateCache.States) then begin
        Opengl1x.glEnable(cap);
        GLStateCache.States:=GLStateCache.States+[state];
        exit;
     end else exit;
  end;
  case cap of
    GL_Texture_1D,GL_Texture_2D,GL_Texture_3D,GL_TEXTURE_CUBE_MAP,
    GL_TEXTURE_RECTANGLE: begin
       SetTextureStates(cap,true);exit;
    end;
  end;
  Opengl1x.glEnable(cap);
end;

procedure glDisable(cap: TGLEnum);
var state: TEnStates;
begin
  state:=StateByEnum(cap);
  if state<>sNone then begin
     if (state in GLStateCache.States) then begin
        Opengl1x.glDisable(cap);
        GLStateCache.States:=GLStateCache.States-[state];
        exit;
     end else exit;
  end;
  case cap of
    GL_Texture_1D,GL_Texture_2D,GL_Texture_3D,GL_TEXTURE_CUBE_MAP,
    GL_TEXTURE_RECTANGLE: begin
       SetTextureStates(cap,false);
       exit;
    end;
  end;
  Opengl1x.glDisable(cap);
end;

procedure glClearDepth(depth: TGLclampd);
begin
  if GLStateCache.DepthCache.DepthClearValue<>depth then
  begin
     Opengl1x.glClearDepth(depth);
     GLStateCache.DepthCache.DepthClearValue:=depth;
  end;
end;

procedure glDepthFunc(func: TGLEnum);
begin
  if GLStateCache.DepthCache.DepthFunc<>func then
  begin
     Opengl1x.glDepthFunc(func);
     GLStateCache.DepthCache.DepthFunc:=func;
  end;
end;

procedure glDepthRange(zNear, zFar: TGLclampd);
begin
  if (GLStateCache.DepthCache.DepthNear<>zNear)
  or (GLStateCache.DepthCache.DepthFar<>zFar)then
  begin
     Opengl1x.glDepthRange(zNear, zFar);
     GLStateCache.DepthCache.DepthNear:=zNear;
     GLStateCache.DepthCache.DepthFar:=zFar;
  end;
end;

procedure glDepthMask(flag: TGLboolean);
begin
  if GLStateCache.DepthCache.DepthMask<>flag then
  begin
     Opengl1x.glDepthMask(flag);
     GLStateCache.DepthCache.DepthMask:=flag;
  end;
end;

procedure glPixelTransferf(pname: TGLEnum; param: TGLfloat);
{
Specifies the symbolic name of the pixel transfer parameter to be set.
            GL_MAP_COLOR,
            GL_MAP_STENCIL,
            GL_INDEX_SHIFT,
            GL_INDEX_OFFSET,
            GL_RED_SCALE,
            GL_RED_BIAS,
            GL_GREEN_SCALE,
            GL_GREEN_BIAS,
            GL_BLUE_SCALE,
            GL_BLUE_BIAS,
            GL_ALPHA_SCALE,
            GL_ALPHA_BIAS,

            Additionally, if the ARB_imaging extension is supported, the
            following symbolic names are accepted:
            GL_POST_COLOR_MATRIX_RED_SCALE,
            GL_POST_COLOR_MATRIX_GREEN_SCALE,
            GL_POST_COLOR_MATRIX_BLUE_SCALE,
            GL_POST_COLOR_MATRIX_ALPHA_SCALE,
            GL_POST_COLOR_MATRIX_RED_BIAS,
            GL_POST_COLOR_MATRIX_GREEN_BIAS,
            GL_POST_COLOR_MATRIX_BLUE_BIAS,
            GL_POST_COLOR_MATRIX_ALPHA_BIAS,
            GL_POST_CONVOLUTION_RED_SCALE,
            GL_POST_CONVOLUTION_GREEN_SCALE,
            GL_POST_CONVOLUTION_BLUE_SCALE,
            GL_POST_CONVOLUTION_ALPHA_SCALE,
            GL_POST_CONVOLUTION_RED_BIAS,
            GL_POST_CONVOLUTION_GREEN_BIAS,
            GL_POST_CONVOLUTION_BLUE_BIAS, and
            GL_POST_CONVOLUTION_ALPHA_BIAS.
}
begin
  case pname of
     GL_DEPTH_SCALE: if param<>GLStateCache.DepthCache.DepthScale then
        begin
           GLStateCache.DepthCache.DepthScale:=param;
           Opengl1x.glPixelTransferf(pname, param); exit;
        end;
     GL_DEPTH_BIAS: if param<>GLStateCache.DepthCache.DepthBias then
        begin
           GLStateCache.DepthCache.DepthBias:=param;
           Opengl1x.glPixelTransferf(pname, param); exit;
        end;
  end;
  Opengl1x.glPixelTransferf(pname, param);
end;

procedure glPixelTransferi(pname: TGLEnum; param: TGLint);
begin
  case pname of
     GL_DEPTH_SCALE: if param<>GLStateCache.DepthCache.DepthScale then begin
           GLStateCache.DepthCache.DepthScale:=param;
           Opengl1x.glPixelTransferi(pname, param); exit;
        end;
     GL_DEPTH_BIAS: if param<>GLStateCache.DepthCache.DepthBias then begin
           GLStateCache.DepthCache.DepthBias:=param;
           Opengl1x.glPixelTransferi(pname, param); exit;
        end;
  end;
  Opengl1x.glPixelTransferi(pname, param);
end;

{ TGLAlphaCache }

procedure TGLAlphaCache.Assign(AlphaCache: TGLAlphaCache);
begin
  if AlphaCache.Enabled then glEnable(GL_ALPHA_TEST)
  else glDisable(GL_ALPHA_TEST);
  glAlphaFunc(AlphaCache.Func, AlphaCache.Ref);
  FChecked:=true;
end;

procedure TGLAlphaCache.GetCurrentGLStates;
begin
  Enabled:=Opengl1x.glisEnabled(GL_ALPHA_TEST);
  if Enabled then GLStateCache.States:=GLStateCache.States+[sAlphaTest];
  Opengl1x.glGetIntegerv(GL_ALPHA_TEST_FUNC,@Func);
  Opengl1x.glGetFloatv(GL_ALPHA_TEST_REF,@Ref);
  FChecked:=true;
end;

procedure TGLAlphaCache.Reset;
begin
  glDisable(GL_ALPHA_TEST);
  GLStateCache.States:=GLStateCache.States-[sAlphaTest];
  glAlphaFunc(GL_ALWAYS, 0);
  FChecked:=true;
end;

procedure glAlphaFunc(func: TGLEnum; ref: TGLclampf);
begin
  if (GLStateCache.AlphaCache.Func<>func)
  or (GLStateCache.AlphaCache.Ref<>ref)
  then begin
    Opengl1x.glAlphaFunc(func, ref);
    GLStateCache.AlphaCache.Func:=func;
    GLStateCache.AlphaCache.Ref:=ref;
  end;
end;

procedure TGLAlphaCache.StoreTo(AlphaCache: TGLAlphaCache);
begin
  AlphaCache.FChecked:=FChecked;
  AlphaCache.Enabled:=Enabled;
  AlphaCache.Func:=Func;
  AlphaCache.Ref:=Ref;
end;

{ TGLBlendingCache }

procedure TGLBlendingCache.Assign(BlendingCache: TGLBlendingCache);
begin
  if BlendingCache.Enabled then glEnable(GL_BLEND)
  else glDisable(GL_BLEND);
  glBlendColor(BlendingCache.BlendColor[0],
               BlendingCache.BlendColor[1],
               BlendingCache.BlendColor[2],
               BlendingCache.BlendColor[3]);
  glBlendEquationSeparate(BlendingCache.BlendEquationRGB,BlendingCache.BlendEquationAlpha);
  glBlendFuncSeparate(BlendingCache.BlendSrcRGB,BlendingCache.BlendDstRGB,
                      BlendingCache.BlendSrcAlpha,BlendingCache.BlendDstAlpha);
  glBlendEquation(BlendingCache.BlendEquation);
  glBlendFunc(BlendingCache.SFactor, BlendingCache.DFactor);
  FChecked:=true;
end;

procedure TGLBlendingCache.GetCurrentGLStates;
begin
  Enabled:=Opengl1x.glisEnabled(GL_BLEND);
  if Enabled then GLStateCache.States:=GLStateCache.States+[sBlend];
  Opengl1x.glGetIntegerv(GL_BLEND_SRC,@SFactor);
  Opengl1x.glGetIntegerv(GL_BLEND_DST,@DFactor);
  Opengl1x.glGetFloatv(GL_BLEND_COLOR,@BlendColor);
  Opengl1x.glGetIntegerv(GL_BLEND_DST_ALPHA,@BlendDstAlpha);
  Opengl1x.glGetIntegerv(GL_BLEND_DST_RGB,@BlendDstRGB);
  Opengl1x.glGetIntegerv(GL_BLEND_SRC_ALPHA,@BlendSRCAlpha);
  Opengl1x.glGetIntegerv(GL_BLEND_SRC_RGB,@BlendSRCRGB);
  Opengl1x.glGetIntegerv(GL_BLEND_EQUATION_ALPHA,@BlendEquationAlpha);
  Opengl1x.glGetIntegerv(GL_BLEND_EQUATION_RGB,@BlendEquationRGB);
  Opengl1x.glGetIntegerv(GL_BLEND_EQUATION,@BlendEquation);
  FChecked:=true;
end;

procedure TGLBlendingCache.Reset;
begin
  glDisable(GL_BLEND);
  Enabled:=false;
  GLStateCache.States:=GLStateCache.States-[sBlend];
  glBlendColor(0,0,0,0);
  glBlendEquationSeparate(GL_FUNC_ADD,GL_FUNC_ADD);
  glBlendFuncSeparate(GL_ONE,GL_ZERO,GL_ONE,GL_ZERO);
  glBlendEquation(GL_FUNC_ADD);
  glBlendFunc(GL_ONE, GL_ZERO);
  FChecked:=true;
end;

procedure glBlendFunc(sfactor: TGLEnum; dfactor: TGLEnum);
begin
  if (GLStateCache.BlendingCache.SFactor<>sfactor)
  or (GLStateCache.BlendingCache.DFactor<>dfactor)
  then begin
    Opengl1x.glBlendFunc(sfactor, dfactor);
    GLStateCache.BlendingCache.SFactor:=sfactor;
    GLStateCache.BlendingCache.DFactor:=dfactor;
  end;
end;

procedure glBlendColor(red, green, blue, alpha: TGLclampf);
begin
  if not VectorEquals(GLStateCache.BlendingCache.BlendColor,
                      vectormake(red, green, blue, alpha))
  then begin
    Opengl1x.glBlendColor(red, green, blue, alpha);
    GLStateCache.BlendingCache.BlendColor:=vectormake(red, green, blue, alpha);
  end;
end;

procedure glBlendEquation(mode: TGLEnum);
begin
  if GLStateCache.BlendingCache.BlendEquation <> mode
  then begin
    Opengl1x.glBlendEquation(mode);
    GLStateCache.BlendingCache.BlendEquation:=mode;
  end;
end;

procedure glBlendFuncSeparate(sfactorRGB, dfactorRGB, sfactorAlpha, dfactorAlpha: TGLenum);
begin
  if (GLStateCache.BlendingCache.BlendSrcRGB<>sfactorRGB)
  or (GLStateCache.BlendingCache.BlendDstRGB<>dfactorRGB)
  or (GLStateCache.BlendingCache.BlendSrcAlpha<>sfactorAlpha)
  or (GLStateCache.BlendingCache.BlendDstAlpha<>dfactorAlpha)
  then begin
    Opengl1x.glBlendFuncSeparate(sfactorRGB, dfactorRGB, sfactorAlpha, dfactorAlpha);
    GLStateCache.BlendingCache.BlendSrcRGB:=sfactorRGB;
    GLStateCache.BlendingCache.BlendDstRGB:=dfactorRGB;
    GLStateCache.BlendingCache.BlendSrcAlpha:=sfactorAlpha;
    GLStateCache.BlendingCache.BlendDstAlpha:=dfactorAlpha;
  end;
end;

procedure glBlendEquationSeparate(modeRGB: TGLenum; modeAlpha: TGLenum);
begin
  if (GLStateCache.BlendingCache.BlendEquationRGB <> modeRGB)
  or (GLStateCache.BlendingCache.BlendEquationAlpha <> modeAlpha)
  then begin
    Opengl1x.glBlendEquationSeparate(modeRGB, modeAlpha);
    GLStateCache.BlendingCache.BlendEquationRGB := modeRGB;
    GLStateCache.BlendingCache.BlendEquationAlpha := modeAlpha;
  end;
end;

procedure TGLBlendingCache.StoreTo(BlendingCache: TGLBlendingCache);
begin
  BlendingCache.FChecked:=FChecked;
  BlendingCache.Enabled:=Enabled;
  BlendingCache.SFactor:=SFactor;
  BlendingCache.DFactor:=DFactor;
  BlendingCache.BlendColor:=BlendColor;
  BlendingCache.BlendDstAlpha:=BlendDstAlpha;
  BlendingCache.BlendDstRGB:=BlendDstRGB;
  BlendingCache.BlendSrcAlpha:=BlendSrcAlpha;
  BlendingCache.BlendSrcRGB:=BlendSrcRGB;
  BlendingCache.BlendEquation:=BlendEquation;
  BlendingCache.BlendEquationAlpha:=BlendEquationAlpha;
  BlendingCache.BlendEquationRGB:=BlendEquationRGB;
end;

{ TGLMaterialCache }

procedure TGLMaterialCache.StoreTo(MaterialCache: TGLMaterialCache);
var i: GLEnum;
begin
  MaterialCache.FChecked:=FChecked;
  for i:=GL_FRONT to GL_BACK do
  with MaterialCache.Faces[i] do begin
         AMBIENT:=Faces[i].AMBIENT;
         iAMBIENT:=Faces[i].iAMBIENT;
         DIFFUSE:=Faces[i].DIFFUSE;
         iDIFFUSE:=Faces[i].iDIFFUSE;
         SPECULAR:=Faces[i].SPECULAR;
         iSPECULAR:=Faces[i].iSPECULAR;
         EMISSION:=Faces[i].EMISSION;
         iEMISSION:=Faces[i].iEMISSION;
         SHININESS:=Faces[i].SHININESS;
         iSHININESS:=Faces[i].iSHININESS;
         ColorMaterialMode:=Faces[i].ColorMaterialMode;
  end;
  MaterialCache.FaceMode:=FaceMode;
  MaterialCache.CurrentFace:=CurrentFace;
end;

procedure TGLMaterialCache.Assign(MaterialCache: TGLMaterialCache);
begin
  glMaterialfv(GL_FRONT, GL_AMBIENT,   @MaterialCache.Faces[GL_FRONT].Ambient);
  glMaterialfv(GL_FRONT, GL_DIFFUSE,   @MaterialCache.Faces[GL_FRONT].Diffuse);
  glMaterialfv(GL_FRONT, GL_SPECULAR,  @MaterialCache.Faces[GL_FRONT].Specular);
  glMaterialfv(GL_FRONT, GL_EMISSION,  @MaterialCache.Faces[GL_FRONT].Emission);
  glMaterialfv(GL_FRONT, GL_SHININESS, @MaterialCache.Faces[GL_FRONT].Shininess);

  glMaterialfv(GL_BACK, GL_AMBIENT,   @MaterialCache.Faces[GL_BACK].Ambient);
  glMaterialfv(GL_BACK, GL_DIFFUSE,   @MaterialCache.Faces[GL_BACK].Diffuse);
  glMaterialfv(GL_BACK, GL_SPECULAR,  @MaterialCache.Faces[GL_BACK].Specular);
  glMaterialfv(GL_BACK, GL_EMISSION,  @MaterialCache.Faces[GL_BACK].Emission);
  glMaterialfv(GL_BACK, GL_SHININESS, @MaterialCache.Faces[GL_BACK].Shininess);
  glColorMaterial(GL_FRONT, MaterialCache.Faces[GL_FRONT].ColorMaterialMode);
  glColorMaterial(GL_BACK, MaterialCache.Faces[GL_BACK].ColorMaterialMode);
  glColorMaterial(MaterialCache.CurrentFace, MaterialCache.FaceMode);
  FChecked:=true;
end;

procedure TGLMaterialCache.GetCurrentGLStates;
var Face, Mode: GLEnum;
begin
  Opengl1x.glGetMaterialfv(GL_FRONT, GL_AMBIENT,  @Faces[GL_FRONT].AMBIENT);
  Opengl1x.glGetMaterialfv(GL_FRONT, GL_DIFFUSE,  @Faces[GL_FRONT].DIFFUSE);
  Opengl1x.glGetMaterialfv(GL_FRONT, GL_SPECULAR, @Faces[GL_FRONT].SPECULAR);
  Opengl1x.glGetMaterialfv(GL_FRONT, GL_EMISSION, @Faces[GL_FRONT].EMISSION);
  Opengl1x.glGetMaterialfv(GL_FRONT, GL_SHININESS,@Faces[GL_FRONT].SHININESS);

  Opengl1x.glGetMaterialfv(GL_BACK, GL_AMBIENT, @Faces[GL_BACK].AMBIENT);
  Opengl1x.glGetMaterialfv(GL_BACK, GL_DIFFUSE, @Faces[GL_BACK].DIFFUSE);
  Opengl1x.glGetMaterialfv(GL_BACK, GL_SPECULAR, @Faces[GL_BACK].SPECULAR);
  Opengl1x.glGetMaterialfv(GL_BACK, GL_EMISSION, @Faces[GL_BACK].EMISSION);
  Opengl1x.glGetMaterialfv(GL_BACK, GL_SHININESS, @Faces[GL_BACK].SHININESS);

  Opengl1x.glGetIntegerv(GL_COLOR_MATERIAL_FACE, @Face);
  Opengl1x.glGetIntegerv(GL_COLOR_MATERIAL_PARAMETER, @Mode);
//  if Face = GL_FRONT_AND_BACK then begin
     Faces[GL_FRONT].ColorMaterialMode:=mode;
     Faces[GL_BACK].ColorMaterialMode:=mode;
//  end else Faces[face].ColorMaterialMode:=mode;
  FaceMode:=mode; CurrentFace:=face;

  Faces[GL_FRONT].iAMBIENT:=FloatToIntVector(Faces[GL_FRONT].AMBIENT);
  Faces[GL_FRONT].iDIFFUSE:=FloatToIntVector(Faces[GL_FRONT].DIFFUSE);
  Faces[GL_FRONT].iSPECULAR:=FloatToIntVector(Faces[GL_FRONT].SPECULAR);
  Faces[GL_FRONT].iEMISSION:=FloatToIntVector(Faces[GL_FRONT].EMISSION);
  Faces[GL_FRONT].iSHININESS:=trunc(Faces[GL_FRONT].SHININESS);

  Faces[GL_BACK].iAMBIENT:=FloatToIntVector(Faces[GL_BACK].AMBIENT);
  Faces[GL_BACK].iDIFFUSE:=FloatToIntVector(Faces[GL_BACK].DIFFUSE);
  Faces[GL_BACK].iSPECULAR:=FloatToIntVector(Faces[GL_BACK].SPECULAR);
  Faces[GL_BACK].iEMISSION:=FloatToIntVector(Faces[GL_BACK].EMISSION);
  Faces[GL_BACK].iSHININESS:=trunc(Faces[GL_BACK].SHININESS);

  FChecked:=true;
end;

procedure TGLMaterialCache.Reset;
begin
  glMaterialfv(GL_FRONT, GL_AMBIENT,   @cAmbientColor);
  glMaterialfv(GL_FRONT, GL_DIFFUSE,   @cDiffuseColor);
  glMaterialfv(GL_FRONT, GL_SPECULAR,  @cSpecularColor);
  glMaterialfv(GL_FRONT, GL_EMISSION,  @cEmissiveColor);
  glMaterialiv(GL_FRONT, GL_SHININESS, @cShininess);

  glMaterialfv(GL_BACK, GL_AMBIENT,   @cAmbientColor);
  glMaterialfv(GL_BACK, GL_DIFFUSE,   @cDiffuseColor);
  glMaterialfv(GL_BACK, GL_SPECULAR,  @cSpecularColor);
  glMaterialfv(GL_BACK, GL_EMISSION,  @cEmissiveColor);
  glMaterialiv(GL_BACK, GL_SHININESS, @cShininess);

  glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
  glDisable(GL_COLOR_MATERIAL);
  FChecked:=true;
end;

procedure glGetMaterialfv(face, pname: TGLEnum; params: PGLfloat);
var p:pointer;
begin
  p:=params;
  with GLStateCache.MaterialCache.Faces[face] do
  case pname of
    GL_AMBIENT: PVector(p)^:=AMBIENT;
    GL_DIFFUSE: PVector(p)^:=DIFFUSE;
    GL_SPECULAR: PVector(p)^:=SPECULAR;
    GL_EMISSION: PVector(p)^:=EMISSION;
    GL_SHININESS: params^:=SHININESS;
  end;
end;

procedure glGetMaterialiv(face, pname: TGLEnum; params: PGLint);
var p:pointer;
begin
  p:=params;
  with GLStateCache.MaterialCache.Faces[face] do
  case pname of
    GL_AMBIENT: PIntegerVector(p)^:=iAMBIENT;
    GL_DIFFUSE: PIntegerVector(p)^:=iDIFFUSE;
    GL_SPECULAR: PIntegerVector(p)^:=iSPECULAR;
    GL_EMISSION: PIntegerVector(p)^:=iEMISSION;
    GL_SHININESS: params^:=iSHININESS;
  end;
end;

procedure glMaterialf(face, pname: TGLEnum; param: TGLfloat);
begin
  if (face=GL_FRONT) or (face=GL_BACK) then begin
    case pname of
      GL_SHININESS: if GLStateCache.MaterialCache.Faces[face].SHININESS<>param then
                    begin
                     GLStateCache.MaterialCache.Faces[face].SHININESS:=trunc(param);
                     GLStateCache.MaterialCache.Faces[face].iSHININESS:=trunc(param);
                     Opengl1x.glMaterialf(face, pname, param);
                    end;
      Else Opengl1x.glMaterialf(face, pname, param);
    end;
  end else with GLStateCache.MaterialCache do begin
    assert(face=GL_FRONT_AND_BACK, 'Invalid Face Type');
    case pname of
      GL_SHININESS: if (Faces[GL_FRONT].SHININESS<>param)
                    or (Faces[GL_BACK].SHININESS<>param)then
                    begin
                      Faces[GL_FRONT].SHININESS:=trunc(param);
                      Faces[GL_FRONT].iSHININESS:=trunc(param);
                      Faces[GL_BACK].SHININESS:=trunc(param);
                      Faces[GL_BACK].iSHININESS:=trunc(param);
                      Opengl1x.glMaterialf(face, pname, param);
                    end;
      Else Opengl1x.glMaterialf(face, pname, param);
    end;
  end;
end;

procedure glMaterialfv(face, pname: TGLEnum; params: PGLfloat);
begin
  if (face=GL_FRONT) or (face=GL_BACK) then begin
    with GLStateCache.MaterialCache.Faces[face] do begin
    case pname of
      GL_AMBIENT: if not VectorEquals(AMBIENT, PVector(params)^) then begin
                     AMBIENT:=PVector(params)^;
                     iAMBIENT:=FloatTointVector(AMBIENT);
                     Opengl1x.glMaterialfv(face, pname, params);
                  end;
      GL_DIFFUSE: if not VectorEquals(DIFFUSE, PVector(params)^) then begin
                     DIFFUSE:=PVector(params)^;
                     iDIFFUSE:=FloatTointVector(DIFFUSE);
                     Opengl1x.glMaterialfv(face, pname, params);
                  end;
      GL_SPECULAR: if not VectorEquals(SPECULAR, PVector(params)^) then begin
                     SPECULAR:=PVector(params)^;
                     iSPECULAR:=FloatTointVector(SPECULAR);
                     Opengl1x.glMaterialfv(face, pname, params);
                  end;
      GL_EMISSION: if not VectorEquals(EMISSION, PVector(params)^) then begin
                     EMISSION:=PVector(params)^;
                     iEMISSION:=FloatTointVector(EMISSION);
                     Opengl1x.glMaterialfv(face, pname, params);
                  end;
      GL_SHININESS: if SHININESS<>params^ then begin
                     SHININESS:=trunc(params^); iSHININESS:=trunc(params^);
                     Opengl1x.glMaterialfv(face, pname, params);
                  end;
    end;
    end;
  end else begin
    assert(face=GL_FRONT_AND_BACK, 'Invalid Face Type');
    with GLStateCache.MaterialCache do begin
    case pname of
      GL_AMBIENT: if (not VectorEquals(Faces[GL_FRONT].AMBIENT, PVector(params)^))
                  or (not VectorEquals(Faces[GL_BACK].AMBIENT, PVector(params)^)) then begin
                     Faces[GL_FRONT].AMBIENT:=PVector(params)^;
                     Faces[GL_BACK].AMBIENT:=PVector(params)^;
                     Faces[GL_FRONT].iAMBIENT:=FloatTointVector(Faces[GL_FRONT].AMBIENT);
                     Faces[GL_BACK].iAMBIENT:=FloatTointVector(Faces[GL_BACK].AMBIENT);
                     Opengl1x.glMaterialfv(face, pname, params);
                  end;
      GL_DIFFUSE: if (not VectorEquals(Faces[GL_FRONT].DIFFUSE, PVector(params)^))
                  or (not VectorEquals(Faces[GL_BACK].DIFFUSE, PVector(params)^)) then begin
                     Faces[GL_FRONT].DIFFUSE:=PVector(params)^;
                     Faces[GL_BACK].DIFFUSE:=PVector(params)^;
                     Faces[GL_FRONT].iDIFFUSE:=FloatTointVector(Faces[GL_FRONT].DIFFUSE);
                     Faces[GL_BACK].iDIFFUSE:=FloatTointVector(Faces[GL_BACK].DIFFUSE);
                     Opengl1x.glMaterialfv(face, pname, params);
                  end;
      GL_SPECULAR:if (not VectorEquals(Faces[GL_FRONT].SPECULAR, PVector(params)^))
                  or (not VectorEquals(Faces[GL_BACK].SPECULAR, PVector(params)^)) then begin
                     Faces[GL_FRONT].SPECULAR:=PVector(params)^;
                     Faces[GL_BACK].SPECULAR:=PVector(params)^;
                     Faces[GL_FRONT].iSPECULAR:=FloatTointVector(Faces[GL_FRONT].SPECULAR);
                     Faces[GL_BACK].iSPECULAR:=FloatTointVector(Faces[GL_BACK].SPECULAR);
                     Opengl1x.glMaterialfv(face, pname, params);
                  end;
      GL_EMISSION:if (not VectorEquals(Faces[GL_FRONT].EMISSION, PVector(params)^))
                  or (not VectorEquals(Faces[GL_BACK].EMISSION, PVector(params)^)) then begin
                     Faces[GL_FRONT].EMISSION:=PVector(params)^;
                     Faces[GL_BACK].EMISSION:=PVector(params)^;
                     Faces[GL_FRONT].iEMISSION:=FloatTointVector(Faces[GL_FRONT].EMISSION);
                     Faces[GL_BACK].iEMISSION:=FloatTointVector(Faces[GL_BACK].EMISSION);
                     Opengl1x.glMaterialfv(face, pname, params);
                  end;
      GL_SHININESS:if (Faces[GL_FRONT].SHININESS<>params^)
                   or (Faces[GL_BACK].SHININESS<>params^) then begin
                     Faces[GL_FRONT].SHININESS:=trunc(params^);
                     Faces[GL_FRONT].iSHININESS:=trunc(params^);
                     Faces[GL_BACK].SHININESS:=trunc(params^);
                     Faces[GL_BACK].iSHININESS:=trunc(params^);
                     Opengl1x.glMaterialfv(face, pname, params);
                  end;
    end;
    end;
  end;
end;

procedure glMateriali(face, pname: TGLEnum; param: TGLint);
begin
  if (face=GL_FRONT) or (face=GL_BACK) then begin
    case pname of
      GL_SHININESS: if GLStateCache.MaterialCache.Faces[face].iSHININESS<>param then
                    begin
                     GLStateCache.MaterialCache.Faces[face].SHININESS:=param;
                     GLStateCache.MaterialCache.Faces[face].iSHININESS:=param;
                     Opengl1x.glMateriali(face, pname, param);
                    end;
      Else Opengl1x.glMateriali(face, pname, param);
    end;
  end else begin
    assert(face=GL_FRONT_AND_BACK, 'Invalid Face Type');
    with GLStateCache.MaterialCache do
    case pname of
      GL_SHININESS: if (Faces[GL_FRONT].iSHININESS<>param)
                    or (Faces[GL_BACK].iSHININESS<>param) then
                    begin
                     Faces[GL_FRONT].SHININESS:=param;
                     Faces[GL_FRONT].iSHININESS:=param;
                     Faces[GL_BACK].SHININESS:=param;
                     Faces[GL_BACK].iSHININESS:=param;
                     Opengl1x.glMateriali(face, pname, param);
                    end;
      Else Opengl1x.glMateriali(face, pname, param);
    end;

  end;
end;

procedure glMaterialiv(face, pname: TGLEnum; params: PGLint);
begin
  if (face=GL_FRONT) or (face=GL_BACK) then begin
    with GLStateCache.MaterialCache.Faces[face] do begin
    case pname of
      GL_AMBIENT: if not VectorEquals(iAMBIENT, PIntegerVector(params)^) then begin
                     iAMBIENT:=PIntegerVector(params)^;
                     AMBIENT:=IntToFloatVector(iAmbient);
                     Opengl1x.glMaterialiv(face, pname, params);
                  end;
      GL_DIFFUSE: if not VectorEquals(iDIFFUSE, PIntegerVector(params)^) then begin
                     iDIFFUSE:=PIntegerVector(params)^;
                     DIFFUSE:=IntToFloatVector(iDIFFUSE);
                     Opengl1x.glMaterialiv(face, pname, params);
                  end;
      GL_SPECULAR: if not VectorEquals(iSPECULAR, PIntegerVector(params)^) then begin
                     iSPECULAR:=PIntegerVector(params)^;
                     SPECULAR:=IntToFloatVector(iSPECULAR);
                     Opengl1x.glMaterialiv(face, pname, params);
                  end;
      GL_EMISSION: if not VectorEquals(iEMISSION, PIntegerVector(params)^) then begin
                     iEMISSION:=PIntegerVector(params)^;
                     EMISSION:=IntToFloatVector(iEMISSION);
                     Opengl1x.glMaterialiv(face, pname, params);
                  end;
      GL_SHININESS: if iSHININESS<>params^ then begin
                     iSHININESS:=params^; SHININESS:=params^;
                     Opengl1x.glMaterialiv(face, pname, params);
                  end;
    end;
    end;
  end else begin
    assert(face=GL_FRONT_AND_BACK, 'Invalid Face Type');
    with GLStateCache.MaterialCache do begin
      case pname of
        GL_AMBIENT: if (not VectorEquals(Faces[GL_FRONT].iAMBIENT, PIntegerVector(params)^))
                    or (not VectorEquals(Faces[GL_BACK].iAMBIENT, PIntegerVector(params)^))
                    then begin
                       Faces[GL_FRONT].iAMBIENT:=PIntegerVector(params)^;
                       Faces[GL_FRONT].AMBIENT:=IntToFloatVector(Faces[GL_FRONT].iAMBIENT);
                       Faces[GL_BACK].iAMBIENT:=Faces[GL_FRONT].iAMBIENT;
                       Faces[GL_BACK].AMBIENT:=Faces[GL_FRONT].AMBIENT;
                       Opengl1x.glMaterialiv(face, pname, params);
                    end;
        GL_DIFFUSE: if (not VectorEquals(Faces[GL_FRONT].iDIFFUSE, PIntegerVector(params)^))
                    or (not VectorEquals(Faces[GL_BACK].iDIFFUSE, PIntegerVector(params)^))
                    then begin
                       Faces[GL_FRONT].iDIFFUSE:=PIntegerVector(params)^;
                       Faces[GL_FRONT].DIFFUSE:=IntToFloatVector(Faces[GL_FRONT].iDIFFUSE);
                       Faces[GL_BACK].iDIFFUSE:=Faces[GL_FRONT].iDIFFUSE;
                       Faces[GL_BACK].DIFFUSE:=Faces[GL_FRONT].DIFFUSE;
                       Opengl1x.glMaterialiv(face, pname, params);
                    end;
        GL_SPECULAR:if (not VectorEquals(Faces[GL_FRONT].iSPECULAR, PIntegerVector(params)^))
                    or (not VectorEquals(Faces[GL_BACK].iSPECULAR, PIntegerVector(params)^))
                    then begin
                       Faces[GL_FRONT].iSPECULAR:=PIntegerVector(params)^;
                       Faces[GL_FRONT].SPECULAR:=IntToFloatVector(Faces[GL_FRONT].iSPECULAR);
                       Faces[GL_BACK].iSPECULAR:=Faces[GL_FRONT].iSPECULAR;
                       Faces[GL_BACK].SPECULAR:=Faces[GL_FRONT].SPECULAR;
                       Opengl1x.glMaterialiv(face, pname, params);
                    end;
        GL_EMISSION:if (not VectorEquals(Faces[GL_FRONT].iEMISSION, PIntegerVector(params)^))
                    or (not VectorEquals(Faces[GL_BACK].iEMISSION, PIntegerVector(params)^))
                    then begin
                       Faces[GL_FRONT].iEMISSION:=PIntegerVector(params)^;
                       Faces[GL_FRONT].EMISSION:=IntToFloatVector(Faces[GL_FRONT].iEMISSION);
                       Faces[GL_BACK].iEMISSION:=Faces[GL_FRONT].iEMISSION;
                       Faces[GL_BACK].EMISSION:=Faces[GL_FRONT].EMISSION;
                       Opengl1x.glMaterialiv(face, pname, params);
                    end;
        GL_SHININESS:if (Faces[GL_FRONT].iSHININESS<>params^)
                     or (Faces[GL_BACK].iSHININESS<>params^)
                    then begin
                        Faces[GL_FRONT].iSHININESS:=params^;
                        Faces[GL_FRONT].SHININESS:=params^;
                        Faces[GL_BACK].iSHININESS:=params^;
                        Faces[GL_BACK].SHININESS:=params^;
                        Opengl1x.glMaterialiv(face, pname, params);
                    end;
      end;
    end;
  end;
end;

procedure glColorMaterial(face: TGLEnum; mode: TGLEnum);
begin
  if face<>GL_FRONT_AND_BACK then
  with GLStateCache.MaterialCache do begin
   if (Faces[face].ColorMaterialMode<>mode)
   or (FaceMode<>face) then begin
      FaceMode:=face;
      Faces[face].ColorMaterialMode:=mode;
      Opengl1x.glColorMaterial(face,mode);
   end;
  end else
  with GLStateCache.MaterialCache do begin
    if (Faces[GL_FRONT].ColorMaterialMode<>mode)
    or (Faces[GL_BACK].ColorMaterialMode<>mode)
    or (FaceMode<>face) then begin
      FaceMode:=face;
      Faces[GL_FRONT].ColorMaterialMode:=mode;
      Faces[GL_BACK].ColorMaterialMode:=mode;
      Opengl1x.glColorMaterial(face,mode);
    end;
  end;
end;

{ TGLLightingCache }

procedure TGLLightingCache.Assign(LightingCache: TGLLightingCache);
var i, LId:integer;
const cWhiteColor: TVector = (1,1,1,1);
begin
  if LightingCache.Enabled then glEnable(GL_LIGHTING)
  else glDisable(GL_LIGHTING);
  for i:=0 to MaxLights-1 do begin
    LId:=GL_LIGHT0+i;
    if LightingCache.Lights[i].Enabled then glEnable(LId)
    else glDisable(LId);

    glLightf(LId,GL_SPOT_EXPONENT, LightingCache.Lights[i].SPOT_EXPONENT);
    glLightf(LId,GL_SPOT_CUTOFF, LightingCache.Lights[i].SPOT_CUTOFF);
    glLightf(LId,GL_CONSTANT_ATTENUATION, LightingCache.Lights[i].CONSTANT_ATTENUATION);
    glLightf(LId,GL_LINEAR_ATTENUATION, LightingCache.Lights[i].LINEAR_ATTENUATION);
    glLightf(LId,GL_QUADRATIC_ATTENUATION, LightingCache.Lights[i].QUADRATIC_ATTENUATION);
    glLightfv(LId,GL_AMBIENT, @LightingCache.Lights[i].AMBIENT);
    glLightfv(LId,GL_DIFFUSE, @LightingCache.Lights[i].DIFFUSE);
    glLightfv(LId,GL_SPECULAR, @LightingCache.Lights[i].SPECULAR);
    glLightfv(LId,GL_POSITION, @LightingCache.Lights[i].POSITION);
    glLightfv(LId,GL_SPOT_DIRECTION, @LightingCache.Lights[i].SPOT_DIRECTION);
  end;

  Opengl1x.glLightModelfv(GL_LIGHT_MODEL_AMBIENT, @LightingCache.LM_AMBIENT);
  Opengl1x.glLightModeliv(GL_LIGHT_MODEL_LOCAL_VIEWER, @LightingCache.LM_LOCAL_VIEWER);
  Opengl1x.glLightModeli(GL_LIGHT_MODEL_COLOR_CONTROL, LightingCache.LM_COLOR_CONTROL);
  Opengl1x.glLightModeliv(GL_LIGHT_MODEL_TWO_SIDE, @LightingCache.LM_TWO_SIDE);

  FChecked:=true;
end;

procedure TGLLightingCache.GetCurrentGLStates;
var LId,i: integer;
begin
  Opengl1x.glGetIntegerv(GL_MAX_LIGHTS,@MaxLights);
  Enabled:=Opengl1x.glIsEnabled(GL_LIGHTING);
  SetLength(Lights,MaxLights);
  for i:=0 to MaxLights-1 do with Lights[i] do begin
      LId:=GL_LIGHT0+i; Enabled:=Opengl1x.glIsEnabled(LId);
      Opengl1x.glGetLightfv(LId, GL_SPOT_EXPONENT, @SPOT_EXPONENT);
      Opengl1x.glGetLightfv(LId, GL_SPOT_CUTOFF, @SPOT_CUTOFF);
      Opengl1x.glGetLightfv(LId, GL_CONSTANT_ATTENUATION, @CONSTANT_ATTENUATION);
      Opengl1x.glGetLightfv(LId, GL_LINEAR_ATTENUATION, @LINEAR_ATTENUATION);
      Opengl1x.glGetLightfv(LId, GL_QUADRATIC_ATTENUATION, @QUADRATIC_ATTENUATION);
      Opengl1x.glGetLightfv(LId, GL_AMBIENT, @AMBIENT);
      Opengl1x.glGetLightfv(LId, GL_DIFFUSE, @DIFFUSE);
      Opengl1x.glGetLightfv(LId, GL_SPECULAR, @SPECULAR);
      Opengl1x.glGetLightfv(LId, GL_POSITION, @POSITION);
      Opengl1x.glGetLightfv(LId, GL_SPOT_DIRECTION, @SPOT_DIRECTION);
  end;

  Opengl1x.glGetFloatv(GL_LIGHT_MODEL_AMBIENT, @LM_AMBIENT);
  Opengl1x.glGetBooleanv(GL_LIGHT_MODEL_LOCAL_VIEWER, @LM_LOCAL_VIEWER);
  Opengl1x.glGetIntegerv(GL_LIGHT_MODEL_COLOR_CONTROL,@LM_COLOR_CONTROL);
  Opengl1x.glGetBooleanv(GL_LIGHT_MODEL_TWO_SIDE, @LM_TWO_SIDE);

  FChecked:=true;
end;

procedure TGLLightingCache.Reset;
var i, LId:integer;
const cWhiteColor: TVector = (1,1,1,1);
begin
  Opengl1x.glGetIntegerv(GL_MAX_LIGHTS,@MaxLights);
  SetLength(Lights,MaxLights);
  Enabled:=false; Opengl1x.glDisable(GL_LIGHTING);
  for i:=0 to MaxLights-1 do with Lights[i] do begin
    Enabled:=false; LId:=GL_LIGHT0+i;
    AMBIENT:=NullHmgPoint;
    if i=0 then begin
       DIFFUSE:=cWhiteColor;
       SPECULAR:=cWhiteColor;
    end else begin
       DIFFUSE:=NullHmgPoint;
       SPECULAR:=NullHmgPoint;
    end;
    POSITION:=vectormake(0,0,1,0);
    SPOT_DIRECTION:=vectormake(0,0,-1,0);
    SPOT_EXPONENT:=0;
    SPOT_CUTOFF:=180;
    CONSTANT_ATTENUATION:=1;
    LINEAR_ATTENUATION:=0;
    QUADRATIC_ATTENUATION:=0;
    Opengl1x.glDisable(LId);
    Opengl1x.glLightf(LId,GL_SPOT_EXPONENT, SPOT_EXPONENT);
    Opengl1x.glLightf(LId,GL_SPOT_CUTOFF, SPOT_CUTOFF);
    Opengl1x.glLightf(LId,GL_CONSTANT_ATTENUATION, CONSTANT_ATTENUATION);
    Opengl1x.glLightf(LId,GL_LINEAR_ATTENUATION, LINEAR_ATTENUATION);
    Opengl1x.glLightf(LId,GL_QUADRATIC_ATTENUATION, QUADRATIC_ATTENUATION);
    Opengl1x.glLightfv(LId,GL_AMBIENT, @AMBIENT);
    Opengl1x.glLightfv(LId,GL_DIFFUSE, @DIFFUSE);
    Opengl1x.glLightfv(LId,GL_SPECULAR, @SPECULAR);
    Opengl1x.glLightfv(LId,GL_POSITION, @POSITION);
    Opengl1x.glLightfv(LId,GL_SPOT_DIRECTION, @SPOT_DIRECTION);
    Opengl1x.glDisable(LId);
  end;

  LM_AMBIENT:=vectormake(0.2,0.2,0.2,1);
  LM_LOCAL_VIEWER:=false;
  LM_COLOR_CONTROL:=GL_SINGLE_COLOR;
  LM_TWO_SIDE:=false;

  Opengl1x.glLightModelfv(GL_LIGHT_MODEL_AMBIENT, @LM_AMBIENT);
  Opengl1x.glLightModeliv(GL_LIGHT_MODEL_LOCAL_VIEWER, @LM_LOCAL_VIEWER);
  Opengl1x.glLightModeli(GL_LIGHT_MODEL_COLOR_CONTROL, LM_COLOR_CONTROL);
  Opengl1x.glLightModeliv(GL_LIGHT_MODEL_TWO_SIDE, @LM_TWO_SIDE);

  FChecked:=true;
end;

procedure glLightModelf(pname: TGLEnum; param: TGLfloat);
begin
   with GLStateCache.LightingCache do begin
      case pname of
        GL_LIGHT_MODEL_LOCAL_VIEWER: if LM_LOCAL_VIEWER<>(param=GL_TRUE) then begin
                   LM_LOCAL_VIEWER:=(param=GL_TRUE);
                   Opengl1x.glLightModelf(pname, param);
                end;
        GL_LIGHT_MODEL_COLOR_CONTROL: if LM_COLOR_CONTROL<>param then begin
                   LM_COLOR_CONTROL:=trunc(param);
                   Opengl1x.glLightModelf(pname, param);
                end;
        GL_LIGHT_MODEL_TWO_SIDE: if LM_TWO_SIDE<>(param=GL_TRUE) then begin
                   LM_LOCAL_VIEWER:=(param=GL_TRUE);
                   Opengl1x.glLightModelf(pname, param);
                end;
        else Opengl1x.glLightModelf(pname, param);
      end;
   end;
end;

procedure glLightModeli(pname: TGLEnum; param: TGLint);
begin
   with GLStateCache.LightingCache do begin
      case pname of
        GL_LIGHT_MODEL_LOCAL_VIEWER: if LM_LOCAL_VIEWER<>(param=GL_TRUE) then begin
                   LM_LOCAL_VIEWER:=(param=GL_TRUE);
                   Opengl1x.glLightModeli(pname, param);
                end;
        GL_LIGHT_MODEL_COLOR_CONTROL: if LM_COLOR_CONTROL<>param then begin
                   LM_COLOR_CONTROL:=param;
                   Opengl1x.glLightModeli(pname, param);
                end;
        GL_LIGHT_MODEL_TWO_SIDE: if LM_TWO_SIDE<>(param=GL_TRUE) then begin
                   LM_LOCAL_VIEWER:=(param=GL_TRUE);
                   Opengl1x.glLightModeli(pname, param);
                end;
        else Opengl1x.glLightModeli(pname, param);
      end;
   end;
end;

procedure glLightModelfv(pname: TGLEnum; params: PGLfloat);
begin
   with GLStateCache.LightingCache do begin
      case pname of
        GL_LIGHT_MODEL_LOCAL_VIEWER: if LM_LOCAL_VIEWER<>(params^=GL_TRUE) then begin
                   LM_LOCAL_VIEWER:=(params^=GL_TRUE);
                   Opengl1x.glLightModelfv(pname, params);
                end;
        GL_LIGHT_MODEL_COLOR_CONTROL: if LM_COLOR_CONTROL<>params^ then begin
                   LM_COLOR_CONTROL:=trunc(params^);
                   Opengl1x.glLightModelfv(pname, params);
                end;
        GL_LIGHT_MODEL_TWO_SIDE: if LM_TWO_SIDE<>(params^=GL_TRUE) then begin
                   LM_LOCAL_VIEWER:=(params^=GL_TRUE);
                   Opengl1x.glLightModelfv(pname, params);
                end;
        GL_LIGHT_MODEL_AMBIENT: if not VectorEquals(LM_AMBIENT, PVector(params)^) then begin
                   LM_AMBIENT:=PVector(params)^;
                   Opengl1x.glLightModelfv(pname, params);
                end;
        else Opengl1x.glLightModelfv(pname, params);
      end;
   end;
end;

procedure glLightModeliv(pname: TGLEnum; params: PGLint);
begin
   with GLStateCache.LightingCache do begin
      case pname of
        GL_LIGHT_MODEL_LOCAL_VIEWER: if LM_LOCAL_VIEWER<>(params^=GL_TRUE) then begin
                   LM_LOCAL_VIEWER:=(params^=GL_TRUE);
                   Opengl1x.glLightModeliv(pname, params);
                end;
        GL_LIGHT_MODEL_COLOR_CONTROL: if LM_COLOR_CONTROL<>params^ then begin
                   LM_COLOR_CONTROL:=params^;
                   Opengl1x.glLightModeliv(pname, params);
                end;
        GL_LIGHT_MODEL_TWO_SIDE: if LM_TWO_SIDE<>(params^=GL_TRUE) then begin
                   LM_LOCAL_VIEWER:=(params^=GL_TRUE);
                   Opengl1x.glLightModeliv(pname, params);
                end;
        GL_LIGHT_MODEL_AMBIENT: if not VectorEquals(LM_AMBIENT, PVector(params)^) then begin
                   LM_AMBIENT:=PVector(params)^;
                   Opengl1x.glLightModeliv(pname, params);
                end;
        else Opengl1x.glLightModeliv(pname, params);
      end;
   end;
end;

procedure glLightf(light, pname: TGLEnum; param: TGLfloat);
var LId: integer;
begin
  LId:=light-GL_LIGHT0;
  with GLStateCache.LightingCache.Lights[LId] do begin
     case pname of
        GL_SPOT_EXPONENT: if SPOT_EXPONENT<>param then begin
                SPOT_EXPONENT:=param;
                Opengl1x.glLightf(light, pname,param);
              end;
        GL_SPOT_CUTOFF: if SPOT_CUTOFF<>param then begin
                SPOT_CUTOFF:=param;
                Opengl1x.glLightf(light, pname,param);
              end;
        GL_CONSTANT_ATTENUATION: if CONSTANT_ATTENUATION<>param then begin
                CONSTANT_ATTENUATION:=param;
                Opengl1x.glLightf(light, pname,param);
              end;
        GL_LINEAR_ATTENUATION: if LINEAR_ATTENUATION<>param then begin
                LINEAR_ATTENUATION:=param;
                Opengl1x.glLightf(light, pname,param);
              end;
        GL_QUADRATIC_ATTENUATION: if QUADRATIC_ATTENUATION<>param then begin
                QUADRATIC_ATTENUATION:=param;
                Opengl1x.glLightf(light, pname,param);
              end;
        Else Opengl1x.glLightf(light, pname,param);
     end;
  end;
end;

procedure glLighti(light, pname: TGLEnum; param: TGLint);
var LId: integer;
begin
  LId:=light-GL_LIGHT0;
  with GLStateCache.LightingCache.Lights[LId] do begin
     case pname of
        GL_SPOT_EXPONENT: if SPOT_EXPONENT<>param then begin
                SPOT_EXPONENT:=param;
                Opengl1x.glLightf(light, pname,param);
              end;
        GL_SPOT_CUTOFF: if SPOT_CUTOFF<>param then begin
                SPOT_CUTOFF:=param;
                Opengl1x.glLightf(light, pname,param);
              end;
        GL_CONSTANT_ATTENUATION: if CONSTANT_ATTENUATION<>param then begin
                CONSTANT_ATTENUATION:=param;
                Opengl1x.glLightf(light, pname,param);
              end;
        GL_LINEAR_ATTENUATION: if LINEAR_ATTENUATION<>param then begin
                LINEAR_ATTENUATION:=param;
                Opengl1x.glLightf(light, pname,param);
              end;
        GL_QUADRATIC_ATTENUATION: if QUADRATIC_ATTENUATION<>param then begin
                QUADRATIC_ATTENUATION:=param;
                Opengl1x.glLightf(light, pname,param);
              end;
        Else Opengl1x.glLightf(light, pname,param);
     end;
  end;
end;

procedure glLightfv(light, pname: TGLEnum; params: PGLfloat);
var LId: integer;
begin
  LId:=light-GL_LIGHT0;
  with GLStateCache.LightingCache.Lights[LId] do begin
     case pname of
        GL_SPOT_EXPONENT: if SPOT_EXPONENT<>params^ then begin
                SPOT_EXPONENT:=params^;
                Opengl1x.glLightfv(light, pname, params);
              end;
        GL_SPOT_CUTOFF: if SPOT_CUTOFF<>params^ then begin
                SPOT_CUTOFF:=params^;
                Opengl1x.glLightfv(light, pname, params);
              end;
        GL_CONSTANT_ATTENUATION: if CONSTANT_ATTENUATION<>params^ then begin
                CONSTANT_ATTENUATION:=params^;
                Opengl1x.glLightfv(light, pname, params);
              end;
        GL_LINEAR_ATTENUATION: if LINEAR_ATTENUATION<>params^ then begin
                LINEAR_ATTENUATION:=params^;
                Opengl1x.glLightfv(light, pname, params);
              end;
        GL_QUADRATIC_ATTENUATION: if QUADRATIC_ATTENUATION<>params^ then begin
                QUADRATIC_ATTENUATION:=params^;
                Opengl1x.glLightfv(light, pname, params);
              end;
        GL_AMBIENT: if not VectorEquals(AMBIENT,PVector(params)^) then begin
                AMBIENT:=PVector(params)^;
                Opengl1x.glLightfv(light, pname, params);
              end;
        GL_DIFFUSE: if not VectorEquals(DIFFUSE,PVector(params)^) then begin
                DIFFUSE:=PVector(params)^;
                Opengl1x.glLightfv(light, pname, params);
              end;
        GL_SPECULAR: if not VectorEquals(SPECULAR,PVector(params)^) then begin
                SPECULAR:=PVector(params)^;
                Opengl1x.glLightfv(light, pname, params);
              end;
        GL_POSITION: if not VectorEquals(POSITION,PVector(params)^) then begin
                POSITION:=PVector(params)^;
                Opengl1x.glLightfv(light, pname, params);
              end;
        GL_SPOT_DIRECTION: if not VectorEquals(SPOT_DIRECTION,PVector(params)^) then begin
                SPOT_DIRECTION:=PVector(params)^;
                Opengl1x.glLightfv(light, pname, params);
              end;
        Else Opengl1x.glLightfv(light, pname,params);
     end;
  end;
end;

procedure glLightiv(light, pname: TGLEnum; params: PGLint);
var LId: integer;
begin
  LId:=light-GL_LIGHT0;
  with GLStateCache.LightingCache.Lights[LId] do begin
     case pname of
        GL_SPOT_EXPONENT: if SPOT_EXPONENT<>params^ then begin
                SPOT_EXPONENT:=params^;
                Opengl1x.glLightiv(light, pname, params);
              end;
        GL_SPOT_CUTOFF: if SPOT_CUTOFF<>params^ then begin
                SPOT_CUTOFF:=params^;
                Opengl1x.glLightiv(light, pname, params);
              end;
        GL_CONSTANT_ATTENUATION: if CONSTANT_ATTENUATION<>params^ then begin
                CONSTANT_ATTENUATION:=params^;
                Opengl1x.glLightiv(light, pname, params);
              end;
        GL_LINEAR_ATTENUATION: if LINEAR_ATTENUATION<>params^ then begin
                LINEAR_ATTENUATION:=params^;
                Opengl1x.glLightiv(light, pname, params);
              end;
        GL_QUADRATIC_ATTENUATION: if QUADRATIC_ATTENUATION<>params^ then begin
                QUADRATIC_ATTENUATION:=params^;
                Opengl1x.glLightiv(light, pname, params);
              end;
        GL_AMBIENT: if not VectorEquals(AMBIENT,PVector(params)^) then begin
                AMBIENT:=PVector(params)^;
                Opengl1x.glLightiv(light, pname, params);
              end;
        GL_DIFFUSE: if not VectorEquals(DIFFUSE,PVector(params)^) then begin
                DIFFUSE:=PVector(params)^;
                Opengl1x.glLightiv(light, pname, params);
              end;
        GL_SPECULAR: if not VectorEquals(SPECULAR,PVector(params)^) then begin
                SPECULAR:=PVector(params)^;
                Opengl1x.glLightiv(light, pname, params);
              end;
        GL_POSITION: if not VectorEquals(POSITION,PVector(params)^) then begin
                POSITION:=PVector(params)^;
                Opengl1x.glLightiv(light, pname, params);
              end;
        GL_SPOT_DIRECTION: if not VectorEquals(SPOT_DIRECTION,PVector(params)^) then begin
                SPOT_DIRECTION:=PVector(params)^;
                Opengl1x.glLightiv(light, pname, params);
              end;
        Else Opengl1x.glLightiv(light, pname,params);
     end;
  end;
end;

procedure glGetLightfv(light, pname: TGLEnum; params: PGLfloat);
var LId: integer;
    p:pointer;
begin
  LId:=light-GL_LIGHT0; p:=params;
  with GLStateCache.LightingCache.Lights[LId] do begin
     case pname of
        GL_SPOT_EXPONENT: params^:=SPOT_EXPONENT;
        GL_SPOT_CUTOFF: params^:=SPOT_CUTOFF;
        GL_CONSTANT_ATTENUATION: params^:=CONSTANT_ATTENUATION;
        GL_LINEAR_ATTENUATION: params^:=LINEAR_ATTENUATION;
        GL_QUADRATIC_ATTENUATION: params^:=QUADRATIC_ATTENUATION;
        GL_AMBIENT: PVector(p)^:=AMBIENT;
        GL_DIFFUSE: PVector(p)^:=DIFFUSE;
        GL_SPECULAR: PVector(p)^:=SPECULAR;
        GL_POSITION: PVector(p)^:=POSITION;
        GL_SPOT_DIRECTION: PVector(p)^:=SPOT_DIRECTION;
        Else Opengl1x.glLightfv(light, pname, params);
     end;
  end;
end;

procedure glGetLightiv(light, pname: TGLEnum; params: PGLint);
var LId: integer;
    p:pointer;
begin
  LId:=light-GL_LIGHT0; p:=params;
  with GLStateCache.LightingCache.Lights[LId] do begin
     case pname of
        GL_SPOT_EXPONENT: params^:=trunc(SPOT_EXPONENT);
        GL_SPOT_CUTOFF: params^:=trunc(SPOT_CUTOFF);
        GL_CONSTANT_ATTENUATION: params^:=trunc(CONSTANT_ATTENUATION);
        GL_LINEAR_ATTENUATION: params^:=trunc(LINEAR_ATTENUATION);
        GL_QUADRATIC_ATTENUATION: params^:=trunc(QUADRATIC_ATTENUATION);
        GL_AMBIENT: PIntegerVector(p)^:=FloatToIntVector(AMBIENT);
        GL_DIFFUSE: PIntegerVector(p)^:=FloatToIntVector(DIFFUSE);
        GL_SPECULAR: PIntegerVector(p)^:=FloatToIntVector(SPECULAR);
        GL_POSITION: PIntegerVector(p)^:=FloatToIntVector(POSITION);
        GL_SPOT_DIRECTION: PIntegerVector(p)^:=FloatToIntVector(SPOT_DIRECTION);
        Else Opengl1x.glLightiv(light, pname, params);
     end;
  end;
end;


procedure glBindTexture(target: TGLEnum; texture: TGLuint);
begin
//Opengl1x.glBindTexture(target,Texture); exit;
  with GLStateCache.TextureCache do begin
       if Units[CurrentUnit].ID<>texture then begin
          Opengl1x.glBindTexture(target,Texture);
          Units[CurrentUnit].Target:=target;
          Units[CurrentUnit].ID:=Texture;
       end;
  end;
end;

procedure glActiveTexture(target: TGLenum);
begin
//  opengl1x.glActiveTexture(target); exit;
  if target<>GLStateCache.TextureCache.CurrentUnit+GL_Texture0
  then begin
    opengl1x.glActiveTexture(target);
    GLStateCache.TextureCache.CurrentUnit:=target-GL_Texture0;
  end;
end;

procedure glClientActiveTexture(target: TGLenum);
begin
  if target<>GLStateCache.TextureCache.CurrentClientUnit+GL_Texture0
  then begin
    opengl1x.glClientActiveTexture(target);
    GLStateCache.TextureCache.CurrentClientUnit:=target-GL_Texture0;
  end;
end;


constructor TGLStateCache.Create;
begin
  inherited;
  FDepthCache:=TGLDepthCache.Create;
  FAlphaCache:=TGLAlphaCache.Create;
  FBlendingCache:=TGLBlendingCache.Create;
  FMaterialCache:=TGLMaterialCache.Create;
  FLightingCache:=TGLLightingCache.Create;
  FTextureCache:=TGLTextureCache.Create;
  FShaderCache:=TGLShaderCache.Create;
  FActiveProgram:=0;
  FGLVBOManager:=TGLVBOManager.Create;
//  MatrixCache:=TGLMatrixCache.Create;
  FChecked:=false;
end;

destructor TGLStateCache.Destroy;
//var i: integer;
begin
    FDepthCache.Free;
    FAlphaCache.Free;
    FBlendingCache.Free;
    FMaterialCache.Free; FMaterialCache:=nil;
    FLightingCache.Free;
    FTextureCache.Free;
    FShaderCache.Free;
    FGLVBOManager.Free;
//    MatrixCache.Free;
    inherited;
end;

procedure TGLStateCache.PopStates;
var Temp: TGLStateCache;
begin
   if StateStack.Count=0 then exit;
   Temp:=StateStack[StateStack.Count-1];
   if Temp.FChecked then Assign(Temp);
   Temp.Free;
   StateStack.Delete(StateStack.Count-1);
end;

procedure TGLStateCache.PushStates;
var Temp: TGLStateCache;
begin
   assert(FChecked,#13+#10+'You can''t store not checked state cache!'+#13+#10);
   Temp:=TGLStateCache.Create;
   AlphaCache.StoreTo(Temp.FAlphaCache);
   DepthCache.StoreTo(Temp.DepthCache);
   BlendingCache.StoreTo(Temp.BlendingCache);
   MaterialCache.StoreTo(Temp.MaterialCache);
   LightingCache.StoreTo(Temp.LightingCache);
   TextureCache.StoreTo(Temp.TextureCache);
   ShaderCache.StoreTo(Temp.ShaderCache);
   Temp.FChecked:=FChecked;
   Temp.States:=States;
   Temp.FActiveProgram:=FActiveProgram;
   StateStack.Add(Temp);
end;

procedure TGLStateCache.ReplaceStates(const StateCache: TGLStateCache);
begin
//
end;

procedure TGLStateCache.ResetStates(CheckGLStates: boolean);
begin
    if CheckGLStates then CheckStates;
    DepthCache.Reset;
    AlphaCache.Reset;
    BlendingCache.Reset;
    MaterialCache.Reset;
    LightingCache.Reset;
    TextureCache.Reset;
    ShaderCache.Reset;
end;

procedure TGLLightingCache.StoreTo(LightingCache: TGLLightingCache);
var i:integer;
begin
   LightingCache.FChecked:=FChecked;
   LightingCache.MaxLights:=MaxLights;
   LightingCache.Enabled:=Enabled;
   setlength(LightingCache.Lights,MaxLights);
   for i:=0 to MaxLights-1 do
   with LightingCache.Lights[i] do begin
        Enabled:=Lights[i].Enabled;
        SPOT_EXPONENT:=Lights[i].SPOT_EXPONENT;
        SPOT_CUTOFF:=Lights[i].SPOT_CUTOFF;
        CONSTANT_ATTENUATION:=Lights[i].CONSTANT_ATTENUATION;
        LINEAR_ATTENUATION:=Lights[i].LINEAR_ATTENUATION;
        QUADRATIC_ATTENUATION:=Lights[i].QUADRATIC_ATTENUATION;
        AMBIENT:=Lights[i].AMBIENT;
        DIFFUSE:=Lights[i].DIFFUSE;
        SPECULAR:=Lights[i].SPECULAR;
        POSITION:=Lights[i].POSITION;
        SPOT_DIRECTION:=Lights[i].SPOT_DIRECTION;
   end;
   LightingCache.LM_AMBIENT:=LM_AMBIENT;
   LightingCache.LM_LOCAL_VIEWER:=LM_LOCAL_VIEWER;
   LightingCache.LM_COLOR_CONTROL:=LM_COLOR_CONTROL;
   LightingCache.LM_TWO_SIDE:=LM_TWO_SIDE;
end;

{ TGLTextureCache }

procedure TGLTextureCache.Assign(TextureCache: TGLTextureCache);
var i:integer;
begin
  glActiveTexture(TextureCache.CurrentUnit+GL_TEXTURE0);
  if TextureCache.Texture1D then glEnable(GL_TEXTURE_1D)
  else glDisable(GL_TEXTURE_1D);
  if TextureCache.Texture2D then glEnable(GL_TEXTURE_2D)
  else glDisable(GL_TEXTURE_2D);
  if TextureCache.Texture3D then glEnable(GL_TEXTURE_3D)
  else glDisable(GL_TEXTURE_3D);
  if TextureCache.TextureCubeMap then glEnable(GL_TEXTURE_CUBE_MAP)
  else glDisable(GL_TEXTURE_CUBE_MAP);
  if TextureCache.TextureRectangle then glEnable(GL_TEXTURE_RECTANGLE)
  else glDisable(GL_TEXTURE_RECTANGLE);
  glBindTexture(TextureCache.Units[CurrentUnit].Target,
                TextureCache.Units[CurrentUnit].ID);
  glClientActiveTexture(TextureCache.CurrentClientUnit+GL_TEXTURE0);
  for i:=0 to high(TextureCache.Units) do Units[i]:=TextureCache.Units[i];

end;

procedure TGLTextureCache.GetCurrentGLStates;
var maxTU, ctu, i: integer;
begin
  OpenGL1x.glGetIntegerv(GL_MAX_TEXTURE_UNITS,@MaxTU);
  setlength(Units,MaxTU);
  if OpenGL1x.glIsEnabled(GL_TEXTURE_1D) then Texture1D:=true else Texture1D:=false;
  if OpenGL1x.glIsEnabled(GL_TEXTURE_2D) then Texture2D:=true else Texture2D:=false;
  if OpenGL1x.glIsEnabled(GL_TEXTURE_3D) then Texture3D:=true else Texture3D:=false;
  if OpenGL1x.glIsEnabled(GL_TEXTURE_CUBE_MAP) then TextureCubeMap:=true else TextureCubeMap:=false;
  if OpenGL1x.glIsEnabled(GL_TEXTURE_RECTANGLE) then TextureRectangle:=true else TextureRectangle:=false;
  glGetIntegerv(GL_ACTIVE_TEXTURE,@CTU);
  CurrentUnit:=CTU-GL_TEXTURE0;
  OpenGL1x.glGetIntegerv(GL_CLIENT_ACTIVE_TEXTURE,@CTU);
  CurrentClientUnit:=CTU-GL_TEXTURE0;
  for i:=0 to maxTU-1 do begin
     Units[i].Target:=GL_TEXTURE_2D;
     Units[i].ID:=0;
  end;
  FChecked:=true;
end;

procedure TGLTextureCache.Reset;
begin
  FChecked:=true;
  glDisable(GL_TEXTURE_2D);
  glDisable(GL_TEXTURE_3D);
  glDisable(GL_TEXTURE_1D);
  glDisable(GL_TEXTURE_RECTANGLE);
  glDisable(GL_TEXTURE_CUBE_MAP);
  glClientActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D,0);
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D,0);
  CurrentUnit:=0;
  CurrentClientUnit:=0;
end;

procedure TGLTextureCache.StoreTo(TextureCache: TGLTextureCache);
var i:integer;
begin
   TextureCache.FChecked:=FChecked;
   TextureCache.Texture1D:=Texture1D;
   TextureCache.Texture2D:=Texture2D;
   TextureCache.Texture3D:=Texture3D;
   TextureCache.TextureCubeMap:=TextureCubeMap;
   TextureCache.TextureRectangle:=TextureRectangle;
   setlength(TextureCache.Units,high(Units)+1);
   for i:=0 to high(Units) do begin
     TextureCache.Units[i].Target:=Units[i].Target;
     TextureCache.Units[i].ID:=Units[i].ID;
   end;
   TextureCache.CurrentUnit:=CurrentUnit;
   TextureCache.CurrentClientUnit:=CurrentClientUnit;
end;

{ TGLMatrixCache }

constructor TGLMatrixCache.Create;
begin
  inherited;
  ColorMatrix:=TList.Create;
  ModelMatrix:=TList.Create;
  ProjectionMatrix:=TList.Create;
  TextureMatrix:=TList.Create;
end;

destructor TGLMatrixCache.Destroy;
begin
  FreeList(ColorMatrix);
  FreeList(ModelMatrix);
  FreeList(ProjectionMatrix);
  FreeList(TextureMatrix);
  inherited;
end;

procedure TGLMatrixCache.GetCurrentGLStates;
begin
  OpenGL1x.glGetIntegerv(GL_MATRIX_MODE, @MatrixMode);
  with Current do begin
    OpenGL1x.glGetFLoatv(GL_MODELVIEW_MATRIX,PGLFloat(@ModelViewMatrix));
    MVLoaded:=true;
    OpenGL1x.glGetFLoatv(GL_PROJECTION_MATRIX,PGLFloat(@ProjectionMatrix));
    PMLoaded:=true;
    OpenGL1x.glGetFLoatv(GL_TEXTURE_MATRIX,PGLFloat(@TextureMatrix));
    TMLoaded:=true;
    OpenGL1x.glGetFLoatv(GL_COLOR_MATRIX,PGLFloat(@ColorMatrix));
    CMLoaded:=true;
  end;
  FChecked:=true;
end;
{
procedure glPushMatrix;
var m: PMatrix;
begin
  new(m);
  with GLStateCache.MatrixCache do begin
   case MatrixMode of
    GL_MODELVIEW  : begin m^:=Current.ModelViewMatrix; ModelMatrix.add(m);end;
    GL_PROJECTION : begin m^:=Current.ProjectionMatrix; ProjectionMatrix.add(m);end;
    GL_TEXTURE    : begin m^:=Current.TextureMatrix; TextureMatrix.add(m);end;
    GL_COLOR      : begin m^:=Current.ColorMatrix; ColorMatrix.add(m);end;
   else Dispose(m);
   end;
  end;
end;

procedure glPopMatrix;
var m: PMatrix;
begin
  with GLStateCache.MatrixCache do begin
   case MatrixMode of
    GL_MODELVIEW  :
           if ModelMatrix.Count>0 then begin
              m:=ModelMatrix.Last;
              Current.ModelViewMatrix:=m^;
              Current.MVLoaded:=false;
              glLoadMatrixf(PGLFloat(m));
              dispose(m);ModelMatrix.Delete(ModelMatrix.Count-1);
           end else begin
              Opengl1x.glPopMatrix; Current.MVLoaded:=true;
              glGetFLoatv(GL_MODELVIEW_MATRIX,PGLFloat(@Current.ModelViewMatrix));
           end;
    GL_PROJECTION :
           if ProjectionMatrix.Count>0 then begin
              m:=ProjectionMatrix.Last;
              Current.ProjectionMatrix:=m^;
              Current.PMLoaded:=false;
              glLoadMatrixf(PGLFloat(m));
              dispose(m);ProjectionMatrix.Delete(ProjectionMatrix.Count-1);
           end else begin
              Opengl1x.glPopMatrix; Current.PMLoaded:=true;
              glGetFLoatv(GL_PROJECTION_MATRIX,PGLFloat(@Current.ProjectionMatrix));
           end;
    GL_TEXTURE    :
           if TextureMatrix.Count>0 then begin
              m:=TextureMatrix.Last;
              Current.TextureMatrix:=m^;
              Current.TMLoaded:=false;
              glLoadMatrixf(PGLFloat(m));
              dispose(m);TextureMatrix.Delete(TextureMatrix.Count-1);
           end else begin
              Opengl1x.glPopMatrix; Current.TMLoaded:=true;
              glGetFLoatv(GL_TEXTURE_MATRIX,PGLFloat(@Current.TextureMatrix));
           end;
    GL_COLOR      :
           if ColorMatrix.Count>0 then begin
              m:=ColorMatrix.Last;
              Current.ColorMatrix:=m^;
              Current.CMLoaded:=false;
              glLoadMatrixf(PGLFloat(m));
              dispose(m);ColorMatrix.Delete(ColorMatrix.Count-1);
           end else begin
              Opengl1x.glPopMatrix; Current.CMLoaded:=true;
              glGetFLoatv(GL_COLOR_MATRIX,PGLFloat(@Current.ColorMatrix));
           end;
   end;
  end;
end;

procedure glLoadMatrixf(m: PGLfloat);
begin
  with GLStateCache.MatrixCache do begin
   case MatrixMode of
    GL_MODELVIEW  : begin Current.ModelViewMatrix:=PMatrix(m)^;
                    Current.MVLoaded:=false; end;
    GL_PROJECTION : begin Current.ProjectionMatrix:=PMatrix(m)^;
                    Current.PMLoaded:=false; end;
    GL_TEXTURE    : begin Current.TextureMatrix:=PMatrix(m)^;
                    Current.TMLoaded:=false; end;
    GL_COLOR      : begin Current.ColorMatrix:=PMatrix(m)^;
                    Current.CMLoaded:=false; end;
   end;
  end;
  Opengl1x.glLoadMatrixf(m);
end;

procedure glLoadIdentity;
begin
  with GLStateCache.MatrixCache do begin
   case MatrixMode of
    GL_MODELVIEW  : begin Current.ModelViewMatrix:=IdentityHmgMatrix;
                    Current.MVLoaded:=false; end;
    GL_PROJECTION : begin Current.ProjectionMatrix:=IdentityHmgMatrix;
                    Current.PMLoaded:=false; end;
    GL_TEXTURE    : begin Current.TextureMatrix:=IdentityHmgMatrix;
                    Current.TMLoaded:=false; end;
    GL_COLOR      : begin Current.ColorMatrix:=IdentityHmgMatrix;
                    Current.CMLoaded:=false; end;
   end;
  end;
  Opengl1x.glLoadIdentity;
end;

procedure glMultMatrixf(m: PGLfloat);
begin
  with GLStateCache.MatrixCache do begin
   case MatrixMode of
    GL_MODELVIEW  : begin
        Current.ModelViewMatrix:=MatrixMultiply(Current.ModelViewMatrix, PMatrix(m)^);
        Current.MVLoaded:=false; glLoadMatrixf(PGLFloat(@Current.ModelViewMatrix));
        end;
    GL_PROJECTION : begin
        Current.ProjectionMatrix:=MatrixMultiply(Current.ProjectionMatrix, PMatrix(m)^);
        Current.PMLoaded:=false; glLoadMatrixf(PGLFloat(@Current.ProjectionMatrix));
        end;
    GL_TEXTURE    : begin
        Current.TextureMatrix:=MatrixMultiply(Current.TextureMatrix, PMatrix(m)^);
        Current.TMLoaded:=false; glLoadMatrixf(PGLFloat(@Current.TextureMatrix));
        end;
    GL_COLOR      : begin
        Current.ColorMatrix:=MatrixMultiply(Current.ColorMatrix, PMatrix(m)^);
        Current.CMLoaded:=false; glLoadMatrixf(PGLFloat(@Current.ColorMatrix));
        end;
   end;
  end;
//  Opengl1x.glMultMatrixf(m);
end;

procedure glScalef(x, y, z: TGLfloat);
var m: TMatrix;
begin
  m:=CreateScaleMatrix(Vectormake(x,y,z,1));
  with GLStateCache.MatrixCache do begin
   case MatrixMode of
    GL_MODELVIEW  : begin
        Current.ModelViewMatrix:=MatrixMultiply(Current.ModelViewMatrix, m);
        Current.MVLoaded:=false; glLoadMatrixf(PGLFloat(@Current.ModelViewMatrix));
        end;
    GL_PROJECTION : begin
        Current.ProjectionMatrix:=MatrixMultiply(Current.ProjectionMatrix, m);
        Current.PMLoaded:=false; glLoadMatrixf(PGLFloat(@Current.ProjectionMatrix));
        end;
    GL_TEXTURE    : begin
        Current.TextureMatrix:=MatrixMultiply(Current.TextureMatrix, m);
        Current.TMLoaded:=false; glLoadMatrixf(PGLFloat(@Current.TextureMatrix));
        end;
    GL_COLOR      : begin
        Current.ColorMatrix:=MatrixMultiply(Current.ColorMatrix, m);
        Current.CMLoaded:=false; glLoadMatrixf(PGLFloat(@Current.ColorMatrix));
        end;
   end;
  end;
//  Opengl1x.glScalef(x, y, z);
end;

procedure glTranslatef(x, y, z: TGLfloat);
var m: TMatrix;
begin
  m:=CreateTranslationMatrix(Vectormake(x,y,z,1));
  with GLStateCache.MatrixCache do begin
   case MatrixMode of
    GL_MODELVIEW  : begin
        Current.ModelViewMatrix:=MatrixMultiply(Current.ModelViewMatrix, m);
        Current.MVLoaded:=false; glLoadMatrixf(PGLFloat(@Current.ModelViewMatrix));
        end;
    GL_PROJECTION : begin
        Current.ProjectionMatrix:=MatrixMultiply(Current.ProjectionMatrix, m);
        Current.PMLoaded:=false; glLoadMatrixf(PGLFloat(@Current.ProjectionMatrix));
        end;
    GL_TEXTURE    : begin
        Current.TextureMatrix:=MatrixMultiply(Current.TextureMatrix, m);
        Current.TMLoaded:=false; glLoadMatrixf(PGLFloat(@Current.TextureMatrix));
        end;
    GL_COLOR      : begin
        Current.ColorMatrix:=MatrixMultiply(Current.ColorMatrix, m);
        Current.CMLoaded:=false; glLoadMatrixf(PGLFloat(@Current.ColorMatrix));
        end;
   end;
  end;
end;

procedure glRotatef(angle, x, y, z: TGLfloat);
var m: TMatrix;
begin
  m:=CreateRotationMatrix(Vectormake(x,y,z,0), angle);
  with GLStateCache.MatrixCache do begin
   case MatrixMode of
    GL_MODELVIEW  : begin
        Current.ModelViewMatrix:=MatrixMultiply(Current.ModelViewMatrix, m);
        Current.MVLoaded:=false; glLoadMatrixf(PGLFloat(@Current.ModelViewMatrix));
        end;
    GL_PROJECTION : begin
        Current.ProjectionMatrix:=MatrixMultiply(Current.ProjectionMatrix, m);
        Current.PMLoaded:=false; glLoadMatrixf(PGLFloat(@Current.ProjectionMatrix));
        end;
    GL_TEXTURE    : begin
        Current.TextureMatrix:=MatrixMultiply(Current.TextureMatrix, m);
        Current.TMLoaded:=false; glLoadMatrixf(PGLFloat(@Current.TextureMatrix));
        end;
    GL_COLOR      : begin
        Current.ColorMatrix:=MatrixMultiply(Current.ColorMatrix, m);
        Current.CMLoaded:=false; glLoadMatrixf(PGLFloat(@Current.ColorMatrix));
        end;
   end;
  end;
end;
}

procedure TGLMatrixCache.Reset;
begin
  glMatrixMode(GL_MODELVIEW);
  FChecked:=true;
end;

procedure glUseProgram(_program: TGLuint);
begin
  if _program<>GLStateCache.FActiveProgram then begin
    GLStateCache.FActiveProgram:=_program;
    OpenGL1x.glUseProgram(_program);
  end;
end;

procedure glGetIntegerv(pname: TGLEnum; params: PGLint);
begin
  case pname of
    GL_CURRENT_PROGRAM: params^:=GLStateCache.FActiveProgram;
  else OpenGL1x.glGetIntegerv(pname,params);
  end;

end;

procedure glBegin(mode: TGLEnum);
{
var Id: cardinal;
begin
 //Адрес возврата как идентификатор буфера рисования
  asm
    mov eax,[esp+12]
    mov [id],eax;
  end;
  CurrentFFPVBO:=GLStateCache.GLVBOManager.GetBuffer(Id);
}
begin
  OpenGL1x.glBegin(mode);
end;

procedure glVertex3f(x, y, z: TGLfloat);
begin
  OpenGL1x.glVertex3f(x, y, z);
end;

{ TGLVBOManager }

constructor TGLVBOManager.Create;
begin
  inherited Create;
  FVBOList:=TList.Create;
  FCurrentAttrib:=TFFPVertexAttrib.Create;
end;

destructor TGLVBOManager.Destroy;
var i: integer;
    FFPBuff: PGLFFPBuffer;
begin
  for i:=0 to FVBOList.Count-1 do begin
    FFPBuff:=FVBOList[i];
    if assigned(FFPBuff) then begin
      if assigned(FFPBuff.BuffData)
      then dispose(FFPBuff.BuffData);
      OpenGL1x.glDeleteBuffers(1,@FFPBuff.VBOId);
      dispose(FFPBuff);
    end;
  end;
  FVBOList.Free; FCurrentAttrib.Free;
  inherited;
end;

function TGLVBOManager.GetBuffer(CallId: cardinal): PGLFFPBuffer;
var i: integer;
    FFPBuff: PGLFFPBuffer;
begin
  for i:=0 to FVBOList.Count-1 do begin
    FFPBuff:=FVBOList[i];
    if FFPBuff.DrawCallId=CallId then break;
  end;
  if not assigned(FFPBuff) or (FFPBuff.DrawCallId<>CallId) then begin
    new(FFPBuff); FFPBuff.DrawCallId:=CallId; FVBOList.Add(FFPBuff);
    FFPBuff.VBOId:=0; FFPBuff.PrimType:=$FFFFFFFF;
    FFPBuff.BuffData:=nil;
  end else result:=FFPBuff;
end;

{ TFFPVertexAttrib }

procedure TFFPVertexAttrib.Color3f(r, g, b: single);
begin
  fColor[0]:=r; fColor[1]:=g; fColor[2]:=b; fColor[3]:=1;
  include(FUsed,ffColor);
end;

procedure TFFPVertexAttrib.Color3fv(c: PAffineVector);
begin
  fColor:=VectorMake(c^,1); include(FUsed,ffColor);
end;

procedure TFFPVertexAttrib.Color4f(r, g, b, a: single);
begin
  fColor[0]:=r; fColor[1]:=g; fColor[2]:=b; fColor[3]:=a;
  include(FUsed,ffColor);
end;

procedure TFFPVertexAttrib.Color4fv(c: PVector);
begin
  fColor:=c^; include(FUsed,ffColor);
end;

constructor TFFPVertexAttrib.Create;
var i: integer;
begin
  fUsed:=[];
  fVertex:=NullVector; fNormal:=NullVector; fColor:=NullHmgVector;
  for i:=0 to 7 do begin fTexCoords[i]:=NullHmgVector; tcCount[i]:=0; end;
  vcCount:=0;
end;

function TFFPVertexAttrib.FindAttr(const buff: PGLFFPBuffer;
  AType: TFFPAttrType): integer;
var i: integer;
begin
  for i:=0 to length(buff.Attribs)-1 do
    if buff.Attribs[i].AttrType=AType then begin
      result:=i; exit;
    end;
  result:=-1;
end;

procedure TFFPVertexAttrib.Normal3f(x, y, z: single);
begin
  fNormal[0]:=x; fNormal[1]:=y; fNormal[2]:=z;
  include(FUsed,ffNormal);
end;

procedure TFFPVertexAttrib.Normal3fv(v: PAffineVector);
begin
  fNormal:=v^; include(FUsed,ffNormal);
end;

procedure TFFPVertexAttrib.PutAttrib(var Buff: PGLFFPBuffer);
var i,n: integer;
begin
  assert((ffVertex in FUsed),'There is no Vertex attribute');
  for i:=0 to length(buff.Attribs)-1 do begin
    case buff.Attribs[i].AttrType of
      ffVertex: PutFloat(fVertex,buff.BuffData,vcCount);
      ffNormal: PutFloat(fNormal,buff.BuffData,3);
      ffColor: PutFloat(fNormal,buff.BuffData,4);
      ffTexCoord0..ffTexCoord7: begin
        n:=cMultiTexIndex[buff.Attribs[i].AttrType];
        PutFloat(fTexCoords[n],buff.BuffData,tcCount[n]);
      end;
    end;
  end;
end;

procedure TFFPVertexAttrib.PutFloat(const SourceBuff; var DestBuff: pointer;
  Count: byte);
var i: integer;
begin
  for i:=0 to Count-1 do begin
    PSingle(DestBuff)^:=TSingleArray(SourceBuff)[i]; inc(PSingle(DestBuff));
  end;
end;

procedure TFFPVertexAttrib.TexCoord1f(s: single; Target: byte);
begin
  fTexCoords[Target]:=VectorMake(s,0,0,0);
  include(FUsed,cMultiTexCoords[Target]);
  if tcCount[Target]<1 then tcCount[Target]:=1;
end;

procedure TFFPVertexAttrib.TexCoord1fv(s: Psingle; Target: byte);
begin
  fTexCoords[Target]:=VectorMake(s^,0,0,0);
  include(FUsed,cMultiTexCoords[Target]);
  if tcCount[Target]<1 then tcCount[Target]:=1;
end;

procedure TFFPVertexAttrib.TexCoord2f(s, t: single; Target: byte);
begin
  fTexCoords[Target]:=VectorMake(s,t,0,0);
  include(FUsed,cMultiTexCoords[Target]);
  if tcCount[Target]<2 then tcCount[Target]:=2;
end;

procedure TFFPVertexAttrib.TexCoord2fv(v: PTexPoint; Target: byte);
begin
  fTexCoords[Target]:=VectorMake(v.S,v.T,0,0);
  include(FUsed,cMultiTexCoords[Target]);
  if tcCount[Target]<2 then tcCount[Target]:=2;
end;

procedure TFFPVertexAttrib.TexCoord3f(s, t, p: single; Target: byte);
begin
  fTexCoords[Target]:=VectorMake(s,t,p,0);
  include(FUsed,cMultiTexCoords[Target]);
  if tcCount[Target]<3 then tcCount[Target]:=3;
end;

procedure TFFPVertexAttrib.TexCoord3fv(v: PAffineVector; Target: byte);
begin
  fTexCoords[Target]:=VectorMake(v^,0);
  include(FUsed,cMultiTexCoords[Target]);
  if tcCount[Target]<3 then tcCount[Target]:=3;
end;

procedure TFFPVertexAttrib.TexCoord4f(s, t, p, q: single; Target: byte);
begin
  fTexCoords[Target]:=VectorMake(s,t,p,q);
  include(FUsed,cMultiTexCoords[Target]);
  if tcCount[Target]<4 then tcCount[Target]:=4;
end;

procedure TFFPVertexAttrib.TexCoord4fv(v: PVector; Target: byte);
begin
  fTexCoords[Target]:=v^;
  include(FUsed,cMultiTexCoords[Target]);
  if tcCount[Target]<4 then tcCount[Target]:=4;
end;

procedure TFFPVertexAttrib.Vertex2f(x, y: single);
begin
  fVertex[0]:=x; fVertex[1]:=y;
  include(FUsed,ffVertex);
  if vcCount<2 then vcCount:=2;
end;

procedure TFFPVertexAttrib.Vertex2fv(v: PTexPoint);
begin
  fVertex[0]:=v.S; fVertex[1]:=v.T;
  include(FUsed,ffVertex);
  if vcCount<2 then vcCount:=2;
end;

procedure TFFPVertexAttrib.Vertex3f(x, y, z: single);
begin
  fVertex[0]:=x; fVertex[1]:=y; fVertex[2]:=z;
  include(FUsed,ffVertex);
  if vcCount<3 then vcCount:=3;
end;

procedure TFFPVertexAttrib.Vertex3fv(v: PAffineVector);
begin
  fVertex:=v^; include(FUsed,ffVertex);
  if vcCount<3 then vcCount:=3;
end;

{ TGLShaderCache }

procedure TGLShaderCache.Assign(ShaderCache: TGLShaderCache);
begin
  FShadersStack:=ShaderCache.FShadersStack;
  FStackTop:=ShaderCache.FStackTop;
end;

procedure TGLShaderCache.Clear;
var i: integer;
begin
  FStackTop:=-1;
  for i:=0 to high(FShadersStack) do FShadersStack[i]:=0;
end;

constructor TGLShaderCache.Create;
begin
  Clear;
end;

procedure TGLShaderCache.GetCurrentGLStates;
begin
  OpenGL1x.glGetIntegerv(GL_CURRENT_PROGRAM,@GLStateCache.FActiveProgram);
end;

function TGLShaderCache.Last: cardinal;
begin
  assert(FStackTop>=0,'Shader stack empty');
  result:=FShadersStack[FStackTop];
end;

function TGLShaderCache.Pop: cardinal;
begin
  assert(FStackTop>=0,'Shader stack empty');
  result:=FShadersStack[FStackTop];
  FShadersStack[FStackTop]:=0; dec(FStackTop);
end;

procedure TGLShaderCache.Push(ShaderId: Cardinal);
begin
  assert(FStackTop<32,'Shader stack full');
  inc(FStackTop); FShadersStack[FStackTop]:=ShaderId;
end;

procedure TGLShaderCache.Reset;
begin
  Clear; glUseProgram(0);
end;

procedure TGLShaderCache.StoreTo(ShaderCache: TGLShaderCache);
begin
  ShaderCache.FShadersStack:=FShadersStack;
  ShaderCache.FStackTop:=FStackTop;
end;

initialization
  GLStateCache:=TGLStateCache.Create;
  StateStack:=TList.Create;
finalization
  //if StateStack.Count>0 then GLStateCache.PopStates;
  FreeStack(StateStack);
  GLStateCache.Free;
end.
