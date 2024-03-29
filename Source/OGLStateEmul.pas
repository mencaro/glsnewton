{: OGLStateEmul
	Historique:
        27/09/10 - Fantom - ���������� ����������� ������ � ������ glMaterial*
}

{TODO:
      - ��������� ��� � ������������ ������� � ����������� ��������� ����� (glGetLight = glLightfv*ViewMatrix)
      - �������� �������� glBegin/End
      - �������� � TFFPVertexAttrib.PutAttrib �������� ����� �������� ��� ������ ������
      + ������������ ������ � ��������� (Push/Pop,Multiple,Load,Identity)
      ----������ � Pop/Scale/Translate/Rotate ����� �������� ����� glLoadMatrix,
      ----��������� �������� ������������� ������� �� ������� ���������
      - ������������ ������� glGet*,glIs*, glEnable/Disable
      - ������������ �������� glPush/PopAttrib
      - ������������ ��������� ���������� (������� �����, ����������� ������ ������ � �.�.)
      - �������� glBegin/End �� glBeginVBO/glEndVBO
      - ����������� ���� ����������, ����������� � ���������
      - ��������� ClientState � ������ gl*Pointer
      - �������� ����������� �������� � ������������ �������� (glGet*(SE_Texture_Target))
      - �������� ����� ������������ ������ � ����� ����� �����������?
      - ������� ������ ����������� ������� (��� ������ ��������� � ������� �������� �����
        �������� � ������ ��������� ��������� ��� � ���������� ��������� ����������.
        ���� ���������� ������ - ������������� ���������(��� ����������) ������ ������
        ������ � ���������� � ������ ����������. ��� ����������� ���������� ��������
        ���������� ���� ������ �������
}

{.$DEFINE DEBUG_STATES} //��������� ����������� ��������� ��������� ��� ��� ������ �������������� ������

unit OGLStateEmul;

interface

Uses
   {$IFNDEF DIRECTGL}OpenGL1x, {$ELSE} dglOpenGL, {$ENDIF}
   VectorTypes, VectorGeometry, Classes, SysUtils, uMiscUtils, uLogs;

Type

   TEnStates = (sAlphaTest,sAutoNormal,sBlend,sColorLogicOp,sColorMaterial,sColorSum,
                sCullFace,sDepthTest,sDither,
                sFog,sIndexLogicOp,sLighting,sLineSmooth,sLineStipple,
                sMultisample,sNormalize,sPointSmooth,sPointSprite,sRescaleNormal,
                sSampleAlphaToCoverage, sScissorTest,sStencilTest,sVertexProgramPointSize,
                sVertexProgramTwoSide,sNone);
   TIntegerVector = array[0..3] of integer;
   PIntegerVector = ^TIntegerVector;

   TDebugState = record
     StateName: string;
     StateValue: string;
     CacheValue: string;
   end;

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
       procedure SnapState(const Log: TStringList);
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
       procedure SnapState(const Log: TStringList);
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
       procedure SnapState(const Log: TStringList);
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
       procedure SnapState(const Log: TStringList);
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
       procedure SnapState(const Log: TStringList);
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
    MatrixStackDepth: integer = 0;
    {$IFDEF DEBUG_STATES}
    vStateLog: TStringList;
    {$ENDIF}

implementation

var CurrentFFPVBO: PGLFFPBuffer;

procedure glPushMatrix;
begin
  inc(MatrixStackDepth);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glPushMatrix;
end;
procedure glPopMatrix;
begin
  dec(MatrixStackDepth);
  assert(MatrixStackDepth>=0,'Matrix Stack is Empty');
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glPopMatrix;
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
    {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glUseProgram(spId); GLStateCache.FActiveProgram:=spId;
  end;
end;

function GetActiveContext: LongInt;
begin
  result:={$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}wglGetCurrentContext;
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
//   {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_DEPTH_RANGE, @DepthRange);
   glPixelTransferf(GL_DEPTH_SCALE,DepthCache.DepthScale);
   glPixelTransferf(GL_DEPTH_BIAS,DepthCache.DepthBias);
   glClearDepth(DepthCache.DepthClearValue);
   DepthBits:=DepthCache.DepthBits;
end;

procedure TGLDepthCache.GetCurrentGLStates;
begin
  Enabled:={$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glisEnabled(GL_DEPTH_TEST);
  if Enabled then include(GLStateCache.States,sDepthTest) else exclude(GLStateCache.States, sDepthTest);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_DEPTH_WRITEMASK, @DepthMask);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_DEPTH_FUNC, @DepthFunc);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_DEPTH_RANGE, @DepthRange);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetFloatv(GL_DEPTH_SCALE, @DepthScale);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetFloatv(GL_DEPTH_BIAS, @DepthBias);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetFloatv(GL_DEPTH_CLEAR_VALUE, @DepthClearValue);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_DEPTH_BITS, @DepthBits);
  FChecked:=true;
  {$IFDEF DEBUG_STATES} SnapState(vStateLog); {$ENDIF}
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
      if {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glisEnabled(CEnStates[state]) then
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

procedure TGLDepthCache.SnapState(const Log: TStringList);
var b: boolean;
    iv: array [0..3] of integer;
    fv: array [0..3] of single;
    i: integer;
    c: cardinal;
    f: single;
const bool: array[false..true] of string = ('false', 'true');
begin
  b:={$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glisEnabled(GL_DEPTH_TEST);
  if b<>(sDepthTest in GLStateCache.States) then
    log.Add('GL_DEPTH_TEST'+#9+#9+bool[b]+#9+bool[sDepthTest in GLStateCache.States]);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_DEPTH_WRITEMASK, @b);
  if b<>DepthMask then
    log.Add('GL_DEPTH_WRITEMASK'+#9+#9+bool[b]+#9+bool[DepthMask]);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_DEPTH_FUNC, @c);
  if c<>DepthFunc then
    log.Add('GL_DEPTH_FUNC'+#9+#9+inttostr(c)+#9+inttostr(DepthFunc));
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_DEPTH_RANGE, @c);
  if c<>DepthRange then
    log.Add('GL_DEPTH_RANGE'+#9+#9+inttostr(c)+#9+inttostr(DepthRange));
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetFloatv(GL_DEPTH_SCALE, @f);
  if f<>DepthScale then
    log.Add('GL_DEPTH_SCALE'+#9+#9+floattostr(f)+#9+floattostr(DepthScale));
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetFloatv(GL_DEPTH_BIAS, @f);
  if f<>DepthBias then
    log.Add('GL_DEPTH_BIAS'+#9+#9+floattostr(f)+#9+floattostr(DepthBias));
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetFloatv(GL_DEPTH_CLEAR_VALUE, @f);
  if f<>DepthClearValue then
    log.Add('GL_DEPTH_CLEAR_VALUE'+#9+#9+floattostr(f)+#9+floattostr(DepthClearValue));
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_DEPTH_BITS, @i);
  if i<>DepthBits then
    log.Add('GL_DEPTH_BITS'+#9+#9+inttostr(i)+#9+inttostr(DepthBits));
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
     else result:={$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glIsEnabled(state);
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
    if Value then {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glEnable(state) else {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glDisable(state);
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
  result:={$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glIsEnabled(cap);
end;

procedure glEnable(cap: TGLEnum);
var state: TEnStates;
begin
  state:=StateByEnum(cap);
  case cap of
    GL_Texture_1D,GL_Texture_2D,GL_Texture_3D,GL_TEXTURE_CUBE_MAP,
    GL_TEXTURE_RECTANGLE: begin
       SetTextureStates(cap,true);exit;
    end;
    GL_LIGHT0..GL_LIGHT7: begin
      GLStateCache.LightingCache.Lights[cap-GL_LIGHT0].Enabled:=true;
      {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glEnable(cap);
      exit;
    end;
  end;

  if state<>sNone then begin
     if not (state in GLStateCache.States) then begin
        {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glEnable(cap);
        GLStateCache.States:=GLStateCache.States+[state];
        exit;
     end else exit;
  end;
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glEnable(cap);
end;

procedure glDisable(cap: TGLEnum);
var state: TEnStates;
begin

  state:=StateByEnum(cap);
  case cap of
    GL_Texture_1D,GL_Texture_2D,GL_Texture_3D,GL_TEXTURE_CUBE_MAP,
    GL_TEXTURE_RECTANGLE: begin
       SetTextureStates(cap,false);
       exit;
    end;
    GL_LIGHT0..GL_LIGHT7: begin
      GLStateCache.LightingCache.Lights[cap-GL_LIGHT0].Enabled:=false;
      {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glDisable(cap);
    end;

  end;
  if state<>sNone then begin
     if (state in GLStateCache.States) then begin
        {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glDisable(cap);
        GLStateCache.States:=GLStateCache.States-[state];
        exit;
     end else exit;
  end;
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glDisable(cap);
end;

procedure glClearDepth(depth: TGLclampd);
begin
  if GLStateCache.DepthCache.DepthClearValue<>depth then
  begin
     {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glClearDepth(depth);
     GLStateCache.DepthCache.DepthClearValue:=depth;
  end;
end;

procedure glDepthFunc(func: TGLEnum);
begin
  if GLStateCache.DepthCache.DepthFunc<>func then
  begin
     {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glDepthFunc(func);
     GLStateCache.DepthCache.DepthFunc:=func;
  end;
end;

procedure glDepthRange(zNear, zFar: TGLclampd);
begin
  if (GLStateCache.DepthCache.DepthNear<>zNear)
  or (GLStateCache.DepthCache.DepthFar<>zFar)then
  begin
     {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glDepthRange(zNear, zFar);
     GLStateCache.DepthCache.DepthNear:=zNear;
     GLStateCache.DepthCache.DepthFar:=zFar;
  end;
end;

procedure glDepthMask(flag: TGLboolean);
begin
  if GLStateCache.DepthCache.DepthMask<>flag then
  begin
     {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glDepthMask(flag);
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
           {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glPixelTransferf(pname, param); exit;
        end;
     GL_DEPTH_BIAS: if param<>GLStateCache.DepthCache.DepthBias then
        begin
           GLStateCache.DepthCache.DepthBias:=param;
           {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glPixelTransferf(pname, param); exit;
        end;
  end;
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glPixelTransferf(pname, param);
end;

procedure glPixelTransferi(pname: TGLEnum; param: TGLint);
begin
  case pname of
     GL_DEPTH_SCALE: if param<>GLStateCache.DepthCache.DepthScale then begin
           GLStateCache.DepthCache.DepthScale:=param;
           {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glPixelTransferi(pname, param); exit;
        end;
     GL_DEPTH_BIAS: if param<>GLStateCache.DepthCache.DepthBias then begin
           GLStateCache.DepthCache.DepthBias:=param;
           {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glPixelTransferi(pname, param); exit;
        end;
  end;
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glPixelTransferi(pname, param);
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
  Enabled:={$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glisEnabled(GL_ALPHA_TEST);
  if Enabled then include(GLStateCache.States,sAlphaTest) else exclude(GLStateCache.States, sAlphaTest);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_ALPHA_TEST_FUNC,@Func);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetFloatv(GL_ALPHA_TEST_REF,@Ref);
  FChecked:=true;
  {$IFDEF DEBUG_STATES} SnapState(vStateLog); {$ENDIF}
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
    {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glAlphaFunc(func, ref);
    GLStateCache.AlphaCache.Func:=func;
    GLStateCache.AlphaCache.Ref:=ref;
  end;
end;

procedure TGLAlphaCache.SnapState(const Log: TStringList);
var b: boolean;
    iv: array [0..3] of integer;
    fv: TVector;
    i: integer;
    c: cardinal;
    f: single;
const bool: array[false..true] of string = ('false', 'true');
begin
  b:={$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glisEnabled(GL_ALPHA_TEST);
  if b<>(sAlphaTest in GLStateCache.States) then
    log.Add('GL_ALPHA_TEST'+#9+#9+bool[b]+#9+bool[sAlphaTest in GLStateCache.States]);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_ALPHA_TEST_FUNC,@c);
  if c<>Func then
    log.Add('GL_ALPHA_TEST_FUNC'+#9+#9+inttostr(c)+#9+inttostr(Func));
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetFloatv(GL_ALPHA_TEST_REF,@f);
  if f<>Ref then
    log.Add('GL_ALPHA_TEST_REF'+#9+#9+floattostr(f)+#9+floattostr(Ref));
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
  Enabled:={$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glisEnabled(GL_BLEND);
  if Enabled then include(GLStateCache.States,sBlend) else exclude(GLStateCache.States, sBlend);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_BLEND_SRC,@SFactor);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_BLEND_DST,@DFactor);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetFloatv(GL_BLEND_COLOR,@BlendColor);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_BLEND_DST_ALPHA,@BlendDstAlpha);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_BLEND_DST_RGB,@BlendDstRGB);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_BLEND_SRC_ALPHA,@BlendSRCAlpha);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_BLEND_SRC_RGB,@BlendSRCRGB);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_BLEND_EQUATION_ALPHA,@BlendEquationAlpha);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_BLEND_EQUATION_RGB,@BlendEquationRGB);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_BLEND_EQUATION,@BlendEquation);
  FChecked:=true;
  {$IFDEF DEBUG_STATES} SnapState(vStateLog); {$ENDIF}
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
    {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glBlendFunc(sfactor, dfactor);
    GLStateCache.BlendingCache.SFactor:=sfactor;
    GLStateCache.BlendingCache.DFactor:=dfactor;
  end;
end;

procedure glBlendColor(red, green, blue, alpha: TGLclampf);
begin
  if not VectorEquals(GLStateCache.BlendingCache.BlendColor,
                      vectormake(red, green, blue, alpha))
  then begin
    {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glBlendColor(red, green, blue, alpha);
    GLStateCache.BlendingCache.BlendColor:=vectormake(red, green, blue, alpha);
  end;
end;

procedure glBlendEquation(mode: TGLEnum);
begin
  if GLStateCache.BlendingCache.BlendEquation <> mode
  then begin
    {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glBlendEquation(mode);
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
    {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glBlendFuncSeparate(sfactorRGB, dfactorRGB, sfactorAlpha, dfactorAlpha);
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
    {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glBlendEquationSeparate(modeRGB, modeAlpha);
    GLStateCache.BlendingCache.BlendEquationRGB := modeRGB;
    GLStateCache.BlendingCache.BlendEquationAlpha := modeAlpha;
  end;
end;

procedure TGLBlendingCache.SnapState(const Log: TStringList);
var b: boolean;
    iv: array [0..3] of integer;
    fv: TVector;
    i: integer;
    c: cardinal;
    f: single;
const bool: array[false..true] of string = ('false', 'true');
begin
  b:={$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glisEnabled(GL_BLEND);
  if b<>(sBlend in GLStateCache.States) then
    log.Add('GL_BLEND'+#9+#9+bool[b]+#9+bool[sBlend in GLStateCache.States]);

  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_BLEND_SRC,@c);
  if c<>SFactor then
    log.Add('GL_BLEND_SRC'+#9+#9+inttostr(c)+#9+inttostr(SFactor));
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_BLEND_DST,@c);
  if c<>DFactor then
    log.Add('GL_BLEND_DST'+#9+#9+inttostr(c)+#9+inttostr(DFactor));

  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetFloatv(GL_BLEND_COLOR,@fv);
  if not VectorEquals(fv, BlendColor) then
    log.Add('GL_BLEND_COLOR'+#9+#9+Vectortostr(fv)+#9+Vectortostr(BlendColor));
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_BLEND_DST_ALPHA,@c);
  if c<>BlendDstAlpha then
    log.Add('GL_BLEND_DST_ALPHA'+#9+#9+inttostr(c)+#9+inttostr(BlendDstAlpha));
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_BLEND_DST_RGB,@c);
  if c<>BlendDstRGB then
    log.Add('GL_BLEND_DST_RGB'+#9+#9+inttostr(c)+#9+inttostr(BlendDstRGB));
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_BLEND_SRC_ALPHA,@c);
  if c<>BlendSRCAlpha then
    log.Add('GL_BLEND_SRC_ALPHA'+#9+#9+inttostr(c)+#9+inttostr(BlendSRCAlpha));
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_BLEND_SRC_RGB,@c);
  if c<>BlendSRCRGB then
    log.Add('GL_BLEND_SRC_RGB'+#9+#9+inttostr(c)+#9+inttostr(BlendSRCRGB));
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_BLEND_EQUATION_ALPHA,@c);
  if c<>BlendEquationAlpha then
    log.Add('GL_BLEND_EQUATION_ALPHA'+#9+#9+inttostr(c)+#9+inttostr(BlendEquationAlpha));
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_BLEND_EQUATION_RGB,@c);
  if c<>BlendEquationRGB then
    log.Add('GL_BLEND_EQUATION_RGB'+#9+#9+inttostr(c)+#9+inttostr(BlendEquationRGB));
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_BLEND_EQUATION,@c);
  if c<>BlendEquation then
    log.Add('GL_BLEND_EQUATION'+#9+#9+inttostr(c)+#9+inttostr(BlendEquation));
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

procedure TGLMaterialCache.SnapState(const Log: TStringList);
var Face, Mode: GLEnum;
    v: TVector;
    f: single;
begin
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetMaterialfv(GL_FRONT, GL_AMBIENT,  @v);
  if not VectorEquals(v,Faces[GL_FRONT].AMBIENT) then
    log.Add('GL_FRONT_AMBIENT'+#9+#9+VectorToStr(v)+#9+VectorToStr(Faces[GL_FRONT].AMBIENT));
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetMaterialfv(GL_FRONT, GL_DIFFUSE,  @v);
  if not VectorEquals(v,Faces[GL_FRONT].DIFFUSE) then
    log.Add('GL_FRONT_DIFFUSE'+#9+#9+VectorToStr(v)+#9+VectorToStr(Faces[GL_FRONT].DIFFUSE));
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetMaterialfv(GL_FRONT, GL_SPECULAR, @v);
  if not VectorEquals(v,Faces[GL_FRONT].SPECULAR) then
    log.Add('GL_FRONT_SPECULAR'+#9+#9+VectorToStr(v)+#9+VectorToStr(Faces[GL_FRONT].SPECULAR));
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetMaterialfv(GL_FRONT, GL_EMISSION, @v);
  if not VectorEquals(v,Faces[GL_FRONT].EMISSION) then
    log.Add('GL_FRONT_EMISSION'+#9+#9+VectorToStr(v)+#9+VectorToStr(Faces[GL_FRONT].EMISSION));
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetMaterialfv(GL_FRONT, GL_SHININESS,@f);
  if f<>Faces[GL_FRONT].SHININESS then
    log.Add('GL_FRONT_SHININESS'+#9+#9+FloatToStr(f)+#9+FloatToStr(Faces[GL_FRONT].SHININESS));

  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetMaterialfv(GL_BACK, GL_AMBIENT,  @v);
  if not VectorEquals(v,Faces[GL_BACK].AMBIENT) then
    log.Add('GL_BACK_AMBIENT'+#9+#9+VectorToStr(v)+#9+VectorToStr(Faces[GL_BACK].AMBIENT));
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetMaterialfv(GL_BACK, GL_DIFFUSE,  @v);
  if not VectorEquals(v,Faces[GL_BACK].DIFFUSE) then
    log.Add('GL_BACK_DIFFUSE'+#9+#9+VectorToStr(v)+#9+VectorToStr(Faces[GL_BACK].DIFFUSE));
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetMaterialfv(GL_BACK, GL_SPECULAR, @v);
  if not VectorEquals(v,Faces[GL_BACK].SPECULAR) then
    log.Add('GL_BACK_SPECULAR'+#9+#9+VectorToStr(v)+#9+VectorToStr(Faces[GL_BACK].SPECULAR));
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetMaterialfv(GL_BACK, GL_EMISSION, @v);
  if not VectorEquals(v,Faces[GL_BACK].EMISSION) then
    log.Add('GL_BACK_EMISSION'+#9+#9+VectorToStr(v)+#9+VectorToStr(Faces[GL_BACK].EMISSION));
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetMaterialfv(GL_BACK, GL_SHININESS,@f);
  if f<>Faces[GL_BACK].SHININESS then
    log.Add('GL_BACK_SHININESS'+#9+#9+FloatToStr(f)+#9+FloatToStr(Faces[GL_BACK].SHININESS));

  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_COLOR_MATERIAL_FACE, @Face);
  if Face<>CurrentFace then
    log.Add('GL_COLOR_MATERIAL_FACE'+#9+#9+IntToStr(Face)+#9+inttostr(CurrentFace));
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_COLOR_MATERIAL_PARAMETER, @Mode);
  if Mode<>FaceMode then
    log.Add('GL_COLOR_MATERIAL_PARAMETER'+#9+IntToStr(mode)+#9+inttostr(FaceMode));
end;

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
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetMaterialfv(GL_FRONT, GL_AMBIENT,  @Faces[GL_FRONT].AMBIENT);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetMaterialfv(GL_FRONT, GL_DIFFUSE,  @Faces[GL_FRONT].DIFFUSE);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetMaterialfv(GL_FRONT, GL_SPECULAR, @Faces[GL_FRONT].SPECULAR);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetMaterialfv(GL_FRONT, GL_EMISSION, @Faces[GL_FRONT].EMISSION);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetMaterialfv(GL_FRONT, GL_SHININESS,@Faces[GL_FRONT].SHININESS);

  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetMaterialfv(GL_BACK, GL_AMBIENT, @Faces[GL_BACK].AMBIENT);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetMaterialfv(GL_BACK, GL_DIFFUSE, @Faces[GL_BACK].DIFFUSE);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetMaterialfv(GL_BACK, GL_SPECULAR, @Faces[GL_BACK].SPECULAR);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetMaterialfv(GL_BACK, GL_EMISSION, @Faces[GL_BACK].EMISSION);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetMaterialfv(GL_BACK, GL_SHININESS, @Faces[GL_BACK].SHININESS);

  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_COLOR_MATERIAL_FACE, @Face);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_COLOR_MATERIAL_PARAMETER, @Mode);
  if Face = GL_FRONT_AND_BACK then begin
     Faces[GL_FRONT].ColorMaterialMode:=mode;
     Faces[GL_BACK].ColorMaterialMode:=mode;
  end else Faces[face].ColorMaterialMode:=mode;
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
  {$IFDEF DEBUG_STATES} SnapState(vStateLog); {$ENDIF}
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
                     {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glMaterialf(face, pname, param);
                    end;
      Else {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glMaterialf(face, pname, param);
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
                      {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glMaterialf(face, pname, param);
                    end;
      Else {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glMaterialf(face, pname, param);
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
                     {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glMaterialfv(face, pname, params);
                  end;
      GL_DIFFUSE: if not VectorEquals(DIFFUSE, PVector(params)^) then begin
                     DIFFUSE:=PVector(params)^;
                     iDIFFUSE:=FloatTointVector(DIFFUSE);
                     {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glMaterialfv(face, pname, params);
                  end;
      GL_SPECULAR: if not VectorEquals(SPECULAR, PVector(params)^) then begin
                     SPECULAR:=PVector(params)^;
                     iSPECULAR:=FloatTointVector(SPECULAR);
                     {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glMaterialfv(face, pname, params);
                  end;
      GL_EMISSION: if not VectorEquals(EMISSION, PVector(params)^) then begin
                     EMISSION:=PVector(params)^;
                     iEMISSION:=FloatTointVector(EMISSION);
                     {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glMaterialfv(face, pname, params);
                  end;
      GL_SHININESS: if SHININESS<>params^ then begin
                     SHININESS:=trunc(params^); iSHININESS:=trunc(params^);
                     {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glMaterialfv(face, pname, params);
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
                     {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glMaterialfv(face, pname, params);
                  end;
      GL_DIFFUSE: if (not VectorEquals(Faces[GL_FRONT].DIFFUSE, PVector(params)^))
                  or (not VectorEquals(Faces[GL_BACK].DIFFUSE, PVector(params)^)) then begin
                     Faces[GL_FRONT].DIFFUSE:=PVector(params)^;
                     Faces[GL_BACK].DIFFUSE:=PVector(params)^;
                     Faces[GL_FRONT].iDIFFUSE:=FloatTointVector(Faces[GL_FRONT].DIFFUSE);
                     Faces[GL_BACK].iDIFFUSE:=FloatTointVector(Faces[GL_BACK].DIFFUSE);
                     {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glMaterialfv(face, pname, params);
                  end;
      GL_SPECULAR:if (not VectorEquals(Faces[GL_FRONT].SPECULAR, PVector(params)^))
                  or (not VectorEquals(Faces[GL_BACK].SPECULAR, PVector(params)^)) then begin
                     Faces[GL_FRONT].SPECULAR:=PVector(params)^;
                     Faces[GL_BACK].SPECULAR:=PVector(params)^;
                     Faces[GL_FRONT].iSPECULAR:=FloatTointVector(Faces[GL_FRONT].SPECULAR);
                     Faces[GL_BACK].iSPECULAR:=FloatTointVector(Faces[GL_BACK].SPECULAR);
                     {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glMaterialfv(face, pname, params);
                  end;
      GL_EMISSION:if (not VectorEquals(Faces[GL_FRONT].EMISSION, PVector(params)^))
                  or (not VectorEquals(Faces[GL_BACK].EMISSION, PVector(params)^)) then begin
                     Faces[GL_FRONT].EMISSION:=PVector(params)^;
                     Faces[GL_BACK].EMISSION:=PVector(params)^;
                     Faces[GL_FRONT].iEMISSION:=FloatTointVector(Faces[GL_FRONT].EMISSION);
                     Faces[GL_BACK].iEMISSION:=FloatTointVector(Faces[GL_BACK].EMISSION);
                     {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glMaterialfv(face, pname, params);
                  end;
      GL_SHININESS:if (Faces[GL_FRONT].SHININESS<>params^)
                   or (Faces[GL_BACK].SHININESS<>params^) then begin
                     Faces[GL_FRONT].SHININESS:=trunc(params^);
                     Faces[GL_FRONT].iSHININESS:=trunc(params^);
                     Faces[GL_BACK].SHININESS:=trunc(params^);
                     Faces[GL_BACK].iSHININESS:=trunc(params^);
                     {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glMaterialfv(face, pname, params);
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
                     {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glMateriali(face, pname, param);
                    end;
      Else {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glMateriali(face, pname, param);
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
                     {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glMateriali(face, pname, param);
                    end;
      Else {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glMateriali(face, pname, param);
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
                     {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glMaterialiv(face, pname, params);
                  end;
      GL_DIFFUSE: if not VectorEquals(iDIFFUSE, PIntegerVector(params)^) then begin
                     iDIFFUSE:=PIntegerVector(params)^;
                     DIFFUSE:=IntToFloatVector(iDIFFUSE);
                     {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glMaterialiv(face, pname, params);
                  end;
      GL_SPECULAR: if not VectorEquals(iSPECULAR, PIntegerVector(params)^) then begin
                     iSPECULAR:=PIntegerVector(params)^;
                     SPECULAR:=IntToFloatVector(iSPECULAR);
                     {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glMaterialiv(face, pname, params);
                  end;
      GL_EMISSION: if not VectorEquals(iEMISSION, PIntegerVector(params)^) then begin
                     iEMISSION:=PIntegerVector(params)^;
                     EMISSION:=IntToFloatVector(iEMISSION);
                     {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glMaterialiv(face, pname, params);
                  end;
      GL_SHININESS: if iSHININESS<>params^ then begin
                     iSHININESS:=params^; SHININESS:=params^;
                     {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glMaterialiv(face, pname, params);
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
                       {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glMaterialiv(face, pname, params);
                    end;
        GL_DIFFUSE: if (not VectorEquals(Faces[GL_FRONT].iDIFFUSE, PIntegerVector(params)^))
                    or (not VectorEquals(Faces[GL_BACK].iDIFFUSE, PIntegerVector(params)^))
                    then begin
                       Faces[GL_FRONT].iDIFFUSE:=PIntegerVector(params)^;
                       Faces[GL_FRONT].DIFFUSE:=IntToFloatVector(Faces[GL_FRONT].iDIFFUSE);
                       Faces[GL_BACK].iDIFFUSE:=Faces[GL_FRONT].iDIFFUSE;
                       Faces[GL_BACK].DIFFUSE:=Faces[GL_FRONT].DIFFUSE;
                       {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glMaterialiv(face, pname, params);
                    end;
        GL_SPECULAR:if (not VectorEquals(Faces[GL_FRONT].iSPECULAR, PIntegerVector(params)^))
                    or (not VectorEquals(Faces[GL_BACK].iSPECULAR, PIntegerVector(params)^))
                    then begin
                       Faces[GL_FRONT].iSPECULAR:=PIntegerVector(params)^;
                       Faces[GL_FRONT].SPECULAR:=IntToFloatVector(Faces[GL_FRONT].iSPECULAR);
                       Faces[GL_BACK].iSPECULAR:=Faces[GL_FRONT].iSPECULAR;
                       Faces[GL_BACK].SPECULAR:=Faces[GL_FRONT].SPECULAR;
                       {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glMaterialiv(face, pname, params);
                    end;
        GL_EMISSION:if (not VectorEquals(Faces[GL_FRONT].iEMISSION, PIntegerVector(params)^))
                    or (not VectorEquals(Faces[GL_BACK].iEMISSION, PIntegerVector(params)^))
                    then begin
                       Faces[GL_FRONT].iEMISSION:=PIntegerVector(params)^;
                       Faces[GL_FRONT].EMISSION:=IntToFloatVector(Faces[GL_FRONT].iEMISSION);
                       Faces[GL_BACK].iEMISSION:=Faces[GL_FRONT].iEMISSION;
                       Faces[GL_BACK].EMISSION:=Faces[GL_FRONT].EMISSION;
                       {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glMaterialiv(face, pname, params);
                    end;
        GL_SHININESS:if (Faces[GL_FRONT].iSHININESS<>params^)
                     or (Faces[GL_BACK].iSHININESS<>params^)
                    then begin
                        Faces[GL_FRONT].iSHININESS:=params^;
                        Faces[GL_FRONT].SHININESS:=params^;
                        Faces[GL_BACK].iSHININESS:=params^;
                        Faces[GL_BACK].SHININESS:=params^;
                        {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glMaterialiv(face, pname, params);
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
      FaceMode:=mode;
      Faces[face].ColorMaterialMode:=mode;
      GLStateCache.MaterialCache.CurrentFace:=face;
      {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glColorMaterial(face,mode);
   end;
  end else
  with GLStateCache.MaterialCache do begin
    if (Faces[GL_FRONT].ColorMaterialMode<>mode)
    or (Faces[GL_BACK].ColorMaterialMode<>mode)
    or (FaceMode<>face) then begin
      FaceMode:=mode;
      Faces[GL_FRONT].ColorMaterialMode:=mode;
      Faces[GL_BACK].ColorMaterialMode:=mode;
      GLStateCache.MaterialCache.CurrentFace:=face;
      {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glColorMaterial(face,mode);
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

  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightModelfv(GL_LIGHT_MODEL_AMBIENT, @LightingCache.LM_AMBIENT);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightModeliv(GL_LIGHT_MODEL_LOCAL_VIEWER, @LightingCache.LM_LOCAL_VIEWER);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightModeli(GL_LIGHT_MODEL_COLOR_CONTROL, LightingCache.LM_COLOR_CONTROL);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightModeliv(GL_LIGHT_MODEL_TWO_SIDE, @LightingCache.LM_TWO_SIDE);

  FChecked:=true;
end;

procedure TGLLightingCache.GetCurrentGLStates;
var LId,i: integer;
begin
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_MAX_LIGHTS,@MaxLights);
  Enabled:={$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glIsEnabled(GL_LIGHTING);
  if Enabled then include(GLStateCache.States,sLighting) else exclude(GLStateCache.States,sLighting);
  if length(Lights)<>MaxLights then SetLength(Lights,MaxLights);
  for i:=0 to MaxLights-1 do with Lights[i] do begin
      LId:=GL_LIGHT0+i; Lights[i].Enabled:={$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glIsEnabled(LId);
      {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetLightfv(LId, GL_SPOT_EXPONENT, @SPOT_EXPONENT);
      {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetLightfv(LId, GL_SPOT_CUTOFF, @SPOT_CUTOFF);
      {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetLightfv(LId, GL_CONSTANT_ATTENUATION, @CONSTANT_ATTENUATION);
      {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetLightfv(LId, GL_LINEAR_ATTENUATION, @LINEAR_ATTENUATION);
      {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetLightfv(LId, GL_QUADRATIC_ATTENUATION, @QUADRATIC_ATTENUATION);
      {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetLightfv(LId, GL_AMBIENT, @AMBIENT);
      {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetLightfv(LId, GL_DIFFUSE, @DIFFUSE);
      {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetLightfv(LId, GL_SPECULAR, @SPECULAR);
      //{$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetLightfv(LId, GL_POSITION, @POSITION);
      //{$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetLightfv(LId, GL_SPOT_DIRECTION, @SPOT_DIRECTION);
  end;

  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetFloatv(GL_LIGHT_MODEL_AMBIENT, @LM_AMBIENT);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetBooleanv(GL_LIGHT_MODEL_LOCAL_VIEWER, @LM_LOCAL_VIEWER);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_LIGHT_MODEL_COLOR_CONTROL,@LM_COLOR_CONTROL);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetBooleanv(GL_LIGHT_MODEL_TWO_SIDE, @LM_TWO_SIDE);

  FChecked:=true;
  {$IFDEF DEBUG_STATES} SnapState(vStateLog); {$ENDIF}
end;

procedure TGLLightingCache.Reset;
var i, LId:integer;
const cWhiteColor: TVector = (1,1,1,1);
begin
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_MAX_LIGHTS,@MaxLights);
  SetLength(Lights,MaxLights);
  Enabled:=false; {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glDisable(GL_LIGHTING);
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
    {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glDisable(LId);
    {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightf(LId,GL_SPOT_EXPONENT, SPOT_EXPONENT);
    {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightf(LId,GL_SPOT_CUTOFF, SPOT_CUTOFF);
    {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightf(LId,GL_CONSTANT_ATTENUATION, CONSTANT_ATTENUATION);
    {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightf(LId,GL_LINEAR_ATTENUATION, LINEAR_ATTENUATION);
    {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightf(LId,GL_QUADRATIC_ATTENUATION, QUADRATIC_ATTENUATION);
    {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightfv(LId,GL_AMBIENT, @AMBIENT);
    {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightfv(LId,GL_DIFFUSE, @DIFFUSE);
    {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightfv(LId,GL_SPECULAR, @SPECULAR);
    //{$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightfv(LId,GL_POSITION, @POSITION);
    //{$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightfv(LId,GL_SPOT_DIRECTION, @SPOT_DIRECTION);
    {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glDisable(LId);
  end;

  LM_AMBIENT:=vectormake(0.2,0.2,0.2,1);
  LM_LOCAL_VIEWER:=false;
  LM_COLOR_CONTROL:=GL_SINGLE_COLOR;
  LM_TWO_SIDE:=false;

  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightModelfv(GL_LIGHT_MODEL_AMBIENT, @LM_AMBIENT);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightModeliv(GL_LIGHT_MODEL_LOCAL_VIEWER, @LM_LOCAL_VIEWER);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightModeli(GL_LIGHT_MODEL_COLOR_CONTROL, LM_COLOR_CONTROL);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightModeliv(GL_LIGHT_MODEL_TWO_SIDE, @LM_TWO_SIDE);

  FChecked:=true;
end;

procedure glLightModelf(pname: TGLEnum; param: TGLfloat);
begin
   with GLStateCache.LightingCache do begin
      case pname of
        GL_LIGHT_MODEL_LOCAL_VIEWER: if LM_LOCAL_VIEWER<>(param=GL_TRUE) then begin
                   LM_LOCAL_VIEWER:=(param=GL_TRUE);
                   {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightModelf(pname, param);
                end;
        GL_LIGHT_MODEL_COLOR_CONTROL: if LM_COLOR_CONTROL<>param then begin
                   LM_COLOR_CONTROL:=trunc(param);
                   {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightModelf(pname, param);
                end;
        GL_LIGHT_MODEL_TWO_SIDE: if LM_TWO_SIDE<>(param=GL_TRUE) then begin
                   LM_LOCAL_VIEWER:=(param=GL_TRUE);
                   {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightModelf(pname, param);
                end;
        else {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightModelf(pname, param);
      end;
   end;
end;

procedure glLightModeli(pname: TGLEnum; param: TGLint);
begin
   with GLStateCache.LightingCache do begin
      case pname of
        GL_LIGHT_MODEL_LOCAL_VIEWER: if LM_LOCAL_VIEWER<>(param=GL_TRUE) then begin
                   LM_LOCAL_VIEWER:=(param=GL_TRUE);
                   {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightModeli(pname, param);
                end;
        GL_LIGHT_MODEL_COLOR_CONTROL: if LM_COLOR_CONTROL<>param then begin
                   LM_COLOR_CONTROL:=param;
                   {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightModeli(pname, param);
                end;
        GL_LIGHT_MODEL_TWO_SIDE: if LM_TWO_SIDE<>(param=GL_TRUE) then begin
                   LM_LOCAL_VIEWER:=(param=GL_TRUE);
                   {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightModeli(pname, param);
                end;
        else {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightModeli(pname, param);
      end;
   end;
end;

procedure glLightModelfv(pname: TGLEnum; params: PGLfloat);
begin
   with GLStateCache.LightingCache do begin
      case pname of
        GL_LIGHT_MODEL_LOCAL_VIEWER: if LM_LOCAL_VIEWER<>(params^=GL_TRUE) then begin
                   LM_LOCAL_VIEWER:=(params^=GL_TRUE);
                   {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightModelfv(pname, params);
                end;
        GL_LIGHT_MODEL_COLOR_CONTROL: if LM_COLOR_CONTROL<>params^ then begin
                   LM_COLOR_CONTROL:=trunc(params^);
                   {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightModelfv(pname, params);
                end;
        GL_LIGHT_MODEL_TWO_SIDE: if LM_TWO_SIDE<>(params^=GL_TRUE) then begin
                   LM_LOCAL_VIEWER:=(params^=GL_TRUE);
                   {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightModelfv(pname, params);
                end;
        GL_LIGHT_MODEL_AMBIENT: if not VectorEquals(LM_AMBIENT, PVector(params)^) then begin
                   LM_AMBIENT:=PVector(params)^;
                   {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightModelfv(pname, params);
                end;
        else {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightModelfv(pname, params);
      end;
   end;
end;

procedure glLightModeliv(pname: TGLEnum; params: PGLint);
begin
   with GLStateCache.LightingCache do begin
      case pname of
        GL_LIGHT_MODEL_LOCAL_VIEWER: if LM_LOCAL_VIEWER<>(params^=GL_TRUE) then begin
                   LM_LOCAL_VIEWER:=(params^=GL_TRUE);
                   {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightModeliv(pname, params);
                end;
        GL_LIGHT_MODEL_COLOR_CONTROL: if LM_COLOR_CONTROL<>params^ then begin
                   LM_COLOR_CONTROL:=params^;
                   {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightModeliv(pname, params);
                end;
        GL_LIGHT_MODEL_TWO_SIDE: if LM_TWO_SIDE<>(params^=GL_TRUE) then begin
                   LM_LOCAL_VIEWER:=(params^=GL_TRUE);
                   {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightModeliv(pname, params);
                end;
        GL_LIGHT_MODEL_AMBIENT: if not VectorEquals(LM_AMBIENT, PVector(params)^) then begin
                   LM_AMBIENT:=PVector(params)^;
                   {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightModeliv(pname, params);
                end;
        else {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightModeliv(pname, params);
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
                {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightf(light, pname,param);
              end;
        GL_SPOT_CUTOFF: if SPOT_CUTOFF<>param then begin
                SPOT_CUTOFF:=param;
                {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightf(light, pname,param);
              end;
        GL_CONSTANT_ATTENUATION: if CONSTANT_ATTENUATION<>param then begin
                CONSTANT_ATTENUATION:=param;
                {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightf(light, pname,param);
              end;
        GL_LINEAR_ATTENUATION: if LINEAR_ATTENUATION<>param then begin
                LINEAR_ATTENUATION:=param;
                {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightf(light, pname,param);
              end;
        GL_QUADRATIC_ATTENUATION: if QUADRATIC_ATTENUATION<>param then begin
                QUADRATIC_ATTENUATION:=param;
                {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightf(light, pname,param);
              end;
        Else {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightf(light, pname,param);
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
                {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightf(light, pname,param);
              end;
        GL_SPOT_CUTOFF: if SPOT_CUTOFF<>param then begin
                SPOT_CUTOFF:=param;
                {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightf(light, pname,param);
              end;
        GL_CONSTANT_ATTENUATION: if CONSTANT_ATTENUATION<>param then begin
                CONSTANT_ATTENUATION:=param;
                {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightf(light, pname,param);
              end;
        GL_LINEAR_ATTENUATION: if LINEAR_ATTENUATION<>param then begin
                LINEAR_ATTENUATION:=param;
                {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightf(light, pname,param);
              end;
        GL_QUADRATIC_ATTENUATION: if QUADRATIC_ATTENUATION<>param then begin
                QUADRATIC_ATTENUATION:=param;
                {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightf(light, pname,param);
              end;
        Else {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightf(light, pname,param);
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
                {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightfv(light, pname, params);
              end;
        GL_SPOT_CUTOFF: if SPOT_CUTOFF<>params^ then begin
                SPOT_CUTOFF:=params^;
                {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightfv(light, pname, params);
              end;
        GL_CONSTANT_ATTENUATION: if CONSTANT_ATTENUATION<>params^ then begin
                CONSTANT_ATTENUATION:=params^;
                {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightfv(light, pname, params);
              end;
        GL_LINEAR_ATTENUATION: if LINEAR_ATTENUATION<>params^ then begin
                LINEAR_ATTENUATION:=params^;
                {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightfv(light, pname, params);
              end;
        GL_QUADRATIC_ATTENUATION: if QUADRATIC_ATTENUATION<>params^ then begin
                QUADRATIC_ATTENUATION:=params^;
                {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightfv(light, pname, params);
              end;
        GL_AMBIENT: if not VectorEquals(AMBIENT,PVector(params)^) then begin
                AMBIENT:=PVector(params)^;
                {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightfv(light, pname, params);
              end;
        GL_DIFFUSE: if not VectorEquals(DIFFUSE,PVector(params)^) then begin
                DIFFUSE:=PVector(params)^;
                {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightfv(light, pname, params);
              end;
        GL_SPECULAR: if not VectorEquals(SPECULAR,PVector(params)^) then begin
                SPECULAR:=PVector(params)^;
                {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightfv(light, pname, params);
              end;
        GL_POSITION: {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightfv(light, pname, params);
(*              if not VectorEquals(POSITION,PVector(params)^) then begin
                POSITION:=PVector(params)^; //* modelViewMatrix
                {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightfv(light, pname, params);
              end; *)
        GL_SPOT_DIRECTION: {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightfv(light, pname, params);
(*              if not VectorEquals(SPOT_DIRECTION,PVector(params)^) then begin
                SPOT_DIRECTION:=PVector(params)^;
                {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightfv(light, pname, params);
              end;*)
        Else {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightfv(light, pname,params);
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
                {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightiv(light, pname, params);
              end;
        GL_SPOT_CUTOFF: if SPOT_CUTOFF<>params^ then begin
                SPOT_CUTOFF:=params^;
                {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightiv(light, pname, params);
              end;
        GL_CONSTANT_ATTENUATION: if CONSTANT_ATTENUATION<>params^ then begin
                CONSTANT_ATTENUATION:=params^;
                {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightiv(light, pname, params);
              end;
        GL_LINEAR_ATTENUATION: if LINEAR_ATTENUATION<>params^ then begin
                LINEAR_ATTENUATION:=params^;
                {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightiv(light, pname, params);
              end;
        GL_QUADRATIC_ATTENUATION: if QUADRATIC_ATTENUATION<>params^ then begin
                QUADRATIC_ATTENUATION:=params^;
                {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightiv(light, pname, params);
              end;
        GL_AMBIENT: if not VectorEquals(AMBIENT,PVector(params)^) then begin
                AMBIENT:=PVector(params)^;
                {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightiv(light, pname, params);
              end;
        GL_DIFFUSE: if not VectorEquals(DIFFUSE,PVector(params)^) then begin
                DIFFUSE:=PVector(params)^;
                {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightiv(light, pname, params);
              end;
        GL_SPECULAR: if not VectorEquals(SPECULAR,PVector(params)^) then begin
                SPECULAR:=PVector(params)^;
                {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightiv(light, pname, params);
              end;
        GL_POSITION: {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightiv(light, pname, params);
(*              if not VectorEquals(POSITION,PVector(params)^) then begin
                POSITION:=PVector(params)^;
                {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightiv(light, pname, params);
              end; *)
        GL_SPOT_DIRECTION: {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightiv(light, pname, params);
(*              if not VectorEquals(SPOT_DIRECTION,PVector(params)^) then begin
                SPOT_DIRECTION:=PVector(params)^;
                {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightiv(light, pname, params);
              end;*)
        Else {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightiv(light, pname,params);
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
        GL_POSITION: {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightfv(light, pname, params);
        //PVector(p)^:=POSITION;
        GL_SPOT_DIRECTION: {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightfv(light, pname, params);
        // PVector(p)^:=SPOT_DIRECTION;
        Else {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightfv(light, pname, params);
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
        GL_POSITION: {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightiv(light, pname, params);
//        PIntegerVector(p)^:=FloatToIntVector(POSITION);
        GL_SPOT_DIRECTION: {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightiv(light, pname, params);
//        PIntegerVector(p)^:=FloatToIntVector(SPOT_DIRECTION);
        Else {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLightiv(light, pname, params);
     end;
  end;
end;


procedure glBindTexture(target: TGLEnum; texture: TGLuint);
begin
//{$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glBindTexture(target,Texture); exit;
  with GLStateCache.TextureCache do begin
       if Units[CurrentUnit].ID<>texture then begin
          {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glBindTexture(target,Texture);
          Units[CurrentUnit].Target:=target;
          Units[CurrentUnit].ID:=Texture;
       end;
  end;
end;

procedure glActiveTexture(target: TGLenum);
begin
//  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glActiveTexture(target); exit;
  if target<>GLStateCache.TextureCache.CurrentUnit+GL_Texture0
  then begin
    {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glActiveTexture(target);
    GLStateCache.TextureCache.CurrentUnit:=target-GL_Texture0;
  end;
end;

procedure glClientActiveTexture(target: TGLenum);
begin
  if target<>GLStateCache.TextureCache.CurrentClientUnit+GL_Texture0
  then begin
    {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glClientActiveTexture(target);
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
  {$IFDEF DEBUG_STATES}
    //vStateLog.Add('===========Restore States===========');
        AlphaCache.SnapState(vStateLog);
        DepthCache.SnapState(vStateLog);
        BlendingCache.SnapState(vStateLog);
        MaterialCache.SnapState(vStateLog);
        LightingCache.SnapState(vStateLog);
  {$ENDIF}

end;

procedure TGLStateCache.PushStates;
var Temp: TGLStateCache;
begin
  {$IFDEF DEBUG_STATES}
    //vStateLog.Add('===========Pushing States===========');
        AlphaCache.SnapState(vStateLog);
        DepthCache.SnapState(vStateLog);
        BlendingCache.SnapState(vStateLog);
        MaterialCache.SnapState(vStateLog);
        LightingCache.SnapState(vStateLog);
  {$ENDIF}
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
    //if CheckGLStates then CheckStates;
    {$IFDEF DEBUG_STATES}CheckStates{$ENDIF};

    DepthCache.Reset;
    AlphaCache.Reset;
    BlendingCache.Reset;
    MaterialCache.Reset;
    LightingCache.Reset;
    TextureCache.Reset;
    ShaderCache.Reset;
end;

procedure TGLLightingCache.SnapState(const Log: TStringList);
var LId,i,c: integer;
    b: boolean;
    ls: string;
    f: single; fv: TVector;
const bool: array[false..true] of string = ('false', 'true');
begin
  b:={$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glisEnabled(GL_LIGHTING);
  if b<>(sLighting in GLStateCache.States) then
    log.Add('GL_LIGHTING'+#9+#9+bool[b]+#9+bool[sLighting in GLStateCache.States]);

  for i:=0 to 0 do //MaxLights-1 do
    with Lights[i] do begin
      LId:=GL_LIGHT0+i; b:={$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glIsEnabled(LId);
      ls:='GL_LIGHT'+inttostr(i)+':';
      if b<>Lights[i].Enabled then
        log.Add(ls+#9+#9+bool[b]+#9+bool[Lights[i].Enabled]);
      {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetLightfv(LId, GL_SPOT_EXPONENT, @f);
      if f<>SPOT_EXPONENT then
        log.Add(#9+'GL_SPOT_EXPONENT'+#9+#9+FloatToStr(f)+#9+FloatToStr(SPOT_EXPONENT));
      {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetLightfv(LId, GL_SPOT_CUTOFF, @f);
      if f<>SPOT_CUTOFF then
        log.Add(#9+'GL_SPOT_CUTOFF'+#9+#9+FloatToStr(f)+#9+FloatToStr(SPOT_CUTOFF));
      {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetLightfv(LId, GL_CONSTANT_ATTENUATION, @f);
      if f<>CONSTANT_ATTENUATION then
        log.Add(#9+'GL_CONSTANT_ATTENUATION'+#9+#9+FloatToStr(f)+#9+FloatToStr(CONSTANT_ATTENUATION));
      {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetLightfv(LId, GL_LINEAR_ATTENUATION, @f);
      if f<>LINEAR_ATTENUATION then
        log.Add(#9+'GL_LINEAR_ATTENUATION'+#9+#9+FloatToStr(f)+#9+FloatToStr(LINEAR_ATTENUATION));
      {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetLightfv(LId, GL_QUADRATIC_ATTENUATION, @f);
      if f<>QUADRATIC_ATTENUATION then
        log.Add(#9+'GL_QUADRATIC_ATTENUATION'+#9+#9+FloatToStr(f)+#9+FloatToStr(QUADRATIC_ATTENUATION));
      {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetLightfv(LId, GL_AMBIENT, @fv);
      if not VectorEquals(fv,AMBIENT) then
        log.Add(#9+'GL_AMBIENT'+#9+#9+VectorToStr(fv)+#9+VectorToStr(AMBIENT));
      {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetLightfv(LId, GL_DIFFUSE, @fv);
      if not VectorEquals(fv, DIFFUSE) then
        log.Add(#9+'GL_DIFFUSE'+#9+#9+VectorToStr(fv)+#9+VectorToStr(DIFFUSE));
      {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetLightfv(LId, GL_SPECULAR, @fv);
      if not VectorEquals(fv,SPECULAR) then
        log.Add(#9+'GL_SPECULAR'+#9+#9+VectorToStr(fv)+#9+VectorToStr(SPECULAR));
      {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetLightfv(LId, GL_POSITION, @fv);
      if not VectorEquals(fv,POSITION) then
        log.Add(#9+'GL_POSITION'+#9+#9+VectorToStr(fv)+#9+VectorToStr(POSITION));
      {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetLightfv(LId, GL_SPOT_DIRECTION, @fv);
      if not VectorEquals(fv,SPOT_DIRECTION) then
        log.Add(#9+'GL_SPOT_DIRECTION'+#9+#9+VectorToStr(fv)+#9+VectorToStr(SPOT_DIRECTION));
    end;

  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetFloatv(GL_LIGHT_MODEL_AMBIENT, @fv);
  if not VectorEquals(fv,LM_AMBIENT) then
    log.Add('GL_LIGHT_MODEL_AMBIENT'+#9+#9+VectorToStr(fv)+#9+VectorToStr(LM_AMBIENT));
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetBooleanv(GL_LIGHT_MODEL_LOCAL_VIEWER, @b);
  if b<>LM_LOCAL_VIEWER then
    log.Add('GL_LIGHT_MODEL_LOCAL_VIEWER'+#9+#9+bool[b]+#9+bool[LM_LOCAL_VIEWER]);
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_LIGHT_MODEL_COLOR_CONTROL,@c);
  if c<>LM_COLOR_CONTROL then
    log.Add('GL_LIGHT_MODEL_COLOR_CONTROL'+#9+#9+inttostr(c)+#9+inttostr(LM_COLOR_CONTROL));
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetBooleanv(GL_LIGHT_MODEL_TWO_SIDE, @b);
  if b<>LM_TWO_SIDE then
    log.Add('GL_LIGHT_MODEL_TWO_SIDE'+#9+#9+bool[b]+#9+bool[LM_TWO_SIDE]);
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
        //POSITION:=Lights[i].POSITION;
        //SPOT_DIRECTION:=Lights[i].SPOT_DIRECTION;
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
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_MAX_TEXTURE_IMAGE_UNITS,@MaxTU);
  setlength(Units,MaxTU);
  if {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glIsEnabled(GL_TEXTURE_1D) then Texture1D:=true else Texture1D:=false;
  if {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glIsEnabled(GL_TEXTURE_2D) then Texture2D:=true else Texture2D:=false;
  if {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glIsEnabled(GL_TEXTURE_3D) then Texture3D:=true else Texture3D:=false;
  if {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glIsEnabled(GL_TEXTURE_CUBE_MAP) then TextureCubeMap:=true else TextureCubeMap:=false;
  if {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glIsEnabled(GL_TEXTURE_RECTANGLE) then TextureRectangle:=true else TextureRectangle:=false;
  glGetIntegerv(GL_ACTIVE_TEXTURE,@CTU);
  CurrentUnit:=CTU-GL_TEXTURE0;
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_CLIENT_ACTIVE_TEXTURE,@CTU);
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
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_MATRIX_MODE, @MatrixMode);
  with Current do begin
    {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetFLoatv(GL_MODELVIEW_MATRIX,PGLFloat(@ModelViewMatrix));
    MVLoaded:=true;
    {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetFLoatv(GL_PROJECTION_MATRIX,PGLFloat(@ProjectionMatrix));
    PMLoaded:=true;
    {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetFLoatv(GL_TEXTURE_MATRIX,PGLFloat(@TextureMatrix));
    TMLoaded:=true;
    {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetFLoatv(GL_COLOR_MATRIX,PGLFloat(@ColorMatrix));
    CMLoaded:=true;
  end;
  FChecked:=true;
end;
(*
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
              {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glPopMatrix; Current.MVLoaded:=true;
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
              {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glPopMatrix; Current.PMLoaded:=true;
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
              {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glPopMatrix; Current.TMLoaded:=true;
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
              {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glPopMatrix; Current.CMLoaded:=true;
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
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLoadMatrixf(m);
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
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glLoadIdentity;
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
//  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glMultMatrixf(m);
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
//  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glScalef(x, y, z);
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
*)

procedure TGLMatrixCache.Reset;
begin
  glMatrixMode(GL_MODELVIEW);
  FChecked:=true;
end;

procedure glUseProgram(_program: TGLuint);
begin
  if _program<>GLStateCache.FActiveProgram then begin
    GLStateCache.FActiveProgram:=_program;
    {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glUseProgram(_program);
  end;
end;

procedure glGetIntegerv(pname: TGLEnum; params: PGLint);
begin
  case pname of
    GL_CURRENT_PROGRAM: params^:=GLStateCache.FActiveProgram;
  else {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(pname,params);
  end;

end;

procedure glBegin(mode: TGLEnum);
{
var Id: cardinal;
begin
 //����� �������� ��� ������������� ������ ���������
  asm
    mov eax,[esp+12]
    mov [id],eax;
  end;
  CurrentFFPVBO:=GLStateCache.GLVBOManager.GetBuffer(Id);
}
begin
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glBegin(mode);
end;

procedure glVertex3f(x, y, z: TGLfloat);
begin
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glVertex3f(x, y, z);
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
      {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glDeleteBuffers(1,@FFPBuff.VBOId);
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
  {$IFNDEF DIRECTGL}OpenGl1x.{$ELSE}dglOpenGL.{$ENDIF}glGetIntegerv(GL_CURRENT_PROGRAM,@GLStateCache.FActiveProgram);
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
  {$IFDEF DEBUG_STATES}
  vStateLog:=TStringList.Create;
  {$ENDIF}
finalization
  //if StateStack.Count>0 then GLStateCache.PopStates;
  FreeStack(StateStack);
  GLStateCache.Free;
{$IFDEF DEBUG_STATES}
  vStateLog.SaveToFile('DebugStates.txt');
  vStateLog.Free;
{$ENDIF}
end.
