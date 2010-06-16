{ TODO : Оттестировать режим спрайта }
unit uTextLayer;

interface

uses OpenGL1x, uVBO, vboMesh, uTextures, uShaders,
     VectorGeometry, VectorLists, Classes, Graphics,
     GLRenderContextInfo;

Type
  TTextureFont = class
   private
     FCharsFWidth: array [0..255] of byte;
     FTexture: GLUInt;
     FWidth, FHeight: integer;
     FCharSizeX, FCharSizeY: integer;
     FBitmap: TBitmap;
     FTextureFile: TGLTexture;
     FCreationType: (fBuilded, fLoaded);
   public
     constructor Create;
     destructor Destroy;override;
     function BuildTexture(FontName: string; FontSize: integer; Style: TFontStyles=[]): GLUInt;
     procedure SaveToFile (Filename:string);
     function LoadFromFile(Filename: string): GLUint;
     property TextureId: GLUint read FTexture;
  end;

  TTextSurface = class (TVBOMeshObject)
    private
      FText: TStringList;
      FTextTexture: GLUint;
      FFBOTex: TGLTexture;
      FspId: GLUInt;
      FShaders: TShaders;
      FTextWidth, FTextHeight: integer;
      FFont: TTextureFont;
      FontAssigned: boolean;
      function TextureFromText(const Text: array of ansistring; w,h: integer):GLUint;
      function FInitFBO(w,h: integer): TGLTexture;
      function FAddQuad: integer;
      function CreateShader: GLUInt;
      procedure FApplyShader(rci:TRenderContextInfo;  mo:TObject);
      procedure FUnApplyShader(rci:TRenderContextInfo;  mo:TObject);
      procedure SetFont(const Value: TTextureFont);
    public
      constructor Create;
      destructor Destroy;override;
      procedure RenderObject(ARCI: TRenderContextInfo;var ViewMatrix: TMatrix);override;

      procedure TextSize(var Width, Height: integer);
      function SetText(Text: string; col:integer=-1; row:integer=-1):GLUint;overload;
      function SetText(Text: TStringList; col:integer=-1; row:integer=-1):GLUint;overload;
      function GetText: string; overload;
      property TextTexture: TGLTexture read FFBOTex;
      property TextColumns: integer read FTextWidth;
      property TextRows: integer read FTextHeight;
      property Font: TTextureFont read FFont write SetFont;
  end;

  TVBOTextRect = class (TVBOMeshObject)
    private
      FTextSurface: TTextSurface;
      FWidth, FHeight: single;
      FText: TStringList;
      FLastUpdate: double;
      FTextStates: set of (tsNeedUpdate,tsCropping,tsUpdateColor);
      FCropX, FCropY, FCropW, FCropH: integer;
      FColor: TVector;
      FEnvMode: TTextureEnvMode;
      FAlpha: single;
      FAlphaTest: single;
      FBlendSF, FBlendDF: GLEnum;
      FViewport: array[0..3] of integer;
      function  GetHUDState: boolean;
      procedure SetHUDState(const Value: boolean);
      procedure CreateRect;
      procedure SetPosition(const Value: TVector);
      procedure SetColor(const Value: TVector);
      procedure SetEnvMode(const Value: TTextureEnvMode);
      procedure SetAlpha(const Value: single);
      function GetFont: TTextureFont;

    public
      constructor Create;
      destructor Destroy; override;
      Procedure RenderObject(ARCI: TRenderContextInfo;var ViewMatrix: TMatrix);override;
      //режим отображения - поверх экрана или как спрайт
      property HUD: boolean read GetHUDState write SetHUDState;
      //Координаты текста
      property Position: TVector read Matrices.WorldMatrix[3] write SetPosition;
      //Выводимый текст
      procedure Text(aText: TStringList); overload;
      procedure Text(aText: string); overload;
      //Устанавливает цвет текста
      property Color: TVector read FColor write SetColor;
      //Задает режим смешивания
      property EnvMode: TTextureEnvMode read FEnvMode write SetEnvMode;
      //Устанавливает порог альфатеста
      property AlphaTest: single read FAlphaTest write FAlphaTest;
      //Задает прозрачность текста
      property Alpha: single read FAlpha write SetAlpha;
      //Функции смешивания
      property BlendSourceFunc: TGLEnum read FBlendSF write FBlendSF;
      property BlendDestFunc: TGLEnum read FBlendDF write FBlendDF;
      //Установка размера спрайта (в юнитах)
      procedure SetSize(Width,Height: single);
      //Вырезание области, смещение и размер задается в символах
      procedure Crop(x,y,w,h: integer);
      //Возвращает текстовую поверхность
      property TextSurface: TTextSurface read FTextSurface;
      //Доступ к текстуре шрифта
      property Font: TTextureFont read GetFont;
  end;

implementation

const cFrameRate = 1/25;

{ TVBOText }

//{$REGION 'TTextSurface'}
  function TTextSurface.FAddQuad: integer;
  var temp: PVBOBuffer;
  begin
    new(Temp);InitVBOBuff(Temp^,GL_QUADS,DrawElements);
    with temp^ do begin
      RenderBuffs:=[uTexCoords, uIndices];
      Vertexes.Add(-1,-1,0);TexCoords.Add(0,0,0);
      Vertexes.Add( 1,-1,0);TexCoords.Add(1,0,0);
      Vertexes.Add( 1, 1,0);TexCoords.Add(1,1,0);
      Vertexes.Add(-1, 1,0);TexCoords.Add(0,1,0);
      Indices.Add(0,1); Indices.Add(2,3);
    end;
    GenVBOBuff(Temp^, False);
    result:=MeshList.Add(temp);
  end;

  function TTextSurface.SetText(Text: string; col:integer=-1; row:integer=-1): GLUint;
  begin
    if FTextTexture<>0 then glDeleteTextures(1,@FTextTexture);
    FText.Clear; FText.Text:=Text;
    if col=-1 then FTextWidth:=length(Text) else FTextWidth:=col;
    if row=-1 then FTextHeight:=1 else FTextHeight:=row;
    if assigned(FFBOTex) then FFBOTex.Free;
    FTextTexture:=TextureFromText([Text], FTextWidth, FTextHeight);
    result:=FTextTexture;
    FFBOTex:=FInitFBO(FTextWidth,FTextHeight);
  end;

  procedure TTextSurface.SetFont(const Value: TTextureFont);
  begin
    if (assigned(FFont)) and (not FontAssigned) then FFont.free;
    FFont := Value;
    FontAssigned:=true;
  end;

function TTextSurface.SetText(Text: TStringList; col:integer=-1; row:integer=-1): GLUint;
  var Strings: array of ansistring;
      i,w:integer;
  begin
    FText.Clear; FText.AddStrings(Text);
    if assigned(FFBOTex) then FFBOTex.Free;
    setlength(strings, Text.Count); w:=0;
    for i:=0 to text.Count-1 do begin
      if w<length(text[i]) then w:=length(text[i]);
      strings[i]:=text[i];
    end;
    if FTextTexture<>0 then glDeleteTextures(1,@FTextTexture);
    if col=-1 then FTextWidth:=w else FTextWidth:=col;
    if row=-1 then FTextHeight:=Text.Count else FTextHeight:=row;
    FTextTexture:=TextureFromText(strings, FTextWidth, FTextHeight);
    result:=FTextTexture;
    FFBOTex:=FInitFBO(FTextWidth,FTextHeight);
  end;

  function TTextSurface.FInitFBO(w,h: integer): TGLTexture;
  var fbo_tex: TGLTexture;
  begin
    fbo_tex:= TGLTexture.Create;
    with fbo_tex do begin
      SetFilters(mnLinearMipmapLinear, mgLinear);
      SetWraps(twClampToEdge,twClampToEdge);
      CreateBGRA8Texture2D(w,h);
    end;
    with FBO do begin
       ResetFBO; ConfigFBO([]);
       AttachTexture(fbo_tex);
       InitFBO(w,h);
       Active:=false;
       DeactivateAfter:=true;
    end;
    result:=fbo_tex;
  end;

  procedure TTextSurface.TextSize(var Width, Height: integer);
  begin
    Width:=FTextWidth;
    Height:=FTextHeight;
  end;


  function TTextSurface.TextureFromText(const Text: array of ansistring; w,h: integer): GLUint;
  var TextTextureId: GLUint;
      data: array of record
              r,g,b,a: byte;
            end;
      i,j,k:integer;
      s: ansistring;
      c,cx,cy:byte;
      vw,mw: integer;
      Chars: array of array of byte;
//      bmp:TBitmap;
//      x,y: integer;
  begin
    mw:=0; setlength(Chars,h,w);
    for i:=0 to h-1 do begin
      if i<=high(Text) then s:=Text[i] else s:=' ';
      vw:=0;
      for j:=0 to w-1 do begin
        if j+1<=length(s) then c:=ord(s[j+1]) else c:=32;
        vw:=vw+FFont.FCharsFWidth[c]; Chars[i,j]:=c;
      end;
      if mw<vw then mw:=vw;
    end;
    setlength(data,mw*h);
    for i:=0 to h-1 do begin
      vw:=0;
      for j:=0 to w-1 do begin
        c:=Chars[i,j]; cy:=c div 16; cx:=c-cy*16;
        for k:=0 to FFont.FCharsFWidth[c]-1 do begin
          with data[i*mw+vw+k] do begin
            r:=k; g:=cy; b:=cx; a:=k*16;
          end;
        end;
        vw:=vw+FFont.FCharsFWidth[Chars[i,j]];
      end;
      for j:=vw to mw-1 do
        with data[i*mw+j] do begin
            r:=0; g:=2; b:=0; a:=0;
        end;
    end;
    FTextWidth:=mw; FTextHeight:=h*FFont.FCharSizeY;
    glEnable(GL_TEXTURE_RECTANGLE);
    glGenTextures(1, @TextTextureId);
    glBindTexture(GL_TEXTURE_RECTANGLE, TextTextureId);
    glTexParameteri(GL_TEXTURE_RECTANGLE,GL_TEXTURE_MAG_FILTER,GL_NEAREST);
    glTexParameteri(GL_TEXTURE_RECTANGLE,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
    glTexImage2D(GL_TEXTURE_RECTANGLE, 0, GL_RGBA8,mw,h,0, GL_RGBA,
                 GL_UNSIGNED_BYTE, @data[0]);
    glDisable(GL_TEXTURE_RECTANGLE);
    result:=TextTextureId;
{    bmp:=TBitmap.Create;
    bmp.Width:=FTextWidth;
    bmp.Height:=FTextHeight;
    for i := 0 to bmp.Height - 1 do
    for j:=0 to mw-1 do begin
       cx:=data[(i div FFont.FCharSizeY)*mw+j].b;
       cy:=data[(i div FFont.FCharSizeY)*mw+j].g;
       k :=data[(i div FFont.FCharSizeY)*mw+j].a;
       x:=cx*FFont.FCharSizeX+k;
       y:=cy*FFont.FCharSizeY+trunc(frac(i/FFont.FCharSizeY)*FFont.FCharSizeY);
       bmp.Canvas.Pixels[j,i]:=Font.FBitmap.Canvas.Pixels[x,y];
    end;
    bmp.SaveToFile('E:\Text.bmp');}

  end;

  function TTextSurface.CreateShader: GLUInt;
  const FS: string =
    '#version 120'+#13#10+
    '#extension GL_ARB_texture_rectangle : enable'+#13#10+
    'uniform sampler2DRect TextT;'+#13#10+
    'uniform sampler2DRect CharTexture;'+#13#10+
    'uniform vec4 CharRect;'+#13#10+
    'uniform float FontHeight;'+#13#10+
    'varying vec2 Texcoord;'+#13#10+

    'void main(void)'+#13#10+
    '{'+#13#10+
    '  vec2 tc = vec2(Texcoord.s,1.0-Texcoord.t)*CharRect.xy;'+#13#10+
    '  vec4 charcode = texture2DRect(TextT,tc);'+#13#10+
    '  vec2 charpos = charcode.bg*255.0*CharRect.zw;'+#13#10+
    '       charpos.x += (charcode.r*256.0);'+#13#10+
//    '       charpos.x += (charcode.a*16.0);'+#13#10+
    '       charpos.y += fract(tc.y)*CharRect.w;'+#13#10+
    '       charpos.y = FontHeight-charpos.y;'+#13#10+
    '  gl_FragColor = texture2DRect(CharTexture,charpos.xy);'+#13#10+
    '  gl_FragColor.a = (gl_FragColor.r+gl_FragColor.g+gl_FragColor.b)/3.0;'+#13#10+
    '}';
        VS: string =
  'varying vec2 Texcoord;'+#13#10+
  'void main(void)'+#13#10+
  '{'+#13#10+
  '  gl_Position = ftransform();'+#13#10+
  '  Texcoord = (gl_MultiTexCoord0.xy);'+#13#10+
  '}';

  var vsId, fsId: integer;
  begin
    FShaders:= TShaders.Create;
    with FShaders do begin
      vsId := AddShaderObject(VS,GL_VERTEX_SHADER);
      fsId := AddShaderObject(FS,GL_FRAGMENT_SHADER);
      vsId := ShaderObjectsList[vsId];
      fsId := ShaderObjectsList[fsId];
      FspId:=CreateShaderProgram;
      AttachShaderObjectToProgram(vsId,FspId);
      AttachShaderObjectToProgram(fsId,FspId);
      LinkShaderProgram(FspId);
    end;
    result:=FspId;
  end;

procedure TTextSurface.FApplyShader(rci: TRenderContextInfo; mo: TObject);
  var v:TVector;
  begin
    if (not FBO.Active) or (not assigned(FShaders)) then exit;
     glActiveTexture(GL_TEXTURE0);

     glEnable(GL_TEXTURE_RECTANGLE);
     glBindTexture(GL_TEXTURE_RECTANGLE,FFont.FTexture);

     glActiveTexture(GL_TEXTURE1);
     glEnable(GL_TEXTURE_RECTANGLE);
     glBindTexture(GL_TEXTURE_RECTANGLE, FTextTexture);
     With FShaders do begin
       UseProgramObject(fspid);
       SetUniforms(fspid,'CharTexture',0);
       SetUniforms(fspid,'TextT',1);
       SetUniforms(fspid,'FontHeight',FFont.FHeight*1.0);
       v[0]:=FTextWidth;v[1]:=FTextHeight div FFont.FCharSizeY;
       v[2]:=FFont.FCharSizeX;v[3]:=FFont.FCharSizeY;
       SetUniforms(fspid,'CharRect',v);
     end;
  end;

  procedure TTextSurface.FUnApplyShader(rci: TRenderContextInfo; mo: TObject);
  begin
     if (not FBO.Active) or (not assigned(FShaders)) then exit;
     glActiveTexture(GL_TEXTURE1);
     glBindTexture(GL_TEXTURE_RECTANGLE, 0);
     glDisable(GL_TEXTURE_RECTANGLE);
     glActiveTexture(GL_TEXTURE0);
     glBindTexture(GL_TEXTURE_RECTANGLE,0);
     glDisable(GL_TEXTURE_RECTANGLE);
     FShaders.UseProgramObject(0);
  end;


constructor TTextSurface.Create;
  begin
    inherited;
    FAddQuad; FMeshType:=mtScreenQuad;
    FText:=TStringList.Create;
    FFont:=TTextureFont.Create;
    FontAssigned:=false;
    FTextTexture:=0;
    FspId:=CreateShader;
    FTextWidth:=0; FTextHeight:=0;
    Pickable:=false;
    Visible:=true;
  end;

  procedure TTextSurface.RenderObject(ARCI: TRenderContextInfo;
    var ViewMatrix: TMatrix);
  begin
    glPushAttrib(GL_ALL_ATTRIB_BITS);
    if not FBO.Active then exit;
    arci.GLStates.ResetAll;
    arci.ignoreDepthRequests:=true;
    FBO.Apply;
      glPushMatrix;
        glMatrixMode(GL_MODELVIEW);
        glLoadIdentity;
        glMatrixMode(GL_PROJECTION);
        glPushMatrix;
         glLoadIdentity;
         glDisable(GL_DEPTH_TEST);
         glDisable(GL_LIGHTING);
      FApplyShader(arci,self);
        RenderVBOList(MeshList);
      FUnApplyShader(arci,self);
        glPopMatrix;
        glMatrixMode(GL_MODELVIEW);
        glEnable(GL_DEPTH_TEST);
        glEnable(GL_LIGHTING);
      glPopMatrix;
    FBO.UnApply;
    if FBO.DeactivateAfter then FBO.Active:=false;
    glPopAttrib;
  end;

  destructor TTextSurface.Destroy;
  begin
    glDeleteTextures(1,@FTextTexture);
    FFBOTex.Free; FText.Free; FShaders.Free;
    if not FontAssigned then FFont.Free;
    inherited;
  end;

  function TTextSurface.GetText: string;
  begin
    result:=FText.Text;
  end;

//{$ENDREGION}

{ TVBOTextRect }

//{$REGION 'TextRect'}

constructor TVBOTextRect.Create;
  begin
     inherited;
     CreateRect;
     FTextSurface:=TTextSurface.Create;
     FText:=TStringList.Create;
     FMeshType:=mtHUDSprite;
     FLastUpdate:=-1;
     FTextStates:=[];
     FColor:=VectorMake(1,1,1,1);
     FEnvMode:=tmReplace;
     FAlpha:=1; FAlphaTest:=1;
     FBlendSF:=GL_ONE;
     FBlendDF:=GL_ZERO;
  end;

  destructor TVBOTextRect.Destroy;
  begin
    FText.Free; FTextSurface.Free;
    inherited;
  end;

  procedure TVBOTextRect.CreateRect;
  var temp: PVBOBuffer;
      stc: TAffineVectorList;
  begin
      new(Temp);InitVBOBuff(Temp^,GL_QUADS,DrawElements);
      stc:=TAffineVectorList.Create;
      with temp^ do begin
        RenderBuffs:=[uTexCoords, uIndices, uMultitexture, uColors];
        Vertexes.Add( 0, 0,0);TexCoords.Add(0,0,0); stc.Add(0,0,0); Colors.Add(1,1,1,FAlpha);
        Vertexes.Add( 1, 0,0);TexCoords.Add(1,0,0); stc.Add(1,0,0); Colors.Add(1,1,1,FAlpha);
        Vertexes.Add( 1, 1,0);TexCoords.Add(1,1,0); stc.Add(1,1,0); Colors.Add(1,1,1,FAlpha);
        Vertexes.Add( 0, 1,0);TexCoords.Add(0,1,0); stc.Add(0,1,0); Colors.Add(1,1,1,FAlpha);
        Indices.Add(0,1); Indices.Add(2,3);
        ExTexCoords.Add(stc);
      end;
      GenVBOBuff(Temp^, False);
      MeshList.Add(temp);
  end;

  procedure TVBOTextRect.Crop(x, y, w, h: integer);
  begin
     FTextStates:=FTextStates+[tsCropping];
     FCropX:=x; FCropY:=-y; FCropW:=w; FCropH:=h;
  end;

  function TVBOTextRect.GetFont: TTextureFont;
  begin
     result:=FTextSurface.FFont;
  end;

function TVBOTextRect.GetHUDState: boolean;
  begin
     result:=FMeshType=mtHUDSprite;
  end;

  procedure TVBOTextRect.RenderObject(ARCI: TRenderContextInfo;
    var ViewMatrix: TMatrix);
  var m: TMatrix;
      newColor: array[0..3] of TVector;
      newCoord: array[0..3] of TAffineVector;
      i,w,h:integer;
      buff: PVBOBuffer;
      cw,ch: single;
  begin
    glGetIntegerv(GL_VIEWPORT,@FViewport);
    if  (tsNeedUpdate in FTextStates) and (FTime-FLastUpdate>=cFrameRate) then
    begin
      FTextSurface.SetText(FText); FLastUpdate:=FTime;
      FTextSurface.FBO.Active:=true;
      FTextSurface.RenderObject(arci,ViewMatrix);
      FTextStates:=FTextStates-[tsNeedUpdate];
      FTextSurface.TextSize(w,h);
      SetSize(w,h);
    end;
    if tsCropping in FTextStates then begin
      FTextStates:=FTextStates-[tsCropping];
      cw:=1/FTextSurface.TextColumns;
      ch:=1/FTextSurface.TextRows;
      newCoord[0]:=AffineVectormake(cw*FCropX, ch*FCropY,0);
      newCoord[1]:=AffineVectormake(cw*(FCropX+FCropW), ch*FCropY,0);
      newCoord[2]:=AffineVectormake(cw*(FCropX+FCropW), ch*(FCropY+FCropH),0);
      newCoord[3]:=AffineVectormake(cw*FCropX, ch*(FCropY+FCropH),0);
      buff:=MeshList[0];
      UpdateVBOBuff(Buff.tId,@newCoord[0],0,sizeof(newCoord),false);
    end;
    if tsUpdateColor in FTextStates then begin
      FTextStates:=FTextStates-[tsUpdateColor];
      for i:= 0 to 3 do newColor[i]:=FColor;
      buff:=MeshList[0];
      UpdateVBOBuff(Buff.cId,@newColor[0],0,sizeof(newColor),false);
    end;
    if not assigned(FTextSurface.TextTexture) then exit;
    glPushAttrib(GL_ALL_ATTRIB_BITS);
    arci.GLStates.ResetAll;
    arci.ignoreDepthRequests:=true;
    if FBO.Active then FBO.Apply;
        glPushMatrix;
        case FMeshType of
          mtHudSprite: begin
            glMatrixMode(GL_MODELVIEW); glPushMatrix;
            m:=Matrices.WorldMatrix; m[3][2]:=0;
            glLoadMatrixf(PGLFloat(@m));
            glMatrixMode(GL_PROJECTION); glPushMatrix;
            glLoadIdentity;
            glOrtho(0,FViewport[2],0,FViewport[3],0,100);
            glDepthMask(false); glDisable(GL_LIGHTING);
            glDisable(GL_DEPTH_TEST);
          end;
          mtSphericalSprite: begin
            m:=CreateBBMatrix(ViewMatrix,Matrices,FRollAngle,stSpherical);
            glLoadMatrixf(PGLFloat(@m));
          end;
        end;

        //Apply Text Texture
        glActiveTexture(GL_TEXTURE0);
        glEnable(GL_TEXTURE_2D);
        glBindTexture(GL_TEXTURE_2D,FTextSurface.TextTexture.Handle);

        glEnable(GL_COLOR_MATERIAL);
        glEnable(GL_ALPHA_TEST);
        glAlphaFunc(GL_GREATER,FAlphaTest);
        if FAlpha<1 then begin
           glEnable(GL_BLEND);
           glBlendFunc(FBlendSF,FBlendDF);
        end;

          if assigned(onBeforeRender) then onBeforeRender(arci,self);
             RenderVBOList(MeshList);
          if assigned(onAfterRender) then onAfterRender(arci,self);

        if FAlpha<1 then glDisable(GL_BLEND);
        glDisable(GL_ALPHA_TEST);
        glDisable(GL_COLOR_MATERIAL);

        //UnApply Text Texture
        //glActiveTexture(GL_TEXTURE0);
        glBindTexture(GL_TEXTURE_2D,0);
        glDisable(GL_TEXTURE_2D);

        if FMeshType=mtHudSprite then begin
          glDepthMask(True); glEnable(GL_DEPTH_TEST); glEnable(GL_LIGHTING);
          glPopMatrix; glMatrixMode(GL_MODELVIEW);glPopMatrix;
        end;
        glPopMatrix;
    if FBO.Active then FBO.UnApply;
    glPopAttrib;
  end;

  procedure TVBOTextRect.SetAlpha(const Value: single);
  begin
    FAlpha := Value;
    FColor[3]:=FAlpha; SetColor(FColor);
  end;

  procedure TVBOTextRect.SetColor(const Value: TVector);
  begin
    FColor := Value;
    FTextStates:=FTextStates+[tsUpdateColor];
    if FColor[3]<>1 then FAlpha:=FColor[3];
  end;

  procedure TVBOTextRect.SetEnvMode(const Value: TTextureEnvMode);
  var Buff: PVBOBuffer;
  begin
    FEnvMode := Value;
    Buff:=MeshList[0];
    with Buff^ do begin
      if ExTexEnvMode.Count=0 then ExTexEnvMode.Add(ord(FEnvMode))
      else ExTexEnvMode[0]:=ord(FEnvMode);
    end;
  end;


procedure TVBOTextRect.SetHUDState(const Value: boolean);
  begin
    if Value=True then FMeshType:=mtHUDSprite else FMeshType:=mtSphericalSprite;
  end;

  procedure TVBOTextRect.SetPosition(const Value: TVector);
  var v: TVector;
  begin
    v:=VectorAdd(Value,vectormake(FWidth,FHeight,0)); v[3]:=1;
    MoveObject(v);
  end;

  procedure TVBOTextRect.SetSize(Width, Height: single);
  begin
    FWidth:=Width; FHeight:=Height;
    ScaleObject(FWidth,FHeight,1);
  end;

  procedure TVBOTextRect.Text(aText: TStringList);
  begin
    FText.Clear;
    FText.AddStrings(aText);
    FTextStates:=FTextStates+[tsNeedUpdate];
  end;

  procedure TVBOTextRect.Text(aText: string);
  begin
    if FText.Text<>aText then begin
       FText.Clear; FText.Add(aText);
       FTextStates:=FTextStates+[tsNeedUpdate];
    end;
  end;

//{$ENDREGION}

{ TTextureFont }

function TTextureFont.BuildTexture(FontName: string; FontSize: integer; Style: TFontStyles): GLUInt;
var i,j,mw,mh,cw,ch,w,h,offs: integer;
    data,p: PByteArray;
begin
  mw:=0;mh:=0;
  with FBitmap do begin
    PixelFormat:=pf24bit;
    with canvas do begin
      Font.Size:=FontSize;
      if FontName<>'' then Font.Name:=FontName;
      Font.Style:=Style;
      for i:=0 to 255 do begin
        if (i<128) or (i>=176) then begin
          ch:=TextHeight(string(ansichar(i)));
          if ch>mh then mh:=ch;
          cw:=TextWidth(string(ansichar(i)));
          if fsItalic in Style then inc(cw,3);
          if cw>mw then mw:=cw;
          FCharsFWidth[i]:=cw+1;
        end;
      end;
      inc(mw,2); inc(mh,2);
      if fsItalic in Style then dec(mw,1);
      Width:=mw*16; Height:=mh*16;
      Font.Color:=clWhite;
      Pen.Color:=clBlack;
      Brush.Color:=clBlack;
      Rectangle(Canvas.ClipRect);
    end;
    w:=FBitmap.Width;h:=FBitmap.Height;
    getmem(data,w*h*4);
    //if fontsize>=32 then offs:=0 else
    offs:=1;
    if fsItalic in Style then inc(offs);
    for i:=0 to 15 do for j:=0 to 15 do
      Canvas.TextOut(j*mw+offs,i*mh+offs,string(ansichar(i*16+j)));
    for i:=0 to h-1 do begin
      p:=ScanLine[h-1-i];
      for j:=0 to w-1 do begin
        data[i*w*4+j*4+0]:=p[j*3+2];
        data[i*w*4+j*4+1]:=p[j*3+1];
        data[i*w*4+j*4+2]:=p[j*3+0];
        if p[j*3+2]*p[j*3+1]*p[j*3+2]=0 then
           data[i*w*4+j*4+3]:=0
        else data[i*w*4+j*4+3]:=255;
      end;
    end;
  end;
  glGenTextures(1, @FTexture);
  glEnable(GL_TEXTURE_RECTANGLE);
  glBindTexture(GL_TEXTURE_RECTANGLE, FTexture);
  glTexParameteri(GL_TEXTURE_RECTANGLE,GL_TEXTURE_MAG_FILTER,GL_NEAREST);
  glTexParameteri(GL_TEXTURE_RECTANGLE,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
  glTexImage2D(GL_TEXTURE_RECTANGLE, 0, GL_RGBA8, w,h, 0, GL_RGBA,
               GL_UNSIGNED_BYTE, @data[0]);
  glBindTexture(GL_TEXTURE_RECTANGLE, 0);
  glDisable(GL_TEXTURE_RECTANGLE);
  freemem(data,w*h*4);
  FCharSizeX:=mw;//W div 16;
  FCharSizeY:=mh;//H div 16;
  FWidth:=W; FHeight:=H;
  result:=FTexture;
  FCreationType:=fBuilded;
end;

constructor TTextureFont.Create;
begin
  FBitmap:=TBitmap.Create;
end;

destructor TTextureFont.Destroy;
begin
  FBitmap.Free;
  if assigned(FTextureFile) then FTextureFile.Free;
  if FTexture<>0 then glDeleteTextures(1,@FTexture);
  inherited;
end;

function TTextureFont.LoadFromFile(Filename: string): GLUint;
var i: integer;
begin
  FTextureFile:=TGLTexture.CreateFromFile(Filename,ttTextureRectangle);
  FTexture:=FTextureFile.Handle;
  FWidth:=FTextureFile.Width;
  FHeight:=FTextureFile.Height;
  FCharSizeX:=FWidth div 16;
  FCharSizeY:=FHeight div 16;
  for i:=0 to 255 do FCharsFWidth[i]:=FCharSizeX;
  result:=FTexture;
  FCreationType:=fLoaded;
end;

procedure TTextureFont.SaveToFile(Filename: string);
begin
  FBitmap.SaveToFile(Filename);
end;

end.
