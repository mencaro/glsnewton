// Запланировано:
// 1. Создание текстуры произвольного формата
// 2. Загрузка текстуры из битмапа, из материала
//
unit uTextures;

interface

Uses Windows, Classes, SysUtils, OpenGL1x, GLTextureFormat, Textures,
     VectorGeometry, Contnrs
     {GLGraphics,GLFileDDS, GLFileJpeg, GLFilePNG};

Type
  TSettedTexParams = set of (stFormats, stFilters, stData,
                             stSizes, stWraps, stTarget);
  TTexTarget = (ttTexture1D, ttTexture2D, ttTexture3D, ttTextureRectangle,
                 ttCubemap, ttCubemapPX, ttCubemapPY, ttCubemapNX, ttCubemapNY,
                 ttCubemapPZ, ttCubemapNZ, tt1DArray, tt2DArray, ttCubeMapArray);
  TTextureWraps = (twClamp, twRepeat, twClampToEdge, twClampToBorder);
  TMagFilter = (mgNearest, mgLinear);
  TMinFilter = (mnNearest, mnLinear, mnNearestMipmapNearest, mnNearestMipmapLinear,
                mnLinearMipmapNearest, mnLinearMipmapLinear);
{  TTextureEnvMode = (tmAdd = GL_ADD, tmModulate = GL_MODULATE,
              tmDecal = GL_DECAL, tmBlend = GL_BLEND,
              tmReplace = GL_REPLACE, tmCombine = GL_COMBINE);}
//  TCompositeTexFormat = ();
  TTextureDecription = record
     InternalFormat: GLUInt;
     Precision: GLUInt;
     ColorChanels: GLUint;
     WrapS, WrapT: GLUInt;
     Target: GLEnum;
     minFilter: GLEnum;
     magFilter: GLEnum;
     GenerateMipMaps: boolean;
     Data: pointer;
     Id, pboRBId,pboWBId: GLUint;
     FullSize, PixelSize: integer;
     Width, Height, Depth: integer;
     UsePBO: boolean;
     Created: boolean;
  end;

  TGLTexture = class;

  TGLTextureLibrary = class (TObjectList)
     private
       function Get(Index: Integer): TGLTexture;
       procedure Put(Index: Integer; Item: TGLTexture);
     public
       property Items[Index: Integer]: TGLTexture read Get write Put; default;
       destructor Destroy;override;
  end;

  TTextureCombines = (tcDecal, tcModulate, tcBlend, tcReplace, tcAdd);
  
  TGLTexture = class(TObject)
     private
       FTexture: TTextureDecription;
       FSettedParams: TSettedTexParams;
       FTextureMode: TTextureCombines;
       FName, FLocation: string;
       FTarget: TTexTarget;
       FOwner: TObject;
       procedure UploadTexture;
       function GetReadPBO: GLUint;
       function GetWritePBO: GLUint;
       procedure SetTextureMode;
     public
       constructor Create;
       constructor CreateFromFile(Filename: string; target: TTexTarget);
       destructor Destroy;override;

       property Created: boolean read FTexture.Created;
       property Owner: TObject read FOwner write FOwner;
       property TextureMode: TTextureCombines read FTextureMode write FTextureMode;

       function  CreateTexture: boolean;
       procedure ImportTextureParams(Target: GLEnum; TexId: GLUInt);
       procedure Assign(Texture: TGLTexture);
       procedure Apply(TextureUnit: GLEnum = 0; ApplyCombiner: boolean=true);
       procedure UnApply(TextureUnit: GLEnum = 0);
       procedure SetPixel(x,y: integer; color: TVector);
       function  ReadPixel(x,y: integer): TVector;

       function  CreateRGBA8Texture2D(Width, Height: integer; Data: pointer=nil; UsePBO: Boolean=false): boolean;
       function  CreateBGRA8Texture2D(Width, Height: integer; Data: pointer=nil; UsePBO: Boolean=false): boolean;
       function  CreateRGB8Texture2D(Width, Height: integer; Data: pointer=nil; UsePBO: Boolean=false): boolean;
       function  CreateRGB16FTexture2D(Width, Height: integer; Data: pointer=nil; UsePBO: Boolean=false): boolean;
       function  CreateRGBA16FTexture2D(Width, Height: integer; Data: pointer=nil; UsePBO: Boolean=false): boolean;
       function  CreateRGB32FTexture2D(Width, Height: integer; Data: pointer=nil; UsePBO: Boolean=false): boolean;
       function  CreateRGBA32FTexture2D(Width, Height: integer; Data: pointer=nil; UsePBO: Boolean=false): boolean;
       function  CreateDepth16FTexture2D(Width, Height: integer; Data: pointer=nil; UsePBO: Boolean=false): boolean;
       function  CreateDepth24FTexture2D(Width, Height: integer; Data: pointer=nil; UsePBO: Boolean=false): boolean;
       function  CreateDepth32FTexture2D(Width, Height: integer; Data: pointer=nil; UsePBO: Boolean=false): boolean;
       function  CreateStencil1Texture2D(Width, Height: integer; Data: pointer=nil; UsePBO: Boolean=false): boolean;
       function  CreateStencil4Texture2D(Width, Height: integer; Data: pointer=nil; UsePBO: Boolean=false): boolean;
       function  CreateStencil8Texture2D(Width, Height: integer; Data: pointer=nil; UsePBO: Boolean=false): boolean;
       function  CreateStencil16Texture2D(Width, Height: integer; Data: pointer=nil; UsePBO: Boolean=false): boolean;

       function  CreateBGRA8TextureRECT(Width, Height: integer; Data: pointer=nil; UsePBO: Boolean=false): boolean;
       function  CreateBGR8TextureRECT(Width, Height: integer; Data: pointer=nil; UsePBO: Boolean=false): boolean;
       function  CreateRGB8TextureRECT(Width, Height: integer; Data: pointer=nil; UsePBO: Boolean=false): boolean;
       function  CreateRGBA8TextureRECT(Width, Height: integer; Data: pointer=nil; UsePBO: Boolean=false): boolean;

       procedure UploadData(Data: pointer; UsePBO: boolean=false);
       function  DownloadData(var Datas: pointer; UsePBO: boolean=false): boolean;
       function  ReadFromPBO(pboId: GLUInt): boolean;
       function  WriteToPBO(pboId: GLUInt): boolean;
       function  LoadDataFromFile(Filename: String; var PixelFormat: GLEnum; var Width, Height: integer): pointer;

       procedure FreePBO;


       function SetInternalFormat(texFormat: TGLInternalFormat): boolean;
       function GetInternalFormat: TGLinternalFormat;
       function SetOGLTextureFormat(InternalFormat, PixelFormat, Precission: GLEnum): TGLinternalFormat;

       function SetDimensions(width: integer; height: integer = 1; depth: integer=1): boolean;
       function SetWraps(WrapS, WrapT: TTextureWraps): boolean;
       function SetTarget(Target: TTexTarget): boolean;
       function SetFilters(MinFilter: TMinFilter; MagFilter: TMagFilter): boolean;

       property Name: string read FName write FName;
       property FileName: string read FLocation write FLocation;

       property Target: TTexTarget read FTarget;
       property Width: integer read FTexture.Width;
       property Height: integer read FTexture.Height;
       property Depth: integer read FTexture.Depth;
       property MemSize: integer read FTexture.FullSize;
       property PixelSize: integer read FTexture.PixelSize;
       property InternalOGLFormat: GLEnum read FTexture.InternalFormat;
       property PrecisionOGLFormat: GLEnum read FTexture.Precision;
       property PixelOGLFormat: GLEnum read FTexture.ColorChanels;
       property Mipmapping: boolean read FTexture.GenerateMipMaps write FTexture.GenerateMipMaps; 

       property Handle: GLUint read FTexture.Id;
       property PBOReadBuffer: GLUint read GetReadPBO;
       property PBOWriteBuffer: GLUint read GetWritePBO;
  end;

(*
  TImgClass = class (TGLBaseImage);

  TTextureResource = record
     Owner: TGLTexture;
     img: TGLBaseImage;
     imgclass: TClass;
     Filename: string;
     Data: pointer;
     Status: (stWaiting, stLoading, stTransfering, stCompleat)
  end;
  PTextureResource = ^TTextureResource;
  TUpdateType = (utAll, utOneStage, utOneResource, utOneStageOneRes);
  TTextureManager = class (TObject)
     private
       FQueue: TList;
       FUpdateType: TUpdateType;
       FUsePBO: boolean;
     public
       Procedure LoadTexture(Filename: string; Texture: TGLTexture);overload;
       Procedure LoadTexture(Data: pointer; Texture: TGLTexture);overload;
       Procedure Update;
  end;
*)
Const
  cTexTargets: array[ttTexture1D..ttCubeMapArray] of GLEnum = (
     GL_TEXTURE_1D, GL_TEXTURE_2D, GL_TEXTURE_3D,
     GL_TEXTURE_RECTANGLE,
     GL_TEXTURE_CUBE_MAP,
     GL_TEXTURE_CUBE_MAP_POSITIVE_X,
     GL_TEXTURE_CUBE_MAP_POSITIVE_Y,
     GL_TEXTURE_CUBE_MAP_NEGATIVE_X,
     GL_TEXTURE_CUBE_MAP_NEGATIVE_Y,
     GL_TEXTURE_CUBE_MAP_POSITIVE_Z,
     GL_TEXTURE_CUBE_MAP_NEGATIVE_Z,
     GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D_ARRAY,
     GL_TEXTURE_CUBE_MAP_ARRAY);
   cWpars: array [twClamp..twClampToBorder] of GLEnum = (
     GL_CLAMP, GL_REPEAT, GL_CLAMP_TO_EDGE, GL_CLAMP_TO_BORDER);
   cMinFilters: array [mnNearest..mnLinearMipmapLinear] of GLEnum = (
     GL_NEAREST, GL_LINEAR, GL_NEAREST_MIPMAP_NEAREST, GL_NEAREST_MIPMAP_LINEAR,
     GL_LINEAR_MIPMAP_NEAREST, GL_LINEAR_MIPMAP_LINEAR);

function GetTextureParams(aTarget: GLEnum; TexId: GLUInt): TTextureDecription;

implementation

function GetTextureParams(aTarget: GLEnum; TexId: GLUInt): TTextureDecription;
var temp: integer;
    ifmt:TGLInternalFormat;
begin
  with result do begin
    Target:=aTarget;
    glBindTexture(target,texId);
    glGetTexParameteriv(target, GL_TEXTURE_MAG_FILTER, @minFilter);
    glGetTexParameteriv(target, GL_TEXTURE_MIN_FILTER, @magFilter);
    glGetTexParameteriv(target, GL_TEXTURE_WRAP_S, @WrapS);
    glGetTexParameteriv(target, GL_TEXTURE_WRAP_T, @WrapT);
    glGetTexParameteriv(target, GL_GENERATE_MIPMAP, @Temp);
                            GenerateMipMaps:=boolean(Temp);
    glGetTexLevelParameteriv(target,0,GL_TEXTURE_WIDTH,@width);
    glGetTexLevelParameteriv(target,0,GL_TEXTURE_HEIGHT,@height);
    glGetTexLevelParameteriv(target,0,GL_TEXTURE_DEPTH,@depth);

    glGetTexLevelParameteriv(target, 0, GL_TEXTURE_INTERNAL_FORMAT, @InternalFormat);
      ifmt:=OpenGLFormatToInternalFormat(InternalFormat);
      FindCompatibleDataFormat(ifmt,ColorChanels,Precision);
      PixelSize:=GetTextureElementSize(ifmt);
      FullSize:=Width*Height*Depth*PixelSize;
    glGetTexLevelParameteriv(target,0,GL_TEXTURE_COMPONENTS,@ColorChanels);
  end;
end;

{ TGLTexture }

constructor TGLTexture.Create;
begin
   inherited;
   FOwner:=nil;
   with FTexture do begin
      Created:=False;
      UsePBO:=false;
      Width:=0; Height:=0;
      pboRBId:=0; pboWBId:=0;
      FSettedParams:=[];
      GenerateMipMaps:=false;
      glGenTextures(1,@Id);
   end;
end;

function TGLTexture.CreateTexture: boolean;
var allOk: boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;
  allOk:=(stFormats in FSettedParams) and (stSizes in FSettedParams) and (Width>0);
  assert(allOk,'Not all of Texture Parameters has been setted');
  with FTexture do begin
    if Height<=0 then Height:=1;
    if Depth<=0 then Depth:=1;
    FullSize:=width*height*depth*PixelSize;
    if not (stTarget in FSettedParams) then Target:=GL_TEXTURE_2D;
    glBindTexture(Target, Id);
    if stWraps in FSettedParams then begin
      glTexParameterf(Target, GL_TEXTURE_WRAP_S, WrapS);
      glTexParameterf(Target, GL_TEXTURE_WRAP_T, WrapT);
    end;
    if stFilters in FSettedParams then begin
      glTexParameterf(Target, GL_TEXTURE_MAG_FILTER, magFilter);
      glTexParameterf(Target, GL_TEXTURE_MIN_FILTER, minFilter);
    end;
    if Target = GL_TEXTURE_2D then
       glTexParameteri(Target, GL_GENERATE_MIPMAP, byte(GenerateMipMaps));
    glTexImage2D(Target, 0, InternalFormat, WIDTH, HEIGHT, 0, ColorChanels, Precision, nil);
    if UsePBO then begin
      glGenBuffers(1,@pboRBId);
      glGenBuffers(1,@pboWBId);
      glBindBuffer(GL_PIXEL_UNPACK_BUFFER, pboWBId);
      glBufferData(GL_PIXEL_UNPACK_BUFFER, FullSize, nil, GL_STREAM_READ);
      glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
      glBindBuffer(GL_PIXEL_PACK_BUFFER, pboRBId);
      glBufferData(GL_PIXEL_PACK_BUFFER, FullSize, nil, GL_STREAM_DRAW);
      glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);
    end;
    if stData in FSettedParams then UploadTexture;
    glBindTexture(Target, 0);
    Created:=true;
  end;
end;

destructor TGLTexture.Destroy;
begin
   with FTexture do begin
     FSettedParams:=[];
     glBindTexture(Target,0);
     glDeleteTextures(1,@Id);
     FreePBO;
   end;
  inherited;
end;

procedure TGLTexture.Apply(TextureUnit: GLEnum = 0; ApplyCombiner: boolean=true);
begin
  glActiveTexture(GL_TEXTURE0+TextureUnit);
  glEnable(FTexture.Target);
  glBindTexture(FTexture.Target,FTexture.Id);
  if ApplyCombiner then SetTextureMode;
end;

procedure TGLTexture.UnApply(TextureUnit: GLEnum = 0);
begin
  glActiveTexture(GL_TEXTURE0+TextureUnit);
  glBindTexture(FTexture.Target,0);
  glDisable(FTexture.Target);
end;


function TGLTexture.GetInternalFormat: TGLinternalFormat;
begin
  result:=OpenGLFormatToInternalFormat(FTexture.InternalFormat);
end;


function TGLTexture.SetWraps(WrapS, WrapT: TTextureWraps): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;

  FTexture.WrapS:=cWpars[WrapS];
  FTexture.WrapT:=cWpars[WrapT];
  FSettedParams:=FSettedParams+[stWraps];
end;

function TGLTexture.SetDimensions(width, height: integer; depth: integer): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;
  FTexture.Width:=Width;
  FTexture.Height:=Height;
  FTexture.Depth:=Depth;
  FSettedParams:=FSettedParams+[stSizes];
end;

function TGLTexture.SetInternalFormat(texFormat: TGLInternalFormat): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;

  with FTexture do begin
    InternalFormat:=InternalFormatToOpenGLFormat(texFormat);
    FindCompatibleDataFormat(texFormat,ColorChanels,Precision);
    PixelSize:=GetTextureElementSize(texFormat);
    FSettedParams:=FSettedParams+[stFormats];
  end;
end;

function TGLTexture.SetTarget(Target: TTexTarget): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;

  if IsTargetSupported(cTexTargets[Target]) then
  FTexture.Target:=cTexTargets[Target];
  FSettedParams:=FSettedParams+[stTarget];
  FTarget:=Target;
end;

function TGLTexture.SetFilters(MinFilter: TMinFilter;
  MagFilter: TMagFilter): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;

  if MagFilter = mgNearest then
       FTexture.magFilter:=GL_NEAREST
  else FTexture.magFilter:=GL_LINEAR;
  FTexture.minFilter:=cMinFilters[MinFilter];
  if MinFilter<=mnLinear then FTexture.GenerateMipMaps:=false
  else FTexture.GenerateMipMaps:=true;
  FSettedParams:=FSettedParams+[stFilters];
end;

procedure TGLTexture.UpLoadData(Data: pointer; UsePBO: boolean);
begin
  FTexture.Data:=Data;
  FTexture.UsePBO:=UsePBO;
  if Data=nil then exit;
  if FTexture.Created then begin
     glBindTexture(FTexture.Target,Ftexture.Id);
     UploadTexture;
     glBindTexture(FTexture.Target,0);
  end;
  FSettedParams:=FSettedParams+[stData];
end;

procedure TGLTexture.UploadTexture;
var t:pointer;
begin
if not FTexture.Created then exit;
with FTexture do begin
  if UsePBO then begin
     if pboWBId=0 then begin
       glGenBuffers(1,@pboWBId);
       glBindBuffer(GL_PIXEL_UNPACK_BUFFER, pboWBId);
       glBufferData(GL_PIXEL_UNPACK_BUFFER, FullSize, nil, GL_STREAM_READ);
//       glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
     end;
     glBindBuffer(GL_PIXEL_UNPACK_BUFFER, pboWBId);
     t := glMapBuffer(GL_PIXEL_UNPACK_BUFFER, GL_WRITE_ONLY);
     CopyMemory(t,data,FullSize);
     glUnmapBuffer(GL_PIXEL_UNPACK_BUFFER);
  end;
  case Target of
     GL_TEXTURE_1D:
        if UsePBO then glTexSubImage1D(Target,0,0,Width,ColorChanels,Precision,nil)
        else glTexSubImage1D(Target,0,0,Width,ColorChanels,Precision,Data);
     GL_TEXTURE_2D,GL_TEXTURE_RECTANGLE, GL_TEXTURE_CUBE_MAP..GL_TEXTURE_CUBE_MAP_NEGATIVE_Z:
        if UsePBO then
             glTexSubImage2D(Target,0,0,0,Width,Height,ColorChanels,Precision,nil)
        else glTexSubImage2D(Target,0,0,0,Width,Height,ColorChanels,Precision,Data);
     GL_TEXTURE_3D:
        if UsePBO then glTexSubImage3D(Target,0,0,0,0,Width,Height,Depth,ColorChanels,Precision,nil)
        else glTexSubImage3D(Target,0,0,0,0,Width,Height,Depth,ColorChanels,Precision,Data);
     Else assert(false,'Unsupported uploading target or method');
  end;
  if UsePBO then glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
  if GenerateMipMaps then glGenerateMipmapEXT(Target);
end;
end;

function TGLTexture.DownloadData(var Datas: pointer; UsePBO: boolean): boolean;
var t:pointer;
begin
  if not FTexture.Created then begin result:=false;exit;
  end else result:=true;
  ReallocMem(Datas,FTexture.FullSize);
  FTexture.Data:=datas;
  FTexture.UsePBO:=UsePBO;
  with FTexture do begin
     glEnable(Target); glBindTexture(Target, Id);
     if not UsePBO then glGetTexImage(Target, 0, ColorChanels,Precision, Datas)
     else begin
        if pboRBId=0 then begin
          glGenBuffers(1,@pboRBId);
          glBindBuffer(GL_PIXEL_PACK_BUFFER, pboRBId);
          glBufferData(GL_PIXEL_PACK_BUFFER, FullSize, nil, GL_STREAM_DRAW);
//          glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);
        end;
        glBindBuffer(GL_PIXEL_PACK_BUFFER, pboRBId);
        glGetTexImage(Target,0,ColorChanels,Precision, nil);
//        glGetTexImage(Target,0,GL_BGRA,Precision, nil);
        t := glMapBuffer(GL_PIXEL_PACK_BUFFER, GL_READ_ONLY);
        CopyMemory(Datas,t,FullSize);
        glUnmapBuffer(GL_PIXEL_PACK_BUFFER);
        glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);
     end;
     glBindTexture(Target, 0); glDisable(Target);
  end;
end;

function TGLTexture.CreateRGBA8Texture2D(Width, Height: integer; Data: pointer; UsePBO:Boolean): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;

  SetInternalFormat(tfRGBA8);
  SetDimensions(width, height);
  SetTarget(ttTexture2D);
  SetFilters(mnLinear, mgLinear);
  FTexture.UsePBO:=UsePBO;
  CreateTexture;
  UploadData(Data,UsePBO);
end;

function TGLTexture.CreateRGB8Texture2D(Width, Height: integer;
  Data: pointer; UsePBO: Boolean): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;

  SetInternalFormat(tfRGB8);
  SetDimensions(width, height);
  SetTarget(ttTexture2D);
  SetFilters(mnLinear, mgLinear);
  FTexture.UsePBO:=UsePBO;
  CreateTexture;
  UploadData(Data,UsePBO);

end;

function TGLTexture.CreateRGB16FTexture2D(Width, Height: integer;
  Data: pointer; UsePBO: Boolean): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;

//  SetInternalFormat(tfFLOAT_RGB16);
  SetOGLTextureFormat(GL_RGB16F_ARB,GL_RGB,GL_HALF_FLOAT);
  FTexture.PixelSize:=6;

  SetDimensions(width, height);
  SetWraps(twClampToEdge,twClampToEdge);
  SetTarget(ttTexture2D);
  SetFilters(mnLinear, mgLinear);
  FTexture.UsePBO:=UsePBO;
  CreateTexture;
  UploadData(Data,UsePBO);

end;

function TGLTexture.CreateRGB32FTexture2D(Width, Height: integer;
  Data: pointer; UsePBO: Boolean): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;

//  SetInternalFormat(tfFLOAT_RGB32);
  SetOGLTextureFormat(GL_RGB32F_ARB,GL_RGB,GL_FLOAT);
  FTexture.PixelSize:=12;

  SetDimensions(width, height);
  SetWraps(twClampToEdge,twClampToEdge);
  FTexture.GenerateMipMaps:=false;
  SetFilters(mnNearest, mgNearest);
  SetTarget(ttTexture2D);
  FTexture.UsePBO:=UsePBO;
  CreateTexture;
  UploadData(Data,UsePBO);
end;

function TGLTexture.CreateRGBA16FTexture2D(Width, Height: integer;
  Data: pointer; UsePBO: Boolean): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;

//  SetInternalFormat(tfFLOAT_RGBA16);
  SetOGLTextureFormat(GL_RGBA16F_ARB,GL_RGBA,GL_HALF_FLOAT);
  FTexture.PixelSize:=8;

  SetDimensions(width, height);
  SetWraps(twClampToEdge,twClampToEdge);
  SetTarget(ttTexture2D);
  FTexture.UsePBO:=UsePBO;
  CreateTexture;
  UploadData(Data,UsePBO);

end;

function TGLTexture.CreateRGBA32FTexture2D(Width, Height: integer;
  Data: pointer; UsePBO: Boolean): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;

//  SetInternalFormat(tfFLOAT_RGBA32);
  SetOGLTextureFormat(GL_RGBA32F_ARB,GL_RGBA,GL_FLOAT);
  FTexture.PixelSize:=16;

  SetDimensions(width, height);
  SetWraps(twClampToEdge,twClampToEdge);
  FTexture.GenerateMipMaps:=false;
  SetFilters(mnNearest, mgNearest);
  SetTarget(ttTexture2D);
  FTexture.UsePBO:=UsePBO;
  CreateTexture;
  if data<>nil then UploadData(Data,UsePBO);

end;

function TGLTexture.CreateDepth16FTexture2D(Width, Height: integer;
  Data: pointer; UsePBO: Boolean): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;

  SetOGLTextureFormat(GL_DEPTH_COMPONENT16,GL_DEPTH_COMPONENT,GL_FLOAT);
  FTexture.PixelSize:=4;
  SetDimensions(width, height);
  SetWraps(twClampToEdge,twClampToEdge);
  FTexture.GenerateMipMaps:=false;
  SetFilters(mnNearest, mgNearest);
  SetTarget(ttTexture2D);

  CreateTexture;
  if data<>nil then UploadData(Data,UsePBO);

end;

function TGLTexture.CreateDepth24FTexture2D(Width, Height: integer;
  Data: pointer; UsePBO: Boolean): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;

  SetOGLTextureFormat(GL_DEPTH_COMPONENT24,GL_DEPTH_COMPONENT,GL_FLOAT);
  FTexture.PixelSize:=4;
  SetDimensions(width, height);
  SetWraps(twClampToEdge,twClampToEdge);
  FTexture.GenerateMipMaps:=false;
  SetFilters(mnNearest, mgNearest);
  SetTarget(ttTexture2D);
  FTexture.UsePBO:=UsePBO;
  CreateTexture;
  if data<>nil then UploadData(Data,UsePBO);

end;

function TGLTexture.CreateDepth32FTexture2D(Width, Height: integer;
  Data: pointer; UsePBO: Boolean): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;

  SetOGLTextureFormat(GL_DEPTH_COMPONENT32,GL_DEPTH_COMPONENT,GL_FLOAT);
  FTexture.PixelSize:=4;
  SetDimensions(width, height);
  SetWraps(twClampToEdge,twClampToEdge);
  FTexture.GenerateMipMaps:=false;
  SetFilters(mnNearest, mgNearest);
  SetTarget(ttTexture2D);
  FTexture.UsePBO:=UsePBO;
  CreateTexture;
  if data<>nil then UploadData(Data,UsePBO);

end;

Constructor  TGLTexture.CreateFromFile(Filename: string; target: TTexTarget);
var format: GLEnum;
    w,h: integer;
    data: pointer;
begin
   inherited Create;
   with FTexture do begin
      Created:=False;
      UsePBO:=false;
      Width:=0; Height:=0;
      pboRBId:=0; pboWBId:=0;
      FSettedParams:=[];
      GenerateMipMaps:=false;
      glGenTextures(1,@Id);
   end;
   Data:=LoadDataFromFile(Filename,format, w,h);
   SetFilters(mnLinearMipmapLinear, mgLinear);
    case format of
       GL_RGB: begin
                 if target = ttTexture2D then
                    CreateRGB8Texture2D(w,h,data);
                 if target = ttTextureRectangle then
                    CreateRGB8TextureRECT(w,h,data);
               end;
       GL_RGBA:begin
                 if target = ttTexture2D then
                    CreateRGBA8Texture2D(w,h,data);
                 if target = ttTextureRectangle then
                    CreateRGBA8TextureRECT(w,h,data);
               end;
    end;
end;

function TGLTexture.CreateStencil1Texture2D(Width, Height: integer;
  Data: pointer; UsePBO: Boolean): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;

  SetOGLTextureFormat(GL_STENCIL_INDEX1,GL_STENCIL_INDEX,GL_UNSIGNED_BYTE);
  SetDimensions(width, height);
  SetWraps(twClampToEdge,twClampToEdge);
  FTexture.GenerateMipMaps:=false;
  SetFilters(mnNearest, mgNearest);
  SetTarget(ttTexture2D);

  CreateTexture;
  if data<>nil then UploadData(Data,UsePBO);
end;

function TGLTexture.CreateStencil4Texture2D(Width, Height: integer;
  Data: pointer; UsePBO: Boolean): boolean;
begin

end;

function TGLTexture.CreateStencil8Texture2D(Width, Height: integer;
  Data: pointer; UsePBO: Boolean): boolean;
begin

end;

function TGLTexture.CreateStencil16Texture2D(Width, Height: integer;
  Data: pointer; UsePBO: Boolean): boolean;
begin

end;

procedure TGLTexture.FreePBO;
begin
  with FTexture do begin
     glBindBuffer(GL_PIXEL_UNPACK_BUFFER,0);
     glBindBuffer(GL_PIXEL_PACK_BUFFER,0);
     if pboRBId<>0 then glDeleteBuffers(1,@pboRBId);
     if pboWBId<>0 then glDeleteBuffers(1,@pboWBId);
     pboRBId:=0; pboWBId:=0;
  end;
end;

function TGLTexture.CreateBGRA8Texture2D(Width, Height: integer;
  Data: pointer; UsePBO: Boolean): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;

  SetInternalFormat(tfRGBA8);
  FTexture.ColorChanels:=GL_BGRA;
  SetDimensions(width, height);
  SetTarget(ttTexture2D);
  SetFilters(mnLinear, mgLinear);
  FTexture.UsePBO:=UsePBO;
  CreateTexture;
  UploadData(Data,UsePBO);
end;

function TGLTexture.CreateBGRA8TextureRECT(Width, Height: integer;
  Data: pointer; UsePBO: Boolean): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;
  SetInternalFormat(tfRGBA8);
  FTexture.ColorChanels:=GL_BGRA;
  SetDimensions(width, height);
  SetTarget(ttTextureRectangle);
  SetFilters(mnLinear, mgLinear);
  FTexture.UsePBO:=UsePBO;
  CreateTexture;
  UploadData(Data,UsePBO);
end;

function TGLTexture.CreateRGBA8TextureRECT(Width, Height: integer;
  Data: pointer; UsePBO: Boolean): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;
  SetInternalFormat(tfRGBA8);
  FTexture.ColorChanels:=GL_RGBA;
  SetDimensions(width, height);
  SetTarget(ttTextureRectangle);
  SetFilters(mnLinear, mgLinear);
  FTexture.UsePBO:=UsePBO;
  CreateTexture;
  UploadData(Data,UsePBO);
end;

function TGLTexture.CreateRGB8TextureRECT(Width, Height: integer;
  Data: pointer; UsePBO: Boolean): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;
  SetInternalFormat(tfRGBA8);
  FTexture.ColorChanels:=GL_RGB;
  SetDimensions(width, height);
  SetTarget(ttTextureRectangle);
  SetFilters(mnLinear, mgLinear);
  FTexture.UsePBO:=UsePBO;
  CreateTexture;
  UploadData(Data,UsePBO);
end;

function TGLTexture.CreateBGR8TextureRECT(Width, Height: integer;
  Data: pointer; UsePBO: Boolean): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;
  SetInternalFormat(tfRGBA8);
  FTexture.ColorChanels:=GL_RGB;
  SetDimensions(width, height);
  SetTarget(ttTextureRectangle);
  SetFilters(mnLinear, mgLinear);
  FTexture.UsePBO:=UsePBO;
  CreateTexture;
  UploadData(Data,UsePBO);
end;


function TGLTexture.ReadFromPBO(pboId: GLUInt): boolean;
begin
 if (not FTexture.Created) or (pboId<=0) then begin result:=false;exit;
 end else result:=true;
 with FTexture do begin
  glEnable(Target); glBindTexture(Target, Id);
  glBindBuffer(GL_PIXEL_UNPACK_BUFFER, pboId);
  case Target of
     GL_TEXTURE_1D: glTexSubImage1D(Target,0,0,Width,ColorChanels,Precision,nil);
     GL_TEXTURE_2D,GL_TEXTURE_RECTANGLE, GL_TEXTURE_CUBE_MAP..GL_TEXTURE_CUBE_MAP_NEGATIVE_Z:
           glTexSubImage2D(Target,0,0,0,Width,Height,ColorChanels,Precision,nil);
     GL_TEXTURE_3D: glTexSubImage3D(Target,0,0,0,0,Width,Height,Depth,ColorChanels,Precision,nil);
     Else assert(false,'Unsupported uploading target or method');
  end;
  glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
  if GenerateMipMaps then glGenerateMipmapEXT(Target);
 end;
end;

function TGLTexture.WriteToPBO(pboId: GLUInt): boolean;
begin
  if (not FTexture.Created) or (pboId<=0) then begin result:=false;exit;
  end else result:=true;
  with FTexture do begin
    glEnable(Target); glBindTexture(Target, Id);
    glBindBuffer(GL_PIXEL_PACK_BUFFER, pboId);
    glGetTexImage(Target,0,ColorChanels,Precision, nil);
    glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);
    glBindTexture(Target, 0); glDisable(Target);
  end;
end;

function TGLTexture.ReadPixel(x, y: integer): TVector;
var t:pointer;
    i,offs: integer;
begin
  if not FTexture.Created then exit;
  with FTexture do begin
     glEnable(Target); glBindTexture(Target, Id);
     if pboRBId=0 then begin
          glGenBuffers(1,@pboRBId);
          glBindBuffer(GL_PIXEL_PACK_BUFFER, pboRBId);
          glBufferData(GL_PIXEL_PACK_BUFFER, FullSize, nil, GL_STREAM_DRAW);
     end else glBindBuffer(GL_PIXEL_PACK_BUFFER, pboRBId);
     glGetTexImage(Target,0,ColorChanels,Precision, nil);
     t := glMapBuffer(GL_PIXEL_PACK_BUFFER, GL_READ_ONLY);
     offs:=(y*Width+x)*PixelSize;
     for i:=0 to PixelSize-1 do begin
       case Precision of
         GL_UNSIGNED_BYTE: begin
                result[i]:=PByteArray(t)[offs+i];
              end;
         GL_FLOAT: begin
                result[i]:=PSingleArray(t)[offs+i];
              end;
       end;
     end;
     glUnmapBuffer(GL_PIXEL_PACK_BUFFER);
     glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);
     glBindTexture(Target, 0); glDisable(Target);
  end;
end;

procedure TGLTexture.SetPixel(x, y: integer; color: TVector);
var t:pointer;
    i,offs: integer;
begin
  if (not FTexture.Created) then exit;
  with FTexture do begin
    glEnable(Target); glBindTexture(Target, Id);
    if pboWBId=0 then begin
         glGenBuffers(1,@pboWBId);
         glBindBuffer(GL_PIXEL_UNPACK_BUFFER, pboWBId);
         glBufferData(GL_PIXEL_UNPACK_BUFFER, FullSize, nil, GL_STREAM_READ);
    end else glBindBuffer(GL_PIXEL_UNPACK_BUFFER, pboWBId);

    t := glMapBuffer(GL_PIXEL_UNPACK_BUFFER, GL_WRITE_ONLY);
    offs:=(y*(Width-1)+x)*PixelSize;
    for i:=0 to PixelSize-1 do begin
      case Precision of
         GL_UNSIGNED_BYTE:     PByteArray(t)[offs+i]:=trunc(color[i]);
         GL_FLOAT:             PSingleArray(t)[offs+i]:=color[i];
      end;
    end;
    glUnmapBuffer(GL_PIXEL_UNPACK_BUFFER);
    glTexSubImage2D(Target,0,x,y,1,1,ColorChanels,Precision,pointer(offs));    
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
    glBindTexture(Target, 0); glDisable(Target);   
  end;
end;


procedure TGLTexture.ImportTextureParams(Target: GLEnum; TexId: GLUInt);
var temp: integer;
    ifmt:TGLInternalFormat;
begin
  FTexture.Target:=Target;
  with FTexture do begin
    glBindTexture(target,texId);
    glGetTexParameteriv(target, GL_TEXTURE_MAG_FILTER, @minFilter);
    glGetTexParameteriv(target, GL_TEXTURE_MIN_FILTER, @magFilter);
    glGetTexParameteriv(target, GL_TEXTURE_WRAP_S, @WrapS);
    glGetTexParameteriv(target, GL_TEXTURE_WRAP_T, @WrapT);
    glGetTexParameteriv(target, GL_GENERATE_MIPMAP, @Temp);
                            GenerateMipMaps:=boolean(Temp);
    glGetTexLevelParameteriv(target,0,GL_TEXTURE_WIDTH,@width);
    glGetTexLevelParameteriv(target,0,GL_TEXTURE_HEIGHT,@height);
    glGetTexLevelParameteriv(target,0,GL_TEXTURE_DEPTH,@depth);

    glGetTexLevelParameteriv(target, 0, GL_TEXTURE_INTERNAL_FORMAT, @InternalFormat);
      ifmt:=OpenGLFormatToInternalFormat(InternalFormat);
      FindCompatibleDataFormat(ifmt,ColorChanels,Precision);
      PixelSize:=GetTextureElementSize(ifmt);
      FullSize:=Width*Height*Depth*PixelSize;
    glGetTexLevelParameteriv(target,0,GL_TEXTURE_COMPONENTS,@ColorChanels);
    Data:=nil;
    Id:=TexId;
    FreePBO;UsePBO:=false;
    Created:=true;
  end;
end;

function TGLTexture.LoadDataFromFile(Filename: String; var PixelFormat: GLEnum; var Width, Height: integer): pointer;
begin
  result:=LoadTexture(Filename, PixelFormat, Width, Height);
end;

function TGLTexture.SetOGLTextureFormat(InternalFormat, PixelFormat,
  Precission: GLEnum): TGLinternalFormat;
begin
  FTexture.InternalFormat:=InternalFormat;
  FTexture.ColorChanels:=PixelFormat;
  FTexture.Precision:=Precission;
  FSettedParams:=FSettedParams+[stFormats];
  result:=OpenGLFormatToInternalFormat(InternalFormat);
end;

function TGLTexture.GetReadPBO: GLUint;
begin
with FTexture do begin
  if pboRBId=0 then begin
     glGenBuffers(1,@pboRBId);
     glBindBuffer(GL_PIXEL_PACK_BUFFER, pboRBId);
     glBufferData(GL_PIXEL_PACK_BUFFER, FullSize, nil, GL_STREAM_DRAW);
     glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);
  end;
  result:=pboRBId;
end;
end;

function TGLTexture.GetWritePBO: GLUint;
begin
with FTexture do begin
  if pboWBId=0 then begin
     glGenBuffers(1,@pboWBId);
     glBindBuffer(GL_PIXEL_UNPACK_BUFFER, pboWBId);
     glBufferData(GL_PIXEL_UNPACK_BUFFER, FullSize, nil, GL_STREAM_READ);
     glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
  end;
  result:=pboWBId;
end;
end;


(*
{ TTextureManager }

procedure TTextureManager.LoadTexture(Filename: string; Texture: TGLTexture);
var ext:string;
    res:PTextureResource;
begin
   new(res); res.Filename:=Filename;
   with res^ do begin
     ext:=lowercase(ExtractFileExt(filename)); delete(ext,1,1);
     if ext='bmp' then begin img:=TGLBitmap32.Create; imgclass:=TGLBitmap32;end;
     if (ext='jpg') or (ext='jpeg') then begin
        img:=TGLJPEGImage.Create; imgclass:=TGLJPEGImage;end;
     if ext='png' then begin img:=TGLPNGImage.Create; imgclass:=TGLPNGImage;end;
     if ext='dds' then begin img:=TGLDDSImage.Create; imgclass:=TGLDDSImage;end;
     owner:=Texture; Data:=img.Data;
     Status:=stLoading;
   end; FQueue.Add(res);
end;

procedure TTextureManager.LoadTexture(Data: pointer; Texture: TGLTexture);
var res:PTextureResource;
begin
   new(res); res.Data:=Data;
   with res^ do begin
     Filename:=''; owner:=Texture;
     Status:=stTransfering; img:=nil;
   end; FQueue.Add(res);
end;

procedure TTextureManager.Update;
  procedure PackList;
  var i:integer;
  begin
     i:=0;
     while i<FQueue.Count do begin
        if FQueue[i]=nil then FQueue.Delete(i)
        else i:=i+1;
     end;
  end;
  procedure GetImageParam (res: PTextureResource);
    begin
     with res^ do begin
        Owner.SetDimensions(TImgClass(img).fWidth,TImgClass(img).fheight);
        Owner.SetTarget(ttTexture2D);
        Owner.FLocation:=Filename;
        Owner.SetInternalFormat(TImgClass(img).fInternalFormat);
     end;
  end;
var res: PTextureResource;
    i:integer;
begin
   case FUpdateType of
     utAll: begin
       for i:=0 to FQueue.Count-1 do begin
          res:=FQueue[i];
          case res.Status of
             stLoading: begin
                  res.img.LoadFromFile(res.filename);
                  res.Data:=res.img.Data;
                  res.Owner.UploadData(res.Data,FUsePBO);
                  res.img.Free;
             end;
             stTransfering: begin
                  res.Owner.UploadData(res.Data,FUsePBO);
             end;
          end; dispose(res);
       end; FQueue.Clear;
     end;
     utOneStage: begin
       for i:=0 to FQueue.Count-1 do begin
          res:=FQueue[i];
          case res.Status of
             stLoading: begin
                  res.img.LoadFromFile(res.filename);
                  res.Data:=res.img.Data;
                  res.Owner.UploadData(res.Data,FUsePBO);
                  res.img.Free; res.Status:=stTransfering;
             end;
             stTransfering: begin
                  res.Owner.UploadData(res.Data,FUsePBO);
                  res.Status:=stCompleat;
             end;
          end;
          if res.Status=stCompleat then begin
            dispose(res); FQueue[i]:=nil; end;
       end; PackList;
     end;
     utOneResource: begin
        if FQueue.Count=0 then exit;
        res:=FQueue[0];
        case res.Status of
           stLoading: begin
                res.img.LoadFromFile(res.filename);
                res.Data:=res.img.Data;
                res.Owner.UploadData(res.Data,FUsePBO);
                res.img.Free;
           end;
           stTransfering: begin
                res.Owner.UploadData(res.Data,FUsePBO);
           end;
        end; dispose(res); FQueue.Delete(0);
     end;
     utOneStageOneRes: begin
        if FQueue.Count=0 then exit;
        res:=FQueue[0];
        case res.Status of
           stLoading: begin
                res.img.LoadFromFile(res.filename);
                res.Data:=res.img.Data;
                res.Owner.UploadData(res.Data,FUsePBO);
                res.img.Free; res.Status:=stTransfering;
           end;
           stTransfering: begin
                res.Owner.UploadData(res.Data,FUsePBO);
                res.Status:=stCompleat;
           end;
        end;
        if res.Status=stCompleat then begin
           dispose(res); FQueue.Delete(0);
        end;
     end;
   end;
end;
*)

procedure TGLTexture.Assign(Texture: TGLTexture);
begin
   FTexture:=Texture.FTexture;
   FSettedParams:=Texture.FSettedParams;
   FName:=Texture.FName;
   FLocation:=Texture.FLocation;
   FTarget:=Texture.FTarget;
end;

procedure TGLTexture.SetTextureMode;
const
  cTextureMode: array[tcDecal..tcAdd] of TGLEnum =
    (GL_DECAL, GL_MODULATE, GL_BLEND, GL_REPLACE, GL_ADD);
begin
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, cTextureMode[FTextureMode]);
end;

{ TGLTextureLibrary }

destructor TGLTextureLibrary.Destroy;
var i:integer;
begin
  for i := 0 to Count-1 do
    if Items[i].FOwner=Self then Items[i].Free;
  inherited;
end;

function TGLTextureLibrary.Get(Index: Integer): TGLTexture;
begin
  result := inherited Get(index);
end;

procedure TGLTextureLibrary.Put(Index: Integer; Item: TGLTexture);
begin
  inherited Put(Index, Item);
  if Item.Owner=nil then Item.Owner:=Self;
end;

end.
