unit uDDSLoader;

interface

uses Classes, uMiscUtils, SysUtils,
  {$IFNDEF DIRECTGL}OpenGL1x;{$ELSE}dglOpenGL;{$ENDIF}

const
//Pixel formats dwFlags
  //Texture contains alpha data; dwRGBAlphaBitMask contains valid data.
  DDPF_ALPHAPIXELS      = 1;
  //Used in some older DDS files for alpha channel only uncompressed data (dwRGBBitCount contains the alpha channel bitcount;
  //dwABitMask contains valid data)
  DDPF_ALPHA            = 2;
  //Texture contains compressed RGB data; dwFourCC contains valid data.
  DDPF_FOURCC           = 4;
  //Texture contains uncompressed RGB data; dwRGBBitCount and the RGB masks (dwRBitMask, dwRBitMask, dwRBitMask) contain valid data.
  DDPF_RGB              = $40;
  DDPF_RGBA             = DDPF_RGB + DDPF_ALPHA;
  //Used in some older DDS files for YUV uncompressed data (dwRGBBitCount contains the YUV bit count; dwRBitMask contains the Y mask,
  //dwGBitMask contains the U mask, dwBBitMask contains the V mask)
  DDPF_YUV              = $200;
  //Used in some older DDS files for single channel color uncompressed data (dwRGBBitCount contains the luminance channel bit count; dwRBitMask contains the channel mask).
  //Can be combined with DDPF_ALPHAPIXELS for a two channel DDS file.
  DDPF_LUMINANCE        = $20000;

//Pixel formats dwFourCC
  FOURCC_UNKNOWN       = 0;
  FOURCC_R8G8B8        = 20;
  FOURCC_A8R8G8B8      = 21;
  FOURCC_X8R8G8B8      = 22;
  FOURCC_R5G6B5        = 23;
  FOURCC_X1R5G5B5      = 24;
  FOURCC_A1R5G5B5      = 25;
  FOURCC_A4R4G4B4      = 26;
  FOURCC_R3G3B2        = 27;
  FOURCC_A8            = 28;
  FOURCC_A8R3G3B2      = 29;
  FOURCC_X4R4G4B4      = 30;
  FOURCC_A2B10G10R10   = 31;
  FOURCC_A8B8G8R8      = 32;
  FOURCC_X8B8G8R8      = 33;
  FOURCC_G16R16        = 34;
  FOURCC_A2R10G10B10   = 35;
  FOURCC_A16B16G16R16  = 36;

  FOURCC_L8            = 50;
  FOURCC_A8L8          = 51;
  FOURCC_A4L4          = 52;
  FOURCC_DXT1          = $31545844;
  FOURCC_DXT2          = $32545844;
  FOURCC_DXT3          = $33545844;
  FOURCC_DXT4          = $34545844;
  FOURCC_DXT5          = $35545844;
  FOURCC_ATI1          = $31495441;
  FOURCC_ATI2          = $32495441;

  FOURCC_D16_LOCKABLE  = 70;
  FOURCC_D32           = 71;
  FOURCC_D24X8         = 77;
  FOURCC_D16           = 80;

  FOURCC_D32F_LOCKABLE = 82;

  FOURCC_L16           = 81;

// Floating point surface formats

// s10e5 formats (16-bits per channel)
  FOURCC_R16F          = 111;
  FOURCC_G16R16F       = 112;
  FOURCC_A16B16G16R16F = 113;

// IEEE s23e8 formats (32-bits per channel)
  FOURCC_R32F          = 114;
  FOURCC_G32R32F       = 115;
  FOURCC_A32B32G32R32F = 116;

  // DX10 header indicator
  FOURCC_DX10         = $30315844;//$47495844 = DXGI;

//DDS header dwFlags
  DDSD_CAPS      = 1; //Required in every .dds file.
  DDSD_HEIGHT    = 2; //Required in every .dds file.
  DDSD_WIDTH     = 4; //Required in every .dds file.
  DDSD_PITCH     = 8; //Required when pitch is provided for an uncompressed texture.
  DDSD_PIXELFORMAT = $1000;  //Required in every .dds file.
  DDSD_MIPMAPCOUNT = $20000; //Required in a mipmapped texture.
  DDSD_LINEARSIZE = $80000;  //Required when pitch is provided for a compressed texture.
  DDSD_DEPTH = $800000;      //Required in a depth texture.

//DDS header dwCaps
  //DDSCAPS_COMPLEX - Optional; must be used on any file that contains more than
  //one surface (a mipmap, a cubic environment map, or mipmapped volume texture).
  DDSCAPS_COMPLEX       = 8;
  DDSCAPS_MIPMAP	= $400000; //Optional; should be used for a mipmap.
  DDSCAPS_TEXTURE	= $1000;   //Required

//DDS header dwCaps2
  DDSCAPS2_CUBEMAP      = $200; //Required for a cube map.
  //Required when these surfaces are stored in a cube map.
  DDSCAPS2_CUBEMAP_POSITIVEX = $400;
  DDSCAPS2_CUBEMAP_NEGATIVEX = $800;
  DDSCAPS2_CUBEMAP_POSITIVEY = $1000;
  DDSCAPS2_CUBEMAP_NEGATIVEY = $2000;
  DDSCAPS2_CUBEMAP_POSITIVEZ = $4000;
  DDSCAPS2_CUBEMAP_NEGATIVEZ = $8000;
  DDSCAPS2_CUBEMAP_ALLFACES  = $FE00;
  //Required for a volume texture.
  DDSCAPS2_VOLUME            = $200000;

//DDS header DXT10 miscFlag
  DDS_RESOURCE_MISC_TEXTURECUBE = 4; //Indicates a 2D texture is a cube-map texture.

Type
  DWORD = cardinal;
  TFOURCC = array[0..3] of AnsiChar;

  PDxgiFormat = ^TDxgiFormat;
  DXGI_FORMAT                              = (
    DXGI_FORMAT_UNKNOWN                    = 0,
    DXGI_FORMAT_R32G32B32A32_TYPELESS      = 1,
    DXGI_FORMAT_R32G32B32A32_FLOAT         = 2,
    DXGI_FORMAT_R32G32B32A32_UINT          = 3,
    DXGI_FORMAT_R32G32B32A32_SINT          = 4,
    DXGI_FORMAT_R32G32B32_TYPELESS         = 5,
    DXGI_FORMAT_R32G32B32_FLOAT            = 6,
    DXGI_FORMAT_R32G32B32_UINT             = 7,
    DXGI_FORMAT_R32G32B32_SINT             = 8,
    DXGI_FORMAT_R16G16B16A16_TYPELESS      = 9,
    DXGI_FORMAT_R16G16B16A16_FLOAT         = 10,
    DXGI_FORMAT_R16G16B16A16_UNORM         = 11,
    DXGI_FORMAT_R16G16B16A16_UINT          = 12,
    DXGI_FORMAT_R16G16B16A16_SNORM         = 13,
    DXGI_FORMAT_R16G16B16A16_SINT          = 14,
    DXGI_FORMAT_R32G32_TYPELESS            = 15,
    DXGI_FORMAT_R32G32_FLOAT               = 16,
    DXGI_FORMAT_R32G32_UINT                = 17,
    DXGI_FORMAT_R32G32_SINT                = 18,
    DXGI_FORMAT_R32G8X24_TYPELESS          = 19,
    DXGI_FORMAT_D32_FLOAT_S8X24_UINT       = 20,
    DXGI_FORMAT_R32_FLOAT_X8X24_TYPELESS   = 21,
    DXGI_FORMAT_X32_TYPELESS_G8X24_UINT    = 22,
    DXGI_FORMAT_R10G10B10A2_TYPELESS       = 23,
    DXGI_FORMAT_R10G10B10A2_UNORM          = 24,
    DXGI_FORMAT_R10G10B10A2_UINT           = 25,
    DXGI_FORMAT_R11G11B10_FLOAT            = 26,
    DXGI_FORMAT_R8G8B8A8_TYPELESS          = 27,
    DXGI_FORMAT_R8G8B8A8_UNORM             = 28,
    DXGI_FORMAT_R8G8B8A8_UNORM_SRGB        = 29,
    DXGI_FORMAT_R8G8B8A8_UINT              = 30,
    DXGI_FORMAT_R8G8B8A8_SNORM             = 31,
    DXGI_FORMAT_R8G8B8A8_SINT              = 32,
    DXGI_FORMAT_R16G16_TYPELESS            = 33,
    DXGI_FORMAT_R16G16_FLOAT               = 34,
    DXGI_FORMAT_R16G16_UNORM               = 35,
    DXGI_FORMAT_R16G16_UINT                = 36,
    DXGI_FORMAT_R16G16_SNORM               = 37,
    DXGI_FORMAT_R16G16_SINT                = 38,
    DXGI_FORMAT_R32_TYPELESS               = 39,
    DXGI_FORMAT_D32_FLOAT                  = 40,
    DXGI_FORMAT_R32_FLOAT                  = 41,
    DXGI_FORMAT_R32_UINT                   = 42,
    DXGI_FORMAT_R32_SINT                   = 43,
    DXGI_FORMAT_R24G8_TYPELESS             = 44,
    DXGI_FORMAT_D24_UNORM_S8_UINT          = 45,
    DXGI_FORMAT_R24_UNORM_X8_TYPELESS      = 46,
    DXGI_FORMAT_X24_TYPELESS_G8_UINT       = 47,
    DXGI_FORMAT_R8G8_TYPELESS              = 48,
    DXGI_FORMAT_R8G8_UNORM                 = 49,
    DXGI_FORMAT_R8G8_UINT                  = 50,
    DXGI_FORMAT_R8G8_SNORM                 = 51,
    DXGI_FORMAT_R8G8_SINT                  = 52,
    DXGI_FORMAT_R16_TYPELESS               = 53,
    DXGI_FORMAT_R16_FLOAT                  = 54,
    DXGI_FORMAT_D16_UNORM                  = 55,
    DXGI_FORMAT_R16_UNORM                  = 56,
    DXGI_FORMAT_R16_UINT                   = 57,
    DXGI_FORMAT_R16_SNORM                  = 58,
    DXGI_FORMAT_R16_SINT                   = 59,
    DXGI_FORMAT_R8_TYPELESS                = 60,
    DXGI_FORMAT_R8_UNORM                   = 61,
    DXGI_FORMAT_R8_UINT                    = 62,
    DXGI_FORMAT_R8_SNORM                   = 63,
    DXGI_FORMAT_R8_SINT                    = 64,
    DXGI_FORMAT_A8_UNORM                   = 65,
    DXGI_FORMAT_R1_UNORM                   = 66,
    DXGI_FORMAT_R9G9B9E5_SHAREDEXP         = 67,
    DXGI_FORMAT_R8G8_B8G8_UNORM            = 68,
    DXGI_FORMAT_G8R8_G8B8_UNORM            = 69,
    DXGI_FORMAT_BC1_TYPELESS               = 70,
    DXGI_FORMAT_BC1_UNORM                  = 71,
    DXGI_FORMAT_BC1_UNORM_SRGB             = 72,
    DXGI_FORMAT_BC2_TYPELESS               = 73,
    DXGI_FORMAT_BC2_UNORM                  = 74,
    DXGI_FORMAT_BC2_UNORM_SRGB             = 75,
    DXGI_FORMAT_BC3_TYPELESS               = 76,
    DXGI_FORMAT_BC3_UNORM                  = 77,
    DXGI_FORMAT_BC3_UNORM_SRGB             = 78,
    DXGI_FORMAT_BC4_TYPELESS               = 79,
    DXGI_FORMAT_BC4_UNORM                  = 80,
    DXGI_FORMAT_BC4_SNORM                  = 81,
    DXGI_FORMAT_BC5_TYPELESS               = 82,
    DXGI_FORMAT_BC5_UNORM                  = 83,
    DXGI_FORMAT_BC5_SNORM                  = 84,
    DXGI_FORMAT_B5G6R5_UNORM               = 85,
    DXGI_FORMAT_B5G5R5A1_UNORM             = 86,
    DXGI_FORMAT_B8G8R8A8_UNORM             = 87,
    DXGI_FORMAT_B8G8R8X8_UNORM             = 88,
    DXGI_FORMAT_R10G10B10_XR_BIAS_A2_UNORM = 89,
    DXGI_FORMAT_B8G8R8A8_TYPELESS          = 90,
    DXGI_FORMAT_B8G8R8A8_UNORM_SRGB        = 91,
    DXGI_FORMAT_B8G8R8X8_TYPELESS          = 92,
    DXGI_FORMAT_B8G8R8X8_UNORM_SRGB        = 93,
    DXGI_FORMAT_BC6H_TYPELESS              = 94,
    DXGI_FORMAT_BC6H_UF16                  = 95,
    DXGI_FORMAT_BC6H_SF16                  = 96,
    DXGI_FORMAT_BC7_TYPELESS               = 97,
    DXGI_FORMAT_BC7_UNORM                  = 98,
    DXGI_FORMAT_BC7_UNORM_SRGB             = 99,
    DXGI_FORMAT_AYUV                       = 100,
    DXGI_FORMAT_Y410                       = 101,
    DXGI_FORMAT_Y416                       = 102,
    DXGI_FORMAT_NV12                       = 103,
    DXGI_FORMAT_P010                       = 104,
    DXGI_FORMAT_P016                       = 105,
    DXGI_FORMAT_420_OPAQUE                 = 106,
    DXGI_FORMAT_YUY2                       = 107,
    DXGI_FORMAT_Y210                       = 108,
    DXGI_FORMAT_Y216                       = 109,
    DXGI_FORMAT_NV11                       = 110,
    DXGI_FORMAT_AI44                       = 111,
    DXGI_FORMAT_IA44                       = 112,
    DXGI_FORMAT_P8                         = 113,
    DXGI_FORMAT_A8P8                       = 114,
    DXGI_FORMAT_B4G4R4A4_UNORM             = 115
//    DXGI_FORMAT_FORCE_UINT                 = $FFFFFFFF
  );
  TDxgiFormat = DXGI_FORMAT;

//DDS header DXT10 resourceDimension
  PD3d10ResourceDimension = ^TD3d10ResourceDimension;
  D3D10_RESOURCE_DIMENSION             = (
    D3D10_RESOURCE_DIMENSION_UNKNOWN   = 0,
    D3D10_RESOURCE_DIMENSION_BUFFER    = 1,
    D3D10_RESOURCE_DIMENSION_TEXTURE1D = 2,
    D3D10_RESOURCE_DIMENSION_TEXTURE2D = 3,
    D3D10_RESOURCE_DIMENSION_TEXTURE3D = 4
  );
  TD3d10ResourceDimension = D3D10_RESOURCE_DIMENSION;

  PDdsPixelformat = ^TDdsPixelformat;
  DDS_PIXELFORMAT = record
    dwSize: DWORD;
    dwFlags: DWORD;
    dwFourCC: DWORD;
    dwRGBBitCount: DWORD;
    dwRBitMask: DWORD;
    dwGBitMask: DWORD;
    dwBBitMask: DWORD;
    dwABitMask: DWORD;
  end;
  TDdsPixelformat = DDS_PIXELFORMAT;

  PDdsHeader = ^TDdsHeader;
  DDS_HEADER = record
    dwSize: DWORD;
    dwFlags: DWORD;
    dwHeight: DWORD;
    dwWidth: DWORD;
    dwPitchOrLinearSize: DWORD;
    dwDepth: DWORD;
    dwMipMapCount: DWORD;
    dwReserved1: array[0..10] of DWORD;
    ddspf: DDS_PIXELFORMAT;
    dwCaps: DWORD;
    dwCaps2: DWORD;
    dwCaps3: DWORD;
    dwCaps4: DWORD;
    dwReserved2: DWORD;
  end;
  TDdsHeader = DDS_HEADER;

  PDdsHeaderDxt10 = ^TDdsHeaderDxt10;
  DDS_HEADER_DXT10 = record
    dxgiFormat: DXGI_FORMAT;
    resourceDimension: D3D10_RESOURCE_DIMENSION;
    miscFlag: cardinal;
    arraySize: cardinal;
    reserved: cardinal;
  end;
  TDdsHeaderDxt10 = DDS_HEADER_DXT10;

  TDDSImage = record
    dwMagic: array[0..3] of ansichar;
    header: DDS_HEADER;
    header10: DDS_HEADER_DXT10;
    data: PByte;
    data2: PByte;
  end;

  TImageLevelDesc = record
    Width: Integer;
    Height: Integer;
    Depth: Integer;
    Offset: LongWord;
    Size: integer;
  end;

  TDDSImageDesc = record
    Format: Cardinal;
    InternalFormat: cardinal;
    ColorFormat: cardinal;
    DataType: cardinal;
    ElementSize: integer;
    DataSize: integer;
    ReservedMem: integer;
    Data: pointer;
    Width, Height, Depth, Levels: integer;
    LODS: array[0..15] of TImageLevelDesc;
    Compressed: boolean;
    CubeMap: boolean;
    TextureArray: boolean;
  end;
  PDDSImageDesc = ^TDDSImageDesc;


  DXTColBlock = record
    col0: GLushort;
    col1: GLushort;
    row: array[0..3] of GLubyte;
  end;
  PDXTColBlock = ^DXTColBlock;

  DXT3AlphaBlock = record
    row: array[0..3] of GLushort;
  end;
  PDXT3AlphaBlock = ^DXT3AlphaBlock;

  DXT5AlphaBlock = record
    alpha0 : GLubyte;
    alpha1 : GLubyte;
    row : array[0..5] of GLubyte;
  end;
  PDXT5AlphaBlock = ^DXT5AlphaBlock;

const
  CDX10CompressedFormat: array[0..21] of DXGI_FORMAT =
  ( DXGI_FORMAT_R9G9B9E5_SHAREDEXP,
    DXGI_FORMAT_BC1_TYPELESS, DXGI_FORMAT_BC1_UNORM,
    DXGI_FORMAT_BC1_UNORM_SRGB, DXGI_FORMAT_BC2_TYPELESS,
    DXGI_FORMAT_BC2_UNORM, DXGI_FORMAT_BC2_UNORM_SRGB,
    DXGI_FORMAT_BC3_TYPELESS, DXGI_FORMAT_BC3_UNORM,
    DXGI_FORMAT_BC3_UNORM_SRGB, DXGI_FORMAT_BC4_TYPELESS,
    DXGI_FORMAT_BC4_UNORM, DXGI_FORMAT_BC4_SNORM,
    DXGI_FORMAT_BC5_TYPELESS, DXGI_FORMAT_BC5_UNORM,
    DXGI_FORMAT_BC5_SNORM, DXGI_FORMAT_BC6H_TYPELESS,
    DXGI_FORMAT_BC6H_UF16, DXGI_FORMAT_BC6H_SF16,
    DXGI_FORMAT_BC7_TYPELESS, DXGI_FORMAT_BC7_UNORM,
    DXGI_FORMAT_BC7_UNORM_SRGB );

function MAKEFOURCC(s: ansistring): cardinal;
function DDSLoadFromStream(aStream: TStream): PDDSImageDesc;

implementation

function max(a,b: single): single;
begin
  if a>b then result:=a else result:=b;
end;

function MAKEFOURCC(s: ansistring): cardinal;
begin
  result:=ord(s[1]) or (ord(s[2]) shl 8) or (ord(s[3]) shl 16) or (ord(s[4]) shl 24);
end;

{$if CompilerVersion>17.0}{$REGION 'FlipBlocks'}{$ifend}
  // flip a DXT1 color block
  ////////////////////////////////////////////////////////////
  procedure flip_blocks_dxtc1( data : PByte; numBlocks: integer);
  var
    curblock : PDXTColBlock;
    temp : GLubyte;
    i : integer;
  begin
    curblock := PDXTColBlock( data );
    for i := 0 to  numBlocks-1 do begin
      temp := curblock.row[0];
      curblock.row[0] := curblock.row[3];
      curblock.row[3] := temp;
      temp := curblock.row[1];
      curblock.row[1] := curblock.row[2];
      curblock.row[2] := temp;

      Inc( curblock );
    end;
  end;

  // flip a DXT3 color block
  ////////////////////////////////////////////////////////////
  procedure flip_blocks_dxtc3( data: PByte; numBlocks: integer );
  var
    curblock : PDXTColBlock;
    alphablock : PDXT3AlphaBlock;
    tempS : GLushort;
    tempB : GLubyte;
    i : integer;
  begin
    curblock := PDXTColBlock( data );
    for i := 0 to numBlocks-1 do
    begin
      alphablock := PDXT3AlphaBlock( curblock );

      tempS := alphablock.row[0];
      alphablock.row[0] := alphablock.row[3];
      alphablock.row[3] := tempS;
      tempS := alphablock.row[1];
      alphablock.row[1] := alphablock.row[2];
      alphablock.row[2] := tempS;

      Inc( curblock );

      tempB := curblock.row[0];
      curblock.row[0] := curblock.row[3];
      curblock.row[3] := tempB;
      tempB := curblock.row[1];
      curblock.row[1] := curblock.row[2];
      curblock.row[2] := tempB;

      Inc( curblock );
    end;
  end;

  //
  // flip a DXT5 alpha block
  ////////////////////////////////////////////////////////////
  procedure flip_dxt5_alpha( block : PDXT5AlphaBlock);
  const
    mask = $00000007;          // bits = 00 00 01 11
  var
    gBits : array[0..3, 0..3] of GLubyte;
    bits  : Integer;
  begin
    bits := 0;
    Move(block.row[0], bits, sizeof(GLubyte) * 3);

    gBits[0][0] := GLubyte(bits and mask);
    bits := bits shr 3;
    gBits[0][1] := GLubyte(bits and mask);
    bits := bits shr 3;
    gBits[0][2] := GLubyte(bits and mask);
    bits := bits shr 3;
    gBits[0][3] := GLubyte(bits and mask);
    bits := bits shr 3;
    gBits[1][0] := GLubyte(bits and mask);
    bits := bits shr 3;
    gBits[1][1] := GLubyte(bits and mask);
    bits := bits shr 3;
    gBits[1][2] := GLubyte(bits and mask);
    bits := bits shr 3;
    gBits[1][3] := GLubyte(bits and mask);

    bits := 0;
    Move(block.row[3], bits, sizeof(GLubyte) * 3);

    gBits[2][0] := GLubyte(bits and mask);
    bits := bits shr 3;
    gBits[2][1] := GLubyte(bits and mask);
    bits := bits shr 3;
    gBits[2][2] := GLubyte(bits and mask);
    bits := bits shr 3;
    gBits[2][3] := GLubyte(bits and mask);
    bits := bits shr 3;
    gBits[3][0] := GLubyte(bits and mask);
    bits := bits shr 3;
    gBits[3][1] := GLubyte(bits and mask);
    bits := bits shr 3;
    gBits[3][2] := GLubyte(bits and mask);
    bits := bits shr 3;
    gBits[3][3] := GLubyte(bits and mask);

    // clear existing alpha bits
    FillChar( block.row, sizeof(GLubyte) * 6, 0);

    bits := block.row[0]+block.row[1]*$100+block.row[2]*$10000;

    bits := bits or (gBits[3][0] shl 0);
    bits := bits or (gBits[3][1] shl 3);
    bits := bits or (gBits[3][2] shl 6);
    bits := bits or (gBits[3][3] shl 9);

    bits := bits or (gBits[2][0] shl 12);
    bits := bits or (gBits[2][1] shl 15);
    bits := bits or (gBits[2][2] shl 18);
    bits := bits or (gBits[2][3] shl 21);

    block.row[0] := bits and $FF;
    block.row[1] := (bits shr 8) and $FF;
    block.row[2] := (bits shr 16) and $FF;

    bits := block.row[3]+block.row[4]*$100+block.row[5]*$10000;

    bits := bits or (gBits[1][0] shl 0);
    bits := bits or (gBits[1][1] shl 3);
    bits := bits or (gBits[1][2] shl 6);
    bits := bits or (gBits[1][3] shl 9);

    bits := bits or (gBits[0][0] shl 12);
    bits := bits or (gBits[0][1] shl 15);
    bits := bits or (gBits[0][2] shl 18);
    bits := bits or (gBits[0][3] shl 21);

    block.row[3] := bits and $FF;
    block.row[4] := (bits shr 8) and $FF;
    block.row[5] := (bits shr 16) and $FF;
  end;

  //
  // flip a DXT5 color block
  ////////////////////////////////////////////////////////////
  procedure flip_blocks_dxtc5( data: PByte; numBlocks: integer );
  var
    curblock : PDXTColBlock;
    temp : GLubyte;
    i : integer;
  begin
    curblock := PDXTColBlock( data );
    for i := 0 to numBlocks-1 do
    begin
      flip_dxt5_alpha( PDXT5AlphaBlock( curblock ) );
      Inc( curblock );
      temp := curblock.row[0];
      curblock.row[0] := curblock.row[3];
      curblock.row[3] := temp;
      temp := curblock.row[1];
      curblock.row[1] := curblock.row[2];
      curblock.row[2] := temp;
      Inc( curblock );
    end;
  end;

{$if CompilerVersion>17.0}{$ENDREGION}{$ifend}

procedure flipSurface(chgData: Pbyte; w, h, d: integer; DDSDesc: PDDSImageDesc);
var
  lineSize: integer;
  sliceSize: integer;
  tempBuf: Pbyte;
  i, j: integer;
  top, bottom: Pbyte;
  flipblocks: procedure(data: Pbyte; size: integer);

begin
  if d = 0 then d := 1;

  if not DDSDesc.Compressed then begin
    lineSize := DDSDesc.ElementSize * w;
    sliceSize := lineSize * h;
    GetMem(tempBuf, lineSize);

    for i := 0 to d - 1 do begin
      top := chgData; Inc(top, i * sliceSize);
      bottom := top;  Inc(bottom, sliceSize - lineSize);

      for j := 0 to (h div 2) - 1 do begin
        Move(top^, tempBuf^, lineSize);
        Move(bottom^, top^, lineSize);
        Move(tempBuf^, bottom^, lineSize);
        Inc(top, lineSize);
        Dec(bottom, lineSize);
      end;
    end;
    FreeMem(tempBuf);
  end else begin
    w := (w + 3) div 4; h := (h + 3) div 4;
    case DDSDesc.ColorFormat of
      GL_COMPRESSED_RGBA_S3TC_DXT1_EXT: flipblocks := flip_blocks_dxtc1;
      GL_COMPRESSED_RGBA_S3TC_DXT3_EXT: flipblocks := flip_blocks_dxtc3;
      GL_COMPRESSED_RGBA_S3TC_DXT5_EXT: flipblocks := flip_blocks_dxtc5;
    else
      exit;
    end;

    lineSize := DDSDesc.ElementSize * w;
    sliceSize := lineSize * h;
    GetMem(tempBuf, lineSize);
    for i := 0 to d - 1 do begin
      top := chgData; Inc(top, i * sliceSize);
      bottom := top;  Inc(bottom, sliceSize - lineSize);

      for j := 0 to (h div 2) - 1 do begin
        if top = bottom then begin flipblocks(top, w); break; end;

        flipblocks(top, w); flipblocks(bottom, w);

        Move(top^, tempBuf^, lineSize);
        Move(bottom^, top^, lineSize);
        Move(tempBuf^, bottom^, lineSize);

        Inc(top, lineSize);
        Dec(bottom, lineSize);
      end;
    end;
    FreeMem(tempBuf);
  end;
end;

function ReservCompMem(bs: integer; var desc: PDDSImageDesc): integer;
var i,s: integer;
    mw,mh,ms,offset: integer;
begin
  with desc^ do begin
    mw:=width; mh:=height;
    offset:=0; s:=0;
    for i:=0 to Levels-1 do begin
      if mw=0 then mw:=1; if mh=0 then mh:=1;
      ms:=trunc(max(1, mw / 4) * max(1, mh / 4)*bs); s:=s+ms;
      LODS[i].Offset:=offset; offset := offset + ms;
      LODS[i].Width:=mw; LODS[i].Height:=mh;
      LODS[i].Size:=ms; LODS[i].Depth:=1;
      mw:=mw shr 1; mh:=mh shr 1;
    end; result:=s;
    getmem(Data,s); DataSize:=s;
  end;
end;

procedure CompleatLods(var desc: PDDSImageDesc; LodsCount: integer);
var i,j,w,h,x,y,b: integer;
    pb: PByteArray;
    c: integer;
begin
  b:=desc.ElementSize;
  pb:=PByteArray(desc.Data);
  with desc^ do begin
    for i:=Levels to LodsCount-1 do begin
      w:=LODS[i].Width; h:=LODS[i].Height;
      for y:=0 to h-1 do for x:=0 to w-1 do begin
        for j:=0 to b-1 do begin
          c:=
            pb[LODS[i-1].Offset+y*2*w*b+x*2*b+j]+
            pb[LODS[i-1].Offset+(y*2+1)*w*b+x*2*b+j]+
            pb[LODS[i-1].Offset+y*2*w*b+(x*2+1)*b+j]+
            pb[LODS[i-1].Offset+(y*2+1)*w*b+(x*2+1)*b+j];
          c:=c div 4;
          pb[LODS[i].Offset+y*w*b+x*b+j]:=c and $FF;
        end;
      end;
    end;
  end;
end;

function ReservUncompMem(bpp: byte; var desc: PDDSImageDesc): integer;
var i,s: integer;
    mw,mh,ms,offset: integer;
    b: byte;
begin
  with desc^ do begin
    mw:=width; mh:=height;
    offset:=0; s:=0; b:=(bpp); i:=0;
    repeat
      if mw=0 then mw:=1; if mh=0 then mh:=1;
      ms:=mw*mh*b; s:=s+ms;
      LODS[i].Offset:=offset; offset := offset + ms;
      LODS[i].Width:=mw; LODS[i].Height:=mh;
      LODS[i].Size:=ms; LODS[i].Depth:=1;
      mw:=mw shr 1; mh:=mh shr 1; inc(i);
    until (i=Levels) or (mw+mh=0);
    if desc.CubeMap then getmem(Data,s*6)
    //else if desc.TextureArray then getmem(Data,s*Depth)
    else getmem(Data,s);
    DataSize:=s;

{    if (i<>Levels) and (Levels>1) then begin
      result:=LODS[Levels].Offset;
      CompleatLods(desc,i); Levels:=i;
    end else
}
    Levels:=1; result:=s;

{    for i:=0 to Levels-1 do begin
      if mw=0 then mw:=1; if mh=0 then mh:=1;
      ms:=mw*mh*b; s:=s+ms;
      LODS[i].Offset:=offset; offset := offset + ms;
      LODS[i].Width:=mw; LODS[i].Height:=mh;
      LODS[i].Size:=ms; LODS[i].Depth:=1;
      mw:=mw shr 1; mh:=mh shr 1;
    end;
}
//    result:=s;
  end;
end;

procedure SwapARGB(data: PInteger; count: integer);assembler;
asm
@loop:
  mov ecx, dword ptr [eax];
  mov byte ptr [eax], cl;
  shr ecx, 24
  mov byte ptr [eax+3], cl;
  add eax, 4;
  dec edx;
  jnz @loop;
end;

procedure SwapARGB16(data: PInteger; count: integer);assembler;
asm
@loop:
  mov cx, word ptr [eax];
  xchg cx, word ptr [eax+6];
  mov word ptr [eax], cx;
  add eax, 8;
  dec edx;
  jnz @loop;
end;

{$I DDSFormats.inc}

function DDSLoadFromStream(aStream: TStream): PDDSImageDesc;
var dds: TDDSImage;
    i,mw,mh: integer;
    glFormat: cardinal;
    buffSize: cardinal;
    bs,ms: cardinal;
    offset: cardinal;
    Rbits,Gbits,Bbits,ABits: byte;
    p: pointer;
    f,FaceCount: integer;
begin
  aStream.Read(dds.dwMagic,4);
  assert(dds.dwMagic='DDS ','Invalid DDS file');
  aStream.Read(dds.header, Sizeof(TDDSHeader));
  assert((dds.header.dwSize=sizeof(DDS_HEADER)) and
    (dds.header.ddspf.dwSize=sizeof(DDS_PIXELFORMAT)),'Invalid DDS file');
  if (dds.header.ddspf.dwFlags and DDPF_FOURCC <> 0)
  and (dds.header.ddspf.dwFourCC = FOURCC_DX10)
  then aStream.Read(dds.header10, Sizeof(DDS_HEADER_DXT10));
  new(result);
  with dds.header,result^ do begin
    Width:=dwWidth; Height:=dwHeight; Depth:=dwDepth;
    if ((dwCaps2 and DDSCAPS2_VOLUME) <> 0) and (dwDepth > 0)
    then Depth := dwDepth else Depth := 0;
    if (dwFlags and DDSD_MIPMAPCOUNT) > 0
    then Levels := dwMipMapCount else Levels:= 1;
    CubeMap:=((dwCaps2 and DDSCAPS2_CUBEMAP_ALLFACES)=DDSCAPS2_CUBEMAP_ALLFACES)
      or (dds.header10.miscFlag=DDS_RESOURCE_MISC_TEXTURECUBE);
    if CubeMap then assert(Width=Height,'Invalid cubemap');
    TextureArray:=(dds.header10.arraySize>1) and (not CubeMap);
    //Compressed:=isCompressedFormat(dds);
    if CubeMap then FaceCount:=6 else FaceCount:=1;

    DDSToGLFormats(dds,result);

    if Compressed then
      buffsize:=ReservCompMem(result.ElementSize,result)
    else buffsize:=ReservUncompMem(result.ElementSize,Result);
    aStream.Read(Result.Data^,buffSize*FaceCount);

    ReservedMem:=buffSize*FaceCount;
    if (not Compressed) and ((ddspf.dwRGBBitCount=32) or (result.ElementSize=4))
    then SwapARGB(data,buffSize div 4);
    //if not Cubemap then
    for f:=0 to FaceCount-1 do
    for i:=0 to result.Levels-1 do begin
      p:=pointer(integer(result.Data)+result.LODS[i].Offset+buffsize*f);
      flipSurface(p,result.LODS[i].width,result.LODS[i].height,1,result);
    end;
  end;
end;

end.

