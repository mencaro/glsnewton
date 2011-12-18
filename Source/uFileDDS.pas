unit uFileDDS;

interface

uses Classes, uDXTC, SysUtilsLite;

Type

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
    Data: pointer;
    Width, Height, Depth, Levels: integer;
    LODS: array[0..15] of TImageLevelDesc;
    Compressed: boolean;
    CubeMap: boolean;
    TextureArray: boolean;
  end;
  PDDSImageDesc = ^TDDSImageDesc;

function LoadDDSTexture(Filename: String; LoadFromResource : Boolean = false): PDDSImageDesc;

implementation

const
   // ARB Extension #52 - GL_ARB_texture_compression_rgtc
   GL_COMPRESSED_RED_RGTC1                             =$8DBB;
   GL_COMPRESSED_SIGNED_RED_RGTC1                      =$8DBC;
   GL_COMPRESSED_RG_RGTC2                              =$8DBD;
   GL_COMPRESSED_SIGNED_RG_RGTC2                       =$8DBE;

  // GL_EXT_texture_compression_latc (#331)
   GL_COMPRESSED_LUMINANCE_LATC1_EXT                = $8C70;
   GL_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT         = $8C71;
   GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT          = $8C72;
   GL_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT   = $8C73;

   // // GL_ATI_texture_compression_3dc
   GL_COMPRESSED_LUMINANCE_ALPHA_3DC_ATI            = $8837;

   // GL_EXT_texture_compression_rgtc (#332)
   GL_COMPRESSED_RED_RGTC1_EXT                      = $8DBB;
   GL_COMPRESSED_SIGNED_RED_RGTC1_EXT               = $8DBC;
   GL_COMPRESSED_RED_GREEN_RGTC2_EXT                = $8DBD;
   GL_COMPRESSED_SIGNED_RED_GREEN_RGTC2_EXT         = $8DBE;

   GL_COMPRESSED_SRGB_EXT                               = $8C48;
   GL_COMPRESSED_SRGB_ALPHA_EXT                         = $8C49;
   GL_COMPRESSED_SLUMINANCE_EXT                         = $8C4A;
   GL_COMPRESSED_SLUMINANCE_ALPHA_EXT                   = $8C4B;
   GL_COMPRESSED_SRGB_S3TC_DXT1_EXT                     = $8C4C;
   GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT               = $8C4D;
   GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT               = $8C4E;
   GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT               = $8C4F;

   // ARB Extension #12 - GL_ARB_texture_compression
   GL_COMPRESSED_ALPHA_ARB                           = $84E9;
   GL_COMPRESSED_LUMINANCE_ARB                       = $84EA;
   GL_COMPRESSED_LUMINANCE_ALPHA_ARB                 = $84EB;
   GL_COMPRESSED_INTENSITY_ARB                       = $84EC;
   GL_COMPRESSED_RGB_ARB                             = $84ED;
   GL_COMPRESSED_RGBA_ARB                            = $84EE;
   GL_TEXTURE_COMPRESSION_HINT_ARB                   = $84EF;
   GL_TEXTURE_COMPRESSED_IMAGE_SIZE_ARB              = $86A0;
   GL_TEXTURE_COMPRESSED_ARB                         = $86A1;
   GL_NUM_COMPRESSED_TEXTURE_FORMATS_ARB             = $86A2;
   GL_COMPRESSED_TEXTURE_FORMATS_ARB                 = $86A3;

   // GL_EXT_texture_compression_s3tc (#198)
   GL_COMPRESSED_RGB_S3TC_DXT1_EXT                  = $83F0;
   GL_COMPRESSED_RGBA_S3TC_DXT1_EXT                 = $83F1;
   GL_COMPRESSED_RGBA_S3TC_DXT3_EXT                 = $83F2;
   GL_COMPRESSED_RGBA_S3TC_DXT5_EXT                 = $83F3;

const
  cCompressedFormat: array[0..24] of cardinal = (
    GL_COMPRESSED_RGB_ARB,
    GL_COMPRESSED_RGBA_ARB,
    GL_COMPRESSED_ALPHA_ARB,
    GL_COMPRESSED_LUMINANCE_ARB,
    GL_COMPRESSED_LUMINANCE_ALPHA_ARB,
    GL_COMPRESSED_INTENSITY_ARB,
    GL_COMPRESSED_RGB_ARB,
    GL_COMPRESSED_RGB_ARB,
    GL_COMPRESSED_RGB_S3TC_DXT1_EXT,
    GL_COMPRESSED_RGBA_S3TC_DXT1_EXT,
    GL_COMPRESSED_RGBA_S3TC_DXT3_EXT,
    GL_COMPRESSED_RGBA_S3TC_DXT5_EXT,
    GL_COMPRESSED_SRGB_S3TC_DXT1_EXT,
    GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT,
    GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT,
    GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT,
    GL_COMPRESSED_LUMINANCE_LATC1_EXT,
    GL_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT,
    GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT,
    GL_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT,
    GL_COMPRESSED_LUMINANCE_ALPHA_3DC_ATI,
    GL_COMPRESSED_RED_RGTC1,
    GL_COMPRESSED_SIGNED_RED_RGTC1,
    GL_COMPRESSED_RG_RGTC2,
    GL_COMPRESSED_SIGNED_RG_RGTC2);

function IsCompressedFormat(intFormat: cardinal): boolean;
var i: cardinal;
begin
  Result := false;
  for i := 0 to High(cCompressedFormat) do
    if cCompressedFormat[i] = intFormat then begin
      Result := true; exit;
    end;
end;

function GetDDSDataSize(DDSDesc: PDDSImageDesc): integer;
var i,w,h,blockSize,offs: integer;
begin
  w:=ddsdesc.Width; h:=ddsdesc.Height; offs:=0;
  if DDSDesc.Format=GL_COMPRESSED_RGBA_S3TC_DXT1_EXT then
    blockSize:=8 else blockSize:=16;
  DDSDesc.DataSize:=0; i:=0;
  while i<DDSDesc.Levels do with DDSDesc^ do begin
    if w=0 then w:=1; if h=0 then h:=1;
    LODS[i].Width:=w; LODS[i].Height:=h; LODS[i].Depth:=i;
    if Compressed then LODS[i].Size:=trunc(((w+3)/4)*((h+3)/4)*blockSize)
    else LODS[i].Size:=w*h*ElementSize;
    if (LODS[i].Size and 3) <> 0 then LODS[i].Size := 4 * (1 + LODS[i].Size div 4); //??
    if CubeMap then LODS[i].Size:=LODS[i].Size*6;
    DataSize:=DataSize+DDSDesc.LODS[i].Size;
    LODS[i].Offset:=offs; offs:=offs+LODS[i].Size;
    if not TextureArray then begin w:=w shr 1; h:=h shr 1; end;
    inc(i);
  end;
  result:=DDSDesc.DataSize;
end;

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

function LoadFromStream(Stream: TStream): PDDSImageDesc;
var
  DDSDesc: PDDSImageDesc;
  header: TDDSHeader;
  DX10header: TDDS_HEADER_DXT10;
  face, faceCount, level: Integer;
  offset: Integer;
  bDXT10Header: Boolean;
  DataSize: integer;
  Image: pointer;
  i: integer;

  function GetLevelAddress(ALevel: Byte; aFace: byte = 0): Pointer;
  begin
    Result := DDSDesc.Data;
    Inc(PByte(Result), DDSDesc.LODS[ALevel].Offset);
    if aFace<>0 then
      Inc(PByte(Result), aFace*(DDSDesc.LODS[ALevel].Size div 6));
  end;

begin
  stream.Read(header, Sizeof(TDDSHeader));
  // DDS files always start with the same magic number ("DDS ")
  assert(TFOURCC(header.Magic)='DDS ','Invalid DDS file');
  // Verify header to validate DDS file
  assert((header.SurfaceFormat.dwSize=sizeof(TDDSURFACEDESC2)) and
    (header.SurfaceFormat.ddpf.dwSize=sizeof(TDDPIXELFORMAT)),
    'Invalid DDS file');
  // Check for DX10 extension
  bDXT10Header := (header.SurfaceFormat.ddpf.dwFlags and DDPF_FOURCC <> 0)
    and (header.SurfaceFormat.ddpf.dwFourCC = FOURCC_DX10);
  if bDXT10Header then stream.Read(DX10header, Sizeof(TDDS_HEADER_DXT10));
  new(DDSDesc); new(Image);
  with header.SurfaceFormat, DDSDesc^ do begin
    {: There are flags that are supposed to mark these fields as valid,
       but some dds files don't set them properly }
    Width := dwWidth;  Height := dwHeight;
    // check if image is a volume texture
    if ((dwCaps2 and DDSCAPS2_VOLUME) <> 0) and (dwDepth > 0)
    then Depth := dwDepth else Depth := 0;

    if (dwFlags and DDSD_MIPMAPCOUNT) > 0
    then Levels := dwMipMapCount else Levels:= 1;

    //check cube-map faces
    DDSDesc.CubeMap := false;  faceCount := 0;
    if (dwCaps2 and DDSCAPS2_CUBEMAP) <> 0 then begin
      //this is a cubemap, count the faces
      if (dwCaps2 and DDSCAPS2_CUBEMAP_POSITIVEX) <> 0 then Inc(faceCount);
      if (dwCaps2 and DDSCAPS2_CUBEMAP_NEGATIVEX) <> 0 then Inc(faceCount);
      if (dwCaps2 and DDSCAPS2_CUBEMAP_POSITIVEY) <> 0 then Inc(faceCount);
      if (dwCaps2 and DDSCAPS2_CUBEMAP_NEGATIVEY) <> 0 then Inc(faceCount);
      if (dwCaps2 and DDSCAPS2_CUBEMAP_POSITIVEZ) <> 0 then Inc(faceCount);
      if (dwCaps2 and DDSCAPS2_CUBEMAP_NEGATIVEZ) <> 0 then Inc(faceCount);
      //check for a complete cubemap
      if (faceCount<>6) or (Width<>Height) then assert(false,'Invalid cubemap');
      DDSDesc.CubeMap := true;
    end;
    DDSDesc.TextureArray := false;

    if not DDSHeaderToGLEnum(header, DX10header, bDXT10Header, InternalFormat,
      ColorFormat, DataType, ElementSize)
    then assert(false,'DDS errorneus format');
    DDSDesc.Compressed := IsCompressedFormat(InternalFormat);
  end; // of with

  offset := 0;
  DataSize:=GetDDSDataSize(DDSDesc);
  ReallocMem(Image, DataSize);
  DDSDesc.Data:=Image;

  if not DDSDesc.CubeMap then faceCount := 1;
  with DDSDesc^ do
  for face := 0 to faceCount - 1 do begin
    if offset > 0 then stream.Seek(offset, Ord(soCurrent));
    for level := 0 to DDSDesc.Levels - 1 do begin
      stream.Read(GetLevelAddress(level, face)^, DDSDesc.LODS[level].Size div faceCount);
      if not DDSDesc.CubeMap then //and vVerticalFlipDDS then
        flipSurface(GetLevelAddress(level, face), LODS[level].Width,
          LODS[level].Height, LODS[level].Depth, DDSDesc);
    end;
  end; // for level
  result:=DDSDesc;
end;

function LoadDDSTexture(Filename: String; LoadFromResource : Boolean): PDDSImageDesc;
var ResStream : TResourceStream;      // used for loading from resource
    fStream: TFileStream;

begin
  result := nil;
  if LoadFromResource then begin// Load from resource
    try
      ResStream := TResourceStream.Create(hInstance, PChar(copy(Filename, 1, Pos('.', Filename)-1)), 'DDS');
      result:=LoadFromStream(ResStream);
      ResStream.Free;
    except on
      EResNotFound do
        assert(false,'File not found in resource - ' + Filename +' (DDS Texture)');
      else
        assert(false, 'Unable to read from resource - ' + Filename+ ' (DDS Texture)');
    end;
  end else begin
    if FileExists(Filename) then begin
      fStream:=TFileStream.Create(FileName, fmOpenRead);
      result:=LoadFromStream(fStream);
      fStream.Free;
    end;
  end;
end;

end.
