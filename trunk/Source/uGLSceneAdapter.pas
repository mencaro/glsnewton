unit uGLSceneAdapter;

interface

uses Classes, SysUtilsLite,
  GLScene, GLFile3ds, GLFileObj, GLMaterial, GLVectorFileObjects,
  GLTextureFormat, OpenGL1x,
  uVBO, VBOMesh, uMaterialObjects, uMaterials, uTextures, uMiscUtils;

Type
  TGLSceneAdapter = class
  private
    FMaterialObjects: TList;
    FGeometryObjects: TList;
    FLightObjects: TList;
    FVBOMesh: TVBOMesh;
  public
    constructor Create;
    destructor Destroy;override;
    function ImportMaterial(lm: TGLLibMaterial): TMaterialObject; overload;
    function ImportMaterial(glmat: TGLMaterial; MatName: string=''): TMaterial; overload;
    function ImportTexture(lm: TGLLibMaterial): TTexture;
    function ImportLight(LightSource: TGLLightSource): TLightSource;
    procedure ImportFreeForm(ff: TGLFreeForm; var MeshList: TList);

    property VBOMesh: TVBOMesh read FVBOMesh write FVBOMesh;
  end;

implementation

{ TGLSceneAdapter }

constructor TGLSceneAdapter.Create;
begin
  inherited;
  FMaterialObjects:=TList.Create;
  FGeometryObjects:=TList.Create;
  FLightObjects:=TList.Create;
  VBOMesh:=nil;
end;

destructor TGLSceneAdapter.Destroy;
var i: integer;
    l: TList;
begin
  FreeObjectList(FMaterialObjects);
  FreeObjectList(FLightObjects);
  for i:=0 to FGeometryObjects.Count-1 do begin
    l:=FGeometryObjects[i]; FreeVBOList(l);
  end; FGeometryObjects.Free;
  inherited;
end;

procedure TGLSceneAdapter.ImportFreeForm(ff: TGLFreeForm; var MeshList: TList);
var TempBuff:PVBOBuffer;
    i,j:integer;
    M:TMeshOBject;
    FG:TFGVertexIndexList;
    lm: TGLLibMaterial;
    MatList: TStringList;
    s:string;
const ModeName:array[0..5] of string=('Triangles','TriangleStrip', 'FaceGroups','Triangles', 'TrianglesFAN', 'Quads');
     cOpenGLMM : array [TFaceGroupMeshMode] of Integer =
         (GL_TRIANGLES, GL_TRIANGLE_STRIP, GL_TRIANGLES, GL_TRIANGLE_FAN, GL_QUADS);
begin
  MatList := TStringList.Create;
  if not assigned(MeshList) then MeshList:=TList.Create;
  for i:=0 to FF.MeshObjects.Count-1 do begin
    M:=FF.MeshObjects[i];
    if M.Mode=momFaceGroups then
    for j:=0 to M.FaceGroups.Count-1 do begin
     FG:=TFGVertexNormalTexIndexList(M.FaceGroups[j]);
     if FG.VertexIndices.Count>0 then begin
      new(TempBuff);InitVBOBuff(TempBuff^,cOpenGLMM[FG.Mode],DrawElements);
      with TempBuff^ do begin
        MatName := FG.MaterialName; Name := M.Name;
        if MatName='' then MatName:='Null';
        if MatList.Values[MatName]='' then
          MatList.Values[MatName]:=inttostr(MatList.Count);
        idxBindOnce:=false;
        Vertexes.Add(M.Vertices);
        Normals.Add(M.Normals);
        TexCoords.Add(M.TexCoords);
        if M.Mode=momFaceGroups then Indices.Add(FG.VertexIndices);
      end;
      MeshList.Add(TempBuff);GenVBOBuff(TempBuff^,false);
     end;
    end;
  end;
  SortListByMaterial(MeshList);
  if not assigned(FVBOMesh) then exit;
  for i:=0 to MatList.Count-1 do begin
    s:=MatList.Names[i];
    lm:=FF.MaterialLibrary.LibMaterialByName(s);
    if assigned(lm) then FVBOMesh.MaterialObjects.Add(ImportMaterial(lm));
  end;
  MatList.Free;
end;

function TGLSceneAdapter.ImportLight(LightSource: TGLLightSource): TLightSource;
var LS: TLightSource;
begin
  LS:=TLightSource.Create;
  LS.Enabled:=LightSource.Shining;
  case ord(LightSource.LightStyle) of
    0: LS.LightStyle:=lsSpot;
    1: LS.LightStyle:=lsOmni;
    2: LS.LightStyle:=lsParallel;
  end;
  LS.LightModel:=lmGouraud;
  LS.SpotDirection:=LightSource.SpotDirection.AsVector;
  LS.SpotCutOff:=LightSource.SpotCutOff;
  LS.SpotExponent:=LightSource.SpotExponent;
  LS.Position:=LightSource.AbsolutePosition;
  LS.Ambient.ColorVector:=LightSource.Ambient.Color;
  LS.Diffuse.ColorVector:=LightSource.Diffuse.Color;
  LS.Specular.ColorVector:=LightSource.Specular.Color;
  LS.ConstAttenuation:=LightSource.ConstAttenuation;
  LS.LinearAttenuation:=LightSource.LinearAttenuation;
  LS.QuadraticAttenuation:=LightSource.QuadraticAttenuation;
  LS.LightSlot:=LightSource.LightID-GL_LIGHT0;
  result:=LS; FLightObjects.Add(LS); VBOMesh.Lights.Add(LS);
end;

function TGLSceneAdapter.ImportMaterial(glmat: TGLMaterial; MatName: string): TMaterial;
var mat: TMaterial;
begin
  mat:=TMaterial.Create; result:=mat;

  if not Assigned(glmat) then exit;
  if MatName<>'' then mat.Name:=MatName
  else mat.Name:='GLMaterial'+inttostr(integer(glmat));
  with mat.Properties,glmat.FrontProperties do begin
    AmbientColor.ColorVector:=Ambient.Color;
    DiffuseColor.ColorVector:=Diffuse.Color;
    SpecularColor.ColorVector:=Specular.Color;
    EmissionColor.ColorVector:=Emission.Color;
    Shininess:=glmat.FrontProperties.Shininess;
  end;
end;

function TGLSceneAdapter.ImportTexture(lm: TGLLibMaterial): TTexture;
var tex: TTexture;
    ifmt: TGLInternalFormat;
const CTexModes: array[0..4] of TTextureCombines =
      (tcDecal, tcModulate, tcBlend, tcReplace, tcAdd);
begin
  if (lm.Material.Texture.Image.Width*lm.Material.Texture.Image.Height<=0)
  or (lm.Material.Texture.Disabled) then tex:=nil else begin
    tex:=TTexture.Create; tex.Name:=lm.Name+inttostr(integer(lm));
    tex.SetTarget(ttTexture2D);
     case ord(lm.Material.Texture.MagFilter) of
       0: tex.magFilter:=mgNearest;
       1: tex.magFilter:=mgLinear;
     end;

     case ord(lm.Material.Texture.MinFilter) of
       0: tex.minFilter:=mnNearest;
       1: tex.minFilter:=mnLinear;
       2: tex.minFilter:=mnNearestMipmapNearest;
       3: tex.minFilter:=mnLinearMipmapNearest;
       4: begin tex.minFilter:=mnNearestMipmapLinear; tex.Mipmapping:=true; end;
       5: begin tex.minFilter:=mnLinearMipmapLinear; tex.Mipmapping:=true; end;
     end;

     case ord(lm.Material.Texture.TextureWrap) of
       0: begin tex.WrapS:=twRepeat; tex.WrapT:=twRepeat; end;
       1: begin tex.WrapS:=twClamp; tex.WrapT:=twClamp; end;
       2: tex.WrapT:=twRepeat;
       3: tex.WrapS:=twRepeat;
       4: begin
            case ord(lm.Material.Texture.TextureWrapS) of
              0: tex.WrapS:=twRepeat;
              1: tex.WrapS:=twRepeat;
              2: tex.WrapS:=twClamp;
              3: tex.WrapS:=twClampToEdge;
              4: tex.WrapS:=twClampToBorder;
              5: tex.WrapS:=twMirrorRepeat;
              else tex.WrapS:=twClamp;
            end;
            case ord(lm.Material.Texture.TextureWrapT) of
              0: tex.WrapT:=twRepeat;
              1: tex.WrapT:=twRepeat;
              2: tex.WrapT:=twClamp;
              3: tex.WrapT:=twClampToEdge;
              4: tex.WrapT:=twClampToBorder;
              5: tex.WrapT:=twMirrorRepeat;
              else tex.WrapT:=twClamp;
            end;
          end;
     end;
     with lm.Material.Texture.Image do tex.SetDimensions(Width,Height,Depth);
     ifmt:=lm.Material.Texture.TextureFormatEx;
     tex.SetInternalFormat(ifmt);

     tex.TextureMode:=CTexModes[ord(lm.Material.Texture.TextureMode)];
     tex.FileName:=lm.Material.Texture.Image.ResorceName;
     tex.Name:=ExtractFileName(tex.FileName);

     case lm.Material.FaceCulling of
       fcCull,fcBufferDefault: tex.TwoSides:=false;
       fcNoCull: tex.TwoSides:=true;
     end;
     with tex.TexDesc^ do begin
       Data:=nil; Id:=lm.Material.Texture.Handle;
       UsePBO:=false; Created:=true;
     end;
     tex.ResourceHandler:=lm;
  end;
  result:=tex;
end;

function TGLSceneAdapter.ImportMaterial(lm: TGLLibMaterial): TMaterialObject;
const cAlphaFunct: array [0..7] of cardinal =
        (GL_NEVER, GL_ALWAYS, GL_LESS, GL_LEQUAL, GL_EQUAL, GL_GREATER,
         GL_NOTEQUAL, GL_GEQUAL);
      cBlendFunction: array [0..14] of cardinal = (GL_ZERO, GL_ONE,
        GL_SRC_COLOR, GL_ONE_MINUS_SRC_COLOR, GL_DST_COLOR, GL_ONE_MINUS_DST_COLOR,
        GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA, GL_DST_ALPHA, GL_ONE_MINUS_DST_ALPHA,
        GL_CONSTANT_COLOR, GL_ONE_MINUS_CONSTANT_COLOR, GL_CONSTANT_ALPHA,
        GL_ONE_MINUS_CONSTANT_ALPHA, GL_SRC_ALPHA_SATURATE);
const CTexModes: array[0..4] of TTextureCombines =
      (tcDecal, tcModulate, tcBlend, tcReplace, tcAdd);
var matObj: TMaterialObject;
    mat: TMaterial;
    tex: TTexture;
    MatName: string;
    ifmt: TGLInternalFormat;
begin
  result:=nil; if not Assigned(lm) then exit;
  matObj:=TMaterialObject.Create;
  matObj.Name:=lm.Name; MatName:=lm.Name;
   case ord(lm.Material.BlendingMode) of
     0: MatObj.Blending.SetByMode(bmOpaque);
     1: MatObj.Blending.SetByMode(bmTransparency);
     2: MatObj.Blending.SetByMode(bmAdditive);
     3: MatObj.Blending.SetByMode(bmAlphaTest50);
     4: MatObj.Blending.SetByMode(bmAlphaTest100);
     5: MatObj.Blending.SetByMode(bmModulate);
     6: begin
        with MatObj.Blending, lm.Material do begin
          AlphaTestEnable:=BlendingParams.UseAlphaFunc;
          AlphaFunc:=cAlphaFunct[ord(BlendingParams.AlphaFunctType)];
          AlphaThreshold:=BlendingParams.AlphaFuncRef;

          BlendEnable:=BlendingParams.UseBlendFunc;
          SrcBlendFunc:=cBlendFunction[ord(BlendingParams.BlendFuncSFactor)];
          DstBlendFunc:=cBlendFunction[ord(BlendingParams.BlendFuncDFactor)];
        end;
     end;
  end;
  mat:=TMaterial.Create; mat.Name:=MatName+inttostr(integer(lm));
  with mat.Properties,lm.Material.FrontProperties do begin
    AmbientColor.ColorVector:=Ambient.Color;
    DiffuseColor.ColorVector:=Diffuse.Color;
    SpecularColor.ColorVector:=Specular.Color;
    EmissionColor.ColorVector:=Emission.Color;
    Shininess:=lm.Material.FrontProperties.Shininess;
  end;

  if (lm.Material.Texture.TextureImageRequiredMemory<=0)
  or (lm.Material.Texture.Disabled) then tex:=nil else begin
    tex:=TTexture.Create; tex.Name:=MatName+inttostr(integer(lm));
    tex.SetTarget(ttTexture2D);
     case ord(lm.Material.Texture.MagFilter) of
       0: tex.magFilter:=mgNearest;
       1: tex.magFilter:=mgLinear;
     end;

     case ord(lm.Material.Texture.MinFilter) of
       0: tex.minFilter:=mnNearest;
       1: tex.minFilter:=mnLinear;
       2: tex.minFilter:=mnNearestMipmapNearest;
       3: tex.minFilter:=mnLinearMipmapNearest;
       4: begin tex.minFilter:=mnNearestMipmapLinear; tex.Mipmapping:=true; end;
       5: begin tex.minFilter:=mnLinearMipmapLinear; tex.Mipmapping:=true; end;
     end;

     case ord(lm.Material.Texture.TextureWrap) of
       0: begin tex.WrapS:=twRepeat; tex.WrapT:=twRepeat; end;
       1: begin tex.WrapS:=twClamp; tex.WrapT:=twClamp; end;
       2: tex.WrapT:=twRepeat;
       3: tex.WrapS:=twRepeat;
       4: begin
            case ord(lm.Material.Texture.TextureWrapS) of
              0: tex.WrapS:=twRepeat;
              1: tex.WrapS:=twRepeat;
              2: tex.WrapS:=twClamp;
              3: tex.WrapS:=twClampToEdge;
              4: tex.WrapS:=twClampToBorder;
              5: tex.WrapS:=twMirrorRepeat;
              else tex.WrapS:=twClamp;
            end;
            case ord(lm.Material.Texture.TextureWrapT) of
              0: tex.WrapT:=twRepeat;
              1: tex.WrapT:=twRepeat;
              2: tex.WrapT:=twClamp;
              3: tex.WrapT:=twClampToEdge;
              4: tex.WrapT:=twClampToBorder;
              5: tex.WrapT:=twMirrorRepeat;
              else tex.WrapT:=twClamp;
            end;
          end;
     end;
     with lm.Material.Texture.Image do tex.SetDimensions(Width,Height,Depth);
     ifmt:=lm.Material.Texture.TextureFormatEx;
     tex.SetInternalFormat(ifmt);

     tex.TextureMode:=CTexModes[ord(lm.Material.Texture.TextureMode)];
     tex.FileName:=lm.Material.Texture.Image.ResorceName;
     tex.Name:=ExtractFileName(tex.FileName);

     case lm.Material.FaceCulling of
       fcCull,fcBufferDefault: tex.TwoSides:=false;
       fcNoCull: tex.TwoSides:=true;
     end;
     with tex.TexDesc^ do begin
       Data:=nil; Id:=lm.Material.Texture.Handle;
       UsePBO:=false; Created:=true;
     end;
     tex.ResourceHandler:=lm;
  end;

  MatObj.AttachMaterial(mat);
  MatObj.AttachTexture(tex);
  result:=matObj;
  if not assigned(FVBOMesh) then exit;
  FVBOMesh.Textures.Add(tex);
  FVBOMesh.Materials.Add(mat);
end;

end.
