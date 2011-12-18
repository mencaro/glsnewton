unit MTLLoader;
interface

Uses Classes, SysUtilsLite, VectorGeometry,
     {GLUtils, ApplicationFileIO, GLMaterial,
     GLColor, GLTexture, Jpeg, TGA, DDS, PNGimage,}
     uMaterials, uTextures, uMaterialObjects;
Type
   TMTLProgressEvent = procedure (CurrentMatName:string; CurrentMatIndex:integer; MatCount:integer);
   TGLMTLFile = class (TStringList)
      private
         FMaterialList: TStringList;
         FPath, FFilePath: string;
         procedure Prepare;
//         procedure LoadMaterials(matLib: TGLMaterialLibrary; const matName : String);overload;
         procedure LoadMaterials(MatObjLib: TMaterialObjectsLib; const matName : String);overload;
         function MaterialStringProperty(const materialName, propertyName : String) : String;
         function MaterialVectorProperty(const materialName, propertyName : String;
                                         const defaultValue : TVector) : TVector;
      public
         onProgress: TMTLProgressEvent;
         constructor Create;
         destructor Destroy; override;
//         procedure LoadMaterialLibrary(MTLFileName: string;
//           MaterialLibrary: TGLMaterialLibrary); overload;
         procedure LoadMaterialLibrary(MTLFileName: string;
           MatLib: TMaterialObjectsLib); overload;
         property Path: string read FPath write FPath;
   end;



implementation

//Prepare
//
procedure TGLMTLFile.Prepare;
var
    i : Integer;
    buf, t : String;
begin
   // "standardize" the mtl file lines
   for i:=Count-1 downto 0 do begin
      buf:=(Trim(Strings[i]));
      if (buf='') or (buf[1] in ['#', '$']) then
         Delete(i)
      else begin
         buf:=StringReplace(buf, #9, #32, [rfIgnoreCase]);
         if pos('NEWMTL ',uppercase(buf))>0 then begin
            t:=copy(buf,8,length(buf)-7);
            Strings[i]:='NEWMTL '+t;
         end else Strings[i]:=UpperCase(buf);
      end;
   end;
end;

// MaterialStringProperty
//
function TGLMTLFile.MaterialStringProperty(const materialName, propertyName : String) : String;
var i, n : Integer;
    buf, line : String;
begin
   buf:='NEWMTL '+UpperCase(materialName);
   i:=IndexOf(buf);
   if i>=0 then begin
      buf:=UpperCase(propertyName)+' ';
      n:=Length(buf); Inc(i);
      while i<Count do begin
         line:=Strings[i];
         if Copy(line, 1, 7)='NEWMTL ' then Break;
         if Copy(line, 1, n)=buf then begin
            Result:=Copy(Strings[i], n+1, MaxInt);
            Exit;
         end;
         Inc(i);
      end;
   end;
   Result:='';
end;

// MaterialVectorProperty
//
function TGLMTLFile.MaterialVectorProperty(const materialName, propertyName : String;
                                           const defaultValue : TVector) : TVector;
var  i : Integer;
    sl : TStringList;
begin
   sl:=TStringList.Create;
   try
      sl.CommaText:=MaterialStringProperty(materialName, propertyName);
      if sl.Count>0 then begin
         Result:=NullHmgVector;
         for i:=0 to 3 do
            if sl.Count>i then
               Result[i]:=StrToFloatDef(sl[i], 0)
            else Break;
      end else Result:=defaultValue;
   finally
      sl.Free;
   end;
end;

procedure TGLMTLFile.LoadMaterialLibrary(MTLFileName: string;
  MatLib: TMaterialObjectsLib);
var fs : TStream;
    i:integer;
    s:string;
begin
   FFilePath:=ExtractFilePath(MTLFileName);
   if FPath<>'' then
      if FPath[length(FPath)]<>'\' then FPath:=FPath+'\';
   if FFilePath<>'' then
      if FFilePath[length(FFilePath)]<>'\' then FFilePath:=FFilePath+'\';

   if not fileexists(MTLFileName) then MTLFileName:=Fpath+MTLFileName;
   try fs:=TFileStream.Create(MTLFileName,fmOpenRead);
//   CreateFileStream(MTLFileName);
   except fs:=nil; end;
   if assigned(FMaterialList) then FMaterialList.Clear
   else FMaterialList:=TStringList.Create;
   if Assigned(fs) then begin
     try
       LoadFromStream(fs);Prepare;
       for i:=0 to Count-1 do begin
         s:=Strings[i];
         if pos('NEWMTL',s)>0 then begin
           System.delete(s,1,7);FMaterialList.add(s);
         end;
       end;
       for i:=0 to FMaterialList.Count-1 do begin
           if assigned(onProgress) then
              onProgress(FMaterialList[i],i+1,FMaterialList.Count);
           LoadMaterials(MatLib, FMaterialList[i]);
       end;
     finally
       fs.Free;
     end;
   end;
end;

procedure TGLMTLFile.LoadMaterials(MatObjLib: TMaterialObjectsLib; const matName: String);
var MatObj: TMaterialObject;
    mat: TMaterial;
    tex: TTexture;
    texName: String;
    TexFileName: string;
    sh: byte;
begin
  Assert(Assigned(MatObjLib),'MaterialLibrary is not assigned');
  MatObj:=MatObjLib.MaterialByName(matName);
  if assigned(MatObj) then exit;
  MatObj:=MatObjLib.AddNewMaterialObject(matName);

  mat:=MatObjLib.MatLib.MaterialByName(matName);
  if not assigned(mat) then mat:=MatObjLib.MatLib.AddNewMaterial(matName);
  // setup material colors
  with Mat.Properties do begin
     AmbientColor.ColorVector:=MaterialVectorProperty(matName, 'Ka', cAmbientColor);
     DiffuseColor.ColorVector:=MaterialVectorProperty(matName, 'Kd', cDiffuseColor);
     DiffuseColor.Alpha:=StrToFloatDef(MaterialStringProperty(matName, 'd'), 1);
     case StrToIntDef(MaterialStringProperty(matName, 'illum'), 1) of
       0 : Mat.UseMaterial:=false;
       1 : ; // flat, non-shiny material
       2 : SpecularColor.ColorVector:=MaterialVectorProperty(matName, 'Ks', cSpecularColor);
     else
       SpecularColor.ColorVector:=cSpecularColor;
     end;
     sh:=StrToIntDef(MaterialStringProperty(matName, 'Ns'), 1);
     if sh>127 then Shininess:=128 else Shininess:=sh;
  end;

  // setup texture
  texName:=MaterialStringProperty(matName, 'map_Kd'); tex:=nil;
  if texName<>'' then begin
    Tex:=MatObjLib.TexLib.TextureByName(texName,'');
    if not assigned(Tex) then begin
      TexFileName:='';
      if FileExists(FPath+texName) then TexFileName:=FPath+texName;
      if FileExists(FFilePath+texName) then TexFileName:=FFilePath+texName;
      assert(TexFileName<>'','Texture '+texName+' not found.');
      try
        tex:=TTexture.CreateFromFile(TexFileName,ttTexture2D);
        tex.Name:=texName; MatObjLib.TexLib.Add(tex);
        if Mat.Properties.DiffuseColor.Alpha<1 then
          tex.BlendingMode:=uTextures.tbmTransparency;
        tex.TextureMode:=tcModulate;
        tex.Disabled:=false;
      except
      end;
    end;
  end;
  MatObj.AttachTexture(tex); MatObj.AttachMaterial(mat);
end;
{
Procedure TGLMTLFile.LoadMaterials(matLib : TGLMaterialLibrary;const matName : String);
var libMat: TGLLibMaterial;
    texName: String;
    TexFileName: string;
    sh: byte;
begin
  Assert(Assigned(matLib),'MaterialLibrary is not assigned');
  libMat:=matLib.Materials.GetLibMaterialByName(matName);
  if not Assigned(libMat) then begin
      // spawn a new material
      libMat:=matLib.Materials.Add;
      libMat.Name:=matName;
      // setup material colors
      with libMat.Material.FrontProperties do begin
         Ambient.Color:=MaterialVectorProperty(matName, 'Ka', clrGray20);
         Diffuse.Color:=MaterialVectorProperty(matName, 'Kd', clrGray80);
         Diffuse.Alpha:=GLUtils.StrToFloatDef(MaterialStringProperty(matName, 'd'), 1);
         if Diffuse.Alpha<1 then
            libMat.Material.BlendingMode:=GLMaterial.bmTransparency;
         case StrToIntDef(MaterialStringProperty(matName, 'illum'), 1) of
            0 : begin // non-lit material
               libMat.Material.MaterialOptions:=[moNoLighting];
            end;
            1 : ; // flat, non-shiny material
            2 : begin // specular material
               Specular.Color:=MaterialVectorProperty(matName, 'Ks', clrTransparent);
            end;
         else
            // unknown, assume unlit
            libMat.Material.MaterialOptions:=[moNoLighting];
            Diffuse.Color:=clrGray80;
         end;
         sh:=StrToIntDef(MaterialStringProperty(matName, 'Ns'), 1);
         if sh>127 then Shininess:=128 else Shininess:=sh;
      end;
      // setup texture
      texName:=MaterialStringProperty(matName, 'map_Kd');
      if texName<>'' then begin
        TexFileName:='';
        if FileExists(FPath+texName) then TexFileName:=FPath+texName;
        if FileExists(FFilePath+texName) then TexFileName:=FFilePath+texName;
        assert(TexFileName<>'','Texture '+texName+' not found.');
        try
          with libMat.Material.Texture do begin
             Image.LoadFromFile(TexFileName);
             Disabled:=False;
             TextureMode:=tmModulate;
          end;
        except
        end;
      end;
   end;
end;
}
constructor TGLMTLFile.Create;
begin
  inherited;
  FMaterialList:=TStringList.Create;
  FFilePath:='';
end;

destructor TGLMTLFile.Destroy;
begin
  FMaterialList.Clear;
  FMaterialList.Free;
  inherited;
end;

{
procedure TGLMTLFile.LoadMaterialLibrary(MTLFileName: string; MaterialLibrary:TGLMaterialLibrary);
var fs : TStream;
    i:integer;
    s:string;
begin
   FFilePath:=ExtractFilePath(MTLFileName);
   if FPath<>'' then
      if FPath[length(FPath)]<>'\' then FPath:=FPath+'\';
   if FFilePath<>'' then
      if FFilePath[length(FFilePath)]<>'\' then FFilePath:=FFilePath+'\';

   if not fileexists(MTLFileName) then MTLFileName:=Fpath+MTLFileName;
   try fs:=CreateFileStream(MTLFileName);
   except fs:=nil; end;
   if assigned(FMaterialList) then FMaterialList.Clear
   else FMaterialList:=TStringList.Create;
   if Assigned(fs) then begin
     try
       LoadFromStream(fs);Prepare;
       for i:=0 to Count-1 do begin
         s:=Strings[i];
         if pos('NEWMTL',s)>0 then begin
           System.delete(s,1,7);FMaterialList.add(s);
         end;
       end;
       for i:=0 to FMaterialList.Count-1 do begin
           if assigned(onProgress) then
              onProgress(FMaterialList[i],i+1,FMaterialList.Count);
           LoadMaterials(MaterialLibrary, FMaterialList[i]);
       end;
     finally
       fs.Free;
     end;
   end;
   //self.Free;
end;
 }
end.
