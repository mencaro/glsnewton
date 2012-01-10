unit MTLLoader;
interface

Uses Classes, SysUtilsLite, VectorGeometry,
     uMaterials, uTextures, uMaterialObjects;
Type
   TMTLProgressEvent = procedure (CurrentMatName:string; CurrentMatIndex:integer; MatCount:integer);
   TGLMTLFile = class (TStringList)
      private
         FMaterialList: TStringList;
         FPath, FFilePath: string;
         procedure Prepare;
         procedure LoadMaterials(MatObjLib: TMaterialObjectsLib; const matName : String);overload;
         procedure LoadTexture(const FileName: string; map: TMapTarget;
                               TexLib: TTextureLibrary; var Tex: TTexture);
         function MaterialStringProperty(const materialName, propertyName : String) : String;
         function MaterialVectorProperty(const materialName, propertyName : String;
                                         const defaultValue : TVector) : TVector;
      public
         onProgress: TMTLProgressEvent;
         constructor Create;
         destructor Destroy; override;
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
    buf, t : string;
begin
   // "standardize" the mtl file lines
   for i:=Count-1 downto 0 do begin
     buf:=(Trim(Strings[i]));
     if (buf='') or (ansichar(buf[1]) in ['#', '$']) then Delete(i)
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
    buff, line : String;
begin
   buff:='NEWMTL '+UpperCase(materialName);
   i:=IndexOf(buff);
   if i>=0 then begin
      buff:=UpperCase(propertyName)+' ';
      n:=Length(buff); Inc(i);
      while i<Count do begin
         line:=Strings[i];
         if Copy(line, 1, 7)='NEWMTL ' then Break;
         if Copy(line, 1, n)=buff then begin
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
  MatObj.AttachMaterial(mat);

  // setup texture
  texName:=MaterialStringProperty(matName, 'map_Kd'); tex:=nil;
  if texName<>'' then LoadTexture(TexName,mtDiffuse,MatObjLib.TexLib,tex);
  if assigned(tex) then begin
    if Mat.Properties.DiffuseColor.Alpha<1 then
      tex.BlendingMode:=uTextures.tbmTransparency;
    tex.TextureMode:=tcModulate;
    MatObj.AttachTexture(tex);
  end;

  texName:=MaterialStringProperty(matName, 'map_bump'); tex:=nil;
  if texName<>'' then LoadTexture(TexName,mtNormalMap,MatObjLib.TexLib,tex);
  if assigned(tex) then MatObj.AddExTextures(tex);

  texName:=MaterialStringProperty(matName, 'bump '); tex:=nil;
  if texName<>'' then LoadTexture(TexName,mtBumpMap,MatObjLib.TexLib,tex);
  if assigned(tex) then MatObj.AddExTextures(tex);

  texName:=MaterialStringProperty(matName, 'map_opacity'); tex:=nil;
  if texName<>'' then LoadTexture(TexName,mtOpacity,MatObjLib.TexLib,tex);
  if assigned(tex) then MatObj.AddExTextures(tex);

  texName:=MaterialStringProperty(matName, 'map_d'); tex:=nil;
  if texName<>'' then LoadTexture(TexName,mtAlpha,MatObjLib.TexLib,tex);
  if assigned(tex) then MatObj.AddExTextures(tex);

  texName:=MaterialStringProperty(matName, 'map_kS'); tex:=nil;
  if texName<>'' then LoadTexture(TexName,mtSpecular,MatObjLib.TexLib,tex);
  if assigned(tex) then MatObj.AddExTextures(tex);

  texName:=MaterialStringProperty(matName, 'map_kA'); tex:=nil;
  if texName<>'' then LoadTexture(TexName,mtAmbient,MatObjLib.TexLib,tex);
  if assigned(tex) then MatObj.AddExTextures(tex);

  texName:=MaterialStringProperty(matName, 'map_Ns'); tex:=nil;
  if texName<>'' then LoadTexture(TexName,mtShininess,MatObjLib.TexLib,tex);
  if assigned(tex) then MatObj.AddExTextures(tex);

end;

procedure TGLMTLFile.LoadTexture(const FileName: string; map: TMapTarget;
  TexLib: TTextureLibrary; var Tex: TTexture);
var TexFileName, TexName: string;
begin
  TexFileName:=''; TexName:=FileName;
  Tex:=TexLib.TextureByName(texName,'');
  if assigned(Tex) then begin
    tex.MapTargets:=tex.MapTargets+[map]; exit;
  end;

  StringReplace(TexName,'/','\',[rfReplaceAll]);
  StringReplace(TexName,'\\','\',[rfReplaceAll]);
  if FileExists(FPath+texName) then TexFileName:=FPath+texName;
  if FileExists(FFilePath+texName) then TexFileName:=FFilePath+texName;
  assert(TexFileName<>'','Texture '+texName+' not found.');
  try
    tex:=TTexture.CreateFromFile(TexFileName,ttTexture2D);
    tex.Name:=texName; TexLib.Add(tex);
    tex.Disabled:=false;
    tex.MapTargets:=[map];
  except
  end;
end;

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

end.
