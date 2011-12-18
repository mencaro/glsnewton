{: uShaders
	Historique:
        24/06/11 - Fantom - Добавлена компиляция геометрического шейдерного объекта
    13/05/11 - Fantom - Добавлена функция LoadShader
    15/07/10 - Fantom - Добавлена функция CreateShader
        07/04/10 - Fantom - Добавлено кеширование юниформ
}
unit uShaders;

interface

Uses Classes, Contnrs, VectorLists, OpenGL1x, VectorTypes, uVBO,
     uMiscUtils, OGLStateEmul;

Type
   THashParam = record
      HashKey: integer;
      Name: AnsiString;
      GroupId: integer;
      Value: integer;
   end;
   PHashParam = ^THashParam;
   THashList = class (TObject)
      private
        FHashList: TList;
        function FGetParamByHashKey(HashKey: integer; Group:integer=-1): integer;
      public
        constructor Create;
        destructor Destroy;override;

        function ComputeNameHashKey(const name: AnsiString): Integer;
        procedure AddParam(const name: AnsiString; value: integer; GroupId: integer=-1);
        function GetParam(const name: AnsiString; GroupId: integer=-1): integer;
   end;

   TResultType = (asIndex, asHandle);
   TShaders = Class
     ShaderObjectsList: TIntegerList;
     ShaderPrograms: TIntegerList;
     Private
       FOwner: TObject;
       FProgramsFriendlyName: TStringList;
       FObjectsFriendlyName: TStringList;
       FUniformsHash: THashList;
       FActiveProgram: integer;
       FVersion: integer;
       function GetVersion: integer;
     Public
       Logs: AnsiString;
       UniformsWarnings: AnsiString;

       property ActiveProgram: integer read FActiveProgram;
       property Version: integer read GetVersion;
       property Owner: TObject read FOwner write FOwner;
       
       Function CreateShader(const VS,FS: AnsiString; res: TResultType = asHandle): GLUint;overload;
       Function CreateShader(const VS,FS: AnsiString; Defines: AnsiString; res: TResultType = asHandle): GLUint;overload;
       Function CreateGSShader(const VS,FS,GS: AnsiString; res: TResultType = asHandle): GLUint;
       Function LoadShader(const VSFileName,FSFileName: string; const GSFileName: string = ''; res: TResultType = asHandle): GLUint;

       Function AddShaderObject(code: ansistring;target: GLUInt; res: TResultType = asIndex):integer;overload;
       Function AddShaderObject(code: ansistring;target: GLUInt; Name:AnsiString; res: TResultType = asIndex):integer;overload;
       Function CreateShaderProgram(res: TResultType = asHandle):integer;overload;
       Function CreateShaderProgram(Name:AnsiString; res: TResultType = asHandle):integer;overload;
       Function GetProgramObjectByName(Name:AnsiString; res: TResultType = asHandle):integer;
       Function GetShaderObjectByName(Name:AnsiString; res: TResultType = asHandle):integer;

       Function GetAttribLocation(ProgramId:GLUInt; const name : AnsiString) : Integer;
       Function GetUniformLocation(ProgramId:GLUInt; const name : AnsiString) : Integer;

       Procedure GetActiveUniforms(ProgramId:GLUint; Uniforms: TSTringList);

       Procedure BindAttribLocation(ProgramId:GLUInt; index : Integer; const name : AnsiString);
       Procedure AttachShaderObjectToProgram(objId:GLUInt;ProgramId:GLUInt);
       Procedure AttachAllObjToProgram(ProgramId:GLUInt);
       Procedure AttachShaderToProgram(ObjectName,ProgramName:AnsiString);
       Procedure LinkShaderProgram(ProgramId:GLUInt);overload;
       Procedure LinkShaderProgram(ProgramName:AnsiString);overload;
       Procedure UseProgramIndex(index:integer);
       Procedure UseProgramObject(id:integer);
       Procedure UseProgram(Name:AnsiString);
       Procedure EndProgram;

       Procedure DeleteShaderObject(id:integer);
       Procedure DeleteShaderProgram(ProgramId:integer);

       Procedure ClearShaderObject;
       Procedure ClearShaderPrograms;

       Constructor Create;
       Destructor Destroy;override;

       Procedure SetUniforms(ProgramId:GLUInt; const name : AnsiString; const value: single;    count: GLsizei=1);overload;
       Procedure SetUniforms(ProgramId:GLUInt; const name : AnsiString; const value: TVector2f; count: GLsizei=1);overload;
       Procedure SetUniforms(ProgramId:GLUInt; const name : AnsiString; const value: TVector3f; count: GLsizei=1);overload;
       Procedure SetUniforms(ProgramId:GLUInt; const name : AnsiString; const value: TVector4f; count: GLsizei=1);overload;
       Procedure SetUniforms(ProgramId:GLUInt; const name : AnsiString; const value: integer;   count: GLsizei=1);overload;
       Procedure SetUniforms(ProgramId:GLUInt; const name : AnsiString; const value: TVector2i; count: GLsizei=1);overload;
       Procedure SetUniforms(ProgramId:GLUInt; const name : AnsiString; const value: TVector3i; count: GLsizei=1);overload;
       Procedure SetUniforms(ProgramId:GLUInt; const name : AnsiString; const value: TVector4i; count: GLsizei=1);overload;
       Procedure SetUniforms(ProgramId:GLUInt; const name : AnsiString; const value: TMatrix2f; count: GLsizei=1; transpose: boolean=false);overload;
       Procedure SetUniforms(ProgramId:GLUInt; const name : AnsiString; const value: TMatrix3f; count: GLsizei=1; transpose: boolean=false);overload;
       Procedure SetUniforms(ProgramId:GLUInt; const name : AnsiString; const value: TMatrix4f; count: GLsizei=1; transpose: boolean=false);overload;

       Procedure SetUniforms(ProgramName:AnsiString; const name : AnsiString; const value: single;    count: GLsizei=1);overload;
       Procedure SetUniforms(ProgramName:AnsiString; const name : AnsiString; const value: TVector2f; count: GLsizei=1);overload;
       Procedure SetUniforms(ProgramName:AnsiString; const name : AnsiString; const value: TVector3f; count: GLsizei=1);overload;
       Procedure SetUniforms(ProgramName:AnsiString; const name : AnsiString; const value: TVector4f; count: GLsizei=1);overload;
       Procedure SetUniforms(ProgramName:AnsiString; const name : AnsiString; const value: integer;   count: GLsizei=1);overload;
       Procedure SetUniforms(ProgramName:AnsiString; const name : AnsiString; const value: TVector2i; count: GLsizei=1);overload;
       Procedure SetUniforms(ProgramName:AnsiString; const name : AnsiString; const value: TVector3i; count: GLsizei=1);overload;
       Procedure SetUniforms(ProgramName:AnsiString; const name : AnsiString; const value: TVector4i; count: GLsizei=1);overload;
       Procedure SetUniforms(ProgramName:AnsiString; const name : AnsiString; const value: TMatrix2f; count: GLsizei=1; transpose: boolean=false);overload;
       Procedure SetUniforms(ProgramName:AnsiString; const name : AnsiString; const value: TMatrix3f; count: GLsizei=1; transpose: boolean=false);overload;
       Procedure SetUniforms(ProgramName:AnsiString; const name : AnsiString; const value: TMatrix4f; count: GLsizei=1; transpose: boolean=false);overload;

       Procedure SetProgParam(ProgramId: GLUInt; Param: GLUint; Value: GLUInt);
       Procedure SetGeomInputType(ProgramId: GLUInt; Value: GLUInt);
       Procedure SetGeomOutputType(ProgramId: GLUInt; Value: GLUInt);
       Procedure SetGeomVerticesOutCount(ProgramId: GLUInt; Value: GLUInt=0);

   end;

   TShaderEvents = procedure (ShaderProgram: TObject) of object;

   TShaderProgram = class
   private
     FOwner: TObject;
     FShaders: TShaders;
     FFragmentText: TStringList;
     FVertexText: TStringList;
     FGeometryText: TStringList;
     FFragObjId: cardinal;
     FVertObjId: cardinal;
     FGeomObjId: cardinal;
     FProgramId: cardinal;
     FName: string;
     FUniforms: TStringList;

     FonApplyShader: TShaderEvents;
     FonUnApplyShader: TShaderEvents;
     function getUniforms: TStringList;
   public
     constructor Create(ShadersCollection: TShaders=nil);overload;
     constructor Create(aVertex,aFragment: string; aGeometry: string='');overload;
     destructor Destroy; override;

     procedure Apply;
     procedure UnApply;

     function CreateFromFile(aVertex,aFragment: string; aGeometry: string=''): cardinal;
     function BuildProgram(aName: string=''): cardinal;
     function GetAttribLocation(const aName: Ansistring): integer; overload;
     function GetAttribLocation(const aName: Ansistring; var buff: TVBOBuffer): integer; overload;

     function GetUniformLocation(const aName: AnsiString): cardinal;
     procedure BindAttribLocation(Index : Integer; const aName : AnsiString);

     procedure SetUniforms( const name : AnsiString; const value: single;    count: GLsizei=1);overload;
     procedure SetUniforms( const name : AnsiString; const value: TVector2f; count: GLsizei=1);overload;
     procedure SetUniforms( const name : AnsiString; const value: TVector3f; count: GLsizei=1);overload;
     procedure SetUniforms( const name : AnsiString; const value: TVector4f; count: GLsizei=1);overload;
     procedure SetUniforms( const name : AnsiString; const value: integer;   count: GLsizei=1);overload;
     procedure SetUniforms( const name : AnsiString; const value: TVector2i; count: GLsizei=1);overload;
     procedure SetUniforms( const name : AnsiString; const value: TVector3i; count: GLsizei=1);overload;
     procedure SetUniforms( const name : AnsiString; const value: TVector4i; count: GLsizei=1);overload;
     procedure SetUniforms( const name : AnsiString; const value: TMatrix2f; count: GLsizei=1; transpose: boolean=false);overload;
     procedure SetUniforms( const name : AnsiString; const value: TMatrix3f; count: GLsizei=1; transpose: boolean=false);overload;
     procedure SetUniforms( const name : AnsiString; const value: TMatrix4f; count: GLsizei=1; transpose: boolean=false);overload;

     procedure SetGeomInputType(ProgramId, Value: GLUInt);
     procedure SetGeomOutputType(ProgramId, Value: GLUInt);
     procedure SetGeomVerticesOutCount(ProgramId, Value: GLUInt);
     procedure SetProgParam(ProgramId, Param, Value: GLUInt);

     property Name: string read FName write FName;
     property onApplyShader: TShaderEvents read FonApplyShader write FonApplyShader;
     property onUnApplyShader: TShaderEvents read FonUnApplyShader write FonUnApplyShader;

     property ProgramId: cardinal read FProgramId;
     property FragmentText: TStringList read FFragmentText;
     property VertexText: TStringList read FVertexText;
     property GeometryText: TStringList read FGeometryText;
     property Uniforms: TStringList read getUniforms;
     property Shaders: TShaders read FShaders;
   end;

   TShaderLibrary = class (TObjectList)
   private
     FHashList: TIntegerList;
     FShaderCollection: TShaders;
     function Get(Index: Integer): TShaderProgram;
     procedure Put(Index: Integer; Item: TShaderProgram);
   public
     property Items[Index: Integer]: TShaderProgram read Get write Put; default;
     property ShadersCollection: TShaders read FShaderCollection write FShaderCollection;
     function AddNewShader(aName:string=''): TShaderProgram;
     function Add(ShaderProg: TShaderProgram): integer;
     function ShaderByName(aName: string): TShaderProgram;
     procedure Clear; override;
     procedure Delete(Index: Integer);
     procedure Insert(Index: Integer; ShaderProg: TShaderProgram);
     procedure Exchange(Index1, Index2: Integer);

     constructor Create;
     destructor Destroy; override;
   end;


implementation

function IntToStr(x:integer):AnsiString;
begin str(x,result); end;
function StrToInt(s:AnsiString):integer;
var code:integer;
begin val(s,result,code);end;

function GetGLSLVersion: integer;
var s: AnsiString;
begin
  s:=glGetString(GL_SHADING_LANGUAGE_VERSION_ARB);
  s:=copy(s,1,4); delete(s,2,1);
  result:=StrToInt(s);
end;

{ TShaders }

Function TShaders.AddShaderObject(code: ansistring; target: GLUInt; res: TResultType):integer;
var p : PGLChar;
    VertexObject, FragmentObject, GeometryObject: integer;
    val,len:integer;
    pLog:PAnsiChar;
begin
   p:=PGLChar(code); Result := -1;
   if not assigned(ShaderObjectsList) then
      ShaderObjectsList:=TIntegerList.Create;
   case target of
      GL_VERTEX_SHADER: begin
           VertexObject := glCreateShader(GL_VERTEX_SHADER);
           if res = asIndex then
              Result := ShaderObjectsList.Add(VertexObject)
           else begin
             Result := VertexObject;
             ShaderObjectsList.Add(VertexObject);
           end;
           glShaderSource(VertexObject, 1, @p, nil);
           glCompileShader(VertexObject);
           glGetShaderiv(VertexObject, GL_COMPILE_STATUS, @val);
           Logs:=Logs+'VERTEX SHADER Object '+inttostr(Result)+' Compilation:';
           if val=GL_FALSE then Logs:=Logs+' Failed!'+#13#10
           else Logs:=Logs+' Successful'+#13#10;
           glGetShaderiv(VertexObject, GL_INFO_LOG_LENGTH, @val);
           getmem(pLog,val);
           glGetShaderInfoLog(VertexObject, val, @len, pLog);
           if length(pLog)>0 then Logs:=Logs+pLog+#13#10;
           FreeMem(pLog,val);
      end;
      GL_FRAGMENT_SHADER: begin
           FragmentObject := glCreateShader(GL_FRAGMENT_SHADER);
           if res = asIndex then
              Result := ShaderObjectsList.Add(FragmentObject)
           else begin
             Result := FragmentObject;
             ShaderObjectsList.Add(FragmentObject);
           end;
           glShaderSource(FragmentObject, 1, @p, nil);
           glCompileShader(FragmentObject);
           glGetShaderiv(FragmentObject, GL_COMPILE_STATUS, @val);
           Logs:=Logs+'FRAGMENT SHADER Object '+inttostr(Result)+' Compilation:';
           if val=GL_FALSE then Logs:=Logs+' Failed!'+#13#10
           else Logs:=Logs+' Successful'+#13#10;
           glGetShaderiv(FragmentObject, GL_INFO_LOG_LENGTH, @val);
           getmem(pLog,val);
           glGetShaderInfoLog(FragmentObject, val, @len, pLog);
           if length(pLog)>0 then Logs:=Logs+pLog+#13#10;
           FreeMem(pLog,val);
      end;
      GL_GEOMETRY_SHADER: begin
           GeometryObject := glCreateShader(GL_GEOMETRY_SHADER);
           if res = asIndex then
              Result := ShaderObjectsList.Add(GeometryObject)
           else begin
             Result := GeometryObject;
             ShaderObjectsList.Add(GeometryObject);
           end;
           glShaderSource(GeometryObject, 1, @p, nil);
           glCompileShader(GeometryObject);
           glGetShaderiv(GeometryObject, GL_COMPILE_STATUS, @val);
           Logs:=Logs+'GEOMETRY SHADER Object '+inttostr(Result)+' Compilation:';
           if val=GL_FALSE then Logs:=Logs+' Failed!'+#13#10
           else Logs:=Logs+' Successful'+#13#10;
           glGetShaderiv(GeometryObject, GL_INFO_LOG_LENGTH, @val);
           getmem(pLog,val);
           glGetShaderInfoLog(GeometryObject, val, @len, pLog);
           if length(pLog)>0 then Logs:=Logs+pLog+#13#10;
           FreeMem(pLog,val);
      end;

      ELSE Logs:=Logs+'Error: Unknown shader target: '+IntToStr(target)+#13#10;
   end;
end;

Function TShaders.CreateShaderProgram(res: TResultType):integer;
var SPId:integer;
begin
  if not assigned(ShaderPrograms) then
     ShaderPrograms:=TIntegerList.Create;
  SPId:=glCreateProgram;
  if res = asIndex then Result:= ShaderPrograms.add(SPId)
  else begin Result:=SPId; ShaderPrograms.add(SPId); end;
end;

procedure TShaders.AttachShaderObjectToProgram(objId, ProgramId: GLUInt);
begin
  glAttachShader(ProgramId, ObjId);
end;

procedure TShaders.AttachAllObjToProgram(ProgramId: GLUInt);
var i:integer;
begin
  for i:=0 to ShaderObjectsList.Count-1 do begin
    AttachShaderObjectToProgram(ShaderObjectsList[i],ProgramId);
    Logs:=Logs+'Shader Object '+inttostr(i)+' Attached to Program '+inttostr(ProgramId)+#13#10;
  end;
end;

procedure TShaders.LinkShaderProgram(ProgramId: GLUInt);
var val,len: integer;
    pLog: PAnsiChar;
    Error: boolean;
begin
  Error := False;
  glLinkProgram(ProgramId);
  glGetProgramiv(ProgramId, GL_LINK_STATUS, @val);
  Logs:=Logs+'Linking Program '+inttostr(ProgramId);
  if val=GL_TRUE then
       Logs:=Logs+' Successful'+#13#10
  else begin
    Logs:=Logs+' Failed'+#13#10;
    Error:=true;
  end;
  glGetProgramiv(ProgramId, GL_INFO_LOG_LENGTH, @val);
  getmem(pLog,val);
  glGetProgramInfoLog(ProgramId, val, @len, pLog);
  if length(pLog)>0 then Logs:=Logs+pLog+#13#10;
  FreeMem(pLog,val); Assert(not Error,string(Logs));
end;

procedure TShaders.UseProgramIndex(index: integer);
begin
  UseProgramObject(ShaderPrograms[index]);
end;

procedure TShaders.UseProgramObject(id: integer);
begin
{  if FActiveProgram<>0 then
     assert(true,#13+#10+'Shader program('+inttostr(FActiveProgram)+') allready active.'+
         #13+#10+'Can''t cascade shader.'+#13+#10);
}
  OGLStateEmul.glUseProgram(id);UniformsWarnings:='';
  FActiveProgram:=id;
end;

procedure TShaders.EndProgram;
begin
  OGLStateEmul.glUseProgram(0); FActiveProgram:=0;
end;

procedure TShaders.DeleteShaderObject(Id: integer);
begin
  glDeleteShader(Id);
end;

procedure TShaders.DeleteShaderProgram(ProgramId: integer);
begin
  glDeleteProgram(ProgramId);
end;

destructor TShaders.Destroy;
begin
  ClearShaderPrograms;
  ClearShaderObject;
  ShaderObjectsList.Free;
  ShaderPrograms.Free;
  FProgramsFriendlyName.Free;
  FObjectsFriendlyName.Free;
  FUniformsHash.Free;
  
  Inherited;
end;

procedure TShaders.ClearShaderObject;
var i:integer;
begin
  for i:=0 to ShaderObjectsList.Count-1 do
    DeleteShaderObject(ShaderObjectsList[i]);
end;

procedure TShaders.ClearShaderPrograms;
var i:integer;
begin
  for i:=0 to ShaderPrograms.Count-1 do
      DeleteShaderProgram(ShaderPrograms[i]);
end;

constructor TShaders.Create;
begin
inherited;
  FProgramsFriendlyName := TStringList.Create;
  FObjectsFriendlyName := TStringList.Create;
  ShaderObjectsList:=TIntegerList.Create;
  ShaderPrograms:=TIntegerList.Create;

  FUniformsHash:= THashList.Create;
  FVersion:=-1; GetVersion;
  FActiveProgram:=0;
  FOwner:=nil;
end;

function TShaders.CreateGSShader(const VS, FS, GS: AnsiString;
  res: TResultType): GLUint;
var vsId, fsId, gsId, spId: GLUint;
begin
  vsId := AddShaderObject(VS,$8B31);
  fsId := AddShaderObject(FS,$8B30);
  gsId := AddShaderObject(GS,$8DD9);

  vsId := ShaderObjectsList[vsId];
  fsId := ShaderObjectsList[fsId];
  gsId := ShaderObjectsList[gsId];

  spId:=CreateShaderProgram(res);
  AttachShaderObjectToProgram(vsId,spId);
  AttachShaderObjectToProgram(fsId,spId);
  AttachShaderObjectToProgram(gsId,spId);
  LinkShaderProgram(spId);
  result:=spId;
end;

Function TShaders.GetAttribLocation(ProgramId:GLUInt; const name : AnsiString) : Integer;
begin
   Result:=glGetAttribLocation(ProgramId, PGLChar(name));
   if Result<0 then
      UniformsWarnings:=UniformsWarnings+'Unknown attrib "'+name+'" or program not in use'+#13#10;
end;

function TShaders.GetUniformLocation(ProgramId: GLUInt;
  const name: AnsiString): Integer;
begin
   result:=FUniformsHash.GetParam(name,ProgramId);
   if result=-2 then begin
      Result:=glGetUniformLocation(ProgramId, PGLChar(name));
      FUniformsHash.AddParam(name,result,ProgramId);
   end;
   if Result<0 then
     UniformsWarnings:=UniformsWarnings+'Unknown uniform "'+name+'" or program not in use'+#13#10;
end;

procedure TShaders.BindAttribLocation(ProgramId: GLUInt; index: Integer;
  const name: AnsiString);
begin
   glBindAttribLocation(ProgramId, index, PGLChar(name));
end;

procedure TShaders.SetUniforms(ProgramId: GLUInt; const name: AnsiString;
  const value: single; count: GLsizei=1);
begin
  glUniform1fv(GetUniformLocation(ProgramId,name),count,@value);
end;

procedure TShaders.SetUniforms(ProgramId: GLUInt; const name: AnsiString;
  const value: TVector2f; count: GLsizei=1);
begin
  glUniform2fv(GetUniformLocation(ProgramId,name),count,@value);
end;

procedure TShaders.SetUniforms(ProgramId: GLUInt; const name: AnsiString;
  const value: TVector3f; count: GLsizei=1);
begin
  glUniform3fv(GetUniformLocation(ProgramId,name),count,@value);
end;

procedure TShaders.SetUniforms(ProgramId: GLUInt; const name: AnsiString;
  const value: TVector4f; count: GLsizei=1);
begin
  glUniform4fv(GetUniformLocation(ProgramId,name),count,@value);
end;

procedure TShaders.SetUniforms(ProgramId: GLUInt; const name: AnsiString;
  const value: integer; count: GLsizei=1);
begin
  glUniform1iv(GetUniformLocation(ProgramId,name),count,@value);
end;

procedure TShaders.SetUniforms(ProgramId: GLUInt; const name: AnsiString;
  const value: TVector2i; count: GLsizei=1);
begin
  glUniform2iv(GetUniformLocation(ProgramId,name),count,@value);
end;

procedure TShaders.SetUniforms(ProgramId: GLUInt; const name: AnsiString;
  const value: TVector3i; count: GLsizei=1);
begin
  glUniform3iv(GetUniformLocation(ProgramId,name),count,@value);
end;

procedure TShaders.SetUniforms(ProgramId: GLUInt; const name: AnsiString;
  const value: TVector4i; count: GLsizei=1);
begin
  glUniform4iv(GetUniformLocation(ProgramId,name),count,@value);
end;

procedure TShaders.SetUniforms(ProgramId: GLUInt; const name: AnsiString;
  const value: TMatrix2f; count: GLsizei=1; transpose: boolean=false);
begin
  glUniformMatrix2fv(GetUniformLocation(ProgramId,name),count,transpose,@value);
end;

procedure TShaders.SetUniforms(ProgramId: GLUInt; const name: AnsiString;
  const value: TMatrix3f; count: GLsizei=1; transpose: boolean=false);
begin
  glUniformMatrix3fv(GetUniformLocation(ProgramId,name),count,transpose,@value);
end;

procedure TShaders.SetUniforms(ProgramId: GLUInt; const name: AnsiString;
  const value: TMatrix4f; count: GLsizei=1; transpose: boolean=false);
begin
  glUniformMatrix4fv(GetUniformLocation(ProgramId,name),count,transpose,@value);
end;

function TShaders.CreateShaderProgram(Name: AnsiString; res: TResultType): integer;
var pid,sid:integer;
begin
   sid:=CreateShaderProgram;
   pid:=ShaderPrograms.Add(sid);
   if res = asHandle then result:=sid else result:=pid;
   assert(FProgramsFriendlyName.IndexOf(Name)<0,'Name "'+Name+'" is already used');
   FProgramsFriendlyName.Values[Name]:=inttostr(pid);
end;

function TShaders.AddShaderObject(code: ansistring; target: GLUInt;
  Name: AnsiString; res: TResultType): integer;
var index: integer;
begin
    index:=AddShaderObject(code, target, asIndex);
    assert(FObjectsFriendlyName.IndexOf(Name)<0,'Name "'+Name+'" is already used');
    FObjectsFriendlyName.Values[Name]:=inttostr(index);
    if res=asIndex then result:=index else result:=target;
end;

function TShaders.GetProgramObjectByName(Name: AnsiString; res: TResultType): integer;
var idx:integer;
begin
  idx:=strtoint(FProgramsFriendlyName.Values[Name]);
  if res = asHandle then result:=ShaderPrograms[idx]
  else result:=idx;
end;

function TShaders.GetShaderObjectByName(Name: AnsiString; res: TResultType): integer;
var idx:integer;
begin
  idx:=strtoint(FObjectsFriendlyName.Values[Name]);
  if res = asHandle then result:=ShaderObjectsList[idx]
  else result:=idx;
end;

procedure TShaders.UseProgram(Name: AnsiString);
var pid:integer;
begin
   pid:=GetProgramObjectByName(Name);
   UseProgramObject(pid);
end;

procedure TShaders.AttachShaderToProgram(ObjectName, ProgramName: AnsiString);
var ProgramId, ObjId:integer;
begin
  ObjId:=GetShaderObjectByName(ObjectName);
  ProgramId:=GetProgramObjectByName(ProgramName);
  glAttachShader(ProgramId, ObjId);
end;

procedure TShaders.LinkShaderProgram(ProgramName: AnsiString);
var ProgramId:integer;
begin
  ProgramId:=GetProgramObjectByName(ProgramName);
  LinkShaderProgram(ProgramId);
end;

function TShaders.LoadShader(const VSFileName, FSFileName: string;
  const GSFileName: string;res: TResultType): GLUint;
var vert, frag, geom: TStringList;
begin
  vert:=TStringList.Create;
  frag:=TStringList.Create;
  vert.LoadFromFile(VSFileName);
  frag.LoadFromFile(FSFileName);
  if GSFileName<>'' then begin
    geom:=TStringList.Create;
    geom.LoadFromFile(GSFileName);
    result:=CreateGSShader(vert.Text, frag.Text, geom.Text, res);
    geom.Free;
  end else result:=CreateShader(vert.Text, frag.Text, res);
  vert.Free; frag.Free;
end;

procedure TShaders.SetUniforms(ProgramName: AnsiString; const name: AnsiString;
  const value: single; count: GLsizei=1);
var ProgramId:GLUInt;
begin
  ProgramId:=GetProgramObjectByName(ProgramName);
  glUniform1fv(GetUniformLocation(ProgramId,name),count,@value);
end;

procedure TShaders.SetUniforms(ProgramName: AnsiString; const name: AnsiString;
  const value: TVector2f; count: GLsizei=1);
var ProgramId:GLUInt;
begin
  ProgramId:=GetProgramObjectByName(ProgramName);
  glUniform2fv(GetUniformLocation(ProgramId,name),count,@value);
end;

procedure TShaders.SetUniforms(ProgramName: AnsiString; const name: AnsiString;
  const value: TVector3f; count: GLsizei=1);
var ProgramId:GLUInt;
begin
  ProgramId:=GetProgramObjectByName(ProgramName);
  glUniform3fv(GetUniformLocation(ProgramId,name),count,@value);
end;

procedure TShaders.SetUniforms(ProgramName: AnsiString; const name: AnsiString;
  const value: TVector4f; count: GLsizei=1);
var ProgramId:GLUInt;
begin
  ProgramId:=GetProgramObjectByName(ProgramName);
  glUniform4fv(GetUniformLocation(ProgramId,name),count,@value);
end;

procedure TShaders.SetUniforms(ProgramName: AnsiString; const name: AnsiString;
  const value: integer; count: GLsizei=1);
var ProgramId:GLUInt;
begin
  ProgramId:=GetProgramObjectByName(ProgramName);
  glUniform1iv(GetUniformLocation(ProgramId,name),count,@value);
end;

procedure TShaders.SetUniforms(ProgramName: AnsiString; const name: AnsiString;
  const value: TVector2i; count: GLsizei=1);
var ProgramId:GLUInt;
begin
  ProgramId:=GetProgramObjectByName(ProgramName);
  glUniform2iv(GetUniformLocation(ProgramId,name),count,@value);
end;

procedure TShaders.SetUniforms(ProgramName: AnsiString; const name: AnsiString;
  const value: TVector3i; count: GLsizei=1);
var ProgramId:GLUInt;
begin
  ProgramId:=GetProgramObjectByName(ProgramName);
  glUniform3iv(GetUniformLocation(ProgramId,name),count,@value);
end;

procedure TShaders.SetUniforms(ProgramName: AnsiString; const name: AnsiString;
  const value: TVector4i; count: GLsizei=1);
var ProgramId:GLUInt;
begin
  ProgramId:=GetProgramObjectByName(ProgramName);
  glUniform4iv(GetUniformLocation(ProgramId,name),count,@value);
end;

procedure TShaders.SetUniforms(ProgramName: AnsiString; const name: AnsiString;
  const value: TMatrix2f; count: GLsizei=1; transpose: boolean=false);
var ProgramId:GLUInt;
begin
  ProgramId:=GetProgramObjectByName(ProgramName);
  glUniformMatrix2fv(GetUniformLocation(ProgramId,name),count,transpose,@value);
end;

procedure TShaders.SetUniforms(ProgramName: AnsiString; const name: AnsiString;
  const value: TMatrix3f; count: GLsizei=1; transpose: boolean=false);
var ProgramId:GLUInt;
begin
  ProgramId:=GetProgramObjectByName(ProgramName);
  glUniformMatrix3fv(GetUniformLocation(ProgramId,name),count,transpose,@value);
end;

procedure TShaders.SetGeomInputType(ProgramId, Value: GLUInt);
begin
  SetProgParam(ProgramId,GL_GEOMETRY_INPUT_TYPE_EXT,Value);
end;

procedure TShaders.SetGeomOutputType(ProgramId, Value: GLUInt);
begin
  SetProgParam(ProgramId,GL_GEOMETRY_OUTPUT_TYPE_EXT,Value);
end;

procedure TShaders.SetGeomVerticesOutCount(ProgramId, Value: GLUInt);
var Temp: cardinal;
begin
  if Value=0 then begin
    glGetIntegerv(GL_MAX_GEOMETRY_OUTPUT_VERTICES, @Temp);
    SetProgParam(ProgramId,GL_GEOMETRY_VERTICES_OUT_EXT,Temp);
  end;
  SetProgParam(ProgramId,GL_GEOMETRY_VERTICES_OUT_EXT,Value);
end;

procedure TShaders.SetProgParam(ProgramId, Param, Value: GLUInt);
begin
  glProgramParameteriEXT(ProgramId,Param,Value);
end;

procedure TShaders.SetUniforms(ProgramName: AnsiString; const name: AnsiString;
  const value: TMatrix4f; count: GLsizei=1; transpose: boolean=false);
var ProgramId:GLUInt;
begin
  ProgramId:=GetProgramObjectByName(ProgramName);
  glUniformMatrix4fv(GetUniformLocation(ProgramId,name),count,transpose,@value);
end;

procedure TShaders.GetActiveUniforms(ProgramId: GLUint;
  Uniforms: TSTringList);
var i,count:integer;
    l,size:integer;
    dtype: cardinal;
    names:PAnsiChar;
begin
  if not assigned(Uniforms) then Uniforms:=TStringList.Create;
  glGetObjectParameterivARB (ProgramId, GL_OBJECT_ACTIVE_UNIFORMS_ARB, @count);
  GetMem(names,10000);
  for i:= 0 to count-1 do begin
     glGetActiveUniform(ProgramId, i, 10000, @l, @size, @dtype, names);
     Uniforms.Add(names);
  end;FreeMem(names,10000);
end;

function TShaders.CreateShader(const VS, FS: AnsiString;
  res: TResultType): GLUint;
var vsId, fsId, spId: GLUint;
begin
  vsId := AddShaderObject(VS,$8B31);
  fsId := AddShaderObject(FS,$8B30);
  vsId := ShaderObjectsList[vsId];
  fsId := ShaderObjectsList[fsId];
  spId:=CreateShaderProgram(res);
  AttachShaderObjectToProgram(vsId,spId);
  AttachShaderObjectToProgram(fsId,spId);
  LinkShaderProgram(spId);
  result:=spId;
end;

function TShaders.CreateShader(const VS, FS: AnsiString; Defines: AnsiString; res: TResultType): GLUint;
var vsId, fsId, spId: GLUint;
begin
  vsId := AddShaderObject(Defines+VS,$8B31);
  fsId := AddShaderObject(Defines+FS,$8B30);
  vsId := ShaderObjectsList[vsId];
  fsId := ShaderObjectsList[fsId];
  spId:=CreateShaderProgram(res);
  AttachShaderObjectToProgram(vsId,spId);
  AttachShaderObjectToProgram(fsId,spId);
  LinkShaderProgram(spId);
  result:=spId;
end;


function TShaders.GetVersion: integer;
begin
   if FVersion<>-1 then result:=FVersion
   else result:=GetGLSLVersion;  
end;

{ THashList }

procedure THashList.AddParam(const name: AnsiString; value, GroupId: integer);
var p:PHashParam;
    key: integer;
begin
   key:=ComputeNameHashKey(name);
   assert(FGetParamByHashKey(key,GroupId)=-2,'Key Exists');
   new(p); p.Name:=name;
   p.GroupId:=GroupId;
   p.HashKey:=key;
   p.Value:=value;
   FHashList.add(p);
end;

function THashList.ComputeNameHashKey(const name: AnsiString): Integer;
var i, n: Integer;
begin
  n := Length(name);  Result := n;
  for i := 1 to n do  Result := (Result shl 1) + Byte(name[i]);
end;

constructor THashList.Create;
begin
  FHashList:= TList.Create;
end;

destructor THashList.Destroy;
var i:integer;
    p:PHashParam;
begin
  for i:=0 to FHashList.Count-1 do begin
    p:=FHashList[i]; dispose(p); end;
  FHashList.Free;
  inherited;
end;

function THashList.FGetParamByHashKey(HashKey: integer; Group:integer): integer;
var p:PHashParam;
    i:integer;
begin
  for i:=0 to FHashList.Count-1 do begin
    p:=FHashList[i];
    if (p.HashKey=HashKey) and (p.GroupId=Group) then begin
       result:=p.Value; exit;
    end;
  end;
  result:=-2;
end;

function THashList.GetParam(const name: AnsiString; GroupId: integer): integer;
var Key: integer;
begin
    key:=ComputeNameHashKey(name);
    result:=FGetParamByHashKey(key,GroupId);
end;

{ TShaderProgram }

procedure TShaderProgram.Apply;
begin
  if FProgramId>0 then FShaders.UseProgramObject(FProgramId);
  if assigned(FOnApplyShader) then FOnApplyShader(self);
end;

procedure TShaderProgram.UnApply;
begin
  FShaders.UseProgramObject(0);
  if assigned(FOnUnApplyShader) then FOnUnApplyShader(self);
end;

procedure TShaderProgram.BindAttribLocation(Index: Integer;
  const aName: AnsiString);
begin
  FShaders.BindAttribLocation(FProgramId,index,aName);
end;

function TShaderProgram.BuildProgram(aName: string): cardinal;
begin
  if FProgramId>0 then begin result:=FProgramId; exit; end;

  if not assigned(FShaders) then begin
    FShaders:=TShaders.Create; FShaders.Owner:=self; end;
  if aName<>'' then FName:=aName;
  if FName<>'' then FProgramId:=FShaders.CreateShaderProgram(FName)
  else FProgramId:=FShaders.CreateShaderProgram;
  if FVertexText.Count>0 then
    FVertObjId:=FShaders.AddShaderObject(FVertexText.Text,GL_VERTEX_SHADER,asHandle);
  if FFragmentText.Count>0 then
    FFragObjId:=FShaders.AddShaderObject(FFragmentText.Text,GL_FRAGMENT_SHADER,asHandle);
  if FGeometryText.Count>0 then
    FGeomObjId:=FShaders.AddShaderObject(FGeometryText.Text,GL_GEOMETRY_SHADER,asHandle);

  if FVertObjId>0 then FShaders.AttachShaderObjectToProgram(FVertObjId,FProgramId);
  if FFragObjId>0 then FShaders.AttachShaderObjectToProgram(FFragObjId,FProgramId);
  if FGeomObjId>0 then FShaders.AttachShaderObjectToProgram(FGeomObjId,FProgramId);

  FShaders.LinkShaderProgram(FProgramId); result:=FProgramId;
end;

constructor TShaderProgram.Create(ShadersCollection: TShaders);
begin
  inherited Create;
  FFragmentText:=TStringList.Create;
  FVertexText:=TStringList.Create;
  FGeometryText:=TStringList.Create;
  FUniforms:=TStringList.Create;
  FFragObjId:=0; FVertObjId:=0;
  FGeomObjId:=0; FProgramId:=0;
  FShaders:=ShadersCollection;
  FOwner:=nil;
end;

destructor TShaderProgram.Destroy;
begin
  FFragmentText.Free; FVertexText.Free;
  FGeometryText.Free; FUniforms.Free;
  if FShaders.Owner=self then FShaders.Free else begin
    FShaders.DeleteShaderObject(FFragObjId);
    FShaders.DeleteShaderObject(FVertObjId);
    FShaders.DeleteShaderObject(FGeomObjId);
    FShaders.DeleteShaderProgram(FProgramId);
  end;
  inherited;
end;

function TShaderProgram.GetAttribLocation(const aName: ansistring): integer;
begin
  Result:=FShaders.GetAttribLocation(FProgramId,aName);
end;

function TShaderProgram.GetAttribLocation(const aName: Ansistring; var buff: TVBOBuffer): integer;
var attr: PVBOAttribute;
    loc: integer;
begin
  loc:=FShaders.GetAttribLocation(FProgramId,aName);
  Attr:=GetAttribByName(buff,aName);
  if assigned(attr) then attr.Location:=loc;
  result:=loc;
end;

function TShaderProgram.GetUniformLocation(const aName: AnsiString): cardinal;
begin
  Result:=FShaders.GetUniformLocation(FProgramId,aName);
end;

function TShaderProgram.getUniforms: TStringList;
var i,count: integer;
    l,size: integer;
    dtype: cardinal;
    names: PAnsiChar;
begin
  if FProgramId>0 then begin
    FUniforms.Clear;
    glGetObjectParameterivARB (FProgramId, GL_OBJECT_ACTIVE_UNIFORMS_ARB, @count);
    GetMem(names,10000);
    for i:= 0 to count-1 do begin
      glGetActiveUniform(FProgramId, i, 10000, @l, @size, @dtype, names);
      FUniforms.Add(names);
    end;FreeMem(names,10000);
  end;
  result:=FUniforms;
end;

procedure TShaderProgram.SetUniforms(const name: AnsiString;
  const value: TVector4f; count: GLsizei);
begin
  glUniform4fv(GetUniformLocation(name),count,@value);
end;

procedure TShaderProgram.SetUniforms(const name: AnsiString;
  const value: integer; count: GLsizei);
begin
  glUniform1iv(GetUniformLocation(name),count,@value);
end;

procedure TShaderProgram.SetUniforms(const name: AnsiString;
  const value: TVector3f; count: GLsizei);
begin
  glUniform3fv(GetUniformLocation(name),count,@value);
end;

procedure TShaderProgram.SetUniforms(const name: AnsiString;
  const value: single; count: GLsizei);
begin
  glUniform1fv(GetUniformLocation(name),count,@value);
end;

procedure TShaderProgram.SetUniforms(const name: AnsiString;
  const value: TVector2f; count: GLsizei);
begin
  glUniform2fv(GetUniformLocation(name),count,@value);
end;

procedure TShaderProgram.SetUniforms(const name: AnsiString;
  const value: TVector2i; count: GLsizei);
begin
  glUniform2iv(GetUniformLocation(name),count,@value);
end;

procedure TShaderProgram.SetUniforms(const name: AnsiString;
  const value: TMatrix3f; count: GLsizei; transpose: boolean);
begin
  glUniformMatrix3fv(GetUniformLocation(name),count,transpose,@value);
end;

procedure TShaderProgram.SetUniforms(const name: AnsiString;
  const value: TMatrix4f; count: GLsizei; transpose: boolean);
begin
  glUniformMatrix4fv(GetUniformLocation(name),count,transpose,@value);
end;

procedure TShaderProgram.SetUniforms(const name: AnsiString;
  const value: TMatrix2f; count: GLsizei; transpose: boolean);
begin
  glUniformMatrix2fv(GetUniformLocation(name),count,transpose,@value);
end;

procedure TShaderProgram.SetUniforms(const name: AnsiString;
  const value: TVector3i; count: GLsizei);
begin
  glUniform3iv(GetUniformLocation(name),count,@value);
end;

procedure TShaderProgram.SetUniforms(const name: AnsiString;
  const value: TVector4i; count: GLsizei);
begin
  glUniform4iv(GetUniformLocation(name),count,@value);
end;

constructor TShaderProgram.Create(aVertex, aFragment, aGeometry: string);
begin
  Create(nil);
  CreateFromFile(aVertex, aFragment, aGeometry);
end;

function TShaderProgram.CreateFromFile(aVertex, aFragment: string; aGeometry: string): cardinal;
begin
  FVertexText.LoadFromFile(aVertex);
  FFragmentText.LoadFromFile(aFragment);
  if aGeometry<>'' then FGeometryText.LoadFromFile(aGeometry);
  result:=BuildProgram;
end;

procedure TShaderProgram.SetGeomInputType(ProgramId, Value: GLUInt);
begin
  SetProgParam(ProgramId,GL_GEOMETRY_INPUT_TYPE_EXT,Value);
end;

procedure TShaderProgram.SetGeomOutputType(ProgramId, Value: GLUInt);
begin
  SetProgParam(ProgramId,GL_GEOMETRY_OUTPUT_TYPE_EXT,Value);
end;

procedure TShaderProgram.SetGeomVerticesOutCount(ProgramId, Value: GLUInt);
var Temp: cardinal;
begin
  if Value=0 then begin
    glGetIntegerv(GL_MAX_GEOMETRY_OUTPUT_VERTICES, @Temp);
    SetProgParam(ProgramId,GL_GEOMETRY_VERTICES_OUT_EXT,Temp);
  end;
  SetProgParam(ProgramId,GL_GEOMETRY_VERTICES_OUT_EXT,Value);
end;

procedure TShaderProgram.SetProgParam(ProgramId, Param, Value: GLUInt);
begin
  glProgramParameteriEXT(ProgramId,Param,Value);
end;


{ TShaderLibrary }

function TShaderLibrary.Add(ShaderProg: TShaderProgram): integer;
var Hash: integer;
begin
 if ShaderProg.Name='' then ShaderProg.Name:='GLSLShader'+inttostr(Count+1);
 Result := inherited Add(ShaderProg);
 Hash:=StringHashKey(ShaderProg.Name);
 FHashList.Add(Hash);
end;

function TShaderLibrary.AddNewShader(aName: string): TShaderProgram;
begin
  result:=TShaderProgram.Create(FShaderCollection);
  result.FOwner:=self;
  if aName<>'' then result.Name:=aName else
    result.Name:='GLSLShader'+inttostr(Count+1);
  Add(result);
end;

procedure TShaderLibrary.Clear;
begin
  inherited;
end;

constructor TShaderLibrary.Create;
begin
  inherited;
  FHashList:=TIntegerList.Create;
end;

procedure TShaderLibrary.Delete(Index: Integer);
begin
  if assigned(Items[Index]) then begin
     Items[Index].Free; Items[Index]:=nil;
  end;
  FHashList.Delete(Index);
  inherited;
end;

destructor TShaderLibrary.Destroy;
var i:integer;
begin
  for i:=0 to Count-1 do begin
    if assigned(Items[i]) then
      if Items[i].FOwner=Self then Items[i].Free;
  end;
  FHashList.Free;
  inherited;
end;

procedure TShaderLibrary.Exchange(Index1, Index2: Integer);
begin
  inherited Exchange(Index1, Index2);
  FHashList.Exchange(Index1, Index2);
end;

function TShaderLibrary.Get(Index: Integer): TShaderProgram;
begin
  result := inherited Get(index);
end;

procedure TShaderLibrary.Insert(Index: Integer; ShaderProg: TShaderProgram);
var Hash: integer;
begin
  inherited Insert(Index, ShaderProg);
  Hash:=StringHashKey(ShaderProg.Name);
  FHashList.Insert(Index,Hash);
end;

procedure TShaderLibrary.Put(Index: Integer; Item: TShaderProgram);
var Hash: integer;
begin
   inherited Put(Index, Item);
   Hash:=StringHashKey(Item.Name);
   FHashList[Index]:=Hash;
end;

function TShaderLibrary.ShaderByName(aName: string): TShaderProgram;
var n, i: integer;
begin
    n:=StringHashKey(aName);
    if n<0 then result:=nil
    else begin
      i:=0;
      while (i<=FHashList.Count-1) do begin
          if n=FHashList[i] then
             if Items[i].Name=aName then Break;
          inc(i);
      end;
      if i<FHashList.Count then result:=Items[i]
      else result:=nil;
    end;
end;

end.

