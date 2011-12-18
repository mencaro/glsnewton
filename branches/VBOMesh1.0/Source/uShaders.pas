{: uShaders
	Historique:
        07/04/10 - Fantom - Добавлено кеширование юниформ
}
unit uShaders;

interface

Uses VectorLists, OpenGL1x, VectorTypes, Classes;

Type
   THashParam = record
      HashKey: integer;
      Name: string;
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

        function ComputeNameHashKey(const name: string): Integer;
        procedure AddParam(const name: string; value: integer; GroupId: integer=-1);
        function GetParam(const name: string; GroupId: integer=-1): integer;
   end;

   TResultType = (asIndex, asHandle);
   TShaders = Class
     ShaderObjectsList:TIntegerList;
     ShaderPrograms:TIntegerList;
     Logs: string;
     UniformsWarnings:string;
     Private
       FProgramsFriendlyName: TStringList;
       FObjectsFriendlyName: TStringList;
       FUniformsHash: THashList;
     Public
       Function AddShaderObject(code: string;target: GLUInt; res: TResultType = asIndex):integer;overload;
       Function AddShaderObject(code: string;target: GLUInt; Name:string; res: TResultType = asIndex):integer;overload;
       Function CreateShaderProgram(res: TResultType = asHandle):integer;overload;
       Function CreateShaderProgram(Name:string; res: TResultType = asHandle):integer;overload;
       Function GetProgramObjectByName(Name:string; res: TResultType = asHandle):integer;
       Function GetShaderObjectByName(Name:string; res: TResultType = asHandle):integer;

       Function GetAttribLocation(ProgramId:GLUInt; const name : String) : Integer;
       Function GetUniformLocation(ProgramId:GLUInt; const name : String) : Integer;

       Procedure GetActiveUniforms(ProgramId:GLUint; Uniforms: TSTringList); 

       Procedure BindAttribLocation(ProgramId:GLUInt; index : Integer; const name : String);
       Procedure AttachShaderObjectToProgram(objId:GLUInt;ProgramId:GLUInt);
       Procedure AttachAllObjToProgram(ProgramId:GLUInt);
       Procedure AttachShaderToProgram(ObjectName,ProgramName:string);
       Procedure LinkShaderProgram(ProgramId:GLUInt);overload;
       Procedure LinkShaderProgram(ProgramName:string);overload;
       Procedure UseProgramIndex(index:integer);
       Procedure UseProgramObject(id:integer);
       Procedure UseProgram(Name:string);
       Procedure EndProgram;

       Procedure DeleteShaderObject(id:integer);
       Procedure DeleteShaderProgram(ProgramId:integer);
       
       Procedure ClearShaderObject;
       Procedure ClearShaderPrograms;

       Constructor Create;
       Destructor Destroy;override;

       Procedure SetUniforms(ProgramId:GLUInt; const name : String; const value: single;    count: GLsizei=1);overload;
       Procedure SetUniforms(ProgramId:GLUInt; const name : String; const value: TVector2f; count: GLsizei=1);overload;
       Procedure SetUniforms(ProgramId:GLUInt; const name : String; const value: TVector3f; count: GLsizei=1);overload;
       Procedure SetUniforms(ProgramId:GLUInt; const name : String; const value: TVector4f; count: GLsizei=1);overload;
       Procedure SetUniforms(ProgramId:GLUInt; const name : String; const value: integer;   count: GLsizei=1);overload;
       Procedure SetUniforms(ProgramId:GLUInt; const name : String; const value: TVector2i; count: GLsizei=1);overload;
       Procedure SetUniforms(ProgramId:GLUInt; const name : String; const value: TVector3i; count: GLsizei=1);overload;
       Procedure SetUniforms(ProgramId:GLUInt; const name : String; const value: TVector4i; count: GLsizei=1);overload;
       Procedure SetUniforms(ProgramId:GLUInt; const name : String; const value: TMatrix2f; count: GLsizei=1; transpose: boolean=false);overload;
       Procedure SetUniforms(ProgramId:GLUInt; const name : String; const value: TMatrix3f; count: GLsizei=1; transpose: boolean=false);overload;
       Procedure SetUniforms(ProgramId:GLUInt; const name : String; const value: TMatrix4f; count: GLsizei=1; transpose: boolean=false);overload;

       Procedure SetUniforms(ProgramName:String; const name : String; const value: single;    count: GLsizei=1);overload;
       Procedure SetUniforms(ProgramName:String; const name : String; const value: TVector2f; count: GLsizei=1);overload;
       Procedure SetUniforms(ProgramName:String; const name : String; const value: TVector3f; count: GLsizei=1);overload;
       Procedure SetUniforms(ProgramName:String; const name : String; const value: TVector4f; count: GLsizei=1);overload;
       Procedure SetUniforms(ProgramName:String; const name : String; const value: integer;   count: GLsizei=1);overload;
       Procedure SetUniforms(ProgramName:String; const name : String; const value: TVector2i; count: GLsizei=1);overload;
       Procedure SetUniforms(ProgramName:String; const name : String; const value: TVector3i; count: GLsizei=1);overload;
       Procedure SetUniforms(ProgramName:String; const name : String; const value: TVector4i; count: GLsizei=1);overload;
       Procedure SetUniforms(ProgramName:String; const name : String; const value: TMatrix2f; count: GLsizei=1; transpose: boolean=false);overload;
       Procedure SetUniforms(ProgramName:String; const name : String; const value: TMatrix3f; count: GLsizei=1; transpose: boolean=false);overload;
       Procedure SetUniforms(ProgramName:String; const name : String; const value: TMatrix4f; count: GLsizei=1; transpose: boolean=false);overload;

   end;

implementation

function IntToStr(x:integer):string;
begin str(x,result); end;
function StrToInt(s:string):integer;
var code:integer;
begin val(s,result,code);end;

{ TShaders }

Function TShaders.AddShaderObject(code: string; target: GLUInt; res: TResultType):integer;
var p : PGLChar;
    VertexObject, FragmentObject: integer;
    val,len:integer;
    pLog:PAnsiChar;
begin
   p:=PGLChar(TGLString(code)); Result := -1;
   if not assigned(ShaderObjectsList) then
      ShaderObjectsList:=TIntegerList.Create;
   case target of
      GL_VERTEX_SHADER: begin
           VertexObject := glCreateShader(GL_VERTEX_SHADER);
           if res = asIndex then
              Result := ShaderObjectsList.Add(VertexObject)
           else Result := VertexObject;
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
           else Result := FragmentObject;
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
      ELSE Logs:=Logs+'Error: Unknown shader target: '+IntToStr(target)+#13#10;
   end;
end;

Function TShaders.CreateShaderProgram(res: TResultType):integer;
var SPId:integer;
begin
  if not assigned(ShaderPrograms) then
     ShaderPrograms:=TIntegerList.Create;
  SPId:=glCreateProgram;
  if res = asIndex then  Result:= ShaderPrograms.add(SPId)
  else Result:=SPId;
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
  FreeMem(pLog,val); Assert(not Error,Logs);
end;

procedure TShaders.UseProgramIndex(index: integer);
begin
  UseProgramObject(ShaderPrograms[index]);
end;

procedure TShaders.UseProgramObject(id: integer);
begin
  glUseProgram(id);UniformsWarnings:='';
end;

procedure TShaders.EndProgram;
begin
  glUseProgram(0);
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
  FUniformsHash:= THashList.Create;
end;

Function TShaders.GetAttribLocation(ProgramId:GLUInt; const name : String) : Integer;
begin
   Result:=glGetAttribLocation(ProgramId, PGLChar(TGLString(name)));
   if Result<0 then
      UniformsWarnings:=UniformsWarnings+'Unknown attrib "'+name+'" or program not in use'+#13#10;
end;

function TShaders.GetUniformLocation(ProgramId: GLUInt;
  const name: String): Integer;
begin
   result:=FUniformsHash.GetParam(name,ProgramId);
   if result=-2 then begin
      Result:=glGetUniformLocation(ProgramId, PGLChar(TGLString(name)));
      FUniformsHash.AddParam(name,result,ProgramId);
   end;
   if Result<0 then
     UniformsWarnings:=UniformsWarnings+'Unknown uniform "'+name+'" or program not in use'+#13#10;
end;

procedure TShaders.BindAttribLocation(ProgramId: GLUInt; index: Integer;
  const name: String);
begin
   glBindAttribLocation(ProgramId, index, PGLChar(TGLString(name)));
end;

procedure TShaders.SetUniforms(ProgramId: GLUInt; const name: String;
  const value: single; count: GLsizei=1);
begin
  glUniform1fv(GetUniformLocation(ProgramId,name),count,@value);
end;

procedure TShaders.SetUniforms(ProgramId: GLUInt; const name: String;
  const value: TVector2f; count: GLsizei=1);
begin
  glUniform2fv(GetUniformLocation(ProgramId,name),count,@value);
end;

procedure TShaders.SetUniforms(ProgramId: GLUInt; const name: String;
  const value: TVector3f; count: GLsizei=1);
begin
  glUniform3fv(GetUniformLocation(ProgramId,name),count,@value);
end;

procedure TShaders.SetUniforms(ProgramId: GLUInt; const name: String;
  const value: TVector4f; count: GLsizei=1);
begin
  glUniform4fv(GetUniformLocation(ProgramId,name),count,@value);
end;

procedure TShaders.SetUniforms(ProgramId: GLUInt; const name: String;
  const value: integer; count: GLsizei=1);
begin
  glUniform1iv(GetUniformLocation(ProgramId,name),count,@value);
end;

procedure TShaders.SetUniforms(ProgramId: GLUInt; const name: String;
  const value: TVector2i; count: GLsizei=1);
begin
  glUniform2iv(GetUniformLocation(ProgramId,name),count,@value);
end;

procedure TShaders.SetUniforms(ProgramId: GLUInt; const name: String;
  const value: TVector3i; count: GLsizei=1);
begin
  glUniform3iv(GetUniformLocation(ProgramId,name),count,@value);
end;

procedure TShaders.SetUniforms(ProgramId: GLUInt; const name: String;
  const value: TVector4i; count: GLsizei=1);
begin
  glUniform4iv(GetUniformLocation(ProgramId,name),count,@value);
end;

procedure TShaders.SetUniforms(ProgramId: GLUInt; const name: String;
  const value: TMatrix2f; count: GLsizei=1; transpose: boolean=false);
begin
  glUniformMatrix2fv(GetUniformLocation(ProgramId,name),count,transpose,@value);
end;

procedure TShaders.SetUniforms(ProgramId: GLUInt; const name: String;
  const value: TMatrix3f; count: GLsizei=1; transpose: boolean=false);
begin
  glUniformMatrix3fv(GetUniformLocation(ProgramId,name),count,transpose,@value);
end;

procedure TShaders.SetUniforms(ProgramId: GLUInt; const name: String;
  const value: TMatrix4f; count: GLsizei=1; transpose: boolean=false);
begin
  glUniformMatrix4fv(GetUniformLocation(ProgramId,name),count,transpose,@value);
end;

function TShaders.CreateShaderProgram(Name: string; res: TResultType): integer;
var pid,sid:integer;
begin
   sid:=CreateShaderProgram;
   pid:=ShaderPrograms.Add(sid);
   if res = asHandle then result:=sid else result:=pid;
   assert(FProgramsFriendlyName.IndexOf(Name)<0,'Name "'+Name+'" is already used');
   FProgramsFriendlyName.Values[Name]:=inttostr(pid);
end;

function TShaders.AddShaderObject(code: string; target: GLUInt;
  Name: string; res: TResultType): integer;
var index: integer;
begin
    index:=AddShaderObject(code, target, asIndex);
    assert(FObjectsFriendlyName.IndexOf(Name)<0,'Name "'+Name+'" is already used');
    FObjectsFriendlyName.Values[Name]:=inttostr(index);
    if res=asIndex then result:=index else result:=target;
end;

function TShaders.GetProgramObjectByName(Name: string; res: TResultType): integer;
var idx:integer;
begin
  idx:=strtoint(FProgramsFriendlyName.Values[Name]);
  if res = asHandle then result:=ShaderPrograms[idx]
  else result:=idx;
end;

function TShaders.GetShaderObjectByName(Name: string; res: TResultType): integer;
var idx:integer;
begin
  idx:=strtoint(FObjectsFriendlyName.Values[Name]);
  if res = asHandle then result:=ShaderObjectsList[idx]
  else result:=idx;
end;

procedure TShaders.UseProgram(Name: string);
var pid:integer;
begin
   pid:=GetProgramObjectByName(Name);
   UseProgramObject(pid);
end;

procedure TShaders.AttachShaderToProgram(ObjectName, ProgramName: string);
var ProgramId, ObjId:integer;
begin
  ObjId:=GetShaderObjectByName(ObjectName);
  ProgramId:=GetProgramObjectByName(ProgramName);
  glAttachShader(ProgramId, ObjId);
end;

procedure TShaders.LinkShaderProgram(ProgramName: string);
var ProgramId:integer;
begin
  ProgramId:=GetProgramObjectByName(ProgramName);
  LinkShaderProgram(ProgramId);
end;

procedure TShaders.SetUniforms(ProgramName: string; const name: String;
  const value: single; count: GLsizei=1);
var ProgramId:GLUInt;
begin
  ProgramId:=GetProgramObjectByName(ProgramName);
  glUniform1fv(GetUniformLocation(ProgramId,name),count,@value);
end;

procedure TShaders.SetUniforms(ProgramName: string; const name: String;
  const value: TVector2f; count: GLsizei=1);
var ProgramId:GLUInt;
begin
  ProgramId:=GetProgramObjectByName(ProgramName);
  glUniform2fv(GetUniformLocation(ProgramId,name),count,@value);
end;

procedure TShaders.SetUniforms(ProgramName: string; const name: String;
  const value: TVector3f; count: GLsizei=1);
var ProgramId:GLUInt;
begin
  ProgramId:=GetProgramObjectByName(ProgramName);
  glUniform3fv(GetUniformLocation(ProgramId,name),count,@value);
end;

procedure TShaders.SetUniforms(ProgramName: string; const name: String;
  const value: TVector4f; count: GLsizei=1);
var ProgramId:GLUInt;
begin
  ProgramId:=GetProgramObjectByName(ProgramName);
  glUniform4fv(GetUniformLocation(ProgramId,name),count,@value);
end;

procedure TShaders.SetUniforms(ProgramName: string; const name: String;
  const value: integer; count: GLsizei=1);
var ProgramId:GLUInt;
begin
  ProgramId:=GetProgramObjectByName(ProgramName);
  glUniform1iv(GetUniformLocation(ProgramId,name),count,@value);
end;

procedure TShaders.SetUniforms(ProgramName: string; const name: String;
  const value: TVector2i; count: GLsizei=1);
var ProgramId:GLUInt;
begin
  ProgramId:=GetProgramObjectByName(ProgramName);
  glUniform2iv(GetUniformLocation(ProgramId,name),count,@value);
end;

procedure TShaders.SetUniforms(ProgramName: string; const name: String;
  const value: TVector3i; count: GLsizei=1);
var ProgramId:GLUInt;
begin
  ProgramId:=GetProgramObjectByName(ProgramName);
  glUniform3iv(GetUniformLocation(ProgramId,name),count,@value);
end;

procedure TShaders.SetUniforms(ProgramName: string; const name: String;
  const value: TVector4i; count: GLsizei=1);
var ProgramId:GLUInt;
begin
  ProgramId:=GetProgramObjectByName(ProgramName);
  glUniform4iv(GetUniformLocation(ProgramId,name),count,@value);
end;

procedure TShaders.SetUniforms(ProgramName: string; const name: String;
  const value: TMatrix2f; count: GLsizei=1; transpose: boolean=false);
var ProgramId:GLUInt;
begin
  ProgramId:=GetProgramObjectByName(ProgramName);
  glUniformMatrix2fv(GetUniformLocation(ProgramId,name),count,transpose,@value);
end;

procedure TShaders.SetUniforms(ProgramName: string; const name: String;
  const value: TMatrix3f; count: GLsizei=1; transpose: boolean=false);
var ProgramId:GLUInt;
begin
  ProgramId:=GetProgramObjectByName(ProgramName);
  glUniformMatrix3fv(GetUniformLocation(ProgramId,name),count,transpose,@value);
end;

procedure TShaders.SetUniforms(ProgramName: string; const name: String;
  const value: TMatrix4f; count: GLsizei=1; transpose: boolean=false);
var ProgramId:GLUInt;
begin
  ProgramId:=GetProgramObjectByName(ProgramName);
  glUniformMatrix4fv(GetUniformLocation(ProgramId,name),count,transpose,@value);
end;

procedure TShaders.GetActiveUniforms(ProgramId: GLUint;
  Uniforms: TSTringList);
var i,count:integer;
    l,size,dtype:integer;
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

{ THashList }

procedure THashList.AddParam(const name: string; value, GroupId: integer);
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

function THashList.ComputeNameHashKey(const name: string): Integer;
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

function THashList.GetParam(const name: string; GroupId: integer): integer;
var Key: integer;
begin
    key:=ComputeNameHashKey(name);
    result:=FGetParamByHashKey(key,GroupId);
end;

end.

