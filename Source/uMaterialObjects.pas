unit uMaterialObjects;

interface

uses Classes, Contnrs,
   {$IFNDEF DIRECTGL}
     OpenGL1x, VectorLists,
   {$ELSE}
     dglOpenGL, uVectorLists,
   {$ENDIF}
     uMiscUtils, uTextures, uMaterials, uShaders, OGLStateEmul;

Type

  TBlendingModes = (bmOpaque, bmTransparency, bmAdditive, bmAlphaTest50,
                    bmAlphaTest100, bmModulate, bmCustom);

  TMaterialObject = class;

  TMaterialProc = procedure (Sender: TMaterialObject) of object;

  TCustomBlending = class
  private
     FBlendEnable: boolean;
     FAlphaTestEnable: boolean;
     FSrcBlendFunc: cardinal;
     FDstBlendFunc: cardinal;
     FAlphaFunc: cardinal;
     FAlphaThreshold: single;
     FHashKey: integer;
     FBlendingMode: TBlendingModes;
     function GetHash: integer;
     procedure SetBlend(const Value: boolean);
  public
     constructor Create;
     destructor Destroy;override;
     procedure Apply;
     procedure SetByMode(aBlendingMode: TBlendingModes);
     procedure Assign(CustomBlending: TCustomBlending);


     property BlendEnable: boolean read FBlendEnable write SetBlend;
     property AlphaTestEnable: boolean read FAlphaTestEnable write FAlphaTestEnable;
     property SrcBlendFunc: cardinal read FSrcBlendFunc write FSrcBlendFunc;
     property DstBlendFunc: cardinal read FDstBlendFunc write FDstBlendFunc;
     property AlphaFunc: cardinal read FAlphaFunc write FAlphaFunc;
     property AlphaThreshold: single read FAlphaThreshold write FAlphaThreshold;
     property BlendingMode: TBlendingModes read FBlendingMode;

     property HashKey: integer read GetHash;
  end;

  TMaterialObject = class
  private
    FOwner: TObject;
    FActive: boolean;
    FBlending: TCustomBlending;
    FTexture: TTexture;
    FMaterial: TMaterial;
    FShader: TShaderProgram;
    FspId: cardinal;
    FHashKey: integer;
    FUseTexture: boolean;
    FUseMaterial: boolean;
    FUseShader: boolean;
    FTwoSideLighting: boolean;
    FIgnoreLighting: boolean;
    FName: string;
    FNameChanged: boolean;
    FAdditionalTextures: array of TTexture;
    FUseAddTex: boolean;
    FLastAddTex: integer;
    FNormalScale: single;

    function GetHash: integer;
    function CheckTransparency: boolean;
    procedure setUseMaterial(const Value: boolean);
    procedure setUseShader(const Value: boolean);
    procedure setUseTexture(const Value: boolean);
    procedure setName(const Value: string);
    function getAddTex(index: integer): TTexture;
    procedure setAddTex(index: integer; const Value: TTexture);
    procedure setIgnoreLighting(const Value: boolean);
    procedure setTwoSideLighting(const Value: boolean);
  public
    constructor Create;
    destructor Destroy;override;
    procedure Assign(MaterialObject: TMaterialObject);
    procedure Apply(ApplyProc: TMaterialProc=nil);
    procedure UnApply(UnApplyProc: TMaterialProc=nil);
    procedure AttachTexture(tex: TTexture);
    procedure AttachMaterial(mat: TMaterial);
    procedure AttachShader(Shader: TShaderProgram);
    procedure AddExTextures(tex: TTexture);

    function AddNewMaterial(aName: string=''): TMaterial;
    function AddNewTexture(aName: string=''): TTexture;
    function AddNewShader(aName: string=''): TShaderProgram;

    function TextureByMapTarget(Map: TMapTarget): TTexture;

    property Name: string read FName write setName;
    property Active: boolean read FActive write FActive;
    property HashKey: integer read GetHash;
    property Blending: TCustomBlending read FBlending;
    property Texture: TTexture read FTexture;
    property Material: TMaterial read FMaterial;
    property Shader: TShaderProgram read FShader;
    property TextureSlot[index: integer]: TTexture read getAddTex write setAddTex;
    property IsTransparency: boolean read CheckTransparency;
    property UseMaterial: boolean read FUseMaterial write setUseMaterial;
    property UseTexture: boolean read FUseTexture write setUseTexture;
    property UseShader: boolean read FUseShader write setUseShader;
    property UseAddinionalTextures: boolean read FUseAddTex write FUseAddTex;
    property TwoSideLighting: boolean read FTwoSideLighting write setTwoSideLighting;
    property IgnoreLighting: boolean read FIgnoreLighting write setIgnoreLighting;

    property NormalScale: single read FNormalScale write FNormalScale;

  end;

  TMaterialCache = class(TList)
  protected
    function GetItem(Index: Integer): TMaterialObject;
    procedure SetItem(Index: Integer; aObject: TMaterialObject);
  public
    function Add(Material: TMaterialObject): Integer;
    property Items[Index: Integer]: TMaterialObject read GetItem write SetItem; default;
  end;

  TMaterialObjectsLib = class (TObjectList)
  private
    FHashList: TIntegerList;
    FMatLib: TMaterialLibrary;
    FTexLib: TTextureLibrary;
    FShaderLib: TShaderLibrary;
    FLightLib: TLightLibrary;
    FOnAdding: TNotifyEvent;
    function Get(Index: Integer): TMaterialObject;
    procedure Put(Index: Integer; Item: TMaterialObject);
    function getShaders: TShaders;
    procedure setShaders(const Value: TShaders);
  public
    property Items[Index: Integer]: TMaterialObject read Get write Put; default;
    function AddNewMaterialObject(Name: string=''): TMaterialObject;
    function Add(MatObj: TMaterialObject): integer;
    function MaterialByName(Name: string): TMaterialObject;
    procedure Clear; override;
    procedure Delete(Index: Integer);
    procedure Insert(Index: Integer; MatObj: TMaterialObject);
    procedure Exchange(Index1, Index2: Integer);

    constructor Create;
    destructor Destroy; override;

    property MatLib: TMaterialLibrary read FMatLib write FMatLib;
    property TexLib: TTextureLibrary read FTexLib write FTexLib;
    property ShaderLib: TShaderLibrary read FShaderLib write FShaderLib;
    property LightLib: TLightLibrary read FLightLib write FLightLib;
    property Shaders: TShaders read getShaders write setShaders;
    property OnAdding: TNotifyEvent read FOnAdding write FonAdding;
  end;

  TTextureMaps = class
    AmbTex: TTexture;
    DifTex: TTexture;
    SpecTex: TTExture;
    GlosTex: TTexture;//mtShininess
    BumpTex: TTexture;
    NormTex: TTexture;
    AlphaTex: TTexture;
    OpacTex: TTexture;
    Refl: TTexture;
  end;
//TMapTarget = (mtAmbient, mtDiffuse, mtSpecular, mtShininess, mtBumpMap,
//                mtNormalMap, mtAlpha, mtOpacity, mtReflection);

  TTangentVectors = (tbTangent, tbNormal, tbBinormal);
  TTangents = set of TTangentVectors;

  TFragShaderOuts = (fsFragColor, fsAlbedo, fsNormal, fsTangent, fsBinormal,
    fsFragDepth, fsZeye, fsLinDepth, fsPosision, fsTexCoords, fsTexCoordsMipMatId);

  TShaderMaterial = class(TMaterialObject)
  private
    FLightModel: TLightModels;
    FSkeletal: boolean;
    FSkelNodesCount: integer;
    FSkeletalWeightsCount: byte;
    FTangents: TTangents;
    FUsedMaps: TMapTargets;
    FUseGLUniforms: boolean;
    FNoSpecular: boolean;
    FMRT: array of TFragShaderOuts;
    function BuildShaderName(ShaderType: TShaderType=stProgram): string; virtual;
    procedure BuildShader; virtual;
  public
    function BuildVertexText: ansistring; virtual;
    function BuildFragmentText: ansistring; virtual;
    function BuildGeometryText: ansistring; virtual;


  end;

implementation

{ TCustomBlending }

procedure TCustomBlending.Apply;
begin
  if FBlendEnable then begin
    glEnable(GL_BLEND);
    glBlendFunc(FSrcBlendFunc,FDstBlendFunc);
  end else glDisable(GL_BLEND);
  if FAlphaTestEnable then begin
    glEnable(GL_ALPHA_TEST);
    glAlphaFunc(FAlphaFunc,FAlphaThreshold);
  end else glDisable(GL_ALPHA_TEST);
end;

procedure TCustomBlending.Assign(CustomBlending: TCustomBlending);
begin
   FBlendEnable:=CustomBlending.BlendEnable;
   FAlphaTestEnable:=CustomBlending.AlphaTestEnable;
   FSrcBlendFunc:=CustomBlending.SrcBlendFunc;
   FDstBlendFunc:=CustomBlending.DstBlendFunc;
   FAlphaFunc:=CustomBlending.AlphaFunc;
   FAlphaThreshold:=CustomBlending.AlphaThreshold;
end;

constructor TCustomBlending.Create;
begin
  inherited;
  FBlendingMode:=bmOpaque;
  FBlendEnable:=false;
  FAlphaTestEnable:=false;
end;

destructor TCustomBlending.Destroy;
begin
  inherited;
end;

function TCustomBlending.GetHash: integer;
var hash: array[0..5] of cardinal;
begin
  if FHashKey<>-1 then begin result:=FHashKey; exit; end;

  hash[0]:=cardinal(FBlendEnable);
  hash[1]:=cardinal(FAlphaTestEnable);
  hash[2]:=FSrcBlendFunc;
  hash[3]:=FDstBlendFunc;
  hash[4]:=FAlphaFunc;
  hash[5]:=trunc(FAlphaThreshold*1000);
  FHashKey:=GetHashFromBuff(hash,16);
  result:=FHashKey;
end;

procedure TCustomBlending.SetBlend(const Value: boolean);
begin
  FBlendEnable := Value;
end;

procedure TCustomBlending.SetByMode(aBlendingMode: TBlendingModes);
begin
  FBlendingMode:=aBlendingMode;
  case aBlendingMode of
    bmOpaque:
      begin
        BlendEnable:=false;
        FAlphaTestEnable:=false;
        FSrcBlendFunc:=GL_ONE;
        FDstBlendFunc:=GL_ZERO;
        FAlphaFunc:=GL_ALWAYS;
        FAlphaThreshold:=0;
      end;
    bmTransparency:
      begin
        BlendEnable:=true;
        FAlphaTestEnable:=true;
        FSrcBlendFunc:=GL_SRC_ALPHA;
        FDstBlendFunc:=GL_ONE_MINUS_SRC_ALPHA;
        FAlphaFunc:=GL_GREATER;
        FAlphaThreshold:=0;
      end;
    bmAdditive:
      begin
        BlendEnable:=true;
        FAlphaTestEnable:=true;
        FSrcBlendFunc:=GL_SRC_ALPHA;
        FDstBlendFunc:=GL_ONE;
        FAlphaFunc:=GL_GREATER;
        FAlphaThreshold:=0;
      end;
    bmAlphaTest50:
      begin
        BlendEnable:=false;
        FAlphaTestEnable:=true;
        FAlphaFunc:=GL_GEQUAL;
        FAlphaThreshold:=0.5;
      end;
    bmAlphaTest100:
      begin
        BlendEnable:=false;
        FAlphaTestEnable:=true;
        FAlphaFunc:=GL_GEQUAL;
        FAlphaThreshold:=1;
      end;
    bmModulate:
      begin
        BlendEnable:=true;
        FAlphaTestEnable:=true;
        FSrcBlendFunc:=GL_DST_COLOR;
        FDstBlendFunc:=GL_ZERO;
        FAlphaFunc:=GL_GREATER;
        FAlphaThreshold:=0;
      end;
    bmCustom:
      begin
        BlendEnable:=false;
        FAlphaTestEnable:=false;
        FSrcBlendFunc:=GL_ONE;
        FDstBlendFunc:=GL_ZERO;
        FAlphaFunc:=GL_ALWAYS;
        FAlphaThreshold:=0;
      end;
  end;
end;

{ TMaterialObject }

procedure TMaterialObject.AddExTextures(tex: TTexture);
begin
  TextureSlot[FLastAddTex+1]:=tex;
end;

function TMaterialObject.AddNewMaterial(aName: string): TMaterial;
begin
  if assigned(FMaterial) and (FMaterial.Owner=self) then FMaterial.Free;
  FMaterial:=TMaterial.Create;
  FMaterial.Name:=aName;
  FMaterial.Owner:=self;
  FUseMaterial:=true;
  result:=FMaterial;
  FActive:=true;
end;

function TMaterialObject.AddNewShader(aName: string): TShaderProgram;
var sl: TShaders;
begin
  if assigned(FOwner) and (FOwner is TMaterialObjectsLib) then
    sl:=TMaterialObjectsLib(FOwner).FShaderLib.ShadersCollection else sl:=nil;
  FShader:=TShaderProgram.Create(sl);
  FShader.Name:=aName;
  FUseShader:=true;
  result:=FShader;
  FActive:=true;
end;

function TMaterialObject.AddNewTexture(aName: string): TTexture;
begin
  if assigned(FTexture) and (FTexture.Owner=self) then FTexture.Free;
  FTexture:=TTexture.Create;
  FTexture.Name:=aName;
  FTexture.Owner:=self;
  FUseTExture:=true;
  result:=FTexture;
  FActive:=true;
end;

procedure TMaterialObject.Apply(ApplyProc: TMaterialProc);
var i: integer;
begin
  FBlending.Apply;
  if assigned(FTexture) and FUseTexture then FTexture.Apply;
  if FUseAddTex then begin
    for i:=0 to FLastAddTex-1 do // high(FAdditionalTextures) do
      if assigned(FAdditionalTextures[i]) then
        FAdditionalTextures[i].Apply(i+1);
  end;
  if TwoSideLighting then glLightModeli(GL_LIGHT_MODEL_TWO_SIDE,1)
  else glLightModeli(GL_LIGHT_MODEL_TWO_SIDE,0);
  if IgnoreLighting then glDisable(GL_LIGHTING);
  if assigned(FMaterial) and FUseMaterial then FMaterial.Apply;
  if assigned(FShader) and FUseShader then FShader.Apply;
  if assigned(ApplyProc) then ApplyProc(Self);
end;

procedure TMaterialObject.Assign(MaterialObject: TMaterialObject);
var i: integer;
begin
  FBlending.Assign(MaterialObject.FBlending);
  FTexture:=MaterialObject.FTexture;
  FMaterial:=MaterialObject.FMaterial;
  FShader:=MaterialObject.FShader;
  FUseTexture:=MaterialObject.FUseTexture;
  FUseMaterial:=MaterialObject.FUseMaterial;
  FUseShader:=assigned(FShader);
  if assigned(FShader) then FSpId:=FShader.ProgramId;
  FActive:=MaterialObject.Active;
  for i:=0 to MaterialObject.FLastAddTex-1 do
    FAdditionalTextures[i]:=MaterialObject.FAdditionalTextures[i];
  FLastAddTex:=MaterialObject.FLastAddTex;
  UseAddinionalTextures:=MaterialObject.UseAddinionalTextures;
end;

procedure TMaterialObject.AttachMaterial(mat: TMaterial);
begin
  if Assigned(FMaterial) and (FMaterial.Owner=self) then FMaterial.Free;
  FMaterial:=mat; FHashKey:=-1;
  if assigned(mat) and (mat.UseMaterial) then
    FUseMaterial:=true else FUseMaterial:=false;
  FActive:=true;
end;

procedure TMaterialObject.AttachShader(Shader: TShaderProgram);
begin
  FShader:=Shader; FHashKey:=-1;
  if assigned(FShader) then FUseShader:=true;
  FActive:=true;
end;

procedure TMaterialObject.AttachTexture(tex: TTexture);
begin
  if assigned(FTexture) and (FTexture.Owner=self) then FTexture.Free;
  FTexture:=tex; FHashKey:=-1;
  if assigned(tex) and (not tex.Disabled) then
    FUseTexture:=true else FUseTexture:=false;
  FActive:=true;
end;

function TMaterialObject.CheckTransparency: boolean;
var alpha: single;
begin
  if assigned(FMaterial) then alpha:=Material.Properties.DiffuseColor.Alpha
  else alpha:=1;
  if assigned(FTexture) then begin
    if (FTexture.BlendingMode = tbmTransparency)
    or (FTexture.BlendingMode = tbmAdditive) then alpha:=0;
  end;
  if FBlending.FBlendEnable or FBlending.FAlphaTestEnable
  or (alpha<1) then result:=true else result:=false;
end;

constructor TMaterialObject.Create;
var mtc,i: integer;
begin
  FBlending:=TCustomBlending.Create;
  FspId:=0; FHashKey:=-1;
  FUseTexture:=false;
  FUseMaterial:=false;
  FUseShader:=false;
  FOwner:=nil;
  FName:='MeterialObject';
  FNameChanged:=true;
  glGetIntegerv( GL_MAX_TEXTURE_IMAGE_UNITS,@mtc);
  setlength(FAdditionalTextures,mtc); FUseAddTex:=false;
  if mtc>1024 then mtc:=1024;
  for i:=0 to length(FAdditionalTextures)-1 do FAdditionalTextures[i]:=nil;
  FLastAddTex:=0;
  FActive:=false; FTexture:=nil; FMaterial:=nil; FShader:=nil;
  FTwoSideLighting:=False;
  FIgnoreLighting:=False;
  inherited;
end;

destructor TMaterialObject.Destroy;
begin
  FBlending.Free;
  if assigned(FMaterial) and (FMaterial.Owner=self) then FMaterial.Free;
  if assigned(FTexture) and (FTexture.Owner=self) then FTexture.Free;
  if assigned(FShader) and (FShader.Owner=self) then FShader.Free;

  inherited;
end;

function TMaterialObject.getAddTex(index: integer): TTexture;
begin
  assert(index<length(FAdditionalTextures),'Not enough texture units');
  assert(index>0,'Use Main texture for slot 0');
  result:=FAdditionalTextures[index-1];
end;

function TMaterialObject.GetHash: integer;
var hash: array [0..4] of integer;
begin
  if FHashKey<>-1 then begin result:=FHashKey; exit; end;
  hash[0]:=integer(FTexture);
  hash[1]:=integer(FMaterial);
  hash[2]:=FBlending.HashKey;
  hash[3]:=FspId;
  hash[4]:=integer(FShader);
  FHashKey:=GetHashFromBuff(hash,sizeof(hash));
  result:=FHashKey;
end;

procedure TMaterialObject.setAddTex(index: integer; const Value: TTexture);
begin
  assert (index<length(FAdditionalTextures),'Not enough texture units');
  assert(index>0,'Use Main texture for slot 0');
  FAdditionalTextures[index-1]:=Value;
  if (Value<>nil) and (index>FLastAddTex) then FLastAddTex:=Index;
end;

procedure TMaterialObject.setIgnoreLighting(const Value: boolean);
begin
  FIgnoreLighting := Value; FActive:=true;
end;

procedure TMaterialObject.setName(const Value: string);
begin
 FName := Value; FNameChanged:=true;
 FHashKey:=StringHashKey(FName);
end;

procedure TMaterialObject.setTwoSideLighting(const Value: boolean);
begin
  FTwoSideLighting := Value; FActive:=true;
end;

procedure TMaterialObject.setUseMaterial(const Value: boolean);
begin
  if assigned(FMaterial) then FUseMaterial := Value
  else FUseMaterial := false;
end;

procedure TMaterialObject.setUseShader(const Value: boolean);
begin
  if assigned(FShader) then FUseShader := Value
  else FUseShader := false;
end;

procedure TMaterialObject.setUseTexture(const Value: boolean);
begin
  if assigned(FTexture) then FUseTexture := Value
  else FUseTexture := false;
end;

function TMaterialObject.TextureByMapTarget(Map: TMapTarget): TTexture;
var i: integer;
begin
  if Map=mtDiffuse then begin result:=FTexture; Exit; end;
  for i:=0 to FLastAddTex-1 do begin
    if assigned(FAdditionalTextures[i]) then
      if Map in FAdditionalTextures[i].MapTargets then begin
        result:=FAdditionalTextures[i]; exit;
      end;
  end; result:=nil;
end;

procedure TMaterialObject.UnApply(UnApplyProc: TMaterialProc);
begin
  if assigned(UnApplyProc) then UnApplyProc(self);
  if assigned(FShader) and FUseShader then
  FShader.UnApply;
  if assigned(FTexture) and FUseTexture then FTexture.UnApply(0);
  if assigned(FMaterial) and FUseMaterial then FMaterial.UnApply;
  if TwoSideLighting then glLightModeli(GL_LIGHT_MODEL_TWO_SIDE,0);
  if IgnoreLighting then glEnable(GL_LIGHTING);
end;

{ TMaterialCache }

function TMaterialCache.Add(Material: TMaterialObject): Integer;
var i,h: integer;
    mo: TMaterialObject;
begin
  h:=Material.HashKey;
  for i:=0 to count-1 do begin
    mo:=Items[i]; if mo.HashKey=h then begin result:=i; exit; end;
  end;
  Result := inherited Add(Material);
end;

function TMaterialCache.GetItem(Index: Integer): TMaterialObject;
begin
  Result := inherited Items[Index];
end;

procedure TMaterialCache.SetItem(Index: Integer; aObject: TMaterialObject);
begin
  inherited Items[Index] := AObject;
end;

{ TMaterialObjectsLib }

function TMaterialObjectsLib.Add(MatObj: TMaterialObject): integer;
var Hash: integer;
begin
 if MatObj.Name='' then MatObj.Name:='GLMaterial'+inttostr(Count+1);
 Result := inherited Add(MatObj);
 Hash:=StringHashKey(MatObj.Name);
 FHashList.Add(Hash);
end;

function TMaterialObjectsLib.AddNewMaterialObject(Name:string=''): TMaterialObject;
var mat: TMaterialObject;
begin
  mat:=TMaterialObject.Create;
  if Name<>'' then mat.Name:=Name else
     mat.Name:='GLMaterial'+inttostr(Count+1);
  mat.FOwner:=Self; result:=mat;
  Add(mat);
  if assigned(FOnAdding) then FOnAdding(self);
end;

procedure TMaterialObjectsLib.Clear;
begin
  inherited;
end;

constructor TMaterialObjectsLib.Create;
begin
  inherited;
  OwnsObjects:=false;
  FHashList:=TIntegerList.Create;
  FMatLib:=nil; FTexLib:=nil; FLightLib:=nil;
  FShaderLib:=TShaderLibrary.Create;
end;

procedure TMaterialObjectsLib.Delete(Index: Integer);
begin
  if assigned(Items[Index]) then begin
     Items[Index].Free; Items[Index]:=nil;
  end;
  FHashList.Delete(Index);
  inherited;
end;

destructor TMaterialObjectsLib.Destroy;
var i:integer;
begin
  for i:=0 to Count-1 do begin
    if assigned(Items[i]) then
      if Items[i].FOwner=Self then begin
         Items[i].Free; //Items[i]:=nil;
      end;
  end;
  FHashList.Free; FShaderLib.Free;
  inherited;
end;

procedure TMaterialObjectsLib.Exchange(Index1, Index2: Integer);
begin
  inherited Exchange(Index1, Index2);
  FHashList.Exchange(Index1, Index2);
end;

function TMaterialObjectsLib.Get(Index: Integer): TMaterialObject;
begin
  result := inherited Get(index);
end;

function TMaterialObjectsLib.getShaders: TShaders;
begin
  result:=FShaderLib.ShadersCollection;
end;

procedure TMaterialObjectsLib.Insert(Index: Integer; MatObj: TMaterialObject);
var Hash: integer;
begin
  inherited Insert(Index, MatObj);
  Hash:=StringHashKey(MatObj.Name);
  FHashList.Insert(Index,Hash);
end;

function TMaterialObjectsLib.MaterialByName(Name: string): TMaterialObject;
var n, i: integer;
    hName: string;
begin
  hName:=Name;
  n:=StringHashKey(hName);
  if n<0 then result:=nil
  else begin
    i:=0;
    while (i<=FHashList.Count-1) do begin
      if Items[i].FNameChanged then begin
        FHashList[i]:=StringHashKey(Items[i].Name);
        Items[i].FNameChanged:=false;
      end;
      if n=FHashList[i] then if Items[i].Name=hName then Break;
      inc(i);
    end;
    if i<FHashList.Count then result:=Items[i]
    else result:=nil;
  end;
end;

procedure TMaterialObjectsLib.Put(Index: Integer; Item: TMaterialObject);
var Hash: integer;
begin
  inherited Put(Index, Item);
  Hash:=StringHashKey(Item.Name);
  FHashList[Index]:=Hash;
end;

procedure TMaterialObjectsLib.setShaders(const Value: TShaders);
begin
  FShaderLib.ShadersCollection:=Value;
end;

{ TShaderMaterial }

function LightType(LightIndex: integer): TLightStyle;
begin
  with GLStateCache.LightingCache.Lights[LightIndex] do begin
    if POSITION[3]=0 then begin
      if SPOT_CUTOFF=0 then result:=lsParallel
      else result:=lsDirectional;
    end else if SPOT_CUTOFF=180 then result:=lsOmni
    else result:=lsSpot;
  end;
end;

function TShaderMaterial.BuildVertexText: ansistring;
var s: ansistring;
    nm: boolean;
    i,n: integer;
    lm: TLightModels;
    ls: set of TLightStyle;
    lt: TLightStyle;
    att: boolean;
    activelights: integer;
const br: ansistring = #13+#10;
begin
  lm:=FLightModel; ls:=[]; att:=false; activelights:=0;
  if (not UseMaterial) or (not assigned(Material)) or IgnoreLighting then lm:=lmNone;
  if not GLStateCache.LightingCache.Enabled then lm:=lmNone;

  if FSkeletal then s:='const int NodesCount = '+inttostr(FSkelNodesCount)+';'+br;
  if FSkeletal then begin
    s:=s+'uniform vec4 Bones[NodesCount*2];'+br;
    s:=s+'vec3 qrot( vec4 q, vec3 v ){ return v + 2.0*cross(q.xyz, cross(q.xyz ,v) + q.w*v); }'+br;
  end;
  if not FUseGLUniforms then begin
    if lm=lmGouraud then begin
      for i:=0 to GLStateCache.LightingCache.MaxLights-1 do
      with GLStateCache.LightingCache do begin
        if Lights[i].Enabled then begin
          if (Lights[i].LINEAR_ATTENUATION<>0)
          or (Lights[i].QUADRATIC_ATTENUATION<>0) then att:=true;
          lt:=LightType(i); if not (lt in ls) then include(ls,lt);
          inc(activelights);
        end;
      end;

      s:=s+'uniform vec4 mAmbientColor;'+br;
      s:=s+'uniform vec4 mDiffuseColor;'+br;
      s:=s+'uniform vec4 mSpecularColor;'+br;
      s:=s+'uniform vec4 mEmissionColor;'+br;
      s:=s+'uniform float mShininess;'+br;
      //Lights
      s:=s+'uniform vec4 lAmbientColor['+inttostr(activelights)+'];'+br;
      s:=s+'uniform vec4 lDiffuseColor['+inttostr(activelights)+'];'+br;
      s:=s+'uniform vec4 lSpecularColor['+inttostr(activelights)+'];'+br;
      s:=s+'uniform vec4 lPosition['+inttostr(activelights)+'];'+br;
      if ls<>[lsOmni] then begin
        s:=s+'uniform vec4 lDirection['+inttostr(activelights)+'];'+br;
        s:=s+'uniform float lSpotExp['+inttostr(activelights)+'];'+br;
        s:=s+'uniform float lSpotCosCutoff['+inttostr(activelights)+'];'+br;
      end;
      if att then begin
        s:=s+'uniform float lConstAtt['+inttostr(activelights)+'];'+br;
        s:=s+'uniform float lLinAtt['+inttostr(activelights)+'];'+br;
        s:=s+'uniform float lQuadAtt['+inttostr(activelights)+'];'+br;
      end;
    end else
      if lm=lmNone then s:=s+'uniform vec4 gDiffuseColor;'+br;
    s:=s+'uniform mat4 gModelViewMatrix;'+br;
    s:=s+'uniform mat3 gNormalMatrix;'+br;
    s:=s+'uniform mat4 gModelViewProjectionMatrix;'+br;
    s:=s+'uniform mat4 gProjectionMatrix;'+br;
  end;
  if tbTangent in FTangents then
    s:=s+'attribute vec3 tangents;'+br;
  if tbBinormal in FTangents then
    s:=s+'attribute vec3 binormals;'+br;
  if FSkeletal then begin
    case FSkeletalWeightsCount of
      0: s:=s+'float Weight; float Joint;'+br;
      1: begin
         s:=s+'attribute float Weight;'+br;
         s:=s+'attribute vec2 Joint;'+br;
      end;
      2: begin
         s:=s+'attribute vec2 Weight;'+br;
         s:=s+'attribute vec2 Joint;'+br;
      end;
      3: begin
         s:=s+'attribute vec3 Weight;'+br;
         s:=s+'attribute vec3 Joint;'+br;
      end;
      4: begin
         s:=s+'attribute vec4 Weight;'+br;
         s:=s+'attribute vec4 Joint;'+br;
      end;
    end;
  end;
  s:=s+'varying vec2 TexCoord;'+br;
  if (lm<>lmNone) and (lm<>lmGouraud) then
    s:=s+'varying vec3 normal, lightDir, eyeVec;'+br;
  if assigned(TextureByMapTarget(mtBumpMap))
  or assigned(TextureByMapTarget(mtNormalMap))
  then nm:=true else nm:=false;
  if nm and ((tbTangent in FTangents) or (tbBinormal in FTangents))
  then nm:=true else nm:=false;
  if (lm=lmNone) or (lm=lmGouraud) then nm:=false;

  if nm then s:=s+'varying vec3 Tang,Binorm;'+br;

  if FSkeletal then begin
    if nm then begin
      s:=s+'void vtransf(in vec4 qp, in vec4 qo, in vec4 V, '+
        'in vec3 T, in vec3 B, in vec3 N, in float w, '+
        'out vec4 _vert, out vec3 _norm, out vec3 _tang, out vec3 _bin)'+br;
      s:=s+'{'+br;
      s:=s+'  _vert += vec4(qrot(qo,V.xyz) + qp.xyz ,1.0)*w'+br;
      s:=s+'  vec3 nm = qrot(qo, N); _norm +=nm*w;'+br;
      s:=s+'  vec3 t,b;'+br;
      if (tbTangent in FTangents) and (tbBinormal in FTangents) then
        s:=s+'  t = qrot(q0,T); b = qrot(q0,B);'+br
      else if (tbTangent in FTangents) then
           s:=s+'  t = qrot(q0,tangents); b = cross(t,nm);'+br
           else s:=s+'  b = qrot(q0,binormals); t = cross(nm,b);'+br;
      s:=s+'   _tang += t*w; _bin +=b*w;'+br;
      s:=s+'}'+br;

    end else begin
      s:=s+'void vtransf(in vec4 qp, in vec4 qo, in vec4 V, in vec3 N, in float w, '+
        'out vec4 _vert, out vec3 _norm)'+br;
      s:=s+'{'+br;
      s:=s+'  _vert += vec4(qrot(qo,V.xyz) + qp.xyz ,1.0)*w'+br;
      s:=s+'  _norm += qrot(qo, N)*w;'+br;
      s:=s+'}'+br;
    end;
  end;

  if lm = lmGouraud then begin
    {$I PhongLightTypes.inc}
  end;


  s:=s+br+'void main ()'+br+'{'+br;

  if FUseGLUniforms then begin
    if lm=lmGouraud then begin
      s:=s+'  vec4 gAmbientColor = gl_FrontMaterial.ambient;'+br;
      s:=s+'  vec4 gDiffuseColor = gl_FrontMaterial.diffuse;'+br;
      s:=s+'  vec4 gSpecularColor = gl_FrontMaterial.specular;'+br;
      s:=s+'  vec4 gEmissionColor = gl_FrontMaterial.emission;'+br;
      s:=s+'  float gShininess = gl_FrontMaterial.shininess;'+br;
    end;
    s:=s+'  mat4 gModelViewMatrix = gl_ModelViewMatrix;'+br;
    s:=s+'  mat3 gNormalMatrix = gl_NormalMatrix;'+br;
    s:=s+'  mat4 gModelViewProjectionMatrix = gl_ModelViewProjectionMatrix;'+br;
    s:=s+'  mat4 gProjectionMatrix = gl_ProjectionMatrix;'+br;
  end;
  s:=s+'  TexCoord = gl_MultiTexCoord0.xy;'+br;
  s:=s+'  vec4 vert = gl_Vertex;'+br;
  s:=s+'  vec3 norm = gl_Normal.xyz;'+br;

  if FSkeletal then begin
    s:=s+'  vec4 outP = vec4(0.0,0.0,0.0,0.0);'+br;
    s:=s+'  vec3 outN = vec3(0.0,0.0,0.0);'+br;
    if nm then begin
      s:=s+'  vec3 T,B,outT,outB;'+br;
      s:=s+'  outB = outN; outT = outN;'+br;
      if (tbTangent in FTangents) then s:=s+'  T = tangents;'+br;
      if (tbBinormal in FTangents) then s:=s+'  B = binormals;'+br;
    end;

    case FSkeletalWeightsCount of
      0: begin
         s:=s+'  Weight = 1.0; Joint = int(gl_MultiTexCoord0.z*2.0);'+br;
         s:=s+'  vec4 qp = Bones[Joint];'+br;
         s:=s+'  vec4 qo = Bones[Joint+1];'+br;
         if nm then begin
           s:=s+'  vtransf(qp, qo, vert, T, B, norm, Weight, outP, outN, outT, outB)'+br;
           s:=s+'  vert = outP; norm = outN; T = outT; B = outB;'+br;
         end else begin
           s:=s+'  vtransf(qp, qo, vert, norm, Weight, outP, outN)'+br;
           s:=s+'  vert = outP; norm = outN;'+br;
         end;
      end;
      1: begin
         s:=s+'  vec4 qp = Bones[int(Joint.x)];'+br;
         s:=s+'  vec4 qo = Bones[int(Joint.x+1.0)];'+br;
         if nm then
           s:=s+'  vtransf(qp, qo, vert, T, B, norm, Weight, outP, outN, outT, outB)'+br
         else s:=s+'  vtransf(qp, qo, vert, norm, Weight, outP, outN)'+br;

         s:=s+'  vec4 qp = Bones[int(Joint.y)];'+br;
         s:=s+'  vec4 qo = Bones[int(Joint.y+1.0)];'+br;
         if nm then
           s:=s+'  vtransf(qp, qo, vert, T, B, norm, 1.0-Weight, outP, outN, outT, outB)'+br
         else s:=s+'  vtransf(qp, qo, vert, norm, 1.0-Weight, outP, outN)'+br;

         if nm then s:=s+'  vert = outP; norm = outN; T = outT; B = outB;'+br
         else s:=s+'  vert = outP; norm = outN;'+br;
      end;
      2..4: begin
         s:=s+'  vec4 qp = Bones[int(Joint.x)];'+br;
         s:=s+'  vec4 qo = Bones[int(Joint.x+1.0)];'+br;
         if nm then
           s:=s+'  vtransf(qp, qo, vert, T, B, norm, Weight.x, outP, outN, outT, outB)'+br
         else s:=s+'  vtransf(qp, qo, vert, norm, Weight.x, outP, outN)'+br;

         s:=s+'  vec4 qp = Bones[int(Joint.y)];'+br;
         s:=s+'  vec4 qo = Bones[int(Joint.y+1.0)];'+br;
         if nm then
           s:=s+'  vtransf(qp, qo, vert, T, B, norm, Weight.y, outP, outN, outT, outB)'+br
         else s:=s+'  vtransf(qp, qo, vert, norm, Weight.y, outP, outN)'+br;
         if FSkeletalWeightsCount>=3 then begin
           s:=s+'  vec4 qp = Bones[int(Joint.z)];'+br;
           s:=s+'  vec4 qo = Bones[int(Joint.z+1.0)];'+br;
           if nm then
             s:=s+'  vtransf(qp, qo, vert, T, B, norm, Weight.z, outP, outN, outT, outB)'+br
           else s:=s+'  vtransf(qp, qo, vert, norm, Weight.z, outP, outN)'+br;
         end;
         if FSkeletalWeightsCount=4 then begin
           s:=s+'  vec4 qp = Bones[int(Joint.w)];'+br;
           s:=s+'  vec4 qo = Bones[int(Joint.w+1.0)];'+br;
           if nm then
             s:=s+'  vtransf(qp, qo, vert, T, B, norm, Weight.w, outP, outN, outT, outB)'+br
           else s:=s+'  vtransf(qp, qo, vert, norm, Weight.w, outP, outN)'+br;
         end;
         if nm then s:=s+'  vert = outP; norm = outN; T = outT; B = outB;'+br
         else s:=s+'  vert = outP; norm = outN;'+br;
      end;
    end;
  end;
  s:=s+'  vert = gModelViewMatrix*vert;'+br;
  if (lm<>lmNone) then
    s:=s+'  norm = normalize(gNormalMatrix*norm);'+br;
  if nm then begin
    s:=s+'  Tang = normalize(gNormalMatrix*T);'+br;
    s:=s+'  Binorm = normalize(gNormalMatrix*B);'+br;
  end;

  s:=s+'  gl_Position = gProjectionMatrix*vert;'+br+br;

  if lm=lmNone then begin
    s:=s+'}'+br; result:=s; exit;
  end;

//  s:=s+'//Lightings'+br;
  if lm=lmNone then s:=s+'  gl_FrontColor = mDifuseColor;'+br
  else if lm=lmGouraud then begin
    s:=s+'  gl_FrontColor = flight(norm,vert);'+br;
{
    s:=s+'  float lamb = max(dot(norm, p), 0.0);'+br;
    s:=s+'  vec4 sceneColor = mEmissionColor + mAmbientColor * lAmbientColor[0];'+br;
    s:=s+'  vec4 final_color = (sceneColor * mAmbientColor) +'+
	'(lAmbientColor[0] * mAmbientColor);'+br;
    s:=s+'  final_color += lDiffuseColor[0] * mDiffuseColor * lamb;'+br;
    s:=s+'  gl_FrontColor = vec4(final_color.rgb,mDiffuseColor.a);'+br;
}
  end else begin
    s:=s+'  vec3 p = normalize(lPosition[0].xyz - vert.xyz);'+br;
    s:=s+'  normal = norm; lightDir = p; eyeVec = -vert.xyz;'+br;
  end;

  s:=s+'}'+br;
  result:=s;
end;

function TShaderMaterial.BuildFragmentText: ansistring;
begin
//
end;

function TShaderMaterial.BuildGeometryText: ansistring;
begin
  result:='';
end;

procedure TShaderMaterial.BuildShader;
var ShaderName: string;
    sp: TShaderProgram;
begin
  ShaderName:=BuildShaderName;
  if assigned(FShader) and (FShader.Name=ShaderName) then exit;
  if assigned(FOwner) then begin
    sp:=TMaterialObjectsLib(FOwner).FShaderLib.ShaderByName(ShaderName);
    if not assigned(sp) then
      sp:=TMaterialObjectsLib(FOwner).FShaderLib.AddNewShader(ShaderName);
  end else begin
    sp:=TShaderProgram.Create; sp.Name:=ShaderName; sp.Owner:=self;
  end;
end;

function TShaderMaterial.BuildShaderName(ShaderType: TShaderType): string;
var sName: string;
    i: integer;
begin
  case ShaderType of
   stVertex: sName:='Vertex: ';
   stFragment: sName:='Fragment: ';
   stGeometry: sName:='Geometry: ';
   stProgram: sName:='Program: ';
   else sName:='';
  end;

  case FLightModel of
    lmNone: sName:='lmNone_';
    lmGouraud: sName:='lmGouraud_';
    lmPhong: sName:='lmPhong_';
    lmBlinn: sName:='lmBlinn_';
    lmLambert: sName:='lmLambert_';
  end;

  if FNoSpecular then sName:=sName+'NoSpecular_';

  if FSkeletal then sName:=sName+'Skel_';
  if FSkeletal and (FSkeletalWeightsCount>0)
  then sName:=sName+inttostr(FSkeletalWeightsCount);
  if FTangents<>[tbNormal] then begin
    if tbTangent in FTangents then sName:=sName+'T';
    if tbBinormal in FTangents then sName:=sName+'B';
    sName:=sName+'_';
  end;
//    FUsedMaps: TMapTargets;
  if FUseGLUniforms then sName:=sName+'GL_';
  if length(FMRT)>0 then begin
    sName:=sName+'MRT';
    for i:=0 to high(FMRT) do sName:=sName+inttostr(ord(FMRT[i]));
    sName:=sName+'_';
  end;

  if sName='' then sName:='Default';
  result:=sName;
end;

end.
