unit uMaterialObjects;

interface

uses Classes, Contnrs,
     VectorLists,
     uMiscUtils, uTextures, uMaterials, uShaders,
     OpenGL1x, OGLStateEmul;

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
    FName: string;
    FNameChanged: boolean;
    FAdditionalTextures: array of TTexture;
    FUseAddTex: boolean;
    FNormalScale: single;
    function GetHash: integer;
    function CheckTransparency: boolean;
    procedure setUseMaterial(const Value: boolean);
    procedure setUseShader(const Value: boolean);
    procedure setUseTexture(const Value: boolean);
    procedure setName(const Value: string);
    function getAddTex(index: integer): TTexture;
    procedure setAddTex(index: integer; const Value: TTexture);
  public
    constructor Create;
    destructor Destroy;override;
    procedure Assign(MaterialObject: TMaterialObject);
    procedure Apply(ApplyProc: TMaterialProc=nil);
    procedure UnApply(UnApplyProc: TMaterialProc=nil);
    procedure AttachTexture(tex: TTexture);
    procedure AttachMaterial(mat: TMaterial);
    procedure AttachShader(Shader: TShaderProgram);

    function AddNewMaterial(aName: string=''): TMaterial;
    function AddNewTexture(aName: string=''): TTexture;

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
    FOnAdding: TNotifyEvent;
    function Get(Index: Integer): TMaterialObject;
    procedure Put(Index: Integer; Item: TMaterialObject);
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
    property OnAdding: TNotifyEvent read FOnAdding write FonAdding;
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

function TMaterialObject.AddNewMaterial(aName: string): TMaterial;
begin
  FMaterial:=TMaterial.Create;
  FMaterial.Name:=aName;
  FMaterial.Owner:=self;
  result:=FMaterial;
  FActive:=true;
end;

function TMaterialObject.AddNewTexture(aName: string): TTexture;
begin
  FTexture:=TTexture.Create;
  FTexture.Name:=aName;
  FTexture.Owner:=self;
  result:=FTexture;
  FActive:=true;
end;

procedure TMaterialObject.Apply(ApplyProc: TMaterialProc);
var i: integer;
begin
  FBlending.Apply;
  if assigned(FTexture) and FUseTexture then FTexture.Apply;
  if FUseAddTex then begin
    for i:=0 to high(FAdditionalTextures) do
      if assigned(FAdditionalTextures[i]) then
        FAdditionalTextures[i].Apply(i+1);
  end;
  if assigned(FMaterial) and FUseMaterial then FMaterial.Apply;
  if assigned(FShader) and FUseShader then FShader.Apply;
  if assigned(ApplyProc) then ApplyProc(Self);
end;

procedure TMaterialObject.Assign(MaterialObject: TMaterialObject);
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
begin
  if FBlending.FBlendEnable or FBlending.FAlphaTestEnable
  then result:=true else result:=false;
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
  glGetIntegerv(GL_MAX_TEXTURE_UNITS,@mtc);
  setlength(FAdditionalTextures,mtc); FUseAddTex:=false;
  if mtc>1024 then mtc:=1024;
  for i:=0 to length(FAdditionalTextures)-1 do FAdditionalTextures[i]:=nil;
  FActive:=false;
  FTexture:=nil; FMaterial:=nil; FShader:=nil;
  inherited;
end;

destructor TMaterialObject.Destroy;
begin
  FBlending.Free;
  if assigned(FMaterial) and (FMaterial.Owner=self) then FMaterial.Free;
  if assigned(FTexture) and (FTexture.Owner=self) then FTexture.Free;
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
end;

procedure TMaterialObject.setName(const Value: string);
begin
 FName := Value; FNameChanged:=true;
 FHashKey:=StringHashKey(FName);
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

procedure TMaterialObject.UnApply(UnApplyProc: TMaterialProc);
begin
  if assigned(UnApplyProc) then UnApplyProc(self);
  if assigned(FShader) and FUseShader then
  FShader.UnApply;
  if assigned(FTexture) and FUseTexture then FTexture.UnApply(0);
  if assigned(FMaterial) and FUseMaterial then FMaterial.UnApply;
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
  FHashList.Free;
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

end.
