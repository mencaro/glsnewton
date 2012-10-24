unit uMaterials;

interface

Uses Classes, Contnrs, VectorTypes, VectorGeometry, SysUtils,
   {$IFNDEF DIRECTGL}
     OpenGL1x, VectorLists,
   {$ELSE}
     dglOpenGL, uVectorLists,
   {$ENDIF}
     IniFiles, uMiscUtils, uBaseResource, OGLStateEmul;

Const
  cDiffuseColor: TVector = (0.8,0.8,0.8,1);
  cAmbientColor: TVector = (0.2,0.2,0.2,1);
  cSpecularColor: TVector = (0,0,0,1);
  cEmissiveColor: TVector = (0,0,0,1);

Type
  TColor = record
    Red, Green, Blue, Alpha: single;
  end;
  TLightModels = (lmNone, lmGouraud, lmPhong, lmBlinn, lmLambert, lmDeferred);
  TColorReplacing = (crDisable, crEmission, crAmbient, crDiffuse, crSpecular, crAmbientAndDiffuse);

  TMaterialType = (mtFFP, mtShader);
  TLightStyle = (lsSpot, lsOmni, lsParallel, lsDirectional);


  TColorVectorClass = class;

  TLightSource = class(TPersistentResource)
    private
     FEnabled: boolean;
     FLightStyle: TLightStyle;
     FLightModel: TLightModels;
     FSpotDirection: TVector;
     FSpotCutOff: single;
     FSpotExponent: single;
     FPosition: TVector;
     FAmbient: TColorVectorClass;
     FDiffuse: TColorVectorClass;
     FSpecular: TColorVectorClass;
     FSceneColor: TColorVectorClass;
     FConstAttenuation: single;
     FLinearAttenuation: single;
     FQuadraticAttenuation: single;
     FLightSlot: integer;
    public
     constructor Create;
     destructor Destroy; override;

     procedure ApplyLight(SlotId: integer);
     procedure UnApplyLight;
     procedure ImportLight(LightSource: integer);

     property SpotDirection: TVector read FSpotDirection write FSpotDirection;
     property Position: TVector read FPosition write FPosition;
     property LightStyle: TLightStyle read FLightStyle write FLightStyle;
     property LightModel: TLightModels read FLightModel write FLightModel;
     property SpotCutOff: single read FSpotCutOff write FSpotCutOff;
     property SpotExponent: single read FSpotExponent write FSpotExponent;
     property Ambient: TColorVectorClass read FAmbient;
     property Diffuse: TColorVectorClass read FDiffuse;
     property Specular: TColorVectorClass read FSpecular;
     property SceneColor: TColorVectorClass read FSceneColor write FSceneColor;
     property ConstAttenuation: single read FConstAttenuation write FConstAttenuation;
     property LinearAttenuation: single read FLinearAttenuation write FLinearAttenuation;
     property QuadraticAttenuation: single read FQuadraticAttenuation write FQuadraticAttenuation;
     property LightSlot: integer read FLightSlot write FLightSlot;

//    published
     property Enabled: boolean read FEnabled write FEnabled;

  end;

  TColorVectorClass = class
    private
      FColorVector: TVector;
      FColorRec: TColor;
      FColorKey: Integer;
      procedure RecalcHashKey;
      procedure setAlpha(const Value: single);
      procedure setBlue(const Value: single);
      procedure setGreen(const Value: single);
      procedure setRed(const Value: single);

      function getVecAddr: Pointer;
      procedure SetColorVector(const Value: TVector);
      procedure SetColorRec(const Value: TColor);
      function getIntColorVect: TVector4i;
      procedure setIntColorVect(const Value: TVector4i);
    public
      constructor Create;
      destructor Destroy;override;
      procedure Assign(Color: TColorVectorClass);

      procedure SetColor(r,g,b,a: byte); overload;
      procedure SetColor(r,g,b,a: single); overload;

      property ColorRec: TColor read FColorRec write SetColorRec;
      property ColorVector: TVector read FColorVector write SetColorVector;
      property ColorVector4i: TVector4i read getIntColorVect write setIntColorVect;
      property ColorAsAddress: Pointer read getVecAddr;
      property HasKey: integer read FColorKey;
//    published
      property Red: single read FColorVector[0] write setRed ;
      property Green: single read FColorVector[1] write setGreen;
      property Blue: single read FColorVector[2] write setBlue;
      property Alpha: single read FColorVector[3] write setAlpha;
  end;

  TMaterialProperties = class
    private
      FDiffuseColor: TColorVectorClass;
      FAmbientColor: TColorVectorClass;
      FSpecularColor: TColorVectorClass;
      FEmissionColor: TColorVectorClass;
      FShininess: single;
      FName: string;
    public
      constructor Create;
      destructor Destroy;override;
      procedure Assign(Material: TMaterialProperties); overload;
      procedure Apply;
      procedure UnApply;
      function IsEqual(MatProp: TMaterialProperties): boolean;
//    published
      property DiffuseColor: TColorVectorClass read FDiffuseColor write FDiffuseColor stored true;
      property AmbientColor: TColorVectorClass read FAmbientColor write FAmbientColor stored true;
      property SpecularColor: TColorVectorClass read FSpecularColor write FSpecularColor stored true;
      property EmissionColor: TColorVectorClass read FEmissionColor write FEmissionColor stored true;
      property Shininess: single read FShininess write FShininess stored true;
      property Name: string read FName write FName;
  end;

  TMaterial = class
      private
        FOwner: TObject;
        FMaterialProperties: TMaterialProperties;
        FLightProperties: TLightSource;
        FColorReplacing: TColorReplacing;
        FMaterialType: TMaterialType;
        FUseMaterial: boolean;
        FName: string;
        FNameHash: integer;
        procedure SetColorReplacing;
        procedure SetName(const Value: string);
        function GetHashName(const Name: string): integer;
      public
        constructor Create;
        destructor Destroy; override;
        procedure Apply;
        function UnApply: boolean;

        property Owner: TObject read FOwner write FOwner;
        property Light: TLightSource read FLightProperties;
        property Properties: TMaterialProperties read FMaterialProperties;
        property ColorReplacing: TColorReplacing read FColorReplacing write FColorReplacing;
        property MaterialType: TMaterialType read FMaterialType write FMaterialType;
        property Name: string read FName write SetName;
        property UseMaterial: boolean read FUseMaterial write FUseMaterial;
  end;

  TLightLibrary = class (TObjectList)
     private
       FOnAdding: TNotifyEvent;
       function Get(Index: Integer): TLightSource;
       procedure Put(Index: Integer; Item: TLightSource);
     public
       procedure Apply;
       function AddNewLight: TLightSource;
       property Items[Index: Integer]: TLightSource read Get write Put; default;
       property OnAdding: TNotifyEvent read FOnAdding write FonAdding;
  end;

  TMaterialLibrary = class (TObjectList)
     private
       FHashList: TIntegerList;
       FOnAdding: TNotifyEvent;
       function Get(Index: Integer): TMaterial;
       procedure Put(Index: Integer; Item: TMaterial);
     public
       property Items[Index: Integer]: TMaterial read Get write Put; default;

       function AddNewMaterial(Name:string=''): TMaterial;
       function Add(Material: TMaterial): integer;
       function MaterialByName(Name: string): TMaterial;
       function Last: TMaterial;

       procedure Clear; override;
       procedure Delete(Index: Integer);
       procedure Insert(Index: Integer; Material: TMaterial);
       procedure Exchange(Index1, Index2: Integer);

       constructor Create;
       destructor Destroy; override;

       property OnAdding: TNotifyEvent read FOnAdding write FonAdding;
  end;

  TMaterialsCollection = class
     private
       FMaterialList: TStringList;
       FMaterials: TList;
       function GetMList: String;
       function GetMatByIdx(index: integer): TMaterialProperties;
       function getCount: integer;
     public
       constructor Create;
       destructor Destroy;override;
       procedure LoadCollection (FileName: string);
       property MaterialList: String read GetMList;
       property Materials[index: integer]: TMaterialProperties read GetMatByIdx; default;
       property Count: integer read getCount;
  end;

implementation

{ TColorVectorClass }

procedure TColorVectorClass.Assign(Color: TColorVectorClass);
begin
   Red:=Color.Red;
   Green:=Color.Green;
   Blue:=Color.Blue;
   Alpha:=Color.Alpha;
   RecalcHashKey;
end;

constructor TColorVectorClass.Create;
begin
  inherited Create;
  Red:=0; Green:=0; Blue:=0; Alpha:=1;
  RecalcHashKey;
end;

destructor TColorVectorClass.Destroy;
begin
  inherited;
end;

function TColorVectorClass.getIntColorVect: TVector4i;
begin
  result[0]:=trunc(FColorVector[0]*255);
  result[1]:=trunc(FColorVector[1]*255);
  result[2]:=trunc(FColorVector[2]*255);
  result[3]:=trunc(FColorVector[3]*255);
end;

function TColorVectorClass.getVecAddr: Pointer;
begin
  result:=@FColorVector[0];
end;

procedure TColorVectorClass.RecalcHashKey;
begin
  FColorKey:=HashKey(FColorVector,65535);
end;

procedure TColorVectorClass.setAlpha(const Value: single);
begin
  FColorVector[3] := Value;
  FColorRec.Alpha := Value;
  RecalcHashKey;
end;

procedure TColorVectorClass.setBlue(const Value: single);
begin
  FColorVector[2] := Value;
  FColorRec.Blue := Value;
  RecalcHashKey;
end;

procedure TColorVectorClass.SetColorVector(const Value: TVector);
begin
  FColorVector := Value;
  FColorRec.Red:=FColorVector[0];
  FColorRec.Green:=FColorVector[1];
  FColorRec.Blue:=FColorVector[2];
  FColorRec.Alpha:=FColorVector[3];
  RecalcHashKey;
end;

procedure TColorVectorClass.SetColor(r, g, b, a: byte);
begin
  setRed(r/255);  setGreen(g/255);
  setBlue(b/255); setAlpha(a/255);
end;

procedure TColorVectorClass.SetColor(r, g, b, a: single);
begin
  setRed(r); setGreen(g); setBlue(b); setAlpha(a);
end;

procedure TColorVectorClass.SetColorRec(const Value: TColor);
begin
  FColorRec := Value;
  RecalcHashKey;
end;

procedure TColorVectorClass.setGreen(const Value: single);
begin
  FColorVector[1] := Value;
  FColorRec.Green := Value;
  RecalcHashKey;
end;

procedure TColorVectorClass.setIntColorVect(const Value: TVector4i);
begin
  setRed(Value[0]/255);  setGreen(Value[1]/255);
  setBlue(Value[2]/255); setAlpha(Value[3]/255);
end;

procedure TColorVectorClass.setRed(const Value: single);
begin
  FColorVector[0] := Value;
  FColorRec.Red := Value;
  RecalcHashKey;
end;

{ TMaterialProp }

procedure TMaterialProperties.Apply;
begin
//exit;
   if not glIsEnabled(GL_LIGHTING) then begin
      glColor4fv(FDiffuseColor.ColorAsAddress); exit;
   end;
   glMaterialfv(GL_FRONT_AND_BACK,GL_AMBIENT,FAmbientColor.ColorAsAddress);
   glMaterialfv(GL_FRONT_AND_BACK,GL_DIFFUSE,FDiffuseColor.ColorAsAddress);
   glMaterialfv(GL_FRONT_AND_BACK,GL_SPECULAR,FSpecularColor.ColorAsAddress);
   glMaterialfv(GL_FRONT_AND_BACK,GL_EMISSION,FEmissionColor.ColorAsAddress);
   glMaterialf (GL_FRONT_AND_BACK,GL_SHININESS,FShininess);

{   glMaterialfv(GL_FRONT,GL_AMBIENT,AmbientColor.ColorAsAddress);
   glMaterialfv(GL_FRONT,GL_DIFFUSE,DiffuseColor.ColorAsAddress);
   glMaterialfv(GL_FRONT,GL_SPECULAR,SpecularColor.ColorAsAddress);
   glMaterialfv(GL_FRONT,GL_EMISSION,EmissionColor.ColorAsAddress);
   glMateriali (GL_FRONT,GL_SHININESS,trunc(Shininess));
}
{   glMaterialfv(GL_BACK,GL_AMBIENT,AmbientColor.ColorAsAddress);
   glMaterialfv(GL_BACK,GL_DIFFUSE,DiffuseColor.ColorAsAddress);
   glMaterialfv(GL_BACK,GL_SPECULAR,SpecularColor.ColorAsAddress);
   glMaterialfv(GL_BACK,GL_EMISSION,EmissionColor.ColorAsAddress);
   glMateriali (GL_BACK,GL_SHININESS,trunc(Shininess));
}
end;

procedure TMaterialProperties.Assign(Material: TMaterialProperties);
begin
  FDiffuseColor.Assign(Material.DiffuseColor);
  FAmbientColor.Assign(Material.AmbientColor);
  FSpecularColor.Assign(Material.SpecularColor);
  FEmissionColor.Assign(Material.EmissionColor);
  FShininess:=Material.Shininess;
end;

constructor TMaterialProperties.Create;
begin
   inherited;
   FDiffuseColor:=TColorVectorClass.Create;
   FAmbientColor:=TColorVectorClass.Create;
   FSpecularColor:=TColorVectorClass.Create;
   FEmissionColor:=TColorVectorClass.Create;

   FAmbientColor.ColorVector:=cAmbientColor;
   FDiffuseColor.ColorVector:=cDiffuseColor;
   FSpecularColor.ColorVector:=cSpecularColor;
   FEmissionColor.ColorVector:=cEmissiveColor;
   FShininess:=0;
   FName:='';
end;

destructor TMaterialProperties.Destroy;
begin
  FDiffuseColor.Free;
  FAmbientColor.Free;
  FSpecularColor.Free;
  FEmissionColor.Free;
  UnApply;
  inherited;
end;

function TMaterialProperties.IsEqual(MatProp: TMaterialProperties): boolean;
begin
  result:=(MatProp.FDiffuseColor.FColorKey=FDiffuseColor.FColorKey) and
          (MatProp.FAmbientColor.FColorKey=FAmbientColor.FColorKey) and
          (MatProp.FSpecularColor.FColorKey=FSpecularColor.FColorKey) and
          (MatProp.FEmissionColor.FColorKey=FEmissionColor.FColorKey) and
          (MatProp.Shininess=Shininess);
end;

procedure TMaterialProperties.UnApply;
var
  vDiffuseColor: TVector;// = (0.8,0.8,0.8,1);
  vAmbientColor: TVector;// = (0.2,0.2,0.2,1);
  vSpecularColor: TVector;// = (0,0,0,1);
  vEmissiveColor: TVector;// = (0,0,0,1);
begin
   if not glIsEnabled(GL_LIGHTING) then begin
      glColor4f(1,1,1,1); exit;
   end;

  vDiffuseColor:=VectorMake(0.8,0.8,0.8,1);
  vAmbientColor:=VectorMake(0.2,0.2,0.2,1);
  vSpecularColor:=VectorMake(0,0,0,1);
  vEmissiveColor:=VectorMake(0,0,0,1);

  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, @vAmbientColor[0]);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, @vDiffuseColor[0]);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, @vSpecularColor[0]);
  glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, @vEmissiveColor[0]);
  glMateriali(GL_FRONT_AND_BACK, GL_SHININESS, 0);
{
  glMaterialfv(GL_FRONT, GL_AMBIENT, @vAmbientColor[0]);
  glMaterialfv(GL_FRONT, GL_DIFFUSE, @vDiffuseColor[0]);
  glMaterialfv(GL_FRONT, GL_SPECULAR, @vSpecularColor[0]);
  glMaterialfv(GL_FRONT, GL_EMISSION, @vEmissiveColor[0]);
  glMateriali(GL_FRONT, GL_SHININESS, 0);

  glMaterialfv(GL_BACK, GL_AMBIENT, @vAmbientColor[0]);
  glMaterialfv(GL_BACK, GL_DIFFUSE, @vDiffuseColor[0]);
  glMaterialfv(GL_BACK, GL_SPECULAR, @vSpecularColor[0]);
  glMaterialfv(GL_BACK, GL_EMISSION, @vEmissiveColor[0]);
  glMateriali(GL_BACK, GL_SHININESS, 0);
}
end;

{ TLightSource }

procedure TLightSource.ApplyLight(SlotId: integer);
var FLightId: integer;
begin
//  glEnable(GL_LIGHTING);
  FLightId:=GL_LIGHT0+SlotId;
  if Enabled then begin
    glEnable(FLightId);
    FLightSlot:=FLightId;
    if FLightStyle = lsParallel then begin
      glLightfv(FLightId, GL_POSITION, @FSpotDirection)
    end else begin
      glLightfv(FLightId, GL_POSITION, @FPosition);
    end;
    glLightfv(FLightId, GL_AMBIENT, @FAmbient.FColorVector);
    glLightfv(FLightId, GL_DIFFUSE, @FDiffuse.FColorVector);
    glLightfv(FLightId, GL_SPECULAR, @FSpecular.FColorVector);
    if FLightStyle = lsSpot then begin
      if FSpotCutOff <> 180 then begin
        glLightfv(FLightId, GL_SPOT_DIRECTION, @FSpotDirection);
        glLightfv(FLightId, GL_SPOT_EXPONENT, @FSpotExponent);
      end;
      glLightfv(FLightId, GL_SPOT_CUTOFF, @FSpotCutOff);
    end else begin
      glLightf(FLightId, GL_SPOT_CUTOFF, 180);
    end;
    glLightfv(FLightId, GL_CONSTANT_ATTENUATION, @FConstAttenuation);
    glLightfv(FLightId, GL_LINEAR_ATTENUATION, @FLinearAttenuation);
    glLightfv(FLightId, GL_QUADRATIC_ATTENUATION, @FQuadraticAttenuation);
  end else begin
    glDisable(FLightId);
    FLightSlot:=-1;
  end;
end;

procedure TLightSource.UnApplyLight;
begin
  if FLightSlot<>-1 then glDisable(FLightSlot);
end;

constructor TLightSource.Create;
begin
  inherited;
  FAmbient:=TColorVectorClass.Create;
  FDiffuse:=TColorVectorClass.Create;
  FSpecular:=TColorVectorClass.Create;
  FSceneColor:=TColorVectorClass.Create;
  FAmbient.ColorVector:=vectormake(0,0,0,1);
  FDiffuse.ColorVector:=vectormake(1,1,1,1);
  FSpecular.ColorVector:=vectormake(1,1,1,1);
  FSceneColor.ColorVector:=vectormake(0.2,0.2,0.2,1);
  FPosition:=vectormake(0,0,1,0);
  FSpotDirection:=vectormake(0,0,-1,0);
  FSpotExponent:=0;
  FSpotCutOff:=180;
  FConstAttenuation:=1;
  FLinearAttenuation:=0;
  FQuadraticAttenuation:=0;
  FLightStyle:=lsOmni;
  FLightModel:=lmGouraud;
  FLightSlot:=-1;
  Enabled:=false;
end;

destructor TLightSource.Destroy;
begin
  FAmbient.Free;
  FDiffuse.Free;
  FSpecular.Free;
  FSceneColor.Free;
  UnApplyLight;
  inherited;
end;

procedure TLightSource.ImportLight(LightSource: integer);
var param: TVector;
    fparam: single;
    li: cardinal;
begin
  li:=GL_LIGHT0+LightSource;
  glGetLightfv(li,GL_AMBIENT,@param);
  FAmbient.ColorVector:=param;
  glGetLightfv(li,GL_DIFFUSE,@param);
  FDiffuse.ColorVector:=param;
  glGetLightfv(li,GL_SPECULAR,@param);
  FSpecular.ColorVector:=param;
  glGetLightfv(li,GL_POSITION,@param);
  FPosition:=param;
  glGetLightfv(li,GL_SPOT_DIRECTION,@param);
  FSpotDirection:=param;
  glGetLightfv(li,GL_SPOT_EXPONENT,@fparam);
  FSpotExponent:=fparam;
  glGetLightfv(li,GL_SPOT_CUTOFF,@fparam);
  FSpotCutOff:=fparam;
  glGetLightfv(li,GL_CONSTANT_ATTENUATION,@fparam);
  FConstAttenuation:=fparam;
  glGetLightfv(li,GL_LINEAR_ATTENUATION,@fparam);
  FLinearAttenuation:=fparam;
  glGetLightfv(li,GL_QUADRATIC_ATTENUATION,@fparam);
  FQuadraticAttenuation:=fparam;
  FLightSlot:=0;
  FLightModel:=lmGouraud;
  if (FPosition[3]=1) and (FSpotCutOff=180) then FLightStyle:=lsOmni;
  if (FPosition[3]=0) and (FSpotCutOff=180) then FLightStyle:=lsDirectional;
  if (FPosition[3]=1) and (FSpotCutOff<180) then FLightStyle:=lsSpot;
  if (FPosition[3]=0) and (FSpotCutOff<180) then FLightStyle:=lsParallel;
  FEnabled:=glIsEnabled(li);
end;

{ TMaterial }

procedure TMaterial.Apply;
//var lPos,lDir:TVector;
begin
  if not FUseMaterial then begin UnApply; exit; end;
  SetColorReplacing;
  FMaterialProperties.Apply;
end;

constructor TMaterial.Create;
begin
  FMaterialProperties:=TMaterialProperties.Create;
  FLightProperties:=TLightSource.Create;
  FUseMaterial:=true;
  FColorReplacing:=crDisable;
  FMaterialType:=mtFFP;
  FOwner:=nil;
end;

destructor TMaterial.Destroy;
begin
  FMaterialProperties.Free;
  FLightProperties.Free;
  inherited;
end;

function TMaterial.GetHashName(const Name: string): integer;
var i, n: Integer;
begin
  n := Length(name);  Result := n;
  for i := 1 to n do  Result := (Result shl 1) + Byte(name[i]);
end;

procedure TMaterial.SetColorReplacing;
const cColorReplaceMode: array [crDisable..crAmbientAndDiffuse] of GLEnum =
      (0, GL_EMISSION, GL_AMBIENT, GL_DIFFUSE, GL_SPECULAR, GL_AMBIENT_AND_DIFFUSE);
begin
  if FColorReplacing=crDisable then
     glDisable(GL_COLOR_MATERIAL)
  else begin
     glEnable(GL_COLOR_MATERIAL);
     glColorMaterial(GL_FRONT,cColorReplaceMode[FColorReplacing]);
  end;
end;

procedure TMaterial.SetName(const Value: string);
begin
  FName := Value;
  FNameHash:=GetHashName(FName);
end;

function TMaterial.UnApply: boolean;
begin
  glDisable(GL_COLOR_MATERIAL);
  FMaterialProperties.UnApply;
  result:=false;
end;

{ TLightLibrary }

function TLightLibrary.AddNewLight: TLightSource;
begin
  result:=TLightSource.Create;
  Add(result);
  //result.Enabled:=true;
  if assigned(FOnAdding) then FOnAdding(self);
end;
procedure TLightLibrary.Apply;
var i: integer;
begin
  if Count>0 then glEnable(GL_LIGHTING);

  for i:=0 to Count-1 do Items[i].ApplyLight(i);
end;

function TLightLibrary.Get(Index: Integer): TLightSource;
begin
  result := inherited Get(index);
end;

procedure TLightLibrary.Put(Index: Integer; Item: TLightSource);
begin
   inherited Put(Index, Item);
end;

{ TMaterialLibrary }

function TMaterialLibrary.Add(Material: TMaterial): integer;
var Hash: integer;
begin
 if Material.Name='' then Material.Name:='GLMaterial'+inttostr(Count+1);
 Result := inherited Add(Material);
 Hash:=StringHashKey(material.Name);
 FHashList.Add(Hash);
 if assigned(FOnAdding) then FOnAdding(self);
end;

function TMaterialLibrary.AddNewMaterial(Name:string=''): TMaterial;
var mat: TMaterial;
begin
  mat:=TMaterial.Create;
  if Name<>'' then mat.Name:=Name else
     mat.Name:='GLMaterial'+inttostr(Count+1);
  mat.FOwner:=Self; result:=mat;
  Add(mat);
end;

procedure TMaterialLibrary.Clear;
begin
  inherited;
end;

constructor TMaterialLibrary.Create;
begin
  inherited;
  OwnsObjects:=false;
  FHashList:=TIntegerList.Create;
end;

procedure TMaterialLibrary.Delete(Index: Integer);
begin
  if assigned(Items[Index]) then begin
     Items[Index].Free; Items[Index]:=nil;
  end;
  FHashList.Delete(Index);
  inherited;
end;

destructor TMaterialLibrary.Destroy;
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

procedure TMaterialLibrary.Exchange(Index1, Index2: Integer);
begin
  inherited Exchange(Index1, Index2);
  FHashList.Exchange(Index1, Index2);
end;

function TMaterialLibrary.Get(Index: Integer): TMaterial;
begin
  result := inherited Get(index);
end;

procedure TMaterialLibrary.Insert(Index: Integer; Material: TMaterial);
var Hash: integer;
begin
  inherited Insert(Index, Material);
  Hash:=StringHashKey(Material.Name);
  FHashList.Insert(Index,Hash);
end;

function TMaterialLibrary.Last: TMaterial;
begin
  result:=Items[Count-1];
end;

function TMaterialLibrary.MaterialByName(Name: string): TMaterial;
var n, i: integer;
    hName: string;
begin
//result:=nil; exit;
    hName:=Name; n:=StringHashKey(hName);
    if n<0 then result:=nil
    else begin
      i:=0;
      while (i<=FHashList.Count-1) do begin
          if n=FHashList[i] then
             if Items[i].Name=hName then Break;
          inc(i);
      end;
      if i<FHashList.Count then result:=Items[i]
      else result:=nil;
    end;
end;

procedure TMaterialLibrary.Put(Index: Integer; Item: TMaterial);
var Hash: integer;
begin
   inherited Put(Index, Item);
   Hash:=StringHashKey(Item.Name);
   FHashList[Index]:=Hash;
end;

{ TMaterialsCollection }

constructor TMaterialsCollection.Create;
begin
  inherited;
  FMaterialList:=TStringList.Create;
  FMaterials:=TList.Create;
end;

destructor TMaterialsCollection.Destroy;
var i:integer;
    mat: TMaterialProperties;
begin
  FMaterialList.Free;
  for i:=0 to FMaterials.Count-1 do begin
    mat:=FMaterials[i]; mat.Free;
  end;
  FMaterials.Free;
  inherited;
end;

function TMaterialsCollection.getCount: integer;
begin
  result:=FMaterials.Count;
end;

function TMaterialsCollection.GetMatByIdx(index: integer): TMaterialProperties;
begin
  result:=FMaterials[index];
end;

function TMaterialsCollection.GetMList: String;
begin
  result:=FMaterialList.Text;
end;

function StringToVector(const s:string): TVector;
var t,c: string;
    i,d: integer;
begin
  t:=copy(s,2,length(s)-2);
  i:=pos(',',t); c:=copy(t,1,i-1); delete(t,1,i);
  trim(c); val(c,Result[0],d);
  i:=pos(',',t); c:=copy(t,1,i-1); delete(t,1,i);
  trim(c); val(c,Result[1],d);
  i:=pos(',',t); c:=copy(t,1,i-1); delete(t,1,i);
  trim(c); val(c,Result[2],d);
  i:=pos(',',t); c:=copy(t,1,i-1); delete(t,1,i);
  trim(c); val(c,Result[3],d);
end;

procedure TMaterialsCollection.LoadCollection(FileName: string);
var ini: TIniFile;
    i,d: integer;
    mat: TMaterialProperties;
    matName, temp: String;
    color: TVector;
begin
  if not fileexists(FileName) then exit;
  ini:=TIniFile.Create(FileName);
  ini.ReadSectionValues('Materials',FMaterialList);
  for i:=0 to FMaterialList.Count-1 do begin
    mat:=TMaterialProperties.Create;
    temp:=FMaterialList[i];
    d:=pos('=',temp); if d>0 then delete(temp,1,d) else temp:='';
    matName:=temp;//FMaterialList.ValueFromIndex[i];
    temp:=ini.ReadString(matName,'Color','(1.0,1.0,1.0,1.0)');
    color:=StringToVector(temp);
    temp:=ini.ReadString(matName,'Ambient','(0.2,0.2,0.2,1.0)');
    mat.AmbientColor.FColorVector:=StringToVector(temp);
    mat.AmbientColor.ColorVector:=VectorScale(mat.AmbientColor.FColorVector,color);
    temp:=ini.ReadString(matName,'Diffuse','(0.8,0.8,0.8,1.0)');
    mat.DiffuseColor.FColorVector:=StringToVector(temp);
    mat.DiffuseColor.ColorVector:=VectorScale(mat.DiffuseColor.FColorVector,color);
    temp:=ini.ReadString(matName,'Specular','(0.0,0.0,0.0,1.0)');
    mat.SpecularColor.FColorVector:=StringToVector(temp);
    mat.SpecularColor.ColorVector:=VectorScale(mat.SpecularColor.FColorVector,color);
    temp:=ini.ReadString(matName,'Shininess','0.0');
    val(temp,mat.FShininess,d);
    mat.Name:=matName;
    FMaterials.Add(mat);
  end;
  ini.Free;
end;

end.
