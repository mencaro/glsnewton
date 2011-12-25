  { TODO 1 :
 - Переработать ХУД-спрайт
 + Проверить работоспособность TVolumetricLine
   + добавить возможность задавать отрезки(маркер?),
   + проверить перемещение линии,
   - добавить затухание линии (альфа, сужение).
 - Считать Extents для коллекций, контейнеров и чаилдов. Добавить проверку
   фрастум/окклюжн кулинга по этому Extents. Пересчитывать Extents при изменении
   стуктуры объекта. Добавить свойство управляющее проверкой видимости (только
   видимость родителя (через свойство чаилда), родитель + чаилды, каждый сам за себя)
 - Добавить StrunctureChanged ко всем методам Add*
 - Пересмотреть рендер 3ds
 - Источник света как MovableObject
 - Реализовать работу с группами мешей из 3ds/obj
 - Переписать загрузчики 3ds/obj на использование сабмешей
 - Добавить к мешу (фриформе) список используемых материалов
 - Для Instance "Master" реализовать расчет AABB по его инстансам
 - Реализовать сохранение/загрузку графа сцены
 - Доработать систему ЛОДов
 - Добавить ко всем классам уникальный идентификатор
 - Реализовать сериализацию (запись чанка в поток + таблица чанков)
 - Упаковка коллекции в сабмеши одного буфера
 - Сортировка сабмешей перед рендерингом по материалам
 - Создание/Установка проекционной матрицы у CameraController
 - FBO - реализовать рендеринг в слои/мип-уровни текстуры
 + Переписать проверку трансформации родителя на систему оповещений
 + Добавить систему нотификаций к TVBOMeshItem (регистрация при
     установке парента, оповещение о трансформации и удалении)
 + FBO Реализовать замену аттачментов
Контейнеры:
+ контейнер может быть чаилдом к любому объекту, включая другой контейнер
+ контейнер может иметь чаилды, чаилды могут обрабатываться либо перед либо после обработки контейнера
+ для контейнера указывается поток вывода - текстура/экран
+ элементы в контейнере могут сортироваться
+ прозрачные элементы в контейнере копируются в другой список, сортируются и выводятся последними
+ контейнеры имеют приоритет обработки - перед обходом дерева или последними
+ содержимое контейнера может быть распаковано в текущий список объектов
+ Сортировка по приоритету
+ Создать свойство, отвечающее за установку матриц сцены (заменять/не заменять)
+ Написать обработчик TVBOMeshObject.Process, с учетом чаилдов и контейнеров.
+ Разделить коллекции по приоритету, меньше нуля, включая 0 - до обработки мешей, остальные после
+ Реализовать передачу матриц сцены всем чаилдам
+ Добавить окклюжн куллинг к RenderShell
+ RenderShell может иметь локальную библиотеку текстур
+ В RenderShell реализовать механизм пересоздания текстур под размер вьюпорта
+ В RenderShell реализовать механизм управления рендертаргетами
+ Реализовать вывод в FBO
+ Реализовать автоматическое прикрепление текстур к FBO (в UpdateRenderTarget)
+ Контейнер vs коллекция? TContainer = class of RenderShell, Camera, Collection?
+ За контейнером могут быть закреплены камера и источники света
+ Свой RenderShell для каждой копии контейнера (RenderShell:=VBOMesh.AddContainer)
+ Если не определен окклюдер - использовать глобальный из vCubicOccluder из uVBO
+ Установить матрицы сцены контейнера (включая VBOMesh) через CameraController
+ Обращение к RenderShell из VBOMesh (VBOMesh.Render/Process->RenderShell.Process)
+ Реализовать работу с матрицами сцены через указатели (UseParentViewer/ParentViewMatrix)
  }

{: vboMesh
	Historique:
  17/12/11 - Fantom - Изменения в архитектуре:
                    - Создан базовый класс TVBOMeshItem, от которого наследуются TMovableObject,
                      TMeshCollection, TMeshContainer.
                    - TVBOMesh теперь наследуется от TMeshCollection и может хранить указатели на TMeshContainer
                    - Создан класс TMovableObject(TVBOMeshItem), в который перенесены
                      все методы TVBOMeshObject по ориентации объекта
                    - TVBOMeshObject наследуется от TMovableObject
                    - Базовые классы TVBOMeshItem и TMovableObject перенесены в модуль uBaseClasses
                    - Старая история перемещена в vboMesh1.0_history.txt
}

unit vboMesh;

interface

Uses
     Windows, Types, Classes,
     GLScene, OpenGL1x, VectorTypes, VectorGeometry, VectorLists,
     GLRenderContextInfo, Octree, GeometryBB,
     uVBO, PFXManager, uTextures, uFBO, uShaders, MTLLoader, SysUtilsLite, uCamera,
     uFileSMD, uFileObj, uMaterials, uMaterialObjects, uMiscUtils, uBaseClasses,
     uMeshObjects, OGLStateEmul;

Type
  TRenderBuffer = (rtNone, rtCurrent, rtFrameBuffer);

  TVBOMesh = class;
  TSceneOctree = class;
  TGLRender = class;


  TMeshCollection = class;

  TSceneParser = class
  private
    FDiffItems: TList;
    FContainers: TList;
    FOpaqueMeshObjects: TList;
    FProxyObjects: TList;
    FTransparencyMeshObjects: TList;
    FLowPriorityIndex: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure ParseCollection(MeshCollection: TMeshCollection;
                              ExpandContainer: boolean=false);
    property LowPriorityIndex: integer read FLowPriorityIndex;
  end;

  TGLRender = class
  private
    class function GetVisibleObjects(MeshList: TList;
      const Frustum: TFrustum): TList;
  public
    class procedure RenderMeshList(const Frustum: TFrustum; List: TList);
  end;

  TRenderShell = class
  private
    FSceneViewer: TViewerSettings;
    FRenderBuffer: TRenderBuffer;
    FCollection: TMeshCollection;
    FFBO: TFrameBufferObject;
    FLastTime: double;
    FSceneParser: TSceneParser;
    FTextures: TTextureLibrary;
    FActive: Boolean;
    FAttachments: array of TFBOTarget;

    procedure setProjMatrix(const Value: TMatrix);
    procedure setViewMatrix(const Value: TMatrix);
    function GetVisibleObjects(MeshList: TList;
      const Frustum: TFrustum): TList;
    procedure SortByDistance(const ViewMatrix: TMatrix; List: TList;
      SortDirection: TSortDirection);
    procedure ProcessCollection(Before: boolean);
    procedure ProcessOpaque;
    procedure ProcessTransparency;
    procedure ProcessProxys;
    procedure ProcessDiffObjects;
    procedure setHeight(const Value: integer);
    procedure setWidth(const Value: integer);
    procedure UpdateRenderTarget;
    function getAttachments(index: integer): TFBOTarget;
  public
    constructor Create(aParent: TMeshCollection);
    destructor Destroy; override;
    procedure Process; virtual;

    procedure AttachTexture(aTexture: TTexture; aTarget: TMRTTarget);

    property Active: boolean read FActive write FActive;
    property ViewMatrix: TMatrix read FSceneViewer.ViewMatrix write setViewMatrix;
    property ProjectionMatrix: TMatrix read FSceneViewer.ProjectionMatrix write setProjMatrix;
    property Frustum: TFrustum read FSceneViewer.Frustum;
    property RenderBuffer: TRenderBuffer read FRenderBuffer write FRenderBuffer;
    property FBO: TFrameBufferObject read FFBO;
    property FBOAttachments[index: integer]: TFBOTarget read getAttachments;
    property ViewportWidth: integer read FSceneViewer.ViewPort[2] write setWidth;
    property ViewportHeight: integer read FSceneViewer.ViewPort[3] write setHeight;
    property Textures: TTextureLibrary read FTextures;
  end;

  TMeshContainer = class;

  TMeshCollection = class (TVBOMeshItem)
  protected
    FMeshList: TList;
    FExtentsBuilded: Boolean;
    FSortDirection: TSortDirection;
    FMaterials: TMaterialLibrary;
    FTextures: TTextureLibrary;
    FMaterialObjects: TMaterialObjectsLib;
    FStructureChanged: boolean;
    FExpand: boolean;
    FPriority: integer;
    FOcclusionCulling: boolean;

    function GetMesh(Index: Integer): TVBOMeshItem;
    function GetCount: integer;
    function AddItem(mi: TVBOMeshItem): integer;virtual;
  public
    OctreeBuilded: Boolean;
    Visible: boolean;

    destructor Destroy;override;
    constructor Create;
    constructor CreateAsChild(aParent: TVBOMeshItem);
    procedure Process; override;

    property Count:integer read GetCount;
    property MeshList: TList read FMeshList;
    property ObjectsList[Index: Integer]: TVBOMeshItem read GetMesh;default;
    property SortDirection: TSortDirection read FSortDirection write FSortDirection;
    property ExpandingRequired: boolean read FExpand write FExpand;
    property Priority: integer read FPriority write FPriority;
    property OcclusionCulling: boolean read FOcclusionCulling write FOcclusionCulling;

    Procedure DeleteMeshObject(MeshObject: TVBOMeshItem; FreeObject: boolean=true);overload;
    Procedure DeleteMeshObject(index: integer; FreeObject: boolean=true);overload;
    Procedure Clear(FreeObjects: boolean=true);
    Procedure Unpack(ParentList: TList);
    Function Last: TVBOMeshItem;

    Function AddPoints(var Points: TAffineVectorArray;
                       var Param: TPointParam; Colors: PVectorArray=nil):
                       TVBOMeshObject;overload;
    Function AddPoints(PointsList:TAffineVectorList; ColorsList: TVectorList;
      var Param: TPointParam): TVBOMeshObject;overload;
    Function AddParticles(MaxCapacity:integer=-1): TVBOMeshObject;
    Function AddPlane(Width,Height:single; TilesX,TilesY:integer; HeightFunc: TGetHeightFunc=nil): TVBOMeshObject;
    Function AddGrid(Width,Height:single; TilesX,TilesY:integer; Color:PVector=nil): TVBOMeshObject;
    Function AddBox(Width,Height,Depth:single; TilesX,TilesY,TilesZ:integer): TVBOMeshObject;
    Function AddSphere(Radius: single; VSegments, HSegments: integer;
                       TileS:single=1;TileT:single=1;NormalInside:boolean = false): TVBOMeshObject;
    Function AddHUDSprite(width, height:single): TVBOMeshObject;
    Function AddScreenQuad(AddToMesh:boolean=true): TVBOMeshObject;
    Function AddSprite(s_type: TSpriteType; width, height: single): TVBOMeshObject;
    Function AddAnimatedSprite(s_type: TSpriteType; width, height: single): TVBOMeshObject;
    Function AddMeshFromFile(FileName: string; aMatLib: string=''): TVBOMeshObject;
    Function AddBBox(Corners: THmgBoundingBox; Color: TVector): TVBOMeshObject;overload;
    Function AddBBox(Extents: TExtents; Color: TVector): TVBOMeshObject;overload;
    Function AddInstanceToObject(MasterObject: TVBOMeshObject): TVBOMeshObject;
    Function AddProxyObject(MasterObject: TVBOMeshObject): TVBOMeshObject;
    Function AddUserObject(Name: string; VBOMeshList: TList): TVBOMeshObject;overload;
    Function AddUserObject(Name: string; VBOBuffer: PVBOBuffer): TVBOMeshObject;overload;
    Function AddMeshObject(mo:TVBOMeshObject): integer;
    Function AddSMDAnimation(MeshFile: string; const AnimFiles: array of string): TVBOMeshObject;overload;
    Function AddSMDAnimation(SMD: TSkeletalRender):TVBOMeshObject; overload;
    Function AddUniformSMD(MeshFile: string; const AnimFiles: array of string): TUniformSMDRender;overload;
    Function AddUniformSMD(SMD: TUniformSMDRender): TUniformSMDRender; overload;
    Function AddNewCollection: TMeshCollection;
    Function AddCollection(MeshCollection: TMeshCollection; Expand: boolean = false): integer;
    Function AddNewContainer: TMeshContainer;

    Function  GetObjectByName(Name: string): TVBOMeshItem;
    Procedure GetObjectListByType(ObjType: TMeshTypes; var List: TList);
    Procedure GetObjectListByTypeSet(ObjTypes: TMeshTypeSet; var List: TList);
    Procedure GetObjectListWithNameIncluded(Name: string; var List: TList);

    Procedure BuildOctree(level:integer=3);

    Function OctreeRayCastIntersect(const rayStart, rayVector: TVector; var List:Tlist ): boolean;
    Function ExtentsIntersect(const rayStart, rayVector: TVector; var List:Tlist): boolean;

    //Сортирует список объектов по расстоянию до камеры
    Procedure SortByDistance(const ViewMatrix: TMatrix; List: TList=nil;
                             SortDirection: TSortDirection=sdFrontToBack);
  end;

  TMeshContainer = class (TVBOMeshItem)
  private
    FRender: TRenderShell;
    FCollection: TMeshCollection;
    FCamera: TCameraController;
    FLights: TLightLibrary;
    FApplyLights: boolean;
  public
    constructor Create(aCollection: TMeshCollection);
    destructor Destroy;override;

    procedure Process; override;

    property Collection: TMeshCollection read FCollection;
    property Render: TRenderShell read FRender;
    property Camera: TCameraController read FCamera write FCamera;
    property Lights: TLightLibrary read FLights;
    property ApplyLights: boolean read FApplyLights write FApplyLights;
  end;

  TGLSceneMeshAdapter = class(TGLBaseSceneObject)
  public
    VBOMesh: TVBOMesh;
    procedure DoRender(var ARci: TRenderContextInfo;
                       ARenderSelf, ARenderChildren: Boolean); override;
  end;

  TVBOMesh = class(TMeshCollection)
    private
      FGLSceneMeshAdapter: TGLSceneMeshAdapter;
      FViewMatrix: TMatrix;
      FProjectionMatrix: TMatrix;
      FViewPort: array [0..3] of integer;
      FPolyCount: integer;
      FonBeforeRender: TVBOMeshRenderEvents;
      FonAfterRender: TVBOMeshRenderEvents;
      FOccluder: PVBOBuffer;
      FLights: TLightLibrary;
      FQueryObjectList: TList;
      FSceneOctree: TSceneOctree;
      FRenderPass: integer;
      FViewer: TViewerSettings;
      FRender: TRenderShell;
      FCamera: TCameraController;
      FOldRender: boolean;

      function GetVisibleObjects(const Frustum: TFrustum): TList;
      function LodSelect(mo: TVBOMeshObject): TVBOMeshObject;
      function GetMesh(Index: Integer): TVBOMeshObject;
      procedure ConvertAABBToCorners(const AABB:TAABB; var Corners:TAABBCorners);
      procedure SetViewMatrix(const Value: TMatrix);
    public
      Visible: boolean;
      property onBeforeRender: TVBOMeshRenderEvents read FonBeforeRender write FonBeforeRender;
      property onAfterRender: TVBOMeshRenderEvents read FonAfterRender write FonAfterRender;
      property ObjectsList[Index: Integer]: TVBOMeshObject read GetMesh;default;

      destructor Destroy;override;
      constructor Create;
      constructor CreateAsChild(aParentOwner: TGLBaseSceneObject);

      property GLSceneMeshAdapter: TGLSceneMeshAdapter read FGLSceneMeshAdapter;
      property PolygonsCount: integer read FPolyCount;
      property Materials: TMaterialLibrary read FMaterials write FMaterials;
      property Textures: TTextureLibrary read FTextures write FTextures;
      property MaterialObjects: TMaterialObjectsLib read FMaterialObjects;
      property Lights: TLightLibrary read FLights write FLights;
      property SceneOctree: TSceneOctree read FSceneOctree;
      property ViewMatrix: TMatrix read FViewMatrix write SetViewMatrix;
      property Camera: TCameraController read FCamera write FCamera;
      property OldRender: boolean read FOldRender write FOldRender;

      Procedure DoRender;
      Procedure Process; override;

      Procedure RenderMeshObject(MeshObject: TVBOMeshObject);
      Procedure DeleteMeshObject(MeshObject: TVBOMeshObject; FreeObject: boolean=true);overload;
      Procedure DeleteMeshObject(index: integer; FreeObject: boolean=true);overload;
      Function Last: TVBOMeshObject;

      Function  GetObjectByName(Name:string):TVBOMeshObject;
      Procedure GetObjectListByType(ObjType: TMeshTypes; var List:TList);
      Procedure GetObjectListByTypeSet(ObjTypes: TMeshTypeSet; var List:TList);
      Procedure GetObjectListWithNameIncluded(Name:String; var List:TList);

      Function OctreeRayCastIntersect(const rayStart, rayVector: TVector; var List:Tlist ): boolean;
      Function ExtentsIntersect(const rayStart, rayVector: TVector; var List:Tlist): boolean;

      //Переводит экранные координаты в пару Позиция в плоскости экрана - Направляющий вектор.
      //Результат функции - координаты точки из буфера глубюины
      Function ScreenToWorld(X, Y: single; Pos: PVector=nil; dir:PVector=nil): TVector;

      //Набор функций возвращающих объект по экранным координатам
      Function PickObject(X,Y:single; UseOctree:boolean=false):TVBOMeshObject;overload;
      Function PickObject(X, Y: single; var List: TList; UseOctree:boolean=false): boolean;overload;
      Function PickObject(Rect: TRect):TVBOMeshObject;overload;
      Function PickObject(Rect: TRect; var List:TList): Boolean;overload;
      Function PickObjectInFrustum(Rect: TRect; var List:TList): Boolean;
      //Переводит точку из глобальной системы координат (перспективной) в координаты плоскости экрана(ортогональные)
      Function WorldToPlane(P: TVector): TVector;

      //Сортирует список объектов по расстоянию до камеры
      Procedure SortByDistance(List: TList=nil; SortDirection: TSortDirection=sdFrontToBack);
      //Обновляет кеш сценовских материалов
      Procedure UpdateSceneMatrix;
  end;

  TOctreeLod = class
  public
    Parent: TOctreeLod;
    Lods: array of TOctreeLod;
    Extents: TExtents;
    ObjList: TList;
    BigList: TList;
    Level: integer;
    LodsCount: integer;
    constructor Create;
    destructor Destroy; override;
  end;

  TOctreeCallback = procedure (var Lod: TOctreeLod; var stop: boolean) of object;

  TSceneOctree = class
  private
    FMaxXLods, FMaxYLods,
    FMaxZLods: integer;
    FRoot: TOctreeLod;
    FOctreeCallback: TOctreeCallback;
    FBuilded: boolean;
    FTempList: TList;

    procedure BuildLeaf(var Lod: TOctreeLod; Offs,Size: TAffineVector);
    procedure ClearLeaf(var Leaf: TOctreeLod);
    procedure ByPassLod(var Lod: TOctreeLod);
    procedure FrustumCulling(var Lod: TOctreeLod; const Frustum: TFrustum);
  public
    procedure Clear;
    procedure BuildOctree(MeshList: TList; XLods, YLods, ZLods: integer);
    procedure GenVisibleList(const Frustum: TFrustum; var VisList: TList);
    procedure ByPassTree;

    constructor Create;
    destructor Destroy; override;

    property Root: TOctreeLod read FRoot;
    property OctreeCallback: TOctreeCallback read FOctreeCallback write FOctreeCallback;
    property Builded: boolean read FBuilded;
  end;

  function IntToStr(x: integer): string;
  procedure FreeList(var List:TList);
  procedure FreeAndNil(var Obj);
  function UpperCase(const S: string): string;
  function GetTime: double;

implementation

uses uAnimatedMesh;

function Max(a,b: single):single;
begin
  if a>b then result:=a else result:=b;
end;

function Min(a,b: single):single;
begin
  if a>b then result:=b else result:=a;
end;

function IntToStr(x: integer): string;
var s: ansistring;
begin Str(x, s); result:=string(s); end;

procedure FreeAndNil(var Obj);
var
  Temp: TObject;
begin
  Temp := TObject(Obj);
  Pointer(Obj) := nil;
  Temp.Free;
end;

procedure FreeList (var List:TList);
var i:integer;
    p:pointer;
begin
   if not assigned(List) then exit;
   for i:=0 to List.Count-1 do begin
     p:=list[i]; list[i]:=nil;
     if p<>nil then dispose(p);
   end; FreeAndNil(List);
end;

function UpperCase(const S: string): string;
var Ch: Char;
  L: Integer;
  Source, Dest: PChar;
begin
  L := Length(S); SetLength(Result, L);
  Source := Pointer(S);
  Dest := Pointer(Result);
  while L <> 0 do begin
    Ch := Source^;
    if (Ch >= 'a') and (Ch <= 'z') then Dec(Ch, 32);
    Dest^ := Ch; Inc(Source); Inc(Dest); Dec(L);
  end;
end;

function GetTime:double;
var Freq, Tick: Int64;
begin
  QueryPerformanceFrequency(Freq);
  QueryPerformanceCounter(Tick);
  Result:=Tick/Freq;
end;

function GetHashFromBuff(const Buffer; Count: Integer): Word; assembler;
asm
        MOV     ECX,EDX
        MOV     EDX,EAX
        XOR     EAX,EAX
@@1:    ROL     AX,5
        XOR     AL,[EDX]
        INC     EDX
        DEC     ECX
        JNE     @@1
end;

{ TVBOMesh }

function TVBOMesh.GetMesh(Index: Integer): TVBOMeshObject;
begin
   result:=FMeshList[Index];
end;

constructor TVBOMesh.Create;
begin
  inherited;
  FExtentsBuilded:=false;
  FMaterials:=TMaterialLibrary.Create;
  FTextures:=TTextureLibrary.Create;
  FMaterialObjects:=TMaterialObjectsLib.Create;
  FMaterialObjects.MatLib:=FMaterials;
  FMaterialObjects.TexLib:=FTextures;
  OctreeBuilded:=false;
  Visible:=true;
  FOcclusionCulling:=false;
  FOccluder:=CreateCubicOccluder;
  FLights:=TLightLibrary.Create;
  FSortDirection:=sdNone;
  FQueryObjectList:=TList.Create;
  FQueryObjectList.Capacity:=100000;
  FSceneOctree:=TSceneOctree.Create;
  FRenderPass:=0; FOldRender:=true;
  FGLSceneMeshAdapter:=nil;
  FRender:=TRenderShell.Create(self);
  FStructureChanged:=true;
end;

constructor TVBOMesh.CreateAsChild(aParentOwner: TGLBaseSceneObject);
begin
  Create;
  FGLSceneMeshAdapter:=TGLSceneMeshAdapter.CreateAsChild(aParentOwner);
  FGLSceneMeshAdapter.VBOMesh:=self;
  FGLSceneMeshAdapter.Visible:=true;
end;

function TVBOMesh.GetObjectByName(Name: string): TVBOMeshObject;
var i:integer;
    mo:TVBOMeshObject;
    s:string;
begin
    s:=Uppercase(Name); result:=nil;
    for i:=0 to FMeshList.Count-1 do begin
       mo:=FMeshList[i];
       if Uppercase(mo.Name)=s then begin
          result:=mo;exit; end;
    end;
end;

procedure TVBOMesh.GetObjectListByType(ObjType: TMeshTypes; var List: TList);
var i:integer;
    mo:TVBOMeshObject;
begin
  if not assigned(List) then List:=TList.Create else List.Clear;
  for i:=0 to FMeshList.Count-1 do begin
      mo:=FMeshList[i];
      if mo.MeshType=ObjType then List.Add(mo);
  end;
end;

procedure TVBOMesh.GetObjectListByTypeSet(ObjTypes: TMeshTypeSet; var List: TList);
var i:integer;
    mo:TVBOMeshObject;
begin
  if not assigned(List) then List:=TList.Create else List.Clear;
  for i:=0 to FMeshList.Count-1 do begin
      mo:=FMeshList[i];
      if mo.MeshType in ObjTypes then List.Add(mo);
  end;
end;

procedure TVBOMesh.GetObjectListWithNameIncluded(Name: String; var List: TList);
var i:integer;
    mo:TVBOMeshObject;
    s:string;
begin
    s:=Uppercase(Name);
    if not assigned(List) then List:=TList.Create else List.Clear;
    for i:=0 to FMeshList.Count-1 do begin
       mo:=FMeshList[i];
       if pos(s, Uppercase(mo.Name))>0 then List.Add(mo);
    end;
end;

function TVBOMesh.GetVisibleObjects(const Frustum: TFrustum): TList;
var //QueryObjectList: TList;
    mo: TVBOMeshObject;
    i: integer;
begin
//  QueryObjectList:=TList.Create;
//  QueryObjectList.Capacity:=FMeshList.Count*2;
  FQueryObjectList.Count:=0;
  for i:=0 to FMeshList.Count-1 do begin
    mo:=FMeshList[i];
    if assigned(mo) and (mo.MeshItemType=mcMeshObject) then begin
      if mo.Culled then mo.Occluded:=false; mo.Culled:=False;
      if (mo.Visible) or (mo.ProxyList.Count>0) then begin
        if assigned(mo.onBeforeCheckVisibility) then mo.onBeforeCheckVisibility(self);
        if not ((mo.MeshType=mtHUDSprite) or (mo.MeshType=mtSphericalSprite) or
               (mo.MeshType=mtCylindricalSprite))
        then begin
          if (mo.MeshType=mtActor) or (mo.MeshType=mtActorProxy) then
            mo.Culled:=TUniformSMDRender(mo).CheckVisibility(Frustum)
          else mo.Culled:=IsVolumeClipped(mo.GeomCenter, mo.BaundedRadius, Frustum);
        end;
        if mo.ProxyList.Count>0 then mo.Culled:=false;
        if (not mo.Culled) and (mo.MeshType<>mtInstance) then FQueryObjectList.Add(mo);
      end;
    end;
  end;
  result:=FQueryObjectList;
end;

destructor TVBOMesh.Destroy;
begin
  FreeVBOBuffer(FOccluder^); Dispose(FOccluder);
  FLights.Free; FQueryObjectList.Free;
  FSceneOctree.Free;
  FGLSceneMeshAdapter.Free;
  FRender.Free;
  inherited;
end;

procedure TVBOMesh.RenderMeshObject(MeshObject: TVBOMeshObject);
var mvm,proj:TMatrix;
begin
  glGetFloatv(GL_MODELVIEW_MATRIX, @mvm);
  glGetFloatv(GL_PROJECTION_MATRIX, @proj);
  MeshObject.Matrices.ProjectionMatrix:=proj;
  MeshObject.Time:=GetTime;
  MeshObject.RenderObject(mvm);
end;

Procedure TVBOMesh.DoRender;
var i{,n}: integer;
    mo,lod,master: TVBOMeshObject;
    F: TFrustum;
    time{,t1,t2}: Double;
    QueryObjectList: TList;
    queries: array of GLUInt;
    sampleCount: GLUint;
    available: GLUint;

begin
  FPolyCount:=0;
  if (not Visible) then exit;

  OGLStateEmul.GLStateCache.CheckStates;
  OGLStateEmul.GLStateCache.PushStates;

  time:=GetTime;
  if assigned(FCamera) then begin
    FRender.FSceneViewer.ViewMatrix:=FCamera.ViewMatrix;
    FRender.FSceneViewer.ProjectionMatrix:=FCamera.ProjectionMatrix;
    FRender.FSceneViewer.Frustum:=GetFrustum(FCamera.ProjectionMatrix,FCamera.ViewMatrix);
    FProjectionMatrix:=FCamera.ProjectionMatrix;
    FViewMatrix:=FCamera.ViewMatrix;
  end else begin
    glGetFloatv(GL_MODELVIEW_MATRIX, @FViewMatrix);
    glGetFloatv(GL_PROJECTION_MATRIX, @FProjectionMatrix);
  end;
  F := GetFrustum(FProjectionMatrix, FViewMatrix);
  FRender.FSceneViewer.Frustum:=F;
  FRender.FSceneViewer.CurrentTime:=time;
  FRender.FSceneViewer.ViewMatrix:=FViewMatrix;
  FRender.FSceneViewer.ProjectionMatrix:=FProjectionMatrix;

  glGetIntegerv(GL_VIEWPORT, @FViewPort);

  if not FOldRender then begin
    glEnable(GL_LIGHTING); glEnable(GL_LIGHT0); glDisable(GL_TEXTURE_2D);
    Lights.Apply;
    if assigned(onBeforeRender) then onBeforeRender;
    FRender.Process;
    if assigned(onAfterRender) then onAfterRender;
    OGLStateEmul.GLStateCache.PopStates;
    exit;
  end;

  glEnable(GL_LIGHTING); glEnable(GL_LIGHT0); glDisable(GL_TEXTURE_2D);
  Lights.Apply;
//  glEnable(GL_LIGHT0);

  if assigned(onBeforeRender) then onBeforeRender;
  if FSceneOctree.Builded then begin
    FSceneOctree.GenVisibleList(F,FQueryObjectList);
    QueryObjectList:=FQueryObjectList;
  end else QueryObjectList:=GetVisibleObjects(F);

  if not FOcclusionCulling then SortByDistance(QueryObjectList,FSortDirection)
  else begin
    SortByDistance(QueryObjectList,sdFrontToBack);
    setlength(queries,QueryObjectList.Count);
    glGenQueriesARB(QueryObjectList.Count, @queries[0]);
  end;
  for i:=0 to QueryObjectList.Count-1 do begin
    mo:=QueryObjectList[i];
    if assigned(mo.Params)
    and ((mo.MeshType<>mtPoints)and(mo.MeshType<>mtParticles))
    then master:=mo.Params else master:=mo;

    mo.Time:=time;
    mo.Matrices.ProjectionMatrix:=FProjectionMatrix;
    mo.Matrices.ViewMatrix:=FViewMatrix;
    mo.ParentViewer:=@FRender.FSceneViewer;

    if (not FOcclusionCulling) or (mo.PolygonsCount<100) then begin
      if (master.LodList.Count=0) or (not master.UseLods) then begin
        if mo.ProxyList.Count>0 then mo.SortProxyByDistance(FSortDirection);
        mo.RenderObject(FViewMatrix);
        FPolyCount:=FPolyCount+mo.PolygonsCount;
      end else begin
        lod:=LodSelect(mo);
        if assigned(Lod) then begin
          lod.RenderObject(FViewMatrix);
          FPolyCount:=FPolyCount+lod.PolygonsCount;
        end;
      end;
    end else begin
      glBeginQueryARB(GL_SAMPLES_PASSED_ARB, queries[i]);
      if mo.Occluded and (not mo.IgnoreOcclusion) then begin
         glDisable(GL_LIGHTING);
         glColorMask(FALSE, FALSE, FALSE, FALSE); glDepthMask(FALSE);
           if assigned(FOccluder) then begin
             mo.RenderOccluder(FViewMatrix,FOccluder);
             FPolyCount:=FPolyCount+FOccluder.ElementsCount*mo.MeshList.Count;
           end else begin
             mo.RenderOccluder(FViewMatrix,vCubicOccluder);
             FPolyCount:=FPolyCount+vCubicOccluder.ElementsCount*mo.MeshList.Count;
           end;
         glColorMask(TRUE, TRUE, TRUE, TRUE); glDepthMask(TRUE);
         glEnable(GL_LIGHTING);
      end else begin
        if (master.LodList.Count=0) or (not master.UseLods) then begin
          mo.RenderObject(FViewMatrix);
          FPolyCount:=FPolyCount+mo.PolygonsCount;
        end else begin
          lod:=LodSelect(mo);
          if assigned(lod) then begin
            lod.RenderObject(FViewMatrix);
            FPolyCount:=FPolyCount+lod.PolygonsCount;
          end;
        end;
      end;
      glEndQueryARB(GL_SAMPLES_PASSED_ARB);
    end;
  end;
  if FOcclusionCulling then begin
//    t1:=GetTime;
    glFinish;
    i:=trunc(QueryObjectList.Count*3/4); //n:=0;
    repeat
      glGetQueryObjectivARB(queries[i], GL_QUERY_RESULT_AVAILABLE_ARB, @available);
      //inc(n);
    until available>0;
//    t2:=GetTime;
//    assert(false,FloatTostr(t2-t1)+' : '+inttostr(n));
    for i:=0 to QueryObjectList.Count-1 do begin
      glGetQueryObjectuivARB(queries[i], GL_QUERY_RESULT_ARB, @sampleCount);
      mo:=QueryObjectList[i];
      if (sampleCount>0) then mo.Occluded:=false else mo.Occluded:=true;
    end;
    glDeleteQueriesARB(QueryObjectList.Count,@queries[0]);
  end;
//  QueryObjectList.Free;
//  inc(FRenderPass); if FRenderPass=2 then FRenderPass:=0;
  if assigned(onAfterRender) then onAfterRender;
  OGLStateEmul.GLStateCache.PopStates;
end;

function TVBOMesh.ExtentsIntersect(const rayStart, rayVector: TVector; var List:Tlist): boolean;
var i:integer;
    mo:TVBOMeshObject;
    f:boolean;
    ri:PRaycastIntersectInfo;
    ip:TVector;
begin
   if not assigned(List) then List:=TList.Create;// else List.Clear;
   result:=false;
   for i:=0 to FMeshList.Count-1 do begin
       mo:=FMeshList[i];
       if mo.Visible and mo.Pickable then begin
         f:=mo.ExtentsIntersect(rayStart, rayVector,@ip);
         if f then begin new(ri);
            with ri^ do begin
              iPoint:=ip;iNormal:=RayVector;
              NegateVector(iNormal);
              PickedObject:=mo; ObjectIndex:=i;
              ObjMeshIndexList:=TIntegerList.Create;
              ObjMeshIndexList.Add(0);
            end;
            List.Add(ri);
         end;
       end;
   end; if List.Count>0 then result:=true;
end;

function TVBOMesh.OctreeRayCastIntersect(const rayStart, rayVector: TVector; var List:TList):boolean;
var i:integer;
    mo:TVBOMeshObject;
    iPoint, iNormal: TVector;
begin
   if not assigned(List) then List:=TList.Create else List.Clear;
   result:=false;
   for i:=0 to FMeshList.Count-1 do begin
       mo:=FMeshList[i];
       if mo.Visible and mo.Pickable then begin
          mo.OctreeRayCastIntersect(rayStart, rayVector, list, @iPoint, @iNormal);
       end;
   end; if list.Count>0 then result:=true;
end;

function TVBOMesh.ScreenToWorld(X, Y: single; Pos: PVector=nil; dir:PVector=nil): TVector;
var Inv,ViewProj:TMatrix;
    nx,ny{,z}:single;
    S,P:TVector;
begin
  ViewProj:=MatrixMultiply(FViewMatrix,FProjectionMatrix);
  Inv:=MatrixInvert(ViewProj);
  nx:=(x-FViewPort[0])/FViewPort[2]*2-1;
  ny:=(FViewPort[3]-(y-FViewPort[1]))/FViewPort[3]*2-1;
  setvector(S,nx,ny,-1,1);
  P:=VectorTransform(S,Inv);
  P[0]:=P[0]/P[3]; P[1]:=P[1]/P[3];
  P[2]:=P[2]/P[3]; P[3]:=1/P[3];
  if pos<>nil then begin pos^:=p; pos^[3]:=1; end;
  result:=pos^;
  setvector(S,nx,ny,-3,1);
  P:=VectorTransform(S,Inv);
  P[0]:=P[0]/P[3]; P[1]:=P[1]/P[3];
  P[2]:=P[2]/P[3]; P[3]:=1;
  if dir<>nil then begin
    P:=VectorSubtract(p,pos^);
    ScaleVector(P,-1);NormalizeVector(P);
    dir^:=p;
  end;
{
  glReadPixels(trunc(x), trunc(Vport[3]-y), 1, 1, GL_DEPTH_COMPONENT, GL_FLOAT, @z);
  setvector(S,nx,ny,2*z-1,1);
  P:=VectorTransform(S,Inv);
  P[0]:=P[0]/P[3]; P[1]:=P[1]/P[3];
  P[2]:=P[2]/P[3]; P[3]:=1/P[3];
  result:=p;
}
end;

procedure TVBOMesh.SetViewMatrix(const Value: TMatrix);
begin
  FViewMatrix := Value;
end;

function TVBOMesh.PickObject(X, Y: single; var List: TList; UseOctree:boolean=false): boolean;
var v,d,p:TVector;
    i:integer;
    ri:PRaycastIntersectInfo;
begin
  if not assigned(List) then List:=TList.Create else List.Clear;
  p:=ScreenToWorld(x,y,@v,@d);
  if UseOctree and OctreeBuilded then OctreeRayCastIntersect(v,d, List)
  else ExtentsIntersect(v,d,List);
  for i:=0 to List.Count-1 do begin
      ri:=List[i];
      ri.inScreen:=affinevectormake(v);
      ri.inObject:=affinevectormake(p);
      ri.Dir:=affinevectormake(d);
  end;
  result:=list.Count>0;
end;

Function TVBOMesh.PickObjectInFrustum(Rect: TRect; var List:TList): Boolean;
var i, j, x : integer;
    fDot: single;
    D,Frustum:array[0..3] of TAffineVector;
    P: array[0..7] of TAffineVector;
    c: TAABBCorners;
    cv: TAffineVector;
    inside:boolean;
    mo:TVBOMeshObject;
begin
  if not assigned(List) then List:=TList.Create else List.Clear;
  with Rect do begin
    ScreenToWorld( Left, Top, @P[0], @D[0]);
    ScreenToWorld( Left, Bottom, @P[1], @D[1]);
    ScreenToWorld( Right, Bottom, @P[2], @D[2]);
    ScreenToWorld( Right, Top, @P[3], @D[3]);
    NegateVector(D[0]);NegateVector(D[1]);
    NegateVector(D[2]);NegateVector(D[3]);
    Frustum[0] := VectorCrossProduct( D[0], D[1] );
    Frustum[1] := VectorCrossProduct( D[1], D[2] );
    Frustum[2] := VectorCrossProduct( D[2], D[3] );
    Frustum[3] := VectorCrossProduct( D[3], D[0] );

    for i := 0 to 3 do NormalizeVector(Frustum[i]);

    for i := 0 to Count-1 do begin
      mo:=FMeshList[i];
      if mo.Pickable then begin
        ConvertAABBToCorners(TAABB(mo.Extents),c); j:=0;
        inside:=true;
        repeat x:=0;
          repeat
             cv:=VectorSubtract(c[j],P[x]);
             NormalizeVector(cv);
             fDot := VectorDotProduct( cv, Frustum[x]);
             if fDot>0 then begin inside:=false; end;
             x:=x+1;
          until (x=4) or (not inside);
          j:=j+1;
        until (j=8) or (not inside);
        if inside then List.add(mo);
      end;
    end;
  end;

  if List.Count>0 then Result:=true else Result:=false;
end;

procedure TVBOMesh.Process;
begin
  DoRender;
end;

function TVBOMesh.PickObject(X, Y: single; UseOctree:boolean=false): TVBOMeshObject;
var L:TList;
    i:integer;
    d:single;
    ri,rio:PRaycastIntersectInfo;
    mo:TVBOMeshObject;
    ip:TVector;
    mv:TMatrix;
    mind:single;
begin
    L:=TList.Create; Result:=nil;
    if not PickObject(x,y,L,UseOctree) then exit;
//    glGetFloatv(GL_MODELVIEW_MATRIX, @mv);
    mind:=-1; rio:=nil;
    for i:=0 to L.Count-1 do begin
        ri:=L[i]; mo:=ri.PickedObject;
        ip:=ri.iPoint;
        mv:=MatrixMultiply(mo.Matrices.WorldMatrix,FViewMatrix);
        ip:=VectorTransform(ip,mv);
        d:=VectorNorm(ip);
        if (d<mind) or (mind<0) then begin
           mind:=d;result:=mo;rio:=ri;end;
    end;
    if assigned(result) and assigned(result.onObjectClick) then
       with rio^ do
          result.onObjectClick(trunc(X),trunc(Y),inScreen,inObject,Dir,result);
end;

procedure MouseLoc3D(var ClickRayP1, ClickRayP2: TAffineVector; X,Y: double);
var
   mvmatrix: TMatrix4d;
   projmatrix: TMatrix4d;
   viewport: TVector4i;
   dX, dY, dZ, dClickY: double;
begin
   glGetIntegerv(GL_VIEWPORT, @viewport);
   glGetDoublev (GL_MODELVIEW_MATRIX, @mvmatrix);
   glGetDoublev (GL_PROJECTION_MATRIX, @projmatrix);
   dClickY := (viewport[3] - y); // OpenGL renders with (0,0) on bottom, mouse reports with (0,0) on top

   gluUnProject (x, dClickY, 0.0, mvmatrix, projmatrix, viewport, @dX, @dY, @dZ);
   ClickRayP1 := affinevectormake ( dX, dY, dZ );
   gluUnProject (x, dClickY, 1.0, mvmatrix, projmatrix, viewport, @dX, @dY, @dZ);
   ClickRayP2 := affinevectormake ( dX, dY, dZ );
end;

Function TVBOMesh.PickObject(Rect: TRect; var List:TList): Boolean;
var i : integer;
    cv: TVector;
    mo:TVBOMeshObject;
    ViewMatrix, ProjMatrix: TMatrix;
    ViewPort: TVector4i;
function PointInRect(const Rect:TRect; aPoint: TVector):boolean;
begin
  with Rect do begin
     if  (aPoint[0]>=Left) and (aPoint[0]<=Right)
     and (aPoint[1]<=bottom) and (aPoint[1]>=Top)
     then result:=true else result:=false;
  end;
end;
begin
    ViewMatrix:=GetViewMatrix;
    ProjMatrix:=GetProjectionMatrix;
    ViewPort:=GetViewPort;
    for i := 0 to Count-1 do begin
      mo:=FMeshList[i];
      if mo.Pickable then begin
         if ProjectPoint(mo.Position, ViewMatrix, ProjMatrix, ViewPort,cv)
         then if PointInRect(Rect,cv)
              then List.add(mo);
      end;
    end;
  if List.Count>0 then Result:=true else Result:=false;
end;

function TVBOMesh.PickObject(Rect: TRect): TVBOMeshObject;
var L:TList;
    i:integer;
    d,mind:single;
    mo:TVBOMeshObject;
    ip:TVector;
    mv:TMatrix;
begin
    L:=TList.Create; Result:=nil; mind:=-1;
    if not PickObject(Rect,L) then exit;
    glGetFloatv(GL_MODELVIEW_MATRIX, @mv);
    for i:=0 to L.Count-1 do begin
        mo:=L[i]; ip:=VectorTransform(mo.Position,mv);
        d:=VectorNorm(ip);
        if (d<mind) or (mind<0) then begin
           mind:=d;result:=mo;end;
    end;
end;

procedure TVBOMesh.ConvertAABBToCorners(const AABB: TAABB;
  var Corners: TAABBCorners);
begin
   setvector(corners[0],aabb.min[0],aabb.max[1],aabb.min[2]);
   setvector(corners[1],aabb.min);
   setvector(corners[2],aabb.max[0],aabb.min[1],aabb.min[2]);
   setvector(corners[3],aabb.max[0],aabb.max[1],aabb.min[2]);

   setvector(corners[4],aabb.min[0],aabb.max[1],aabb.max[2]);
   setvector(corners[5],aabb.min[0],aabb.min[1],aabb.max[2]);
   setvector(corners[6],aabb.max[0],aabb.min[1],aabb.max[2]);
   setvector(corners[7],aabb.max);
end;

procedure TVBOMesh.SortByDistance(List: TList; SortDirection: TSortDirection);
Type TPDI = record p: TVector; d: single; Obj: pointer; end;
var i: integer;
    curr: TVBOMeshObject;
    vm: TMatrix;
    pd: array of TPDI;
    ObjList: TList;
    Temp: TList;
  function CompareDistanceFTB(Item1, Item2: Pointer): Integer;
  begin result:=trunc(TPDI(Item1^).d-TPDI(Item2^).d); end;
  function CompareDistanceBTF(Item1, Item2: Pointer): Integer;
  begin result:=trunc(TPDI(Item2^).d-TPDI(Item1^).d); end;
begin
  if assigned(List) then ObjList:=List else ObjList:=FMeshList;
  if SortDirection=sdNone then exit;
  Temp:=TList.Create; Temp.Count:=ObjList.Count;
  vm:=FViewMatrix;
  setlength(pd,ObjList.Count);
  for i:=0 to ObjList.Count-1 do begin
     curr:=ObjList[i];
     if not curr.WorldMatrixUpdated then curr.UpdateWorldMatrix;
     pd[i].p:=VectorTransform(curr.Position,vm);
     pd[i].d:=VectorNorm(pd[i].p); pd[i].Obj:=curr;
     Temp[i]:=@pd[i];
  end;

  if SortDirection=sdFrontToBack then Temp.Sort(@CompareDistanceFTB);
  if SortDirection=sdBackToFront then Temp.Sort(@CompareDistanceBTF);
  for i:=0 to ObjList.Count-1 do ObjList[i]:=TPDI(Temp[i]^).Obj;
  Temp.Free;
end;

procedure TVBOMesh.DeleteMeshObject(MeshObject: TVBOMeshObject; FreeObject: boolean);
var i:integer;
begin
  i:=FMeshList.IndexOf(MeshObject);
  DeleteMeshObject(i,FreeObject);
  FStructureChanged:=true;
end;

procedure TVBOMesh.DeleteMeshObject(index: integer; FreeObject: boolean);
var MeshObject: TVBOMeshObject;
begin
   MeshObject:=FMeshList[index];
   if FreeObject then FreeAndNil(MeshObject);
   FMeshList.Delete(index);
   FStructureChanged:=true;
end;

function TVBOMesh.WorldToPlane(P: TVector): TVector;
var m: TMatrix;
begin
  m:=MatrixMultiply(FViewMatrix,FProjectionMatrix);
  result:=VectorTransform(P,M);
  if result[3]<>0 then ScaleVector(result,1/result[3]);
end;

function TVBOMesh.Last: TVBOMeshObject;
begin
  result:=FMeshList.Last;
end;

function TVBOMesh.LodSelect(mo: TVBOMeshObject): TVBOMeshObject;
var dist: single;
    m: TMatrix;
    i: integer;
    pl: PLODs;
begin
  result:=mo; if mo.LodList.Count=0 then exit;
  m:=MatrixMultiply(mo.Matrices.WorldMatrix, FViewMatrix);
  dist:=VectorLength(m[3]); i:=0;
  repeat pl:=mo.LodList[i];i:=i+1;
  until (dist<=pl.MaxViewDistance) or (i=mo.LodList.Count);
  if (dist>pl.MaxViewDistance) then pl:=mo.LodList[mo.LodList.Count-1];
  if assigned(pl.LodMesh) then begin
    pl.LodMesh.Matrices.WorldMatrix:=mo.Matrices.WorldMatrix;
    pl.LodMesh.WorldMatrixUpdated:=true;
  end; result:=pl.LodMesh;
end;

procedure TVBOMesh.UpdateSceneMatrix;
begin
   glGetFloatv(GL_MODELVIEW_MATRIX, @FViewMatrix);
   glGetFloatv(GL_PROJECTION_MATRIX, @FProjectionMatrix);
   glGetIntegerv(GL_VIEWPORT, @FViewPort);
end;

constructor TOctreeLod.Create;
begin
  inherited;
  ObjList:=TList.Create;
  BigList:=TList.Create;
end;

destructor TOctreeLod.Destroy;
begin
  ObjList.Free; BigList.Free;
  inherited;
end;

{ TSceneOctree }

function AABBIntersected(const bb1,bb2: TExtents): integer;
var C: array[0..7] of TAffineVector;
    i: integer;
begin
  result:=0;
  setAffinevector(c[0],bb2.emin[0],bb2.emax[1],bb2.emin[2]);
  c[1]:=bb2.emin;
  setAffinevector(c[2],bb2.emax[0],bb2.emin[1],bb2.emin[2]);
  setAffinevector(c[3],bb2.emax[0],bb2.emax[1],bb2.emin[2]);
  setAffinevector(c[4],bb2.emin[0],bb2.emax[1],bb2.emax[2]);
  setAffinevector(c[5],bb2.emin[0],bb2.emin[1],bb2.emax[2]);
  setAffinevector(c[6],bb2.emax[0],bb2.emin[1],bb2.emax[2]);
  c[7]:=bb2.emax;
  for i:=0 to 7 do if PointInAABB(c[i],TAABB(bb1)) then inc(Result);
end;

procedure TSceneOctree.BuildLeaf(var Lod: TOctreeLod; Offs,Size: TAffineVector);
var i,j,k: integer;
    mo: TVBOMeshObject;
    nOffs, hSize: TAffineVector;
    x,y,z,n: integer;
begin
  Lod.Extents.emin:=offs;
  Lod.Extents.emax:=VectorAdd(offs,Size);
  Lod.ObjList.Capacity:=Lod.Parent.ObjList.Count;
  for i:=0 to Lod.Parent.ObjList.Count-1 do begin
    mo:=Lod.Parent.ObjList[i];
//=>>>>
    n:=0;
    for j:=0 to high(Lod.Parent.Lods) do begin
      if AABBIntersected(mo.Extents,Lod.Parent.Lods[j].Extents)>0 then inc(n);
    end;
    if n>1 then Lod.Parent.BigList.Add(mo) else if n=1 then Lod.ObjList.Add(mo);

//=>>>>
{    if PointInAABB(mo.Position,TAABB(Lod.Extents)) then begin
      if AABBIntersected(mo.Extents,Lod.Extents)<>8 then
        Lod.BigList.Add(mo) else Lod.ObjList.Add(mo);
    end;
}
  end;
  if Lod.ObjList.Count=0 then begin Lod.LodsCount:=0; exit; end;

  Lod.Level:=Lod.Parent.Level+1;
  x:=1;y:=1;z:=1;
  if Lod.Level<FMaxXLods then x:=2;
  if Lod.Level<FMaxYLods then y:=2;
  if Lod.Level<FMaxZLods then z:=2;
  if x*y*z<=1 then begin Lod.LodsCount:=0; exit; end;
//  hSize:=VectorScale(Size,0.5);
  hSize:=VectorScale(Size,AffineVectorMake(1/x,1/y,1/z));

  Setlength(Lod.Lods,x*y*z); Lod.LodsCount:=x*y*z;
  for k:=0 to y-1 do for j:=0 to x-1 do for i:=0 to z-1 do begin
    n:=k*x*z+j*z+i; Lod.Lods[n]:=TOctreeLod.Create;
    Lod.Lods[n].Parent:=Lod;
    nOffs:=VectorAdd(Lod.Extents.emin,VectorScale(hSize,AffineVectorMake(j,k,i)));
    BuildLeaf(Lod.Lods[n],nOffs,hsize);
  end;

  mo:=Lod.ObjList[0];
  Lod.Extents:=mo.Extents;
  for i:=1 to Lod.ObjList.Count-1 do begin
    mo:=Lod.ObjList[i];
    AABBInclude(TAABB(Lod.Extents),mo.Extents.emin);
    AABBInclude(TAABB(Lod.Extents),mo.Extents.emax);
  end;
end;

procedure TSceneOctree.BuildOctree(MeshList: TList; XLods, YLods, ZLods: integer);
var i,j,k: integer;
    mo: TVBOMeshObject;
    Size, Offs, hSize: TAffineVector;
    x,y,z,n: integer;
begin
  if (not assigned(MeshList)) or (MeshList.Count=0) then exit;
  FMaxXLods:=XLods; FMaxYLods:=YLods; FMaxZLods:=ZLods;
  FRoot:=TOctreeLod.Create;
  FRoot.ObjList.Assign(MeshList);
  mo:=MeshList[0]; FRoot.Parent:=nil;
  FRoot.Extents:=mo.Extents;
  for i:=1 to MeshList.Count-1 do begin
    mo:=MeshList[i];
    AABBInclude(TAABB(FRoot.Extents),mo.Extents.emin);
    AABBInclude(TAABB(FRoot.Extents),mo.Extents.emax);
  end;
  with Froot.Extents do Size:=VectorSubtract(emax,emin);
  //hSize:=VectorScale(Size,0.5);
  x:=1;y:=1;z:=1; FRoot.Level:=0;
  if XLods>1 then x:=2; if YLods>1 then y:=2; if ZLods>1 then z:=2;
  hSize:=VectorScale(Size,AffineVectorMake(1/x,1/y,1/z));
  Setlength(FRoot.Lods,x*y*z); FRoot.LodsCount:=x*y*z;

  for k:=0 to y-1 do for j:=0 to x-1 do for i:=0 to z-1 do begin
    n:=k*x*z+j*z+i; FRoot.Lods[n]:=TOctreeLod.Create;
    FRoot.Lods[n].Parent:=FRoot;
    offs:=VectorAdd(FRoot.Extents.emin,VectorScale(hSize,AffineVectorMake(j,k,i)));
    BuildLeaf(FRoot.Lods[n],offs,hsize);
  end;
  FBuilded:=true;
end;

procedure TSceneOctree.ByPassLod(var Lod: TOctreeLod);
var i: integer;
    stop: boolean;
begin
  FOctreeCallback(Lod,stop); if stop then exit;
  for i:=0 to Lod.LodsCount-1 do ByPassLod(Lod.Lods[i]);
end;

procedure TSceneOctree.ByPassTree;
begin
  if not assigned(FOctreeCallback) then exit;
  ByPassLod(FRoot);
end;

procedure TSceneOctree.Clear;
var i: integer;
begin
  if not assigned(FRoot) then exit;
  for i:=0 to FRoot.LodsCount-1 do begin
    if assigned(FRoot.Lods[i]) then ClearLeaf(FRoot.Lods[i]);
    FRoot.Lods[i].Free;
  end; FRoot.Free; FRoot:=nil;
end;

procedure TSceneOctree.ClearLeaf(var Leaf: TOctreeLod);
var i: integer;
begin
  for i:=0 to Leaf.LodsCount-1 do begin
    if assigned(Leaf.Lods[i]) then ClearLeaf(Leaf.Lods[i]);
    Leaf.Lods[i].Free;
  end;
  Leaf.Free; Leaf:=nil;
end;

constructor TSceneOctree.Create;
begin
  inherited;
  FBuilded:=false;
end;

destructor TSceneOctree.Destroy;
begin
  Clear;
  inherited;
end;

procedure TSceneOctree.FrustumCulling(var Lod: TOctreeLod;
  const Frustum: TFrustum);
var i: integer;
    mo: TVBOMeshObject;
begin
  if Lod.ObjList.Count=0 then exit;
  if not IsVolumeClipped(Lod.Extents, Frustum) then begin
    //Проверяем видимость больших частей, вне дерева октри
    for i:=0 to Lod.BigList.Count-1 do begin
      mo:=Lod.BigList[i];
      if assigned(mo) then begin
        if mo.Culled then mo.Occluded:=false;
        mo.Culled:=False;
        if (mo.Visible) or (mo.ProxyList.Count>0) then begin
          if assigned(mo.onBeforeCheckVisibility) then mo.onBeforeCheckVisibility(self);
          if not ((mo.MeshType=mtHUDSprite) or (mo.MeshType=mtSphericalSprite) or
                 (mo.MeshType=mtCylindricalSprite))
          then mo.Culled:=IsVolumeClipped(mo.GeomCenter, mo.BaundedRadius, Frustum);
          if mo.ProxyList.Count>0 then mo.Culled:=false;
          if not mo.Culled then FTempList.Add(mo);
        end;
      end;
    end;
    //Если нижний ЛОД - проверяем видимость его мешей
    if Lod.LodsCount=0 then begin
      for i:=0 to Lod.ObjList.Count-1 do begin
        mo:=Lod.ObjList[i];
        if assigned(mo) then begin
          if mo.Culled then mo.Occluded:=false;
          mo.Culled:=False;
          if (mo.Visible) or (mo.ProxyList.Count>0) then begin
            if assigned(mo.onBeforeCheckVisibility) then mo.onBeforeCheckVisibility(self);
            if not ((mo.MeshType=mtHUDSprite) or (mo.MeshType=mtSphericalSprite) or
                   (mo.MeshType=mtCylindricalSprite))
            then mo.Culled:=IsVolumeClipped(mo.GeomCenter, mo.BaundedRadius, Frustum);
            if mo.ProxyList.Count>0 then mo.Culled:=false;
            if not mo.Culled then FTempList.Add(mo);
          end;
        end;
      end;
    end else begin
      for i:=0 to Lod.LodsCount-1 do FrustumCulling(Lod.Lods[i],Frustum);
    end;
  end;
end;

procedure TSceneOctree.GenVisibleList(const Frustum: TFrustum;
  var VisList: TList);
begin
  assert(assigned(VisList),'Result List is not assigned');
  FTempList:=VisList; FTempList.Count:=0;
  FrustumCulling(FRoot,Frustum);
end;


{ TMeshCollection }

function TMeshCollection.AddItem(mi: TVBOMeshItem): integer;
var mo: TVBOMeshObject;
begin
  FStructureChanged:=true; result:=-1;
  if mi is TVBOMeshObject then begin
    mo:=TVBOMeshObject(mi);
    mo.IndexInMesh:=FMeshList.Add(mo);
    mo.MatLib:=FMaterials; mo.TexLib:=FTextures;
    mo.MatObjLib:=FMaterialObjects;
    result:=mo.IndexInMesh; exit;
  end;
  if mi is TMeshCollection then begin
    if not TMeshCollection(mi).FExpand then result:=FMeshList.Add(mi)
    else begin TMeshCollection(mi).Unpack(FMeshList); result:=-1; end;
    exit;
  end;
end;

function TMeshCollection.GetMesh(Index: Integer): TVBOMeshItem;
begin
   result:=FMeshList[Index];
end;

function TMeshCollection.GetCount: integer;
begin
  result:=FMeshList.Count;
end;

procedure TMeshCollection.BuildOctree(level: integer);
var mo:TVBOMeshObject;
    i:integer;
begin
   for i:=0 to FMeshList.Count-1 do begin
      mo:=FMeshList[i];
      with mo do begin
        if not WorldMatrixUpdated then UpdateWorldMatrix;
        BuildOctreeList(Level);
      end;
   end; OctreeBuilded:=true;
end;

constructor TMeshCollection.Create;
begin
  inherited;
  FMeshList:=TList.Create;
  FExtentsBuilded:=false;
  OctreeBuilded:=false;
  Visible:=true;
  FParent:=nil; FOwner:=nil;
  FSortDirection:=sdNone;
  FItemType:=mcCollection;
  FStructureChanged:=false;
  FExpand:=false;
  FPriority:=0;
end;

constructor TMeshCollection.CreateAsChild(aParent: TVBOMeshItem);
begin
  assert((not assigned(aParent.Childe)),'The object already has child');
  Create; FParent:=aParent; FOwner:=aParent; aParent.Childe:=self;
  FProcessChilds:=pcAfter;
end;

function TMeshCollection.GetObjectByName(Name: string): TVBOMeshItem;
var i: integer;
    mo: TVBOMeshObject;
    s: string;
begin
  s:=Uppercase(Name); result:=nil;
  for i:=0 to FMeshList.Count-1 do begin
    mo:=FMeshList[i];
    if Uppercase(mo.Name)=s then begin result:=mo;exit; end;
  end;
end;

procedure TMeshCollection.GetObjectListByType(ObjType: TMeshTypes; var List: TList);
var i:integer;
    mo: TVBOMeshObject;
begin
  if not assigned(List) then List:=TList.Create else List.Clear;
  for i:=0 to FMeshList.Count-1 do begin
    mo:=FMeshList[i];
    if mo.MeshType=ObjType then List.Add(mo);
  end;
end;

procedure TMeshCollection.GetObjectListByTypeSet(ObjTypes: TMeshTypeSet; var List: TList);
var i:integer;
    mo:TVBOMeshObject;
begin
  if not assigned(List) then List:=TList.Create else List.Clear;
  for i:=0 to FMeshList.Count-1 do begin
    mo:=FMeshList[i];
    if mo.MeshType in ObjTypes then List.Add(mo);
  end;
end;

procedure TMeshCollection.GetObjectListWithNameIncluded(Name: String; var List: TList);
var i:integer;
    mo:TVBOMeshObject;
    s:string;
begin
  s:=Uppercase(Name);
  if not assigned(List) then List:=TList.Create else List.Clear;
  for i:=0 to FMeshList.Count-1 do begin
     mo:=FMeshList[i];
     if pos(s, Uppercase(mo.Name))>0 then List.Add(mo);
  end;
end;

function TMeshCollection.AddBox(Width,Height, Depth: single; TilesX, TilesY, TilesZ: integer): TVBOMeshObject;
var mo:TVBOMeshObject;
    Temp,Res:PVBOBuffer;
    wm:TMatrix;
begin
  FExtentsBuilded:=false;
  mo:=TVBOMeshObject.Create;
  New(Res);InitVBOBuff(Res^, GL_TRIANGLE_STRIP, DrawElements);
  Temp:=CreatePlane(Width,Depth,TilesX,TilesZ,nil,false);
  Temp.Vertexes.Translate(affinevectormake(0,Height/2,0));
  AttachBuffer(Temp^,Res^);
  wm:=CreateRotationMatrixZ(Pi);
  Temp.Vertexes.TransformAsPoints(wm);
  Temp.Normals.TransformAsVectors(wm);
  AttachBuffer(Temp^,Res^);
  FreeVBOBuffer(Temp^); Dispose(Temp);
  Temp:=CreatePlane(Height,Depth,TilesY,TilesZ,nil,false);
  wm:=CreateRotationMatrixZ(-Pi/2);
  Temp.Vertexes.TransformAsPoints(wm);
  Temp.Normals.TransformAsVectors(wm);
  Temp.Vertexes.Translate(affinevectormake(Width/2,0,0));
  AttachBuffer(Temp^,Res^);
  wm:=CreateRotationMatrixZ(Pi);
  Temp.Vertexes.TransformAsPoints(wm);
  Temp.Normals.TransformAsVectors(wm);
  AttachBuffer(Temp^,Res^);
  FreeVBOBuffer(Temp^); Dispose(Temp);
  Temp:=CreatePlane(Width,Height,TilesX,TilesY,nil,false);
  wm:=CreateRotationMatrixX(-Pi/2);
  Temp.Vertexes.TransformAsPoints(wm);
  Temp.Normals.TransformAsVectors(wm);
  Temp.Vertexes.Translate(affinevectormake(0,0,-Depth/2));
  AttachBuffer(Temp^,Res^);
  wm:=CreateRotationMatrixX(Pi);
  Temp.Vertexes.TransformAsPoints(wm);
  Temp.Normals.TransformAsVectors(wm);
  AttachBuffer(Temp^,Res^);
  FreeVBOBuffer(Temp^); Dispose(Temp);

  GenVBOBuff(Res^, False); Res.MatName:='';
  if not assigned(FMeshList) then FMeshList:=TList.Create;
  with mo do begin
    MeshType:=mtBox; Parent:=nil;
    MeshList.Add(Res); Visible:=true;
    Name:='VBOBox'+inttostr(FMeshList.Count);
    UpdateExtents; UpdateWorldMatrix; UpdateMaterialList;
  end; mo.IndexInMesh:=FMeshList.Add(mo); Result:=mo;
  mo.MatLib:=FMaterials; mo.TexLib:=FTextures; mo.MatObjLib:=FMaterialObjects;
end;

function TMeshCollection.AddCollection(MeshCollection: TMeshCollection; Expand: boolean): integer;
var i: integer;
begin
  if Expand then begin
    result:=-1;
    for i:=0 to MeshCollection.Count-1 do begin
      AddItem(MeshCollection[i]);
    end;
  end else result:=AddItem(MeshCollection);
  FStructureChanged:=true;
end;

function TMeshCollection.AddNewCollection: TMeshCollection;
begin
  result:=TMeshCollection.Create;
  result.FOwner:=self; FMeshList.Add(result);
  FStructureChanged:=true;
end;

function TMeshCollection.AddNewContainer: TMeshContainer;
begin
  result:=TMeshContainer.Create(TMeshCollection.Create);
  result.FOwner:=self; FMeshList.Add(result);
  FStructureChanged:=true;
end;

function TMeshCollection.AddPlane(Width, Height: single; TilesX, TilesY: integer; HeightFunc: TGetHeightFunc=nil): TVBOMeshObject;
var Temp: PVBOBuffer;
    mo: TVBOMeshObject;
begin
FExtentsBuilded:=false;
mo:=TVBOMeshObject.Create;
Temp:=CreatePlane(Width,Height,TilesX,TilesY,HeightFunc);
  if not assigned(FMeshList) then FMeshList:=TList.Create;
  with mo do begin
    MeshType:=mtPlane; Parent:=nil;
    MeshList.Add(Temp); Visible:=true;
    Name:='VBOPlane'+inttostr(FMeshList.Count);
    UpdateExtents; UpdateWorldMatrix; UpdateMaterialList;
  end; mo.IndexInMesh:=FMeshList.Add(mo); Result:=mo;
  mo.MatLib:=FMaterials; mo.TexLib:=FTextures; mo.MatObjLib:=FMaterialObjects;
end;

function TMeshCollection.AddSphere(Radius: single; VSegments, HSegments: integer;
                  TileS:single=1;TileT:single=1;NormalInside:boolean = false): TVBOMeshObject;
var a,b:single;
  Temp: PVBOBuffer;
  mo: TVBOMeshObject;
  i,j,si,vi:integer;
  da,db,rx,rz,rxz,ry:single;
  ks,kt:single;
  norm:TAffineVector;
begin
  Assert(Vsegments*HSegments<>0,'Segments count must be more than "0".');
  FExtentsBuilded:=false;
  mo:=TVBOMeshObject.Create;
  da:=pi/VSegments; db:=2*pi/(HSegments-1);
  ks:=1/(HSegments-1);kt:=1/(VSegments);
  new(Temp);InitVBOBuff(Temp^,GL_TRIANGLE_STRIP,DrawElements);
  with temp^ do begin
  RenderBuffs:=[uTexCoords,uNormals,uIndices];
  for i:=0 to VSegments do begin
      if NormalInside then a:=pi/2-i*da
      else a:=i*da-pi/2;
      ry:=Radius*sin(a);rxz:=Radius*cos(a);
      si:=Indices.Count;
      for j:=0 to HSegments-1 do begin
          b:=j*db; rx:=rxz*cos(b); rz:=rxz*sin(b);
          vi:=i*(HSegments)+j;
          Vertexes.Add(rx,ry,rz);
          TexCoords.Add(j*ks*TileS,i*kt*TileT);
          norm:=affinevectormake(rx/Radius,ry/Radius,rz/Radius);
          if NormalInside then NegateVector(norm);
          Normals.Add(norm);
          if i<VSegments then
             Indices.Add(vi,vi+HSegments);
      end;
      Indices.Add(Indices[si],Indices[si+1]);
  end;
  Indices.Count:=Indices.Count-2;
  Vertexes.GetExtents(emin, emax); MatName:='';
  end;
  GenVBOBuff(Temp^, False);
  with mo do begin
    MeshList.Add(Temp); MeshType:=mtSphere; Visible:=true;
    Name:='VBOSphere'+inttostr(FMeshList.Count); Parent:=nil;
    UpdateExtents; UpdateWorldMatrix; UpdateMaterialList;
  end;result:=mo; mo.IndexInMesh:=FMeshList.Add(mo);
  mo.MatLib:=FMaterials; mo.TexLib:=FTextures; mo.MatObjLib:=FMaterialObjects;
end;

destructor TMeshCollection.Destroy;
begin
  FreeObjectList(FMeshList);

  FMaterials.Free; FTextures.Free;
  FMaterialObjects.Free;

  inherited;
end;

function TMeshCollection.AddMeshFromFile(FileName, aMatLib: string): TVBOMeshObject;
var fext,tPath: string;
    mo: TVBOMeshObject;
    objfile: TOBJLoader;
    mtl: TGLMTLFile;
begin
  fext:=Uppercase(ExtractFileExt(FileName)); tpath:='';
  assert((fext='.3DS') or (fext='.OBJ'),'Unsupported file type: "'+fext+'"');
  if (aMatLib='') and (fext='.OBJ') then begin
    aMatLib:=ChangeFileExt(FileName,'.MTL');
  end;
  if aMatLib<>'' then begin
    if uppercase(ExtractFileExt(aMatLib))='.MTL' then begin
      mtl:=TGLMTLFile.Create;
      mtl.LoadMaterialLibrary(aMatLib,FMaterialObjects);
      mtl.Free;
    end else tpath:=aMatLib;
  end;

  if fext='.3DS' then begin
    mo:=TAnimatedMesh.Create;
    mo.MatLib:=FMaterials;
    mo.TexLib:=FTextures;
    mo.MatObjLib:=FMaterialObjects;

    TAnimatedMesh(mo).Load3dsMesh(FileName,tpath);
  end else begin
    objfile:=TOBJLoader.Create;
    objfile.LoadFromFile(FileName);
    mo:=TVBOMeshObject.Create;
    FreeAndNil(mo.MeshList);
    mo.MeshList:=objfile.MeshList;
    GenVBOBuffers(mo.MeshList,false);
    mo.MatLib:=FMaterials;
    mo.TexLib:=FTextures;
    mo.MatObjLib:=FMaterialObjects;
    objfile.Free;
  end;

  with mo do begin
    MeshType:=mtFreeForm; Visible:=true;
    Name:='VBOFreeForm'+inttostr(FMeshList.Count); Parent:=nil;
    UpdateExtents; UpdateWorldMatrix; Pickable:=true;
  end; result:=mo;
  mo.IndexInMesh:=FMeshList.Add(mo);
end;

function TMeshCollection.AddScreenQuad(AddToMesh:boolean): TVBOMeshObject;
var Temp: PVBOBuffer;
    mo: TVBOMeshObject;
begin
  mo:=TVBOMeshObject.Create;
  new(Temp);
//  InitVBOBuff(Temp^,GL_QUADS,DrawArrays);
  InitVBOBuff(Temp^,GL_QUADS,DrawElements);
  with temp^ do begin
    RenderBuffs:=[uTexCoords, uIndices];
    Vertexes.Add(-1,-1,0);TexCoords.Add(0,0,0);
    Vertexes.Add( 1,-1,0);TexCoords.Add(1,0,0);
    Vertexes.Add( 1, 1,0);TexCoords.Add(1,1,0);
    Vertexes.Add(-1, 1,0);TexCoords.Add(0,1,0);
    Indices.Add(0,1); Indices.Add(2,3);
  end;
  GenVBOBuff(Temp^, False);
  with mo do begin
    MeshList.Add(Temp); MeshType:=mtScreenQuad; Visible:=true;
    Name:='VBOScreenQuad'+inttostr(FMeshList.Count); Parent:=nil;
    UpdateExtents; UpdateWorldMatrix; UpdateMaterialList;
    Pickable:=false;
  end; result:=mo;
  if AddToMesh then mo.IndexInMesh:=FMeshList.Add(mo)
  else mo.IndexInMesh:=-1;
  mo.MatLib:=FMaterials; mo.TexLib:=FTextures; mo.MatObjLib:=FMaterialObjects;
end;

function TMeshCollection.AddSMDAnimation(SMD: TSkeletalRender): TVBOMeshObject;
var mo: TSkeletalRender;
begin
  mo:=TSkeletalRender.Create;
  mo.Name:='SMDAnimation'+inttostr(FMeshList.Add(mo));
  result:=mo; mo.MatLib:=FMaterials; mo.TexLib:=FTextures; mo.MatObjLib:=FMaterialObjects;
  mo.AddScreenQuad(AddScreenQuad(false));
  with mo do begin
    Assign(SMD);
    UpdateWorldMatrix;
    //Pack Mesh to Texture
    PackMeshesToTexture(vtex,ntex);
    glDeleteBuffers(1,@MultiBuffer[0].Buff.vid);
    glDeleteBuffers(1,@MultiBuffer[0].Buff.nid);
    RotateAroundX(-pi/2); //visible:=false;
    rvtex:=TTexture.Create; //Vertex readback Texture
    rvtex.CreateRGBA32FTexture2D(vtex.Width,vtex.Height);
    rntex:=TTexture.Create; //Vertex readback Texture
    rntex.CreateRGBA32FTexture2D(vtex.Width,vtex.Height);
//    Visible:=false;

    //Setup FBO
    //FMeshList.Delete(FScreenQuad.FIndexInMesh);
    with ScreenQuad do begin
       with FBO do begin
         AttachTexture(rvtex);
         AttachTexture(rntex);
         SetReadBackBuffer([0,1]);
         InitFBO(vtex.Width,vtex.Height);
         Active:=true;
       end;
    end;
    ScreenQuad.visible:=true;
    CreatSMDShader;
  end;
end;

function TMeshCollection.AddHUDSprite(width, height: single): TVBOMeshObject;
var Temp: PVBOBuffer;
    mo: TVBOMeshObject;
    kx,ky:single;
begin
  mo:=TVBOMeshObject.Create;
  new(Temp);InitVBOBuff(Temp^,GL_QUADS,DrawArrays);
  kx:=width/2;ky:=height/2;
  with temp^ do begin
    RenderBuffs:=[uTexCoords,uNormals];
    Vertexes.Add(-kx,-ky,0);TexCoords.Add(0,0,0);Normals.Add(0,0,1);
    Vertexes.Add( kx,-ky,0);TexCoords.Add(1,0,0);Normals.Add(0,0,1);
    Vertexes.Add( kx, ky,0);TexCoords.Add(1,1,0);Normals.Add(0,0,1);
    Vertexes.Add(-kx, ky,0);TexCoords.Add(0,1,0);Normals.Add(0,0,1);
  end;
  GenVBOBuff(Temp^, False);
  with mo do begin
    MeshList.Add(Temp); MeshType:=mtHUDSprite; Visible:=true;
    Name:='VBOHUDSprite'+inttostr(FMeshList.Count); Parent:=nil;
    UpdateExtents; Pickable:=false;
    UpdateWorldMatrix; UpdateMaterialList;
    NoZWrite:=false; NoDepthTest:=false;
  end; result:=mo; mo.IndexInMesh:=FMeshList.Add(mo);
  mo.MatLib:=FMaterials; mo.TexLib:=FTextures; mo.MatObjLib:=FMaterialObjects;
end;

function TMeshCollection.AddSprite(s_type: TSpriteType; width,
  height: single): TVBOMeshObject;
var Temp: PVBOBuffer;
    mo: TVBOMeshObject;
    kx,ky:single;
begin
  mo:=TVBOMeshObject.Create;
  new(Temp);InitVBOBuff(Temp^,GL_QUADS,DrawArrays);
  kx:=width/2;ky:=height/2;
  with temp^ do begin
    RenderBuffs:=[uTexCoords,uNormals];
    Vertexes.Add(-kx,-ky,0);TexCoords.Add(0,0,0);Normals.Add(0,0,1);
    Vertexes.Add( kx,-ky,0);TexCoords.Add(1,0,0);Normals.Add(0,0,1);
    Vertexes.Add( kx, ky,0);TexCoords.Add(1,1,0);Normals.Add(0,0,1);
    Vertexes.Add(-kx, ky,0);TexCoords.Add(0,1,0);Normals.Add(0,0,1);
  end;
  GenVBOBuff(Temp^, False);
  with mo do begin
    case s_type of
      stSpherical:  MeshType:=mtSphericalSprite;
      stCylindrical: MeshType:=mtCylindricalSprite;
    Else MeshType:=mtSphericalSprite;
    end;
    MeshList.Add(Temp); Visible:=true;
    Name:='VBOSprite'+inttostr(FMeshList.Count); Parent:=nil;
    UpdateExtents;
    Pickable:=false;
    UpdateWorldMatrix; UpdateMaterialList;
  end; result:=mo; mo.IndexInMesh:=FMeshList.Add(mo);
  mo.MatLib:=FMaterials; mo.TexLib:=FTextures; mo.MatObjLib:=FMaterialObjects;
end;

function TMeshCollection.ExtentsIntersect(const rayStart, rayVector: TVector; var List:Tlist): boolean;
var i:integer;
    mo:TVBOMeshObject;
    f:boolean;
    ri:PRaycastIntersectInfo;
    ip:TVector;
begin
   if not assigned(List) then List:=TList.Create;// else List.Clear;
   result:=false;
   for i:=0 to FMeshList.Count-1 do begin
       mo:=FMeshList[i];
       if mo.Visible and mo.Pickable then begin
         f:=mo.ExtentsIntersect(rayStart, rayVector,@ip);
         if f then begin new(ri);
            with ri^ do begin
              iPoint:=ip;iNormal:=RayVector;
              NegateVector(iNormal);
              PickedObject:=mo; ObjectIndex:=i;
              ObjMeshIndexList:=TIntegerList.Create;
              ObjMeshIndexList.Add(i);
            end;
            List.Add(ri);
         end;
       end;
   end; if List.Count>0 then result:=true;
end;

function TMeshCollection.OctreeRayCastIntersect(const rayStart, rayVector: TVector; var List:TList):boolean;
var i:integer;
    mo:TVBOMeshObject;
    iPoint, iNormal: TVector;
begin
   if not assigned(List) then List:=TList.Create else List.Clear;
   result:=false;
   for i:=0 to FMeshList.Count-1 do begin
       mo:=FMeshList[i];
       if mo.Visible and mo.Pickable then begin
          mo.OctreeRayCastIntersect(rayStart, rayVector, list, @iPoint, @iNormal);
       end;
   end; if list.Count>0 then result:=true;
end;

procedure TMeshCollection.Process;
begin
  inherited;
end;

function TMeshCollection.AddBBox(Corners: THmgBoundingBox; Color: TVector): TVBOMeshObject;
const Reindex:array[0..7] of byte = (7,6,2,3,4,5,1,0);
var Temp: PVBOBuffer;
    mo: TVBOMeshObject;
begin
  mo:=TVBOMeshObject.Create;
  new(Temp);InitVBOBuff(Temp^,GL_LINE_STRIP,DrawArrays);
  with temp^ do begin
    RenderBuffs:=[uColors];
    Vertexes.Add(Corners[Reindex[0]]);Colors.Add(Color);
    Vertexes.Add(Corners[Reindex[1]]);Colors.Add(Color);
    Vertexes.Add(Corners[Reindex[2]]);Colors.Add(Color);
    Vertexes.Add(Corners[Reindex[3]]);Colors.Add(Color);
    Vertexes.Add(Corners[Reindex[0]]);Colors.Add(Color);
    Vertexes.Add(Corners[Reindex[4]]);Colors.Add(Color);
    Vertexes.Add(Corners[Reindex[5]]);Colors.Add(Color);
    Vertexes.Add(Corners[Reindex[1]]);Colors.Add(Color);
    Vertexes.Add(Corners[Reindex[2]]);Colors.Add(Color);
    Vertexes.Add(Corners[Reindex[6]]);Colors.Add(Color);
    Vertexes.Add(Corners[Reindex[5]]);Colors.Add(Color);
    Vertexes.Add(Corners[Reindex[4]]);Colors.Add(Color);
    Vertexes.Add(Corners[Reindex[7]]);Colors.Add(Color);
    Vertexes.Add(Corners[Reindex[6]]);Colors.Add(Color);
    Vertexes.Add(Corners[Reindex[2]]);Colors.Add(Color);
    Vertexes.Add(Corners[Reindex[3]]);Colors.Add(Color);
    Vertexes.Add(Corners[Reindex[7]]);Colors.Add(Color);
  end;
  GenVBOBuff(Temp^, False);
  with mo do begin
    MeshList.Add(Temp); MeshType:=mtBBox; Visible:=true;
    Name:='VBOBoundingBox'+inttostr(FMeshList.Count); Parent:=nil;
    UpdateExtents;
    Pickable:=false;
    UpdateWorldMatrix; UpdateMaterialList;
  end; result:=mo; mo.IndexInMesh:=FMeshList.Add(mo);
  mo.MatLib:=FMaterials; mo.TexLib:=FTextures; mo.MatObjLib:=FMaterialObjects;
end;

function TMeshCollection.AddAnimatedSprite(s_type: TSpriteType; width,
  height: single): TVBOMeshObject;
var Temp: PVBOBuffer;
    mo: TVBOMeshObject;
    kx,ky:single;
begin
  mo:=TVBOAnimatedSprite.Create;
  new(Temp);InitVBOBuff(Temp^,GL_QUADS,DrawArrays);
  kx:=2/width;ky:=2/height;
  with temp^ do begin
    RenderBuffs:=[uTexCoords, uNormals];
    Vertexes.Add(-kx,-ky,0);TexCoords.Add(0,0,0);Normals.Add(0,0,1);
    Vertexes.Add( kx,-ky,0);TexCoords.Add(1,0,0);Normals.Add(0,0,1);
    Vertexes.Add( kx, ky,0);TexCoords.Add(1,1,0);Normals.Add(0,0,1);
    Vertexes.Add(-kx, ky,0);TexCoords.Add(0,1,0);Normals.Add(0,0,1);
  end;
  GenVBOBuff(Temp^, False);
  with mo do begin
    case s_type of
      stSpherical:  MeshType:=mtSphericalSprite;
      stCylindrical: MeshType:=mtCylindricalSprite;
    Else MeshType:=mtSphericalSprite;
    end;
    MeshList.Add(Temp); Visible:=true;
    Name:='VBOAnimatedSprite'+inttostr(FMeshList.Count); Parent:=nil;
    UpdateExtents;
    UpdateWorldMatrix; UpdateMaterialList;
    Pickable:=false;
  end; result:=mo; mo.IndexInMesh:=FMeshList.Add(mo);
  mo.MatLib:=FMaterials; mo.TexLib:=FTextures; mo.MatObjLib:=FMaterialObjects;
end;

function TMeshCollection.AddBBox(Extents: TExtents; Color: TVector): TVBOMeshObject;
var AABB:TAABB;
    Coners: THmgBoundingBox;
begin
   AABB.min:=Extents.emin;
   AABB.max:=Extents.emax;
   Coners:=AABBToBB(aabb);
   result:=AddBBox(Coners,Color);
end;

function TMeshCollection.AddPoints(var Points: TAffineVectorArray;
 var Param: TPointParam; Colors: PVectorArray=nil): TVBOMeshObject;
var Temp: PVBOBuffer;
    mo: TVBOMeshObject;
    i:integer;
    p:PPointParam;
begin
  mo:=TVBOMeshObject.Create;
  if Colors<>nil then assert(high(Colors^)=high(Points),'Sizes of Colors and Points arrays are not equal.');
  new(Temp);InitVBOBuff(Temp^,GL_POINTS,DrawArrays);
  Temp^.RenderBuffs:=[];
  if Colors<>nil then Temp^.RenderBuffs:=Temp^.RenderBuffs+[uColors];
  for i:=0 to high(Points) do begin
      Temp^.Vertexes.Add(Points[i]);
      if Colors<>nil then Temp^.Colors.Add(Colors^[i]);
  end;
  GenVBOBuff(Temp^, False);
  with mo do begin
    new(p);p^:=Param;
    Params:=p;
    MeshList.Add(Temp);
    MeshType:=mtPoints; Visible:=true;
    Name:='VBOPoints'+inttostr(FMeshList.Count); Parent:=nil;
    UpdateExtents;
    Pickable:=false;
    UpdateWorldMatrix; UpdateMaterialList;
  end; result:=mo; mo.IndexInMesh:=FMeshList.Add(mo);
  mo.MatLib:=FMaterials; mo.TexLib:=FTextures; mo.MatObjLib:=FMaterialObjects;
end;

function TMeshCollection.AddPoints(PointsList:TAffineVectorList; ColorsList:
  TVectorList; var Param: TPointParam): TVBOMeshObject;
var Temp: PVBOBuffer;
    mo: TVBOMeshObject;
    p:PPointParam;
begin
  mo:=TVBOMeshObject.Create;
  if assigned(ColorsList) and assigned(PointsList)
  then assert(ColorsList.Count=PointsList.Count,'Sizes of Colors and Points arrays are not equal.');
  if not assigned(PointsList) then begin
     result:=nil;exit;end;
  new(Temp);InitVBOBuff(Temp^,GL_POINTS,DrawArrays);
  with Temp^ do begin
    Vertexes.Assign(PointsList);
    if assigned(ColorsList) then begin
      Colors.Assign(ColorsList);
      RenderBuffs:=[uColors];
    end else RenderBuffs:=[];
  end;
  GenVBOBuff(Temp^, False);
  with mo do begin
    new(p);p^:=Param;
    Params:=p;
    MeshList.Add(Temp); MeshType:=mtPoints; Visible:=true;
    Name:='VBOPoints'+inttostr(FMeshList.Count); Parent:=nil;
    UpdateExtents;
    Pickable:=false;
    UpdateWorldMatrix; UpdateMaterialList;
  end; result:=mo; mo.IndexInMesh:=FMeshList.Add(mo);
  mo.MatLib:=FMaterials; mo.TexLib:=FTextures; mo.MatObjLib:=FMaterialObjects;
end;

function TMeshCollection.AddGrid(Width, Height: single; TilesX, TilesY: integer;
  Color: PVector): TVBOMeshObject;
var Temp: PVBOBuffer;
    mo: TVBOMeshObject;
    p1,p2:TAffineVector;
    i:integer;
    sx,sy:single;
    wd2,hd2:single;
begin
  mo:=TVBOMeshObject.Create;
  new(Temp);InitVBOBuff(Temp^,GL_LINES,DrawArrays);
  sx:=Width/TilesX; sy:=Height/TilesY;
  wd2:=width/2; hd2:=height/2;
  with temp^ do begin
    if color<>nil then RenderBuffs:=[uColors] else RenderBuffs:=[];
    for i:=0 to TilesY do begin
      setvector(p1,-wd2,0,i*sy-hd2);
      setvector(p2, wd2,0,i*sy-hd2);
      Vertexes.Add(p1,p2);
      if Colors<>nil then begin
         Colors.Add(Color^);Colors.Add(Color^);end;
    end;
    for i:=0 to TilesX do begin
      setvector(p1,i*sx-wd2,0,-hd2);
      setvector(p2,i*sx-wd2,0, hd2);
      Vertexes.Add(p1,p2);
      if Colors<>nil then begin
         Colors.Add(Color^);Colors.Add(Color^);end;
    end;
  end;
  GenVBOBuff(Temp^, False);
  with mo do begin
    MeshList.Add(Temp); MeshType:=mtGrid; Visible:=true;
    Name:='VBOGrid'+inttostr(FMeshList.Count); Parent:=nil;
    UpdateExtents;
    UpdateWorldMatrix; UpdateMaterialList;
    IgnoreOcclusion:=true;
  end; result:=mo; mo.IndexInMesh:=FMeshList.Add(mo);
  mo.MatLib:=FMaterials; mo.TexLib:=FTextures; mo.MatObjLib:=FMaterialObjects;
end;

function TMeshCollection.AddInstanceToObject(MasterObject: TVBOMeshObject): TVBOMeshObject;
var mo: TVBOMeshObject;
begin
  mo:=TVBOMeshObject.Create;
  with mo do begin
    OctreeList.Free; MeshList.Free; LodList.Free;
    MeshType:=mtInstance;
    Visible:=true;
    Name:='VBOProxy'+inttostr(FMeshList.Count);
    Params:=MasterObject;
    MeshList:=MasterObject.MeshList;
    OctreeList:=MasterObject.OctreeList;
    BaseExtents:=MasterObject.BaseExtents;
    UpdateWorldMatrix; UpdateMaterialList;
  end;
  MasterObject.ProxyList.Add(mo);
  result:=mo; mo.IndexInMesh:=FMeshList.Add(mo);
  mo.MatLib:=FMaterials; mo.TexLib:=FTextures; mo.MatObjLib:=FMaterialObjects;
end;

function TMeshCollection.AddProxyObject(MasterObject: TVBOMeshObject): TVBOMeshObject;
var mo: TVBOMeshObject;
begin
  mo:=TVBOMeshObject.Create;
  with mo do begin
    OctreeList.Free; MeshList.Free; LodList.Free;
//    MeshType:=mtProxy;
    MeshType:=MasterObject.MeshType;
    Visible:=true;
    Name:='VBOProxy'+inttostr(FMeshList.Count);
    Params:=MasterObject;
    MeshList:=MasterObject.MeshList;
    OctreeList:=MasterObject.OctreeList;
    LodList:=MasterObject.LodList;
    UseLods:=MasterObject.UseLods;
    BaseExtents:=MasterObject.BaseExtents;
    UpdateWorldMatrix; UpdateMaterialList;
  end;
  result:=mo; mo.IndexInMesh:=FMeshList.Add(mo);
  mo.MatLib:=FMaterials; mo.TexLib:=FTextures;
  mo.MatObjLib:=MasterObject.MatObjLib;
end;

function TMeshCollection.AddUserObject(Name: string;
  VBOMeshList: TList): TVBOMeshObject;
var mo: TVBOMeshObject;
begin
  mo:=TVBOMeshObject.Create;
  if name='' then Name:='VBOUserObject';
  if (GetObjectByName(name)=nil) then mo.Name:=Name
  else mo.Name:=Name+inttostr(FMeshList.Count);
  with mo do begin
    MeshType:=mtUser; UpdateExtents;
    if Assigned(VBOMeshList) then MeshList.Assign(VBOMeshList);
    Visible:=true;
    UpdateWorldMatrix; UpdateMaterialList;
  end;
  result:=mo; mo.IndexInMesh:=FMeshList.Add(mo);
  mo.MatLib:=FMaterials; mo.TexLib:=FTextures;
  mo.MatObjLib:=FMaterialObjects;
end;

function TMeshCollection.AddUniformSMD(MeshFile: string;
  const AnimFiles: array of string): TUniformSMDRender;
var mo: TUniformSMDRender;
    i: integer;
    path,s,t: string;
    Tex: TTexture;
    mat: TMaterialObject;
begin
  assert(MeshFile<>'','Need Mesh');
  mo:=TUniformSMDRender.Create;
  mo.Name:='UniformSMDAnimation'+inttostr(FMeshList.Add(mo));
  result:=mo; mo.MatLib:=FMaterials; mo.TexLib:=FTextures;
  with mo do begin
    //Load Mesh File
    Anim.Mesh:=SMDLoad(MeshFile);
    BonesCount:=Anim.Mesh.NodesCount;
    AnimationName:=''; AnimationNum:=-1;
    //Load Animation files
    for i:=0 to high(AnimFiles) do AddAnimation(Anim^,AnimFiles[i]);
    //Create Mesh from SMD
    GetMeshFormSMD(Anim.Mesh,MeshList,true);
    UpdateExtents; UpdateWorldMatrix;
    NodeRadius:=BaundedRadius/10;
    //Load Materials
    MatObjLib:=FMaterialObjects;
    path:=ExtractFilePath(MeshFile);
    if path<>'' then if path[length(path)]<>'\' then path:=path+'\';
    for i:=0 to mo.Anim.Mesh.Mesh.Textures.Count-1 do begin
       s:=mo.Anim.Mesh.Mesh.Textures[i];
       t:=s; delete(t,length(t)-4,4);
       Tex:=TTexture.CreateFromFile(path+s);
       Tex.Name:=t; FTextures.Add(Tex);
       mat:=TMaterialObject.Create;
       mat.AttachTexture(tex);
       mat.Name:=t;
       MatObjLib.Add(mat);
    end;
    RotateAroundX(-pi/2);
    //Disable Frame interpolation
    Smoothed:=false;
    //Create Shader
    CreatSMDShader;
  end;
end;

function TMeshCollection.AddUniformSMD(SMD: TUniformSMDRender): TUniformSMDRender;
var mo: TUniformSMDRender;
begin
  mo:=TUniformSMDRender.Create;
  mo.MeshType:=mtActorProxy;
  mo.Anim.Animations.Free; dispose(mo.Anim);
  mo.Name:='UniformSMDAnimationProxy'+inttostr(FMeshList.Add(mo));
  result:=mo; mo.MatLib:=FMaterials;
  mo.TexLib:=FTextures; mo.MatObjLib:=FMaterialObjects;
  with mo do begin
    Anim:=SMD.Anim;
    BonesCount:=Anim.Mesh.NodesCount;
    MeshList.Assign(SMD.MeshList);
    BaseExtents:=SMD.BaseExtents;
    UpdateWorldMatrix;
    RotateAroundX(-pi/2);
    //Disable Frame interpolation
    Smoothed:=false;
    //Create Shader
    CreatSMDShader;
  end;
end;

function TMeshCollection.AddUserObject(Name: string;
  VBOBuffer: PVBOBuffer): TVBOMeshObject;
var mo:TVBOMeshObject;
begin
  mo:=TVBOMeshObject.Create;
  if name='' then Name:='VBOUserObject';
  if (GetObjectByName(name)=nil) then mo.Name:=Name
  else mo.Name:=Name+inttostr(FMeshList.Count);
  with mo do begin
    MeshType:=mtUser;
    MeshList.Add(VBOBuffer);
    UpdateExtents;
    Visible:=true; Parent:=nil;
    UpdateWorldMatrix; UpdateMaterialList;
  end;
  VBOBuffer.MaterialFunc:=mo.MaterialSetter;
  result:=mo; mo.IndexInMesh:=FMeshList.Add(mo);
  mo.MatLib:=FMaterials; mo.TexLib:=FTextures;
  mo.MatObjLib:=FMaterialObjects;
end;

procedure TMeshCollection.Clear(FreeObjects: boolean);
var i: integer;
    mo: TVBOMeshItem;
begin
  if not FreeObjects then FMeshList.Clear else begin
    for i:=0 to FMeshList.Count-1 do begin
      mo:=FMeshList[i]; FreeAndNil(mo);
    end; FMeshList.Clear;
  end;
  FStructureChanged:=true;
end;

procedure TMeshCollection.SortByDistance(const ViewMatrix: TMatrix; List: TList;
  SortDirection: TSortDirection);
Type TPDI = record p: TVector; d: single; Obj: pointer; end;
var i: integer;
    curr: TVBOMeshObject;
    pd: array of TPDI;
    ObjList: TList;
    Temp: TList;
  function CompareDistanceFTB(Item1, Item2: Pointer): Integer;
  begin result:=trunc(TPDI(Item1^).d-TPDI(Item2^).d); end;
  function CompareDistanceBTF(Item1, Item2: Pointer): Integer;
  begin result:=trunc(TPDI(Item2^).d-TPDI(Item1^).d); end;
begin
  if assigned(List) then ObjList:=List else ObjList:=FMeshList;
  if SortDirection=sdNone then exit;
  Temp:=TList.Create; Temp.Count:=ObjList.Count;
  setlength(pd,ObjList.Count);
  for i:=0 to ObjList.Count-1 do begin
     curr:=ObjList[i];
     if not curr.WorldMatrixUpdated then curr.UpdateWorldMatrix;
     pd[i].p:=VectorTransform(curr.Position,ViewMatrix);
     pd[i].d:=VectorNorm(pd[i].p); pd[i].Obj:=curr;
     Temp[i]:=@pd[i];
  end;

  if SortDirection=sdFrontToBack then Temp.Sort(@CompareDistanceFTB);
  if SortDirection=sdBackToFront then Temp.Sort(@CompareDistanceBTF);
  for i:=0 to ObjList.Count-1 do ObjList[i]:=TPDI(Temp[i]^).Obj;
  Temp.Free;
end;

procedure TMeshCollection.Unpack(ParentList: TList);
var i: integer;
    mi: TVBOMeshItem;
begin
  i:=0;
  repeat
    mi:=FMeshList[i];
    if mi is TMeshCollection then begin
      if TMeshCollection(mi).ExpandingRequired
      then TMeshCollection(mi).Unpack(FMeshList);
    end else ParentList.Add(mi);
    inc(i);
  until i=Count;
end;

procedure TMeshCollection.DeleteMeshObject(MeshObject: TVBOMeshItem; FreeObject: boolean);
var i:integer;
begin
  i:=FMeshList.IndexOf(MeshObject);
  DeleteMeshObject(i,FreeObject);
end;

procedure TMeshCollection.DeleteMeshObject(index: integer; FreeObject: boolean);
var MeshObject: TVBOMeshObject;
begin
   MeshObject:=FMeshList[index];
   if FreeObject then FreeAndNil(MeshObject);
   FMeshList.Delete(index);
end;

function TMeshCollection.AddParticles(MaxCapacity: integer): TVBOMeshObject;
var mo:TVBOParticles;
begin
  mo:=TVBOParticles.Create(MaxCapacity);
  with mo do begin
    Name:='VBOParticles'+inttostr(FMeshList.Count);
    Parent:=nil;
    UpdateWorldMatrix; UpdateMaterialList;
  end;
  result:=mo; mo.IndexInMesh:=FMeshList.Add(mo);
  mo.MatLib:=FMaterials; mo.TexLib:=FTextures;
  mo.MatObjLib:=FMaterialObjects;
end;

function TMeshCollection.AddMeshObject(mo: TVBOMeshObject): integer;
begin
  result:=FMeshList.Add(mo);
  mo.UpdateWorldMatrix; mo.UpdateMaterialList;
  mo.MatLib:=FMaterials; mo.TexLib:=FTextures; mo.MatObjLib:=FMaterialObjects;
end;

function TMeshCollection.AddSMDAnimation(MeshFile: string;
  const AnimFiles: array of string): TVBOMeshObject;
var mo: TSkeletalRender;
    i: integer;
    path,s,t: string;
    Tex: TTexture;
begin
  assert(MeshFile<>'','Need Mesh');
  mo:=TSkeletalRender.Create;
  mo.Name:='SMDAnimation'+inttostr(FMeshList.Add(mo));
  result:=mo; mo.MatLib:=FMaterials; mo.TexLib:=FTextures; mo.MatObjLib:=FMaterialObjects;
  mo.AddScreenQuad(AddScreenQuad(false));
  with mo do begin
    //Load Mesh File
    Anim.Mesh:=SMDLoad(MeshFile);
    AnimationName:=''; AnimationNum:=-1;
    //Load Animation files
    for i:=0 to high(AnimFiles) do AddAnimation(Anim^,AnimFiles[i]);
    //Create Mesh from SMD
    GetMeshFormSMD(Anim.Mesh,MeshList,true);
    UpdateExtents;
    //Pack Animations to Texture
    Anim.TextureId:=GetTextureFromAnim(Anim^);
    //Load Materials
    MatObjLib:=FMaterialObjects;
    UpdateWorldMatrix;
    path:=ExtractFilePath(MeshFile);
    if path<>'' then if path[length(path)]<>'\' then path:=path+'\';
    for i:=0 to mo.Anim.Mesh.Mesh.Textures.Count-1 do begin
       s:=mo.Anim.Mesh.Mesh.Textures[i];
       t:=s; delete(t,length(t)-4,4);
       Tex:=TTexture.CreateFromFile(path+s);
       Tex.Name:=t; FTextures.Add(Tex);
//       MaterialLibrary.AddTextureMaterial(t,path+s).Material.Texture.Enabled:=true;
    end;
    //Pack Mesh to Texture
    PackMeshesToTexture(vtex,ntex);
    glDeleteBuffers(1,@MultiBuffer[0].Buff.vid);
    glDeleteBuffers(1,@MultiBuffer[0].Buff.nid);
    RotateAroundX(-pi/2); //visible:=false;
    rvtex:=TTexture.Create; //Vertex readback Texture
    rvtex.CreateRGBA32FTexture2D(vtex.Width,vtex.Height);
    rntex:=TTexture.Create; //Vertex readback Texture
    rntex.CreateRGBA32FTexture2D(vtex.Width,vtex.Height);
//    Visible:=false;

    //Setup FBO
//TODO: Разобраться почему прямой рендеринг скринквада
//оказывается медленее рендеринга квада как объекта
//    FMeshList.Delete(FScreenQuad.FIndexInMesh);
    with ScreenQuad do begin
       with FBO do begin
         AttachTexture(rvtex);
         AttachTexture(rntex);
         SetReadBackBuffer([0,1]);
         InitFBO(vtex.Width,vtex.Height);
         Active:=true;
       end;
    end;
    ScreenQuad.visible:=true;
    //Create Shader
    CreatSMDShader;
  end;
end;

function TMeshCollection.Last: TVBOMeshItem;
begin
  result:=FMeshList.Last;
end;

{ TGLSceneMeshAdapter }

procedure TGLSceneMeshAdapter.DoRender(var ARci: TRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
begin
  if assigned(VBOMesh) then VBOMesh.DoRender;
end;

{ TSceneParser }

procedure TSceneParser.Clear;
begin
  FDiffItems.Clear;
  FContainers.Clear;
  FOpaqueMeshObjects.Clear;
  FTransparencyMeshObjects.Clear;
end;

constructor TSceneParser.Create;
begin
  inherited;
  FDiffItems:=TList.Create;
  FContainers:=TList.Create;
  FOpaqueMeshObjects:=TList.Create;
  FTransparencyMeshObjects:=TList.Create;
  FProxyObjects:=TList.Create;
  FLowPriorityIndex:=-1;
end;

destructor TSceneParser.Destroy;
begin
  FDiffItems.Free;
  FContainers.Free;
  FOpaqueMeshObjects.Free;
  FTransparencyMeshObjects.Free;
  FProxyObjects.Free;
  inherited;
end;

procedure TSceneParser.ParseCollection(MeshCollection: TMeshCollection;
  ExpandContainer: boolean);
var i,j: integer;
    mi: TVBOMeshItem;
    mc1,mc2: TMeshCollection;
begin
  clear;
  for i:=0 to MeshCollection.Count-1 do begin
    mi:=MeshCollection[i];
    case mi.MeshItemType of
      mcCollection,mcContainer: FContainers.Add(mi);
      mcMeshObject: begin
        if TVBOMeshObject(mi).MeshType=mtProxy then FProxyObjects.Add(mi)
        else if TVBOMeshObject(mi).MaterialObject.IsTransparency
        then FTransparencyMeshObjects.Add(mi)
        else FOpaqueMeshObjects.Add(mi);
      end;
      else FDiffItems.Add(mi);
    end;
  end;

  //Sort collections by Priority
  FLowPriorityIndex:=-1;
  for i:=0 to FContainers.Count-2 do begin
    mc1:=FContainers[i];
    for j:=i+1 to FContainers.Count-1 do begin
      mc2:=FContainers[j];
      if mc1.Priority>mc2.Priority then begin
        FContainers.Exchange(i,j); mc1:=mc2;
      end;
    end;
    if (mc1.Priority>0) and (FLowPriorityIndex=-1) then FLowPriorityIndex:=i;
  end;

end;

{ TGLRender }

class function TGLRender.GetVisibleObjects(MeshList: TList; const Frustum: TFrustum): TList;
var mo: TVBOMeshObject;
    i: integer;
    QueryObjectList: TList;
begin
  QueryObjectList:=TList.Create;
  QueryObjectList.Capacity:=MeshList.Count*2;
  QueryObjectList.Count:=0;
  for i:=0 to MeshList.Count-1 do begin
    mo:=MeshList[i];
    if assigned(mo) and (mo.MeshItemType=mcMeshObject) then begin
      if mo.Culled then mo.Occluded:=false; mo.Culled:=False;
      if (mo.Visible) or (mo.ProxyList.Count>0) then begin
        if assigned(mo.onBeforeCheckVisibility) then mo.onBeforeCheckVisibility(nil);
        if not ((mo.MeshType=mtHUDSprite) or (mo.MeshType=mtSphericalSprite) or
               (mo.MeshType=mtCylindricalSprite))
        then begin
          if (mo.MeshType=mtActor) or (mo.MeshType=mtActorProxy) then
            mo.Culled:=TUniformSMDRender(mo).CheckVisibility(Frustum)
          else mo.Culled:=IsVolumeClipped(mo.GeomCenter, mo.BaundedRadius, Frustum);
        end;
        if mo.ProxyList.Count>0 then mo.Culled:=false;
        if (not mo.Culled) and (mo.MeshType<>mtInstance) then QueryObjectList.Add(mo);
      end;
    end;
  end;
  result:=QueryObjectList;
end;

class procedure TGLRender.RenderMeshList(const Frustum: TFrustum; List: TList);
var i:integer;
    VisList: TList;
begin
  VisList:=GetVisibleObjects(List,Frustum);
  for i:=0 to VisList.Count-1 do begin

  end;
  VisList.Free;
end;


{ TRenderShell }

procedure TRenderShell.AttachTexture(aTexture: TTexture; aTarget: TMRTTarget);
var i,n: integer;
begin
  n:=length(FAttachments);
  for i:=0 to n-1 do
    assert(FAttachments[i].TargetTo<>aTarget,
      'Texture with this target already attached.');
  setlength(FAttachments,n+1);
  FAttachments[n].Texture:=aTexture;
  FAttachments[n].TargetTo:=aTarget;
end;

constructor TRenderShell.Create(aParent: TMeshCollection);
begin
  inherited Create;
  FSceneViewer.ViewMatrix:=IdentityHmgMatrix;
  FSceneViewer.ProjectionMatrix:=IdentityHmgMatrix;
  FSceneViewer.Frustum:=GetFrustum(FSceneViewer.ProjectionMatrix,FSceneViewer.ViewMatrix);
  setlength(FAttachments,0);
  FSceneViewer.ViewPort[0]:=0; FSceneViewer.ViewPort[1]:=0;
  FSceneViewer.ViewPort[2]:=1; FSceneViewer.ViewPort[3]:=1;
  FSceneViewer.CurrentTime:=-1;
  FRenderBuffer:=rtNone;
  FFBO:=TFrameBufferObject.Create;
  FCollection:=aParent;
  FActive:=false;
  FSceneParser:=TSceneParser.Create;
  FTextures:=TTextureLibrary.Create;
  FLastTime:=-1;
end;

destructor TRenderShell.Destroy;
begin
  FFBO.Free; FSceneParser.Free; FTextures.Free;
  inherited;
end;

function TRenderShell.getAttachments(index: integer): TFBOTarget;
begin
  assert((index>=0) and (index<length(FAttachments)),'Not in FBO attachments');
  result:=FAttachments[index];
end;

function TRenderShell.GetVisibleObjects(MeshList: TList;
  const Frustum: TFrustum): TList;
var mo: TVBOMeshObject;
    i: integer;
    QueryObjectList: TList;
begin
  QueryObjectList:=TList.Create;
  QueryObjectList.Capacity:=MeshList.Count*2;
  QueryObjectList.Count:=0;
  for i:=0 to MeshList.Count-1 do begin
    mo:=MeshList[i];
    if assigned(mo) and (mo.MeshItemType=mcMeshObject) then begin
      if mo.Culled then mo.Occluded:=false; mo.Culled:=False;
      if (mo.Visible) or (mo.ProxyList.Count>0) then begin
        if assigned(mo.onBeforeCheckVisibility) then mo.onBeforeCheckVisibility(nil);
        if not ((mo.MeshType=mtHUDSprite) or (mo.MeshType=mtSphericalSprite) or
               (mo.MeshType=mtCylindricalSprite))
        then begin
          if (mo.MeshType=mtActor) or (mo.MeshType=mtActorProxy) then
            mo.Culled:=TUniformSMDRender(mo).CheckVisibility(Frustum)
          else mo.Culled:=IsVolumeClipped(mo.GeomCenter, mo.BaundedRadius, Frustum);
        end;
        if mo.ProxyList.Count>0 then mo.Culled:=false;
        if (not mo.Culled) and (mo.MeshType<>mtInstance) then QueryObjectList.Add(mo);
      end;
    end;
  end;
  result:=QueryObjectList;
end;

procedure TRenderShell.Process;
begin
  if FCollection.FStructureChanged then begin
    FSceneParser.ParseCollection(FCollection);
    FCollection.FStructureChanged:=false;
  end;
  FSceneViewer.CurrentTime:=GetTime;

  //Process containers before mesh objects
  ProcessCollection(true);
  if FRenderBuffer=rtFrameBuffer then
  with FSceneViewer do begin
    //glViewport(0,0,ViewPort[2],ViewPort[3]);
    FFBO.InitFBO(ViewPort[2],ViewPort[3]);
    UpdateRenderTarget;
    FFBO.Apply;
  end;

  //Rendering opaque mesh objects
  ProcessOpaque;
  //Rendering proxy objects
  ProcessProxys;
  //Rendering transparency objects
  ProcessTransparency;
  //Process containers after mesh objects
  ProcessCollection(false);
  //Process different mesh items
  ProcessDiffObjects;

  if FRenderBuffer=rtFrameBuffer then FFBO.UnApply;
end;

procedure TRenderShell.ProcessCollection(Before: boolean);
var mc: TMeshCollection;
    i,m,n: integer;
begin
  if Before then begin
    m:=0;
    if FSceneParser.LowPriorityIndex>=0 then
      n:=FSceneParser.LowPriorityIndex-1
    else n:=FSceneParser.FContainers.Count;
  end else begin
    m:=FSceneParser.LowPriorityIndex;
    if m<0 then m:=FSceneParser.FContainers.Count;
    n:=FSceneParser.FContainers.Count;
  end;
  for i:=m to n-1 do begin
    mc:=FSceneParser.FContainers[i];
    if mc.UseParentViewer then mc.ParentViewer:=@FSceneViewer;
    mc.Process;
  end;
end;

procedure TRenderShell.ProcessDiffObjects;
var i: integer;
    mi: TVBOMeshItem;
begin
  //Process different mesh items
  for i:=0 to FSceneParser.FDiffItems.Count-1 do begin
    mi:=FSceneParser.FDiffItems[i];
    mi.Process;
    if mi.UseParentViewer then mi.ParentViewer:=@FSceneViewer;
  end;
end;

procedure TRenderShell.ProcessOpaque;
var i,n: integer;
    VisList: TList;
    mo: TVBOMeshObject;
    queries: array of GLUInt;
    sampleCount: GLUint;
    available: GLUint;

begin
  //Rendering opaque mesh objects
  VisList:=GetVisibleObjects(FSceneParser.FOpaqueMeshObjects,FSceneViewer.Frustum);
  SortByDistance(FSceneViewer.ViewMatrix,VisList,sdFrontToBack);
  if FCollection.FOcclusionCulling then begin
    setlength(queries,VisList.Count);
    glGenQueriesARB(VisList.Count, @queries[0]);
  end;
  n:=0;
  for i:=0 to VisList.Count-1 do begin
    mo:=VisList[i];
    mo.Matrices.ViewMatrix:=FSceneViewer.ViewMatrix;
    mo.Matrices.ProjectionMatrix:=FSceneViewer.ProjectionMatrix;
    mo.Time:=FSceneViewer.CurrentTime;
    if mo.UseParentViewer then mo.ParentViewer:=@FSceneViewer;
    if (not FCollection.FOcclusionCulling) or (mo.IgnoreOcclusion)
    or (mo.PolygonsCount<100) or (mo.MeshType=mtGrid) then mo.Process
    else begin
      glBeginQueryARB(GL_SAMPLES_PASSED_ARB, queries[i]);
      if (mo.Occluded) and (assigned(mo.Occluder)) then begin
         glDisable(GL_LIGHTING); glDepthMask(FALSE);
         glColorMask(FALSE, FALSE, FALSE, FALSE);
           mo.RenderOccluder(mo.Matrices.ViewMatrix,mo.Occluder);
         glColorMask(TRUE, TRUE, TRUE, TRUE);
         glDepthMask(TRUE); glEnable(GL_LIGHTING);
      end else mo.Process; n:=i;
      glEndQueryARB(GL_SAMPLES_PASSED_ARB);
    end;
  end;
  if FCollection.FOcclusionCulling then begin
    glFinish;
    repeat
      glGetQueryObjectivARB(queries[n], GL_QUERY_RESULT_AVAILABLE_ARB, @available);
    until available>0;
    for i:=0 to VisList.Count-1 do begin
      glGetQueryObjectuivARB(queries[i], GL_QUERY_RESULT_ARB, @sampleCount);
      mo:=VisList[i];
      if (sampleCount>1) then mo.Occluded:=false else mo.Occluded:=true;
    end;
    glDeleteQueriesARB(VisList.Count,@queries[0]);
  end; VisList.Free;
end;

procedure TRenderShell.ProcessProxys;
var i: integer;
    mo: TVBOMeshObject;
begin
  //Rendering proxy objects
  SortByDistance(FSceneViewer.ViewMatrix,FSceneParser.FProxyObjects,sdFrontToBack);
  for i:=0 to FSceneParser.FProxyObjects.Count-1 do begin
    mo:=FSceneParser.FProxyObjects[i];
    mo.Matrices.ViewMatrix:=FSceneViewer.ViewMatrix;
    mo.Matrices.ProjectionMatrix:=FSceneViewer.ProjectionMatrix;
    mo.Time:=FSceneViewer.CurrentTime;
    if mo.MaterialObject.IsTransparency then
      mo.SortProxyByDistance(sdBackToFront)
    else mo.SortProxyByDistance(sdFrontToBack);
    if mo.UseParentViewer then mo.ParentViewer:=@FSceneViewer;
    mo.Process;
  end;
end;

procedure TRenderShell.ProcessTransparency;
var i: integer;
    VisList: TList;
    mo: TVBOMeshObject;
begin
  //Rendering transparency objects
  VisList:=GetVisibleObjects(FSceneParser.FTransparencyMeshObjects,FSceneViewer.Frustum);
  SortByDistance(FSceneViewer.ViewMatrix,VisList,sdBackToFront);
  for i:=0 to VisList.Count-1 do begin
    mo:=VisList[i];
    mo.Matrices.ViewMatrix:=FSceneViewer.ViewMatrix;
    mo.Matrices.ProjectionMatrix:=FSceneViewer.ProjectionMatrix;
    mo.Time:=FSceneViewer.CurrentTime;
    if mo.UseParentViewer then mo.ParentViewer:=@FSceneViewer;
    mo.Process;
  end; VisList.Free;
end;

procedure TRenderShell.setHeight(const Value: integer);
begin
  FSceneViewer.ViewPort[3] := Value;
end;

procedure TRenderShell.setWidth(const Value: integer);
begin
  FSceneViewer.ViewPort[2] := Value;
end;

procedure TRenderShell.setProjMatrix(const Value: TMatrix);
begin
  FSceneViewer.ProjectionMatrix := Value;
  FSceneViewer.Frustum:=GetFrustum(FSceneViewer.ProjectionMatrix,FSceneViewer.ViewMatrix);
end;

procedure TRenderShell.setViewMatrix(const Value: TMatrix);
begin
  FSceneViewer.ViewMatrix := Value;
  FSceneViewer.Frustum:=GetFrustum(FSceneViewer.ProjectionMatrix,FSceneViewer.ViewMatrix);
end;

procedure TRenderShell.SortByDistance(const ViewMatrix: TMatrix; List: TList;
  SortDirection: TSortDirection);
Type TPDI = record p: TVector; d: single; Obj: pointer; end;
var i: integer;
    curr: TVBOMeshObject;
    pd: array of TPDI;
    ObjList: TList;
    Temp: TList;
  function CompareDistanceFTB(Item1, Item2: Pointer): Integer;
  begin result:=trunc(TPDI(Item1^).d-TPDI(Item2^).d); end;
  function CompareDistanceBTF(Item1, Item2: Pointer): Integer;
  begin result:=trunc(TPDI(Item2^).d-TPDI(Item1^).d); end;
begin
  if assigned(List) then ObjList:=List else exit;
  if SortDirection=sdNone then exit;
  Temp:=TList.Create; Temp.Count:=ObjList.Count;
  setlength(pd,ObjList.Count);
  for i:=0 to ObjList.Count-1 do begin
     curr:=ObjList[i];
     if not curr.WorldMatrixUpdated then curr.UpdateWorldMatrix;
     pd[i].p:=VectorTransform(curr.Position,ViewMatrix);
     pd[i].d:=VectorNorm(pd[i].p); pd[i].Obj:=curr;
     Temp[i]:=@pd[i];
  end;

  if SortDirection=sdFrontToBack then Temp.Sort(@CompareDistanceFTB);
  if SortDirection=sdBackToFront then Temp.Sort(@CompareDistanceBTF);
  for i:=0 to ObjList.Count-1 do ObjList[i]:=TPDI(Temp[i]^).Obj;
  Temp.Free;
end;

procedure TRenderShell.UpdateRenderTarget;
var i: integer;
    tex: TTexture;
begin
  if length(FAttachments)=0 then exit;
  for i:=0 to high(FAttachments) do begin
    tex:=FAttachments[i].Texture;
    FFBO.AttachTexture(tex,FAttachments[i].TargetTo);
    if assigned(tex) then begin
      if (tex.Width<>ViewportWidth) or (tex.Height<>ViewportHeight)
      then tex.SetDimensions(ViewportWidth,ViewportHeight);
    end;
  end;
end;

{ TMeshContainer }

constructor TMeshContainer.Create(aCollection: TMeshCollection);
begin
  inherited Create;
  FItemType:=mcContainer;
  FCollection:=aCollection;
  FRender:=TRenderShell.Create(FCollection);
  FLights:=TLightLibrary.Create;
  FApplyLights:=false;
  FCamera:=nil;
end;

destructor TMeshContainer.Destroy;
begin
  FRender.Free; FLights.Free;
  inherited;
end;

procedure TMeshContainer.Process;
begin
  //Apply Lights
  if FApplyLights then FLights.Apply;

  if assigned(FCamera) then begin
    FRender.FSceneViewer.ViewMatrix:=FCamera.ViewMatrix;
    FRender.FSceneViewer.ProjectionMatrix:=FCamera.ProjectionMatrix;
    FRender.FSceneViewer.Frustum:=GetFrustum(FCamera.ProjectionMatrix,FCamera.ViewMatrix);
  end;
  FRender.FSceneViewer.CurrentTime:=GetTime;

  //Process Container childs before container rendering
  if assigned(FChilde) then begin
    if FProcessChilds=pcBefore then FChilde.Process;
  end;
  //Process Collection childs before container rendering
  if assigned(FCollection.FChilde) then begin
    if FCollection.FProcessChilds=pcBefore then FCollection.FChilde.Process;
  end;
  //Rendering Containers
  if FRender.Active then Render.Process;
  //Process Collection childs after container rendering
  if assigned(FCollection.FChilde) then begin
    if FCollection.FProcessChilds=pcAfter then FCollection.FChilde.Process;
  end;
  //Process Container childs after container rendering
  if assigned(FChilde) then begin
    if FProcessChilds=pcAfter then FChilde.Process;
  end;
end;

end.


