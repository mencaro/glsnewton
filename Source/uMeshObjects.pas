unit uMeshObjects;

interface

uses Classes, Types, SysUtils,
     {$IFNDEF DIRECTGL}
     OpenGL1x, VectorLists, Octree,
     {$ELSE}
     dglOpenGL, uOctree,
     {$ENDIF}
     GeometryBB, VectorGeometry,
     VectorTypes, uVectorLists, uMiscUtils, uBaseClasses, uFileSMD,
     PFXManager, uTextures, uFBO, uVBO, uShaders, uStorage, uBaseResource,
     uMaterials, uMaterialObjects, OGLStateEmul;

Type

  TMeshTypes = (mtPlane, mtBox, mtSphere, mtFreeForm, mtHUDSprite, mtSprite, mtBBox,
                mtSphericalSprite, mtCylindricalSprite, mtScreenQuad, mtPoints,
                mtGrid, mtProxy, mtInstance, mtUser, mtParticles, mtActor, mtActorProxy);
  TMeshPlacement = (mpBackground, mpForeground, mpOrdinary);
  TMeshTypeSet = set of TMeshTypes;
  TSpriteType = (stSpherical, stCylindrical);

  T2DSingleArray = array of array of single;
  TFaceMode = (fmPoints, fmLines, fmFill);

  PPointParam = ^TPointParam;
  TPointParam = record
    DistanceAttenuation: TAffineVector;
    FadeTresholdSize: single;
    MinPointSize:single;
    PointSize:single;
    PointSmooth: boolean;
    PointSprite: boolean;
    UseColors: boolean;
  end;


  TVBOMeshObject = class;

  TLoDs = record
     MaxViewDistance: single;
     LodMesh: TVBOMeshObject;
  end; PLODs = ^TLoDs;

  TRaycastIntersectInfo = record
     iPoint,iNormal: TVector;
     inScreen, inObject, Dir: TAffineVector;
     PickedObject: TVBOMeshObject;
     ObjectIndex: integer;
     ObjMeshIndex: integer;
     ObjMeshIndexList: TIntegerList;
  end;
  PRaycastIntersectInfo=^TRaycastIntersectInfo;

  TVBOMeshObject = class (TMovableObject)
  Private
    FIndexInMesh: integer;
    FHandle: TVBOMeshObject;
    FParams: Pointer;
    FFBO: TFrameBufferObject;

    FPolyCount: integer; //количество полигонов
    FExtents: TExtents;  //модифицированный текущей модельной матрицей
    FBaundedRadius: single; //Радиус окаймляющей сферы
    FGeomCenter: TAffineVector; //геометрический центр объекта
    FOctreeList: TList;
    FExtentsBuilded: boolean;
    FonBeforeCheckVisibility: TObjectRenderEvents;
    FonBeforeRender: TObjectRenderEvents;
    FonAfterRender: TObjectRenderEvents;
    FonObjectClick: TVBOObjectClickEvents;
    FonMaterialApply: TMaterialProc;
    FonMaterialUnApply: TMaterialProc;
    FCulled: boolean;
    FOccluded: boolean;
    FIgnoreOcclusion: boolean;

    FFaceMode: TFaceMode;
    FUseLods: boolean;
    FTwoSides: boolean;
    FOwner: TVBOMeshItem;
    FIsProxy: boolean;

    procedure fUpdateExtents;
    function  AABBUpdate(const aabb: TAABB; const WorldMatrix:TMatrix): TAABB;
    function  AABBTransform(const ext: TExtents; const WorldMatrix:TMatrix): TExtents;
    function GetPolyCount: integer; virtual;
    procedure ExtentsToTranslateScale(var pos, sc: TVector);
    procedure SetFaceMode;
    function getMaterial: TMaterial;
    function getTexture: TTexture;
    procedure SetMaterial(const Value: TMaterial);
    procedure setTexture(const Value: TTexture);
    function getShader: TShaderProgram;
    procedure setShader(const Value: TShaderProgram);
    function getParent: TVBOMeshObject;
    procedure setParent(const Value: TVBOMeshObject);
    function getBlending: TBlendingModes;
    procedure SetBlending(const Value: TBlendingModes);
    function getMasterProxy: TVBOMeshObject;
    procedure setMasterProxy(const Value: TVBOMeshObject);
    procedure AddInstance(MeshObject: TVBOMeshObject);
    procedure RemoveInstance(Index: integer); overload;
    procedure RemoveInstance(MeshObject: TVBOMeshObject); overload;

  Protected
    FUseRenderList: boolean;
    FRenderList:TList;
    FMultiBuffer:TMultiPackBuff;

    FTime: Double;
    FOctreeBuilded: boolean;
    FBaseExtents: TExtents; //базовый Extents
    FMeshType: TMeshTypes; //тип объекта
    FMeshPlacement: TMeshPlacement; //расположение объекта на сцене (фон, передний план, обычное)

    FMaterial: TMaterial;
    FTexture: TTexture;
    FMaterialObject: TMaterialObject;
    FMaterials: TMaterialLibrary;
    FTextures: TTextureLibrary;
    FMatObjLib: TMaterialObjectsLib;

    FProxyList: TList;
    FProxyMatrixList: TList;
    FLodList: TList;

    FOccluder: PVBOBuffer;

    function SetMaterialName(const Name: string): TMaterialObject;
    procedure ResetBlending;
    procedure ClearProxyMatrix;
  Public
    MeshList: TList; //список VBO буферов
    Materials: TStringList;

    TextureId: GLUInt;
    Visible: boolean;
    Pickable: boolean; //будет ли объект выбираться через PickObject
    NoZWrite: boolean; //отключение режима записи в буфер глубины
    NoDepthTest: boolean; //отключение режима записи в буфер глубины

    procedure SaveToStream(s: TStream); override;
    procedure LoadFromStream(s: TStream); override;
    procedure SaveToStorage(const DataStorage: TDataStorage; const aGuid: TGUID);


    procedure MaterialSetter(const MatName: string; Action: TMatAction);
    property MatLib: TMaterialLibrary read FMaterials write FMaterials;
    property TexLib: TTextureLibrary read FTextures write FTextures;
    property MatObjLib: TMaterialObjectsLib read FMatObjLib write FMatObjLib;
    property OctreeList: TList read FOctreeList write FOctreeList;
    //-------Events--------
    //событие вызывается при рендеринге контейнера, непосредственно перед проверкой видимости
    property onBeforeCheckVisibility:TObjectRenderEvents read FonBeforeCheckVisibility write FonBeforeCheckVisibility;
    //Событие вызывается после бинда текстуры, непосредственно перед рендерингом
    property onBeforeRender:TObjectRenderEvents read FonBeforeRender write FonBeforeRender;
    //Событие вызывается непосредственно после рендеринга, перед анбиндом текстуры
    property onAfterRender:TObjectRenderEvents read FonAfterRender write FonAfterRender;
    //Вызывается после вызова функции PickObject
    property onObjectClick: TVBOObjectClickEvents read FonObjectClick write FonObjectClick;
    //Вызывается в момент применения шейдера
    property onMaterialApply: TMaterialProc read FonMaterialApply write FonMaterialApply;
    property onMaterialUnApply: TMaterialProc read FonMaterialUnApply write FonMaterialApply;


    Constructor Create;
    Destructor Destroy;override;

    Procedure RenderObject(const ViewMatrix: TMatrix);virtual;
    Procedure RenderOccluder(var ViewMatrix: TMatrix; Occluder: PVBOBuffer);virtual;
    Procedure Process; override;

    procedure UpdateWorldMatrix(UseMatrix: TTransforms=[ttAll]); override;
    //Обновляет материалы экспортированные со сцены
    Procedure UpdateMaterialList;
    //Создает составной материал для указанного меша
    function GetMeshMaterial(MeshIndex: integer): TMaterialObject;
    //Сортировка прокси
    procedure RebuildProxyList(const ViewMatrix, mv: TMatrix);
    procedure SortProxyByDistance(SortDirection: TSortDirection);

    Property Material: TMaterial read getMaterial write setMaterial;
    Property Texture: TTexture read getTexture write setTexture;
    Property Shader: TShaderProgram read getShader write setShader;
    Property MaterialObject: TMaterialObject read FMaterialObject;
    Property Blending: TBlendingModes read getBlending write SetBlending;
    Property TwoSides: boolean read FTwoSides write FTwoSides;

    //Указатель на самого себя
    Property Handle: TVBOMeshObject read FHandle;
    Property Owner: TVBOMeshItem read FOwner write FOwner;
    Property Params: Pointer read FParams write FParams;
    //Индекс в контейнере
    Property IndexInMesh: integer read FIndexInMesh write FIndexInMesh;
    //Доступ к буферу кадра
    Property FBO: TFrameBufferObject read FFBO;
    //Тип объекта
    Property MeshType: TMeshTypes read FMeshType write FMeshType;
    //Расположение объекта на сцене (задний/передний план, обычное)
    Property MeshPlacement: TMeshPlacement read FMeshPlacement write FMeshPlacement;
    //Возвращает трансформированные координаты окаймляющего бокса
    Property Extents: TExtents read FExtents;
    //Возвращает координаты исходного окаймляющего бокса
    Property BaseExtents: TExtents read FBaseExtents write FBaseExtents;
    //Радиус окаймляющей сферы
    Property BaundedRadius: single read FBaundedRadius;
    //Геометрический центр объекта
    Property GeomCenter: TAffineVector read FGeomCenter;

    //Установка родителя, из которого будет браться базовая матрица трансформаций
//    Property Parent: TVBOMeshObject read getParent write SetParent;
    //Время начала рендеринга кадра
    Property Time: double read FTime write FTime;
    //количество полигонов
    Property PolygonsCount: integer read GetPolyCount;
    //Результат проверки видимости
    Property Culled: boolean read FCulled write FCulled;
    Property Occluded: boolean read FOccluded write FOccluded;

    //Читает мультибуфер
    Property MultiBuffer: TMultiPackBuff read FMultiBuffer;
    //Список инстансов
    Property ProxyList: TList read FProxyList;
    //Мастер-прокси объекта
    Property MasterProxy: TVBOMeshObject read getMasterProxy write setMasterProxy;
    //Режим отображения граней
    Property FaceMode: TFaceMode read FFaceMode write FFaceMode;
    //Игнорировать проверку перекрытия
    Property IgnoreOcclusion: boolean read FIgnoreOcclusion write FIgnoreOcclusion;
    Property Occluder: PVBOBuffer read FOccluder write FOccluder;
    //Использовать ЛОДы
    Property UseLods: boolean read FUseLods write FUseLods;
    Property LodList: TList read FLodList write FLodList;


    Procedure UpdateExtents;
    //трансформирует все вершины используя матрицу WMatrix,
    //возвращает false в случае если буферы были очищены
    Function AbsoluteTransform:boolean;
    //Производит абсолютную трансформацию вершин относительно геометрического центра
    Procedure AbsoluteToExtents;
    //Переводит мировые ортогональные координаты в относительные координаты
    Function ScreenToLocal(P: TVector): TVector;
    //Очищает занимаемую объектом оперативную память
    Procedure FreeObjectsMemory;
    //Строит Octree по каждому из мешей в MeshList
    Procedure BuildOctreeList(Level:integer=3);
    //Упаковывает все меши в один буфер
    Procedure PackMeshes(FreeOldBuffers:boolean=false; BuffSize:integer=-1);
    Procedure PackToSubMeshes;
    //Упаковывает меш в текстуру, x,y,z-координаты вершины, w-текстурная координата p
    Procedure PackMeshesToTexture(var vtex,ntex: TTexture);
    //Упаковывает tri-list в текстуру, 3 последовательных пикселя задают вершины
    //треугольника, координата w=0 - вершина не используется
    Procedure PackTriListToTexture(var vtex: TTexture);
    //Извлекает все треугольники из меша и помещает в один буфер
    Procedure GetTriMesh(var TriMesh: TAffinevectorList);
    //Ищет точку пересечения луча с объектом
    Function OctreeRayCastIntersect(const rayStart, rayVector: TVector; var iList:TList;
                         iPoint: PVector = nil;iNormal: PVector = nil): integer; overload;
    Function OctreeRayCastIntersect(const rayStart, rayVector: TVector;
                         iPoint: PVector = nil; iNormal: PVector = nil): boolean; overload;
    //Проверяет пересечения луча с окаймляющим боксом объекта
    Function ExtentsIntersect(const rayStart,
                      rayVector: TVector; iPoint:PVector=nil): boolean;
    //Проверяет попадание точки в окаймляющий бокс объекта
    Function PointInExtents(p: TVector): boolean;
    //Переносит буферы VBO с прозрачностью в конец списка
    Procedure SortMeshByTransparency;
    //Добавляет LOD
    Procedure AddLod(LOD: TVBOMeshObject; MaxViewDistance:single);
    //Меняет мастер-объект прокси
    Procedure ChangeProxy(MasterObject: TVBOMeshObject);
    //Тесселяция геоемтрии
    Procedure Tesselate(iter: integer = 1);
    //Очищает список всех прокси
    Procedure ClearProxy;
  end;

  TVBOTerrain = class (TVBOMeshObject)
    private
      FHMWidth, FHMHeight: integer;
      FHMap: array of array of single;
      FXTiles, FYTiles: integer;
      FTileSize: integer;
      FPatchWidth, FPatchHeight: integer;
      FCreateBuff: TRenderBuff;
      FExtList: TList;
      FOffsList: TIntegerList;
      FCountList: TIntegerList;
      FVisiblePolyCount: integer;
      function GetPolyCount: integer; override;
      Procedure GetHeights(Width, Height: integer; GetHeightFunc: TGetHeightFunc);
      Procedure CreateSolidBuffer;
    public
      Constructor Create;
      Procedure BuildTerrain(Width, Height: integer; GetHeightFunc: TGetHeightFunc; TileSize: integer = 32);
      Destructor Destroy;override;
      Procedure RenderObject(const ViewMatrix: TMatrix);override;
      //Get Height&Normal from Terrain
      Function GetInterpolatedHeight(X, Y: single): single;
      Function GetNormalInPoint(X, Y: single): TAffineVector;
      Function GetNormalFromHField(X, Y: integer): TAffineVector;
      Function GetTerrainPatch(X,Y, W,H: integer; var Patch: T2DSingleArray): integer;
      Function UpdateTerrainPatch(X,Y: integer; var Patch: T2DSingleArray): boolean;

      Procedure RebuildNormals;

      Function LineOfSight(p1,p2: TAffineVector): boolean;
      Property MapWidth: integer read FHMWidth;
      Property MapHeight: integer read FHMHeight;
  end;

  TVBOParticles = class (TVBOMeshObject)
    private
      FPointParam: TPointParam;
      FMaxCapacity: integer;
      FCount: integer;
      FRealCount: integer;
      FPositions: TAffineVectorList;
      FColors: TVectorList;
      //Отсортированые списки для рендеринга
      FSortedColors: TVectorList;
      FSortedPositions: TAffineVectorList;
      //Указывает способ сортировки частиц
      FSortParticles: TSortDirection;

      FPFXManager: TPFXManager;

      FVelocityList: TAffineVectorList;
      FAccelList: TAffineVectorList;
      FMassList: TSingleList;

      FImmediate: boolean;
      FVNeedUpdate: boolean;
      FCNeedUpdate: boolean;

      FExtents: TExtents; //модифицированный текущей модельной матрицей

      Function  FGetSpriteData(Index: Integer): TPointSpriteData;
      Procedure FSetSpriteData(Index:integer; SpriteData: TPointSpriteData);
      Function  GetColor(Index: Integer): TVector;
      Procedure SetColor(Index: integer; v: TVector);
      Function  GetPosition(Index: Integer): TVector;
      Procedure SetPosition(Index: integer; v: TVector);

      Function  FAddNewBuffer:PVBOBuffer;
      Procedure FUpdateBuffer(Index:integer; UpdBuff:TUpdateBuff=
                              [upVertex, upColor]);overload;
      Procedure FUpdateBuffer(Index:integer; Count:integer;
                              UpdBuff:TUpdateBuff=[upVertex,upColor]);overload;
      Procedure SetPFXManager(const Value: TPFXManager);

      Function GetAccel(Index: Integer): TVector;
      Function GetMass(Index: Integer): single;
      Function GetVel(Index: Integer): TVector;
      Procedure SetAccel(Index: Integer; const Value: TVector);
      Procedure SetMass(Index: Integer; const Value: single);
      Procedure SetVel(Index: Integer; const Value: TVector);
      Procedure SetCount(const Value: integer);
      Procedure SetImmediate(const Value: boolean);
      Procedure SetExtents(const Value: TExtents);
      Procedure setUseColors(const Value: boolean);

      Procedure SortParticles(const VM: TMatrix; SortDirection: TSortDirection=sdBackToFront);
      Procedure FUpdateFromSortedList(Index:integer; Count:integer;
                  UpdBuff:TUpdateBuff=[upVertex,upColor]);

    public
      Constructor Create(MaxCapacity:integer=-1);
      Destructor Destroy;override;

      Procedure RenderObject(const ViewMatrix:TMatrix);override;

      Property Extents:TExtents read FExtents write SetExtents;

      Property Count:integer read FCount write SetCount;

      property Particles[Index: Integer]: TPointSpriteData
                        read FGetSpriteData
                        write FSetSpriteData; default;
      property Colors[Index: Integer]: TVector
                        read GetColor write SetColor;
      property Positions[Index: Integer]: TVector
                        read GetPosition write SetPosition;
      property Velocity[Index: Integer]: TVector
                        read GetVel write SetVel;
      property Acceleration[Index: Integer]: TVector
                        read GetAccel write SetAccel;
      property Mass[Index: Integer]: single
                        read GetMass write SetMass;
      //Задает режим обновления данных true - сразу обновляет данные в буфере
      property Immediate: boolean read FImmediate write SetImmediate;

      //Общие для всех частиц параметры - уменьшение размера по мере отдаления от камеры,
      //усечение спрайта, минимальный/максимальный размер, режим смешивания и альфатеста,
      //режим скругления границ спрайта, тесты глубины
      property DistanceAttenuation: TAffineVector read FPointParam.DistanceAttenuation
                                             write FPointParam.DistanceAttenuation;
      property FadeTresholdSize: single read FPointParam.FadeTresholdSize
                                       write FPointParam.FadeTresholdSize;
      property MinPointSize:single read FPointParam.MinPointSize
                                   write FPointParam.MinPointSize;
      property PointSize:single read FPointParam.PointSize
                               write FPointParam.PointSize;
      property PointSmooth: boolean read FPointParam.PointSmooth
                                   write FPointParam.PointSmooth;
      property PointSprite: boolean read FPointParam.PointSprite
                                   write FPointParam.PointSprite;
      //говорит будет ли использоваться буфер цвета для спрайта
      property UseColors: boolean read FPointParam.UseColors
                                 write setUseColors;
      //Задает сортировку частиц
      property Sorting: TSortDirection read FSortParticles write FSortParticles;
      //менеджер частиц
      property PFXManager:TPFXManager read FPFXManager write SetPFXManager;
      //функции добавления частиц и их свойств
      Function AddParticle(Position:TVector; Color: TVector): integer; overload;
      Function AddParticle(Position:TVector): integer; overload;
      Procedure AddVelocity(Velocity:TAffineVector; index:integer=-1);
      Procedure AddAcceleration(Acceleration:TAffineVector; index:integer=-1);
      Procedure AddMass(Mass:single; index:integer=-1);

  end;

{------------TVBOAnimatedSprite-------------}
  TFramesDirection = (fdHorizontal, fdVertical);
  TSpriteAnimationMode = (samNone, samPlayOnce, samLoop, samLoopBackward);
  TVBOAnimatedSprite = class (TVBOMeshObject)
    private
      FFramesDir: TFramesDirection;
      FFramesCount: integer;
      FHorFramesCount: integer;
      FVertFramesCount: integer;
      FAnimated: boolean;
      FFrameNum: integer;
      FFrameRate: single;
      FFrameWidth: single;
      FFrameHeight: single;
      FOldTime: double;
      FDeltaFrameTime: double;
      FAnimationMode: TSpriteAnimationMode;
      FPlayDir:integer;
      FHorInv:boolean;
      FVerInv: boolean;

      procedure SetHorFramesCount(const Value: integer);
      procedure SetVertFramesCount(const Value: integer);
      procedure UpdatePosition(const Value: integer);
      procedure SetFrameRate(const Value: single);

    public
      OnEndFrameReached : procedure of Object;
      property AnimationMode:TSpriteAnimationMode read FAnimationMode;
      //указывает в каком направлении увеличивается номер кадра в кинограме
      property FramesDirection: TFramesDirection read FFramesDir write FFramesDir;
      //количество кадров в кинограмме
      property FramesCount: integer read FFramesCount write FFramesCount;
      //количество кадров по горизонтали
      property HorFramesCount: integer read FHorFramesCount write SetHorFramesCount;
      //количество кадров по вертикали
      property VertFramesCount: integer read FVertFramesCount write SetVertFramesCount;
      //номер текущего кадра
      property FrameNum: integer read FFrameNum write UpdatePosition;
      //частота кадров
      property FrameRate: single read FFrameRate write SetFrameRate;
      //отразить зеркально по вертикали и горизонтали
      property VerticalFlipped: boolean read FVerInv write FVerInv;
      property HorizontalFlipped: boolean read FHorInv write FHorInv;

      Constructor Create;
      Destructor Destroy; override;
      Procedure RenderObject(const ViewMatrix: TMatrix);override;

      //движение по анимации - первый, последний, следующий, предыдущий, конкретный кадр
      Procedure FirstFrame;
      Procedure LastFrame;
      Procedure NextFrame;
      Procedure PrevFrame;
      Procedure ToFrame(index: integer);
      //остановитьп роигрывание и начать проигрывание с заданным режимом
      Procedure Stop;
      Procedure Play(Mode:TSpriteAnimationMode=samLoop);
      //отразить зеркально по вертикали и горизонтали
      Function FlipVertical: boolean;
      Function FlipHorizontal: boolean;
  end;

  TSkeletalRender = class (TVBOMeshObject)
    private
      FAnim: PAnimations;
      FScreenQuad: TVBOMeshObject;
      spId: GLUInt;
      Shaders: TShaders;
      FFramesCount: integer;
      FramePosition: single;
      FAnimationName: string;
      FAnimationNum: integer;

      procedure FApplyShader(mo:TObject);
      procedure FUnApplyShader(mo:TObject);
      procedure FSetAnimationByNum(Value: integer);
      procedure FSetAnimationByName(Value: string);
      procedure FSetFrame(Value: single);
      function GetAnimCount: integer;
    public
      vtex,ntex,rvtex,rntex: TTexture;

      Constructor Create;
      Destructor Destroy; override;

      Procedure Assign(const SMD: TSkeletalRender);
      Procedure AddScreenQuad(SQ: TVBOMeshObject);
      procedure CreatSMDShader;

      Procedure RenderObject(const ViewMatrix: TMatrix);override;
      Procedure NextFrame(n:single=1);

      Property FramePos: single read FramePosition write FSetFrame;
      Property FramesCount: integer read FFramesCount;
      Property AnimationsCount: integer read GetAnimCount;
      Property AnimationNum: integer read FAnimationNum write FSetAnimationByNum;
      Property AnimationName: string read FAnimationName write FSetAnimationByName;
      Property ScreenQuad: TVBOMeshObject read FScreenQuad;
      Property Anim: PAnimations read FAnim write FAnim;
  end;

  //WeightsFormat:
  //wfTC1p: 1 node in the TexCoord0.p
  //wf2i2w: 2 nodes - 2 index + 2 weights in the new attrib
  //wf4i4w: 4 nodes - 4 node index in the Attr1, 4 weights in the Attr2
  TWeightsFormat = (wfTC1p,wf2i2w,wf4i4w);

  TWeightsAttrNames = record IndexAttrName, WeightsAttrName: string; end;

  TGetShaderTextProc = function (ShaderType: TShaderType): ansistring of Object;
  TUniformSMDRender = class (TVBOMeshObject)
    private
      FAnim: PAnimations;
      spId: GLUInt;
      Shaders: TShaders;
      FFramesCount: integer;
      FOldFrameNum: single;
      FramePosition: single;
      FAnimationName: string;
      FAnimationNum: integer;
      FBones: integer;
      FBoneArray: array of TVector;
      FSmoothed: boolean;
      FBlended: boolean;
      FUseBlended: boolean;
      FCurrentFrame: TSMDNodes;
      FNodeRadius: single;
      FonUserCulling: TVBOVisibilityEvents;
      FWeightsCount: integer;
      FWeightsFormat: TWeightsFormat;
      FAttrNames: TWeightsAttrNames;
      FWeightsRebuilded: boolean;
      FonGetShaderText: TGetShaderTextProc;

      procedure FApplyShader(mo:TObject);
      procedure FUnApplyShader(mo:TObject);
      procedure FSetAnimationByNum(Value: integer);
      procedure FSetAnimationByName(Value: string);
      procedure FSetFrame(Value: single);
      function GetAnimCount: integer;
      function GetFramesCount(index: integer): integer;
      function getBone(index: integer): TVector;
      procedure setBone(index: integer; const Value: TVector);
    public
      constructor Create;
      destructor Destroy; override;

      procedure CreatSMDShader;
      function CheckVisibility(const F: TFrustum): boolean;
      procedure RenderObject(const ViewMatrix: TMatrix);override;
      procedure NextFrame(n:single=1);
      procedure BlendAnimation(AnimIdx1,AnimIdx2: integer; Frame1,Frame2, Factor: single);
      procedure BlendWithCurrentFrame(Anim: integer; Frame: single; Factor: single);
      function GetBoneMatrix(Index: integer): TMatrix;
      procedure SetBoneMatrix(Index: integer; const Matrix: TMatrix);
      function GetBoneGlobalMatrix(Index: integer): TMatrix;
      procedure SetBoneGlobalMatrix(Index: integer; const Matrix: TMatrix);
      procedure AttachMeshFromSMD(FileName: string);
      procedure AddAnimationFromSMD(const AnimFiles: array of string);

      property FramePos: single read FramePosition write FSetFrame;
      property FramesCount: integer read FFramesCount;
      property AnimationsCount: integer read GetAnimCount;
      property AnimationNum: integer read FAnimationNum write FSetAnimationByNum;
      property AnimationName: string read FAnimationName write FSetAnimationByName;
      property Smoothed: boolean read FSmoothed write FSmoothed;
      property AnimationsFrames[index: integer]: integer read GetFramesCount;
      property Bones[index: integer]: TVector read getBone write setBone;
      property BonesCount: integer read FBones write FBones;
      property CurrentFrame: TSMDNodes read FCurrentFrame write FCurrentFrame;
      property NodeRadius: single read FNodeRadius write FNodeRadius;
      property onUserCulling: TVBOVisibilityEvents read FonUserCulling write FonUserCulling;
      property Anim: PAnimations read FAnim write FAnim;
      property onGetShaderText: TGetShaderTextProc read FonGetShaderText write FonGetShaderText;

      procedure SaveToFile(FileName: string; compressed: boolean=false);
      procedure LoadFromFile(FileName: string);
      procedure RebuildWeightsToOne;
      procedure RebuildWeights(WeightsCount: integer;
        WeightsFormat: TWeightsFormat; IndexAttrName, WeightAttrName: string);
  end;

  TVolumetricLines = class (TVBOMeshObject)
  private
    FNodes: TAffineVectorList;
    FLineWidth: single;
    FUpdated: boolean;
    FCount: integer;
    FMaxCapacity: integer;
    FSId: cardinal;
    FShaders: TShaders;
    FVBOBuff: PVBOBuffer;
    FBreaks: TIntegerList;
    function GetNode(Index: Integer): TAffineVector;
    procedure SetNode(Index: Integer; const Value: TAffineVector);
    procedure CreateVLShader;
    procedure AddLine(I1,I2: integer);
    procedure AddTestQuad;
  public
    constructor Create(MaxNodes: integer = 1000);
    destructor Destroy; override;
    Procedure RenderObject(const ViewMatrix: TMatrix);override;

    function AddNode(v: TAffineVector): integer; overload;
    function AddNode(x,y,z: single): integer; overload;
    procedure BreakLine;

    property LineWidth: single read FLineWidth write FLineWidth;
    property Nodes[Index: Integer]: TAffineVector read GetNode write SetNode; default;
    property Count: integer read FCount;

//    procedure Delete
//    procedure Exchange
    procedure Clear;
  end;

  TAnimatedMeshResource = class (TPersistentResource)
  private
    FSMD: TUniformSMDRender;
    procedure WriteVector4f(const Value: TVector; const stream: TStream);
    procedure WriteMatrix(const Value: TMatrix; const stream: TStream);
    function ReadVector4f(const stream: TStream): TVector;
    function ReadMatrix(const stream: TStream): TMatrix;
  public
    constructor Create(SMD: TUniformSMDRender);
    procedure SaveToStream(s: TStream); override;
    procedure LoadFromStream(s: TStream); override;
  end;

procedure QuadFromCount (count: integer; var size: integer);
function CreateBBMatrix (const View: TMatrix; const mat: TMatrixStack; angle: single;stype: TSpriteType):TMatrix;

implementation

procedure QuadFromCount (count: integer; var size: integer);
const pow2:array[0..12] of integer =
      (1,2,4,8,16,32,64,128,256,512,1024,2048,4096);
      sq2: array[0..12] of integer =
      (1,4,16,64,256,1024,4096,16384,65536,262144,1048576,4194304,16777216);
var i:integer;
begin
  i:=0;
  while (i<=12) and (sq2[i]<count) do inc(i);
  assert(i<=12,'To many vertexes');
  size:=pow2[i];
end;

function CreateBBMatrix (const View: TMatrix; const mat: TMatrixStack; angle: single;stype: TSpriteType):TMatrix;
var camUp: TVector;
    look,Right:TVector;
    mv,wm,rmat:TMatrix;
begin
  wm:=MatrixMultiply(Mat.ModelMatrix,Mat.TranslationMatrix);
  mv:=MatrixMultiply(wm,View);
  Right:=vectormake(-1,0,0,0);
  if SType = stCylindrical then begin
     camUp:=mv[1]; NormalizeVector(camUp); //Cilindrical
  end else camUp:=vectormake(0,1,0,0);     //Spherical
  look:=VectorCrossProduct(right,camup);
  NormalizeVector(look);
  result[0]:=vectormake(1,0,0,0);
  result[1]:=CamUp;
  result[2]:=look;
  result[3]:=mv[3];
  result:=MatrixMultiply(Mat.ScaleMatrix,result);
  rmat:=CreateRotationMatrix(look,angle);
  result:=MatrixMultiply(rmat,result);
end;

{ TVBOMeshObject }

function TVBOMeshObject.AbsoluteTransform: boolean;
var i:integer;
    P:PVBOBuffer;
begin
    result:=false; UpdateWorldMatrix;
    //Check Buffs - нельзя трансформировать очищенные буферы
    for i:=0 to MeshList.Count-1 do begin
       P:=MeshList[i]; if P.Cleared then exit;end;
    //Transform Buffs - трансформируем координаты вершин и нормали, используя текущую матрицу
    Visible:=false;
    for i:=0 to MeshList.Count-1 do begin
       P:=MeshList[i];FreeVBOBuffer(P^, false);
       P.Vertexes.TransformAsPoints(Matrices.WorldMatrix);
       if assigned(P.Normals) then begin
         P.Normals.TransformAsVectors(Matrices.WorldMatrix);
         P.Normals.Normalize;
       end;
       GenVBOBuff(P^,false);
    end; Result:=true;
    FBaseExtents:=GetExtentsOfList(MeshList);
    ResetMatrices; UpdateWorldMatrix; UpdateMaterialList;Visible:=true;
end;

procedure TVBOMeshObject.AddInstance(MeshObject: TVBOMeshObject);
begin
  ProxyList.Add(MeshObject);
end;

procedure TVBOMeshObject.AddLod(LOD: TVBOMeshObject; MaxViewDistance: single);
var l: PLODs;
begin
   new(l);
   l.MaxViewDistance:=MaxViewDistance;
   l.LodMesh:=lod; //lod.NonVisual:=true;
   FLodList.Add(l);
end;

procedure TVBOMeshObject.BuildOctreeList(Level: integer=3);
var Temp: TOctree;
    P:PVBOBuffer;
    i:integer;
    TriList: TAffineVectorList;
begin
  for i:=0 to MeshList.Count-1 do begin
   TriList:=TAffineVectorList.Create;
   P:=MeshList[i];
   if (p.Vertexes.Count mod 3) = 0 then
     TriList.Assign(p.Vertexes)
   else ExtractTriangles(P^,TriList);
 {$IFNDEF DIRECTGL}
   Temp:=TOctree.Create;
   with Temp do begin
    DisposeTree;
    InitializeTree(P.emin, P.emax, TriList, Level);
   end;
 {$ELSE}
   Temp:=TOctree.Create(TriList,Level);
 {$ENDIF}
   FOctreeList.Add(Temp); TriList.Free;
  end;
  FOctreeBuilded:=true;
end;

procedure TVBOMeshObject.ChangeProxy(MasterObject: TVBOMeshObject);
begin
  assert(FIsProxy,'MeshObject "'+Name+'" is not ProxyObject');
  if assigned(MasterObject) then begin
    MeshType:=MasterObject.MeshType;
    Params:=MasterObject;
    MeshList:=MasterObject.MeshList;
    OctreeList:=MasterObject.OctreeList;
    LodList:=MasterObject.LodList;
    UseLods:=MasterObject.UseLods;
    BaseExtents:=MasterObject.BaseExtents;
    MatObjLib:=MasterObject.MatObjLib;
    UpdateWorldMatrix; UpdateMaterialList;
  end else Visible:=false;
end;

procedure TVBOMeshObject.ClearProxy;
var pm: PMatrix;
    i: integer;
begin
  for i:=0 to FProxyMatrixList.Count-1 do begin
        pm:=FProxyMatrixList[i];
        if pm<>nil then dispose(pm);
        FProxyMatrixList[i]:=nil;
  end;
  FProxyMatrixList.Clear;
  FProxyList.Clear;
end;

procedure TVBOMeshObject.ClearProxyMatrix;
var i: integer;
  pm: PMatrix;
begin
  for i:=0 to FProxyMatrixList.Count-1 do begin
    pm:=FProxyMatrixList[i];
    if pm<>nil then dispose(pm);
    FProxyMatrixList[i]:=nil;
  end;
  FProxyMatrixList.Clear;
end;

constructor TVBOMeshObject.Create;
begin
  FHandle:=inherited Create;
  FItemType:=mcMeshObject;
  FIsProxy:=false;
  FMeshPlacement:=mpOrdinary;

  MeshList:=TList.Create;
  Materials:=TStringList.Create;
  FOctreeList:=TList.Create;
  FLodList:=TList.Create;
  FOctreeBuilded:=false;
  FProxyList:=TList.Create;
  FProxyMatrixList:=TList.Create;
  FBaundedRadius:=1;
  FGeomCenter:=NullVector;
  UpdateWorldMatrix;
  FExtentsBuilded:=false;
  Pickable:=true;
  FParams:=nil; Parent:=nil;
  FOccluded:=false; FCulled:=false;
  FUseRenderList:=false;
  FRenderList:=TList.Create;
  NoZWrite:=false;
  NoDepthTest:=false;
  FIgnoreOcclusion:=false;
  FUseLods:=true;
  FaceMode:=fmFill;
  FFBO:=TFrameBufferObject.Create;
  FTwoSides:=false;
  FMaterialObject:= TMaterialObject.Create;
  FMatObjLib:=nil;
  FPolyCount:=-1;
  FIndexInMesh:=-1;
end;

destructor TVBOMeshObject.Destroy;
var i: integer;
    temp: TOctree;
    pd: PMultiRenderDescr;
    m: PMatrix;
    pl: PLODs;
    mo: TVBOMeshObject;
begin
  Visible:=false;
  if assigned(FParams) and ((FMeshType<>mtPoints) or (FMeshType=mtInstance))
  then begin
    MeshList:=nil; FOctreeList:=nil; FLodList:=nil;
    if FMeshType=mtInstance then begin
      try
        TVBOMeshObject(FParams).RemoveInstance(self);
      except
      end;
    end;
  end else begin
    FreeVBOList(MeshList,true); MeshList.Free;
    for i:=0 to FOctreeList.Count-1 do begin
      temp:=FOctreeList[i]; Temp.Free;
    end; FOctreeList.Free;
    FreeList(FLodList);
  end;

  if FUseRenderList then begin
    for i:=0 to high(FMultiBuffer) do
    with FMultiBuffer[i] do begin
      FreeVBOBuffer(Buff);
      CountList.Clear;    CountList.Free;
      IndiceList.Clear;   IndiceList.Free;
      ExtentsList.Clear;  ExtentsList.Free;
      FaceTypes.Clear;    FaceTypes.Free;
      MaterialList.Clear; MaterialList.Free;
    end;
    for i:=0 to FRenderList.Count-1 do begin
      pd:=FRenderList[i];
      pd.RenderCount:=nil; pd.RenderIndice:=nil;
      dispose(pd);
    end; FRenderList.Clear;
  end;
  FProxyList.Free;
  for i:= 0 to FProxyMatrixList.Count - 1 do begin
     m:=FProxyMatrixList[i];
     if m<>nil then freemem(m,sizeof(TMatrix));
     FProxyMatrixList[i]:=nil;
  end;
  FProxyMatrixList.Free;
  FRenderList.Free;
  if FMeshType = mtPoints then Dispose(FParams);
  Materials.Free; FFBO.Free;
  FMaterialObject.Free;
  inherited;
end;

procedure TVBOMeshObject.FreeObjectsMemory;
var i:integer;
    P:PVBOBuffer;
begin
   for i:=0 to MeshList.Count-1 do begin
     P:=MeshList[i]; FreeVBOMem(P^); end;
end;

function TVBOMeshObject.getBlending: TBlendingModes;
begin
  result:=MaterialObject.Blending.BlendingMode;
end;

function TVBOMeshObject.getMasterProxy: TVBOMeshObject;
begin
  result:=FParams;
end;

function TVBOMeshObject.getMaterial: TMaterial;
begin
  Result:=FMaterialObject.Material;
end;

function TVBOMeshObject.AABBTransform(const ext: TExtents;
  const WorldMatrix:TMatrix): TExtents;
begin
  result.emin:=VectorTransform(ext.emin,WorldMatrix);
  result.emax:=VectorTransform(ext.emax,WorldMatrix);
end;

Function TVBOMeshObject.AABBUpdate(const aabb: TAABB;
  const WorldMatrix: TMatrix):TAABB;
var c:array[0..7] of TVector;
    i:integer;
    emin,emax:TVector;
begin
   setvector(c[0],aabb.min[0],aabb.max[1],aabb.min[2],1);
   setvector(c[1],aabb.min,1);
   setvector(c[2],aabb.max[0],aabb.min[1],aabb.min[2],1);
   setvector(c[3],aabb.max[0],aabb.max[1],aabb.min[2],1);

   setvector(c[4],aabb.min[0],aabb.max[1],aabb.max[2],1);
   setvector(c[5],aabb.min[0],aabb.min[1],aabb.max[2],1);
   setvector(c[6],aabb.max[0],aabb.min[1],aabb.max[2],1);
   setvector(c[7],aabb.max,1);

   for i:=0 to 7 do c[i]:=VectorTransform(c[i],WorldMatrix);
   emin:=c[0];emax:=emin;

   for i:=0 to 7 do begin
      if c[i][0]<emin[0] then emin[0]:=c[i][0];
      if c[i][0]>emax[0] then emax[0]:=c[i][0];
      if c[i][1]<emin[1] then emin[1]:=c[i][1];
      if c[i][1]>emax[1] then emax[1]:=c[i][1];
      if c[i][2]<emin[2] then emin[2]:=c[i][2];
      if c[i][2]>emax[2] then emax[2]:=c[i][2];
   end;
   setvector(Result.min,emin);
   setvector(Result.max,emax);
end;

procedure TVBOMeshObject.UpdateWorldMatrix;
var wm: TMatrix;
    bb: TAABB;
    pbb: PExtents;
    buff: PVBOBuffer;
begin
  inherited UpdateWorldMatrix(UseMatrix);
 with Matrices do begin
  bb:=TAABB(FBaseExtents);
  bb:=AABBUpdate(bb,WorldMatrix);
  FExtents:=TExtents(bb);
  FBaundedRadius:=VectorDistance(FExtents.emin, FExtents.emax) * 0.5;
  FGeomCenter:=VectorScale(VectorAdd(FExtents.emin, FExtents.emax), 0.5);
  WorldMatrixUpdated:=true;
 end;
end;

procedure TVBOMeshObject.Tesselate(iter: integer);
var i,j: integer;
    buff: PVBOBuffer;
begin
  for i:=0 to MeshList.Count-1 do begin
    buff:=MeshList[i];
    for j:=1 to iter do uVBO.Tesselate(buff^,2);
    RebuildVBOBuff(buff^,false);
  end;
end;

function TVBOMeshObject.OctreeRayCastIntersect(const rayStart, rayVector: TVector;
  var iList:TList; iPoint: PVector = nil; iNormal: PVector = nil): integer;
var i:integer;
  locRayStart, locRayVector: TVector;
  mOctree:TOctree;
  ip,n:TVector;
  f:boolean;
  icount:integer;
  ri: PRaycastIntersectInfo;
begin
  if not assigned(iList) then iList:=TList.Create;// else iList.Clear;
  Assert(FOctreeList.Count>0, 'Octree must have been prepared before use RayCast.');
  f:=ExtentsIntersect(rayStart, rayVector,@ip);
  locRayStart := AbsoluteToLocal(RayStart);
  locRayVector := vectormake(VectorToLocal(affinevectormake(RayVector)));
  if not f then begin result:=-1; exit; end;
  icount:=0;
  for i:=0 to FOctreeList.Count-1 do begin
    mOctree := FOctreeList[i];
    f := mOctree.RayCastIntersect(locRayStart, locRayVector, @ip, @N);
    if f then begin
       new(ri); ri.ObjMeshIndexList:=TIntegerList.Create;
       ri.iPoint:=ip;ri.iNormal:=N;
       ri.PickedObject:=self; ri.ObjMeshIndex:=i;
       ri.ObjectIndex:=TVBOMeshObject(self).FIndexInMesh;
       ri.ObjMeshIndexList.Add(i);
       iList.Add(ri); inc(icount);
    end;
  end;
  result:=icount;
end;

function TVBOMeshObject.OctreeRayCastIntersect(const rayStart, rayVector: TVector;
  iPoint: PVector = nil; iNormal: PVector = nil): boolean;
var i:integer;
  locRayStart, locRayVector: TVector;
  mOctree:TOctree;
  ip,n:TVector;
  f:boolean;
begin
  Assert(FOctreeList.Count>0, 'Octree must have been prepared before use RayCast.');
  result:=ExtentsIntersect(rayStart, rayVector,@ip);
  locRayStart := AbsoluteToLocal(RayStart);
  locRayVector := vectormake(VectorToLocal(affinevectormake(RayVector)));
  if result=false then exit; result:=false;
  for i:=0 to FOctreeList.Count-1 do begin
    mOctree := FOctreeList[i];
    f := mOctree.RayCastIntersect(locRayStart, locRayVector, @ip, @N);
    if f then begin
      ip:=VectorTransform(ip,Matrices.WorldMatrix);N[3]:=0;
      N:=VectorTransform(N,Matrices.WorldMatrix);
      if iPoint<>nil then iPoint^:=ip;
      if iNormal<>nil then inormal^:=N;
      result:=true;
      exit;
    end;
  end;
end;

function TVBOMeshObject.ExtentsIntersect(const rayStart,
  rayVector: TVector; iPoint:PVector=nil): boolean;
var bb:TAABB;
    ip:TVector;
begin
    bb:=TAABB(FExtents);
    result:=RayCastAABBIntersect(RayStart,RayVector,bb,@ip);
    iPoint^:=ip;
end;

procedure TVBOMeshObject.SetParent(const Value: TVBOMeshObject);
begin
  FParent := Value; UpdateWorldMatrix;
end;

procedure TVBOMeshObject.SortMeshByTransparency;
var i,j: integer;
    buff,temp: PVBOBuffer;
    mat: TMaterialObject;
    trList,opList: TList;
  procedure SortListByMaterial(var List: TList);
  var i,j,l: integer;
      buff,temp: PVBOBuffer;
  begin
    if List.Count<=2 then exit;
    for j:=0 to List.Count-2 do begin
      buff:=List[j];l:=j+1;
      for i:=j+1 to List.Count-1 do begin
        temp:=List[i];
        if buff.MatName=temp.MatName then begin
          if i<>l then List.Exchange(i,l);
          inc(l);
        end;
      end;
    end;
  end;

begin
  trList:=TList.Create; opList:=TList.Create;
  trList.Capacity:=MeshList.Count;
  opList.Capacity:=MeshList.Count;
  for i:=0 to MeshList.Count-1 do begin
    buff:=MeshList[i];
    mat:=FMatObjLib.MaterialByName(buff.MatName);
    if assigned(mat) then begin
      if mat.IsTransparency then trList.Add(buff) else opList.Add(buff);
    end;
  end;
  SortListByMaterial(trList);
  SortListByMaterial(opList);
  j:=MeshList.Count; MeshList.Clear;
  MeshList.Capacity:=j;
  for i:=0 to opList.Count-1 do MeshList.Add(opList[i]);
  for i:=0 to trList.Count-1 do MeshList.Add(trList[i]);
  trList.Free; opList.Free;
end;

procedure TVBOMeshObject.SortProxyByDistance(SortDirection: TSortDirection);
Type TPDI = record p: TVector; d: single; Obj: pointer; end;
var dmin: single;
    i,j: integer;
    pd: array of TPDI;
    t: TPDI;
    ObjList: TList;
    Temp: TList;
    pm: PMatrix;
    mv: TMatrix;
  function CompareDistanceFTB(Item1, Item2: Pointer): Integer;
  begin result:=trunc(TPDI(Item1^).d-TPDI(Item2^).d); end;
  function CompareDistanceBTF(Item1, Item2: Pointer): Integer;
  begin result:=trunc(TPDI(Item2^).d-TPDI(Item1^).d); end;
begin
  if not assigned(FProxyMatrixList) then exit;
  ObjList:=FProxyMatrixList;
  if SortDirection=sdNone then exit;
  Temp:=TList.Create; Temp.Count:=ObjList.Count;
  setlength(pd,ObjList.Count);
  for i:=0 to ObjList.Count-1 do begin
     pm:=ObjList[i]; mv:=MatrixMultiply(pm^,ParentViewer.ViewMatrix);
     pd[i].p:=mv[3]; pd[i].d:=VectorNorm(pd[i].p); pd[i].Obj:=pm;
     Temp[i]:=@pd[i];
  end;

  if SortDirection=sdFrontToBack then Temp.Sort(@CompareDistanceFTB);
  if SortDirection=sdBackToFront then Temp.Sort(@CompareDistanceBTF);
  for i:=0 to ObjList.Count-1 do ObjList[i]:=TPDI(Temp[i]^).Obj;
  Temp.Free;
end;

procedure TVBOMeshObject.RebuildProxyList(const ViewMatrix, mv: TMatrix);
var i,j,n: integer;
    pm: PMatrix;
    F: TFrustum;
    ProxyObj: TVBOMeshObject;
    d, mind: single;
    v: TVector;
begin
//  F := GetFrustum(Matrices.ProjectionMatrix, ViewMatrix);
  F := ParentViewer.Frustum;
{
  for i:=0 to FProxyMatrixList.Count-1 do begin
    pm:=FProxyMatrixList[i];
    if pm<>nil then dispose(pm);
    FProxyMatrixList[i]:=nil;
  end;
  FProxyMatrixList.Clear;
}
  if Visible then begin
    if (not IsVolumeClipped(Extents, F))
    and (not FCulled) then begin
       new(pm); pm^:=mv; FProxyMatrixList.Add(pm);
    end;
  end;
  //Frustum culling
  for i:=0 to FProxyList.Count-1 do begin
    ProxyObj:=FProxyList[i];
    if ProxyObj.Visible {and (not ProxyObj.FCulled)} then begin
      if (not ProxyObj.WorldMatrixUpdated) then ProxyObj.UpdateWorldMatrix;
      ProxyObj.FCulled:=IsVolumeClipped(ProxyObj.Extents, F);
      if not ProxyObj.FCulled then begin
        new(pm); pm^:=MatrixMultiply(ProxyObj.Matrices.WorldMatrix,ViewMatrix);
        FProxyMatrixList.Add(pm);
      end;
    end;
  end;
  exit;
  //Sorting by Distance
  for i:=0 to FProxyMatrixList.Count-2 do begin
    pm:=FProxyMatrixList[i];
    mind:=VectorNorm(pm[3]);
    for j:=i+1 to FProxyMatrixList.Count-1 do begin
      pm:=FProxyMatrixList[j];
      d:=VectorNorm(pm[3]);
      if d<mind then begin
         FProxyMatrixList.Exchange(i,j);
         mind:=d;
      end;
    end;
  end;
end;

procedure TVBOMeshObject.RemoveInstance(Index: integer);
begin
  ProxyList.Delete(Index);
  ClearProxyMatrix;
end;

procedure TVBOMeshObject.RemoveInstance(MeshObject: TVBOMeshObject);
var i: integer;
begin
  i:=ProxyList.IndexOf(MeshObject);
  if i>=0 then RemoveInstance(i);
end;

procedure TVBOMeshObject.RenderObject(const ViewMatrix:TMatrix);
var m: TMatrix;
    mv: TMatrix;
    i,j,tid: integer;
    P: PVBOBuffer;
    PDescr: PMultiRenderDescr;
    ActiveMaterial: string;
    tex: TTexture;
    MasterObj, ProxyObj: TVBOMeshObject;
    bindedBuff: pointer;
    bindState: TBindState;
    Rcount: integer;
    singleMat: boolean;
    CMName: string;
    mat: TMaterialObject;
begin
  if FMeshType = mtInstance then exit;

  if (not WorldMatrixUpdated) then UpdateWorldMatrix;
  mv:=MatrixMultiply(Matrices.WorldMatrix,ViewMatrix);
  m:=mv;
  glPushMatrix;
  ClearProxyMatrix;
  if FProxyList.Count>0 then RebuildProxyList(ViewMatrix, mv);

  if FFBO.Active then FFBO.Apply;
  mat:=nil;

  case FMeshType of
    mtScreenQuad: begin
     glMatrixMode(GL_MODELVIEW);
     glLoadIdentity;
     glMatrixMode(GL_PROJECTION);
     glPushMatrix;glLoadIdentity;
     //glDisable(GL_DEPTH_TEST);
     glDisable(GL_LIGHTING);
    end;
    mtHudSprite: begin
        glMatrixMode(GL_MODELVIEW); glPushMatrix;
        glLoadIdentity;
        glMatrixMode(GL_PROJECTION); glPushMatrix;
        m:=Matrices.WorldMatrix;
        glLoadMatrixf(PGLFloat(@m));
        //if (not assigned(FMaterial)) then
        glDisable(GL_LIGHTING);
    end;
    mtSphericalSprite: begin
      m:=CreateBBMatrix(ViewMatrix,Matrices,FRollAngle,stSpherical);
      glLoadMatrixf(PGLFloat(@m));
    end;
    mtCylindricalSprite: begin
      m:=CreateBBMatrix(ViewMatrix,Matrices,FRollAngle,stCylindrical);
      glLoadMatrixf(PGLFloat(@m));
    end;
    mtGrid, mtBBox: begin
      glDisable(GL_TEXTURE_2D);
      glDisable(GL_LIGHTING);
      glLoadMatrixf(PGLFloat(@m));
    end;
    mtPoints,mtParticles: begin
      if FMaterialObject.Active then begin
        FMaterialObject.Apply(FonMaterialApply);
      end else FMaterialObject.Blending.Apply;
      glDisable(GL_LIGHTING);
      with PPointParam(FParams)^ do begin
        glPointSize(PointSize);
        if PointSmooth then glEnable(GL_POINT_SMOOTH);
        glPointParameterfv(GL_POINT_DISTANCE_ATTENUATION, @DistanceAttenuation);
        glPointParameterf( GL_POINT_FADE_THRESHOLD_SIZE,FadeTresholdSize);
        glPointParameterf( GL_POINT_SIZE_MIN,MinPointSize);
        glPointParameterf( GL_POINT_SIZE_MAX,PointSize);
        if PointSprite then begin
          glTexEnvf( GL_POINT_SPRITE, GL_COORD_REPLACE, GL_TRUE );
          glEnable(GL_POINT_SPRITE);
        end else begin
          glTexEnvf( GL_POINT_SPRITE, GL_COORD_REPLACE, GL_FALSE );
          glDisable(GL_POINT_SPRITE);
        end;
      end;glLoadMatrixf(PGLFloat(@m));
    end;
    Else glLoadMatrixf(PGLFloat(@m));
  end;
  if NoZWrite then glDepthMask(False) else glDepthMask(True);
  if NoDepthTest then glDisable(GL_DEPTH_TEST) else glEnable(GL_DEPTH_TEST);

  if FTwoSides then begin
    //glCullFace(GL_FRONT); glDisable(GL_CULL_FACE);
    //glCullFace(GL_BACK);

    {$IFNDEF DIRECTGL}OpenGL1x.{$ELSE}dglOpenGL.{$ENDIF}glDisable(GL_CULL_FACE);
  end else
    {$IFNDEF DIRECTGL}OpenGL1x.{$ELSE}dglOpenGL.{$ENDIF}glEnable(GL_CULL_FACE);
  singleMat:=false; CMName:='';
  if not FCulled then begin
    SetFaceMode;
    if FUseRenderList then rcount:=FRenderList.Count else rcount:=MeshList.Count;
    if rcount>0 then begin
    if FMeshType<>mtParticles then begin
      ResetBlending;
      if TextureId<>0 then begin
        glActiveTexture(GL_TEXTURE0);
        glEnable(GL_TEXTURE_2D);
        glBindTexture(GL_TEXTURE_2D, TextureId);
        singleMat:=true;
      end else begin
        if FMaterialObject.Active then begin
          FMaterialObject.Apply(FonMaterialApply);
          singleMat:=true;
        end else FMaterialObject.Blending.Apply;
      end;
    end else singleMat:=true;
    if singleMat then begin
      if assigned(FonBeforeRender) then FonBeforeRender(self);
      if FUseRenderList then begin
        bindedBuff:=nil;
        for i:=0 to FRenderList.Count-1 do begin
            PDescr:=FRenderList[i];
            if bindedBuff<>PDescr.RenderBuff then begin
               BindState:=[sActivate];bindedBuff:=PDescr.RenderBuff;end;
            if i=FRenderList.Count-1 then  begin
              BindState:=BindState+[sDeactivate];
              bindedBuff:=nil;
            end;
            RenderVBOMultiPart(PDescr,BindState,false);
        end;
      end else begin
        if FProxyMatrixList.Count>0 then
          RenderVBOList(MeshList,FProxyMatrixList)
        else RenderVBOList(MeshList);
      end;
      ResetBlending;
//      glDisable(GL_TEXTURE_2D);
      if assigned(FonAfterRender) then FonAfterRender(self);
      FMaterialObject.UnApply(FonMaterialUnApply);
//      if assigned(FMaterial) then FMaterial.UnApply;
//      if assigned(FTexture) then FTexture.UnApply;
    end else begin
    //MultiMaterial
      if FUseRenderList then begin
         PDescr:=FRenderList[0]; CMName:=PDescr.MaterialName;
      end else begin
         P := MeshList[0]; CMName:=P.MatName;
      end;
      ActiveMaterial:='';
      if CMName<>'' then begin
        mat:=SetMaterialName(CMName); ActiveMaterial:=CMName;
        if assigned(mat) then begin
          if mat.Active then begin
            mat.Apply(FonMaterialApply);
          end else mat.Blending.Apply;
        end;
      end else begin
        ResetBlending; glDisable(GL_TEXTURE_2D);
        OGLStateEmul.GLStateCache.MaterialCache.Reset;
      end;
      bindedBuff:=nil; i:=0;
      repeat
         //Render+++++++++++++++++
         if assigned(FonBeforeRender) then FonBeforeRender(self);
         if FUseRenderList then begin
           PDescr:=FRenderList[i];
           if bindedBuff<>PDescr.RenderBuff then begin
              BindState:=[sActivate];bindedBuff:=PDescr.RenderBuff;end;
           if i=FRenderList.Count-1 then begin
             BindState:=BindState+[sDeactivate];
             bindedBuff:=nil;
           end;
           RenderVBOMultiPart(PDescr,BindState,false);
         end else begin
           P := MeshList[i];
           if FProxyMatrixList.Count>0 then
              RenderVBOBuffer(P^,FProxyMatrixList)
           else RenderVBOBuffer(P^);
         end;
         if assigned(FonAfterRender) then FonAfterRender(self);
         //Render-----------------
         inc(i);
         if i<rcount then begin
           if FUseRenderList then begin
             PDescr:=FRenderList[i]; CMName:=PDescr.MaterialName;
           end else begin
             P := MeshList[i]; CMName:=P.MatName;
           end;
         end else CMName:='';
         if ((i=rcount) or (CMName<>ActiveMaterial)) then
         begin
           if assigned(mat) then begin
             if mat.Active then begin
               mat.UnApply(FonMaterialUnApply);
               ResetBlending;
             end else ResetBlending;
           end;
         end;
         if (CMName<>ActiveMaterial) then begin
           if CMName<>'' then begin
             mat:=SetMaterialName(CMName); ActiveMaterial:=CMName;
             if assigned(mat) then begin
               if mat.Active then begin
                 mat.Apply(FonMaterialApply);
               end else mat.Blending.Apply;
             end else glDisable(GL_TEXTURE_2D);
           end else begin
             mat:=nil; ResetBlending;
             OGLStateEmul.GLStateCache.MaterialCache.Reset;
             glDisable(GL_TEXTURE_2D);
           end;
         end;
      until i=rcount;
      ResetBlending;
    end;
    end;//RCount<=0
  end else FCulled:=true;
  if NoZWrite then glDepthMask(true);
  if NoDepthTest then glEnable(GL_DEPTH_TEST);
  glEnable(GL_CULL_FACE);

  case FMeshType of
    mtScreenQuad: begin
     glPopMatrix; glMatrixMode(GL_MODELVIEW);
     glEnable(GL_DEPTH_TEST);
     glEnable(GL_LIGHTING);
     if assigned(mat) then
       if mat.Active then mat.UnApply(FonMaterialUnApply);
     ResetBlending;
    end;

    mtHUDSprite: begin
      glEnable(GL_LIGHTING); glEnable(GL_LIGHT0);
      glPopMatrix; glMatrixMode(GL_MODELVIEW);
      glPopMatrix;
    end;
    mtPoints, mtParticles: begin
      glTexEnvf( GL_POINT_SPRITE, GL_COORD_REPLACE, GL_FALSE );
      glDisable( GL_POINT_SPRITE );
      glDisable(GL_ALPHA_TEST);
      glDisable( GL_BLEND );
      glDepthMask(True);
      glEnable(GL_DEPTH_TEST);
      glEnable(GL_LIGHTING);
      if assigned(mat) then if mat.Active then mat.UnApply(FonMaterialUnApply);
      ResetBlending;
    end;
    mtGrid,mtBBox: glEnable(GL_LIGHTING);
  end;
  if FFBO.Active then FFBO.UnApply;
{  if FProxyList.Count>0 then begin
    ProxyObj:=FProxyList[i]; ProxyObj.FCulled:=false;
  end;
}
  glPopMatrix;
end;

procedure TVBOMeshObject.setShader(const Value: TShaderProgram);
begin
  FMaterialObject.AttachShader(Value);
end;

procedure TVBOMeshObject.setTexture(const Value: TTexture);
begin
  FMaterialObject.AttachTexture(Value); FTexture:=Value;
end;

procedure TVBOMeshObject.MaterialSetter(const MatName: string;
  Action: TMatAction);
var mat: TMaterialObject;
begin
  mat:=FMatObjLib.MaterialByName(MatName);
  if not assigned(mat) then exit;
  case Action of
    maApply: mat.Apply(FonMaterialApply);
    maUnApply: mat.UnApply(FonMaterialUnApply);
  end;

end;

function TVBOMeshObject.GetMeshMaterial(MeshIndex: integer): TMaterialObject;
var Buff: PVBOBuffer;
    matObj: TMaterialObject;
begin
  if (MeshIndex>=0) and (MeshIndex<MeshList.Count) then begin
    Buff:=MeshList[MeshIndex];
    matObj:=FMatObjLib.MaterialByName(Buff.MatName);
    if not assigned(matObj) then result:=FMaterialObject
    else result:=matObj;
  end else result:=FMaterialObject;
end;

function TVBOMeshObject.getParent: TVBOMeshObject;
begin
  result:=TVBOMeshObject(FParent);
end;

function TVBOMeshObject.GetPolyCount: integer;
var i,j,k,n,count, pmcount: integer;
    P: PVBOBuffer;
begin
   pmcount:=FProxyMatrixList.Count; if pmcount=0 then pmcount:=1;
   if FMeshType=mtInstance then begin
     result:=pmCount*TVBOMeshObject(FParams).PolygonsCount;
     FPolyCount:=Result;
     exit;
   end;

   if FPolyCount<>-1 then result:=FPolyCount*pmcount;
   count:=0;n:=0;
   for i:=0 to MeshList.Count-1 do begin
      p:=MeshList[i];
      if uIndices in p.RenderBuffs then begin
         if FUseRenderList then
         for j:=0 to high(FMultiBuffer) do begin
            for k:=0 to FMultiBuffer[j].CountList.Count-1 do
                n:=n+FMultiBuffer[j].CountList[j];
         end else n:=p.Indices.Count;
         case p.FaceType of
           GL_TRIANGLES: n:=n div 3;
           GL_TRIANGLE_STRIP: n:=n-2;
           GL_QUADS: n:=n div 4;
           GL_QUAD_STRIP: n:=n-3;
         end;
      end else begin
         n:=p.Vertexes.Count;
         case p.FaceType of
           GL_TRIANGLES: n:=n div 3;
           GL_TRIANGLE_STRIP: n:=n-2;
           GL_QUADS: n:=n div 4;
           GL_QUAD_STRIP: n:=n-3;
         end;
      end;
      count:=count+n;
   end;

   Result:=Count*pmCount; FPolyCount:=Result;
end;

function TVBOMeshObject.getShader: TShaderProgram;
begin
  result:=FMaterialObject.Shader;
end;

function TVBOMeshObject.getTexture: TTexture;
begin
  Result:=FMaterialObject.Texture;
end;

procedure TVBOMeshObject.AbsoluteToExtents;
var C:TVector;
begin
   c[0]:=-(FBaseExtents.emax[0]-FBaseExtents.emin[0])/2-FBaseExtents.emin[0];
   c[1]:=-(FBaseExtents.emax[1]-FBaseExtents.emin[1])/2-FBaseExtents.emin[1];
   c[2]:=-(FBaseExtents.emax[2]-FBaseExtents.emin[2])/2-FBaseExtents.emin[2];
   c[3]:=1;// ResetMatrices;
   MoveObject(c);
   UpdateWorldMatrix;
   AbsoluteTransform;
end;

procedure TVBOMeshObject.PackMeshes(FreeOldBuffers:boolean; BuffSize:integer);
begin
  if FUseRenderList then exit;
  ListToMultiBuffer(MeshList,FMultiBuffer,FRenderList,true,BuffSize);
  if FreeOldBuffers then FreeVBOList(MeshList);
  FUseRenderList:=true;
end;

procedure TVBOMeshObject.PackMeshesToTexture(var vtex,ntex: TTexture);
var vert,norm: array of single;
    i, size, count, len: integer;
    v,n,t:TaffineVector;
begin
  ListToMultiBuffer(MeshList,FMultiBuffer,FRenderList,false,0);
  FUseRenderList:=true;
  if not assigned(vtex) then vtex:=TTexture.Create;
  if not assigned(ntex) then ntex:=TTexture.Create;
  count:=FMultiBuffer[0].Buff.Vertexes.count;
  QuadFromCount(count,size);
  vtex.CreateRGBA32FTexture2D(size,size);
  ntex.CreateRGBA32FTexture2D(size,size);
  len:=size*size*4;
  setlength(vert,len); setlength(norm,len);
  for i:=0 to count-1 do begin
      v:=FMultiBuffer[0].Buff.Vertexes[i];
      t:=FMultiBuffer[0].Buff.TexCoords[i];
      n:=FMultiBuffer[0].Buff.Normals[i];
      vert[i*4+0]:=v[0]; vert[i*4+1]:=v[1];
      vert[i*4+2]:=v[2]; vert[i*4+3]:=t[2];

      norm[i*4+0]:=n[0]; norm[i*4+1]:=n[1];
      norm[i*4+2]:=n[2]; norm[i*4+3]:=0;
      FMultiBuffer[0].Buff.UseTwoTexturesCoord:=false;
  end;
  vtex.UploadData(@vert[0]);
  ntex.UploadData(@norm[0]);
end;

procedure TVBOMeshObject.SetBlending(const Value: TBlendingModes);
begin
  MaterialObject.Blending.SetByMode(Value);
end;

procedure TVBOMeshObject.SetFaceMode;
begin
  case FFaceMode of
    fmPoints: glPolygonMode(GL_FRONT_AND_BACK, GL_POINT);
    fmLines: glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    fmFill: glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  end;
end;

procedure TVBOMeshObject.setMasterProxy(const Value: TVBOMeshObject);
begin
  if not FIsProxy then begin
    OctreeList.Free; LodList.Free;
    FreeVBOList(MeshList); MeshList.Free;
  end; fIsProxy:=true;
  ChangeProxy(Value);
end;

procedure TVBOMeshObject.GetTriMesh(var TriMesh: TAffinevectorList);
var i:integer;
    p:PVBOBuffer;
begin
  if not assigned(TriMesh) then TriMesh:=TAffineVectorList.Create
  else TriMesh.Clear;
  for i:=0 to MeshList.Count-1 do begin
     p:=MeshList[i]; ExtractTriangles(p^,TriMesh);
  end;
end;

procedure TVBOMeshObject.PackToSubMeshes;
var vbo: PVBOBuffer;
begin
  vbo:=PackVBOListToSubmesh(MeshList);
  GenVBOBuff(vbo^,false);
  //exclude(vbo.RenderBuffs,uVao);
  vbo.MaterialFunc:=MaterialSetter;
  FreeVBOList(MeshList,false);
  MeshList.Add(vbo);
end;

procedure TVBOMeshObject.PackTriListToTexture(var vtex: TTexture);
var triMesh: TAffineVectorList;
    count,size,w: integer;
    i,j,n,k,a,b:integer;
    data:PSingleArray;
    v:TAffineVector;
begin
  {TODO: найти причину АВ}
  triMesh:=TAffineVectorList.Create;
  GetTriMesh(triMesh); count:=triMesh.Count*4;
  QuadFromCount(count,size);
  if (size-(size div 12)*12)*size+count>size*size then size:=size*2;
  vtex.CreateRGBA32FTexture2D(size,size);
  w:=size div 12; n:=0;k:=0;
  GetMem(data,size*size);
  for i:=0 to size-1 do begin
     for j:=0 to w-1 do begin
       for a:=0 to 2 do begin
         if n<count then begin
           v:=triMesh[n]; inc(n);
           for b:=0 to 2 do begin
             data[k]:=v[b]; inc(k);
           end; data[k]:=n; inc(k);
         end else begin
           for b:=0 to 3 do begin
             data[k]:=-1; inc(k);
           end;
         end;
       end;
     end;
     for j:=w*12 to size-1 do begin
            data[k]:=-1; inc(k); end;
  end;
  vtex.UploadData(data);
  freemem(Data,size*size);
end;

procedure TVBOMeshObject.SaveToStorage(const DataStorage: TDataStorage;
  const aGuid: TGUID);
begin
  DataStorage.AddObject(Self,aGuid);
end;

procedure TVBOMeshObject.SaveToStream(s: TStream);
begin
  inherited;
end;

procedure TVBOMeshObject.LoadFromStream(s: TStream);
begin
  inherited;

end;

function TVBOMeshObject.ScreenToLocal(P: TVector): TVector;
var size: TAffineVector;
    SP: TVector;
    m: TMatrix;
begin
  if FMeshType=mtHUDSprite then begin
     m:=IdentityHmgMatrix;
  end else begin
     m:=MatrixMultiply(Matrices.ViewMatrix,Matrices.ProjectionMatrix);
  end;
  SP:=VectorTransform(Position,M);
  if SP[3]<>0 then ScaleVector(SP,1/SP[3]);
  result:=VectorSubtract(P,SP);
  size:=VectorSubtract(Extents.emax,Extents.emin);
  ScaleVector(size,0.5);
  if Size[0]<>0 then Result[0]:=Result[0]/size[0];
  if Size[1]<>0 then Result[1]:=Result[1]/size[1];
  if Size[2]<>0 then Result[2]:=Result[2]/size[2];
  Result[3]:=1;
end;

procedure TVBOMeshObject.SetMaterial(const Value: TMaterial);
begin
  FMaterialObject.AttachMaterial(Value); FMaterial:=Value;
end;

function TVBOMeshObject.SetMaterialName(const Name: string): TMaterialObject;
var mat: TMaterial;
    tex: TTexture;
    matObj: TMaterialObject;
begin
   matObj:=FMatObjLib.MaterialByName(Name); Result:=matObj;
   if not assigned(matObj) then result:=FMaterialObject else result:=matObj;
end;

procedure TVBOMeshObject.ResetBlending;
begin
  OGLStateEmul.GLStateCache.BlendingCache.Reset;
  OGLStateEmul.GLStateCache.AlphaCache.Reset;
end;


procedure TVBOMeshObject.ExtentsToTranslateScale(var pos, sc: TVector);
var size: TAffineVector;
begin
  size:=VectorAdd(Extents.emax,Extents.emin);
  ScaleVector(size,0.5);
  pos:=Vectormake(size,1);
  sc:=VectorMake(VectorSubtract(Extents.emax,Extents.emin));
  ScaleVector(Sc,0.5);
end;

procedure TVBOMeshObject.RenderOccluder(var ViewMatrix: TMatrix;
  Occluder: PVBOBuffer);
var m: TMatrix;
    S,P: TVector;
    i: integer;
    buff: PVBOBuffer;
    ext: TExtents;
    bb: TAABB;
begin
    if (not WorldMatrixUpdated) then UpdateWorldMatrix;
    glPushMatrix;
    for i:=0 to MeshList.Count-1 do begin
//      ext:=PExtents(FMeshExtents[i])^;
      buff:=MeshList[i];
      ext.emin:=buff.emin; ext.emax:=buff.emax;
      //bb:=AABBUpdate(TAABB(ext),Matrices.WorldMatrix);
      //ext:=TExtents(bb);
      ext:=AABBTransform(ext, Matrices.WorldMatrix);
      ExtentsToTranslateAndScale(ext,P,S);
      m:=IdentityHmgMatrix;
      m[0,0]:=s[0]; m[1,1]:=s[1]; m[2,2]:=s[2]; m[3]:=p;
      //m:=MatrixMultiply(Matrices.WorldMatrix,m);
      m:=MatrixMultiply(m,ViewMatrix);
      case FMeshType of
        mtHudSprite: begin
            glMatrixMode(GL_MODELVIEW); glPushMatrix;
            glLoadIdentity;
            glMatrixMode(GL_PROJECTION); glPushMatrix;
            m:=Matrices.WorldMatrix;
        end;
        mtSphericalSprite: begin
          m:=CreateBBMatrix(ViewMatrix,Matrices,FRollAngle,stSpherical);
        end;
        mtCylindricalSprite: begin
          m:=CreateBBMatrix(ViewMatrix,Matrices,FRollAngle,stCylindrical);
        end;
      end;
      glLoadMatrixf(PGLFloat(@m));
      RenderVBOBuffer(Occluder^);

      case FMeshType of
        mtHUDSprite: begin
          glPopMatrix; glMatrixMode(GL_MODELVIEW);
          glPopMatrix;
        end;
      end;
    end;
    glPopMatrix;
end;

function TVBOMeshObject.PointInExtents(p: TVector): boolean;
begin
  result:=PointInAABB(P,TAABB(FExtents));
end;

procedure TVBOMeshObject.Process;
begin
  inherited Process;
  if assigned(FChilde) then begin
    if FChilde.UseParentViewer then
        FChilde.ParentViewer:=FParentViewer;
    if FProcessChilds=pcBefore then FChilde.Process;
  end;
//    RenderObject(Matrices.ViewMatrix);
    RenderObject(ParentViewer.ViewMatrix);
  if assigned(FChilde) then begin
    if FProcessChilds=pcAfter then FChilde.Process;
  end;
end;

procedure TVBOMeshObject.fUpdateExtents;
var bb: TAABB;
begin
  bb:=TAABB(FBaseExtents);
  bb:=AABBUpdate(bb,Matrices.WorldMatrix);
  FExtents:=TExtents(bb);
  FBaundedRadius:=VectorDistance(FExtents.emin, FExtents.emax) * 0.5;
  FGeomCenter:=VectorScale(VectorAdd(FExtents.emin, FExtents.emax), 0.5);
end;

procedure TVBOMeshObject.UpdateExtents;
begin
  FBaseExtents:=GetExtentsOfList(MeshList);
end;

procedure TVBOMeshObject.UpdateMaterialList;
var i: integer;
    p: PVBOBuffer;
begin
  Materials.Clear;
  for i:=0 to MeshList.Count-1 do begin
    p:=MeshList[i]; if not assigned(p) then exit;

    if (p.MatName<>'') and (Materials.IndexOf(p.MatName)<0) then
       Materials.Add(p.MatName);
  end;
end;

{ TUniformSMDRender }

procedure TUniformSMDRender.AddAnimationFromSMD(const AnimFiles: array of string);
var i: integer;
begin
  //Load Animation files
  for i:=0 to high(AnimFiles) do AddAnimation(FAnim^,AnimFiles[i]);
end;

procedure TUniformSMDRender.AttachMeshFromSMD(FileName: string);
var Mesh: TSMDFile;
    path,s,t: string;
    i: integer;
    Tex: TTexture;
    mat: TMaterialObject;
begin
  Mesh:=SMDLoad(FileName); GetMeshFormSMD(Mesh,MeshList,true);
  FBaseExtents:=GetExtentsOfList(MeshList); UpdateWorldMatrix;
  FNodeRadius:=FBaundedRadius/10;
  path:=ExtractFilePath(FileName);
  if path<>'' then if path[length(path)]<>'\' then path:=path+'\';
  for i:=0 to Mesh.Mesh.Textures.Count-1 do begin
     s:=Mesh.Mesh.Textures[i]; t:=s; delete(t,length(t)-4,4);
     Tex:=TTexture.CreateFromFile(path+s);
     Tex.Name:=t; FTextures.Add(Tex);
     mat:=TMaterialObject.Create;
     mat.AttachTexture(tex); mat.Name:=t;
     FMatObjLib.Add(mat);
  end;
end;

procedure TUniformSMDRender.BlendAnimation(AnimIdx1, AnimIdx2: integer; Frame1,
  Frame2, Factor: single);
var Nodes1,Nodes2: TSMDNodes;
    a1,a2: PSMDFile;
    f1,f2: single;
begin
  a1:=FAnim.Animations[AnimIdx1]; a2:=FAnim.Animations[AnimIdx2];
  if Frame1>=a1.FramesCount then f1:=Frame1-trunc(frame1/a1.FramesCount)*a1.FramesCount
  else f1:=Frame1;
  if Frame2>=a2.FramesCount then f2:=Frame2-trunc(frame2/a2.FramesCount)*a2.FramesCount
  else f2:=Frame2;
  uFileSMD.BlendAnimation(Nodes1,a1,a2,f1,f2,Factor);

  Frame1:=Frame1+1; Frame2:=Frame2+1;
  if Frame1>=a1.FramesCount then f1:=Frame1-trunc(frame1/a1.FramesCount)*a1.FramesCount
  else f1:=Frame1;
  if Frame2>=a2.FramesCount then f2:=Frame2-trunc(frame2/a2.FramesCount)*a2.FramesCount
  else f2:=Frame2;
  uFileSMD.BlendAnimation(Nodes2,a1,a2,f1,f2,Factor);

  if length(FCurrentFrame)<>length(Nodes1) then setlength(FCurrentFrame,length(Nodes1));
  FCurrentFrame:=BlendFrames(Nodes1,Nodes2,Factor,FBoneArray);

  FBlended:=true;
end;

procedure TUniformSMDRender.BlendWithCurrentFrame(Anim: integer; Frame,
  Factor: single);
var N2, Nodes1,Nodes2: TSMDNodes;
    a2: PSMDFile;
    f1,f2: single;
begin
  a2:=FAnim.Animations[Anim];
  if Frame>=a2.FramesCount then f2:=Frame-trunc(frame/a2.FramesCount)*a2.FramesCount
  else f2:=Frame;
  Nodes1:=a2.Frames[trunc(f2)];
  if f2+1<a2.FramesCount then Nodes2:=a2.Frames[trunc(f2+1)]
  else Nodes2:=a2.Frames[0];
  setlength(N2,length(Nodes1));
  N2:=InterpolateFrame(Nodes1,Nodes2,frac(f2));
  FCurrentFrame:=BlendFrames(FCurrentFrame,N2,Factor,FBoneArray);
  FBlended:=true;
end;

function TUniformSMDRender.CheckVisibility(const F: TFrustum): boolean;
var i: integer;
    p: TVector;
begin
  if assigned(FonUserCulling) then begin
    FonUserCulling(result); exit;
  end;

  if (not FBlended) and (not FUseBlended) then
    result:=IsVolumeClipped(FGeomCenter, FBaundedRadius, F)
  else begin
    if not WorldMatrixUpdated then UpdateWorldMatrix;
    result:=true; i:=0;
    while (i<FBones) and Result do begin
      p:=FBoneArray[i*2]; p[3]:=1; p:=VectorTransform(p,Matrices.WorldMatrix);
      result:=IsVolumeClipped(p, FNodeRadius, F); inc(i);
    end;
  end;
end;

constructor TUniformSMDRender.Create;
begin
  inherited;
  new(FAnim); FAnim.Animations:=TList.Create;
  FOldFrameNum:=-1; FMeshType:=mtActor;
  FUseBlended:=false; FBlended:=false;
  FWeightsFormat:=wfTC1p;
  FAttrNames.IndexAttrName:=''; FAttrNames.WeightsAttrName:='';
  FWeightsRebuilded:=false;
  FWeightsCount:=0;
end;

procedure TUniformSMDRender.FApplyShader(mo: TObject);
begin
  Shaders.UseProgramObject(spid);
  if (FOldFrameNum<>FramePosition) or FBlended then begin
    Shaders.SetUniforms(spId,'Bones',FBoneArray[0],FBones*2);
    FOldFrameNum:=FramePosition;
  end; if FBlended then FUseBlended:=true;
  FBlended:=false;
end;

procedure TUniformSMDRender.CreatSMDShader;
const Fragment: ansistring =
'uniform sampler2D Texture;'+#13+#10+
'varying vec2 TexCoord;'+#13+#10+
'void main(void)'+#13+#10+
'{'+#13+#10+
'  gl_FragColor = vec4(gl_Color.rgb,1.0)*texture2D(Texture, TexCoord);'+#13+#10+
'}';
var vsId, fsId: integer;
    Vertex: ansistring;
begin
Vertex:=
'const int NodesCount = '+inttostr(FBones)+';'+#13+#10+
'uniform vec4 Bones[NodesCount*2];'+#13+#10+
'varying vec2 TexCoord;'+#13+#10+
'vec3 qrot( vec4 q, vec3 v ){ return v + 2.0*cross(q.xyz, cross(q.xyz ,v) + q.w*v); }'+#13+#10+
'void main ()'+#13+#10+
'{'+#13+#10+
'  TexCoord = gl_MultiTexCoord0.xy;'+#13+#10+
'  int bIndex = int(gl_MultiTexCoord0.z*2.0);'+#13+#10+
'  vec4 qp = Bones[bIndex];'+#13+#10+
'  vec4 qo = Bones[bIndex+1];'+#13+#10+
'  vec4 vert = vec4(qrot(qo,gl_Vertex.xyz) + qp.xyz ,1.0);'+#13+#10+
'  gl_Position = gl_ModelViewProjectionMatrix*vert;'+#13+#10+
//Освещение
'  vec3 normal = qrot(qo, gl_Normal.xyz);'+#13+#10+
'  vec4 ecPosition = gl_ModelViewMatrix * vert;'+#13+#10+
'  normal = gl_NormalMatrix * normal;'+#13+#10+
'  normal = normalize(normal);'+#13+#10+
'  vec3 p = gl_LightSource[0].position.xyz - ecPosition.xyz;'+#13+#10+
'  gl_FrontColor.rgb = ((0.8 * max(dot(normal, p), 0.0)) + 0.2) * vec3(1.0);'+#13+#10+
'  gl_FrontColor.a = 1.0;'+#13+#10+
'}';

  onBeforeRender:=FApplyShader;
  onAfterRender:=FUnApplyShader;

  if assigned(Shaders) then begin
    Shaders.ClearShaderPrograms;
    Shaders.ClearShaderObject;
  end else Shaders:= TShaders.Create;
  with Shaders do begin
    if assigned(FonGetShaderText) then
      vsId := AddShaderObject(FonGetShaderText(stVertex),GL_VERTEX_SHADER)
    else
      vsId := AddShaderObject(Vertex,GL_VERTEX_SHADER);
    if assigned(FonGetShaderText) then
      fsId := AddShaderObject(FonGetShaderText(stFragment),GL_FRAGMENT_SHADER)
    else
      fsId := AddShaderObject(Fragment,GL_FRAGMENT_SHADER);

    vsId := ShaderObjectsList[vsId];
    fsId := ShaderObjectsList[fsId];
    spId:=CreateShaderProgram;
    AttachShaderObjectToProgram(vsId,spId);
    AttachShaderObjectToProgram(fsId,spId);
    LinkShaderProgram(spId);
    UseProgramObject(spid);
    SetUniforms(spid,'Texture',0);
    UseProgramObject(0);
  end;
  setlength(FBoneArray,FBones*2); FBlended:=false;
end;

procedure TUniformSMDRender.FSetAnimationByName(Value: string);
var i: integer;
    anim: PSMDFile;
begin
  for i:=0 to FAnim.Animations.Count-1 do begin
    anim:=FAnim.Animations[i];
    if anim.Name=value then begin
       FSetAnimationByNum(i); exit;
    end;
  end;
end;

procedure TUniformSMDRender.FSetAnimationByNum(Value: integer);
var anim: PSMDFile;
begin
   if (Value>=0) and (Value<FAnim.Animations.Count) then begin
      FAnimationNum:=Value;
      anim:=FAnim.Animations[FAnimationNum];
      FramePosition:=0; FOldFrameNum:=-1;
      FFramesCount:=anim.FramesCount;
   end;
end;

procedure TUniformSMDRender.FSetFrame(Value: single);
begin
  if FramePosition>=FFramesCount then FramePosition:=FFramesCount
  else if FramePosition<0 then FramePosition:=0 else FramePosition:=Value;
  if FOldFrameNum<>FramePosition then begin
    if not FBlended then begin
      if FSmoothed then
        FCurrentFrame:=GetInterpolatedBones(FAnim.Animations[FAnimationNum],FBoneArray,(FramePosition))
      else
        FCurrentFrame:=GetInterpolatedBones(FAnim.Animations[FAnimationNum],FBoneArray,trunc(FramePosition))
    end;
  end; FUseBlended:=false;
end;

procedure TUniformSMDRender.FUnApplyShader(mo: TObject);
begin
  Shaders.UseProgramObject(0);
end;

function TUniformSMDRender.GetAnimCount: integer;
begin
  result:=FAnim.Animations.Count;
end;

function TUniformSMDRender.getBone(index: integer): TVector;
begin
  result:=FBoneArray[index];
end;

function TUniformSMDRender.GetFramesCount(index: integer): integer;
var anim: PSMDFile;
begin
   if (index>=0) and (index<FAnim.Animations.Count) then begin
      anim:=FAnim.Animations[FAnimationNum];
      result:=anim.FramesCount;
   end else result:=-1;
end;

procedure TUniformSMDRender.LoadFromFile(FileName: string);
var f: TFileStream;
    t,s: TMemoryStream;
    sam: TAnimatedMeshResource;
    compressed: boolean;
begin
  f:=TFileStream.Create(FileName,fmOpenRead);
    sam:=TAnimatedMeshResource.Create(Self);
    f.ReadBuffer(Compressed,sizeof(Boolean));
    if Compressed then begin
      t:=TMemoryStream.Create;
      s:=TMemoryStream.Create;
      s.CopyFrom(f,f.Size-f.Position);
      DecompressStream(s,t); s.Free;
      sam.LoadFromStream(t); t.Free;
    end else sam.LoadFromStream(f);
    UpdateExtents; UpdateWorldMatrix;
    NodeRadius:=BaundedRadius/10;
    RotateAroundX(-pi/2);
    Smoothed:=false;
    CreatSMDShader;
  f.Free; sam.Free;
end;

procedure TUniformSMDRender.NextFrame(n: single);
begin
  if FramePosition+n>=FFramesCount then FSetFrame(FramePosition+n-FFramesCount)
  else FSetFrame(FramePosition+n);
end;

procedure TUniformSMDRender.RebuildWeights(WeightsCount: integer;
  WeightsFormat: TWeightsFormat; IndexAttrName, WeightAttrName: string);
begin
  if FWeightsRebuilded and (FWeightsCount<=WeightsCount) then exit;
  FWeightsRebuilded:=true; FWeightsCount:=WeightsCount;
end;

procedure TUniformSMDRender.RebuildWeightsToOne;
begin
  if FWeightsRebuilded and (FWeightsCount=1) then exit;
  DecreaseWeights(FAnim.Mesh.Mesh,1);
  FAnim.Mesh.Mesh.MaxWeights:=0;
  FWeightsRebuilded:=true; FWeightsCount:=1;
end;

procedure TUniformSMDRender.RenderObject(const ViewMatrix: TMatrix);
begin
  if not FWeightsRebuilded then RebuildWeightsToOne;

  inherited;
end;

procedure TUniformSMDRender.SaveToFile(FileName: string; compressed: boolean);
var f: TFileStream;
    sam: TAnimatedMeshResource;
    t: TMemoryStream;
begin
  f:=TFileStream.Create(FileName,fmCreate);
  sam:=TAnimatedMeshResource.Create(Self);
  f.WriteBuffer(Compressed,sizeof(Boolean));
  if Compressed then begin
    t:=TMemoryStream.Create;
    sam.SaveToStream(t);
    CompressStream(t,f);
    t.Free;
  end else sam.SaveToStream(f);
  f.Free; sam.Free;
end;

procedure TUniformSMDRender.setBone(Index: Integer; const Value: TVector);
begin
  FBoneArray[index]:=Value; FBlended:=true;
end;

function TUniformSMDRender.GetBoneMatrix(Index:integer): TMatrix;
var V: TVector;
    Q: TQuaternion;
begin
  V:=Bones[Index*2+1];
  Q.ImagPart:=affinevectormake(V);
  Q.RealPart:=V[3];
  Result:=QuaternionToMatrix(Q);
  V:=Bones[Index*2]; V[3]:=1;
  Result[3]:=V;
end;

procedure TUniformSMDRender.SetBoneMatrix(Index: integer; const Matrix: TMatrix);
var V:TVector;
    Q:TQuaternion;
begin
  Q:=QuaternionFromMatrix(Matrix);
  V:=VectorMake(Q.ImagPart);
  V[3]:=Q.RealPart;
  Bones[Index*2+1]:=V;
  V:=Matrix[3];
  Bones[Index*2]:=V;
  FBlended:=true;
end;

procedure TUniformSMDRender.SetBoneGlobalMatrix(Index:integer; const Matrix:TMatrix);
var Mat:TMatrix;
begin
  Mat:=MatrixMultiply(Matrix,Matrices.InvWorldMatrix);
  SetBoneMatrix(Index,Mat);
end;

function TUniformSMDRender.GetBoneGlobalMatrix(Index: integer):TMatrix;
begin
  Result:=GetBoneMatrix(Index);
  Result:=MatrixMultiply(Result,Matrices.WorldMatrix);
end;

destructor TUniformSMDRender.Destroy;
begin
  Shaders.Free; FreeList(FAnim.Animations);
  glDeleteTextures(1,@FAnim.TextureId);
  if FMeshType=mtActorProxy then MeshList.Clear
  else begin
    FAnim.Animations.Free; dispose(FAnim);
  end;

  inherited;
end;

{ TVolumetricLines }

function TVolumetricLines.AddNode(v: TAffineVector): integer;
begin
  if FCount<FMaxCapacity then begin
     result:=FCount; FNodes[FCount]:=v;
     FUpdated:=true; inc(FCount);
     if FCount=1 then begin
       FBaseExtents.emin:=v;
       FBaseExtents.emax:=v;
     end else begin
       if v[0]<FBaseExtents.emin[0] then FBaseExtents.emin[0]:=v[0];
       if v[1]<FBaseExtents.emin[1] then FBaseExtents.emin[1]:=v[1];
       if v[2]<FBaseExtents.emin[2] then FBaseExtents.emin[2]:=v[2];
       if v[0]>FBaseExtents.emax[0] then FBaseExtents.emax[0]:=v[0];
       if v[1]>FBaseExtents.emax[1] then FBaseExtents.emax[1]:=v[1];
       if v[2]>FBaseExtents.emax[2] then FBaseExtents.emax[2]:=v[2];
       FExtents:=FBaseExtents;
     end;
  end else result:=-1;
end;

procedure TVolumetricLines.AddLine(I1, I2: integer);
var fFromPos_x, fFromPos_y, fFromPos_z,
    fToPos_x, fToPos_y,fToPos_z: single;
    fVolumeWidth: single;
    fHalfVolumeWidth: single;
    v1,v2: TAffineVector;
    tc1,tc2,tc3: TAffineVectorList;
    attr: PVBOAttribute;
    VPos: integer;
begin
    fVolumeWidth:=FLineWidth;
    fHalfVolumeWidth := fVolumeWidth * 0.5;
    v1:=FNodes[I1]; v2:=FNodes[I2];
    fFromPos_x:=v1[0]; fFromPos_y:=v1[1]; fFromPos_z:=v1[2];
    fToPos_x:=v2[0]; fToPos_y:= v2[1]; fToPos_z:=v2[2];
    VPos:=FVBOBuff.ElementsCount; FUpdated:=true;

    with FVBOBuff^ do begin
      attr:=ExTexCoords[0]; tc1:=TAffineVectorList(attr.DataHandler);
      attr:=ExTexCoords[1]; tc2:=TAffineVectorList(attr.DataHandler);
      attr:=ExTexCoords[2]; tc3:=TAffineVectorList(attr.DataHandler);

      tc1[VPos]:=AffineVectorMake(0.0, 0.0, fVolumeWidth );                     // tu, tv, width
      tc2[VPos]:=AffineVectorMake(-fVolumeWidth, fHalfVolumeWidth, 0 );         // width tweaks
      tc3[VPos]:=AffineVectorMake(fToPos_x, fToPos_y, fToPos_z );               // end position
      Vertexes[VPos]:=AffineVectorMake( fFromPos_x, fFromPos_y, fFromPos_z );   // vertex 0 of quad
      inc(VPos);
      tc1[VPos]:=AffineVectorMake( 0.25, 0.0, fVolumeWidth );            // tu, tv, width
      tc2[VPos]:=AffineVectorMake( fVolumeWidth, fHalfVolumeWidth, 0 );  // width tweaks
      tc3[VPos]:=AffineVectorMake( fFromPos_x, fFromPos_y, fFromPos_z ); // end position
      Vertexes[VPos]:=AffineVectorMake( fToPos_x, fToPos_y, fToPos_z );  // vertex 1 of quad
      inc(VPos);
      tc1[VPos]:=AffineVectorMake( 0.25, 0.25, fVolumeWidth );           // tu, tv, width
      tc2[VPos]:=AffineVectorMake( -fVolumeWidth, fHalfVolumeWidth, 0 ); // width tweaks
      tc3[VPos]:=AffineVectorMake( fFromPos_x, fFromPos_y, fFromPos_z ); // end position
      Vertexes[VPos]:=AffineVectorMake( fToPos_x, fToPos_y, fToPos_z );  // vertex 2 of quad
      inc(VPos);
      tc1[VPos]:=AffineVectorMake( 0.0, 0.25, fVolumeWidth );            // tu, tv, width
      tc2[VPos]:=AffineVectorMake( fVolumeWidth, fHalfVolumeWidth, 0 );  // width tweaks
      tc3[VPos]:=AffineVectorMake( fToPos_x, fToPos_y, fToPos_z );       // end position
      Vertexes[VPos]:=AffineVectorMake( fFromPos_x, fFromPos_y, fFromPos_z );// vertex 3 of quad
      ElementsCount:=VPos+1;
    end;
end;

function TVolumetricLines.AddNode(x, y, z: single): integer;
begin
  result:=AddNode(affinevectormake(x,y,z));
end;

procedure TVolumetricLines.Clear;
begin
  FCount:=0; FUpdated:=true; FBreaks.Clear; FVBOBuff.ElementsCount:=0;
end;

constructor TVolumetricLines.Create(MaxNodes: integer = 1000);
var TC1,TC2,TC3: TAffineVectorList;
    attr: PVBOAttribute;
begin
  inherited Create;
  FNodes:=TAffineVectorList.Create;
  FNodes.Count:=MaxNodes;
  FMaxCapacity:=MaxNodes;
  FBreaks:=TIntegerList.Create;
  //CreateVBOBuffer
  FUpdated:=false;
  FCount:=0; FSId:=0;
  FShaders:=TShaders.Create;
  CreateVLShader;
  FLineWidth:=0.3;
  new(FVBOBuff);
  InitVBOBuff(FVBOBuff^,GL_QUADS,DrawArrays);
  TC1:=TAffineVectorList.Create;
  TC2:=TAffineVectorList.Create;
  TC3:=TAffineVectorList.Create;

  with FVBOBuff^ do begin
    Vertexes.Count:=MaxNodes*4;
    new(attr); TC1.Count:=MaxNodes*4; attr.Id:=0; attr.Location:=-1;
    AttributeFromList(TC1,attr^); attr.AttrType:=atTexCoord; ExTexCoords.Add(attr);

    new(attr); TC2.Count:=MaxNodes*4; attr.Id:=0; attr.Location:=-1;
    AttributeFromList(TC2,attr^); attr.AttrType:=atTexCoord; ExTexCoords.Add(attr);

    new(attr); TC3.Count:=MaxNodes*4; attr.Id:=0; attr.Location:=-1;
    AttributeFromList(TC3,attr^); attr.AttrType:=atTexCoord; ExTexCoords.Add(attr);

    UseTwoTexturesCoord:=false;
  end; GenVBOBuff(FVBOBuff^,false);
  FVBOBuff.ElementsCount:=0;
  FVBOBuff^.RenderBuffs:=[uMultitexture];
  FVBOBuff.VAO:=0;

//  AddTestQuad;
end;

procedure TVolumetricLines.CreateVLShader;
const
  FS =
      'uniform sampler2D volumeLineTexure;'+#13+#10+
      'varying vec4 Texcoord0;'+#13+#10+
      'varying vec4 Texcoord1;'+#13+#10+
      'varying vec4 col1;'+#13+#10+
      ''+#13+#10+
      'void main(void)'+#13+#10+
      '{'+#13+#10+
        '  vec4 texelColor0 = texture2D( volumeLineTexure, Texcoord0.xy );'+#13+#10+
        '  vec4 texelColor1 = texture2D( volumeLineTexure, Texcoord1.xy );'+#13+#10+
        '  gl_FragColor =  mix( texelColor0, texelColor1, col1);'+#13+#10+
      '}'+#13+#10;

  VS =//'#version 130'+#13#10+
      'varying vec4 Texcoord0;'+#13#10+
      'varying vec4 Texcoord1;'+#13#10+
      'varying vec4 col1;'+#13#10+
      ''+#13#10+
      'void main(void)'+#13#10+
      '{'+#13#10+
      '  vec4 startpos = vec4(gl_Vertex.xyz,1.0);'+#13#10+
      '  vec4 endpos = vec4(gl_MultiTexCoord2.xyz,1.0);'+#13#10+
      ''+#13#10+
      '  Texcoord0 = vec4(gl_MultiTexCoord0.xyz,1.0);'+#13#10+
      '  Texcoord1 = vec4(gl_MultiTexCoord1.xyz,1.0);'+#13#10+
      ''+#13#10+
      '  vec4 posstart = gl_ModelViewMatrix * startpos;'+#13#10+
      '  vec4 posend   = gl_ModelViewMatrix * endpos;'+#13#10+
      '  vec3 middlepoint = normalize((posstart.xyz + posend.xyz)/2.0);'+#13#10+
      ''+#13#10+
      '  vec3 lineoffset = posend.xyz - posstart.xyz; '+#13#10+
      '  vec3 linedir = normalize(lineoffset); '+#13#10+
      '  float sqlinelength = dot(lineoffset, lineoffset);'+#13#10+
      '  float texcoef = abs( dot(linedir, middlepoint) );'+#13#10+
      ''+#13#10+
      '  texcoef = max( ((texcoef - 1.0)*(sqlinelength / Texcoord1.y)) + 1.0, 0.0 );'+#13#10+
      ''+#13#10+
      '  posstart = gl_ModelViewProjectionMatrix * startpos;'+#13#10+
      '  posend   = gl_ModelViewProjectionMatrix * endpos;'+#13#10+
      ''+#13#10+
      '  vec2 startpos2d = posstart.xy / posstart.w;'+#13#10+
      '  vec2 endpos2d   = posend.xy / posend.w;'+#13#10+
      ''+#13#10+
      '  vec2 linedir2d = normalize(startpos2d - endpos2d);'+#13#10+
      '  posstart.xy = ((texcoef * Texcoord0.z) * linedir2d.xy) + posstart.xy;'+#13#10+
      ''+#13#10+
      '  linedir2d = Texcoord1.x * linedir2d;'+#13#10+
      '    '+#13#10+
      '  posstart.x = posstart.x + linedir2d.y; // vertical x'+#13#10+
      '  posstart.y = posstart.y - linedir2d.x; // vertical y'+#13#10+
      ''+#13#10+
      '  gl_Position = posstart;'+#13#10+
      ''+#13#10+
      '    float blend;'+#13#10+
      '    vec4 tex;'+#13#10+
      '    tex.zw = vec2(0.0,1.0);'+#13#10+
      '    tex.y = min(15.0/16.0, texcoef);'+#13#10+
      '    tex.x = modf(tex.y * 4.0, tex.y);'+#13#10+
      '    blend = modf(tex.x * 4.0, tex.x);'+#13#10+
      '    tex.xy = (tex.xy / 4.0) + Texcoord0.xy; '+#13#10+
      '    vec4 tc0 = tex;'+#13#10+
      ''+#13#10+
      '    tex.y = min(texcoef + (1.0/16.0), 15.0/16.0);'+#13#10+
      '    tex.x = modf(tex.y * 4.0, tex.y);'+#13#10+
      '    tex.x = floor(tex.x * 4.0);'+#13#10+
      '    tex.xy = (tex.xy / 4.0) + Texcoord0.xy; '+#13#10+
      '    Texcoord1 = tex;'+#13#10+
      '    Texcoord0 = tc0;'+#13#10+
      '    col1 = vec4(blend,blend,blend,blend);'+#13#10+
      '}'+#13+#10;

begin
  FSId:=FShaders.CreateShader(VS,FS);
//  assert(false,FShaders.Logs);
end;

destructor TVolumetricLines.Destroy;
begin
  FNodes.Free; FShaders.Free;
  FreeVBOBuffer(FVBOBuff^); Dispose(FVBOBuff);
  FBreaks.Free;
  inherited;
end;

function TVolumetricLines.GetNode(Index: Integer): TAffineVector;
begin
   if Index<FNodes.Count then result:=FNodes[Index]
   else assert(false,'Index out of Bound!');
end;

procedure TVolumetricLines.RenderObject(const ViewMatrix: TMatrix);
var i,br: integer;
    attr: PVBOAttribute;
    mv: TMatrix;
begin
  if FUpdated then with FVBOBuff^ do begin
     FVBOBuff.ElementsCount:=0; br:=0; i:=0;
     while i<FCount-1 do begin
       if FBreaks.Count>br then begin
         if i+1=FBreaks[br] then inc(br)
         else AddLine(i,i+1);
       end else AddLine(i,i+1);
       inc(i);
     end;
     if FCount>0 then begin
       UpdateVBOBuff(vId,Vertexes.List,0,(FCount-1)*48);
       attr:=ExTexCoords[0]; UpdateVBOBuff(attr.Id,attr.Data,0,(FCount-1)*48);
       attr:=ExTexCoords[1]; UpdateVBOBuff(attr.Id,attr.Data,0,(FCount-1)*48);
       attr:=ExTexCoords[2]; UpdateVBOBuff(attr.Id,attr.Data,0,(FCount-1)*48);
       FVBOBuff.ElementsCount:=(FCount-1)*4;
       FVBOBuff.MaxElements:=FVBOBuff.ElementsCount;
       FVBOBuff.VertexCount:=FVBOBuff.ElementsCount;
     end;
     FUpdated:=false;
  end;
  glDisable(GL_CULL_FACE);
  glDepthMask( FALSE );
  glEnable( GL_DEPTH_TEST );

  glPushMatrix;
  if WorldMatrixUpdated then UpdateWorldMatrix;
  mv:=MatrixMultiply(Matrices.WorldMatrix,ViewMatrix);
  //glLoadMatrixf(PGLFloat(@ViewMatrix));
  glLoadMatrixf(@mv);
  MaterialObject.Apply;
  FShaders.UseProgramObject(FSId);
  FShaders.SetUniforms(FSId,'volumeLineTexure',0);
//  if FTexture<>nil then FTexture.Apply(0);
//  SetBlending;
  RenderVBOBuffer(FVBOBuff^);
//  if FTexture<>nil then FTexture.UnApply(0);
  FShaders.UseProgramObject(0);
  MaterialObject.UnApply;
  glPopMatrix;
  glEnable(GL_CULL_FACE);
end;

procedure TVolumetricLines.SetNode(Index: Integer;
  const Value: TAffineVector);
begin
   if Index<FNodes.Count then begin
      FNodes[Index]:=Value; FUpdated:=true; end
   else assert(false,'Index out of Bound!');
   AABBInclude(TAABB(FBaseExtents),Value);
   WorldMatrixUpdated:=false;
end;

procedure TVolumetricLines.AddTestQuad;
begin
  with FVBOBuff^ do begin
    Vertexes[0]:=affineVectorMake(-0.2,-0.1, 2);
    Vertexes[1]:=affineVectorMake(-0.2, 0.1, 2);
    Vertexes[2]:=affineVectorMake( 0.2, 0.1, 2);
    Vertexes[3]:=affineVectorMake( 0.2,-0.1, 2);
  end;
  UpdateVBOBuff(FVBOBuff.vId,FVBOBuff.Vertexes.List,0,48);
  FVBOBuff.ElementsCount:=4;
  FVBOBuff.VertexCount:=4;
  FVBOBuff.MaxElements:=4;
end;

procedure TVolumetricLines.BreakLine;
begin
  FBreaks.Add(FCount);
end;

{ TVBOParticles }

function TVBOParticles.AddParticle(Position, Color: TVector): integer;
begin
  result:=FCount;
  FPositions[FCount]:=affinevectormake(Position);
  if FPointParam.UseColors then FColors[FCount]:=Color;
  if Immediate then FUpdateBuffer(Result)
  else begin
    FVNeedUpdate:=true;
    FCNeedUpdate:=true;
  end;
  AABBInclude(TAABB(FBaseExtents),affinevectormake(Position));
  fUpdateExtents;
  inc(FRealCount);
end;

procedure TVBOParticles.AddAcceleration(Acceleration: TAffineVector;
  index: integer);
begin
  assert(index<FCount,'Index ['+inttostr(index)+'] Out of Bound');
  if (index>0) and (index<FAccelList.Count) then FAccelList[index]:=Acceleration
  else begin
    if FCount>=FAccelList.Count then FAccelList.Count:=FCount;
    FAccelList[FCount-1]:=Acceleration;
  end;

end;

procedure TVBOParticles.AddMass(Mass: single; index: integer);
begin
  assert(index<FCount,'Index ['+inttostr(index)+'] Out of Bound');
  if (index>0) and (index<FMassList.Count) then FMassList[index]:=mass
  else begin
    if FCount>=FMassList.Count then FMassList.Count:=FCount;
    FMassList[FCount-1]:=mass;
  end;
end;

function TVBOParticles.AddParticle(Position: TVector): integer;
begin
  result:=AddParticle(Position, vectormake(1,1,1,1));
  AABBInclude(TAABB(FBaseExtents),affinevectormake(Position));
  fUpdateExtents; inc(FRealCount);
end;

procedure TVBOParticles.AddVelocity(Velocity: TAffineVector; index: integer);
begin
  assert(index<FCount,'Index ['+inttostr(index)+'] Out of Bound');
  if (index>0) and (index<FVelocityList.Count) then begin
     FVelocityList[index]:=Velocity;
  end else begin
    if FCount>=FVelocityList.Count then FVelocityList.Count:=FCount;
    FVelocityList[FCount-1]:=Velocity;
  end;
end;

constructor TVBOParticles.Create(MaxCapacity:integer=-1);
var mvc:integer;
begin
  inherited Create;
  FImmediate:=true;
  FVNeedUpdate:=false;
  FCNeedUpdate:=false;
  FRealCount:=0;
  FMeshType:=mtParticles;
  FPositions:=TAffineVectorList.Create;
  FColors:=TVectorList.Create;

  FSortedColors:=TVectorList.Create;
  FSortedPositions:=TAffineVectorList.Create;
  FSortParticles:=sdNone;

  if MaxCapacity=-1 then begin
     mvc:=GetMaxVertexCount;
     if mvc>2000000 then mvc:=1000000;
     FMaxCapacity:=mvc;
  end else FMaxCapacity:=MaxCapacity;
  FPositions.Count:=FMaxCapacity;
  FColors.Count:=FMaxCapacity;
  With FPointParam do begin
      //функция задающая уменьшение размера по мере отдаления от камеры
    DistanceAttenuation:=Affinevectormake( 1.0, 0.0, 0.01 );
      //задает "усечение" спрайта до заданного размера
    FadeTresholdSize:= 60;
      //минимальный размер спрайта
    MinPointSize:= 2;
      //размер спрайта
    PointSize:=64;
      //это лучше всегда держать выключенным, задает "аппаратное сглаживание" спрайта,
      //тоесть квадрат вырождается в круг. Тормоза страшные.
    PointSmooth:= false;
      //говорит как будет выглядеть эта точка, как точка (квадрат с заданным материалом)
      //или как текстурированный спрайт
    PointSprite:=true;
      //режимы теста/записи глубины, чтоб не рисовалось за объектами и само не писалось
    NoDepthTest:=false; NoZWrite:=true;
      //Использовать ли буфер цвета
    UseColors:=true;
  end;
  FParams:=@FPointParam;
  FAddNewBuffer;

  FVelocityList:=TAffineVectorList.Create;
  FAccelList:=TAffineVectorList.Create;
  FMassList:=TSingleList.Create;
  Visible:=true;
end;

destructor TVBOParticles.Destroy;
begin
  FPositions.Clear; FColors.Clear;
  FPositions.Free; FColors.Free;
  FVelocityList.Clear;FVelocityList.Free;
  FAccelList.Clear; FAccelList.Free;
  FMassList.Clear; FMassList.Free;
  FSortedColors.Free; FSortedPositions.Free;
  inherited;
end;

function TVBOParticles.FAddNewBuffer: PVBOBuffer;
var LastBuff: PVBOBuffer;
begin
  new(LastBuff);
  InitVBOBuff(LastBuff^,GL_POINTS,DrawArrays);
  with LastBuff^ do begin
    RenderBuffs:=[];
    Vertexes.Count:=FMaxCapacity;
    if FPointParam.UseColors then begin
       Colors.Count:=FMaxCapacity;
       RenderBuffs:=RenderBuffs+[uColors];
    end;
  end;
  GenVBOBuff(LastBuff^,false);
  LastBuff^.ElementsCount:=0;
  MeshList.Add(LastBuff);
  Result:=LastBuff;
end;


function TVBOParticles.GetAccel(Index: Integer): TVector;
begin
   Result:=vectormake(FAccelList[Index]);
end;

function TVBOParticles.GetColor(Index: Integer): TVector;
begin
   Result:=FColors[Index];
end;

function TVBOParticles.GetMass(Index: Integer): single;
begin
   result:=FMassList[Index];
end;

function TVBOParticles.GetPosition(Index: Integer): TVector;
begin
   Result:=vectormake(FPositions[Index]);
end;

function TVBOParticles.GetVel(Index: Integer): TVector;
begin
   Result:=vectormake(FVelocityList[Index]);
end;

function TVBOParticles.FGetSpriteData(Index: Integer): TPointSpriteData;
begin
  Result.v:=vectormake(FPositions[index]);
  Result.c:=FColors[index];
end;

procedure TVBOParticles.SetAccel(Index: Integer; const Value: TVector);
begin
   FAccelList[Index]:=affinevectormake(Value);
end;

procedure TVBOParticles.SetColor(Index: integer; v: TVector);
begin
  FColors[Index]:=v;
  if Immediate then FUpdateBuffer(Index,[upColor])
  else FCNeedUpdate:=true;
end;

procedure TVBOParticles.SetCount(const Value: integer);
var i:integer;
    p:PVBOBuffer;
begin
  if Value=-1 then FCount:=FRealCount
  else if (Value<FRealCount) and (Value>=0) then
  FCount := Value;
  for i := 0 to MeshList.Count-1 do begin
    p:=MeshList[i];
    p.ElementsCount:=FCount;
  end;
end;

procedure TVBOParticles.SetExtents(const Value: TExtents);
begin
  FBaseExtents := Value;
  UpdateWorldMatrix;
end;

procedure TVBOParticles.SetImmediate(const Value: boolean);
begin
  FImmediate := Value;
end;

procedure TVBOParticles.SetMass(Index: Integer; const Value: single);
begin
   FMassList[Index]:=Value;
end;

procedure TVBOParticles.SetPosition(Index: integer; v: TVector);
begin
  FPositions[Index]:=affinevectormake(v);
  if Immediate then FUpdateBuffer(Index,[upVertex])
  else FVNeedUpdate:=true;
end;

procedure TVBOParticles.setUseColors(const Value: boolean);
begin
  assert(FCount=0,'"UseColors" must be set before adding particles!');
  FPointParam.UseColors := Value;
end;

procedure TVBOParticles.SetVel(Index: Integer; const Value: TVector);
begin
   FVelocityList[Index]:=AffineVectorMake(Value);
end;

procedure TVBOParticles.SortParticles(const VM: TMatrix; SortDirection: TSortDirection);
Type TPDI = record p: TVector; d: single; RealIndex: integer; end;
var dmin: single;
    i,j,curr: integer;
    pd: array of TPDI;
    t: TPDI;
    Temp: TList;
  function CompareDistanceFTB(Item1, Item2: Pointer): Integer;
  begin result:=trunc(TPDI(Item1^).d-TPDI(Item2^).d); end;
  function CompareDistanceBTF(Item1, Item2: Pointer): Integer;
  begin result:=trunc(TPDI(Item2^).d-TPDI(Item1^).d); end;
begin
  if SortDirection=sdNone then exit;
  Temp:=TList.Create; Temp.Count:=FCount;
  FSortedPositions.Count:=FCount;
  FSortedColors.Count:=FCount;
  setlength(pd,FCount);
  for i:=0 to FCount-1 do begin
     pd[i].p:=VectorTransform(Vectormake(FPositions[i]),vm);
     pd[i].d:=VectorNorm(pd[i].p); pd[i].RealIndex:=i;
     Temp[i]:=@pd[i];
  end;

  if SortDirection=sdFrontToBack then Temp.Sort(@CompareDistanceFTB);
  if SortDirection=sdBackToFront then Temp.Sort(@CompareDistanceBTF);
  for i:=0 to FCount-1 do begin
    FSortedPositions[i]:=FPositions[TPDI(Temp[i]^).RealIndex];
    if UseColors then FSortedColors[i]:=FColors[TPDI(Temp[i]^).RealIndex];
  end; Temp.Free;
end;

procedure TVBOParticles.FSetSpriteData(Index: integer;
  SpriteData: TPointSpriteData);
begin
  FPositions[index]:=affinevectormake(SpriteData.v);
  FColors[index]:=SpriteData.c;
  if Immediate then FUpdateBuffer(Index)
  else begin
    FVNeedUpdate:=true;
    FCNeedUpdate:=true;
  end;
end;

procedure TVBOParticles.FUpdateBuffer(Index: integer;UpdBuff:TUpdateBuff=[upVertex,upColor]);
var Buff: PVBOBuffer;
    v:TAffineVector;
    c:TVector;
    count:integer;
begin
    count:=index div FMaxCapacity;
    if count=MeshList.Count then begin
       Buff:=FAddNewBuffer; MeshList.Add(Buff);
    end else Buff:=MeshList[count];
    count:=index-count*FMaxCapacity;
    if upVertex in UpdBuff then begin
       v:=FPositions[index]; Buff.Vertexes[count]:=v;
       UpdateVBOBuff(Buff.vId,@v,count*sizeof(v),sizeof(v));
    end;
    if FPointParam.UseColors and (upColor in UpdBuff) then begin
      c:=FColors[index]; Buff.Colors[count]:=c;
      UpdateVBOBuff(Buff.cId,@c,count*sizeof(c),sizeof(c));
    end;
    if count=Buff.ElementsCount then begin
       inc(FCount); Buff.ElementsCount:=Buff.ElementsCount+1;
    end;
end;

procedure TVBOParticles.FUpdateFromSortedList(Index:integer; Count:integer;
                        UpdBuff:TUpdateBuff);
var i,offs,size:integer;
    Buff:PVBOBuffer;
begin
  for i:=0 to MeshList.Count-1 do begin
    Buff:=MeshList[i];
    if upVertex in UpdBuff then begin
       offs:=Index*sizeof(TAffinevector);
       size:=Count*sizeof(TAffinevector);
       UpdateVBOBuff(Buff.vId,FSortedPositions.List,offs,size,false);
    end;
    if FPointParam.UseColors and (upColor in UpdBuff) then begin
       offs:=Index*sizeof(TVector);
       size:=Count*sizeof(TVector);
       UpdateVBOBuff(Buff.cId,FSortedColors.List,offs,size,false);
    end;
  end;
end;


Procedure TVBOParticles.FUpdateBuffer(Index:integer; Count:integer;
                        UpdBuff:TUpdateBuff=[upVertex,upColor]);
var i,offs,size:integer;
    Buff:PVBOBuffer;
begin
  for i:=0 to MeshList.Count-1 do begin
    Buff:=MeshList[i];
    if upVertex in UpdBuff then begin
       offs:=Index*sizeof(TAffinevector);
       size:=Count*sizeof(TAffinevector);
       UpdateVBOBuff(Buff.vId,FPositions.List,offs,size,false);
    end;
    if FPointParam.UseColors and (upColor in UpdBuff) then begin
       offs:=Index*sizeof(TVector);
       size:=Count*sizeof(TVector);
       UpdateVBOBuff(Buff.cId,FColors.List,offs,size,false);
    end;
  end;
end;

procedure TVBOParticles.RenderObject(const ViewMatrix: TMatrix);
begin
  if assigned(FPFXManager) then begin
    FPFXManager.SetUpdateCounts(FCount);
    if FPFXManager.UpdateParticles then begin
       if FSortParticles=sdNone then
          FUpdateBuffer(0,FCount,[upVertex]);
       FVNeedUpdate:=false;
    end;
  end;
  if FSortParticles<>sdNone then begin
    SortParticles(ViewMatrix,FSortParticles);
    if UseColors then
      FUpdateFromSortedList(0,FCount,[upVertex, upColor])
    else FUpdateFromSortedList(0,FCount,[upVertex]);
    FVNeedUpdate:=false; FCNeedUpdate:=false;
  end;

  if FVNeedUpdate then FUpdateBuffer(0,FCount,[upVertex]);
  if FCNeedUpdate then FUpdateBuffer(0,FCount,[upColor]);
  FVNeedUpdate:=false; FCNeedUpdate:=false;
  inherited;
end;

procedure TVBOParticles.SetPFXManager(const Value: TPFXManager);
begin
  FPFXManager := Value;
  if assigned(FPFXManager) then
    FPFXManager.AssignParticles(FPositions,FVelocityList,FAccelList,FMassList);
end;

{ TVBOAnimatedSprite }

constructor TVBOAnimatedSprite.Create;
begin
   inherited;
   FFramesDir:=fdHorizontal;
   FFramesCount:=0;
   FHorFramesCount:=1;
   FVertFramesCount:=1;
   FAnimated:=false;
   FFrameRate:=25;
   FDeltaFrameTime:=1/25;
   FFrameWidth:=1;
   FFrameHeight:=1;
   FOldTime:=-1;
   FFrameNum:=-1;
   FPlayDir:=1;
   FHorInv:=false;
   FVerInv:=false;
end;

destructor TVBOAnimatedSprite.Destroy;
begin
  FAnimated:=false;
  inherited;
end;

procedure TVBOAnimatedSprite.FirstFrame;
begin
  FrameNum:=0;
end;

function TVBOAnimatedSprite.FlipHorizontal: boolean;
begin
   FHorInv:=not FHorInv;  result:=FHorInv;
end;

function TVBOAnimatedSprite.FlipVertical: boolean;
begin
   FVerInv:=not FVerInv;  result:=FVerInv;
end;

procedure TVBOAnimatedSprite.LastFrame;
begin
  FrameNum:=FFramesCount;
end;

procedure TVBOAnimatedSprite.NextFrame;
begin
  FrameNum:=FFrameNum+FPlayDir;
end;

procedure TVBOAnimatedSprite.Play(Mode:TSpriteAnimationMode);
begin
  FOldTime:=-1; FAnimated:=True;
  FAnimationMode:=mode;
end;

procedure TVBOAnimatedSprite.PrevFrame;
begin
  FrameNum:=FFrameNum-FPlayDir;
end;

procedure TVBOAnimatedSprite.RenderObject(const ViewMatrix: TMatrix);
begin
  if FAnimated then begin
    if FOldTime=-1 then FOldTime:=FTime else begin
      if (FTime-FOldTime)>FDeltaFrameTime then begin
       FOldTime:=FTime; NextFrame;
      end;
    end;
  end;
  inherited;
end;

procedure TVBOAnimatedSprite.SetFrameRate(const Value: single);
begin
  FFrameRate := Value;
  FDeltaFrameTime:=1/Value;
end;

procedure TVBOAnimatedSprite.SetHorFramesCount(const Value: integer);
begin
  assert(Value<>0,'Illegal Horizontal Frames Count');
  FHorFramesCount := Value;
  FFrameWidth := 1/Value;
end;

procedure TVBOAnimatedSprite.SetVertFramesCount(const Value: integer);
begin
  assert(Value<>0,'Illegal Vertical Frames Count');
  FVertFramesCount := Value;
  FFrameHeight := 1/Value;
end;

procedure TVBOAnimatedSprite.Stop;
begin
  FAnimated:=False;
end;

procedure TVBOAnimatedSprite.ToFrame(index: integer);
begin
  FrameNum:=index;
end;

procedure TVBOAnimatedSprite.UpdatePosition(const Value: integer);
var x,y,i,n:integer;
    s,t,p,q:single;
    buff:PVBOBuffer;
    tc:array[0..3] of TAffineVector;
begin
  if FFrameNum=Value then exit;
  n:=Value;
  case FAnimationMode of
    samNone: FAnimated:=False;
    samPlayOnce:
      if n>=FFramesCount then
      begin
        FAnimated:=False; FAnimationMode:=samNone;
        if Assigned(OnEndFrameReached) then OnEndFrameReached;
      end;
    samLoop: if n=FFramesCount then
    begin
     n:=0;
     if Assigned(OnEndFrameReached) then OnEndFrameReached;
    end;
    samLoopBackward: begin
        if (n=FFramesCount) or (n=-1) then FPlayDir:=-FPlayDir;
        n:=n+FPlayDir;
    end;
  end;

  FFrameNum := n;
  y:=trunc(n/FHorFramesCount);
  x:=n-y*FHorFramesCount;
  if not FHorInv then begin
     s:=x*FFrameWidth;
     p:=s+FFrameWidth;
  end else begin
     p:=x*FFrameWidth;
     s:=p+FFrameWidth;
  end;
  if not FVerInv then begin
     t:=1-y*FFrameHeight;
     q:=t-FFrameHeight;
  end else begin
     q:=1-y*FFrameHeight;
     t:=q-FFrameHeight;
  end;

  for i := 0 to MeshList.Count - 1 do begin
    buff:=MeshList[0];
    setvector(tc[0], p, q, 0);  setvector(tc[1], s, q, 0);
    setvector(tc[2], s, t, 0);  setvector(tc[3], p, t, 0);
    UpdateVBOBuff(Buff.tId,@tc[0],0,4*sizeof(TAffineVector));
  end;
end;

{ TSkeletalRender }

procedure TSkeletalRender.AddScreenQuad(SQ: TVBOMeshObject);
begin
  FScreenQuad:=SQ;
  FScreenQuad.onBeforeRender:=FApplyShader;
  FScreenQuad.onAfterRender:=FUnApplyShader;

end;

procedure TSkeletalRender.Assign(const SMD: TSkeletalRender);
begin
  FAnim.Mesh:=SMD.FAnim.Mesh;
  FAnim.Animations:=TList.Create;
  FAnim.Animations.Assign(SMD.FAnim.Animations);
  GetMeshFormSMD(FAnim.Mesh,MeshList,true);
  FBaseExtents:=SMD.BaseExtents;
  FAnim.TextureId:=SMD.FAnim.TextureId;
end;

procedure TSkeletalRender.FApplyShader(mo: TObject);
var Frame: TVector;
    pos: single;
begin
  glActiveTexture(GL_TEXTURE0);
  glEnable(GL_Texture_2D);
  glBindTexture(GL_TEXTURE_2D,vtex.Handle);
  glActiveTexture(GL_TEXTURE1);
  glEnable(GL_Texture_2D);
  glBindTexture(GL_TEXTURE_2D,ntex.Handle);

  glActiveTexture(GL_TEXTURE2);
  glEnable(GL_TEXTURE_RECTANGLE);
  glBindTexture(GL_TEXTURE_RECTANGLE,FAnim.TextureId);
  pos:=frac(FramePosition);
  Frame[0]:=trunc(FramePosition)*2; Frame[1]:=0;
  Frame[2]:=trunc(FramePosition+1)*2; Frame[3]:=FAnim.Mesh.NodesCount*FAnimationNum;
//  if Frame[2]>=FFramesCount then Frame[2]:=0;
  Shaders.UseProgramObject(spid);
//  Shaders.SetUniforms(spid,'VertexTexture',0);
//  Shaders.SetUniforms(spid,'NormalTexture',1);
//  Shaders.SetUniforms(spid,'BoneTexture',2);
  Shaders.SetUniforms(spid,'frame',Frame);
  Shaders.SetUniforms(spid,'alpha',pos);
  //Visible:=false;
end;

constructor TSkeletalRender.Create;
begin
  inherited Create;
  new(FAnim); FAnim.Animations:=TList.Create;
  FMeshType:=mtActor;
end;

procedure TSkeletalRender.CreatSMDShader;
const Vertex: ansistring =
'varying vec2 TexCoord;'+#13+#10+
'void main ()'+#13+#10+
'{'+#13+#10+
'  TexCoord = gl_MultiTexCoord0.xy;'+#13+#10+
'  gl_Position = ftransform();'+#13+#10+
'}';
const Fragment: ansistring =
'#extension GL_ARB_texture_rectangle : enable'+#13+#10+
'uniform sampler2D VertexTexture;'+#13+#10+
'uniform sampler2D NormalTexture;'+#13+#10+
'uniform sampler2DRect BoneTexture;'+#13+#10+
'uniform vec4 frame;'+#13+#10+
'uniform float alpha;'+#13+#10+
'varying vec2 TexCoord;'+#13+#10+
'vec3 qrot( vec4 q, vec3 v ){ return v + 2.0*cross(q.xyz, cross(q.xyz ,v) + q.w*v); }'+#13+#10+
'vec4 qlerp (in vec4 q1, in vec4 q2, in float t)'+#13+#10+
'{'+#13+#10+
'  float inner = dot( q1, q2 );'+#13+#10+
'  if(inner < 0.0 ) q2 = -q2;'+#13+#10+
'  q2 -= q1;'+#13+#10+
'  return q1 + q2*t;'+#13+#10+
'}'+#13+#10+
'void main(void)'+#13+#10+
'{'+#13+#10+
'  vec4 v = texture2D(VertexTexture, TexCoord);'+#13+#10+
'  vec4 n = texture2D(NormalTexture, TexCoord);'+#13+#10+
'  vec4 qp = texture2DRect(BoneTexture, vec2(frame.x,frame.y+v.w+frame.w));'+#13+#10+
'  vec4 qo = texture2DRect(BoneTexture, vec2(frame.x+1.0,frame.y+v.w+frame.w));'+#13+#10+
//'  vec4 qp_ = texture2DRect(BoneTexture, vec2(frame.z,frame.w+v.w));'+#13+#10+
//'  vec4 qo_ = texture2DRect(BoneTexture, vec2(frame.z+1.0,frame.w+v.w));'+#13+#10+
//'  qp = mix(qp, qp_, alpha);'+#13+#10+
//'  qo = qlerp(qo, qo_, alpha); normalize(qo);'+#13+#10+
'  gl_FragData[0] = vec4(qrot(qo,v.xyz) + qp.xyz ,1.0);'+#13+#10+
'  gl_FragData[1] = vec4(qrot(qo, n.xyz), 1.0);'+#13+#10+
'}';

var vsId, fsId: integer;
begin
  Shaders:= TShaders.Create;
  with Shaders do begin
    vsId := AddShaderObject(Vertex,GL_VERTEX_SHADER);
    fsId := AddShaderObject(Fragment,GL_FRAGMENT_SHADER);
    vsId := ShaderObjectsList[vsId];
    fsId := ShaderObjectsList[fsId];
    spId:=CreateShaderProgram;
    AttachShaderObjectToProgram(vsId,spId);
    AttachShaderObjectToProgram(fsId,spId);
    LinkShaderProgram(spId);
    UseProgramObject(spid);
    SetUniforms(spid,'VertexTexture',0);
    SetUniforms(spid,'NormalTexture',1);
    SetUniforms(spid,'BoneTexture',2);
    UseProgramObject(0);
  end;
end;

destructor TSkeletalRender.Destroy;
begin
  Shaders.Free; FreeList(FAnim.Animations);
  glDeleteTextures(1,@FAnim.TextureId);
  if FMeshType=mtActorProxy then MeshList.Clear
  else begin
    FAnim.Animations.Free; dispose(FAnim);
  end;

  inherited;
end;

procedure TSkeletalRender.FSetAnimationByName(Value: string);
var i: integer;
    anim: PSMDFile;
begin
  for i:=0 to FAnim.Animations.Count-1 do begin
    anim:=FAnim.Animations[i];
    if anim.Name=value then begin
       FSetAnimationByNum(i); exit;
    end;
  end;
end;

procedure TSkeletalRender.FSetAnimationByNum(Value: integer);
var anim: PSMDFile;
begin
   if (Value>=0) and (Value<FAnim.Animations.Count) then begin
      FAnimationNum:=Value;
      anim:=FAnim.Animations[FAnimationNum];
      FramePosition:=0;
      FFramesCount:=anim.FramesCount;
   end;
end;

procedure TSkeletalRender.FSetFrame(Value: single);
begin
  if FramePosition>=FFramesCount then FramePosition:=FFramesCount
  else if FramePosition<0 then FramePosition:=0 else FramePosition:=Value;
  FScreenQuad.Visible:=true; //Visible:=false;
end;

procedure TSkeletalRender.FUnApplyShader(mo: TObject);
begin
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D,0);
  glDisable(GL_Texture_2D);

  glActiveTexture(GL_TEXTURE1);
  glBindTexture(GL_TEXTURE_2D,0);
  glDisable(GL_Texture_2D);

  glActiveTexture(GL_TEXTURE2);
  glBindTexture(GL_TEXTURE_RECTANGLE,0);
  glDisable(GL_TEXTURE_RECTANGLE);

  glActiveTexture(GL_TEXTURE0);

  Shaders.UseProgramObject(0);
  FScreenQuad.Visible:=false;

//  Visible:=true;

  with MultiBuffer[0].Buff do begin
    vid:=rvtex.PBOReadBuffer;
    nid:=rntex.PBOReadBuffer;
    nccount:=4; vccount:=4;
  end;
end;

function TSkeletalRender.GetAnimCount: integer;
begin
  result:=FAnim.Animations.Count;
end;

procedure TSkeletalRender.NextFrame(n: single);
begin
  if FramePosition+n>=FFramesCount then FSetFrame(FramePosition+n-FFramesCount)
  else FSetFrame(FramePosition+n);
end;

procedure TSkeletalRender.RenderObject(const ViewMatrix: TMatrix);
begin
  if FAnimationNum=-1 then exit;
  inherited;
  //exit;
  if FScreenQuad.Visible then
     FScreenQuad.RenderObject(ViewMatrix);
  FScreenQuad.Visible:=false;
end;

{ TVBOTerrain }

procedure TVBOTerrain.BuildTerrain(Width, Height: integer;
  GetHeightFunc: TGetHeightFunc; TileSize: integer);
begin
  FreeVBOList(MeshList);
  FHMHeight:=Height; FHMWidth:=Width;
  GetHeights(Width, Height, GetHeightFunc);
  CreateSolidBuffer;
end;

constructor TVBOTerrain.Create;
begin
  inherited;
  FCreateBuff:=[uNormals, uTexCoords, uIndices];
  //FaceMode:=fmTriangles;
  FExtList:=TList.Create;
  FOffsList:=TIntegerList.Create;
  FCountList:=TIntegerList.Create;
  FTileSize:=32;
end;

destructor TVBOTerrain.Destroy;
begin
  FreeList(FExtList);
  FOffsList.Free;
  FCountList.Free;
  inherited;
end;

procedure TVBOTerrain.GetHeights(Width, Height: integer;
  GetHeightFunc: TGetHeightFunc);
var i, j : integer;
begin
  FHMWidth := Width;
  FHMHeight := Height;
  setlength(FHMap, FHMHeight, FHMWidth);
  for i := 0 to FHMHeight - 1 do begin
    for j := 0 to FHMWidth - 1 do FHMap[i, j] := GetHeightFunc(FHMHeight-i,j);
  end;
end;

procedure TVBOTerrain.RebuildNormals;
var i: integer;
    n: TAffineVector;
    v1,v2,v3: TAffineVector;
    Buff: PVBOBuffer;
begin
  Buff:=MeshList[0];
  for i:=0 to Buff.Normals.Count-1 do Buff.Normals[i]:=NullVector;
  for i:=0 to (Buff.Indices.Count div 3)-1 do begin
    v1:=Buff.Vertexes[Buff.Indices[i*3]];
    v2:=Buff.Vertexes[Buff.Indices[i*3+1]];
    v3:=Buff.Vertexes[Buff.Indices[i*3+2]];
    n:=CalcPlaneNormal(v1,v2,v3);
    Buff.Normals.TranslateItem(Buff.Indices[i*3],n);
    Buff.Normals.TranslateItem(Buff.Indices[i*3+1],n);
    Buff.Normals.TranslateItem(Buff.Indices[i*3+2],n);
  end;
  Buff.Normals.Normalize;
  UpdateVBOBuff(Buff.nId,Buff.Normals.List,0,Buff.Normals.DataSize,true);
end;

procedure TVBOTerrain.RenderObject(const ViewMatrix: TMatrix);
var mv: TMatrix;
    ProjectionMatrix: TMatrix;
    F: TFrustum;
    i,Offs,RCount: integer;
    patch: PVBOBuffer;
    emin, emax: TAffineVector;
    ObjPos: TAffineVector;
    Radius: single;
    pExt: PExtents;
begin
  glGetFloatv(GL_PROJECTION_MATRIX, @ProjectionMatrix);
  F := GetFrustum(ProjectionMatrix, ViewMatrix);
  glPushMatrix;
    if not WorldMatrixUpdated then UpdateWorldMatrix;
    mv:=MatrixMultiply(Matrices.WorldMatrix, ViewMatrix);
    glLoadMatrixf(PGLFloat(@mv));
    if assigned(FonBeforeRender) then FonBeforeRender(self);
    SetFaceMode;
    if FTwoSides then begin
      //glCullFace(GL_FRONT); glDisable(GL_CULL_FACE);
      //glCullFace(GL_BACK);

      {$IFNDEF DIRECTGL}OpenGL1x.{$ELSE}dglOpenGL.{$ENDIF}glDisable(GL_CULL_FACE);
    end else
      {$IFNDEF DIRECTGL}OpenGL1x.{$ELSE}dglOpenGL.{$ENDIF}glEnable(GL_CULL_FACE);

       FMaterialObject.Apply(FonMaterialApply);
//       if assigned(FTexture) then FTexture.Apply;
       glEnableClientState(GL_VERTEX_ARRAY);
       glEnableClientState(GL_TEXTURE_COORD_ARRAY);
       glEnableClientState(GL_NORMAL_ARRAY);
       patch:=MeshList[0];
       with Patch^ do begin
         glBindBuffer(GL_ARRAY_BUFFER, nId);
         glNormalPointer(GL_FLOAT, 0, nil);
         glBindBuffer(GL_ARRAY_BUFFER, tId);
         glTexCoordPointer(3, GL_FLOAT, SizeOf(TAffineVector), nil);
         glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, iId);
         glBindBuffer(GL_ARRAY_BUFFER, vId);
         glVertexPointer(3, GL_FLOAT, 0, nil);
       end;
       FVisiblePolyCount:=0;
       for i:=0 to FOffsList.Count-1 do begin
           pExt:=FExtList[i];
           emin:=VectorTransform(PExt.emin, Matrices.WorldMatrix);
           emax:=VectorTransform(PExt.emax, Matrices.WorldMatrix);
           objPos := VectorScale(VectorAdd(emin, emax), 0.5);
           Radius := VectorDistance(emin, emax) * 0.5;
           if not isVolumeClipped(objPos, Radius, F) then begin
              RCount:=FCountList[i]; Offs:=FOffsList[i];
              glDrawRangeElements(Patch.FaceType, 0, Patch.Indices.Count-1, RCount, GL_UNSIGNED_INT, pointer(offs*4));
//              glDrawRangeElements(Patch.FaceType, offs, offs+Rcount, RCount, GL_UNSIGNED_INT, pointer(offs*4));
              //glDrawElements(Patch.FaceType, RCount, GL_UNSIGNED_INT, pointer(offs*4));
              FVisiblePolyCount:=FVisiblePolyCount+RCount div 3;
           end;
       end;
{
       FVisiblePolyCount:=0;
       for i:=0 to MeshList.Count-1 do begin
           Patch:=MeshList[i];
           emin:=VectorTransform(Patch.emin, Matrices.WorldMatrix);
           emax:=VectorTransform(Patch.emax, Matrices.WorldMatrix);
           objPos := VectorScale(VectorAdd(emin, emax), 0.5);
           Radius := VectorDistance(emin, emax) * 0.5;
           if not isVolumeClipped(objPos, Radius, F) then
           with Patch^ do begin
              glBindBuffer(GL_ARRAY_BUFFER, nId);
              glNormalPointer(GL_FLOAT, 0, nil);
              glBindBuffer(GL_ARRAY_BUFFER, tId);
              if UseTwoTexturesCoord then
                 glTexCoordPointer(2, GL_FLOAT, SizeOf(TAffineVector), nil)
              else glTexCoordPointer(3, GL_FLOAT, SizeOf(TAffineVector), nil);
              glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, iId);
              glBindBuffer(GL_ARRAY_BUFFER, vId);
              glVertexPointer(3, GL_FLOAT, 0, nil);
              if ElementsCount>MaxElements then RCount:=MaxElements else
                 RCount:=ElementsCount;
              glDrawElements(FaceType, RCount, GL_UNSIGNED_INT, nil);
              FVisiblePolyCount:=FVisiblePolyCount+RCount div 3;
           end;
       end;
}
       glDisableClientState(GL_NORMAL_ARRAY);
       glDisableClientState(GL_TEXTURE_COORD_ARRAY);
       glDisableClientState(GL_VERTEX_ARRAY);
       glBindBuffer(GL_ARRAY_BUFFER, 0);
       glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
       FMaterialObject.UnApply(FonMaterialUnApply);
//       if assigned(FTexture) then FTexture.UnApply;
    if assigned(FonAFterRender) then FonAfterRender(self);
  glPopMatrix;
end;

function TVBOTerrain.UpdateTerrainPatch(X, Y: integer;
  var Patch: T2DSingleArray): boolean;
var Buff: PVBOBuffer;
    p:pointer;
    i,j,k,offs:integer;
    W,H: integer;
    size: integer;
    s: ^single;
begin
  H:=High(Patch); W:=High(Patch[0]);
  Buff:=MeshList[0]; new(s);
  size:=sizeof(TAffineVector);
  glBindBuffer(GL_ARRAY_BUFFER, Buff.vId);
  p := glMapBuffer(GL_ARRAY_BUFFER, GL_WRITE_ONLY);
  for i:=0 to H do for j:=0 to W do begin
    if (X+j>=0) and (Y+i>=0) and (X+j<FHMWidth) and (Y+i<FHMHeight) then
    begin
      offs:=((Y+i)*FHMHeight+X+j)*size;
      s^:=Patch[i,j]; //s^:=0;
      for k:=0 to 3 do PByteArray(p)[offs+4+k]:=PByteArray(s)[k];
      FHMap[Y+i,X+j]:=Patch[i,j];
    end;
  end;
  glUnMapBuffer(GL_ARRAY_BUFFER);
  glBindBuffer(GL_ARRAY_BUFFER,0);
  dispose(s);
end;

function TVBOTerrain.GetInterpolatedHeight(X, Y: single): single;
var h1, h2, h3, h4: single;
  L1, L2, fx, fy, Hr: single;
  tx, ty: integer;
  mx, my: single;
  mh:single;
  sVector,vDir,iPoint:TVector;
  p1,p2,p3,p4:TAffineVector;
begin
  result := 0; //RayCastTriangleIntersect
    mx := (x - Position[0]) / Scale[0];
    my := (y - Position[2]) / Scale[2];
    tx := trunc(mx); ty := trunc(my);
    if (tx >= FHMWidth) or (ty >= FHMHeight)
      or (tx < 0) or (ty < 0) then exit;
    fx := frac(mx); fy := frac(my);
    h1 := FHMap[ty, tx];
    if (tx < FHMWidth - 1) then h2 := FHMap[ty, tx + 1] else h2 := h1;
    if (ty < FHMHeight - 1) then h3 := FHMap[ty + 1, tx] else h3 := h1;
    if (tx < FHMWidth - 1) and (ty < FHMHeight - 1) then
      h4 := FHMap[ty + 1, tx + 1] else h4 := h1;
    mh:=MaxFloat(h1,h2,maxfloat(h3,h4))+1;
    setvector(sVector,fx,mh,fy); setvector(vDir,0,-1,0);
    setvector(p1,0,h1,0);setvector(p2,1,h2,0);
    setvector(p3,0,h3,1);setvector(p4,1,h4,1);
    if RayCastTriangleIntersect(sVector, vDir, p1,p2,p3, @iPoint)
    then begin
       result:=iPoint[1]*Scale[1]+Position[1]; exit;
    end;
    if RayCastTriangleIntersect(sVector, vDir, p2,p4,p3, @iPoint)
    then begin
       result:=iPoint[1]*Scale[1]+Position[1]; exit;
    end;
    L1 := h1 * (1 - (fx)) + h2 * (fx); L2 := h3 * (1 - (fx)) + h4 * (fx);
    Hr := (L1 * (1 - (fy)) + L2 * (fy)) * Scale[1];
    result := Hr + Position[1];
end;

function TVBOTerrain.GetNormalInPoint(X, Y: single): TAffineVector;
var h1, h2, h3, h4: single;
  p1, p2, p3, p4, S: TAffineVector;
  tx, ty: integer;
  rx,ry:single;
  L1, L2: single;
  mh:single;
  sVector,vDir,iPoint,iNormal:TVector;
begin
  S:=AffineVectormake(Scale);
  SetVector(Result, 0, 0, 0);
    rx := (x - Position[0]) / S[0];
    ry := (y - Position[2]) / S[2];
    tx:=trunc(rx);ty:=trunc(ry);
  if (tx >= FHMWidth) or (ty >= FHMHeight)
    or (tx + 1 >= FHMWidth) or (ty + 1 >= FHMHeight)
    or (tx < 0) or (ty < 0) then exit;
  h1 := FHMap[ty, tx];
  if (tx < FHMWidth - 1) then h2 := FHMap[ty, tx + 1] else h2 := h1;
  if (ty < FHMHeight - 1) then h3 := FHMap[ty + 1, tx] else h3 := h1;
  if (tx < FHMWidth - 1) and (ty < FHMHeight - 1) then h4 := FHMap[ty + 1, tx + 1] else h4 := h1;

  setvector(p1, tx, h1, ty); ScaleVector(p1, S);
  setvector(p2, tx + 1, h2, ty); ScaleVector(p2, S);
  setvector(p3, tx, h3, ty + 1); ScaleVector(p3, S);
  setvector(p4, tx + 1, h4, ty + 1); ScaleVector(p4, S);
  mh:=MaxFloat(h1,h2,maxfloat(h3,h4))+1;
  setvector(sVector,rx,mh,ry); setvector(vDir,0,-1,0);
  if RayCastTriangleIntersect(sVector, vDir, p1,p2,p3, @iPoint,@iNormal)
  then begin result:=AffineVectorMake(iNormal);exit; end;
  if RayCastTriangleIntersect(sVector, vDir, p2,p4,p3, @iPoint,@iNormal)
  then begin result:=AffineVectorMake(iNormal);exit; end;

  L1 := VectorNorm(x - p1[0], y - p1[2]);
  L2 := VectorNorm(x - p4[0], y - p4[2]);
  if L1 < L2 then
    result := CalcPlaneNormal(p2, p1, p3)
  else result := CalcPlaneNormal(p3, p2, p4);
end;

function TVBOTerrain.GetNormalFromHField(X, Y: integer): TAffineVector;
var h1, h2, h3: single;
  p1, p2, p3, v1,v2: TAffineVector;
  tx, ty: integer;
  kx,ky: single;
begin
  SetVector(Result, 0, 1, 0);
  tx := x; ty := y;
  if (tx >= FHMWidth) or (ty >= FHMHeight)
    or (tx + 1 >= FHMWidth) or (ty + 1 >= FHMHeight)
    or (tx < 0) or (ty < 0) then exit;
  h1 := FHMap[ty, tx];
  if (tx < FHMWidth - 1) then h2 := FHMap[ty, tx + 1] else h2 := h1;
  if (ty < FHMHeight - 1) then h3 := FHMap[ty + 1, tx] else h3 := h1;

  kx:=255/FHMWidth; ky:=255/FHMHeight;
  setvector(p1, (tx*kx), (h1*255), (ty*ky));
  setvector(p2, (tx + 1)*kx, (h2*255), (ty*ky));
  setvector(p3, (tx)*kx, (h3*255), (ty + 1)*ky);
  v1:=VectorSubtract(p2,p1); NormalizeVector(v1);
  v2:=VectorSubtract(p3,p1); NormalizeVector(v2);
  result:=VectorCrossProduct(v2,v1);
  NormalizeVector(result);
  exit;

//  NormalizeVector(p1); NormalizeVector(p2); NormalizeVector(p3);
  result := CalcPlaneNormal(p1, p3, p2);
  if result[1]<0.5 then
     halt(0);
{  result[0]:=abs(result[0]);
  result[1]:=abs(result[1]);
  result[2]:=abs(result[2]);}
//  NormalizeVector(result);
end;

procedure TVBOTerrain.CreateSolidBuffer;
var i,j,a,b,offs :integer;
    v,t,n,v2,v3: TAffineVector;
    dh,dw: integer;
    MaxInd,ps: integer;
    wCount,hCount,c: integer;
    R: TRect;
    Ext: PExtents;
    Temp,Buff: PVBOBuffer;
begin
  dh:=FHMHeight div 2; dw:=FHMWidth div 2;
  new(Buff); InitVBOBuff(Buff^,GL_TRIANGLES,DrawElements);
  Buff.Vertexes.Count:=FHMHeight*(FHMWidth);
  Buff.Normals.Count:=FHMHeight*(FHMWidth);
  Buff.TexCoords.Count:=FHMHeight*(FHMWidth);
  offs:=0;
  for i:=0 to FHMHeight-1 do for j:=0 to FHMWidth-1 do begin
    v[0]:=j-dw; v[1]:=FHMap[i, j]; v[2]:=i-dh;
    t[0]:=(j)/FHMWidth; t[1]:=1-(i)/FHMHeight; t[2]:=v[1];
    Buff.Vertexes[offs]:=v;
    Buff.TexCoords[offs]:=t;
    Buff.Normals[offs]:=NullVector;
    inc(offs);
  end;
  FPatchHeight:=FTileSize;
  FPatchWidth:=FTileSize;
  if FPatchHeight>FHMHeight then FPatchHeight:=FHMHeight;
  if FPatchWidth>FHMHeight then FPatchWidth:=FHMWidth;

  wCount := FHMWidth div FPatchWidth;
  if FHMWidth mod FPatchWidth > 0 then inc(wCount);
  hCount := FHMHeight div FPatchHeight;
  if FHMHeight mod FPatchHeight > 0 then inc(hCount);
  FXTiles := wCount; FYTiles := hCount;
  for i := 0 to hCount - 1 do for j := 0 to wCount - 1 do begin
      R.Left := j * FPatchWidth;
      R.Top := i * FPatchHeight;
      R.Right := (j + 1) * FPatchWidth;
      R.Bottom := (i + 1) * FPatchHeight;
      if R.Right >= FHMWidth then R.Right := FHMWidth - 2;
      if R.Bottom >= FHMHeight then R.Bottom := FHMHeight - 2;
      FOffsList.Add(Buff.Indices.Count); c:=0;
      for a:=R.Top+1 to R.Bottom do for b:=R.Left+1 to R.Right do begin
        offs:=a*(FHMWidth)+b;
        Buff.Indices.Add(offs,offs+FHMWidth,offs+1);
        Buff.Indices.Add(offs+FHMWidth,offs+FHMWidth+1, offs+1);
        c:=c+6;
      end;
      FCountList.Add(c);
  end;
  for i:=0 to (Buff.Indices.Count div 3)-1 do begin
    v:=Buff.Vertexes[Buff.Indices[i*3]];
    v2:=Buff.Vertexes[Buff.Indices[i*3+1]];
    v3:=Buff.Vertexes[Buff.Indices[i*3+2]];
    n:=CalcPlaneNormal(v,v2,v3);
    Buff.Normals.TranslateItem(Buff.Indices[i*3],n);
    Buff.Normals.TranslateItem(Buff.Indices[i*3+1],n);
    Buff.Normals.TranslateItem(Buff.Indices[i*3+2],n);
  end;
  Buff.Normals.Normalize;

  for i:=0 to FOffsList.Count-1 do begin
      offs:=FOffsList[i]; c:=FCountList[i];
      New(ext); v:=Buff.Vertexes[Buff.Indices[offs]];
      ext.emin:=v; ext.emax:=v;
      for j:=offs to offs+c-1 do begin
         v:=Buff.Vertexes[Buff.Indices[j]];
         ext.emin[0]:=min(v[0],ext.emin[0]);
         ext.emin[1]:=min(v[1],ext.emin[1]);
         ext.emin[2]:=min(v[2],ext.emin[2]);
         ext.emax[0]:=max(v[0],ext.emax[0]);
         ext.emax[1]:=max(v[1],ext.emax[1]);
         ext.emax[2]:=max(v[2],ext.emax[2]);
      end; FExtList.Add(ext);
  end;

  Buff.UseTwoTexturesCoord:=false;
  Buff.RenderBuffs:=[uNormals,uTexCoords,uIndices];
  MeshList.Add(Buff); GenVBOBuff(buff^,false);

  FBaseExtents:=GetExtentsOfList(MeshList);
//  FNeedUpdate:=false; FNeedUpdateTC:=false; FBuilded:=true;
end;

function TVBOTerrain.GetPolyCount: integer;
begin
  result:=FVisiblePolyCount;
end;

function TVBOTerrain.GetTerrainPatch(X, Y, W, H: integer;
  var Patch: T2DSingleArray): integer;
var i,j,n: integer;
begin
  setlength(Patch, H,W); n:=0;
  for i:=0 to H-1 do for j := 0 to W-1 do begin
    if  (x+j < FHMWidth) and (y+i < FHMHeight)
    and (x+j>=0) and (y+i>=0) then begin
      Patch[i,j]:=FHMap[y+i,x+j]; inc(n);
    end;
  end;
  result:=n;
end;

function TVBOTerrain.LineOfSight(p1, p2: TAffineVector): boolean;
var x,y: integer;
    L,dt,t: single;
    v: TAffineVector;
begin
   Result:=true;
   p1[0]:=(p1[0]-Extents.emin[0])/(Extents.emax[0]-Extents.emin[0])*FHMWidth;
   p1[2]:=(p1[2]-Extents.emin[2])/(Extents.emax[2]-Extents.emin[2])*FHMHeight;
//   p1[1]:=(p1[1]-Extents.emin[1])/(Extents.emax[1]-Extents.emin[1])*255;
   p1[1]:=(p1[1]/Scale[1]);

   p2[0]:=(p2[0]-Extents.emin[0])/(Extents.emax[0]-Extents.emin[0])*FHMWidth;
   p2[2]:=(p2[2]-Extents.emin[2])/(Extents.emax[2]-Extents.emin[2])*FHMHeight;
   p2[1]:=(p2[1]/Scale[1]);
//   p2[1]:=(p2[1]-Extents.emin[1])/(Extents.emax[1]-Extents.emin[1])*255;

   L:=VectorDistance(p1,p2);
   if L=0 then exit;
   t:=0; dt:=1/L;
   repeat
     v:=VectorLerp(p1,p2,t); t:=t+dt;
     y:=trunc(v[2]); x:=trunc(v[0]);
     if (x>=0) and (x<FHMWidth) and (y>=0) and (y<FHMHeight) then begin
       if FHMap[Y,X]>v[1] then begin
         result:= false; exit;
       end;
     end;
   until t>1;
end;

{ TAnimatedMeshResource }

constructor TAnimatedMeshResource.Create(SMD: TUniformSMDRender);
begin
  inherited Create;
  assert(assigned(SMD), 'SMD Mesh is not assigned');
  FSMD:=SMD;
end;

procedure TAnimatedMeshResource.WriteVector4f(const Value: TVector;
  const stream: TStream);
begin
  stream.WriteBuffer(Value,sizeof(TVector));
end;

procedure TAnimatedMeshResource.WriteMatrix(const Value: TMatrix; const stream: TStream);
begin
  WriteVector4f(Value[0],Stream);
  WriteVector4f(Value[1],Stream);
  WriteVector4f(Value[2],Stream);
  WriteVector4f(Value[3],Stream);
end;

procedure TAnimatedMeshResource.LoadFromStream(s: TStream);
var i,j,k,n,count: integer;
    P: PVBOBuffer;
    Mat: TMaterialObject;
    anim: PSMDFile;
    np: PSMDNodePos;
    t: string;
    tex: TTexture;
begin
  inherited LoadFromStream(s);
  t:=ReadString(s); //сигнатура
  assert (t='SAM','Unknown file format.');
  Count:=ReadInt(s);
  FSMD.MeshList.Count:=Count;

  //Грузим геометрию
  for i:=0 to Count-1 do begin
    new(p); InitVBOBuff(p^,GL_TRIANGLES,DrawElements);
    FSMD.MeshList[i]:=p;
    p.Vertexes.LoadFromStream(s);
    p.Normals.LoadFromStream(s);
    p.TexCoords.LoadFromStream(s);
    p.Colors.LoadFromStream(s);
    p.Indices.LoadFromStream(s);
    p.MatName:=ReadString(s);
    p.UseTwoTexturesCoord:=false;
    GenVBOBuff(p^,false);
    mat:=fsmd.MatObjLib.MaterialByName(p.MatName);
    t:='';
//    FillChar(t,length(t),0);
    t:=ReadString(s); //Texture FileName
    if t<>'' then
    if not assigned(mat) then begin
       Tex:=TTexture.CreateFromFile(t);
       Tex.Name:=p.MatName;
       fsmd.TexLib.Add(Tex);
       mat:=TMaterialObject.Create;
       mat.AttachTexture(tex);
       mat.Name:=p.MatName;
       fsmd.MatObjLib.Add(mat);
    end;
  end;
  //Грузим скелет

  Count:=ReadInt(s); fsmd.Anim.Mesh.NodesCount:=Count;
  setlength(fsmd.Anim.Mesh.Nodes,Count);
  for i:=0 to Count-1 do begin
    fsmd.Anim.Mesh.Nodes[i].index:=ReadInt(s);
    fsmd.Anim.Mesh.Nodes[i].name:=ReadString(s);
    fsmd.Anim.Mesh.Nodes[i].parent:=ReadInt(s);
  end;

  //Грузим анимацию

  Count:=ReadInt(s);
  fsmd.Anim.Animations.Count:=Count;
  for i:=0 to Count-1 do begin
    new(anim); fsmd.Anim.Animations[i]:=anim;
    anim.Name:=ReadString(s);
    anim.NodesCount:=ReadInt(s); fsmd.BonesCount:=anim.NodesCount;
    anim.FramesCount:=ReadInt(s);
    setlength(anim.Frames,anim.FramesCount,anim.NodesCount);
    for j:=0 to anim.FramesCount-1 do begin
      n:=ReadInt(s);
      for k:=0 to anim.NodesCount-1 do begin
        np:=@anim.Frames[j][k];
        np.index:=ReadInt(s);
        np.name:=ReadString(s);
        np.parent:=ReadInt(s);
        np.x:=ReadFloat(s);
        np.y:=ReadFloat(s);
        np.z:=ReadFloat(s);
        np.rx:=ReadFloat(s);
        np.ry:=ReadFloat(s);
        np.rz:=ReadFloat(s);
        np.LocalMatrix:=ReadMatrix(s);
        np.GlobalMatrix:=ReadMatrix(s);
        np.Quaternion:=TQuaternion(ReadVector4f(s));
        np.GlobalPos:=ReadVector4f(s);
        np.LocalPos:=ReadVector4f(s);
      end;
    end;
  end;
end;

function TAnimatedMeshResource.ReadMatrix(const stream: TStream): TMatrix;
begin
  result[0]:=ReadVector4f(Stream);
  result[1]:=ReadVector4f(Stream);
  result[2]:=ReadVector4f(Stream);
  result[3]:=ReadVector4f(Stream);
end;

function TAnimatedMeshResource.ReadVector4f(const stream: TStream): TVector;
begin
  stream.ReadBuffer(result,sizeof(TVector));
end;

procedure TAnimatedMeshResource.SaveToStream(s: TStream);
var i,j,k,count: integer;
    P: PVBOBuffer;
    Mat: TMaterialObject;
    anim: PSMDFile;
    np: PSMDNodePos;
    t: string;
begin
  inherited SaveToStream(s);
  WriteString('SAM',s); //сигнатура
  Count:=FSMD.MeshList.Count;
  WriteInt(Count,s); //Mesh Count
  //Сохраняем геометрию
  for i:=0 to Count-1 do begin
    P:=FSMD.MeshList[i];
    p.Vertexes.SaveToStream(s);
    p.Normals.SaveToStream(s);
    p.TexCoords.SaveToStream(s);
    p.Colors.SaveToStream(s);
    p.Indices.SaveToStream(s);
    WriteString(p.MatName,s);
    mat:=fsmd.MatObjLib.MaterialByName(p.MatName);
    t:='';
    if assigned(mat) then //mat.SaveToStream(s);
      t:=mat.Texture.FileName;
    WriteString(t,s);
  end;
  //Сохраняем скелет
  Count:=fsmd.Anim.Mesh.NodesCount;
  WriteInt(Count,s);
  for i:=0 to Count-1 do begin
    WriteInt(fsmd.Anim.Mesh.Nodes[i].index,s);
    WriteString(fsmd.Anim.Mesh.Nodes[i].name,s);
    WriteInt(fsmd.Anim.Mesh.Nodes[i].parent,s);
  end;
  //Сохраняем анимацию
  Count:=fsmd.AnimationsCount;
  WriteInt(Count,s);
  for i:=0 to Count-1 do begin
    anim:=fsmd.Anim.Animations[i];
    WriteString(anim.Name,s);
    WriteInt(anim.NodesCount,s);
    WriteInt(anim.FramesCount,s);
    for j:=0 to anim.FramesCount-1 do begin
      WriteInt(j,s);
      for k:=0 to anim.NodesCount-1 do begin
        np:=@anim.Frames[j][k];
        WriteInt(np.index,s);
        WriteString(np.name,s);
        WriteInt(np.parent,s);
        WriteFloat(np.x,s);
        WriteFloat(np.y,s);
        WriteFloat(np.z,s);
        WriteFloat(np.rx,s);
        WriteFloat(np.ry,s);
        WriteFloat(np.rz,s);
        WriteMatrix(np.LocalMatrix,s);
        WriteMatrix(np.GlobalMatrix,s);
        WriteVector4f(TVector(np.Quaternion),s);
        WriteVector4f(np.GlobalPos,s);
        WriteVector4f(np.LocalPos,s);
      end;
    end;
  end;
end;

end.
