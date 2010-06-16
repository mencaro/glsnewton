
{: vboMesh
	Historique:
  16/06/10 - Fantom - Исправлен рейкаст для повернутых объектов
  16/06/10 - Fantom - В рендер включено следование за сценовскими объектами
  16/06/10 - Fantom - Доработан выбор объектов в прямоугольной болсти (фрастуме)
  16/06/10 - Fantom - Добавлено к TVBOMeshObject свойство NoDepthTest
        20/04/10 - Fantom - Добавлен метод TVBOMeshObject.LocalToAbsolute
        20/04/10 - Fantom - Добавлен метод TVBOMeshObject.GetTriMesh
  17/04/10 - Fantom - Исправлено вычисление FBaseExtents
  17/04/10 - Fantom - Добавлено свойство TVBOMeshObject.Direction
        07/04/10 - Fantom - Добавлен класс TSkeletalRender
        07/04/10 - Fantom - Добавлен метод AddSMDAnimation к TVBOMesh
  27/03/10 - Fantom - Добавлены экспериментальные модули uFBO,uTextures,uShaders
        22/03/10 - Fantom - Переработана система спрайтов, появилась возможность вращать
                            спрайт в плоскости экрана (функция RollObject)
        22/03/10 - Fantom - Исправлен FrustumCulling
  19/03/10 - Fantom - Внесены незначительные исправления
	12/12/09 - Fantom - Добавлен класс TVBOAnimatedSprite
  25/11/09 - Fantom - Добавлен менеджер частиц, с связи с чем в класс TVBOParticles
                      внесены соответствующие изменения
	24/11/09 - Fantom - Добавлен класс TVBOParticles для работы с частицами
	24/11/09 - Fantom - Проведена капитальная чистка Memory Leaks, иправлены мелкие ошибки
  21/11/09 - Fantom - Добавлено свойство TVBOMeshObject.NoZWrite
  20/11/09 - Fantom - Исправлена ошибка с освобождением видеопамяти при выходе
  20/11/09 - Fantom - Произведена оптимизация методов DoRender, RenderObject
  20/11/09 - Fantom - Добавлен метод TVBOMeshObject.PackMeshes
	12/11/09 - YarUnderoaker - Добавлена поддержка многопроходных шейдеров
	12/11/09 - Fantom - Исправлена автоцентровка
	12/11/09 - Fantom - Добавлен учет отрендеренных полигонов прокси-объектов
}

unit vboMesh;

interface

Uses Windows, GLRenderContextInfo, GLScene, OpenGL1x, VectorTypes, VectorGeometry,
     uVBO, Classes, SysUtils, Types, GLVectorFileObjects, GLFile3ds, GLFileObj, VectorLists,
     GLMaterial, Octree, GeometryBB, GLState, PFXManager, uTextures, uFBO, uShaders,
     uFileSMD;

Type
  TMeshTypes = (mtPlane, mtBox, mtSphere, mtFreeForm, mtHUDSprite, mtSprite, mtBBox,
                mtSphericalSprite, mtCylindricalSprite, mtScreenQuad, mtPoints,
                mtGrid, mtProxy, mtUser, mtParticles);
  TMeshTypeSet = set of TMeshTypes;
  TTransformsTypes = (ttPosition, ttScale, ttRotation, ttModel, ttParent, ttFollow, ttAll);
  TTransforms = set of TTransformsTypes;
  TSpriteType = (stSpherical, stCylindrical);
  PPointParam = ^TPointParam;
  TPointParam = record
    DistanceAttenuation: TVector3f;
    FadeTresholdSize: single;
    MinPointSize:single;
    PointSize:single;
    PointBlend: boolean;
    AlphaTest: boolean;
    BlendSFactor: cardinal;
    BlendDFactor: cardinal;
    PointSmooth: boolean;
    PointSprite: boolean;
    DepthTest: boolean;
    DepthMask: boolean;
    UseColors: boolean;
  end;

  TMatrixStack = record
    //Матрицы трансформации
    ModelMatrix: TMatrix; //модельная матрица, хранит базовые трансформации объекта
    ScaleMatrix: TMatrix; //масштабная матрица
    RotationMatrix: TMatrix; //матрица поворота
    TranslationMatrix: TMatrix; //матрица переноса
    WorldMatrix: TMatrix; //результирующая мировая матрица
    WorldMatrixT: TMatrix; //транспонированная мировая матрица
    InvWorldMatrix: TMatrix;//обратная мировая матрица
    ProjectionMatrix: TMatrix; //Проекционная матрица, заполняется при рендеринге
  end;

  TObjectRenderEvents = procedure (rci:TRenderContextInfo; MeshObject: TObject) of object;
  TVBOMeshRenderEvents = procedure (rci:TRenderContextInfo) of object;
  TVBOObjectClickEvents = procedure (X,Y:integer; NearPos,inObjectPos,dir: TAffineVector; MeshObject: TObject) of object;
  TVBOParticles = class;

  TVBOMeshObject = class
  Private
    FUseRenderList: boolean;
    FRenderList:TList;
    FMultiBuffer:TMultiPackBuff;
    FParent: TVBOMeshObject;
    FIndexInMesh: integer;
    FParams: Pointer;
    FFBO: TFrameBufferObject;
    //координатный базис
    FAbsolutePosition: TVector;
    FPosition: TVector; //глобальные координаты объекта
    FScale: TVector;    //масштаб объекта, совместно с положением - только для чтения
    FUp: TVector; // OY
    FDirection: TVector; //OZ
    FLeft: TVector; //OX

    FExtents: TExtents; //модифицированный текущей модельной матрицей
    FOctreeList: TList;
    FExtentsBuilded: boolean;
    FProxyList: TList;
    FonBeforeCheckVisibility:TObjectRenderEvents;
    FonBeforeRender:TObjectRenderEvents;
    FonAfterRender:TObjectRenderEvents;
    FonObjectClick: TVBOObjectClickEvents;
    FCulled: boolean;
//    FGetAsParticles: TVBOParticles;

    function  FGetAsParticles:TVBOParticles;
    procedure SetParent(const Value: TVBOMeshObject);
    procedure SetPosition(const Value: TVector);
    procedure SetScale(const Value: TVector);
    function  AABBUpdate(aabb:TAABB; const WorldMatrix:TMatrix):TAABB;
    function GetPolyCount: integer;
    //Ориентирует объект в заданном направлении
    procedure SetDirection(const Direction: TVector);

  Protected
    FRollAngle: single;
    FTurnAngle: single;
    FPitchAngle: single;
    FXRotationAngle: single;
    FYRotationAngle: single;
    FZRotationAngle: single;
    FTime: Double;
    FOctreeBuilded: boolean;
    FBaseExtents: TExtents; //базовый Extents
    FMeshType: TMeshTypes; //тип объекта, совместно с именем только для информации    
  Public
    Name: string; //Имя объекта
    FriendlyName: string; //храните любой текст или комментарии тут
    Tag: integer; //для нужд пользователя
    DirectingAxis: TVector; //Хранит направляющую ось Axis
    Matrices:TMatrixStack;
    MeshList: TList; //список VBO буферов
    Materials: TStringList;
    Material: TGLMaterial;
    MaterialName: string;
    TextureId: GLUInt;
    MeshMaterialLibrary: TGLMaterialLibrary;
    WorldMatrixUpdated: boolean; //false=требуется перестроить мировую матрицу
    Visible: boolean;
    //матрица трансформации будет экспортировать из указанного объекта сцены
    ToFollowObject: TGLBaseSceneObject;
    Pickable: boolean; //будет ли объект выбираться через PickObject
    NoZWrite: boolean; //отключение режима записи в буфер глубины
    NoDepthTest: boolean; //отключение режима записи в буфер глубины

    property AsVBOParticles: TVBOParticles read FGetAsParticles;
    //-------Events--------
    //событие вызывается при рендеринге контейнера, непосредственно перед проверкой видимости
    property onBeforeCheckVisibility:TObjectRenderEvents read FonBeforeCheckVisibility write FonBeforeCheckVisibility;
    //Событие вызывается после бинда текстуры, непосредственно перед рендерингом
    property onBeforeRender:TObjectRenderEvents read FonBeforeRender write FonBeforeRender;
    //Событие вызывается непосредственно после рендеринга, перед анбиндом текстуры
    property onAfterRender:TObjectRenderEvents read FonAfterRender write FonAfterRender;
    //Вызывается после вызова функции PickObject
    property onObjectClick: TVBOObjectClickEvents read FonObjectClick write FonObjectClick;

    Constructor Create;
    Destructor Destroy;override;

    Procedure RenderObject(ARCI: TRenderContextInfo;var ViewMatrix: TMatrix);virtual;
    //Доступ к буферу кадра
    Property FBO: TFrameBufferObject read FFBO;
    //Возвращает тип объекта
    Property MeshType:TMeshTypes read FMeshType;
    //возвращает координаты окаймляющего бокса
    Property Extents:TExtents read FExtents;
    //установка родителя, из которого будет браться базовая матрица трансформаций
    Property Parent:TVBOMeshObject read FParent write SetParent;
    Property PolygonsCount: integer read GetPolyCount;
    //Установка/чтение глобальных координат
    Property Position: TVector read FPosition write SetPosition;
    //Установка/чтение масштаба объекта
    Property Scale: TVector read FScale write SetScale;
    //Установка/чтение ориентации объекта
    Property Direction: TVector read Matrices.WorldMatrix[2] write SetDirection;
    //Читает мультибуфер
    Property MultiBuffer: TMultiPackBuff read FMultiBuffer;

    //Вращение относительно локальных осей
    Procedure TurnObject(Angle:single);  //Вокруг локальной оси Y
    Procedure RollObject(Angle:single);  //Вокруг локальной оси Z
    Procedure PitchObject(Angle:single); //Вокруг локальной оси X
    //Передвигает объект вдоль оси Direction
    Procedure MoveForward(Step:single);
    //Передвигает объект вдоль оси Left
    Procedure MoveLeft(Step:single);
    //Передвигает объект вдоль оси Up
    Procedure MoveUp(Step:single);
    //формирует матрицу поворота, при AbsoluteRotation=false модифицируется существующая
    Procedure RotateObject(Axis:TVector;Angle:single;AbsoluteRotation:boolean=true);
    Procedure RotateAroundX(Angle:single;AbsoluteRotation:boolean=true);
    Procedure RotateAroundY(Angle:single;AbsoluteRotation:boolean=true);
    Procedure RotateAroundZ(Angle:single;AbsoluteRotation:boolean=true);
    //Накопленные углы при абсолютном повороте
    property XRotationAngle: single read FXRotationAngle;
    property YRotationAngle: single read FYRotationAngle;
    property ZRotationAngle: single read FZRotationAngle;

    //формирует матрицу масштабирования, при AbsoluteScale=false модифицируется существующая
    Procedure ScaleObject(Scale:TVector;AbsoluteScale:boolean=true);overload;
    Procedure ScaleObject(ScaleX,ScaleY,ScaleZ: single;AbsoluteScale:boolean=true);overload;
    //формирует матрицу переноса, при AbsolutePos=false модифицируется существующая
    Procedure MoveObject(Position:TVector;AbsolutePos:boolean=true);overload;
    Procedure MoveObject(x,y,z: single;AbsolutePos:boolean=true);overload;
    //перестраивается мировая матрица
    Procedure UpdateWorldMatrix(UseMatrix:TTransforms=[ttAll]);
    //Заменяет все матрицы трансформаций на единичные
    Procedure ResetMatrices;
    //Заменяет модельную матрицу текущей мировой матрицей
    Procedure StoreTransforms(ToStore: TTransforms);
    //трансформирует все вершины используя матрицу WMatrix,
    //возвращает false в случае если буферы были очищены
    Function AbsoluteTransform:boolean;
    //Производит абсолютную трансформацию вершин относительно геометрического центра
    Procedure AbsoluteToExtents;
    //Переводит точку из глобальной системы координат в систему координат объекта
    Function AbsoluteToLocal(P: TVector):TVector;
    //Переводит вектор из глобальной системы координат в локальную
    Function VectorToLocal(V: TAffineVector; Norm: boolean=true):TAffineVector;
    //Переводит точку из локальной системы координат в глобальную
    Function LocalToAbsolute(P: TVector): TVector;
    //Очищает занимаемую объектом оперативную память
    Procedure FreeObjectsMemory;
    //Строит Octree по каждому из мешей в MeshList
    Procedure BuildOctreeList(Level:integer=3);
    //Упаковывает все меши в один буфер
    Procedure PackMeshes(FreeOldBuffers:boolean=true; BuffSize:integer=-1);
    //Упаковывает меш в текстуру, x,y,z-координаты вершины, w-текстурная координата p
    Procedure PackMeshesToTexture(var vtex,ntex: TGLTexture);
    //Упаковывает tri-list в текстуру, 3 последовательных пикселя задают вершины
    //треугольника, координата w=0 - вершина не используется
    Procedure PackTriListToTexture(var vtex: TGLTexture);
    //Извлекает все треугольники из меша и помещает в один буфер
    Procedure GetTriMesh(var TriMesh: TAffinevectorList);
    
    Function OctreeRayCastIntersect(const rayStart, rayVector: TVector; var iList:TList;
                         iPoint: PVector = nil;iNormal: PVector = nil): integer; overload;
    Function OctreeRayCastIntersect(const rayStart, rayVector: TVector;
                         iPoint: PVector = nil; iNormal: PVector = nil): boolean; overload;
    Function ExtentsIntersect(const rayStart,
                      rayVector: TVector; iPoint:PVector=nil): boolean;

  end;

  TVBOParticles = class (TVBOMeshObject)
    private
      FPointParam: TPointParam;
      FMaxCapacity: integer;
      FCount: integer;
      FRealCount: integer;
      FPositions: TAffineVectorList;
      FColors: TVectorList;
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
    procedure SetExtents(const Value: TExtents);

    public
      Constructor Create(MaxCapacity:integer=-1);
      Destructor Destroy;override;
       Procedure RenderObject(ARCI: TRenderContextInfo;var ViewMatrix:
        TMatrix);override;

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
      property DistanceAttenuation: TVector3f read FPointParam.DistanceAttenuation
                                             write FPointParam.DistanceAttenuation;
      property FadeTresholdSize: single read FPointParam.FadeTresholdSize
                                       write FPointParam.FadeTresholdSize;
      property MinPointSize:single read FPointParam.MinPointSize;
      property PointSize:single read FPointParam.PointSize
                               write FPointParam.PointSize;
      property PointBlend: boolean read FPointParam.PointBlend
                                  write FPointParam.PointBlend;
      property AlphaTest: boolean read FPointParam.AlphaTest
                                 write FPointParam.AlphaTest;
      property BlendSFactor: cardinal read FPointParam.BlendSFactor
                                     write FPointParam.BlendSFactor;
      property BlendDFactor: cardinal read FPointParam.BlendDFactor
                                     write FPointParam.BlendDFactor;
      property PointSmooth: boolean read FPointParam.PointSmooth
                                   write FPointParam.PointSmooth;
      property PointSprite: boolean read FPointParam.PointSprite
                                   write FPointParam.PointSprite;
      property DepthTest: boolean read FPointParam.DepthTest
                                 write FPointParam.DepthTest;
      property DepthMask: boolean read FPointParam.DepthMask
                                 write FPointParam.DepthMask;
      //говорит будет ли использоваться буфер цвета для спрайта
      property UseColors: boolean read FPointParam.UseColors
                                 write FPointParam.UseColors;
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
      Procedure RenderObject(ARCI: TRenderContextInfo; var ViewMatrix: TMatrix);override;

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
      FAnim: TAnimations;
      vtex,ntex,rvtex,rntex: TGLTexture;
      FScreenQuad: TVBOMeshObject;
      spId: GLUInt;
      Shaders: TShaders;
      FFramesCount: integer;
      FramePosition: single;
      FAnimationName: string;
      FAnimationNum: integer;
      FOldFrame: single;
      
      procedure FApplyShader(rci:TRenderContextInfo;  mo:TObject);
      procedure FUnApplyShader(rci:TRenderContextInfo; mo:TObject);
      procedure FSetAnimationByNum(Value: integer);
      procedure FSetAnimationByName(Value: string);
      procedure FSetFrame(Value: single);
      procedure FCreatSMDShader;
    public
      Procedure RenderObject(ARCI: TRenderContextInfo;var ViewMatrix: TMatrix);override;

      Property FramePos: single read FramePosition write FSetFrame;
      Property FramesCount: integer read FFramesCount;
      Property AnimationNum: integer read FAnimationNum write FSetAnimationByNum;
      Property AnimationName: string read FAnimationName write FSetAnimationByName;
  end;

  TVBOMesh = class(TGLBaseSceneObject)
    private
      FMeshList: TList;
      FExtentsBuilded: Boolean;
      FViewMatrix: TMatrix;
      FProjectionMatrix: TMatrix;
      FPolyCount: integer;
      FonBeforeRender: TVBOMeshRenderEvents;
      FonAfterRender: TVBOMeshRenderEvents;

      function GetMesh(Index: Integer): TVBOMeshObject;
      procedure ConvertAABBToCorners(const AABB:TAABB; var Corners:TAABBCorners);
      function GetCount: integer;
    public
      OctreeBuilded: Boolean;
      MaterialLibrary: TGLMaterialLibrary;
      Visible: boolean;
      property onBeforeRender: TVBOMeshRenderEvents read FonBeforeRender write FonBeforeRender;
      property onAfterRender: TVBOMeshRenderEvents read FonAfterRender write FonAfterRender;

      destructor Destroy;override;
      constructor CreateAsChild(aParentOwner: TGLBaseSceneObject);

      property Count:integer read GetCount;
      property ObjectsList[Index: Integer]: TVBOMeshObject read GetMesh;default;
      property PolygonsCount: integer read FPolyCount;

      Procedure DoRender(var ARci: TRenderContextInfo;
                            ARenderSelf, ARenderChildren: Boolean); override;
      Procedure RenderMeshObject(MeshObject: TVBOMeshObject; var ARci: TRenderContextInfo);
      Procedure DeleteMeshObject(MeshObject: TVBOMeshObject);overload;
      Procedure DeleteMeshObject(index: integer);overload;

      Function AddPoints(var Points: TAffineVectorArray;
                         var Param: TPointParam; Colors: PVectorArray=nil):
                         TVBOMeshObject;overload;
      Function AddPoints(PointsList:TAffineVectorList; ColorsList: TVectorList;
        var Param: TPointParam): TVBOMeshObject;overload;
      Function AddParticles(MaxCapacity:integer=-1):TVBOMeshObject;
      Function AddPlane(Width,Height:single; TilesX,TilesY:integer; HeightFunc: TGetHeightFunc=nil):TVBOMeshObject;
      Function AddGrid(Width,Height:single; TilesX,TilesY:integer; Color:PVector=nil):TVBOMeshObject;
      Function AddBox(Width,Height,Depth:single; TilesX,TilesY,TilesZ:integer):TVBOMeshObject;
      Function AddSphere(Radius: single; VSegments, HSegments: integer;
                         TileS:single=1;TileT:single=1;NormalInside:boolean = false): TVBOMeshObject;
      Function AddHUDSprite(width, height:single):TVBOMeshObject;
      Function AddScreenQuad:TVBOMeshObject;
      Function AddSprite(s_type:TSpriteType;width, height:single):TVBOMeshObject;
      Function AddAnimatedSprite(s_type:TSpriteType;width, height:single):TVBOMeshObject;
      Function AddMeshFromFreeForm(FF: TGLFreeForm):TVBOMeshObject;
      Function AddMeshFromFile(FileName: string; UseMeshMaterials:Boolean=true):TVBOMeshObject;
      Function AddBBox(Corners:THmgBoundingBox; Color:TVector):TVBOMeshObject;overload;
      Function AddBBox(Extents:TExtents; Color:TVector):TVBOMeshObject;overload;
      Function AddProxyObject(MasterObject:TVBOMeshObject):TVBOMeshObject;
      Function AddUserObject(Name:string; VBOMeshList:TList):TVBOMeshObject;overload;
      Function AddUserObject(Name:string; VBOBuffer:PVBOBuffer):TVBOMeshObject;overload;
      Function AddMeshObject(mo:TVBOMeshObject): integer;
      Function AddSMDAnimation(MeshFile: string; const AnimFiles: array of string):TVBOMeshObject;
      
      Function  GetObjectByName(Name:string):TVBOMeshObject;
      Procedure GetObjectListByType(ObjType: TMeshTypes; var List:TList);
      Procedure GetObjectListByTypeSet(ObjTypes: TMeshTypeSet; var List:TList);
      Procedure GetObjectListWithNameIncluded(Name:String; var List:TList);

      Procedure BuildOctree(level:integer=3);
      Procedure BuildExtents;

      Function OctreeRayCastIntersect(const rayStart, rayVector: TVector; var List:Tlist ): boolean;
      Function ExtentsIntersect(const rayStart, rayVector: TVector; var List:Tlist): boolean;

      //Переводит экранные координаты в пару Позиция в плоскости экрана - Направляющий вектор.
      //Результат функции - координаты точки из буфера глубюины
      Function ScreenToWorld(X, Y: single; Pos: PVector=nil; dir:PVector=nil): TVector;

      //Набор функций возвращающих объект по экранным координатам
      Function PickObject(X,Y:single; UseOctree:boolean=false):TVBOMeshObject;overload;
      Function PickObject(X, Y: single; var List: TList; UseOctree:boolean=false): boolean;overload;
      Function PickObject(Rect:TRect):TVBOMeshObject;overload;
      Function PickObject(Rect: TRect; var List:TList): Boolean;overload;
      Function PickObjectInFrustum(Rect: TRect; var List:TList): Boolean;

      //Сортирует список объектов по расстоянию до камеры, IgnoreBack - исключает из списка все меши сзади камеры
      Procedure SortByDistance(var SortedMeshList: TList; IgnoreBack:boolean=true);
  end;

  TRaycastIntersectInfo = record
     iPoint,iNormal: TVector;
     inScreen, inObject, Dir: TAffineVector;
     PickedObject: TVBOMeshObject;
     ObjectIndex: integer;
     ObjMeshIndex: integer;
     ObjMeshIndexList: TIntegerList;
  end;
  PRaycastIntersectInfo=^TRaycastIntersectInfo;

  function IntToStr(x: integer): string;
  procedure FreeList (var List:TList);
  procedure FreeAndNil(var Obj);
  function UpperCase(const S: string): string;
  function GetTime:double;
  function CreateBBMatrix (var View: TMatrix; const mat: TMatrixStack; angle: single;stype: TSpriteType):TMatrix;

implementation

function IntToStr(x: integer): string;
begin Str(x, Result); end;

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
       p:=list[i];dispose(p);end;
   FreeAndNil(List);
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

function CreateBBMatrix (var View: TMatrix; const mat: TMatrixStack; angle: single;stype: TSpriteType):TMatrix;
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

function TVBOMeshObject.AbsoluteToLocal(P: TVector): TVector;
begin
    if not WorldMatrixUpdated then UpdateWorldMatrix;p[3]:=1;
    Result:=VectorTransform(P,Matrices.InvWorldMatrix);
end;

function TVBOMeshObject.VectorToLocal(V: TAffineVector; Norm: boolean=true): TAffineVector;
begin
    if not WorldMatrixUpdated then UpdateWorldMatrix;
    Result:=affinevectormake(VectorTransform(vectormake(V,0),Matrices.InvWorldMatrix));
    if Norm then NormalizeVector(Result);
end;

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
    ResetMatrices; UpdateWorldMatrix; Visible:=true;
end;

procedure TVBOMeshObject.BuildOctreeList(Level: integer=3);
var Temp:TOctree;
    P:PVBOBuffer;
    i:integer;
    TriList:TAffineVectorList;
begin
  for i:=0 to MeshList.Count-1 do begin
   TriList:=TAffineVectorList.Create;
   P:=MeshList[i]; ExtractTriangles(P^,TriList);
   Temp:=TOctree.Create;
   with Temp do begin
    DisposeTree;
    InitializeTree(P.emin, P.emax, TriList, Level);
   end;
   FOctreeList.Add(Temp); TriList.Clear; TriList.Free;
  end;
  FOctreeBuilded:=true;
end;

constructor TVBOMeshObject.Create;
begin
  inherited;
  with Matrices do begin
    ModelMatrix:=IdentityHmgMatrix;
    ScaleMatrix:=IdentityHmgMatrix;
    RotationMatrix:=IdentityHmgMatrix;
    TranslationMatrix:=IdentityHmgMatrix;
    WorldMatrix:=IdentityHmgMatrix;
    WorldMatrixT:=IdentityHmgMatrix;
    InvWorldMatrix:=IdentityHmgMatrix;
  end;

  FRollAngle:=0;
  FTurnAngle:=0;
  FPitchAngle:=0;
  FXRotationAngle:=0;
  FYRotationAngle:=0;
  FZRotationAngle:=0;

  MeshList:=TList.Create;
  Materials:=TStringList.Create;
  FOctreeList:=TList.Create;
  FOctreeBuilded:=false;
  FProxyList:=TList.Create;
  FPosition:=vectormake(0,0,0,0);
  FScale:=vectormake(1,1,1,1);
  UpdateWorldMatrix;
  FExtentsBuilded:=false;
  Pickable:=true;
  FParams:=nil;
  Parent:=nil;
  FUseRenderList:=false;
  FRenderList:=TList.Create;
  NoZWrite:=false;
  NoDepthTest:=false;
  FFBO:=TFrameBufferObject.Create;
end;

destructor TVBOMeshObject.Destroy;
var i:integer;
    temp:TOctree;
    pd:PMultiRenderDescr;
begin
  Visible:=false;
  if FMeshType<>mtProxy then begin
     FreeVBOList(MeshList,true);
     MeshList.Clear; MeshList.Free;
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
  for i:=0 to FOctreeList.Count-1 do begin
    temp:=FOctreeList[i];
    temp.DisposeTree;
    temp.triangleFiler.Clear;
    Temp.Free;
  end;FOctreeList.Clear; FOctreeList.Free;
  FProxyList.Clear;FProxyList.Free;
  FRenderList.Free;
  if FMeshType = mtPoints then Dispose(FParams);
  Materials.Free;
  FFBO.Free;
  inherited;
end;

procedure TVBOMeshObject.FreeObjectsMemory;
var i:integer;
    P:PVBOBuffer;
begin
   for i:=0 to MeshList.Count-1 do begin
       P:=MeshList[i]; FreeVBOMem(P^);end;
end;

procedure TVBOMeshObject.MoveObject(Position:TVector;AbsolutePos:boolean=true);
var mt:TMatrix;
begin
  mt:=CreateTranslationMatrix(Position);
  with Matrices do begin
   if AbsolutePos then begin
     TranslationMatrix:=mt;
     FPosition:=Position;
   end else begin
     FPosition:=vectoradd(FPosition,VectorTransform(Position,TranslationMatrix));
     TranslationMatrix:=MatrixMultiply(TranslationMatrix,mt);
   end;
  end;
  UpdateWorldMatrix;
end;

procedure TVBOMeshObject.ResetMatrices;
begin
  with Matrices do begin
    ModelMatrix:=IdentityHmgMatrix;
    ScaleMatrix:=IdentityHmgMatrix;
    RotationMatrix:=IdentityHmgMatrix;
    TranslationMatrix:=IdentityHmgMatrix;
    WorldMatrix:=IdentityHmgMatrix;
  end;
end;

procedure TVBOMeshObject.RotateObject(Axis: TVector; Angle: single;
  AbsoluteRotation: boolean);
var mr:TMatrix;
begin
 with Matrices do begin
  mr:=CreateRotationMatrix(Axis,Angle);
  if AbsoluteRotation then RotationMatrix:=mr
  else RotationMatrix:=MatrixMultiply(RotationMatrix,mr);
 end;
 UpdateWorldMatrix;
end;

procedure TVBOMeshObject.ScaleObject(Scale:TVector;AbsoluteScale:boolean=true);
var ms:TMatrix;
begin
 with Matrices do begin
  ms:=CreateScaleMatrix(Scale);
  if AbsoluteScale then begin
     ScaleMatrix:=ms;
     FScale:=Scale;
  end else begin
     FScale:=VectorTransform(Scale,ScaleMatrix);
     ScaleMatrix:=MatrixMultiply(ScaleMatrix,ms);
  end;
 end;
 UpdateWorldMatrix;
end;

Function TVBOMeshObject.AABBUpdate(aabb:TAABB; const WorldMatrix:TMatrix):TAABB;
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
var wm:TMatrix;
    bb:TAABB;
begin
 with Matrices do begin
  if (FParent<>nil) and ((ttParent in UseMatrix) or (ttAll in UseMatrix)) then begin
     if not FParent.WorldMatrixUpdated then Fparent.UpdateWorldMatrix;
     wm:=Fparent.Matrices.WorldMatrix;
     wm:=MatrixMultiply(wm, ModelMatrix);
  end else wm := ModelMatrix;
  if (assigned(ToFollowObject)) and ((ttFollow in UseMatrix) or (ttAll in UseMatrix))
  then wm := ToFollowObject.AbsoluteMatrix;

  if (not (ttModel in UseMatrix)) and (not(ttAll in UseMatrix))
  then wm:=IdentityHmgMatrix;
 if (FMeshType <> mtSphericalSprite) and (FMeshType <> mtCylindricalSprite) then begin
  if (ttScale in UseMatrix) or (ttAll in UseMatrix) then wm := MatrixMultiply(wm, ScaleMatrix);
  if (ttRotation in UseMatrix) or (ttAll in UseMatrix) then wm := MatrixMultiply(wm, RotationMatrix);
  if (ttPosition in UseMatrix) or (ttAll in UseMatrix) then wm := MatrixMultiply(wm, TranslationMatrix);
  WorldMatrix:=wm;
  FLeft:=WorldMatrix[0];NormalizeVector(FLeft);
  FUp:=WorldMatrix[1];  NormalizeVector(FUp);
  FDirection:=WorldMatrix[2]; NormalizeVector(FDirection);
 end else begin
  if (ttPosition in UseMatrix) or (ttAll in UseMatrix) then wm := MatrixMultiply(wm, TranslationMatrix);
  WorldMatrix:=wm;
  FLeft:=Vectormake(1,0,0,0);
  FUp:=Vectormake(0,1,0,0);
  FDirection:=Vectormake(0,0,1,0);
 end;
  FAbsolutePosition:=WorldMatrix[3];
  TransposeMatrix(wm);WorldMatrixT:=wm;
  InvWorldMatrix:=matrixInvert(WorldMatrix);
  DirectingAxis:=vectormake(WorldMatrix[0,0],WorldMatrix[1,1],WorldMatrix[2,2]);
  NormalizeVector(DirectingAxis);
  bb:=TAABB(FBaseExtents);
  bb:=AABBUpdate(bb,WorldMatrix);
//  AABBTransform(bb,WorldMatrixT);
  FExtents:=TExtents(bb);
  WorldMatrixUpdated:=true;
 end;
end;

procedure TVBOMeshObject.PitchObject(Angle: single);
begin
  //вокруг оси X в YZ
  if not WorldMatrixUpdated then UpdateWorldMatrix;
  with Matrices do RotationMatrix:=Pitch(RotationMatrix,Angle);
  UpdateWorldMatrix;
  FPitchAngle:=FPitchAngle+Angle;
end;

procedure TVBOMeshObject.RollObject(Angle: single);
begin
  //вокруг оси Z в XY
  if not WorldMatrixUpdated then UpdateWorldMatrix;
  with Matrices do RotationMatrix:=Roll(RotationMatrix,Angle);
  UpdateWorldMatrix;
  FRollAngle:=FRollAngle+Angle;
end;

procedure TVBOMeshObject.TurnObject(Angle: single);
begin
  //вокруг оси Y в XZ
  if not WorldMatrixUpdated then UpdateWorldMatrix;
  with Matrices do RotationMatrix:=Turn(RotationMatrix,Angle);
  UpdateWorldMatrix;
  FTurnAngle:=FTurnAngle+Angle;
end;


function TVBOMeshObject.OctreeRayCastIntersect(const rayStart, rayVector: TVector;
  var iList:TList; iPoint: PVector = nil; iNormal: PVector = nil): integer;
var i:integer;
  locRayStart, locRayVector: TVector;
  mOctree:TOctree;
  ip,n:TVector;
  f:boolean;
  icount:integer;
  ri:PRaycastIntersectInfo;
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
//    locRayStart, locRayVector: TVector;
    ip:TVector;
begin
    bb:=TAABB(FExtents);
//    locRayStart := AbsoluteToLocal(RayStart);
//    locRayVector := vectormake(VectorToLocal(affinevectormake(RayVector)));
    result:=RayCastAABBIntersect(RayStart,RayVector,bb,@ip);
//    ip:=VectorTransform(ip,Matrices.WorldMatrix);
    iPoint^:=ip;
end;

procedure TVBOMeshObject.SetParent(const Value: TVBOMeshObject);
begin
  FParent := Value; UpdateWorldMatrix;
end;

procedure TVBOMeshObject.StoreTransforms(ToStore: TTransforms);
var wm,mm:TMatrix;
    ms:TMatrixStack;
begin
  ms:=Matrices;
  with Matrices do begin
    if ttModel in toStore then wm := ModelMatrix else wm:=IdentityHmgMatrix;
    if ttScale in toStore then wm := MatrixMultiply(wm, ScaleMatrix);
    if ttRotation in toStore then wm := MatrixMultiply(wm, RotationMatrix);
    if ttposition in toStore then wm := MatrixMultiply(wm, TranslationMatrix);
    mm:=ModelMatrix; ResetMatrices;
    Matrices.ModelMatrix:=MatrixMultiply(mm, wm);
    if not (ttScale in toStore) then ScaleMatrix:=ms.ScaleMatrix;
    if not (ttRotation in toStore) then RotationMatrix:=ms.RotationMatrix;
    if not (ttPosition in toStore) then TranslationMatrix:=ms.TranslationMatrix;    
    UpdateWorldMatrix;
  end;
end;

procedure TVBOMeshObject.RotateAroundX(Angle: single;
  AbsoluteRotation: boolean);
var rm:TMatrix;
begin
 with Matrices do begin
  //вокруг глобальной оси X
  if not WorldMatrixUpdated then UpdateWorldMatrix;
  if AbsoluteRotation then begin
     RotationMatrix:=CreateRotationMatrixX(Angle);
  end else begin
     FXRotationAngle:=FXRotationAngle+Angle;
     rm:=CreateRotationMatrixX(Angle);
     RotationMatrix:=MatrixMultiply(RotationMatrix,rm);
  end;
  UpdateWorldMatrix;
 end;
end;

procedure TVBOMeshObject.RotateAroundY(Angle: single;
  AbsoluteRotation: boolean);
var rm:TMatrix;
begin
 with Matrices do begin
  //вокруг глобальной оси Y
  if not WorldMatrixUpdated then UpdateWorldMatrix;
  if AbsoluteRotation then begin
     RotationMatrix:=CreateRotationMatrixY(Angle);
  end else begin
     FYRotationAngle:=FYRotationAngle+Angle;
     rm:=CreateRotationMatrixY(Angle);
     RotationMatrix:=MatrixMultiply(RotationMatrix,rm);
  end;
  UpdateWorldMatrix;
 end;
end;

procedure TVBOMeshObject.RotateAroundZ(Angle: single;
  AbsoluteRotation: boolean);
var rm:TMatrix;
begin
 with Matrices do begin
  //вокруг глобальной оси Z
  if not WorldMatrixUpdated then UpdateWorldMatrix;
  rm:=CreateRotationMatrixZ(Angle);
  if AbsoluteRotation then begin
     RotationMatrix:=CreateRotationMatrixZ(Angle);
  end else begin
     FZRotationAngle:=FZRotationAngle+Angle;
     rm:=CreateRotationMatrixZ(Angle);
     RotationMatrix:=MatrixMultiply(RotationMatrix,rm);
  end;
  UpdateWorldMatrix;
 end;
end;

procedure TVBOMeshObject.RenderObject(ARCI: TRenderContextInfo;
          var ViewMatrix:TMatrix);
var m: TMatrix;
    mv: TMatrix;
    i,j: integer;
    P: PVBOBuffer;
    PDescr: PMultiRenderDescr;
    Lm: TGLLibMaterial;
    ActiveMaterial: string;
    TempMatrix: TMatrixStack;
    MasterObj: TVBOMeshObject;
    MultiPass: Boolean;
    bindedBuff: pointer;
    bindState: TBindState;
    Rcount: integer;
    singleMat: boolean;
    CMName: string;
    Frustum: TFrustum;
    MVProj: TMatrix;
begin
    arci.GLStates.ResetAll;
    arci.ignoreDepthRequests:=true;
    if Assigned(ToFollowObject) then
       if not MatrixEquals(ToFollowObject.AbsoluteMatrix, Matrices.ModelMatrix)
       then UpdateWorldMatrix;
    if (not WorldMatrixUpdated) then UpdateWorldMatrix;
    mv:=MatrixMultiply(Matrices.WorldMatrix,ViewMatrix);
    m:=mv; glPushMatrix;
    if FFBO.Active then FFBO.Apply;

    case FMeshType of
      mtScreenQuad: begin
       glMatrixMode(GL_MODELVIEW);
       glLoadIdentity;
       glMatrixMode(GL_PROJECTION);
       glPushMatrix;glLoadIdentity;
       glDisable(GL_DEPTH_TEST);
       glDisable(GL_LIGHTING);
      end;
      mtHudSprite: begin
          glMatrixMode(GL_MODELVIEW); glPushMatrix;
          m:=Matrices.WorldMatrix; m[3][2]:=0;
          glLoadMatrixf(PGLFloat(@m));
          glMatrixMode(GL_PROJECTION); glPushMatrix;
          glLoadIdentity;
          glDepthMask(false); glDisable(GL_LIGHTING);
          glDisable(GL_DEPTH_TEST);
      end;
      mtSphericalSprite: begin
        m:=CreateBBMatrix(ViewMatrix,Matrices,FRollAngle,stSpherical);
        glLoadMatrixf(PGLFloat(@m));
      end;
      mtCylindricalSprite: begin
        m:=CreateBBMatrix(ViewMatrix,Matrices,FRollAngle,stCylindrical);
        glLoadMatrixf(PGLFloat(@m));
      end;
      mtGrid: begin
         glDisable(GL_TEXTURE_2D);
         glLoadMatrixf(PGLFloat(@m));
      end;
      mtPoints,mtParticles: begin
        if assigned(Material) then Material.Apply(arci)
        else
        if MaterialName<>'' then begin
           Lm:=MeshMaterialLibrary.LibMaterialByName(MaterialName);
           if assigned(Lm) then Lm.Apply(arci);
        end;
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
           if PointBlend then begin
              glEnable(GL_BLEND);
              glBlendFunc(BlendSFactor, BlendDFactor);
           end else glDisable(GL_BLEND);
           if AlphaTest then glEnable(GL_ALPHA_TEST) else glDisable(GL_ALPHA_TEST);
           if DepthTest then glEnable(GL_DEPTH_TEST) else glDisable(GL_DEPTH_TEST);
           glDepthMask(DepthMask);
        end;glLoadMatrixf(PGLFloat(@m));
      end;
      mtProxy: begin
            MasterObj:=FParams;
            TempMatrix:=MasterObj.Matrices;
            MasterObj.Matrices:=Matrices;
            MasterObj.RenderObject(arci,ViewMatrix);
            MasterObj.Matrices:=TempMatrix;
            glPopMatrix;
            exit;
        end;
      Else glLoadMatrixf(PGLFloat(@m));
    end;

    if NoZWrite then glDepthMask(False) else glDepthMask(True);
    if NoDepthTest then glDisable(GL_DEPTH_TEST) else glEnable(GL_DEPTH_TEST);

    if assigned(onBeforeCheckVisibility) then onBeforeCheckVisibility(arci,self);
    MVProj := MatrixMultiply(ViewMatrix, Matrices.ProjectionMatrix);
    Frustum := ExtractFrustumFromModelViewProjection(MVProj);
    if not ((FMeshType=mtHUDSprite) or (FMeshType=mtSphericalSprite) or
    (FMeshType=mtCylindricalSprite)) then
       FCulled:=IsVolumeClipped(Extents, Frustum) else FCulled:=False;
    if not FCulled then begin
      if FUseRenderList then rcount:=FRenderList.Count else rcount:=MeshList.Count;
      if rcount>0 then begin
      singleMat:=false; Lm:=nil;
      if FMeshType<>mtParticles then begin
        if assigned(Material) then begin Material.Apply(arci); singleMat:=true;end
        else begin
          if MaterialName<>'' then begin
            Lm:=MeshMaterialLibrary.LibMaterialByName(MaterialName);
            if assigned(Lm) then begin Lm.Apply(arci);singleMat:=true;end;
          end else if TextureId<>0 then begin
                 glActiveTexture(GL_TEXTURE0);
                 glEnable(GL_TEXTURE_2D);
                 glBindTexture(GL_TEXTURE_2D, TextureId);
                 singleMat:=true;
          end;
        end;
      end else singleMat:=true;

      if singleMat then begin
        repeat
          if assigned(FonBeforeRender) then FonBeforeRender(arci,self);
          if FUseRenderList then begin
            bindedBuff:=nil;
            for i:=0 to FRenderList.Count-1 do begin
                PDescr:=FRenderList[i];
                if bindedBuff<>PDescr.RenderBuff then begin
                   BindState:=[sActivate];bindedBuff:=PDescr.RenderBuff;end;
                if i=FRenderList.Count-1 then begin BindState:=BindState+[sDeactivate];bindedBuff:=nil;end;
                RenderVBOMultiPart(PDescr,BindState,false);
            end;
          end else begin
            RenderVBOList(MeshList);
          end;
          if assigned(FonAfterRender) then FonAfterRender(arci,self);
          if assigned(Material) then MultiPass:=Material.UnApply(arci)
            else if assigned(Lm) then MultiPass:=Lm.UnApply(arci)
              else begin
                 MultiPass:=false;
                 glDisable(GL_TEXTURE_2D);
              end;
        until not MultiPass;
      end else begin
      //MultiMaterial

        if FUseRenderList then begin
           PDescr:=FRenderList[0]; CMName:=PDescr.MaterialName;
        end else begin
           P := MeshList[0]; CMName:=P.MatName;
        end;
        ActiveMaterial:=''; bindedBuff:=nil; i:=0; j:=0;
        Lm := MeshMaterialLibrary.LibMaterialByName(CMName);
        if assigned(Lm) then begin
           Lm.Apply(arci); ActiveMaterial:=CMName; end
        else begin ActiveMaterial:=''; Lm:=nil; end;
        repeat
           //Render+++++++++++++++++
           if assigned(FonBeforeRender) then FonBeforeRender(arci,self);
           if FUseRenderList then begin
             PDescr:=FRenderList[i];
             if bindedBuff<>PDescr.RenderBuff then begin
                BindState:=[sActivate];bindedBuff:=PDescr.RenderBuff;end;
             if i=FRenderList.Count-1 then begin BindState:=BindState+[sDeactivate];bindedBuff:=nil;end;
             RenderVBOMultiPart(PDescr,BindState,false);
           end else begin
             P := MeshList[i];
             RenderVBOBuffer(P^);
           end;
           if assigned(FonAfterRender) then FonAfterRender(arci,self);
           //Render-----------------
           inc(i);
           if i<rcount then begin
             if FUseRenderList then begin
               PDescr:=FRenderList[i]; CMName:=PDescr.MaterialName;
             end else begin
               P := MeshList[i]; CMName:=P.MatName;
             end;
           end else CMName:='';
           if ((i=rcount) or (CMName<>ActiveMaterial)) and (assigned(Lm)) then
              MultiPass:=Lm.UnApply(arci) else MultiPass:=false;
           if MultiPass then i:=j else
           if CMName<>ActiveMaterial then begin
              Lm := MeshMaterialLibrary.LibMaterialByName(CMName);
              ActiveMaterial:=CMName;
              if assigned(Lm) then begin Lm.Apply(arci); j:=i; end
              else Lm:=nil;
           end;
        until i=rcount;
      end;
      end;//RCount<=0
    end else FCulled:=true;
    if NoZWrite then glDepthMask(true);
    if NoDepthTest then glEnable(GL_DEPTH_TEST);
    case FMeshType of
      mtScreenQuad: begin
       glPopMatrix; glMatrixMode(GL_MODELVIEW);
       glEnable(GL_DEPTH_TEST);
       glEnable(GL_LIGHTING);
      end;
      mtHUDSprite: begin
        glDepthMask(True); glEnable(GL_DEPTH_TEST); glEnable(GL_LIGHTING);
        glPopMatrix; glMatrixMode(GL_MODELVIEW);glPopMatrix;
      end;
      mtPoints, mtParticles: begin
        glDisable( GL_POINT_SPRITE );
        glDisable( GL_BLEND );
        glDepthMask(True);
        glEnable(GL_DEPTH_TEST);
      end;
    end;
    if FFBO.Active then FFBO.UnApply;
    glPopMatrix;
end;

procedure TVBOMeshObject.SetPosition(const Value: TVector);
begin
  MoveObject(Value); UpdateWorldMatrix;
end;

procedure TVBOMeshObject.SetScale(const Value: TVector);
begin
  ScaleObject(Value); UpdateWorldMatrix;
end;

procedure TVBOMeshObject.MoveForward(Step: single);
begin
  with Matrices do begin
    TranslationMatrix[3,0]:=TranslationMatrix[3,0]+FDirection[0]*Step;
    TranslationMatrix[3,1]:=TranslationMatrix[3,1]+FDirection[1]*Step;
    TranslationMatrix[3,2]:=TranslationMatrix[3,2]+FDirection[2]*Step;
  end; UpdateWorldMatrix;
end;

procedure TVBOMeshObject.MoveLeft(Step: single);
begin
  with Matrices do begin
    TranslationMatrix[3,0]:=TranslationMatrix[3,0]+FLeft[0]*Step;
    TranslationMatrix[3,1]:=TranslationMatrix[3,1]+FLeft[1]*Step;
    TranslationMatrix[3,2]:=TranslationMatrix[3,2]+FLeft[2]*Step;
  end; UpdateWorldMatrix;
end;

procedure TVBOMeshObject.MoveUp(Step: single);
begin
  with Matrices do begin
    TranslationMatrix[3,0]:=TranslationMatrix[3,0]+FUp[0]*Step;
    TranslationMatrix[3,1]:=TranslationMatrix[3,1]+FUp[1]*Step;
    TranslationMatrix[3,2]:=TranslationMatrix[3,2]+FUp[2]*Step;
  end; UpdateWorldMatrix;
end;


function TVBOMeshObject.GetPolyCount: integer;
var i,j,k,n,count:integer;
    P:PVBOBuffer;
begin
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
   Result:=Count;
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

function TVBOMeshObject.FGetAsParticles: TVBOParticles;
begin
   if Self is TVBOParticles then result:=TVBOParticles(self)
   else result:=nil;
end;

procedure TVBOMeshObject.MoveObject(x, y, z: single; AbsolutePos: boolean);
begin
   MoveObject(vectormake(x,y,z,1),AbsolutePos);
end;

procedure TVBOMeshObject.ScaleObject(ScaleX, ScaleY, ScaleZ: single;
  AbsoluteScale: boolean);
begin
  ScaleObject(vectormake(ScaleX, ScaleY, ScaleZ, 0),AbsoluteScale);
end;

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

procedure TVBOMeshObject.PackMeshesToTexture(var vtex,ntex: TGLTexture);
var vert,norm: array of single;
    i, size, count, len: integer;
    v,n,t:TaffineVector;
begin
  ListToMultiBuffer(MeshList,FMultiBuffer,FRenderList,false,0);
  FUseRenderList:=true;
  if not assigned(vtex) then vtex:=TGLTexture.Create;
  if not assigned(ntex) then ntex:=TGLTexture.Create;
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

procedure TVBOMeshObject.SetDirection(const Direction: TVector);
var up,left,right,dir: TVector;
begin
  with Matrices do begin
    up:=ModelMatrix[1];
    NormalizeVector(up);
    dir:=VectorNormalize(direction);
    right:=VectorCrossProduct(Dir, Up);
    if VectorLength(right)<1e-5  then begin
       right:=VectorCrossProduct(ZHmgVector, Up);
       if VectorLength(right)<1e-5 then
          right:=VectorCrossProduct(XHmgVector, Up);
    end;
    NormalizeVector(right);
    Up:=VectorCrossProduct(right, Dir);
    NormalizeVector(Up);
    Left:=VectorCrossProduct(Up, Dir);
    NormalizeVector(Left);
    ModelMatrix[0]:=Left;
    ModelMatrix[1]:=Up;
    ModelMatrix[2]:=Dir;
  end;
  UpdateWorldMatrix;
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

function TVBOMeshObject.LocalToAbsolute(P: TVector): TVector;
begin
  if not WorldMatrixUpdated then UpdateWorldMatrix;
  Result:=VectorTransform(P,Matrices.WorldMatrix);
end;

procedure TVBOMeshObject.PackTriListToTexture(var vtex: TGLTexture);
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

{ TVBOMesh }

function TVBOMesh.GetMesh(Index: Integer): TVBOMeshObject;
begin
   result:=FMeshList[Index];
end;

function TVBOMesh.GetCount: integer;
begin
  result:=FMeshList.Count;
end;

procedure TVBOMesh.BuildOctree(level: integer);
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

procedure TVBOMesh.BuildExtents;
var mo:TVBOMeshObject;
    i:integer;
begin
   for i:=0 to FMeshList.Count-1 do begin
      mo:=FMeshList[i];
      mo.FBaseExtents:=GetExtentsOfList(mo.MeshList);
      mo.UpdateWorldMatrix;
   end;
end;

constructor TVBOMesh.CreateAsChild(aParentOwner: TGLBaseSceneObject);
begin
  inherited;
  FMeshList:=TList.Create;
  FExtentsBuilded:=false;
  OctreeBuilded:=false;
  Visible:=true;
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
      if mo.FMeshType=ObjType then List.Add(mo);
  end;
end;

procedure TVBOMesh.GetObjectListByTypeSet(ObjTypes: TMeshTypeSet; var List: TList);
var i:integer;
    mo:TVBOMeshObject;
begin
  if not assigned(List) then List:=TList.Create else List.Clear;
  for i:=0 to FMeshList.Count-1 do begin
      mo:=FMeshList[i];
      if mo.FMeshType in ObjTypes then List.Add(mo);
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

function TVBOMesh.AddBox(Width,Height, Depth: single; TilesX, TilesY, TilesZ: integer): TVBOMeshObject;
var mo:TVBOMeshObject;
    Temp,Res:PVBOBuffer;
    wm:TMatrix;
begin
  FExtentsBuilded:=false;
  mo:=TVBOMeshObject.Create;
  New(Res);InitVBOBuff(Res^, GL_TRIANGLE_STRIP, DrawElements);
  Temp:=CreatePlane(Width,Depth,TilesX,TilesZ,false);
  Temp.Vertexes.Translate(affinevectormake(0,Height/2,0));
  AttachBuffer(Temp^,Res^);
  wm:=CreateRotationMatrixZ(Pi);
  Temp.Vertexes.TransformAsPoints(wm);
  Temp.Normals.TransformAsVectors(wm);
  AttachBuffer(Temp^,Res^);
  FreeVBOMem(Temp^);Temp:=CreatePlane(Height,Depth,TilesY,TilesZ,false);
  wm:=CreateRotationMatrixZ(-Pi/2);
  Temp.Vertexes.TransformAsPoints(wm);
  Temp.Normals.TransformAsVectors(wm);
  Temp.Vertexes.Translate(affinevectormake(Width/2,0,0));
  AttachBuffer(Temp^,Res^);
  wm:=CreateRotationMatrixZ(Pi);
  Temp.Vertexes.TransformAsPoints(wm);
  Temp.Normals.TransformAsVectors(wm);
  AttachBuffer(Temp^,Res^);

  FreeVBOMem(Temp^);Temp:=CreatePlane(Width,Height,TilesX,TilesY,false);
  wm:=CreateRotationMatrixX(-Pi/2);
  Temp.Vertexes.TransformAsPoints(wm);
  Temp.Normals.TransformAsVectors(wm);
  Temp.Vertexes.Translate(affinevectormake(0,0,-Depth/2));
  AttachBuffer(Temp^,Res^);
  wm:=CreateRotationMatrixX(Pi);
  Temp.Vertexes.TransformAsPoints(wm);
  Temp.Normals.TransformAsVectors(wm);
  AttachBuffer(Temp^,Res^); FreeVBOMem(Temp^);

  GenVBOBuff(Res^, False);
  if not assigned(FMeshList) then FMeshList:=TList.Create;
  with mo do begin
    FMeshType:=mtBox; Parent:=nil;
    MeshList.Add(Res); Visible:=true;
    Name:='VBOBox'+inttostr(FMeshList.Count);
    FBaseExtents:=GetExtentsOfList(MeshList);
    MeshMaterialLibrary:=MaterialLibrary;
    UpdateWorldMatrix;
  end; mo.FIndexInMesh:=FMeshList.Add(mo); Result:=mo;
end;

function TVBOMesh.AddPlane(Width, Height: single; TilesX, TilesY: integer; HeightFunc: TGetHeightFunc=nil): TVBOMeshObject;
var Temp: PVBOBuffer;
    mo: TVBOMeshObject;
begin
FExtentsBuilded:=false;
mo:=TVBOMeshObject.Create;
Temp:=CreatePlane(Width,Height,TilesX,TilesY);
  if not assigned(FMeshList) then FMeshList:=TList.Create;
  with mo do begin
    FMeshType:=mtPlane; Parent:=nil;
    MeshList.Add(Temp); Visible:=true;
    Name:='VBOPlane'+inttostr(FMeshList.Count);
    FBaseExtents:=GetExtentsOfList(MeshList);
    MeshMaterialLibrary:=MaterialLibrary;
    UpdateWorldMatrix;
  end; mo.FIndexInMesh:=FMeshList.Add(mo); Result:=mo;
end;

function TVBOMesh.AddSphere(Radius: single; VSegments, HSegments: integer;
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
  ks:=1/(HSegments-1);kt:=1/(VSegments-1);
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
    MeshList.Add(Temp); FMeshType:=mtSphere; Visible:=true;
    Name:='VBOSphere'+inttostr(FMeshList.Count); Parent:=nil;
    FBaseExtents:=GetExtentsOfList(MeshList);
    MeshMaterialLibrary:=MaterialLibrary;
    UpdateWorldMatrix;
  end;result:=mo; mo.FIndexInMesh:=FMeshList.Add(mo);
end;

destructor TVBOMesh.Destroy;
var i:integer;
    mo:TVBOMeshObject;
begin
  for i:=0 to FMeshList.Count-1 do begin
      mo:=FMeshList[i]; FreeAndNil(mo);
  end;  FMeshList.Free;
  inherited;
end;

procedure TVBOMesh.RenderMeshObject(MeshObject: TVBOMeshObject; var ARci: TRenderContextInfo);
var mvm,proj:TMatrix;
begin
  glGetFloatv(GL_MODELVIEW_MATRIX, @mvm);
  glGetFloatv(GL_PROJECTION_MATRIX, @proj);
  MeshObject.Matrices.ProjectionMatrix:=proj;
  MeshObject.FTime:=GetTime;
  MeshObject.RenderObject(ARCI,mvm);
end;

procedure TVBOMesh.DoRender(var ARci: TRenderContextInfo; ARenderSelf,
  ARenderChildren: Boolean);
var i:integer;
    mo:TVBOMeshObject;
    //F:TFrustum;
    time:Double;
begin
FPolyCount:=0; //F := GetFrustum;
if assigned(onBeforeRender) then onBeforeRender(arci);
   glGetFloatv(GL_MODELVIEW_MATRIX, @FViewMatrix);
   glGetFloatv(GL_PROJECTION_MATRIX, @FProjectionMatrix);
   time:=GetTime;
   if not Visible then exit;
   if ARenderSelf then begin
      if assigned(FMeshList) then begin
        for i:=0 to FMeshList.Count-1 do begin
         mo:=FMeshList[i];
         if assigned(mo) then begin
           if (mo.Visible) then begin
             mo.Matrices.ProjectionMatrix:=FProjectionMatrix;
             mo.FTime:=time;
             mo.RenderObject(ARCI,FViewMatrix);
             if not mo.FCulled then FPolyCount:=FPolyCount+mo.PolygonsCount;
           end;
         end;
        end;
      end;
   end;
if assigned(onAfterRender) then onAfterRender(arci);      
end;


function TVBOMesh.AddMeshFromFile(FileName: string; UseMeshMaterials:Boolean=true): TVBOMeshObject;
var ff:TGLFreeForm;
begin
  FExtentsBuilded:=false;
  FF := TGLFreeForm.Create(nil);
  FF.ObjectStyle:=[osDirectDraw];
  FF.UseMeshMaterials:=UseMeshMaterials;
  //if UseMeshMaterials then
  FF.MaterialLibrary:=MaterialLibrary;
  FF.loadfromfile( FileName );
  Result:=AddMeshFromFreeForm(FF);
  FF.Free;
end;

function TVBOMesh.AddMeshFromFreeForm(FF: TGLFreeForm): TVBOMeshObject;
var TempBuff:PVBOBuffer;
    i,j:integer;
    M:TMeshOBject;
    FG:TFGVertexIndexList;
    MatList:TStringList;
    mo: TVBOMeshObject;
    ml,lm:TGLLibMaterial;
    s:string;
const ModeName:array[0..5] of string=('Triangles','TriangleStrip', 'FaceGroups','Triangles', 'TrianglesFAN', 'Quads');
     cOpenGLMM : array [TFaceGroupMeshMode] of Integer =
         (GL_TRIANGLES, GL_TRIANGLE_STRIP, GL_TRIANGLES, GL_TRIANGLE_FAN, GL_QUADS);
begin
  mo := TVBOMeshObject.Create;
  FExtentsBuilded:=false;
  MatList := TStringList.Create;
  if not assigned(FMeshList) then FMeshList:=TList.Create;
  with mo do begin
   MeshMaterialLibrary:=MaterialLibrary;
   for i:=0 to FF.MeshObjects.Count-1 do begin
    M:=FF.MeshObjects[i];
    if M.Mode=momFaceGroups then
    for j:=0 to M.FaceGroups.Count-1 do begin
     FG:=TFGVertexNormalTexIndexList(M.FaceGroups[j]);
     if FG.VertexIndices.Count>0 then begin
      new(TempBuff);InitVBOBuff(TempBuff^,cOpenGLMM[FG.Mode],DrawElements);
      with TempBuff^ do begin
       MatName := FG.MaterialName;
       Name := M.Name;
       if MatName='' then MatName:='Null';
       if MatList.Values[MatName]='' then MatList.Values[MatName]:=inttostr(
         MatList.Count);
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
  FMeshType:=mtFreeForm; Visible:=true;
  Name:='VBOFreeForm'+inttostr(FMeshList.Count);
  Materials.AddStrings(MatList); Parent:=nil;
  if FF.MaterialLibrary<>MaterialLibrary then begin
     for i:=0 to MatList.Count-1 do begin
        s:=MatList.Names[i];
        lm:=MaterialLibrary.LibMaterialByName(s);
        if not assigned(lm) then begin
          ml:=FF.MaterialLibrary.LibMaterialByName(s);
          if assigned(ml) then begin
           MaterialLibrary.Materials.Add.Material:=ml.Material;
           MaterialLibrary.Materials[MaterialLibrary.Materials.Count-1].Name:=s;
          end;
        end;
     end;
  end;
  FBaseExtents:=GetExtentsOfList(MeshList);
  UpdateWorldMatrix;
  end; Result:=mo;  mo.FIndexInMesh:=FMeshList.Add(mo);
  MatList.Clear; MatList.Free;
end;


function TVBOMesh.AddScreenQuad: TVBOMeshObject;
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
    MeshList.Add(Temp); FMeshType:=mtScreenQuad; Visible:=true;
    Name:='VBOScreenQuad'+inttostr(FMeshList.Count); Parent:=nil;
    FBaseExtents:=GetExtentsOfList(MeshList);
    MeshMaterialLibrary:=MaterialLibrary;
    UpdateWorldMatrix;
    Pickable:=false;
  end; result:=mo; mo.FIndexInMesh:=FMeshList.Add(mo);
end;

function TVBOMesh.AddHUDSprite(width, height: single): TVBOMeshObject;
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
    MeshList.Add(Temp); FMeshType:=mtHUDSprite; Visible:=true;
    Name:='VBOHUDSprite'+inttostr(FMeshList.Count); Parent:=nil;
    FBaseExtents:=GetExtentsOfList(MeshList);
    MeshMaterialLibrary:=MaterialLibrary;
    Pickable:=false;
    UpdateWorldMatrix;
  end; result:=mo; mo.FIndexInMesh:=FMeshList.Add(mo);
end;

function TVBOMesh.AddSprite(s_type: TSpriteType; width,
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
      stSpherical:  FMeshType:=mtSphericalSprite;
      stCylindrical: FMeshType:=mtCylindricalSprite;
    Else FMeshType:=mtSphericalSprite;
    end;
    MeshList.Add(Temp); Visible:=true;
    Name:='VBOSprite'+inttostr(FMeshList.Count); Parent:=nil;
    FBaseExtents:=GetExtentsOfList(MeshList);
    MeshMaterialLibrary:=MaterialLibrary;
    Pickable:=false;
    UpdateWorldMatrix;
  end; result:=mo; mo.FIndexInMesh:=FMeshList.Add(mo);
end;

function TVBOMesh.ExtentsIntersect(const rayStart, rayVector: TVector; var List:Tlist): boolean;
var i:integer;
    mo:TVBOMeshObject;
    f:boolean;
    ri:PRaycastIntersectInfo;
    ip:TVector;
begin
   if not assigned(List) then List:=TList.Create else List.Clear;
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
var Proj,View,Inv,ViewProj:TMatrix;
    Vport:TVector4i;
    nx,ny,z:single;
    S,P:TVector;
begin
  glGetFloatv(GL_PROJECTION_MATRIX, @Proj);
  glGetFloatv(GL_MODELVIEW_MATRIX, @View);
  glGetIntegerv(GL_VIEWPORT,@Vport);
  ViewProj:=MatrixMultiply(View,Proj);
  Inv:=MatrixInvert(ViewProj);
  nx:=(x-Vport[0])/Vport[2]*2-1;
  ny:=(Vport[3]-(y-Vport[1]))/Vport[3]*2-1;
  setvector(S,nx,ny,-1,1);
  P:=VectorTransform(S,Inv);
  P[0]:=P[0]/P[3]; P[1]:=P[1]/P[3];
  P[2]:=P[2]/P[3]; P[3]:=1/P[3];
  if pos<>nil then begin pos^:=p; pos^[3]:=1; end;
  setvector(S,nx,ny,-3,1);
  P:=VectorTransform(S,Inv);
  P[0]:=P[0]/P[3]; P[1]:=P[1]/P[3];
  P[2]:=P[2]/P[3]; P[3]:=1;
  if dir<>nil then begin
    P:=VectorSubtract(p,pos^);
    ScaleVector(P,-1);NormalizeVector(P);
    dir^:=p;
  end;
  glReadPixels(trunc(x), trunc(Vport[3]-y), 1, 1, GL_DEPTH_COMPONENT, GL_FLOAT, @z);
  setvector(S,nx,ny,2*z-1,1);
  P:=VectorTransform(S,Inv);
  P[0]:=P[0]/P[3]; P[1]:=P[1]/P[3];
  P[2]:=P[2]/P[3]; P[3]:=1/P[3];
  result:=p;
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
    glGetFloatv(GL_MODELVIEW_MATRIX, @mv);
    mind:=-1; rio:=nil;
    for i:=0 to L.Count-1 do begin
        ri:=L[i]; mo:=ri.PickedObject;
        ip:=ri.iPoint;
        mv:=MatrixMultiply(mo.Matrices.WorldMatrix,mv);
        ip:=VectorTransform(ip,mv);
        d:=VectorNorm(ip);
        if (d<mind) or (mind<0) then begin
           mind:=d;result:=mo;rio:=ri;end;
    end;
    if assigned(result) and assigned(result.onObjectClick) then
       with rio^ do
          result.onObjectClick(trunc(X),trunc(Y),inScreen,inObject,Dir,result);
end;

function TVBOMesh.AddBBox(Corners: THmgBoundingBox; Color: TVector): TVBOMeshObject;
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
    MeshList.Add(Temp); FMeshType:=mtBBox; Visible:=true;
    Name:='VBOBoundingBox'+inttostr(FMeshList.Count); Parent:=nil;
    FBaseExtents:=GetExtentsOfList(MeshList);
    MeshMaterialLibrary:=MaterialLibrary;
    Pickable:=false;
    UpdateWorldMatrix;
  end; result:=mo; mo.FIndexInMesh:=FMeshList.Add(mo);
end;

function TVBOMesh.AddAnimatedSprite(s_type: TSpriteType; width,
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
      stSpherical:  FMeshType:=mtSphericalSprite;
      stCylindrical: FMeshType:=mtCylindricalSprite;
    Else FMeshType:=mtSphericalSprite;
    end;
    MeshList.Add(Temp); Visible:=true;
    Name:='VBOAnimatedSprite'+inttostr(FMeshList.Count); Parent:=nil;
    FBaseExtents:=GetExtentsOfList(MeshList);
    MeshMaterialLibrary:=MaterialLibrary;
    UpdateWorldMatrix;
    Pickable:=false;
  end; result:=mo; mo.FIndexInMesh:=FMeshList.Add(mo);
end;

function TVBOMesh.AddBBox(Extents: TExtents; Color: TVector): TVBOMeshObject;
var AABB:TAABB;
    Coners: THmgBoundingBox;
begin
   AABB.min:=Extents.emin;
   AABB.max:=Extents.emax;
   Coners:=AABBToBB(aabb);
   result:=AddBBox(Coners,Color);
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

function TVBOMesh.AddPoints(var Points: TAffineVectorArray;
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
    FParams:=p;
    MeshList.Add(Temp);
    FMeshType:=mtPoints; Visible:=true;
    Name:='VBOPoints'+inttostr(FMeshList.Count); Parent:=nil;
    FBaseExtents:=GetExtentsOfList(MeshList);
    MeshMaterialLibrary:=MaterialLibrary;
    Pickable:=false;
    UpdateWorldMatrix;
  end; result:=mo; mo.FIndexInMesh:=FMeshList.Add(mo);
end;

function TVBOMesh.AddPoints(PointsList:TAffineVectorList; ColorsList:
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
    FParams:=p;
    MeshList.Add(Temp); FMeshType:=mtPoints; Visible:=true;
    Name:='VBOPoints'+inttostr(FMeshList.Count); Parent:=nil;
    FBaseExtents:=GetExtentsOfList(MeshList);
    MeshMaterialLibrary:=MaterialLibrary;
    Pickable:=false;
    UpdateWorldMatrix;
  end; result:=mo; mo.FIndexInMesh:=FMeshList.Add(mo);
end;

function TVBOMesh.AddGrid(Width, Height: single; TilesX, TilesY: integer;
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
    MeshList.Add(Temp); FMeshType:=mtGrid; Visible:=true;
    Name:='VBOGrid'+inttostr(FMeshList.Count); Parent:=nil;
    FBaseExtents:=GetExtentsOfList(MeshList);
    MeshMaterialLibrary:=MaterialLibrary;
    UpdateWorldMatrix;
  end; result:=mo; mo.FIndexInMesh:=FMeshList.Add(mo);
end;

function TVBOMesh.AddProxyObject(MasterObject: TVBOMeshObject): TVBOMeshObject;
var   mo:TVBOMeshObject;
begin
  mo:=TVBOMeshObject.Create;
  with mo do begin
    FMeshType:=mtProxy;
    Visible:=true;
    Name:='VBOProxy'+inttostr(FMeshList.Count);
    FParams:=MasterObject; MeshList.Free;
    MeshList:=MasterObject.MeshList;    
    FBaseExtents:=MasterObject.FBaseExtents;
    UpdateWorldMatrix;
  end;
  MasterObject.FProxyList.Add(mo);
  result:=mo; mo.FIndexInMesh:=FMeshList.Add(mo);
end;

function TVBOMesh.AddUserObject(Name: string;
  VBOMeshList: TList): TVBOMeshObject;
var mo:TVBOMeshObject;
begin
  mo:=TVBOMeshObject.Create;
  if name='' then Name:='VBOUserObject';
  if (GetObjectByName(name)=nil) then mo.Name:=Name
  else mo.Name:=Name+inttostr(FMeshList.Count);
  with mo do begin
    FMeshType:=mtUser;
    FBaseExtents:=GetExtentsOfList(VBOMeshList);
    if Assigned(VBOMeshList) then
       MeshList.Assign(VBOMeshList);
    MeshMaterialLibrary:=MaterialLibrary;
    Visible:=true;
    UpdateWorldMatrix;
  end;
  result:=mo; mo.FIndexInMesh:=FMeshList.Add(mo);
end;

function TVBOMesh.AddUserObject(Name: string;
  VBOBuffer: PVBOBuffer): TVBOMeshObject;
var mo:TVBOMeshObject;
begin
  mo:=TVBOMeshObject.Create;
  if name='' then Name:='VBOUserObject';
  if (GetObjectByName(name)=nil) then mo.Name:=Name
  else mo.Name:=Name+inttostr(FMeshList.Count);
  with mo do begin
    FMeshType:=mtUser;
    MeshList.Add(VBOBuffer);
    FBaseExtents:=GetExtentsOfList(MeshList);
    Visible:=true; Parent:=nil;
    MeshMaterialLibrary:=MaterialLibrary;
    UpdateWorldMatrix;
  end;
  result:=mo; mo.FIndexInMesh:=FMeshList.Add(mo);
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

procedure TVBOMesh.SortByDistance(var SortedMeshList: TList;
  IgnoreBack: boolean);
Type TPDI = record p:TVector; d:single; end;
var dmin: single;
    i,j: integer;
    curr: TVBOMeshObject;
    vm: TMatrix;
    pd: array of TPDI;
    t: TPDI;
begin
    assert(SortedMeshList<>nil, 'SortedMeshList is not exists');
    if assigned(SortedMeshList) then SortedMeshList.Clear
    else SortedMeshList:=TList.Create;
    vm:=GetViewMatrix;
    setlength(pd,FMeshList.Count);
    for i:=0 to FMeshList.Count-1 do begin
       curr:=FMeshList[i];
       if not curr.WorldMatrixUpdated then curr.UpdateWorldMatrix;
       pd[i].p:=VectorTransform(curr.Position,vm);
       pd[i].d:=VectorNorm(pd[i].p);
    end;
    for i:=0 to FMeshList.Count-2 do begin
       curr:=FMeshList[i];
       if Curr.FMeshType in [mtHUDSprite, mtScreenQuad]
       then SortedMeshList.Add(curr)
       else begin
         dmin:=pd[i].d;
         for j:=i+1 to FMeshList.Count-1 do begin
             if pd[j].d<dmin then begin
                dmin:=pd[j].d; FMeshList.Exchange(i,j);
                t:=pd[j];pd[j]:=pd[i]; pd[i]:=t;
             end;
         end;
       end;
    end;
end;
procedure TVBOMesh.DeleteMeshObject(MeshObject: TVBOMeshObject);
var i:integer;
begin
  i:=FMeshList.IndexOf(MeshObject);
  DeleteMeshObject(i);
end;

procedure TVBOMesh.DeleteMeshObject(index: integer);
var MeshObject: TVBOMeshObject;
begin
   MeshObject:=FMeshList[index];
   FreeAndNil(MeshObject);
   FMeshList.Delete(index);
end;

function TVBOMesh.AddParticles(MaxCapacity: integer): TVBOMeshObject;
var mo:TVBOParticles;
begin
  mo:=TVBOParticles.Create(MaxCapacity);
  with mo do begin
    Name:='VBOParticles'+inttostr(FMeshList.Count);
    Parent:=nil;
    MeshMaterialLibrary:=MaterialLibrary;
    UpdateWorldMatrix;
  end;
  result:=mo; mo.FIndexInMesh:=FMeshList.Add(mo);
end;

function TVBOMesh.AddMeshObject(mo: TVBOMeshObject): integer;
begin
  result:=FMeshList.Add(mo);
  mo.UpdateWorldMatrix;
end;

function TVBOMesh.AddSMDAnimation(MeshFile: string;
  const AnimFiles: array of string): TVBOMeshObject;
var mo:TSkeletalRender;
    i:integer;
    path,s,t:string;
begin
  assert(MeshFile<>'','Need Mesh');
  mo:=TSkeletalRender.Create;
  mo.Name:='SMDAnimation'+inttostr(FMeshList.Add(mo));
  result:=mo;
  with mo do begin
    //Load Mesh File
    FAnim.Mesh:=SMDLoad(MeshFile);
    FAnimationName:=''; FAnimationNum:=-1; FOldFrame:=-1;
    //Load Animation files
    for i:=0 to high(AnimFiles) do AddAnimation(FAnim,AnimFiles[i]);
    //Create Mesh from SMD
    GetMeshFormSMD(FAnim.Mesh,MeshList,true);
//TODO: Проверить почему падает фпс при отсечении невидимой геометрии
    FBaseExtents:=GetExtentsOfList(MeshList);
    //Pack Animations to Texture
    FAnim.TextureId:=GetTextureFromAnim(FAnim);
    //Load Materials
    MeshMaterialLibrary:=MaterialLibrary;
    UpdateWorldMatrix;
    path:=ExtractFilePath(MeshFile);
    if path<>'' then if path[length(path)]<>'\' then path:=path+'\';
    for i:=0 to mo.FAnim.Mesh.Mesh.Textures.Count-1 do begin
       s:=mo.FAnim.Mesh.Mesh.Textures[i];
       t:=s; delete(t,length(t)-4,4);
       MaterialLibrary.AddTextureMaterial(t,path+s).Material.Texture.Enabled:=true;
    end;
    //Pack Mesh to Texture
    PackMeshesToTexture(vtex,ntex);
    glDeleteBuffers(1,@FMultiBuffer[0].Buff.vid);
    glDeleteBuffers(1,@FMultiBuffer[0].Buff.nid);
    RotateAroundX(-pi/2); //visible:=false;
    rvtex:=TGLTexture.Create; //Vertex readback Texture
    rvtex.CreateRGBA32FTexture2D(vtex.Width,vtex.Height);
    rntex:=TGLTexture.Create; //Vertex readback Texture
    rntex.CreateRGBA32FTexture2D(vtex.Width,vtex.Height);

    //Setup FBO
    mo.FScreenQuad:=AddScreenQuad;
//TODO: Разобраться почему прямой рендеринг скринквада
//оказывается медленее рендеринга квада как объекта  
    //FMeshList.Delete(FScreenQuad.FIndexInMesh);
    with FScreenQuad do begin
       with FBO do begin
         AttachTexture(rvtex);
         AttachTexture(rntex);
         SetReadBackBuffer([0,1]);
         InitFBO(vtex.Width,vtex.Height);
         Active:=true;
       end;
      onBeforeRender:=FApplyShader;
      onAfterRender:=FUnApplyShader;
      visible:=true;
    end;
    //Create Shader
    FCreatSMDShader;
  end;
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
      //режимы смешивания
    PointBlend:= true;
    BlendSFactor:= GL_SRC_ALPHA;
    BlendDFactor:= GL_ONE;
    AlphaTest:=false;
      //это лучше всегда держать выключенным, задает "аппаратное сглаживание" спрайта,
      //тоесть квадрат вырождается в круг. Тормоза страшные.
    PointSmooth:= false;
      //говорит как будет выглядеть эта точка, как точка (квадрат с заданным материалом)
      //или как текстурированный спрайт
    PointSprite:=true;
      //режимы теста глубины, чтоб не рисовалось за объектами и само не писалось
      //в буфер глубины, позволяет обойти проблему с прозрачностью неотсортированных
      //перекрывающихся спрайтов
    DepthTest:=true;
    DepthMask:=false;
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

procedure TVBOParticles.SetVel(Index: Integer; const Value: TVector);
begin
   FVelocityList[Index]:=AffineVectorMake(Value);
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
       UpdateVBOBuff(Buff.vId,FPositions.List,offs,size,true);
    end;
    if FPointParam.UseColors and (upColor in UpdBuff) then begin
       offs:=Index*sizeof(TVector);
       size:=Count*sizeof(TVector);
       UpdateVBOBuff(Buff.cId,FColors.List,offs,size,true);
    end;
  end;
end;

procedure TVBOParticles.RenderObject(ARCI: TRenderContextInfo;
  var ViewMatrix: TMatrix);
begin
  if assigned(FPFXManager) then begin
    FPFXManager.SetUpdateCounts(FCount);
    if FPFXManager.UpdateParticles then begin
       FUpdateBuffer(0,FCount,[upVertex]);
       FVNeedUpdate:=false;
    end;
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

procedure TVBOAnimatedSprite.RenderObject(ARCI: TRenderContextInfo;
  var ViewMatrix: TMatrix);
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
    samPlayOnce: if n=FFramesCount then FAnimated:=False;
    samLoop: if n=FFramesCount then n:=0;
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

procedure TSkeletalRender.FApplyShader(rci: TRenderContextInfo; mo: TObject);
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
  Frame[2]:=trunc(FramePosition+1)*2; Frame[3]:=0;
//  if Frame[2]>=FFramesCount then Frame[2]:=0;
  Shaders.UseProgramObject(spid);
  Shaders.SetUniforms(spid,'VertexTexture',0);
  Shaders.SetUniforms(spid,'NormalTexture',1);
  Shaders.SetUniforms(spid,'BoneTexture',2);
  Shaders.SetUniforms(spid,'frame',Frame);
  Shaders.SetUniforms(spid,'alpha',pos);
end;

procedure TSkeletalRender.FCreatSMDShader;
const Vertex: string =
'varying vec2 TexCoord;'+#13+#10+
'void main ()'+#13+#10+
'{'+#13+#10+
'  TexCoord = gl_MultiTexCoord0.xy;'+#13+#10+
'  gl_Position = ftransform();'+#13+#10+
'}';
const Fragment: string =
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
'  vec4 qp = texture2DRect(BoneTexture, vec2(frame.x,frame.y+v.w));'+#13+#10+
'  vec4 qo = texture2DRect(BoneTexture, vec2(frame.x+1.0,frame.y+v.w));'+#13+#10+
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
  end;
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

procedure TSkeletalRender.FUnApplyShader(rci: TRenderContextInfo; mo: TObject);
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

  //Visible:=true;

  with MultiBuffer[0].Buff do begin
    vid:=rvtex.PBOReadBuffer;
    nid:=rntex.PBOReadBuffer;
    nccount:=4; vccount:=4;
  end;
end;

procedure TSkeletalRender.RenderObject(ARCI: TRenderContextInfo;
  var ViewMatrix: TMatrix);
begin
  if FAnimationNum=-1 then exit;
  if FramePosition<>FOldFrame then begin
//     glGetFloatv(GL_PROJECTION_MATRIX, @FScreenQuad.Matrices.ProjectionMatrix);
//     FScreenQuad.FTime:=FTime;
//     if FScreenQuad.Visible then
//        FScreenQuad.RenderObject(ARCI,ViewMatrix);
//     FScreenQuad.Visible:=false;
     FOldFrame:=FramePosition;
  end;
  inherited;
end;

end.

