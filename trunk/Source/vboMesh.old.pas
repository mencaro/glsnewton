{: vboMesh
	Historique:
  13/08/11 - Fantom - мелкий багфикс ошибок найденных SoV
        07/06/11 - Fantom - Оптимизировано построение Октри
                          - Исправлена привязка объекта через Parent
  09/05/11 - Fantom - Оптимизирована сортировка инстансов
        07/05/11 - Fantom - Исправлена ошибка переключения анимаций в TSkeletalRender.
                          - Добавлено свойство AnimationsCount в TSkeletalRender и
                            TUniformSMDRender
  03/05/11 - Fantom - Добавлена сортировка частиц TVBOParticles.Sorting: TSortDirection
        02/05/11 - Fantom - Исправлена ошибка проверки видимости массива партиклов.
                          - Добавлен метод TVBOMeshObject.UpdateExtents;
  01/05/11 - Fantom - Свойства PointBlend, AlphaTest, BlendSFactor, BlendDFactor заменены
                      свойством TVBOParticles.Blending: TBlendingModes;
                      Свойства DepthTest и DepthMask заменены на NoDepthTest и NoZWrite
        26/04/11 - Fantom - Создан класс TUniformSMDRender для рендеринга скелетной анимации
                            при передаче скелета через юниформы.
                            Добавлен метод TVBOMesh.AddUniformSMD для добавления TUniformSMDRender
  25/04/11 - Fantom - Восстановлена работа скелетной анимации
        15/02/11 - Fantom - Изменена работа с прокси - метод AddInstanceToObject создает
                            низкоуровневый прокси(быстрый рендер, не действует окклюжин куллинг
                            и смена материалов) и AddProxyObject - полноценный объект ссылающийся
                            на геометрию мастера
                          - Добавлена технология окклюжн куллинга, в связи с чем переработан TVBOMesh.DoRender.
                            Активируется окклюжн куллинг через свойство TVBOMesh.OcclusionCulling
                          - Восстановлена система уровней детализации геометрии(LOD), добавить ЛОД можно через
                            TVBOMeshObject.AddLod
  24/12/10 - Fantom - Добавлен режим отображения граней TVBOMesh.FaceMode
        25/09/10 - Fantom - Внесены исправления в работу TVBOTerrain, исправлен возврат значений
                            рейкаста для несгенерированного октри
        25/09/10 - Fantom - Заменено прямое чтение матриц View, Proj, Viewport в функциях
                            ScreenToWorld/WorldToScree на последние использованные при рендеринге
  29/08/10 - Fantom - Исправлена ошибка текстурирования фриформ.
        19/08/10 - Fantom - Произведена подмена сценовских материалов моими
        19/08/10 - Fantom - Произведена некторая чистка модулей от остатков RCI, в следствии чего
                            изменены конструктор VBOMesh, методы DoRender и события
                            onBefore/AfterRender
        19/08/10 - Fantom - Изменен принцип работы прокси
        19/08/10 - Fantom - Добавлено кеширование стэйтов OGL
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

Uses
     Windows, GLScene, OpenGL1x, VectorTypes, VectorGeometry, Classes, SysUtilsLite,
     uVBO, Types, GLVectorFileObjects, GLFile3ds, GLFileObj, VectorLists,
     GLMaterial, Octree, GeometryBB, PFXManager, uTextures, uFBO, uShaders,
     uFileSMD, uFileObj, uMaterials, GLRenderContextInfo, OGLStateEmul;

Type
  TMeshTypes = (mtPlane, mtBox, mtSphere, mtFreeForm, mtHUDSprite, mtSprite, mtBBox,
                mtSphericalSprite, mtCylindricalSprite, mtScreenQuad, mtPoints,
                mtGrid, mtProxy, mtInstance, mtUser, mtParticles);
  TMeshTypeSet = set of TMeshTypes;
  TTransformsTypes = (ttPosition, ttScale, ttRotation, ttModel, ttParent, ttFollow, ttAll);
  TTransforms = set of TTransformsTypes;
  TSpriteType = (stSpherical, stCylindrical);
  TBlendingModes = (bmOpaque, bmTransparency, bmAdditive, bmAlphaTest50,
                    bmAlphaTest100, bmModulate, bmCustom);
  TSortDirection = (sdFrontToBack, sdBackToFront, sdNone);
  T2DSingleArray = array of array of single;

  TFaceMode = (fmPoints, fmLines, fmFill);

  PPointParam = ^TPointParam;
  TPointParam = record
    DistanceAttenuation: TVector3f;
    FadeTresholdSize: single;
    MinPointSize:single;
    PointSize:single;
    PointSmooth: boolean;
    PointSprite: boolean;
    UseColors: boolean;
  end;

  TVBOMeshObject = class;

  PLODs = ^TLoDs;
  TLoDs= record
     MaxViewDistance: single;
     LodMesh: TVBOMeshObject;
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
    ViewMatrix: TMatrix; //Видовая матрица, заполняется при рендеринге
  end; PMatrixStack = ^TMatrixStack;

  TObjectRenderEvents = procedure (MeshObject: TObject) of object;
  TVBOMeshRenderEvents = procedure of object;
  TVBOObjectClickEvents = procedure (X,Y:integer; NearPos,inObjectPos,dir: TAffineVector; MeshObject: TObject) of object;
  TVBOParticles = class;
  TVBOMesh = class;

  TCustomBlending = class
     private
       FBlendEnable: boolean;
       FAlphaTestEnable: boolean;
       FSrcBlendFunc: GLEnum;
       FDstBlendFunc: GLEnum;
       FAlphaFunc: GLEnum;
       FAlphaThreshold: single;
     public
       constructor Create;
       destructor Destroy;override;
       procedure Apply;

       property BlendEnable: boolean read FBlendEnable write FBlendEnable;
       property AlphaTestEnable: boolean read FAlphaTestEnable write FAlphaTestEnable;
       property SrcBlendFunc: GLEnum read FSrcBlendFunc write FSrcBlendFunc;
       property DstBlendFunc: GLEnum read FDstBlendFunc write FDstBlendFunc;
       property AlphaFunc: GLEnum read FAlphaFunc write FAlphaFunc;
       property AlphaThreshold: single read FAlphaThreshold write FAlphaThreshold;
  end;

  TSceneOctree = class;

  TVBOMeshObject = class
  Private
    FParent: TVBOMeshObject;
//    FOwner: TVBOMesh;
    FRCI: TRenderContextInfo;
    FIndexInMesh: integer;
    FHandle: TVBOMeshObject;
    FParams: Pointer;
    FFBO: TFrameBufferObject;
    //координатный базис
    FAbsolutePosition: TVector;
    FPosition: TVector; //глобальные координаты объекта
    FScale: TVector;    //масштаб объекта, совместно с положением - только для чтения
    FUp: TVector; // OY
    FDirection: TVector; //OZ
    FLeft: TVector; //OX

    FExtents: TExtents;  //модифицированный текущей модельной матрицей
    FBaundedRadius: single; //Радиус окаймляющей сферы
    FGeomCenter: TAffineVector; //геометрический центр объекта
    FOctreeList: TList;
    FExtentsBuilded: boolean;
    FonBeforeCheckVisibility:TObjectRenderEvents;
    FonBeforeRender:TObjectRenderEvents;
    FonAfterRender:TObjectRenderEvents;
    FonObjectClick: TVBOObjectClickEvents;
    FonApplyShader: TObjectRenderEvents;
    FonUnApplyShader: TObjectRenderEvents;

    FCulled: boolean;
    FOccluded: boolean;
    FIgnoreOcclusion: boolean;

    FBlendingMode: TBlendingModes;
    FCustomBlending: TCustomBlending;

    FSceneMaterialName: String;
    FSceneMaterial: TGLLibMaterial;
//    FGetAsParticles: TVBOParticles;
    FFaceMode: TFaceMode;
    FUseLods: boolean;
    FTwoSides: boolean;
    FPolygonsCount: integer;

    function  FGetAsParticles:TVBOParticles;
    procedure SetParent(const Value: TVBOMeshObject);
    procedure SetPosition(const Value: TVector);
    procedure SetScale(const Value: TVector);
    procedure UpdateExtents;
    function  AABBUpdate(const aabb: TAABB; const WorldMatrix:TMatrix): TAABB;
    function  AABBTransform(const ext: TExtents; const WorldMatrix:TMatrix): TExtents;
    function GetPolyCount: integer; virtual;
    //Ориентирует объект в заданном направлении
    procedure SetDirection(const Direction: TVector);
    procedure SetMaterial(const Value: TGLLibMaterial);
    procedure ImportBlending(Mat: TGLMaterial);
    procedure ExtentsToTranslateScale(var pos, sc: TVector);
    procedure SetFaceMode;
    procedure RebuildProxyList(const ViewMatrix, mv: TMatrix);
    procedure SortProxyByDistance(SortDirection: TSortDirection);
    function getBuffer(index: integer): PVBOBuffer;
  Protected
    FUseRenderList: boolean;
    FRenderList:TList;
    FMultiBuffer:TMultiPackBuff;

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
    FMaterial: TMaterial;
    FTexture: TGLTexture;
    FMaterials: TMaterialLibrary;
    FTextures: TGLTextureLibrary;
    FProxyList: TList;
    FProxyMatrixList: TList;
    FLodList: TList;
    procedure SetMaterialName(const Name: string);
    procedure SetBlending;
    procedure ResetBlending;
  Public
    Name: string; //Имя объекта
    FriendlyName: string; //храните любой текст или комментарии тут
    Tag: integer; //для нужд пользователя
    DirectingAxis: TVector; //Хранит направляющую ось Axis
    Matrices:TMatrixStack;
    MeshList: TList; //список VBO буферов
    Materials: TStringList;

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
    //Вызываются до и после рендеринга объекта
    property onApplyShader: TObjectRenderEvents read FonApplyShader write FonApplyShader;
    property onUnApplyShader: TObjectRenderEvents read FonUnApplyShader write FonUnApplyShader;

    Constructor Create;
    Destructor Destroy;override;

    Procedure RenderObject(const ViewMatrix: TMatrix);virtual;
    Procedure RenderOccluder(const ViewMatrix: TMatrix; Occluder: PVBOBuffer);virtual;
    //Обновляет материалы экспортированные со сцены
    Procedure UpdateMaterialList;
    //Конвертирование сценовских материалов в родные
    Property Material: TGLLibMaterial read FSceneMaterial write SetMaterial;
    Property MaterialName: string read FSceneMaterialName write SetMaterialName;
    Property MaterialExt: TMaterial read FMaterial write FMaterial;
    Property TextureExt: TGLTexture read FTexture write FTexture;
    //Настройки смешивания
    Property Blending: TBlendingModes read FBlendingMode write FBlendingMode;
    Property CustomBlending: TCustomBlending read FCustomBlending;
    Property TwoSides: boolean read FTwoSides write FTwoSides;

    //Указатель на самого себя
    Property Handle: TVBOMeshObject read FHandle;
    //Индекс в контейнере
    Property IndexInMesh: integer read FIndexInMesh;
    //Доступ к буферу кадра
    Property FBO: TFrameBufferObject read FFBO;
    //Возвращает тип объекта
    Property MeshType:TMeshTypes read FMeshType;
    //возвращает координаты окаймляющего бокса
    Property Extents:TExtents read FExtents;
    //установка родителя, из которого будет браться базовая матрица трансформаций
    Property Parent:TVBOMeshObject read FParent write SetParent;
    //количество полигонов
    Property PolygonsCount: integer read GetPolyCount;
    //Установка/чтение локальных координат
    Property Position: TVector read Matrices.TranslationMatrix[3] write SetPosition;
    //Глобальные координаты объектов
    Property AbsolutePosition: TVector read FAbsolutePosition;
    //Установка/чтение масштаба объекта
    Property Scale: TVector read FScale write SetScale;
    //Угол поворота в плоскости экрана
    Property RollAngle: single read FRollAngle write FRollAngle;
    //Установка/чтение ориентации объекта
    Property Direction: TVector read Matrices.WorldMatrix[2] write SetDirection;
    Property Left: TVector read Matrices.WorldMatrix[0];
    Property UP: TVector read Matrices.WorldMatrix[1];

    //Читает мультибуфер
    Property MultiBuffer: TMultiPackBuff read FMultiBuffer;
    //Режим отображения граней
    Property FaceMode: TFaceMode read FFaceMode write FFaceMode;
    //Игнорировать проверку перекрытия
    Property IgnoreOcclusion: boolean read FIgnoreOcclusion write FIgnoreOcclusion;
    Property UseLods: boolean read FUseLods write FUseLods;
    //Доступ к буферам VBO
    Property Items[index: integer]: PVBOBuffer read getBuffer;

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
    //Переводит мировые ортогональные координаты в относительные координаты
    Function ScreenToLocal(P: TVector): TVector;
    //Очищает занимаемую объектом оперативную память
    Procedure FreeObjectsMemory;
    //Строит Octree по каждому из мешей в MeshList
    Procedure BuildOctreeList(Level:integer=3);
    //Упаковывает все меши в один буфер
    Procedure PackMeshes(FreeOldBuffers:boolean=false; BuffSize:integer=-1);
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
    Function PointInExtents(p: TVector): boolean;

    //Добавляет LOD
    Procedure AddLod(LOD: TVBOMeshObject; MaxViewDistance:single);
    Procedure ClearProxy;
  end;

  TVBOTerrain = class (TVBOMeshObject)
    private
      FHMWidth, FHMHeight: integer;
      FHMap: array of array of single;
      FXTiles, FYTiles: integer;
      FPatchWidth, FPatchHeight: integer;
      FCreateBuff: TRenderBuff;
      FExtList: TList;
      FOffsList: TIntegerList;
      FCountList: TIntegerList;
      FVisiblePolyCount: integer;
      function GetPolyCount: integer; override;
      Procedure GetHeights(Width, Height: integer; GetHeightFunc: TGetHeightFunc);
      Procedure CreateAndFillTriBuff(TBuff: PVBOBuffer; Rect: TRect);
      Procedure CreateAndFillTriStripBuff(TBuff: PVBOBuffer; Rect: TRect);
      Procedure CreateSolidBuff;
    public
      FaceMode: (fmTriangles, fmTriangleStrips);
      Constructor Create;
      Procedure BuildTerrain(Width, Height: integer; GetHeightFunc: TGetHeightFunc);
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
      property DistanceAttenuation: TVector3f read FPointParam.DistanceAttenuation
                                             write FPointParam.DistanceAttenuation;
      property FadeTresholdSize: single read FPointParam.FadeTresholdSize
                                       write FPointParam.FadeTresholdSize;
      property MinPointSize:single read FPointParam.MinPointSize;
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

      procedure FApplyShader(mo:TObject);
      procedure FUnApplyShader(mo:TObject);
      procedure FSetAnimationByNum(Value: integer);
      procedure FSetAnimationByName(Value: string);
      procedure FSetFrame(Value: single);
      procedure FCreatSMDShader;
      function GetAnimCount: integer;
    public
      Destructor Destroy; override;

      Procedure RenderObject(const ViewMatrix: TMatrix);override;
      Procedure NextFrame(n:single=1);

      Property FramePos: single read FramePosition write FSetFrame;
      Property FramesCount: integer read FFramesCount;
      Property AnimationsCount: integer read GetAnimCount;
      Property AnimationNum: integer read FAnimationNum write FSetAnimationByNum;
      Property AnimationName: string read FAnimationName write FSetAnimationByName;
  end;

  TUniformSMDRender = class (TVBOMeshObject)
    private
      FAnim: TAnimations;
      spId: GLUInt;
      Shaders: TShaders;
      FFramesCount: integer;
      FOldFrameNum: single;
      FramePosition: single;
      FAnimationName: string;
      FAnimationNum: integer;
      FOldFrame: single;
      FBones: integer;
      FBoneArray: array of TVector;
      FSmoothed: boolean;
      FBlended: boolean;
      FCurrentFrame: TSMDNodes;

      procedure FApplyShader(mo:TObject);
      procedure FUnApplyShader(mo:TObject);
      procedure FSetAnimationByNum(Value: integer);
      procedure FSetAnimationByName(Value: string);
      procedure FSetFrame(Value: single);
      procedure FCreatSMDShader;
      function GetAnimCount: integer;
      function GetFramesCount(index: integer): integer;
      procedure setBone(Index: Integer; const Value: TVector);
      function getBone(index: integer): TVector;
    public
      Destructor Destroy; override;

      Procedure RenderObject(const ViewMatrix: TMatrix);override;
      Procedure NextFrame(n:single=1);
      Procedure BlendAnimation(AnimIdx1,AnimIdx2: integer; Frame1,Frame2, Factor: single);
      Procedure BlendWithCurrentFrame(Anim: integer; Frame: single; Factor: single);

      Property FramePos: single read FramePosition write FSetFrame;
      Property FramesCount: integer read FFramesCount;
      Property AnimationsCount: integer read GetAnimCount;
      Property AnimationNum: integer read FAnimationNum write FSetAnimationByNum;
      Property AnimationName: string read FAnimationName write FSetAnimationByName;
      Property Smoothed: boolean read FSmoothed write FSmoothed;
      Property AnimationsFrames[index: integer]: integer read GetFramesCount;
      Property Bones[index: integer]: TVector read getBone write setBone;
      Property CurrentFrame: TSMDNodes read FCurrentFrame write FCurrentFrame;
  end;

  TVBOMesh = class(TGLBaseSceneObject)
    private
      FMeshList: TList;
      FExtentsBuilded: Boolean;
      FViewMatrix: TMatrix;
      FProjectionMatrix: TMatrix;
      FViewPort: array [0..3] of integer;
      FPolyCount: integer;
      FonBeforeRender: TVBOMeshRenderEvents;
      FonAfterRender: TVBOMeshRenderEvents;

      FMaterials: TMaterialLibrary;
      FTextures: TGLTextureLibrary;
      FOccluder: PVBOBuffer;
      FOcclusionCulling: boolean;
      FLights: TLightLibrary;
      FSortDirection: TSortDirection;
      FQueryObjectList: TList;
      FSceneOctree: TSceneOctree;
      FRenderPass: integer;
      function GetVisibleObjects(const Frustum: TFrustum): TList;
      function LodSelect(mo: TVBOMeshObject): TVBOMeshObject;
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
      property MeshList: TList read FMeshList;
      property ObjectsList[Index: Integer]: TVBOMeshObject read GetMesh;default;
      property PolygonsCount: integer read FPolyCount;
      property OcclusionCulling: boolean read FOcclusionCulling write FOcclusionCulling;
      property Materials: TMaterialLibrary read FMaterials write FMaterials;
      property Textures: TGLTextureLibrary read FTextures write FTextures;
      property Lights: TLightLibrary read FLights write FLights;
      property SortDirection: TSortDirection read FSortDirection write FSortDirection;
      property SceneOctree: TSceneOctree read FSceneOctree;
      property ViewMatrix: TMatrix read FViewMatrix;


      Procedure DoRender(var ARci: TRenderContextInfo;
                            ARenderSelf, ARenderChildren: Boolean); override;

      Procedure RenderMeshObject(MeshObject: TVBOMeshObject);
      Procedure DeleteMeshObject(MeshObject: TVBOMeshObject; FreeObject: boolean=true);overload;
      Procedure DeleteMeshObject(index: integer; FreeObject: boolean=true);overload;
      Procedure Clear(FreeObjects: boolean=true);
      Function Last: TVBOMeshObject;

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
      Function AddSprite(s_type:TSpriteType;width, height:single): TVBOMeshObject;
      Function AddAnimatedSprite(s_type:TSpriteType;width, height:single): TVBOMeshObject;
      Function AddMeshFromFreeForm(FF: TGLFreeForm): TVBOMeshObject;
      Function AddMeshFromFile(FileName: string; UseMeshMaterials:Boolean=true): TVBOMeshObject;
      Function AddBBox(Corners:THmgBoundingBox; Color:TVector): TVBOMeshObject;overload;
      Function AddBBox(Extents:TExtents; Color:TVector): TVBOMeshObject;overload;
      Function AddInstanceToObject(MasterObject:TVBOMeshObject): TVBOMeshObject;
      Function AddProxyObject(MasterObject:TVBOMeshObject): TVBOMeshObject;
      Function AddUserObject(Name:string; VBOMeshList:TList): TVBOMeshObject;overload;
      Function AddUserObject(Name:string; VBOBuffer:PVBOBuffer): TVBOMeshObject;overload;
      Function AddMeshObject(mo:TVBOMeshObject): integer;
      Function AddSMDAnimation(MeshFile: string; const AnimFiles: array of string): TVBOMeshObject;overload;
      Function AddSMDAnimation(SMD: TSkeletalRender):TVBOMeshObject; overload;
      Function AddUniformSMD(MeshFile: string; const AnimFiles: array of string): TUniformSMDRender;overload;
      Function AddUniformSMD(SMD: TUniformSMDRender): TUniformSMDRender; overload;


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
      //Переводит точку из глобальной системы координат (перспективной) в координаты плоскости экрана(ортогональные)
      Function WorldToPlane(P: TVector): TVector;

      //Сортирует список объектов по расстоянию до камеры
      Procedure SortByDistance(List: TList=nil; SortDirection: TSortDirection=sdFrontToBack);
      //Обновляет кеш сценовских материалов 
      Procedure UpdateMaterialCache;
      Procedure UpdateSceneMatrix;
  end;

  TParticleEmitter = class
  private
    fPFX: TPFXManager;
    fParticles: TVBOParticles;
    function ParticleLifeTime(var PLife: TParticleLifeCycle;
                            CheckTime: TCheckTime):TCheckTimeRes;
  public
    Position: TAffineVector;
    Direction: TAffineVector;
    DirectionRange: TAffineVector;
    LifeTime: single;
    LifetimeRange: single;
    Count: integer;
    Speed: TAffineVector;
    SpeedRange: TAffineVector;
    Gravity: TAffineVector;
    Acceleration: TAffineVector;
    AccelerationRange: TAffineVector;

    constructor Create(Particles: TVBOParticles);
    destructor Destroy;override;
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
    function GetNode(Index: Integer): TAffineVector;
    procedure SetNode(Index: Integer; const Value: TAffineVector);
    procedure CreateVLShader;
    procedure AddLine(I1,I2: integer);
    procedure AddTestQuad;
  public
    constructor Create(MaxNodes: integer = 1000);
    destructor Destroy; override;
    Procedure RenderObject(const ViewMatrix:TMatrix);override;

    function AddNode(v: TAffineVector): integer; overload;
    function AddNode(x,y,z: single): integer; overload;

    property LineWidth: single read FLineWidth write FLineWidth;
    property Nodes[Index: Integer]: TAffineVector read GetNode write SetNode; default;
    property Count: integer read FCount;

//    procedure Delete
//    procedure Exchange
//    procedure Clear
  end;

  TVBOActor = class
  private
    FMeshIndex: integer;
    FMeshOffset: integer;
    FAnimIndices: TIntegerList;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TActorObject = class
  private
    FBaseExtents: TExtents;
    FPosition,FScale,FDirection: TVector;
    FModelMatrix: TMatrix;
    vOffset, iOffset: integer;
    vCount, iCount: integer;
    Frame: single;
  public
  end;

  TAnimationManager = class
  private
    FMeshes: TList;
    FAnimations: TList;
    FActorList: TList;
    FActorPack: PVBOBuffer;
    FActors: TList;
    vtex,ntex,ftex: TGLTexture;
    rvtex, rntex: TGLTexture;
    FActIdx: array of integer;
    FFBO: TFrameBufferObject;
    function GetActor(Index: integer): TActorObject;
    function ActorByOffset(vOffset: integer): integer;
    procedure PackActorsToTexture;
    procedure UpdateFrames;
  public
    constructor Create;
    destructor Destroy; override;
    function AddSMDFile(MeshFile: string; const AnimFiles: array of string): integer;
    function AddActorToPack(Index: integer): TActorObject;
    procedure BuildActorPack;
    procedure UpdateGeometry;
    property Actors[Index: integer]: TActorObject read GetActor; default;
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
    procedure FrustumCulling(var Lod: TOctreeLod;
      const Frustum: TFrustum);
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
  function CreateBBMatrix (const View: TMatrix; const mat: TMatrixStack; angle: single;stype: TSpriteType):TMatrix;

implementation

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
   end;
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
    ResetMatrices; UpdateWorldMatrix; UpdateMaterialList;Visible:=true;
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
var Temp:TOctree;
    P:PVBOBuffer;
    i:integer;
    TriList:TAffineVectorList;
begin
  for i:=0 to MeshList.Count-1 do begin
   TriList:=TAffineVectorList.Create;
   P:=MeshList[i];
   if (p.Vertexes.Count mod 3) = 0 then
     TriList.Assign(p.Vertexes)
   else ExtractTriangles(P^,TriList);
   Temp:=TOctree.Create;
   with Temp do begin
    DisposeTree;
    InitializeTree(P.emin, P.emax, TriList, Level);
   end;
   FOctreeList.Add(Temp); TriList.Free;
  end;
  FOctreeBuilded:=true;
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
  FProxyMatrixList.Clear; FProxyList.Clear;
end;

constructor TVBOMeshObject.Create;
begin
  FHandle:=inherited Create;
  with Matrices do begin
    ModelMatrix:=IdentityHmgMatrix;
    ScaleMatrix:=IdentityHmgMatrix;
    RotationMatrix:=IdentityHmgMatrix;
    TranslationMatrix:=IdentityHmgMatrix;
    WorldMatrix:=IdentityHmgMatrix;
    WorldMatrixT:=IdentityHmgMatrix;
    InvWorldMatrix:=IdentityHmgMatrix;
  end;

  FRollAngle:=0;  FTurnAngle:=0;  FPitchAngle:=0;
  FXRotationAngle:=0;  FYRotationAngle:=0;  FZRotationAngle:=0;

  MeshList:=TList.Create;
  Materials:=TStringList.Create;
  FOctreeList:=TList.Create;
  FLodList:=TList.Create;
  FOctreeBuilded:=false;
  FProxyList:=TList.Create;
  FProxyMatrixList:=TList.Create;
  FPosition:=vectormake(0,0,0,0);
  FScale:=vectormake(1,1,1,1);
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
  FCustomBlending:=TCustomBlending.Create;
  FTwoSides:=false;
  FPolygonsCount:=-1;
end;

destructor TVBOMeshObject.Destroy;
var i: integer;
    temp: TOctree;
    pd: PMultiRenderDescr;
    m: PMatrix;
    pl: PLODs;
begin
  Visible:=false;
  FCustomBlending.Free;
  if assigned(FParams) and (FMeshType<>mtPoints) then begin
    MeshList:=nil; FOctreeList:=nil; FLodList:=nil;
  end else begin
    FreeVBOList(MeshList,true); MeshList.Free;
    for i:=0 to FOctreeList.Count-1 do begin
      temp:=FOctreeList[i]; Temp.Free;
    end; FOctreeList.Free;
    for i:=0 to FLodList.Count - 1 do begin
      pl:=FLodList[i]; dispose(pl);
    end;FLodList.Free;
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

  inherited;
end;

procedure TVBOMeshObject.FreeObjectsMemory;
var i:integer;
    P:PVBOBuffer;
begin
   for i:=0 to MeshList.Count-1 do begin P:=MeshList[i]; FreeVBOMem(P^);end;
end;

function TVBOMeshObject.getBuffer(index: integer): PVBOBuffer;
begin
  result:=MeshList[index];
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
  end; UpdateWorldMatrix;
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
    ScaleMatrix:=ms; FScale:=Scale;
  end else begin
    FScale:=VectorTransform(Scale,ScaleMatrix);
    ScaleMatrix:=MatrixMultiply(ScaleMatrix,ms);
  end;
 end;
 UpdateWorldMatrix;
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
    i: integer;
    pbb: PExtents;
    buff: PVBOBuffer;
begin
 with Matrices do begin
  wm:=IdentityHmgMatrix;
  if (FParent<>nil) and ((ttParent in UseMatrix) or (ttAll in UseMatrix)) then begin
    if not FParent.WorldMatrixUpdated then Fparent.UpdateWorldMatrix;
    wm:=Fparent.Matrices.WorldMatrix;
    wm:=MatrixMultiply(wm, ModelMatrix);
  end else wm := ModelMatrix;

  if (assigned(ToFollowObject)) and ((ttFollow in UseMatrix) or (ttAll in UseMatrix))
  then wm:=MatrixMultiply(wm, ToFollowObject.AbsoluteMatrix);
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
  FExtents:=TExtents(bb);
  FBaundedRadius:=VectorDistance(FExtents.emin, FExtents.emax) * 0.5;
  FGeomCenter:=VectorScale(VectorAdd(FExtents.emin, FExtents.emax), 0.5);
{
  if MeshList.Count<>FMeshExtents.Count then begin
    for i:=0 to FMeshExtents.Count-1 do
      if assigned(FMeshExtents[i]) then dispose(FMeshExtents[i]);
    FMeshExtents.Clear; FMeshExtents.Count:=MeshList.Count;
    for i:=0 to FMeshExtents.Count-1 do begin
      new(pbb); FMeshExtents[i]:=pbb;
    end;
  end;
  for i:=0 to FMeshExtents.Count-1 do begin
    buff:=MeshList[i]; pbb:=FMeshExtents[i];
    bb.min:=buff.emin; bb.max:=buff.emax;
    pbb^:=TExtents(AABBUpdate(bb,Matrices.WorldMatrix));
  end;
}
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
      result:=true; exit;
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
  end; UpdateWorldMatrix;
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
  end; UpdateWorldMatrix;
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
  end; UpdateWorldMatrix;
 end;
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
  function CompareDistanceFTB(Item1, Item2: Pointer): Integer;
  begin result:=trunc(TPDI(Item1^).d-TPDI(Item2^).d); end;
  function CompareDistanceBTF(Item1, Item2: Pointer): Integer;
  begin result:=trunc(TPDI(Item2^).d-TPDI(Item1^).d); end;
begin
  if not assigned(FProxyMatrixList) then exit;
  ObjList:=FProxyMatrixList; if ObjList.Count<=1 then exit;
  if SortDirection=sdNone then exit;
  Temp:=TList.Create; Temp.Count:=ObjList.Count;
  setlength(pd,ObjList.Count);
  for i:=0 to ObjList.Count-1 do begin
    pm:=ObjList[i]; pd[i].p:=pm[3];
    pd[i].d:=VectorNorm(pd[i].p); pd[i].Obj:=pm;
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
  F := GetFrustum(Matrices.ProjectionMatrix, ViewMatrix);
  for i:=0 to FProxyMatrixList.Count-1 do begin
    pm:=FProxyMatrixList[i];
    if pm<>nil then dispose(pm);
    FProxyMatrixList[i]:=nil;
  end;
  FProxyMatrixList.Clear;
  if Visible then begin
    if (not IsVolumeClipped(Extents, F)) and (not FCulled) then begin
      new(pm); pm^:=mv; FProxyMatrixList.Add(pm);
    end;
  end;
  //Frustum culling
  for i:=0 to FProxyList.Count-1 do begin
    ProxyObj:=FProxyList[i];
    if ProxyObj.Visible and (not ProxyObj.FCulled) then begin
      if (not ProxyObj.WorldMatrixUpdated) then ProxyObj.UpdateWorldMatrix;
      if (not IsVolumeClipped(ProxyObj.Extents, F))
      and(not ProxyObj.FCulled) then begin
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
      if d<mind then begin FProxyMatrixList.Exchange(i,j); mind:=d; end;
    end;
  end;
end;

procedure TVBOMeshObject.RenderObject(const ViewMatrix:TMatrix);
var m: TMatrix;
    mv: TMatrix;
    i,j,tid: integer;
    P: PVBOBuffer;
    PDescr: PMultiRenderDescr;
    ActiveMaterial: string;
    tex: TGLTexture;
    MasterObj, ProxyObj: TVBOMeshObject;
    bindedBuff: pointer;
    bindState: TBindState;
    Rcount: integer;
    singleMat: boolean;
    CMName: string;
    pm: PMatrix;
    F: TFrustum;
begin
  if FMeshType = mtInstance then exit;
  if Assigned(ToFollowObject) then
     if not MatrixEquals(ToFollowObject.AbsoluteMatrix, Matrices.ModelMatrix)
     then UpdateWorldMatrix;
  if assigned(FParent) or Assigned(ToFollowObject) then UpdateWorldMatrix;


  if (not WorldMatrixUpdated) then UpdateWorldMatrix;
  mv:=MatrixMultiply(Matrices.WorldMatrix,ViewMatrix); m:=mv;
  glPushMatrix;

  if FProxyList.Count>0 then RebuildProxyList(ViewMatrix, mv);

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
      SetBlending;
      if assigned(FMaterial) then FMaterial.Apply;
      if assigned(FTexture) then FTexture.Apply;
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
  if FTwoSides then glDisable(GL_CULL_FACE) else glEnable(GL_CULL_FACE);
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
        if assigned(FMaterial) or assigned(FTexture) then begin
          SetBlending;
          if assigned(FMaterial) then FMaterial.Apply;
          if assigned(FTexture) then FTexture.Apply
          else glDisable(GL_TEXTURE_2D);
          singleMat:=true;
        end else begin
         ResetBlending; glDisable(GL_TEXTURE_2D);
         OGLStateEmul.GLStateCache.MaterialCache.Reset;
        end;
      end;
    end else singleMat:=true;
    if singleMat then begin
      if assigned(FonBeforeRender) then FonBeforeRender(self);
      if assigned(FonApplyShader) then FonApplyShader(self);

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
      if assigned(FonUnApplyShader) then FonUnApplyShader(self);
      if assigned(FMaterial) then FMaterial.UnApply;
      if assigned(FTexture) then FTexture.UnApply;
    end else begin
    //MultiMaterial
      if FUseRenderList then begin
         PDescr:=FRenderList[0]; CMName:=PDescr.MaterialName;
      end else begin
         P := MeshList[0]; CMName:=P.MatName;
      end;
      ActiveMaterial:='';
      if CMName<>'' then begin
        SetMaterialName(CMName); ActiveMaterial:=CMName;
        SetBlending;
        if assigned(FMaterial) then FMaterial.Apply;
        if assigned(FTexture) then FTexture.Apply;
      end else begin
        ResetBlending; glDisable(GL_TEXTURE_2D);
        OGLStateEmul.GLStateCache.MaterialCache.Reset;
      end;
      bindedBuff:=nil; i:=0;
      repeat
         //Render+++++++++++++++++
         if assigned(FonBeforeRender) then FonBeforeRender(self);
         if assigned(FonApplyShader) then FonApplyShader(self);
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
         if assigned(FonUnApplyShader) then FonUnApplyShader(self);
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
            if assigned(FMaterial) then FMaterial.UnApply;
            if assigned(FTexture) then FTexture.UnApply;
            FTexture:=nil; FMaterial:=nil; ResetBlending;
         end;
         if (CMName<>ActiveMaterial) then begin
           if CMName<>'' then begin
             SetMaterialName(CMName); ActiveMaterial:=CMName;
             SetBlending;
             if assigned(FMaterial) then FMaterial.Apply;
             if assigned(FTexture) then FTexture.Apply
             else glDisable(GL_TEXTURE_2D);
           end else begin
             FTexture:=nil; FMaterial:=nil; ResetBlending;
             OGLStateEmul.GLStateCache.MaterialCache.Reset;
             glDisable(GL_TEXTURE_2D);
           end;
         end;
      until i=rcount;
      FTexture:=nil; FMaterial:=nil; ResetBlending;
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
     if assigned(FMaterial) then FMaterial.UnApply;
     if assigned(FTexture) then FTexture.UnApply;
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
      if assigned(FMaterial) then FMaterial.UnApply;
      if assigned(FTexture) then FTexture.UnApply;
      ResetBlending;
    end;
    mtGrid,mtBBox: glEnable(GL_LIGHTING);
  end;
  if FFBO.Active then FFBO.UnApply;
  if FProxyList.Count>0 then begin
    ProxyObj:=FProxyList[i]; ProxyObj.FCulled:=false;
  end;
  glPopMatrix;
//CheckOpenGLError;
end;

procedure TVBOMeshObject.SetPosition(const Value: TVector);
begin
  MoveObject(Value);
end;

procedure TVBOMeshObject.SetScale(const Value: TVector);
begin
  ScaleObject(Value);
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
var i,j,k,n,count: integer;
    P: PVBOBuffer;
begin
   if FPolygonsCount<>-1 then begin
     result:=FPolygonsCount; exit; end;
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
   Result:=Count; FPolygonsCount:=Count;
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
    RotationMatrix:=IdentityHmgMatrix;
  end;
  UpdateWorldMatrix;
end;

procedure TVBOMeshObject.SetFaceMode;
begin
  case FFaceMode of
    fmPoints: glPolygonMode(GL_FRONT_AND_BACK, GL_POINT);
    fmLines: glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    fmFill: glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  end;
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

procedure TVBOMeshObject.SetMaterial(const Value: TGLLibMaterial);
var mat: TMaterial;
    tex: TGLTexture;
begin
   if not assigned(Value) then begin
     FMaterial:=nil; FTexture:=nil;
     FSceneMaterial:=nil; exit;
   end;
   mat:=FMaterials.MaterialByName(Value.Name);
   tex:=Ftextures.TextureByName(Value.Name);
   if mat=nil then begin
      mat:=FMaterials.AddSceneMaterial(Value.Material,Value.Name);
      ImportBlending(Value.Material);
   end else ImportBlending(Value.Material);
   if tex=nil then tex:=FTextures.AddSceneTexture(Value.Material,Value.Name,'');
   FMaterial:=mat; FTexture:=tex;
   FSceneMaterial:=Value;
end;

procedure TVBOMeshObject.ImportBlending(Mat: TGLMaterial);
begin
  case ord(Mat.BlendingMode) of
   0: FBlendingMode:=bmOpaque;
   1: FBlendingMode:=bmTransparency;
   2: FBlendingMode:=bmAdditive;
   3: FBlendingMode:=bmAlphaTest50;
   4: FBlendingMode:=bmAlphaTest100;
   5: FBlendingMode:=bmModulate;
   6: FBlendingMode:=bmCustom;
  end;
end;

procedure TVBOMeshObject.SetBlending;
begin
  case FBlendingMode of
    bmOpaque:
      begin
        glDisable(GL_BLEND);
        glDisable(GL_ALPHA_TEST);
      end;
    bmTransparency:
      begin
        glEnable(GL_BLEND); glEnable(GL_ALPHA_TEST);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        glAlphaFunc(GL_GREATER,0);
      end;
    bmAdditive:
      begin
        glEnable(GL_BLEND);
        glEnable(GL_ALPHA_TEST);
        glBlendFunc(GL_SRC_ALPHA,GL_ONE);
        glAlphaFunc(GL_GREATER,0);
      end;
    bmAlphaTest50:
      begin
        glDisable(GL_BLEND);
        glEnable(GL_ALPHA_TEST);
        glAlphaFunc(GL_GEQUAL,0.5);
      end;
    bmAlphaTest100:
      begin
        glDisable(GL_BLEND);
        glEnable(GL_ALPHA_TEST);
        glAlphaFunc(GL_GEQUAL,1);
      end;
    bmModulate:
      begin
        glEnable(GL_BLEND);
        glEnable(GL_ALPHA_TEST);
        glBlendFunc(GL_DST_COLOR,GL_ZERO);
        glAlphaFunc(GL_GREATER,0);
      end;
    bmCustom:
      begin
        FCustomBlending.Apply;
      end;
  end;
end;

procedure TVBOMeshObject.SetMaterialName(const Name: string);
var mat: TMaterial;
    Lm: TGLLibMaterial;
    tex: TGLTexture;
begin
   mat:=nil; tex:=nil;
   if assigned(MeshMaterialLibrary) then begin
      mat:=FMaterials.MaterialByName(Name,MeshMaterialLibrary.Name);
      tex:=Ftextures.TextureByName(name);
   end else begin
      mat:=FMaterials.MaterialByName(Name);
      tex:=Ftextures.TextureByName(name);
   end;
   if (mat=nil) and (assigned(MeshMaterialLibrary)) then begin
      Lm:=MeshMaterialLibrary.LibMaterialByName(Name);
      if assigned(Lm) then begin
         mat:=FMaterials.AddSceneMaterial(Lm.Material,Name,MeshMaterialLibrary.Name);
         ImportBlending(Lm.Material);
         if tex=nil then tex:=FTextures.AddSceneTexture(Lm.Material,Lm.Name,'');
      end else FMaterial:=nil;
   end else begin
      if assigned(MeshMaterialLibrary) then
         Lm:=MeshMaterialLibrary.LibMaterialByName(Name)
      else lm:=nil;
      if assigned(Lm) then ImportBlending(Lm.Material);
   end;
   FMaterial:=mat; FTexture:=tex;
   FSceneMaterialName:=name;
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

procedure TVBOMeshObject.RenderOccluder(const ViewMatrix: TMatrix;
  Occluder: PVBOBuffer);
var m: TMatrix;
    S,P: TVector;
    i: integer;
    buff: PVBOBuffer;
    ext: TExtents;
    bb: TAABB;
begin
    if Assigned(ToFollowObject) then
       if not MatrixEquals(ToFollowObject.AbsoluteMatrix, Matrices.ModelMatrix)
       then UpdateWorldMatrix;

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

procedure TVBOMeshObject.UpdateExtents;
var bb: TAABB;
begin
  bb:=TAABB(FBaseExtents);
  bb:=AABBUpdate(bb,Matrices.WorldMatrix);
  FExtents:=TExtents(bb);
  FBaundedRadius:=VectorDistance(FExtents.emin, FExtents.emax) * 0.5;
  FGeomCenter:=VectorScale(VectorAdd(FExtents.emin, FExtents.emax), 0.5);
end;

procedure TVBOMeshObject.UpdateMaterialList;
var i: integer;
    p: PVBOBuffer;
begin
  for i:=0 to MeshList.Count-1 do begin
    p:=MeshList[i];
    if (p.MatName<>'') and (Materials.IndexOf(p.MatName)<0) then
       Materials.Add(p.MatName);
  end;
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
  FMaterials:=TMaterialLibrary.Create;
  FTextures:=TGLTextureLibrary.Create;
  OctreeBuilded:=false;
  Visible:=true;
  FOcclusionCulling:=false;
  FOccluder:=CreateCubicOccluder;
  FLights:=TLightLibrary.Create;
  FSortDirection:=sdNone;
  FQueryObjectList:=TList.Create;
  FQueryObjectList.Capacity:=100000;
  FSceneOctree:=TSceneOctree.Create;
  FRenderPass:=0;
  FViewMatrix:=IdentityHmgMatrix;
//  OGLStateEmul.GLStateCache.CheckStates;
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
    if assigned(mo) then begin
      if mo.FCulled then mo.FOccluded:=false; mo.FCulled:=False;
      if (mo.Visible) or (mo.FProxyList.Count>0) then begin
        if assigned(mo.onBeforeCheckVisibility) then mo.onBeforeCheckVisibility(self);
        if not ((mo.FMeshType=mtHUDSprite) or (mo.FMeshType=mtSphericalSprite) or
               (mo.FMeshType=mtCylindricalSprite))
        then mo.FCulled:=IsVolumeClipped(mo.FGeomCenter, mo.FBaundedRadius, Frustum);
        if mo.FProxyList.Count>0 then mo.FCulled:=false;
        if (not mo.FCulled) and (mo.FMeshType<>mtInstance) then FQueryObjectList.Add(mo);
      end;
    end;
  end;
  result:=FQueryObjectList;
end;

function TVBOMesh.AddBox(Width,Height, Depth: single; TilesX, TilesY, TilesZ: integer): TVBOMeshObject;
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
  FreeVBOMem(Temp^); Dispose(Temp); 
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
  FreeVBOMem(Temp^); Dispose(Temp);
  Temp:=CreatePlane(Width,Height,TilesX,TilesY,nil,false);
  wm:=CreateRotationMatrixX(-Pi/2);
  Temp.Vertexes.TransformAsPoints(wm);
  Temp.Normals.TransformAsVectors(wm);
  Temp.Vertexes.Translate(affinevectormake(0,0,-Depth/2));
  AttachBuffer(Temp^,Res^);
  wm:=CreateRotationMatrixX(Pi);
  Temp.Vertexes.TransformAsPoints(wm);
  Temp.Normals.TransformAsVectors(wm);
  AttachBuffer(Temp^,Res^); FreeVBOMem(Temp^); Dispose(Temp);

  GenVBOBuff(Res^, False); Res.MatName:='';
  if not assigned(FMeshList) then FMeshList:=TList.Create;
  with mo do begin
    FMeshType:=mtBox; Parent:=nil;
    MeshList.Add(Res); Visible:=true;
    Name:='VBOBox'+inttostr(FMeshList.Count);
    FBaseExtents:=GetExtentsOfList(MeshList);
    MeshMaterialLibrary:=MaterialLibrary;
    UpdateWorldMatrix; UpdateMaterialList;
  end; mo.FIndexInMesh:=FMeshList.Add(mo); Result:=mo;
  mo.FMaterials:=FMaterials; mo.FTextures:=FTextures;  
end;

function TVBOMesh.AddPlane(Width, Height: single; TilesX, TilesY: integer; HeightFunc: TGetHeightFunc=nil): TVBOMeshObject;
var Temp: PVBOBuffer;
    mo: TVBOMeshObject;
begin
FExtentsBuilded:=false;
mo:=TVBOMeshObject.Create;
Temp:=CreatePlane(Width,Height,TilesX,TilesY,HeightFunc);
  if not assigned(FMeshList) then FMeshList:=TList.Create;
  with mo do begin
    FMeshType:=mtPlane; Parent:=nil;
    MeshList.Add(Temp); Visible:=true;
    Name:='VBOPlane'+inttostr(FMeshList.Count);
    FBaseExtents:=GetExtentsOfList(MeshList);
    MeshMaterialLibrary:=MaterialLibrary;
    UpdateWorldMatrix; UpdateMaterialList;
  end; mo.FIndexInMesh:=FMeshList.Add(mo); Result:=mo;
  mo.FMaterials:=FMaterials; mo.FTextures:=FTextures;  
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
    MeshList.Add(Temp); FMeshType:=mtSphere; Visible:=true;
    Name:='VBOSphere'+inttostr(FMeshList.Count); Parent:=nil;
    FBaseExtents:=GetExtentsOfList(MeshList);
    MeshMaterialLibrary:=MaterialLibrary;
    UpdateWorldMatrix; UpdateMaterialList;
  end;result:=mo; mo.FIndexInMesh:=FMeshList.Add(mo);
  mo.FMaterials:=FMaterials; mo.FTextures:=FTextures;  
end;

destructor TVBOMesh.Destroy;
var i:integer;
    mo:TVBOMeshObject;
begin
  for i:=0 to FMeshList.Count-1 do begin
      mo:=FMeshList[i];
      FreeAndNil(mo);
  end;  FMeshList.Free;
  FMaterials.Free; FTextures.Free;
  FreeVBOBuffer(FOccluder^); Dispose(FOccluder);
  FLights.Free; FQueryObjectList.Free;
  FSceneOctree.Free;
  inherited;
end;

procedure TVBOMesh.RenderMeshObject(MeshObject: TVBOMeshObject);
var mvm,proj:TMatrix;
begin
  glGetFloatv(GL_MODELVIEW_MATRIX, @mvm);
  glGetFloatv(GL_PROJECTION_MATRIX, @proj);
  MeshObject.Matrices.ProjectionMatrix:=proj;
  MeshObject.FTime:=GetTime;
  MeshObject.RenderObject(mvm);
end;

Procedure TVBOMesh.DoRender(var ARci: TRenderContextInfo;
                            ARenderSelf, ARenderChildren: Boolean);
var i,n: integer;
    mo,lod,master: TVBOMeshObject;
    F: TFrustum;
    time,t1,t2: Double;
    QueryObjectList: TList;
    queries: array of GLUInt;
    sampleCount: GLUint;
    available: GLUint;

begin
  FPolyCount:=0;
  if (not Visible) or (not assigned(FMeshList)) then exit;

  OGLStateEmul.GLStateCache.CheckStates;
  OGLStateEmul.GLStateCache.PushStates;

  glEnable(GL_LIGHTING); glEnable(GL_LIGHT0); glDisable(GL_TEXTURE_2D);
  for i:=0 to Lights.Count-1 do Lights[i].ApplyLight(i);

  if assigned(onBeforeRender) then onBeforeRender;
  glGetFloatv(GL_MODELVIEW_MATRIX, @FViewMatrix);
  glGetFloatv(GL_PROJECTION_MATRIX, @FProjectionMatrix);
  glGetIntegerv(GL_VIEWPORT, @FViewPort);
  F := GetFrustum(FProjectionMatrix, FViewMatrix);
  time:=GetTime;
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
    if assigned(mo.FParams)
    and ((mo.FMeshType<>mtPoints)and(mo.FMeshType<>mtParticles))
    then master:=mo.FParams else master:=mo;

    mo.FTime:=time; mo.FRCI:=ARCI;
    mo.Matrices.ProjectionMatrix:=FProjectionMatrix;
    mo.Matrices.ViewMatrix:=FViewMatrix;

    if (not FOcclusionCulling) or (mo.PolygonsCount<200) then begin
      if (master.FLodList.Count=0) or (not master.UseLods) then begin
        if mo.FProxyList.Count>0 then mo.SortProxyByDistance(FSortDirection);
        mo.RenderObject(FViewMatrix);
        if (mo.FProxyList.Count>0) then
           FPolyCount:=FPolyCount+mo.PolygonsCount*mo.FProxyMatrixList.Count
        else FPolyCount:=FPolyCount+mo.PolygonsCount;
      end else begin
        lod:=LodSelect(mo);
        if assigned(Lod) then begin
          lod.RenderObject(FViewMatrix);
          FPolyCount:=FPolyCount+lod.PolygonsCount;
        end;
      end;
    end else begin
      glBeginQueryARB(GL_SAMPLES_PASSED_ARB, queries[i]);
      if mo.FOccluded and (not mo.IgnoreOcclusion) then begin
         glDisable(GL_LIGHTING);
         glColorMask(FALSE, FALSE, FALSE, FALSE); glDepthMask(FALSE);
           mo.RenderOccluder(FViewMatrix,FOccluder);
         glColorMask(TRUE, TRUE, TRUE, TRUE); glDepthMask(TRUE);
         glEnable(GL_LIGHTING);
         FPolyCount:=FPolyCount+FOccluder.ElementsCount*mo.MeshList.Count;
      end else begin
        if (master.FLodList.Count=0) or (not master.UseLods) then begin
          mo.RenderObject(FViewMatrix);
          if (mo.FProxyList.Count>0) then
             FPolyCount:=FPolyCount+mo.PolygonsCount*mo.FProxyMatrixList.Count
          else FPolyCount:=FPolyCount+mo.PolygonsCount;
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
  if FOcclusionCulling and (QueryObjectList.Count>0) then begin
//    t1:=GetTime;
    glFinish;
    i:=trunc(QueryObjectList.Count*3/4); n:=0;
    repeat
      glGetQueryObjectivARB(queries[i], GL_QUERY_RESULT_AVAILABLE_ARB, @available);
      inc(n);
    until available>0;
//    t2:=GetTime;
//    assert(false,FloatTostr(t2-t1)+' : '+inttostr(n));
    for i:=0 to QueryObjectList.Count-1 do begin
      glGetQueryObjectuivARB(queries[i], GL_QUERY_RESULT_ARB, @sampleCount);
      mo:=QueryObjectList[i];
      if (sampleCount>0) then mo.FOccluded:=false else mo.FOccluded:=true;
    end;
    glDeleteQueriesARB(QueryObjectList.Count,@queries[0]);
  end;
//  QueryObjectList.Free;
//  inc(FRenderPass); if FRenderPass=2 then FRenderPass:=0;
  if assigned(onAfterRender) then onAfterRender;
  OGLStateEmul.GLStateCache.PopStates;
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
  if assigned(FF.MaterialLibrary) and (FF.MaterialLibrary.TexturePaths='')
  then FF.MaterialLibrary.TexturePaths:=extractfilepath(FileName);
  //FF.UseMeshMaterials:=assigned(MaterialLibrary);
  FF.loadfromfile( FileName );
  Result:=AddMeshFromFreeForm(FF);
  FF.Free;
  if assigned(FF.MaterialLibrary) then
    if MaterialLibrary.TexturePaths=extractfilepath(FileName) then
      MaterialLibrary.TexturePaths:='';

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
       MatName := FG.MaterialName; Name := M.Name;
       if MatName='' then MatName:='Null';
       if MatList.Values[MatName]='' then
          MatList.Values[MatName]:=inttostr(MatList.Count);
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
           ml.Material.Texture.Disabled:=false;
           lm:=MaterialLibrary.Materials.Add;
           lm.Material:=ml.Material; lm.Name:=s;
           //MaterialLibrary.Materials[MaterialLibrary.Materials.Count-1].Name:=s;
          end;
        end;
     end;
  end;
  FBaseExtents:=GetExtentsOfList(MeshList);
  UpdateWorldMatrix;
  end; Result:=mo;  mo.FIndexInMesh:=FMeshList.Add(mo);
  MatList.Clear; MatList.Free;
  mo.FMaterials:=FMaterials; mo.FTextures:=FTextures;
end;


function TVBOMesh.AddScreenQuad(AddToMesh:boolean): TVBOMeshObject;
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
    UpdateWorldMatrix; UpdateMaterialList;
    Pickable:=false;
  end; result:=mo;
  if AddToMesh then mo.FIndexInMesh:=FMeshList.Add(mo)
  else mo.FIndexInMesh:=-1;
  mo.FMaterials:=FMaterials; mo.FTextures:=FTextures;
end;

function TVBOMesh.AddSMDAnimation(SMD: TSkeletalRender): TVBOMeshObject;
var mo:TSkeletalRender;
    i:integer;
begin
  mo:=TSkeletalRender.Create;
  mo.Name:='SMDAnimation'+inttostr(FMeshList.Add(mo));
  result:=mo; mo.FMaterials:=FMaterials; mo.FTextures:=FTextures;
  with mo do begin
    FAnim.Mesh:=SMD.FAnim.Mesh;
    FAnim.Animations:=TList.Create;
    FAnim.Animations.Assign(SMD.FAnim.Animations);
    GetMeshFormSMD(FAnim.Mesh,MeshList,true);
    FBaseExtents:=SMD.FBaseExtents;
    FAnim.TextureId:=SMD.FAnim.TextureId;
    MeshMaterialLibrary:=MaterialLibrary;
    UpdateWorldMatrix;
    //Pack Mesh to Texture
    PackMeshesToTexture(vtex,ntex);
    glDeleteBuffers(1,@FMultiBuffer[0].Buff.vid);
    glDeleteBuffers(1,@FMultiBuffer[0].Buff.nid);
    RotateAroundX(-pi/2); //visible:=false;
    rvtex:=TGLTexture.Create; //Vertex readback Texture
    rvtex.CreateRGBA32FTexture2D(vtex.Width,vtex.Height);
    rntex:=TGLTexture.Create; //Vertex readback Texture
    rntex.CreateRGBA32FTexture2D(vtex.Width,vtex.Height);
//    Visible:=false;

    //Setup FBO
    mo.FScreenQuad:=AddScreenQuad(false);
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
    end;
    FScreenQuad.visible:=true;
    FCreatSMDShader;
  end;
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
    UpdateWorldMatrix; UpdateMaterialList;
    NoZWrite:=false; NoDepthTest:=false;
  end; result:=mo; mo.FIndexInMesh:=FMeshList.Add(mo);
  mo.FMaterials:=FMaterials; mo.FTextures:=FTextures;  
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
    UpdateWorldMatrix; UpdateMaterialList;
  end; result:=mo; mo.FIndexInMesh:=FMeshList.Add(mo);
  mo.FMaterials:=FMaterials; mo.FTextures:=FTextures;
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
              ObjMeshIndexList.Add(i);
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
    UpdateWorldMatrix; UpdateMaterialList;
  end; result:=mo; mo.FIndexInMesh:=FMeshList.Add(mo);
  mo.FMaterials:=FMaterials; mo.FTextures:=FTextures;  
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
    UpdateWorldMatrix; UpdateMaterialList;
    Pickable:=false;
  end; result:=mo; mo.FIndexInMesh:=FMeshList.Add(mo);
  mo.FMaterials:=FMaterials; mo.FTextures:=FTextures;
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
    UpdateWorldMatrix; UpdateMaterialList;
  end; result:=mo; mo.FIndexInMesh:=FMeshList.Add(mo);
  mo.FMaterials:=FMaterials; mo.FTextures:=FTextures;
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
    UpdateWorldMatrix; UpdateMaterialList;
  end; result:=mo; mo.FIndexInMesh:=FMeshList.Add(mo);
  mo.FMaterials:=FMaterials; mo.FTextures:=FTextures;  
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
    UpdateWorldMatrix; UpdateMaterialList;
    IgnoreOcclusion:=true;
  end; result:=mo; mo.FIndexInMesh:=FMeshList.Add(mo);
  mo.FMaterials:=FMaterials; mo.FTextures:=FTextures;
end;

function TVBOMesh.AddInstanceToObject(MasterObject: TVBOMeshObject): TVBOMeshObject;
var mo: TVBOMeshObject;
begin
  mo:=TVBOMeshObject.Create;
  with mo do begin
    FOctreeList.Free; MeshList.Free;  FLodList.Free;
    FMeshType:=mtInstance;
    Visible:=true;
    Name:='VBOProxy'+inttostr(FMeshList.Count);
    FParams:=MasterObject;
    MeshList:=MasterObject.MeshList;
    FOctreeList:=MasterObject.FOctreeList;
    FBaseExtents:=MasterObject.FBaseExtents;
    UpdateWorldMatrix; UpdateMaterialList;
  end;
  MasterObject.FProxyList.Add(mo);
  result:=mo; mo.FIndexInMesh:=FMeshList.Add(mo);
  mo.FMaterials:=FMaterials; mo.FTextures:=FTextures;
end;

function TVBOMesh.AddProxyObject(MasterObject: TVBOMeshObject): TVBOMeshObject;
var mo: TVBOMeshObject;
begin
  mo:=TVBOMeshObject.Create;
  with mo do begin
    FOctreeList.Free; MeshList.Free; FLodList.Free;
    MeshMaterialLibrary:=MasterObject.MeshMaterialLibrary;
//    FMeshType:=mtProxy;
    FMeshType:=MasterObject.FMeshType;
    Visible:=true;
    Name:='VBOProxy'+inttostr(FMeshList.Count);
    FParams:=MasterObject;
    MeshList:=MasterObject.MeshList;
    FOctreeList:=MasterObject.FOctreeList;
    FLodList:=MasterObject.FLodList;
    UseLods:=MasterObject.UseLods;
    FBaseExtents:=MasterObject.FBaseExtents;
    UpdateWorldMatrix; UpdateMaterialList;
  end;
  result:=mo; mo.FIndexInMesh:=FMeshList.Add(mo);
  mo.FMaterials:=FMaterials; mo.FTextures:=FTextures;
end;

function TVBOMesh.AddUserObject(Name: string;
  VBOMeshList: TList): TVBOMeshObject;
var mo: TVBOMeshObject;
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
    UpdateWorldMatrix; UpdateMaterialList;
  end;
  result:=mo; mo.FIndexInMesh:=FMeshList.Add(mo);
  mo.FMaterials:=FMaterials; mo.FTextures:=FTextures;
end;

function TVBOMesh.AddUniformSMD(MeshFile: string;
  const AnimFiles: array of string): TUniformSMDRender;
var mo: TUniformSMDRender;
    i: integer;
    path,s,t: string;
    Tex: TGLTexture;
begin
  assert(MeshFile<>'','Need Mesh');
  mo:=TUniformSMDRender.Create;
  mo.Name:='UniformSMDAnimation'+inttostr(FMeshList.Add(mo));
  result:=mo; mo.FMaterials:=FMaterials; mo.FTextures:=FTextures;
  with mo do begin
    //Load Mesh File
    FAnim.Mesh:=SMDLoad(MeshFile);
    mo.FBones:=FAnim.Mesh.NodesCount;
    FAnimationName:=''; FAnimationNum:=-1; FOldFrame:=-1;
    //Load Animation files
    for i:=0 to high(AnimFiles) do AddAnimation(FAnim,AnimFiles[i]);
    //Create Mesh from SMD
    GetMeshFormSMD(FAnim.Mesh,MeshList,true);
    FBaseExtents:=GetExtentsOfList(MeshList);
    //Load Materials
    MeshMaterialLibrary:=MaterialLibrary;
    UpdateWorldMatrix;
    path:=ExtractFilePath(MeshFile);
    if path<>'' then if path[length(path)]<>'\' then path:=path+'\';
    for i:=0 to mo.FAnim.Mesh.Mesh.Textures.Count-1 do begin
       s:=mo.FAnim.Mesh.Mesh.Textures[i];
       t:=s; delete(t,length(t)-4,4);
       Tex:=TGLTexture.CreateFromFile(path+s);
       Tex.Name:=t+'_'; FTextures.Add(Tex);
    end;
    RotateAroundX(-pi/2);
    //Disable Frame interpolation
    Smoothed:=false; FBlended:=false;
    FOldFrameNum:=-1; FramePosition:=0;

    onBeforeRender:=FApplyShader;
    onAfterRender:=FUnApplyShader;
    //Create Shader
    FCreatSMDShader;
  end;
end;

function TVBOMesh.AddUniformSMD(SMD: TUniformSMDRender): TUniformSMDRender;
var mo: TUniformSMDRender;
    i: integer;
    path,s,t: string;
begin
 { TODO : Сделать прокси, ссылаться на геометрию и анимацию }
(*  mo:=TUniformSMDRender.Create;
  mo.Name:='SMDAnimation'+inttostr(FMeshList.Add(mo));
  result:=mo; mo.FMaterials:=FMaterials; mo.FTextures:=FTextures;
  with mo do begin
    FAnim.Mesh:=SMD.FAnim.Mesh;
    FAnim.Animations:=TList.Create;
    FAnim.Animations.Assign(SMD.FAnim.Animations);
    mo.FBones:=FAnim.Mesh.NodesCount;
    GetMeshFormSMD(FAnim.Mesh,MeshList,true);
    FBaseExtents:=SMD.FBaseExtents;
    FAnim.TextureId:=SMD.FAnim.TextureId;
    MeshMaterialLibrary:=MaterialLibrary;
    UpdateWorldMatrix;
    path:=ExtractFilePath(MeshFile);
    if path<>'' then if path[length(path)]<>'\' then path:=path+'\';
    for i:=0 to mo.FAnim.Mesh.Mesh.Textures.Count-1 do begin
       s:=mo.FAnim.Mesh.Mesh.Textures[i];
       t:=s; delete(t,length(t)-4,4);
       Tex:=TGLTexture.CreateFromFile(path+s);
       Tex.Name:=t+'_'; FTextures.Add(Tex);
    end;
    RotateAroundX(-pi/2);
    //Disable Frame interpolation
    Smoothed:=false; FBlended:=false;
    FOldFrameNum:=-1; FramePosition:=0;
    onBeforeRender:=FApplyShader;
    onAfterRender:=FUnApplyShader;
    //Create Shader
    FCreatSMDShader;
  end;
*)
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
    UpdateWorldMatrix; UpdateMaterialList;
  end;
  result:=mo; mo.FIndexInMesh:=FMeshList.Add(mo);
  mo.FMaterials:=FMaterials; mo.FTextures:=FTextures;
end;

procedure TVBOMesh.Clear(FreeObjects: boolean);
var i: integer;
    mo: TVBOMeshObject;
begin
  if not FreeObjects then FMeshList.Clear else begin
    for i:=0 to FMeshList.Count-1 do begin
      mo:=FMeshList[i]; FreeAndNil(mo);
    end; FMeshList.Clear;
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
var dmin: single;
    i,j: integer;
    curr: TVBOMeshObject;
    vm: TMatrix;
    pd: array of TPDI;
    t: TPDI;
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
  vm:=FViewMatrix; setlength(pd,ObjList.Count);
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
end;

procedure TVBOMesh.DeleteMeshObject(index: integer; FreeObject: boolean);
var MeshObject: TVBOMeshObject;
begin
   MeshObject:=FMeshList[index];
   if FreeObject then FreeAndNil(MeshObject);
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
    UpdateWorldMatrix; UpdateMaterialList;
  end;
  result:=mo; mo.FIndexInMesh:=FMeshList.Add(mo);
  mo.FMaterials:=FMaterials; mo.FTextures:=FTextures;
end;

function TVBOMesh.AddMeshObject(mo: TVBOMeshObject): integer;
begin
  result:=FMeshList.Add(mo);
  mo.UpdateWorldMatrix; mo.UpdateMaterialList;
  mo.FMaterials:=FMaterials; mo.FTextures:=FTextures;
end;

function TVBOMesh.AddSMDAnimation(MeshFile: string;
  const AnimFiles: array of string): TVBOMeshObject;
var mo: TSkeletalRender;
    i: integer;
    path,s,t: string;
    Tex: TGLTexture;
begin
  assert(MeshFile<>'','Need Mesh');
  mo:=TSkeletalRender.Create;
  mo.Name:='SMDAnimation'+inttostr(FMeshList.Add(mo));
  result:=mo; mo.FMaterials:=FMaterials; mo.FTextures:=FTextures;
  with mo do begin
    //Load Mesh File
    FAnim.Mesh:=SMDLoad(MeshFile);
    FAnimationName:=''; FAnimationNum:=-1; FOldFrame:=-1;
    //Load Animation files
    for i:=0 to high(AnimFiles) do AddAnimation(FAnim,AnimFiles[i]);
    //Create Mesh from SMD
    GetMeshFormSMD(FAnim.Mesh,MeshList,true);
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
       Tex:=TGLTexture.CreateFromFile(path+s);
       Tex.Name:=t+'_'; FTextures.Add(Tex);
//       MaterialLibrary.AddTextureMaterial(t,path+s).Material.Texture.Enabled:=true;
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
//    Visible:=false;

    //Setup FBO
    mo.FScreenQuad:=AddScreenQuad(false);
//TODO: Разобраться почему прямой рендеринг скринквада
//оказывается медленее рендеринга квада как объекта
//    FMeshList.Delete(FScreenQuad.FIndexInMesh);
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
    end;
    FScreenQuad.visible:=true;
    //Create Shader
    FCreatSMDShader;
  end;
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
  result:=mo; if mo.FLodList.Count=0 then exit;
  m:=MatrixMultiply(mo.Matrices.WorldMatrix, FViewMatrix);
  dist:=VectorLength(m[3]); i:=0;
  repeat pl:=mo.FLodList[i];i:=i+1;
  until (dist<=pl.MaxViewDistance) or (i=mo.FLodList.Count);
  if (dist>pl.MaxViewDistance) then pl:=mo.FLodList[mo.FLodList.Count-1];
  if assigned(pl.LodMesh) then begin
    pl.LodMesh.Matrices.WorldMatrix:=mo.Matrices.WorldMatrix;
    pl.LodMesh.WorldMatrixUpdated:=true;
  end; result:=pl.LodMesh;
end;

procedure TVBOMesh.UpdateMaterialCache;
var i,j: integer;
    mo: TVBOMeshObject;
    mat: TGLLibMaterial;
begin
  for i:=0 to FMeshList.count-1 do begin
    mo:=FMeshList[i]; mat:=nil;
    if assigned(mo.FSceneMaterial) or (mo.FSceneMaterialName<>'') then begin
      mo.MaterialExt:=nil; mo.TextureExt:=nil;
      if assigned(mo.FSceneMaterial) then mo.SetMaterial(mo.FSceneMaterial)
      else mo.SetMaterialName(mo.FSceneMaterialName);
    end;
{    if assigned(mo.Material) then mat:=mo.Material else
      if mo.MaterialName<>'' then mat:=MaterialLibrary.LibMaterialByName(mo.MaterialName);
    if assigned(mat) then begin
      Materials.AddSceneMaterial(mat.Material, mat.Name);
      Textures.AddSceneTexture(mat.Material,mat.Name,'');
      mo.SetMaterial(mat);
      mo.ImportBlending(mat.Material);
    end else if mo.Materials.Count>0 then begin
      for j:=0 to mo.Materials.Count-1 do begin
        mat:=MaterialLibrary.LibMaterialByName(mo.Materials[j]);
        if assigned(mat) then begin
          Materials.AddSceneMaterial(mat.Material, mat.Name);
          Textures.AddSceneTexture(mat.Material,mat.Name,'');
          mo.SetMaterial(mat);
          mo.ImportBlending(mat.Material); mat:=nil;
        end;
      end;
    end;}
  end;
end;

procedure TVBOMesh.UpdateSceneMatrix;
begin
   glGetFloatv(GL_MODELVIEW_MATRIX, @FViewMatrix);
   glGetFloatv(GL_PROJECTION_MATRIX, @FProjectionMatrix);
   glGetIntegerv(GL_VIEWPORT, @FViewPort);
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
  UpdateExtents;
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
  UpdateExtents; inc(FRealCount);
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
       UpdateVBOBuff(Buff.vId,FSortedPositions.List,offs,size,true);
    end;
    if FPointParam.UseColors and (upColor in UpdBuff) then begin
       offs:=Index*sizeof(TVector);
       size:=Count*sizeof(TVector);
       UpdateVBOBuff(Buff.cId,FSortedColors.List,offs,size,true);
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
       UpdateVBOBuff(Buff.vId,FPositions.List,offs,size,true);
    end;
    if FPointParam.UseColors and (upColor in UpdBuff) then begin
       offs:=Index*sizeof(TVector);
       size:=Count*sizeof(TVector);
       UpdateVBOBuff(Buff.cId,FColors.List,offs,size,true);
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

destructor TSkeletalRender.Destroy;
begin
  FScreenQuad.Free; Shaders.Free;
  FreeList(FAnim.Animations);
  glDeleteTextures(1,@FAnim.TextureId);
  inherited;
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
  Frame[0]:=trunc(FramePosition)*2; Frame[1]:=Frame[0]+1;
  Frame[2]:=trunc(FramePosition+1)*2;
  Frame[3]:=FAnim.Mesh.NodesCount*FAnimationNum;
//  if Frame[2]>=FFramesCount then Frame[2]:=0;
  Shaders.UseProgramObject(spid);
  Shaders.SetUniforms(spid,'frame',Frame);
  Shaders.SetUniforms(spid,'alpha',pos);
  //Visible:=false;
end;

procedure TSkeletalRender.FCreatSMDShader;
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
'  float y = v.w+frame.w;'+#13+#10+
'  vec4 qp = texture2DRect(BoneTexture, vec2(frame.x,y));'+#13+#10+
'  vec4 qo = texture2DRect(BoneTexture, vec2(frame.y,y));'+#13+#10+
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

constructor TCustomBlending.Create;
begin
  inherited;
end;

destructor TCustomBlending.Destroy;
begin
  inherited;
end;

{ TVBOTerrain }

procedure TVBOTerrain.CreateAndFillTriBuff(TBuff: PVBOBuffer; Rect: TRect);
var i, j, ox, oy, offs: integer;
  LW: integer;
  h: Single;
  v, v1, v2, v3, nm: TAffineVector;
begin
  InitVBOBuff(TBuff^, GL_TRIANGLES, DrawElements);
  with TBuff^ do begin
    LW := Rect.Right - Rect.Left + 1;
    for i := Rect.Top to Rect.Bottom do begin
      oy := i - Rect.Top;
      for j := Rect.Left to Rect.Right do begin
        H := FHMap[i, j];
        SetVector(v, j-FHMWidth/2, H, i-FHMHeight/2);
        Vertexes.Add(v);
        if uTexCoords in FCreateBuff then begin
          v[0] := j / (FHMWidth - 1);
          v[1] := 1 - i / (FHMHeight - 1);
          v[2] := (H+1)/2.0;
          TexCoords.Add(v);
        end;
{        if uNormals in FCreateBuff then begin
           c:=0; nm:=NullVector;
           for a:=-1 to 1 do for b:=-1 to 1 do begin
               if  (j+a>=0) and (j+a<FHMWidth)
               and (i+b>=0) and (i+b<FHMHeight)
               then begin
                 nm:=VectorAdd(nm,GetNormalFromHField(j+a,i+b));inc(c);
               end;
           end;
           NormalizeVector(nm); Normals.Add(nm);
//           nm:=(VectorScale(nm,1/c)); NormalizeVector(nm);
//           Normals.Add(nm);
           //Normals.Add(VectorScale(nm,1/c));
        end;
//           Normals.Add(GetNormalInPoint(j,i));
}
        if (i < Rect.Bottom) and (j < Rect.Right) then begin
          ox := j - Rect.Left; offs := oy * LW + ox;
          Indices.Add(offs, offs + LW, offs + 1);
          Indices.Add(offs + LW, offs + LW + 1, offs + 1);
        end;
      end;
    end;
    if uNormals in FCreateBuff then begin
      Normals.Count := Vertexes.Count;
      for i := 0 to Normals.Count-1 do Normals[i]:=NullVector;
      for i := 0 to (Indices.Count div 3) - 1 do begin
        v1 := Vertexes[Indices[i * 3]]; ScaleVector(v1,affinevectormake(1,128,1));
        v2 := Vertexes[Indices[i * 3 + 1]]; ScaleVector(v2,affinevectormake(1,128,1));
        v3 := Vertexes[Indices[i * 3 + 2]]; ScaleVector(v3,affinevectormake(1,128,1));
        nm := CalcPlaneNormal(v1, v2, v3);
        Normals.TranslateItem(Indices[i * 3], nm);
//        Normals[Indices[i * 3]] := nm;
        nm := CalcPlaneNormal(v2, v3, v1);
        Normals.TranslateItem(Indices[i * 3+1], nm);
//        Normals[Indices[i * 3 + 1]] := nm;
        nm := CalcPlaneNormal(v3, v1, v2);
        Normals.TranslateItem(Indices[i * 3+2], nm);
//        Normals[Indices[i * 3 + 2]] := nm;
      end;
    end;

    Vertexes.GetExtents(emin, emax);
  end;
  TBuff^.RenderBuffs := FCreateBuff + [uIndices];
  GenVBOBuff(TBuff^, false);
end;

//Создаем и заполняем буффер для индексированных Triangle_Strip

procedure TVBOTerrain.CreateAndFillTriStripBuff(TBuff: PVBOBuffer; Rect: TRect);
var i, j, ox, oy, offs: integer;
  LW: integer;
  h: Single;
  v, v1, v2, v3, nm: TAffineVector;
begin
  InitVBOBuff(TBuff^, GL_TRIANGLE_STRIP, DrawElements);
  with TBuff^ do begin
    LW := Rect.Right - Rect.Left + 1;
    for i := Rect.Top to Rect.Bottom do begin
      oy := i - Rect.Top;
      for j := Rect.Left to Rect.Right do begin
        H := FHMap[i, j];
        SetVector(v, j-FHMWidth/2, H, i-FHMHeight/2);
        Vertexes.Add(v);
        if uTexCoords in FCreateBuff then begin
          v[0] := j / (FHMWidth - 1);
          v[1] := 1 - i / (FHMHeight - 1);
          v[2] := H*2+1; TexCoords.Add(v);
        end;
        if (i < Rect.Bottom) then begin
          ox := j - Rect.Left; offs := oy * LW + ox;
          Indices.Add(offs, offs + LW);
        end;
      end;
      j := Indices[Indices.Count - 1]; offs := (oy + 1) * LW;
      Indices.Add(j, offs);
    end;
    Indices.Count:=Indices.Count-2;
    if uNormals in FCreateBuff then begin
      Normals.Count := Vertexes.Count;
      for i := 0 to Normals.Count-1 do Normals[i]:=NullVector; 
      for i := 0 to Indices.Count - 3 do begin
        v1 := Vertexes[Indices[i]];
        v2 := Vertexes[Indices[i + 1]];
        v3 := Vertexes[Indices[i + 2]];
        nm := CalcPlaneNormal(v1, v2, v3);
//        Normals[Indices[i]] := nm;
        Normals.TranslateItem(Indices[i], nm);
        nm := CalcPlaneNormal(v2, v3, v1);
//        Normals[Indices[i + 1]] := nm;
        Normals.TranslateItem(Indices[i+1], nm);
        nm := CalcPlaneNormal(v3, v1, v2);
//        Normals[Indices[i + 2]] := nm;
        Normals.TranslateItem(Indices[i+2], nm);
      end;
    end;
    Vertexes.GetExtents(emin, emax);
  end;
  TBuff^.RenderBuffs := FCreateBuff + [uIndices];
  GenVBOBuff(TBuff^, false);
end;

procedure TVBOTerrain.BuildTerrain(Width, Height: integer; GetHeightFunc: TGetHeightFunc);
var wCount, hCount: integer;
    i,j: integer;
    R: TRect;
    Patch: PVBOBuffer;
    MaxInd: integer;
    ps: integer;
begin
  FreeVBOList(MeshList);
  FHMHeight:=Height;
  FHMWidth:=Width;
  GetHeights(Width, Height, GetHeightFunc);
  CreateSolidBuff;
exit;
  MaxInd:=GetMaxIndicesCount;
  if MaxInd<FHMWidth*FHMHeight*2 then begin
     ps:=trunc(sqrt(MaxInd));
     if FHMHeight div ps<10 then FPatchHeight:= FHMHeight div 10
     else FPatchHeight:=ps;
     if FHMWidth div ps<10 then FPatchWidth:= FPatchWidth div 10
     else FPatchWidth:=ps;
  end else begin
     FPatchHeight:=FHMHeight div 20;
     FPatchWidth:=FHMWidth div 20;
  end;

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
        if R.Right >= FHMWidth then R.Right := FHMWidth - 1;
        if R.Bottom >= FHMHeight then R.Bottom := FHMHeight - 1;
        new(Patch);
        case FaceMode of
          fmTriangles: CreateAndFillTriBuff(Patch, R);
          fmTriangleStrips: CreateAndFillTriStripBuff(Patch, R);
        end;
        Patch.UseTwoTexturesCoord:=false;
        MeshList.Add(Patch);
    end;
    FBaseExtents:=GetExtentsOfList(MeshList);
end;

constructor TVBOTerrain.Create;
begin
  inherited;
  FCreateBuff:=[uNormals, uTexCoords, uIndices];
  FaceMode:=fmTriangles;
  FExtList:=TList.Create;
  FOffsList:=TIntegerList.Create;
  FCountList:=TIntegerList.Create;
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
var i,j,offs:integer;
    n: TAffineVector;
    Buff: PVBOBuffer;
begin
    offs:=0; Buff:=MeshList[0];
    for i:=0 to FHMHeight-1 do for j:=0 to FHMWidth-1 do begin
        n:=GetNormalFromHField(j,i);
        Buff^.Normals[offs]:=n; inc(offs);
    end; //Buff.Normals.Normalize;
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
    mv:=MatrixMultiply(Matrices.WorldMatrix, ViewMatrix);
    glLoadMatrixf(PGLFloat(@mv));
    if assigned(FonBeforeRender) then FonBeforeRender(self);
       if assigned(FTexture) then FTexture.Apply;
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
       if assigned(FTexture) then FTexture.UnApply;
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
    mx := (x - FPosition[0]) / FScale[0];
    my := (y - FPosition[2]) / FScale[2];
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
       result:=iPoint[1]*FScale[1]+FPosition[1]; exit;
    end;
    if RayCastTriangleIntersect(sVector, vDir, p2,p4,p3, @iPoint)
    then begin
       result:=iPoint[1]*FScale[1]+FPosition[1]; exit;
    end;
    L1 := h1 * (1 - (fx)) + h2 * (fx); L2 := h3 * (1 - (fx)) + h4 * (fx);
    Hr := (L1 * (1 - (fy)) + L2 * (fy)) * FScale[1];
    result := Hr + FPosition[1];
end;

function TVBOTerrain.GetNormalInPoint(X, Y: single): TAffineVector;
var h1, h2, h3, h4: single;
  p1, p2, p3, p4, Scale: TAffineVector;
  tx, ty: integer;
  rx,ry:single;
  L1, L2: single;
  mh:single;
  sVector,vDir,iPoint,iNormal:TVector;
begin
  Scale:=AffineVectormake(FScale);
  SetVector(Result, 0, 0, 0);
    rx := (x - FPosition[0]) / FScale[0];
    ry := (y - FPosition[2]) / FScale[2];
    tx:=trunc(rx);ty:=trunc(ry);
  if (tx >= FHMWidth) or (ty >= FHMHeight)
    or (tx + 1 >= FHMWidth) or (ty + 1 >= FHMHeight)
    or (tx < 0) or (ty < 0) then exit;
  h1 := FHMap[ty, tx];
  if (tx < FHMWidth - 1) then h2 := FHMap[ty, tx + 1] else h2 := h1;
  if (ty < FHMHeight - 1) then h3 := FHMap[ty + 1, tx] else h3 := h1;
  if (tx < FHMWidth - 1) and (ty < FHMHeight - 1) then h4 := FHMap[ty + 1, tx + 1] else h4 := h1;

  setvector(p1, tx, h1, ty); ScaleVector(p1, Scale);
  setvector(p2, tx + 1, h2, ty); ScaleVector(p2, Scale);
  setvector(p3, tx, h3, ty + 1); ScaleVector(p3, Scale);
  setvector(p4, tx + 1, h4, ty + 1); ScaleVector(p4, Scale);
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

procedure TVBOTerrain.CreateSolidBuff;
var Buff: PVBOBuffer;
    i,j,a,b,offs :integer;
    v,t,n,v2,v3: TAffineVector;
    dh,dw: integer;
    MaxInd,ps: integer;
    wCount,hCount,c: integer;
    R: TRect;
    Ext: PExtents;
begin
    new(Buff); InitVBOBuff(Buff^,GL_TRIANGLES,DrawElements);
    dh:=FHMHeight div 2; dw:=FHMWidth div 2;
    Buff.Vertexes.Count:=FHMHeight*(FHMWidth);
    Buff.Normals.Count:=FHMHeight*(FHMWidth);
    Buff.TexCoords.Count:=FHMHeight*(FHMWidth);
    offs:=0;
    for i:=0 to FHMHeight-1 do for j:=0 to FHMWidth-1 do begin
        v[0]:=j-dw; v[1]:=FHMap[i, j]; v[2]:=i-dh;
        t[0]:=(j)/FHMWidth; t[1]:=(i)/FHMHeight; t[2]:=v[1];
        n:=GetNormalFromHField(j,i);
        with Buff^ do begin
          Vertexes[offs]:=v;
          TexCoords[offs]:=t;
          Normals[offs]:=n;
          inc(offs);
//          Normals.Add(NullVector);
        end;
    end;
  MaxInd:=GetMaxIndicesCount;
  if MaxInd<FHMWidth*FHMHeight*2 then begin
     ps:=trunc(sqrt(MaxInd));
     if FHMHeight div ps<10 then FPatchHeight:= FHMHeight div 10
     else FPatchHeight:=ps;
     if FHMWidth div ps<10 then FPatchWidth:= FPatchWidth div 10
     else FPatchWidth:=ps;
  end else begin
     FPatchHeight:=FHMHeight div 20;
     FPatchWidth:=FHMWidth div 20;
  end;
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
//        offs:=a*(FHMWidth+1)+b;
//        Buff.Indices.Add(offs,offs+FHMWidth+1,offs+1);
//        Buff.Indices.Add(offs+FHMWidth+1,offs+FHMWidth+2, offs+1);
        offs:=a*(FHMWidth)+b;
        Buff.Indices.Add(offs,offs+FHMWidth,offs+1);
        Buff.Indices.Add(offs+FHMWidth,offs+FHMWidth+1, offs+1);
        c:=c+6;
      end;
      FCountList.Add(c);
  end;
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
{         if j<Buff.Indices.Count-2 then begin
            v[1]:=v[1]*128;
            v2:=Buff.Vertexes[Buff.Indices[j+1]]; v2[1]:=v2[1]*128;
            v3:=Buff.Vertexes[Buff.Indices[j+2]]; v3[1]:=v3[1]*128;
            n:=CalcPlaneNormal(v,v2,v3);
            Buff.Normals.TranslateItem(Buff.Indices[j],n);
         end;
}
      end; FExtList.Add(ext);
  end;
  Buff.Normals.Normalize;
  Buff.UseTwoTexturesCoord:=false;
  Buff.RenderBuffs:=[uNormals,uTexCoords,uIndices];
  MeshList.Clear; MeshList.Add(Buff);
  GenVBOBuff(buff^,false);
  FBaseExtents:=GetExtentsOfList(MeshList);
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

{ TParticleEmitter }

constructor TParticleEmitter.Create(Particles: TVBOParticles);
begin
  inherited Create;
  Gravity:=AffineVectorMake(0,-9.8,0);
  DirectionRange:=AffineVectorMake(0,0,0);
  LifetimeRange:=0;
  SpeedRange:=AffineVectorMake(0,0,0);
  AccelerationRange:=AffineVectorMake(0,0,0);
  fParticles:=Particles;
  fPFX:=TPFXManager.Create(fParticles.FPositions,nil,nil,nil);
end;

destructor TParticleEmitter.Destroy;
begin
  fPFX.Free;
  inherited;
end;

function TParticleEmitter.ParticleLifeTime(var PLife: TParticleLifeCycle;
  CheckTime: TCheckTime): TCheckTimeRes;
var p,n,v,c1,c2:TVector;
    d:double;
begin
{
 result:=trContinue;
 //Эта процедура будет вызываться дважды - до и после расчета координат
 //На первом проходе мы вычисляем возможную точку коллизии,
 //на втором - если коллизия произошла - запускаем жзненный цикл частицы
 with PLife do begin
   case CheckTime of
      ctBefore: begin
        if not Started then begin
          if CheckBox4.Checked and (Dir[1]<>0)then begin
            if not RayCasted or (RayCastFreq=rcEveryUpdate) then begin
             RayIntersected:=Meshes[1].OctreeRayCastIntersect(vectormake(Pos),
             vectormake(Dir),@p,@n);
             if RayIntersected then begin
                iPosition:=p; iNormal:=n;
             end;
             RayCasted:=true;
            end;
          end;
        end;
      end;

      ctAfter: begin
        if not started and (RayCasted and RayIntersected) then begin
         if (Pos[1]<iPosition[1]) then begin
           Particles.Positions[PIndex]:=iPosition;
           Particles.Velocity[PIndex]:=vectormake(0,0,0,0);
           Particles.Colors[PIndex]:=vectormake(1,0,0,1);
           Started:=true; Iteration:=0; LifeTime:=1; TimeLeft:=0;
           result:=trBreak;
         end;
        end;
        if (Pos[1]<-20) and (not RayIntersected) then begin
           Started:=true; TimeLeft:=LifeTime;
        end;
        if Started then begin
           TimeLeft:=TimeLeft+dTime;
           if TimeLeft>LifeTime then begin
              with Particles do begin
                setvector(v,random(90)-30,20,random(70)-30);
                Positions[PIndex]:=v;
                if CheckBox5.Checked then
                     Colors[PIndex]:=vectormake(1,1,1,0)
                else Colors[PIndex]:=vectormake(1,1,1,1);
                Velocity[PIndex]:=vectormake((random(10)-5)/1000,-(random(10))/1000-0.0001,(random(10)-5)/1000);
//                Velocity[PIndex]:=vectormake(0,-0.001*(1+random(10)),0);
              end;
              Started:=false; RayCasted:=false;
              RayIntersected:=false;
              RayCastFreq:=rcEveryUpdate;
              result:=trBreak;
           end else begin
              d:=TimeLeft/LifeTime;
              c1:=vectormake(1,0,0,1);
              c2:=vectormake(1,1,1,0);
              Particles.Colors[PIndex]:=vectorlerp(c1,c2,d);
              result:=trBreak;
           end;
        end;
       //Уменьшение прозрачности по мере удаления от камеры
        if CheckBox5.Checked then begin
          d:=1-VectorDistance(Pos,GLCamera1.Position.AsAffineVector)/50;
          if d<0 then d:=0.1;
          c1:=Particles.Colors[PIndex];
          if c1[3]<>d then begin
            c1[3]:=3*d; Particles.Colors[PIndex]:=c1;
          end;
        end;
      end;
   end;
 end;
}
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
    VPos: integer;
begin
    fVolumeWidth:=FLineWidth;
    fHalfVolumeWidth := fVolumeWidth * 0.5;
    v1:=FNodes[I1]; v2:=FNodes[I2];
    fFromPos_x:=v1[0]; fFromPos_y:=v1[1]; fFromPos_z:=v1[2];
    fToPos_x:=v2[0]; fToPos_y:= v2[1]; fToPos_z:=v2[2];
    VPos:=FVBOBuff.ElementsCount; FUpdated:=true;
{  with FVBOBuff^ do begin
    Vertexes[VPos]:=affineVectorMake(-0.2,-0.1, 2); inc(VPos);
    Vertexes[VPos]:=affineVectorMake(-0.2, 0.1, 2); inc(VPos);
    Vertexes[VPos]:=affineVectorMake( 0.2, 0.1, 2); inc(VPos);
    Vertexes[VPos]:=affineVectorMake( 0.2,-0.1, 2); inc(VPos);
    ElementsCount:=VPos;
  end;
}

    with FVBOBuff^ do begin
      tc1:=ExTexCoords[0]; tc2:=ExTexCoords[1]; tc3:=ExTexCoords[2];
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
    end;{
    with FVBOBuff^ do begin
      tc1:=ExTexCoords[0]; tc2:=ExTexCoords[1]; tc3:=ExTexCoords[2];
      tc1[VPos]:=AffineVectorMake(0.0, 0.0, fVolumeWidth );                     // tu, tv, width
      tc2[VPos]:=AffineVectorMake(-fVolumeWidth, fHalfVolumeWidth, 0 );         // width tweaks
      tc3[VPos]:=AffineVectorMake(fToPos_x, fToPos_y, fToPos_z );               // end position
      Vertexes[VPos]:=AffineVectorMake( fFromPos_x, fFromPos_y, fFromPos_z );   // vertex 0 of quad
      inc(VPos);
      tc1[VPos]:=AffineVectorMake( 0.0, 0.25, fVolumeWidth );            // tu, tv, width
      tc2[VPos]:=AffineVectorMake( fVolumeWidth, fHalfVolumeWidth, 0 );  // width tweaks
      tc3[VPos]:=AffineVectorMake( fToPos_x, fToPos_y, fToPos_z );       // end position
      Vertexes[VPos]:=AffineVectorMake( fFromPos_x, fFromPos_y, fFromPos_z );// vertex 3 of quad
      inc(VPos);
      tc1[VPos]:=AffineVectorMake( 0.25, 0.25, fVolumeWidth );           // tu, tv, width
      tc2[VPos]:=AffineVectorMake( -fVolumeWidth, fHalfVolumeWidth, 0 ); // width tweaks
      tc3[VPos]:=AffineVectorMake( fFromPos_x, fFromPos_y, fFromPos_z ); // end position
      Vertexes[VPos]:=AffineVectorMake( fToPos_x, fToPos_y, fToPos_z );  // vertex 2 of quad
      inc(VPos);
      tc1[VPos]:=AffineVectorMake( 0.25, 0.0, fVolumeWidth );            // tu, tv, width
      tc2[VPos]:=AffineVectorMake( fVolumeWidth, fHalfVolumeWidth, 0 );  // width tweaks
      tc3[VPos]:=AffineVectorMake( fFromPos_x, fFromPos_y, fFromPos_z ); // end position
      Vertexes[VPos]:=AffineVectorMake( fToPos_x, fToPos_y, fToPos_z );  // vertex 1 of quad
      inc(VPos);
      ElementsCount:=VPos;
    end;}

end;

function TVolumetricLines.AddNode(x, y, z: single): integer;
begin
  result:=AddNode(affinevectormake(x,y,z));
end;

constructor TVolumetricLines.Create(MaxNodes: integer = 1000);
var TC1,TC2,TC3: TAffineVectorList;
begin
  inherited Create;
  FNodes:=TAffineVectorList.Create;
  FNodes.Count:=MaxNodes;
  FMaxCapacity:=MaxNodes;
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
    TC1.Count:=MaxNodes*4;
    TC2.Count:=MaxNodes*4;
    TC3.Count:=MaxNodes*4;
    ExTexCoords.Add(TC1);
    ExTexCoords.Add(TC2);
    ExTexCoords.Add(TC3);
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
  inherited;
end;

function TVolumetricLines.GetNode(Index: Integer): TAffineVector;
begin
   if Index<FNodes.Count then result:=FNodes[Index]
   else assert(false,'Index out of Bound!');
end;

procedure TVolumetricLines.RenderObject(const ViewMatrix: TMatrix);
var TC: TAffineVectorList;
    tcId: integer;
    i: integer;
begin
  if FUpdated then with FVBOBuff^ do begin
     for i:=0 to FCount-2 do AddLine(i,i+1);
     UpdateVBOBuff(vId,Vertexes.List,0,(FCount-1)*48);
     TC:=ExTexCoords[0]; tcId:=ExTexCoordsId[0];
     UpdateVBOBuff(tcId,TC.List,0,(FCount-1)*48);
     TC:=ExTexCoords[1]; tcId:=ExTexCoordsId[1];
     UpdateVBOBuff(tcId,TC.List,0,(FCount-1)*48);
     TC:=ExTexCoords[2]; tcId:=ExTexCoordsId[2];
     UpdateVBOBuff(tcId,TC.List,0,(FCount-1)*48);
     FVBOBuff.ElementsCount:=(FCount-1)*4;
     FVBOBuff.MaxElements:=FVBOBuff.ElementsCount;
     FVBOBuff.VertexCount:=FVBOBuff.ElementsCount;
     FUpdated:=false;
  end;
  glDisable(GL_CULL_FACE);
  glDepthMask( FALSE );
  glEnable( GL_DEPTH_TEST );

  glPushMatrix;
//  glLoadMatrixf(PGLFloat(@ViewMatrix));
  FShaders.UseProgramObject(FSId);
  FShaders.SetUniforms(FSId,'volumeLineTexure',0);
  if FTexture<>nil then FTexture.Apply(0);
  SetBlending;
  RenderVBOBuffer(FVBOBuff^);
  if FTexture<>nil then FTexture.UnApply(0);
  FShaders.UseProgramObject(0);
  glPopMatrix;
  glEnable(GL_CULL_FACE);
end;

procedure TVolumetricLines.SetNode(Index: Integer;
  const Value: TAffineVector);
begin
   if Index<FNodes.Count then begin
      FNodes[Index]:=Value; FUpdated:=true; end
   else assert(false,'Index out of Bound!');
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

{ TAnimationManager }

function TAnimationManager.ActorByOffset(vOffset: integer): integer;
var i:integer;
    AO: TActorObject;
begin
  for i:=0 to FActors.Count-1 do begin
    AO:=FActors[i];
    if (AO.vOffset>=vOffset) and (vOffset<AO.vOffset+AO.vCount) then
    begin result:=i; exit; end;
  end; result:=-1;
end;

function TAnimationManager.AddActorToPack(Index: integer): TActorObject;
var Actor: TVBOActor;
    Source: PVBOBuffer;
    i,n, vOffs, iOffs: integer;
    AO: TActorObject;
begin
  Assert(FActors.Count<4096,'This Pack is full!');
  Actor:=FActorList[Index];
  Source:=FMeshes[Index];
  vOffs:=FActorPack.Vertexes.Count;
  iOffs:=FActorPack.Indices.Count;
  AO:=TActorObject.Create;
  AO.vOffset:=vOffs; AO.iOffset:=iOffs;
  AO.vCount:=Source.Vertexes.Count;
  AO.iCount:=Source.Indices.Count;
  AttachBufferInPos(Source^,FActorPack^,vOffs,iOffs);
  n:=FActors.Add(AO); result:=AO;
  setlength(FActIdx,FActorPack.Vertexes.Count);
  for i:= AO.vOffset to AO.vOffset+AO.vCount-1 do FActIdx[i]:=n;
end;

function TAnimationManager.AddSMDFile(MeshFile: string;
  const AnimFiles: array of string): integer;
var i: integer;
    Actor: TVBOActor;
    Anim: TAnimations;
begin
  Actor:=TVBOActor.Create;
  Anim.Mesh:=SMDLoad(MeshFile);
  Actor.FMeshIndex:=GetMeshFormSMD(Anim.Mesh,FMeshes,true);
  for i:=0 to high(AnimFiles) do
    Actor.FAnimIndices.Add(AttachAnimation(FAnimations,AnimFiles[i]));
  result:=FActorList.Add(Actor);
end;

procedure TAnimationManager.BuildActorPack;
begin
  PackActorsToTexture;
  GenVBOBuff(FActorPack^);
  UpdateFrames;
end;

procedure TAnimationManager.PackActorsToTexture;
var i, size, count: integer;
    attr: array of TVector;
begin
  if not assigned(vtex) then vtex:=TGLTexture.Create;
  if not assigned(ntex) then ntex:=TGLTexture.Create;
  count:=FActorPack.Vertexes.Count;
  QuadFromCount(count,size);
  vtex.CreateRGBA32FTexture2D(size,size);
  ntex.CreateRGBA32FTexture2D(size,size);
  setlength(attr,size*size);
  for i:=0 to count-1 do attr[i]:=vectormake(FActorPack.Vertexes[i],FactIdx[i]);
  vtex.UploadData(@attr[0]);
  for i:=0 to count-1 do attr[i]:=vectormake(FActorPack.Normals[i],FactIdx[i]);
  ntex.UploadData(@attr[0]);
  attr:=nil;
  rvtex:=TGLTexture.Create; //Vertex readback Texture
  rvtex.CreateRGBA32FTexture2D(vtex.Width,vtex.Height);
  rntex:=TGLTexture.Create; //Vertex readback Texture
  rntex.CreateRGBA32FTexture2D(vtex.Width,vtex.Height);
  with FFBO do begin
    AttachTexture(rvtex);
    AttachTexture(rntex);
    SetReadBackBuffer([0,1]);
    InitFBO(vtex.Width,vtex.Height);
  end;
end;

procedure TAnimationManager.UpdateFrames;
var AO: TActorObject;
    i: integer;
    frames: array of single;
begin
  setlength(frames,FActors.Count);
  if (not assigned(ftex)) or (ftex.Width<>FActors.Count) then begin
    if assigned(ftex) then ftex.Free;
    ftex:=TGLTexture.Create;
    ftex.CreateLuminance32FTextureRECT(FActors.Count,1);
  end;
  for i:=0 to FActors.Count-1 do begin AO:=FActors[i]; frames[i]:=AO.Frame; end;
  ftex.UploadData(@Frames[0]);
end;

procedure TAnimationManager.UpdateGeometry;
var vId, nId: Cardinal;
begin
  FFBO.Apply;
  FFBO.UnApply;
  vid:=rvtex.PBOReadBuffer;
  nid:=rntex.PBOReadBuffer;
//    nccount:=4; vccount:=4;

end;

constructor TAnimationManager.Create;
begin
  inherited;
  FAnimations:=TList.Create;
  FMeshes:=TList.Create;
  FActorList:=TList.Create;
  FActors:=TList.Create;
  new(FActorPack); InitVBOBuff(FActorPack^,GL_TRIANGLES,DrawElements);
  FActorPack.RenderBuffs:=[uNormals, uTexCoords];
  FFBO:=TFrameBufferObject.Create;
end;

destructor TAnimationManager.Destroy;
var i: integer;
    p: pointer;
begin
  FreeVBOList(FMeshes); FMeshes.Free;
  for i:=0 to FAnimations.Count-1 do begin
    p:=FAnimations[i]; dispose(p);
  end; FAnimations.Free;
  for i:=0 to FActorList.Count-1 do begin
    p:=FActorList[i]; TVBOActor(p).Free;
  end; FActorList.Free;
  for i:=0 to FActors.Count-1 do begin
    p:=FActors[i]; TActorObject(p).Free;
  end; FActors.Free;
  FreeVBOBuffer(FActorPack^); Dispose(FActorPack);
  vtex.Free; ntex.Free; ftex.Free;
  if assigned(rvtex) then rvtex.Free;
  if assigned(rntex) then rntex.Free;
  FFBO.Free;
  inherited;
end;

function TAnimationManager.GetActor(Index: integer): TActorObject;
begin
  result:=FActors[Index];
end;

{ TVBOActor }

constructor TVBOActor.Create;
begin
  inherited;
  FAnimIndices:=TIntegerList.Create;
  FMeshOffset:=-1;
end;

destructor TVBOActor.Destroy;
begin
  FAnimIndices.Free;
  inherited;
end;

{ TOctreeLod }

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

  Setlength(Lod.Lods,x*y*z); Lod.LodsCount:=x*y*z; n:=0;
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
        if mo.FCulled then mo.FOccluded:=false;
        mo.FCulled:=False;
        if (mo.Visible) or (mo.FProxyList.Count>0) then begin
          if assigned(mo.onBeforeCheckVisibility) then mo.onBeforeCheckVisibility(self);
          if not ((mo.FMeshType=mtHUDSprite) or (mo.FMeshType=mtSphericalSprite) or
                 (mo.FMeshType=mtCylindricalSprite))
          then mo.FCulled:=IsVolumeClipped(mo.FGeomCenter, mo.FBaundedRadius, Frustum);
          if mo.FProxyList.Count>0 then mo.FCulled:=false;
          if not mo.FCulled then FTempList.Add(mo);
        end;
      end;
    end;
    //Если нижний ЛОД - проверяем видимость его мешей
    if Lod.LodsCount=0 then begin
      for i:=0 to Lod.ObjList.Count-1 do begin
        mo:=Lod.ObjList[i];
        if assigned(mo) then begin
          if mo.FCulled then mo.FOccluded:=false;
          mo.FCulled:=False;
          if (mo.Visible) or (mo.FProxyList.Count>0) then begin
            if assigned(mo.onBeforeCheckVisibility) then mo.onBeforeCheckVisibility(self);
            if not ((mo.FMeshType=mtHUDSprite) or (mo.FMeshType=mtSphericalSprite) or
                   (mo.FMeshType=mtCylindricalSprite))
            then mo.FCulled:=IsVolumeClipped(mo.FGeomCenter, mo.FBaundedRadius, Frustum);
            if mo.FProxyList.Count>0 then mo.FCulled:=false;
            if not mo.FCulled then FTempList.Add(mo);
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

{ TUniformSMDRender }

procedure TUniformSMDRender.BlendAnimation(AnimIdx1, AnimIdx2: integer; Frame1,
  Frame2, Factor: single);
var N1,N2, Nodes1,Nodes2: TSMDNodes;
    a1,a2: PSMDFile;
    f1,f2: single;
begin
  a1:=FAnim.Animations[AnimIdx1]; a2:=FAnim.Animations[AnimIdx2];
  if Frame1>=a1.FramesCount then f1:=Frame1-trunc(frame1/a1.FramesCount)*a1.FramesCount
  else f1:=Frame1;
  Nodes1:=a1.Frames[trunc(f1)];
  if f1+1<a1.FramesCount then Nodes2:=a1.Frames[trunc(f1+1)]
  else Nodes2:=a1.Frames[0];
  setlength(N1,length(Nodes1));
  N1:=InterpolateFrame(Nodes1,Nodes2,frac(f1));

  if Frame2>=a2.FramesCount then f2:=Frame2-trunc(frame2/a2.FramesCount)*a2.FramesCount
  else f2:=Frame2;
  Nodes1:=a2.Frames[trunc(f2)];
  if f2+1<a2.FramesCount then Nodes2:=a2.Frames[trunc(f2+1)]
  else Nodes2:=a2.Frames[0];
  setlength(N2,length(Nodes1));
  N2:=InterpolateFrame(Nodes1,Nodes2,frac(f2));

  if length(FCurrentFrame)<>length(N1) then setlength(FCurrentFrame,length(N1));
  FCurrentFrame:=BlendFrames(N1,N2,Factor,FBoneArray);

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
end;

destructor TUniformSMDRender.Destroy;
begin
  Shaders.Free; FreeList(FAnim.Animations);
  glDeleteTextures(1,@FAnim.TextureId);
  inherited;
end;

procedure TUniformSMDRender.FApplyShader(mo: TObject);
begin
  Shaders.UseProgramObject(spid);
  if not FBlended then begin
    if FOldFrameNum<>FramePosition then begin
      if FSmoothed then begin
        GetBonesAtFrame(FAnim.Animations[FAnimationNum],FBoneArray,(FramePosition));
      end else begin
        GetBonesAtFrame(FAnim.Animations[FAnimationNum],FBoneArray,trunc(FramePosition));
      end;
      FOldFrameNum:=FramePosition;
      Shaders.SetUniforms(spId,'Bones',FBoneArray[0],FBones*2);
    end;
  end else begin
    Shaders.SetUniforms(spId,'Bones',FBoneArray[0],FBones*2);
    FBlended:=false;
  end;
end;

procedure TUniformSMDRender.FCreatSMDShader;
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

procedure TUniformSMDRender.NextFrame(n: single);
begin
  if FramePosition+n>=FFramesCount then FSetFrame(FramePosition+n-FFramesCount)
  else FSetFrame(FramePosition+n);
end;

procedure TUniformSMDRender.RenderObject(const ViewMatrix: TMatrix);
begin

  inherited;
end;

procedure TUniformSMDRender.setBone(Index: Integer; const Value: TVector);
begin
  FBoneArray[index]:=Value; FBlended:=true;
end;

end.

