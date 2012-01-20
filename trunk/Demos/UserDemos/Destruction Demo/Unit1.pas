unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StdCtrls, Dialogs, GLScene, GLObjects, GLCoordinates, GLCadencer, GLWin32Viewer,
  GLCrossPlatform, BaseClasses, GLKeyboard, VectorGeometry, VectorLists, VectorTypes,
  VBOMesh, uMeshObjects, uTextures, uMaterials, NewtonImport, OGLStateEmul;

type
  RParticle = record
    Sprite : TVBOMeshObject;
    Speed  : single;
    Active : boolean;
  end;                    

  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure Button1Click(Sender: TObject);
    procedure EndFrame;
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  VBOMesh : TVBOMesh;
  Master,MasterBroken,Bench:TVbOMeshObject;
  Expl: TVBOAnimatedSprite;
  ExplTex,PartTex: TTexture;
  Particles : array of RParticle;
  NWorld    : PNewtonWorld;
  Fragments : array of TVBOMeshObject;
  FragB     : array of PNewtonBody;
  Ground    : PNewtonBody;
  MX,MY     : integer;


implementation

{$R *.dfm}

//Функция обратного вызова для нашых физических тел
procedure NewtonCallBack( const body : PNewtonBody; timestep : Float; threadIndex : int ); cdecl;
var
  M : Single;
  I : TVector3f;
  F : TVector3f;
begin
  NewtonBodyGetMassMatrix(Body, @M, @I[0], @I[1], @I[2]); //Получаем массу тела
  F:= affinevectormake(0, -10*m, 0);//Задём силу, F=M*G
  NewtonBodyAddForce(Body, @F[0]);  //Добавляем силу к телу
end;

procedure TForm1.EndFrame;          //Событие на достижение конечного кадра анимации нашего анимированного спрайта
begin
  Expl.Stop;                        //Останавливаем анимацию
  Expl.FirstFrame;                  //возвращаем в нулевой кадр
end;

procedure TForm1.FormCreate(Sender: TObject);
var List: TAffineVectorList;
    Col: PNewtonCollision;
    i,j:integer;
    Face :TMatrix3f;
    Faces:array of TVector3f;
begin
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  OGLStateEmul.GLStateCache.CheckStates;
  VBOMesh:=TVBOMesh.CreateAsChild(GLscene1.Objects);

  Master:=VBOMesh.AddMeshFromFile('Media\BenchNew.obj');             //Создаём мастер объект для целой скамейки
  Master.Visible:=false;                                       //скрываем его
  MasterBroken:=VBOMesh.AddMeshFromFile('Media\BrokenBenchNew.obj'); //создаём мастер объект для сломаной скамейки
  MasterBroken.Visible:=false;                                 //также скрываем его
  Bench:=VBOMesh.AddProxyObject(Master);                       //создаём на прокси объект
  //Ссылка на мастер объект хранится у прокси в PARAMS!

  ExplTex:=TTexture.CreateFromFile('Media\expl.bmp');                //Создаём текстуру взрыва
  ExplTex.BlendingMode:=tbmAdditive;
  ExplTex.TextureMode:=tcReplace;
  Expl:=(VBOmesh.AddAnimatedSprite(stSpherical,1,1) as TVBOAnimatedSprite); //Создаём анимированных спрайт взрыва
  Expl.Texture:=ExplTex;                                       //Присваиваем ему уже созданную текстуру
  Expl.FramesDirection:=fdHorizontal;                          //Задём направление кадров по горизонтали
  Expl.FramesCount:=9;                                         //Указываем количество кадров
  Expl.HorFramesCount:=3;                                      //Кол-во кадров по горизонтали
  Expl.VertFramesCount:=3;                                     //Кол-во кадров по вертикали
  Expl.FrameRate:=18;                                          //Кол-во кадров в секунду
  Expl.NoZWrite:=true;
  Expl.Visible:=false;
  Expl.ToFrame(0);
  Expl.OnEndFrameReached:=EndFrame;                            //Задаём процедуру которая будет выполняться по достижении последнего кадра

  PartTex:=TTexture.CreateFromFile('Media\Dust.tga');                //Создаём текстуру взрыва
  PartTex.BlendingMode:=tbmTransparency;                       //настраиваем параметры текстуры
  PartTex.TextureMode:=tcAdd;
  SetLength(Particles,8);
  for i:=0 to Length(Particles)-1 do                           //Создаёи наши частицы на спрайтах кторые будут в роли пыли
  begin
    with Particles[i] do
    begin
      Sprite:=VBOMesh.AddSprite(stSpherical,3,3);             //Создаём спрайт сферический размерами 3 на 3
      Sprite.Visible:=false;
      Sprite.Texture:=PartTex;                                //назначаем текстуру
      Sprite.Material:=TMaterial.Create;                      //Создаём материал, чтобы можно было менять Альфа канал
      Sprite.NoZWrite:=true;
      Active:=false;
    end;
  end;                                                        //Создаём осколки от разрушеной скамейки
                                                              //Создаём ньютоновский мир
  NWorld:=NewtonCreate(nil,nil);
  NewtonSetSolverModel(NWorld,1);
  SetLength(Fragments,5);
  SetLength(FragB,5);
  for i:=0 to length(Fragments)-1 do
  begin
    Fragments[i]:=VBOmesh.AddMeshFromFile('Media\Mesh.3ds');       //Создаём меш которые будет изображать наши осколки
    Fragments[i].Visible:=false;
    List:=TAffineVectorList.Create;
    Fragments[i].GetTriMesh(List);                           //Создаём коллизию для нашего физического тела
    SetLength(Faces,List.Count);                             //Для этого извлекаем треугольники из меша
    for j:=0 to Length(Faces)-1 do                           //Заносим их в массив Faces
    begin
      Faces[j][0]:=List[j][0];
      Faces[j][1]:=List[j][1];
      Faces[j][2]:=List[j][2];
    end;
    Col := NewtonCreateConvexHull(NWorld,List.Count,@Faces[0],SizeOf(TAffineVector),0,0,nil); //Строим физическую коллизию на нашим треугольникам
    List.Free;                                            //ConvexHull(выпуклая оболчка) в отличие от TriMeshCollision не может может иметь вогнутых поверхностей, но может быть динамическим
    FragB[i]:= NewtonCreateBody(NWorld, Col);             // Создаём тело с нашей коллизией
    NewtonReleaseCollision(NWorld, Col);                  //Удаляем ненужную нам коллизию
    NewtonBodySetMassMatrix(FragB[i],0,1,1,1);            //Устанавливаем весо и моменты инерции
    NewtonBodySetForceAndTorqueCallBack(FragB[i],NewtonCallBack); //Задаём функцию обратного вызова, в которой мы будет прикладывать силы к телу
  end;

  Col:=NewtonCreateBox(NWorld,100,0.2,100,0,nil);          //Создаём тело иммитирующее землю
  Ground:=NewtonCreateBody(NWorld,Col);
  NewtonReleaseCollision(NWorld, Col);
end;



procedure TForm1.FormDestroy(Sender: TObject);
begin
  NewtonDestroy(NWorld);
  VBOmesh.Free;
  GLSceneViewer1.Buffer.RenderingContext.Deactivate;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
var i:integer;
M:TMatrix;
begin
  NewtonUpdate(NWorld,DeltaTime);               //Производим симуляцию ньютоновского мира
  for i:=0 to length(Fragments)-1 do
  begin
    NewtonBodyGetMatrix(FragB[i],@M);           //Синхронизируем графические объекты с физическими телами
    Fragments[i].Matrices.ModelMatrix:=M;
    Fragments[i].UpdateWorldMatrix();
  end;
  for i:=0 to Length(Particles)-1 do
  begin
    if Particles[i].Active then
    With Particles[i] do begin
      Sprite.MoveForward(Speed*DeltaTime);       //Когда наша частица активна она будет двигаться вперёд
      Speed:=Speed-(Speed*Speed*DeltaTime);      //Постепенно уменьшаем скорость движения
      Sprite.ScaleObject(VectorAdd(Sprite.Scale,DeltaTime)); //Увеличиваем размер спрайта
      with Sprite.Material.Properties.DiffuseColor do begin
        Alpha:=Alpha-DeltaTime/2;//Уменьшаем альфу
        if Alpha<0.01 then Particles[i].Active:=false; //Выключам нашу частицу когда она становиться прозрачной
      end;
    end;
  end;                   
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.Button1Click(Sender: TObject);
var i:integer;
    Angle:single;
    V:TVector;
    M:TMatrix;
begin
  Randomize;
  Expl.Visible:=true;
  Expl.Position:=VectorMake(0,0.5,0,1);
  Expl.Play(samLoop);                       //Запускаеманимацию взрыва
  Bench.ChangeProxy(MasterBroken);          //Меняем мастер объект нашего прокси на сломанный
  Angle:=0;
  for i:=0 to Length(Particles)-1 do
  begin
    with Particles[i] do                   //Создаём эффект разлетающейся пыли с помощью наших частиц
    begin
      Sprite.Visible:=true;
      Sprite.Material.Properties.DiffuseColor.Alpha:=1;
      Sprite.ScaleObject(1,1,1);
      Sprite.Direction:=VectorMake(0,0,1);
      Sprite.Position:=VectorMake(0,0,0,1);
      Active:=true;
      Speed:=50;
      Sprite.TurnObject(Angle);
    end;
    Angle:=Angle+2*pi/8;
  end;

  for i:=0 to Length(FragB)-1 do
  begin
    Fragments[i].Visible:=true;
    NewtonBodyGetMatrix(FragB[i],@M);        //Ставим наши физические тела в нужную позицию, чтобы они находились в скамейке
    M[3]:=VectorMake(0,1,0,1);
    NewtonBodySetMatrix(FragB[i],@M);
    V:=VectorMake(20-Random(40),5+Random(15),20-Random(40));
    NewtonBodySetVelocity(FragB[i],@V);     //Задаём рандомную скорость наших физических тел
    V:=VectorMake(Random(50),Random(50),Random(50));
    NewtonBodySetOmega(FragB[i],@V);        //задаём рандомную угловую скорость по всем осям , чтобы создать вращение
    NewtonBodySetMassMatrix(FragB[i],1,1,1,1); //Задаём массу и моменты инерции тел
  end;
end;


procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;  //Вращение камеры и зум
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  GLCamera1.AdjustDistanceToTarget(Power(1.1, WheelDelta/120));
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if IsKeyDown($1) then
  begin
   GLCamera1.MoveAroundTarget((MY-Y), (MX-X));
   MX:=X; MY:=Y;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Bench.ChangeProxy(Master); //Востанавливаем нашу скамейку меняя мастер объект
end;

end.
