unit PFXManager;

interface

Uses Classes, VectorGeometry, VectorLists, uVBO, OGLStateEmul;

Type
  TForceType = (ftConstant, ftLinAttenuate, ftQuadricAttenuate,ftUserFunction);
  TRayCastFreq = (rcNone, rcOnce, rcEveryUpdate, rcOnDemand);
  TCheckTime = (ctBefore, ctAfter);
  TCheckTimeRes = (trContinue, trBreak);
  PForce = ^TForce;
  TForce = record
    Position: TAffineVector;
    Force: TAffineVector;
    Attenuation: TVector;
    ForceType: TForceType;
    Name:String;
  end;
  //Жизненный цикл частицы, заполняется пользователем
  PParticleLifeCycle = ^TParticleLifeCycle;
  TParticleLifeCycle = record
     PIndex: integer; //номер частицы в списке
     Pos, Dir: TAffineVector; //текущие координаты
       //позиция и направление движения частицы на предыдущем шаге
     OldPos, OldDir: TAffineVector;
     dTime: double;//сколько прошло времени с предыдущего обновления
     Started: boolean; //активен ли счетчик
     Iteration: integer; //номер итерации, для нужд пользователя
     TimeLeft: double; //сколько прошло времени с момента активации
     LifeTime: double; //каково суммарное время жизни частицы на этой итерации
     iPosition: TVector; //точка пересечения траектории движения частицы с объектом
     iNormal: TVector;//нормаль к точке пересечения
     Color: TVector; //Для нужд пользователя, цвет точки на данной итерации
     //коэффициенты полинома по которому будет интерполироваться цвет частицы
     Attenuation: TVector;
     //как часто искать коллизии - rcOnce - единожды,
     //rcEveryUpdate - при каждом обновлении, rcOnDemand - по запросу
     RayCastFreq: TRayCastFreq; //частота вызова функции определения коллизии
     RayCasted: boolean; //была ли попытка найти пересечении пути частицы с элементами сцены
     RayIntersected: boolean; //результат поиска коллизии
     Tag:Integer; //Для нужд пользователя
  end;
  //Events
  TLifeTimeProc = function (var PLife: TParticleLifeCycle;
                            CheckTime: TCheckTime):TCheckTimeRes of object;
  TUserForceProc = function (Pos:TAffineVector):TAffineVector of object;
  TRayCastProc = function (Pos,Direction: TVector; var iPoint: TVector;
                           var iNormal: TVector): boolean of object;

  TPFXManager = class(TObject)
  Private
    procedure FInit;
    procedure SetGravity(const Value: single);
    procedure SetUpVector(const Value: TAffineVector);
    //Расчет суперпозиции сил
    function  FForcesSuperposition(Pos: TAffineVector):TAffineVector;
    procedure SetTimeSpeed(const Value: single);
    procedure SetDeltaTime(const Value: double);
    procedure SetRayCastFreq(const Value: TRayCastFreq);
  Protected
    FCount: integer;
    FMass: single;
    FVelocity: TAffineVector;
    FAcceleration: TAffineVector;
    FPosList: TAffineVectorList;
    FVelocityList: TAffineVectorList;
    FAccelList: TAffineVectorList;
    FUpdateList: TIntegerList;
    FMassList: TSingleList;
    FLifeTimeList: TList;
    FGravity: single;
    FUpGravity: TAffineVector;

    FTimeSpeed: single;
    FDeltaTime: double;
    FUpdateTime: double;
    FForces: TList;
    FUseForces: Boolean;
    FUp: TAffineVector;

    FTimeUpdateEvent: TLifeTimeProc;
    FRayCastFreq: TRayCastFreq;

    FUserForce: TUserForceProc;

    FExtents: TExtents;

    procedure FInitTimes;
  public
    Enabled:Boolean;

    constructor Create(Positions:TAffineVectorList;Velocity:TAffineVectorList=nil;
       Acceleration:TAffineVectorList=nil; Mass:TSingleList=nil);overload;
    constructor Create (Positions:TAffineVectorList; Velocity,
       Acceleration:TAffineVector; Mass:Single);overload;
    constructor Create (Positions:TAffineVectorList);overload;
    constructor Create; overload;
    destructor Destroy; override;

    //Задает границы частиц
    Property Extents:TExtents read FExtents write FExtents;
    //Задает вектор UP, участвует при расчете ускорения свободного падения
    Property Up:TAffineVector read FUp write SetUpVector;
    //Использовать ли при расчетах силы, масса должна быть больше 0
    Property UseForces: Boolean read FUseForces write FUseForces;
    //Гравитация, масса, скорость, ускорение - константы используемые
    // в случае отсутствия соответствующих списков
    Property Gravity:single read FGravity write SetGravity;
    Property Mass:single read FMass write FMass;
    Property Velocity:TAffineVector read FVelocity write FVelocity;
    Property Acceleration:TAffineVector read FAcceleration write FAcceleration;
    //скорость течения времени, 1 - в реальном времени
    Property TimeSpeed:single read FTimeSpeed write SetTimeSpeed;
    Property DeltaTime:double read FDeltaTime write SetDeltaTime;
    Property UpdateTime:double read FUpdateTime write FUpdateTime;
    Property RayCastFreq:TRayCastFreq read FRayCastFreq write SetRayCastFreq;
    //Вызывается каждый раз перед началом расчета координат
    Property OnUpdateTime:TLifeTimeProc read FTimeUpdateEvent write FTimeUpdateEvent;
    //Списки с характеристиками каждой частицы
    Function AddForce(Force:TForce):integer; overload;
    Function AddForce(aForce, aPosition: TAffineVector; aForceType:
                    TForceType=ftConstant; aName:string=''):integer; overload;
    Function AddForce(aForce, aPosition: TAffineVector;
                      aAttenuation:TVector; aForceType: TForceType=
                      ftQuadricAttenuate; aName: string=''): integer;overload;
    //Получить списко координат и свойств частиц
    Procedure AssignParticles(Positions:TAffineVectorList;Velocity:TAffineVectorList=nil;
       Acceleration:TAffineVectorList=nil; Mass:TSingleList=nil);
    //Подключает списко индексов частиц, для которых нужно пересчитать координаты
    Procedure SetUpdateList(UpdateList:TIntegerList=nil);
    //Указывает активное количество частиц
    Procedure SetUpdateCounts(Count:integer=-1);
    //Вычисляет новые координаты частицы, вызывается перед рендерингом частицы
    Function  UpdateParticles(UpdateList:TIntegerList=nil):boolean;
    Procedure ResetVelocity(Velocity:TAffineVector; ResetList:TIntegerList=nil);
  end;

implementation

{ TPFXManager }

constructor TPFXManager.Create(Positions:TAffineVectorList;
  Velocity:TAffineVectorList=nil; Acceleration:TAffineVectorList=nil;
  Mass:TSingleList=nil);
begin
  inherited Create;
  FInit;
  FCount:=Positions.count;
  FLifeTimeList.Count:=FCount;
  if Velocity<>nil then assert(Fcount=Velocity.Count,'Length of Positions and ' +
    'Velocity in not equal');
  if Acceleration<>nil then assert(Fcount=Acceleration.Count,'Length of ' +
    'Positions and Acceleration in not equal');
  if Mass<>nil then assert(Fcount=Mass.Count,'Length of Positions and ' +
    'Mass in not equal');
  FPosList:=Positions;
  FVelocityList:=Velocity;
  FAccelList:=Acceleration;
  FMassList:=Mass;
  FInitTimes;
end;


constructor TPFXManager.Create(Positions: TAffineVectorList; Velocity,
  Acceleration: TAffineVector; Mass: Single);
begin
  inherited Create;
  FInit;
  FCount:=Positions.count;
  FLifeTimeList.Count:=FCount;
  FPosList:=Positions;
  FVelocity:=Velocity;
  FAcceleration:=Acceleration;
  FMass:=Mass;
  FInitTimes;
end;

function TPFXManager.AddForce(Force: TForce): integer;
var PF:PForce;
begin
   new(PF); PF^:=Force;
   Result:=FForces.Add(PF);
end;

Function TPFXManager.AddForce(aForce, aPosition: TAffineVector;
                              aForceType:TForceType; aName:string):integer;
var PF:PForce;
begin
   new(PF);
   with PF^ do begin
      Force:=aForce;
      Attenuation:=vectormake(0,0,0,0);
      Position:=aPosition;
      ForceType:=aForceType;
      Name:=aName;
   end;
   Result:=FForces.Add(PF);
end;

Function TPFXManager.AddForce(aForce, aPosition: TAffineVector;
           aAttenuation:TVector; aForceType: TForceType; aName:string): integer;
var PF:PForce;
begin
   new(PF);
   with PF^ do begin
      Force:=aForce;
      Attenuation:=aAttenuation;
      Position:=aPosition;
      ForceType:=aForceType;
      Name:=aName;
   end;
   Result:=FForces.Add(PF);
end;

procedure TPFXManager.AssignParticles(Positions, Velocity,
  Acceleration: TAffineVectorList; Mass: TSingleList);
begin
  FCount:=Positions.count;
  FLifeTimeList.Count:=FCount;
  FInitTimes;
  FPosList:=Positions;
  if (Velocity<>nil) //and (Fcount=Velocity.Count)
  then FVelocityList:=Velocity else FVelocityList:=nil;
//  assert(Fcount=Velocity.Count,'Length of Positions and Velocity in not equal');
  if (Acceleration<>nil) //and (Fcount=Acceleration.Count)
  then FAccelList:=Acceleration else FAccelList:=nil;
//   assert(Fcount=Acceleration.Count,'Length of ' +
//    'Positions and Acceleration in not equal');
  if (Mass<>nil)// and (Fcount=Mass.Count)
  then FMassList:=Mass else FMassList:=nil;
//  assert(Fcount=Mass.Count,'Length of Positions and Mass in not equal');
end;

constructor TPFXManager.Create;
begin
  inherited Create;
  FInit;
end;

constructor TPFXManager.Create(Positions: TAffineVectorList);
begin
  inherited Create;
  FInit;
  FCount:=Positions.count;
  FPosList:=Positions;
  FLifeTimeList.Count:=FCount;
  FInitTimes;
end;

destructor TPFXManager.Destroy;
var i:integer;
    PF:PForce;
    plt:PParticleLifeCycle;
begin
  Enabled:=false;
  for i:=0 to FForces.Count-1 do begin
      PF:=FForces[i];Dispose(PF);
  end; FForces.Clear; FForces.Free;
  for i:=0 to FLifeTimeList.Count-1 do begin
     plt:=FLifeTimeList[i]; dispose(plt);
  end;
  FLifeTimeList.Clear;FLifeTimeList.Free;
  inherited;
end;

function TPFXManager.FForcesSuperposition(Pos: TAffineVector): TAffineVector;
var i:integer;
    PF:PForce;
    d,att:single;
    F:TAffineVector;
begin
   Setvector(Result,0,0,0);
   for i := 0 to FForces.Count-1 do begin
       PF:=FForces[i];
       with PF^ do begin
         case ForceType of
           ftConstant: AddVector(Result,Force);
           ftLinAttenuate: begin
             d:=VectorLength(VectorSubtract(Pos, Position));
             att:=Attenuation[0]+d*Attenuation[1];
             F:=VectorScale(Force,att);
             AddVector(Result,F);
           end;
           ftQuadricAttenuate: begin
             d:=VectorLength(VectorSubtract(Pos, Position));
             att:=Attenuation[0]+d*Attenuation[1]+d*d*Attenuation[2];
             F:=VectorScale(Force,att);
             AddVector(Result,F);
           end;
           ftUserFunction: begin
             if assigned(FUserForce) then begin
                F:=FUserForce(Pos);
                AddVector(Result,F);
             end;
           end;
         end;
       end;
   end;
end;

procedure TPFXManager.FInit;
begin
  FCount:=-1;
  FPosList:=nil;
  FVelocityList:=nil;
  FAccelList:=nil;
  FMassList:=nil;
  FVelocity:=NullVector;
  FAcceleration:=NullVector;
  FMass:=-1; FGravity:=0;
  setvector(FUp,0,1,0);
  FForces:=TList.Create;
  FLifeTimeList:=TList.Create;
  FUseForces:=false;
  FUserForce:=nil;
  FTimeSpeed:=1;
  FUpdateList:=nil;
  FUpdateTime:=-1;
  FTimeUpdateEvent:=nil;
  Enabled:=false;
end;

procedure TPFXManager.FInitTimes;
var plc:PParticleLifeCycle;
    i:integer;
begin
  for i := 0 to FLifeTimeList.Count-1 do begin
    new(plc);
    plc.Started:=false;
    plc.RayCasted:=false;
    plc.RayCastFreq:=FRayCastFreq;
    plc.RayIntersected:=false;
    FLifeTimeList[i]:=plc;
  end;
end;

procedure TPFXManager.ResetVelocity(Velocity: TAffineVector;
  ResetList: TIntegerList);
var i,n,rcount:integer;

begin
   if ResetList=nil then rcount:=FVelocityList.Count
   else rcount:=ResetList.Count;
   for i:=0 to rcount-1 do begin
      if ResetList=nil then n:=i else n:=ResetList[i];
      FVelocityList[n]:=Velocity;
   end;
end;

procedure TPFXManager.SetDeltaTime(const Value: double);
begin
  if Enabled then FDeltaTime := FDeltaTime + Value
  else FDeltaTime := Value;
end;

procedure TPFXManager.SetGravity(const Value: single);
begin
  FGravity := Value;
  FUpGravity := VectorScale(FUp,FGravity);
end;

procedure TPFXManager.SetRayCastFreq(const Value: TRayCastFreq);
begin
  FRayCastFreq := Value;
end;

procedure TPFXManager.SetTimeSpeed(const Value: single);
begin
  FTimeSpeed := Value;
end;

procedure TPFXManager.SetUpdateCounts(Count: integer);
begin
  if count>=0 then begin
     FUpdateList:=nil;
     FCount:=count;
  end else begin
     FCount:=FPosList.Count;
     FUpdateList:=nil;
  end;
end;

procedure TPFXManager.SetUpdateList(UpdateList: TIntegerList);
begin
  if not assigned(UpdateList) then begin
      FCount:=FPosList.Count;
      FUpdateList:=nil;
  end else begin
    FCount:=UpdateList.Count;
    FUpdateList:=UpdateList;
  end;
end;

procedure TPFXManager.SetUpVector(const Value: TAffineVector);
begin
  FUp := Value;
  FUpGravity := VectorScale(FUp,FGravity);
end;

//Основная функция менеджера, здесь происходит расчет всей физики и коллизий
function TPFXManager.UpdateParticles(UpdateList: TIntegerList):boolean;
var p:TAffineVector;
    v,sv:TAffineVector;
    a,sa:TAffineVector;
    m,deltaTime:single;
    i,n,ucount:integer;
    useUL:boolean;
    F,Ft:TAffineVector;
    plc:PParticleLifeCycle;
begin
   result:=false;
   if (not Enabled) or ((FUpdateTime>0) and (FUpdateTime>FDeltaTime)) then exit;
   deltaTime:=FDeltaTime*FTimeSpeed;
   if assigned(UpdateList)then begin
      ucount:=UpdateList.Count; useUL:=true;
   end else begin
      ucount:=FCount; useUL:=false;
   end;
   for i:=0 to uCount-1 do begin
      if useUL then n:=UpdateList[i] else n:=i;
      p:=FPosList[n];
      if assigned(FVelocityList) and (n<FVelocityList.Count)
      then v:=FVelocityList[n] else v:=FVelocity;
      if assigned(FAccelList) and (n<FAccelList.Count)
      then a:=FAccelList[n] else a:=FAcceleration;
      if assigned(FMassList) and (n<FMassList.Count)
      then m:=FMassList[n] else m:=FMass;
      plc:=FLifeTimeList[n];
      //Проверим состояние частицы с текущими параметрами
      //если функция вернула trBreak - частица не активна
      if assigned(FTimeUpdateEvent) then begin
         with plc^ do begin
            PIndex:=n; Pos:=p; Dir:=v; dTime:=FDeltaTime;
         end;
         if FTimeUpdateEvent(plc^, ctBefore)=trBreak then Continue;
      end;
      //Если используются силы - вычислим суперпозицию всех сил в этой точке
      //Работать с силами можно только если масса частицы "m" больше 0
      if FUseForces and (m>0) then begin
         F:=FForcesSuperposition(p);
         Ft:=VectorScale(v,m); //Импульс тела Ft=m*v
         SubtractVector(Ft,F); //учтем воздействие внешних сил
         v:=VectorScale(Ft,1/m); //вычислим скорость из результирующего импульса
      end;
      //компонента перемещения при равномерном движении S=Vt
      sv:=vectorscale(v,deltaTime*10);
      //учитываем гравитацию
      a:=VectorSubtract(a,FUpGravity);
      //компонента перемещения при равноускоренном движении S=a(t*t)/2
      sa:=vectorscale(a,sqr(deltaTime)/2);
      //Результирующее перемещение S=Vt+1/2*at^2
      p[0]:=p[0]+(sv[0]+sa[0]);
      p[1]:=p[1]+(sv[1]+sa[1]);
      p[2]:=p[2]+(sv[2]+sa[2]);
      //Пересчитываем вектор скорости с учетом ускорения V=V+at
      scalevector(a,deltatime); AddVector(v,a);
      //Проверим состояние частицы с новыми параметрами
      //если функция вернула trBreak - частица не активна и параметры не нужно обновлять
      if assigned(FTimeUpdateEvent) then begin
         with plc^ do begin
            PIndex:=n; Pos:=p; Dir:=v; dTime:=FDeltaTime;
         end;
         if FTimeUpdateEvent(plc^,ctAfter)=trBreak then Continue;
      end;
      //обновим скорости частиц
      if assigned(FVelocityList) and (n<FVelocityList.Count)
      then FVelocityList[n]:=v else FVelocity:=v;
      //Сохраняем новые координаты
      FPosList[n]:=p;
   end;
   FDeltaTime:=0; result:=true;
end;

end.
