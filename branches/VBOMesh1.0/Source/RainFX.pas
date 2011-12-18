unit RainFX;

interface

Uses Classes, VectorGeometry, VectorLists, GeometryBB, PFXManager;

Type
   TUpdateColorProc = procedure (Index:integer; Color:TVector) of object;

   TRainPFX = class (TPFXManager)
     Private
       FUseRayCast: boolean;
       FUseTransparency: boolean;
       FRayCastProc: TRayCastProc;
       FUpdateColor: TUpdateColorProc;
       FCamPos: TAffineVector;
       FFallenDropLifeTime: single;
       function FUpdateParticle (var PLife: TParticleLifeCycle;
                                     CheckTime: TCheckTime):TCheckTimeRes;
     Public

       constructor Create (Positions:TAffineVectorList; Velocity :TAffineVector);overload;
       constructor Create (Positions:TAffineVectorList);overload;
       constructor Create; overload;

       property onRayCast: TRayCastProc read FRayCastProc write FRayCastProc;
       property onUpdateColor: TUpdateColorProc read FUpdateColor write FUpdateColor;
       property RayCastFreq:TRayCastFreq read FRayCastFreq default rcOnce;

       property UseRayCast: boolean read FUseRayCast write FUseRayCast;
       property UseTransparency: boolean read FUseTransparency write FUseTransparency;
       property CamPossition: TAffineVector read FCamPos write FCamPos;
       property FallenDropLifeTime: single read FFallenDropLifeTime
                            write FFallenDropLifeTime;

      //ѕолучить список координат и свойств частиц
       Procedure AssignParticles(Positions:TAffineVectorList;Velocity:TAffineVectorList=nil;
          Acceleration:TAffineVectorList=nil; Mass:TSingleList=nil);

   end;

implementation


{ TRainPFX }
procedure TRainPFX.AssignParticles(Positions, Velocity,
  Acceleration: TAffineVectorList; Mass: TSingleList);
begin
  FCount:=Positions.count;
  FLifeTimeList.Count:=FCount;
  FInitTimes;
  FPosList:=Positions;
  FVelocityList:=nil;
  FAccelList:=nil;
  FMassList:=nil;
end;

constructor TRainPFX.Create(Positions: TAffineVectorList; Velocity:TAffineVector);
begin
  inherited Create(Positions,Velocity, NullVector, -1);
  FTimeUpdateEvent:=FUpdateParticle;
  FRayCastFreq:=rcOnce;
  FUseForces:=false;
  FGravity:=0;
  FFallenDropLifeTime:=0.5;
end;

constructor TRainPFX.Create;
begin
  inherited;
  FTimeUpdateEvent:=FUpdateParticle;
  FRayCastFreq:=rcOnce;
  FUseForces:=false;
  FGravity:=0;
  FFallenDropLifeTime:=0.5;
end;

constructor TRainPFX.Create(Positions: TAffineVectorList);
begin
  inherited;
  FTimeUpdateEvent:=FUpdateParticle;
  FRayCastFreq:=rcOnce;
  FUseForces:=false;
  FGravity:=0;
  FFallenDropLifeTime:=0.5;
end;

function TRainPFX.FUpdateParticle(var PLife: TParticleLifeCycle;
  CheckTime: TCheckTime): TCheckTimeRes;
var p,n,v:TVector;
    d:double;
    w,l:integer;
begin
 result:=trContinue;
 //Ёта процедура будет вызыватьс€ дважды - до и после расчета координат
 //Ќа первом проходе мы вычисл€ем возможную точку коллизии,
 //на втором - если коллизи€ произошла - запускаем жзненный цикл частицы
 with PLife, FExtents do begin
   w:=trunc(emax[0]-emin[0]);
   l:=trunc(emax[2]-emin[2]);
   case CheckTime of
      ctBefore: begin
        if not Started then begin
          if FUseRayCast and (Dir[1]<>0)then begin
            if not RayCasted or (RayCastFreq=rcEveryUpdate) then begin
             if assigned(FRayCastProc) then
                RayIntersected:=FRayCastProc(vectormake(Pos),vectormake(Dir),p,n)
             else RayIntersected:=false;
             if RayIntersected then begin
                iPosition:=p; iNormal:=n;
             end;
             RayCasted:=true;
            end;
          end;
        end;
      end;

      ctAfter: begin
        if (not started) and (RayCasted and RayIntersected) then begin
         if (Pos[1]<=iPosition[1]) then begin
           FPosList[PIndex]:=affinevectormake(iPosition);
           Started:=true; Iteration:=0; LifeTime:=FFallenDropLifeTime; TimeLeft:=0;
           result:=trBreak;
         end;
        end;
        if (not PointInAABB(Pos,TAABB(FExtents))) and (not RayIntersected) then begin
           setvector(v,random(w*100)/100+emin[0],emax[1],random(l*100)/100+emin[2]);
           FPosList[PIndex]:=affinevectormake(v);
           if assigned(FUpdateColor) then begin
              FUpdateColor(PIndex,vectormake(1,0,0,1));
              if FUseTransparency then
                 FUpdateColor(PIndex,vectormake(1,1,1,0))
              else FUpdateColor(PIndex,vectormake(1,1,1,1));
           end;
           TimeLeft:=0;
           Started:=false; RayCasted:=false;
           RayIntersected:=false;
           RayCastFreq:=FRayCastFreq;
           result:=trBreak;
        end;
        if Started then begin
           TimeLeft:=TimeLeft+dTime;
           if TimeLeft>=LifeTime then begin
              setvector(v,random(w*100)/100+emin[0],emax[1],random(l*100)/100+emin[2]);
              FPosList[PIndex]:=affinevectormake(v);
              Started:=false; RayCasted:=false;
              RayIntersected:=false;
              RayCastFreq:=FRayCastFreq;
              TimeLeft:=0;
              result:=trBreak;
           end else begin
              d:=1-TimeLeft/LifeTime;
              if assigned(FUpdateColor) and FUseTransparency then
                 FUpdateColor(PIndex,vectormake(1,1,1,d));
              result:=trBreak;
           end;
        end;
       //”меньшение прозрачности по мере удалени€ от камеры
        if FUseTransparency and assigned(FUpdateColor) then begin
          d:=1-VectorDistance(Pos,FCamPos)/50;
          if d<0 then d:=0.1;
          FUpdateColor(PIndex,vectormake(1,1,1,d));
        end;
      end;
   end;
 end;
end;

end.
