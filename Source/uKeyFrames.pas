unit uKeyFrames;

interface

uses classes, VectorGeometry, uMiscUtils;


Type
  TKeyFrameHeader = record
    Value: TVector;
    Visible: boolean;
    Time: single;
    vType: (kfPosition,kfRotation,kfScale,kfVisible);
  end;
  PKeyFrameHeader = ^TKeyFrameHeader;

  TKeyFrame = record
    Position: TVector;
    Rotation: TVector;
    Scale: TVector;
    Time: single;
    Visible: boolean;
  end;

  TKeyFrameList = class
    private
       FPFrames: TList;  //Ключевые кадры положения
       FRFrames: TList;  //Ключевые кадры поворота
       FSFrames: TList;  //Ключевые кадры масштабирования
       FHFrames: TList;  //Ключевые кадры видимости
       FPivot: TVector;
       FLocalMatrix: TMatrix;
       FFrameRate: single;
       FCurrentFrame: integer;
       FAnimLength: integer;

       function GetCount: integer;
       function GetInterpolatedValue(const v1,v2: PKeyFrameHeader; Time: single): TKeyFrameHeader;
       function GetFrame(Time: single): TKeyFrame;
    public
       constructor Create;
       destructor Destroy; override;

       function AddPosKey(const Pos: TVector; Time: integer): integer;
       function AddRotKey(const Rot: TVector; Time: integer): integer;
       function AddScaleKey(const Scale: TVector; Time: integer): integer;
       function AddVisKey(Hide: boolean; Time: integer): integer;
       function GetModelMatrix(Time: single): TMatrix;

       procedure Clear;

       property Frames[Time: single]: TKeyFrame read GetFrame; default;
       property FrameRate: single read FFrameRate write FFrameRate;
       property Count: integer read FAnimLength;

  end;

implementation

{ TKeyFrameList }

function TKeyFrameList.AddPosKey(const Pos: TVector; Time: integer): integer;
var P: PKeyFrameHeader;
begin
  new(p); p.Value:=pos; p.Time:=Time;
  result:=FPFrames.Add(p);
end;

function TKeyFrameList.AddRotKey(const Rot: TVector; Time: integer): integer;
var P: PKeyFrameHeader;
begin
  new(p); p.Value:=rot; p.Time:=Time;
  result:=FRFrames.Add(p);
end;

function TKeyFrameList.AddScaleKey(const Scale: TVector; Time: integer): integer;
var P: PKeyFrameHeader;
begin
  new(p); p.Value:=Scale; p.Time:=Time;
  result:=FSFrames.Add(p);
end;

function TKeyFrameList.AddVisKey(hide: boolean; Time: integer): integer;
var P: PKeyFrameHeader;
begin
  new(p); p.Visible:=not hide; p.Time:=Time;
  result:=FHFrames.Add(p);
end;

procedure TKeyFrameList.Clear;
var i:integer;
begin
//
end;

constructor TKeyFrameList.Create;
begin
  inherited;
  FPFrames:=TList.Create;
  FRFrames:=TList.Create;
  FSFrames:=TList.Create;
  FHFrames:=TList.Create;

  FFrameRate:=25;
  FCurrentFrame:=0;
end;

destructor TKeyFrameList.Destroy;
begin
  FreeList(FPFrames); FreeList(FRFrames);
  FreeList(FSFrames); FreeList(FHFrames);
  inherited;
end;

function TKeyFrameList.GetCount: integer;
begin
//  result:=FFrameList.Count;
end;

function TKeyFrameList.GetFrame(Time: single): TKeyFrame;
var i: integer;
    v1,v2: PKeyFrameHeader;
    v: TKeyFrameHeader;
begin
  with result do begin
    Scale:=vectormake(1,1,1,1);
    Position:=NullHmgPoint;
    Rotation:=NullHmgVector;
    Visible:=true;
  end;

  //Интерполируем положение между кадрами
  if FPFrames.Count=1 then begin
    v1:=FPFrames[0]; result.Position:=v1.Value;
  end else begin
    for i:=0 to FPFrames.Count-2 do begin
      v1:=FPFrames[i]; v2:=FPFrames[i+1];
      if (Time>=v1.Time) and (Time<v2.Time) then begin
        v:=GetInterpolatedValue(v1,v2,Time);
        result.Position:=v.Value; Break;
      end;
    end;
    if FPFrames.Count>0 then begin
      v1:=FPFrames[0]; v2:=FPFrames[FPFrames.Count-1];
      if Time<v1.Time then result.Position:=v1.Value;
      if Time>=v2.Time then result.Position:=v2.Value;
    end;
  end;

  //Интерполируем поворот между кадрами
  if FRFrames.Count=1 then begin
    v1:=FRFrames[0]; result.Rotation:=v1.Value;
  end else begin
    for i:=0 to FRFrames.Count-2 do begin
      v1:=FRFrames[i]; v2:=FRFrames[i+1];
      if (Time>=v1.Time) and (Time<v2.Time) then begin
        v:=GetInterpolatedValue(v1,v2,Time);
        result.Rotation:=v.Value; Break;
      end;
    end;
    if FRFrames.Count>0 then begin
      v1:=FRFrames[0]; v2:=FRFrames[FRFrames.Count-1];
      if Time<v1.Time then result.Rotation:=v1.Value;
      if Time>=v2.Time then result.Rotation:=v2.Value;
    end;
  end;

  //Интерполируем масштаб между кадрами
  if FSFrames.Count=1 then begin
    v1:=FSFrames[0]; result.Scale:=v1.Value;
  end else begin
    for i:=0 to FSFrames.Count-2 do begin
      v1:=FSFrames[i]; v2:=FSFrames[i+1];
      if (Time>=v1.Time) and (Time<v2.Time) then begin
        v:=GetInterpolatedValue(v1,v2,Time);
        result.Scale:=v.Value; Break;
      end;
    end;
    if FSFrames.Count>0 then begin
      v1:=FSFrames[0]; v2:=FSFrames[FSFrames.Count-1];
      if Time<v1.Time then result.Scale:=v1.Value;
      if Time>=v2.Time then result.Scale:=v2.Value;
    end;
  end;

  //Интерполируем видимость между кадрами
  if FHFrames.Count=1 then begin
    v1:=FHFrames[0]; result.Visible:=v1.Visible;
  end else begin
    for i:=0 to FHFrames.Count-2 do begin
      v1:=FHFrames[i]; v2:=FHFrames[i+1];
      if (Time>=v1.Time) and (Time<v2.Time) then begin
        v:=GetInterpolatedValue(v1,v2,Time);
        result.Visible:=v.Visible; Break;
      end;
    end;
    if FHFrames.Count>0 then begin
      v1:=FHFrames[0]; v2:=FHFrames[FHFrames.Count-1];
      if Time<v1.Time then result.Visible:=v1.Visible;
      if Time>=v2.Time then result.Visible:=v2.Visible;
    end;
  end;

  Result.time:=time;
end;

function TKeyFrameList.GetInterpolatedValue(const v1,
  v2: PKeyFrameHeader; Time: single): TKeyFrameHeader;
var k: double;
begin
  assert(v1.vType=v2.vType,'KeyFrame header has different type');
  result.vType:=v1.vType;
  result.Time:=Time;
  if v1.vType=kfVisible then begin
    if v1.Time<Time then result.Visible:=false else result.Visible:=true;
  end else begin
    if v1.Time=v2.Time then k:=0
    else k:=(Time-v1.Time)/(v2.Time-v1.Time);
    result.Value:=VectorLerp(v1.Value,v2.Value,k);
  end;
end;

function TKeyFrameList.GetModelMatrix(Time: single): TMatrix;
var P: TKeyFrame;
    m,ms,mt,mr,mp,mnp: TMatrix;
    np: TVector;
begin
  P:=GetFrame(Time);

  ms:=CreateScaleMatrix(P.Scale);
  mt:=CreateTranslationMatrix(P.Position);
  mr:=CreateRotationMatrix(AffineVectorMake(p.Rotation),p.Rotation[3]);

  np:=VectorNegate(FPivot);
  mp:=CreateTranslationMatrix(FPivot);
  mnp:=CreateTranslationMatrix(np);

  m:=FLocalMatrix;

  //Поворот вокруг заданной точки
  m:=MatrixMultiply(m,mp);
  m:=MatrixMultiply(m,mr);
  m:=MatrixMultiply(m,mnp);
  //Масштаб
  m:=MatrixMultiply(m,ms);
  //Позиция
  m:=MatrixMultiply(m,mt);

  result:=m;
end;

end.