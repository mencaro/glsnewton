{: uRenderStates
  //
  /* Объединения основных состояний рендера в один класс
  //
	Historique:
    12/05/11 - Fantom - Создан модуль
}


unit uRenderStates;

interface

uses OpenGL1x, OGLStateEmul;

Type

  TBlendingMode = (bmOpaque, bmTransparency, bmAdditive, bmAlphaTest50,
                    bmAlphaTest100, bmModulate);
  TDepthFunc = (dfNever,dfLess,dfEqual,dfLEqual,dfGreater,dfNotEqual,dfGEqual,dfAlways);
  TFaceMode = (fmPoints, fmLines, fmFill);
  TFaceWinding = (fwCW, fwCCW);

  TBlendingStates = class
   private
     FBlendEnable: boolean;
     FAlphaTestEnable: boolean;
     FSrcBlendFunc: Cardinal;
     FDstBlendFunc: Cardinal;
     FAlphaFunc: Cardinal;
     FAlphaThreshold: single;
   public
     constructor Create;
     destructor Destroy;override;
     procedure Apply;
     procedure unApply;
     procedure ResetBlendingStates;
     procedure LoadPreset(BlendingMode: TBlendingMode);

     property BlendEnable: boolean read FBlendEnable write FBlendEnable;
     property AlphaTestEnable: boolean read FAlphaTestEnable write FAlphaTestEnable;
     property SrcBlendFunc: Cardinal read FSrcBlendFunc write FSrcBlendFunc;
     property DstBlendFunc: Cardinal read FDstBlendFunc write FDstBlendFunc;
     property AlphaFunc: Cardinal read FAlphaFunc write FAlphaFunc;
     property AlphaThreshold: single read FAlphaThreshold write FAlphaThreshold;
  end;

  TDepthStates = class
  private
    FDepthMask: boolean;
    FDepthTest: boolean;
    FDepthFunc: TDepthFunc;
  public
    constructor Create;
    destructor Destroy;override;

    procedure ResetDepthStates;
    procedure Apply;
    procedure unApply;


    property DepthTest: boolean read FDepthTest write FDepthTest;
    property DepthMask: boolean read FDepthMask write FDepthMask;
    property DepthFunc: TDepthFunc read FDepthFunc write FDepthFunc;
  end;

  TClipPlane = class
    PlaneEquation: array[0..3] of single;
    Active: boolean;
  public
    constructor Create;
  end;

  TClipPlanes = class
  private
    FPlanes: array of TClipPlane;
    function getClipPlane(Index: integer): TClipPlane;
  public
    constructor Create;
    destructor Destroy;override;

    procedure Apply;
    procedure unApply;
    procedure ResetClipPlanes;

    property Planes[Index: integer]: TClipPlane read getClipPlane; default;
  end;

  TPolygonStates = class
  private
    FFaceMode: TFaceMode;
    FFaceWinding: TFaceWinding;
    FFaceCull: boolean;
  public
    constructor Create;

    procedure Apply;
    procedure unApply;
    procedure ResetPolygonStates;

    property FaceMode: TFaceMode read FFaceMode write FFaceMode;
    property FaceWinding: TFaceWinding read FFaceWinding write FFaceWinding;
    property FaceCull: boolean read FFaceCull write FFaceCull;
  end;


  TRenderStates = class
  private
    FBlendingStates: TBlendingStates;
    FDepthStates: TDepthStates;
    FClipPlanes: TClipPlanes;
    FPolygonStates: TPolygonStates;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ApplyStates;
    procedure UnApplyStates;
    procedure ResetStates;

    property BlendingStates: TBlendingStates read FBlendingStates;
    property DepthStates: TDepthStates read FDepthStates;
    property ClipPlanes: TClipPlanes read FClipPlanes;
    property PolygonStates: TPolygonStates read FPolygonStates;
  end;

const
  cDepthFunc: array[dfNever..dfAlways] of cardinal = (GL_NEVER,GL_LESS,
    GL_EQUAL,GL_LEQUAL,GL_GREATER,GL_NOTEQUAL,GL_GEQUAL,GL_ALWAYS);

implementation


{ TBlendingStates }


procedure TBlendingStates.Apply;
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

constructor TBlendingStates.Create;
begin
  inherited;
  ResetBlendingStates;
end;

destructor TBlendingStates.Destroy;
begin
  inherited;
end;

procedure TBlendingStates.LoadPreset(BlendingMode: TBlendingMode);
begin
  ResetBlendingStates;
  case BlendingMode of
    bmOpaque:
      begin
        FBlendEnable:=false;
        FAlphaTestEnable:=false;
      end;
    bmTransparency:
      begin
        FBlendEnable:=True;
        FAlphaTestEnable:=True;
        FSrcBlendFunc:=GL_SRC_ALPHA;
        FDstBlendFunc:=GL_ONE_MINUS_SRC_ALPHA;
        FAlphaFunc:=GL_GREATER;
        FAlphaThreshold:=0;
      end;
    bmAdditive:
      begin
        FBlendEnable:=True;
        FAlphaTestEnable:=True;
        FSrcBlendFunc:=GL_SRC_ALPHA;
        FDstBlendFunc:=GL_ONE;
        FAlphaFunc:=GL_GREATER;
        FAlphaThreshold:=0;
      end;
    bmAlphaTest50:
      begin
        FBlendEnable:=False;
        FAlphaTestEnable:=True;
        FAlphaFunc:=GL_GEQUAL;
        FAlphaThreshold:=0.5;
      end;
    bmAlphaTest100:
      begin
        FBlendEnable:=False;
        FAlphaTestEnable:=True;
        FAlphaFunc:=GL_GEQUAL;
        FAlphaThreshold:=1;
      end;
    bmModulate:
      begin
        FBlendEnable:=True;
        FAlphaTestEnable:=True;
        FSrcBlendFunc:=GL_DST_COLOR;
        FDstBlendFunc:=GL_ZERO;
        FAlphaFunc:=GL_GREATER;
        FAlphaThreshold:=0;
      end;
  end;
end;

procedure TBlendingStates.ResetBlendingStates;
begin
  FBlendEnable:=false;
  FAlphaTestEnable:=false;
  FSrcBlendFunc:=GL_ONE;
  FDstBlendFunc:=GL_ZERO;
  FAlphaFunc:=GL_ALWAYS;
  FAlphaThreshold:=0;
end;

procedure TBlendingStates.unApply;
begin
  OGLStateEmul.GLStateCache.BlendingCache.Reset;
  OGLStateEmul.GLStateCache.AlphaCache.Reset;
end;

{ TDepthStates }

procedure TDepthStates.Apply;
begin
  if FDepthMask then glDepthMask(True) else glDepthMask(False);
  if FDepthTest then glEnable(GL_DEPTH_TEST) else glDisable(GL_DEPTH_TEST);
  glDepthFunc(cDepthFunc[FDepthFunc]);
end;

constructor TDepthStates.Create;
begin
  inherited;
  ResetDepthStates;
end;

destructor TDepthStates.Destroy;
begin
  inherited;
end;

procedure TDepthStates.ResetDepthStates;
begin
  FDepthFunc:=dfLess;
  FDepthMask:=true;
  FDepthTest:=true;
end;

procedure TDepthStates.unApply;
begin
  OGLStateEmul.GLStateCache.DepthCache.Reset;
end;

{ TClipPlanes }

procedure TClipPlanes.Apply;
var i: integer;
begin
  for i:=0 to high(FPlanes) do begin
    if FPlanes[i].Active then begin
      glClipPlane(GL_CLIP_PLANE0+i, @FPlanes[i].PlaneEquation[0]);
      glEnable(GL_CLIP_PLANE0+i);
    end;
  end;
end;

constructor TClipPlanes.Create;
var i: integer;
begin
  inherited;
  if GL_MAX_CLIP_PLANES>65535 then setlength(FPlanes, 65535)
  else setlength(FPlanes, GL_MAX_CLIP_PLANES);
  for i:=0 to high(FPlanes) do FPlanes[i]:=TClipPlane.Create;
end;

destructor TClipPlanes.Destroy;
var i: integer;
begin
  for i:=0 to high(FPlanes) do FPlanes[i].Free;
  inherited;
end;

function TClipPlanes.getClipPlane(Index: integer): TClipPlane;
begin
  assert(index<high(FPlanes),'Index out of bounds!');
  result:=FPlanes[Index];
end;


procedure TClipPlanes.ResetClipPlanes;
var i: integer;
begin
  for i:=0 to high(FPlanes) do with FPlanes[i] do begin
    PlaneEquation[0]:=0; PlaneEquation[1]:=0;
    PlaneEquation[2]:=0; PlaneEquation[3]:=0;
    Active:=false;
  end;
end;

procedure TClipPlanes.UnApply;
var i: integer;
begin
  for i:=0 to high(FPlanes) do glDisable(GL_CLIP_PLANE0+i);
end;

{ TClipPlane }

constructor TClipPlane.Create;
begin
  PlaneEquation[0]:=0; PlaneEquation[1]:=0;
  PlaneEquation[2]:=0; PlaneEquation[3]:=0;
  Active:=false;
end;

{ TPolygonStates }

procedure TPolygonStates.Apply;
begin
  case FFaceMode of
    fmPoints: glPolygonMode(GL_FRONT_AND_BACK, GL_POINT);
    fmLines: glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    fmFill: glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  end;
  case FFaceWinding of
    fwCW: glFrontFace(GL_CW);
    fwCCW: glFrontFace(GL_CCW);
  end;
  if FFaceCull then glEnable(GL_CULL_FACE) else glDisable(GL_CULL_FACE);

end;

constructor TPolygonStates.Create;
begin
  inherited;
  FFaceMode:= fmFill; FFaceCull:=true; FFaceWinding:=fwCCW;
end;

procedure TPolygonStates.ResetPolygonStates;
begin
  FFaceMode:= fmFill; FFaceCull:=true; FFaceWinding:=fwCCW;
end;

procedure TPolygonStates.UnApply;
begin
  glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  glFrontFace(GL_CCW);
  glEnable(GL_CULL_FACE)
end;

{ TRenderStates }

procedure TRenderStates.ApplyStates;
begin
  FBlendingStates.Apply;
  FDepthStates.Apply;
  FClipPlanes.Apply;
  FPolygonStates.Apply;
end;

constructor TRenderStates.Create;
begin
  inherited;
  FBlendingStates:=TBlendingStates.Create;
  FDepthStates:=TDepthStates.Create;
  FClipPlanes:=TClipPlanes.Create;
  FPolygonStates:=TPolygonStates.Create;
end;

destructor TRenderStates.Destroy;
begin
  FBlendingStates.Free;
  FDepthStates.Free;
  FClipPlanes.Free;
  FPolygonStates.Free;

  inherited;
end;

procedure TRenderStates.ResetStates;
begin
  FBlendingStates.ResetBlendingStates;
  FDepthStates.ResetDepthStates;
  FClipPlanes.ResetClipPlanes;
  FPolygonStates.ResetPolygonStates;
end;

procedure TRenderStates.UnApplyStates;
begin
  FBlendingStates.unApply;
  FDepthStates.unApply;
  FClipPlanes.UnApply;
  FPolygonStates.UnApply;
end;

end.