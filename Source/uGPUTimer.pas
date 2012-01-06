// Simple GPU timer class that allows the timing of rendering without
// idling the hardware.
//

unit uGPUTimer;

interface

uses OpenGL1x;

//
//  Simple timer class using GL_EXT_timer_query. Used to track
// rendering time used on the GPU.
//
//////////////////////////////////////////////////////////////////////
type

  TGPUtimer = class
  private
    Ftimers: array of cardinal;
    FTimersCount: integer;
    FactiveTimer: integer;
  public
    constructor Create(TimersCount: integer=100);
    destructor Destroy; override;

    // begin the query, must not have nested timer queries
    procedure startTimer;

    // stop the query and advance to the next one as active
    procedure stopTimer;

    // return the most recent available timer result
    function getLatestTime: double;
  end;

implementation

{ TGPUtimer }

constructor TGPUtimer.Create(TimersCount: integer);
begin
  setlength(FTimers,TimersCount);
  FTimersCount:=TimersCount;
  glGenQueries( FTimersCount, @Ftimers[0]);
  FactiveTimer := 0;
end;

destructor TGPUtimer.Destroy;
begin
  glDeleteQueries(FTimersCount,@FTimers[0]);
  inherited;
end;

function TGPUtimer.getLatestTime: double;
var
  testNum: integer;
  elapsedTime: GLInt64Ext;
  queryReady: GLInt;
begin
  testNum := FactiveTimer;
  elapsedTime := 0;
  queryReady := 0;

  repeat
    testNum := ( testNum + FTimersCount-1) mod FTimersCount;
    glGetQueryObjectiv( Ftimers[testNum], GL_QUERY_RESULT_AVAILABLE, @queryReady);
  until (queryReady>0);

  glGetQueryObjecti64vEXT( Ftimers[testNum], GL_QUERY_RESULT, @elapsedTime);

  result:=elapsedTime / 1000000.0; //convert to milliseconds
end;

procedure TGPUtimer.startTimer;
begin
  glBeginQuery( GL_TIME_ELAPSED_EXT, Ftimers[FactiveTimer]);
end;

procedure TGPUtimer.stopTimer;
begin
  glEndQuery( GL_TIME_ELAPSED_EXT);
  FactiveTimer := (FactiveTimer + 1) mod FTimersCount;
end;

end.

