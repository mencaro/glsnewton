unit uLogs;

interface

uses SysUtilsLite, Classes, Windows;

var vLog: TFileStream;

procedure WriteToLog(s: ansistring; br: boolean = true); overload;
procedure WriteToLog(v: integer; br: boolean = true); overload;
//procedure WriteToLog(v: double; br: boolean = true); overload;
procedure WriteToLog(b: boolean; br: boolean = true); overload;
function GetTime:double;

implementation

const
  cBreakLine: array[false..true] of string = ('',#13+#10);

function GetTime:double;
var Freq, Tick: Int64;
begin
  QueryPerformanceFrequency(Freq);
  QueryPerformanceCounter(Tick);
  Result:=Tick/Freq;
end;

procedure WriteToLog(s: ansistring; br: boolean = true); overload;
var x: pchar;
    t: ansistring;
begin
  t:=s+cBreakLine[br];
  x:=pchar(t);
  vlog.Write(x^, length(t));
end;

procedure WriteToLog(v: integer; br: boolean = true); overload;
var s: string;
begin
  str(v,s); WriteToLog(s,br);
end;

procedure WriteToLog(v: double; br: boolean = true); overload;
var s: string;
begin
  str(v,s); WriteToLog(s,br);
end;

procedure WriteToLog(b: boolean; br: boolean = true); overload;
const bool: array[false..true] of string = ('False', 'True');
begin
  WriteToLog(bool[b],br);
end;

initialization
begin
  {$IFDEF Logging}
     vLog:=TFileStream.Create('log.txt',fmCreate); exit;
  {$ENDIF}
end;

finalization
begin
  {$IFDEF Logging}
    vLog.Free;
  {$ENDIF}
end;

end.
