unit uLogs;

interface

uses SysUtilsLite, Classes;

var vLog: TFileStream;

procedure WriteToLog(s: ansistring; br: boolean = true); overload;
procedure WriteToLog(v: integer; br: boolean = true); overload;
//procedure WriteToLog(v: double; br: boolean = true); overload;
procedure WriteToLog(b: boolean; br: boolean = true); overload;

implementation

const
  cBreakLine: array[false..true] of string = ('',#13+#10);

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
     vLog:=TFileStream.Create('log.txt',fmCreate);
  {$ENDIF}
end;

finalization
begin
  vLog.Free;
end;

end.
