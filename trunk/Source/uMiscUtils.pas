unit uMiscUtils;

interface

uses Classes, SysUtils, Contnrs, VectorTypes;
const
   BufSize = 10240; { Load input data in chunks of BufSize Bytes. }
   LineLen = 100;   { Allocate memory for the current line in chunks
                      of LineLen Bytes. }
Type
  TTextFileParser = class
  private
    FSourceStream : TStream;     { Load from this stream }
    FBuffer: AnsiString;         { Buffer }
    FLine : String;              { current line }
    FLineNo : Integer;           { current Line number - for error messages }
    FEof : Boolean;              { Stream done? }
    FBufPos : Integer;           { Position in the buffer }
    FStreamOwner: boolean;       { Stream owner? }
    function StreamEOF(S: TStream): Boolean;
    function Rest(const s:string; Count: integer):string;
    procedure FillBuffer;
    procedure LoadFromStream(aStream: TStream);
    procedure LoadFromFile(FileName: string);
  public
    constructor Create(aStream: TStream); overload;
    constructor Create(FileName: string); overload;
    destructor Destroy; override;
    // Read a single line of text from the source stream, set FEof to true when done.
    function ReadLine: string;
    function NextToken(var s: String; delimiter: Char) : String;
    function NextTokenAsFloat(var s: String; delimiter: Char) : single;
    function NextTokenAsInt(var s: String; delimiter: Char) : integer;
    function NextTokenAsQuotedString(var s: string; const Quote: string='"'): string;

    property EoF: boolean read FEof;
    property LineNo: integer read FLineNo;
  end;

{ TBits class }

  TIntegerBits = class
  private
    FSize: Integer;
    FFullMemSize: integer;
    FBits: Pointer;
    procedure Error;
    procedure SetSize(Value: Integer);
    procedure SetBit(Index: Integer; Value: Boolean);
    function GetBit(Index: Integer): Boolean;
  public
    destructor Destroy; override;
    function OpenBit: Integer;
    procedure ResetBits;
    property Bits[Index: Integer]: Boolean read GetBit write SetBit; default;
    property Size: Integer read FSize write SetSize;
  end;

function StringHashKey(const name: string): Integer;
function BufferHash(const Buffer; Count: integer): word;
function HashKey(const v : TVector4f; hashSize : Integer) : Integer;
function GetHashFromBuff(const Buffer; Count: Integer): Word; assembler;

function CutString(const s: string; l: integer): string;

function min(a,b: integer): integer; overload;
function min(a,b: single): single; overload;
function max(a,b: integer): integer; overload;
function max(a,b: single): single; overload;

procedure FreeList(var List: TList);
procedure FreeObjectList(var List: TList);overload;
procedure FreeObjectList(var List: TObjectList);overload;
procedure FreeAndNil(var Obj);

function StrToInt(s:string):integer;
function StrToFloat(s:string):Single;
function FloatToStr(x: double; width: byte=10; decimals: byte=6): string;
function IntToStr(x:integer):string;

implementation

function BufferHash(const Buffer; Count: integer): word; assembler;
asm
        MOV     ECX,EDX
        MOV     EDX,EAX
        XOR     EAX,EAX
@@1:    ROL     AX,5
        XOR     AL,[EDX]
        INC     EDX
        DEC     ECX
        JNE     @@1
end;

function min(a,b: integer): integer; overload;
begin if a<b then result:=a else result:=b; end;

function max(a,b: integer): integer; overload;
begin if a>b then result:=a else result:=b; end;

function min(a,b: single): single; overload;
begin if a<b then result:=a else result:=b; end;

function max(a,b: single): single; overload;
begin if a>b then result:=a else result:=b; end;


function CutString(const s: string; l: integer): string;
begin
  result:=copy(s,1,min(length(s),l));
end;

procedure FreeList(var List: TList);
var i: integer;
    p: pointer;
begin
  if not assigned(List) then exit;
  for i:=0 to List.Count-1 do begin
    p:=List[i]; if assigned(p) then Dispose(p);
  end; List.Free; List:=nil;
end;

procedure FreeObjectList(var List: TList);overload;
var i: integer;
    p: TObject;
begin
  if not assigned(List) then exit;
  for i:=0 to List.Count-1 do begin
    p:=List[i]; if assigned(p) then FreeAndNil(p);
  end; FreeAndNil(List);
end;

procedure FreeObjectList(var List: TObjectList);overload;
var i: integer;
    p: TObject;
begin
  if not assigned(List) then exit;
  for i:=0 to List.Count-1 do begin
    p:=List[i]; if assigned(p) then FreeAndNil(p);
  end; FreeAndNil(List);
end;

procedure FreeAndNil(var Obj);
var Temp: TObject;
begin
  Temp := TObject(Obj);
  Pointer(Obj) := nil;
  if assigned(Temp) then Temp.Free;
end;

function StrToInt(s:string):integer;
var code:integer;
begin
  val(s,result,code);
end;
function StrToFloat(s:string):Single;
var code:integer;
begin
  val(s,result,code);
end;
function FloatToStr(x: double; width: byte=10; decimals: byte=6): string;
begin
  str(x:width:decimals,result);
end;
function IntToStr(x:integer):string;
var s: ansistring;
begin
   str(x,s); result:=string(s);
end;

function HashKey(const v : TVector4f; hashSize : Integer) : Integer;
begin
  Result:=((Integer(@v[0])
            xor Integer(@v[1])
            xor Integer(@v[2])
            xor Integer(@v[3])) shr 16) and hashSize;
end;

function StringHashKey(const name: string): Integer;
var i, n, res: Integer;
begin
  if name='' then result:=-1 else begin
    n := Length(name); Res := n;
    for i := 1 to n do
        Res := (Res shl 1) + Byte(name[i]);
    result:=res;
  end;
end;

function GetHashFromBuff(const Buffer; Count: Integer): Word; assembler;
asm
        MOV     ECX,EDX
        MOV     EDX,EAX
        XOR     EAX,EAX
@@1:    ROL     AX,5
        XOR     AL,[EDX]
        INC     EDX
        DEC     ECX
        JNE     @@1
end;


{ TTextFileParser }

// StreamEOF
//
function TTextFileParser.StreamEOF(S : TStream) : Boolean;
begin
  { Is the stream at its end? }
  if not assigned(S) then result:=true
  else Result:=(S.Position>=S.Size);
end;

function TTextFileParser.ReadLine: string;
var j : Integer;
begin
  Inc(FLineNo); j:=1; Result:='';
  if FBufPos<1 then FillBuffer;
  while True do begin
    if FBufPos>Length(FBuffer) then begin
      if StreamEof(FSourceStream) then begin
        FEof:=True; break; end else FillBuffer;
    end else begin
      case FBuffer[FBufPos] of
        #10, #13 : begin
          Inc(FBufPos);
          if FBufPos>Length(FBuffer) then
            if StreamEof(FSourceStream) then break else FillBuffer;
            if (FBuffer[FBufPos]=#10) or (FBuffer[FBufPos]=#13) then Inc(FBufPos);
          break;
        end;
      else
        if j>Length(FLine) then SetLength(FLine, Length(FLine)+LineLen);
        if FBuffer[FBufPos]=#9 then FLine[j]:=#32 else FLine[j]:=Char(FBuffer[FBufPos]);
        Inc(FBufPos); Inc(j);
      end;
    end;
  end;
  SetLength(FLine,j-1); result:=FLine;
end;

function TTextFileParser.Rest(const s:string; Count:integer):string;
{ Return the right part of s including s[Count]. }
begin
  Result:=copy(s,Count,Length(s)-Count+1);
end;

// NextToken
//
function TTextFileParser.NextToken(var s : String; delimiter : Char) : String;
{ Return the next Delimiter-delimited Token from the string s and
  remove it from s }
var p: Integer;
begin
   p:=Pos(Delimiter, s);
   if p=0 then begin Result:=s; s:=''; end else begin
     Result:=copy(s, 1, p-1); s:=TrimLeft(Rest(s, p+1));
   end;
end;

constructor TTextFileParser.Create(aStream: TStream);
begin
  inherited Create;
  FStreamOwner:=false;
  LoadFromStream(aStream);
end;

constructor TTextFileParser.Create(FileName: string);
begin
  inherited Create;
  LoadFromFile(FileName);
end;

destructor TTextFileParser.Destroy;
begin
  if FStreamOwner then FSourceStream.Free;
  inherited;
end;

procedure TTextFileParser.FillBuffer;
var l : Integer;
begin
   l:=FSourceStream.Size-FSourceStream.Position;
   if l>BufSize then l:=BufSize;
   SetLength(FBuffer, l);
   FSourceStream.Read(FBuffer[1], l); FBufPos:=1;
end;

procedure TTextFileParser.LoadFromFile(FileName: string);
var S: TStream;
begin
  S:=TFileStream.Create(FileName,fmOpenRead);
  FStreamOwner:=true; LoadFromStream(S);
end;

procedure TTextFileParser.LoadFromStream(aStream: TStream);
begin
  if assigned(FSourceStream) and (FStreamOwner) then FSourceStream.Free;
  FEof:=False; FSourceStream:=aStream; FLineNo:=0;
end;

function TTextFileParser.NextTokenAsFloat(var s: String;
  delimiter: Char): single;
var x: string;
begin
  x:=NextToken(s,delimiter); result:=strtofloat(x);
end;

function TTextFileParser.NextTokenAsInt(var s: String;
  delimiter: Char): integer;
var x: string;
begin
  x:=NextToken(s,delimiter); result:=strtoint(x);
end;

function TTextFileParser.NextTokenAsQuotedString(var s: string;
  const Quote: string): string;
var p,n: integer;
    pc: PChar;
begin
  pc:=PChar(Quote);
  if StrScan(PChar(s),Quote[1])=nil then result:='' else begin
    p:=pos(Quote[1],s);
    n:=1; while (p+n<length(s)) and (StrScan(pc,s[p+n])=nil) do inc(n);
    if StrScan(pc,s[n])=nil then dec(n);
    result:=copy(s,2,n); s:=trimleft(Rest(s,p+n+1));
  end;
end;

{ TBits }

const
  BitsPerInt = SizeOf(Integer) * 8;

type
  TBitEnum = 0..BitsPerInt - 1;
  TBitSet = set of TBitEnum;
  PBitArray = ^TBitArray;
  TBitArray = array[0..4096] of TBitSet;

destructor TIntegerBits.Destroy;
begin
  SetSize(0);
  inherited Destroy;
end;

procedure TIntegerBits.Error;
var SBitsIndexError: string;
begin
  SBitsIndexError:='Bits index out of range';
  raise EBitsError.CreateRes(@SBitsIndexError);
end;

procedure TIntegerBits.SetSize(Value: Integer);
var
  NewMem: Pointer;
  NewMemSize: Integer;
  OldMemSize: Integer;

  function Min(X, Y: Integer): Integer;
  begin
    Result := X;
    if X > Y then Result := Y;
  end;

begin
  if Value <> Size then
  begin
    if Value < 0 then Error;
    NewMemSize := ((Value + BitsPerInt - 1) div BitsPerInt) * SizeOf(Integer);
    OldMemSize := ((Size + BitsPerInt - 1) div BitsPerInt) * SizeOf(Integer);
    if NewMemSize <> OldMemSize then
    begin
      NewMem := nil;
      if NewMemSize <> 0 then
      begin
        GetMem(NewMem, NewMemSize);
        FillChar(NewMem^, NewMemSize, 0);
      end;
      if OldMemSize <> 0 then
      begin
        if NewMem <> nil then
          Move(FBits^, NewMem^, Min(OldMemSize, NewMemSize));
        FreeMem(FBits, OldMemSize);
      end;
      FBits := NewMem;
    end;
    FSize := Value; FFullMemSize := NewMemSize;
  end;
end;

procedure TIntegerBits.SetBit(Index: Integer; Value: Boolean); assembler;
asm
        CMP     Index,[EAX].FSize
        JAE     @@Size

@@1:    MOV     EAX,[EAX].FBits
        OR      Value,Value
        JZ      @@2
        BTS     [EAX],Index
        RET

@@2:    BTR     [EAX],Index
        RET

@@Size: CMP     Index,0
        JL      TBits.Error
        PUSH    Self
        PUSH    Index
        PUSH    ECX {Value}
        INC     Index
        CALL    TBits.SetSize
        POP     ECX {Value}
        POP     Index
        POP     Self
        JMP     @@1
end;

function TIntegerBits.GetBit(Index: Integer): Boolean; assembler;
asm
        CMP     Index,[EAX].FSize
        JAE     TBits.Error
        MOV     EAX,[EAX].FBits
        BT      [EAX],Index
        SBB     EAX,EAX
        AND     EAX,1
end;

function TIntegerBits.OpenBit: Integer;
var
  I: Integer;
  B: TBitSet;
  J: TBitEnum;
  E: Integer;
begin
  E := (Size + BitsPerInt - 1) div BitsPerInt - 1;
  for I := 0 to E do
    if PBitArray(FBits)^[I] <> [0..BitsPerInt - 1] then
    begin
      B := PBitArray(FBits)^[I];
      for J := Low(J) to High(J) do
      begin
        if not (J in B) then
        begin
          Result := I * BitsPerInt + J;
          if Result >= Size then Result := Size;
          Exit;
        end;
      end;
    end;
  Result := Size;
end;

procedure TIntegerBits.ResetBits;
begin
  if FSize>0 then FillChar(FBits^, FFullMemSize, 0);
end;

procedure QuickSort(var A: array of Integer);
  procedure Sort(var A: array of Integer; iLo, iHi: Integer);
  var Lo, Hi, Mid, T: Integer;
  begin
    Lo := iLo; Hi := iHi;
    Mid := A[(Lo + Hi) div 2];
    repeat
      while A[Lo] < Mid do Inc(Lo);
      while A[Hi] > Mid do Dec(Hi);
      if Lo <= Hi then
      begin
        T := A[Lo]; A[Lo] := A[Hi]; A[Hi] := T;
        Inc(Lo); Dec(Hi);
      end;
    until Lo > Hi;
    if Hi > iLo then Sort(A, iLo, Hi);
    if Lo < iHi then Sort(A, Lo, iHi);
  end;
begin
  Sort(A, Low(A), High(A));
end;

end.
