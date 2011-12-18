unit SysUtilsLite;

interface
Uses Windows, StrUtils, TypInfo;
const
  PathDelim  = {$IFDEF MSWINDOWS} '\'; {$ELSE} '/'; {$ENDIF}
  DriveDelim = {$IFDEF MSWINDOWS} ':'; {$ELSE} '';  {$ENDIF}
  PathSep    = {$IFDEF MSWINDOWS} ';'; {$ELSE} ':'; {$ENDIF}
{ File open modes }

{$IFDEF LINUX}
  fmOpenRead       = O_RDONLY;
  fmOpenWrite      = O_WRONLY;
  fmOpenReadWrite  = O_RDWR;
//  fmShareCompat not supported
  fmShareExclusive = $0010;
  fmShareDenyWrite = $0020;
//  fmShareDenyRead  not supported
  fmShareDenyNone  = $0030;
{$ENDIF}
{$IFDEF MSWINDOWS}
  fmOpenRead       = $0000;
  fmOpenWrite      = $0001;
  fmOpenReadWrite  = $0002;

  fmShareCompat    = $0000 platform; // DOS compatibility mode is not portable
  fmShareExclusive = $0010;
  fmShareDenyWrite = $0020;
  fmShareDenyRead  = $0030 platform; // write-only not supported on all platforms
  fmShareDenyNone  = $0040;
{$ENDIF}

type
  TReplaceFlags = set of (rfReplaceAll, rfIgnoreCase);
  LongRec = packed record
    case Integer of
      0: (Lo, Hi: Word);
      1: (Words: array [0..1] of Word);
      2: (Bytes: array [0..3] of Byte);
  end;

  PByteArray = ^TByteArray;
  TByteArray = array[0..32767] of Byte;

function StringReplace(const S, OldPattern, NewPattern: string;
  Flags: TReplaceFlags): string;
function FloatToStr(Value: Extended): string;overload;
function FloatToStr(Value: Extended; width, decimals: integer): string;overload;
function TryStrToFloat(const strValue : String; var val : Extended): Boolean;
function StrToFloatDef(const strValue : String; defValue : Extended = 0) : Extended;
function IntToStr(Value: Integer): string;
function StrToInt(const S: string): Integer;
function StrToIntDef(const S: string; Default: Integer): Integer;
function Trim(const S: WideString): WideString; overload;
function Trim(const S: AnsiString): AnsiString; overload;
function TrimLeft(const S: string): string; overload;
function TrimLeft(const S: AnsiString): AnsiString; overload;
function CropString(const s: ansistring; left,right: integer): ansistring;

function UpperCase(const S: string): string;
function LowerCase(const S: string): string;
function StrScan(const Str: PChar; Chr: Char): PChar;
function CompareStr(const S1, S2: string): Integer;
function CompareMem(P1, P2: Pointer; Length: Integer): Boolean;
function LastDelimiter(const Delimiters, S: string): Integer;
function ExtractFilePath(const FileName: string): string;
function ExtractFileName(const FileName: string): string;
function ExtractFileExt(const FileName: string): string;
function FileAge(const FileName: string): Integer;
function FileExists(const FileName: string): Boolean;
function ChangeFileExt(const FileName, Extension: string): string;

procedure CopyComponentProp(Source, Target: TObject; aExcept: array of string);
procedure AssignComponentProp(Source, Target: TObject; aProp: array of string);

implementation

function FloatToStr(Value: Extended): string; overload;
begin
  str(Value, result);
end;
function FloatToStr(Value: Extended; Width, Decimals: integer): string; overload;
begin
  str(Value:Width:Decimals, result);
end;

function IntToStr(Value: Integer): string;
begin
  str(Value, result);
end;


function StrToInt(const S: string): Integer;
var E: Integer;
begin
  Val(S, Result, E);
end;

function StrToIntDef(const S: string; Default: Integer): Integer;
var E: Integer;
begin
  Val(S, Result, E);
  if E <> 0 then Result := Default;
end;

function Trim(const S: WideString): WideString;
var I, L: Integer;
begin
  L := Length(S); I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  if I > L then Result := '' else
  begin
    while S[L] <= ' ' do Dec(L);
    Result := Copy(S, I, L - I + 1);
  end;
end;

function Trim(const S: AnsiString): AnsiString; overload;
var I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  if I > L then Result := '' else
  begin
    while S[L] <= ' ' do Dec(L);
    Result := Copy(S, I, L - I + 1);
  end;
end;

function TrimLeft(const S: string): string; overload;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  Result := Copy(S, I, Maxint);
end;

function TrimLeft(const S: AnsiString): AnsiString; overload;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  Result := Copy(S, I, Maxint);
end;

function CropString(const s: ansistring; left,right: integer): ansistring;
begin
  result:=copy(s,left+1,length(s)-right-left);
end;

function StringReplace(const S, OldPattern, NewPattern: string;
  Flags: TReplaceFlags): string;
var
  SearchStr, Patt, NewStr: string;
  Offset: Integer;
begin
  if rfIgnoreCase in Flags then
  begin
    SearchStr := UpperCase(S);
    Patt := UpperCase(OldPattern);
  end else
  begin
    SearchStr := S;
    Patt := OldPattern;
  end;
  NewStr := S;
  Result := '';
  while SearchStr <> '' do
  begin
    Offset := Pos(Patt, SearchStr);
    if Offset = 0 then
    begin
      Result := Result + NewStr;
      Break;
    end;
    Result := Result + Copy(NewStr, 1, Offset - 1) + NewPattern;
    NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);
    if not (rfReplaceAll in Flags) then
    begin
      Result := Result + NewStr;
      Break;
    end;
    SearchStr := Copy(SearchStr, Offset + Length(Patt), MaxInt);
  end;
end;

function UpperCase(const S: string): string;
var
  Ch: Char;
  L: Integer;
  Source, Dest: PChar;
begin
  L := Length(S);
  SetLength(Result, L);
  Source := Pointer(S);
  Dest := Pointer(Result);
  while L <> 0 do
  begin
    Ch := Source^;
    if (Ch >= 'a') and (Ch <= 'z') then Dec(Ch, 32);
    Dest^ := Ch;
    Inc(Source);
    Inc(Dest);
    Dec(L);
  end;
end;

function LowerCase(const S: string): string;
var
  Ch: Char;
  L: Integer;
  Source, Dest: PChar;
begin
  L := Length(S);
  SetLength(Result, L);
  Source := Pointer(S);
  Dest := Pointer(Result);
  while L <> 0 do
  begin
    Ch := Source^;
    if (Ch >= 'A') and (Ch <= 'Z') then Inc(Ch, 32);
    Dest^ := Ch;
    Inc(Source);
    Inc(Dest);
    Dec(L);
  end;
end;

function StrScan(const Str: PChar; Chr: Char): PChar;
begin
  Result := Str;
  while Result^ <> #0 do
  begin
    if Result^ = Chr then
      Exit;
    Inc(Result);
  end;
  if Chr <> #0 then
    Result := nil;
end;

function LastDelimiter(const Delimiters, S: string): Integer;
var
  P: PChar;
begin
  Result := Length(S);
  P := PChar(Delimiters);
  while Result > 0 do
  begin
    if (S[Result] <> #0) and (StrScan(P, S[Result]) <> nil) then Exit;
    Dec(Result);
  end;
end;

function ExtractFilePath(const FileName: string): string;
var
  I: Integer;
begin
  I := LastDelimiter(PathDelim + DriveDelim, FileName);
  Result := Copy(FileName, 1, I);
end;

function ExtractFileName(const FileName: string): string;
var
  I: Integer;
begin
  I := LastDelimiter(PathDelim + DriveDelim, FileName);
  Result := Copy(FileName, I + 1, MaxInt);
end;

function ExtractFileExt(const FileName: string): string;
var
  I: Integer;
begin
  I := LastDelimiter('.' + PathDelim + DriveDelim, FileName);
  if (I > 0) and (FileName[I] = '.') then
    Result := Copy(FileName, I, MaxInt) else
    Result := '';
end;

function FileAge(const FileName: string): Integer;
{$IFDEF MSWINDOWS}
var
  Handle: THandle;
  FindData: TWin32FindData;
  LocalFileTime: TFileTime;
begin
  Handle := FindFirstFile(PChar(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
    begin
      FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFileTime);
      if FileTimeToDosDateTime(LocalFileTime, LongRec(Result).Hi,
        LongRec(Result).Lo) then Exit;
    end;
  end;
  Result := -1;
end;
{$ENDIF}
{$IFDEF LINUX}
var
  st: TStatBuf;
begin
  if stat(PChar(FileName), st) = 0 then
    Result := st.st_mtime
  else
    Result := -1;
end;
{$ENDIF}

function FileExists(const FileName: string): Boolean;
{$IFDEF MSWINDOWS}
begin
  Result := FileAge(FileName) <> -1;
end;
{$ENDIF}
{$IFDEF LINUX}
begin
  Result := euidaccess(PChar(FileName), F_OK) = 0;
end;
{$ENDIF}

function ChangeFileExt(const FileName, Extension: string): string;
var
  I: Integer;
begin
  I := LastDelimiter('.' + PathDelim + DriveDelim,Filename);
  if (I = 0) or (FileName[I] <> '.') then I := MaxInt;
  Result := Copy(FileName, 1, I - 1) + Extension;
end;

function CompareStr(const S1, S2: string): Integer; assembler;
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,EAX
        MOV     EDI,EDX
        OR      EAX,EAX
        JE      @@1
        MOV     EAX,[EAX-4]
@@1:    OR      EDX,EDX
        JE      @@2
        MOV     EDX,[EDX-4]
@@2:    MOV     ECX,EAX
        CMP     ECX,EDX
        JBE     @@3
        MOV     ECX,EDX
@@3:    CMP     ECX,ECX
        REPE    CMPSB
        JE      @@4
        MOVZX   EAX,BYTE PTR [ESI-1]
        MOVZX   EDX,BYTE PTR [EDI-1]
@@4:    SUB     EAX,EDX
        POP     EDI
        POP     ESI
end;

function CompareMem(P1, P2: Pointer; Length: Integer): Boolean; assembler;
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,P1
        MOV     EDI,P2
        MOV     EDX,ECX
        XOR     EAX,EAX
        AND     EDX,3
        SHR     ECX,1
        SHR     ECX,1
        REPE    CMPSD
        JNE     @@2
        MOV     ECX,EDX
        REPE    CMPSB
        JNE     @@2
        INC     EAX
@@2:    POP     EDI
        POP     ESI
end;


procedure CopyComponentProp(Source, Target: TObject; aExcept: array of string);
// Копирование всех одинаковых по названию свойств/методов одного компонента в
// другой за исключение "Name", "Left", "Top" и тех которые заданы в aExcept
// Примеры использования:
// CopyComponentProp(N11, N21, []);
// CopyComponentProp(ListBox2, ListBox3, []);
// CopyComponentProp(ListView1, ListView2, ['Items', 'Color']);
var
  I, Index: Integer;
  PropName: string;
  Source_PropList  , Target_PropList  : PPropList;
  Source_NumProps  , Target_NumProps  : Word;
  Source_PropObject, Target_PropObject: TObject;
  // Поиск в списке свойства с заданным именем
  function FindProperty(const PropName: string; PropList: PPropList; NumProps: Word): Integer;
  var
    I: Integer;
  begin
    Result:= -1;
    for I:= 0 to NumProps - 1 do
      if CompareStr(PropList^[I]^.Name, PropName) = 0 then begin
        Result:= I;
        Break;
      end;
  end;
begin
  if not Assigned(Source) or not Assigned(Target) then Exit;
  Source_NumProps:= GetTypeData(Source.ClassInfo)^.PropCount;
  Target_NumProps:= GetTypeData(Target.ClassInfo)^.PropCount;
  GetMem(Source_PropList, Source_NumProps * SizeOf(Pointer));
  GetMem(Target_PropList, Target_NumProps * SizeOf(Pointer));
  try
    // Получаем список свойств
    GetPropInfos(Source.ClassInfo, Source_PropList);
    GetPropInfos(Target.ClassInfo, Target_PropList);
    for I:= 0 to Source_NumProps - 1 do begin
      PropName:= Source_PropList^[I]^.Name;
      if  (AnsiIndexText('None'  , aExcept                ) =  -1) and
         ((AnsiIndexText(PropName, ['Name', 'Left', 'Top']) <> -1) or
          (AnsiIndexText(PropName, aExcept                ) <> -1)) then Continue;
      Index:= FindProperty(PropName, Target_PropList, Target_NumProps);
      if Index = -1 then Continue; // не нашли
      // Проверить совпадение типов
      if Source_PropList^[I]^.PropType^.Kind <> Target_PropList^[Index]^.PropType^.Kind then
        Continue;
      case Source_PropList^[I]^.PropType^^.Kind of
        tkClass:  begin
                    Source_PropObject:= GetObjectProp(Source, Source_PropList^[I    ]);
                    Target_PropObject:= GetObjectProp(Target, Target_PropList^[Index]);
                    CopyComponentProp(Source_PropObject, Target_PropObject, ['None']);
                  end;
        tkMethod: SetMethodProp(Target, PropName, GetMethodProp(Source, PropName));
      else
        SetPropValue(Target, PropName, GetPropValue(Source, PropName));
      end;
    end;
  finally
    FreeMem(Source_PropList);
    FreeMem(Target_PropList);
  end;
end;

procedure AssignComponentProp(Source, Target: TObject; aProp: array of string);
// Копирование свойств/методов заданых в aProp одного компонента в другой
// Пример использования:
// AssignedComponentProp(ListView1, ListView2, ['Items', 'Color']);
var
  I, Index: Integer;
  PropName: string;
  Source_PropList  , Target_PropList  : PPropList;
  Source_NumProps  , Target_NumProps  : Word;
  Source_PropObject, Target_PropObject: TObject;
  // Поиск в списке свойства с заданным именем
  function FindProperty(const PropName: string; PropList: PPropList; NumProps: Word): Integer;
  var
    I: Integer;
  begin
    Result:= -1;
    for I:= 0 to NumProps - 1 do
      if CompareStr(PropList^[I]^.Name, PropName) = 0 then begin
        Result:= I;
        Break;
      end;
  end;
begin
  if not Assigned(Source) or not Assigned(Target) then Exit;
  Source_NumProps:= GetTypeData(Source.ClassInfo)^.PropCount;
  Target_NumProps:= GetTypeData(Target.ClassInfo)^.PropCount;
  GetMem(Source_PropList, Source_NumProps * SizeOf(Pointer));
  GetMem(Target_PropList, Target_NumProps * SizeOf(Pointer));
  try
    // Получаем список свойств
    GetPropInfos(Source.ClassInfo, Source_PropList);
    GetPropInfos(Target.ClassInfo, Target_PropList);
    for I:= 0 to Source_NumProps - 1 do begin
      PropName:= Source_PropList^[I]^.Name;
      if (AnsiIndexText('None'  , aProp   ) = -1) and
         (AnsiIndexText(PropName, aProp   ) = -1) then Continue;
      Index:= FindProperty(PropName, Target_PropList, Target_NumProps);
      if Index = -1 then Continue; // не нашли
      // Проверить совпадение типов
      if Source_PropList^[I]^.PropType^.Kind <> Target_PropList^[Index]^.PropType^.Kind then
        Continue;
      case Source_PropList^[I]^.PropType^^.Kind of
        tkClass:  begin
                    Source_PropObject:= GetObjectProp(Source, Source_PropList^[I    ]);
                    Target_PropObject:= GetObjectProp(Target, Target_PropList^[Index]);
                    AssignComponentProp(Source_PropObject, Target_PropObject, ['None']);
                  end;
        tkMethod: SetMethodProp(Target, PropName, GetMethodProp(Source, PropName));
      else
        SetPropValue(Target, PropName, GetPropValue(Source, PropName));
      end;
    end;
  finally
    FreeMem(Source_PropList);
    FreeMem(Target_PropList);
  end;
end;

// TryStrToFloat
//
function TryStrToFloat(const strValue : String; var val : Extended): Boolean;
var
   i, j, divider, lLen, exponent : Integer;
   c : Char;
   v : Extended;
begin
   if strValue='' then begin
      Result:=False;
      Exit;
   end else v:=0;
   lLen:=Length(strValue);
   while (lLen>0) and (strValue[lLen]=' ') do Dec(lLen);
   divider:=lLen+1;
   exponent:=0;
   for i:=1 to lLen do begin
      c:=strValue[i];
      case c of
         ' ' : if v<>0 then begin
            Result:=False;
            Exit;
         end;
         '0'..'9' : v:=(v*10)+Integer(c)-Integer('0');
         ',', '.' : begin
            if (divider>lLen) then
               divider:=i+1
            else begin
               Result:=False;
               Exit;
            end;
         end;
         '-', '+' : if i>1 then begin
            Result:=False;
            Exit;
         end;
         'e', 'E' : begin
            if i+1>lLen then begin
               Result:=False;
               Exit;
            end;
            for j:=i+1 to lLen do begin
               c:=strValue[j];
               case c of
                  '-', '+' : if j<>i+1 then begin
         				Result:=False;
                     Exit;
                  end;
                  '0'..'9' : exponent:=(exponent*10)+Integer(c)-Integer('0');
               else
                  Result:=False;
                  Exit;
               end;
            end;
            if strValue[i+1]<>'-' then
               exponent:=-exponent;
            exponent:=exponent-1;
            lLen:=i;
            if divider>lLen then
               divider:=lLen;
            Break;
         end;
		else
         Result:=False;
         Exit;
      end;
   end;
   divider:=lLen-divider+exponent+1;
   if strValue[1]='-' then begin
      v:=-v;
   end;
   if divider<>0 then
      v:=v*Exp(-divider*Ln(10));
   val:=v;
   Result:=True;
end;

// StrToFloatDef
//
function StrToFloatDef(const strValue : String; defValue : Extended = 0) : Extended;
begin
   if not TryStrToFloat(strValue, Result) then
      result:=defValue;
end;

end.
(*
function VectorToString(V: TVector): string; overload;
begin
  result:='( '+floattostr(v[0])+'; '+floattostr(v[1])+'; '+floattostr(v[2])+' )';
end;

function VectorToString(V: TAffineVector; Width: integer = 8; Decimals: integer = 4 ): string; overload;
var s: string;
begin
{  str(v[0]:Width:Decimals,s); result:='( '+s+'; ';
  str(v[1]:Width:Decimals,s); result:=result+s+'; ';
  str(v[2]:Width:Decimals,s); result:=result+s+')';}
  str(v[0],s); result:='( '+s+'; ';
  str(v[1],s); result:=result+s+'; ';
  str(v[2],s); result:=result+s+')';
//  result:='( '+floattostr(v[0])+'; '+floattostr(v[1])+'; '+floattostr(v[2])+' )';
end;

function StringToVector(S: string): TAffineVector;
var t,c: string;
    d: integer;
begin
  t:=s; delete(t,1,7); delete(t,length(t),1);
  c:=copy(t,1,pos(';',t)-1); delete(t,1,pos(';',t)+1);
  val(c,result[0],d);
  c:=copy(t,1,pos(';',t)-1); delete(t,1,pos(';',t)+1);
  val(c,result[1],d); val(t,result[2],d);
end;
*)
