// by For[)
// Updated 27.07.10

unit uOpenAL_Sound;

interface

uses OpenAL, Classes, SysUtilsLite, VBOMesh, uMeshObjects, VectorGeometry,
     VectorTypes;

type
  TBufferData = packed record
    format: TALenum;
    data: TALvoid;
    SIZE: TALsizei;
    freq: TALsizei;
    loop: TALint;
    FileName: String;
  end;

  TOALSoundLibrary = class
    private
      FSounds: array of TBufferData;
      function GetSound(Index: Integer) : TBufferData;
    public
      Count : Byte;
      property Sounds[Index: Integer]: TBufferData read GetSound;
      procedure AddSoundFromFile(FileName:String);
      Constructor Create; overload;
      Destructor Destroy; override;
  end;

  TOALSoundSource = class
  private
    Buffer: TALuint;
    Source: TALuint;
    FLoopMode: boolean;
    procedure SetLoopMode(LoopMode: boolean);
  public
    MeshObject: TVBOMeshObject;
    Playing: boolean;
    Volume: Single;
    FileName: String;
    FREQUENCY, BITS, CHANNELS, SIZE: Integer;
    property LoopMode: boolean read FLoopMode write SetLoopMode;
    procedure Play;
    procedure Pause;
    procedure Stop;
    destructor Destroy; override;
  end;

  TOpenALVBOEngine = class
  private
    FSoundSources: TList;
    MainDevice: TALCdevice;
    MainContext: TALCcontext;
    procedure UpdateSourceSoundInfo(Source : TOALSoundSource);
    function GetLastSource: TOALSoundSource;
    function GetSource(Index: Integer): TOALSoundSource;
  public
    Volume: Single;
    Listener: TOALSoundSource;
    VENDOR, VERSION, RENDERER, EXTENSIONS: String;
    ErrorName: String;
    property LastSource: TOALSoundSource read GetLastSource;
    property Sources[Index: Integer]: TOALSoundSource read GetSource;
    procedure AddSourceFromFile(FileName: String;
      VBOMeshObject: TVBOMeshObject; LoopMode: boolean = false);
    procedure AddSourceFromBuffer(BufferData: TBufferData;
      VBOMeshObject: TVBOMeshObject; LoopMode: boolean = false);
    procedure ReLoadSourceSoundBuffer(Source : TOALSoundSource;
     FileName: String); overload;
    procedure ReLoadSourceSoundBuffer(Source : TOALSoundSource;
      BufferData : TBufferData); overload;
    procedure UpdateSources;
    procedure ClearSources;
    function OALError: boolean;
    constructor Create(ListenerObject: TVBOMeshObject); overload;
    destructor Destroy; override;
  end;

function V4ftoV3f(V: TVector4f): TVector3f;
function ScaleVector3f(V: TVector3f; Scale: Single): TVector3f;
function ReadALError: String;

implementation

procedure TOALSoundLibrary.AddSoundFromFile(FileName:String);
var
  Buf : TBufferData;
begin
  alutLoadWAVFile(FileName, Buf.format, Buf.data, Buf.SIZE, Buf.freq, Buf.loop);
  Buf.FileName := FileName;
  FSounds[Count] := Buf;
  alutUnloadWAV(Buf.format, Buf.data, Buf.SIZE, Buf.freq);
  Count := Count + 1;
end;

function TOALSoundLibrary.GetSound(Index: Integer) : TBufferData;
begin
  result := FSounds[Index];
end;

Constructor TOALSoundLibrary.Create;
begin
  Count := 0;
  SetLength(FSounds, 511);
  inherited Create;
end;


Destructor TOALSoundLibrary.Destroy;
begin
  inherited Destroy;
end;

//

function ReadALError: String;
var
  I: Integer;
begin
  I := alGetError;
  case I of
    AL_NO_ERROR:
      Result := '';
    AL_INVALID_NAME:
      Result := 'AL_INVALID_NAME';
    AL_INVALID_ENUM:
      Result := 'AL_INVALID_ENUM';
    AL_INVALID_VALUE:
      Result := 'AL_INVALID_VALUE';
    AL_INVALID_OPERATION:
      Result := 'AL_INVALID_OPERATION';
    AL_OUT_OF_MEMORY:
      Result := 'AL_OUT_OF_MEMORY';
  end;
end;

function V4ftoV3f(V: TVector4f): TVector3f;
begin
  Result[0] := V[0];
  Result[1] := V[1];
  Result[2] := V[2];
end;

function ScaleVector3f(V: TVector3f; Scale: Single): TVector3f;
begin
  Result[0] := V[0] * Scale;
  Result[1] := V[1] * Scale;
  Result[2] := V[2] * Scale;
end;

destructor TOALSoundSource.Destroy;
begin
  alDeleteBuffers(1, @Buffer);
  alDeleteSources(1, @Source);
  inherited Destroy;
end;

procedure TOALSoundSource.SetLoopMode(LoopMode: boolean);
begin
  FLoopMode := LoopMode;
  if LoopMode then
    alSourcei(Source, AL_LOOPING, AL_TRUE)
  else
    alSourcei(Source, AL_LOOPING, AL_FALSE);
end;

procedure TOALSoundSource.Pause;
begin
  alSourcePause(Source);
  Playing := false;
end;

procedure TOALSoundSource.Play;
begin
  alSourcePlay(Source);
  Playing := true;
end;

procedure TOALSoundSource.Stop;
begin
  alSourceStop(Source);
  Playing := false;
end;

//

procedure TOpenALVBOEngine.UpdateSources;
var
  I: Integer;
  sVolume: Single;
  V: TVector3f;
begin
  if Volume = 0 then Exit;
  sVolume := 1 / Volume;
  V := ScaleVector3f(V4ftoV3f(Listener.MeshObject.Position), sVolume);
  alListenerfv(AL_POSITION, @V);
  alListenerfv(AL_ORIENTATION, @Listener.MeshObject.Direction);
  for I := 0 to FSoundSources.Count - 1 do
  begin
    V := ScaleVector3f(V4ftoV3f(Sources[I].MeshObject.Position), sVolume *(1 / Sources[I].Volume));
    alSourcefv(Sources[I].Source, AL_POSITION, @V);
  end;
end;

procedure TOpenALVBOEngine.ClearSources;
var
  I: Integer;
begin
  for I := 0 to FSoundSources.Count - 1 do
    Sources[I].Free;
end;

function TOpenALVBOEngine.GetLastSource: TOALSoundSource;
begin
  Result := FSoundSources.Last;
end;

procedure TOpenALVBOEngine.UpdateSourceSoundInfo(Source : TOALSoundSource);
var
  I: Integer;
begin
  alGetBufferi(Source.Buffer, AL_FREQUENCY, @I);
  Source.FREQUENCY := I;
  alGetBufferi(Source.Buffer, AL_BITS, @I);
  Source.BITS := I;
  alGetBufferi(Source.Buffer, AL_CHANNELS, @I);
  Source.CHANNELS := I;
  alGetBufferi(Source.Buffer, AL_SIZE, @I);
  Source.SIZE := I;
end;

procedure TOpenALVBOEngine.ReLoadSourceSoundBuffer(Source : TOALSoundSource;
 FileName: String);
var
  Buf : TBufferData;
begin
  alutLoadWAVFile(FileName, Buf.format, Buf.data, Buf.SIZE, Buf.freq, Buf.loop);
  alBufferData(Source.Buffer, Buf.format, Buf.data, Buf.SIZE, Buf.freq);
  alutUnloadWAV(Buf.format, Buf.data, Buf.SIZE, Buf.freq);
  Source.FileName := FileName;
  UpdateSourceSoundInfo(Source);
end;

procedure TOpenALVBOEngine.ReLoadSourceSoundBuffer(Source : TOALSoundSource;
  BufferData : TBufferData);
begin
  alBufferData(Source.Buffer, BufferData.format, BufferData.data,
                              BufferData.SIZE, BufferData.freq);
  Source.FileName := BufferData.FileName;
  UpdateSourceSoundInfo(Source);
end;

procedure TOpenALVBOEngine.AddSourceFromFile(FileName: String;
  VBOMeshObject: TVBOMeshObject; LoopMode: boolean = false);
const
  SourcevVel: array [0 .. 2] of TALfloat = (0, 0, 0);
var
  Buf : TBufferData;
begin
  FSoundSources.Add(TOALSoundSource.Create);
  LastSource.MeshObject := VBOMeshObject;
  LastSource.FileName := ExtractFileName(FileName);
  LastSource.Volume := 1;
  alGenBuffers(1, @LastSource.Buffer);
  alutLoadWAVFile(FileName, Buf.format, Buf.data, Buf.SIZE, Buf.freq, Buf.loop);
  alBufferData(LastSource.Buffer, Buf.format, Buf.data, Buf.SIZE, Buf.freq);
  alutUnloadWAV(Buf.format, Buf.data, Buf.SIZE, Buf.freq);
  alGenSources(1, @LastSource.Source);
  alSourcei(LastSource.Source, AL_BUFFER, LastSource.Buffer);
  alSourcefv(LastSource.Source, AL_VELOCITY, @SourcevVel);
  if LoopMode then
    alSourcei(LastSource.Source, AL_LOOPING, AL_TRUE)
  else
    alSourcei(LastSource.Source, AL_LOOPING, AL_FALSE);
  UpdateSourceSoundInfo(LastSource);
  UpdateSources;
end;

procedure TOpenALVBOEngine.AddSourceFromBuffer(BufferData: TBufferData;
      VBOMeshObject: TVBOMeshObject; LoopMode: boolean = false);
const
  SourcevVel: array [0 .. 2] of TALfloat = (0, 0, 0);
begin
  FSoundSources.Add(TOALSoundSource.Create);
  LastSource.MeshObject := VBOMeshObject;
  LastSource.FileName := BufferData.FileName;
  LastSource.Volume := 1;
  alGenBuffers(1, @LastSource.Buffer);
  alBufferData(LastSource.Buffer, BufferData.format, BufferData.data,
                                  BufferData.SIZE, BufferData.freq);
  alGenSources(1, @LastSource.Source);
  alSourcei(LastSource.Source, AL_BUFFER, LastSource.Buffer);
  alSourcefv(LastSource.Source, AL_VELOCITY, @SourcevVel);
  if LoopMode then
    alSourcei(LastSource.Source, AL_LOOPING, AL_TRUE)
  else
    alSourcei(LastSource.Source, AL_LOOPING, AL_FALSE);
  UpdateSourceSoundInfo(LastSource);
  UpdateSources;
end;

function TOpenALVBOEngine.GetSource(Index: Integer): TOALSoundSource;
begin
  Result := FSoundSources[Index];
end;

function TOpenALVBOEngine.OALError;
begin
  ErrorName := ReadALError;
  if ErrorName = '' then
    Result := false
  else
    Result := true;
end;

constructor TOpenALVBOEngine.Create(ListenerObject: TVBOMeshObject);
const
  ListenerVel: array [0 .. 2] of TALfloat = (0, 0, 0);
begin
  InitOpenAL;
  MainDevice := alcOpenDevice(nil);
  MainContext := alcCreateContext(MainDevice, nil);
  alcMakeContextCurrent(MainContext);
  VENDOR := alGetString(AL_VENDOR);
  VERSION := alGetString(AL_VERSION);
  RENDERER := alGetString(AL_RENDERER);
  EXTENSIONS := alGetString(AL_EXTENSIONS);
  FSoundSources := TList.Create;
  Listener := TOALSoundSource.Create;
  Listener.MeshObject := ListenerObject;
  alListenerfv(AL_VELOCITY, @ListenerVel);
  Volume := 1;
  inherited Create;
end;

destructor TOpenALVBOEngine.Destroy;
begin
  ClearSources;
  alcMakeContextCurrent(nil);
  alcDestroyContext(MainContext);
  alcCloseDevice(MainDevice);
  FreeAndNil(FSoundSources);
  FreeAndNil(Listener);
  inherited Destroy;
end;

end.
