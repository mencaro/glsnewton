//http://sites.google.com/site/opengltutorialsbyaks/introduction-to-opengl-4-1---tutorial-05

unit uGLDebug;

interface

uses OpenGL1x;

const
  GL_DEBUG_OUTPUT_SYNCHRONOUS_ARB =             $8242;
  GL_MAX_DEBUG_MESSAGE_LENGTH_ARB =             $9143;
  GL_MAX_DEBUG_LOGGED_MESSAGES_ARB =            $9144;
  GL_DEBUG_LOGGED_MESSAGES_ARB =                $9145;
  GL_DEBUG_NEXT_LOGGED_MESSAGE_LENGTH_ARB =     $8243;
  GL_DEBUG_CALLBACK_FUNCTION_ARB =              $8244;
  GL_DEBUG_CALLBACK_USER_PARAM_ARB =            $8245;
  GL_DEBUG_SOURCE_API_ARB =                     $8246;
  GL_DEBUG_SOURCE_WINDOW_SYSTEM_ARB =           $8247;
  GL_DEBUG_SOURCE_SHADER_COMPILER_ARB =         $8248;
  GL_DEBUG_SOURCE_THIRD_PARTY_ARB =             $8249;
  GL_DEBUG_SOURCE_APPLICATION_ARB =             $824A;
  GL_DEBUG_SOURCE_OTHER_ARB =                   $824B;
  GL_DEBUG_TYPE_ERROR_ARB =                     $824C;
  GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR_ARB =       $824D;
  GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR_ARB =        $824E;
  GL_DEBUG_TYPE_PORTABILITY_ARB =               $824F;
  GL_DEBUG_TYPE_PERFORMANCE_ARB =               $8250;
  GL_DEBUG_TYPE_OTHER_ARB =                     $8251;
  GL_DEBUG_SEVERITY_HIGH_ARB =                  $9146;
  GL_DEBUG_SEVERITY_MEDIUM_ARB =                $9147;
  GL_DEBUG_SEVERITY_LOW_ARB =                   $9148;


type
  // GL_ARB_debug_output (ARB #104)
  PFNGLDEBUGMESSAGECONTROLARBPROC = procedure(source: TGLenum; _type: TGLenum; severity: TGLenum; count: TGLsizei; {const} ids: PGLuint; enabled: TGLboolean);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDEBUGMESSAGEINSERTARBPROC = procedure(source: TGLenum; _type: TGLenum; id: TGLuint; severity: TGLenum; length: TGLsizei; {const} buf: PGLchar);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDEBUGMESSAGECALLBACKARBPROC = procedure(callback: TGLDEBUGPROCARB; {const} userParam: Pointer);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETDEBUGMESSAGELOGARBPROC = function(count: TGLuint; bufsize: TGLsizei; sources: PGLenum; types: PGLenum; ids: PGLuint; severities: PGLenum; lengths: PGLsizei; messageLog: PGLchar): TGLuint;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; 

var
   // GL_ARB_debug_output (ARB #104)
   glDebugMessageControlARB: PFNGLDEBUGMESSAGECONTROLARBPROC;
   glDebugMessageInsertARB: PFNGLDEBUGMESSAGEINSERTARBPROC;
   glDebugMessageCallbackARB: PFNGLDEBUGMESSAGECALLBACKARBPROC;
   glGetDebugMessageLogARB: PFNGLGETDEBUGMESSAGELOGARBPROC;

   GL_ARB_debug_output: boolean = false;

implementation

function CheckExtension(const Extension: string): Boolean;
var ExtPos: Integer;
begin
  ExtPos := Pos(Extension, Buffer);
  Result := ExtPos > 0;
  if Result then
    Result := ((ExtPos + Length(Extension) - 1)= Length(Buffer))
              or (Buffer[ExtPos + Length(Extension)]=' ');
end;
 

procedure ReadDebugExtensions;
var Buffer : String;
begin
  // GL_ARB_debug_output (ARB #104)
  Buffer := String(glGetString(GL_EXTENSIONS));

  GL_ARB_debug_output := CheckExtension('GL_ARB_debug_output');

  if GL_ARB_debug_output then begin
    glDebugMessageControlARB := GLGetProcAddress('glDebugMessageControlARB');
    glDebugMessageInsertARB := GLGetProcAddress('glDebugMessageInsertARB');
    glDebugMessageCallbackARB := GLGetProcAddress('glDebugMessageCallbackARB');
    glGetDebugMessageLogARB := GLGetProcAddress('glGetDebugMessageLogARB');
  end;
end;
