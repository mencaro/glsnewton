unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLObjects, GLCoordinates, GLCadencer, GLWin32Viewer,
  GLCrossPlatform, BaseClasses, StdCtrls, ExtCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    Timer1: TTimer;
    Memo1: TMemo;
    Memo2: TMemo;
    GroupBox1: TGroupBox;
    TrackBar1: TTrackBar;
    procedure TrackBar1Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses uOpenAL_Sound, VBOMesh, VectorGeometry;

var
  OAL_Eng: TOpenALVBOEngine;
  Vm: TVBOMesh;
{$R *.dfm}

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  GLCadencer1.Enabled := false;
  FreeAndNil(GLSceneViewer1);
  FreeAndNil(OAL_Eng);
  FreeAndNil(Vm);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Vm := TVBOMesh.CreateAsChild(GLScene1.Objects);
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  Vm.AddSphere(1, 16, 16);
  Vm.AddBox(1, 1, 1, 1, 1, 1);
  Vm.AddBox(1, 1, 1, 1, 1, 1);
  GLSceneViewer1.Buffer.RenderingContext.DeActivate;
  // OAL
  OAL_Eng := TOpenALVBOEngine.Create(Vm[0]);
  OAL_Eng.AddSourceFromFile('..\Media\TestSound.wav', Vm[1], true);
  OAL_Eng.LastSource.Play;

  Memo1.Lines.Add('OpenAL Version: ' + OAL_Eng.VERSION);
  Memo1.Lines.Add('System Vendor: ' + OAL_Eng.VENDOR);
  Memo1.Lines.Add('Renderer Type: ' + OAL_Eng.RENDERER);
  Memo1.Lines.Add('Extensions: ');
  Memo1.Lines.Add(OAL_Eng.EXTENSIONS);

  Memo2.Lines.Add('File: ' + OAL_Eng.Sources[0].FileName);
  Memo2.Lines.Add('FREQUENCY: ' + IntToStr(OAL_Eng.Sources[0].FREQUENCY));
  Memo2.Lines.Add('BITS: ' + IntToStr(OAL_Eng.Sources[0].BITS));
  Memo2.Lines.Add('CHANNELS: ' + IntToStr(OAL_Eng.Sources[0].CHANNELS));
  Memo2.Lines.Add('SIZE: ' + IntToStr(OAL_Eng.Sources[0].SIZE div 1024)
      + ' kb');

  if OAL_Eng.OALError then
    ShowMessage(OAL_Eng.ErrorName)
  else
    Timer1.Enabled := true;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
var
  alpha: Single;
begin
  alpha := 60 * DegToRad(newTime);
  Vm[1].Position := Vectormake(sin(alpha) * 2, 0.5, cos(alpha) * 5);
  Vm[2].Position := Vectormake(0, sin(alpha) * 5, cos(alpha) * 8);
  OAL_Eng.UpdateSources;
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var SoundLib : TOALSoundLibrary;
begin
  // использование буффера
  SoundLib := TOALSoundLibrary.Create;
  SoundLib.AddSoundFromFile('..\Media\TestSound.wav');
  OAL_Eng.AddSourceFromBuffer(SoundLib.Sounds[0], Vm[2], true);
  OAL_Eng.LastSource.Play;
  Timer1.Enabled := false;
  GLCadencer1.Enabled := true;
  SoundLib.Free;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  OAL_Eng.Volume := TrackBar1.Position;
end;

end.
