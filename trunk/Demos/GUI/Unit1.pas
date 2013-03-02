unit Unit1;

interface

uses
  Windows, Messages, Classes, Graphics, Math, Forms, SysUtils,
  Dialogs, GLCadencer, GLScene, GLCoordinates,
  GLWin32Viewer, GLCrossPlatform, BaseClasses, Vectorgeometry,
  Controls, StdCtrls, ExtCtrls, OpenGL1x,
  //VBOMesh libs:
  VBOMesh, uTextures, uGUI, OGLStateEmul;
type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure onButtonClick(Sender: TObject);
  end;

var
  Form1: TForm1;
  world: TVBOMesh;
  tex: TTexture;
  GUIRender:  TGUIRender;
  Layer: TGUILayer;
  GLButton: TGLButton;
  Layer2: TGUILayer;
  GLPanel: TGLPanel;
  GLButton2: TGLButton;
  Buttons: array[0..30,0..30] of TGLButton;
  GLCheckBox: TGLCheckBox;
  GLStaticText, GLFPSText: TGLStaticText;
  GLProgressBar: TGLProgressBar;
  fps: double=-1;
  nt: double = 0;
implementation

{$R *.dfm}

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  GLCadencer1.Enabled:=false;
  world.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
    i,j: integer;
begin
  //Активируем контекст OGL и делаем сохраняем состояния переменных OGL
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  OGLStateEmul.GLStateCache.CheckStates;
  //Создаем текстуру из файла
  tex:=TTexture.CreateFromFile('Media\GUI.tga');
  tex.BlendingMode:=tbmAlphaTest50;
  tex.TextureMode:=tcModulate;

  //Добавляем к сцене контейнер с примитивами
  world:=TVBOMesh.CreateAsChild(GLScene1.Objects);
  world.OldRender:=false;
  GUIRender:=world.AddGUI;

  Layer:=GUIRender.AddNewLayer(0);
  Layer.Material.AttachTexture(tex);
  Layer.Left:=0; Layer.Top:=0;
  Layer.Width:=626;
  Layer.Height:=-1;//GLSceneViewer1.Height;

  GLPanel:=TGLPanel.Create;
  GUIRender.AddLayer(GLPanel);
  GLPanel.Material.AttachTexture(tex);
  GLPanel.Left:=630; GLPanel.Top:=0;
  GLPanel.Width:=GLSceneViewer1.Width-630;
  GLPanel.Height:=GLSceneViewer1.Height;
  GLPanel.SetVertGradient(VectorMake(1,0,0,1),VectorMake(0,1,0,1));

  for i:=0 to 24 do for j:=1 to 24 do begin
    Buttons[i,j]:=TGLButton.Create(Layer);
    Buttons[i,j].Left:=25*i;
    Buttons[i,j].Top:=25*j;
    Buttons[i,j].Width:=20;
    Buttons[i,j].Height:=20;
    Buttons[i,j].DragEnable:=true;
  end;

  GLProgressBar:=TGLProgressBar.Create(Layer);
  GLProgressBar.Left:=2; GLProgressBar.Top:=2;
  GLProgressBar.Width:=618;
  GLProgressBar.Height:=20;
  GLProgressBar.Position:=70;
  Visible:=true;

  for i:=0 to 3 do for j:=1 to 9 do begin
    GLButton2:=TGLButton.Create(GLPanel);
    GLButton2.Left:=10+i*58;
    GLButton2.Top:=10+j*61;
    GLButton2.Width:=50;
    GLButton2.Height:=50;
    GLButton2.onClick:=onButtonClick;
  end;

  GLCheckBox:=TGLCheckbox.Create(GLPanel);
  GLCheckBox.Left:=10;
  GLCheckBox.Top:=10;
  GLCheckBox.RadioButton:=false;

  GLCheckBox:=TGLCheckbox.Create(GLPanel);
  GLCheckBox.Left:=32;
  GLCheckBox.Top:=10;
  GLCheckBox.RadioButton:=true;

  GLCheckBox:=TGLCheckbox.Create(GLPanel);
  GLCheckBox.Left:=50;
  GLCheckBox.Top:=10;
  GLCheckBox.RadioButton:=true;

  GLCheckBox:=TGLCheckbox.Create(GLPanel);
  GLCheckBox.Left:=68;
  GLCheckBox.Top:=10;
  GLCheckBox.RadioButton:=true;

  GLStaticText:=TGLStaticText.Create(GLPanel);
  GLStaticText.Left:=90;
  GLStaticText.Top:=10;
  GLStaticText.Font.Color:=clRed;
  GLStaticText.Autosize:=true;
  GLStaticText.Text:='<<<<<Static Text>>>>>';

  GLFPSText:=TGLStaticText.Create(GLPanel);
  GLFPSText.Left:=150;
  GLFPSText.Top:=10;
  GLFPSText.Font.Color:=clYellow;
  GLFPSText.Autosize:=true;
  GLFPSText.Text:='FPS:';

  GLButton:=TGLButton.Create(GLPanel);
  GLButton.Left:=10;
  GLButton.Top:=40;
  GLButton.Width:=224;
  GLButton.Height:=24;
  GLButton.Caption:='<<Button Caption>>';
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  //Layer.Width:=GLSceneViewer1.Width;
  //Layer.Height:=GLSceneViewer1.Height;
  //GLPanel.Width:=GLSceneViewer1.Width;
  //GLPanel.Height:=GLSceneViewer1.Height;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  if fps=-1 then fps:=deltaTime else fps:=(fps+deltatime)/2;

  if assigned(GLStaticText) then begin
    GLStaticText.Text:=TimeToStr(Time);
  end;

  if assigned(GLFPSText) then begin
    if newTime-nt>=1 then begin
      GLFPSText.Text:='FPS: '+floattostr(roundto(1/fps,-2));
      nt:=newTime;
    end;
  end;

  if assigned(GLProgressBar) then begin
    GLProgressBar.Position:=frac(Newtime/2)*100;
  end;
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.onButtonClick(Sender: TObject);
begin
  TGUIControl(Sender).Visible:=false;
end;

function BinToStr(X: word): string;
var i: integer;
    Y: word;
    s: string;
begin
  s:='';Y:=1;
  for i:=0 to 15 do begin
    if X and Y>0 then s:='1'+s else s:='0'+s;
    Y:=Y shl 1;
  end;
  result:=s;
end;

end.
