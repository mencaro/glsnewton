unit Unit1;

interface

uses
  Windows, Messages, SysUtilsLite, Variants, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, Dialogs,GLCadencer, GLScene, GLObjects, GLCoordinates,
  GLWin32Viewer, GLCrossPlatform, BaseClasses, GLKeyboard, Vectorgeometry,
  GLSimpleNavigation, VBOMesh, uBaseClasses, uMaterialObjects, OGLStateEmul;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLLightSource1: TGLLightSource;
    GLCamera1: TGLCamera;
    GLPlane1: TGLPlane;
    Panel1: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    CheckBox1: TCheckBox;
    GLSimpleNavigation1: TGLSimpleNavigation;
    GLDummyCube1: TGLDummyCube;
    CheckBox2: TCheckBox;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

  world: TVBOMesh;
implementation

{$R *.dfm}

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  World[0].NoDepthTest:=not CheckBox2.Checked;
  World[1].NoDepthTest:=not CheckBox2.Checked;
  World[2].NoDepthTest:=not CheckBox2.Checked;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  OGLStateEmul.GLStateCache.CheckStates;
  world:=TVBOMesh.CreateAsChild(glscene1.Objects);

  with world.AddBox(1,1,1,1,1,1) do begin
     MoveObject(0,0,1);
     MaterialObject.AddNewMaterial('Red').Properties.DiffuseColor.SetColor(1.0,0,0,0.5);
     NoDepthTest:=true;
  end;
  with world.AddBox(1,1,1,1,1,1) do begin
     MoveObject(1,0,3);
     MaterialObject.AddNewMaterial('Green').Properties.DiffuseColor.SetColor(0,1.0,0,0.5);
     NoDepthTest:=true;
  end;
  with world.AddBox(1,1,1,1,1,1) do begin
     MoveObject(2,0,5);
     MaterialObject.AddNewMaterial('Blue').Properties.DiffuseColor.SetColor(0.0,0,1.0,0.5);
     NoDepthTest:=true;
  end;
end;

procedure TForm1.RadioButton1Click(Sender: TObject);
begin
//Указываем порядок сортировки
   if RadioButton2.Checked then
      World.SortDirection:=sdBackToFront
   else World.SortDirection:=sdFrontToBack;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then begin
     //Прозрачные
     World[0].MaterialObject.Blending.SetByMode(bmTransparency);
     World[1].MaterialObject.Blending.SetByMode(bmTransparency);
     World[2].MaterialObject.Blending.SetByMode(bmTransparency);
  end else begin
     //Не прозрачные
     World[0].MaterialObject.Blending.SetByMode(bmOpaque);
     World[1].MaterialObject.Blending.SetByMode(bmOpaque);
     World[2].MaterialObject.Blending.SetByMode(bmOpaque);
  end;
end;

end.
