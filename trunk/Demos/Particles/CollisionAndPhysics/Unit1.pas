unit Unit1;

interface

uses
  Windows, Messages, SysUtilsLite, Variants, Classes, Controls, Forms,
  Dialogs, GLSimpleNavigation, GLScene, GLObjects, GLCoordinates,
  GLCadencer, GLWin32Viewer, GLCrossPlatform, BaseClasses, StdCtrls,
  VectorLists, VectorGeometry, ComCtrls, ExtCtrls, OpenGL1x,
  vboMesh, uMeshObjects, PFXManager, uTextures, uMaterialObjects,
  OGLStateEmul;
type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLDummyCube1: TGLDummyCube;
    GLSimpleNavigation1: TGLSimpleNavigation;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    Button1: TButton;
    TrackBar1: TTrackBar;
    Panel1: TPanel;
    Button2: TButton;
    Panel2: TPanel;
    TrackBar2: TTrackBar;
    Panel3: TPanel;
    TrackBar3: TTrackBar;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CheckBox3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure tbChange(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure tbPCChange(Sender: TObject);
    procedure tbSetUTime(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function ParticleLifeTime(var PLife: TParticleLifeCycle;
                            CheckTime: TCheckTime):TCheckTimeRes;
  end;

var
  Form1: TForm1;
  Meshes:TVBOMesh;
  PFX: TPFXManager;
  particles:TVBOParticles;
  Back, Snow: TTexture;


implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var i:integer;
    v:taffinevector;
    c:TVector;
begin
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  OGLStateEmul.GLStateCache.CheckStates;
  Back:=TTexture.CreateFromFile('Media\3D_nature_048.jpg');
  Back.Name:='Back';
  Snow:=TTexture.CreateFromFile('Media\Snow1_128.tga');
  Snow.TextureMode:=tcModulate;
  //добавл€ем к сцене наш контейнер
  Meshes:=TVBOMesh.CreateAsChild(GLScene1.Objects);
  //ƒобавл€ем квад на весь экран, NoZWrite говорит о том,
  //что он будет использован в качестве заднего плана
  with Meshes.AddScreenQuad do begin
    //Material:=GLMaterialLibrary1.LibMaterialByName('Back');
    Texture:=Back;
    NoZWrite:=true;
    visible:=false;
    Blending:=bmOpaque;
  end;

  //«агрузим домики дл€ проверки коллизий с ними и подготовим октри
  with Meshes.AddMeshFromFile('Media\3.3ds') do begin
    ScaleObject(vectormake(0.01,0.01,0.01,1));
    MoveObject(vectormake(0,-20,0));
  end;
  Meshes.BuildOctree(3);

  PFX:= TPFXManager.Create;
  with PFX do begin
    Gravity:=0.000098; //гравитаци€
    TimeSpeed:=10; //множитель времени
    UpdateTime:=1/25; //частота обновлени€ физики, (раз в секунду)
    Mass:=10; //масса частицы, об€зательно задавать при работе с силами
    RayCastFreq:=rcEveryUpdate; //частота вызова функции проверки коллизий
    //‘ункци€ вызываетс€ каждый раз перед/после расчета координат
    OnUpdateTime:=ParticleLifeTime;
  end;
  //–езервируем пам€ть под 10к частиц и добавл€ем их на сцену
  Particles:=Meshes.AddParticles(10000) as TVBOParticles;
  with Particles do begin
    Texture:=snow;
    Name:='Particles';
    //говорим что будет использоватьс€ буфер цвета,
    //значение нужно выставл€ть до добавлени€ первой частицы
    UseColors:=true;
    Visible:=false;
    For i:=1 to 10000 do begin
      setvector(v,random(90)-30,random(40)-20,random(70)-30);
       //c:=vectormake(random(255)/255,random(255)/255,random(255)/255,1);
      c:=vectormake(1,1,1,1); //снежок у нас белого цвета
      AddParticle(vectormake(v),c);
      AddVelocity(affinevectormake((random(10)-5)/1000,-(random(10))/1000-0.0001,(random(10)-5)/1000));
    end;

    Count:=1000; //сделаем видимыми лишь 1000 частиц

    MaterialObject.Blending.SetByMode(bmTransparency);
    NoZWrite:=true;
   //зададим размеры частиц
    PointSize:=40;
    FadeTresholdSize:=10;

    PFXManager:=PFX; //подключим к частицам менеджер

    //скажем что обновл€ть VBO нужно лишь после расчета физики,
    //и сразу дл€ всего массива
    Immediate:=false; //true - данные в VBO буфере будут изменены незамедлительно

    Visible:=true;
  end;

end;

procedure TForm1.FormResize(Sender: TObject);
begin
   GLSceneViewer1.Buffer.ClearBuffers;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  PFX.DeltaTime:=deltaTime; //обновим врем€ у менеджера частиц
  GLSceneViewer1.Invalidate;
end;

function TForm1.ParticleLifeTime(var PLife: TParticleLifeCycle;
                            CheckTime: TCheckTime):TCheckTimeRes;
var p,n,v,c1,c2:TVector;
    d:double;
begin
 result:=trContinue;
 //Ёта процедура будет вызыватьс€ дважды - до и после расчета координат
 //Ќа первом проходе мы вычисл€ем возможную точку коллизии,
 //на втором - если коллизи€ произошла - запускаем жзненный цикл частицы
 with PLife do begin
   case CheckTime of
      ctBefore: begin
        if not Started then begin
          if CheckBox4.Checked and (Dir[1]<>0)then begin
            if not RayCasted or (RayCastFreq=rcEveryUpdate) then begin
             RayIntersected:=Meshes[1].OctreeRayCastIntersect(vectormake(Pos),
             vectormake(Dir),@p,@n);
             if RayIntersected then begin
                iPosition:=p; iNormal:=n;
             end;
             RayCasted:=true;
            end;
          end;
        end;
      end;

      ctAfter: begin
        if not started and (RayCasted and RayIntersected) then begin
         if (Pos[1]<iPosition[1]) then begin
           Particles.Positions[PIndex]:=iPosition;
           Particles.Velocity[PIndex]:=vectormake(0,0,0,0);
           Particles.Colors[PIndex]:=vectormake(1,0,0,1);
           Started:=true; Iteration:=0; LifeTime:=1; TimeLeft:=0;
           result:=trBreak;
         end;
        end;
        if (Pos[1]<-20) and (not RayIntersected) then begin
           Started:=true; TimeLeft:=LifeTime;
        end;
        if Started then begin
           TimeLeft:=TimeLeft+dTime;
           if TimeLeft>LifeTime then begin
              with Particles do begin
                setvector(v,random(90)-30,20,random(70)-30);
                Positions[PIndex]:=v;
                if CheckBox5.Checked then
                     Colors[PIndex]:=vectormake(1,1,1,0)
                else Colors[PIndex]:=vectormake(1,1,1,1);
                Velocity[PIndex]:=vectormake((random(10)-5)/1000,-(random(10))/1000-0.0001,(random(10)-5)/1000);
//                Velocity[PIndex]:=vectormake(0,-0.001*(1+random(10)),0);
              end;
              Started:=false; RayCasted:=false;
              RayIntersected:=false;
              RayCastFreq:=rcEveryUpdate;
              result:=trBreak;
           end else begin
              d:=TimeLeft/LifeTime;
              c1:=vectormake(1,0,0,1);
              c2:=vectormake(1,1,1,0);
              Particles.Colors[PIndex]:=vectorlerp(c1,c2,d);
              result:=trBreak;
           end;
        end;
       //”меньшение прозрачности по мере удалени€ от камеры
        if CheckBox5.Checked then begin
          d:=1-VectorDistance(Pos,GLCamera1.Position.AsAffineVector)/50;
          if d<0 then d:=0.1;
          c1:=Particles.Colors[PIndex];
          if c1[3]<>d then begin
            c1[3]:=3*d; Particles.Colors[PIndex]:=c1;
          end;
        end;
      end;
   end;
 end;
end;

procedure TForm1.tbChange(Sender: TObject);
begin
   PFX.TimeSpeed:=1/Power(10,(TrackBar1.Position-10)/1.5);
end;

procedure TForm1.tbPCChange(Sender: TObject);
begin
   Particles.Count:=TrackBar2.Position*1000;
end;

procedure TForm1.tbSetUTime(Sender: TObject);
begin
   PFX.UpdateTime:=1/sqr(TrackBar3.Position);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
   TrackBar1.Position:=7;
   PFX.Enabled:=true;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  //добавим слабенькую силу действующую во всех точках пространства,
  //эмулиру€ ветер дующий с посто€нной скоростью и направлением
  PFX.AddForce(affinevectormake(0.0001,0,0),affinevectormake(0,20,0),ftConstant);
  pfx.UseForces:=true;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  Meshes[0].Visible:=CheckBox1.Checked;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  Meshes[2].Visible:=CheckBox2.Checked;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  GLCadencer1.Enabled:=false;
  Meshes.Visible:=false;
  GLSceneViewer1.Buffer.RenderingContext.Deactivate;
  PFX.Free;
end;

procedure TForm1.CheckBox3Click(Sender: TObject);
begin
  Meshes[2].Texture.Disabled:=not CheckBox3.Checked;
end;


end.
