unit Unit1;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  //GLScene Units
  GLCadencer, GLScene, GLObjects, GLCoordinates,
  GLWin32Viewer, GLCrossPlatform, BaseClasses, Vectorgeometry,
  GLGeomObjects, GLMaterial, GLSimpleNavigation,
  GLVectorFileObjects, GLRenderContextInfo, glFile3DS,
  //VBOMesh Units:
  VBOMesh, uMeshObjects, uBaseClasses, OGLStateEmul;
type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLLightSource1: TGLLightSource;
    GLCamera1: TGLCamera;
    matlib: TGLMaterialLibrary;
    GLDummyCube1: TGLDummyCube;
    GLSimpleNavigation1: TGLSimpleNavigation;
    rgProxyType: TRadioGroup;
    Panel1: TPanel;
    tbObjectsCount: TTrackBar;
    cbObjectType: TComboBox;
    Label1: TLabel;
    Button1: TButton;
    Label2: TLabel;
    Label3: TLabel;
    CheckBox1: TCheckBox;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure rgProxyTypeClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure tbObjectsCountChange(Sender: TObject);
    procedure cbObjectTypeChange(Sender: TObject);
    procedure cbObjectTypeCloseUp(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  //Каждый тип прокси будем создавать в своем "мире"
  InstanceWorld: TVBOMesh;
  ProxyWorld: TVBOMesh;
  SceneWorld: TGLBaseSceneObject;
  //Мастер объект для объектов GLScene и VBOMesh
  vMasterObject: TVBOMeshObject;
  sMasterObject: TGLBaseSceneObject;
  //Ссылка на активный мир
  ActWorld: TGLBaseSceneObject;
  VBOWorld: TVBOMesh;

  PrevMode: integer = -1;
  h: single;
  iwidx, pwidx,swidx: integer;
implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  rgProxyType.OnClick(self);
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
//Включаем режим сортировки объектов по глубине
  if CheckBox1.Checked then begin
    InstanceWorld.SortDirection:=sdFrontToBack;
    ProxyWorld.SortDirection:=sdFrontToBack;
    SceneWorld.ObjectsSorting:=osRenderNearestFirst;
  end else begin
    InstanceWorld.SortDirection:=sdNone;
    ProxyWorld.SortDirection:=sdNone;
    SceneWorld.ObjectsSorting:=osNone;
  end;
end;

procedure TForm1.cbObjectTypeChange(Sender: TObject);
begin
  rgProxyType.OnClick(self);
end;

procedure TForm1.cbObjectTypeCloseUp(Sender: TObject);
begin
  rgProxyType.OnClick(self);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  //Активируем контекст OGL и делаем сохраняем состояния переменных OGL
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  OGLStateEmul.GLStateCache.CheckStates;

  //Добавляем на сцену 3 мира для каждого типа прокси
  InstanceWorld:=TVBOMesh.CreateAsChild(GLScene1.Objects);
  InstanceWorld.Name:='InstanceWorld'; InstanceWorld.Visible:=false;
  iwidx:=GLScene1.Objects.IndexOfChild(InstanceWorld.GLSceneMeshAdapter);

  ProxyWorld:=TVBOMesh.CreateAsChild(GLScene1.Objects);
  ProxyWorld.Name:='ProxyWorld'; ProxyWorld.Visible:=false;
  pwidx:=GLScene1.Objects.IndexOfChild(ProxyWorld.GLSceneMeshAdapter);

  SceneWorld:=TGLDummyCube.CreateAsChild(GLScene1.Objects);
  SceneWorld.Name:='SceneWorld'; SceneWorld.Visible:=false;
  swidx:=GLScene1.Objects.IndexOfChild(SceneWorld);

end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.rgProxyTypeClick(Sender: TObject);
var i,j,n: integer;
   emin,emax: TAffineVector;
begin
  if not GLSceneViewer1.Buffer.RenderingContext.Active then
    GLSceneViewer1.Buffer.RenderingContext.Activate;
  n:=tbObjectsCount.Position;
  //Деактивируем активный мир и удаляем все его прокси
  case PrevMode of
    0: begin
      InstanceWorld.Visible:=false;
      InstanceWorld.Clear;
    end;
    1: begin
      ProxyWorld.Visible:=false;
      ProxyWorld.Clear;
    end;
    2: begin
      SceneWorld.Visible:=false;
      SceneWorld.DeleteChildren;
    end;
  end;
  //Выбираем какой мир будет активным
  case rgProxyType.ItemIndex of
    0: VBOWorld:=InstanceWorld;
    1: VBOWorld:=ProxyWorld;
    2: ActWorld:=SceneWorld;
  end;

  //Добавляем к миру мастер объект
  case rgProxyType.ItemIndex of
    0,1: begin
        case cbObjectType.ItemIndex of
          0: vMasterObject:=VBOWorld.AddMeshFromFile('Media\Column.3ds');
          1: vMasterObject:=VBOWorld.AddBox(1,1,1,1,1,1);
          2: vMasterObject:=VBOWorld.AddSphere(1,8,16);
          3: vMasterObject:=VBOWorld.AddSphere(1,16,32);
        end; vMasterObject.Visible:=false;
        h:=vMasterObject.Extents.emax[1]-vMasterObject.Extents.emin[1];
        VBOWorld.Visible:=true;
      end;
      2: begin
        case cbObjectType.ItemIndex of
          0: begin
            sMasterObject:=TGLFreeForm.CreateAsChild(ActWorld);
            TGLFreeForm(sMasterObject).MaterialLibrary:=matlib;
            TGLFreeForm(sMasterObject).LoadFromFile('Media\Column.3ds');
            TGLFreeForm(sMasterObject).MeshObjects.GetExtents(emin,emax);
            h:=emax[1]-emin[1];
          end;
          1: sMasterObject:=TGLCube.CreateAsChild(ActWorld);
          2: begin
            sMasterObject:=TGLSphere.CreateAsChild(ActWorld);
            TGLSphere(sMasterObject).Slices:=16;
            TGLSphere(sMasterObject).Stacks:=16;
            TGLSphere(sMasterObject).Radius:=1;
          end;
          3: begin
            sMasterObject:=TGLSphere.CreateAsChild(ActWorld);
            TGLSphere(sMasterObject).Slices:=32;
            TGLSphere(sMasterObject).Stacks:=32;
            TGLSphere(sMasterObject).Radius:=1;
          end;
        end; sMasterObject.Visible:=false; ActWorld.Visible:=true;
      end;
  end;

  //Добавляем на сцену прокси заданного типа
  for i:=1 to n do for j:=1 to n do begin
    case rgProxyType.ItemIndex of
      0: begin
        case cbObjectType.ItemIndex of
             0: with VBOWorld.AddInstanceToObject(vMasterObject) do begin
               ScaleObject(1/400,1/h*4,1/400);
               MoveObject((i-n/2)*1.5,-2,(j-n/2)*1.5);
               Visible:=true;
             end;
         1,2,3: with VBOWorld.AddInstanceToObject(vMasterObject) do begin
               MoveObject((i-n/2)*1.5,-2,(j-n/2)*1.5); Visible:=true;
              end;
        end;
      end;
      1: begin
        case cbObjectType.ItemIndex of
             0: with VBOWorld.AddProxyObject(vMasterObject) do begin
               ScaleObject(1/400,1/h*4,1/400);
               MoveObject((i-n/2)*1.5,-2,(j-n/2)*1.5); Visible:=true;
             end;
         1,2,3: with VBOWorld.AddProxyObject(vMasterObject) do begin
               MoveObject((i-n/2)*1.5,-2,(j-n/2)*1.5); Visible:=true;
             end;
        end;
      end;
      2: begin
        case cbObjectType.ItemIndex of
             0: with TGLProxyObject.CreateAsChild(ActWorld) do begin
               Scale.SetVector(1/400,1/h*4,1/400);
               Position.SetPoint((i-n/2)*1.5,-2,(j-n/2)*1.5);
               Visible:=true; MasterObject:=sMasterObject;
             end;
         1,2,3: with TGLProxyObject.CreateAsChild(ActWorld) do begin
               Position.SetPoint((i-n/2)*1.5,-2,(j-n/2)*1.5);
               Visible:=true; MasterObject:=sMasterObject;
             end;
        end;
      end;
    end;
  end;
  PrevMode:=rgProxyType.ItemIndex;
end;

procedure TForm1.tbObjectsCountChange(Sender: TObject);
begin
  Label2.Caption:=inttostr(sqr(tbObjectsCount.Position));
end;

end.
