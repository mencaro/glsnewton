unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLObjects, GLCoordinates, GLCadencer, GLWin32Viewer,
  GLCrossPlatform, BaseClasses, StdCtrls, vectorgeometry,
  GLSimpleNavigation, GLMaterial, GLGeomObjects,
  //VBOMesh
  vboMesh, uVBO, uMeshObjects, uMaterials, uMaterialObjects, uGLSceneAdapter,
  OGLStateEmul;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLDummyCube1: TGLDummyCube;
    mtl: TGLMaterialLibrary;
    ListBox1: TListBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  mesh:TVBOMesh;
  picked: TVBOMeshObject;
  srect: TVBOMeshObject;
  mdx,mdy,mmx,mmy: integer;
  md: boolean = false;
  mm: boolean = false;
  sp,ep:TVector;

  Sel,trans: TMaterial;

implementation

{$R *.dfm}

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
var v,d,p,s:TVector;
    t:single;
begin
  GLSceneViewer1.Invalidate;
   if md and mm then begin
      mesh.ScreenToWorld(mmx,mmy,@v,@d);
      ep:=v; t:=-v[2]/d[2];
      VectorCombine(v, d, t, p);
      s[0]:=(p[0]-(sp[0]));
      s[2]:=((sp[1]-p[1]));
      s[1]:=1; s[3]:=0;
      srect.Scale:=s;
      s[0]:=sp[0]+s[0]/2;
      s[1]:=sp[1]-s[2]/2;
      s[2]:=0; s[3]:=1;
      srect.Position:=s;
   end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var i:integer;
    Box: TVBOMeshObject;
    GLSAdapter: TGLSceneAdapter;
begin
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  OGLStateEmul.GLStateCache.CheckStates;

  GLSAdapter:=TGLSceneAdapter.Create;
  sel:=GLSAdapter.ImportMaterial(mtl.Materials[1].Material);
  trans:=GLSAdapter.ImportMaterial(mtl.Materials[2].Material);
  GLSAdapter.Free;
  Mesh:=TVBOMesh.CreateAsChild(GLScene1.Objects);
  For i:=0 to 100 do begin
    with Mesh.AddSphere(0.1,16,32) do begin
      RotateAroundY(-pi/1.2+random(314)/100);
      MoveObject(random(200)/10-10,random(200)/10-10,random(200)/10-10);
      box:=Mesh.AddBBox(Extents,vectormake(1,0,0));
      box.Visible:=false; Tag:=box.IndexInMesh;
    end;
  end;
  //Этот плэйн будет использован в качестве рамки выделения
  srect:=mesh.AddPlane(1,1,1,1);
  with srect do begin
     RotateAroundX(pi/2);
     Material:=trans;
     MaterialObject.Blending.SetByMode(bmTransparency);
     visible:=false;
     scale:=vectormake(0,0,0);
     pickable:=false;
     NoDepthTest:=true;
  end;
  Mesh.BuildOctree(1);
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Intersected:TList;
    ri:PRaycastIntersectInfo;
    i,j:integer;
    mo: TVBOMeshObject;
    buff: PVBOBuffer;
    s:string;
    v,d,p:TVector;
    t:single;
begin
   if not md then begin
     //Запоминаем левый верхний угол прямоугольника выделения
     md:=true;   srect.Visible:=true;
     mesh.ScreenToWorld(x,y,@v,@d);
     t:=-v[2]/d[2];
     VectorCombine(v, d, t, p);
     sp:=p;
     srect.Position:=sp;
     srect.Scale:=vectormake(0,0,0,1);
     mdx:=X; mdy:=Y;
   end;
   Intersected:=TList.Create; listbox1.Clear;
   //Убираем все старое выделение
   for i:=0 to mesh.Count-1 do begin
      if mesh[i].MeshType=mtSphere then begin
        mesh[i].Material:=nil;
        Mesh[mesh[i].Tag].Visible:=false;
      end;
   end;
   //Проверяем есть ли объект в этой точке
   if Mesh.PickObject(x,y,intersected,true) then begin
      //если есть и их несколько - добавляем их в список и выделяем их цветом и боксом
      for i:=0 to intersected.Count-1 do begin
         ri:=intersected[i];
         if ri<>nil then begin
           mo:=ri.PickedObject;
           s:=mo.Name;
           if mo.MeshType=mtSphere then picked:=mo;
           if Picked<>nil then begin
             picked.Material:=sel;
             Mesh[picked.Tag].Visible:=true;
           end;
           if assigned(ri.ObjMeshIndexList) then begin
             for j:=0 to ri.ObjMeshIndexList.Count-1 do begin
               buff:=mo.MeshList[ri.ObjMeshIndexList[j]];
               s:=s+':'+buff.name+':'+inttostr(ri.ObjMeshIndexList[j]);
             end;
             ri.ObjMeshIndexList.Free;
             listbox1.Items.Add(s);
           end;
         end;
      end;
   end;
   FreeList(Intersected);
end;

procedure TForm1.GLSceneViewer1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var list:Tlist;
    r:Trect;
    i:integer;
    f:boolean;
begin
  //Убираем рамку выделения и ищем объекты в заданной области
  md:=false;mm:=false;
  srect.Visible:=false;
  r:=Rect(mdx,mdy,x,y);
  list:=Tlist.Create;
  if abs((mdx-x)*(mdy-y))<>0 then begin
    //выбираем один из двух способов поиска объектов
    if RadioButton1.Checked then
         f:=mesh.PickObjectInFrustum(r,list)
    else f:=mesh.PickObject(r,list);
    //Если объекты были выбраны - выделяем их и добавляем в список
    if f then begin
       listbox1.Items.Add(inttostr(list.Count));
       for i:=0 to list.Count-1 do begin
          picked:=list[i];
          picked.Material:=sel;
          Mesh[picked.Tag].Visible:=true;
          listbox1.Items.Add(picked.Name);
       end;
    end;
  end;
  list.Free;
end;


procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if not md then exit;
  mm:=true;
  mmx:=x;mmy:=y;
end;

end.

