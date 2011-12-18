unit Unit1;

interface

uses
  Windows, Messages, SysUtilsLite, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FRColorEditor, ComCtrls, FRTrackBarEdit, StdCtrls, Buttons, ExtCtrls,
  ExtDlgs, OpenGL1x, BaseClasses, GLScene, GLCoordinates, GLObjects, GLCadencer,
  GLSimpleNavigation,GLWin32Viewer, GLCrossPlatform,
  //VBOMesh Libs
  uTextures, uMaterials, vboMesh, uMeshObjects, uMaterialObjects, uMiscUtils,
  OGLStateEmul;
type
  TForm1 = class(TForm)
    ColorEditor: TPageControl;
    Ambient: TTabSheet;
    Diffuse: TTabSheet;
    Specular: TTabSheet;
    Emissive: TTabSheet;
    RColorEditor1: TRColorEditor;
    RColorEditor2: TRColorEditor;
    RColorEditor3: TRColorEditor;
    RColorEditor4: TRColorEditor;
    Material: TTabSheet;
    AddToLib: TButton;
    MaterialSelector: TComboBox;
    Panel1: TPanel;
    tfBrowse: TSpeedButton;
    GroupBox1: TGroupBox;
    Label11: TLabel;
    ComboBox10: TComboBox;
    GroupBox2: TGroupBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CBObject: TComboBox;
    Image1: TImage;
    GroupBox3: TGroupBox;
    Label10: TLabel;
    Label12: TLabel;
    ComboBox9: TComboBox;
    ComboBox11: TComboBox;
    GroupBox4: TGroupBox;
    Label14: TLabel;
    Label15: TLabel;
    tredAlphaTest: TRTrackBarEdit;
    ComboBox12: TComboBox;
    MatProp: TGroupBox;
    lShininess: TLabel;
    Label2: TLabel;
    SRTrackBarEdit: TRTrackBarEdit;
    cbColorReplacement: TComboBox;
    cbMDisable: TCheckBox;
    TexProp: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    cbMinFilter: TComboBox;
    cbMagFilter: TComboBox;
    cbWrapS: TComboBox;
    cbWrapT: TComboBox;
    cbMatBlend: TComboBox;
    OpenPictureDialog1: TOpenPictureDialog;
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLCadencer1: TGLCadencer;
    GLSimpleNavigation1: TGLSimpleNavigation;
    cbUseTexture: TCheckBox;
    tvMatlib: TTreeView;
    tvTextures: TTreeView;
    Label1: TLabel;
    Label3: TLabel;
    GLLightSource1: TGLLightSource;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox10Change(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure ComboBox9Change(Sender: TObject);
    procedure ComboBox11Change(Sender: TObject);
    procedure CBObjectChange(Sender: TObject);
    procedure ColorEditorChanging(Sender: TObject;
      var AllowChange: Boolean);
    procedure MaterialSelectorChange(Sender: TObject);
    procedure AddToLibClick(Sender: TObject);
    procedure cbColorReplacementChange(Sender: TObject);
    procedure cbMDisableClick(Sender: TObject);
    procedure cbMinFilterChange(Sender: TObject);
    procedure cbMagFilterChange(Sender: TObject);
    procedure cbMatBlendChange(Sender: TObject);
    procedure tfBrowseClick(Sender: TObject);
    procedure ComboBox12Change(Sender: TObject);
    procedure cbWrapSChange(Sender: TObject);
    procedure cbWrapTChange(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure cbUseTextureClick(Sender: TObject);
    procedure tredAlphaTestTrackBarChange(Sender: TObject);
    procedure tvTexturesClick(Sender: TObject);
    procedure tvMatlibClick(Sender: TObject);
  private
    { Private declarations }
    procedure MaterialAdded(Sender: TObject);
    procedure TextureAdded(Sender: TObject);
  public
    { Public declarations }
    procedure SetFromMaterial;
    procedure SetFromTexture;
    procedure sTrackbarChange(Sender: TObject);
    procedure aTrackbarChange(Sender: TObject);
    procedure AColorChange(Sender: TObject);
    procedure DColorChange(Sender: TObject);
    procedure SColorChange(Sender: TObject);
    procedure EColorChange(Sender: TObject);
    function LoadTexture(filename: string): TTexture;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

var Mesh: TVBOMesh;
    MatObj: TMaterialObject;
    tex: TTexture;
    mat: TMaterial;
    mdx,mdy: integer;
    Rx,Ry: integer;
    MaterialGetted: boolean = false;
    TextureGetted: boolean = false;
    ang: single = 0;
    ObjProp: TVBOMeshObject;

procedure TForm1.FormCreate(Sender: TObject);
var i:integer;
begin
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  OGLStateEmul.GLStateCache.CheckStates;

   SRTrackBarEdit.OnChange:=sTrackbarChange;
   tredAlphaTest.OnChange:=aTrackbarChange;
   RColorEditor1.OnChange:=AColorChange;
   RColorEditor2.OnChange:=DColorChange;
   RColorEditor3.OnChange:=SColorChange;
   RColorEditor4.OnChange:=EColorChange;

   image1.Picture.Bitmap.PixelFormat:=pf24bit;
   tex:=TTexture.CreateFromBitmap(image1.Picture.Bitmap);
   tex.TextureMode:=tcReplace;
   Mesh:=TVBOMesh.CreateAsChild(GLScene1.Objects);
   Mesh.Materials.OnAdding:=MaterialAdded;
   Mesh.Textures.OnAdding:=TextureAdded;
   with Mesh.AddScreenQuad do begin
     Texture:=tex;
     NoZWrite:=false;
     NoDepthTest:=true;
   end;
   with Mesh.AddBox(1,1,1,1,1,1) do begin
     visible:=true;
     PitchObject(-pi/4);
     TurnObject(pi/6);
   end; ObjProp:=Mesh[1];
   with Mesh.AddSphere(0.7,16,32,1,1) do begin
     visible:=false;
   end;
   matObj:=ObjProp.MaterialObject;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.ComboBox10Change(Sender: TObject);
begin
  if Combobox10.ItemIndex=6 then GroupBox2.Enabled:=true
  else GroupBox2.Enabled:=false;
  matObj.Blending.SetByMode(TBlendingModes(Combobox10.ItemIndex));
end;

procedure TForm1.cbUseTextureClick(Sender: TObject);
begin
  matObj.UseTexture:=cbUseTexture.Checked;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  GroupBox3.Enabled:=CheckBox2.Checked;
  matObj.Blending.BlendEnable:=CheckBox2.Checked;
end;

procedure TForm1.CheckBox3Click(Sender: TObject);
begin
  GroupBox4.Enabled:=CheckBox3.Checked;
  matObj.Blending.AlphaTestEnable:=CheckBox3.Checked;
end;

procedure TForm1.ComboBox9Change(Sender: TObject);
const src:array[0..10] of cardinal =(
  GL_ZERO,GL_ONE,GL_SRC_COLOR,GL_ONE_MINUS_SRC_COLOR,
  GL_DST_COLOR,GL_ONE_MINUS_DST_COLOR,GL_SRC_ALPHA,
  GL_ONE_MINUS_SRC_ALPHA,GL_DST_ALPHA,GL_ONE_MINUS_DST_ALPHA,
  GL_SRC_ALPHA_SATURATE);
begin
   matObj.Blending.SrcBlendFunc:=src[ComboBox9.ItemIndex];
end;

procedure TForm1.ComboBox11Change(Sender: TObject);
const dst:array[0..9] of cardinal =(
  GL_ZERO,GL_ONE,GL_SRC_COLOR,GL_ONE_MINUS_SRC_COLOR,
  GL_DST_COLOR,GL_ONE_MINUS_DST_COLOR,GL_SRC_ALPHA,
  GL_ONE_MINUS_SRC_ALPHA,GL_DST_ALPHA,GL_ONE_MINUS_DST_ALPHA);
begin
  matObj.Blending.DstBlendFunc:=dst[ComboBox11.ItemIndex];
end;

procedure TForm1.CBObjectChange(Sender: TObject);
begin
  if CBObject.ItemIndex=0 then begin
     mesh[1].Visible:=true;
     mesh[2].Visible:=false;
     ObjProp:=mesh[1];
  end;
  if CBObject.ItemIndex=1 then begin
     mesh[2].Visible:=true;
     mesh[1].Visible:=false;
     ObjProp:=mesh[2];
  end;
  if assigned(matObj) then
    ObjProp.MaterialObject.Assign(matObj);
  matObj:=ObjProp.MaterialObject;
end;

procedure TForm1.ColorEditorChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  AllowChange:=MaterialGetted;
end;

procedure TForm1.MaterialAdded(Sender: TObject);
begin
  tvMatLib.Items.Add(nil,TMaterialLibrary(Sender).Last.Name);
end;

procedure TForm1.MaterialSelectorChange(Sender: TObject);
begin
  if MaterialSelector.ItemIndex<>-1 then begin
     MaterialGetted:=true;
     objProp.Material:=Mesh.Materials[MaterialSelector.ItemIndex];
     AddToLib.Enabled:=false;
     MatProp.Enabled:=true;
     SetFromMaterial;
  end else begin
     MaterialGetted:=false;
     ObjProp.Material:=nil;
     AddToLib.Enabled:=true;
     MatProp.Enabled:=false;
  end;
end;

procedure TForm1.AddToLibClick(Sender: TObject);
begin
   if MaterialSelector.ItemIndex=-1 then begin
      MaterialSelector.ItemIndex:=
        MaterialSelector.Items.Add(
          Mesh.Materials.AddNewMaterial(MaterialSelector.Text).Name);
      MaterialSelectorChange(nil);
   end;MaterialGetted:=true;
end;

procedure TForm1.aTrackbarChange(Sender: TObject);
begin
  MatObj.Blending.AlphaThreshold:=tredAlphaTest.Value/255;
end;

procedure TForm1.SetFromMaterial;
begin
  cbMDisable.Checked:=ObjProp.Material.UseMaterial;
  SRTrackBarEdit.Value:=trunc(ObjProp.Material.Properties.Shininess);
  cbColorReplacement.ItemIndex:=byte(ObjProp.Material.ColorReplacing);
  RColorEditor1.Color:=ObjProp.Material.Properties.AmbientColor.ColorVector;
  RColorEditor2.Color:=ObjProp.Material.Properties.DiffuseColor.ColorVector;
  RColorEditor3.Color:=ObjProp.Material.Properties.SpecularColor.ColorVector;
  RColorEditor4.Color:=ObjProp.Material.Properties.EmissionColor.ColorVector;
end;


procedure TForm1.cbColorReplacementChange(Sender: TObject);
begin
  MatObj.Material.ColorReplacing:= TColorReplacing(cbColorReplacement.ItemIndex);
end;

procedure TForm1.cbMDisableClick(Sender: TObject);
begin
  MatObj.Material.UseMaterial:=cbMDisable.Checked;
end;

procedure TForm1.sTrackbarChange(Sender: TObject);
begin
   MatObj.Material.Properties.Shininess:=SRTrackBarEdit.Value;
end;

procedure TForm1.AColorChange(Sender: TObject);
begin
   MatObj.Material.Properties.AmbientColor.ColorVector:=RColorEditor1.Color;
end;

procedure TForm1.DColorChange(Sender: TObject);
begin
   MatObj.Material.Properties.DiffuseColor.ColorVector:=RColorEditor2.Color;
end;

procedure TForm1.EColorChange(Sender: TObject);
begin
   MatObj.Material.Properties.EmissionColor.ColorVector:=RColorEditor4.Color;
end;

procedure TForm1.SColorChange(Sender: TObject);
begin
   MatObj.Material.Properties.SpecularColor.ColorVector:=RColorEditor3.Color;
end;

procedure TForm1.TextureAdded(Sender: TObject);
begin
  tvTextures.Items.Add(nil,TTextureLibrary(Sender).Last.Name);
end;

procedure TForm1.SetFromTexture;
var tw: TTextureWraps;
begin
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  cbMagFilter.ItemIndex:=byte(ObjProp.Texture.MagFilter);
  cbMinFilter.ItemIndex:=byte(ObjProp.Texture.MinFilter);
  cbWrapS.ItemIndex:=byte(ObjProp.Texture.WrapS);
  cbWrapT.ItemIndex:=byte(ObjProp.Texture.WrapT);
  tw:=TTextureWraps(cbWrapS.ItemIndex);
  ObjProp.Texture.WrapS:=tw;
  tw:=TTextureWraps(cbWrapT.ItemIndex);
  ObjProp.Texture.WrapT:=tw;
  cbMatBlend.ItemIndex:=byte(ObjProp.Texture.TextureMode);
end;

procedure TForm1.cbMinFilterChange(Sender: TObject);
begin
  MatObj.Texture.MinFilter:=TMinFilter(cbMinFilter.ItemIndex);
end;

procedure TForm1.cbMagFilterChange(Sender: TObject);
begin
  MatObj.Texture.MagFilter:=TMagFilter(cbMagFilter.ItemIndex);
end;

procedure TForm1.cbMatBlendChange(Sender: TObject);
begin
  MatObj.Texture.TextureMode:=TTextureCombines(cbMatBlend.ItemIndex);
end;

function TForm1.LoadTexture(filename: string): TTexture;
var tex: TTexture;
    s: string;
begin
  tex:=TTexture.CreateFromFile(filename);
  s:=ExtractFileName(filename);
  tex.Name:=CutString(s,length(s)-4);

  result:=tex;
end;

procedure TForm1.tfBrowseClick(Sender: TObject);
begin
   if not OpenPictureDialog1.Execute then exit;
   ObjProp.Texture:=LoadTexture(OpenPictureDialog1.FileName);
   ObjProp.Texture.FileName:=OpenPictureDialog1.FileName;
   Mesh.Textures.Add(ObjProp.Texture);
   SetFromTexture;
   cbUseTexture.Checked:=true;
end;


procedure TForm1.tredAlphaTestTrackBarChange(Sender: TObject);
begin
  tredAlphaTest.TrackBarChange(Sender);
end;

procedure TForm1.tvMatlibClick(Sender: TObject);
begin
  if tvMatLib.Selected.Index<>-1 then
  MatObj.AttachMaterial(Mesh.Materials[tvMatLib.Selected.Index]);
end;

procedure TForm1.tvTexturesClick(Sender: TObject);
begin
  if tvTextures.Selected.Index<>-1 then
  MatObj.AttachTexture(Mesh.Textures[tvTextures.Selected.Index]);
end;

procedure TForm1.ComboBox12Change(Sender: TObject);
const af: array[0..7] of cardinal = (
        GL_NEVER,GL_LESS,GL_EQUAL,GL_LEQUAL,GL_GREATER,
        GL_NOTEQUAL,GL_GEQUAL,GL_ALWAYS);
begin
  MatObj.Blending.AlphaFunc:=af[ComboBox12.ItemIndex];
end;

procedure TForm1.cbWrapSChange(Sender: TObject);
begin
  MatObj.Texture.WrapS:=TTextureWraps(cbWrapS.ItemIndex);
end;

procedure TForm1.cbWrapTChange(Sender: TObject);
begin
  MatObj.Texture.WrapT:=TTextureWraps(cbWrapT.ItemIndex);
end;

end.
