unit CalculatePhysics;

interface

uses Classes,Windows,SysUtils,GLVectorFileObjects,VectorTypes,VectorGeometry,
     vboMesh,uVBO;

const PDLLName='CalculatePhysics.dll';

function CalculateVerticesMass(Obj:TGLFreeForm):TVector4f; stdcall; external PDLLName;
function CalculateBBox(Obj:TGLFreeForm): TAffineVector; stdcall; external PDLLName;
//function CalculateBBoxVBO(Obj:TVBOMeshObject):TAffineVector; //stdcall; external PDLLName;
function CalculateVerticesMassVBO(Obj:TVBOMeshObject):TVector4f; //stdcall; external PDLLName;
procedure Check; stdcall; external PDLLName;

implementation

function CalculateVerticesMassVBO(Obj:TVBOMeshObject):TVector4f;
var i:integer; tx,ty,tz:real; buff:PVBOBuffer;
begin
 buff:=Obj.MeshList[0]; result:=VectorMake(0,0,0); tx:=0; ty:=0; tz:=0;
 for i:=0 to buff.VertexCount-1 do begin
   tx:=tx+buff.Vertexes[i][0];
   ty:=ty+buff.Vertexes[i][1];
   tz:=tz+buff.Vertexes[i][2];
 end;
 result:=VectorMake(tx/buff.VertexCount,ty/buff.VertexCount,tz/buff.VertexCount)
end;

end.
