unit uTBN;

interface

uses VectorGeometry, VectorLists, uVectorLists, uVBO;

procedure CalculateTangentsAndBinormals(var buff: TVBOBuffer);
procedure CalcTriangleBasis( const E,F,G: TAffineVector;
  const tc1,tc2,tc3: TAffineVector; var tangentX,	tangentY: TAffineVector);
function ClosestPointOnLine( const a,b,p: TAffineVector ): TAffineVector;
function Ortogonalize( const v1, v2: TAffineVector ): TAffineVector;

implementation

procedure CalculateTangentsAndBinormals(var buff: TVBOBuffer);
var i,j,ind0,ind1,ind2: integer;
    Normals, Tangents, Binormals: TAffineVectorList;
    v1,v2,v3: TAffineVector;
    t1,t2,t3: TAffineVector;
    T,B: TAffineVector;
    attr: PVBOAttribute;
    smesh: PSubMesh;
begin
  Normals:=buff.Normals;
  Tangents:=TAffineVectorList.Create;
//  AddAttribute3f(buff,'Tangent');
  Binormals:=TAffineVectorList.Create;
//   AddAttribute3f(buff,'Binormal');
  Tangents.Count:=Normals.Count;
  Binormals.Count:=Normals.Count;
  for i:=0 to Normals.Count-1 do begin
    Tangents[i]:=NullVector; Binormals[i]:=NullVector; end;
  // проходимся по всем треугольникам, и для каждого считаем базис
  if Buff.SubMeshes.Count>0 then begin
    for j:=0 to Buff.SubMeshes.Count-1 do begin
      smesh:=Buff.SubMeshes[j];
      for i:=0 to smesh.PrimitiveCount-1 do begin
        ind0:=buff.Indices[i*3+smesh.StartingIndex];
        ind1:=buff.Indices[i*3+1+smesh.StartingIndex];
        ind2:=buff.Indices[i*3+2+smesh.StartingIndex];
        v1:=buff.Vertexes[ind0]; t1:=buff.TexCoords[ind0];
        v2:=buff.Vertexes[ind1]; t2:=buff.TexCoords[ind1];
        v3:=buff.Vertexes[ind2]; t3:=buff.TexCoords[ind2];
        CalcTriangleBasis( v1, v2, v3, t1, t2, t3, t, b );
        tangents.TranslateItem(ind0,t);
        tangents.TranslateItem(ind1,t);
        tangents.TranslateItem(ind2,t);

        binormals.TranslateItem(ind0,b);
        binormals.TranslateItem(ind1,b);
        binormals.TranslateItem(ind2,b);
      end;
    end;
  end else
  for i:=0 to (buff.Indices.Count div 3)-1 do begin
    ind0:=buff.Indices[i*3];  ind1:=buff.Indices[i*3+1];
    ind2:=buff.Indices[i*3+2];
    v1:=buff.Vertexes[ind0]; t1:=buff.TexCoords[ind0];
    v2:=buff.Vertexes[ind1]; t2:=buff.TexCoords[ind1];
    v3:=buff.Vertexes[ind2]; t3:=buff.TexCoords[ind2];
    CalcTriangleBasis( v1, v2, v3, t1, t2, t3, t, b );
    tangents.TranslateItem(ind0,t);
    tangents.TranslateItem(ind1,t);
    tangents.TranslateItem(ind2,t);

    binormals.TranslateItem(ind0,b);
    binormals.TranslateItem(ind1,b);
    binormals.TranslateItem(ind2,b);
  end;
  tangents.Normalize; binormals.Normalize;
  for i:=0 to Normals.Count-1 do begin
    tangents[i]:=Ortogonalize(Normals[i],tangents[i]);
    binormals[i]:=Ortogonalize(Normals[i],binormals[i]);
  end;
  new(attr); attr.Data:=tangents.List; attr.Size:=tangents.DataSize;
  attr.CCount:=3; attr.CSize:=sizeof(TAffineVector); attr.CType:=5126;
  attr.Name:='Tangent'; attr.DataHandler:=tangents; attr.AttrType:=atUserAttrib;
  attr.Location:=-1; buff.AttribList.Add(attr);
  new(attr); attr.Data:=binormals.List; attr.Size:=binormals.DataSize;
  attr.CCount:=3; attr.CSize:=sizeof(TAffineVector); attr.CType:=5126;
  attr.Name:='Binormal'; attr.DataHandler:=binormals; attr.AttrType:=atUserAttrib;
  attr.Location:=-1; buff.AttribList.Add(attr);
end;

procedure CalcTriangleBasis( const E,F,G: TAffineVector;
  const tc1,tc2,tc3: TAffineVector; var tangentX,	tangentY: TAffineVector);
var P,Q: TAffineVector;
    s1,t1,s2,t2: single;
    pqMatrix: array[0..1,0..2] of single;
    stMatrix: array[0..1,0..1] of single;
    tbMatrix: array[0..1,0..2] of single;
    temp: single;
begin
  P:=VectorSubtract(F,E);	Q:=VectorSubtract(G,E);
  s1:=tc2[0]-tc1[0]; t1:=tc2[1]-tc1[1];
  s2:=tc3[0]-tc1[0]; t2:=tc3[1]-tc1[1];

  pqMatrix[0][0]:=P[0];
  pqMatrix[0][1]:=P[1];
  pqMatrix[0][2]:=P[2];
  pqMatrix[1][0]:=Q[0];
  pqMatrix[1][1]:=Q[1];
  pqMatrix[1][2]:=Q[2];

  temp:=1.0 / ( s1 * t2 - s2 * t1);
  stMatrix[0][0]:= t2 * temp;
  stMatrix[0][1]:=-t1 * temp;
  stMatrix[1][0]:=-s2 * temp;
  stMatrix[1][1]:= s1 * temp;

  tbMatrix[0][0]:=stMatrix[0][0] * pqMatrix[0][0] + stMatrix[0][1] * pqMatrix[1][0];
  tbMatrix[0][1]:=stMatrix[0][0] * pqMatrix[0][1] + stMatrix[0][1] * pqMatrix[1][1];
  tbMatrix[0][2]:=stMatrix[0][0] * pqMatrix[0][2] + stMatrix[0][1] * pqMatrix[1][2];
  tbMatrix[1][0]:=stMatrix[1][0] * pqMatrix[0][0] + stMatrix[1][1] * pqMatrix[1][0];
  tbMatrix[1][1]:=stMatrix[1][0] * pqMatrix[0][1] + stMatrix[1][1] * pqMatrix[1][1];
  tbMatrix[1][2]:=stMatrix[1][0] * pqMatrix[0][2] + stMatrix[1][1] * pqMatrix[1][2];
  SetAffineVector(tangentX, tbMatrix[0][0], tbMatrix[0][1], tbMatrix[0][2]);
  SetAffineVector(tangentY, tbMatrix[1][0], tbMatrix[1][1], tbMatrix[1][2]);
  NormalizeVector(tangentX); NormalizeVector(tangentY);
end;

function ClosestPointOnLine( const a,b,p: TAffineVector ): TAffineVector;
var c,v: TAffineVector;
    t,d: single;
begin
  c:=VectorSubtract(p,a);
  V:=VectorSubtract(b,a);
  d:=VectorLength(V); NormalizeVector(V);
  t:=VectorDotProduct(V,c);
  // проверка на выход за границы отрезка
  if ( t < 0.0 ) then begin result:=a; exit; end;
  if ( t > d ) then begin	result:=b; exit; end;
    // Вернем точку между a и b
  ScaleVector(V,t);
  result:=VectorAdd(a,V);
end;

function Ortogonalize( const v1, v2: TAffineVector ): TAffineVector;
var v2ProjV1, res, nv1: TAffineVector;
begin
  nv1:=VectorNegate(v1);
  v2ProjV1:=ClosestPointOnLine( v1, nv1, v2 );
  res:=VectorSubtract(v2,v2ProjV1);
  result:=VectorNormalize(res);
end;

end.
