unit uOctree;

interface

uses Classes, uVBO, GeometryBB, VectorGeometry, VectorLists;

Type
  POctreeLevel = ^TOctreeLevel;
  TOctreeLevel = record
    Level: integer;
    Extents: TExtents;
    trilist: TAffineVectorList;
    HasLeafs: boolean;
    Triangles: array of integer;
    Octets: array[0..7] of POctreeLevel;
    BigTriangles: array of integer;
  end;

  TIntersectInfo = record
    iPoint, iNormal: TVector;
    triIndex: integer;
    MeshIndex: integer;
  end;

  TOctree = class
  private
    FGlobalExtents: TExtents;
    FMeshCount: integer;
    FMeshes: array of POctreeLevel;
    FMaxLevel: integer;
    FBuilded: boolean;
    FIntersected: array of TIntersectInfo;

    function GetMeshOctree(Index: Integer): TOctreeLevel;
    function PointInExtents(const v: TAffineVector; const Extents: TExtents): boolean;
    function TriInExtents( const v1,v2,v3: TAffineVector; const Extents: TExtents): boolean;
    function GetOctetExtents(Octet: byte; const Extents: TExtents): TExtents;
    procedure OctreeLeaf(Maxlevel: integer; var Leaf: POctreeLevel);
    function ExtentsIntersect(const Extents: TExtents; const rayStart,
      rayVector: TVector; iPoint:PVector=nil): boolean;
    function SphereExtentsIntersect(const Pos: TVector; R: single;
      const Extents: TExtents): boolean;
    function SphereTriangleIntersect(const Pos: TVector; R2: single;
      const v1,v2,v3: TAffineVector): boolean;
    function ExtentsFromTriangles(OLevel: POctreeLevel): TExtents;
    function OctreeLevelRayCast(OLevel: POctreeLevel; Pos, Dir: TVector;
      iPos, iNorm: PVector): boolean;
    function OctreeLevelSprCol(OLevel: POctreeLevel; const Pos: TVector;
      R:single): boolean;
    procedure FreeLeaf(var Leaf: POctreeLevel);
    procedure Load(Filename: string);
  public
    property GlobalExtents: TExtents read FGlobalExtents;
    property MeshCount: integer read FMeshCount;
    property MesheOctree[Index: Integer]: TOctreeLevel read GetMeshOctree;default;
    property MaxLevel: integer read FMaxLevel;

    function RayCastIntersect(Pos, Dir: TVector; iPos: PVector=nil; iNorm: PVector=nil): boolean;
    function SphereCollision(Pos: TVector; R: single): boolean;
    procedure Save(Filename: string);

    constructor Create(MeshList: TList; aLevel: integer=3);overload;
    constructor Create(aTriList: TAffineVectorList; aLevel: integer=3);overload;
    constructor Create(Filename: string);overload;
    destructor Destroy;override;
  end;

implementation

function min(a,b: single): single;
begin if a<b then result:=a else result:=b; end;

function max(a,b: single): single;
begin if a>b then result:=a else result:=b; end;

function MinVector(const v1,v2: TAffineVector): TAffineVector;
begin
  result[0]:=min(v1[0],v2[0]);
  result[1]:=min(v1[1],v2[1]);
  result[2]:=min(v1[2],v2[2]);
end;

function MaxVector(const v1,v2: TAffineVector): TAffineVector;
begin 
  result[0]:=max(v1[0],v2[0]);
  result[1]:=max(v1[1],v2[1]);
  result[2]:=max(v1[2],v2[2]);
end;


{ TOctree }

procedure TOctree.FreeLeaf(var Leaf: POctreeLevel);
var j: integer;
begin
  if Leaf=nil then exit;
  for j:=0 to 7 do FreeLeaf(Leaf.Octets[j]);
  dispose(Leaf); Leaf:=nil;
end;

destructor TOctree.Destroy;
var i,j: integer;
begin
  for i:=0 to FMeshCount-1 do begin
    for j:=0 to 7 do FreeLeaf(FMeshes[i].Octets[j]);
    FMeshes[i].trilist.Free;
    dispose(FMeshes[i]);
  end;
  inherited;
end;

function TOctree.GetMeshOctree(Index: Integer): TOctreeLevel;
begin
  result:=FMeshes[Index]^;
end;

function TOctree.PointInExtents(const v: TAffineVector; const Extents: TExtents): boolean;
begin
  if (v[0]>=Extents.emin[0]) and (v[0]<=Extents.emax[0])
  and(v[1]>=Extents.emin[1]) and (v[1]<=Extents.emax[1])
  and(v[2]>=Extents.emin[2]) and (v[2]<=Extents.emax[2])
  then result:=true else result:=false;
end;

function TOctree.TriInExtents( const v1,v2,v3: TAffineVector; const Extents: TExtents): boolean;
begin
  if PointInExtents(v1,Extents) or PointInExtents(v2,Extents)
  or PointInExtents(v3,Extents) then result:=true else result:=false;
end;

function TOctree.GetOctetExtents(Octet: byte; const Extents: TExtents): TExtents;
var ext: TExtents;
    n: byte;
begin
  ext:=Extents; n:=Octet;
  if octet<4 then begin
     ext.emin[1]:=(Extents.emax[1]+Extents.emin[1])/2;
     n:=octet;
  end else begin
     ext.emax[1]:=(Extents.emax[1]+Extents.emin[1])/2;
     n:=n-4;
  end;
  case n of
    0: begin
       ext.emax[0]:=(Extents.emax[0]+Extents.emin[0])/2;
       ext.emin[2]:=(Extents.emax[2]+Extents.emin[2])/2;
    end;
    1: begin
       ext.emax[0]:=(Extents.emax[0]+Extents.emin[0])/2;
       ext.emax[2]:=(Extents.emax[2]+Extents.emin[2])/2;
    end;
    2: begin
       ext.emin[0]:=(Extents.emax[0]+Extents.emin[0])/2;
       ext.emax[2]:=(Extents.emax[2]+Extents.emin[2])/2;
    end;
    3: begin
       ext.emin[0]:=(Extents.emax[0]+Extents.emin[0])/2;
       ext.emin[2]:=(Extents.emax[2]+Extents.emin[2])/2;
    end;
    else assert(false,'Unknown octet index');
  end;
  result:=ext;
end;

procedure TOctree.Load(Filename: string);
var FStream: TFileStream;
    i,j,n: integer;
    Leaf: POctreeLevel;
    v: TAffineVector;
    b: byte;
    tl: TAffineVectorList;
  procedure LoadOctreeLeaf(var Leaf: POctreeLevel);
  var j,n: integer;
  begin
    FStream.Read(Leaf.Level,4);
    FStream.Read(Leaf.Extents,sizeof(TExtents));
    FStream.Read(b,1); Leaf.HasLeafs:=boolean(b);
    FStream.Read(n,4); setlength(Leaf.Triangles,n);
    FStream.Read(Leaf.Triangles[0],4*n);
//    for j:=0 to n-1 do FStream.Read(Leaf.Triangles[j],4);
    for j:=0 to 7 do begin
      FStream.Read(n,4);
      if assigned(pointer(n)) then begin
        new(Leaf.Octets[j]);
        Leaf.Octets[j].trilist:=Leaf.trilist;
        LoadOctreeLeaf(Leaf.Octets[j]);
      end else Leaf.Octets[j]:=nil;
    end;
  end;

begin
  FStream:=TFileStream.Create(FileName,0);
  FStream.Read(FGlobalExtents,sizeof(FGlobalExtents));
  FStream.Read(FMeshCount,4);
  FStream.Read(FMaxLevel,4);
  setlength(FMeshes,FMeshCount);
  for i:=0 to high(FMeshes) do begin
    new(Leaf); FMeshes[i]:=Leaf;
    FStream.Read(Leaf.Level,4);
    FStream.Read(Leaf.Extents,sizeof(TExtents));
    FStream.Read(n,4);
    Leaf.trilist:=TAffineVectorList.Create;
    Leaf.trilist.Count:=n; tl:=Leaf.trilist;
    FStream.read(Leaf.trilist.List[0],sizeof(TAffineVector)*Leaf.trilist.Count);
{    for j:=0 to Leaf.trilist.Count-1 do begin
      FStream.read(v,sizeof(TAffineVector));
      Leaf.trilist[j]:=v;
    end;
}
    FStream.Read(b,1); Leaf.HasLeafs:=boolean(b);
    FStream.Read(n,4); setlength(Leaf.Triangles,n);
    //for j:=0 to n-1 do FStream.Read(Leaf.Triangles[j],4);
    FStream.Read(Leaf.Triangles[0],4*n);
    for j:=0 to 7 do begin
      FStream.Read(n,4);
      if assigned(pointer(n)) then begin
        new(Leaf.Octets[j]); Leaf.Octets[j].trilist:=tl;
        LoadOctreeLeaf(Leaf.Octets[j]);
      end else Leaf.Octets[j]:=nil;
    end;
  end;
  Fstream.Free; FBuilded:=true;
end;

constructor TOctree.Create(Filename: string);
begin
  inherited Create;
  Load(Filename);
end;

constructor TOctree.Create(aTriList: TAffineVectorList; aLevel: integer);
var j,k,n,tri: integer;
    triCount: integer;
    ol: POctreeLevel;
begin
  inherited Create;
  assert((aTriList<>nil),'Mesh List in not assigned!');
  assert((aLevel>1),'Octree Levels must be > 1!');
  FMaxLevel:=aLevel;
  aTriList.GetExtents(FGlobalExtents.emin, FGlobalExtents.emax);
  FMeshCount:=1;
  SetLength(FMeshes,FMeshCount);
  new(FMeshes[0]);
  with FMeshes[0]^ do begin
    trilist:=aTriList;
    triCount:=trilist.Count div 3;
    Extents:=FGlobalExtents;
    Level:=0; if aLevel>0 then HasLeafs:=true;
    SetLength(Triangles,triCount);
    for j:=0 to triCount-1 do Triangles[j]:=j*3;
    if aLevel=0 then begin
       HasLeafs:=false;
       for k:=0 to 7 do Octets[k]:=nil;
    end else begin
       for k:=0 to 7 do begin
         new(ol); Octets[k]:=ol;
         ol.Extents:=GetOctetExtents(k,Extents);
         ol.trilist:=trilist;
         setlength(ol.Triangles, triCount);
         ol.Level:=1; tri:=0;
         for j:=0 to TriCount-1 do begin
            n:=Triangles[j];
            if IntersectTriangleBox(trilist[n],trilist[n+1],trilist[n+2],
              ol.Extents.emin, ol.Extents.emax) or
              TriInExtents(trilist[n],trilist[n+1],trilist[n+2],ol.Extents)
              then begin ol.Triangles[tri]:=n; inc(tri); end;
         end;
         setlength(ol.Triangles,tri);
         if tri=0 then begin
            dispose(ol); Octets[k]:=nil;
         end else begin
//           ol.Extents:=ExtentsFromTriangles(ol);
           OctreeLeaf(alevel,ol);
           if assigned(ol) then ol.Extents:=ExtentsFromTriangles(ol);
         end;
       end;
    end;
    if HasLeafs then begin Triangles:=nil; end;
  end;
  FBuilded:=true;
end;

constructor TOctree.Create(MeshList: TList; aLevel: integer);
var i,j,k,n,tri: integer;
    p: PVBOBuffer;
    triCount: integer;
    ol: POctreeLevel;
begin
  inherited Create;
  assert((MeshList<>nil),'Mesh List in not assigned!');
  assert((aLevel>1),'Octree Levels must be > 1!');
  FMaxLevel:=aLevel;
  FGlobalExtents:=GetExtentsOfList(MeshList);
  SetLength(FMeshes,MeshList.Count);
  FMeshCount:=MeshList.Count;
  for i:=0 to FMeshCount-1 do begin
    new(FMeshes[i]);
    with FMeshes[i]^ do begin
      p:=MeshList[i];
      trilist:=TAffineVectorList.Create;
      ExtractTriangles(p^,trilist);
      triCount:=trilist.Count div 3;
      Extents.emin:=p.emin;
      Extents.emax:=p.emax;
      Level:=0; if aLevel>0 then HasLeafs:=true;
      SetLength(Triangles,triCount);
      for j:=0 to triCount-1 do Triangles[j]:=j*3;
      if aLevel=0 then begin
         HasLeafs:=false;
         for k:=0 to 7 do Octets[k]:=nil;
      end else begin
         for k:=0 to 7 do begin
           new(ol); Octets[k]:=ol;
           ol.Extents:=GetOctetExtents(k,Extents);
           ol.trilist:=trilist;
           setlength(ol.Triangles, triCount);
           ol.Level:=1; tri:=0;
           for j:=0 to TriCount-1 do begin
              n:=Triangles[j];
              if IntersectTriangleBox(trilist[n],trilist[n+1],trilist[n+2],
                ol.Extents.emin, ol.Extents.emax) then begin
                  ol.Triangles[tri]:=n; inc(tri);
                end;
           end;
           setlength(ol.Triangles,tri);
           if tri=0 then begin
              dispose(ol); Octets[k]:=nil;
           end else begin
//             ol.Extents:=ExtentsFromTriangles(ol);
             OctreeLeaf(alevel,ol);
             if assigned(Octets[k]) then
               Octets[k].Extents:=ExtentsFromTriangles(Octets[k]);
           end;
         end;
      end;
      if HasLeafs then begin Triangles:=nil; end;
    end;
  end;
  FBuilded:=true;
end;

procedure TOctree.OctreeLeaf(Maxlevel: integer; var Leaf: POctreeLevel);
var j,k,n,tri: integer;
    triCount, childs: integer;
    ol: POctreeLevel;
begin
  if leaf=nil then exit;
  if Leaf.Level=MaxLevel then begin
     for k:=0 to 7 do Leaf.Octets[k]:=nil;
     Leaf.HasLeafs:=false; exit;
  end;

  TriCount:=high(Leaf.Triangles)+1; childs:=0;
  for k:=0 to 7 do begin
    new(ol); Leaf.Octets[k]:=ol;
    ol.Extents:=GetOctetExtents(k,Leaf.Extents);
    ol.trilist:=Leaf.trilist;
    setlength(ol.Triangles,TriCount);
    ol.Level:=Leaf.Level+1;
    ol.HasLeafs:=ol.Level<MaxLevel;
    tri:=0;
    for j:=0 to TriCount-1 do begin
        n:=Leaf.Triangles[j];
        if IntersectTriangleBox(Leaf.trilist[n],Leaf.trilist[n+1],Leaf.trilist[n+2],
          ol.Extents.emin, ol.Extents.emax) or
          TriInExtents(Leaf.trilist[n],Leaf.trilist[n+1],Leaf.trilist[n+2],ol.Extents)
          then begin ol.Triangles[tri]:=n; inc(tri); end;
    end;
    setlength(ol.Triangles,tri);
    if tri=0 then begin
       dispose(ol); Leaf.Octets[k]:=nil;
    end else begin
     inc(childs);
//     ol.Extents:=ExtentsFromTriangles(ol);
     OctreeLeaf(MaxLevel,ol);
     if assigned(Leaf.Octets[k]) then
       Leaf.Octets[k].Extents:=ExtentsFromTriangles(Leaf.Octets[k]);
    end;
  end;
  if childs>0 then Leaf.HasLeafs:=true else Leaf.HasLeafs:=false;
  if Leaf.HasLeafs then begin leaf.Triangles:=nil;end;
end;

function TOctree.ExtentsFromTriangles(OLevel: POctreeLevel): TExtents;
var i,j,n: integer;
    v: TAffineVector;
    e: TExtents;
begin
result:=OLevel.Extents; exit;
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  assert(OLevel<>nil,'Not Assigned Leaf!');
  if length(OLevel.Triangles)=0 then begin
    result:=OLevel.Extents; exit;
  end;

  e.emin:=OLevel.trilist[OLevel.Triangles[0]]; e.emax:=e.emin;
  for i:=1 to high(OLevel.Triangles) do begin
    v:=OLevel.trilist[OLevel.Triangles[i]];
    if PointInExtents(v,OLevel.Extents) then begin
      e.emin:=MinVector(v,e.emin);
      e.emax:=MaxVector(v,e.emax);
    end else begin
{      n:=length(OLevel.BigTriangles);
      setlength(OLevel.BigTriangles,n+3);
      j:=(i div 3)*3;
      OLevel.BigTriangles[n]:=OLevel.Triangles[j];
      OLevel.BigTriangles[n+1]:=OLevel.Triangles[j+1];
      OLevel.BigTriangles[n+2]:=OLevel.Triangles[j+2];
}
      if v[0]<=OLevel.Extents.emin[0] then e.emin[0]:=OLevel.Extents.emin[0];
      if v[1]<=OLevel.Extents.emin[1] then e.emin[1]:=OLevel.Extents.emin[1];
      if v[2]<=OLevel.Extents.emin[2] then e.emin[2]:=OLevel.Extents.emin[2];
      if v[0]>=OLevel.Extents.emax[0] then e.emax[0]:=OLevel.Extents.emax[0];
      if v[1]>=OLevel.Extents.emax[1] then e.emax[1]:=OLevel.Extents.emax[1];
      if v[2]>=OLevel.Extents.emax[2] then e.emax[2]:=OLevel.Extents.emax[2];
    end;
  end; result:=e;
end;

function TOctree.ExtentsIntersect(const Extents: TExtents; const rayStart,
  rayVector: TVector; iPoint: PVector): boolean;
var bb: TAABB;
begin
  bb:=TAABB(Extents);
  result:=RayCastAABBIntersect(RayStart,RayVector,bb);
end;

function TOctree.OctreeLevelRayCast(OLevel: POctreeLevel; Pos, Dir: TVector;
  iPos, iNorm: PVector): boolean;
var i: integer;
    ip,nm,fp,fv,v: TVector;
    f,t: boolean;
    v1,v2,v3: TAffineVector;
    d,dist: single;
begin
  result:=false; f:=false; if not assigned(OLevel) then exit;
  if not ExtentsIntersect(OLevel.Extents,Pos,Dir) then exit;
  if OLevel.HasLeafs then begin
    for i:=0 to 7 do begin
      if (iPos=nil) and (iNorm=nil) then
        t:=OctreeLevelRayCast(OLevel.Octets[i],Pos,Dir,nil,nil)
      else t:=OctreeLevelRayCast(OLevel.Octets[i],Pos,Dir,@ip,@nm);
      if t then begin
        result:=true; f:=true;
        if (iPos=nil) and (iNorm=nil) then exit;
      end;
    end;
  end else begin
    for i:=0 to high(OLevel.Triangles) do begin
       v1:=OLevel.trilist[OLevel.Triangles[i]];
       v2:=OLevel.trilist[OLevel.Triangles[i]+1];
       v3:=OLevel.trilist[OLevel.Triangles[i]+2];
       t:=RayCastTriangleIntersect(Pos,Dir,v1,v2,v3,@ip,@nm);

       if t then begin
         if (iPos=nil) and (iNorm=nil) then begin result:=true; exit; end;
         setlength(FIntersected,length(FIntersected)+1);
         with FIntersected[high(FIntersected)] do begin
           iPoint:=ip; iNormal:=nm;
           triIndex:=OLevel.Triangles[i];
         end; f:=true;
       end;
    end;
  end;
  result:=f;
  if f then begin
    if iPos<>nil then iPos^:=fp;
    if iNorm<>nil then iNorm^:=fv;
  end;
end;

function TOctree.OctreeLevelSprCol(OLevel: POctreeLevel; const Pos: TVector;
  R: single): boolean;
var i: integer;
    ip,nm: TVector;
    f: boolean;
    v1,v2,v3: TAffineVector;
    r2: single;
begin
  result:=false; if not assigned(OLevel) then exit;
  if not SphereExtentsIntersect(Pos,R,OLevel.Extents) then exit;
  r2:=r*r;
  if OLevel.HasLeafs then begin
    for i:=0 to 7 do begin
      f:=OctreeLevelSprCol(OLevel.Octets[i],Pos,R);
      if f then begin result:=true; exit; end;
    end;
  end else begin
    for i:=0 to high(OLevel.Triangles) do begin
       v1:=OLevel.trilist[OLevel.Triangles[i]];
       v2:=OLevel.trilist[OLevel.Triangles[i]+1];
       v3:=OLevel.trilist[OLevel.Triangles[i]+2];
       f:=SphereTriangleIntersect(Pos,r2,v1,v2,v3);
       if f then begin result:=f; exit; end;
    end;
  end;
end;

function TOctree.RayCastIntersect(Pos, Dir: TVector; iPos, iNorm: PVector): boolean;
var i: integer;
    OL: POctreeLevel;
    f,t: boolean;
    ip,inm: TVector;
    fp,fv,v: TVector;
    dist,d: single;
begin
  result:=false; f:=false; dist:=high(integer);
  setlength(FIntersected,0);
  if not ExtentsIntersect(FGlobalExtents,Pos,Dir) then exit;
  for i:=0 to FMeshCount-1 do begin
    ol:=FMeshes[i];
    if ExtentsIntersect(OL.Extents,Pos,Dir) then begin
      if (iPos=nil) and (iNorm=nil) then
        t:=OctreeLevelRayCast(OL,Pos,Dir,nil,nil)
      else t:=OctreeLevelRayCast(OL,Pos,Dir,@ip,@inm);

      if t then begin
         result:=true; if (iPos=nil) and (iNorm=nil) then exit;
         FIntersected[high(FIntersected)].MeshIndex:=i;
      end;
    end;
  end;
  dist:=1e40;
  for i:=0 to high(FIntersected) do begin
    d:=VectorDistance2(FIntersected[i].iPoint,Pos);
    if d<dist then begin
      fp:=FIntersected[i].iPoint;
      fv:=FIntersected[i].iNormal;
      dist:=d;
    end;
  end;
  if iPos<>nil then iPos^:=fp;
  if iNorm<>nil then iNorm^:=fv;
  finalize(FIntersected);
end;

procedure TOctree.Save(Filename: string);
var FStream: TFileStream;
    i,j,n: integer;
    Leaf: POctreeLevel;
    v: TAffineVector;
  procedure SaveOctreeLeaf(Leaf: POctreeLevel);
  var j,n: integer;
  begin
    FStream.Write(integer(Leaf),4);
    if not assigned(Leaf) then exit;

    FStream.Write(Leaf.Level,4);
    FStream.Write(Leaf.Extents,sizeof(TExtents));
    FStream.Write(byte(Leaf.HasLeafs),1);
    n:=length(Leaf.Triangles);
    FStream.Write(n,4);
    for j:=0 to n-1 do FStream.Write(Leaf.Triangles[j],4);
    for j:=0 to 7 do SaveOctreeLeaf(Leaf.Octets[j]);
  end;

begin
  assert(FBuilded=true,'Octree need be builded before saving!');
  FStream:=TFileStream.Create(FileName,fmCreate);
  FStream.Write(FGlobalExtents,sizeof(FGlobalExtents));
  FStream.Write(FMeshCount,4);
  FStream.Write(FMaxLevel,4);
  for i:=0 to high(FMeshes) do begin
    Leaf:=FMeshes[i];
    FStream.Write(Leaf.Level,4);
    FStream.Write(Leaf.Extents,sizeof(TExtents));
    FStream.Write(Leaf.trilist.Count,4);
    for j:=0 to Leaf.trilist.Count-1 do begin
      v:=Leaf.trilist[j]; FStream.Write(v,sizeof(TAffineVector));
    end;
    FStream.Write(byte(Leaf.HasLeafs),1);
    n:=length(Leaf.Triangles);
    FStream.Write(n,4);
    for j:=0 to n-1 do FStream.Write(Leaf.Triangles[j],4);
    for j:=0 to 7 do SaveOctreeLeaf(Leaf.Octets[j]);
  end;
  Fstream.Free;
end;

function TOctree.SphereCollision(Pos: TVector; R: single): boolean;
var i:integer;
    OL: POctreeLevel;
    f: boolean;
    ip,inm: TVector;
    fp,fv,v: TVector;
    dist,d: single;
begin
  result:=false; f:=false; dist:=high(integer);
  if not SphereExtentsIntersect(Pos,R,FGlobalExtents) then exit;
  for i:=0 to FMeshCount-1 do begin
    ol:=FMeshes[i];
    if SphereExtentsIntersect(Pos,R,OL.Extents) then begin
      if OctreeLevelSprCol(OL,Pos,R) then begin result:=true; exit; end;
    end;
  end;
end;

function TOctree.SphereExtentsIntersect(const Pos: TVector; R: single;
  const Extents: TExtents): boolean;
var r2: single;
    c: array[0..7] of TAffineVector;
    i: integer;
    p: TAffineVector;
begin
  result:=false; r2:=r*r;
  setAffineVector(c[0],Extents.emin[0],Extents.emax[1],Extents.emin[2]);
  c[1]:=Extents.emin;
  setAffinevector(c[2],Extents.emax[0],Extents.emin[1],Extents.emin[2]);
  setAffinevector(c[3],Extents.emax[0],Extents.emax[1],Extents.emin[2]);

  setAffinevector(c[4],Extents.emin[0],Extents.emax[1],Extents.emax[2]);
  setAffinevector(c[5],Extents.emin[0],Extents.emin[1],Extents.emax[2]);
  setAffinevector(c[6],Extents.emax[0],Extents.emin[1],Extents.emax[2]);
  c[7]:=Extents.emax;

  p:=AffineVectorMake(Pos);
  for i:=0 to 7 do begin
    if VectorDistance2(P,c[i])<r2 then begin result:=true; exit; end;
  end;
  if PointInExtents(p,Extents) then result:=true;

end;

function TOctree.SphereTriangleIntersect(const Pos: TVector; R2: single;
  const v1, v2, v3: TAffineVector): boolean;
var p: TAffineVector;
begin
  p:=AffineVectorMake(Pos);
  result:=(VectorDistance2(p,v1)<=r2) or ((VectorDistance2(p,v2)<=r2))
    or (VectorDistance2(p,v3)<=r2);
end;

end.

