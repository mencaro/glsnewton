unit uFileObj;

interface

uses Classes, SysUtils, VectorGeometry,
     {$IFNDEF DIRECTGL} VectorLists,{$ELSE}uVectorLists,{$ENDIF}
     uVBO, uMiscUtils;

type

  TOBJLoader = class
  private
    FMultiObjectMesh: TMultiObjectMesh;
    FMeshList: TList;
    FTextFile: TTextFileParser;
    FLine: string;
    FVertices: TAffineVectorList;
    FNormals: TAffineVectorList;
    FTexCoords: TAffineVectorList;
    FVIndices: TIntegerList;
    FNIndices: TIntegerList;
    FTIndices: TIntegerList;
    FFaceGroups: TList;
    FIA: TVBOIndiceAdapter;

    function ReadHomogeneousVector: TVector;
    function ReadAffineVector: TAffineVector;
    function GetIndex(var FaceVertices: String; Count: Integer): Integer;
    function getMultiObjectMesh: TMultiObjectMesh;
    function getMeshList: TList;

  protected
    // Raise a class-specific exception
    procedure Error(const msg : String);
    procedure SaveToBinaryStream(aStream: TStream);
    procedure SaveToBinaryFile(FileName: string);

  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromStream(aStream: TStream);
    procedure LoadFromFile(FileName: string);

    property MultiObjectMesh: TMultiObjectMesh read getMultiObjectMesh;
    property MeshList: TList read getMeshList;
  end;

implementation

function TOBJLoader.GetIndex(var FaceVertices: String; Count: Integer): Integer;
var s : String;
begin
   s:=FTextFile.NextToken(FaceVertices, '/');
   Result:=StrToIntDef(s, 0);
   if Result=0 then Result:=-1 // Missing
   else if Result<0 then begin
     { Relative, make absolute. "-1" means last, "-2" second last. }
     Result:=Count+Result
   end else begin
     { Absolute, correct for zero-base. }
     Dec(Result);
   end;
end;

function TOBJLoader.getMeshList: TList;
begin
  if not assigned(FMeshList) then fia.BuildVBOMeshList(FMeshList);
  result:=FMeshList;
end;

function TOBJLoader.GetMultiObjectMesh: TMultiObjectMesh;
begin
  if not assigned(FMultiObjectMesh) then
    fia.BuildMultiObjectMesh(FMultiObjectMesh);
  result:=FMultiObjectMesh;
end;

function TOBJLoader.ReadAffineVector: TAffineVector;
{ Read a vector with a maximum of 3 elements from the current line. }
var i, c : integer;
    f : String;
begin
  FillChar(Result,SizeOf(Result),0); i:=0;
  while (FLine<>'') and (i<3) do begin
    f:=FTextFile.NextToken(FLine, ' '); Val(f, Result[i], c);
    if c<>0 then Error('"'+f+'" is not a valid floating-point constant.');
    inc(i);
  end;
end;

function TOBJLoader.ReadHomogeneousVector: TVector;
{ Read a vector with a maximum of 4 elements from the current line. }
var i, c : Integer;
    f : String;
begin
  FillChar(Result, SizeOf(Result), 0); i:=0;
  while (FLine<>'') and (i<4) do begin
    f:=FTextFile.NextToken(FLine, ' '); Val(f, Result[i], c);
    if c<>0 then Error('"'+f+'" is not a valid floating-point constant.');
    Inc(i);
  end;
end;

procedure TOBJLoader.SaveToBinaryFile(FileName: string);
var s: TFileStream;
begin
  s:=TFileStream.Create(FileName,fmCreate);
  SaveToBinaryStream(s);
  s.Free;
end;

procedure TOBJLoader.SaveToBinaryStream(aStream: TStream);
const ID: ansistring = 'MMO';
      MOMId: byte = 1;
      VMLId: byte = 2;
var iTemp: integer;
    sTemp: ansistring;
    bTemp: byte;
    piTemp: PIntegerVector;
    paTemp: PAffineVectorArray;
    pVBOBuff: PVBOBuffer;
    i: integer;

begin
  astream.WriteBuffer(ID[1],3);   //Write ID to file
  if assigned(FMultiObjectMesh) then begin
    astream.WriteBuffer(MOMId,1); //Write Mesh Collection Type /MultiObjectMesh
    iTemp:=FMultiObjectMesh.ObjectsCount;
    astream.WriteBuffer(iTemp,4); //Write Mesh count to file
    for i:=0 to FMultiObjectMesh.ObjectsCount-1 do begin
      iTemp:=FMultiObjectMesh.MeshObjects[i].IndiceOffset;
      astream.WriteBuffer(iTemp,4); //Write Mesh offset to file
      iTemp:=FMultiObjectMesh.MeshObjects[i].Count;
      astream.WriteBuffer(iTemp,4); //Write Mesh Elements count to file
      sTemp:=FMultiObjectMesh.MeshObjects[i].MaterialName;
      bTemp:=length(sTemp); astream.WriteBuffer(bTemp,1); //Write MatName length to file
      astream.WriteBuffer(sTemp[1],length(sTemp)); //Write Mesh MaterialName to file
      sTemp:=FMultiObjectMesh.MeshObjects[i].GroupName;
      bTemp:=length(sTemp); astream.WriteBuffer(bTemp,1); //Write GroupName length to file
      astream.WriteBuffer(sTemp[1],length(sTemp)); //Write Mesh GroupName to file
    end;
    //write Indices
    iTemp:=FMultiObjectMesh.Indices.Count;
    astream.WriteBuffer(iTemp,4); //Write Indices count to file
    iTemp:=FMultiObjectMesh.Indices.DataSize;
    astream.WriteBuffer(iTemp,4); //Write Indices memsize to file
    piTemp:=FMultiObjectMesh.Indices.List;
    astream.WriteBuffer(piTemp^,iTemp); //Write Indices count to file
    //write Vertices
    iTemp:=FMultiObjectMesh.Vertices.Count;
    astream.WriteBuffer(iTemp,4); //Write Vertices count to file
    iTemp:=FMultiObjectMesh.Vertices.DataSize;
    astream.WriteBuffer(iTemp,4); //Write Vertices memsize to file
    paTemp:=FMultiObjectMesh.Vertices.List;
    astream.WriteBuffer(paTemp^,iTemp); //Write Vertices count to file
    //write TexCoords
    iTemp:=FMultiObjectMesh.TexCoords.Count;
    astream.WriteBuffer(iTemp,4); //Write TexCoords count to file
    iTemp:=FMultiObjectMesh.TexCoords.DataSize;
    astream.WriteBuffer(iTemp,4); //Write TexCoords memsize to file
    paTemp:=FMultiObjectMesh.TexCoords.List;
    astream.WriteBuffer(paTemp^,iTemp); //Write TexCoords count to file
    //write Normals
    iTemp:=FMultiObjectMesh.Normals.Count;
    astream.WriteBuffer(iTemp,4); //Write Normals count to file
    iTemp:=FMultiObjectMesh.Normals.DataSize;
    astream.WriteBuffer(iTemp,4); //Write Normals memsize to file
    paTemp:=FMultiObjectMesh.Normals.List;
    astream.WriteBuffer(paTemp^,iTemp); //Write Normals count to file
  end else if Assigned(FMeshList) then begin
    astream.WriteBuffer(VMLId,1); //Write Mesh Collection Type /VBOMeshList
    iTemp:=FMeshList.Count;
    astream.WriteBuffer(iTemp,4); //Write Mesh count to file
    for i:=0 to FMeshList.Count-1 do begin
      pVBOBuff:=FMeshList[i];
    end;
  end;



end;

// Error
//
procedure TOBJLoader.Error(const msg : String);
begin
   if assigned(FTextFile) then
     assert(false,'Line '+inttostr(FTextFile.LineNo)+': '+Msg)
   else assert(false,'File not open');
end;

constructor TOBJLoader.Create;
begin
  inherited;
  FTextFile:=nil;
  FVertices:=TAffineVectorList.Create;
  FNormals:=TAffineVectorList.Create;
  FTexCoords:=TAffineVectorList.Create;
  FVIndices:=TIntegerList.Create;
  FNIndices:=TIntegerList.Create;
  FTIndices:=TIntegerList.Create;
  FFaceGroups:=TList.Create;
  FIA:=TVBOIndiceAdapter.Create;
end;

destructor TOBJLoader.Destroy;
begin
  if assigned(FTextFile) then FTextFile.Free;
  FVertices.Free; FNormals.Free; FTexCoords.Free;
  FVIndices.Free; FNIndices.Free; FTIndices.Free;
  FreeList(FFaceGroups);
  FIA.Free;
  inherited;
end;

// LoadFromStream
//
procedure TOBJLoader.LoadFromFile(FileName: string);
var s: TFileStream;
begin
  s:=TFileStream.Create(FileName,fmOpenRead);
  LoadFromStream(s); s.Free;
end;

procedure TOBJLoader.LoadFromStream(aStream:TStream);
var
   hv : THomogeneousVector;
   av : TAffineVector;

   procedure GetFaceIndices(faceVertices : String; var vIdx, tIdx, nIdx: integer);
   begin
      vIdx:=(GetIndex(faceVertices, FVertices.Count));
      tIdx:=(GetIndex(faceVertices, FTexCoords.Count));
      nIdx:=(GetIndex(faceVertices, FNormals.Count));
   end;

   procedure ReadFace(const curMtlName : String);
   var faceVertices : String;
       n: integer;
       vIdx, tIdx, nIdx: integer;
       vIdx1, tIdx1, nIdx1: integer;
       vIdx2, tIdx2, nIdx2: integer;
   begin
      if FLine<>'' then begin
        n:=0; vIdx:=-1; tIdx:=-1; nIdx:=-1;
        while FLine<>'' do begin
           faceVertices:=FTextFile.NextToken(FLine, ' ');
           vIdx2:=vIdx; tIdx2:=tIdx; nIdx2:=nIdx;
           GetFaceIndices(faceVertices, vIdx, tIdx, nIdx); inc(n);
           if n=1 then begin vIdx1:=vIdx; tIdx1:=tIdx; nIdx1:=nIdx; end;
           if n>3 then begin
           //Split poly to triangles
             FVIndices.Add(vIdx1); FTIndices.Add(tIdx1); FNIndices.Add(nIdx1);
             FVIndices.Add(vIdx2); FTIndices.Add(tIdx2); FNIndices.Add(nIdx2);
             FVIndices.Add(vIdx); FTIndices.Add(tIdx); FNIndices.Add(nIdx);
           end else begin
             FVIndices.Add(vIdx); FTIndices.Add(tIdx); FNIndices.Add(nIdx);
           end;
        end;
      end;
   end;

var command, objMtlFileName, curMtlName, curGroup : String;
    curSmothingGroup: integer;
    FG: PFaceGroup;
begin
  if assigned(FTextFile) then FTextFile.Free;
  FTextFile:=TTextFileParser.Create(aStream);
  FVertices.Clear; FNormals.Clear; FTexCoords.Clear;
  FVIndices.Clear; FTIndices.Clear; FNIndices.Clear;

  objMtlFileName:=''; curMtlName:=''; curGroup:='default';
  new(FG); FG.vOffset:=0; FG.nOffset:=0; FG.tOffset:=0;
  FG.GroupName:=''; FG.MaterialName:=''; FG.SmoothingGroup:=0;
  FFaceGroups.Add(FG); FG.MatNameHash:=StringHashKey(FG.MaterialName);

  while not FTextFile.Eof do begin
    FLine:=FTextFile.ReadLine;
    if (FLine='') or (FLine[1] in ['#', '$']) then Continue;
    command:=UpperCase(FTextFile.NextToken(FLine, ' '));
    if command='V' then begin
        hv:=ReadHomogeneousVector; FVertices.Add(hv);
    end else if command='VT' then begin
        av:=ReadAffineVector; FTexCoords.Add(av);
    end else if command='VN' then begin
        av:=ReadAffineVector; FNormals.Add(av);
    end else if command='VP' then begin
        { Parameter Space Vertex: Ignore }
    end else if command='G' then begin
        { Only the first name on the line, multiple groups not supported. }
        curGroup:=FTextFile.NextToken(FLine, ' ');
        if FG.vOffset<>FVIndices.Count then begin
          new(FG); FG.vOffset:=FVIndices.Count;
          FG.nOffset:=FNIndices.Count; FG.tOffset:=FTIndices.Count;
          FG.GroupName:=curGroup; FG.MaterialName:=''; FG.SmoothingGroup:=0;
          FG.MatNameHash:=StringHashKey(FG.MaterialName); FFaceGroups.Add(FG);
        end else FG.GroupName:=curGroup;
    end else if command='F' then begin
        ReadFace(curMtlName);
    end else if command='O' then begin
        { Object Name:  Ignore }
    end else if command='MTLLIB' then begin
        objMtlFileName:=FTextFile.NextToken(FLine, ' ');
    end else if command='USEMTL' then begin
        curMtlName:=FTextFile.NextToken(FLine, ' ');
        if FG.vOffset<>FVIndices.Count then begin
          new(FG); FG.vOffset:=FVIndices.Count;
          FG.nOffset:=FNIndices.Count; FG.tOffset:=FTIndices.Count;
          FG.GroupName:=curGroup; FG.MaterialName:=curMtlName;
          FG.MatNameHash:=StringHashKey(FG.MaterialName);
          FG.SmoothingGroup:=0; FFaceGroups.Add(FG);
        end else FG.MaterialName:=curMtlName;
    end else if command='S' then begin
        { Smooth Group}
        curSmothingGroup:=StrToIntDef(FTextFile.NextToken(FLine, ' '), 0);
        if FG.vOffset<>FVIndices.Count then begin
          new(FG); FG.vOffset:=FVIndices.Count;
          FG.nOffset:=FNIndices.Count; FG.tOffset:=FTIndices.Count;
          FG.GroupName:=curGroup; FG.MaterialName:=curMtlName;
          FG.MatNameHash:=StringHashKey(FG.MaterialName);
          FG.SmoothingGroup:=curSmothingGroup; FFaceGroups.Add(FG);
        end else FG.SmoothingGroup:=curSmothingGroup;
    end else Error('Unsupported Command '''+command+'''');
  end;

  fia.AttachVertices(FVertices, FVIndices);
  if (FTexCoords.Count>0) and (FTIndices.Count>0) then
    fia.AttachTexCoords(FTexCoords, FTIndices);
  if (FNormals.Count>0) and (FNIndices.Count>0) then
    fia.AttachNormals(FNormals, FNIndices);
  fia.AttachFaceGroups(FFaceGroups);
end;


end.

