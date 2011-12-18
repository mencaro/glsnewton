unit uPerlinNoise;

interface

Uses Types;
type
   TSingleArray1D = array of single;
   TSingleArray2D = array of TSingleArray1D;
   TSeedRect = Record
       S11,S12,S21,S22:integer;
   end;
   Procedure CreateNoise(Size:integer;var Bitmap:TSingleArray2D;Seeds:TSeedRect);

implementation

var
   YLine  : TSingleArray1D;
   Noise  : array of array of array of byte;
   Layers : array of array of array of single;
   InitDone : boolean=false;

   CLayer: byte = 7;  {Actual Layers = CLayer+1}
   Max   : word = 255;{Actual Size = (Max+1)*(Max+1) }

   Grain : word = 127;
   nSeed1, nSeed2:Integer;
   SeedQuad:TSeedRect;

Function GetMaxLayer(X:integer):byte;
var n:byte;
begin
  n:=0; repeat x:=x div 2;inc(n);until x=0;
  result:=n-1;
end;

function freq(xy,layer : word):word;
begin
 Result:= xy shr layer;
end;

procedure InterpolateRect(Rect : TRect; v1,v2,v3,v4 : Single; Layer : byte);
var
 x,y,dx,dy,
 dxy,dxX,dyY: word;
 c:single;
begin
{Interpolation between the values v1..v4 in the size of rect}
with Rect do begin
  dx:=Right-Left; dy:=Bottom-Top;
  dxy:=dx*dy;
  for y:=0 to dy do begin
   dyY:=dy-y;
   for x:=0 to dx do begin
     dxX:=dx-x;
     c:=  (v1*dyY*dxX)/ dxy + (v2*dyY*x)/dxy+
          (v3*  y*dxX)/ dxy + (v4*  y*x)/ dxy;
     Layers[Layer,left+x,top+y]:=c;
    end;
  end;
 end;
end;

procedure InitNoise(NumberOFLayers : byte);
var
 xy,y : word;
 l   : byte;
begin
 SetLength(Noise ,NumberOFLayers+1);
 SetLength(Layers,NumberOFLayers+1);
 for l:=0 to NumberOFLayers do begin
   xy:=freq(Max,l);
   SetLength(Noise[l] ,xy+1);
   SetLength(Layers[l],Max+1);
   for y:=0 to xy  do SetLength(Noise[l,y] ,xy+1);
   for y:=0 to Max do SetLength(Layers[l,y],Max+1);
 end;
end;

Procedure GenNoise;
var x,y,l,s : word;
    TempNoise : array of array of array of byte;
begin
 RandSeed:=SeedQuad.S11;
 setlength(TempNoise,CLAyer+1);
  for l:=0 to CLAyer do begin
    s:=freq(Max,l);setlength(TempNoise[l],s+1,s+1);
    for x:=0 to s do for y:=0 to s do Noise[l,x,y]:=Random(32768);
  end;
 RandSeed:=SeedQuad.S12;
  for l:=0 to CLAyer do begin
    s:=freq(Max,l);
    for x:=0 to s do for y:=0 to s do TempNoise[l,x,y]:=Random(32768);
    for y:=0 to s do Noise[l,s,y]:=TempNoise[l,s,y];
  end;
 RandSeed:=SeedQuad.S21;
  for l:=0 to CLAyer do begin
    s:=freq(Max,l);
    for x:=0 to s do for y:=0 to s do TempNoise[l,x,y]:=Random(32768);
    for x:=0 to s do Noise[l,x,s]:=TempNoise[l,x,s];
  end;
end;

procedure Mix(var Bitmap:TSingleArray2D);
var x,y : word;
    l, z,g : byte;
    c : single;
begin
l:=0;g:=Grain;z:=not g;
if l=0 then
 for y:=0 to Max do begin
   YLine:=Bitmap[y];
   for x:=0 to Max do begin
     c:=Layers[0,x,y];
     for l:=1 to CLayer{!!!} do c:=((c*g)+(Layers[l,x,y]*z))/256;
     YLine[x]:=c-Layers[CLayer,x,y];
    end;
  end
 else
   for y:=0 to Max do begin
    YLine:=Bitmap[y];
    for x:=0 to Max do begin
      c:=Layers[l-1,x,y];
      YLine[x]:=c;
     end;
   end;
end;

procedure DrawNoise(var Bitmap:TSingleArray2D);
var
 x,y : word;
 l,cl: byte;
 sc  : single;
 v1,v2,v3,v4 : Single;
begin
 {No Interpolation and layer[0] fill}
 for l:=0 to CLayer do
   for x:=0 to Max do for y:=0 to Max do
     Layers[l,x,y]:=Noise[l,freq(x,l),freq(y,l)];
 {Interpolation}
  for l:=1 to CLayer do
    begin
     y:=0;
     cl:=freq(Max,l); sc:=Max/cl;
     repeat
       x:=0;
       repeat
         v1:=Noise[l,x,y];
         v2:=Noise[l,x+1,y];
         v3:=Noise[l,x,y+1];
         v4:=Noise[l,x+1,y+1];
         InterPolateRect(Rect(Round(x*sc),Round(y*sc),Round((x*sc)+sc),Round((y*sc)+sc)),v1,v2,v3,v4,l);
         Inc(x);
       until x=cl;
       inc(y);
     until y=cl;
   end;
 Mix(Bitmap);
end;


Procedure CreateNoise(Size:integer;var Bitmap:TSingleArray2D;Seeds:TSeedRect);
begin
  SeedQuad:=Seeds;
  Max:=Size; CLayer:=GetMaxLayer(Max+1)-1;
  Grain := ((size+1) div 2)-1;
  InitNoise(CLayer); GenNoise;
  DrawNoise(Bitmap);
end;
end.
