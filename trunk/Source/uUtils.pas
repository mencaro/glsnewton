unit uUtils;

interface
uses
  uVbo, GeometryBB;

type
  TExtents = uVbo.TExtents;

Function FromExtentsToAABB(aExtents: TExtents): TAABB;
Function FromAABBToExtents(aAABB: TAABB): TExtents;

implementation

Function FromExtentsToAABB(aExtents: TExtents): TAABB;
begin
  result.min := aExtents.emin;
  result.max := aExtents.emax;
end;

Function FromAABBToExtents(aAABB: TAABB): TExtents;
begin
  result.emin := aAABB.min;
  result.emax := aAABB.max;
end;

end.
