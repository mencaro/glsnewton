unit uPhysicObjectList;

interface
uses
  VectorGeometry, GlScene, GlObjects,
  uBaseObjectList, uBasePhysics, uPhysicRender;

type
//==============================================================================
  TPhysicGameList = class(TBaseGameList)
  protected
    fGraphWorld: TGraphicWorld;
    fPhysicWorld: TPhysicWorld;
    Function AddPhysicObject(aPhysicObject: TBasePhysicObject): Integer; virtual;
  public
    Function  GetPhysicByIndex(const aIndex: Integer): TBasePhysicObject; virtual;

    Function AddSimplePhysicSphere(const aSizes: TVector): TBasePhysicObject; virtual;
    Function AddSimplePhysicBox   (const aSizes: TVector): TBasePhysicObject; virtual;

    Procedure DoProgress(const DeltaTime: Single); override;
    Constructor Create(aPhysicWorld: TPhysicWorld; aGraphWorld: TGraphicWorld);
  end;
//==============================================================================
implementation

//==============================================================================

{TPhysicGameList}

Function TPhysicGameList.AddPhysicObject(aPhysicObject: TBasePhysicObject): Integer;
begin
  result := fListItems.Add(aPhysicObject);
end;

Function  TPhysicGameList.GetPhysicByIndex(const aIndex: Integer): TBasePhysicObject;
begin
  if (aIndex >= 0) and (aIndex < ItemsCount) then
    result := fListItems.Items[aIndex]
  else
    result := nil;
end;

Function TPhysicGameList.AddSimplePhysicSphere(const aSizes: TVector): TBasePhysicObject;
begin
  result := fPhysicWorld.CreateSimplePhysicSphere(aSizes);
  result.AttachGraphObject(fGraphWorld.CreateSimpleGraphSphere(aSizes));
  AddPhysicObject(result);
end;

Function TPhysicGameList.AddSimplePhysicBox   (const aSizes: TVector): TBasePhysicObject;
begin
  result := fPhysicWorld.CreateSimplePhysicBox(aSizes);
  result.AttachGraphObject(fGraphWorld.CreateSimpleGraphBox(aSizes));
  AddPhysicObject(result);
end;

Procedure TPhysicGameList.DoProgress(const DeltaTime: Single);
begin
  fPhysicWorld.DoProgress(DeltaTime);
  inherited;
end;

Constructor TPhysicGameList.Create(aPhysicWorld: TPhysicWorld; aGraphWorld: TGraphicWorld);
begin
  inherited Create;
  fPhysicWorld := aPhysicWorld;
  fGraphWorld := aGraphWorld;
end;

//==============================================================================
end.
