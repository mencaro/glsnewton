unit uPhysicObjectList;

interface
uses
  VectorGeometry,
  uBaseObjectList, uBasePhysics;

type
//==============================================================================
  TPhysicGameList = class(TBaseGameList)
  protected
    fPhysicWorld: TPhysicWorld;
    Function AddPhysicObject(aPhysicObject: TBasePhysicObject): Integer; virtual;
  public
    Function  GetPhysicByIndex(const aIndex: Integer): TBasePhysicObject; virtual;

    Function AddSimplePhysicSphere(const aSizes: TVector): TBasePhysicObject; virtual;
    Function AddSimplePhysicBox   (const aSizes: TVector): TBasePhysicObject; virtual;

    Procedure DoProgress(const DeltaTime: Single); override;
    Constructor Create(aPhysicWorld: TPhysicWorld);
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
  AddPhysicObject(result);
end;

Function TPhysicGameList.AddSimplePhysicBox   (const aSizes: TVector): TBasePhysicObject;
begin
  result := fPhysicWorld.CreateSimplePhysicBox(aSizes);
  AddPhysicObject(result);
end;

Procedure TPhysicGameList.DoProgress(const DeltaTime: Single);
begin
  fPhysicWorld.DoProgress(DeltaTime);
  inherited;
end;

Constructor TPhysicGameList.Create(aPhysicWorld: TPhysicWorld);
begin
  inherited Create;
  fPhysicWorld := aPhysicWorld;
end;

//==============================================================================
end.
