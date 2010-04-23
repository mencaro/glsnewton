unit uBaseObjectList;

interface
uses
  Classes,
  uBasePhysics;
type  
//==============================================================================
  TBaseGameList = class(TBaseGameObject)
  protected
    fListItems: TList;
//    fMatLib: TGLMaterialLibrary; // не знаю, нужно ли
  protected
    Function AddObject: Integer; virtual;
    Procedure FreeAll; virtual;
    Function GetItemsCount: Integer; virtual;
  public
    property ItemsCount: Integer read GetItemsCount;
//    property MatLib: TGLMaterialLibrary read fMatLib;

    Function  GetObjectbyIndex(const aIndex: Integer): TBaseGameObject; virtual;
    Function  GetIndexByObject(const aObj: TBaseGameObject): Integer;   virtual;
    Function  DeleteByObject(aObj:   TBaseGameObject; const aShouldToFree: boolean = true): boolean;
    Function  DeleteByIndex (aIndex: integer;         const aShouldToFree: boolean = true): boolean;

    Procedure DoProgress(const DeltaTime: Single); override;

    Constructor Create;
    Destructor  Destroy; override;
  end;
//==============================================================================
implementation
uses
  SysUtils;
  
//==============================================================================

{TBaseGameList}

Function TBaseGameList.AddObject: Integer;
var
  BaseObject: TBaseGameObject;
begin
  BaseObject := TBaseGameObject.Create;
  result := fListItems.Add(BaseObject);
end;

Procedure TBaseGameList.FreeAll;
begin
  while ItemsCount > 0 do
    DeleteByIndex(0, true)
end;

Function TBaseGameList.GetItemsCount: Integer;
begin
  result := fListItems.Count;
end;

Function  TBaseGameList.GetObjectbyIndex(const aIndex: Integer): TBaseGameObject;
begin
  if (aIndex >= 0) and (aIndex < ItemsCount) then
    result := fListItems.Items[aIndex]
  else
    result := nil;
end;

Function  TBaseGameList.GetIndexByObject(const aObj: TBaseGameObject): Integer;
begin
  result := fListItems.IndexOf(aObj);
end;

Function  TBaseGameList.DeleteByObject(aObj:   TBaseGameObject; const aShouldToFree: boolean = true): boolean;
var
  Index: Integer;
begin
  Index := GetIndexByObject(aObj);
  result := Index >= 0;
  if result then
    DeleteByIndex(Index, aShouldToFree);
end;

Function  TBaseGameList.DeleteByIndex (aIndex: integer;         const aShouldToFree: boolean = true): boolean;
var
  aObj: TBaseGameObject;
begin
  aObj := GetObjectbyIndex(aIndex);
  result := aObj <> nil;
  if result and aShouldToFree then
    FreeAndNil(aObj);
  fListItems.Delete(aIndex);
end;

Procedure TBaseGameList.DoProgress(const DeltaTime: Single);
var
  i: Integer;
  Obj: TBaseGameObject;
begin
  for i := 0 to ItemsCount - 1 do
  begin
    Obj := GetObjectbyIndex(i);
    if Obj <> nil then
      Obj.DoProgress(DeltaTime);
  end;
end;

Constructor TBaseGameList.Create;
begin
  inherited Create;
  fListItems := TList.Create;
end;

Destructor  TBaseGameList.Destroy;
begin
  FreeAll;
  inherited;
end;

//==============================================================================
end.
