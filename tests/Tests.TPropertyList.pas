unit Tests.TPropertyList;

interface

uses
  DUnitX.TestFramework,
  System.Classes, System.SysUtils, System.TypInfo,
  Pattern.Command;

{$TYPEINFO ON}  { Requred for old RTTI metadata form published section }

type

  [TestFixture]
  TestPropertyList = class(TObject)
  strict private
    fMetadataArray_TComponent: TPropertyArray;
    fMetadataArray_ComponentTListComponent: TPropertyArray;
    fMetadataArray_ComponentWithManyProps: TPropertyArray;
  private
  public
    [Setup]
    procedure Setup;
  published
    procedure SimpleComponent;
    procedure OnePropertyComponent;
    procedure Assert_Count;
    procedure ManyProperties_Param1;
    procedure ManyProperties_Param2;
    procedure ManyProperties_Param3;
    procedure ManyProperties_Param4;
    procedure ManyProperties_Param5;
    procedure ManyProperties_Param6;
    procedure ManyProperties_Param7;
  end;

implementation

// ----------------------------------------------------------------------
// Tested samples (components)
// ----------------------------------------------------------------------

type
  TComponentOneProps = class(TComponent)
  private
    FList: TList;
  published
    property List: TList read FList write FList;
  end;

  TComponentManyProps = class(TComponent)
  private
    FStrList: TStringList;
    FIsDone: boolean;
    FCollection: TCollection;
    FValueInt: integer;
    FAnyDate: TDateTime;
    FMemStream: TMemoryStream;
    FText: string;
  published
    property StrList: TStringList read FStrList write FStrList;
    property IsDone: boolean read FIsDone write FIsDone;
    property Collection: TCollection read FCollection write FCollection;
    property ValueInt: integer read FValueInt write FValueInt;
    property AnyDate: TDateTime read FAnyDate write FAnyDate;
    property MemStream: TMemoryStream read FMemStream write FMemStream;
    property Text: string read FText write FText;
  end;

type
  TTypeKindHelper = record helper for TTypeKind
    function ToString: string;
  end;

function TTypeKindHelper.ToString: string;
begin
  Result := GetEnumName(TypeInfo(TTypeKind), integer(Self));
end;

// ----------------------------------------------------------------------
// Setup / TearDown
// ----------------------------------------------------------------------

procedure TestPropertyList.Setup;
var
  fOwner: TComponent;
  fComponentOneProp: TComponentOneProps;
  fComponentManyProps: TComponentManyProps;
begin
  fOwner := TComponent.Create(nil);
  try
    fMetadataArray_TComponent :=
      TComponentMetadata.GetPublishedPropetries(fOwner);
    fComponentOneProp := TComponentOneProps.Create(fOwner);
    fMetadataArray_ComponentTListComponent :=
      TComponentMetadata.GetPublishedPropetries(fComponentOneProp);
    fComponentManyProps := TComponentManyProps.Create(fOwner);
    fMetadataArray_ComponentWithManyProps :=
      TComponentMetadata.GetPublishedPropetries(fComponentManyProps);
  finally
    fOwner.Free;
  end;
end;


// ----------------------------------------------------------------------
// Tests
// ----------------------------------------------------------------------

procedure TestPropertyList.SimpleComponent;
begin
  Assert.AreEqual(0, Length(fMetadataArray_TComponent));
end;

procedure TestPropertyList.OnePropertyComponent;
begin
  Assert.AreEqual(1, Length(fMetadataArray_ComponentTListComponent));
  with fMetadataArray_ComponentTListComponent[0] do
  begin
    Assert.AreEqual('List', PropertyName);
    Assert.AreEqual('TList', ClassName);
  end;
end;

procedure TestPropertyList.Assert_Count;
begin
  Assert.AreEqual(7, Length(fMetadataArray_ComponentWithManyProps));
end;

procedure TestPropertyList.ManyProperties_Param1;
begin
  Assert.AreEqual(7, Length(fMetadataArray_ComponentWithManyProps));
  with fMetadataArray_ComponentWithManyProps[0] do
  begin
    Assert.AreEqual('StrList', PropertyName);
    Assert.AreEqual('TStringList', ClassName);
    Assert.AreEqual('tkClass', Kind.ToString);
  end;
end;

procedure TestPropertyList.ManyProperties_Param2;
begin
  Assert.AreEqual(7, Length(fMetadataArray_ComponentWithManyProps));
  with fMetadataArray_ComponentWithManyProps[1] do
  begin
    Assert.AreEqual('IsDone', PropertyName);
    Assert.AreEqual('Boolean', ClassName);
    Assert.AreEqual('tkEnumeration', Kind.ToString);
  end;
end;

procedure TestPropertyList.ManyProperties_Param3;
begin
  Assert.AreEqual(7, Length(fMetadataArray_ComponentWithManyProps));
  with fMetadataArray_ComponentWithManyProps[2] do
  begin
    Assert.AreEqual('Collection', PropertyName);
    Assert.AreEqual('TCollection', ClassName);
    Assert.AreEqual('tkClass', Kind.ToString);
  end;
end;

procedure TestPropertyList.ManyProperties_Param4;
begin
  Assert.AreEqual(7, Length(fMetadataArray_ComponentWithManyProps));
  with fMetadataArray_ComponentWithManyProps[3] do
  begin
    Assert.AreEqual('ValueInt', PropertyName);
    Assert.AreEqual('Integer', ClassName);
    Assert.AreEqual('tkInteger', Kind.ToString);
  end;
end;

procedure TestPropertyList.ManyProperties_Param5;
begin
  Assert.AreEqual(7, Length(fMetadataArray_ComponentWithManyProps));
  with fMetadataArray_ComponentWithManyProps[4] do
  begin
    Assert.AreEqual('AnyDate', PropertyName);
    Assert.AreEqual('TDateTime', ClassName);
    Assert.AreEqual('tkFloat', Kind.ToString);
  end;
end;

procedure TestPropertyList.ManyProperties_Param6;
begin
  Assert.AreEqual(7, Length(fMetadataArray_ComponentWithManyProps));
  with fMetadataArray_ComponentWithManyProps[5] do
  begin
    Assert.AreEqual('MemStream', PropertyName);
    Assert.AreEqual('TMemoryStream', ClassName);
    Assert.AreEqual('tkClass', Kind.ToString);
  end;
end;

procedure TestPropertyList.ManyProperties_Param7;
begin
  Assert.AreEqual(7, Length(fMetadataArray_ComponentWithManyProps));
  with fMetadataArray_ComponentWithManyProps[6] do
  begin
    Assert.AreEqual('Text', PropertyName);
    Assert.AreEqual('String', ClassName);
    Assert.AreEqual('tkUString', Kind.ToString);
  end;
end;

end.
