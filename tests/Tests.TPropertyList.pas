unit Tests.TPropertyList;

interface

uses
  DUnitX.TestFramework,
  System.Classes,
  System.SysUtils,
  System.TypInfo,
  Pattern.Command;

{$M+}

type

  [TestFixture]
  TestPropertyList = class(TObject)
  private
    fComponent: TComponent;
    procedure AssertMetadataItem(const expectedPropertyName
  : string; const expectedClassName: string; expectedKind: TTypeKind;
  const metadataItem: TPropertyInfo);
    function AssertMetadataSize(const expectedSize: integer;
      const metadataArray: TPropertyArray): boolean;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    procedure ComponentWithoutProperties;
    procedure OneProperty;
    procedure ManyProperties_StrListTStringList;
    procedure ManyProperties_IsDoneBoolean;
    procedure ManyProperties_TCollection;
    procedure ManyProperties_ValueIntInteger;
    procedure ManyProperties_AnyDateTDateTime;
    procedure ManyProperties_MemStreamTMemoryStream;
    procedure ManyProperties_TextString;
  end;

implementation

// ----------------------------------------------------------------------
// Tested samples (components)
// ----------------------------------------------------------------------

type
  TComponentWithTList = class(TComponent)
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
    property Text: String read FText write FText;
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
begin
  fComponent := TComponent.Create(nil);
end;

procedure TestPropertyList.TearDown;
begin
  fComponent.Free;
end;

function TestPropertyList.AssertMetadataSize(const expectedSize: integer;
  const metadataArray: TPropertyArray): boolean;
begin
  Result := (expectedSize <= Length(metadataArray));
  if not Result then
    Assert.Fail
      (Format('Expected %d items but got %d items (metadata TPropertyArray has not enough items)',
      [expectedSize,Length(metadataArray)]));
end;

procedure TestPropertyList.AssertMetadataItem(const expectedPropertyName
  : string; const expectedClassName: string; expectedKind: TTypeKind;
  const metadataItem: TPropertyInfo);
begin
    if (expectedPropertyName <> metadataItem.PropertyName) or
      (expectedClassName <> metadataItem.ClassName) then
      Assert.Fail(Format('Expected item %s:%s but got %s:%s',
        [expectedPropertyName, expectedClassName, metadataItem.PropertyName,
        metadataItem.ClassName]))
    else if (expectedKind<>metadataItem.Kind) then
      Assert.Fail(Format('Expected item kind %s but got %s',
        [expectedKind.ToString, metadataItem.Kind.ToString]))
    else
      Assert.Pass;

end;

// ----------------------------------------------------------------------
// Tests
// ----------------------------------------------------------------------

procedure TestPropertyList.ComponentWithoutProperties;
var
  metadata: TPropertyArray;
begin
  metadata := TComponentMetadata.GetPublishedPropetries(fComponent);
  if AssertMetadataSize(0,metadata) then
    Assert.Pass;
end;

procedure TestPropertyList.OneProperty;
var
  metadata: TPropertyArray;
begin
  metadata := TComponentMetadata.GetPublishedPropetries
    (TComponentWithTList.Create(fComponent));
  AssertMetadataSize(1,metadata);
  AssertMetadataItem('List','TList',tkClass, metadata[0]);
end;

procedure TestPropertyList.ManyProperties_StrListTStringList;
var
  metadata: TPropertyArray;
begin
  metadata := TComponentMetadata.GetPublishedPropetries
    (TComponentManyProps.Create(fComponent));
  AssertMetadataSize(7,metadata);
  AssertMetadataItem('StrList','TStringList',tkClass, metadata[0]);
end;

procedure TestPropertyList.ManyProperties_IsDoneBoolean;
var
  metadata: TPropertyArray;
begin
  metadata := TComponentMetadata.GetPublishedPropetries
    (TComponentManyProps.Create(fComponent));
  AssertMetadataSize(7,metadata);
  AssertMetadataItem('IsDone', 'Boolean', tkEnumeration, metadata[1]);
end;

procedure TestPropertyList.ManyProperties_TCollection;
var
  metadata: TPropertyArray;
begin
  metadata := TComponentMetadata.GetPublishedPropetries
    (TComponentManyProps.Create(fComponent));
  AssertMetadataSize(7,metadata);
  AssertMetadataItem('Collection','TCollection',tkClass, metadata[2]);
end;

procedure TestPropertyList.ManyProperties_ValueIntInteger;
var
  metadata: TPropertyArray;
begin
  metadata := TComponentMetadata.GetPublishedPropetries
    (TComponentManyProps.Create(fComponent));
  AssertMetadataSize(7,metadata);
  AssertMetadataItem('ValueInt', 'Integer', tkInteger, metadata[3]);
end;

procedure TestPropertyList.ManyProperties_AnyDateTDateTime;
var
  metadata: TPropertyArray;
begin
  metadata := TComponentMetadata.GetPublishedPropetries
    (TComponentManyProps.Create(fComponent));
  AssertMetadataSize(7,metadata);
  AssertMetadataItem('AnyDate', 'TDateTime', tkFloat, metadata[4]);
end;

procedure TestPropertyList.ManyProperties_MemStreamTMemoryStream;
var
  metadata: TPropertyArray;
begin
  metadata := TComponentMetadata.GetPublishedPropetries
    (TComponentManyProps.Create(fComponent));
  AssertMetadataSize(7,metadata);
  AssertMetadataItem('MemStream', 'TMemoryStream', tkClass, metadata[5]);
end;

procedure TestPropertyList.ManyProperties_TextString;
var
  metadata: TPropertyArray;
begin
  metadata := TComponentMetadata.GetPublishedPropetries
    (TComponentManyProps.Create(fComponent));
  AssertMetadataSize(7,metadata);
  AssertMetadataItem('Text', 'string', tkUString, metadata[6]);
end;

end.
