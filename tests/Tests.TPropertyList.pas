unit Tests.TPropertyList;

interface

uses
  DUnitX.TestFramework,
  System.Classes, System.SysUtils, System.TypInfo,
  Vcl.Pattern.Command;

{$TYPEINFO ON}  { Requred for old RTTI metadata form published section }

type

  [TestFixture]
  TComponentPropertiesSUT = class(TObject)
  strict private
    fMetadataArray: TPropertyArray;
    fOwnerComponent: TComponent;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    procedure SimpleComponent;
    procedure OnePropertyComponent;
    procedure ManyPropertiesComponent;
  end;

implementation

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

{$REGION 'TComponentPropertiesSUT: '}

procedure TComponentPropertiesSUT.Setup;
begin
  fOwnerComponent := TComponent.Create(nil); // used as Owner
end;

procedure TComponentPropertiesSUT.TearDown;
begin
  fOwnerComponent.Free;
end;

procedure TComponentPropertiesSUT.SimpleComponent;
var
  fComponentPlain: TComponent;
begin
  fComponentPlain := TComponent.Create(fOwnerComponent);
  fMetadataArray := TComponentMetadata.GetPublishedPropetries(fComponentPlain);
  Assert.AreEqual(2, Length(fMetadataArray));
  with fMetadataArray[0] do
  begin
    Assert.AreEqual('Name', PropertyName);
    Assert.AreEqual('TComponentName', ClassName);
  end;
  with fMetadataArray[1] do
  begin
    Assert.AreEqual('Tag', PropertyName);
    Assert.AreEqual('NativeInt', ClassName);
  end;
end;

procedure TComponentPropertiesSUT.OnePropertyComponent;
var
  fComponentWithOneProps: TComponentOneProps;
begin
  fComponentWithOneProps:= TComponentOneProps.Create(fOwnerComponent);
  fMetadataArray := TComponentMetadata.GetPublishedPropetries(fComponentWithOneProps);
  Assert.AreEqual(3, Length(fMetadataArray));
  with fMetadataArray[2] do
  begin
    Assert.AreEqual('List', PropertyName);
    Assert.AreEqual('TList', ClassName);
  end;
end;

procedure TComponentPropertiesSUT.ManyPropertiesComponent;
var
  fComponentManyProps: TComponentManyProps;
begin
  fComponentManyProps:= TComponentManyProps.Create(fOwnerComponent);
  fMetadataArray := TComponentMetadata.GetPublishedPropetries(fComponentManyProps);
  Assert.AreEqual(9, Length(fMetadataArray));
  with fMetadataArray[2] do
  begin
    Assert.AreEqual('StrList', PropertyName);
    Assert.AreEqual('TStringList', ClassName);
    Assert.AreEqual('tkClass', Kind.ToString);
  end;
  with fMetadataArray[3] do
  begin
    Assert.AreEqual('IsDone', PropertyName);
    Assert.AreEqual('Boolean', ClassName);
    Assert.AreEqual('tkEnumeration', Kind.ToString);
  end;
  with fMetadataArray[4] do
  begin
    Assert.AreEqual('Collection', PropertyName);
    Assert.AreEqual('TCollection', ClassName);
    Assert.AreEqual('tkClass', Kind.ToString);
  end;
  with fMetadataArray[5] do
  begin
    Assert.AreEqual('ValueInt', PropertyName);
    Assert.AreEqual('Integer', ClassName);
    Assert.AreEqual('tkInteger', Kind.ToString);
  end;
  with fMetadataArray[6] do
  begin
    Assert.AreEqual('AnyDate', PropertyName);
    Assert.AreEqual('TDateTime', ClassName);
    Assert.AreEqual('tkFloat', Kind.ToString);
  end;
  with fMetadataArray[7] do
  begin
    Assert.AreEqual('MemStream', PropertyName);
    Assert.AreEqual('TMemoryStream', ClassName);
    Assert.AreEqual('tkClass', Kind.ToString);
  end;
  with fMetadataArray[8] do
  begin
    Assert.AreEqual('Text', PropertyName);
    Assert.AreEqual('String', ClassName);
    Assert.AreEqual('tkUString', Kind.ToString);
  end;
end;

{$ENDREGION}

end.
