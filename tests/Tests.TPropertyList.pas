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
    fComponentProperties: TClassPropertyList;
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
  FOwnerComponent := TComponent.Create(nil); // used as Owner
end;

procedure TComponentPropertiesSUT.TearDown;
begin
  FOwnerComponent.Free;
end;

procedure TComponentPropertiesSUT.SimpleComponent;
var
  fComponentPlain: TComponent;
begin
  fComponentPlain := TComponent.Create(FOwnerComponent);
  try
    fComponentProperties := TClassPropertyList.Create(fComponentPlain);
    Assert.AreEqual(2, fComponentProperties.Count);
    Assert.AreEqual('Name', fComponentProperties.GetInfo(0).PropertyName);
    Assert.AreEqual('TComponentName', fComponentProperties.GetInfo(0).ClassName);
    Assert.AreEqual('Tag', fComponentProperties.GetInfo(1).PropertyName);
    Assert.AreEqual('NativeInt', fComponentProperties.GetInfo(1).ClassName);
  finally
    fComponentProperties.Free;
  end;
end;

procedure TComponentPropertiesSUT.OnePropertyComponent;
var
  fComponentWithOneProps: TComponentOneProps;
begin
  fComponentWithOneProps := TComponentOneProps.Create(FOwnerComponent);
  try
    fComponentProperties := TClassPropertyList.Create(fComponentWithOneProps);
    Assert.AreEqual(3, fComponentProperties.Count);
    Assert.AreEqual('List', fComponentProperties.GetInfo(2).PropertyName);
    Assert.AreEqual('TList', fComponentProperties.GetInfo(2).ClassName);
  finally
    fComponentProperties.Free;
  end;
end;

procedure TComponentPropertiesSUT.ManyPropertiesComponent;
var
  fComponentManyProps: TComponentManyProps;
begin
  fComponentManyProps := TComponentManyProps.Create(FOwnerComponent);
  try
    fComponentProperties := TClassPropertyList.Create(fComponentManyProps);
    Assert.AreEqual(9, fComponentProperties.Count);
    with fComponentProperties.GetInfo(2) do
    begin
      Assert.AreEqual('StrList', PropertyName);
      Assert.AreEqual('TStringList', ClassName);
      Assert.AreEqual('tkClass', Kind.ToString);
    end;
    with fComponentProperties.GetInfo(3) do
    begin
      Assert.AreEqual('IsDone', PropertyName);
      Assert.AreEqual('Boolean', ClassName);
      Assert.AreEqual('tkEnumeration', Kind.ToString);
    end;
    with fComponentProperties.GetInfo(4) do
    begin
      Assert.AreEqual('Collection', PropertyName);
      Assert.AreEqual('TCollection', ClassName);
      Assert.AreEqual('tkClass', Kind.ToString);
    end;
    with fComponentProperties.GetInfo(5) do
    begin
      Assert.AreEqual('ValueInt', PropertyName);
      Assert.AreEqual('Integer', ClassName);
      Assert.AreEqual('tkInteger', Kind.ToString);
    end;
    with fComponentProperties.GetInfo(6) do
    begin
      Assert.AreEqual('AnyDate', PropertyName);
      Assert.AreEqual('TDateTime', ClassName);
      Assert.AreEqual('tkFloat', Kind.ToString);
    end;
    with fComponentProperties.GetInfo(7) do
    begin
      Assert.AreEqual('MemStream', PropertyName);
      Assert.AreEqual('TMemoryStream', ClassName);
      Assert.AreEqual('tkClass', Kind.ToString);
    end;
    with fComponentProperties.GetInfo(8) do
    begin
      Assert.AreEqual('Text', PropertyName);
      Assert.AreEqual('String', ClassName);
      Assert.AreEqual('tkUString', Kind.ToString);
    end;
  finally
    fComponentProperties.Free;
  end;
end;

{$ENDREGION}
end.
