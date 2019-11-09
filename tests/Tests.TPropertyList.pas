unit Tests.TPropertyList;

interface

uses
  DUnitX.TestFramework,
  System.Classes, System.SysUtils,
  Vcl.Pattern.Command;

{$TYPEINFO ON}  { Requred for old RTTI metadata form published section }

type
  TComponentProps = class(TComponent)
  private
    FList: TList;
  published
    property List: TList read FList write FList;
  end;

  [TestFixture]
  TComponentPropertiesSUT = class(TObject)
  strict private
    fComponentProperties: TClassPropertyList;
    fOwnerComponent: TComponent;
    fComponentPlain: TComponent;
    fComponentWithProps: TComponentProps;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    procedure Test_01;
    procedure Test_02;
  end;

implementation

{$REGION 'TComponentPropertiesSUT: '}

procedure TComponentPropertiesSUT.Setup;
begin
  FOwnerComponent := TComponent.Create(nil); // used as Owner for TCommand-s

  fComponentPlain := TComponent.Create(FOwnerComponent);
  fComponentWithProps := TComponentProps.Create(FOwnerComponent);
end;

procedure TComponentPropertiesSUT.TearDown;
begin
  FOwnerComponent.Free;
end;

procedure TComponentPropertiesSUT.Test_01;
begin
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

procedure TComponentPropertiesSUT.Test_02;
begin
  Assert.Pass;
end;

{$ENDREGION}
end.
