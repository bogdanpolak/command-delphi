unit Test.ComponentInjection;

interface

uses
  DUnitX.TestFramework,
  System.Classes, System.SysUtils,
  Vcl.Pattern.Command;

{$M+}

type

  [TestFixture]
  TestComponentIjection_OneInjection = class(TObject)
  private
    FInteger101: integer;
    FStrings: TStringList;
    FOwnerComponent: TComponent;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    procedure InvalidInjection_Exception;
    procedure UnsupportedProperty_Exception;
  end;

  [TestFixture]
  TestComponentInjection_MoreInjections = class(TObject)
  private
    FStrings1: TStringList;
    FStrings2: TStringList;
    FSampleComponent: TComponent;
    FOwnerComponent: TComponent;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    procedure InjectAll;
    procedure Inject_TwoStringLists;
  end;

implementation

// ------------------------------------------------------------------------
// Test_OneInjection - test injection for components with one published property
// ------------------------------------------------------------------------

type
  TStringsComponent = class(TComponent)
  private
    FStrings: TStrings;
  published
    property Strings: TStrings read FStrings write FStrings;
  end;

procedure TestComponentIjection_OneInjection.Setup;
begin
  FOwnerComponent := TComponent.Create(nil); // used as Owner for TCommand-s
  FStrings := TStringList.Create();
  FInteger101 := 101;
end;

procedure TestComponentIjection_OneInjection.TearDown;
begin
  FOwnerComponent.Free;
  FreeAndNil(FStrings);
end;

procedure TestComponentIjection_OneInjection.InvalidInjection_Exception;
var
  StringsComponent: TStringsComponent;
begin
  StringsComponent := TStringsComponent.Create(FOwnerComponent);
  Assert.WillRaise(
    procedure
    begin
      TComponentInjector.InjectProperties(StringsComponent, [FInteger101]);
    end);
end;

type
  TIntegerComponent = class(TComponent)
  private
    FNumber: integer;
  published
    property Number: integer read FNumber write FNumber;
  end;

procedure TestComponentIjection_OneInjection.UnsupportedProperty_Exception;
var
  IntegerComponent: TIntegerComponent;
begin
  IntegerComponent := TIntegerComponent.Create(FOwnerComponent);
  Assert.WillRaise(
    procedure
    begin
      TComponentInjector.InjectProperties(IntegerComponent, [FInteger101]);
    end);
end;


// ------------------------------------------------------------------------
// Test_MoreInjections - tests component with many injected properties
// * 2x TStringList, 1x TComponent
// ------------------------------------------------------------------------

type
  TManyPropComponent = class(TComponent)
  strict private
    FCount: integer;
    FEvenLines: TStringList;
    FOddLines: TStringList;
    FComponent: TComponent;
    FStream: TStream;
  public
    property Count: integer read FCount write FCount;
    property Stream: TStream read FStream write FStream;
  published
    property OddLines: TStringList read FOddLines write FOddLines;
    property Component: TComponent read FComponent write FComponent;
    property EvenLines: TStringList read FEvenLines write FEvenLines;
  end;

procedure TestComponentInjection_MoreInjections.Setup;
begin
  FStrings1 := TStringList.Create;
  FStrings2 := TStringList.Create;
  FOwnerComponent := TComponent.Create(nil);
end;

procedure TestComponentInjection_MoreInjections.TearDown;
begin
  FreeAndNil(FStrings1);
  FreeAndNil(FStrings2);
  FreeAndNil(FOwnerComponent);
end;

procedure TestComponentInjection_MoreInjections.InjectAll;
var
  ManyPropComponent: TManyPropComponent;
begin
  // Arrange:
  ManyPropComponent := TManyPropComponent.Create(FOwnerComponent);
  // Act:
  TComponentInjector.InjectProperties(ManyPropComponent,
    [FStrings1, FStrings2, FOwnerComponent]);
  // Assert
  Assert.AreSame(FStrings1, ManyPropComponent.OddLines);
  Assert.AreSame(FStrings2, ManyPropComponent.EvenLines);
  Assert.AreSame(FOwnerComponent, ManyPropComponent.Component);
end;

procedure TestComponentInjection_MoreInjections.Inject_TwoStringLists;
var
  ManyPropComponent: TManyPropComponent;
begin
  // --
  ManyPropComponent := TManyPropComponent.Create(FOwnerComponent);
  // --
  TComponentInjector.InjectProperties(ManyPropComponent,
    [FStrings1, FStrings2]);
  // --
  Assert.AreSame(FStrings1, ManyPropComponent.OddLines);
  Assert.AreSame(FStrings2, ManyPropComponent.EvenLines);
end;

// ------------------------------------------------------------------------
// ------------------------------------------------------------------------

initialization

TDUnitX.RegisterTestFixture(TestComponentIjection_OneInjection);
TDUnitX.RegisterTestFixture(TestComponentInjection_MoreInjections);

end.
