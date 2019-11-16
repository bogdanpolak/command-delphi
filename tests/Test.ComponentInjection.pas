unit Test.ComponentInjection;

interface

uses
  DUnitX.TestFramework,
  System.Classes, System.SysUtils,
  Vcl.Pattern.Command;

{$M+}

type

  [TestFixture]
  TestSingleInjection = class(TObject)
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
    procedure ParameterStringList;
    procedure ParameterInteger;
    procedure ParameterBoolean;
    procedure ParameterDouble;
    procedure ParameterDateTime;
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
  strict private
    FStrings: TStringList;
  published
    property Strings: TStringList read FStrings write FStrings;
  end;

  TIntegerComponent = class(TComponent)
  strict private
    FNumber: integer;
  published
    property Number: integer read FNumber write FNumber;
  end;

  TSimpleComponent = class(TComponent)
  strict private
    FNumber: integer;
    FIsTrue: boolean;
    FFloatNumber: Double;
    FStartDate: TDateTime;
  published
    property Number: integer read FNumber write FNumber;
    property IsTrue: boolean read FIsTrue write FIsTrue;
    property FloatNumber: Double read FFloatNumber write FFloatNumber;
    property StartDate: TDateTime read FStartDate write FStartDate;
  end;

procedure TestSingleInjection.Setup;
begin
  FOwnerComponent := TComponent.Create(nil); // used as Owner for TCommand-s
  FStrings := TStringList.Create();
  FInteger101 := 101;
end;

procedure TestSingleInjection.TearDown;
begin
  FOwnerComponent.Free;
  FreeAndNil(FStrings);
end;

procedure TestSingleInjection.ParameterStringList;
var
  StringsComponent: TStringsComponent;
begin
  StringsComponent := TStringsComponent.Create(FOwnerComponent);
  TComponentInjector.InjectProperties(StringsComponent, [FStrings]);
  Assert.AreSame(FStrings, StringsComponent.Strings);
end;

procedure TestSingleInjection.ParameterInteger;
var
  IntegerComponent: TIntegerComponent;
begin
  IntegerComponent := TIntegerComponent.Create(FOwnerComponent);
  TComponentInjector.InjectProperties(IntegerComponent, [FInteger101]);
  Assert.AreEqual(FInteger101, IntegerComponent.Number);
end;

procedure TestSingleInjection.ParameterBoolean;
var
  SimpleComponent: TSimpleComponent;
  b: boolean;
begin
  SimpleComponent := TSimpleComponent.Create(FOwnerComponent);
  b := True;
  TComponentInjector.InjectProperties(SimpleComponent, [b]);
  Assert.AreEqual(b, SimpleComponent.IsTrue);
end;

procedure TestSingleInjection.ParameterDouble;
var
  SimpleComponent: TSimpleComponent;
  val: Double;
begin
  SimpleComponent := TSimpleComponent.Create(FOwnerComponent);
  val := Pi;
  TComponentInjector.InjectProperties(SimpleComponent, [val]);
  Assert.AreEqual(val, SimpleComponent.FloatNumber);
end;

procedure TestSingleInjection.ParameterDateTime;
var
  SimpleComponent: TSimpleComponent;
  FloatVal: Single;
  Date: TDateTime;
begin
  SimpleComponent := TSimpleComponent.Create(FOwnerComponent);
  FloatVal := 2.1;
  Date := EncodeDate(2019, 02, 01) + EncodeTime(18, 50, 0, 0);
  TComponentInjector.InjectProperties(SimpleComponent, [FloatVal, Date]);
  Assert.AreEqual(Double(FloatVal), SimpleComponent.FloatNumber);
  Assert.AreEqual(Date, SimpleComponent.StartDate);
end;

procedure TestSingleInjection.UnsupportedProperty_Exception;
type
  TMyRec = record
    a: integer;
    b: boolean;
  end;
var
  aRec1: TMyRec;
  StringsComponent: TStringsComponent;
begin
  StringsComponent := TStringsComponent.Create(FOwnerComponent);
  Assert.WillRaise(
    procedure
    begin
      TComponentInjector.InjectProperties(StringsComponent, [@aRec1]);
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

TDUnitX.RegisterTestFixture(TestSingleInjection);
TDUnitX.RegisterTestFixture(TestComponentInjection_MoreInjections);

end.
