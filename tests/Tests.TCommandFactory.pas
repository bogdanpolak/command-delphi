unit Tests.TCommandFactory;

interface

uses
  DUnitX.TestFramework,
  System.Classes, System.SysUtils,
  Vcl.Pattern.Command;

{$TYPEINFO ON}  { Requred for old RTTI metadata form published section }

type

  [TestFixture]
  TCommndFactoryNoInjection = class(TObject)
  strict private
    FOwnerComponent: TComponent;
    FCommandA: TCommand;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    procedure Test_AdhocExecuteCommand;
    procedure Test_CreateCommandProperType;
    procedure Test_CreateCommandAndDestroyOwner;
    procedure Test_ExecuteCommandAndCheckActive;
    procedure Test_NotExecuteCommand_CounterZero;
    procedure Test_ExecuteCommand2x;
  end;

  [TestFixture]
  TCommandFactoryOneInjection = class(TObject)
  strict private
    FStrings: TStringList;
    FOwnerComponent: TComponent;
  private
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    procedure Test_Injection_AssertOneInjection;
    procedure Test_Injection_ExecuteAndCheckLinesCount;
    procedure Test_ExceptionNoRequiredInjection;
    procedure Test_ExceptionInvalidInjection;
  end;

  [TestFixture]
  TCommandFactoryMoreInjections = class(TObject)
  strict private
    FStrings1: TStringList;
    FStrings2: TStringList;
    FSampleComponent: TComponent;
    FOwnerComponent: TComponent;
  private
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    procedure Test_InjectDependenciesNoExceptions;
    procedure Test_VerifiyDependenciesAfterExecute;
  end;

implementation

// ------------------------------------------------------------------------
// class TCommandA
// ------------------------------------------------------------------------
{$REGION 'class TCommandA'}

type
  TCommandA = class(TCommand)
  strict private
    FActive: boolean;
    FCount: integer;
  protected
    procedure Guard; override;
  public
    class var IsExecuted: boolean;
    class var IsDestroyed: boolean;
    procedure Execute; override;
    destructor Destroy; override;
    property Active: boolean read FActive write FActive;
    property Count: integer read FCount write FCount;
  end;

procedure TCommandA.Guard;
begin
end;

destructor TCommandA.Destroy;
begin
  IsDestroyed := True;
  inherited;
end;

procedure TCommandA.Execute;
begin
  Active := True;
  IsExecuted := True;
  Count := Count + 1;
end;

{$ENDREGION}
// ------------------------------------------------------------------------
// class TCommandStringList
// ------------------------------------------------------------------------
{$REGION 'class TCommandStringList'}

type
  TCommandStringList = class(TCommand)
  strict private
    FCount: integer;
    FLines: TStringList;
  protected
    procedure Guard; override;
  public
    procedure Execute; override;
    property Count: integer read FCount write FCount;
  published
    property Lines: TStringList read FLines write FLines;
  end;

procedure TCommandStringList.Guard;
begin
  System.Assert(Lines <> nil);
end;

procedure TCommandStringList.Execute;
begin
  inherited;
  Count := Count + 1;
  Lines.Add(Format('%.3d', [Count]));
end;

{$ENDREGION}
// ------------------------------------------------------------------------
// class TCommandMore
// ------------------------------------------------------------------------
{$REGION 'class TCommandMore'}

type
  TCommandMore = class(TCommand)
  strict private
    FCount: integer;
    FEvenLines: TStringList;
    FOddLines: TStringList;
    FComponent: TComponent;
  protected
    procedure Guard; override;
  public
    procedure Execute; override;
    property Count: integer read FCount write FCount;
  published
    property OddLines: TStringList read FOddLines write FOddLines;
    property Component: TComponent read FComponent write FComponent;
    property EvenLines: TStringList read FEvenLines write FEvenLines;
  end;

procedure TCommandMore.Guard;
begin
  System.Assert(EvenLines <> nil);
  System.Assert(OddLines <> nil);
  System.Assert(Component <> nil);
end;

procedure TCommandMore.Execute;
begin
  inherited;
  Count := Count + 1;
  if Odd(Count) then
  begin
    OddLines.Add(Format('%.3d - %s', [Count, Component.Name]));
    Component.Name := 'A' + Component.Name;
  end
  else
    EvenLines.Add(Format('%.3d', [Count]));
end;

{$ENDREGION}
// ------------------------------------------------------------------------
// class TCommandInvalidInjection
// ------------------------------------------------------------------------
{$REGION 'class TCommandMore'}

type
  TCommandInvalidInjection = class(TCommand)
  strict private
    FCount: integer;
  protected
    procedure Guard; override;
  public
    procedure Execute; override;
  published
    property Count: integer read FCount write FCount;
  end;

procedure TCommandInvalidInjection.Guard;
begin
  System.Assert(Count > 0);
end;

procedure TCommandInvalidInjection.Execute;
begin
  inherited;
  Count := Count + 1;
end;

{$ENDREGION}
// ------------------------------------------------------------------------
// TFactoryNoInjectionTest: TCommandA
// ------------------------------------------------------------------------
{$REGION 'TCommandFactoryTests: TCommandA - no injection'}

procedure TCommndFactoryNoInjection.Setup;
begin
  FOwnerComponent := TComponent.Create(nil); // used as Owner for TCommand-s
  FCommandA := TCommandVclFactory.CreateCommand<TCommandA>(FOwnerComponent, []);
  TCommandA.IsExecuted := False;
end;

procedure TCommndFactoryNoInjection.TearDown;
begin
  FOwnerComponent.Free;
  // FCommandA destroyed by Owner
  TCommandA.IsExecuted := False;
end;

procedure TCommndFactoryNoInjection.Test_AdhocExecuteCommand;
begin
  TCommandVclFactory.ExecuteCommand<TCommandA>([]);
  Assert.IsTrue(TCommandA.IsExecuted, 'TCommandA not executed');
end;

procedure TCommndFactoryNoInjection.Test_CreateCommandProperType;
var
  cmd: TCommandA;
begin
  cmd := TCommandVclFactory.CreateCommand<TCommandA>(FOwnerComponent, []);
  Assert.InheritsFrom(cmd.ClassType, TCommandA);
end;

procedure TCommndFactoryNoInjection.Test_CreateCommandAndDestroyOwner;
var
  AOwner: TComponent;
begin
  TCommandA.IsDestroyed := False;
  AOwner := TComponent.Create(nil);
  TCommandVclFactory.CreateCommand<TCommandA>(AOwner, []);
  AOwner.Free;
  Assert.IsTrue(TCommandA.IsDestroyed);
end;

procedure TCommndFactoryNoInjection.Test_ExecuteCommandAndCheckActive;
begin
  FCommandA.Execute;
  Assert.IsTrue((FCommandA as TCommandA).Active,
    'TCommanndA.Active property expected True');
end;

procedure TCommndFactoryNoInjection.Test_NotExecuteCommand_CounterZero;
begin
  Assert.AreEqual(0, (FCommandA as TCommandA).Count);
end;

procedure TCommndFactoryNoInjection.Test_ExecuteCommand2x;
begin
  FCommandA.Execute;
  FCommandA.Execute;
  Assert.AreEqual(2, (FCommandA as TCommandA).Count);
end;

{$ENDREGION}
// ------------------------------------------------------------------------
// TFactoryWithInjectionTest: TStringList one injection
// ------------------------------------------------------------------------
{$REGION 'TCommandFactoryTests: TCommandStringList - check injection'}

procedure TCommandFactoryOneInjection.Setup;
begin
  FOwnerComponent := TComponent.Create(nil); // used as Owner for TCommand-s
  FStrings := TStringList.Create();
end;

procedure TCommandFactoryOneInjection.TearDown;
begin
  FOwnerComponent.Free;
  FreeAndNil(FStrings);
end;

procedure TCommandFactoryOneInjection.Test_Injection_AssertOneInjection;
begin
  TCommandVclFactory.ExecuteCommand<TCommandStringList>([FStrings]);
  Assert.Pass; // Fine is there was any exception above - correct injection
end;

procedure TCommandFactoryOneInjection.Test_Injection_ExecuteAndCheckLinesCount;
var
  cmd: TCommandStringList;
begin
  cmd := TCommandVclFactory.CreateCommand<TCommandStringList>(FOwnerComponent,
    [FStrings]);
  cmd.Execute;
  cmd.Execute;
  cmd.Lines.Delete(0);
  Assert.AreEqual(1, cmd.Lines.Count);
end;

procedure TCommandFactoryOneInjection.Test_ExceptionNoRequiredInjection;
begin
  Assert.WillRaiseDescendant(
    procedure
    begin
      TCommandVclFactory.ExecuteCommand<TCommandStringList>([]);
    end, EAssertionFailed);
end;

{$ENDREGION}
// ------------------------------------------------------------------------
// CommandFactory tests factory methods with more injection
// * 2x TStringList, 1x TComponent
// ------------------------------------------------------------------------
{$REGION 'TFactoryWithMoreInjectionTest: check more injection'}

procedure TCommandFactoryMoreInjections.Setup;
begin
  FStrings1 := TStringList.Create;
  FStrings2 := TStringList.Create;
  FSampleComponent := TComponent.Create(nil);
  FSampleComponent.Name := 'NothingBox';
  // have to see Mark Gungor in action: https://www.youtube.com/watch?v=SWiBRL-bxiA
  FOwnerComponent := TComponent.Create(nil);
end;

procedure TCommandFactoryMoreInjections.TearDown;
begin
  FreeAndNil(FStrings1);
  FreeAndNil(FStrings2);
  FreeAndNil(FSampleComponent);
  FreeAndNil(FOwnerComponent);
end;

procedure TCommandFactoryMoreInjections.Test_InjectDependenciesNoExceptions;
begin
  Assert.WillNotRaise(
    procedure
    begin
      TCommandVclFactory.ExecuteCommand<TCommandMore>([FStrings1, FStrings2,
        FSampleComponent]);
    end);
end;

procedure TCommandFactoryMoreInjections.Test_VerifiyDependenciesAfterExecute;
begin
  TCommandVclFactory.ExecuteCommand<TCommandMore>
    ([FStrings1, FStrings2, FSampleComponent]);
  Assert.AreEqual(1, FStrings1.Count);
  Assert.AreEqual(0, FStrings2.Count);
  Assert.AreEqual('ANothingBox', FSampleComponent.Name);
end;

procedure TCommandFactoryOneInjection.Test_ExceptionInvalidInjection;
var
  i10: Integer;
begin
  i10 := 10;
  Assert.WillRaise(
    procedure
    begin
      TCommandVclFactory.ExecuteCommand<TCommandInvalidInjection>([i10]);
    end);
end;

{$ENDREGION}
// ------------------------------------------------------------------------
// ------------------------------------------------------------------------


initialization

TDUnitX.RegisterTestFixture(TCommndFactoryNoInjection);
TDUnitX.RegisterTestFixture(TCommandFactoryOneInjection);
TDUnitX.RegisterTestFixture(TCommandFactoryMoreInjections);

end.
