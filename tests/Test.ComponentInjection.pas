unit Test.ComponentInjection;

interface

uses
  DUnitX.TestFramework,
  System.Classes, System.SysUtils,
  Vcl.Pattern.Command;

{$M+}
type

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
// class TCommandTwoStrLists
// ------------------------------------------------------------------------
{$REGION 'class TCommandTwoStrLists'}

type
  TCommandTwoStrLists = class(TCommand)
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

procedure TCommandTwoStrLists.Guard;
begin
  System.Assert(EvenLines <> nil);
  System.Assert(OddLines <> nil);
  System.Assert(Component <> nil);
end;

procedure TCommandTwoStrLists.Execute;
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
{$REGION 'class TCommandInvalidInjection'}

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
      TCommandVclFactory.ExecuteCommand<TCommandTwoStrLists>([FStrings1,
        FStrings2, FSampleComponent]);
    end);
end;

procedure TCommandFactoryMoreInjections.Test_VerifiyDependenciesAfterExecute;
begin
  TCommandVclFactory.ExecuteCommand<TCommandTwoStrLists>
    ([FStrings1, FStrings2, FSampleComponent]);
  Assert.AreEqual(1, FStrings1.Count);
  Assert.AreEqual(0, FStrings2.Count);
  Assert.AreEqual('ANothingBox', FSampleComponent.Name);
end;

procedure TCommandFactoryOneInjection.Test_ExceptionInvalidInjection;
var
  i10: integer;
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

TDUnitX.RegisterTestFixture(TCommandFactoryOneInjection);
TDUnitX.RegisterTestFixture(TCommandFactoryMoreInjections);

end.
