unit Tests.TCommandFactory;

interface

uses
  DUnitX.TestFramework,
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  Vcl.Pattern.Command;

{$M+}

type

  [TestFixture]
  TestCommndFactory_BasicCommand = class(TObject)
  strict private
    FOwnerComponent: TComponent;
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
  TestCommndFactory_StrigListCommand = class(TObject)
  private
    FOwnerComponent: TComponent;
    FStrings: TStringList;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    procedure NoGuardAssert_WithProperInjection;
    procedure ChangeStringList_AfterExecute;
    procedure GuardException_NoInjection;
  end;

  [TestFixture]
  TestCommndFactory_AdvancedCommand = class(TObject)
  private
    FComponent: TComponent;
    FStringPrime: TStringList;
    FStringNonPrime: TStringList;
    FMemStream: TMemoryStream;
    FList: TList<Integer>;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    procedure Execute_TestPrimes;
    procedure Execute_TestNonPrimes;
    procedure Execute_TestStream;
    procedure Execute_ProcessOnlyPrimes;
  end;

implementation

// ------------------------------------------------------------------------
// TestCommndFactory_BasicCommand: TCommandA
// ------------------------------------------------------------------------

type
  TCommandA = class(TCommand)
  strict private
    FActive: boolean;
    FCount: Integer;
  protected
    procedure Guard; override;
  public
    class var IsExecuted: boolean;
    class var IsDestroyed: boolean;
    procedure Execute; override;
    destructor Destroy; override;
    property Active: boolean read FActive write FActive;
    property Count: Integer read FCount write FCount;
  end;

{$REGION 'implementation of the Basic command = TCommandA'}

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

procedure TestCommndFactory_BasicCommand.Setup;
begin
  FOwnerComponent := TComponent.Create(nil);
  TCommandA.IsExecuted := False;
  TCommandA.IsDestroyed := False;
end;

procedure TestCommndFactory_BasicCommand.TearDown;
begin
  FOwnerComponent.Free;
end;

procedure TestCommndFactory_BasicCommand.Test_AdhocExecuteCommand;
begin
  TCommandVclFactory.ExecuteCommand<TCommandA>([]);
  Assert.IsTrue(TCommandA.IsExecuted, 'TCommandA not executed');
end;

procedure TestCommndFactory_BasicCommand.Test_CreateCommandProperType;
var
  cmd: TCommandA;
begin
  cmd := TCommandVclFactory.CreateCommand<TCommandA>(FOwnerComponent, []);
  Assert.InheritsFrom(cmd.ClassType, TCommandA);
end;

procedure TestCommndFactory_BasicCommand.Test_CreateCommandAndDestroyOwner;
var
  AOwner: TComponent;
begin
  AOwner := TComponent.Create(nil);
  TCommandVclFactory.CreateCommand<TCommandA>(AOwner, []);
  AOwner.Free;
  Assert.IsTrue(TCommandA.IsDestroyed);
end;

procedure TestCommndFactory_BasicCommand.Test_ExecuteCommandAndCheckActive;
var
  CommandA: TCommandA;
begin
  CommandA := TCommandVclFactory.CreateCommand<TCommandA>(FOwnerComponent, []);
  CommandA.Execute;
  Assert.IsTrue(CommandA.Active, 'TCommanndA.Active property expected True');
end;

procedure TestCommndFactory_BasicCommand.Test_NotExecuteCommand_CounterZero;
var
  CommandA: TCommandA;
begin
  CommandA := TCommandVclFactory.CreateCommand<TCommandA>(FOwnerComponent, []);
  Assert.AreEqual(0, CommandA.Count);
end;

procedure TestCommndFactory_BasicCommand.Test_ExecuteCommand2x;
var
  CommandA: TCommandA;
begin
  CommandA := TCommandVclFactory.CreateCommand<TCommandA>(FOwnerComponent, []);
  CommandA.Execute;
  CommandA.Execute;
  Assert.AreEqual(2, CommandA.Count);
end;


// ------------------------------------------------------------------------
// TestCommndFactory_StrigListCommand
// ------------------------------------------------------------------------

type
  TCommandStringList = class(TCommand)
  strict private
    FCount: Integer;
    FLines: TStringList;
  protected
    procedure Guard; override;
  public
    procedure Execute; override;
    property Count: Integer read FCount write FCount;
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

procedure TestCommndFactory_StrigListCommand.Setup;
begin
  FOwnerComponent := TComponent.Create(nil);
  FStrings := TStringList.Create;
end;

procedure TestCommndFactory_StrigListCommand.TearDown;
begin
  FStrings.Free;
  FOwnerComponent.Free;
end;

procedure TestCommndFactory_StrigListCommand.NoGuardAssert_WithProperInjection;
begin
  TCommandVclFactory.ExecuteCommand<TCommandStringList>([FStrings]);
  // Fine if there was any exception above
  Assert.Pass;
end;

procedure TestCommndFactory_StrigListCommand.ChangeStringList_AfterExecute;
var
  CommandStrings: TCommandStringList;
begin
  CommandStrings := TCommandVclFactory.CreateCommand<TCommandStringList>
    (FOwnerComponent, [FStrings]);
  CommandStrings.Execute;
  CommandStrings.Execute;
  FStrings.Delete(0);
  Assert.AreEqual(1, FStrings.Count);
  Assert.AreEqual(1, CommandStrings.Lines.Count);
end;

procedure TestCommndFactory_StrigListCommand.GuardException_NoInjection;
begin
  Assert.WillRaiseDescendant(
    procedure
    begin
      TCommandVclFactory.ExecuteCommand<TCommandStringList>([]);
    end, EAssertionFailed);
end;

// ------------------------------------------------------------------------
// TestCommndFactory_StrigListCommand
// ------------------------------------------------------------------------

type
  TAdvancedCommand = class(TCommand)
  private
    FCount: Integer;
    FNonPrimeLines: TStrings;
    FPrimeLines: TStrings;
    FComponent: TComponent;
    FStream: TStream;
    FListInt: TList<Integer>;
    FProcessNonPrimeNumbers: boolean;
    procedure WriteIntegerToStream(aValue: Integer);
    class function isPrime (num: integer): boolean;
  protected
    procedure Guard; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Execute; override;
    property Count: Integer read FCount write FCount;
  published
    property Stream: TStream read FStream write FStream;
    property PrimeLines: TStrings read FPrimeLines write FPrimeLines;
    property Component: TComponent read FComponent write FComponent;
    property ProcessNonPrimeNumbers: boolean read FProcessNonPrimeNumbers
      write FProcessNonPrimeNumbers;
    property NonPrimeLines: TStrings read FNonPrimeLines write FNonPrimeLines;
    property ListInt: TList<Integer> read FListInt write FListInt;
  end;

{$REGION 'implementation TAdvancedCommand'}

procedure TAdvancedCommand.Guard;
begin
  System.Assert(Stream <> nil);
  System.Assert(PrimeLines <> nil);
  System.Assert(Component <> nil);
  System.Assert(NonPrimeLines <> nil);
  System.Assert(ListInt <> nil);
end;

class function TAdvancedCommand.isPrime(num: integer): boolean;
var
  M: Integer;
begin
  if num <= 1 then
    exit(false);
  for M := 2 to (num div 2) do
    if num mod M = 0 then
      exit(false);
  exit(true);
end;


procedure TAdvancedCommand.WriteIntegerToStream(aValue: Integer);
begin
  Stream.Write(aValue, SizeOf(aValue));
end;

constructor TAdvancedCommand.Create(AOwner: TComponent);
begin
  inherited;
  ProcessNonPrimeNumbers := True;
end;

procedure TAdvancedCommand.Execute;
var
  i: Integer;
begin
  inherited;
  PrimeLines.Clear;
  NonPrimeLines.Clear;
  WriteIntegerToStream(ListInt.Count);
  for i := 0 to ListInt.Count - 1 do
  begin
    WriteIntegerToStream(ListInt[i]);
    if isPrime(ListInt[i]) then
      PrimeLines.Add(Format('%d is prime',[ListInt[i]]))
    else if ProcessNonPrimeNumbers then
      NonPrimeLines.Add(ListInt[i].ToString);
  end;
  with Component do
  begin
    Name := 'A' + Component.Name;
    Tag := ListInt.Count;
  end;
  Count := ListInt.Count;
end;

{$ENDREGION}

procedure TestCommndFactory_AdvancedCommand.Setup;
begin
  FComponent := TComponent.Create(nil);
  FStringPrime := TStringList.Create;
  FStringNonPrime := TStringList.Create;
  FMemStream := TMemoryStream.Create;
  FList := TList<Integer>.Create;
end;

procedure TestCommndFactory_AdvancedCommand.TearDown;
begin
  FStringPrime.Free;
  FStringNonPrime.Free;
  FMemStream.Free;
  FList.Free;
  FComponent.Free;
end;

procedure TestCommndFactory_AdvancedCommand.Execute_TestPrimes;
begin
  with FList do
  begin
    Clear;
    AddRange([10, 13, 20, 17, 100, 101, 105]);
  end;
  TCommandVclFactory.ExecuteCommand<TAdvancedCommand>
    ([FComponent, FStringPrime, FStringNonPrime, FMemStream, FList]);
  Assert.AreEqual(3, FStringPrime.Count);
  Assert.AreEqual('13 is prime', FStringPrime[0]);
  Assert.AreEqual('17 is prime', FStringPrime[1]);
  Assert.AreEqual('101 is prime', FStringPrime[2]);
end;

procedure TestCommndFactory_AdvancedCommand.Execute_TestNonPrimes;
begin
  with FList do
  begin
    Clear;
    AddRange([10, 13, 20, 17, 100, 101, 105]);
  end;
  TCommandVclFactory.ExecuteCommand<TAdvancedCommand>
    ([FComponent, FStringPrime, FStringNonPrime, FMemStream, FList]);
  Assert.AreEqual(4, FStringNonPrime.Count);
  Assert.AreEqual('10', FStringNonPrime[0]);
  Assert.AreEqual('20', FStringNonPrime[1]);
  Assert.AreEqual('100', FStringNonPrime[2]);
  Assert.AreEqual('105', FStringNonPrime[3]);
end;

procedure TestCommndFactory_AdvancedCommand.Execute_TestStream;
begin
  with FList do
  begin
    Clear;
    AddRange([10, 13, 20, 17, 100, 101, 105]);
  end;
  TCommandVclFactory.ExecuteCommand<TAdvancedCommand>
    ([FComponent, FStringPrime, FStringNonPrime, FMemStream, FList]);
  Assert.AreEqual(32, Integer(FMemStream.Size));
end;

procedure TestCommndFactory_AdvancedCommand.Execute_ProcessOnlyPrimes;
begin
  with FList do
  begin
    Clear;
    AddRange([10, 13, 20, 17, 100, 101, 105]);
  end;
  TCommandVclFactory.ExecuteCommand<TAdvancedCommand>
    ([FComponent, FStringPrime, FStringNonPrime, FMemStream, FList,False]);
  Assert.AreEqual(3, FStringPrime.Count);
  Assert.AreEqual(0, FStringNonPrime.Count);
end;

// ------------------------------------------------------------------------
// ------------------------------------------------------------------------

initialization

end.
