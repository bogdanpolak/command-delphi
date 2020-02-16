unit Tests.TCommand;

interface

uses
  DUnitX.TestFramework,
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  Pattern.Command;

{$M+}

type

  [TestFixture]
  TestCommnd_Basic = class(TObject)
  strict private
    FOwnerComponent: TComponent;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    procedure Test_ExecuteCommandAndCheckActive;
    procedure Test_NotExecuteCommand_CounterZero;
    procedure Test_ExecuteCommand2x;
  end;

  [TestFixture]
  TestCommnd_StrigsCommand = class(TObject)
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
  TestCommnd_Advanced = class(TObject)
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
// Test Basic Command - TCommandA
// ------------------------------------------------------------------------

type
  TCommandA = class(TCommand)
  strict private
    FActive: boolean;
    FCount: Integer;
  strict protected
    procedure DoGuard; override;
    procedure DoExecute; override;
  public
    property Active: boolean read FActive write FActive;
    property Count: Integer read FCount write FCount;
  end;


procedure TCommandA.DoGuard;
begin
  // System.Assert( check is injected required property );
end;

procedure TCommandA.DoExecute;
begin
  Active := True;
  Count := Count + 1;
end;

// ------------------------------------------------------------------------

procedure TestCommnd_Basic.Setup;
begin
  FOwnerComponent := TComponent.Create(nil);
end;

procedure TestCommnd_Basic.TearDown;
begin
  FOwnerComponent.Free;
end;

procedure TestCommnd_Basic.Test_ExecuteCommandAndCheckActive;
var
  CommandA: TCommandA;
begin
  CommandA := TCommandA.Create(FOwnerComponent);
  CommandA.Execute;
  Assert.IsTrue(CommandA.Active, 'TCommanndA.Active property expected True');
  Assert.AreEqual(1, CommandA.Count);
end;

procedure TestCommnd_Basic.Test_NotExecuteCommand_CounterZero;
var
  CommandA: TCommandA;
begin
  CommandA := TCommandA.Create(FOwnerComponent);
  Assert.AreEqual(0, CommandA.Count);
end;

procedure TestCommnd_Basic.Test_ExecuteCommand2x;
var
  CommandA: TCommandA;
begin
  CommandA := TCommandA.Create(FOwnerComponent);
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
  strict protected
    procedure DoGuard; override;
    procedure DoExecute; override;
  public
    property Count: Integer read FCount write FCount;
  published
    property Lines: TStringList read FLines write FLines;
  end;

{$REGION 'implementation TCommandStringList'}

procedure TCommandStringList.DoGuard;
begin
  System.Assert(Lines <> nil);
end;

procedure TCommandStringList.DoExecute;
begin
  inherited;
  Count := Count + 1;
  Lines.Add(Format('%.3d', [Count]));
end;

{$ENDREGION}

procedure TestCommnd_StrigsCommand.Setup;
begin
  FOwnerComponent := TComponent.Create(nil);
  FStrings := TStringList.Create;
end;

procedure TestCommnd_StrigsCommand.TearDown;
begin
  FStrings.Free;
  FOwnerComponent.Free;
end;

procedure TestCommnd_StrigsCommand.NoGuardAssert_WithProperInjection;
begin
  TCommand.AdhocExecute<TCommandStringList>([FStrings]);
  // Check if  there was any exception above
  Assert.Pass;
end;

procedure TestCommnd_StrigsCommand.ChangeStringList_AfterExecute;
var
  CommandStrings: TCommandStringList;
begin
  CommandStrings := TCommandStringList.Create(FOwnerComponent);
  CommandStrings.WithInjections([FStrings]);
  CommandStrings.Execute;
  CommandStrings.Execute;
  FStrings.Delete(0);
  Assert.AreEqual(1, FStrings.Count);
  Assert.AreEqual(1, CommandStrings.Lines.Count);
end;

procedure TestCommnd_StrigsCommand.GuardException_NoInjection;
begin
  Assert.WillRaiseDescendant(
    procedure
    begin
      TCommand.AdhocExecute<TCommandStringList>([]);
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
    class function isPrime(num: Integer): boolean;
  strict protected
    procedure DoGuard; override;
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
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

procedure TAdvancedCommand.DoGuard;
begin
  System.Assert(Stream <> nil);
  System.Assert(PrimeLines <> nil);
  System.Assert(Component <> nil);
  System.Assert(NonPrimeLines <> nil);
  System.Assert(ListInt <> nil);
end;

class function TAdvancedCommand.isPrime(num: Integer): boolean;
var
  M: Integer;
begin
  if num <= 1 then
    exit(false);
  for M := 2 to (num div 2) do
    if num mod M = 0 then
      exit(false);
  exit(True);
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

procedure TAdvancedCommand.DoExecute;
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
      PrimeLines.Add(Format('%d is prime', [ListInt[i]]))
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

procedure TestCommnd_Advanced.Setup;
begin
  FComponent := TComponent.Create(nil);
  FStringPrime := TStringList.Create;
  FStringNonPrime := TStringList.Create;
  FMemStream := TMemoryStream.Create;
  FList := TList<Integer>.Create;
end;

procedure TestCommnd_Advanced.TearDown;
begin
  FStringPrime.Free;
  FStringNonPrime.Free;
  FMemStream.Free;
  FList.Free;
  FComponent.Free;
end;

procedure TestCommnd_Advanced.Execute_TestPrimes;
begin
  with FList do
  begin
    Clear;
    AddRange([10, 13, 20, 17, 100, 101, 105]);
  end;
  TCommand.AdhocExecute<TAdvancedCommand>([FComponent, FStringPrime,
    FStringNonPrime, FMemStream, FList]);
  Assert.AreEqual(3, FStringPrime.Count);
  Assert.AreEqual('13 is prime', FStringPrime[0]);
  Assert.AreEqual('17 is prime', FStringPrime[1]);
  Assert.AreEqual('101 is prime', FStringPrime[2]);
end;

procedure TestCommnd_Advanced.Execute_TestNonPrimes;
begin
  with FList do
  begin
    Clear;
    AddRange([10, 13, 20, 17, 100, 101, 105]);
  end;
  TCommand.AdhocExecute<TAdvancedCommand>([FComponent, FStringPrime,
    FStringNonPrime, FMemStream, FList]);
  Assert.AreEqual(4, FStringNonPrime.Count);
  Assert.AreEqual('10', FStringNonPrime[0]);
  Assert.AreEqual('20', FStringNonPrime[1]);
  Assert.AreEqual('100', FStringNonPrime[2]);
  Assert.AreEqual('105', FStringNonPrime[3]);
end;

procedure TestCommnd_Advanced.Execute_TestStream;
begin
  with FList do
  begin
    Clear;
    AddRange([10, 13, 20, 17, 100, 101, 105]);
  end;
  TCommand.AdhocExecute<TAdvancedCommand>([FComponent, FStringPrime,
    FStringNonPrime, FMemStream, FList]);
  Assert.AreEqual(32, Integer(FMemStream.Size));
end;

procedure TestCommnd_Advanced.Execute_ProcessOnlyPrimes;
begin
  with FList do
  begin
    Clear;
    AddRange([10, 13, 20, 17, 100, 101, 105]);
  end;
  TCommand.AdhocExecute<TAdvancedCommand>([FComponent, FStringPrime,
    FStringNonPrime, FMemStream, FList, false]);
  Assert.AreEqual(3, FStringPrime.Count);
  Assert.AreEqual(0, FStringNonPrime.Count);
end;

// ------------------------------------------------------------------------
// ------------------------------------------------------------------------

initialization

end.
