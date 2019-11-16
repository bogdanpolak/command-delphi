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
    FStringO: TStringList;
    FStringE: TStringList;
    FMemStream: TMemoryStream;
    FList: TList<Integer>;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    procedure Execute;
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
    FEvenLines: TStringList;
    FOddLines: TStringList;
    FComponent: TComponent;
    FStream: TMemoryStream;
    FListInt: TList<Integer>;
    procedure WriteIntegerToStream(aValue: Integer);
  protected
    procedure Guard; override;
  public
    procedure Execute; override;
    property Count: Integer read FCount write FCount;
  published
    property Stream: TMemoryStream read FStream write FStream;
    property OddLines: TStringList read FOddLines write FOddLines;
    property Component: TComponent read FComponent write FComponent;
    property EvenLines: TStringList read FEvenLines write FEvenLines;
    property ListInt: TList<Integer> read FListInt write FListInt;
  end;

{$REGION 'implementation TAdvancedCommand'}

procedure TAdvancedCommand.Guard;
begin
  System.Assert(Stream <> nil);
  System.Assert(OddLines <> nil);
  System.Assert(Component <> nil);
  System.Assert(EvenLines <> nil);
  System.Assert(ListInt <> nil);
end;

procedure TAdvancedCommand.WriteIntegerToStream(aValue: Integer);
begin
  Stream.Write(aValue, SizeOf(aValue));
end;

procedure TAdvancedCommand.Execute;
var
  i: Integer;
begin
  inherited;
  OddLines.Clear;
  EvenLines.Clear;
  WriteIntegerToStream(ListInt.Count);
  for i := 0 to ListInt.Count - 1 do
  begin
    WriteIntegerToStream(ListInt[i]);
    if odd(i) then
      OddLines.Add('Number: ' + ListInt[i].ToString)
    else
      EvenLines.Add(ListInt[i].ToString);
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
  FStringO := TStringList.Create;
  FStringE := TStringList.Create;
  FMemStream := TMemoryStream.Create;
  FList := TList<Integer>.Create;
  FList.AddRange([10,13,20,17,100,101,105]);
end;

procedure TestCommndFactory_AdvancedCommand.TearDown;
begin
  FStringO.Free;
  FStringE.Free;
  FMemStream.Free;
  FList.Free;
  FComponent.Free;
end;

procedure TestCommndFactory_AdvancedCommand.Execute;
var
  MyStream: TStream;
begin
  MyStream := (FMemStream as TStream);
  TCommandVclFactory.ExecuteCommand<TAdvancedCommand>
    ([FComponent, FStringO, FStringE, MyStream, FList]);
  Assert.AreEqual(3,FStringO.Count);
  Assert.AreEqual('Number: 13',FStringO[0]);
  Assert.AreEqual(4,FStringE.Count);
  Assert.AreEqual('10',FStringE[0]);
  Assert.AreEqual(32,integer(FMemStream.Size));
end;

// ------------------------------------------------------------------------
// ------------------------------------------------------------------------

initialization

end.
