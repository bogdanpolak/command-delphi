unit Tests.TCommandFactory;

interface

uses
  DUnitX.TestFramework,
  System.Classes,
  System.SysUtils,
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

implementation

// ------------------------------------------------------------------------
// TestCommndFactory_BasicCommand: TCommandA
// ------------------------------------------------------------------------

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
// ------------------------------------------------------------------------

initialization

TDUnitX.RegisterTestFixture(TestCommndFactory_BasicCommand);

end.
