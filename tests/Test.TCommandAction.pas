unit Test.TCommandAction;

interface

uses
  DUnitX.TestFramework,
  System.Classes,
  System.SysUtils,
  Pattern.Command,
  Pattern.CommandAction;

{$M+}

type

  [TestFixture]
  TestCommandAction = class(TObject)
  strict private
    fOwnerComponent: TComponent;
    fStringList: TStringList;
    fAction: TCommandAction;
  private
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    procedure Call_SetupCaption;
    procedure Call_SetupCommand;
  end;

implementation

type
  TTestCommand = class (TCommand)
  const
    DefaultRange = 10;
  private
    FRandomNumbers: TStringList;
    FRange: integer;
  protected
    procedure DoGuard; override;
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Range: integer read FRange write FRange;
    property RandomNumbers: TStringList read FRandomNumbers write FRandomNumbers;
  end;

{$REGION 'implementation TTestCommand -----------------'}

constructor TTestCommand.Create(AOwner: TComponent);
begin
  inherited;
  Randomize;
  Range := DefaultRange;
end;

procedure TTestCommand.DoGuard;
begin
  System.Assert(RandomNumbers<>nil);
end;

procedure TTestCommand.DoExecute;
begin
  RandomNumbers.Add ( (1+Random(Range)).ToString );
end;

{$ENDREGION --------------------------------------------}

procedure TestCommandAction.Setup;
begin
  fOwnerComponent:= TComponent.Create(nil);
  fAction:= TCommandAction.Create(fOwnerComponent);
  fStringList:= TStringList.Create;
end;

procedure TestCommandAction.TearDown;
begin
  fOwnerComponent.Free;
  fStringList.Free;
end;

procedure TestCommandAction.Call_SetupCaption;
var
  act: TCommandAction;
begin
  // Arrage & Act:
  fAction.SetupCaption('Execute test command');
  // Assert
  Assert.AreEqual('Execute test command',fAction.Caption);
end;

procedure TestCommandAction.Call_SetupCommand;
var
  cmd: TTestCommand;
  act: TCommandAction;
begin
  // Arrage:
  cmd := TTestCommand.Create(fOwnerComponent);
  cmd.Inject([fStringList]);
  // Act:
  fAction.SetupCommand(cmd);
  fAction.Execute;
  fAction.Execute;
  // Assert
  Assert.AreEqual(2,cmd.RandomNumbers.Count);
end;

end.
