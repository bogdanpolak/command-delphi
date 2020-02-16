unit Tests.TCommandAction;

interface

uses
  DUnitX.TestFramework,
  System.Classes,
  System.SysUtils,
  Pattern.Command,
  Pattern.CommandAction,
  Vcl.StdCtrls,
  Vcl.Forms;

{$M+}

type

  [TestFixture]
  TestCommandAction = class(TObject)
  private
    fOwnerComponent: TComponent;
    fStringList: TStringList;
    fAction: TCommandAction;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    procedure ActionWithCaption;
    procedure ActionWithCommand;
    procedure ActionWithShotcut;
    procedure ActionWitnEventOnUpdate;
    procedure ActionWitEventAfterExecution;
    procedure ActionWithInjections;
    procedure ActionCaption_WillChangeButtonCaption;
  end;

implementation

uses
  Vcl.Menus;

type
  TTestCommand = class(TCommand)
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
    property RandomNumbers: TStringList read FRandomNumbers
      write FRandomNumbers;
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
  System.Assert(RandomNumbers <> nil);
end;

procedure TTestCommand.DoExecute;
begin
  RandomNumbers.Add((1 + Random(Range)).ToString);
end;

{$ENDREGION --------------------------------------------}

procedure TestCommandAction.Setup;
begin
  fOwnerComponent := TComponent.Create(nil);
  fAction := TCommandAction.Create(fOwnerComponent);
  fStringList := TStringList.Create;
end;

procedure TestCommandAction.TearDown;
begin
  fOwnerComponent.Free;
  fStringList.Free;
end;

procedure TestCommandAction.ActionWithCaption;
begin
  // Arrage & Act:
  fAction.WithCaption('Execute test command');
  // Assert
  Assert.AreEqual('Execute test command', fAction.Caption);
end;

procedure TestCommandAction.ActionWithCommand;
var
  cmd: TTestCommand;
begin
  // Arrage:
  cmd := TTestCommand.Create(fOwnerComponent);
  cmd.WithInjections([fStringList]);
  // Act:
  fAction.WithCommand(cmd);
  fAction.Execute;
  fAction.Execute;
  // Assert
  Assert.AreEqual(2, cmd.RandomNumbers.Count);
end;

procedure TestCommandAction.ActionWithShotcut;
var
  aShortCut: TShortCut;
begin
  aShortCut := TextToShortCut('CTRL+K');
  fAction.WithShortCut(aShortCut);
  Assert.AreEqual(ShortCutToText(aShortCut), ShortCutToText(fAction.ShortCut));
end;

procedure TestCommandAction.ActionWitnEventOnUpdate;
begin
  fAction.WithEventOnUpdate(
    procedure(act: TCommandAction)
    begin
      act.Tag := act.Tag + 1;
    end);
  fAction.Update;
  Assert.AreEqual(1, fAction.Tag);
end;

procedure TestCommandAction.ActionWitEventAfterExecution;
begin
  fAction.Tag := -1;
  fAction.Command := TTestCommand.Create(fOwnerComponent);
  fAction.Command.WithInjections([fStringList]);
  fAction.WitEventAfterExecution(
    procedure(act: TCommandAction)
    begin
      act.Tag := 99;
    end);

  fAction.Execute;

  Assert.AreEqual(99, fAction.Tag);
end;

procedure TestCommandAction.ActionWithInjections;
var
  actualNumbers: integer;
begin
  fAction // --+
    .WithCommand(TTestCommand.Create(fOwnerComponent)) //--+
    .WithInjections([fStringList]);
  fAction.Execute;
  fAction.Execute;
  fAction.Execute;
  actualNumbers := (fAction.Command as TTestCommand).RandomNumbers.Count;
  Assert.AreEqual(3, actualNumbers);
end;

procedure TestCommandAction.ActionCaption_WillChangeButtonCaption;
var
  aButton: TButton;
begin
  aButton := TButton.Create(fOwnerComponent);
  fAction.WithCaption('Sample caption');
  aButton.Action := fAction;
  Assert.AreEqual('Sample caption', aButton.Caption);
end;

end.
