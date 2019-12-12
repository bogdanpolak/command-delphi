unit Form.Main;

interface

uses
  Winapi.Windows, Winapi.Messages,

  System.SysUtils, System.Variants, System.Classes, System.Actions,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ActnList, Vcl.ExtCtrls, Vcl.PlatformDefaultStyleActnCtrls,
  Vcl.ActnMan, Vcl.ComCtrls,

  Pattern.Command,
  Pattern.CommandAction,
  Command.DiceRoll;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    GroupBoxSimpleDemo: TGroupBox;
    btnExecuteCommand: TButton;
    GroupBoxButtonCommands: TGroupBox;
    GroupBoxDiceRolls: TGroupBox;
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    CheckBox1: TCheckBox;
    Button3: TButton;
    chkShowProgressbar: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure chkShowProgressbarClick(Sender: TObject);
    procedure btnExecuteCommandClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    fStrsDiceResults: TStringList;
    actCommandDiceRoll: TCommandAction;
    actCommandButon1: TCommandAction;
    actCommandButon2: TCommandAction;
    procedure BuildCommandsAndActions;
    procedure OnFormSetup;
    procedure OnFormTearDown;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Command.Button1,
  Command.Button2,
  Helper.TWinControl;

procedure TForm1.BuildCommandsAndActions;
begin
  // ---------------------------------------------------------
  // ---------------------------------------------------------
  actCommandButon1 := TCommandAction.Create(Self)
    .SetupCommand(TButon1Command.Create(Self))
    .SetupCaption('Run command: Button1') //.
    .Inject([Memo1]);
  // ---------------------------------------------------------
  // ---------------------------------------------------------
  actCommandButon2 := TCommandAction.Create(Self)
    .SetupCommand(TButon2Command.Create(Self))
    .SetupCaption('Run command: Button2') //.
    .Inject([Memo1, Edit1]) //.
    .SetupEventOnUpdate(
    procedure(actCommand: TCommandAction)
    begin
      actCommand.Enabled := CheckBox1.Checked;
    end);
  // ---------------------------------------------------------
  // ---------------------------------------------------------
  actCommandDiceRoll := TCommandAction.Create(Self)
    .SetupCommand(TDiceRollCommand.Create(Self))
    .SetupCaption('Dice Rolls Command') //.
    .Inject([fStrsDiceResults]) //.
    .SetupEventOnUpdate(
    procedure(actCommand: TCommandAction)
    begin
      (actCommand.Command as TDiceRollCommand).ProgressBar :=
        GroupBoxDiceRolls.FindChildControlRecursiveByType(TProgressBar)
        as TProgressBar;
    end) //.
    .SetupEventAfterExecution(
    procedure(actCommand: TCommandAction)
    var
      cmd: TDiceRollCommand;
      i: integer;
    begin
      cmd := actCommand.Command as TDiceRollCommand;
      Memo1.Lines.Add
        (Format('Dice results (%d-sided dice) (number of rolls: %d)',
        [cmd.MaxDiceValue, cmd.RollCount]));
      for i := 0 to cmd.Results.Count - 1 do
        Memo1.Lines.Add('  ' + cmd.Results[i]);
    end);
  actCommandDiceRoll.DisableDuringExecution := True;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  // this event is not called, see method: BuildCommandsAndActions
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  // this event is not called, see method: BuildCommandsAndActions
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  // this event is not called, see method: BuildCommandsAndActions
end;

procedure TForm1.OnFormSetup;
begin
  fStrsDiceResults := TStringList.Create;
  BuildCommandsAndActions;
  Button1.Action := actCommandButon1;
  Button2.Action := actCommandButon2;
  Button3.Action := actCommandDiceRoll;
end;

procedure TForm1.OnFormTearDown;
begin
  fStrsDiceResults.Free;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  OnFormTearDown;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Memo1.Clear;
  ReportMemoryLeaksOnShutdown := True;
  OnFormSetup;
end;

procedure TForm1.btnExecuteCommandClick(Sender: TObject);
begin
  TCommand.AdhocExecute<TButon1Command>([Memo1]);
end;

procedure TForm1.chkShowProgressbarClick(Sender: TObject);
var
  aProgressBar: TProgressBar;
begin
  if chkShowProgressbar.Checked then
  begin
    aProgressBar := TProgressBar.Create(Self);
    with aProgressBar do
    begin
      Name := 'ProgrssBarRolls1';
      Parent := GroupBoxDiceRolls;
      Top := 999;
      Align := alTop;
      AlignWithMargins := True;
    end;
  end
  else
  begin
    aProgressBar := GroupBoxDiceRolls.FindChildControl('ProgrssBarRolls1')
      as TProgressBar;
    aProgressBar.Free;
  end;
end;

end.
