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
    GroupBox1: TGroupBox;
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    Edit1: TEdit;
    Button3: TButton;
    CheckBox1: TCheckBox;
    Button4: TButton;
    ActionManager1: TActionManager;
    actExecuteTwoCommands: TAction;
    actDiceRolls: TAction;
    actShowProgressBar: TAction;
    CheckBox2: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actExecuteTwoCommandsExecute(Sender: TObject);
    procedure actShowProgressBarExecute(Sender: TObject);
    procedure actDiceRollsExecute(Sender: TObject);
  private
    cmdRollDice: TDiceRollCommand;
    fStrsDiceResults: TStringList;
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

procedure TForm1.OnFormSetup;
begin
  fStrsDiceResults := TStringList.Create;
  // ---------------------------------------------------------
  Button1.Action := TCommandAction.Create(Self)
    .SetupCaption('Run command: Button1')
    .SetupCommand(TButon1Command.Create(Self).Inject([Memo1]));
  Button2.Action := TCommandAction.Create(Self)
    .SetupCaption('Run command: Button2')
    .SetupCommand(TButon2Command.Create(Self).Inject([Memo1, Edit1]))
    .SetupEventOnUpdate(
    procedure(actCommand: TCommandAction)
    begin
      actCommand.Enabled := CheckBox1.Checked;
    end);
  cmdRollDice := TDiceRollCommand.Create(Self);
  cmdRollDice.Results := fStrsDiceResults;
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
  ReportMemoryLeaksOnShutdown := true;
  OnFormSetup;
end;

procedure TForm1.actShowProgressBarExecute(Sender: TObject);
begin
  with TProgressBar.Create(Self) do
  begin
    Name := 'ProgrssBar1';
    Parent := GroupBox1;
    Top := 999;
    Align := alTop;
    AlignWithMargins := true;
  end;
  actShowProgressBar.Visible := False;
end;

procedure TForm1.actDiceRollsExecute(Sender: TObject);
var
  i: Integer;
begin
  cmdRollDice.ProgressBar := Self.FindChildControlRecursiveByType(TProgressBar)
    as TProgressBar;
  try
    actDiceRolls.Enabled := False;
    cmdRollDice.Execute;
  finally
    actDiceRolls.Enabled := true;
  end;
  Memo1.Lines.Add(Format('Dice results (%d-sided dice) (number of rolls: %d)',
    [cmdRollDice.MaxDiceValue, cmdRollDice.RollCount]));
  for i := 0 to cmdRollDice.Results.Count - 1 do
    Memo1.Lines.Add('  ' + cmdRollDice.Results[i]);
end;

procedure TForm1.actExecuteTwoCommandsExecute(Sender: TObject);
begin
  TCommand.AdhocExecute<TButon1Command>([Memo1]);
  TCommand.AdhocExecute<TButon2Command>([Memo1, Edit1]);
end;

end.
