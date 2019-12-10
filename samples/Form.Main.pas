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
    btnExecuteCommand: TButton;
    Button1: TButton;
    Memo1: TMemo;
    Edit1: TEdit;
    Button2: TButton;
    CheckBox1: TCheckBox;
    Button3: TButton;
    chkShowProgressbar: TCheckBox;
    Bevel1: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure chkShowProgressbarClick(Sender: TObject);
    procedure btnExecuteCommandClick(Sender: TObject);
  private
    fStrsDiceResults: TStringList;
    actCommandDiceRoll: TCommandAction;
    actCommandButon1: TCommandAction;
    actCommandButon2: TCommandAction;
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
  actCommandButon1 := TCommandAction.Create(Self)
    .SetupCaption('Run command: Button1')
    .SetupCommand(TButon1Command.Create(Self).Inject([Memo1]));
  // ---------------------------------------------------------
  actCommandButon2 := TCommandAction.Create(Self)
    .SetupCaption('Run command: Button2')
    .SetupCommand(TButon2Command.Create(Self).Inject([Memo1, Edit1]))
    .SetupEventOnUpdate(
    procedure(actCommand: TCommandAction)
    begin
      actCommand.Enabled := CheckBox1.Checked;
    end);
  // ---------------------------------------------------------
  actCommandDiceRoll := TCommandAction.Create(Self)
    .SetupCaption('Dice Rolls Command')
    .SetupCommand(TDiceRollCommand.Create(Self).Inject([fStrsDiceResults]))
    .SetupEventOnUpdate(
    procedure(actCommand: TCommandAction)
    begin
      (actCommand.Command as TDiceRollCommand).ProgressBar :=
        Self.FindChildControlRecursiveByType(TProgressBar) as TProgressBar;
    end).SetupEventAfterExecution(
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
  // ---------------------------------------------------------
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
      Name := 'ProgrssBar1';
      Parent := GroupBox1;
      Top := 999;
      Align := alTop;
      AlignWithMargins := True;
    end;
  end
  else
  begin
    aProgressBar := GroupBox1.FindChildControl('ProgrssBar1') as TProgressBar;
    aProgressBar.Free;
  end;
end;

end.
