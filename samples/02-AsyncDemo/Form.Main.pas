unit Form.Main;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  Winapi.Windows, Winapi.Messages,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.ExtCtrls,

  Command.DiceRoll,
  Command.AsyncDiceRoll,
  Command.AsyncDiceRollExtra;

type
  TForm1 = class(TForm)
    pnProgressBar: TPanel;
    ProgressBar1: TProgressBar;
    Memo1: TMemo;
    GroupBoxDiceRolls: TGroupBox;
    btnAsycDiceRollCmd: TButton;
    btnDiceRollCommand: TButton;
    Timer1: TTimer;
    btnAsycDiceRollCmdTwo: TButton;
    ProgressBar2: TProgressBar;
    ProgressBar3: TProgressBar;
    chkShowProgressPanel: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure btnAsycDiceRollCmdClick(Sender: TObject);
    procedure btnDiceRollCommandClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnAsycDiceRollCmdTwoClick(Sender: TObject);
    procedure chkShowProgressPanelClick(Sender: TObject);
  private
    fCommand: TDiceRollCommand;
    fAsyncCommand: TAsyncDiceRollCommand;
    fAsyncCommand2: TAsyncDiceRollCommandEx;
    procedure DiceRoll_GenerateReport;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := fAsyncCommand.IsFinished;
  if not fAsyncCommand.IsFinished then
    ShowMessage('Can''t close application - async command in progress');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Memo1.Clear;
  fCommand := TDiceRollCommand.Create(Self);
  fAsyncCommand := TAsyncDiceRollCommand.Create(Self);
  fAsyncCommand2 := TAsyncDiceRollCommandEx.Create(Self);
  ReportMemoryLeaksOnShutdown := True;
end;

procedure TForm1.chkShowProgressPanelClick(Sender: TObject);
begin
  pnProgressBar.Visible := chkShowProgressPanel.Checked;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  btnAsycDiceRollCmd.Enabled := fAsyncCommand.IsFinished;
  btnAsycDiceRollCmdTwo.Enabled := fAsyncCommand2.IsFinished;
end;

procedure TForm1.btnDiceRollCommandClick(Sender: TObject);
begin
  fCommand.WithInjections([ProgressBar1, Memo1, 500]);
  fCommand.Execute;
end;

procedure TForm1.btnAsycDiceRollCmdClick(Sender: TObject);
begin
  fAsyncCommand.WithInjections([ProgressBar2, Memo1, 500]);
  fAsyncCommand.Execute;
end;

procedure TForm1.DiceRoll_GenerateReport;
var
  aDistribution: TArray<Integer>;
  i: Integer;
begin
  ProgressBar3.Position := fAsyncCommand2.RollsCount;
  aDistribution := fAsyncCommand2.GetDistribution;
  Memo1.Lines.Add(Format('Elapsed time: %.1f seconds',
    [fAsyncCommand2.GetElapsedTime.TotalSeconds]));
  Memo1.Lines.Add(Format('Dice results (%d-sided dice) (number of rolls: %d)',
    [fAsyncCommand2.MaxDiceValue, fAsyncCommand2.RollsCount]));
  for i := 1 to High(aDistribution) do
    Memo1.Lines.Add(Format('  [%d] : %d', [i, aDistribution[i]]));
end;

procedure TForm1.btnAsycDiceRollCmdTwoClick(Sender: TObject);
begin
  fAsyncCommand2.RollsCount := 500;
  fAsyncCommand2.WithEventBeforeStart(
    procedure
    begin
      ProgressBar3.Position := 0;
      ProgressBar3.Max := fAsyncCommand2.RollsCount;
    end);
  fAsyncCommand2.WithEventOnUpdate(
    procedure
    begin
      ProgressBar3.Position := fAsyncCommand2.CurrentRoll;
    end);
  fAsyncCommand2.WithEventAfterFinish(DiceRoll_GenerateReport);
  fAsyncCommand2.Execute;
end;

end.
