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
    btnTermianteAllBackgroundJobs: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnAsycDiceRollCmdClick(Sender: TObject);
    procedure btnDiceRollCommandClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnAsycDiceRollCmdTwoClick(Sender: TObject);
    procedure chkShowProgressPanelClick(Sender: TObject);
    procedure btnTermianteAllBackgroundJobsClick(Sender: TObject);
  private
    fCommand: TDiceRollCommand;
    fAsyncCommand: TAsyncDiceRollCommand;
    fAsyncCommandEx: TAsyncDiceRollCommandEx;
    procedure DiceRoll_GenerateReport;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := not(fCommand.IsBusy) and not(fAsyncCommand.IsBusy) and
    not(fAsyncCommandEx.IsBusy);
  if not CanClose then
    ShowMessage('Can''t close application - async command in progress');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Memo1.Clear;
  fCommand := TDiceRollCommand.Create(Self);
  fAsyncCommand := TAsyncDiceRollCommand.Create(Self);
  fAsyncCommandEx := TAsyncDiceRollCommandEx.Create(Self);
  ReportMemoryLeaksOnShutdown := True;
end;

procedure TForm1.chkShowProgressPanelClick(Sender: TObject);
begin
  pnProgressBar.Visible := chkShowProgressPanel.Checked;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  btnDiceRollCommand.Enabled := not fCommand.IsBusy;
  btnAsycDiceRollCmd.Enabled := not fAsyncCommand.IsBusy;
  btnAsycDiceRollCmdTwo.Enabled := not fAsyncCommandEx.IsBusy;
end;

procedure TForm1.btnDiceRollCommandClick(Sender: TObject);
begin
  fCommand.WithInjections([ProgressBar1, Memo1, 500]);
  fCommand.Execute;
end;

procedure TForm1.btnTermianteAllBackgroundJobsClick(Sender: TObject);
begin
  fAsyncCommand.Terminate;
  fAsyncCommandEx.Terminate;
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
  ProgressBar3.Position := fAsyncCommandEx.RollsCount;
  aDistribution := fAsyncCommandEx.GetDistribution;
  Memo1.Lines.Add(Format('Elapsed time: %.1f seconds',
    [fAsyncCommandEx.GetElapsedTime.TotalSeconds]));
  Memo1.Lines.Add(Format('Dice results (%d-sided dice) (number of rolls: %d)',
    [fAsyncCommandEx.MaxDiceValue, fAsyncCommandEx.RollsCount]));
  for i := 1 to High(aDistribution) do
    Memo1.Lines.Add(Format('  [%d] : %d', [i, aDistribution[i]]));
end;

procedure TForm1.btnAsycDiceRollCmdTwoClick(Sender: TObject);
begin
  fAsyncCommandEx.RollsCount := 500;
  fAsyncCommandEx.WithEventBeforeStart(
    procedure
    begin
      ProgressBar3.Position := 0;
      ProgressBar3.Max := fAsyncCommandEx.RollsCount;
    end);
  fAsyncCommandEx.WithEventOnUpdate(
    procedure
    begin
      ProgressBar3.Position := fAsyncCommandEx.CurrentRoll;
    end);
  fAsyncCommandEx.WithEventAfterFinish(DiceRoll_GenerateReport);
  fAsyncCommandEx.Execute;
end;

end.
