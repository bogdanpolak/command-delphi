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
  Command.AsyncDiceRoll2;

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
    procedure FormCreate(Sender: TObject);
    procedure btnAsycDiceRollCmdClick(Sender: TObject);
    procedure btnDiceRollCommandClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnAsycDiceRollCmdTwoClick(Sender: TObject);
  private
    fCommand: TDiceRollCommand;
    fAsyncCommand: TAsyncDiceRollCommand;
    fAsyncCommand2: TAsyncDiceRollCommandTwo;
    procedure WriteReport;
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
  fAsyncCommand2 := TAsyncDiceRollCommandTwo.Create(Self);
  ReportMemoryLeaksOnShutdown := True;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  btnAsycDiceRollCmd.Enabled := fAsyncCommand.IsFinished;
  btnAsycDiceRollCmdTwo.Enabled := fAsyncCommand2.IsFinished;
end;

procedure TForm1.btnAsycDiceRollCmdClick(Sender: TObject);
begin
  fAsyncCommand.WithInjections([ProgressBar1, Memo1, 500]);
  fAsyncCommand.Execute;
end;

procedure TForm1.btnDiceRollCommandClick(Sender: TObject);
begin
  fCommand.WithInjections([ProgressBar1, Memo1, 100]);
  fCommand.Execute;
end;

procedure TForm1.WriteReport;
var
  aDistribution: TArray<Integer>;
  i: Integer;
begin
  ProgressBar1.Position := ProgressBar1.Max;
  aDistribution := fAsyncCommand2.GetDistribution;
  Memo1.Lines.Add(Format('Dice results (%d-sided dice) (number of rolls: %d)',
    [fAsyncCommand2.MaxDiceValue, fAsyncCommand2.RollsCount]));
  for i := 1 to High(aDistribution) do
    Memo1.Lines.Add(Format('  [%d] : %d', [i, aDistribution[i]]));
end;

procedure TForm1.btnAsycDiceRollCmdTwoClick(Sender: TObject);
begin
  fAsyncCommand2.WithInjections([ProgressBar1, 500]);
  fAsyncCommand2 //--+
    .WithEventAfterFinish(WriteReport).Execute;
end;

end.
