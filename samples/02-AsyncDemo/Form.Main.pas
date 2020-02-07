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
  Command.AsyncDiceRoll;

type
  TForm1 = class(TForm)
    pnProgressBar: TPanel;
    ProgressBar1: TProgressBar;
    Memo1: TMemo;
    GroupBoxDiceRolls: TGroupBox;
    btnAsycDiceRollCmd: TButton;
    btnDiceRollCommand: TButton;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure btnAsycDiceRollCmdClick(Sender: TObject);
    procedure btnDiceRollCommandClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    fCommand: TDiceRollCommand;
    fAsyncCommand: TAsyncDiceRollCommand;
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
    ShowMessage('Cant colse application - async command in progress');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Memo1.Clear;
  fCommand := TDiceRollCommand.Create(Self);
  fAsyncCommand := TAsyncDiceRollCommand.Create(Self);
  ReportMemoryLeaksOnShutdown := True;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  btnAsycDiceRollCmd.Enabled := fAsyncCommand.IsFinished;
end;

procedure TForm1.btnAsycDiceRollCmdClick(Sender: TObject);
begin
  fAsyncCommand.WithInjections([ProgressBar1,Memo1,500]);
  fAsyncCommand.Execute;
end;

procedure TForm1.btnDiceRollCommandClick(Sender: TObject);
begin
  fCommand.WithInjections([ProgressBar1,Memo1,100]);
  fCommand.Execute;
end;

end.
