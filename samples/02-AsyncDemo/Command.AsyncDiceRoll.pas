unit Command.AsyncDiceRoll;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.ComCtrls, // for TProgressBar class (needs refactoring)
  Vcl.StdCtrls, // for TMemo class (needs refactoring)
  Pattern.AsyncCommand;

type
  TAsyncDiceRollCommand = class(TAsyncCommand)
  const
    MaxDiceValue = 6;
    ReportEveryRolls = 10;
  private
    fRolls: TArray<Integer>;
    fResultDistribution: TArray<Integer>;
    fReportMemo: TMemo;
    fProgressBar: TProgressBar;
    fRollsCount: Integer;
    procedure ReportResults;
  protected
    procedure DoGuard; override;
    procedure DoPrepare; override;
    procedure DoExecute; override;
    procedure DoTeardown; override;
  published
    property ReportMemo: TMemo read fReportMemo write fReportMemo;
    property ProgressBar: TProgressBar read fProgressBar write fProgressBar;
    property RollsCount: Integer read fRollsCount write fRollsCount;
  end;

implementation

procedure TAsyncDiceRollCommand.DoGuard;
begin
  System.Assert(fReportMemo <> nil);
  System.Assert(fProgressBar <> nil);
end;

procedure TAsyncDiceRollCommand.DoPrepare;
begin
end;

procedure TAsyncDiceRollCommand.DoExecute;
var
  i: Integer;
  number: Integer;
  counterReport: Integer;
begin
  TThread.Synchronize(fThread,
    procedure
    begin
      fProgressBar.Max := fRollsCount;
      fProgressBar.Position := 0;
    end);
  SetLength(fRolls, fRollsCount);
  SetLength(fResultDistribution, MaxDiceValue + 1);
  counterReport := 0;
  for i := 1 to MaxDiceValue do
    fResultDistribution[i] := 0;
  for i := 0 to fRollsCount - 1 do
  begin
    number := RandomRange(1, MaxDiceValue + 1);
    fResultDistribution[number] := fResultDistribution[number] + 1;
    fRolls[i] := number;
    if counterReport = 0 then
    begin
      counterReport := ReportEveryRolls;
      TThread.Synchronize(fThread,
        procedure
        begin
          fProgressBar.Position := i + 1;
        end);
    end;
    dec(counterReport);
    fThread.Sleep(2);
  end;
  TThread.Synchronize(fThread, ReportResults);
end;

procedure TAsyncDiceRollCommand.ReportResults;
var
  i: Integer;
begin
  fProgressBar.Position := fRollsCount;
  ReportMemo.Lines.Add
    (Format('Dice results (%d-sided dice) (number of rolls: %d)',
    [MaxDiceValue, fRollsCount]));
  for i := 1 to MaxDiceValue do
    ReportMemo.Lines.Add(Format('  [%d] : %d', [i, fResultDistribution[i]]));
end;

procedure TAsyncDiceRollCommand.DoTeardown;
begin
end;

end.
