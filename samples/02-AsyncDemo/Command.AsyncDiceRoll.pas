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
  private
    fRolls: TArray<Integer>;
    fResultDistribution: TArray<Integer>;
    fReportMemo: TMemo;
    fProgressBar: TProgressBar;
    fRollsCount: Integer;
    procedure ReportResults;
  protected
    procedure DoGuard; override;
    procedure DoExecute; override;
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

procedure TAsyncDiceRollCommand.DoExecute;
var
  i: Integer;
  number: Integer;
begin
  Synchronize(
    procedure
    begin
      fProgressBar.Max := fRollsCount;
      fProgressBar.Position := 0;
    end);
  SetLength(fRolls, fRollsCount);
  SetLength(fResultDistribution, MaxDiceValue + 1);
  for i := 1 to MaxDiceValue do
    fResultDistribution[i] := 0;
  for i := 0 to fRollsCount - 1 do
  begin
    number := RandomRange(1, MaxDiceValue + 1);
    fResultDistribution[number] := fResultDistribution[number] + 1;
    fRolls[i] := number;
    if i mod 10 = 0 then
    begin
      Synchronize(
        procedure
        begin
          fProgressBar.Position := i + 1;
        end);
    end;
    fThread.Sleep(2);
    if TThread.CheckTerminated then
      Break;
  end;
  Synchronize(ReportResults);
end;

procedure TAsyncDiceRollCommand.ReportResults;
var
  i: Integer;
begin
  fProgressBar.Position := fRollsCount;
  ReportMemo.Lines.Add(Format('Elapsed time: %.1f seconds',
    [GetElapsedTime.TotalSeconds]));
  ReportMemo.Lines.Add
    (Format('Dice results (%d-sided dice) (number of rolls: %d)',
    [MaxDiceValue, fRollsCount]));
  for i := 1 to MaxDiceValue do
    ReportMemo.Lines.Add(Format('  [%d] : %d', [i, fResultDistribution[i]]));
end;

end.
