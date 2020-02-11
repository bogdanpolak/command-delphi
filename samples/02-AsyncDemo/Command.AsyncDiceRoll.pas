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
    fProgressLabel: TLabel;
    fStep: Integer;
    fRollsCount: Integer;
    procedure DoDisplayStepInfo;
    procedure DoDisplaySummaryInfo;
  protected
    procedure DoGuard; override;
    procedure DoExecute; override;
  published
    property ReportMemo: TMemo read fReportMemo write fReportMemo;
    property ProgressBar: TProgressBar read fProgressBar write fProgressBar;
    property ProgressLabel: TLabel read fProgressLabel write fProgressLabel;
    property RollsCount: Integer read fRollsCount write fRollsCount;
  end;

implementation

procedure TAsyncDiceRollCommand.DoGuard;
begin
  System.Assert(fReportMemo <> nil);
  System.Assert(fProgressBar <> nil);
end;

procedure TAsyncDiceRollCommand.DoDisplayStepInfo;
begin
  fProgressBar.Position := fStep;
  if fProgressLabel <> nil then
    fProgressLabel.Caption := Format('calculating %d/%d', [fStep, fRollsCount]);
end;

procedure TAsyncDiceRollCommand.DoDisplaySummaryInfo;
var
  i: Integer;
begin
  ReportMemo.Lines.Add(Format('Elapsed time: %.1f seconds',
    [GetElapsedTime.TotalSeconds]));
  ReportMemo.Lines.Add
    (Format('Dice results (%d-sided dice) (number of rolls: %d)',
    [MaxDiceValue, fRollsCount]));
  for i := 1 to MaxDiceValue do
    ReportMemo.Lines.Add(Format('  [%d] : %d', [i, fResultDistribution[i]]));
end;

procedure TAsyncDiceRollCommand.DoExecute;
var
  idx: Integer;
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
  for idx := 1 to MaxDiceValue do
    fResultDistribution[idx] := 0;
  for idx := 0 to fRollsCount - 1 do
  begin
    fStep := idx + 1;
    Synchronize(DoDisplayStepInfo);
    number := RandomRange(1, MaxDiceValue + 1);
    fResultDistribution[number] := fResultDistribution[number] + 1;
    fRolls[idx] := number;
    fThread.Sleep(2);
    if TThread.CheckTerminated then
      Break;
  end;
  Synchronize(DoDisplayStepInfo);
  Synchronize(DoDisplaySummaryInfo);
end;

end.
