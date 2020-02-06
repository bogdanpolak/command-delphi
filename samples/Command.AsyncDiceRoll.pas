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
    RollCount = 200;
    MaxDiceValue = 6;
    ReportEveryRolls = 10;
  private
    // internal momeory (for working thread)
    fRolls: TArray<Integer>;
    fResultDistribution: TArray<Integer>;
    // shared momeory (access only through main thread)
    fReportMemo: TMemo;
    fProgressBar: TProgressBar;
  protected
    procedure DoGuard; override; // .... main thread
    procedure DoPrepare; override; // .. main thread
    procedure DoExecute; override; // .. !!! WORKING THREAD:
    procedure DoTeardown; override; // . main thread
  published
    property ReportMemo: TMemo read fReportMemo write fReportMemo;
    property ProgressBar: TProgressBar read fProgressBar write fProgressBar;
  end;

implementation

procedure TAsyncDiceRollCommand.DoGuard;
begin
  System.Assert(ReportMemo <> nil);
  System.Assert(ProgressBar <> nil);
end;

procedure TAsyncDiceRollCommand.DoPrepare;
begin
  fProgressBar.Max := RollCount;
  fProgressBar.Position := 0;
end;

procedure TAsyncDiceRollCommand.DoExecute;
var
  i: Integer;
  number: Integer;
  counterReport: Integer;
begin
  SetLength(fRolls, RollCount);
  SetLength(fResultDistribution, MaxDiceValue + 1);
  counterReport := 0;
  for i := 1 to MaxDiceValue do
    fResultDistribution[i] := 0;
  for i := 0 to RollCount - 1 do
  begin
    number := RandomRange(1, MaxDiceValue);
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
    fThread.Sleep(10);
  end;
end;

procedure TAsyncDiceRollCommand.DoTeardown;
var
  i: Integer;
begin
  fProgressBar.Position := RollCount;
  ReportMemo.Lines.Add
        (Format('Dice results (%d-sided dice) (number of rolls: %d)',
        [MaxDiceValue, RollCount]));
  for i := 1 to MaxDiceValue do
    ReportMemo.Lines.Add(Format('  [%d] : %d', [i, fResultDistribution[i]]));
end;

end.
