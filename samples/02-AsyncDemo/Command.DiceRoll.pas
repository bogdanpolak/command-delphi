unit Command.DiceRoll;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  Pattern.Command,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ComCtrls;

type
  TDiceRollCommand = class(TCommand)
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

procedure TDiceRollCommand.DoGuard;
begin
  System.Assert(fReportMemo <> nil);
  System.Assert(fProgressBar <> nil);
end;

procedure TDiceRollCommand.DoExecute;
var
  i: Integer;
  number: Integer;
begin
  fProgressBar.Max := fRollsCount;
  fProgressBar.Position := 0;
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
      fProgressBar.Position := i + 1;
      Application.ProcessMessages;
    end;
    Sleep(2);
  end;
  ReportResults;
end;

procedure TDiceRollCommand.ReportResults;
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
