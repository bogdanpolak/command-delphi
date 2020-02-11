unit Command.DiceRoll;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Diagnostics,
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
    fProgressLabel: TLabel;
    fRollsCount: Integer;
    fStep: Integer;
    fIsTerminated: boolean;
    procedure DoDisplayStepInfo;
    procedure DoDisplaySummaryInfo;
    procedure DoSomeOtherWork(aSpendMiliseconds: double);
  protected
    procedure DoGuard; override;
    procedure DoExecute; override;
  public
    procedure Terminate;
  published
    property ReportMemo: TMemo read fReportMemo write fReportMemo;
    property ProgressBar: TProgressBar read fProgressBar write fProgressBar;
    property ProgressLabel: TLabel read fProgressLabel write fProgressLabel;
    property RollsCount: Integer read fRollsCount write fRollsCount;
  end;

implementation

procedure TDiceRollCommand.DoGuard;
begin
  System.Assert(fReportMemo <> nil);
  System.Assert(fProgressBar <> nil);
end;

procedure TDiceRollCommand.Terminate;
begin
  fIsTerminated := True;
end;

procedure TDiceRollCommand.DoDisplayStepInfo;
begin
  fProgressBar.Position := fStep;
  if fProgressLabel <> nil then
    fProgressLabel.Caption := Format('calculating %d/%d', [fStep, fRollsCount]);
end;

procedure TDiceRollCommand.DoDisplaySummaryInfo;
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

procedure TDiceRollCommand.DoSomeOtherWork (aSpendMiliseconds: double);
var
  sw: TStopwatch;
begin
  sw := TStopWatch.Create;
  sw.Start;
  while sw.Elapsed.Milliseconds<aSpendMiliseconds do;
end;

procedure TDiceRollCommand.DoExecute;
var
  idx: Integer;
  number: Integer;
begin
  fIsTerminated := False;
  fProgressBar.Max := fRollsCount;
  fProgressBar.Position := 0;
  SetLength(fRolls, fRollsCount);
  SetLength(fResultDistribution, MaxDiceValue + 1);
  for idx := 1 to MaxDiceValue do
    fResultDistribution[idx] := 0;
  for idx := 0 to fRollsCount - 1 do
  begin
    fStep := idx + 1;
    DoDisplayStepInfo;
    number := RandomRange(1, MaxDiceValue + 1);
    fResultDistribution[number] := fResultDistribution[number] + 1;
    fRolls[idx] := number;
    DoSomeOtherWork(1.5);
    Application.ProcessMessages;
    if fIsTerminated then
      Break;
  end;
  DoDisplaySummaryInfo;
  DoDisplayStepInfo;
end;

end.
