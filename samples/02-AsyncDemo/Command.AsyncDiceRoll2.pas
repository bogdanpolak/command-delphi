unit Command.AsyncDiceRoll2;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.ComCtrls, // for TProgressBar class (needs refactoring)
  Pattern.AsyncCommand;

type
  TAsyncDiceRollCommandTwo = class(TAsyncCommand)
  const
    MaxDiceValue = 6;
    ReportEveryRolls = 10;
  private
    fRolls: TArray<Integer>;
    fResultDistribution: TArray<Integer>;
    fProgressBar: TProgressBar;
    fRollsCount: Integer;
  protected
    procedure DoGuard; override;
    procedure DoExecute; override;
  public
    function GetDistribution: TArray<Integer>;
  published
    property ProgressBar: TProgressBar read fProgressBar write fProgressBar;
    property RollsCount: Integer read fRollsCount write fRollsCount;
  end;

implementation

procedure TAsyncDiceRollCommandTwo.DoGuard;
begin
  System.Assert(fProgressBar <> nil);
end;

function TAsyncDiceRollCommandTwo.GetDistribution: TArray<Integer>;
begin
  Result := fResultDistribution;
end;

procedure TAsyncDiceRollCommandTwo.DoExecute;
var
  i: Integer;
  number: Integer;
  counterReport: Integer;
begin
  Synchronize(
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
      Synchronize(
        procedure
        begin
          fProgressBar.Position := i + 1;
        end);
    end;
    dec(counterReport);
    fThread.Sleep(2);
  end;
end;

end.
