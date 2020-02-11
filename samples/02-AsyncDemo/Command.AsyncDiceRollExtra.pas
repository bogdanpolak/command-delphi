unit Command.AsyncDiceRollExtra;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Diagnostics,
  Pattern.AsyncCommand;

type
  TAsyncDiceRollCommandEx = class(TAsyncCommand)
  const
    MaxDiceValue = 6;
  private
    fRolls: TArray<Integer>;
    fResultDistribution: TArray<Integer>;
    fStep: Integer;
    fRollsCount: Integer;
    function GetStep: Integer;
    procedure SetStep(aStep: Integer);
    procedure DoSomeOtherWork(aSpendMiliseconds: double);
  protected
    procedure DoGuard; override;
    procedure DoExecute; override;
  public
    function GetDistribution: TArray<Integer>;
    property Step: Integer read GetStep write SetStep;
  published
    property RollsCount: Integer read fRollsCount write fRollsCount;
  end;

implementation

procedure TAsyncDiceRollCommandEx.DoGuard;
begin
end;

function TAsyncDiceRollCommandEx.GetDistribution: TArray<Integer>;
begin
  Result := fResultDistribution;
end;

function TAsyncDiceRollCommandEx.GetStep: Integer;
begin
  TMonitor.Enter(Self);
  try
    Result := fStep;
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TAsyncDiceRollCommandEx.SetStep(aStep: Integer);
begin
  TMonitor.Enter(Self);
  try
    fStep := aStep;
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TAsyncDiceRollCommandEx.DoSomeOtherWork (aSpendMiliseconds: double);
var
  sw: TStopwatch;
begin
  sw := TStopWatch.Create;
  sw.Start;
  while sw.Elapsed.Milliseconds<aSpendMiliseconds do;
end;

procedure TAsyncDiceRollCommandEx.DoExecute;
var
  i: Integer;
  number: Integer;
begin
  SetStep(0);
  SetLength(fRolls, fRollsCount);
  SetLength(fResultDistribution, MaxDiceValue + 1);
  for i := 1 to MaxDiceValue do
    fResultDistribution[i] := 0;
  for i := 0 to fRollsCount - 1 do
  begin
    SetStep(i+1);
    number := RandomRange(1, MaxDiceValue + 1);
    fResultDistribution[number] := fResultDistribution[number] + 1;
    fRolls[i] := number;
    DoSomeOtherWork(1.5);
    if TThread.CheckTerminated then
      Break;
  end;
end;

end.
