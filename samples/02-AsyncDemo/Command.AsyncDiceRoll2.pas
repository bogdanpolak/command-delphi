unit Command.AsyncDiceRoll2;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  Pattern.AsyncCommand;

type
  TAsyncDiceRollCommandTwo = class(TAsyncCommand)
  const
    MaxDiceValue = 6;
  private
    fRolls: TArray<Integer>;
    fResultDistribution: TArray<Integer>;
    fCurrentRoll: Integer;
    fRollsCount: Integer;
    function GetCurrentRoll: Integer;
    procedure SetCurrentRoll(aCurrentRoll: Integer);
  protected
    procedure DoGuard; override;
    procedure DoExecute; override;
  public
    function GetDistribution: TArray<Integer>;
  published
    property RollsCount: Integer read fRollsCount write fRollsCount;
    property CurrentRoll: Integer read GetCurrentRoll write SetCurrentRoll;
  end;

implementation

procedure TAsyncDiceRollCommandTwo.DoGuard;
begin
end;

function TAsyncDiceRollCommandTwo.GetDistribution: TArray<Integer>;
begin
  Result := fResultDistribution;
end;

function TAsyncDiceRollCommandTwo.GetCurrentRoll: Integer;
begin
  TMonitor.Enter(Self);
  try
    Result := fCurrentRoll;
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TAsyncDiceRollCommandTwo.SetCurrentRoll(aCurrentRoll: Integer);
begin
  TMonitor.Enter(Self);
  try
    fCurrentRoll := aCurrentRoll;
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TAsyncDiceRollCommandTwo.DoExecute;
var
  i: Integer;
  number: Integer;
begin
  SetCurrentRoll(0);
  SetLength(fRolls, fRollsCount);
  SetLength(fResultDistribution, MaxDiceValue + 1);
  for i := 1 to MaxDiceValue do
    fResultDistribution[i] := 0;
  for i := 0 to fRollsCount - 1 do
  begin
    number := RandomRange(1, MaxDiceValue + 1);
    fResultDistribution[number] := fResultDistribution[number] + 1;
    fRolls[i] := number;
    SetCurrentRoll(i);
    fThread.Sleep(2);
  end;
end;

end.
