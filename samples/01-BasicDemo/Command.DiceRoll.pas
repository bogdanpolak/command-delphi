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
  TDiceRollCommand = class (TCommand)
  const
    RollCount = 100;
    MaxDiceValue = 6;
  private
    FOutput: TStrings;
    FProgressBar: TProgressBar;
  protected
    procedure DoGuard; override;
    procedure DoExecute; override;
  published
    property Results: TStrings read FOutput write FOutput;
    property ProgressBar: TProgressBar read FProgressBar write FProgressBar;
  end;

implementation

procedure TDiceRollCommand.DoGuard;
begin
  System.Assert(FOutput<>nil);
end;

procedure TDiceRollCommand.DoExecute;
var
  i: Integer;
  number: Integer;
  ResultCounters: array[1..MaxDiceValue] of integer;
begin
  if Assigned(FProgressBar) then
  begin
    FProgressBar.Max := RollCount;
    FProgressBar.Position := 0;
  end;
  for i := 1 to MaxDiceValue do
    ResultCounters[i] := 0;
  FOutput.Clear;
  for i := 0 to RollCount-1 do
  begin
    number := RandomRange(1,MaxDiceValue);
    ResultCounters[number]  := ResultCounters[number] + 1;
    Application.ProcessMessages;
    if Assigned(FProgressBar) then
      FProgressBar.Position := i+1;
    Sleep(1);
  end;
  for i := 1 to MaxDiceValue do
    FOutput.Add(Format('[%d] : %d',[i,ResultCounters[i]]));
end;

end.
