program AsyncCommandDemo;

uses
  Vcl.Forms,
  Pattern.AsyncCommand in '..\..\src\Pattern.AsyncCommand.pas',
  Pattern.Command in '..\..\src\Pattern.Command.pas',
  Command.AsyncDiceRoll in 'Command.AsyncDiceRoll.pas',
  Command.DiceRoll in 'Command.DiceRoll.pas',
  Form.Main in 'Form.Main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
