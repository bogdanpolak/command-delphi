program CommandDemo;

uses
  Vcl.Forms,
  Pattern.Command in '..\src\Pattern.Command.pas',
  Pattern.CommandAction in '..\src\Pattern.CommandAction.pas',
  Pattern.AsyncCommand in '..\src\Pattern.AsyncCommand.pas',
  Command.Button1 in 'Command.Button1.pas',
  Command.Button2 in 'Command.Button2.pas',
  Command.DiceRoll in 'Command.DiceRoll.pas',
  Helper.TWinControl in 'Helper.TWinControl.pas',
  Command.AsyncDiceRoll in 'Command.AsyncDiceRoll.pas',
  Form.Main in 'Form.Main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
