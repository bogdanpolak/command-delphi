program CommandDemo;

uses
  Vcl.Forms,
  Pattern.Command in '..\..\src\Pattern.Command.pas',
  Pattern.CommandAction in '..\..\src\Pattern.CommandAction.pas',
  Command.Button1 in 'Command.Button1.pas',
  Command.Button2 in 'Command.Button2.pas',
  Form.Main in 'Form.Main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
