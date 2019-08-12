program CommandFactoryDemo;

uses
  Vcl.Forms,
  Form.Main in 'Form.Main.pas' {Form1},
  Command.Button1 in 'Command.Button1.pas',
  Vcl.Pattern.Command in '..\src\Vcl.Pattern.Command.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
