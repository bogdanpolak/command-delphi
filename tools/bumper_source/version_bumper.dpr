program version_bumper;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Main in 'Main.pas',
  AppConfiguration in 'AppConfiguration.pas',
  Processor.PascalUnit in 'Processor.PascalUnit.pas',
  Processor.ReadmeMarkdown in 'Processor.ReadmeMarkdown.pas',
  Processor.Utils in 'Processor.Utils.pas';

const
  APP_Version = '1.1';
  APP_Date = '2020-09-26';

begin
  writeln(Format('*** version_bumper.exe - %s (%s)',[APP_Version,APP_Date]));
  try
    TMainApplication.Run();
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
