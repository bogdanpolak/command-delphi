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

function ExpandStringWidth(s: String; len: Integer): string;
begin
  while Length(s) < len do
    s := s + ' ';
  Result := s;
end;

const
  APP_Version = '1.1';
  APP_Date = '2020-09-26';

var
  aTitle: String;

begin
  aTitle := ExpandStringWidth(Format('version_bumper.exe - %s (%s)',
      [APP_Version, APP_Date]), 50);
  writeln('+--------------------------------------------------------+');
  writeln('|   ' + aTitle + '   |');
  writeln('+--------------------------------------------------------+');
  writeln('| Can''t execute - required version string as parameter   |');
  writeln('| Syntax: version_bumper.exe version                     |');
  writeln('| Sample: version_bumper.exe "1.3"                       |');
  writeln('+--------------------------------------------------------+');
  writeln('');
  writeln('New version number is required to update files!');
  writeln('  Type new version ([Enter] exits application):');
  try
    TMainApplication.Run();
  except
    on E: Exception do
      writeln(E.ClassName, ': ', E.Message);
  end;

end.
