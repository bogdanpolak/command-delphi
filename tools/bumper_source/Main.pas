unit Main;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,

  AppConfiguration;

type
  TMainApplication = class
  private
    fAppConfig: TAppConfiguration;
    fSilentMode: boolean;
    procedure ValidateSourceConfiguration();
    function ExtractInputParameters(): string;
    procedure ProcessReadmeMarkdown(const aNewVersion: string);
    procedure ProcessSourcePasFiles(const aNewVersion: string);
    procedure ProcessOnePasFile(const aPath: string; const aNewVersion: string);
    procedure WriteProcessErrorAndHalt(const AErrorMsg: string);
  public
    constructor Create();
    destructor Destroy; override;
    procedure ExecuteApplication();
    class procedure Run;
  end;

implementation

uses
  Processor.Utils,
  Processor.PascalUnit,
  Processor.ReadmeMarkdown;

constructor TMainApplication.Create();
begin
  fAppConfig := TAppConfiguration.Create;
  fAppConfig.LoadFromFile;
  fSilentMode := true;
end;

destructor TMainApplication.Destroy;
begin
  fAppConfig.Free;
  inherited;
end;

procedure TMainApplication.ValidateSourceConfiguration();
var
  aIsError: boolean;
  aSourceDir: string;
  aSourceUnit: string;
begin
  aIsError := False;
  for aSourceUnit in fAppConfig.SourceUnits do
  begin
    aSourceDir := ExtractFileDir(aSourceUnit);
    if not DirectoryExists(aSourceDir) then
    begin
      writeln(Format
          ('Configured source directory [%s] didnt exists. Please update configuration!',
          [aSourceDir]));
      aIsError := true;
    end;
  end;
  if aIsError then
    Halt(1);
end;

procedure TMainApplication.WriteProcessErrorAndHalt(const AErrorMsg: string);
begin
  writeln('    [Error] Processing error!');
  writeln('    ' + AErrorMsg);
  Halt(3);
end;

procedure TMainApplication.ProcessReadmeMarkdown(const aNewVersion: string);
var
  aFilePath: string;
  aSourceText: string;
  aNewSource: string;
begin
  aFilePath := fAppConfig.ReadmeFilePath;
  aSourceText := TFile.ReadAllText(aFilePath, TEncoding.UTF8);
  try
    aNewSource := TReadmeMarkdownProcessor.ProcessReadme(aSourceText, aNewVersion,
        fAppConfig.ReadmeSearchPattern);
  except
    on E: Processor.Utils.EProcessError do
      WriteProcessErrorAndHalt(E.Message);
  end;
  TFile.WriteAllText(aFilePath, aNewSource, TEncoding.UTF8);
  writeln('   - bumped readme version to: '+aNewVersion);
end;

procedure TMainApplication.ProcessSourcePasFiles(const aNewVersion: string);
var
  aSourcePath: string;
  aSourceDir: string;
  aSourcePattern: string;
  aFiles: TArray<string>;
  aPath: string;
begin
  for aSourcePath in fAppConfig.SourceUnits do
  begin
    if FileExists(aSourcePath) then
    begin
      ProcessOnePasFile(aSourcePath, aNewVersion)
    end
    else
    begin
      aSourceDir := ExtractFileDir(aSourcePath);
      aSourcePattern := ExtractFileName(aSourcePath);
      aFiles := TDirectory.GetFiles(aSourceDir, aSourcePattern);
      for aPath in aFiles do
      begin
        ProcessOnePasFile(aPath, aNewVersion);
      end;
    end;
  end;
end;

procedure TMainApplication.ProcessOnePasFile(const aPath: string; const aNewVersion: string);
var
  aSourceText: string;
  aOldVersion: string;
  aNewSource: string;
begin
  aSourceText := TFile.ReadAllText(aPath, TEncoding.UTF8);
  try
    aNewSource := TPascalUnitProcessor.ProcessUnit(aSourceText, aNewVersion);
    aOldVersion := TPascalUnitProcessor.OldVersion;
  except
    on E: Processor.Utils.EProcessError do
      WriteProcessErrorAndHalt(E.Message);
  end;
  if aSourceText <> aNewSource then
  begin
    TFile.WriteAllText(aPath, aNewSource, TEncoding.UTF8);
    writeln(Format('   - %s  -  %s => %s', [aPath, aOldVersion, aNewVersion]));
  end;
end;

procedure TMainApplication.ExecuteApplication();
var
  aNewVersion: string;
begin
  ValidateSourceConfiguration;
  aNewVersion := ExtractInputParameters;
  if fAppConfig.DoReadmeBump then
    ProcessReadmeMarkdown(aNewVersion);
  ProcessSourcePasFiles(aNewVersion);
  if fSilentMode = False then
  begin
    writeln('');
    write('All files was updated. Press [Enter] to close application ...');
    readln;
  end;
end;

function TMainApplication.ExtractInputParameters: string;
var
  version: string;
begin
  if ParamCount = 0 then
  begin
    fSilentMode := False;
    Write('New version: ');
    readln(version);
    if Trim(version) = '' then
      Halt(2);
    writeln('');
  end
  else
    version := ParamStr(1);
  Result := version;
end;

class procedure TMainApplication.Run;
var
  App: TMainApplication;
begin
  App := TMainApplication.Create;
  try
    App.ExecuteApplication;
  finally
    App.Free;
  end;
end;

end.
