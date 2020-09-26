unit AppConfiguration;

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.IOUtils,
  System.Generics.Collections;

type
  TAppConfiguration = class
  private const
    KeySourceUnits = 'sourceUnits';
    KeyReadmeSection = 'bumpReadme';
    KeyReadmeFilePath = 'fileName';
    KeyReadmeSearchPattern = 'versionPrefix';
  private
    FSourceUnits: TList<string>;
    FDoReadmeBump: boolean;
    FReadmeFilePath: string;
    FReadmeSearchPattern: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile;
    property SourceUnits: TList<string> read FSourceUnits write FSourceUnits;
    property DoReadmeBump: boolean read FDoReadmeBump write FDoReadmeBump;
    property ReadmeFilePath: string read FReadmeFilePath write FReadmeFilePath;
    property ReadmeSearchPattern: string read FReadmeSearchPattern
        write FReadmeSearchPattern;
  end;

implementation

constructor TAppConfiguration.Create;
begin
  FSourceUnits := TList<string>.Create;
end;

destructor TAppConfiguration.Destroy;
begin
  FSourceUnits.Free;
  inherited;
end;

procedure TAppConfiguration.LoadFromFile;
var
  aJsonData: string;
  jsObject: TJSONObject;
  jsTrue: TJSONTrue;
  jsValueSourceUnits: TJSONValue;
  jsSourceUnitsArray: TJSONArray;
  aSourcePath: string;
  i: integer;
  jsReadmeBump: TJSONObject;
begin
  aJsonData := TFile.ReadAllText('app-config.json');
  jsObject := TJSONObject.ParseJSONValue(aJsonData) as TJSONObject;
  jsTrue := TJSONTrue.Create;
  try
    // --- PAS Source ----
    jsValueSourceUnits := jsObject.GetValue(KeySourceUnits);
    if jsValueSourceUnits=nil then
    begin
      writeln(Format('Error! Mandatory configuration item: "%s" does not exist.',
          [KeySourceUnits]));
      Halt(2);
    end;
    if not(jsValueSourceUnits is TJSONArray) then
    begin
      writeln(Format('Error! Configuration item: "%s" is not array of strings',
          [KeySourceUnits]));
      Halt(2);
    end;
    jsSourceUnitsArray := jsValueSourceUnits as TJSONArray;
    FSourceUnits.Clear;
    for i := 0 to jsSourceUnitsArray.Count - 1 do
    begin
      aSourcePath := jsSourceUnitsArray.Items[i].Value;
      FSourceUnits.Add(aSourcePath);
    end;
    // --- README ----

    DoReadmeBump := (jsObject.GetValue(KeyReadmeSection) <> nil);
    if DoReadmeBump then
    begin
      jsReadmeBump := jsObject.GetValue(KeyReadmeSection) as TJSONObject;
      ReadmeFilePath := jsReadmeBump.GetValue(KeyReadmeFilePath).Value;
      ReadmeSearchPattern := jsReadmeBump.GetValue(KeyReadmeSearchPattern).Value;
    end;
  finally
    jsObject.Free;
    jsTrue.Free;
  end;
end;

end.
