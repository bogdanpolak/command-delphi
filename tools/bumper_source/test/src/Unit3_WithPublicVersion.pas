unit Unit3_WithPublicVersion;

interface

uses
  System.Classes,
  System.SysUtils;

type
  ICommand = interface
    procedure Execute();
  end;

  TFooInfo = record
    Kind: TTypeKind;
    PropertyName: string;
    ClassName: string;
    function isAvaliableForInjection(const aInjection: TVarRec): boolean;
  end;

type
  TFoo3 = class(TComponent)
  public
    function GetElapsedTimeMs: integer;
    function IsBusy: boolean; virtual;
  public const
    Version = '1.0';
  end;

implementation

uses
  System.RTTI;

const
  ERRMSG_NotSupportedParameter = 'Not supported parameter type to inject!' +
    'Parameter index (zaro-based): %d. Paramter type: %s';

function TFoo3.GetElapsedTimeMs: integer;
begin
  Result := 0;
end;

function TFoo3.IsBusy: TTimeSpan;
begin
  Result := False;
end;

end.
