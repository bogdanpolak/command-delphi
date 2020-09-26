unit Unit2_NoVersion;

interface

uses
  System.Classes,
  System.SysUtils;

type
  TFoo = class(TComponent)
  public
    function GetElapsedTimeMs: integer;
    function IsBusy: boolean; virtual;
  end;

implementation

function TFoo.GetElapsedTimeMs: integer;
begin
  Result := 0;
end;

function TCommand.IsBusy: TTimeSpan;
begin
  Result := False;
end;

end.
