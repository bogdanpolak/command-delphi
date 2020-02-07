{* ------------------------------------------------------------------------ *
 * Command Parttern  ♥  TAsyncCommand
 * ------------------------------------------------------------------------ *}
unit Pattern.AsyncCommand;

interface

uses
  System.Classes,
  System.SysUtils,
  System.TypInfo,

  Pattern.Command;

type
  TAsyncCommand = class(TCommand)
  private const
    Version = '0.7';
  protected
    fThread: TThread;
    fIsThreadTermianed: boolean;
    procedure DoPrepare; virtual; abstract;
    procedure DoTeardown; virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Execute; override;
    function IsFinished: boolean;
  end;

implementation

// ------------------------------------------------------------------------
// TAsyncCommand
// ------------------------------------------------------------------------

constructor TAsyncCommand.Create(AOwner: TComponent);
begin
  inherited;
  fThread := nil;
  fIsThreadTermianed := true;
end;

procedure TAsyncCommand.Execute;
begin
  DoGuard;
  DoPrepare;
  fThread := TThread.CreateAnonymousThread(
    procedure
    begin
      try
        fIsThreadTermianed := False;
        DoExecute;
      finally
        TMonitor.Enter(Self);
        try
          fIsThreadTermianed := true;
        finally
          TMonitor.Exit(Self);
        end;
      end;
    end);
  fThread.FreeOnTerminate := False;
  fThread.Start;
end;

function TAsyncCommand.IsFinished: boolean;
begin
  if fThread = nil then
    Exit(true);
  // ---
  TMonitor.Enter(Self);
  try
    Result := fIsThreadTermianed;
  finally
    TMonitor.Exit(Self);
  end;
  // ---
  if Result and (fThread <> nil) then
  begin
    fThread.Free;
    fThread := nil;
    DoTeardown;
  end;
end;

end.
