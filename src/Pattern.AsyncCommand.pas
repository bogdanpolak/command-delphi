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
    fBeforeStartEvent: TProc;
    fAfterFinishEvent: TProc;
    fThread: TThread;
    fIsThreadTermianed: boolean;
    procedure Synchronize(aProc: TThreadProcedure);
  public
    constructor Create(AOwner: TComponent); override;
    function WithEventBeforeStart(aBeforeStart: TProc): TAsyncCommand;
    function WithEventAfterFinish(aAfterFinish: TProc): TAsyncCommand;
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
  fBeforeStartEvent := nil;
  fAfterFinishEvent := nil;
  fIsThreadTermianed := true;
end;

procedure TAsyncCommand.Execute;
begin
  DoGuard;
  if Assigned(fBeforeStartEvent) then
    fBeforeStartEvent();
  fThread := TThread.CreateAnonymousThread(
    procedure
    begin
      TThread.NameThreadForDebugging('Command: ' + Self.ClassName);
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
    if Assigned(fAfterFinishEvent) then
      fAfterFinishEvent();
  end;
end;

procedure TAsyncCommand.Synchronize(aProc: TThreadProcedure);
begin
  if (fThread <> nil) and Assigned(aProc) then
    TThread.Synchronize(fThread, aProc);
end;

function TAsyncCommand.WithEventAfterFinish(aAfterFinish: TProc): TAsyncCommand;
begin
  fAfterFinishEvent := aAfterFinish;
  Result := Self;
end;

function TAsyncCommand.WithEventBeforeStart(aBeforeStart: TProc): TAsyncCommand;
begin
  fBeforeStartEvent := aBeforeStart;
  Result := Self;
end;

end.
