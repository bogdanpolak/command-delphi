{* ------------------------------------------------------------------------ *
 * Command Parttern  ♥  TAsyncCommand
 * ------------------------------------------------------------------------ *}
unit Pattern.AsyncCommand;

interface

uses
  System.Classes,
  System.SysUtils,
  System.TypInfo,
  System.Diagnostics,
  System.TimeSpan,

  Vcl.ExtCtrls, // TTimer (VCL)

  Pattern.Command;

type
  TAsyncCommand = class(TCommand)
  private const
    Version = '0.7';
  private
    fUpdateInterval: integer;
    fOnUpdateProc: TProc;
    procedure OnUpdateTimer(Sender: TObject);
  protected
    fBeforeStartEvent: TProc;
    fAfterFinishEvent: TProc;
    fThread: TThread;
    fIsThreadTermianed: boolean;
    fTimer: TTimer;
    procedure Synchronize(aProc: TThreadProcedure);
    function GetIsThreadTerminated: boolean;
    procedure SetIsThreadTerminated(aIsTermianted: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function WithEventBeforeStart(aBeforeStart: TProc): TAsyncCommand;
    function WithEventAfterFinish(aAfterFinish: TProc): TAsyncCommand;
    function WithEventOnUpdate(aOnUpdateProc: TProc): TAsyncCommand;
    procedure Execute; override;
    function IsBusy: boolean; override;
    property UpdateInterval: integer read fUpdateInterval
      write fUpdateInterval;
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
  fUpdateInterval := 100;
  // --- Timer ---
  fTimer := TTimer.Create(nil);
  fTimer.Enabled := false;
  fTimer.Interval := fUpdateInterval;
  fTimer.OnTimer := OnUpdateTimer;
end;

destructor TAsyncCommand.Destroy;
begin
  Self.IsBusy; // call to tear down all internal structures
  fTimer.Free;
  inherited;
end;

procedure TAsyncCommand.Execute;
begin
  DoGuard;
  fThread := TThread.CreateAnonymousThread(
    procedure
    begin
      TThread.NameThreadForDebugging('Command: ' + Self.ClassName);
      try
        SetIsThreadTerminated(false);
        DoExecute;
      finally
        SetIsThreadTerminated(true);
      end;
    end);
  fThread.FreeOnTerminate := false;
  fTimer.Enabled := True;
  if Assigned(fBeforeStartEvent) then
    fBeforeStartEvent();
  fStopwatch := TStopwatch.StartNew;
  fThread.Start;
end;

function TAsyncCommand.GetIsThreadTerminated: boolean;
begin
  TMonitor.Enter(Self);
  try
    Result := fIsThreadTermianed;
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TAsyncCommand.SetIsThreadTerminated(aIsTermianted: boolean);
begin
  TMonitor.Enter(Self);
  try
    fIsThreadTermianed := aIsTermianted;
  finally
    TMonitor.Exit(Self);
  end;
end;

function TAsyncCommand.IsBusy: boolean;
var
  IsTerminatedFlag: boolean;
begin
  IsTerminatedFlag := GetIsThreadTerminated;
  if IsTerminatedFlag and (fThread <> nil) then
  begin
    fTimer.Enabled := False;
    FreeAndNil (fThread);
    fStopwatch.Stop;
    if Assigned(fAfterFinishEvent) then
      fAfterFinishEvent();
  end;
  Result := not IsTerminatedFlag;
end;

procedure TAsyncCommand.OnUpdateTimer(Sender: TObject);
begin
  fTimer.Enabled := Self.IsBusy;
  if Assigned(fOnUpdateProc) then
    fOnUpdateProc;
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

function TAsyncCommand.WithEventOnUpdate(aOnUpdateProc: TProc): TAsyncCommand;
begin
  fOnUpdateProc := aOnUpdateProc;
  Result := Self;
end;

end.
