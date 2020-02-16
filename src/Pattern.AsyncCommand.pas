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
    Version = '1.0';
  private
    fUpdateInterval: integer;
    fOnUpdateProc: TProc;
    procedure OnUpdateTimer(Sender: TObject);
  protected
    fBeforeStartEvent: TProc;
    fAfterFinishEvent: TProc;
    fThread: TThread;
    fIsCommandDone: boolean;
    fTimer: TTimer;
    procedure Synchronize(aProc: TThreadProcedure);
    function GetIsCommandDone: boolean;
    procedure SetIsCommandDone(aIsTermianted: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function WithInjections(const Injections: array of const): TAsyncCommand;
    function WithEventBeforeStart(aBeforeStart: TProc): TAsyncCommand;
    function WithEventAfterFinish(aAfterFinish: TProc): TAsyncCommand;
    function WithEventOnUpdate(aOnUpdateProc: TProc): TAsyncCommand;
    procedure Execute; override;
    procedure Terminate;
    function IsBusy: boolean; override;
    function IsTerminated: boolean;
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
  fIsCommandDone := true;
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
  SetIsCommandDone(false);
  DoGuard;
  fThread := TThread.CreateAnonymousThread(
    procedure
    begin
      TThread.NameThreadForDebugging('Command: ' + Self.ClassName);
      try
        SetIsCommandDone(false);
        DoExecute;
      finally
        SetIsCommandDone(true);
      end;
    end);
  fThread.FreeOnTerminate := false;
  fTimer.Enabled := True;
  if Assigned(fBeforeStartEvent) then
    fBeforeStartEvent();
  fStopwatch := TStopwatch.StartNew;
  fThread.Start;
end;

function TAsyncCommand.GetIsCommandDone: boolean;
begin
  TMonitor.Enter(Self);
  try
    Result := fIsCommandDone;
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TAsyncCommand.SetIsCommandDone(aIsTermianted: boolean);
begin
  TMonitor.Enter(Self);
  try
    fIsCommandDone := aIsTermianted;
  finally
    TMonitor.Exit(Self);
  end;
end;

function TAsyncCommand.IsBusy: boolean;
var
  IsCommandDone: boolean;
begin
  IsCommandDone := GetIsCommandDone;
  if IsCommandDone and (fThread <> nil) then
  begin
    fTimer.Enabled := False;
    FreeAndNil (fThread);
    fStopwatch.Stop;
    if Assigned(fAfterFinishEvent) then
      fAfterFinishEvent();
  end;
  Result := not IsCommandDone;
end;

function TAsyncCommand.IsTerminated: boolean;
begin
  Result := TThread.CheckTerminated;
end;

procedure TAsyncCommand.OnUpdateTimer(Sender: TObject);
begin
  fTimer.Enabled := Self.IsBusy;
  if fTimer.Enabled and Assigned(fOnUpdateProc) then
    fOnUpdateProc;
end;

procedure TAsyncCommand.Synchronize(aProc: TThreadProcedure);
begin
  if (fThread <> nil) and Assigned(aProc) then
    TThread.Synchronize(fThread, aProc);
end;

procedure TAsyncCommand.Terminate;
begin
  if (fThread<>nil) and not GetIsCommandDone then
    fThread.Terminate;
end;

function TAsyncCommand.WithInjections(const Injections: array of const): TAsyncCommand;
begin
  TComponentInjector.InjectProperties(Self, Injections);
  Result := Self;
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
