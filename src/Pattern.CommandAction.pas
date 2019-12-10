unit Pattern.CommandAction;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Actions,
  Vcl.ActnList,
  Pattern.Command;

type
  TCommandAction = class(TAction)
  private
    fCommand: TCommand;
    fOnUpdateProc: TProc<TCommandAction>;
    fOnAfterProc: TProc<TCommandAction>;
    fDisableDuringExecution: boolean;
    procedure OnExecuteEvent(Sender: TObject);
    procedure OnUpdateEvent(Sender: TObject);
    procedure DoExecuteAction(Sender: TObject);
  public
    constructor Create(aOwner: TComponent); override;
    function SetupCaption(const aCaption: string): TCommandAction;
    function SetupCommand(aCommand: TCommand): TCommandAction;
    function SetupShortCut(aShorcut: TShortCut): TCommandAction;
    function SetupEventOnUpdate(AUpdateProc: TProc<TCommandAction>)
      : TCommandAction;
    function SetupEventAfterExecution(aAfterProc: TProc<TCommandAction>)
      : TCommandAction;
    property Command: TCommand read fCommand write fCommand;
    property DisableDuringExecution: boolean read fDisableDuringExecution
      write fDisableDuringExecution;
  end;

implementation

// ------------------------------------------------------------------------
{ TCommandAction }

constructor TCommandAction.Create(aOwner: TComponent);
begin
  inherited;
  DisableDuringExecution := False;
  fOnAfterProc := nil;
  Self.OnExecute := OnExecuteEvent;
end;

procedure TCommandAction.DoExecuteAction(Sender: TObject);
begin
  Command.Execute;
  if Assigned(fOnAfterProc) then
    fOnAfterProc(Self)
end;

procedure TCommandAction.OnExecuteEvent(Sender: TObject);
begin
  System.Assert(Command <> nil);
  if DisableDuringExecution then
  begin
    try
      Self.Enabled := False;
      DoExecuteAction(Sender);
    finally
      Self.Enabled := True;
    end;
  end
  else
    DoExecuteAction(Sender);
end;

procedure TCommandAction.OnUpdateEvent(Sender: TObject);
begin
  if Assigned(fOnUpdateProc) then
    fOnUpdateProc(Self);
end;

function TCommandAction.SetupCaption(const aCaption: string): TCommandAction;
begin
  Caption := aCaption;
  Result := Self;
end;

function TCommandAction.SetupCommand(aCommand: TCommand): TCommandAction;
begin
  Command := aCommand;
  Result := Self;
end;

function TCommandAction.SetupEventAfterExecution
  (aAfterProc: TProc<TCommandAction>): TCommandAction;
begin
  fOnAfterProc := aAfterProc;
  Result := Self;
end;

function TCommandAction.SetupEventOnUpdate(AUpdateProc: TProc<TCommandAction>)
  : TCommandAction;
begin
  fOnUpdateProc := AUpdateProc;
  Self.OnUpdate := OnUpdateEvent;
  Result := Self;
end;

function TCommandAction.SetupShortCut(aShorcut: TShortCut): TCommandAction;
begin
  Self.ShortCut := aShorcut;
  Result := Self;
end;

end.
