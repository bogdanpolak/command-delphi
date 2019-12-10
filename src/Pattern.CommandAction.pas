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
    fDisableDuringExecution: boolean;
    procedure OnExecuteEvent(Sender: TObject);
    procedure OnUpdateEvent(Sender: TObject);
  public
    constructor Create(aOwner: TComponent); override;
    function SetupCaption(const aCaption: string): TCommandAction;
    function SetupCommand(aCommand: TCommand): TCommandAction;
    function SetupShortCut(aShorcut: TShortCut): TCommandAction;
    function SetupEventOnUpdate(AUpdateProc: TProc<TCommandAction>)
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
  Self.OnExecute := OnExecuteEvent;
end;

procedure TCommandAction.OnExecuteEvent(Sender: TObject);
begin
  System.Assert(Command <> nil);
  if DisableDuringExecution then
  begin
    try
      Self.Enabled := False;
      Command.Execute;
    finally
      Self.Enabled := True;
    end;
  end
  else
    Command.Execute;
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
