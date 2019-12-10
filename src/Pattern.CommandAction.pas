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
    FCommand: TCommand;
    FOnUpdateProc: TProc<TCommandAction>;
    procedure OnExecuteEvent(Sender: TObject);
    procedure OnUpdateEvent(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    function SetupCaption(const ACaption: string): TCommandAction;
    function SetupCommand(ACommand: TCommand): TCommandAction;
    function SetupShortCut(AShorcut: TShortCut): TCommandAction;
    function SetupEventOnUpdate(AUpdateProc: TProc<TCommandAction>)
      : TCommandAction;
    property Command: TCommand read FCommand write FCommand;
  end;

implementation

// ------------------------------------------------------------------------
{ TCommandAction }

constructor TCommandAction.Create(AOwner: TComponent);
begin
  inherited;
  Self.OnExecute := OnExecuteEvent;
end;

procedure TCommandAction.OnExecuteEvent(Sender: TObject);
begin
  System.Assert(Command <> nil);
  Command.Execute;
end;

procedure TCommandAction.OnUpdateEvent(Sender: TObject);
begin
  if Assigned(FOnUpdateProc) then
    FOnUpdateProc(Self);
end;

function TCommandAction.SetupCaption(const ACaption: string): TCommandAction;
begin
  Caption := ACaption;
  Result := Self;
end;

function TCommandAction.SetupCommand(ACommand: TCommand): TCommandAction;
begin
  Command := ACommand;
  Result := Self;
end;

function TCommandAction.SetupEventOnUpdate(AUpdateProc: TProc<TCommandAction>)
  : TCommandAction;
begin
  FOnUpdateProc := AUpdateProc;
  Self.OnUpdate := OnUpdateEvent;
  Result := Self;
end;

function TCommandAction.SetupShortCut(AShorcut: TShortCut): TCommandAction;
begin
  Self.ShortCut := AShorcut;
  Result := Self;
end;

end.
