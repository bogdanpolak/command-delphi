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
  strict private
    FCommand: TCommand;
    procedure OnExecuteEvent(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    function SetupCaption(const ACaption: string): TCommandAction;
    function SetupCommand(ACommand: TCommand): TCommandAction;
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
  Assert(Command <> nil);
  FCommand.ExecuteCommand;
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

end.
