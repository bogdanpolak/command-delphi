unit Vcl.Pattern.Command;

interface

uses
  System.Classes, System.SysUtils, System.Actions,
  Vcl.ActnList;

type
  ICommand = interface
    procedure Execute();
  end;

  TCommand = class(TComponent, ICommand)
  strict private
    // FReceiver: TReceiver;
    // procedure Guard; - before execute, assert the injection of all required propertied (Reciver, etc)
  public
    procedure Execute; virtual; abstract;
    // call receiver method(s) or just do the job (merged command)
    // property Receiver: TReceiver read FReceiver set FReceiver;
  end;

  TCommandVclFactory = class(TComponent)
  public
    class function CreateAction<T: TCommand>(AOwner: TComponent;
      const ACaption: string): TAction;
  end;

  TCommandAction = class(TAction)
  strict private
    FCommand: TCommand;
    procedure OnExecuteEvent(Sender: TObject);
  published
    constructor Create(AOwner: TComponent); override;
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
  FCommand.Execute;
end;

// ------------------------------------------------------------------------
{ TActionFactory }

class function TCommandVclFactory.CreateAction<T>(AOwner: TComponent;
  const ACaption: string): TAction;
var
  Command: TCommand;
  act: TCommandAction;
begin
  act := TCommandAction.Create(AOwner);
  with act do
  begin
    Command := T.Create(act);
    Caption := ACaption;
  end;
  Result := act;
end;

// ------------------------------------------------------------------------

end.
