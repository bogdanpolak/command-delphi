unit Vcl.Pattern.Command;

interface

uses
  System.Classes, System.SysUtils, System.Actions, System.TypInfo,
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
  private
    class procedure InjectProperties(ACommand: TCommand;
      const Injections: array of const);
  public
    class function CreateCommandAction<T: TCommand>(AOwner: TComponent;
      const ACaption: string; const Injections: array of const): TAction;
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

class procedure TCommandVclFactory.InjectProperties(ACommand: TCommand;
  const Injections: array of const);
var
  PropList: PPropList;
  PropCount: Integer;
  i: Integer;
  j: Integer;
begin
  // Inject dependencies to the command.
  // Limitations of this version:
  // * only TObject and descendants injections is supported
  // * properties must have different types (ClassName)
  try
    PropCount := System.TypInfo.GetPropList(ACommand, PropList);
    for i := 0 to PropCount - 1 do
    begin
      if PropList^[i].PropType^.Kind = tkClass then
      begin
        // Do injection
        for j := 0 to High(Injections) do
          if Injections[j].VType = vtObject then
          begin
            // PropList^[i].PropType^.Name - ClassName of the property
            if Injections[j].VObject.ClassName = String
              (PropList^[i].PropType^.Name) then
              SetObjectProp(ACommand, String(PropList^[i].Name),
                Injections[j].VObject);
          end
          else
            Assert(False,
              'Not supported yet! Only objects can be injected to a command');
      end;
    end;
  finally
    FreeMem(PropList);
  end;
end;

class function TCommandVclFactory.CreateCommandAction<T>(AOwner: TComponent;
  const ACaption: string; const Injections: array of const): TAction;
var
  act: TCommandAction;
begin
  act := TCommandAction.Create(AOwner);
  with act do
  begin
    Command := T.Create(act);
    Caption := ACaption;
  end;
  InjectProperties(act.Command, Injections);
  Result := act;
end;

// ------------------------------------------------------------------------

end.
