{ * ------------------------------------------------------------------------
  * ♥
  * ♥ VCL Command component/class with a factory
  * ♥
  * Components:     TCommand, TCommandAction
  * Classes:        TCommandVclFactory
  * Project:        https://github.com/bogdanpolak/command-delphi
  * Documentation:  on the github site
  * ReleaseDate:    ↓ see Signature below ↓
  * ReleaseVersion: ↓ see Signature below ↓
  * ------------------------------------------------------------------------ }
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
  const
    // * --------------------------------------------------------------------
    // * Signature
    ReleaseDate = '2019.08.28';
    ReleaseVersion = '0.2';
    // * --------------------------------------------------------------------
  strict private
    // FReceiver: TReceiver;
  strict protected
    // procedure Guard; - assert injections of all required properties
    procedure Guard; virtual; abstract;
  public
    procedure Execute; virtual;
    // call receiver method(s) or just do the job (merged command)
    // property Receiver: TReceiver read FReceiver set FReceiver;
  end;

  TCommandVclFactory = class(TComponent)
  private
    class procedure InjectProperties(ACommand: TCommand;
      const Injections: array of const);
  public
    class function CreateCommand<T: TCommand>(AOwner: TComponent;
      const Injections: array of const): T;
    class procedure ExecuteCommand<T: TCommand>(const Injections
      : array of const);
    class function CreateCommandAction<T: TCommand>(AOwner: TComponent;
      const ACaption: string; const Injections: array of const): TAction;
  end;

  TCommandAction = class(TAction)
  strict private
    FCommand: TCommand;
    procedure OnExecuteEvent(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    property Command: TCommand read FCommand write FCommand;
  end;

implementation

// ------------------------------------------------------------------------
{ TCommand }

procedure TCommand.Execute;
begin
  Guard;
end;

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

class function TCommandVclFactory.CreateCommand<T>(AOwner: TComponent;
  const Injections: array of const): T;
begin
  Result := T.Create(AOwner);
  InjectProperties(Result, Injections);
end;

class procedure TCommandVclFactory.ExecuteCommand<T>(const Injections
  : array of const);
var
  Command: T;
begin
  try
    Command := T.Create(nil);
    InjectProperties(Command, Injections);
    Command.Execute;
  finally
    Command.Free;
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
