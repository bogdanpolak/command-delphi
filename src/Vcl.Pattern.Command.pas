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
    ReleaseDate = '2019.09.01';
    ReleaseVersion = '0.3.1';
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

  TPropertyInfo = record
    Kind: TTypeKind;
    PropertyName: string;
    ClassName: string;
  end;

  TClassPropertyList = class
  private
    FPropList: PPropList;
    FCount: Integer;
  public
    constructor Create(c: TComponent);
    destructor Destroy; override;
    function Count: Integer;
    function GetInfo(AIndex: Integer): TPropertyInfo;
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

type
  TCommandClass = class of TCommand;

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
  i: Integer;
  j: Integer;
  PropertyList: TClassPropertyList;
  propInfo: TPropertyInfo;
  UsedInjection: TArray<boolean>;
begin
  // Inject dependencies to the command.
  // Limitations of this version:
  // * only TObject and descendants injections is supported
  // * important is properties order in the command class and
  // ..  in the Injections array (should be the same)
  // --------------------------------
  PropertyList := TClassPropertyList.Create(ACommand);
  SetLength(UsedInjection, Length(Injections));
  try
    for i := 0 to PropertyList.Count - 1 do
    begin
      propInfo := PropertyList.GetInfo(i);
      if propInfo.Kind = tkClass then
      begin
        for j := 0 to High(Injections) do
          if not(UsedInjection[j]) then
          begin
            if (Injections[j].VType = vtObject) then
            begin
              if Injections[j].VObject.ClassName = propInfo.ClassName then
              begin
                SetObjectProp(ACommand, propInfo.PropertyName,
                  Injections[j].VObject);
                UsedInjection[j] := True;
                Break;
              end;
            end
            else
              Assert(False,
                'Not supported yet! Only objects can be injected to a command');
          end;
      end;
    end;
  finally
    PropertyList.Free;
  end;
end;

class function TCommandVclFactory.CreateCommand<T>(AOwner: TComponent;
  const Injections: array of const): T;
var
  AClass: TCommandClass;
begin
  // -----------------------------------------
  AClass := T;
  Result := T(AClass.Create(AOwner));
  // 10.3 Rio: just one line: Result := T.Create(AOwner);
  // -----------------------------------------
  InjectProperties(Result, Injections);
end;

class procedure TCommandVclFactory.ExecuteCommand<T>(const Injections
  : array of const);
var
  AClass: TCommandClass;
  Command: T;
begin
  try
    // -----------------------------------------
    AClass := T;
    Command := T(AClass.Create(nil));
    // 10.3 Rio: Command := T.Create(nil);
    // -----------------------------------------
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
  AClass: TCommandClass;
begin
  act := TCommandAction.Create(AOwner);
  with act do
  begin
    // -----------------------------------------
    AClass := (T);
    Command := T(AClass.Create(act));
    // 10.3 Rio: Command := T.Create(act);
    // -----------------------------------------
    Caption := ACaption;
  end;
  InjectProperties(act.Command, Injections);
  Result := act;
end;

// ------------------------------------------------------------------------

{ TClassPropertyList }

constructor TClassPropertyList.Create(c: TComponent);
begin
  FCount := System.TypInfo.GetPropList(c, FPropList);
end;

destructor TClassPropertyList.Destroy;
begin
  FreeMem(FPropList);
  inherited;
end;

function TClassPropertyList.Count: Integer;
begin
  Result := FCount;
end;

function TClassPropertyList.GetInfo(AIndex: Integer): TPropertyInfo;
begin
  System.Assert(AIndex < FCount);
  Result.Kind := FPropList^[AIndex].PropType^.Kind;
  Result.PropertyName := string(FPropList^[AIndex].Name);
  Result.ClassName := string(FPropList^[AIndex].PropType^.Name);
end;

end.
