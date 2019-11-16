{* ------------------------------------------------------------------------
 * ♥
 * ♥ VCL Command component/class with a factory
 * ♥
 * Components:     TCommand, TCommandAction
 * Classes:        TCommandVclFactory
 * Project:        https://github.com/bogdanpolak/command-delphi
 * Documentation:  on the github site
 * ReleaseDate:    ↓ see Signature below ↓
 * ReleaseVersion: ↓ see Signature below ↓
 * ------------------------------------------------------------------------}
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
    ReleaseDate = '2019.11.16';
    ReleaseVersion = '0.4';
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

  TPropertyArray = array of TPropertyInfo;

  TComponentMetadata = class
  public
    class function GetPublishedPropetries(aComponent: TComponent)
      : TPropertyArray;
  end;

  TComponentInjector = class
    class procedure InjectProperties(aComponent: TComponent;
      const Injections: array of const);
  private
    class procedure AssertParameters(const Injections: array of const); static;
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

const
  ERRMSG_NotSupportedInjection = 'Not supported injection type!' +
    ' This property %s: %s in not supported in command pattern.' +
    ' Move this propoerty into [public] section';
  ERRMSG_NotSupportedParameter = 'Not supported parameter type to inject!' +
    'Parameter index (zaro-based): %d. Paramter type: %s';

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
// TComponentInjector
// ------------------------------------------------------------------------

function TypeKindToStr(value: TTypeKind): string;
begin
  Result := System.TypInfo.GetEnumName(TypeInfo(TTypeKind), Integer(value));
end;

function VTypeToStr(value: byte): string;
begin
  case value of
{$REGION 'case VType values 0 .. 17, vtInteger = 0, vtBoolean = 1, ...'}
    0:
      Result := 'vtInteger';
    1:
      Result := 'vtBoolean';
    2:
      Result := 'vtChar';
    3:
      Result := 'vtExtended';
    4:
      Result := 'vtString';
    5:
      Result := 'vtPointer';
    6:
      Result := 'vtPChar';
    7:
      Result := 'vtObject';
    8:
      Result := 'vtClass';
    9:
      Result := 'vtWideChar';
    10:
      Result := 'vtPWideChar';
    11:
      Result := 'vtAnsiString';
    12:
      Result := 'vtCurrency';
    13:
      Result := 'vtVariant';
    14:
      Result := 'vtInterface';
    15:
      Result := 'vtWideString';
    16:
      Result := 'vtInt64';
    17:
      Result := 'vtUnicodeString';
{$ENDREGION}
  end;
end;

class procedure TComponentInjector.AssertParameters(const Injections
  : array of const);
var
  j: Integer;
begin
  for j := 0 to High(Injections) do
    if not(Injections[j].VType in [vtObject, vtInteger, vtBoolean, vtExtended])
    then
      Assert(False, Format(ERRMSG_NotSupportedParameter,
        [j, VTypeToStr(Injections[j].VType)]));
end;

class procedure TComponentInjector.InjectProperties(aComponent: TComponent;
  const Injections: array of const);
var
  i: Integer;
  j: Integer;
  PropertyList: TPropertyArray;
  propInfo: TPropertyInfo;
  UsedInjection: TArray<boolean>;
begin
  // Inject dependencies to the command.
  // Limitations of this version:
  // * only TObject and descendants injections is supported
  // * important is properties order in the command class and
  // ..  in the Injections array (should be the same)
  // --------------------------------
  AssertParameters(Injections);
  PropertyList := TComponentMetadata.GetPublishedPropetries(aComponent);
  SetLength(UsedInjection, Length(Injections));
  for i := 0 to High(PropertyList) do
  begin
    propInfo := PropertyList[i];
    // propInfo.Kind:
    // tkInteger, tkChar, tkEnumeration, tkFloat, tkString, tkSet,
    // tkWChar, tkLString, tkWString, tkVariant, tkArray, tkRecord,
    // tkInterface, tkInt64, tkDynArray, tkUString
    if propInfo.Kind = tkClass then
    begin
      for j := 0 to High(Injections) do
        if not(UsedInjection[j]) then
        begin
          if (Injections[j].VType = vtObject) then
          begin
            if Injections[j].VObject.ClassName = propInfo.ClassName then
            begin
              SetObjectProp(aComponent, propInfo.PropertyName,
                Injections[j].VObject);
              UsedInjection[j] := True;
              Break;
            end;
          end
        end;
    end
    else if (propInfo.Kind = tkInteger) or (propInfo.Kind = tkEnumeration) or
      (propInfo.Kind = tkFloat) then
    begin
      for j := 0 to High(Injections) do
        if not(UsedInjection[j]) then
        begin
          if (propInfo.Kind = tkInteger) and (Injections[j].VType = vtInteger) then
          begin
            SetOrdProp(aComponent, propInfo.PropertyName,
              Injections[j].VInteger);
            UsedInjection[j] := True;
            Break;
          end
          else if (propInfo.Kind = tkEnumeration) and (Injections[j].VType = vtBoolean) then
          begin
            SetOrdProp(aComponent, propInfo.PropertyName,
              Injections[j].VInteger);
            UsedInjection[j] := True;
            Break;
          end
          else if (propInfo.Kind = tkFloat) and (Injections[j].VType = vtExtended) then
          begin
            SetFloatProp(aComponent, propInfo.PropertyName,
              Injections[j].VExtended^);
            UsedInjection[j] := True;
            Break;
          end;
        end;
    end
    else
      Assert(False, Format(ERRMSG_NotSupportedInjection, [propInfo.PropertyName,
        TypeKindToStr(propInfo.Kind)]));
  end;
end;

// ------------------------------------------------------------------------
// TCommandVclFactory
// ------------------------------------------------------------------------

class procedure TCommandVclFactory.InjectProperties(ACommand: TCommand;
  const Injections: array of const);
begin
  TComponentInjector.InjectProperties(ACommand, Injections);
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

class function TComponentMetadata.GetPublishedPropetries(aComponent: TComponent)
  : TPropertyArray;
var
  aStandardComponent: TComponent;
  FPropList: PPropList;
  FStandardPropList: PPropList;
  aStandardCount: Integer;
  aCount: Integer;
  i: Integer;
begin
  aCount := System.TypInfo.GetPropList(aComponent, FPropList);
  aStandardComponent := TComponent.Create(nil);
  aStandardCount := System.TypInfo.GetPropList(aStandardComponent,
    FStandardPropList);
  try
    SetLength(Result, aCount - aStandardCount);
    for i := 0 to aCount - aStandardCount - 1 do
    begin
      Result[i].Kind := FPropList^[aStandardCount + i].PropType^.Kind;
      Result[i].PropertyName := string(FPropList^[aStandardCount + i].Name);
      Result[i].ClassName :=
        string(FPropList^[aStandardCount + i].PropType^.Name);
    end;
  finally
    FreeMem(FPropList);
    aStandardComponent.Free;
    FreeMem(FStandardPropList);
  end;
end;

end.
