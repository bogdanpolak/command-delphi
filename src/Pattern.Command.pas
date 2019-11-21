{* ------------------------------------------------------------------------
 * ♥
 * ♥ Command Parttern
 * ♥
 * Components:     TCommand
 * Project:        https://github.com/bogdanpolak/command-delphi
 * ------------------------------------------------------------------------}
unit Pattern.Command;

interface

uses
  System.Classes,
  System.SysUtils,
  System.TypInfo;

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
  protected
    // procedure Guard; - assert injections of all required properties
    procedure Guard; virtual;
    procedure Execute; virtual; abstract;
  public
    class procedure AdhocExecute<T: TCommand>(const Injections
      : array of const); static;
    function Inject(const Injections: array of const): TCommand;
    procedure ExecuteCommand;
  end;

  TPropertyInfo = record
    Kind: TTypeKind;
    PropertyName: string;
    ClassName: string;
    function isAvaliableForInjection(const aInjection: TVarRec): boolean;
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

type
  TCommandClass = class of TCommand;

implementation

const
  ERRMSG_NotSupportedParameter = 'Not supported parameter type to inject!' +
    'Parameter index (zaro-based): %d. Paramter type: %s';

procedure __for_code_formatter;
begin
end;

// ------------------------------------------------------------------------
// TCommand
// ------------------------------------------------------------------------

procedure TCommand.ExecuteCommand;
begin
  Guard;
  Execute;
end;

procedure TCommand.Guard;
begin
  raise EAbort.Create('Define Guard method for the child Command class. Do not call `inherited` in Guard method.');
end;

function TCommand.Inject(const Injections: array of const): TCommand;
begin
  TComponentInjector.InjectProperties(Self, Injections);
  Result := Self;
end;

class procedure TCommand.AdhocExecute<T>(const Injections: array of const);
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
    TComponentInjector.InjectProperties(Command, Injections);
    Command.ExecuteCommand;
  finally
    Command.Free;
  end;
end;


// ------------------------------------------------------------------------
// TComponentInjector
// ------------------------------------------------------------------------

{ TPropertyInfo }

function TPropertyInfo.isAvaliableForInjection(const aInjection
  : TVarRec): boolean;
var
  classType: TClass;
begin
  if (Self.Kind = tkClass) and (aInjection.VType = vtObject) then
  begin
    Result := (aInjection.VObject.ClassName = Self.ClassName);
    classType := aInjection.VObject.classType;
    while not(Result) and (classType.ClassParent <> nil) do
    begin
      Result := (classType.ClassParent.ClassName = Self.ClassName);
      classType := classType.ClassParent;
    end;
  end
  else
    Result := (Self.Kind = tkInteger) and (aInjection.VType = vtInteger) or
      (Self.Kind = tkEnumeration) and (aInjection.VType = vtBoolean) or
      (Self.Kind = tkFloat) and (aInjection.VType = vtExtended);
end;

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

// - - - - - - - - - - - - - - - - - - - - - - - - -
// TPropertyInfo.Kind: tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
// tkSet, tkWChar, tkLString, tkWString, tkVariant, tkArray, tkRecord,
// tkInterface, tkInt64, tkDynArray, tkUString
// - - - - - - - - - - - - - - - - - - - - - - - - -
class procedure TComponentInjector.InjectProperties(aComponent: TComponent;
  const Injections: array of const);
var
  i: Integer;
  j: Integer;
  PropertyList: TPropertyArray;
  propInfo: TPropertyInfo;
  UsedInjection: TArray<boolean>;
begin
  AssertParameters(Injections);
  PropertyList := TComponentMetadata.GetPublishedPropetries(aComponent);
  SetLength(UsedInjection, Length(Injections));
  for i := 0 to High(PropertyList) do
  begin
    propInfo := PropertyList[i];
    for j := 0 to High(Injections) do
    begin
      if not(UsedInjection[j]) and propInfo.isAvaliableForInjection
        (Injections[j]) then
      begin
        UsedInjection[j] := True;
        case propInfo.Kind of
          tkClass:
            SetObjectProp(aComponent, propInfo.PropertyName,
              Injections[j].VObject);
          tkInteger, tkEnumeration:
            SetOrdProp(aComponent, propInfo.PropertyName,
              Injections[j].VInteger);
          tkFloat:
            SetFloatProp(aComponent, propInfo.PropertyName,
              Injections[j].VExtended^);
        end;
        Break;
      end;
    end
  end;
end;


// ------------------------------------------------------------------------
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
