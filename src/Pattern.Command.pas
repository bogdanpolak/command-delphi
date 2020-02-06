{* ------------------------------------------------------------------------ *
 * Command Parttern  ♥  TCommand
 * ------------------------------------------------------------------------ *}
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
  private const
    Version = '0.7';
  protected
    // procedure Guard; - assert injections of all required properties
    procedure DoGuard; virtual;
    procedure DoExecute; virtual; abstract;
  public
    class procedure AdhocExecute<T: TCommand>(const Injections
      : array of const); static;
    function Inject(const Injections: array of const): TCommand;
    procedure Execute; virtual;
  end;

  TAsyncCommand = class(TCommand)
  protected
    fThread: TThread;
    fIsThreadTermianed: boolean;
    procedure DoPrepare; virtual; abstract;
    procedure DoTeardown; virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Execute; override;
    function IsFinished: boolean;
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

uses
  System.RTTI;

// ------------------------------------------------------------------------
// TCommand
// ------------------------------------------------------------------------

const
  ERRMSG_NotSupportedParameter = 'Not supported parameter type to inject!' +
    'Parameter index (zaro-based): %d. Paramter type: %s';

procedure TCommand.Execute;
begin
  DoGuard;
  DoExecute;
end;

procedure TCommand.DoGuard;
begin
  raise EAbort.Create
    ('Define Guard method for the child Command class. Do not call `inherited` in Guard method.');
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
    Command.Execute;
  finally
    Command.Free;
  end;
end;


// ------------------------------------------------------------------------
// TAsyncCommand
// ------------------------------------------------------------------------

constructor TAsyncCommand.Create(AOwner: TComponent);
begin
  inherited;
  fThread := nil;
  fIsThreadTermianed := true;
end;

procedure TAsyncCommand.Execute;
begin
  DoGuard;
  DoPrepare;
  fThread := TThread.CreateAnonymousThread(
    procedure
    begin
      try
        fIsThreadTermianed := False;
        DoExecute;
      finally
        // TODO: lock or critical section is required bellow (critical !!!)
        fIsThreadTermianed := true;
      end;
    end);
  fThread.FreeOnTerminate := False;
  fThread.Start;
end;

function TAsyncCommand.IsFinished: boolean;
begin
  if fThread = nil then
    Exit(true);
  Result := fIsThreadTermianed;
  if Result and (fThread <> nil) then
  begin
    fThread.Free;
    fThread := nil;
    DoTeardown;
  end;
end;

// ------------------------------------------------------------------------
// TComponentInjector
// ------------------------------------------------------------------------

{ TPropertyInfo }

procedure SetInterfaceProperty(aComponent: TComponent;
const aPropertyName: string; const aInjection: TVarRec);
var
  ctx: TRttiContext;
  typ: TRttiType;
  prop: TRttiProperty;
  val: TValue;
begin
  typ := ctx.GetType(aComponent.ClassType);
  val := TValue.From(IInterface(aInjection.VInterface) as TObject);
  for prop in typ.GetProperties do
    if prop.Name = aPropertyName then
      prop.SetValue(aComponent, val);
end;

function IsInterfaceInjectionImplementsInterface(const aInjection: TVarRec;
const aInterfaceName: string): boolean;
var
  obj: TObject;
  implementedList: TArray<TRttiInterfaceType>;
  IntfType: TRttiInterfaceType;
  ctx: TRttiContext;
begin
  System.Assert(aInjection.VType = vtInterface);
  obj := IInterface(aInjection.VInterface) as TObject;
  implementedList := (ctx.GetType(obj.ClassType) as TRttiInstanceType)
    .GetImplementedInterfaces;
  for IntfType in implementedList do
    if IntfType.Name = aInterfaceName then
      Exit(true);
  Result := False;
end;

function TPropertyInfo.isAvaliableForInjection(const aInjection
  : TVarRec): boolean;
var
  ClassType: TClass;
begin
  if (Self.Kind = tkInterface) and (aInjection.VType = vtInterface) then
    Result := IsInterfaceInjectionImplementsInterface(aInjection,
      Self.ClassName)
  else if (Self.Kind = tkClass) and (aInjection.VType = vtObject) then
  begin
    Result := (aInjection.VObject.ClassName = Self.ClassName);
    ClassType := aInjection.VObject.ClassType;
    while not(Result) and (ClassType.ClassParent <> nil) do
    begin
      Result := (ClassType.ClassParent.ClassName = Self.ClassName);
      ClassType := ClassType.ClassParent;
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
    if not(Injections[j].VType in [vtObject, vtInterface, vtInteger, vtBoolean,
      vtExtended]) then
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
        UsedInjection[j] := true;
        case propInfo.Kind of
          tkInterface:
            SetInterfaceProperty(aComponent, propInfo.PropertyName,
              Injections[j]);
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
