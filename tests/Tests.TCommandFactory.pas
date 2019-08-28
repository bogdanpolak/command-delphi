unit Tests.TCommandFactory;

interface

uses
  DUnitX.TestFramework,
  System.Classes, System.SysUtils,
  Vcl.Pattern.Command;

{$TYPEINFO ON}  {Requred for old RTTI metadata form published section}

type
  [TestFixture]
  TCommandFactoryTests = class(TObject)
  strict private
    FStrings: TStringList;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    // -------------
    procedure TestAdhocExecuteCommand;
  end;

implementation

// ------------------------------------------------------------------------
// class TCommandA
// ------------------------------------------------------------------------
{$REGION 'class TCommandA'}
type
  TCommandA = class(TCommand)
  strict private
    FActive: boolean;
    FCount: integer;
  protected
    procedure Guard; override;
  public
    class var IsExecuted: boolean;
    procedure Execute; override;
    property Active: boolean read FActive write FActive;
    property Count: integer read FCount write FCount;
  end;

procedure TCommandA.Guard;
begin
end;

procedure TCommandA.Execute;
begin
  Active := True;
  IsExecuted := True;
  Count := Count + 1;
end;

{$ENDREGION}
// ------------------------------------------------------------------------
// class TCommandStringList
// ------------------------------------------------------------------------
{$REGION 'class TCommandStringList'}
type
  TCommandStringList = class(TCommand)
  strict private
    FCount: integer;
    FLines: TStringList;
  protected
    procedure Guard; override;
  public
    procedure Execute; override;
    property Count: integer read FCount write FCount;
    property Lines: TStringList read FLines write FLines;
  end;

procedure TCommandStringList.Guard;
begin
  System.Assert(Lines <> nil);
end;

procedure TCommandStringList.Execute;
begin
  Count := Count + 1;
  Lines.Add(Format('%.3d', [Count]));
end;

{$ENDREGION}
// ------------------------------------------------------------------------
// TCommandFactoryTests: Setup and TearDown
// ------------------------------------------------------------------------
{$REGION 'TCommandFactoryTests: Setup and tear down'}

procedure TCommandFactoryTests.Setup;
begin
  FStrings := TStringList.Create();
  TCommandA.IsExecuted := False;
end;

procedure TCommandFactoryTests.TearDown;
begin
  FreeAndNil(FStrings);
  TCommandA.IsExecuted := False;
end;

{$ENDREGION}
// ------------------------------------------------------------------------
// TCommandFactoryTests: Basic tests
// ------------------------------------------------------------------------
{$REGION 'TCommandFactoryTests: Basic tests'}

procedure TCommandFactoryTests.TestAdhocExecuteCommand;
begin
  TCommandVclFactory.ExecuteCommand<TCommandA>([]);
  Assert.IsTrue(TCommandA.IsExecuted,'TCommandA not executed');
end;

{$ENDREGION}
// ------------------------------------------------------------------------
// ------------------------------------------------------------------------

initialization

TDUnitX.RegisterTestFixture(TCommandFactoryTests);

end.
