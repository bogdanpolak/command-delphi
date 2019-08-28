unit Tests.TCommandFactory;

interface

uses
  DUnitX.TestFramework,
  System.Classes, System.SysUtils;

{$M+}

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
    procedure TestSample1;
  end;

type
  TCommandA = class (TCommand)
  strict private
    FActive: boolean;
    FCount: integer;
  protected
    procedure Guard; override;
  public
    procedure Execute; override;
    property Active: boolean read FActive write FActive;
    property Count: integer read FCount write FCount;
  end;

  TCommandStringList = class (TCommand)
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


implementation

// ------------------------------------------------------------------------
// class TCommandA
// ------------------------------------------------------------------------
{$REGION 'class TCommandA'}
procedure TCommandA.Guard; 
begin
end;

procedure TCommandA.Execute;
begin
  Active := True;
  Count := Count +1;
end;

{$ENDREGION}
// ------------------------------------------------------------------------
// class TCommandStringList
// ------------------------------------------------------------------------
{$REGION 'class TCommandStringList'}
procedure TCommandStringList.Guard; 
begin
  Assert(Lines<>nil);
end;

procedure TCommandStringList.Execute;
begin
  Count := Count +1;
  Lines.Add (Format('%.3d',[Count]));
end;

{$ENDREGION}
// ------------------------------------------------------------------------
// Setup and TearDown
// ------------------------------------------------------------------------
{$REGION 'Setup and tear down'}

procedure TCommandFactoryTests.Setup;
begin
  FStrings := TStringList.Create();
end;

procedure TCommandFactoryTests.TearDown;
begin
  FreeAndNil (FStrings);
end;

{$ENDREGION}
// ------------------------------------------------------------------------
// Basic tests
// ------------------------------------------------------------------------
{$REGION 'Basic tests'}

procedure TCommandFactoryTests.TestSample1;
begin

end;

{$ENDREGION}

initialization

TDUnitX.RegisterTestFixture(TObervableTests);

end.
