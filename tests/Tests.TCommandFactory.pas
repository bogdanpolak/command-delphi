unit Tests.TCommandFactory;

interface

uses
  DUnitX.TestFramework,
  System.Classes, System.SysUtils;

{$M+}

type

  [TestFixture]
  TObervableTests = class(TObject)
  private
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    // -------------
    procedure TestSample1;
  end;

implementation

// ------------------------------------------------------------------------
// Setup and TearDown
// ------------------------------------------------------------------------
{$REGION 'Setup and tear down'}

procedure TObervableTests.Setup;
begin

end;

procedure TObervableTests.TearDown;
begin

end;

{$ENDREGION}
// ------------------------------------------------------------------------
// Basic tests
// ------------------------------------------------------------------------
{$REGION 'Basic tests'}

procedure TObervableTests.TestSample1;
begin

end;

{$ENDREGION}

initialization

TDUnitX.RegisterTestFixture(TObervableTests);

end.
