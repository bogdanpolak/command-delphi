unit Unit4_Subfolder_WithVersion;

interface

uses
  System.Classes,
  System.SysUtils;

type
  TClass1 = class
  strict private const
    Version = '2.1';
  private
    fUpdateInterval: integer;
    fOnUpdateProc: TProc;
    procedure OnUpdateTimer(Sender: TObject);
  protected
    fBeforeStartEvent: TProc;
    fAfterFinishEvent: TProc;
    fThread: TThread;
    fIsCommandDone: boolean;
    fTimer: TTimer;
    procedure SetIsCommandDone(aIsTermianted: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

constructor TClass1.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TClass1.Destroy;
begin
  inherited;
end;

procedure TClass1.OnUpdateTimer(Sender: TObject);
begin
end;

procedure TClass1.SetIsCommandDone(aIsTermianted: boolean);
begin
end;

end.
