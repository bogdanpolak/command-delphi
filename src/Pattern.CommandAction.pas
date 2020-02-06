{* ------------------------------------------------------------------------ *
 * ♥   Command Invoker
 * Components:     TCommandAction
 * Project:        https://github.com/bogdanpolak/command-delphi
 * ------------------------------------------------------------------------ *}
unit Pattern.CommandAction;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Actions,
  Vcl.ActnList,
  Pattern.Command;

type
  TCommandAction = class(TAction)
  private const
    Version = '0.7';
  private
    fCommand: TCommand;
    fOnUpdateProc: TProc<TCommandAction>;
    fOnAfterProc: TProc<TCommandAction>;
    fDisableDuringExecution: boolean;
    fActionList: TActionList;
    procedure OnExecuteEvent(Sender: TObject);
    procedure OnUpdateEvent(Sender: TObject);
    procedure DoExecuteAction(Sender: TObject);
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    function SetupCaption(const aCaption: string): TCommandAction;
    function SetupCommand(aCommand: TCommand): TCommandAction;
    function SetupShortCut(aShorcut: TShortCut): TCommandAction;
    function SetupEventOnUpdate(AUpdateProc: TProc<TCommandAction>)
      : TCommandAction;
    function SetupEventAfterExecution(aAfterProc: TProc<TCommandAction>)
      : TCommandAction;
    function Inject(const Injections: array of const): TCommandAction;
    property Command: TCommand read fCommand write fCommand;
    property DisableDuringExecution: boolean read fDisableDuringExecution
      write fDisableDuringExecution;
  end;

implementation

constructor TCommandAction.Create(aOwner: TComponent);
begin
  inherited;
  DisableDuringExecution := False;
  fActionList := nil;
  fCommand := nil;
  fOnUpdateProc := nil;
  fOnAfterProc := nil;
  Self.OnExecute := OnExecuteEvent;
end;

destructor TCommandAction.Destroy;
begin
  if fActionList <> nil then
    fActionList.Free;
  inherited;
end;

procedure TCommandAction.DoExecuteAction(Sender: TObject);
begin
  fCommand.Execute;
  if Assigned(fOnAfterProc) then
    fOnAfterProc(Self)
end;

function TCommandAction.Inject(const Injections: array of const)
  : TCommandAction;
begin
  System.Assert(fCommand <> nil,
    'Command have to be created and provided before injection');
  fCommand.Inject(Injections);
  Result := Self;
end;

procedure TCommandAction.OnExecuteEvent(Sender: TObject);
begin
  System.Assert(fCommand <> nil);
  if DisableDuringExecution then
  begin
    try
      Self.Enabled := False;
      DoExecuteAction(Sender);
    finally
      Self.Enabled := True;
    end;
  end
  else
    DoExecuteAction(Sender);
end;

procedure TCommandAction.OnUpdateEvent(Sender: TObject);
begin
  if Assigned(fOnUpdateProc) then
    fOnUpdateProc(Self);
end;

function TCommandAction.SetupCaption(const aCaption: string): TCommandAction;
begin
  Caption := aCaption;
  Result := Self;
end;

function TCommandAction.SetupCommand(aCommand: TCommand): TCommandAction;
begin
  fCommand := aCommand;
  Result := Self;
end;

function TCommandAction.SetupEventAfterExecution
  (aAfterProc: TProc<TCommandAction>): TCommandAction;
begin
  fOnAfterProc := aAfterProc;
  Result := Self;
end;

function TCommandAction.SetupEventOnUpdate(AUpdateProc: TProc<TCommandAction>)
  : TCommandAction;
begin
  fOnUpdateProc := AUpdateProc;
  Self.OnUpdate := OnUpdateEvent;
  Result := Self;
end;

function TCommandAction.SetupShortCut(aShorcut: TShortCut): TCommandAction;
begin
  if fActionList = nil then
  begin
    fActionList := TActionList.Create(Owner);
    ActionList := fActionList;
  end;
  Self.ShortCut := aShorcut;
  Result := Self;
end;

end.
