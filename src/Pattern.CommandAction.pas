﻿{* ------------------------------------------------------------------------ *
 * Command Parttern  ♥  TCommandAction = command invoker
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
    Version = '1.0';
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
    function WithCaption(const aCaption: string): TCommandAction;
    function WithCommand(aCommand: TCommand): TCommandAction;
    function WithShortCut(aShorcut: TShortCut): TCommandAction;
    function WithEventOnUpdate(AUpdateProc: TProc<TCommandAction>)
      : TCommandAction;
    function WitEventAfterExecution(aAfterProc: TProc<TCommandAction>)
      : TCommandAction;
    function WithInjections(const Injections: array of const): TCommandAction;
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
  inherited;
end;

procedure TCommandAction.DoExecuteAction(Sender: TObject);
begin
  fCommand.Execute;
  if Assigned(fOnAfterProc) then
    fOnAfterProc(Self)
end;

function TCommandAction.WithInjections(const Injections: array of const)
  : TCommandAction;
begin
  System.Assert(fCommand <> nil,
    'Command have to be created and provided before injection');
  fCommand.WithInjections(Injections);
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

function TCommandAction.WithCaption(const aCaption: string): TCommandAction;
begin
  Caption := aCaption;
  Result := Self;
end;

function TCommandAction.WithCommand(aCommand: TCommand): TCommandAction;
begin
  fCommand := aCommand;
  Result := Self;
end;

function TCommandAction.WitEventAfterExecution
  (aAfterProc: TProc<TCommandAction>): TCommandAction;
begin
  fOnAfterProc := aAfterProc;
  Result := Self;
end;

function TCommandAction.WithEventOnUpdate(AUpdateProc: TProc<TCommandAction>)
  : TCommandAction;
begin
  fOnUpdateProc := AUpdateProc;
  Self.OnUpdate := OnUpdateEvent;
  Result := Self;
end;

function TCommandAction.WithShortCut(aShorcut: TShortCut): TCommandAction;
begin
  // ------------------------------------------------------------------
  // Too support shortcuts action requires TActionList assigned
  // ---
  // this code is constructing a new ActionList only once when a new
  // shortcut is assigned to this action (deleyed construction)
  // ---
  // Memory of fActionList is not released by Free but managed by Owner
  // ------------------------------------------------------------------
  if (Owner <> nil) and (Self.ActionList = nil) and (fActionList = nil) then
  begin
    fActionList := TActionList.Create(Owner);
    Self.ActionList := fActionList;
  end;
  Self.ShortCut := aShorcut;
  Result := Self;
end;

end.
