unit Command.Button1;

interface

uses
  System.Classes,
  System.SysUtils,
  Vcl.StdCtrls,
  Pattern.Command;

type
  TButon1Command = class (TCommand)
  private
    FMemo: TMemo;
  protected
    procedure DoGuard; override;
    procedure DoExecute; override;
  published
    property Memo: Vcl.StdCtrls.TMemo read FMemo write FMemo;
  end;

implementation

procedure TButon1Command.DoGuard;
begin
  Assert(Memo<>nil);
end;

procedure TButon1Command.DoExecute;
begin
  Memo.Lines.Add('[1] Simple message from command 1');
  Memo.Lines.Add('--- ---');
end;

end.
