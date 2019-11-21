unit Command.Button2;

interface

uses
  System.Classes,
  System.SysUtils,
  Vcl.StdCtrls,
  Pattern.Command;

type
  TButon2Command = class (TCommand)
  private
    FMemo: TMemo;
    FEdit: TEdit;
  protected
    procedure DoGuard; override;
    procedure DoExecute; override;
  published
    property Memo: TMemo read FMemo write FMemo;
    property Edit: TEdit read FEdit write FEdit;
  end;

implementation

procedure TButon2Command.DoGuard;
begin
  Assert(Memo<>nil);
  Assert(Edit<>nil);
end;

procedure TButon2Command.DoExecute;
begin
  Memo.Lines.Add('');
  Memo.Lines.Add('Getting info form Edit and put it here ...');
  Memo.Lines.Add('  * Edit.Text: '+Edit.Text);
end;

end.

