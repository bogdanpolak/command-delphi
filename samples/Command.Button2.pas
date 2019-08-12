unit Command.Button2;

interface

uses
  System.Classes, System.SysUtils,
  Vcl.StdCtrls, Vcl.Pattern.Command;

type
  TButon2Command = class (TCommand)
  private
    FMemo: TMemo;
    FEdit: TEdit;
  public
    procedure Execute; override;
  published
    property Memo: TMemo read FMemo write FMemo;
    property Edit: TEdit read FEdit write FEdit;
  end;

implementation

procedure TButon2Command.Execute;
begin
  inherited;
  Memo.Lines.Add('');
  Memo.Lines.Add('Getting info form Edit and put it here ...');
  Memo.Lines.Add('  * Edit.Text: '+Edit.Text);
end;

end.

