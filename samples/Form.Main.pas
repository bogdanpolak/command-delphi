unit Form.Main;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ActnList, Vcl.ExtCtrls,
  Vcl.Pattern.Command;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    Edit1: TEdit;
    Bevel1: TBevel;
    Bevel2: TBevel;
    btnExecuteTwoCommands: TButton;
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses Command.Button1, Command.Button2;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Memo1.Clear;
  Button1.Action := TCommandVclFactory.CreateCommandAction<TButon1Command>(Self,
    'Run command: Button1', [Memo1]);
  Button2.Action := TCommandVclFactory.CreateCommandAction<TButon2Command>(Self,
    'Run command: Button2', [Memo1, Edit1]);
  {
    . Button2.Action := TCommandVclFactory.CreateCommandAction<TButon2Command>(Self,
    .   'Run command: Button2', [InjectProperty('Memo', Memo1),
    .   InjectProperty('Edit', Edit1)]);
    .
    . Button2.Action := TCommandVclFactory.CreateCommandAction<TButon2Command>(Self,
    .   'Run command: Button2', TPublishedIjector.Create.Inject('Memo', Memo1)
    .   .Injcect('Edit', Edit1));
    .
    . Button2.Action := TCommandVclFactory.CreateCommandAction<TButon2Command>(Self,
    .   'Run command: Button2', ['Memo','Edit'], [Memo1,Edit1]);
    .
    . Button2.Action := TCommandVclFactory.CreateCommandAction<TButon2Command>(Self,
    .   'Run command: Button2', [['property.Memo',Memo1],['Edit',Edit1]]);
    .
  }
  ReportMemoryLeaksOnShutdown := true;
end;

end.
