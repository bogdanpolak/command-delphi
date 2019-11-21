unit Form.Main;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ActnList, Vcl.ExtCtrls,
  Pattern.Command,
  Pattern.CommandAction;

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
    procedure btnExecuteTwoCommandsClick(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses Command.Button1, Command.Button2;

procedure TForm1.btnExecuteTwoCommandsClick(Sender: TObject);
begin
  TCommand.AdhocExecute<TButon1Command>([Memo1]);
  TCommand.AdhocExecute<TButon2Command>([Memo1, Edit1]);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Memo1.Clear;
  Button1.Action := TCommandAction.Create(Self)
    .SetupCaption('Run command: Button1')
    .SetupCommand(TButon1Command.Create(Self).Inject([Memo1]));

  Button2.Action := TCommandAction.Create(Self)
    .SetupCaption('Run command: Button2')
    .SetupCommand(TButon2Command.Create(Self).Inject([Memo1, Edit1]));

  ReportMemoryLeaksOnShutdown := true;
end;

end.
