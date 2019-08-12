unit Form.Main;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  System.Actions, Vcl.ActnList, Vcl.Pattern.Command;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    Edit1: TEdit;
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses Command.Button1;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Button1.Action := TCommandVclFactory.CreateAction<TButon1Command>(Self,
    'Run command: Button1', [Memo1]);
end;

end.
