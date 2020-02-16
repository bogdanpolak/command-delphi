unit Form.Main;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Actions,
  Winapi.Windows, Winapi.Messages,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ActnList, Vcl.ExtCtrls, Vcl.PlatformDefaultStyleActnCtrls,
  Vcl.ActnMan, Vcl.ComCtrls, Vcl.Menus,
  Pattern.Command,
  Pattern.CommandAction,
  Command.Button1,
  Command.Button2;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    GroupBoxSimpleDemo: TGroupBox;
    btnAdhocExecute: TButton;
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure btnAdhocExecuteClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  public
  private
    actCommandButon1: TCommandAction;
    actCommandButon2: TCommandAction;
    procedure BuildCommandsOnStart;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.BuildCommandsOnStart;
begin
  // ---------------------------------------------------------
  // ---------------------------------------------------------
  actCommandButon1 := TCommandAction.Create(Self)
    .WithCommand(TButon1Command.Create(Self))
    .WithShortCut(TextToShortCut('Ctrl+1'))
    .WithCaption('Run command: Button1') //--+
    .WithInjections([Memo1]);
  // ---------------------------------------------------------
  // ---------------------------------------------------------
  actCommandButon2 := TCommandAction.Create(Self)
    .WithCommand(TButon2Command.Create(Self))
    .WithShortCut(TextToShortCut('Ctrl+2'))
    .WithCaption('Run command: Button2') //--+
    .WithInjections([Memo1, Edit1]);
  // ---------------------------------------------------------
  // ---------------------------------------------------------
  Button1.Action := actCommandButon1;
  Button2.Action := actCommandButon2;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  BuildCommandsOnStart;
  Memo1.Clear;
  ReportMemoryLeaksOnShutdown := True;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  // this event is not called. Action actCommandButon1 is triggered
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  // this event is not called. Action actCommandButon2 is triggered
end;

procedure TForm1.btnAdhocExecuteClick(Sender: TObject);
begin
  TCommand.AdhocExecute<TButon1Command>([Memo1]);
end;

end.
