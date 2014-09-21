{Define a la ventana de sesión. Esta ventana permite mostrar el texto que va llegando
 de un proceso. Servirá para visualizar como se interactúa con la sesión y para poder
 iniciar conexiones a sqlplus mediante el telnet.}

unit FormVentSesion;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, SQLPlusConsole, SynEditKeyCmds;

type

  { TfrmVentSesion }

  TfrmVentSesion = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    edSal: TSynEdit;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
    proc:  TSQLPlusCon;         //referencia al proceso actual
  end;

var
  frmVentSesion: TfrmVentSesion;

implementation
{$R *.lfm}
const MAX_LINEAS = 1000;
{ TfrmVentSesion }

procedure TfrmVentSesion.FormCreate(Sender: TObject);
begin
  proc := nil;
end;

procedure TfrmVentSesion.Button1Click(Sender: TObject);
begin
  if proc = nil then exit;
  proc.SendLn(Edit1.Text);
end;

end.

