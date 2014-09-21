unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls, Menus, SQLPlusConsole, FormConfig;

type

  { TForm1 }

  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem4: TMenuItem;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    edSQL: TSynEdit;
    edSal: TSynEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure StatusBar1DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
  private
    { private declarations }
  public
    sqlCon: TSQLPlusCon;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  sqlCon:= TSQLPlusCon.Create;
  sqlCon.sendCRLF:=true;

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  sqlCon.Destroy;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Config.Initiate(StatusBar1.Panels[0]);
  sqlCon.Iniciar(StatusBar1.Panels[1], edSal,Config.fcConOra);
end;

procedure TForm1.MenuItem1Click(Sender: TObject);  //config
begin
  Config.ShowModal;
end;

procedure TForm1.MenuItem2Click(Sender: TObject);  //connect
begin
  sqlCon.Open;
end;

procedure TForm1.MenuItem3Click(Sender: TObject);   //disconnect
begin
  sqlCon.Close;
end;

procedure TForm1.MenuItem4Click(Sender: TObject);  //send
begin
  sqlCon.EnviarSQL(edSQL.Text);
end;

procedure TForm1.StatusBar1DrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
begin
  if panel.Index = 0 then begin
    Config.fcConOra.DrawStatusPanel(StatusBar.Canvas, Rect);
  end;
  if panel.Index = 1 then
    sqlCon.DrawStatePanel(StatusBar.Canvas, Rect);
end;

end.

