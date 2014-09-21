{This unit is used like the Main Configuration Form. It needs the units FrameCfgConOra
and ConfigFrame }
unit FormConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Buttons, FrameCfgConOra, ConfigFrame;

type

  { TConfig }

  TConfig = class(TForm)
    BitAceptar: TBitBtn;
    BitCancel: TBitBtn;
    procedure BitAceptarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure SaveToFile;
    { private declarations }
  public
    msjError: string;    //Error string
    fraError: TFrame;    //Frame with error (in this case, we just have one)
    arIni   : String;    //INI file

    fcConOra: TfraCfgConOra;   //frame for to configurate the connection
    procedure Initiate(PanConAct: TStatusPanel);
  end;

var
  Config: TConfig;

implementation

{$R *.lfm}

{ TConfig }

procedure TConfig.FormCreate(Sender: TObject);
begin
  fcConOra := TfraCfgConOra.Create(Self);
  fcConOra.Parent := self;
  arIni := GetIniName;
end;

procedure TConfig.FormDestroy(Sender: TObject);
begin
  Free_AllConfigFrames(self);  //free all config frames
end;

procedure TConfig.FormShow(Sender: TObject);
begin
  fraError := PropToWindow_AllFrames(self);
end;

procedure TConfig.Initiate(PanConAct: TStatusPanel);
begin
  fcConOra.Iniciar('fcConOra', PanConAct);
  fcConOra.ShowPos(60,0);
  fcConOra.SetLanguage('en');
  msjError := ReadFileToProp_AllFrames(self, arINI);
end;

procedure TConfig.BitAceptarClick(Sender: TObject);
begin
  fraError := WindowToProp_AllFrames(self);
  if fraError<>nil then begin
    showmessage(fraError.MsjErr);
    exit;
  end;
  SaveToFile;
  self.Close;
end;

procedure TConfig.SaveToFile;
begin
  msjError := SavePropToFile_AllFrames(self, arINI);
end;

end.

