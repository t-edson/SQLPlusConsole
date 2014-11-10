unit FrameCfgSQLPlusOut;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, FrameSQLPlusOut,
  ConfigFrame;

type

  { TfraCfgSQLPlusOut }

  TfraCfgSQLPlusOut = class(TFrame)
    chkResPrompt: TCheckBox;
    rdgOutMode: TRadioGroup;
    procedure fraCfgSQLPlusOutUpdateChanges;
  private
    fraSQLout: TfraSQLPlusOut;
  public
    ResPrompt  : Boolean;
    OutMode    : TSQLPlusOutMode;   //modo de salida
    procedure Iniciar(secINI0: string; fraSQLout0: TfraSQLPlusOut);
    procedure SetLanguage(lang: string);
  end;

implementation

{$R *.lfm}

{ TfraCfgSQLPlusOut }

procedure TfraCfgSQLPlusOut.Iniciar(secINI0: string; fraSQLout0: TfraSQLPlusOut);
begin
  secINI := secINI0;  //sección INI
  fraSQLout := fraSQLout0;  //referencia
  Asoc_Bol_TChkB(@ResPrompt, chkResPrompt,'ResPrompt', true);
  Asoc_Enum_TRadGroup(@OutMode, SizeOf(OutMode), rdgOutMode, 'OutMode',0);
  OnUpdateChanges:=@fraCfgSQLPlusOutUpdateChanges;
end;

procedure TfraCfgSQLPlusOut.fraCfgSQLPlusOutUpdateChanges;
//Actualiza los cambios
begin
  if fraSQLout = nil then exit;
  fraSQLout.Mode:=OutMode;
end;

procedure TfraCfgSQLPlusOut.SetLanguage(lang: string);
//Rutina de traducción
begin
  case lowerCase(lang) of
  'es': begin
      chkResPrompt.Caption:='&Resaltar prompt en salida';
      rdgOutMode.Caption:='Formato de salida';
      rdgOutMode.Items[0] := '&Texto (Solo última consulta).';
      rdgOutMode.Items[1] := 'Texto &Contínuo';
      rdgOutMode.Items[2] := 'Mostrar &Grilla';

  end;
  'en': begin
      chkResPrompt.Caption:='&Highlight prompt in output';
      rdgOutMode.Caption:='Output Mode';
      rdgOutMode.Items[0] := '&Text (Only Last query).';
      rdgOutMode.Items[1] := '&Contínued Text';
      rdgOutMode.Items[2] := 'Show &Grid';
    end;
  end;
end;

end.

