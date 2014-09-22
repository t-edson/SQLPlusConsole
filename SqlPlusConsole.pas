{SQLPlusConsole 0.4b
===================
Por Tito Hinostroza 21/9/2014
*
Descripción
===========
Unidad para manejar el proceso SQLPLUS, como un proceso en una consola, usando la
unidad "UnTerminal", para controlas la entrada/salida. Espera trabajar con un editor
de tipo SynEdit. Por lo tanto, intercepta los eventos de refresco para un editor.
También intercepta a los eventos: OnFirstReady(), OnReadData(), OnGetPrompt().
Agrega además los eventos:
-OnQueryEnd(), para indicar que terminó la consulta.
-OnErrorConx(), cuando se produce un error en la conexión.

Requiere de la unidad 'FrameCfgConOra', para acceder a un frame de configuración de
donde debe leer la conexión actual.
}
unit SQLPlusConsole;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, SynEdit, ComCtrls, strutils, Dialogs, LCLProc, math,
  TermVT, UnTerminal, FrameCfgConOra;

const
  TXT_MJE_ERROR = 'ERROR at line ';  //texto indicativo de error con posición

  COMAN_INIC = #13#10'set linesize 6000;'#13#10+
               'set pagesize 100'#13#10+
               'set tab off'#13#10+
               'set trimspool on;'#13#10+
               'set serveroutput on;'#13#10+
//'set feedback off;'#13#10+ //Si no hay datos con feedback=off el prompt sale en la misma fila
               'alter session set nls_date_format = ''yyyy/mm/dd hh24:mi'';'#13#10 +
               '';
type
  { TSQLPlusCon }

  TSQLPlusCon = class(TConsoleProc)
  private
    fcConOra : TfraCfgConOra;   //referencia a Frame d configuración.
    FmaxLinTer : integer;  //cantidad máxima de líneas que debe almacenar el terminal
    procedure BuscarErrorEnLineas;
    procedure PosicionarCursor(HeightScr: integer);
    procedure SetmaxLinTer(AValue: integer);
    procedure SQLPlusConFirstReady(prompt: string; pIni: TPoint;
      HeightScr: integer);
    procedure SQLPlusConGetPrompt(prompt: string; pIni: TPoint;
      HeightScr: integer);
    procedure SQLPlusConReadData(nDat: integer; pFinal: TPoint);
  protected
    ed     : TSynEdit;   //editor donde se mostrará la salida
    linSql : integer;    //línea donde empieza la última consulta enviada
    linSqlT: integer;    //línea en el terminal donde empieza la consulta enviada
    procedure procInitScreen(const grilla: TtsGrid; fIni, fFin: integer);
    procedure procRefreshLine(const grilla: TtsGrid; fIni, HeightScr: integer);
    procedure procRefreshLines(const grilla: TtsGrid; fIni, fFin,
      HeightScr: integer);
    procedure procAddLine(HeightScr: integer);
  public
    curSigPrm : boolean;   //Indica si el cursor seguirá al texto que llega
    HayError  : boolean;   //Indica que se detectó un error en la última exploración
    cadError  : string;    //guarda el mensaje de error
    pErr      : TPoint;    //posición del error;
    OnQueryEnd: procedure of object;
    OnErrorConx: procedure of object;
    property maxLinTer: integer read FmaxLinTer write SetmaxLinTer;
    procedure Open;        //inicia proceso
    function Closed: boolean;
    procedure Iniciar(PanControl: TStatusPanel; ed0: TSynEdit;
      fcConOra0: TfraCfgConOra);
    procedure EnviarSQL(txt: string);  //Envía consulta al SQLPLUS
    procedure ClearScreen;
    constructor Create;  //Constructor
  end;

implementation
const
  MIN_LIN_TER = 200;  //nunca debe ser menor que BLOCK_DEL + term.Height
  BLOCK_DEL   = 100;  //cantidad de líneas a borrar en bloque

{ TSQLPlusCon }

procedure TSQLPlusCon.Iniciar(PanControl: TStatusPanel; ed0: TSynEdit; fcConOra0: TfraCfgConOra);
//Configura la conexión, fijando el panel de estado, editor para mostrar la salida, y el
//frame con las conexiónes.
begin
  panel := PanControl;
  if panel<> nil then
    panel.Style:=psOwnerDraw;  //configura panel para dibujarse por evento
  if ed0= nil then exit;
  ed := ed0;
  fcConOra := fcConOra0;  //guarda referecnia
  OnInitScreen:=@procInitScreen;
  OnRefreshLine:=@procRefreshLine;
  OnRefreshLines:=@procRefreshLines;
  OnAddLine:=@procAddLine;

  OnFirstReady:=@SQLPlusConFirstReady;
  OnGetPrompt:=@SQLPlusConGetPrompt;
  OnReadData:=@SQLPlusConReadData;
//  OnLineCompleted:=@SQLPlusConLineCompleted;
  ClearOnOpen:=false;  //No limpiaremos en cada conexión
  ClearScreen;   //limpiamos solo ahora
end;

procedure TSQLPlusCon.EnviarSQL(txt: string);
var
  yvt: Integer;
begin
  //captura posición del prompt en el editor
  if ed = nil then begin
    linSql := -1;  //no accesible
  end else begin
    yvt := ed.Lines.Count- term.height-1;
    linSql := yvt+term.CurY+1;  //posición del cursor
  end;
  //captura posición del prompt en Terminal
  linSqlT := term.CurY;
  //envía
  HayError := false;
  SendLn(txt);
end;
procedure TSQLPlusCon.ClearScreen;
//Limpia la pantalla de salida. Deja al menos 25 líneas.
begin
  ed.ClearAll;
  ClearTerminal;  //generará el evento OnInitScreen()
end;
procedure TSQLPlusCon.BuscarErrorEnLineas;
//Busca en las últimas líneas, para ver si hay un mensaje de error
//Solo explora desde la posición del prompt anterior, o desde que se inicia
//la sesión.
//Actualiza las banderas "HayDesco", "HayError" y "cadError"
var
  i,j   : integer;
  pmsj  : integer;
  linea : string;
//  linExp: Integer;

  function HaySP2(nLin: integer): integer;
  //Indica si en la línea dada, está la cadena de error " SP2-"
  //Si existe, devuelve la posición horizontal, de otra forma devuelve 0
  var linea: string;
  begin
    Result:=0;
    if nLin<1 then exit;  //no es línea válida
    linea := term.buf[nLin];
    if length(linea) < 6 then exit;  //no puede ser
    //Primer intento de búsqueda. Al inicio de línea
    if copy(linea,1,4) = 'SP2-' then begin
      Result := 1;  //hay
      exit;
    end;
    //Segundo intento de búsqueda. Dentro de la línea
    Result := Pos(' SP2-',linea); //busca
    if Result>0 then Inc(Result);  //ajusta posición
  end;

begin
  //Busca mensajes de tipo ORA-???? o el fin de la sesión
  for i:= term.CurY downto max(1,linSqlT) do begin  //busca las últimas líneas
    linea := term.buf[i];
    if AnsiContainsStr(linea,'ORA-') then begin
       HayError := true;  //Se detectó error
       cadError := linea;
       pErr.x := -1;   //indica que no hay fila de error
       pErr.y := -1;   //indica que no hay columna de error
       //busca línea anterior
       j := i-1;   //número de línea anterior
       if j < 1 then break;  //no hay más líneas
       linea := term.buf[j];
       if AnsiStartsStr(TXT_MJE_ERROR, linea) then begin
         //quita mensaje inicial y ":" final
         linea := copy(linea,length(TXT_MJE_ERROR)+1,1000);
         linea := copy(linea,1,length(linea)-1);
         //obtiene número de línea
         pErr.y := StrToInt(linea);
         //busca línea anterior
         Dec(j);
         if j < 1 then break;  //no hay más líneas
         linea := term.buf[j];
         //verifica si hay información de posición horizontal
         if AnsiEndsStr('*', linea) then pErr.x := length(linea);
       end;
       break;   //sale
    end else if AnsiStartsStr('Disconnected from Oracle', linea) then begin
      //se desconectó, ya no vale seguir buscando atrás
//      HayDesco := true;
      break;
    end else if AnsiStartsStr('SQL*Plus: Release', linea) then begin
      //se inició una nueva conexión, ya no vale seguir buscando atrás
//      HayDesco := true;
      break;
    end;
  end;
  //Busca mensajes de tipo SP2-????
  for i:= term.CurY downto max(1,linSqlT) do begin  //busca las últimas líneas
    if HaySP2(i)>0 then begin
       //hay error. En este caso, pueden haber mensajes anteriores
       j := i;   //toma índice
       //busca si hay anteriores
       while (HaySP2(j-1)>0) or (HaySP2(j-2)>0) do begin
         j:=j-1;         //retrocede
       end;
      //solo hay hasta "j"
       HayError := true;  //Se detectó error
       pmsj := HaySP2(j);  //lee posición
       cadError := Copy(term.buf[j], pmsj, 1000);  //coge cadena
       //este mensaje de error no contiene posición. Aunque se podría deducir.
       pErr.x := -1;   //indica que no hay fila de error
       pErr.y := -1;   //indica que no hay columna de error
       //Intenta ubicar línea con error cosiderando que la línea tiene el formato:
       //   SQL>SQL> 2 3 4 5 SQL> SP2-XXX
       if pmsj > 9 then begin
         //puede ser.
         linea := copy(term.buf[j],1,pmsj-1);
         if copy(linea,pmsj-5,1000) = 'SQL> ' then begin
           linea := StringReplace(linea,'SQL> ','',[rfReplaceAll]); //quita " SQL>"
           linea := trim(linea);   //limpia
           j:=length(linea);   //apunta al final de  SQL>SQL> 2 3 4 5
           if j=0 then break; //no hay datos
           if not (linea[j] in ['0'..'9']) then  //ve si es número
             break;  //sale
           //coge número de línea final
           while linea[j] in ['0'..'9'] do
             Dec(j);
           //se puede adivinar la línea con error
           pErr.x := 1;   //indica que no hay fila de error
           pErr.y := StrToInt(copy(linea,j,1000));   //toma el último número de línaa
         end;
       end;
       break;   //sale
    end;
  end;
end;
constructor TSQLPlusCon.Create;
//"PanControl" es el panel en donde se indicará el estado de la conexión
begin
  inherited Create(nil);
  curSigPrm := true;
  FmaxLinTer := 10000;   //líneas a almacenar
  self.TerminalWidth:=10000;  //para soportar muchas columnas
  linSql := -1;   //inicialmente no accesible
  linSqlT := -1;  //inicialmente no accesible
  //configura prompt
  self.detecPrompt:=true;
  self.promptIni:='SQL> ';
  self.promptFin:='';
  self.promptMatch:=prmAtEnd;
end;
procedure TSQLPlusCon.PosicionarCursor(HeightScr: integer);
//Coloca el cursor del editor, en la misma línea que tiene el cursor del
//terminal VT100 virtual.
var
  yvt: Integer;
begin
  if curSigPrm then begin
    yvt := ed.Lines.Count-HeightScr-1;  //calcula fila equivalente a inicio de VT100
    ed.CaretXY := Point(1, yvt+term.CurY+1);  //siempre al inicio
  end;
end;
procedure TSQLPlusCon.SetmaxLinTer(AValue: integer);
begin
  if FmaxLinTer=AValue then Exit;
  if FmaxLinTer<MIN_LIN_TER then exit;  //protección
  FmaxLinTer:=AValue;
end;

procedure TSQLPlusCon.SQLPlusConFirstReady(prompt: string; pIni: TPoint;
  HeightScr: integer);
//Se produjo la primer oonexión
begin
  state := ECO_READY;  //para que pase a ECO_BUSY
  SendLn(COMAN_INIC);
end;
procedure TSQLPlusCon.SQLPlusConGetPrompt(prompt: string; pIni: TPoint;
  HeightScr: integer);
begin
//debugln('-GetPrompt');
//debugln('promp ant:'+IntToStr(linSqlT));
  BuscarErrorEnLineas;  //busca si hubo algún mesaje de error
  if HayError then begin
    if pErr.y<>-1 then begin //hay información de posición
      ShowMessage(cadError + #13#10' (Línea:' + IntToStr(pErr.y) + ', Columna:' +
                          IntToStr(pErr.x)+')');
//      edSQL.CaretXY:=pErr;  //ubica
    end else begin
      ShowMessage(cadError);
    end;
  end;
  if OnQueryEnd<>nil then OnQueryEnd;
end;
procedure TSQLPlusCon.SQLPlusConReadData(nDat: integer; pFinal: TPoint);
begin
//debugln('   ReadData:'+IntToStr(nDat));
  //Se aprovecha para verificar si hay mensajes de error
  if state = ECO_CONNECTING then begin
    //Estamos esperando una conexión. Hay que buscar errores de conexión
    BuscarErrorEnLineas;    //busca si hubo algún mesaje de error
    if HayError and (OnErrorConx <> nil) then OnErrorConx;
//    if HayError then ShowMessage(cadError);
  end;
end;

procedure TSQLPlusCon.procInitScreen(const grilla: TtsGrid; fIni, fFin: integer);
var
  i: Integer;
begin
//debugln('==InitLines');
  for i:=fIni to fFin do
    ed.Lines.Add(grilla[i]);
end;
procedure TSQLPlusCon.procRefreshLine(const grilla: TtsGrid; fIni, HeightScr: integer);
var
  yvt: Integer;
begin
//  debugln('procRefreshLine: '+IntToStr(fIni));
  yvt := ed.Lines.Count-HeightScr-1;
  ed.Lines[yvt+fIni] := grilla[fIni];
  //Llamamos a ProcessMessages para pòder refrescar la aplicación cuando los datos
  //llegan muy seguido.
//  Application.ProcessMessages; //PRODUCE que se desordenen las línesa de salida
end;
procedure TSQLPlusCon.procRefreshLines(const grilla: TtsGrid; fIni, fFin,
  HeightScr: integer);
var
  yvt: Integer;
  f: Integer;
begin
//  debugln('procRefreshLines: '+IntToStr(fIni)+','+IntToSTr(fFin));
  yvt := ed.Lines.Count-HeightScr-1;  //calcula fila equivalente a inicio de VT100
  ed.BeginUpdate;
  for f:=fIni to fFin do
    ed.Lines[yvt+ f] := grilla[f];
  PosicionarCursor(HeightScr);
  ed.EndUpdate;
  ed.Refresh;  //para mostrar el cambio
end;
procedure TSQLPlusCon.procAddLine(HeightScr: integer);
var
  i: Integer;
begin
  ed.BeginUpdate();
  if ed.Lines.Count > FmaxLinTer then begin
    //hace espacio
    for i:= 1 to BLOCK_DEL do
      ed.Lines.Delete(0);
    //actualiza linSql
//    if linSql> 0 then  //para que de la diferencia
    linSql := linSql - BLOCK_DEL;
  end;
  ed.Lines.Add('');
  ed.EndUpdate;
  //actualiza linSqlT
  linSqlT := linSqlT - 1;
end;

procedure TSQLPlusCon.Open;
//Abre una conexión al SQLPLUS, con la conexión actual, definida en Config.fcConOra
var
  con: TConOra;
begin
  HayError := false;
  cadError := '';
  if fcConOra = nil then begin
    HayError :=true;
    cadError := 'No se ha especificado conexiones.';
    exit;
  end;
  con := fcConOra.ConexActual;  //lee conexión
  if con.Nombre='' then begin
    HayError :=true;
    cadError := 'No se ha especificado la conexión actual.';
    exit;
  end;
  self.progPath:=con.RutSql;
  self.progParam:=con.Params;
  inherited;
end;

function TSQLPlusCon.Closed: boolean;
//Indica si la conexión está cerrada
begin
  Result := (state = ECO_STOPPED);
end;

end.

