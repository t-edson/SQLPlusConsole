{
SQLPlusConsole 0.5
==================
Por Tito Hinostroza 12/10/2014
* Se cambia el nombre del método DisableOut() por DisableOutEdit().
* Se cambia el nombre de "ed" a "edSal".
* Se crea el evento OnErrorSQL(), y se quita el mensaje de error que se mostraba
desde dentro de esta unidad.
* Se simplifica al método Init(), para que no se indique el editor de salida.
* Se modifica InitOut(), para que reciba al editor de salida.
* Se definen dos métodos para direccionar la salida a un editor. Uno es usando SetOut()
y el otro usando SetOutVT100(). Ambos métodos usan grupos de eventos distintos.
* Se cambia de nombre a los métodos de respuesta a eventos, para que sigan la norma:
<objeto>-<evento>.
* Se crea la función CurrentCon(), para retornar la conexión actual.
* Se crea el método DisableAllOut(), para desconectar la salida de datos.
* Se crea la función TakeConnection(), para permitir usar la conexión por diversos
procesos.

Descripción
===========
Unidad para manejar el proceso SQLPLUS, como un proceso en una consola, usando la
unidad "UnTerminal", para controlar la entrada/salida. Espera trabajar con un editor
de tipo SynEdit como visor de datos de salida. Por lo tanto, intercepta los eventos
de refresco para un editor, que ofrece la unidad UnTerminal.
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
  Classes, SysUtils, LCLProc, LCLType, Controls, ComCtrls, strutils,
  Graphics, Math, SynEdit, SynEditKeyCmds,
  MisUtils, TermVT, UnTerminal, FrameCfgConOra, SqlPlusHighlighter;

const
  TXT_MJE_ERROR = 'ERROR at line ';  //texto indicativo de error con posición

  COMAN_INIC = #13#10+
               'set linesize 9000;'#13#10+
               'set pagesize 100'#13#10+
               'set tab off'#13#10+
               'set trimspool on;'#13#10+
               'set serveroutput on;'#13#10+
//'set feedback off;'#13#10+ //Si no hay datos con feedback=off el prompt sale en la misma fila
               'alter session set nls_date_format = ''yyyy/mm/dd hh24:mi'';'#13#10 +
               '';
type
  TsqEvError = procedure(CurXY: TPoint; const msg: string) of object;
  { TSQLPlusCon }

  TSQLPlusCon = class(TConsoleProc)
    procedure ed_CommandProcessed(Sender: TObject;
      var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
    procedure ed_MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    fcConOra   : TfraCfgConOra;   //referencia a Frame de configuración.
    FmaxLinTer : integer;  //cantidad máxima de líneas que debe almacenar el terminal
    procedure BuscarErrorEnLineas;
    procedure PosicionarCursor; inline;
    procedure PosicionarCursor(HeightScr: integer);
    procedure SetmaxLinTer(AValue: integer);
  protected
    linSql : integer;    //línea donde empieza la última consulta enviada
    linSqlT: integer;    //línea en el terminal donde empieza la consulta enviada
    { TODO : Se debe implementar el procesamiento de linSqlT, cuando se use SetOut() }
    procedure SQLPlusCon_FirstReady(prompt: string; pIni: TPoint;
      HeightScr: integer);
    procedure SQLPlusCon_GetPrompt(prmLine: string; pIni: TPoint; HeightScr: integer);
    //control de salida
    procedure SQLPlusCon_ReadData(nDat: integer; const lastLin: string);
    procedure SQLPlusCon_LineCompleted(const lin: string);
    procedure SQLPlusCon_LinePartial(nDat: integer; const lastLin: string);
    procedure SQLPlusCon_AddLine(HeightScr: integer);
    procedure SQLPlusCon_InitScreen(const grilla: TtsGrid; fIni, fFin: integer);
    procedure SQLPlusCon_RefreshLine(const grilla: TtsGrid; fIni, HeightScr: integer);
    procedure SQLPlusCon_RefreshLines(const grilla: TtsGrid; fIni, fFin,
      HeightScr: integer);
  public
    curSigPrm  : boolean;   //Indica si el cursor seguirá al texto que llega
    HayError   : boolean;   //Indica que se detectó un error en la última exploración
    cadError   : string;    //guarda el mensaje de error
    pErr       : TPoint;    //posición del error;
    outHL      : TSQLplusHighligh;  //Resaltador de salida
    OnQueryEnd : procedure of object;   //Fin de la consulta
    OnErrorConx: TsqEvError;   //Error en la conexión
    OnErrorSQL : TsqEvError;   //Error en la consulta
    OnLinePartial: TEvReadData; //Reemplazo de OnReaddata
    edSal      : TSynEdit;   {Editor donde se mostrará la salida. Se hace público para
                              poder redireccionar la salida}
    LinPartial  : boolean;  //para manejar al editor de salida
    property maxLinTer: integer read FmaxLinTer write SetmaxLinTer;
    procedure Open;        //inicia proceso
    function Closed: boolean;
    procedure Init(ConnectStatus: TStatusPanel; fcConOra0: TfraCfgConOra);
    function CurrentCon: TConOra;   //conexión actual
    //manejo del editor de salida
    procedure SetOut(OutEdit: TSynEdit; CursorPan: TStatusPanel;
      Highlight: boolean = true);
    procedure SetOutVT100(OutEdit: TSynEdit; CursorPan: TStatusPanel;
      Highlight: boolean = true);
    procedure EnableOut;  //habilita salida al editor
    procedure EnableOutVT100;
//    procedure DisableOut; //deshabilita salida al editor
//    procedure DisableOutVT100;
    procedure DisableAllOut;   //Desconecta de todas las salidas que pudiera tener
    procedure DisableOutEdit;
    function TakeConnection: boolean;  //Permite tomar la conexión para usarla
    procedure SendSQL(txt: string);   //Envía consulta al SQLPLUS
    procedure ClearScreen;
    procedure SetLanguage(lang: string);
    constructor Create;  //Constructor
    destructor Destroy; override;
  end;

implementation
const
  MIN_LIN_TER = 200;  //nunca debe ser menor que BLOCK_DEL + term.Height
  BLOCK_DEL   = 100;  //cantidad de líneas a borrar en bloque

{ TSQLPlusCon }

procedure TSQLPlusCon.Init(ConnectStatus: TStatusPanel; fcConOra0: TfraCfgConOra);
//Configura la conexión, fijando el panel de estado, y el frame con las conexiónes.
begin
  panel := ConnectStatus;
  if panel<> nil then
    panel.Style:=psOwnerDraw;  //configura panel para dibujarse por evento
  fcConOra := fcConOra0;  //guarda referecnia
  OnFirstReady:=@SQLPlusCon_FirstReady;
  OnGetPrompt:=@SQLPlusCon_GetPrompt;
  OnReadData:=@SQLPlusCon_ReadData;
  ClearOnOpen:=false;  //No limpiaremos en cada conexión
  ClearScreen;   //limpiamos solo ahora
end;
function TSQLPlusCon.CurrentCon: TConOra;
//Devuelve referecnia a la conexión actual. Si no se ha definido ninguna, devuelve
//una conexión con nombre vacío.
begin
  if fcConOra=nil then
    Result.Nombre:=''
  else
    Result := fcConOra.ConexActual;
end;

//manejo del editor de salida
procedure TSQLPlusCon.SetOut(OutEdit: TSynEdit; CursorPan: TStatusPanel;
                              Highlight: boolean);
//Configura la salida del proceso indicando un editor de salida, y un panel para el cursor.
//Al editor indicado, se le aplica una configuración predeterminada simple. Opcionalmente
//se puede activar el resaltador de sintaxis por defecto.
//Debe llamarse después de llamar a Init().
begin
  if OutEdit= nil then exit;
  edSal := OutEdit;
  curPanel := CursorPan;
  //configura editor de salida
  if Highlight then begin
    edSal.Highlighter := outHL;
    outHL.detecPrompt:=true;
  end;
  edSal.ReadOnly:=true;   //no se espera modificar la salida
//  InicEditorC1(edSal);  requeriría SynFacilUtils
  edSal.Options := edSal.Options - [eoKeepCaretX];  //Quita límite horizontal al cursor
  //para actualizar posición de cursor
  edSal.OnMouseDown:=@ed_MouseDown;
  edSal.OnCommandProcessed:=@ed_CommandProcessed;
  EnableOut;   //habilita la salida al editor
  LinPartial := false;   //inicia estado
end;
procedure TSQLPlusCon.SetOutVT100(OutEdit: TSynEdit; CursorPan: TStatusPanel;
  Highlight: boolean);
//Similar a SetOut(), pero controla la salida como si fuera un terminal VT100.
begin
  if OutEdit= nil then exit;
  edSal := OutEdit;
  curPanel := CursorPan;
  //configura editor de salida
  if Highlight then begin
    edSal.Highlighter := outHL;
    outHL.detecPrompt:=true;
  end;
  edSal.ReadOnly:=true;   //no se espera modificar la salida
//  InicEditorC1(edSal);  requeriría SynFacilUtils
  edSal.Options := edSal.Options - [eoKeepCaretX];  //Quita límite horizontal al cursor
  //para actualizar posición de cursor
  edSal.OnMouseDown:=@ed_MouseDown;
  edSal.OnCommandProcessed:=@ed_CommandProcessed;
  EnableOutVT100;   //habilita la salida al editor
  ClearOnOpen:=false;  //No limpiaremos en cada conexión
  ClearScreen;   //limpiamos solo ahora, para sincronizar al editor
end;
procedure TSQLPlusCon.EnableOut;
begin
  //Engancha los eventos de salida para control de salida en el editor configurado.
  //Usa el método de captura de línea por línea
  OnLineCompleted:=@SQLPlusCon_LineCompleted;
  OnLinePartial:=@SQLPlusCon_LinePartial;
end;
procedure TSQLPlusCon.EnableOutVT100;
begin
  //Engancha los eventos de salida para control de salida en el editor configurado.
  //Usa el método de captura como Terminal VT100
  OnInitScreen:=@SQLPlusCon_InitScreen;
  OnRefreshLine:=@SQLPlusCon_RefreshLine;
  OnRefreshLines:=@SQLPlusCon_RefreshLines;
  OnAddLine:=@SQLPlusCon_AddLine;
end;
{procedure TSQLPlusCon.DisableOut;
begin
  OnLineCompleted:=nil;
  OnLinePartial:=nil;
end;
procedure TSQLPlusCon.DisableOutVT100;
begin
  OnInitScreen:=nil;
  OnRefreshLine:=nil;
  OnRefreshLines:=nil;
  OnAddLine:=nil;
end;}
procedure TSQLPlusCon.DisableAllOut;
//Deshabilita todos los eventos de salida de datos y quita las referecnias
//a cualquier editor de salida que se haya configurado.
begin
  OnLineCompleted:=nil;
  OnLinePartial :=nil;
  OnLinePrompt  :=nil;

  OnInitScreen  :=nil;
  OnRefreshLine :=nil;
  OnRefreshLines:=nil;
  OnAddLine     :=nil;
  if edSal<> nil then  begin
    edSal.OnMouseDown:=nil;
    edSal.OnCommandProcessed:=nil;
    edSal := nil;
  end;
end;
procedure TSQLPlusCon.DisableOutEdit;
//Colorea el editor de salida de modo que de la apariencia de que está desconectado
var
  colFon: TColor;
  colTxt: TColor;
begin
  //lo pinta para que parezca deshabilitado
  colFon := clMenu;
  colTxt := TColor($909090);
  edSal.Color:=colFon;
  edSal.Gutter.Color:=colFon;  //color de fondo del panel
  edSal.Gutter.Parts[1].MarkupInfo.Background:=colFon; //fondo del núemro de línea
  edSal.Gutter.Parts[1].MarkupInfo.Foreground:=colTxt; //texto del número de línea
  edSal.Font.Color:=colTxt;      //color de texto normal
end;
function TSQLPlusCon.TakeConnection: boolean;
//Verifica si la conexión está disponible para ser usada por algún proceso. De ser así
//elimina cualquier salida que pudiera tener configurada y devuleve TRUE.
//Si la conexión estuviera ocupada, devuelve FALSE.
begin
  Result := true;  //por defecto
  case State of
  ECO_BUSY: begin
      MsgExc('Hay una consulta en proceso');
      exit(false);
    end;
  ECO_CONNECTING: begin
      MsgExc('Conexión pendiente');
      exit(false);
    end;
  ECO_STOPPED, ECO_ERROR_CON, ECO_READY: begin
    //En estos casos asumimos que podemos tomar la conexión
    end;
  end;
  DisableAllOut;  //desengancha cualquier salida que pueda tener
end;

procedure TSQLPlusCon.ed_CommandProcessed(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
begin
  if curPanel = nil then exit;
  curPanel.Text:= dic('fil=%d, col=%d',[edSal.CaretY, edSal.CaretX]);
end;
procedure TSQLPlusCon.ed_MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if curPanel = nil then exit;
  curPanel.Text:= dic('fil=%d, col=%d',[edSal.CaretY, edSal.CaretX]);
end;
//Control de salida
procedure TSQLPlusCon.SQLPlusCon_LineCompleted(const lin: string);
begin
  if LinPartial then begin
    //Estamos en la línea del prompt
    edSal.Lines[edSal.Lines.Count-1] := lin;  //reemplaza última línea
    LinPartial := false;
  end else begin  //caso común
    edSal.Lines.Add(lin);
    PosicionarCursor;
  end;
end;
procedure TSQLPlusCon.SQLPlusCon_LinePartial(nDat: integer;
  const lastLin: string);
begin
  LinPartial := true;   //marca bandera
  edSal.Lines.Add(lastLin);   //agrega la línea que contiene al prompt
  PosicionarCursor;
end;
procedure TSQLPlusCon.SQLPlusCon_InitScreen(const grilla: TtsGrid; fIni, fFin: integer);
var
  i: Integer;
begin
//debugln('==InitLines');
  for i:=fIni to fFin do
    edSal.Lines.Add(grilla[i]);
end;
procedure TSQLPlusCon.SQLPlusCon_RefreshLine(const grilla: TtsGrid; fIni, HeightScr: integer);
var
  yvt: Integer;
begin
//  debugln('procRefreshLine: '+IntToStr(fIni));
  yvt := edSal.Lines.Count-HeightScr-1;
  edSal.Lines[yvt+fIni] := grilla[fIni];
  //Llamamos a ProcessMessages para pòder refrescar la aplicación cuando los datos
  //llegan muy seguido.
//  Application.ProcessMessages; //PRODUCE que se desordenen las línesa de salida
end;
procedure TSQLPlusCon.SQLPlusCon_RefreshLines(const grilla: TtsGrid; fIni, fFin,
  HeightScr: integer);
var
  yvt: Integer;
  f: Integer;
begin
//  debugln('procRefreshLines: '+IntToStr(fIni)+','+IntToSTr(fFin));
  yvt := edSal.Lines.Count-HeightScr-1;  //calcula fila equivalente a inicio de VT100
  edSal.BeginUpdate;
  for f:=fIni to fFin do
    edSal.Lines[yvt+ f] := grilla[f];
  PosicionarCursor(HeightScr);
  edSal.EndUpdate;
  edSal.Refresh;  //para mostrar el cambio
end;
procedure TSQLPlusCon.SQLPlusCon_AddLine(HeightScr: integer);
var
  i: Integer;
begin
  edSal.BeginUpdate();
  if edSal.Lines.Count > FmaxLinTer then begin
    //hace espacio
    for i:= 1 to BLOCK_DEL do
      edSal.Lines.Delete(0);
    //actualiza linSql
//    if linSql> 0 then  //para que de la diferencia
    linSql := linSql - BLOCK_DEL;
  end;
  edSal.Lines.Add('');
  edSal.EndUpdate;
  //actualiza linSqlT
  linSqlT := linSqlT - 1;
end;

procedure TSQLPlusCon.SendSQL(txt: string);
var
  yvt: Integer;
begin
  //captura posición del prompt en el editor
  if edSal = nil then begin
    linSql := -1;  //no accesible
  end else begin
    yvt := edSal.Lines.Count- term.height-1;
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
  if edSal<> nil then edSal.ClearAll;
  ClearTerminal;  //generará el evento OnInitScreen()
end;

procedure TSQLPlusCon.BuscarErrorEnLineas;
//Busca en las últimas líneas, para ver si hay un mensaje de error
//Solo explora desde la posición del prompt anterior, o desde que se inicia
//la sesión.
//Actualiza las banderas "HayError" y "cadError"
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
    end else if HaySP2(i)>0 then begin //Busca mensajes de tipo SP2-????
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
    end else if AnsiStartsStr('Disconnected from Oracle', linea) then begin
      //se desconectó, ya no vale seguir buscando atrás
      break;
    end else if AnsiStartsStr('SQL*Plus: Release', linea) then begin
      //se inició una nueva conexión, ya no vale seguir buscando atrás
      break;
    end;
  end;
end;
constructor TSQLPlusCon.Create;
//"PanControl" es el panel en donde se indicará el estado de la conexión
begin
  inherited Create(nil);
  outHL := TSQLplusHighligh.Create(nil);
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
destructor TSQLPlusCon.Destroy;
begin
  outHL.Destroy;
  inherited;
end;

procedure TSQLPlusCon.PosicionarCursor; inline;
//Coloca el cursor del editor en la última línea.
var
  yvt: Integer;
begin
  if curSigPrm then begin
    yvt := edSal.Lines.Count;  //calcula última fila
    edSal.CaretXY := Point(1, yvt);  //siempre al inicio
  end;
end;
procedure TSQLPlusCon.PosicionarCursor(HeightScr: integer);
//Coloca el cursor del editor, en la misma línea que tiene el cursor del
//terminal VT100 virtual.
var
  yvt: Integer;
begin
  if curSigPrm then begin
    yvt := edSal.Lines.Count-HeightScr-1;  //calcula fila equivalente a inicio de VT100
    edSal.CaretXY := Point(1, yvt+term.CurY+1);  //siempre al inicio
  end;
end;
procedure TSQLPlusCon.SetmaxLinTer(AValue: integer);
begin
  if FmaxLinTer=AValue then Exit;
  if FmaxLinTer<MIN_LIN_TER then exit;  //protección
  FmaxLinTer:=AValue;
end;

procedure TSQLPlusCon.SQLPlusCon_FirstReady(prompt: string; pIni: TPoint;
  HeightScr: integer);
//Se produjo la primer oonexión
begin
  state := ECO_READY;  //para que pase a ECO_BUSY
  SendLn(COMAN_INIC);
end;
procedure TSQLPlusCon.SQLPlusCon_GetPrompt(prmLine: string; pIni: TPoint;
  HeightScr: integer);
begin
//debugln('-GetPrompt');
//debugln('promp ant:'+IntToStr(linSqlT));
  BuscarErrorEnLineas;  //busca si hubo algún mesaje de error
  if HayError then begin
    if OnErrorSQL <> nil then OnErrorSQL(PErr,cadError);  //error en la consulta
  end;
  if OnQueryEnd<>nil then OnQueryEnd;
end;
procedure TSQLPlusCon.SQLPlusCon_ReadData(nDat: integer; const lastLin: string);
begin
//debugln('   ReadData:'+IntToStr(nDat));
  if OnLinePartial<>nil then OnLinePartial(nDat, lastLin);
  //Se aprovecha para verificar si hay mensajes de error
  if state = ECO_CONNECTING then begin
    //Estamos esperando una conexión. Hay que buscar errores de conexión
    BuscarErrorEnLineas;    //busca si hubo algún mesaje de error
    if HayError and (OnErrorConx <> nil) then OnErrorConx(Point(0,0), cadError);
  end;
end;
procedure TSQLPlusCon.Open;
//Abre una conexión al SQLPLUS, con la conexión actual. La conexión actual se lee
//de 'fcConOra', que se debe haber definido con Init().
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
    cadError := dic('No se ha especificado la conexión actual.');
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

procedure TSQLPlusCon.SetLanguage(lang: string);
begin
  case lowerCase(lang) of
  'es': begin
     dicClear;   //los mensajes están en español
    end;
  'en': begin
      dicSet('(Línea: %d, Columna: %d)','(Row: %d, Column: %d)');
      dicSet('fil=%d, col=%d','row=%d, col=%d');
      dicSet('No se ha especificado la conexión actual.','Current connection not specified.')
    end;
  end;
end;

end.

