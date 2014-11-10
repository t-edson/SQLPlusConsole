{
FrameExplorBD 0.5
===================
Por Tito Hinostroza 11/10/2014
* Se cambia el nombre de TEstadoNodo a TsqNodeStatus
* Se cambia el nombre de TTipoNodo a TsqNodeType
* Se cambia el nombre de TBDNodo.codigo a TBDNodo.source a TsqNodeType
* Se modifica qLlegoPrompt(), para que agregue información adicional a los nodos
de tipo Tabla y Vista.
* Se cambia nombre de qLLegoLineasC(), qErrorConex() y qLlegoPrompt().
* Se incluye la unidad FrameSQLPlusOut, em la sección USES, para poder manejar
la salida en un frame "TfraSQLPlusOut".
* Se configura para poder usar la conexión del explorador, para lanzar otras
consultas adicionales a las que lanza el mismo explorador.
* Se modifica sqlCon_LlegoPrompt(), para que agregue información adicional a los
nodos de tipo tabla o vista.
* Se cambian lo síconos de procedimiento y función
* Se reordena el código.
* Se cambia de nombre a TfraExplorBD.Iniciar() por TfraExplorBD.SetConnection() y se
simplifican sus parámetros.
* Se elimina TfraExplorBD.DrawStatePanel(), porque la función de dibujar el estado,
ya no es de este TfraExplorBD.
* Se cambia el nombre de ActualizNodo() a UpdateNode().

Descripción
===========
Frame para implementar un explorador de objetos de una base de datos Oracle, usando
la unidad SQLPlusConsole.
Este explorador se implementa en un frame para poder isnertarlo fácilmente en un
formulario cualquiera. Para mostrar el árbol, se usa un control TTreeView.
Las acciones a realizar con los objetos del arbol, se deben implementar aparte. Esta
unidad no incluye ningún menú contextual.
Para el manejo de los nodos se usa la clase TBDnodo, en lugar del nodo TTreeNode, que
usa comúnmente TTreeView.
Los tipos de nodo que se muestran se identifican con el enumerado TsqNodeType.
}
unit FrameExplorBD;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Forms, Controls, ComCtrls, StdCtrls, LCLProc, SynEdit,
  MisUtils, UnTerminal, SQLPlusConsole, SQLPlusParser,
  FormVentSesion;

type
  //Estados de nodo
  TsqNodeStatus = (enNoInic,    //Creado pero no actualizado
                  enLeyendo,   //Esperando a que aparezcan resultados
                  enEsperand,  //Están llegando datos
                  enErrConex,  //hubo error de conexión
                  enLleno,     //actualizado con datos
                  enLlenoErr,  //actualizado con datos
                  enSinDatos   //actualizado pero sin datos
                  );
  //Tipos de nodo
  TsqNodeType = (tnDescon,      //desconocido

                tnListTablas,  //lista de tablas
                tnTabla,       //tabla
                tnTabListCampo, //lista de campo de tabla
                tnTabCampo,     //campo de tabla
                tnTabListIndic, //lista de índices de tabla
                tnTabIndic,     //índice de tabla

                tnListVistas,  //lista de vistas
                tnVista,       //actualizado pero sin datos
                tnVisListCampo,//Lista de campos de uan vista
                tnVisCampo,    //campo de una vista
                tnVisDefinic,  //Definición de la vista

                tnListIndic,   //lista de índices
                tnIndice,      //Índice
                tnListProced,  //lista de procedimientos
                tnProced,      //procedimiento
                tnListFuncio,  //lista de funciones
                tnFuncio,      //función
                tnListDBlnks,  //lsita de DB links
                tnDBlnk,       //DB link
                tnListEsquem,  //lista de esquemas
                tnEsquem,      //esquema
                tnListUsuar,   //lista de usuarios
                tnUsuar,       //usuario
                tnListTabSpa,  //lista de Tablespaces
                tnTabSpa,       //Tablespace
                tnListProces,  //lista de Procesos
                tnProces       //Proceso
                );

  { TBDNodo }

  TBDNodo = class(TTreeNode)  //tipo de nodo personalizado
    private
    public
      tipNod: TsqNodeType;   //tipo de nodo
      estado: TsqNodeStatus; //estado del nodo
      user  : string;        //para cuando el nodo pertenezca a un usuario (para el usuario actual, dejar en blanco)
      sql   : string;        //consulta ejecutada para llenar el nodo.
      dat   : string;        //fila de datos, cuando se haya definido que el nodo trabaje así
      source: string;        {Código fuente de procedimiento o función. Para los nodos Tabla y Vista, aquí
                             se guarda información de los campos de la tabla y de los índices}
      campos: TCamposSqlPlus;  {Encabezados de la información del nodo. Necesario para extraer la información
                               del campo "dat" de los nodos hijos. En los nodos tabla y vista funcionan de modo
                               distinto.}
//      valores: TstringList;  //filas de datos
      function nombre: string; //devuelve el nombre del nodo
      function RutaEs(cad: string): boolean;  //compara con una ruta
    end;

  TOnNodSelec = procedure(nod: TBDNodo) of object;
  TOnNodMouse = procedure(nod: TBDNodo; Button: TMouseButton;
                          Shift: TShiftState) of object;

  { TfraExplorBD }

  TfraExplorBD = class(TFrame)
    ComboBox1: TComboBox;
    ImageList1: TImageList;
    TreeView1: TTreeView;
    //Eventos de la conexión
    procedure sqlCon_ErrorSQL(CurXY: TPoint; const msg: string);
    procedure sqlCon_LineaCompleta(const txt: string);
    procedure sqlCon_ErrorConex(CurXY: TPoint; const msg: string);
    procedure sqlCon_QueryEnd;
    //Eventos de TreeView1
    procedure TreeView1CreateNodeClass(Sender: TCustomTreeView;
      var NodeClass: TTreeNodeClass);
    procedure TreeView1Expanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure TreeView1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TreeView1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TreeView1SelectionChanged(Sender: TObject);
    procedure TreeView1Click(Sender: TObject);
    procedure TreeView1DblClick(Sender: TObject);
    constructor Create(AOwner: TComponent) ; override;
    destructor Destroy; override;
  private
    c       : TConvSqlPlus;    //convertidor de texto
    VentSes : TfrmVentSesion;  //ventana de sesión
    txtSentEspera: String;
    nodAct: TBDnodo;  //Nodo que recibe datos de la consulta actual
    FOnChangeState: TEvRecSysComm;
    sqlCon  : TSQLPlusCon;   //conexión Oracle
    procedure CreaEstructuraEsquema(nodEsq: TBDNodo; user: string);
    procedure FijarNodoActualSQL(Node: TTreeNode; ForzarExpan: boolean=true);
    procedure FijEstadoNodo(n: TBDnodo; estado0: TsqNodeStatus; ForzarExpan: boolean=
      true);
    procedure InicEstructura;
    function LanzarSentencia(sql: string; Node: TTreeNode; ForzarExpan: boolean
      ): boolean;
    procedure LLenarNodoActual(const cad: string; tipNod: TsqNodeType; sql: string);
    function NuevoNodo(nod: TBDNodo; txt: String; tipNod0: TsqNodeType; idIco: integer
      ): TBDnodo;
    procedure PonerMensNodo(nod: TBDnodo; n: integer);
    procedure PonerMensNodo(nod: TBDnodo; txt: string);
    procedure SentenciaEnEspera;
    procedure SetOnChangeState(AValue: TEvRecSysComm);
    function TakeConnection: boolean;
  public
    //eventos para permitir personalizar las respuestas
    OnMouseUpNod : TOnNodMouse;  //MouseUp sobre un nodo
    OnNodSelec   : TOnNodSelec;  //Selecciona un nodo
    OnNodClick   : TOnNodSelec;  //Click sobre nodo
    OnNodUpdate  : TOnNodSelec;  //Un nodo se ha actualizado
    OnDblClickNod: TOnNodSelec;  //DOble Click sobre un nodo
    //Eventos adicionales
    OnLineCompleted: TEvLinCompleted;  //Evento de línea completa recibida
    OnQueryEnd : procedure of object;  //Evento de fin de la consulta

    property OnChangeState: TEvRecSysComm read FOnChangeState write SetOnChangeState;
    //funciones generales
    procedure SetConnection(sqlCon0: TSQLPlusCon);  //inicia
    function NodSelec: TBDNodo;  //nodo selecionado
    function UpdateNode(nod: TBDNodo): boolean;  //actualiza el contenido del nodo
    procedure MostVentanaSesion;
    procedure OculVentanaSesion;
    //reflejo de las funciones de sqlCon
//    procedure Open;
//    procedure Close;
//    procedure SendSQL(txt: string);
    procedure SetLanguage(lang: string);
  end;

implementation
{$R *.lfm}
const
  MAX_LIN_TER = 1000;  //cantidad de líneas que se guardará en la ventana de salida
  //mensajes de control para los nodos. Deben empezar con "<" para ser reconocidos.
  MSJ_CONECTANDO = '<Conectando>';
  MSJ_LEYENDO    = '<Leyendo>';
  MSJ_ERORCONEX  = '<Error de conexión>';
  MSJ_EROR_SQL   = '<Error en consulta>';
  MSJ_SINDATOS   = '<Sin datos>';
var
  nEsqActual, nEsqOtros, nEsqTodos, nUsuar, nTabSpa : TBDNodo;

{ TBDNodo }

function TBDNodo.nombre: string;
//devuelve el nombre del nodo. Qiita el mensaje adicional que pueda contener
var i: integer;
begin
  i := Pos('(', Text);
  if i<>0 then  //hay mensaje en el nombre
    Result := Copy(Text,1,i-2)  //no considera el mensaje
  else  //no hay mensaje
    Result := Text;
  //verifica si tiene información de usuario
  if user <> '' then Result := user + '.' + Result;
end;
function TBDNodo.RutaEs(cad: string): boolean;
var i: integer;
    path: string;
begin
  path := GetTextPath;
  i := Pos('(', path);
  if i<>0 then  //hay mensaje en el nombre
    path := trim(Copy(path,1,i-2));  //no considera el mensaje
  if UpCase(path) = UpCase(cad)  then
    exit(true)
  else
    exit(false);
end;

{ TfraExplorBD }

function TfraExplorBD.NodSelec: TBDNodo;  //Devuelve el nodo seleccionado
begin
  if TreeView1.Selected = nil then
    Result := nil
  else
    Result :=TBDnodo(TreeView1.Selected);
end;

procedure TfraExplorBD.FijEstadoNodo(n: TBDnodo; estado0: TsqNodeStatus;
                                ForzarExpan: boolean = true);
{Fija el estado de un nodo, configurando su contenido si es necesario. NO geenra el
 evento OnExpanding(). Si ForzarExpan = TRUE, fuerza la expansión de otra forma, mantiene
 el estado de expansión .}
var
  nod : TTreeNode;
  evTmp: TTVExpandingEvent;
  exTmp: boolean;
begin
  if n = nil then exit;
  if n.estado = estado0 then exit;  //no hay cambio

  evTmp := TreeView1.OnExpanding; //guarda evento
  exTmp := n.Expanded;            //guarda estado
  TreeView1.OnExpanding:=nil;     //deshabilita evento
  TreeView1.BeginUpdate;
  //Cambia estado
  case estado0 of
  enNoInic:begin  //sin inicializar
    if n.Count>0 then n.DeleteChildren;
    n.estado := estado0;  //cambia su estado
  end;
  enLleno: begin  //llegaron datos sin problema
    n.estado := estado0;  //cambia su estado
  end;
  enLlenoErr: begin  //llegaron datos con problemas
    if n.estado = enLeyendo then n.DeleteChildren;
    nod:= TreeView1.Items.AddChild(n,MSJ_EROR_SQL);  //agrega un nodo
//    nod.ImageIndex:=11;                 //e íconos
//    nod.SelectedIndex:=11;
    n.estado := estado0;  //cambia su estado
  end;
  enLeyendo:begin  //está leyendo datos
    if n.tipNod in [tnVista,tnProced,tnFuncio,tnTabla] then begin
      PonerMensNodo(n, dic('Leyendo...')); //no agrega nodo
    end else begin  //otro tipo de nodo
      if n.Count>0 then n.DeleteChildren;
      nod:= TreeView1.Items.AddChild(n,MSJ_LEYENDO);  //agrega un nodo
      nod.ImageIndex:=11;                 //e íconos
      nod.SelectedIndex:=11;
    end;
    n.estado := estado0;  //cambia su estado
  end;
  enEsperand:begin
    n.estado := estado0;  //cambia su estado
  end;
  enErrConex:begin
    if n.Count>0 then n.DeleteChildren;
    nod:= TreeView1.Items.AddChild(n,MSJ_ERORCONEX);  //agrega un nodo
    nod.ImageIndex:=10;                 //e íconos
    nod.SelectedIndex:=10;
    n.estado := estado0;  //cambia su estado
  end;
  enSinDatos:begin
    if n.Count>0 then n.DeleteChildren;
    nod:= TreeView1.Items.AddChild(n,MSJ_SINDATOS);  //agrega un nodo
//    nod.ImageIndex:=11;                 //e íconos
//    nod.SelectedIndex:=11;
    n.estado := estado0;  //cambia su estado
  end;
  end;
  //termina
  if ForzarExpan then
    n.Expand(false)      //fuerza expansión
  else
    n.Expanded:=exTmp;   //mantiene estado anterior
  TreeView1.EndUpdate;           //termina actualización
  TreeView1.OnExpanding:=evTmp;  //restablece evento
end;
function TfraExplorBD.NuevoNodo(nod: TBDNodo; txt: String; tipNod0: TsqNodeType; idIco: integer): TBDnodo;
//Agrega un nodo a TreeView1, y coloca su índice de ícono. Devuelve un TBDNodo, en lugar
//de TTreeNode. Pone además HasChildren en TRUE y pone su estado en "enNoInic".
begin
  Result:= TBDNodo(TreeView1.Items.AddChild(nod,txt));
  Result.tipNod:=tipNod0;
  Result.ImageIndex:=idIco;
  Result.SelectedIndex:=idIco;
  Result.HasChildren:=true;
//  Result.user:=nod.user;  //no conviene generalizar
  FijEstadoNodo(Result, enNoInic);  //lo crea como nodo vacío
end;
constructor TfraExplorBD.Create(AOwner: TComponent);  //
begin
  inherited Create(AOwner);
  TreeView1.Images:=ImageList1;   //asigna íconos
  TreeView1.Options:= TreeView1.Options + [tvoReadOnly];  //para que no permita editar el nodo
  TreeView1.OnCreateNodeClass:=@TreeView1CreateNodeClass;
  InicEstructura;            //Crea la estructura inicial

  ventSes := TfrmVentSesion.Create(nil);  //ventana de salida
  c := TConvSqlPlus.Create;  //crea conversor
end;
destructor TfraExplorBD.Destroy;
begin
  c.Free;  //libera objeto
  ventSes.Free;
  inherited Destroy;
end;
procedure TfraExplorBD.SetOnChangeState(AValue: TEvRecSysComm);
begin
  if FOnChangeState=AValue then Exit;
  FOnChangeState:=AValue;
  sqlCon.OnChangeState:=FOnChangeState;  //configura evento
  //lanza para actualizar al primera vez
  if FOnChangeState<>nil then FOnChangeState('',Point(0,0));
end;
function TfraExplorBD.TakeConnection: boolean;
//Prepara la conexión configurada para usarla aquí. Se tiene el supuesto de que dicha
//conexión puede estar siendo usada por otros objetos.
//Si no la puede usar genera un mensaje de error y devuelve FALSE.
begin
  Result := true;  //por defecto
  if not sqlCon.TakeConnection then exit(false);  //toma si está disponible
  //la conexión está disponible para usarla
  sqlCon.maxLinTer := MAX_LIN_TER;  //fija límite de líneas
  //toma control de los eventos
  sqlCon.OnErrorConx:=@sqlCon_ErrorConex;
  sqlCon.OnErrorSQL:=@sqlCon_ErrorSQL;
  sqlCon.OnQueryEnd :=@sqlCon_QueryEnd;
  //configura eventos de salida de datos
  sqlCon.OnLineCompleted:=@sqlCon_LineaCompleta;
  //Aprovechamos los eventos adicionales para controlar a nuestro visor
  //La desventaja de trabajar con SetOutVT100(), es que siempre limpiará la salida,
  //cuando se tome la conexión.
  sqlCon.SetOutVT100(ventSes.edSal, nil, false);
end;
procedure TfraExplorBD.SetConnection(sqlCon0: TSQLPlusCon);
//Define la conexión que debe usar para pedir datos.
begin
//  InicEstructura;
//  if ConexIgual(cnx, fcConOra.ConexActual) then exit; //no hay cambio
  sqlCon := sqlCon0;
  OculVentanaSesion; //cierra ventana de sesión por si estaba abierta
end;
procedure TfraExplorBD.CreaEstructuraEsquema(nodEsq: TBDNodo; user: string);
//Recrea la estructura del TreeView para un nodo de tipo esquema
var
  nod: TBDNodo;
begin
  if user = '' then begin  //es el usuario actual
    nod:= NuevoNodo(nodEsq,dic('Tablas'), tnListTablas, 7);
    nod.user:=user;  //pasa al usuario
    nod.sql:='SELECT TABLE_NAME, ''      '' "OWNER", NUM_ROWS, TABLESPACE_NAME' +LineEnding+
             'FROM USER_TABLES ORDER BY 1;';
    nod := NuevoNodo(nodEsq,dic('Vistas'), tnListVistas,6);
    nod.user:=user;  //pasa al usuario
    nod.sql:='SELECT VIEW_NAME , ''      '' "OWNER", TEXT_LENGTH, TEXT' + LineEnding +
             'FROM USER_VIEWS ORDER BY 1;';
    nod := NuevoNodo(nodEsq,dic('Índices'), tnListIndic,5);
    nod.user:=user;  //pasa al usuario
    nod.sql:='SELECT INDEX_NAME, ''      '' "OWNER", TABLE_NAME, UNIQUENESS, TABLESPACE_NAME' + LineEnding +
             'FROM USER_INDEXES ORDER BY 1;';
    nod := NuevoNodo(nodEsq,dic('Procedimientos'), tnListProced,4);
    nod.user:=user;  //pasa al usuario
    nod.sql:='select OBJECT_NAME, OBJECT_ID, CREATED, LAST_DDL_TIME, STATUS, TEMPORARY'#13#10+
             'from user_objects where OBJECT_TYPE=''PROCEDURE'' ORDER BY 1;';
    nod := NuevoNodo(nodEsq,dic('Funciones'), tnListFuncio,3);
    nod.user:=user;  //pasa al usuario
    nod.sql:='select OBJECT_NAME, OBJECT_ID, CREATED, LAST_DDL_TIME, STATUS, TEMPORARY'#13#10+
             'from user_objects where OBJECT_TYPE=''FUNCTION'' ORDER BY 1;';
    nod := NuevoNodo(nodEsq,dic('Enlaces a BD'), tnListDBlnks,2);
    nod.user:=user;  //pasa al usuario
    nod.sql:='SELECT DB_LINK, USERNAME, PASSWORD, HOST, CREATED' + LineEnding +
             'FROM USER_DB_LINKS ORDER BY 1;';
    nodEsq.Expand(false);
  end else begin  //se especifica otro usuario

  end;
end;
procedure TfraExplorBD.InicEstructura;  //Recrea la estructura del TreeView
var nod : TBDnodo;
begin
  TreeView1.BeginUpdate;
  TreeView1.Items.Clear;  //inicia
  ////////////// ESQUEMA ACTUAL ///////////////////
  nEsqActual := NuevoNodo(nil,dic('Esquema Actual'),tnEsquem, 8);
  nEsqActual.user:='';  //indica que es el usuario actual
  CreaEstructuraEsquema(nEsqActual,'');
  ////////////// OTROS ESQUEMAS ///////////////////
  nEsqOtros := NuevoNodo(nil,dic('Otros Esquemas'), tnListEsquem, 8);
  nEsqOtros.sql:='SELECT * FROM all_users;'; //Se requiere privilegios DBA.

  //////////// TODOS LOS ESQUEMAS ///////////////////
  nEsqTodos := NuevoNodo(nil,dic('Todos los Esquemas'),tnEsquem, 8);
  nod:= NuevoNodo(nEsqTodos,dic('Tablas'),tnListTablas,7);
  nod.sql:='SELECT TABLE_NAME, OWNER, NUM_ROWS, TABLESPACE_NAME FROM ALL_TABLES ORDER BY 1;';
  nod := NuevoNodo(nEsqTodos,dic('Vistas'),tnListVistas,6);
  nod.sql:='SELECT VIEW_NAME, OWNER, TEXT_LENGTH, TEXT FROM ALL_VIEWS ORDER BY 1;';
  nod := NuevoNodo(nEsqTodos,dic('Índices'),tnListIndic,5);
  nod.sql:='SELECT INDEX_NAME, OWNER, TABLE_NAME, UNIQUENESS, TABLESPACE_NAME FROM ALL_INDEXES ORDER BY 1;';
  nod := NuevoNodo(nEsqTodos,dic('Procedimientos'),tnListProced,4);
  nod.sql:='select OBJECT_NAME, OBJECT_ID, CREATED, LAST_DDL_TIME, STATUS, TEMPORARY'#13#10+
           'from all_objects where OBJECT_TYPE=''PROCEDURE'' ORDER BY 1;';
  nod := NuevoNodo(nEsqTodos,dic('Funciones'),tnListFuncio,3);
  nod.sql:='select OBJECT_NAME, OBJECT_ID, CREATED, LAST_DDL_TIME, STATUS, TEMPORARY'#13#10+
           'from all_objects where OBJECT_TYPE=''FUNCTION'' ORDER BY 1;';
  nod := NuevoNodo(nEsqTodos,dic('Enlaces a BD'),tnListDBlnks,2);
  nod.sql:='SELECT DB_LINK, OWNER, USERNAME, HOST, CREATED FROM ALL_DB_LINKS ORDER BY 1;';

  //////////// OTROS ///////////////////
  nUsuar := NuevoNodo(nil,dic('Usuarios'),tnListUsuar,1);
  {Realmente, debería ser sobre DBA_USERS, pero se requiere privilegios DBA.}
  nUsuar.sql:='SELECT USERNAME, USER_ID, CREATED FROM ALL_USERS;';
  nTabSpa := NuevoNodo(nil,dic('TableSpaces'),tnListTabSpa,0);
  {Se requiere privilegios DBA.}
  nTabSpa.sql:='SELECT a.tablespace_name "NOMBRE", a.TOTAL "TOTAL(Mb)",'#13#10+
'       a.TOTAL-b.LIBRE "USADO(Mb)",'#13#10+
'       to_char(100*(a.TOTAL-b.LIBRE)/a.TOTAL,''999.9'') "  %",'#13#10+
'       RPAD(RPAD(''|'',1+10*(a.TOTAL-b.LIBRE)/a.TOTAL,''#''),11) || ''|'' "BAR"'#13#10+
'FROM'#13#10+
'(SELECT tablespace_name, round(sum(BYTES/1024/1024),0) "TOTAL"'#13#10+
' FROM dba_data_files b'#13#10+
' GROUP BY b.tablespace_name) a,'#13#10+
'(SELECT tablespace_name, ROUND(sum(bytes)/1024/1024,0) "LIBRE"'#13#10+
' FROM dba_free_space'#13#10+
' GROUP BY tablespace_name) b'#13#10+
'WHERE a.tablespace_name = b.tablespace_name ORDER BY 1;';
  nTabSpa := NuevoNodo(nil,'Procesos',tnListProces,0);

  TreeView1.EndUpdate;
end;
procedure TfraExplorBD.PonerMensNodo(nod: TBDnodo; n: integer);
//Coloca información numérica en un nodo, al lado de la etiqueta.
var i: integer;
begin
  i := Pos('(', nod.Text);
  if i<>0 then
    nod.Text := Copy(nod.Text,1,i) + IntTOStr(n) + ')'
  else
    nod.Text := nod.Text + ' (' + IntToStr(n) + ')';
end;
procedure TfraExplorBD.PonerMensNodo(nod: TBDnodo; txt: string);
//Coloca información de texto en un nodo, al lado de la etiqueta.
var i: integer;
begin
  i := Pos('(', nod.Text);
  if i<>0 then
    nod.Text := Copy(nod.Text,1,i) + txt + ')'
  else
    nod.Text := nod.Text + ' (' + txt + ')';
end;
//////////////////Eventos de la conexión ////////////////
procedure TfraExplorBD.sqlCon_LineaCompleta(const txt: string);
//Evento de llegada de datos. Llena al nodo actual
var i: integer;
    cad: string;
    tipNod: TsqNodeType;
    tmp : string;
begin
  if OnLineCompleted<>nil then OnLineCompleted(txt);  //genera evento
  if nodAct = nil then exit;  //no hay nodo actual, se ignora.
  //identifica tipo de nodo hijo
  case nodAct.tipNod of
//  tnListTablas: tipNod:= tnTabla;
//  tnListVistas: tipNod:= tnVista;
//  tnListProced: tipNod:= tnProced;
//  tnListFuncio: tipNod:= tnFuncio;
  tnListIndic : tipNod:= tnIndice;
  tnListDBlnks: tipNod:= tnDBlnk;
  tnListEsquem: tipNod:= tnEsquem;
  tnListUsuar : tipNod:= tnUsuar;
  tnListTabSpa: tipNod:= tnTabSpa;
  tnTabListCampo: tipNod:= tnTabCampo;
  tnTabListIndic: tipNod:= tnTabIndic;
  else
    tipNod := tnDescon;
  end;
  //agrega filas de datos
  TreeView1.BeginUpdate;
  {if nodAct.tipNod = tnListVistas then begin
    //las vistas tienen un campo multilínea
    if nodAct.RutaEs('Todos los Esquemas/Vistas') then tmp := 'ALL_VIEWS' else tmp := 'USER_VIEWS';
    if c.ExploraLinCamBas(txt,2) then{ TODO : Debería identificar también por el campo OWNER para diferenciar }
       LLenarNodoActual(txt, tnVista, 'DECLARE txt long;'+
          'BEGIN  dbms_output.enable(100000);'#13#10+
          'SELECT TEXT INTO txt FROM '+tmp+' where view_name = '''+c.Campo(0,txt)+''';'#13#10+
          'dbms_output.put_line(''.'');'#13#10+
          'dbms_output.put_line(''&%$'');'#13#10+
          'dbms_output.put_line(txt);'#13#10+
          'dbms_output.put_line(''&%$'');'#13#10+
          'END;'#13#10+
          '/');
  end else }if nodAct.tipNod = tnListProced then begin
    //los procedimientos tienen un campo multilínea
    if nodAct.RutaEs('Todos los Esquemas/Procedimientos') then tmp := 'ALL_SOURCE' else tmp := 'USER_SOURCE';
    if c.ExplorarLin(txt) then  { TODO : Debería identificar también por el campo OWNER para diferenciar }
       LLenarNodoActual(txt, tnProced, 'select text from '+tmp+#13#10+
    'where type=''PROCEDURE'' AND name='''+c.Campo(0,txt)+''''#13#10+
    'order by LINE;');
  end else if nodAct.tipNod = tnListFuncio then begin
    //las funciones tienen un campo multilínea
    if nodAct.RutaEs('Todos los Esquemas/Funciones') then tmp := 'ALL_SOURCE' else tmp := 'USER_SOURCE';
    if c.ExplorarLin(txt) then  { TODO : Debería identificar también por el campo OWNER para diferenciar }
       LLenarNodoActual(txt, tnFuncio, 'select text from '+tmp+#13#10+
    'where type=''FUNCTION'' AND name='''+c.Campo(0,txt)+''''#13#10+
    'order by LINE;');
  end else if nodAct.tipNod = tnProced then begin   //los procedimientos vienen en varias líneas
    c.ExplorarAcum(txt); //captura contenido y lo acumula
  end else if nodAct.tipNod = tnFuncio then begin   //las funciones vienen en varias líneas
    c.ExplorarAcum(txt); //captura contenido y lo acumula
  end else if nodAct.tipNod = tnListTablas then begin   //lista de tablas
    if c.ExplorarLin(txt) then begin
       //Esta consulta logra obtener información de la tabla y sus indices, sin necesidad de
       //realizar dos consultas. Su principal objetivo es servir al aplicativo SQGraf
       LLenarNodoActual(txt, tnTabla,'SELECT substr(column_name,1,32) "COLUMN_NAME", data_type, '' '' "INDEX_NAME", column_id' +#13#10+
'FROM user_tab_columns WHERE TABLE_NAME='''+c.Campo(0,txt)+''' UNION ALL'+#13#10+
'SELECT substr(C.COLUMN_NAME,1,32) "COLUMN_NAME", I.UNIQUENESS, C.INDEX_NAME, 1000'+#13#10+
'FROM ALL_INDEXES I, ALL_IND_COLUMNS C'+#13#10+
'WHERE I.INDEX_NAME = C.INDEX_NAME AND I.TABLE_NAME = '''+c.Campo(0,txt)+''''+#13#10+
'ORDER BY 4;');
    end;
  end else if nodAct.tipNod = tnListVistas then begin   //lista de tablas
    //las vistas tienen un campo multilínea
    if c.ExploraLinCamBas(txt,2) then begin { TODO : Debería identificar también por el campo OWNER para diferenciar }
       //Esta consulta logra obtener información de la tabla y sus indices, sin necesidad de
       //realizar dos consultas. Su principal objetivo es servir al aplicativo SQGraf
       LLenarNodoActual(txt, tnVista,'SELECT substr(column_name,1,32) "COLUMN_NAME", data_type, '' '' "INDEX_NAME", column_id' +#13#10+
'FROM user_tab_columns WHERE TABLE_NAME='''+c.Campo(0,txt)+''' UNION ALL'+#13#10+
'SELECT substr(C.COLUMN_NAME,1,32) "COLUMN_NAME", I.UNIQUENESS, C.INDEX_NAME, 1000'+#13#10+
'FROM ALL_INDEXES I, ALL_IND_COLUMNS C'+#13#10+
'WHERE I.INDEX_NAME = C.INDEX_NAME AND I.TABLE_NAME = '''+c.Campo(0,txt)+''''+#13#10+
'ORDER BY 4;');
    end;
  end else if nodAct.tipNod = tnTabla then begin   //lista de tablas
    c.ExplorarAcum(txt); //captura contenido y lo acumula
  end else if nodAct.tipNod = tnVista then begin   //las vistas leen el texto
    c.ExplorarAcum(txt); //captura contenido y lo acumula
  end else if nodAct.tipNod = tnVisDefinic then begin   //la defin. de vistas leen el texto
    c.ExplorarDelim(txt,'&%$'); //busca contenido y lo acumula
  end else begin         //los otros objetos son comunes
    if c.ExplorarLin(txt) then
       LLenarNodoActual(txt, tipNod,'');
  end;
  TreeView1.EndUpdate;
end;
procedure TfraExplorBD.sqlCon_QueryEnd;
var
  lin: String;
  campos: TCamposSqlPlus;
  index_name: String;
  n: Integer;
begin
//debugln('  llegPrompt');
  if nodAct = nil then exit;
  //hay un nodo esperando. Verifica si es mensaje de control
  case nodAct.estado of
  enLeyendo: begin  //caso normal
      if sqlCon.HayError then  //hubo error
        FijEstadoNodo(nodAct,enLlenoErr)
      else begin  //sin errores
        if nodAct.tipNod in [tnProced,tnFuncio,tnVisDefinic] then begin
          //estos casos no se expanden
          nodAct.source:=c.bolsa.Text;  //toma el resultado
          FijEstadoNodo(nodAct,enLLeno);
          PonerMensNodo(nodAct, IntToStr(c.bolsa.Count)+' líneas.');
        end else if nodAct.tipNod in [tnTabla, tnVista] then begin
          //Las tablas y vistas, devuelven una cadena con descripción
          nodAct.source:=c.linEncab + LineEnding + c.linMarca + LineEnding +
                         c.bolsa.Text;  //resultado con encabezado
          {------------------------------------------------------
          Como ayuda adicional, y aprovechando 'nodAct.campos', está vacío para tablas y
          vistas, escribe información de los campos de la tabla (no del texto). Este código
          además sirve de ejemplo sobre cómo tratar la información proporcionada}
          n:=0;
          setlength(nodAct.campos,n);
          sqExtractColumns(c.linMarca, c.linEncab, campos);  //extrae campos del texto
          for lin in c.bolsa do begin
            index_name := sqGetColTxt(lin, campos, 2);
            if index_name='' then begin  //noe s índice
              Inc(n);
              setlength(nodAct.campos,n);
              nodAct.campos[n-1].nombre:= sqGetColTxt(lin, campos, 0);
              nodAct.campos[n-1].tipOra:= sqGetColTxt(lin, campos, 1);
              nodAct.campos[n-1].etiq:= nodAct.campos[n-1].nombre;
              nodAct.campos[n-1].tipCam:= tcsCad; //no examina el texto
            end;
          end;
          {------------------------------------------------------}
          FijEstadoNodo(nodAct,enLLeno,false);  //no expande
          PonerMensNodo(nodAct, IntToStr(c.bolsa.Count)+' líneas.');
        end else begin  //otros nodos
          FijEstadoNodo(nodAct,enSinDatos);
          PonerMensNodo(nodAct,0);
        end;
      end;
    end;
  enEsperand:begin  //caso normal
      if sqlCon.HayError then  //hubo error
        FijEstadoNodo(nodAct,enLlenoErr)
      else begin  //sin errores
        FijEstadoNodo(nodAct,enLleno);
        PonerMensNodo(nodAct, nodAct.Count);
      end;
    end;
  enSinDatos, enErrConex, enNoInic, enLleno: begin
      //no debería pasar ninguno de estos casos, porque se supone que para que lleguen
      //datos, debería estar en "enLeyendo" o "enEsperand".
      FijEstadoNodo(nodAct,enSinDatos);
    end;
  end;
  if OnNodUpdate<>nil then OnNodUpdate(nodAct);  //evento
  nodAct := nil;   //ya terminó la consulta
  if OnQueryEnd<>nil then OnQueryEnd;    //evento
end;
procedure TfraExplorBD.sqlCon_ErrorConex(CurXY: TPoint; const msg: string);
begin
  FijEstadoNodo(nodAct, enErrConex);
  MsgErr(msg);
end;
procedure TfraExplorBD.sqlCon_ErrorSQL(CurXY: TPoint; const msg: string);
begin
  if CurXY.y<>-1 then begin //hay información de posición
    MsgErr(msg + #13#10 + dic('(Línea: %d, Columna: %d)', [CurXY.y, CurXY.x]));
    //El número de línea y columna, está referido a la consulta actual, no a todo el texto.
//      edSQL.CaretXY:=CurXY;  //ubica.
  end else begin
    MsgErr(msg);
  end;
end;

procedure TfraExplorBD.LLenarNodoActual(const cad: string; tipNod: TsqNodeType;
                                        sql: string);
//Llena el nodo actual el texto indicado. Usa el mismo ícono del nodo actual.
var
  nod : TBDNodo;
  txt : string;
  n: TBDNodo;
begin
  if nodAct = nil then exit;  //no hay nodo que llenar
  if nodAct.estado = enLeyendo then begin
    //es el primer elemento
    FijEstadoNodo(nodAct,enNoInic);  //para limpiarlo primero
    nodAct.campos:=c.Enc;   //copia estrucutura de campos
  end;
  //agrega nuevo nodo con ícono del padre
  txt := c.campo(0,cad);  //toma el primer campo
  nod:= NuevoNodo(nodAct,txt, tipNod, nodAct.ImageIndex);
  nod.user:=nodAct.user;
  //algunos nodos ponen sus nodos hijos sin opción de expandir
  if tipNod in [tnProced,tnFuncio,tnTabCampo,tnTabIndic, tnVisCampo] then begin
    nod.HasChildren:=false;
  end else if tipNod = tnTabla then begin
    //las tablas tienen 2 nodos fijos
    if nod.user = '' then begin   //es del usuario actual
      n:= NuevoNodo(nod,'Campos', tnTabListCampo, 7);
      n.sql:='SELECT column_name, data_type,'+LineEnding+
      'data_length, nullable || ''       '', data_default'+LineEnding+
      'FROM user_tab_columns'+LineEnding+
      'WHERE TABLE_NAME='''+txt+''' '+   ///*******SEIM????????
      'ORDER BY column_id;';
      n := NuevoNodo(nod,'Índices', tnTabListIndic,5);
      n.sql:='SELECT INDEX_NAME, INDEX_TYPE, TABLE_NAME, UNIQUENESS'+LineEnding+
      'FROM user_indexes'+LineEnding+
      'WHERE TABLE_NAME = '''+txt+''';';
    end else if nod.user = '*' then begin  //de todos los usuarios
{      n:= NuevoNodo(nod,'Campos', tnTabListCampo, 7);
      n.sql:='SELECT column_name, data_type,'+LineEnding+
      'data_length, nullable || ''       '', data_default'+LineEnding+
      'FROM all_tab_columns'+LineEnding+
      'WHERE owner = ''XXXXXX'' AND TABLE_NAME='''+txt+''' '+   ///*******SEIM????????
      'ORDER BY column_id;';
      n := NuevoNodo(nod,'Índices', tnTabListIndic,5);
      n.sql:='SELECT INDEX_NAME, INDEX_TYPE, TABLE_NAME, UNIQUENESS'+LineEnding+
      'FROM all_indexes'+LineEnding+
      'WHERE TABLE_NAME = '''+txt+''';';}
    end;
  end else if tipNod = tnVista then begin
    //los índices tienen 2 nodos fijos
    if nod.user = '' then begin   //es del usuario actual
      n:= NuevoNodo(nod,'Campos', tnVisListCampo, 7);
      n.sql:='SELECT column_name, data_type,'+LineEnding+
      'data_length, nullable || ''       '', data_default'+LineEnding+
      'FROM user_tab_columns'+LineEnding+
      'WHERE TABLE_NAME='''+txt+''' '+   ///*******SEIM????????
      'ORDER BY column_id;';
      n := NuevoNodo(nod,'Definición', tnVisDefinic,5);
      n.sql:='DECLARE txt long;'+
          'BEGIN  dbms_output.enable(100000);'#13#10+
          'SELECT TEXT INTO txt FROM USER_VIEWS where view_name = '''+c.Campo(0,txt)+''';'#13#10+
          'dbms_output.put_line(''.'');'#13#10+
          'dbms_output.put_line(''&%$'');'#13#10+
          'dbms_output.put_line(txt);'#13#10+
          'dbms_output.put_line(''&%$'');'#13#10+
          'END;'#13#10+
          '/';
    end else if nod.user = '*' then begin  //de todos los usuarios
{      n:= NuevoNodo(nod,'Campos', tnTabListCampo, 7);
      n.sql:='SELECT column_name, data_type,'+LineEnding+
      'data_length, nullable || ''       '', data_default'+LineEnding+
      'FROM all_tab_columns'+LineEnding+
      'WHERE owner = ''XXXXXX'' AND TABLE_NAME='''+txt+''' '+   ///*******SEIM????????
      'ORDER BY column_id;';
      n := NuevoNodo(nod,'Índices', tnTabListIndic,5);
      n.sql:='SELECT INDEX_NAME, INDEX_TYPE, TABLE_NAME, UNIQUENESS'+LineEnding+
      'FROM all_indexes'+LineEnding+
      'WHERE TABLE_NAME = '''+txt+''';';}
    end;
  end;
  //agrega fila de datos de la consulta
  nod.dat:=cad;
  nod.sql:=sql;  //agrega consulta
  PonerMensNodo(nodAct, nodAct.Count);
  FijEstadoNodo(nodAct,enEsperand);  //cambia estado
end;
function TfraExplorBD.UpdateNode(nod: TBDNodo): boolean;
//Actualiza contenido del nodo, ejecutando su consulta
begin
  Result := false;
  if nod = nil then exit;
  if nod.sql<>'' then
    Result := LanzarSentencia(nod.sql, nod, false);
end;
///////////////// Eventos de TreeView1 //////////////////
procedure TfraExplorBD.TreeView1CreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
//Cambia la clase de nodo para poder agregarle más propiedades
begin
  NodeClass := TBDNodo;
end;
procedure TfraExplorBD.TreeView1Expanding(Sender: TObject; Node: TTreeNode;
                   var AllowExpansion: Boolean);
//se pide expandir
begin
  if TBDNodo(Node).estado in [enLleno, enSinDatos] then exit;   //ya está actualizado
  //Esquema actual
  if TBDNodo(Node).sql<>'' then begin
    //El nodo contiene una consulta
    AllowExpansion := LanzarSentencia(TBDNodo(Node).sql, node, true);
  end;
end;
procedure TfraExplorBD.TreeView1KeyDown(Sender: TObject; var Key: Word; //tecla pulsads
  Shift: TShiftState);
begin
  if TreeView1.Selected = nil then exit;
  if Key = 13 then   //expande el nodo seleccionado con <enter>
    UpdateNode(TBDNodo(TreeView1.Selected));
end;
procedure TfraExplorBD.TreeView1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if TreeView1.Selected = nil then exit;
  //genera evento de Doble Click en nodo
  if OnMouseUpNod <> nil then
    OnMouseUpNod(TBDnodo(TreeView1.Selected), Button, Shift);
end;
procedure TfraExplorBD.TreeView1SelectionChanged(Sender: TObject);
begin
  if TreeView1.Selected = nil then exit;
  //genera evento de nodo seleccionado
  if OnNodSelec <> nil then
    OnNodSelec(TBDnodo(TreeView1.Selected));
end;
procedure TfraExplorBD.TreeView1DblClick(Sender: TObject);
begin
  if TreeView1.Selected = nil then exit;
  //genera evento de Doble Click en nodo
  if OnDblClickNod <> nil then
    OnDblClickNod(TBDnodo(TreeView1.Selected));
end;
procedure TfraExplorBD.TreeView1Click(Sender: TObject);
begin
  if TreeView1.Selected = nil then exit;
  if OnNodClick<>nil then
    OnNodClick(TBDnodo(TreeView1.Selected));
end;

procedure TfraExplorBD.FijarNodoActualSQL(Node: TTreeNode; ForzarExpan: boolean = true);
//Fija el nodo actual que recibirá la consulta y lo prepara para recibir datos
begin
  nodAct := TBDNodo(Node); //donde se llenará la lista de tablas
  FijEstadoNodo(nodAct, enLeyendo, ForzarExpan);  //pone en espera
  c.InicExplorac;           //inicia la exploración de las líneas
end;
function TfraExplorBD.LanzarSentencia(sql: string; Node: TTreeNode;
  ForzarExpan: boolean): boolean;
{Manda una sentencia "sql" a Oracle. Si la conexión está cerrada, intenta abrir
 la conexión y pone la consulta en la cola. Si la conexión está ocupada, genera
 un mesaje y retorna False. En caso contrario retorne true.}
begin
  if not TakeConnection then exit; //Toma control de la ceonxión
//debugln('---lanzar---');
  Result := true;
  case sqlCon.State of
  ECO_BUSY: begin
    MsgExc('Hay una consulta en proceso');
    Result := false;
  end;
  ECO_CONNECTING: begin
    if nodAct <> nil then begin
      MsgExc('Hay una consulta pendiente');
      Result := false;
      exit;
    end;
    FijarNodoActualSQL(Node,ForzarExpan);
    txtSentEspera := sql;  //guarda sentencia
    sqlCon.OnQueryEnd:=@SentenciaEnEspera;  //programa
  end;
  ECO_STOPPED: begin
    FijarNodoActualSQL(Node,ForzarExpan);
    txtSentEspera := sql;  //guarda sentencia
    sqlCon.OnQueryEnd:=@SentenciaEnEspera; //programa
    sqlCon.Open;
    if sqlCon.HayError then  //no se pudo Open
      FijEstadoNodo(nodAct, enErrConex);
  end;
  ECO_ERROR_CON: begin
//    ShowMessage('Error en conexión: ' + sqlCon.cadError);
    //cierra la conexión
    sqlCon.Close;
    //prepara otro intento
    FijarNodoActualSQL(Node,ForzarExpan);
    txtSentEspera := sql;  //guarda sentencia
    sqlCon.OnQueryEnd:=@SentenciaEnEspera; //programa
    sqlCon.Open;
    if sqlCon.HayError then  //no se pudo Open
      FijEstadoNodo(nodAct, enErrConex);
  end;
  ECO_READY: begin
    FijarNodoActualSQL(Node,ForzarExpan);
    sqlCon.SendSQl(sql);
  end;
  end;
end;
procedure TfraExplorBD.SentenciaEnEspera;
//Lanza la sentencia que está en txtSentEspera
begin
  if sqlCon.state <> ECO_READY then exit;
  sqlCon.SendSQl(txtSentEspera);  //lanza consulta
  txtSentEspera := '';  //limpia
  sqlCon.OnQueryEnd:=@sqlCon_QueryEnd;  //restaura evento
end;
procedure TfraExplorBD.MostVentanaSesion;  //Abre la ventana de sesión
begin
  ventSes.proc := sqlCon;
  ventSes.Show;
end;
procedure TfraExplorBD.OculVentanaSesion;  //cierra venatana
begin
  ventSes.Hide;
end;
//reflejo de las funciones de sqlCon
{procedure TfraExplorBD.Open;  //Inicia la conexión
begin
  if sqlCon.state = ECO_READY then exit;
  sqlCon.Open;
  if sqlCon.HayError then begin
    MsgErr(sqlCon.cadError);
    exit;
  end;
  if sqlCon.CurrentCon.AbrSes then
    MostVentanaSesion; //debe abrir la ventana al conectar
end;
procedure TfraExplorBD.Close;  //Desconecta
begin
  if sqlCon.state = ECO_STOPPED then exit;
  //hay conexión
  if not sqlCon.Close then
    msgerr('No se puede cerrar el proceso actual.');
  if nodAct<>nil then begin
    //había nodo esperando su resultado
    FijEstadoNodo(nodAct, enErrConex);
  end;
end;

procedure TfraExplorBD.SendSQL(txt: string);
{Función reflejo de sqlCon.SendSQL(), pero con la posibilidad de gestionar el tipo de salida
 (texto o grills)}
begin
  //verifica estado de la conexión
  if sqlCon.Closed then begin
    if MsgYesNo('Conexión no iniciada. ¿Conectar?') = 1 then sqlCon.Open;
    exit;
  end;
  //la salida es el visor interno o alguno externo
  sqlCon.SendSQL(txt);
  end;
end;}

procedure TfraExplorBD.SetLanguage(lang: string);
//Rutina de traducción
begin
  case lowerCase(lang) of
  'es': begin
      //controles
      //mensajes
      dicClear;  //ya está en español
    end;
  'en': begin
      //controles
      //mensajes
      dicSet('Leyendo...','Reading...');
      dicSet('Tablas','Tables');
      dicSet('Vistas','Views');
      dicSet('Índices','Indexes');
      dicSet('Procedimientos','Procedures');
      dicSet('Funciones','Functions');
      dicSet('Enlaces a BD','DB links');
      dicSet('Esquema Actual','Current Scheme');
      dicSet('Otros Esquemas','Other Schemes');
      dicSet('Todos los Esquemas','All the Schemes');
      dicSet('Usuarios','Users');
      dicSet('TableSpaces','TableSpaces');
      ///NO está completa la traducción

    end; end; end;
end.

