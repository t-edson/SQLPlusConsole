SqlPlusConsole 0.5b
===================

Librería en Lazarus, para crear conexiones a una base de datos en Oracle, usando el cliente SQLplus.

Esta librería tiene las siguientes dependencias: 
* Librería UnTerminal: (https://github.com/t-edson/UnTerminal)
* Librería ConfigFrame: (https://github.com/t-edson/ConfigFrame)
* Librería MisUtils: (https://github.com/t-edson/MisUtils)

Por lo tanto es necesario tener estas librerías disponibles.

La librería SqlPlusConsole, consta de los siguientes archivos:

* SqlPlusConsole.pas -> Unidad básica para crear conexiones con el SQLPlus.
* FrameCfgConOra.pas -> Frame de configuración para administrar las conexiones.
* SqlPlusHighlighter.pas -> Resaltador de sintaxis para la salida del SQLPlus.
* SqlPlusParser.pas -> Unidad para procesar la salida de textto del SQLPlus.
* FrameExplorBD.pas -> Frame con un "explorador de objetos" para la base de datos.
* FormVentSesion.pas -> Formulario sencillo para mostrar la salida del SQLPlus.
* FrameSqlPlusOut.pas -> Frame para mostrar la salida de texto( o en grilla ) de la consulta.
* FrameCfgSqlPlusOut.pas -> Frame de configuración para FrameSqlPlusOut.

Para una conexión sencilla, solo basta con usar las primeras tres unidades.

SqlPlusConsole
==============

La unidad principal de la líbrería es SqlPlusConsole, porque aquí se define al objeto 'TSQLPlusCon' que permite la conexión a la base de datos de Oracle.

El proceso para crear una conexión es:

* Crear un objeto 'TfraCfgConOra', con una conexión válida a la base de datos.
* Crear un objeto 'TSQLPlusCon' e iniciarlo con el objeto 'TfraCfgConOra' creado, usando el método Init().
* Abrir la conexión con el método Open(), y verificar si se genera algún error.

Un código sencillo para usar la consola sería:

```
uses ... , SQLPlusConsole;
...
var
  sqlCon: TSQLPlusCon;
...

  sqlCon:= TSQLPlusCon.Create;
  //Inicia la configuración. Se requiere un editor de tipo SynEdit para la salida
  //y un frame de tipo 'TfraCfgConOra', con la conexión a usar.
  sqlCon.Init(nil, conexiones);
  sqlCon.SetOut(editorSalida, nil);  //define editor de salida
  //Abre la conexión
  sqlCon.Open;  
  //Envía la consulta. La salida se mostará en 'editorSalida'
  sqlCon.SendSQL('SELECT * FROM DUAL;');  
  //Cierra la conexión
  sqlCon.Close; 
  
...

  sqlCon.Destroy;

```

El último parámetro de Init(), es un objeto de tipo 'TfraCfgConOra', esto es un frame de configuración (Ver https://github.com/t-edson/ConfigFrame), que está definido en la unidad FrameCfgConOra. En este frame se indica cuál es la conexión actual que debe usarse para iniciar la conexión a la base de datos Oracle.

Se usa un frame en lugar de una simple variable, para poder facilitar la configuración visualmente. Lo recomendable, es crear una ventana de configuración e incluir un frame de este tipo dentro de esta ventana de configuración, como se muestra en el ejemplo adjunto.

El objeto 'TSQLPlusCon', es el que realmente gestiona la conexión a Oracle. Es un descendiente directo de 'TConsoleProc' de la unidad 'UnTerminal', así que básicamente es un terminal especializado en procesar la salida de texto del cliente SQLPlus, al que  controla como un proceso, monitoreando su salida y enviando datos de entrada.

Como 'TSQLPlusCon', funciona como un simple terminal, una consulta lanzada con SendSql(), hará que los datos, que vayan llegando aparezcan en el editor de salida configurado con SetOut().

Estos datos de salida tienen la forma:

```
SQL*Plus: Release 11.2.0.2.0 Production on Wed Oct 15 10:58:45 2014
Copyright (c) 1982, 2010, Oracle.  All rights reserved.

Connected to:
Oracle Database 10g Express Edition Release 10.2.0.1.0 - Production

SQL> SQL> SQL>
Session altered.

SQL> SQL> 
CAMPO1  CAMPO2
------ -------
ABC         99

SQL> 
```

Este contenido es el que aparecerá en el editor de salida configurado. Por lo tanto, la respuesta a una consulta, usando 'TSQLPlusCon', es ... simplemente texto. Lo cual es lógico, porque esta es la misma salida que nos ofrece el SQLPlus.

Una ventaja adicional de usar el SQLPlus, como cliente es que los comandos que podemos enviar no se remiten solamente a sentencias SELECT, sino que puede enviarse cualquier tipo de comando, como sentencias DDL, DCL, o comandos de configuración de entorno como SET.

Los datos pueden demorar en llegar, pero el proceso no se detendrá. Esta es la forma de trabajo de 'TSQLPlusCon' (y de 'TConsoleProc'). Para saber que los datos han terminado de llegar, se debe interceptar usar el evento OnQueryEnd(). El evento OnGetPrompt(), no se debe tocar porque lo usa 'TSQLPlusCon', para su procesamiento interno.

Los eventos más comunes que deben (y pueden) usarse en 'TConsoleProc', son:

* OnQueryEnd() -> Se genera cuando se detecta el prompt, lo que significa que la consulta actual ha terminado normalmente o con error.
* OnErrorConx() -> Se genera cuando se detecta un mensaje de error en el proceso de conexión a la base de datos.
* OnErrorSQL() -> Se genera cuando se produce un error en la última consulta lanzada.

Para capturar la salida se pueden usar también eventos o definir un editor de salida con el método SetOut().

La especialización de 'TSQLPlusCon' con respecto a 'TConsoleProc', se da principalmente en el manejo de errores. 'TSQLPlusCon' contiene código considerable para reconocer los mensajes de error que genera el SQLPlus, en su salida de texto.

Como terminal, 'TSQLPlusCon' hereda las características de 'TConsoleProc', así que incluye también la capacidad de procesar secuencias de control ANSI, pero estas funciones no son usadas en la práctica porque la salida del SQLPlus, es solo texto plano. Sin embargo es posible que se generen secuencias ANSI, si es que la conexión se realiza usando un cliente de telnet (llmanado remotamente al SSQLPLUS).

Se ha creado 'TSQLPlusCon' como un terminal 'TConsoleProc' por los siguientes motivos:

* Porque permite reutilizar el código ya creado para un terminal. Sobre todo, es útil la detección de prompt.
* Porque permite manejar conexiones lentas y de volúmenes grandes de datos (que es característica de 'TConsoleProc').
* Porque permite usar un cliente de telnet para ejecutar remótamente al SQLPlus (este fue uno de los requerimientos principales de la librería), en lugar de ejecutarlo localmente.

La capacidad de poder conectarse a la base de datos, ejecutando remotamente el SQLPlus, fue un requerimiento de diseño. Por ello es que se creó 'TConsoleProc' como un terminal. Las conexiones usando la librería Zeos o la conexión nativa de Lazarus, no hubieran permitido tal característica.

Debido a la posibilidad de poder usar una conexión alternativa al SQLPlus, es que se puede configurar un cliente de telnet usando el frame de configuración 'FrameCfgConOra'.

Detección de errores
--------------------

Para ver el error de la última operación se debe leer la bandera 'HayError'. Y el mensaje de error aparecerá en la cadena 'cadError'. Sin embargo, los errores recibidos en los eventos OnErrorConx() y OnErrorSQL(), incluyen el mensaje de error y, en el caso de OnErrorSQL(), puede incluir también la posición donde el SQLPlus indica que se ha producido el error. 

Esta posición se da siempre con respecto a la consulta que generó el error sin considerar espacios iniciales o comentarios o cualquier otra sentencia que no pertenezca a la sentencia actual (como sentencias SET PAGESIZE ... ).

La salida del SQLPLUS es solamente texto, por lo tanto la detección de errores trabaja explorando las últimas líneas del terminal para ver si muestran algún mensaje de error. Para la exploración se usan solo las líneas disponibles en el terminal VT100 (por defecto 25). Así que si apareciera un mensaje de error fuera del terminal, no se detectará

En la versión actual, la detección de errores funciona mejor cuando se usa SetOutVT100(), porque se trabaja con la variable interna 'linSqlT', que permite delimitar mejor la zona en donde puede aparecer el mensaje de error.

Mostrando el estado de la conexión
-----------------------------------

'TConsoleProc' deriva de 'TConsoleProc', y por lo tanto hereda la funcionalidad de poder mostrar el estado de la conexión en el panel de una barra de estado. Este estado se mostrará como un ícono animado y un texto descriptivo.

El panel a usar, se define en el método Init():

´´´
  sqlCon:= TSQLPlusCon.Create;
  
  sqlCon.Init(StatusBar1.Panels[1], conexiones);
  
´´´

Luego se debe procesar el evento OnDrawPanel(), de la barra de estado, para ejecutar la rutina de dibujo personalizada de 'TSQLPlusCon':

```
 StatusBar1.OnDrawPanel:=@SBDrawPanel;
 ...
 procedure Form1.SBDrawPanel(StatusBar:TStatusBar; Panel:TStatusPanel; const Rect:TRect);
 begin
   if panel.Index = 1 then sqlCon.DrawStatePanel(StatusBar.Canvas, Rect);
 end;
```

En este ejemplo se ha usado el segundo panel para mostrar el estado de la conexión. Aquí aparecerá el ícono y el texto predefinido.

Los posibles estados de la conexión son los mismos que se definen en UnTerminal:

* ECO_STOPPED
* ECO_CONNECTING
* ECO_ERROR_CON
* ECO_READY
* ECO_BUSY

Dicho estado corresponde al estado del proceso que se use para la conexión. Normalmente este proceso será el SQLPLUS, pero no siempre será así porque 'TSQLPlusCon' se ha diseñado para conectarse mediante cualquier cliente de tipo consola como un cliente Telnet.

La conexión pasará al estado ECO_READY, cuando se detecte el prompt SQL>, sea que se ejecute el SQLPLUS o cualquier proceso que genere ese Prompt.

El estado de la conexión mostrado en los íconos no refleja necesariamente el estado del SQLPLUS, porque el proceso se podría mostrar como ECO_READY, cuando el SQLPLUS muestre el prompt pero no tenga conexión con la base de datos.

Captura de la salida
--------------------

La forma más simple de ver la salida de las consultas enviadas a 'TSQLPlusCon', es sencillamente configurar un editor de salida con el método SetOut():

```
  sqlCon.SetOut(editorSalida, nil);  //define editor de salida
```

Esta instrucción debe ejecutarse después de iniciar la conexión con Init(). El método SetOut(), hace una configuración basica del editor (como ponerlo en modo de solo lectura). Tiene la siguiente definición:

```
procedure SetOut(OutEdit: TSynEdit; CursorPan: TStatusPanel; Highlight: boolean = true);
```

Es decir, que se espera un editor de tipo SynEdit, para mostrar la salida de las consultas. El parámetro  'CursorPan' permite indicar un panel de una barra de estado en donde se actualizará la posición del cursor del editor de salida, cuando este tenga el enfoque. 

Opcionalmente se puede indicar que se desea usar un resaltador de sintaxis, para el editor de salida. El resaltador ofrecido, remarca el prompt, los mensajes de error y los encabezados de los datos de salida.

Para la captura de datos del SQLPLUS se usa el método de 'Salida línea por línea con línea parcial' (Ver https://github.com/t-edson/UnTerminal), lo que significa que no se procesarán secuencias de escape ANSI.

Existe también el método SetOutVT100(), que es similar a SetOut(), pero que controla al editor para que pueda soportar secuencias de escape ANSI. Sin embargo, es poco probable que la conexión al SQLPLUS genere estas secuencias. De ser el caso, sería recomendable su uso. Una precaución a tener con SetOutVT100(), es que el manejo de la posición del cursor es más complicado (aunque se realiza internamente), por lo tanto si en algún momento de la aplicación se requiere redireccinar la salida de 'TSQLPlusCon', a otro editor, no se debe usar SenOutVT100(), porque puede perder la correcta ubicación del cursor, además SenOutVT100() limpiará al editor de salida cada vez que se ejecute, para asegurar que se tengan suficientes líneas para contener a una pantalla VT100.

SendOut() en cambio, recibe la información línea por línea, así que es fácil redireccionar la salida entre varios editores, sin perder información. Solo es conveniente iniciar siempre la bandera 'LinPartial' en FALSE, siempre al cambiar de editor, haciendo por seguridad, que los datos nuevos empiecen siempre en una línea nueva.

SendOut() and SetOutVT100() controlan a su editor usando grupos distintos de eventos, asi que en teoría podrian funcionar en paralelo, mostrando la salida en dos editores simultáneamente, usando métodos deistintos de visualización.

SetOut() y SendOutVT100() son la forma simple para configurar la salida del SQLPLUS, pero no son las únicas. Si se desea mayor libertad para controlar, cómo se mostrará la información de salida, se puede hacer uso de los eventos de salida.

El grupo de eventos recomendados es:

* OnLineCompleted() -> Se genera cuando se recibe un salto de línea, lo que indica que la línea actual se terminó de recibir.
* OnLinePartial() -> Se genera cuando se recibe una línea incompleta al final de la trama de  datos.
* OnLinePrompt() -> Se genera cuando se recibe una línea incompleta, que contiene el prompt al final de una trama de datos.

Notar que estos eventos son equivalentes a los eventos: OnLineCompleted(), OnReadData() y OnLinePrompt(), de la unidad "UnTerminal", con la salvedad de que en lugar de usar OnReadData(), aquí debemos usar OnLinePartial(), porque OnReadData(), es usado internamente por TConsoleProc.

Por lo tanto para la captura de datos de salida, podemos usar las combinaciones:
* OnLineCompleted() & OnLinePartial() -> Si se desea capturar las líneas por cada bloques de datos que se reibe.
* OnLineCompleted() & OnLinePrompt() -> Si se desea capturar las líneas que llegan hasta que aparezca nuevamente el Prompt.

También existe la forma alternativa de configurar un editor de salida usando los eventos: OnInitScreen(), OnRefreshLine(), OnRefreshLines(), y OnAddLine(), que no son usados internamente por TConsoleProc. Sin embargo, este método de control de salida es hasta cierto punto innecesario para una salida que es solo texto plano (salida del SQLPLUS).

Para mayor información sobre cómo configurar la salida de un editor usando eventos, se recomienda leer la documentación de 'UnTerminal' en : https://github.com/t-edson/UnTerminal.

Hay que considerar que cuando se usa SetOut(), se usarán los eventos OnLineCompleted() & OnLinePartial(), así que no es posible usar simultáneamente estos eventos con SetOut().

También, si es que se usa SetOutVT100(), se usarán los eventos OnInitScreen(), OnRefreshLine(), OnRefreshLines(), y OnAddLine(), así que no es posible usar simultáneamente estos eventos con SetOutVT100(). 

En resumen se tiene el siguiente esquema de captura de la salida de TConsoleProc:

```
+-------------------+
|                   | ----> SetOut()
|                   |       (forma simple)
|                   |
|                   | ----> SetOutVT100()
|                   |       (con soporte de secuencias ANSI)
|   TConsoleProc    |                
|                   | ----> Eventos OnLineCompleted, OnLinePartial y OnLinePrompt
|                   |       (con más control)         
|                   |                
|                   | ----> Eventos OnInitScreen, OnRefreshLine, OnRefreshLines, y OnAddLine
|                   |       (demasiado control)
+-------------------+
```

Si se quiere cancelar la salida de información a algún editor configurado (o a algún otro objeto), se debe ejecutar el método DisableAllOut(). 

La norma es, ejecutar primero DisableAllOut(), antes de configurar algún otro editor de salida o usar directamente los eventos para capturar la información de salida.

Requerimientos
--------------

La unidad 'TSQLPlusCon', para su funcionamiento, hace reconocimiento del texto que genera el SQLPLUS. Por ello se debe tener en consideración algunos elementos:

* El sistema de detección del prompt, asume que el prompt es siempre la cadena 'SQL> '. Si se cambiara este prompt mediante comandos del SQLPlus, la detección de prompt dejará de funcionar.
* El lenguaje configurado del SQLPLUS, debe ser inglés. Esto es importante sobretodo para el procesamiento del texto de salida con la unidad 'SqlPlusParser.pas'.


SqlPlusParser
=============

Como una utilidad adicional (y tal vez necesaria), se incluye adicionalmente un procesador de texto para manejar la salida del SQLPlus.

Este procesador (parser) se encuentra implementado en la unidad 'SqlPlusParser' en el archivo 'SqlPlusParser.pas'.

Para usar el procesador, se debe crear primero una instancia de 'TConvSqlPlus':

```
var
  c       : TConvSqlPlus;    //convertidor de texto
...

  c := TConvSqlPlus.Create;  //crea conversor
  
  c.Destroy;  //libera objeto
```

El objeto TConvSqlPlus, está preparado para manejar textos de tipo:

El formato del archivo de texto es de campos de tamaño fijo y debe tener la forma:
```
                       <línea en blanco>
BSS   COUNT(*)         <encabezado con nombres de campos>
--- ----------         <marcas con el tamaño de los campos>
1          101         <fila de datos>
17         130         <fila de datos>
19         110         <fila de datos>
                       <línea en blanco>
BSS   COUNT(*)         <encabezado con nombres de campos>
--- ----------         <marcas con el tamaño de los campos>
2          144         <fila de datos>
5          121         <fila de datos>
                       <línea en blanco>
21 rows selected.      <opcional, Número de registros>
```

Esta es la salida típica del SQLPlus, como respuesta a una consulta de tipo SELECT.

El objetivo de 'TConvSqlPlus', es poder filtrar las líneas de texto que son realmente de datos, y extraer los encabezados (nombres de columnas).

'TConvSqlPlus', tiene dos modos de trabajo:

* Exploración por líneas
* Exploración completa de un TStrings.

Para cada modo, exite un grupo de métodos en el objeto:

```
    //Funciones de exploración de línea
    procedure InicExplorac;
    function ExplorarLin(const cad: String): boolean;
    function ExplorarAcum(const cad: String): boolean;
    function ExplorarDelim(const cad: String; delim: string): boolean;
    function ExploraLinCamBas(const cad: String; nCampoBase: integer): boolean;
    //funciones de exploración en listas
    function FinLee: boolean;
    function LeeSig(var lin0: string): boolean;
    procedure InicExplorList(lista: TStrings);
    function LeeRegDeList(var lin: string): boolean;
```

El modo de exploración por líneas, se usa cuando no se quiere esperar a que lleguen todas las líneas de la consola para empear a mostrarlas o procesarlas. Este modo permite manejar bien las conexiones lentas o las consultas pesadas.

Básicamente el modo de trabajo es:

  - Llamar a InicExplorac(), para iniciar una exploración de datos cualquiera.
  - Lanzar la consulta a la consola y procesar cada una de las líneas que vayan llegando (de preferencia usando el método OnLineCompleted())
  - Para cada línea que llega de la consola, llamar a ExplorarLin().
  - Si ExplorarLin(), devuelve TRUE, significa que se ha encontrado la primera línea de datos y que ya se ha identificado el encabezado. El encabezado se puede entonces acceder desde la propiedad 'Enc':
  - Continuar leyendo con ExplorarLin(), las líneas quevan llegando. Cuando ExplorarLin() devuelve FALSE, indica que la línea indicada no es un alínea de datos (puede ser una línea en blanco, otro encabezado o un mensaje como "5 rows selected").
  - La exploración se debe realizar hasta que llegue el prompt, lo que indicará que ha terminado la consulta.
  
El modo de exploración completa, permite leer rápidamente el contenido de un TStrings, y extraer las líneas de datos. El procedimiento consiste en llamar primero a InicExplorList(), indicando el TStrings que se usará de fuente. Luego hacer llamadas sucesivas a LeeRegDeList(), para ir extrayendo las filas de datos, hasta que devuelva FALSE.

Si la primera llamada a LeeRegDeList(), devuelve TRUE, indica que hay datos en el TStrings, y que se puede leer el encabezado en 'Enc'. Esta propiedad está definida así:

```
  TCampoSqlPlus = record
    nombre : string;   //nombre de campo
    etiq   : string;   //etiqueta del campo
    posIni : integer;  //posición de inicio en el texto
    nCar   : integer;  //tamaño del campo en el texto
    tipCam : TTipoCamSqlP;  //tipo del campo
  end;

  TCamposSqlPlus = array of TCampoSqlPlus;

  TConvSqlPlus = class
  public
	...
    Enc     : TCamposSqlPlus; 
	...
  end;
```

Este arreglo, es de tipo 'TCampoSqlPlus', y guarda información diversa sobre una columna de datos, como el nombre de la columna (el que aparece en el encabezado), la posición de inicio en la cadena y la longitud.

Con esta información, es sencillo, extraer la cadena que representa a una columna de datos, desde cualquier línea de datos. Como utilidad adicional para extraer un campo cualquiera a partir de la línea de datos y del encabezado, se tienen las funciones: Campo() y CampoN().

Existen métodos adicionales de exploración como:

```
    function ExplorarAcum(const cad: String): boolean;
    function ExplorarDelim(const cad: String; delim: string): boolean;
    function ExploraLinCamBas(const cad: String; nCampoBase: integer): boolean;
```

ExplorarAcum(), hace una explración similar a ExplorarLin(), pero el resultado lo va guardando en un TSTringList interno.

ExplorarDelim(), detecta una cadena como delimitador inicial de un contenido. 
```
SQL> 1 2 3
xxx                  <--delimitador de inicio
línea 1
línea 2
línea 3
xxx                  <--delimitador de fin
...
```

ExploraLinCamBas(), permite explorar salidas de texto de tipo:
```
CAMPO_BASE     CAMPO_CUALQUIERA    CAMPO_MULTILINEA
-------------- ------------------- --------------------
VALOR          xxx                 En un lugar            <--solo deja pasar esta fila
                                   de la mancha de cuyo
                                   nombre
VALOR                              Cadena corta           <--solo deja pasar esta fila
VALOR          xxx                 Puedo escribir         <--solo deja pasar esta fila
                                   los versos más trist
                                   es esta noche
```

Como se puede ver, 'TConvSqlPlus', permite filtrar las líneas de datos de un texto de salida común del SQLPlus. Además extrae información básica sobre las columnas de datos que vienen en las líneas. 

Más allá de eso, no brinda información mágica. No se muestran los tipos de datos de las columnas (1). Y los nombres de las columnas que captura, pueden estar recortados, si es que el SQLPlus los recorta.

Tampoco se carga esta información en ningún control. 'TConvSqlPlus', no tiene contenedores para texto, simplemente tiene rutinas de procesamiento (Excepto cuando se usa la rutina ExplorarAcum). 

Si se desea mostrar la información en una grilla, se puede usar la unidad 'FrameSqlPlusOut.pas'.

(1) La Rutina sqExtractColumns(), hace un pequeño procesamiento para determinar si la columna es de tipo numérica, viendo el alineamiento del nombre de la columna. Si se encuentra alineado a la derecha, asume que es de tipo numérico.

Panel de Exploración
====================

Entre los archivos de la librería, se encuentra el frame TfraExplorBD, definido en  'FrameExplorBD.pas', y su archivo correspondiente 'FrameExplorBD.lfm'.

Estos archivos definen a un frame que permite mostrar visualmente los objetos de la base de datos, como tablas, vistas o procedimientos almacenados.

Este es un objeto visual, si se inserta en un formulario, tendrá esta apariencia:

![SQLPlusConsole](http://blog.pucp.edu.pe/media/4946/20141108-panelexpl.png "Panel de exploración")

Los objetos de la base de datos se encuentran clasificados en tres niveles de visibilidad:

```
-Esquema Actual
   +Tablas
   +Vistas
   +Índices
   +Procedimientos
   +Funciones
   +Enlaces a BD
-Otros Esquemas
   +Tablas
   +Vistas
   +Índices
   +Procedimientos
   +Funciones
   +Enlaces a BD
-Todos los esquemas
   +Tablas
   +Vistas
   +Índices
   +Procedimientos
   +Funciones
   +Enlaces a BD
```

El panel también puede mostrar información sobre los usuarios, tablespace y los procesos. Para acceder a cierta información de la base de datos, como los Tablespace, se debe usar un usuario con privilegios de DBA. Si no se tienen privilegios de DBA, el panel no podrá acceder a las tablas o vistas necesarias y mostrará el mensaje "ORA-00942-table or view doesn't exist"

El frame puede ser insertado en cualquier formulario y servirá como una ayuda visual en el aplicativo.

Para usar el panel de exploración se debe insertar un frame TfraExplorBD, en el formulario en donde deseemos usarlo. 

```
uses ... , FrameExplorBD;

//se supone que se tiene el panel 'frmPanelBD ', insertado en el formulario.

procedure TPrincipal.FormShow(Sender: TObject);
begin
  sqlCon.Init(...);  //inicia conexión
  fraPanelBD.SetConnection(sqlCon);   //pasa conexión al panel
  ...
end;
```

Uso de la conexión
------------------

'TfraExplorBD', utiliza una conexión 'TSQLPlusCon' (de la unidad SqlPlusConsole), para realizar la conexión a la base de datos. Dicha conexión debe estar ya configurada y funcional. Toda la información visual mostrada en el frame, se obtiene a través de consultas SQL, cuyas salida de texto es procesada para convertirla en información visual.

'TfraExplorBD', también incluye un visor de texto, para mostrar la salida de las consultas que realiza, pero esta ventana no es primordial para el funcionamiento del frame, sino más bien sirve como una ayuda para monitorear la conexión.

Se ha probado con éxito, usar 'TfraExplorBD', en conexiones a la base de datos mediante un cliente de Telnet, en vez de usar el SQLPlus como cliente local.

El panel de exploración, requiere que se le pase la referencia de una conexión a la base de datos, y usa solamente esa conexión para todas las tareas de refresco de su interfaz gráfica. 

Eso no significa que no pueda usarse esa conexión para lanzar consultas a la base de datos.  El panel de exploración se ha diseñado para poder usar una conexión y poder compartirla con otros procesos de la aplicación. Cada vez que el panel de exploración necesita obtener información de la base de datos, verifica primero si la conexión referenciada, se encuentra disponible para usarla. De ser así toma el control de los eventos de la conexión para su trabajo interno. Si la conexión se encontrara ocupada, mostraría un mensaje de error y no ejecutaría la consulta.

De la misma forma se espera que trabaje el objeto con el que se comparta la conexión para evitar interferencias con el trabajo.

Solo como una funcionalidad adicional, se tienen definidos dos eventos-reflejo de TSQLPlusCon:

```
    OnLineCompleted: TEvLinCompleted;  //Evento de línea completa recibida
    OnQueryEnd : procedure of object;  //Evento de Fin de la consulta
```

Estos eventos pueden servir de ayuda cuando se requiera información adicional del SQLPLUS, mientras el panel de exploración tenga el control de la conexión.

Si se comparte la conexión con otro proceso, una precaución importante, para evitar que el panel de exploración falle en el refresco interno de sus nodos, es evitar enviar comandos que cambien las variables de entorno como LINESIZE, PAGESIZE, FEEDBACK, etc, porque podría evitar que el panel de exploración pueda reconocer correctamente el resultado de sus consultas. 

El desarrollo del panel de exploración ha requerido un trabajo considerable, y se puede decir que es una de las partes principales de la librería.

No todas las funcionalidades  del panel están habilitadas. Se ha trabajado principalmente en los objetos del grupo "Esquema Actual". El grupo "Otros esquemas" no está habilitado actualmente, y el grupo "Todos los esquemas", no es totalmente funcional.


Epílogo
=======

La librería 'SqlPlusConsole' a parte de la definición del objeto 'TSQLPlusCon', tiene un conjunto surtido de utilidades para crear aplicativos de conexión a una Base de Datos Oracle.

Ha tomado forma como soporte para el desarrollo del aplicativo SQMax, por lo tanto muchas de sus funcionalidades se han implementado como soporte a los requerimientos de dicho aplicativo.

Aún hay varios errores que corregir en la librería, pero aún así es completamente funcional, como lo prueban los aplicativos que se han desarrollado sobre ella.
