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
* SqlPlusParser.pas -> Unidad para procesar la salida de textto del SQLPlus.
* FrameCfgConOra.pas -> Frame de configuración para administrar las conexiones.
* FrameExplorBD.pas -> Frame con un "explorador de objetos" para la base de datos.
* FormVentSesion.pas -> Formulario sencillo para mostrar la salida del SQLPlus.
* SqlPlusHighlighter.pas -> Resaltador de sintaxis para la salida del SQLPlus.
* FrameSqlPlusOut.pas -> Frame para mostrar la salida de texto( o en grilla ) de la consulta.
* FrameCfgSqlPlusOut.pas -> Frame de configuración para FrameSqlPlusOut.

SqlPlusConsole
==============

La unidad principal de la líbrería es SqlPlusConsole, porque aquí se define al objeto 'TSQLPlusCon' que permite la conexión a la base de datos de Oracle.

Para una conexión sencilla, solo basta con usar las primeras tres unidades.

El proceso para crear una conexión es:

* Crear un objeto 'TfraCfgConOra', con una conexión válida a la base de datos.
* Crear un objeto 'TSQLPlusCon' e iniciarlo con el objeto 'TfraCfgConOra' creado, usando el método Init().
* Abrir la conexión con el método Open(), y verificar si se genera algún error.

Un código sencillo para usa la consola sería:

```
uses ... , SQLPlusConsole;
...
var
  sqlCon: TSQLPlusCon;
...

  sqlCon:= TSQLPlusCon.Create;
  //Inicia la configuración. Se requiere un editor de tipo SynEdit para la salida
  //y un frame de tipo 'TfraCfgConOra', con la conexión a usar.
  sqlCon.Init(nil, editorSalida, conexiones);
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

Como 'TSQLPlusCon', funciona como un simple terminal, una consulta lanzada con SendSql, hará que los datos, que vayan llegando aparezcan en el editor de salida configurado con Init().

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

Una evntaja adicional de usar el SQLPlus, como cliente es que los comandos que podemos enviar no se remiten solamente a sentencias SELECT, sino que puede enviarse cualquier tipo de comando, como sentencias DDL, DCL, o comandos de configuración de entorno como  SET .

Los datos pueden demorar en llegar, pero el proceso no se detendrá. Esta es la forma de trabajo de 'TSQLPlusCon' (y de 'TConsoleProc'). Para saber que los datos han terminado de llegar, se debe interceptar usar el evento OnQueryEnd(). El evento OnGetPrompt(), no se debe tocar porque lo 'TSQLPlusCon', para su procesamiento interno.

Los eventos más comunes que deben (y pueden) usarse en 'TConsoleProc', son:

* OnQueryEnd() -> Se genera cuando se detecta el prompt, lo que significa que la consulta actual ha terminado normalmente o con error.
* OnErrorConx() -> Se genera cuando se detecta un mensaje de error en el proceso de conexión a la base de datos.
* OnErrorSQL() -> Se genera cuando se produce un error en la última consulta lanzada.
* OnLineCompleted() -> Se genera cuando se recibe un salto de línea, lo que indica que la línea actual se terminó de recibir.

Los eventos OnInitScreen(), OnRefreshLine(), OnRefreshLines, y OnAddLine() tampoco deben usarse porque son usados internamente por 'TConsoleProc'.

El sistema de detección del prompt, asume que el prompt es siempre la cadena 'SQL> '. Si se cambiara este prompt mediante comadnos del SQLPlus, la detección de prompt dejará de funcionar.

Para ver el error de la última operación se debe leer la bandera 'HayError'. Y el mensaje de error aparecerá en la cadena 'cadError'. Sin embargo, los errores recibidos en los eventos OnErrorConx() y OnErrorSQL(), incluyen el mensaje de error y, en el caso de OnErrorSQL(), puede incluir también la posición donde se el SQLPlus indica que se ha producido el error. Esta posición se da siempre con respecto a la consulta que generó el error sin considerar espacios iniciales o comentarios o cualquier otra sentencia que no pertenezca a la sentencia actual (como sentencias SET pagesize ... ).

La especialización de 'TSQLPlusCon' con respecto a 'TConsoleProc', se da principalmente en el manejo de errores. 'TSQLPlusCon' contiene código considerable para reconocer los mensajes de error que genera el SQLPlus, en su salida de texto.

Como terminal, 'TSQLPlusCon' hereda las características de 'TConsoleProc', así que incluye también la capacidad de procesar secuencias de controsl ANSI, pero estas funciones no son usadas en la práctica porque la salida del SQLPlus, es solo texto plano.

Se ha creado 'TSQLPlusCon' como un terminal 'TConsoleProc' por los siguientes motivos:

* Porque permite reutilizar el código ya creado para un terminal. Sobre todo, es útil la detección de prompt.
* Porque permite manejar conexiones lentas y de volúmenes grandes de datos ( que es característica de 'TConsoleProc').
* Porque permite usar un cliente de telnet para ejecutar remótamente al SQLPlus (este fue uno de los requerimientos principales de la librería), en lugar de ejecutarlo localmente.

La capacidad de poder conectarse a la base de datos, ejecutando remotamente el SQLPlus, fue un requerimiento de diseño. Por ello es que se creó 'TConsoleProc' como un terminal. Las conexiones usando la librería Zeos o la conexión nativa de Lazarus, no hubieran permitido tal característica.

Debido a la posibilidad de poder usar una conexión alternativa al SQLPlus, es que se puede   configurar un cliente de telnet usando el frame de configuración 'FrameCfgConOra'.


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

Además estos elementos, se encuentran clasificados por Tres niveles de visibilidad:

```
-Esquema Actual
   +Tablas
   +Vistas
   +Índices
   +Procedimientos
   +Funciones
   +Enlaces a BD
+Otros Esquemas
   +Tablas
   +Vistas
   +Índices
   +Procedimientos
   +Funciones
   +Enlaces a BD
+Todos los esquemas
   +Tablas
   +Vistas
   +Índices
   +Procedimientos
   +Funciones
   +Enlaces a BD
```

El panel también puede mostrar información sobre los tablespace y los procesos. Para acceder ciertas información de la base de datos, como los Tablespace, se debe usar un usuario con privilegios de DBA. Si no se tienen privilegios de DBA, el panel no podrá acceder a las tablas o vistas necesarias y mostrará el mensaje "ORA-00942-table or view doesn't exist"

El frame puede ser insertado en cualquier formulario y servirá como una ayuda visual en el aplicativo.

Para usar el panel de exploración se debe insertar un frame TfraExplorBD, en el formulario en donde deseemos usarlo. 

```
uses ... , FrameExplorBD;

//se supone que se tiene el panel 'frmPanelBD ', insertado en el formulario.

procedure TPrincipal.FormShow(Sender: TObject);
begin
  Config.Iniciar(...);   //inicia configuración
  frmPanelBD.Iniciar(StatusBar1.Panels[1], Config.fcConOra);  //inicia panel 
  ...
end;
```

'TfraExplorBD', utiliza a la unidad SqlPlusConsole, para realizar la conexión a la base de datos. Toda la información visual mostrada en el frame, se obtiene a través de consultas SQL, cuyas salida de texto es procesada para convertirla en información visual.

'TfraExplorBD', también incluye un visor de texto, para mostrar la salida de las consultas que realiza, pero esta ventana no es primordial para el funcionamiento del frame, sino más bien sirve como una ayuda para la depuración.

Se ha probado con éxito, usar 'TfraExplorBD', en conexiones a la base de datos mediante un cliente de Telnet, en vez de usar el SQLPlus como cliente local.

El panel de exploración, abre una conexión a la base de datos, y usa solamente esa conexión para todas las tareas de refresco de su interfaz gráfica. Eso no significa que no pueda usarse esa conexión para lanzar consultas a la base de datos.

De hecho, el panel de exploración, se puede usar como una conexión a la base de datos, reemplazando a 'TSQLPlusCon'. Para ello implementa los métodos:

    procedure Open;
    procedure Close;
    function Closed: boolean;
    procedure DrawStatePanel(cv: TCanvas; const Rect: TRect);

que son reflejo de las funciones del mismo nombre que tiene un objeto 'TSQLPlusCon'.

También se pueden enviar consultas a la base de datos, usando el método:

    procedure SendSQL(txt: string);

Sin embargo, el resultado de las consultas aparecerá en la ventana de sesión del panel de exploración, por defecto.
	
Para redireccionar la salida de texto del panel de exploración, se puede jugar directamente con 'sqlCon.edSal', pero es más seguro usar el método:

procedure TfraExplorBD.SetOutput(edSal: TSynEdit; maxLinOut0: integer = 100000);

Una vez direccionada la salida a un editor, todo el texto, incluyendo el resultado de las consultas que ejecuta el mismo panel de exploración, se mostrará en el nuevo editor redireccionado. Para retornar la salida a la ventana de sesión del panel, se debe usar el método:

procedure TfraExplorBD.SetOutputInternal;

Sin embargo, si de mostrar resultados, se trata, el panel de exploración maneja mejor la salida usando el frame 'TfraSQLPlusOut' (que además puede mostrar los datos en forma de grilla), definido en la unidad 'FrameSqlPlusOut.pas'.

Para asociar el panel a un frame de salida 'TfraSQLPlusOut', se debe usar el método:

procedure TfraExplorBD.SetOutput(fraSQLOut0: TfraSQLPlusOut; CursorPan: TStatusPanel;
                                maxLinOut0: integer = 100000);

El parámentro 'CursorPan' es un panel de un TStatusBar, en donde se desea mostrar la posición del cursor cuando el enfoque lo tenga el editor de salida del objeto 'TfraSQLPlusOut'.

Una vez direccionada la salida a un frame 'TfraSQLPlusOut', el panel de exploración gestionará la salida de datos, inclusive cuando el frame se encuentre en modo grilla.

Una precaución importante, para evitar que el panel de exploración falle en el refresco interno de sus nodos, es evitar enviar comandos que cambien las variables de entorno como LINESIZE, PAGESIZE, FEEDBACK, etc, porque podría evitar que el panel de exploración pueda reconocer correctamente el resultado de sus consultas. 

El desarrollo del panel de exploración ha requerido un trabajo considerable, y se puede decir que es una de las partes principales de la librería.

No todas las funcionalidades están habilitadas

Epílogo
=======

La librería 'SqlPlusConsole' a parte de la definición del objeto 'TSQLPlusCon', tiene un conjunto surtido de utilidades para crear aplicativos de conexión a una Base de Datos Oracle.

Ha tomado forma como soporte para el desarrollo del aplicativo SQMax, por lo tanto muchas de sus funcionalidades se han implementado como soporte a los requerimientos de dicho aplicativo.

Aún hay varios errores que corregir en la librería, pero aún así es completamente funcional, como lo prueban los aplicativos que se han desarrollado sobre ella.
