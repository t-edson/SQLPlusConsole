SqlPlusConsole 0.4b
===================

Librería en Lazarus, para crear conexiones a una base de datos en Oracle, usando el cliente SQLplus.

Esta librería tiene las siguientes dependencias: 

* Librería UnTerminal: (https://github.com/t-edson/UnTerminal)
* Librería ConfigFrame: (https://github.com/t-edson/ConfigFrame)
* Librería MisUtils: (https://github.com/t-edson/MisUtils)

Por lo tanto es necesario tener estas librería disponibles.

La librería SqlPlusConsole, consta de los siguientes archivos:

* SqlPlusConsole.pas -> Unidad básica para crear conexiones con el SQLPlus.
* SqlPlusParser.pas -> Unidad para procesar la salida de textto del SQLPlus.
* FrameCfgConOra.pas -> Frame de configuración para administrar las conexiones.
* FrameExplorBD.pas -> Frame con un "explorador de objetos" para la base de datos.
* FormVentSesion.pas -> Formulario sencillo para mostrar la salida del SQLPlus.
* SqlPlusHighlighter.pas -> Resaltador de sintaxis para la salida del SQLPlus.

Para una conexión sencilla, solo basta con usar las primeras tres unidades.

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
