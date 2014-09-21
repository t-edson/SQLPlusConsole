SqlPlusConsole 0.3b
===================

Librería en Lazarus, para crear conexiones a una base de datos en Oracle, usando el cliente SQLplus.

ESta librería tiene las siguientes dependencias: 
Librería UnTerminal: (https://github.com/t-edson/UnTerminal)
Librería ConfigFrame: (https://github.com/t-edson/ConfigFrame)

Por lo tanto es necesario tener estas librería disponibles.

La librería SqlPlusConsole, consta de los siguientes archivos:

* SqlPlusConsole.pas -> Unidad básica para crear conexiones con el SQLPlus.
* SqlPlusParser.pas -> Unidad para procesar la salida de textto del SQLPlus.
* FrameCfgConOra.pas -> Frame de configuración para administrar las conexiones.
* FrameExplorBD.pas -> Frame con un "explorador de objetos" para la base de datos.
* FormVentSesion.pas -> Formulario sencillo para mostrar la salida del SQLPlus.
* SqlPlusHighlighter.pas -> Resaltador de sintaxis para la salida del SQLPlus.

Para una conexión sencilla, solo basta con usar las primeras tres unidades.

