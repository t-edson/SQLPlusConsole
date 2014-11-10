{
SQLPlusParser 0.5
=================
Por Tito Hinostroza  12/10/2014
* Se agregan métodos y variables para la exploración de datos que estén en un
TStringList.
* Se crea el método CampoN().
* Se reordena el código.
* Se agrega el campo "tipOra" a TCampoSqlPlus, para que pueda servir a las rutinas
que manejan tipo de datos de oracle.

Descripción
===========
Unidad con utilidades para la lectura y reconocimiento de archivos de texto en el
formato típico de salida del SQLPLUS del Oracle.

El formato del archivo de texto es de campos de tamaño fijo y debe tener la forma:

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

También permite procesar los datos que se obtienen de conexiones usando la unidad
"SqlPlusConsole"
}
unit SQLPlusParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, StrUtils;
type
  //estructura para guardar datos de los campos
  TTipoCamSqlP = (tcsCad, tcsNum);  //tipos de campos
  TCampoSqlPlus = record
    nombre : string;   //nombre de campo
    etiq   : string;   //etiqueta del campo
    posIni : integer;  //posición de inicio en el texto (en caracteres)
    index  : integer;  //posición del campo dentro del encabezado (inicica en 1)
    nCar   : integer;  //tamaño del campo en el texto
    tipCam : TTipoCamSqlP;  //tipo del campo
    tipOra : String;   //Tipo de dato según Oracle (Varchar2, Number, etc)
  end;

  TCamposSqlPlus = array of TCampoSqlPlus;
  { TConvSqlPlus }
  //Objeto para tratamiento de los archivos de texto que genera el SQLPLUS de Oracle
  TConvSqlPlus = class
  public
    msjError: string;
    Enc     : TCamposSqlPlus; //encabezados
    Titulo  : string;     //para determinar si se encontró título
    bolsa   : TStringList;  //bolsa para guardar un contenido
    linEncab  : string;   //línea de encabezados
    linMarca  : string;   //línea de marcas
    function Campo(n: integer; const cad: String): string;
    function CampoN(n: integer; const cad: String):double;
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
    //funciones de archivo
    procedure LeeEncabezado(arc: string; CerrarArch: Boolean=True);
  private
    hayEncab  : boolean;  //para determinar si se encontró encabezado
    sinDatos  : boolean;
    linAnter  : string;   //línea anterior
    //para exploración de listas
    l : Tstrings;
    ie: integer;
  public  //funciones de la clase
    constructor Create;
    destructor Destroy; override;
  end;
procedure sqExtractColumns(const linMarca, linEncab: string; var campos: TCamposSqlPlus);
function sqGetColTxt(const lin: string; const campos: TCamposSqlPlus;
                           n: integer): string; inline;

implementation
//utilidades
function Explode(delimiter:string; str:string):TStringDynArray;
var
  p, n, dsize:integer;
begin
  n := 0;
  dsize := length(delimiter);
  while true do begin
    p := pos(delimiter,str);
    if p > 0 then begin
      inc(n);
      SetLength(Result,n);
      Result[n-1] := copy(str,1,p-1);
      delete(str,1,p+dsize-1);
    end else break;
  end;
  inc(n);
  SetLength(Result,n);
  Result[n-1] := str;
end;
procedure sqExtractColumns(const linMarca, linEncab: string; var campos: TCamposSqlPlus);
//Extrae los campos de un texto, revisando las líneas de encabezados y de marcas
//Se expone esta función sirva también para usarse desde fuera de la unidad.
var
  a   : TStringDynArray;
  x, i: Integer;
  tmp: String;
begin
  a := Explode(' ', linMarca); //separa campos
  SetLength(campos,High(a)+1);
  //escribe propiedades
  x := 1;
  For i := 0 To High(a) do begin
    tmp:= Copy(linEncab, x, Length(a[i]));
    campos[i].nombre := Trim(tmp);
    campos[i].etiq := campos[i].nombre; //etiqueta por defecto
//      campos(i).ind := i;    //pone índice
    campos[i].posIni := X;
    campos[i].nCar := Length(a[i]);
    campos[i].index:=i+1;
    //intenta determinar si es un campo numérico
    if tmp[1] = ' ' then  //campo alineado a la derecha
      campos[i].tipCam:=tcsNum   //se asume numérico
    else
      campos[i].tipCam:=tcsCad;  //cadena por defecto
    x := x + Length(a[i]) + 1;   //calcula siguiente posición
  end;
end;

function sqGetColTxt(const lin: string; const campos: TCamposSqlPlus;
                           n: integer): string; inline;
//Extrae el campo "n", de una línea de acuerdo a la información de "campos"
begin
  Result:= trim(copy(lin, campos[n].posIni,campos[n].nCar));
end;

{ TConvSqlPlus }
function TConvSqlPlus.Campo(n: integer; const cad: String):string;
//Devuelve el campo n-ésimo de la linea "cad". Debe llamarse cuando se ha
//reconocido el encabezado
begin
  //observar que los campos de cadena que incluyen espacios iniciales o finales
  //aparcerán recortados.
  Result := Trim(Copy(cad,Enc[n].posIni,Enc[n].nCar));
end;
function TConvSqlPlus.CampoN(n: integer; const cad: String):double;
//Devuelve el campo n-ésimo de la linea "cad". Devuelve en número.
begin
  Try
    Result := StrToFloat(Campo(n, cad));
  except
    On E : Exception do
      Result := 0;
  end;
end;
//Funciones de exploración de línea
procedure TConvSqlPlus.InicExplorac;  //Inicia exploración
begin
  hayEncab := false;  //inicia badnera
  Titulo := '';    //inicia título
  setLength(Enc,0);  //iniica lista de campos
  bolsa.Clear;       //limpia bolsa por si se necesita usarla
  linAnter := '';    //limpia
end;
function TConvSqlPlus.ExplorarLin(const cad: String):boolean;
//Explora una línea de datos y extrae el encabezado y determina si la fila
//ingresada es una fila de datos. De ser así devuelve TRUE.
begin
  Result := false;
  if hayEncab then begin
    //línea normal
    if length(cad) = 0 then exit;  //no es de datos
    //estas comparaciones podrían ser pesadas
    if cad = linMarca then exit;
    if cad = linEncab then exit;
    if  AnsiEndsStr('rows selected',cad) or
        AnsiEndsStr('rows selected.',cad) then exit;
    Result := true;    //se asume que si es
  end else begin
    //aún no se ha encontrado el inicio del reporte
    if AnsiStartsText('$TITULO', cad) then Titulo:=copy(cad,9,1000);
    //verifica si la fila indicada, contiene las marcas
    if (length(cad)>0) and (cad[1] = '-') and
       (cad[length(cad)] = '-') then begin  //primer y último caracter de marca
      { TODO : Tal vez debería quitar los espacios finales de "cad" antes de ver el último caracter}
      //encontró línea de marcas
      linMarca := cad;
      //lee encabezados
      linEncab := linAnter;  //línea anterior
      if linEncab = '' then begin
        msjError := 'Error en formato de datos.';
        exit;
      end;
      hayEncab:=true;
      //extrae información de los encabezados
      sqExtractColumns(linMarca, linEncab, Enc);
      exit;
    end;
  end;
//  linAnter2 := linAnter;  nol lo usa
  linAnter := cad;
end;
function TConvSqlPlus.ExplorarAcum(const cad: String): boolean;
//Explora al igual que ExplorarLin(), pero va acumulando las líneas de datos
begin
  if ExplorarLin(cad) then
    bolsa.Add(cad);  //acumula
end;
function TConvSqlPlus.ExplorarDelim(const cad: String; delim: string): boolean;
{Explora una línea de datos, buscando una línea especial (delimitador) para
asumir que las líneas siguientes son el contenido, hasta encontrar nuevamente
el delimitador. Va acumulando el texto en "bolsa" ,Usa la bandera "hayEncab".
Un caso sería:

SQL> 1 2 3
xxx                  <--delimitador de inicio
línea 1
línea 2
línea 3
xxx                  <--delimitador de fin
...
}
begin
  Result := false;  //valor por defecto
  if hayEncab then begin
    //ya se detectó el delimitador de origen
    if cad = delim then begin  //busca otro delimitador
      hayEncab:=false;   //cambia bandera
      Result := false;   //valor por defecto
      exit;
    end;
    Result := true;
    bolsa.Add(cad);  //acumula
  end else begin
    //busca la líea-delimitador
    if cad = delim then hayEncab:=true;
  end;
end;
function TConvSqlPlus.ExploraLinCamBas(const cad: String; nCampoBase: integer):boolean;
{Similar a ExplorarLin() pero permite filtrar los campos que sean multilínea.
 Se le debe indicar una columna como referencia que siempre tenga algún valor, para
 usarla como campo de referencia. Un caso sería:
CAMPO_BASE     CAMPO_CUALQUIERA    CAMPO_MULTILINEA
-------------- ------------------- --------------------
VALOR          xxx                 En un lugar            <--solo deja pasar esta fila
                                   de la mancha de cuyo
                                   nombre
VALOR                              Cadena corta           <--solo deja pasar esta fila
VALOR          xxx                 Puedo escribir         <--solo deja pasar esta fila
                                   los versos más trist
                                   es esta noche
}
var i, x: Integer;
    tmp : string;
begin
  Result := false;
  ExplorarLin(cad);
  if not hayEncab then exit;  //no hay encabezado
  //hay encabezado
  if length(cad) = 0 then exit;  //no es de datos
  //estas comparaciones podrían ser pesadas
  if cad = linMarca then exit;  //no es
  if cad = linEncab then exit;  //no es
  if  AnsiEndsStr('rows selected',cad) or
      AnsiEndsStr('rows selected.',cad) then exit;  //no es
  //debería ser fila de datos pero falta aplicar el filtro
  if campo(nCampoBase, cad)<> '' then Result := true;
end;
//funciones de exploración en listas
function TConvSqlPlus.FinLee: boolean;  //indica cuando ya no hay líneas que leer de "ed"
begin
  Result := ie>=l.Count;
end;
function TConvSqlPlus.LeeSig(var lin0:string): boolean;
//Lee la siguiente línea del editor "ed". La posición actual se guarda en "ie".
//Si no hay más líneas, devuelve FALSE.
begin
  Result := true;   //por defecto
  inc(ie);   //pasa al siguiente elemento
  if FinLee then begin  //no hay más datos
    lin0 := '';  //devuelve línea vacía
    exit(false);
  end;
  lin0 := l[ie];   //lee línea
end;
procedure TConvSqlPlus.InicExplorList(lista: TStrings);
begin
  l := lista;  //guarda referecnia
  ie := -1;    //inicia puntero
  InicExplorac;  //prepara exploración

end;
function TConvSqlPlus.LeeRegDeList(var lin: string): boolean;
//Lee un registro de la lista. Si no encuentra alguno, devuelve FALSE.
begin
  if Not hayEncab then begin  //es la primera lectura
    //busca la primera fila con datos
    LeeSig(lin);   //lee primero
    while not FinLee and not ExplorarLin(lin) do begin
      LeeSig(lin);  //lee siguiente
    end;
    if FinLee then exit(false); //no encontró
    //encontró
    Result := true
  end else begin  //ya se ha encontrado un bloque de lectura
    //lee siguiente
    LeeSig(lin);
    if FinLee then exit(false); //no encontró
    if lin = '' then begin
      //puede ser el fin del a página, pero dentro del mismo reporte
      LeeSig(lin);   //ve al siguiente
      if FinLee then exit(false); //no encontró
      //verifica
      if AnsiEndsStr('rows selected.',lin) or AnsiEndsStr('rows selected',lin) then begin
        //definitivamente es el fin del reporte. Auqnue puede haber otro después
//        hayEncab := false;  //prepara para siguiente exploración
        exit(false)
      end else if lin = linEncab then begin
        //lo esperado en la continuación de un reporte
        LeeSig(lin);   //ve al siguiente
        if FinLee then exit(false); //no encontró
        if lin = linMarca then begin
          //lo esperado
          LeeSig(lin);   //ve al siguiente
          if FinLee then exit(false); //no encontró
          Result := ExplorarLin(lin);  //lee dato
        end else begin  //algo está mal
          exit(false);  //se asuem que termina el reporte actual
        end;
      end else begin  //no es el mismo reporte
        exit(false);  //se asuem que termina el reporte actual
      end;
    end else begin
      //procesa línea
      Result := ExplorarLin(lin);
    end;
  end;

end;
//funciones de archivo
procedure TConvSqlPlus.LeeEncabezado(arc: string; CerrarArch: Boolean = True);
{Analiza el archivo "arc" y extrae datos del encabezado en campo[].
Opcionalmente puede dejar abierto el archivo. En este caso, se devuelve
el número de manejador del archivo
Actualiza la variable global "linEncab" con la línea que contiene los
encabezados.}
var
  nar : Integer;
  lin : String;
  tmp : String;
  a : array of String;
  i : Integer;
  x : Integer;
begin
  msjError := '';
  sinDatos := False;
  try
      If arc = '' Then begin
          msjError := 'No se ha definido el archivo de datos';
          Exit;
      End;
{      If Dir(arc) = '' Then begin
          msjError = 'No se encuentra el archivo de datos';
          Exit;
      End;
      nar := FreeFile;
      Open arc For Input As #nar
      If EOF(nar) Then
  '        msjError = 'Archivo vacío.'
          If CerrarArch Then Close #nar Else LeeEncabezadoArc2 = nar
          Exit;
      End If
      Line Input #nar, lin
      If lin <> '' Then
          'No es el formato esperado
          'Verifica si es un mensaje de error.
          Line Input #nar, lin
          If Trim(lin) = '*' Then
              'hay un mensaje de error
              Line Input #nar, tmp    'toma número de línea
              Line Input #nar, lin    'toma mensaje
              'msjError = 'Error SQL: ' & tmp & ' ' & lin
              msjError = tmp & ' ' & lin
              If CerrarArch Then Close #nar Else LeeEncabezadoArc2 = nar
              Exit Function
          Else
              msjError = 'Error en formato de datos.'
              If CerrarArch Then Close #nar Else LeeEncabezadoArc2 = nar
              Exit Function
          End If
      End If
      Line Input #nar, linEncab   'lee encabezados
      If linEncab = 'no rows selected' Then
          sinDatos = True     'indica que no hay datos
          If CerrarArch Then Close #nar Else LeeEncabezadoArc2 = nar
          Exit Function
      End If
      If EOF(nar) Then msjError = 'Archivo incompleto.': Exit Function
      Line Input #nar, lin    'lee marcas
      If Left$(lin, 2) <> '--' Then
          msjError = 'Error en formato de datos.'
          If CerrarArch Then Close #nar Else LeeEncabezadoArc2 = nar
          Exit Function
      End If
      a = Split(lin, ' ') 'separa campos
      nEnc = UBound(a) + 1
      ReDim Enc(nEnc)
      'escribe propiedades
      X = 1
      For i = 1 To nEnc
          Enc(i).nombre = Trim(Mid$(linEncab, X, Len(a(i - 1))))
          Enc(i).etiq = Enc(i).nombre 'etiqueta por defecto
          Enc(i).ind = i    'pone índice
          Enc(i).posIni = X
          Enc(i).nCar = Len(a(i - 1))
          'Enc(i).tipAgr = AG_CTA     'valor por defecto
          'Enc(i).formato = ''        'sin formato
          X = X + Len(a(i - 1)) + 1   'calcula siguiente posición
      Next
      If CerrarArch Then Close #nar Else LeeEncabezadoArc2 = nar
      On Error GoTo 0 'desactiva tratamiento de error}
  except
//      If CerrarArch Then Close #nar Else LeeEncabezadoArc2 = nar
      msjError := 'Error leyendo archivo de datos.';
  end;
end;
//funciones de la clase
constructor TConvSqlPlus.Create;
begin
  bolsa := TStringList.Create;  //crea bolsa
end;
destructor TConvSqlPlus.Destroy;
begin
  bolsa.Free;
  inherited Destroy;
end;

end.

