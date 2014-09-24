{
SQLPlusParser
=============
Por Tito Hinostroza  21/09/2014
* Se modifica ExplorarLin(), para modularizar la parte de extracción de encabezados
y hacerlo accesible al exterior.

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
"uConexOraSqlP"
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
    posIni : integer;  //posición de inicio en el texto
    nCar   : integer;  //tamaño del campo en el texto
    tipCam : TTipoCamSqlP;  //tipo del campo
  end;

  TCamposSqlPlus = array of TCampoSqlPlus;
  { TConvSqlPlus }
  //Objeto para tratamiento de los archivos de texto que genera el SQLPLUS de Oracle
  TConvSqlPlus = class
  public
    msjError: string;
    Enc     : TCamposSqlPlus; //encabezados
    bolsa   : TStringList;    //bolsa para guardar un contenido
    linEncab  : string;   //línea de encabezados
    linMarca  : string;   //línea de marcas
    procedure InicExplorac;
    function ExplorarLin(const cad: String): boolean;
    function ExplorarAcum(const cad: String): boolean;
    function ExplorarDelim(const cad: String; delim: string): boolean;
    function ExploraLinCamBas(const cad: String; nCampoBase: integer): boolean;
    function Campo(n: integer; const cad: String): string;
    //funciones de archivo
    procedure LeeEncabezado(arc: string; CerrarArch: Boolean=True);
    constructor Create;
    destructor Destroy; override;
  private
    hayEncab  : boolean;  //para determinar si se encontró encabezado
    linAnter  : string;   //línea anterior
    sinDatos  : boolean;
  end;
procedure ExtraerCampos(const linMarca, linEncab: string; var campos: TCamposSqlPlus);

implementation
//utilidades
function Explode(delimiter:string; str:string; limit:integer=MaxInt):TStringDynArray;
var
  p,cc,dsize:integer;
begin
  cc := 0;
  dsize := length(delimiter);
  while cc+1 < limit do begin
    p := pos(delimiter,str);
    if p > 0 then begin
      inc(cc);
      setlength(result,cc);
      result[cc-1] := copy(str,1,p-1);
      delete(str,1,p+dsize-1);
    end else break;
  end;
  inc(cc);
  setlength(result,cc);
  result[cc-1] := str;
end;
procedure ExtraerCampos(const linMarca, linEncab: string; var campos: TCamposSqlPlus);
//Extrae los campos de un texto, revisando las líneas de encabezados y de marcas
//SE expone esta función para que sirva para extraer campos, desde fuera.
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
    //intenta deetrminar si es un campo numérico
    if tmp[1] = ' ' then  //campo alineado a la derecha
      campos[i].tipCam:=tcsNum   //se asume numérico
    else
      campos[i].tipCam:=tcsCad;  //cadena por defecto
    x := x + Length(a[i]) + 1;   //calcula siguiente posición
  end;
end;

{ TConvSqlPlus }
procedure TConvSqlPlus.InicExplorac;  //Inicia exploración
begin
  hayEncab := false;  //inicia badnera
  setLength(Enc,0);  //iniica lista de campos
  bolsa.Clear;       //limpia bolsa por si se necesita usarla
end;
function TConvSqlPlus.Campo(n: integer; const cad: String):string;
//Devuelve el campo n-ésimo de la linea "cad". Debe llamarse cuando se ha
//reconocido el encabezado
begin
  //observar que los campos de cdaena que incluyen espacios iniciales o finales
  //aparcerán recortados.
  Result := Trim(Copy(cad,Enc[n].posIni,Enc[n].nCar));
end;
function TConvSqlPlus.ExplorarLin(const cad: String):boolean;
//Explora una línea de datos y extrae el encabezado y determina si la fila
//ingresada es una fila de datos
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
    //verifica si la fila indicada, contiene las marcas
    if (length(cad)>0) and (cad[1] = '-') and
       (cad[length(cad)] = '-') then begin  //primer caracter de marcas
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
      ExtraerCampos(linMarca, linEncab, Enc);
      exit;
    end;
  end;
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

