{Resaltador de sintaxis sencillo usado para la salida de datos del SQLPLus
 Se basa en el ejemplo de resaltador con plegado publicado en "La Biblia del SynEdit"

                                     Por Tito Hinostroza 27/06/2014
}
unit SqlPlusHighlighter;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Graphics, SynEditHighlighter, SynEditHighlighterFoldBase,
  strutils;
type
  {Clase para la creación de un resaltador}
  TRangeState = (rsUnknown, rsComment);
  //ID para categorizar a los tokens
  TtkTokenKind = (tkUnknown, tkNull, tkIndentif, tkComment, tkKey, tkSpace, tkString,
                  tkPrompt, tkMensaje, tkError, tkEncab);

  TProcTableProc = procedure of object; //Tipo procedimiento para procesar el
                                        //token por el carácter inicial.
  { TSQLplusHighligh }
  TSQLplusHighligh = class(TSynCustomFoldHighlighter)
  protected
    posIni, posFin: Integer;
    fStringLen: Integer;  //Tamaño del token actual
    fToIdent: PChar;      //Puntero a identificcdor
    linAct   : PChar;
    fProcTable: array[#0..#255] of TProcTableProc; //tabla de procedimientos
    fTokenID : TtkTokenKind;  //Id del token actual
    fRange: TRangeState;
    //define las categorías de los "tokens"
    fAtriIdentif : TSynHighlighterAttributes;
    fAtriComent  : TSynHighlighterAttributes;
    fAtriClave   : TSynHighlighterAttributes;
    fAtriEspac   : TSynHighlighterAttributes;
    fAtriCadena  : TSynHighlighterAttributes;
    fAtriPrompt  : TSynHighlighterAttributes;
    fAtriMensaje : TSynHighlighterAttributes;
    fAtriError   : TSynHighlighterAttributes;
    fAtriEncab   : TSynHighlighterAttributes;
  public
    detecPrompt : boolean;    //activa la detección del prompt
//    prIni, prFin: string;  //cadena inicial, internedia y final del prompt
    procedure SetLine(const NewValue: String; LineNumber: Integer); override;
    procedure Next; override;
    function  GetEol: Boolean; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
              override;
    function  GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetToken: String; override;
    function GetTokenPos: Integer; override;
    function GetTokenKind: integer; override;
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
  private
    posTok     : integer;       //para identificar el ordinal del token en una línea
    procedure CommentProc;
    procedure CreaTablaDeMetodos;
    function KeyComp(const aKey: String): Boolean;
    procedure ProcComent;
    procedure ProcIdent;
    procedure ProcNull;
    procedure ProcSpace;
    procedure ProcString;
    procedure ProcUnknown;

    //Funciones de procesamiento de identificadores
    procedure ProcA;
    procedure ProcB;

    function GetRange: Pointer; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
  end;


implementation
uses FormConfig; //para la detección de prompt
var
  Identifiers: array[#0..#255] of ByteBool;
  mHashTable: array[#0..#255] of Integer;

procedure CreaTablaIdentif;
var  i, j: Char;
begin
  for i := #0 to #255 do
  begin
    Case i of
      '_', '0'..'9', 'a'..'z', 'A'..'Z': Identifiers[i] := True;
    else Identifiers[i] := False;
    end;
    j := UpCase(i);
    Case i in ['_', 'A'..'Z', 'a'..'z'] of
      True: mHashTable[i] := Ord(j) - 64
    else
      mHashTable[i] := 0;
    end;
  end;
end;

constructor TSQLplusHighligh.Create(AOwner: TComponent);
//Constructor de la clase. Aquí se deben crear los atributos a usar.
begin
  inherited Create(AOwner);
  //atributo de identificadores
  fAtriIdentif  := TSynHighlighterAttributes.Create('Identif');
  fAtriIdentif.Foreground := clWhite;    //color de letra
  AddAttribute(fAtriIdentif);
  //atributo de comentarios
  fAtriComent  := TSynHighlighterAttributes.Create('Comment');
//  fAtriComent.Style := [fsItalic];     //en cursiva
  fAtriComent.Foreground := clLtGray;    //color de letra gris
  AddAttribute(fAtriComent);
  //atribuuto de palabras claves
  fAtriClave   := TSynHighlighterAttributes.Create('Key');
  fAtriClave.Style := [fsBold];       //en negrita
  fAtriClave.Foreground:=TColor($40D040);;     //color de letra verde
  AddAttribute(fAtriClave);
  //atributo de espacios. Sin atributos
  fAtriEspac   := TSynHighlighterAttributes.Create('space');
  AddAttribute(fAtriEspac);
  //atributo de cadenas
  fAtriCadena  := TSynHighlighterAttributes.Create('String');
  fAtriCadena.Foreground :=  TColor($FFFF00);   //color de letra celeste
  AddAttribute(fAtriCadena);
  //atributo de prompt
  fAtriPrompt  := TSynHighlighterAttributes.Create('Prompt');
  fAtriPrompt.Foreground := TColor($088A08);   //color de letra
  fAtriPrompt.Style := [fsBold];       //en negrita
//  fAtriPrompt.Background:= clGreen;
  AddAttribute(fAtriPrompt);
  //atributo de directorio
  fAtriMensaje  := TSynHighlighterAttributes.Create('Direct');
  fAtriMensaje.Foreground := clBlue;   //color de letra
  AddAttribute(fAtriMensaje);
  //atributo de error
  fAtriError  := TSynHighlighterAttributes.Create('Error');
  fAtriError.Foreground := clRed;   //color de letra
  fAtriError.Style := [fsBold];       //en negrita
  AddAttribute(fAtriError);
  //atributo de encbezado
  fAtriEncab  := TSynHighlighterAttributes.Create('Encab');
//  fAtriEncab.Foreground := clRed;   //color de letra
  fAtriEncab.Background:= TColor($90FFFF);
//  fAtriEncab.Style := [fsBold];       //en negrita
  AddAttribute(fAtriEncab);

  CreaTablaDeMetodos;  //Construye tabla de métodos
end;

procedure TSQLplusHighligh.CreaTablaDeMetodos;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      '#'    : fProcTable[I] := @ProcComent;
      '"'    : fProcTable[I] := @ProcString;
      'a': fProcTable[I] := @ProcA;
      'b': fProcTable[I] := @ProcB;
      'c'..'z': fProcTable[I] := @ProcIdent;
      'A'..'Z': fProcTable[I] := @ProcIdent;
      #0     : fProcTable[I] := @ProcNull;   //Se lee el caracter de marca de fin de cadena
      #1..#9, #11, #12, #14..#32: fProcTable[I] := @ProcSpace;
      else fProcTable[I] := @ProcUnknown;
    end;
end;
function TSQLplusHighligh.KeyComp(const aKey: String): Boolean;
var
  I: Integer;
  Temp: PChar;
begin
  Temp := fToIdent;
  if Length(aKey) = fStringLen then
  begin
    Result := True;
    for i := 1 to fStringLen do
    begin
      if Temp^ <> aKey[i] then
      begin
        Result := False;
        break;
      end;
      inc(Temp);
    end;
  end else Result := False;
end;
procedure TSQLplusHighligh.ProcComent;
//Procesa el símbolo '#'
begin
  begin
    fTokenID := tkComment;
    inc(PosFin);       //salta a siguiente token
    while not (linAct[PosFin] in [#0, #10, #13]) do Inc(PosFin);
  end;
end;
procedure TSQLplusHighligh.ProcString;
//Procesa el caracter comilla.
begin
  fTokenID := tkString;   //marca como cadena
  Inc(PosFin);
  while (not (linAct[PosFin] in [#0, #10, #13])) do begin
    if linAct[PosFin] = '"' then begin //busca fin de cadena
      Inc(PosFin);
      if (linAct[PosFin] <> '"') then break;  //si no es doble comilla
    end;
    Inc(PosFin);
  end;
end;
procedure TSQLplusHighligh.ProcIdent;
begin
  while Identifiers[linAct[posFin]] do inc(posFin);
  fTokenID := tkIndentif;  //identificador común
end;

procedure TSQLplusHighligh.ProcA;
begin
  while Identifiers[linAct[posFin]] do inc(posFin);
  fStringLen := posFin - posIni - 1;  //calcula tamaño - 1
  fToIdent := linAct + posIni + 1;  //puntero al identificador + 1
  if KeyComp('lias')     then fTokenID := tkKey else
  if KeyComp('propos')     then fTokenID := tkKey else
  if KeyComp('wk')     then fTokenID := tkKey else
    fTokenID := tkIndentif;  //identificador común
end;
procedure TSQLplusHighligh.ProcB;
begin
  while Identifiers[linAct[posFin]] do inc(posFin);
  fStringLen := posFin - posIni - 1;  //calcula tamaño - 1
  fToIdent := linAct + posIni + 1;  //puntero al identificador + 1
  if KeyComp('anner')     then fTokenID := tkKey else
  if KeyComp('reak')     then fTokenID := tkKey else
    fTokenID := tkIndentif;  //identificador común
end;

procedure TSQLplusHighligh.ProcNull;
//Procesa la ocurrencia del caracter #0
begin
  fTokenID := tkNull;   //Solo necesita esto para indicar que se llegó al final de la línae
end;
procedure TSQLplusHighligh.ProcSpace;
//Procesa caracter que es inicio de espacio
begin
  fTokenID := tkSpace;
  repeat
    Inc(posFin);
  until (linAct[posFin] > #32) or (linAct[posFin] in [#0, #10, #13]);
end;
procedure TSQLplusHighligh.ProcUnknown;
begin
  inc(posFin);
  while (linAct[posFin] in [#128..#191]) OR // continued utf8 subcode
   ((linAct[posFin]<>#0) and (fProcTable[linAct[posFin]] = @ProcUnknown)) do inc(posFin);
  fTokenID := tkUnknown;
end;

procedure TSQLplusHighligh.SetLine(const NewValue: String; LineNumber: Integer);
begin
  inherited;
  linAct := PChar(NewValue);  //copia la línea actual
  posFin := 0;                //apunta al primer caracter
  posTok := 0;    //inicia contador de token
  Next;
end;

procedure TSQLplusHighligh.Next;
var
  l: Integer;
begin
  Inc(posTok);  //lleva la cuenta del orden del token
  posIni := PosFin;   //apunta al primer elemento
  //busca prompt en el inicio de la línea
  if (posTok=1) then begin
    //Estamos al inicio
    //verifica si hay prompt
    if detecPrompt then begin
      if AnsiStartsStr('SQL> ', linAct) then begin
        l := length(linAct);
        posFin += l;   //pasa a siguiente token
        fTokenID := tkPrompt;  //de tipo prompt
        EndCodeFoldBlock();  //cierra plegado
        StartCodeFoldBlock(nil);  //abre plegado
        exit;
      end;
    end;
    //verifica si es listado detallado de arcchivos "ls -l"
//    tmp := copy(linAct,1,3);
    if AnsiStartsStr('SQL*Plus: ', linAct) or
       AnsiStartsStr('Copyright (c)', linAct) then begin //un listado común de archivos tiene al menos este tamaño
       //es listado detallado de un directorio
       posFin := length(linAct);
       fTokenID := tkMensaje;  //de tipo directorio
       exit;
    end else if AnsiStartsStr('ERROR at ', linAct) or
       AnsiStartsStr('SP2-0', linAct) then begin //un listado común de archivos tiene al menos este tamaño
       //es listado detallado de un directorio
       posFin := length(linAct);
       fTokenID := tkError;  //de tipo directorio
       exit;
     end else if (CurrentLines<>nil) and
          AnsiStartsStr('--', linAct) then begin //un listado común de archivos tiene al menos este tamaño
        //es listado detallado de un directorio
        posFin := length(linAct);
        fTokenID := tkEncab;  //de tipo encabezado
        exit;
    end;
  end;
  //caso normal
//  if fRange = rsComment then begin
//     CommentProc
//  end else begin
      fRange := rsUnknown;
      fProcTable[linAct[PosFin]]; //Se ejecuta la función que corresponda.
//  end;
end;

function TSQLplusHighligh.GetEol: Boolean;
{Indica cuando se ha llegado al final de la línea}
begin
  Result := fTokenId = tkNull;
end;

procedure TSQLplusHighligh.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
{Devuelve información sobre el token actual}
begin
  TokenLength := posFin - posIni;
  TokenStart := linAct + posIni;
end;

function TSQLplusHighligh.GetTokenAttribute: TSynHighlighterAttributes;
//Devuelve información sobre el token actual
begin
  case fTokenID of
    tkIndentif: Result := fAtriIdentif;
    tkComment : Result := fAtriComent;
    tkKey     : Result := fAtriClave;
    tkSpace   : Result := fAtriEspac;
    tkString  : Result := fAtriCadena;
    tkPrompt  : Result := fAtriPrompt;
    tkMensaje : Result := fAtriMensaje;
    tkError   : Result := fAtriError;
    tkEncab   : Result := fAtriEncab;
    else
      Result := nil;  //tkUnknown, tkNull
  end;
end;
function TSQLplusHighligh.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
{Este método es llamado por la clase "TSynCustomHighlighter", cuando se accede a alguna de
 sus propiedades:  CommentAttribute, IdentifierAttribute, KeywordAttribute, StringAttribute,
 SymbolAttribute o WhitespaceAttribute.}
begin
  case Index of
    SYN_ATTR_COMMENT   : Result := fAtriComent;
    SYN_ATTR_IDENTIFIER: Result := fAtriIdentif;
    SYN_ATTR_KEYWORD   : Result := fAtriClave;
    SYN_ATTR_WHITESPACE: Result := fAtriEspac;
    SYN_ATTR_STRING    : Result := fAtriCadena;
    else Result := nil;
  end;
end;

{Las siguientes funciones, son usadas por SynEdit para el manejo de las
 llaves, corchetes, parentesis y comillas. No son cruciales para el coloreado
 de tokens, pero deben responder bien.}
function TSQLplusHighligh.GetToken: String;
begin
  Result := '';
end;
function TSQLplusHighligh.GetTokenPos: Integer;
begin
  Result := posIni - 1;
end;
function TSQLplusHighligh.GetTokenKind: integer;
begin
  Result := 0;
end;
procedure TSQLplusHighligh.CommentProc;
begin
  fTokenID := tkComment;
  case linAct[PosFin] of
    #0:
      begin
        ProcNull;
        exit;
      end;
  end;
  while linAct[PosFin] <> #0 do
    case linAct[PosFin] of
      '*':
        if linAct[PosFin + 1] = '/' then
        begin
          inc(PosFin, 2);
          fRange := rsUnknown;
          break;
        end
        else inc(PosFin);
      #10: break;
      #13: break;
    else inc(PosFin);
    end;
end;

///////// Implementación de las funcionalidades de rango //////////
procedure TSQLplusHighligh.ResetRange;
begin
  inherited;
  fRange := rsUnknown;
end;
function TSQLplusHighligh.GetRange: Pointer;
begin
  CodeFoldRange.RangeType := Pointer(PtrInt(fRange));
  Result := inherited;
end;
procedure TSQLplusHighligh.SetRange(Value: Pointer);
begin
  inherited;
  fRange := TRangeState(PtrUInt(CodeFoldRange.RangeType));
end;

initialization
   CreaTablaIdentif;   //Crea la tabla para búsqueda rápida
end.

