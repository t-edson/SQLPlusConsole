unit FrameSQLPlusOut;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Grids, ComCtrls,
  LCLType, Buttons, SynEdit, SynEditKeyCmds,
  SynFacilUtils, UnTerminal, SQLPlusConsole, SQLPlusParser, MisUtils;

type

  TSQLPlusOutMode = (spmTextLast, spmContText, spmGrid);

  { TfraSQLPlusOut }

  TfraSQLPlusOut = class(TFrame)
    gridSal: TStringGrid;
    edSal: TSynEdit;
    ImageList1: TImageList;
    procedure FrameEnter(Sender: TObject);
    procedure sqlCon_LineCompleted(const lin: string);
    procedure sqlCon_QueryEnd;
  private
    FMode: TSQLPlusOutMode;
    sqlCon: TSQLPlusCon;
    nFilas : Integer;   //contador para número de filas
    llegoEncabezado: boolean;
    curPanel : TStatusPanel;
    procedure ed_CommandProcessed(Sender: TObject;
      var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
    procedure ed_MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LLenaEncabezGrilla;
    function LLenaGrillaDeEditor: boolean;
    procedure AnchoColumnas(anchos: array of integer);
    procedure LlenaFilas(lin: string; const n: integer);
    procedure SetMode(AValue: TSQLPlusOutMode);
    procedure SetTextMode;
    { private declarations }
  public
    pars: TConvSqlPlus;   //conversor
    property Mode: TSQLPlusOutMode read FMode write SetMode;
    procedure Init(sqlCon0: TSQLPlusCon; CursorPan: TStatusPanel);
    procedure ClearGrid;
    procedure InitOut;
    constructor Create(AOwner: TComponent) ; override;
    destructor Destroy; override;
  end;

implementation
{$R *.lfm}

{ TfraSQLPlusOut }
procedure TfraSQLPlusOut.AnchoColumnas(anchos: array of integer);
//Fija el ancho de las columnas del StringGrid.
var
  i: Integer;
begin
  for i:=0 to high(anchos) do begin
    gridSal.ColWidths[i] := anchos[i];
  end;
end;
procedure TfraSQLPlusOut.LlenaFilas(lin: string; const n: integer);
//Agrega y llena una fila de datos del StringGrid, usando "lin" y la estructura de
//campos de par.Enc. "n" es el número de fila.
var y,x: integer;
    tmp: string;
begin
  gridSal.RowCount:=gridSal.RowCount+1;  //agrega fila
  y:= gridSal.RowCount-1;
  gridSal.Cells[0,y] := IntToStr(n);  //número de fila
  for x:=0 to High(pars.Enc) do begin
//      tmp := Copy(nodHij.dat, nodPad.campos[x].posIni, nodPad.campos[x].nCar);
    tmp := pars.Campo(x, lin); //sqGetColTxt(lin, campos, x);
    gridSal.Cells[x+1,y] := Trim(tmp);
  end;
end;
procedure TfraSQLPlusOut.FrameEnter(Sender: TObject);
//Toma el enfoque
begin
  if FMode in [spmTextLast, spmContText] then edSal.SetFocus
  else gridSal.SetFocus;
end;
procedure TfraSQLPlusOut.LLenaEncabezGrilla;
//Llena el encabezado de la grilla  a partir de par.Enc
var
  i: Integer;
begin
  gridSal.BeginUpdate;

  gridSal.RowCount:=1;
  gridSal.FixedRows:=1;
  gridSal.FixedCols := 1;  { TODO : Puede fallar si ColsCount = 0 }
//  gridSal.ColCount:=High(pars.Enc)+2;  //cantidad de columnas (más la columna fija)
  //Usamos columnas personalizables, en lugar de las columnas normales por facilidad.
  gridSal.Columns.Clear;
  for i:=0 to High(pars.Enc) do gridSal.Columns.Add;
  //define ancho de la columna fija
  gridSal.ColWidths[0] := 25;  //se debe hacer después de definir al menos una fila
  //llena encabezado
  for i:=0 to High(pars.Enc) do begin
    gridSal.Columns[i].Title.Caption:=pars.Enc[i].nombre;
//    gridSal.Columns[i].Title.ImageLayout:=blGlyphLeft;  //íconos a la izquierda
    if pars.Enc[i].tipCam = tcsNum then begin
      //es columna numérica (métrica o KPI)
//      gridSal.Cells[i+1,0] := pars.Enc[i].nombre;
      gridSal.Columns[i].Alignment := taRightJustify;
      gridSal.Columns[i].Title.ImageIndex:=1;
    end else begin
      //es columna de dimensión
//      gridSal.Cells[i+1,0] := pars.Enc[i].nombre;
      gridSal.Columns[i].Title.ImageIndex:=0;
    end;
  end;
  gridSal.EndUpdate(false);
end;
function TfraSQLPlusOut.LLenaGrillaDeEditor: boolean;
//Agrega un reporte a la ventana principal. Si encuentra alguno, devuelve TRUE
var
  lin: String;
begin
  Result := false;    //valor por defecto
  //explora reporte
  if pars.LeeRegDeList(lin) then begin  //busca primer dato
    //Encontró al menos un reporte. Agrega gráfico
    Result := true;
    LLenaEncabezGrilla;
  end;
  if not Result then begin
    //no encontró ningúnFilas reporte
    gridSal.ColCount:=1;  //cantidad de columnas
    gridSal.FixedCols:=0;
    gridSal.RowCount:=2;
    gridSal.FixedRows:=1;
    exit;   //no encontró
  end;
  //busca las otras líneas de datos
  nFilas := 1;
  gridSal.BeginUpdate;
  LlenaFilas(lin, nFilas);  //carga la primera fila leida
  Inc(nFilas);
  while pars.LeeRegDeList(lin) do begin  //lee siguientes
    LlenaFilas(lin, nFilas);
    Inc(nFilas);
  end;
  gridSal.EndUpdate(true);
end;
procedure TfraSQLPlusOut.SetTextMode;  //COnfigura los controles en modo texto
begin
  gridSal.Visible := false;
  edSal.Visible := true;
  edSal.Align := alClient;
end;
procedure TfraSQLPlusOut.SetMode(AValue: TSQLPlusOutMode);
begin
  if FMode=AValue then Exit;
  //cambio de modo
  if (sqlCon<>nil) and (sqlCon.State = ECO_BUSY) then begin
    MsgBox('No se puede cambiar modo en medio de una consulta.');
    exit;
  end;
  if AValue in [spmTextLast, spmContText] then begin
    //Cambia de grilla a modo texto
    SetTextMode;  //muestra lo que había en el editor
    FMode:=AValue;  //actualiza modo
  end else begin //FMode = spmGrid;
    edSal.Visible  :=false;
    gridSal.Visible:=true;
    gridSal.Align:=alClient;
    FMode:=AValue;  //actualiza modo
    //Estaba en modo texto, y debemos actualizar el contenido en la grilla
    gridSal.Clear;
    pars.InicExplorList(edSal.Lines);   //prepara la lectura de las líneas
    if pars.FinLee then exit;
    LLenaGrillaDeEditor;  //carga el primero
  end;
end;
procedure TfraSQLPlusOut.sqlCon_LineCompleted(const lin: string);
//Se usa para llenar la grilla.
//Debe estar asociado al evento "OnLineCompleted", de la conexión o del panel de la
//conexión.
begin
  if pars.ExplorarLin(lin) then begin
    //es una fila de datos
    if not llegoEncabezado then begin
      //llegó el encabezado
      LLenaEncabezGrilla;
      llegoEncabezado:= true;
    end;
    //carga dato
    LlenaFilas(lin, nFilas);  //carga la primera fila leida
    Inc(nFilas);
  end;
end;
procedure TfraSQLPlusOut.sqlCon_QueryEnd;
begin
  gridSal.EndUpdate(true);
end;
procedure TfraSQLPlusOut.ed_CommandProcessed(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
begin
  if curPanel = nil then exit;
  curPanel.Text:= dic('fil=%d, col=%d',[edSal.CaretY, edSal.CaretX]);
end;
procedure TfraSQLPlusOut.ed_MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if curPanel = nil then exit;
  curPanel.Text:= dic('fil=%d, col=%d',[edSal.CaretY, edSal.CaretX]);
end;
procedure TfraSQLPlusOut.Init(sqlCon0: TSQLPlusCon; CursorPan: TStatusPanel);
//Inicia el frame
begin
  sqlCon := sqlCon0; //necesita la referencia a la conexión
  //para actualizar posición de cursor}
  curPanel := CursorPan;
  //configura editor de salida
  if sqlCon = nil then exit;
  edSal.Highlighter := sqlCon.outHL;  //toma el resaltador de sqlCon (se espera que no lo use)
  sqlCon.outHL.detecPrompt:=true;
  edSal.ReadOnly:=true;   //no se espera modificar la salida
//  InicEditorC1(edSal);  requeriría SynFacilUtils
  edSal.Options := edSal.Options - [eoKeepCaretX];  //Quita límite horizontal al cursor
  //para actualizar posición de cursor
  edSal.OnMouseDown:=@ed_MouseDown;
  edSal.OnCommandProcessed:=@ed_CommandProcessed;
end;
procedure TfraSQLPlusOut.ClearGrid;
//Limpia la grilla y prepara la llegada de datos
begin
  gridSal.Clear;
  nFilas := 1;        //para llevar la cuenta de filas
  llegoEncabezado := false;  //para detectar la aparición del encabezado
  pars.InicExplorac;    //inicia la exploración de las líneas que llegarán
end;
procedure TfraSQLPlusOut.InitOut;
//Inicia la salida de datos. Debe llamarse antes de enviar la consulta.
//Esta diseñado para trabajar cuando se maneja una conexión con TSQLPlusCon.
//Si se usa el frame con "FrameExplorBD" con este frame, no se debe llamar este
//método, sino que debe usarse el método TfraExplorBD. SendSQL(), que ya administra
//la salida apropiada.
//Aún se debe mejorar el comportamiento de esta rutina.
begin
  case FMode of
  spmTextLast: begin
      //solo debe mantener el último resultado
      sqlCon.ClearScreen;
      sqlCon.EnableOut;   //por si estaba deshabilitado
      sqlCon.OnLineCompleted:=nil;  //desactiva la salida a la grilla
    end;
  spmContText: begin
      sqlCon.EnableOut;   //por si estaba deshabilitado
      sqlCon.OnLineCompleted:=nil;  //desactiva la salida a la grilla
      //mantiene todo lo que pueda en el editor
    end;
  spmGrid: begin
      ClearGrid;           //debe limpiar la grilla
      sqlCon.ClearScreen;  //limpia tambien el editor para evitar confusión
      //prepara para capturar la salida directamente a la grilla
      sqlCon.DisableAllOut;   //deshabilita la salida de información
      gridSal.BeginUpdate; //EndUpdate se hará en sqlCon_QueryEnd().
      sqlCon.OnLineCompleted:=@sqlCon_LineCompleted;
      sqlCon.OnQueryEnd:=@sqlCon_QueryEnd;
    end;
  end;
end;

constructor TfraSQLPlusOut.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  edSal.Align:=alBottom;
  //configura editor de salida
  InicEditorC1(edSal);
//  edSal.OnKeyDown:=@edSal_KeyDown;
  FMode:=spmTextLast;   //inicia como texto
  SetTextMode;  //inicia controles
  pars := TConvSqlPlus.Create;  //crea motor de lectura de texto
end;
destructor TfraSQLPlusOut.Destroy;
begin
  pars.Destroy;
  inherited Destroy;
end;

end.

