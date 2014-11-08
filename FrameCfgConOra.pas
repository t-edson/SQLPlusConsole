{
FrameCfgConOra 0.4
==================
Por Tito Hinostroza 20/09/2014

Descripción
===========
Frame de configuración para almacenar y configurar las conexiones disponibles a
una base de datos Oracle. (Para más información sobre Frame de Conmfiguración, ver
https://github.com/t-edson/ConfigFrame)

Las conexiones se manipulan como cadenas para facilitar el alamacenamiento en
disco. La lista lstConex, está personalizada para almacenar todas las propiedades
de la conexión, pero solo muestra el nombre.
Este frane contiene un menú contextual, que puede usarse para cambiar rápidamente
la conexión activa.
También admite un panel de una barra de estado para mostrar la conexión activa,
incluyendo un ícono. El panel se especifica en el me´todo Iniciar() y debe estar
configurado como "psOwnerDraw", para que admita gráficos.

En al rutina de refresco de la barra de estado, se debe llamar a DrawStatusPanel()
para actualizar correctamente el panel usado:

procedure TForm1.StatusBar1DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
begin
  if panel.Index = 0 then begin
    Config.fcConOra.RefresPanConex(StatusBar, Rect);  //refresca
  end;
  ...
end;


                               Por Tito Hinostroza  12/12/2013
}
unit FrameCfgConOra;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, EditBtn, IniFiles,
  Types, Graphics, Dialogs, Menus, ComCtrls, MisUtils,
  ConfigFrame;

type
  TConOra = record
    Nombre    : string;  //nombre de la conexión
    Tipo      : string;  //tipo de conexión
    RutSql    : String;  //Ruta del aplicativo (SQLPLUS o telnet)
    Params    : String;  //parámetros del aplicativo (caena de conexión o IP)
    AbrSes    : boolean; //Bandera para abrir sesión al conectar
  end;

  TEvenCambiaConexMen = procedure of object;
  { TfraCfgConOra }

  TfraCfgConOra = class(TFrame)
  published
    btnNuevo: TButton;
    btnModif: TButton;
    btnElim: TButton;
    btnFijActual: TButton;
    btnProbar: TButton;
    filRuta: TFileNameEdit;
    ImageList1: TImageList;
    lblTipo: TLabel;
    lan: TMemo;
    optDriver: TRadioButton;
    mnConex: TPopupMenu;   //menú contextual de las conexiones
    txtParam: TEdit;
    txtNombre: TEdit;
    lblRuta: TLabel;
    lblParam: TLabel;
    lblNombre: TLabel;
    lstConex: TListBox;
    optSqlplus: TRadioButton;
    optTelnet: TRadioButton;
    //genera constructor y destructor
    procedure btnElimClick(Sender: TObject);
    procedure btnFijActualClick(Sender: TObject);
    procedure btnModifClick(Sender: TObject);
    procedure btnNuevoClick(Sender: TObject);
    procedure btnProbarClick(Sender: TObject);
    constructor Create(AOwner: TComponent) ; override;
    destructor Destroy; override;
    procedure itemClick(Sender: TObject);
    procedure lstConexClick(Sender: TObject);
    procedure lstConexDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure lstConexMeasureItem(Control: TWinControl; Index: Integer;
      var AHeight: Integer);
    procedure optDriverChange(Sender: TObject);
    procedure optSqlplusChange(Sender: TObject);
    procedure optTelnetChange(Sender: TObject);
  private
    PanEstado: TStatusPanel; //referencia al panel de la barra de estado
    procedure ActualizarContr;
    procedure ActualizarEtiq;
    function ConexDeControles: string;
    procedure DeshabControles;
    procedure EstadoInicControles;
    procedure HabilControles;
    procedure RefPanelEstado;
  public
    //parámetros de la conexión
    Conex     : TStringList; //Lista de conexiones
    nConex    : Integer;     //número de conexión actual
    nItemRes  : Integer;     //índice auxiliar para marcar la conexión actual
    //eventos
    procedure Iniciar(secINI0: string; PanEstado0: TStatusPanel); //Inicia el frame
    procedure ReadFileToProp(var arcINI: TIniFile); override;
    procedure PropToWindow; override;
    procedure WindowToProp; override;
    function ConexActual: TConOra;
    procedure DrawStatusPanel(cv: TCanvas; const Rect: TRect);
    procedure LlenaMenuConex;
    procedure ShowPos(x,y: integer); override;
    //funciones para lenguaje
    procedure SetLanguage(lang: string);
  end;

function ConexIgual(cnx1, cnx2: TConOra): boolean; //compara dos conexiones

implementation
{$R *.lfm}
//funciones básicas
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
function ConexIgual(cnx1, cnx2: TConOra): boolean;
begin
  if (cnx1.Nombre = cnx2.Nombre) and
     (cnx1.RutSql = cnx2.RutSql) and
     (cnx1.Params = cnx2.Params) then
    Result := true
  else
    Result := false;
end;
function CadAConex(cad: string): TCOnOra;
var a: TStringDynArray;
begin
   a := Explode(#9,cad);
   Result.Nombre:=a[0];
   Result.Tipo:=a[1];
   Result.RutSql:=a[2];
   Result.Params:=a[3];
   Result.AbrSes := (a[4] = 'V');
end;
function ConexACad(cnx: TConOra): string;
var tmp: string;
begin
   if cnx.AbrSes then tmp := 'V' else tmp := 'F';
   Result := cnx.Nombre + #9 + cnx.Tipo + #9 + cnx.RutSql + #9 +
             cnx.Params + #9 + tmp + #9 + '.';
end;
function TfraCfgConOra.ConexDeControles: string; //lee controles y devuelve una cadena
var
  tip, ses: string;
begin
  if optDriver.Checked then tip:= 'D';
  if optSqlplus.Checked then tip := 'S';
  if optTelnet.Checked then tip := 'T';
  //Obtiene cadena
  Result:= txtNombre.Text + #9 + tip + #9 + filRuta.Caption + #9 +
        txtParam.Text + #9 + 'F' + #9 + '.';  //deja campo para ampliación
end;
procedure TfraCfgConOra.HabilControles; //habilita los controles de edición
begin
  optSqlplus.Enabled:=true;
  optTelnet.Enabled:=true;
  txtNombre.Enabled:=true;
  filRuta.Enabled:=true;
  txtParam.Enabled:=true;

  lblNombre.Enabled:=true;
  lblTipo.Enabled:=true;
  lblRuta.Enabled:=true;
  lblParam.Enabled:=true;

  lstConex.Enabled:=false;  //deshabilita lista
end;

procedure TfraCfgConOra.Iniciar(secINI0: string; PanEstado0: TStatusPanel);
begin
  secINI := secINI0;  //sección INI
  PanEstado := PanEstado0;
  if PanEstado<> nil then
     PanEstado.Style:=psOwnerDraw;  //configura panel para dibujarse por evento
  //asocia propiedades a controles
  Asoc_Int(@nConex, 'nConex', -1);
  Asoc_StrList_TListBox(@Conex, lstConex, 'Conex');
end;

procedure TfraCfgConOra.DeshabControles; //deshabilita los controles
begin
  optSqlplus.Enabled:=false;
  optTelnet.Enabled:=false;
  txtNombre.Enabled:=false;
  filRuta.Enabled:=false;
  txtParam.Enabled:=false;

  lblNombre.Enabled:=false;
  lblTipo.Enabled:=false;
  lblRuta.Enabled:=false;
  lblParam.Enabled:=false;

  lstConex.Enabled:=true;  //habilita lista
end;
procedure TfraCfgConOra.EstadoInicControles;  //fija estado inicial de los controles
begin
  btnNuevo.Visible:=true;
  btnModif.Visible:=true;
  btnElim.Visible:=true;
  btnFijActual.Visible:=true;
  DeshabControles;
end;
procedure TfraCfgConOra.ActualizarEtiq; //actualizar etiquetas de acuerdo al tipo
begin
  if optSqlplus.Checked then begin
    lblRuta.Caption  := dic('Ruta de SQLPLUS:');
    lblParam.Caption := dic('Cadena de conexión:');
  end else begin   //de tipo telnet
    lblRuta.Caption  := dic('Ruta de aplicativo:');
    lblParam.Caption := dic('Parámetros:');
  end;
end;
procedure TfraCfgConOra.ActualizarContr; //actualizar los controles de acuerdo al item seleccionado
var Reg: TConOra;
begin
  if lstConex.ItemIndex=-1 then begin
    //no hay selección
    txtNombre.Caption :='';
    optDriver.Checked := false;
    optSqlplus.Checked:= false;
    optTelnet.Checked := false;
    filRuta.Caption   :='';
    txtParam.Caption  :='';
    exit;
  end;
  //lee registro actual
  Reg := CadAConex(lstConex.Items[lstConex.ItemIndex]);
  txtNombre.Caption:=reg.Nombre;
  if reg.Tipo = 'D' then optDriver.Checked := true;
  if reg.tipo = 'S' then optSqlplus.Checked := true;
  if reg.tipo = 'T' then optTelnet.Checked := true;
  filRuta.Caption  :=reg.RutSql;
  txtParam.Caption :=reg.Params;
  //actualiza etiqueta
  ActualizarEtiq;
end;
procedure TfraCfgConOra.optDriverChange(Sender: TObject);
begin
  ActualizarEtiq;
end;
procedure TfraCfgConOra.optSqlplusChange(Sender: TObject);
begin
  ActualizarEtiq;
end;
procedure TfraCfgConOra.optTelnetChange(Sender: TObject);
begin
  ActualizarEtiq;
end;
constructor TfraCfgConOra.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  conex := TStringList.Create;
  EstadoInicControles;      //restaura estado

  ActualizarContr;
  if lstConex.ItemIndex = -1 then //no hay ítem seleccionado
    DeshabControles;
end;
destructor TfraCfgConOra.Destroy;
begin
  conex.Free;
  inherited Destroy;
end;

procedure TfraCfgConOra.btnNuevoClick(Sender: TObject);  //Agrega nuevo
begin
  if lstConex.Enabled then begin
    //Está en modo normal. Se pide crear una nueva conexión
    //opción por defecto
    optSqlplus.Checked:=true;  //tipo por defecto
    txtNombre.Text := 'Conexión' + IntToStr(lstConex.Count+1); //nombre por defecto
    filRuta.Caption := 'sqlplus'; //ruta por defecto
    txtParam.Caption:= 'usuario/clave@sid';
    ActualizarEtiq;

    //quita selección
    lstConex.ItemIndex:=-1;

    HabilControles;
    //controles en modo de Nuevo
    btnNuevo.Visible:=true;  //lo deja visible
    btnModif.Visible:=false;
    btnElim.Visible:=false;
    btnFijActual.Visible:=false;
  end else begin
    //Está en modo de creación, lo termina.
    if trim(txtNombre.Caption) = '' then begin
      ShowMessage(dic('Debe agregar un nombre.'));
      txtNombre.SetFocus;
      exit;
    end;
    //agrega registro
    lstConex.Items.Add(ConexDeControles);
    EstadoInicControles;      //restaura estado
    //selecciona el último
    lstConex.ItemIndex:=lstConex.Count-1;
    ActualizarContr;
    //se produjo un cambio en las conexiones (del frame, no necesariamente se aceptará)
    LlenaMenuConex;  //actualiza menú contextual
  end;
end;

procedure TfraCfgConOra.btnProbarClick(Sender: TObject);
//var q: TConexOraSqlp;
begin
//  q := TConexOraSqlp.Create(nil);
//  InicConexion(txtNombre, txtParam); //Inicia conexión

//  q.Free;
end;

procedure TfraCfgConOra.btnModifClick(Sender: TObject); //Modifica una conexión
begin
  if lstConex.Enabled then begin
    //Está en modo normal. Se pide iniciar la actualización
    if lstConex.ItemIndex = -1 then exit;
    HabilControles;
    //controles en modo Modificar
    btnNuevo.Visible:=False;
    btnModif.Visible:=true;  //lo deja visible
    btnElim.Visible:=false;
    btnFijActual.Visible:=false;
  end else begin
    //Está en modo de actualización, terminar
    if lstConex.ItemIndex = -1 then begin  //no debería pasar
      EstadoInicControles;      //restaura estado
      exit;
    end;
    if trim(txtNombre.Caption) = '' then begin
      ShowMessage(dic('Debe agregar un nombre.'));
      txtNombre.SetFocus;
      exit;
    end;
    //actualiza registro
    lstConex.Items[lstConex.ItemIndex]:= ConexDeControles;
    EstadoInicControles;      //restaura estado
    //no cambia el elemento seleccionado
    //se produjo un cambio en las conexiones (del frame, no necesariamente se aceptará)
    LlenaMenuConex;  //actualiza menú contextual
  end;
end;
procedure TfraCfgConOra.btnElimClick(Sender: TObject);  //Elimina una conexión
begin
  if lstConex.ItemIndex = -1 then exit;
  lstConex.Items.Delete(lstConex.ItemIndex);
  //se produjo un cambio en las conexiones (del frame, no necesariamente se aceptará)
  LlenaMenuConex;  //actualiza menú contextual
end;
procedure TfraCfgConOra.btnFijActualClick(Sender: TObject); //Fija la conexión actual
begin
  nItemRes := lstConex.ItemIndex;
  lstConex.Invalidate;
end;
function TfraCfgConOra.ConexActual: TConOra;  //devuelve la conexión actual
//Si no hay conexión actual seleccionada, devuelve un registro en blanco.
begin
  Result.Nombre:='';
  Result.RutSql:='';
  Result.Params:='';
  Result.Tipo:='';
  if nConex < 0 then exit;  //no hay seleccionado
  if nConex > Conex.Count-1 then exit; //se sobrepasa
  //toma la conexión actual
  Result := CadAConex(conex[nConex]);
end;

procedure TfraCfgConOra.DrawStatusPanel(cv: TCanvas; const Rect: TRect);
//escribe un texto e ícono con la conex. actual
var
  ca: TConOra;
begin
  ca := ConexActual;
  if ca.Nombre = '' then begin  //no hay conexión
    cv.Font.Color:=clGray;
    cv.TextRect(Rect, 5 + Rect.Left, 2 + Rect.Top, dic('<No hay conexiones>'))
  end else begin  //hay conexión actual
    ImageList1.Draw(cv,Rect.Left + 2, Rect.Top + 1, 0);  //ícono
    cv.Font.Color:=clBlack;
//    cv.Font.Bold:=true;
    cv.TextRect(Rect, 20 + Rect.Left, 2 + Rect.Top, ca.Nombre);  //texto
  end;
end;
procedure TfraCfgConOra.LlenaMenuConex;
//Llena el menú contextual con la lista de conexiones creadas
var item: TMenuItem;
  i: Integer;
  bmp: TCustomBitmap;
  tmp: String;
begin
//showmessage('Llenamenu');
//  if OnCambiaConex<> nil then OnCambiaConex(lstConex.Items);  //dispara evento
  mnConex.Items.Clear;  //limpia
  for i:= 0 to Conex.Count-1 do begin
    item := TMenuItem.Create(nil);
    item.Caption:= CadAConex(Conex[i]).Nombre;  //nombre
    item.ImageIndex:=0;  //asigna imagen
    if i = nConex then  //marca el ítem actual
      item.Default:=true;
    item.OnClick:=@itemClick;
    mnConex.Items.Add(item);
  end;
end;
procedure TfraCfgConOra.itemClick(Sender: TObject);  //respuesta a evento
//Se seleccionó un ítem del menú contextual
var item: TMenuItem;
begin
  item := (Sender as tMenuItem);
  nConex := mnConex.Items.IndexOf(item);  //lee su posición
  LlenaMenuConex;  //para actualizar el ítem actual
  lstConex.Invalidate;  //para forzar a actualizar
  RefPanelEstado;     //actualiza panel de la barra de estado
end;
procedure TfraCfgConOra.RefPanelEstado;
//Refresca el panel del StatusBar asociado, con la conexión activa.
//Se debe llamar cada vez que cambia la conexión activa
begin
  if PanEstado = nil then exit;  //protección
  //fuerza a llamar al evento OnDrawPanel del StatusBar
  PanEstado.StatusBar.InvalidatePanel(PanEstado.Index,[ppText]);
  //y este a us vez debe llamar a DibPanelEstado()
end;

procedure TfraCfgConOra.lstConexClick(Sender: TObject);
begin
  ActualizarContr;
end;
procedure TfraCfgConOra.lstConexDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  txt: string;
  con: TConOra;
begin
  lstConex.Canvas.FillRect(ARect);  //fondo
  //dibuja ícono
  ImageList1.Draw(lstConex.Canvas,ARect.Left + 2, ARect.Top + 1, 0);
  //escribe el texto
  con := CadAConex(lstConex.Items[index]);  //convierte a registro
  txt := con.Nombre;  //solo muestra el nombre
  if lstConex.Enabled then   //selecciona color
    lstConex.Canvas.Font.Color:=clBlack
  else
    lstConex.Canvas.Font.Color:=clGray;
  if Index = nItemRes then  //marca la conexión actual
    lstConex.Canvas.Font.Bold := true
  else
    lstConex.Canvas.Font.Bold := false;
  lstConex.Canvas.TextOut(ARect.left + ImageList1.Width + 4 , ARect.Top + 1, txt);
end;
procedure TfraCfgConOra.lstConexMeasureItem(Control: TWinControl;
  Index: Integer; var AHeight: Integer);
begin
  AHeight := 18;
end;

procedure TfraCfgConOra.ReadFileToProp(var arcINI: TIniFile);
begin
  inherited;
  //se produjo un cambio en las conexiones (del frame, no necesariamente se aceptará)
  LlenaMenuConex;  //actualiza menú contextual
end;
procedure TfraCfgConOra.PropToWindow;  //muestra propiedades en controles
var i: integer;
begin
  EstadoInicControles;      //restaura estado
  inherited;
  nItemRes := nConex;   //para que lo resalte
  ActualizarContr;
end;
procedure TfraCfgConOra.WindowToProp;
var i: integer;
begin
  if not lstConex.Enabled then begin
    //está en modo de Agregar o Modificar
    if btnNuevo.Visible then
      btnNuevoClick(nil)    //agrega
    else if btnModif.Visible then
      btnModifClick(nil);   //modifica
  end;
  inherited;
  nConex := nItemRes;   //lee resaltado
  EstadoInicControles;      //restaura estado
  RefPanelEstado;    //pa actualizar panel
end;
procedure TfraCfgConOra.ShowPos(x, y: integer);  //Muestra frame
begin
  EstadoInicControles;      //restaura estado
  ActualizarContr;
  inherited;
end;

procedure TfraCfgConOra.SetLanguage(lang: string);
//Rutina de traducción
begin
  case lang of
  'es': begin
      //configura mensajes
      dicDel('Ruta de SQLPLUS:');
      dicDel('Cadena de conexión:');
      dicDel('Ruta de aplicativo:');
      dicDel('Parámetros:');
      dicDel('<No hay conexiones>');
      dicDel('Debe agregar un nombre.');
      //fija texto de controles
      BtnNuevo.Caption:='Nuevo';
      BtnModif.Caption:='Modificar';
      BtnElim.Caption:='Eliminar';
      btnFijActual.Caption:='Fijar Actual';
      lblNombre.Caption:='Nombre:';
      lblTipo.Caption:='Tipo de Conexión:';
      btnProbar.Caption:='Probar';
    end;
  'en': begin
      //configura mensajes
      dicSet('Ruta de SQLPLUS:','SQLPLUS Path:');
      dicSet('Cadena de conexión:','Connection String:');
      dicSet('Ruta de aplicativo:','Application path:');
      dicSet('Parámetros:','Parameters:');
      dicSet('<No hay conexiones>','<No connections>');
      dicSet('Debe agregar un nombre.','You must add a name.');
      //fija texto de controles
      BtnNuevo.Caption:='New';
      BtnModif.Caption:='Modify';
      BtnElim.Caption:='Delete';
      btnFijActual.Caption:='Set Current';
      lblNombre.Caption:='Name:';
      lblTipo.Caption:='Connection Type:';
      btnProbar.Caption:='Test';
    end;
  end;
  ActualizarEtiq;  //actualiza etiquetas
end;

end.

