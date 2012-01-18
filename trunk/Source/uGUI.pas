unit uGUI;

interface

uses Windows, Classes, Controls, Graphics, VectorGeometry,
     uMaterialObjects, uBaseClasses,
     uTextures, uMiscUtils, OpenGL1x, OGLStateEmul;

Type
  TMouseButtons = set of TMouseButton;
  TNotifyEvent = procedure (Sender: TObject) of object;
  TMouseEvent = procedure (Sender: TObject; Buttons: TMouseButtons;
    Shift: TShiftState; X, Y: Integer) of object;
  TDragEvent = procedure (Sender: TObject; X, Y: Integer) of object;

  PQuadObject = ^TQuadObject;
  TQuadObject = record
    Vertex: array [0..3] of TAffineVector;
    Color: array [0..3] of TVector;
    TexCoord: array [0..3] of TAffineVector;
    Indices: array[0..5] of GLUInt;
    vOffs,iOffs: integer;
    Updated: boolean;
  end;

  TScrollBarOrientation = (sbHorizontal, sbVertical);

  TGUILayer = class;

  TGUIControl = class
  private
    FParent: TGUILayer;
    function getClientRect: TRect;
  protected
    FOnMouseEnter: TNotifyEvent;
    FMouseEnter: boolean;
    FOnMouseLeave: TNotifyEvent;
    FMouseLeave: boolean;
    FOnMouseDown: TMouseEvent;
    FMouseDown: boolean;
    FOnMouseUp: TMouseEvent;
    FMouseUp: Boolean;
    FOnClick: TNotifyEvent;
    FMouseX,FMouseY: integer;
    FOnStartDrag: TNotifyEvent;
    FOnDragDrop: TDragEvent;
    FonMouseDrag: TMouseEvent;
    FDrag: boolean;
    FVisible: boolean;
    FDragEnable: boolean;
    FLeft, FTop, FWidth, FHeight: integer;

    function getVisible: boolean;virtual;
    procedure SetVisible(const Value: boolean);virtual;

    procedure MouseProc(const pos: TPoint; Buttons: TMouseButtons; Shift: TShiftState);virtual;
    procedure MouseEnter;virtual;
    procedure MouseLeave;virtual;
    procedure MouseDown(Buttons: TMouseButtons; Shift: TShiftState; X, Y: Integer);virtual;
    procedure MouseUp(Buttons: TMouseButtons; Shift: TShiftState; X, Y: Integer);virtual;
    procedure MouseDrag(Buttons: TMouseButtons; Shift: TShiftState; X, Y: Integer);virtual;

    procedure setHeight(const Value: integer);virtual; abstract;
    procedure setLeft(const Value: integer);virtual; abstract;
    procedure setTop(const Value: integer);virtual; abstract;
    procedure setWidth(const Value: integer);virtual; abstract;

  public
    constructor Create;

    //Events
    property onMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property onMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property onClick: TNotifyEvent read FOnClick write FOnCLick;
    property OnStartDrag: TNotifyEvent read FOnStartDrag write FOnStartDrag;
    property OnDragDrop: TDragEvent read FOnDragDrop write FOnDragDrop;
    property onMouseDrag: TMouseEvent read FonMouseDrag write FonMouseDrag;
    //
    property DragEnable: boolean read FDragEnable write FDragEnable;
    //
    property Left: integer read FLeft write setLeft;
    property Top: integer read FTop write setTop;
    property Width: integer read FWidth write setWidth;
    property Height: integer read FHeight write setHeight;
    property ClientRect: TRect read getClientRect;
    property Visible: boolean read getVisible write SetVisible;

  end;

  TGUIObject = class
  private
    FOwner: TGUIControl;
    FQuads: TList;
    function getQuadsCount: integer;
  public
    IndOffset: integer;
    VertOffset: integer;
    Parent: PQuadObject;
    ZOrder: single;
    Visible: boolean;
    constructor Create(vOffs, iOffs: cardinal);
    destructor Destroy; override;
    function AddQuad: PQuadObject;

    property QuadsCount: integer read getQuadsCount;
  end;

  TGUILayer = class
  private
    FTextLayer: TGUILayer;
    FBuilded: boolean;
    vId,tId,cId,iId,VAO: cardinal;
    FZOrder: single;
    FGUIObjectsList: TList;
    FGUIControls: TList;
    FTextList: TList;
    FMaterial: TMaterialObject;
    FWidth: integer;
    FTop: integer;
    FHeight: integer;
    FLeft: integer;
    FControlRised: single;
    FVisible: boolean;
    FvCount,FiCount: integer;
    procedure GetVACount(var VCount, ICount: integer);
    function AddGUIControl(GUIControl: TGUIControl): integer;
    function getVisible: boolean;
    procedure setVisible(const Value: boolean);
    function getICount: integer;
    function getVCount: integer;
    procedure setZOrder(const Value: single);
  public
    constructor Create;
    destructor Destroy; override;
    function AddGUIObject: TGUIObject;
    property ZOrder: single read FZOrder write setZOrder;
    property Material: TMaterialObject read FMaterial;
    property Left: integer read FLeft write FLeft;
    property Top: integer read FTop write FTop;
    property Width: integer read FWidth write FWidth;
    property Height: integer read FHeight write FHeight;
    property VertexCount: integer read getVCount;
    property IndiceCount: integer read getICount;
    property Visible: boolean read getVisible write setVisible;
  end;

  TGLPanel = class(TGUILayer)
  private
    FPanel: TGUIObject;
    FWidth: integer;
    FTop: integer;
    FHeight: integer;
    FLeft: integer;

    procedure UpdateRect;
    procedure setHeight(const Value: integer);
    procedure setLeft(const Value: integer);
    procedure setTop(const Value: integer);
    procedure setWidth(const Value: integer);
    procedure setControlRised(const Value: single);
    function getVisible: boolean;
    procedure setVisible(const Value: boolean);

  public
    constructor Create;
    destructor Destroy; override;

    procedure SetColor(const Color: TVector);
    procedure SetVertGradient(const C1,C2: TVector);

    property Left: integer read FLeft write setLeft;
    property Top: integer read FTop write setTop;
    property Width: integer read FWidth write setWidth;
    property Height: integer read FHeight write setHeight;
    property ControlRised: single read FControlRised write setControlRised;
    property Visible: boolean read getVisible write setVisible;
  end;

  TGUIRender = class (TVBOMeshItem)
  private
    FControls: TList;
    FViewer: array[0..3] of integer;
    FMousePos: TPoint;
    FButtons: TMouseButtons;
    FShift: TShiftState;
    FInWindow: boolean;
    FMouseCaptured: TGUIControl;
    procedure BuildVBO(Layer: TGUILayer);
    procedure UpdateVBO(Layer: TGUILayer; var Quad: PQuadObject);
    procedure BindVBO(Layer: TGUILayer);
    procedure UnBindVBO;
    procedure FreeVBO(Layer: TGUILayer);
    procedure GetMouseState;
    function AbsoluteToLocal(const Pos: TPoint; Left,Top: integer): TPoint;
    procedure UpdateLayer(Layer: TGUILayer);
  public
    property MousePos: TPoint read FMousePos;
    constructor Create;
    destructor Destroy; override;
    function AddNewLayer(aZOrder: single=0): TGUILayer;
    function AddLayer(Layer: TGUILayer): integer;

    procedure DoRender;
    procedure Process; override;
  end;

  TGLStaticText = class (TGUIControl)
  private
    FBitmap: TBitmap;
    FTexture: TTexture;
    FText: string;
    FAutosize: boolean;
    FTextRect: TGUIObject;
    FFont: TFont;
    FFixedWidth, FFixedHeight: integer;
    procedure onFontChanged(Sender: TObject);
    procedure CreateTextTexture(const aText: string);
    procedure setVisible(const Value: boolean); override;
    procedure setText(const Value: string);
    procedure UpdateTextRect(aText: TGUIObject);
  protected
    procedure setHeight(const Value: integer);override;
    procedure setLeft(const Value: integer);override;
    procedure setTop(const Value: integer);override;
    procedure setWidth(const Value: integer);override;
  public
    constructor Create(aParent: TGUILayer);
    destructor Destroy; override;

    property Text: string read FText write setText;
    property Autosize: boolean read FAutosize write FAutosize;
    property Font: TFont read FFont write FFont;
  end;

  TGLButton = class (TGUIControl)
  private
    FPressedButton: TGUIObject;
    FDefaultButton: TGUIObject;
    FPressed: boolean;
    FCaption: TGLStaticText;
    procedure UpdateButtonRect(aButton: TGUIObject);
    procedure setPressed(const Value: boolean);
    procedure setVisible(const Value: boolean); override;
    function getCaption: string;
    procedure setCaption(const Value: string);
  protected
    procedure setHeight(const Value: integer);override;
    procedure setLeft(const Value: integer);override;
    procedure setTop(const Value: integer);override;
    procedure setWidth(const Value: integer);override;
    procedure MouseDown(Buttons: TMouseButtons; Shift: TShiftState; X, Y: Integer);override;
    procedure MouseUp(Buttons: TMouseButtons; Shift: TShiftState; X, Y: Integer);override;
  public
    constructor Create(aParent: TGUILayer);
    destructor Destroy; override;

    property Pressed: boolean read FPressed write setPressed;
    property Caption: string read getCaption write setCaption;
  end;

  TGLCheckBox = class (TGUIControl)
  private
    FCheckedBox: TGUIObject;
    FDefaultBox: TGUIObject;
    FCheckedRBox: TGUIObject;
    FDefaultRBox: TGUIObject;

    FChecked: boolean;
    FGroupId: integer;
    FRadioButton: boolean;

    procedure UpdateBoxRect(aButton: TGUIObject);
    procedure setChecked(const Value: boolean);
    procedure setVisible(const Value: boolean); override;
    procedure setRadioButton(const Value: boolean);
    procedure setGroupId(const Value: integer);
  protected
    procedure setHeight(const Value: integer);override;
    procedure setLeft(const Value: integer);override;
    procedure setTop(const Value: integer);override;
    procedure setWidth(const Value: integer);override;
    procedure MouseDown(Buttons: TMouseButtons; Shift: TShiftState; X, Y: Integer);override;
  public
    constructor Create(aParent: TGUILayer);
    destructor Destroy; override;

    property Checked: boolean read FChecked write setChecked;
    property RadioButton: boolean read FRadioButton write setRadioButton;
    property GroupId: integer read FGroupId write setGroupId;
  end;

  TGLProgressBar = class (TGUIControl)
  private
    FProgressBar: TGUIObject;
    FMinValue: single;
    FMaxValue: single;
    FPosition: single;
    procedure setMaxValue(const Value: single);
    procedure setMinValue(const Value: single);
    procedure setPosition(const Value: single);
    procedure setVisible(const Value: boolean); override;
    procedure UpdateProgressBar(aBar: TGUIObject);
    procedure UpdateProgress;
  protected
    procedure setHeight(const Value: integer);override;
    procedure setLeft(const Value: integer);override;
    procedure setTop(const Value: integer);override;
    procedure setWidth(const Value: integer);override;
  public
    constructor Create(aParent: TGUILayer);
    destructor Destroy; override;

    property Position: single read FPosition write setPosition;
    property MinValue: single read FMinValue write setMinValue;
    property MaxValue: single read FMaxValue write setMaxValue;
  end;

  TGLScrollBar = class (TGUIControl)
  private
    FScrollBar: TGUIObject;
    FMinValue: single;
    FKind: TScrollBarOrientation;
    FMaxValue: single;
    FPosition: single;

    procedure setVisible(const Value: boolean); override;
    procedure UpdateScrollBar(aListBox: TGUIObject);
    procedure UpdatePosition;
    procedure setKind(const Value: TScrollBarOrientation);
    procedure setMaxValue(const Value: single);
    procedure setMinValue(const Value: single);
    procedure setPosition(const Value: single);
  protected
    procedure setHeight(const Value: integer);override;
    procedure setLeft(const Value: integer);override;
    procedure setTop(const Value: integer);override;
    procedure setWidth(const Value: integer);override;
  public
    constructor Create(aParent: TGUILayer);
    destructor Destroy; override;

    property Visible: boolean read getVisible write setVisible;
    property Kind: TScrollBarOrientation read FKind write setKind;
    property Position: single read FPosition write setPosition;
    property MinValue: single read FMinValue write setMinValue;
    property MaxValue: single read FMaxValue write setMaxValue;

  end;

{  TGLListBox = class (TGUIControl)
  private
    FListBox: TGUIObject;
    FItems: TList;

    procedure setVisible(const Value: boolean);
    procedure UpdateListBox(aListBox: TGUIObject);
  protected
    procedure setHeight(const Value: integer);override;
    procedure setLeft(const Value: integer);override;
    procedure setTop(const Value: integer);override;
    procedure setWidth(const Value: integer);override;
  public
    constructor Create(aParent: TGUILayer);
    destructor Destroy; override;

    property Visible: boolean read FVisible write setVisible;
  end;
}
implementation

function inRect(const P: TPoint; const Rect: TRect): boolean;
begin
  if (p.X>=Rect.Left) and (p.X<=Rect.Right)
  and (p.Y>=Rect.Top) and (p.Y<=Rect.Bottom)
  then result:=true else result:=false;
end;

{ TGUIObject }

function TGUIObject.AddQuad;
var Quad: PQuadObject;
begin
  New(Quad); Quad.Updated:=true;
  result:=Quad;
  Quad.vOffs:=VertOffset+FQuads.Count*4;
  Quad.iOffs:=IndOffset+FQuads.Count*6;
  Quad.Indices[0]:=Quad.vOffs;
  Quad.Indices[1]:=Quad.vOffs+1;
  Quad.Indices[2]:=Quad.vOffs+3;
  Quad.Indices[3]:=Quad.vOffs+3;
  Quad.Indices[4]:=Quad.vOffs+1;
  Quad.Indices[5]:=Quad.vOffs+2;

  FQuads.Add(Quad);
end;

constructor TGUIObject.Create;
begin
  FQuads:=TList.Create;
  IndOffset:=iOffs;
  VertOffset:=vOffs;
  Visible:=False;
  FOwner:=nil;
end;

destructor TGUIObject.Destroy;
begin
  FreeList(FQuads);
  inherited;
end;

function TGUIObject.getQuadsCount: integer;
begin
  result:=FQuads.Count;
end;

{ TGUIRender }

function TGUILayer.AddGUIControl(GUIControl: TGUIControl): integer;
begin
  //if GUIControl is TGLStaticText then result:=FTextList.Add(GUIControl) else
  result:=FGUIControls.Add(GUIControl);
end;

function TGUILayer.AddGUIObject: TGUIObject;
var GObj: TGUIObject;
    VCount, ICount: integer;
begin
  GetVACount(VCount, ICount);
  GObj:=TGUIObject.Create(VCount, ICount);
  FGUIObjectsList.Add(GObj);
  result:=GObj;
end;

constructor TGUILayer.Create;
begin
  FZorder:=0; FControlRised:=0;
  FBuilded:=false;
  vId:=0; tId:=0; cId:=0;
  FGUIObjectsList:=TList.Create;
  FGUIControls:=TList.Create;
  FMaterial:=TMaterialObject.Create;
  FTextList:=TList.Create;
  Left:=0; Top:=0; Width:=-1; Height:=-1;
  FvCount:=-1; FiCount:=-1;
  FTextLayer:=nil;
  Visible:=true;
end;

destructor TGUILayer.Destroy;
begin
  if assigned(FTextLayer) then FTextLayer.Free;
  FreeObjectList(FGUIObjectsList);
  FGUIControls.Free;
  FTextList.Free;
  inherited;
end;

function TGUILayer.getICount: integer;
begin
  if FICount=-1 then GetVACount(FvCount,FiCount);
  result:=FiCount;
end;

procedure TGUILayer.GetVACount(var VCount, ICount: integer);
var i: integer;
    GObj: TGUIObject;
begin
  VCount:=0; ICount:=0;
  for i:=0 to FGUIObjectsList.Count-1 do begin
    GObj:=FGUIObjectsList[i];
    VCount:=VCount+GObj.QuadsCount*4;
    ICount:=ICount+GObj.QuadsCount*6;
  end;
end;

function TGUILayer.getVCount: integer;
begin
  if FVCount=-1 then GetVACount(FvCount,FiCount);
  result:=FvCount;
end;

function TGUILayer.getVisible: boolean;
begin
  result:=FVisible;
end;

procedure TGUILayer.setVisible(const Value: boolean);
begin
  FVisible:=Value;
end;

procedure TGUILayer.setZOrder(const Value: single);
begin
  FZOrder := Value;
  if assigned(FTextLayer) then FTextLayer.ZOrder:=FZOrder;
end;

{ TGUIRender }

function TGUIRender.AbsoluteToLocal(const Pos: TPoint; Left,Top: integer): TPoint;
begin
  result.X:=Pos.X-Left;
  result.Y:=Pos.Y-Top;
end;

function TGUIRender.AddLayer(Layer: TGUILayer): integer;
begin
  result:=FControls.Add(Layer);
end;

function TGUIRender.AddNewLayer(aZOrder: single): TGUILayer;
var GuiLayer: TGUILayer;
begin
  GuiLayer:=TGUILayer.Create; GuiLayer.ZOrder:=aZOrder;
  FControls.Add(GuiLayer);
  result:=GuiLayer;
end;

procedure TGUIRender.BindVBO(Layer: TGUILayer);
begin
  if (GL_ARB_vertex_array_object) and (Layer.VAO>0) then
    glBindVertexArray(Layer.vao)
  else begin
    glEnableClientState( GL_COLOR_ARRAY );
    glBindBuffer(GL_ARRAY_BUFFER, Layer.cId );
    glColorPointer(4,GL_FLOAT, 0, nil);

    glClientActiveTexture(GL_TEXTURE0);
    glEnableClientState( GL_TEXTURE_COORD_ARRAY );
    glBindBuffer(GL_ARRAY_BUFFER, Layer.tId );
    glTexCoordPointer(3, GL_FLOAT, SizeOf(TAffineVector), nil);

    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, Layer.iId);

    glEnableClientState( GL_VERTEX_ARRAY );
    glBindBuffer(GL_ARRAY_BUFFER, Layer.vId );
    glVertexPointer( 3, GL_FLOAT, 0, nil );
  end;
end;

procedure TGUIRender.BuildVBO(Layer: TGUILayer);
var vCount, iCount: integer;
begin
  Layer.GetVACount(vCount, iCount);
  glGenBuffers(1,@Layer.vId);
  glGenBuffers(1,@Layer.tId);
  glGenBuffers(1,@Layer.cId);
  glGenBuffers(1,@Layer.iId);

  glBindBuffer(GL_ARRAY_BUFFER, Layer.vId);
  glBufferData(GL_ARRAY_BUFFER, 12* vCount, nil, GL_DYNAMIC_DRAW);

  glBindBuffer(GL_ARRAY_BUFFER, Layer.tId);
  glBufferData(GL_ARRAY_BUFFER, 12* vCount, nil, GL_DYNAMIC_DRAW);

  glBindBuffer(GL_ARRAY_BUFFER, Layer.cId);
  glBufferData(GL_ARRAY_BUFFER, 12 * 4 * vCount, nil, GL_DYNAMIC_DRAW);

  glBindBuffer(GL_ARRAY_BUFFER, 0);

  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, Layer.iId);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, 4*iCount, nil, GL_STATIC_DRAW);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);

  if not (GL_ARB_vertex_array_object) then begin
    Layer.FBuilded:=true; Layer.VAO:=0; exit;
  end;

  glGenVertexArrays(1,@Layer.VAO);
  glBindVertexArray(Layer.vao);

    glEnableClientState( GL_COLOR_ARRAY );
    glBindBuffer(GL_ARRAY_BUFFER, Layer.cId );
    glColorPointer(4,GL_FLOAT, 0, nil);

    glClientActiveTexture(GL_TEXTURE0);
    glEnableClientState( GL_TEXTURE_COORD_ARRAY );
    glBindBuffer(GL_ARRAY_BUFFER, Layer.tId );
    glTexCoordPointer(3, GL_FLOAT, SizeOf(TAffineVector), nil);

    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, Layer.iId);

    glEnableClientState( GL_VERTEX_ARRAY );
    glBindBuffer(GL_ARRAY_BUFFER, Layer.vId );
    glVertexPointer( 3, GL_FLOAT, 0, nil );

  glBindVertexArray(0);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);

  Layer.FBuilded:=true;
end;

constructor TGUIRender.Create;
begin
  inherited;
  FControls:=TList.Create;
  FInWindow:=false;
  FMouseCaptured:=nil;
  FItemType:=mcGUI;
end;

destructor TGUIRender.Destroy;
var i: integer;
    Layer: TGUILayer;
begin
  for i:=0 to FControls.Count-1 do begin
    Layer:=FControls[i]; FreeVBO(Layer); Layer.Free;
  end; FControls.Free;
  inherited;
end;

procedure TGUIRender.DoRender;
var i,j,k: integer;
    Layer: TGUILayer;
    GUIObj: TGUIObject;
    StatText: TGLStaticText;
    Quad: PQuadObject;
    ctrl: TGUIControl;
    pos: TPoint;
    LW,LH: integer;
begin
  if not assigned (FControls) then exit;

  OGLStateEmul.GLStateCache.CheckStates;
  OGLStateEmul.GLStateCache.PushStates;

  glGetIntegerv(GL_VIEWPORT,@FViewer);
  glPushMatrix; glLoadIdentity;
  glMatrixMode(GL_PROJECTION);
  glPushMatrix; glLoadIdentity;
  glOrtho(0,FViewer[2],FViewer[3], 0,-100,100);
  glMatrixMode(GL_MODELVIEW);
  glFrontFace(GL_CCW);
  glEnable(GL_COLOR_MATERIAL);
  glDisable(GL_CULL_FACE);
  glDisable(GL_LIGHTING);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_DEPTH_TEST);
  glDepthMask(true);
  glEnable(GL_SCISSOR_TEST);

  GetMouseState;

  for i:=0 to FControls.Count-1 do begin
    Layer:=FControls[i];
    if Layer.Width<0 then LW:=FViewer[2]-Layer.Left else LW:=Layer.Width;
    if Layer.Height<0 then LH:=FViewer[3]-Layer.Top else LH:=Layer.Height;


    if assigned(Layer) and Layer.Visible then begin
      //Dispatch mouse message
      pos:=AbsoluteToLocal(FMousePos,Layer.Left,Layer.Top);
      if assigned(FMouseCaptured) and (FMouseCaptured.FParent=Layer) then begin
        FMouseCaptured.MouseProc(pos,FButtons, FShift);
        if not FMouseCaptured.FDrag then FMouseCaptured:=nil;
      end else begin
        if inRect(Pos,Rect(0,0,LW, LH)) then begin
          for j:=0 to Layer.FGUIControls.Count-1 do begin
            ctrl:=Layer.FGUIControls[j];
            if ctrl.FVisible then begin
              ctrl.MouseProc(pos,FButtons, FShift);
              if ctrl.FDrag then FMouseCaptured:=ctrl;
            end;
          end;
        end;
      end;

      UpdateLayer(Layer);

      //Render Layer Controls
      if assigned(Layer.Material) then Layer.Material.Texture.Apply(0);
      BindVBO(Layer);
      glPushMatrix; glTranslatef(Layer.Left,Layer.Top,Layer.ZOrder+Layer.FControlRised);
      glScissor(Layer.Left,FViewer[3]-(Layer.Top+LH),LW,LH);
      glDrawElements(GL_TRIANGLES,Layer.IndiceCount,GL_UNSIGNED_INT, nil);
      Layer.Material.Texture.UnApply;
      UnBindVBO;

      if assigned(Layer.FTextLayer) then begin
        UpdateLayer(Layer.FTextLayer); BindVBO(Layer.FTextLayer);
        for j:=0 to Layer.FTextLayer.FTextList.Count-1 do begin
          StatText:=Layer.FTextLayer.FTextList[j];
          if (StatText.Visible) and (StatText.Text<>'') then begin
            if assigned(StatText.FTexture) then StatText.FTexture.Apply(0);
            GUIObj:=StatText.FTextRect;
            glScissor(Layer.Left+StatText.Left,FViewer[3]-Layer.Top-StatText.Top-StatText.FFixedHeight,
              StatText.FFixedWidth,StatText.FFixedHeight);
            glDrawElements(GL_TRIANGLES,GUIObj.FQuads.Count*6,GL_UNSIGNED_INT,
              pointer(GUIObj.IndOffset*4));
          end;
        end;
        //Layer.FTextLayer.Material.Texture.UnApply;
        UnBindVBO;
      end;
      glPopMatrix;
    end;
  end;
  glPopMatrix; glMatrixMode(GL_PROJECTION); glPopMatrix;
  glMatrixMode(GL_MODELVIEW);
  glEnable(GL_LIGHTING);
  glDisable(GL_SCISSOR_TEST);
  OGLStateEmul.GLStateCache.PopStates;
end;

procedure TGUIRender.FreeVBO(Layer: TGUILayer);
begin
  if (GL_ARB_vertex_array_object) then glBindVertexArray(0);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
  glDeleteBuffers(1,@Layer.vId);
  glDeleteBuffers(1,@Layer.tId);
  glDeleteBuffers(1,@Layer.cId);
  glDeleteBuffers(1,@Layer.iId);
  if (GL_ARB_vertex_array_object) then glDeleteVertexArrays(1,@Layer.VAO);
end;

procedure TGUIRender.GetMouseState;
var wnd: HWND;
    DC: HDC;
    rect: TRect;
    key: word;
begin
  dc:=wglGetCurrentDC;
  wnd:=WindowFromDC(dc);
  GetWindowRect(wnd,rect);
  FButtons:=[]; FShift:=[];
  key:=word(GetAsyncKeyState(VK_LBUTTON));
  if key and 32768>0 then include(FShift,ssLeft);
  if key and 1>0 then include(FButtons,mbLeft);

  key:=word(GetAsyncKeyState(VK_RBUTTON));
  if key and 32768>0 then include(FShift,ssRight);
  if key and 1>0 then include(FButtons,mbRight);

  key:=word(GetAsyncKeyState(VK_MBUTTON));
  if key and 32768>0 then include(FShift,ssMiddle);
  if key and 1>0 then include(FButtons,mbMiddle);

  if InRect(Mouse.CursorPos,Rect) then begin
    FMousePos:=Mouse.CursorPos;
    ScreenToClient(wnd,FMousePos);
    FInWindow:=true;
  end else FinWindow:=false;
 //  wnd:=WindowFromPoint(Mouse.CursorPos);
end;

procedure TGUIRender.Process;
begin
  inherited;
  if assigned(FChilde) then begin
    if FChilde.UseParentViewer then
        FChilde.ParentViewer:=FParentViewer;
    if FProcessChilds=pcBefore then FChilde.Process;
  end;
    DoRender;
  if assigned(FChilde) then begin
    if FProcessChilds=pcAfter then FChilde.Process;
  end;
end;

procedure TGUIRender.UnBindVBO;
begin
  if (GL_ARB_vertex_array_object) then glBindVertexArray(0);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
end;

procedure TGUIRender.UpdateLayer(Layer: TGUILayer);
var j,k: integer;
    GUIObj: TGUIObject;
    Quad: PQuadObject;
begin
  //Build Layer VBO
  if not Layer.FBuilded then BuildVBO(Layer);
  UnBindVBO;
  //Update VBO Buffers
  for j:=0 to Layer.FGUIObjectsList.Count-1 do begin
    GUIObj:=Layer.FGUIObjectsList[j];
      for k:=0 to GUIObj.FQuads.Count-1 do begin
        Quad:=GUIObj.FQuads[k];
        if Quad.Updated then UpdateVBO(Layer,Quad);
    end;
  end;
end;

procedure TGUIRender.UpdateVBO(Layer: TGUILayer; var Quad: PQuadObject);
begin
  glBindBuffer(GL_ARRAY_BUFFER, Layer.vId);
  glBufferSubData(GL_ARRAY_BUFFER, Quad.vOffs*12, sizeof(Quad.Vertex),@Quad.Vertex[0]);

  glBindBuffer(GL_ARRAY_BUFFER, Layer.tId);
  glBufferSubData(GL_ARRAY_BUFFER, Quad.vOffs*12, sizeof(Quad.TexCoord),@Quad.TexCoord[0]);

  glBindBuffer(GL_ARRAY_BUFFER, Layer.cId);
  glBufferSubData(GL_ARRAY_BUFFER, Quad.vOffs*16, sizeof(Quad.Color),@Quad.Color[0]);

  glBindBuffer(GL_ARRAY_BUFFER, 0);

  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, Layer.iId);
  glBufferSubData(GL_ELEMENT_ARRAY_BUFFER, Quad.iOffs*4, sizeof(Quad.Indices),@Quad.Indices[0]);

  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);

  Quad.Updated:=false;
end;

{ TGLButton }

constructor TGLButton.Create(aParent: TGUILayer);
var PQuad: PQuadObject;
begin
  //inherited Create;
  aParent.AddGUIControl(inherited Create);

  FParent:=aParent;
  FDefaultButton:=FParent.AddGUIObject;
  FDefaultButton.FOwner:=Self;

      // left side
  PQuad:=FDefaultButton.AddQuad;
  PQuad.TexCoord[0][0]:=41/128; PQuad.TexCoord[0][1]:=95/128;
  PQuad.TexCoord[1][0]:=41/128; PQuad.TexCoord[1][1]:=70/128;
  PQuad.TexCoord[2][0]:=46/128; PQuad.TexCoord[2][1]:=70/128;
  PQuad.TexCoord[3][0]:=46/128; PQuad.TexCoord[3][1]:=95/128;
  PQuad.TexCoord[0][2]:=0; PQuad.TexCoord[1][2]:=0;
  PQuad.TexCoord[2][2]:=0; PQuad.TexCoord[3][2]:=0;
  PQuad.Updated:=true;

      // middle
  PQuad:=FDefaultButton.AddQuad;
  PQuad.TexCoord[0][0]:=46/128; PQuad.TexCoord[0][1]:=95/128;
  PQuad.TexCoord[1][0]:=46/128; PQuad.TexCoord[1][1]:=70/128;
  PQuad.TexCoord[2][0]:=50/128; PQuad.TexCoord[2][1]:=70/128;
  PQuad.TexCoord[3][0]:=50/128; PQuad.TexCoord[3][1]:=95/128;
  PQuad.TexCoord[0][2]:=0; PQuad.TexCoord[1][2]:=0;
  PQuad.TexCoord[2][2]:=0; PQuad.TexCoord[3][2]:=0;

  PQuad.Updated:=true;

      // right side
  PQuad:=FDefaultButton.AddQuad;
  PQuad.TexCoord[0][0]:=50/128; PQuad.TexCoord[0][1]:=95/128;
  PQuad.TexCoord[1][0]:=50/128; PQuad.TexCoord[1][1]:=70/128;
  PQuad.TexCoord[2][0]:=55/128; PQuad.TexCoord[2][1]:=70/128;
  PQuad.TexCoord[3][0]:=55/128; PQuad.TexCoord[3][1]:=95/128;
  PQuad.TexCoord[0][2]:=0; PQuad.TexCoord[1][2]:=0;
  PQuad.TexCoord[2][2]:=0; PQuad.TexCoord[3][2]:=0;

  PQuad.Updated:=true;

  FPressedButton:=FParent.AddGUIObject;
  FPressedButton.FOwner:=Self;

      // left side
  PQuad:=FPressedButton.AddQuad;
  PQuad.TexCoord[0][0]:=57/128; PQuad.TexCoord[0][1]:=95/128;
  PQuad.TexCoord[1][0]:=57/128; PQuad.TexCoord[1][1]:=70/128;
  PQuad.TexCoord[2][0]:=62/128; PQuad.TexCoord[2][1]:=70/128;
  PQuad.TexCoord[3][0]:=62/128; PQuad.TexCoord[3][1]:=95/128;
  PQuad.TexCoord[0][2]:=0; PQuad.TexCoord[1][2]:=0;
  PQuad.TexCoord[2][2]:=0; PQuad.TexCoord[3][2]:=0;

  PQuad.Updated:=true;

      // middle
  PQuad:=FPressedButton.AddQuad;
  PQuad.TexCoord[0][0]:=62/128; PQuad.TexCoord[0][1]:=95/128;
  PQuad.TexCoord[1][0]:=62/128; PQuad.TexCoord[1][1]:=70/128;
  PQuad.TexCoord[2][0]:=66/128; PQuad.TexCoord[2][1]:=70/128;
  PQuad.TexCoord[3][0]:=66/128; PQuad.TexCoord[3][1]:=95/128;
  PQuad.TexCoord[0][2]:=0; PQuad.TexCoord[1][2]:=0;
  PQuad.TexCoord[2][2]:=0; PQuad.TexCoord[3][2]:=0;

  PQuad.Updated:=true;

      // right side
  PQuad:=FPressedButton.AddQuad;
  PQuad.TexCoord[0][0]:=66/128; PQuad.TexCoord[0][1]:=95/128;
  PQuad.TexCoord[1][0]:=66/128; PQuad.TexCoord[1][1]:=70/128;
  PQuad.TexCoord[2][0]:=71/128; PQuad.TexCoord[2][1]:=70/128;
  PQuad.TexCoord[3][0]:=71/128; PQuad.TexCoord[3][1]:=95/128;
  PQuad.TexCoord[0][2]:=0; PQuad.TexCoord[1][2]:=0;
  PQuad.TexCoord[2][2]:=0; PQuad.TexCoord[3][2]:=0;

  PQuad.Updated:=true;

  Pressed:=false;
  FCaption:=TGLStaticText.Create(aParent);
  FCaption.Font.Color:=clBlack;
  FCaption.Autosize:=false;
  FCaption.Text:='';
end;

destructor TGLButton.Destroy;
begin
  FDefaultButton.Free;
  FPressedButton.Free;
  inherited;
end;

function TGLButton.getCaption: string;
begin
  result:=FCaption.Text;
end;

procedure TGLButton.MouseDown(Buttons: TMouseButtons; Shift: TShiftState; X,
  Y: Integer);
begin
  if not FMouseDown then begin
    Pressed:=true;
    FCaption.Left:=FCaption.Left+1;
    FCaption.Top:=FCaption.Top+1;
  end;
  inherited;
end;

procedure TGLButton.MouseUp(Buttons: TMouseButtons; Shift: TShiftState; X,
  Y: Integer);
begin
  if not FMouseUp then begin
    Pressed:=false;
    FCaption.Left:=FCaption.Left-1;
    FCaption.Top:=FCaption.Top-1;
  end;
  inherited;
end;

procedure TGLButton.setCaption(const Value: string);
begin
  FCaption.Text:=Value;
  Width:=FWidth; Height:=FHeight;
end;

procedure TGLButton.setHeight(const Value: integer);
begin
  FHeight := Value;
  UpdateButtonRect(FDefaultButton);
  UpdateButtonRect(FPressedButton);
  if FCaption.Height>FHeight then begin
    FCaption.Height:=FHeight;
    FCaption.Top:=FTop;
  end else begin
    FCaption.Top:=FTop+((FHeight) div 2)-(FCaption.Height div 2)-1;
    FCaption.FFixedHeight:=FCaption.Height;
  end;
end;

procedure TGLButton.setLeft(const Value: integer);
begin
  FLeft := Value;
  UpdateButtonRect(FDefaultButton);
  UpdateButtonRect(FPressedButton);
  if FCaption.Width>FWidth-12 then begin
    FCaption.width:=FWidth-12;
    FCaption.Left:=FLeft+6;
  end else begin
    FCaption.Left:=FLeft+6+((FWidth-12) div 2)-(FCaption.Width div 2);
    FCaption.FFixedWidth:=FCaption.Width;
  end;

end;

procedure TGLButton.setPressed(const Value: boolean);
begin
  FPressed := Value;
  FDefaultButton.Visible:=not FPressed;
  FPressedButton.Visible:=FPressed;
  UpdateButtonRect(FDefaultButton);
  UpdateButtonRect(FPressedButton);
end;

procedure TGLButton.setTop(const Value: integer);
begin
  FTop := Value;
  UpdateButtonRect(FDefaultButton);
  UpdateButtonRect(FPressedButton);
  if FCaption.Height>FHeight then begin
    FCaption.Height:=FHeight;
    FCaption.Top:=FTop;
  end else begin
    FCaption.Top:=FTop+((FHeight) div 2)-(FCaption.Height div 2);
    FCaption.FFixedHeight:=FCaption.Height;
  end;
end;

procedure TGLButton.setVisible(const Value: boolean);
begin
  inherited;
  if FPressed then FPressedButton.Visible:=Value
  else FDefaultButton.Visible:=Value;
  UpdateButtonRect(FDefaultButton);
  UpdateButtonRect(FPressedButton);
end;

procedure TGLButton.setWidth(const Value: integer);
begin
  FWidth := Value;
  UpdateButtonRect(FDefaultButton);
  UpdateButtonRect(FPressedButton);

  if FCaption.Width>FWidth-12 then begin
    FCaption.width:=FWidth-12;
    FCaption.Left:=FLeft+6;
  end else begin
    FCaption.Left:=FLeft+6+((FWidth-12) div 2)-(FCaption.Width div 2);
    FCaption.FFixedWidth:=FCaption.Width;
  end;
end;

procedure TGLButton.UpdateButtonRect(aButton: TGUIObject);
var PQuad: PQuadObject;
    i,j: integer;
begin
  if not assigned(aButton) then exit;
  if not aButton.Visible then begin
    for i:=0 to 2 do begin
      PQuad:=aButton.FQuads[i];
      for j:=0 to 3 do begin
        PQuad.Vertex[j][0]:=0; PQuad.Vertex[j][1]:=0;
      end; PQuad.Updated:=true;
    end; exit;
  end;

  PQuad:=aButton.FQuads[0];
  PQuad.Vertex[0][0]:=FLeft; PQuad.Vertex[0][1]:=FTop;
  PQuad.Vertex[1][0]:=FLeft; PQuad.Vertex[1][1]:=FTop+FHeight;
  PQuad.Vertex[2][0]:=FLeft+6; PQuad.Vertex[2][1]:=FTop+FHeight;
  PQuad.Vertex[3][0]:=FLeft+6; PQuad.Vertex[3][1]:=FTop;
  PQuad.Vertex[0][2]:=0; PQuad.Vertex[1][2]:=0;
  PQuad.Vertex[2][2]:=0; PQuad.Vertex[3][2]:=0;
  PQuad.Color[0]:=Vectormake(1,1,1,1);
  PQuad.Color[1]:=Vectormake(1,1,1,1);
  PQuad.Color[2]:=Vectormake(1,1,1,1);
  PQuad.Color[3]:=Vectormake(1,1,1,1);
  PQuad.Updated:=true;

  PQuad:=aButton.FQuads[1];
  PQuad.Vertex[0][0]:=FLeft+6; PQuad.Vertex[0][1]:=FTop;
  PQuad.Vertex[1][0]:=FLeft+6; PQuad.Vertex[1][1]:=FTop+FHeight;
  PQuad.Vertex[2][0]:=FLeft+FWidth-6; PQuad.Vertex[2][1]:=FTop+FHeight;
  PQuad.Vertex[3][0]:=FLeft+FWidth-6; PQuad.Vertex[3][1]:=FTop;
  PQuad.Vertex[0][2]:=0; PQuad.Vertex[1][2]:=0;
  PQuad.Vertex[2][2]:=0; PQuad.Vertex[3][2]:=0;
  PQuad.Color[0]:=Vectormake(1,1,1,1);
  PQuad.Color[1]:=Vectormake(1,1,1,1);
  PQuad.Color[2]:=Vectormake(1,1,1,1);
  PQuad.Color[3]:=Vectormake(1,1,1,1);

  PQuad.Updated:=true;

  PQuad:=aButton.FQuads[2];
  PQuad.Vertex[0][0]:=FLeft+FWidth-6; PQuad.Vertex[0][1]:=FTop;
  PQuad.Vertex[1][0]:=FLeft+FWidth-6; PQuad.Vertex[1][1]:=FTop+FHeight;
  PQuad.Vertex[2][0]:=FLeft+FWidth; PQuad.Vertex[2][1]:=FTop+FHeight;
  PQuad.Vertex[3][0]:=FLeft+FWidth; PQuad.Vertex[3][1]:=FTop;
  PQuad.Vertex[0][2]:=0; PQuad.Vertex[1][2]:=0;
  PQuad.Vertex[2][2]:=0; PQuad.Vertex[3][2]:=0;

  PQuad.Color[0]:=Vectormake(1,1,1,1);
  PQuad.Color[1]:=Vectormake(1,1,1,1);
  PQuad.Color[2]:=Vectormake(1,1,1,1);
  PQuad.Color[3]:=Vectormake(1,1,1,1);

  PQuad.Updated:=true;
end;

{ TMouseEvents }

procedure TGUIControl.MouseEnter;
begin
  if not FMouseEnter then begin
    if assigned(FOnMouseEnter) then FOnMouseEnter(Self);
    FMouseEnter:=true; FMouseLeave:=false;
  end;
end;

procedure TGUIControl.MouseLeave;
begin
  if not FMouseLeave then begin
    if assigned(FOnMouseLeave) then FOnMouseLeave(Self);
    FMouseLeave:=true; FMouseEnter:=false;
  end;
end;

procedure TGUIControl.MouseProc(const pos: TPoint; Buttons: TMouseButtons;
  Shift: TShiftState);
begin
  if inRect(Pos,ClientRect) or (FDrag and FDragEnable) then begin
    if not FMouseEnter then MouseEnter;
    if (Shift<>[]) and (not FMouseDown) then begin
      MouseDown(Buttons,Shift,Pos.X,Pos.Y);
    end else if (Shift=[]) and (FMouseDown) then
      MouseUP(Buttons,Shift,Pos.X,Pos.Y) else
      if (Shift<>[]) and FMouseDown
      and ((Pos.X-FMouseX<>0)or(Pos.Y-FMouseY<>0)) then
        MouseDrag(Buttons,Shift,Pos.X,Pos.Y);
  end else if not FMouseLeave then begin
    if FMouseDown then MouseUP(Buttons,Shift,Pos.X,Pos.Y);
    MouseLeave;
  end;
end;

constructor TGUIControl.Create;
begin
  inherited;
  FMouseEnter:=false;
  FMouseLeave:=true;
  FMouseDown:=false;
  FMouseUp:=false;
  FDrag:=false;
  FVisible:=True;
  FDragEnable:=false;
end;

function TGUIControl.getClientRect: TRect;
begin
  result.Left:=Left;
  result.Top:=Top;
  result.Right:=Left+Width;
  result.Bottom:=Top+Height;
end;

function TGUIControl.getVisible: boolean;
begin
  result:=FVisible;
end;

procedure TGUIControl.MouseDown(Buttons: TMouseButtons; Shift: TShiftState; X,
  Y: Integer);
begin
  if not FMouseDown then begin
    if assigned(FOnMouseDown) then FOnMouseDown(Self,Buttons,Shift,X,Y)
    else if assigned(FOnClick) then FOnClick(Self);
    FMouseDown:=true; FMouseX:=X; FMouseY:=Y; FMouseUp:=false;
  end else begin
    if (X<>FMouseX) or (Y<>FMouseY) then begin
      if (not FDrag) and assigned(FOnStartDrag) then FOnStartDrag(Self);
      if assigned(FOnMouseDrag) then FOnMouseDrag(Self,Buttons,Shift,X,Y);
      if FDragEnable then begin
        Left:=Left+FMouseX-X;
        Top:=Top+FMouseY-Y;
      end;
      FDrag:=true;
    end;
  end;
end;

procedure TGUIControl.MouseDrag(Buttons: TMouseButtons; Shift: TShiftState; X,
  Y: Integer);
begin
  if (not FDrag) and assigned(FOnStartDrag) then FOnStartDrag(Self);
  if assigned(FOnMouseDrag) then FOnMouseDrag(Self,Buttons,Shift,X,Y);
  if FDragEnable then begin
    Left:=Left-(FMouseX-X); FMouseX:=X;
    Top:=Top-(FMouseY-Y); FMouseY:=Y;
  end;
  FDrag:=true;
end;

procedure TGUIControl.MouseUp(Buttons: TMouseButtons; Shift: TShiftState; X,
  Y: Integer);
begin
  if not FMouseUp then begin
    if assigned(FOnMouseUp) then FOnMouseUp(Self,Buttons,Shift,X,Y);
    FMouseDown:=false; FMouseX:=X; FMouseY:=Y; FMouseUp:=true;
    if FDrag then begin
      if assigned(FOnDragDrop) then FOnDragDrop(Self,x,y);
      FDrag:=false;
    end;
  end;
end;

procedure TGUIControl.SetVisible(const Value: boolean);
begin
  FVisible:=Value;
end;

{ TGLPanel }

constructor TGLPanel.Create;
var PQuad: PQuadObject;
begin
  inherited Create;

  Visible:=true;
  FPanel:=AddGUIObject;
  FPanel.FOwner:=nil;
  FPanel.Visible:=true;
  FControlRised:=0.1;
      // top left corner of panel.
  PQuad:=FPanel.AddQuad;
  PQuad.TexCoord[0][0]:=8/128; PQuad.TexCoord[0][1]:=64/128;
  PQuad.TexCoord[1][0]:=8/128; PQuad.TexCoord[1][1]:=54/128;
  PQuad.TexCoord[2][0]:=18/128; PQuad.TexCoord[2][1]:=54/128;
  PQuad.TexCoord[3][0]:=18/128; PQuad.TexCoord[3][1]:=64/128;
  PQuad.TexCoord[0][2]:=0; PQuad.TexCoord[1][2]:=0;
  PQuad.TexCoord[2][2]:=0; PQuad.TexCoord[3][2]:=0;
  PQuad.Updated:=true;

      // top of panel.
  PQuad:=FPanel.AddQuad;
  PQuad.TexCoord[0][0]:=18/128; PQuad.TexCoord[0][1]:=64/128;
  PQuad.TexCoord[1][0]:=18/128; PQuad.TexCoord[1][1]:=54/128;
  PQuad.TexCoord[2][0]:=30/128; PQuad.TexCoord[2][1]:=54/128;
  PQuad.TexCoord[3][0]:=30/128; PQuad.TexCoord[3][1]:=64/128;
  PQuad.TexCoord[0][2]:=0; PQuad.TexCoord[1][2]:=0;
  PQuad.TexCoord[2][2]:=0; PQuad.TexCoord[3][2]:=0;
  PQuad.Updated:=true;

        // top right corder of panel.
  PQuad:=FPanel.AddQuad;
  PQuad.TexCoord[0][0]:=30/128; PQuad.TexCoord[0][1]:=64/128;
  PQuad.TexCoord[1][0]:=30/128; PQuad.TexCoord[1][1]:=54/128;
  PQuad.TexCoord[2][0]:=40/128; PQuad.TexCoord[2][1]:=54/128;
  PQuad.TexCoord[3][0]:=40/128; PQuad.TexCoord[3][1]:=64/128;
  PQuad.TexCoord[0][2]:=0; PQuad.TexCoord[1][2]:=0;
  PQuad.TexCoord[2][2]:=0; PQuad.TexCoord[3][2]:=0;
  PQuad.Updated:=true;

      // left side of panel.
  PQuad:=FPanel.AddQuad;
  PQuad.TexCoord[0][0]:=8/128; PQuad.TexCoord[0][1]:=54/128;
  PQuad.TexCoord[1][0]:=8/128; PQuad.TexCoord[1][1]:=42/128;
  PQuad.TexCoord[2][0]:=18/128; PQuad.TexCoord[2][1]:=42/128;
  PQuad.TexCoord[3][0]:=18/128; PQuad.TexCoord[3][1]:=54/128;
  PQuad.TexCoord[0][2]:=0; PQuad.TexCoord[1][2]:=0;
  PQuad.TexCoord[2][2]:=0; PQuad.TexCoord[3][2]:=0;
  PQuad.Updated:=true;

      // middle of panel.
  PQuad:=FPanel.AddQuad;
  PQuad.TexCoord[0][0]:=18/128; PQuad.TexCoord[0][1]:=54/128;
  PQuad.TexCoord[1][0]:=18/128; PQuad.TexCoord[1][1]:=42/128;
  PQuad.TexCoord[2][0]:=30/128; PQuad.TexCoord[2][1]:=42/128;
  PQuad.TexCoord[3][0]:=30/128; PQuad.TexCoord[3][1]:=54/128;
  PQuad.TexCoord[0][2]:=0; PQuad.TexCoord[1][2]:=0;
  PQuad.TexCoord[2][2]:=0; PQuad.TexCoord[3][2]:=0;
  PQuad.Updated:=true;

      // right side of panel.
  PQuad:=FPanel.AddQuad;
  PQuad.TexCoord[0][0]:=30/128; PQuad.TexCoord[0][1]:=54/128;
  PQuad.TexCoord[1][0]:=30/128; PQuad.TexCoord[1][1]:=42/128;
  PQuad.TexCoord[2][0]:=40/128; PQuad.TexCoord[2][1]:=42/128;
  PQuad.TexCoord[3][0]:=40/128; PQuad.TexCoord[3][1]:=54/128;
  PQuad.TexCoord[0][2]:=0; PQuad.TexCoord[1][2]:=0;
  PQuad.TexCoord[2][2]:=0; PQuad.TexCoord[3][2]:=0;
  PQuad.Updated:=true;

      // bottom left corner of panel.
  PQuad:=FPanel.AddQuad;
  PQuad.TexCoord[0][0]:=8/128; PQuad.TexCoord[0][1]:=42/128;
  PQuad.TexCoord[1][0]:=8/128; PQuad.TexCoord[1][1]:=32/128;
  PQuad.TexCoord[2][0]:=18/128; PQuad.TexCoord[2][1]:=32/128;
  PQuad.TexCoord[3][0]:=18/128; PQuad.TexCoord[3][1]:=42/128;
  PQuad.TexCoord[0][2]:=0; PQuad.TexCoord[1][2]:=0;
  PQuad.TexCoord[2][2]:=0; PQuad.TexCoord[3][2]:=0;
  PQuad.Updated:=true;

      // bottom middle of panel.
  PQuad:=FPanel.AddQuad;
  PQuad.TexCoord[0][0]:=18/128; PQuad.TexCoord[0][1]:=42/128;
  PQuad.TexCoord[1][0]:=18/128; PQuad.TexCoord[1][1]:=32/128;
  PQuad.TexCoord[2][0]:=30/128; PQuad.TexCoord[2][1]:=32/128;
  PQuad.TexCoord[3][0]:=30/128; PQuad.TexCoord[3][1]:=42/128;
  PQuad.TexCoord[0][2]:=0; PQuad.TexCoord[1][2]:=0;
  PQuad.TexCoord[2][2]:=0; PQuad.TexCoord[3][2]:=0;
  PQuad.Updated:=true;

      // bottom right corner of panel.
  PQuad:=FPanel.AddQuad;
  PQuad.TexCoord[0][0]:=30/128; PQuad.TexCoord[0][1]:=42/128;
  PQuad.TexCoord[1][0]:=30/128; PQuad.TexCoord[1][1]:=32/128;
  PQuad.TexCoord[2][0]:=40/128; PQuad.TexCoord[2][1]:=32/128;
  PQuad.TexCoord[3][0]:=40/128; PQuad.TexCoord[3][1]:=42/128;
  PQuad.TexCoord[0][2]:=0; PQuad.TexCoord[1][2]:=0;
  PQuad.TexCoord[2][2]:=0; PQuad.TexCoord[3][2]:=0;
  PQuad.Updated:=true;

end;

destructor TGLPanel.Destroy;
begin

  inherited;
end;

function TGLPanel.getVisible: boolean;
begin
  result:=inherited Visible;
end;

procedure TGLPanel.SetColor(const Color: TVector);
var i: integer;
    pQuad: PQuadObject;
begin
  for i:=0 to FPanel.FQuads.Count-1 do begin
    pQuad:=FPanel.FQuads[i]; pQuad.Updated:=true;
    pQuad.Color[0]:=Color; pQuad.Color[1]:=Color;
    pQuad.Color[2]:=Color; pQuad.Color[3]:=Color;
  end;
end;

procedure TGLPanel.setControlRised(const Value: single);
begin
  FControlRised := Value; UpdateRect;
end;


procedure TGLPanel.setHeight(const Value: integer);
begin
  FHeight:=Value; inherited Height:=value; UpdateRect;
end;

procedure TGLPanel.setLeft(const Value: integer);
begin
  FLeft:=Value; inherited Left:=Value; UpdateRect;
end;

procedure TGLPanel.setTop(const Value: integer);
begin
  FTop:=Value; inherited Top:=value; UpdateRect;
end;

procedure TGLPanel.SetVertGradient(const C1, C2: TVector);
var i: integer;
    pQuad: PQuadObject;
begin
  for i:=0 to FPanel.FQuads.Count-1 do begin
    pQuad:=FPanel.FQuads[i]; pQuad.Updated:=true;
    case i of
      0..2: begin
        pQuad.Color[1]:=C2; pQuad.Color[2]:=C2;
        pQuad.Color[0]:=C2; pQuad.Color[3]:=C2;
      end;
      6..8: begin
        pQuad.Color[1]:=C1; pQuad.Color[2]:=C1;
        pQuad.Color[0]:=C1; pQuad.Color[3]:=C1;
      end;
      else begin
        pQuad.Color[1]:=C1; pQuad.Color[2]:=C1;
        pQuad.Color[0]:=C2; pQuad.Color[3]:=C2;
      end;
    end;
  end;
end;

procedure TGLPanel.setVisible(const Value: boolean);
begin
  inherited Visible:=Value;
  UpdateRect;
end;

procedure TGLPanel.setWidth(const Value: integer);
begin
  FWidth:=Value; inherited Width:=value; UpdateRect;
end;

procedure TGLPanel.UpdateRect;
var PQuad: PQuadObject;
    i,j: integer;
begin
  if not assigned(FPanel) then exit;
  if not Visible then begin
    for i:=0 to 8 do begin
      PQuad:=FPanel.FQuads[i];
      for j:=0 to 3 do begin
        PQuad.Vertex[j][0]:=0; PQuad.Vertex[j][1]:=0;
      end; PQuad.Updated:=true;
    end; exit;
  end;

  PQuad:=FPanel.FQuads[0];
  PQuad.Vertex[0][0]:=0; PQuad.Vertex[0][1]:=0;
  PQuad.Vertex[1][0]:=0; PQuad.Vertex[1][1]:=0+10;
  PQuad.Vertex[2][0]:=0+10; PQuad.Vertex[2][1]:=0+10;
  PQuad.Vertex[3][0]:=0+10; PQuad.Vertex[3][1]:=0;
  PQuad.Vertex[0][2]:=-FControlRised; PQuad.Vertex[1][2]:=-FControlRised;
  PQuad.Vertex[2][2]:=-FControlRised; PQuad.Vertex[3][2]:=-FControlRised;
  PQuad.Color[0]:=Vectormake(1,1,1,1);
  PQuad.Color[1]:=Vectormake(1,1,1,1);
  PQuad.Color[2]:=Vectormake(1,1,1,1);
  PQuad.Color[3]:=Vectormake(1,1,1,1);
  PQuad.Updated:=true;

  PQuad:=FPanel.FQuads[1];
  PQuad.Vertex[0][0]:=0+10; PQuad.Vertex[0][1]:=0;
  PQuad.Vertex[1][0]:=0+10; PQuad.Vertex[1][1]:=0+10;
  PQuad.Vertex[2][0]:=0+10+FWidth-20; PQuad.Vertex[2][1]:=0+10;
  PQuad.Vertex[3][0]:=0+10+FWidth-20; PQuad.Vertex[3][1]:=0;
  PQuad.Vertex[0][2]:=-FControlRised; PQuad.Vertex[1][2]:=-FControlRised;
  PQuad.Vertex[2][2]:=-FControlRised; PQuad.Vertex[3][2]:=-FControlRised;
  PQuad.Color[0]:=Vectormake(1,1,1,1);
  PQuad.Color[1]:=Vectormake(1,1,1,1);
  PQuad.Color[2]:=Vectormake(1,1,1,1);
  PQuad.Color[3]:=Vectormake(1,1,1,1);
  PQuad.Updated:=true;

  PQuad:=FPanel.FQuads[2];
  PQuad.Vertex[0][0]:=0+FWidth-10; PQuad.Vertex[0][1]:=0;
  PQuad.Vertex[1][0]:=0+FWidth-10; PQuad.Vertex[1][1]:=0+10;
  PQuad.Vertex[2][0]:=0+FWidth; PQuad.Vertex[2][1]:=0+10;
  PQuad.Vertex[3][0]:=0+FWidth; PQuad.Vertex[3][1]:=0;
  PQuad.Vertex[0][2]:=-FControlRised; PQuad.Vertex[1][2]:=-FControlRised;
  PQuad.Vertex[2][2]:=-FControlRised; PQuad.Vertex[3][2]:=-FControlRised;
  PQuad.Color[0]:=Vectormake(1,1,1,1);
  PQuad.Color[1]:=Vectormake(1,1,1,1);
  PQuad.Color[2]:=Vectormake(1,1,1,1);
  PQuad.Color[3]:=Vectormake(1,1,1,1);
  PQuad.Updated:=true;

  PQuad:=FPanel.FQuads[3];
  PQuad.Vertex[0][0]:=0; PQuad.Vertex[0][1]:=0+10;
  PQuad.Vertex[1][0]:=0; PQuad.Vertex[1][1]:=0+FHeight-10;
  PQuad.Vertex[2][0]:=0+10; PQuad.Vertex[2][1]:=0+FHeight-10;
  PQuad.Vertex[3][0]:=0+10; PQuad.Vertex[3][1]:=0+10;
  PQuad.Vertex[0][2]:=-FControlRised; PQuad.Vertex[1][2]:=-FControlRised;
  PQuad.Vertex[2][2]:=-FControlRised; PQuad.Vertex[3][2]:=-FControlRised;
  PQuad.Color[0]:=Vectormake(1,1,1,1);
  PQuad.Color[1]:=Vectormake(1,1,1,1);
  PQuad.Color[2]:=Vectormake(1,1,1,1);
  PQuad.Color[3]:=Vectormake(1,1,1,1);
  PQuad.Updated:=true;

  PQuad:=FPanel.FQuads[4];
  PQuad.Vertex[0][0]:=0+10; PQuad.Vertex[0][1]:=0+10;
  PQuad.Vertex[1][0]:=0+10; PQuad.Vertex[1][1]:=0+FHeight-10;
  PQuad.Vertex[2][0]:=0+10+FWidth-20; PQuad.Vertex[2][1]:=0+FHeight-10;
  PQuad.Vertex[3][0]:=0+10+FWidth-20; PQuad.Vertex[3][1]:=0+10;
  PQuad.Vertex[0][2]:=-FControlRised; PQuad.Vertex[1][2]:=-FControlRised;
  PQuad.Vertex[2][2]:=-FControlRised; PQuad.Vertex[3][2]:=-FControlRised;
  PQuad.Color[0]:=Vectormake(1,1,1,1);
  PQuad.Color[1]:=Vectormake(1,1,1,1);
  PQuad.Color[2]:=Vectormake(1,1,1,1);
  PQuad.Color[3]:=Vectormake(1,1,1,1);
  PQuad.Updated:=true;

  PQuad:=FPanel.FQuads[5];
  PQuad.Vertex[0][0]:=0+FWidth-10; PQuad.Vertex[0][1]:=0+10;
  PQuad.Vertex[1][0]:=0+FWidth-10; PQuad.Vertex[1][1]:=0+FHeight-10;
  PQuad.Vertex[2][0]:=0+FWidth; PQuad.Vertex[2][1]:=0+FHeight-10;
  PQuad.Vertex[3][0]:=0+FWidth; PQuad.Vertex[3][1]:=0+10;
  PQuad.Vertex[0][2]:=-FControlRised; PQuad.Vertex[1][2]:=-FControlRised;
  PQuad.Vertex[2][2]:=-FControlRised; PQuad.Vertex[3][2]:=-FControlRised;
  PQuad.Color[0]:=Vectormake(1,1,1,1);
  PQuad.Color[1]:=Vectormake(1,1,1,1);
  PQuad.Color[2]:=Vectormake(1,1,1,1);
  PQuad.Color[3]:=Vectormake(1,1,1,1);
  PQuad.Updated:=true;

  PQuad:=FPanel.FQuads[6];
  PQuad.Vertex[0][0]:=0; PQuad.Vertex[0][1]:=0+FHeight-10;
  PQuad.Vertex[1][0]:=0; PQuad.Vertex[1][1]:=0+FHeight;
  PQuad.Vertex[2][0]:=0+10; PQuad.Vertex[2][1]:=0+FHeight;
  PQuad.Vertex[3][0]:=0+10; PQuad.Vertex[3][1]:=0+FHeight-10;
  PQuad.Vertex[0][2]:=-FControlRised; PQuad.Vertex[1][2]:=-FControlRised;
  PQuad.Vertex[2][2]:=-FControlRised; PQuad.Vertex[3][2]:=-FControlRised;
  PQuad.Color[0]:=Vectormake(1,1,1,1);
  PQuad.Color[1]:=Vectormake(1,1,1,1);
  PQuad.Color[2]:=Vectormake(1,1,1,1);
  PQuad.Color[3]:=Vectormake(1,1,1,1);
  PQuad.Updated:=true;

  PQuad:=FPanel.FQuads[7];
  PQuad.Vertex[0][0]:=0+10; PQuad.Vertex[0][1]:=0+FHeight-10;
  PQuad.Vertex[1][0]:=0+10; PQuad.Vertex[1][1]:=0+FHeight;
  PQuad.Vertex[2][0]:=0+10+FWidth-20; PQuad.Vertex[2][1]:=0+FHeight;
  PQuad.Vertex[3][0]:=0+10+FWidth-20; PQuad.Vertex[3][1]:=0+FHeight-10;
  PQuad.Vertex[0][2]:=-FControlRised; PQuad.Vertex[1][2]:=-FControlRised;
  PQuad.Vertex[2][2]:=-FControlRised; PQuad.Vertex[3][2]:=-FControlRised;
  PQuad.Color[0]:=Vectormake(1,1,1,1);
  PQuad.Color[1]:=Vectormake(1,1,1,1);
  PQuad.Color[2]:=Vectormake(1,1,1,1);
  PQuad.Color[3]:=Vectormake(1,1,1,1);
  PQuad.Updated:=true;

  PQuad:=FPanel.FQuads[8];
  PQuad.Vertex[0][0]:=0+FWidth-10; PQuad.Vertex[0][1]:=0+FHeight-10;
  PQuad.Vertex[1][0]:=0+FWidth-10; PQuad.Vertex[1][1]:=0+FHeight;
  PQuad.Vertex[2][0]:=0+FWidth; PQuad.Vertex[2][1]:=0+FHeight;
  PQuad.Vertex[3][0]:=0+FWidth; PQuad.Vertex[3][1]:=0+FHeight-10;
  PQuad.Vertex[0][2]:=-FControlRised; PQuad.Vertex[1][2]:=-FControlRised;
  PQuad.Vertex[2][2]:=-FControlRised; PQuad.Vertex[3][2]:=-FControlRised;
  PQuad.Color[0]:=Vectormake(1,1,1,1);
  PQuad.Color[1]:=Vectormake(1,1,1,1);
  PQuad.Color[2]:=Vectormake(1,1,1,1);
  PQuad.Color[3]:=Vectormake(1,1,1,1);
  PQuad.Updated:=true;

end;

{ TGLCheckBox }

constructor TGLCheckBox.Create(aParent: TGUILayer);
var PQuad: PQuadObject;
begin
  aParent.AddGUIControl(inherited Create);
  FLeft:=0; FTop:=0; FWidth:=16; FHeight:=16;

  FParent:=aParent; FGroupId:=0;
  FDefaultBox:=FParent.AddGUIObject;
  FDefaultBox.FOwner:=Self;
      // UnChecked box
  PQuad:=FDefaultBox.AddQuad;
  PQuad.TexCoord[0][0]:=72/128; PQuad.TexCoord[0][1]:=96/128;
  PQuad.TexCoord[1][0]:=72/128; PQuad.TexCoord[1][1]:=82/128;
  PQuad.TexCoord[2][0]:=86/128; PQuad.TexCoord[2][1]:=82/128;
  PQuad.TexCoord[3][0]:=86/128; PQuad.TexCoord[3][1]:=96/128;
  PQuad.TexCoord[0][2]:=0; PQuad.TexCoord[1][2]:=0;
  PQuad.TexCoord[2][2]:=0; PQuad.TexCoord[3][2]:=0;
  PQuad.Updated:=true;

  FCheckedBox:=FParent.AddGUIObject;
  FCheckedBox.FOwner:=Self;
      // Checked box
  PQuad:=FCheckedBox.AddQuad;
  PQuad.TexCoord[0][0]:=88/128; PQuad.TexCoord[0][1]:=96/128;
  PQuad.TexCoord[1][0]:=88/128; PQuad.TexCoord[1][1]:=82/128;
  PQuad.TexCoord[2][0]:=102/128; PQuad.TexCoord[2][1]:=82/128;
  PQuad.TexCoord[3][0]:=102/128; PQuad.TexCoord[3][1]:=96/128;
  PQuad.TexCoord[0][2]:=0; PQuad.TexCoord[1][2]:=0;
  PQuad.TexCoord[2][2]:=0; PQuad.TexCoord[3][2]:=0;
  PQuad.Updated:=true;

  FDefaultRBox:=FParent.AddGUIObject;
  FDefaultRBox.FOwner:=Self;
      // UnChecked RadioButton
  PQuad:=FDefaultRBox.AddQuad;
  PQuad.TexCoord[0][0]:=72/128; PQuad.TexCoord[0][1]:=80/128;
  PQuad.TexCoord[1][0]:=72/128; PQuad.TexCoord[1][1]:=66/128;
  PQuad.TexCoord[2][0]:=86/128; PQuad.TexCoord[2][1]:=66/128;
  PQuad.TexCoord[3][0]:=86/128; PQuad.TexCoord[3][1]:=80/128;
  PQuad.TexCoord[0][2]:=0; PQuad.TexCoord[1][2]:=0;
  PQuad.TexCoord[2][2]:=0; PQuad.TexCoord[3][2]:=0;
  PQuad.Updated:=true;

  FCheckedRBox:=FParent.AddGUIObject;
  FCheckedRBox.FOwner:=Self;
      // Checked RadioButton
  PQuad:=FCheckedRBox.AddQuad;
  PQuad.TexCoord[0][0]:=88/128; PQuad.TexCoord[0][1]:=80/128;
  PQuad.TexCoord[1][0]:=88/128; PQuad.TexCoord[1][1]:=66/128;
  PQuad.TexCoord[2][0]:=102/128; PQuad.TexCoord[2][1]:=66/128;
  PQuad.TexCoord[3][0]:=102/128; PQuad.TexCoord[3][1]:=80/128;
  PQuad.TexCoord[0][2]:=0; PQuad.TexCoord[1][2]:=0;
  PQuad.TexCoord[2][2]:=0; PQuad.TexCoord[3][2]:=0;
  PQuad.Updated:=true;

  Checked:=false; FRadioButton:=false;
end;

destructor TGLCheckBox.Destroy;
begin
  FCheckedBox.Free;
  FDefaultBox.Free;
  FCheckedRBox.Free;
  FDefaultRBox.Free;
  inherited;
end;

procedure TGLCheckBox.MouseDown(Buttons: TMouseButtons; Shift: TShiftState; X,
  Y: Integer);
begin
  if not FMouseDown then Checked:=not Checked;
  inherited;
end;

procedure TGLCheckBox.setChecked(const Value: boolean);
var i: integer;
    gc: TGUIControl;
begin
  FChecked:=Value;
  if not FRadioButton then begin
    FCheckedBox.Visible:=FChecked;
    FDefaultBox.Visible:=not FChecked;
  end else begin
    FCheckedRBox.Visible:=FChecked;
    FDefaultRBox.Visible:=not FChecked;
    if FChecked then
    for i:=0 to FParent.FGUIControls.Count-1 do begin
      gc:=FParent.FGUIControls[i];
      if (gc<>self) and (gc is TGLCheckBox)
      and (TGLCheckBox(gc).RadioButton)
      and (TGLCheckBox(gc).FGroupId=FGroupId)
      and (TGLCheckBox(gc).Checked)
      then TGLCheckBox(gc).Checked:=false;
    end;
  end;
  UpdateBoxRect(FDefaultBox);
  UpdateBoxRect(FDefaultRBox);
  UpdateBoxRect(FCheckedBox);
  UpdateBoxRect(FCheckedRBox);
end;

procedure TGLCheckBox.setGroupId(const Value: integer);
begin
  FGroupId := Value; setChecked(true);
end;

procedure TGLCheckBox.setHeight(const Value: integer);
begin
  FHeight := Value;
  UpdateBoxRect(FDefaultBox);
  UpdateBoxRect(FDefaultRBox);
  UpdateBoxRect(FCheckedBox);
  UpdateBoxRect(FCheckedRBox);
end;

procedure TGLCheckBox.setLeft(const Value: integer);
begin
  FLeft := Value;
  UpdateBoxRect(FDefaultBox);
  UpdateBoxRect(FDefaultRBox);
  UpdateBoxRect(FCheckedBox);
  UpdateBoxRect(FCheckedRBox);
end;

procedure TGLCheckBox.setRadioButton(const Value: boolean);
begin
  FRadioButton := Value;
  if FRadioButton then begin
    FCheckedBox.Visible:=false;
    FDefaultBox.Visible:=false;
  end else begin
    FCheckedRBox.Visible:=false;
    FDefaultRBox.Visible:=false;
  end;
  setChecked(true);
end;

procedure TGLCheckBox.setTop(const Value: integer);
begin
  FTop := Value;
  UpdateBoxRect(FDefaultBox);
  UpdateBoxRect(FDefaultRBox);
  UpdateBoxRect(FCheckedBox);
  UpdateBoxRect(FCheckedRBox);
end;

procedure TGLCheckBox.setVisible(const Value: boolean);
begin
  inherited;
  if not FRadioButton then begin
    if FChecked then FCheckedBox.Visible:=Value
    else FDefaultBox.Visible:=Value;
  end else begin
    if FChecked then FCheckedRBox.Visible:=Value
    else FDefaultRBox.Visible:=Value;
  end;
  UpdateBoxRect(FDefaultBox);
  UpdateBoxRect(FDefaultRBox);
  UpdateBoxRect(FCheckedBox);
  UpdateBoxRect(FCheckedRBox);
end;

procedure TGLCheckBox.setWidth(const Value: integer);
begin
  FWidth := Value;
  UpdateBoxRect(FDefaultBox);
  UpdateBoxRect(FDefaultRBox);
  UpdateBoxRect(FCheckedBox);
  UpdateBoxRect(FCheckedRBox);
end;

procedure TGLCheckBox.UpdateBoxRect(aButton: TGUIObject);
var PQuad: PQuadObject;
    j: integer;
begin
  if not assigned(aButton) then exit;
  if not aButton.Visible then begin
    PQuad:=aButton.FQuads[0];
    for j:=0 to 3 do begin
      PQuad.Vertex[j][0]:=0; PQuad.Vertex[j][1]:=0;
    end; PQuad.Updated:=true;
    exit;
  end;
  PQuad:=aButton.FQuads[0];
  PQuad.Vertex[0][0]:=FLeft+1; PQuad.Vertex[0][1]:=FTop+1;
  PQuad.Vertex[1][0]:=FLeft+1; PQuad.Vertex[1][1]:=FTop+FHeight-2;
  PQuad.Vertex[2][0]:=FLeft+FWidth-2; PQuad.Vertex[2][1]:=FTop+FHeight-2;
  PQuad.Vertex[3][0]:=FLeft+FWidth-2; PQuad.Vertex[3][1]:=FTop+1;
  PQuad.Vertex[0][2]:=0.1; PQuad.Vertex[1][2]:=0.1;
  PQuad.Vertex[2][2]:=0.1; PQuad.Vertex[3][2]:=0.1;
  PQuad.Color[0]:=Vectormake(1,1,1,1);
  PQuad.Color[1]:=Vectormake(1,1,1,1);
  PQuad.Color[2]:=Vectormake(1,1,1,1);
  PQuad.Color[3]:=Vectormake(1,1,1,1);
  PQuad.Updated:=true;
end;

{ TGLStaticText }

constructor TGLStaticText.Create(aParent: TGUILayer);
var PQuad: PQuadObject;
begin
  FTexture:=nil;
  FFont:=TFont.Create;
  FFont.Name:='MS Sans Serif';
  FBitmap:=TBitmap.Create;
  FBitmap.Canvas.Font.Assign(FFont);
//  FBitmap.Canvas.Font.Quality:=fqNonAntialiased;
  FFont.OnChange:=onFontChanged;
  FAutosize:=true;

  if not assigned(aParent.FTextLayer) then begin
    aParent.FTextLayer:=TGUILayer.Create;
    aParent.FTextLayer.ZOrder:=aParent.ZOrder;
  end;

  FParent:=aParent.FTextLayer;
  //FParent:=aParent;
  FParent.AddGUIControl(inherited Create);
  FParent.FTextList.Add(Self);

  FTextRect:=FParent.AddGUIObject;
  FTextRect.FOwner:=Self;
  FTextRect.Visible:=false;

      // Text rect
  PQuad:=FTextRect.AddQuad;
  PQuad.TexCoord[0][0]:=0; PQuad.TexCoord[0][1]:=1;
  PQuad.TexCoord[1][0]:=0; PQuad.TexCoord[1][1]:=0;
  PQuad.TexCoord[2][0]:=1; PQuad.TexCoord[2][1]:=0;
  PQuad.TexCoord[3][0]:=1; PQuad.TexCoord[3][1]:=1;
  PQuad.TexCoord[0][2]:=0; PQuad.TexCoord[1][2]:=0;
  PQuad.TexCoord[2][2]:=0; PQuad.TexCoord[3][2]:=0;
  PQuad.Updated:=true;

end;

procedure TGLStaticText.CreateTextTexture(const aText: string);
var TextureBitmap: PByteArray;
    TW,TH,i,j: integer;
    BgColor,c: cardinal;
    p: PByteArray;
begin
  if aText='' then exit;
  
  TW:=FBitmap.Canvas.TextWidth(aText);
  TH:=FBitmap.Canvas.TextHeight(aText);
  if (TW<>FWidth) or (TH<>FHeight) or (not assigned(FTexture)) then begin
    if assigned(FTexture) then FTexture.Free;
    if TW*TH<>0 then begin
      FBitmap.Width:=TW; FBitmap.Height:=TH;
      FTexture:=TTexture.Create;
      FTexture.CreateRGBA8Texture2D(TW,TH);
      FTexture.BlendingMode:=tbmTransparency;
      FTexture.TextureMode:=tcReplace;
    end;
    if FAutosize then begin
      Width:=TW; Height:=TH;
      FFixedWidth:=TW; FFixedHeight:=TH;
    end else begin
      FWidth:=TW; FHeight:=TH;
      UpdateTextRect(FTextRect);
    end;

  end; if not assigned(FTexture) then exit;

  BgColor:=($00FFFFFF-(FFont.Color and $00FFFFFF)) and $00FFFFFF;
  FBitmap.PixelFormat:=pf24Bit;
  FBitmap.Canvas.Brush.Style:=bsClear;
  FBitmap.Canvas.Brush.Color:=bgColor;
  FBitmap.Canvas.Pen.Color:=bgColor;
  FBitmap.Canvas.Rectangle(0,0,TW,TH);
  FBitmap.Canvas.Pen.Color:=FFont.Color;
  FBitmap.Canvas.Brush.Color:=FFont.Color;
  FBitmap.Canvas.Brush.Style:=bsClear;
  FBitmap.Canvas.TextOut(0,0,aText);
  getmem(TextureBitmap,tw*th*4);
  for i:=0 to th-1 do begin
    p:=FBitmap.ScanLine[th-1-i];
    for j:=0 to tw-1 do begin
      c:=(p[j*3+0] shl 16) + (p[j*3+1] shl 8) + p[j*3+2];
      if c = bgColor then begin
        TextureBitmap[i*tw*4+j*4+3]:=0;
        TextureBitmap[i*tw*4+j*4+0]:=0;
        TextureBitmap[i*tw*4+j*4+1]:=0;
        TextureBitmap[i*tw*4+j*4+2]:=0;
      end else begin
        TextureBitmap[i*tw*4+j*4+3]:=255;
        TextureBitmap[i*tw*4+j*4+0]:=p[j*3+2];
        TextureBitmap[i*tw*4+j*4+1]:=p[j*3+1];
        TextureBitmap[i*tw*4+j*4+2]:=p[j*3+0];
      end;
    end;
  end;
  FTexture.UploadData(TextureBitmap);
  freemem(TextureBitmap);
end;

destructor TGLStaticText.Destroy;
begin
  FTextRect.Free; FFont.Free; FBitmap.Free;
  if assigned(FTexture) then FTexture.Free;
  inherited;
end;

procedure TGLStaticText.onFontChanged(Sender: TObject);
begin
  FBitmap.Canvas.Font.Assign(FFont);
  CreateTextTexture(FText);
end;

procedure TGLStaticText.setHeight(const Value: integer);
begin
  FHeight := Value; FFixedHeight:=Value;
  UpdateTextRect(FTextRect);
end;

procedure TGLStaticText.setLeft(const Value: integer);
begin
  FLeft := Value;
  UpdateTextRect(FTextRect);
end;

procedure TGLStaticText.setText(const Value: string);
begin
  FText := Value; CreateTextTexture(FText);
end;

procedure TGLStaticText.setTop(const Value: integer);
begin
  FTop := Value;
  UpdateTextRect(FTextRect);
end;

procedure TGLStaticText.setVisible(const Value: boolean);
begin
  inherited;
  UpdateTextRect(FTextRect);
end;

procedure TGLStaticText.setWidth(const Value: integer);
begin
  FWidth := Value; FFixedWidth:=Value;
  UpdateTextRect(FTextRect);
end;

procedure TGLStaticText.UpdateTextRect(aText: TGUIObject);
var PQuad: PQuadObject;
    i: integer;
begin
  if not Visible then begin
    PQuad:=aText.FQuads[0];
    for i:=0 to 3 do begin
      PQuad.Vertex[i][0]:=0; PQuad.Vertex[i][1]:=0;
    end; PQuad.Updated:=true;
    exit;
  end;

  PQuad:=aText.FQuads[0];
  PQuad.Vertex[0][0]:=FLeft; PQuad.Vertex[0][1]:=FTop;
  PQuad.Vertex[1][0]:=FLeft; PQuad.Vertex[1][1]:=FTop+FHeight;
  PQuad.Vertex[2][0]:=FLeft+FWidth; PQuad.Vertex[2][1]:=FTop+FHeight;
  PQuad.Vertex[3][0]:=FLeft+FWidth; PQuad.Vertex[3][1]:=FTop;
  PQuad.Vertex[0][2]:=0.1; PQuad.Vertex[1][2]:=0.1;
  PQuad.Vertex[2][2]:=0.1; PQuad.Vertex[3][2]:=0.1;
  PQuad.Color[0]:=Vectormake(1,1,1,1);
  PQuad.Color[1]:=Vectormake(1,1,1,1);
  PQuad.Color[2]:=Vectormake(1,1,1,1);
  PQuad.Color[3]:=Vectormake(1,1,1,1);
  PQuad.Updated:=true;
end;

{ TGLProgressBar }

constructor TGLProgressBar.Create(aParent: TGUILayer);
var PQuad: PQuadObject;
begin
  aParent.AddGUIControl(inherited Create);
  FLeft:=0; FTop:=0; FWidth:=96; FHeight:=16;
  FPosition:=0; FMinValue:=0; FMaxValue:=100;

  FParent:=aParent;
  FProgressBar:=FParent.AddGUIObject;
  FProgressBar.FOwner:=Self;
      // LeftStaticPart
  PQuad:=FProgressBar.AddQuad;
  PQuad.TexCoord[0][0]:=41/128; PQuad.TexCoord[0][1]:=95/128;
  PQuad.TexCoord[1][0]:=41/128; PQuad.TexCoord[1][1]:=70/128;
  PQuad.TexCoord[2][0]:=42/128; PQuad.TexCoord[2][1]:=70/128;
  PQuad.TexCoord[3][0]:=42/128; PQuad.TexCoord[3][1]:=95/128;
  PQuad.TexCoord[0][2]:=0; PQuad.TexCoord[1][2]:=0;
  PQuad.TexCoord[2][2]:=0; PQuad.TexCoord[3][2]:=0;
  PQuad.Updated:=true;

      // RightStaticPart
  PQuad:=FProgressBar.AddQuad;
  PQuad.TexCoord[0][0]:=54/128; PQuad.TexCoord[0][1]:=95/128;
  PQuad.TexCoord[1][0]:=54/128; PQuad.TexCoord[1][1]:=70/128;
  PQuad.TexCoord[2][0]:=56/128; PQuad.TexCoord[2][1]:=70/128;
  PQuad.TexCoord[3][0]:=56/128; PQuad.TexCoord[3][1]:=95/128;
  PQuad.TexCoord[0][2]:=0; PQuad.TexCoord[1][2]:=0;
  PQuad.TexCoord[2][2]:=0; PQuad.TexCoord[3][2]:=0;
  PQuad.Updated:=true;

      // MiddleFillPart
  PQuad:=FProgressBar.AddQuad;
  PQuad.TexCoord[0][0]:=46/128; PQuad.TexCoord[0][1]:=95/128;
  PQuad.TexCoord[1][0]:=46/128; PQuad.TexCoord[1][1]:=70/128;
  PQuad.TexCoord[2][0]:=50/128; PQuad.TexCoord[2][1]:=70/128;
  PQuad.TexCoord[3][0]:=50/128; PQuad.TexCoord[3][1]:=95/128;
  PQuad.TexCoord[0][2]:=0; PQuad.TexCoord[1][2]:=0;
  PQuad.TexCoord[2][2]:=0; PQuad.TexCoord[3][2]:=0;
  PQuad.Updated:=true;

      // MiddleEmptyPart
  PQuad:=FProgressBar.AddQuad;
  PQuad.TexCoord[0][0]:=62/128; PQuad.TexCoord[0][1]:=95/128;
  PQuad.TexCoord[1][0]:=62/128; PQuad.TexCoord[1][1]:=70/128;
  PQuad.TexCoord[2][0]:=66/128; PQuad.TexCoord[2][1]:=70/128;
  PQuad.TexCoord[3][0]:=66/128; PQuad.TexCoord[3][1]:=95/128;
  PQuad.TexCoord[0][2]:=0; PQuad.TexCoord[1][2]:=0;
  PQuad.TexCoord[2][2]:=0; PQuad.TexCoord[3][2]:=0;
  PQuad.Updated:=true;
end;

procedure TGLProgressBar.UpdateProgress;
var PQuad: PQuadObject;
    Pos: single;
begin
  if not Visible then exit;

  Pos:=(FPosition-FMinValue)/(FMaxValue-FMinValue)*(FWidth-3);

  PQuad:=FProgressBar.FQuads[2];
  PQuad.Vertex[0][0]:=FLeft+1; PQuad.Vertex[0][1]:=FTop;
  PQuad.Vertex[1][0]:=FLeft+1; PQuad.Vertex[1][1]:=FTop+FHeight;
  PQuad.Vertex[2][0]:=FLeft+1+Pos; PQuad.Vertex[2][1]:=FTop+FHeight;
  PQuad.Vertex[3][0]:=FLeft+1+Pos; PQuad.Vertex[3][1]:=FTop;
  PQuad.Vertex[0][2]:=0; PQuad.Vertex[1][2]:=0;
  PQuad.Vertex[2][2]:=0; PQuad.Vertex[3][2]:=0;
  PQuad.Color[0]:=Vectormake(1,1,1,1);
  PQuad.Color[1]:=Vectormake(1,1,1,1);
  PQuad.Color[2]:=Vectormake(0.1,0.1,0.1,1);
  PQuad.Color[3]:=Vectormake(0.1,0.1,0.1,1);
  PQuad.Updated:=true;

  PQuad:=FProgressBar.FQuads[3];
  PQuad.Vertex[0][0]:=FLeft+1+Pos; PQuad.Vertex[0][1]:=FTop;
  PQuad.Vertex[1][0]:=FLeft+1+Pos; PQuad.Vertex[1][1]:=FTop+FHeight;
  PQuad.Vertex[2][0]:=FLeft+FWidth-2; PQuad.Vertex[2][1]:=FTop+FHeight;
  PQuad.Vertex[3][0]:=FLeft+FWidth-2; PQuad.Vertex[3][1]:=FTop;
  PQuad.Vertex[0][2]:=0; PQuad.Vertex[1][2]:=0;
  PQuad.Vertex[2][2]:=0; PQuad.Vertex[3][2]:=0;
  PQuad.Color[0]:=Vectormake(1,1,1,1);
  PQuad.Color[1]:=Vectormake(1,1,1,1);
  PQuad.Color[2]:=Vectormake(1,1,1,1);
  PQuad.Color[3]:=Vectormake(1,1,1,1);
  PQuad.Updated:=true;

  FProgressBar.Visible:=true;
//  FVisible:=true;
end;

procedure TGLProgressBar.UpdateProgressBar(aBar: TGUIObject);
var PQuad: PQuadObject;
    i,j: integer;
begin
  if not assigned(aBar) then exit;
  if not Visible then begin
    for i:=0 to 3 do begin
      PQuad:=aBar.FQuads[i];
      for j:=0 to 3 do begin
        PQuad.Vertex[j][0]:=0; PQuad.Vertex[j][1]:=0;
      end; PQuad.Updated:=true;
    end; exit;
  end;

  PQuad:=aBar.FQuads[0];
  PQuad.Vertex[0][0]:=FLeft; PQuad.Vertex[0][1]:=FTop;
  PQuad.Vertex[1][0]:=FLeft; PQuad.Vertex[1][1]:=FTop+FHeight;
  PQuad.Vertex[2][0]:=FLeft+1; PQuad.Vertex[2][1]:=FTop+FHeight;
  PQuad.Vertex[3][0]:=FLeft+1; PQuad.Vertex[3][1]:=FTop;
  PQuad.Vertex[0][2]:=0; PQuad.Vertex[1][2]:=0;
  PQuad.Vertex[2][2]:=0; PQuad.Vertex[3][2]:=0;
  PQuad.Color[0]:=Vectormake(1,1,1,1);
  PQuad.Color[1]:=Vectormake(1,1,1,1);
  PQuad.Color[2]:=Vectormake(1,1,1,1);
  PQuad.Color[3]:=Vectormake(1,1,1,1);
  PQuad.Updated:=true;

  PQuad:=aBar.FQuads[1];
  PQuad.Vertex[0][0]:=FLeft+FWidth-2; PQuad.Vertex[0][1]:=FTop;
  PQuad.Vertex[1][0]:=FLeft+FWidth-2; PQuad.Vertex[1][1]:=FTop+FHeight;
  PQuad.Vertex[2][0]:=FLeft+FWidth; PQuad.Vertex[2][1]:=FTop+FHeight;
  PQuad.Vertex[3][0]:=FLeft+FWidth; PQuad.Vertex[3][1]:=FTop;
  PQuad.Vertex[0][2]:=0; PQuad.Vertex[1][2]:=0;
  PQuad.Vertex[2][2]:=0; PQuad.Vertex[3][2]:=0;
  PQuad.Color[0]:=Vectormake(1,1,1,1);
  PQuad.Color[1]:=Vectormake(1,1,1,1);
  PQuad.Color[2]:=Vectormake(1,1,1,1);
  PQuad.Color[3]:=Vectormake(1,1,1,1);
  PQuad.Updated:=true;

  UpdateProgress;
end;

destructor TGLProgressBar.Destroy;
begin
  FProgressBar.Free;
  inherited;
end;

procedure TGLProgressBar.setHeight(const Value: integer);
begin
  FHeight := Value; UpdateProgressBar(FProgressBar);
end;

procedure TGLProgressBar.setLeft(const Value: integer);
begin
  FLeft := Value; UpdateProgressBar(FProgressBar);
end;

procedure TGLProgressBar.setTop(const Value: integer);
begin
  FTop := Value; UpdateProgressBar(FProgressBar);
end;

procedure TGLProgressBar.setWidth(const Value: integer);
begin
  FWidth := Value; UpdateProgressBar(FProgressBar);
end;

procedure TGLProgressBar.setVisible(const Value: boolean);
begin
  inherited;
  FProgressBar.Visible:=Value;
  UpdateProgressBar(FProgressBar);
end;

procedure TGLProgressBar.setMaxValue(const Value: single);
begin
  FMaxValue := Value; UpdateProgress;
end;

procedure TGLProgressBar.setMinValue(const Value: single);
begin
  FMinValue := Value; UpdateProgress;
end;

procedure TGLProgressBar.setPosition(const Value: single);
begin
  FPosition := Value;
  if FPosition>FMaxValue then FPosition:=FMaxValue;
  if FPosition<FMinValue then FPosition:=FMinValue;
  UpdateProgress;
end;

{ TGLScrollBar }

constructor TGLScrollBar.Create(aParent: TGUILayer);
begin

end;

destructor TGLScrollBar.Destroy;
begin
  FScrollBar.Free;
  inherited;
end;

procedure TGLScrollBar.setHeight(const Value: integer);
begin
  FHeight:=Value; UpdateScrollBar(FScrollBar);
end;

procedure TGLScrollBar.setKind(const Value: TScrollBarOrientation);
begin
  FKind := Value; UpdateScrollBar(FScrollBar);
end;

procedure TGLScrollBar.setLeft(const Value: integer);
begin
  FLeft:=Value; UpdateScrollBar(FScrollBar);
end;

procedure TGLScrollBar.setMaxValue(const Value: single);
begin
  FMaxValue := Value; UpdatePosition;
end;

procedure TGLScrollBar.setMinValue(const Value: single);
begin
  FMinValue := Value; UpdatePosition;
end;

procedure TGLScrollBar.setPosition(const Value: single);
begin
  FPosition := Value; UpdatePosition;
end;

procedure TGLScrollBar.setTop(const Value: integer);
begin
  FTop:=Value; UpdateScrollBar(FScrollBar);
end;

procedure TGLScrollBar.setVisible(const Value: boolean);
begin
  inherited;
  FScrollBar.Visible:=Value;
end;

procedure TGLScrollBar.setWidth(const Value: integer);
begin
  FWidth:=Value; UpdateScrollBar(FScrollBar);
end;

procedure TGLScrollBar.UpdatePosition;
begin
//
end;

procedure TGLScrollBar.UpdateScrollBar(aListBox: TGUIObject);
begin

end;

end.


Procedure drawMouse;
begin
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_TEXTURE_2D);
  glcolor4f(1,1,1,1);
  glBegin(GL_QUADS);
    glTexCoord(40/128, 64/128); glVertex(Mouse.X,    Mouse.Y, 1);
    glTexCoord(72/128, 64/128); glVertex(Mouse.X+32, Mouse.Y, 1);
    glTexCoord(72/128, 32/128); glVertex(Mouse.X+32, Mouse.Y-32, 1);
    glTexCoord(40/128, 32/128); glVertex(Mouse.X,    Mouse.Y-32, 1);
  glEnd();
  glDisable(GL_TEXTURE_2D);
  glDisable(GL_BLEND);
end;


