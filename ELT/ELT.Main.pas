﻿{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{                                                       }
{            Enhanced Legerity Texts (ELT)              }
{                   增强轻量文段                        }
{                     ELT 1.21                          }
{                                                       }
{    Copyright(c) 2016-2018 Reniasty de El Magnifico    }
{                   天道玄虚 出品                       }
{                 All rights reserved                   }
{                   保留所有权利                        }
{                                                       }
{*******************************************************}

unit ELT.Main;

interface

uses
  System.SysUtils, System.IOUtils, System.Types, System.UITypes, System.Classes, System.Generics.Collections,
  System.Math, FMX.Types, FMX.Objects, FMX.Controls, FMX.Graphics, FMX.Platform, FMX.Clipboard, FMX.Ani,
  ELT.Extra;

{$R ELT_MAIN.DCR}

procedure Register;

type
  TSctUI = class;
  TCameraUI = class;
  TParaImage = class;
  TMouseCon = class;
  TUndoCon = class;
  TUIPassage = class;
  TUIManager = class;

  TSctUI = class
  private
    // 段落格式
    FFirstIndent:                      Single;        // 首行缩进
    FLeftOffset:                       Single;        // 左侧偏移
    FRightOffset:                      Single;        // 右侧偏移
    FSpaceBefore:                      Single;        // 段前间距
    FSpaceAfter:                       Single;        // 段后间距
    FLineSpace:                        Single;        // 行间距
    FAlignment:                        TParaAlign;    // 对齐方式
    // 文字格式
    FFont:                             Integer;       // 字体列表编号
    FSize:                             Single;        // 字体大小
    FOpacity:                          Single;        // 字体透明度
    FBgOpacity:                        Single;        // 背景透明度
    FOffset:                           Single;        // 字体偏移
    FColor:                            Cardinal;      // 字体颜色
    FBgColor:                          Cardinal;      // 背景颜色
    FStyle:                            TFontStyles;   // 字体属性
    FEffect:                           TCharEffect;   // 文字效果
    // 控制
    FPassage:                          TUIPassage;    // 对应文段
    function GetFont: string;
    function GetCharSct: TCharSct;
    function GetParaSct: TParaSct;
    function GetImageWidth: Single;
    function GetImageHeight: Single;
    procedure SetFirstIndent(const Value: Single);
    procedure SetLeftOffset(const Value: Single);
    procedure SetRightOffset(const Value: Single);
    procedure SetSpaceBefore(const Value: Single);
    procedure SetSpaceAfter(const Value: Single);
    procedure SetLineSpace(const Value: Single);
    procedure SetAlignment(const Value: TParaAlign);
    procedure SetFont(const Value: string);
    procedure SetSize(const Value: Single);
    procedure SetOpacity(const Value: Single);
    procedure SetBgOpacity(const Value: Single);
    procedure SetOffset(const Value: Single);
    procedure SetColor(const Value: Cardinal);
    procedure SetBgColor(const Value: Cardinal);
    procedure SetStyle(const Value: TFontStyles);
    procedure SetCharSct(const Value: TCharSct);
    procedure SetParaSct(const Value: TParaSct);
    procedure SetImageWidth(const Value: Single);
    procedure SetImageHeight(const Value: Single);
  public
    // 方法
    constructor Create(APassage: TUIPassage);
    // 段落属性
    property FirstIndent:         Single         read FFirstIndent             write SetFirstIndent;
    property LeftOffset:          Single         read FLeftOffset              write SetLeftOffset;
    property RightOffset:         Single         read FRightOffset             write SetRightOffset;
    property SpaceBefore:         Single         read FSpaceBefore             write SetSpaceBefore;
    property SpaceAfter:          Single         read FSpaceAfter              write SetSpaceAfter;
    property LineSpace:           Single         read FLineSpace               write SetLineSpace;
    property Alignment:           TParaAlign     read FAlignment               write SetAlignment;
    // 字体属性
    property Font:                string         read GetFont                  write SetFont;
    property Size:                Single         read FSize                    write SetSize;
    property Opacity:             Single         read FOpacity                 write SetOpacity;
    property BgOpacity:           Single         read FBgOpacity               write SetBgOpacity;
    property Offset:              Single         read FOffset                  write SetOffset;
    property Color:               Cardinal       read FColor                   write SetColor;
    property BgColor:             Cardinal       read FBgColor                 write SetBgColor;
    property Style:               TFontStyles    read FStyle                   write SetStyle;
    property Effect:              TCharEffect    read FEffect; // 区分是否为文字
    // 插图属性
    property ImageWidth:          Single         read GetImageWidth            write SetImageWidth;
    property ImageHeight:         Single         read GetImageHeight           write SetImageHeight;
    // 输出
    property CharSct:             TCharSct       read GetCharSct               write SetCharSct;
    property ParaSct:             TParaSct       read GetParaSct               write SetParaSct;
  end;

  TCameraUI = class  // 镜头功能
  private
    FX:                               Single;
    FY:                               Single;
    FManager:                         TUIManager;
    function GetPoint: TPointF;
    procedure SetCameraPoint(const Value: TPointF);
    procedure SetPosPoint(const Value: TPointF);
    procedure MoveFinish(Sender: TObject);
    procedure MoveFinishEnd(Sender: TObject);
  public
    constructor Create(X, Y: Single; AManager: TUIManager);
    procedure Overwrite(X, Y: Single);
    property CameraPoint:         TPointF        read GetPoint                 write SetCameraPoint;
    property PosPoint:            TPointF        read GetPoint                 write SetPosPoint;
    property PosX:                Single         read FX                       write FX;
    property PosY:                Single         read FY                       write FY;
  end;

  TParaImage = class // 有一定局限性的段落插图
  private
    FImage:                       TBitmap;
    // 绘制尺寸
    FWidth:                       Single;
    FHeight:                      Single;
    // 所属信息
    FPassage:                     TUIPassage;
    // 属性方法
    procedure SetWidth(const Value: Single);
    procedure SetHeight(const Value: Single);
  public
    property Width:               Single         read FWidth                   write SetWidth;
    property Height:              Single         read FHeight                  write SetHeight;
    // 方法
    constructor Create(AOwner: TUIPassage; ImageFile: string; Width: Single = 0); overload;
    constructor Create(AOwner: TUIPassage; Stream: TStream); overload;
    constructor Create(AOwner: TUIPassage; Bitmap: TBitmap); overload;
    constructor Create(ACopy: TParaImage); overload;
    destructor Destroy; override;
  end;

  TMouseCon = class(TShape)  // 鼠标拖动、缩放等功能的实现
  private
    FDnX:                         Single;
    FDnY:                         Single;
    FLink:                        TControl;
    FInfo:                        TMouseInfo;
    procedure DealingMouse;
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Initiate(Link: TControl; X, Y: Single; Info: TMouseInfo);
  end;

  TUndoCon = class
  private
    FCount:             Integer;
    FStreams:           TObjectList<TMemoryStream>;
    FCurIndex:          Integer;
    FPointIndex:        Integer;
    FValidPoints:       Integer;
  public
    constructor Create(Count: Integer);
    destructor Destroy; override;
    function RecordInfo: TMemoryStream;
    function Undo: TMemoryStream;
    function Redo: TMemoryStream;
    procedure Initiate(Count: Integer);
  end;

  TUIPassage = class(TShape)
  private
    // 事件属性
    FOnSelChange:                      TNotifyEvent;
    // 文段属性
    FPassHeight:                       Single;
    FSelPos:                           Integer; // 记录光标位置
    FSelX:                             Single;  // 记录光标的横坐标
    FSelY:                             Single;  // 记录光标的纵坐标
    FSelH:                             Single;  // 记录光标的高度
    FInsertPos:                        Integer; // 记录拖动文字的插入位置
    FMouseDn:                          Boolean; // 记录鼠标按下
    // 交互属性
    FSctUI:                            TSctUI;
    FSelStart:                         Integer;  // 最首位为0，最末位为字符串长度
    FSelLength:                        Integer;
    // 段落属性
    FParaMaps:                         TObjectList<TBitmap>;
    FParaHeight:                       TList<Single>; // 段落高度
    FParaEnd:                          TList<Single>; // 段落末尾
      // 以上两项通过绘图获得
    FFonts:                            TStringList;   // 字体列表
    FParaSct:                          TList<PParaSct>;
    FParas:                            TObjectList<TList<PSctStr>>;
    FParaImages:                       TObjectList<TObjectList<TParaImage>>;
    // 交互信息
    FLineTop:                          TObjectList<TList<Single>>;
    FLineLeft:                         TObjectList<TList<Single>>;
    FCharRight:                        TObjectList<TObjectList<TList<Single>>>;
      // 以上三项通过绘图获取，每一项都为相对段落的值
    FFlicker:                          Boolean;  // 光标是否正在闪烁
    FTimer:                            TTimer;   // 光标闪烁计时器
    FDrawCursor:                       Boolean;  // 是否绘制光标
    // 控制
    FIsPsgUpdating:                    Boolean;                 // 是否在更新
    FImageIndex:                       Integer;
    FSelImage:                         Boolean;                 // 是否选中图片
    FScrollPos:                        Single;                  // 滚动条位置
    FMouseCon:                         TMouseInfos;
    FManager:                          TUIManager;
    FShowSides:                        Boolean;                 // 是否显示边框
    FSidesColor:                       TAlphaColor;             // 边框颜色
    FSidesWidth:                       Single;                  // 边框宽度
    FReadOnly:                         Boolean;
    FUndoCon:                          TUndoCon;                // 撤销功能
    FCanUndo:                          Boolean;                 // 撤销控制
    FUndoWaiting:                      Boolean;                 // 撤销等待
    // 输入等待计时器
    FDelayCon:                         Integer;
    FBufferStr:                        string;
    FDelayTimer:                       TTimer;
    // 背景优化
    FBackground:                       TStream;                 // 缓存流
    function GetTextLength: Integer;
    function GetParaIndex(const StartPos: Integer = -1): Integer;
    function GetSelPos(const ParaIndex, LineIndex, CharIndex: Integer): Integer; // 由行列位置获取选取位置
    procedure DeletePara(const Index: Integer); // 注销包含的指针并删除段落
    procedure DeleteSctStr(const ParaIndex, StrIndex: Integer); // 注销相应指针并删除结构字符串
    procedure DeleteString(const StartPos, Count: Integer); // StartPos从零开始，从StartPos往后删
    procedure DrawSelection;
    procedure InsertPara(const Index: Integer); // 插入一个段落
    procedure GetCurrentPos(const StartPos: Integer; var ParaIndex, StrIndex, CharIndex: Integer); // 下标均从零开始
    procedure GetLinePos(const StartPos: Integer; var ParaIndex, LineIndex, CharIndex: Integer); // 获取行列位置
    procedure InsertString(const StartPos: Integer; AString: string; NonFormat: Boolean = False); // StartPos从零开始
    procedure SelRecDown; // 光标下移
    procedure SelRecLeft; // 光标左移
    procedure SelRecRight; // 光标上移
    procedure SelRecUp; // 光标上移
    procedure SelRecHome; // 光标移至行首
    procedure SelRecEnd; // 光标移至行末
    procedure SetSelRec; // 设置光标位置和高度
      //输入控制
    procedure InsertChar(AChar: Char);
    procedure InputBuffer;
      // 设置文字格式
    procedure ChangeAttrib(const Attrib: TCharAttrib; const Value: Cardinal); overload;
    procedure ChangeAttrib(const Attrib: TCharAttrib; const Value: Integer); overload;
    procedure ChangeAttrib(const Attrib: TCharAttrib; const Value: Single); overload;
    procedure ChangeAttrib(const Attrib: TCharAttrib; const Value: TFontStyles; const Add: Boolean); overload;
      // 属性方法
    function GetParaString(Index: Integer): string;
    function GetParaLength(Index: Integer): Integer;
    function GetSelText: string;
    procedure SetSelText(const Value: string);
    procedure SetSelStart(const Value: Integer);
    procedure SetSelLength(const Value: Integer);
    procedure SetScrollPos(const Value: Single);
    procedure SetMouseCon(const Value: TMouseInfos);
      // 附属事件
    procedure Flickering(Sender: TObject);
    procedure InputControl(Sender: TObject);
    function GetColor: TAlphaColor;
    procedure SetColor(const Value: TAlphaColor);
    procedure SetShowSides(const Value: Boolean);
    procedure SetSidesColor(const Value: TAlphaColor);
    procedure SetSidesWidth(const Value: Single);
  protected
    function Select(X, Y: Single): Integer; // 由坐标获取选取位置
    procedure DeleteChar; // 当前位置删除一个字符
    procedure DelCharAfter; // 删除后一个字符
    procedure DrawToMap(Index: Integer); // 将段落绘制到图像上，并获取必要信息
    procedure SetSelection(const P1, P2: Integer); // 由两处选取位置获得选取开始及长度
    // 覆盖属性方法
    procedure SetWidth(const Value: Single); override;
    procedure SetHeight(const Value: Single); override;
    // 覆盖事件
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DoMouseLeave; override;
    procedure KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    procedure Paint; override;
    // 自定义事件
    procedure SelChange; virtual;
  public
    // 属性
    property Actived:                       Boolean             read FFlicker;
    property CanUndo:                       Boolean             read FCanUndo;
    property IsPsgUpdating:                 Boolean             read FIsPsgUpdating;
    property Manager:                       TUIManager          read FManager            write FManager;
    property Paras[Index: Integer]:         string              read GetParaString;      default;
    property ParaLength[Index: Integer]:    Integer             read GetParaLength;
    property PassHeight:                    Single              read FPassHeight;
    property SelStruct:                     TSctUI              read FSctUI;
    property SelImage:                      Boolean             read FSelImage;
    property SelStart:                      Integer             read FSelStart           write SetSelStart;
    property SelLength:                     Integer             read FSelLength          write SetSelLength;
    property SelText:                       string              read GetSelText          write SetSelText;
    property ScrollPos:                     Single              read FScrollPos          write SetScrollPos;
    property TextLength:                    Integer             read GetTextLength;
    // 方法
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function EnableUndo(Count: Integer): Boolean;
    function DisableUndo: Boolean;
    procedure Add(S: string);
    procedure BeginUpdating;
    procedure EndUpdating;
    procedure Clear;
    procedure CopyText;
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure Undo;
    procedure Redo;
    procedure FindPosList(AStr: string; var Text: string; ListPos: TList<Integer>);
      // 在文档中查找一段字符，并把全文档以字符串的形式写入Text中，把所有找到的位置写入ListPos中。
    procedure InsertImage(FileName: string; Width: Single = 0); overload; // 在当前位置插入一个图片
    procedure InsertImage(Stream: TStream); overload; // 在当前位置插入流中的图片
    procedure InsertImage(Bitmap: TBitmap); overload;
    procedure InsertStr(S: string; NonFormat: Boolean = False); // 在当前位置插入字符串
    procedure DividePara; // 在当前位置分段
    procedure LoadFromFile(FileName: string);
    procedure LoadFromStream(Stream: TStream; Buffer: Boolean = False);
    procedure PasteFromClipboard;
    procedure Redraw;
    procedure ReplaceByList(OldLen: Integer; PosList: TList<Integer>; NewStr: string);
    procedure SaveToHTML(FileName: string);
    procedure SaveToFile(FileName: string);
    procedure SaveToRTF(FileName: string);
    procedure SaveToStream(Stream: TStream; Buffer: Boolean = False);
    procedure SelectAll;
    procedure SetBackground(AFileName: string);
    procedure SetSelInfo(ASelStart, ASelLength: Integer);
  published
    property BackgroundColor:     TAlphaColor         read GetColor       write SetColor;
    property MouseCon:            TMouseInfos         read FMouseCon      write SetMouseCon   default [Wheeling];
    property ShowSides:           Boolean             read FShowSides     write SetShowSides  default False;
    property ReadOnly:            Boolean             read FReadOnly      write FReadOnly     default False;
    property SidesColor:          TAlphaColor         read FSidesColor    write SetSidesColor;
    property SidesWidth:          Single              read FSidesWidth    write SetSidesWidth;
    property OnSelChange:         TNotifyEvent        read FOnSelChange   write FOnSelChange;
    // 继承属性
    property Align;
    property Anchors;
    property CanFocus default True;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crIBeam;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Fill;
    property Locked default False;
    property Height;
    property HitTest default True;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property Stroke;
    property Visible default True;
    property Width;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnClick;
    property OnDblClick;
    property OnKeyDown;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPainting;
    property OnPaint;
    property OnResize;
  end;

  TUIManager = class(TShape)
  private
    FOnCreatePass:                     TManagerEvent;           // 创建文本框事件
    FOnRemovePass:                     TNotifyEvent;            // 移除文本框事件
    FOnMovePassEnd:                    TManagerEvent;           // 移动文本框事件
    FOnDrawMiniMap:                    TNotifyEvent;            // 绘制小地图事件
    FOnApplyCamera:                    TNotifyEvent;            // 应用镜头完成事件
    FOnDragCamera:                     TNotifyEvent;            // 拖拽镜头完成事件
    FInfo:                             TManagerInfo;            // 当前操作信息
    FPassages:                         TObjectList<TUIPassage>; // 包含的文段
    FActPass:                          TUIPassage;              // 当前激活的文段
    FDnX:                              Single;                  // 鼠标按下横坐标
    FDnY:                              Single;                  // 鼠标按下纵坐标
    FCrX:                              Single;                  // 鼠标当前横坐标
    FCrY:                              Single;                  // 鼠标当前纵坐标
    FIdentified:                       Single;                  // 标识，0-不标识，负数-改变宽度，正数-改变宽度并标识边框
    FMouseDn:                          Boolean;                 // 鼠标是否按下
    FMouseLR:                          Boolean;                 // 左键-True，右键-False
    FMiniMap:                          TBitmap;                 // 小地图
    FCameraList:                       TObjectList<TCameraUI>;  // 镜头列表
    FCamNameList:                      TStringList;             // 镜头名称
    FIsChangingCamera:                 Boolean;                 // 是否正在切换镜头
    FShowSides:                        Boolean;                 // 是否显示边框
    FSidesColor:                       TAlphaColor;             // 边框颜色
    FSidesWidth:                       Single;                  // 边框宽度
    FCanUndo:                          Boolean;                 // 撤销控制
    FUndoCon:                          TUndoCon;                // 撤销功能
    FReadOnly:                         Boolean;                 // 只读选项
    FBackground:                       TStream;                 // 背景缓存
    FIntervalBase:                     Single;                  // 镜头基础切换时间
    FIntervalPlus:                     Single;                  // 距离加成切换时间
    function GetCurRect: TRectF;
    procedure CreatePassage;
    function GetCamName(Index: Integer): string;
    procedure SetCamName(Index: Integer; const Value: string);
    function GetColor: TAlphaColor;
    procedure SetColor(const Value: TAlphaColor);
    procedure SetSidesColor(const Value: TAlphaColor);
    procedure SetShowSides(const Value: Boolean);
    function GetCamNameList: string;
    procedure SetSidesWidth(const Value: Single);
    function GetCurCamera: Integer;
    procedure SetCurCamera(const Value: Integer);
    procedure SetIdentified(const Value: Single);
    procedure SetIntervalBase(const Value: Single);
    procedure SetIntervalPlus(const Value: Single);
  protected
    // 覆盖事件
    procedure DoMouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure Paint; override;
    // 自定义事件
    procedure DoCreatePassage(Pass: TUIPassage);
    procedure DoRemovePassage;
    procedure DoDrawMiniMap;
    procedure MovePassEnd(Pass: TUIPassage);
    procedure DoApplyCamera;
    procedure DoDragCamera;
  public
    property ActPassage:               TUIPassage               read FActPass;
    property MiniMap:                  TBitmap                  read FMiniMap;
    property Passages:                 TObjectList<TUIPassage>  read FPassages;
    property CamName[Index: Integer]:  string                   read GetCamName        write SetCamName;
    property CamNameList:              string                   read GetCamNameList;
    property CanUndo:                  Boolean                  read FCanUndo;
    property CurCamera:                Integer                  read GetCurCamera      write SetCurCamera; // 无效值为-1
    property Identified:               Single                   read FIdentified       write SetIdentified;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function EnableUndo(Count: Integer): Boolean;
    function DisableUndo: Boolean;
    function GetCameraNames: string;
    procedure Clear;
    procedure DrawMiniMap;
    procedure LoadFromFile(FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure ViewMiniMap(X, Y: Single); // 移动镜头中心至小地图目标位置
    procedure SaveToFile(FileName: string);
    procedure SaveToStream(Stream: TStream);
    procedure SetCamera; // 在镜头中心保存镜头
    procedure ApplyCamera(Index: Integer);
    procedure ClearCamera;
    procedure DeleteCamera(Index: Integer);
    procedure MoveCameraItem(IFrom, ITo: Integer);
    procedure MovePassage(Pass: TUIPassage; X, Y: Single);
    procedure OverwriteCam(Index: Integer);
    procedure RemovePassage(APassage: TUIPassage);
    procedure Redraw;
    procedure Undo;
    procedure Redo;
    procedure SetBackground(AFileName: string);
  published
    property BackgroundColor:          TAlphaColor         read GetColor            write SetColor;
    property OnCreatePass:             TManagerEvent       read FOnCreatePass       write FOnCreatePass;
    property OnRemovePass:             TNotifyEvent        read FOnRemovePass       write FOnRemovePass;
    property OnDrawMiniMap:            TNotifyEvent        read FOnDrawMiniMap      write FOnDrawMiniMap;
    property OnMovePassEnd:            TManagerEvent       read FOnMovePassEnd      write FOnMovePassEnd;
    property OnApplyCamera:            TNotifyEvent        read FOnApplyCamera      write FOnApplyCamera;
    property OnDragCamera:             TNotifyEvent        read FOnDragCamera       write FOnDragCamera;
    property ReadOnly:                 Boolean             read FReadOnly           write FReadOnly     default False;
    property ShowSides:                Boolean             read FShowSides          write SetShowSides  default False;
    property SidesColor:               TAlphaColor         read FSidesColor         write SetSidesColor;
    property SidesWidth:               Single              read FSidesWidth         write SetSidesWidth;
    property IntervalBase:             Single              read FIntervalBase       write SetIntervalBase;
    property IntervalPlus:             Single              read FIntervalPlus       write SetIntervalPlus;
    // 继承属性
    property Align;
    property Anchors;
    property CanFocus default True;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crArrow;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Fill;
    property Locked default False;
    property Height;
    property HitTest default True;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property Stroke;
    property Visible default True;
    property Width;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnClick;
    property OnDblClick;
    property OnKeyDown;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPainting;
    property OnPaint;
    property OnResize;
  end;

implementation

procedure Register;
begin
  RegisterComponents('ELTexts', [TUIPassage, TUIManager]);
end;

{ TParaImage }

constructor TParaImage.Create(AOwner: TUIPassage; ImageFile: string; Width: Single);
begin
  if AOwner = nil then
    Exit;
  inherited Create;
  FPassage := AOwner;
  FImage := TBitmap.Create;
  FImage.LoadFromFile(ImageFile);
  if Width <= 0 then
    Width := FImage.Width;
  if Width <= FPassage.Width then
    FWidth := Width
  else
    FWidth := FPassage.Width;
  if FWidth > FImage.Width then
    FWidth := FImage.Width;
  FHeight := FImage.Height / FImage.Width * FWidth;
end;

constructor TParaImage.Create(AOwner: TUIPassage; Stream: TStream);
begin
  if AOwner = nil then
    Exit;
  inherited Create;
  FPassage := AOwner;
  FImage := TBitmap.Create;
  FImage.LoadFromStream(Stream);
  Width := FImage.Width;
  if Width <= FPassage.Width then
    FWidth := Width
  else
    FWidth := FPassage.Width;
  FHeight := FImage.Height / FImage.Width * FWidth;
end;

constructor TParaImage.Create(ACopy: TParaImage);
begin
  inherited Create;
  FPassage := ACopy.FPassage;
  FImage := TBitmap.Create;
  FImage.Assign(ACopy.FImage);
  FWidth := ACopy.FWidth;
  FHeight := ACopy.FHeight;
end;

constructor TParaImage.Create(AOwner: TUIPassage; Bitmap: TBitmap);
begin
  if AOwner = nil then
    Exit;
  inherited Create;
  FPassage := AOwner;
  FImage := TBitmap.Create;
  FImage.Assign(Bitmap);
  Width := FImage.Width;
  if Width <= FPassage.Width then
    FWidth := Width
  else
    FWidth := FPassage.Width;
  FHeight := FImage.Height / FImage.Width * FWidth;
end;

destructor TParaImage.Destroy;
begin
  FImage.Free;
  inherited;
end;

procedure TParaImage.SetHeight(const Value: Single);
var
  temp: Single;
begin
  if Value < 0 then
    Exit;
  if Value = 0 then
    temp := FImage.Width / FImage.Height * FImage.Height
  else
    temp := FImage.Width / FImage.Height * Value;
  if temp <= FPassage.Width then
    FWidth := temp
  else
    FWidth := FPassage.Width;
  if FWidth > FImage.Width then
    FWidth := FImage.Width;
  FHeight := FImage.Height / FImage.Width * FWidth;
end;

procedure TParaImage.SetWidth(const Value: Single);
begin
  if Value < 0 then
    Exit;
  if Value = 0 then
  begin
    if FImage.Width > FPassage.Width then
      FWidth := FPassage.Width
    else
      FWidth := FImage.Width;
  end
  else if Value <= FPassage.Width then
    FWidth := Value
  else
    FWidth := FPassage.Width;
  if Value > FImage.Width then
    FWidth := FImage.Width;
  FHeight := FImage.Height / FImage.Width * FWidth;
end;

{ TSctUI }

constructor TSctUI.Create(APassage: TUIPassage);
begin
  if APassage = nil then
    Exit;
  inherited Create;
  // 段落默认值
  FFirstIndent := 0;
  FLeftOffset := 0;
  FRightOffset := 0;
  FSpaceBefore := 0;
  FSpaceAfter := 0;
  FLineSpace := 0;
  FAlignment := TParaAlign.Left;
  // 文字默认值
  FFont := 0;
  FSize := 15;
  FOpacity := 1;
  FBgOpacity := 1;
  FColor := $FF000000;
  FBgColor := $00FFFFFF;
  FStyle := [];
  FPassage := APassage;
end;

function TSctUI.GetCharSct: TCharSct;
begin
  with Result do
  begin
    Font := FFont;
    Size := FSize;
    Opacity := FOpacity;
    BgOpacity := FBgOPacity;
    Offset := FOffset;
    Color := FColor;
    BgColor := FBgColor;
    Style := FStyle;
    Effect := FEffect;
  end;
end;

function TSctUI.GetFont: string;
begin
  Exit(FPassage.FFonts[FFont]);
end;

function TSctUI.GetImageHeight: Single;
begin
  with FPassage do
    if FSelImage then
      Exit(FParaImages[GetParaIndex][FImageIndex].FHeight)
    else
      Exit(0);
end;

function TSctUI.GetImageWidth: Single;
begin
  with FPassage do
    if FSelImage then
      Exit(FParaImages[GetParaIndex][FImageIndex].FWidth)
    else
      Exit(0);
end;

function TSctUI.GetParaSct: TParaSct;
begin
  with Result do
  begin
    FirstIndent := FFirstIndent;
    LeftOffset := FLeftOffset;
    RightOffset := FRightOffset;
    SpaceBefore := FSpaceBefore;
    SpaceAfter := FSpaceAfter;
    LineSpace := FLineSpace;
    Alignment := FAlignment;
  end;
end;

procedure TSctUI.SetAlignment(const Value: TParaAlign);
var
  pi1, pi2, i: Integer;
begin
  FAlignment := Value;
  with FPassage do
  begin
    pi1 := GetParaIndex(FSelStart);
    pi2 := GetParaIndex(FSelStart + FSelLength);
    for i := pi1 to pi2 do
    begin
      FParaSct[i].Alignment := Value;
      DrawToMap(i);
    end;
    SetSelRec;
    Repaint;
  end;
end;

procedure TSctUI.SetBgColor(const Value: Cardinal);
begin
  FBgColor := Value;
  if FPassage.FSelLength > 0 then
    FPassage.ChangeAttrib(TCharAttrib.BgColor, Value);
end;

procedure TSctUI.SetBgOpacity(const Value: Single);
begin
  if (Value >= 0) and (Value <= 1) then
    FBgOpacity := Value
  else
    Exit;
  if FPassage.FSelLength > 0 then
    FPassage.ChangeAttrib(TCharAttrib.BgOpacity, Value);
end;

procedure TSctUI.SetCharSct(const Value: TCharSct);
begin
  with Value do
  begin
    FFont := Font;
    FSize := Size;
    FOpacity := Opacity;
    FBgOpacity := BgOpacity;
    FOffset := Offset;
    FColor := Color;
    FBgColor := BgColor;
    FStyle := Style;
  end;
end;

procedure TSctUI.SetColor(const Value: Cardinal);
begin
  FColor := Value;
  if FPassage.FSelLength > 0 then
    FPassage.ChangeAttrib(TCharAttrib.Color, Value);
end;

procedure TSctUI.SetFont(const Value: string);
var
  i: Integer;
  hd: Boolean;
begin
  hd := False;
  for i := 0 to FPassage.FFonts.Count - 1 do
    if FPassage.FFonts[i] = Value then
    begin
      FFont := i;
      hd := True;
      Break;
    end;
  if not hd then
  begin
    FPassage.FFonts.Add(Value);
    FFont :=FPassage.FFonts.Count - 1;
  end;
  if FPassage.FSelLength > 0 then
    FPassage.ChangeAttrib(TCharAttrib.Font, FFont);
end;

procedure TSctUI.SetImageHeight(const Value: Single);
var
  i: Integer;
begin
  with FPassage do
    if FSelImage then
    begin
      i := GetParaIndex;
      FParaImages[i][FImageIndex].Height := Value;
      DrawToMap(i);
      SetSelRec;
      Repaint;
    end;
end;

procedure TSctUI.SetImageWidth(const Value: Single);
var
  i: Integer;
begin
  with FPassage do
    if FSelImage then
    begin
      i := GetParaIndex;
      FParaImages[i][FImageIndex].Width := Value;
      DrawToMap(i);
      SetSelRec;
      Repaint;
    end;
end;

procedure TSctUI.SetLeftOffset(const Value: Single);
var
  pi1, pi2, i: Integer;
begin
  if (Value >= 0) and (Value <= 20) then
    FLeftOffset := Value
  else
    Exit;
  with FPassage do
  begin
    pi1 := GetParaIndex(FSelStart);
    pi2 := GetParaIndex(FSelStart + FSelLength);
    for i := pi1 to pi2 do
    begin
      FParaSct[i].LeftOffset := Value;
      DrawToMap(i);
    end;
    SetSelRec;
    Repaint;
  end;
end;

procedure TSctUI.SetLineSpace(const Value: Single);
var
  pi1, pi2, i: Integer;
begin
  if (Value >= 0) and (Value <= 20) then
    FLineSpace := Value
  else
    Exit;
  with FPassage do
  begin
    pi1 := GetParaIndex(FSelStart);
    pi2 := GetParaIndex(FSelStart + FSelLength);
    for i := pi1 to pi2 do
    begin
      FParaSct[i].LineSpace := Value;
      DrawToMap(i);
    end;
    SetSelRec;
    Repaint;
  end;
end;

procedure TSctUI.SetFirstIndent(const Value: Single);
var
  pi1, pi2, i: Integer;
begin
  if (Value >= 0) and (Value <= 20) then
    FFirstIndent := Value
  else
    Exit;
  with FPassage do
  begin
    pi1 := GetParaIndex(FSelStart);
    pi2 := GetParaIndex(FSelStart + FSelLength);
    for i := pi1 to pi2 do
    begin
      FParaSct[i].FirstIndent := Value;
      DrawToMap(i);
    end;
    SetSelRec;
    Repaint;
  end;
end;

procedure TSctUI.SetOffset(const Value: Single);
begin
  if (Value >= -1000) and (Value <= 1000) then
    FOffset := Value
  else
    Exit;
  if FPassage.FSelLength > 0 then
    FPassage.ChangeAttrib(TCharAttrib.Offset, Value);
end;

procedure TSctUI.SetOpacity(const Value: Single);
begin
  if (Value >= 0) and (Value <= 1) then
    FOpacity := Value
  else
    Exit;
  if FPassage.FSelLength > 0 then
    FPassage.ChangeAttrib(TCharAttrib.Opacity, Value);
end;

procedure TSctUI.SetParaSct(const Value: TParaSct);
begin
  with Value do
  begin
    FFirstIndent := FirstIndent;
    FLeftOffset := LeftOffset;
    FRightOffset := RightOffset;
    FSpaceBefore := SpaceBefore;
    FSpaceAfter := SpaceAfter;
    FLineSpace := LineSpace;
    FAlignment := Alignment;
  end;
end;

procedure TSctUI.SetRightOffset(const Value: Single);
var
  pi1, pi2, i: Integer;
begin
  if (Value >= 0) and (Value <= 20) then
    FSpaceAfter := Value
  else
    Exit;
  with FPassage do
  begin
    pi1 := GetParaIndex(FSelStart);
    pi2 := GetParaIndex(FSelStart + FSelLength);
    for i := pi1 to pi2 do
    begin
      FParaSct[i].RightOffset := Value;
      DrawToMap(i);
    end;
    SetSelRec;
    Repaint;
  end;
end;

procedure TSctUI.SetSize(const Value: Single);
begin
  if (Value > 0) and (Value <= 100) then
    FSize := Value
  else
    Exit;
  if FPassage.FSelLength > 0 then
    FPassage.ChangeAttrib(TCharAttrib.Size, Value);
end;

procedure TSctUI.SetSpaceAfter(const Value: Single);
var
  pi1, pi2, i: Integer;
begin
  if (Value >= 0) and (Value <= 20) then
    FSpaceAfter := Value
  else
    Exit;
  with FPassage do
  begin
    pi1 := GetParaIndex(FSelStart);
    pi2 := GetParaIndex(FSelStart + FSelLength);
    for i := pi1 to pi2 do
    begin
      FParaSct[i].SpaceAfter := Value;
      DrawToMap(i);
    end;
    SetSelRec;
    Repaint;
  end;
end;

procedure TSctUI.SetSpaceBefore(const Value: Single);
var
  pi1, pi2, i: Integer;
begin
  if (Value >= 0) and (Value <= 20) then
    FSpaceBefore := Value
  else
    Exit;
  with FPassage do
  begin
    pi1 := GetParaIndex(FSelStart);
    pi2 := GetParaIndex(FSelStart + FSelLength);
    for i := pi1 to pi2 do
    begin
      FParaSct[i].SpaceBefore := Value;
      DrawToMap(i);
    end;
    SetSelRec;
    Repaint;
  end;
end;

procedure TSctUI.SetStyle(const Value: TFontStyles);
var
  add, sub: TFontStyles;
begin
  if Value = FStyle then
    Exit;
  add := Value - FStyle;
  sub := FStyle - Value;
  if FPassage.FSelLength > 0 then
    begin
      FPassage.ChangeAttrib(TCharAttrib.Style, add, True);
      FPassage.ChangeAttrib(TCharAttrib.Style, sub, False);
    end;
  FStyle := Value;
end;

{ TUIPassage }

procedure TUIPassage.Add(S: string);
var
  m: Integer;
begin
  m := FParas.Count - 1 + GetTextLength;
  FSelLength := 0;
  FSelStart := m;
  if FParas.Last.Count > 0 then
    DividePara;
  InsertStr(S);
end;

procedure TUIPassage.ChangeAttrib(const Attrib: TCharAttrib; const Value: Single);
var
  pi1, pi2, si1, si2, ci1, ci2, i, tp, ts: Integer;
begin
  GetCurrentPos(FSelStart, pi1, si1, ci1);
  GetCurrentPos(FSelStart + FSelLength, pi2, si2, ci2);
  tp := pi1;
  if Larger2(pi2, si2, pi1, si1) then
  begin
    ts := si1 + 1;
    while (ts >= FParas[tp].Count) and (tp < FParas.Count) do
    begin
      ts := 0;
      Inc(tp);
    end;
    while Smaller2(tp, ts, pi2, si2) do
    begin
      case Attrib of
        TCharAttrib.Size: FParas[tp][ts].Sct.Size := Value;
        TCharAttrib.Opacity: FParas[tp][ts].Sct.Opacity := Value;
        TCharAttrib.BgOpacity: FParas[tp][ts].Sct.BgOpacity := Value;
        TCharAttrib.Offset: FParas[tp][ts].Sct.Offset := Value;
      end;
      Inc(ts);
      while (ts >= FParas[tp].Count) and (tp < FParas.Count) do
      begin
        ts := 0;
        Inc(tp);
      end;
    end;
  end;
  if ci2 < FParas[pi2][si2].Str.Length then
  begin
    FParas[pi2].Insert(si2 + 1, SctStrP(FParas[pi2][si2].Sct^, FParas[pi2][si2].Str.Substring(ci2)));
    FParas[pi2][si2].Str := FParas[pi2][si2].Str.Remove(ci2);
  end;
  if not Equal2(pi1, si1, pi2, si2) then
  case Attrib of
    TCharAttrib.Size: FParas[pi2][si2].Sct.Size := Value;
    TCharAttrib.Opacity: FParas[pi2][si2].Sct.Opacity := Value;
    TCharAttrib.BgOpacity: FParas[pi2][si2].Sct.BgOpacity := Value;
    TCharAttrib.Offset: FParas[pi2][si2].Sct.Offset := Value;
  end;
  if ci1 > 0 then
  begin
    FParas[pi1].Insert(si1 + 1, SctStrP(FParas[pi1][si1].Sct^, FParas[pi1][si1].Str.Substring(ci1)));
    FParas[pi1][si1].Str := FParas[pi1][si1].Str.Remove(ci1);
    case Attrib of
      TCharAttrib.Size: FParas[pi1][si1 + 1].Sct.Size := Value;
      TCharAttrib.Opacity: FParas[pi1][si1 + 1].Sct.Opacity := Value;
      TCharAttrib.BgOpacity: FParas[pi1][si1 + 1].Sct.BgOpacity := Value;
      TCharAttrib.Offset: FParas[pi1][si1 + 1].Sct.Offset := Value;
    end;
  end
  else
  case Attrib of
    TCharAttrib.Size: FParas[pi1][si1].Sct.Size := Value;
    TCharAttrib.Opacity: FParas[pi1][si1].Sct.Opacity := Value;
    TCharAttrib.BgOpacity: FParas[pi1][si1].Sct.BgOpacity := Value;
    TCharAttrib.Offset: FParas[pi1][si1].Sct.Offset := Value;
  end;
  for i := pi1 to pi2 do
    DrawToMap(i);
  Repaint;
end;

procedure TUIPassage.ChangeAttrib(const Attrib: TCharAttrib; const Value: Integer);
var
  pi1, pi2, si1, si2, ci1, ci2, i, tp, ts: Integer;
begin
  GetCurrentPos(FSelStart, pi1, si1, ci1);
  GetCurrentPos(FSelStart + FSelLength, pi2, si2, ci2);
  tp := pi1;
  if Larger2(pi2, si2, pi1, si1) then
  begin
    ts := si1 + 1;
    while (ts >= FParas[tp].Count) and (tp < FParas.Count) do
    begin
      ts := 0;
      Inc(tp);
    end;
    while Smaller2(tp, ts, pi2, si2) do
    begin
      case Attrib of
        TCharAttrib.Font: FParas[tp][ts].Sct.Font := Value;
      end;
      Inc(ts);
      while (ts >= FParas[tp].Count) and (tp < FParas.Count) do
      begin
        ts := 0;
        Inc(tp);
      end;
    end;
  end;
  if ci2 < FParas[pi2][si2].Str.Length then
  begin
    FParas[pi2].Insert(si2 + 1, SctStrP(FParas[pi2][si2].Sct^, FParas[pi2][si2].Str.Substring(ci2)));
    FParas[pi2][si2].Str := FParas[pi2][si2].Str.Remove(ci2);
  end;
  if not Equal2(pi1, si1, pi2, si2) then
  case Attrib of
    TCharAttrib.Font: FParas[pi2][si2].Sct.Font := Value;
  end;
  if ci1 > 0 then
  begin
    FParas[pi1].Insert(si1 + 1, SctStrP(FParas[pi1][si1].Sct^, FParas[pi1][si1].Str.Substring(ci1)));
    FParas[pi1][si1].Str := FParas[pi1][si1].Str.Remove(ci1);
    case Attrib of
      TCharAttrib.Font: FParas[pi1][si1 + 1].Sct.Font := Value;
    end;
  end
  else
  case Attrib of
    TCharAttrib.Font: FParas[pi1][si1].Sct.Font := Value;
  end;
  for i := pi1 to pi2 do
    DrawToMap(i);
  Repaint;
end;

procedure TUIPassage.ChangeAttrib(const Attrib: TCharAttrib; const Value: Cardinal);
var
  pi1, pi2, si1, si2, ci1, ci2, i, tp, ts: Integer;
begin
  GetCurrentPos(FSelStart, pi1, si1, ci1);
  GetCurrentPos(FSelStart + FSelLength, pi2, si2, ci2);
  tp := pi1;
  if Larger2(pi2, si2, pi1, si1) then
  begin
    ts := si1 + 1;
    while (ts >= FParas[tp].Count) and (tp < FParas.Count) do
    begin
      ts := 0;
      Inc(tp);
    end;
    while Smaller2(tp, ts, pi2, si2) do
    begin
      case Attrib of
        TCharAttrib.Color: FParas[tp][ts].Sct.Color := Value;
        TCharAttrib.BgColor: FParas[tp][ts].Sct.BgColor := Value;
      end;
      Inc(ts);
      while (ts >= FParas[tp].Count) and (tp < FParas.Count) do
      begin
        ts := 0;
        Inc(tp);
      end;
    end;
  end;
  if ci2 < FParas[pi2][si2].Str.Length then
  begin
    FParas[pi2].Insert(si2 + 1, SctStrP(FParas[pi2][si2].Sct^, FParas[pi2][si2].Str.Substring(ci2)));
    FParas[pi2][si2].Str := FParas[pi2][si2].Str.Remove(ci2);
  end;
  if not Equal2(pi1, si1, pi2, si2) then
  case Attrib of
    TCharAttrib.Color: FParas[pi2][si2].Sct.Color := Value;
    TCharAttrib.BgColor: FParas[pi2][si2].Sct.BgColor := Value;
  end;
  if ci1 > 0 then
  begin
    FParas[pi1].Insert(si1 + 1, SctStrP(FParas[pi1][si1].Sct^, FParas[pi1][si1].Str.Substring(ci1)));
    FParas[pi1][si1].Str := FParas[pi1][si1].Str.Remove(ci1);
    case Attrib of
      TCharAttrib.Color: FParas[pi1][si1 + 1].Sct.Color := Value;
      TCharAttrib.BgColor: FParas[pi1][si1 + 1].Sct.BgColor := Value;
    end;
  end
  else
  case Attrib of
    TCharAttrib.Color: FParas[pi1][si1].Sct.Color := Value;
    TCharAttrib.BgColor: FParas[pi1][si1].Sct.BgColor := Value;
  end;
  for i := pi1 to pi2 do
    DrawToMap(i);
  Repaint;
end;

procedure TUIPassage.BeginUpdating;
begin
  FIsPsgUpdating := True;
end;

procedure TUIPassage.ChangeAttrib(const Attrib: TCharAttrib; const Value: TFontStyles; const Add: Boolean);
var
  pi1, pi2, si1, si2, ci1, ci2, i, tp, ts: Integer;
begin
  GetCurrentPos(FSelStart, pi1, si1, ci1);
  GetCurrentPos(FSelStart + FSelLength, pi2, si2, ci2);
  tp := pi1;
  if Larger2(pi2, si2, pi1, si1) then
  begin
    ts := si1 + 1;
    while (ts >= FParas[tp].Count) and (tp < FParas.Count) do
    begin
      ts := 0;
      Inc(tp);
    end;
    while Smaller2(tp, ts, pi2, si2) do
    begin
      case Attrib of
        TCharAttrib.Style:
          if Add then
            FParas[tp][ts].Sct.Style := FParas[tp][ts].Sct.Style + Value
          else
            FParas[tp][ts].Sct.Style := FParas[tp][ts].Sct.Style - Value;
      end;
      Inc(ts);
      while (ts >= FParas[tp].Count) and (tp < FParas.Count) do
      begin
        ts := 0;
        Inc(tp);
      end;
    end;
  end;
  if ci2 < FParas[pi2][si2].Str.Length then
  begin
    FParas[pi2].Insert(si2 + 1, SctStrP(FParas[pi2][si2].Sct^, FParas[pi2][si2].Str.Substring(ci2)));
    FParas[pi2][si2].Str := FParas[pi2][si2].Str.Remove(ci2);
  end;
  if not Equal2(pi1, si1, pi2, si2) then
  case Attrib of
    TCharAttrib.Style:
      if Add then
        FParas[pi2][si2].Sct.Style := FParas[pi2][si2].Sct.Style + Value
      else
        FParas[pi2][si2].Sct.Style := FParas[pi2][si2].Sct.Style - Value;
  end;
  if ci1 > 0 then
  begin
    FParas[pi1].Insert(si1 + 1, SctStrP(FParas[pi1][si1].Sct^, FParas[pi1][si1].Str.Substring(ci1)));
    FParas[pi1][si1].Str := FParas[pi1][si1].Str.Remove(ci1);
    case Attrib of
      TCharAttrib.Style:
        if Add then
          FParas[pi1][si1 + 1].Sct.Style := FParas[pi1][si1 + 1].Sct.Style + Value
        else
          FParas[pi1][si1 + 1].Sct.Style := FParas[pi1][si1 + 1].Sct.Style - Value;
    end;
  end
  else
  case Attrib of
    TCharAttrib.Style:
      if Add then
        FParas[pi1][si1].Sct.Style := FParas[pi1][si1].Sct.Style + Value
      else
        FParas[pi1][si1].Sct.Style := FParas[pi1][si1].Sct.Style - Value;
  end;
  for i := pi1 to pi2 do
    DrawToMap(i);
  Repaint;
end;

procedure TUIPassage.Clear;
var
  i, j: Integer;
begin
  for i := 0 to FParas.Count - 1 do
  begin
    Dispose(FParaSct[i]);
    for j := 0 to FParas[i].Count - 1 do
      DeleteSctStr(i, j);
  end;
  FSelStart := 0;
  FSelLength := 0;
  FParas.Clear;
  FParaImages.Clear;
  FParaMaps.Clear;
  FParaHeight.Clear;
  FParaEnd.Clear;
  FParaSct.Clear;
  FLineTop.Clear;
  FLineLeft.Clear;
  FCharRight.Clear;
  InsertPara(0);
end;

procedure TUIPassage.CopyText;
var
  pi1, pi2, si1, si2, ci1, ci2, i, j: Integer;
  ClipService: IFMXExtendedClipboardService;
  st: TStringList;
  s: string;
begin
  if FSelLength = 0 then
    Exit;
  GetCurrentPos(FSelStart, pi1, si1, ci1);
  GetCurrentPos(FSelStart + FSelLength, pi2, si2, ci2);
  st := TStringList.Create;
  try
    if (pi1 = pi2) and (si1 = si2) then
      st.Add(FParas[pi1][si1].Str.Substring(ci1, ci2 - ci1))
    else if pi1 = pi2 then
    begin
      s := FParas[pi1][si1].Str.Substring(ci1);
      for i := si1 + 1 to si2 - 1 do
        s := s + FParas[pi1][i].Str;
      s := s + FParas[pi1][si2].Str;
    end
    else
    begin
      if FParas[pi1].Count > 0 then
      begin
        s := FParas[pi1][si1].Str.Substring(ci1);
        for i := si1 + 1 to FParas[pi1].Count - 1 do
          s := s + FParas[pi1][1].Str;
        st.Add(s);
      end;
      for i := pi1 + 1 to pi2 - 1 do
      begin
        s := '';
        for j := 0 to FParas[i].Count - 1 do
          s := s + FParas[i][j].Str;
        st.Add(s);
      end;
      if FParas[pi2].Count > 0 then
      begin
        s := '';
        for i := 0 to si2 - 1 do
          s := s + FParas[pi2][i].Str;
        s := s + FParas[pi2][si2].Str.Substring(0, ci2);
        st.Add(s);
      end;
    end;
    if TPlatformServices.Current.SupportsPlatformService(IFMXExtendedClipboardService, ClipService) then
      ClipService.SetText(st.Text);
  finally
    FreeAndNil(st);
  end;
end;

procedure TUIPassage.CopyToClipboard;
var
  pi1, pi2, si1, si2, ci1, ci2, i, j, tc, ti, c, d, e: Integer;
  save, temp: TMemoryStream;
  ClipService: IFMXExtendedClipboardService;
  sp: TObjectList<TList<PSctStr>>;
  si: TObjectList<TObjectList<TParaImage>>;
  bs: TBytes;
begin
  if FSelLength = 0 then
    Exit;
  sp := TObjectList<TList<PSctStr>>.Create;
  si := TObjectList<TObjectList<TParaImage>>.Create;
  save := TMemoryStream.Create;
  try
    // 获取选取部分
    GetCurrentPos(FSelStart, pi1, si1, ci1);
    GetCurrentPos(FSelStart + FSelLength, pi2, si2, ci2);
    // 获取首段选取之前的图片数量
    tc := 0;
    for i := 0 to si1 - 1 do
    begin
      if FParas[pi1][i].Sct.Effect = TCharEffect.Image then
        Inc(tc, FParas[pi1][i].Str.Length);
    end;
    if (si1 < FParas[pi1].Count) and (FParas[pi1][si1].Sct.Effect = TCharEffect.Image) then
      Inc(tc, ci1);
    if (pi1 = pi2) and (si1 = si2) then
    begin
      si.Add(TObjectList<TParaImage>.Create);
      if FParas[pi1][si1].Sct.Effect = TCharEffect.Image then
      begin
        for i := 0 to FSelLength - 1 do
          si[0].Add(FParaImages[pi1][tc + i]);
      end;
      sp.Add(TList<PSctStr>.Create);
      sp[0].Add(SctStrP(FParas[pi1][si1].Sct^, FParas[pi1][si1].Str.SubString(ci1, FSelLength)));
    end
    else if (pi1 = pi2) then
    begin
      // 获取首段选取位置末之前的图片数量
      ti := 0;
      for i := 0 to si2 - 1 do
      begin
        if FParas[pi1][i].Sct.Effect = TCharEffect.Image then
          Inc(ti, FParas[pi1][i].Str.Length);
      end;
      if (si2 < FParas[pi1].Count) and (FParas[pi1][si2].Sct.Effect = TCharEffect.Image) then
        Inc(ti, ci2);
      si.Add(TObjectList<TParaImage>.Create);
      if ti > tc then
      begin
        for i := tc to ti - 1 do
          si[0].Add(FParaImages[pi1][i]);
      end;
      sp.Add(TList<PSctStr>.Create);
      if ci1 < FParas[pi1][si1].Str.Length then
        sp[0].Add(SctStrP(FParas[pi1][si1].Sct^, FParas[pi1][si1].Str.Substring(ci1)));
      for i := si1 + 1 to si2 - 1 do
        sp[0].Add(SctStrP(FParas[pi1][i].Sct^, FParas[pi1][i].Str));
      sp[0].Add(SctStrP(FParas[pi1][si2].Sct^, FParas[pi1][si2].Str.Substring(0, ci2)));
    end
    else
    begin
      // 复制首段图片
      si.Add(TObjectList<TParaImage>.Create);
      if tc < FParaImages[pi1].Count then
      begin
        for i := tc to FParaImages[pi1].Count - 1 do
          si[0].Add(FParaImages[pi1][i]);
      end;
      // 获取末端选取之前的图片数量
      tc := 0;
      for i := 0 to si2 - 1 do
      begin
        if FParas[pi2][i].Sct.Effect = TCharEffect.Image then
          Inc(tc, FParas[pi2][i].Str.Length);
      end;
      if (si2 < FParas[pi2].Count) and (FParas[pi2][si2].Sct.Effect = TCharEffect.Image) then
        Inc(tc, ci2);
      // 复制中间图片
      for i := pi1 + 1 to pi2 - 1 do
      begin
        si.Add(TObjectList<TParaImage>.Create);
        for j := 0 to FParaImages[i].Count - 1 do
          si[i - pi1].Add(FParaImages[i][j]);
      end;
      // 复制末段图片
      si.Add(TObjectList<TParaImage>.Create);
      if tc > 0 then
      begin
        for i := 0 to tc - 1 do
          si[pi2 - pi1].Add(FParaImages[pi2][i]);
      end;
      // 复制首段文字
      sp.Add(TList<PSctStr>.Create);
      sp[0].Add(SctStrP(FParas[pi1][si1].Sct^, FParas[pi1][si1].Str.Substring(ci1)));
      for i := si1 + 1 to FParas[pi1].Count - 1 do
        sp[0].Add(SctStrP(FParas[pi1][i].Sct^, FParas[pi1][i].Str));
      // 复制中间文字
      for i := pi1 + 1 to pi2 - 1 do
      begin
        sp.Add(TList<PSctStr>.Create);
        for j := 0 to FParas[i].Count - 1 do
          sp[i - pi1].Add(SctStrP(FParas[i][j].Sct^, FParas[i][j].Str));
      end;
      // 复制末段文字
      sp.Add(TList<PSctStr>.Create);
      for i := 0 to si2 - 1 do
        sp[pi2 - pi1].Add(SctStrP(FParas[pi2][i].Sct^, FParas[pi2][i].Str));
      if ci2 > 0 then
        sp[pi2 - pi1].Add(SctStrP(FParas[pi2][si2].Sct^, FParas[pi2][si2].Str.Substring(0, ci2)));
    end;
    // 储存文字信息
    c := sp.Count;
    save.Write(c, SizeOf(Integer));
    for i := 0 to c - 1 do
    begin
      d := sp[i].Count;
      save.Write(d, SizeOf(Integer));
      for j := 0 to d - 1 do
      begin
        // 文字结构
        save.Write(sp[i][j].Sct.Font, SizeOf(Integer));
        save.Write(sp[i][j].Sct.Size, SizeOf(Single));
        save.Write(sp[i][j].Sct.Opacity, SizeOf(Single));
        save.Write(sp[i][j].Sct.BgOpacity, SizeOf(Single));
        save.Write(sp[i][j].Sct.Offset, SizeOf(Single));
        save.Write(sp[i][j].Sct.Color, SizeOf(Cardinal));
        save.Write(sp[i][j].Sct.BgColor, SizeOf(Cardinal));
        e := 0;
        if TFontStyle.fsBold in sp[i][j].Sct.Style then
          Inc(e);
        if TFontStyle.fsItalic in sp[i][j].Sct.Style then
          Inc(e, 2);
        if TFontStyle.fsUnderline in sp[i][j].Sct.Style then
          Inc(e, 4);
        if TFontStyle.fsStrikeOut in sp[i][j].Sct.Style then
          Inc(e, 8);
        save.Write(e, SizeOf(Integer));
        save.Write(sp[i][j].Sct.Effect, SizeOf(TCharEffect));
        // 文字
        bs := WideBytesOf(sp[i][j].Str);
        e := Length(bs);
        save.Write(e, SizeOf(Integer));
        save.WriteBuffer(bs, e);
      end;
    end;
    temp := TMemoryStream.Create;
    try
      // 储存图像信息
      c := si.Count;
      save.Write(c, SizeOf(Integer));
      for i := 0 to c - 1 do
      begin
        d := si[i].Count;
        save.Write(d, SizeOf(Integer));
        for j := 0 to d - 1 do
        begin
          si[i][j].FImage.SaveToStream(temp);
          e := temp.Size;
          temp.Position := 0;
          save.Write(e, SizeOf(Integer));
          save.WriteBuffer(temp.Memory^, e);
          temp.Clear;
          save.Write(si[i][j].FWidth, SizeOf(Single));
          save.Write(si[i][j].FHeight, SizeOf(Single));
        end;
      end;
      // 储存字体列表
      FFonts.SaveToStream(temp);
      e := temp.Size;
      temp.Position := 0;
      save.Write(e, SizeOf(Integer));
      save.WriteBuffer(temp.Memory^, e);
    finally
      FreeAndNil(temp);
    end;
    if TPlatformServices.Current.SupportsPlatformService(IFMXExtendedClipboardService, ClipService) then
    begin
      if not ClipService.IsCustomFormatRegistered('UIPassage') then
        ClipService.RegisterCustomFormat('UIPassage');
      save.Position := 0;
      ClipService.SetCustomFormat('UIPassage', save);
    end;
  finally
    FreeAndNil(sp);
    FreeAndNil(si);
    FreeAndNil(save);
  end;
end;

constructor TUIPassage.Create(AOwner: TComponent);
begin
  inherited;
  CanFocus := True;
  Cursor := crIBeam;
  Fill.Color := TAlphaColors.White;
  // 文段属性
  FInsertPos := -1;
  // 交互属性
  FSctUI := TSctUI.Create(Self);
  FSelStart := 0;
  FSelLength := 0;
  // 段落属性
  FFonts := TStringList.Create;
  FFonts.Add('宋体');
  FParaMaps := TObjectList<TBitmap>.Create;
  FParaMaps.Add(TBitmap.Create(1, 1));
  FParaHeight := TList<Single>.Create;
  FParaHeight.Add(0);
  FParaEnd := TList<Single>.Create;
  FParaEnd.Add(0);
  FParaSct := TList<PParaSct>.Create;
  FParaSct.Add(ParaSctP(FSctUI.ParaSct));
  FParas := TObjectList<TList<PSctStr>>.Create;
  FParas.Add(TList<PSctStr>.Create);
  FParaImages := TObjectList<TObjectList<TParaImage>>.Create;
  FParaImages.Add(TObjectList<TParaImage>.Create);
  // 交互信息
  FLineLeft := TObjectList<TList<Single>>.Create;
  FLineLeft.Add(TList<Single>.Create);
  FLineTop := TObjectList<TList<Single>>.Create;
  FLineTop.Add(TList<Single>.Create);
  FCharRight := TObjectList<TObjectList<TList<Single>>>.Create;
  FCharRight.Add(TObjectList<TList<Single>>.Create);
  FFlicker := False;
  FDrawCursor := True;
  // 控制
  FIsPsgUpdating := False;
  FImageIndex := 0;
  FSelImage := False;
  FScrollPos := 0;
  FMouseCon := [TMouseInfo.Wheeling];
  FDelayCon := -1;
  FBufferStr := '';
  FCanUndo := False;
  // 初始化
  DrawToMap(0);
end;

procedure TUIPassage.CutToClipboard;
begin
  CopyToClipboard;
  DeleteString(FSelStart, FSelLength);
  FSelLength := 0;
end;

procedure TUIPassage.DelCharAfter;
var
  c: Integer;
begin
  if FSelLength > 0 then
  begin
    c := FSelLength;
    FSelLength := 0;
    DeleteString(FSelStart, c);
  end
  else if FSelStart < FParas.Count - 1 + GetTextLength then
    DeleteString(FSelStart, 1);
  Repaint;
end;

procedure TUIPassage.DeleteChar;
var
  c: Integer;
begin
  if FSelLength > 0 then
  begin
    c := FSelLength;
    FSelLength := 0;
    DeleteString(FSelStart, c);
  end
  else if FSelStart > 0 then
  begin
    DeleteString(FSelStart - 1, 1);
    SelStart := FSelStart - 1;
  end;
  Repaint;
end;

procedure TUIPassage.DeletePara(const Index: Integer);
var
  i: Integer;
begin
  if Index >= FParas.Count then
    Exit;
  for i := 0 to FParas[Index].Count - 1 do
    DeleteSctStr(Index, i);
  FParas.Delete(Index);
  FParaImages.Delete(Index);
  FParaMaps.Delete(Index);
  FParaHeight.Delete(Index);
  FParaEnd.Delete(Index);
  FParaSct.Delete(Index);
  FLineTop.Delete(Index);
  FLineLeft.Delete(Index);
  FCharRight.Delete(Index);
end;

procedure TUIPassage.DeleteSctStr(const ParaIndex, StrIndex: Integer);
begin
  if (ParaIndex < FParas.Count) and (StrIndex < FParas[ParaIndex].Count) then
  begin
    if FParas[ParaIndex][StrIndex].Sct <> nil then
      Dispose(FParas[ParaIndex][StrIndex].Sct);
    if FParas[ParaIndex][StrIndex] <> nil then
      Dispose(FParas[ParaIndex][StrIndex]);
    FParas[ParaIndex].Delete(StrIndex);
  end;
end;

procedure TUIPassage.DeleteString(const StartPos, Count: Integer);
var
  pi1, pi2, si1, si2, ci1, ci2, i, tc, ti: Integer;
begin
  if Count <= 0 then
    Exit;
  GetCurrentPos(StartPos, pi1, si1, ci1);
  GetCurrentPos(StartPos + Count, pi2, si2, ci2);
  // 获取首段选取之前的图片数量
  tc := 0;
  for i := 0 to si1 - 1 do
  begin
    if FParas[pi1][i].Sct.Effect = TCharEffect.Image then
      Inc(tc, FParas[pi1][i].Str.Length);
  end;
  if (si1 < FParas[pi1].Count) and (FParas[pi1][si1].Sct.Effect = TCharEffect.Image) then
    Inc(tc, ci1);
  if (pi1 = pi2) and (si1 = si2) then
  begin
    if FParas[pi1][si1].Sct.Effect = TCharEffect.Image then
      FParaImages[pi1].DeleteRange(tc, Count);
    FParas[pi1][si1].Str := FParas[pi1][si1].Str.Remove(ci1, Count);
    if FParas[pi1][si1].Str = '' then
      DeleteSctStr(pi1, si1);
  end
  else if (pi1 = pi2) then
  begin
    // 获取首段选取位置末之前的图片数量
    ti := 0;
    for i := 0 to si2 - 1 do
    begin
      if FParas[pi1][i].Sct.Effect = TCharEffect.Image then
        Inc(ti, FParas[pi1][i].Str.Length);
    end;
    if (si2 < FParas[pi1].Count) and (FParas[pi1][si2].Sct.Effect = TCharEffect.Image) then
      Inc(ti, ci2);
    if ti > tc then
      FParaImages[pi1].DeleteRange(tc, ti - tc);
    FParas[pi1][si1].Str := FParas[pi1][si1].Str.Remove(ci1);
    FParas[pi1][si2].Str := FParas[pi1][si2].Str.Remove(0, ci2);
    if FParas[pi1][si2].Str = '' then
      DeleteSctStr(pi1, si2);
    for i := si1 + 1 to si2 - 1 do
      DeleteSctStr(pi1, si1 + 1);
    if FParas[pi1][si1].Str = '' then
      DeleteSctStr(pi1, si1);
  end
  else
  begin
    // 删除首段图片
    if tc < FParaImages[pi1].Count then
      FParaImages[pi1].DeleteRange(tc, FParaImages[pi1].Count - tc);
    // 获取末端选取之前的图片数量
    tc := 0;
    for i := 0 to si2 - 1 do
    begin
      if FParas[pi2][i].Sct.Effect = TCharEffect.Image then
        Inc(tc, FParas[pi2][i].Str.Length);
    end;
    if (si2 < FParas[pi2].Count) and (FParas[pi2][si2].Sct.Effect = TCharEffect.Image) then
      Inc(tc, ci2);
    // 删除末段图片
    if tc > 0 then
      FParaImages[pi2].DeleteRange(0, tc);
    // 删除文字
    if si1 < FParas[pi1].Count then
    begin
      FParas[pi1][si1].Str := FParas[pi1][si1].Str.SubString(0, ci1);
      for i := si1 + 1 to FParas[pi1].Count - 1 do
        DeleteSctStr(pi1, si1 + 1);
      if FParas[pi1][si1].Str = '' then
        DeleteSctStr(pi1, si1);
    end;
    if si2 < FParas[pi2].Count then
    begin
      FParas[pi2][si2].Str := FParas[pi2][si2].Str.SubString(ci2);
      for i := 0 to si2 - 1 do
        DeleteSctStr(pi2, 0);
      if FParas[pi2][0].Str = '' then
        DeleteSctStr(pi2, 0);
    end;
    for i := pi1 + 1 to pi2 - 1 do
      DeletePara(pi1 + 1);
    if FParas[pi1 + 1].Count = 0 then
    begin
      DeletePara(pi1 + 1);
    end
    else
    begin
      if (FParas[pi1].Count > 0) and (FParas[pi1 + 1].Count > 0)
        and CharSctSame(FParas[pi1].Last.Sct^, FParas[pi1 + 1][0].Sct^) then
      begin
        FParas[pi1].Last.Str := FParas[pi1].Last.Str + FParas[pi1 + 1][0].Str;
        DeleteSctStr(pi1 + 1, 0);
      end;
      for i := 0 to FParas[pi1 + 1].Count - 1 do
      begin
        FParas[pi1].Add(FParas[pi1 + 1][0]);
        FParas[pi1 + 1][0] := nil;
        FParas[pi1 + 1].Delete(0);
      end;
      FParaImages[pi1 + 1].OwnsObjects := False;
      for i := 0 to FParaImages[pi1 + 1].Count - 1 do
      begin
        FParaImages[pi1].Add(FParaImages[pi1 + 1][i]);
        FParaImages[pi1 + 1][i] := nil;
      end;
      FParaImages[pi1 + 1].OwnsObjects := True;
      DeletePara(pi1 + 1);
    end;
  end;
  DrawToMap(pi1);
end;

destructor TUIPassage.Destroy;
begin
  Clear;
  FreeAndNil(FSctUI);
  FreeAndNil(FFonts);
  FreeAndNil(FParaHeight);
  FreeAndNil(FParaEnd);
  FreeAndNil(FParaMaps);
  FreeAndNil(FParaSct);
  FreeAndNil(FParas);
  FreeAndNil(FLineLeft);
  FreeAndNil(FLineTop);
  FreeAndNil(FCharRight);
  FreeAndNil(FParaImages);
  if Assigned(FUndoCon) then
    FreeAndNil(FUndoCon);
  if FFlicker then
    FreeAndNil(FTimer);
  inherited;
end;

function TUIPassage.DisableUndo: Boolean;
begin
  if not Assigned(FUndoCon) then
    Exit(False);
  FCanUndo := False;
  FreeAndNil(FUndoCon);
  Exit(True);
end;

procedure TUIPassage.DividePara;
var
  pi, si, ci, i, tc: Integer;
begin
  if FSelImage then
    FSelImage := False;
  if FSelLength > 0 then
  begin
    DeleteString(FSelStart, FSelLength);
    FSelLength := 0;
  end;
  GetCurrentPos(FSelStart, pi, si, ci);
  // 获取图像数量
  tc := 0;
  for i := 0 to si - 1 do
  begin
    if FParas[pi][i].Sct.Effect = TCharEffect.Image then
      Inc(tc, FParas[pi][i].Str.Length);
  end;
  if (si < FParas[pi].Count) and (FParas[pi][si].Sct.Effect = TCharEffect.Image) then
    Inc(tc, ci);
  if (FParas[pi].Count = 0) or ((si = FParas[pi].Count - 1) and (ci = FParas[pi][si].Str.Length)) then
  begin
    InsertPara(pi + 1);
  end
  else if (si = 0) and (ci = 0) then
  begin
    InsertPara(pi);
  end
  else
  begin
    InsertPara(pi + 1);
    for i := si + 1 to FParas[pi].Count - 1 do
    begin
      FParas[pi + 1].Add(FParas[pi][i]);
      FParas[pi][i] := nil;
    end;
    FParas[pi].DeleteRange(si + 1, FParas[pi].Count - si - 1);
    if ci < FParas[pi][si].Str.Length then
    begin
      FParas[pi + 1].Insert(0, SctStrP(FParas[pi][si].Sct^, FParas[pi][si].Str.Substring(ci)));
      FParas[pi][si].Str := FParas[pi][si].Str.Remove(ci);
    end;
  end;
  if tc < FParaImages[pi].Count then
  begin
    FParaImages[pi].OwnsObjects := False;
    for i := tc to FParaImages[pi].Count - 1 do
    begin
      FParaImages[pi + 1].Add(FParaImages[pi][i]);
      FParaImages[pi][i] := nil;
    end;
    FParaImages[pi].DeleteRange(tc, FParaImages[pi].Count - tc);
    FParaImages[pi].OwnsObjects := True;
  end;
  DrawToMap(pi);
  DrawToMap(pi + 1);
  SelStart := FSelStart + 1;
  Repaint;
end;

procedure TUIPassage.DoEnter;
begin
  inherited;
  BringToFront;
  if FReadOnly then
    Exit;
  if Assigned(FManager) and FManager.FReadOnly then
    Exit;
  if not FFlicker then
  begin
    FTimer := TTimer.Create(Owner);
    FTimer.Enabled := True;
    FTimer.Interval := 500;
    FTimer.OnTimer := Flickering;
    FFlicker := True;
    if FManager <> nil then
      FManager.FActPass := Self;
  end;
end;

procedure TUIPassage.DoExit;
begin
  inherited;
  if FFlicker then
  begin
    FreeAndNil(FTimer);
    FFlicker := False;
    FDrawCursor := False;
  end;
end;

procedure TUIPassage.DoMouseLeave;
begin
  inherited;
  SetSelection(FSelStart, FSelStart + FSelLength);
  if FMouseDn and (FInsertPos > -1) then
  begin
    if FInsertPos < FSelStart then
    begin
      CopyToClipboard;
      DeleteString(FSelStart, FSelLength);
      FSelLength := 0;
      FSelStart := FInsertPos;
      PasteFromClipboard;
      Repaint;
    end
    else if FInsertPos > FSelStart + FSelLength then
    begin
      CopyToClipboard;
      Dec(FInsertPos, FSelLength);
      DeleteString(FSelStart, FSelLength);
      FSelLength := 0;
      FSelStart := FInsertPos;
      PasteFromClipboard;
      Repaint;
    end;
    FInsertPos := -1;
    FSelLength := 0;
  end;
  if FUndoWaiting then
  begin
    SaveToStream(FUndoCon.RecordInfo);
    FUndoWaiting := False;
  end;
  FMouseDn := False;
end;

procedure TUIPassage.DrawSelection;
var
  pi1, pi2, li1, li2, ci1, ci2, pt, tl, i: Integer;
  st: Single;
  bb: TBrush;
begin
  if FSelLength = 0 then
    Exit;
  GetLinePos(Min(FSelStart, FSelStart + FSelLength), pi1, li1, ci1);
  GetLinePos(Max(FSelStart, FSelStart + FSelLength), pi2, li2, ci2);
  if (pi1 = pi2) and (li1 = li2) and (ci1 = ci2) then
    Exit;
  st := Padding.Top - FScrollPos;
  if TMouseInfo.Moving in FMouseCon then
    st := st + 5;
  for i := 0 to pi1 - 1 do
    st := st + FParaHeight[i];
  bb := TBrush.Create(TBrushKind.Solid, TAlphaColors.Gray);
  // 只选取一行的情况
  if (pi1 = pi2) and (li1 = li2) then
  begin
    if ci1 = 0 then
    begin
      if li1 = FLineTop[pi1].Count - 1 then
        Canvas.FillRect(RectF(FLineLeft[pi1][li1], st + FLineTop[pi1][li1], FLeft + FCharRight[pi2][li2][ci2 - 1],
          st + FParaEnd[pi1]) * RectF(0, 0, Width, Height), 0, 0, [], 0.5, bb)
      else
        Canvas.FillRect(RectF(FLineLeft[pi1][li1], st + FLineTop[pi1][li1], FLeft + FCharRight[pi2][li2][ci2 - 1],
          st + FLineTop[pi1][li1 + 1]) * RectF(0, 0, Width, Height), 0, 0, [], 0.5, bb);
    end
    else
    begin
      if li1 = FLineTop[pi1].Count - 1 then
        Canvas.FillRect(RectF(FCharRight[pi1][li1][ci1 - 1], st + FLineTop[pi1][li1],
          FLeft + FCharRight[pi2][li2][ci2 - 1], st + FParaEnd[pi1]) * RectF(0, 0, Width, Height), 0, 0, [], 0.5, bb)
      else
        Canvas.FillRect(RectF(FCharRight[pi1][li1][ci1 - 1], st + FLineTop[pi1][li1],
          FLeft + FCharRight[pi2][li2][ci2 - 1], st + FLineTop[pi1][li1 + 1]) * RectF(0, 0, Width, Height),
          0, 0, [], 0.5, bb);
    end;
    FreeAndNil(bb);
    Exit;
  end;
  // 选取了多行，先分别绘制第一行，循环绘制中间部分，然后绘制最后一行
  if FParas[pi1].Count > 0 then
    if ci1 = 0 then
    begin
      if li1 = FLineTop[pi1].Count - 1 then
        Canvas.FillRect(RectF(FLineLeft[pi1][li1], st + FLineTop[pi1][li1], FLeft + FCharRight[pi1][li1].Last,
          st + FParaEnd[pi1]) * RectF(0, 0, Width, Height), 0, 0, [], 0.5, bb)
      else
        Canvas.FillRect(RectF(FLineLeft[pi1][li1], st + FLineTop[pi1][li1], FLeft + FCharRight[pi1][li1].Last,
          st + FLineTop[pi1][li1 + 1]) * RectF(0, 0, Width, Height), 0, 0, [], 0.5, bb);
    end
    else
    begin
      if li1 = FLineTop[pi1].Count - 1 then
        Canvas.FillRect(RectF(FCharRight[pi1][li1][ci1 - 1], st + FLineTop[pi1][li1],
          FLeft + FCharRight[pi1][li1].Last, st + FParaEnd[pi1]) * RectF(0, 0, Width, Height), 0, 0, [], 0.5, bb)
      else
        Canvas.FillRect(RectF(FCharRight[pi1][li1][ci1 - 1], st + FLineTop[pi1][li1],
          FLeft + FCharRight[pi1][li1].Last, st + FLineTop[pi1][li1 + 1]) * RectF(0, 0, Width, Height),
          0, 0, [], 0.5, bb);
    end;
  pt := pi1;
  tl := li1 + 1;
  while (pt < FLineTop.Count) and (tl >= FLineTop[pt].Count) do
  begin
    tl := 0;
    st := st + FParaHeight[pt];
    Inc(pt);
  end;
  while Smaller2(pt, tl, pi2, li2) do
  begin
    if FParas[pt].Count > 0 then
    begin
      if tl = FLineTop[pt].Count - 1 then
        Canvas.FillRect(RectF(FLineLeft[pt][tl], st + FLineTop[pt][tl], FLeft + FCharRight[pt][tl].Last,
          st + FParaEnd[pt]) * RectF(0, 0, Width, Height), 0, 0, [], 0.5, bb)
      else
        Canvas.FillRect(RectF(FLineLeft[pt][tl], st + FLineTop[pt][tl], FLeft + FCharRight[pt][tl].Last,
          st + FLineTop[pt][tl + 1]) * RectF(0, 0, Width, Height), 0, 0, [], 0.5, bb);
    end;
    Inc(tl);
    while (pt < FLineTop.Count) and (tl >= FLineTop[pt].Count) do
    begin
      tl := 0;
      st := st + FParaHeight[pt];
      Inc(pt);
    end;
  end;
  if (ci2 <> 0) or (li2 <> 0) then
  begin
    if li2 = FLineTop[pi2].Count - 1 then
      Canvas.FillRect(RectF(FLineLeft[pi2][li2], st + FLineTop[pi2][li2], FLeft + FCharRight[pi2][li2][ci2 - 1],
        st + FParaEnd[pi2]) * RectF(0, 0, Width, Height), 0, 0, [], 0.5, bb)
    else
      Canvas.FillRect(RectF(FLineLeft[pi2][li2], st + FLineTop[pi2][li2], FLeft + FCharRight[pi2][li2][ci2 - 1],
        st + FLineTop[pi2][li2 + 1]) * RectF(0, 0, Width, Height), 0, 0, [], 0.5, bb);
  end;
  FreeAndNil(bb);
end;

procedure TUIPassage.DrawToMap(Index: Integer);
var
  fi, li, ri, um, dm:        Single;             // 缩进长度（首行、左侧、右侧），偏移最大值（上、下）
  ts, tt:                    Single;             // 临时实数
  ti, tj:                    Integer;            // 临时整数
  tw, th:                    Single;             // 临时行宽，临时行高
  cx, ga:                    Single;             // 横坐标，间距
  i, j:                      Integer;            // 循环整数
  ln, cc, sc, tc, ii:        Integer;            // 行数，字索引，字符串索引，临时字数、图像索引
  ed:                        Boolean;            // 循环跳出
  lc, rc:                    TList<Integer>;     // 行字数、实际字数
  lw, lp:                    TList<Single>;      // 行宽度，行基准位置
  bb:                        TBrush;             // 背景刷
  st:                        string;             // 临时字符串
begin
  if Index >= FParas.Count then
    Exit;
  // 初始化
  FLineTop[Index].Clear;
  FLineLeft[Index].Clear;
  FCharRight[Index].Clear;
  // 绘制空段
  if FParas[Index].Count = 0 then
    with FParaMaps[Index] do
    begin
      Canvas.Font.Family := FSctUI.Font;
      Canvas.Font.Size := FSctUI.FSize * 2;
      tt := Canvas.TextHeight('　');
      ts := Canvas.TextWidth('　');
      FLineTop[Index].Add(tt * FParaSct[Index].SpaceBefore);
      FParaEnd[Index] := tt + FLineTop[Index][0];
      FParaHeight[Index] := tt * (1 + FParaSct[Index].SpaceAfter) + FLineTop[Index][0];
      Width := Ceil(Self.Width);
      Height := Ceil(FParaHeight[Index]);
      Clear(0);
      fi := FParaSct[Index].FirstIndent * ts + Padding.Left;
      ri := FParaSct[Index].RightOffset * ts + Padding.Right;
      if TMouseInfo.Wheeling in FMouseCon then
        ri := ri + 10;
      if fi + ri > Self.Width then
      begin
        fi := 0;
        ri := 0;
      end;
      case FParaSct[Index].Alignment of
        TParaAlign.Left, TParaAlign.Justify, TParaAlign.Stretch:
          FLineLeft[Index].Add(fi);
        TParaAlign.Center:
          FLineLeft[Index].Add((Width - fi - ri) / 2);
        TParaAlign.Right:
          FLineLeft[Index].Add(Width - ri);
      end;
      Exit;
    end;
  // 初始化
  lc := TList<Integer>.Create;
  rc := TList<Integer>.Create;
  lw := TList<Single>.Create;
  lp := TList<Single>.Create;
  cc := 0;
  sc := 0;
  ln := 0;
  ii := 0;
  // 校验格式
  i := 0;
  while i < FParas[Index].Count do
  begin
    if FParas[Index][i].Str = '' then
      DeleteSctStr(Index, i)
    else if (i > 0) and CharSctSame(FParas[Index][i].Sct^, FParas[Index][i - 1].Sct^) then
    begin
      FParas[Index][i - 1].Str := FParas[Index][i - 1].Str + FParas[Index][i].Str;
      DeleteSctStr(Index, i);
    end
    else
      Inc(i);
  end;
  // 校验尺寸，获取每一行字数、行数，每一行顶部位置、段落高度
  with FParaMaps[Index].Canvas do
  begin
    Font.Family := FFonts[FParas[Index][0].Sct.Font];
    Font.Size := FParas[Index][0].Sct.Size * 2;
    Font.Style := FParas[Index][0].Sct.Style;
  end;
  // 设置缩进
  ts := FParaMaps[Index].Canvas.TextWidth('　');
  fi := FParaSct[Index].FirstIndent * ts + Padding.Left;
  li := FParaSct[Index].LeftOffset * ts + Padding.Left;
  ri := FParaSct[Index].RightOffset * ts + Padding.Right;
  if TMouseInfo.Wheeling in FMouseCon then
    ri := ri + 10;
  if (fi + ri > Width) or (li + ri > Width) then
  begin
    fi := 0;
    li := 0;
    ri := 0;
  end;
  ed := False;
  // 段循环
  while not ed do
  begin
    if ln = 0 then
      tw := fi + ri
    else
      tw := li + ri;
    th := 0;
    tc := 0;
    tj := 0;
    um := 0;
    dm := 0;
    // 行循环
    while True do
    begin
      if FParas[Index][sc].Sct.Effect = TCharEffect.Image then
      begin
        st := '';
        tt := FParaImages[Index][ii].FWidth;
        if (tc = 0) and (tt + tw > Width) then
        begin
          FParaImages[Index][ii].Width := Width - tw;
          tt := FParaImages[Index][ii].FWidth;
        end;
      end
      else if AlphaOrNum(FParas[Index][sc].Str.Chars[cc]) then
      begin
        ti := 1;
        while (cc + ti < FParas[Index][sc].Str.Length) and AlphaOrNum(FParas[Index][sc].Str.Chars[cc + ti]) do
          Inc(ti);
        tt := FParaMaps[Index].Canvas.TextWidth(FParas[Index][sc].Str.Substring(cc, ti));
        if tw + tt <= Width then
        begin
          st := FParas[Index][sc].Str.Substring(cc, ti);
          Inc(cc, ti - 1);
          Inc(tj, ti - 1);
        end
        else if tc = 0 then
        begin
          Dec(ti);
          tt := FParaMaps[Index].Canvas.TextWidth(FParas[Index][sc].Str.Substring(cc, ti));
          while tw + tt > Width do
          begin
            Dec(ti);
            tt := FParaMaps[Index].Canvas.TextWidth(FParas[Index][sc].Str.Substring(cc, ti));
          end;
          st := FParas[Index][sc].Str.Substring(cc, ti);
          Inc(cc, ti - 1);
          Inc(tc, ti - 1);
          Inc(tj, ti - 1);
        end;
      end
      else
      begin
        tt := FParaMaps[Index].Canvas.TextWidth(FParas[Index][sc].Str.Chars[cc]);
        st := FParas[Index][sc].Str.Chars[cc];
      end;
      if (tw + tt > Width) and (tc > 0) then
      begin
        if ln = 0 then
          FLineTop[Index].Add(FParaSct[Index].SpaceBefore * th);
        lw.Add(tw);
        lc.Add(tc);
        rc.Add(tj);
        lp.Add(um + FLineTop[Index][ln] + th);
        FLineTop[Index].Add(lp[ln] + dm + FParaSct[Index].LineSpace * th);
        Inc(ln);
        Break;
      end;
      tw := tw + tt;
      if FParas[Index][sc].Sct.Effect = TCharEffect.Image then
      begin
        ts := FParaImages[Index][ii].Height;
        Inc(ii);
      end
      else
        ts := FParaMaps[Index].Canvas.TextHeight(st);
      Inc(tc);
      Inc(tj);
      if ts > th then
        th := ts;
      if ts + FParas[Index][sc].Sct.Offset > th + um then
        um := ts + FParas[Index][sc].Sct.Offset - th;
      if - FParas[Index][sc].Sct.Offset > dm then
        dm := - FParas[Index][sc].Sct.Offset;
      Inc(cc);
      if cc >= FParas[Index][sc].Str.Length then
      begin
        cc := 0;
        Inc(sc);
        if sc >= FParas[Index].Count then
        begin
          if ln = 0 then
            FLineTop[Index].Add(FParaSct[Index].SpaceBefore * th);
          ed := True;
          lw.Add(tw);
          lc.Add(tc);
          rc.Add(tj);
          lp.Add(um + FLineTop[Index][ln] + th);
          FParaHeight[Index] := lp[ln] + dm + FParaSct[Index].SpaceAfter * th;
          FParaEnd[Index] := lp[ln] + dm;
          Inc(ln);
          Break;
        end
        else
        begin
          FParaMaps[Index].Canvas.Font.Size := FParas[Index][sc].Sct.Size * 2;
          FParaMaps[Index].Canvas.Font.Family := FFonts[FParas[Index][sc].Sct.Font];
        end;
      end;
    end;
  end;
  // 绘图
  FParaMaps[Index].Width := Ceil(Width);
  FParaMaps[Index].Height := Ceil(FParaHeight[Index]);
  FParaMaps[Index].Clear($00FFFFFF);
  if FParaMaps[Index].Canvas.BeginScene then
  try
    // 绘制背景
    sc := 0;
    cc := 0;
    ii := 0;
    ga := 0;
    bb := TBrush.Create(TBrushKind.Solid, FParas[Index][0].Sct.BgColor);
    with FParaMaps[Index].Canvas do
    begin
      Font.Family := FFonts[FParas[Index][0].Sct.Font];
      Font.Size := FParas[Index][0].Sct.Size * 2;
      Font.Style := FParas[Index][0].Sct.Style;
    end;
    for i := 0 to ln - 1 do
    begin
      FCharRight[Index].Add(TList<Single>.Create);
      if i = 0 then
        cx := fi
      else
        cx := li;
      case FParaSct[Index].Alignment of
        TParaAlign.Center:
          cx := (Width - lw[i]) / 2 + cx;
        TParaAlign.Right:
          cx := Width - lw[i] + cx;
        TParaAlign.Justify:
          if (lc[i] > 1) and (i < ln - 1) then
            ga := (Width - lw[i]) / (lc[i] - 1)
          else
            ga := 0;
        TParaAlign.Stretch:
          if lc[i] > 1 then
            ga := (Width - lw[i]) / (lc[i] - 1)
          else
            ga := 0;
      end;
      FLineLeft[Index].Add(cx);
      ed := False;
      tj := 0;
      for j := 1 to lc[i] do
      begin
        if FParas[Index][sc].Sct.Effect = TCharEffect.Image then
        begin
          tt := FParaImages[Index][ii].FWidth;
          Inc(ii);
          Inc(tj);
        end
        else if AlphaOrNum(FParas[Index][sc].Str.Chars[cc]) then
        begin
          ti := 1;
          tt := FParaMaps[Index].Canvas.TextWidth(FParas[Index][sc].Str.Substring(cc, ti));
          while (cc + ti < FParas[Index][sc].Str.Length) and AlphaOrNum(FParas[Index][sc].Str.Chars[cc + ti])
            and (ti + tj < rc[i]) do
          begin
            if j = 1 then
              FCharRight[Index].Last.Add(cx + tt)
            else
              FCharRight[Index].Last.Add(FCharRight[Index].Last[tj - 1] + tt);
            Inc(ti);
            tt := FParaMaps[Index].Canvas.TextWidth(FParas[Index][sc].Str.Substring(cc, ti));
          end;
          Inc(cc, ti - 1);
          if (j = 1) and (ti + tj >= rc[i]) then
            ed := True;
          Inc(tj, ti);
        end
        else
        begin
          tt := FParaMaps[Index].Canvas.TextWidth(FParas[Index][sc].Str.Chars[cc]);
          Inc(tj);
        end;
        if j < lc[i] then
        case FParaSct[Index].Alignment of
          TParaAlign.Justify, TParaAlign.Stretch: tt := tt + ga;
        end;
        if i < ln - 1 then
          FParaMaps[Index].Canvas.FillRect(RectF(cx - 0.1, FLineTop[Index][i] - 0.1, cx + tt + 0.1,
           FLineTop[Index][i + 1] + 0.1), 0, 0, [], FParas[Index][sc].Sct.BgOpacity, bb)
        else
          FParaMaps[Index].Canvas.FillRect(RectF(cx - 0.1, FLineTop[Index][i] - 0.1, cx + tt + 0.1,
           FParaEnd[Index] + 0.1), 0, 0, [], FParas[Index][sc].Sct.BgOpacity, bb);
        cx := cx + tt;
        FCharRight[Index].Last.Add(cx);
        Inc(cc);
        if cc >= FParas[Index][sc].Str.Length then
        begin
          cc := 0;
          Inc(sc);
          if sc < FParas[Index].Count then
          begin
            bb := TBrush.Create(TBrushKind.Solid, FParas[Index][sc].Sct.BgColor);
            with FParaMaps[Index].Canvas do
            begin
              Font.Family := FFonts[FParas[Index][sc].Sct.Font];
              Font.Size := FParas[Index][sc].Sct.Size * 2;
              Font.Style := FParas[Index][sc].Sct.Style;
            end;
          end;
        end;
        if ed then
          Break;
      end;
    end;
    // 绘制文字内容
    sc := 0;
    cc := 0;
    ii := 0;
    with FParaMaps[Index].Canvas do
    begin
      Font.Family := FFonts[FParas[Index][0].Sct.Font];
      Font.Size := FParas[Index][0].Sct.Size * 2;
      Font.Style := FParas[Index][0].Sct.Style;
      Fill.Color := FParas[Index][0].Sct.Color;
    end;
    for i := 0 to ln - 1 do
    begin
      ed := False;
      tj := 0;
      cx := FLineLeft[Index][i];
      for j := 1 to lc[i] do
      begin
        if FParas[Index][sc].Sct.Effect = TCharEffect.Image then
        begin
          st := '';
          tt := FParaImages[Index][ii].FWidth;
          Inc(tj);
        end
        else if AlphaOrNum(FParas[Index][sc].Str.Chars[cc]) then
        begin
          ti := 1;
          while (cc + ti < FParas[Index][sc].Str.Length) and AlphaOrNum(FParas[Index][sc].Str.Chars[cc + ti])
            and (ti + tj < rc[i]) do
            Inc(ti);
          st := FParas[Index][sc].Str.Substring(cc, ti);
          tt := FParaMaps[Index].Canvas.TextWidth(st);
          Inc(cc, ti - 1);
          if (j = 1) and (ti + tj >= rc[i]) then
            ed := True;
          Inc(tj, ti);
        end
        else
        begin
          st := FParas[Index][sc].Str.Chars[cc];
          tt := FParaMaps[Index].Canvas.TextWidth(st);
          Inc(tj);
        end;
        if FParas[Index][sc].Sct.Effect = TCharEffect.Image then
        begin
          with FParaImages[Index][ii] do
            FParaMaps[Index].Canvas.DrawBitmap(FImage, RectF(0, 0, FImage.Width, FImage.Height),
              RectF(cx, lp[i]- FParas[Index][sc].Sct.Offset - FHeight, cx + tt, lp[i]- FParas[Index][sc].Sct.Offset),
              1);
          Inc(ii);
        end
        else
          FParaMaps[Index].Canvas.FillText(RectF(cx - tt, FLineTop[Index][i], cx + 2 * tt,
            lp[i] - FParas[Index][sc].Sct.Offset), st, False, FParas[Index][sc].Sct.Opacity, [],
            TTextAlign.Center, TTextAlign.Trailing);
        cx := FCharRight[Index][i][tj - 1];
        Inc(cc);
        if cc >= FParas[Index][sc].Str.Length then
        begin
          cc := 0;
          Inc(sc);
          if sc < FParas[Index].Count then
            with FParaMaps[Index].Canvas do
            begin
              Font.Family := FFonts[FParas[Index][sc].Sct.Font];
              Font.Size := FParas[Index][sc].Sct.Size * 2;
              Font.Style := FParas[Index][sc].Sct.Style;
              Fill.Color := FParas[Index][sc].Sct.Color;
            end;
        end;
        if ed then
          Break;
      end;
    end;
  finally
    FParaMaps[Index].Canvas.EndScene;
    FreeAndNil(lw);
    FreeAndNil(lc);
    FreeAndNil(rc);
    FreeAndNil(bb);
  end;
end;

function TUIPassage.EnableUndo(Count: Integer): Boolean;
begin
  if Assigned(FUndoCon) then
    Exit(False);
  FUndoCon := TUndoCon.Create(Count);
  FCanUndo := True;
  SaveToStream(FUndoCon.RecordInfo, True);
  Exit(True);
end;

procedure TUIPassage.EndUpdating;
begin
  FIsPsgUpdating := False;
end;

procedure TUIPassage.FindPosList(AStr: string; var Text: string; ListPos: TList<Integer>);
var
  p: Integer;
  i, os: Integer;
begin
  Text := '';
  ListPos.Clear;
  for i := 0 to FParas.Count - 1 do
    Text := Text + Paras[i] + #13;
  os := 0;
  repeat
    p := Text.IndexOf(AStr, os);
    if p >= 0 then
    begin
      ListPos.Add(p);
      os := p + 1;
    end;
  until p < 0;
end;

procedure TUIPassage.Flickering(Sender: TObject);
begin
  FDrawCursor := not FDrawCursor;
  Repaint;
end;

function TUIPassage.GetColor: TAlphaColor;
begin
  Exit(Fill.Color);
end;

procedure TUIPassage.GetCurrentPos(const StartPos: Integer; var ParaIndex, StrIndex, CharIndex: Integer);
var
  p, i, j: Integer;
begin
  if StartPos < 0 then
  begin
    ParaIndex := 0;
    StrIndex := 0;
    CharIndex := 0;
    Exit;
  end;
  p := StartPos;
  for i := 0 to FParas.Count - 1 do
  begin
    for j := 0 to FParas[i].Count - 1 do
      if p > FParas[i][j].Str.Length then
      begin
        Dec(p, FParas[i][j].Str.Length);
        Continue;
      end
      else
      begin
        ParaIndex := i;
        StrIndex := j;
        CharIndex := p;
        Exit;
      end;
    if p = 0 then
    begin
      ParaIndex := i;
      CharIndex := p;
      if FParas[i].Count = 0 then
        StrIndex := FParas[i].Count
      else
        StrIndex := FParas[i].Count - 1;
      Exit;
    end;
    Dec(p);
  end;
  ParaIndex := FParas.Count - 1;
  if FParas[ParaIndex].Count = 0 then
  begin
    StrIndex := 0;
    CharIndex := 0;
  end
  else
  begin
    StrIndex := FParas[ParaIndex].Count - 1;
    CharIndex := FParas[ParaIndex][StrIndex].Str.Length;
  end;
end;

procedure TUIPassage.GetLinePos(const StartPos: Integer; var ParaIndex, LineIndex, CharIndex: Integer);
var
  p, i, j: Integer;
begin
  if StartPos < 0 then
  begin
    ParaIndex := 0;
    LineIndex := 0;
    CharIndex := 0;
    Exit;
  end;
  p := StartPos;
  for i := 0 to FParas.Count - 1 do
  begin
    for j := 0 to FLineTop[i].Count - 1 do
    begin
      if FCharRight[i].Count = 0 then
      begin
        if p <> 0 then
          Break;
        ParaIndex := i;
        LineIndex := j;
        CharIndex := p;
        Exit;
      end
      else if p > FCharRight[i][j].Count then
      begin
        Dec(p, FCharRight[i][j].Count);
        Continue;
      end
      else
      begin
        ParaIndex := i;
        LineIndex := j;
        CharIndex := p;
        Exit;
      end;
    end;
    Dec(p);
  end;
  ParaIndex := FParas.Count - 1;
  LineIndex := FLineTop.Last.Count - 1;
  CharIndex := p;
end;

function TUIPassage.GetParaIndex(const StartPos: Integer): Integer;
var
  p, i, j: Integer;
begin
  if StartPos < -1 then
    Exit(0);
  if StartPos = -1 then
    p := FSelStart
  else
    p := StartPos;
  for i := 0 to FParas.Count - 1 do
  begin
    for j := 0 to FParas[i].Count - 1 do
      if p > FParas[i][j].Str.Length then
      begin
        Dec(p, FParas[i][j].Str.Length);
        Continue;
      end
      else
        Exit(i);
    if p = 0 then
      Exit(i);
    Dec(p);
  end;
  Exit(FParas.Count - 1);
end;

function TUIPassage.GetParaLength(Index: Integer): Integer;
var
  i, n: Integer;
begin
  n := 0;
  for i := 0 to FParas[Index].Count - 1 do
    Inc(n, FParas[Index][i].Str.Length);
  Exit(n);
end;

function TUIPassage.GetParaString(Index: Integer): string;
var
  i: Integer;
begin
  Result := '';
  if Index >= FParas.Count then
    Exit;
  for i := 0 to FParas[Index].Count - 1 do
    if FParas[Index][i].Sct.Effect = TCharEffect.Text then
      Result := Result + FParas[Index][i].Str;
end;

function TUIPassage.GetSelPos(const ParaIndex, LineIndex, CharIndex: Integer): Integer;
var
  p, i, j: Integer;
begin
  if (ParaIndex >= FParas.Count) or (LineIndex >= FLineTop[ParaIndex].Count) then
    Exit(-1);
  p := 0;
  for i := 0 to ParaIndex - 1 do
  begin
    for j := 0 to FParas[i].Count - 1 do
      Inc(p, FParas[i][j].Str.Length);
    Inc(p);
  end;
  for i := 0 to LineIndex - 1 do
    Inc(p, FCharRight[ParaIndex][i].Count);
  Inc(p, CharIndex);
  Exit(p);
end;

function TUIPassage.GetSelText: string;
var
  pi1, pi2, si1, si2, ci1, ci2, i, j: Integer;
  st: TStringList;
  s: string;
begin
  if FSelLength = 0 then
    Exit;
  GetCurrentPos(FSelStart, pi1, si1, ci1);
  GetCurrentPos(FSelStart + FSelLength, pi2, si2, ci2);
  st := TStringList.Create;
  try
    if (pi1 = pi2) and (si1 = si2) then
      st.Add(FParas[pi1][si1].Str.Substring(ci1, ci2 - ci1))
    else if pi1 = pi2 then
    begin
      s := FParas[pi1][si1].Str.Substring(ci1);
      for i := si1 + 1 to si2 - 1 do
        s := s + FParas[pi1][i].Str;
      s := s + FParas[pi1][si2].Str;
    end
    else
    begin
      if FParas[pi1].Count > 0 then
      begin
        s := FParas[pi1][si1].Str.Substring(ci1);
        for i := si1 + 1 to FParas[pi1].Count - 1 do
          s := s + FParas[pi1][1].Str;
        st.Add(s);
      end;
      for i := pi1 + 1 to pi2 - 1 do
      begin
        s := '';
        for j := 0 to FParas[i].Count - 1 do
          s := s + FParas[i][j].Str;
        st.Add(s);
      end;
      if FParas[pi2].Count > 0 then
      begin
        s := '';
        for i := 0 to si2 - 1 do
          s := s + FParas[pi2][i].Str;
        s := s + FParas[pi2][si2].Str.Substring(0, ci2);
        st.Add(s);
      end;
    end;
    s := st.Text;
    if s.Substring(s.Length - 2, 2) = #$D#$A then
      s := s.Substring(0, s.Length - 2);
  finally
    FreeAndNil(st);
  end;
  Exit(s);
end;

function TUIPassage.GetTextLength: Integer;
var
  i, j, l: Integer;
begin
  l := 0;
  for i := 0 to FParas.Count - 1 do
    for j := 0 to FParas[i].Count - 1 do
      Inc(l, FParas[i][j].Str.Length);
  Exit(l);
end;

procedure TUIPassage.InsertStr(S: string; NonFormat: Boolean);
begin
  if FSelImage then
    FSelImage := False;
  if FSelLength > 0 then
    DeleteString(FSelStart, FSelLength);
  FSelLength := 0;
  InsertString(FSelStart, S, NonFormat);
  Inc(FSelStart, S.Length);
  Repaint;
  SelChange;
end;

procedure TUIPassage.InputBuffer;
begin
  if FDelayCon <> -1 then
  begin
    InsertStr(FBufferStr);
    FBufferStr := '';
    FDelayCon := -1;
    FreeAndNil(FDelayTimer);
  end;
end;

procedure TUIPassage.InputControl(Sender: TObject);
begin
  Dec(FDelayCon);
  if FDelayCon = 0 then
    InputBuffer;
end;

procedure TUIPassage.InsertChar(AChar: Char);
begin
  if FDelayCon = -1 then
  begin
    FDelayCon := 2;
    FDelayTimer := TTimer.Create(Owner);
    FDelayTimer.Interval := 10;
    FDelayTimer.OnTimer := InputControl;
    FBufferStr := AChar;
  end
  else
  begin
    FDelayCon := 2;
    FBufferStr := FBufferStr + AChar;
  end;
end;

procedure TUIPassage.InsertImage(Bitmap: TBitmap);
var
  pi, si, ci, tc, i: Integer;
begin
  FSctUI.FEffect := TCharEffect.Image;
  GetCurrentPos(FSelStart, pi, si, ci);
  // 获取该段选取位置之前图像的数目
  tc := 0;
  for i := 0 to si - 1 do
  begin
    if FParas[pi][i].Sct.Effect = TCharEffect.Image then
      Inc(tc, FParas[pi][i].Str.Length);
  end;
  if (si < FParas[pi].Count) and (FParas[pi][si].Sct.Effect = TCharEffect.Image) then
    Inc(tc, ci);
  // 在列表中插图
  FParaImages[pi].Insert(tc, TParaImage.Create(Self, Bitmap));
  // 插入相应文本
  InsertStr('　');
  FSctUI.FEffect := TCharEffect.Text;
end;

procedure TUIPassage.InsertImage(Stream: TStream);
var
  pi, si, ci, tc, i: Integer;
begin
  FSctUI.FEffect := TCharEffect.Image;
  GetCurrentPos(FSelStart, pi, si, ci);
  // 获取该段选取位置之前图像的数目
  tc := 0;
  for i := 0 to si - 1 do
  begin
    if FParas[pi][i].Sct.Effect = TCharEffect.Image then
      Inc(tc, FParas[pi][i].Str.Length);
  end;
  if (si < FParas[pi].Count) and (FParas[pi][si].Sct.Effect = TCharEffect.Image) then
    Inc(tc, ci);
  // 在列表中插图
  FParaImages[pi].Insert(tc, TParaImage.Create(Self, Stream));
  // 插入相应文本
  InsertStr('　');
  FSctUI.FEffect := TCharEffect.Text;
end;

procedure TUIPassage.InsertImage(FileName: string; Width: Single);
var
  pi, si, ci, tc, i: Integer;
begin
  FSctUI.FEffect := TCharEffect.Image;
  GetCurrentPos(FSelStart, pi, si, ci);
  // 获取该段选取位置之前图像的数目
  tc := 0;
  for i := 0 to si - 1 do
  begin
    if FParas[pi][i].Sct.Effect = TCharEffect.Image then
      Inc(tc, FParas[pi][i].Str.Length);
  end;
  if (si < FParas[pi].Count) and (FParas[pi][si].Sct.Effect = TCharEffect.Image) then
    Inc(tc, ci);
  // 在列表中插图
  FParaImages[pi].Insert(tc, TParaImage.Create(Self, FileName, Width));
  // 插入相应文本
  InsertStr('　');
  FSctUI.FEffect := TCharEffect.Text;
end;

procedure TUIPassage.InsertPara(const Index: Integer);
begin
  FParas.Insert(Index, TList<PSctStr>.Create);
  FParaImages.Insert(Index, TObjectList<TParaImage>.Create);
  FParaMaps.Insert(Index, TBitmap.Create(1, 1));
  FParaHeight.Insert(Index, 0);
  FParaEnd.Insert(Index, 0);
  FParaSct.Insert(Index, ParaSctP(FSctUI.ParaSct));
  FLineTop.Insert(Index, TList<Single>.Create);
  FLineLeft.Insert(Index, TList<Single>.Create);
  FCharRight.Insert(Index, TObjectList<TList<Single>>.Create);
end;

procedure TUIPassage.InsertString(const StartPos: Integer; AString: string; NonFormat: Boolean);
var
  pi, si, ci: Integer;
begin
  if AString = '' then
    Exit;
  GetCurrentPos(StartPos, pi, si, ci);
  if FParas[pi].Count = 0 then
  begin
    FParas[pi].Add(SctStrP(FSctUI.CharSct, AString));
  end
  else if CharSctSame(FParas[pi][si].Sct^, FSctUI.CharSct) or NonFormat then
  begin
    FParas[pi][si].Str.Insert(ci, AString);
  end
  else
  begin
    if ci = FParas[pi][si].Str.Length then
    begin
      FParas[pi].Insert(si + 1, SctStrP(FSctUI.CharSct, AString));
    end
    else if ci = 0 then
    begin
      FParas[pi].Insert(si, SctStrP(FSctUI.CharSct, AString));
    end
    else
    begin
      FParas[pi].Insert(si + 1, SctStrP(FParas[pi][si].Sct^, FParas[pi][si].Str.Substring(ci)));
      FParas[pi][si].Str := FParas[pi][si].Str.Remove(ci);
      FParas[pi].Insert(si + 1, SctStrP(FSctUI.CharSct, AString));
    end;
  end;
  DrawToMap(pi);
end;

procedure TUIPassage.KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
begin
  inherited;
  if FReadOnly then
    Exit;
  if Assigned(FManager) and FManager.FReadOnly then
    Exit;
  if KeyChar <> '' then
    InsertChar(KeyChar)
  else
  begin
    InputBuffer;
    if (ssShift in Shift) and (ssCtrl in Shift) then
    case Key of
      67:
        CopyText;
      90:
        Redo;
    end
    else if ssCtrl in Shift then
    case Key of
      65:
        SelectAll;
      67:
        CopyToClipboard;
      86:
        PasteFromClipboard;
      88:
        CutToClipboard;
      90:
        Undo;
    end
    else
    case Key of
      8:
        DeleteChar;
      13:
        DividePara;
      35:
        SelRecEnd;
      36:
        SelRecHome;
      37:
        SelRecLeft;
      38:
        SelRecUp;
      39:
        SelRecRight;
      40:
        SelRecDown;
      46:
        DelCharAfter;
    end;
  end;
end;

procedure TUIPassage.LoadFromFile(FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TUIPassage.LoadFromStream(Stream: TStream; Buffer: Boolean);
var
  c, d, e, i, j: Integer;
  b: Boolean;
  ts: Single;
  ac: Cardinal;
  temp: TMemoryStream;
  bs: TBytes;
begin
  if not Assigned(Stream) then
    Exit;
  // 初始化
  Stream.Read(e, SizeOf(Integer));
  SetLength(bs, e);
  Stream.ReadBuffer(bs, e);
  if WideStringOf(bs) <> 'ELT_P' then
    Exit;
  Clear;
  DeletePara(0);
  // 显示背景
  Stream.Read(b, SizeOf(Boolean));
  if not b then
    Fill.Kind := TBrushKind.None
  else
  // 图像背景
  begin
    Stream.Read(b, SizeOf(Boolean));
    if b then
    begin
      temp := TMemoryStream.Create;
      try
        Stream.Read(c, SizeOf(Integer));
        temp.SetSize(c);
        Stream.ReadBuffer(temp.Memory^, c);
        temp.Position := 0;
        Fill.Kind := TBrushKind.Bitmap;
        Fill.Bitmap.Bitmap.LoadFromStream(temp);
        temp.Position := 0;
        if Assigned(FBackground) then
          FBackground.Free;
        FBackground := TMemoryStream.Create;
        temp.SaveToStream(FBackground);
      finally
        FreeAndNil(temp);
      end;
    end
    else
    begin
      Stream.Read(ac, SizeOf(Cardinal));
      Fill.Kind := TBrushKind.Solid;
      Fill.Color := ac;
    end;
  end;
  // 鼠标控制信息
  MouseCon := [];
  Stream.Read(b, SizeOf(Boolean));
  if b then
    MouseCon := MouseCon + [TMouseInfo.Moving];
  Stream.Read(b, SizeOf(Boolean));
  if b then
    MouseCon := MouseCon + [TMouseInfo.Sizing];
  Stream.Read(b, SizeOf(Boolean));
  if b then
    MouseCon := MouseCon + [TMouseInfo.Wheeling];
  // 边距
  Stream.Read(ts, SizeOf(Single));
  Padding.Top := ts;
  Stream.Read(ts, SizeOf(Single));
  Padding.Bottom := ts;
  Stream.Read(ts, SizeOf(Single));
  Padding.Left := ts;
  Stream.Read(ts, SizeOf(Single));
  Padding.Right := ts;
  // 读取段落结构
  Stream.Read(c, SizeOf(Integer));
  for i := 0 to c - 1 do
  begin
    InsertPara(i);
    Stream.Read(FParaSct[i]^.FirstIndent, SizeOf(Single));
    Stream.Read(FParaSct[i]^.LeftOffset, SizeOf(Single));
    Stream.Read(FParaSct[i]^.RightOffset, SizeOf(Single));
    Stream.Read(FParaSct[i]^.SpaceBefore, SizeOf(Single));
    Stream.Read(FParaSct[i]^.SpaceAfter, SizeOf(Single));
    Stream.Read(FParaSct[i]^.LineSpace, SizeOf(Single));
    Stream.Read(FParaSct[i]^.Alignment, SizeOf(TParaAlign));
  end;
  // 读取文字信息
  Stream.Read(c, SizeOf(Integer));
  for i := 0 to c - 1 do
  begin
    Stream.Read(d, SizeOf(Integer));
    for j := 0 to d - 1 do
    begin
      // 读取文字结构
      Stream.Read(FSctUI.FFont, SizeOf(Integer));
      Stream.Read(FSctUI.FSize, SizeOf(Single));
      Stream.Read(FSctUI.FOpacity, SizeOf(Single));
      Stream.Read(FSctUI.FBgOpacity, SizeOf(Single));
      Stream.Read(FSctUI.FOffset, SizeOf(Single));
      Stream.Read(FSctUI.FColor, SizeOf(Cardinal));
      Stream.Read(FSctUI.FBgColor, SizeOf(Cardinal));
      Stream.Read(e, SizeOf(Integer));
      FSctUI.FStyle := [];
      if e mod 2 = 1 then
        FSctUI.FStyle := FSctUI.FStyle + [TFontStyle.fsBold];
      if e div 2 mod 2 = 1 then
        FSctUI.FStyle := FSctUI.FStyle + [TFontStyle.fsItalic];
      if e div 4 mod 2 = 1 then
        FSctUI.FStyle := FSctUI.FStyle + [TFontStyle.fsUnderline];
      if e div 8 = 1 then
        FSctUI.FStyle := FSctUI.FStyle + [TFontStyle.fsStrikeOut];
      Stream.Read(FSctUI.FEffect, SizeOf(TCharEffect));
      // 读取文字
      Stream.Read(e, SizeOf(Integer));
      SetLength(bs, e);
      Stream.ReadBuffer(bs, e);
      FParas[i].Add(SctStrP(FSctUI.CharSct, WideStringOf(bs)));
    end;
  end;
  temp := TMemoryStream.Create;
  try
    // 读取图像
    Stream.Read(c, SizeOf(Integer));
    for i := 0 to c -1 do
    begin
      Stream.Read(d, SizeOf(Integer));
      for j := 0 to d - 1 do
      begin
        Stream.Read(e, SizeOf(Integer));
        temp.SetSize(e);
        temp.Position := 0;
        Stream.ReadBuffer(temp.Memory^, e);
        FParaImages[i].Add(TParaImage.Create(Self, temp));
        Stream.Read(FParaImages[i][j].FWidth, SizeOf(Single));
        Stream.Read(FParaImages[i][j].FHeight, SizeOf(Single));
      end;
    end;
    // 读取字体
    FFonts.Clear;
    Stream.Read(e, SizeOf(Integer));
    temp.SetSize(e);
    temp.Position := 0;
    Stream.ReadBuffer(temp.Memory^, e);
    FFonts.LoadFromStream(temp);
  finally
    FreeAndNil(temp);
  end;
  // 初始化
  for i := 0 to FParas.Count - 1 do
    DrawToMap(i);
  // 缓存信息
  if Buffer then
  begin
    Stream.Read(ts, SizeOf(Single));
    ScrollPos := ts;
    Stream.Read(c, SizeOf(Integer));
    FSelStart := c;
    SelStart := c;
  end;
  Repaint;
end;

procedure TUIPassage.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  p: Integer;
begin
  inherited;
  if FReadOnly then
    Exit;
  if Assigned(FManager) and FManager.FReadOnly then
    Exit;
  case Button of
    TMouseButton.mbLeft:
      begin
        SetFocus;
        p := Select(X, Y);
        if (FSelLength > 0) and (p >= FSelStart) and (p <= FSelStart + FSelLength) then
        begin
          FInsertPos := p;
          FMouseDn := True;
          Cursor := crArrow;
          Repaint;
        end
        else
        case Cursor of
          crSizeAll:
            with TMouseCon.Create(Owner) do
              Initiate(Self, X + Self.Position.X, Y + Self.Position.Y, TMouseInfo.Moving);
          crSizeNWSE:
            with TMouseCon.Create(Owner) do
              Initiate(Self, X + Self.Position.X, Y + Self.Position.Y, TMouseInfo.Sizing);
          crArrow:
            with TMouseCon.Create(Owner) do
              Initiate(Self, X + Self.Position.X, Y + Self.Position.Y, TMouseInfo.Wheeling);
        else
          if ssShift in Shift then
          begin
            if p > FSelStart then
              SetSelection(FSelStart, p)
            else
              SetSelection(p, FSelStart + FSelLength);
          end
          else
          begin
            FSelPos := p;
            SelStart := FSelPos;
          end;
          FMouseDn := True;
          Repaint;
        end;
      end;
    TMouseButton.mbRight:
      if Assigned(FManager) then
      begin
        FManager.MouseDown(Button, Shift, X + Position.X, Y + Position.Y);
        Exit;
      end;
  end;
end;

procedure TUIPassage.MouseMove(Shift: TShiftState; X, Y: Single);
var
  it: Single;
begin
  inherited;
  if FReadOnly or (Assigned(FManager) and FManager.FReadOnly) then
    Exit;
  if Assigned(FManager) and ((FManager.FInfo = TManagerInfo.Create) or (not FManager.FMouseLR and FManager.FMouseDn)) then
  begin
    FManager.MouseMove(Shift, X + Position.X, Y + Position.Y);
    Exit;
  end;
  if (FInsertPos > -1) and FMouseDn and not FReadOnly then
  begin
    Cursor := crArrow;
    FInsertPos := Select(X, Y);
    SetSelRec;
    Repaint;
  end
  else
  begin
    if Assigned(FManager) then
      it := Abs(FManager.FIdentified) / 2
    else
      it := 1;
    if (Y < 4 + it) and (TMouseInfo.Moving in FMouseCon) then
      Cursor := crSizeAll
    else if (X > Width - 4 - it) and (Y > Height - 4 - it) and (TMouseInfo.Sizing in FMouseCon) then
      Cursor := crSizeNWSE
    else if (X > Width - 7 - it) and (Y > 10) and (Y < Height - 10) and (TMouseInfo.Wheeling in FMouseCon) then
      Cursor := crArrow
    else
      Cursor := crIBeam;
    if FMouseDn and (FSelPos <> -1) and not FReadOnly then
    begin
      SetSelInfo(FSelPos, Select(X, Y) - FSelPos);
      Repaint;
    end;
  end;
end;

procedure TUIPassage.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  pi1, pi2, si1, si2, ci1, ci2, i, j, tc, ti: Integer;
  spr: TObjectList<TList<PSctStr>>;
  sig: TObjectList<TObjectList<TParaImage>>;
begin
  inherited;
  if FReadOnly then
  begin
    FMouseDn := False;
    Exit;
  end;
  if Assigned(FManager) and FManager.FReadOnly then
    Exit;
  if Assigned(FManager) and ((FManager.FInfo = TManagerInfo.Create) or (not FManager.FMouseLR)) then
  begin
    FManager.MouseUp(Button, Shift, X + Position.X, Y + Position.Y);
    FMouseDn := False;
    Exit;
  end;
  if FMouseDn and (FInsertPos > -1) and not FReadOnly then
  begin
    if (FInsertPos < FSelStart) or (FInsertPos > FSelStart + FSelLength) then
    begin
      spr := TObjectList<TList<PSctStr>>.Create;
      sig := TObjectList<TObjectList<TParaImage>>.Create;
      try
        GetCurrentPos(FSelStart, pi1, si1, ci1);
        GetCurrentPos(FSelStart + FSelLength, pi2, si2, ci2);
        // 获取首段选取之前的图片数量
        tc := 0;
        for i := 0 to si1 - 1 do
        begin
          if FParas[pi1][i].Sct.Effect = TCharEffect.Image then
            Inc(tc, FParas[pi1][i].Str.Length);
        end;
        if (si1 < FParas[pi1].Count) and (FParas[pi1][si1].Sct.Effect = TCharEffect.Image) then
          Inc(tc, ci1);
        if (pi1 = pi2) and (si1 = si2) then
        begin
          sig.Add(TObjectList<TParaImage>.Create);
          if FParas[pi1][si1].Sct.Effect = TCharEffect.Image then
          begin
            for i := 0 to FSelLength - 1 do
              sig[0].Add(FParaImages[pi1][tc + i]);
          end;
          spr.Add(TList<PSctStr>.Create);
          spr[0].Add(SctStrP(FParas[pi1][si1].Sct^, FParas[pi1][si1].Str.SubString(ci1, FSelLength)));
        end
        else if (pi1 = pi2) then
        begin
          // 获取首段选取位置末之前的图片数量
          ti := 0;
          for i := 0 to si2 - 1 do
          begin
            if FParas[pi1][i].Sct.Effect = TCharEffect.Image then
              Inc(ti, FParas[pi1][i].Str.Length);
          end;
          if (si2 < FParas[pi1].Count) and (FParas[pi1][si2].Sct.Effect = TCharEffect.Image) then
            Inc(ti, ci2);
          sig.Add(TObjectList<TParaImage>.Create);
          if ti > tc then
          begin
            for i := tc to ti - 1 do
              sig[0].Add(FParaImages[pi1][i]);
          end;
          spr.Add(TList<PSctStr>.Create);
          if ci1 < FParas[pi1][si1].Str.Length then
            spr[0].Add(SctStrP(FParas[pi1][si1].Sct^, FParas[pi1][si1].Str.Substring(ci1)));
          for i := si1 + 1 to si2 - 1 do
            spr[0].Add(SctStrP(FParas[pi1][i].Sct^, FParas[pi1][i].Str));
          spr[0].Add(SctStrP(FParas[pi1][si2].Sct^, FParas[pi1][si2].Str.Substring(0, ci2)));
        end
        else
        begin
          // 复制首段图片
          sig.Add(TObjectList<TParaImage>.Create);
          if tc < FParaImages[pi1].Count then
          begin
            for i := tc to FParaImages[pi1].Count - 1 do
              sig[0].Add(FParaImages[pi1][i]);
          end;
          // 获取末端选取之前的图片数量
          tc := 0;
          for i := 0 to si2 - 1 do
          begin
            if FParas[pi2][i].Sct.Effect = TCharEffect.Image then
              Inc(tc, FParas[pi2][i].Str.Length);
          end;
          if (si2 < FParas[pi2].Count) and (FParas[pi2][si2].Sct.Effect = TCharEffect.Image) then
            Inc(tc, ci2);
          // 复制中间图片
          for i := pi1 + 1 to pi2 - 1 do
          begin
            sig.Add(TObjectList<TParaImage>.Create);
            for j := 0 to FParaImages[i].Count - 1 do
              sig[i - pi1].Add(FParaImages[i][j]);
          end;
          // 复制末段图片
          sig.Add(TObjectList<TParaImage>.Create);
          if tc > 0 then
          begin
            for i := 0 to tc - 1 do
              sig[pi2 - pi1].Add(FParaImages[pi2][i]);
          end;
          // 复制首段文字
          spr.Add(TList<PSctStr>.Create);
          spr[0].Add(SctStrP(FParas[pi1][si1].Sct^, FParas[pi1][si1].Str.Substring(ci1)));
          for i := si1 + 1 to FParas[pi1].Count - 1 do
            spr[0].Add(SctStrP(FParas[pi1][i].Sct^, FParas[pi1][i].Str));
          // 复制中间文字
          for i := pi1 + 1 to pi2 - 1 do
          begin
            spr.Add(TList<PSctStr>.Create);
            for j := 0 to FParas[i].Count - 1 do
              spr[i - pi1].Add(SctStrP(FParas[i][j].Sct^, FParas[i][j].Str));
          end;
          // 复制末段文字
          spr.Add(TList<PSctStr>.Create);
          for i := 0 to si2 - 1 do
            spr[pi2 - pi1].Add(SctStrP(FParas[pi2][i].Sct^, FParas[pi2][i].Str));
          if ci2 > 0 then
            spr[pi2 - pi1].Add(SctStrP(FParas[pi2][si2].Sct^, FParas[pi2][si2].Str.Substring(0, ci2)));
        end;
        if FInsertPos > FSelStart + FSelLength then
          Dec(FInsertPos, FSelLength);
        DeleteString(FSelStart, FSelLength);
        FSelStart := FInsertPos;
        GetCurrentPos(FSelStart, pi1, si1, ci1);
        if spr.Count = 1 then
        begin
          if (si1 < FParas[pi1].Count) and (ci1 < FParas[pi1][si1].Str.Length) then
          begin
            FParas[pi1].Insert(si1 + 1, SctStrP(FParas[pi1][si1].Sct^, FParas[pi1][si1].Str.Substring(ci1)));
            FParas[pi1][si1].Str := FParas[pi1][si1].Str.Substring(0, ci1);
          end;
          if si1 >= FParas[pi1].Count then
          begin
            for i := 0 to spr[0].Count - 1 do
              FParas[pi1].Insert(si1 + i, SctStrP(spr[0][i].Sct^, spr[0][i].Str));
          end
          else
          begin
            for i := 0 to spr[0].Count - 1 do
              FParas[pi1].Insert(si1 + 1 + i, SctStrP(spr[0][i].Sct^, spr[0][i].Str));
          end;
          tc := 0;
          for i := 0 to si1 - 1 do
          begin
            if FParas[pi1][i].Sct.Effect = TCharEffect.Image then
              Inc(tc, FParas[pi1][i].Str.Length);
          end;
          if (si1 < FParas[pi1].Count) and (FParas[pi1][si1].Sct.Effect = TCharEffect.Image) then
            Inc(tc, ci1);
          for i := 0 to sig[0].Count - 1 do
            FParaImages[pi1].Insert(tc + i, TParaImage.Create(sig[0][i]));
          DrawToMap(pi1);
        end
        else
        begin
          // 分段
          tc := 0;
          for i := 0 to si1 - 1 do
          begin
            if FParas[pi1][i].Sct.Effect = TCharEffect.Image then
              Inc(tc, FParas[pi1][i].Str.Length);
          end;
          if (si1 < FParas[pi1].Count) and (FParas[pi1][si1].Sct.Effect = TCharEffect.Image) then
            Inc(tc, ci1);
          if (FParas[pi1].Count = 0) or ((si1 = FParas[pi1].Count - 1) and (ci1 = FParas[pi1][si1].Str.Length)) then
          begin
            InsertPara(pi1 + 1);
          end
          else if (si1 = 0) and (ci1 = 0) then
          begin
            InsertPara(pi1);
          end
          else
          begin
            InsertPara(pi1 + 1);
            for i := si1 + 1 to FParas[pi1].Count - 1 do
            begin
              FParas[pi1 + 1].Add(FParas[pi1][i]);
              FParas[pi1][i] := nil;
            end;
            FParas[pi1].DeleteRange(si1 + 1, FParas[pi1].Count - si1 - 1);
            if ci1 < FParas[pi1][si1].Str.Length then
            begin
              FParas[pi1 + 1].Insert(0, SctStrP(FParas[pi1][si1].Sct^, FParas[pi1][si1].Str.Substring(ci1)));
              FParas[pi1][si1].Str := FParas[pi1][si1].Str.Remove(ci1);
            end;
          end;
          if tc < FParaImages[pi1].Count then
          begin
            for i := tc to FParaImages[pi1].Count - 1 do
            begin
              FParaImages[pi1 + 1].Add(FParaImages[pi1][i]);
              FParaImages[pi1][i] := nil;
            end;
            FParaImages[pi1].DeleteRange(tc, FParaImages[pi1].Count - tc);
          end;
          // 插入首段
          if si1 >= FParas[pi1].Count then
          begin
            for i := 0 to spr[0].Count - 1 do
              FParas[pi1].Insert(si1 + i, SctStrP(spr[0][i].Sct^, spr[0][i].Str));
          end
          else
          begin
            for i := 0 to spr[0].Count - 1 do
              FParas[pi1].Insert(si1 + 1 + i, SctStrP(spr[0][i].Sct^, spr[0][i].Str));
          end;
          tc := 0;
          for i := 0 to si1 - 1 do
          begin
            if FParas[pi1][i].Sct.Effect = TCharEffect.Image then
              Inc(tc, FParas[pi1][i].Str.Length);
          end;
          if (si1 < FParas[pi1].Count) and (FParas[pi1][si1].Sct.Effect = TCharEffect.Image) then
            Inc(tc, ci1);
          for i := 0 to sig[0].Count - 1 do
            FParaImages[pi1].Insert(tc + i, TParaImage.Create(sig[0][i]));
          DrawToMap(pi1);
          // 插入末段
          Inc(pi1);
          si1 := 0;
          ci1 := 0;
          if si1 >= FParas[pi1].Count then
          begin
            for i := 0 to spr.Last.Count - 1 do
              FParas[pi1].Insert(si1 + i, SctStrP(spr.Last[i].Sct^, spr.Last[i].Str));
          end
          else
          begin
            for i := 0 to spr.Last.Count - 1 do
              FParas[pi1].Insert(si1 + 1 + i, SctStrP(spr.Last[i].Sct^, spr.Last[i].Str));
          end;
          tc := 0;
          for i := 0 to si1 - 1 do
          begin
            if FParas[pi1][i].Sct.Effect = TCharEffect.Image then
              Inc(tc, FParas[pi1][i].Str.Length);
          end;
          if (si1 < FParas[pi1].Count) and (FParas[pi1][si1].Sct.Effect = TCharEffect.Image) then
            Inc(tc, ci1);
          for i := 0 to sig.Last.Count - 1 do
            FParaImages[pi1].Insert(tc + i, TParaImage.Create(sig.Last[i]));
          DrawToMap(pi1);
          // 插入中间段
          for i := 1 to spr.Count - 2 do
          begin
            InsertPara(pi1 - 1 + i);
            for j := 0 to spr[i].Count - 1 do
              FParas[pi1 - 1 + i].Add(SctStrP(spr[i][j].Sct^, spr[i][j].Str));
          end;
          for i := 1 to sig.Count - 2 do
          begin
            for j := 0 to sig[i].Count - 1 do
              FParaImages[pi1 - 1 + i].Add(TParaImage.Create(sig[i][j]));
            DrawToMap(pi1 - 1 + i);
          end;
        end;
      finally
        FreeAndNil(spr);
        FreeAndNil(sig);
      end;
      Repaint;
    end
    else
      SetSelInfo(FInsertPos, 0);
    FInsertPos := -1;
  end
  else if FMouseDn and (FSelPos <> -1) and not FReadOnly then
  begin
    SetSelection(FSelPos, Select(X, Y));
    FSelPos := -1;
    Repaint;
  end;
  if FUndoWaiting then
  begin
    SaveToStream(FUndoCon.RecordInfo);
    FUndoWaiting := False;
  end;
  FMouseDn := False;
end;

procedure TUIPassage.MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  inherited;
  ScrollPos := ScrollPos - WheelDelta;
end;

procedure TUIPassage.Paint;
var
  tp, tg, tt, it: Single;
  i: Integer;
  bb: TBrush;
  sb: TStrokeBrush;
begin
  inherited;
  Canvas.FillRect(GetShapeRect, 0, 0, [], 1, Fill);
  if Assigned(FManager) then
    it := Abs(FManager.FIdentified) / 2
  else
    it := 1;
  if TMouseInfo.Moving in FMouseCon then
    tt := 4 + it
  else
    tt := 0;
  tg := Padding.Top + Padding.Bottom + tt;
  if Canvas.BeginScene then
  try
    tp := 0;
    for i := 0 to FParas.Count - 1 do
    begin
      if (tp + FParaHeight[i] >= FScrollPos) and (tp < FScrollPos) then
      begin
        Canvas.DrawBitmap(FParaMaps[i], RectF(0, FScrollPos - tp, Width, FParaHeight[i]),
          RectF(0, Padding.Top + tt, Width, tp + FParaHeight[i] - FScrollPos + Padding.Top + tt), 1);
      end
      else if (tp >= FScrollPos) and (tp - FScrollPos + FParaHeight[i] <= Height - tg) then
      begin
        Canvas.DrawBitmap(FParaMaps[i], RectF(0, 0, Width, FParaHeight[i]),
          RectF(0, tp - FScrollPos + Padding.Top + tt, Width, tp - FScrollPos + FParaHeight[i] + Padding.Top + tt), 1);
      end
      else if (tp - FScrollPos < Height - Padding.Bottom)
        and (tp - FScrollPos + FParaHeight[i] > Height - tg) then
      begin
        Canvas.DrawBitmap(FParaMaps[i], RectF(0, 0, Width, Height + FScrollPos - tp - tg),
          RectF(0, tp - FScrollPos + Padding.Top + tt, Width, Height - Padding.Bottom), 1);
      end;
      tp := tp + FParaHeight[i];
    end;
    FPassHeight := tp;
  finally
    Canvas.EndScene;
  end;
  if (FSelLength <> 0) and not FReadOnly then // 绘制选取
    DrawSelection;
  bb := TBrush.Create(TBrushKind.Solid, TAlphaColors.Grey);
  try
    if TMouseInfo.Moving in FMouseCon then
      Canvas.FillEllipse(RectF(0, 0, Width, 4 + it), 1, bb);
    if TMouseInfo.Sizing in FMouseCon then
      Canvas.FillRect(RectF(Width - 4 - it, Height - 4 - it, Width, Height), 0, 0, [], 1, bb);
    if TMouseInfo.Wheeling in FMouseCon then
    begin
      if FPassHeight + tg - Height <= 0 then
        tp := 10
      else
        tp := 10 + FScrollPos / (FPassHeight + tg - Height) * (Height - 20);
      Canvas.FillEllipse(RectF(Width - 7 -  it, tp - 2.5 - 0.5 * it, Width - 2, tp + 2.5 + 0.5 * it), 1, bb);
      Canvas.FillRect(RectF(Width - 5.5 - 0.5 * it, 10, Width - 4.5 - 0.5 * it, Height - 10), 0, 0, [], 1, bb);
    end;
    if FDrawCursor and (FSelLength = 0) and not FReadOnly then
    begin
      bb.Color := TAlphaColors.White;
      Canvas.FillRect(RectF(FSelX - 1.2, FSelY, FSelX + 1.2, FSelY + FSelH), 0, 0, [], 1, bb);
      bb.Color := TAlphaColors.Black;
      Canvas.FillRect(RectF(FSelX - 0.6, FSelY, FSelX + 0.6, FSelY + FSelH), 0, 0, [], 1, bb);
    end
    else if (FInsertPos > -1) and not FReadOnly then
    begin
      bb.Color := TAlphaColors.White;
      Canvas.FillRect(RectF(FSelX - 1.5, FSelY, FSelX + 1.5, FSelY + FSelH), 0, 0, [], 1, bb);
      bb.Color := TAlphaColors.Black;
      Canvas.FillRect(RectF(FSelX - 1, FSelY, FSelX + 1, FSelY + FSelH), 0, 0, [], 1, bb);
    end;
  finally
    FreeAndNil(bb);
  end;
  sb := TStrokeBrush.Create(TBrushKind.Solid, TAlphaColors.White);
  try
    if FFlicker then
    begin
      sb.Thickness := it + 1;
      Canvas.DrawRect(RectF(0, 0, Width, Height), 0, 0, [], 1, sb);
      sb.Thickness := 0.5 * it + 0.5;
      sb.Color := TAlphaColors.Black;
      Canvas.DrawRect(RectF(0, 0, Width, Height), 0, 0, [], 1, sb);
    end
    else if FShowSides and Assigned(FManager) and (FManager.FActPass <> Self) then
    begin
      sb.Thickness := FSidesWidth;
      sb.Color := FSidesColor;
      Canvas.DrawRect(RectF(0, 0, Width, Height), 0, 0, [], 1, sb);
    end;
  finally
    FreeAndNil(sb);
  end;
  Repaint;
end;

procedure TUIPassage.PasteFromClipboard;
var
  pi1, si1, ci1, tc, i, j, c, d, e: Integer;
  save, temp: TMemoryStream;
  ClipService: IFMXExtendedClipboardService;
  sp: TObjectList<TList<PSctStr>>;
  si: TObjectList<TObjectList<TParaImage>>;
  bmp: TBitmap;
  st, sf: TStringList;
  bs: TBytes;
  s: string;
begin
  if FSelLength > 0 then
  begin
    DeleteString(FSelStart, FSelLength);
    FSelLength := 0;
  end;
  TPlatformServices.Current.SupportsPlatformService(IFMXExtendedClipboardService, ClipService);
  if not ClipService.IsCustomFormatRegistered('UIPassage') then
    ClipService.RegisterCustomFormat('UIPassage');
  st := TStringList.Create;
  sf := TStringList.Create;
  sp := TObjectList<TList<PSctStr>>.Create;
  si := TObjectList<TObjectList<TParaImage>>.Create;
  save := TMemoryStream.Create;
  try
    if (not ClipService.HasCustomFormat('UIPassage')) and ClipService.HasImage then
    begin
      bmp := TBitmap.Create;
      try
        bmp.Assign(ClipService.GetImage);
        InsertImage(bmp);
      finally
        FreeAndNil(bmp);
      end;
      Repaint;
      Exit;
    end;
    if (not ClipService.HasCustomFormat('UIPassage')) and ClipService.HasText then
    begin
      st.Text := ClipService.GetText;
      for i := 0 to st.Count - 1 do
      begin
        InsertString(FSelStart, st[i]);
        Inc(FSelStart, st[i].Length);
        if i < st.Count - 1 then
          DividePara;
        SelStart := FSelStart;
      end;
      Repaint;
      Exit;
    end;
    if not ClipService.HasCustomFormat('UIPassage') then
      Exit;
    if not ClipService.GetCustomFormat('UIPassage', save) then
      Exit;
    // 读取文字信息
    save.Read(c, SizeOf(Integer));
    for i := 0 to c - 1 do
    begin
      sp.Add(TList<PSctStr>.Create);
      si.Add(TObjectList<TParaImage>.Create);
      save.Read(d, SizeOf(Integer));
      for j := 0 to d - 1 do
      begin
        // 读取文字结构
        save.Read(FSctUI.FFont, SizeOf(Integer));
        save.Read(FSctUI.FSize, SizeOf(Single));
        save.Read(FSctUI.FOpacity, SizeOf(Single));
        save.Read(FSctUI.FBgOpacity, SizeOf(Single));
        save.Read(FSctUI.FOffset, SizeOf(Single));
        save.Read(FSctUI.FColor, SizeOf(Cardinal));
        save.Read(FSctUI.FBgColor, SizeOf(Cardinal));
        save.Read(e, SizeOf(Integer));
        FSctUI.FStyle := [];
        if e div 8 = 1 then
          FSctUI.FStyle := FSctUI.FStyle + [TFontStyle.fsStrikeOut];
        if e div 4 mod 2 = 1 then
          FSctUI.FStyle := FSctUI.FStyle + [TFontStyle.fsUnderline];
        if e div 2 mod 2 = 1 then
          FSctUI.FStyle := FSctUI.FStyle + [TFontStyle.fsItalic];
        if e mod 2 = 1 then
          FSctUI.FStyle := FSctUI.FStyle + [TFontStyle.fsBold];
        save.Read(FSctUI.FEffect, SizeOf(TCharEffect));
        // 读取文字
        save.Read(e, SizeOf(Integer));
        SetLength(bs, e);
        save.ReadBuffer(bs, e);
        sp[i].Add(SctStrP(FSctUI.CharSct, WideStringOf(bs)));
      end;
    end;
    temp := TMemoryStream.Create;
    try
      // 读取图像
      save.Read(c, SizeOf(Integer));
      for i := 0 to c -1 do
      begin
        save.Read(d, SizeOf(Integer));
        for j := 0 to d - 1 do
        begin
          save.Read(e, SizeOf(Integer));
          temp.SetSize(e);
          temp.Position := 0;
          save.ReadBuffer(temp.Memory^, e);
          si[i].Add(TParaImage.Create(Self, temp));
          save.Read(si[i][j].FWidth, SizeOf(Single));
          save.Read(si[i][j].FHeight, SizeOf(Single));
        end;
      end;
      // 读取字体
      save.Read(e, SizeOf(Integer));
      temp.SetSize(e);
      temp.Position := 0;
      save.ReadBuffer(temp.Memory^, e);
      sf.LoadFromStream(temp);
    finally
      FreeAndNil(temp);
    end;
    // 协调字体
    if not FFonts.Equals(sf) then
    begin
      for i := 0 to sf.Count - 1 do
      begin
        for j := 0 to FFonts.Count do
          if j = FFonts.Count then
            FFonts.Add(sf[i])
          else if FFonts[j] = sf[i] then
            Break;
      end;
      for i := 0 to sp.Count - 1 do
        for j := 0 to sp[i].Count - 1 do
        begin
          s := sf[sp[i][j].Sct.Font];
          for c := 0 to FFonts.Count - 1 do
            if FFonts[c] = s then
            begin
              sp[i][j].Sct.Font := c;
              Break;
            end;
        end;
    end;
    // 校验纯文字
    for i := 0 to sp.Count - 1 do
    begin
      s := '';
      for j := 0 to sp[i].Count - 1 do
        s := s + sp[i][j].Str;
      st.Add(s);
    end;
    // 写入
    GetCurrentPos(FSelStart, pi1, si1, ci1);
    if sp.Count = 1 then
    begin
      if (si1 < FParas[pi1].Count) and (ci1 < FParas[pi1][si1].Str.Length) then
      begin
        FParas[pi1].Insert(si1 + 1, SctStrP(FParas[pi1][si1].Sct^, FParas[pi1][si1].Str.Substring(ci1)));
        FParas[pi1][si1].Str := FParas[pi1][si1].Str.Substring(0, ci1);
      end;
      if si1 >= FParas[pi1].Count then
      begin
        for i := 0 to sp[0].Count - 1 do
          FParas[pi1].Insert(si1 + i, SctStrP(sp[0][i].Sct^, sp[0][i].Str));
      end
      else
      begin
        for i := 0 to sp[0].Count - 1 do
          FParas[pi1].Insert(si1 + 1 + i, SctStrP(sp[0][i].Sct^, sp[0][i].Str));
      end;
      tc := 0;
      for i := 0 to si1 - 1 do
      begin
        if FParas[pi1][i].Sct.Effect = TCharEffect.Image then
          Inc(tc, FParas[pi1][i].Str.Length);
      end;
      if (si1 < FParas[pi1].Count) and (FParas[pi1][si1].Sct.Effect = TCharEffect.Image) then
        Inc(tc, ci1);
      for i := 0 to si[0].Count - 1 do
        FParaImages[pi1].Insert(tc + i, TParaImage.Create(si[0][i]));
      DrawToMap(pi1);
    end
    else
    begin
      // 分段
      tc := 0;
      for i := 0 to si1 - 1 do
      begin
        if FParas[pi1][i].Sct.Effect = TCharEffect.Image then
          Inc(tc, FParas[pi1][i].Str.Length);
      end;
      if (si1 < FParas[pi1].Count) and (FParas[pi1][si1].Sct.Effect = TCharEffect.Image) then
        Inc(tc, ci1);
      if (FParas[pi1].Count = 0) or ((si1 = FParas[pi1].Count - 1) and (ci1 = FParas[pi1][si1].Str.Length)) then
      begin
        InsertPara(pi1 + 1);
      end
      else if (si1 = 0) and (ci1 = 0) then
      begin
        InsertPara(pi1);
      end
      else
      begin
        InsertPara(pi1 + 1);
        for i := si1 + 1 to FParas[pi1].Count - 1 do
        begin
          FParas[pi1 + 1].Add(FParas[pi1][i]);
          FParas[pi1][i] := nil;
        end;
        FParas[pi1].DeleteRange(si1 + 1, FParas[pi1].Count - si1 - 1);
        if ci1 < FParas[pi1][si1].Str.Length then
        begin
          FParas[pi1 + 1].Insert(0, SctStrP(FParas[pi1][si1].Sct^, FParas[pi1][si1].Str.Substring(ci1)));
          FParas[pi1][si1].Str := FParas[pi1][si1].Str.Remove(ci1);
        end;
      end;
      if tc < FParaImages[pi1].Count then
      begin
        for i := tc to FParaImages[pi1].Count - 1 do
        begin
          FParaImages[pi1 + 1].Add(FParaImages[pi1][i]);
          FParaImages[pi1][i] := nil;
        end;
        FParaImages[pi1].DeleteRange(tc, FParaImages[pi1].Count - tc);
      end;
      // 插入首段
      if si1 >= FParas[pi1].Count then
      begin
        for i := 0 to sp[0].Count - 1 do
          FParas[pi1].Insert(si1 + i, SctStrP(sp[0][i].Sct^, sp[0][i].Str));
      end
      else
      begin
        for i := 0 to sp[0].Count - 1 do
          FParas[pi1].Insert(si1 + 1 + i, SctStrP(sp[0][i].Sct^, sp[0][i].Str));
      end;
      tc := 0;
      for i := 0 to si1 - 1 do
      begin
        if FParas[pi1][i].Sct.Effect = TCharEffect.Image then
          Inc(tc, FParas[pi1][i].Str.Length);
      end;
      if (si1 < FParas[pi1].Count) and (FParas[pi1][si1].Sct.Effect = TCharEffect.Image) then
        Inc(tc, ci1);
      for i := 0 to si[0].Count - 1 do
        FParaImages[pi1].Insert(tc + i, TParaImage.Create(si[0][i]));
      DrawToMap(pi1);
      // 插入末段
      Inc(pi1);
      si1 := 0;
      ci1 := 0;
      if si1 >= FParas[pi1].Count then
      begin
        for i := 0 to sp.Last.Count - 1 do
          FParas[pi1].Insert(si1 + i, SctStrP(sp.Last[i].Sct^, sp.Last[i].Str));
      end
      else
      begin
        for i := 0 to sp.Last.Count - 1 do
          FParas[pi1].Insert(si1 + 1 + i, SctStrP(sp.Last[i].Sct^, sp.Last[i].Str));
      end;
      tc := 0;
      for i := 0 to si1 - 1 do
      begin
        if FParas[pi1][i].Sct.Effect = TCharEffect.Image then
          Inc(tc, FParas[pi1][i].Str.Length);
      end;
      if (si1 < FParas[pi1].Count) and (FParas[pi1][si1].Sct.Effect = TCharEffect.Image) then
        Inc(tc, ci1);
      for i := 0 to si.Last.Count - 1 do
        FParaImages[pi1].Insert(tc + i, TParaImage.Create(si.Last[i]));
      DrawToMap(pi1);
      // 插入中间段
      for i := 1 to sp.Count - 2 do
      begin
        InsertPara(pi1 - 1 + i);
        for j := 0 to sp[i].Count - 1 do
          FParas[pi1 - 1 + i].Add(SctStrP(sp[i][j].Sct^, sp[i][j].Str));
      end;
      for i := 1 to si.Count - 2 do
      begin
        for j := 0 to si[i].Count - 1 do
          FParaImages[pi1 - 1 + i].Add(TParaImage.Create(si[i][j]));
        DrawToMap(pi1 - 1 + i);
      end;
    end;
    for i := 0 to st.Count - 1 do
      Inc(FSelStart, st[i].Length + 1);
    SelStart := FSelStart - 1;
  finally
    FreeAndNil(st);
    FreeAndNil(sf);
    FreeAndNil(sp);
    FreeAndNil(si);
    FreeAndNil(save);
  end;
  Repaint;
end;

procedure TUIPassage.Redo;
begin
  if FCanUndo then
    LoadFromStream(FUndoCon.Redo, True);
end;

procedure TUIPassage.Redraw;
var
  i: Integer;
begin
  for i := 0 to FParas.Count - 1 do
    DrawToMap(i);
  Repaint;
end;

procedure TUIPassage.ReplaceByList(OldLen: Integer; PosList: TList<Integer>; NewStr: string);
var
  i, n, d: Integer;
begin
  d := NewStr.Length - OldLen;
  n := 0;
  for i := 0 to PosList.Count - 1 do
  begin
    if FSelStart > PosList[i] + OldLen then
      Inc(FSelStart, d)
    else if (FSelStart > PosList[i]) and (FSelStart <= PosList[i] + OldLen) then
      FSelStart := PosList[i];
    DeleteString(PosList[i] + n * d, OldLen);
    InsertString(PosList[i] + n * d, NewStr, True);
    Inc(n);
  end;
  SelStart := FSelStart;
end;

procedure TUIPassage.SelChange;
var
  pi, si, ci: Integer;
begin
  FIsPsgUpdating := True;
  try
    SetSelRec;
    GetCurrentPos(FSelStart + FSelLength, pi, si, ci);
    if FParas[pi].Count > 0 then
      FSctUI.CharSct := FParas[pi][si].Sct^;
    FSctUI.ParaSct := FParaSct[pi]^;
    if FSelLength = 0 then
    begin
      if FSelY + FSelH > Height - Padding.Bottom then
        ScrollPos := FScrollPos + FSelY + FSelH - Height + Padding.Bottom
      else if FSelY < 0 then
        ScrollPos := FScrollPos + FSelY;
    end;
    if Assigned(FOnSelChange) then
      FOnSelChange(Self);
  finally
    FIsPsgUpdating := False;
  end;
  if FCanUndo then
  begin
    if not FMouseDn then
      SaveToStream(FUndoCon.RecordInfo, True)
    else
      FUndoWaiting := True;
  end;
end;

function TUIPassage.Select(X, Y: Single): Integer;
var
  i, j, k, pi, si, ci, n: Integer;
begin
  Y := Y + FScrollPos - Padding.Top;
  for i := 0 to FParas.Count - 1 do
  begin
    if (i < FParas.Count - 1) and (Y > FParaHeight[i]) then
    begin
      Y := Y - FParaHeight[i];
      Continue;
    end;
    for j := FLineTop[i].Count - 1 downto 0 do
    begin
      if (Y < FLineTop[i][j]) and (j > 0) then
        Continue;
      // 获取选取文字
      if j >= FCharRight[i].Count then
        Exit(GetSelPos(i, j, 0));
      if ((FCharRight[i][j].Count > 0) and (0.5 * FLineLeft[i][j] + 0.5 * FCharRight[i][j][0] > X))
        or (FLineLeft[i][j] > X) then
        if X > 0 then
        begin
          n := GetSelPos(i, j, 1);
          GetCurrentPos(n, pi, si, ci);
          if FParas[pi][si].Sct.Effect = TCharEffect.Image then
            Exit(n)
          else
            Exit(n - 1);
        end
        else
          Exit(GetSelPos(i, j, 0));
      if X > FCharRight[i][j].Last then
        Exit(GetSelPos(i, j, FCharRight[i][j].Count));
      for k := 1 to FCharRight[i][j].Count - 1 do
      begin
        if X > (0.5 * FCharRight[i][j][k - 1] + 0.5 * FCharRight[i][j][k]) then
          Continue;
        if X > FCharRight[i][j][k - 1] then
        begin
          n := GetSelPos(i, j, k + 1);
          GetCurrentPos(n, pi, si, ci);
          if FParas[pi][si].Sct.Effect = TCharEffect.Image then
            Exit(n)
          else
            Exit(n - 1);
        end;
        Exit(GetSelPos(i, j, k));
      end;
      Exit(GetSelPos(i, j, FCharRight[i][j].Count));
    end;
  end;
  Exit(-1);
end;

procedure TUIPassage.SelectAll;
begin
  SetSelInfo(0, FParas.Count - 1 + GetTextLength);
end;

procedure TUIPassage.SelRecDown;
var
  pi, li, ci: Integer;
begin
  GetLinePos(FSelStart, pi, li, ci);
  Inc(li);
  if li >= FCharRight[pi].Count then
  begin
    Inc(pi);
    if pi >= FParas.Count then
      Exit;
    li := 0;
  end;
  if li < FCharRight[pi].Count then
  begin
    if ci > FCharRight[pi][li].Count then
      ci := FCharRight[pi][li].Count;
    SelStart := GetSelPos(pi, li, ci);
  end
  else
  begin
    SelStart := GetSelPos(pi, 0, 0);
  end;
  Repaint;
end;

procedure TUIPassage.SelRecEnd;
var
  pi, li, ci: Integer;
begin
  GetLinePos(FSelStart, pi, li, ci);
  if li < FCharRight[pi].Count then
    SelStart := GetSelPos(pi, li, FCharRight[pi][li].Count);
end;

procedure TUIPassage.SelRecHome;
var
  pi, li, ci: Integer;
begin
  GetLinePos(FSelStart, pi, li, ci);
  if li < FCharRight[pi].Count then
    SelStart := GetSelPos(pi, li, 0);
end;

procedure TUIPassage.SelRecLeft;
begin
  SelStart := FSelStart - 1;
  Repaint;
end;

procedure TUIPassage.SelRecRight;
begin
  SelStart := FSelStart + 1;
  Repaint;
end;

procedure TUIPassage.SelRecUp;
var
  pi, li, ci: Integer;
begin
  GetLinePos(FSelStart, pi, li, ci);
  Dec(li);
  if li < 0 then
  begin
    Dec(pi);
    if pi < 0 then
      Exit;
    li := FCharRight[pi].Count - 1;
  end;
  if li >= 0 then
  begin
    if ci > FCharRight[pi][li].Count then
      ci := FCharRight[pi][li].Count;
    SelStart := GetSelPos(pi, li, ci);
  end
  else
  begin
    SelStart := GetSelPos(pi, 0, 0);
  end;
  Repaint;
end;

procedure TUIPassage.SetBackground(AFileName: string);
begin
  Fill.Kind := TBrushKind.Bitmap;
  if Assigned(FBackground) then
    FBackground.Free;
  FBackground := TFileStream.Create(AFileName, fmOpenRead);
  Fill.Bitmap.Bitmap.LoadFromStream(FBackground);
end;

procedure TUIPassage.SetColor(const Value: TAlphaColor);
begin
  Fill.Color := Value;
end;

procedure TUIPassage.SetHeight(const Value: Single);
begin
  inherited;
  Paint;
end;

procedure TUIPassage.SetMouseCon(const Value: TMouseInfos);
var
  i: Integer;
begin
  if Value <> FMouseCon then
  begin
    FMouseCon := Value;
    for i := 0 to FParas.Count - 1 do
      DrawToMap(i);
  end;
end;

procedure TUIPassage.SetScrollPos(const Value: Single);
var
  tg, tt: Single;
begin
  if TMouseInfo.Moving in FMouseCon then
    tt := 5
  else
    tt := 0;
  tg := Padding.Top + Padding.Bottom + tt;
  if Value <= 0 then
    FScrollPos := 0
  else if FPassHeight + tg <= Height then
    FScrollPos := 0
  else if Value + Height - tg <= FPassHeight then
    FScrollPos := Value
  else
    FScrollPos := FPassHeight - Height + tg;
  SetSelRec;
  Repaint;
end;

procedure TUIPassage.SetSelection(const P1, P2: Integer);
begin
  if (P1 < 0) or (P2 < 0) then
    Exit;
  if P1 < P2 then
    SetSelInfo(P1, P2 - P1)
  else if P1 = P2 then
    SetSelInfo(P1, 0)
  else
    SetSelInfo(P2, P1 - P2);
end;

procedure TUIPassage.SetSelInfo(ASelStart, ASelLength: Integer);
var
  pi1, pi2, si1, si2, ci1, ci2, i, j: Integer;
begin
  if (ASelStart < 0) or (ASelStart + ASelLength < 0) then
    Exit;
  if (ASelLength <> FSelLength) or (ASelStart <> FSelStart) then
  begin
    SelStart := ASelStart;
    FSelLength := ASelLength;
    if ASelLength > 0 then
    begin
      GetCurrentPos(Min(FSelStart, FSelStart + FSelLength), pi1, si1, ci1);
      GetCurrentPos(Max(FSelStart, FSelStart + FSelLength), pi2, si2, ci2);
      if si1 < FParas[pi1].Count then
        FSctUI.FStyle := FParas[pi1][si1].Sct.Style;
      for i := si1 + 1 to FParas[pi1].Count - 1 do
        FSctUI.FStyle := FSctUI.FStyle * FParas[pi1][i].Sct.Style;
      for i := pi1 + 1 to pi2 - 1 do
        for j := 0 to FParas[i].Count - 1 do
          FSctUI.FStyle := FSctUI.FStyle * FParas[i][j].Sct.Style;
      for i := 0 to si2 do
      begin
        if i >= FParas[pi2].Count then
          Break;
        FSctUI.FStyle := FSctUI.FStyle * FParas[pi2][i].Sct.Style;
      end;
    end;
    SelChange;
    Repaint;
  end;
end;

procedure TUIPassage.SetSelLength(const Value: Integer);
begin
  if Value = FSelLength then
    Exit;
  if Value >= 0 then
    FSelLength := Value
  else
    FSelLength := 0;
end;

procedure TUIPassage.SetSelRec;
var
  pi, li, ci, i: Integer;
  tt, X, Y: Single;
begin
  if FInsertPos > -1 then
    GetLinePos(FInsertPos, pi, li, ci)
  else
    GetLinePos(FSelStart, pi, li, ci);
  tt := 0;
  for i := 0 to pi - 1 do
    tt := tt + FParaHeight[i];
  if ci > 0 then
    X := FCharRight[pi][li][ci - 1]
  else
    X := FLineLeft[pi][li];
  if TMouseInfo.Moving in FMouseCon then
    tt := tt + 5;
  Y := tt + FLineTop[pi][li] - FScrollPos + Padding.Top;
  if li = FLineTop[pi].Count - 1 then
    tt := FParaEnd[pi] - FLineTop[pi][li]
  else
    tt := FLineTop[pi][li + 1] - FLineTop[pi][li];
  FSelX := X;
  FSelY := Y;
  FSelH := tt;
end;

procedure TUIPassage.SetSelStart(const Value: Integer);
var
  m, i, tc, pi, si, ci: Integer;
  ch: Boolean;
begin
  m := FParas.Count - 1 + GetTextLength;
  if FSelStart = Value then
    ch := False
  else
    ch := True;
  FSelImage := False;
  if (Value >= 0) and (Value <= m) then
  begin
    if FSelLength <> 0 then
      SelLength := 0;
    FSelStart := Value;
  end
  else
    Exit;
  if FSelLength = 0 then
  begin
    GetCurrentPos(FSelStart, pi, si, ci);
    if (si < FParas[pi].Count) and (FParas[pi][si].Sct.Effect = TCharEffect.Image) and (ci > 0) then
    begin
      FSelImage := True;
      tc := 0;
      for i := 0 to si - 1 do
      begin
        if FParas[pi][i].Sct.Effect = TCharEffect.Image then
          Inc(tc, FParas[pi][i].Str.Length);
      end;
      if (si < FParas[pi].Count) and (FParas[pi][si].Sct.Effect = TCharEffect.Image) then
        Inc(tc, ci - 1);
      FImageIndex := tc;
      ch := True;
    end;
  end;
  FDrawCursor := True;
  if ch then
    SelChange;
  SetSelRec;
  Repaint;
end;

procedure TUIPassage.SetSelText(const Value: string);
begin
  InsertStr(Value, True);
end;

procedure TUIPassage.SetShowSides(const Value: Boolean);
begin
  FShowSides := Value;
  Repaint;
end;

procedure TUIPassage.SetSidesColor(const Value: TAlphaColor);
begin
  FSidesColor := Value;
  Repaint;
end;

procedure TUIPassage.SetSidesWidth(const Value: Single);
begin
  if Value < 0 then
    FSidesWidth := 0
  else if FSidesWidth > 10 then
    FSidesWidth := 10
  else
    FSidesWidth := Value;
  Repaint;
end;

procedure TUIPassage.SetWidth(const Value: Single);
begin
  inherited;
  Redraw;
end;

procedure TUIPassage.Undo;
begin
  if FCanUndo then
    LoadFromStream(FUndoCon.Undo, True);
end;

procedure TUIPassage.SaveToFile(FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TUIPassage.SaveToHTML(FileName: string);
var
  s, fn, dn: string;
  i, j, ii: Integer;
  fi, li, ri, sb, sa, ls, bs: Single;
  ht: TStringList;
begin
  ht := TStringList.Create;
  try
    fn := GetFileName(FileName);
    dn := GetDirectory(FileName) + PathDelim + fn.Replace('.', '_') + '_Data' + PathDelim;
    TDirectory.CreateDirectory(dn);
    for i := 0 to FParas.Count - 1 do
    begin
      if FParas[i].Count > 0 then
      begin
        // 设置基准字符尺寸（即为第一个字符尺寸）
        bs := FParas[i][0].Sct.Size;
        // 段落属性
        fi := FParaSct[i].FirstIndent * bs;
        li := FParaSct[i].LeftOffset * bs;
        if li > fi then
          li :=li - fi
        else
          li := 0;
        ri := FParaSct[i].RightOffset * bs;
        sb := FParaSct[i].SpaceBefore * bs;
        sa := FParaSct[i].SpaceAfter * bs;
        ls := FParaSct[i].LineSpace * 2 + 2;
        s := Format('<p style = "text-indent: %fpx; padding: %fpx %fpx %fpx %fpx; line-height: %fem; ',
          [fi, sb, ri, sa, li, ls]);
        case FParaSct[i].Alignment of
          TParaAlign.Left:
            s := s + 'text-align: left">';
          TParaAlign.Center:
            s := s + 'text-align: center">';
          TParaAlign.Right:
            s := s + 'text-align: right">';
          TParaAlign.Justify, TParaAlign.Stretch:
            s := s + 'text-align: Justify">';
        end;
        ht.Add(s);
        ii := 0;
        s := '';
        // 依次输入文字
        for j := 0 to FParas[i].Count - 1 do
        begin
          if FParas[i][j].Sct.Effect = TCharEffect.Image then
          begin
            FParaImages[i][ii].FImage.SaveToFile(Format('%s%d.png', [dn, ii]));
            s := s + Format('<img src = "%s%d.png" width = %f/>', [fn.Replace('.', '_') + '_Data/',
              ii, FParaImages[i][ii].Width]);
            Inc(ii);
          end
          else
          begin
            // 字体和颜色
            s := s + Format('<font style = "font-size: %fpx; background-color: #%s" face = "%s"; color = "#%s">',
              [FParas[i][j].Sct.Size, IntToHex(FParas[i][j].Sct.BgColor mod $1000000, 6),
              FFonts[FParas[i][j].Sct.Font], IntToHex(FParas[i][j].Sct.Color mod $1000000, 6)]);
            // 偏移损失为角标
            if FParas[i][j].Sct.Offset > 0 then
              s := s + '<sup>'
            else if FParas[i][j].Sct.Offset < 0 then
              s := s + '<sub>';
            // 显示风格
            if TFontStyle.fsBold in FParas[i][j].Sct.Style then
              s := s + '<b>';
            if TFontStyle.fsItalic in FParas[i][j].Sct.Style then
              s := s + '<i>';
            if TFontStyle.fsUnderline in FParas[i][j].Sct.Style then
              s := s + '<u>';
            if TFontStyle.fsStrikeOut in FParas[i][j].Sct.Style then
              s := s + '<s>';
            // 插入文字
            s := s + FParas[i][j].Str;
            // 格式末
            if TFontStyle.fsBold in FParas[i][j].Sct.Style then
              s := s + '</b>';
            if TFontStyle.fsItalic in FParas[i][j].Sct.Style then
              s := s + '</i>';
            if TFontStyle.fsUnderline in FParas[i][j].Sct.Style then
              s := s + '</u>';
            if TFontStyle.fsStrikeOut in FParas[i][j].Sct.Style then
              s := s + '</s>';
            // 偏移损失为角标
            if FParas[i][j].Sct.Offset > 0 then
              s := s + '</sup>'
            else if FParas[i][j].Sct.Offset < 0 then
              s := s + '</sub>';
            s := s + '</font>';
          end;
        end;
        ht.Add(s);
        ht.Add('</p>');
      end
      else // 绘制空段落
      begin
        ht.Add('<p>　</p>');
      end;
    end;
    ht.SaveToFile(FileName);
  finally
    FreeAndNil(ht);
  end;
end;

procedure TUIPassage.SaveToRTF(FileName: string);
var
  rtf: TStringList;
  fs, ts: string;
  i, j, k, ii: Integer;
  cl: TList<Cardinal>;
  clr, bgc: Integer;
  color: Cardinal;
  temp: TMemoryStream;
begin
  rtf := TStringList.Create;
  cl := TList<Cardinal>.Create;
  temp := TMemoryStream.Create;
  try
    // 基本属性
    rtf.Add('{\rtf1\ansi\ansicpg936\deff0{\fonttbl');
    // 字体列表
    fs := '';
    for i := 0 to FFonts.Count - 1 do
      fs := fs + Format('{\f%d\fnil\fcharset134 %s;}', [i, StrToRTFCode(FFonts[i])]);
    fs := fs + '}';
    rtf.Add(fs);
    // 颜色列表
    fs := '{\colortbl ;';
    // 遍历颜色
    cl.Add($FF000000);
    for i := 0 to FParas.Count - 1 do
      for j := 0 to FParas[i].Count - 1 do
      begin
        color := FParas[i][j].Sct.Color;
        for k := 0 to cl.Count do
        begin
          if k = cl.Count then
          begin
            cl.Add(color);
            Break;
          end;
          if color = cl[k] then
            Break;
        end;
        color := FParas[i][j].Sct.BgColor;
        for k := 0 to cl.Count do
        begin
          if k = cl.Count then
          begin
            cl.Add(color);
            Break;
          end;
          if color = cl[k] then
            Break;
        end;
      end;
    for i := 1 to cl.Count - 1 do
      fs := fs + ColorToRTFCode(cl[i]);
    fs := fs + '}';
    rtf.Add(fs);
    // 生成器和默认信息
    rtf.Add('{\*\generator ELT 1.13}\viewkind4\uc1');
    for i := 0 to FParas.Count - 1 do with FParaSct[i]^ do
    begin
      // 段落属性
      fs := '\pard';
      fs := fs + Format('\cufi%d\culi%d\curi%d\lisb%d\lisa%d', [Trunc((FirstIndent - LeftOffset) * 100),
        Trunc(LeftOffset * 100), Trunc(RightOffset * 100), Trunc(SpaceBefore * 100), Trunc(SpaceAfter * 100)]);
        // 损失行后间距
      case Alignment of
        TParaAlign.Left: fs := fs + '\ql';
        TParaAlign.Center: fs := fs + '\qc';
        TParaAlign.Right: fs := fs + '\qr';
        TParaAlign.Justify: fs := fs + '\qj';
        TParaAlign.Stretch: fs := fs + '\qd';
      end;
      rtf.Add(fs);
      // 依次输入文字
      ii := 0;
      fs := '';
      for j := 0 to FParas[i].Count - 1 do
      begin
        if FParas[i][j].Sct.Effect = TCharEffect.Image then
        begin
          temp.Clear;
          FParaImages[i][ii].FImage.SaveToStream(temp);
          temp.Position := 0;
          ts := StreamToRTFCode(temp);
          with FParaImages[i][ii] do
            fs := Format('{\pict{\*\picprop}\wmetafile8\picw%d\pich%d\picwgoal%d\pichgoal%d %s}',
              [Trunc(FImage.Width * 635 / 24), Trunc(FImage.Height * 635 / 24), Trunc(Width * 15),
              Trunc(Height * 15), ts]);
          rtf.Add(fs);
          Inc(ii);
        end
        else
        begin
          // 获取颜色索引
          clr := 0;
          for k := 0 to cl.Count - 1 do
            if FParas[i][j].Sct.Color = cl[k] then
            begin
              clr := k;
              Break;
            end;
          bgc := 0;
          for k := 0 to cl.Count - 1 do
            if FParas[i][j].Sct.BgColor = cl[k] then
            begin
              bgc := k;
              Break;
            end;
          fs := Format('\f%d\fs%d\cf%d\chcbpat%d', [FParas[i][j].Sct.Font, Trunc(FParas[i][j].Sct.Size * 2), clr, bgc]);
          if FParas[i][j].Sct.Offset > 0 then
            fs := fs + Format('\up%d', [Trunc(FParas[i][j].Sct.Offset)])
          else if FParas[i][j].Sct.Offset < 0 then
            fs := fs + Format('\dn%d', [Trunc(- FParas[i][j].Sct.Offset)])
          else
            fs := fs + '\nosupersub';
          if TFontStyle.fsBold in FParas[i][j].Sct.Style then
            fs := fs + '\b'
          else
            fs := fs + '\b0';
          if TFontStyle.fsItalic in FParas[i][j].Sct.Style then
            fs := fs + '\i'
          else
            fs := fs + '\i0';
          if TFontStyle.fsUnderline in FParas[i][j].Sct.Style then
            fs := fs + '\ul'
          else
            fs := fs + '\ulnone';
          if TFontStyle.fsStrikeOut in FParas[i][j].Sct.Style then
            fs := fs + '\strike'
          else
            fs := fs + '\strike0';
          fs := fs + Format(' %s', [StrToRTFCode(FParas[i][j].Str)]);
          rtf.Add(fs);
        end;
      end;
      rtf.Add('\par');
    end;
    rtf.Add('}');
    rtf.SaveToFile(FileName);
  finally
    FreeAndNil(rtf);
    FreeAndNil(cl);
    FreeAndNil(temp);
  end;
end;

procedure TUIPassage.SaveToStream(Stream: TStream; Buffer: Boolean);
var
  c, d, e, i, j: Integer;
  b: Boolean;
  ts: Single;
  ac: Cardinal;
  temp: TMemoryStream;
  bs: TBytes;
  fs: TStringList;
  af: string;
begin
  // 初始化
  bs := WideBytesOf('ELT_P');
  e := Length(bs);
  Stream.Write(e, SizeOf(Integer));
  Stream.WriteBuffer(bs, e);
  // 清理字体列表
  fs := TStringList.Create;
  try
    for i := 0 to FParas.Count - 1 do
      for j := 0 to FParas[i].Count - 1 do
      begin
        af := FFonts[FParas[i][j].Sct.Font];
        for e := 0 to fs.Count do
          if e = fs.Count then
          begin
            fs.Add(af);
            FParas[i][j].Sct.Font := e;
          end
          else if af = fs[e] then
          begin
            FParas[i][j].Sct.Font := e;
            Break;
          end;
      end;
    af := FSctUI.Font;
    for e := 0 to fs.Count do
      if e = fs.Count then
      begin
        fs.Add(af);
        FSctUI.FFont := e;
      end
      else if af = fs[e] then
      begin
        FSctUI.FFont := e;
        Break;
      end;
    FFonts.Assign(fs);
  finally
    FreeAndNil(fs);
  end;
  // 显示背景
  if Fill.Kind <> TBrushKind.None then
    b := True
  else
    b := False;
  Stream.Write(b, SizeOf(Boolean));
  // 图像背景
  if b then
  begin
    if Fill.Kind = TbrushKind.Bitmap then
      b := True
    else
      b := False;
    Stream.Write(b, SizeOf(Boolean));
    if b then
    begin
      temp := TMemoryStream.Create;
      try
        if Assigned(FBackground) then
        begin
          FBackground.Position := 0;
          temp.LoadFromStream(FBackground);
        end
        else
          Fill.Bitmap.Bitmap.SaveToStream(temp);
        temp.Position := 0;
        c := temp.Size;
        Stream.Write(c, SizeOf(Integer));
        Stream.WriteBuffer(temp.Memory^, c);
      finally
        FreeAndNil(temp);
      end;
    end
    else
    begin
      ac := Fill.Color;
      Stream.Write(ac, SizeOf(Cardinal));
    end;
  end;
  // 鼠标控制信息
  if TMouseInfo.Moving in MouseCon then
    b := True
  else
    b := False;
  Stream.Write(b, SizeOf(Boolean));
  if TMouseInfo.Sizing in MouseCon then
    b := True
  else
    b := False;
  Stream.Write(b, SizeOf(Boolean));
  if TMouseInfo.Wheeling in MouseCon then
    b := True
  else
    b := False;
  Stream.Write(b, SizeOf(Boolean));
  // 边距
  ts := Padding.Top;
  Stream.Write(ts, SizeOf(Single));
  ts := Padding.Bottom;
  Stream.Write(ts, SizeOf(Single));
  ts := Padding.Left;
  Stream.Write(ts, SizeOf(Single));
  ts := Padding.Right;
  Stream.Write(ts, SizeOf(Single));
  // 储存段落结构
  c := FParaSct.Count;
  Stream.Write(c, SizeOf(Integer));
  for i := 0 to c - 1 do
  begin
    Stream.Write(FParaSct[i].FirstIndent, SizeOf(Single));
    Stream.Write(FParaSct[i].LeftOffset, SizeOf(Single));
    Stream.Write(FParaSct[i].RightOffset, SizeOf(Single));
    Stream.Write(FParaSct[i].SpaceBefore, SizeOf(Single));
    Stream.Write(FParaSct[i].SpaceAfter, SizeOf(Single));
    Stream.Write(FParaSct[i].LineSpace, SizeOf(Single));
    Stream.Write(FParaSct[i].Alignment, SizeOf(TParaAlign));
  end;
  // 储存文字信息
  c := FParas.Count;
  Stream.Write(c, SizeOf(Integer));
  for i := 0 to c - 1 do
  begin
    d := FParas[i].Count;
    Stream.Write(d, SizeOf(Integer));
    for j := 0 to d - 1 do
    begin
      // 文字结构
      Stream.Write(FParas[i][j].Sct.Font, SizeOf(Integer));
      Stream.Write(FParas[i][j].Sct.Size, SizeOf(Single));
      Stream.Write(FParas[i][j].Sct.Opacity, SizeOf(Single));
      Stream.Write(FParas[i][j].Sct.BgOpacity, SizeOf(Single));
      Stream.Write(FParas[i][j].Sct.Offset, SizeOf(Single));
      Stream.Write(FParas[i][j].Sct.Color, SizeOf(Cardinal));
      Stream.Write(FParas[i][j].Sct.BgColor, SizeOf(Cardinal));
      e := 0;
      if TFontStyle.fsBold in FParas[i][j].Sct.Style then
        Inc(e);
      if TFontStyle.fsItalic in FParas[i][j].Sct.Style then
        Inc(e, 2);
      if TFontStyle.fsUnderline in FParas[i][j].Sct.Style then
        Inc(e, 4);
      if TFontStyle.fsStrikeOut in FParas[i][j].Sct.Style then
        Inc(e, 8);
      Stream.Write(e, SizeOf(Integer));
      Stream.Write(FParas[i][j].Sct.Effect, SizeOf(TCharEffect));
      // 文字
      bs := WideBytesOf(FParas[i][j].Str);
      e := Length(bs);
      Stream.Write(e, SizeOf(Integer));
      Stream.WriteBuffer(bs, e);
    end;
  end;
  temp := TMemoryStream.Create;
  try
    // 储存图像信息
    c := FParaImages.Count;
    Stream.Write(c, SizeOf(Integer));
    for i := 0 to c - 1 do
    begin
      d := FParaImages[i].Count;
      Stream.Write(d, SizeOf(Integer));
      for j := 0 to d - 1 do
      begin
        FParaImages[i][j].FImage.SaveToStream(temp);
        e := temp.Size;
        temp.Position := 0;
        Stream.Write(e, SizeOf(Integer));
        Stream.WriteBuffer(temp.Memory^, e);
        temp.Clear;
        Stream.Write(FParaImages[i][j].FWidth, SizeOf(Single));
        Stream.Write(FParaImages[i][j].FHeight, SizeOf(Single));
      end;
    end;
    // 储存字体列表
    FFonts.SaveToStream(temp);
    e := temp.Size;
    temp.Position := 0;
    Stream.Write(e, SizeOf(Integer));
    Stream.WriteBuffer(temp.Memory^, e);
  finally
    FreeAndNil(temp);
  end;
  // 缓存信息信息
  if Buffer then
  begin
    ts := FScrollPos;
    Stream.Write(ts, SizeOf(Single));
    c := FSelStart;
    Stream.Write(c, SizeOf(Integer));
  end;
end;

{ TMouseCon }

constructor TMouseCon.Create(AOwner: TComponent);
begin
  inherited;
  Width := 100;
  Height := 100;
  Fill.Kind := TBrushKind.None;
  Stroke.Kind := TBrushKind.None;
end;

procedure TMouseCon.DealingMouse;
begin
  if (FLink is TUIPassage) and (FInfo = TMouseInfo.Sizing) then
    with TUIPassage(FLink) do
    begin
      Redraw;
      FManager.Repaint;
    end;
  Free;
end;

procedure TMouseCon.Initiate(Link: TControl; X, Y: Single; Info: TMouseInfo);
begin
  Parent := Link.Parent;
  Position.Point := PointF(X - 0.5 * Width, Y - 0.5 * Height);
  FLink := Link;
  FDnX := 0.5 * Width + Position.X;
  FDnY := 0.5 * Height + Position.Y;
  FInfo := Info;
  case Info of
    TMouseInfo.Moving: Cursor := crSizeAll;
    TMouseInfo.Sizing: Cursor := crSizeNWSE;
    TMouseInfo.Wheeling: Cursor := crArrow;
  end;
end;

procedure TMouseCon.MouseMove(Shift: TShiftState; X, Y: Single);
var
  dx, dy, tt, tg: Single;
begin
  if FLink = nil then
    Exit;
  X := X + Position.X;
  Y := Y + Position.Y;
  dx := X - FDnX;
  dy := Y - FDnY;
  FDnX := X;
  FDnY := Y;
  Position.Point := PointF(Position.X + dx, Position.Y + dy);
  case FInfo of
    TMouseInfo.Moving:
      FLink.Position.Point := PointF(FLink.Position.X + dx, FLink.Position.Y + dy);
    TMouseInfo.Sizing:
      begin
        FLink.Size.Size := TSizeF.Create(FLink.Width + dx, Flink.Height + dy);
        if Flink is TUIPassage then with TUIPassage(FLink) do
        begin
          Repaint;
          FManager.Repaint;
        end;
      end;
    TMouseInfo.Wheeling:
      if FLink is TUIPassage then with TUIPassage(FLink) do
      begin
        if TMouseInfo.Moving in FMouseCon then
          tt := 5
        else
          tt := 0;
        tg := Padding.Top + Padding.Bottom + tt;
        ScrollPos := (Y - Position.Y - 10) * (FPassHeight + tg - Height) / (Height - 20);
      end;
  end;
  inherited;
end;

procedure TMouseCon.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  DealingMouse;
  inherited;
end;

{ TUIManager }

procedure TUIManager.ApplyCamera(Index: Integer);
begin
  if (Index < 0) or (Index >= FCameraList.Count) then
    Exit;
  FCameraList[Index].CameraPoint := PointF(0.5 * Width, 0.5 * Height);
end;

procedure TUIManager.Clear;
begin
  FPassages.Clear;
  FCameraList.Clear;
  FCamNameList.Clear;
  Fill.Kind := TBrushKind.None;
  DrawMiniMap;
end;

procedure TUIManager.ClearCamera;
begin
  FCameraList.Clear;
  FCamNameList.Clear;
end;

constructor TUIManager.Create(AOwner: TComponent);
begin
  inherited;
  FPassages := TObjectList<TUIPassage>.Create;
  FCameraList := TObjectList<TCameraUI>.Create;
  FCamNameList := TStringList.Create;
  FActPass := nil;
  FInfo := TManagerInfo.Move;
  FDnX := 0;
  FDnY := 0;
  FCrX := 0;
  FCrY := 0;
  FMouseDn := False;
  FMouseLR := True;
  FMiniMap := TBitmap.Create;
  FMiniMap.Width := 300;
  FMiniMap.Height := 300;
  FIsChangingCamera := False;
  CanFocus := True;
  FIntervalBase := 0.25;
  FIntervalPlus := 0.05;
end;

procedure TUIManager.DoApplyCamera;
begin
  DrawMiniMap;
  if FCanUndo then
    SaveToStream(FUndoCon.RecordInfo);
  FIsChangingCamera := False;
  if Assigned(FOnApplyCamera) then
    FOnApplyCamera(Self);
end;

procedure TUIManager.DoCreatePassage(Pass: TUIPassage);
begin
  DrawMiniMap;
  if Assigned(FOnCreatePass) then
    FOnCreatePass(Self, Pass);
end;

procedure TUIManager.DoDragCamera;
begin
  DrawMiniMap;
  if FCanUndo then
    SaveToStream(FUndoCon.RecordInfo);
  FIsChangingCamera := False;
  if Assigned(FOnDragCamera) then
    FOnDragCamera(Self);
end;

procedure TUIManager.DoDrawMiniMap;
begin
  if Assigned(FOnDrawMiniMap) then
    FOnDrawMiniMap(Self);
end;

procedure TUIManager.DoMouseLeave;
begin
  inherited;
  case FInfo of
    TManagerInfo.Create:
      if (FCrX > 5) and (FCrX < Width - 5) and (FCrY > 5) and (FCrY < Height - 5) then
        Exit;
  end;
  if FMouseDn and FMouseLR then
  case FInfo of
    TManagerInfo.Create: MouseUp(TMouseButton.mbLeft, [ssCtrl], FCrX, FCrY);
    TManagerInfo.Move: MouseUp(TMouseButton.mbLeft, [], FDnX, FDnY);
  end
  else if FMouseDn and not FMouseLR then
    MouseUp(TMouseButton.mbRight, [], FDnX, FDnY);
end;

procedure TUIManager.DoRemovePassage;
begin
  DrawMiniMap;
  if Assigned(FOnRemovePass) then
    FOnRemovePass(Self);
end;

procedure TUIManager.CreatePassage;
var
  x, y, w, h: Single;
  pa: TUIPassage;
begin
  if FDnX < FCrX then
  begin
    x := FDnX;
    w := FCrX - FDnX;
  end
  else if FDnX > FCrX then
  begin
    x := FCrX;
    w := FDnX - FCrX;
  end
  else
  begin
    x := FDnX;
    w := 100;
  end;
  if FDnY < FCrY then
  begin
    y := FDnY;
    h := FCrY - FDnY;
  end
  else if FDnY > FCrY then
  begin
    y := FCrY;
    h := FDnY - FCrY;
  end
  else
  begin
    y := FDnY;
    h := 100;
  end;
  if w < 100 then
    w := 100;
  if h < 100 then
    h := 100;
  pa := TUIPassage.Create(Owner);
  pa.Parent := Self;
  pa.FManager := Self;
  FPassages.Add(pa);
  pa.Position.Point := PointF(x, y);
  pa.Size.Width := w;
  pa.Size.Height := h;
  if FCanUndo then
    SaveToStream(FUndoCon.RecordInfo);
  DoCreatePassage(pa);
end;

procedure TUIManager.DeleteCamera(Index: Integer);
begin
  FCameraList.Delete(Index);
  FCamNameList.Delete(Index);
end;

destructor TUIManager.Destroy;
begin
  Clear;
  FreeAndNil(FPassages);
  FreeAndNil(FCameraList);
  FreeAndNil(FCamNameList);
  FreeAndNil(FMiniMap);
  inherited;
end;

function TUIManager.DisableUndo: Boolean;
begin
  if not Assigned(FUndoCon) then
    Exit(False);
  FCanUndo := False;
  FreeAndNil(FUndoCon);
  Exit(True);
end;

procedure TUIManager.DrawMiniMap;
var
  pt, pb, pl, pr, he, wi, x, y, ra: Single;
  bb: TBrush;
  sb: TStrokeBrush;
  i: Integer;
begin
  FMiniMap.Clear($00FFFFFF);
  // 获取边界坐标
  pt := 0;
  pb := Height;
  pl := 0;
  pr := Width;
  for i := 0 to FPassages.Count - 1 do
  begin
    if FPassages[i].Position.Y < pt then
      pt := FPassages[i].Position.Y;
    if FPassages[i].Position.Y + FPassages[i].Height > pb then
      pb := FPassages[i].Position.Y + FPassages[i].Height;
    if FPassages[i].Position.X < pl then
      pl := FPassages[i].Position.X;
    if FPassages[i].Position.X + FPassages[i].Width > pr then
      pr := FPassages[i].Position.X + FPassages[i].Width;
  end;
  // 确定绘图原点，缩放比例
  he := pb - pt;
  wi := pr - pl;
  if he > wi then
  begin
    x := 150 * (1 - wi / he);
    y := 0;
    ra := 300 / he;
  end
  else if he = wi then
  begin
    x := 0;
    y := 0;
    ra := 300 / he;
  end
  else
  begin
    x := 0;
    y := 150 * (1 - he / wi);
    ra := 300 / wi;
  end;
  bb := TBrush.Create(TBrushKind.Solid, TAlphaColors.Grey);
  sb := TStrokeBrush.Create(TBrushKind.Solid, TAlphaColors.White);
  if FMiniMap.Canvas.BeginScene then
  try
    // 绘制文本框
    for i := 0 to FPassages.Count - 1 do
    begin
      FMiniMap.Canvas.FillRect(RectF(x + (FPassages[i].Position.X - pl) * ra,
        y + (FPassages[i].Position.Y - pt) * ra,
        x + (FPassages[i].Position.X + FPassages[i].Width - pl) * ra,
        y + (FPassages[i].Position.Y + FPassages[i].Height - pt) * ra), 0, 0, [], 1, bb);
    end;
    // 绘制镜头
    FMiniMap.Canvas.DrawRect(RectF(x - pl * ra, y - pt * ra, x + (Width - pl) * ra, y + (Height - pt) * ra),
      0, 0, [], 1, sb);
  finally
    FMiniMap.Canvas.EndScene;
    FreeAndNil(bb);
  end;
  DoDrawMiniMap;
end;

function TUIManager.EnableUndo(Count: Integer): Boolean;
begin
  if Assigned(FUndoCon) then
    Exit(False);
  FUndoCon := TUndoCon.Create(Count);
  FCanUndo := True;
  SaveToStream(FUndoCon.RecordInfo);
  Exit(True);
end;

function TUIManager.GetCameraNames: string;
begin
  Exit(FCamNameList.Text);
end;

function TUIManager.GetCamName(Index: Integer): string;
begin
  if (Index < FCamNameList.Count) and (Index >= 0) then
    Exit(FCamNameList[Index]);
end;

function TUIManager.GetCamNameList: string;
begin
  Exit(FCamNameList.Text);
end;

function TUIManager.GetColor: TAlphaColor;
begin
  Exit(Fill.Color);
end;

function TUIManager.GetCurCamera: Integer;
var
  i: Integer;
begin
  if FCameraList.Count = 0 then
    Exit(-1);
  for i := 0 to FCameraList.Count - 1 do
  begin
    if (FCameraList[i].FX = 0.5 * Width) and (FCameraList[i].FY = 0.5 * Height) then
      Exit(i);
  end;
  Exit(-1);
end;

function TUIManager.GetCurRect: TRectF;
var
  x1, x2, y1, y2: Single;
begin
  if FDnX < FCrX then
  begin
    x1 := FDnX;
    x2 := FCrX;
  end
  else
  begin
    x1 := FCrX;
    x2 := FDnX;
  end;
  if FDnY < FCrY then
  begin
    y1 := FDnY;
    y2 := FCrY;
  end
  else
  begin
    y1 := FCrY;
    y2 := FDnY;
  end;
  Exit(RectF(x1, y1, x2,y2));
end;

procedure TUIManager.LoadFromFile(FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TUIManager.LoadFromStream(Stream: TStream);
var
  bs: TBytes;
  i, c, d: Integer;
  b: Boolean;
  ac: Cardinal;
  ts, tt: Single;
  temp: TMemoryStream;
begin
  if not Assigned(Stream) then
    Exit;
  // 初始化
  Stream.Read(c, SizeOf(Integer));
  SetLength(bs, c);
  Stream.ReadBuffer(bs, c);
  if WideStringOf(bs) <> 'ELT_M' then
    Exit;
  Clear;
  // 读取镜头
  Stream.Read(c, SizeOf(Integer));
  for i := 0 to c - 1 do
  begin
    Stream.Read(ts, SizeOf(Single));
    Stream.Read(tt, SizeOf(Single));
    FCameraList.Add(TCameraUI.Create(ts, tt, Self));
  end;
  Stream.Read(c, SizeOf(Integer));
  SetLength(bs, c);
  Stream.ReadBuffer(bs, c);
  FCamNameList.Text := WideStringOf(bs);
  // 读取背景
  Stream.Read(b, SizeOf(Boolean));
  if b then
  begin
    Stream.Read(b, SizeOf(Boolean));
    if b then
    begin
      temp := TMemoryStream.Create;
      try
        Stream.Read(c, SizeOf(Integer));
        temp.SetSize(c);
        Stream.ReadBuffer(temp.Memory^, c);
        temp.Position := 0;
        Fill.Kind := TBrushKind.Bitmap;
        Fill.Bitmap.Bitmap.LoadFromStream(temp);
        temp.Position := 0;
        if Assigned(FBackground) then
          FBackground.Free;
        FBackground := TMemoryStream.Create;
        temp.SaveToStream(FBackground);
      finally
        FreeAndNil(temp);
      end;
    end
    else
    begin
      Stream.Read(ac, SizeOf(Cardinal));
      Fill.Kind := TBrushKind.Solid;
      Fill.Color := ac;
    end;
  end
  else
    Fill.Kind := TBrushKind.None;
  // 对文档循环
  temp := TMemoryStream.Create;
  try
    Stream.Read(d, SizeOf(Integer));
    for i := 0 to d - 1 do
    begin
      FPassages.Add(TUIPassage.Create(Owner));
      DoCreatePassage(FPassages.Last);
      FPassages.Last.Parent := Self;
      FPassages.Last.Manager := Self;
      // 位置和尺寸
      Stream.Read(ts, SizeOf(Single));
      FPassages[i].Position.X := ts;
      Stream.Read(ts, SizeOf(Single));
      FPassages[i].Position.Y := ts;
      Stream.Read(ts, SizeOf(Single));
      FPassages[i].Width := ts;
      Stream.Read(ts, SizeOf(Single));
      FPassages[i].Height := ts;
      // 内容
      temp.Clear;
      Stream.Read(c, SizeOf(Integer));
      temp.SetSize(c);
      Stream.ReadBuffer(temp.Memory^, c);
      temp.Position := 0;
      FPassages[i].LoadFromStream(temp);
    end;
  finally
    FreeAndNil(temp);
  end;
  // 边框信息
  Stream.Read(b, SizeOf(Boolean));
  FShowSides := b;
  Stream.Read(ts, SizeOf(Single));
  FSidesWidth := ts;
  Stream.Read(ac, SizeOf(Cardinal));
  FSidesColor := ac;
  DrawMiniMap;
end;

procedure TUIManager.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if FReadOnly then
    Exit;
  FMouseDn := True;
  case Button of
    TMouseButton.mbLeft:
      begin
        FMouseLR := True;
        if FActPass <> nil then
        begin
          FActPass.DoExit;
          FActPass := nil;
          if FCanUndo then
            SaveToStream(FUndoCon.RecordInfo);
        end;
        SetFocus;
        FDnX := X;
        FDnY := Y;
        FCrX := X;
        FCrY := Y;
        if ssCtrl in Shift then
          FInfo := TManagerInfo.Create
        else
        begin
          FInfo := TManagerInfo.Move;
          FIsChangingCamera := True;
        end;
      end;
    TMouseButton.mbRight:
      begin
        FMouseLR := False;
        if FActPass <> nil then
        begin
          FDnX := X;
          FDnY := Y;
        end;
        Repaint;
      end;
  end;
end;

procedure TUIManager.MouseMove(Shift: TShiftState; X, Y: Single);
var
  dx, dy: Single;
  i: Integer;
begin
  inherited;
  if FReadOnly then
    Exit;
  if FMouseDn and FMouseLR then
  begin
    case FInfo of
      TManagerInfo.Create:
        begin
          FCrX := X;
          FCrY := Y;
        end;
      TManagerInfo.Move:
        begin
          dx := X - FDnX;
          dy := Y - FDnY;
          for i := 0 to FPassages.Count - 1  do
            with FPassages[i].Position do
              Point := PointF(X + dx, Y + dy);
          for i := 0 to FCameraList.Count - 1  do
            with FCameraList[i] do
              PosPoint := PointF(PosX + dx, PosY + dy);
          FDnX := X;
          FDnY := Y;
        end;
    end;
    Repaint;
  end
  else if FMouseDn and not FMouseLR and (FActPass <> nil) then
  begin
    FDnX := X;
    FDnY := Y;
    Repaint;
  end;
end;

procedure TUIManager.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  dx, dy, px, py: Single;
  i: Integer;
begin
  inherited;
  if FReadOnly then
    Exit;
  if FMouseDn and FMouseLR then
  begin
    case FInfo of
      TManagerInfo.Create:
        begin
          FCrX := X;
          FCrY := Y;
          CreatePassage;
        end;
      TManagerInfo.Move:
        begin
          dx := X - FDnX;
          dy := Y - FDnY;
          for i := 0 to FPassages.Count - 1  do
            with FPassages[i].Position do
              Point := PointF(X + dx, Y + dy);
          for i := 0 to FCameraList.Count - 1  do
            with FCameraList[i] do
              PosPoint := PointF(PosX + dx, PosY + dy);
          FDnX := X;
          FDnY := Y;
          DoDragCamera;
        end;
    end;
    FInfo := TManagerInfo.Move;
  end
  else if FMouseDn and not FMouseLR and (FActPass <> nil) then
  begin
    with FActPass do
    begin
      BringToFront;
      SetFocus;
      if not FFlicker then
      begin
        FTimer := TTimer.Create(Owner);
        FTimer.Enabled := True;
        FTimer.Interval := 500;
        FTimer.OnTimer := Flickering;
        FFlicker := True;
      end;
    end;
    px := X - 0.5 * FActPass.Width;
    py := Y - 0.5 * FActPass.Height;
    MovePassage(FActPass, px, py);
    FActPass := nil;
    SetFocus;
  end;
  FMouseDn := False;
  FMouseLR := True;
end;

procedure TUIManager.MoveCameraItem(IFrom, ITo: Integer);
begin
  if (IFrom >= 0) and (IFrom < FCameraList.Count) and (ITo >= 0) and (ITo < FCameraList.Count) then
  begin
    FCameraList.Move(IFrom, ITo);
    FCamNameList.Move(IFrom, ITo);
  end;
end;

procedure TUIManager.MovePassage(Pass: TUIPassage; X, Y: Single);
begin
  Pass.Position.X := X;
  Pass.Position.Y := Y;
  MovePassEnd(Pass);
end;

procedure TUIManager.MovePassEnd(Pass: TUIPassage);
begin
  if FCanUndo then
    SaveToStream(FUndoCon.RecordInfo);
  if Assigned(FOnMovePassEnd) then
    FOnMovePassEnd(Self, Pass);
end;

procedure TUIManager.OverwriteCam(Index: Integer);
begin
  FCameraList[Index].Overwrite(0.5 * Width, 0.5 * Height);
end;

procedure TUIManager.Paint;
var
  sb: TStrokeBrush;
  it: Single;
begin
  inherited;
  Canvas.FillRect(GetShapeRect, 0, 0, [], 1, Fill);
  sb := TStrokeBrush.Create(TBrushKind.Solid, TAlphaColors.Black);
  try
    it := Abs(FIdentified);
    if FMouseDn and (FInfo = TManagerInfo.Create) then
    begin
      sb.Thickness := 0.5 * it + 1;
      sb.Color := TalphaColors.White;
      Canvas.DrawRect(GetCurRect, 0, 0, [], 1, sb);
      sb.Thickness := 0.25 * it + 0.5;
      sb.Color := TalphaColors.Black;
      Canvas.DrawRect(GetCurRect, 0, 0, [], 1, sb);
    end;
    if FMouseDn and not FMouseLR and (FActPass <> nil) then
      with FActPass do
      begin
        sb.Thickness := 0.5 * it + 1;
        sb.Color := TalphaColors.White;
        Self.Canvas.DrawRect(RectF(FDnX - 0.5 * Width, FDnY - 0.5 * Height, FDnX + 0.5 * Width, FDnY + 0.5 * Height),
          0, 0, [], 1, sb);
        sb.Thickness := 0.25 * it + 0.5;
        sb.Color := TalphaColors.Black;
        Self.Canvas.DrawRect(RectF(FDnX - 0.5 * Width, FDnY - 0.5 * Height, FDnX + 0.5 * Width, FDnY + 0.5 * Height),
          0, 0, [], 1, sb);
      end;
    if FIdentified > 0 then
    begin
      sb.Thickness := it + 1;
      sb.Color := TAlphaColors.White;
      Canvas.DrawRect(RectF(0, 0, Width, Height), 0, 0, [], 1, sb);
      sb.Thickness := 0.5 * it + 0.5;
      sb.Color := TAlphaColors.Black;
      Canvas.DrawRect(RectF(0, 0, Width, Height), 0, 0, [], 1, sb);
    end
    else if FShowSides then
    begin
      sb.Thickness := FSidesWidth;
      sb.Color := FSidesColor;
      Canvas.DrawRect(RectF(0, 0, Width, Height), 0, 0, [], 1, sb);
    end;
  finally
    FreeAndNil(sb);
  end;
end;

procedure TUIManager.RemovePassage(APassage: TUIPassage);
var
  i: Integer;
begin
  FActPass := nil;
  for i := 0 to FPassages.Count - 1 do
    if FPassages[i] = APassage then
    begin
      FPassages.Delete(i);
      if FCanUndo then
        SaveToStream(FUndoCon.RecordInfo);
      DoRemovePassage;
      Exit;
    end;
end;

procedure TUIManager.Redo;
begin
  if FCanUndo then
    LoadFromStream(FUndoCon.Redo);
end;

procedure TUIManager.Redraw;
var
  i: Integer;
begin
  for i := 0 to FPassages.Count - 1 do
    FPassages[i].Redraw;
  Repaint;
end;

procedure TUIManager.SaveToFile(FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TUIManager.SaveToStream(Stream: TStream);
var
  bs: TBytes;
  i, c: Integer;
  b: Boolean;
  ac: Cardinal;
  ts: Single;
  temp: TMemoryStream;
begin
  if not Assigned(Stream) then
    Exit;
  // 初始化
  bs := WideBytesOf('ELT_M');
  c := Length(bs);
  Stream.Write(c, SizeOf(Integer));
  Stream.WriteBuffer(bs, c);
  // 储存镜头
  c := FCameraList.Count;
  Stream.Write(c, SizeOf(Integer));
  for i := 0 to c - 1 do
  begin
    Stream.Write(FCameraList[i].FX, SizeOf(Single));
    Stream.Write(FCameraList[i].FY, SizeOf(Single));
  end;
  bs := WideBytesOf(FCamNameList.Text);
  c := Length(bs);
  Stream.Write(c, SizeOf(Integer));
  Stream.WriteBuffer(bs, c);
  // 储存背景
  if Fill.Kind = TBrushKind.None then
  begin
    b := False;
    Stream.Write(b, SizeOf(Boolean));
  end
  else
  begin
    b := True;
    Stream.Write(b, SizeOf(Boolean));
    if Fill.Kind = TBrushKind.Bitmap then
    begin
      b := True;
      Stream.Write(b, SizeOf(Boolean));
      temp := TMemoryStream.Create;
      try
        if Assigned(FBackground) then
        begin
          FBackground.Position := 0;
          temp.LoadFromStream(FBackground);
        end
        else
          Fill.Bitmap.Bitmap.SaveToStream(temp);
        temp.Position := 0;
        c := temp.Size;
        Stream.Write(c, SizeOf(Integer));
        Stream.WriteBuffer(temp.Memory^, c);
      finally
        FreeAndNil(temp);
      end;
    end
    else
    begin
      b := False;
      Stream.Write(b, SizeOf(Boolean));
      ac := Fill.Color;
      Stream.Write(ac, SizeOf(Cardinal));
    end;
  end;
  // 对文档循环
  temp := TMemoryStream.Create;
  try
    c := FPassages.Count;
    Stream.Write(c, SizeOf(Integer));
    for i := 0 to c - 1 do
    begin
      // 位置和尺寸
      ts := FPassages[i].Position.X;
      Stream.Write(ts, SizeOf(Single));
      ts := FPassages[i].Position.Y;
      Stream.Write(ts, SizeOf(Single));
      ts := FPassages[i].Width;
      Stream.Write(ts, SizeOf(Single));
      ts := FPassages[i].Height;
      Stream.Write(ts, SizeOf(Single));
      // 内容
      temp.Clear;
      FPassages[i].SaveToStream(temp);
      temp.Position := 0;
      c := temp.Size;
      Stream.Write(c, SizeOf(Integer));
      Stream.Write(temp.Memory^, c);
    end;
  finally
    FreeAndNil(temp);
  end;
  // 边框信息
  b := FShowSides;
  Stream.Write(b, SizeOf(Boolean));
  ts := FSidesWidth;
  Stream.Write(ts, SizeOf(Single));
  ac := FSidesColor;
  Stream.Write(ac, SizeOf(Cardinal));
end;

procedure TUIManager.SetBackground(AFileName: string);
begin
  Fill.Kind := TBrushKind.Bitmap;
  if Assigned(FBackground) then
    FBackground.Free;
  FBackground := TFileStream.Create(AFileName, fmOpenRead);
  Fill.Bitmap.Bitmap.LoadFromStream(FBackground);
end;

procedure TUIManager.SetCamera;
begin
  FCameraList.Add(TCameraUI.Create(0.5 * Width, 0.5 * Height, Self));
  FCamNameList.Add(Format('镜头 - %d', [FCameraList.Count]));
end;

procedure TUIManager.SetCamName(Index: Integer; const Value: string);
begin
  FCamNameList[Index] := Value;
end;

procedure TUIManager.SetColor(const Value: TAlphaColor);
begin
  Fill.Color := Value;
end;

procedure TUIManager.SetCurCamera(const Value: Integer);
begin
  if (Value > -1) and (Value < FCameraList.Count) then
    ApplyCamera(Value);
end;

procedure TUIManager.SetIdentified(const Value: Single);
begin
  if FIdentified <> Value then
    Repaint;
  if Value > 20 then
    FIdentified := 20
  else if Value < - 20 then
    FIdentified := - 20
  else
    FIdentified := Value;
end;

procedure TUIManager.SetIntervalBase(const Value: Single);
begin
  if Value <= 0 then
    FIntervalBase := 0
  else if Value >= 2 then
    FIntervalBase := 2
  else
    FIntervalBase := Value;
end;

procedure TUIManager.SetIntervalPlus(const Value: Single);
begin
  if Value <= 0 then
    FIntervalPlus := 0
  else if Value >= 2 then
    FIntervalPlus := 2
  else
    FIntervalPlus := Value;
end;

procedure TUIManager.SetShowSides(const Value: Boolean);
begin
  FShowSides := Value;
  Repaint;
end;

procedure TUIManager.SetSidesColor(const Value: TAlphaColor);
begin
  FSidesColor := Value;
  Repaint;
end;

procedure TUIManager.SetSidesWidth(const Value: Single);
begin
  if Value < 0 then
    FSidesWidth := 0
  else if FSidesWidth > 10 then
    FSidesWidth := 10
  else
    FSidesWidth := Value;
  Repaint;
end;

procedure TUIManager.Undo;
begin
  if FCanUndo then
    LoadFromStream(FUndoCon.Undo);
end;

procedure TUIManager.ViewMiniMap(X, Y: Single);
var
  pt, pb, pl, pr, he, wi, ra: Single;
  i: Integer;
begin
  if FIsChangingCamera then
    Exit;
  if (X < 0) or (X > 300) or (Y < 0) or (Y > 300) then
    Exit;
  // 获取边界坐标
  pt := 0;
  pb := Height;
  pl := 0;
  pr := Width;
  for i := 0 to FPassages.Count - 1 do
  begin
    if FPassages[i].Position.Y < pt then
      pt := FPassages[i].Position.Y;
    if FPassages[i].Position.Y + FPassages[i].Height > pb then
      pb := FPassages[i].Position.Y + FPassages[i].Height;
    if FPassages[i].Position.X < pl then
      pl := FPassages[i].Position.X;
    if FPassages[i].Position.X + FPassages[i].Width > pr then
      pr := FPassages[i].Position.X + FPassages[i].Width;
  end;
  // 确定实际位置
  he := pb - pt;
  wi := pr - pl;
  if he > wi then
  begin
    X := X - 150 * (1 - wi / he);
    ra := he / 300;
  end
  else if he = wi then
  begin
    ra := he / 300;
  end
  else
  begin
    Y := Y - 150 * (1 - he / wi);
    ra := wi / 300;
  end;
  X := X * ra + pl - 0.5 * Width;
  Y := Y * ra + pt - 0.5 * Height;
  // 调整镜头
  for i := 0 to FPassages.Count - 1 do
    with FPassages[i] do
      Position.Point := PointF(Position.X - X, Position.Y - Y);
  for i := 0 to FCameraList.Count - 1  do
    with FCameraList[i] do
      PosPoint := PointF(PosX - X, PosY - Y);
  DrawMiniMap;
end;

{ TCameraUI }

constructor TCameraUI.Create(X, Y: Single; AManager: TUIManager);
begin
  if AManager = nil then
    Exit;
  inherited Create;
  FManager := AManager;
  FX := X;
  FY := Y;
end;

function TCameraUI.GetPoint: TPointF;
begin
  Exit(PointF(FX, FY));
end;

procedure TCameraUI.MoveFinish(Sender: TObject);
begin
  FreeAndNil(Sender);
end;

procedure TCameraUI.MoveFinishEnd(Sender: TObject);
begin
  FreeAndNil(Sender);
  FManager.DoApplyCamera;
end;

procedure TCameraUI.Overwrite(X, Y: Single);
begin
  FX := X;
  FY := Y;
end;

procedure TCameraUI.SetCameraPoint(const Value: TPointF);
var
  dx, dy, interval, r: Single;
  i: Integer;
  an: TFloatAnimation;
begin
  if (FManager.FPassages.Count = 0) or (FManager.FIsChangingCamera) then
    Exit;
  dx := Value.X - FX;
  dy := Value.Y - FY;
  if (dx = 0) and (dy = 0) then
    Exit;
  r := Sqrt(dx * dx + dy * dy);
  interval := FManager.IntervalBase + FManager.IntervalPlus * r / 1000;
  FManager.FIsChangingCamera := True;
  for i := 1 to FManager.FPassages.Count - 1 do
  begin
    an := TFloatAnimation.Create(FManager.Owner);
    an.Parent := FManager.FPassages[i];
    an.PropertyName := 'Position.X';
    an.StartValue := FManager.FPassages[i].Position.X;
    an.StopValue := FManager.FPassages[i].Position.X + dx;
    an.Duration := interval;
    an.OnFinish := MoveFinish;
    an.Start;
    an := TFloatAnimation.Create(FManager.Owner);
    an.Parent := FManager.FPassages[i];
    an.PropertyName := 'Position.Y';
    an.StartValue := FManager.FPassages[i].Position.Y;
    an.StopValue := FManager.FPassages[i].Position.Y + dy;
    an.Duration := interval;
    an.OnFinish := MoveFinish;
    an.Start;
  end;
  an := TFloatAnimation.Create(FManager.Owner);
  an.Parent := FManager.FPassages[0];
  an.PropertyName := 'Position.X';
  an.StartValue := FManager.FPassages[0].Position.X;
  an.StopValue := FManager.FPassages[0].Position.X + dx;
  an.Duration := interval;
  an.OnFinish := MoveFinish;
  an.Start;
  an := TFloatAnimation.Create(FManager.Owner);
  an.Parent := FManager.FPassages[0];
  an.PropertyName := 'Position.Y';
  an.StartValue := FManager.FPassages[0].Position.Y;
  an.StopValue := FManager.FPassages[0].Position.Y + dy;
  an.Duration := interval;
  an.OnFinish := MoveFinishEnd;
  an.Start;
  for i := 0 to FManager.FCameraList.Count - 1 do
    with FManager.FCameraList[i] do
      PosPoint := PointF(PosX + dx, PosY + dy);
end;

procedure TCameraUI.SetPosPoint(const Value: TPointF);
begin
  FX := Value.X;
  FY := Value.Y;
end;

{ TUndoCon }

constructor TUndoCon.Create(Count: Integer);
var
  i: Integer;
begin
  inherited Create;
  FCount := Count;
  FStreams := TObjectList<TMemoryStream>.Create;
  for i := 0 to FCount - 1 do
    FStreams.Add(TMemoryStream.Create);
  FCurIndex := -1;
  FPointIndex := -1;
  FValidPoints := -1;
end;

destructor TUndoCon.Destroy;
begin
  FStreams.Clear;
  FStreams.Free;
  inherited;
end;

procedure TUndoCon.Initiate(Count: Integer);
var
  i: Integer;
begin
  FStreams.Clear;
  FCount := Count;
  for i := 0 to FCount - 1 do
    FStreams.Add(TMemoryStream.Create);
  FCurIndex := -1;
  FPointIndex := -1;
  FValidPoints := -1;
end;

function TUndoCon.RecordInfo: TMemoryStream;
begin
  FCurIndex := FCurIndex + 1;
  if FCurIndex = FCount then
    FCurIndex := 0;
  FPointIndex := FCurIndex;
  if FValidPoints < FCount then
    FValidPoints := FValidPoints + 1;
  FStreams[FCurIndex].Clear;
  Exit(FStreams[FCurIndex]);
end;

function TUndoCon.Redo: TMemoryStream;
begin
  if FCurIndex = FPointIndex then
    Exit(nil);
  FCurIndex := FCurIndex + 1;
  if FCurIndex = FCount then
    FCurIndex := 0;
  if FValidPoints < FCount then
    FValidPoints := FValidPoints + 1;
  FStreams[FCurIndex].Position := 0;
  Exit(FStreams[FCurIndex]);
end;

function TUndoCon.Undo: TMemoryStream;
var
  i: Integer;
begin
  if FValidPoints <= 0 then
    Exit(nil)
  else
    FValidPoints := FValidPoints - 1;
  i := FCurIndex - 1;
  if i = -1 then
    i := FCount - 1;
  if i = FPointIndex then
    Exit(nil);
  FCurIndex := i;
  FStreams[FCurIndex].Position := 0;
  Exit(FStreams[FCurIndex]);
end;

end.
