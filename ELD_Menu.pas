{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{                                                       }
{               ELD Menu Control Frame                  }
{                   ELD 主菜单框架                      }
{                     ELD 1.11                          }
{                                                       }
{    Copyright(c) 2016-2018 Reniasty de El Magnifico    }
{                   天道玄虚 出品                       }
{                 All rights reserved                   }
{                   保留所有权利                        }
{                                                       }
{*******************************************************}

unit ELD_Menu;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Math.Vectors, System.Generics.Collections,
  FMX.Controls3D, FMX.Objects3D, FMX.Edit, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Objects,
  FMX.NumberBox, FMX.ListBox, FMX.TabControl, FMX.Colors, ELT.Main, FMX.EditBox, FMX.Layouts, FMX.Controls.Presentation,
  FMX.Types, Xml.XMLDoc, Xml.XMLIntf;

type
  TELDType = (Document, Model, Lattice);

  TCameraInfo = record
    IsTransited:             Boolean;
    FocusPoint, AnglePoint:  TPoint3D;
    FocalDistance:           Single;
    IntervalBase:            Single;
    IntervalPlus:            Single;
    class function Create(Focus, Angle: TPoint3D; FocalDis: Single; IsTrans: Boolean = False;
      IntB: Single = 0.20; IntP: Single = 0.05): TCameraInfo; static;
  end;

  TKeyInfo = record
    Key:           Word;
    KeyChar:       Char;
    class function Create(Key: Word; KeyChar: Char): TKeyInfo; overload; static;
    class function Create(Str: string): TKeyInfo; overload; static;
    class function Zero: TKeyInfo; static;
    class operator Equal(const KeyInfo1, KeyInfo2: TKeyInfo): Boolean; inline;
    class operator NotEqual(const KeyInfo1, KeyInfo2: TKeyInfo): Boolean; inline;
    function KeyToStr: string; overload;
    function KeyToStr(KeyInfo: TKeyInfo): string; overload;
    function KeyToStr(Key: Word; KeyChar: Char): string; overload;
  end;

  TMenuFrame = class(TFrame)
    MenuBack: TTabControl;
    MenuFile: TTabItem;
    MenuNew: TTabItem;
    MenuCamera: TTabItem;
    FileBack: TPanel;
    FileNew: TButton;
    FileOpen: TButton;
    FileSave: TButton;
    FileSaveAs: TButton;
    NewBack: TPanel;
    NewELT: TButton;
    NewModel: TButton;
    CamList: TListBox;
    BtnRecord: TButton;
    BtnOverwrite: TButton;
    BtnDelete: TButton;
    BtnListClr: TButton;
    BtnUp: TButton;
    BtnDn: TButton;
    FileExit: TButton;
    MenuInfo: TTabItem;
    InfoAbout: TUIPassage;
    FileHint: TUIPassage;
    NewHint: TUIPassage;
    NewLattice: TButton;
    MenuBackground: TTabItem;
    BackCon: TTabControl;
    BackColor: TTabItem;
    BackHint: TUIPassage;
    BackGradient: TTabItem;
    BackImage: TTabItem;
    ColorSolid: TColorPanel;
    ColorR: TNumberBox;
    ColorLR: TLabel;
    ColorLG: TLabel;
    ColorG: TNumberBox;
    ColorLB: TLabel;
    ColorB: TNumberBox;
    ColorLA: TLabel;
    ColorA: TNumberBox;
    GradLR: TLabel;
    GradR: TNumberBox;
    GradG: TNumberBox;
    GradLG: TLabel;
    GradLB: TLabel;
    GradB: TNumberBox;
    GradA: TNumberBox;
    GradLA: TLabel;
    GradientEdit: TGradientEdit;
    ColorGrad: TColorPanel;
    GradDelete: TSpeedButton;
    StyleBox: TComboBox;
    X1V: TNumberBox;
    X1L: TLabel;
    Y1V: TNumberBox;
    Y1L: TLabel;
    X2L: TLabel;
    X2V: TNumberBox;
    Y2L: TLabel;
    Y2V: TNumberBox;
    ImageKind: TComboBox;
    ImageLoad: TSpeedButton;
    IsTransparency: TCheckBox;
    CamBack: TPanel;
    CamFPX: TNumberBox;
    CamLFPX: TLabel;
    CamLFPY: TLabel;
    CamFPY: TNumberBox;
    CamLPFZ: TLabel;
    CamFPZ: TNumberBox;
    CamLRAX: TLabel;
    CamRAX: TNumberBox;
    CamLRAY: TLabel;
    CamRAY: TNumberBox;
    CamLRAZ: TLabel;
    CamRAZ: TNumberBox;
    CamLFD: TLabel;
    CamFD: TNumberBox;
    CamLName: TLabel;
    CamName: TEdit;
    CamRenClr: TClearEditButton;
    IntervalBL: TLabel;
    IntervalB: TNumberBox;
    IntervalPL: TLabel;
    IntervalP: TNumberBox;
    CamIsTransit: TCheckBox;
    DIntLB: TLabel;
    DIntB: TNumberBox;
    DIntLP: TLabel;
    DIntP: TNumberBox;
    MenuKeys: TTabItem;
    KeyList: TListBox;
    KeyBack: TPanel;
    KeySave: TButton;
    KeyDefault: TButton;
    KeySelClr: TButton;
    CVAngleL: TLabel;
    CVAngle: TNumberBox;
    BackRect: TRectangle;
    FPSLV: TLabel;
    FPSV: TNumberBox;
    procedure ColorSolidChange(Sender: TObject);
    procedure SolidColorChange(Sender: TObject);
    procedure ColorGradChange(Sender: TObject);
    procedure GradDeleteClick(Sender: TObject);
    procedure GradColorChange(Sender: TObject);
    procedure XYVChange(Sender: TObject);
    procedure GradientEditChange(Sender: TObject);
    procedure StyleBoxChange(Sender: TObject);
    procedure GradientEditClick(Sender: TObject);
    procedure BtnDeleteClick(Sender: TObject);
    procedure BtnDnClick(Sender: TObject);
    procedure BtnListClrClick(Sender: TObject);
    procedure BtnUpClick(Sender: TObject);
    procedure CamListChange(Sender: TObject);
    procedure CamRenClrClick(Sender: TObject);
    procedure CamNameChange(Sender: TObject);
    procedure CamIsTransitChange(Sender: TObject);
    procedure IntervalBChange(Sender: TObject);
    procedure IntervalPChange(Sender: TObject);
    procedure CamFPXChange(Sender: TObject);
    procedure CamFPYChange(Sender: TObject);
    procedure CamFPZChange(Sender: TObject);
    procedure CamRAXChange(Sender: TObject);
    procedure CamRAYChange(Sender: TObject);
    procedure CamRAZChange(Sender: TObject);
    procedure CamFDChange(Sender: TObject);
    procedure KeySelClrClick(Sender: TObject);
  private
    FCreateType:        TELDType;
    FIsUpdating:        Boolean;
    FisChangingColor:   Boolean;
    FIsChangingGrad:    Boolean;
    FIsChangingCam:     Boolean;
    FCamInfos:          TList<TCameraInfo>;
    // 快捷键
    FKeyInfos:          TList<TKeyInfo>;
    FKeyNames:          TList<string>;
    function GetKeyName(Index: Integer): string;
    function GetKeyId(Index: string): Integer;
    function GetKeyInfo(Index: Integer): TKeyInfo;
    procedure SetKeyInfo(Index: Integer; const Value: TKeyInfo);
    procedure SetCreateType(const Value: TELDType);
  public
    constructor Create(AOwner: TComponent); override;
    function GetCamInterval(Distance: Single): Single;
    function LoadKeyConfig(AFileName: TFileName): Boolean;
    function SaveKeyConfig(AFileName: TFileName): Boolean;
    property CreateType:               TELDType            read FCreateType         write SetCreateType;
    property IsListUpdating:           Boolean             read FIsUpdating         write FIsUpdating;
    property CamInfos:                 TList<TCameraInfo>  read FCamInfos;
    property KeyNames[Index: Integer]: string              read GetKeyName;
    property KeyIds[Index: string]:    Integer             read GetKeyId;
    property KeyInfos[Index: Integer]: TKeyInfo            read GetKeyInfo          write SetKeyInfo;
  end;

const
  // 镜头移动
  KI_MoveForward:       Word = $0000;
  KI_MoveBackward:      Word = $0001;
  KI_MoveLeft:          Word = $0002;
  KI_MoveRight:         Word = $0003;
  KI_MoveUp:            Word = $0004;
  KI_MoveDown:          Word = $0005;
  // 镜头转动
  KI_RotateUp:          Word = $0006;
  KI_RotateDown:        Word = $0007;
  KI_RotateLeft:        Word = $0008;
  KI_RotateRight:       Word = $0009;
  KI_RotateClock:       Word = $000A;
  KI_RotateAnti:        Word = $000B;
  // 镜头控制
  KI_ResetCamera:       Word = $000C;
  KI_NewCamera:         Word = $000D;
  KI_PrevCamera:        Word = $000E;
  KI_NextCamera:        Word = $000F;
  KI_FirstCamera:       Word = $0010;
  KI_LastCamera:        Word = $0011;
  // 界面控制
  KI_Menu:              Word = $0012;
  KI_View:              Word = $0013;

implementation

{$R *.fmx}

{ TMenuFrame }

function TMenuFrame.SaveKeyConfig(AFileName: TFileName): Boolean;
var
  i: Integer;
  ss: TStringList;
begin
  ss := TStringList.Create;
  try
    ss.Add('<?xml version="1.0" encoding="utf-8"?>');
    ss.Add('<KeyInfos>');
    for i := 0 to FKeyNames.Count - 1 do
      ss.Add(Format('%s<KeyInfo Name="%s" Value="%s"/>', [#9, FKeyNames[i], FKeyInfos[i].KeyToStr]));
    ss.Add('</KeyInfos>');
  finally
    ss.SaveToFile(AFileName, TEncoding.UTF8);
    ss.Free;
  end;
  Exit(True);
end;

procedure TMenuFrame.SetCreateType(const Value: TELDType);
begin
  FCreateType := Value;
  NewELT.Text := '文档框架';
  NewModel.Text := '模型框架';
  NewLattice.Text := '阵列框架';
  case FCreateType of
    Document: NewELT.Text := '*文档框架';
    Model: NewModel.Text := '*模型框架';
    Lattice: NewLattice.Text := '*阵列框架';
  end;
end;

procedure TMenuFrame.SetKeyInfo(Index: Integer; const Value: TKeyInfo);
begin
  if (Index > -1) and (Index < FKeyInfos.Count) then
    FKeyInfos.List[Index] := Value;
end;

procedure TMenuFrame.SolidColorChange(Sender: TObject);
var
  ac: TAlphaColor;
begin
  if not FIsChangingColor then
  begin
    TAlphaColorRec(ac).R := Trunc(ColorR.Value);
    TAlphaColorRec(ac).G := Trunc(ColorG.Value);
    TAlphaColorRec(ac).B := Trunc(ColorB.Value);
    TAlphaColorRec(ac).A := Trunc(ColorA.Value);
    ColorSolid.Color := ac;
  end;
end;

procedure TMenuFrame.StyleBoxChange(Sender: TObject);
begin
  if not FIsChangingGrad then
  begin
    GradientEdit.Gradient.Style := TGradientStyle(StyleBox.ItemIndex);
    GradientEdit.Gradient.Change;
    GradientEdit.UpdateEffects;
  end;
end;

procedure TMenuFrame.XYVChange(Sender: TObject);
begin
  if not FIsChangingGrad then
  begin
    with GradientEdit.Gradient do
    if Style = TGradientStyle.Linear then
    begin
      StartPosition.Point := PointF(X1V.Value, Y1V.Value);
      StopPosition.Point := PointF(X2V.Value, Y2V.Value);
    end
    else if Style = TGradientStyle.Radial then
      RadialTransform.Position.Point := PointF(X1V.Value, Y1V.Value);
  end;
end;

constructor TMenuFrame.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited;
  FCamInfos := TList<TCameraInfo>.Create;
  FKeyInfos := TList<TKeyInfo>.Create;
  FKeyNames := TList<string>.Create;
  CreateType := Document;
  // 快捷键名称初始化
  with FKeyNames do
  begin
    // 镜头移动
    Add('焦点前移');
    Add('焦点后移');
    Add('焦点左移');
    Add('焦点右移');
    Add('焦点上移');
    Add('焦点下移');
    // 镜头转动
    Add('镜头上旋');
    Add('镜头下旋');
    Add('镜头左旋');
    Add('镜头右旋');
    Add('镜头顺旋');
    Add('镜头逆旋');
    // 镜头控制
    Add('重置相机');
    Add('新建相机');
    Add('前一相机');
    Add('后一相机');
    Add('首个相机');
    Add('末个相机');
    // 界面控制
    Add('菜单选项');
    Add('观察选项');
  end;
  // 快捷键初始化
  for i := 0 to FKeyNames.Count - 1 do
  begin
    FKeyInfos.Add(TKeyInfo.Zero);
    KeyList.Items.Add(Format('%s：[%s]', [FKeyNames[i], FKeyInfos[i].KeyToStr]));
  end;
end;

procedure TMenuFrame.BtnDeleteClick(Sender: TObject);
begin
  if CamList.ItemIndex > -1 then
  begin
    CamInfos.Delete(CamList.ItemIndex);
    CamList.Items.Delete(CamList.ItemIndex);
  end;
end;

procedure TMenuFrame.BtnDnClick(Sender: TObject);
var
  i: Integer;
begin
  with CamList do if (ItemIndex > -1) and (ItemIndex < Count - 1) then
  begin
    i := ItemIndex + 1;
    CamInfos.Move(ItemIndex, i);
    Items.Move(ItemIndex, i);
    ItemIndex := i;
    CamName.Text := Items[i];
  end;
end;

procedure TMenuFrame.BtnListClrClick(Sender: TObject);
begin
  CamInfos.Clear;
  CamList.Items.Clear;
end;

procedure TMenuFrame.BtnUpClick(Sender: TObject);
var
  i: Integer;
begin
  with CamList do if ItemIndex > 0 then
  begin
    i := ItemIndex - 1;
    CamInfos.Move(ItemIndex, i);
    Items.Move(ItemIndex, i);
    ItemIndex := i;
    CamName.Text := Items[i];
  end;
end;

procedure TMenuFrame.CamFDChange(Sender: TObject);
begin
  if FIsChangingCam then
    Exit;
  if CamList.ItemIndex > -1 then
    FCamInfos.List[CamList.ItemIndex].FocalDistance := CamFD.Value;
end;

procedure TMenuFrame.CamFPXChange(Sender: TObject);
begin
  if FIsChangingCam then
    Exit;
  if CamList.ItemIndex > -1 then
    FCamInfos.List[CamList.ItemIndex].FocusPoint.X := CamFPX.Value;
end;

procedure TMenuFrame.CamFPYChange(Sender: TObject);
begin
  if FIsChangingCam then
    Exit;
  if CamList.ItemIndex > -1 then
    FCamInfos.List[CamList.ItemIndex].FocusPoint.Y := CamFPY.Value;
end;

procedure TMenuFrame.CamFPZChange(Sender: TObject);
begin
  if FIsChangingCam then
    Exit;
  if CamList.ItemIndex > -1 then
    FCamInfos.List[CamList.ItemIndex].FocusPoint.Z := CamFPZ.Value;
end;

procedure TMenuFrame.CamIsTransitChange(Sender: TObject);
begin
  if FIsChangingCam then
    Exit;
  if CamList.ItemIndex > -1 then
    FCamInfos.List[CamList.ItemIndex].IsTransited := CamIsTransit.IsChecked;
end;

procedure TMenuFrame.CamListChange(Sender: TObject);
begin
  if CamList.ItemIndex > -1 then with FCamInfos[CamList.ItemIndex] do
  begin
    FIsChangingCam := True;
    CamName.Text := CamList.Items[CamList.ItemIndex];
    CamIsTransit.IsChecked := IsTransited;
    IntervalB.Value := IntervalBase;
    IntervalP.Value := IntervalPlus;
    CamFPX.Value := FocusPoint.X;
    CamFPY.Value := FocusPoint.Y;
    CamFPZ.Value := FocusPoint.Z;
    CamRAX.Value := AnglePoint.X;
    CamRAY.Value := AnglePoint.Y;
    CamRAZ.Value := AnglePoint.Z;
    CamFD.Value := FocalDistance;
  end;
  FIsChangingCam := False;
end;

procedure TMenuFrame.CamNameChange(Sender: TObject);
begin
  if FIsChangingCam then
    Exit;
  if CamList.ItemIndex > -1 then
    CamList.Items[CamList.ItemIndex] := CamName.Text;
end;

procedure TMenuFrame.CamRAXChange(Sender: TObject);
begin
  if FIsChangingCam then
    Exit;
  if CamList.ItemIndex > -1 then
    FCamInfos.List[CamList.ItemIndex].AnglePoint.X := CamRAX.Value;
end;

procedure TMenuFrame.CamRAYChange(Sender: TObject);
begin
  if FIsChangingCam then
    Exit;
  if CamList.ItemIndex > -1 then
    FCamInfos.List[CamList.ItemIndex].AnglePoint.Y := CamRAY.Value;
end;

procedure TMenuFrame.CamRAZChange(Sender: TObject);
begin
  if FIsChangingCam then
    Exit;
  if CamList.ItemIndex > -1 then
    FCamInfos.List[CamList.ItemIndex].AnglePoint.Z := CamRAZ.Value;
end;

procedure TMenuFrame.CamRenClrClick(Sender: TObject);
begin
  CamName.Text := '';
end;

procedure TMenuFrame.ColorGradChange(Sender: TObject);
var
  ac: TAlphaColor;
begin
  FIsChangingColor := True;
  try
    ac := ColorGrad.Color;
    GradR.Value := TAlphaColorRec(ac).R;
    GradG.Value := TAlphaColorRec(ac).G;
    GradB.Value := TAlphaColorRec(ac).B;
    GradA.Value := TAlphaColorRec(ac).A;
  finally
    FIsChangingColor := False;
  end;
  if not FIsChangingGrad then
  begin
    GradientEdit.Gradient.Points[GradientEdit.CurrentPoint].Color := ColorGrad.Color;
    GradientEdit.Gradient.Change;
    GradientEdit.UpdateEffects;
  end;
end;

procedure TMenuFrame.GradDeleteClick(Sender: TObject);
begin
  with GradientEdit do
    if (CurrentPoint > -1) and (CurrentPoint < Gradient.Points.Count) then
    begin
      Gradient.Points.Delete(CurrentPoint);
      GradientEdit.Gradient.Change;
      GradientEdit.UpdateEffects;
    end;
end;

procedure TMenuFrame.GradientEditChange(Sender: TObject);
begin
  FIsChangingGrad := True;
  try
    with GradientEdit do
    begin
      ColorGrad.Color := Gradient.Points[CurrentPoint].Color;
      if Gradient.Style = TGradientStyle.Linear then
      begin
        X1V.Value := Gradient.StartPosition.X;
        Y1V.Value := Gradient.StartPosition.Y;
        X2V.Value := Gradient.StopPosition.X;
        Y2V.Value := Gradient.StopPosition.Y;
        StyleBox.ItemIndex := 0;
      end
      else if Gradient.Style = TGradientStyle.Radial then
      begin
        X1V.Value := Gradient.RadialTransform.Position.X;
        Y1V.Value := Gradient.RadialTransform.Position.Y;
        StyleBox.ItemIndex := 1;
      end;
    end;
  finally
    FIsChangingGrad := False;
  end;
end;

procedure TMenuFrame.GradientEditClick(Sender: TObject);
begin
  FIsChangingGrad := True;
  try
  with GradientEdit do
    ColorGrad.Color := Gradient.Points[CurrentPoint].Color;
  finally
    FIsChangingGrad := False;
  end;
end;

procedure TMenuFrame.IntervalBChange(Sender: TObject);
begin
  if CamList.ItemIndex > -1 then
    FCamInfos.List[CamList.ItemIndex].IntervalBase := IntervalB.Value;
end;

procedure TMenuFrame.IntervalPChange(Sender: TObject);
begin
  if CamList.ItemIndex > -1 then
    FCamInfos.List[CamList.ItemIndex].IntervalPlus := IntervalP.Value;
end;

procedure TMenuFrame.KeySelClrClick(Sender: TObject);
begin
  KeyList.ItemIndex := -1;
end;

function TMenuFrame.LoadKeyConfig(AFileName: TFileName): Boolean;
var
  AXMLDoc: TXMLDocument;
  AXMLNode: IXMLNode;
  i: Integer;
  ks, ksv, ki, kiv: string;
begin
  if not FileExists(AFileName) then
    Exit(False);
  AXMLDoc := TXMLDocument.Create(Self);
  try
    AXMLDoc.LoadFromFile(AFileName);
    if AXMLDoc.DocumentElement.NodeName <> 'KeyInfos' then
      Exit(False);
    for i := 0 to AXMLDoc.DocumentElement.ChildNodes.Count - 1 do
    begin
      AXMLNode := AXMLDoc.DocumentElement.ChildNodes[i];
      if AXMLNode.NodeName <> 'KeyInfo' then
        Exit(False);
      ks := AXMLNode.AttributeNodes[0].NodeName;
      ki := AXMLNode.AttributeNodes[1].NodeName;
      ksv := AXMLNode.AttributeNodes[0].Text;
      kiv := AXMLNode.AttributeNodes[1].Text;
      if (ks = 'Name') and (ki = 'Value') then
        FKeyInfos[KeyIds[ksv]] := TKeyInfo.Create(kiv)
      else if (ks = 'Value') and (ki = 'Value') then
        KeyInfos[KeyIds[kiv]] := TKeyInfo.Create(ksv)
      else
        Exit(False);
    end;
  finally
    AXMLDoc.Free;
  end;
  KeyList.Items.Clear;
  for i := 0 to FKeyNames.Count - 1 do
    KeyList.Items.Add(Format('%s：[%s]', [FKeyNames[i], FKeyInfos[i].KeyToStr]));
  Exit(True);
end;

function TMenuFrame.GetCamInterval(Distance: Single): Single;
begin
  Exit(IntervalB.Value + IntervalP.Value * Distance / 100);
end;

function TMenuFrame.GetKeyId(Index: string): Integer;
var
  i: Integer;
begin
  for i := 0 to FKeyNames.Count do
  begin
    if i = FKeyNames.Count then
      Exit(-1);
    if FKeyNames[i] = Index then
      Exit(i);
  end;
  Exit(-1);
end;

function TMenuFrame.GetKeyInfo(Index: Integer): TKeyInfo;
begin
  if (Index > -1) and (Index < FKeyInfos.Count) then
    Exit(FKeyInfos[Index])
  else
    Exit(TKeyInfo.Create(0, #0));
end;

function TMenuFrame.GetKeyName(Index: Integer): string;
begin
  if (Index > -1) and (Index < FKeyNames.Count) then
    Exit(FKeyNames[Index])
  else
    Exit('');
end;

procedure TMenuFrame.GradColorChange(Sender: TObject);
var
  ac: TAlphaColor;
begin
  if not FIsChangingColor then
  begin
    TAlphaColorRec(ac).R := Trunc(GradR.Value);
    TAlphaColorRec(ac).G := Trunc(GradG.Value);
    TAlphaColorRec(ac).B := Trunc(GradB.Value);
    TAlphaColorRec(ac).A := Trunc(GradA.Value);
    ColorGrad.Color := ac;
  end;
end;

procedure TMenuFrame.ColorSolidChange(Sender: TObject);
var
  ac: TAlphaColor;
begin
  FIsChangingColor := True;
  try
    ac := ColorSolid.Color;
    ColorR.Value := TAlphaColorRec(ac).R;
    ColorG.Value := TAlphaColorRec(ac).G;
    ColorB.Value := TAlphaColorRec(ac).B;
    ColorA.Value := TAlphaColorRec(ac).A;
  finally
    FIsChangingColor := False;
  end;
end;

{ TCameraInfo }

class function TCameraInfo.Create(Focus, Angle: TPoint3D; FocalDis: Single; IsTrans: Boolean; IntB,
  IntP: Single): TCameraInfo;
begin
  Result.FocusPoint := Focus;
  Result.AnglePoint := Angle;
  Result.FocalDistance := FocalDis;
  Result.IsTransited := IsTrans;
  Result.IntervalBase := IntB;
  Result.IntervalPlus := IntP;
end;

{ TKeyInfo }

class function TKeyInfo.Create(Key: Word; KeyChar: Char): TKeyInfo;
begin
  if Key > 127 then
    Exit(TKeyInfo.Zero);
  Result.Key := Key;
  Result.KeyChar := KeyChar;
end;

class function TKeyInfo.Create(Str: string): TKeyInfo;
begin
  if Str.Length = 1 then
  begin
    Result.Key := 0;
    Result.KeyChar := Str.Chars[0];
  end
  else if Str.Chars[0] = '#' then
  begin
    Result.Key := StrToInt(Str.Substring(1));
    Result.KeyChar := #0;
  end;
end;

class function TKeyInfo.Zero: TKeyInfo;
begin
  Result.Key := 0;
  Result.KeyChar := #0;
end;

class operator TKeyInfo.Equal(const KeyInfo1, KeyInfo2: TKeyInfo): Boolean;
begin
  Result := (KeyInfo1.Key = KeyInfo2.Key) and (UpperCase(KeyInfo1.KeyChar) = UpperCase(KeyInfo2.KeyChar));
end;

class operator TKeyInfo.NotEqual(const KeyInfo1, KeyInfo2: TKeyInfo): Boolean;
begin
  Result := not (KeyInfo1 = KeyInfo2);
end;

function TKeyInfo.KeyToStr: string;
begin
  Exit(KeyToStr(Key, KeyChar));
end;

function TKeyInfo.KeyToStr(KeyInfo: TKeyInfo): string;
begin
  Exit(KeyToStr(KeyInfo.Key, KeyInfo.KeyChar));
end;

function TKeyInfo.KeyToStr(Key: Word; KeyChar: Char): string;
begin
  if KeyChar <> '' then
    Exit(UpperCase(KeyChar))
  else
    Exit(Format('#%d', [Key]));
end;

end.
