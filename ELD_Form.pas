{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{                                                       }
{            Enchanted Lens Documents (ELD)             }
{                   附魔镜头文档                        }
{                     ELD 1.11                          }
{                                                       }
{    Copyright(c) 2016-2018 Reniasty de El Magnifico    }
{                   天道玄虚 出品                       }
{                 All rights reserved                   }
{                   保留所有权利                        }
{                                                       }
{*******************************************************}

unit ELD_Form;

interface

uses
  Winapi.Windows, System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Math,
  System.Generics.Collections, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, System.Math.Vectors,
  FMX.Dialogs, FMX.Layers3D, FMX.Objects3D, FMX.Controls3D, FMX.ASE.Importer, FMX.DAE.Importer, FMX.OBJ.Importer,
  FMX.Objects,FMX.MaterialSources, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Viewport3D, FMX.Types3D,
  ELT.Main, ELT.Extra, ELT_MngFrame, ELT_PsgFrame, ELD_Menu, ELD_Model, ELD_CamCon, ELD_Lens, ELD_Lattice;

type
  TELDForm = class(TForm)
    ELDView: TViewport3D;
    FocusColor: TColorMaterialSource;
    CurColor: TColorMaterialSource;
    Light: TLight;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    BottomMenu: TPanel;
    CamCon: TCamConMenu;
    LeftMenu: TPanel;
    RightMenu: TPanel;
    TopMenu: TPanel;
    Background: TRectangle;
    SysClose: TSpeedButton;
    SysMax: TSpeedButton;
    SysMin: TSpeedButton;
    MainMenu: TPanel;
    Title: TLabel;
    PsgCon: TPsgFrame;
    MngCon: TMngFrame;
    MdlCon: TModelFrame;
    LatCon: TLatticeFrame;
    LenCon: TLensFrame;
    TheMenu: TMenuFrame;
    FPSTimer: TTimer;
    CamFocus: TSphere;
    CamCurX: TSphere;
    CamCurY: TSphere;
    LenRecord: TSphere;
    MainCam: TCamera;
    SysFull: TSpeedButton;
    procedure CamConResetClick(Sender: TObject);
    procedure ELDViewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ELDViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure ELDViewMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ELDViewMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
    procedure ELDViewKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure ELDViewKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FPSTimerTimer(Sender: TObject);
    procedure FrameSizeChange(Sender: TObject);
    procedure MdlConBtnDoneClick(Sender: TObject);
    procedure MngConBtnLoadClick(Sender: TObject);
    procedure LatConBtnDoneClick(Sender: TObject);
    procedure ELTBtnDoneClick(Sender: TObject);
    procedure SysCloseClick(Sender: TObject);
    procedure SysMaxClick(Sender: TObject);
    procedure SysMinClick(Sender: TObject);
    procedure TheMenuFileNewClick(Sender: TObject);
    procedure TheMenuFileOpenClick(Sender: TObject);
    procedure TheMenuFileSaveAsClick(Sender: TObject);
    procedure TheMenuFileSaveClick(Sender: TObject);
    procedure TheMenuNewModelClick(Sender: TObject);
    procedure TheMenuNewLatticeClick(Sender: TObject);
    procedure TheMenuColorSolidChange(Sender: TObject);
    procedure TheMenuMenuBackChange(Sender: TObject);
    procedure TheMenuBackConChange(Sender: TObject);
    procedure TheMenuImageLoadClick(Sender: TObject);
    procedure TheMenuImageKindChange(Sender: TObject);
    procedure TheMenuGradientEditChange(Sender: TObject);
    procedure TheMenuIsTransparencyChange(Sender: TObject);
    procedure TopMenuMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure TheMenuCamListClick(Sender: TObject);
    procedure TheMenuFileExitClick(Sender: TObject);
    procedure TheMenuBtnOverwriteClick(Sender: TObject);
    procedure TheMenuBtnRecordClick(Sender: TObject);
    procedure CamConUpClick(Sender: TObject);
    procedure CamConDownClick(Sender: TObject);
    procedure CamConFirstClick(Sender: TObject);
    procedure CamConLastClick(Sender: TObject);
    procedure TheMenuKeySaveClick(Sender: TObject);
    procedure TheMenuKeyDefaultClick(Sender: TObject);
    procedure TheMenuCVAngleChange(Sender: TObject);
    procedure SysFullClick(Sender: TObject);
    procedure LenConLenHideShowClick(Sender: TObject);
    procedure TheMenuNewELTClick(Sender: TObject);
    procedure TopMenuMouseEnter(Sender: TObject);
    procedure ELDViewMouseEnter(Sender: TObject);
    procedure CamConCConResizeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
  private
    procedure ManagerCreatePass(Sender, Control: TObject);
    procedure ManagerKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure The3DMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single; RayPos,
  RayDir: TVector3D);
    procedure The3DMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
    procedure The3DMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single; RayPos,
      RayDir: TVector3D);
    procedure TheMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure TheMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure TheMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ManagerEnter(Sender: TObject);
    procedure PassageEnter(Sender: TObject);
    procedure ModelEnter(Sender: TObject);
    procedure LatticeEnter(Sender: TObject);
    procedure ManagerMouseEnter(Sender: TObject);
    procedure ManagerMouseLeave(Sender: TObject);
    procedure PassageMouseEnter(Sender: TObject);
    procedure PassageMouseLeave(Sender: TObject);
    procedure ModelMouseEnter(Sender: TObject);
    procedure ModelMouseLeave(Sender:TObject);
    procedure UpdateMaterial;
    procedure ManagerLastFinish(Sender: TObject);
    procedure ManagerReturnFinish(Sender: TObject);
    procedure ModelLastFinish(Sender: TObject);
    procedure ModelReturnFinish(Sender: TObject);
    procedure LatticeLastFinish(Sender: TObject);
    procedure LatticeReturnFinish(Sender: TObject);
    procedure PositionLastFinish(Sender: TObject);
    procedure ShowSideMenu(AMenu: TFrame);
    procedure ShowHideMainMenu;
    procedure HideSideMenu;
  public
    function GetFocalDistance: Single;
    procedure Clear;
    procedure HideOthers;
    procedure HideNone;
    procedure LoadFromFile(FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(FileName: string);
    procedure SaveToStream(Stream: TStream);
    procedure ShowTitle;
  end;

function DealVersion(Ver: string): string;
function GetVersion(FileName: string): string;

var
  ELDForm:                             TELDForm;
  ABackground:                         TLayer3D;
  AManager:                            TUIManager;
  AModel:                              TModel3D;
  LManager:                            TObjectList<TUIManager>;
  LModel:                              TObjectList<TModel3D>;
  LLattice:                            TObjectList<TELDLattice>;
  AModelContain:                       TStrokeCube;
  ALattice:                            TELDLattice;
  APoint:                              TPoint;
  AFocusType:                          TELDType;
  AFormBackground:                     TStream;
  AFile, APath, AExeName, AVersion:    string;
  // 速度变量
  AFPS_vx, AFPS_vy, AFPS_vz:           Single;
  AFPS_cx, AFPS_cy, AFPS_cz:           Single;
  // 判断用辅助变量
  IsChangingCamera:                    Boolean = False;
  IsCreating:                          Boolean = False;
  IsEditing:                           Boolean = False;
  IsViewing:                           Boolean = False;
  IsMouseDown:                         Boolean = False;
  IsMouseMoved:                        Boolean = False;
  IsOthersHided:                       Boolean = False;
  IsFullScreen:                        Boolean = False;
  g_DnX, g_DnY:                        Single;

implementation

function DealVersion(Ver: string): string;
var
  ss: TArray<string>;
  b, c: string;
  i: Integer;
begin
  ss := Ver.Split(['.']);
  if ss[1].Length = 1 then
    b := '0' + ss[1]
  else
    b := ss[1];
  i := StrToInt(ss[2]);
  if (i < 27) and (i > -1) then
    c := Chr(i + 65)
  else
    c := '';
  Exit(Format('%s.%s%s.%s', [ss[0], b, c, ss[3]]));
end;

function GetVersion(FileName: string): string;
var
  InfoSize, Wnd: DWORD;
  VerBuf: Pointer;
  VerInfo: ^VS_FIXEDFILEINFO;
begin
  InfoSize := GetFileVersionInfoSize(PChar(FileName), Wnd);
  if InfoSize <> 0 then
  begin
    GetMem(VerBuf, InfoSize);
    try
      if GetFileVersionInfo(PChar(FileName), Wnd, InfoSize, VerBuf) then
      begin
        VerInfo := nil;
        VerQueryValue(VerBuf, '\', Pointer(VerInfo), Wnd);
        if VerInfo <> nil then
          Exit(Format('%d.%d.%d.%d', [VerInfo^.dwFileVersionMS shr 16,
            VerInfo^.dwFileVersionMS and $0000FFFF,
            VerInfo^.dwFileVersionLS shr 16, VerInfo^.dwFileVersionLS and
            $0000FFFF]));
      end;
    finally
      FreeMem(VerBuf, InfoSize);
    end;
  end;
  Exit('0.0.0.0');
end;

{$R *.fmx}

procedure TELDForm.ManagerCreatePass(Sender, Control: TObject);
begin
  with TUIPassage(Control) do
  begin
    OnMouseDown := TheMouseDown;
    OnMouseMove := TheMouseMove;
    OnMouseUp := TheMouseUp;
    OnMouseEnter := PassageMouseEnter;
    OnMouseLeave := PassageMouseLeave;
  end;
end;

procedure TELDForm.ManagerEnter(Sender: TObject);
var
  ra: TPoint3D;
begin
  if (MdlCon.Model <> nil) or (LatCon.Lattice <> nil) then
    Exit;
  if (MngCon.Manager <> nil) and (MngCon.Manager <> TUIManager(Sender)) then
    Exit;
  AFocusType := TELDType.Document;
  if TUIManager(Sender) <> nil then
  begin
    AManager := TUIManager(Sender);
    ABackground := TLayer3D(AManager.Parent);
    ABackGround.BringToFront;
  end;
  LenCon.Aim := ABackground;
  LenCon.GetRotationAngle(ra);
  if MngCon.Manager = nil then
  begin
    LenRecord.Position.Point := CamFocus.Position.Point;
    LenRecord.RotationAngle.Y := ra.Y;
    LenRecord.RotationAngle.X := ra.X;
    LenRecord.RotationAngle.Z := ra.Z;
    IsChangingCamera := True;
    MngCon.Manager := AManager;
    LenCon.FinishSender := AManager;
    LenCon.FinishEvent := ManagerLastFinish;
    LenCon.SetLen(ABackground, TheMenu.DIntB.Value, TheMenu.DIntP.Value);
  end
  else
  begin
    with ABackground.RotationAngle do if ((ra - Point).Length < 1E-4) then
    begin
      IsChangingCamera := True;
      LenCon.FinishSender := AManager;
      LenCon.FinishEvent := ManagerLastFinish;
      LenCon.SetLen(ABackground, TheMenu.DIntB.Value, TheMenu.DIntP.Value);
    end
    else
      ManagerLastFinish(nil);
  end;
end;

procedure TELDForm.ManagerKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if not MngCon.Pause then
  begin
    case Key of
      VK_RETURN:
        begin
          if MngCon.Manager <> nil then
          begin
            MngCon.Manager.SetCamera;
            MngCon.CamList.Items.Text := MngCon.Manager.GetCameraNames;
          end;
        end;
      VK_UP:
        begin
          if MngCon.CamList.Count = 0 then
            Exit;
          if MngCon.CamList.ItemIndex > 0 then
            MngCon.CamList.ItemIndex := MngCon.CamList.ItemIndex - 1
          else
            MngCon.CamList.ItemIndex := MngCon.CamList.Items.Count - 1;
          MngCon.CamRename.Text := MngCon.Manager.CamName[MngCon.CamList.ItemIndex];
          MngCon.Manager.ApplyCamera(MngCon.CamList.ItemIndex);
        end;
      VK_DOWN:
        begin
          if MngCon.CamList.Count = 0 then
            Exit;
          if MngCon.CamList.ItemIndex < MngCon.CamList.Items.Count - 1 then
            MngCon.CamList.ItemIndex := MngCon.CamList.ItemIndex + 1
          else
            MngCon.CamList.ItemIndex := 0;
          MngCon.CamRename.Text := MngCon.Manager.CamName[MngCon.CamList.ItemIndex];
          MngCon.Manager.ApplyCamera(MngCon.CamList.ItemIndex);
        end;
    end;
  end
  else
    MngCon.ContinueDeal;
end;

procedure TELDForm.PassageEnter(Sender: TObject);
begin
  if (MdlCon.Model <> nil) or (LatCon.Lattice <> nil) then
    Exit;
  if TUIPassage(Sender).Manager <> nil then
    AManager := TUIPassage(Sender).Manager;
  if (MngCon.Manager <> nil) and (MngCon.Manager <> AManager) then
    Exit;
  if MngCon.Manager = nil then
  begin
    AManager.SetFocus;
    ManagerEnter(AManager)
  end
  else if MngCon.Manager = AManager then
  begin
    TUIPassage(Sender).SetFocus;
    if not IsViewing then
      ShowSideMenu(PsgCon);
    PsgCon.Passage := TUIPassage(Sender);
  end;
end;

procedure TELDForm.PassageMouseEnter(Sender: TObject);
var
  bg: TLayer3D;
  cm: TPoint3D;
  um: TUIManager;
begin
  if IsMouseDown or IsEditing or IsChangingCamera then
    Exit;
  if Assigned(TUIPassage(Sender).Manager) then
    um := TUIPassage(Sender).Manager
  else
    Exit;
  bg := TLayer3D(um.Parent);
  cm := CamFocus.Position.Point + MainCam.Position.Point;
  if bg.Canvas.BeginScene then
  try
    um.Identified := cm.Distance(bg.Position.Point) / 2;
  finally
    bg.Canvas.EndScene;
  end;
end;

procedure TELDForm.PassageMouseLeave(Sender: TObject);
var
  bg: TLayer3D;
  um: TUIManager;
begin
  if IsEditing or IsChangingCamera then
    Exit;
  if Assigned(TUIPassage(Sender).Manager) then
    um := TUIPassage(Sender).Manager
  else
    Exit;
  bg := TLayer3D(um.Parent);
  if bg.Canvas.BeginScene then
  try
    um.Identified := 0;
  finally
    bg.Canvas.EndScene;
  end;
end;

procedure TELDForm.PositionLastFinish(Sender: TObject);
var
  i: Integer;
begin
  IsChangingCamera := False;
  CamCon.ShowPosition(MainCam.RotationAngle.Point, CamFocus.Position.Point, LenCon.FocalDis);
  i := TheMenu.CamList.ItemIndex;
  if Assigned(Sender) and TheMenu.CamInfos[i].IsTransited then
    TButton(Sender).OnClick(Sender);
end;

procedure TELDForm.SaveToFile(FileName: string);
var
  Stream: TStream;
  temp: TMemoryStream;
begin
  AFile := FileName;
  ShowTitle;
  temp := TMemoryStream.Create;
  try
    SaveToStream(temp);
    // 为防止出现错误时存盘损坏文件
    Stream := TFileStream.Create(FileName, fmCreate);
    temp.Position := 0;
    temp.SaveToStream(Stream);
    Stream.Free;
  finally
    temp.Free;
  end;
end;

procedure TELDForm.SaveToStream(Stream: TStream);
var
  bs: TBytes;
  i, c, d: Integer;
  b: Boolean;
  ts: Single;
  ac: Cardinal;
  temp: TMemoryStream;
  Mesh : TMesh;
  s, p: string;
begin
  // 文件校验
  bs := WideBytesOf('ELD_Universe');
  c := Length(bs);
  Stream.Write(c, SizeOf(Integer));
  Stream.WriteBuffer(bs, c);
  // 整体背景
  b := Transparency;
  Stream.Write(b, SizeOf(Boolean));
  with Background.Fill do
  case Kind of
    TBrushKind.Solid:
      begin
        c := 0;
        Stream.Write(c, SizeOf(Integer));
        ac := Color;
        Stream.Write(ac, SizeOf(Cardinal));
      end;
    TBrushKind.Gradient:
      begin
        c := 1;
        Stream.Write(c, SizeOf(Integer));
        c := Gradient.Points.Count;
        Stream.Write(c, SizeOf(Integer));
        for i := 0 to c - 1 do
        begin
          ac := Gradient.Points[i].Color;
          Stream.Write(ac, SizeOf(Cardinal));
          ts := Gradient.Points[i].Offset;
          Stream.Write(ts, SizeOf(Single));
        end;
        case Gradient.Style of
          TGradientStyle.Linear:
            begin
              c := 0;
              Stream.Write(c, SizeOf(Integer));
              ts := Gradient.StartPosition.X;
              Stream.Write(ts, SizeOf(Single));
              ts := Gradient.StartPosition.Y;
              Stream.Write(ts, SizeOf(Single));
              ts := Gradient.StopPosition.X;
              Stream.Write(ts, SizeOf(Single));
              ts := Gradient.StopPosition.Y;
              Stream.Write(ts, SizeOf(Single));
            end;
          TGradientStyle.Radial:
            begin
              c := 1;
              Stream.Write(c, SizeOf(Integer));
              ts := Gradient.RadialTransform.Position.X;
              Stream.Write(ts, SizeOf(Single));
              ts := Gradient.RadialTransform.Position.Y;
              Stream.Write(ts, SizeOf(Single));
            end;
        end;
      end;
    TBrushKind.Bitmap:
      begin
        c := 2;
        Stream.Write(c, SizeOf(Integer));
        temp := TMemoryStream.Create;
        try
          if Assigned(AFormBackground) then
          begin
            AFormBackground.Position := 0;
            temp.LoadFromStream(AFormBackground);
          end
          else
            Bitmap.Bitmap.SaveToStream(temp);
          temp.Position := 0;
          c := temp.Size;
          Stream.Write(c, SizeOf(Integer));
          Stream.Write(temp.Memory^, c);
        finally
          FreeAndNil(temp);
        end;
        case Bitmap.WrapMode of
          TWrapMode.Tile:         c := 0;
          TWrapMode.TileStretch:  c := 1;
        end;
        Stream.Write(c, SizeOf(Integer));
      end;
  end;
  // 储存镜头
  c := TheMenu.CamInfos.Count;
  Stream.Write(c, SizeOf(Integer));
  for i := 0 to c - 1 do
    with TheMenu.CamInfos[i] do
  begin
    b := IsTransited;
    Stream.Write(b, SizeOf(Boolean));
    ts := FocusPoint.X;
    Stream.Write(ts, SizeOf(Single));
    ts := FocusPoint.Y;
    Stream.Write(ts, SizeOf(Single));
    ts := FocusPoint.Z;
    Stream.Write(ts, SizeOf(Single));
    ts := AnglePoint.X;
    Stream.Write(ts, SizeOf(Single));
    ts := AnglePoint.Y;
    Stream.Write(ts, SizeOf(Single));
    ts := AnglePoint.Z;
    Stream.Write(ts, SizeOf(Single));
    ts := FocalDistance;
    Stream.Write(ts, SizeOf(Single));
    ts := IntervalBase;
    Stream.Write(ts, SizeOf(Single));
    ts := IntervalPlus;
    Stream.Write(ts, SizeOf(Single));
  end;
  bs := WideBytesOf(TheMenu.CamList.Items.Text);
  c := Length(bs);
  Stream.Write(c, SizeOf(Integer));
  Stream.WriteBuffer(bs, c);
  // 储存文本框架
  temp := TMemoryStream.Create;
  try
    c := LManager.Count;
    Stream.Write(c, SizeOf(Integer));
    for i := 0 to c - 1 do
      with TLayer3D(LManager[i].Parent) do
    begin
      // 位置和尺寸
      ts := Position.X;
      Stream.Write(ts, SizeOf(Single));
      ts := Position.Y;
      Stream.Write(ts, SizeOf(Single));
      ts := Position.Z;
      Stream.Write(ts, SizeOf(Single));
      ts := RotationAngle.Y;
      Stream.Write(ts, SizeOf(Single));
      ts := RotationAngle.X;
      Stream.Write(ts, SizeOf(Single));
      ts := RotationAngle.Z;
      Stream.Write(ts, SizeOf(Single));
      ts := Width;
      Stream.Write(ts, SizeOf(Single));
      ts := Height;
      Stream.Write(ts, SizeOf(Single));
      // 内容
      temp.Clear;
      LManager[i].SaveToStream(temp);
      temp.Position := 0;
      c := temp.Size;
      Stream.Write(c, SizeOf(Integer));
      Stream.Write(temp.Memory^, c);
    end;
  finally
    FreeAndNil(temp);
  end;
  // 储存模型框架
  c := LModel.Count;
  Stream.Write(c, SizeOf(Integer));
  for i := 0 to c - 1 do
  begin
    // 位置和尺寸
    with TStrokeCube(LModel[i].Parent) do
    begin
      ts := Position.X;
      Stream.Write(ts, SizeOf(Single));
      ts := Position.Y;
      Stream.Write(ts, SizeOf(Single));
      ts := Position.Z;
      Stream.Write(ts, SizeOf(Single));
      ts := RotationAngle.Y;
      Stream.Write(ts, SizeOf(Single));
      ts := RotationAngle.X;
      Stream.Write(ts, SizeOf(Single));
      ts := RotationAngle.Z;
      Stream.Write(ts, SizeOf(Single));
      ts := Width;
      Stream.Write(ts, SizeOf(Single));
      ts := Height;
      Stream.Write(ts, SizeOf(Single));
      ts := Depth;
      Stream.Write(ts, SizeOf(Single));
    end;
    // 路径
    p := GetDirectory(AFile) + PathDelim;
    with LModel[i] do
    begin
      if Hint.StartsWith(p) then
        s := Hint.Replace(p, '')
      else
        s := Hint;
      bs := WideBytesOf(s);
      d := Length(bs);
      Stream.Write(d, SizeOf(Integer));
      Stream.WriteBuffer(bs, d);
      // 材质
      for Mesh in MeshCollection do
      begin
        if Mesh.MaterialSource is TColorMaterialSource then
        begin
          d := 0;
          Stream.Write(d, SizeOf(Integer));
          ac := TColorMaterialSource(Mesh.MaterialSource).Color;
          Stream.Write(ac, SizeOf(Cardinal));
        end
        else if Mesh.MaterialSource is TTextureMaterialSource then
        begin
          d := 1;
          Stream.Write(d, SizeOf(Integer));
          temp := TMemoryStream.Create;
          try
            TTextureMaterialSource(Mesh.MaterialSource).Texture.SaveToStream(temp);
            temp.Position := 0;
            c := temp.Size;
            Stream.Write(c, SizeOf(Integer));
            Stream.Write(temp.Memory^, c);
          finally
            FreeAndNil(temp);
          end;
        end
        else if Mesh.MaterialSource is TLightMaterialSource then
        begin
          d := 2;
          Stream.Write(d, SizeOf(Integer));
          with TLightMaterialSource(Mesh.MaterialSource) do
          begin
            ac := Ambient;
            Stream.Write(ac, SizeOf(Cardinal));
            ac := Diffuse;
            Stream.Write(ac, SizeOf(Cardinal));
            ac := Emissive;
            Stream.Write(ac, SizeOf(Cardinal));
            ac := Specular;
            Stream.Write(ac, SizeOf(Cardinal));
            d := Shininess;
            Stream.Write(d, SizeOf(Integer));
          end;
        end
      end;
    end;
  end;
  // 储存阵列框架
  c := LLattice.Count;
  Stream.Write(c, SizeOf(Integer));
  temp := TMemoryStream.Create;
  try
    for i := 0 to c - 1 do
      with LLattice[i] do
    begin
      // 位置和尺寸
      ts := Contain.Position.X;
      Stream.Write(ts, SizeOf(Single));
      ts := Contain.Position.Y;
      Stream.Write(ts, SizeOf(Single));
      ts := Contain.Position.Z;
      Stream.Write(ts, SizeOf(Single));
      ts := Contain.RotationAngle.Y;
      Stream.Write(ts, SizeOf(Single));
      ts := Contain.RotationAngle.X;
      Stream.Write(ts, SizeOf(Single));
      ts := Contain.RotationAngle.Z;
      Stream.Write(ts, SizeOf(Single));
      ts := Contain.Width;
      Stream.Write(ts, SizeOf(Single));
      ts := Contain.Height;
      Stream.Write(ts, SizeOf(Single));
      ts := Contain.Depth;
      Stream.Write(ts, SizeOf(Single));
      // 点阵信息
      c := Size;
      Stream.Write(c, SizeOf(Integer));
      ts := Radius;
      Stream.Write(ts, SizeOf(Single));
      temp.Clear;
      Strings.SaveToStream(temp);
      temp.Position := 0;
      c := temp.Size;
      Stream.Write(c, SizeOf(Integer));
      Stream.Write(temp.Memory^, c);
      // 材质
      if Material is TColorMaterialSource then
      begin
        d := 0;
        Stream.Write(d, SizeOf(Integer));
        ac := TColorMaterialSource(Material).Color;
        Stream.Write(ac, SizeOf(Cardinal));
      end
      else if Material is TTextureMaterialSource then
      begin
        d := 1;
        Stream.Write(d, SizeOf(Integer));
        temp := TMemoryStream.Create;
        try
          TTextureMaterialSource(Material).Texture.SaveToStream(temp);
          temp.Position := 0;
          c := temp.Size;
          Stream.Write(c, SizeOf(Integer));
          Stream.Write(temp.Memory^, c);
        finally
          FreeAndNil(temp);
        end;
      end
      else if Material is TLightMaterialSource then
      begin
        d := 2;
        Stream.Write(d, SizeOf(Integer));
        with TLightMaterialSource(Material) do
        begin
          ac := Ambient;
          Stream.Write(ac, SizeOf(Cardinal));
          ac := Diffuse;
          Stream.Write(ac, SizeOf(Cardinal));
          ac := Emissive;
          Stream.Write(ac, SizeOf(Cardinal));
          ac := Specular;
          Stream.Write(ac, SizeOf(Cardinal));
          d := Shininess;
          Stream.Write(d, SizeOf(Integer));
        end;
      end
    end;
  finally
    temp.Free;
  end;
end;

procedure TELDForm.ShowHideMainMenu;
begin
  if MainMenu.Visible then
  begin
    MainMenu.Visible := False;
    Focused := nil;
  end
  else
  begin
    MainMenu.Visible := True;
    MainMenu.Position.X := 0.5 * ClientWidth - 0.5 * MainMenu.Width;
    MainMenu.Position.Y := 0.5 * ClientHeight - 0.5 * MainMenu.Height;
    TheMenu.UpdateEffects;
  end;
end;

procedure TELDForm.ShowSideMenu(AMenu: TFrame);
begin
  if not IsViewing then
  begin
    LeftMenu.Visible := True;
    RightMenu.Visible := True;
    BottomMenu.Visible := False;
    PsgCon.Visible := False;
    MngCon.Visible := False;
    MdlCon.Visible := False;
    LatCon.Visible := False;
    AMenu.Visible := True;
    AMenu.Height := LeftMenu.Height - 10;
    LenCon.Height := RightMenu.Height - 10;
  end;
  HideOthers;
end;

procedure TELDForm.ShowTitle;
begin
  if FileExists(AFile) then
    Title.Text := 'ELD ' + AVersion + ' - ' + AFile
  else
    Title.Text := 'ELD ' + AVersion;
end;

procedure TELDForm.SysCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TELDForm.SysFullClick(Sender: TObject);
var
  r : TRect;
begin
  if not IsFullScreen then
  begin
    Left := 0;
    Top := 0;
    Width := Screen.Width;
    Height := Screen.Height;
    TopMenu.Height := 1;
    IsFullScreen := True;
  end
  else
  begin
    SystemParametersInfo(SPI_GETWORKAREA,0,@r,0);
    if SysMax.Text = '|]' then
    begin
      Left := 0;
      Top := 0;
      Width := r.Width;
      Height := r.Height;
    end
    else if SysMax.Text = '□' then
    begin
      Top := r.Height div 2 - 390;
      Left := r.Width div 2 - 600;
      Width := 1200;
      Height := 780;
      if (Width > r.Width) or (Height > r.Height) then
      begin
        Width := r.Width;
        Height := r.Height;
        Top := 0;
        Left := 0;
      end;
    end;
    TopMenu.Height := 30;
    IsFullScreen := False;
  end;
end;

procedure TELDForm.SysMaxClick(Sender: TObject);
var
  r : TRect;
begin
  SystemParametersInfo(SPI_GETWORKAREA,0,@r,0);
  if SysMax.Text = '□' then
  begin
    SysMax.Text := '|]';
    Left := 0;
    Top := 0;
    Width := r.Width;
    Height := r.Height;
  end
  else
  begin
    SysMax.Text := '□';
    Top := r.Height div 2 - 390;
    Left := r.Width div 2 - 600;
    Width := 1200;
    Height := 780;
    if (Width > r.Width) or (Height > r.Height) then
    begin
      Width := r.Width;
      Height := r.Height;
      Top := 0;
      Left := 0;
    end;
  end;
end;
                                                
procedure TELDForm.SysMinClick(Sender: TObject);
begin
  WindowState := TWindowState.wsMinimized;
end;

procedure TELDForm.The3DMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single; RayPos,
  RayDir: TVector3D);
var
  rx, ry, x0, y0, z0, r0: Single;
  p: TPoint;
begin
  GetCursorPos(p);
  g_DnX := p.X;
  g_DnY := p.Y;
  if IsChangingCamera then
  begin
    IsMouseDown := False;
    Exit;
  end;
  GetCursorPos(APoint);
  IsChangingCamera := True;
  IsMouseDown := True;
  IsMouseMoved := False;
  if (Shift = [ssCtrl, ssLeft]) and (IsEditing = False) then
  case TheMenu.CreateType of
    TELDType.Document:
      begin
        IsCreating := True;
        ABackground := TLayer3D.Create(ELDView);
        with ABackground do
        begin
          Parent := ELDView;
          Width := 0.1;
          Height := 0.1;
          Position.X := CamFocus.Position.X;
          Position.Y := CamFocus.Position.Y;
          Position.Z := CamFocus.Position.Z;
          x0 := MainCam.Position.X - CamFocus.Position.X;
          y0 := MainCam.Position.Y - CamFocus.Position.Y;
          z0 := MainCam.Position.Z - CamFocus.Position.Z;
          r0 := Sqrt(x0 * x0 + z0 * z0);
          ry := Arccos(- z0 / r0) * 180 / PI;
          rx := Arccos(- r0 / Sqrt(r0 * r0 + y0 * y0)) * 180 / PI;
          if y0 < 0 then
            rx := 360 - rx;
          if x0 > 0 then
            ry := 360 - ry;
          RotationAngle.X := 270;
          RotationAngle.Z := ry;
          RotationAngle.X := 180 - rx;
          Resolution := 100;
          Transparency := True;
        end;
        LManager.Add(TUIManager.Create(ABackGround));
        AManager := LManager.Last;
        with AManager do
        begin
          Parent := ABackground;
          Cursor := crCross;
          Align := TAlignLayout.Client;
          ReadOnly := True;
          OnClick := ManagerEnter;
          OnCreatePass := ManagerCreatePass;
          OnMouseDown := TheMouseDown;
          OnMouseMove := TheMouseMove;
          OnMouseUp := TheMouseUp;
          OnMouseWheel := ELDViewMouseWheel;
          OnMouseEnter := ManagerMouseEnter;
          OnMouseLeave := ManagerMouseLeave;
          OnKeyDown := ManagerKeyDown;
          OnRemovePass := ManagerEnter;
        end;
        AManager := nil;
      end;
    TELDType.Model:
      begin
        IsCreating := True;
        LModel.Add(TModel3D.Create(ELDView));
        AModel := LModel.Last;
        with AModel do
        begin
          Parent := ELDView;
          Position.Vector := CamFocus.Position.Vector;
          RotationAngle.Z := 180;
        end;
        AModelContain := TStrokeCube.Create(AModel);
        with AModelContain do
        begin
          Parent := AModel;
          RotationAngle.Z := 180;
          Width := 0.1;
          Height := 0.1;
          Depth := 0.1;
          OnMouseDown := The3DMouseDown;
          OnMouseMove := The3DMouseMove;
          OnMouseUp := The3DMouseUp;
          OnMouseWheel := ELDViewMouseWheel;
          OnMouseEnter := ModelMouseEnter;
          OnMouseLeave := ModelMouseLeave;
        end;
      end;
    TELDType.Lattice:
      begin
        IsCreating := True;
        LLattice.Add(TELDLattice.Create(ELDView));
        ALattice := LLattice.Last;
        AModelContain := TStrokeCube.Create(ELDView);
        ALattice.Contain := AModelContain;
        with AModelContain do
        begin
          Parent := ELDView;
          Position.Vector := CamFocus.Position.Vector;
          RotationAngle.Z := 180;
          Width := 0.1;
          Height := 0.1;
          Depth := 0.1;
          OnMouseDown := The3DMouseDown;
          OnMouseMove := The3DMouseMove;
          OnMouseUp := The3DMouseUp;
          OnMouseWheel := ELDViewMouseWheel;
        end;
      end;
  end
  else
    FocusColor.Color := TAlphaColors.White;
end;

procedure TELDForm.The3DMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
var
  p: TPoint;
begin
  GetCursorPos(p);
  if not IsMouseDown then
    Exit;
  if (Abs(g_DnX - p.X) < 1) and (Abs(g_DnY - p.Y) < 1) then
    Exit;
  if not IsMouseMoved then
    IsMouseMoved := True;
  ELDViewMouseMove(Sender, Shift, X, Y);
end;

procedure TELDForm.The3DMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single; RayPos,
  RayDir: TVector3D);
begin
  if not IsMouseDown then
    Exit;
  if not IsMouseMoved then
  begin
    if (Sender is TStrokeCube) then
      if (TStrokeCube(Sender).ChildrenCount > 0) and (TStrokeCube(Sender).Children[0] is TModel3D) then
        ModelEnter(Sender)
      else
        LatticeEnter(Sender);
    FocusColor.Color := TAlphaColors.Null;
    IsChangingCamera := False;
    IsMouseDown := False;
    Exit;
  end;
  ELDViewMouseUp(Sender, Button, Shift, X, Y);
end;

procedure TELDForm.TheMenuBackConChange(Sender: TObject);
begin
  with TheMenu do
  case BackCon.TabIndex  of
    0:
      begin
        Background.Fill.Kind := TBrushKind.Solid;
        ColorSolid.Color := Background.Fill.Color;
      end;
    1:
      begin
        Background.Fill.Kind := TBrushKind.Gradient;
        GradientEdit.Gradient := Background.Fill.Gradient;
      end;
    2:
      begin
        Background.Fill.Kind := TBrushKind.Bitmap;
        if BackGround.Fill.Bitmap.WrapMode = TWrapMode.Tile then
          ImageKind.ItemIndex := 0
        else if BackGround.Fill.Bitmap.WrapMode = TWrapMode.TileStretch then
          ImageKind.ItemIndex := 1
        else
          ImageKind.ItemIndex := 0;
      end;
  end;
end;

procedure TELDForm.TheMenuBtnOverwriteClick(Sender: TObject);
var
  i: Integer;
  ra: TPoint3D;
begin
  with TheMenu do if CamList.ItemIndex > -1 then
  begin
    LenCon.GetRotationAngle(ra);
    i := CamList.ItemIndex;
    CamInfos[i] := TCameraInfo.Create(CamFocus.Position.Point, ra, LenCon.FocalDis, CamInfos[i].IsTransited,
      CamInfos[i].IntervalBase, CamInfos[i].IntervalPlus);
    with CamInfos[CamList.ItemIndex] do
    begin
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
  end;
end;

procedure TELDForm.TheMenuBtnRecordClick(Sender: TObject);
var
  ra: TPoint3D;
begin
  with TheMenu do
  begin
    LenCon.GetRotationAngle(ra);
    CamInfos.Add(TCameraInfo.Create(CamFocus.Position.Point, ra, LenCon.FocalDis,
      False, TheMenu.DIntB.Value, TheMenu.DIntP.Value));
    CamList.Items.Add(Format('新镜头 - %d', [CamList.Items.Count]));
    CamList.ItemIndex := CamList.Items.Count - 1;
    with CamInfos[CamList.ItemIndex] do
    begin
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
  end;
end;

procedure TELDForm.TheMenuCamListClick(Sender: TObject);
begin
  if TheMenu.CamList.ItemIndex > -1 then with TheMenu.CamInfos[TheMenu.CamList.ItemIndex] do
  begin
    LenRecord.Position.Point := FocusPoint;
    LenRecord.RotationAngle.Y := AnglePoint.Y;
    LenRecord.RotationAngle.X := AnglePoint.X;
    LenRecord.RotationAngle.Z := AnglePoint.Z;
    IsChangingCamera := True;
    LenCon.FinishEvent := PositionLastFinish;
    LenCon.SetLen(LenRecord, IntervalBase, IntervalPlus, FocalDistance);
  end;
end;

procedure TELDForm.TheMenuColorSolidChange(Sender: TObject);
begin
  TheMenu.ColorSolidChange(Sender);
  Background.Fill.Color := TheMenu.ColorSolid.Color;
end;

procedure TELDForm.TheMenuCVAngleChange(Sender: TObject);
begin
  MainCam.AngleOfView := TheMenu.CVAngle.Value;
end;

procedure TELDForm.TheMenuFileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TELDForm.TheMenuFileNewClick(Sender: TObject);
begin
  Clear;
  AFile := '';
  ShowTitle;
  ShowHideMainMenu;
end;

procedure TELDForm.TheMenuFileOpenClick(Sender: TObject);
begin
  OpenDialog.Title := '打开演示空间';
  OpenDialog.Filter := 'ELD演示空间(*.eldu)|*.eldu';
  if OpenDialog.Execute then
    LoadFromFile(OpenDialog.FileName)
  else
    Exit;
  AFile := OpenDialog.FileName;
  ShowTitle;
  ShowHideMainMenu;
end;

procedure TELDForm.TheMenuFileSaveAsClick(Sender: TObject);
begin
  SaveDialog.Title := '保存演示空间';
  SaveDialog.Filter := 'ELD演示空间(*.eldu)|*.eldu';
  SaveDialog.DefaultExt := 'eldf';
  if SaveDialog.Execute then
    SaveToFile(SaveDialog.FileName)
  else
    Exit;
  ShowHideMainMenu;
end;

procedure TELDForm.TheMenuFileSaveClick(Sender: TObject);
begin
  if AFile <> '' then
  begin
    SaveToFile(AFile);
    ShowHideMainMenu;
  end
  else
    TheMenuFileSaveAsClick(nil);
end;

procedure TELDForm.TheMenuGradientEditChange(Sender: TObject);
begin
  TheMenu.GradientEditChange(Sender);
  Background.Fill.Gradient := TheMenu.GradientEdit.Gradient;
end;

procedure TELDForm.TheMenuImageKindChange(Sender: TObject);
begin
  case TheMenu.ImageKind.ItemIndex of
    0: Background.Fill.Bitmap.WrapMode := TWrapMode.Tile;
    1: Background.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;   
  end;
end;

procedure TELDForm.TheMenuImageLoadClick(Sender: TObject);
begin
  OpenDialog.Title := '载入图片';
  OpenDialog.Filter := '';
  if OpenDialog.Execute then
  begin
    if Assigned(AFormBackground) then
      AFormBackground.Free;
    AFormBackground := TFileStream.Create(OpenDialog.FileName, fmOpenRead);
  end
  else
    Exit;
  Background.Fill.Bitmap.Bitmap.LoadFromStream(AFormBackground);
end;

procedure TELDForm.TheMenuIsTransparencyChange(Sender: TObject);
begin
  Transparency := TheMenu.IsTransparency.IsChecked;
end;

procedure TELDForm.TheMenuKeyDefaultClick(Sender: TObject);
begin
  if FileExists(APath + '\Config\KeyInfo\Custom.xml') then
    DeleteFile(APath + '\Config\KeyInfo\Custom.xml');
end;

procedure TELDForm.TheMenuKeySaveClick(Sender: TObject);
begin
  TheMenu.SaveKeyConfig(APath + '\Config\KeyInfo\Custom.xml');
end;

procedure TELDForm.TheMenuMenuBackChange(Sender: TObject);
begin
  with Background.Fill do
  if TheMenu.MenuBack.TabIndex = 3 then with TheMenu.BackCon do
  begin
    TabIndex := -1;
    case Kind of
      TBrushKind.Solid: TabIndex := 0;
      TBrushKind.Gradient: TabIndex := 1;
      TBrushKind.Bitmap:
        begin
          TabIndex := 2;
          case Bitmap.WrapMode of
            TWrapMode.Tile: TheMenu.ImageKind.ItemIndex := 0;
            TWrapMode.TileStretch: TheMenu.ImageKind.ItemIndex := 1;
          end;
        end;
    end;
    TheMenu.IsTransparency.IsChecked := Transparency;
  end;
end;

procedure TELDForm.TheMenuNewELTClick(Sender: TObject);
begin
  TheMenu.CreateType := TELDType.Document;
  ShowHideMainMenu;
end;

procedure TELDForm.TheMenuNewLatticeClick(Sender: TObject);
begin
  TheMenu.CreateType := TELDType.Lattice;
  ShowHideMainMenu;
end;

procedure TELDForm.TheMenuNewModelClick(Sender: TObject);
begin
  TheMenu.CreateType := TELDType.Model;
  ShowHideMainMenu;
end;

procedure TELDForm.TheMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  rx, ry, x0, y0, z0, r0: Single;
  p: TPoint;
begin
  GetCursorPos(p);
  g_DnX := p.X;
  g_DnY := p.Y;
  if IsChangingCamera then
  begin
    IsMouseDown := False;
    Exit;
  end;
  if not IsEditing then
    IsChangingCamera := True;
  GetCursorPos(APoint);
  IsMouseDown := True;
  IsMouseMoved := False;
  if (Shift = [ssCtrl, ssLeft]) and (IsEditing = False) then
  case TheMenu.CreateType of
    TELDType.Document:
      begin
        IsCreating := True;
        ABackground := TLayer3D.Create(ELDView);
        with ABackground do
        begin
          Parent := ELDView;
          Width := 0.1;
          Height := 0.1;
          Position.X := CamFocus.Position.X;
          Position.Y := CamFocus.Position.Y;
          Position.Z := CamFocus.Position.Z;
          x0 := MainCam.Position.X - CamFocus.Position.X;
          y0 := MainCam.Position.Y - CamFocus.Position.Y;
          z0 := MainCam.Position.Z - CamFocus.Position.Z;
          r0 := Sqrt(x0 * x0 + z0 * z0);
          ry := Arccos(- z0 / r0) * 180 / PI;
          rx := Arccos(- r0 / Sqrt(r0 * r0 + y0 * y0)) * 180 / PI;
          if y0 < 0 then
            rx := 360 - rx;
          if x0 > 0 then
            ry := 360 - ry;
          RotationAngle.X := 270;
          RotationAngle.Z := ry;
          RotationAngle.X := 180 - rx;
          Resolution := 100;
          Transparency := True;
        end;
        LManager.Add(TUIManager.Create(ABackGround));
        AManager := LManager.Last;
        with AManager do
        begin
          Parent := ABackground;
          Cursor := crCross;
          Align := TAlignLayout.Client;
          ReadOnly := True;
          OnClick := ManagerEnter;
          OnCreatePass := ManagerCreatePass;
          OnMouseDown := TheMouseDown;
          OnMouseMove := TheMouseMove;
          OnMouseUp := TheMouseUp;
          OnMouseWheel := ELDViewMouseWheel;
          OnMouseEnter := ManagerMouseEnter;
          OnMouseLeave := ManagerMouseLeave;
          OnKeyDown := ManagerKeyDown;
          OnRemovePass := ManagerEnter;
        end;
        AManager := nil;
      end;
    TELDType.Model:
      begin
        IsCreating := True;
        LModel.Add(TModel3D.Create(ELDView));
        AModel := LModel.Last;
        with AModel do
        begin
          Parent := ELDView;
          Position.Vector := CamFocus.Position.Vector;
          RotationAngle.Z := 180;
        end;
        AModelContain := TStrokeCube.Create(AModel);
        with AModelContain do
        begin
          Parent := AModel;
          RotationAngle.Z := 180;
          Width := 0.1;
          Height := 0.1;
          Depth := 0.1;
          OnMouseDown := The3DMouseDown;
          OnMouseMove := The3DMouseMove;
          OnMouseUp := The3DMouseUp;
          OnMouseWheel := ELDViewMouseWheel;
          OnMouseEnter := ModelMouseEnter;
          OnMouseLeave := ModelMouseLeave;
        end;
      end;
    TELDType.Lattice:
      begin
        IsCreating := True;
        LLattice.Add(TELDLattice.Create(ELDView));
        ALattice := LLattice.Last;
        AModelContain := TStrokeCube.Create(ELDView);
        ALattice.Contain := AModelContain;
        with AModelContain do
        begin
          Parent := ELDView;
          Position.Vector := CamFocus.Position.Vector;
          RotationAngle.Z := 180;
          Width := 0.1;
          Height := 0.1;
          Depth := 0.1;
          OnMouseDown := The3DMouseDown;
          OnMouseMove := The3DMouseMove;
          OnMouseUp := The3DMouseUp;
          OnMouseWheel := ELDViewMouseWheel;
        end;
      end;
  end
  else if IsEditing = False then
    FocusColor.Color := TAlphaColors.White;
end;

procedure TELDForm.TheMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
var
  p: TPoint;
begin
  GetCursorPos(p);
  if (MngCon.Manager <> nil) or not IsMouseDown then
    Exit;
  if (Abs(g_DnX - p.X) < 1) and (Abs(g_DnY - p.Y) < 1) then
    Exit;
  if not IsMouseMoved then
    IsMouseMoved := True;
  ELDViewMouseMove(Sender, Shift, X, Y);
end;

procedure TELDForm.TheMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if not IsMouseDown then
    Exit;
  if not IsMouseMoved then
  begin
    if Sender is TUIPassage then
      PassageEnter(Sender)
    else if Sender is TUIManager then
      ManagerEnter(Sender);
    FocusColor.Color := TAlphaColors.Null;
    IsChangingCamera := False;
    IsMouseDown := False;
    Exit;
  end;
  if IsEditing then
    Exit;
  ELDViewMouseUp(Sender, Button, Shift, X, Y);
end;

procedure TELDForm.TopMenuMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if SysMax.Text <> '|]' then
    StartWindowDrag;
end;

procedure TELDForm.TopMenuMouseEnter(Sender: TObject);
begin
  if IsFullScreen then
    TopMenu.Height := 30;
end;

procedure TELDForm.UpdateMaterial;
var
  Mesh : TMesh;
begin
  for Mesh in AModel.MeshCollection do
  begin
    if Assigned(Mesh.MaterialSource) then
      Mesh.MaterialSource.Free;
    Mesh.MaterialSource := TLightMaterialSource.Create(AModel);
  end;
end;

procedure TELDForm.ManagerLastFinish(Sender: TObject);
var
  bg: TLayer3D;
  cm: TPoint3D;
  um: TUIManager;
begin
  IsChangingCamera := False;
  CamCon.ShowPosition(MainCam.RotationAngle.Point, CamFocus.Position.Point, LenCon.FocalDis);
  IsEditing := True;
  ShowSideMenu(MngCon);
  if Assigned(Sender) and (Sender is TUIManager) then
  begin
    um := TUIManager(Sender);
    bg := TLayer3D(um.Parent);
    cm := MainCam.Position.Point;
    if bg.Canvas.BeginScene then
    try
      um.Identified := cm.Length / 2;
    finally
      bg.Canvas.EndScene;
    end;
  end;
end;

procedure TELDForm.ManagerMouseEnter(Sender: TObject);
var
  bg: TLayer3D;
  cm: TPoint3D;
  um: TUIManager;
begin
  if IsMouseDown or IsEditing or IsChangingCamera then
    Exit;
  um := TUIManager(Sender);
  bg := TLayer3D(um.Parent);
  cm := CamFocus.Position.Point + MainCam.Position.Point;
  if bg.Canvas.BeginScene then
  try
    um.Identified := cm.Distance(bg.Position.Point) / 2;
  finally
    bg.Canvas.EndScene;
  end;
end;

procedure TELDForm.ManagerMouseLeave(Sender: TObject);
var
  bg: TLayer3D;
begin
  if IsEditing or IsChangingCamera then
    Exit;
  bg := TLayer3D(TUIManager(Sender).Parent);
  if bg.Canvas.BeginScene then
  try
    TUIManager(Sender).Identified := 0;
  finally
    bg.Canvas.EndScene;
  end;
end;

procedure TELDForm.ManagerReturnFinish(Sender: TObject);
begin
  IsChangingCamera := False;
  IsEditing := False;
  if not Assigned(MngCon.Manager) then
    Exit;
  MngCon.Manager := nil;
  Focused := nil;
  AManager := nil;
  ABackground := nil;
  CamCon.ShowPosition(MainCam.RotationAngle.Point, CamFocus.Position.Point, LenCon.FocalDis);
end;

procedure TELDForm.MdlConBtnDoneClick(Sender: TObject);
var
  i: Integer;
begin
  IsChangingCamera := True;
  HideSideMenu;
  if not Assigned(MdlCon.Model) then
    Exit;
  if MdlCon.IsDeleted.IsChecked then
  begin
    MdlCon.Clear;
    for i := 0 to LModel.Count - 1 do if LModel[i] = AModel then
    begin
      LModel.Delete(i);
      MdlCon.Contain.Free;
      Break;
    end;
  end;
  LenCon.FinishEvent := ModelReturnFinish;
  LenCon.SetLen(LenRecord, TheMenu.DIntB.Value, TheMenu.DIntP.Value);
end;

procedure TELDForm.MngConBtnLoadClick(Sender: TObject);
begin
  MngCon.BtnLoadClick(Sender);
  if ABackground.Canvas.BeginScene then
  try
    AManager.Redraw;
  finally
    ABackground.Canvas.EndScene;
  end;
end;

procedure TELDForm.ELDViewKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
var
  ki: TKeyInfo;
  i: Integer;
begin
  ki := TKeyInfo.Create(Key, KeyChar);
  if MainMenu.Visible and (TheMenu.MenuBack.TabIndex = 4) and (TheMenu.KeyList.ItemIndex > -1) and (ki <> TKeyInfo.Zero) then
    with TheMenu do
  begin
    i := KeyList.ItemIndex;
    KeyInfos[KeyList.ItemIndex] := ki;
    KeyList.Items[KeyList.ItemIndex] := Format('%s：[%s]', [KeyNames[i], KeyInfos[i].KeyToStr]);
    KeyList.ItemIndex := -1;
    Exit;
  end;
  if MainMenu.Visible then
  begin
    if ki = TheMenu.KeyInfos[KI_Menu] then
      ShowHideMainMenu;
    Exit;
  end;
  if ki = TheMenu.KeyInfos[KI_MoveForward] then
  begin
    if not IsChangingCamera then
      IsChangingCamera := True;
    if not FPSTimer.Enabled then
      FPSTimer.Enabled := True;
    AFPS_vz := 1;
  end
  else if ki = TheMenu.KeyInfos[KI_MoveBackward] then
  begin
    if not IsChangingCamera then
      IsChangingCamera := True;
    if not FPSTimer.Enabled then
      FPSTimer.Enabled := True;
    AFPS_vz := -1;
  end
  else if ki = TheMenu.KeyInfos[KI_MoveLeft] then
  begin
    if not IsChangingCamera then
      IsChangingCamera := True;
    if not FPSTimer.Enabled then
      FPSTimer.Enabled := True;
    AFPS_vx := -1;
  end
  else if ki = TheMenu.KeyInfos[KI_MoveRight] then
  begin
    if not IsChangingCamera then
      IsChangingCamera := True;
    if not FPSTimer.Enabled then
      FPSTimer.Enabled := True;
    AFPS_vx := 1;
  end
  else if ki = TheMenu.KeyInfos[KI_MoveUp] then
  begin
    if not IsChangingCamera then
      IsChangingCamera := True;
    if not FPSTimer.Enabled then
      FPSTimer.Enabled := True;
    AFPS_vy := -1;
  end
  else if ki = TheMenu.KeyInfos[KI_MoveDown] then
  begin
    if not IsChangingCamera then
      IsChangingCamera := True;
    if not FPSTimer.Enabled then
      FPSTimer.Enabled := True;
    AFPS_vy := 1;
  end
  else if ki = TheMenu.KeyInfos[KI_RotateUp] then
  begin
    if not IsChangingCamera then
      IsChangingCamera := True;
    if not FPSTimer.Enabled then
      FPSTimer.Enabled := True;
    AFPS_cx := 1;
  end
  else if ki = TheMenu.KeyInfos[KI_RotateDown] then
  begin
    if not IsChangingCamera then
      IsChangingCamera := True;
    if not FPSTimer.Enabled then
      FPSTimer.Enabled := True;
    AFPS_cx := -1;
  end
  else if ki = TheMenu.KeyInfos[KI_RotateLeft] then
  begin
    if not IsChangingCamera then
      IsChangingCamera := True;
    if not FPSTimer.Enabled then
      FPSTimer.Enabled := True;
    AFPS_cy := - 1;
  end
  else if ki = TheMenu.KeyInfos[KI_RotateRight] then
  begin
    if not IsChangingCamera then
      IsChangingCamera := True;
    if not FPSTimer.Enabled then
      FPSTimer.Enabled := True;
    AFPS_cy := 1;
  end
  else if ki = TheMenu.KeyInfos[KI_RotateClock] then
  begin
    if not IsChangingCamera then
      IsChangingCamera := True;
    if not FPSTimer.Enabled then
      FPSTimer.Enabled := True;
    AFPS_cz := -1;
  end
  else if ki = TheMenu.KeyInfos[KI_RotateAnti] then
  begin
    if not IsChangingCamera then
      IsChangingCamera := True;
    if not FPSTimer.Enabled then
      FPSTimer.Enabled := True;
    AFPS_cz := 1;
  end;
  if IsChangingCamera then
    Exit;
  if IsEditing then
  begin
    if ki = TheMenu.KeyInfos[KI_Menu] then
    begin
      Focused := nil;
      if MngCon.Manager <> nil then
        ELTBtnDoneClick(nil)
      else if MdlCon.Model <> nil then
        MdlConBtnDoneClick(nil)
      else if LatCon.Lattice <> nil then
        LatConBtnDoneClick(nil);
    end
    else if (ki = TheMenu.KeyInfos[KI_View]) and
      (((MngCon.Manager <> nil) and (MngCon.Manager.ActPassage = nil)) or (MngCon.Manager = nil)) then
    begin
      if IsViewing then
      begin
        IsViewing := False;
        if MngCon.Manager <> nil then
          ShowSideMenu(MngCon)
        else if MdlCon.Model <> nil then
          ShowSideMenu(MdlCon)
        else if LatCon.Lattice <> nil then
          ShowSideMenu(LatCon);
        HideOthers;
      end
      else
      begin
        IsViewing := True;
        HideSideMenu;
        HideNone;
      end;
    end
    else if ((KeyChar = 'z') or (KeyChar = 'Z')) and (Assigned(AManager) and (AManager.ActPassage = nil)) then
    begin
      if Shift = [ssCtrl] then
      begin
        AManager.Undo;
        if ABackground.Canvas.BeginScene then
        try
          AManager.Redraw;
        finally
          ABackground.Canvas.EndScene;
        end;
      end
      else if Shift = [ssCtrl, ssShift] then
      begin
        AManager.Redo;
        if ABackground.Canvas.BeginScene then
        try
          AManager.Redraw;
        finally
          ABackground.Canvas.EndScene;
        end;
      end;
      if Assigned(AManager.OnEnter) then
        AManager.OnEnter(AManager);
    end
  end
  else
  begin
    if ki = TheMenu.KeyInfos[KI_ResetCamera] then
    begin
      if Assigned(CamCon.CConReset.OnClick) then
        CamCon.CConReset.OnClick(CamCon.CConReset);
    end
    else if ki = TheMenu.KeyInfos[KI_NewCamera] then
    begin
      if Assigned(CamCon.CConNew.OnClick) then
        CamCon.CConNew.OnClick(CamCon.CConNew);
    end
    else if ki = TheMenu.KeyInfos[KI_PrevCamera] then
    begin
      if Assigned(CamCon.CConUp.OnClick) then
        CamCon.CConUp.OnClick(CamCon.CConUp);
    end
    else if ki = TheMenu.KeyInfos[KI_NextCamera] then
    begin
      if Assigned(CamCon.CConDown.OnClick) then
        CamCon.CConDown.OnClick(CamCon.CConDown);
    end
    else if ki = TheMenu.KeyInfos[KI_FirstCamera] then
    begin
      if Assigned(CamCon.CConFirst.OnClick) then
        CamCon.CConFirst.OnClick(CamCon.CConFirst);
    end
    else if ki = TheMenu.KeyInfos[KI_LastCamera] then
    begin
      if Assigned(CamCon.CConLast.OnClick) then
        CamCon.CConLast.OnClick(CamCon.CConLast);
    end
    else if ki = TheMenu.KeyInfos[KI_Menu] then
    begin
      ShowHideMainMenu;
    end
    else if ki = TheMenu.KeyInfos[KI_View] then
    begin
      BottomMenu.Visible := IsViewing;
      IsViewing := not IsViewing;
    end;
  end;
end;

procedure TELDForm.ELDViewKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
var
  ki: TKeyInfo;
begin
  ki := TKeyInfo.Create(Key, KeyChar);
  if (ki = TheMenu.KeyInfos[KI_MoveForward]) or (ki = TheMenu.KeyInfos[KI_MoveBackward]) then
    AFPS_vz := 0
  else if (ki = TheMenu.KeyInfos[KI_MoveLeft]) or (ki = TheMenu.KeyInfos[KI_MoveRight]) then
    AFPS_vx := 0
  else if (ki = TheMenu.KeyInfos[KI_MoveUp]) or (ki = TheMenu.KeyInfos[KI_MoveDown]) then
    AFPS_vy := 0
  else if (ki = TheMenu.KeyInfos[KI_RotateUp]) or (ki = TheMenu.KeyInfos[KI_RotateDown]) then
    AFPS_cx := 0
  else if (ki = TheMenu.KeyInfos[KI_RotateLeft]) or (ki = TheMenu.KeyInfos[KI_RotateRight]) then
    AFPS_cy := 0
  else if (ki = TheMenu.KeyInfos[KI_RotateClock]) or (ki = TheMenu.KeyInfos[KI_RotateAnti]) then
    AFPS_cz := 0;
end;

procedure TELDForm.ELDViewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  rx, ry, rz, fd: Single;
begin
  if IsChangingCamera then
  begin
    IsMouseDown := False;
    Exit;
  end;
  Focused := nil;
  IsMouseDown := True;
  IsMouseMoved := False;
  GetCursorPos(APoint);
  IsChangingCamera := True;
  LenCon.GetRotationAngle(rx, ry, rz);
  fd := LenCon.FocalDis;
  CamFocus.Width := Sqrt(fd / 1000);
  CamFocus.Height := Sqrt(fd / 1000);
  CamFocus.Depth := Sqrt(fd / 1000);
  if (Shift = [ssCtrl, ssLeft]) and (IsEditing = False) then
  case TheMenu.CreateType of
    TELDType.Document:
      begin
        IsCreating := True;
        ABackground := TLayer3D.Create(ELDView);
        with ABackground do
        begin
          Parent := ELDView;
          Width := 0.1;
          Height := 0.1;
          Position.Point := CamFocus.Position.Point;
          RotationAngle.Y := ry;
          RotationAngle.X := rx;
          RotationAngle.Z := rz;
          Resolution := 100;
          Transparency := True;
        end;
        LManager.Add(TUIManager.Create(ABackGround));
        AManager := LManager.Last;
        with AManager do
        begin
          Parent := ABackground;
          Align := TAlignLayout.Client;
          ReadOnly := True;
          OnCreatePass := ManagerCreatePass;
          OnMouseDown := TheMouseDown;
          OnMouseMove := TheMouseMove;
          OnMouseUp := TheMouseUp;
          OnMouseWheel := ELDViewMouseWheel;
          OnMouseEnter := ManagerMouseEnter;
          OnMouseLeave := ManagerMouseLeave;
          OnKeyDown := ManagerKeyDown;
          OnRemovePass := ManagerEnter;
        end;
        AManager := nil;
      end;
    TELDType.Model:
      begin
        IsCreating := True;
        AModelContain := TStrokeCube.Create(ELDView);
        with AModelContain do
        begin
          Parent := ELDView;
          Width := 0.1;
          Height := 0.1;
          Depth := 0.1;
          Position.Point := CamFocus.Position.Point;
          RotationAngle.Y := ry;
          RotationAngle.X := rx;
          RotationAngle.Z := rz;
          OnMouseDown := The3DMouseDown;
          OnMouseMove := The3DMouseMove;
          OnMouseUp := The3DMouseUp;
          OnMouseWheel := ELDViewMouseWheel;
          OnMouseEnter := ModelMouseEnter;
          OnMouseLeave := ModelMouseLeave;
        end;
        LModel.Add(TModel3D.Create(ELDView));
        AModel := LModel.Last;
        with AModel do
        begin
          Parent := AModelContain;
          RotationAngle.Z := 180;
        end;
      end;
    TELDType.Lattice:
      begin
        IsCreating := True;
        LLattice.Add(TELDLattice.Create(ELDView));
        ALattice := LLattice.Last;
        AModelContain := TStrokeCube.Create(ELDView);
        ALattice.Contain := AModelContain;
        with AModelContain do
        begin
          Parent := ELDView;
          Position.Point := CamFocus.Position.Point;
          RotationAngle.Y := ry;
          RotationAngle.X := rx;
          RotationAngle.Z := rz;
          OnMouseDown := The3DMouseDown;
          OnMouseMove := The3DMouseMove;
          OnMouseUp := The3DMouseUp;
          OnMouseWheel := ELDViewMouseWheel;
        end;
      end;
  end
  else
    FocusColor.Color := TAlphaColors.White;
end;

procedure TELDForm.ELDViewMouseEnter(Sender: TObject);
begin
  if IsFullScreen then
    TopMenu.Height := 1;
end;

procedure TELDForm.ELDViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
var
  dx, dy, r0: Single;
  p: TPoint;
begin
  CamCon.ShowPosition(MainCam.RotationAngle.Point, CamFocus.Position.Point, LenCon.FocalDis);
  if IsMouseDown then
  begin
    GetCursorPos(p);
    dx := p.X - APoint.X;
    dy := p.Y - APoint.Y;
    GetCursorPos(APoint);
    r0 := LenCon.FocalDis;
    if IsCreating then
    case TheMenu.CreateType of
      TELDType.Document:
        with ABackground do
        begin
          Width := Width + dx * r0 / 500;
          Height := Height + dy * r0 / 500;
          if Width <= 0.1 then
            Width := 0.1;
          if Height <= 0.1 then
            Height := 0.1;
        end;
      TELDType.Model:
        with AModelContain do
        begin
          Width := Width + (dx + dy) * r0 / 1000;
          Height := Height + (dx + dy) * r0 / 1000;
          Depth := Depth + (dx + dy) * r0 / 1000;
          if Width <= 0.1 then
            Width := 0.1;
          if Height <= 0.1 then
            Height := 0.1;
          if Depth <= 0.1 then
            Depth := 0.1;
        end;
      TELDType.Lattice:
        with AModelContain do
        begin
          Width := Width + (dx + dy) * r0 / 1000;
          Height := Height + (dx + dy) * r0 / 1000;
          Depth := Depth + (dx + dy) * r0 / 1000;
          if Width <= 0.1 then
            Width := 0.1;
          if Height <= 0.1 then
            Height := 0.1;
          if Depth <= 0.1 then
            Depth := 0.1;
        end;
    end
    else
    begin
      if ssLeft in Shift then
      begin
        if not (ssCtrl in Shift) then
          LenCon.RotateControl(- dy / 500, dx / 500, 0, False)
        else if IsEditing then
        begin
          if MngCon.Manager <> nil then with TLayer3D(MngCon.Manager.Parent) do
          begin
            Width := Width + dx * r0 / 500;
            Height := Height + dy * r0 / 500;
            if Width <= 0.1 then
              Width := 0.1;
            if Height <= 0.1 then
              Height := 0.1;
            LenCon.FSX.Value := Width;
            LenCon.FSY.Value := Height;
            LenCon.FSZ.Value := Depth;
          end
          else if MdlCon.Model <> nil then with MdlCon.Contain do
          begin
            Width := Width + (dx + dy) * r0 / 1000;
            Height := Height + (dx + dy) * r0 / 1000;
            Depth := Depth + (dx + dy) * r0 / 1000;
            if Width <= 0.1 then
              Width := 0.1;
            if Height <= 0.1 then
              Height := 0.1;
            if Depth <= 0.1 then
              Depth := 0.1;
            MdlCon.Model.Width  := Width;
            MdlCon.Model.Height := Height;
            MdlCon.Model.Depth := Depth;
            LenCon.FSX.Value := Width;
            LenCon.FSY.Value := Height;
            LenCon.FSZ.Value := Depth;
          end
          else if LatCon.Lattice <> nil then with LatCon.Lattice do
          begin
            Contain.Width := Contain.Width + (dx + dy) * r0 / 1000;
            Contain.Height := Contain.Height + (dx + dy) * r0 / 1000;
            Contain.Depth := Contain.Depth + (dx + dy) * r0 / 1000;
            if Contain.Width <= 0.1 then
              Contain.Width := 0.1;
            if Contain.Height <= 0.1 then
              Contain.Height := 0.1;
            if Contain.Depth <= 0.1 then
              Contain.Depth := 0.1;
            ShowDotLattice(Contain);
            LenCon.FSX.Value := Contain.Width;
            LenCon.FSY.Value := Contain.Height;
            LenCon.FSZ.Value := Contain.Depth;
          end;
        end;
      end
      else if ssRight in Shift then
      begin
        if not (ssCtrl in Shift) then
          LenCon.MoveControl(- dx * r0 / 500, - dy * r0 / 500, 0, False)
        else if IsEditing then
        begin
          if (MngCon.Manager <> nil) or (MdlCon.Model <> nil) or (LatCon.Lattice <> nil) then
            LenCon.RotateControl(- dy / 500, dx / 500, 0);
        end;
      end;
      if IsEditing then
      begin
        if IsOthersHided then
          HideNone
        else
      end;
    end;
    if IsEditing and (ssCtrl in Shift) and IsOthersHided then
      HideNone
    else if IsEditing and not ((ssCtrl in Shift) or IsOthersHided) then
      HideOthers;
  end;
end;

procedure TELDForm.ELDViewMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  s: string;
begin
  if not IsMouseDown then
    Exit;
  IsChangingCamera := False;
  CamCon.ShowPosition(MainCam.RotationAngle.Point, CamFocus.Position.Point, LenCon.FocalDis);
  IsMouseDown := False;
  FocusColor.Color := TAlphaColors.Null;
  if IsCreating and (TheMenu.CreateType = TELDType.Model) then
  begin
    OpenDialog.Title := '选择3D模型';
    OpenDialog.Filter := '3D模型（*.ase, *.dae, *.obj）|*.ase;*.dae;*.obj';
    if OpenDialog.Execute then
      s := OpenDialog.FileName
    else
      s := '';
    if s = '' then
    begin
      LModel.Delete(LModel.Count - 1);
      AModelContain.Free;
    end
    else
    begin
      Application.ProcessMessages;
      if AModel.LoadFromFile(s) then
      begin
        with AModel do
        begin
          Width  := AModelContain.Width;
          Height := AModelContain.Height;
          Depth := AModelContain.Depth;
        end;
        AModel.Hint := s;
        UpdateMaterial;
        AModelContain.Color := TAlphaColors.Null;
      end
      else
      begin
        AModelContain.Free;
        LModel.Delete(LModel.Count - 1);
      end;
    end;
    AModel := nil;
    AModelContain := nil;
  end;
  IsCreating := False;
end;

procedure TELDForm.ELDViewMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
var
  r, s: Single;
begin
  if IsChangingCamera and not IsEditing then
    Exit;
  if (Shift = [ssCtrl]) then
  begin
    r := LenCon.FocalDis;
    s := r - WheelDelta / 300;
    if s < 1 then
      s := 1
    else if s > 60 then
      s := 60;
    CamFocus.Width := Sqrt(s / 1000);
    CamFocus.Height := Sqrt(s / 1000);
    CamFocus.Depth := Sqrt(s / 1000);
    LenCon.FocalDis := s;
    CamCon.ShowPosition(MainCam.RotationAngle.Point, CamFocus.Position.Point, LenCon.FocalDis);
  end;
end;

procedure TELDForm.ELTBtnDoneClick(Sender: TObject);
var
  i: Integer;
  o: TFMXObject;
begin
  IsChangingCamera := True;
  HideSideMenu;
  if MngCon.IsDeleted.IsChecked then
  begin
    for i := 0 to LManager.Count - 1 do if LManager[i] = MngCon.Manager then
    begin
      o := MngCon.Manager.Parent;
      LManager.Delete(i);
      o.Free;
      Break;
    end;
    MngCon.IsDeleted.IsChecked := False;
  end
  else if ABackground.Canvas.BeginScene then
  try
    MngCon.Manager.Identified := 0;
  finally
    ABackground.Canvas.EndScene;
  end;
  LenCon.FinishEvent := ManagerReturnFinish;
  LenCon.SetLen(LenRecord, TheMenu.DIntB.Value, TheMenu.DIntP.Value);
end;

procedure TELDForm.ModelEnter(Sender: TObject);
var
  ra: TPoint3D;
begin
  if (MngCon.Manager <> nil) or (LatCon.Lattice <> nil) then
    Exit;
  if (MdlCon.Model <> nil) and (MdlCon.Contain <> TStrokeCube(Sender)) then
    Exit;
  AFocusType := TELDType.Model;
  if TModel3D(TStrokeCube(Sender).Children[0]) <> nil then
  begin
    AModelContain := TStrokeCube(Sender);
    AModel := TModel3D(AModelContain.Children[0]);
  end;
  LenCon.Aim := AModelContain;
  LenCon.GetRotationAngle(ra);
  if MdlCon.Model <> AModel then
  begin
    if MdlCon.Model = nil then
    begin
      LenRecord.Position.Point := CamFocus.Position.Point;
      LenRecord.RotationAngle.Y := ra.Y;
      LenRecord.RotationAngle.X := ra.X;
      LenRecord.RotationAngle.Z := ra.Z;
    end;
    MdlCon.Model := AModel;
    MdlCon.Contain.Color := TAlphaColors.Null;
    IsChangingCamera := True;
    LenCon.FinishEvent := ModelLastFinish;
    LenCon.SetLen(AModelContain, TheMenu.DIntB.Value, TheMenu.DIntP.Value);
  end
  else
  begin
    if ra.Distance(AModelContain.RotationAngle.Point) > 1 then
    begin
      IsChangingCamera := True;
      LenCon.FinishEvent := ModelLastFinish;
      LenCon.SetLen(AModelContain, TheMenu.DIntB.Value, TheMenu.DIntP.Value);
    end
    else
      ModelLastFinish(nil);
  end;
end;

procedure TELDForm.ModelLastFinish(Sender: TObject);
begin
  IsChangingCamera := False;
  CamCon.ShowPosition(MainCam.RotationAngle.Point, CamFocus.Position.Point, LenCon.FocalDis);
  IsEditing := True;
  ShowSideMenu(MdlCon);
end;

procedure TELDForm.ModelMouseEnter(Sender: TObject);
begin
  if IsEditing or IsMouseDown then
    Exit;
  TStrokeCube(Sender).Color := TAlphaColors.White;
end;

procedure TELDForm.ModelMouseLeave(Sender: TObject);
begin
  if IsEditing or IsMouseDown then
    Exit;
  TStrokeCube(Sender).Color := TAlphaColors.Null;
end;

procedure TELDForm.ModelReturnFinish(Sender: TObject);
begin
  IsChangingCamera := False;
  Focused := nil;
  MdlCon.Model := nil;
  AModel := nil;
  AModelContain := nil;
  IsEditing := False;
  CamCon.ShowPosition(MainCam.RotationAngle.Point, CamFocus.Position.Point, LenCon.FocalDis);
end;

procedure TELDForm.LenConLenHideShowClick(Sender: TObject);
begin
  if IsOthersHided then
    HideNone
  else
    HideOthers;
end;

procedure TELDForm.CamConCConResizeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if (SysMax.Text <> '|]') and not IsFullScreen then
    StartWindowResize;
end;

procedure TELDForm.CamConDownClick(Sender: TObject);
var
  i: Integer;
begin
  i := TheMenu.CamList.ItemIndex;
  if i > -1 then
  begin
    Inc(i);
    if i = TheMenu.CamList.Count then
      i := 0;
    if i < TheMenu.CamList.Count - 1 then
      LenCon.FinishSender := Sender;
    with TheMenu.CamInfos[i] do
    begin
      LenRecord.Position.Point := FocusPoint;
      LenRecord.RotationAngle.Y := AnglePoint.Y;
      LenRecord.RotationAngle.X := AnglePoint.X;
      LenRecord.RotationAngle.Z := AnglePoint.Z;
      IsChangingCamera := True;
      LenCon.FinishEvent := PositionLastFinish;
      LenCon.SetLen(LenRecord, IntervalBase, IntervalPlus, FocalDistance);
    end;
    TheMenu.CamList.ItemIndex := i;
  end
  else if TheMenu.CamList.Items.Count > 0 then
  begin
    i := 0;
    with TheMenu.CamInfos[i] do
    begin
      LenRecord.Position.Point := FocusPoint;
      LenRecord.RotationAngle.Y := AnglePoint.Y;
      LenRecord.RotationAngle.X := AnglePoint.X;
      LenRecord.RotationAngle.Z := AnglePoint.Z;
      IsChangingCamera := True;
      LenCon.FinishEvent := PositionLastFinish;
      LenCon.SetLen(LenRecord, IntervalBase, IntervalPlus, FocalDistance);
    end;
    TheMenu.CamList.ItemIndex := i;
  end;
end;

procedure TELDForm.CamConFirstClick(Sender: TObject);
begin
  if TheMenu.CamList.Items.Count > 0 then
  begin
    TheMenu.CamList.ItemIndex := 0;
    with TheMenu.CamInfos[TheMenu.CamList.ItemIndex] do
    begin
      LenRecord.Position.Point := FocusPoint;
      LenRecord.RotationAngle.Y := AnglePoint.Y;
      LenRecord.RotationAngle.X := AnglePoint.X;
      LenRecord.RotationAngle.Z := AnglePoint.Z;
      IsChangingCamera := True;
      LenCon.FinishEvent := PositionLastFinish;
      LenCon.SetLen(LenRecord, IntervalBase, IntervalPlus, FocalDistance);
    end;
  end;
end;

procedure TELDForm.CamConLastClick(Sender: TObject);
begin
  if TheMenu.CamList.Items.Count > 0 then
  begin
    TheMenu.CamList.ItemIndex := TheMenu.CamList.Items.Count - 1;
    with TheMenu.CamInfos[TheMenu.CamList.ItemIndex] do
    begin
      LenRecord.Position.Point := FocusPoint;
      LenRecord.RotationAngle.Y := AnglePoint.Y;
      LenRecord.RotationAngle.X := AnglePoint.X;
      LenRecord.RotationAngle.Z := AnglePoint.Z;
      IsChangingCamera := True;
      LenCon.FinishEvent := PositionLastFinish;
      LenCon.SetLen(LenRecord, IntervalBase, IntervalPlus, FocalDistance);
    end;
  end;
end;

procedure TELDForm.CamConUpClick(Sender: TObject);
var
  i: Integer;
begin
  i := TheMenu.CamList.ItemIndex;
  if i > -1 then
  begin
    Dec(i);
    if i = -1 then
      i := TheMenu.CamList.Count - 1;
    if i > 0 then
      LenCon.FinishSender := Sender;
    with TheMenu.CamInfos[i] do
    begin
      LenRecord.Position.Point := FocusPoint;
      LenRecord.RotationAngle.Y := AnglePoint.Y;
      LenRecord.RotationAngle.X := AnglePoint.X;
      LenRecord.RotationAngle.Z := AnglePoint.Z;
      IsChangingCamera := True;
      LenCon.FinishEvent := PositionLastFinish;
      LenCon.SetLen(LenRecord, IntervalBase, IntervalPlus, FocalDistance);
    end;
    TheMenu.CamList.ItemIndex := i;
  end
  else if TheMenu.CamList.Items.Count > 0 then
  begin
    i := TheMenu.CamList.Count - 1;
    with TheMenu.CamInfos[i] do
    begin
      LenRecord.Position.Point := FocusPoint;
      LenRecord.RotationAngle.Y := AnglePoint.Y;
      LenRecord.RotationAngle.X := AnglePoint.X;
      LenRecord.RotationAngle.Z := AnglePoint.Z;
      IsChangingCamera := True;
      LenCon.FinishSender := Sender;
      LenCon.FinishEvent := PositionLastFinish;
      LenCon.SetLen(LenRecord, IntervalBase, IntervalPlus, FocalDistance);
    end;
    TheMenu.CamList.ItemIndex := i;
  end;
end;

procedure TELDForm.CamConResetClick(Sender: TObject);
begin
  if TheMenu.CamList.ItemIndex > -1 then with TheMenu.CamInfos[TheMenu.CamList.ItemIndex] do
  begin
    LenRecord.Position.Point := FocusPoint;
    LenRecord.RotationAngle.Y := AnglePoint.Y;
    LenRecord.RotationAngle.X := AnglePoint.X;
    LenRecord.RotationAngle.Z := AnglePoint.Z;
    IsChangingCamera := True;
    LenCon.FinishEvent := PositionLastFinish;
    LenCon.SetLen(LenRecord, IntervalBase, IntervalPlus, FocalDistance);
  end
  else
  begin
    LenRecord.Position.Point := TPoint3D.Zero;
    LenRecord.RotationAngle.Point := TPoint3D.Zero;
    LenCon.SetLen(LenRecord, TheMenu.DIntB.Value, TheMenu.DIntP.Value, 20);
  end;
  CamCon.ShowPosition(MainCam.RotationAngle.Point, CamFocus.Position.Point, LenCon.FocalDis);
end;

procedure TELDForm.Clear;
var
  i: Integer;
begin
  for i := 0 to LManager.Count - 1 do
  begin
    ABackground := TLayer3D(LManager.Last.Parent);
    LManager.Delete(LManager.Count - 1);
    ABackground.Free;
  end;
  for i := 0 to LModel.Count - 1 do
  begin
    AModelContain := TStrokeCube(LModel.Last.Parent);
    LModel.Delete(LModel.Count - 1);
    AModelContain.Free;
  end;
  for i := 0 to LLattice.Count - 1 do
  begin
    AModelContain := TStrokeCube(LLattice.Last.Contain);
    LLattice.Delete(LLattice.Count - 1);
    AModelContain.Free;
  end;
  TheMenu.CamInfos.Clear;
  TheMenu.CamList.Items.Clear;
end;

procedure TELDForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Clear;
  if Assigned(LManager) then
    LManager.Free;
  if Assigned(LModel) then
    LModel.Free;
  if Assigned(TheMenu.CamInfos) then
    TheMenu.CamInfos.Free;
  if Assigned(LLattice) then
    LLattice.Free;
end;

procedure TELDForm.FormCreate(Sender: TObject);
var
  r : TRect;
begin
  if FileExists(AExeName) then with TheMenu do
  begin
    APath := GetDirectory(AExeName);
    if FileExists(APath + '\Data\关于.eltp') then
      InfoAbout.LoadFromFile(APath + '\Data\关于.eltp');
    if FileExists(APath + '\Data\文件.eltp') then
      FileHint.LoadFromFile(APath + '\Data\文件.eltp');
    if FileExists(APath + '\Data\创建.eltp') then
      NewHint.LoadFromFile(APath + '\Data\创建.eltp');
    if FileExists(APath + '\Data\背景.eltp') then
      BackHint.LoadFromFile(APath + '\Data\背景.eltp');
    AVersion := DealVersion(GetVersion(AExeName));
  end;
  MainMenu.Visible := False;
  LeftMenu.Visible := False;
  RightMenu.Visible := False;
  BottomMenu.Visible := True;
  CamCon.ShowPosition(MainCam.RotationAngle.Point, CamFocus.Position.Point, LenCon.FocalDis);
  LManager := TObjectList<TUIManager>.Create;
  LModel := TObjectList<TModel3D>.Create;
  LLattice := TObjectList<TELDLattice>.Create;
  ShowTitle;
  LenCon.Camera := MainCam;
  LenCon.Focus := CamFocus;
  LenCon.CamCurX := CamCurX;
  LenCon.CamCurY := CamCurY;
  if not TheMenu.LoadKeyConfig(APath + '\Config\KeyInfo\Custom.xml') then
    TheMenu.LoadKeyConfig(APath + '\Config\KeyInfo\Default.xml');
  if FileExists(AFile) then
    LoadFromFile(AFile)
  else
    AFile := '';
  SystemParametersInfo(SPI_GETWORKAREA,0,@r,0);
  if (Width > r.Width) or (Height > r.Height) then
  begin
    Width := r.Width;
    Height := r.Height;
    Top := 0;
    Left := 0;
  end;
end;

procedure TELDForm.FPSTimerTimer(Sender: TObject);
var
  fd: Single;
begin
  if ((AFPS_vx = 0) and (AFPS_vy = 0) and (AFPS_vz = 0) and (AFPS_cx = 0) and (AFPS_cy = 0) and (AFPS_cz = 0)) then
  begin
    FPSTimer.Enabled := False;
    IsChangingCamera := False;
    Exit;
  end;
  fd := LenCon.FocalDis;
  if IsEditing then
  begin
    if Assigned(MngCon.Manager) and Assigned(MngCon.Manager.ActPassage) and MngCon.Manager.ActPassage.Actived then
      Exit;
    LenCon.MoveControl(TPoint3D.Create(AFPS_vx, AFPS_vy, AFPS_vz) * TheMenu.FPSV.Value * Sqrt(fd) / 10);
    LenCon.RotateControl(TPoint3D.Create(AFPS_cx, AFPS_cy, AFPS_cz) * TheMenu.FPSV.Value * Sqrt(fd) / 100);
    if IsOthersHided then
      HideNone;
  end
  else
  begin
    LenCon.MoveControl(TPoint3D.Create(AFPS_vx, AFPS_vy, AFPS_vz) * TheMenu.FPSV.Value * Sqrt(fd) / 10, False);
    LenCon.RotateControl(TPoint3D.Create(AFPS_cx, AFPS_cy, AFPS_cz) * TheMenu.FPSV.Value * Sqrt(fd) / 100, False);
  end;
  CamCon.ShowPosition(MainCam.RotationAngle.Point, CamFocus.Position.Point, LenCon.FocalDis);
end;

procedure TELDForm.FrameSizeChange(Sender: TObject);
begin
  if IsChangingCamera then
    Exit;
  if Assigned(MngCon.Manager) then
  begin
    with ABackground do
    begin
      Width := LenCon.FSX.Value;
      Height := LenCon.FSY.Value;
      Depth := 0.001;
      LenCon.FSZ.Value := 0.001;
    end;
  end
  else if Assigned(MdlCon.Model) then
  begin
    with AModelContain do
    begin
      Width := LenCon.FSX.Value;
      Height := LenCon.FSY.Value;
      Depth := Lencon.FSZ.Value;
    end;
    with AModel do
    begin
      Width := LenCon.FSX.Value;
      Height := LenCon.FSY.Value;
      Depth := Lencon.FSZ.Value;
    end;
  end
  else if Assigned(LatCon.Lattice) then
  begin
    with AModelContain do
    begin
      Width := LenCon.FSX.Value;
      Height := LenCon.FSY.Value;
      Depth := Lencon.FSZ.Value;
    end;
    LatCon.Lattice.ShowDotLattice(LatCon.Contain);
  end;
end;

function TELDForm.GetFocalDistance: Single;
var
  x, y, z: Single;
begin
  x := MainCam.Position.X - CamFocus.Position.X;
  y := MainCam.Position.Y - CamFocus.Position.Y;
  z := MainCam.Position.Z - CamFocus.Position.Z;
  Exit(Sqrt(x * x + y * y + z * z));
end;

procedure TELDForm.HideNone;
var
  i: Integer;
begin
  for i := 0 to LManager.Count - 1 do
    TLayer3D(LManager[i].Parent).Visible := True;
  for i := 0 to LModel.Count - 1 do
    TStrokeCube(LModel[i].Parent).Visible := True;
  for i := 0 to LLattice.Count - 1 do
    LLattice[i].Visible := True;
  IsOthersHided := False;
end;

procedure TELDForm.HideOthers;
var
  i: Integer;
begin
  for i := 0 to LManager.Count - 1 do
    TLayer3D(LManager[i].Parent).Visible := False;
  for i := 0 to LModel.Count - 1 do
    TStrokeCube(LModel[i].Parent).Visible := False;
  for i := 0 to LLattice.Count - 1 do
    LLattice[i].Visible := False;
  case AFocusType of
    TELDType.Document:
      ABackground.Visible := True;
    TELDType.Model:
      AModelContain.Visible := True;
    TELDType.Lattice:
      ALattice.Visible := True;
  end;
  IsOthersHided := True;
end;

procedure TELDForm.HideSideMenu;
begin
  LeftMenu.Visible := False;
  RightMenu.Visible := False;
  if not IsViewing then
    BottomMenu.Visible := True;
  HideNone;
end;

procedure TELDForm.LatConBtnDoneClick(Sender: TObject);
var
  i: Integer;
begin
  IsChangingCamera := True;
  HideSideMenu;
  if LatCon.IsDeleted.IsChecked then
    for i := 0 to LLattice.Count - 1 do
      if LLattice[i] = LatCon.Lattice then
    begin
      LLattice.Delete(i);
      Break;
    end;
  LenCon.FinishEvent := LatticeReturnFinish;
  LenCon.SetLen(LenRecord, TheMenu.DIntB.Value, TheMenu.DIntP.Value);
end;

procedure TELDForm.LatticeEnter(Sender: TObject);
var
  ra: TPoint3D;
  i: Integer;
begin
  if (MngCon.Manager <> nil) or (MdlCon.Model <> nil) then
    Exit;
  if (LatCon.Lattice <> nil) and (LatCon.Contain <> TStrokeCube(Sender)) then
    Exit;
  AFocusType := TELDType.Lattice;
  AModelContain := TStrokeCube(Sender);
  for i := 0 to LLattice.Count do
  begin
    if i = LLattice.Count then
      Exit;
    if LLattice[i].Contain = AModelContain then
    begin
      ALattice := LLattice[i];
      Break;
    end;
  end;
  LenCon.Aim := AModelContain;
  LenCon.GetRotationAngle(ra);
  if LatCon.Lattice <> ALattice then
  begin
    if LatCon.Lattice = nil then
    begin
      LenCon.GetRotationAngle(ra);
      LenRecord.Position.Point := CamFocus.Position.Point;
      LenRecord.RotationAngle.Y := ra.Y;
      LenRecord.RotationAngle.X := ra.X;
      LenRecord.RotationAngle.Z := ra.Z;
    end;
    IsChangingCamera := True;
    LatCon.Lattice := ALattice;
    LenCon.FinishEvent := LatticeLastFinish;
    LenCon.SetLen(AModelContain, TheMenu.DIntB.Value, TheMenu.DIntP.Value);
  end
  else
  begin
    if ra.Distance(AModelContain.RotationAngle.Point) > 1 then
    begin
      IsChangingCamera := True;
      LenCon.FinishEvent := LatticeLastFinish;
      LenCon.SetLen(AModelContain, TheMenu.DIntB.Value, TheMenu.DIntP.Value);
    end
    else
      LatticeLastFinish(nil);
  end;
end;

procedure TELDForm.LatticeLastFinish(Sender: TObject);
begin
  IsChangingCamera := False;
  CamCon.ShowPosition(MainCam.RotationAngle.Point, CamFocus.Position.Point, LenCon.FocalDis);
  IsEditing := True;
  ShowSideMenu(LatCon);
end;

procedure TELDForm.LatticeReturnFinish(Sender: TObject);
begin
  IsChangingCamera := False;
  Focused := nil;
  LatCon.Lattice := nil;
  ALattice := nil;
  IsEditing := False;
  CamCon.ShowPosition(MainCam.RotationAngle.Point, CamFocus.Position.Point, LenCon.FocalDis);
end;

procedure TELDForm.LoadFromFile(FileName: string);
var
  Stream: TStream;
begin
  AFile := FileName;
  ShowTitle;
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
  if TheMenu.CamList.Items.Count > 0 then with TheMenu.CamInfos[0] do
  begin
    LenRecord.Position.Point := FocusPoint;
    LenRecord.RotationAngle.Y := AnglePoint.Y;
    LenRecord.RotationAngle.X := AnglePoint.X;
    LenRecord.RotationAngle.Z := AnglePoint.Z;
    IsChangingCamera := True;
    LenCon.FinishEvent := PositionLastFinish;
    LenCon.SetLen(LenRecord, IntervalBase, IntervalPlus, FocalDistance);
    TheMenu.CamList.ItemIndex := 0;
  end;
end;

procedure TELDForm.LoadFromStream(Stream: TStream);
var
  bs: TBytes;
  i, c, d, e: Integer;
  b: Boolean;
  ts: Single;
  ac: Cardinal;
  temp: TMemoryStream;
  Mesh : TMesh;
  s: string;
begin
  // 初始化
  Stream.Read(c, SizeOf(Integer));
  SetLength(bs, c);
  Stream.ReadBuffer(bs, c);
  if WideStringOf(bs) <> 'ELD_Universe' then
    Exit;
  Clear;
  // 整体背景
  Stream.Read(b, SizeOf(Boolean));
  Transparency := b;
  Stream.Read(c, SizeOf(Integer));
  TheMenu.BackCon.TabIndex := c;
  with Background.Fill do
  case c of
    0:
      begin
        Kind := TBrushKind.Solid;
        Stream.Read(ac, SizeOf(Cardinal));
        Color := ac;
      end;
    1:
      begin
        Kind := TBrushKind.Gradient;
        Stream.Read(c, SizeOf(Integer));
        Gradient.Points.Clear;
        for i := 0 to c - 1 do
        begin
          Stream.Read(ac, SizeOf(Cardinal));
          Gradient.Points.Add;
          Gradient.Points[i].Color := ac;
          Stream.Read(ts, SizeOf(Single));
          Gradient.Points[i].Offset := ts;
        end;
        Gradient.Change;
        Stream.Read(c, SizeOf(Integer));
        case c of
          0:
            begin
              Gradient.Style := TGradientStyle.Linear;
              Stream.Read(ts, SizeOf(Single));
              Gradient.StartPosition.X := ts;
              Stream.Read(ts, SizeOf(Single));
              Gradient.StartPosition.Y := ts;
              Stream.Read(ts, SizeOf(Single));
              Gradient.StopPosition.X := ts;
              Stream.Read(ts, SizeOf(Single));
              Gradient.StopPosition.Y := ts;
            end;
          1:
            begin
              Gradient.Style := TGradientStyle.Radial;
              Stream.Read(ts, SizeOf(Single));
              Gradient.RadialTransform.Position.X := ts;
              Stream.Read(ts, SizeOf(Single));
              Gradient.RadialTransform.Position.Y := ts;
            end;
        end;
        TheMenu.GradientEdit.Gradient := Gradient;
      end;
    2:
      begin
        Kind := TBrushKind.Bitmap;
        temp := TMemoryStream.Create;
        try
          Stream.Read(c, SizeOf(Integer));
          temp.SetSize(c);
          Stream.ReadBuffer(temp.Memory^, c);
          temp.Position := 0;
          Bitmap.Bitmap.LoadFromStream(temp);
          if Assigned(AFormBackground) then
            AFormBackground.Free;
          AFormBackground := TMemoryStream.Create;
          temp.Position := 0;
          temp.SaveToStream(AFormBackground);
        finally
          temp.Free;
        end;
        Stream.Read(c, SizeOf(Integer));
        case c of
          0: Bitmap.WrapMode := TWrapMode.Tile;
          1: Bitmap.WrapMode := TWrapMode.TileStretch;
        end;
      end;
  end;
  // 读取镜头
  TheMenu.CamInfos.Clear;
  Stream.Read(c, SizeOf(Integer));
  for i := 0 to c - 1 do with TheMenu.CamInfos do
  begin
    Add(TCameraInfo.Create(TPoint3D.Zero, TPoint3D.Zero, 0));
    Stream.Read(b, SizeOf(Boolean));
    List[i].IsTransited := b;
    Stream.Read(ts, SizeOf(Single));
    List[i].FocusPoint.X := ts;
    Stream.Read(ts, SizeOf(Single));
    List[i].FocusPoint.Y := ts;
    Stream.Read(ts, SizeOf(Single));
    List[i].FocusPoint.Z := ts;
    Stream.Read(ts, SizeOf(Single));
    List[i].AnglePoint.X := ts;
    Stream.Read(ts, SizeOf(Single));
    List[i].AnglePoint.Y := ts;
    Stream.Read(ts, SizeOf(Single));
    List[i].AnglePoint.Z := ts;
    Stream.Read(ts, SizeOf(Single));
    List[i].FocalDistance := ts;
    Stream.Read(ts, SizeOf(Single));
    List[i].IntervalBase := ts;
    Stream.Read(ts, SizeOf(Single));
    List[i].IntervalPlus := ts;
  end;
  Stream.Read(c, SizeOf(Integer));
  SetLength(bs, c);
  Stream.ReadBuffer(bs, c);
  TheMenu.CamList.Items.Text := WideStringOf(bs);
  // 读取文本框架
  temp := TMemoryStream.Create;
  try
    Stream.Read(d, SizeOf(Integer));
    for i := 0 to d - 1 do
    begin
      ABackground := TLayer3D.Create(ELDView);
      with ABackground do
      begin
        Parent := ELDView;
        Resolution := 100;
        Transparency := True;
      end;
      LManager.Add(TUIManager.Create(ELDView));
      AManager := LManager.Last;
      with AManager do
      begin
        Parent := ABackground;
        Align := TAlignLayout.Client;
        ReadOnly := True;
        OnCreatePass := ManagerCreatePass;
        OnMouseDown := TheMouseDown;
        OnMouseMove := TheMouseMove;
        OnMouseUp := TheMouseUp;
        OnMouseWheel := ELDViewMouseWheel;
        OnMouseEnter := ManagerMouseEnter;
        OnMouseLeave := ManagerMouseLeave;
        OnKeyDown := ManagerKeyDown;
        OnRemovePass := ManagerEnter;
      end;
      // 位置和尺寸
      with ABackground do
      begin
        Stream.Read(ts, SizeOf(Single));
        Position.X := ts;
        Stream.Read(ts, SizeOf(Single));
        Position.Y := ts;
        Stream.Read(ts, SizeOf(Single));
        Position.Z := ts;
        Stream.Read(ts, SizeOf(Single));
        RotationAngle.Y := ts;
        Stream.Read(ts, SizeOf(Single));
        RotationAngle.X := ts;
        Stream.Read(ts, SizeOf(Single));
        RotationAngle.Z := ts;
        Stream.Read(ts, SizeOf(Single));
        Width := ts;
        Stream.Read(ts, SizeOf(Single));
        Height := ts;
      end;
      // 内容
      temp.Clear;
      Stream.Read(c, SizeOf(Integer));
      temp.SetSize(c);
      Stream.ReadBuffer(temp.Memory^, c);
      temp.Position := 0;
      AManager.LoadFromStream(temp);
      if ABackground.Canvas.BeginScene then
      try
        AManager.Redraw;
      finally
        ABackground.Canvas.EndScene;
      end;
    end;
  finally
    FreeAndNil(temp);
    AManager := nil;
    ABackground := nil;
  end;
  // 读取模型框架
  Stream.Read(d, SizeOf(Integer));
  for i := 0 to d - 1 do
  begin
    AModelContain := TStrokeCube.Create(ELDView);
    with AModelContain do
    begin
      Parent := ELDView;
      Color := TAlphaColors.Null;
      OnMouseDown := The3DMouseDown;
      OnMouseMove := The3DMouseMove;
      OnMouseUp := The3DMouseUp;
      OnMouseWheel := ELDViewMouseWheel;
      OnMouseEnter := ModelMouseEnter;
      OnMouseLeave := ModelMouseLeave;
    end;
    LModel.Add(TModel3D.Create(ELDView));
    AModel := LModel.Last;
    with AModel do
    begin
      Parent := AModelContain;
      RotationAngle.Z := 180;
    end;
    // 位置和尺寸
    with AModelContain do
    begin
      Stream.Read(ts, SizeOf(Single));
      Position.X := ts;
      Stream.Read(ts, SizeOf(Single));
      Position.Y := ts;
      Stream.Read(ts, SizeOf(Single));
      Position.Z := ts;
      Stream.Read(ts, SizeOf(Single));
      RotationAngle.Y := ts;
      Stream.Read(ts, SizeOf(Single));
      RotationAngle.X := ts;
      Stream.Read(ts, SizeOf(Single));
      RotationAngle.Z := ts;
      Stream.Read(ts, SizeOf(Single));
      Width := ts;
      Stream.Read(ts, SizeOf(Single));
      Height := ts;
      Stream.Read(ts, SizeOf(Single));
      Depth := ts;
    end;
    // 路径
    Stream.Read(c, SizeOf(Integer));
    SetLength(bs, c);
    Stream.ReadBuffer(bs, c);
    with AModel do
    begin
      s := WideStringOf(bs);
      if (s.Substring(1, 1) = ':') and FileExists(s) then
      begin
        LoadFromFile(s);
        Hint := s;
      end
      else
      begin
        s := GetDirectory(AFile) + PathDelim + s;
        if FileExists(s) then
        begin
          LoadFromFile(s);
          Hint := s;
        end;
      end;
      Width := ts;
      Height := ts;
      Depth := ts;
    end;
    // 材质
    for Mesh in AModel.MeshCollection do
    begin
      Stream.Read(e, SizeOf(Integer));
      if Assigned(Mesh.MaterialSource) then
        Mesh.MaterialSource.Free;
      case e of
        0:
          begin
            Mesh.MaterialSource := TColorMaterialSource.Create(ELDView);
            Stream.Read(ac, SizeOf(Cardinal));
            TColorMaterialSource(Mesh.MaterialSource).Color := ac;
          end;
        1:
          begin
            Mesh.MaterialSource := TTextureMaterialSource.Create(ELDView);
            temp := TMemoryStream.Create;
            try
              Stream.Read(c, SizeOf(Integer));
              temp.SetSize(c);
              Stream.ReadBuffer(temp.Memory^, c);
              temp.Position := 0;
              TTextureMaterialSource(Mesh.MaterialSource).Texture.LoadFromStream(temp);
            finally
              FreeAndNil(temp);
            end;
          end;
        2:
          begin
            Mesh.MaterialSource := TLightMaterialSource.Create(ELDView);
            with TLightMaterialSource(Mesh.MaterialSource) do
            begin
              Stream.Read(ac, SizeOf(Cardinal));
              Ambient := ac;
              Stream.Read(ac, SizeOf(Cardinal));
              Diffuse := ac;
              Stream.Read(ac, SizeOf(Cardinal));
              Emissive := ac;
              Stream.Read(ac, SizeOf(Cardinal));
              Specular := ac;
              Stream.Read(c, SizeOf(Integer));
              Shininess := c;
            end;
          end;
      end;
    end;
  end;
  AModel := nil;
  AModelContain := nil;
  // 读取阵列框架
  temp := TMemoryStream.Create;
  try
    Stream.Read(d, SizeOf(Integer));
    for i := 0 to d - 1 do
    begin
      LLattice.Add(TELDLattice.Create(ELDView));
      ALattice := LLattice.Last;
      AModelContain := TStrokeCube.Create(ELDView);
      ALattice.Contain := AModelContain;
      with AModelContain do
      begin
        Parent := ELDView;
        OnMouseDown := The3DMouseDown;
        OnMouseMove := The3DMouseMove;
        OnMouseUp := The3DMouseUp;
        OnMouseWheel := ELDViewMouseWheel;
      end;
      // 位置尺寸
      with AModelContain do
      begin
        Stream.Read(ts, SizeOf(Single));
        Position.X := ts;
        Stream.Read(ts, SizeOf(Single));
        Position.Y := ts;
        Stream.Read(ts, SizeOf(Single));
        Position.Z := ts;
        Stream.Read(ts, SizeOf(Single));
        RotationAngle.Y := ts;
        Stream.Read(ts, SizeOf(Single));
        RotationAngle.X := ts;
        Stream.Read(ts, SizeOf(Single));
        RotationAngle.Z := ts;
        Stream.Read(ts, SizeOf(Single));
        Width := ts;
        Stream.Read(ts, SizeOf(Single));
        Height := ts;
        Stream.Read(ts, SizeOf(Single));
        Depth := ts;
      end;
      // 点阵信息
      with ALattice do
      begin
        Stream.Read(c, SizeOf(Integer));
        Size := c;
        Stream.Read(ts, SizeOf(Single));
        Radius := ts;
        temp.Clear;
        Stream.Read(c, SizeOf(Integer));
        temp.SetSize(c);
        Stream.ReadBuffer(temp.Memory^, c);
        temp.Position := 0;
        Strings.LoadFromStream(temp);
        // 材质
        Stream.Read(e, SizeOf(Integer));
        if Assigned(Material) then
          Material.Free;
        case e of
          0:
            begin
              Material := TColorMaterialSource.Create(ELDView);
              Stream.Read(ac, SizeOf(Cardinal));
              TColorMaterialSource(Material).Color := ac;
            end;
          1:
            begin
              Material := TTextureMaterialSource.Create(ELDView);
              temp := TMemoryStream.Create;
              try
                Stream.Read(c, SizeOf(Integer));
                temp.SetSize(c);
                Stream.ReadBuffer(temp.Memory^, c);
                temp.Position := 0;
                TTextureMaterialSource(Material).Texture.LoadFromStream(temp);
              finally
                FreeAndNil(temp);
              end;
            end;
          2:
            begin
              Material := TLightMaterialSource.Create(ELDView);
              with TLightMaterialSource(Material) do
              begin
                Stream.Read(ac, SizeOf(Cardinal));
                Ambient := ac;
                Stream.Read(ac, SizeOf(Cardinal));
                Diffuse := ac;
                Stream.Read(ac, SizeOf(Cardinal));
                Emissive := ac;
                Stream.Read(ac, SizeOf(Cardinal));
                Specular := ac;
                Stream.Read(c, SizeOf(Integer));
                Shininess := c;
              end;
            end;
        end;
        LoadLattice;
        ShowDotLattice(Contain);
      end;
    end;
  finally
    temp.Free;
    ALattice := nil;
    AModelContain := nil;
  end;
end;

end.
