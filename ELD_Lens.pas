{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{                                                       }
{                   ELD Lens Frame                      }
{                  ELD 镜头系统框架                     }
{                     ELD 1.11                          }
{                                                       }
{    Copyright(c) 2016-2018 Reniasty de El Magnifico    }
{                   天道玄虚 出品                       }
{                 All rights reserved                   }
{                   保留所有权利                        }
{                                                       }
{*******************************************************}

unit ELD_Lens;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Math.Vectors, System.Math,
  FMX.Controls3D, FMX.Objects3D, FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit, FMX.EditBox, FMX.NumberBox;

type
  TLensFrame = class(TFrame)
    AniTimer: TTimer;
    GroupPos: TGroupBox;
    FPX: TNumberBox;
    FPY: TNumberBox;
    FPZ: TNumberBox;
    GroupRot: TGroupBox;
    RAX: TNumberBox;
    RAY: TNumberBox;
    RAZ: TNumberBox;
    GroupSize: TGroupBox;
    FSX: TNumberBox;
    FSY: TNumberBox;
    FSZ: TNumberBox;
    procedure AniTimerTimer(Sender: TObject);
    procedure FPChange(Sender: TObject);
    procedure RAChange(Sender: TObject);
  private
    FCamera: TCamera;                                 // 镜头
    FFocus: TSphere;                                  // 焦点
    FAim: TControl3D;                                 // 目标框架
      // 辅助方向为初位置分别位于X轴，Y轴负方向的两个球，随镜头运动，用于辅助定位
    FCamCurX: TSphere;                                // 辅助方向
    FCamCurY: TSphere;                                // 辅助方向
    FFinishEvent: TNotifyEvent;                       // 结束事件
    FFinishSender: TObject;
      // 动画控制
    FAniAim: TControl3D;
    FAniDur: Integer;
    FAniCur: Integer;
    FAniFcd: Single;
    FIsChangingData: Boolean;
    function GetFocalDis: Single;
    procedure SetFocalDis(const Value: Single);
    procedure SetAim(const Value: TControl3D);
  public
    constructor Create(AOwner: TComponent); override;
    function AngleNormalize(Angle: Single): Single; // 角度标准化（-180~180，<>-180）
    function RadianNormalize(Rad: Single): Single;// 弧度标准化（-PI~PI，<>-PI）
    function SameLen(AAim: TControl3D): Boolean;
    procedure LenReset; // 重置镜头到初始值。
    procedure LenCorrect; // 镜头校正
    procedure MoveControl(dx, dy, dz: Single; const FrameEnabled: Boolean = True); overload;
    procedure MoveControl(po: TPoint3D; const FrameEnabled: Boolean = True); overload;
      // X正向向右，Y正向向下，Z正向向前，仅调整焦点位置
    procedure RotateControl(ax, ay, az: Single; const FrameEnabled: Boolean = True); overload;
    procedure RotateControl(ra: TPoint3D; const FrameEnabled: Boolean = True); overload;
      // 改变镜头相对位置调整旋转角，弧度单位，方向分别为0-Z，1-X，2-Y，依照YXZ顺序处理
    procedure GetRotationAngle(var Rx, Ry, Rz: Single); overload;
    procedure GetRotationAngle(var Rp: TPoint3D); overload;
      // 获取镜头对应的旋转角，角度单位，以先Y后X最后Z的顺序旋转。
    procedure SetLen(DurB: Single = 1; DurP: Single = 0; FcdE: Single = 0); overload;
    procedure SetLen(AAim: TControl3D; DurB: Single = 1; DurP: Single = 0; FcdE: Single = 0); overload;
    property Camera:         TCamera        read FCamera        write FCamera;
    property CamCurX:        TSphere        read FCamCurX       write FCamCurX;
    property CamCurY:        TSphere        read FCamCurY       write FCamCurY;
    property Focus:          TSphere        read FFocus         write FFocus;
    property Aim:            TControl3D     read FAim           write SetAim;
    property FocalDis:       Single         read GetFocalDis    write SetFocalDis;
    property FinishEvent:    TNotifyEvent   read FFinishEvent   write FFinishEvent;
    property FinishSender:   TObject        read FFinishSender  write FFinishSender;
    property AniAim:         TControl3D     read FAniAim        write FAniAim;
    property AniDur:         Integer        read FAniDur        write FAniDur;
    property AniCur:         Integer        read FAniCur        write FAniCur;
    property AniFcd:         Single         read FAniFcd        write FAniFcd;
  end;

implementation

{$R *.fmx}

function TLensFrame.AngleNormalize(Angle: Single): Single;
var
  e: Integer;
  d: Single;
begin
  if Angle >= 0 then
  begin
    // 分别取整数小数部分
    e := Trunc(Angle);
    d := Angle - e;
    e := e mod 360;
    Angle := d + e;
    if Angle > 180 then
      Exit(Angle - 360)
    else
      Exit(Angle);
  end
  else
  begin
    // 取为相反数
    Angle := - Angle;
    // 分别取整数和小数部分
    e := Trunc(Angle);
    d := Angle - e;
    e := e mod 360;
    Angle := 360 - d - e;
    if Angle > 180 then
      Exit(Angle - 360)
    else
      Exit(Angle);
  end;
end;

procedure TLensFrame.AniTimerTimer(Sender: TObject);
var
  fd: Single;
  r, a, t: TPoint3D;
  d: Integer;
  fe: TNotifyEvent;
  fs: TObject;
begin
  fd := GetFocalDis;
  if AniCur <= AniDur then
  begin
    d := AniDur - AniCur + 1;
    if AniFcd > 1 then
    begin
      fd := fd + (AniFcd - fd) / d;
      SetFocalDis(fd);
    end;
    GetRotationAngle(a);
    t := AniAim.RotationAngle.Point - a;
    t.X := AngleNormalize(t.X);
    t.Y := AngleNormalize(t.Y);
    t.Z := AngleNormalize(t.Z);
    r := a + t / d;
    FFocus.Position.Point := FFocus.Position.Point + (AniAim.Position.Point - FFocus.Position.Point) / d;
    FCamera.RotationAngle.Point := TPoint3D.Zero;
    FCamCurX.Position.Point := TPoint3D.Create(-1, 0, 0);
    FCamCurY.Position.Point := TPoint3D.Create(0, -1, 0);
    FCamera.Position.Point := TPoint3D.Create(0, 0, - fd);
    RotateControl(r.X / 180 * PI, r.Y / 180 * PI, r.Z / 180 * PI, False);
  end
  else
  begin
    AniAim := nil;
    AniDur := 0;
    AniCur := 0;
    AniTimer.Enabled := False;
    if Assigned(FFinishEvent) then
    begin
      fe := FFinishEvent;
      fs := FFinishSender;
      FFinishEvent := nil;
      FFinishSender := nil;
      fe(fs);
    end;
  end;
  AniCur := AniCur + 1;
end;

constructor TLensFrame.Create(AOwner: TComponent);
begin
  inherited;
  FAniAim := nil;
  FAniDur := 0;
  FAniCur := 0;
end;

procedure TLensFrame.FPChange(Sender: TObject);
begin
  if FIsChangingData or not Assigned(FAim) then
    Exit;
  with FAim.Position do
  begin
    X := FPX.Value;
    Y := FPY.Value;
    Z := FPZ.Value;
  end;
end;

function TLensFrame.GetFocalDis: Single;
var
  r: Single;
begin
  if not (Assigned(FFocus) and Assigned(FCamera)) then
    Exit(0);
  r := FCamera.Position.Point.Length;
  if r = NaN then
    FCamera.Position.Point := TPoint3D.Create(0, 0, -20);
  Exit(r);
end;

procedure TLensFrame.GetRotationAngle(var Rp: TPoint3D);
var
  x, y, z: Single;
begin
  GetRotationAngle(x,y, z);
  Rp := TPoint3D.Create(x, y, z);
end;

procedure TLensFrame.GetRotationAngle(var Rx, Ry, Rz: Single);
var
  Bx, By, Bz: TPoint3D; // 基准方向
  Cv, Cw: TPoint3D;     // 辅助向量
  px, py: Single;       // 辅助变量
begin
  if not (Assigned(FCamera) and Assigned(FFocus) and Assigned(FCamCurX) and Assigned(FCamCurY)) then
    Exit;
  // 获取基准位置
  Bz := - FCamera.Position.Point.Normalize;
  By := - FCamCurY.Position.Point.Normalize;
  Bx := - FCamCurX.Position.Point.Normalize;
  if Abs(Bx.DotProduct(By.CrossProduct(Bz)) - 1) > 1E-6 then
  begin
    Bx := By.CrossProduct(Bz).Normalize;
    By := Bz.CrossProduct(Bx).Normalize;
    Bz := Bx.CrossProduct(By).Normalize;
  end;
  // 计算镜头俯仰
  px := Bz.DotProduct(TPoint3D.Create(0, 1, 0));
  if px >= 1 then
    Rx := -0.5 * PI
  else if px <= -1 then
    Rx := 0.5 * PI
  else
    Rx := - ArcSin(px);
  // 计算镜头方位
  Cv := TPoint3D.Create(Bz.X, 0, Bz.Z).Normalize;
  px := Cv.DotProduct(TPoint3D.Create(1, 0, 0));
  py := Cv.DotProduct(TPoint3D.Create(0, 0, 1));
  if SameValue(Rx, 0.5 * PI, 1E-4) or SameValue(Rx, -0.5 * PI, 1E-4) then
    Ry := 0
  else if py >= 1 then
    Ry := 0
  else if py <= -1 then
    Ry := PI
  else if px < 0 then
    Ry := - ArcCos(py)
  else
    Ry := ArcCos(py);
  // 计算镜头滚转
  Cv := TPoint3D.Create(Cos(Ry), 0, - Sin(Ry));
  Cw := TPoint3D.Create(Sin(Rx) * Sin(Ry), Cos(Rx), Sin(Rx) * Cos(Ry));
  px := Cv.DotProduct(By);
  py := Cw.DotProduct(By);
  if py >= 1 then
    Rz := 0
  else if py <= -1 then
    Rz := PI
  else if px < 0 then
    Rz := ArcCos(py)
  else
    Rz := - ArcCos(py);
  // 化为角度
  Rx := AngleNormalize(Rx * 180 / PI);
  Ry := AngleNormalize(Ry * 180 / PI);
  Rz := AngleNormalize(Rz * 180 / PI);
end;

procedure TLensFrame.LenCorrect;
var
  Bx, By, Bz: TPoint3D; // 基准方向、
begin
  Bz := - FCamera.Position.Point.Normalize;
  Bx := - FCamCurX.Position.Point.Normalize;
  By := - FCamCurY.Position.Point.Normalize;
  Bx := By.CrossProduct(Bz).Normalize;
  By := Bz.CrossProduct(Bx).Normalize;
  FCamCurX.Position.Point := - Bx;
  FCamCurY.Position.Point := - By;
end;

procedure TLensFrame.LenReset;
begin
  if not (Assigned(FCamera) and Assigned(FFocus) and Assigned(FCamCurX) and Assigned(FCamCurY)) then
    Exit;
  FCamera.Position.Point := TPoint3D.Create(0, 0, -20);
  FCamCurY.Position.Point := TPoint3D.Create(0, -1, 0);
  FCamCurX.Position.Point := TPoint3D.Create(-1, 0, 0);
end;

procedure TLensFrame.MoveControl(po: TPoint3D; const FrameEnabled: Boolean);
begin
  MoveControl(po.X, po.Y, po.Z, FrameEnabled);
end;

procedure TLensFrame.MoveControl(dx, dy, dz: Single; const FrameEnabled: Boolean);
var
  Bx, By, Bz: TPoint3D; // 基准方向
begin
  if not (Assigned(FCamera) and Assigned(FFocus) and Assigned(FCamCurX) and Assigned(FCamCurY)) then
    Exit;
  // 获取基准位置
  with FCamera.Position do
    Bz := - TPoint3D.Create(X, Y, Z).Normalize;
  with FCamCurY.Position do
    By := - TPoint3D.Create(X, Y, Z).Normalize;
  with FCamCurX.Position do
    Bx := - TPoint3D.Create(X, Y, Z).Normalize;
  // 移动焦点
  with FFocus.Position do
    Point := Point + dx * Bx + dy * By + dz * Bz;
  // 移动框架
  if FrameEnabled and Assigned(FAim) then with FAim.Position do
  begin
    Point := Point + dx * Bx + dy * By + dz * Bz;
    FIsChangingData := True;
    try
      FPX.Value := X;
      FPY.Value := Y;
      FPZ.Value := Z;
    finally
      FIsChangingData := False;
    end;
  end;
end;

procedure TLensFrame.RAChange(Sender: TObject);
begin
  if FIsChangingData or not Assigned(FAim) then
    Exit;
  with FAim.RotationAngle do
  begin
    Point := TPoint3D.Zero;
    Y := RAY.Value;
    X := RAX.Value;
    Z := RAZ.Value;
  end;
end;

function TLensFrame.RadianNormalize(Rad: Single): Single;
begin
  Exit(AngleNormalize(Rad * 180 / PI) * PI / 180);
end;

procedure TLensFrame.RotateControl(ra: TPoint3D; const FrameEnabled: Boolean);
begin
  RotateControl(ra.X, ra.Y, ra.Z, FrameEnabled);
end;

procedure TLensFrame.RotateControl(ax, ay, az: Single; const FrameEnabled: Boolean);
var
  fd: Single;           // 辅助变量
  Bx, By, Bz: TPoint3D; // 基准方向
  rx, ry, rz: Single;   // 转角变量
  tp, tq: TPoint3D;     // 辅助变量
begin
  if not (Assigned(FCamera) and Assigned(FFocus) and Assigned(FCamCurX) and Assigned(FCamCurY)) then
    Exit;
  fd := GetFocalDis;
  ax := RadianNormalize(ax);
  ay := RadianNormalize(ay);
  az := RadianNormalize(az);
  // 获取基准位置
  Bz := - FCamera.Position.Point.Normalize;
  By := - FCamCurY.Position.Point.Normalize;
  Bx := - FCamCurX.Position.Point.Normalize;
  if Abs(Bx.DotProduct(By.CrossProduct(Bz)) - 1) > 1E-6 then
  begin
    Bx := By.CrossProduct(Bz).Normalize;
    By := Bz.CrossProduct(Bx).Normalize;
    Bz := Bx.CrossProduct(By).Normalize;
  end;
  if ay <> 0 then // 处理旋转，绕Y
  begin
    tp := Cos(ay) * Bz + Sin(ay) * Bx;
    tq := Cos(ay) * Bx - Sin(ay) * Bz;
    Bz := tp.Normalize;
    Bx := tq.Normalize;
  end;
  if Abs(Bx.DotProduct(By.CrossProduct(Bz)) - 1) > 1E-6 then
  begin
    Bx := By.CrossProduct(Bz).Normalize;
    By := Bz.CrossProduct(Bx).Normalize;
    Bz := Bx.CrossProduct(By).Normalize;
  end;
  if ax <> 0 then // 处理旋转，绕X
  begin
    tp := Cos(ax) * By + Sin(ax) * Bz;
    tq := Cos(ax) * Bz - Sin(ax) * By;
    By := tp.Normalize;
    Bz := tq.Normalize;
  end;
  if Abs(Bx.DotProduct(By.CrossProduct(Bz)) - 1) > 1E-6 then
  begin
    Bx := By.CrossProduct(Bz).Normalize;
    By := Bz.CrossProduct(Bx).Normalize;
    Bz := Bx.CrossProduct(By).Normalize;
  end;
  if az <> 0 then // 处理旋转，绕Z
  begin
    tp := Cos(az) * Bx + Sin(az) * By;
    tq := Cos(az) * By - Sin(az) * Bx;
    Bx := tp.Normalize;
    By := tq.Normalize;
  end;
  if Abs(Bx.DotProduct(By.CrossProduct(Bz)) - 1) > 1E-6 then
  begin
    Bx := By.CrossProduct(Bz).Normalize;
    By := Bz.CrossProduct(Bx).Normalize;
    Bz := Bx.CrossProduct(By).Normalize;
  end;
  FCamera.Position.Point := - fd * Bz;
  FCamCurY.Position.Point := - By;
  FCamCurX.Position.Point := - Bx;
  // 计算镜头转角
  GetRotationAngle(rx, ry, rz);
  with FCamera.RotationAngle do
  begin
    Point := TPoint3D.Zero;
    Y := ry;
    X := rx;
    Z := rz;
  end;
  if FrameEnabled and Assigned(FAim) then with FAim.RotationAngle do
  begin
    Point := TPoint3D.Zero;
    Y := ry;
    X := rx;
    Z := rz;
    FIsChangingData := True;
    try
      RAX.Value := X;
      RAY.Value := Y;
      RAZ.Value := Z;
    finally
      FIsChangingData := False;
    end;
  end;
end;

function TLensFrame.SameLen(AAim: TControl3D): Boolean;
begin
  Exit(((AAim.RotationAngle.Point - FCamera.RotationAngle.Point).Length < 1E-4)
    and ((AAim.Position.Point - FFocus.Position.Point).Length < 1E-4));
end;

procedure TLensFrame.SetAim(const Value: TControl3D);
begin
  FAim := Value;
  if not Assigned(FAim) then
    Exit;
  FIsChangingData := True;
  try
    with FAim.Position do
    begin
      FPX.Value := X;
      FPY.Value := Y;
      FPZ.Value := Z;
    end;
    with FAim.RotationAngle do
    begin
      RAX.Value := X;
      RAY.Value := Y;
      RAZ.Value := Z;
    end;
    with FAim do
    begin
      FSX.Value := Width;
      FSY.Value := Height;
      FSZ.Value := Depth;
    end;
  finally
    FIsChangingData := False;
  end;
end;

procedure TLensFrame.SetFocalDis(const Value: Single);
begin
  with FCamera.Position do
    if Value <= 1 then
      Point := Point.Normalize
    else
      Point := Point.Normalize * Value;
end;

procedure TLensFrame.SetLen(AAim: TControl3D; DurB, DurP, FcdE: Single);
var
  fd: Single;
  fe: TNotifyEvent;
  fs: TObject;
begin
  if not (Assigned(FCamera) and Assigned(FFocus) and Assigned(FCamCurX) and Assigned(FCamCurY)) then
    Exit;
  if SameLen(AAim) then
  begin
    if Assigned(FFinishEvent) then
    begin
      fe := FFinishEvent;
      fs := FFinishSender;
      FFinishEvent := nil;
      FFinishSender := nil;
      fe(fs);
    end;
    Exit;
  end;
  AniAim := AAim;
  AniCur := 0;
  fd := GetFocalDis;
  if FcdE > 1 then
    AniFcd := FcdE
  else
    AniFcd := 0;
  AniDur := Trunc((DurB + DurP * (FCamera.RotationAngle.Point.Distance(AAim.RotationAngle.Point) / 360 * PI
    + Focus.Position.Point.Distance(AAim.Position.Point)) / (fd + AniFcd)) / 0.02);
  if AniDur < 1 then
    AniDur := 1;
  AniTimer.Enabled := True;
end;

procedure TLensFrame.SetLen(DurB, DurP, FcdE: Single);
begin
  SetLen(FAim, DurB, DurP, FcdE);
end;

end.
