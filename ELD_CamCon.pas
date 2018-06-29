{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{                                                       }
{              ELD Camera Control Frame                 }
{                  ELD 相机控制框架                     }
{                     ELD 1.11                          }
{                                                       }
{    Copyright(c) 2016-2018 Reniasty de El Magnifico    }
{                   天道玄虚 出品                       }
{                 All rights reserved                   }
{                   保留所有权利                        }
{                                                       }
{*******************************************************}

unit ELD_CamCon;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Math.Vectors, System.Math,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Objects;

type
  TCamConMenu = class(TFrame)
    CConReset: TSpeedButton;
    CConNew: TSpeedButton;
    CConUp: TSpeedButton;
    CConDown: TSpeedButton;
    CConPos: TLabel;
    CConFirst: TSpeedButton;
    CConLast: TSpeedButton;
    CConResize: TCircle;
  public
    procedure ShowPosition(cx, cy, cz, fx, fy, fz, fd: Single); overload;
    procedure ShowPosition(cv, fv: TPoint3D; fd: Single); overload;
  end;

implementation

{$R *.fmx}

{ TCamConMenu }

procedure TCamConMenu.ShowPosition(cx, cy, cz, fx, fy, fz, fd: Single);
begin
  CConPos.Text := Format('镜头：(%.1f°, %.1f°, %.1f°)；焦点：(%.1f, %.1f, %.1f)；焦距：%.1f',
    [cx, cy, cz, fx, fy, fz, fd]);
end;

procedure TCamConMenu.ShowPosition(cv, fv: TPoint3D; fd: Single);
begin
  CConPos.Text := Format('镜头：(%.1f°, %.1f°, %.1f°)；焦点：(%.1f, %.1f, %.1f)；焦距：%.1f',
    [cv.X, cv.Y, cv.Z, fv.X, fv.Y, fv.Z, fd]);
end;

end.
