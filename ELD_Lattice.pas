{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{                                                       }
{              ELD Lattice Control Frame                }
{                  ELD 阵列控制框架                     }
{                     ELD 1.11                          }
{                                                       }
{    Copyright(c) 2016-2018 Reniasty de El Magnifico    }
{                   天道玄虚 出品                       }
{                 All rights reserved                   }
{                   保留所有权利                        }
{                                                       }
{*******************************************************}

unit ELD_Lattice;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Math.Vectors, FMX.Objects3D,
  System.Generics.Collections, FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Edit,
  FMX.EditBox, FMX.NumberBox, FMX.Colors, FMX.Controls.Presentation, FMX.TabControl, FMX.Controls3D, FMX.MaterialSources,
  FMX.ScrollBox, FMX.Memo, Xml.XMLDoc, Xml.XMLIntf;

type
  TELDLattice = class;

  TLatticeFrame = class(TFrame)
    GroupMat: TGroupBox;
    MaterialCon: TTabControl;
    ColorItem: TTabItem;
    MatColor: TComboColorBox;
    TextureItem: TTabItem;
    MatTexure: TButton;
    MatTexureClr: TButton;
    LightingItem: TTabItem;
    LigAmbient: TComboColorBox;
    LigLAmbient: TLabel;
    LigLEmissive: TLabel;
    LigEmissive: TComboColorBox;
    LigDiffuse: TComboColorBox;
    LigLDiffuse: TLabel;
    LigLSpecular: TLabel;
    LigSpecular: TComboColorBox;
    LigShininess: TNumberBox;
    LigLShininess: TLabel;
    LigTexture: TButton;
    LigTextureClr: TButton;
    OpenDialog: TOpenDialog;
    GroupMesh: TGroupBox;
    IsDeleted: TCheckBox;
    LatSize: TNumberBox;
    LatLSize: TLabel;
    LatLRadius: TLabel;
    LatRadius: TNumberBox;
    LatInfo: TMemo;
    BtnEnter: TButton;
    BtnDone: TButton;
    BtnSave: TButton;
    BtnLoad: TButton;
    SaveDialog: TSaveDialog;
    procedure MatColorChange(Sender: TObject);
    procedure MaterialConChange(Sender: TObject);
    procedure MatTexureClick(Sender: TObject);
    procedure MatTexureClrClick(Sender: TObject);
    procedure LigAmbientChange(Sender: TObject);
    procedure LigDiffuseChange(Sender: TObject);
    procedure LigEmissiveChange(Sender: TObject);
    procedure LigSpecularChange(Sender: TObject);
    procedure LigShininessChange(Sender: TObject);   
    procedure LigTextureClick(Sender: TObject);
    procedure LigTextureClrClick(Sender: TObject);
    procedure LatSizeChange(Sender: TObject);
    procedure LatRadiusChange(Sender: TObject);
    procedure BtnEnterClick(Sender: TObject);
    procedure BtnLoadClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
  private
    FLattice:           TELDLattice;
    FContain:           TStrokeCube;
    FIsUpdating:        Boolean;
    procedure SetLattice(const Value: TELDLattice);
    procedure ShowMat;
  public
    property Lattice:                  TELDLattice         read FLattice       write SetLattice;
    property Contain:                  TStrokeCube         read FContain;
  end;

  TELDLattice = class
  private
    FLattice:           TList<TPoint3D>;
    FDots:              TObjectList<TSphere>;
    FSize:              Integer;
    FRadius:            Single;
    FOwner:             TComponent;
    FMat:               TMaterialSource;
    FContain:           TControl3D;
    FInfos:             TStrings;
    FVisible:           Boolean;
    function GetCount: Integer;
    function GetLattice(Index: Integer): TPoint3D;
    procedure SetSize(const Value: Integer);
    procedure SetRadius(const Value: Single);
    procedure SetContain(const Value: TControl3D);
    procedure SetVisible(const Value: Boolean);
  protected
    procedure DotsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single; RayPos,
      RayDir: TVector3D);
    procedure DotsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
    procedure DotsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single; RayPos,
      RayDir: TVector3D);
    procedure DotsMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    function LoadLattice: Boolean;
    function LoadFromFile(AFileName: TFileName): Boolean;
    function SaveToFile(AFileName: TFileName): Boolean;
    procedure Clear;
    procedure ShowDotLattice(AContain: TControl3D);
    property Lattice[Index: Integer]:  TPoint3D            read GetLattice;
    property Contain:                  TControl3D          read FContain       write SetContain;
    property Count:                    Integer             read GetCount;
    property Radius:                   Single              read FRadius        write SetRadius;
    property Material:                 TMaterialSource     read FMat           write FMat;
    property Size:                     Integer             read FSize          write SetSize;
    property Strings:                  TStrings            read FInfos;
    property Visible:                  Boolean             read FVisible       write SetVisible;
  end;

implementation

{$R *.fmx}

{ TELDLattice }

procedure TELDLattice.Clear;
begin
  FLattice.Clear;
  FDots.Clear;
end;

constructor TELDLattice.Create(AOwner: TComponent);
begin
  inherited Create;
  FLattice := TList<TPoint3D>.Create;
  FDots := TObjectList<TSphere>.Create;
  FMat := TColorMaterialSource.Create(AOwner);
  FInfos := TStringList.Create;
  FOwner := AOwner;
  FSize := 1;
  FRadius := 0.01;
  FVisible := True;
end;

destructor TELDLattice.Destroy;
begin
  FLattice.Free;
  FDots.Free;
  FMat.Free;
  FInfos.Free;
  inherited;
end;

procedure TELDLattice.DotsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single; RayPos,
  RayDir: TVector3D);
begin
  if Assigned(TSphere(Sender).Parent) and Assigned(TStrokeCube(TSphere(Sender).Parent).OnMouseDown) then
    TStrokeCube(TSphere(Sender).Parent).OnMouseDown(TSphere(Sender).Parent, Button, Shift, X, Y, RayPos, RayDir);
end;

procedure TELDLattice.DotsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
begin
  if Assigned(TSphere(Sender).Parent) and Assigned(TStrokeCube(TSphere(Sender).Parent).OnMouseMove) then
    TStrokeCube(TSphere(Sender).Parent).OnMouseMove(TSphere(Sender).Parent, Shift, X, Y, RayPos, RayDir);
end;

procedure TELDLattice.DotsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single; RayPos,
  RayDir: TVector3D);
begin
  if Assigned(TSphere(Sender).Parent) and Assigned(TStrokeCube(TSphere(Sender).Parent).OnMouseUp) then
    TStrokeCube(TSphere(Sender).Parent).OnMouseUp(TSphere(Sender).Parent, Button, Shift, X, Y, RayPos, RayDir);
end;

procedure TELDLattice.DotsMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  if Assigned(TSphere(Sender).Parent) and Assigned(TStrokeCube(TSphere(Sender).Parent).OnMouseWheel) then
    TStrokeCube(TSphere(Sender).Parent).OnMouseWheel(TSphere(Sender).Parent, Shift, WheelDelta, Handled);
end;

function TELDLattice.GetCount: Integer;
begin
  Exit(FLattice.Count);
end;

function TELDLattice.GetLattice(Index: Integer): TPoint3D;
begin
  if (Index > -1) and (Index < FLattice.Count) then
    Exit(Lattice[Index]);
end;

function TELDLattice.LoadFromFile(AFileName: TFileName): Boolean;
var
  AXMLDoc: TXMLDocument;
  AXMLNode: IXMLNode;
  ks, ksv, ki, kiv: string;
begin
  if not FileExists(AFileName) then
    Exit(False);
  Clear;
  AXMLDoc := TXMLDocument.Create(FOwner);
  try
    AXMLDoc.LoadFromFile(AFileName);
    if AXMLDoc.DocumentElement.NodeName <> 'LatticeInfo' then
      Exit(False);
    AXMLNode := AXMLDoc.DocumentElement;
    ks := AXMLNode.AttributeNodes[0].NodeName;
    ki := AXMLNode.AttributeNodes[1].NodeName;
    ksv := AXMLNode.AttributeNodes[0].Text;
    kiv := AXMLNode.AttributeNodes[1].Text;
    if (ks = 'Radius') and (ki = 'Size') then
    begin
      FRadius := StrToFloat(ksv);
      FSize := StrToInt(kiv);
    end
    else if (ki = 'Radius') and (ks = 'Size') then
    begin
      FRadius := StrToFloat(kiv);
      FSize := StrToInt(ksv);
    end
    else
      Exit(False);
    FInfos.Text := AXMLNode.Text.Replace(#9, '');
    FInfos.Delete(0);
  finally
    AXMLDoc.Free;
  end;
  Exit(True);
end;

function TELDLattice.LoadLattice: Boolean;
var
  s: string;
  ss: TArray<string>;
  i1, i2, i3, j, k: Integer;
  a, b, c, x, y, z: Single;
begin
  if not Assigned(FInfos) then
    Exit(False);
  FLattice.Clear;
  Result := True;
  for j := 0 to FInfos.Count - 1 do
  begin
    s := FInfos[j].Replace(' ', '');
    ss := s.Split([',']);
    if Length(ss) <> 3 then
    begin
      Result := False;
      Break;
    end;
    a := StrToFloat(ss[0]);
    b := StrToFloat(ss[1]);
    c := StrToFloat(ss[2]);
    FInfos[j] := Format('%.3f, %.3f, %.3f', [a, b, c]);
    for i1 := 0 to FSize do
    begin
      x := a + i1;
      if x > FSize then
        Break;
      for i2 := 0 to FSize do
      begin
        y := b + i2;
        if y > FSize then
          Break;
        for i3 := 0 to FSize do
        begin
          z := c + i3;
          if z > FSize then
            Break;
          for k := 0 to FLattice.Count do
          begin
            if k = FLattice.Count then
            begin
              FLattice.Add(TPoint3D.Create(x, y, z));
              Break;
            end;
            if x < FLattice[k].X then
            begin
              FLattice.Insert(k, TPoint3D.Create(x, y, z));
              Break;
            end
            else if x = FLattice[k].X then
            begin
              if y < FLattice[k].Y then
              begin
                FLattice.Insert(k, TPoint3D.Create(x, y, z));
                Break;
              end
              else if y = FLattice[k].Y then
              begin
                if z < FLattice[k].Z then
                begin
                  FLattice.Insert(k, TPoint3D.Create(x, y, z));
                  Break;
                end
                else if z = FLattice[k].Z then
                  Break
                else
                  Continue;
              end
              else
                Continue;
            end
            else
              Continue;
          end;
        end;
      end;
    end;
  end;
  if Result = False then
    FLattice.Clear;
end;

function TELDLattice.SaveToFile(AFileName: TFileName): Boolean;
var
  i: Integer;
  ss: TStringList;
begin
  ss := TStringList.Create;
  try
    ss.Add('<?xml version="1.0" encoding="utf-8"?>');
    ss.Add(Format('<LatticeInfo Radius="%.2f" Size="%d">', [FRadius, FSize]));
    for i := 0 to FInfos.Count - 1 do
      ss.Add(#9 + FInfos[i]);
    ss.Add('</LatticeInfo>');
  finally
    ss.SaveToFile(AFileName, TEncoding.UTF8);
    ss.Free;
  end;
  Exit(True);
end;

procedure TELDLattice.SetContain(const Value: TControl3D);
begin
  if Assigned(Value) then
    FContain := Value;
end;

procedure TELDLattice.SetRadius(const Value: Single);
begin
  if Value > 1 then
    FRadius := 1
  else if Value > 0.01 then
    FRadius := Value
  else
    FRadius := 0.01;
  ShowDotLattice(FContain);
end;

procedure TELDLattice.SetSize(const Value: Integer);
begin
  if Value >= 3 then
    FSize := 3
  else if Value > 1 then
    FSize := Value
  else
    FSize := 1;
  ShowDotLattice(FContain);
end;

procedure TELDLattice.SetVisible(const Value: Boolean);
var
  i: Integer;
begin
  FVisible := Value;
  for i := 0 to FDots.Count - 1 do
    FDots[i].Visible := Value;
  if Assigned(FContain) then
    FContain.Visible := Value;
end;

procedure TELDLattice.ShowDotLattice(AContain: TControl3D);
var
  i: Integer;
  a, b, c, x, y, z: Single;
begin
  if FLattice.Count = 0 then
    Exit;
  if FDots.Count <> FLattice.Count then
  begin
    FDots.Clear;
    for i := 0 to FLattice.Count - 1 do
    begin
      FDots.Add(TSphere.Create(FOwner));
      with FDots.Last do
      begin
        Parent := AContain;
        OnMouseDown := DotsMouseDown;
        OnMouseMove := DotsMouseMove;
        OnMouseUp := DotsMouseUp;
        OnMouseWheel := DotsMouseWheel;
      end;
      if Assigned(FDots.Last.MaterialSource) then
        FDots.Last.MaterialSource.Free;
      FDots.Last.MaterialSource := FMat;
    end;
  end;
  a := AContain.Width;
  b := AContain.Height;
  c := AContain.Depth;
  x := a / FSize;
  y := b / FSize;
  z := c / FSize;
  for i := 0 to FDots.Count - 1 do
  begin
    FDots[i].Position.Point := FLattice[i] * TPoint3D.Create(x, y, z) - TPoint3D.Create(a / 2, b / 2, c / 2);
    FDots[i].Width := FRadius * 2;
    FDots[i].Height := FRadius * 2;
    FDots[i].Depth := FRadius * 2;
  end;
end;

{ TLatticeFrame }

procedure TLatticeFrame.BtnEnterClick(Sender: TObject);
begin
  FLattice.Strings.Text := LatInfo.Lines.Text;
  FLattice.LoadLattice;
  FLattice.ShowDotLattice(FContain);
end;

procedure TLatticeFrame.BtnLoadClick(Sender: TObject);
var
  fn: string;
begin
  OpenDialog.Title := '选择点阵文件';
  OpenDialog.Filter := '点阵文件（*.xml）|*.xml';
  if OpenDialog.Execute then
    fn := OpenDialog.FileName
  else
    fn := '';
  if FileExists(fn) then
    FLattice.LoadFromFile(fn);
  Lattice := FLattice;
  FLattice.LoadLattice;
  FLattice.ShowDotLattice(FContain);
end;

procedure TLatticeFrame.BtnSaveClick(Sender: TObject);
var
  fn: string;
begin
  SaveDialog.Title := '保存为点阵文件';
  SaveDialog.Filter := '点阵文件（*.xml）|*.xml';
  SaveDialog.DefaultExt := 'xml';
  if SaveDialog.Execute then
    fn := SaveDialog.FileName
  else
    fn := '';
  if fn <> '' then
    FLattice.SaveToFile(fn);
end;

procedure TLatticeFrame.LatRadiusChange(Sender: TObject);
begin
  if Assigned(FLattice) then
  begin
    FLattice.FRadius := LatRadius.Value;
    FLattice.ShowDotLattice(FContain);
  end;
end;

procedure TLatticeFrame.LatSizeChange(Sender: TObject);
begin
  if Assigned(FLattice) then
  begin
    FLattice.FSize := Trunc(LatSize.Value);
    FLattice.LoadLattice;
    FLattice.ShowDotLattice(FContain);
  end;
end;

procedure TLatticeFrame.LigAmbientChange(Sender: TObject);
begin
  if FIsUpdating or (FLattice.FMat = nil) then
    Exit;
  FLattice.Visible := False;
  if FLattice.FMat is TLightMaterialSource then
    TLightMaterialSource(FLattice.FMat).Ambient := LigAmbient.Color;
  FLattice.Visible := True;
end;

procedure TLatticeFrame.LigDiffuseChange(Sender: TObject);
begin
  if FIsUpdating or (FLattice.FMat = nil) then
    Exit;
  FLattice.Visible := False;
  if FLattice.FMat is TLightMaterialSource then
    TLightMaterialSource(FLattice.FMat).Diffuse := LigDiffuse.Color;
  FLattice.Visible := True;
end;

procedure TLatticeFrame.LigEmissiveChange(Sender: TObject);
begin
  if FIsUpdating or (FLattice.FMat = nil) then
    Exit;
  FLattice.Visible := False;
  if FLattice.FMat is TLightMaterialSource then
    TLightMaterialSource(FLattice.FMat).Emissive := LigEmissive.Color;
  FLattice.Visible := True;
end;

procedure TLatticeFrame.LigShininessChange(Sender: TObject);
begin
  if FIsUpdating or (FLattice.FMat = nil) then
    Exit;
  FLattice.Visible := False;
  if FLattice.FMat is TLightMaterialSource then
    TLightMaterialSource(FLattice.FMat).Shininess := Trunc(LigShininess.Value);
  FLattice.Visible := True;
end;

procedure TLatticeFrame.LigSpecularChange(Sender: TObject);
begin
  if FIsUpdating or (FLattice.FMat = nil) then
    Exit;
  FLattice.Visible := False;
  if FLattice.FMat is TLightMaterialSource then
    TLightMaterialSource(FLattice.FMat).Specular := LigSpecular.Color;
  FLattice.Visible := True;
end;

procedure TLatticeFrame.LigTextureClick(Sender: TObject);
begin
  if FIsUpdating or (FLattice.FMat = nil) then
    Exit;
  FLattice.Visible := False;
  if FLattice.FMat is TLightMaterialSource then
    if OpenDialog.Execute then
      TLightMaterialSource(FLattice.FMat).Texture.LoadFromFile(OpenDialog.FileName)
    else
      Exit;
  FLattice.Visible := True;
end;

procedure TLatticeFrame.LigTextureClrClick(Sender: TObject);
begin
  if FIsUpdating or (FLattice.FMat = nil) then
    Exit;
  FLattice.Visible := False;
  if FLattice.FMat is TLightMaterialSource then
    TLightMaterialSource(FLattice.FMat).Texture.Clear(TAlphaColors.Null);
  FLattice.Visible := True;
end;

procedure TLatticeFrame.MatColorChange(Sender: TObject);
begin
  if FIsUpdating or (FLattice.FMat = nil) then
    Exit;
  if FLattice.FMat is TColorMaterialSource then
    TColorMaterialSource(FLattice.FMat).Color := MatColor.Color;
end;

procedure TLatticeFrame.MaterialConChange(Sender: TObject);
var
  i: Integer;
begin
  if FIsUpdating or (FLattice.FMat = nil) then
    Exit;
  if Assigned(FLattice.FMat) then
    FLattice.FMat.Free;
  with MaterialCon do
  case TabIndex of
    0: FLattice.FMat := TColorMaterialSource.Create(Self);
    1: FLattice.FMat := TTextureMaterialSource.Create(Self);
    2: FLattice.FMat := TLightMaterialSource.Create(Self);
  end;
  for i := 0 to FLattice.FDots.Count - 1 do
  begin
    if Assigned(FLattice.FDots[i].MaterialSource) then
      FLattice.FDots[i].MaterialSource.Free;
    FLattice.FDots[i].MaterialSource := FLattice.FMat;
  end;
  ShowMat;
  if Assigned(FContain) then
    FLattice.ShowDotLattice(FContain);
end;

procedure TLatticeFrame.MatTexureClick(Sender: TObject);
begin
  if FIsUpdating or (FLattice.FMat = nil) then
    Exit;
  if FLattice.FMat is TTextureMaterialSource then
    if OpenDialog.Execute then
      TTextureMaterialSource(FLattice.FMat).Texture.LoadFromFile(OpenDialog.FileName)
    else
      Exit;
end;

procedure TLatticeFrame.MatTexureClrClick(Sender: TObject);
begin
  if FIsUpdating or (FLattice.FMat = nil) then
    Exit;
  if FLattice.FMat is TTextureMaterialSource then
    TTextureMaterialSource(FLattice.FMat).Texture.Clear(TAlphaColors.Null);
end;

procedure TLatticeFrame.SetLattice(const Value: TELDLattice);
begin
  if not Assigned(Value) then
  begin
    FLattice := nil;
    Exit;
  end;
  IsDeleted.IsChecked := False;
  FLattice := Value;
  if FLattice.Contain is TStrokeCube then
    FContain := TStrokeCube(FLattice.Contain)
  else
    FContain := nil;
  LatInfo.Lines.Text := Lattice.FInfos.Text;
  LatSize.Value := FLattice.FSize;
  LatRadius.Value := Flattice.Radius;
  ShowMat;
end;

procedure TLatticeFrame.ShowMat;
begin
  FIsUpdating := True;    
  try
    if FLattice.FMat is TColorMaterialSource then
    begin
      MaterialCon.TabIndex := 0;
      MatColor.Color := TColorMaterialSource(FLattice.FMat).Color;
    end
    else if FLattice.FMat is TTextureMaterialSource then
    begin
      MaterialCon.TabIndex := 1;
    end
    else if FLattice.FMat is TLightMaterialSource then
    begin
      MaterialCon.TabIndex := 2;
      with TLightMaterialSource(FLattice.FMat) do
      begin
        LigAmbient.Color := Ambient;
        LigDiffuse.Color := Diffuse;
        LigEmissive.Color := Emissive;
        LigSpecular.Color := Specular;
        LigShininess.Value := Shininess;
      end;
    end;
  finally
    FIsUpdating := False;
  end;
end;

end.
