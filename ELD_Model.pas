{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{                                                       }
{               ELD Model Control Frame                 }
{                  ELD 模型控制框架                     }
{                     ELD 1.11                          }
{                                                       }
{    Copyright(c) 2016-2018 Reniasty de El Magnifico    }
{                   天道玄虚 出品                       }
{                 All rights reserved                   }
{                   保留所有权利                        }
{                                                       }
{*******************************************************}

unit ELD_Model;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Generics.Collections,
  FMX.Objects3D, FMX.MaterialSources, FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TabControl, FMX.Controls.Presentation, FMX.Colors, FMX.Edit, FMX.EditBox, FMX.NumberBox, FMX.Layouts, FMX.ListBox;

type
  TModelFrame = class(TFrame)
    OpenDialog: TOpenDialog;
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
    GroupMesh: TGroupBox;
    MeshList: TListBox;
    BtnLoad: TButton;
    BtnDone: TButton;
    IsDeleted: TCheckBox;
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
    procedure BtnLoadClick(Sender: TObject);
    procedure MeshListChange(Sender: TObject);
  private
    FModel: TModel3D;
    FContain: TStrokeCube;
    FIsUpdating: Boolean;
    procedure ShowMesh;
    procedure SetModel(const Value: TModel3D);
    procedure ClearMeshes(var Model3D: TModel3D);
  public
    procedure Clear;
    property Model:               TModel3D       read FModel              write SetModel;
    property Contain:             TStrokeCube    read FContain;
  end;

implementation

{$R *.fmx}

{ TModelFrame }

procedure TModelFrame.BtnLoadClick(Sender: TObject);
var
  fn: string;
begin
  OpenDialog.Title := '选择3D模型';
  OpenDialog.Filter := '3D模型（*.dae, *.obj, *.ase）|*.dae;*.obj;*.ase';
  if OpenDialog.Execute then
    fn := OpenDialog.FileName
  else
    fn := '';
  if FileExists(fn) then
  begin
    Application.ProcessMessages;
    FModel.Clear;
    ClearMeshes(FModel);
    if FModel.LoadFromFile(fn) then
      FModel.Hint := fn;
    Model := FModel;
  end;
end;

procedure TModelFrame.Clear;
begin
  ClearMeshes(FModel);
end;

procedure TModelFrame.ClearMeshes(var Model3D: TModel3D);
var
  i: Integer;
  LMeshes: TList<TMesh>;
  LMesh: TMesh;
begin
  LMeshes := TList<TMesh>.Create;
  try
    for i := 0 to Model3D.ChildrenCount - 1 do
    begin
      if Model3D.Children[i] is TMesh then
      begin
        LMeshes.Add(TMesh(Model3D.Children[i]));
      end;
    end;
    if LMeshes.Count > 0 then begin
      for LMesh in LMeshes do
      begin
        Model3D.RemoveObject(LMesh);
        LMesh.Free;
      end
    end;
  finally
    LMeshes.Free;
  end;
end;

procedure TModelFrame.LigAmbientChange(Sender: TObject);
var
  Mesh: TMesh;
begin
  if FIsUpdating or (MeshList.ItemIndex < 0) then
    Exit;
  Mesh := FModel.MeshCollection[MeshList.ItemIndex];
  Mesh.Visible := False;
  if Mesh.MaterialSource is TLightMaterialSource then
    TLightMaterialSource(Mesh.MaterialSource).Ambient := LigAmbient.Color;
  Mesh.Visible := True;
end;

procedure TModelFrame.LigDiffuseChange(Sender: TObject);
var
  Mesh: TMesh;
begin
  if FIsUpdating or (MeshList.ItemIndex < 0) then
    Exit;
  Mesh := FModel.MeshCollection[MeshList.ItemIndex];
  Mesh.Visible := False;
  if Mesh.MaterialSource is TLightMaterialSource then
    TLightMaterialSource(Mesh.MaterialSource).Diffuse := LigDiffuse.Color;
  Mesh.Visible := True;
end;

procedure TModelFrame.LigEmissiveChange(Sender: TObject);
var
  Mesh: TMesh;
begin
  if FIsUpdating or (MeshList.ItemIndex < 0) then
    Exit;
  Mesh := FModel.MeshCollection[MeshList.ItemIndex];
  Mesh.Visible := False;
  if Mesh.MaterialSource is TLightMaterialSource then
    TLightMaterialSource(Mesh.MaterialSource).Emissive := LigEmissive.Color;
  Mesh.Visible := True;
end;

procedure TModelFrame.LigShininessChange(Sender: TObject);
var
  Mesh: TMesh;
begin
  if FIsUpdating or (MeshList.ItemIndex < 0) then
    Exit;
  Mesh := FModel.MeshCollection[MeshList.ItemIndex];
  Mesh.Visible := False;
  if Mesh.MaterialSource is TLightMaterialSource then
    TLightMaterialSource(Mesh.MaterialSource).Shininess := Trunc(LigShininess.Value);
  Mesh.Visible := True;
end;

procedure TModelFrame.LigSpecularChange(Sender: TObject);
var
  Mesh: TMesh;
begin
  if FIsUpdating or (MeshList.ItemIndex < 0) then
    Exit;
  Mesh := FModel.MeshCollection[MeshList.ItemIndex];
  Mesh.Visible := False;
  if Mesh.MaterialSource is TLightMaterialSource then
    TLightMaterialSource(Mesh.MaterialSource).Specular := LigSpecular.Color;
  Mesh.Visible := True;
end;

procedure TModelFrame.LigTextureClick(Sender: TObject);
var
  Mesh: TMesh;
begin
  if FIsUpdating or (MeshList.ItemIndex < 0) then
    Exit;
  Mesh := FModel.MeshCollection[MeshList.ItemIndex];
  Mesh.Visible := False;
  if Mesh.MaterialSource is TLightMaterialSource then
    if OpenDialog.Execute then
      TLightMaterialSource(Mesh.MaterialSource).Texture.LoadFromFile(OpenDialog.FileName)
    else
      Exit;
  Mesh.Visible := True;
end;

procedure TModelFrame.LigTextureClrClick(Sender: TObject);
var
  Mesh: TMesh;
begin
  if FIsUpdating or (MeshList.ItemIndex < 0) then
    Exit;
  Mesh := FModel.MeshCollection[MeshList.ItemIndex];
  Mesh.Visible := False;
  if Mesh.MaterialSource is TLightMaterialSource then
    TLightMaterialSource(Mesh.MaterialSource).Texture.Clear(TAlphaColors.Null);
  Mesh.Visible := True;
end;

procedure TModelFrame.MatColorChange(Sender: TObject);
var
  Mesh: TMesh;
begin
  if FIsUpdating or (MeshList.ItemIndex < 0) then
    Exit;
  Mesh := FModel.MeshCollection[MeshList.ItemIndex];
  if Mesh.MaterialSource is TColorMaterialSource then
    TColorMaterialSource(Mesh.MaterialSource).Color := MatColor.Color;
end;

procedure TModelFrame.MaterialConChange(Sender: TObject);
var
  Mesh: TMesh;
begin
  if FIsUpdating or (MeshList.ItemIndex < 0) then
    Exit;
  Mesh := FModel.MeshCollection[MeshList.ItemIndex];
  Mesh.Visible := False;
  if Assigned(Mesh.MaterialSource) then
    Mesh.MaterialSource.Free;
  with MaterialCon do
  case TabIndex of
    0: Mesh.MaterialSource := TColorMaterialSource.Create(Self);
    1: Mesh.MaterialSource := TTextureMaterialSource.Create(Self);
    2: Mesh.MaterialSource := TLightMaterialSource.Create(Self);
  end;
  Mesh.MaterialSource.Parent := FModel;
  Mesh.Visible := True;
  ShowMesh;
end;

procedure TModelFrame.MatTexureClick(Sender: TObject);
var
  Mesh: TMesh;
begin
  if FIsUpdating or (MeshList.ItemIndex < 0) then
    Exit;
  Mesh := FModel.MeshCollection[MeshList.ItemIndex];
  if Mesh.MaterialSource is TTextureMaterialSource then
    if OpenDialog.Execute then
      TTextureMaterialSource(Mesh.MaterialSource).Texture.LoadFromFile(OpenDialog.FileName)
    else
      Exit;
end;

procedure TModelFrame.MatTexureClrClick(Sender: TObject);
var
  Mesh: TMesh;
begin
  if FIsUpdating or (MeshList.ItemIndex < 0) then
    Exit;
  Mesh := FModel.MeshCollection[MeshList.ItemIndex];
  if Mesh.MaterialSource is TTextureMaterialSource then
    TTextureMaterialSource(Mesh.MaterialSource).Texture.Clear(TAlphaColors.Null);
end;

procedure TModelFrame.MeshListChange(Sender: TObject);
begin
  ShowMesh;
end;

procedure TModelFrame.SetModel(const Value: TModel3D);
var
  Mesh : TMesh;
begin
  FModel := Value;
  if FModel = nil then
  begin
    FContain := nil;
    Exit;
  end
  else
    FContain := TStrokeCube(FModel.Parent);
  MeshList.Items.Clear;
  for Mesh in FModel.MeshCollection do
    MeshList.Items.Add(Format('组件 - %d', [MeshList.Count + 1]));
  MeshList.ItemIndex := 0;
  ShowMesh;
  IsDeleted.IsChecked := False;
end;

procedure TModelFrame.ShowMesh;
var
  Mesh: TMesh;
begin
  if MeshList.ItemIndex < 0 then
    Exit;
  FIsUpdating := True;
  try
    Mesh := FModel.MeshCollection[MeshList.ItemIndex];
    if Mesh.MaterialSource is TColorMaterialSource then
    begin
      MaterialCon.TabIndex := 0;
      MatColor.Color := TColorMaterialSource(Mesh.MaterialSource).Color;
    end
    else if Mesh.MaterialSource is TTextureMaterialSource then
    begin
      MaterialCon.TabIndex := 1;
    end
    else if Mesh.MaterialSource is TLightMaterialSource then
    begin
      MaterialCon.TabIndex := 2;
      with TLightMaterialSource(Mesh.MaterialSource) do
      begin
        LigAmbient.Color := Ambient;
        LigDiffuse.Color := Diffuse;
        LigEmissive.Color := Emissive;
        LigSpecular.Color := Specular;
        LigShininess.Value := Shininess;
      end;
    end
    else
    begin
      if Assigned(Mesh.MaterialSource) then
        Mesh.MaterialSource.Free;
      Mesh.MaterialSource := TLightMaterialSource.Create(FModel);
    end;
  finally
    FIsUpdating := False;
  end;
end;

end.
