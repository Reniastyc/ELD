program ELD_Project;

uses
  System.StartUpCopy,
  FMX.Forms,
  ELD_Form in 'ELD_Form.pas' {ELDForm},
  ELD_CamCon in 'ELD_CamCon.pas' {CamConMenu: TFrame},
  ELD_Lattice in 'ELD_Lattice.pas' {LatticeFrame: TFrame},
  ELD_Menu in 'ELD_Menu.pas' {MenuFrame: TFrame},
  ELD_Model in 'ELD_Model.pas' {ModelFrame: TFrame},
  ELD_Lens in 'ELD_Lens.pas' {LensFrame: TFrame},
  ELT.Extra in '..\ELT\ELT.Extra.pas',
  ELT.Main in '..\ELT\ELT.Main.pas',
  ELT.Windows in '..\ELT\ELT.Windows.pas',
  ELT_MngFrame in '..\ELT\ELT_MngFrame.pas' {MngFrame: TFrame},
  ELT_PsgFrame in '..\ELT\ELT_PsgFrame.pas' {PsgFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TELDForm, ELDForm);
  AExeName := ParamStr(0);
  AFile := ParamStr(1);
  Application.Run;
end.
