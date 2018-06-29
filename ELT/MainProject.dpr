program MainProject;

uses
  System.StartUpCopy,
  FMX.Forms,
  ELT.Windows in 'ELT.Windows.pas',
  ELT.Extra in 'ELT.Extra.pas',
  ELT.Main in 'ELT.Main.pas',
  ELT_PsgFrame in 'ELT_PsgFrame.pas' {PsgFrame: TFrame},
  TestForm in 'TestForm.pas' {MainForm},
  ELT_MngFrame in 'ELT_MngFrame.pas' {MngFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
