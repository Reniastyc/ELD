program ELD_RegProject;

uses
  Vcl.Forms,
  ELD_Register in 'ELD_Register.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
