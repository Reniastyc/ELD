unit ELD_Register;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, System.Win.Registry,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TMainForm = class(TForm)
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function SetAssociatedExec(FiExt, Fitype, FileDes, MIME, Exen: string): Boolean;
function GetDirectory(Path: string): string;

var
  MainForm: TMainForm;

implementation

function GetDirectory(Path: string): string;
begin
  Exit(Path.Substring(0, Path.LastIndexOf(PathDelim)));
end;

function SetAssociatedExec(FiExt, Fitype, FileDes, MIME, Exen: string): Boolean;
var
  Reg: TRegistry;
begin
  if (FiExt = '') or (Exen = '') then
    Exit(False);
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKey_Classes_Root;
    if not Reg.OpenKey(FiExt, True) then
      Exit(False);
    Reg.WriteString('', Fitype);
    if MIME <> '' then
      Reg.WriteString('Content Type', MIME);
    Reg.CloseKey;
    if not Reg.OpenKey(Fitype, True) then
      Exit(False);
    Reg.WriteString('', FileDes);
    if not Reg.OpenKey('shell\open\command', True) then
      Exit(False);
    Reg.WriteString('', Exen + ' %1');
    Reg.CloseKey;
  finally
    Reg.Free;
  end;
  Exit(True);
end;

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Timer.Enabled := True;
end;

procedure TMainForm.TimerTimer(Sender: TObject);
var
  fn, pa: string;
begin
  pa := GetDirectory(Application.ExeName);
  fn := pa + '\ELD.exe';
  TTimer(Sender).Free;
  if not FileExists(fn) then
  begin
    MessageBox(Handle, '设置失败！请确保程序完整。', '提醒', MB_OK or MB_ICONSTOP);
    Exit;
  end;
  if SetAssociatedExec('.eldu', 'ELD演示空间', 'ELD演示空间', 'ELD演示空间', fn) then
    MessageBox(Handle, '设置成功！', '提醒', MB_OK or MB_ICONINFORMATION)
  else
    MessageBox(Handle, '设置失败！请以管理员身份运行该程序以完成设置。', '提醒', MB_OK or MB_ICONSTOP);
  Close;
end;

end.
