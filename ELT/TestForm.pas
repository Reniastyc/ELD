unit TestForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Graphics, FMX.Dialogs, FMX.Objects, FMX.Ani, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts,
  ELT.Main, ELT_MngFrame, ELT_PsgFrame;

type
  TMainForm = class(TForm)
    MngrCon: TMngFrame;
    FormBack: TScrollBox;
    ConBack: TPanel;
    PassCon: TPsgFrame;
    AManager: TUIManager;
    procedure FormCreate(Sender: TObject);
    procedure AManagerCreatePass(Sender, Control: TObject);
    procedure AManagerClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure MngrConBtnLoadClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure EnterUIPassage(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

var
  PassAni, MngrAni: TFloatAnimation;

{$R *.fmx}

procedure TMainForm.EnterUIPassage(Sender: TObject);
begin
  PassCon.Passage := TUIPassage(Sender);
  PassAni.StopValue := 0;
  PassAni.Enabled := True;
  PassAni.Start;
  MngrAni.StopValue := ConBack.Width + 5;
  MngrAni.Enabled := True;
  MngrAni.Start;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  FreeAndNil(MngrAni);
  FreeAndNil(PassAni);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  MngrCon.Manager := AManager;
  PassAni := TFloatAnimation.Create(Self);
  PassAni.Parent := PassCon;
  PassAni.PropertyName := 'Position.X';
  PassAni.StartFromCurrent := True;
  MngrAni := TFloatAnimation.Create(Self);
  MngrAni.Parent := MngrCon;
  MngrAni.PropertyName := 'Position.X';
  MngrAni.StartFromCurrent := True;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  PassCon.Height := ClientHeight;
  MngrCon.Height := ClientHeight;
  if PassCon.Position.X > MngrCon.Position.X then
  begin
    PassCon.Position.X := ConBack.Width + 5;
    MngrCon.Position.X := 0;
  end
  else
  begin
    PassCon.Position.X := 0;
    MngrCon.Position.X := ConBack.Width + 5;
  end;
end;

procedure TMainForm.MngrConBtnLoadClick(Sender: TObject);
begin
  MngrCon.BtnLoadClick(Sender);
  Self.BeginUpdate;
  try
    Self.RecreateCanvas;
  finally
    Self.EndUpdate;
  end;
end;

procedure TMainForm.AManagerClick(Sender: TObject);
begin
  PassCon.Passage := nil;
  PassAni.StopValue := ConBack.Width + 5;
  PassAni.Enabled := True;
  PassAni.Start;
  MngrAni.StopValue := 0;
  MngrAni.Enabled := True;
  MngrAni.Start;
end;

procedure TMainForm.AManagerCreatePass(Sender, Control: TObject);
begin
  TUIPassage(Control).OnEnter := EnterUIPassage;
end;

end.
