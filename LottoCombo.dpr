program LottoCombo;

uses
  Vcl.Forms,
  main in 'main.pas' {MainForm},
  worker in 'worker.pas',
  fvm.combinations in 'fvm.combinations.pas',
  calculator in 'calculator.pas',
  fvm.strings in 'fvm.strings.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
