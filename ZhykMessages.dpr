program ZhykMessages;





{$R *.dres}

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  login_form in 'login_form.pas' {LoginForm},
  main in 'main.pas' {MainForm},
  ZhykAuth in 'ZhykAuth.pas',
  zhyk_component in 'zhyk_component.pas',
  settings in 'settings.pas' {SettingsForm},
  ZChat in 'ZChat.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Portrait, TFormOrientation.Landscape, TFormOrientation.InvertedLandscape];
  Application.CreateForm(TLoginForm, LoginForm);
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TSettingsForm, SettingsForm);
  Application.Run;

end.
