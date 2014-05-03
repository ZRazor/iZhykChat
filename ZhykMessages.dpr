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
  ZChat in 'ZChat.pas',
  new_msg in 'new_msg.pas' {NewMsgForm},
  add_user in 'add_user.pas' {AddUserForm};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Portrait, TFormOrientation.Landscape, TFormOrientation.InvertedLandscape];
  Application.CreateForm(TLoginForm, LoginForm);
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TSettingsForm, SettingsForm);
  Application.CreateForm(TNewMsgForm, NewMsgForm);
  Application.CreateForm(TAddUserForm, AddUserForm);
  Application.Run;

end.
