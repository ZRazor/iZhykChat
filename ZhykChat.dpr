program ZhykChat;

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  login_form in 'login_form.pas' {LoginForm},
  chat in 'chat.pas' {ChatForm},
  ZhykAuth in 'ZhykAuth.pas',
  chat_component in 'chat_component.pas',
  settings in 'settings.pas' {SettingsForm},
  chat_utils in 'chat_utils.pas',
  ZChat in 'ZChat.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TLoginForm, LoginForm);
  Application.CreateForm(TChatForm, ChatForm);
  Application.CreateForm(TSettingsForm, SettingsForm);
  Application.Run;

end.
