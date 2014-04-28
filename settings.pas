unit settings;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  chat_component;

type
  TSettingsForm = class(TForm)
    LogoutButton: TButton;
    SettingsToolBat: TToolBar;
    CloseButton: TButton;
    SettingsLabel: TLabel;
    AboutLabel: TLabel;
    procedure CloseButtonClick(Sender: TObject);
    procedure LogoutButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SettingsForm: TSettingsForm;

implementation

{$R *.fmx}

uses chat, login_form;

procedure TSettingsForm.CloseButtonClick(Sender: TObject);
begin
  Hide;
end;

procedure TSettingsForm.LogoutButtonClick(Sender: TObject);
begin
  if FileExists(COOKIE_FILE) then
    DeleteFile(COOKIE_FILE);
  ZA.Logout;
  LoginForm.Show;
end;

end.
