unit login_form;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Objects, zhyk_component, main, IdBaseComponent,
  IdThreadComponent;

type
  TLoginForm = class(TForm)
    CaptchaPanel: TPanel;
    CaptchaEdit: TEdit;
    CaptchaImage: TImage;
    CaptchaButton: TButton;
    RefreshCaptchaButton: TButton;
    CaptchaToolBar: TToolBar;
    CaptchaLabel: TLabel;
    LoginPanel: TPanel;
    LoginButton: TButton;
    PasswordEdit: TEdit;
    PasswordLabel: TLabel;
    LoginEdit: TEdit;
    LoginLabel: TLabel;
    LogoToolBar: TToolBar;
    LogoLabel: TLabel;
    procedure LoginButtonClick(Sender: TObject);
    procedure RefreshCaptchaButtonClick(Sender: TObject);
    procedure CaptchaButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure GoToMainForm(OnStart: Boolean = false);
  public
    { Public declarations }
  end;

var
  LoginForm: TLoginForm;

implementation

{$R *.fmx}

procedure TLoginForm.CaptchaButtonClick(Sender: TObject);
begin
  try
    ZA.AuthAfterCaptcha(CaptchaEdit.Text);
  finally
    if ZA.IsAuth then
    begin
      GoToMainForm;
    end
    else
    begin
      LoginPanel.Visible   := true;
      CaptchaPanel.Visible := false;
    end;
  end;
end;

procedure TLoginForm.FormCreate(Sender: TObject);
var
  Coo: TStringList;
begin
  if (FileExists(COOKIE_FILE)) then
  begin
    Coo := TStringList.Create;
    Coo.LoadFromFile(COOKIE_FILE);
    ZA.SetCookies(Coo.Text);
    Coo.Free;
    try
      ZA.ParsSecureToken;
    except
    end;
    if ZA.IsAuth then
      GoToMainForm(true)
    else
    begin
      DeleteFile(COOKIE_FILE);
      LoginPanel.Visible := true;
    end;
  end
  else
  begin
    LoginPanel.Visible := true;
  end;
end;

procedure TLoginForm.FormShow(Sender: TObject);
begin
  if SHOW_MAIN_FORM_ON_START then
  begin
    SHOW_MAIN_FORM_ON_START := false;
    Hide;
  end;
end;

procedure TLoginForm.GoToMainForm(OnStart: Boolean = false);
var
  Coo: TStringList;
begin
  Coo      := TStringList.Create;
  Coo.Text := ZA.GetCookies;
  Coo.SaveToFile(COOKIE_FILE);
  Coo.Free;
  // ZA.StartChatUpdate;
  if OnStart then
  begin
    SHOW_MAIN_FORM_ON_START := true;
  end
  else
  begin
    // MainForm.ChatList.Clear;
    MainForm.LoadFirstPages;
    MainForm.Show;
  end;
  LoginPanel.Visible   := true;
  CaptchaPanel.Visible := false;
end;

procedure TLoginForm.LoginButtonClick(Sender: TObject);
begin
  CaptchaEdit.Text := '';
  try
    ZA.AuthBeforeCaptcha(LoginEdit.Text, PasswordEdit.Text, CaptchaImage);
  finally
  end;
  if ZA.IsAuthBeforeCaptcha then
  begin
    CaptchaPanel.Visible := true;
    LoginPanel.Visible   := false;
  end;
end;

procedure TLoginForm.RefreshCaptchaButtonClick(Sender: TObject);
begin
  ZA.ReloadCaptcha(CaptchaImage);
end;

end.
