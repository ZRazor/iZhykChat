unit ZhykAuth;

interface

uses
  FMX.Objects,
  FMX.Types,
  System.SysUtils,
  System.Classes,
  IdHTTP,
  IdCookieManager,
  IdHashMessageDigest,
  IdMultipartFormData,
  IdURI,
  IdThreadComponent,
  ZChat;

type
  TAjaxUser = record
    Nick: string;
    ID: string;
  end;

  TArrayOfAjaxUsers = array of TAjaxUser;

  TChatUpdateEvent = procedure(Sender: TObject) of object;

type
  TZhykAuth = class
  private
    FChat:                TChat;
    ChatThread:           TIdThreadComponent;
    CM:                   TIdCookieManager;
    SecurityToken:        string;
    ReCaptchaHash:        string;
    ReCaptchaImgHash:     string;
    HumanVerifyHash:      string;
    FLogin:               string;
    FPasswordMD5:         string;
    FIsAuth:              Boolean;
    FIsAuthBeforeCaptcha: Boolean;
    FChatUpdateInterval:  Integer;
    FOnChatUpdate:        TChatUpdateEvent;
    function CreateHTTPObject: TIdHTTP;
    procedure LoadCaptcha(CaptchaImg: TImage; Reload: Boolean = false);
    procedure ClearData;
    procedure CheckAuth;
    procedure SetCharUpdateInterval(Value: Integer);
    procedure ChatThreadRun(Sender: TIdThreadComponent);
    procedure SendChatUpdate;
    procedure UpdateChat;
  public
    constructor Create;
    destructor Destroy;
    function GetCookies: string;
    function FindAjaxUsers(PartOfNick: string): TArrayOfAjaxUsers;
    procedure SetCookies(Cookies: string);
    procedure AuthBeforeCaptcha(Login, Password: string; CaptchaImg: TImage);
    procedure ReloadCaptcha(CaptchaImg: TImage);
    procedure AuthAfterCaptcha(CaptchaText: string);
    procedure SendPM(Recipients, BccRecipients: TStringList; Title, MessageBody: string; IconIndex: Integer = 0);
    procedure SendChatMsg(Msg: String);
    procedure ParsSecureToken;
    procedure Logout;
    procedure StartChatUpdate;
    procedure StopChatUpdate;
    property Chat: TChat read FChat;
    property IsAuth: Boolean read FIsAuth;
    property IsAuthBeforeCaptcha: Boolean read FIsAuthBeforeCaptcha;
    property OnChatUpdate: TChatUpdateEvent read FOnChatUpdate write FOnChatUpdate;
    property ChatUpdateInterval: Integer read FChatUpdateInterval write SetCharUpdateInterval;
  end;

implementation

function PosEx(SubStr, str: string; Index: longint): Integer;
begin
  delete(str, 1, index);
  Result := index + Pos(SubStr, str);
end;

function ParsSubString(defString, LeftString, RightString: string; AssignLeft: Boolean = false): string;
begin
  Result := '';
  if (Pos(LeftString, defString) = 0) or (Pos(RightString, defString) = 0) then
    Exit;
  Result := Copy(defString, Pos(LeftString, defString) + Length(LeftString),
    PosEx(RightString, defString, Pos(LeftString, defString) + Length(LeftString)) - Pos(LeftString, defString) -
    Length(LeftString));
  if AssignLeft then
    Result := LeftString + Result;
end;

{ TZhykAuth }

// ===================BASIC BASIC========================//

procedure TZhykAuth.ClearData;
begin
  CM.CookieCollection.Clear;
  SecurityToken        := '';
  FIsAuth              := false;
  FIsAuthBeforeCaptcha := false;
  ReCaptchaHash        := '';
  ReCaptchaImgHash     := '';
  HumanVerifyHash      := '';
end;

constructor TZhykAuth.Create;
begin
  FChat := TChat.Create;

  CM := TIdCookieManager.Create(nil);

  FChatUpdateInterval := 3000;
  FIsAuth             := false;
  SecurityToken       := '';
  ChatThread          := TIdThreadComponent.Create(nil);
  ChatThread.OnRun    := ChatThreadRun;
  ChatThread.Loop     := true;
end;

function TZhykAuth.CreateHTTPObject: TIdHTTP;
begin
  Result                   := TIdHTTP.Create(nil);
  Result.AllowCookies      := true;
  Result.HandleRedirects   := true;
  Result.Request.UserAgent := 'Mozilla/5.0 (TZhykAuth)';
  Result.CookieManager     := CM;
end;

destructor TZhykAuth.Destroy;
begin
  FChat.Free;
  CM.Free;
  ChatThread.Free;
  inherited Destroy;
end;

// ===================AUTH AUTH AUTH AUTH========================//

procedure TZhykAuth.CheckAuth;
begin
  if not FIsAuth then
    raise Exception.Create('Для этого действия необходима авторизация');
end;

procedure TZhykAuth.AuthBeforeCaptcha(Login, Password: string; CaptchaImg: TImage);
var
  Post:   TIdMultipartFormDataStream;
  HashMD: TIdHashMessageDigest5;
  S:      string;
  HTTP:   TIdHTTP;
begin
  ClearData;
  Post         := TIdMultipartFormDataStream.Create;
  HashMD       := TIdHashMessageDigest5.Create;
  FPasswordMD5 := LowerCase(HashMD.HashStringAsHex(Password));
  FLogin       := Login;
  HashMD.Free;
  with Post do
  begin
    AddFormField('vb_login_username', FLogin, 'windows-1251').ContentTransfer := '8bit';
    AddFormField('cookieuser', '1');
    AddFormField('vb_login_password', '');
    AddFormField('s', '');
    AddFormField('securitytoken', 'guest');
    AddFormField('do', 'login');
    AddFormField('vb_login_md5password', FPasswordMD5);
    AddFormField('vb_login_md5password_utf', FPasswordMD5);
  end;
  S               := '';
  ReCaptchaHash   := '';
  HumanVerifyHash := '';
  HTTP            := CreateHTTPObject;
  try
    S := HTTP.Post('http://zhyk.ru/forum/login.php?do=login', Post);
  except
  end;
  Post.Free;
  if Pos('humanverify[hash]', S) = 0 then
  begin
    HTTP.Free;
    raise Exception.Create('Неудалось отправить запрос');
  end;
  ReCaptchaHash    := ParsSubString(S, 'http://api.recaptcha.net/challenge?k=', '"');
  HumanVerifyHash  := ParsSubString(S, 'name="humanverify[hash]" value="', '"');
  ReCaptchaImgHash := '';
  S                := '';
  try
    S := HTTP.Get('http://api.recaptcha.net/challenge?k=' + ReCaptchaHash);
  except
  end;
  HTTP.Free;
  if Pos('challenge', S) = 0 then
    raise Exception.Create('Неудалось загрузить капчу');
  ReCaptchaImgHash := ParsSubString(S, 'challenge : ''', '''');
  LoadCaptcha(CaptchaImg);
  FIsAuthBeforeCaptcha := true;
end;

procedure TZhykAuth.LoadCaptcha(CaptchaImg: TImage; Reload: Boolean = false);
var
  MS:   TMemoryStream;
  S:    string;
  HTTP: TIdHTTP;
begin
  HTTP := CreateHTTPObject;
  if Reload then
  begin
    S := '';
    try
      S := HTTP.Get('http://www.google.com/recaptcha/api/reload?c=' + ReCaptchaImgHash + '&k=' + ReCaptchaHash +
        '&reason=[object%20MouseEvent]&type=image&lang=ru');
    except
    end;
    if Pos('finish_reload', S) = 0 then
      raise Exception.Create('Неудалось обновить капчу');
    ReCaptchaImgHash := ParsSubString(S, 'finish_reload(''', '''');
  end;
  MS := TMemoryStream.Create;
  try
    HTTP.Get('http://www.google.com/recaptcha/api/image?c=' + ReCaptchaImgHash, MS);
  except
    HTTP.Free;
    raise Exception.Create('Неудалось загрузить картинку капчи');
  end;
  HTTP.Free;
  MS.Position := 0;
  CaptchaImg.Bitmap.LoadFromStream(MS);
  MS.Free;
end;

procedure TZhykAuth.ReloadCaptcha(CaptchaImg: TImage);
begin
  LoadCaptcha(CaptchaImg, true);
end;

procedure TZhykAuth.AuthAfterCaptcha(CaptchaText: string);
var
  Post: TIdMultipartFormDataStream;
  HTTP: TIdHTTP;
  S:    string;
begin
  Post := TIdMultipartFormDataStream.Create;
  if (HumanVerifyHash = '') or (ReCaptchaImgHash = '') then
    raise Exception.Create('Отсуствуют необходимые параметры для отправки капчи');
  with Post do
  begin
    AddFormField('humanverify[hash]', HumanVerifyHash);
    AddFormField('vb_login_username', FLogin, 'windows-1251').ContentTransfer := '8bit';
    AddFormField('vb_login_password', FPasswordMD5);
    AddFormField('cookieuser', '1');
    AddFormField('url', 'http://zhyk.ru/forum/index.php');
    AddFormField('s', '');
    AddFormField('securitytoken', 'guest');
    AddFormField('do', 'dologin');
    AddFormField('recaptcha_challenge_field', ReCaptchaImgHash);
    AddFormField('recaptcha_response_field', CaptchaText);
    AddFormField('postvars', '');
    AddFormField('logintype', '');
    AddFormField('cssprefs', '');
  end;
  HTTP := CreateHTTPObject;
  try
    S := HTTP.Post('http://zhyk.ru/forum/login.php?do=dologin', Post);
  except
  end;
  HTTP.Free;
  Post.Free;
  if (Pos('name="securitytoken"', S) = 0) and (Pos('Спасибо, что зашли', S) = 0) then
    raise Exception.Create('Не удалось авторизоваться');
  if (Pos('Спасибо, что зашли', S) <> 0) then
    ParsSecureToken
  else
    SecurityToken := ParsSubString(S, 'name="securitytoken" value="', '"');
  if SecurityToken = 'guest' then
    raise Exception.Create('Неправильный логин пароль');
end;

procedure TZhykAuth.Logout;
var
  HTTP: TIdHTTP;
begin
  HTTP := CreateHTTPObject;
  try
    HTTP.Get('http://zhyk.ru/forum/login.php?do=logout&logouthash=' + SecurityToken);
  except
  end;
  HTTP.Free;
  ClearData;
end;

procedure TZhykAuth.ParsSecureToken;
var
  S:    string;
  HTTP: TIdHTTP;
begin
  HTTP := CreateHTTPObject;
  try
    S := HTTP.Get('http://zhyk.ru/forum/index.php');
  except
  end;
  HTTP.Free;
  if Pos('name="securitytoken"', S) = 0 then
    raise Exception.Create('Неудалось загрузить ключ безопастности');
  SecurityToken := ParsSubString(S, 'name="securitytoken" value="', '"');
  if SecurityToken <> 'guest' then
    FIsAuth := true;
end;

// ===================COOKIES COOKIES COOKIES========================//

function TZhykAuth.GetCookies: string;
var
  I: Integer;
begin
  Result   := '';
  for I    := 0 to CM.CookieCollection.Count - 1 do
    Result := Result + CM.CookieCollection[I].CookieName + '=' + CM.CookieCollection[I].Value + #13#10;
end;

procedure TZhykAuth.SetCookies(Cookies: string);
var
  T: TStringList;
  S: string;
begin
  ClearData;
  T      := TStringList.Create;
  T.Text := Cookies;
  for S in T do
  begin
    CM.CookieCollection.AddClientCookie(S);
    CM.CookieCollection.AddServerCookie(S, TIdURI.Create('http://zhyk.ru'));
  end;
  T.Free;
end;


// ===================PM PM PM PM PM PM========================//

procedure TZhykAuth.SendChatMsg(Msg: String);
var
  Post: TIdMultipartFormDataStream;
  HTTP: TIdHTTP;
begin
  Post := TIdMultipartFormDataStream.Create;
  with Post do
  begin
    AddFormField('s', '');
    AddFormField('securitytoken', SecurityToken);
    AddFormField('do', 'cb_postnew');
    AddFormField('ccb_newmessage', Msg, 'windows-1251').ContentTransfer := '8bit';
  end;
  HTTP := CreateHTTPObject;
  try
    HTTP.Post('http://zhyk.ru/forum/misc.php', Post);
  except
  end;
  HTTP.Free;
  Post.Free;
end;

function TZhykAuth.FindAjaxUsers(PartOfNick: string): TArrayOfAjaxUsers;
var
  Post: TIdMultipartFormDataStream;
  HTTP: TIdHTTP;
  S:    string;
begin
  CheckAuth;
  SetLength(Result, 0);
  Post := TIdMultipartFormDataStream.Create;
  with Post do
  begin
    AddFormField('securitytoken', SecurityToken);
    AddFormField('do', 'usersearch');
    AddFormField('fragment', PartOfNick, 'windows-1251').ContentTransfer := '8bit';
  end;
  HTTP := CreateHTTPObject;
  try
    S := HTTP.Post('http://zhyk.ru/forum/ajax.php?do=usersearch', Post);
  except
  end;
  HTTP.Free;
  Post.Free;
  if Pos('<users>', S) = 0 then
    raise Exception.Create('Неудалось загрузить список пользователей');
  while Pos('<user userid="', S) > 0 do
  begin
    SetLength(Result, Length(Result) + 1);
    Result[high(Result)].ID   := ParsSubString(S, '<user userid="', '"');
    Result[high(Result)].Nick := ParsSubString(S, '">', '</user>');
    delete(S, 1, Pos('</user>', S));
  end;
end;

procedure TZhykAuth.SendPM(Recipients, BccRecipients: TStringList; Title, MessageBody: string; IconIndex: Integer = 0);
var
  Post:           TIdMultipartFormDataStream;
  HTTP:           TIdHTTP;
  S:              string;
  I:              Integer;
  ARecipients:    string;
  ABccRecipients: string;
begin
  CheckAuth;
  Post := TIdMultipartFormDataStream.Create;
  if Recipients.Count + BccRecipients.Count = 0 then
    raise Exception.Create('Отсуствуют получатели');
  ARecipients      := '';
  for I            := 0 to Recipients.Count - 1 do
    ARecipients    := ARecipients + Recipients[I] + ' ;';
  ABccRecipients   := '';
  for I            := 0 to BccRecipients.Count - 1 do
    ABccRecipients := ABccRecipients + BccRecipients[I] + ' ;';
  with Post do
  begin
    AddFormField('recipients', ARecipients, 'windows-1251').ContentTransfer := '8bit';
    AddFormField('bccrecipients', ABccRecipients, 'windows-1251').ContentTransfer := '8bit';
    AddFormField('title', Title, 'windows-1251').ContentTransfer := '8bit';
    AddFormField('message', MessageBody, 'windows-1251').ContentTransfer := '8bit';
    AddFormField('wysiwyg', '0');
    AddFormField('iconid', IntToStr(IconIndex)); // !
    AddFormField('s', '');
    AddFormField('securitytoken', SecurityToken);
    AddFormField('do', 'insertpm');
    AddFormField('pmid', '');
    AddFormField('forward', '');
    AddFormField('sbutton', 'Создать сообщение');
    AddFormField('parseurl', '1');
  end;
  HTTP := CreateHTTPObject;
  try
    S := HTTP.Post('http://zhyk.ru/forum/private.php?do=insertpm&pmid=', Post);
  except
  end;
  HTTP.Free;
  Post.Free;
end;


// ===================CHAT CHAT CHAT CHAT========================//

procedure TZhykAuth.SendChatUpdate;
begin
  if Assigned(FOnChatUpdate) then
  begin
    FOnChatUpdate(Self);
  end;
end;

procedure TZhykAuth.ChatThreadRun(Sender: TIdThreadComponent);
begin
  if not IsAuth then
  begin
    StopChatUpdate;
    raise Exception.Create('Для обновления чата нужна авторизация');
  end;

  // Sender.Synchronize(UpdateChat);
  UpdateChat;

  Sender.Synchronize(SendChatUpdate);
  // SendChatUpdate;

  sleep(FChatUpdateInterval);
end;

procedure TZhykAuth.UpdateChat;
var
  S:    string;
  Post: TStringList;
  HTTP: TIdHTTP;
begin
  CheckAuth;
  Post := TStringList.Create;
  with Post do
  begin
    Add('securitytoken=' + SecurityToken);
    Add('s=');
  end;
  HTTP := CreateHTTPObject;
  try
    S := HTTP.Get('http://zhyk.ru/forum/misc.php?show=ccbmessages');
  except
  end;
  Post.Free;
  HTTP.Free;
  FChat.LoadNewMessages(S);
end;

procedure TZhykAuth.SetCharUpdateInterval(Value: Integer);
begin
  FChatUpdateInterval := Value;
end;

procedure TZhykAuth.StartChatUpdate;
begin
  if ChatThread.Stopped then
    ChatThread.Start;
end;

procedure TZhykAuth.StopChatUpdate;
begin
  ChatThread.Stop;
end;

end.
