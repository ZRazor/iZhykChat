unit chat_component;

interface

uses ZhykAuth, System.SysUtils, System.IOUtils;

var
  ZA:                 TZhykAuth;
  COOKIE_FILE:        String;
  SHOW_CHAT_ON_START: Boolean;

implementation

initialization

SHOW_CHAT_ON_START := false;
COOKIE_FILE        := TPath.GetDocumentsPath + PathDelim + 'cookies.txt';
ZA                 := TZhykAuth.Create;

end.
