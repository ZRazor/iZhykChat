unit zhyk_component;

interface

uses ZhykAuth, System.SysUtils, System.IOUtils;

var
  ZA:                      TZhykAuth;
  COOKIE_FILE:             String;
  UNREAD_IMG:              String;
  READ_IMG:                String;
  SHOW_MAIN_FORM_ON_START: Boolean;

implementation

initialization

SHOW_MAIN_FORM_ON_START := false;
UNREAD_IMG              := TPath.GetDocumentsPath + PathDelim + '';
READ_IMG                := TPath.GetDocumentsPath + PathDelim + '';
COOKIE_FILE             := TPath.GetDocumentsPath + PathDelim + 'cookies.txt';
ZA                      := TZhykAuth.Create;

end.
