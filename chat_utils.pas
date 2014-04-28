unit chat_utils;

interface

uses System.Classes, RegularExpressions, System.SysUtils;

function FilterChat(const InStr: String): string;

implementation

function StripHtmlMarkup(const source: string): string;
var
  i, count: Integer;
  InTag:    Boolean;
  P:        PChar;
begin
  SetLength(Result, Length(source));
  P     := PChar(Result);
  InTag := False;
  count := 0;
  for i := 1 to Length(source) do
    if InTag then
    begin
      if source[i] = '>' then
        InTag := False;
    end
    else if source[i] = '<' then
      InTag := True
    else
    begin
      P[count] := source[i];
      Inc(count);
    end;
  SetLength(Result, count);
end;

function FilterChat(const InStr: String): string;
const
  REG_MSG  = '<td style=[^>]*>(.*?)<\/';
  REG_NICK = '<a href="member\.php\?u=[^>]*>(.*?)<\/';
var
  RegEx:           TRegEx;
  Nicks, Messages: TMatchCollection;
  i:               Integer;
  Rez:             TstringList;
begin
  Rez := TstringList.Create;

  if RegEx.IsMatch(InStr, REG_MSG, [roSingleLine]) then
  begin
    Messages := RegEx.Matches(InStr, REG_MSG, [roSingleLine]);
    Nicks    := RegEx.Matches(InStr, REG_NICK, [roSingleLine]);
    for i    := 0 to Nicks.count - 1 do
      if Messages.count - 1 >= i then
      begin

        Rez.Add(StringReplace(Format('%s: %s', [StripHtmlMarkup('<' + Nicks.Item[i].Value),
          StripHtmlMarkup('<' + Messages.Item[i].Value)]), sLineBreak, '', [rfReplaceAll]));
      end;
  end;
  Result := Rez.Text;
  Rez.Free;
end;

end.
