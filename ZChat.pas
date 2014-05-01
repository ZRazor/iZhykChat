unit ZChat;

interface

uses
  RegularExpressions,
  System.SysUtils,
  System.Classes;

type
  TChatMsg = record
    Nick: String;
    Text: String;
    ServerTime: String;
    function IsEqual(Msg: TChatMsg): boolean; overload;
    function IsEqual(ANick, AText, AServerTime: String): boolean; overload;
  end;

  TChatMessagesArray = array of TChatMsg;

type
  TChat = class
  private
    FMessages: array of TChatMsg;
    function GetMsg(Index: integer): TChatMsg;
    function GetCount: integer;
    procedure SetMsg(Index: integer; Msg: TChatMsg);
  public
    function LoadNewMessages(HTMLText: String): integer;
    procedure AddMsg(NewMsg: TChatMsg); overload;
    procedure AddMsg(ANick, AText, AServerTime: String); overload;
    property Count: integer read GetCount;
    property Messages[Index: integer]: TChatMsg read GetMsg write SetMsg;
  end;

implementation

type
  TSmileReplace = record
    B: String; // before
    A: String; // after
    function ReplaceStr(Str: String): String;
  end;

var
  SmilesReplace: array of TSmileReplace;

procedure AddSR(FB, FA: String);
begin
  SetLength(SmilesReplace, length(SmilesReplace) + 1);
  SmilesReplace[High(SmilesReplace)].B := FB;
  SmilesReplace[High(SmilesReplace)].A := FA;
end;

function TSmileReplace.ReplaceStr(Str: String): String;
begin
  Result := StringReplace(Str, B, A, [rfReplaceAll]);
end;

{ TChatMsg }

function TChatMsg.IsEqual(Msg: TChatMsg): boolean;
begin
  Result := (Msg.Nick = Nick) and (Msg.Text = Text) and (Msg.ServerTime = ServerTime);
end;

function TChatMsg.IsEqual(ANick, AText, AServerTime: String): boolean;
begin
  Result := (ANick = Nick) and (AText = Text) and (AServerTime = ServerTime);
end;

{ TChat }

procedure TChat.AddMsg(NewMsg: TChatMsg);
begin
  SetLength(FMessages, length(FMessages) + 1);
  FMessages[High(FMessages)] := NewMsg;
end;

procedure TChat.AddMsg(ANick, AText, AServerTime: String);
var
  NewMsg: TChatMsg;
begin
  with NewMsg do
  begin
    Nick       := ANick;
    ServerTime := AServerTime;
    Text       := AText;
  end;
  AddMsg(NewMsg);
end;

function TChat.GetCount: integer;
begin
  Result := length(FMessages);
end;

function TChat.GetMsg(Index: integer): TChatMsg;
begin
  Result := FMessages[Index];
end;

function StripHtmlMarkup(const source: string): string;
var
  i, Count: integer;
  InTag:    boolean;
  P:        PChar;
begin
  SetLength(Result, length(source));
  P     := PChar(Result);
  InTag := False;
  Count := 0;
  for i := 1 to length(source) do
    if InTag then
    begin
      if source[i] = '>' then
        InTag := False;
    end
    else if source[i] = '<' then
      InTag := True
    else
    begin
      P[Count] := source[i];
      Inc(Count);
    end;
  SetLength(Result, Count);
end;

function HandleMsg(Msg: TChatMsg): TChatMsg;
var
  i: TSmileReplace;
begin
  Msg.Nick := StripHtmlMarkup(Msg.Nick);
  Msg.Text := StringReplace(Msg.Text, #9, '', [rfReplaceAll]);
  Msg.Text := StringReplace(Msg.Text, #$D, '', [rfReplaceAll]);
  Msg.Text := StringReplace(Msg.Text, sLineBreak, '', [rfReplaceAll]);
  for i in SmilesReplace do
    Msg.Text := i.ReplaceStr(Msg.Text);
  Msg.Text   := StripHtmlMarkup(Msg.Text);
  exit(Msg);

end;

function TChat.LoadNewMessages(HTMLText: String): integer;
const
  REG_MSG  = '<td style=[^>]*>(.*?)<\/td>.*?<\/tr>.*?(<tr valign="top">|$)';
  REG_NICK = '<a href="member[^>]*>(.*?)<\/';
  REG_TIME = 'border="0" \/> <\/a> (.*?)<\/';
var
  Nicks, Msgs, Times: TMatchCollection;
  i:                  integer;
  NewMsg:             TChatMsg;
begin
  Result := 0;
  Nicks  := TRegEx.Matches(HTMLText, REG_NICK, [roSingleLine]);
  Msgs   := TRegEx.Matches(HTMLText, REG_MSG, [roSingleLine]);
  Times  := TRegEx.Matches(HTMLText, REG_TIME, [roSingleLine]);
  SetLength(FMessages, 0);
  for i := 0 to Nicks.Count - 1 do
  begin
    if (Msgs.Count - 1 >= i) and (Times.Count - 1 >= i) then
    begin
      NewMsg.Nick       := '<' + Nicks.Item[i].Groups.Item[1].Value;
      NewMsg.Text       := '<' + Msgs.Item[i].Groups.Item[1].Value;
      NewMsg.ServerTime := Times.Item[i].Groups.Item[1].Value;

      NewMsg := HandleMsg(NewMsg);

      AddMsg(NewMsg);
      Inc(Result);
    end;
  end;
end;

procedure TChat.SetMsg(Index: integer; Msg: TChatMsg);
begin
  FMessages[Index] := Msg;
end;

initialization

AddSR('<img src="images/smilies/trololo/fthat.png" border="0" alt="" title="Не говори глупостей" class="inlineimg" />',
  '/dgs');
AddSR('<img src="images/smilies/trololo/coolface.gif" border="0" alt="" title="Coolface" class="inlineimg" />',
  '/problem');
AddSR('<img src="images/smilies/trololo/poptartFINALTINY.gif" border="0" alt="" title="nyaa" class="inlineimg" />',
  '/nyan');

end.
