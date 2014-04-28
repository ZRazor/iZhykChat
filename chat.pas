unit chat;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Memo, FMX.Edit, FMX.StdCtrls, chat_component, zchat, settings;

type
  TChatForm = class(TForm)
    ChatToolBar: TToolBar;
    SettingsButton: TButton;
    ChatLabel: TLabel;
    SendButton: TButton;
    ChatMemo: TMemo;
    MessageEdit: TClearingEdit;
    procedure SendButtonClick(Sender: TObject);
    procedure SettingsButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private

  public
    procedure OnChatTimer(Sender: TObject);
  end;

var
  ChatForm: TChatForm;

implementation

{$R *.fmx}
{ TChatForm }

procedure TChatForm.FormActivate(Sender: TObject);
begin
  if ZA.IsAuth then
    ZA.StartChatUpdate;
end;

procedure TChatForm.FormCreate(Sender: TObject);
begin
  if SHOW_CHAT_ON_START then
    Show;
  ZA.OnChatUpdate := ChatForm.OnChatTimer;
end;

procedure TChatForm.FormDeactivate(Sender: TObject);
begin
  if ZA.IsAuth then
    ZA.StopChatUpdate;
end;

procedure TChatForm.OnChatTimer(Sender: TObject);
var
  i: integer;
begin
  ChatMemo.BeginUpdate;
  ChatMemo.Lines.Clear;
  for i := 0 to ZA.chat.Count - 1 do
    ChatMemo.Lines.Add(Format('%s %s: %s', [ZA.chat.Messages[i].ServerTime, ZA.chat.Messages[i].Nick,
      ZA.chat.Messages[i].Text]));
  ChatMemo.EndUpdate;
  ChatMemo.GoToTextEnd;
end;

procedure TChatForm.SendButtonClick(Sender: TObject);
begin
  ZA.SendChatMsg(MessageEdit.Text);
  MessageEdit.Text := '';
end;

procedure TChatForm.SettingsButtonClick(Sender: TObject);
begin
  SettingsForm.Show;
end;

end.
