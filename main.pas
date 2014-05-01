unit main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Memo, FMX.Edit, FMX.StdCtrls, zhyk_component, zchat, settings, FMX.ListBox,
  FMX.ListView.Types, FMX.ListView, FMX.TabControl;

type
  TMainForm = class(TForm)
    MainToolBar: TToolBar;
    MainLabel: TLabel;
    MsgTabs: TTabControl;
    IncomeTab: TTabItem;
    OutcomeTab: TTabItem;
    IncomeView: TListView;
    SettingsButton: TButton;
    NewMsgButton: TButton;
    IncomeBottomIndicator: TAniIndicator;
    IncomeBottomUpdateTimer: TTimer;
    IncomeBottomHintLabel: TLabel;
    IncomeTopIndicator: TAniIndicator;
    IncomeTopHintLabel: TLabel;
    IncomeTopUpdateTimer: TTimer;
    procedure SettingsButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure IncomeViewDeletingItem(Sender: TObject; AIndex: Integer; var ACanDelete: Boolean);
    procedure IncomeViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure IncomeBottomUpdateTimerTimer(Sender: TObject);
    procedure IncomeTopUpdateTimerTimer(Sender: TObject);
  private
    UpdatingIncome:         Boolean;
    IncomeCurrentMaxScroll: Single;
    ReloadingIncome:        Boolean;
    DisableScrollActions:   Boolean;

    UnreadBitmap: TBitmap;
    ReadBitmap:   TBitmap;
  public
    procedure OnChatTimer(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}
{ TChatForm }

procedure TMainForm.FormActivate(Sender: TObject);
begin
  // if ZA.IsAuth then
  // ZA.StartChatUpdate;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  i:      Integer;
  LImage: TListItemImage;
  LItem:  TListViewItem;
  TRes:   TResourceStream;
begin
  UnreadBitmap := TBitmap.Create;
  ReadBitmap   := TBitmap.Create;
  TRes         := TResourceStream.Create(hInstance, 'UNREAD_PNG', RT_RCDATA);
  UnreadBitmap.LoadFromStream(TRes);
  TRes.Free;
  if SHOW_MAIN_FORM_ON_START then
    Show;
  for i := 1 to 30 do
  begin
    LItem := IncomeView.Items.AddItem;
    with LItem do
    begin
      Text                 := 'пожалуйста подскажи по своему бруту';
      Detail               := 'Legolasses 07.04.2014, 09:09';
      LImage               := TListItemImage.Create(LItem);
      LImage.Name          := 'Resim';
      LImage.Align         := TListItemAlign.Leading; // En Saр
      LImage.VertAlign     := TListItemAlign.Center; // Orta
      LImage.PlaceOffset.Y := 2;
      LImage.PlaceOffset.X := 0;
      LImage.Width         := 35;
      LImage.Height        := 45;
      LImage.Bitmap        := UnreadBitmap;
      // LImage.OwnsBitmap       := True;
      // LImage.Bitmap           := TBitmap.Create(0, 0);
      // MS := TMemoryStream.Create;
      // IdHttp.Get(Bilgi.strDescription, MS);
      // MS.Seek(0,soFromBeginning);
      // LImage.Bitmap.LoadFromStream(MS);
      // MS.Free;
    end;
  end;
  // ZA.OnChatUpdate := ChatForm.OnChatTimer;
end;

procedure TMainForm.FormDeactivate(Sender: TObject);
begin
  // if ZA.IsAuth then
  // ZA.StopChatUpdate;
end;

procedure TMainForm.IncomeBottomUpdateTimerTimer(Sender: TObject);
begin
  if IncomeView.ScrollViewPos < IncomeCurrentMaxScroll + 15 then
  begin
    IncomeBottomUpdateTimer.Enabled := false;
    IncomeBottomHintLabel.Visible   := false;
  end;
  if UpdatingIncome and (IncomeView.ScrollViewPos < IncomeCurrentMaxScroll + 60) then
  begin
    IncomeBottomUpdateTimer.Enabled := false;
    IncomeBottomHintLabel.Visible   := false;
    IncomeBottomIndicator.Visible   := false;
    IncomeBottomIndicator.Enabled   := false;
    UpdatingIncome                  := false;
  end;

end;

procedure TMainForm.IncomeTopUpdateTimerTimer(Sender: TObject);
begin
  if IncomeView.ScrollViewPos > -15 then
  begin
    IncomeTopUpdateTimer.Enabled := false;
    IncomeTopHintLabel.Visible   := false;
  end;
  if ReloadingIncome and (IncomeView.ScrollViewPos > -60) then
  begin
    IncomeTopUpdateTimer.Enabled := false;
    IncomeTopHintLabel.Visible   := false;
    IncomeTopIndicator.Visible   := false;
    IncomeTopIndicator.Enabled   := false;
    ReloadingIncome              := false;
  end;

end;

procedure TMainForm.IncomeViewDeletingItem(Sender: TObject; AIndex: Integer; var ACanDelete: Boolean);
begin
  //
end;

procedure TMainForm.IncomeViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  if DisableScrollActions or UpdatingIncome or ReloadingIncome then
    exit;
  if IncomeView.ScrollViewPos < -80 then
  begin
    DisableScrollActions         := true;
    ReloadingIncome              := true;
    IncomeTopHintLabel.Visible   := false;
    IncomeTopIndicator.Visible   := true;
    IncomeTopIndicator.Enabled   := true;
    IncomeTopUpdateTimer.Enabled := true;
    exit;
  end;
  if not ReloadingIncome and (IncomeView.ScrollViewPos < -20) then
  begin
    IncomeTopHintLabel.Visible   := true;
    IncomeTopUpdateTimer.Enabled := true;
    exit;
  end;
  IncomeCurrentMaxScroll := IncomeView.ItemCount * IncomeView.ItemAppearance.ItemHeight - IncomeView.Height;
  if IncomeView.ScrollViewPos > IncomeCurrentMaxScroll + 80 then
  begin
    DisableScrollActions            := true;
    UpdatingIncome                  := true;
    IncomeBottomHintLabel.Visible   := false;
    IncomeBottomIndicator.Visible   := true;
    IncomeBottomIndicator.Enabled   := true;
    IncomeBottomUpdateTimer.Enabled := true;
    exit;
  end;
  if not UpdatingIncome and (IncomeView.ScrollViewPos > IncomeCurrentMaxScroll + 20) then
  begin
    IncomeBottomHintLabel.Visible   := true;
    IncomeBottomUpdateTimer.Enabled := true;
    exit;
  end;
end;

procedure TMainForm.OnChatTimer(Sender: TObject);
// var
// i: integer;
begin
  // ChatMemo.Lines.BeginUpdate;
  // ChatList.Clear;
  // for i := 0 to ZA.chat.Count - 1 do
  // ChatList.Items.Add(Format('%s %s: %s', [ZA.chat.Messages[i].ServerTime, ZA.chat.Messages[i].Nick,
  // ZA.chat.Messages[i].Text]));
  // ChatMemo.Lines.EndUpdate;
  // ChatMemo.GoToTextEnd;
end;

procedure TMainForm.SettingsButtonClick(Sender: TObject);
begin
  SettingsForm.Show;
end;

end.
