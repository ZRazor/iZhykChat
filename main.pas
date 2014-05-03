unit main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Memo, FMX.Edit, FMX.StdCtrls, zhyk_component, zchat, settings, FMX.ListBox,
  FMX.ListView.Types, FMX.ListView, FMX.TabControl, ZhykAuth, new_msg;

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
    procedure NewMsgButtonClick(Sender: TObject);
  private
    UpdatingIncome:         Boolean;
    IncomeCurrentMaxScroll: Single;
    IncomePage:             Integer;
    IncomeMaxPage:          Boolean;
    ReloadingIncome:        Boolean;
    DisableScrollActions:   Boolean;

    UnreadBitmap: TBitmap;
    ReadBitmap:   TBitmap;

    procedure AddItemToList(PM: TPMessageInfo; Folder: TPMFolder);
    procedure OnFolderClear(Sender: TObject; Folder: TPMFolder);
    procedure OnFolderLoad(Sender: TObject; Folder: TPMFolder);
    procedure OnChatTimer(Sender: TObject);
  public
    procedure LoadFirstPages;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}
{ TChatForm }

procedure TMainForm.AddItemToList(PM: TPMessageInfo; Folder: TPMFolder);
var
  LImage: TListItemImage;
  LItem:  TListViewItem;
  View:   TListView;
begin
  case Folder of
    pfIncome:
      View := IncomeView;
    pfOutcome:
      LItem := nil; // !
  end;

  LItem := View.Items.AddItem;

  with LItem do
  begin
    Text                 := PM.Title;
    Detail               := PM.Sender + ' ' + PM.Date + ' ' + PM.Time;
    LImage               := TListItemImage.Create(LItem);
    LImage.Name          := '';
    LImage.Align         := TListItemAlign.Leading;
    LImage.VertAlign     := TListItemAlign.Center;
    LImage.PlaceOffset.Y := 2;
    LImage.PlaceOffset.X := 0;
    LImage.Width         := 35;
    LImage.Height        := 45;
    if Folder = pfIncome then
    begin
      if PM.Unread then
        LImage.Bitmap := UnreadBitmap
      else
        LImage.Bitmap := ReadBitmap;
    end;
  end;

end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  // if ZA.IsAuth then
  // ZA.StartChatUpdate;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  TRes: TResourceStream;
begin
  UnreadBitmap := TBitmap.Create;
  ReadBitmap   := TBitmap.Create;
  TRes         := TResourceStream.Create(hInstance, 'UNREAD_PNG', RT_RCDATA);
  UnreadBitmap.LoadFromStream(TRes);
  TRes.Free;

  TRes := TResourceStream.Create(hInstance, 'READ_PNG', RT_RCDATA);
  ReadBitmap.LoadFromStream(TRes);
  TRes.Free;

  ZA.OnPMFolderLoad  := OnFolderLoad;
  ZA.OnPMFolderClear := OnFolderClear;

  ZA.ClearPMFolder(pfIncome);

  if SHOW_MAIN_FORM_ON_START then
  begin
    Show;
    LoadFirstPages;
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
  IncomeCurrentMaxScroll := IncomeView.ItemCount * IncomeView.ItemAppearance.ItemHeight - IncomeView.Height;
  if IncomeView.ScrollViewPos < IncomeCurrentMaxScroll + 19 then
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
  if IncomeView.ScrollViewPos > -19 then
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
  if { DisableScrollActions or } UpdatingIncome or ReloadingIncome then
    exit;
  if IncomeView.ScrollViewPos < -80 then
  begin
    DisableScrollActions         := true;
    ReloadingIncome              := true;
    IncomeTopHintLabel.Visible   := false;
    IncomeTopIndicator.Visible   := true;
    IncomeTopIndicator.Enabled   := true;
    IncomeTopUpdateTimer.Enabled := true;
    ZA.ClearPMFolder(pfIncome);
    ZA.LoadPMFolder(pfIncome, IncomePage);
    exit;
  end;
  if not ReloadingIncome and (IncomeView.ScrollViewPos < -20) then
  begin
    IncomeTopHintLabel.Visible   := true;
    IncomeTopUpdateTimer.Enabled := true;
    exit;
  end;
  if IncomeView.ItemCount = 0 then
    exit;
  if IncomeMaxPage then
    exit;
  IncomeCurrentMaxScroll := IncomeView.ItemCount * IncomeView.ItemAppearance.ItemHeight - IncomeView.Height;
  if IncomeView.ScrollViewPos > IncomeCurrentMaxScroll + 80 then
  begin
    DisableScrollActions            := true;
    UpdatingIncome                  := true;
    IncomeBottomHintLabel.Visible   := false;
    IncomeBottomIndicator.Visible   := true;
    IncomeBottomIndicator.Enabled   := true;
    IncomeBottomUpdateTimer.Enabled := true;
    Inc(IncomePage);
    ZA.LoadPMFolder(pfIncome, IncomePage);
    exit;
  end;
  if not UpdatingIncome and (IncomeView.ScrollViewPos > IncomeCurrentMaxScroll + 20) then
  begin
    IncomeBottomHintLabel.Visible   := true;
    IncomeBottomUpdateTimer.Enabled := true;
    exit;
  end;
end;

procedure TMainForm.LoadFirstPages;
var
  i: Integer;
begin
  ZA.LoadPMFolder(pfIncome, IncomePage);
end;

procedure TMainForm.NewMsgButtonClick(Sender: TObject);
begin
  NewMsgForm.ClearFields;
  NewMsgForm.Show;
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

procedure TMainForm.OnFolderClear(Sender: TObject; Folder: TPMFolder);
begin
  case Folder of
    pfIncome:
      begin
        IncomePage    := 1;
        IncomeMaxPage := false;
        IncomeView.ClearItems;
      end;
    pfOutcome:
      begin

      end;
  end;

end;

procedure TMainForm.OnFolderLoad(Sender: TObject; Folder: TPMFolder);
var
  i, sidx: Integer;
begin
  case Folder of
    pfIncome:
      begin
        sidx := IncomeView.ItemCount - 1;
        IncomeView.BeginUpdate;
      end;
    pfOutcome:
      begin

      end;
  end;
  if sidx = -1 then
    sidx := 0;
  for i  := sidx to ZA.GetPMFolderCount(Folder) - 1 do
    AddItemToList(ZA.GetPM(i, Folder), Folder);
  case Folder of
    pfIncome:
      begin
        IncomeView.EndUpdate;
        if ZA.GetPMMaxPage(pfIncome) <= IncomePage then
          IncomeMaxPage := true;
      end;
    pfOutcome:
      begin

      end;
  end;

end;

procedure TMainForm.SettingsButtonClick(Sender: TObject);
begin
  SettingsForm.Show;
end;

end.
