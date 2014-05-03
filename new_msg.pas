unit new_msg;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Memo, FMX.Edit, zhyk_component, add_user;

type
  TNewMsgForm = class(TForm)
    NewMsgBar: TToolBar;
    NewMsgLabel: TLabel;
    CloseButton: TButton;
    ReceiversEdit: TEdit;
    ReceiversLabel: TLabel;
    AddReceiverButton: TButton;
    LabelTitle: TLabel;
    TitleEdit: TEdit;
    TextMemo: TMemo;
    SendButton: TButton;
    procedure CloseButtonClick(Sender: TObject);
    procedure SendButtonClick(Sender: TObject);
    procedure AddReceiverButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure ClearFields;
  end;

var
  NewMsgForm: TNewMsgForm;

implementation

{$R *.fmx}

uses main;

procedure TNewMsgForm.AddReceiverButtonClick(Sender: TObject);
begin
  AddUserForm.ClearFields;
  AddUserForm.Show;
end;

procedure TNewMsgForm.ClearFields;
begin
  ReceiversEdit.Text := '';
  TitleEdit.Text     := '';
  TextMemo.Text      := '';
end;

procedure TNewMsgForm.CloseButtonClick(Sender: TObject);
begin
  Hide;
  MainForm.Show;
end;

procedure TNewMsgForm.SendButtonClick(Sender: TObject);
begin
  ZA.SendPM(ReceiversEdit.Text, '', TitleEdit.Text, TextMemo.Text);
end;

end.
