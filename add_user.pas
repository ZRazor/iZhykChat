unit add_user;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Layouts, FMX.ListBox, zhyk_component, ZhykAuth;

type
  TAddUserForm = class(TForm)
    AddUserBar: TToolBar;
    AddUserLabel: TLabel;
    CloseButton: TButton;
    NickLabel: TLabel;
    NickEdit: TEdit;
    SearchButton: TButton;
    UsersListBox: TListBox;
    AddButton: TButton;
    procedure CloseButtonClick(Sender: TObject);
    procedure SearchButtonClick(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure ClearFields;
  end;

var
  AddUserForm: TAddUserForm;

implementation

uses new_msg;
{$R *.fmx}

procedure TAddUserForm.AddButtonClick(Sender: TObject);
begin
  if NewMsgForm.ReceiversEdit.Text <> '' then
    NewMsgForm.ReceiversEdit.Text := NewMsgForm.ReceiversEdit.Text + '; ';
  NewMsgForm.ReceiversEdit.Text   := NewMsgForm.ReceiversEdit.Text + UsersListBox.Selected.Text;
  Hide;
  NewMsgForm.Show;
end;

procedure TAddUserForm.ClearFields;
begin
  UsersListBox.Clear;
  NickEdit.Text     := '';
  AddButton.Enabled := false;
end;

procedure TAddUserForm.CloseButtonClick(Sender: TObject);
begin
  Hide;
  NewMsgForm.Show;
end;

procedure TAddUserForm.SearchButtonClick(Sender: TObject);
var
  users: TArrayOfAjaxUsers;
  i:     integer;
begin
  UsersListBox.Clear;
  AddButton.Enabled := false;
  users             := ZA.FindAjaxUsers(NickEdit.Text);
  // UsersListBox.BeginUpdate;
  for i := 0 to High(users) do
    UsersListBox.Items.Add(users[i].Nick);
  // UsersListBox.EndUpdate;
  if length(users) > 0 then
    AddButton.Enabled    := true;
  UsersListBox.ItemIndex := 0;
end;

end.
