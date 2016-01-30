unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TMainForm = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    ComboCalculateBtn: TButton;
    ComboSizeEdit: TEdit;
    ComboMaxEdit: TEdit;
    Label1: TLabel;
    ComboResultBox: TListBox;
    procedure ComboCalculateBtnClick(Sender: TObject);
  private
    procedure ChangeEnabled;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
    fvm.Combinations;

{$R *.dfm}

{ TMainForm }

procedure TMainForm.ChangeEnabled;
begin
  {}
end;

procedure TMainForm.ComboCalculateBtnClick(Sender: TObject);
var
   num, max : integer;
   list     : TCombinationList;
   i, j     : integer;
   s        : string;
   combo    : PCombination;
begin
  num := StrToIntDef(ComboSizeEdit.Text, 0);
  max := StrToIntDef(ComboMaxEdit.Text, 0);
  if (num = 0) or (max = 0) or (num > max) then
  begin
    MessageDlg('Input error.', mtError, [mbOK], 0);
    Exit;
  end;

  ChangeEnabled;

  list := TCombinationList.Create;
  try
    CalculateCombinations(num, 1, max, list);

    ComboResultBox.Items.BeginUpdate;
    ComboResultBox.Items.Clear;
    for i := 0 to list.Count-1 do
    begin
      combo := list.Items[i];
      s := '';
      for j := 0 to num-1 do
        s := s + ' ' + IntToStr(combo^[j]);
      ComboResultBox.Items.Add(s);
    end;
    ComboResultBox.Items.EndUpdate;
  finally
    list.Free;
  end;
end;

initialization
  ReportMemoryLeaksOnShutdown := TRUE;
end.
