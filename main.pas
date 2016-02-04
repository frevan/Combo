unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ComCtrls, Worker, fvm.Combinations;

const
     PM_ComboThreadFinished = WM_USER+1;

type
  TMainForm = class(TForm)
    PageControl: TPageControl;
    ComboSheet: TTabSheet;
    ResultsSheet: TTabSheet;
    ComboCalculateBtn: TButton;
    ComboSizeEdit: TEdit;
    ComboMaxEdit: TEdit;
    Label1: TLabel;
    ComboBox: TListBox;
    lblComboCount: TLabel;
    ComboMemo: TMemo;
    PastResultsView: TListView;
    lblPastResultsCount: TLabel;
    CalculationsSheet: TTabSheet;
    procedure ComboCalculateBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ComboBoxClick(Sender: TObject);
    procedure ComboBoxKeyPress(Sender: TObject; var Key: Char);
  private
    AppPath     : string;
    ComboThread : TCalculationThread;
    procedure ChangeEnabled;
    procedure PMComboThreadFinished(var M: TMessage); message PM_ComboThreadFinished;
    function GetFilenameForCombination(Num, Max: integer): string;
    procedure FindCombinationFiles(List: TStrings);
    procedure LoadSelectedCombo;
    function AddNewCombinationFile(const Filename: string): integer;
    procedure LoadPastResults;
    function ParseCSVLine(const Line: string; out Date: TDate; out Combo: PCombination; out Length: integer): boolean;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

const
     CombinationsPathConst = 'combinations\';
     PastResultsPathConst  = 'results\';

     PastResultsFilenameConst = 'results.csv';

     ComboFileExt = '.combo';



function NextValueFrom(var s: string; Separator: char = ','): string;
var
   p: integer;
begin
  p := Pos(Separator, s);
  if p = 0 then
  begin
    Result := Trim(s);
    s := '';
  end
  else
  begin
    Result := Trim(Copy(s, 1, p-1));
    s := Trim(Copy(s, p+1, MaxInt));
  end;
end;



{ TMainForm }

function TMainForm.AddNewCombinationFile(const Filename: string): integer;
var
   s   : string;
   idx : integer;
begin
  Result := -1;
  if not FileExists(Filename) then
    Exit;

  s := ChangeFileExt(ExtractFileName(Filename), '');
  idx := ComboBox.Items.IndexOf(s);
  if idx < 0 then
    idx := ComboBox.Items.Add(s);

  Result := idx;
end;

procedure TMainForm.ChangeEnabled;
begin
  lblComboCount.Visible := Assigned(ComboThread) or (ComboMemo.Lines.Count > 0);
end;

procedure TMainForm.ComboBoxClick(Sender: TObject);
begin
  LoadSelectedCombo;
end;

procedure TMainForm.ComboBoxKeyPress(Sender: TObject; var Key: Char);
begin
  LoadSelectedCombo;
end;

procedure TMainForm.ComboCalculateBtnClick(Sender: TObject);
var
   num, max : integer;
begin
  num := StrToIntDef(ComboSizeEdit.Text, 0);
  max := StrToIntDef(ComboMaxEdit.Text, 0);
  if (num = 0) or (max = 0) or (num > max) then
  begin
    MessageDlg('Input error.', mtError, [mbOK], 0);
    Exit;
  end;

  lblComboCount.Caption := 'Calculating...';

  ComboThread := TCalculationThread.Create;
  //ComboThread.Priority := tpHigher;
  ComboThread.Num := num;
  ComboThread.Max := max;
  ComboThread.NotificationWindow := Handle;
  ComboThread.NotificationMsg := PM_ComboThreadFinished;

  ComboThread.Start;

  ChangeEnabled;
end;

procedure TMainForm.FindCombinationFiles(List: TStrings);
var
   SR : TSearchRec;
   r  : integer;
   sl : TStringList;
   s  : string;
begin
  r := FindFirst(AppPath + CombinationsPathConst + '*' + ComboFileExt, faAnyFile, SR);
  while r = 0 do
  begin
    if (SR.Name = '.') or (SR.Name = '..') or (SR.Attr and faDirectory = faDirectory) then
      Continue;

    s := AppPath + CombinationsPathConst + SR.Name;
    List.Add(s);

    r := FindNext(SR);
  end;
  FindClose(SR);
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
   sl : TStringList;
   i  : integer;
begin
  ComboThread := nil;
  AppPath := ExtractFilePath(Application.ExeName);

  sl := TStringList.Create;
  try
    FindCombinationFiles(sl);

    ComboBox.Items.BeginUpdate;
    for i := 0 to sl.Count-1 do
      AddNewCombinationFile(sl[i]);
    ComboBox.Items.EndUpdate;

    ComboBox.ItemIndex := 0;
    LoadSelectedCombo;
  finally
    sl.Free;
  end;

  LoadPastResults;

  ChangeEnabled;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if Assigned(ComboThread) then
  begin
    ComboThread.Cancelled := TRUE;
    ComboThread.WaitFor;
    FreeAndNil(ComboThread);
  end;
end;

function TMainForm.GetFilenameForCombination(Num, Max: integer): string;
begin
  Result := AppPath + CombinationsPathConst + Format('%d from %d%s', [Num, Max, ComboFileExt]);
end;

procedure TMainForm.LoadPastResults;
var
   fname   : string;
   sl      : TStringList;
   csvline : string;
   d       : TDate;
   combo   : PCombination;
   item    : TListItem;
   i       : integer;
   combocount: integer;
begin
  PastResultsView.Items.Clear;

  fname := AppPath + PastResultsPathConst + PastResultsFilenameConst;
  if not FileExists(fname) then
    Exit;

  sl := TStringList.Create;
  try
    sl.LoadFromFile(fname);
    for csvline in sl do
    begin
      if not ParseCSVLine(csvline, d, combo, combocount) then
        Continue;

      item := PastResultsView.Items.Add;
      item.Caption := DateToStr(d);
      for i := 0 to combocount-1 do
        item.SubItems.Add(IntToStr(combo^[i]));

      FreeMem(combo);
    end;
  finally
    sl.Free;
  end;

  lblPastResultsCount.Caption := Format('Found %d past results.', [PastResultsView.Items.Count]);
end;

procedure TMainForm.LoadSelectedCombo;
var
   fname: string;
begin
  if (ComboBox.ItemIndex < 0) or (ComboBox.ItemIndex >= ComboBox.Items.Count) then
    Exit;

  fname := AppPath + CombinationsPathConst + ComboBox.Items[ComboBox.ItemIndex] + ComboFileExt;
  if not FileExists(fname) then
  begin
    MessageDlg('The selected combination file doesn''t exist.', mtError, [mbOK], 0);
    Exit;
  end;

  lblComboCount.Caption := 'Loading combinations...';
  lblComboCount.Visible := TRUE;
  Application.ProcessMessages;

  ComboMemo.Lines.LoadFromFile(fname);

  lblComboCount.Caption := Format('%d combination(s)', [ComboMemo.Lines.Count]);
  ChangeEnabled;
end;

function TMainForm.ParseCSVLine(const Line: string; out Date: TDate; out Combo: PCombination; out Length: integer): boolean;
var
   s, vstring : string;
   value      : cardinal;
begin
  Date := 0;
  Combo := nil;
  Length := 0;

  s := Trim(Line);
  vstring := NextValueFrom(s, ',');
  if vstring = '' then
    Exit(FALSE);

  try
    Date := StrToDate(vstring);
  except
    on EConvertError do
      Exit(FALSE);
  end;

  while s <> '' do
  begin
    vstring := NextValueFrom(s, ',');
    value := StrToIntDef(vstring, -1);
    if value = -1 then
      Break;

    inc(Length);
    ReAllocMem(Combo, Length*SizeOf(cardinal));
    Combo^[Length-1] := value;
  end;

  Result := (Length > 0);
end;

procedure TMainForm.PMComboThreadFinished(var M: TMessage);
var
   i, j  : integer;
   s     : string;
   combo : PCombination;
   sl    : TStringList;
   fname : string;
   idx   : integer;
begin
  fname := GetFilenameForCombination(ComboThread.Num, ComboThread.Max);

  sl := TStringList.Create;
  try
    for i := 0 to ComboThread.List.Count-1 do
    begin
      combo := ComboThread.List.Items[i];
      s := '';
      for j := 0 to ComboThread.Num-1 do
        s := s + ' ' + IntToStr(combo^[j]);
      sl.Add(s);
    end;
    sl.SaveToFile(fname);
  finally
    sl.Free;
  end;

  idx := AddNewCombinationFile(fname);
  if idx >= 0 then
  begin
    ComboBox.ItemIndex := idx;
    LoadSelectedCombo;
  end;

  FreeAndNil(ComboThread);

  ChangeEnabled;
end;

initialization
  ReportMemoryLeaksOnShutdown := TRUE;
end.
