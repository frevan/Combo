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
    Label2: TLabel;
    CalcSourceCombo: TComboBox;
    CalcInclusionCombo: TComboBox;
    Label3: TLabel;
    CalculateBtn: TButton;
    CalculationResultBox: TListBox;
    ComboLoadListBtn: TButton;
    ComboSaveAsTextBtn: TButton;
    SaveDialog: TSaveDialog;
    CalcSaveAsTextBtn: TButton;
    CalcWinBox: TListBox;
    CalcLoadFromTextBtn: TButton;
    OpenDialog: TOpenDialog;
    CalcRandomBtn: TButton;
    procedure ComboCalculateBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CalculateBtnClick(Sender: TObject);
    procedure ComboLoadListBtnClick(Sender: TObject);
    procedure ComboSaveAsTextBtnClick(Sender: TObject);
    procedure CalcSaveAsTextBtnClick(Sender: TObject);
    procedure ComboBoxClick(Sender: TObject);
    procedure ComboBoxKeyPress(Sender: TObject; var Key: Char);
    procedure CalcLoadFromTextBtnClick(Sender: TObject);
    procedure CalcRandomBtnClick(Sender: TObject);
  private
    AppPath        : string;
    ComboThread    : TCalculationThread;
    PastResultList : TCombinationList;
    procedure ChangeEnabled;
    procedure PMComboThreadFinished(var M: TMessage); message PM_ComboThreadFinished;
    function GetFilenameForCombination(Num, Max: integer): string;
    procedure FindCombinationFiles(List: TStrings);
    procedure LoadSelectedCombo;
    function AddNewCombinationFile(const Filename: string): integer;
    procedure LoadPastResults;
    function ParseCSVLine(const Line: string; out Date: TDate; out Combo: TCombination): boolean;
    procedure UpdateCalculationCombos;
    function BuildFileNameForCombination(const ComboName, SubFolder: string; ShouldExist: boolean): string;
    procedure CheckResults;
  public
  end;

var
  MainForm: TMainForm;

implementation

uses
    calculator, fvm.Strings;

{$R *.dfm}

const
     CombinationsPathConst = 'combinations\';
     PastResultsPathConst  = 'results\';
     CalculationsPathConst = 'calculations\';

     PastResultsFilenameConst = 'results.csv';

     ComboFileExt = '.combo';

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

function TMainForm.BuildFileNameForCombination(const ComboName, SubFolder: string; ShouldExist: boolean): string;
var
   fname: string;
begin
  Result := '';

  fname := AppPath + SubFolder + ComboName + ComboFileExt;
  if FileExists(fname) or not ShouldExist then
    Result := fname;
end;

procedure TMainForm.ComboLoadListBtnClick(Sender: TObject);
begin
  LoadSelectedCombo;
end;

procedure TMainForm.ComboSaveAsTextBtnClick(Sender: TObject);
var   fname : string;
      list  : TCombinationList;
      sl    : TStrings;
begin
  if (ComboBox.ItemIndex < 0) or (ComboBox.ItemIndex >= ComboBox.Items.Count) then
    Exit;

  fname := BuildFileNameForCombination(ComboBox.Items[ComboBox.ItemIndex], CombinationsPathConst, TRUE);
  if fname = '' then
  begin
    MessageDlg('The selected combination file doesn''t exist.', mtError, [mbOK], 0);
    Exit;
  end;

  SaveDialog.FileName := ChangeFileExt(ExtractFileName(fname), '') + '.txt';
  if SaveDialog.Execute then
  begin
    list := TCombinationList.Create;
    try
      list.LoadFromFile(fname);
      sl := list.ToStrings;
      sl.SaveToFile(SaveDialog.FileName);
    finally
      sl.Free;
      list.Free;
    end;
  end;
end;

procedure TMainForm.CalcLoadFromTextBtnClick(Sender: TObject);
var  idx   : integer;
     combo : TCombination;
begin
  if OpenDialog.Execute then
  begin
    CalculationResultBox.Items.BeginUpdate;

    CalculationResultBox.Items.LoadFromFile(OpenDialog.FileName);
    idx := 0;
    while idx < CalculationResultBox.Items.Count do
    begin
      combo := StringToCombination(CalculationResultBox.Items[idx]);
      if combo = 0 then
      begin
        CalculationResultBox.Items.Delete(idx);
        Continue;
      end;
      CalculationResultBox.Items.Objects[idx] := TObject(combo);
      inc(idx);
    end;

    CalculationResultBox.Items.EndUpdate;

    CheckResults;
  end;
end;

procedure TMainForm.CalcRandomBtnClick(Sender: TObject);
var   resultlist, sourcelist : TCombinationList;
      sourcefname, fname     : string;
      sl                     : TStrings;
      i                      : integer;
begin
  sourcefname := BuildFileNameForCombination(CalcSourceCombo.Text, CombinationsPathConst, TRUE);
  if sourcefname = '' then
  begin
    MessageDlg('Please choose both combinations.', mtError, [mbOK], 0);
    Exit;
  end;

  resultlist := TCombinationList.Create;
  sourcelist := TCombinationList.Create;
  try
    sourcelist.LoadFromFile(sourcefname);

    FindRandomCombinations(resultlist, sourcelist, 720);

    sl := resultlist.ToStrings;
    CalculationResultBox.Items.Assign(sl);
    for i := 0 to resultlist.Count-1 do
      CalculationResultBox.Items.Objects[i] := TObject(resultlist.Items[i]);

    fname := BuildFileNameForCombination(FormatDateTime('yymmdd-hhmmss', Now), CalculationsPathConst, FALSE);
    ForceDirectories(ExtractFileDir(fname));
    CalculationResultBox.Items.SaveToFile(fname);
  finally
    sl.Free;
    resultlist.Free;
    sourcelist.Free;
  end;

  CheckResults;
end;

procedure TMainForm.CalcSaveAsTextBtnClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    CalculationResultBox.Items.SaveToFile(SaveDialog.FileName);
end;

procedure TMainForm.CalculateBtnClick(Sender: TObject);
var   resultlist, sourcelist, includelist : TCombinationList;
      sourcefname, includefname, fname    : string;
      sl                                  : TStrings;
      i                                   : integer;
begin
  sourcefname := BuildFileNameForCombination(CalcSourceCombo.Text, CombinationsPathConst, TRUE);
  includefname := BuildFileNameForCombination(CalcInclusionCombo.Text, CombinationsPathConst, TRUE);
  if (sourcefname = '') or (includefname = '') then
  begin
    MessageDlg('Please choose both combinations.', mtError, [mbOK], 0);
    Exit;
  end;

  resultlist := TCombinationList.Create;
  sourcelist := TCombinationList.Create;
  includelist := TCombinationList.Create;
  try
    sourcelist.LoadFromFile(sourcefname);
    includelist.LoadFromFile(includefname);

    FindCombinationsIncludingAllOf(resultlist, sourcelist, includelist);

    sl := resultlist.ToStrings;
    CalculationResultBox.Items.Assign(sl);
    for i := 0 to resultlist.Count-1 do
      CalculationResultBox.Items.Objects[i] := TObject(resultlist.Items[i]);

    fname := BuildFileNameForCombination(FormatDateTime('yymmdd-hhmmss', Now), CalculationsPathConst, FALSE);
    ForceDirectories(ExtractFileDir(fname));
    CalculationResultBox.Items.SaveToFile(fname);
  finally
    sl.Free;
    resultlist.Free;
    sourcelist.Free;
    includelist.Free;
  end;

  CheckResults;
end;

procedure TMainForm.ChangeEnabled;
begin
  lblComboCount.Visible := Assigned(ComboThread) or (ComboMemo.Lines.Count > 0);
end;

procedure TMainForm.CheckResults;
var   i, j           : integer;
      combo1, combo2 : TCombination;
      correct        : integer;
      s              : string;
      corrects       : array of integer;
begin
  CalcWinBox.Items.BeginUpdate;
  CalcWinBox.Items.Clear;
  for i := 0 to PastResultList.Count-1 do
  begin
    combo1 := PastResultList.Items[i];
    SetLength(corrects, TCombinationRec(combo1).NumPlaces+1);
    for j := 0 to Length(corrects)-1 do
      corrects[j] := 0;

    for j := 0 to CalculationResultBox.Items.Count-1 do
    begin
      combo2 := TCombination(CalculationResultBox.Items.Objects[j]);
      correct := CalculateHighestSubCountOf(combo1, combo2);
      if correct > 0 then
        inc(corrects[correct]);
    end;

    s := '';
    for j := 3 to Length(corrects)-1 do
    begin
      if corrects[j] = 0 then  s := Format('%s ..', [s, corrects[j]])
      else                     s := Format('%s %.2d', [s, corrects[j]])
    end;
    s := Format('[%.3d] %s', [i+1, s]);  // Trim(CombinationToString(combo1))
    CalcWinBox.Items.Add(s);
  end;
  CalcWinBox.Items.EndUpdate;
end;

procedure TMainForm.ComboBoxClick(Sender: TObject);
begin
  ComboMemo.Lines.Clear;
  lblComboCount.Caption := '';
end;

procedure TMainForm.ComboBoxKeyPress(Sender: TObject; var Key: Char);
begin
  ComboBoxClick(ComboBox);
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

  PastResultList := TCombinationList.Create;

  sl := TStringList.Create;
  try
    FindCombinationFiles(sl);

    ComboBox.Items.BeginUpdate;
    for i := 0 to sl.Count-1 do
      AddNewCombinationFile(sl[i]);
    ComboBox.Items.EndUpdate;

    ComboBox.ItemIndex := 0;
  finally
    sl.Free;
  end;

  LoadPastResults;

  UpdateCalculationCombos;
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

  FreeAndNil(PastResultList);
end;

function TMainForm.GetFilenameForCombination(Num, Max: integer): string;
begin
  Result := AppPath + CombinationsPathConst + Format('%d from %d%s', [Num, Max, ComboFileExt]);
end;

procedure TMainForm.LoadPastResults;
var   fname, csvline : string;
      sl             : TStringList;
      d              : TDate;
      combo          : TCombination;
      r              : TCombinationRec;
      item           : TListItem;
      i              : integer;
begin
  PastResultList.Clear;
  PastResultsView.Items.Clear;

  fname := AppPath + PastResultsPathConst + PastResultsFilenameConst;
  if not FileExists(fname) then
    Exit;

  sl := TStringList.Create;
  try
    sl.LoadFromFile(fname);
    PastResultList.Capacity := sl.Count;
    for csvline in sl do
    begin
      if not ParseCSVLine(csvline, d, combo) then
        Continue;

      r.Blob := combo;
      r.MaxValue := 45;
      PastResultList.Add(r.Blob);

      item := PastResultsView.Items.Add;
      item.Caption := DateToStr(d);
      for i := 0 to r.NumPlaces-1 do
        item.SubItems.Add(IntToStr(r.Data[i]));
    end;
  finally
    sl.Free;
  end;

  lblPastResultsCount.Caption := Format('Found %d past results.', [PastResultsView.Items.Count]);
end;

procedure TMainForm.LoadSelectedCombo;
var   fname : string;
      list  : TCombinationList;
      sl    : TStrings;
begin
  if (ComboBox.ItemIndex < 0) or (ComboBox.ItemIndex >= ComboBox.Items.Count) then
    Exit;

  fname := BuildFileNameForCombination(ComboBox.Items[ComboBox.ItemIndex], CombinationsPathConst, TRUE);
  if not FileExists(fname) then
  begin
    MessageDlg('The selected combination file doesn''t exist.', mtError, [mbOK], 0);
    Exit;
  end;

  lblComboCount.Caption := 'Loading combinations...';
  lblComboCount.Visible := TRUE;
  Application.ProcessMessages;

  list := TCombinationList.Create;
  try
    list.LoadFromFile(fname);
    sl := list.ToStrings;
    ComboMemo.Lines.Assign(sl);
  finally
    sl.Free;
    list.Free;
  end;

  lblComboCount.Caption := Format('%d combination(s)', [ComboMemo.Lines.Count]);
  ChangeEnabled;
end;

function TMainForm.ParseCSVLine(const Line: string; out Date: TDate; out Combo: TCombination): boolean;
var   s, vstring : string;
      idx        : integer;
      r          : TCombinationRec;
begin
  Date := 0;
  r.Blob := 0;

  s := Trim(Line);
  vstring := RetrieveNextValueFrom(s, ',');
  if vstring = '' then
    Exit(FALSE);

  try
    Date := StrToDate(vstring);
  except
    on EConvertError do
      Exit(FALSE);
  end;

  r.NumPlaces := 0;
  while (s <> '') and (r.NumPlaces < fvm.Combinations.MaxPlacesConst) do
  begin
    r.Data[r.NumPlaces] := StrToIntDef(RetrieveNextValueFrom(s, ','), 0);
    if r.Data[r.NumPlaces] = 0 then
      Break;
    inc(r.NumPlaces);
  end;

  Result := (r.NumPlaces > 0);
  if Result then
    Combo := r.Blob;
end;

procedure TMainForm.PMComboThreadFinished(var M: TMessage);
var   fname : string;
      idx   : integer;
begin
  fname := GetFilenameForCombination(ComboThread.Num, ComboThread.Max);
  ComboThread.List.SaveToFile(fname);

  idx := AddNewCombinationFile(fname);
  if idx >= 0 then
    ComboBox.ItemIndex := idx;

  FreeAndNil(ComboThread);

  ComboMemo.Lines.Clear;
  lblComboCount.Caption := '';
  UpdateCalculationCombos;
  ChangeEnabled;
end;

procedure TMainForm.UpdateCalculationCombos;
begin
  CalcSourceCombo.Items.Assign(ComboBox.Items);
  CalcInclusionCombo.Items.Assign(ComboBox.Items);
end;

initialization
  ReportMemoryLeaksOnShutdown := TRUE;
end.
