unit fvm.Combinations;

interface

uses
    Generics.Collections, Classes, SyncObjs;

type
    PCombination = ^TCombination;
    TCombination = record
      Next, Prev : PCombination;
      Places     : cardinal;
      Data       : array[0..0] of cardinal;
      function ContainsOtherCombination(Other: PCombination): boolean;
      function ToString: string;
    end;


    TCombinationList = class
    private
      FFirst  : PCombination;
      FLast   : PCombination;
      FCount  : integer;
      Lock    : TCriticalSection;
      function GetItems(Index: integer): PCombination;
      function CreateItemFromString(const Line: string): PCombination;
    public
      constructor Create;
      destructor Destroy; override;

      class function CreateNewItem(Places: cardinal; CopyFrom: PCombination = nil): PCombination;
      procedure Add(Item: PCombination);
      procedure Delete(Item: PCombination; FreeAsWell: boolean);
      procedure Clear;

      procedure SaveToFile(const FileName: string);
      procedure LoadFromFile(const FileName: string);

      function ToStrings: TStrings;

      property First: PCombination read FFirst;
      property Last: PCombination read FLast;
      property Count: integer read FCount;
      property Items[Index: integer]: PCombination read GetItems;
    end;

    TCompactCombo = int64;  TODO: use this instead of the record above - it will use 6 bits per place for a max of 10 places out of 1-63 numbers
                                                                       - could also use some bits to store the number of places (3-4 bits) and the max (6 bits)



procedure CalculateCombinations(Places, MaxValue: cardinal; ResultList: TCombinationList);  // minvalue = 1



implementation

uses
    SysUtils, fvm.Strings;

{$IFDEF DEBUG}
var
   OneConst : cardinal = 1;
   TwoConst : cardinal = 2;
{$ENDIF}

function NextCombination(var Combination: PCombination; SetSize: integer): boolean;
var
   i: integer;
begin
  i := Combination^.Places-1;
  inc(Combination^.Data[i]);
  while (i > 0) and (Combination^.Data[i] >= SetSize-Combination^.Places+1+i) do
  begin
    dec(i);
    inc(Combination^.Data[i]);
  end;

  if Combination^.Data[0] > SetSize-Combination^.Places then   // combination (n-k, n-k+1, ..., n) reached
  begin
    // No more combinations can be generated
    Result := FALSE;
    Exit;
  end;

  // Combination^ now looks like (..., x, n, n, n, ..., n).
  // turn it into (..., x, x + 1, x + 2, ...)
  for i := i+1 to Combination^.Places-1 do
    Combination^.Data[i] := Combination^.Data[i-1]+1;

  Result := TRUE;
end;

procedure CalculateCombinations(Places, MaxValue: cardinal; ResultList: TCombinationList);
var
   combo         : PCombination;
   i, j, setsize : cardinal;
begin
  ResultList.Clear;
  {$IFDEF DEBUG}
  if Places < OneConst then
    Exit;
  if Places < TwoConst then
    Exit;
  {$ENDIF}

  // initial combination
  combo := TCombinationList.CreateNewItem(Places);
  for i := 0 to Places-1 do
    combo^.Data[i] := i;
  ResultList.Add(combo);

  // all other combinations
  setsize := MaxValue;
  combo := TCombinationList.CreateNewItem(Places, combo);
  while NextCombination(combo, setsize) do
  begin
    ResultList.Add(combo);
    combo := TCombinationList.CreateNewItem(Places, combo);
  end;

  // free last combination we allocated, as it's not used
  FreeMem(combo);

  // make sure the value range is correct
  for i := 0 to ResultList.Count-1 do
  begin
    combo := ResultList.Items[i];
    for j := 0 to Places-1 do
      combo^.Data[j] := combo^.Data[j]+1;
  end;
end;

{ TCombinationList }

procedure TCombinationList.Add(Item: PCombination);
begin
  Lock.Enter;
  try
    if Assigned(FLast) then
      FLast^.Next := Item;

    Item^.Prev := FLast;
    Item^.Next := nil;

    if not Assigned(FFirst) then
      FFirst := Item;
    FLast := Item;

    inc(FCount);
  finally
    Lock.Leave;
  end;
end;

procedure TCombinationList.Clear;
var
   prev, next: PCombination;
begin
  Lock.Enter;
  try
    next := FFirst;
    FFirst := nil;
    FLast := nil;
    FCount := 0;

    while Assigned(next) do
    begin
      prev := next;
      next := next^.Next;
      FreeMem(prev);
    end;
  finally
    Lock.Leave;
  end;
end;

constructor TCombinationList.Create;
begin
  inherited;

  FFirst := nil;
  FLast := nil;
  FCount := 0;

  Lock := TCriticalSection.Create;
end;

function TCombinationList.CreateItemFromString(const Line: string): PCombination;
var
   s      : string;
   value  : cardinal;
   values : array of cardinal;
   combo  : PCombination;
   i      : integer;
begin
  Result := nil;

  SetLength(values, 0);

  s := Trim(Line);
  while s <> '' do
  begin
    value := StrToIntDef(RetrieveNextValueFrom(s, ' '), 0);
    if value = 0 then
      Break;

    SetLength(values, Length(values)+1);
    values[High(values)] := value;
  end;

  if Length(values) = 0 then
    Exit;

  combo := CreateNewItem(Length(values), 0);
  for i := 0 to combo^.Places-1 do
    combo^.Data[i] := values[i];

  SetLength(values, 0);

  Result := combo;
end;

class function TCombinationList.CreateNewItem(Places: cardinal; CopyFrom: PCombination = nil): PCombination;
var
   i, cnt  : integer;
   newsize : int64;
   item    : PCombination;
begin
  newsize := SizeOf(TCombination) + (Places-1) * SizeOf(cardinal);
  GetMem(item, newsize);

  FillChar(item^, newsize, 0);
  item^.Places := Places;

  if Assigned(CopyFrom) then
  begin
    cnt := Places;
    if cnt > CopyFrom^.Places then
      cnt := CopyFrom^.Places;
    for i := 0 to cnt-1 do
      item^.Data[i] := CopyFrom^.Data[i];
  end;

  Result := item;
end;

procedure TCombinationList.Delete(Item: PCombination; FreeAsWell: boolean);
begin
  Lock.Enter;
  try
    if Assigned(Item^.Prev) then  Item^.Prev^.Next := Item^.Next;
    if Assigned(Item^.Next) then  Item^.Next^.Prev := Item^.Prev;
    if Item = FFirst then         FFirst := Item^.Next;
    if Item = FLast then          FLast := Item^.Prev;

    dec(FCount);
  finally
    Lock.Leave;
  end;

  if FreeAsWell then
    FreeMem(Item);
end;

destructor TCombinationList.Destroy;
begin
  Clear;
  FreeAndNil(Lock);

  inherited;
end;

function TCombinationList.GetItems(Index: integer): PCombination;
var
   combo : PCombination;
   idx   : integer;
begin
  Result := nil;

  Lock.Enter;
  try
    idx := 0;
    combo := FFirst;
    while Assigned(combo) do
    begin
      if idx = Index then
        Exit(combo);

      combo := combo^.Next;
      inc(idx);
    end;
  finally
    Lock.Leave;
  end;
end;

procedure TCombinationList.LoadFromFile(const FileName: string);
var
   sl    : TStringList;
   line  : string;
   combo : PCombination;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(FileName);

    Lock.Enter;
    try
      Clear;
      for line in sl do
      begin
        combo := CreateItemFromString(line);
        if Assigned(combo) then
          Add(combo);
      end;
    finally
      Lock.Leave;
    end;
  finally
    sl.Free;
  end;
end;

procedure TCombinationList.SaveToFile(const FileName: string);
var
   sl    : TStringList;
   line  : string;
   combo : PCombination;
begin
  sl := TStringList.Create;
  try
    Lock.Enter;
    try
      combo := First;
      while Assigned(combo) do
      begin
        line := combo^.ToString;
        sl.Add(line);
      end;
    finally
      Lock.Leave;
    end;

    sl.SaveToFile(FileName);
  finally
    sl.Free;
  end;
end;

function TCombinationList.ToStrings: TStrings;
var
   combo: PCombination;
begin
  Result := TStringList.Create;

  Lock.Enter;
  try
    combo := First;
    while Assigned(combo) do
    begin
      Result.Add(combo^.ToString);
      combo := combo^.Next;
    end;
  finally
    Lock.Leave;
  end;
end;

{ TCombination }

function TCombination.ContainsOtherCombination(Other: PCombination): boolean;
var
   i, j  : integer;
   found : boolean;
begin
  for i := 0 to Other^.Places-1 do
  begin
    for j := 0 to Places-1 do
    begin
      found := (Other^.Data[i] = Data[j]);
      if found then
        Break;
    end;
    if not found then
      Exit(FALSE);
  end;

  Result := TRUE;
end;

function TCombination.ToString: string;
var
   i: integer;
begin
  Result := '';
  for i := 0 to Places-1 do
    Result := Result + IntToStr(Data[i]) + ' ';
end;

end.
