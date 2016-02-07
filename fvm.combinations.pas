unit fvm.Combinations;

interface

uses
    Generics.Collections;

type
    PCombination = ^TCombination;
    TCombination = record
      Next, Prev : PCombination;
      Places     : cardinal;
      MaxValue   : cardinal;
      Data       : array[0..0] of cardinal;
      function ContainsOtherCombination(Other: PCombination): boolean;
    end;


    TCombinationList = class
    private
      FFirst  : PCombination;
      FLast   : PCombination;
      FCount  : integer;
      function GetItems(Index: integer): PCombination;
    public
      constructor Create;
      destructor Destroy; override;

      class function CreateNewItem(Places, MaxValue: cardinal; CopyFrom: PCombination = nil): PCombination;
      procedure Add(Item: PCombination);
      procedure Delete(Item: PCombination; FreeAsWell: boolean);
      procedure Clear;

      property First: PCombination read FFirst;
      property Last: PCombination read FLast;
      property Count: integer read FCount;
      property Items[Index: integer]: PCombination read GetItems;
    end;



procedure CalculateCombinations(Places, MaxValue: cardinal; ResultList: TCombinationList);  // minvalue = 1



implementation

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
  combo := TCombinationList.CreateNewItem(Places, MaxValue);
  for i := 0 to Places-1 do
    combo^.Data[i] := i;
  ResultList.Add(combo);

  // all other combinations
  setsize := MaxValue;
  combo := TCombinationList.CreateNewItem(Places, MaxValue, combo);
  while NextCombination(combo, setsize) do
  begin
    ResultList.Add(combo);
    combo := TCombinationList.CreateNewItem(Places, MaxValue, combo);
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
  Item^.Prev := FLast;

  if not Assigned(FFirst) then
  begin
    FFirst := Item;
    FLast := Item;
  end;

  if Assigned(FLast) then
    FLast^.Next := Item;

  inc(FCount);
end;

procedure TCombinationList.Clear;
var
   prev, next: PCombination;
begin
  next := FFirst;
  FFirst := nil;
  FCount := 0;

  while Assigned(next) do
  begin
    prev := next;
    next := next^.Next;
    FreeMem(prev);
  end;
end;

constructor TCombinationList.Create;
begin
  inherited;
  FFirst := nil;
  FCount := 0;
end;

class function TCombinationList.CreateNewItem(Places, MaxValue: cardinal; CopyFrom: PCombination = nil): PCombination;
var
   i, cnt  : integer;
   newsize : int64;
   item    : PCombination;
begin
  newsize := SizeOf(TCombination) + (Places-1) * SizeOf(cardinal);
  GetMem(item, newsize);

  FillChar(item^, newsize, 0);
  item^.Places := Places;
  item^.MaxValue := MaxValue;

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
  if Assigned(Item^.Prev) then  Item^.Prev^.Next := Item^.Next;
  if Assigned(Item^.Next) then  Item^.Next^.Prev := Item^.Prev;
  if Item = FFirst then         FFirst := Item^.Next;
  if Item = FLast then          FLast := Item^.Prev;

  dec(FCount);

  if FreeAsWell then
    FreeMem(Item);
end;

destructor TCombinationList.Destroy;
begin
  Clear;
  inherited;
end;

function TCombinationList.GetItems(Index: integer): PCombination;
var
   combo : PCombination;
   idx   : integer;
begin
  Result := nil;

  idx := 0;
  combo := FFirst;
  while Assigned(combo) do
  begin
    if idx = Index then
      Exit(combo);

    combo := combo^.Next;
    inc(idx);
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
    found := FALSE;
    for j := 0 to Places-1 do
      found := found or (Other^.Data[i] = Data[j]);
    if not found then
      Exit(FALSE);
  end;

  Result := TRUE;
end;

end.
