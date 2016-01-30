unit fvm.Combinations;

interface

uses
    Generics.Collections;

type
    TCombination = array[0..0] of cardinal;
    PCombination = ^TCombination;

    TCombinationList = class
    private
      FItems : TList<PCombination>;
      function GetCount: integer;
      function GetItems(Index: integer): PCombination;
    public
      constructor Create;
      destructor Destroy; override;

      procedure Clear;

      property Count: integer read GetCount;
      property Items[Index: integer]: PCombination read GetItems;
    end;



procedure CalculateCombinations(Places, MinValue, MaxValue: cardinal; ResultList: TCombinationList);



implementation

{$IFDEF DEBUG}
var
   OneConst : cardinal = 1;
   TwoConst : cardinal = 2;
{$ENDIF}

function CreateNewItem(Places: cardinal; Previous: PCombination = nil): PCombination;
var
   i: integer;
begin
  GetMem(Result, SizeOf(cardinal)*Places);

  if Assigned(Previous) then for i := 0 to Places-1 do
    Result^[i] := Previous^[i];
end;

function NextCombination(var Combination: PCombination; Places, SetSize: integer): boolean;
var
   i: integer;
begin
  i := Places-1;
  inc(Combination^[i]);
  while (i > 0) and (Combination^[i] >= SetSize-Places+1+i) do
  begin
    dec(i);
    inc(Combination^[i]);
  end;

  if Combination^[0] > SetSize-Places then   // combination (n-k, n-k+1, ..., n) reached
  begin
    // No more combinations can be generated
    Result := FALSE;
    Exit;
  end;

  // Combination^ now looks like (..., x, n, n, n, ..., n).
  // turn it into (..., x, x + 1, x + 2, ...)
  for i := i+1 to Places-1 do
    Combination^[i] := Combination^[i-1]+1;

  Result := TRUE;
end;

procedure CalculateCombinations(Places, MinValue, MaxValue: cardinal; ResultList: TCombinationList);
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
  combo := CreateNewItem(Places);
  for i := 0 to Places-1 do
    combo^[i] := i;
  ResultList.FItems.Add(combo);
  // all other combinations
  setsize := MaxValue-MinValue+1;
  combo := CreateNewItem(Places, combo);
  while NextCombination(combo, Places, setsize) do
  begin
    ResultList.FItems.Add(combo);
    combo := CreateNewItem(Places, combo);
  end;
  // free last combination we allocated, as it's not used
  FreeMem(combo);

  // make sure the value range is correct
  for i := 0 to ResultList.Count-1 do
  begin
    combo := ResultList.Items[i];
    for j := 0 to Places-1 do
      combo^[j] := combo^[j] + MinValue;
  end;
end;

{ TCombinationList }

procedure TCombinationList.Clear;
var
   combination: PCombination;
begin
  for combination in FItems do
    FreeMem(combination);
  FItems.Clear;
end;

constructor TCombinationList.Create;
begin
  inherited;

  FItems := TList<PCombination>.Create;
end;

destructor TCombinationList.Destroy;
begin
  Clear;
  FItems.Free;

  inherited;
end;

function TCombinationList.GetCount: integer;
begin
  Result := FItems.Count;
end;

function TCombinationList.GetItems(Index: integer): PCombination;
begin
  Result := FItems[Index];
end;

end.
