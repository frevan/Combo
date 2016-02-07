unit calculator;

interface

uses
    fvm.Combinations;

procedure FindCombinationsIncludingAllOfLesser(ResultList, SourceList, LesserList: TCombinationList);



implementation

procedure FindAllCombinationsFrom(Combo: PCombination; Places: cardinal; ResultList: TCombinationList);
var
   current, next : PCombination;
   newlist       : TCombinationList;
   i             : integer;
begin
  ResultList.Clear;

  newlist := TCombinationList.Create;
  try
    CalculateCombinations(Places, Combo^.Places, newlist);

    current := newlist.First;
    while Assigned(current) do
    begin
      next := current^.Next;
      for i := 0 to current^.Places-1 do
        current^.Data[i] := Combo^.Data[current^.Data[i]];
      current.MaxValue := Combo^.MaxValue;
      ResultList.Add(current);
      current := next;
    end;
  finally
    newlist.Free;
  end;
end;

function FindUniqueCombination(SourceList: TCombinationList; StartIndex: integer; ResultList, LesserList: TCombinationList): PCombination;
begin
  Result := nil;
end;

procedure FindCombinationsIncludingAllOfLesser(ResultList, SourceList, LesserList: TCombinationList);
var
   curidx              : integer;
   combo, lesser, next : PCombination;
   done                : boolean;
   sourceplaces        : cardinal;
begin
  if not Assigned(ResultList) or not Assigned(SourceList) or not Assigned(LesserList) then
    Exit;
  if (SourceList.Count = 0) or (LesserList.Count = 0) then
    Exit;

  sourceplaces := SourceList.First^.Places;

  ResultList.Clear;

  curidx := SourceList.Count div 2;
  combo := SourceList.Items[curidx];
  SourceList.Delete(combo, FALSE);
  ResultList.Add(combo);

  while TRUE do
  begin
    curidx := SourceList.Count div 2;
    combo := FindUniqueCombination(SourceList, curidx, ResultList, LesserList);
    if not Assigned(combo) then
      Break;
    SourceList.Delete(combo, FALSE);
    ResultList.Add(combo);

    // remove combinations from lesser list
    lesser := LesserList.First;
    while Assigned(lesser) do
    begin
      next := lesser^.Next;
      if combo^.ContainsOtherCombination(lesser) then
        LesserList.Delete(lesser, TRUE);
      lesser := next;
    end;

    if (LesserList.Count = 0) or (SourceList.Count = 0) then
      Break;
  end;
end;

end.
