unit calculator;

interface

uses
    Classes, SyncObjs, fvm.Combinations, Generics.Collections;

procedure FindCombinationsIncludingAllOfLesser(ResultList, SourceList, LesserList: TCombinationList);

(*
type
    TCalculatorThread = class(TThread)
    private
      SourceList, LesserList: TCombinationList;
    public
      Event         : TEvent;
      FinishedEvent : TEvent;
      Combo         : PCombination;
      IsUnique      : boolean;
      LesserCombos  : TList<PCombination>;

      constructor Create(SetSourceList, SetLesserList: TCombinationList);
      destructor Destroy; override;

      procedure Execute; override;

      procedure StartNewJob(SetCombo: PCombination);
      procedure PrepareToDestroy;
    end;
*)



implementation

uses
    SysUtils;

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
      ResultList.Add(current);
      current := next;
    end;
  finally
    newlist.Free;
  end;
end;

function FindUniqueCombination(SourceList: TCombinationList; StartIndex: integer; ResultList, LesserList: TCombinationList; RemoveFromSource, RemoveFromLesser: boolean): PCombination;
var
   combo, lesser       : PCombination;
   idx, lessercount, i : integer;
   usedcombos          : array of PCombination;
begin
  Result := nil;
  if (StartIndex < 0) or (StartIndex >= SourceList.Count) then
    Exit;

  SetLength(usedcombos, 100);

  idx := StartIndex;
  while TRUE do
  begin
    combo := SourceList.Items[idx];
    inc(idx);
    if idx >= SourceList.Count then
      idx := 0;
    if idx = StartIndex then
      Break;

    lessercount := 0;
    lesser := LesserList.First;
    while Assigned(lesser) do
    begin
      if combo^.ContainsOtherCombination(lesser) then
      begin
        usedcombos[lessercount] := lesser;
        inc(lessercount);
      end;
      lesser := lesser^.Next;
    end;
    if lessercount = 20 then
    begin
      Result := combo;
      Break;
    end;
  end;

  if Assigned(Result) then
  begin
    if RemoveFromSource then
      SourceList.Delete(Result, FALSE);
    if RemoveFromLesser then for i := 0 to lessercount-1 do
      LesserList.Delete(usedcombos[i], TRUE);
  end;
end;

procedure FindCombinationsIncludingAllOfLesser(ResultList, SourceList, LesserList: TCombinationList);
{const
     threadcount = 8;}
var
   curidx              : integer;
   combo, lesser, next : PCombination;
   done                : boolean;
   //threads             : array[0..threadcount-1] of TCalculatorThread;
   i                   : integer;
begin
  if not Assigned(ResultList) or not Assigned(SourceList) or not Assigned(LesserList) then
    Exit;
  if (SourceList.Count = 0) or (LesserList.Count = 0) then
    Exit;

  ResultList.Clear;

  curidx := SourceList.Count div 2;
  while (SourceList.Count > 0) and (LesserList.Count > 0) do
  begin
    curidx := SourceList.Count div 2;
    combo := FindUniqueCombination(SourceList, curidx, ResultList, LesserList, TRUE, TRUE);
    if not Assigned(combo) then
      Break;

    ResultList.Add(combo);
  end;
end;

{ TCalculatorThread }

(*
constructor TCalculatorThread.Create;
begin
  inherited Create(FALSE);

  Event := TEvent.Create(nil, TRUE, TRUE, '');
  FinishedEvent := TEvent.Create(nil, TRUE, TRUE, '');
end;

destructor TCalculatorThread.Destroy;
begin
  FreeAndNil(Event);
  FreeAndNil(FinishedEvent);

  inherited;
end;

procedure TCalculatorThread.Execute;
begin
  Event.WaitFor(INFINITE);
  ddd
end;

procedure TCalculatorThread.PrepareToDestroy;
begin
  Terminate;
  Event.SetEvent;
  WaitFor;
end;

procedure TCalculatorThread.StartNewJob(SetCombo: PCombination);
begin
  Combo := SetCombo;
  Event.SetEvent;
  FinishedEvent.ResetEvent;
end;
*)

end.
