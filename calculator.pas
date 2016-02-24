unit calculator;

interface

uses
    Classes, SyncObjs, fvm.Combinations, Generics.Collections;

procedure FindCombinationsIncludingAllOf(ResultList, SourceList, IncludeList: TCombinationList);
procedure FindRandomCombinations(ResultList, SourceList: TCombinationList; Count: integer);

function CalculateHighestSubCountOf(Combo1, Combo2: TCombination; MaxPlaces: cardinal = MaxInt): integer;



implementation

uses
    SysUtils, Generics.Defaults;

type
    TComboCombo = record
      Used : boolean;
      Main : TCombinationRec;
      Subs : array[0..19] of TCombinationRec;
    end;

    TComboComboArray = array[0..0] of TComboCombo;
    PComboComboArray = ^TComboComboArray;

type
    TCalculatorThread = class(TThread)
    public
      Subs  : TArray<TCombination>;
      Combo : TCombination;
      Found : boolean;
      procedure Execute; override;
    end;

    TCalculator = class
    private
      Threads        : TList<TCalculatorThread>;
      Src            : PComboComboArray;
      SrcLength      : cardinal;
      SrcIndex       : cardinal;
      Subs           : TArray<TCombination>;
      SubsIndex      : cardinal;
      SubsRequired   : cardinal;
      SubsInSrcCount : cardinal;
      procedure CreateSrcArray(SourceList: TCombinationList; SubNumPlaces: cardinal);
      procedure CreateSubArray(SubList: TCombinationList);
      function FindUnique(UniqueSubsRequired: integer): TCombination;
    public
      Results: TCombinationList;

      constructor Create;
      destructor Destroy; override;

      procedure Calculate(SourceList, SubsList: TCombinationList);
    end;



procedure FindCombinationsIncludingAllOf(ResultList, SourceList, IncludeList: TCombinationList);
var
   calc : TCalculator;
begin
  calc := TCalculator.Create;
  try
    calc.Calculate(SourceList, IncludeList);
    ResultList.Assign(calc.Results);
  finally
    calc.Free;
  end;
end;

procedure FindRandomCombinations(ResultList, SourceList: TCombinationList; Count: integer);
var   src    : TCombinationList;
      i, idx : integer;
      combo  : TCombination;
begin
  ResultList.Clear;

  src := TCombinationList.Create;
  try
    src.Assign(SourceList);
    for i := 0 to Count-1 do
    begin
      idx := Random(src.Count);
      combo := src.Items[idx];
      src.Delete(idx);
      ResultList.Add(combo);
    end;
  finally
    src.Free;
  end;
end;

function CalculateHighestSubCountOf(Combo1, Combo2: TCombination; MaxPlaces: cardinal): integer;
var   r1, r2 : TCombinationRec;
      l1, l2 : TCombinationList;
      places : cardinal;
      i, j   : integer;
      found  : boolean;
      maxplc : cardinal;

  procedure CombosNow(List: TCombinationList; const Combination: TCombinationRec);
  var   counter, counter2 : integer;
        rn      : TCombinationRec;
  begin
    List.Clear;
    CalculateCombinations(places, r1.NumPlaces, List);
    for counter := 0 to List.Count-1 do
    begin
      rn.Blob := List.Items[counter];
      for counter2 := 0 to places-1 do
        rn.Data[counter2] := Combination.Data[rn.Data[counter2]-1];
      List.Items[counter] := rn.Blob;
    end;
  end;

begin
  Result := 0;

  l1 := TCombinationList.Create;
  l2 := TCombinationList.Create;

  r1.Blob := Combo1;
  r2.Blob := Combo2;

  maxplc := r1.NumPlaces;
  if MaxPlaces < maxplc then
    maxplc := MaxPlaces;

  for places := maxplc downto 1 do
  begin
    CombosNow(l1, r1);
    CombosNow(l2, r2);

    found := FALSE;
    for i := 0 to l1.Count-1 do for j := 0 to l2.Count-1 do
    begin
      if l1.Items[i] = l2.Items[j] then
      begin
        Result := places;
        found := TRUE;
        Break;
      end;
    end;
    if found then
      Break;
  end;

  l1.Free;
  l2.Free;
end;

{ TCalculatorThread }

procedure TCalculatorThread.Execute;
begin
  {}
end;

{ TCalculator }

procedure TCalculator.Calculate(SourceList, SubsList: TCombinationList);
var   combo: TCombination;
begin
  Results.Clear;
  if not Assigned(SourceList) or not Assigned(SubsList) then
    Exit;

  CreateSrcArray(SourceList, SubsList.NumPlaces);
  if not Assigned(Src) then
    Exit;

  CreateSubArray(SubsList);
  if Length(Subs) = 0 then
  begin
    FreeMem(Src);
    Src := nil;
    Exit;
  end;

  SrcIndex := 0;
  SubsIndex := 0;
  SubsRequired := SubsInSrcCount;
  repeat
    combo := FindUnique(SubsRequired);
    if combo = 0 then
    begin
      dec(subsrequired);
      srcindex := 0;
    end
    else
      Results.Add(combo);
  until (SubsRequired = 0) or (SubsIndex >= Length(Subs));

  FreeMem(Src);
  Src := nil;
  SrcLength := 0;
  SetLength(Subs, 0);
end;

constructor TCalculator.Create;
begin
  inherited;

  Threads := TList<TCalculatorThread>.Create;
  Results := TCombinationList.Create;
end;

procedure TCalculator.CreateSrcArray(SourceList: TCombinationList; SubNumPlaces: cardinal);
var   i, j, k : integer;
      clist   : TCombinationList;
      r       : TCombinationRec;
begin
  SubsInSrcCount := 0;

  GetMem(Src, SizeOf(TComboCombo) * SourceList.Count);
  SrcLength := SourceList.Count;

  clist := TCombinationList.Create;
  try
    CalculateCombinations(SubNumPlaces, SourceList.NumPlaces, clist);
    if clist.Count > 20 then
    begin
      FreeMem(Src);
      Src := nil;
      SrcLength := 0;
      Exit;
    end;

    SubsInSrcCount := clist.Count;

    for i := 0 to SourceList.Count-1 do with Src^[i] do
    begin
      Used := FALSE;
      Main.Blob := SourceList.Items[i];
      for j := 0 to clist.Count-1 do
      begin
        r.Blob := clist.Items[j];
        Subs[j].MaxValue := SourceList.MaxValue;
        Subs[j].NumPlaces := SubNumPlaces;
        for k := 0 to SubNumPlaces-1 do
          Subs[j].Data[k] := Main.Data[r.Data[k]-1];
      end;
    end;
  finally
    clist.Free;
  end;
end;

procedure TCalculator.CreateSubArray(SubList: TCombinationList);
var   i: integer;
begin
  Subs := TArray<TCombination>.Create();
  SetLength(Subs, SubList.Count);
  for i := 0 to Length(Subs)-1 do
    Subs[i] := 0;
end;

destructor TCalculator.Destroy;
begin
  SetLength(Subs, 0);
  FreeMem(Src);
  Results.Free;
  Threads.Free;

  inherited;
end;

function TCalculator.FindUnique(UniqueSubsRequired: integer): TCombination;
var   i, j, k, startidx : cardinal;
      subplaces         : cardinal;
      sub               : TCombination;
      remaining         : integer;
      foundidxs         : array of boolean;
begin
  Result := 0;

  SetLength(foundidxs, SubsInSrcCount);

  startidx := SrcIndex;
  for i := startidx to SrcLength-1 do
  begin
    inc(SrcIndex);
    if Src^[i].Used then
      Continue;

    remaining := SubsInSrcCount;
    for j := 0 to SubsInSrcCount-1 do
    begin
      sub := Src^[i].Subs[j].Blob;
      foundidxs[j] := FALSE;
      for k := 0 to Length(Subs)-1 do
      begin
        if Subs[k] = sub then
        begin
          foundidxs[j] := TRUE;
          Break;
        end
        else if Subs[k] = 0 then
          Break;
      end;
      if foundidxs[j] then
      begin
        dec(remaining);
        if remaining < UniqueSubsRequired then
          Break;
      end;
    end;

    if remaining >= UniqueSubsRequired then
    begin
      for j := 0 to SubsInSrcCount-1 do if not foundidxs[j] then
      begin
        Subs[SubsIndex] := Src^[i].Subs[j].Blob;
        inc(SubsIndex);
      end;
      Src^[i].Used := TRUE;
      Result := Src^[i].Main.Blob;
      Break;
    end;
  end;
end;

end.
