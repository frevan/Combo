unit fvm.Combinations;

interface

uses
    Generics.Collections, Classes, SyncObjs;



function CalculateNumberOfCombinations(NumPlaces, MaxValue: cardinal): cardinal;



const MaxPlacesConst      = 6;

type  TCombination = uint64;
      TCombinationRec = packed record
        case integer of
          0 :  (MaxValue, NumPlaces: byte; Data: array[0..MaxPlacesConst-1] of byte);
          1 :  (Blob: TCombination);
      end;

function CombinationToString(Source: TCombination): string;
function StringToCombination(const Line: string): TCombination; inline;



type
    PCombinationListData = ^TCombinationListData;
    TCombinationListData = array[0..0] of TCombination;

    TCombinationList = class
    private
      FCount    : integer;
      FCapacity : integer;
      Lock      : TCriticalSection;
      function GetItems(Index: integer): TCombination;
      procedure SetItems(Index: integer; NewValue: TCombination);
      procedure SetCapacity(NewValue: integer);
      function GetNumPlaces: cardinal;
      function GetMaxValue: cardinal;
      function ToRawStrings: TStrings;
    public
      FItems    : PCombinationListData;

      constructor Create;
      destructor Destroy; override;

      procedure Add(Item: TCombination); overload;
      procedure Delete(Index: integer);
      procedure Remove(Item: TCombination);
      procedure Clear;

      procedure Assign(Source: TCombinationList);

      procedure SaveToFile(const FileName: string);
      procedure LoadFromFile(const FileName: string);

      function ToStrings: TStrings;

      property Capacity: integer read FCapacity write SetCapacity;
      property Count: integer read FCount;
      property Items[Index: integer]: TCombination read GetItems write SetItems;
      property NumPlaces: cardinal read GetNumPlaces;
      property MaxValue: cardinal read GetMaxValue;
    end;



procedure CalculateCombinations(NumPlaces, MaxValue: cardinal; ResultList: TCombinationList);  // minvalue = 1



implementation

uses
    SysUtils, fvm.Strings, Windows;

const FlagMaskConst      = $80;
      FlagShiftConst     = 7;
      MaxValueMaskConst  = $7F;
      MaxValueShiftConst = 0;

      FileHeaderConst  = $80907050;
      FileVersionConst = 1;


function FactorialIterative(aNumber: integer): int64;
var
  i: Integer;
begin
  Result := 1;
  for i := 1 to aNumber do
    Result := i * Result;
end;

function CombinationToString(Source: TCombination): string;
var   i: integer;
      r: TCombinationRec;
begin
  r.Blob := Source;

  Result := '';
  for i := 0 to r.NumPlaces-1 do
    Result := Result + IntToStr(r.Data[i]) + ' ';
end;

function StringToCombination(const Line: string): TCombination;
var   s: string;
      r: TCombinationRec;
begin
  r.Blob := 0;

  s := Trim(Line);
  r.NumPlaces := 0;
  while (s <> '') and (r.NumPlaces < MaxPlacesConst) do
  begin
    r.Data[r.NumPlaces] := StrToIntDef(RetrieveNextValueFrom(s, ' '), 0);
    if r.Data[r.NumPlaces] = 0 then
      Break;
    inc(r.NumPlaces);
  end;

  Result := r.Blob;
end;

function CalculateNumberOfCombinations(NumPlaces, MaxValue: cardinal): cardinal;
begin
  Result := FactorialIterative(MaxValue) div (FactorialIterative(NumPlaces)*FactorialIterative(MaxValue-NumPlaces));
end;

{$IFDEF DEBUG}
var
   OneConst : cardinal = 1;
   TwoConst : cardinal = 2;
{$ENDIF}

function NextCombination(var Combination: TCombination; NumPlaces, MaxValue: cardinal): boolean;
var   i: integer;
      r: TCombinationRec;
begin
  r.Blob := Combination;

  i := NumPlaces-1;
  inc(r.Data[i]);
  while (i > 0) and (r.Data[i] >= MaxValue-NumPlaces+1+i) do
  begin
    dec(i);
    inc(r.Data[i]);
  end;

  if r.Data[0] > MaxValue-NumPlaces then   // combination (n-k, n-k+1, ..., n) reached
  begin
    // No more combinations can be generated
    Result := FALSE;
    Exit;
  end;

  // Combination^ now looks like (..., x, n, n, n, ..., n).
  // turn it into (..., x, x + 1, x + 2, ...)
  for i := i+1 to NumPlaces-1 do
    r.Data[i] := r.Data[i-1]+1;

  r.MaxValue := MaxValue;
  r.NumPlaces := NumPlaces;
  Combination := r.Blob;

  Result := TRUE;
end;

procedure CalculateCombinations(NumPlaces, MaxValue: cardinal; ResultList: TCombinationList);
var   r     : TCombinationRec;
      i, j  : cardinal;
      combo : TCombination;

  function ComboPlusOne(const Source: TCombination): TCombination;
  var   counter : cardinal;
        r2      : TCombinationRec;
  begin
    r2.Blob := Source;
    for counter := 0 to r2.NumPlaces-1 do
      inc(r2.Data[counter]);
    Result := r2.Blob;
  end;

begin
  ResultList.Clear;
  {$IFDEF DEBUG}
  if NumPlaces < OneConst then  Exit;
  if NumPlaces < TwoConst then  Exit;
  {$ENDIF}

  // initial combination
  r.Blob := 0;
  r.MaxValue := MaxValue;
  r.NumPlaces := NumPlaces;
  for i := 0 to NumPlaces-1 do
    r.Data[i] := i;
  combo := r.Blob;
  ResultList.Add(ComboPlusOne(combo));

  // all other combinations
  while NextCombination(combo, NumPlaces, MaxValue) do
    ResultList.Add(ComboPlusOne(combo));
end;

{ TCombinationList }

procedure TCombinationList.Add(Item: TCombination);
begin
  Lock.Enter;
  try
    if FCount = FCapacity then
      SetCapacity(FCapacity+128);
    FItems^[FCount] := Item;
    inc(FCount);
  finally
    Lock.Leave;
  end;
end;

procedure TCombinationList.Assign(Source: TCombinationList);
begin
  Lock.Enter;
  try
    SetCapacity(Source.Capacity);
    FCount := Source.FCount;
    CopyMemory(FItems, Source.FItems, SizeOf(TCombination) * FCount);
  finally
    Lock.Leave;
  end;
end;

procedure TCombinationList.Clear;
begin
  Lock.Enter;
  try
    FCount := 0;
  finally
    Lock.Leave;
  end;
end;

constructor TCombinationList.Create;
begin
  inherited;

  FItems := nil;
  FCount := 0;
  FCapacity := 0;

  Lock := TCriticalSection.Create;
end;

procedure TCombinationList.Delete(Index: integer);
var   i: integer;
begin
  Lock.Enter;
  try
    if Index <= FCount-2 then for i := Index+1 to FCount-1 do
      FItems^[i-1] := FItems^[i];
    dec(FCount);
  finally
    Lock.Leave;
  end;
end;

destructor TCombinationList.Destroy;
begin
  FreeMem(FItems);
  FItems := nil;
  FreeAndNil(Lock);

  inherited;
end;

function TCombinationList.GetItems(Index: integer): TCombination;
begin
  Result := 0;

  Lock.Enter;
  try
    if (Index >= 0) or (Index < FCount) then
      Result := FItems[Index];
  finally
    Lock.Leave;
  end;
end;

function TCombinationList.GetMaxValue: cardinal;
begin
  Result := 0;
  if FCount > 0 then
    Result := TCombinationRec(FItems^[0]).MaxValue;
end;

function TCombinationList.GetNumPlaces: cardinal;
begin
  Result := 0;
  if FCount > 0 then
    Result := TCombinationRec(FItems^[0]).NumPlaces;
end;

procedure TCombinationList.LoadFromFile(const FileName: string);
var   m                    : TMemoryStream;
      header, version, cnt : longword;
begin
  Clear;
  SetCapacity(0);

  m := TMemoryStream.Create;
  try
    m.LoadFromFile(FileName);
    m.Position := 0;

    m.Read(header, 4);
    if header <> FileHeaderConst then
      Exit;

    m.Read(version, 4);
    if version > FileVersionConst then
      Exit;

    m.Read(cnt, 4);

    Lock.Enter;
    try
      SetCapacity(cnt);
      m.Read(FItems^, SizeOf(TCombination) * cnt);
      FCount := cnt;
    finally
      Lock.Leave;
    end;
  finally
    m.Free;
  end;
end;

procedure TCombinationList.Remove(Item: TCombination);
var   i: integer;
begin
  Lock.Enter;
  try
    for i := 0 to FCount-1 do if FItems[i] = Item then
    begin
      Delete(i);
      Break;
    end;
  finally
    Lock.Leave;
  end;
end;

procedure TCombinationList.SaveToFile(const FileName: string);
var   m : TMemoryStream;
      n : longword;
begin
  m := TMemoryStream.Create;
  try
    n := FileHeaderConst;
    m.Write(n, 4);

    n := FileVersionConst;
    m.Write(n, 4);

    Lock.Enter;
    try
      n := FCount;
      m.Write(n, 4);

      m.Write(FItems^, SizeOf(TCombination) * FCount);
    finally
      Lock.Leave;
    end;

    m.SaveToFile(FileName);
  finally
    m.Free;
  end;
end;

procedure TCombinationList.SetCapacity(NewValue: integer);
begin
  FCapacity := NewValue;
  if FCapacity < 0 then
    FCapacity := 0;

  ReallocMem(FItems, FCapacity * SizeOf(TCombination));
  if FCount > FCapacity then
    FCount := FCapacity;
end;

procedure TCombinationList.SetItems(Index: integer; NewValue: TCombination);
begin
  FItems[Index] := NewValue;
end;

function TCombinationList.ToRawStrings: TStrings;
var   i: integer;
begin
  Result := TStringList.Create;

  Lock.Enter;
  try
    for i := 0 to FCount-1 do
      Result.Add(Format('%d', [FItems^[i]]));
  finally
    Lock.Leave;
  end;
end;

function TCombinationList.ToStrings: TStrings;
var   i: integer;
begin
  Result := TStringList.Create;

  Lock.Enter;
  try
    for i := 0 to FCount-1 do
      Result.Add(CombinationToString(FItems^[i]));
  finally
    Lock.Leave;
  end;
end;

{ TCombination }

(*
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
*)

end.
