unit fvm.Strings;

interface

function RetrieveNextValueFrom(var s: string; Separator: char): string;



implementation

uses
    SysUtils;

function RetrieveNextValueFrom(var s: string; Separator: char): string;
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

end.
