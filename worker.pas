unit worker;

interface

uses
    Classes, Generics.Collections, Windows, fvm.Combinations;

type
    TCalculationThread = class(TThread)
    public
      Num, Max           : integer;
      List               : TCombinationList;
      NotificationWindow : THandle;
      NotificationMsg    : cardinal;
      Cancelled          : boolean;

      constructor Create;
      destructor Destroy; override;
      procedure Execute; override;
    end;



implementation

{ TCalculationThread }

constructor TCalculationThread.Create;
begin
  inherited Create(TRUE);

  List := TCombinationList.Create;
  Cancelled := FALSE;
  NotificationWindow := 0;
  NotificationMsg := 0;
end;

destructor TCalculationThread.Destroy;
begin
  List.Free;
  inherited;
end;

procedure TCalculationThread.Execute;
begin
  List.Clear;
  Cancelled := FALSE;

  CalculateCombinations(Num, Max, List);

  if not Cancelled and (NotificationWindow <> 0) then
    PostMessage(NotificationWindow, NotificationMsg, 0, 0);
end;

end.
