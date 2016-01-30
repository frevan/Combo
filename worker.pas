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

      constructor Create;
      destructor Destroy; override;
      procedure Execute; override;
    end;



implementation

{ TCalculationThread }

constructor TCalculationThread.Create;
begin
  List := TCombinationList.Create;
end;

destructor TCalculationThread.Destroy;
begin
  List.Free;
  inherited;
end;

procedure TCalculationThread.Execute;
begin
  List.Clear;

  if NotificationWindow <> 0 then
    PostMessage(NotificationWindow, NotificationMsg, 0, 0);
end;

end.
