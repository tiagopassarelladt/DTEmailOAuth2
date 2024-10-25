unit DTEmailOAuth2.ObserverEvents;

interface

uses
  System.Classes, System.Generics.Collections;

type
  // Definição da Interface Observer
  IObsEvents = interface
    ['{8C6E8A90-4B89-4A71-8A34-BE73F2A6D9A2}']
    procedure onEvent(Sender: TObject; Mensagem: string);
  end;

  // Definição da Classe Sujeito (Subject)
  TSubEvents = class
  private
    FObservers: TList<IObsEvents>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddObserver(const Observer: IObsEvents);
    procedure RemoveObserver(const Observer: IObsEvents);
    procedure NotifyObservers(Mensagem: string);
  end;

implementation

{ TSubject }

constructor TSubEvents.Create;
begin
  FObservers := TList<IObsEvents>.Create;
end;

destructor TSubEvents.Destroy;
begin
  FObservers.Free;
  inherited;
end;

procedure TSubEvents.AddObserver(const Observer: IObsEvents);
begin
  if FObservers.IndexOf(Observer) = -1 then
    FObservers.Add(Observer);
end;

procedure TSubEvents.RemoveObserver(const Observer: IObsEvents);
begin
  FObservers.Remove(Observer);
end;

procedure TSubEvents.NotifyObservers(Mensagem: string);
var
  Observer: IObsEvents;
begin
  for Observer in FObservers do
  begin
    Observer.onEvent(Self, Mensagem);
  end;
end;

end.

