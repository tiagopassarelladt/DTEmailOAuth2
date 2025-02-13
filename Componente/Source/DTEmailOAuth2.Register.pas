unit DTEmailOAuth2.Register;

interface

uses
  System.Classes,
  DTEmailOAuth2.OAuth2;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('DT Inovacao', [TDTEmailOAuth2]);
end;

end.
