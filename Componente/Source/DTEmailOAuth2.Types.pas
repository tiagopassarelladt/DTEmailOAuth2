unit DTEmailOAuth2.Types;

interface

uses
    DTEmailOAuth2.IdSASLOAuthBase
  , IdExplicitTLSClientServerBase
  ;

type

  TAuthType = class of TIdSASLOAuthBase;

  TOnLog = procedure(const log: string) of object;

  TMailProviderInfo = record
    AuthenticationType : TAuthType;
    AuthorizationEndpoint : string;
    AccessTokenEndpoint : string;
    LogoutEndpoint : string;
    ClientID : String;
    ClientSecret : string;
    ClientAccount : string;
    ClientName : string;
    RefreshToken : string;
    Scopes : string;
    SmtpHost : string;
    SmtpPort : Integer;
    PopHost : string;
    PopPort : Integer;
    ImapHost : string;
    ImapPort : Integer;
    AuthName : string;
    TLS : TIdUseTLS;
    TwoLinePOPFormat: Boolean;
    function TokenName: string;
  end;


implementation

{ TProviderInfo }

function TMailProviderInfo.TokenName: string;
begin
  Result := AuthName + 'Token';
end;

end.
