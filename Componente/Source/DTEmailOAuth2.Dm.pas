unit DTEmailOAuth2.Dm;

interface

uses
    System.SysUtils
  , System.Classes
  , Winapi.ShellAPI
  , IniFiles
  , DTEmailOAuth2.RESTAuthenticatorEnhancedOAuth
  , IdIntercept
  , IdGlobal
  , IdContext
  , IdCustomHTTPServer
  , IdCustomTCPServer
  , IdHTTPServer
  , IdSMTPBase
  , IdSMTP
  , IdTCPConnection
  , IdTCPClient
  , IdExplicitTLSClientServerBase
  , IdSASLCollection
  , IdMessage
  , IdMessageClient
  , IdPOP3
  , IdBaseComponent
  , IdComponent
  , IdIOHandler
  , IdIOHandlerSocket
  , IdIOHandlerStack
  , IdSSL
  , IdSSLOpenSSL
  , IdIMAP4
  , IdSASL
  , DTEmailOAuth2.IdSASLOAuthBase
  , DTEmailOAuth2.Types
  , windows
  , IdAttachmentFile
  , DTEmailOAuth2.ObserverEvents
  ;

  type
  TOnLog = procedure(const log: string) of object;

  TCustomEvent = procedure(Sender: TObject; const Authentication: string) of object;

type
  TEmailOAuthDataModule = class(TDataModule)
    IdSSLIOHandlerSocketPOP: TIdSSLIOHandlerSocketOpenSSL;
    IdPOP3: TIdPOP3;
    IdSMTP1: TIdSMTP;
    IdConnectionInterceptSMTP: TIdConnectionIntercept;
    IdSSLIOHandlerSocketSMTP: TIdSSLIOHandlerSocketOpenSSL;
    IdHTTPServer1: TIdHTTPServer;
    IdConnectionInterceptIMAP: TIdConnectionIntercept;
    IdConnectionPOP: TIdConnectionIntercept;
    IdIMAP: TIdIMAP4;
    IdSSLIOHandlerSocketIMAP: TIdSSLIOHandlerSocketOpenSSL;
    procedure DataModuleDestroy(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure IdConnectionReceive(ASender: TIdConnectionIntercept; var ABuffer: TIdBytes);
    procedure IdConnectionSend(ASender: TIdConnectionIntercept; var ABuffer: TIdBytes);
    procedure IdHTTPServer1CommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  private

    FOAuth2_Enhanced : TEnhancedOAuth2Authenticator;
    FIsAuthenticated : Boolean;

    procedure DoLog(const msg: String);
    procedure NotifyObserver(const Mensagem: string);

  public
    FSubEvents       : TSubEvents;
    FClientID        : String;
    FClientSecret    : String;
    FRefreshToken    : string;
    FOnLog           : TOnLog;
    SelectedProvider : Integer;
    Provider         : TMailProviderInfo;
    function IsAuthenticated: Boolean;
    function HasRefreshToken: Boolean;
    procedure Authenticate(clientID, ClientSecret : string);
    procedure ClearAuthentication;
    procedure SetupAuthenticator;
    procedure SendMessage(const destAddress: string; const Subject, Body: String; FileList : Tstringlist; ClientID, ClientSecret, RefreshToken, From, ClientName : String);
    procedure CheckIMAP;
    procedure CheckPOP;
    procedure SetSubEvents(ASubEvents: TSubEvents);

  end;

var
  EmailOAuthDataModule: TEmailOAuthDataModule;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

uses
  System.NetEncoding
  , System.Net.URLClient
  , System.DateUtils
  , Dialogs
  , REST.Client
  , REST.Consts
  , REST.Types
  ;


const
  clientredirect = 'http://localhost:2132';

procedure TEmailOAuthDataModule.DataModuleCreate(Sender: TObject);
begin
  FOAuth2_Enhanced          := TEnhancedOAuth2Authenticator.Create(nil);
  FSubEvents                := TSubEvents.Create;
end;


procedure TEmailOAuthDataModule.DataModuleDestroy(Sender: TObject);
begin
    try
    if IdHTTPServer1.Active then
    begin
      IdHTTPServer1.Active := False;
      IdHTTPServer1.Bindings.Clear;
    end;
  except
    on E: Exception do
      DoLog('Erro ao desativar o servidor HTTP: ' + E.Message);
  end;

  FreeAndNil(FOAuth2_Enhanced);

  FreeAndNil(FSubEvents);
end;

procedure TEmailOAuthDataModule.DoLog(const msg: String);
begin
  if Assigned(FOnLog) then
    FOnLog(msg);
end;

function TEmailOAuthDataModule.HasRefreshToken: Boolean;
begin
  Result := not FOAuth2_Enhanced.RefreshToken.IsEmpty;
end;

procedure TEmailOAuthDataModule.IdConnectionReceive(ASender: TIdConnectionIntercept; var ABuffer: TIdBytes);
begin
  DoLog('R:' + TEncoding.ASCII.GetString(ABuffer));
end;

procedure TEmailOAuthDataModule.IdConnectionSend(ASender: TIdConnectionIntercept; var ABuffer: TIdBytes);
begin
  DoLog('S:' + TEncoding.ASCII.GetString(ABuffer));
end;

procedure TEmailOAuthDataModule.IdHTTPServer1CommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  LCode: string;
  LURL : TURI;
  LTokenName : string;

begin
  if ARequestInfo.QueryParams = '' then
    Exit;
  LURL := TURI.Create('https://localhost/?' + ARequestInfo.QueryParams);
  try
    LCode := LURL.ParameterByName['code'];
  except
    Exit;
  end;
  FOAuth2_Enhanced.ClientID     := FClientID;
  FOAuth2_Enhanced.ClientSecret := FClientSecret;
  FOAuth2_Enhanced.RefreshToken := FRefreshToken;
  FOAuth2_Enhanced.AuthCode     := LCode;
  FOAuth2_Enhanced.ChangeAuthCodeToAccesToken;
  LTokenName := Provider.AuthName + 'Token';

  DoLog('Authenticated via OAUTH2');
  DoLog(FOAuth2_Enhanced.RefreshToken);

  NotifyObserver( FOAuth2_Enhanced.RefreshToken);

  SetupAuthenticator;
  AResponseInfo.ContentText := '<html><body>Successfully Authenticated. You can now close this tab/window.</body></html>';
  //ForceForegroundNoActivate(HWNDHandle);
end;

function TEmailOAuthDataModule.IsAuthenticated: boolean;
begin
  Result := FIsAuthenticated;
end;

procedure TEmailOAuthDataModule.NotifyObserver(const Mensagem: string);
begin
  if Assigned(FSubEvents) then
    FSubEvents.NotifyObservers(Mensagem)
end;

procedure TEmailOAuthDataModule.Authenticate(clientID, ClientSecret : string);
var
  uri : TURI;
begin
  SetupAuthenticator;

  if IdHTTPServer1.Active then
    IdHTTPServer1.Active := False; // Desativar primeiro se já estiver ativo

  IdHTTPServer1.DefaultPort := 2132;

  try
    IdHTTPServer1.Active := True;  // Ativar novamente
  except
    on E: Exception do
      DoLog('Erro ao iniciar o servidor HTTP: ' + E.Message);
  end;

  uri := TURI.Create(FOAuth2_Enhanced.AuthorizationRequestURI(clientID, ClientSecret));

  ShellExecute(0,
    'open',
    PChar(uri.ToString),
    nil,
    nil,
    0
  );
end;

procedure TEmailOAuthDataModule.ClearAuthentication;
begin
  //FIniSettings.DeleteKey('Authentication', Provider.TokenName);
  SetupAuthenticator;
end;

procedure TEmailOAuthDataModule.SendMessage(const destAddress: string; const Subject, Body: String; FileList : Tstringlist; ClientID, ClientSecret, RefreshToken, From, ClientName : String);
var
  IdMessage   : TIdMessage;
  xoauthSASL  : TIdSASLListEntry;
  I           : integer;
begin
  FOAuth2_Enhanced.ClientID     := ClientID;
  FOAuth2_Enhanced.ClientSecret := ClientSecret;
  FOAuth2_Enhanced.RefreshToken := RefreshToken;
  FOAuth2_Enhanced.RefreshAccessTokenIfRequired(clientID, ClientSecret);

  DoLog('refresh_token=' + FOAuth2_Enhanced.RefreshToken);
  DoLog('access_token='  + FOAuth2_Enhanced.AccessToken);

  if FOAuth2_Enhanced.AccessToken.Length = 0 then
  begin
    DoLog('Failed to authenticate properly');
    Exit;
  end;

  IdSMTP1.Host                                    := Provider.SmtpHost;
  IdSMTP1.UseTLS                                  := Provider.TLS;
  IdSMTP1.Port                                    := Provider.SmtpPort;

  xoauthSASL                                      := IdSMTP1.SASLMechanisms.Add;
  xoauthSASL.SASL                                 := Provider.AuthenticationType.Create(nil);

  TIdSASLOAuthBase(xoauthSASL.SASL).Token         := FOAuth2_Enhanced.AccessToken;
  TIdSASLOAuthBase(xoauthSASL.SASL).User          := from;

  IdSSLIOHandlerSocketSMTP.SSLOptions.SSLVersions := [ sslvTLSv1_2 ];

  IdSMTP1.Connect;

  IdSMTP1.AuthType                 := satSASL;
  IdSMTP1.Authenticate;

  IdMessage                        := TIdMessage.Create(self);
  IdMessage.From.Address           := from;
  IdMessage.From.Name              := clientname;
  IdMessage.ReplyTo.EMailAddresses := IdMessage.From.Address;
  IdMessage.Recipients.Add.Text    := destAddress;
  IdMessage.Subject                := Subject;
  IdMessage.Body.Text              := Body;

  for I := 0 to FileList.Count - 1 do
    TIdAttachmentFile.Create(IdMessage.MessageParts, FileList[I]);

  try
    IdSMTP1.Send(IdMessage);
  except
  on E: Exception do
    DoLog('Erro ao enviar email: ' + E.Message);
  end;
  IdSMTP1.Disconnect;
  IdMessage.free;
  DoLog('Email enviado');
end;

procedure TEmailOAuthDataModule.CheckIMAP;
var
  xoauthSASL : TIdSASLListEntry;
//  msgCount   : Integer;
  mailboxes  : TStringList;
begin
  DoLog('refresh_token=' + FOAuth2_Enhanced.RefreshToken);
  DoLog('access_token='  + FOAuth2_Enhanced.AccessToken);

  FOAuth2_Enhanced.ClientID     := FClientID;
  FOAuth2_Enhanced.ClientSecret := FClientSecret;
  FOAuth2_Enhanced.RefreshToken := FRefreshToken;

  FOAuth2_Enhanced.RefreshAccessTokenIfRequired(FclientID, FClientSecret);

  if FOAuth2_Enhanced.AccessToken.Length = 0 then
  begin
    DoLog('Failed to authenticate properly');
    Exit;
  end;

  IdIMAP.Host                             := Provider.ImapHost;
  IdIMAP.Port                             := Provider.ImapPort;
  IdIMAP.UseTLS                           := Provider.TLS;

  xoauthSASL                              := IdIMAP.SASLMechanisms.Add;
  xoauthSASL.SASL                         := Provider.AuthenticationType.Create(nil);

  TIdSASLOAuthBase(xoauthSASL.SASL).Token := FOAuth2_Enhanced.AccessToken;
  TIdSASLOAuthBase(xoauthSASL.SASL).User  := Provider.ClientAccount;

  IdIMAP.AuthType                         := iatSASL;
  IdIMAP.Connect;

  mailboxes                               := TStringList.Create;
  try
    IdImap.ListMailBoxes(mailboxes);
    DoLog(mailboxes.Text);
  finally
    FreeAndNil(mailboxes);
  end;

  IdIMAP.Disconnect;
end;

procedure TEmailOAuthDataModule.CheckPOP;
const
  ST_OK           = '+OK';
  ST_SASLCONTINUE = '+';  {Do not translate}
var
  xoauthSASL : TIdSASLListEntry;
  msgCount   : Integer;
begin
  IdPOP3.Disconnect;
  IdPOP3.AutoLogin := False;
  DoLog('refresh_token=' + FOAuth2_Enhanced.RefreshToken);
  DoLog('access_token='  + FOAuth2_Enhanced.AccessToken);

  FOAuth2_Enhanced.ClientID     := FClientID;
  FOAuth2_Enhanced.ClientSecret := FClientSecret;
  FOAuth2_Enhanced.RefreshToken := FRefreshToken;

  FOAuth2_Enhanced.RefreshAccessTokenIfRequired(FclientID, FClientSecret);

  if FOAuth2_Enhanced.AccessToken.Length = 0 then
  begin
    DoLog('Failed to authenticate properly');
    Exit;
  end;

  IdPOP3.Host                             := Provider.PopHost;
  IdPOP3.Port                             := Provider.PopPort;
  IdPOP3.UseTLS                           := utUseImplicitTLS;

  xoauthSASL                              := IdPOP3.SASLMechanisms.Add;
  xoauthSASL.SASL                         := Provider.AuthenticationType.Create(nil);

  TIdSASLOAuthBase(xoauthSASL.SASL).Token := FOAuth2_Enhanced.AccessToken;
  TIdSASLOAuthBase(xoauthSASL.SASL).User  := Provider.ClientAccount;

  IdPOP3.AuthType                         := patSASL;
  IdPOP3.UseTLS                           := utUseImplicitTLS;
  IdPOP3.Connect;
  IdPOP3.CAPA;
//  IdPOP3.Login;
  IdPOP3.SASLMechanisms.LoginSASL('AUTH', IdPOP3.Host, 'pop', [ST_OK], [ST_SASLCONTINUE], IdPOP3, IdPOP3.Capabilities, 'SASL', False); {do not localize}

  msgCount := IdPOP3.CheckMessages;

  ShowMessage(msgCount.ToString + ' Messages available for download');

  IdPOP3.Disconnect;
end;

procedure TEmailOAuthDataModule.SetSubEvents(ASubEvents: TSubEvents);
begin
  FSubEvents := ASubEvents;
end;

procedure TEmailOAuthDataModule.SetupAuthenticator;
begin
  FOAuth2_Enhanced.Scope                 := Provider.Scopes;
  FOAuth2_Enhanced.RedirectionEndpoint   := clientredirect;
  FOAuth2_Enhanced.AuthorizationEndpoint := Provider.AuthorizationEndpoint;
  FOAuth2_Enhanced.AccessTokenEndpoint   := Provider.AccessTokenEndpoint;
  FOAuth2_Enhanced.AccessToken           := '';
  FOAuth2_Enhanced.AccessTokenExpiry     := 0;
  IdSMTP1.Disconnect;
  IdPOP3.Disconnect;
  IdIMAP.Disconnect;
end;

end.
