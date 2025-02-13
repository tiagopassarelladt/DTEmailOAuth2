unit DTEmailOAuth2.OAuth2;

interface

uses
  System.SysUtils,
  System.Classes,
  mimemess,
  smtpsend,
  synachar,
  blcksock,
  system.StrUtils,
  synautil,
  ssl_openssl,
  ssl_openssl_lib,
  DTEmailOAuth2.Types,
  IdExplicitTLSClientServerBase,
  DTEmailOAuth2.IdSASLOAuth.OAuth2Bearer,
  mimepart,
  Contnrs,
  WinAPI.ShellAPI,
  DTEmailOAuth2.Dm,
  DTEmailOAuth2.IdSASLOAuth.XOAUTH2,
  Winapi.Windows,
  DTEmailOAuth2.ObserverEvents
  ;


type
 TMailCharset = TMimeChar;
 TMailAttachmentDisposition = (adAttachment, adInline);
 TProvedorEmail = (pvGmail, pvOutlook, pvMicrosoft, pvYahoo, pvOutros);
 TMailStatus = ( pmsStartProcess, pmsConfigHeaders, pmsAddingMimeParts,
                 pmsLoginSMTP, pmsStartSends, pmsSendTo, pmsSendCC, pmsSendBCC,
                 pmsSendReplyTo, pmsSendData, pmsLogoutSMTP, pmsDone, pmsError);

 TMailAttachment = class
  private
    FFileName: String;
    FDescription: String;
    FStream: TMemoryStream;
    FDisposition: TMailAttachmentDisposition;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Assign(Source: TMailAttachment);

    property FileName    : String                     read FFileName    write FFileName;
    property Stream      : TMemoryStream              read FStream;
    property Description : String                     read FDescription write FDescription;
    property Disposition : TMailAttachmentDisposition read FDisposition write FDisposition;
  end;

  { TMailAttachments }

  TMailAttachments = class(TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TMailAttachment);
    function GetObject (Index: Integer): TMailAttachment;
    procedure Insert (Index: Integer; Obj: TMailAttachment);
  public
    function New: TMailAttachment;
    function Add (Obj: TMailAttachment): Integer;
    property Objects [Index: Integer]: TMailAttachment read GetObject write SetObject; default;
  end;

 TOnLog = procedure(const log: string) of object;

type
  TOnAuthenticateEvent = procedure(Sender: TObject; const Token: string) of object;

type
  TDTEmailOAuth2 = class(TComponent, IObsEvents)
  private
    FSubEvents            : TSubEvents;
    FOnNotifyEvent        : TOnAuthenticateEvent;

    fSMTP                 : TSMTPSend;
    fMIMEMess             : TMimeMess;
    fArqMIMe              : TMemoryStream;

    fOnBeforeMailProcess  : TNotifyEvent;
    fIsHTML               : boolean;
    fAttempts             : Byte;
    fIDECharsetCode       : TMailCharset;

    fBody                 : TStringList;
    fAltBody              : TStringList;
    fAttachments          : TMailAttachments;
    fReplyTo              : TStringList;
    fBCC                  : TStringList;

    fFrom                 : string;
    fFromName             : string;
    fSubject              : string;
    fTimeOut              : Integer;
    fReadingConfirmation  : boolean;
    fDefaultCharsetCode   : TMailCharset;
    fDeliveryConfirmation : boolean;
    fUseThread            : boolean;
    fClientID             : string;
    fClientSecret         : string;
    fGetLastSmtpError: string;
    FProvedorEmail: TProvedorEmail;
    fOnAfterMailProcess: TNotifyEvent;
    fOnLog: TOnLog;
    FRefreshToken: string;

    procedure onEvent(Sender: TObject; Mensagem: string);

    function GetAutoTLS: boolean;
    function GetFullSSL: boolean;
    function GetHost: string;
    function GetPassword: string;
    function GetPort: string;
    function GetPriority: TMessPriority;
    function GetSSLType: TSSLType;
    function GetUsername: string;

    procedure SetAttempts(const Value: Byte);
    procedure SetAutoTLS(const Value: boolean);
    procedure SetFullSSL(const Value: boolean);
    procedure SetHost(const Value: string);
    procedure SetPassword(const Value: string);
    procedure SetPort(const Value: string);
    procedure SetPriority(const Value: TMessPriority);
    procedure SetSSLType(const Value: TSSLType);
    procedure SetUsername(const Value: string);

    procedure SmtpError(const pMsgError: string);
    procedure DoException(E: Exception);

    procedure AddEmailWithDelimitersToList( aEmail: String; aList: TStrings);
    function FindDelimiterInText( const AText: String; ADelimiters: String = ''): Char;
    function AddDelimitedTextToList( const AText: String; const ADelimiter: Char; AStringList: TStrings; const AQuoteChar: Char = '"'): Integer;

   procedure DoLog(const log: string);

  protected
    procedure SendMail;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure MailProcess(const aStatus: TMailStatus);
    procedure Send(UseThreadNow: Boolean); overload;
    procedure Send; overload;
    procedure BuildMimeMess;
    procedure Clear;
    procedure SaveToFile(const AFileName: String);
    function SaveToStream(AStream: TStream): Boolean;

    procedure AddAttachment(const aFileName: string; aDescription: string; const aDisposition: TMailAttachmentDisposition = adInline); overload;
    procedure AddAttachment(const aFileName: string); overload;
    procedure AddAttachment(aStream: TStream; aDescription: string; const aDisposition: TMailAttachmentDisposition = adInline); overload;
    procedure AddAttachment(aStream: TStream); overload;
    procedure ClearAttachments;

    procedure AddAddress(const aEmail: string; const aName: string = '');
    procedure AddReplyTo(const aEmail: string; const aName: string = '');
    procedure AddCC(const aEmail: string; const aName: string = '');
    procedure AddBCC(const aEmail: string);

    property SMTP: TSMTPSend read fSMTP;
    property MIMEMess: TMimeMess read fMIMEMess;
    property Attachments: TMailAttachments read fAttachments;
    property BCC: TStringList read fBCC;
    property ReplyTo: TStringList read fReplyTo;

    property AltBody: TStringList read fAltBody;
    property Body: TStringList read fBody;

    property GetLastSmtpError: string read fGetLastSmtpError;

    procedure Authenticate;
    procedure OpenGoogleConsole;
    procedure ClearAuthentication;
    procedure SendMessageAuth2;

    procedure InternalLogHandler(const log: string);

    procedure SetSubEvents(ASubEvents: TSubEvents);

  published
    property Host                 : string                read GetHost               write SetHost;
    property Port                 : string                read GetPort               write SetPort;
    property Username             : string                read GetUsername           write SetUsername;
    property Password             : string                read GetPassword           write SetPassword;
    property SetSSL               : boolean               read GetFullSSL            write SetFullSSL;
    property SSLType              : TSSLType              read GetSSLType            write SetSSLType default LT_all;
    property SetTLS               : boolean               read GetAutoTLS            write SetAutoTLS;
    property Priority             : TMessPriority         read GetPriority           write SetPriority default MP_normal;
    property ReadingConfirmation  : boolean               read fReadingConfirmation  write fReadingConfirmation default False;
    property DeliveryConfirmation : boolean               read fDeliveryConfirmation write fDeliveryConfirmation default False;
    property IsHTML               : boolean               read fIsHTML               write fIsHTML default False;
    property UseThread            : boolean               read fUseThread            write fUseThread default False;
    property TimeOut              : Integer               read fTimeOut              write fTimeOut default 0;
    property Attempts             : Byte                  read fAttempts             write SetAttempts;
    property From                 : string                read fFrom                 write fFrom;
    property FromName             : string                read fFromName             write fFromName;
    property Subject              : string                read fSubject              write fSubject;

    property ClientID             : string                read fClientID             write fClientID;
    property ClientSecret         : string                read fClientSecret         write fClientSecret;
    property ProvedorEmail        : TProvedorEmail        read FProvedorEmail       write FProvedorEmail;
    property RefreshToken         : string                read FRefreshToken        write FRefreshToken;

    property DefaultCharset       : TMailCharset          read fDefaultCharsetCode   write fDefaultCharsetCode;
    property IDECharset           : TMailCharset          read fIDECharsetCode       write fIDECharsetCode;

    property OnBeforeMailProcess  : TNotifyEvent          read fOnBeforeMailProcess  write fOnBeforeMailProcess;
    property OnAfterMailProcess   : TNotifyEvent          read fOnAfterMailProcess   write fOnAfterMailProcess;
    property OnLog                : TOnLog                read fOnLog                write fOnLog;
    property OnAuthenticate       : TOnAuthenticateEvent  read FOnNotifyEvent        write FOnNotifyEvent;

  end;



implementation

const
  Providers : array[0..3] of TMailProviderInfo =
  (
    (  AuthenticationType : TIdSASLXOAuth;
       AuthorizationEndpoint : 'https://accounts.google.com/o/oauth2/auth?access_type=offline';
       AccessTokenEndpoint : 'https://accounts.google.com/o/oauth2/token';
       LogoutEndpoint : 'https://www.google.com/accounts/Logout';
       ClientID : '';
       ClientSecret : '';
       ClientAccount : '';
       ClientName : '';
       RefreshToken : '';
       Scopes : 'https://mail.google.com/ openid';
       SmtpHost : 'smtp.gmail.com';
       SmtpPort : 465;
       PopHost : 'pop.gmail.com';
       PopPort : 995;
       ImapHost : 'imap.gmail.com';
       ImapPort : 143;
       AuthName : 'Google';
       TLS : utUseImplicitTLS;
       TwoLinePOPFormat: False
    ),
    (  AuthenticationType : TIdSASLXOAuth;
       AuthorizationEndpoint : 'https://login.microsoftonline.com/common/oauth2/v2.0/authorize';//'https://login.live.com/oauth20_authorize.srf';
       AccessTokenEndpoint : 'https://login.microsoftonline.com/common/oauth2/v2.0/token';//'https://login.live.com/oauth20_token.srf';
       LogoutEndpoint : 'https://login.microsoftonline.com/common/oauth2/v2.0/logout';
       ClientID : '';
       ClientSecret : '';
       ClientAccount : '';
       ClientName : '';
       RefreshToken : '';
       Scopes : 'https://outlook.office.com/IMAP.AccessAsUser.All https://outlook.office.com/POP.AccessAsUser.All https://outlook.office.com/SMTP.Send offline_access';
       //'wl.imap offline_access';
       SmtpHost : 'smtp-mail.outlook.com';
       SmtpPort : 587;
       PopHost : 'smtp-mail.outlook.com';
       PopPort : 995;
       ImapHost : 'outlook.office365.com';
       ImapPort : 993;
       AuthName : 'Microsoft';
       TLS : utUseExplicitTLS;
       TwoLinePOPFormat: True
    ),
    (  AuthenticationType : TIdSASLXOAuth;
       AuthorizationEndpoint : 'https://login.live.com/oauth20_authorize.srf';
       AccessTokenEndpoint : 'https://login.live.com/oauth20_token.srf';
       LogoutEndpoint : 'https://login.live.com/logout.srf';
       ClientID : '';
       ClientSecret : '';
       ClientAccount : '';
      // Scopes : 'https://outlook.office.com/IMAP.AccessAsUser.All https://outlook.office.com/POP.AccessAsUser.All https://outlook.office.com/SMTP.Send offline_access';
       ClientName : '';
       RefreshToken : '';
       Scopes : 'wl.imap wl.emails wl.offline_access';
       SmtpHost : 'smtp.office365.com';
       SmtpPort : 587;
       PopHost : 'outlook.office365.com';
       PopPort : 995;
       ImapHost : 'outlook.office365.com';
       ImapPort : 993;
       AuthName : 'Hotmail';
       TLS : utUseExplicitTLS;
       TwoLinePOPFormat: false
    ),
    (  AuthenticationType : TIdOAuth2Bearer;
       AuthorizationEndpoint : 'https://api.login.yahoo.com/oauth2/request_auth';
       AccessTokenEndpoint : 'https://api.login.yahoo.com/oauth2/get_token';
       LogoutEndpoint : '';
       ClientID : '';
       ClientSecret : '';
       ClientAccount : '';
      // Scopes : 'https://outlook.office.com/IMAP.AccessAsUser.All https://outlook.office.com/POP.AccessAsUser.All https://outlook.office.com/SMTP.Send offline_access';
       ClientName : '';
       RefreshToken : '';
       Scopes : 'email mail-r mail-w';
       SmtpHost : 'smtp.mail.yahoo.com';
       SmtpPort : 465;
       PopHost : '';
       PopPort : 995;
       ImapHost : 'imap.mail.yahoo.com';
       ImapPort : 993;
       AuthName : 'Yahoo';
       TLS : utUseExplicitTLS;
       TwoLinePOPFormat: false
    )
  );


{ TMailAttachments }

function TMailAttachments.Add(Obj: TMailAttachment): Integer;
begin
 Result := inherited Add(Obj) ;
end;

function TMailAttachments.GetObject(Index: Integer): TMailAttachment;
begin
 Result := TMailAttachment(inherited Items[Index]);
end;

procedure TMailAttachments.Insert(Index: Integer; Obj: TMailAttachment);
begin
 inherited Insert(Index, Obj);
end;

function TMailAttachments.New: TMailAttachment;
begin
  Result := TMailAttachment.Create;
  Add(Result);
end;

procedure TMailAttachments.SetObject(Index: Integer; Item: TMailAttachment);
begin
  inherited Items[Index] := Item;
end;

{ TMailAttachment }

procedure TMailAttachment.Assign(Source: TMailAttachment);
begin
  Clear;
  FFileName     := Source.FileName;
  FDescription  := Source.Description;
  Source.Stream.Position := 0;
  FStream.CopyFrom(Source.Stream, Stream.Size);
  FDisposition  := Source.Disposition;
end;

procedure TMailAttachment.Clear;
begin
  FFileName    := '';
  FDescription := '';
  FStream.Clear;
end;

constructor TMailAttachment.Create;
begin
  inherited Create;
  FStream      := TMemoryStream.Create;
  FDisposition := adInline;
  Clear;
end;

destructor TMailAttachment.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

{ TDTEmailOAuth2 }

procedure TDTEmailOAuth2.AddAttachment(const aFileName: string;
  aDescription: string; const aDisposition: TMailAttachmentDisposition);
var
  AAttachment: TMailAttachment;
begin
  if not FileExists(aFileName) then
    DoException( Exception.Create('Add Attachment: File not Exists.') );

  AAttachment              := fAttachments.New;
  AAttachment.FileName     := aFileName;

  if (aDescription = '') then
    AAttachment.Description := ExtractFileName(AAttachment.FileName)
  else
    AAttachment.Description := aDescription;

  AAttachment.Disposition   := aDisposition;

  AAttachment.Stream.LoadFromFile(aFileName)

end;

procedure TDTEmailOAuth2.AddAttachment(const aFileName: string);
begin
  AddAttachment(aFileName, '');
end;

procedure TDTEmailOAuth2.AddAttachment(aStream: TStream; aDescription: string;
  const aDisposition: TMailAttachmentDisposition);
var
  AAttachment: TMailAttachment;
begin
  if not Assigned(aStream) then
    DoException( Exception.Create('Add Attachment: Access Violation.') );

  if (Trim(aDescription) = '') then
    aDescription := 'file_' + FormatDateTime('hhnnsszzz',Now);

  aStream.Position        := 0;
  AAttachment             := fAttachments.New;
  AAttachment.FileName    := aDescription;
  AAttachment.Description := aDescription;
  AAttachment.Disposition := aDisposition;
  AAttachment.Stream.CopyFrom(aStream, aStream.Size);
end;

procedure TDTEmailOAuth2.AddAddress(const aEmail, aName: string);
begin
   if Trim(aName) <> '' then
    fMIMEMess.Header.ToList.Add('"' + aName + '" <' + aEmail + '>')
  else
    AddEmailWithDelimitersToList(aEmail, fMIMEMess.Header.ToList);
end;

procedure TDTEmailOAuth2.AddAttachment(aStream: TStream);
begin
  AddAttachment(aStream, '');
end;

procedure TDTEmailOAuth2.AddBCC(const aEmail: string);
begin
  AddEmailWithDelimitersToList(aEmail, fBCC);
end;

procedure TDTEmailOAuth2.AddCC(const aEmail, aName: string);
begin
 if Trim(aName) <> '' then
    fMIMEMess.Header.CCList.Add('"' + aName + '" <' + aEmail + '>')
  else
    AddEmailWithDelimitersToList(aEmail, fMIMEMess.Header.CCList);
end;

procedure TDTEmailOAuth2.AddEmailWithDelimitersToList(aEmail: String;
  aList: TStrings);
var
  sDelimiter: Char;
begin
  aEmail := Trim(aEmail);
  sDelimiter := FindDelimiterInText(aEmail);

  if (sDelimiter = ' ') then
    aList.Add(aEmail)
  else
    AddDelimitedTextToList(aEmail, sDelimiter, aList);

end;

function TDTEmailOAuth2.AddDelimitedTextToList(const AText: String; const ADelimiter: Char;
  AStringList: TStrings; const AQuoteChar: Char): Integer;
var
  SL: TStringList;
  {$IfNDef HAS_STRICTDELIMITER}
   L, Pi, Pf, Pq: Integer;
  {$EndIf}
begin
  Result := 0;
  if (AText = '') then
    Exit;

  SL := TStringList.Create;
  try
    {$IfDef HAS_STRICTDELIMITER}
     SL.Delimiter := ADelimiter;
     SL.QuoteChar := AQuoteChar;
     SL.StrictDelimiter := True;
     SL.DelimitedText := AText;
    {$Else}
     L  := Length(AText);
     Pi := 1;
     if (ADelimiter = AQuoteChar) then
       Pq := L+1
     else
     begin
       Pq := Pos(AQuoteChar, AText);
       if Pq = 0 then
         Pq := L+1;
     end;

     while Pi <= L do
     begin
       if (Pq = Pi) then
       begin
         Inc(Pi);  // Pula o Quote
         Pf := PosEx(AQuoteChar, AText, Pi);
         Pq := Pf;
       end
       else
         Pf := PosEx(ADelimiter, AText, Pi);

       if Pf = 0 then
         Pf := L+1;

       SL.Add(Copy(AText, Pi, Pf-Pi));

       if (Pq = Pf) then
       begin
         Pq := PosEx(AQuoteChar, AText, Pq+1);
         Inc(Pf);
       end;

       Pi := Pf + 1;
     end;
    {$EndIf}
    Result := SL.Count;

    AStringList.AddStrings(SL);
  finally
    SL.Free;
  end;
end;

function TDTEmailOAuth2.FindDelimiterInText(const AText: String; ADelimiters: String): Char;
var
  I: Integer;
begin
  if (ADelimiters = '') then
    ADelimiters := ';,|';

  Result := ' ';
  I := 1;
  while (Result = ' ') and (I <= Length(ADelimiters)) do
  begin
    if (pos( ADelimiters[I], AText) > 0) then
      Result := ADelimiters[I];

    Inc(I);
  end;
end;

procedure TDTEmailOAuth2.AddReplyTo(const aEmail, aName: string);
begin
   if Trim(aName) <> '' then
    fReplyTo.Add('"' + aName + '" <' + aEmail + '>')
  else
    AddEmailWithDelimitersToList(aEmail, fReplyTo);
end;

procedure TDTEmailOAuth2.Assign(Source: TPersistent);
var
  i: Integer;
  AAttachment: TMailAttachment;
begin
  if not (Source is TDTEmailOAuth2) then
    raise Exception.Create('Source must be TDTEmailOAuth2');

  with TDTEmailOAuth2(Source) do
  begin
    Self.Host := Host;
    Self.Port := Port;
    Self.Username := Username;
    Self.Password := Password;
    Self.SetSSL := SetSSL;
    Self.SetTLS := SetTLS;
    Self.Priority := Priority;
    Self.ReadingConfirmation := ReadingConfirmation;
    Self.IsHTML := IsHTML;
    Self.UseThread := UseThread;
    Self.Attempts := Attempts;
    Self.From := From;
    Self.FromName := FromName;
    Self.Subject := Subject;
    Self.DefaultCharset := DefaultCharset;
    Self.IDECharset := IDECharset;
    Self.OnBeforeMailProcess := OnBeforeMailProcess;
//    Self.OnMailProcess := OnMailProcess;
//    Self.OnAfterMailProcess := OnAfterMailProcess;
//    Self.OnMailException := OnMailException;
    Self.Tag := Tag;

    for i := 0 to Attachments.Count-1 do
    begin
      AAttachment := Self.Attachments.New;
      AAttachment.Assign(Attachments[I]);
    end;

    Self.AltBody.Assign(AltBody);
    Self.Body.Assign(Body);
    Self.ReplyTo.Assign(ReplyTo);
    Self.BCC.Assign(BCC);

    Self.MIMEMess.Header.ToList.Assign( MIMEMess.Header.ToList );
    Self.MIMEMess.Header.CCList.Assign( MIMEMess.Header.CCList );
    Self.MIMEMess.Header.Organization := MIMEMess.Header.Organization;
    Self.MIMEMess.Header.CustomHeaders.Assign( MIMEMess.Header.CustomHeaders );
    Self.MIMEMess.Header.Date    := MIMEMess.Header.Date;
    Self.MIMEMess.Header.XMailer := MIMEMess.Header.XMailer;
  end;

end;

procedure TDTEmailOAuth2.Authenticate;
begin
   EmailOAuthDataModule.FCLIENTID     := FclientID;
   EmailOAuthDataModule.FClientSecret := FClientSecret;
   EmailOAuthDataModule.FRefreshToken := FRefreshToken;
   EmailOAuthDataModule.Authenticate(FclientID, FClientSecret);
end;

function EncodeQuotedPrintable(const S: string; Charset: string = 'UTF-8'): string;
var
  I: Integer;
  C: Char;
begin
  Result := '=?' + Charset + '?Q?';
  for I := 1 to Length(S) do
  begin
    C := S[I];
    if (C in ['A'..'Z', 'a'..'z', '0'..'9', ' ', '!', '*', '+', '-', '/', '=']) then
      Result := Result + C
    else
      Result := Result + '=' + IntToHex(Ord(C), 2);
  end;
  Result := Result + '?=';
end;


procedure TDTEmailOAuth2.BuildMimeMess;
var
  i                                  : Integer;
  MultiPartParent, MimePartAttach    : TMimePart;
  NeedMultiPartRelated, BodyHasImage : Boolean;
  AAttachment                        : TMailAttachment;
begin
  if Assigned(OnBeforeMailProcess) then
    OnBeforeMailProcess( self );

  MailProcess(pmsStartProcess);

  // Configuring the Headers //
  MailProcess(pmsConfigHeaders);

  fMIMEMess.Header.CharsetCode   := fDefaultCharsetCode;
  fMIMEMess.Header.TargetCharset := fIDECharsetCode;
  fMIMEMess.Header.Subject       := fSubject;

  if Trim(fFromName) <> '' then
    fMIMEMess.Header.From := '"' + fFromName + '" <' + From + '>'
  else
    fMIMEMess.Header.From := fFrom;

  if fReplyTo.Count > 0 then
    fMIMEMess.Header.ReplyTo := fReplyTo.DelimitedText;

  if fReadingConfirmation then
    fMIMEMess.Header.CustomHeaders.Insert(0, 'Disposition-Notification-To: ' + fMIMEMess.Header.From);

  if fDeliveryConfirmation then
    fMIMEMess.Header.CustomHeaders.Insert(0, 'Return-Receipt-To: ' + fMIMEMess.Header.From);

  fMIMEMess.Header.XMailer := 'Synapse - DTEmailOAuth2';


  // Adding MimeParts //
  // Inspiration: http://www.ararat.cz/synapse/doku.php/public:howto:mimeparts
  MailProcess(pmsAddingMimeParts);

  // Encoding according to IDE and Mail Charset //
  NeedMultiPartRelated := fIsHTML and (fBody.Count > 0) and (fAltBody.Count > 0);

  // The Root //
  MultiPartParent                := fMIMEMess.AddPartMultipart( IfThen(NeedMultiPartRelated, 'alternative', 'mixed'), nil );
  MultiPartParent.CharsetCode    := fDefaultCharsetCode;
  MultiPartParent.TargetCharset  := fIDECharsetCode;
  MultiPartParent.ConvertCharset := (fDefaultCharsetCode <> fIDECharsetCode);

  // Text part //
  if (fAltBody.Count > 0) then
  begin
    with fMIMEMess.AddPart( MultiPartParent ) do
    begin
      fAltBody.SaveToStream(DecodedLines);
      Primary        := 'text';
      Secondary      := 'plain';
      Description    := 'Message text';
      Disposition    := 'inline';
      CharsetCode    := fDefaultCharsetCode;
      TargetCharset  := fIDECharsetCode;
      ConvertCharset := (fDefaultCharsetCode <> fIDECharsetCode);
      EncodingCode   := ME_QUOTED_PRINTABLE;// ME_BASE64;
      EncodePart;
      EncodePartHeader;
    end;
  end;

  // Need New branch ? //
  if NeedMultiPartRelated then
  begin
    MultiPartParent                := fMIMEMess.AddPartMultipart( 'related', MultiPartParent );
    MultiPartParent.CharsetCode    := fDefaultCharsetCode;
    MultiPartParent.TargetCharset  := fIDECharsetCode;
    MultiPartParent.ConvertCharset := (fDefaultCharsetCode <> fIDECharsetCode);
  end;

  if fIsHTML and (fBody.Count > 0) then
  begin
    // Adding HTML Part //
    with fMIMEMess.AddPart( MultiPartParent ) do
    begin
      fBody.SaveToStream(DecodedLines);
      Primary        := 'text';
      Secondary      := 'html';
      Description    := 'HTML text';
      Disposition    := 'inline';
      CharsetCode    := fDefaultCharsetCode;
      TargetCharset  := fIDECharsetCode;
      ConvertCharset := (fDefaultCharsetCode <> fIDECharsetCode);
      EncodingCode   := ME_QUOTED_PRINTABLE; // ME_BASE64;
      EncodePart;
      EncodePartHeader;
    end;
  end;

  // Adding the Attachments //
  for i := 0 to fAttachments.Count-1 do
  begin
    AAttachment  := fAttachments[i];
    BodyHasImage := pos(':'+LowerCase(AAttachment.Description), LowerCase(fBody.Text)) > 0;

    AAttachment.Stream.Position := 0;
    MimePartAttach              := fMIMEMess.AddPart(MultiPartParent);
    MimePartAttach.DecodedLines.LoadFromStream(AAttachment.Stream);
    MimePartAttach.Description  := AAttachment.Description;
    case AAttachment.Disposition of
      adInline: MimePartAttach.Disposition := 'inline';
    else
      MimePartAttach.Disposition := 'attachment';
    end;
    if fIsHTML and BodyHasImage then
      MimePartAttach.ContentID := AAttachment.Description;

    MimePartAttach.FileName       := ExtractFileName(AAttachment.FileName);
    MimePartAttach.EncodingCode   := ME_BASE64;
    MimePartAttach.PrimaryCode    := MP_BINARY;  // To avoid MP_TEXT internal conversion ;
    MimePartAttach.CharsetCode    := fIDECharsetCode;
    MimePartAttach.TargetCharset  := fIDECharsetCode;
    MimePartAttach.ConvertCharset := False;
    MimePartAttach.MimeTypeFromExt(AAttachment.FileName);

    MimePartAttach.EncodePart;
    MimePartAttach.EncodePartHeader;
  end;

  fMIMEMess.EncodeMessage;

  fArqMIMe.Clear;
  fMIMEMess.Lines.SaveToStream(fArqMIMe);

end;

procedure TDTEmailOAuth2.Clear;
begin
  ClearAttachments;
  fMIMEMess.Header.Clear;
  fMIMEMess.Clear;
  fReplyTo.Clear;
  fBCC.Clear;
  fSubject := '';
  fBody.Clear;
  fAltBody.Clear;
end;

procedure TDTEmailOAuth2.ClearAttachments;
begin
    fAttachments.Clear;
end;

procedure TDTEmailOAuth2.ClearAuthentication;
begin
  EmailOAuthDataModule.ClearAuthentication;
end;

constructor TDTEmailOAuth2.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FSubEvents   := nil;

  fSMTP        := TSMTPSend.Create;
  fMIMEMess    := TMimeMess.Create;
  fAltBody     := CreateStringList;
  fBody        := CreateStringList;
  fArqMIMe     := TMemoryStream.Create;
  fAttachments := TMailAttachments.Create(True); // FreeObjects
  fTimeOut     := 0;

  fOnBeforeMailProcess := nil;

  fAttachments.Clear;
  SetPriority(MP_normal);
  fDefaultCharsetCode   := UTF_8;
  fIDECharsetCode       := {$IfDef USE_UTF8}UTF_8{$Else}{$IfDef MSWINDOWS}CP1252{$Else}UTF_8{$EndIf}{$EndIf};
  fReadingConfirmation  := False;
  fDeliveryConfirmation := False;
  fIsHTML               := False;
  fUseThread            := False;
  fAttempts             := 3;
  fFrom                 := '';
  fFromName             := '';
  fSubject              := '';

  fReplyTo := CreateStringList;
  {$IfDef HAS_STRICTDELIMITER}
  fReplyTo.StrictDelimiter := True;
  {$EndIf}
  fReplyTo.Delimiter := ';';

  fBCC := CreateStringList;
  {$IfDef HAS_STRICTDELIMITER}
  fBCC.StrictDelimiter := True;
  {$EndIf}
  fBCC.Delimiter := ';';

  //definir como conexão padrão TLS1.2;
  //mantido LT_ALL por compatibilidade com os diversos provedores que por hora não suportam
  fSMTP.Sock.SSL.SSLType            := LT_all;

  EmailOAuthDataModule              := TEmailOAuthDataModule.Create(nil);

  EmailOAuthDataModule.FCLIENTID     := FclientID;
  EmailOAuthDataModule.FClientSecret := FClientSecret;
  EmailOAuthDataModule.FRefreshToken := FRefreshToken;
  EmailOAuthDataModule.Provider      := Providers[ ord(FProvedorEmail) ];
  EmailOAuthDataModule.SetupAuthenticator;

  EmailOAuthDataModule.SetSubEvents(EmailOAuthDataModule.FSubEvents);
  self.SetSubEvents(EmailOAuthDataModule.FSubEvents);

end;

destructor TDTEmailOAuth2.Destroy;
begin
  if Assigned(FSubEvents) then
     FSubEvents.RemoveObserver(Self);

  ClearAttachments;
  fAltBody.Free;
  fBody.Free;
  fBCC.Free;
  fReplyTo.Free;
  fMIMEMess.Free;
  fSMTP.Free;
  fArqMIMe.Free;
  fAttachments.Free;
  EmailOAuthDataModule.free;

  inherited Destroy;
end;

procedure TDTEmailOAuth2.DoException(E: Exception);
//Var
//  ThrowIt: Boolean;
begin
//  if Assigned(fOnMailException) then
//  begin
//    ThrowIt := True;
//    fOnMailException( Self, E, ThrowIt );
//
//    if ThrowIt then
//      raise E
//    else
//    begin
//      E.Free;
//      Abort;
//    end;
//  end
//  else
    raise E;

end;

procedure TDTEmailOAuth2.DoLog(const log: string);
begin
    if Assigned(fOnLog) then
    fOnLog(log); // Dispara
end;

function TDTEmailOAuth2.GetAutoTLS: boolean;
begin
  Result := fSMTP.AutoTLS;
end;

function TDTEmailOAuth2.GetFullSSL: boolean;
begin
  Result := fSMTP.FullSSL;
end;

function TDTEmailOAuth2.GetHost: string;
begin
  Result := fSMTP.TargetHost;
end;

function TDTEmailOAuth2.GetPassword: string;
begin
  Result := fSMTP.Password;
end;

function TDTEmailOAuth2.GetPort: string;
begin
   Result := fSMTP.TargetPort;
end;

function TDTEmailOAuth2.GetPriority: TMessPriority;
begin
   Result := fMIMEMess.Header.Priority;
end;

function TDTEmailOAuth2.GetSSLType: TSSLType;
begin
  Result := fSMTP.Sock.SSL.SSLType;
end;

function TDTEmailOAuth2.GetUsername: string;
begin
  Result := fSMTP.UserName;
end;

procedure TDTEmailOAuth2.InternalLogHandler(const log: string);
begin
    Dolog(log);
end;

procedure TDTEmailOAuth2.MailProcess(const aStatus: TMailStatus);
begin
//    if Assigned(fOnMailProcess) then
//    fOnMailProcess(Self, aStatus);
end;

procedure TDTEmailOAuth2.onEvent(Sender: TObject; Mensagem: string);
begin
   // Disparar o evento quando ocorrer a notificação
  if Assigned(FOnNotifyEvent) then
    FOnNotifyEvent(Self, Mensagem);

  // Aqui você pode processar a mensagem da notificação
  DoLog('Recebeu notificação: ' + Mensagem);
end;

procedure TDTEmailOAuth2.OpenGoogleConsole;
begin
    ShellExecute(HInstance, 'open',pchar('https://console.cloud.google.com/apis/dashboard'), nil, nil, SW_NORMAL);
end;

procedure TDTEmailOAuth2.SaveToFile(const AFileName: String);
begin
   BuildMimeMess;

  if AFileName <> '' then
    fArqMIMe.SaveToFile(AFileName);
end;

function TDTEmailOAuth2.SaveToStream(AStream: TStream): Boolean;
begin
   Result := True;
  try
    fArqMIMe.SaveToStream(AStream);
  except
    Result := False;
  end;
end;

procedure TDTEmailOAuth2.Send(UseThreadNow: Boolean);
begin
//    if UseThreadNow then
//    //SendEmailByThread(Self)
//  else
    SendMail;
end;

procedure TDTEmailOAuth2.Send;
begin
    IF FProvedorEmail = pvOutros then
     Send( UseThread )
    else
     SendMessageAuth2;
end;

procedure TDTEmailOAuth2.SendMail;
var
  vAttempts: Byte;
  c, i: Integer;
  ErrorMsgs: String;

  procedure AddErrorMsg(const AError: String);
  begin
    if Trim(AError) = '' then
      Exit;

    if (pos(AError, ErrorMsgs) = 0) then
    begin
      if (ErrorMsgs <> '') then
        ErrorMsgs := ErrorMsgs + sLineBreak;

      ErrorMsgs := ErrorMsgs + AError;
    end;
  end;

  procedure LogSMTPResponse;
  begin
    DoLog('Resposta completa do servidor SMTP: ' + fSMTP.FullResult.Text);
    DoLog('Última mensagem do servidor SMTP: ' + fSMTP.ResultString);
    DoLog('Último código de status do servidor SMTP: ' + IntToStr(fSMTP.Sock.LastError) + ' - ' + fSMTP.Sock.LastErrorDesc);
  end;

begin
  ErrorMsgs := '';
  BuildMimeMess;
  DoLog('Iniciando o processo de envio de email.');

  if fTimeOut > 0 then
  begin
    fSMTP.Timeout := fTimeOut;
    fSMTP.Sock.ConnectionTimeout := fTimeOut;
    DoLog('Timeout configurado: ' + IntToStr(fTimeOut));
  end;

  // DEBUG //
  // SaveToFile('c:\app\Mail.eml');

  DoLog('Tentando conectar ao servidor SMTP: ' + fSMTP.TargetHost);

  // Login in SMTP //
  MailProcess(pmsLoginSMTP);
  if (fSMTP.TargetHost = '') then
    SmtpError('SMTP Error: Servidor não informado');

  for vAttempts := 1 to fAttempts do
  begin
    DoLog('Tentativa de login no SMTP #' + IntToStr(vAttempts));
    if fSMTP.Login then
    begin
      DoLog('Login no SMTP bem-sucedido.');
      LogSMTPResponse;
      Break;
    end;

    AddErrorMsg(fSMTP.ResultString);
    LogSMTPResponse;
    AddErrorMsg(IntToStr(fSMTP.Sock.LastError) + ' - ' + fSMTP.Sock.LastErrorDesc);

    DoLog('Erro no login SMTP: ' + fSMTP.ResultString);

    if vAttempts >= fAttempts then
      SmtpError('SMTP Error: Não foi possível realizar login.' + sLineBreak + ErrorMsgs);
  end;

  if fDeliveryConfirmation then
  begin
    if (fSMTP.FindCap('DSN') = '') then
    begin
      DoLog('O servidor SMTP não suporta Delivery Status Notification (DSN).');
      SmtpError('SMTP Error: O servidor SMTP não suporta Delivery Status Notification');
    end;

    fSMTP.DeliveryStatusNotification := [dsnSucecess, dsnFailure];
    DoLog('Delivery Status Notification ativado.');
  end;

  // Enviando a mensagem //
  MailProcess(pmsStartSends);
  DoLog('Enviando e-mail de: ' + fFrom);

  for vAttempts := 1 to fAttempts do
  begin
    if fSMTP.MailFrom(fFrom, Length(fFrom)) then
    begin
      DoLog('Comando MAIL FROM aceito pelo servidor SMTP.');
      LogSMTPResponse;
      Break;
    end;

    AddErrorMsg(fSMTP.ResultString);
    LogSMTPResponse;
    DoLog('Erro no comando MAIL FROM: ' + fSMTP.ResultString);

    if vAttempts >= fAttempts then
      SmtpError('SMTP Error: Não foi possível enviar comando MailFrom.' + sLineBreak + ErrorMsgs);
  end;

  // Enviando para os destinatários //
  MailProcess(pmsSendTo);
  DoLog('Enviando para destinatários principais.');

  for i := 0 to fMIMEMess.Header.ToList.Count - 1 do
  begin
    DoLog('Enviando para: ' + fMIMEMess.Header.ToList[i]);
    for vAttempts := 1 to fAttempts do
    begin
      if fSMTP.MailTo(GetEmailAddr(fMIMEMess.Header.ToList[i])) then
      begin
        DoLog('Destinatário aceito: ' + fMIMEMess.Header.ToList[i]);
        LogSMTPResponse;
        Break;
      end;

      AddErrorMsg(fSMTP.ResultString);
      LogSMTPResponse;
      DoLog('Erro ao enviar para ' + fMIMEMess.Header.ToList[i] + ': ' + fSMTP.ResultString);

      if vAttempts >= fAttempts then
        SmtpError('SMTP Error: Não foi possível enviar para ' + fMIMEMess.Header.ToList[i] + '.' + sLineBreak + ErrorMsgs);
    end;
  end;

  // Enviando cópias (CC) //
  c := fMIMEMess.Header.CCList.Count;
  if c > 0 then
  begin
    MailProcess(pmsSendCC);
    DoLog('Enviando cópias (CC).');

    for i := 0 to c - 1 do
    begin
      DoLog('Enviando cópia para: ' + fMIMEMess.Header.CCList[i]);
      for vAttempts := 1 to fAttempts do
      begin
        if fSMTP.MailTo(GetEmailAddr(fMIMEMess.Header.CCList[i])) then
        begin
          DoLog('Cópia aceita: ' + fMIMEMess.Header.CCList[i]);
          LogSMTPResponse;
          Break;
        end;

        AddErrorMsg(fSMTP.ResultString);
        LogSMTPResponse;
        DoLog('Erro ao enviar cópia para ' + fMIMEMess.Header.CCList[i] + ': ' + fSMTP.ResultString);

        if vAttempts >= fAttempts then
          SmtpError('SMTP Error: Não foi possível enviar cópia para ' + fMIMEMess.Header.CCList[i] + '.' + sLineBreak + ErrorMsgs);
      end;
    end;
  end;

  // Enviando cópias ocultas (BCC) //
  c := fBCC.Count;
  if c > 0 then
  begin
    MailProcess(pmsSendBCC);
    DoLog('Enviando cópias ocultas (BCC).');

    for i := 0 to c - 1 do
    begin
      DoLog('Enviando cópia oculta para: ' + fBCC[i]);
      for vAttempts := 1 to fAttempts do
      begin
        if fSMTP.MailTo(GetEmailAddr(fBCC[I])) then
        begin
          DoLog('Cópia oculta aceita: ' + fBCC[I]);
          LogSMTPResponse;
          Break;
        end;

        AddErrorMsg(fSMTP.ResultString);
        LogSMTPResponse;
        DoLog('Erro ao enviar cópia oculta para ' + fBCC[I] + ': ' + fSMTP.ResultString);

        if vAttempts >= fAttempts then
          SmtpError('SMTP Error: Não foi possível enviar cópia oculta para ' + fBCC[I] + '.' + sLineBreak + ErrorMsgs);
      end;
    end;
  end;

  // Enviando o conteúdo do email (DATA) //
  MailProcess(pmsSendData);
  DoLog('Enviando o conteúdo do e-mail.');

  for vAttempts := 1 to fAttempts do
  begin
    if fSMTP.MailData(fMIMEMess.Lines) then
    begin
      DoLog('Conteúdo do e-mail enviado com sucesso.');
      LogSMTPResponse;
      Break;
    end;

    AddErrorMsg(fSMTP.ResultString);
    LogSMTPResponse;
    DoLog('Erro ao enviar o conteúdo do e-mail: ' + fSMTP.ResultString);

    if vAttempts >= fAttempts then
      SmtpError('SMTP Error: Não foi possível enviar os dados do e-mail.' + sLineBreak + ErrorMsgs);
  end;

  // Finalizando e desconectando //
  MailProcess(pmsLogoutSMTP);
  DoLog('Finalizando conexão SMTP.');

  for vAttempts := 1 to fAttempts do
  begin
    if fSMTP.Logout then
    begin
      DoLog('Logout do servidor SMTP bem-sucedido.');
      LogSMTPResponse;
      Break;
    end;

    AddErrorMsg(fSMTP.ResultString);
    LogSMTPResponse;
    DoLog('Erro ao tentar logout no SMTP: ' + fSMTP.ResultString);

    if vAttempts >= fAttempts then
      SmtpError('SMTP Error: Não foi possível fazer logout.' + sLineBreak + ErrorMsgs);
  end;

  // Finalizado //
  try
    MailProcess(pmsDone);
    DoLog('Processo de envio de e-mail finalizado.');

    if Assigned(OnAfterMailProcess) then
      OnAfterMailProcess(self);
  finally
    Clear;
    DoLog('Recursos limpos e processo concluído.');
  end;
end;

procedure TDTEmailOAuth2.SendMessageAuth2;
var
  FileList: TStringList;
  I, x: Integer;
//  vAttempts: Byte;
//  ErrorMsgs: String;
  vBody : string;
begin
  EmailOAuthDataModule.FCLIENTID     := FclientID;
  EmailOAuthDataModule.FClientSecret := FClientSecret;
  EmailOAuthDataModule.FRefreshToken := FRefreshToken;

  FileList := TStringList.Create;
  try
    for I := 0 to fAttachments.Count - 1 do
    begin
      if FileExists(fAttachments[I].FileName) then
        FileList.Add(fAttachments[I].FileName);
    end;

    if length(fBody.Text) > 0 then
      vbody := fBody.Text;

    if length(faltBody.Text) > 0 then
      vbody := faltBody.Text;

    for x := 0 to fMIMEMess.Header.ToList.Count - 1 do
    begin
      EmailOAuthDataModule.SendMessage(GetEmailAddr(fMIMEMess.Header.ToList[x]), fSubject, vBody, FileList,fclientid,fclientsecret, FRefreshToken, ffrom,ffromname);
    end;

  finally
    FileList.Free;
    Clear;
  end;
end;

procedure TDTEmailOAuth2.SetAttempts(const Value: Byte);
begin
  fAttempts := Value;
end;

procedure TDTEmailOAuth2.SetAutoTLS(const Value: boolean);
begin
  fSMTP.AutoTLS := Value;
end;

procedure TDTEmailOAuth2.SetFullSSL(const Value: boolean);
begin
  fSMTP.FullSSL := Value;
end;

procedure TDTEmailOAuth2.SetHost(const Value: string);
begin
  fSMTP.TargetHost := Value;
end;

procedure TDTEmailOAuth2.SetPassword(const Value: string);
begin
  fSMTP.Password := Value;
end;

procedure TDTEmailOAuth2.SetPort(const Value: string);
begin
  fSMTP.TargetPort := Value;
end;

procedure TDTEmailOAuth2.SetPriority(const Value: TMessPriority);
begin
 fMIMEMess.Header.Priority := Value;
end;

procedure TDTEmailOAuth2.SetSSLType(const Value: TSSLType);
begin
  fSMTP.Sock.SSL.SSLType := Value;
end;

procedure TDTEmailOAuth2.SetSubEvents(ASubEvents: TSubEvents);
begin
   if Assigned(FSubEvents) then
    FSubEvents.RemoveObserver(Self);  // Remove o observador anterior, se existir

   FSubEvents := ASubEvents;
   FSubEvents.AddObserver(Self);  // Adiciona o novo observador
end;

procedure TDTEmailOAuth2.SetUsername(const Value: string);
begin
  fSMTP.UserName := Value;
end;

procedure TDTEmailOAuth2.SmtpError(const pMsgError: string);
begin
  try
    fGetLastSmtpError := pMsgError;
    MailProcess(pmsError);
    DoException( Exception.Create(pMsgError) );
  finally
    Clear;
  end;
end;

end.
