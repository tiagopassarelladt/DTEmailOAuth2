unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls,
  DTEmailOAuth2.OAuth2;

type
  TForm1 = class(TForm)
    pnlTopo: TPanel;
    lblDescricao: TLabel;
    pgc: TPageControl;
    tsMensagem: TTabSheet;
    Label2: TLabel;
    Label6: TLabel;
    Label5: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label1: TLabel;
    lblAdressName: TLabel;
    grpOpcoes: TGroupBox;
    cbUsarTXT: TCheckBox;
    cbUsarHTML: TCheckBox;
    cbAddImgHTML: TCheckBox;
    cbAddImgAtt: TCheckBox;
    cbAddPDF: TCheckBox;
    cbAddXML: TCheckBox;
    cbUsarThread: TCheckBox;
    edSubject: TEdit;
    edtAddressEmail: TEdit;
    mLog: TMemo;
    mAltBody: TMemo;
    mBody: TMemo;
    ProgressBar1: TProgressBar;
    bEnviar: TButton;
    edtAddressName: TEdit;
    tsConfigConta: TTabSheet;
    lblHost: TLabel;
    lblFrom: TLabel;
    lblFromName: TLabel;
    lblUser: TLabel;
    lblPassword: TLabel;
    lblPort: TLabel;
    lblTipoAutenticacao: TLabel;
    lblDefaultCharset: TLabel;
    lbl1: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    edtHost: TEdit;
    edtFrom: TEdit;
    edtFromName: TEdit;
    edtUser: TEdit;
    edtPassword: TEdit;
    chkMostraSenha: TCheckBox;
    edtPort: TEdit;
    chkTLS: TCheckBox;
    chkSSL: TCheckBox;
    cbbDefaultCharset: TComboBox;
    cbbIdeCharSet: TComboBox;
    cbxSSLTYPE: TComboBox;
    edtClientID: TEdit;
    edtClientSecret: TEdit;
    btnAuthenticate: TButton;
    btnClearAuthToken: TButton;
    cboProvedor: TComboBox;
    DTEmailOAuth21: TDTEmailOAuth2;
    edtRefreshToken: TEdit;
    Label11: TLabel;
    Button1: TButton;
    procedure chkMostraSenhaClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DTEmailOAuth21AfterMailProcess(Sender: TObject);
    procedure DTEmailOAuth21BeforeMailProcess(Sender: TObject);
    procedure btnAuthenticateClick(Sender: TObject);
    procedure btnClearAuthTokenClick(Sender: TObject);
    procedure bEnviarClick(Sender: TObject);
    procedure DTEmailOAuth21Authenticate(Sender: TObject;
      const Token: string);
    procedure DTEmailOAuth21Log(const log: string);
    procedure Button1Click(Sender: TObject);

  private
     procedure ConfiguraComponente;
     procedure HandleLog(const log : string);
  public

  end;

var
  Form1: TForm1;

implementation

uses
  blcksock, System.TypInfo;

{$R *.dfm}

{ TForm1 }

procedure TForm1.bEnviarClick(Sender: TObject);
var
  Dir, ArqXML : string;
  MS          : TMemoryStream;
  P, N        : Integer;
begin
  mLog.Lines.Clear;
  ProgressBar1.Position := 1;
  Dir                   := ExtractFilePath(ParamStr(0));

  P := pos(' - ', edSubject.Text);
  if P > 0 then
  begin
    N              := StrToIntDef(copy(edSubject.Text, P + 3, 5), 0) + 1;
    edSubject.Text := copy(edSubject.Text, 1, P + 2) + IntToStr(N);
  end;

  DTEmailOAuth21.Clear;
  DTEmailOAuth21.IsHTML  := cbUsarHTML.Checked;
  DTEmailOAuth21.Subject := edSubject.Text;

  ConfiguraComponente;

  // mensagem principal do e-mail. pode ser html ou texto puro
  if cbUsarTXT.Checked then
    DTEmailOAuth21.AltBody.Assign(mAltBody.Lines);

  if cbUsarHTML.Checked then
    DTEmailOAuth21.Body.Assign(mBody.Lines);

  if cbUsarHTML.Checked and cbAddImgHTML.Checked then
  begin
    // Depende de: "<img src='cid:LogoDT'>" em DTEmailOAuth21.Body;
    if Pos('cid:LogoDT', DTEmailOAuth21.Body.Text) > 0 then
      DTEmailOAuth21.AddAttachment(Dir + 'LogoDT.png', 'LogoDT', adInline);
  end;

  if cbAddImgAtt.Checked then
    DTEmailOAuth21.AddAttachment(Dir + 'LogoDT.jpg', '', adAttachment);

  if cbAddPDF.Checked then
    DTEmailOAuth21.AddAttachment(Dir + '35150905481336000137550010000111291000111298-nfe.pdf', 'DANFE', adAttachment);

  if cbAddXML.Checked then
  begin
    MS := TMemoryStream.Create;
    try
      ArqXML := '35150905481336000137550010000111291000111298-nfe.xml';
      MS.LoadFromFile(Dir + ArqXML);
      DTEmailOAuth21.AddAttachment(MS, ArqXML, adAttachment);
    finally
      MS.Free;
    end;
  end;

 if DTEmailOAuth21.ProvedorEmail = pvOutros then
 begin
      try
        DTEmailOAuth21.Send(cbUsarThread.Checked);
      except on E: Exception do
        ShowMessage(E.Message);
      end;
 end;

 if DTEmailOAuth21.ProvedorEmail <> pvOutros then
 begin

     try
        DTEmailOAuth21.SendMessageAuth2;
      except on E: Exception do
        ShowMessage(E.Message);
      end;
 end;

end;

procedure TForm1.btnAuthenticateClick(Sender: TObject);
begin
    ConfiguraComponente;
    DTEmailOAuth21.Authenticate;
end;

procedure TForm1.btnClearAuthTokenClick(Sender: TObject);
begin
    DTEmailOAuth21.ClearAuthentication;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
   DTEmailOAuth21.OpenGoogleConsole;
end;

procedure TForm1.chkMostraSenhaClick(Sender: TObject);
begin
    if chkMostraSenha.Checked then
      edtPassword.PasswordChar := #0
    else
      edtPassword.PasswordChar := '@';
end;

procedure TForm1.ConfiguraComponente;
begin
      DTEmailOAuth21.From           := edtFrom.Text;
      DTEmailOAuth21.FromName       := edtFromName.Text;
      DTEmailOAuth21.Host           := edthost.Text;
      DTEmailOAuth21.port           := edtport.text;
      DTEmailOAuth21.SetTLS         := chkTLS.Checked;
      DTEmailOAuth21.SetSSL         := chkSSL.Checked;
      DTEmailOAuth21.Username       := edtUser.Text;
      DTEmailOAuth21.Password       := edtPassword.Text;
      DTEmailOAuth21.ClientID       := edtClientID.Text;
      DTEmailOAuth21.ClientSecret   := edtClientSecret.Text;
      dtemailoauth21.refreshtoken   := edtrefreshtoken.text;
      DTEmailOAuth21.SSLType        := TSSLType(cbxSSLTYPE.ItemIndex);
      DTEmailOAuth21.DefaultCharset := TMailCharset(cbbDefaultCharset.ItemIndex);
      DTEmailOAuth21.IDECharset     := TMailCharset(cbbIdeCharSet.ItemIndex);
      DTEmailOAuth21.AddAddress(edtAddressEmail.text, edtAddressName.text);
      DTEmailOAuth21.OnLog          := HandleLog;

      DTEmailOAuth21.ProvedorEmail  := TProvedorEmail(GetEnumValue(TypeInfo(TProvedorEmail), cboprovedor.Text));
end;

procedure TForm1.DTEmailOAuth21AfterMailProcess(Sender: TObject);
begin
    mLog.Lines.Add('Depois de Enviar o email: ' + TDTEmailOAuth2(Sender).Subject);
end;

procedure TForm1.DTEmailOAuth21Authenticate(Sender: TObject;
  const Token: string);
begin
    if length(Token) > 0 then
    begin
     edtrefreshtoken.text := Token;
     mlog.lines.add('autenticação: ' + Token);
    end;
end;

procedure TForm1.DTEmailOAuth21BeforeMailProcess(Sender: TObject);
begin
    mLog.Lines.Add('Antes de Enviar o email: ' + TDTEmailOAuth2(Sender).Subject);
end;

procedure TForm1.DTEmailOAuth21Log(const log: string);
begin
   mlog.Lines.Add(log);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  m        : TMailCharset;
  LTLS     : TSSLType;
  Provedor : TProvedorEmail;
begin
  cbbDefaultCharset.Items.Clear;
  for m := Low(TMailCharset) to High(TMailCharset) do
    cbbDefaultCharset.Items.Add(GetEnumName(TypeInfo(TMailCharset), integer(m)));

  cbbDefaultCharset.ItemIndex := 0;
  cbbIdeCharSet.Items.Assign(cbbDefaultCharset.Items);
  cbbIdeCharSet.ItemIndex     := 0;

  for LTLS := Low(TSSLType) to High(TSSLType) do
    cbxSSLTYPE.Items.Add(GetEnumName(TypeInfo(TSSLType), integer(LTLS)));

  cboprovedor.Items.Clear;
  for Provedor := Low(TProvedorEmail) to High(TProvedorEmail) do
    cboprovedor.Items.Add(GetEnumName(TypeInfo(TProvedorEmail), Ord(Provedor)));

  cbxSSLTYPE.ItemIndex        := 0;
  cbbDefaultCharset.ItemIndex := 27;
  cbbIdeCharSet.ItemIndex     := 15;
  cboprovedor.itemindex       := 0;
end;

procedure TForm1.HandleLog(const log: string);
begin
    mlog.lines.add(log);
end;

end.
