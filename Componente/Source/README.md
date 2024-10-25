
# DTEmailOAuth2  
![](Images/SMTPServiceLogos.png)  
Com este componente, além dos envios de email por SMTP, e IMAP também é possível autenticar com OAUTH2 e enviar uma mensagem de e-mail para endereços de e-mail do [gmail](https://www.gmail.com), [microsoft/office365](https://outlook.office.com/mail/) e também [hotmail.com/outlook.com/live.com](https://www.outlook.com). 

OAuth2 é um padrão aberto de autorização usado para conceder acesso a recursos protegidos por um servidor. Ele permite que um aplicativo ou serviço se autentique com um servidor de recursos e acesse recursos protegidos em nome de um usuário, sem exigir que o usuário forneça suas credenciais diretamente ao aplicativo.

Quando um usuário deseja acessar seu e-mail usando um cliente de e-mail ou outro aplicativo, o aplicativo pode usar OAuth2 para autenticar-se com o serviço de e-mail e solicitar acesso ao e-mail do usuário. O usuário será solicitado a fazer login em sua conta de e-mail e conceder acesso ao aplicativo. Uma vez que o acesso seja concedido, o aplicativo pode usar o token de acesso OAuth2 para autenticar-se com o serviço de e-mail e acessar o e-mail do usuário.

Usar OAuth2 para autenticação traz vários benefícios. Ele permite que os usuários concedam acesso ao seu e-mail sem compartilhar suas credenciais de login com o aplicativo, ajudando a proteger sua privacidade e segurança. Também facilita o acesso ao e-mail a partir de vários dispositivos e aplicativos, pois o usuário só precisa conceder o acesso uma vez e, em seguida, pode usar o token de acesso OAuth2 para autenticar-se no serviço de e-mail a partir de qualquer dispositivo.

![](Images/SampleIMAPSession.png)  
## Configuração do Google

Você precisará criar um arquivo chamado `Globals.pas` que contenha as seguintes constantes:  
![](Images/GooglePermissions.png)  
  * google_clientid  
  * google_clientsecret  
Para obter essas informações, acesse: [https://console.cloud.google.com/apis/credentials](https://console.cloud.google.com/apis/credentials)

  * google_clientaccount  

## Configuração da Microsoft

  * microsoft_clientid  
  * microsoft_clientaccount  
Para obter essas informações, acesse: [https://portal.azure.com/#blade/Microsoft_AAD_RegisteredApps/ApplicationsListBlade](https://portal.azure.com/#blade/Microsoft_AAD_RegisteredApps/ApplicationsListBlade)  
A Microsoft também exigirá várias permissões de aplicativo, conforme mostrado abaixo:  
![](Images/MSPermissions.png)  

  * clientsendtoaddress  
  * clientname  

## Dependências
Este projeto requer OpenSSL. Você pode encontrar os arquivos necessários em: [https://github.com/IndySockets/OpenSSL-Binaries](https://github.com/IndySockets/OpenSSL-Binaries)

Obrigado,  
Tiago Passarella
