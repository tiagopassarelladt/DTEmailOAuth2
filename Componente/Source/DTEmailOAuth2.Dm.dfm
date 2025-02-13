object EmailOAuthDataModule: TEmailOAuthDataModule
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 225
  Width = 661
  object IdSSLIOHandlerSocketPOP: TIdSSLIOHandlerSocketOpenSSL
    Destination = ':110'
    Intercept = IdConnectionPOP
    MaxLineAction = maException
    Port = 110
    DefaultPort = 0
    SSLOptions.Method = sslvTLSv1_2
    SSLOptions.SSLVersions = [sslvTLSv1_2]
    SSLOptions.Mode = sslmClient
    SSLOptions.VerifyMode = []
    SSLOptions.VerifyDepth = 0
    Left = 240
    Top = 80
  end
  object IdPOP3: TIdPOP3
    Intercept = IdConnectionPOP
    IOHandler = IdSSLIOHandlerSocketPOP
    AuthType = patSASL
    AutoLogin = False
    SASLMechanisms = <>
    Left = 240
    Top = 144
  end
  object IdSMTP1: TIdSMTP
    Intercept = IdConnectionInterceptSMTP
    IOHandler = IdSSLIOHandlerSocketSMTP
    AuthType = satSASL
    SASLMechanisms = <>
    UseTLS = utUseRequireTLS
    Left = 80
    Top = 144
  end
  object IdConnectionInterceptSMTP: TIdConnectionIntercept
    OnReceive = IdConnectionReceive
    OnSend = IdConnectionSend
    Left = 80
    Top = 16
  end
  object IdSSLIOHandlerSocketSMTP: TIdSSLIOHandlerSocketOpenSSL
    Destination = ':25'
    Intercept = IdConnectionInterceptSMTP
    MaxLineAction = maException
    Port = 25
    DefaultPort = 0
    SSLOptions.Method = sslvTLSv1_2
    SSLOptions.SSLVersions = [sslvTLSv1_2]
    SSLOptions.Mode = sslmClient
    SSLOptions.VerifyMode = []
    SSLOptions.VerifyDepth = 0
    Left = 80
    Top = 80
  end
  object IdHTTPServer1: TIdHTTPServer
    Bindings = <>
    DefaultPort = 1212
    OnCommandGet = IdHTTPServer1CommandGet
    Left = 552
    Top = 16
  end
  object IdConnectionInterceptIMAP: TIdConnectionIntercept
    OnReceive = IdConnectionReceive
    OnSend = IdConnectionSend
    Left = 400
    Top = 16
  end
  object IdConnectionPOP: TIdConnectionIntercept
    OnReceive = IdConnectionReceive
    OnSend = IdConnectionSend
    Left = 240
    Top = 16
  end
  object IdIMAP: TIdIMAP4
    Intercept = IdConnectionInterceptIMAP
    IOHandler = IdSSLIOHandlerSocketIMAP
    UseTLS = utUseRequireTLS
    SASLMechanisms = <>
    AuthType = iatSASL
    MilliSecsToWaitToClearBuffer = 10
    Left = 400
    Top = 144
  end
  object IdSSLIOHandlerSocketIMAP: TIdSSLIOHandlerSocketOpenSSL
    Destination = ':143'
    Intercept = IdConnectionInterceptIMAP
    MaxLineAction = maException
    Port = 143
    DefaultPort = 0
    SSLOptions.Method = sslvTLSv1_2
    SSLOptions.SSLVersions = [sslvTLSv1_2]
    SSLOptions.Mode = sslmClient
    SSLOptions.VerifyMode = []
    SSLOptions.VerifyDepth = 0
    Left = 400
    Top = 80
  end
end
