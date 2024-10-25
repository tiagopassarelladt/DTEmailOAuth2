object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Demo - DTEmailOAuth2'
  ClientHeight = 439
  ClientWidth = 744
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object pnlTopo: TPanel
    Left = 0
    Top = 0
    Width = 744
    Height = 41
    Align = alTop
    TabOrder = 0
    object lblDescricao: TLabel
      Left = 7
      Top = 7
      Width = 144
      Height = 23
      Caption = 'DTEmailOAuth2'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -21
      Font.Name = 'Times New Roman'
      Font.Style = [fsItalic]
      ParentFont = False
    end
  end
  object pgc: TPageControl
    Left = 0
    Top = 41
    Width = 744
    Height = 398
    Cursor = crHandPoint
    ActivePage = tsConfigConta
    Align = alClient
    TabOrder = 1
    object tsMensagem: TTabSheet
      Caption = 'E-Mail'
      object Label2: TLabel
        Left = 206
        Top = 50
        Width = 96
        Height = 15
        Caption = 'Assunto (Subject):'
        Color = clBtnFace
        ParentColor = False
      end
      object Label6: TLabel
        Left = 206
        Top = 5
        Width = 138
        Height = 15
        Caption = 'Para e-Mail (AddAddress):'
        Color = clBtnFace
        ParentColor = False
      end
      object Label5: TLabel
        Left = 513
        Top = 50
        Width = 23
        Height = 15
        Caption = 'LOG'
        Color = clBtnFace
        ParentColor = False
      end
      object Label3: TLabel
        Left = 206
        Top = 95
        Width = 180
        Height = 15
        Caption = 'Mensagem (Alt Body modo texto)'
        Color = clBtnFace
        ParentColor = False
      end
      object Label4: TLabel
        Left = 209
        Top = 199
        Width = 167
        Height = 15
        Caption = 'Mensagem (Body modo HTML)'
        Color = clBtnFace
        ParentColor = False
      end
      object Label1: TLabel
        Left = 3
        Top = 234
        Width = 194
        Height = 50
        Alignment = taCenter
        AutoSize = False
        Caption = 
          'Altere os par'#226'metros na '#13#10'guia Configura'#231#227'o Conta '#13#10'antes de faz' +
          'er os envios.'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object lblAdressName: TLabel
        Left = 513
        Top = 5
        Width = 62
        Height = 15
        Caption = 'Para Nome:'
        Color = clBtnFace
        ParentColor = False
      end
      object grpOpcoes: TGroupBox
        Left = 3
        Top = 5
        Width = 194
        Height = 171
        Caption = '[ Op'#231#245'es ]'
        TabOrder = 0
        object cbUsarTXT: TCheckBox
          Left = 16
          Top = 17
          Width = 158
          Height = 19
          Caption = 'Enviar Mensagem em TXT'
          Checked = True
          State = cbChecked
          TabOrder = 0
        end
        object cbUsarHTML: TCheckBox
          Left = 16
          Top = 37
          Width = 170
          Height = 19
          Caption = 'Enviar Mensagem em HTML'
          TabOrder = 1
        end
        object cbAddImgHTML: TCheckBox
          Left = 16
          Top = 56
          Width = 156
          Height = 19
          Caption = 'Incluir Imagem em HTML'
          TabOrder = 2
        end
        object cbAddImgAtt: TCheckBox
          Left = 16
          Top = 76
          Width = 156
          Height = 19
          Caption = 'Incluir Imagem em Anexo'
          TabOrder = 3
        end
        object cbAddPDF: TCheckBox
          Left = 16
          Top = 96
          Width = 129
          Height = 19
          Caption = 'Incluir Anexo de PDF'
          TabOrder = 4
        end
        object cbAddXML: TCheckBox
          Left = 16
          Top = 117
          Width = 141
          Height = 19
          Caption = 'Incluir XML por Stream'
          TabOrder = 5
        end
        object cbUsarThread: TCheckBox
          Left = 16
          Top = 137
          Width = 80
          Height = 19
          Caption = 'Usar thread'
          TabOrder = 6
        end
      end
      object edSubject: TEdit
        Left = 206
        Top = 68
        Width = 301
        Height = 23
        TabOrder = 3
        Text = 'Teste de Envio '#193#201#205#211#218#199#231#225#233#237#250#243' - 0'
      end
      object edtAddressEmail: TEdit
        Left = 206
        Top = 23
        Width = 301
        Height = 23
        TabOrder = 1
        Text = 'fulano@dominio.com.br'
      end
      object mLog: TMemo
        Left = 513
        Top = 68
        Width = 219
        Height = 274
        TabOrder = 6
      end
      object mAltBody: TMemo
        Left = 207
        Top = 113
        Width = 300
        Height = 80
        Lines.Strings = (
          'Linha 1'
          'Linha 2'
          '   Teste de e-mail'
          #193#201#205#211#218#231#199#227#194)
        TabOrder = 4
        WordWrap = False
      end
      object mBody: TMemo
        Left = 206
        Top = 214
        Width = 301
        Height = 128
        Lines.Strings = (
          
            '<html><head><meta http-equiv="content-type" content="text/html; ' +
            'charset=UTF-8"></head>'
          '<body text="#000000" bgcolor="#FFFFFF">'
          '<h1>Texto em HTML.</h1><br>'
          '<p>Teste de Envio '#193#201#205#211#218#199#231#225#233#237#250#243' '#221#205#195#227#245#213'</p><br>'
          '<img src='#39'cid:LogoDT'#39'>'
          '</body></html>'
          ''
          '')
        TabOrder = 5
        WordWrap = False
      end
      object ProgressBar1: TProgressBar
        Left = 207
        Top = 348
        Width = 525
        Height = 20
        Max = 11
        Step = 1
        TabOrder = 8
      end
      object bEnviar: TButton
        Left = 3
        Top = 343
        Width = 194
        Height = 25
        Caption = 'Enviar Email'
        TabOrder = 7
        OnClick = bEnviarClick
      end
      object edtAddressName: TEdit
        Left = 513
        Top = 23
        Width = 219
        Height = 23
        TabOrder = 2
        Text = 'DT Componentes'
      end
    end
    object tsConfigConta: TTabSheet
      Caption = 'Configura'#231#227'o Conta'
      ImageIndex = 1
      object lblHost: TLabel
        Left = 3
        Top = 48
        Width = 28
        Height = 15
        Caption = 'Host:'
        Color = clBtnFace
        ParentColor = False
      end
      object lblFrom: TLabel
        Left = 3
        Top = 3
        Width = 68
        Height = 15
        Caption = 'From e-Mail:'
        Color = clBtnFace
        ParentColor = False
      end
      object lblFromName: TLabel
        Left = 292
        Top = 2
        Width = 66
        Height = 15
        Caption = 'From Name:'
        Color = clBtnFace
        ParentColor = False
      end
      object lblUser: TLabel
        Left = 3
        Top = 95
        Width = 61
        Height = 15
        Caption = 'User Name:'
        Color = clBtnFace
        ParentColor = False
      end
      object lblPassword: TLabel
        Left = 292
        Top = 95
        Width = 53
        Height = 15
        Caption = 'Password:'
        Color = clBtnFace
        ParentColor = False
      end
      object lblPort: TLabel
        Left = 292
        Top = 48
        Width = 25
        Height = 15
        Caption = 'Port:'
        Color = clBtnFace
        ParentColor = False
      end
      object lblTipoAutenticacao: TLabel
        Left = 364
        Top = 49
        Width = 99
        Height = 15
        Caption = 'Tipo Autentica'#231#227'o:'
        Color = clBtnFace
        ParentColor = False
      end
      object lblDefaultCharset: TLabel
        Left = 3
        Top = 140
        Width = 84
        Height = 15
        Caption = 'Default Charset:'
        Color = clBtnFace
        ParentColor = False
      end
      object lbl1: TLabel
        Left = 292
        Top = 140
        Width = 63
        Height = 15
        Caption = 'IDE Charset:'
        Color = clBtnFace
        ParentColor = False
      end
      object Label7: TLabel
        Left = 466
        Top = 49
        Width = 48
        Height = 15
        Caption = 'SSL Type:'
        Color = clBtnFace
        ParentColor = False
      end
      object Label8: TLabel
        Left = 3
        Top = 187
        Width = 45
        Height = 15
        Caption = 'ClientID:'
        Color = clBtnFace
        ParentColor = False
      end
      object Label9: TLabel
        Left = 292
        Top = 187
        Width = 66
        Height = 15
        Caption = 'ClientSecret:'
        Color = clBtnFace
        ParentColor = False
      end
      object Label10: TLabel
        Left = 3
        Top = 236
        Width = 51
        Height = 15
        Caption = 'Provedor:'
        Color = clBtnFace
        ParentColor = False
      end
      object Label11: TLabel
        Left = 292
        Top = 236
        Width = 76
        Height = 15
        Caption = 'Refresh Token:'
        Color = clBtnFace
        ParentColor = False
      end
      object edtHost: TEdit
        Left = 3
        Top = 66
        Width = 283
        Height = 23
        TabOrder = 2
        Text = 'email-ssl.com.br'
      end
      object edtFrom: TEdit
        Left = 3
        Top = 21
        Width = 283
        Height = 23
        TabOrder = 0
        Text = 'fulano@gmail.com'
      end
      object edtFromName: TEdit
        Left = 292
        Top = 21
        Width = 283
        Height = 23
        TabOrder = 1
        Text = 'DT COMPONENTES'
      end
      object edtUser: TEdit
        Left = 3
        Top = 113
        Width = 283
        Height = 23
        TabOrder = 6
        Text = 'fulano@dominio.com.br'
      end
      object edtPassword: TEdit
        Left = 292
        Top = 113
        Width = 283
        Height = 23
        PasswordChar = '@'
        TabOrder = 7
        Text = '123456'
      end
      object chkMostraSenha: TCheckBox
        Left = 348
        Top = 94
        Width = 77
        Height = 17
        Caption = 'Mostrar?'
        TabOrder = 10
        OnClick = chkMostraSenhaClick
      end
      object edtPort: TEdit
        Left = 292
        Top = 66
        Width = 58
        Height = 23
        TabOrder = 3
        Text = '465'
      end
      object chkTLS: TCheckBox
        Left = 364
        Top = 68
        Width = 45
        Height = 17
        Caption = 'TLS'
        Checked = True
        State = cbChecked
        TabOrder = 4
      end
      object chkSSL: TCheckBox
        Left = 415
        Top = 68
        Width = 45
        Height = 17
        Caption = 'SSL'
        Checked = True
        State = cbChecked
        TabOrder = 5
      end
      object cbbDefaultCharset: TComboBox
        Left = 3
        Top = 160
        Width = 283
        Height = 23
        Style = csDropDownList
        TabOrder = 8
      end
      object cbbIdeCharSet: TComboBox
        Left = 292
        Top = 160
        Width = 283
        Height = 23
        Style = csDropDownList
        TabOrder = 9
      end
      object cbxSSLTYPE: TComboBox
        Left = 466
        Top = 68
        Width = 109
        Height = 23
        Style = csDropDownList
        TabOrder = 11
      end
      object edtClientID: TEdit
        Left = 3
        Top = 205
        Width = 283
        Height = 23
        TabOrder = 12
      end
      object edtClientSecret: TEdit
        Left = 292
        Top = 205
        Width = 283
        Height = 23
        TabOrder = 13
      end
      object btnAuthenticate: TButton
        Left = 187
        Top = 285
        Width = 168
        Height = 25
        Cursor = crHandPoint
        Caption = 'Autenticar / Autorizar Auth2'
        TabOrder = 14
        OnClick = btnAuthenticateClick
      end
      object btnClearAuthToken: TButton
        Left = 374
        Top = 285
        Width = 201
        Height = 25
        Cursor = crHandPoint
        Caption = 'Limpar Autentica'#231#227'o / Autoriza'#231#227'o'
        TabOrder = 15
        OnClick = btnClearAuthTokenClick
      end
      object cboProvedor: TComboBox
        Left = 3
        Top = 256
        Width = 283
        Height = 23
        Style = csDropDownList
        ItemIndex = 3
        TabOrder = 16
        Text = '4 - Outros'
        Items.Strings = (
          '1 - Gmail'
          '2 - Hotmail'
          '3 - Microsoft'
          '4 - Outros')
      end
      object edtRefreshToken: TEdit
        Left = 292
        Top = 256
        Width = 283
        Height = 23
        TabOrder = 17
      end
      object Button1: TButton
        Left = 3
        Top = 285
        Width = 158
        Height = 25
        Cursor = crHandPoint
        Caption = '1 - Google Console'
        TabOrder = 18
        OnClick = Button1Click
      end
    end
  end
  object DTEmailOAuth21: TDTEmailOAuth2
    Host = '127.0.0.1'
    Port = '25'
    SetSSL = False
    SetTLS = False
    Attempts = 3
    ProvedorEmail = pvGmail
    DefaultCharset = UTF_8
    IDECharset = CP1252
    OnLog = DTEmailOAuth21Log
    OnAuthenticate = DTEmailOAuth21Authenticate
    Left = 628
    Top = 171
  end
end
