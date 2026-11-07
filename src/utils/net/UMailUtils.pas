unit UMailUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ULBLogger, mimepart, Laz2_DOM, IniFiles;

type
  TArrayOfBoolean = Array of Boolean;

  { TMailServerData }

  TMailServerData = class(TObject)
    strict protected
      FServer   : String;
      FPort     : Integer;
      FUserName : String;
      FPassword : String;
      FUse_SSL  : Boolean;
      FUse_TLS  : Boolean;
      FEMail    : String;
      FSenderAlias : String;

      function get_InvalidData: Boolean; virtual;

      const
        cServer      : DOMString = 'Server';
        cPort        : DOMString = 'Port';
        cUserName    : DOMString = 'UserName';
        cPassword    : DOMString = 'Password';
        cUseSSL      : DOMString = 'UseSSL';
        cUseTLS      : DOMString = 'UseTLS';
        cEmail       : DOMString = 'Email';
        cSenderAlias : DOMString = 'SenderAlias';


    public
      function Save(): Boolean; virtual;
      function Load(): Boolean; virtual;

      function LoadFromXMLDocument(AFile: String; NodeName: DOMString): Boolean;
      function LoadFromXMLNode(ANode: TDOMNode): Boolean; virtual;
      function LoadFromINIFile(aIniFile: TIniFile): Boolean;

      function SaveIntoXMLNode(ADoc: TXMLDocument; ANode: TDOMNode): Boolean;

      function SendMails(ATo, ASubject: string; AMailBody, Attachments: TStringList; out SentTo: TArrayOfBoolean; MailNotification: Boolean): Boolean;

      property Server    : String  read FServer    write FServer;
      property Port      : Integer read FPort      write FPort;
      property UserName  : String  read FUserName  write FUserName;
      property Password  : String  read FPassword  write FPassword;
      property Use_SSL   : Boolean read FUse_SSL   write FUse_SSL;
      property Use_TLS   : Boolean read FUse_TLS   write FUse_TLS;
      property EMail     : String  read FEMail     write FEMail;
      property SenderAlias : String  read FSenderAlias write FSenderAlias;

      property InvalidData: Boolean read get_InvalidData;

      const
        cINI_SECTION_NAME = String('MailServer');
    end;

  { TIncomingMailServer }

  TIncomingMailServer = class(TMailServerData)
    public
      type TIncomingMailServerType = (POP3 = 1, IMAP = 2);
    strict private
      FType : TIncomingMailServerType;
      FFolderName : String;
    strict protected
      function get_InvalidData: Boolean; override;
      function getMailBody(AMimePart: TMimePart): String;
    public
      constructor Create;

      property ServerType: TIncomingMailServerType read FType write FType;
      property FolderName: String read FFolderName write FFolderName;
  end;

  { TEmailEssentialHeaders }

  TEmailEssentialHeaders = class(TObject)
    strict private
      FSubject : String;
      FFrom : String;
      FDate : TDateTime;
    public
      constructor Create;
      procedure Clear();

      property Subject: String read FSubject write FSubject;
      property From: String read FFrom write FFrom;
      property MailDate: TDateTime read FDate write FDate;
  end;

function SendMail(const AFrom, ATo, ASubject, AUser, APassword, AHost, APort : string;
                  AMailBody: TStringList; ASSL, ATLS : Boolean; var AMessage: string; FromAlias: String = '') : Boolean; overload;

function SendMail(const ATo, ASubject: String; SMTPData: TMailServerData;
                  AMailBody: TStringList; var AMessage: string) : Boolean; overload;

function SendMail(const AFrom, ATo, ASubject, AUser, APassword, AHost, APort : string;
                  AMailBody: TStringList; ASSL, ATLS : Boolean; var AMessage: string;
                  var SentTo: TArrayOfBoolean; Attachments: TStringList;
                  FromAlias: String = ''; MailNotification: Boolean = False) : Boolean; overload;


implementation

uses
  smtpsend, ssl_openssl11, mimemess, strutils, RegExpr, base64, laz2_XMLRead;

function SendMail(const ATo, ASubject: String; SMTPData: TMailServerData;
                  AMailBody: TStringList; var AMessage: string) : Boolean; overload;
begin
  Result := SendMail(SMTPData.EMail, ATo, ASubject, SMTPData.UserName, SMTPData.Password,
                     SMTPData.Server, IntToStr(SMTPData.Port), AMailBody, SMTPData.Use_SSL,
                     SMTPData.Use_TLS, AMessage, SMTPData.SenderAlias);
end;


function SendMail(const AFrom, ATo, ASubject, AUser, APassword, AHost,
  APort: string; AMailBody: TStringList; ASSL, ATLS: Boolean;
  var AMessage: string; var SentTo: TArrayOfBoolean; Attachments: TStringList;
  FromAlias: String; MailNotification: Boolean): Boolean;
var
  _smtp: TSMTPSend = nil;
  _MM : TMimeMess = nil;
  _Part : TMimePart = nil;
  _ATo : TStringList = nil;
  _RegEx : TRegExpr = nil;

  _tmp : String;
  _SingleMessage : String;
  j : Integer;


begin
  Result := False;
  SetLength(SentTo, 0);
  AMessage := '';

  if ASSL or ATLS then
    InitOpenSSL11;

  LBLogger.Write(5, 'umailutils.SendMail', TLBLoggerMessageType.lmt_Debug, 'Sending mail to <%s>',[ATo]);

  _tmp := ReplaceStr(ATo, ' ', '');
  _tmp := ReplaceStr(_tmp, ',', sLineBreak);
  _tmp := ReplaceStr(_tmp, ';', sLineBreak);

  try

    _MM := TMimeMess.Create;

    if Length(FromAlias) > 0 then
    begin
      _MM.Header.From := '"' + FromAlias + '" <' + AFrom + '>';
      LBLogger.Write(1, 'umailutils.SendMail', lmt_Debug, 'Sending mail with alias: <%s>', [FromAlias]);
    end
    else
      _MM.Header.From := AFrom;

    _MM.Header.Subject := ASubject;
    _MM.Header.ToList.Text := _tmp;
    _MM.Header.ReplyTo := AFrom;

    if MailNotification then
    begin
      _MM.Header.CustomHeaders.Add('Disposition-Notification-To: ' + _MM.Header.From);
      _MM.Header.CustomHeaders.Add('Return-Receipt-To: ' + _MM.Header.From);
      _MM.Header.CustomHeaders.Add('X-Confirm-Reading-To: ' + _MM.Header.From);
      _MM.Header.CustomHeaders.Add('Read-Receipt-To: ' + _MM.Header.From);
      _MM.Header.CustomHeaders.Add('Generate-Delivery-Report: ' + _MM.Header.From);
    end;

    if Assigned(Attachments) and (Attachments.Count > 0) then
      _Part := _MM.AddPartMultipart('mixed', nil);

    if (AMailBody <> nil) and (AMailBody.Count > 0) then
      _MM.AddPartText(AMailBody, _Part);

    if Assigned(Attachments) and (Attachments.Count > 0) then
    begin
      for j := 0 to Attachments.Count - 1 do
      begin
        if FileExists(Attachments.Strings[j]) then
          _MM.AddPartBinaryFromFile(Attachments.Strings[j], _Part);
      end;
    end;

    _MM.EncodeMessage;

    _smtp := TSMTPSend.Create;

    _smtp.UserName := AUser;
    _smtp.Password := APassword;

    _smtp.TargetHost := AHost;
    _smtp.TargetPort := APort;

    _smtp.AutoTLS := ATLS;
    _smtp.FullSSL := APort <> '25';

    if _smtp.Login then
    begin

      if Length(AMessage) = 0 then
      begin
        _ATo := TStringList.Create;
        _ATo.Text := _tmp;

        LBLogger.Write(3, 'umailutils.SendMail', lmt_Debug, 'Numero destinatari: %d',[_ATo.Count]);

        SetLength(SentTo, _ATo.Count);

        _SingleMessage := '';

        if _smtp.MailFrom(AFrom, Length(_MM.Lines.Text)) then
        begin
          LBLogger.Write(5, 'umailutils.SendMail', lmt_Debug, 'MailFrom set', []);

          _RegEx := TRegExpr.Create;
          _RegEx.Expression := '^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,6}$';

          for j := 0 to _ATo.Count - 1 do
          begin
            if Length(_ATo.Strings[j]) > 0 then
            begin
              if _RegEx.Exec(_ATo.Strings[j]) then
              begin
                SentTo[j] := _smtp.MailTo(_ATo.Strings[j]);
                if not SentTo[j] then
                begin
                  _SingleMessage := _SingleMessage + Format(' MailTo: %s  -  Error: %s', [_ATo.Strings[j], _smtp.EnhCodeString]);
                  LBLogger.Write(3, 'umailutils.SendMail', lmt_Warning, 'MailTo: %s  -  Error: %s', [_ATo.Strings[j], _smtp.EnhCodeString]);
                end
                else
                  LBLogger.Write(3, 'umailutils.SendMail', lmt_Debug, 'Mail sent to: %s', [_ATo.Strings[j]]);
              end
              else begin
                SentTo[j] := False;
                _SingleMessage := _SingleMessage + 'MailTo: <' + _ATo.Strings[j] + '> invalid mail address';
                LBLogger.Write(3, 'umailutils.SendMail', lmt_Warning, PChar('MailTo: <' + _ATo.Strings[j] + '> invalid mail address'));
              end;
            end
            else begin
              SentTo[j] := False;
              LBLogger.Write(3, 'umailutils.SendMail', lmt_Warning, PChar('Destination address not set!'));
            end;
          end;

          FreeAndNil(_RegEx);

          if (Length(_SingleMessage) = 0) and (not _smtp.MailData(_MM.Lines)) then
            _SingleMessage := 'MailData: ' + _smtp.EnhCodeString;
        end
        else
          _SingleMessage := Format('MailFrom: %s  -  Error: %s',[AFrom, _smtp.EnhCodeString]);

        AMessage := AMessage + IfThen(Length(AMessage) = 0, '', sLineBreak) + _SingleMessage;

        FreeAndNil(_ATo);
      end
      else
        LBLogger.Write(1, 'SendMail', lmt_Warning, 'Error setting TLS: <%s>', [AMessage]);

      if not _smtp.Logout() then
      begin
        if Length(AMessage) > 0 then
          AMessage := AMessage + sLineBreak;
        AMessage := AMessage + 'Logout: ' + _smtp.EnhCodeString;
        LBLogger.Write(3, 'umailutils.SendMail', lmt_Warning, PChar('Error logging out: ' + _smtp.EnhCodeString));
      end
      else
        LBLogger.Write(3, 'umailutils.SendMail', lmt_Debug, 'Logged out!');


      if Length(SentTo) > 0 then
      begin
        for j := 0 to High(SentTo) do
        begin
          Result := SentTo[j];
          if not Result then
            Break;
        end;

        if not Result then
          LBLogger.Write(1, 'umailutils.SendMail', lmt_Warning, 'Errors sending mail: %s', [AMessage]);
      end
      else
        LBLogger.Write(3, 'umailutils.SendMail', lmt_Warning, 'Data not sent!');

    end
    else begin
      AMessage := 'Login failed: ' + _smtp.EnhCodeString;
      LBLogger.Write(1, 'umailutils.SendMail', lmt_Warning, PChar(AMessage));
    end;

  except
    on E: Exception do
    begin
      Result := False;
      LBLogger.Write(1, 'umailutils.SendMail', lmt_Error, PChar(E.Message));
      AMessage := E.Message;
    end;
  end;

  if _ATo <> nil then
    _ATo.Free;

  if _RegEx <> nil then
    _RegEx.Free;

  if _MM <> nil then
    _MM.Free;

  if _smtp <> nil then
    _smtp.Free;

end;

function SendMail(const AFrom, ATo, ASubject, AUser, APassword, AHost, APort : string;
                  AMailBody: TStringList; ASSL, ATLS : Boolean; var AMessage: string; FromAlias: String = '') : Boolean;
var
  _smtp: TSMTPSend = nil;
  _MM : TMimeMess = nil;
  _Part : TMimePart = nil;
  j : Integer;
  _To : String;


begin
  Result := False;
  AMessage := '';

  InitOpenSSL11;

  _To := ATo;
  if Length(_To) > 0 then
  begin
    _To := AnsiReplaceStr(_To,';',sLineBreak);
    _To := AnsiReplaceStr(_To,',',sLineBreak);
  end;

  _MM := TMimeMess.Create;
  if Length(FromAlias) > 0 then
    _MM.Header.From := '"' + FromAlias + '" <' + AFrom + '>'
  else
    _MM.Header.From := AFrom;

  LBLogger.Write(1, 'umailutils.SendMail', lmt_Debug, 'Sending from <%s> to <%s>', [_MM.Header.From, ATo]);

  _MM.Header.Subject := ASubject;
  _MM.Header.ToList.Text := _To;
  _MM.Header.ReplyTo := AFrom;

//  _Part := _MM.AddPartMultipart('mixed', nil);
  if AMailBody <> nil then
    _MM.AddPartText(AMailBody, _Part);
//  _MM.AddPartBinaryFromFile('d:\Perche un ERP.pdf', _Part);
  _MM.EncodeMessage;

  _smtp := TSMTPSend.Create;
  try
    _smtp.UserName := AUser;
    _smtp.Password := APassword;

    _smtp.TargetHost := AHost;
    _smtp.TargetPort := APort;

    _smtp.AutoTLS := True;
    _smtp.FullSSL := APort <> '25';
(*
    _smtp.FullSSL := ASSL;
    _smtp.AutoTLS := ATLS;
*)

    if _smtp.Login then
    begin
      if ATLS then
         if not _smtp.StartTLS() then
           AMessage := 'START TSL: ' + _smtp.EnhCodeString;

      if Length(AMessage) = 0 then
      begin
        for j := 0 to _MM.Header.ToList.Count - 1 do
        begin
          if not _smtp.MailFrom(AFrom, Length(AFrom)) then
            AMessage := 'MailFrom: ' + _smtp.EnhCodeString;
          if (Length(AMessage) = 0) and (not _smtp.MailTo(_MM.Header.ToList[j])) then
            AMessage := 'MailTo: ' + _smtp.EnhCodeString;
          if (Length(AMessage) = 0) and (not _smtp.MailData(_MM.Lines)) then
            AMessage := 'MailData: ' + _smtp.EnhCodeString;
        end;
      end;

      if not _smtp.Logout() then
      begin
        if Length(AMessage) > 0 then
          AMessage += sLineBreak;
        AMessage += 'Logout: ' + _smtp.EnhCodeString;
      end
      else begin
        Result := Boolean(Length(AMessage) = 0);
        if not Result then
          LBLogger.Write(1, 'SendMail', lmt_Warning, 'Errors: %s', [AMessage]);
      end;


    end
    else begin
      AMessage := 'Not enable to login: ' + _smtp.EnhCodeString;
      LBLogger.Write(1, 'SendMail', lmt_Warning, PChar(AMessage));
    end;

  except
    on E: Exception do
      LBLogger.Write(1, 'SendMail', lmt_Error, PChar(E.Message));
  end;

  if _MM <> nil then
    _MM.Free;

  if _smtp <> nil then
    _smtp.Free;
end;

{ TIncomingMailServer }

function TIncomingMailServer.get_InvalidData: Boolean;
begin
  Result := inherited get_InvalidData;
  if Result and (FType = TIncomingMailServerType.IMAP) then
    Result := Length(FFolderName) > 0;
end;

function TIncomingMailServer.getMailBody(AMimePart: TMimePart): String;
var
  i : Integer;

begin
  Result := '';

  if AMimePart <> nil then
  begin
    LBLogger.Write(5, 'TIncomingMailServer.getMailBody', lmt_Debug, 'SubParts %d. Mime part content ID: <%s>',[AMimePart.GetSubPartCount, AMimePart.ContentID]);
    AMimePart.DecodePart;

    //AMimePart.Lines.SaveToFile('c:\MailLines.txt');
    if AMimePart.GetSubPartCount = 0 then
    begin
      if AMimePart.EncodingCode = ME_BASE64 then
        Result := DecodeStringBase64(AMimePart.PartBody.Text)
      else
        Result := AMimePart.PartBody.Text;

      LBLogger.Write(5, 'TIncomingMailServer.getMailBody', lmt_Debug, 'Body: <%s>',[Result]);
    end
    else begin
      for i := 0 to AMimePart.GetSubPartCount - 1 do
      begin
        Result := Self.getMailBody(AMimePart.GetSubPart(i));
        if Length(Result) > 0 then
          Break;
      end;
    end;
  end;
end;

constructor TIncomingMailServer.Create;
begin
  inherited Create;
  FType := TIncomingMailServerType.POP3;
  FFolderName := '';
end;

{ TEmailEssentialHeaders }

constructor TEmailEssentialHeaders.Create;
begin
  inherited Create;
  Clear();
end;

procedure TEmailEssentialHeaders.Clear;
begin
  FSubject := '';
  FFrom := '';
  FDate := 0;
end;

{ TMailServerData }

function TMailServerData.get_InvalidData: Boolean;
begin
  Result := (Length(FServer) = 0) or
            (FPort <= 0);
end;

function TMailServerData.Save(): Boolean;
begin
  Result := False;
end;

function TMailServerData.Load(): Boolean;
begin
  Result := False;
end;

function TMailServerData.LoadFromXMLDocument(AFile: String; NodeName: DOMString): Boolean;
var
  _Doc : TXMLDocument = nil;
  _Root : TDOMNode;
  _Item : TDOMNode;

begin
  Result := False;

  try
    if FileExists(AFile) then
    begin
      ReadXMLFile(_Doc, AFile);

      if _Doc <> nil then
      begin
        _Root := _Doc.DocumentElement;
        if _Root.NodeName = NodeName then
          _Item := _Root
        else
          _Item := _Root.FindNode(DOMString(NodeName));

        if _Item = nil then
          LBLogger.Write(1, 'TMailServerData.LoadFromXMLDocument', lmt_Warning, 'Node <%s> not found!', [NodeName])
        else
          Result := Self.LoadFromXMLNode(_Item);
      end
      else
        LBLogger.Write(1, 'TMailServerData.LoadFromXMLDocument', lmt_Warning, 'Error reading configuration file <%s>',[AFile]);

    end
    else
      LBLogger.Write(1, 'TMailServerData.LoadFromXMLDocument', lmt_Warning, 'File <%s> not found!', [AFile]);

  except
    on E: Exception do
      LBLogger.Write(1, 'TMailServerData.LoadFromXMLDocument', lmt_Error, PChar(E.Message));
  end;

  if _Doc <> nil then
    _Doc.Free;
end;

function TMailServerData.LoadFromXMLNode(ANode: TDOMNode): Boolean;
var
  _Item : TDOMNode;

begin
  Result := False;

  if ANode <> nil then
  begin


    _Item := ANode.FindNode(cServer);
    if _Item <> nil then
      FServer := AnsiString(_Item.TextContent);

    _Item := ANode.FindNode(cPort);
    if _Item <> nil then
      FPort := StrToIntDef(AnsiString(_Item.TextContent), 0);

    if (Length(FServer) > 0) and (FPort > 0) then
    begin
      _Item := ANode.FindNode(cUserName);
      if _Item <> nil then
        FUserName := AnsiString(_Item.TextContent);

      _Item := ANode.FindNode(cPassword);
      if _Item <> nil then
        FPassword := AnsiString(_Item.TextContent);

      _Item := ANode.FindNode(cUseSSL);
      if _Item <> nil then
        FUse_SSL := StrToIntDef(AnsiString(_Item.TextContent), 0) = 1;

      _Item := ANode.FindNode(cUseTLS);
      if _Item <> nil then
        FUse_TLS := StrToIntDef(AnsiString(_Item.TextContent), 0) = 1;

      _Item := ANode.FindNode(cEmail);
      if _Item <> nil then
        FEMail := AnsiString(_Item.TextContent);

      _Item := ANode.FindNode(cSenderAlias);
      if _Item <> nil then
        FSenderAlias := AnsiString(_Item.TextContent);

      Result := True;
    end
    else
      LBLogger.Write(1, 'TMailServerData.LoadFromXMLNode', lmt_Warning, 'Wrong parameters! Server <%s>  -  Port: %d', [FServer, FPort]);

  end
  else
    LBLogger.Write(1, 'TMailServerData.LoadFromXMLNode', lmt_Warning, 'No XML node!');


end;

function TMailServerData.LoadFromINIFile(aIniFile: TIniFile): Boolean;
begin
  Result := False;

  if aIniFile <> nil then
  begin
    FServer      := aIniFile.ReadString(cINI_SECTION_NAME, cServer, '');
    FPort        := aIniFile.ReadInteger(cINI_SECTION_NAME, cPort, 0);
    FUserName    := aIniFile.ReadString(cINI_SECTION_NAME, cUserName, '');
    FPassword    := aIniFile.ReadString(cINI_SECTION_NAME, cPassword, '');
    FUse_SSL     := aIniFile.ReadInteger(cINI_SECTION_NAME, cUseSSL, 0) = 1;
    FUse_TLS     := aIniFile.ReadInteger(cINI_SECTION_NAME, cUseTLS, 0) = 1;
    FSenderAlias := aIniFile.ReadString(cINI_SECTION_NAME, cSenderAlias, '');
    FEMail       := aIniFile.ReadString(cINI_SECTION_NAME, cEmail, '');

    Result := (FServer <> '') and (FPort >= 0) and (FUserName <> '') and (FPassword <> '');
  end
  else
    LBLogger.Write(1, 'TMailServerData.LoadFromINIFile', lmt_Warning, 'No INI file!');
end;

function TMailServerData.SaveIntoXMLNode(ADoc: TXMLDocument; ANode: TDOMNode): Boolean;
var
  _Item : TDOMNode;

begin
  Result := False;

  if (ADoc = nil) or (ANode = nil) then
    Exit;

  _Item := ADoc.CreateElement(cServer);
  ANode.AppendChild(_Item);
  _Item.TextContent := DOMString(FServer);

  _Item := ADoc.CreateElement(cPort);
  ANode.AppendChild(_Item);
  _Item.TextContent := DOMString(IntToStr(FPort));

  _Item := ADoc.CreateElement(cUserName);
  ANode.AppendChild(_Item);
  _Item.TextContent := DOMString(FUserName);

  _Item := ADoc.CreateElement(cPassword);
  ANode.AppendChild(_Item);
  _Item.TextContent := DOMString(FPassword);

  _Item := ADoc.CreateElement(cUseSSL);
  ANode.AppendChild(_Item);
  _Item.TextContent := DOMString(IfThen(FUse_SSL, '1', '0'));

  _Item := ADoc.CreateElement(cUseTLS);
  ANode.AppendChild(_Item);
  _Item.TextContent := DOMString(IfThen(FUse_TLS, '1', '0'));

  _Item := ADoc.CreateElement(cEmail);
  ANode.AppendChild(_Item);
  _Item.TextContent := DOMString(FEMail);

  _Item := ADoc.CreateElement(cSenderAlias);
  ANode.AppendChild(_Item);
  _Item.TextContent := DOMString(FSenderAlias);

  Result := True;
end;

function TMailServerData.SendMails(ATo, ASubject: string; AMailBody, Attachments: TStringList; out SentTo: TArrayOfBoolean; MailNotification: Boolean): Boolean;
var
  _Message : String = '';

begin
  Result := False;

  if not Self.InvalidData then
  begin
    Result := SendMail(FEMail, ATo, ASubject, FUserName, FPassword, FServer, IntToStr(FPort),
                       AMailBody, FUse_SSL, FUse_TLS, _Message, SentTo, Attachments, FSenderAlias, True);

    if not Result then
      LBLogger.Write(1, 'TMailServerData.SendMail', lmt_Warning, 'Error sending mails!');

    if Length(_Message) > 0 then
      LBLogger.Write(1, 'TMailServerData.SendMail', lmt_Warning, PChar(_Message));

  end
  else
    LBLogger.Write(1, 'TMailServerData.SendMail', lmt_Warning, 'Wrong parameters');

end;

end.

