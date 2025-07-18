unit uHTTPConsts;

{$mode objfpc}{$H+}

interface

const
  // 🔹 Standard HTTP Headers
  cHTTPHeader_ContentType      = 'Content-Type';
  cHTTPHeader_ContentLength    = 'Content-Length';
  cHTTPHeader_Connection       = 'Connection';
  cHTTPHeader_Upgrade          = 'Upgrade';
  cHTTPHeader_Host             = 'Host';
  cHTTPHeader_Range            = 'Range';
  cHTTPHeader_Accept           = 'Accept';
  cHTTPHeader_ContentRange     = 'Content-Range';
  cHTTPHeader_AcceptRanges     = 'Accept-Ranges';

  // 🔹 Common Header Values
  cHTTPValue_ConnectionClose   = 'close';
  cHTTPValue_ConnectionUpgrade = 'Upgrade';
  cHTTPValue_ConnectionKeepAlive = 'keep-alive';
  cHTTPValue_UpgradeWebSocket  = 'websocket';

  // 🔹 MIME Types
  cHTTPValue_ContentTypeJSON   = 'application/json';
  cHTTPValue_ContentTypeHTML   = 'text/html';
  cHTTPValue_ContentTypePlain  = 'text/plain';
  cHTTPValue_ContentTypeOctet  = 'application/octet-stream';
  cHTTPValue_ContentTypeJPEG   = 'image/jpeg';
  cHTTPValue_ContentTypePNG    = 'image/png';
  cHTTPValue_ContentTypePDF    = 'application/pdf';

  // 🔹 WebSocket Specific Headers
  cWebSocketHeader_SecKey      = 'Sec-WebSocket-Key';
  cWebSocketHeader_Protocol    = 'Sec-WebSocket-Protocol';
  cWebSocketHeader_Accept      = 'Sec-WebSocket-Accept';

  // 🔹 WebSocket Handshake Response Lines
  cWebSocket_HandshakeStatus   = 'HTTP/1.1 101 Switching Protocols' + #13#10;
  cWebSocket_HandshakeUpgrade  = 'Upgrade: websocket' + #13#10;
  cWebSocket_HandshakeConnect  = 'Connection: Upgrade' + #13#10;

  // 🔹 WebSocket GUID for handshake
  cWebSocket_GUID              = '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';

  // 🔹 WebSocket Frame Flags
  cWebSocket_FIN_Bit           = $80;
  cWebSocket_Frame_Close       = AnsiString(#$88#$00);

  // 🔹 WebSocket Length Markers
  cWebSocket_Len125            = Byte(125);
  cWebSocket_LenGreaterThan125= Byte(126);
  cWebSocket_LenGreaterThanWord= Byte(127);

  // 🔹 Timeouts and Session Management (ms)
  cWebSocket_Timeout_ReadByte     = 500;
  cWebSocket_Timeout_ReadPayload  = 2000;
  cWebSocket_PingInterval         = QWord(60000);
  cWebSocket_SessionTimeout       = QWord(180000);
  cWebSocket_SleepInterval        = 50;

  // 🔹 Standard HTTP Status Codes
  http_Ok                     = 200;
  http_Created                = 201;
  http_NoContent              = 204;
  http_PartialContent         = 206;
  http_BadRequest             = 400;
  http_Unauthorized           = 401;
  http_Forbidden              = 403;
  http_NotFound               = 404;
  http_MethodNotAllowed       = 405;
  http_RequestTimeout         = 408;
  http_RangeNotSatisfiable    = 416;
  http_InternalServerError    = 500;
  http_NotImplemented         = 501;
  http_ServiceUnavailable     = 503;


  // 🔹 MIME Types by Extension
  cHTTP_MimeType_HTML     = 'text/html';
  cHTTP_MimeType_CSS      = 'text/css';
  cHTTP_MimeType_JS       = 'application/javascript';
  cHTTP_MimeType_JSON     = 'application/json';
  cHTTP_MimeType_XML      = 'application/xml';
  cHTTP_MimeType_TXT      = 'text/plain';

  cHTTP_MimeType_JPEG     = 'image/jpeg';
  cHTTP_MimeType_PNG      = 'image/png';
  cHTTP_MimeType_GIF      = 'image/gif';
  cHTTP_MimeType_SVG      = 'image/svg+xml';
  cHTTP_MimeType_WEBP     = 'image/webp';
  cHTTP_MimeType_ICO      = 'image/x-icon';

  cHTTP_MimeType_PDF      = 'application/pdf';

  cHTTP_MimeType_MP4      = 'video/mp4';
  cHTTP_MimeType_WEBM     = 'video/webm';
  cHTTP_MimeType_MOV      = 'video/mov';
  cHTTP_MimeType_M3U8     = 'application/vnd.apple.mpegurl';
  cHTTP_MimeType_TS       = 'video/MP2T';

  cHTTP_MimeType_WOFF     = 'font/woff';
  cHTTP_MimeType_WOFF2    = 'font/woff2';

  // 🔹 Extensions (lowercase preferred)
  cHTTP_Ext_HTML          = '.html';
  cHTTP_Ext_CSS           = '.css';
  cHTTP_Ext_JS            = '.js';
  cHTTP_Ext_JSON          = '.json';
  cHTTP_Ext_XML           = '.xml';
  cHTTP_Ext_TXT           = '.txt';

  cHTTP_Ext_JPG           = '.jpg';
  cHTTP_Ext_JPEG          = '.jpeg';
  cHTTP_Ext_PNG           = '.png';
  cHTTP_Ext_GIF           = '.gif';
  cHTTP_Ext_SVG           = '.svg';
  cHTTP_Ext_WEBP          = '.webp';
  cHTTP_Ext_ICO           = '.ico';

  cHTTP_Ext_PDF           = '.pdf';

  cHTTP_Ext_MP4           = '.mp4';
  cHTTP_Ext_WEBM          = '.webm';
  cHTTP_Ext_MOV           = '.mov';
  cHTTP_Ext_M3U8          = '.m3u8';
  cHTTP_Ext_TS            = '.ts';

  cHTTP_Ext_WOFF          = '.woff';
  cHTTP_Ext_WOFF2         = '.woff2';


implementation

end.

