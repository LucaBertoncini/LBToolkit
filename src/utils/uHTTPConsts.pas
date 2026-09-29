unit uHTTPConsts;

{$mode objfpc}{$H+}

interface

const
  // 🔹 Standard HTTP Headers
  HTTP_HEADER_CONTENT_TYPE                 = 'Content-Type';
  HTTP_HEADER_CONTENT_LENGTH               = 'Content-Length';
  HTTP_HEADER_CONTENT_DISPOSITION          = 'Content-Disposition';
  HTTP_HEADER_CONNECTION                   = 'Connection';
  HTTP_HEADER_UPGRADE                      = 'Upgrade';
  HTTP_HEADER_HOST                         = 'Host';
  HTTP_HEADER_RANGE                        = 'Range';
  HTTP_HEADER_ACCEPT                       = 'Accept';
  HTTP_HEADER_CONTENT_RANGE                = 'Content-Range';
  HTTP_HEADER_ACCEPT_RANGES                = 'Accept-Ranges';
  HTTP_HEADER_DATE                         = 'Date';
  HTTP_HEADER_SERVER                       = 'Server';
  HTTP_HEADER_ACCESS_CONTROL_ALLOW_ORIGIN  = 'Access-Control-Allow-Origin';
  HTTP_HEADER_ACCESS_CONTROL_ALLOW_METHODS = 'Access-Control-Allow-Methods';
  HTTP_HEADER_ACCESS_CONTROL_ALLOW_HEADERS = 'Access-Control-Allow-Headers';
  HTTP_HEADER_SET_COOKIE                   = 'Set-Cookie';
  HTTP_HEADER_COOKIE                       = 'Cookie';

  // 🔹 HTTP Methods
  HTTP_METHOD_GET                = 'GET';
  HTTP_METHOD_POST               = 'POST';
  HTTP_METHOD_HEAD               = 'HEAD';
  HTTP_METHOD_PUT                = 'PUT';
  HTTP_METHOD_DELETE             = 'DELETE';
  HTTP_METHOD_PATCH              = 'PATCH';
  HTTP_METHOD_OPTIONS            = 'OPTIONS';

  // 🔹 Common HTTP Header Values
  HTTP_CONNECTION_CLOSE          = 'close';
  HTTP_CONNECTION_UPGRADE        = 'Upgrade';
  HTTP_CONNECTION_UPGRADE_LOWER  = 'upgrade';
  HTTP_CONNECTION_KEEP_ALIVE     = 'keep-alive';
  HTTP_UPGRADE_WEBSOCKET         = 'websocket';

  // 🔹 Standard MIME Types
  MIME_TYPE_JSON                 = 'application/json';
  MIME_TYPE_HTML                 = 'text/html';
  MIME_TYPE_PLAIN_TEXT           = 'text/plain';
  MIME_TYPE_OCTET_STREAM         = 'application/octet-stream';
  MIME_TYPE_CSS                  = 'text/css';
  MIME_TYPE_JAVASCRIPT           = 'application/javascript';
  MIME_TYPE_XML                  = 'application/xml';
  MIME_TYPE_MULTIPART            = 'multipart/form-data';

  // 🔹 Image MIME Types
  MIME_TYPE_JPEG                 = 'image/jpeg';
  MIME_TYPE_PNG                  = 'image/png';
  MIME_TYPE_GIF                  = 'image/gif';
  MIME_TYPE_SVG                  = 'image/svg+xml';
  MIME_TYPE_WEBP                 = 'image/webp';
  MIME_TYPE_ICO                  = 'image/x-icon';

  // 🔹 Document MIME Types
  MIME_TYPE_PDF                  = 'application/pdf';

  // 🔹 Video MIME Types
  MIME_TYPE_MP4                  = 'video/mp4';
  MIME_TYPE_WEBM                 = 'video/webm';
  MIME_TYPE_MOV                  = 'video/mov';
  MIME_TYPE_M3U8                 = 'application/vnd.apple.mpegurl';
  MIME_TYPE_TS                   = 'video/MP2T';

  // 🔹 Font MIME Types
  MIME_TYPE_WOFF                 = 'font/woff';
  MIME_TYPE_WOFF2                = 'font/woff2';

  // 🔹 File Extensions
  FILE_EXT_HTML                  = '.html';
  FILE_EXT_CSS                   = '.css';
  FILE_EXT_JS                    = '.js';
  FILE_EXT_JSON                  = '.json';
  FILE_EXT_XML                   = '.xml';
  FILE_EXT_TXT                   = '.txt';
  FILE_EXT_JPG                   = '.jpg';
  FILE_EXT_JPEG                  = '.jpeg';
  FILE_EXT_PNG                   = '.png';
  FILE_EXT_GIF                   = '.gif';
  FILE_EXT_SVG                   = '.svg';
  FILE_EXT_WEBP                  = '.webp';
  FILE_EXT_ICO                   = '.ico';
  FILE_EXT_PDF                   = '.pdf';
  FILE_EXT_MP4                   = '.mp4';
  FILE_EXT_WEBM                  = '.webm';
  FILE_EXT_MOV                   = '.mov';
  FILE_EXT_M3U8                  = '.m3u8';
  FILE_EXT_TS                    = '.ts';
  FILE_EXT_WOFF                  = '.woff';
  FILE_EXT_WOFF2                 = '.woff2';

  // 🔹 HTTP Status Codes
  HTTP_STATUS_OK                    = 200;
  HTTP_STATUS_CREATED               = 201;
  HTTP_STATUS_NO_CONTENT            = 204;
  HTTP_STATUS_PARTIAL_CONTENT       = 206;
  HTTP_STATUS_BAD_REQUEST           = 400;
  HTTP_STATUS_UNAUTHORIZED          = 401;
  HTTP_STATUS_FORBIDDEN             = 403;
  HTTP_STATUS_NOT_FOUND             = 404;
  HTTP_STATUS_METHOD_NOT_ALLOWED    = 405;
  HTTP_STATUS_REQUEST_TIMEOUT       = 408;
  HTTP_STATUS_RANGE_NOT_SATISFIABLE = 416;
  HTTP_STATUS_INTERNAL_ERROR        = 500;
  HTTP_STATUS_NOT_IMPLEMENTED       = 501;
  HTTP_STATUS_SERVICE_UNAVAILABLE   = 503;

  // 🔹 WebSocket Specific Headers
  WS_HEADER_SEC_KEY              = 'Sec-WebSocket-Key';
  WS_HEADER_SEC_PROTOCOL         = 'Sec-WebSocket-Protocol';
  WS_HEADER_SEC_ACCEPT           = 'Sec-WebSocket-Accept';
  WS_HEADER_SEC_VERSION          = 'Sec-WebSocket-Version';

  // 🔹 WebSocket Protocol Constants
  WS_GUID                        = '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';
  WS_VERSION                     = '13';

  // 🔹 WebSocket Handshake Response
  WS_HANDSHAKE_STATUS_LINE       = 'HTTP/1.1 101 Switching Protocols';
  WS_HANDSHAKE_UPGRADE_HEADER    = 'Upgrade: websocket';
  WS_HANDSHAKE_CONNECTION_HEADER = 'Connection: Upgrade';

  // 🔹 WebSocket Frame Opcodes
  WS_OPCODE_CONTINUATION         = $00;
  WS_OPCODE_TEXT                 = $01;
  WS_OPCODE_BINARY               = $02;
  WS_OPCODE_CLOSE                = $08;
  WS_OPCODE_PING                 = $09;
  WS_OPCODE_PONG                 = $0A;

  // 🔹 WebSocket Frame Flags
  WS_FLAG_FIN                    = $80;
  WS_FLAG_MASK                   = $80;

  // 🔹 WebSocket Length Markers
  WS_LEN_STANDARD_MAX            = 125;
  WS_LEN_EXTENDED_16BIT          = 126;
  WS_LEN_EXTENDED_64BIT          = 127;

  // 🔹 WebSocket Masked Length Markers
  WS_LEN_MASKED_EXTENDED_16BIT   = 254;  // 126 | 128
  WS_LEN_MASKED_EXTENDED_64BIT   = 255;  // 127 | 128

  // 🔹 WebSocket Close Frame
  WS_FRAME_CLOSE                 = AnsiString(#$88#$00);

  // 🔹 WebSocket Timeouts (milliseconds)
  WS_TIMEOUT_READ_BYTE           = 500;
  WS_TIMEOUT_READ_PAYLOAD        = 2000;
  WS_TIMEOUT_HANDSHAKE           = 10000;
  WS_TIMEOUT_HEARTBEAT           = 20000;

  // 🔹 WebSocket Intervals (milliseconds)
  WS_PING_INTERVAL_DEFAULT       = QWord(30000);   // 30 seconds
  WS_SESSION_TIMEOUT_DEFAULT     = QWord(180000);  // 3 minutes
  WS_SLEEP_INTERVAL              = 50;

implementation

end.

