# Uploading Files with LBmicroWebServer

A guide for those who configure the server and for developers building clients that upload files (videos or other multimedia content) through LBmicroWebServer and the DMA application.

## Contents

1. [Overview](#1-overview)
2. [Server configuration](#2-server-configuration)
3. [Authentication](#3-authentication)
4. [Record data (`X-JSON` header)](#4-record-data-x-json-header)
5. [Mode A: single-request upload](#5-mode-a-single-request-upload)
6. [Mode B: chunked upload](#6-mode-b-chunked-upload)
7. [Choosing a mode](#7-choosing-a-mode)
8. [Responses and errors](#8-responses-and-errors)
9. [Server behaviour after the upload](#9-server-behaviour-after-the-upload)
10. [Timeouts and limits](#10-timeouts-and-limits)
11. [Diagnostics](#11-diagnostics)
12. [Protocol summary](#12-protocol-summary)

---

## 1. Overview

LBmicroWebServer receives files as the **raw binary body** of a `POST` request sent to the *upload endpoint*. The file is not encoded (no `multipart/form-data`, no base64 in the body): the bytes of the file are the request body. Descriptive data (title, categories, etc.) travel in the `X-JSON` header.

Two modes are available. The client chooses one depending on the file size and the reliability of the network:

| Mode | Description | Suited for |
|---|---|---|
| **A – single request** | The whole file is the body of one `POST`. | Small and medium files, stable networks, simple clients. |
| **B – chunked** | The file is sent as sequential chunks, each in its own `POST`. An interrupted upload can resume from the point reached. | Large files (GB), Internet connections, intermittent networks. |

Both modes use the same endpoint, the same authentication and the same record data format. The server recognises mode B by the presence of the `upl` object in the `X-JSON` header (see [§6](#6-mode-b-chunked-upload)).

```
Client                                         LBmicroWebServer
  │  POST /UploadVideo                              │
  │  Cookie: token=…                                │
  │  X-JSON: <record, base64>                       │
  │  Content-Length: N                              │
  │  <file or chunk bytes>  ──────────────────────► │ writes to <DocumentsFolder>/tmpUpload
  │                                                 │ validates token and permissions
  │  ◄─────────────────────────────────────────────  │ stores the file and the record
  │  200 OK  {"Success":true, …}                    │
```

## 2. Server configuration

### 2.1 Upload endpoint

In the configuration file, section `[LBmicroWebServer]`:

```ini
[LBmicroWebServer]
Port=10322
DocumentsFolder=./html
UploadEndpoint=/UploadVideo
```

| Key | Meaning |
|---|---|
| `Port` | Listening port. |
| `DocumentsFolder` | Root folder of the documents served by the server. It must exist. |
| `UploadEndpoint` | Path on which the server accepts uploads. Without this key uploads are disabled. |

The server treats as an upload every `POST` whose URI **starts with** `UploadEndpoint`. For the upload requests of the DMA application the URI must be exactly `/UploadVideo`, with no query string.

### 2.2 Working folders

| Folder | Purpose |
|---|---|
| `<DocumentsFolder>/tmpUpload/` | Area where the server writes bytes as they arrive (the whole file in mode A, a single chunk in mode B). It is not reachable over HTTP: every GET request to this path is refused. |
| `[Mediafiles] Folder_incoming` | Area for the **partial** files of chunked uploads (default `./Incoming`). |
| `[Mediafiles] Folder_upload` | Final destination of uploaded files, organised as `Multimedia/yyyy/mm/dd/`. |

```ini
[Mediafiles]
Folder=./Mediafiles
Folder_incoming=./Incoming
```

Operational notes:

- `Folder_incoming` must be **on the same volume** as `Folder_upload`: on completion the file is *renamed* into its final destination, which is instantaneous only within a single volume. Across volumes it would amount to a copy of the entire file.
- `Folder_incoming` must not be a folder watched by the video processing program, so that a file that is still incomplete is not picked up.
- The free disk space must be able to hold the incoming file plus one chunk: the server does not check the available space beforehand.

### 2.3 HTTPS

HTTPS is recommended for uploads over the Internet (section `[SSLData]` with certificate and private key). Authentication relies on a session cookie and the record data travel in a header: without TLS both are exposed.

## 3. Authentication

Every upload request requires a valid session. The session is identified by the `token` cookie, issued by the server at login.

### 3.1 Login

`POST /` with a JSON body:

```http
POST / HTTP/1.1
Content-Type: application/json

{"Req":"DMALin","usrName":"<user>","psw":"<password>"}
```

The response contains the user data (`"Success":true`) and a `Set-Cookie` header with the session token:

```
Set-Cookie: token=<value>; Path=/; HttpOnly; SameSite=Strict; Max-Age=3600;
```

The client must send the same cookie in every subsequent request, upload chunks included:

```
Cookie: token=<value>
```

### 3.2 Session lifetime

- The session expires after a period of **inactivity**: every authenticated request renews it. The default is 1 hour (`TokenExpirationTime`, in seconds, in the `[TokenManager]` section). The `X-Session-Expires-In` response header reports the remaining seconds.
- The token is checked once the server has **fully received** the upload request. In mode A, an upload lasting longer than the allowed inactivity interval ends with an expired session and the file is discarded (see [§7](#7-choosing-a-mode)). In mode B every chunk is an authenticated request and renews the session.

### 3.3 Permissions

| User type | Upload |
|---|---|
| Administrator | Allowed; may modify all records. |
| Standard user | Allowed; may modify only their own records that are not validated. |
| Read-only user | Not allowed. |

Replacing the file of a **validated** record is not allowed for anyone, administrators included.

## 4. Record data (`X-JSON` header)

Information about the content is sent in the `X-JSON` header as a JSON object encoded in **UTF-8** and then in **standard base64** (alphabet `A–Z a–z 0–9 + /`, with `=` padding). Base64 guarantees that the header contains ASCII characters only.

Example of the object before encoding:

```json
{
  "title": "Spring concert",
  "description": "Full recording of the concert.",
  "date": "2024-04-12",
  "IdCategories": [3, 7],
  "idProvider": 0,
  "Provider": "Choral Association"
}
```

| Field | Type | Required | Description |
|---|---|---|---|
| `title` | string | yes | Title. |
| `description` | string | yes | Description. |
| `date` | string | yes | Production date (free text, e.g. `2024`, `June 2024`, `2024-04-12`). |
| `IdCategories` | array of integers | yes for a new record | Category identifiers. |
| `idProvider` | integer | no | Id of an existing provider; `0` to indicate a new one. |
| `Provider` | string | no | Provider description, used when `idProvider` is `0`. If a provider with the same name already exists it is reused. |
| `sign` | string | no | "Verification" field: considered for administrators only. |
| `mId` | integer | no | Id of an existing record. When present, the uploaded file **replaces** the record's file. |
| `upl` | object | mode B only | Chunked upload descriptor (see [§6](#6-mode-b-chunked-upload)). |

Notes:

- The headers of a request as a whole cannot exceed the maximum size handled by the server (16 KB by default). Since base64 increases the size by about one third, the record JSON must stay within a few KB, also taking into account the cookie and the other headers.
- The file name comes from the `Content-Disposition` header (`filename="…"`) or, in mode B, from the `upl.name` field. File names made of ASCII characters are recommended: the server removes from the name any character that is unsafe for the file system.

## 5. Mode A: single-request upload

The file is the body of one request.

### 5.1 Request

```http
POST /UploadVideo HTTP/1.1
Host: archive.example.org
Cookie: token=<value>
Content-Type: application/octet-stream
Content-Disposition: attachment; filename="concert.mp4"
Content-Length: 1073741824
X-JSON: eyJ0aXRsZSI6IlNwcmluZyBjb25jZXJ0Iiw…

<file bytes>
```

Requirements:

- `Content-Length` is mandatory and must equal the exact size of the file. The server does not support `Transfer-Encoding: chunked`.
- `Content-Disposition` with the `filename` parameter.
- `X-JSON` without the `upl` object.

### 5.2 Response

```json
{"_type":"SaveMedia","Success":true,"mId":1234}
```

`mId` is the identifier of the record that was created or updated. On error: `{"_type":"SaveMedia","Success":false,"ErrMsg":"…"}` (see [§8](#8-responses-and-errors)).

### 5.3 curl example

```bash
TOKEN="…"   # value of the cookie obtained at login
META='{"title":"Spring concert","description":"Full recording.","date":"2024-04-12","IdCategories":[3]}'

curl -X POST "https://archive.example.org:10322/UploadVideo" \
  -H "Cookie: token=$TOKEN" \
  -H "Content-Type: application/octet-stream" \
  -H 'Content-Disposition: attachment; filename="concert.mp4"' \
  -H "X-JSON: $(printf '%s' "$META" | base64 -w0)" \
  --data-binary @concert.mp4
```

(On macOS use `base64` without `-w0`, removing any line breaks.)

## 6. Mode B: chunked upload

The file is split into consecutive chunks. Each chunk is sent with its own `POST` to the same endpoint. The server appends the chunks to a partial file and, when it receives the last one, completes the operation as in mode A.

### 6.1 Concepts

| Term | Meaning |
|---|---|
| **Upload id** | Identifier chosen by the client, identical for all chunks of the same file. 8 to 64 characters among `0-9`, `a-z`, `A-Z` and `-`. It must be unique per user (a UUID or 16 random bytes in hexadecimal is recommended). |
| **Offset** | Position of the first byte of the chunk within the file, starting from 0. |
| **Received** | Number of bytes the server has received and kept for that upload. It is the only source of truth about progress. |

### 6.2 Chunk request

```http
POST /UploadVideo HTTP/1.1
Host: archive.example.org
Cookie: token=<value>
Content-Type: application/octet-stream
Content-Disposition: attachment; filename="concert.mp4"
Content-Length: 16777216
X-JSON: <record + upl, base64>

<chunk bytes>
```

The object encoded in `X-JSON` contains the same fields as the record ([§4](#4-record-data-x-json-header)) plus the `upl` object:

```json
{
  "title": "Spring concert",
  "description": "Full recording.",
  "date": "2024-04-12",
  "IdCategories": [3],
  "upl": {
    "id": "9f2c41d07b8e4a63a1c5d0e7f3b2a498",
    "offset": 33554432,
    "size": 1073741824,
    "name": "concert.mp4"
  }
}
```

| `upl` field | Description |
|---|---|
| `id` | Upload id. |
| `offset` | Position of the chunk in the file. Must be less than `size`. |
| `size` | **Total** size of the file in bytes (constant across all chunks, greater than 0). |
| `name` | File name. |

The body contains the bytes from `offset` to `offset + Content-Length − 1`. **16 MiB** chunks are recommended; the client is free to choose the size (smaller chunks for very unstable networks).

The record (`title`, `description`, …) must be repeated in every request, because each chunk is processed independently; it is used when the last chunk is received.

### 6.3 Chunk response

```json
{"_type":"UploadChunk","Success":true,"received":50331648}
```

| Field | Description |
|---|---|
| `Success` | `true` if the chunk was accepted. |
| `received` | Bytes kept by the server for this upload. |
| `done` | Present and `true` only in the response to the last chunk, when the file has been stored and the record registered. In this case the response also includes the fields of the save operation (`mId`). |
| `resync` | `true` when the chunk is not accepted because the offset does not match the server's state. The client must restart from `received`. |
| `ErrMsg` | Error description when `Success` is `false`. |

Response to the last chunk:

```json
{"_type":"SaveMedia","Success":true,"mId":1234,"done":true,"received":1073741824}
```

### 6.4 Client algorithm

```
id       ← random identifier
offset   ← 0
attempts ← 0

while offset < size:
    chunk    ← bytes [offset, min(offset + chunkSize, size))
    response ← POST(chunk, upl = {id, offset, size, name})

    if network error, timeout or non-JSON response:
        attempts ← attempts + 1
        if attempts > maximum → stop (optionally send CancelUpload)
        wait (e.g. 2·attempts seconds, max 30) and repeat the same chunk

    else if response.Success:
        if response.done → upload finished (response.mId = record id)
        else offset ← response.received ; attempts ← 0

    else if response.resync:
        offset ← response.received                       (limit the number of resyncs)

    else:
        final error: show response.ErrMsg and stop
```

Key points:

- **Sequential.** Chunks must be sent one at a time, in order.
- **Safe repetition.** A chunk may be sent more than once (for example because the response was lost): if the server already has those bytes it does not duplicate them; if it has received them in part, it appends only the missing portion. In both cases it answers with the updated value of `received`.
- **Resuming.** To resume an interrupted upload when the id is known, it is enough to resend a chunk starting from the last known offset (even from 0): the response indicates where to continue.
- **Repeated completion.** If the response to the last chunk is lost and the client resends it, the server answers with the same outcome already recorded, without creating a second record.
- **Offset ahead of the server's state.** If `offset` is greater than `received`, the server answers `Success:false` with `resync:true` and `received`.
- **Final errors.** A response with `Success:false` and no `resync` (permissions, validated record, invalid request, database error) must not be repeated.

### 6.5 Cancelling

To stop an upload and immediately free the space taken by the partial file:

```http
POST / HTTP/1.1
Cookie: token=<value>
Content-Type: application/json

{"Req":"CancelUpload","id":"9f2c41d07b8e4a63a1c5d0e7f3b2a498"}
```

Response: `{"_type":"CancelUpload","Success":true}`. An upload abandoned without cancelling is deleted by the server after 72 hours anyway.

### 6.6 JavaScript example (browser)

```javascript
const CHUNK_SIZE = 16 * 1024 * 1024;
const MAX_RETRIES = 12;

const toBase64 = (obj) => btoa(unescape(encodeURIComponent(JSON.stringify(obj))));
const wait = (ms) => new Promise((r) => setTimeout(r, ms));

async function sendChunk(url, file, meta, id, offset) {
  const end = Math.min(offset + CHUNK_SIZE, file.size);
  const info = { ...meta, upl: { id, offset, size: file.size, name: file.name } };

  const response = await fetch(url, {
    method: 'POST',
    credentials: 'include',                       // sends the session cookie
    headers: {
      'Content-Type': 'application/octet-stream',
      'Content-Disposition': `attachment; filename="${file.name}"`,
      'X-JSON': toBase64(info),
    },
    body: file.slice(offset, end),
  });
  if (!response.ok) throw new Error(`HTTP ${response.status}`);
  return response.json();
}

async function uploadFile(url, file, meta, onProgress) {
  const id = crypto.randomUUID().replace(/-/g, '');
  let offset = 0;
  let retries = 0;
  let resyncs = 0;

  while (offset < file.size) {
    let answer;
    try {
      answer = await sendChunk(url, file, meta, id, offset);
    } catch (err) {
      if (++retries > MAX_RETRIES) throw err;
      await wait(Math.min(30, retries * 2) * 1000);
      continue;                                    // same chunk, new attempt
    }

    if (answer.Success) {
      if (answer.done) return answer;              // answer.mId = record id
      offset = answer.received;
      retries = 0;
      onProgress?.(offset / file.size);
    } else if (answer.resync && ++resyncs <= 20) {
      offset = answer.received;
    } else {
      throw new Error(answer.ErrMsg || 'Upload failed');
    }
  }
}
```

In Node.js, replace `toBase64` with `Buffer.from(JSON.stringify(obj), 'utf8').toString('base64')` and handle the `token` cookie obtained at login.

## 7. Choosing a mode

| Situation | Recommended mode |
|---|---|
| Files of a few MB or tens of MB, local network | A |
| Large files (hundreds of MB or GB) | B |
| Internet connection, Wi-Fi, mobile networks | B |
| Upload that may last longer than the session inactivity interval | B |
| Minimal client (scripts, command-line tools) on a reliable network | A |

Considerations:

- In mode A the whole transfer is a single connection: an interruption forces a restart from zero. The server waits up to 60 seconds without receiving data before considering the upload failed.
- In mode A the token is verified at the end of reception. With very long uploads the session must not expire in the meantime: increase `TokenExpirationTime`, or have the client perform an authenticated request at regular intervals while sending.
- In mode B every chunk renews the session, and an interruption costs at most the chunk in progress.

## 8. Responses and errors

Application responses are JSON objects with `Success` and, on error, `ErrMsg`. The `ErrMsg` texts are returned by the server in Italian; they are reported below verbatim, with their meaning.

| `ErrMsg` | Cause | Client action |
|---|---|---|
| `Richiesta di upload non valida!` | Missing or inconsistent `upl` data (id, name, size, offset). | Fix the request; do not repeat. |
| `Operazione non consentita!` | The user type cannot upload or modify. | Do not repeat. |
| `Video non trovato!` | `mId` does not match an existing record. | Do not repeat. |
| `Il video e' validato: non e' possibile sostituire il file.` | The record is validated. | Do not repeat. |
| `Blocco fuori sequenza.` | `offset` greater than the bytes already received (together with `resync:true`). | Restart from `received`. |
| `Il blocco supera la dimensione dichiarata del file.` | The chunk exceeds `size`. | Fix `size` or the chunk length. |
| `Errore nella scrittura del file sul server.` | File system error (e.g. disk full). | Retry later; notify the administrator. |
| `Impossibile completare il file sul server.` | Error while completing the upload. | Retry later; notify the administrator. |
| `Error saving data into DB!` | Error while saving the record. | Notify the administrator. |

HTTP-level conditions:

- **Missing or expired session:** the request produces no application response (non-JSON body, status other than 200) and the received file is deleted. The client must log in again; in mode B it can resume with the same id because the chunks already received are kept.
- **Missing or zero `Content-Length`:** the request is refused.
- **Connection interrupted during reception:** the server discards what it received in mode A; in mode B the chunks already completed remain available for resuming.

## 9. Server behaviour after the upload

When the file is complete (mode A) or the last chunk has been received (mode B), the server:

1. verifies the session, the user type and, for a replacement, that the record exists, belongs to the user (or the user is an administrator) and is not validated;
2. moves the file into `Folder_upload/Multimedia/yyyy/mm/dd/`. If a file with the same name already exists in the folder, the new one receives a progressive suffix (`name_1.mp4`, `name_2.mp4`, …): no existing file is ever overwritten;
3. registers or updates the record in the database. For a new record, the record is validated automatically if the user is an administrator or has auto-validation enabled;
4. for a **replacement**, resets the record's duration (`Duration = 0`) and, only after the database has been updated, deletes the old original file and its processed copies. The processing program finds the new file through the path stored in the database and regenerates the copies.

If an error occurs after the transfer (permissions, database) the received file is deleted and no orphan files remain.

## 10. Timeouts and limits

| Parameter | Value |
|---|---|
| Maximum wait without receiving bytes during an upload | 60 seconds |
| Socket read/write timeout | 10 seconds |
| Maximum size of a request's headers | 16 KB (default) |
| Maximum file size | No application limit; disk space applies |
| Recommended chunk size (mode B) | 16 MiB |
| Retention of an incomplete chunked upload | 72 hours |
| Default session lifetime on inactivity | 1 hour (`TokenExpirationTime`) |

The 60-second timeout refers to periods with **no bytes arriving at all**: as long as the client keeps transmitting, an upload can last a long time. Any reverse proxy or firewall in front of the server must allow the required body (no overly low size limit, adequate read timeouts) and must not alter requests.

## 11. Diagnostics

The server's messages about uploads appear in the log file (`[LBLogger]`). The most useful ones:

| Message | Meaning |
|---|---|
| `Receiving raw file upload to <…>` | Start of reception of a file or a chunk. |
| `File upload completed: N bytes written` | Reception completed. |
| `Connection closed by the client. Remaining bytes: …` | The client closed the connection before the end; the log reports the missing bytes, the last socket error and the elapsed time. |
| `No data received for 60 s. Remaining bytes: …` | No data received for 60 seconds. |
| `File upload incomplete. Expected …, received …` | Incomplete transfer: the partial file is discarded (mode A) or the chunk is not appended (mode B). |
| `Access to the upload temporary folder denied` | GET request to `tmpUpload`: refused by design. |

Quick checks when something goes wrong:

1. *The upload does not start:* check `UploadEndpoint`, the exact request URI and the presence of the `token` cookie.
2. *Uploads interrupted halfway over the Internet:* use mode B; check the timeouts and limits of any reverse proxy.
3. *Error on completion:* check the available space, the write permissions on `Folder_incoming` and `Folder_upload`, and that they are on the same volume.
4. *Session expired at the end of the upload (mode A):* switch to mode B or increase `TokenExpirationTime`.

## 12. Protocol summary

**Upload endpoint:** `POST <UploadEndpoint>` (DMA: `/UploadVideo`)

| Header | Mode A | Mode B |
|---|---|---|
| `Cookie: token=…` | required | required |
| `Content-Type: application/octet-stream` | yes | yes |
| `Content-Disposition: attachment; filename="…"` | yes | yes |
| `Content-Length` | file size | chunk size |
| `X-JSON` | record (base64 of UTF-8 JSON) | record + `upl` (base64 of UTF-8 JSON) |

**`upl` object (mode B):** `id`, `offset`, `size`, `name`.

**Chunk response:** `Success`, `received`, `done`, `resync`, `ErrMsg`.

**Cancel:** `POST /` with `{"Req":"CancelUpload","id":"…"}`.

**Login:** `POST /` with `{"Req":"DMALin","usrName":"…","psw":"…"}`; the token is in the `token` cookie.
