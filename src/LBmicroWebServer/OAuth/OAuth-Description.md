# OAuth explained simply

This document explains the logic of OAuth 2.1 as implemented in `uLBOAuthProvider.pas`
(the LBmicroWebServer AI connector), with examples of what the client sends and what the server answers.
The examples come from the automated test; long values are shortened.

---

## 1. The problem

Claude.ai (or another AI) wants to use your application **on behalf of a user**, for example Mario.

Wrong solutions:

- Give Claude Mario's password. Claude would store it, and Mario could not revoke the access without
  changing his password.
- Give Claude a "system" password. You would no longer know who does what, and the permissions would be wrong.

OAuth solves it like this: **Mario types his password only on your server**. Claude never sees it.
In the end Claude receives a **token** that is valid for Mario, expires soon and can be revoked.

> Analogy: the hotel key card. At the reception (your server) you show your ID (the password) and receive
> a card. The card opens only your room, and only for the length of your stay. The ID stays in your pocket.

### In short: what OAuth is for

OAuth lets **an application act on your behalf without knowing your credentials**. It separates two things:

- **who you are**: verified only by your server (username and password on the login page);
- **who acts for you**: another application (Claude), with a permission that is **limited**, **temporary** and **revocable**.

Compared with a shared password it gives four advantages:

| Advantage | How it is achieved here |
|---|---|
| Limited | the token only allows what the user's role allows, and only for the `mcp` scope |
| Temporary | access token 15 minutes, refresh token 30 days |
| Revocable | Claude's access can be removed without changing the password |
| Traceable | the server knows which application (`client_id`) acts on the user's behalf |

**Warning:** the client does not know the password, but **the token is still a key**. Whoever holds it
acts as the user while it is valid. That is why tokens are short-lived, the refresh token is single-use
and the server must be exposed over HTTPS.

## 2. The actors

| Actor | Who | In our case |
|---|---|---|
| User | the owner of the data | Mario, with his username and password |
| Client | the application that wants access | Claude.ai |
| Authorization server | verifies Mario and issues tokens | your LBmicroWebServer (`/oauth/...`) |
| Protected resource | what the token gives access to | the `/mcp` endpoint |

In our case the authorization server and the protected resource are the same program.

## 3. The two tokens

- **Access token**: the card shown on every call (`Authorization: Bearer ...`). It lasts a short time
  (15 minutes), so if it is stolen the damage is limited.
- **Refresh token**: only used to obtain a new access token without asking for the password again.
  It lasts 30 days and is used **only once**: every renewal returns a new one.

Mario types his password once; afterwards Claude renews itself for weeks.

## 4. The three security ideas

1. **The password only goes through your server.** Mario's browser opens your login page.
2. **The temporary code.** After the login the server does not hand over the tokens. It hands over a *code*
   valid for 5 minutes and usable once. The client exchanges it for the tokens in a direct call to the
   server, so tokens never appear in the browser's address bar.
3. **PKCE.** The client invents a random secret (`code_verifier`) and at the beginning sends only its
   fingerprint (`code_challenge`, a SHA-256 hash). To exchange the code it must show the original secret.
   If someone intercepts the code, without the secret they cannot use it. PKCE is mandatory on this server.

## 5. The flow step by step

```
Claude                    Mario's browser               Your server
  |  POST /mcp (no token) ---------------------------------->|
  |<------------------------------ 401 + where to authenticate|   (A)
  |  GET /.well-known/... ----------------------------------->|
  |<-------------------------------- server addresses        |   (B)
  |  POST /oauth/register ----------------------------------->|
  |<-------------------------------- client_id               |   (C)
  |  opens the browser -->|  GET /oauth/authorize?... ------->|
  |                       |<---------------- login page       |   (D)
  |                       |  POST username+password -------->|
  |                       |<---- 302 to Claude with code      |   (E)
  |  receives the code <--|                                   |
  |  POST /oauth/token (code + code_verifier) --------------->|
  |<------------------------------ access + refresh token    |   (F)
  |  POST /mcp with Authorization: Bearer ... --------------->|
  |<-------------------------------- response                 |   (G)
  |  ... after 15 minutes: POST /oauth/token (refresh) ------>|
  |<------------------------------ new tokens                |   (H)
```

### (A) Claude tries without a token

Request:

```http
POST /mcp HTTP/1.1
Content-Type: application/json

{"jsonrpc":"2.0","id":1,"method":"initialize", ...}
```

Response: the server says "a token is required" and tells where to find the instructions.

```http
HTTP/1.1 401 Unauthorized
WWW-Authenticate: Bearer resource_metadata="https://app.example.com/.well-known/oauth-protected-resource"
```

### (B) Claude discovers the addresses

```http
GET /.well-known/oauth-protected-resource HTTP/1.1
```

```json
{
  "resource": "https://app.example.com/mcp",
  "authorization_servers": ["https://app.example.com"],
  "bearer_methods_supported": ["header"],
  "scopes_supported": ["mcp"]
}
```

Then Claude reads `/.well-known/oauth-authorization-server`:

```json
{
  "issuer": "https://app.example.com",
  "authorization_endpoint": "https://app.example.com/oauth/authorize",
  "token_endpoint": "https://app.example.com/oauth/token",
  "registration_endpoint": "https://app.example.com/oauth/register",
  "response_types_supported": ["code"],
  "grant_types_supported": ["authorization_code", "refresh_token"],
  "code_challenge_methods_supported": ["S256"],
  "token_endpoint_auth_methods_supported": ["none"]
}
```

It is an "address book" that lets Claude know no address in advance except `/mcp`.

### (C) Claude registers

The server does not know Claude: it must introduce itself and say where it wants to be called back.

```http
POST /oauth/register HTTP/1.1
Content-Type: application/json

{"client_name":"Claude","redirect_uris":["https://claude.ai/api/mcp/auth_callback"]}
```

```http
HTTP/1.1 201 Created

{"client_id":"mcp-3VWcb9UdTjm6...","client_name":"Claude",
 "redirect_uris":["https://claude.ai/api/mcp/auth_callback"],
 "grant_types":["authorization_code","refresh_token"],
 "token_endpoint_auth_method":"none"}
```

The server only accepts authorized return addresses (`AllowedRedirectURIs`). If Claude gives
`https://evil.example/cb` it gets `400 invalid_redirect_uri`. This is the protection that prevents the code
from being hijacked to a foreign site.

### (D) Mario sees the login page

Claude prepares the PKCE secret and opens in Mario's browser:

```
GET /oauth/authorize
    ?response_type=code
    &client_id=mcp-3VWcb9UdTjm6...
    &redirect_uri=https://claude.ai/api/mcp/auth_callback
    &code_challenge=E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM
    &code_challenge_method=S256
    &state=xyz123
```

- `code_challenge` is the fingerprint of the secret that Claude keeps for itself.
- `state` is a value from Claude that the server returns unchanged: it lets Claude recognize that the
  answer belongs to its own request.

The server answers with an HTML page with username and password. The page also contains a hidden field
`req` that identifies this authorization request.

### (E) Mario logs in

The browser sends:

```http
POST /oauth/authorize HTTP/1.1
Content-Type: application/x-www-form-urlencoded

req=P0XWjkZ0h7Hl-90...&username=mario&password=********
```

- If the password is wrong: `401` and the page is shown again. After 5 failures the server answers
  `429 Too Many Requests` with `Retry-After: 600` (blocked for 10 minutes).
- If it is correct, the server does **not** show the tokens. It redirects the browser to Claude with the code:

```http
HTTP/1.1 302 Found
Location: https://claude.ai/api/mcp/auth_callback?code=PZje4bViKcSx...&state=xyz123
```

### (F) Claude exchanges the code for the tokens

This call goes directly from Claude to the server, not through the browser.

```http
POST /oauth/token HTTP/1.1
Content-Type: application/x-www-form-urlencoded

grant_type=authorization_code
&code=PZje4bViKcSx...
&redirect_uri=https://claude.ai/api/mcp/auth_callback
&client_id=mcp-3VWcb9UdTjm6...
&code_verifier=the-original-secret
```

The server checks that:

- the code exists, has not expired (5 minutes) and has not been used already;
- `client_id` and `redirect_uri` are the same as in the initial request;
- `SHA-256(code_verifier)` matches the `code_challenge` received at the beginning.

If everything matches:

```http
HTTP/1.1 200 OK
Cache-Control: no-store

{"access_token":"…","token_type":"Bearer","expires_in":900,
 "refresh_token":"…","scope":"mcp"}
```

If something does not match, for example the wrong secret:

```json
{"error":"invalid_grant","error_description":"The code does not match the request"}
```

In that case the code is burnt anyway: a second attempt with the same code fails.

### (G) Claude uses the access token

```http
POST /mcp HTTP/1.1
Authorization: Bearer <access_token>
Content-Type: application/json

{"jsonrpc":"2.0","id":2,"method":"tools/list"}
```

The server looks the token up in the TokenManager, finds that it belongs to Mario and applies **Mario's
role**. Claude sees and runs only what Mario could do in the application.

### (H) Renewal without a password

After 15 minutes the access token expires and `/mcp` answers `401`. Claude does not bother Mario:

```http
POST /oauth/token HTTP/1.1
Content-Type: application/x-www-form-urlencoded

grant_type=refresh_token&refresh_token=<old>&client_id=mcp-3VWcb9UdTjm6...
```

```json
{"access_token":"…new…","token_type":"Bearer","expires_in":900,
 "refresh_token":"…new…","scope":"mcp"}
```

The old refresh token is now **unusable**. If someone tries to reuse it:

```json
{"error":"invalid_grant","error_description":"The refresh token is invalid or expired"}
```

This is refresh token rotation: a stolen token stops working as soon as the legitimate owner renews,
or vice versa.

## 6. Error summary

| Situation | Response |
|---|---|
| No token on `/mcp` | `401` + `WWW-Authenticate` with the metadata address |
| Expired or fake access token | `401` |
| Refresh token used as an access token | `401` (tokens have a specific scope) |
| Return address not authorized | `400 invalid_redirect_uri` |
| Request without PKCE | redirect with `error=invalid_request` |
| Expired or reused code, or wrong `code_verifier` | `400 invalid_grant` |
| Refresh token already used or from another client | `400 invalid_grant` |
| Too many failed logins | `429` + `Retry-After: 600` |
| User without the permission a tool requires | the tool is not listed in `tools/list`, and calling it gives an error |

## 7. Where it lives in the code

- `uLBOAuthProvider.pas`: metadata, registration, login page, `/oauth/token`, anti-abuse limits.
- `uLBOAuthSetup.pas`: reads the `[OAuth]` section of the INI file (public address, token lifetimes, return addresses, texts of the login page), creates the provider and adds it to the web server chain. The provider does not depend on the AI connector.
- `uTokenManager.pas`: stores the tokens (scope `ai` for access and `ai_refresh` for renewal),
  expires them and associates them with the user. The rest of the application does not change.
- `uLBAIConnector.pas`: asks the TokenManager to validate the Bearer and filters the tools by role.

## 8. Minimal configuration to remember

- `PublicBaseURL`: the public **https** address of the server. Always set it. It holds **only scheme, host and port** (for example `https://cmms.example.com`): the paths (`/mcp`, `/oauth/...`, `/.well-known/...`) are added by the server. With a path OAuth does not start.
- `AllowedRedirectURIs`: `https://claude.ai/api/mcp/auth_callback` (add those of other AIs if needed).
- The texts of the login page (title, heading, intro with `{host}` and `{app}`, labels) are chosen in the `[OAuth]` section; the defaults are neutral.
- The server must be exposed over HTTPS (directly or behind a reverse proxy): without HTTPS tokens and
  password travel in clear text.

## 9. In one sentence

> Mario proves who he is **to your server**, your server gives Claude a short-lived card, and every
> time Claude shows it the server applies Mario's permissions.
