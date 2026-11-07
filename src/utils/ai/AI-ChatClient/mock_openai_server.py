#!/usr/bin/env python3
"""
mock_openai_server.py - a tiny, dependency-free HTTP server that mimics an
OpenAI-compatible "chat completions" endpoint, for testing TAIChatClient
(uAIChatClient.pas) WITHOUT spending real API credits or depending on a
live provider's availability/rate limits.

Usage:
    python3 mock_openai_server.py [port]   # default port 8765

Point TAIChatProviderConfig.Endpoint at:
    http://127.0.0.1:8765/v1/chat/completions

And APIKey at:
    test-key-123

Behavior, driven entirely by what the CLIENT sends (no special server-side
flags needed for the common cases):
  - A well-formed request with Authorization: Bearer test-key-123 gets a
    200 response shaped exactly like a real OpenAI chat completion,
    echoing back the user's own message content inside the answer (so a
    test can assert the RIGHT text was actually sent, not just that SOME
    response came back).
  - Any other (or missing) Authorization header gets a 401, with a real
    OpenAI-shaped error body ({"error": {"message": ..., "type": ...}}) -
    for testing the HTTP-error/provider-error-message path.
  - A user message containing the literal marker "TRIGGER_500" gets a 500
    with a generic error body - for testing the generic HTTP-error path
    (no parseable error.message).
  - A user message containing "TRIGGER_MALFORMED_JSON" gets a 200 status
    but a deliberately truncated/invalid JSON body - for testing the
    "malformed JSON in an otherwise-200 response" path.
  - A user message containing "TRIGGER_UNEXPECTED_SHAPE" gets a 200 with
    valid JSON that lacks choices[0].message.content entirely - for
    testing the "unexpected JSON structure" path.
  - A user message containing "TRIGGER_SLOW" sleeps for 5 seconds before
    responding - for testing a short client-side timeout.
"""

import json
import re
import time
from http.server import BaseHTTPRequestHandler, HTTPServer

EXPECTED_TOKEN = "test-key-123"


class Handler(BaseHTTPRequestHandler):
    def log_message(self, fmt, *args):
        # Quieter default logging; still prints one line per request.
        print(f"[mock-server] {self.command} {self.path} -> ", end="")

    def _send_json(self, status: int, payload: dict):
        body = json.dumps(payload).encode("utf-8")
        self.send_response(status)
        self.send_header("Content-Type", "application/json; charset=utf-8")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)
        print(status)

    def _send_raw(self, status: int, raw_body: bytes):
        self.send_response(status)
        self.send_header("Content-Type", "application/json; charset=utf-8")
        self.send_header("Content-Length", str(len(raw_body)))
        self.end_headers()
        self.wfile.write(raw_body)
        print(status, "(raw/malformed body)")

    def do_POST(self):
        length = int(self.headers.get("Content-Length", "0"))
        raw_body = self.rfile.read(length) if length > 0 else b""

        auth = self.headers.get("Authorization", "")
        expected = f"Bearer {EXPECTED_TOKEN}"
        if auth != expected:
            self._send_json(401, {
                "error": {
                    "message": f"Incorrect API key provided (got Authorization: {auth!r}).",
                    "type": "invalid_request_error",
                    "code": "invalid_api_key",
                }
            })
            return

        try:
            payload = json.loads(raw_body.decode("utf-8"))
        except Exception as e:
            self._send_json(400, {"error": {"message": f"Client sent invalid JSON: {e}"}})
            return

        # Extract the user message content, for the trigger checks and
        # for echoing it back in the canned success response.
        user_content = ""
        for m in payload.get("messages", []):
            if m.get("role") == "user":
                user_content = m.get("content", "")

        if "TRIGGER_500" in user_content:
            self._send_json(500, {"error": {"message": "Internal provider error (simulated)."}})
            return

        if "TRIGGER_MALFORMED_JSON" in user_content:
            # Deliberately truncated JSON: a real "the provider's own
            # response was garbage" scenario, status 200 on the wire.
            self._send_raw(200, b'{"choices": [ { "message": { "content": "oops, truncated')
            return

        if "TRIGGER_UNEXPECTED_SHAPE" in user_content:
            # Valid JSON, but missing choices[0].message.content entirely.
            self._send_json(200, {"id": "cmpl-test", "object": "chat.completion", "choices": []})
            return

        if "TRIGGER_SLOW" in user_content:
            time.sleep(5)

        # Default: a well-formed success response, echoing the model name
        # sent and a snippet of the user's own message, so a test can
        # assert the RIGHT request actually reached the server.
        model = payload.get("model", "unknown-model")
        snippet = user_content[:60].replace("\n", " ")
        self._send_json(200, {
            "id": "cmpl-test-001",
            "object": "chat.completion",
            "model": model,
            "choices": [
                {
                    "index": 0,
                    "message": {
                        "role": "assistant",
                        "content": f"[risposta simulata, modello={model}] Ho ricevuto: {snippet!r}",
                    },
                    "finish_reason": "stop",
                }
            ],
            "usage": {"prompt_tokens": 10, "completion_tokens": 10, "total_tokens": 20},
        })


def main():
    import sys
    port = int(sys.argv[1]) if len(sys.argv) > 1 else 8765
    server = HTTPServer(("127.0.0.1", port), Handler)
    print(f"Mock OpenAI-compatible server listening on http://127.0.0.1:{port}/v1/chat/completions")
    print(f"Expected API key: {EXPECTED_TOKEN}")
    server.serve_forever()


if __name__ == "__main__":
    main()
