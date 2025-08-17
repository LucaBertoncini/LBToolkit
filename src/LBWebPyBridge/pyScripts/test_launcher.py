import sys
import os
import runpy
import json
import gc
from collections.abc import MutableMapping

# --- Copied from bridge_base.py to make the launcher self-contained ---
class OrderedCaseInsensitiveDict(MutableMapping):
    def __init__(self, data=None):
        self._data = []
        if data:
            for k, v in data.items() if isinstance(data, dict) else data:
                self._data.append((k, v))
    def __setitem__(self, key, value):
        for i, (k, v) in enumerate(self._data):
            if k.lower() == key.lower(): self._data[i] = (key, value); return
        self._data.append((key, value))
    def __getitem__(self, key):
        if isinstance(key, int): return self._data[key]
        for k, v in self._data:
            if k.lower() == key.lower(): return v
        raise KeyError(key)
    def __delitem__(self, key):
        for i, (k, v) in enumerate(self._data):
            if k.lower() == key.lower(): del self._data[i]; return
        raise KeyError(key)
    def __iter__(self): return iter(k for k, v in self._data)
    def __len__(self): return len(self._data)
    def __repr__(self): return repr(self._data)
    def items(self): return self._data

class Request:
    def __init__(self, script_name, uri_params, headers):
        self.script_name = script_name
        self.uri_params = uri_params
        self.headers = headers
# --- End of copied code ---


class FakeBridge:
    """
    A fake bridge that simulates the real BridgeBase API.
    It takes mock data and prints responses to the console instead of using IPC.
    """
    def __init__(self, mock_payload=None):
        self._mock_payload = (mock_payload or b"")

    def getRawPayload(self):
        print("[Launcher] Script requested Raw Payload.")
        return self._mock_payload

    def getPayloadAsJSON(self):
        print("[Launcher] Script requested JSON Payload.")
        if not self._mock_payload:
            return None
        try:
            return json.loads(self._mock_payload.decode('utf-8'))
        except (json.JSONDecodeError, UnicodeDecodeError):
            print("[Launcher] Warning: Mock payload is not valid JSON.")
            return None

    def writeResponseAsJSON(self, obj, status=1):
        tag = "✅ SUCCESS" if status == 1 else "❌ ERROR"
        resp = json.dumps(obj, ensure_ascii=False, indent=2)
        print(f"\n[{tag}] Script Response (JSON):\n{resp}")

    def writeRawResponse(self, data, status=1):
        tag = "✅ SUCCESS" if status == 1 else "❌ ERROR"
        try:
            # Try to decode for pretty printing, but fall back if it's not text
            decoded_data = data.decode('utf-8')
            print(f"\n[{tag}] Script Response (Raw Text):\n{decoded_data}")
        except UnicodeDecodeError:
            print(f"\n[{tag}] Script Response (Raw Bytes, hex):\n{data.hex()}")


def launch(script_path, uri_params=None, headers=None, payload=None):
    """
    Main launcher function. Sets up the environment and runs the target script.
    """
    script_path = os.path.abspath(script_path)
    if not os.path.isfile(script_path):
        print(f"❌ Error: Script not found at '{script_path}'")
        return

    # Set up the environment
    script_dir = os.path.dirname(script_path)
    os.chdir(script_dir)
    if script_dir not in sys.path:
        sys.path.insert(0, script_dir)

    # Create the fake objects with mock data
    bridge = FakeBridge(payload)
    request = Request(
        os.path.basename(script_path),
        OrderedCaseInsensitiveDict(uri_params),
        OrderedCaseInsensitiveDict(headers)
    )

    print(f"\n🚀 Executing: {script_path}")
    print(f"   - URI Params: {request.uri_params}")
    print(f"   - Headers: {request.headers}")
    print(f"   - Payload: {payload}")
    print("-" * 20)

    try:
        runpy.run_path(script_path, init_globals={
            "bridge": bridge,
            "request": request
        })
    except Exception as e:
        print(f"\n❌ Unhandled exception during script execution: {e}")
    finally:
        gc.collect()

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python3 test_launcher.py <script_path> [uri_params_json] [headers_json] [payload_string]")
        print("\nExample:")
        print("python3 test_launcher.py tests/AppHello/main.py '{\"id\":\"123\"}' '{\"X-API-KEY\":\"abc\"}' '{\"message\":\"hello\"}'")
        print("python3 test_launcher.py your_script.py '{}' '{}' 'the payload'")
        sys.exit(1)

    script = sys.argv[1]
    
    try:
        p = json.loads(sys.argv[2]) if len(sys.argv) > 2 else {}
    except Exception as e:
        print(f"⚠️ Invalid JSON for URI Params: {e}")
        p = {}

    try:
        h = json.loads(sys.argv[3]) if len(sys.argv) > 3 else {}
    except Exception as e:
        print(f"⚠️ Invalid JSON for Headers: {e}")
        h = {}
        
    load = sys.argv[4].encode('utf-8') if len(sys.argv) > 4 else b''

    launch(script, uri_params=p, headers=h, payload=load)

