# launcher.py

import sys, os, runpy
import json

# Simula la classe Request come nel worker
class FakeRequest:
    def __init__(self, filename, params_json=None):
        self.filename = filename
        self.params_raw = json.dumps(params_json or {}).encode('utf-8')

# Simula il bridge: logga le chiamate invece di scrivere su shared memory
class FakeBridge:
    def write_json(self, success, obj):
        tag = "✅ SUCCESS" if success else "❌ ERROR"
        print(f"\n[{tag}] JSON response:")
        print(json.dumps(obj, indent=2))

    def write_error(self, msg):
        self.write_json(False, { "wpbError": msg })

    def get_params_as_json(self, request):
        try:
            return json.loads(request.params_raw.decode('utf-8'))
        except Exception:
            return {}

# Launcher
def launch(script_path, params=None):
    script_path = os.path.abspath(script_path)

    if not os.path.isfile(script_path):
        print(f"Script not found: {script_path}")
        return

    script_dir = os.path.dirname(script_path)
    os.chdir(script_dir)
    sys.path.insert(0, script_dir)

    try:
        bridge = FakeBridge()
        request = FakeRequest(script_path, params)
    except Exception as e:
	    print(f"\nException: {e}")

    print(f"\nExecuting: {script_path}")
    try:
        runpy.run_path(script_path, init_globals={
            "bridge": bridge,
            "request": request
        })
        print(f"Execution terminated")
    except Exception as e:
        print(f"\nException: {e}")

# Esempio d’uso
if __name__ == "__main__":
    # filename come primo argomento, JSON opzionale come secondo
    file = sys.argv[1] if len(sys.argv) > 1 else "main.py"
    params = json.loads(sys.argv[2]) if len(sys.argv) > 2 else {}
    launch(file, params)
