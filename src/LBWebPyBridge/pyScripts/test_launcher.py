import sys, os, runpy, json, gc
from lb_logger import logger, set_web_env

set_web_env(False)

# Simula la classe Request come nel worker
class FakeRequest:
    def __init__(self, filename, params_json=None):
        self.filename = filename
        self.params_raw = json.dumps(params_json or {}).encode('utf-8')
        
    def get_json(self):
        try:
            return json.loads(self.params_raw.decode('utf-8'))
        except Exception:
            return {}

# Simula il bridge: logga le chiamate invece di scrivere su shared memory
class FakeBridge:
    def write_json(self, success, obj):
        tag = "✅ SUCCESS" if success else "❌ ERROR"
        resp = json.dumps(obj, ensure_ascii=False, indent=2)
        print(f"\n[{tag}] JSON response:\n{resp}")

    def write_error(self, msg):
        self.write_json(False, { "wpbError": msg })


# Launcher
def launch(script_path, params=None):
    script_path = os.path.abspath(script_path)

    if not os.path.isfile(script_path):
        logger.warning(f"Script not found: {script_path}")
        return

    script_dir = os.path.dirname(script_path)
    os.chdir(script_dir)
    sys.path.insert(0, script_dir)

    bridge = FakeBridge()
    request = FakeRequest(script_path, params)

    logger.info(f"\n🚀 Executing: {script_path}")
    try:
        result = runpy.run_path(script_path, init_globals={
            "bridge": bridge,
            "request": request
        })
        logger.info(f"✅ Execution terminated")
    except Exception as e:
        logger.error(f"\n❌ Exception during execution: {e}")
        bridge.write_error(str(e))
    finally:
        # 🧹 Pulizia esplicita della memoria
        gc.collect()

# Esempio d’uso
if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python3 launcher.py path_to_script.py [parameters in JSON format]")
        sys.exit(1)

    file = sys.argv[1]
    try:
        params = json.loads(sys.argv[2]) if len(sys.argv) > 2 else {}
    except Exception as e:
        print(f"⚠️ Invalid JSON parameters: {e}")
        params = {}

    launch(file, params)
