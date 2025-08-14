import unittest
import os
import sys
import time
import requests
import json

current_dir = os.path.dirname(__file__)
pywrapper_dir = os.path.abspath(os.path.join(current_dir, '..'))
sys.path.insert(0, pywrapper_dir)

from lbtoolkit_wrapper import LBToolkit, Logger

# --- Test Setup ---
CONFIG_FILE = os.path.join(current_dir, "webpybridge_config.ini")
SERVER_URL = "http://127.0.0.1:8098"
LOG_FILE = os.path.join(current_dir, "test_webpybridge.log")

g_toolkit = None
g_logger = None
g_app_instance = None # Will hold the WebPyBridgeApp object

# --- Test Module Setup ---

def setUpModule():
    """Load the library and logger once for the entire test module."""
    global g_toolkit, g_logger
    try:
        lib_path = os.environ.get("LBTOOLKIT_LIB_PATH")
        if not lib_path:
            lib_path_default = os.path.abspath(os.path.join(current_dir, '..', 'src', 'shared', 'LBToolkit', 'sharedLib', 'liblbtoolkit.so'))
            if os.path.exists(lib_path_default):
                lib_path = lib_path_default
        
        if not lib_path or not os.path.exists(lib_path):
            raise ImportError("LBTOOLKIT_LIB_PATH env var not set or default library path not found.")

        g_toolkit = LBToolkit(library_path=lib_path)
        g_logger = g_toolkit.get_logger(log_filename=LOG_FILE, log_level=10)

    except (ImportError, Exception) as e:
        print(f"CRITICAL: Could not load LBToolkit library. All tests will be skipped. Error: {e}")
        g_toolkit = None
        g_logger = None

def tearDownModule():
    """Finalize the logger after all tests are done."""
    if g_logger:
        g_logger.close()

# --- Test Case ---

class TestWebPyBridgeWrapper(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        """Start the WebPyBridge application before any tests run."""
        global g_app_instance
        
        g_logger.write(1, "TestWebPyBridge", g_logger.DEBUG, "Setting up test class.")
        # Ensure the script directory exists
        py_scripts_dir = "py_scripts/test_app"
        if not os.path.exists(py_scripts_dir):
            os.makedirs(py_scripts_dir, exist_ok=True)
        
        with open(os.path.join(py_scripts_dir, "main.py"), "w") as f:
            f.write('import json\n\ndef main(bridge, request):\n  data=request.get_json()\n  data["processed_by_python"]=True\n  bridge.write_json(True, data)\nmain(bridge, request)')

        g_logger.write(1, "TestWebPyBridge", g_logger.INFO, f"Creating WebPyBridge app with config: {CONFIG_FILE}")
        g_app_instance = g_toolkit.create_webpy_bridge_app(
            config_filename=CONFIG_FILE
        )
        
        time.sleep(0.2)
        

    @classmethod
    def tearDownClass(cls):
        """Stop the app after all tests have run."""
        global g_app_instance
        if g_app_instance:
            g_logger.write(1, "TestWebPyBridge", g_logger.DEBUG, "Destroying WebPyBridge app...")
            g_app_instance.destroy()
            g_app_instance = None
            time.sleep(0.2) # Give time for threads to close

    def test_python_script_execution(self):
        """
        Tests if a POST request correctly executes a python script via the bridge
        and returns the processed data.
        """
        test_path = "/test_app/main"
        g_logger.write(1, "WebPyBridgeTest", g_logger.INFO, f"Sending POST request to: {SERVER_URL}{test_path}")
        
        input_data = {
            "client_name": "PythonTest",
            "request_id": 12345
        }
        
        response = requests.post(f"{SERVER_URL}{test_path}", json=input_data)
        
        self.assertEqual(response.status_code, 200, f"Expected status 200, but got {response.status_code}. Response: {response.text}")
        
        try:
            response_data = response.json()
        except json.JSONDecodeError:
            self.fail(f"Failed to decode JSON from response. Response text: {response.text}")

        g_logger.write(1, "WebPyBridgeTest", g_logger.DEBUG, f"Received response: {response_data}")
        self.assertIn("processed_by_python", response_data)
        self.assertTrue(response_data["processed_by_python"])
        self.assertEqual(response_data["client_name"], "PythonTest")
        self.assertEqual(response_data["request_id"], 12345)

if __name__ == '__main__':
    unittest.main(verbosity=2)

