import unittest
import os
import sys
import time
import requests
import ctypes

current_dir = os.path.dirname(__file__)
pywrapper_dir = os.path.abspath(os.path.join(current_dir, '..'))
sys.path.insert(0, pywrapper_dir)

from lbtoolkit_wrapper import LBToolkit, Logger

# --- Test Setup ---
CONFIG_FILE = os.path.join(current_dir, "webserver_config.ini")
SERVER_URL = "http://127.0.0.1:8099"
LOG_FILE = os.path.join(current_dir, "test_webserver.log")

g_toolkit = None
g_logger = None
g_server_instance = None # Will hold the WebServer object
g_callback_data = {} # Shared dictionary to verify callbacks were called

# --- Python Callback Functions ---

def handle_get_request(elaborator_ptr, resource_ptr, response_code_ptr):
    """Callback for GET requests."""
    global g_callback_data, g_server_instance
    try:
        resource = ctypes.cast(resource_ptr, ctypes.c_char_p).value.decode('utf-8')
        g_logger.write(1, "handle_get_request", g_logger.DEBUG, f"GET request received for resource: {resource}")
        
        g_callback_data['get_called_for'] = resource
        
        response_str = f"Hello from Python! You requested: {resource}".encode('utf-8')
        g_server_instance.set_response_data(elaborator_ptr, response_str)
        
        response_code_ptr.contents.value = 200
        return True
    except Exception as e:
        g_logger.write(1, "handle_get_request", g_logger.ERROR, f"Error in GET callback: {e}")
        return False

def handle_post_request(elaborator_ptr, resource_ptr, payload_ptr, payload_len, response_code_ptr):
    """Callback for POST requests."""
    global g_callback_data, g_server_instance
    try:
        resource = ctypes.cast(resource_ptr, ctypes.c_char_p).value.decode('utf-8')
        payload = ctypes.string_at(payload_ptr, payload_len).decode('utf-8')
        g_logger.write(1, "handle_post_request", g_logger.DEBUG, f"POST request for resource: {resource} with payload: {payload}")
        
        g_callback_data['post_called_for'] = resource
        g_callback_data['post_payload'] = payload
        
        response_str = f"Python received your post: {payload}".encode('utf-8')
        g_server_instance.set_response_data(elaborator_ptr, response_str)
        
        response_code_ptr.contents.value = 201
        return True
    except Exception as e:
        g_logger.write(1, "handle_post_request", g_logger.ERROR, f"Error in POST callback: {e}")
        return False

# --- Test Module Setup ---

def setUpModule():
    """Load the library and initialize logger once for the entire test module."""
    global g_toolkit, g_logger
    try:
        lib_path = os.environ.get("LBTOOLKIT_LIB_PATH")
        if not lib_path:
            # A default path for convenience
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
class TestWebServerWrapper(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        """Start the server before any tests run."""
        global g_server_instance
        g_logger.write(1, "TestWebServerWrapper", g_logger.DEBUG, "Creating webserver instance...")
        g_server_instance = g_toolkit.create_web_server(
            config_filename=CONFIG_FILE,
            get_callback=handle_get_request,
            post_callback=handle_post_request
        )
        if g_server_instance is None:
            raiseunittest.SkipTest("Could not create server instance.")
        
        # Give the server time to start up since it's non-blocking
        time.sleep(0.5)

    @classmethod
    def tearDownClass(cls):
        """Stop the server after all tests have run."""
        global g_server_instance
        if g_server_instance:
            g_logger.write(1, "TestWebServerWrapper", g_logger.DEBUG, "Destroying webserver instance...")
            g_server_instance.destroy()
            g_server_instance = None
            time.sleep(0.2) # Give time for threads to close

    def setUp(self):
        """Reset callback data before each test."""
        global g_callback_data
        g_callback_data = {}

    def test_01_get_request(self):
        """Tests if a GET request is correctly handled by the Python callback."""
        test_path = "/test_get"
        g_logger.write(1, "GET_Test", g_logger.INFO, f"Sending request to: {SERVER_URL}{test_path}")
        response = requests.get(f"{SERVER_URL}{test_path}")
        
        self.assertEqual(response.status_code, 200)
        self.assertEqual(response.text, f"Hello from Python! You requested: {test_path}")
        self.assertEqual(g_callback_data.get('get_called_for'), test_path)

    def test_02_post_request(self):
        """Tests if a POST request is correctly handled by the Python callback."""
        test_path = "/test_post"
        test_payload = "This is a test payload."
        g_logger.write(1, "POST_Test", g_logger.INFO, f"Sending request to: {SERVER_URL}{test_path}")
        
        response = requests.post(f"{SERVER_URL}{test_path}", data=test_payload.encode('utf-8'))
        
        self.assertEqual(response.status_code, 201)
        self.assertEqual(response.text, f"Python received your post: {test_payload}")
        self.assertEqual(g_callback_data.get('post_called_for'), test_path)
        self.assertEqual(g_callback_data.get('post_payload'), test_payload)

if __name__ == '__main__':
    unittest.main(verbosity=2)

