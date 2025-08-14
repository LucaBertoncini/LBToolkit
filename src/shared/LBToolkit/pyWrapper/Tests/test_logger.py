import unittest
import os
import time
import sys

current_dir = os.path.dirname(__file__)
pywrapper_dir = os.path.abspath(os.path.join(current_dir, '..'))
sys.path.insert(0, pywrapper_dir)

from lbtoolkit_wrapper import LBToolkit

# Define the path to the compiled library.
LIB_PATH = None # Will be set from environment variable
LOG_FILE = os.path.join(current_dir, "test_log.log")

# A global variable to hold the toolkit instance
g_toolkit = None

def setUpModule():
    """
    Load the library once for the entire test module.
    If the library cannot be loaded, all tests will be skipped.
    """
    global g_toolkit, LIB_PATH
    try:
        # Get library path from environment variable for flexibility
        LIB_PATH = os.environ.get("LBTOOLKIT_LIB_PATH")

        # If it's not defined, use a default relative path for convenience
        if not LIB_PATH:
            default_path = os.path.abspath(os.path.join(current_dir, '..', 'src', 'shared', 'LBToolkit', 'sharedLib', 'liblbtoolkit.so'))
            if os.path.exists(default_path):
                 LIB_PATH = default_path
        
        if not LIB_PATH or not os.path.exists(LIB_PATH):
             raise ImportError(f"Library path not found. Set LBTOOLKIT_LIB_PATH or place library at default location.")

        g_toolkit = LBToolkit(LIB_PATH)
    except ImportError as e:
        print(f"CRITICAL: Could not load LBToolkit library. All tests will be skipped. Error: {e}")
        g_toolkit = None
    except Exception as e:
        print(f"An unexpected error occurred during module setup: {e}")
        g_toolkit = None


class TestLoggerWrapper(unittest.TestCase):

    def setUp(self):
        if g_toolkit is None:
            self.skipTest("Skipping test because LBToolkit library could not be loaded.")
        """Ensure the log file doesn't exist before a test."""
        if os.path.exists(LOG_FILE):
            os.remove(LOG_FILE)

    def tearDown(self):
        """Clean up the log file after each test."""
        # if os.path.exists(LOG_FILE):
        #     try:
        #         os.remove(LOG_FILE)
        #     except OSError as e:
        #         print(f"Error removing test log file: {e}")

    def test_log_lifecycle_with_new_api(self):
        """
        Tests the full lifecycle of the logger using the new object-oriented API.
        """
        # 1. Get a logger instance. Initialization happens here.
        logger = g_toolkit.get_logger(log_filename=LOG_FILE, log_level=10)
        self.assertIsNotNone(logger)
        
        # 2. Write some logs using the new API
        test_message_info = "This is an info message from the new Logger class."
        test_message_err = "This is an error message from the new Logger class."

        logger.write(1, "PythonTest", logger.INFO, test_message_info)
        logger.write(1, "PythonTest", logger.ERROR, test_message_err)

        # Give the logger thread a moment to write the file.
        time.sleep(0.2)

        # 3. Finalize the logger to ensure logs are flushed to disk
        logger.close()
        
        # 4. Verify the log file content
        self.assertTrue(os.path.exists(LOG_FILE), "Log file was not created.")
        
        with open(LOG_FILE, 'r', encoding='utf-8', errors='ignore') as f:
            content = f.read()
        
        self.assertIn(test_message_info, content, "The info message was not found in the log.")
        self.assertIn(test_message_err, content, "The error message was not found in the log.")
        self.assertIn("INF", content, "The 'INF' message type prefix was not found.")
        self.assertIn("ERR", content, "The 'ERR' message type prefix was not found.")
        self.assertIn("PythonTest", content, "The sender 'PythonTest' was not found.")

if __name__ == '__main__':
    unittest.main(verbosity=2)

