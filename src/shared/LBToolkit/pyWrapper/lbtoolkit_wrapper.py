import ctypes
import os
import platform
from ctypes import (
    CFUNCTYPE, POINTER, c_bool, c_byte, c_char_p, c_int, c_void_p, c_uint
)

# Define callback function types to be used in the wrapper
GET_CALLBACK_TYPE = CFUNCTYPE(c_bool, c_void_p, c_char_p, POINTER(c_int))
POST_CALLBACK_TYPE = CFUNCTYPE(c_bool, c_void_p, c_char_p, c_void_p, c_int, POINTER(c_int))
WS_CONNECTED_CALLBACK_TYPE = CFUNCTYPE(None)
WS_MESSAGE_CALLBACK_TYPE = CFUNCTYPE(None, c_void_p, c_int)


class Logger:
    """
    A high-level wrapper for the global Logger instance in the LBToolkit library.
    """
    def __init__(self, toolkit_instance, log_filename="toolkit.log", log_level=5):
        self._toolkit = toolkit_instance
        self._lib = self._toolkit._lib
        
        b_log_filename = log_filename.encode('utf-8')
        self._lib.Logger_Initialize(b_log_filename, log_level)
        print(f"Logger initialized. Log file: '{log_filename}', Level: {log_level}")

        # Fetch and store message types for convenience
        self.INFO = self._lib.Logger_GetMsgType_Info()
        self.ERROR = self._lib.Logger_GetMsgType_Error()
        self.WARNING = self._lib.Logger_GetMsgType_Warning()
        self.DEBUG = self._lib.Logger_GetMsgType_Debug()

    def write(self, level, sender, msg_type, message):
        """
        Writes a message to the log.
        :param level: The log level of the message.
        :param sender: The source of the message.
        :param msg_type: The type of the message (e.g., logger.INFO).
        :param message: The log message content.
        """
        b_sender = sender.encode('utf-8')
        b_message = message.encode('utf-8')
        self._lib.Logger_Write(level, b_sender, msg_type, b_message)

    def close(self):
        """
        Finalizes and releases the global logger.
        """
        if self._lib:
            self._lib.Logger_Finalize()
            print("Logger finalized.")
            self._lib = None # Prevent multiple calls
    
    def __del__(self):
        self.close()


class WebServer:
    """A high-level wrapper for a WebServer instance from the LBToolkit library."""
    def __init__(self, toolkit_instance, server_handle, 
                 get_cb_obj, post_cb_obj, ws_conn_cb_obj, ws_msg_cb_obj):
        self._toolkit = toolkit_instance
        self._lib = self._toolkit._lib
        self.handle = server_handle
        self._get_cb_obj, self._post_cb_obj, self._ws_conn_cb_obj, self._ws_msg_cb_obj = \
            get_cb_obj, post_cb_obj, ws_conn_cb_obj, ws_msg_cb_obj
        print(f"WebServer instance created with handle: {self.handle}")

    def destroy(self):
        if self.handle:
            print(f"Destroying WebServer instance with handle: {self.handle}")
            self._lib.WebServer_Destroy(self.handle)
            self.handle = None

    def __del__(self):
        self.destroy()

    def add_response_header(self, elaborator_ptr, name, value):
        return self._lib.ReqElab_addResponseHeader(elaborator_ptr, name.encode('utf-8'), value.encode('utf-8'))

    def set_response_data(self, elaborator_ptr, data_bytes):
        return self._lib.ReqElab_setResponseData(elaborator_ptr, data_bytes, len(data_bytes))

    def get_all_request_headers(self, elaborator_ptr):
        headers = {}
        count = self._lib.ReqElab_RequestHeadersCount(elaborator_ptr)
        for i in range(count):
            name_len = self._lib.ReqElab_RequestHeaderName(elaborator_ptr, i, None, 0)
            name_buffer = ctypes.create_string_buffer(name_len)
            self._lib.ReqElab_RequestHeaderName(elaborator_ptr, i, name_buffer, name_len)
            value_len = self._lib.ReqElab_RequestHeaderValue(elaborator_ptr, i, None, 0)
            value_buffer = ctypes.create_string_buffer(value_len)
            self._lib.ReqElab_RequestHeaderValue(elaborator_ptr, i, value_buffer, value_len)
            headers[name_buffer.value.decode('utf-8')] = value_buffer.value.decode('utf-8')
        return headers

class WebPyBridgeApp:
    """A high-level wrapper for a WebPyBridge application instance."""
    def __init__(self, toolkit_instance, app_handle):
        self._toolkit = toolkit_instance
        self._lib = self._toolkit._lib
        self.handle = app_handle
        print(f"WebPyBridgeApp instance created with handle: {self.handle}")

    def destroy(self):
        if self.handle:
            print(f"Destroying WebPyBridgeApp instance with handle: {self.handle}")
            self._lib.WebPyBridge_Destroy(self.handle)
            self.handle = None
    
    def __del__(self):
        self.destroy()

class CircularBuffer:
    """A high-level wrapper for a CircularBuffer instance from the LBToolkit library."""
    def __init__(self, toolkit_instance, buffer_handle, is_thread_safe=False):
        self._toolkit = toolkit_instance
        self._lib = self._toolkit._lib
        self.handle = buffer_handle
        self.is_thread_safe = is_thread_safe
        
        if is_thread_safe:
            self._destroy_func, self._write_func, self._read_func, self._peek_func, self._clear_func, \
            self._get_available_for_read, self._get_available_for_write = \
                self._lib.CircularBufferTS_Destroy, self._lib.CircularBufferTS_Write, self._lib.CircularBufferTS_Read, \
                self._lib.CircularBufferTS_Peek, self._lib.CircularBufferTS_Clear, \
                self._lib.CircularBufferTS_GetAvailableForRead, self._lib.CircularBufferTS_GetAvailableForWrite
        else:
            self._destroy_func, self._write_func, self._read_func, self._peek_func, self._clear_func, \
            self._get_available_for_read, self._get_available_for_write = \
                self._lib.CircularBuffer_Destroy, self._lib.CircularBuffer_Write, self._lib.CircularBuffer_Read, \
                self._lib.CircularBuffer_Peek, self._lib.CircularBuffer_Clear, \
                self._lib.CircularBuffer_GetAvailableForRead, self._lib.CircularBuffer_GetAvailableForWrite

    def destroy(self):
        if self.handle:
            self._destroy_func(self.handle)
            self.handle = None

    def __del__(self):
        self.destroy()

    def write(self, data: bytes) -> bool:
        return self._write_func(self.handle, data, len(data)) if self.handle else False

    def read(self, count: int) -> bytes:
        if not self.handle: return b''
        buffer = ctypes.create_string_buffer(count)
        return buffer.value if self._read_func(self.handle, buffer, count) else b''

    def peek(self, count: int, offset: int = 0) -> bytes:
        if not self.handle: return b''
        buffer = ctypes.create_string_buffer(count)
        return buffer.raw[:count] if self._peek_func(self.handle, buffer, count, offset) else b''
        
    def clear(self):
        if self.handle: self._clear_func(self.handle)

    @property
    def available_for_read(self) -> int:
        return self._get_available_for_read(self.handle) if self.handle else 0

    @property
    def available_for_write(self) -> int:
        return self._get_available_for_write(self.handle) if self.handle else 0

class LBToolkit:
    """A Python wrapper for the LBToolkit shared library."""
    def __init__(self, library_path=None):
        self._lib = self._load_library(library_path)
        if not self._lib:
            raise ImportError("Could not load the LBToolkit shared library.")
        self._define_function_prototypes()
        print("LBToolkit library loaded and function prototypes defined.")

    def _load_library(self, library_path):
        if library_path and os.path.isfile(library_path):
            lib_path = library_path
        else:
            system = platform.system()
            if system == "Windows": lib_name = "LBToolkit.dll"
            elif system == "Linux": lib_name = "libLBToolkit.so"
            else: raise NotImplementedError(f"Unsupported platform: {system}")
            search_dir = library_path if library_path and os.path.isdir(library_path) else os.getcwd()
            lib_path = os.path.join(search_dir, lib_name)

        if not os.path.exists(lib_path):
            print(f"Error: Library not found at '{os.path.abspath(lib_path)}'")
            return None
        try:
            print(f"Attempting to load library from: {os.path.abspath(lib_path)}")
            return ctypes.CDLL(lib_path)
        except OSError as e:
            print(f"Error loading library: {e}")
            return None

    def _define_function_prototypes(self):
        # Logger
        self._lib.Logger_Initialize.argtypes = [c_char_p, c_int]; self._lib.Logger_Initialize.restype = None
        self._lib.Logger_Finalize.argtypes = []; self._lib.Logger_Finalize.restype = None
        self._lib.Logger_Write.argtypes = [c_int, c_char_p, c_byte, c_char_p]; self._lib.Logger_Write.restype = None
        self._lib.Logger_GetMsgType_Error.argtypes = []; self._lib.Logger_GetMsgType_Error.restype = c_byte
        self._lib.Logger_GetMsgType_Warning.argtypes = []; self._lib.Logger_GetMsgType_Warning.restype = c_byte
        self._lib.Logger_GetMsgType_Debug.argtypes = []; self._lib.Logger_GetMsgType_Debug.restype = c_byte
        self._lib.Logger_GetMsgType_Info.argtypes = []; self._lib.Logger_GetMsgType_Info.restype = c_byte
        # WebServer
        self._lib.WebServer_Create.argtypes = [c_char_p, GET_CALLBACK_TYPE, POST_CALLBACK_TYPE, WS_CONNECTED_CALLBACK_TYPE, WS_MESSAGE_CALLBACK_TYPE]; self._lib.WebServer_Create.restype = c_void_p
        self._lib.WebServer_Destroy.argtypes = [c_void_p]; self._lib.WebServer_Destroy.restype = c_bool
        # ReqElab
        self._lib.ReqElab_addResponseHeader.argtypes = [c_void_p, c_char_p, c_char_p]; self._lib.ReqElab_addResponseHeader.restype = c_bool
        self._lib.ReqElab_setResponseData.argtypes = [c_void_p, c_void_p, c_int]; self._lib.ReqElab_setResponseData.restype = c_bool
        self._lib.ReqElab_RequestHeadersCount.argtypes = [c_void_p]; self._lib.ReqElab_RequestHeadersCount.restype = c_int
        self._lib.ReqElab_RequestHeaderName.argtypes = [c_void_p, c_int, c_void_p, c_int]; self._lib.ReqElab_RequestHeaderName.restype = c_int
        self._lib.ReqElab_RequestHeaderValue.argtypes = [c_void_p, c_int, c_void_p, c_int]; self._lib.ReqElab_RequestHeaderValue.restype = c_int
        # WebPyBridge
        self._lib.WebPyBridge_Create.argtypes = [c_char_p]; self._lib.WebPyBridge_Create.restype = c_void_p
        self._lib.WebPyBridge_Destroy.argtypes = [c_void_p]; self._lib.WebPyBridge_Destroy.restype = None
        # CircularBuffer
        self._lib.CircularBuffer_Create.argtypes = [c_uint]; self._lib.CircularBuffer_Create.restype = c_void_p
        self._lib.CircularBuffer_Destroy.argtypes = [c_void_p]; self._lib.CircularBuffer_Destroy.restype = None
        self._lib.CircularBuffer_Write.argtypes = [c_void_p, c_void_p, c_uint]; self._lib.CircularBuffer_Write.restype = c_bool
        self._lib.CircularBuffer_Read.argtypes = [c_void_p, c_void_p, c_uint]; self._lib.CircularBuffer_Read.restype = c_bool
        self._lib.CircularBuffer_Peek.argtypes = [c_void_p, c_void_p, c_uint, c_uint]; self._lib.CircularBuffer_Peek.restype = c_bool
        self._lib.CircularBuffer_Clear.argtypes = [c_void_p]; self._lib.CircularBuffer_Clear.restype = None
        self._lib.CircularBuffer_GetAvailableForRead.argtypes = [c_void_p]; self._lib.CircularBuffer_GetAvailableForRead.restype = c_uint
        self._lib.CircularBuffer_GetAvailableForWrite.argtypes = [c_void_p]; self._lib.CircularBuffer_GetAvailableForWrite.restype = c_uint
        # CircularBufferTS
        self._lib.CircularBufferTS_Create.argtypes = [c_uint]; self._lib.CircularBufferTS_Create.restype = c_void_p
        self._lib.CircularBufferTS_Destroy.argtypes = [c_void_p]; self._lib.CircularBufferTS_Destroy.restype = None
        self._lib.CircularBufferTS_Write.argtypes = [c_void_p, c_void_p, c_uint]; self._lib.CircularBufferTS_Write.restype = c_bool
        self._lib.CircularBufferTS_Read.argtypes = [c_void_p, c_void_p, c_uint]; self._lib.CircularBufferTS_Read.restype = c_bool
        self._lib.CircularBufferTS_Peek.argtypes = [c_void_p, c_void_p, c_uint, c_uint]; self._lib.CircularBufferTS_Peek.restype = c_bool
        self._lib.CircularBufferTS_Clear.argtypes = [c_void_p]; self._lib.CircularBufferTS_Clear.restype = None
        self._lib.CircularBufferTS_GetAvailableForRead.argtypes = [c_void_p]; self._lib.CircularBufferTS_GetAvailableForRead.restype = c_uint
        self._lib.CircularBufferTS_GetAvailableForWrite.argtypes = [c_void_p]; self._lib.CircularBufferTS_GetAvailableForWrite.restype = c_uint

    def get_logger(self, log_filename="toolkit.log", log_level=5) -> 'Logger':
        """
        Creates and returns a Logger instance.
        Note: The underlying logger in the Pascal library is a singleton.
        """
        return Logger(self, log_filename, log_level)

    def create_web_server(self, config_filename, get_callback=None, post_callback=None, ws_connected_callback=None, ws_message_callback=None):
        c_get_cb = GET_CALLBACK_TYPE(get_callback or (lambda *args: False))
        c_post_cb = POST_CALLBACK_TYPE(post_callback or (lambda *args: False))
        c_ws_conn_cb = WS_CONNECTED_CALLBACK_TYPE(ws_connected_callback or (lambda *args: None))
        c_ws_msg_cb = WS_MESSAGE_CALLBACK_TYPE(ws_message_callback or (lambda *args: None))
        server_handle = self._lib.WebServer_Create(config_filename.encode('utf-8'), c_get_cb, c_post_cb, c_ws_conn_cb, c_ws_msg_cb)
        if not server_handle: raise RuntimeError("Failed to create WebServer instance.")
        return WebServer(self, server_handle, c_get_cb, c_post_cb, c_ws_conn_cb, c_ws_msg_cb)

    def create_webpy_bridge_app(self, config_filename):
        app_handle = self._lib.WebPyBridge_Create(config_filename.encode('utf-8'))
        if not app_handle: raise RuntimeError("Failed to create WebPyBridgeApp instance.")
        return WebPyBridgeApp(self, app_handle)

    def create_circular_buffer(self, size: int) -> 'CircularBuffer':
        buffer_handle = self._lib.CircularBuffer_Create(size)
        if not buffer_handle: raise MemoryError(f"Failed to create circular buffer of size {size}.")
        return CircularBuffer(self, buffer_handle, is_thread_safe=False)

    def create_circular_buffer_ts(self, size: int) -> 'CircularBuffer':
        buffer_handle = self._lib.CircularBufferTS_Create(size)
        if not buffer_handle: raise MemoryError(f"Failed to create thread-safe circular buffer of size {size}.")
        return CircularBuffer(self, buffer_handle, is_thread_safe=True)

if __name__ == "__main__":
    import sys
    print("--- LBToolkit Python Wrapper Test ---")
    print(f"Operating System: {platform.system()}")
    
    try:
        if len(sys.argv) == 2:
          lib_path = sys.argv[1]
          print(f"Loading lib: {lib_path}")
        else:
          lib_path = None

        toolkit = LBToolkit(library_path=lib_path)
        print("SUCCESS: LBToolkit instance created.")
    except ImportError as e:
        print(f"FAILURE: {e}")
    except Exception as e:
        print(f"An unexpected error occurred: {e}")

