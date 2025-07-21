# bridge_win.py
import struct, json, sys, time
from multiprocessing import shared_memory
from .LBBridge_constants import *

class Request:
    def __init__(self, filename, params_raw):
        self.filename = filename
        self.params_raw = params_raw

class LBBridge:
    def __init__(self, shm_name, shm_size):
        self.shm = shared_memory.SharedMemory(name=shm_name)
        self.shm_size = shm_size
        self.buffer = self.shm.buf

    def wait_for_request(self):
        while True:
            raw = bytes(self.buffer[:1 + HEADER_SIZE_IN])
            if raw and raw[0] == TRIGGER_REQUEST:
                file_len, params_len = struct.unpack(HEADER_FORMAT_IN, raw[1:1 + HEADER_SIZE_IN])
                return file_len, params_len
            time.sleep(0.01)

    def read_request(self):
        file_len, params_len = self.wait_for_request()
        offset = 1 + HEADER_SIZE_IN
        raw = bytes(self.buffer[offset:offset + file_len + params_len])

        filename_bytes = raw[:file_len]
        params_raw = raw[file_len:]

        filename = filename_bytes.decode('utf-8', errors='replace').strip()
        return Request(filename, params_raw)

    def get_params_as_json(self, request):
        try:
            return json.loads(request.params_raw.decode('utf-8'))
        except Exception:
            return {}

    def write_response(self, success, data: bytes):
        status = STATUS_SUCCESS if success else STATUS_FAILURE
        required = HEADER_SIZE_OUT + len(data)
        if required > self.shm_size:
            fallback = b"Out of memory"
            header = struct.pack(HEADER_FORMAT_OUT, 0, STATUS_FAILURE, len(fallback))
            self.buffer[:len(header) + len(fallback)] = header + fallback
            self._trigger()
            return False
        header = struct.pack(HEADER_FORMAT_OUT, 0, status, len(data))
        self.buffer[:len(header) + len(data)] = header + data
        self._trigger()
        return True

    def write_json(self, success, obj: dict):
        try:
            payload = json.dumps(obj).encode('utf-8')
            return self.write_response(success, payload)
        except Exception as e:
            return self.write_error(f"JSON serialization failed: {str(e)}")

    def write_error(self, msg: str):
        return self.write_json(False, { "wpbError": msg })

    def _trigger(self):
        self.buffer[0] = TRIGGER_RESPONSE

