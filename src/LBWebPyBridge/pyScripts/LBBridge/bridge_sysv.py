# bridge_sysv.py

import struct, json, time
import sysv_ipc
from .LBBridge_constants import *

class Request:
    def __init__(self, filename, params_raw):
        self.filename = filename
        self.params_raw = params_raw

    def get_json(self):
        try:
            return json.loads(self.params_raw.decode('utf-8'))
        except Exception:
            return {}


class LBBridge:
    def __init__(self, shm, shm_size):
        self.shm = shm
        self.shm_size = shm_size

    def wait_for_request(self):
        while True:
            raw = self.shm.read(1 + HEADER_SIZE_IN, offset=0)
            if raw and raw[0] == TRIGGER_REQUEST:
                file_len, params_len = struct.unpack(HEADER_FORMAT_IN, raw[1:1 + HEADER_SIZE_IN])
                return file_len, params_len
            time.sleep(0.01)

    def read_request(self):
        file_len, params_len = self.wait_for_request()
        raw = self.shm.read(file_len + params_len, offset=1 + HEADER_SIZE_IN)

        filename_bytes = raw[:file_len]
        params_raw = raw[file_len:]

        filename = filename_bytes.decode('utf-8', errors='replace').strip()
        return Request(filename, params_raw)


    def write_response(self, success, data: bytes):
        status = STATUS_SUCCESS if success else STATUS_FAILURE
        required = HEADER_SIZE_OUT + len(data)

        if required > self.shm_size:
            fallback = b"Out of memory"
            header = struct.pack(HEADER_FORMAT_OUT, 0, STATUS_FAILURE, len(fallback))
            self.shm.write(header + fallback)
            self._trigger()
            return False

        header = struct.pack(HEADER_FORMAT_OUT, 0, status, len(data))
        self.shm.write(header + data)
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
        self.shm.write(bytes([TRIGGER_RESPONSE]), 0)

