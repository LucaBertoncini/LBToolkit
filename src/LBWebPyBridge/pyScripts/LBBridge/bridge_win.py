# bridge_win.py
import struct, json, sys, time
from multiprocessing import shared_memory
from .LBBridge_constants import *
from . import sem_win

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
    def __init__(self, shm_name, shm_size, req_sem_name=None, res_sem_name=None):
        self.shm = shared_memory.SharedMemory(name=shm_name)
        self.shm_size = shm_size
        self.buffer = self.shm.buf
        self.req_sem = sem_win.Semaphore(req_sem_name) if req_sem_name else None
        self.res_sem = sem_win.Semaphore(res_sem_name) if res_sem_name else None

    def wait_for_request(self):
        # This call will now block until the Pascal host signals the request semaphore.
        if self.req_sem:
            self.req_sem.acquire()

        # Once woken up, read the header from shared memory
        raw = bytes(self.buffer[:1 + HEADER_SIZE_IN])
        file_len, params_len = struct.unpack(HEADER_FORMAT_IN, raw[1:1 + HEADER_SIZE_IN])
        return file_len, params_len

    def read_request(self):
        file_len, params_len = self.wait_for_request()
        offset = 1 + HEADER_SIZE_IN
        raw = bytes(self.buffer[offset:offset + file_len + params_len])

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

    def signal_completion(self):
        if self.res_sem:
            self.res_sem.release()


