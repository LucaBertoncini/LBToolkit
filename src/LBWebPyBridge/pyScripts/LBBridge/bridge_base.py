import struct
import json
import sys
from collections.abc import MutableMapping
from lb_logger import logger

STRUCT_ENDIAN = '<' if sys.byteorder == 'little' else '>'
HEADER_FORMAT_IN = STRUCT_ENDIAN + 'BHH' + 'I'
HEADER_SIZE_IN = struct.calcsize(HEADER_FORMAT_IN)
HEADER_FORMAT_OUT = STRUCT_ENDIAN + 'B' + 'I'
HEADER_SIZE_OUT = struct.calcsize(HEADER_FORMAT_OUT)

class OrderedCaseInsensitiveDict(MutableMapping):
    def __init__(self, data=None):
        self._data = []
        if data:
            if isinstance(data, dict):
                for k, v in data.items():
                    self._data.append((k, v))
            else:
                for k, v in data:
                    self._data.append((k, v))

    def __setitem__(self, key, value):
        for i, (k, v) in enumerate(self._data):
            if k.lower() == key.lower():
                self._data[i] = (key, value)
                return
        self._data.append((key, value))

    def __getitem__(self, key):
        if isinstance(key, int):
            return self._data[key]
        for k, v in self._data:
            if k.lower() == key.lower():
                return v
        raise KeyError(key)

    def __delitem__(self, key):
        for i, (k, v) in enumerate(self._data):
            if k.lower() == key.lower():
                del self._data[i]
                return
        raise KeyError(key)

    def __iter__(self):
        return iter(k for k, v in self._data)

    def __len__(self):
        return len(self._data)

    def __repr__(self):
        return repr(self._data)

    def items(self):
        return self._data

class Request:
    def __init__(self, script_name, uri_params, headers):
        self.script_name = script_name
        self.uri_params = uri_params
        self.headers = headers

    def __repr__(self):
        return (f"Request(script_name='{self.script_name}', "
                f"uri_params={self.uri_params}, headers={self.headers})")

class BridgeBase:
    def __init__(self, ipc_objects):
        self.shm, self.sem_req, self.sem_res = ipc_objects
        self._payload_info = None

    def read_shm(self, size, offset):
        return self.shm.read(size, offset=offset)

    def write_shm(self, data, offset):
        self.shm.write(data, offset=offset)

    def _parse_key_value(self, raw_data, separator):
        if not raw_data:
            return OrderedCaseInsensitiveDict()
        
        pairs = []
        lines = raw_data.decode('utf-8', errors='ignore').strip().split('\r\n')
        for line in lines:
            if not line:
                continue
            parts = line.split(separator, 1)
            if len(parts) == 2:
                pairs.append((parts[0].strip(), parts[1].strip()))
        return OrderedCaseInsensitiveDict(pairs)

    def read_request(self):
        self.sem_req.acquire()
        
        header_data = self.read_shm(HEADER_SIZE_IN, offset=0)
        script_len, params_len, headers_len, payload_len = struct.unpack(HEADER_FORMAT_IN, header_data)

        current_offset = HEADER_SIZE_IN

        script_name_raw = self.read_shm(script_len, offset=current_offset)
        script_name = script_name_raw.decode('utf-8', errors='ignore')
        current_offset += script_len

        uri_params_raw = self.read_shm(params_len, offset=current_offset)
        current_offset += params_len
        
        headers_raw = self.read_shm(headers_len, offset=current_offset)
        current_offset += headers_len

        self._payload_info = (current_offset, payload_len)

        uri_params = self._parse_key_value(uri_params_raw, '=')
        headers = self._parse_key_value(headers_raw, ':')
        
        return Request(script_name, uri_params, headers)

    def getRawPayload(self):
        if not self._payload_info or self._payload_info[1] == 0:
            return b''
        offset, length = self._payload_info
        return self.read_shm(length, offset)

    def getPayloadAsJSON(self):
        raw_payload = self.getRawPayload()
        if not raw_payload:
            return None
        try:
            return json.loads(raw_payload.decode('utf-8'))
        except (json.JSONDecodeError, UnicodeDecodeError):
            return None

    def _write_response(self, status, payload):
        header = struct.pack(HEADER_FORMAT_OUT, status, len(payload))
        self.write_shm(header, offset=0)
        if payload:
            self.write_shm(payload, offset=HEADER_SIZE_OUT)

    def writeResponseAsJSON(self, data_obj, success=True):
        status_code = 1 if success else 0
        try:
            logger.info(f"writeResponseAsJSON: data_obj {data_obj}")
            payload = json.dumps(data_obj, ensure_ascii=False).encode('utf-8')
            logger.info(f"Sending JSON {payload}")
            self._write_response(status_code, payload)
        except TypeError as e:
            error_payload = json.dumps({'error': f'JSON serialization failed: {e}'}).encode('utf-8')
            self._write_response(0, error_payload)

    def writeRawResponse(self, data, success=True):
        status_code = 1 if success else 0
        if not isinstance(data, bytes):
            raise TypeError("Raw response data must be of type bytes.")
        self._write_response(status_code, data)

    def signal_completion(self):
        self.sem_res.release()

