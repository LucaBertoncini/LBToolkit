import win32event
import win32con
from multiprocessing import shared_memory
from .bridge_base import BridgeBase

SEMAPHORE_ALL_ACCESS = 0x1F0003

class WindowsBridge(BridgeBase):
    def __init__(self, shm_name, shm_size, sem_req_name, sem_res_name):
        self._shm_instance = None
        self._sem_req_handle = None
        self._sem_res_handle = None

        try:
            self._shm_instance = shared_memory.SharedMemory(name=shm_name, create=False)
            shm_wrapper = self._SharedMemoryWrapper(self._shm_instance.buf)

            self._sem_req_handle = win32event.OpenSemaphore(SEMAPHORE_ALL_ACCESS, False, sem_req_name)
            self._sem_res_handle = win32event.OpenSemaphore(SEMAPHORE_ALL_ACCESS, False, sem_res_name)

        except Exception as e:
            self.cleanup()
            raise ConnectionError(f"Failed to connect to Windows IPC objects: {e}")

        super().__init__((shm_wrapper, self, self))

    def cleanup(self):
        if self._shm_instance:
            self._shm_instance.close()
            self._shm_instance = None
        if self._sem_req_handle:
            win32event.CloseHandle(self._sem_req_handle)
        if self._sem_res_handle:
            win32event.CloseHandle(self._sem_res_handle)

    def __del__(self):
        self.cleanup()

    # Wrapper BridgeBase
    def acquire(self):
        win32event.WaitForSingleObject(self._sem_req_handle, win32event.INFINITE)

    def release(self):
        win32event.ReleaseSemaphore(self._sem_res_handle, 1)

    class _SharedMemoryWrapper:
        def __init__(self, memview):
            self.buf = memview

        def read(self, size, offset=0):
            return self.buf[offset:offset+size]

        def write(self, data, offset=0):
            size = len(data)
            self.buf[offset:offset+size] = data
