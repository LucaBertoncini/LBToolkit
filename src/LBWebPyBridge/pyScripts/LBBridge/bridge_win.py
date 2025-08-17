from multiprocessing import shared_memory, Semaphore
from .bridge_base import BridgeBase

class WindowsBridge(BridgeBase):
    """
    A bridge implementation for Windows using the standard 'multiprocessing' module.
    """
    def __init__(self, shm_name, shm_size, sem_req_name, sem_res_name):
        self._shm_instance = None
        self._sem_req_instance = None
        self._sem_res_instance = None
        try:
            # Create or attach to a shared memory segment using the standard library
            self._shm_instance = shared_memory.SharedMemory(name=shm_name, create=True, size=shm_size)
            
            # The 'buf' attribute is a memoryview, which has a compatible interface
            # (it can be sliced and written to). We need a small wrapper for read/write methods.
            shm_wrapper = self._SharedMemoryWrapper(self._shm_instance.buf)

            # Create named semaphores for cross-process communication
            self._sem_req_instance = Semaphore(value=0, name=sem_req_name)
            self._sem_res_instance = Semaphore(value=0, name=sem_res_name)

        except Exception as e:
            self.cleanup()
            raise ConnectionError(f"Failed to create Windows IPC objects: {e}")

        super().__init__((shm_wrapper, self._sem_req_instance, self._sem_res_instance))

    def cleanup(self):
        """Releases and unlinks the shared memory and semaphores."""
        if self._shm_instance:
            self._shm_instance.close()
            self._shm_instance.unlink()
            self._shm_instance = None
        if self._sem_req_instance:
            self._sem_req_instance.close()
        if self._sem_res_instance:
            self._sem_res_instance.close()

    def __del__(self):
        self.cleanup()

    class _SharedMemoryWrapper:
        """
        A small wrapper to make the memoryview object from multiprocessing.shared_memory
        have the same .read() and .write() methods as the sysv_ipc object,
        to provide a consistent interface to BridgeBase.
        """
        def __init__(self, memview):
            self.buf = memview

        def read(self, size, offset=0):
            return self.buf[offset:offset+size]

        def write(self, data, offset=0):
            size = len(data)
            self.buf[offset:offset+size] = data

