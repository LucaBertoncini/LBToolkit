import ctypes
from lb_logger import logger

logger = logging.getLogger(__name__)

# Win32 API definitions
kernel32 = ctypes.WinDLL('kernel32', use_last_error=True)
SYNCHRONIZE = 0x00100000
SEMAPHORE_MODIFY_STATE = 0x0002

# Function prototypes
OpenSemaphore = kernel32.OpenSemaphoreW
OpenSemaphore.argtypes = [ctypes.c_uint, ctypes.c_bool, ctypes.c_wchar_p]
OpenSemaphore.restype = ctypes.c_void_p

ReleaseSemaphore = kernel32.ReleaseSemaphore
ReleaseSemaphore.argtypes = [ctypes.c_void_p, ctypes.c_long, ctypes.c_void_p]
ReleaseSemaphore.restype = ctypes.c_bool

CloseHandle = kernel32.CloseHandle
CloseHandle.argtypes = [ctypes.c_void_p]
CloseHandle.restype = ctypes.c_bool

WaitForSingleObject = kernel32.WaitForSingleObject
WaitForSingleObject.argtypes = [ctypes.c_void_p, ctypes.c_uint]
WaitForSingleObject.restype = ctypes.c_uint

WAIT_OBJECT_0 = 0x00000000
WAIT_TIMEOUT = 0x00000102
INFINITE = 0xFFFFFFFF

class Semaphore:
    def __init__(self, name):
        self.name = name
        self.handle = OpenSemaphore(SEMAPHORE_MODIFY_STATE | SYNCHRONIZE, False, self.name)
        if not self.handle:
            raise ctypes.WinError(ctypes.get_last_error())

    def acquire(self, timeout_ms=-1):
        """Wait until the semaphore is signaled or timeout expires."""
        if self.handle:
            result = WaitForSingleObject(self.handle, timeout_ms if timeout_ms >= 0 else INFINITE)
            if result == WAIT_OBJECT_0:
                return True
            elif result == WAIT_TIMEOUT:
                logger.warning(f"Semaphore '{self.name}' acquire timed out.")
                return False
            else:
                logger.error(f"Semaphore '{self.name}' acquire failed with code {result}.")
        return False

    def release(self):
        """Signal the semaphore (increment its count)."""
        if self.handle:
            success = ReleaseSemaphore(self.handle, 1, None)
            if not success:
                logger.warning(f"Semaphore '{self.name}' release failed.")

    def reset(self):
        """Simulate a reset by acquiring the semaphore to bring its count back to 0."""
        # WARNING: This assumes the semaphore count is 1 and will block if already 0
        if self.handle:
            acquired = self.acquire(timeout_ms=0)
            if acquired:
                logger.info(f"Semaphore '{self.name}' reset to 0.")
            else:
                logger.debug(f"Semaphore '{self.name}' already at 0; no reset needed.")

    def __del__(self):
        if self.handle:
            CloseHandle(self.handle)

