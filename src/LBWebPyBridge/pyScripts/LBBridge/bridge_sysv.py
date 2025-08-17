import sysv_ipc
from .bridge_base import BridgeBase

class SysVBridge(BridgeBase):
    """
    A bridge implementation for System V IPC (Linux).
    This class is responsible for creating the platform-specific
    shared memory and semaphore objects.
    """
    def __init__(self, shm_key, shm_size, sem_req_key, sem_res_key):
        try:
            shm = sysv_ipc.SharedMemory(shm_key, flags=sysv_ipc.IPC_CREAT, size=shm_size)
            sem_req = sysv_ipc.Semaphore(sem_req_key, flags=sysv_ipc.IPC_CREAT, initial_value=0)
            sem_res = sysv_ipc.Semaphore(sem_res_key, flags=sysv_ipc.IPC_CREAT, initial_value=0)
        except sysv_ipc.Error as e:
            # Propagate the error for the main worker script to handle
            raise ConnectionError(f"Failed to create System V IPC objects: {e}")

        # Pass the created IPC objects to the base class constructor
        super().__init__((shm, sem_req, sem_res))

    def __del__(self):
        # Clean up IPC objects when the bridge is destroyed
        try:
            if self.sem_req:
                self.sem_req.remove()
            if self.sem_res:
                self.sem_res.remove()
            if self.shm:
                self.shm.remove()
        except sysv_ipc.Error:
            # Ignore errors during cleanup, as the orchestrator might have
            # already removed them.
            pass

