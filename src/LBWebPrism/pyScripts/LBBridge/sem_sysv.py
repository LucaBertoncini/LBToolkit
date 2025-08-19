import sysv_ipc
from lb_logger import logger


class Semaphore:
    def __init__(self, key):
        self.key = key
        self.sem = None
        try:
            # Try to attach to an existing semaphore
            self.sem = sysv_ipc.Semaphore(self.key)
        except sysv_ipc.ExistentialError:
            # If it doesn't exist, create a new one and initialize it
            self.sem = sysv_ipc.Semaphore(self.key, sysv_ipc.IPC_CREAT)
            try:
                self.sem.release()  # Set initial value to 1
            except Exception as e:
                logger.error(f"Initial release failed: {e}")

    def acquire(self, timeout=None):
        # Block until the semaphore can be acquired, or timeout expires.
        if self.sem:
            try:
                if timeout is None:
                    self.sem.acquire()
                else:
                    self.sem.acquire(timeout=timeout)
            except Exception as e:
                logger.error(f"Semaphore acquire failed: {e}")

    def release(self):
        # Release the semaphore (increment its value).
        if self.sem:
            try:
                self.sem.release()
            except Exception as e:
                logger.warning(f"Semaphore release failed: {e}")

    def reset(self):
        # Forcefully acquire the semaphore to reset its value to 0.
        if self.sem:
            try:
                self.sem.acquire()
            except Exception as e:
                logger.warning(f"Semaphore reset failed: {e}")

    def __del__(self):
        # Do not remove the semaphore; it's managed externally
        pass

