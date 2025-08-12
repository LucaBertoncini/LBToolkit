import sys
import runpy
import os
import gc
from LBBridge import LBBridge
from lb_logger import logger, set_web_env

set_web_env(False)
logger.info(f"Worker started")

def main():
    try:

        if len(sys.argv) < 4:
            logger.warning("[Worker] Missing arguments. Usage: worker.py <shm_key> <shm_size> <sem_req> <sem_res>")
            return

        shm_key  = sys.argv[1]
        shm_size = int(sys.argv[2])
        sem_req = sys.argv[3]
        sem_res = sys.argv[4]

        logger.info(f"Starting parameter: shm_key={shm_key}, shm_size={shm_size}, sem_req={sem_req}, sem_res={sem_res}")

        # POSIX: shm_key/sem_id is int → IPC key
        # Windows: shm_key/sem_id is string → name
        if shm_key.isdigit():
            import sysv_ipc
            shm = sysv_ipc.SharedMemory(int(shm_key), sysv_ipc.IPC_CREAT, size=shm_size, mode=0o666)
            bridge = LBBridge(shm, shm_size, int(sem_req), int(sem_res))
        else:
            bridge = LBBridge(shm_key, shm_size, sem_req, sem_res)

        while True:
            try:
                logger.info(f"[Workey.py] - Waiting request ...")
                request = bridge.read_request()
                
                script_rel = request.filename.strip()

                base_dir = os.path.dirname(__file__)
                script_path = os.path.join(base_dir, script_rel)

                if not os.path.isfile(script_path):
                    logger.warning(f"Script file not found: {script_path}")
                    raise FileNotFoundError(f"Script file not found: {script_path}")

                logger.info(f"Elaborating script {script_path}")
                script_dir = os.path.dirname(script_path)
                os.chdir(script_dir)
                if script_dir not in sys.path:
                    sys.path.insert(0, script_dir)

                runpy.run_path(script_path, init_globals={
                    "bridge": bridge,
                    "request": request
                })

            except Exception as e:
                logger.error(f"Execution failed: {str(e)}")
                bridge.write_error(f"Execution failed: {str(e)}")

            finally:
                bridge.signal_completion()
                gc.collect()

    except Exception as outer:
        print(f"[Worker] Startup error: {outer}")

if __name__ == '__main__':
    main()

