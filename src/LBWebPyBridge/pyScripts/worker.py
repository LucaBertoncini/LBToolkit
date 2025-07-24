import sys
import runpy
import os
import gc
from LBBridge import LBBridge

def main():
    try:
        shm_key  = sys.argv[1]
        shm_size = int(sys.argv[2])

        # POSIX: shm_key è int → IPC key
        # Windows: shm_key è string → nome del segmento
        if shm_key.isdigit():
            import sysv_ipc
            shm = sysv_ipc.SharedMemory(int(shm_key), sysv_ipc.IPC_CREAT, size=shm_size, mode=0o666)
            bridge = LBBridge(shm, shm_size)
        else:
            bridge = LBBridge(shm_key, shm_size)

        while True:
            try:
                request = bridge.read_request()
                script_rel = request.filename.strip()

                base_dir = os.path.dirname(__file__)
                script_path = os.path.join(base_dir, script_rel)

                if not os.path.isfile(script_path):
                    raise FileNotFoundError(f"Script file non trovato: {script_path}")

                script_dir = os.path.dirname(script_path)
                os.chdir(script_dir)
                if script_dir not in sys.path:
                    sys.path.insert(0, script_dir)

                runpy.run_path(script_path, init_globals={
                    "bridge": bridge,
                    "request": request
                })

            except Exception as e:
                bridge.write_error(f"Execution failed: {str(e)}")

            finally:
                gc.collect()

    except Exception as outer:
        print(f"[Worker] Startup error: {outer}")

if __name__ == '__main__':
    main()
