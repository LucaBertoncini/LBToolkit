# --- Example of a User Script ---
# The worker will dynamically run user scripts found on the filesystem.
# Those scripts are expected to have access to two global objects, which are
# injected by this worker at runtime: 'bridge' and 'request'.
#
# A typical user script (e.g., /api/process_data.py) would look like this:
#
# """
# # This code runs inside the script located at `request.script_name`
#
# def some_business_logic(payload, params):
#     # ... do something important ...
#     user_id = params.get('user_id', 'default')
#     return {'status': 'processed', 'user': user_id, 'original_payload': payload}
#
# try:
#     # 1. Get metadata from the 'request' object
#     content_type = request.headers.get('Content-Type')
#     user_params = request.uri_params
#
#     # 2. Get the payload 'on-demand' from the 'bridge' object
#     if content_type == 'application/json':
#         payload = bridge.getPayloadAsJSON()
#     else:
#         payload = bridge.getRawPayload()
#
#     # 3. Execute logic
#     result = some_business_logic(payload, user_params)
#
#     # 4. Write a success response back to the orchestrator
#     bridge.writeResponseAsJSON(result, status=1)
#
# except Exception as e:
#     # 5. On error, write an error response
#     bridge.writeResponseAsJSON({'error': str(e)}, status=0)
# """
import sys
import runpy
import os
import gc
import platform
from lb_logger import logger, disable_logger

disable_logger(False)

# --- Example of a User Script ---
# ... (comment block as added before) ...

def main():
    """
    Main worker entry point.
    Initializes the correct platform-specific bridge and enters the main loop
    to process requests from the Pascal orchestrator.
    """
    bridge = None
    try:
        # --- 1. Initialize the correct bridge for the current OS ---
        if platform.system() == "Linux":
            from LBBridge.bridge_sysv import SysVBridge
            if len(sys.argv) < 5:
                logger.error("[Worker] Missing arguments for Linux (shm_key, shm_size, sem_req_key, sem_res_key)")
                return
            shm_key, shm_size, sem_req_key, sem_res_key = int(sys.argv[1]), int(sys.argv[2]), int(sys.argv[3]), int(sys.argv[4])
            bridge = SysVBridge(shm_key, shm_size, sem_req_key, sem_res_key)
            logger.info(f"Linux worker started with System V IPC keys: {shm_key}, {sem_req_key}, {sem_res_key}")

        else: # Assuming Windows
            from LBBridge.bridge_win import WindowsBridge
            if len(sys.argv) < 5:
                logger.error("[Worker] Missing arguments for Windows (shm_name, shm_size, sem_req_name, sem_res_name)")
                return
            shm_name, shm_size, sem_req_name, sem_res_name = sys.argv[1], int(sys.argv[2]), sys.argv[3], sys.argv[4]
            bridge = WindowsBridge(shm_name, shm_size, sem_req_name, sem_res_name)
            logger.info(f"Windows worker started with names: {shm_name}, {sem_req_name}, {sem_res_name}")

        # --- 2. Main processing loop ---
        while True:
            try:
                logger.info("[Worker] Waiting for request...")
                request = bridge.read_request()
                logger.info(f"[Worker] Processing script: {request.script_name}")

                script_full_path = os.path.join(os.path.dirname(__file__), request.script_name)
                if not os.path.isfile(script_full_path):
                    raise FileNotFoundError(f"Script file not found: {script_full_path}")
                
                script_dir = os.path.dirname(script_full_path)
                os.chdir(script_dir)
                if script_dir not in sys.path:
                    sys.path.insert(0, script_dir)

                runpy.run_path(script_full_path, init_globals={
                    "bridge": bridge,
                    "request": request
                })

            except Exception as e:
                logger.error(f"[Worker] Execution failed: {str(e)}")
                bridge.writeResponseAsJSON({'wpbError': f"Execution failed: {str(e)}"}, success=False)
            finally:
                bridge.signal_completion()
                gc.collect()

    except ConnectionError as e:
        logger.error(f"[Worker] IPC Connection Error: {e}")
    except Exception as e:
        logger.error(f"[Worker] Unhandled startup error: {e}")

if __name__ == '__main__':
    main()

