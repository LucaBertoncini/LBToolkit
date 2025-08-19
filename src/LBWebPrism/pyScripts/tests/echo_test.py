# echo_test.py
from lb_logger import logger

def main(bridge, request):
    logger.info(f"Started test echo with request {request}")
    params = bridge.getPayloadAsJSON()
    bridge.writeResponseAsJSON({"echo":params}, True)

# ⚠️ Needed for LBWebPyBridge    
main(bridge, request)

