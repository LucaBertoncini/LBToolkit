# main.py
from utils import format_output
from lb_logger import logger
import config

def main(bridge, request):
    logger.info(f"Started with request {request}")
    params = bridge.getPayloadAsJSON()
    logger.info(f"Params: {params}")

    msg = params.get("custom_message", config.DEFAULT_MESSAGE)
    response = format_output(msg)
    bridge.writeResponseAsJSON({ "output": response }, True)

main(bridge, request)

