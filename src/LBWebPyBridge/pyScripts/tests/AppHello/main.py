# main.py

from utils import format_output
from lb_logger import logger
import config

def main(bridge, request):
    logger.info(f"Started")
    params = request.get_json()
    logger.info(f"Params: {params}")

    msg = params.get("custom_message", config.DEFAULT_MESSAGE)
    response = format_output(msg)
    bridge.write_json(True, { "output": response })

main(bridge, request)

