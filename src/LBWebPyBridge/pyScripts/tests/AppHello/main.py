# main.py

from utils import format_output
import config

def main():
    print(f"Started")
    params = bridge.get_params_as_json(request)
    print(f"Params: {params}")

    msg = params.get("custom_message", config.DEFAULT_MESSAGE)
    response = format_output(msg)
    bridge.write_json(True, { "output": response })

main()

