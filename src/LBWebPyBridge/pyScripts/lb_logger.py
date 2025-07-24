# common/lb_logger.py

import traceback
import datetime
import inspect
import os

# Variabile globale interna
IS_WEB_ENV = True  # di default assume ambiente web

class Logger:
    def __init__(self):
        self.enable_console = not IS_WEB_ENV

    def _timestamp(self):
        return datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")

    def _caller(self):
        frame = inspect.stack()[2]
        filename = os.path.basename(frame.filename)
        return filename

    def info(self, message):
        if self.enable_console:
            print(f"[{self._timestamp()}] [{self._caller()}] ℹ️ INFO: {message}")

    def warning(self, message):
        if self.enable_console:
            print(f"[{self._timestamp()}] [{self._caller()}] ⚠️ WARNING: {message}")

    def error(self, exception):
        if self.enable_console:
            print(f"[{self._timestamp()}] [{self._caller()}] ❌ ERROR: {exception}")
            traceback.print_exc()

# Istanza globale
logger = Logger()

# Funzione per modificare IS_WEB_ENV
def set_web_env(value: bool):
    global IS_WEB_ENV
    IS_WEB_ENV = value
    logger.enable_console = not IS_WEB_ENV

