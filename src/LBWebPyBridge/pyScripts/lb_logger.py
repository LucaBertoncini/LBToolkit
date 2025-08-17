import traceback
import datetime
import inspect
import os

IS_LOG_DISABLED = True

class Logger:
    def __init__(self):
        self.log_dir = os.path.join(os.path.dirname(__file__), '..', 'logs')
        self.log_file = os.path.join(self.log_dir, 'WebPyBridge.log')
        self._ensure_log_directory()

    def _ensure_log_directory(self):
        os.makedirs(self.log_dir, exist_ok=True)

    def _timestamp(self):
        return datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")

    def _caller(self):
        frame = inspect.stack()[2]
        filename = os.path.basename(frame.filename)
        return filename

    def _write_to_file(self, level: str, message: str):
        if IS_LOG_DISABLED:
            return  # Salta la scrittura se siamo in ambiente web
        log_entry = f"[{self._timestamp()}] [{self._caller()}] {level}: {message}\n"
        try:
            with open(self.log_file, 'a', encoding='utf-8') as f:
                f.write(log_entry)
        except Exception:
            pass  # Silenziosamente ignora errori di scrittura

    def info(self, message):
        self._write_to_file("#INF#", message)

    def warning(self, message):
        self._write_to_file("#WRN#", message)

    def error(self, exception):
        self._write_to_file("#ERR#", str(exception))
        self._write_to_file("#TRACE#", traceback.format_exc())

# Istanza globale
logger = Logger()

# Funzione per modificare IS_WEB_ENV
def disable_logger(value: bool):
    global IS_LOG_DISABLED
    IS_LOG_DISABLED = value

