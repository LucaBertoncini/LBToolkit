import traceback
import datetime
import inspect
import os

# Variabile globale interna
IS_WEB_ENV = True  # di default assume ambiente web

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
        if IS_WEB_ENV:
            return  # Salta la scrittura se siamo in ambiente web
        log_entry = f"[{self._timestamp()}] [{self._caller()}] {level}: {message}\n"
        try:
            with open(self.log_file, 'a', encoding='utf-8') as f:
                f.write(log_entry)
        except Exception:
            pass  # Silenziosamente ignora errori di scrittura

    def info(self, message):
        self._write_to_file("ℹ️ INFO", message)

    def warning(self, message):
        self._write_to_file("⚠️ WARNING", message)

    def error(self, exception):
        self._write_to_file("❌ ERROR", str(exception))
        self._write_to_file("❌ TRACE", traceback.format_exc())

# Istanza globale
logger = Logger()

# Funzione per modificare IS_WEB_ENV
def set_web_env(value: bool):
    global IS_WEB_ENV
    IS_WEB_ENV = value

