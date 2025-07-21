# __init__.py
import platform

if platform.system() == "Windows":
    from .bridge_win import LBBridge, Request
else:
    from .bridge_sysv import LBBridge, Request

