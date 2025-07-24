# error_test.py
def main(bridge, request):
    raise RuntimeError("Intentional test error")

# ⚠️ Needed for LBWebPyBridge
main(bridge, request)
