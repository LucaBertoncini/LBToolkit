# echo_test.py
def main(bridge, request):
    params = request.get_json()
    bridge.write_json(True, {"echo":params})

# ⚠️ Needed for LBWebPyBridge    
main(bridge, request)

