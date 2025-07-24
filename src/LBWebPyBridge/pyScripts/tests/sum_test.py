# sum_test.py
def main(bridge, request):
    p = request.get_json()
    a, b = p.get("a", 0), p.get("b", 0)
    bridge.write_json(True, { "sum": a + b })

# ⚠️ Needed for LBWebPyBridge    
main(bridge, request)

