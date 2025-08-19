# sum_test.py
def main(bridge, request):
    p = bridge.getPayloadAsJSON()
    a, b = p.get("a", 0), p.get("b", 0)
    bridge.writeResponseAsJSON({ "sum": a + b }, True)

# Needed for LBWebPyBridge    
main(bridge, request)

