# sum_test.py
p = bridge.get_params_as_json(request)
a, b = p.get("a", 0), p.get("b", 0)
bridge.write_json(True, { "sum": a + b })

