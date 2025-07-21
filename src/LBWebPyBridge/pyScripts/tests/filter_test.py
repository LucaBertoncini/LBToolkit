# filter_test.py
params = bridge.get_params_as_json()
user = params.get("user", "").strip()
if user == "":
    bridge.write_json({ "wpbError": "Missing user" })
else:
    bridge.write_json({ "status": "ok", "user": user.upper() })

