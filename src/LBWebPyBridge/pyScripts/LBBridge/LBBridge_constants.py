import sys

# Trigger values
TRIGGER_REQUEST   = 19     # Sent by Pascal to activate worker
TRIGGER_RESPONSE  = 175    # Sent by Python to signal response is ready

# Status codes
STATUS_SUCCESS    = 1      # Execution OK
STATUS_FAILURE    = 0      # Execution failed

# Endianness for struct formatting
STRUCT_ENDIAN     = '<' if sys.byteorder == 'little' else '>'

# Header formats
HEADER_LAYOUT_IN  = 'BI'   # CodeLen, ParamsLen
HEADER_LAYOUT_OUT = 'BBI'  # Ignored trigger, Status, Length

# Calculated sizes
HEADER_FORMAT_IN  = STRUCT_ENDIAN + HEADER_LAYOUT_IN
HEADER_FORMAT_OUT = STRUCT_ENDIAN + HEADER_LAYOUT_OUT

import struct
HEADER_SIZE_IN    = struct.calcsize(HEADER_FORMAT_IN)
HEADER_SIZE_OUT   = struct.calcsize(HEADER_FORMAT_OUT)

# Fallback message
MAX_ERROR_MESSAGE = b"Out of memory"

