# Prerequisites

This document lists the required software and libraries to use the various components of the LBToolkit, particularly the language wrappers.

## Python Wrapper

-   **Python Version:** Python 3.8 or newer is recommended.
-   **Installation:** No external packages are required for the basic functionality. The bridge uses standard Python libraries.

To check your Python version, run:
```bash
python3 --version
```

## Node.js Wrapper

-   **Node.js Version:** Node.js 14.x or newer is recommended.
-   **pywin32**: Required for Pacal - Node.js communication via Named Pipes on Windows.
    To install:
    ```bash
    pip install pywin32
    ```
	
To check your Node.js version, run:
```bash
node --version
```

