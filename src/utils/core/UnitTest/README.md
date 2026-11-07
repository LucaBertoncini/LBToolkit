# 🧪 LBToolkit Unit Tests

This directory contains structured and isolated unit tests for verifying the behavior and reliability of key modules within `LBToolkit`.

Each test is built using [FPCUnit](https://wiki.freepascal.org/fpcunit) and organized by functional domain.


## 🗓️ LBCalendar Tests

📍 Location: `/src/utils/UnitTest/LBCalendar/uLBCalendarTest.pas`

These tests validate:

- Accurate conversion between UTC and local time
- Historical daylight saving time transitions in Italy (from 1940 onward)
- Integration with parsing and decoding logic (`decodeStringDatetime`)
- Synchronization via `ReReadLocalTime` (Linux systems)

## 📡 EventsManager Tests

📍 Location: `/src/utils/UnitTest/EventsManager/`

### 🔸 `uEventsManagerTest.pas`

Validates base functionality:

- Registration and dispatch of named events (`emm_Events`)
- Triggering C-style callbacks (`emm_Callbacks`)
- Single callback behavior (`emm_EventsSingleCallback`)
- Listener removal and disabling

### 🔸 `uEventsManagerLifecycleTest.pas`

Advanced testing of concurrency and lifecycle:

- Multi-threaded stress tests (40 concurrent event calls)
- Automatic deregistration of listeners on owner destruction
- Event raise during active listener removal
- Validation of internal listener count consistency

## 🔍 How to Run

All tests are compatible with Lazarus and `fpTest`.  
They can be run individually or integrated into automated workflows.

