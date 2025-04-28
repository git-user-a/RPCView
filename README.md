# RPC View
**2025.04.26**

## Application
RPCView is a Windows application designed to simplify the management of remote procedures within the Veterans Health Information Systems and Technology Architecture (VistA).

With RPCView, users can:
- Create, View and modify the definitions of remote procedures (file #8994).
- Register remote procedures in application contexts.
- Execute remote procedures.

RPCView requires the **XWB** and **FMDC** packages to be installed on the server. Custom RPC allows application to review source code of the RPC implementing routine.

The application is designed to work in **Cache, YottaDB, and GT.M** environments and to connect to VistA instances using hash tables `"VistA"`, `"VistA Demo"`, and `"OSHERA"`.

## Disclaimer
The application enables users to view and modify data on both server and client machines. However, changes to the RPC may impact applications relying on it. Errors in the descriptor can render an RPC unusable.

To minimize risk, always test the application on a non-production database instance first and ensure compatibility with your server software version before accessing critical data.

> **Note:** The application is provided **"AS IS" WITH NO WARRANTIES**—the author is not responsible for any data loss or damage that may occur as a result of using this application.

## Privacy Policy
The application does not collect any personal user information. It may generate a log file that records connections to VistA accounts and RPC executions. This log file is stored locally on the machine where the application is running.

## Implementation
RPCView is built with **Embarcadero Delphi 12 (Community Edition)**. It accesses VistA Files via the **FileMan Delphi Components (FMDC v1.0)** library and retrieves RPC implementation details through custom RPC.

Additionally, the following packages are utilized for syntax highlighting and data presentation:
- **SynEdit (TurboPack)**
- **VirtualTree (TurboPack)**

## Credits
Special thanks to the **WorldVistA open-source community** for providing access to test databases, engaging in discussions, and offering invaluable feedback.
