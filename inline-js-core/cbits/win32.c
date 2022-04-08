#include <fcntl.h>
#include <stdbool.h>
#include <stdio.h>
#include <windows.h>

bool inline_js_mkNamedPipe(bool pipe_inbound, char *pipe_name,
                           size_t pipe_name_length, HANDLE *pipe_handle) {
  GUID guid;
  if (CoCreateGuid(&guid) != S_OK)
    return false;

  RPC_CSTR guidStr;
  if (UuidToStringA((UUID *)&guid, &guidStr) != S_OK)
    return false;

  if (snprintf(pipe_name, pipe_name_length, "\\\\.\\pipe\\inline-js:%s",
               guidStr) <= 0) {
    RpcStringFreeA(&guidStr);
    return false;
  }

  RpcStringFreeA(&guidStr);

  *pipe_handle = CreateNamedPipeA(
      pipe_name,
      (pipe_inbound ? PIPE_ACCESS_INBOUND : PIPE_ACCESS_OUTBOUND) |
          FILE_FLAG_FIRST_PIPE_INSTANCE | FILE_FLAG_OVERLAPPED,
      PIPE_TYPE_BYTE | PIPE_READMODE_BYTE | PIPE_WAIT |
          PIPE_REJECT_REMOTE_CLIENTS,
      1, 65536, 65536, 0, NULL);

  return *pipe_handle != INVALID_HANDLE_VALUE;
}
