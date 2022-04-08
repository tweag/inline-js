#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <unistd.h>

bool mkNamedPipe(char *pipe_name, size_t pipe_name_length) {
  char *env = getenv("TMPDIR");
  char *dest_dir = env ? env : "/tmp";
  if (snprintf(pipe_name, pipe_name_length, "%s/%s", dest_dir,
               "inline-js-XXXXXX") <= 0)
    return false;
  int pipe_fd = mkstemp(pipe_name);
  if (pipe_fd == -1)
    return false;
  if (close(pipe_fd) != 0)
    return false;
  if (unlink(pipe_name) != 0)
    return false;
  return mkfifo(pipe_name, 0600) == 0;
}
