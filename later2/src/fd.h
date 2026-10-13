#ifndef _LATER2_FD_H_
#define _LATER2_FD_H_

#ifdef _WIN32
#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0600 // so R <= 4.1 can find WSAPoll() on Windows
#endif
#include <winsock2.h>
#else
#include <poll.h>
#endif

#ifdef _WIN32
#define LATER2_POLL_FUNC WSAPoll
#define LATER2_NFDS_T ULONG
#else
#define LATER2_POLL_FUNC poll
#define LATER2_NFDS_T nfds_t
#endif

#endif // _LATER2_FD_H_
