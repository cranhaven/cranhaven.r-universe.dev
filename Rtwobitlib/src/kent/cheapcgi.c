/* Routines for getting variables passed in from web page
 * forms via CGI.
 *
 * This file is copyright 2002 Jim Kent, but license is hereby
 * granted for all use - public, private or commercial. */

#include "common.h"
#include "hash.h"
#include "cheapcgi.h"
#include "linefile.h"
#include "errAbort.h"
//#include "filePath.h"
//#include "htmshell.h"
#ifndef GBROWSE
//#include "mime.h"
#endif /* GBROWSE */
#include <signal.h>


#define memmem(hay, haySize, needle, needleSize) \
    memMatch(needle, needleSize, hay, haySize)


/* NOTE: Where in the URL to use which of these functions:
 *
 * Parts of a URL:
 *   protocol://user:password@server.com:port/path/filename?var1=val1&var2=val2
 *
 * Note that a space should only be encoded to a plus and decoded from a plus
 * when dealing with http URLs in the query part of the string,
 * which is the part after the ? above.
 * It should not be used in the rest of the URL.  
 * So in the query string part of a URL, do use cgiEncode/cgiDecode. 
 * And in the rest of the URL, use cgiEncodeFUll/cgiDecodeFull 
 * which do not code space as plus.
 * Since FTP does not use URLs with query parameters, use the Full version.
 */

void cgiDecode(const char *in, char *out, int inLength)
/* Decode from cgi pluses-for-spaces format to normal.
 * Out will be a little shorter than in typically, and
 * can be the same buffer. */
{
char c;
int i;
for (i=0; i<inLength;++i)
    {
    c = *in++;
    if (c == '+')
	*out++ = ' ';
    else if (c == '%')
	{
	unsigned int code;
        if (sscanf(in, "%2x", &code) != 1)
	    code = '?';
	in += 2;
	i += 2;
	*out++ = code;
	}
    else
	*out++ = c;
    }
*out++ = 0;
}

#define BOOLSHAD_EXTRA "class='cbShadow'"

