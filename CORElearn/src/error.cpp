/********************************************************************
*
*   Name:                modul error
*
*   Description:  reports errors in uniform way throughout the system
*
*********************************************************************/


#include <cstdio>
#include <cstdlib>

#include "general.h"
#include "error.h"

using namespace std ;

void merror(const char* Msg1, const char* Msg2)
{
#if defined(R_PORT)
    Rprintf("\nERROR in CORElearn: %s %s\n", Msg1, Msg2) ;
#else
    fprintf(stderr, "\nERROR: %s %s\n", Msg1, Msg2) ;
    fflush(stderr) ;
#endif
}

void stop(const char* Msg1, const char* Msg2)
{
#if defined(R_PORT)
    Rprintf("\nFATAL ERROR in CORElearn: %s %s\n", Msg1, Msg2) ;
#else
    fprintf(stderr, "\nFATAL ERROR: %s %s\n", Msg1, Msg2) ;
    fflush(stderr) ;
    exit(1);
#endif
}
