//  by Greg Ridgeway  Copyright (C) 2003
//  License:    GNU GPL (version 2 or later)

#ifndef BUILDINFO_H
#define BUILDINFO_H

    #undef ERROR
    #include <R.h>

    #define erboost_FAILED(hr) ((unsigned long)hr != 0)
    typedef unsigned long erboostRESULT;
    #define erboost_OK 0
    #define erboost_FAIL 1
    #define erboost_INVALIDARG 2
    #define erboost_OUTOFMEMORY 3
    #define erboost_INVALID_DATA 4
    #define erboost_NOTIMPL 5

    #define LEVELS_PER_CHUNK ((unsigned long) 1)

    typedef unsigned long ULONG;
    typedef char *PCHAR;

    // #define NOISY_DEBUG

#endif // BUILDINFO_H
