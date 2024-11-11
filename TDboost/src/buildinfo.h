//  by Greg Ridgeway  Copyright (C) 2003
//  License:    GNU GPL (version 2 or later)

#ifndef BUILDINFO_H
#define BUILDINFO_H

    #undef ERROR
    #include <R.h>

    #define TDboost_FAILED(hr) ((unsigned long)hr != 0)
    typedef unsigned long TDboostRESULT;
    #define TDboost_OK 0
    #define TDboost_FAIL 1
    #define TDboost_INVALIDARG 2
    #define TDboost_OUTOFMEMORY 3
    #define TDboost_INVALID_DATA 4
    #define TDboost_NOTIMPL 5

    #define LEVELS_PER_CHUNK ((unsigned long) 1)

    typedef unsigned long ULONG;
    typedef char *PCHAR;

    // #define NOISY_DEBUG

#endif // BUILDINFO_H
