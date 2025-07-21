#ifndef CORETOOLS_MACROS_H_
#define CORETOOLS_MACROS_H_

#define _STR_HELPER(...) #__VA_ARGS__
#define STRINGIFY(...)   _STR_HELPER(__VA_ARGS__)

#define _MERGE_HELPER(a, b) a##b
#define MERGE(a, b)         _MERGE_HELPER(a, b)

#endif
