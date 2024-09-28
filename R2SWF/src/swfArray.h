/*
  Resizeable arrays. Mainly used to store SWF objects and destroy
  them after writing movie to file.
*/

#ifndef SWFARRAY_H_INCLUDED
#define SWFARRAY_H_INCLUDED

#include <ming.h>
#include <blocks/error.h>
#include <stdlib.h>


typedef void* SWFObject;

struct SWFArray_s {
    SWFObject *data;
    int len;
    int alloc_len;
};

typedef struct SWFArray_s* SWFArray;

SWFArray newSWFArray(int initialSize);
void SWFArray_append(SWFArray array, SWFObject obj);
/* 
    Most SWF objects can be destroyed automatically by calling
    Ming_collectGarbage(), but SWFFillStyle has to be destroyed
    manually.
*/
void destroySWFFillStyleArray(SWFArray array);

#endif /* SWFARRAY_H_INCLUDED */
