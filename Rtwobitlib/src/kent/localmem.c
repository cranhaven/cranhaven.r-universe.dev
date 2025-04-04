/* LocalMem.c - local memory routines. 
 * 
 * These routines are meant for the sort of scenario where
 * a lot of little to medium size pieces of memory are
 * allocated, and then disposed of all at once.
 *
 * This file is copyright 2002 Jim Kent, but license is hereby
 * granted for all use - public, private or commercial. */


#include "common.h"
#include "localmem.h"


struct lm
    {
    struct lmBlock *blocks;
    size_t blockSize;
    size_t allignMask;
    size_t allignAdd;
    boolean doMemoryAllocs; // if true, do our own memory allocs, otherwise use passed in pointer
    };

struct lmBlock
    {
    struct lmBlock *next;
    char *free;
    char *end;
    char *extra;
    };

static struct lmBlock *newBlock(struct lm *lm, size_t reqSize)
/* Allocate a new block of at least reqSize */
{
size_t size = (reqSize > lm->blockSize ? reqSize : lm->blockSize);
size_t fullSize = size + sizeof(struct lmBlock);
struct lmBlock *mb = needLargeZeroedMem(fullSize);
if (mb == NULL)
    errAbort("Couldn't allocate %lld bytes", (long long)fullSize);
mb->free = (char *)(mb+1);
mb->end = ((char *)mb) + fullSize;
mb->next = lm->blocks;
lm->blocks = mb;
return mb;
}

struct lm *lmGuts(int blockSize, void *mem)
/* Create a local memory pool. */
{
struct lm *lm;
int aliSize = sizeof(long);
if (aliSize < sizeof(double))
    aliSize = sizeof(double);
if (aliSize < sizeof(void *))
    aliSize = sizeof(void *);
lm = needMem(sizeof(*lm));
lm->blocks = NULL;
if (blockSize <= 0)
    blockSize = (1<<14);    /* 16k default. */
lm->blockSize = blockSize;
lm->allignAdd = (aliSize-1);
lm->allignMask = ~lm->allignAdd;
if (mem != NULL)
    {
    lm->doMemoryAllocs = FALSE;
    struct lmBlock *mb = mem;
    mb->free = (char *)(mb+1);
    mb->end = ((char *)mb) + blockSize;
    mb->next = lm->blocks;
    lm->blocks = mb;
    }
else
    {
    lm->doMemoryAllocs = TRUE;
    newBlock(lm, blockSize);
    }

return lm;
}

struct lm *lmInit(int blockSize)
/* Create a local memory pool. */
{
return lmGuts(blockSize, NULL);
}

void lmCleanup(struct lm **pLm)
/* Clean up a local memory pool. */
{
    struct lm *lm = *pLm;
    if (lm == NULL)
        return;
    *pLm = NULL;
    slFreeList(&lm->blocks);
    freeMem(lm);
}

void *lmAlloc(struct lm *lm, size_t size)
/* Allocate memory from local pool. */
{
struct lmBlock *mb = lm->blocks;
void *ret;
size_t memLeft = mb->end - mb->free;
if (memLeft < size)
    {
    if (lm->doMemoryAllocs)
        mb = newBlock(lm, size);
    else
        errAbort("attempted local memory alloc in fixed size allocator");
    }

ret = mb->free;
mb->free += ((size+lm->allignAdd)&lm->allignMask);
if (mb->free > mb->end)
    mb->free = mb->end;
return ret;
}

