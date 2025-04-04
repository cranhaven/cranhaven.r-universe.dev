/* dnaSeq.c - stuff to manage DNA sequences. 
 *
 * This file is copyright 2002 Jim Kent, but license is hereby
 * granted for all use - public, private or commercial. */

#include "common.h"
#include "dnaseq.h"
#include "bits.h"
#include "hash.h"
#include "obscure.h"



struct dnaSeq *newDnaSeq(DNA *dna, int size, char *name)
/* Create a new DNA seq. */
{
struct dnaSeq *seq;

seq = needMem(sizeof(*seq));
if (name != NULL)
    seq->name = cloneString(name);
seq->dna = dna;
seq->size = size;
seq->mask = NULL;
return seq;
}

void freeDnaSeq(struct dnaSeq **pSeq)
/* Free up DNA seq. (And unlink underlying resource node.) */
{
struct dnaSeq *seq = *pSeq;
if (seq == NULL)
    return;
freeMem(seq->name);
freeMem(seq->dna);
bitFree(&seq->mask);
freez(pSeq);
}

void freeDnaSeqList(struct dnaSeq **pSeqList)
/* Free up list of DNA sequences. */
{
struct dnaSeq *seq, *next;

for (seq = *pSeqList; seq != NULL; seq = next)
    {
    next = seq->next;
    freeDnaSeq(&seq);
    }
*pSeqList = NULL;
}

char *dnaSeqCannibalize(struct dnaSeq **pSeq)
/* Return the already-allocated dna string and free the dnaSeq container. */
{
char *seq = NULL;
if (pSeq && *pSeq)
    {
    struct dnaSeq *dnaSeq = *pSeq;
    seq = dnaSeq->dna;
    dnaSeq->dna = NULL;
    freeDnaSeq(pSeq);
    }
return seq;
}

boolean seqIsLower(bioSeq *seq)
/* Return TRUE if sequence is all lower case. */
{
int size = seq->size, i;
char *poly = seq->dna;
for (i=0; i<size; ++i)
    if (!islower(poly[i]))
        return FALSE;
return TRUE;
}

boolean seqIsDna(bioSeq *seq)
/* Make educated guess whether sequence is DNA or protein. */
{
return isDna(seq->dna, seq->size);
}


aaSeq *translateSeqN(struct dnaSeq *inSeq, unsigned offset, unsigned inSize, boolean stop)
/* Return a translated sequence.  Offset is position of first base to
 * translate. If size is 0 then use length of inSeq. */
{
aaSeq *seq;
DNA *dna = inSeq->dna;
AA *pep, aa;
int i, lastCodon;
int actualSize = 0;

assert(offset <= inSeq->size);
if ((inSize == 0) || (inSize > (inSeq->size - offset)))
    inSize = inSeq->size - offset;
lastCodon = offset + inSize - 3;

AllocVar(seq);
seq->dna = pep = needLargeMem(inSize/3+1);
for (i=offset; i <= lastCodon; i += 3)
    {
    aa = lookupCodon(dna+i);
    if (aa == 0)
	{
        if (stop)
	    break;
	else
	    aa = 'Z';
	}
    *pep++ = aa;
    ++actualSize;
    }
*pep = 0;
assert(actualSize <= inSize/3+1);
seq->size = actualSize;
seq->name = cloneString(inSeq->name);
return seq;
}

aaSeq *translateSeq(struct dnaSeq *inSeq, unsigned offset, boolean stop)
/* Return a translated sequence.  Offset is position of first base to
 * translate. If stop is TRUE then stop at first stop codon.  (Otherwise 
 * represent stop codons as 'Z'). */
{
return translateSeqN(inSeq, offset, 0, stop);
}

void aaSeqZToX(aaSeq *aa)
/* If seq has a 'Z' for stop codon, possibly followed by other bases, change the 'Z' to an X
 * (compatible with dnautil's aminoAcidTable) and truncate there. */
{
char *p = strchr(aa->dna, 'Z');
if (p)
    {
    *p++ = 'X';
    *p = '\0';
    aa->size = p - aa->dna;
    }
}

bioSeq *whichSeqIn(bioSeq **seqs, int seqCount, char *letters)
/* Figure out which if any sequence letters is in. */
{
aaSeq *seq;
int i;

for (i=0; i<seqCount; ++i)
    {
    seq = seqs[i];
    if (seq->dna <= letters && letters < seq->dna + seq->size)
        return seq;
    }
internalErr();
return NULL;
}

int dnaSeqCmpName(const void *va, const void *vb)
/* Compare to sort based on sequence name. */
{
const struct dnaSeq *a = *((struct dnaSeq **)va);
const struct dnaSeq *b = *((struct dnaSeq **)vb);
return strcmp(a->name, b->name);
}

