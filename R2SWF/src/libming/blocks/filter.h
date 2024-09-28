#ifndef __FILTER_H
#define __FILTER_H

#include "output.h"

int SWFFilter_testBlockType(int type);

void
SWFOutput_writeSWFFilter(SWFOutput out, SWFFilter filter);

struct SWFFilterList_s
{
        int numFilter;
        SWFFilter *filter;
};

typedef struct SWFFilterList_s * SWFFilterList;

SWFFilterList newSWFFilterList(void);
void SWFFilterList_add(SWFFilterList list, SWFFilter filter);
void SWFOutput_writeFilterList(SWFOutput out, SWFFilterList list);
void destroySWFFilterList(SWFFilterList list);
#endif
