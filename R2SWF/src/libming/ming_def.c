#include "libming.h"

char* __my__strdup (const char *s)
{
    char *p = malloc(strlen(s) + 1);
    if(p) { strcpy(p, s); }
    return p;
}

SWFDisplayItem SWFMovie_add(SWFMovie movie, SWFBlock block)
{
	SWFMovieBlockType ublock;
	ublock.block = block;
	return SWFMovie_add_internal(movie, ublock);
}

int SWFMovie_replace(SWFMovie movie, SWFDisplayItem item, SWFBlock block)
{
	SWFMovieBlockType ublock;
	ublock.block = block;
	return SWFMovie_replace_internal(movie, item, ublock);
}

