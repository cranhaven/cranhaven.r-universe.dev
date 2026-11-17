// ### This file is part of 'PGRdup' package for R.

// ### Copyright (C) 2014-2020, ICAR-NBPGR.
// #
// # PGRdup is free software: you can redistribute it and/or modify it
// # under the terms of the GNU General Public License as published by
// # the Free Software Foundation, either version 2 of the License, or
// # (at your option) any later version.
// #
// # PGRdup is distributed in the hope that it will be useful, but
// # WITHOUT ANY WARRANTY; without even the implied warranty of
// # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// # GNU General Public License for more details.
// #
// #  A copy of the GNU General Public License is available at
// #  http://www.r-project.org/Licenses/


/*
double_metaphone.* are based on the perl Metaphone module developed
by Maurice Aubrey
Here is the original license info:

--------------------------------------------------------------------------------
DESCRIPTION

This module implements a "sounds like" algorithm developed
by Lawrence Philips which he published in the June, 2000 issue
of C/C++ Users Journal. Double Metaphone is an improved
version of Philips' original Metaphone algorithm.

COPYRIGHT

Copyright 2000, Maurice Aubrey <maurice@hevanet.com>.
All rights reserved.
This code is based heavily on the C++ implementation by
Lawrence Philips and incorporates several bug fixes courtesy
of Kevin Atkinson <kevina@users.sourceforge.net>.
This module is free software; you may redistribute it and/or
modify it under the same terms as Perl itself.
--------------------------------------------------------------------------------
*/

#ifndef DOUBLE_METAPHONE__H
#define DOUBLE_METAPHONE__H


typedef struct
{
    char *str;
    int length;
    int bufsize;
    int free_string_on_destroy;
}
metastring;      


void DoubleMetaphone(char *str, char **codes,char **codes1);
void fdouble_metaphone(char** p_input, char** p1_output, char** p2_output, int* tot_length);


#endif /* DOUBLE_METAPHONE__H */
