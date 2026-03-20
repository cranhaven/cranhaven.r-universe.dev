/* glpkCallback.c
   R interface to GLPK.
 
   Copyright (C) 2011-2014 Gabriel Gelius-Dietrich, Dpt. for Bioinformatics,
   Institute for Informatics, Heinrich-Heine-University, Duesseldorf, Germany.
   All right reserved.
   Email: geliudie@uni-duesseldorf.de
 
   This file is part of glpkAPI.
 
   GlpkAPI is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
 
   GlpkAPI is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
 
   You should have received a copy of the GNU General Public License
   along with glpkAPI.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "glpkR.h"

/* for the user callback routine */
/*
struct cbInfo {
    int b;
};
cbInfo.b = 42;
*/

/* this is from the GLPK manual, fit according to your needs */
void glpkCallback(glp_tree *tree, void *info) {
    switch (glp_ios_reason(tree)) { 

        /* Rprintf("transit pointer: %i", info.b); */

        case GLP_ISELECT:
            Rprintf("request for subproblem selection\n");
        break;

        case GLP_IPREPRO:
            Rprintf("request for preprocessing\n");
        break;

        case GLP_IROWGEN:
            Rprintf("request for row generation\n");
        break;

        case GLP_IHEUR:
            Rprintf("request for heuristic solution\n");
        break;

        case GLP_ICUTGEN:
            Rprintf("request for cut generation\n");
        break;

        case GLP_IBRANCH:
            Rprintf("request for branching\n");
        break;

        case GLP_IBINGO:
            Rprintf("better integer solution found\n");
        break;

        default:
        /* ignore call for other reasons */
        break;
    }
    return;
}
