/* glpkAPI.c
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


#include <setjmp.h>
#include "glpkAPI.h"
#include "glpkCallback.h"


static SEXP tagGLPKprob;
static SEXP tagGLPKparm;
static SEXP tagMATHprog;

/* for the user callback routine */
/* static struct cbInfo glpkCallbackInfo; */

/* structure for glpk parameters */
glp_smcp parmS;
glp_iptcp parmI;
glp_iocp parmM;

/* BEGIN code by Ulrich Wittelsbuerger */
struct glpkError {
    int e;
};

static struct glpkError ge;
jmp_buf jenv;

void cleanGLPKerror( struct glpkError * ptr ) {
    if( ptr != 0 ) {
        /* Rprintf("GLPK error: %d\n",ptr->e); */
        Rprintf("GLPK error\n");
    }
    else {
        Rprintf("NULL pointer!\n");
    }
    longjmp(jenv, 1);
}

typedef void (*func)(void *info);
/* END code by Ulrich Wittelsbuerger */


/* -------------------------------------------------------------------------- */
/* Finalizer                                                                  */
/* -------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------- */
/* finalizer for glpk problem objects */
static void glpkProbFinalizer (SEXP lp) {
    if (!R_ExternalPtrAddr(lp)) {
        return;
    }
    else {
        delProb(lp);
    }
}

/* finalizer for MathProg translator workspace */
static void mathProgFinalizer (SEXP wk) {
    if (!R_ExternalPtrAddr(wk)) {
        return;
    }
    else {
        mplFreeWksp(wk);
    }
}


/* -------------------------------------------------------------------------- */
/* help functions                                                             */
/* -------------------------------------------------------------------------- */

/* check for pointer to glpk */
SEXP isGLPKptr(SEXP ptr) {

    SEXP out = R_NilValue;

    if ( (TYPEOF(ptr) == EXTPTRSXP) &&
         (R_ExternalPtrTag(ptr) == tagGLPKprob) ) {
        out = Rf_ScalarLogical(1);
    }
    else {
        out = Rf_ScalarLogical(0);
    }

    return out;
}

/* check for pointer to translator workspace */
SEXP isTRWKSptr(SEXP ptr) {

    SEXP out = R_NilValue;

    if ( (TYPEOF(ptr) == EXTPTRSXP) &&
         (R_ExternalPtrTag(ptr) == tagMATHprog) ) {
        out = Rf_ScalarLogical(1);
    }
    else {
        out = Rf_ScalarLogical(0);
    }

    return out;
}

/* check for NULL pointer */
SEXP isNULLptr(SEXP ptr) {

    SEXP out = R_NilValue;

    if ( (TYPEOF(ptr) == EXTPTRSXP) &&
         (R_ExternalPtrAddr(ptr) == NULL) ) {
        out = Rf_ScalarLogical(1);
    }
    else {
        out = Rf_ScalarLogical(0);
    }

    return out;
}


/* -------------------------------------------------------------------------- */
/* API-Functions                                                              */
/* -------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------- */
/* initialize glpk */
SEXP initGLPK(void) {
    tagGLPKprob = Rf_install("TYPE_GLPK_PROB");
    tagGLPKparm = Rf_install("TYPE_GLPK_PARM");
    tagMATHprog = Rf_install("TYPE_MATH_PROG");
    tagGLPKparm = Rf_install("TYPE_GLPK_PARM");
    return R_NilValue;
}


/* -------------------------------------------------------------------------- */
/* remove problem object */
SEXP delProb(SEXP lp) {

    SEXP out = R_NilValue;
    glp_prob *del = NULL;

    checkProb(lp);

    del = R_ExternalPtrAddr(lp);

    glp_delete_prob(del);
    R_ClearExternalPtr(lp);

    return out;
}


/* -------------------------------------------------------------------------- */
/* erase problem object content */
SEXP eraseProb(SEXP lp) {

    SEXP out = R_NilValue;
    glp_prob *del = NULL;

    checkProb(lp);

    del = R_ExternalPtrAddr(lp);

    glp_erase_prob(del);

    return out;
}


/* -------------------------------------------------------------------------- */
/* copy problem object content */
SEXP copyProb(SEXP lp, SEXP clp, SEXP names) {

    SEXP out = R_NilValue;
    glp_prob *prob = NULL;
    glp_prob *dest = NULL;

    checkProb(lp);
    checkProb(clp);

    prob = R_ExternalPtrAddr(lp);
    dest = R_ExternalPtrAddr(clp);

    glp_copy_prob(dest, prob, Rf_asInteger(names));

    return out;
}


/* -------------------------------------------------------------------------- */
/* create new problem object */
SEXP initProb(SEXP ptrtype) {

    SEXP lpext = R_NilValue;
    SEXP ptr, class;

    glp_prob *lp;

    /* initialize structure for control parameters */
    glp_init_smcp(&parmS);
    glp_init_iptcp(&parmI);
    glp_init_iocp(&parmM);

    /* create problem pointer */
    PROTECT(ptr = Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(ptr, 0, STRING_ELT(ptrtype, 0));

    PROTECT(class = Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(class, 0, Rf_mkChar("glpk_ptr"));

    lp = glp_create_prob();

    lpext = R_MakeExternalPtr(lp, tagGLPKprob, R_NilValue);
    PROTECT(lpext);
    R_RegisterCFinalizerEx(lpext, glpkProbFinalizer, TRUE);
    Rf_setAttrib(ptr, class, lpext);
    Rf_classgets(ptr, class);

    UNPROTECT(3);

    return ptr;
}

/* -------------------------------------------------------------------------- */
/* create new problem object */
/*
SEXP initProb(SEXP ptrtype) {

    SEXP lpext = R_NilValue;
    SEXP ptr, class;

    glp_prob *lp;

glp_smcp parmSa;
glp_iptcp parmIa;
glp_iocp parmMa;
SEXP psn, pin, pmn, psp, pip, pmp;





    glp_init_smcp(&parmSa);
    glp_init_iptcp(&parmIa);
    glp_init_iocp(&parmMa);

    PROTECT(ptr = Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(ptr, 0, STRING_ELT(ptrtype, 0));

PROTECT(psn = Rf_allocVector(STRSXP, 1));
SET_STRING_ELT(psn, 0, Rf_mkChar("simplex"));
PROTECT(pin = Rf_allocVector(STRSXP, 1));
SET_STRING_ELT(pin, 0, Rf_mkChar("interior"));
PROTECT(pmn = Rf_allocVector(STRSXP, 1));
SET_STRING_ELT(pmn, 0, Rf_mkChar("mip"));

psp = R_MakeExternalPtr(&parmSa, tagGLPKparm, R_NilValue);
pip = R_MakeExternalPtr(&parmIa, tagGLPKparm, R_NilValue);
pmp = R_MakeExternalPtr(&parmMa, tagGLPKparm, R_NilValue);
PROTECT(psp);
PROTECT(pip);
PROTECT(pmp);

    PROTECT(class = Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(class, 0, Rf_mkChar("glpk_ptr"));

    lp = glp_create_prob();

    lpext = R_MakeExternalPtr(lp, tagGLPKprob, R_NilValue);
    PROTECT(lpext);
    R_RegisterCFinalizerEx(lpext, glpkProbFinalizer, TRUE);
    Rf_setAttrib(ptr, class, lpext);
    Rf_classgets(ptr, class);

Rf_setAttrib(ptr, psn, psp);
Rf_setAttrib(ptr, pin, pip);
Rf_setAttrib(ptr, pmn, pmp);


    UNPROTECT(9);

    return ptr;
}
*/


/* -------------------------------------------------------------------------- */
/* set problem name */
SEXP setProbName(SEXP lp, SEXP pname) {

    SEXP out = R_NilValue;

    const char *rpname;

    checkProb(lp);

    if (pname == R_NilValue) {
        rpname = NULL;
    }
    else {
        rpname = CHAR(STRING_ELT(pname, 0));
    }

    glp_set_prob_name(R_ExternalPtrAddr(lp), rpname);

    return out;

}


/* -------------------------------------------------------------------------- */
/* get problem name */
SEXP getProbName(SEXP lp) {

    SEXP out = R_NilValue;

    const char *rpname = NULL;

    checkProb(lp);

    rpname = glp_get_prob_name(R_ExternalPtrAddr(lp));

    if (rpname != NULL) {
        out = Rf_mkString(rpname);
    }

    return out;

}


/* -------------------------------------------------------------------------- */
/* set objective function name */
SEXP setObjName(SEXP lp, SEXP oname) {

    SEXP out = R_NilValue;

    const char *roname;
    if (oname == R_NilValue) {
        roname = NULL;
    }
    else {
        roname = CHAR(STRING_ELT(oname, 0));
    }

    checkProb(lp);

    glp_set_obj_name(R_ExternalPtrAddr(lp), roname);

    return out;

}


/* -------------------------------------------------------------------------- */
/* get objective function name */
SEXP getObjName(SEXP lp) {

    SEXP out = R_NilValue;

    const char *roname;

    checkProb(lp);

    roname = glp_get_obj_name(R_ExternalPtrAddr(lp));

    if (roname != NULL) {
        out = Rf_mkString(roname);
    }

    return out;

}


/* -------------------------------------------------------------------------- */
/* create name index */
SEXP createIndex(SEXP lp) {

    SEXP out = R_NilValue;

    checkProb(lp);

    glp_create_index(R_ExternalPtrAddr(lp));

    return out;

}


/* -------------------------------------------------------------------------- */
/* delete name index */
SEXP deleteIndex(SEXP lp) {

    SEXP out = R_NilValue;

    checkProb(lp);

    glp_delete_index(R_ExternalPtrAddr(lp));

    return out;

}


/* -------------------------------------------------------------------------- */
/* create parameter structure for simplex */
SEXP setDefaultSmpParm(void) {

    SEXP parmext = R_NilValue;

    glp_init_smcp(&parmS);

    return parmext;

}


/* -------------------------------------------------------------------------- */
/* create parameter structure for interior */
SEXP setDefaultIptParm(void) {

    SEXP parmext = R_NilValue;

    glp_init_iptcp(&parmI);

    return parmext;

}


/* -------------------------------------------------------------------------- */
/* create parameter structure for MIP */
SEXP setDefaultMIPParm(void) {

    SEXP parmext = R_NilValue;

    glp_init_iocp(&parmM);

    return parmext;

}


/* -------------------------------------------------------------------------- */
/* set simplex control parameters */
SEXP setSimplexParm(SEXP npari, SEXP pari, SEXP vali,
                    SEXP npard, SEXP pard, SEXP vald) {

    SEXP parmext = R_NilValue;

    int *rpari;
    int *rvali;

    int *rpard;
    double *rvald;

    int i, d;

    if (Rf_asInteger(npari) == 0) {
        rpari = NULL;
        rvali = NULL;
        /* parmS.tm_lim = 10000; */
    }
    else {
        rpari = INTEGER(pari);
        rvali = INTEGER(vali);

        for (i = 0; i < Rf_asInteger(npari); i++) {
            /* Rprintf("par: %i  val: %i\n", rpari[i], rvali[i]); */
            switch (rpari[i]) {
                case 101:
                    parmS.msg_lev = rvali[i];
                    break;
                case 102:
                    parmS.meth = rvali[i];
                    break;
                case 103:
                    parmS.pricing = rvali[i];
                    break;
                case 104:
                    parmS.r_test = rvali[i];
                    break;
                case 105:
                    parmS.it_lim = rvali[i];
                    break;
                case 106:
                    parmS.tm_lim = rvali[i];
                    break;
                case 107:
                    parmS.out_frq = rvali[i];
                    break;
                case 108:
                    parmS.out_dly = rvali[i];
                    break;
                case 109:
                    parmS.presolve = rvali[i];
                    break;
                default:
                    Rf_warning("Unknown integer simplex parameter: %i!", rpari[i]);
                    break;
            }
        }

    }

    if (Rf_asInteger(npard) == 0) {
        rpard = NULL;
        rvald = NULL;
    }
    else {
        rpard = INTEGER(pard);
        rvald = REAL(vald);

        for (d = 0; d < Rf_asInteger(npard); d++) {
            /* Rprintf("par: %i  val: %i\n", rpard[d], rvald[d]); */
            switch (rpard[d]) {
                case 201:
                    parmS.tol_bnd = rvald[d];
                    break;
                case 202:
                    parmS.tol_dj = rvald[d];
                    break;
                case 203:
                    parmS.tol_piv = rvald[d];
                    break;
                case 204:
                    parmS.obj_ll = rvald[d];
                    break;
                case 205:
                    parmS.obj_ul = rvald[d];
                    break;
                default:
                    Rf_warning("Unknown double simplex parameter: %i!", rpard[d]);
                    break;
            }
        }

    }

    return parmext;

}


/* -------------------------------------------------------------------------- */
/* set interior control parameters */
SEXP setInteriorParm(SEXP npari, SEXP pari, SEXP vali) {

    SEXP parmext = R_NilValue;

    int *rpari = INTEGER(pari);
    int *rvali = INTEGER(vali);

    int i;

    for (i = 0; i < Rf_asInteger(npari); i++) {
        switch (rpari[i]) {
            case 101:
                parmI.msg_lev = rvali[i];
                break;
            case 301:
                parmI.ord_alg = rvali[i];
                break;
            default:
                Rf_warning("Unknown interior parameter: %i!", rpari[i]);
                break;
        }
    }

    return parmext;
}


/* -------------------------------------------------------------------------- */
/* set MIP control parameters */
SEXP setMIPParm(SEXP npari, SEXP pari, SEXP vali,
                SEXP npard, SEXP pard, SEXP vald) {

    SEXP parmext = R_NilValue;

    int *rpari;
    int *rvali;

    int *rpard;
    double *rvald;

    int i, d;

    if (Rf_asInteger(npari) == 0) {
        rpari = NULL;
        rvali = NULL;
    }
    else {
        rpari = INTEGER(pari);
        rvali = INTEGER(vali);

        for (i = 0; i < Rf_asInteger(npari); i++) {
            switch (rpari[i]) {
                case 101:
                    parmM.msg_lev = rvali[i];
                    break;
                case 106:
                    parmM.tm_lim = rvali[i];
                    break;
                case 107:
                    parmM.out_frq = rvali[i];
                    break;
                case 108:
                    parmM.out_dly = rvali[i];
                    break;
                case 109:
                    parmM.presolve = rvali[i];
                    break;
                case 601:
                    parmM.br_tech = rvali[i];
                    break;
                case 602:
                    parmM.bt_tech = rvali[i];
                    break;
                case 603:
                    parmM.pp_tech = rvali[i];
                    break;
                case 604:
                    parmM.fp_heur = rvali[i];
                    break;
                case 605:
                    parmM.gmi_cuts = rvali[i];
                    break;
                case 606:
                    parmM.mir_cuts = rvali[i];
                    break;
                case 607:
                    parmM.cov_cuts = rvali[i];
                    break;
                case 608:
                    parmM.clq_cuts = rvali[i];
                    break;
                case 609:
                    parmM.cb_size = rvali[i];
                    break;
                case 610:
                    parmM.binarize = rvali[i];
                    break;
                case 651:
                    if (rvali[i] == 0) {
                        parmM.cb_func = NULL;
                        /* parmM.cb_info = NULL; */
                    }
                    else {
                        parmM.cb_func = glpkCallback;
                        /* parmM.cb_info = &glpkCallbackInfo; */
                    }
                    break;
                default:
                    Rf_warning("Unknown integer MIP parameter: %i!", rpari[i]);
                    break;
            }
        }

    }

    if (Rf_asInteger(npard) == 0) {
        rpard = NULL;
        rvald = NULL;
    }
    else {
        rpard = INTEGER(pard);
        rvald = REAL(vald);

        for (d = 0; d < Rf_asInteger(npard); d++) {
            switch (rpard[d]) {
                case 701:
                    parmM.tol_int = rvald[d];
                    break;
                case 702:
                    parmM.tol_obj = rvald[d];
                    break;
                case 703:
                    parmM.mip_gap = rvald[d];
                    break;
                default:
                    Rf_warning("Unknown double MIP parameter: %i!", rpard[d]);
                    break;
            }
        }

    }

    return parmext;

}


/* -------------------------------------------------------------------------- */
/* get simplex control parameters */
SEXP getSimplexParm(void) {

    SEXP listv   = R_NilValue;
    SEXP parmext = R_NilValue;
    SEXP intids  = R_NilValue;
    SEXP dbids   = R_NilValue;

    SEXP pint  = R_NilValue;
    SEXP pdb   = R_NilValue;

    PROTECT(pint = Rf_allocVector(INTSXP, 9));
    PROTECT(pdb  = Rf_allocVector(REALSXP, 5));

    INTEGER(pint)[0] = parmS.msg_lev;
    INTEGER(pint)[1] = parmS.meth;
    INTEGER(pint)[2] = parmS.pricing;
    INTEGER(pint)[3] = parmS.r_test;
    INTEGER(pint)[4] = parmS.it_lim;
    INTEGER(pint)[5] = parmS.tm_lim;
    INTEGER(pint)[6] = parmS.out_frq;
    INTEGER(pint)[7] = parmS.out_dly;
    INTEGER(pint)[8] = parmS.presolve;

    REAL(pdb)[0] = parmS.tol_bnd;
    REAL(pdb)[1] = parmS.tol_dj;
    REAL(pdb)[2] = parmS.tol_piv;
    REAL(pdb)[3] = parmS.obj_ll;
    REAL(pdb)[4] = parmS.obj_ul;

    PROTECT(intids = Rf_allocVector(STRSXP, 9));
    SET_STRING_ELT(intids, 0, Rf_mkChar("msg_lev"));
    SET_STRING_ELT(intids, 1, Rf_mkChar("meth"));
    SET_STRING_ELT(intids, 2, Rf_mkChar("pricing"));
    SET_STRING_ELT(intids, 3, Rf_mkChar("r_test"));
    SET_STRING_ELT(intids, 4, Rf_mkChar("it_lim"));
    SET_STRING_ELT(intids, 5, Rf_mkChar("tm_lim"));
    SET_STRING_ELT(intids, 6, Rf_mkChar("out_frq"));
    SET_STRING_ELT(intids, 7, Rf_mkChar("out_dly"));
    SET_STRING_ELT(intids, 8, Rf_mkChar("presolve"));

    PROTECT(dbids = Rf_allocVector(STRSXP, 5));
    SET_STRING_ELT(dbids, 0, Rf_mkChar("tol_bnd"));
    SET_STRING_ELT(dbids, 1, Rf_mkChar("tol_dj"));
    SET_STRING_ELT(dbids, 2, Rf_mkChar("tol_piv"));
    SET_STRING_ELT(dbids, 3, Rf_mkChar("obj_ll"));
    SET_STRING_ELT(dbids, 4, Rf_mkChar("obj_ul"));

    Rf_setAttrib(pint, R_NamesSymbol, intids);
    Rf_setAttrib(pdb, R_NamesSymbol, dbids);

    PROTECT(parmext = Rf_allocVector(VECSXP, 2));
    SET_VECTOR_ELT(parmext, 0, pint);
    SET_VECTOR_ELT(parmext, 1, pdb);

    PROTECT(listv = Rf_allocVector(STRSXP, 2));
    SET_STRING_ELT(listv, 0, Rf_mkChar("integer"));
    SET_STRING_ELT(listv, 1, Rf_mkChar("double"));

    Rf_setAttrib(parmext, R_NamesSymbol, listv);

    UNPROTECT(6);

    return parmext;

}


/* -------------------------------------------------------------------------- */
/* get interior control parameters */
SEXP getInteriorParm(void) {

    SEXP listv   = R_NilValue;
    SEXP parmext = R_NilValue;
    SEXP intids  = R_NilValue;
    SEXP pint    = R_NilValue;

    PROTECT(pint = Rf_allocVector(INTSXP, 2));
    INTEGER(pint)[0] = parmI.msg_lev;
    INTEGER(pint)[1] = parmI.ord_alg;

    PROTECT(intids = Rf_allocVector(STRSXP, 2));
    SET_STRING_ELT(intids, 0, Rf_mkChar("msg_lev"));
    SET_STRING_ELT(intids, 1, Rf_mkChar("ord_alg"));

    Rf_setAttrib(pint, R_NamesSymbol, intids);

    PROTECT(parmext = Rf_allocVector(VECSXP, 1));
    SET_VECTOR_ELT(parmext, 0, pint);

    PROTECT(listv = Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(listv, 0, Rf_mkChar("integer"));

    Rf_setAttrib(parmext, R_NamesSymbol, listv);

    UNPROTECT(4);

    return parmext;

}


/* -------------------------------------------------------------------------- */
/* get MIP control parameters */
SEXP getMIPParm(void) {

    SEXP listv   = R_NilValue;
    SEXP parmext = R_NilValue;
    SEXP intids  = R_NilValue;
    SEXP dbids   = R_NilValue;

    SEXP pint  = R_NilValue;
    SEXP pdb   = R_NilValue;

    PROTECT(pint = Rf_allocVector(INTSXP, 16));
    PROTECT(pdb  = Rf_allocVector(REALSXP, 3));

    INTEGER(pint)[0]  = parmM.msg_lev;
    INTEGER(pint)[1]  = parmM.br_tech;
    INTEGER(pint)[2]  = parmM.bt_tech;
    INTEGER(pint)[3]  = parmM.pp_tech;
    INTEGER(pint)[4]  = parmM.fp_heur;
    INTEGER(pint)[5]  = parmM.gmi_cuts;
    INTEGER(pint)[6]  = parmM.mir_cuts;
    INTEGER(pint)[7]  = parmM.cov_cuts;
    INTEGER(pint)[8]  = parmM.clq_cuts;
    INTEGER(pint)[9]  = parmM.tm_lim;
    INTEGER(pint)[10] = parmM.out_frq;
    INTEGER(pint)[11] = parmM.out_dly;
    INTEGER(pint)[12] = parmM.cb_size;
    INTEGER(pint)[13] = parmM.presolve;
    INTEGER(pint)[14] = parmM.binarize;
    if (parmM.cb_func) {
        INTEGER(pint)[15] = 1;
    }
    else {
        INTEGER(pint)[15] = 0;
    }

    REAL(pdb)[0] = parmM.tol_int;
    REAL(pdb)[1] = parmM.tol_obj;
    REAL(pdb)[2] = parmM.mip_gap;

    PROTECT(intids = Rf_allocVector(STRSXP, 16));
    SET_STRING_ELT(intids, 0,  Rf_mkChar("msg_lev"));
    SET_STRING_ELT(intids, 1,  Rf_mkChar("br_tech"));
    SET_STRING_ELT(intids, 2,  Rf_mkChar("bt_tech"));
    SET_STRING_ELT(intids, 3,  Rf_mkChar("pp_tech"));
    SET_STRING_ELT(intids, 4,  Rf_mkChar("fp_heur"));
    SET_STRING_ELT(intids, 5,  Rf_mkChar("gmi_cuts"));
    SET_STRING_ELT(intids, 6,  Rf_mkChar("mir_cuts"));
    SET_STRING_ELT(intids, 7,  Rf_mkChar("cov_cuts"));
    SET_STRING_ELT(intids, 8,  Rf_mkChar("clq_cuts"));
    SET_STRING_ELT(intids, 9,  Rf_mkChar("tm_lim"));
    SET_STRING_ELT(intids, 10, Rf_mkChar("out_frq"));
    SET_STRING_ELT(intids, 11, Rf_mkChar("out_dly"));
    SET_STRING_ELT(intids, 12, Rf_mkChar("cb_size"));
    SET_STRING_ELT(intids, 13, Rf_mkChar("presolve"));
    SET_STRING_ELT(intids, 14, Rf_mkChar("binarize"));
    SET_STRING_ELT(intids, 15, Rf_mkChar("cb_func"));

    PROTECT(dbids = Rf_allocVector(STRSXP, 3));
    SET_STRING_ELT(dbids, 0, Rf_mkChar("tol_int"));
    SET_STRING_ELT(dbids, 1, Rf_mkChar("tol_obj"));
    SET_STRING_ELT(dbids, 2, Rf_mkChar("mip_gap"));

    Rf_setAttrib(pint, R_NamesSymbol, intids);
    Rf_setAttrib(pdb, R_NamesSymbol, dbids);

    PROTECT(parmext = Rf_allocVector(VECSXP, 2));
    SET_VECTOR_ELT(parmext, 0, pint);
    SET_VECTOR_ELT(parmext, 1, pdb);

    PROTECT(listv = Rf_allocVector(STRSXP, 2));
    SET_STRING_ELT(listv, 0, Rf_mkChar("integer"));
    SET_STRING_ELT(listv, 1, Rf_mkChar("double"));

    Rf_setAttrib(parmext, R_NamesSymbol, listv);

    UNPROTECT(6);

    return parmext;

}


/* -------------------------------------------------------------------------- */
/* set optimization direction */
SEXP setObjDir(SEXP lp, SEXP dir) {

    SEXP out = R_NilValue;
    int dr;
    
    checkProb(lp);

    dr = (Rf_asInteger(dir) == GLP_MAX) ? GLP_MAX : GLP_MIN;

    glp_set_obj_dir(R_ExternalPtrAddr(lp), dr);

    return out;
}


/* -------------------------------------------------------------------------- */
/* get optimization direction */
SEXP getObjDir(SEXP lp) {

    SEXP out = R_NilValue;
    int dir = 0;

    checkProb(lp);

    dir = glp_get_obj_dir(R_ExternalPtrAddr(lp));

    out = Rf_ScalarInteger(dir);

    return out;
}


/* -------------------------------------------------------------------------- */
/* add rows to the problem object */
SEXP addRows(SEXP lp, SEXP nrows) {

    SEXP out = R_NilValue;
    int frow = 0;

    checkProb(lp);

    frow = glp_add_rows(R_ExternalPtrAddr(lp), Rf_asInteger(nrows));

    out = Rf_ScalarInteger(frow);

    return out;
}


/* -------------------------------------------------------------------------- */
/* set row name i */
SEXP setRowName(SEXP lp, SEXP i, SEXP rname) {

    SEXP out = R_NilValue;

    const char *rrname;
    if (rname == R_NilValue) {
        rrname = NULL;
    }
    else {
        rrname = CHAR(STRING_ELT(rname, 0));
    }

    checkProb(lp);
    checkRowIndex(lp, i);

    glp_set_row_name(R_ExternalPtrAddr(lp), Rf_asInteger(i), rrname);

    return out;

}


/* -------------------------------------------------------------------------- */
/* set row names */
SEXP setRowsNames(SEXP lp, SEXP i, SEXP rnames) {

    SEXP out = R_NilValue;

    int *ri = INTEGER(i);
    int k, numrn;

    checkProb(lp);
    checkRowIndices(lp, i, NULL);

    if (rnames == R_NilValue) {
        numrn = Rf_length(i);
        for (k = 0; k < numrn; k++) {
            glp_set_row_name(R_ExternalPtrAddr(lp), ri[k], NULL);
        }
    }
    else {
        checkVecLen(Rf_ScalarInteger(Rf_length(i)), rnames);
        numrn = Rf_length(rnames);
        for (k = 0; k < numrn; k++) {
            glp_set_row_name(R_ExternalPtrAddr(lp),
                             ri[k], CHAR(STRING_ELT(rnames, k)));
        }
    }

    return out;

}


/* -------------------------------------------------------------------------- */
/* get row name i */
SEXP getRowName(SEXP lp, SEXP i) {

    SEXP out = R_NilValue;

    const char *rrname;

    checkProb(lp);
    checkRowIndex(lp, i);

    rrname = glp_get_row_name(R_ExternalPtrAddr(lp), Rf_asInteger(i));

    if (rrname != NULL) {
        out = Rf_mkString(rrname);
    }

    return out;

}


/* -------------------------------------------------------------------------- */
/* find row by its name */
SEXP findRow(SEXP lp, SEXP rname) {

    SEXP out = R_NilValue;
    int rind;

    const char *rrname = CHAR(STRING_ELT(rname, 0));

    checkProb(lp);

    if ( setjmp(jenv) ) {
        glp_error_hook( NULL, NULL );
        return out;
    }

    ge.e = 100;
    glp_error_hook( (func) &cleanGLPKerror, &ge );

    rind = glp_find_row(R_ExternalPtrAddr(lp), rrname);

    glp_error_hook( NULL, NULL );

    out = Rf_ScalarInteger(rind);

    return out;

}


/* -------------------------------------------------------------------------- */
/* add collumns to the problem object */
SEXP addCols(SEXP lp, SEXP ncols) {

    SEXP out = R_NilValue;
    int fcol = 0;

    checkProb(lp);

    fcol = glp_add_cols(R_ExternalPtrAddr(lp), Rf_asInteger(ncols));

    out = Rf_ScalarInteger(fcol);

    return out;
}


/* -------------------------------------------------------------------------- */
/* set column name j */
SEXP setColName(SEXP lp, SEXP j, SEXP cname) {

    SEXP out = R_NilValue;

    const char *rcname;
    if (cname == R_NilValue) {
        rcname = NULL;
    }
    else {
        rcname = CHAR(STRING_ELT(cname, 0));
    }

    checkProb(lp);
    checkColIndex(lp, j);

    glp_set_col_name(R_ExternalPtrAddr(lp), Rf_asInteger(j), rcname);

    return out;

}


/* -------------------------------------------------------------------------- */
/* set column names */
SEXP setColsNames(SEXP lp, SEXP j, SEXP cnames) {

    SEXP out = R_NilValue;

    int *rj = INTEGER(j);
    int k, numcn;

    checkProb(lp);
    checkColIndices(lp, j, NULL);

    if (cnames == R_NilValue) {
        numcn = Rf_length(j);
        for (k = 0; k < numcn; k++) {
            glp_set_col_name(R_ExternalPtrAddr(lp), rj[k], NULL);
        }
    }
    else {
        checkVecLen(Rf_ScalarInteger(Rf_length(j)), cnames);
        numcn = Rf_length(cnames);
        for (k = 0; k < numcn; k++) {
            glp_set_col_name(R_ExternalPtrAddr(lp),
                             rj[k], CHAR(STRING_ELT(cnames, k)));
        }
    }

    return out;

}


/* -------------------------------------------------------------------------- */
/* get column name j */
SEXP getColName(SEXP lp, SEXP j) {

    SEXP out = R_NilValue;

    const char *rcname;

    checkProb(lp);
    checkColIndex(lp, j);

    rcname = glp_get_col_name(R_ExternalPtrAddr(lp), Rf_asInteger(j));

    if (rcname != NULL) {
        out = Rf_mkString(rcname);
    }

    return out;

}


/* -------------------------------------------------------------------------- */
/* find column by its name */
SEXP findCol(SEXP lp, SEXP cname) {

    SEXP out = R_NilValue;
    int cind;

    const char *rcname = CHAR(STRING_ELT(cname, 0));

    checkProb(lp);

    if ( setjmp(jenv) ) {
        glp_error_hook( NULL, NULL );
        return out;
    }

    ge.e = 100;
    glp_error_hook( (func) &cleanGLPKerror, &ge );

    cind = glp_find_col(R_ExternalPtrAddr(lp), rcname);

    glp_error_hook( NULL, NULL );

    out = Rf_ScalarInteger(cind);

    return out;

}


/* -------------------------------------------------------------------------- */
/* get number of rows */
SEXP getNumRows(SEXP lp) {

    SEXP out = R_NilValue;
    int nrows = 0;

    checkProb(lp);

    nrows = glp_get_num_rows(R_ExternalPtrAddr(lp));

    out = Rf_ScalarInteger(nrows);

    return out;
}


/* -------------------------------------------------------------------------- */
/* get number of columns */
SEXP getNumCols(SEXP lp) {

    SEXP out = R_NilValue;
    int ncols = 0;

    checkProb(lp);

    ncols = glp_get_num_cols(R_ExternalPtrAddr(lp));

    out = Rf_ScalarInteger(ncols);

    return out;
}


/* -------------------------------------------------------------------------- */
/* set column bounds (for more than one column) */
SEXP setColsBnds(SEXP lp, SEXP j, SEXP type, SEXP lb, SEXP ub) {

    SEXP out = R_NilValue;

    int k, nj;
    int *rj = INTEGER(j);
    double *rlb = REAL(lb), *rub = REAL(ub);
    const int *rtype;

    if (type == R_NilValue) {
        rtype = NULL;
    }
    else {
        rtype = INTEGER(type);
    }

    checkProb(lp);
    checkColIndices(lp, j, NULL);
    checkVarTypes(type);

    /* nj = sizeof(j)/sizeof(j[0]) */       /* j is a SEXP,                   */
    nj = Rf_length(j);                      /*  better use Rf_length() from R */

    if (rtype == NULL) {
        for (k = 0; k < nj; k++) {
            if (islessgreater(rlb[k], rub[k])) {
                glp_set_col_bnds(R_ExternalPtrAddr(lp),
                                 rj[k], GLP_DB, rlb[k], rub[k]
                );
            }
            else {
                glp_set_col_bnds(R_ExternalPtrAddr(lp),
                                 rj[k], GLP_FX, rlb[k], rub[k]
                );
            }
        }
    }
    else {
        for (k = 0; k < nj; k++) {
            glp_set_col_bnds(R_ExternalPtrAddr(lp),
                             rj[k], rtype[k], rlb[k], rub[k]
            );
        }
    }

    return out;

}


/* -------------------------------------------------------------------------- */
/* set column bounds and objective coefficients (for more than one column) */
SEXP setColsBndsObjCoefs(SEXP lp, SEXP j, SEXP type,
                         SEXP lb, SEXP ub, SEXP obj_coef) {

    SEXP out = R_NilValue;

    int k, nj;
    int *rj = INTEGER(j);
    double *rlb = REAL(lb), *rub = REAL(ub), *robj_coef = REAL(obj_coef);
    const int *rtype;

    if (type == R_NilValue) {
        rtype = NULL;
    }
    else {
        rtype = INTEGER(type);
    }

    checkProb(lp);
    checkColIndices(lp, j, NULL);
    checkVarTypes(type);

    nj = Rf_length(j);

    if (rtype == NULL) {
        for (k = 0; k < nj; k++) {
            if (islessgreater(rlb[k], rub[k])) {
                glp_set_col_bnds(R_ExternalPtrAddr(lp),
                                 rj[k], GLP_DB, rlb[k], rub[k]
                );
            }
            else {
                glp_set_col_bnds(R_ExternalPtrAddr(lp),
                                 rj[k], GLP_FX, rlb[k], rub[k]
                );
            }
            glp_set_obj_coef(R_ExternalPtrAddr(lp), rj[k], robj_coef[k]);
        }
    }
    else {
        for (k = 0; k < nj; k++) {
            glp_set_col_bnds(R_ExternalPtrAddr(lp),
                             rj[k], rtype[k], rlb[k], rub[k]
            );
            glp_set_obj_coef(R_ExternalPtrAddr(lp), rj[k], robj_coef[k]);
        }

    }

    return out;

}

/* -------------------------------------------------------------------------- */
/* set column bound (for only one column) */
SEXP setColBnd(SEXP lp, SEXP j, SEXP type, SEXP lb, SEXP ub) {

    SEXP out = R_NilValue;

    checkProb(lp);
    checkColIndex(lp, j);
    checkVarType(type);

    glp_set_col_bnds(R_ExternalPtrAddr(lp), Rf_asInteger(j), Rf_asInteger(type),
                     Rf_asReal(lb), Rf_asReal(ub)
    );

    return out;
}


/* -------------------------------------------------------------------------- */
/* get column lower bounds (for more than one column) */
SEXP getColsLowBnds(SEXP lp, SEXP j) {

    SEXP out = R_NilValue;

    int k, nj;
    double lowbnd;
    int *rj = INTEGER(j);

    checkProb(lp);
    checkColIndices(lp, j, NULL);

    nj = Rf_length(j);

    PROTECT(out = Rf_allocVector(REALSXP, nj));
    for (k = 0; k < nj; k++) {
        lowbnd = glp_get_col_lb(R_ExternalPtrAddr(lp), rj[k]);
        REAL(out)[k] = lowbnd;
    }
    UNPROTECT(1);

    return out;

}


/* -------------------------------------------------------------------------- */
/* get column low bound (for only one column) */
SEXP getColLowBnd(SEXP lp, SEXP j) {

    SEXP out = R_NilValue;
    double lowbnd = 0;

    checkProb(lp);
    checkColIndex(lp, j);

    lowbnd = glp_get_col_lb(R_ExternalPtrAddr(lp), Rf_asInteger(j));

    out = Rf_ScalarReal(lowbnd);

    return out;
}


/* -------------------------------------------------------------------------- */
/* get column upper bounds (for more than one column) */
SEXP getColsUppBnds(SEXP lp, SEXP j) {

    SEXP out = R_NilValue;

    int k, nj;
    double uppbnd;
    int *rj = INTEGER(j);

    checkProb(lp);
    checkColIndices(lp, j, NULL);

    nj = Rf_length(j);

    PROTECT(out = Rf_allocVector(REALSXP, nj));
    for (k = 0; k < nj; k++) {
        uppbnd = glp_get_col_ub(R_ExternalPtrAddr(lp), rj[k]);
        REAL(out)[k] = uppbnd;
    }
    UNPROTECT(1);

    return out;

}


/* -------------------------------------------------------------------------- */
/* get column upper bound (for only one column) */
SEXP getColUppBnd(SEXP lp, SEXP j) {

    SEXP out = R_NilValue;
    double uppbnd = 0;

    checkProb(lp);
    checkColIndex(lp, j);

    uppbnd = glp_get_col_ub(R_ExternalPtrAddr(lp), Rf_asInteger(j));

    out = Rf_ScalarReal(uppbnd);

    return out;
}


/* -------------------------------------------------------------------------- */
/* set column kind (for only one column) */
SEXP setColKind(SEXP lp, SEXP j, SEXP kind) {

    SEXP out = R_NilValue;

    checkProb(lp);
    checkColIndex(lp, j);
    checkVarKind(kind);

    glp_set_col_kind(R_ExternalPtrAddr(lp),
                     Rf_asInteger(j), Rf_asInteger(kind));

    return out;
}


/* -------------------------------------------------------------------------- */
/* set column kind (for more than one column) */
SEXP setColsKind(SEXP lp, SEXP j, SEXP kind) {

    SEXP out = R_NilValue;

    int k, nj;
    int *rj = INTEGER(j);
    int *rkind = INTEGER(kind);

    checkProb(lp);
    checkColIndices(lp, j, NULL);
    checkVarKinds(kind);

    nj = Rf_length(j);
    for (k = 0; k < nj; k++) {
        glp_set_col_kind(R_ExternalPtrAddr(lp), rj[k], rkind[k]);
    }

    return out;

}


/* -------------------------------------------------------------------------- */
/* retrieve column kind (for only one column) */
SEXP getColKind(SEXP lp, SEXP j) {

    SEXP out = R_NilValue;
    int kind = 0;

    checkProb(lp);
    checkColIndex(lp, j);

    kind = glp_get_col_kind(R_ExternalPtrAddr(lp), Rf_asInteger(j));

    out = Rf_ScalarInteger(kind);

    return out;
}


/* -------------------------------------------------------------------------- */
/* retrieve column kind (for more than one column) */
SEXP getColsKind(SEXP lp, SEXP j) {

    SEXP out = R_NilValue;

    int k, nj;
    int kind;
    int *rj = INTEGER(j);

    checkProb(lp);
    checkColIndices(lp, j, NULL);

    nj = Rf_length(j);

    PROTECT(out = Rf_allocVector(INTSXP, nj));
    for (k = 0; k < nj; k++) {
        kind = glp_get_col_kind(R_ExternalPtrAddr(lp), rj[k]);
        INTEGER(out)[k] = kind;
    }
    UNPROTECT(1);

    return out;

}


/* -------------------------------------------------------------------------- */
/* retrieve number of integer columns */
SEXP getNumInt(SEXP lp) {

    SEXP out = R_NilValue;
    int num = 0;

    checkProb(lp);

    num = glp_get_num_int(R_ExternalPtrAddr(lp));

    out = Rf_ScalarInteger(num);

    return out;
}


/* -------------------------------------------------------------------------- */
/* retrieve number of binary columns */
SEXP getNumBin(SEXP lp) {

    SEXP out = R_NilValue;
    int num = 0;

    checkProb(lp);

    num = glp_get_num_bin(R_ExternalPtrAddr(lp));

    out = Rf_ScalarInteger(num);

    return out;
}


/* -------------------------------------------------------------------------- */
/* set row bounds (for more than one row) */
SEXP setRowsBnds(SEXP lp, SEXP i, SEXP type, SEXP lb, SEXP ub) {

    SEXP out = R_NilValue;

    int k, ni;
    int *ri = INTEGER(i);
    double *rlb = REAL(lb), *rub = REAL(ub);
    const int *rtype;

    if (type == R_NilValue) {
        rtype = NULL;
    }
    else {
        rtype = INTEGER(type);
    }

    checkProb(lp);
    checkRowIndices(lp, i, NULL);
    checkVarTypes(type);

    ni = Rf_length(i);

    if (rtype == NULL) {
        for (k = 0; k < ni; k++) {
            if (islessgreater(rlb[k], rub[k])) {
                glp_set_row_bnds(R_ExternalPtrAddr(lp),
                                 ri[k], GLP_DB, rlb[k], rub[k]
                );
            }
            else {
                glp_set_row_bnds(R_ExternalPtrAddr(lp),
                                 ri[k], GLP_FX, rlb[k], rub[k]
                );
            }
        }
    }
    else {
        for (k = 0; k < ni; k++) {
            glp_set_row_bnds(R_ExternalPtrAddr(lp),
                             ri[k], rtype[k], rlb[k], rub[k]
            );
        }
    }

    return out;

}


/* -------------------------------------------------------------------------- */
/* set right hand side (rhs) to zero (fixed) */
SEXP setRhsZero(SEXP lp) {

    SEXP out = R_NilValue;

    int k, nrows;

    checkProb(lp);

    nrows = glp_get_num_rows(R_ExternalPtrAddr(lp));

    for (k = 1; k <= nrows; k++) {
        glp_set_row_bnds(R_ExternalPtrAddr(lp), k, GLP_FX,
                         (double) 0, (double) 0
        );
    }

    return out;

}


/* -------------------------------------------------------------------------- */
/* set row bound (for only one row) */
SEXP setRowBnd(SEXP lp, SEXP i, SEXP type, SEXP lb, SEXP ub) {

    SEXP out = R_NilValue;

    checkProb(lp);
    checkRowIndex(lp, i);
    checkVarType(type);

    glp_set_row_bnds(R_ExternalPtrAddr(lp), Rf_asInteger(i),
                     Rf_asInteger(type), Rf_asReal(lb), Rf_asReal(ub)
    );

    return out;
}


/* -------------------------------------------------------------------------- */
/* get row lower bounds (for more than one row) */
SEXP getRowsLowBnds(SEXP lp, SEXP i) {

    SEXP out = R_NilValue;

    int k, ni;
    double lowbnd;
    int *ri = INTEGER(i);

    checkProb(lp);
    checkRowIndices(lp, i, NULL);

    ni = Rf_length(i);

    PROTECT(out = Rf_allocVector(REALSXP, ni));
    for (k = 0; k < ni; k++) {
        lowbnd = glp_get_row_lb(R_ExternalPtrAddr(lp), ri[k]);
        REAL(out)[k] = lowbnd;
    }
    UNPROTECT(1);

    return out;

}


/* -------------------------------------------------------------------------- */
/* get row low bound (for only one row) */
SEXP getRowLowBnd(SEXP lp, SEXP i) {

    SEXP out = R_NilValue;
    double lowbnd = 0;

    checkProb(lp);
    checkRowIndex(lp, i);

    lowbnd = glp_get_row_lb(R_ExternalPtrAddr(lp), Rf_asInteger(i));

    out = Rf_ScalarReal(lowbnd);

    return out;
}


/* -------------------------------------------------------------------------- */
/* get row upper bounds (for more than one row) */
SEXP getRowsUppBnds(SEXP lp, SEXP i) {

    SEXP out = R_NilValue;

    int k, ni;
    double uppbnd;
    int *ri = INTEGER(i);

    checkProb(lp);
    checkRowIndices(lp, i, NULL);

    ni = Rf_length(i);

    PROTECT(out = Rf_allocVector(REALSXP, ni));
    for (k = 0; k < ni; k++) {
        uppbnd = glp_get_row_ub(R_ExternalPtrAddr(lp), ri[k]);
        REAL(out)[k] = uppbnd;
    }
    UNPROTECT(1);

    return out;

}


/* -------------------------------------------------------------------------- */
/* get row upper bound (for only one row) */
SEXP getRowUppBnd(SEXP lp, SEXP i) {

    SEXP out = R_NilValue;
    double uppbnd = 0;

    checkProb(lp);
    checkRowIndex(lp, i);

    uppbnd = glp_get_row_ub(R_ExternalPtrAddr(lp), Rf_asInteger(i));

    out = Rf_ScalarReal(uppbnd);

    return out;
}


/* -------------------------------------------------------------------------- */
/* get row type */
SEXP getRowType(SEXP lp, SEXP i) {

    SEXP out = R_NilValue;
    int type = 0;

    checkProb(lp);
    checkRowIndex(lp, i);

    type = glp_get_row_type(R_ExternalPtrAddr(lp), Rf_asInteger(i));

    out = Rf_ScalarInteger(type);

    return out;
}


/* -------------------------------------------------------------------------- */
/* get row types (for more than one row) */
SEXP getRowsTypes(SEXP lp, SEXP i) {

    SEXP out = R_NilValue;

    int k, ni, type;
    int *ri = INTEGER(i);

    checkProb(lp);
    checkRowIndices(lp, i, NULL);

    ni = Rf_length(i);

    PROTECT(out = Rf_allocVector(INTSXP, ni));
    for (k = 0; k < ni; k++) {
        type = glp_get_row_type(R_ExternalPtrAddr(lp), ri[k]);
        INTEGER(out)[k] = type;
    }
    UNPROTECT(1);

    return out;
}


/* -------------------------------------------------------------------------- */
/* get col type */
SEXP getColType(SEXP lp, SEXP j) {

    SEXP out = R_NilValue;
    int type = 0;

    checkProb(lp);
    checkColIndex(lp, j);

    type = glp_get_col_type(R_ExternalPtrAddr(lp), Rf_asInteger(j));

    out = Rf_ScalarInteger(type);

    return out;
}


/* -------------------------------------------------------------------------- */
/* set objective coefficients (for more than one column) */
SEXP setObjCoefs(SEXP lp, SEXP j, SEXP obj_coef) {

    SEXP out = R_NilValue;

    int k, nj;
    int *rj = INTEGER(j);
    double *robj_coef = REAL(obj_coef);

    checkProb(lp);
    checkColIndices(lp, j, NULL);

    nj = Rf_length(j);
    for (k = 0; k < nj; k++) {
        glp_set_obj_coef(R_ExternalPtrAddr(lp), rj[k], robj_coef[k]);
    }

    return out;

}


/* -------------------------------------------------------------------------- */
/* set objective coefficient (for only one column) */
SEXP setObjCoef(SEXP lp, SEXP j, SEXP obj_coef) {

    SEXP out = R_NilValue;

    checkProb(lp);
    checkColIndex(lp, j);

    glp_set_obj_coef(R_ExternalPtrAddr(lp),
                     Rf_asInteger(j), Rf_asReal(obj_coef)
    );

    return out;

}


/* -------------------------------------------------------------------------- */
/* get objective coefficients (for more than one column) */
SEXP getObjCoefs(SEXP lp, SEXP j) {

    SEXP out = R_NilValue;

    int k, nj;
    double obj_coef;
    int *rj = INTEGER(j);

    checkProb(lp);
    checkColIndices(lp, j, NULL);

    nj = Rf_length(j);

    PROTECT(out = Rf_allocVector(REALSXP, nj));
    for (k = 0; k < nj; k++) {
        obj_coef = glp_get_obj_coef(R_ExternalPtrAddr(lp), rj[k]);
        REAL(out)[k] = obj_coef;
    }
    UNPROTECT(1);

    return out;

}


/* -------------------------------------------------------------------------- */
/* get objective coefficient (for only one column) */
SEXP getObjCoef(SEXP lp, SEXP j) {

    SEXP out = R_NilValue;
    double obj_coef = 0;

    checkProb(lp);
    checkColIndex(lp, j);

    obj_coef = glp_get_obj_coef(R_ExternalPtrAddr(lp), Rf_asInteger(j));

    out = Rf_ScalarReal(obj_coef);

    return out;
}


/* -------------------------------------------------------------------------- */
/* load the whole constraint matrix */
SEXP loadMatrix(SEXP lp, SEXP ne, SEXP ia, SEXP ja, SEXP ra) {

    SEXP out = R_NilValue;

    const int *ria = INTEGER(ia);
    const int *rja = INTEGER(ja);
    const double *rra = REAL(ra);

    checkProb(lp);
    checkVecLen(ne, ia);
    checkVecLen(ne, ja);
    checkVecLen(ne, ra);
    checkRowIndices(lp, ia, NULL);
    checkColIndices(lp, ja, NULL);
    checkDupIndices(ia, ja, ne);

/*
    if ( setjmp(jenv) ) {
        glp_error_hook( NULL, NULL );
        return out;
    }

    ge.e = 100;
    glp_error_hook( (func) &cleanGLPKerror, &ge );
*/

    glp_load_matrix(R_ExternalPtrAddr(lp), Rf_asInteger(ne),
                    &(ria[-1]), &(rja[-1]), &(rra[-1]));

/*
    glp_error_hook( NULL, NULL );
*/

    return out;
}


/* -------------------------------------------------------------------------- */
/* check for duplicate elements in sparse matrix */
SEXP checkDup(SEXP m, SEXP n, SEXP ne, SEXP ia, SEXP ja) {

    SEXP out = R_NilValue;
    int dup = 0;

    const int *ria = INTEGER(ia);
    const int *rja = INTEGER(ja);

    dup = glp_check_dup(Rf_asInteger(m), Rf_asInteger(n), Rf_asInteger(ne),
                        &(ria[-1]), &(rja[-1]));

    out = Rf_ScalarInteger(dup);

    return out;
}


/* -------------------------------------------------------------------------- */
/* sort elements of the constraint matrix */
SEXP sortMatrix(SEXP lp) {

    SEXP out = R_NilValue;

    checkProb(lp);

    glp_sort_matrix(R_ExternalPtrAddr(lp));

    return out;
}


/* -------------------------------------------------------------------------- */
/* delete rows from problem object */
SEXP delRows(SEXP lp, SEXP nrows, SEXP i) {

    SEXP out = R_NilValue;

    const int *ri = INTEGER(i);

    checkProb(lp);
    checkVecLen(Rf_ScalarInteger(Rf_asInteger(nrows) + 1), i);
    checkRowIndices(lp, i, 1);

    glp_del_rows(R_ExternalPtrAddr(lp), Rf_asInteger(nrows), ri);

    return out;
}


/* -------------------------------------------------------------------------- */
/* delete columns from problem object */
SEXP delCols(SEXP lp, SEXP ncols, SEXP j) {

    SEXP out = R_NilValue;

    const int *rj = INTEGER(j);

    checkProb(lp);
    checkVecLen(Rf_ScalarInteger(Rf_asInteger(ncols) + 1), j);
    checkColIndices(lp, j, 1);

    glp_del_cols(R_ExternalPtrAddr(lp), Rf_asInteger(ncols), rj);

    return out;
}


/* -------------------------------------------------------------------------- */
/* set row scale factor */
SEXP setRii(SEXP lp, SEXP i, SEXP rii) {

    SEXP out = R_NilValue;

    checkProb(lp);
    checkRowIndex(lp, i);

    glp_set_rii(R_ExternalPtrAddr(lp), Rf_asInteger(i), Rf_asReal(rii));

    return out;
}


/* -------------------------------------------------------------------------- */
/* set column scale factor */
SEXP setSjj(SEXP lp, SEXP j, SEXP sjj) {

    SEXP out = R_NilValue;

    checkProb(lp);
    checkColIndex(lp, j);

    glp_set_sjj(R_ExternalPtrAddr(lp), Rf_asInteger(j), Rf_asReal(sjj));

    return out;
}


/* -------------------------------------------------------------------------- */
/* retrieve row scale factor */
SEXP getRii(SEXP lp, SEXP i) {

    SEXP out = R_NilValue;
    double rii = 0;

    checkProb(lp);
    checkRowIndex(lp, i);

    rii = glp_get_rii(R_ExternalPtrAddr(lp), Rf_asInteger(i));

    out = Rf_ScalarReal(rii);

    return out;
}


/* -------------------------------------------------------------------------- */
/* retrieve column scale factor */
SEXP getSjj(SEXP lp, SEXP j) {

    SEXP out = R_NilValue;
    double sjj = 0;

    checkProb(lp);
    checkColIndex(lp, j);

    sjj = glp_get_sjj(R_ExternalPtrAddr(lp), Rf_asInteger(j));

    out = Rf_ScalarReal(sjj);

    return out;
}


/* -------------------------------------------------------------------------- */
/* problem scaling */
SEXP scaleProb(SEXP lp, SEXP opt) {

    SEXP out = R_NilValue;

    checkProb(lp);
    checkScaling(opt);

    glp_scale_prob(R_ExternalPtrAddr(lp), Rf_asInteger(opt));

    return out;
}


/* -------------------------------------------------------------------------- */
/* problem unscaling */
SEXP unscaleProb(SEXP lp) {

    SEXP out = R_NilValue;

    checkProb(lp);

    glp_unscale_prob(R_ExternalPtrAddr(lp));

    return out;
}


/* -------------------------------------------------------------------------- */
/* set row status */
SEXP setRowStat(SEXP lp, SEXP i, SEXP stat) {

    SEXP out = R_NilValue;

    checkProb(lp);
    checkRowIndex(lp, i);
    checkVarStat(stat);

    glp_set_row_stat(R_ExternalPtrAddr(lp),
                     Rf_asInteger(i), Rf_asInteger(stat)
    );

    return out;
}


/* -------------------------------------------------------------------------- */
/* set column status */
SEXP setColStat(SEXP lp, SEXP j, SEXP stat) {

    SEXP out = R_NilValue;

    checkProb(lp);
    checkColIndex(lp, j);
    checkVarStat(stat);

    glp_set_col_stat(R_ExternalPtrAddr(lp),
                     Rf_asInteger(j), Rf_asInteger(stat)
    );

    return out;
}


/* -------------------------------------------------------------------------- */
/* construct standard initial LP basis */
SEXP stdBasis(SEXP lp) {

    SEXP out = R_NilValue;

    checkProb(lp);

    glp_std_basis(R_ExternalPtrAddr(lp));

    return out;

}


/* -------------------------------------------------------------------------- */
/* construct advanced initial LP basis */
SEXP advBasis(SEXP lp) {

    SEXP out = R_NilValue;

    checkProb(lp);

    glp_adv_basis(R_ExternalPtrAddr(lp), 0);

    return out;

}


/* -------------------------------------------------------------------------- */
/* construct Bixby's initial LP basis */
SEXP cpxBasis(SEXP lp) {

    SEXP out = R_NilValue;

    checkProb(lp);

    glp_cpx_basis(R_ExternalPtrAddr(lp));

    return out;

}


/* -------------------------------------------------------------------------- */
/* "warm up" LP basis */
SEXP warmUp(SEXP lp) {

    SEXP out = R_NilValue;
    int wup = 0;

    checkProb(lp);

    wup = glp_warm_up(R_ExternalPtrAddr(lp));

    out = Rf_ScalarInteger(wup);

    return out;
}


/* -------------------------------------------------------------------------- */
/* enable/disable terminal output */
SEXP termOut(SEXP flag) {

    SEXP out = R_NilValue;
    int pfl = 0;
    int fl;

    fl = (Rf_asInteger(flag) == GLP_ON) ? GLP_ON : GLP_OFF;

    pfl = glp_term_out(fl);

    out = Rf_ScalarInteger(pfl);

    return out;
}


/* -------------------------------------------------------------------------- */
/* solve problem with simplex algorithm */
SEXP solveSimplex(SEXP lp) {

    SEXP out = R_NilValue;
    int ret = 0;

    checkProb(lp);

    /*
    glp_smcp parmS;
    glp_init_smcp(&parmS);
    parmS.tm_lim = 10000;
    */
    ret = glp_simplex(R_ExternalPtrAddr(lp), &parmS);
    /* ret = glp_simplex(R_ExternalPtrAddr(lp), NULL); */

    out = Rf_ScalarInteger(ret);

    return out;
}


/* -------------------------------------------------------------------------- */
/* solve problem with exact simplex algorithm */
SEXP solveSimplexExact(SEXP lp) {

    SEXP out = R_NilValue;
    int ret = 0;

    checkProb(lp);

    ret = glp_exact(R_ExternalPtrAddr(lp), &parmS);
    /* ret = glp_exact(R_ExternalPtrAddr(lp), NULL); */

    out = Rf_ScalarInteger(ret);

    return out;
}


/* -------------------------------------------------------------------------- */
/* get value of the objective function after simplex */
SEXP getObjVal(SEXP lp) {

    SEXP out = R_NilValue;
    double obj_val = 0;

    checkProb(lp);

    obj_val = glp_get_obj_val(R_ExternalPtrAddr(lp));

    out = Rf_ScalarReal(obj_val);

    return out;
}


/* -------------------------------------------------------------------------- */
/* get solution status after simplex */
SEXP getSolStat(SEXP lp) {

    SEXP out = R_NilValue;
    int stat = 0;

    checkProb(lp);

    stat = glp_get_status(R_ExternalPtrAddr(lp));

    out = Rf_ScalarInteger(stat);

    return out;
}


/* -------------------------------------------------------------------------- */
/* get column primal value (flux distribution) for all columns */
SEXP getColsPrim(SEXP lp) {

    SEXP out = R_NilValue;
    double col_prim = 0;

    int num_cols, k;

    checkProb(lp);

    num_cols = glp_get_num_cols(R_ExternalPtrAddr(lp));

    PROTECT(out = Rf_allocVector(REALSXP, num_cols));
    for (k = 1; k <= num_cols; k++) {
        col_prim = glp_get_col_prim(R_ExternalPtrAddr(lp), k);
        REAL(out)[k-1] = col_prim;
    }
    UNPROTECT(1);

    return out;
}


/* -------------------------------------------------------------------------- */
/* get column primal value (flux distribution) for one column */
SEXP getColPrim(SEXP lp, SEXP j) {

    SEXP out = R_NilValue;
    double col_prim = 0;

    checkProb(lp);
    checkColIndex(lp, j);

    col_prim = glp_get_col_prim(R_ExternalPtrAddr(lp), Rf_asInteger(j));

    out = Rf_ScalarReal(col_prim);

    return out;
}


/* -------------------------------------------------------------------------- */
/* retrieve status of primal basic solution */
SEXP getPrimStat(SEXP lp) {

    SEXP out = R_NilValue;
    int prim_stat = 0;

    checkProb(lp);

    prim_stat = glp_get_prim_stat(R_ExternalPtrAddr(lp));

    out = Rf_ScalarInteger(prim_stat);

    return out;
}


/* -------------------------------------------------------------------------- */
/* retrieve status of dual basic solution */
SEXP getDualStat(SEXP lp) {

    SEXP out = R_NilValue;
    int dual_stat = 0;

    checkProb(lp);

    dual_stat = glp_get_dual_stat(R_ExternalPtrAddr(lp));

    out = Rf_ScalarInteger(dual_stat);

    return out;
}


/* -------------------------------------------------------------------------- */
/* retrieve row status */
SEXP getRowStat(SEXP lp, SEXP i) {

    SEXP out = R_NilValue;
    int row_stat = 0;

    checkProb(lp);
    checkRowIndex(lp, i);

    row_stat = glp_get_row_stat(R_ExternalPtrAddr(lp), Rf_asInteger(i));

    out = Rf_ScalarInteger(row_stat);

    return out;
}


/* -------------------------------------------------------------------------- */
/* retrieve row status for all rows */
SEXP getRowsStat(SEXP lp) {

    SEXP out = R_NilValue;
    int row_stat = 0;

    int num_rows, k;

    checkProb(lp);

    num_rows = glp_get_num_rows(R_ExternalPtrAddr(lp));

    PROTECT(out = Rf_allocVector(INTSXP, num_rows));
    for (k = 1; k <= num_rows; k++) {
        row_stat = glp_get_row_stat(R_ExternalPtrAddr(lp), k);
        INTEGER(out)[k-1] = row_stat;
    }
    UNPROTECT(1);

    return out;
}


/* -------------------------------------------------------------------------- */
/* retrieve row primal value */
SEXP getRowPrim(SEXP lp, SEXP i) {

    SEXP out = R_NilValue;
    double row_prim;

    checkProb(lp);
    checkRowIndex(lp, i);

    row_prim = glp_get_row_prim(R_ExternalPtrAddr(lp), Rf_asInteger(i));

    out = Rf_ScalarReal(row_prim);

    return out;
}


/* -------------------------------------------------------------------------- */
/* retrieve row primal value for all rows */
SEXP getRowsPrim(SEXP lp) {

    SEXP out = R_NilValue;
    double row_prim = 0;

    int num_rows, k;

    checkProb(lp);

    num_rows = glp_get_num_rows(R_ExternalPtrAddr(lp));

    PROTECT(out = Rf_allocVector(REALSXP, num_rows));
    for (k = 1; k <= num_rows; k++) {
        row_prim = glp_get_row_prim(R_ExternalPtrAddr(lp), k);
        REAL(out)[k-1] = row_prim;
    }
    UNPROTECT(1);

    return out;
}


/* -------------------------------------------------------------------------- */
/* retrieve row dual value */
SEXP getRowDual(SEXP lp, SEXP i) {

    SEXP out = R_NilValue;
    double row_dual;

    checkProb(lp);
    checkRowIndex(lp, i);

    row_dual = glp_get_row_dual(R_ExternalPtrAddr(lp), Rf_asInteger(i));

    out = Rf_ScalarReal(row_dual);

    return out;
}


/* -------------------------------------------------------------------------- */
/* retrieve row dual value for all rows */
SEXP getRowsDual(SEXP lp) {

    SEXP out = R_NilValue;
    double row_dual = 0;

    int num_rows, k;

    checkProb(lp);

    num_rows = glp_get_num_rows(R_ExternalPtrAddr(lp));

    PROTECT(out = Rf_allocVector(REALSXP, num_rows));
    for (k = 1; k <= num_rows; k++) {
        row_dual = glp_get_row_dual(R_ExternalPtrAddr(lp), k);
        REAL(out)[k-1] = row_dual;
    }
    UNPROTECT(1);

    return out;
}


/* -------------------------------------------------------------------------- */
/* retrieve column status */
SEXP getColStat(SEXP lp, SEXP j) {

    SEXP out = R_NilValue;
    int col_stat = 0;

    checkProb(lp);
    checkColIndex(lp, j);

    col_stat = glp_get_col_stat(R_ExternalPtrAddr(lp), Rf_asInteger(j));

    out = Rf_ScalarInteger(col_stat);

    return out;
}


/* -------------------------------------------------------------------------- */
/* retrieve column status for all columns */
SEXP getColsStat(SEXP lp) {

    SEXP out = R_NilValue;
    int col_stat = 0;

    int num_cols, k;

    checkProb(lp);

    num_cols = glp_get_num_cols(R_ExternalPtrAddr(lp));

    PROTECT(out = Rf_allocVector(INTSXP, num_cols));
    for (k = 1; k <= num_cols; k++) {
        col_stat = glp_get_col_stat(R_ExternalPtrAddr(lp), k);
        INTEGER(out)[k-1] = col_stat;
    }
    UNPROTECT(1);

    return out;
}


/* -------------------------------------------------------------------------- */
/* retrieve column dual value */
SEXP getColDual(SEXP lp, SEXP j) {

    SEXP out = R_NilValue;
    double col_dual = 0;

    checkProb(lp);
    checkColIndex(lp, j);

    col_dual = glp_get_col_dual(R_ExternalPtrAddr(lp), Rf_asInteger(j));

    out = Rf_ScalarReal(col_dual);

    return out;
}


/* -------------------------------------------------------------------------- */
/* retrieve column dual value for all columns */
SEXP getColsDual(SEXP lp) {

    SEXP out = R_NilValue;
    double col_dual = 0;

    int num_cols, k;

    checkProb(lp);

    num_cols = glp_get_num_cols(R_ExternalPtrAddr(lp));

    PROTECT(out = Rf_allocVector(REALSXP, num_cols));
    for (k = 1; k <= num_cols; k++) {
        col_dual = glp_get_col_dual(R_ExternalPtrAddr(lp), k);
        REAL(out)[k-1] = col_dual;
    }
    UNPROTECT(1);

    return out;
}


/* -------------------------------------------------------------------------- */
/* determine variable causing unboundedness */
SEXP getUnbndRay(SEXP lp) {

    SEXP out = R_NilValue;
    int unbnd;

    checkProb(lp);

    unbnd = glp_get_unbnd_ray(R_ExternalPtrAddr(lp));

    out = Rf_ScalarInteger(unbnd);

    return out;
}


/* -------------------------------------------------------------------------- */
/* solve problem with interior point method */
SEXP solveInterior(SEXP lp) {

    SEXP out = R_NilValue;
    int ret = 0;

    checkProb(lp);

    ret = glp_interior(R_ExternalPtrAddr(lp), &parmI);
    /* ret = glp_interior(R_ExternalPtrAddr(lp), NULL); */

    out = Rf_ScalarInteger(ret);

    return out;
}


/* -------------------------------------------------------------------------- */
/* get value of the objective function after interior point method */
SEXP getObjValIpt(SEXP lp) {

    SEXP out = R_NilValue;
    double obj_val = 0;

    checkProb(lp);

    obj_val = glp_ipt_obj_val(R_ExternalPtrAddr(lp));

    out = Rf_ScalarReal(obj_val);

    return out;
}


/* -------------------------------------------------------------------------- */
/* get solution status after interior point method */
SEXP getSolStatIpt(SEXP lp) {

    SEXP out = R_NilValue;
    int stat = 0;

    checkProb(lp);

    stat = glp_ipt_status(R_ExternalPtrAddr(lp));

    out = Rf_ScalarInteger(stat);

    return out;
}


/* -------------------------------------------------------------------------- */
/* get column primal value (flux distribution) for all columns (interior) */
SEXP getColsPrimIpt(SEXP lp) {

    SEXP out = R_NilValue;
    double col_prim = 0;

    int num_cols, k;

    checkProb(lp);

    num_cols = glp_get_num_cols(R_ExternalPtrAddr(lp));

    PROTECT(out = Rf_allocVector(REALSXP, num_cols));
    for (k = 1; k <= num_cols; k++) {
        col_prim = glp_ipt_col_prim(R_ExternalPtrAddr(lp), k);
        REAL(out)[k-1] = col_prim;
    }
    UNPROTECT(1);

    return out;
}


/* -------------------------------------------------------------------------- */
/* get column primal value (flux distribution) for one column (interior) */
SEXP getColPrimIpt(SEXP lp, SEXP j) {

    SEXP out = R_NilValue;
    double col_prim = 0;

    checkProb(lp);
    checkColIndex(lp, j);

    col_prim = glp_ipt_col_prim(R_ExternalPtrAddr(lp), Rf_asInteger(j));

    out = Rf_ScalarReal(col_prim);

    return out;
}


/* -------------------------------------------------------------------------- */
/* retrieve row primal value (interior) */
SEXP getRowPrimIpt(SEXP lp, SEXP i) {

    SEXP out = R_NilValue;
    double row_prim = 0;

    checkProb(lp);
    checkRowIndex(lp, i);

    row_prim = glp_ipt_row_prim(R_ExternalPtrAddr(lp), Rf_asInteger(i));

    out = Rf_ScalarReal(row_prim);

    return out;
}


/* -------------------------------------------------------------------------- */
/* retrieve row primal value (interior) for all rows (interior) */
SEXP getRowsPrimIpt(SEXP lp) {

    SEXP out = R_NilValue;
    double row_prim = 0;

    int num_rows, k;

    checkProb(lp);

    num_rows = glp_get_num_rows(R_ExternalPtrAddr(lp));

    PROTECT(out = Rf_allocVector(REALSXP, num_rows));
    for (k = 1; k <= num_rows; k++) {
        row_prim = glp_ipt_row_prim(R_ExternalPtrAddr(lp), k);
        REAL(out)[k-1] = row_prim;
    }
    UNPROTECT(1);

    return out;
}


/* -------------------------------------------------------------------------- */
/* retrieve row dual value (interior) */
SEXP getRowDualIpt(SEXP lp, SEXP i) {

    SEXP out = R_NilValue;
    double row_dual = 0;

    checkProb(lp);
    checkRowIndex(lp, i);

    row_dual = glp_ipt_row_dual(R_ExternalPtrAddr(lp), Rf_asInteger(i));

    out = Rf_ScalarReal(row_dual);

    return out;
}


/* -------------------------------------------------------------------------- */
/* retrieve row dual value (interior) for all rows (interior) */
SEXP getRowsDualIpt(SEXP lp) {

    SEXP out = R_NilValue;
    double row_dual = 0;

    int num_rows, k;

    checkProb(lp);

    num_rows = glp_get_num_rows(R_ExternalPtrAddr(lp));

    PROTECT(out = Rf_allocVector(REALSXP, num_rows));
    for (k = 1; k <= num_rows; k++) {
        row_dual = glp_ipt_row_dual(R_ExternalPtrAddr(lp), k);
        REAL(out)[k-1] = row_dual;
    }
    UNPROTECT(1);

    return out;
}


/* -------------------------------------------------------------------------- */
/* retrieve column dual value (interior) */
SEXP getColDualIpt(SEXP lp, SEXP j) {

    SEXP out = R_NilValue;
    double col_dual = 0;

    checkProb(lp);
    checkColIndex(lp, j);

    col_dual = glp_ipt_col_dual(R_ExternalPtrAddr(lp), Rf_asInteger(j));

    out = Rf_ScalarReal(col_dual);

    return out;
}


/* -------------------------------------------------------------------------- */
/* retrieve column dual value (interior) for all columns (interior) */
SEXP getColsDualIpt(SEXP lp) {

    SEXP out = R_NilValue;
    double col_dual = 0;

    int num_cols, k;

    checkProb(lp);

    num_cols = glp_get_num_cols(R_ExternalPtrAddr(lp));

    PROTECT(out = Rf_allocVector(REALSXP, num_cols));
    for (k = 1; k <= num_cols; k++) {
        col_dual = glp_ipt_col_dual(R_ExternalPtrAddr(lp), k);
        REAL(out)[k-1] = col_dual;
    }
    UNPROTECT(1);

    return out;
}


/* -------------------------------------------------------------------------- */
/* solve MIP problem with the branch-and-cut method */
SEXP solveMIP(SEXP lp) {

    SEXP out = R_NilValue;
    int ret = 0;

    checkProb(lp);

    ret = glp_intopt(R_ExternalPtrAddr(lp), &parmM);

    out = Rf_ScalarInteger(ret);

    return out;
}


/* -------------------------------------------------------------------------- */
/* determine status of MIP solution */
SEXP mipStatus(SEXP lp) {

    SEXP out = R_NilValue;
    int stat = 0;

    checkProb(lp);

    stat = glp_mip_status(R_ExternalPtrAddr(lp));

    out = Rf_ScalarInteger(stat);

    return out;
}


/* -------------------------------------------------------------------------- */
/* retrieve objective value */
SEXP mipObjVal(SEXP lp) {

    SEXP out = R_NilValue;
    double obj_val = 0;

    checkProb(lp);

    obj_val = glp_mip_obj_val(R_ExternalPtrAddr(lp));

    out = Rf_ScalarReal(obj_val);

    return out;
}


/* -------------------------------------------------------------------------- */
/* retrieve row value (MIP) */
SEXP mipRowVal(SEXP lp, SEXP i) {

    SEXP out = R_NilValue;
    double row = 0;

    checkProb(lp);
    checkRowIndex(lp, i);

    row = glp_mip_row_val(R_ExternalPtrAddr(lp), Rf_asInteger(i));

    out = Rf_ScalarReal(row);

    return out;
}


/* -------------------------------------------------------------------------- */
/* retrieve row value for all rows (MIP) */
SEXP mipRowsVal(SEXP lp) {

    SEXP out = R_NilValue;
    double row = 0;

    int num_rows, k;

    checkProb(lp);

    num_rows = glp_get_num_rows(R_ExternalPtrAddr(lp));

    PROTECT(out = Rf_allocVector(REALSXP, num_rows));
    for (k = 1; k <= num_rows; k++) {
        row = glp_mip_row_val(R_ExternalPtrAddr(lp), k);
        REAL(out)[k-1] = row;
    }
    UNPROTECT(1);

    return out;
}


/* -------------------------------------------------------------------------- */
/* retrieve column value (MIP) */
SEXP mipColVal(SEXP lp, SEXP j) {

    SEXP out = R_NilValue;
    double col = 0;

    checkProb(lp);
    checkColIndex(lp, j);

    col = glp_mip_col_val(R_ExternalPtrAddr(lp), Rf_asInteger(j));

    out = Rf_ScalarReal(col);

    return out;
}


/* -------------------------------------------------------------------------- */
/* retrieve column value for all columns (MIP) */
SEXP mipColsVal(SEXP lp) {

    SEXP out = R_NilValue;
    double col = 0;

    int num_cols, k;

    checkProb(lp);

    num_cols = glp_get_num_cols(R_ExternalPtrAddr(lp));

    PROTECT(out = Rf_allocVector(REALSXP, num_cols));
    for (k = 1; k <= num_cols; k++) {
        col = glp_mip_col_val(R_ExternalPtrAddr(lp), k);
        REAL(out)[k-1] = col;
    }
    UNPROTECT(1);

    return out;
}


/* -------------------------------------------------------------------------- */
/* get the number of constraint coefficients (number of non-zero elements in
   the consrtaint matrix) */
SEXP getNumNnz(SEXP lp) {

    SEXP out = R_NilValue;
    int nnz = 0;

    checkProb(lp);

    nnz = glp_get_num_nz(R_ExternalPtrAddr(lp));

    out = Rf_ScalarInteger(nnz);

    return out;
}


/* -------------------------------------------------------------------------- */
/* get row i of the contraint matrix */
SEXP getMatRow(SEXP lp, SEXP i) {

    SEXP out   = R_NilValue;
    SEXP ind   = R_NilValue;
    SEXP val   = R_NilValue;
    SEXP listv = R_NilValue;

    int nnzr = 0;

    checkProb(lp);
    checkRowIndex(lp, i);

    nnzr = glp_get_mat_row(R_ExternalPtrAddr(lp), Rf_asInteger(i), NULL, NULL);

    if (nnzr > 0) {
        PROTECT(ind = Rf_allocVector(INTSXP,  nnzr+1));
        PROTECT(val = Rf_allocVector(REALSXP, nnzr+1));

        nnzr = glp_get_mat_row(R_ExternalPtrAddr(lp), Rf_asInteger(i),
                               INTEGER(ind), REAL(val)
               );

        /* maybe this is ugly */
        REAL(val)[0] = 0;
        INTEGER(ind)[0] = 0;

        PROTECT(out = Rf_allocVector(VECSXP, 3));
        SET_VECTOR_ELT(out, 0, Rf_ScalarInteger(nnzr));
        SET_VECTOR_ELT(out, 1, ind);
        SET_VECTOR_ELT(out, 2, val);

        PROTECT(listv = Rf_allocVector(STRSXP, 3));
        SET_STRING_ELT(listv, 0, Rf_mkChar("nnz"));
        SET_STRING_ELT(listv, 1, Rf_mkChar("index"));
        SET_STRING_ELT(listv, 2, Rf_mkChar("value"));
        Rf_setAttrib(out, R_NamesSymbol, listv);

        UNPROTECT(4);
    }

    return out;
}


/* -------------------------------------------------------------------------- */
/* set row i of the contraint matrix */
SEXP setMatRow(SEXP lp, SEXP i, SEXP len, SEXP ind, SEXP val) {

    SEXP out   = R_NilValue;

    int *rind;
    double *rval;

    if (ind == R_NilValue) {
        rind = NULL;
    }
    else {
        rind = INTEGER(ind);
    }

    if (val == R_NilValue) {
        rval = NULL;
    }
    else {
        rval = REAL(val);
    }

    checkProb(lp);
    checkRowIndex(lp, i);
    checkColIndices(lp, ind, 1);

    glp_set_mat_row(R_ExternalPtrAddr(lp), Rf_asInteger(i),
                    Rf_asInteger(len), rind, rval
                   );

    return out;
}


/* -------------------------------------------------------------------------- */
/* get column j of the contraint matrix */
SEXP getMatCol(SEXP lp, SEXP j) {

    SEXP out   = R_NilValue;
    SEXP ind   = R_NilValue;
    SEXP val   = R_NilValue;
    SEXP listv = R_NilValue;

    int nnzc = 0;

    checkProb(lp);
    checkColIndex(lp, j);

    nnzc = glp_get_mat_col(R_ExternalPtrAddr(lp), Rf_asInteger(j), NULL, NULL);

    if (nnzc > 0) {
        PROTECT(ind = Rf_allocVector(INTSXP,  nnzc+1));
        PROTECT(val = Rf_allocVector(REALSXP, nnzc+1));

        nnzc = glp_get_mat_col(R_ExternalPtrAddr(lp), Rf_asInteger(j),
                               INTEGER(ind), REAL(val)
                               );

        /* maybe this is ugly */
        REAL(val)[0] = 0;
        INTEGER(ind)[0] = 0;

        PROTECT(out = Rf_allocVector(VECSXP, 3));
        SET_VECTOR_ELT(out, 0, Rf_ScalarInteger(nnzc));
        SET_VECTOR_ELT(out, 1, ind);
        SET_VECTOR_ELT(out, 2, val);

        PROTECT(listv = Rf_allocVector(STRSXP, 3));
        SET_STRING_ELT(listv, 0, Rf_mkChar("nnz"));
        SET_STRING_ELT(listv, 1, Rf_mkChar("index"));
        SET_STRING_ELT(listv, 2, Rf_mkChar("value"));
        Rf_setAttrib(out, R_NamesSymbol, listv);

        UNPROTECT(4);
    }

    return out;
}


/* -------------------------------------------------------------------------- */
/* set column j of the contraint matrix */
SEXP setMatCol(SEXP lp, SEXP j, SEXP len, SEXP ind, SEXP val) {

    SEXP out   = R_NilValue;

    const int *rind;
    const double *rval;

    if (ind == R_NilValue) {
        rind = NULL;
    }
    else {
        rind = INTEGER(ind);
    }

    if (val == R_NilValue) {
        rval = NULL;
    }
    else {
        rval = REAL(val);
    }

    checkProb(lp);
    checkColIndex(lp, j);
    checkRowIndices(lp, ind, 1);

    glp_set_mat_col(R_ExternalPtrAddr(lp), Rf_asInteger(j),
                    Rf_asInteger(len), rind, rval
                   );

    return out;
}


/* -------------------------------------------------------------------------- */
/* read problem data in MPS format */
SEXP readMPS(SEXP lp, SEXP fmt, SEXP fname) {

    SEXP out = R_NilValue;
    const char *rfname = CHAR(STRING_ELT(fname, 0));
    int check = 0;
    int fm;

    checkProb(lp);

    fm = (Rf_asInteger(fmt) == GLP_MPS_DECK) ? GLP_MPS_DECK : GLP_MPS_FILE;

    check = glp_read_mps(R_ExternalPtrAddr(lp), fm, NULL, rfname);

    out = Rf_ScalarInteger(check);

    return out;
}


/* -------------------------------------------------------------------------- */
/* read problem data in CPLEX LP format */
SEXP readLP(SEXP lp, SEXP fname) {

    SEXP out = R_NilValue;
    const char *rfname = CHAR(STRING_ELT(fname, 0));
    int check = 0;

    checkProb(lp);

    check = glp_read_lp(R_ExternalPtrAddr(lp), NULL, rfname);

    out = Rf_ScalarInteger(check);

    return out;
}


/* -------------------------------------------------------------------------- */
/* read problem data in GLPK format */
SEXP readProb(SEXP lp, SEXP fname) {

    SEXP out = R_NilValue;
    const char *rfname = CHAR(STRING_ELT(fname, 0));
    int check = 0;
    int flags = 0;

    checkProb(lp);

    check = glp_read_prob(R_ExternalPtrAddr(lp), flags, rfname);

    out = Rf_ScalarInteger(check);

    return out;
}


/* -------------------------------------------------------------------------- */
/* write problem data in MPS format */
SEXP writeMPS(SEXP lp, SEXP fmt, SEXP fname) {

    SEXP out = R_NilValue;
    const char *rfname = CHAR(STRING_ELT(fname, 0));
    int check = 0;
    int fm;

    checkProb(lp);

    fm = (Rf_asInteger(fmt) == GLP_MPS_DECK) ? GLP_MPS_DECK : GLP_MPS_FILE;

    check = glp_write_mps(R_ExternalPtrAddr(lp), fm, NULL, rfname);

    out = Rf_ScalarInteger(check);

    return out;
}


/* -------------------------------------------------------------------------- */
/* write problem data in CPLEX LP format */
SEXP writeLP(SEXP lp, SEXP fname) {

    SEXP out = R_NilValue;
    const char *rfname = CHAR(STRING_ELT(fname, 0));
    int check = 0;

    checkProb(lp);

    check = glp_write_lp(R_ExternalPtrAddr(lp), NULL, rfname);

    out = Rf_ScalarInteger(check);

    return out;
}


/* -------------------------------------------------------------------------- */
/* write problem data in GLPK format */
SEXP writeProb(SEXP lp, SEXP fname) {

    SEXP out = R_NilValue;
    const char *rfname = CHAR(STRING_ELT(fname, 0));
    int check = 0;
    int flags = 0;

    checkProb(lp);

    check = glp_write_prob(R_ExternalPtrAddr(lp), flags, rfname);

    out = Rf_ScalarInteger(check);

    return out;
}


/* -------------------------------------------------------------------------- */
/* write basic solution in printable format */
SEXP printSol(SEXP lp, SEXP fname) {

    SEXP out = R_NilValue;
    const char *rfname = CHAR(STRING_ELT(fname, 0));
    int check = 0;

    checkProb(lp);

    check = glp_print_sol(R_ExternalPtrAddr(lp), rfname);

    out = Rf_ScalarInteger(check);

    return out;
}


/* -------------------------------------------------------------------------- */
/* read basic solution from text file */
SEXP readSol(SEXP lp, SEXP fname) {

    SEXP out = R_NilValue;
    const char *rfname = CHAR(STRING_ELT(fname, 0));
    int check = 0;

    checkProb(lp);

    check = glp_read_sol(R_ExternalPtrAddr(lp), rfname);

    out = Rf_ScalarInteger(check);

    return out;
}


/* -------------------------------------------------------------------------- */
/* write basic solution to text file */
SEXP writeSol(SEXP lp, SEXP fname) {

    SEXP out = R_NilValue;
    const char *rfname = CHAR(STRING_ELT(fname, 0));
    int check = 0;

    checkProb(lp);

    check = glp_write_sol(R_ExternalPtrAddr(lp), rfname);

    out = Rf_ScalarInteger(check);

    return out;
}


/* -------------------------------------------------------------------------- */
/* write interior-point solution in printable format */
SEXP printIpt(SEXP lp, SEXP fname) {

    SEXP out = R_NilValue;
    const char *rfname = CHAR(STRING_ELT(fname, 0));
    int check = 0;

    checkProb(lp);

    check = glp_print_ipt(R_ExternalPtrAddr(lp), rfname);

    out = Rf_ScalarInteger(check);

    return out;
}


/* -------------------------------------------------------------------------- */
/* read interior-point solution from text file */
SEXP readIpt(SEXP lp, SEXP fname) {

    SEXP out = R_NilValue;
    const char *rfname = CHAR(STRING_ELT(fname, 0));
    int check = 0;

    checkProb(lp);

    check = glp_read_ipt(R_ExternalPtrAddr(lp), rfname);

    out = Rf_ScalarInteger(check);

    return out;
}


/* -------------------------------------------------------------------------- */
/* write interior-point solution to text file */
SEXP writeIpt(SEXP lp, SEXP fname) {

    SEXP out = R_NilValue;
    const char *rfname = CHAR(STRING_ELT(fname, 0));
    int check = 0;

    checkProb(lp);

    check = glp_write_ipt(R_ExternalPtrAddr(lp), rfname);

    out = Rf_ScalarInteger(check);

    return out;
}


/* -------------------------------------------------------------------------- */
/* write MIP solution in printable format */
SEXP printMIP(SEXP lp, SEXP fname) {

    SEXP out = R_NilValue;
    const char *rfname = CHAR(STRING_ELT(fname, 0));
    int check = 0;

    checkProb(lp);

    check = glp_print_mip(R_ExternalPtrAddr(lp), rfname);

    out = Rf_ScalarInteger(check);

    return out;
}


/* -------------------------------------------------------------------------- */
/* read MIP solution from text file */
SEXP readMIP(SEXP lp, SEXP fname) {

    SEXP out = R_NilValue;
    const char *rfname = CHAR(STRING_ELT(fname, 0));
    int check = 0;

    checkProb(lp);

    check = glp_read_mip(R_ExternalPtrAddr(lp), rfname);

    out = Rf_ScalarInteger(check);

    return out;
}


/* -------------------------------------------------------------------------- */
/* write MIP solution to text file */
SEXP writeMIP(SEXP lp, SEXP fname) {

    SEXP out = R_NilValue;
    const char *rfname = CHAR(STRING_ELT(fname, 0));
    int check = 0;

    checkProb(lp);

    check = glp_write_mip(R_ExternalPtrAddr(lp), rfname);

    out = Rf_ScalarInteger(check);

    return out;
}


/* -------------------------------------------------------------------------- */
/* determine library version */
SEXP version(void) {

    SEXP out = R_NilValue;

    const char *vstr = glp_version();

    out = Rf_mkString(vstr);

    return out;
}


/* -------------------------------------------------------------------------- */
/* check if the basis factorization exists */
SEXP bfExists(SEXP lp) {

    SEXP out = R_NilValue;
    int bex = 0;

    checkProb(lp);

    bex = glp_bf_exists(R_ExternalPtrAddr(lp));

    out = Rf_ScalarInteger(bex);


    return out;
}


/* -------------------------------------------------------------------------- */
/* compute the basis factorization */
SEXP factorize(SEXP lp) {

    SEXP out = R_NilValue;
    int fex = 0;

    checkProb(lp);

    fex = glp_factorize(R_ExternalPtrAddr(lp));

    out = Rf_ScalarInteger(fex);

    return out;
}


/* -------------------------------------------------------------------------- */
/* check if the basis factorization has been updated */
SEXP bfUpdated(SEXP lp) {

    SEXP out = R_NilValue;
    int bup = 0;

    checkProb(lp);

    bup = glp_bf_updated(R_ExternalPtrAddr(lp));

    out = Rf_ScalarInteger(bup);

    return out;
}


/* -------------------------------------------------------------------------- */
/* change basis factorization control parameters */
SEXP setBfcp(SEXP lp, SEXP npari, SEXP pari, SEXP vali,
                      SEXP npard, SEXP pard, SEXP vald) {

    SEXP out   = R_NilValue;

    glp_bfcp parmB;

    int *rpari;
    int *rvali;

    int *rpard;
    double *rvald;

    int i, d;

    checkProb(lp);

    /* get current values of control parameters */
    glp_get_bfcp(R_ExternalPtrAddr(lp), &parmB);

    if (Rf_asInteger(npari) == 0) {
        rpari = NULL;
        rvali = NULL;
    }
    else {
        rpari = INTEGER(pari);
        rvali = INTEGER(vali);

        for (i = 0; i < Rf_asInteger(npari); i++) {
            /* Rprintf("par: %i  val: %i\n", rpari[i], rvali[i]); */
            switch (rpari[i]) {
                case 401:
                    parmB.type = rvali[i];
                    break;
                case 402:
                    parmB.lu_size = rvali[i];
                    break;
                case 403:
                    parmB.piv_lim = rvali[i];
                    break;
                case 404:
                    parmB.suhl = rvali[i];
                    break;
                case 405:
                    parmB.nfs_max = rvali[i];
                    break;
                case 406:
                    parmB.nrs_max = rvali[i];
                    break;
                case 407:
                    parmB.rs_size = rvali[i];
                    break;
                default:
                    Rf_warning("Unknown integer basis factorization parameter: %i!", rpari[i]);
                    break;
            }
        }

    }

    if (Rf_asInteger(npard) == 0) {
        rpard = NULL;
        rvald = NULL;
    }
    else {
        rpard = INTEGER(pard);
        rvald = REAL(vald);

        for (d = 0; d < Rf_asInteger(npard); d++) {
            /* Rprintf("par: %i  val: %i\n", rpard[d], rvald[d]); */
            switch (rpard[d]) {
                case 501:
                    parmB.piv_tol = rvald[d];
                    break;
                case 502:
                    parmB.eps_tol = rvald[d];
                    break;
                case 503:
                    parmB.max_gro = rvald[d];
                    break;
                case 504:
                    parmB.upd_tol = rvald[d];
                    break;
                default:
                    Rf_warning("Unknown double basis factorization parameter: %i!", rpard[d]);
                    break;
            }
        }

    }

    /* set new values of control parameters */
    glp_set_bfcp(R_ExternalPtrAddr(lp), &parmB);

    return out;
}


/* -------------------------------------------------------------------------- */
/* get basis factorization control parameters */
SEXP getBfcp(SEXP lp) {

    glp_bfcp parmB;

    SEXP listv   = R_NilValue;
    SEXP parmext = R_NilValue;
    SEXP intids  = R_NilValue;
    SEXP dbids   = R_NilValue;

    SEXP pint  = R_NilValue;
    SEXP pdb   = R_NilValue;

    /* get current values of control parameters */
    glp_get_bfcp(R_ExternalPtrAddr(lp), &parmB);

    PROTECT(pint = Rf_allocVector(INTSXP, 7));
    PROTECT(pdb  = Rf_allocVector(REALSXP, 4));

    INTEGER(pint)[0] = parmB.type;
    INTEGER(pint)[1] = parmB.lu_size;
    INTEGER(pint)[2] = parmB.piv_lim;
    INTEGER(pint)[3] = parmB.suhl;
    INTEGER(pint)[4] = parmB.nfs_max;
    INTEGER(pint)[5] = parmB.nrs_max;
    INTEGER(pint)[6] = parmB.rs_size;

    REAL(pdb)[0] = parmB.piv_tol;
    REAL(pdb)[1] = parmB.eps_tol;
    REAL(pdb)[2] = parmB.max_gro;
    REAL(pdb)[3] = parmB.upd_tol;

    PROTECT(intids = Rf_allocVector(STRSXP, 7));
    SET_STRING_ELT(intids, 0, Rf_mkChar("type"));
    SET_STRING_ELT(intids, 1, Rf_mkChar("lu_size"));
    SET_STRING_ELT(intids, 2, Rf_mkChar("piv_lim"));
    SET_STRING_ELT(intids, 3, Rf_mkChar("suhl"));
    SET_STRING_ELT(intids, 4, Rf_mkChar("nfs_max"));
    SET_STRING_ELT(intids, 5, Rf_mkChar("nrs_max"));
    SET_STRING_ELT(intids, 6, Rf_mkChar("rs_size"));

    PROTECT(dbids = Rf_allocVector(STRSXP, 4));
    SET_STRING_ELT(dbids, 0, Rf_mkChar("piv_tol"));
    SET_STRING_ELT(dbids, 1, Rf_mkChar("eps_tol"));
    SET_STRING_ELT(dbids, 2, Rf_mkChar("max_gro"));
    SET_STRING_ELT(dbids, 3, Rf_mkChar("upd_tol"));

    Rf_setAttrib(pint, R_NamesSymbol, intids);
    Rf_setAttrib(pdb, R_NamesSymbol, dbids);

    PROTECT(parmext = Rf_allocVector(VECSXP, 2));
    SET_VECTOR_ELT(parmext, 0, pint);
    SET_VECTOR_ELT(parmext, 1, pdb);

    PROTECT(listv = Rf_allocVector(STRSXP, 2));
    SET_STRING_ELT(listv, 0, Rf_mkChar("integer"));
    SET_STRING_ELT(listv, 1, Rf_mkChar("double"));

    Rf_setAttrib(parmext, R_NamesSymbol, listv);

    UNPROTECT(6);

    return parmext;

}


/* -------------------------------------------------------------------------- */
/* retrieve basis header information */
SEXP getBhead(SEXP lp, SEXP k) {

    SEXP out = R_NilValue;
    int bh;

    checkProb(lp);

    bh = glp_get_bhead(R_ExternalPtrAddr(lp), Rf_asInteger(k));

    out = Rf_ScalarInteger(bh);

    return out;
}


/* -------------------------------------------------------------------------- */
/* retrieve row index in the basis header */
SEXP getRbind(SEXP lp, SEXP i) {

    SEXP out = R_NilValue;
    int rh;

    checkProb(lp);
    checkRowIndex(lp, i);

    rh = glp_get_row_bind(R_ExternalPtrAddr(lp), Rf_asInteger(i));

    out = Rf_ScalarInteger(rh);

    return out;
}


/* -------------------------------------------------------------------------- */
/* retrieve column index in the basis header */
SEXP getCbind(SEXP lp, SEXP j) {

    SEXP out = R_NilValue;
    int ch;

    checkProb(lp);
    checkColIndex(lp, j);

    ch = glp_get_col_bind(R_ExternalPtrAddr(lp), Rf_asInteger(j));

    out = Rf_ScalarInteger(ch);

    return out;
}


/* -------------------------------------------------------------------------- */
/* print sensitivity analysis report */
SEXP printRanges(SEXP lp, SEXP numrc, SEXP rowcol, SEXP fname) {

    SEXP out = R_NilValue;

    const int *rrowcol;
    const char *rfname = CHAR(STRING_ELT(fname, 0));
    int sensit;

    if (rowcol == R_NilValue) {
        rrowcol = NULL;
    }
    else {
        rrowcol = INTEGER(rowcol);
    }

    checkProb(lp);

    sensit = glp_print_ranges(R_ExternalPtrAddr(lp), Rf_asInteger(numrc),
                              rrowcol, 0, rfname
                             );

    out = Rf_ScalarInteger(sensit);

    return out;

}


/* -------------------------------------------------------------------------- */
/* allocate translator workspace */
SEXP mplAllocWksp(SEXP ptrtype) {

    SEXP wkext = R_NilValue;
    SEXP ptr, class;
    
    glp_tran *wk;

    /* create translator workspace pointer */
    PROTECT(ptr = Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(ptr, 0, STRING_ELT(ptrtype, 0));

    PROTECT(class = Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(class, 0, Rf_mkChar("trwks_ptr"));

    wk = glp_mpl_alloc_wksp();

    wkext = R_MakeExternalPtr(wk, tagMATHprog, R_NilValue);
    PROTECT(wkext);
    R_RegisterCFinalizerEx(wkext, mathProgFinalizer, TRUE);
    Rf_setAttrib(ptr, class, wkext);
    Rf_classgets(ptr, class);
    UNPROTECT(3);

    return ptr;
}


/* -------------------------------------------------------------------------- */
/* free translator workspace */
SEXP mplFreeWksp(SEXP wksp) {

    SEXP out = R_NilValue;
    glp_tran *delwk = NULL;

    checkMathProg(wksp);

    delwk = R_ExternalPtrAddr(wksp);

    glp_mpl_free_wksp(delwk);
    R_ClearExternalPtr(wksp);

    return out;
}


/* -------------------------------------------------------------------------- */
/* read and translate model section */
SEXP mplReadModel(SEXP wk, SEXP fname, SEXP skip) {

    SEXP out = R_NilValue;
    const char *rfname = CHAR(STRING_ELT(fname, 0));
    int check = 0;

    checkMathProg(wk);

    if ( setjmp(jenv) ) {
        glp_error_hook( NULL, NULL );
        return out;
    }

    ge.e = 100;
    glp_error_hook( (func) &cleanGLPKerror, &ge );

    check = glp_mpl_read_model(R_ExternalPtrAddr(wk),
                               rfname, Rf_asInteger(skip));

    glp_error_hook( NULL, NULL );
    
    if (check != 0) {
        out = Rf_ScalarInteger(check);
    }

    return out;
}


/* -------------------------------------------------------------------------- */
/* read and translate data section */
SEXP mplReadData(SEXP wk, SEXP fname) {

    SEXP out = R_NilValue;
    const char *rfname = CHAR(STRING_ELT(fname, 0));
    int check = 0;

    checkMathProg(wk);

    if ( setjmp(jenv) ) {
        glp_error_hook( NULL, NULL );
        return out;
    }

    ge.e = 100;
    glp_error_hook( (func) &cleanGLPKerror, &ge );

    check = glp_mpl_read_data(R_ExternalPtrAddr(wk), rfname);

    glp_error_hook( NULL, NULL );

    if (check != 0) {
        out = Rf_ScalarInteger(check);
    }

    return out;
}


/* -------------------------------------------------------------------------- */
/* generate the model */
SEXP mplGenerate(SEXP wk, SEXP fname) {

    SEXP out = R_NilValue;
    const char *rfname;
    int check = 0;

    checkMathProg(wk);

    if (fname == R_NilValue) {
        rfname = NULL;
    }
    else {
        rfname = CHAR(STRING_ELT(fname, 0));
    }

    if ( setjmp(jenv) ) {
        glp_error_hook( NULL, NULL );
        return out;
    }

    ge.e = 100;
    glp_error_hook( (func) &cleanGLPKerror, &ge );

    check = glp_mpl_generate(R_ExternalPtrAddr(wk), rfname);

    glp_error_hook( NULL, NULL );

    if (check != 0) {
        out = Rf_ScalarInteger(check);
    }

    return out;
}


/* -------------------------------------------------------------------------- */
/* build problem instance from model */
SEXP mplBuildProb(SEXP wk, SEXP lp) {

    SEXP out = R_NilValue;

    checkMathProg(wk);
    checkProb(lp);

    glp_mpl_build_prob(R_ExternalPtrAddr(wk), R_ExternalPtrAddr(lp));

    return out;
}


/* -------------------------------------------------------------------------- */
/* postsolve model */
SEXP mplPostsolve(SEXP wk, SEXP lp, SEXP sol) {

    SEXP out = R_NilValue;
    int check = 0;

    checkMathProg(wk);
    checkProb(lp);
    checkSolType(sol);

    ge.e = 100;
    glp_error_hook( (func) &cleanGLPKerror, &ge );

    check = glp_mpl_postsolve(R_ExternalPtrAddr(wk),
                              R_ExternalPtrAddr(lp),
                              Rf_asInteger(sol));

    glp_error_hook( NULL, NULL );

    if (check != 0) {
        out = Rf_ScalarInteger(check);
    }

    return out;
}
