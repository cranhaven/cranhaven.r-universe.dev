/* glpkAPI.h
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


/* -------------------------------------------------------------------------- */
/* help functions                                                             */
/* -------------------------------------------------------------------------- */

/* check for pointer to glpk */
SEXP isGLPKptr(SEXP ptr);

/* check for pointer to translator workspace */
SEXP isTRWKSptr(SEXP ptr);

/* check for NULL pointer */
SEXP isNULLptr(SEXP ptr);


/* -------------------------------------------------------------------------- */
/* API functions                                                              */
/* -------------------------------------------------------------------------- */

/* initialize glpk */
SEXP initGLPK(void);

/* remove problem object */
SEXP delProb(SEXP lp);

/* erase problem object content */
SEXP eraseProb(SEXP lp);

/* copy problem object content */
SEXP copyProb(SEXP lp, SEXP clp, SEXP names);

/* create new problem object */
SEXP initProb(SEXP ptrtype);

/* set problem name */
SEXP setProbName(SEXP lp, SEXP pname);

/* get problem name */
SEXP getProbName(SEXP lp);

/* set objective function name */
SEXP setObjName(SEXP lp, SEXP oname);

/* get objective function name */
SEXP getObjName(SEXP lp);

/* create name index */
SEXP createIndex(SEXP lp);

/* delete name index */
SEXP deleteIndex(SEXP lp);

/* create parameter structure for simplex */
SEXP setDefaultSmpParm(void);

/* create parameter structure for interior */
SEXP setDefaultIptParm(void);

/* create parameter structure for MIP */
SEXP setDefaultMIPParm(void);

/* set simplex control parameters */
SEXP setSimplexParm(SEXP npari, SEXP pari, SEXP vali,
                    SEXP npard, SEXP pard, SEXP vald);

/* set interior control parameters */
SEXP setInteriorParm(SEXP npari, SEXP pari, SEXP vali);

/* set MIP control parameters */
SEXP setMIPParm(SEXP npari, SEXP pari, SEXP vali,
                SEXP npard, SEXP pard, SEXP vald);

/* get simplex control parameters */
SEXP getSimplexParm(void);

/* get interior control parameters */
SEXP getInteriorParm(void);

/* get MIP control parameters */
SEXP getMIPParm(void);

/* set optimization direction */
SEXP setObjDir(SEXP lp, SEXP dir);

/* get optimization direction */
SEXP getObjDir(SEXP lp);

/* add rows to the problem object */
SEXP addRows(SEXP lp, SEXP nrows);

/* set row name i */
SEXP setRowName(SEXP lp, SEXP i, SEXP rname);

/* set row names */
SEXP setRowsNames(SEXP lp, SEXP i, SEXP rnames);

/* get row name i */
SEXP getRowName(SEXP lp, SEXP i);

/* find row by its name */
SEXP findRow(SEXP lp, SEXP rname);

/* add collumns to the problem object */
SEXP addCols(SEXP lp, SEXP ncols);

/* set column name j */
SEXP setColName(SEXP lp, SEXP j, SEXP cname);

/* set column names */
SEXP setColsNames(SEXP lp, SEXP j, SEXP cnames);

/* get column name j */
SEXP getColName(SEXP lp, SEXP j);

/* find column by its name */
SEXP findCol(SEXP lp, SEXP cname);

/* get number of rows */
SEXP getNumRows(SEXP lp);

/* get number of columns */
SEXP getNumCols(SEXP lp);

/* set column bounds (for more than one column) */
SEXP setColsBnds(SEXP lp, SEXP j, SEXP type, SEXP lb, SEXP ub);

/* set column bounds and objective coefficients (for more than one column) */
SEXP setColsBndsObjCoefs(SEXP lp, SEXP j, SEXP type,
                         SEXP lb, SEXP ub, SEXP obj_coef);

/* set column bound (for only one column) */
SEXP setColBnd(SEXP lp, SEXP j, SEXP type, SEXP lb, SEXP ub);

/* get column lower bounds (for more than one column) */
SEXP getColsLowBnds(SEXP lp, SEXP j);

/* get column low bound (for only one column) */
SEXP getColLowBnd(SEXP lp, SEXP j);

/* get column upper bounds (for more than one column) */
SEXP getColsUppBnds(SEXP lp, SEXP j);

/* get column upper bound (for only one column) */
SEXP getColUppBnd(SEXP lp, SEXP j);

/* set column kind (for only one column) */
SEXP setColKind(SEXP lp, SEXP j, SEXP kind);

/* set column kind (for more than one column) */
SEXP setColsKind(SEXP lp, SEXP j, SEXP kind);

/* retrieve column kind (for only one column) */
SEXP getColKind(SEXP lp, SEXP j);

/* retrieve column kind (for more than one column) */
SEXP getColsKind(SEXP lp, SEXP j);

/* retrieve number of integer columns */
SEXP getNumInt(SEXP lp);

/* retrieve number of binary columns */
SEXP getNumBin(SEXP lp);

/* set row bounds (for more than one row) */
SEXP setRowsBnds(SEXP lp, SEXP i, SEXP type, SEXP lb, SEXP ub);

/* set right hand side (rhs) to zero (fixed) */
SEXP setRhsZero(SEXP lp);

/* set row bound (for only one row) */
SEXP setRowBnd(SEXP lp, SEXP i, SEXP type, SEXP lb, SEXP ub);

/* get row lower bounds (for more than one row) */
SEXP getRowsLowBnds(SEXP lp, SEXP i);

/* get row low bound (for only one row) */
SEXP getRowLowBnd(SEXP lp, SEXP i);

/* get row upper bounds (for more than one row) */
SEXP getRowsUppBnds(SEXP lp, SEXP i);

/* get row upper bound (for only one row) */
SEXP getRowUppBnd(SEXP lp, SEXP i);

/* get row type */
SEXP getRowType(SEXP lp, SEXP i);

/* get row types (for more than one row) */
SEXP getRowsTypes(SEXP lp, SEXP i);

/* get col type */
SEXP getColType(SEXP lp, SEXP j);

/* set objective coefficients (for more than one column) */
SEXP setObjCoefs(SEXP lp, SEXP j, SEXP obj_coef);

/* set objective coefficient (for only one column) */
SEXP setObjCoef(SEXP lp, SEXP j, SEXP obj_coef);

/* get objective coefficients (for more than one column) */
SEXP getObjCoefs(SEXP lp, SEXP j);

/* get objective coefficient (for only one column) */
SEXP getObjCoef(SEXP lp, SEXP j);

/* load the whole constraint matrix */
SEXP loadMatrix(SEXP lp, SEXP ne, SEXP ia, SEXP ja, SEXP ra);

/* check for duplicate elements in sparse matrix */
SEXP checkDup(SEXP m, SEXP n, SEXP ne, SEXP ia, SEXP ja);

/* sort elements of the constraint matrix */
SEXP sortMatrix(SEXP lp);

/* delete rows from problem object */
SEXP delRows(SEXP lp, SEXP nrows, SEXP i);

/* delete columns from problem object */
SEXP delCols(SEXP lp, SEXP ncols, SEXP j);

/* set row scale factor */
SEXP setRii(SEXP lp, SEXP i, SEXP rii);

/* set column scale factor */
SEXP setSjj(SEXP lp, SEXP j, SEXP sjj);

/* retrieve row scale factor */
SEXP getRii(SEXP lp, SEXP i);

/* retrieve column scale factor */
SEXP getSjj(SEXP lp, SEXP j);

/* problem scaling */
SEXP scaleProb(SEXP lp, SEXP opt);

/* problem unscaling */
SEXP unscaleProb(SEXP lp);

/* set row status */
SEXP setRowStat(SEXP lp, SEXP i, SEXP stat);

/* set column status */
SEXP setColStat(SEXP lp, SEXP j, SEXP stat);

/* construct standard initial LP basis */
SEXP stdBasis(SEXP lp);

/* construct advanced initial LP basis */
SEXP advBasis(SEXP lp);

/* construct Bixby's initial LP basis */
SEXP cpxBasis(SEXP lp);

/* "warm up" LP basis */
SEXP warmUp(SEXP lp);

/* enable/disable terminal output */
SEXP termOut(SEXP flag);

/* solve problem with simplex algorithm */
SEXP solveSimplex(SEXP lp);

/* solve problem with exact simplex algorithm */
SEXP solveSimplexExact(SEXP lp);

/* get value of the objective function after simplex */
SEXP getObjVal(SEXP lp);

/* get solution status after simplex */
SEXP getSolStat(SEXP lp);

/* get column primal value (flux distribution) for all columns */
SEXP getColsPrim(SEXP lp);

/* get column primal value (flux distribution) for one column */
SEXP getColPrim(SEXP lp, SEXP j);

/* retrieve status of primal basic solution */
SEXP getPrimStat(SEXP lp);

/* retrieve status of dual basic solution */
SEXP getDualStat(SEXP lp);

/* retrieve row status */
SEXP getRowStat(SEXP lp, SEXP i);

/* retrieve row status for all rows */
SEXP getRowsStat(SEXP lp);

/* retrieve row primal value */
SEXP getRowPrim(SEXP lp, SEXP i);

/* retrieve row primal value for all rows */
SEXP getRowsPrim(SEXP lp);

/* retrieve row dual value */
SEXP getRowDual(SEXP lp, SEXP i);

/* retrieve row dual value for all rows */
SEXP getRowsDual(SEXP lp);

/* retrieve column status */
SEXP getColStat(SEXP lp, SEXP j);

/* retrieve column status for all columns */
SEXP getColsStat(SEXP lp);

/* retrieve column dual value */
SEXP getColDual(SEXP lp, SEXP j);

/* retrieve column dual value for all columns */
SEXP getColsDual(SEXP lp);

/* determine variable causing unboundedness */
SEXP getUnbndRay(SEXP lp);

/* solve problem with interior point method */
SEXP solveInterior(SEXP lp);

/* get value of the objective function after interior point method */
SEXP getObjValIpt(SEXP lp);

/* get solution status after interior point method */
SEXP getSolStatIpt(SEXP lp);

/* get column primal value (flux distribution) for all columns (interior) */
SEXP getColsPrimIpt(SEXP lp);

/* get column primal value (flux distribution) for one column (interior) */
SEXP getColPrimIpt(SEXP lp, SEXP j);

/* retrieve row primal value (interior) */
SEXP getRowPrimIpt(SEXP lp, SEXP i);

/* retrieve row primal value (interior) for all rows (interior) */
SEXP getRowsPrimIpt(SEXP lp);

/* retrieve row dual value (interior) */
SEXP getRowDualIpt(SEXP lp, SEXP i);

/* retrieve row dual value (interior) for all rows (interior) */
SEXP getRowsDualIpt(SEXP lp);

/* retrieve column dual value (interior) */
SEXP getColDualIpt(SEXP lp, SEXP j);

/* retrieve column dual value (interior) for all columns (interior) */
SEXP getColsDualIpt(SEXP lp);

/* solve MIP problem with the branch-and-cut method */
SEXP solveMIP(SEXP lp);

/* determine status of MIP solution */
SEXP mipStatus(SEXP lp);

/* retrieve objective value */
SEXP mipObjVal(SEXP lp);

/* retrieve row value (MIP) */
SEXP mipRowVal(SEXP lp, SEXP i);

/* retrieve row value for all rows (MIP) */
SEXP mipRowsVal(SEXP lp);

/* retrieve column value (MIP) */
SEXP mipColVal(SEXP lp, SEXP j);

/* retrieve column value for all columns (MIP) */
SEXP mipColsVal(SEXP lp);

/* get the number of constraint coefficients (number of non-zero elements in
   the consrtaint matrix) */
SEXP getNumNnz(SEXP lp);

/* get row i of the contraint matrix */
SEXP getMatRow(SEXP lp, SEXP i);

/* set row i of the contraint matrix */
SEXP setMatRow(SEXP lp, SEXP i, SEXP len, SEXP ind, SEXP val);

/* get column j of the contraint matrix */
SEXP getMatCol(SEXP lp, SEXP j);

/* set column j of the contraint matrix */
SEXP setMatCol(SEXP lp, SEXP j, SEXP len, SEXP ind, SEXP val);

/* read problem data in MPS format */
SEXP readMPS(SEXP lp, SEXP fmt, SEXP fname);

/* read problem data in CPLEX LP format */
SEXP readLP(SEXP lp, SEXP fname);

/* read problem data in GLPK format */
SEXP readProb(SEXP lp, SEXP fname);

/* write problem data in MPS format */
SEXP writeMPS(SEXP lp, SEXP fmt, SEXP fname);

/* write problem data in CPLEX LP format */
SEXP writeLP(SEXP lp, SEXP fname);

/* write problem data in GLPK format */
SEXP writeProb(SEXP lp, SEXP fname);

/* write basic solution in printable format */
SEXP printSol(SEXP lp, SEXP fname);

/* read basic solution from text file */
SEXP readSol(SEXP lp, SEXP fname);

/* write basic solution to text file */
SEXP writeSol(SEXP lp, SEXP fname);

/* write interior-point solution in printable format */
SEXP printIpt(SEXP lp, SEXP fname);

/* read interior-point solution from text file */
SEXP readIpt(SEXP lp, SEXP fname);

/* write interior-point solution to text file */
SEXP writeIpt(SEXP lp, SEXP fname);

/* write MIP solution in printable format */
SEXP printMIP(SEXP lp, SEXP fname);

/* read MIP solution from text file */
SEXP readMIP(SEXP lp, SEXP fname);

/* write MIP solution to text file */
SEXP writeMIP(SEXP lp, SEXP fname);

/* determine library version */
SEXP version(void);

/* check if the basis factorization exists */
SEXP bfExists(SEXP lp);

/* compute the basis factorization */
SEXP factorize(SEXP lp);

/* check if the basis factorization has been updated */
SEXP bfUpdated(SEXP lp);

/* change basis factorization control parameters */
SEXP setBfcp(SEXP lp, SEXP npari, SEXP pari, SEXP vali,
                      SEXP npard, SEXP pard, SEXP vald);

/* get basis factorization control parameters */
SEXP getBfcp(SEXP lp);

/* retrieve basis header information */
SEXP getBhead(SEXP lp, SEXP k);

/* retrieve row index in the basis header */
SEXP getRbind(SEXP lp, SEXP i);

/* retrieve column index in the basis header */
SEXP getCbind(SEXP lp, SEXP j);

/* print sensitivity analysis report */
SEXP printRanges(SEXP lp, SEXP numrc, SEXP rowcol, SEXP fname);

/* allocate translator workspace */
SEXP mplAllocWksp(SEXP ptrtype);

/* free translator workspace */
SEXP mplFreeWksp(SEXP wksp);

/* read and translate model section */
SEXP mplReadModel(SEXP wk, SEXP fname, SEXP skip);

/* read and translate data section */
SEXP mplReadData(SEXP wk, SEXP fname);

/* generate the model */
SEXP mplGenerate(SEXP wk, SEXP fname);

/* build problem instance from model */
SEXP mplBuildProb(SEXP wk, SEXP lp);

/* postsolve model */
SEXP mplPostsolve(SEXP wk, SEXP lp, SEXP sol);

