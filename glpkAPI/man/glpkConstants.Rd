\name{glpkConstants}
\alias{glpkConstants}
\alias{glpk_Constants}
\alias{constantsGLPK}

\alias{BINARIZE}
\alias{BR_TECH}
\alias{BT_TECH}
\alias{CB_FUNC}
\alias{CB_SIZE}
\alias{CLQ_CUTS}
\alias{COV_CUTS}
\alias{EPS_TOL}
\alias{FP_HEUR}
\alias{GMI_CUTS}
\alias{IT_LIM}
\alias{LU_SIZE}
\alias{MAX_GRO}
\alias{METH}
\alias{MIP_GAP}
\alias{MIR_CUTS}
\alias{MSG_LEV}
\alias{NFS_MAX}
\alias{NRS_MAX}
\alias{OBJ_LL}
\alias{OBJ_UL}
\alias{ORD_ALG}
\alias{OUT_DLY}
\alias{OUT_FRQ}
\alias{PIV_LIM}
\alias{PIV_TOL}
\alias{PP_TECH}
\alias{PRESOLVE}
\alias{PRICING}
\alias{R_TEST}
\alias{RS_SIZE}
\alias{SUHL}
\alias{TM_LIM}
\alias{TOL_BND}
\alias{TOL_DJ}
\alias{TOL_INT}
\alias{TOL_OBJ}
\alias{TOL_PIV}
\alias{TYPE}
\alias{UPD_TOL}

\alias{GLP_MIN}
\alias{GLP_MAX}
\alias{GLP_CV}
\alias{GLP_IV}
\alias{GLP_BV}
\alias{GLP_FR}
\alias{GLP_LO}
\alias{GLP_UP}
\alias{GLP_DB}
\alias{GLP_FX}
\alias{GLP_BS}
\alias{GLP_NL}
\alias{GLP_NU}
\alias{GLP_NF}
\alias{GLP_NS}
\alias{GLP_SF_GM}
\alias{GLP_SF_EQ}
\alias{GLP_SF_2N}
\alias{GLP_SF_SKIP}
\alias{GLP_SF_AUTO}
\alias{GLP_SOL}
\alias{GLP_IPT}
\alias{GLP_MIP}
\alias{GLP_UNDEF}
\alias{GLP_FEAS}
\alias{GLP_INFEAS}
\alias{GLP_NOFEAS}
\alias{GLP_OPT}
\alias{GLP_UNBND}
\alias{GLP_BF_FT}
\alias{GLP_BF_BG}
\alias{GLP_BF_GR}
\alias{GLP_BF_LUF}
\alias{GLP_BF_BTF}
\alias{GLP_MSG_OFF}
\alias{GLP_MSG_ERR}
\alias{GLP_MSG_ON}
\alias{GLP_MSG_ALL}
\alias{GLP_MSG_DBG}
\alias{GLP_PRIMAL}
\alias{GLP_DUALP}
\alias{GLP_DUAL}
\alias{GLP_PT_STD}
\alias{GLP_PT_PSE}
\alias{GLP_RT_STD}
\alias{GLP_RT_HAR}
\alias{GLP_ORD_NONE}
\alias{GLP_ORD_QMD}
\alias{GLP_ORD_AMD}
\alias{GLP_ORD_SYMAMD}
\alias{GLP_BR_FFV}
\alias{GLP_BR_LFV}
\alias{GLP_BR_MFV}
\alias{GLP_BR_DTH}
\alias{GLP_BR_PCH}
\alias{GLP_BT_DFS}
\alias{GLP_BT_BFS}
\alias{GLP_BT_BLB}
\alias{GLP_BT_BPH}
\alias{GLP_PP_NONE}
\alias{GLP_PP_ROOT}
\alias{GLP_PP_ALL}
\alias{GLP_RF_REG}
\alias{GLP_RF_LAZY}
\alias{GLP_RF_CUT}
\alias{GLP_RF_GMI}
\alias{GLP_RF_MIR}
\alias{GLP_RF_COV}
\alias{GLP_RF_CLQ}
\alias{GLP_ON}
\alias{GLP_OFF}
\alias{GLP_IROWGEN}
\alias{GLP_IBINGO}
\alias{GLP_IHEUR}
\alias{GLP_ICUTGEN}
\alias{GLP_IBRANCH}
\alias{GLP_ISELECT}
\alias{GLP_IPREPRO}
\alias{GLP_NO_BRNCH}
\alias{GLP_DN_BRNCH}
\alias{GLP_UP_BRNCH}
\alias{GLP_EBADB}
\alias{GLP_ESING}
\alias{GLP_ECOND}
\alias{GLP_EBOUND}
\alias{GLP_EFAIL}
\alias{GLP_EOBJLL}
\alias{GLP_EOBJUL}
\alias{GLP_EITLIM}
\alias{GLP_ETMLIM}
\alias{GLP_ENOPFS}
\alias{GLP_ENODFS}
\alias{GLP_EROOT}
\alias{GLP_ESTOP}
\alias{GLP_EMIPGAP}
\alias{GLP_ENOFEAS}
\alias{GLP_ENOCVG}
\alias{GLP_EINSTAB}
\alias{GLP_EDATA}
\alias{GLP_ERANGE}
\alias{GLP_KKT_PE}
\alias{GLP_KKT_PB}
\alias{GLP_KKT_DE}
\alias{GLP_KKT_DB}
\alias{GLP_KKT_CS}
\alias{GLP_MPS_DECK}
\alias{GLP_MPS_FILE}

\title{
  Constants, Return and Status Codes of GLPK
}

\description{
  This is a list containing constants used by GLPK. Cunsult the glpk manual
  for more information, in praticular for the control parameters.
}

%------------------------------------------------------------------------------
\section{Control Parameters}{

  \emph{Simplex}
  \tabular{ll}{
    \code{MSG_LEV  <- 101} \tab Message level for terminal output (default: \code{GLP_MSG_ALL}). \cr
    \code{METH     <- 102} \tab Simplex method option (default: \code{GLP_PRIMAL}). \cr
    \code{PRICING  <- 103} \tab Pricing technique (default: \code{GLP_PT_PSE}). \cr
    \code{R_TEST   <- 104} \tab Ratio test technique (default: \code{GLP_RT_HAR}). \cr
    \code{IT_LIM   <- 105} \tab Simplex iteration limit (default: \code{INT_MAX}). \cr
    \code{TM_LIM   <- 106} \tab Searching time limit, in milliseconds (default: \code{INT_MAX}). \cr
    \code{OUT_FRQ  <- 107} \tab Output frequency, in iterations (default: \code{500}). \cr
    \code{OUT_DLY  <- 108} \tab Output delay, in milliseconds (default: \code{0}). \cr
    \code{PRESOLVE <- 109} \tab LP presolver option (default: \code{GLP_OFF}). \cr
    \code{TOL_BND  <- 201} \tab Tolerance used to check if the basic solution is primal feasible (default: \code{1e-7}). \cr
    \code{TOL_DJ   <- 202} \tab Tolerance used to check if the basic solution is dual feasible (default: \code{1e-7}). \cr
    \code{TOL_PIV  <- 203} \tab Tolerance used to choose eligble pivotal elements of the simplex table (default: \code{1e-10}). \cr
    \code{OBJ_LL   <- 204} \tab Lower limit of the objective function (default: \code{-DBL_MAX}). \cr
    \code{OBJ_UL   <- 205} \tab Upper limit of the objective function (default: \code{DBL_MAX}). \cr
  }

  The exact simplex method uses only the parameters \code{IT_LIM} and \code{TM_LIM}.

  \emph{Interior}
  \tabular{ll}{
    \code{MSG_LEV  <- 101} \tab Message level for terminal output (default: \code{GLP_MSG_ALL}). \cr
    \code{ORD_ALG  <- 301} \tab Ordering algorithm used prior to Cholesky factorization (default: \code{GLP_ORD_AMD}). \cr
  }

  \emph{MIP}
  \tabular{ll}{
    \code{MSG_LEV  <- 101} \tab Message level for terminal output (default: \code{GLP_MSG_ALL}). \cr
    \code{TM_LIM   <- 106} \tab Searching time limit, in milliseconds (default: \code{INT_MAX}). \cr
    \code{OUT_FRQ  <- 107} \tab Output frequency, in iterations (default: \code{5000}). \cr
    \code{OUT_DLY  <- 108} \tab Output delay, in milliseconds (default: \code{10000}). \cr
    \code{PRESOLVE <- 109} \tab MIP presolver option (default: \code{GLP_OFF}). \cr
    \code{BR_TECH  <- 601} \tab Branching technique option (default: \code{GLP_BR_DTH}). \cr
    \code{BT_TECH  <- 602} \tab Backtracking technique option (default: \code{GLP_BT_BLB}). \cr
    \code{PP_TECH  <- 603} \tab Preprocessing technique option (default: \code{GLP_PP_ALL}). \cr
    \code{FP_HEUR  <- 604} \tab Feasibility pump heuristic option (default: \code{GLP_OFF}). \cr
    \code{GMI_CUTS <- 605} \tab Gomory's mixed integer cut option (default: \code{GLP_OFF}). \cr
    \code{MIR_CUTS <- 606} \tab Mixed integer rounding (MIR) cut option (default: \code{GLP_OFF}). \cr
    \code{COV_CUTS <- 607} \tab Mixed cover cut option (default: \code{GLP_OFF}). \cr
    \code{CLQ_CUTS <- 608} \tab Clique cut option (default: \code{GLP_OFF}). \cr
    \code{CB_SIZE  <- 609} \tab The number of extra (up to 256) bytes allocated for each node of the branch-and-bound tree to store application-specific data. On creating a node these bytes are initialized by binary zeros (default: \code{0}). \cr
    \code{BINARIZE <- 610} \tab LP presolver option (default: \code{GLP_OFF}). \cr
    \code{CB_FUNC  <- 651} \tab Use a user defined callback routine \code{glpkCallback} which is written in the file \file{glpkCallback.c}. This file should be edited according to the users requirements. If set to \code{GLP_ON}, the callback routine defined there is used (default: \code{NULL}). \cr
    \code{TOL_INT  <- 701} \tab Absolute tolerance used to check if optimal solution to the current LP relaxation is integer feasible (default: \code{1e-5}). \cr
    \code{TOL_OBJ  <- 702} \tab Relative tolerance used to check if the objective value in optimal solution to the current LP relaxation is not better than in the best known inte- ger feasible solution (default: \code{1e-7}). \cr
    \code{MIP_GAP  <- 703} \tab The relative mip gap tolerance. If the relative mip gap for currently known best integer feasible solution falls below this tolerance, the solver terminates the search. This allows obtainig suboptimal integer feasible solutions if solving the problem to optimality takes too long time (default: \code{0.0}). \cr
  }

  \emph{Basis Factorization}
  \tabular{ll}{
    \code{TYPE     <- 401} \tab Basis factorization type (default: \code{GLP_BF_FT}). \cr
    \code{LU_SIZE  <- 402} \tab Initial size of the Sparse Vector Area (default: \code{0}). \cr
    \code{PIV_LIM  <- 403} \tab computing LU-factorization of the basis matrix (default: \code{4}). \cr
    \code{SUHL     <- 404} \tab computing LU-factorization of the basis matrix (default: \code{GLP_ON}). \cr
    \code{NFS_MAX  <- 405} \tab Maximal number of additional row-like factors (default: \code{100}). \cr
    \code{NRS_MAX  <- 406} \tab Maximal number of additional rows and columns (default: \code{100}). \cr
    \code{RS_SIZE  <- 407} \tab Initial size of the Sparse Vector Area (default: \code{0}). \cr
    \code{PIV_TOL  <- 501} \tab Threshold pivoting (Markowitz) tolerance (default: \code{0.10}). \cr
    \code{EPS_TOL  <- 502} \tab Epsilon tolerance (default: \code{1e-15}). \cr
    \code{MAX_GRO  <- 503} \tab Maximal growth of elements of factor U (default: \code{1e+10}). \cr
    \code{UPD_TOL  <- 504} \tab Update tolerance (default: \code{1e-6}). \cr
  }

}


%------------------------------------------------------------------------------
\section{LP/MIP problem object}{

  \emph{optimization direction flag}
  \tabular{ll}{
    \code{GLP_MIN <- 1} \tab minimization \cr
    \code{GLP_MAX <- 2} \tab maximization \cr
  }

  \emph{kind of structural variable}
  \tabular{ll}{
    \code{GLP_CV <- 1} \tab continuous variable \cr
    \code{GLP_IV <- 2} \tab integer variable \cr
    \code{GLP_BV <- 3} \tab binary variable \cr
  }
  
  \emph{type of auxiliary/structural variable}
  \tabular{ll}{
    \code{GLP_FR <- 1} \tab free variable \cr
    \code{GLP_LO <- 2} \tab variable with lower bound \cr
    \code{GLP_UP <- 3} \tab variable with upper bound \cr
    \code{GLP_DB <- 4} \tab double-bounded variable \cr
    \code{GLP_FX <- 5} \tab fixed variable \cr
  }
  
  \emph{status of auxiliary/structural variable}
  \tabular{ll}{
    \code{GLP_BS <- 1} \tab basic variable \cr
    \code{GLP_NL <- 2} \tab non-basic variable on lower bound \cr
    \code{GLP_NU <- 3} \tab non-basic variable on upper bound \cr
    \code{GLP_NF <- 4} \tab non-basic free variable \cr
    \code{GLP_NS <- 5} \tab non-basic fixed variable \cr
  }
  
  \emph{scaling options}
  \tabular{ll}{
    \code{GLP_SF_GM   <- 0x01} \tab perform geometric mean scaling \cr
    \code{GLP_SF_EQ   <- 0x10} \tab perform equilibration scaling \cr
    \code{GLP_SF_2N   <- 0x20} \tab round scale factors to power of two \cr
    \code{GLP_SF_SKIP <- 0x40} \tab skip if problem is well scaled \cr
    \code{GLP_SF_AUTO <- 0x80} \tab choose scaling options automatically \cr
  }
  
  \emph{solution indicator}
  \tabular{ll}{
    \code{GLP_SOL <- 1} \tab basic solution \cr
    \code{GLP_IPT <- 2} \tab interior-point solution \cr
    \code{GLP_MIP <- 3} \tab mixed integer solution \cr
  }
  
  \emph{solution status}
  \tabular{ll}{
    \code{GLP_UNDEF  <- 1} \tab solution is undefined \cr
    \code{GLP_FEAS   <- 2} \tab solution is feasible \cr
    \code{GLP_INFEAS <- 3} \tab solution is infeasible \cr
    \code{GLP_NOFEAS <- 4} \tab no feasible solution exists \cr
    \code{GLP_OPT    <- 5} \tab solution is optimal \cr
    \code{GLP_UNBND  <- 6} \tab solution is unbounded \cr
  }
}

%------------------------------------------------------------------------------
\section{basis factorization control parameters}{

  \emph{type}
  \tabular{ll}{
    \code{GLP_BF_FT <-  0x01} \tab LUF + Forrest-Tomlin \cr
    \code{GLP_BF_BG <-  0x02} \tab LUF + Schur compl. + Bartels-Golub \cr
    \code{GLP_BF_GR <-  0x03} \tab LUF + Schur compl. + Givens rotation \cr
    \code{GLP_BF_LUF <- 0x00} \tab plain LU-factorization \cr
    \code{GLP_BF_BTF <- 0x10} \tab block triangular LU-factorization \cr
  }
}

%------------------------------------------------------------------------------
\section{simplex method control parameters}{

  \emph{msg_lev} message level:
  \tabular{ll}{
    \code{GLP_MSG_OFF <- 0} \tab no output \cr
    \code{GLP_MSG_ERR <- 1} \tab warning and error messages only \cr
    \code{GLP_MSG_ON  <- 2} \tab normal output \cr
    \code{GLP_MSG_ALL <- 3} \tab full output \cr
    \code{GLP_MSG_DBG <- 4} \tab debug output \cr
  }

  \emph{meth} simplex method option:
  \tabular{ll}{
    \code{GLP_PRIMAL <- 1} \tab use primal simplex \cr
    \code{GLP_DUALP  <- 2} \tab use dual; if it fails, use primal \cr
    \code{GLP_DUAL   <- 3} \tab use dual simplex \cr
  }

  \emph{pricing} pricing technique:
  \tabular{ll}{
    \code{GLP_PT_STD <- 0x11} \tab standard (Dantzig rule) \cr
    \code{GLP_PT_PSE <- 0x22} \tab projected steepest edge \cr
  }

  \emph{r_test} ratio test technique:
  \tabular{ll}{
    \code{GLP_RT_STD <- 0x11} \tab standard (textbook) \cr
    \code{GLP_RT_HAR <- 0x22} \tab two-pass Harris' ratio test \cr
  }
}

%------------------------------------------------------------------------------
\section{interior-point solver control parameters}{

  \emph{ord_alg} ordering algorithm:
  \tabular{ll}{
    \code{GLP_ORD_NONE   <- 0} \tab natural (original) ordering \cr
    \code{GLP_ORD_QMD    <- 1} \tab quotient minimum degree (QMD) \cr
    \code{GLP_ORD_AMD    <- 2} \tab approx. minimum degree (AMD) \cr
    \code{GLP_ORD_SYMAMD <- 3} \tab approx. minimum degree (SYMAMD) \cr
  }
}

%------------------------------------------------------------------------------
\section{integer optimizer control parameters}{

  \emph{br_tech} branching technique:
  \tabular{ll}{
    \code{GLP_BR_FFV <- 1} \tab first fractional variable \cr
    \code{GLP_BR_LFV <- 2} \tab last fractional variable \cr
    \code{GLP_BR_MFV <- 3} \tab most fractional variable \cr
    \code{GLP_BR_DTH <- 4} \tab heuristic by Driebeck and Tomlin \cr
    \code{GLP_BR_HPC <- 5} \tab hybrid pseudocost \cr
  }

  \emph{bt_tech} backtracking technique:
  \tabular{ll}{
    \code{GLP_BT_DFS <- 1} \tab depth first search \cr
    \code{GLP_BT_BFS <- 2} \tab breadth first search \cr
    \code{GLP_BT_BLB <- 3} \tab best local bound \cr
    \code{GLP_BT_BPH <- 4} \tab best projection heuristic \cr
  }

  \emph{pp_tech} preprocessing technique:
  \tabular{ll}{
    \code{GLP_PP_NONE <- 0} \tab disable preprocessing \cr
    \code{GLP_PP_ROOT <- 1} \tab preprocessing only on root level \cr
    \code{GLP_PP_ALL  <- 2} \tab preprocessing on all levels \cr
  }
}

%------------------------------------------------------------------------------
\section{additional row attributes}{

  \emph{the row origin flag}
  \tabular{ll}{
    \code{GLP_RF_REG  <- 0} \tab regular constraint \cr
    \code{GLP_RF_LAZY <- 1} \tab "lazy" constraint \cr
    \code{GLP_RF_CUT  <- 2} \tab cutting plane constraint \cr
  }

  \emph{the row class descriptor} klass
  \tabular{ll}{
    \code{GLP_RF_GMI <- 1} \tab Gomory's mixed integer cut \cr
    \code{GLP_RF_MIR <- 2} \tab mixed integer rounding cut \cr
    \code{GLP_RF_COV <- 3} \tab mixed cover cut \cr
    \code{GLP_RF_CLQ <- 4} \tab clique cut \cr
  }
}

%------------------------------------------------------------------------------
\section{enable/disable flag}{

  \tabular{ll}{
    \code{GLP_ON  <- 1} \tab enable something \cr
    \code{GLP_OFF <- 0} \tab disable something \cr
  }
}

%------------------------------------------------------------------------------
\section{reason codes}{
  \tabular{ll}{
    \code{GLP_IROWGEN <- 0x01} \tab request for row generation \cr
    \code{GLP_IBINGO  <- 0x02} \tab better integer solution found \cr
    \code{GLP_IHEUR   <- 0x03} \tab request for heuristic solution \cr
    \code{GLP_ICUTGEN <- 0x04} \tab request for cut generation \cr
    \code{GLP_IBRANCH <- 0x05} \tab request for branching \cr
    \code{GLP_ISELECT <- 0x06} \tab request for subproblem selection \cr
    \code{GLP_IPREPRO <- 0x07} \tab request for preprocessing \cr
  }
}

%------------------------------------------------------------------------------
\section{branch selection indicator}{

  \tabular{ll}{
    \code{GLP_NO_BRNCH <- 0} \tab select no branch \cr
    \code{GLP_DN_BRNCH <- 1} \tab select down-branch \cr
    \code{GLP_UP_BRNCH <- 2} \tab select up-branch \cr
  }
}

%------------------------------------------------------------------------------
\section{return codes}{

  \tabular{ll}{
    \code{GLP_EBADB   <- 0x01} \tab invalid basis \cr
    \code{GLP_ESING   <- 0x02} \tab singular matrix \cr
    \code{GLP_ECOND   <- 0x03} \tab ill-conditioned matrix \cr
    \code{GLP_EBOUND  <- 0x04} \tab invalid bounds \cr
    \code{GLP_EFAIL   <- 0x05} \tab solver failed \cr
    \code{GLP_EOBJLL  <- 0x06} \tab objective lower limit reached \cr
    \code{GLP_EOBJUL  <- 0x07} \tab objective upper limit reached \cr
    \code{GLP_EITLIM  <- 0x08} \tab iteration limit exceeded \cr
    \code{GLP_ETMLIM  <- 0x09} \tab time limit exceeded \cr
    \code{GLP_ENOPFS  <- 0x0A} \tab no primal feasible solution \cr
    \code{GLP_ENODFS  <- 0x0B} \tab no dual feasible solution \cr
    \code{GLP_EROOT   <- 0x0C} \tab root LP optimum not provided \cr
    \code{GLP_ESTOP   <- 0x0D} \tab search terminated by application \cr
    \code{GLP_EMIPGAP <- 0x0E} \tab relative mip gap tolerance reached \cr
    \code{GLP_ENOFEAS <- 0x0F} \tab no primal/dual feasible solution \cr
    \code{GLP_ENOCVG  <- 0x10} \tab no convergence \cr
    \code{GLP_EINSTAB <- 0x11} \tab numerical instability \cr
    \code{GLP_EDATA   <- 0x12} \tab invalid data \cr
    \code{GLP_ERANGE  <- 0x13} \tab result out of range \cr
  }
}

%------------------------------------------------------------------------------
\section{condition indicator}{

  \tabular{ll}{
    \code{GLP_KKT_PE <- 1} \tab primal equalities \cr
    \code{GLP_KKT_PB <- 2} \tab primal bounds \cr
    \code{GLP_KKT_DE <- 3} \tab dual equalities \cr
    \code{GLP_KKT_DB <- 4} \tab dual bounds \cr
    \code{GLP_KKT_CS <- 5} \tab complementary slackness \cr
  }
}

%------------------------------------------------------------------------------
\section{MPS file format}{

  \tabular{ll}{
    \code{GLP_MPS_DECK <- 1} \tab fixed (ancient) \cr
    \code{GLP_MPS_FILE <- 2} \tab free (modern) \cr
  }
}


\references{
  Based on the package \pkg{glpk} by Lopaka Lee.
    
  The GNU GLPK home page at \url{http://www.gnu.org/software/glpk/glpk.html}.
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Mayo Roettger <mayo.roettger@hhu.de>
}


\seealso{
  \code{\link{status_codeGLPK}}, \code{\link{return_codeGLPK}}
}

\keyword{ optimize }


