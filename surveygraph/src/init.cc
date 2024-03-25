#include <stdlib.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP rmake_proj_agent_lcc(SEXP df, SEXP mvalue, SEXP c, SEXP sm);
extern SEXP rmake_proj_agent_ad(SEXP df, SEXP mvalue, SEXP c, SEXP sm);
extern SEXP rmake_proj_agent_similar(SEXP df, SEXP mvalue, SEXP c, SEXP sm);
extern SEXP rmake_proj_symbolic_lcc(SEXP df, SEXP mvalue, SEXP c, SEXP sm);
extern SEXP rmake_proj_symbolic_ad(SEXP df, SEXP mvalue, SEXP c, SEXP sm);
extern SEXP rmake_proj_symbolic_similar(SEXP df, SEXP mvalue, SEXP c, SEXP sm);
extern SEXP rmake_threshold_profile_agent(SEXP m);
extern SEXP rmake_threshold_profile_symbolic(SEXP m);

extern SEXP archived_inputoutput(SEXP m, SEXP n);
extern SEXP archived_hwinteger(SEXP a, SEXP b);
extern SEXP archived_hwnumeric(SEXP a, SEXP b);
extern SEXP archived_dftypes(SEXP a);
extern SEXP archived_vectormanip(SEXP m);
extern SEXP archived_dfmanip(SEXP m);

static const R_CallMethodDef R_CallDef[] = {
  // R package entry points
  {"rmake_proj_agent_lcc",         (DL_FUNC) &rmake_proj_agent_lcc, 4},
  {"rmake_proj_agent_ad",          (DL_FUNC) &rmake_proj_agent_ad, 4},
  {"rmake_proj_agent_similar",     (DL_FUNC) &rmake_proj_agent_similar, 4},

  {"rmake_proj_symbolic_lcc",      (DL_FUNC) &rmake_proj_symbolic_lcc, 4},
  {"rmake_proj_symbolic_ad",       (DL_FUNC) &rmake_proj_symbolic_ad, 4},
  {"rmake_proj_symbolic_similar",  (DL_FUNC) &rmake_proj_symbolic_similar, 4},

  {"rmake_threshold_profile_agent",      (DL_FUNC) &rmake_threshold_profile_agent, 1},
  {"rmake_threshold_profile_symbolic",   (DL_FUNC) &rmake_threshold_profile_symbolic, 1},

  // archived routines, could call but we don't
  {"archived_inputoutput",      (DL_FUNC) &archived_inputoutput, 2},
  {"archived_hwinteger",        (DL_FUNC) &archived_hwinteger, 2},
  {"archived_hwnumeric",        (DL_FUNC) &archived_hwnumeric, 2},
  {"archived_dftypes",          (DL_FUNC) &archived_dftypes, 1},
  {"archived_vectormanip",      (DL_FUNC) &archived_vectormanip, 1},
  {"archived_dfmanip",          (DL_FUNC) &archived_dfmanip, 1},

  {NULL, NULL, 0}
};

extern "C" void R_init_surveygraph(DllInfo *dll) 
{
  R_registerRoutines(dll, NULL, R_CallDef, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
