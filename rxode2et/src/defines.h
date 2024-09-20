#define qtest _rxode2et_qtest
#define qstrictS _rxode2et_qstrictS
#define qstrictSn _rxode2et_qstrictSn
#define qstrictSdn _rxode2et_qstrictSdn
#define qassertS _rxode2et_qassertS
#define rx_global rxode2et_rx_global
#define op_global rxode2et_op_global
#define convertId_ _rxode2et_convertId_
#define rxModelVars_ _rxode2et_rxModelVars_
#if defined(__cplusplus)
extern "C" {
#endif
  
  SEXP _rxode2et_convertId_(SEXP id);
  SEXP _rxode2et_qassertS(SEXP in, const char *test, const char *what);

  typedef SEXP (*getEtRxsolveSexp_t)(SEXP);
  extern getEtRxsolveSexp_t _rxode2et_getEtRxsolveSexp_from_rxode2;
  
#if defined(__cplusplus)
}
Rcpp::List _rxode2et_rxModelVars_(SEXP);
#endif  

