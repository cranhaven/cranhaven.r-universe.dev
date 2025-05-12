#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern void c_opf_accuracy(int *, char **);
extern void c_opf_accuracy4label(int *, char **);
extern void c_opf_check(int *, char **);
extern void c_opf_classify(int *, char **);
extern void c_opf_cluster(int *, char **);
extern void c_opf_distance(int *, char **);
extern void c_opf_fold(int *, char **);
extern void c_opf_info(int *, char **);
extern void c_opf_learn(int *, char **);
extern void c_opf_merge(int *, char **);
extern void c_opf_normalize(int *, char **);
extern void c_opf_pruning(int *, char **);
extern void c_opf_semi(int *, char **);
extern void c_opf_split(int *, char **);
extern void c_opf_train(int *, char **);
extern void c_opf2svm(int *, char **);
extern void c_opf2txt(int *, char **);
extern void c_opfknn_classify(int *, char **);
extern void c_opfknn_train(int *, char **);
extern void c_svm2opf(int *, char **);
extern void c_txt2opf(int *, char **);

static const R_CallMethodDef CallEntries[] = {
    {"c_opf_accuracy",       (DL_FUNC) &c_opf_accuracy,       2},
    {"c_opf_accuracy4label", (DL_FUNC) &c_opf_accuracy4label, 2},
    {"c_opf_check",          (DL_FUNC) &c_opf_check,          2},
    {"c_opf_classify",       (DL_FUNC) &c_opf_classify,       2},
    {"c_opf_cluster",        (DL_FUNC) &c_opf_cluster,        2},
    {"c_opf_distance",       (DL_FUNC) &c_opf_distance,       2},
    {"c_opf_fold",           (DL_FUNC) &c_opf_fold,           2},
    {"c_opf_info",           (DL_FUNC) &c_opf_info,           2},
    {"c_opf_learn",          (DL_FUNC) &c_opf_learn,          2},
    {"c_opf_merge",          (DL_FUNC) &c_opf_merge,          2},
    {"c_opf_normalize",      (DL_FUNC) &c_opf_normalize,      2},
    {"c_opf_pruning",        (DL_FUNC) &c_opf_pruning,        2},
    {"c_opf_semi",           (DL_FUNC) &c_opf_semi,           2},
    {"c_opf_split",          (DL_FUNC) &c_opf_split,          2},
	{"c_opf_train",          (DL_FUNC) &c_opf_train,          2},
    {"c_opf2svm",            (DL_FUNC) &c_opf2svm,            2},
    {"c_opf2txt",            (DL_FUNC) &c_opf2txt,            2},
    {"c_opfknn_classify",    (DL_FUNC) &c_opfknn_classify,    2},
    {"c_opfknn_train",       (DL_FUNC) &c_opfknn_train,       2},
    {"c_svm2opf",            (DL_FUNC) &c_svm2opf,            2},
    {"c_txt2opf",            (DL_FUNC) &c_txt2opf,            2},
    {NULL, NULL, 0}
};

void R_init_Libc_opf(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
