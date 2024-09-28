#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

SEXP swfInit(void);
SEXP image2swf(SEXP fileNames, SEXP format, SEXP outName, SEXP bgColor, SEXP interval);
SEXP svg2swf(SEXP filesData, SEXP outName, SEXP size, SEXP bgColor, SEXP interval);
SEXP swfDevice(SEXP filename_r, SEXP width_r, SEXP height_r, SEXP bg_r, SEXP fg_r, SEXP frameRate_r);
void Ming_collectGarbage(void);

static R_CMethodDef c_methods[] = {
    {"Ming_collectGarbage", (DL_FUNC) &Ming_collectGarbage, 0},
    {NULL, NULL, 0}
};

static R_CallMethodDef call_methods[] = {
    {"swfInit",     (DL_FUNC) &swfInit,   0},
    {"image2swf_c", (DL_FUNC) &image2swf, 5},
    {"svg2swf_c",   (DL_FUNC) &svg2swf,   5},
    {"swfDevice",   (DL_FUNC) &swfDevice, 6},
    {NULL, NULL, 0}
};

void R_init_R2SWF(DllInfo* info)
{
    R_registerRoutines(info, c_methods, call_methods, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
}
