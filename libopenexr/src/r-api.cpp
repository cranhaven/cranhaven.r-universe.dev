#define R_NO_REMAP

#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>

#include <ImathBox.h>
#include <ImfArray.h>
#include <ImfRgbaFile.h>

using namespace OPENEXR_IMF_NAMESPACE;
using namespace IMATH_NAMESPACE;

// ---------------------------------------------------------------------
// helpers
inline void check_bool(bool ok, const char *msg) {
  if (!ok)
    Rf_error("%s", msg);
}

// ---------------------------------------------------------------------
// .Call("C_read_exr", "path/to/file.exr")
extern "C" SEXP C_read_exr(SEXP path_SEXP) {
  const char *path = CHAR(STRING_ELT(path_SEXP, 0));

  try {
    RgbaInputFile file(path);
    Box2i dw = file.dataWindow();
    const int w = dw.max.x - dw.min.x + 1;
    const int h = dw.max.y - dw.min.y + 1;

    Imf::Array2D<Rgba> px;
    px.resizeErase(h, w);

    // FrameBuffer mapping identical to the example in the docs
    file.setFrameBuffer(&px[0][0] - dw.min.x - dw.min.y * w, 1, w);
    file.readPixels(dw.min.y, dw.max.y);

    // Allocate matrices (column‑major in R, but we’ll fill row‑major then
    // transpose)
    SEXP rMat = PROTECT(Rf_allocMatrix(REALSXP, h, w));
    SEXP gMat = PROTECT(Rf_allocMatrix(REALSXP, h, w));
    SEXP bMat = PROTECT(Rf_allocMatrix(REALSXP, h, w));
    SEXP aMat = PROTECT(Rf_allocMatrix(REALSXP, h, w));

    double *r = REAL(rMat), *g = REAL(gMat), *b = REAL(bMat), *a = REAL(aMat);

    // Copy, row‑major → column‑major
    for (int y = 0; y < h; ++y) {
      for (int x = 0; x < w; ++x) {
        const Rgba &p = px[y][x];
        const ptrdiff_t pos = x * h + y; // col‑major index
        r[pos] = p.r;
        g[pos] = p.g;
        b[pos] = p.b;
        a[pos] = p.a;
      }
    }

    // Return list
    SEXP out = PROTECT(Rf_allocVector(VECSXP, 6));
    SET_VECTOR_ELT(out, 0, rMat);
    SET_VECTOR_ELT(out, 1, gMat);
    SET_VECTOR_ELT(out, 2, bMat);
    SET_VECTOR_ELT(out, 3, aMat);
    SET_VECTOR_ELT(out, 4, Rf_ScalarInteger(w));
    SET_VECTOR_ELT(out, 5, Rf_ScalarInteger(h));

    SEXP names = PROTECT(Rf_allocVector(STRSXP, 6));
    const char *nms[6] = {"r", "g", "b", "a", "width", "height"};
    for (int i = 0; i < 6; ++i) {
      SET_STRING_ELT(names, i, Rf_mkChar(nms[i]));
    }
    Rf_setAttrib(out, R_NamesSymbol, names);

    UNPROTECT(6);
    return out;
  } catch (const std::exception &e) {
    Rf_error("OpenEXR read error: %s", e.what());
  }
}

// ---------------------------------------------------------------------
// .Call("C_write_exr", path, r, g, b, a, width, height)
extern "C" SEXP C_write_exr(SEXP path_SEXP, SEXP rMat, SEXP gMat, SEXP bMat,
                            SEXP aMat, SEXP w_SEXP, SEXP h_SEXP) {
  const char *path = CHAR(STRING_ELT(path_SEXP, 0));
  const int w = INTEGER(w_SEXP)[0];
  const int h = INTEGER(h_SEXP)[0];

  check_bool(Rf_isMatrix(rMat) && Rf_isMatrix(gMat) && Rf_isMatrix(bMat) &&
                 Rf_isMatrix(aMat),
             "All channels must be matrices");
  check_bool(Rf_nrows(rMat) == h && Rf_ncols(rMat) == w, "Dimension mismatch");

  const double *r = REAL(rMat), *g = REAL(gMat), *b = REAL(bMat),
               *a = REAL(aMat);

  Imf::Array2D<Rgba> px;
  px.resizeErase(h, w);

  for (int y = 0; y < h; ++y) {
    for (int x = 0; x < w; ++x) {
      const ptrdiff_t pos = x * h + y; // col‑major index
      px[y][x].r = static_cast<float>(r[pos]);
      px[y][x].g = static_cast<float>(g[pos]);
      px[y][x].b = static_cast<float>(b[pos]);
      px[y][x].a = static_cast<float>(a[pos]);
    }
  }

  try {
    RgbaOutputFile file(path, w, h, WRITE_RGBA);
    file.setFrameBuffer(&px[0][0], 1, w);
    file.writePixels(h);
  } catch (const std::exception &e) {
    Rf_error("OpenEXR write error: %s", e.what());
  }

  return R_NilValue;
}

// ---------------------------------------------------------------------
// registration
static const R_CallMethodDef callTable[] = {
    {"C_read_exr", (DL_FUNC)&C_read_exr, 1},
    {"C_write_exr", (DL_FUNC)&C_write_exr, 7},
    {NULL, NULL, 0}};

extern "C" void R_init_libopenexr(DllInfo *dll) {
  R_registerRoutines(dll, NULL, callTable, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
