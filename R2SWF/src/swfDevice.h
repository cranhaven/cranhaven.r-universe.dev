#ifndef SWFDEVICE_H_INCLUDED
#define SWFDEVICE_H_INCLUDED

#include <ming.h>
#include "swfArray.h"
#include "swfFont.h"
#include "swfText.h"

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

#define R_USE_PROTOTYPES 1
#include <R_ext/GraphicsEngine.h>
#include <R_ext/GraphicsDevice.h>

/* SWF device specific data */
typedef struct swfDesc {
    char filename[1024]; /* swf filename */
    SWFMovie m;          /* movie object */
    SWFMovieClip currentFrame;
                         /* current frame */
    SWFDisplayItem currentClip;
                         /* current mask/clip layer */
    SWFArray array;      /* store temporary objects */
    FT_Outline_Funcs outlnFuns;
                         /* functions to draw font outline,
                            defined in swfText.h */
} swfDesc;

typedef swfDesc* pswfDesc;

/************************************************************* 
 
 The followings are utility functions used by plotting handlers
 
*************************************************************/
/* Setup the device descriptions data */
Rboolean swfSetup(pDevDesc dev, const char *filename,
    double width, double height,
    const int *bg, float frameRate);

/* Initialize SWF device specific data */
Rboolean swfSetupSWFInfo(pswfDesc swfInfo, const char *filename,
    double width, double height,
    const int *bg, float frameRate);

/* Function to set line style */
void swfSetLineStyle(SWFShape shape, const pGEcontext gc, pswfDesc swfInfo);
/* Function to set fill style */
void swfSetFillStyle(SWFShape shape, const pGEcontext gc, pswfDesc swfInfo);
/* Function to set text color */
void swfSetTextColor(SWFShape shape, const pGEcontext gc, pswfDesc swfInfo);
/* Draw line respecting lty parameter */
void swfDrawStyledLineTo(SWFShape shape, double x, double y, const pGEcontext gc);



/************************************************************* 
 
 Device plotting function hooks. Defined in R_ext/GraphicsDevice.h
 
*************************************************************/
static void swfActivate(pDevDesc dd);

static void swfCircle(double x, double y, double r, const pGEcontext gc, pDevDesc dd);

static void swfClip(double x0, double x1, double y0, double y1, pDevDesc dd);

static void swfClose(pDevDesc dd);

static void swfDeactivate(pDevDesc dd);

/* static Rboolean swfLocator(double *x, double *y, pDevDesc dd); */

static void swfLine(double x1, double y1, double x2, double y2, const pGEcontext gc, pDevDesc dd);

static void swfMetricInfo(int c, const pGEcontext gc, double* ascent, double* descent, double* width, pDevDesc dd);

static void swfMode(int mode, pDevDesc dd);

static void swfNewPage(const pGEcontext gc, pDevDesc dd);

static void swfPolygon(int n, double *x, double *y, const pGEcontext gc, pDevDesc dd);

static void swfPolyline(int n, double *x, double *y, const pGEcontext gc, pDevDesc dd);

static void swfRect(double x0, double y0, double x1, double y1, const pGEcontext gc, pDevDesc dd);

static void swfPath(double *x, double *y, int npoly, int *nper, Rboolean winding, const pGEcontext gc, pDevDesc dd);

/*
static void swfRaster(unsigned int *raster, int w, int h,
                      double x, double y, 
                      double width, double height,
                      double rot, 
                      Rboolean interpolate,
                      const pGEcontext gc, pDevDesc dd);
*/

/* static SEXP swfCap(pDevDesc dd); */

static void swfSize(double *left, double *right, double *bottom, double *top, pDevDesc dd);

static double swfStrWidth(const char *str, const pGEcontext gc, pDevDesc dd);

static void swfText(double x, double y, const char *str, double rot, double hadj, const pGEcontext gc, pDevDesc dd);

static void swfTextUTF8(double x, double y, const char *str, double rot, double hadj, const pGEcontext gc, pDevDesc dd);

static double swfStrWidthUTF8(const char *str, const pGEcontext gc, pDevDesc dd);

/* Below are added in R_GE_version >= 13 */
static SEXP swfSetPattern(SEXP pattern, pDevDesc dd);

static void swfReleasePattern(SEXP ref, pDevDesc dd);

static SEXP swfSetClipPath(SEXP path, SEXP ref, pDevDesc dd);

static void swfReleaseClipPath(SEXP ref, pDevDesc dd);

static SEXP swfSetMask(SEXP path, SEXP ref, pDevDesc dd);

static void swfReleaseMask(SEXP ref, pDevDesc dd);


#endif /* SWFDEVICE_H_INCLUDED */
