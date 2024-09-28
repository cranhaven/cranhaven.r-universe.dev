#include "swfDevice.h"

/* #define SWF_DEBUG */

/* Function called by R to open swf device */
SEXP swfDevice(SEXP filename_r, SEXP width_r, SEXP height_r,
               SEXP bg_r, SEXP fg_r, SEXP frameRate_r)
{
    /* This is the device object used by graphics engine */
    pGEDevDesc gdd;
    /* This is the description of graphics device,
       including physical characteristics, plotting functions, etc. */
    pDevDesc dev;
    /* Retrieve information from arguments, and then pass them
       to setup function */
    const char *filename = CHAR(STRING_ELT(filename_r, 0));
    double width = REAL(width_r)[0];
    double height = REAL(height_r)[0];
    const int *bg = INTEGER(bg_r);
    float frameRate = (float) REAL(frameRate_r)[0];

    /* Check if the version of graphics engine matches */
    R_GE_checkVersionOrDie(R_GE_version);
    /* Check if there is enough place to allocate a device */
    R_CheckDeviceAvailable();
    BEGIN_SUSPEND_INTERRUPTS {
    /* Allocate and initialize the device description data */
    if(!(dev = (pDevDesc) calloc(1, sizeof(DevDesc))))
        return 0;
    if(!swfSetup(dev, filename, width, height, bg, frameRate))
    {
        free(dev);
        Rf_error("unable to start swf device");
    }
    
    gdd = GEcreateDevDesc(dev);
    GEaddDevice2(gdd, "swf");
    } END_SUSPEND_INTERRUPTS;

    return R_NilValue;
}

/* Function to setup device description data,
   like physical characteristics and plotting functions */
Rboolean swfSetup(pDevDesc dev, const char *filename,
    double width, double height,
    const int *bg, float frameRate)
{
    /* Create object to store swf device specific data  */
    pswfDesc swfInfo;
    if(!(swfInfo = (pswfDesc) calloc(1, sizeof(swfDesc))))
        return FALSE;
    
    /* Setup swfInfo data */
    swfSetupSWFInfo(swfInfo, filename, width, height, bg, frameRate);
    
    /* Comments coming form R_ext/GraphicsDevice.h */
    /********************************************************
     * Device physical characteristics
     ********************************************************/
    /*
     (0, 0) --------------> x   (width, 0)
       |
       |
       |
       |
       |
       v
       
       y
     
     (0, height)
    */
    /* width and height are specified by inches.
       Here we convert them to points. */
    dev->left = 0;                   /* left raster coordinate */
    dev->right = width * 72.0;       /* right raster coordinate */
    dev->bottom = height * 72.0;     /* bottom raster coordinate */
    dev->top = 0;                    /* top raster coordinate */
    
    /*
    dev->clipLeft;
    dev->clipRight;
    dev->clipBottom;
    dev->clipTop;
    */
    
    dev->xCharOffset = 0.4900;            /* x character addressing offset - unused */
    dev->yCharOffset = 0.3333;            /* y character addressing offset */
    dev->yLineBias = 0.2;                 /* 1/2 interline space as frac of line height */
    dev->ipr[0] = dev->ipr[1] = 1.0 / 72; /* Inches per raster; [0]=x, [1]=y */
    
    dev->cra[0] = 12 * 0.9;               /* Character size in rasters; [0]=x, [1]=y */
    dev->cra[1] = 12 * 1.2;
    
    /*
    dev->gamma;
    */

    /********************************************************
     * Device capabilities
     ********************************************************/
    dev->canClip = TRUE;             /* Device-level clipping */
    dev->canChangeGamma = FALSE;     /* can the gamma factor be modified? */
    dev->canHAdj = 2;                /* Can do at least some horiz adjust of text
                                        0 = none, 1 = {0,0.5,1}, 2 = [0,1] */
    
    /********************************************************
     * Device initial settings
     ********************************************************/
    dev->startps = 12;               /* initial pointsize for text */
    dev->startcol = R_RGB(0, 0, 0);  /* sets par("fg"), par("col") and gpar("col") */
    dev->startfill = R_RGB(bg[0], bg[1], bg[2]);
                                     /* sets par("bg") and gpar("fill") */
    dev->startlty = LTY_SOLID;       /* solid line */
    dev->startfont = 1;              /* plain text */
    dev->startgamma = 1;
    
    /********************************************************
     * Device specific information
     ********************************************************/
    dev->deviceSpecific = (void *) swfInfo;    /* pointer to device specific parameters */
    
    /********************************************************
     * Device display list
     ********************************************************/
    dev->displayListOn = FALSE;      /* toggle for initial display list status */
    
    /********************************************************
     * Event handling entries
     ********************************************************/
    dev->canGenMouseDown = FALSE;    /* can the device generate mousedown events */
    dev->canGenMouseMove = FALSE;    /* can the device generate mousemove events */
    dev->canGenMouseUp = FALSE;      /* can the device generate mouseup events */
    dev->canGenKeybd = FALSE;        /* can the device generate keyboard events */
    dev->gettingEvent = FALSE;       /* This is set while getGraphicsEvent
                                        is actively looking for events */
    
    /********************************************************
     * Device procedures.
     ********************************************************/
    dev->activate = swfActivate;
    dev->circle = swfCircle;
    dev->clip = swfClip;
    dev->close = swfClose;
    dev->deactivate = swfDeactivate;
    dev->locator = NULL;
    dev->line = swfLine;
    dev->metricInfo = swfMetricInfo;
    dev->mode = swfMode;
    dev->newPage = swfNewPage;
    dev->polygon = swfPolygon;
    dev->polyline = swfPolyline;
    dev->rect = swfRect;
    dev->path = NULL;
    dev->raster = NULL;
    dev->cap = NULL;
    dev->size = swfSize;
    dev->strWidth = swfStrWidth;
    dev->text = swfText;
    dev->onExit = NULL;
    dev->getEvent = NULL;
    dev->newFrameConfirm = NULL;
    dev->hasTextUTF8 = TRUE;
    dev->textUTF8 = swfTextUTF8;
    dev->strWidthUTF8 = swfStrWidthUTF8;
    dev->wantSymbolUTF8 = TRUE;
    dev->useRotatedTextInContour = TRUE;
    dev->eventEnv = NULL;
    dev->eventHelper = NULL;
    dev->holdflush = NULL;
    dev->haveTransparency = 2;       /* 1 = no, 2 = yes */
    dev->haveTransparentBg = 1;      /* 1 = no, 2 = fully, 3 = semi */
    dev->haveRaster = 1;             /* 1 = no, 2 = yes, 3 = except for missing values */
    dev->haveCapture = 1;            /* 1 = no, 2 = yes */
    dev->haveLocator = 1;            /* 1 = no, 2 = yes */
#if R_GE_version >= 13
    dev->setPattern      = swfSetPattern;
    dev->releasePattern  = swfReleasePattern;
    dev->setClipPath     = swfSetClipPath;
    dev->releaseClipPath = swfReleaseClipPath;
    dev->setMask         = swfSetMask;
    dev->releaseMask     = swfReleaseMask;

    dev->deviceVersion = R_GE_definitions;
#endif
    return TRUE;
}

/* Initialize swf device specific data,
   for example swf movie object */
Rboolean swfSetupSWFInfo(pswfDesc swfInfo, const char *filename,
    double width, double height,
    const int *bg, float frameRate)
{
    /* Filename */
    strcpy(swfInfo->filename, filename);
    
    /* Movie object */
    swfInfo->m = newSWFMovieWithVersion(8);
    /* Set swf background color */
    SWFMovie_setBackground(swfInfo->m, (byte) (bg[0]), (byte) (bg[1]), (byte) (bg[2]));
    /* Set movie frame rate */
    SWFMovie_setRate(swfInfo->m, frameRate);
    /* Set movie dimension */
    SWFMovie_setDimension(swfInfo->m, (float) (width * 72.0), (float) (height * 72.0));
    /* Set number of frames, only useful when you want to insert
       blank frames at the end of the movie. Otherwise it will be
       increased automatically every time you call
       SWFMovie_nextFrame() */
    SWFMovie_setNumberOfFrames(swfInfo->m, 1);
    
    /* Pointer to the current frame (a MovieClip holding several shapes) */
    swfInfo->currentFrame = NULL;
    
    /* Pointer to the clip layer, a SWFShape object */
    swfInfo->currentClip = NULL;
    
    /* Initialize SWFArray, used to store SWFFillStyle objects
       and free them when movie is written to hard disk */
    swfInfo->array = newSWFArray(100);
    
    /* Functions to draw font outline, used by swfTextUTF8() */
    swfInfo->outlnFuns.move_to = outlineMoveTo;
    swfInfo->outlnFuns.line_to = outlineLineTo;
    swfInfo->outlnFuns.conic_to = outlineConicTo;
    swfInfo->outlnFuns.cubic_to = outlineCubicTo;
    swfInfo->outlnFuns.shift = 0;
    swfInfo->outlnFuns.delta = 0;
    
    return TRUE;
}

void swfActivate(pDevDesc dd) {}
void swfDeactivate(pDevDesc dd) {}

/* We do the following when opening a new page (frame):
   1. Add all pending shape objects to the current frame
   2. Add current frame to the movie
   3. Advance movie by 1 frame
   4. Since in Flash, when you advance the movie to the next frame,
      all items in the previous frame will be preserved.
      To keep in accordance with R's plotting model, we need to
      remove the content of previous frame
   5. Create a new empty canvas (frame)
   6. Reset the clip layer
*/
void swfNewPage(const pGEcontext gc, pDevDesc dd)
{
#ifdef SWF_DEBUG
    Rprintf("newPage called\n");
#endif
    pswfDesc swfInfo = (pswfDesc) dd->deviceSpecific;
    SWFDisplayItem display = NULL;
    
    /* Add current frame to the movie */
    if(swfInfo->currentFrame)
    {
        SWFMovieClip_nextFrame(swfInfo->currentFrame);
        display = SWFMovie_add(swfInfo->m, (SWFBlock) swfInfo->currentFrame);
        SWFMovie_nextFrame(swfInfo->m);
    }
    /* To move to the next frame, we need to first clear the previous one */
    if(display) SWFMovie_remove(swfInfo->m, display);
    /* Create a new canvas(frame) for next plot() */
    swfInfo->currentFrame = newSWFMovieClip();
    SWFMovieClip_setNumberOfFrames(swfInfo->currentFrame, 1);
    /* Renew the clip layer */
    swfInfo->currentClip = NULL;
}

void swfClip(double x0, double x1, double y0, double y1, pDevDesc dd)
{
#ifdef SWF_DEBUG
    Rprintf("clip called\n");
#endif
    pswfDesc swfInfo = (pswfDesc) dd->deviceSpecific;
    SWFShape shape = newSWFShape();
    SWFFillStyle fill;
    
    /* Set previous clip layer to the appropriate depth */
    if(swfInfo->currentClip)
    {
        SWFDisplayItem_endMask(swfInfo->currentClip);
    }
    
    /* Create new clip layer */
    fill = newSWFSolidFillStyle(0xFF, 0xFF, 0xFF, 0xFF);
    SWFShape_setRightFillStyle(shape, fill);
    SWFArray_append(swfInfo->array, (SWFObject) fill);
    
    SWFShape_movePenTo(shape, x0, y0);
    SWFShape_drawLineTo(shape, x1, y0);
    SWFShape_drawLineTo(shape, x1, y1);
    SWFShape_drawLineTo(shape, x0, y1);
    SWFShape_drawLineTo(shape, x0, y0);
    SWFShape_end(shape);
    swfInfo->currentClip = SWFMovieClip_add(swfInfo->currentFrame, (SWFBlock) shape);
    SWFDisplayItem_setMaskLevel(swfInfo->currentClip, 99999);
}

/**********************************
 * Below are supporting functions *
 **********************************/
void swfSetLineStyle(SWFShape shape, const pGEcontext gc, pswfDesc swfInfo)
{
    int cap = SWF_LINESTYLE_CAP_ROUND;
    int endcap = SWF_LINESTYLE_FLAG_ENDCAP_ROUND;
    int join = SWF_LINESTYLE_JOIN_ROUND;
    switch(gc->lend)
    {
        case GE_ROUND_CAP:
            cap = SWF_LINESTYLE_CAP_ROUND;
            endcap = SWF_LINESTYLE_FLAG_ENDCAP_ROUND;
            break;
        case GE_BUTT_CAP:
            cap = SWF_LINESTYLE_CAP_NONE;
            endcap = SWF_LINESTYLE_FLAG_ENDCAP_NONE;
            break;
        case GE_SQUARE_CAP:
            cap = SWF_LINESTYLE_CAP_SQUARE;
            endcap = SWF_LINESTYLE_FLAG_ENDCAP_SQUARE;
            break;
    }
    switch(gc->ljoin)
    {
        case GE_ROUND_JOIN:
            join = SWF_LINESTYLE_JOIN_ROUND;
            break;
        case GE_MITRE_JOIN:
            join = SWF_LINESTYLE_JOIN_MITER;
            break;
        case GE_BEVEL_JOIN:
            join = SWF_LINESTYLE_JOIN_BEVEL;
            break;
    }
    SWFShape_setLine2(shape, (unsigned short) gc->lwd,
                             R_RED(gc->col),
                             R_GREEN(gc->col),
                             R_BLUE(gc->col),
                             R_ALPHA(gc->col),
                             cap | endcap | join,
                             (float) gc->lmitre);
}

void swfSetFillStyle(SWFShape shape, const pGEcontext gc, pswfDesc swfInfo)
{
    SWFFillStyle fill;
    fill = newSWFSolidFillStyle(R_RED(gc->fill),
                                R_GREEN(gc->fill),
                                R_BLUE(gc->fill),
                                R_ALPHA(gc->fill));
    SWFShape_setLeftFillStyle(shape, fill);
    
    SWFArray_append(swfInfo->array, (SWFObject) fill);
}

/* R uses gc->col for text color, but actually we need to "fill" the outlines */
void swfSetTextColor(SWFShape shape, const pGEcontext gc, pswfDesc swfInfo)
{
    SWFFillStyle fill;
    fill = newSWFSolidFillStyle(R_RED(gc->col),
                                R_GREEN(gc->col),
                                R_BLUE(gc->col),
                                R_ALPHA(gc->col));
    SWFShape_setRightFillStyle(shape, fill);
    
    SWFArray_append(swfInfo->array, (SWFObject) fill);
}

void swfDrawStyledLineTo(SWFShape shape, double x, double y, const pGEcontext gc)
{
    int lty = gc->lty;
    int lwd = (int) (gc->lwd);
    /* Original positions */
    double x0 = SWFShape_getPenX(shape);
    double y0 = SWFShape_getPenY(shape);
    double x_next, y_next;
    /* Distance between (x0, y0) and (x, y) */
    double dist = sqrt(pow(x - x0, 2) + pow(y - y0, 2));
    
    unsigned char dashlist[8];
    int ndash = 0;
    /* Length of one cycle of dashlines */
    int cycle_len = 0;
    /* How many full cycles will we have in drawing the line? */
    int ncycle = 0;
    /* How many pen-up and pen-down segments? */
    int nseg = 0;
    /* Distance the pen has moved */
    /* s is a temp variable */
    double dist_moved = 0.0, s = 0.0;
    int i;
    
    /* Is the pen for next segment up or down? */
    Rboolean is_down = TRUE;
    
    /* If it is a solid line */
    /* NOTE: Here lty == 0 corresponds to lty = 1 in R */
    if(lty == LTY_SOLID) /* LTY_SOLID == 0 */
    {
        SWFShape_drawLineTo(shape, x, y);
        return;
    }
    if(lty == LTY_BLANK) /* LTY_BLANK == -1 */
    {
        SWFShape_movePenTo(shape, x, y);
        return;
    }

    /* Decode texture description */
    for(i = 0; i < 8 && lty & 15; i++)
    {
        dashlist[ndash++] = lty & 15;
        lty = lty >> 4;
        cycle_len += dashlist[i];
    }
    
    /* Cycle length proportional to lwd */
    cycle_len *= lwd;
    ncycle = (int) floor(dist / cycle_len);
    nseg = ncycle * ndash;
    /* Length of the last incomplete cycle */
    s = dist - cycle_len * ncycle;
    for(i = 0; i < ndash; i++)
    {
        if(dist_moved + dashlist[i] * lwd >= s)
            break;

        dist_moved += dashlist[i] * lwd;
        nseg++;
    }

    dist_moved = 0.0;
    for(i = 0; i < nseg; i++)
    {
        dist_moved += dashlist[i % ndash] * lwd;
        x_next = x0 + dist_moved / dist * (x - x0);
        y_next = y0 + dist_moved / dist * (y - y0);
        if(is_down)
        {
            SWFShape_drawLineTo(shape, x_next, y_next);
        } else {
            SWFShape_movePenTo(shape, x_next, y_next);
        }
        is_down = !is_down;
    }
    if(is_down)
    {
        SWFShape_drawLineTo(shape, x, y);
    } else {
        SWFShape_movePenTo(shape, x, y);
    }
}

/* The following two functions are copied from R/src/main/util.c */
static size_t utf8toucs(wchar_t *wc, const char *s)
{
    unsigned int byte;
    wchar_t local, *w;
    byte = *((unsigned char *)s);
    w = wc ? wc: &local;

    if (byte == 0)
    {
        *w = (wchar_t) 0;
        return 0;
    }
    else if (byte < 0xC0)
    {
        *w = (wchar_t) byte;
        return 1;
    }
    else if (byte < 0xE0)
    {
        if(strlen(s) < 2) return (size_t)-2;
        if ((s[1] & 0xC0) == 0x80)
        {
            *w = (wchar_t) (((byte & 0x1F) << 6) | (s[1] & 0x3F));
            return 2;
        }
        else return (size_t)-1;
    }
    else if (byte < 0xF0)
    {
        if(strlen(s) < 3) return (size_t)-2;
        if (((s[1] & 0xC0) == 0x80) && ((s[2] & 0xC0) == 0x80))
        {
            *w = (wchar_t) (((byte & 0x0F) << 12)
                            | (unsigned int) ((s[1] & 0x3F) << 6)
                            | (s[2] & 0x3F));
            byte = (unsigned int) *w;
            /* Surrogates range */
            if(byte >= 0xD800 && byte <= 0xDFFF) return (size_t)-1;
            if(byte == 0xFFFE || byte == 0xFFFF) return (size_t)-1;
            return 3;
        }
        else return (size_t)-1;
    }
    if(sizeof(wchar_t) < 4) return (size_t)-2;
    /* So now handle 4,5.6 byte sequences with no testing */
    if (byte < 0xf8)
    {
        if(strlen(s) < 4) return (size_t)-2;
        *w = (wchar_t) (((byte & 0x0F) << 18)
                        | (unsigned int) ((s[1] & 0x3F) << 12)
                        | (unsigned int) ((s[2] & 0x3F) << 6)
                        | (s[3] & 0x3F));
        return 4;
    }
    else if (byte < 0xFC)
    {
        if(strlen(s) < 5) return (size_t)-2;
        *w = (wchar_t) (((byte & 0x0F) << 24)
                        | (unsigned int) ((s[1] & 0x3F) << 12)
                        | (unsigned int) ((s[2] & 0x3F) << 12)
                        | (unsigned int) ((s[3] & 0x3F) << 6)
                        | (s[4] & 0x3F));
        return 5;
    }
    else
    {
        if(strlen(s) < 6) return (size_t)-2;
        *w = (wchar_t) (((byte & 0x0F) << 30)
                        | (unsigned int) ((s[1] & 0x3F) << 24)
                        | (unsigned int) ((s[2] & 0x3F) << 18)
                        | (unsigned int) ((s[3] & 0x3F) << 12)
                        | (unsigned int) ((s[4] & 0x3F) << 6)
                        | (s[5] & 0x3F));
        return 6;
    }
}

static int utf8towcs(wchar_t *wc, const char *s, int n)
{
    ssize_t m, res = 0;
    const char *t;
    wchar_t *p;
    wchar_t local;

    if(wc)
        for(p = wc, t = s; ; p++, t += m)
        {
            m  = (ssize_t) utf8toucs(p, t);
            if (m < 0) Rf_error("invalid input '%s' in 'utf8towcs'", s);
            if (m == 0) break;
            res ++;
            if (res >= n) break;
        }
    else
        for(t = s; ; res++, t += m)
        {
            m  = (ssize_t) utf8toucs(&local, t);
            if (m < 0) Rf_error("invalid input '%s' in 'utf8towcs'", s);
            if (m == 0) break;
        }
    return (int) res;
}


/**********************************
 * Device plotting function hooks *
 **********************************/
void swfRect(double x0, double y0, double x1, double y1, const pGEcontext gc, pDevDesc dd)
{
#ifdef SWF_DEBUG
    Rprintf("rect called\n");
#endif
    pswfDesc swfInfo = (pswfDesc) dd->deviceSpecific;
    SWFShape shape = newSWFShape();
    swfSetLineStyle(shape, gc, swfInfo);
    swfSetFillStyle(shape, gc, swfInfo);
    
    SWFShape_movePenTo(shape, x0, y0);
    swfDrawStyledLineTo(shape, x1, y0, gc);
    swfDrawStyledLineTo(shape, x1, y1, gc);
    swfDrawStyledLineTo(shape, x0, y1, gc);
    swfDrawStyledLineTo(shape, x0, y0, gc);
    SWFShape_end(shape);
    SWFMovieClip_add(swfInfo->currentFrame, (SWFBlock) shape);
}


void swfCircle(double x, double y, double r, const pGEcontext gc, pDevDesc dd)
{
#ifdef SWF_DEBUG
    Rprintf("circle called\n");
#endif
    pswfDesc swfInfo = (pswfDesc) dd->deviceSpecific;
    SWFShape shape = newSWFShape();
    swfSetLineStyle(shape, gc, swfInfo);
    swfSetFillStyle(shape, gc, swfInfo);
    
    SWFShape_movePenTo(shape, x, y);
    SWFShape_drawCircle(shape, r);
    SWFShape_end(shape);
    SWFMovieClip_add(swfInfo->currentFrame, (SWFBlock) shape);
}



void swfLine(double x1, double y1, double x2, double y2, const pGEcontext gc, pDevDesc dd)
{
#ifdef SWF_DEBUG
    Rprintf("line called\n");
#endif
    pswfDesc swfInfo = (pswfDesc) dd->deviceSpecific;
    SWFShape shape = newSWFShape();
    swfSetLineStyle(shape, gc, swfInfo);
    
    SWFShape_movePenTo(shape, x1, y1);
    swfDrawStyledLineTo(shape, x2, y2, gc);
    SWFShape_end(shape);
    SWFMovieClip_add(swfInfo->currentFrame, (SWFBlock) shape);
}


void swfPolygon(int n, double *x, double *y, const pGEcontext gc, pDevDesc dd)
{
#ifdef SWF_DEBUG
    Rprintf("polygon called\n");
#endif
    pswfDesc swfInfo = (pswfDesc) dd->deviceSpecific;
    SWFShape shape = newSWFShape();
    int i = 0;
    
    /* First fill the polygon with no stroke, then draw polyline additionally */
    swfSetFillStyle(shape, gc, swfInfo);
    
    SWFShape_movePenTo(shape, x[0], y[0]);
    for(i = 1; i < n; i++)
    {
        SWFShape_drawLineTo(shape, x[i], y[i]);
    }
    SWFShape_drawLineTo(shape, x[0], y[0]);
    SWFShape_end(shape);
    SWFMovieClip_add(swfInfo->currentFrame, (SWFBlock) shape);
    swfPolyline(n, x, y, gc, dd);
}


void swfPolyline(int n, double *x, double *y, const pGEcontext gc, pDevDesc dd)
{
#ifdef SWF_DEBUG
    Rprintf("polyline called\n");
    Rprintf("** polyline(n = %d, (x0, y0) = (%f, %f), (x1, y1) = (%f, %f))\n",
        n, x[0], y[0], x[1], y[1]);
#endif
    pswfDesc swfInfo = (pswfDesc) dd->deviceSpecific;
    SWFShape shape = newSWFShape();
    int i = 0;
    
    swfSetLineStyle(shape, gc, swfInfo);
    
    SWFShape_movePenTo(shape, x[0], y[0]);
    for(i = 1; i < n; i++)
    {
        swfDrawStyledLineTo(shape, x[i], y[i], gc);
    }
    SWFShape_end(shape);
    SWFMovieClip_add(swfInfo->currentFrame, (SWFBlock) shape);
}


void swfPath(double *x, double *y, int npoly, int *nper, Rboolean winding, const pGEcontext gc, pDevDesc dd)
{
#ifdef SWF_DEBUG
    Rprintf("path called\n");
#endif
}


void swfMetricInfo(int c, const pGEcontext gc, double* ascent, double* descent, double* width, pDevDesc dd)
{
#ifdef SWF_DEBUG
    Rprintf("metricInfo called\n");
    Rprintf("** family = %s, c = %d\n", gc->fontfamily, c);
#endif
    FT_Face face = swfGetFTFace(gc);
    FT_Error err;
    double fontSize = gc->ps * gc->cex;
    double ratio = fontSize / face->units_per_EM;
  
    if(c == 0) c = 77;
    if(c < 0)
    {
        c = -c;
    }
    
    /* c is the unicode of the character */
    FT_Set_Char_Size(face, 0, (FT_F26Dot6) (fontSize * 64), 72, 0);
    err = FT_Load_Char(face, c, FT_LOAD_NO_SCALE);
    if(err)
    {
        errorcode(err);
        *ascent = *descent = *width = 0.0;
        return;
    }
    
    *ascent = face->glyph->metrics.horiBearingY * ratio;
    *descent = face->glyph->metrics.height * ratio - *ascent;
    *width = face->glyph->metrics.horiAdvance * ratio;
#ifdef SWF_DEBUG
    Rprintf("** metricInfo(ascent = %f, descent = %f, width = %f)\n",
            *ascent, *descent, *width);
#endif
}

void swfMode(int mode, pDevDesc dd)
{
#ifdef SWF_DEBUG
    Rprintf("mode called\n");
#endif
}


void swfSize(double *left, double *right, double *bottom, double *top, pDevDesc dd)
{
#ifdef SWF_DEBUG
    Rprintf("size called\n");
#endif
    *left = dd->left;
    *right = dd->right;
    *bottom = dd->bottom;
    *top = dd->top;
}


double swfStrWidth(const char *str, const pGEcontext gc, pDevDesc dd)
{
#ifdef SWF_DEBUG
    Rprintf("strWidth called\n");
#endif
    return swfStrWidthUTF8(str, gc, dd);
}


void swfText(double x, double y, const char *str, double rot, double hadj, const pGEcontext gc, pDevDesc dd)
{
#ifdef SWF_DEBUG
    Rprintf("text called\n");
#endif
    swfTextUTF8(x, y, str, rot, hadj, gc, dd);
}


double swfStrWidthUTF8(const char *str, const pGEcontext gc, pDevDesc dd)
{
#ifdef SWF_DEBUG
    Rprintf("strWidthUTF8 called\n");
    Rprintf("** family = %s, str[0] = %d, str[1] = %d\n",
        gc->fontfamily, str[0], str[1]);
#endif
    /* Convert UTF-8 string to Unicode array */
    int maxLen = (int) (strlen(str));
    wchar_t *unicode = (wchar_t *) calloc(maxLen + 1, sizeof(wchar_t));
    int len = utf8towcs(unicode, str, maxLen);
    /* Get the font face object */
    FT_Face face = swfGetFTFace(gc);
    FT_Error err;
    double fontSize = gc->ps * gc->cex;
    double ratio = fontSize / face->units_per_EM;
    double width = 0.0;
    int i;
    /* Add up the 'advance' of each character */
    for(i = 0; i < len; i++)
    {
        err = FT_Load_Char(face, unicode[i], FT_LOAD_NO_SCALE);
        if(err)
        {
            errorcode(err);
            continue;
        }
        width += face->glyph->metrics.horiAdvance * ratio;
    }
    
    free(unicode);

#ifdef SWF_DEBUG
    Rprintf("** strWidthUTF8(width = %f)\n", width);
#endif

    return width;
}


void swfTextUTF8(double x, double y, const char *str, double rot, double hadj, const pGEcontext gc, pDevDesc dd)
{
#ifdef SWF_DEBUG
    Rprintf("textUTF8 called\n");
    Rprintf("** family = %s, font = %d\n", gc->fontfamily, gc->fontface);
    Rprintf("** textUTF8(str[0] = %d, str[1] = %d, str[2] = %d, str[3] = %d)\n",
            str[0], str[1], str[2], str[3]);
#endif
    pswfDesc swfInfo = (pswfDesc) dd->deviceSpecific;
    SWFShape text = newSWFShape();
    SWFDisplayItem text_display;
    /* Convert UTF-8 string to Unicode array */
    int maxLen = (int) (strlen(str));
    wchar_t *unicode = (wchar_t *) calloc(maxLen + 1, sizeof(wchar_t));
    int len = utf8towcs(unicode, str, maxLen);
    
    FT_Face face = swfGetFTFace(gc);
    double fontSize = gc->ps * gc->cex;

    double strWidth = 0.0;

    swfSetTextColor(text, gc, swfInfo);
    strWidth = SWFShape_addString(text, unicode, len, fontSize, face,
                                  &(swfInfo->outlnFuns));
    
    text_display = SWFMovieClip_add(swfInfo->currentFrame, (SWFBlock) text);
    
    SWFDisplayItem_rotate(text_display, rot);
    SWFDisplayItem_moveTo(text_display,
                          x - hadj * strWidth * cos(rot * DEG2RAD),
                          y + hadj * strWidth * sin(rot * DEG2RAD));
    
    free(unicode);
}


void swfClose(pDevDesc dd)
{
#ifdef SWF_DEBUG
    Rprintf("close called\n");
#endif
    pswfDesc swfInfo = (pswfDesc) dd->deviceSpecific;
    
    if(swfInfo->currentFrame)
    {
        SWFMovieClip_nextFrame(swfInfo->currentFrame);
        SWFMovie_add(swfInfo->m, (SWFBlock) swfInfo->currentFrame);
        SWFMovie_nextFrame(swfInfo->m);
    }
    SWFMovie_save(swfInfo->m, swfInfo->filename);
    destroySWFFillStyleArray(swfInfo->array);
    free(swfInfo);
}


SEXP swfSetPattern(SEXP pattern, pDevDesc dd)
{
#ifdef SWF_DEBUG
    Rprintf("setPattern called\n");
#endif
    return R_NilValue;
}


void swfReleasePattern(SEXP ref, pDevDesc dd)
{
#ifdef SWF_DEBUG
    Rprintf("releasePattern called\n");
#endif
}


SEXP swfSetClipPath(SEXP path, SEXP ref, pDevDesc dd)
{
#ifdef SWF_DEBUG
    Rprintf("setClipPath called\n");
#endif
    return R_NilValue;
}


void swfReleaseClipPath(SEXP ref, pDevDesc dd)
{
#ifdef SWF_DEBUG
    Rprintf("releaseClipPath called\n");
#endif
}


SEXP swfSetMask(SEXP path, SEXP ref, pDevDesc dd)
{
#ifdef SWF_DEBUG
    Rprintf("setMask called\n");
#endif
    return R_NilValue;
}


void swfReleaseMask(SEXP ref, pDevDesc dd)
{
#ifdef SWF_DEBUG
    Rprintf("releaseMask called\n");
#endif
}
