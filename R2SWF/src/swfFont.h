#ifndef SWFFONT_H_INCLUDED
#define SWFFONT_H_INCLUDED

#include <ming.h>

#include <ft2build.h>
#include FT_FREETYPE_H

/* Freetype 2.6 introduces a macro TYPEOF that conflicts with R */
#ifdef TYPEOF
#undef TYPEOF
#endif

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <R_ext/GraphicsEngine.h>
#include <R_ext/GraphicsDevice.h>

typedef struct fontDesc {
    FT_Library library;
    FT_Face face;
} FontDesc;

typedef FontDesc* pFontDesc;

FT_Face swfGetFTFace(const pGEcontext gc);

#endif /* SWFFONT_H_INCLUDED */

