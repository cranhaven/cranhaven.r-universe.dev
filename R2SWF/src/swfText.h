#ifndef SWFTEXT_H_INCLUDED
#define SWFTEXT_H_INCLUDED

#include <ming.h>

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_OUTLINE_H

/* These are data passed to outline drawing handlers */
typedef struct
{
    SWFShape shape;
    double ratio_EM;
    double deltax;
} OutlineData;

/* Outline drawing functions used by FT_Outline_Decompose(),
   modified from libming/blocks/ttffont.c */
int outlineMoveTo(const FT_Vector* to, void* user);
int outlineLineTo(const FT_Vector* to, void* user);
int outlineConicTo(const FT_Vector* control, const FT_Vector* to, void* user);
int outlineCubicTo(const FT_Vector* control1, const FT_Vector* control2,
            const FT_Vector* to, void* user);

void errorcode(FT_Error err);

/* Draw a string on a SWFShape object. String will be placed at (0, 0),
   which is at the bottom left corner of the first character */
double SWFShape_addString(SWFShape shape, const wchar_t* str, size_t nchar,
                          double fontSize,
                          FT_Face face, FT_Outline_Funcs *funs);



#endif /* SWFTEXT_H_INCLUDED */

