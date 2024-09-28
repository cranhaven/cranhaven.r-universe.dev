#include <ming.h>

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>


SEXP image2swf(SEXP fileNames, SEXP format, SEXP outName,
               SEXP bgColor, SEXP interval)
{
    /* Create swf movie object */
    SWFMovie m = newSWFMovieWithVersion(8);
    /* Number of files to be converted */
    int nFiles = LENGTH(fileNames);
    int i;
    /* We need to find the first image available to determine
     * the dimension of the movie. This variable serves as
     * a flag in the loop. */
    int dimNotSet = 1;
    /* Height and width of the movie */
    int swfHeight, swfWidth;
    /* Image object in the loop to be added to the movie */
    SWFBitmap image = NULL;
    /* Filename of each file to be converted */
    const char *filename;
    /* Block object to be added to the movie. Images
     * should first be converted to block objects before
     * being added to movie. */
    SWFMovieBlockType ublock;

    /* Set the background color of the movie.
     * bgColor is a vector of length 3, representing RGB values. */
    SWFMovie_setBackground(m,
                           (byte) (INTEGER(bgColor)[0]),
                           (byte) (INTEGER(bgColor)[1]),
                           (byte) (INTEGER(bgColor)[2]));
    /* Set the frame rate of the movie */
    SWFMovie_setRate(m, (float) (1.0 / REAL(interval)[0]));

    /* Main loop.
     * Adding an image to the movie at one time. */
    for(i = 0; i < nFiles; i++)
    {
        filename = CHAR(STRING_ELT(fileNames, i));
        /* Format of the image.
         * 1 for png, 2 for jpeg/jpg, 0 for other */
        if(INTEGER(format)[i] == 1)
        {
            /* Reading the png file to an image object */
            image = (SWFBitmap) newSWFDBLBitmapData_fromPngFile(filename);
        } else if(INTEGER(format)[i] == 2) {
            /* Reading the jpg file to an image object */
            FILE *fp = fopen(filename, "rb");
            image = (fp == NULL) ? NULL: (SWFBitmap) newSWFJpegBitmap(fp);
            fclose(fp);
        } else {
            image = NULL;
        }
        /* For unsupported files, give a warning. */
        if(image == NULL)
        {
            Rf_warning("Unsupported file \"%s\", ignored.\n", filename);
            continue;
        }
        /* If the dimension is not set yet, set it, and change the flag. */
        if(dimNotSet)
        {
            swfHeight = SWFBitmap_getHeight(image);
            swfWidth = SWFBitmap_getWidth(image);
            SWFMovie_setDimension(m, swfWidth, swfHeight);
            dimNotSet = 0;
        }
        /* Convert image object to block object,
         * and add it to the movie. */
        if(INTEGER(format)[i] == 1)
        {
            ublock.dblbmp_data = (SWFDBLBitmapData) image;
            SWFMovie_add_internal(m, ublock);
            SWFMovie_nextFrame(m);
        } else if(INTEGER(format)[i] == 2) {
            ublock.jpegbmp = (SWFJpegBitmap) image;
            SWFMovie_add_internal(m, ublock);
            SWFMovie_nextFrame(m);
        }
    }
    /* Save movie to file */
    SWFMovie_save(m, CHAR(STRING_ELT(outName, 0)));
    /* Free resources */
    /* Ming_collectGarbage(); */

    return R_NilValue;
}

