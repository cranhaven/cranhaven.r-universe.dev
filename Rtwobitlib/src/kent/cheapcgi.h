/* cheapcgi.h - turns variables passed from the web form into
 * something that C understands. 
 * 
 * This file is copyright 2000 Jim Kent, but license is hereby
 * granted for all use - public, private or commercial. */

#ifndef CHEAPCGI_H
#define CHEAPCGI_H

#ifndef HASH_H
#include "hash.h"
#endif


#define COLOR_BG_DEFAULT         "#FFFEE8"
#define COLOR_BG_ALTDEFAULT      "#FFF9D2"
#define COLOR_BG_DEFAULT_DARKER  "#FCECC0"
#define COLOR_BG_DEFAULT_DARKEST "#EED5B7"
#define COLOR_BG_GHOST           "#EEEEEE"
#define COLOR_BG_PALE            "#F8F8F8"
#define COLOR_BG_HEADER_LTBLUE   "#D9E4F8"
#define COLOR_DARKGREEN          "#008800"
#define COLOR_LTGREEN            "#CCFFCC"
#define COLOR_DARKBLUE           "#000088"
#define COLOR_BLUE_BUTTON        "#91B3E6"
#define COLOR_DARKGREY           "#666666"
#define COLOR_LTGREY             "#CCCCCC"
#define COLOR_YELLOW             "#FFFF00"
#define COLOR_LTYELLOW           "#FFF380"
#define COLOR_WHITE              "#FFFFFF"
#define COLOR_RED                "#AA0000"
#define COLOR_TRACKLIST_LEVEL1   COLOR_BG_DEFAULT
#define COLOR_TRACKLIST_LEVEL2   COLOR_BG_ALTDEFAULT
#define COLOR_TRACKLIST_LEVEL3   COLOR_BG_DEFAULT_DARKER
#define COLOR_TRACKLIST_LEVEL4   COLOR_BG_DEFAULT_DARKEST

enum browserType
/* How to look at a track. */
    {
    btUnknown=0, // Not yet known
    btOpera=1,   // Opera
    btIE=2,      // MS Internet Explorer
    btFF=3,      // Firefox
    btChrome=4,  // Google Chrome
    btSafari=5,  // Safari
    btOther=6    // Anything else
    };

enum osType
/* How to look at a track. */
    {
    osUnknown=0, // Not yet known
    osWindows=1, // The evil empire
    osLinux=2,   // Workhorse
    osMac=3,     // ashion or Religion
    osOther=4    // Anything else
    };

struct cgiChoice
/* Choice table */
    {
    char *name;
    int value;
    };

void cgiDecode(const char *in, char *out, int inLength);
/* Decode from cgi pluses-for-spaces format to normal.
 * Out will be a little shorter than in typically. */

#define NO_VALUE            -96669

#endif /* CHEAPCGI_H */
