/*  utf8proc feature probes.  Each HAVE_UTF8PROC_* is defined when
    the corresponding field, enum constant, or function was present
    in the libutf8proc headers at configure time.  Older distro
    packages (Ubuntu LTS, ...) ship utf8proc versions predating
    charwidth/ambiguous_width/indic_conjunct_break/EXTENDED_PICTOGRAPHIC;
    unicode4pl.c gates the corresponding unicode_property/2 branches
    on these macros.
*/

#cmakedefine HAVE_UTF8PROC_CHARWIDTH		  1
#cmakedefine HAVE_UTF8PROC_AMBIGUOUS_WIDTH	  1
#cmakedefine HAVE_UTF8PROC_BIDI_MIRRORED	  1
#cmakedefine HAVE_UTF8PROC_INDIC_CONJUNCT_BREAK	  1
#cmakedefine HAVE_UTF8PROC_BOUNDCLASS_EXTENDED_PICTOGRAPHIC 1
#cmakedefine HAVE_UTF8PROC_BOUNDCLASS_E_BASE	  1
#cmakedefine HAVE_UTF8PROC_UNICODE_VERSION	  1
#cmakedefine HAVE_UTF8PROC_CODEPOINT_VALID	  1
#cmakedefine HAVE_UTF8PROC_GRAPHEME_BREAK_STATEFUL 1
#cmakedefine HAVE_UTF8PROC_TOLOWER		  1
#cmakedefine HAVE_UTF8PROC_TOUPPER		  1
#cmakedefine HAVE_UTF8PROC_TOTITLE		  1
