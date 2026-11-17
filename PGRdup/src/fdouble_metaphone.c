// ### This file is part of 'PGRdup' package for R.

// ### Copyright (C) 2014-2020, ICAR-NBPGR.
// #
// # PGRdup is free software: you can redistribute it and/or modify it
// # under the terms of the GNU General Public License as published by
// # the Free Software Foundation, either version 2 of the License, or
// # (at your option) any later version.
// #
// # PGRdup is distributed in the hope that it will be useful, but
// # WITHOUT ANY WARRANTY; without even the implied warranty of
// # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// # GNU General Public License for more details.
// #
// #  A copy of the GNU General Public License is available at
// #  http://www.r-project.org/Licenses/


/*
double_metaphone.* are based on the perl Metaphone module developed
by Maurice Aubrey
Here is the original license info:

--------------------------------------------------------------------------------
DESCRIPTION

This module implements a "sounds like" algorithm developed
by Lawrence Philips which he published in the June, 2000 issue
of C/C++ Users Journal. Double Metaphone is an improved
version of Philips' original Metaphone algorithm.

COPYRIGHT

Copyright 2000, Maurice Aubrey <maurice@hevanet.com>.
All rights reserved.
This code is based heavily on the C++ implementation by
Lawrence Philips and incorporates several bug fixes courtesy
of Kevin Atkinson <kevina@users.sourceforge.net>.
This module is free software; you may redistribute it and/or
modify it under the same terms as Perl itself.
--------------------------------------------------------------------------------
*/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>

#include <R.h>
#include <Rmath.h>

#include "double_metaphone.h"


#ifndef ALLOCATIONS
  #define ALLOCATIONS
	#define META_MALLOC(v,n,t) \
			  (v = (t*)malloc(((n)*sizeof(t))))
	#define META_REALLOC(v,n,t) \
						  (v = (t*)realloc((v),((n)*sizeof(t))))
	#define META_FREE(x) free((x))
#endif


void fdouble_metaphone(char** p_input, char** p1_output, char** p2_output, int* tot_length)
{
	//char** pointeroutput = p1_output;
	//char** pointerinput = p_input;
	int i=0;
	for (i=0;i<(*tot_length);i++){
		DoubleMetaphone(*p_input++,p1_output++,p2_output++);
	}
}




void DestroyMetaString(metastring * s)
{
    if (s == NULL)
	return;

    if (s->free_string_on_destroy && (s->str != NULL))
	META_FREE(s->str);

    META_FREE(s);
}


void IncreaseBuffer(metastring * s, int chars_needed)
{
    META_REALLOC(s->str, (s->bufsize + chars_needed + 10), char);
    assert( s->str != NULL );
    s->bufsize = s->bufsize + chars_needed + 10;
}


void MakeUpper(metastring * s)
{
    char *i;

    for (i = s->str; *i; i++)
      {
	  *i = toupper(*i);
      }
}


void IsVowel(metastring * s, int pos, int* answer)
{
    char c;
	
	*answer = 0;
    if ((pos < 0) || (pos >= s->length))
		return;
		//*answer = 0;

    c = *(s->str + pos);
    if ((c == 'A') || (c == 'E') || (c == 'I') || (c =='O') || 
        (c =='U')  || (c == 'Y'))
	*answer = 1;
}


void SlavoGermanic(metastring * s, int* answer)
{
    if ((char *) strstr(s->str, "W"))
	*answer = 1;
    else if ((char *) strstr(s->str, "K"))
	*answer = 1;
    else if ((char *) strstr(s->str, "CZ"))
	*answer = 1;
    else if ((char *) strstr(s->str, "WITZ"))
	*answer = 1;
    else
	*answer = 0;
}


/*int GetLength(metastring * s)
{
    return s->length;
}*/


void GetAt(char* getata, metastring * s, int pos)
{
    if ((pos < 0) || (pos >= s->length))
	return;

    *getata = ((char) *(s->str + pos));
}


void SetAt(metastring * s, int pos, char c)
{
    if ((pos < 0) || (pos >= s->length))
	return;

    *(s->str + pos) = c;
}


/* 
   Caveats: the START value is 0 based
*/
void StringAt(int* stringata, metastring * s, int start, int length, ...)
{
    char *test;
    char *pos;
    va_list ap;
	*stringata =  0;//-------------------------------------------------->
    if ((start < 0) || (start >= s->length))
		return;
        //*stringata =  0;

    pos = (s->str + start);
    va_start(ap, length);

    do
      {
	  test = va_arg(ap, char *);
	  if (*test && (strncmp(pos, test, length) == 0))
	      *stringata =  1;
      }
    while (strcmp(test, ""));

    va_end(ap);

    //*stringata =  0;//------------------------------>commented
}


void MetaphAdd(metastring * s, char *new_str)
{
    int add_length;

    if (new_str == NULL)
	return;

    add_length = strlen(new_str);
    if (((s->length + add_length) - (s->bufsize - 1))>0)
      {
	  IncreaseBuffer(s, add_length);
      }

    strcat(s->str, new_str);
    s->length += add_length;
}


void DoubleMetaphone(char *str, char **codes,char **codes1)
{
    int        length;
    metastring *original;
    metastring *primary;
    metastring *secondary;
    int        current;
    int        last;
		int answer;
		int answer2;
		char getata = 0;
		char getata1 = 0;
		int stringata;
		int stringata1;
		int stringata2;
		int stringata3;
		int stringata4;
		int stringata5;
		
		
		/**************************************************/
		char empty_string[] = "";
		char test_string[]="";
		/*************************************************/
		
			
		
    current = 0;
    /* we need the real length and last prior to padding */
    length  = strlen(str); 
    last    = length - 1; 
    
    
    
    
    
    
    
    
    
    
    //original = NewMetaString(str);
    META_MALLOC(original, 1, metastring);
    assert( original != NULL );

    if (str == NULL)
			str = empty_string;
    original->length  = strlen(str);
    //original->size  = sizeof(str);
    /* preallocate a bit more for potential growth */
    original->bufsize = original->length + 7;

    META_MALLOC(original->str, original->bufsize, char);
    assert( original->str != NULL );
    
	  strcpy(original->str, str);
	//strlcpy(original->str, str, sizeof(original->bufsize))
	//[Org]strncpy(original->str, str, original->length + 1);
	//[Test]strncpy(original->str, str, sizeof(original->str) - 1);
  //memcpy(original->str, str, sizeof(original->str));
    original->free_string_on_destroy = 1; 
    

    /* Pad original so we can index beyond end */
    MetaphAdd(original, "     ");

    //primary = NewMetaString("");
    //strcpy(str,empty_string);
    META_MALLOC(primary, 1, metastring);
    assert( primary != NULL );

    //if (test_string == ((void *)0))
	//		strcpy(test_string,"");
    primary->length  = strlen(test_string);
    /* preallocate a bit more for potential growth */
    primary->bufsize = primary->length + 7;

    META_MALLOC(primary->str, primary->bufsize, char);
    assert( primary->str != NULL );
    
	strcpy(primary->str,"");
	//[qrt]strncpy(primary->str, "", primary->length + 1);
    //strncpy(primary->str, test_string, primary->length + 1);
    primary->free_string_on_destroy = 1; 
    
    
    
    //secondary = NewMetaString("");
    //strcpy(str,empty_string);
    META_MALLOC(secondary, 1, metastring);
    assert( secondary != NULL );

    //if (test_string == ((void *)0))
	//		strcpy(test_string,"");
    secondary->length  = strlen(test_string);
    /* preallocate a bit more for potential growth */
    secondary->bufsize = secondary->length + 7;

    META_MALLOC(secondary->str, secondary->bufsize, char);
    assert( secondary->str != NULL );
    
	strcpy(secondary->str,"");
	//[qrt]strncpy(secondary->str, "", secondary->length + 1);
    //strncpy(secondary->str, test_string, secondary->length + 1);
    secondary->free_string_on_destroy = 1; 
    
    
    
    

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    //primary->free_string_on_destroy = 0;
    //secondary->free_string_on_destroy = 0;

    MakeUpper(original);  

    /* skip these when at start of word */
    StringAt(&stringata,original, 0, 2, "GN", "KN", "PN", "WR", "PS", "");
    if (stringata)
	current += 1;

    /* Initial 'X' is pronounced 'Z' e.g. 'Xavier' */
    GetAt(&getata,original, 0);
    if (getata == 'X')
      {
	  MetaphAdd(primary, "S");	/* 'Z' maps to 'S' */
	  MetaphAdd(secondary, "S");
	  current += 1;
      }

    /* main loop */
    while ((primary->length < 4) || (secondary->length < 4))  
      {
	  if (current >= length)
	      break;
		GetAt(&getata,original, current);
	  switch (getata)
	    {
	    case 'A':
	    case 'E':
	    case 'I':
	    case 'O':
	    case 'U':
	    case 'Y':
		if (current == 0)
                  {
		    /* all init vowels now map to 'A' */
		    MetaphAdd(primary, "A");
		    MetaphAdd(secondary, "A");
                  }
		current += 1;
		break;

	    case 'B':

		/* "-mb", e.g", "dumb", already skipped over... */
		MetaphAdd(primary, "P");
		MetaphAdd(secondary, "P");
		GetAt(&getata,original, current + 1);
		if (getata == 'B')
		    current += 2;
		else
		    current += 1;
		break;

/*	    case 'Ç':
		MetaphAdd(primary, "S");
		MetaphAdd(secondary, "S");
		current += 1;
		break;
*/
	    case 'C':
		/* various germanic */		
		StringAt(&stringata,original, (current - 1), 3, "ACH", "");
		StringAt(&stringata1,original, (current - 2), 6, "BACHER",
					"MACHER", "");
		IsVowel(original, current - 2, &answer);
		GetAt(&getata,original, current + 2);
		GetAt(&getata1,original, current + 2);
		if ((current > 1)
		    && !answer
		    && stringata
		    && ((getata != 'I')
			&& ((getata1 != 'E')
			    || stringata1)))
		  {
		      MetaphAdd(primary, "K");
		      MetaphAdd(secondary, "K");
		      current += 2;
		      break;
		  }

		/* special case 'caesar' */
		StringAt(&stringata,original, current, 6, "CAESAR", "");
		if ((current == 0)
		    && stringata)
		  {
		      MetaphAdd(primary, "S");
		      MetaphAdd(secondary, "S");
		      current += 2;
		      break;
		  }

		/* italian 'chianti' */
		StringAt(&stringata,original, current, 4, "CHIA", "");
		if (stringata)
		  {
		      MetaphAdd(primary, "K");
		      MetaphAdd(secondary, "K");
		      current += 2;
		      break;
		  }
		StringAt(&stringata,original, current, 2, "CH", "");
		if (stringata)
		  {
		      /* find 'michael' */
		      StringAt(&stringata,original, current, 4, "CHAE", "");
		      if ((current > 0)
			  && stringata)
			{
			    MetaphAdd(primary, "K");
			    MetaphAdd(secondary, "X");
			    current += 2;
			    break;
			}

		      /* greek roots e.g. 'chemistry', 'chorus' */
		      StringAt(&stringata,original, (current + 1), 5, "HARAC", "HARIS", "");
			   	StringAt(&stringata1,original, (current + 1), 3, "HOR",
				       "HYM", "HIA", "HEM", "");
			  	StringAt(&stringata2,original, 0, 5, "CHORE", "");
		      if ((current == 0)
			  && (stringata
			   || stringata1)
			  && !stringata2)
			{
			    MetaphAdd(primary, "K");
			    MetaphAdd(secondary, "K");
			    current += 2;
			    break;
			}

		      /* germanic, greek, or otherwise 'ch' for 'kh' sound */
		      StringAt(&stringata,original, 0, 4, "VAN ", "VON ", "");
			   	StringAt(&stringata1,original, 0, 3, "SCH", "");
			   	StringAt(&stringata2,original, (current - 2), 6, "ORCHES",
				      "ARCHIT", "ORCHID", "");
			  	StringAt(&stringata3,original, (current + 2), 1, "T", "S",
				      "");
			  	StringAt(&stringata4,original, (current - 1), 1, "A", "O", "U", "E", "");
			   	StringAt(&stringata5,original, (current + 2), 1, "L", "R",
		                      "N", "M", "B", "H", "F", "V", "W", " ", "");
		      
		      
		      
		      if (
			  (stringata
			   || stringata1)
			  /*  'architect but not 'arch', 'orchestra', 'orchid' */
			  || stringata2
			  || stringata3
			  || ((stringata4 || (current == 0))
			   /* e.g., 'wachtler', 'wechsler', but not 'tichner' */
			  && stringata5))
			{
			    MetaphAdd(primary, "K");
			    MetaphAdd(secondary, "K");
			}
		      else
			{
			    if (current > 0)
			      {
			      StringAt(&stringata,original, 0, 2, "MC", "");
				  if (stringata)
				    {
					/* e.g., "McHugh" */
					MetaphAdd(primary, "K");
					MetaphAdd(secondary, "K");
				    }
				  else
				    {
					MetaphAdd(primary, "X");
					MetaphAdd(secondary, "K");
				    }
			      }
			    else
			      {
				  MetaphAdd(primary, "X");
				  MetaphAdd(secondary, "X");
			      }
			}
		      current += 2;
		      break;
		  }
		/* e.g, 'czerny' */
		StringAt(&stringata,original, current, 2, "CZ", "");
		StringAt(&stringata1,original, (current - 2), 4, "WICZ", "");
		if (stringata
		    && !stringata1)
		  {
		      MetaphAdd(primary, "S");
		      MetaphAdd(secondary, "X");
		      current += 2;
		      break;
		  }

		/* e.g., 'focaccia' */
		StringAt(&stringata,original, (current + 1), 3, "CIA", "");
		if (stringata)
		  {
		      MetaphAdd(primary, "X");
		      MetaphAdd(secondary, "X");
		      current += 3;
		      break;
		  }

		/* double 'C', but not if e.g. 'McClellan' */
		StringAt(&stringata,original, current, 2, "CC", "");
		GetAt(&getata,original, 0);
		if (stringata
		    && !((current == 1) && (getata == 'M')))
		{
		    /* 'bellocchio' but not 'bacchus' */
		    StringAt(&stringata,original, (current + 2), 1, "I", "E", "H", "");
				StringAt(&stringata1,original, (current + 2), 2, "HU", "");
		    if (stringata
			&& !stringata1)
		      {
			  /* 'accident', 'accede' 'succeed' */
			  StringAt(&stringata,original, (current - 1), 5, "UCCEE",
					  "UCCES", "");
					  GetAt(&getata,original, current - 1);
			  if (
			      ((current == 1)
			       && (getata == 'A'))
			      || stringata)
			    {
				MetaphAdd(primary, "KS");
				MetaphAdd(secondary, "KS");
				/* 'bacci', 'bertucci', other italian */
			    }
			  else
			    {
				MetaphAdd(primary, "X");
				MetaphAdd(secondary, "X");
			    }
			  current += 3;
			  break;
		      }
		}
		//-------------------------------------------------> block comment by anand
		/*else
		  {	  // Pierce's rule //
		  MetaphAdd(primary, "K");
		  MetaphAdd(secondary, "K");
		  current += 2;
		  break;
		  }
		*/
		StringAt(&stringata,original, current, 2, "CK", "CG", "CQ", "");
		if (stringata)
		  {
		      MetaphAdd(primary, "K");
		      MetaphAdd(secondary, "K");
		      current += 2;
		      break;
		  }
		StringAt(&stringata,original, current, 2, "CI", "CE", "CY", "");
		if (stringata)
		  {
		      /* italian vs. english */
		      StringAt(&stringata,original, current, 3, "CIO", "CIE", "CIA", "");
		      if (stringata)
			{
			    MetaphAdd(primary, "S");
			    MetaphAdd(secondary, "X");
			}
		      else
			{
			    MetaphAdd(primary, "S");
			    MetaphAdd(secondary, "S");
			}
		      current += 2;
		      break;
		  }

		/* else */
		MetaphAdd(primary, "K");
		MetaphAdd(secondary, "K");

		/* name sent in 'mac caffrey', 'mac gregor */
		StringAt(&stringata,original, (current + 1), 2, " C", " Q", " G", "");
		StringAt(&stringata1,original, (current + 1), 1, "C", "K", "Q", "");
		StringAt(&stringata2,original, (current + 1), 2, "CE", "CI", "");
		if (stringata)
		    current += 3;
		else
		    if (stringata1
			&& !stringata2)
		    current += 2;
		else
		    current += 1;
		break;

	    case 'D':
	    	StringAt(&stringata,original, current, 2, "DG", "");
	    	StringAt(&stringata1,original, (current + 2), 1, "I", "E", "Y", "");
		if (stringata)
                  {
		      if (stringata1)
		        {
			    /* e.g. 'edge' */
			    MetaphAdd(primary, "J");
			    MetaphAdd(secondary, "J");
			    current += 3;
			    break;
		        }
		      else
		        {
			    /* e.g. 'edgar' */
			    MetaphAdd(primary, "TK");
			    MetaphAdd(secondary, "TK");
			    current += 2;
			    break;
		        }
                  }
		StringAt(&stringata,original, current, 2, "DT", "DD", "");
		if (stringata)
		  {
		      MetaphAdd(primary, "T");
		      MetaphAdd(secondary, "T");
		      current += 2;
		      break;
		  }

		/* else */
		MetaphAdd(primary, "T");
		MetaphAdd(secondary, "T");
		current += 1;
		break;

	    case 'F':
	    	GetAt(&getata,original, current + 1);
		if (getata == 'F')
		    current += 2;
		else
		    current += 1;
		MetaphAdd(primary, "F");
		MetaphAdd(secondary, "F");
		break;

	    case 'G':
	    IsVowel(original, current - 1, &answer);
	    GetAt(&getata,original, current + 1);
		if (getata == 'H')
		  {
		      if ((current > 0) && !answer)
			{
			    MetaphAdd(primary, "K");
			    MetaphAdd(secondary, "K");
			    current += 2;
			    break;
			}

		      if (current < 3)
			{
			    /* 'ghislane', ghiradelli */
			    if (current == 0)
			      {
			      	GetAt(&getata,original, current + 2);
				  if (getata == 'I')
				    {
					MetaphAdd(primary, "J");
					MetaphAdd(secondary, "J");
				    }
				  else
				    {
					MetaphAdd(primary, "K");
					MetaphAdd(secondary, "K");
				    }
				  current += 2;
				  break;
			      }
			}
		      /* Parker's rule (with some further refinements) - e.g., 'hugh' */
		      
		      StringAt(&stringata,original, (current - 2), 1, "B", "H", "D", "");
			    StringAt(&stringata1,original, (current - 3), 1, "B", "H", "D", "");
			    StringAt(&stringata2,original, (current - 4), 1, "B", "H", "");
		      if (
			  ((current > 1)
			   && stringata)
			  /* e.g., 'bough' */
			  || ((current > 2)
			      && stringata1)
			  /* e.g., 'broughton' */
			  || ((current > 3)
			      && stringata2))
			{
			    current += 2;
			    break;
			}
		      else
			{
			    /* e.g., 'laugh', 'McLaughlin', 'cough', 'gough', 'rough', 'tough' */
			    StringAt(&stringata,original, (current - 3), 1, "C",
					    "G", "L", "R", "T", "");
					GetAt(&getata,original, current - 1);
			    if ((current > 2)
				&& (getata == 'U')
				&& stringata)
			      {
				  MetaphAdd(primary, "F");
				  MetaphAdd(secondary, "F");
			      }
			    else if ((current > 0)
				     && getata != 'I')
			      {


				  MetaphAdd(primary, "K");
				  MetaphAdd(secondary, "K");
			      }

			    current += 2;
			    break;
			}
		  }
			IsVowel(original, 0, &answer);
			SlavoGermanic(original,&answer2);
			GetAt(&getata,original, current + 1);
		if (getata == 'N')
		  {
		      if ((current == 1) && answer
			  && !answer2)
			{
			    MetaphAdd(primary, "KN");
			    MetaphAdd(secondary, "N");
				current += 2;
				break;
			}
		      else{
			  /* not e.g. 'cagney' */
				SlavoGermanic(original,&answer2);
				GetAt(&getata,original, current + 1);
				StringAt(&stringata,original, (current + 2), 2, "EY", "");
				if (!stringata
				  && (getata != 'Y')
				  && !answer2)
				{
					MetaphAdd(primary, "N");
					MetaphAdd(secondary, "KN");
				}
				current += 2;
				break;
			  }
		      //else
				{
					MetaphAdd(primary, "KN");
					MetaphAdd(secondary, "KN");
				}
		      current += 2;
		      break;
		  }

		/* 'tagliaro' */
		SlavoGermanic(original,&answer2);
		StringAt(&stringata,original, (current + 1), 2, "LI", "");
		if (stringata
		    && !answer2)
		  {
		      MetaphAdd(primary, "KL");
		      MetaphAdd(secondary, "L");
		      current += 2;
		      break;
		  }

		/* -ges-,-gep-,-gel-, -gie- at beginning */
		StringAt(&stringata,original, (current + 1), 2, "ES", "EP",
				    "EB", "EL", "EY", "IB", "IL", "IN", "IE",
				    "EI", "ER", "");
				    GetAt(&getata,original, current + 1);
		if ((current == 0)
		    && ((getata == 'Y')
			|| stringata))
		  {
		      MetaphAdd(primary, "K");
		      MetaphAdd(secondary, "J");
		      current += 2;
		      break;
		  }

		/*  -ger-,  -gy- */
		StringAt(&stringata,original, (current + 1), 2, "ER", "");
		StringAt(&stringata1,original, 0, 6, "DANGER", "RANGER", "MANGER", "");
		StringAt(&stringata2,original, (current - 1), 1, "E", "I", "");
		StringAt(&stringata3,original, (current - 1), 3, "RGY", "OGY",
				 "");
				 GetAt(&getata,original, current + 1);
		if (
		    (stringata
		     || (getata == 'Y'))
		    && !stringata1
		    && !stringata2
		    && !stringata3)
		  {
		      MetaphAdd(primary, "K");
		      MetaphAdd(secondary, "J");
		      current += 2;
		      break;
		  }

		/*  italian e.g, 'biaggi' */
		StringAt(&stringata,original, (current + 1), 1, "E", "I", "Y", "");
		StringAt(&stringata1,original, (current - 1), 4, "AGGI", "OGGI", "");
		if (stringata
		    || stringata1)
		  {
		      /* obvious germanic */
		      StringAt(&stringata,original, 0, 4, "VAN ", "VON ", "");
			   	StringAt(&stringata1,original, 0, 3, "SCH", "");
			  	StringAt(&stringata2,original, (current + 1), 2, "ET", "");
		      if (
			  (stringata
			   || stringata1)
			  || stringata2)
			{
			    MetaphAdd(primary, "K");
			    MetaphAdd(secondary, "K");
			}
		      else
			{
			    /* always soft if french ending */
			    StringAt(&stringata,original, (current + 1), 4, "IER ", "");
			    if (stringata)
			      {
				  MetaphAdd(primary, "J");
				  MetaphAdd(secondary, "J");
			      }
			    else
			      {
				  MetaphAdd(primary, "J");
				  MetaphAdd(secondary, "K");
			      }
			}
		      current += 2;
		      break;
		  }
		GetAt(&getata,original, current + 1);
		if (getata == 'G')
		    current += 2;
		else
		    current += 1;
		MetaphAdd(primary, "K");
		MetaphAdd(secondary, "K");
		break;

	    case 'H':
		/* only keep if first & before vowel or btw. 2 vowels */
		IsVowel(original, current - 1, &answer);
		IsVowel(original, current + 1, &answer2);
		if (((current == 0) || answer)
		    && answer2)
		  {
		      MetaphAdd(primary, "H");
		      MetaphAdd(secondary, "H");
		      current += 2;
		  }
		else		/* also takes care of 'HH' */
		    current += 1;
		break;

	    case 'J':
		/* obvious spanish, 'jose', 'san jacinto' */
		StringAt(&stringata,original, current, 4, "JOSE", "");
		StringAt(&stringata1,original, 0, 4, "SAN ", "");
		if (stringata
		    || stringata1)
		  {
		  		StringAt(&stringata,original, 0, 4, "SAN ", "");
		  		GetAt(&getata,original, current + 4);
		      if (((current == 0)
			   && (getata == ' '))
			  || stringata)
			{
			    MetaphAdd(primary, "H");
			    MetaphAdd(secondary, "H");
			}
		      else
			{
			    MetaphAdd(primary, "J");
			    MetaphAdd(secondary, "H");
			}
		      current += 1;
		      break;
		  }
		StringAt(&stringata,original, current, 4, "JOSE", "");
		if ((current == 0)
		    && !stringata)
		  {
		      MetaphAdd(primary, "J");	/* Yankelovich/Jankelowicz */
		      MetaphAdd(secondary, "A");
		  }
		else
		  {
		      /* spanish pron. of e.g. 'bajador' */
		      IsVowel(original, current - 1, &answer);
		      SlavoGermanic(original,&answer2);
		      GetAt(&getata,original, current + 1);
			    GetAt(&getata1,original, current + 1);
		      if (answer
			  && !answer2
			  && ((getata == 'A')
			      || (getata1 == 'O')))
			{
			    MetaphAdd(primary, "J");
			    MetaphAdd(secondary, "H");
			}
		      else
			{
			    if (current == last)
			      {
				  MetaphAdd(primary, "J");
				  MetaphAdd(secondary, "");
			      }
			    else
				{
				StringAt(&stringata,original, (current + 1), 1, "L", "T",
						"K", "S", "N", "M", "B", "Z", "");
				StringAt(&stringata1,original, (current - 1), 1,
				   "S", "K", "L", "");
				if ((!stringata) && (!stringata1))
				{
					MetaphAdd(primary, "J");
					MetaphAdd(secondary, "J");
				}
			      }
			}
		  }
		GetAt(&getata,original, current + 1);
		if (getata == 'J')	/* it could happen! */
		    current += 2;
		else
		    current += 1;
		break;

	    case 'K':
	    	GetAt(&getata,original, current + 1);
		if (getata == 'K')
		    current += 2;
		else
		    current += 1;
		MetaphAdd(primary, "K");
		MetaphAdd(secondary, "K");
		break;

	    case 'L':
	    	StringAt(&stringata,original, (current - 1), 4, "ILLO",
				       "ILLA", "ALLE", "");
			  StringAt(&stringata1,original, (last - 1), 2, "AS", "OS", "");
			  StringAt(&stringata2,original, last, 1, "A", "O", "");
			  StringAt(&stringata3,original, (current - 1), 4, "ALLE", "");
			  GetAt(&getata,original, current + 1);
		if (getata == 'L')
		  {
		      /* spanish e.g. 'cabrillo', 'gallegos' */
		      if (((current == (length - 3))
			   && stringata)
			  || ((stringata1
			    || stringata2)
			   && stringata3))
			{
			    MetaphAdd(primary, "L");
			    MetaphAdd(secondary, "");
			    current += 2;
			    break;
			}
		      current += 2;
		  }
		else
		    current += 1;
		MetaphAdd(primary, "L");
		MetaphAdd(secondary, "L");
		break;

	    case 'M':
	    	StringAt(&stringata,original, (current - 1), 3, "UMB", "");
		    StringAt(&stringata1,original, (current + 2), 2, "ER", "");
		    GetAt(&getata,original, current + 1);
		if ((stringata
		     && (((current + 1) == last)
			 || stringata1))
		    /* 'dumb','thumb' */
		    || (getata == 'M'))
		    current += 2;
		else
		    current += 1;
		MetaphAdd(primary, "M");
		MetaphAdd(secondary, "M");
		break;

	    case 'N':
	    	GetAt(&getata,original, current + 1);
		if (getata == 'N')
		    current += 2;
		else
		    current += 1;
		MetaphAdd(primary, "N");
		MetaphAdd(secondary, "N");
		break;
/*
	    case 'Ñ':
		current += 1;
		MetaphAdd(primary, "N");
		MetaphAdd(secondary, "N");
		break;
*/
	    case 'P':
	    	GetAt(&getata,original, current + 1);
		if (getata == 'H')
		  {
		      MetaphAdd(primary, "F");
		      MetaphAdd(secondary, "F");
		      current += 2;
		      break;
		  }

		/* also account for "campbell", "raspberry" */
		StringAt(&stringata,original, (current + 1), 1, "P", "B", "");
		if (stringata)
		    current += 2;
		else
		    current += 1;
		MetaphAdd(primary, "P");
		MetaphAdd(secondary, "P");
		break;

	    case 'Q':
	    	GetAt(&getata,original, current + 1);
		if (getata == 'Q')
		    current += 2;
		else
		    current += 1;
		MetaphAdd(primary, "K");
		MetaphAdd(secondary, "K");
		break;

	    case 'R':
		/* french e.g. 'rogier', but exclude 'hochmeier' */
		SlavoGermanic(original,&answer2);
		StringAt(&stringata,original, (current - 2), 2, "IE", "");
		StringAt(&stringata1,original, (current - 4), 2, "ME", "MA", "");
		if ((current == last)
		    && !answer2
		    && stringata
		    && !stringata1)
		  {
		      MetaphAdd(primary, "");
		      MetaphAdd(secondary, "R");
		  }
		else
		  {
		      MetaphAdd(primary, "R");
		      MetaphAdd(secondary, "R");
		  }
		GetAt(&getata,original, current + 1);
		if (getata == 'R')
		    current += 2;
		else
		    current += 1;
		break;

	    case 'S':
		/* special cases 'island', 'isle', 'carlisle', 'carlysle' */
		StringAt(&stringata,original, (current - 1), 3, "ISL", "YSL", "");
		if (stringata)
		  {
		      current += 1;
		      break;
		  }

		/* special case 'sugar-' */
		StringAt(&stringata,original, current, 5, "SUGAR", "");
		if ((current == 0)
		    && stringata)
		  {
		      MetaphAdd(primary, "X");
		      MetaphAdd(secondary, "S");
		      current += 1;
		      break;
		  }
		StringAt(&stringata,original, current, 2, "SH", "");
		if (stringata)
		  {
		      /* germanic */
		      StringAt(&stringata,original, (current + 1), 4, "HEIM", "HOEK", "HOLM",
			   "HOLZ", "");
		      if (stringata)
			{
			    MetaphAdd(primary, "S");
			    MetaphAdd(secondary, "S");
			}
		      else
			{
			    MetaphAdd(primary, "X");
			    MetaphAdd(secondary, "X");
			}
		      current += 2;
		      break;
		  }

		/* italian & armenian */
		StringAt(&stringata,original, current, 3, "SIO", "SIA", "");
		StringAt(&stringata1,original, current, 4, "SIAN", "");
		if (stringata
		    || stringata1)
		  {
		  		SlavoGermanic(original,&answer2);
		      if (!answer2)
			{
			    MetaphAdd(primary, "S");
			    MetaphAdd(secondary, "X");
			}
		      else
			{
			    MetaphAdd(primary, "S");
			    MetaphAdd(secondary, "S");
			}
		      current += 3;
		      break;
		  }

		/* german & anglicisations, e.g. 'smith' match 'schmidt', 'snider' match 'schneider' 
		   also, -sz- in slavic language altho in hungarian it is pronounced 's' */
		   StringAt(&stringata,original, (current + 1), 1, "M", "N", "L", "W", "");
		   StringAt(&stringata1,original, (current + 1), 1, "Z", "");
		if (((current == 0)
		     && stringata)
		    || stringata1)
		  {
		      MetaphAdd(primary, "S");
		      MetaphAdd(secondary, "X");
		      StringAt(&stringata,original, (current + 1), 1, "Z", "");
		      if (stringata)
			  current += 2;
		      else
			  current += 1;
		      break;
		  }
		StringAt(&stringata,original, current, 2, "SC", "");
		if (stringata)
		  {
		      /* Schlesinger's rule */
		      GetAt(&getata,original, current + 2);
		      if (getata == 'H')
			  {
			  /* dutch origin, e.g. 'school', 'schooner' */
			  StringAt(&stringata,original, (current + 3), 2, "OO", "ER", "EN",
			               "UY", "ED", "EM", "");
			  if (stringata)
			    {
				/* 'schermerhorn', 'schenker' */
				StringAt(&stringata,original, (current + 3), 2, "ER", "EN", "");
				if (stringata)
				  {
				      MetaphAdd(primary, "X");
				      MetaphAdd(secondary, "SK");
				  }
				else
                                  {
				      MetaphAdd(primary, "SK");
				      MetaphAdd(secondary, "SK");
                                  }
				current += 3;
				break;
			    }
			  else
			    {
			    	IsVowel(original, 3, &answer);
			    	GetAt(&getata,original, 3);
				if ((current == 0) && !answer
				    && (getata != 'W'))
				  {
				      MetaphAdd(primary, "X");
				      MetaphAdd(secondary, "S");
				  }
				else
				  {
				      MetaphAdd(primary, "X");
				      MetaphAdd(secondary, "X");
				  }
				current += 3;
				break;
			    }
					StringAt(&stringata,original, (current + 2), 1, "I", "E", "Y", "");
		      if (stringata)
			{
			    MetaphAdd(primary, "S");
			    MetaphAdd(secondary, "S");
			    current += 3;
			    break;
			}
		      /* else */
		      MetaphAdd(primary, "SK");
		      MetaphAdd(secondary, "SK");
		      current += 3;
		      break;
		  }
		}

		/* french e.g. 'resnais', 'artois' */
		StringAt(&stringata,original, (current - 2), 2, "AI", "OI", "");
		if ((current == last)
		    && stringata)
		  {
		      MetaphAdd(primary, "");
		      MetaphAdd(secondary, "S");
		  }
		else
		  {
		      MetaphAdd(primary, "S");
		      MetaphAdd(secondary, "S");
		  }
		StringAt(&stringata,original, (current + 1), 1, "S", "Z", "");
		if (stringata)
		    current += 2;
		else
		    current += 1;
		break;

	    case 'T':
	    	StringAt(&stringata,original, current, 4, "TION", "");
		if (stringata)
		  {
		      MetaphAdd(primary, "X");
		      MetaphAdd(secondary, "X");
		      current += 3;
		      break;
		  }
		StringAt(&stringata,original, current, 3, "TIA", "TCH", "");
		if (stringata)
		  {
		      MetaphAdd(primary, "X");
		      MetaphAdd(secondary, "X");
		      current += 3;
		      break;
		  }
		StringAt(&stringata,original, current, 2, "TH", "");
		StringAt(&stringata1,original, current, 3, "TTH", "");
		if (stringata
		    || stringata1)
		  {
		      /* special case 'thomas', 'thames' or germanic */
		      StringAt(&stringata,original, (current + 2), 2, "OM", "AM", "");
			 		StringAt(&stringata1,original, 0, 4, "VAN ", "VON ", "");
					StringAt(&stringata2,original, 0, 3, "SCH", "");
		      if (stringata
			  || stringata1
			  || stringata2)
			{
			    MetaphAdd(primary, "T");
			    MetaphAdd(secondary, "T");
			}
		      else
			{
			    MetaphAdd(primary, "0"); /* yes, zero */
			    MetaphAdd(secondary, "T");
			}
		      current += 2;
		      break;
		  }
		StringAt(&stringata,original, (current + 1), 1, "T", "D", "");
		if (stringata)
		    current += 2;
		else
		    current += 1;
		MetaphAdd(primary, "T");
		MetaphAdd(secondary, "T");
		break;

	    case 'V':
	    	GetAt(&getata,original, current + 1);
		if (getata == 'V')
		    current += 2;
		else
		    current += 1;
		MetaphAdd(primary, "F");
		MetaphAdd(secondary, "F");
		break;

	    case 'W':
		/* can also be in middle of word */
		StringAt(&stringata,original, current, 2, "WR", "");
		if (stringata)
		  {
		      MetaphAdd(primary, "R");
		      MetaphAdd(secondary, "R");
		      current += 2;
		      break;
		  }
			IsVowel(original, current + 1,&answer);
			StringAt(&stringata,original, current, 2, "WH", "");
		if ((current == 0)
		    && (answer
			|| stringata))
		  {
		      /* Wasserman should match Vasserman */
		      IsVowel(original, current + 1,&answer);
		      if (answer)
			{
			    MetaphAdd(primary, "A");
			    MetaphAdd(secondary, "F");
			}
		      else
			{
			    /* need Uomo to match Womo */
			    MetaphAdd(primary, "A");
			    MetaphAdd(secondary, "A");
			}
		  }

		/* Arnow should match Arnoff */
		IsVowel(original, current - 1,&answer);
		StringAt(&stringata,original, (current - 1), 5, "EWSKI", "EWSKY",
				"OWSKI", "OWSKY", "");
		StringAt(&stringata1,original, 0, 3, "SCH", "");
		if (((current == last) && answer)
		    || stringata
		    || stringata1)
		  {
		      MetaphAdd(primary, "");
		      MetaphAdd(secondary, "F");
		      current += 1;
		      break;
		  }

		/* polish e.g. 'filipowicz' */
		StringAt(&stringata,original, current, 4, "WICZ", "WITZ", "");
		if (stringata)
		  {
		      MetaphAdd(primary, "TS");
		      MetaphAdd(secondary, "FX");
		      current += 4;
		      break;
		  }

		/* else skip it */
		current += 1;
		break;

	    case 'X':
		/* french e.g. breaux */
		StringAt(&stringata,original, (current - 3), 3, "IAU", "EAU", "");
		StringAt(&stringata1,original, (current - 2), 2, "AU", "OU", "");
		if (!((current == last)
		      && (stringata
		       || stringata1)))
                  {
		      MetaphAdd(primary, "KS");
		      MetaphAdd(secondary, "KS");
                  }
                  
		StringAt(&stringata,original, (current + 1), 1, "C", "X", "");
		if (stringata)
		    current += 2;
		else
		    current += 1;
		break;

	    case 'Z':
		/* chinese pinyin e.g. 'zhao' */
		SlavoGermanic(original,&answer2);
		StringAt(&stringata,original, (current + 1), 2, "ZO", "ZI", "ZA", "");
		GetAt(&getata,original, current + 1);
		GetAt(&getata1,original, current - 1);
		if (getata == 'H')
		  {
		      MetaphAdd(primary, "J");
		      MetaphAdd(secondary, "J");
		      current += 2;
		      break;
		  }
		else if (stringata
			|| (answer2
			    && ((current > 0)
				&& getata1 != 'T')))
		  {
		      MetaphAdd(primary, "S");
		      MetaphAdd(secondary, "TS");
		  }
		else
                  {
		    MetaphAdd(primary, "S");
		    MetaphAdd(secondary, "S");
                  }
		GetAt(&getata,original, current + 1);
		if (getata == 'Z')
		    current += 2;
		else
		    current += 1;
		break;

	    default:
		current += 1;
	    }
        /* printf("PRIMARY: %s\n", primary->str);
        printf("SECONDARY: %s\n", secondary->str);  */
      }


    if (primary->length > 4)
	SetAt(primary, 4, '\0');

    if (secondary->length > 4)
	SetAt(secondary, 4, '\0');

    //*codes = primary->str;
    //*codes1 = secondary->str;

    strcpy(*codes,primary->str);
    strcpy(*codes1,secondary->str);
	
	  //[qrt]strncpy(*codes,primary->str, strlen(*codes) + 1);
	  //[qrt] strncpy(*codes1,secondary->str, strlen(*codes1) + 1);
	
    DestroyMetaString(original);
    DestroyMetaString(primary);
    DestroyMetaString(secondary);
}


