/* A Bison parser, made by GNU Bison 2.5.  */

/* Bison implementation for Yacc-like parsers in C
   
      Copyright (C) 1984, 1989-1990, 2000-2011 Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.
   
   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.5"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1

/* Using locations.  */
#define YYLSP_NEEDED 0

/* Substitute the variable and function names.  */
#define yyparse         swf5parse
#define yylex           swf5lex
#define yyerror         swf5error
#define yylval          swf5lval
#define yychar          swf5char
#define yydebug         swf5debug
#define yynerrs         swf5nerrs


/* Copy the first part of user declarations.  */



#include <time.h>
#include <string.h>
#include <stdlib.h>
#include "compile.h"
#include <actiontypes.h>
#include "assembler.h"

#define YYERROR_VERBOSE 1
#define YYPARSE_PARAM buffer
/* #define DEBUG 1 */

#ifdef _MSC_VER 
#define strcasecmp stricmp 
#endif


Buffer bf, bc;
static int classContext = 0;




/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     BREAK = 258,
     CONTINUE = 259,
     FUNCTION = 260,
     ELSE = 261,
     SWITCH = 262,
     CASE = 263,
     DEFAULT = 264,
     FOR = 265,
     IN = 266,
     IF = 267,
     WHILE = 268,
     DO = 269,
     VAR = 270,
     NEW = 271,
     DELETE = 272,
     DELETE2 = 273,
     TARGETPATH = 274,
     RETURN = 275,
     END = 276,
     WITH = 277,
     ASM = 278,
     EVAL = 279,
     SWFACTION = 280,
     RANDOM = 281,
     GETTIMER = 282,
     LENGTH = 283,
     CONCAT = 284,
     SUBSTR = 285,
     TRACE = 286,
     INT = 287,
     ORD = 288,
     CHR = 289,
     GETURL = 290,
     GETURL1 = 291,
     NEXTFRAME = 292,
     PREVFRAME = 293,
     PLAY = 294,
     STOP = 295,
     TOGGLEQUALITY = 296,
     STOPSOUNDS = 297,
     DUP = 298,
     SWAP = 299,
     POP = 300,
     PUSH = 301,
     SETREGISTER = 302,
     CALLFUNCTION = 303,
     CALLMETHOD = 304,
     NEWOBJECT = 305,
     NEWMETHOD = 306,
     AND = 307,
     OR = 308,
     XOR = 309,
     MODULO = 310,
     ADD = 311,
     LESSTHAN = 312,
     EQUALS = 313,
     INC = 314,
     DEC = 315,
     TYPEOF = 316,
     INSTANCEOF = 317,
     ENUMERATE = 318,
     ENUMERATE2 = 319,
     INITOBJECT = 320,
     INITARRAY = 321,
     GETMEMBER = 322,
     SETMEMBER = 323,
     SHIFTLEFT = 324,
     SHIFTRIGHT = 325,
     SHIFTRIGHT2 = 326,
     VAREQUALS = 327,
     OLDADD = 328,
     SUBTRACT = 329,
     MULTIPLY = 330,
     DIVIDE = 331,
     OLDEQUALS = 332,
     OLDLESSTHAN = 333,
     LOGICALAND = 334,
     LOGICALOR = 335,
     NOT = 336,
     STRINGEQ = 337,
     STRINGLENGTH = 338,
     SUBSTRING = 339,
     GETVARIABLE = 340,
     SETVARIABLE = 341,
     SETTARGETEXPRESSION = 342,
     DUPLICATEMOVIECLIP = 343,
     REMOVEMOVIECLIP = 344,
     STRINGLESSTHAN = 345,
     MBLENGTH = 346,
     MBSUBSTRING = 347,
     MBORD = 348,
     MBCHR = 349,
     BRANCHALWAYS = 350,
     BRANCHIFTRUE = 351,
     GETURL2 = 352,
     POST = 353,
     GET = 354,
     CAST = 355,
     LOADVARIABLES = 356,
     LOADMOVIE = 357,
     LOADVARIABLESNUM = 358,
     LOADMOVIENUM = 359,
     CALLFRAME = 360,
     STARTDRAG = 361,
     STOPDRAG = 362,
     GOTOANDSTOP = 363,
     GOTOANDPLAY = 364,
     SETTARGET = 365,
     GETPROPERTY = 366,
     SETPROPERTY = 367,
     TONUMBER = 368,
     TOSTRING = 369,
     TRY = 370,
     THROW = 371,
     CATCH = 372,
     FINALLY = 373,
     THIS = 374,
     EXTENDS = 375,
     IMPLEMENTS = 376,
     FSCOMMAND2 = 377,
     CLASS = 378,
     PUBLIC = 379,
     PRIVATE = 380,
     NULLVAL = 381,
     UNDEFINED = 382,
     INTEGER = 383,
     DOUBLE = 384,
     BOOLEAN = 385,
     REGISTER = 386,
     BROKENSTRING = 387,
     STRING = 388,
     IDENTIFIER = 389,
     EQ = 390,
     EEQ = 391,
     NEE = 392,
     LE = 393,
     GE = 394,
     NE = 395,
     LAN = 396,
     LOR = 397,
     INCR = 398,
     DECR = 399,
     IEQ = 400,
     DEQ = 401,
     MEQ = 402,
     SEQ = 403,
     REQ = 404,
     AEQ = 405,
     OEQ = 406,
     SHL = 407,
     SHR = 408,
     SHR2 = 409,
     SHLEQ = 410,
     SHREQ = 411,
     SHR2EQ = 412,
     _P_X = 413,
     _P_Y = 414,
     _P_XSCALE = 415,
     _P_YSCALE = 416,
     _P_CURRENTFRAME = 417,
     _P_TOTALFRAMES = 418,
     _P_ALPHA = 419,
     _P_VISIBLE = 420,
     _P_WIDTH = 421,
     _P_HEIGHT = 422,
     _P_ROTATION = 423,
     _P_TARGET = 424,
     _P_FRAMESLOADED = 425,
     _P_NAME = 426,
     _P_DROPTARGET = 427,
     _P_URL = 428,
     _P_HIGHQUALITY = 429,
     _P_FOCUSRECT = 430,
     _P_SOUNDBUFTIME = 431,
     _P_QUALITY = 432,
     _P_XMOUSE = 433,
     _P_YMOUSE = 434,
     NOELSE = 435,
     UMINUS = 437,
     POSTFIX = 438
   };
#endif
/* Tokens.  */
#define BREAK 258
#define CONTINUE 259
#define FUNCTION 260
#define ELSE 261
#define SWITCH 262
#define CASE 263
#define DEFAULT 264
#define FOR 265
#define IN 266
#define IF 267
#define WHILE 268
#define DO 269
#define VAR 270
#define NEW 271
#define DELETE 272
#define DELETE2 273
#define TARGETPATH 274
#define RETURN 275
#define END 276
#define WITH 277
#define ASM 278
#define EVAL 279
#define SWFACTION 280
#define RANDOM 281
#define GETTIMER 282
#define LENGTH 283
#define CONCAT 284
#define SUBSTR 285
#define TRACE 286
#define INT 287
#define ORD 288
#define CHR 289
#define GETURL 290
#define GETURL1 291
#define NEXTFRAME 292
#define PREVFRAME 293
#define PLAY 294
#define STOP 295
#define TOGGLEQUALITY 296
#define STOPSOUNDS 297
#define DUP 298
#define SWAP 299
#define POP 300
#define PUSH 301
#define SETREGISTER 302
#define CALLFUNCTION 303
#define CALLMETHOD 304
#define NEWOBJECT 305
#define NEWMETHOD 306
#define AND 307
#define OR 308
#define XOR 309
#define MODULO 310
#define ADD 311
#define LESSTHAN 312
#define EQUALS 313
#define INC 314
#define DEC 315
#define TYPEOF 316
#define INSTANCEOF 317
#define ENUMERATE 318
#define ENUMERATE2 319
#define INITOBJECT 320
#define INITARRAY 321
#define GETMEMBER 322
#define SETMEMBER 323
#define SHIFTLEFT 324
#define SHIFTRIGHT 325
#define SHIFTRIGHT2 326
#define VAREQUALS 327
#define OLDADD 328
#define SUBTRACT 329
#define MULTIPLY 330
#define DIVIDE 331
#define OLDEQUALS 332
#define OLDLESSTHAN 333
#define LOGICALAND 334
#define LOGICALOR 335
#define NOT 336
#define STRINGEQ 337
#define STRINGLENGTH 338
#define SUBSTRING 339
#define GETVARIABLE 340
#define SETVARIABLE 341
#define SETTARGETEXPRESSION 342
#define DUPLICATEMOVIECLIP 343
#define REMOVEMOVIECLIP 344
#define STRINGLESSTHAN 345
#define MBLENGTH 346
#define MBSUBSTRING 347
#define MBORD 348
#define MBCHR 349
#define BRANCHALWAYS 350
#define BRANCHIFTRUE 351
#define GETURL2 352
#define POST 353
#define GET 354
#define CAST 355
#define LOADVARIABLES 356
#define LOADMOVIE 357
#define LOADVARIABLESNUM 358
#define LOADMOVIENUM 359
#define CALLFRAME 360
#define STARTDRAG 361
#define STOPDRAG 362
#define GOTOANDSTOP 363
#define GOTOANDPLAY 364
#define SETTARGET 365
#define GETPROPERTY 366
#define SETPROPERTY 367
#define TONUMBER 368
#define TOSTRING 369
#define TRY 370
#define THROW 371
#define CATCH 372
#define FINALLY 373
#define THIS 374
#define EXTENDS 375
#define IMPLEMENTS 376
#define FSCOMMAND2 377
#define CLASS 378
#define PUBLIC 379
#define PRIVATE 380
#define NULLVAL 381
#define UNDEFINED 382
#define INTEGER 383
#define DOUBLE 384
#define BOOLEAN 385
#define REGISTER 386
#define BROKENSTRING 387
#define STRING 388
#define IDENTIFIER 389
#define EQ 390
#define EEQ 391
#define NEE 392
#define LE 393
#define GE 394
#define NE 395
#define LAN 396
#define LOR 397
#define INCR 398
#define DECR 399
#define IEQ 400
#define DEQ 401
#define MEQ 402
#define SEQ 403
#define REQ 404
#define AEQ 405
#define OEQ 406
#define SHL 407
#define SHR 408
#define SHR2 409
#define SHLEQ 410
#define SHREQ 411
#define SHR2EQ 412
#define _P_X 413
#define _P_Y 414
#define _P_XSCALE 415
#define _P_YSCALE 416
#define _P_CURRENTFRAME 417
#define _P_TOTALFRAMES 418
#define _P_ALPHA 419
#define _P_VISIBLE 420
#define _P_WIDTH 421
#define _P_HEIGHT 422
#define _P_ROTATION 423
#define _P_TARGET 424
#define _P_FRAMESLOADED 425
#define _P_NAME 426
#define _P_DROPTARGET 427
#define _P_URL 428
#define _P_HIGHQUALITY 429
#define _P_FOCUSRECT 430
#define _P_SOUNDBUFTIME 431
#define _P_QUALITY 432
#define _P_XMOUSE 433
#define _P_YMOUSE 434
#define NOELSE 435
#define UMINUS 437
#define POSTFIX 438




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{
  Buffer action;
  char *str;
  SWFGetUrl2Method getURLMethod;
  int op;
  int intVal;
  int len;
  double doubleVal;
  ASFunction 		function;
  ASClass		clazz;
  ASClassMember		classMember;
  struct exprlist_s 	exprlist;
  struct switchcase	switchcase;
  struct switchcases	switchcases;
  struct
  {
	Buffer obj, ident, memexpr;
  } lval;
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif


/* Copy the second part of user declarations.  */



#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int yyi)
#else
static int
YYID (yyi)
    int yyi;
#endif
{
  return yyi;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)				\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack_alloc, Stack, yysize);			\
	Stack = &yyptr->Stack_alloc;					\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   6748

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  208
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  72
/* YYNRULES -- Number of rules.  */
#define YYNRULES  456
/* YYNRULES -- Number of states.  */
#define YYNSTATES  766

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   438

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   196,     2,     2,     2,   195,   188,     2,
     206,   207,   193,   191,   181,   192,   200,   194,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   185,   205,
     186,   182,   187,   184,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   201,     2,   202,   190,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   203,   189,   204,   197,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,   134,
     135,   136,   137,   138,   139,   140,   141,   142,   143,   144,
     145,   146,   147,   148,   149,   150,   151,   152,   153,   154,
     155,   156,   157,   158,   159,   160,   161,   162,   163,   164,
     165,   166,   167,   168,   169,   170,   171,   172,   173,   174,
     175,   176,   177,   178,   179,   180,   183,   198,   199
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     4,     7,     8,    10,    13,    15,    17,
      19,    21,    24,    27,    29,    33,    35,    38,    40,    42,
      44,    46,    48,    50,    52,    55,    57,    59,    62,    65,
      70,    72,    74,    78,    82,    88,    89,    91,    93,    94,
      97,    99,   103,   108,   111,   115,   118,   126,   131,   141,
     149,   152,   156,   158,   162,   170,   176,   177,   179,   181,
     189,   190,   193,   198,   202,   204,   206,   208,   210,   212,
     214,   216,   218,   220,   222,   224,   226,   228,   230,   232,
     234,   236,   238,   240,   242,   244,   246,   248,   250,   252,
     254,   256,   258,   260,   262,   264,   266,   268,   270,   272,
     274,   276,   278,   280,   282,   284,   286,   288,   290,   292,
     294,   296,   298,   300,   302,   304,   306,   308,   310,   312,
     314,   316,   318,   320,   322,   324,   326,   328,   330,   332,
     334,   336,   338,   340,   342,   344,   346,   348,   350,   352,
     354,   356,   358,   360,   362,   364,   366,   368,   370,   372,
     374,   376,   378,   380,   382,   384,   386,   388,   390,   392,
     394,   396,   398,   400,   402,   404,   406,   408,   410,   412,
     414,   416,   418,   420,   422,   423,   426,   431,   432,   434,
     442,   445,   447,   449,   451,   453,   455,   457,   459,   463,
     465,   467,   468,   469,   475,   482,   493,   501,   510,   511,
     513,   516,   519,   520,   523,   526,   529,   531,   536,   541,
     549,   557,   565,   573,   581,   586,   593,   608,   612,   621,
     626,   633,   637,   641,   645,   649,   653,   657,   662,   667,
     672,   677,   682,   687,   692,   697,   706,   711,   716,   721,
     725,   730,   735,   740,   745,   750,   757,   766,   771,   778,
     781,   782,   784,   788,   795,   803,   807,   809,   813,   815,
     817,   819,   821,   823,   825,   827,   829,   831,   833,   835,
     837,   839,   841,   843,   845,   847,   851,   856,   858,   860,
     863,   866,   869,   873,   877,   881,   885,   889,   893,   897,
     901,   905,   909,   913,   917,   921,   925,   929,   933,   937,
     941,   945,   949,   953,   959,   963,   967,   969,   972,   978,
     983,   989,   997,  1006,  1010,  1012,  1016,  1018,  1020,  1022,
    1025,  1028,  1032,  1036,  1038,  1040,  1042,  1044,  1046,  1048,
    1050,  1054,  1059,  1062,  1063,  1069,  1072,  1074,  1076,  1078,
    1080,  1083,  1086,  1090,  1094,  1096,  1099,  1100,  1105,  1107,
    1109,  1111,  1113,  1115,  1117,  1119,  1121,  1125,  1126,  1130,
    1132,  1135,  1137,  1139,  1141,  1143,  1145,  1147,  1149,  1151,
    1153,  1155,  1157,  1159,  1161,  1163,  1165,  1167,  1169,  1171,
    1173,  1175,  1177,  1179,  1181,  1183,  1185,  1187,  1189,  1191,
    1193,  1195,  1197,  1200,  1202,  1204,  1206,  1208,  1210,  1212,
    1214,  1216,  1218,  1220,  1222,  1224,  1226,  1228,  1230,  1232,
    1234,  1236,  1238,  1240,  1242,  1244,  1246,  1248,  1250,  1252,
    1254,  1256,  1258,  1260,  1262,  1264,  1266,  1268,  1270,  1272,
    1274,  1276,  1278,  1281,  1284,  1286,  1288,  1290,  1292,  1294,
    1296,  1298,  1300,  1302,  1304,  1306,  1308,  1310,  1312,  1314,
    1316,  1318,  1320,  1322,  1324,  1326,  1328
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
     209,     0,    -1,    -1,   210,   211,    -1,    -1,   212,    -1,
     211,   212,    -1,   215,    -1,   239,    -1,   220,    -1,   215,
      -1,   213,   215,    -1,   203,   204,    -1,   214,    -1,   203,
     213,   204,    -1,   205,    -1,   270,   205,    -1,   230,    -1,
     247,    -1,   249,    -1,   250,    -1,   233,    -1,   228,    -1,
     227,    -1,   226,   205,    -1,   225,    -1,   217,    -1,   216,
     217,    -1,   221,   239,    -1,   221,    15,   223,   205,    -1,
     123,    -1,   214,    -1,   203,   216,   204,    -1,   218,   236,
     219,    -1,   218,   236,   120,   236,   219,    -1,    -1,   124,
      -1,   125,    -1,    -1,   185,   236,    -1,   224,    -1,   223,
     181,   224,    -1,   236,   222,   182,   267,    -1,   236,   222,
      -1,   116,   265,   205,    -1,   115,   215,    -1,   115,   215,
     117,   206,   236,   207,   215,    -1,   115,   215,   118,   215,
      -1,   115,   215,   117,   206,   236,   207,   215,   118,   215,
      -1,    22,   206,   264,   207,   203,   213,   204,    -1,    20,
     205,    -1,    20,   265,   205,    -1,   270,    -1,   229,   181,
     270,    -1,    12,   206,   264,   207,   215,     6,   215,    -1,
      12,   206,   264,   207,   215,    -1,    -1,   264,    -1,     7,
      -1,   232,   206,   264,   207,   203,   234,   204,    -1,    -1,
     234,   235,    -1,     8,   264,   185,   213,    -1,     9,   185,
     213,    -1,   134,    -1,    19,    -1,    26,    -1,    27,    -1,
      28,    -1,    29,    -1,    30,    -1,    31,    -1,    32,    -1,
      33,    -1,    34,    -1,    35,    -1,    36,    -1,    37,    -1,
      38,    -1,    39,    -1,    40,    -1,    41,    -1,    42,    -1,
      43,    -1,    44,    -1,    45,    -1,    46,    -1,    47,    -1,
      48,    -1,    49,    -1,    52,    -1,    53,    -1,    54,    -1,
      55,    -1,    56,    -1,    57,    -1,    58,    -1,    59,    -1,
      60,    -1,    61,    -1,    64,    -1,    63,    -1,    65,    -1,
      66,    -1,    67,    -1,    68,    -1,    69,    -1,    70,    -1,
      71,    -1,    72,    -1,    73,    -1,    74,    -1,    75,    -1,
      76,    -1,    77,    -1,    78,    -1,    79,    -1,    80,    -1,
      81,    -1,    82,    -1,    83,    -1,    84,    -1,    85,    -1,
      86,    -1,    87,    -1,    88,    -1,    89,    -1,   106,    -1,
     107,    -1,    90,    -1,    91,    -1,    92,    -1,    93,    -1,
      94,    -1,    95,    -1,    96,    -1,    97,    -1,    98,    -1,
      99,    -1,   101,    -1,   103,    -1,   102,    -1,   104,    -1,
     108,    -1,   109,    -1,   110,    -1,   105,    -1,   111,    -1,
     112,    -1,   100,    -1,    25,    -1,   119,    -1,   158,    -1,
     159,    -1,   160,    -1,   161,    -1,   162,    -1,   163,    -1,
     164,    -1,   165,    -1,   166,    -1,   167,    -1,   168,    -1,
     169,    -1,   170,    -1,   171,    -1,   172,    -1,   173,    -1,
     174,    -1,   175,    -1,   176,    -1,   177,    -1,   178,    -1,
     179,    -1,    -1,   236,   222,    -1,   237,   181,   236,   222,
      -1,    -1,   236,    -1,     5,   238,   206,   237,   207,   222,
     215,    -1,    11,   241,    -1,   263,    -1,   254,    -1,   257,
      -1,   263,    -1,   254,    -1,   253,    -1,   257,    -1,   206,
     242,   207,    -1,    13,    -1,    14,    -1,    -1,    -1,   243,
     206,   264,   207,   215,    -1,   244,   215,    13,   206,   264,
     207,    -1,    10,   206,   248,   205,   231,   205,   248,   207,
     245,   215,    -1,    10,   206,   236,   240,   207,   246,   215,
      -1,    10,   206,    15,   236,   240,   207,   246,   215,    -1,
      -1,   229,    -1,     4,   205,    -1,     3,   205,    -1,    -1,
     181,    99,    -1,   181,    98,    -1,   181,   133,    -1,   264,
      -1,    31,   206,   265,   207,    -1,    35,   206,   264,   207,
      -1,    35,   206,   264,   181,   264,   251,   207,    -1,   101,
     206,   264,   181,   264,   251,   207,    -1,   103,   206,   264,
     181,   252,   251,   207,    -1,   102,   206,   264,   181,   264,
     251,   207,    -1,   104,   206,   264,   181,   252,   251,   207,
      -1,   105,   206,   264,   207,    -1,   106,   206,   264,   181,
     264,   207,    -1,   106,   206,   264,   181,   264,   181,   264,
     181,   264,   181,   264,   181,   264,   207,    -1,   107,   206,
     207,    -1,    88,   206,   264,   181,   264,   181,   264,   207,
      -1,    89,   206,   264,   207,    -1,    36,   206,   133,   181,
     133,   207,    -1,    37,   206,   207,    -1,    38,   206,   207,
      -1,    39,   206,   207,    -1,    40,   206,   207,    -1,    42,
     206,   207,    -1,    41,   206,   207,    -1,   109,   206,   128,
     207,    -1,   108,   206,   128,   207,    -1,   109,   206,   133,
     207,    -1,   108,   206,   133,   207,    -1,   109,   206,   264,
     207,    -1,   108,   206,   264,   207,    -1,   110,   206,   133,
     207,    -1,   110,   206,   264,   207,    -1,   112,   206,   264,
     181,   279,   181,   264,   207,    -1,   134,   206,   256,   207,
      -1,    19,   206,   134,   207,    -1,    24,   206,   264,   207,
      -1,    27,   206,   207,    -1,    26,   206,   264,   207,    -1,
      28,   206,   265,   207,    -1,    32,   206,   264,   207,    -1,
      33,   206,   264,   207,    -1,    34,   206,   264,   207,    -1,
      29,   206,   264,   181,   264,   207,    -1,    30,   206,   264,
     181,   264,   181,   264,   207,    -1,    61,   206,   265,   207,
      -1,   111,   206,   264,   181,   279,   207,    -1,    17,   242,
      -1,    -1,   265,    -1,   256,   181,   265,    -1,   262,   200,
     236,   206,   256,   207,    -1,   262,   201,   264,   202,   206,
     256,   207,    -1,   236,   185,   265,    -1,   258,    -1,   259,
     181,   258,    -1,   145,    -1,   148,    -1,   147,    -1,   146,
      -1,   149,    -1,   150,    -1,   151,    -1,   183,    -1,   155,
      -1,   156,    -1,   157,    -1,   143,    -1,   144,    -1,   263,
      -1,   254,    -1,   257,    -1,   236,    -1,   262,   200,   236,
      -1,   262,   201,   264,   202,    -1,   266,    -1,   267,    -1,
     192,   264,    -1,   197,   264,    -1,   196,   264,    -1,   264,
     141,   264,    -1,   264,   142,   264,    -1,   264,   193,   264,
      -1,   264,   194,   264,    -1,   264,   195,   264,    -1,   264,
     191,   264,    -1,   264,   192,   264,    -1,   264,   188,   264,
      -1,   264,   189,   264,    -1,   264,   190,   264,    -1,   264,
     186,   264,    -1,   264,   187,   264,    -1,   264,   138,   264,
      -1,   264,   139,   264,    -1,   264,   135,   264,    -1,   264,
     136,   264,    -1,   264,   140,   264,    -1,   264,   137,   264,
      -1,   264,   152,   264,    -1,   264,   153,   264,    -1,   264,
     154,   264,    -1,   264,   184,   264,   185,   264,    -1,   263,
     182,   265,    -1,   264,    62,   262,    -1,   264,    -1,    16,
     236,    -1,    16,   134,   206,   256,   207,    -1,    16,   262,
     200,   134,    -1,    16,   262,   201,   264,   202,    -1,    16,
     262,   200,   134,   206,   256,   207,    -1,    16,   262,   201,
     264,   202,   206,   256,   207,    -1,   201,   256,   202,    -1,
     214,    -1,   203,   259,   204,    -1,   239,    -1,   262,    -1,
     255,    -1,   261,   263,    -1,   263,   261,    -1,   206,   264,
     207,    -1,   263,   260,   264,    -1,   130,    -1,   126,    -1,
     127,    -1,   133,    -1,   128,    -1,   129,    -1,   269,    -1,
     268,   181,   269,    -1,   236,   222,   182,   265,    -1,   236,
     222,    -1,    -1,    23,   203,   271,   272,   204,    -1,    15,
     268,    -1,   253,    -1,   255,    -1,   254,    -1,   257,    -1,
     261,   263,    -1,   263,   261,    -1,   263,   182,   265,    -1,
     263,   260,   264,    -1,   277,    -1,   272,   277,    -1,    -1,
      22,   274,   272,    21,    -1,   133,    -1,   128,    -1,   129,
      -1,   130,    -1,   126,    -1,   127,    -1,   131,    -1,   275,
      -1,   276,   181,   275,    -1,    -1,    46,   278,   276,    -1,
     273,    -1,    47,   131,    -1,    48,    -1,    20,    -1,    49,
      -1,    50,    -1,    51,    -1,    52,    -1,    53,    -1,    54,
      -1,    55,    -1,    56,    -1,    57,    -1,    58,    -1,    59,
      -1,    60,    -1,    61,    -1,    62,    -1,    63,    -1,    64,
      -1,    17,    -1,    18,    -1,    16,    -1,    66,    -1,    65,
      -1,    67,    -1,    68,    -1,    69,    -1,    70,    -1,    71,
      -1,    15,    -1,   120,    -1,    19,    -1,    25,   128,    -1,
     121,    -1,   122,    -1,   100,    -1,    73,    -1,    74,    -1,
      75,    -1,    76,    -1,    77,    -1,    78,    -1,    79,    -1,
      80,    -1,    81,    -1,    82,    -1,    83,    -1,    84,    -1,
      32,    -1,    43,    -1,    44,    -1,    45,    -1,    85,    -1,
      86,    -1,   111,    -1,   112,    -1,   113,    -1,   114,    -1,
      87,    -1,    29,    -1,    88,    -1,    89,    -1,    31,    -1,
      90,    -1,    26,    -1,    91,    -1,    33,    -1,    34,    -1,
      27,    -1,    92,    -1,    93,    -1,    94,    -1,    95,   133,
      -1,    96,   133,    -1,   133,    -1,   158,    -1,   159,    -1,
     160,    -1,   161,    -1,   162,    -1,   163,    -1,   164,    -1,
     165,    -1,   166,    -1,   167,    -1,   168,    -1,   169,    -1,
     170,    -1,   171,    -1,   172,    -1,   173,    -1,   174,    -1,
     175,    -1,   176,    -1,   177,    -1,   178,    -1,   179,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   184,   184,   184,   192,   196,   197,   201,   203,   211,
     216,   219,   225,   229,   230,   231,   232,   233,   234,   235,
     236,   237,   238,   239,   240,   241,   246,   247,   255,   256,
     260,   272,   273,   277,   282,   289,   291,   292,   295,   297,
     300,   302,   309,   314,   322,   326,   336,   350,   361,   379,
     390,   403,   418,   419,   423,   434,   444,   445,   449,   454,
     468,   471,   479,   491,   509,   510,   511,   512,   513,   514,
     515,   516,   517,   518,   519,   520,   521,   522,   523,   524,
     525,   526,   527,   528,   529,   530,   531,   532,   533,   534,
     535,   536,   537,   538,   539,   540,   541,   542,   543,   544,
     545,   546,   547,   548,   549,   550,   551,   552,   553,   554,
     555,   556,   557,   558,   559,   560,   561,   562,   563,   564,
     565,   566,   567,   568,   569,   570,   571,   572,   573,   574,
     575,   576,   577,   578,   579,   580,   581,   582,   583,   584,
     585,   586,   587,   588,   589,   590,   591,   592,   593,   594,
     595,   597,   600,   601,   602,   603,   604,   605,   606,   607,
     608,   609,   610,   611,   612,   613,   614,   615,   616,   617,
     618,   619,   620,   621,   626,   629,   635,   643,   644,   648,
     659,   664,   681,   682,   687,   702,   703,   704,   705,   709,
     714,   720,   725,   729,   742,   755,   794,   831,   869,   870,
     876,   891,   916,   918,   920,   922,   931,   940,   944,   956,
     967,   974,   981,   988,   995,  1001,  1008,  1019,  1024,  1032,
    1036,  1046,  1050,  1054,  1058,  1062,  1066,  1070,  1077,  1084,
    1092,  1100,  1106,  1112,  1119,  1123,  1134,  1145,  1152,  1156,
    1160,  1164,  1168,  1172,  1176,  1180,  1185,  1191,  1199,  1209,
    1221,  1224,  1229,  1238,  1246,  1255,  1263,  1267,  1273,  1274,
    1275,  1276,  1277,  1278,  1279,  1280,  1281,  1282,  1283,  1287,
    1288,  1294,  1311,  1312,  1317,  1324,  1331,  1340,  1342,  1344,
    1349,  1354,  1358,  1368,  1377,  1382,  1387,  1392,  1397,  1402,
    1407,  1412,  1417,  1422,  1427,  1433,  1438,  1442,  1446,  1451,
    1456,  1460,  1464,  1468,  1478,  1505,  1513,  1515,  1526,  1537,
    1549,  1561,  1573,  1584,  1589,  1594,  1602,  1615,  1617,  1619,
    1664,  1709,  1713,  1762,  1765,  1769,  1773,  1777,  1780,  1786,
    1788,  1794,  1801,  1810,  1809,  1814,  1817,  1819,  1823,  1827,
    1831,  1870,  1909,  1930,  1975,  1976,  1981,  1980,  1988,  1991,
    1993,  1995,  1999,  2002,  2005,  2013,  2014,  2018,  2018,  2023,
    2025,  2032,  2034,  2036,  2038,  2040,  2042,  2044,  2046,  2048,
    2050,  2052,  2054,  2056,  2058,  2060,  2062,  2064,  2066,  2068,
    2070,  2072,  2074,  2076,  2078,  2080,  2082,  2084,  2086,  2088,
    2090,  2092,  2096,  2099,  2101,  2103,  2107,  2108,  2109,  2110,
    2111,  2112,  2113,  2114,  2115,  2116,  2117,  2118,  2119,  2120,
    2121,  2122,  2123,  2124,  2125,  2126,  2127,  2128,  2129,  2130,
    2131,  2132,  2133,  2134,  2135,  2136,  2137,  2138,  2139,  2140,
    2141,  2142,  2145,  2149,  2156,  2157,  2158,  2159,  2160,  2161,
    2162,  2163,  2164,  2165,  2166,  2167,  2168,  2169,  2170,  2171,
    2172,  2173,  2174,  2175,  2176,  2177,  2178
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "BREAK", "CONTINUE", "FUNCTION", "ELSE",
  "SWITCH", "CASE", "DEFAULT", "FOR", "IN", "IF", "WHILE", "DO", "VAR",
  "NEW", "DELETE", "DELETE2", "TARGETPATH", "RETURN", "END", "WITH", "ASM",
  "EVAL", "SWFACTION", "RANDOM", "GETTIMER", "LENGTH", "CONCAT", "SUBSTR",
  "TRACE", "INT", "ORD", "CHR", "GETURL", "GETURL1", "NEXTFRAME",
  "PREVFRAME", "PLAY", "STOP", "TOGGLEQUALITY", "STOPSOUNDS", "DUP",
  "SWAP", "POP", "PUSH", "SETREGISTER", "CALLFUNCTION", "CALLMETHOD",
  "NEWOBJECT", "NEWMETHOD", "AND", "OR", "XOR", "MODULO", "ADD",
  "LESSTHAN", "EQUALS", "INC", "DEC", "TYPEOF", "INSTANCEOF", "ENUMERATE",
  "ENUMERATE2", "INITOBJECT", "INITARRAY", "GETMEMBER", "SETMEMBER",
  "SHIFTLEFT", "SHIFTRIGHT", "SHIFTRIGHT2", "VAREQUALS", "OLDADD",
  "SUBTRACT", "MULTIPLY", "DIVIDE", "OLDEQUALS", "OLDLESSTHAN",
  "LOGICALAND", "LOGICALOR", "NOT", "STRINGEQ", "STRINGLENGTH",
  "SUBSTRING", "GETVARIABLE", "SETVARIABLE", "SETTARGETEXPRESSION",
  "DUPLICATEMOVIECLIP", "REMOVEMOVIECLIP", "STRINGLESSTHAN", "MBLENGTH",
  "MBSUBSTRING", "MBORD", "MBCHR", "BRANCHALWAYS", "BRANCHIFTRUE",
  "GETURL2", "POST", "GET", "CAST", "LOADVARIABLES", "LOADMOVIE",
  "LOADVARIABLESNUM", "LOADMOVIENUM", "CALLFRAME", "STARTDRAG", "STOPDRAG",
  "GOTOANDSTOP", "GOTOANDPLAY", "SETTARGET", "GETPROPERTY", "SETPROPERTY",
  "TONUMBER", "TOSTRING", "TRY", "THROW", "CATCH", "FINALLY", "THIS",
  "EXTENDS", "IMPLEMENTS", "FSCOMMAND2", "CLASS", "PUBLIC", "PRIVATE",
  "NULLVAL", "UNDEFINED", "INTEGER", "DOUBLE", "BOOLEAN", "REGISTER",
  "BROKENSTRING", "STRING", "IDENTIFIER", "\"==\"", "\"===\"", "\"!==\"",
  "\"<=\"", "\">=\"", "\"!=\"", "\"&&\"", "\"||\"", "\"++\"", "\"--\"",
  "\"+=\"", "\"/=\"", "\"*=\"", "\"-=\"", "\"%=\"", "\"&=\"", "\"|=\"",
  "\"<<\"", "\">>\"", "\">>>\"", "\"<<=\"", "\">>=\"", "\">>>=\"", "_P_X",
  "_P_Y", "_P_XSCALE", "_P_YSCALE", "_P_CURRENTFRAME", "_P_TOTALFRAMES",
  "_P_ALPHA", "_P_VISIBLE", "_P_WIDTH", "_P_HEIGHT", "_P_ROTATION",
  "_P_TARGET", "_P_FRAMESLOADED", "_P_NAME", "_P_DROPTARGET", "_P_URL",
  "_P_HIGHQUALITY", "_P_FOCUSRECT", "_P_SOUNDBUFTIME", "_P_QUALITY",
  "_P_XMOUSE", "_P_YMOUSE", "NOELSE", "','", "'='", "\"^=\"", "'?'", "':'",
  "'<'", "'>'", "'&'", "'|'", "'^'", "'+'", "'-'", "'*'", "'/'", "'%'",
  "'!'", "'~'", "UMINUS", "POSTFIX", "'.'", "'['", "']'", "'{'", "'}'",
  "';'", "'('", "')'", "$accept", "program", "$@1", "code", "anycode",
  "stmts", "emptybraces", "stmt", "class_stmts", "class_stmt",
  "class_init", "class_body", "class_decl", "access_attr", "type_attr",
  "class_vars", "class_var", "throw_stmt", "try_catch_stmt", "with_stmt",
  "return_stmt", "assign_stmts", "if_stmt", "expr_opt", "switch_init",
  "switch_stmt", "switch_cases", "switch_case", "identifier",
  "formals_list", "function_identifier", "function_decl", "inpart",
  "obj_ref", "obj_ref_for_delete_only", "while_init", "do_init",
  "for_init", "for_in_init", "iter_stmt", "assign_stmts_opt", "cont_stmt",
  "break_stmt", "urlmethod", "level", "void_function_call",
  "function_call", "delete_call", "expr_list", "method_call", "objexpr",
  "objexpr_list", "assignop", "incdecop", "lvalue_expr", "lvalue", "expr",
  "expr_or_obj", "primary", "primary_constant", "init_vars", "init_var",
  "assign_stmt", "$@2", "opcode_list", "with", "@3", "push_item",
  "push_list", "opcode", "@4", "property", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,   363,   364,
     365,   366,   367,   368,   369,   370,   371,   372,   373,   374,
     375,   376,   377,   378,   379,   380,   381,   382,   383,   384,
     385,   386,   387,   388,   389,   390,   391,   392,   393,   394,
     395,   396,   397,   398,   399,   400,   401,   402,   403,   404,
     405,   406,   407,   408,   409,   410,   411,   412,   413,   414,
     415,   416,   417,   418,   419,   420,   421,   422,   423,   424,
     425,   426,   427,   428,   429,   430,   431,   432,   433,   434,
     435,    44,    61,   436,    63,    58,    60,    62,    38,   124,
      94,    43,    45,    42,    47,    37,    33,   126,   437,   438,
      46,    91,    93,   123,   125,    59,    40,    41
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint16 yyr1[] =
{
       0,   208,   210,   209,   209,   211,   211,   212,   212,   212,
     213,   213,   214,   215,   215,   215,   215,   215,   215,   215,
     215,   215,   215,   215,   215,   215,   216,   216,   217,   217,
     218,   219,   219,   220,   220,   221,   221,   221,   222,   222,
     223,   223,   224,   224,   225,   226,   226,   226,   226,   227,
     228,   228,   229,   229,   230,   230,   231,   231,   232,   233,
     234,   234,   235,   235,   236,   236,   236,   236,   236,   236,
     236,   236,   236,   236,   236,   236,   236,   236,   236,   236,
     236,   236,   236,   236,   236,   236,   236,   236,   236,   236,
     236,   236,   236,   236,   236,   236,   236,   236,   236,   236,
     236,   236,   236,   236,   236,   236,   236,   236,   236,   236,
     236,   236,   236,   236,   236,   236,   236,   236,   236,   236,
     236,   236,   236,   236,   236,   236,   236,   236,   236,   236,
     236,   236,   236,   236,   236,   236,   236,   236,   236,   236,
     236,   236,   236,   236,   236,   236,   236,   236,   236,   236,
     236,   236,   236,   236,   236,   236,   236,   236,   236,   236,
     236,   236,   236,   236,   236,   236,   236,   236,   236,   236,
     236,   236,   236,   236,   237,   237,   237,   238,   238,   239,
     240,   241,   241,   241,   242,   242,   242,   242,   242,   243,
     244,   245,   246,   247,   247,   247,   247,   247,   248,   248,
     249,   250,   251,   251,   251,   251,   252,   253,   253,   253,
     253,   253,   253,   253,   253,   253,   253,   253,   253,   253,
     253,   253,   253,   253,   253,   253,   253,   253,   253,   253,
     253,   253,   253,   253,   253,   253,   254,   254,   254,   254,
     254,   254,   254,   254,   254,   254,   254,   254,   254,   255,
     256,   256,   256,   257,   257,   258,   259,   259,   260,   260,
     260,   260,   260,   260,   260,   260,   260,   260,   260,   261,
     261,   262,   262,   262,   263,   263,   263,   264,   264,   264,
     264,   264,   264,   264,   264,   264,   264,   264,   264,   264,
     264,   264,   264,   264,   264,   264,   264,   264,   264,   264,
     264,   264,   264,   264,   264,   264,   265,   265,   265,   265,
     265,   265,   265,   265,   265,   265,   266,   266,   266,   266,
     266,   266,   266,   267,   267,   267,   267,   267,   267,   268,
     268,   269,   269,   271,   270,   270,   270,   270,   270,   270,
     270,   270,   270,   270,   272,   272,   274,   273,   275,   275,
     275,   275,   275,   275,   275,   276,   276,   278,   277,   277,
     277,   277,   277,   277,   277,   277,   277,   277,   277,   277,
     277,   277,   277,   277,   277,   277,   277,   277,   277,   277,
     277,   277,   277,   277,   277,   277,   277,   277,   277,   277,
     277,   277,   277,   277,   277,   277,   277,   277,   277,   277,
     277,   277,   277,   277,   277,   277,   277,   277,   277,   277,
     277,   277,   277,   277,   277,   277,   277,   277,   277,   277,
     277,   277,   277,   277,   277,   277,   277,   277,   277,   277,
     277,   277,   277,   277,   279,   279,   279,   279,   279,   279,
     279,   279,   279,   279,   279,   279,   279,   279,   279,   279,
     279,   279,   279,   279,   279,   279,   279
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     2,     0,     1,     2,     1,     1,     1,
       1,     2,     2,     1,     3,     1,     2,     1,     1,     1,
       1,     1,     1,     1,     2,     1,     1,     2,     2,     4,
       1,     1,     3,     3,     5,     0,     1,     1,     0,     2,
       1,     3,     4,     2,     3,     2,     7,     4,     9,     7,
       2,     3,     1,     3,     7,     5,     0,     1,     1,     7,
       0,     2,     4,     3,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     0,     2,     4,     0,     1,     7,
       2,     1,     1,     1,     1,     1,     1,     1,     3,     1,
       1,     0,     0,     5,     6,    10,     7,     8,     0,     1,
       2,     2,     0,     2,     2,     2,     1,     4,     4,     7,
       7,     7,     7,     7,     4,     6,    14,     3,     8,     4,
       6,     3,     3,     3,     3,     3,     3,     4,     4,     4,
       4,     4,     4,     4,     4,     8,     4,     4,     4,     3,
       4,     4,     4,     4,     4,     6,     8,     4,     6,     2,
       0,     1,     3,     6,     7,     3,     1,     3,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     3,     4,     1,     1,     2,
       2,     2,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     5,     3,     3,     1,     2,     5,     4,
       5,     7,     8,     3,     1,     3,     1,     1,     1,     2,
       2,     3,     3,     1,     1,     1,     1,     1,     1,     1,
       3,     4,     2,     0,     5,     2,     1,     1,     1,     1,
       2,     2,     3,     3,     1,     2,     0,     4,     1,     1,
       1,     1,     1,     1,     1,     1,     3,     0,     3,     1,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     2,     2,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1
};

/* YYDEFACT[STATE-NAME] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       2,     0,     0,     1,     0,     0,   177,    58,     0,     0,
     189,   190,     0,     0,    65,     0,     0,     0,     0,   150,
      66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
      96,    97,    98,    99,   101,   100,   102,   103,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   129,   130,   131,   132,   133,   134,   135,   136,   137,
     138,   149,   139,   141,   140,   142,   146,   127,   128,   143,
     144,   145,   147,   148,     0,     0,   151,    30,    64,   269,
     270,   152,   153,   154,   155,   156,   157,   158,   159,   160,
     161,   162,   163,   164,   165,   166,   167,   168,   169,   170,
     171,   172,   173,     0,    15,     3,     5,    13,     7,     0,
       9,    25,     0,    23,    22,    17,     0,    21,   274,     8,
       0,     0,    18,    19,    20,   336,   338,   337,   339,     0,
       0,   271,     0,   201,   200,    65,    66,    67,    68,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    99,   125,   126,   139,   141,   140,   142,
     146,   127,   128,   143,   144,   145,   147,   148,    64,   178,
       0,   198,     0,    38,   335,   329,     0,   249,   186,   185,
     187,   184,     0,     0,   324,   325,   327,   328,   323,   326,
       0,     0,     0,   250,     0,    50,     0,   314,   316,   272,
     318,   273,     0,   317,   271,   306,     0,   277,   278,     0,
     333,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    45,     0,   250,    12,     0,    10,
       6,     0,    24,     0,     0,     0,   340,     0,     0,   258,
     261,   260,   259,   262,   263,   264,   266,   267,   268,     0,
     265,     0,   341,    16,   174,     0,   199,   274,     0,    52,
       0,     0,   332,     0,     0,     0,    64,   307,     0,   271,
     279,   281,   280,     0,   251,     0,   256,     0,     0,   319,
       0,     0,   320,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    51,     0,     0,     0,
       0,   239,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   221,   222,   223,   224,   226,   225,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   217,   327,   326,     0,
     327,   326,     0,   326,     0,     0,     0,     0,     0,    44,
       0,    14,    11,     0,    35,    31,    33,     0,     0,     0,
     275,     0,   342,   343,    38,     0,    38,     0,     0,     0,
      56,     0,    39,     0,   330,   188,   237,   250,     0,     0,
       0,   313,     0,     0,   315,   321,   304,   322,   305,   296,
     297,   299,   294,   295,   298,   282,   283,   300,   301,   302,
       0,   292,   293,   289,   290,   291,   287,   288,   284,   285,
     286,     0,   389,   381,   379,   380,   391,   362,   346,     0,
     424,   428,   419,   422,   408,   426,   427,   409,   410,   411,
     357,     0,   361,   363,   364,   365,   366,   367,   368,   369,
     370,   371,   372,   373,   374,   375,   376,   377,   378,   383,
     382,   384,   385,   386,   387,   388,   396,   397,   398,   399,
     400,   401,   402,   403,   404,   405,   406,   407,   412,   413,
     418,   420,   421,   423,   425,   429,   430,   431,     0,     0,
     395,   414,   415,   416,   417,   390,   393,   394,     0,   359,
     344,   238,   240,   241,     0,     0,   207,   242,   243,   244,
       0,   208,     0,   247,     0,   219,     0,     0,     0,     0,
     214,     0,   228,   230,   232,   227,   229,   231,   233,   234,
       0,     0,     0,    47,   236,     0,    36,    37,    35,    26,
       0,     0,     0,     0,   250,   276,   175,     0,    38,     0,
      53,   180,   272,   273,   271,   192,     0,    57,    55,   331,
       0,   309,     0,   252,   255,   257,     0,     0,     0,   392,
       0,   360,   432,   433,   334,   345,     0,     0,   202,     0,
       0,   202,   202,   202,   206,   202,     0,   434,   435,   436,
     437,   438,   439,   440,   441,   442,   443,   444,   445,   446,
     447,   448,   449,   450,   451,   452,   453,   454,   455,   456,
       0,     0,     0,    34,    32,    27,     0,    28,    60,   193,
       0,     0,   250,    38,     0,   192,     0,   198,     0,   308,
     250,   310,   303,     0,     0,   352,   353,   349,   350,   351,
     354,   348,   355,   358,   245,     0,     0,     0,   220,     0,
       0,     0,     0,     0,     0,   215,   248,     0,     0,     0,
      40,    38,     0,   194,   253,     0,   176,   179,     0,   196,
       0,    54,     0,   250,    49,   347,     0,     0,   204,   203,
     205,   209,     0,   210,   212,   211,   213,     0,     0,    46,
       0,    29,    43,     0,     0,    59,    61,   254,   197,   191,
     311,     0,   356,   246,   218,     0,   235,     0,    41,     0,
       0,     0,     0,   312,     0,    48,    42,     0,    63,   195,
       0,    62,     0,     0,     0,   216
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,   135,   136,   278,   137,   279,   578,   579,
     139,   406,   140,   580,   312,   699,   700,   141,   142,   143,
     144,   306,   145,   596,   146,   147,   702,   736,   148,   415,
     200,   228,   419,   591,   207,   150,   151,   752,   666,   152,
     308,   153,   154,   687,   623,   155,   229,   230,   323,   231,
     326,   327,   301,   232,   233,   234,   235,   324,   237,   238,
     204,   205,   162,   358,   538,   539,   608,   682,   683,   540,
     610,   650
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -598
static const yytype_int16 yypact[] =
{
      31,    34,  1468,  -598,  -169,  -167,  4556,  -598,  -161,  -130,
    -598,  -598,  4556,  3544,  -105,  2446,  -103,  -156,   -93,  -598,
     -83,   -71,   -48,   -39,   -38,   -37,   -35,   -33,   -32,   -31,
     -28,   -27,   -23,   -22,   -20,   -14,   -13,  -598,  -598,  -598,
    -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,
    -598,  -598,  -598,   -12,  -598,  -598,  -598,  -598,  -598,  -598,
    -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,
    -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,   -10,
       3,  -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,
    -598,  -598,     9,    13,    14,    16,    17,    19,    20,    23,
      25,    27,    28,    29,  2264,  2629,  -598,  -598,    32,  -598,
    -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,
    -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,
    -598,  -598,  -598,  1655,  -598,  1468,  -598,  -598,  -598,  4556,
    -598,  -598,  -109,  -598,  -598,  -598,    35,  -598,  -598,  -598,
      36,  2264,  -598,  -598,  -598,  -598,  -160,  -598,  -114,  4234,
    -102,   -94,   -60,  -598,  -598,  -598,  -598,  -598,  -598,  -598,
    -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,
    -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,
    -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,
      38,  3908,  2812,    18,    21,  -598,  3544,  -598,  -598,  -160,
    -114,   -61,    42,  4395,  -598,  -598,  -598,  -598,  -598,  -598,
    2812,  2812,  2812,  2629,  3726,  -598,  2812,  -598,  -598,  -598,
    -598,  -598,  4234,  -102,   -78,  6276,   -52,  -598,  -598,  2812,
    -598,  2812,  2812,    30,  2629,  2812,  2812,  2629,  2812,  2812,
    2812,  2812,    68,    43,    44,    45,    46,    47,    48,  2629,
    2812,  2812,  2812,  2812,  2812,  2812,  2812,  2812,    49,  2995,
    3178,  3361,  2812,  2812,   -25,     0,  2629,  -598,  1858,  -598,
    -598,  -108,  -598,  2812,  2812,   203,   -61,  4556,  2812,  -598,
    -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,  2629,
    -598,  2812,  -598,  -598,  4556,  4556,    58,   217,    53,  -598,
    1143,  4556,    77,  4556,    55,    57,    60,   -58,   -53,  -598,
     178,   178,   178,  -158,  -598,    75,  -598,  -107,  1167,   -61,
    2629,  2812,  -598,  4234,  2812,  2812,  2812,  2812,  2812,  2812,
    2812,  2812,  2812,  2812,  2812,  2812,  2812,  2812,  2812,  2812,
    2812,  2812,  2812,  2812,  2812,  2812,  -598,  1228,  6626,  1250,
    4768,  -598,    63,  5547,  5608,    64,  4790,  4851,  4873,   435,
      80,  -598,  -598,  -598,  -598,  -598,  -598,    65,  5628,  4934,
    5689,  5709,  5770,  5790,  4956,  5851,  -598,    66,    71,  5017,
      72,    74,  5039,    78,  5100,  5871,  5932,    69,  2264,  -598,
    -165,  -598,  -598,  4556,  -110,  -598,  -598,  5122,  5183,    82,
      84,  5454,  -598,  6276,    18,  -164,    -5,  4073,  4234,    79,
    2812,  2264,  -598,  2629,  -598,  -598,  -598,  2629,  4717,  2812,
    2629,  -598,  2629,  4556,  -598,  -598,  -598,  6276,  -102,   283,
     283,   283,   297,   297,   283,  6337,  6337,    -4,    -4,    -4,
    5952,   297,   297,    54,    54,    54,   -30,   -30,   178,   178,
     178,    88,  -598,  -598,  -598,  -598,  -598,  -598,  -598,   141,
    -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,
    -598,   151,  -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,
    -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,
    -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,
    -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,
    -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,   161,   163,
    -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,  1106,  -598,
    -598,  -598,  -598,  -598,  2812,  2812,  -598,  -598,  -598,  -598,
    2812,  -598,   164,  -598,  2812,  -598,  2812,  2812,  2812,  2812,
    -598,  2812,  -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,
     736,   736,  4556,  -598,  -598,    95,  -598,  -598,  -104,  -598,
      22,    97,  2264,  2812,  2629,   100,  -598,  4556,    18,    96,
    -598,  -598,   104,   105,   106,  -598,   102,  6276,   308,  -598,
    -148,  -171,  5525,  -598,  -598,  -598,  2812,  2264,  6626,  -598,
      -9,  -598,  -598,  -598,  -598,  -598,  5205,  6013,  6033,   108,
    6094,  6033,  6033,   136,  6276,   136,   891,  -598,  -598,  -598,
    -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,
    -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,
     112,   143,   115,  -598,  -598,  -598,  4556,  -598,  -598,  -598,
    5266,  -101,  2629,    18,  2264,  -598,  2264,  4073,  2264,   -50,
    2629,  -116,  6276,  2061,  6518,  -598,  -598,  -598,  -598,  -598,
    -598,  -598,  -598,   144,  -598,  2812,   -73,   119,  -598,  2812,
     120,   121,   122,   123,  2812,  -598,  -598,  2812,  2264,  -159,
    -598,    18,    10,  -598,  -598,  -100,  -598,  -598,  2264,  -598,
     129,  -598,   -99,  2629,  -598,  -598,    -9,  5288,  -598,  -598,
    -598,  -598,  5349,  -598,  -598,  -598,  -598,  6114,  5371,   219,
    4556,  -598,   156,  2812,   154,  -598,  -598,  -598,  -598,  -598,
    -598,   -98,  -598,  -598,  -598,  2812,  -598,  2264,  -598,    -1,
    6175,  2264,  2264,   -45,  6195,  -598,  -598,  2264,  2264,  -598,
    2812,  2264,  6256,  2812,  5432,  -598
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -598,  -598,  -598,  -598,   206,  -597,    76,   170,  -598,  -236,
    -598,  -232,  -598,  -598,  -406,  -598,  -384,  -598,  -598,  -598,
    -598,  -598,  -598,  -598,  -598,  -598,  -598,  -598,    -3,  -598,
    -598,     2,   -69,  -598,   142,  -598,  -598,  -598,  -316,  -598,
    -317,  -598,  -598,  -511,  -208,    -6,    26,   183,  -275,    62,
     -80,  -598,   124,   132,    -2,    11,   716,    33,  -598,  -395,
    -598,    51,  -199,  -598,  -253,  -598,  -598,  -360,  -598,  -533,
    -598,  -211
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -277
static const yytype_int16 yytable[] =
{
     160,   400,   309,   199,   149,   615,   418,   208,   586,   203,
     673,   160,   403,   161,   576,   577,   430,   587,   733,   734,
     576,   577,   730,   430,   211,   718,   719,     6,   156,   -64,
     -64,    -4,   333,   430,     3,   670,   163,   656,   164,   209,
    -272,  -272,   574,   588,   431,   201,   731,   240,   236,   109,
     110,   289,   290,   291,   292,   293,   294,   295,   333,   669,
     720,   296,   297,   298,   158,   109,   110,   289,   290,   291,
     292,   293,   294,   295,   433,   210,   202,   296,   297,   298,
     430,   430,   430,   430,  -276,  -276,  -273,  -273,   299,   300,
     713,   227,   397,   398,   277,   404,   282,   434,   287,   288,
     654,   212,   160,   239,   330,   300,   704,   737,   740,   753,
     690,   691,   692,   241,   693,   161,   333,   675,   676,   677,
     678,   679,   680,   242,   681,   214,   215,   216,   217,   218,
     156,   160,   219,   160,   159,   243,   281,   149,   275,  -271,
    -271,   615,  -274,  -274,   161,   303,   161,   428,   429,   160,
    -236,  -236,   600,   356,   758,  -254,  -254,   160,   244,   156,
     761,   156,   161,   353,   354,   355,   158,   245,   246,   247,
     286,   248,   138,   249,   250,   251,   315,   156,   252,   253,
     311,   227,   664,   254,   255,   157,   256,   351,   352,   353,
     354,   355,   257,   258,   259,   158,   260,   158,   307,   160,
     208,   370,   313,   311,   160,   399,   342,   343,   344,   261,
     317,   318,   161,   158,   735,   262,   409,   211,   590,   263,
     264,   325,   265,   266,   319,   267,   268,   156,   418,   269,
     160,   270,   209,   271,   272,   273,   159,   361,   276,   417,
     333,   283,   284,   329,   304,   351,   352,   353,   354,   355,
     371,   372,   373,   374,   375,   376,   386,   706,   420,   423,
     432,   552,   425,   158,   426,   159,   427,   159,   210,   609,
     543,   546,   553,   562,   274,   572,   160,   362,   563,   565,
     365,   566,   611,   159,   410,   568,   595,   157,   583,   161,
     584,   607,   377,   302,   612,   732,   613,   619,   404,   227,
     658,   414,   416,   665,   156,   138,   662,   667,   422,   661,
     203,  -182,  -183,  -181,   668,   688,   157,   686,   157,   696,
     227,   285,   698,   227,   697,   716,   721,   723,   724,   725,
     726,   438,   412,   159,   157,   227,   739,   747,   749,   751,
     158,   280,   655,   653,   319,   333,   748,   589,   314,   708,
     710,   625,   227,   605,   756,   674,   742,   405,   331,   333,
     651,     0,     0,   436,   424,     0,   332,     0,     0,     0,
       0,     0,     0,     0,     0,   227,     0,     0,     0,     0,
       0,     0,     0,     0,   157,     0,     0,   705,     0,     0,
       0,     0,     0,     0,     0,   712,   160,     0,     0,     0,
     575,     0,     0,     0,     0,     0,   227,     0,     0,   161,
     159,     0,     0,     0,     0,   160,   160,     0,     0,   160,
       0,   337,   338,     0,   156,   410,     0,     0,   161,   594,
     325,     0,   161,     0,     0,   342,   343,   344,   741,     0,
       0,     0,     0,   156,   592,     0,     0,   156,   402,   342,
     343,   344,     0,     0,     0,     0,   599,     0,     0,     0,
     158,   157,     0,   603,     0,   604,     0,     0,   309,   346,
     347,   348,   349,   350,   351,   352,   353,   354,   355,   158,
     593,     0,     0,   158,     0,   348,   349,   350,   351,   352,
     353,   354,   355,     0,     0,     0,     0,   333,     0,   227,
       0,     0,     0,   227,     0,     0,   227,     0,   227,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     159,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   159,
       0,     0,     0,   159,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   573,   652,
     334,   335,   336,   337,   338,   339,   340,   341,     0,     0,
     160,   157,   657,     0,   663,     0,     0,   342,   343,   344,
       0,   598,     0,   161,     0,     0,     0,     0,     0,     0,
     157,     0,     0,     0,   157,   160,     0,     0,   156,     0,
       0,     0,     0,     0,     0,     0,   550,     0,   161,   345,
       0,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,     0,     0,   156,     0,     0,     0,     0,     0,     0,
       0,     0,   551,     0,   158,     0,     0,     0,     0,     0,
       0,   405,     0,   701,     0,     0,     0,     0,     0,     0,
     227,     0,   160,     0,   160,   160,   160,     0,     0,   158,
       0,   160,     0,     0,     0,   161,     0,   161,   161,   161,
       0,     0,     0,     0,   161,     0,     0,     0,     0,     0,
     156,     0,   156,   156,   156,     0,   160,     0,     0,   156,
       0,     0,     0,     0,     0,     0,   160,     0,     0,   161,
       0,     0,     0,     0,   159,     0,     0,     0,     0,   161,
       0,     0,     0,     0,   156,     0,   158,   701,   158,   158,
     158,     0,     0,     0,   156,   158,     0,     0,   227,   159,
       0,     0,     0,     0,     0,   160,   227,     0,     0,   160,
     160,     0,   659,     0,     0,   160,   160,     0,   161,   160,
     158,     0,   161,   161,     0,   157,     0,     0,   161,   161,
     158,     0,   161,   156,     0,     0,     0,   156,   156,     0,
       0,     0,     0,   156,   156,     0,     0,   156,     0,   227,
     157,     0,     0,     0,     0,     0,   159,     0,   159,   159,
     159,     0,     0,     0,     0,   159,     0,     0,     0,   158,
       0,     0,     0,   158,   158,     0,     0,     0,     0,   158,
     158,     0,     0,   158,     0,     0,     0,     0,     0,     0,
     159,     0,     0,     0,   707,     0,   709,     0,   711,     0,
     159,     0,     0,   402,     0,     0,     0,   157,     0,   157,
     157,   157,     0,     0,     0,     0,   157,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   729,   627,
       0,     0,     0,     0,     0,     0,     0,     0,   738,   159,
       0,   157,     0,   159,   159,     0,     0,     0,     0,   159,
     159,   157,     0,   159,   628,   629,   630,   631,   632,   633,
     634,   635,   636,   637,   638,   639,   640,   641,   642,   643,
     644,   645,   646,   647,   648,   649,     0,   755,   310,     0,
       0,     0,   759,     0,     0,     0,     0,     0,   402,     0,
     157,   402,     0,     0,   157,   157,   320,   321,   322,     0,
     157,   157,   328,     0,   157,     0,     0,     0,     0,     0,
       0,     0,     0,   333,     0,   357,     0,   359,   360,     0,
       0,   363,   364,     0,   366,   367,   368,   369,     0,     0,
       0,     0,     0,     0,     0,     0,   378,   379,   380,   381,
     382,   383,   384,   385,     0,   389,   392,   394,   395,   396,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   407,
     408,     0,     0,     0,   411,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   413,     0,     0,
       0,     0,     0,     0,     0,     0,   334,   335,   336,   337,
     338,   339,   340,   341,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   342,   343,   344,     0,   437,     0,     0,
     439,   440,   441,   442,   443,   444,   445,   446,   447,   448,
     449,   450,   451,   452,   453,   454,   455,   456,   457,   458,
     459,   460,   694,     0,     0,   345,     0,   346,   347,   348,
     349,   350,   351,   352,   353,   354,   355,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   695,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   462,   463,   464,   465,   466,   467,     0,   468,     0,
       0,   469,   470,   471,     0,   472,   597,   473,   474,   475,
     476,     0,     0,     0,     0,   602,     0,     0,     0,   477,
     478,   479,   480,   481,   482,   483,   484,   485,   486,   487,
     488,   489,   490,   491,   492,   493,   494,   495,   496,   497,
     498,   499,   500,   501,   502,   503,   504,   505,     0,   506,
     507,   508,   509,   510,   511,   512,   513,   514,   515,   516,
     517,   518,   519,   520,   521,   522,   523,   524,   525,   526,
     527,   528,   529,     0,     0,   333,   530,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   531,   532,   533,
     534,     0,     0,     0,     0,     0,   535,   536,   537,   333,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     616,   617,     0,     0,     0,     0,   618,     0,     0,     0,
     620,     0,   621,   622,   624,   624,     0,   626,   334,   335,
     336,   337,   338,   339,   340,   341,     0,     0,     0,     0,
     333,     0,     0,     0,     0,   342,   343,   344,     0,   660,
       0,     0,   334,   335,   336,   337,   338,   339,   340,   341,
     614,     0,   333,     0,     0,     0,     0,     0,     0,   342,
     343,   344,   672,     0,     0,     0,     0,   345,     0,   346,
     347,   348,   349,   350,   351,   352,   353,   354,   355,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     421,   345,     0,   346,   347,   348,   349,   350,   351,   352,
     353,   354,   355,   334,   335,   336,   337,   338,   339,   340,
     341,     0,     0,     0,   435,     0,     0,     0,     0,     0,
     342,   343,   344,     0,     0,   334,   335,   336,   337,   338,
     339,   340,   341,     0,     0,     0,     0,     0,     0,     0,
       0,   717,   342,   343,   344,   722,     0,     0,     0,     0,
     727,     0,   345,   728,   346,   347,   348,   349,   350,   351,
     352,   353,   354,   355,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   345,   461,   346,   347,   348,   349,
     350,   351,   352,   353,   354,   355,     0,     0,     0,   750,
       0,     0,     0,     0,     0,     0,     0,   541,     0,     0,
       0,   754,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     4,     5,     6,     0,     7,   762,     0,     8,   764,
       9,    10,    11,    12,     0,    13,     0,    14,    15,     0,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,     0,     0,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
       0,    54,    55,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    95,    96,    97,    98,    99,   100,   101,   102,
     103,     0,     0,   104,   105,     0,     0,   106,     0,     0,
       0,   107,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   108,     0,     0,     0,     0,     0,     0,     0,
       0,   109,   110,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,   132,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     4,     5,
       0,     0,     7,     0,     0,     8,     0,     9,    10,    11,
      12,   133,    13,   134,    14,    15,     0,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,     0,     0,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,     0,    54,    55,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
      96,    97,    98,    99,   100,   101,   102,   103,     0,     0,
     104,   105,     0,     0,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   108,
       0,     0,     0,     0,     0,     0,     0,     0,   109,   110,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
     128,   129,   130,   131,   132,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   133,   277,
     134,     4,     5,     0,     0,     7,     0,     0,     8,     0,
       9,    10,    11,    12,     0,    13,     0,    14,    15,     0,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,     0,     0,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
       0,    54,    55,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    95,    96,    97,    98,    99,   100,   101,   102,
     103,     0,     0,   104,   105,     0,     0,   106,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   108,     0,     0,     0,     0,     0,     0,     0,
       0,   109,   110,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,   132,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   133,   401,   134,     4,     5,     0,     0,     7,     0,
       0,     8,     0,     9,    10,    11,    12,     0,    13,     0,
      14,    15,     0,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,     0,     0,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,     0,    54,    55,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    98,    99,
     100,   101,   102,   103,     0,     0,   104,   105,     0,     0,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   108,     0,     0,     0,     0,
       0,     0,     0,     0,   109,   110,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   127,   128,   129,   130,   131,
     132,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   133,   714,   134,     4,     5,     0,
       0,     7,     0,     0,     8,     0,     9,    10,    11,    12,
       0,    13,     0,    14,    15,     0,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,     0,     0,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,     0,    54,    55,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,   103,     0,     0,   104,
     105,     0,     0,   106,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   108,     0,
       0,     0,     0,     0,     0,     0,     0,   109,   110,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,   128,
     129,   130,   131,   132,     0,     0,     0,     0,     0,     0,
       0,     6,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   213,    13,     0,    14,     0,   133,     0,   134,
      18,    19,    20,    21,    22,    23,    24,   171,    26,    27,
      28,   175,   176,   177,   178,   179,   180,   181,   182,    37,
      38,    39,    40,    41,    42,    43,     0,     0,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,     0,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   184,   185,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,   186,   187,   188,
     189,   190,   191,   192,   193,   194,   195,   102,   197,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,     0,   214,   215,   216,   217,   218,     0,     0,   219,
     108,     0,     0,     0,     0,     0,     0,     0,     0,   109,
     110,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,   128,   129,   130,   131,   132,     0,     0,     0,     0,
       0,     0,     0,     0,     6,     0,     0,     0,   220,     0,
       0,     0,   221,   222,     0,   213,    13,   223,    14,   224,
       0,   225,   226,    18,    19,    20,    21,    22,    23,    24,
     171,    26,    27,    28,   175,   176,   177,   178,   179,   180,
     181,   182,    37,    38,    39,    40,    41,    42,    43,     0,
       0,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,     0,    54,    55,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   184,   185,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
     186,   187,   188,   189,   190,   191,   192,   193,   194,   195,
     102,   197,     0,     0,     0,     0,     0,     0,   106,     0,
       0,     0,     0,     0,     0,   214,   215,   216,   217,   218,
       0,     0,   219,   108,     0,     0,     0,     0,     0,     0,
       0,     0,   109,   110,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,   128,   129,   130,   131,   132,     0,
       0,     0,     0,     0,     0,     0,     0,     6,     0,     0,
       0,   220,     0,     0,     0,   221,   222,     0,     0,    13,
     223,    14,   224,     0,     0,   226,    18,    19,    20,    21,
      22,    23,    24,   171,    26,    27,    28,   175,   176,   177,
     178,   179,   180,   181,   182,    37,    38,    39,    40,    41,
      42,    43,     0,     0,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,     0,    54,    55,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     184,   185,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    91,   186,   187,   188,   189,   190,   191,   192,
     193,   194,   195,   102,   197,     0,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,     0,   214,   215,
     216,   217,   218,     0,     0,   219,   108,     0,     0,     0,
       0,     0,     0,     0,     0,   109,   110,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,   128,   129,   130,
     131,   132,     0,     0,     0,     0,     0,     0,     0,     0,
       6,     0,     0,     0,   220,     0,     0,     0,   221,   222,
       0,     0,    13,     0,    14,     0,     0,     0,   226,    18,
      19,    20,    21,    22,    23,    24,   171,    26,    27,    28,
     175,   176,   177,   178,   179,   180,   181,   182,    37,    38,
      39,    40,    41,    42,    43,     0,     0,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,     0,    54,    55,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   184,   185,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,   186,   187,   188,   189,
     190,   191,   192,   193,   194,   195,   102,   197,     0,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
       0,   214,   215,   387,   217,   218,     0,     0,   388,   108,
       0,     0,     0,     0,     0,     0,     0,     0,   109,   110,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
     128,   129,   130,   131,   132,     0,     0,     0,     0,     0,
       0,     0,     0,     6,     0,     0,     0,   220,     0,     0,
       0,   221,   222,     0,     0,    13,     0,    14,     0,     0,
       0,   226,    18,    19,    20,    21,    22,    23,    24,   171,
      26,    27,    28,   175,   176,   177,   178,   179,   180,   181,
     182,    37,    38,    39,    40,    41,    42,    43,     0,     0,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
       0,    54,    55,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   184,   185,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,   186,
     187,   188,   189,   190,   191,   192,   193,   194,   195,   102,
     197,     0,     0,     0,     0,     0,     0,   106,     0,     0,
       0,     0,     0,     0,   214,   215,   390,   217,   218,     0,
       0,   391,   108,     0,     0,     0,     0,     0,     0,     0,
       0,   109,   110,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,   132,     0,     0,
       0,     0,     0,     0,     0,     0,     6,     0,     0,     0,
     220,     0,     0,     0,   221,   222,     0,     0,    13,     0,
      14,     0,     0,     0,   226,    18,    19,    20,    21,    22,
      23,    24,   171,    26,    27,    28,   175,   176,   177,   178,
     179,   180,   181,   182,    37,    38,    39,    40,    41,    42,
      43,     0,     0,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,     0,    54,    55,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   184,
     185,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,   186,   187,   188,   189,   190,   191,   192,   193,
     194,   195,   102,   197,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,     0,   214,   215,   216,
     217,   218,     0,     0,   393,   108,     0,     0,     0,     0,
       0,     0,     0,     0,   109,   110,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   127,   128,   129,   130,   131,
     132,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   220,     0,     0,     0,   221,   222,     0,
       0,     0,     0,    14,     0,     0,     0,   226,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,     0,     0,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,     0,    54,    55,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,   103,     0,     0,     0,
       0,     0,     0,   106,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   108,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,   128,
     129,   130,   131,   132,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   165,     0,     0,     0,     0,
     206,    19,   166,   167,   168,   169,   170,   171,   172,   173,
     174,   175,   176,   177,   178,   179,   180,   181,   182,    37,
      38,    39,    40,    41,    42,    43,     0,     0,    44,    45,
      46,    47,    48,    49,    50,    51,    52,   183,     0,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   184,   185,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,   186,   187,   188,
     189,   190,   191,   192,   193,   194,   195,   196,   197,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     198,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,   128,   129,   130,   131,   132,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   305,     0,    13,     0,    14,     0,     0,
     277,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,     0,     0,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
       0,    54,    55,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    95,    96,    97,    98,    99,   100,   101,   102,
     103,     0,     0,     0,     0,     0,     0,   106,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   108,     0,     0,     0,     0,     0,     0,     0,
       0,   109,   110,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,   132,    12,     0,
      13,     0,    14,     0,     0,     0,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,     0,     0,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,     0,    54,    55,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    92,    93,    94,    95,    96,    97,
      98,    99,   100,   101,   102,   103,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   108,     0,     0,
       0,     0,     0,     0,     0,     0,   109,   110,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,   127,   128,   129,
     130,   131,   132,    14,     0,     0,     0,     0,    18,    19,
      20,    21,    22,    23,    24,   171,    26,    27,    28,   175,
     176,   177,   178,   179,   180,   181,   182,    37,    38,    39,
      40,    41,    42,    43,     0,     0,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,     0,    54,    55,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   184,   185,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,   186,   187,   188,   189,   190,
     191,   192,   193,   194,   195,   102,   197,     0,     0,     0,
       0,     0,     0,   106,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   108,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,   128,
     129,   130,   131,   132,    14,     0,     0,     0,     0,    18,
      19,    20,    21,    22,    23,    24,   171,    26,    27,    28,
     175,   176,   177,   178,   179,   180,   181,   182,    37,    38,
      39,    40,    41,    42,    43,     0,     0,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,     0,    54,    55,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   184,   185,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,   186,   187,   188,   189,
     190,   191,   192,   193,   194,   195,   102,   197,     0,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   316,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   165,     0,     0,     0,     0,
       0,    19,   166,   167,   168,   169,   170,   171,   172,   173,
     174,   175,   176,   177,   178,   179,   180,   181,   182,    37,
      38,    39,    40,    41,    42,    43,     0,     0,    44,    45,
      46,    47,    48,    49,    50,    51,    52,   183,     0,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   184,   185,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,   186,   187,   188,
     189,   190,   191,   192,   193,   194,   195,   196,   197,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     198,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,   128,   129,   130,   131,   132,   165,     0,     0,     0,
       0,     0,    19,   166,   167,   168,   169,   170,   171,   172,
     173,   174,   175,   176,   177,   178,   179,   180,   181,   182,
      37,    38,    39,    40,    41,    42,    43,     0,     0,    44,
      45,    46,    47,    48,    49,    50,    51,    52,   183,     0,
      54,    55,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,   184,   185,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,   186,   187,
     188,   189,   190,   191,   192,   193,   194,   195,   196,   197,
     333,     0,     0,     0,     0,     0,   106,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   601,   333,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,   128,   129,   130,   131,   132,     0,     0,     0,
       0,     0,     0,   334,   335,   336,   337,   338,   339,   340,
     341,     0,     0,   333,     0,     0,     0,     0,     0,     0,
     342,   343,   344,     0,     0,   334,   335,   336,   337,   338,
     339,   340,   341,     0,     0,   333,     0,     0,     0,     0,
       0,     0,   342,   343,   344,     0,     0,     0,     0,     0,
       0,     0,   345,     0,   346,   347,   348,   349,   350,   351,
     352,   353,   354,   355,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   345,   542,   346,   347,   348,   349,
     350,   351,   352,   353,   354,   355,   334,   335,   336,   337,
     338,   339,   340,   341,     0,     0,   333,   547,     0,     0,
       0,     0,     0,   342,   343,   344,     0,     0,   334,   335,
     336,   337,   338,   339,   340,   341,     0,     0,   333,     0,
       0,     0,     0,     0,     0,   342,   343,   344,     0,     0,
       0,     0,     0,     0,     0,   345,     0,   346,   347,   348,
     349,   350,   351,   352,   353,   354,   355,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   345,   548,   346,
     347,   348,   349,   350,   351,   352,   353,   354,   355,   334,
     335,   336,   337,   338,   339,   340,   341,     0,     0,   333,
     549,     0,     0,     0,     0,     0,   342,   343,   344,     0,
       0,   334,   335,   336,   337,   338,   339,   340,   341,     0,
       0,   333,     0,     0,     0,     0,     0,     0,   342,   343,
     344,     0,     0,     0,     0,     0,     0,     0,   345,     0,
     346,   347,   348,   349,   350,   351,   352,   353,   354,   355,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     345,   555,   346,   347,   348,   349,   350,   351,   352,   353,
     354,   355,   334,   335,   336,   337,   338,   339,   340,   341,
       0,     0,   333,   560,     0,     0,     0,     0,     0,   342,
     343,   344,     0,     0,   334,   335,   336,   337,   338,   339,
     340,   341,     0,     0,   333,     0,     0,     0,     0,     0,
       0,   342,   343,   344,     0,     0,     0,     0,     0,     0,
       0,   345,     0,   346,   347,   348,   349,   350,   351,   352,
     353,   354,   355,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   345,   564,   346,   347,   348,   349,   350,
     351,   352,   353,   354,   355,   334,   335,   336,   337,   338,
     339,   340,   341,     0,     0,   333,   567,     0,     0,     0,
       0,     0,   342,   343,   344,     0,     0,   334,   335,   336,
     337,   338,   339,   340,   341,     0,     0,   333,     0,     0,
       0,     0,     0,     0,   342,   343,   344,     0,     0,     0,
       0,     0,     0,     0,   345,     0,   346,   347,   348,   349,
     350,   351,   352,   353,   354,   355,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   345,   569,   346,   347,
     348,   349,   350,   351,   352,   353,   354,   355,   334,   335,
     336,   337,   338,   339,   340,   341,     0,     0,   333,   581,
       0,     0,     0,     0,     0,   342,   343,   344,     0,     0,
     334,   335,   336,   337,   338,   339,   340,   341,     0,     0,
     333,     0,     0,     0,     0,     0,     0,   342,   343,   344,
       0,     0,     0,     0,     0,     0,     0,   345,     0,   346,
     347,   348,   349,   350,   351,   352,   353,   354,   355,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   345,
     582,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   334,   335,   336,   337,   338,   339,   340,   341,     0,
       0,   333,   684,     0,     0,     0,     0,     0,   342,   343,
     344,     0,     0,   334,   335,   336,   337,   338,   339,   340,
     341,     0,     0,   333,     0,     0,     0,     0,     0,     0,
     342,   343,   344,     0,     0,     0,     0,     0,     0,     0,
     345,     0,   346,   347,   348,   349,   350,   351,   352,   353,
     354,   355,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   345,   703,   346,   347,   348,   349,   350,   351,
     352,   353,   354,   355,   334,   335,   336,   337,   338,   339,
     340,   341,     0,     0,   333,   743,     0,     0,     0,     0,
       0,   342,   343,   344,     0,     0,   334,   335,   336,   337,
     338,   339,   340,   341,     0,     0,   333,     0,     0,     0,
       0,     0,     0,   342,   343,   344,     0,     0,     0,     0,
       0,     0,     0,   345,     0,   346,   347,   348,   349,   350,
     351,   352,   353,   354,   355,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   345,   744,   346,   347,   348,
     349,   350,   351,   352,   353,   354,   355,   334,   335,   336,
     337,   338,   339,   340,   341,     0,     0,     0,   746,     0,
       0,     0,     0,     0,   342,   343,   344,   333,     0,   334,
     335,   336,   337,   338,   339,   340,   341,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   342,   343,   344,   333,
       0,     0,     0,     0,     0,     0,   345,     0,   346,   347,
     348,   349,   350,   351,   352,   353,   354,   355,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   345,   765,
     346,   347,   348,   349,   350,   351,   352,   353,   354,   355,
       0,     0,     0,     0,     0,     0,   585,     0,     0,     0,
     334,   335,   336,   337,   338,   339,   340,   341,     0,     0,
     333,     0,     0,     0,     0,     0,     0,   342,   343,   344,
       0,     0,   334,   335,   336,   337,   338,   339,   340,   341,
     333,     0,     0,     0,     0,     0,     0,     0,     0,   342,
     343,   344,     0,     0,     0,     0,     0,     0,     0,   345,
       0,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,     0,     0,     0,     0,     0,     0,   671,   544,     0,
       0,   345,     0,   346,   347,   348,   349,   350,   351,   352,
     353,   354,   355,   334,   335,   336,   337,   338,   339,   340,
     341,   333,     0,     0,     0,     0,     0,     0,     0,     0,
     342,   343,   344,   334,   335,   336,   337,   338,   339,   340,
     341,   333,     0,     0,     0,     0,     0,     0,     0,     0,
     342,   343,   344,     0,     0,     0,     0,     0,     0,   545,
       0,     0,   345,     0,   346,   347,   348,   349,   350,   351,
     352,   353,   354,   355,     0,     0,     0,     0,     0,   554,
       0,     0,   345,     0,   346,   347,   348,   349,   350,   351,
     352,   353,   354,   355,   334,   335,   336,   337,   338,   339,
     340,   341,   333,     0,     0,     0,     0,     0,     0,     0,
       0,   342,   343,   344,   334,   335,   336,   337,   338,   339,
     340,   341,   333,     0,     0,     0,     0,     0,     0,     0,
       0,   342,   343,   344,     0,     0,     0,     0,     0,     0,
     556,     0,     0,   345,     0,   346,   347,   348,   349,   350,
     351,   352,   353,   354,   355,     0,     0,     0,     0,     0,
     557,     0,     0,   345,     0,   346,   347,   348,   349,   350,
     351,   352,   353,   354,   355,   334,   335,   336,   337,   338,
     339,   340,   341,   333,     0,     0,     0,     0,     0,     0,
       0,     0,   342,   343,   344,   334,   335,   336,   337,   338,
     339,   340,   341,   333,     0,     0,     0,     0,     0,     0,
       0,     0,   342,   343,   344,     0,     0,     0,     0,     0,
       0,   558,     0,     0,   345,     0,   346,   347,   348,   349,
     350,   351,   352,   353,   354,   355,     0,     0,     0,     0,
       0,   559,     0,     0,   345,     0,   346,   347,   348,   349,
     350,   351,   352,   353,   354,   355,   334,   335,   336,   337,
     338,   339,   340,   341,   333,     0,     0,     0,     0,     0,
       0,     0,     0,   342,   343,   344,   334,   335,   336,   337,
     338,   339,   340,   341,   333,     0,     0,     0,     0,     0,
       0,     0,     0,   342,   343,   344,     0,     0,     0,     0,
       0,     0,   561,     0,     0,   345,     0,   346,   347,   348,
     349,   350,   351,   352,   353,   354,   355,     0,     0,     0,
       0,     0,   570,     0,     0,   345,     0,   346,   347,   348,
     349,   350,   351,   352,   353,   354,   355,   334,   335,   336,
     337,   338,   339,   340,   341,   333,     0,     0,     0,     0,
       0,     0,     0,     0,   342,   343,   344,   334,   335,   336,
     337,   338,   339,   340,   341,   333,     0,     0,     0,     0,
       0,     0,     0,     0,   342,   343,   344,     0,     0,     0,
       0,     0,     0,   571,     0,     0,   345,     0,   346,   347,
     348,   349,   350,   351,   352,   353,   354,   355,     0,     0,
       0,     0,     0,     0,     0,     0,   345,   606,   346,   347,
     348,   349,   350,   351,   352,   353,   354,   355,   334,   335,
     336,   337,   338,   339,   340,   341,   333,     0,     0,     0,
       0,     0,     0,     0,     0,   342,   343,   344,   334,   335,
     336,   337,   338,   339,   340,   341,   333,     0,     0,     0,
       0,     0,     0,     0,     0,   342,   343,   344,     0,     0,
       0,     0,     0,     0,   685,     0,     0,   345,     0,   346,
     347,   348,   349,   350,   351,   352,   353,   354,   355,     0,
       0,     0,     0,     0,   686,     0,     0,   345,     0,   346,
     347,   348,   349,   350,   351,   352,   353,   354,   355,   334,
     335,   336,   337,   338,   339,   340,   341,   333,     0,     0,
       0,     0,     0,     0,     0,     0,   342,   343,   344,   334,
     335,   336,   337,   338,   339,   340,   341,   333,     0,     0,
       0,     0,     0,     0,     0,     0,   342,   343,   344,     0,
       0,     0,     0,     0,     0,   689,     0,     0,   345,     0,
     346,   347,   348,   349,   350,   351,   352,   353,   354,   355,
       0,     0,     0,     0,     0,   745,     0,     0,   345,     0,
     346,   347,   348,   349,   350,   351,   352,   353,   354,   355,
     334,   335,   336,   337,   338,   339,   340,   341,   333,     0,
       0,     0,     0,     0,     0,     0,     0,   342,   343,   344,
     334,   335,   336,   337,   338,   339,   340,   341,   333,     0,
       0,     0,     0,     0,     0,     0,     0,   342,   343,   344,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   345,
     757,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,     0,     0,     0,     0,     0,   760,     0,     0,   345,
       0,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   334,   335,   336,   337,   338,   339,   340,   341,   333,
       0,     0,     0,     0,     0,     0,     0,     0,   342,   343,
     344,   334,   335,   336,   337,   338,   339,   340,   341,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   342,   343,
     344,     0,     0,     0,     0,     0,     0,   763,     0,     0,
     345,     0,   346,   347,   348,   349,   350,   351,   352,   353,
     354,   355,     0,     0,     0,     0,     0,     0,     0,     0,
     345,     0,   346,   347,   348,   349,   350,   351,   352,   353,
     354,   355,   334,   335,   336,   337,   338,   339,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   342,
     343,   344,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   346,   347,   348,   349,   350,   351,   352,
     353,   354,   355,   462,   463,   464,   465,   466,   467,   715,
     468,     0,     0,   469,   470,   471,     0,   472,     0,   473,
     474,   475,   476,     0,     0,     0,     0,     0,     0,     0,
       0,   477,   478,   479,   480,   481,   482,   483,   484,   485,
     486,   487,   488,   489,   490,   491,   492,   493,   494,   495,
     496,   497,   498,   499,   500,   501,   502,   503,   504,   505,
       0,   506,   507,   508,   509,   510,   511,   512,   513,   514,
     515,   516,   517,   518,   519,   520,   521,   522,   523,   524,
     525,   526,   527,   528,   529,     0,     0,     0,   530,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   531,
     532,   533,   534,     0,     0,     0,     0,     0,   535,   536,
     537,   462,   463,   464,   465,   466,   467,     0,   468,     0,
       0,   469,   470,   471,     0,   472,     0,   473,   474,   475,
     476,     0,     0,     0,     0,     0,     0,     0,     0,   477,
     478,   479,   480,   481,   482,   483,   484,   485,   486,   487,
     488,   489,   490,   491,   492,   493,   494,   495,   496,   497,
     498,   499,   500,   501,   502,   503,   504,   505,     0,   506,
     507,   508,   509,   510,   511,   512,   513,   514,   515,   516,
     517,   518,   519,   520,   521,   522,   523,   524,   525,   526,
     527,   528,   529,     0,     0,     0,   530,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   531,   532,   533,
     534,     0,     0,     0,     0,     0,   535,   536,   537
};

#define yypact_value_is_default(yystate) \
  ((yystate) == (-598))

#define yytable_value_is_error(yytable_value) \
  YYID (0)

static const yytype_int16 yycheck[] =
{
       2,   276,   201,     6,     2,   538,    11,    13,   414,    12,
     607,    13,   120,     2,   124,   125,   181,   181,     8,     9,
     124,   125,   181,   181,    13,    98,    99,     5,     2,   200,
     201,     0,    62,   181,     0,   206,   205,    15,   205,    13,
     200,   201,   207,   207,   202,   206,   205,   203,    15,   143,
     144,   145,   146,   147,   148,   149,   150,   151,    62,   207,
     133,   155,   156,   157,     2,   143,   144,   145,   146,   147,
     148,   149,   150,   151,   181,    13,   206,   155,   156,   157,
     181,   181,   181,   181,   200,   201,   200,   201,   182,   183,
     206,    15,   117,   118,   204,   203,   205,   204,   200,   201,
     204,   206,   104,   206,   182,   183,   207,   207,   207,   207,
     621,   622,   623,   206,   625,   104,    62,   126,   127,   128,
     129,   130,   131,   206,   133,   126,   127,   128,   129,   130,
     104,   133,   133,   135,     2,   206,   139,   135,   105,   200,
     201,   674,   200,   201,   133,   205,   135,   200,   201,   151,
     200,   201,   427,   205,   751,   200,   201,   159,   206,   133,
     757,   135,   151,   193,   194,   195,   104,   206,   206,   206,
     159,   206,     2,   206,   206,   206,   134,   151,   206,   206,
     185,   105,   588,   206,   206,     2,   206,   191,   192,   193,
     194,   195,   206,   206,   206,   133,   206,   135,   201,   201,
     206,   133,   181,   185,   206,   205,   152,   153,   154,   206,
     213,   213,   201,   151,   204,   206,    13,   206,   417,   206,
     206,   224,   206,   206,   213,   206,   206,   201,    11,   206,
     232,   206,   206,   206,   206,   206,   104,   207,   206,   181,
      62,   206,   206,   232,   206,   191,   192,   193,   194,   195,
     207,   207,   207,   207,   207,   207,   207,   663,   205,   182,
     185,   181,   207,   201,   207,   133,   206,   135,   206,   128,
     207,   207,   207,   207,   104,   206,   278,   244,   207,   207,
     247,   207,   131,   151,   287,   207,   207,   104,   206,   278,
     206,   203,   259,   161,   133,   701,   133,   133,   203,   223,
     203,   304,   305,   207,   278,   135,   206,   205,   311,   584,
     313,   207,   207,   207,     6,   207,   133,   181,   135,   207,
     244,   151,   207,   247,   181,   181,   207,   207,   207,   207,
     207,   333,   299,   201,   151,   259,   207,   118,   182,   185,
     278,   135,   578,   575,   333,    62,   730,   416,   206,   665,
     667,   559,   276,   433,   749,   608,   716,   281,   234,    62,
     571,    -1,    -1,   330,   313,    -1,   234,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   299,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   201,    -1,    -1,   662,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   670,   398,    -1,    -1,    -1,
     403,    -1,    -1,    -1,    -1,    -1,   330,    -1,    -1,   398,
     278,    -1,    -1,    -1,    -1,   417,   418,    -1,    -1,   421,
      -1,   138,   139,    -1,   398,   428,    -1,    -1,   417,   418,
     433,    -1,   421,    -1,    -1,   152,   153,   154,   713,    -1,
      -1,    -1,    -1,   417,   418,    -1,    -1,   421,   278,   152,
     153,   154,    -1,    -1,    -1,    -1,   423,    -1,    -1,    -1,
     398,   278,    -1,   430,    -1,   432,    -1,    -1,   667,   186,
     187,   188,   189,   190,   191,   192,   193,   194,   195,   417,
     418,    -1,    -1,   421,    -1,   188,   189,   190,   191,   192,
     193,   194,   195,    -1,    -1,    -1,    -1,    62,    -1,   423,
      -1,    -1,    -1,   427,    -1,    -1,   430,    -1,   432,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     398,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   417,
      -1,    -1,    -1,   421,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   398,   572,
     135,   136,   137,   138,   139,   140,   141,   142,    -1,    -1,
     582,   398,   580,    -1,   587,    -1,    -1,   152,   153,   154,
      -1,   421,    -1,   582,    -1,    -1,    -1,    -1,    -1,    -1,
     417,    -1,    -1,    -1,   421,   607,    -1,    -1,   582,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   181,    -1,   607,   184,
      -1,   186,   187,   188,   189,   190,   191,   192,   193,   194,
     195,    -1,    -1,   607,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   207,    -1,   582,    -1,    -1,    -1,    -1,    -1,
      -1,   575,    -1,   656,    -1,    -1,    -1,    -1,    -1,    -1,
     584,    -1,   664,    -1,   666,   667,   668,    -1,    -1,   607,
      -1,   673,    -1,    -1,    -1,   664,    -1,   666,   667,   668,
      -1,    -1,    -1,    -1,   673,    -1,    -1,    -1,    -1,    -1,
     664,    -1,   666,   667,   668,    -1,   698,    -1,    -1,   673,
      -1,    -1,    -1,    -1,    -1,    -1,   708,    -1,    -1,   698,
      -1,    -1,    -1,    -1,   582,    -1,    -1,    -1,    -1,   708,
      -1,    -1,    -1,    -1,   698,    -1,   664,   730,   666,   667,
     668,    -1,    -1,    -1,   708,   673,    -1,    -1,   662,   607,
      -1,    -1,    -1,    -1,    -1,   747,   670,    -1,    -1,   751,
     752,    -1,   582,    -1,    -1,   757,   758,    -1,   747,   761,
     698,    -1,   751,   752,    -1,   582,    -1,    -1,   757,   758,
     708,    -1,   761,   747,    -1,    -1,    -1,   751,   752,    -1,
      -1,    -1,    -1,   757,   758,    -1,    -1,   761,    -1,   713,
     607,    -1,    -1,    -1,    -1,    -1,   664,    -1,   666,   667,
     668,    -1,    -1,    -1,    -1,   673,    -1,    -1,    -1,   747,
      -1,    -1,    -1,   751,   752,    -1,    -1,    -1,    -1,   757,
     758,    -1,    -1,   761,    -1,    -1,    -1,    -1,    -1,    -1,
     698,    -1,    -1,    -1,   664,    -1,   666,    -1,   668,    -1,
     708,    -1,    -1,   673,    -1,    -1,    -1,   664,    -1,   666,
     667,   668,    -1,    -1,    -1,    -1,   673,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   698,   133,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   708,   747,
      -1,   698,    -1,   751,   752,    -1,    -1,    -1,    -1,   757,
     758,   708,    -1,   761,   158,   159,   160,   161,   162,   163,
     164,   165,   166,   167,   168,   169,   170,   171,   172,   173,
     174,   175,   176,   177,   178,   179,    -1,   747,   202,    -1,
      -1,    -1,   752,    -1,    -1,    -1,    -1,    -1,   758,    -1,
     747,   761,    -1,    -1,   751,   752,   220,   221,   222,    -1,
     757,   758,   226,    -1,   761,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    62,    -1,   239,    -1,   241,   242,    -1,
      -1,   245,   246,    -1,   248,   249,   250,   251,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   260,   261,   262,   263,
     264,   265,   266,   267,    -1,   269,   270,   271,   272,   273,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   283,
     284,    -1,    -1,    -1,   288,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   301,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   135,   136,   137,   138,
     139,   140,   141,   142,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   152,   153,   154,    -1,   331,    -1,    -1,
     334,   335,   336,   337,   338,   339,   340,   341,   342,   343,
     344,   345,   346,   347,   348,   349,   350,   351,   352,   353,
     354,   355,   181,    -1,    -1,   184,    -1,   186,   187,   188,
     189,   190,   191,   192,   193,   194,   195,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   207,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    15,    16,    17,    18,    19,    20,    -1,    22,    -1,
      -1,    25,    26,    27,    -1,    29,   420,    31,    32,    33,
      34,    -1,    -1,    -1,    -1,   429,    -1,    -1,    -1,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    -1,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    95,    96,    -1,    -1,    62,   100,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   111,   112,   113,
     114,    -1,    -1,    -1,    -1,    -1,   120,   121,   122,    62,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     544,   545,    -1,    -1,    -1,    -1,   550,    -1,    -1,    -1,
     554,    -1,   556,   557,   558,   559,    -1,   561,   135,   136,
     137,   138,   139,   140,   141,   142,    -1,    -1,    -1,    -1,
      62,    -1,    -1,    -1,    -1,   152,   153,   154,    -1,   583,
      -1,    -1,   135,   136,   137,   138,   139,   140,   141,   142,
     204,    -1,    62,    -1,    -1,    -1,    -1,    -1,    -1,   152,
     153,   154,   606,    -1,    -1,    -1,    -1,   184,    -1,   186,
     187,   188,   189,   190,   191,   192,   193,   194,   195,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     207,   184,    -1,   186,   187,   188,   189,   190,   191,   192,
     193,   194,   195,   135,   136,   137,   138,   139,   140,   141,
     142,    -1,    -1,    -1,   207,    -1,    -1,    -1,    -1,    -1,
     152,   153,   154,    -1,    -1,   135,   136,   137,   138,   139,
     140,   141,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   685,   152,   153,   154,   689,    -1,    -1,    -1,    -1,
     694,    -1,   184,   697,   186,   187,   188,   189,   190,   191,
     192,   193,   194,   195,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   184,   207,   186,   187,   188,   189,
     190,   191,   192,   193,   194,   195,    -1,    -1,    -1,   733,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   207,    -1,    -1,
      -1,   745,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,    -1,     7,   760,    -1,    10,   763,
      12,    13,    14,    15,    -1,    17,    -1,    19,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    -1,    -1,
      52,    53,    54,    55,    56,    57,    58,    59,    60,    61,
      -1,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      92,    93,    94,    95,    96,    97,    98,    99,   100,   101,
     102,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,    -1,   115,   116,    -1,    -1,   119,    -1,    -1,
      -1,   123,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   143,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   158,   159,   160,   161,
     162,   163,   164,   165,   166,   167,   168,   169,   170,   171,
     172,   173,   174,   175,   176,   177,   178,   179,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
      -1,    -1,     7,    -1,    -1,    10,    -1,    12,    13,    14,
      15,   203,    17,   205,    19,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    -1,    -1,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    -1,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,    -1,
     115,   116,    -1,    -1,   119,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   143,   144,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   158,   159,   160,   161,   162,   163,   164,
     165,   166,   167,   168,   169,   170,   171,   172,   173,   174,
     175,   176,   177,   178,   179,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   203,   204,
     205,     3,     4,    -1,    -1,     7,    -1,    -1,    10,    -1,
      12,    13,    14,    15,    -1,    17,    -1,    19,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    -1,    -1,
      52,    53,    54,    55,    56,    57,    58,    59,    60,    61,
      -1,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      92,    93,    94,    95,    96,    97,    98,    99,   100,   101,
     102,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,    -1,   115,   116,    -1,    -1,   119,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   143,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   158,   159,   160,   161,
     162,   163,   164,   165,   166,   167,   168,   169,   170,   171,
     172,   173,   174,   175,   176,   177,   178,   179,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   203,   204,   205,     3,     4,    -1,    -1,     7,    -1,
      -1,    10,    -1,    12,    13,    14,    15,    -1,    17,    -1,
      19,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    -1,    -1,    52,    53,    54,    55,    56,    57,    58,
      59,    60,    61,    -1,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    91,    92,    93,    94,    95,    96,    97,    98,
      99,   100,   101,   102,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,   115,   116,    -1,    -1,
     119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   143,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,
     159,   160,   161,   162,   163,   164,   165,   166,   167,   168,
     169,   170,   171,   172,   173,   174,   175,   176,   177,   178,
     179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   203,   204,   205,     3,     4,    -1,
      -1,     7,    -1,    -1,    10,    -1,    12,    13,    14,    15,
      -1,    17,    -1,    19,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    -1,    -1,    52,    53,    54,    55,
      56,    57,    58,    59,    60,    61,    -1,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
      96,    97,    98,    99,   100,   101,   102,   103,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,    -1,   115,
     116,    -1,    -1,   119,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   143,   144,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   158,   159,   160,   161,   162,   163,   164,   165,
     166,   167,   168,   169,   170,   171,   172,   173,   174,   175,
     176,   177,   178,   179,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     5,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    16,    17,    -1,    19,    -1,   203,    -1,   205,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    -1,    -1,    52,    53,
      54,    55,    56,    57,    58,    59,    60,    61,    -1,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    95,    96,    97,    98,    99,   100,   101,   102,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
      -1,    -1,    -1,    -1,    -1,   119,    -1,    -1,    -1,    -1,
      -1,    -1,   126,   127,   128,   129,   130,    -1,    -1,   133,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   143,
     144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   158,   159,   160,   161,   162,   163,
     164,   165,   166,   167,   168,   169,   170,   171,   172,   173,
     174,   175,   176,   177,   178,   179,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     5,    -1,    -1,    -1,   192,    -1,
      -1,    -1,   196,   197,    -1,    16,    17,   201,    19,   203,
      -1,   205,   206,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    -1,
      -1,    52,    53,    54,    55,    56,    57,    58,    59,    60,
      61,    -1,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,    97,    98,    99,   100,
     101,   102,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,    -1,    -1,    -1,    -1,   119,    -1,
      -1,    -1,    -1,    -1,    -1,   126,   127,   128,   129,   130,
      -1,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   143,   144,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,   159,   160,
     161,   162,   163,   164,   165,   166,   167,   168,   169,   170,
     171,   172,   173,   174,   175,   176,   177,   178,   179,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     5,    -1,    -1,
      -1,   192,    -1,    -1,    -1,   196,   197,    -1,    -1,    17,
     201,    19,   203,    -1,    -1,   206,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    -1,    -1,    52,    53,    54,    55,    56,    57,
      58,    59,    60,    61,    -1,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    92,    93,    94,    95,    96,    97,
      98,    99,   100,   101,   102,   103,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    -1,    -1,    -1,    -1,    -1,
      -1,   119,    -1,    -1,    -1,    -1,    -1,    -1,   126,   127,
     128,   129,   130,    -1,    -1,   133,   134,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   143,   144,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     158,   159,   160,   161,   162,   163,   164,   165,   166,   167,
     168,   169,   170,   171,   172,   173,   174,   175,   176,   177,
     178,   179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       5,    -1,    -1,    -1,   192,    -1,    -1,    -1,   196,   197,
      -1,    -1,    17,    -1,    19,    -1,    -1,    -1,   206,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    -1,    -1,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    -1,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,    -1,
      -1,    -1,    -1,    -1,   119,    -1,    -1,    -1,    -1,    -1,
      -1,   126,   127,   128,   129,   130,    -1,    -1,   133,   134,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   143,   144,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   158,   159,   160,   161,   162,   163,   164,
     165,   166,   167,   168,   169,   170,   171,   172,   173,   174,
     175,   176,   177,   178,   179,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     5,    -1,    -1,    -1,   192,    -1,    -1,
      -1,   196,   197,    -1,    -1,    17,    -1,    19,    -1,    -1,
      -1,   206,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    -1,    -1,
      52,    53,    54,    55,    56,    57,    58,    59,    60,    61,
      -1,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      92,    93,    94,    95,    96,    97,    98,    99,   100,   101,
     102,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,    -1,    -1,    -1,    -1,    -1,   119,    -1,    -1,
      -1,    -1,    -1,    -1,   126,   127,   128,   129,   130,    -1,
      -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   143,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   158,   159,   160,   161,
     162,   163,   164,   165,   166,   167,   168,   169,   170,   171,
     172,   173,   174,   175,   176,   177,   178,   179,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     5,    -1,    -1,    -1,
     192,    -1,    -1,    -1,   196,   197,    -1,    -1,    17,    -1,
      19,    -1,    -1,    -1,   206,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    -1,    -1,    52,    53,    54,    55,    56,    57,    58,
      59,    60,    61,    -1,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    91,    92,    93,    94,    95,    96,    97,    98,
      99,   100,   101,   102,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    -1,    -1,    -1,    -1,
     119,    -1,    -1,    -1,    -1,    -1,    -1,   126,   127,   128,
     129,   130,    -1,    -1,   133,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   143,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,
     159,   160,   161,   162,   163,   164,   165,   166,   167,   168,
     169,   170,   171,   172,   173,   174,   175,   176,   177,   178,
     179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   192,    -1,    -1,    -1,   196,   197,    -1,
      -1,    -1,    -1,    19,    -1,    -1,    -1,   206,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    -1,    -1,    52,    53,    54,    55,
      56,    57,    58,    59,    60,    61,    -1,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
      96,    97,    98,    99,   100,   101,   102,   103,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,    -1,    -1,
      -1,    -1,    -1,   119,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   158,   159,   160,   161,   162,   163,   164,   165,
     166,   167,   168,   169,   170,   171,   172,   173,   174,   175,
     176,   177,   178,   179,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    19,    -1,    -1,    -1,    -1,
     206,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    -1,    -1,    52,    53,
      54,    55,    56,    57,    58,    59,    60,    61,    -1,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    95,    96,    97,    98,    99,   100,   101,   102,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
      -1,    -1,    -1,    -1,    -1,   119,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   158,   159,   160,   161,   162,   163,
     164,   165,   166,   167,   168,   169,   170,   171,   172,   173,
     174,   175,   176,   177,   178,   179,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    15,    -1,    17,    -1,    19,    -1,    -1,
     204,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    -1,    -1,
      52,    53,    54,    55,    56,    57,    58,    59,    60,    61,
      -1,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      92,    93,    94,    95,    96,    97,    98,    99,   100,   101,
     102,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,    -1,    -1,    -1,    -1,    -1,   119,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   143,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   158,   159,   160,   161,
     162,   163,   164,   165,   166,   167,   168,   169,   170,   171,
     172,   173,   174,   175,   176,   177,   178,   179,    15,    -1,
      17,    -1,    19,    -1,    -1,    -1,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    -1,    -1,    52,    53,    54,    55,    56,
      57,    58,    59,    60,    61,    -1,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,    -1,    -1,
      -1,    -1,   119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   143,   144,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   158,   159,   160,   161,   162,   163,   164,   165,   166,
     167,   168,   169,   170,   171,   172,   173,   174,   175,   176,
     177,   178,   179,    19,    -1,    -1,    -1,    -1,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    -1,    -1,    52,    53,    54,    55,
      56,    57,    58,    59,    60,    61,    -1,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
      96,    97,    98,    99,   100,   101,   102,   103,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,    -1,    -1,
      -1,    -1,    -1,   119,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   158,   159,   160,   161,   162,   163,   164,   165,
     166,   167,   168,   169,   170,   171,   172,   173,   174,   175,
     176,   177,   178,   179,    19,    -1,    -1,    -1,    -1,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    -1,    -1,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    -1,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,    -1,
      -1,    -1,    -1,    -1,   119,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   158,   159,   160,   161,   162,   163,   164,
     165,   166,   167,   168,   169,   170,   171,   172,   173,   174,
     175,   176,   177,   178,   179,    19,    -1,    -1,    -1,    -1,
      -1,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    -1,    -1,    52,    53,
      54,    55,    56,    57,    58,    59,    60,    61,    -1,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    95,    96,    97,    98,    99,   100,   101,   102,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
      -1,    -1,    -1,    -1,    -1,   119,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   158,   159,   160,   161,   162,   163,
     164,   165,   166,   167,   168,   169,   170,   171,   172,   173,
     174,   175,   176,   177,   178,   179,    19,    -1,    -1,    -1,
      -1,    -1,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    -1,    -1,    52,
      53,    54,    55,    56,    57,    58,    59,    60,    61,    -1,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    95,    96,    97,    98,    99,   100,   101,   102,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      62,    -1,    -1,    -1,    -1,    -1,   119,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   134,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   158,   159,   160,   161,   162,
     163,   164,   165,   166,   167,   168,   169,   170,   171,   172,
     173,   174,   175,   176,   177,   178,   179,    -1,    -1,    -1,
      -1,    -1,    -1,   135,   136,   137,   138,   139,   140,   141,
     142,    -1,    -1,    62,    -1,    -1,    -1,    -1,    -1,    -1,
     152,   153,   154,    -1,    -1,   135,   136,   137,   138,   139,
     140,   141,   142,    -1,    -1,    62,    -1,    -1,    -1,    -1,
      -1,    -1,   152,   153,   154,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   184,    -1,   186,   187,   188,   189,   190,   191,
     192,   193,   194,   195,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   184,   207,   186,   187,   188,   189,
     190,   191,   192,   193,   194,   195,   135,   136,   137,   138,
     139,   140,   141,   142,    -1,    -1,    62,   207,    -1,    -1,
      -1,    -1,    -1,   152,   153,   154,    -1,    -1,   135,   136,
     137,   138,   139,   140,   141,   142,    -1,    -1,    62,    -1,
      -1,    -1,    -1,    -1,    -1,   152,   153,   154,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   184,    -1,   186,   187,   188,
     189,   190,   191,   192,   193,   194,   195,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   184,   207,   186,
     187,   188,   189,   190,   191,   192,   193,   194,   195,   135,
     136,   137,   138,   139,   140,   141,   142,    -1,    -1,    62,
     207,    -1,    -1,    -1,    -1,    -1,   152,   153,   154,    -1,
      -1,   135,   136,   137,   138,   139,   140,   141,   142,    -1,
      -1,    62,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,
     154,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   184,    -1,
     186,   187,   188,   189,   190,   191,   192,   193,   194,   195,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     184,   207,   186,   187,   188,   189,   190,   191,   192,   193,
     194,   195,   135,   136,   137,   138,   139,   140,   141,   142,
      -1,    -1,    62,   207,    -1,    -1,    -1,    -1,    -1,   152,
     153,   154,    -1,    -1,   135,   136,   137,   138,   139,   140,
     141,   142,    -1,    -1,    62,    -1,    -1,    -1,    -1,    -1,
      -1,   152,   153,   154,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   184,    -1,   186,   187,   188,   189,   190,   191,   192,
     193,   194,   195,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   184,   207,   186,   187,   188,   189,   190,
     191,   192,   193,   194,   195,   135,   136,   137,   138,   139,
     140,   141,   142,    -1,    -1,    62,   207,    -1,    -1,    -1,
      -1,    -1,   152,   153,   154,    -1,    -1,   135,   136,   137,
     138,   139,   140,   141,   142,    -1,    -1,    62,    -1,    -1,
      -1,    -1,    -1,    -1,   152,   153,   154,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   184,    -1,   186,   187,   188,   189,
     190,   191,   192,   193,   194,   195,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   184,   207,   186,   187,
     188,   189,   190,   191,   192,   193,   194,   195,   135,   136,
     137,   138,   139,   140,   141,   142,    -1,    -1,    62,   207,
      -1,    -1,    -1,    -1,    -1,   152,   153,   154,    -1,    -1,
     135,   136,   137,   138,   139,   140,   141,   142,    -1,    -1,
      62,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,   154,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   184,    -1,   186,
     187,   188,   189,   190,   191,   192,   193,   194,   195,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   184,
     207,   186,   187,   188,   189,   190,   191,   192,   193,   194,
     195,   135,   136,   137,   138,   139,   140,   141,   142,    -1,
      -1,    62,   207,    -1,    -1,    -1,    -1,    -1,   152,   153,
     154,    -1,    -1,   135,   136,   137,   138,   139,   140,   141,
     142,    -1,    -1,    62,    -1,    -1,    -1,    -1,    -1,    -1,
     152,   153,   154,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     184,    -1,   186,   187,   188,   189,   190,   191,   192,   193,
     194,   195,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   184,   207,   186,   187,   188,   189,   190,   191,
     192,   193,   194,   195,   135,   136,   137,   138,   139,   140,
     141,   142,    -1,    -1,    62,   207,    -1,    -1,    -1,    -1,
      -1,   152,   153,   154,    -1,    -1,   135,   136,   137,   138,
     139,   140,   141,   142,    -1,    -1,    62,    -1,    -1,    -1,
      -1,    -1,    -1,   152,   153,   154,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   184,    -1,   186,   187,   188,   189,   190,
     191,   192,   193,   194,   195,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   184,   207,   186,   187,   188,
     189,   190,   191,   192,   193,   194,   195,   135,   136,   137,
     138,   139,   140,   141,   142,    -1,    -1,    -1,   207,    -1,
      -1,    -1,    -1,    -1,   152,   153,   154,    62,    -1,   135,
     136,   137,   138,   139,   140,   141,   142,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,   153,   154,    62,
      -1,    -1,    -1,    -1,    -1,    -1,   184,    -1,   186,   187,
     188,   189,   190,   191,   192,   193,   194,   195,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   184,   207,
     186,   187,   188,   189,   190,   191,   192,   193,   194,   195,
      -1,    -1,    -1,    -1,    -1,    -1,   202,    -1,    -1,    -1,
     135,   136,   137,   138,   139,   140,   141,   142,    -1,    -1,
      62,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,   154,
      -1,    -1,   135,   136,   137,   138,   139,   140,   141,   142,
      62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,
     153,   154,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   184,
      -1,   186,   187,   188,   189,   190,   191,   192,   193,   194,
     195,    -1,    -1,    -1,    -1,    -1,    -1,   202,   181,    -1,
      -1,   184,    -1,   186,   187,   188,   189,   190,   191,   192,
     193,   194,   195,   135,   136,   137,   138,   139,   140,   141,
     142,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,   153,   154,   135,   136,   137,   138,   139,   140,   141,
     142,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,   153,   154,    -1,    -1,    -1,    -1,    -1,    -1,   181,
      -1,    -1,   184,    -1,   186,   187,   188,   189,   190,   191,
     192,   193,   194,   195,    -1,    -1,    -1,    -1,    -1,   181,
      -1,    -1,   184,    -1,   186,   187,   188,   189,   190,   191,
     192,   193,   194,   195,   135,   136,   137,   138,   139,   140,
     141,   142,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   152,   153,   154,   135,   136,   137,   138,   139,   140,
     141,   142,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   152,   153,   154,    -1,    -1,    -1,    -1,    -1,    -1,
     181,    -1,    -1,   184,    -1,   186,   187,   188,   189,   190,
     191,   192,   193,   194,   195,    -1,    -1,    -1,    -1,    -1,
     181,    -1,    -1,   184,    -1,   186,   187,   188,   189,   190,
     191,   192,   193,   194,   195,   135,   136,   137,   138,   139,
     140,   141,   142,    62,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   152,   153,   154,   135,   136,   137,   138,   139,
     140,   141,   142,    62,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   152,   153,   154,    -1,    -1,    -1,    -1,    -1,
      -1,   181,    -1,    -1,   184,    -1,   186,   187,   188,   189,
     190,   191,   192,   193,   194,   195,    -1,    -1,    -1,    -1,
      -1,   181,    -1,    -1,   184,    -1,   186,   187,   188,   189,
     190,   191,   192,   193,   194,   195,   135,   136,   137,   138,
     139,   140,   141,   142,    62,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   152,   153,   154,   135,   136,   137,   138,
     139,   140,   141,   142,    62,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   152,   153,   154,    -1,    -1,    -1,    -1,
      -1,    -1,   181,    -1,    -1,   184,    -1,   186,   187,   188,
     189,   190,   191,   192,   193,   194,   195,    -1,    -1,    -1,
      -1,    -1,   181,    -1,    -1,   184,    -1,   186,   187,   188,
     189,   190,   191,   192,   193,   194,   195,   135,   136,   137,
     138,   139,   140,   141,   142,    62,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   152,   153,   154,   135,   136,   137,
     138,   139,   140,   141,   142,    62,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   152,   153,   154,    -1,    -1,    -1,
      -1,    -1,    -1,   181,    -1,    -1,   184,    -1,   186,   187,
     188,   189,   190,   191,   192,   193,   194,   195,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   184,   185,   186,   187,
     188,   189,   190,   191,   192,   193,   194,   195,   135,   136,
     137,   138,   139,   140,   141,   142,    62,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   152,   153,   154,   135,   136,
     137,   138,   139,   140,   141,   142,    62,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   152,   153,   154,    -1,    -1,
      -1,    -1,    -1,    -1,   181,    -1,    -1,   184,    -1,   186,
     187,   188,   189,   190,   191,   192,   193,   194,   195,    -1,
      -1,    -1,    -1,    -1,   181,    -1,    -1,   184,    -1,   186,
     187,   188,   189,   190,   191,   192,   193,   194,   195,   135,
     136,   137,   138,   139,   140,   141,   142,    62,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,   153,   154,   135,
     136,   137,   138,   139,   140,   141,   142,    62,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,   153,   154,    -1,
      -1,    -1,    -1,    -1,    -1,   181,    -1,    -1,   184,    -1,
     186,   187,   188,   189,   190,   191,   192,   193,   194,   195,
      -1,    -1,    -1,    -1,    -1,   181,    -1,    -1,   184,    -1,
     186,   187,   188,   189,   190,   191,   192,   193,   194,   195,
     135,   136,   137,   138,   139,   140,   141,   142,    62,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,   154,
     135,   136,   137,   138,   139,   140,   141,   142,    62,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,   154,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   184,
     185,   186,   187,   188,   189,   190,   191,   192,   193,   194,
     195,    -1,    -1,    -1,    -1,    -1,   181,    -1,    -1,   184,
      -1,   186,   187,   188,   189,   190,   191,   192,   193,   194,
     195,   135,   136,   137,   138,   139,   140,   141,   142,    62,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,
     154,   135,   136,   137,   138,   139,   140,   141,   142,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,
     154,    -1,    -1,    -1,    -1,    -1,    -1,   181,    -1,    -1,
     184,    -1,   186,   187,   188,   189,   190,   191,   192,   193,
     194,   195,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     184,    -1,   186,   187,   188,   189,   190,   191,   192,   193,
     194,   195,   135,   136,   137,   138,   139,   140,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,
     153,   154,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   186,   187,   188,   189,   190,   191,   192,
     193,   194,   195,    15,    16,    17,    18,    19,    20,    21,
      22,    -1,    -1,    25,    26,    27,    -1,    29,    -1,    31,
      32,    33,    34,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      -1,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      92,    93,    94,    95,    96,    -1,    -1,    -1,   100,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   111,
     112,   113,   114,    -1,    -1,    -1,    -1,    -1,   120,   121,
     122,    15,    16,    17,    18,    19,    20,    -1,    22,    -1,
      -1,    25,    26,    27,    -1,    29,    -1,    31,    32,    33,
      34,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    -1,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    95,    96,    -1,    -1,    -1,   100,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   111,   112,   113,
     114,    -1,    -1,    -1,    -1,    -1,   120,   121,   122
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint16 yystos[] =
{
       0,   209,   210,     0,     3,     4,     5,     7,    10,    12,
      13,    14,    15,    17,    19,    20,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    52,    53,    54,    55,    56,    57,
      58,    59,    60,    61,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    91,    92,    93,    94,    95,    96,    97,    98,
      99,   100,   101,   102,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   115,   116,   119,   123,   134,   143,
     144,   158,   159,   160,   161,   162,   163,   164,   165,   166,
     167,   168,   169,   170,   171,   172,   173,   174,   175,   176,
     177,   178,   179,   203,   205,   211,   212,   214,   215,   218,
     220,   225,   226,   227,   228,   230,   232,   233,   236,   239,
     243,   244,   247,   249,   250,   253,   254,   255,   257,   261,
     262,   263,   270,   205,   205,    19,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    61,    88,    89,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   134,   236,
     238,   206,   206,   236,   268,   269,   206,   242,   253,   254,
     257,   263,   206,    16,   126,   127,   128,   129,   130,   133,
     192,   196,   197,   201,   203,   205,   206,   214,   239,   254,
     255,   257,   261,   262,   263,   264,   265,   266,   267,   206,
     203,   206,   206,   206,   206,   206,   206,   206,   206,   206,
     206,   206,   206,   206,   206,   206,   206,   206,   206,   206,
     206,   206,   206,   206,   206,   206,   206,   206,   206,   206,
     206,   206,   206,   206,   215,   265,   206,   204,   213,   215,
     212,   236,   205,   206,   206,   215,   263,   200,   201,   145,
     146,   147,   148,   149,   150,   151,   155,   156,   157,   182,
     183,   260,   261,   205,   206,    15,   229,   236,   248,   270,
     264,   185,   222,   181,   242,   134,   134,   236,   262,   263,
     264,   264,   264,   256,   265,   236,   258,   259,   264,   263,
     182,   260,   261,    62,   135,   136,   137,   138,   139,   140,
     141,   142,   152,   153,   154,   184,   186,   187,   188,   189,
     190,   191,   192,   193,   194,   195,   205,   264,   271,   264,
     264,   207,   265,   264,   264,   265,   264,   264,   264,   264,
     133,   207,   207,   207,   207,   207,   207,   265,   264,   264,
     264,   264,   264,   264,   264,   264,   207,   128,   133,   264,
     128,   133,   264,   133,   264,   264,   264,   117,   118,   205,
     256,   204,   215,   120,   203,   214,   219,   264,   264,    13,
     236,   264,   265,   264,   236,   237,   236,   181,    11,   240,
     205,   207,   236,   182,   269,   207,   207,   206,   200,   201,
     181,   202,   185,   181,   204,   207,   265,   264,   262,   264,
     264,   264,   264,   264,   264,   264,   264,   264,   264,   264,
     264,   264,   264,   264,   264,   264,   264,   264,   264,   264,
     264,   207,    15,    16,    17,    18,    19,    20,    22,    25,
      26,    27,    29,    31,    32,    33,    34,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
     100,   111,   112,   113,   114,   120,   121,   122,   272,   273,
     277,   207,   207,   207,   181,   181,   207,   207,   207,   207,
     181,   207,   181,   207,   181,   207,   181,   181,   181,   181,
     207,   181,   207,   207,   207,   207,   207,   207,   207,   207,
     181,   181,   206,   215,   207,   236,   124,   125,   216,   217,
     221,   207,   207,   206,   206,   202,   222,   181,   207,   240,
     270,   241,   254,   257,   263,   207,   231,   264,   215,   265,
     256,   134,   264,   265,   265,   258,   185,   203,   274,   128,
     278,   131,   133,   133,   204,   277,   264,   264,   264,   133,
     264,   264,   264,   252,   264,   252,   264,   133,   158,   159,
     160,   161,   162,   163,   164,   165,   166,   167,   168,   169,
     170,   171,   172,   173,   174,   175,   176,   177,   178,   179,
     279,   279,   236,   219,   204,   217,    15,   239,   203,   215,
     264,   256,   206,   236,   222,   207,   246,   205,     6,   207,
     206,   202,   264,   213,   272,   126,   127,   128,   129,   130,
     131,   133,   275,   276,   207,   181,   181,   251,   207,   181,
     251,   251,   251,   251,   181,   207,   207,   181,   207,   223,
     224,   236,   234,   207,   207,   256,   222,   215,   246,   215,
     248,   215,   256,   206,   204,    21,   181,   264,    98,    99,
     133,   207,   264,   207,   207,   207,   207,   264,   264,   215,
     181,   205,   222,     8,     9,   204,   235,   207,   215,   207,
     207,   256,   275,   207,   207,   181,   207,   118,   224,   182,
     264,   185,   245,   207,   264,   215,   267,   185,   213,   215,
     181,   213,   264,   181,   264,   207
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  However,
   YYFAIL appears to be in use.  Nevertheless, it is formally deprecated
   in Bison 2.4.2's NEWS entry, where a plan to phase it out is
   discussed.  */

#define YYFAIL		goto yyerrlab
#if defined YYFAIL
  /* This is here to suppress warnings from the GCC cpp's
     -Wunused-macros.  Normally we don't worry about that warning, but
     some users do, and we want to make it easy for users to remove
     YYFAIL uses, which will produce warnings from Bison 2.5.  */
#endif

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* This macro is provided for backward compatibility. */

#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
#else
static void
yy_stack_print (yybottom, yytop)
    yytype_int16 *yybottom;
    yytype_int16 *yytop;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (0, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  YYSIZE_T yysize1;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = 0;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - Assume YYFAIL is not used.  It's too flawed to consider.  See
       <http://lists.gnu.org/archive/html/bison-patches/2009-12/msg00024.html>
       for details.  YYERROR is fine as it does not invoke this
       function.
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                yysize1 = yysize + yytnamerr (0, yytname[yyx]);
                if (! (yysize <= yysize1
                       && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                  return 2;
                yysize = yysize1;
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  yysize1 = yysize + yystrlen (yyformat);
  if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
    return 2;
  yysize = yysize1;

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */
#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */


/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       `yyss': related to states.
       `yyvs': related to semantic values.

       Refer to the stacks thru separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yytoken = 0;
  yyss = yyssa;
  yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */
  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = (yytype_int16) yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;

	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss_alloc, yyss);
	YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:

    { bf = newBuffer();
		bc = newBuffer();
	}
    break;

  case 3:

    { Buffer b = newBuffer();
		  bufferWriteConstants(b);
		  bufferConcat(b, bf);
		  bufferConcat(b, bc);
		  *((Buffer *)buffer) = b; }
    break;

  case 4:

    { Buffer b = newBuffer(); *((Buffer *)buffer) = b; }
    break;

  case 7:

    { bufferConcat(bc, (yyvsp[(1) - (1)].action)); }
    break;

  case 8:

    { 
		  if(swfVersion > 6)
			bufferWriteFunction(bf, (yyvsp[(1) - (1)].function), 2); 
		  else
			bufferWriteFunction(bf, (yyvsp[(1) - (1)].function), 1);
		}
    break;

  case 9:

    { bufferWriteClass(bf, (yyvsp[(1) - (1)].clazz)); }
    break;

  case 10:

    { (yyval.action) = (yyvsp[(1) - (1)].action); }
    break;

  case 11:

    { (yyval.action) = (yyvsp[(1) - (2)].action);
		  bufferConcat((yyval.action), (yyvsp[(2) - (2)].action)); }
    break;

  case 12:

    { }
    break;

  case 13:

    { (yyval.action) = NULL; }
    break;

  case 14:

    { (yyval.action) = (yyvsp[(2) - (3)].action); }
    break;

  case 15:

    { (yyval.action) = NULL; }
    break;

  case 16:

    { (yyval.action) = (yyvsp[(1) - (2)].action); }
    break;

  case 27:

    { 	
		(yyval.classMember) = (yyvsp[(1) - (2)].classMember);
		ASClassMember_append((yyvsp[(1) - (2)].classMember), (yyvsp[(2) - (2)].classMember));			
	}
    break;

  case 28:

    { (yyval.classMember) = newASClassMember_function((yyvsp[(2) - (2)].function)); }
    break;

  case 29:

    { (yyval.classMember) = (yyvsp[(3) - (4)].classMember); }
    break;

  case 30:

    {
		if(classContext)
		{
			swf5error("Nested classes are not allowed\n");
			YYABORT;
		}
		classContext = 1;
	}
    break;

  case 31:

    {(yyval.classMember) = NULL; }
    break;

  case 32:

    { (yyval.classMember) = (yyvsp[(2) - (3)].classMember); }
    break;

  case 33:

    { 
		(yyval.clazz) = newASClass((yyvsp[(2) - (3)].str), NULL, (yyvsp[(3) - (3)].classMember));
		classContext = 0;
	}
    break;

  case 34:

    { 
		(yyval.clazz) = newASClass((yyvsp[(2) - (5)].str), (yyvsp[(4) - (5)].str), (yyvsp[(5) - (5)].classMember));
		classContext = 0;
	}
    break;

  case 41:

    { (yyval.classMember) = (yyvsp[(1) - (3)].classMember);
		  ASClassMember_append((yyvsp[(1) - (3)].classMember), (yyvsp[(3) - (3)].classMember)); 
		}
    break;

  case 42:

    { 
		  ASVariable v = newASVariable((yyvsp[(1) - (4)].str), (yyvsp[(4) - (4)].action)); 
		  (yyval.classMember) = newASClassMember_variable(v);
		}
    break;

  case 43:

    { 
			ASVariable v = newASVariable((yyvsp[(1) - (2)].str), NULL);
			(yyval.classMember) = newASClassMember_variable(v);
		}
    break;

  case 44:

    { (yyval.action) = (yyvsp[(2) - (3)].action); bufferWriteOp((yyval.action), SWFACTION_THROW); }
    break;

  case 45:

    { 	(yyval.action) = newBuffer();
									bufferWriteOp((yyval.action), SWFACTION_TRY);
									bufferWriteS16((yyval.action), 8);                /* TRY tag length */
									bufferWriteU8((yyval.action), 0);                 /* flags */
									bufferWriteS16((yyval.action), bufferLength((yyvsp[(2) - (2)].action))); /* try block length */
									bufferWriteS16((yyval.action), 0);                /* catch block length */
									bufferWriteS16((yyval.action), 0);                /* finally block length */
									bufferWriteU8((yyval.action), 0);                 /* catch name - empty string */
									bufferConcat((yyval.action), (yyvsp[(2) - (2)].action));                 /* append TRY body */
								 }
    break;

  case 46:

    { (yyval.action) = newBuffer();
									bufferWriteOp((yyval.action), SWFACTION_TRY);
									bufferWriteS16((yyval.action), (int) (8+strlen((yyvsp[(5) - (7)].str))));       /* TRY tag length */
									bufferWriteU8((yyval.action), 1);                   /* flags */
									bufferWriteS16((yyval.action), bufferLength((yyvsp[(2) - (7)].action))+5); /* try block length + JUMP length */
									bufferWriteS16((yyval.action), bufferLength((yyvsp[(7) - (7)].action)));   /* catch block length */
									bufferWriteS16((yyval.action), 0);                  /* finally block length */
									bufferWriteHardString((yyval.action), (yyvsp[(5) - (7)].str), strlen((yyvsp[(5) - (7)].str))+1); /* catch name */
									bufferConcat((yyval.action), (yyvsp[(2) - (7)].action));                   /* append TRY body */
									bufferWriteOp((yyval.action), SWFACTION_JUMP);      /* jump after catch */
									bufferWriteS16((yyval.action), 2);                  /* ... */
									bufferWriteS16((yyval.action), bufferLength((yyvsp[(7) - (7)].action)));   /* ... */
									bufferConcat((yyval.action), (yyvsp[(7) - (7)].action));                   /* append CATCH body */
								}
    break;

  case 47:

    {	(yyval.action) = newBuffer();
									bufferWriteOp((yyval.action), SWFACTION_TRY);
									bufferWriteS16((yyval.action), 8);                /* TRY tag length */
									bufferWriteU8((yyval.action), 2);                 /* flags */
									bufferWriteS16((yyval.action), bufferLength((yyvsp[(2) - (4)].action))); /* try block length */
									bufferWriteS16((yyval.action), 0);                /* catch block length */
									bufferWriteS16((yyval.action), bufferLength((yyvsp[(4) - (4)].action))); /* finally block length */
									bufferWriteU8((yyval.action), 0);                 /* catch name - empty string */
									bufferConcat((yyval.action), (yyvsp[(2) - (4)].action));                 /* append TRY body */
									bufferConcat((yyval.action), (yyvsp[(4) - (4)].action));                 /* append FINALLY body */
								 }
    break;

  case 48:

    { (yyval.action) = newBuffer();
									bufferWriteOp((yyval.action), SWFACTION_TRY);
									bufferWriteS16((yyval.action), (int) (8+strlen((yyvsp[(5) - (9)].str))));        /* TRY tag length */
									bufferWriteU8((yyval.action), 3);                    /* flags */
									bufferWriteS16((yyval.action), bufferLength((yyvsp[(2) - (9)].action))+5);  /* try block length + JUMP length */
									bufferWriteS16((yyval.action), bufferLength((yyvsp[(7) - (9)].action)));    /* catch block length */
									bufferWriteS16((yyval.action), bufferLength((yyvsp[(9) - (9)].action)));    /* finally block length */
									bufferWriteHardString((yyval.action), (yyvsp[(5) - (9)].str), strlen((yyvsp[(5) - (9)].str))+1); /* catch name */
									bufferConcat((yyval.action), (yyvsp[(2) - (9)].action));                    /* append TRY body */
									bufferWriteOp((yyval.action), SWFACTION_JUMP);       /* jump after catch */
									bufferWriteS16((yyval.action), 2); 
									bufferWriteS16((yyval.action), bufferLength((yyvsp[(7) - (9)].action)));
									bufferConcat((yyval.action), (yyvsp[(7) - (9)].action));                    /* append CATCH body */
									bufferConcat((yyval.action), (yyvsp[(9) - (9)].action));                    /* append FINALLY body */
								}
    break;

  case 49:

    { (yyval.action) = (yyvsp[(3) - (7)].action);
		  bufferWriteOp((yyval.action), SWFACTION_WITH);
		  bufferWriteS16((yyval.action), 2);
		  bufferWriteS16((yyval.action), bufferLength((yyvsp[(6) - (7)].action)));
		  bufferConcat((yyval.action), (yyvsp[(6) - (7)].action)); }
    break;

  case 50:

    { int tmp = chkctx(CTX_FUNCTION);
		  if(tmp < 0) 
		  {
			swf5error("return outside function");
			YYABORT;
		  }
		  (yyval.action) = newBuffer();
		  while(--tmp >= 0)
			bufferWriteOp((yyval.action), SWFACTION_POP);
		  bufferWriteUndef((yyval.action));
		  bufferWriteOp((yyval.action), SWFACTION_RETURN); }
    break;

  case 51:

    { int tmp = chkctx(CTX_FUNCTION);
		  if(tmp < 0)
		  {
			swf5error("return outside function");
			YYABORT;
		  }
		  (yyval.action) = newBuffer();
		  while(--tmp >= 0)
			bufferWriteOp((yyval.action), SWFACTION_POP);
		  bufferConcat((yyval.action), (yyvsp[(2) - (3)].action));
		  bufferWriteOp((yyval.action), SWFACTION_RETURN); }
    break;

  case 53:

    { bufferConcat((yyvsp[(1) - (3)].action), (yyvsp[(3) - (3)].action)); }
    break;

  case 54:

    { (yyval.action) = (yyvsp[(3) - (7)].action);
		  bufferWriteOp((yyval.action), SWFACTION_IF);
		  bufferWriteS16((yyval.action), 2);
		  bufferWriteS16((yyval.action), bufferLength((yyvsp[(7) - (7)].action))+5);
		  bufferConcatSimple((yyval.action), (yyvsp[(7) - (7)].action));
		  bufferWriteOp((yyval.action), SWFACTION_JUMP);
		  bufferWriteS16((yyval.action), 2);
		  bufferWriteS16((yyval.action), bufferLength((yyvsp[(5) - (7)].action)));
		  bufferConcatSimple((yyval.action), (yyvsp[(5) - (7)].action)); }
    break;

  case 55:

    { (yyval.action) = (yyvsp[(3) - (5)].action);
		  bufferWriteOp((yyval.action), SWFACTION_LOGICALNOT);
		  bufferWriteOp((yyval.action), SWFACTION_IF);
		  bufferWriteS16((yyval.action), 2);
		  bufferWriteS16((yyval.action), bufferLength((yyvsp[(5) - (5)].action)));
		  bufferConcatSimple((yyval.action), (yyvsp[(5) - (5)].action)); }
    break;

  case 56:

    { (yyval.action) = NULL; }
    break;

  case 57:

    { (yyval.action) = (yyvsp[(1) - (1)].action); }
    break;

  case 58:

    { addctx(CTX_SWITCH); }
    break;

  case 59:

    { (yyval.action) = (yyvsp[(3) - (7)].action);
		  bufferResolveSwitch((yyval.action), &(yyvsp[(6) - (7)].switchcases));
		  bufferResolveJumps((yyval.action));
		  bufferWriteOp((yyval.action), SWFACTION_POP);
		  delctx(CTX_SWITCH);
 /* FIXME: continue in switch continues surrounding loop, if any */
	}
    break;

  case 60:

    { (yyval.switchcases).count = 0;
		  (yyval.switchcases).list = 0; }
    break;

  case 61:

    { (yyval.switchcases) = (yyvsp[(1) - (2)].switchcases);
		  (yyval.switchcases).list = (struct switchcase*) realloc((yyval.switchcases).list, ((yyval.switchcases).count+1) * sizeof(struct switchcase));
		  (yyval.switchcases).list[(yyval.switchcases).count] = (yyvsp[(2) - (2)].switchcase);
		  (yyval.switchcases).count++; }
    break;

  case 62:

    { (yyval.switchcase).cond = (yyvsp[(2) - (4)].action);
		  (yyval.switchcase).action = (yyvsp[(4) - (4)].action);
		  if(chkctx(CTX_BREAK) == CTX_BREAK)
		  {
			delctx(CTX_BREAK);
		  	(yyval.switchcase).isbreak = 1;
		  }
		  else
			(yyval.switchcase).isbreak = 0; 
		}
    break;

  case 63:

    { (yyval.switchcase).cond = NULL;
		  (yyval.switchcase).action = (yyvsp[(3) - (3)].action);
		  if(chkctx(CTX_BREAK) == CTX_BREAK)
	          {
			delctx(CTX_BREAK);
		  	(yyval.switchcase).isbreak = 1;
		  }
		  else
			(yyval.switchcase).isbreak = 0;
		}
    break;

  case 65:

    { (yyval.str) = strdup("targetPath"); }
    break;

  case 66:

    { (yyval.str) = strdup("random"); }
    break;

  case 67:

    { (yyval.str) = strdup("getTimer"); }
    break;

  case 68:

    { (yyval.str) = strdup("length"); }
    break;

  case 69:

    { (yyval.str) = strdup("concat"); }
    break;

  case 70:

    { (yyval.str) = strdup("substr"); }
    break;

  case 71:

    { (yyval.str) = strdup("trace"); }
    break;

  case 72:

    { (yyval.str) = strdup("int"); }
    break;

  case 73:

    { (yyval.str) = strdup("ord"); }
    break;

  case 74:

    { (yyval.str) = strdup("chr"); }
    break;

  case 75:

    { (yyval.str) = strdup("getURL"); }
    break;

  case 76:

    { (yyval.str) = strdup("getURL1"); }
    break;

  case 77:

    { (yyval.str) = strdup("nextFrame"); }
    break;

  case 78:

    { (yyval.str) = strdup("prevFrame"); }
    break;

  case 79:

    { (yyval.str) = strdup("play"); }
    break;

  case 80:

    { (yyval.str) = strdup("stop"); }
    break;

  case 81:

    { (yyval.str) = strdup("toggleQuality"); }
    break;

  case 82:

    { (yyval.str) = strdup("stopSounds"); }
    break;

  case 83:

    { (yyval.str) = strdup("dup"); }
    break;

  case 84:

    { (yyval.str) = strdup("swap"); }
    break;

  case 85:

    { (yyval.str) = strdup("pop"); }
    break;

  case 86:

    { (yyval.str) = strdup("push"); }
    break;

  case 87:

    { (yyval.str) = strdup("setRegister"); }
    break;

  case 88:

    { (yyval.str) = strdup("callFunction"); }
    break;

  case 89:

    { (yyval.str) = strdup("callMethod"); }
    break;

  case 90:

    { (yyval.str) = strdup("and"); }
    break;

  case 91:

    { (yyval.str) = strdup("or"); }
    break;

  case 92:

    { (yyval.str) = strdup("xor"); }
    break;

  case 93:

    { (yyval.str) = strdup("modulo"); }
    break;

  case 94:

    { (yyval.str) = strdup("add"); }
    break;

  case 95:

    { (yyval.str) = strdup("lessThan"); }
    break;

  case 96:

    { (yyval.str) = strdup("equals"); }
    break;

  case 97:

    { (yyval.str) = strdup("inc"); }
    break;

  case 98:

    { (yyval.str) = strdup("dec"); }
    break;

  case 99:

    { (yyval.str) = strdup("typeof"); }
    break;

  case 100:

    { (yyval.str) = strdup("enumerate2"); }
    break;

  case 101:

    { (yyval.str) = strdup("enumerate"); }
    break;

  case 102:

    { (yyval.str) = strdup("initobject"); }
    break;

  case 103:

    { (yyval.str) = strdup("initarray"); }
    break;

  case 104:

    { (yyval.str) = strdup("getmember"); }
    break;

  case 105:

    { (yyval.str) = strdup("setmember"); }
    break;

  case 106:

    { (yyval.str) = strdup("shiftleft"); }
    break;

  case 107:

    { (yyval.str) = strdup("shiftright"); }
    break;

  case 108:

    { (yyval.str) = strdup("shiftright2"); }
    break;

  case 109:

    { (yyval.str) = strdup("varequals"); }
    break;

  case 110:

    { (yyval.str) = strdup("oldAdd"); }
    break;

  case 111:

    { (yyval.str) = strdup("subtract"); }
    break;

  case 112:

    { (yyval.str) = strdup("multiply"); }
    break;

  case 113:

    { (yyval.str) = strdup("divide"); }
    break;

  case 114:

    { (yyval.str) = strdup("oldequals"); }
    break;

  case 115:

    { (yyval.str) = strdup("oldlessthan"); }
    break;

  case 116:

    { (yyval.str) = strdup("logicaland"); }
    break;

  case 117:

    { (yyval.str) = strdup("logicalor"); }
    break;

  case 118:

    { (yyval.str) = strdup("not"); }
    break;

  case 119:

    { (yyval.str) = strdup("stringeq"); }
    break;

  case 120:

    { (yyval.str) = strdup("stringlength"); }
    break;

  case 121:

    { (yyval.str) = strdup("substring"); }
    break;

  case 122:

    { (yyval.str) = strdup("getvariable"); }
    break;

  case 123:

    { (yyval.str) = strdup("setvariable"); }
    break;

  case 124:

    { (yyval.str) = strdup("settargetexpression"); }
    break;

  case 125:

    { (yyval.str) = strdup("duplicateMovieClip"); }
    break;

  case 126:

    { (yyval.str) = strdup("removeMovieClip"); }
    break;

  case 127:

    { (yyval.str) = strdup("startDrag"); }
    break;

  case 128:

    { (yyval.str) = strdup("stopDrag"); }
    break;

  case 129:

    { (yyval.str) = strdup("stringlessthan"); }
    break;

  case 130:

    { (yyval.str) = strdup("mblength"); }
    break;

  case 131:

    { (yyval.str) = strdup("mbsubstring"); }
    break;

  case 132:

    { (yyval.str) = strdup("mbord"); }
    break;

  case 133:

    { (yyval.str) = strdup("mbchr"); }
    break;

  case 134:

    { (yyval.str) = strdup("branchalways"); }
    break;

  case 135:

    { (yyval.str) = strdup("branchiftrue"); }
    break;

  case 136:

    { (yyval.str) = strdup("getURL2"); }
    break;

  case 137:

    { (yyval.str) = strdup("post"); }
    break;

  case 138:

    { (yyval.str) = strdup("get"); }
    break;

  case 139:

    { (yyval.str) = strdup("loadVariables"); }
    break;

  case 140:

    { (yyval.str) = strdup("loadVariablesNum"); }
    break;

  case 141:

    { (yyval.str) = strdup("loadMovie"); }
    break;

  case 142:

    { (yyval.str) = strdup("loadMovieNum"); }
    break;

  case 143:

    { (yyval.str) = strdup("gotoAndStop"); }
    break;

  case 144:

    { (yyval.str) = strdup("gotoAndPlay"); }
    break;

  case 145:

    { (yyval.str) = strdup("setTarget"); }
    break;

  case 146:

    { (yyval.str) = strdup("call"); }
    break;

  case 147:

    { (yyval.str) = strdup("getProperty"); }
    break;

  case 148:

    { (yyval.str) = strdup("setProperty"); }
    break;

  case 149:

    { (yyval.str) = strdup("cast"); }
    break;

  case 150:

    { (yyval.str) = strdup("swfAction"); }
    break;

  case 151:

    { (yyval.str) = strdup("this"); }
    break;

  case 152:

    { (yyval.str) = strdup("_x"); }
    break;

  case 153:

    { (yyval.str) = strdup("_y"); }
    break;

  case 154:

    { (yyval.str) = strdup("_xscale"); }
    break;

  case 155:

    { (yyval.str) = strdup("_yscale"); }
    break;

  case 156:

    { (yyval.str) = strdup("_currentframe"); }
    break;

  case 157:

    { (yyval.str) = strdup("_totalframes"); }
    break;

  case 158:

    { (yyval.str) = strdup("_alpha"); }
    break;

  case 159:

    { (yyval.str) = strdup("_visible"); }
    break;

  case 160:

    { (yyval.str) = strdup("_width"); }
    break;

  case 161:

    { (yyval.str) = strdup("_height"); }
    break;

  case 162:

    { (yyval.str) = strdup("_rotation"); }
    break;

  case 163:

    { (yyval.str) = strdup("_target"); }
    break;

  case 164:

    { (yyval.str) = strdup("_framesloaded"); }
    break;

  case 165:

    { (yyval.str) = strdup("_name"); }
    break;

  case 166:

    { (yyval.str) = strdup("_droptarget"); }
    break;

  case 167:

    { (yyval.str) = strdup("_url"); }
    break;

  case 168:

    { (yyval.str) = strdup("_highquality"); }
    break;

  case 169:

    { (yyval.str) = strdup("_focusrect"); }
    break;

  case 170:

    { (yyval.str) = strdup("_soundbuftime"); }
    break;

  case 171:

    { (yyval.str) = strdup("_quality"); }
    break;

  case 172:

    { (yyval.str) = strdup("_xmouse"); }
    break;

  case 173:

    { (yyval.str) = strdup("_ymouse"); }
    break;

  case 174:

    { (yyval.exprlist).buffer = newBuffer();
		  (yyval.exprlist).count = 0; }
    break;

  case 175:

    { (yyval.exprlist).buffer = newBuffer();
		  bufferWriteHardString((yyval.exprlist).buffer, (yyvsp[(1) - (2)].str), strlen((yyvsp[(1) - (2)].str))+1);
		  (yyval.exprlist).count = 1;
		  free((yyvsp[(1) - (2)].str)); }
    break;

  case 176:

    { (yyval.exprlist) = (yyvsp[(1) - (4)].exprlist);
		  bufferWriteHardString((yyval.exprlist).buffer, (yyvsp[(3) - (4)].str), strlen((yyvsp[(3) - (4)].str))+1);
		  ++(yyval.exprlist).count;
		  free((yyvsp[(3) - (4)].str)); }
    break;

  case 177:

    { addctx(CTX_FUNCTION); (yyval.str) = NULL; }
    break;

  case 178:

    { addctx(CTX_FUNCTION); (yyval.str) = (yyvsp[(1) - (1)].str); }
    break;

  case 179:

    {
		(yyval.function) = newASFunction();
		(yyval.function)->name = (yyvsp[(2) - (7)].str);
		(yyval.function)->params = (yyvsp[(4) - (7)].exprlist);
		(yyval.function)->code = (yyvsp[(7) - (7)].action);	
		delctx(CTX_FUNCTION);	
	}
    break;

  case 180:

    { (yyval.action) = (yyvsp[(2) - (2)].action); }
    break;

  case 181:

    { if((yyvsp[(1) - (1)].lval).obj)
		  {
		    (yyval.action) = (yyvsp[(1) - (1)].lval).obj;
		    (yyval.action)->hasObject = 1;
		    if((yyvsp[(1) - (1)].lval).ident)
		      bufferConcat((yyval.action), (yyvsp[(1) - (1)].lval).ident);
		    else
		      bufferConcat((yyval.action), (yyvsp[(1) - (1)].lval).memexpr);

		    bufferWriteOp((yyval.action), SWFACTION_GETMEMBER);
		  }
		  else
		  {
		    (yyval.action) = (yyvsp[(1) - (1)].lval).ident;
		  }
		}
    break;

  case 184:

    { if((yyvsp[(1) - (1)].lval).obj)
		  {
		    (yyval.action) = (yyvsp[(1) - (1)].lval).obj;
		    (yyval.action)->hasObject = 1; 
		    if((yyvsp[(1) - (1)].lval).ident)
		      bufferConcat((yyval.action), (yyvsp[(1) - (1)].lval).ident);
		    else
		      bufferConcat((yyval.action), (yyvsp[(1) - (1)].lval).memexpr);
		  }
		  else
		  {
		    (yyval.action) = (yyvsp[(1) - (1)].lval).ident;
		  }
		}
    break;

  case 188:

    { (yyval.action) = (yyvsp[(2) - (3)].action); }
    break;

  case 189:

    { addctx(CTX_LOOP); }
    break;

  case 190:

    { addctx(CTX_LOOP); }
    break;

  case 191:

    { addctx(CTX_LOOP); }
    break;

  case 192:

    { addctx(CTX_FOR_IN); }
    break;

  case 193:

    { (yyval.action) = (yyvsp[(3) - (5)].action);
		  bufferWriteOp((yyval.action), SWFACTION_LOGICALNOT);
		  bufferWriteOp((yyval.action), SWFACTION_IF);
		  bufferWriteS16((yyval.action), 2);
		  bufferWriteS16((yyval.action), bufferLength((yyvsp[(5) - (5)].action))+5);
		  bufferConcat((yyval.action), (yyvsp[(5) - (5)].action));
		  bufferWriteOp((yyval.action), SWFACTION_JUMP);
		  bufferWriteS16((yyval.action), 2);
		  bufferWriteS16((yyval.action), -(bufferLength((yyval.action))+2));
		  bufferResolveJumps((yyval.action));
		  delctx(CTX_LOOP); }
    break;

  case 194:

    { if((yyvsp[(2) - (6)].action))
			{	(yyval.action) = (yyvsp[(2) - (6)].action);
		  		bufferConcat((yyval.action), (yyvsp[(5) - (6)].action));
			}
			else
				(yyval.action) = (yyvsp[(5) - (6)].action);
		  bufferWriteOp((yyval.action), SWFACTION_IF);
		  bufferWriteS16((yyval.action), 2);
		  bufferWriteS16((yyval.action), -(bufferLength((yyval.action))+2));
		  bufferResolveJumps((yyval.action));
		  delctx(CTX_LOOP); }
    break;

  case 195:

    {
		  int continue_len;
		  if((yyvsp[(3) - (10)].action))
		    (yyval.action) = (yyvsp[(3) - (10)].action);
		  else
		    (yyval.action) = newBuffer();

		  continue_len = bufferLength ((yyvsp[(7) - (10)].action));
		  if((yyvsp[(10) - (10)].action))
		    bufferConcatSimple((yyvsp[(10) - (10)].action), (yyvsp[(7) - (10)].action));
		  else if ((yyvsp[(7) - (10)].action))
		    (yyvsp[(10) - (10)].action) = (yyvsp[(7) - (10)].action);
		  else 
		    (yyvsp[(10) - (10)].action) = newBuffer();
		  if((yyvsp[(5) - (10)].action))
		  {
                    bufferWriteOp((yyvsp[(5) - (10)].action), SWFACTION_LOGICALNOT);
                    bufferWriteOp((yyvsp[(5) - (10)].action), SWFACTION_IF);
                    bufferWriteS16((yyvsp[(5) - (10)].action), 2);
                    bufferWriteS16((yyvsp[(5) - (10)].action), bufferLength((yyvsp[(10) - (10)].action))+5);
		    bufferConcat((yyvsp[(5) - (10)].action), (yyvsp[(10) - (10)].action));
                  }
		  else
		    (yyvsp[(5) - (10)].action) = (yyvsp[(10) - (10)].action);

                  bufferWriteOp((yyvsp[(5) - (10)].action), SWFACTION_JUMP);
                  bufferWriteS16((yyvsp[(5) - (10)].action), 2);
                  bufferWriteS16((yyvsp[(5) - (10)].action), -(bufferLength((yyvsp[(5) - (10)].action))+2));
		  /* need to jump to last part of for stmt in continue case */
		  if (continue_len)
		    bufferResolveJumpsFull((yyvsp[(5) - (10)].action), (yyvsp[(5) - (10)].action)->pos, (yyvsp[(5) - (10)].action)->pos - continue_len - 5);
		  else
		    bufferResolveJumps((yyvsp[(5) - (10)].action));

                  bufferConcat((yyval.action), (yyvsp[(5) - (10)].action));
		  delctx(CTX_LOOP);
                }
    break;

  case 196:

    { Buffer b2, b3;
		  int tmp;

		  (yyval.action) = (yyvsp[(4) - (7)].action);
		  if((yyvsp[(4) - (7)].action)->hasObject)
			bufferWriteOp((yyval.action), SWFACTION_ENUMERATE2);
		  else
			bufferWriteOp((yyval.action), SWFACTION_ENUMERATE);

		  b2 = newBuffer();
		  bufferWriteSetRegister(b2, 0);
		  bufferWriteOp(b2, SWFACTION_PUSH);
		  bufferWriteS16(b2, 1);
		  bufferWriteU8(b2, 2);
		  bufferWriteOp(b2, SWFACTION_EQUALS2);
		  bufferWriteOp(b2, SWFACTION_IF);
		  bufferWriteS16(b2, 2);

		  b3 = newBuffer();
/* basically a lvalue could be used here rather than an ident !!! */
/* probably by using reg1 for the test rather than reg0 */
		  bufferWriteString(b3, (yyvsp[(3) - (7)].str), strlen((yyvsp[(3) - (7)].str))+1);
		  bufferWriteRegister(b3, 0);
		  bufferWriteOp(b3, SWFACTION_SETVARIABLE);
		  bufferConcat(b3, (yyvsp[(7) - (7)].action));
		  bufferWriteS16(b2, bufferLength(b3) + 5);
		  tmp = bufferLength(b2) + bufferLength(b3) + 5;
		  bufferWriteOp(b3, SWFACTION_JUMP);
		  bufferWriteS16(b3, 2);
		  bufferWriteS16(b3, -tmp);
		  bufferConcat(b2, b3);
		  bufferResolveJumps(b2);
		  bufferConcat((yyval.action), b2);
		  delctx(CTX_FOR_IN);
		  free((yyvsp[(3) - (7)].str)); }
    break;

  case 197:

    { Buffer b2, b3;
		  int tmp;

		  (yyval.action) = (yyvsp[(5) - (8)].action);
		  if((yyvsp[(5) - (8)].action)->hasObject)
			bufferWriteOp((yyval.action), SWFACTION_ENUMERATE2);        
		  else
			bufferWriteOp((yyval.action), SWFACTION_ENUMERATE); 

		  b2 = newBuffer();
		  bufferWriteSetRegister(b2, 0);
		  bufferWriteOp(b2, SWFACTION_PUSH);
		  bufferWriteS16(b2, 1);
		  bufferWriteU8(b2, 2);
		  bufferWriteOp(b2, SWFACTION_EQUALS2);
		  bufferWriteOp(b2, SWFACTION_IF);
		  bufferWriteS16(b2, 2);
		  /* add size later */

		  b3 = newBuffer();
		  bufferWriteString(b3, (yyvsp[(4) - (8)].str), strlen((yyvsp[(4) - (8)].str))+1);
		  bufferWriteRegister(b3, 0);
		  bufferWriteOp(b3, SWFACTION_DEFINELOCAL);
		  bufferConcat(b3, (yyvsp[(8) - (8)].action));
		  bufferWriteS16(b2, bufferLength(b3) + 5);
		  tmp = bufferLength(b2) + bufferLength(b3) + 5;
		  bufferWriteOp(b3, SWFACTION_JUMP);
		  bufferWriteS16(b3, 2);
		  bufferWriteS16(b3, -tmp);
		  bufferConcat(b2, b3);
		  bufferResolveJumps(b2);
		  bufferConcat((yyval.action), b2);
		  delctx(CTX_FOR_IN);
		  free((yyvsp[(4) - (8)].str)); }
    break;

  case 198:

    { (yyval.action) = NULL; }
    break;

  case 200:

    { 
		  if(chkctx(CTX_CONTINUE) < 0)
		  {
			swf5error("continue outside loop");
			YYABORT;
		  }
		  (yyval.action) = newBuffer();
		  bufferWriteOp((yyval.action), SWFACTION_JUMP);
		  bufferWriteS16((yyval.action), 2);
		  bufferWriteS16((yyval.action), MAGIC_CONTINUE_NUMBER); }
    break;

  case 201:

    { int context = chkctx(CTX_BREAK);
		  (yyval.action) = newBuffer();
		  if(context == CTX_FOR_IN || context == CTX_LOOP)
		  {
		  	if(context == CTX_FOR_IN)	/* break out of a for .. in */
				bufferWriteOp((yyval.action), SWFACTION_POP);
		  	bufferWriteOp((yyval.action), SWFACTION_JUMP);
		  	bufferWriteS16((yyval.action), 2);
		  	bufferWriteS16((yyval.action), MAGIC_BREAK_NUMBER); 
		  }
		  else if(context == CTX_SWITCH)
		  {
			addctx(CTX_BREAK);	
		  }
		  else
		  {
			swf5error("break outside switch / loop");
			YYABORT;
		  }
		}
    break;

  case 202:

    { (yyval.getURLMethod) = GETURL_METHOD_NOSEND; }
    break;

  case 203:

    { (yyval.getURLMethod) = GETURL_METHOD_GET; }
    break;

  case 204:

    { (yyval.getURLMethod) = GETURL_METHOD_POST; }
    break;

  case 205:

    { if(strcasecmp((yyvsp[(2) - (2)].str), "GET") == 0)
				    (yyval.getURLMethod) = GETURL_METHOD_GET;
				  else if(strcasecmp((yyvsp[(2) - (2)].str), "POST") == 0)
				    (yyval.getURLMethod) = GETURL_METHOD_POST;
				  else (yyval.getURLMethod) = GETURL_METHOD_NOSEND;
				}
    break;

  case 206:

    { (yyval.action) = newBuffer();
		  bufferWriteString((yyval.action), "_level", 7);
		  bufferConcat((yyval.action), (yyvsp[(1) - (1)].action));
		  bufferWriteOp((yyval.action), SWFACTION_STRINGCONCAT); }
    break;

  case 207:

    { (yyval.action) = (yyvsp[(3) - (4)].action);
		  bufferWriteOp((yyval.action), SWFACTION_TRACE); }
    break;

  case 208:

    {
#ifdef DEBUG
		  printf("void_function_call: GETURL '(' expr ')'\n");
#endif
		  (yyval.action) = (yyvsp[(3) - (4)].action);
		  bufferWriteOp((yyval.action), SWFACTION_PUSH);
		  bufferWriteS16((yyval.action), 2); bufferWriteS16((yyval.action), 0); /* two 0 bytes */
		  bufferWriteOp((yyval.action), SWFACTION_GETURL2);
		  bufferWriteS16((yyval.action), 1);
		  bufferWriteU8((yyval.action), 0); }
    break;

  case 209:

    {
#ifdef DEBUG
		  printf("void_function_call: GETURL '(' expr ',' expr urlmethod ')'\n");
#endif
		  (yyval.action) = (yyvsp[(3) - (7)].action);
		  bufferConcat((yyval.action), (yyvsp[(5) - (7)].action));
		  bufferWriteOp((yyval.action), SWFACTION_GETURL2);
		  bufferWriteS16((yyval.action), 1);
		  bufferWriteU8((yyval.action), (yyvsp[(6) - (7)].getURLMethod)); }
    break;

  case 210:

    { (yyval.action) = (yyvsp[(3) - (7)].action);
		  bufferConcat((yyval.action), (yyvsp[(5) - (7)].action));
		  bufferWriteOp((yyval.action), SWFACTION_GETURL2);
		  bufferWriteS16((yyval.action), 1);
		  bufferWriteU8((yyval.action), 0xc0+(yyvsp[(6) - (7)].getURLMethod)); }
    break;

  case 211:

    { (yyval.action) = (yyvsp[(3) - (7)].action);
		  bufferConcat((yyval.action), (yyvsp[(5) - (7)].action));
		  bufferWriteOp((yyval.action), SWFACTION_GETURL2);
		  bufferWriteS16((yyval.action), 1);
		  bufferWriteU8((yyval.action), 0x80+(yyvsp[(6) - (7)].getURLMethod)); }
    break;

  case 212:

    { (yyval.action) = (yyvsp[(3) - (7)].action);
		  bufferConcat((yyval.action), (yyvsp[(5) - (7)].action));
		  bufferWriteOp((yyval.action), SWFACTION_GETURL2);
		  bufferWriteS16((yyval.action), 1);
		  bufferWriteU8((yyval.action), 0x40+(yyvsp[(6) - (7)].getURLMethod)); }
    break;

  case 213:

    { (yyval.action) = (yyvsp[(3) - (7)].action);
		  bufferConcat((yyval.action), (yyvsp[(5) - (7)].action));
		  bufferWriteOp((yyval.action), SWFACTION_GETURL2);
		  bufferWriteS16((yyval.action), 1);
		  bufferWriteU8((yyval.action), (yyvsp[(6) - (7)].getURLMethod)); }
    break;

  case 214:

    { (yyval.action) = (yyvsp[(3) - (4)].action);
		  bufferWriteOp((yyval.action), SWFACTION_CALLFRAME);
		  bufferWriteS16((yyval.action), 0); }
    break;

  case 215:

    { (yyval.action) = newBuffer();
		  bufferWriteString((yyval.action), "0", 2); /* no constraint */
		  bufferConcat((yyval.action), (yyvsp[(5) - (6)].action));
		  bufferConcat((yyval.action), (yyvsp[(3) - (6)].action));
		  bufferWriteOp((yyval.action), SWFACTION_STARTDRAG); }
    break;

  case 216:

    { (yyval.action) = newBuffer();
		  bufferConcat((yyval.action), (yyvsp[(7) - (14)].action));
		  bufferConcat((yyval.action), (yyvsp[(11) - (14)].action));
		  bufferConcat((yyval.action), (yyvsp[(9) - (14)].action));
		  bufferConcat((yyval.action), (yyvsp[(13) - (14)].action));
		  bufferWriteString((yyval.action), "1", 2); /* has constraint */
		  bufferConcat((yyval.action), (yyvsp[(5) - (14)].action));
		  bufferConcat((yyval.action), (yyvsp[(3) - (14)].action));
		  bufferWriteOp((yyval.action), SWFACTION_STARTDRAG); }
    break;

  case 217:

    { (yyval.action) = newBuffer();
		  bufferWriteOp((yyval.action), SWFACTION_ENDDRAG); }
    break;

  case 218:

    { (yyval.action) = (yyvsp[(3) - (8)].action);
		  bufferConcat((yyval.action), (yyvsp[(5) - (8)].action));
		  bufferConcat((yyval.action), (yyvsp[(7) - (8)].action));
		  bufferWriteInt((yyval.action), 16384); /* magic number */
		  bufferWriteOp((yyval.action), SWFACTION_ADD);
		  bufferWriteOp((yyval.action), SWFACTION_DUPLICATECLIP); }
    break;

  case 219:

    { (yyval.action) = (yyvsp[(3) - (4)].action);
		  bufferWriteOp((yyval.action), SWFACTION_REMOVECLIP); }
    break;

  case 220:

    { (yyval.action) = newBuffer();
		  bufferWriteOp((yyval.action), SWFACTION_GETURL);
		  bufferWriteS16((yyval.action), (int) (strlen((yyvsp[(3) - (6)].str)) + strlen((yyvsp[(5) - (6)].str)) + 2));
		  bufferWriteHardString((yyval.action), (yyvsp[(3) - (6)].str), strlen((yyvsp[(3) - (6)].str)));
		  bufferWriteU8((yyval.action), 0);
		  bufferWriteHardString((yyval.action), (yyvsp[(5) - (6)].str), strlen((yyvsp[(5) - (6)].str)));
		  bufferWriteU8((yyval.action), 0); }
    break;

  case 221:

    { (yyval.action) = newBuffer();
		  bufferWriteOp((yyval.action), SWFACTION_NEXTFRAME); }
    break;

  case 222:

    { (yyval.action) = newBuffer();
		  bufferWriteOp((yyval.action), SWFACTION_PREVFRAME); }
    break;

  case 223:

    { (yyval.action) = newBuffer();
		  bufferWriteOp((yyval.action), SWFACTION_PLAY); }
    break;

  case 224:

    { (yyval.action) = newBuffer();
		  bufferWriteOp((yyval.action), SWFACTION_STOP); }
    break;

  case 225:

    { (yyval.action) = newBuffer();
		  bufferWriteOp((yyval.action), SWFACTION_STOPSOUNDS); }
    break;

  case 226:

    { (yyval.action) = newBuffer();
		  bufferWriteOp((yyval.action), SWFACTION_TOGGLEQUALITY); }
    break;

  case 227:

    { (yyval.action) = newBuffer();
		  bufferWriteOp((yyval.action), SWFACTION_GOTOFRAME);
		  bufferWriteS16((yyval.action), 2);
		  bufferWriteS16((yyval.action), (yyvsp[(3) - (4)].intVal)-1); /* GOTOFRAME target is 0-based */
		  bufferWriteOp((yyval.action), SWFACTION_PLAY); }
    break;

  case 228:

    { (yyval.action) = newBuffer();
		  bufferWriteOp((yyval.action), SWFACTION_GOTOFRAME);
		  bufferWriteS16((yyval.action), 2);
		  bufferWriteS16((yyval.action), (yyvsp[(3) - (4)].intVal)-1);  /* GOTOFRAME target is 0-based */
		  bufferWriteOp((yyval.action), SWFACTION_STOP); }
    break;

  case 229:

    { (yyval.action) = newBuffer();
		  bufferWriteOp((yyval.action), SWFACTION_GOTOLABEL);
		  bufferWriteS16((yyval.action), strlen((yyvsp[(3) - (4)].str))+1);
		  bufferWriteHardString((yyval.action), (yyvsp[(3) - (4)].str), strlen((yyvsp[(3) - (4)].str))+1);
		  free((yyvsp[(3) - (4)].str)); 
		  bufferWriteOp((yyval.action), SWFACTION_PLAY); }
    break;

  case 230:

    { (yyval.action) = newBuffer();
		  bufferWriteOp((yyval.action), SWFACTION_GOTOLABEL);
		  bufferWriteS16((yyval.action), strlen((yyvsp[(3) - (4)].str))+1);
		  bufferWriteHardString((yyval.action), (yyvsp[(3) - (4)].str), strlen((yyvsp[(3) - (4)].str))+1);
		  free((yyvsp[(3) - (4)].str)); 
		  bufferWriteOp((yyval.action), SWFACTION_STOP); }
    break;

  case 231:

    { (yyval.action) = (yyvsp[(3) - (4)].action);
		  bufferWriteOp((yyval.action), SWFACTION_GOTOFRAME2);
		  bufferWriteS16((yyval.action), 1);
		  bufferWriteU8((yyval.action), 1); }
    break;

  case 232:

    { (yyval.action) = (yyvsp[(3) - (4)].action);
		  bufferWriteOp((yyval.action), SWFACTION_GOTOFRAME2);
		  bufferWriteS16((yyval.action), 1);
		  bufferWriteU8((yyval.action), 0); }
    break;

  case 233:

    { (yyval.action) = newBuffer();
		  bufferWriteOp((yyval.action), SWFACTION_SETTARGET);
		  bufferWriteS16((yyval.action), (int) (strlen((yyvsp[(3) - (4)].str))+1));
		  bufferWriteHardString((yyval.action), (yyvsp[(3) - (4)].str), strlen((yyvsp[(3) - (4)].str))+1);
		  free((yyvsp[(3) - (4)].str)); }
    break;

  case 234:

    { (yyval.action) = (yyvsp[(3) - (4)].action);
		  bufferWriteOp((yyval.action), SWFACTION_SETTARGET2); }
    break;

  case 235:

    {
			(yyval.action) = (yyvsp[(3) - (8)].action);
			bufferWriteFloat((yyval.action), (yyvsp[(5) - (8)].intVal));
			bufferConcat((yyval.action), (yyvsp[(7) - (8)].action));
			bufferWriteOp((yyval.action), SWFACTION_SETPROPERTY);	
		}
    break;

  case 236:

    {
#ifdef DEBUG
		  printf("function_call: %s '(' expr_list ')'\n", (yyvsp[(1) - (4)].str));
#endif
		  (yyval.action) = (yyvsp[(3) - (4)].exprlist).buffer;
		  bufferWriteInt((yyval.action), (yyvsp[(3) - (4)].exprlist).count);
		  bufferWriteString((yyval.action), (yyvsp[(1) - (4)].str), strlen((yyvsp[(1) - (4)].str))+1);
		  bufferWriteOp((yyval.action), SWFACTION_CALLFUNCTION);
		  free((yyvsp[(1) - (4)].str)); }
    break;

  case 237:

    { (yyval.action) = newBuffer();
		  bufferWriteString((yyval.action), (yyvsp[(3) - (4)].str), strlen((yyvsp[(3) - (4)].str))+1);
		  free((yyvsp[(3) - (4)].str));
		  bufferWriteOp((yyval.action), SWFACTION_GETVARIABLE); 
		  bufferWriteOp((yyval.action), SWFACTION_TARGETPATH); }
    break;

  case 238:

    { (yyval.action) = (yyvsp[(3) - (4)].action);
		  bufferWriteOp((yyval.action), SWFACTION_GETVARIABLE); }
    break;

  case 239:

    { (yyval.action) = newBuffer();
		  bufferWriteOp((yyval.action), SWFACTION_GETTIME); }
    break;

  case 240:

    { (yyval.action) = (yyvsp[(3) - (4)].action);
		  bufferWriteOp((yyval.action), SWFACTION_RANDOMNUMBER); }
    break;

  case 241:

    { (yyval.action) = (yyvsp[(3) - (4)].action);
		  bufferWriteOp((yyval.action), SWFACTION_STRINGLENGTH); }
    break;

  case 242:

    { (yyval.action) = (yyvsp[(3) - (4)].action);
		  bufferWriteOp((yyval.action), SWFACTION_INT); }
    break;

  case 243:

    { (yyval.action) = (yyvsp[(3) - (4)].action);
		  bufferWriteOp((yyval.action), SWFACTION_ORD); }
    break;

  case 244:

    { (yyval.action) = (yyvsp[(3) - (4)].action);
		  bufferWriteOp((yyval.action), SWFACTION_CHR); }
    break;

  case 245:

    { (yyval.action) = (yyvsp[(3) - (6)].action);
		  bufferConcat((yyval.action), (yyvsp[(5) - (6)].action));
		  bufferWriteOp((yyval.action), SWFACTION_STRINGCONCAT); }
    break;

  case 246:

    { (yyval.action) = (yyvsp[(3) - (8)].action);
		  bufferConcat((yyval.action), (yyvsp[(5) - (8)].action));
		  bufferConcat((yyval.action), (yyvsp[(7) - (8)].action));
		  bufferWriteOp((yyval.action), SWFACTION_SUBSTRING); }
    break;

  case 247:

    {
#ifdef DEBUG
		  printf("function_call: TYPEOF '(' expr_or_obj ')'\n");
#endif
		  (yyval.action) = (yyvsp[(3) - (4)].action);
		  bufferWriteOp((yyval.action), SWFACTION_TYPEOF); }
    break;

  case 248:

    { (yyval.action) = newBuffer();
		  bufferConcat((yyval.action), (yyvsp[(3) - (6)].action));
		  bufferWriteFloat((yyval.action), (yyvsp[(5) - (6)].intVal));
		  bufferWriteOp((yyval.action), SWFACTION_GETPROPERTY);
		}
    break;

  case 249:

    { (yyval.action) = (yyvsp[(2) - (2)].action);
		  if((yyvsp[(2) - (2)].action)->hasObject)
		    bufferWriteOp((yyval.action), SWFACTION_DELETE);
		  else 
		    bufferWriteOp((yyval.action), SWFACTION_DELETE2);
		}
    break;

  case 250:

    { (yyval.exprlist).buffer = newBuffer();
		  (yyval.exprlist).count = 0; }
    break;

  case 251:

    { (yyval.exprlist).buffer = (yyvsp[(1) - (1)].action);
		  (yyval.exprlist).count = 1; }
    break;

  case 252:

    { Buffer tmp = newBuffer();
		  bufferConcat(tmp, (yyvsp[(3) - (3)].action));
		  bufferConcat(tmp, (yyval.exprlist).buffer);
		  (yyval.exprlist).buffer = tmp;
		  ++(yyval.exprlist).count;  }
    break;

  case 253:

    { (yyval.action) = (yyvsp[(5) - (6)].exprlist).buffer;
		  bufferWriteInt((yyval.action), (yyvsp[(5) - (6)].exprlist).count);
		  bufferConcat((yyval.action), (yyvsp[(1) - (6)].action));
		  bufferWriteString((yyval.action), (yyvsp[(3) - (6)].str), strlen((yyvsp[(3) - (6)].str))+1);
		  bufferWriteOp((yyval.action), SWFACTION_CALLMETHOD);
		  free((yyvsp[(3) - (6)].str)); }
    break;

  case 254:

    { (yyval.action) = (yyvsp[(6) - (7)].exprlist).buffer;
		  bufferWriteInt((yyval.action), (yyvsp[(6) - (7)].exprlist).count);
		  bufferConcat((yyval.action), (yyvsp[(1) - (7)].action));
		  bufferConcat((yyval.action), (yyvsp[(3) - (7)].action));
		  bufferWriteOp((yyval.action), SWFACTION_CALLMETHOD); }
    break;

  case 255:

    { (yyval.action) = newBuffer();
		  bufferWriteString((yyval.action), (yyvsp[(1) - (3)].str), strlen((yyvsp[(1) - (3)].str))+1);
		  bufferConcat((yyval.action), (yyvsp[(3) - (3)].action));
		  free((yyvsp[(1) - (3)].str)); }
    break;

  case 256:

    { (yyval.exprlist).buffer = (yyvsp[(1) - (1)].action);
		  (yyval.exprlist).count = 1; }
    break;

  case 257:

    { bufferConcat((yyval.exprlist).buffer, (yyvsp[(3) - (3)].action));
		  ++(yyval.exprlist).count;  }
    break;

  case 258:

    { (yyval.op) = SWFACTION_ADD2; }
    break;

  case 259:

    { (yyval.op) = SWFACTION_SUBTRACT; }
    break;

  case 260:

    { (yyval.op) = SWFACTION_MULTIPLY; }
    break;

  case 261:

    { (yyval.op) = SWFACTION_DIVIDE; }
    break;

  case 262:

    { (yyval.op) = SWFACTION_MODULO; }
    break;

  case 263:

    { (yyval.op) = SWFACTION_BITWISEAND; }
    break;

  case 264:

    { (yyval.op) = SWFACTION_BITWISEOR; }
    break;

  case 265:

    { (yyval.op) = SWFACTION_BITWISEXOR; }
    break;

  case 266:

    { (yyval.op) = SWFACTION_SHIFTLEFT; }
    break;

  case 267:

    { (yyval.op) = SWFACTION_SHIFTRIGHT; }
    break;

  case 268:

    { (yyval.op) = SWFACTION_SHIFTRIGHT2; }
    break;

  case 269:

    { (yyval.op) = SWFACTION_INCREMENT; }
    break;

  case 270:

    { (yyval.op) = SWFACTION_DECREMENT; }
    break;

  case 271:

    { if((yyvsp[(1) - (1)].lval).obj)
		  {
		    (yyval.action) = (yyvsp[(1) - (1)].lval).obj;
		    if((yyvsp[(1) - (1)].lval).ident)
		      bufferConcat((yyval.action), (yyvsp[(1) - (1)].lval).ident);
		    else
		      bufferConcat((yyval.action), (yyvsp[(1) - (1)].lval).memexpr);

		    bufferWriteOp((yyval.action), SWFACTION_GETMEMBER);
		  }
		  else
		  {
		    (yyval.action) = (yyvsp[(1) - (1)].lval).ident;
		    bufferWriteOp((yyval.action), SWFACTION_GETVARIABLE);
		  }
		}
    break;

  case 274:

    { (yyval.lval).ident = newBuffer();
		  bufferWriteString((yyval.lval).ident, (yyvsp[(1) - (1)].str), strlen((yyvsp[(1) - (1)].str))+1);
		  free((yyvsp[(1) - (1)].str));
		  (yyval.lval).obj = 0;
		  (yyval.lval).memexpr = 0; }
    break;

  case 275:

    { (yyval.lval).obj = (yyvsp[(1) - (3)].action);
		  (yyval.lval).ident = newBuffer();
		  bufferWriteString((yyval.lval).ident, (yyvsp[(3) - (3)].str), strlen((yyvsp[(3) - (3)].str))+1);
		  free((yyvsp[(3) - (3)].str));
		  (yyval.lval).memexpr = 0; }
    break;

  case 276:

    { (yyval.lval).obj = (yyvsp[(1) - (4)].action);
		  (yyval.lval).memexpr = (yyvsp[(3) - (4)].action);
		  (yyval.lval).ident = 0; }
    break;

  case 279:

    { (yyval.action) = (yyvsp[(2) - (2)].action);
		  bufferWriteInt((yyvsp[(2) - (2)].action), -1);
		  bufferWriteOp((yyvsp[(2) - (2)].action), SWFACTION_MULTIPLY); }
    break;

  case 280:

    { (yyval.action) = (yyvsp[(2) - (2)].action);
		  bufferWriteInt((yyvsp[(2) - (2)].action), 0xffffffff);
		  bufferWriteOp((yyvsp[(2) - (2)].action), SWFACTION_BITWISEXOR); }
    break;

  case 281:

    { (yyval.action) = (yyvsp[(2) - (2)].action);
		  bufferWriteOp((yyvsp[(2) - (2)].action), SWFACTION_LOGICALNOT); }
    break;

  case 282:

    { (yyval.action) = (yyvsp[(1) - (3)].action);
		  bufferWriteOp((yyval.action), SWFACTION_PUSHDUP);
		  bufferWriteOp((yyval.action), SWFACTION_LOGICALNOT);
		  bufferWriteOp((yyval.action), SWFACTION_IF);
		  bufferWriteS16((yyval.action), 2);
		  bufferWriteS16((yyval.action), bufferLength((yyvsp[(3) - (3)].action))+1);
		  bufferWriteOp((yyval.action), SWFACTION_POP);
		  bufferConcatSimple((yyval.action), (yyvsp[(3) - (3)].action)); }
    break;

  case 283:

    { (yyval.action) = (yyvsp[(1) - (3)].action);
		  bufferWriteOp((yyval.action), SWFACTION_PUSHDUP);
		  bufferWriteOp((yyval.action), SWFACTION_IF);
		  bufferWriteS16((yyval.action), 2);
		  bufferWriteS16((yyval.action), bufferLength((yyvsp[(3) - (3)].action))+1);
		  bufferWriteOp((yyval.action), SWFACTION_POP);
		  bufferConcatSimple((yyval.action), (yyvsp[(3) - (3)].action)); }
    break;

  case 284:

    { (yyval.action) = (yyvsp[(1) - (3)].action);
		  bufferConcat((yyval.action), (yyvsp[(3) - (3)].action));
		  bufferWriteOp((yyval.action), SWFACTION_MULTIPLY); }
    break;

  case 285:

    { (yyval.action) = (yyvsp[(1) - (3)].action);
		  bufferConcat((yyval.action), (yyvsp[(3) - (3)].action));
		  bufferWriteOp((yyval.action), SWFACTION_DIVIDE); }
    break;

  case 286:

    { (yyval.action) = (yyvsp[(1) - (3)].action);
		  bufferConcat((yyval.action), (yyvsp[(3) - (3)].action));
		  bufferWriteOp((yyval.action), SWFACTION_MODULO); }
    break;

  case 287:

    { (yyval.action) = (yyvsp[(1) - (3)].action);
		  bufferConcat((yyval.action), (yyvsp[(3) - (3)].action));
		  bufferWriteOp((yyval.action), SWFACTION_ADD2); }
    break;

  case 288:

    { (yyval.action) = (yyvsp[(1) - (3)].action);
		  bufferConcat((yyval.action), (yyvsp[(3) - (3)].action));
		  bufferWriteOp((yyval.action), SWFACTION_SUBTRACT); }
    break;

  case 289:

    { (yyval.action) = (yyvsp[(1) - (3)].action);
		  bufferConcat((yyval.action), (yyvsp[(3) - (3)].action));
		  bufferWriteOp((yyval.action), SWFACTION_BITWISEAND); }
    break;

  case 290:

    { (yyval.action) = (yyvsp[(1) - (3)].action);
		  bufferConcat((yyval.action), (yyvsp[(3) - (3)].action));
		  bufferWriteOp((yyval.action), SWFACTION_BITWISEOR); }
    break;

  case 291:

    { (yyval.action) = (yyvsp[(1) - (3)].action);
		  bufferConcat((yyval.action), (yyvsp[(3) - (3)].action));
		  bufferWriteOp((yyval.action), SWFACTION_BITWISEXOR); }
    break;

  case 292:

    { (yyval.action) = (yyvsp[(1) - (3)].action);
		  bufferConcat((yyval.action), (yyvsp[(3) - (3)].action));
		  bufferWriteOp((yyval.action), SWFACTION_LESS2); }
    break;

  case 293:

    { (yyval.action) = (yyvsp[(3) - (3)].action);
		  bufferConcat((yyval.action), (yyvsp[(1) - (3)].action));
		  bufferWriteOp((yyval.action), SWFACTION_LESS2); }
    break;

  case 294:

    { (yyval.action) = (yyvsp[(3) - (3)].action);
		  bufferConcat((yyval.action), (yyvsp[(1) - (3)].action));
		  bufferWriteOp((yyval.action), SWFACTION_LESS2);
		  bufferWriteOp((yyval.action), SWFACTION_LOGICALNOT); }
    break;

  case 295:

    { bufferConcat((yyvsp[(1) - (3)].action), (yyvsp[(3) - (3)].action));
		  bufferWriteOp((yyvsp[(1) - (3)].action), SWFACTION_LESS2);
		  bufferWriteOp((yyvsp[(1) - (3)].action), SWFACTION_LOGICALNOT); }
    break;

  case 296:

    { bufferConcat((yyvsp[(1) - (3)].action), (yyvsp[(3) - (3)].action));
		  bufferWriteOp((yyvsp[(1) - (3)].action), SWFACTION_EQUALS2); }
    break;

  case 297:

    { bufferConcat((yyvsp[(1) - (3)].action), (yyvsp[(3) - (3)].action));
		  bufferWriteOp((yyvsp[(1) - (3)].action), SWFACTION_STRICTEQUALS); }
    break;

  case 298:

    { bufferConcat((yyvsp[(1) - (3)].action), (yyvsp[(3) - (3)].action));
		  bufferWriteOp((yyvsp[(1) - (3)].action), SWFACTION_EQUALS2);
		  bufferWriteOp((yyvsp[(1) - (3)].action), SWFACTION_LOGICALNOT); }
    break;

  case 299:

    { bufferConcat((yyvsp[(1) - (3)].action), (yyvsp[(3) - (3)].action));
		  bufferWriteOp((yyvsp[(1) - (3)].action), SWFACTION_STRICTEQUALS); 
		  bufferWriteOp((yyvsp[(1) - (3)].action), SWFACTION_LOGICALNOT); }
    break;

  case 300:

    { bufferConcat((yyvsp[(1) - (3)].action), (yyvsp[(3) - (3)].action));
		  bufferWriteOp((yyvsp[(1) - (3)].action), SWFACTION_SHIFTLEFT); }
    break;

  case 301:

    { bufferConcat((yyvsp[(1) - (3)].action), (yyvsp[(3) - (3)].action));
		  bufferWriteOp((yyvsp[(1) - (3)].action), SWFACTION_SHIFTRIGHT); }
    break;

  case 302:

    { bufferConcat((yyvsp[(1) - (3)].action), (yyvsp[(3) - (3)].action));
		  bufferWriteOp((yyvsp[(1) - (3)].action), SWFACTION_SHIFTRIGHT2); }
    break;

  case 303:

    { bufferWriteOp((yyvsp[(1) - (5)].action), SWFACTION_IF);
		  bufferWriteS16((yyvsp[(1) - (5)].action), 2);
		  bufferWriteS16((yyvsp[(1) - (5)].action), bufferLength((yyvsp[(5) - (5)].action))+5);
		  bufferConcatSimple((yyvsp[(1) - (5)].action), (yyvsp[(5) - (5)].action));
		  bufferWriteOp((yyvsp[(1) - (5)].action), SWFACTION_JUMP);
		  bufferWriteS16((yyvsp[(1) - (5)].action), 2);
		  bufferWriteS16((yyvsp[(1) - (5)].action), bufferLength((yyvsp[(3) - (5)].action)));
		  bufferConcatSimple((yyvsp[(1) - (5)].action), (yyvsp[(3) - (5)].action)); }
    break;

  case 304:

    { if((yyvsp[(1) - (3)].lval).obj) /* obj[memexpr] or obj.ident */
		  {
		    (yyval.action) = (yyvsp[(1) - (3)].lval).obj;

		    if((yyvsp[(1) - (3)].lval).ident)
		      bufferConcat((yyval.action), (yyvsp[(1) - (3)].lval).ident);
		    else
		      bufferConcat((yyval.action), (yyvsp[(1) - (3)].lval).memexpr);

		    bufferConcat((yyval.action), (yyvsp[(3) - (3)].action));
		    bufferWriteSetRegister((yyval.action), 0);
		    bufferWriteOp((yyval.action), SWFACTION_SETMEMBER);
		    bufferWriteRegister((yyval.action), 0);
		  }
		  else /* just ident */
		  {
		    (yyval.action) = (yyvsp[(3) - (3)].action);
		    bufferWriteOp((yyval.action), SWFACTION_PUSHDUP);
		    bufferConcat((yyval.action), (yyvsp[(1) - (3)].lval).ident);
		    bufferWriteOp((yyval.action), SWFACTION_STACKSWAP);
		    bufferWriteOp((yyval.action), SWFACTION_SETVARIABLE);
		  }
/* tricky case missing here: lvalue ASSIGN expr */
/* like in x = y += z; */
		}
    break;

  case 305:

    { (yyval.action) = (yyvsp[(1) - (3)].action);
		  bufferConcat((yyval.action), (yyvsp[(3) - (3)].action));
		  bufferWriteOp((yyval.action), SWFACTION_INSTANCEOF); }
    break;

  case 307:

    {
#ifdef DEBUG
		  printf("NEW %s\n", (yyvsp[(2) - (2)].str));
#endif
		  (yyval.action) = newBuffer();
		  bufferWriteInt((yyval.action), 0);
		  bufferWriteString((yyval.action), (yyvsp[(2) - (2)].str), strlen((yyvsp[(2) - (2)].str))+1);
		  free((yyvsp[(2) - (2)].str));
		  bufferWriteOp((yyval.action), SWFACTION_NEWOBJECT); }
    break;

  case 308:

    {
#ifdef DEBUG
		  printf("NEW %s '(' expr_list ')'\n", (yyvsp[(2) - (5)].str));
#endif
		  (yyval.action) = (yyvsp[(4) - (5)].exprlist).buffer;
		  bufferWriteInt((yyval.action), (yyvsp[(4) - (5)].exprlist).count);
		  bufferWriteString((yyval.action), (yyvsp[(2) - (5)].str), strlen((yyvsp[(2) - (5)].str))+1);
		  free((yyvsp[(2) - (5)].str));
		  bufferWriteOp((yyval.action), SWFACTION_NEWOBJECT); }
    break;

  case 309:

    {
#ifdef DEBUG
		  printf("NEW lvalue_expr '.' %s\n", (yyvsp[(4) - (4)].str));
#endif
		  (yyval.action) = newBuffer();
		  bufferWriteInt((yyval.action), 0);
		  bufferConcat((yyval.action), (yyvsp[(2) - (4)].action));
		  bufferWriteString((yyval.action), (yyvsp[(4) - (4)].str), strlen((yyvsp[(4) - (4)].str))+1);
		  free((yyvsp[(4) - (4)].str));
		  bufferWriteOp((yyval.action), SWFACTION_NEWMETHOD); }
    break;

  case 310:

    {
#ifdef DEBUG
		  printf("NEW lvalue_expr '[' expr ']'\n");
#endif
		  (yyval.action) = newBuffer();
		  bufferWriteInt((yyval.action), 0);
		  bufferConcat((yyval.action), (yyvsp[(2) - (5)].action));
		  bufferConcat((yyval.action), (yyvsp[(4) - (5)].action));
		  bufferWriteOp((yyval.action), SWFACTION_NEWMETHOD); }
    break;

  case 311:

    {
#ifdef DEBUG
		  printf("NEW lvalue_expr '.' %s '(' expr_list ')'\n", (yyvsp[(4) - (7)].str));
#endif
		  (yyval.action) = (yyvsp[(6) - (7)].exprlist).buffer;
		  bufferWriteInt((yyval.action), (yyvsp[(6) - (7)].exprlist).count);
		  bufferConcat((yyval.action), (yyvsp[(2) - (7)].action));
		  bufferWriteString((yyval.action), (yyvsp[(4) - (7)].str), strlen((yyvsp[(4) - (7)].str))+1);
		  free((yyvsp[(4) - (7)].str));
		  bufferWriteOp((yyval.action), SWFACTION_NEWMETHOD); }
    break;

  case 312:

    {
#ifdef DEBUG
		  printf("NEW lvalue_expr '[' expr ']' '(' expr_list ')'\n");
#endif
		  (yyval.action) = (yyvsp[(7) - (8)].exprlist).buffer;
		  bufferWriteInt((yyval.action), (yyvsp[(7) - (8)].exprlist).count);
		  bufferConcat((yyval.action), (yyvsp[(2) - (8)].action));
		  bufferConcat((yyval.action), (yyvsp[(4) - (8)].action));
		  bufferWriteOp((yyval.action), SWFACTION_NEWMETHOD); }
    break;

  case 313:

    { (yyval.action) = (yyvsp[(2) - (3)].exprlist).buffer;
		  bufferWriteInt((yyval.action), (yyvsp[(2) - (3)].exprlist).count);
		  bufferWriteOp((yyval.action), SWFACTION_INITARRAY); }
    break;

  case 314:

    { (yyval.action) = newBuffer();
		  bufferWriteInt((yyval.action), 0);
		  bufferWriteOp((yyval.action), SWFACTION_INITOBJECT); }
    break;

  case 315:

    { (yyval.action) = (yyvsp[(2) - (3)].exprlist).buffer;
		  bufferWriteInt((yyval.action), (yyvsp[(2) - (3)].exprlist).count);
		  bufferWriteOp((yyval.action), SWFACTION_INITOBJECT); }
    break;

  case 316:

    {
			if((yyvsp[(1) - (1)].function)->name != NULL)
			{
				swf5error("anonymous decl only. identifier not allowed");
				YYABORT;
			}
			(yyval.action) = newBuffer();
			if(swfVersion > 6)
				bufferWriteFunction((yyval.action), (yyvsp[(1) - (1)].function), 2);
			else
				bufferWriteFunction((yyval.action), (yyvsp[(1) - (1)].function), 1);
		}
    break;

  case 319:

    { if((yyvsp[(2) - (2)].lval).obj)
		  {
		    if((yyvsp[(2) - (2)].lval).ident)	/* expr . identifier */
		    {
		      (yyval.action) = (yyvsp[(2) - (2)].lval).obj;
		      bufferWriteOp((yyval.action), SWFACTION_PUSHDUP);	      /* a, a */
		      bufferWriteBuffer((yyval.action), (yyvsp[(2) - (2)].lval).ident);        /* a, a, i */
		      bufferWriteOp((yyval.action), SWFACTION_STACKSWAP);      /* a, i, a */
		      bufferConcat((yyval.action), (yyvsp[(2) - (2)].lval).ident);             /* a, i, a, i */
		      bufferWriteOp((yyval.action), SWFACTION_GETMEMBER);
		      bufferWriteOp((yyval.action), (yyvsp[(1) - (2)].op));
		      bufferWriteSetRegister((yyval.action), 0);
		      bufferWriteOp((yyval.action), SWFACTION_SETMEMBER); /* a.i = a.i+1 */
		      bufferWriteRegister((yyval.action), 0);	      /* a.i+1 */
		    }
		    else	/* expr [ expr ] */
		    {
		      (yyval.action) = (yyvsp[(2) - (2)].lval).memexpr;			      /* i */
		      bufferConcat((yyval.action), (yyvsp[(2) - (2)].lval).obj);		      /* i, a */
		      bufferWriteSetRegister((yyval.action), 0);	/* ($2.memexpr can use reg0) */
		      bufferWriteOp((yyval.action), SWFACTION_STACKSWAP);      /* a, i */
		      bufferWriteOp((yyval.action), SWFACTION_PUSHDUP);	      /* a, i, i */
		      bufferWriteRegister((yyval.action), 0);	      /* a, i, i, a */
		      bufferWriteOp((yyval.action), SWFACTION_STACKSWAP);      /* a, i, a, i */
		      bufferWriteOp((yyval.action), SWFACTION_GETMEMBER); /* a, i, a[i] */
		      bufferWriteOp((yyval.action), (yyvsp[(1) - (2)].op));		      /* a, i, a[i]+1 */
		      bufferWriteSetRegister((yyval.action), 0);
		      bufferWriteOp((yyval.action), SWFACTION_SETMEMBER); /* a[i] = a[i]+1 */
		      bufferWriteRegister((yyval.action), 0);	      /* a[i]+1 */
		    }
		  }
		  else	/* identifier */
		  {
		    (yyval.action) = newBuffer();
		    bufferWriteBuffer((yyval.action), (yyvsp[(2) - (2)].lval).ident);
		    bufferWriteOp((yyval.action), SWFACTION_GETVARIABLE);
		    bufferWriteOp((yyval.action), (yyvsp[(1) - (2)].op));
		    bufferWriteOp((yyval.action), SWFACTION_PUSHDUP);
		    bufferConcat((yyval.action), (yyvsp[(2) - (2)].lval).ident);
		    bufferWriteOp((yyval.action), SWFACTION_STACKSWAP);
		    bufferWriteOp((yyval.action), SWFACTION_SETVARIABLE);
		  }
		}
    break;

  case 320:

    { if((yyvsp[(1) - (2)].lval).obj)
		  {
		    if((yyvsp[(1) - (2)].lval).ident)
		    {
		      (yyval.action) = (yyvsp[(1) - (2)].lval).obj;	                      /* a */
		      bufferWriteOp((yyval.action), SWFACTION_PUSHDUP);	      /* a, a */
		      bufferWriteBuffer((yyval.action), (yyvsp[(1) - (2)].lval).ident);        /* a, a, i */
		      bufferWriteOp((yyval.action), SWFACTION_GETMEMBER); /* a, a.i */
		      bufferWriteSetRegister((yyval.action), 0);
		      bufferWriteOp((yyval.action), SWFACTION_STACKSWAP);      /* a.i, a */
		      bufferConcat((yyval.action), (yyvsp[(1) - (2)].lval).ident);             /* a.i, a, i */
		      bufferWriteRegister((yyval.action), 0);             /* a.i, a, i, a.i */
		      bufferWriteOp((yyval.action), (yyvsp[(2) - (2)].op));		      /* a.i, a, i, a.i+1 */
		      bufferWriteOp((yyval.action), SWFACTION_SETMEMBER);
		    }
		    else
		    {
		      (yyval.action) = (yyvsp[(1) - (2)].lval).memexpr;
		      bufferConcat((yyval.action), (yyvsp[(1) - (2)].lval).obj);               /* i, a */
		      bufferWriteSetRegister((yyval.action), 0);
		      bufferWriteOp((yyval.action), SWFACTION_STACKSWAP);      /* a, i */
		      bufferWriteOp((yyval.action), SWFACTION_PUSHDUP);	      /* a, i, i */
		      bufferWriteRegister((yyval.action), 0);             /* a, i, i, a */
		      bufferWriteOp((yyval.action), SWFACTION_STACKSWAP);      /* a, i, a, i */
		      bufferWriteOp((yyval.action), SWFACTION_GETMEMBER); /* a, i, a[i] */
		      bufferWriteSetRegister((yyval.action), 0);
		      bufferWriteOp((yyval.action), (yyvsp[(2) - (2)].op));		      /* a, i, a[i]+1 */
		      bufferWriteOp((yyval.action), SWFACTION_SETMEMBER);
		      bufferWriteRegister((yyval.action), 0);             /* a[i] */
		    }
		  }
		  else
		  {
		    (yyval.action) = newBuffer();
		    bufferWriteBuffer((yyval.action), (yyvsp[(1) - (2)].lval).ident);
		    bufferWriteOp((yyval.action), SWFACTION_GETVARIABLE);
		    bufferWriteOp((yyval.action), SWFACTION_PUSHDUP);
		    bufferWriteOp((yyval.action), (yyvsp[(2) - (2)].op));
		    bufferConcat((yyval.action), (yyvsp[(1) - (2)].lval).ident);
		    bufferWriteOp((yyval.action), SWFACTION_STACKSWAP);
		    bufferWriteOp((yyval.action), SWFACTION_SETVARIABLE);
		  }
		}
    break;

  case 321:

    { (yyval.action) = (yyvsp[(2) - (3)].action); }
    break;

  case 322:

    { if((yyvsp[(1) - (3)].lval).obj)
		  {
		    if((yyvsp[(1) - (3)].lval).ident)
		    {
		      (yyval.action) = (yyvsp[(1) - (3)].lval).obj;			      /* a */
		      bufferWriteOp((yyval.action), SWFACTION_PUSHDUP);	      /* a, a */
		      bufferWriteBuffer((yyval.action), (yyvsp[(1) - (3)].lval).ident);	      /* a, a, i */
		      bufferWriteOp((yyval.action), SWFACTION_GETMEMBER); /* a, a.i */
		      bufferConcat((yyval.action), (yyvsp[(3) - (3)].action));		      /* a, a.i, v */
		      bufferWriteOp((yyval.action), (yyvsp[(2) - (3)].op));		      /* a, a.i+v */
		      bufferConcat((yyval.action), (yyvsp[(1) - (3)].lval).ident);	      /* a, a.i+v, i */
		      bufferWriteOp((yyval.action), SWFACTION_STACKSWAP);      /* a, i, a.i+v */
		      bufferWriteSetRegister((yyval.action), 0);
		      bufferWriteOp((yyval.action), SWFACTION_SETMEMBER); /* a.i = a.i+v */
		      bufferWriteRegister((yyval.action), 0);
		    }
		    else
		    {
		      (yyval.action) = (yyvsp[(1) - (3)].lval).memexpr;			      /* i */
		      bufferConcat((yyval.action), (yyvsp[(1) - (3)].lval).obj);		      /* i, a */
		      bufferWriteSetRegister((yyval.action), 0);
		      bufferWriteOp((yyval.action), SWFACTION_STACKSWAP);      /* a, i */
		      bufferWriteOp((yyval.action), SWFACTION_PUSHDUP);	      /* a, i, i */
		      bufferWriteRegister((yyval.action), 0);	      /* a, i, i, a */
		      bufferWriteOp((yyval.action), SWFACTION_STACKSWAP);      /* a, i, a, i */
		      bufferWriteOp((yyval.action), SWFACTION_GETMEMBER); /* a, i, a[i] */
		      bufferConcat((yyval.action), (yyvsp[(3) - (3)].action));		      /* a, i, a[i], v */
		      bufferWriteOp((yyval.action), (yyvsp[(2) - (3)].op));		      /* a, i, a[i]+v */
		      bufferWriteSetRegister((yyval.action), 0);
		      bufferWriteOp((yyval.action), SWFACTION_SETMEMBER); /* a[i] = a[i]+v */
		      bufferWriteRegister((yyval.action), 0);
		    }
		  }
		  else
		  {
		    (yyval.action) = (yyvsp[(1) - (3)].lval).ident;
		    bufferWriteOp((yyval.action), SWFACTION_PUSHDUP);
		    bufferWriteOp((yyval.action), SWFACTION_GETVARIABLE);
		    bufferConcat((yyval.action), (yyvsp[(3) - (3)].action));
		    bufferWriteOp((yyval.action), (yyvsp[(2) - (3)].op));
		    bufferWriteSetRegister((yyval.action), 0);
		    bufferWriteOp((yyval.action), SWFACTION_SETVARIABLE);
		    bufferWriteRegister((yyval.action), 0);
		  }
		}
    break;

  case 323:

    { (yyval.action) = newBuffer();
		  bufferWriteBoolean((yyval.action), (yyvsp[(1) - (1)].intVal)); }
    break;

  case 324:

    { (yyval.action) = newBuffer();
		  bufferWriteNull((yyval.action)); }
    break;

  case 325:

    { (yyval.action) = newBuffer();
		  bufferWriteUndef((yyval.action)); }
    break;

  case 326:

    { (yyval.action) = newBuffer();
		  bufferWriteString((yyval.action), (yyvsp[(1) - (1)].str), strlen((yyvsp[(1) - (1)].str))+1);
		  free((yyvsp[(1) - (1)].str)); }
    break;

  case 327:

    { (yyval.action) = newBuffer();
		  bufferWriteInt((yyval.action), (yyvsp[(1) - (1)].intVal)); }
    break;

  case 328:

    { (yyval.action) = newBuffer();
		  bufferWriteDouble((yyval.action), (yyvsp[(1) - (1)].doubleVal)); }
    break;

  case 330:

    { (yyval.action) = (yyvsp[(1) - (3)].action);
		  bufferConcat((yyval.action), (yyvsp[(3) - (3)].action)); }
    break;

  case 331:

    { (yyval.action) = newBuffer();
		  bufferWriteString((yyval.action), (yyvsp[(1) - (4)].str), strlen((yyvsp[(1) - (4)].str))+1);
		  free((yyvsp[(1) - (4)].str));
		  bufferConcat((yyval.action), (yyvsp[(4) - (4)].action));
		  bufferWriteOp((yyval.action), SWFACTION_DEFINELOCAL); }
    break;

  case 332:

    { (yyval.action) = newBuffer();
		  bufferWriteString((yyval.action), (yyvsp[(1) - (2)].str), strlen((yyvsp[(1) - (2)].str))+1);
		  free((yyvsp[(1) - (2)].str));
		  bufferWriteOp((yyval.action), SWFACTION_DEFINELOCAL2); }
    break;

  case 333:

    { asmBuffer = newBuffer(); }
    break;

  case 334:

    { (yyval.action) = asmBuffer; }
    break;

  case 335:

    { (yyval.action) = (yyvsp[(2) - (2)].action); }
    break;

  case 337:

    { (yyval.action) = (yyvsp[(1) - (1)].action);
		  bufferWriteOp((yyval.action), SWFACTION_POP); }
    break;

  case 338:

    { (yyval.action) = (yyvsp[(1) - (1)].action);
		  bufferWriteOp((yyval.action), SWFACTION_POP); }
    break;

  case 339:

    { (yyval.action) = (yyvsp[(1) - (1)].action);
		  bufferWriteOp((yyval.action), SWFACTION_POP); }
    break;

  case 340:

    { if((yyvsp[(2) - (2)].lval).obj)
		  {
		    if((yyvsp[(2) - (2)].lval).ident)
		    {
		      (yyval.action) = (yyvsp[(2) - (2)].lval).obj;		              /* a */
		      bufferWriteOp((yyval.action), SWFACTION_PUSHDUP);	      /* a, a */
		      bufferWriteBuffer((yyval.action), (yyvsp[(2) - (2)].lval).ident);	      /* a, a, i */
		      bufferWriteOp((yyval.action), SWFACTION_GETMEMBER); /* a, a.i */
		      bufferWriteOp((yyval.action), (yyvsp[(1) - (2)].op));		      /* a, a.i+1 */
		      bufferConcat((yyval.action), (yyvsp[(2) - (2)].lval).ident);	      /* a, a.i+1, i */
		      bufferWriteOp((yyval.action), SWFACTION_STACKSWAP);      /* a, i, a.i+1 */
		      bufferWriteOp((yyval.action), SWFACTION_SETMEMBER); /* a.i = a.i+1 */
		    }
		    else
		    {
		      /* weird contortions so that $2.memexpr can use reg 0 */
		      (yyval.action) = (yyvsp[(2) - (2)].lval).memexpr;			      /* i */
		      bufferConcat((yyval.action), (yyvsp[(2) - (2)].lval).obj);		      /* i, a */
		      bufferWriteSetRegister((yyval.action), 0);
		      bufferWriteOp((yyval.action), SWFACTION_STACKSWAP);      /* a, i */
		      bufferWriteOp((yyval.action), SWFACTION_PUSHDUP);	      /* a, i, i */
		      bufferWriteRegister((yyval.action), 0);	      /* a, i, i, a */
		      bufferWriteOp((yyval.action), SWFACTION_STACKSWAP);      /* a, i, a, i */
		      bufferWriteOp((yyval.action), SWFACTION_GETMEMBER); /* a, i, a[i] */
		      bufferWriteOp((yyval.action), (yyvsp[(1) - (2)].op));		      /* a, i, a[i]+1 */
		      bufferWriteOp((yyval.action), SWFACTION_SETMEMBER); /* a[i] = a[i]+1 */
		    }
		  }
		  else
		  {
		    (yyval.action) = (yyvsp[(2) - (2)].lval).ident;
		    bufferWriteOp((yyval.action), SWFACTION_PUSHDUP);
		    bufferWriteOp((yyval.action), SWFACTION_GETVARIABLE);
		    bufferWriteOp((yyval.action), (yyvsp[(1) - (2)].op));
		    bufferWriteOp((yyval.action), SWFACTION_SETVARIABLE);
		  }
		}
    break;

  case 341:

    { if((yyvsp[(1) - (2)].lval).obj)
		  {
		    if((yyvsp[(1) - (2)].lval).ident)
		    {
		      (yyval.action) = (yyvsp[(1) - (2)].lval).obj;			      /* a */
		      bufferWriteOp((yyval.action), SWFACTION_PUSHDUP);       /* a, a */
		      bufferWriteBuffer((yyval.action), (yyvsp[(1) - (2)].lval).ident);	      /* a, a, i */
		      bufferWriteOp((yyval.action), SWFACTION_GETMEMBER); /* a, a.i */
		      bufferWriteOp((yyval.action), (yyvsp[(2) - (2)].op));                  /* a, a.i+1 */
		      bufferConcat((yyval.action), (yyvsp[(1) - (2)].lval).ident);             /* a, a.i+1, i */
		      bufferWriteOp((yyval.action), SWFACTION_STACKSWAP);      /* a, i, a.i+1 */
		      bufferWriteOp((yyval.action), SWFACTION_SETMEMBER); /* a.i = a.i+1 */
		    }
		    else
		    {
		      /* weird contortions so that $1.memexpr can use reg 0 */
		      (yyval.action) = (yyvsp[(1) - (2)].lval).memexpr;	/* i */
		      bufferConcat((yyval.action), (yyvsp[(1) - (2)].lval).obj);		      /* i, a */
		      bufferWriteSetRegister((yyval.action), 0);
		      bufferWriteOp((yyval.action), SWFACTION_STACKSWAP);      /* a, i */
		      bufferWriteOp((yyval.action), SWFACTION_PUSHDUP);       /* a, i, i */
		      bufferWriteRegister((yyval.action), 0);             /* a, i, i, a */
		      bufferWriteOp((yyval.action), SWFACTION_STACKSWAP);      /* a, i, a, i */
		      bufferWriteOp((yyval.action), SWFACTION_GETMEMBER); /* a, i, a[i] */
		      bufferWriteOp((yyval.action), (yyvsp[(2) - (2)].op));                  /* a, i, a[i]+1 */
		      bufferWriteOp((yyval.action), SWFACTION_SETMEMBER); /* a[i] = a[i]+1 */
		    }
		  }
		  else
		  {
		    (yyval.action) = (yyvsp[(1) - (2)].lval).ident;
		    bufferWriteOp((yyval.action), SWFACTION_PUSHDUP);	
		    bufferWriteOp((yyval.action), SWFACTION_GETVARIABLE);
		    bufferWriteOp((yyval.action), (yyvsp[(2) - (2)].op));
		    bufferWriteOp((yyval.action), SWFACTION_SETVARIABLE);
		  }
		}
    break;

  case 342:

    { if((yyvsp[(1) - (3)].lval).obj)
		  {
		    (yyval.action) = (yyvsp[(1) - (3)].lval).obj;

		    if((yyvsp[(1) - (3)].lval).ident)
		      bufferConcat((yyval.action), (yyvsp[(1) - (3)].lval).ident);
		    else
		      bufferConcat((yyval.action), (yyvsp[(1) - (3)].lval).memexpr);

		    bufferConcat((yyval.action), (yyvsp[(3) - (3)].action));
		    bufferWriteOp((yyval.action), SWFACTION_SETMEMBER);
		  }
		  else
		  {
		    (yyval.action) = (yyvsp[(1) - (3)].lval).ident;
		    bufferConcat((yyval.action), (yyvsp[(3) - (3)].action));
		    bufferWriteOp((yyval.action), SWFACTION_SETVARIABLE);
		  }
		}
    break;

  case 343:

    { if((yyvsp[(1) - (3)].lval).obj)
		  {
		    if((yyvsp[(1) - (3)].lval).ident)
		    {
		      (yyval.action) = (yyvsp[(1) - (3)].lval).obj;			      /* a */
		      bufferWriteOp((yyval.action), SWFACTION_PUSHDUP);	      /* a, a */
		      bufferWriteBuffer((yyval.action), (yyvsp[(1) - (3)].lval).ident);	      /* a, a, i */
		      bufferWriteOp((yyval.action), SWFACTION_GETMEMBER); /* a, a.i */
		      bufferConcat((yyval.action), (yyvsp[(3) - (3)].action));		      /* a, a.i, v */
		      bufferWriteOp((yyval.action), (yyvsp[(2) - (3)].op));		      /* a, a.i+v */
		      bufferConcat((yyval.action), (yyvsp[(1) - (3)].lval).ident);	      /* a, a.i+v, i */
		      bufferWriteOp((yyval.action), SWFACTION_STACKSWAP);      /* a, i, a.i+v */
		      bufferWriteOp((yyval.action), SWFACTION_SETMEMBER); /* a.i = a.i+v */
		    }
		    else
		    {
		      (yyval.action) = (yyvsp[(1) - (3)].lval).memexpr;			      /* i */
		      bufferConcat((yyval.action), (yyvsp[(1) - (3)].lval).obj);		      /* i, a */
		      bufferWriteSetRegister((yyval.action), 0);
		      bufferWriteOp((yyval.action), SWFACTION_STACKSWAP);      /* a, i */
		      bufferWriteOp((yyval.action), SWFACTION_PUSHDUP);	      /* a, i, i */
		      bufferWriteRegister((yyval.action), 0);	      /* a, i, i, a */
		      bufferWriteOp((yyval.action), SWFACTION_STACKSWAP);      /* a, i, a, i */
		      bufferWriteOp((yyval.action), SWFACTION_GETMEMBER); /* a, i, a[i] */
		      bufferConcat((yyval.action), (yyvsp[(3) - (3)].action));		      /* a, i, a[i], v */
		      bufferWriteOp((yyval.action), (yyvsp[(2) - (3)].op));		      /* a, i, a[i]+v */
		      bufferWriteOp((yyval.action), SWFACTION_SETMEMBER); /* a[i] = a[i]+v */
		    }
		  }
		  else
		  {
		    (yyval.action) = (yyvsp[(1) - (3)].lval).ident;
		    bufferWriteOp((yyval.action), SWFACTION_PUSHDUP);
		    bufferWriteOp((yyval.action), SWFACTION_GETVARIABLE);
		    bufferConcat((yyval.action), (yyvsp[(3) - (3)].action));
		    bufferWriteOp((yyval.action), (yyvsp[(2) - (3)].op));
		    bufferWriteOp((yyval.action), SWFACTION_SETVARIABLE);
		  }
		}
    break;

  case 345:

    { (yyval.len) = (yyvsp[(1) - (2)].len) + (yyvsp[(2) - (2)].len); }
    break;

  case 346:

    { (yyval.len) = bufferWriteOp(asmBuffer,
						     SWFACTION_WITH); }
    break;

  case 347:

    { (yyval.len) = (yyvsp[(2) - (4)].len) + (yyvsp[(3) - (4)].len);
				  bufferPatchLength(asmBuffer, (yyvsp[(3) - (4)].len)); }
    break;

  case 348:

    { (yyval.len) = bufferWriteConstantString(asmBuffer, (yyvsp[(1) - (1)].str),
								 strlen((yyvsp[(1) - (1)].str))+1); }
    break;

  case 349:

    { (yyval.len) = bufferWriteInt(asmBuffer, (yyvsp[(1) - (1)].intVal)); }
    break;

  case 350:

    { (yyval.len) = bufferWriteDouble(asmBuffer, (yyvsp[(1) - (1)].doubleVal)); }
    break;

  case 351:

    { bufferWriteU8(asmBuffer, PUSH_BOOLEAN);
				  (yyval.len) = bufferWriteU8(asmBuffer, (yyvsp[(1) - (1)].intVal))+1;
				  bufferPatchPushLength(asmBuffer, 2); }
    break;

  case 352:

    { (yyval.len) = bufferWriteU8(asmBuffer, PUSH_NULL);
				  bufferPatchPushLength(asmBuffer, 1); }
    break;

  case 353:

    { (yyval.len) = bufferWriteU8(asmBuffer, PUSH_UNDEF);
				  bufferPatchPushLength(asmBuffer, 1); }
    break;

  case 354:

    { bufferWriteU8(asmBuffer, PUSH_REGISTER);
				  (yyval.len) = bufferWriteU8(asmBuffer,
						     (char)atoi((yyvsp[(1) - (1)].str)))+1;
				  bufferPatchPushLength(asmBuffer, 2); }
    break;

  case 355:

    { (yyval.len) = (yyvsp[(1) - (1)].len); }
    break;

  case 356:

    { (yyval.len) = (yyvsp[(1) - (3)].len) + (yyvsp[(3) - (3)].len); }
    break;

  case 357:

    { (yyval.len) = bufferWritePushOp(asmBuffer);
				  (yyval.len) += bufferWriteS16(asmBuffer, 0); }
    break;

  case 358:

    { (yyval.len) = (yyvsp[(2) - (3)].len) + (yyvsp[(3) - (3)].len);
				  bufferPatchLength(asmBuffer, (yyvsp[(3) - (3)].len)); }
    break;

  case 360:

    { (yyval.len) = bufferWriteOp(asmBuffer, 
						     SWFACTION_STOREREGISTER);
				  (yyval.len) += bufferWriteS16(asmBuffer, 1);
				  (yyval.len) += bufferWriteU8(asmBuffer,
						      (char)atoi((yyvsp[(2) - (2)].str))); }
    break;

  case 361:

    { (yyval.len) = bufferWriteOp(asmBuffer,
						     SWFACTION_CALLFUNCTION); }
    break;

  case 362:

    { (yyval.len) = bufferWriteOp(asmBuffer,
						     SWFACTION_RETURN); }
    break;

  case 363:

    { (yyval.len) = bufferWriteOp(asmBuffer, 
						     SWFACTION_CALLMETHOD); }
    break;

  case 364:

    { (yyval.len) = bufferWriteOp(asmBuffer, 
						     SWFACTION_NEWOBJECT); }
    break;

  case 365:

    { (yyval.len) = bufferWriteOp(asmBuffer, 
						     SWFACTION_NEWMETHOD); }
    break;

  case 366:

    { (yyval.len) = bufferWriteOp(asmBuffer, 
						     SWFACTION_BITWISEAND); }
    break;

  case 367:

    { (yyval.len) = bufferWriteOp(asmBuffer, 
						     SWFACTION_BITWISEOR); }
    break;

  case 368:

    { (yyval.len) = bufferWriteOp(asmBuffer, 
						     SWFACTION_BITWISEXOR); }
    break;

  case 369:

    { (yyval.len) = bufferWriteOp(asmBuffer, 
						     SWFACTION_MODULO); }
    break;

  case 370:

    { (yyval.len) = bufferWriteOp(asmBuffer, 
						     SWFACTION_ADD2); }
    break;

  case 371:

    { (yyval.len) = bufferWriteOp(asmBuffer, 
						     SWFACTION_LESS2); }
    break;

  case 372:

    { (yyval.len) = bufferWriteOp(asmBuffer, 
						     SWFACTION_EQUALS2); }
    break;

  case 373:

    { (yyval.len) = bufferWriteOp(asmBuffer, 
						     SWFACTION_INCREMENT); }
    break;

  case 374:

    { (yyval.len) = bufferWriteOp(asmBuffer, 
						     SWFACTION_DECREMENT); }
    break;

  case 375:

    { (yyval.len) = bufferWriteOp(asmBuffer, 
						     SWFACTION_TYPEOF); }
    break;

  case 376:

    { (yyval.len) = bufferWriteOp(asmBuffer, 
						     SWFACTION_INSTANCEOF); }
    break;

  case 377:

    { (yyval.len) = bufferWriteOp(asmBuffer, 
						     SWFACTION_ENUMERATE); }
    break;

  case 378:

    { (yyval.len) = bufferWriteOp(asmBuffer, 
						     SWFACTION_ENUMERATE2); }
    break;

  case 379:

    { (yyval.len) = bufferWriteOp(asmBuffer, 
						     SWFACTION_DELETE); }
    break;

  case 380:

    { (yyval.len) = bufferWriteOp(asmBuffer, 
						     SWFACTION_DELETE2); }
    break;

  case 381:

    { (yyval.len) = bufferWriteOp(asmBuffer, 
						     SWFACTION_NEWOBJECT); }
    break;

  case 382:

    { (yyval.len) = bufferWriteOp(asmBuffer, 
						     SWFACTION_INITARRAY); }
    break;

  case 383:

    { (yyval.len) = bufferWriteOp(asmBuffer, 
						     SWFACTION_INITOBJECT); }
    break;

  case 384:

    { (yyval.len) = bufferWriteOp(asmBuffer, 
						     SWFACTION_GETMEMBER); }
    break;

  case 385:

    { (yyval.len) = bufferWriteOp(asmBuffer, 
						     SWFACTION_SETMEMBER); }
    break;

  case 386:

    { (yyval.len) = bufferWriteOp(asmBuffer, 
						     SWFACTION_SHIFTLEFT); }
    break;

  case 387:

    { (yyval.len) = bufferWriteOp(asmBuffer, 
						     SWFACTION_SHIFTRIGHT); }
    break;

  case 388:

    { (yyval.len) = bufferWriteOp(asmBuffer, 
						     SWFACTION_SHIFTRIGHT2); }
    break;

  case 389:

    { (yyval.len) = bufferWriteOp(asmBuffer, 
						     SWFACTION_DEFINELOCAL2); }
    break;

  case 390:

    { (yyval.len) = bufferWriteOp(asmBuffer, 
						     SWFACTION_EXTENDS); }
    break;

  case 391:

    { (yyval.len) = bufferWriteOp(asmBuffer, 
						     SWFACTION_TARGETPATH); }
    break;

  case 392:

    { (yyval.len) = bufferWriteOp(asmBuffer,
						     (char)(yyvsp[(2) - (2)].intVal)); }
    break;

  case 393:

    { (yyval.len) = bufferWriteOp(asmBuffer, 
						     SWFACTION_IMPLEMENTSOP); }
    break;

  case 394:

    { (yyval.len) = bufferWriteOp(asmBuffer, 
						     SWFACTION_FSCOMMAND2); }
    break;

  case 395:

    { (yyval.len) = bufferWriteOp(asmBuffer,
						     SWFACTION_CASTOP);	}
    break;

  case 396:

    { (yyval.len) = bufferWriteOp(asmBuffer, SWFACTION_ADD); }
    break;

  case 397:

    { (yyval.len) = bufferWriteOp(asmBuffer, SWFACTION_SUBTRACT); }
    break;

  case 398:

    { (yyval.len) = bufferWriteOp(asmBuffer, SWFACTION_MULTIPLY); }
    break;

  case 399:

    { (yyval.len) = bufferWriteOp(asmBuffer, SWFACTION_DIVIDE); }
    break;

  case 400:

    { (yyval.len) = bufferWriteOp(asmBuffer, SWFACTION_EQUAL); }
    break;

  case 401:

    { (yyval.len) = bufferWriteOp(asmBuffer, SWFACTION_LESSTHAN); }
    break;

  case 402:

    { (yyval.len) = bufferWriteOp(asmBuffer, SWFACTION_LOGICALAND); }
    break;

  case 403:

    { (yyval.len) = bufferWriteOp(asmBuffer, SWFACTION_LOGICALOR); }
    break;

  case 404:

    { (yyval.len) = bufferWriteOp(asmBuffer, SWFACTION_LOGICALNOT); }
    break;

  case 405:

    { (yyval.len) = bufferWriteOp(asmBuffer, SWFACTION_STRINGEQ); }
    break;

  case 406:

    { (yyval.len) = bufferWriteOp(asmBuffer, SWFACTION_STRINGLENGTH); }
    break;

  case 407:

    { (yyval.len) = bufferWriteOp(asmBuffer, SWFACTION_SUBSTRING); }
    break;

  case 408:

    { (yyval.len) = bufferWriteOp(asmBuffer, SWFACTION_INT); }
    break;

  case 409:

    { (yyval.len) = bufferWriteOp(asmBuffer, SWFACTION_PUSHDUP); }
    break;

  case 410:

    { (yyval.len) = bufferWriteOp(asmBuffer, SWFACTION_STACKSWAP); }
    break;

  case 411:

    { (yyval.len) = bufferWriteOp(asmBuffer, SWFACTION_POP); }
    break;

  case 412:

    { (yyval.len) = bufferWriteOp(asmBuffer, SWFACTION_GETVARIABLE); }
    break;

  case 413:

    { (yyval.len) = bufferWriteOp(asmBuffer, SWFACTION_SETVARIABLE); }
    break;

  case 414:

    { (yyval.len) = bufferWriteOp(asmBuffer, SWFACTION_GETPROPERTY); }
    break;

  case 415:

    { (yyval.len) = bufferWriteOp(asmBuffer, SWFACTION_SETPROPERTY); }
    break;

  case 416:

    { (yyval.len) = bufferWriteOp(asmBuffer, SWFACTION_TONUMBER); }
    break;

  case 417:

    { (yyval.len) = bufferWriteOp(asmBuffer, SWFACTION_TOSTRING); }
    break;

  case 418:

    { (yyval.len) = bufferWriteOp(asmBuffer, SWFACTION_SETTARGET2); }
    break;

  case 419:

    { (yyval.len) = bufferWriteOp(asmBuffer, SWFACTION_STRINGCONCAT); }
    break;

  case 420:

    { (yyval.len) = bufferWriteOp(asmBuffer, SWFACTION_DUPLICATECLIP); }
    break;

  case 421:

    { (yyval.len) = bufferWriteOp(asmBuffer, SWFACTION_REMOVECLIP); }
    break;

  case 422:

    { (yyval.len) = bufferWriteOp(asmBuffer, SWFACTION_TRACE); }
    break;

  case 423:

    { (yyval.len) = bufferWriteOp(asmBuffer, SWFACTION_STRINGCOMPARE); }
    break;

  case 424:

    { (yyval.len) = bufferWriteOp(asmBuffer, SWFACTION_RANDOMNUMBER); }
    break;

  case 425:

    { (yyval.len) = bufferWriteOp(asmBuffer, SWFACTION_MBLENGTH); }
    break;

  case 426:

    { (yyval.len) = bufferWriteOp(asmBuffer, SWFACTION_ORD); }
    break;

  case 427:

    { (yyval.len) = bufferWriteOp(asmBuffer, SWFACTION_CHR); }
    break;

  case 428:

    { (yyval.len) = bufferWriteOp(asmBuffer, SWFACTION_GETTIME); }
    break;

  case 429:

    { (yyval.len) = bufferWriteOp(asmBuffer, SWFACTION_MBSUBSTRING); }
    break;

  case 430:

    { (yyval.len) = bufferWriteOp(asmBuffer, SWFACTION_MBORD); }
    break;

  case 431:

    { (yyval.len) = bufferWriteOp(asmBuffer, SWFACTION_MBCHR); }
    break;

  case 432:

    { (yyval.len) = bufferWriteOp(asmBuffer, SWFACTION_JUMP);
				  (yyval.len) += bufferWriteS16(asmBuffer, 2);
				  (yyval.len) += bufferBranchTarget(asmBuffer, (yyvsp[(2) - (2)].str)); }
    break;

  case 433:

    { (yyval.len) = bufferWriteOp(asmBuffer, SWFACTION_IF);
				  (yyval.len) += bufferWriteS16(asmBuffer, 2);
				  (yyval.len) += bufferBranchTarget(asmBuffer, (yyvsp[(2) - (2)].str)); }
    break;

  case 434:

    { (yyval.intVal) = lookupProperty((yyvsp[(1) - (1)].str)); }
    break;

  case 435:

    { (yyval.intVal) = PROPERTY_X; }
    break;

  case 436:

    { (yyval.intVal) = PROPERTY_Y; }
    break;

  case 437:

    { (yyval.intVal) = PROPERTY_XSCALE; }
    break;

  case 438:

    { (yyval.intVal) = PROPERTY_YSCALE; }
    break;

  case 439:

    { (yyval.intVal) = PROPERTY_CURRENTFRAME; }
    break;

  case 440:

    { (yyval.intVal) = PROPERTY_TOTALFRAMES; }
    break;

  case 441:

    { (yyval.intVal) = PROPERTY_ALPHA; }
    break;

  case 442:

    { (yyval.intVal) = PROPERTY_VISIBLE; }
    break;

  case 443:

    { (yyval.intVal) = PROPERTY_WIDTH; }
    break;

  case 444:

    { (yyval.intVal) = PROPERTY_HEIGHT; }
    break;

  case 445:

    { (yyval.intVal) = PROPERTY_ROTATION; }
    break;

  case 446:

    { (yyval.intVal) = PROPERTY_TARGET; }
    break;

  case 447:

    { (yyval.intVal) = PROPERTY_FRAMESLOADED; }
    break;

  case 448:

    { (yyval.intVal) = PROPERTY_NAME; }
    break;

  case 449:

    { (yyval.intVal) = PROPERTY_DROPTARGET; }
    break;

  case 450:

    { (yyval.intVal) = PROPERTY_URL; }
    break;

  case 451:

    { (yyval.intVal) = PROPERTY_HIGHQUALITY; }
    break;

  case 452:

    { (yyval.intVal) = PROPERTY_FOCUSRECT; }
    break;

  case 453:

    { (yyval.intVal) = PROPERTY_SOUNDBUFTIME; }
    break;

  case 454:

    { (yyval.intVal) = PROPERTY_QUALITY; }
    break;

  case 455:

    { (yyval.intVal) = PROPERTY_XMOUSE; }
    break;

  case 456:

    { (yyval.intVal) = PROPERTY_YMOUSE; }
    break;



      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined(yyoverflow) || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}

