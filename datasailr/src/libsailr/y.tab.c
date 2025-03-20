#include <R_ext/Print.h>
/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

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

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 2

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 1 "parse.y"

#include <stdio.h>
#include "node.h"
#include "parser_state.h"
#include "string/common_string.h"

int yydebug = 0; /* 0 : no debug, 1: debug */


#line 81 "y.tab.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

/* Use api.header.include to #include this header
   instead of duplicating it here.  */
#ifndef YY_YY_Y_TAB_H_INCLUDED
# define YY_YY_Y_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    LIT_NUM = 258,                 /* LIT_NUM  */
    NA_NUM = 259,                  /* NA_NUM  */
    LIT_STR = 260,                 /* LIT_STR  */
    LIT_REXP = 261,                /* LIT_REXP  */
    IDENT = 262,                   /* IDENT  */
    KEY_IF = 263,                  /* KEY_IF  */
    KEY_ELSE = 264,                /* KEY_ELSE  */
    ASSIGN = 265,                  /* ASSIGN  */
    TERMIN = 266,                  /* TERMIN  */
    PLCUR = 267,                   /* PLCUR  */
    PRCUR = 268,                   /* PRCUR  */
    COMMA = 269,                   /* COMMA  */
    OR = 270,                      /* OR  */
    AND = 271,                     /* AND  */
    OP_EQ = 272,                   /* OP_EQ  */
    OP_NEQ = 273,                  /* OP_NEQ  */
    REXP_MATCH = 274,              /* REXP_MATCH  */
    OP_LT = 275,                   /* OP_LT  */
    OP_LE = 276,                   /* OP_LE  */
    OP_GT = 277,                   /* OP_GT  */
    OP_GE = 278,                   /* OP_GE  */
    OP_PLUS = 279,                 /* OP_PLUS  */
    OP_SUB = 280,                  /* OP_SUB  */
    OP_MULT = 281,                 /* OP_MULT  */
    OP_DIV = 282,                  /* OP_DIV  */
    OP_MOD = 283,                  /* OP_MOD  */
    OP_POWER = 284,                /* OP_POWER  */
    FACTOR = 285,                  /* FACTOR  */
    UMINUS = 286                   /* UMINUS  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif
/* Token kinds.  */
#define YYEMPTY -2
#define YYEOF 0
#define YYerror 256
#define YYUNDEF 257
#define LIT_NUM 258
#define NA_NUM 259
#define LIT_STR 260
#define LIT_REXP 261
#define IDENT 262
#define KEY_IF 263
#define KEY_ELSE 264
#define ASSIGN 265
#define TERMIN 266
#define PLCUR 267
#define PRCUR 268
#define COMMA 269
#define OR 270
#define AND 271
#define OP_EQ 272
#define OP_NEQ 273
#define REXP_MATCH 274
#define OP_LT 275
#define OP_LE 276
#define OP_GT 277
#define OP_GE 278
#define OP_PLUS 279
#define OP_SUB 280
#define OP_MULT 281
#define OP_DIV 282
#define OP_MOD 283
#define OP_POWER 284
#define FACTOR 285
#define UMINUS 286

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 13 "parse.y"

  TreeNode* nd;
  string_object* str;
  char* id;

#line 202 "y.tab.c"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif

/* Location type.  */
#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE YYLTYPE;
struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
};
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif




int yyparse (parser_state *p, void* scanner);


#endif /* !YY_YY_Y_TAB_H_INCLUDED  */
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_LIT_NUM = 3,                    /* LIT_NUM  */
  YYSYMBOL_NA_NUM = 4,                     /* NA_NUM  */
  YYSYMBOL_LIT_STR = 5,                    /* LIT_STR  */
  YYSYMBOL_LIT_REXP = 6,                   /* LIT_REXP  */
  YYSYMBOL_IDENT = 7,                      /* IDENT  */
  YYSYMBOL_KEY_IF = 8,                     /* KEY_IF  */
  YYSYMBOL_KEY_ELSE = 9,                   /* KEY_ELSE  */
  YYSYMBOL_ASSIGN = 10,                    /* ASSIGN  */
  YYSYMBOL_TERMIN = 11,                    /* TERMIN  */
  YYSYMBOL_PLCUR = 12,                     /* PLCUR  */
  YYSYMBOL_PRCUR = 13,                     /* PRCUR  */
  YYSYMBOL_COMMA = 14,                     /* COMMA  */
  YYSYMBOL_OR = 15,                        /* OR  */
  YYSYMBOL_AND = 16,                       /* AND  */
  YYSYMBOL_OP_EQ = 17,                     /* OP_EQ  */
  YYSYMBOL_OP_NEQ = 18,                    /* OP_NEQ  */
  YYSYMBOL_REXP_MATCH = 19,                /* REXP_MATCH  */
  YYSYMBOL_OP_LT = 20,                     /* OP_LT  */
  YYSYMBOL_OP_LE = 21,                     /* OP_LE  */
  YYSYMBOL_OP_GT = 22,                     /* OP_GT  */
  YYSYMBOL_OP_GE = 23,                     /* OP_GE  */
  YYSYMBOL_OP_PLUS = 24,                   /* OP_PLUS  */
  YYSYMBOL_OP_SUB = 25,                    /* OP_SUB  */
  YYSYMBOL_OP_MULT = 26,                   /* OP_MULT  */
  YYSYMBOL_OP_DIV = 27,                    /* OP_DIV  */
  YYSYMBOL_OP_MOD = 28,                    /* OP_MOD  */
  YYSYMBOL_OP_POWER = 29,                  /* OP_POWER  */
  YYSYMBOL_FACTOR = 30,                    /* FACTOR  */
  YYSYMBOL_UMINUS = 31,                    /* UMINUS  */
  YYSYMBOL_32_ = 32,                       /* '('  */
  YYSYMBOL_33_ = 33,                       /* ')'  */
  YYSYMBOL_34_ = 34,                       /* '{'  */
  YYSYMBOL_35_ = 35,                       /* '}'  */
  YYSYMBOL_YYACCEPT = 36,                  /* $accept  */
  YYSYMBOL_program = 37,                   /* program  */
  YYSYMBOL_prgm = 38,                      /* prgm  */
  YYSYMBOL_stmts = 39,                     /* stmts  */
  YYSYMBOL_stmt = 40,                      /* stmt  */
  YYSYMBOL_expr = 41,                      /* expr  */
  YYSYMBOL_arg = 42,                       /* arg  */
  YYSYMBOL_primary = 43,                   /* primary  */
  YYSYMBOL_fcall = 44,                     /* fcall  */
  YYSYMBOL_fname = 45,                     /* fname  */
  YYSYMBOL_args = 46,                      /* args  */
  YYSYMBOL_if_stmt = 47,                   /* if_stmt  */
  YYSYMBOL_condition = 48,                 /* condition  */
  YYSYMBOL_then_stmts = 49,                /* then_stmts  */
  YYSYMBOL_opt_else = 50,                  /* opt_else  */
  YYSYMBOL_assign_stmt = 51,               /* assign_stmt  */
  YYSYMBOL_lvar = 52,                      /* lvar  */
  YYSYMBOL_opt_termin = 53,                /* opt_termin  */
  YYSYMBOL_opt_termins = 54,               /* opt_termins  */
  YYSYMBOL_termins = 55                    /* termins  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;


/* Second part of user prologue.  */
#line 50 "parse.y"

// Prevent undefined reference warnings
// (ref) https://stackoverflow.com/questions/23717039/generating-a-compiler-from-lex-and-yacc-grammar
// (ref) https://stackoverflow.com/questions/28643114/how-to-use-flex-with-my-own-parser
int yylex (YYSTYPE* yylval, YYLTYPE* yylloc, parser_state* p, void* scanner);
void yyerror (YYLTYPE* loc, parser_state* p, void* scanner, char const *);
#line 58 "parse.y"

TreeNode* node_set_loc( TreeNode* nd, YYLTYPE* loc);

#line 307 "y.tab.c"


#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_int8 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if !defined yyoverflow

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
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
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
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* !defined yyoverflow */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL \
             && defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
  YYLTYPE yyls_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE) \
             + YYSIZEOF (YYLTYPE)) \
      + 2 * YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  7
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   144

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  36
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  20
/* YYNRULES -- Number of rules.  */
#define YYNRULES  55
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  91

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   286


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      32,    33,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    34,     2,    35,     2,     2,     2,     2,
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
      25,    26,    27,    28,    29,    30,    31
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint8 yyrline[] =
{
       0,   108,   108,   110,   111,   113,   114,   119,   120,   121,
     123,   124,   125,   127,   128,   129,   130,   131,   132,   133,
     134,   135,   136,   137,   138,   139,   140,   141,   142,   143,
     145,   152,   153,   154,   155,   156,   158,   160,   162,   163,
     166,   175,   180,   182,   183,   186,   187,   188,   191,   193,
     201,   202,   204,   205,   207,   208
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if YYDEBUG || 0
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "LIT_NUM", "NA_NUM",
  "LIT_STR", "LIT_REXP", "IDENT", "KEY_IF", "KEY_ELSE", "ASSIGN", "TERMIN",
  "PLCUR", "PRCUR", "COMMA", "OR", "AND", "OP_EQ", "OP_NEQ", "REXP_MATCH",
  "OP_LT", "OP_LE", "OP_GT", "OP_GE", "OP_PLUS", "OP_SUB", "OP_MULT",
  "OP_DIV", "OP_MOD", "OP_POWER", "FACTOR", "UMINUS", "'('", "')'", "'{'",
  "'}'", "$accept", "program", "prgm", "stmts", "stmt", "expr", "arg",
  "primary", "fcall", "fname", "args", "if_stmt", "condition",
  "then_stmts", "opt_else", "assign_stmt", "lvar", "opt_termin",
  "opt_termins", "termins", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-51)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-50)

#define yytable_value_is_error(Yyn) \
  ((Yyn) == YYTABLE_NINF)

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int8 yypact[] =
{
       1,     1,     3,   -51,    64,   -51,   -51,   -51,   -51,   -51,
     -51,   -51,    -6,   -19,    70,    70,     1,   -51,    -8,    88,
      -4,   -51,    19,   -51,   -51,     7,    70,    24,    20,   -51,
      67,   -51,    64,    70,    70,    70,    70,    70,    70,    70,
      70,    70,    70,    70,    70,    70,    70,   -51,    18,    70,
      70,    71,     1,    43,    46,   -51,   -51,    41,   -51,   102,
     102,   109,   109,   109,   109,   114,   114,   -20,   -20,   -20,
     -20,   -51,   -51,    -8,     0,    -8,    54,    44,   -51,    56,
     -51,    70,   -51,   -51,   -51,   -51,     1,   -51,    -8,    45,
     -51
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int8 yydefact[] =
{
      52,    55,     0,     2,     4,    53,    54,     1,    31,    35,
      32,    33,    30,     0,     0,     0,    52,     5,     9,    12,
      29,    13,     0,     8,     7,     0,     0,     0,    30,    27,
       0,     3,    53,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    19,     0,    38,
       0,     0,    52,     0,    45,    34,     6,    11,    10,    21,
      22,    24,    26,    23,    25,    14,    15,    16,    17,    18,
      20,    30,    28,    39,     0,    48,    50,     0,    43,     0,
      41,     0,    36,    51,    42,    44,    52,    46,    40,     0,
      47
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -51,   -51,   -50,   -51,   -26,   -15,     2,    30,   -51,   -51,
     -51,   -51,   -51,   -51,   -51,   -51,   -51,   -51,    68,     4
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
       0,     2,     3,    16,    17,    18,    19,    20,    21,    22,
      74,    23,    27,    54,    80,    24,    25,    84,     4,     5
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int8 yytable[] =
{
      30,    53,    77,     7,   -49,     6,    56,    33,    34,    46,
      47,    51,     1,    26,    81,    48,    29,    50,    57,    58,
      32,     8,     9,    10,    11,    71,   -37,     8,     9,    10,
      11,    12,    13,    82,    73,    75,    89,    59,    60,    61,
      62,    63,    64,    65,    66,    67,    68,    69,    70,    14,
      15,    49,   -37,    87,    78,    79,    15,    34,    52,     8,
       9,    10,    11,    12,    13,    83,    88,     8,     9,    10,
      11,    12,    13,     8,     9,    10,    11,    28,    72,    85,
      90,    14,    33,    34,    31,     0,    33,    34,    15,    14,
      86,     0,     0,     0,     0,    14,    15,     0,     0,     0,
      55,     0,    15,     0,    76,    35,    36,     0,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,   -50,
     -50,     0,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    41,    42,    43,    44,    45,    46,    47,
      43,    44,    45,    46,    47
};

static const yytype_int8 yycheck[] =
{
      15,    27,    52,     0,    10,     1,    32,    15,    16,    29,
      30,    26,    11,    32,    14,    19,    14,    10,    33,    34,
      16,     3,     4,     5,     6,     7,    32,     3,     4,     5,
       6,     7,     8,    33,    49,    50,    86,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    25,
      32,    32,    32,    79,    11,     9,    32,    16,    34,     3,
       4,     5,     6,     7,     8,    11,    81,     3,     4,     5,
       6,     7,     8,     3,     4,     5,     6,     7,    48,    35,
      35,    25,    15,    16,    16,    -1,    15,    16,    32,    25,
      34,    -1,    -1,    -1,    -1,    25,    32,    -1,    -1,    -1,
      33,    -1,    32,    -1,    33,    17,    18,    -1,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    17,
      18,    -1,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    24,    25,    26,    27,    28,    29,    30,
      26,    27,    28,    29,    30
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,    11,    37,    38,    54,    55,    55,     0,     3,     4,
       5,     6,     7,     8,    25,    32,    39,    40,    41,    42,
      43,    44,    45,    47,    51,    52,    32,    48,     7,    42,
      41,    54,    55,    15,    16,    17,    18,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    19,    32,
      10,    41,    34,    40,    49,    33,    40,    41,    41,    42,
      42,    42,    42,    42,    42,    42,    42,    42,    42,    42,
      42,     7,    43,    41,    46,    41,    33,    38,    11,     9,
      50,    14,    33,    11,    53,    35,    34,    40,    41,    38,
      35
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr1[] =
{
       0,    36,    37,    38,    38,    39,    39,    40,    40,    40,
      41,    41,    41,    42,    42,    42,    42,    42,    42,    42,
      42,    42,    42,    42,    42,    42,    42,    42,    42,    42,
      43,    43,    43,    43,    43,    43,    44,    45,    46,    46,
      46,    47,    48,    49,    49,    50,    50,    50,    51,    52,
      53,    53,    54,    54,    55,    55
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     1,     3,     1,     1,     3,     1,     1,     1,
       3,     3,     1,     1,     3,     3,     3,     3,     3,     2,
       3,     3,     3,     3,     3,     3,     3,     2,     3,     1,
       1,     1,     1,     1,     3,     1,     4,     1,     0,     1,
       3,     4,     4,     2,     3,     0,     2,     4,     3,     1,
       0,     1,     0,     1,     2,     1
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (&yylloc, p, scanner, YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF

/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)                                \
    do                                                                  \
      if (N)                                                            \
        {                                                               \
          (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;        \
          (Current).first_column = YYRHSLOC (Rhs, 1).first_column;      \
          (Current).last_line    = YYRHSLOC (Rhs, N).last_line;         \
          (Current).last_column  = YYRHSLOC (Rhs, N).last_column;       \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).first_line   = (Current).last_line   =              \
            YYRHSLOC (Rhs, 0).last_line;                                \
          (Current).first_column = (Current).last_column =              \
            YYRHSLOC (Rhs, 0).last_column;                              \
        }                                                               \
    while (0)
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K])


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)


/* YYLOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

# ifndef YYLOCATION_PRINT

#  if defined YY_LOCATION_PRINT

   /* Temporary convenience wrapper in case some people defined the
      undocumented and private YY_LOCATION_PRINT macros.  */
#   define YYLOCATION_PRINT(File, Loc)  YY_LOCATION_PRINT(File, *(Loc))

#  elif defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL

/* Print *YYLOCP on YYO.  Private, do not rely on its existence. */

YY_ATTRIBUTE_UNUSED
static int
yy_location_print_ (FILE *yyo, YYLTYPE const * const yylocp)
{
  int res = 0;
  int end_col = 0 != yylocp->last_column ? yylocp->last_column - 1 : 0;
  if (0 <= yylocp->first_line)
    {
      res += YYFPRINTF (yyo, "%d", yylocp->first_line);
      if (0 <= yylocp->first_column)
        res += YYFPRINTF (yyo, ".%d", yylocp->first_column);
    }
  if (0 <= yylocp->last_line)
    {
      if (yylocp->first_line < yylocp->last_line)
        {
          res += YYFPRINTF (yyo, "-%d", yylocp->last_line);
          if (0 <= end_col)
            res += YYFPRINTF (yyo, ".%d", end_col);
        }
      else if (0 <= end_col && yylocp->first_column < end_col)
        res += YYFPRINTF (yyo, "-%d", end_col);
    }
  return res;
}

#   define YYLOCATION_PRINT  yy_location_print_

    /* Temporary convenience wrapper in case some people defined the
       undocumented and private YY_LOCATION_PRINT macros.  */
#   define YY_LOCATION_PRINT(File, Loc)  YYLOCATION_PRINT(File, &(Loc))

#  else

#   define YYLOCATION_PRINT(File, Loc) ((void) 0)
    /* Temporary convenience wrapper in case some people defined the
       undocumented and private YY_LOCATION_PRINT macros.  */
#   define YY_LOCATION_PRINT  YYLOCATION_PRINT

#  endif
# endif /* !defined YYLOCATION_PRINT */


# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value, Location, p, scanner); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, parser_state *p, void* scanner)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  YY_USE (yylocationp);
  YY_USE (p);
  YY_USE (scanner);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, parser_state *p, void* scanner)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  YYLOCATION_PRINT (yyo, yylocationp);
  YYFPRINTF (yyo, ": ");
  yy_symbol_value_print (yyo, yykind, yyvaluep, yylocationp, p, scanner);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp, YYLTYPE *yylsp,
                 int yyrule, parser_state *p, void* scanner)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)],
                       &(yylsp[(yyi + 1) - (yynrhs)]), p, scanner);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, yylsp, Rule, p, scanner); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
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






/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep, YYLTYPE *yylocationp, parser_state *p, void* scanner)
{
  YY_USE (yyvaluep);
  YY_USE (yylocationp);
  YY_USE (p);
  YY_USE (scanner);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}






/*----------.
| yyparse.  |
`----------*/

int
yyparse (parser_state *p, void* scanner)
{
/* Lookahead token kind.  */
int yychar;


/* The semantic value of the lookahead symbol.  */
/* Default value used for initialization, for pacifying older GCCs
   or non-GCC compilers.  */
YY_INITIAL_VALUE (static YYSTYPE yyval_default;)
YYSTYPE yylval YY_INITIAL_VALUE (= yyval_default);

/* Location data for the lookahead symbol.  */
static YYLTYPE yyloc_default
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
  = { 1, 1, 1, 1 }
# endif
;
YYLTYPE yylloc = yyloc_default;

    /* Number of syntax errors so far.  */
    int yynerrs = 0;

    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

    /* The location stack: array, bottom, top.  */
    YYLTYPE yylsa[YYINITDEPTH];
    YYLTYPE *yyls = yylsa;
    YYLTYPE *yylsp = yyls;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

  /* The locations where the error started and ended.  */
  YYLTYPE yyerror_range[3];



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */

  yylsp[0] = yylloc;
  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;
        YYLTYPE *yyls1 = yyls;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yyls1, yysize * YYSIZEOF (*yylsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
        yyls = yyls1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
        YYSTACK_RELOCATE (yyls_alloc, yyls);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
      yylsp = yyls + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


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

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex (&yylval, &yylloc, p, scanner);
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      yyerror_range[1] = yylloc;
      goto yyerrlab1;
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
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END
  *++yylsp = yylloc;

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
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
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];

  /* Default location. */
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
  yyerror_range[1] = yyloc;
  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2: /* program: prgm  */
#line 108 "parse.y"
                                                { p->tree = new_node_prgm( (yyvsp[0].nd) );}
#line 1458 "y.tab.c"
    break;

  case 3: /* prgm: opt_termins stmts opt_termins  */
#line 110 "parse.y"
                                                { (yyval.nd) = (yyvsp[-1].nd);  }
#line 1464 "y.tab.c"
    break;

  case 4: /* prgm: opt_termins  */
#line 111 "parse.y"
                                                        { (yyval.nd) = NULL; }
#line 1470 "y.tab.c"
    break;

  case 5: /* stmts: stmt  */
#line 113 "parse.y"
                                                        { (yyval.nd) = new_node_stmt((yyvsp[0].nd)); }
#line 1476 "y.tab.c"
    break;

  case 6: /* stmts: stmts termins stmt  */
#line 115 "parse.y"
                                        {
					(yyval.nd) = pushback_node_stmt((yyvsp[-2].nd), new_node_stmt((yyvsp[0].nd)));
					}
#line 1484 "y.tab.c"
    break;

  case 7: /* stmt: assign_stmt  */
#line 119 "parse.y"
                                                { /*printf("ASSIGN STMT!!!"); */ (yyval.nd) = (yyvsp[0].nd); }
#line 1490 "y.tab.c"
    break;

  case 8: /* stmt: if_stmt  */
#line 120 "parse.y"
                                                                { /*printf("IF STMT!!!"); */ (yyval.nd) = (yyvsp[0].nd); }
#line 1496 "y.tab.c"
    break;

  case 9: /* stmt: expr  */
#line 121 "parse.y"
                                                                { /*printf("JUST STMT!!!"); */ (yyval.nd) = (yyvsp[0].nd); }
#line 1502 "y.tab.c"
    break;

  case 10: /* expr: expr AND expr  */
#line 123 "parse.y"
                                        { (yyval.nd) = new_node_op("AND", (yyvsp[-2].nd), (yyvsp[0].nd)); (yyval.nd) = node_set_loc( (yyval.nd), &(yyloc) ); }
#line 1508 "y.tab.c"
    break;

  case 11: /* expr: expr OR expr  */
#line 124 "parse.y"
                                                        { (yyval.nd) = new_node_op("OR", (yyvsp[-2].nd), (yyvsp[0].nd)); (yyval.nd) = node_set_loc( (yyval.nd), &(yyloc) ); }
#line 1514 "y.tab.c"
    break;

  case 12: /* expr: arg  */
#line 125 "parse.y"
                                                                { (yyval.nd) = (yyvsp[0].nd); }
#line 1520 "y.tab.c"
    break;

  case 13: /* arg: fcall  */
#line 127 "parse.y"
                                                { (yyval.nd) = (yyvsp[0].nd); }
#line 1526 "y.tab.c"
    break;

  case 14: /* arg: arg OP_PLUS arg  */
#line 128 "parse.y"
                                                        { (yyval.nd) = new_node_op("PLUS", (yyvsp[-2].nd), (yyvsp[0].nd)); (yyval.nd) = node_set_loc( (yyval.nd), &(yyloc) ); }
#line 1532 "y.tab.c"
    break;

  case 15: /* arg: arg OP_SUB arg  */
#line 129 "parse.y"
                                                        { (yyval.nd) = new_node_op("SUB", (yyvsp[-2].nd), (yyvsp[0].nd)); (yyval.nd) = node_set_loc( (yyval.nd), &(yyloc) ); }
#line 1538 "y.tab.c"
    break;

  case 16: /* arg: arg OP_MULT arg  */
#line 130 "parse.y"
                                                        { (yyval.nd) = new_node_op("MULT", (yyvsp[-2].nd), (yyvsp[0].nd)); (yyval.nd) = node_set_loc( (yyval.nd), &(yyloc) ); }
#line 1544 "y.tab.c"
    break;

  case 17: /* arg: arg OP_DIV arg  */
#line 131 "parse.y"
                                                        { (yyval.nd) = new_node_op("DIV", (yyvsp[-2].nd), (yyvsp[0].nd)); (yyval.nd) = node_set_loc( (yyval.nd), &(yyloc) ); }
#line 1550 "y.tab.c"
    break;

  case 18: /* arg: arg OP_MOD arg  */
#line 132 "parse.y"
                                                        { (yyval.nd) = new_node_op("MOD", (yyvsp[-2].nd), (yyvsp[0].nd)); (yyval.nd) = node_set_loc( (yyval.nd), &(yyloc) ); }
#line 1556 "y.tab.c"
    break;

  case 19: /* arg: arg FACTOR  */
#line 133 "parse.y"
                                                        { (yyval.nd) = new_node_uniop("FACTOR", (yyvsp[-1].nd)); (yyval.nd) = node_set_loc( (yyval.nd), &(yyloc) ); }
#line 1562 "y.tab.c"
    break;

  case 20: /* arg: arg OP_POWER arg  */
#line 134 "parse.y"
                                                        { (yyval.nd) = new_node_op("POWER", (yyvsp[-2].nd), (yyvsp[0].nd)); (yyval.nd) = node_set_loc( (yyval.nd), &(yyloc) ); }
#line 1568 "y.tab.c"
    break;

  case 21: /* arg: arg OP_EQ arg  */
#line 135 "parse.y"
                                                { (yyval.nd) = new_node_op("EQ", (yyvsp[-2].nd), (yyvsp[0].nd)); (yyval.nd) = node_set_loc( (yyval.nd), &(yyloc) ); }
#line 1574 "y.tab.c"
    break;

  case 22: /* arg: arg OP_NEQ arg  */
#line 136 "parse.y"
                                                        { (yyval.nd) = new_node_op("NEQ", (yyvsp[-2].nd), (yyvsp[0].nd)); (yyval.nd) = node_set_loc( (yyval.nd), &(yyloc) ); }
#line 1580 "y.tab.c"
    break;

  case 23: /* arg: arg OP_GT arg  */
#line 137 "parse.y"
                                                { (yyval.nd) = new_node_op("GT", (yyvsp[-2].nd), (yyvsp[0].nd)); (yyval.nd) = node_set_loc( (yyval.nd), &(yyloc) ); }
#line 1586 "y.tab.c"
    break;

  case 24: /* arg: arg OP_LT arg  */
#line 138 "parse.y"
                                                { (yyval.nd) = new_node_op("LT", (yyvsp[-2].nd), (yyvsp[0].nd)); (yyval.nd) = node_set_loc( (yyval.nd), &(yyloc) ); }
#line 1592 "y.tab.c"
    break;

  case 25: /* arg: arg OP_GE arg  */
#line 139 "parse.y"
                                                { (yyval.nd) = new_node_op("GE", (yyvsp[-2].nd), (yyvsp[0].nd)); (yyval.nd) = node_set_loc( (yyval.nd), &(yyloc) ); }
#line 1598 "y.tab.c"
    break;

  case 26: /* arg: arg OP_LE arg  */
#line 140 "parse.y"
                                                { (yyval.nd) = new_node_op("LE", (yyvsp[-2].nd), (yyvsp[0].nd)); (yyval.nd) = node_set_loc( (yyval.nd), &(yyloc) ); }
#line 1604 "y.tab.c"
    break;

  case 27: /* arg: OP_SUB arg  */
#line 141 "parse.y"
                                                        { (yyval.nd) = new_node_uniop("UMINUS", (yyvsp[0].nd)); (yyval.nd) = node_set_loc( (yyval.nd), &(yyloc) ); }
#line 1610 "y.tab.c"
    break;

  case 28: /* arg: primary REXP_MATCH primary  */
#line 142 "parse.y"
                                                                { (yyval.nd) = new_node_op("REXP_MATCH", (yyvsp[-2].nd), (yyvsp[0].nd)); (yyval.nd) = node_set_loc( (yyval.nd), &(yyloc) ); }
#line 1616 "y.tab.c"
    break;

  case 29: /* arg: primary  */
#line 143 "parse.y"
                                                                { (yyval.nd) = (yyvsp[0].nd); }
#line 1622 "y.tab.c"
    break;

  case 30: /* primary: IDENT  */
#line 146 "parse.y"
                        {
			(yyval.nd) = new_node_ident( (yyvsp[0].id) ); 
			var_hash_add_name( &(p->vars) , (yyvsp[0].id) );
			var_hash_add_name( &(p->rhsvars) , (yyvsp[0].id) );
			(yyval.nd) = node_set_loc( (yyval.nd), &(yyloc) ); 
			}
#line 1633 "y.tab.c"
    break;

  case 31: /* primary: LIT_NUM  */
#line 152 "parse.y"
                                                                { (yyval.nd) = (yyvsp[0].nd); (yyval.nd) = node_set_loc( (yyval.nd), &(yyloc) ); }
#line 1639 "y.tab.c"
    break;

  case 32: /* primary: LIT_STR  */
#line 153 "parse.y"
                                                                { (yyval.nd) = new_node_str( (yyvsp[0].str) , p->ptrtable ); (yyval.nd) = node_set_loc( (yyval.nd), &(yyloc) ); }
#line 1645 "y.tab.c"
    break;

  case 33: /* primary: LIT_REXP  */
#line 154 "parse.y"
                                                                { (yyval.nd) = new_node_rexp( (yyvsp[0].str) , p->ptrtable , p->rexp_encoding ); (yyval.nd) = node_set_loc( (yyval.nd), &(yyloc) ); }
#line 1651 "y.tab.c"
    break;

  case 34: /* primary: '(' expr ')'  */
#line 155 "parse.y"
                                                        { (yyval.nd) = (yyvsp[-1].nd); }
#line 1657 "y.tab.c"
    break;

  case 35: /* primary: NA_NUM  */
#line 156 "parse.y"
                                                                { (yyval.nd) = (yyvsp[0].nd); }
#line 1663 "y.tab.c"
    break;

  case 36: /* fcall: fname '(' args ')'  */
#line 158 "parse.y"
                                        { (yyval.nd) = new_node_fcall((yyvsp[-3].nd), (yyvsp[-1].nd)); (yyval.nd) = node_set_loc( (yyval.nd), &(yyloc) ); }
#line 1669 "y.tab.c"
    break;

  case 37: /* fname: IDENT  */
#line 160 "parse.y"
                                                { (yyval.nd) = new_node_ident((yyvsp[0].id)); (yyval.nd) = node_set_loc( (yyval.nd), &(yyloc) ); }
#line 1675 "y.tab.c"
    break;

  case 38: /* args: %empty  */
#line 162 "parse.y"
                                        { (yyval.nd) = new_node_null(); }
#line 1681 "y.tab.c"
    break;

  case 39: /* args: expr  */
#line 163 "parse.y"
                                                        { 
								(yyval.nd) = new_node_farg((yyvsp[0].nd)); 
								}
#line 1689 "y.tab.c"
    break;

  case 40: /* args: args COMMA expr  */
#line 167 "parse.y"
                                        {
					(yyval.nd) = pushback_node_farg((yyvsp[-2].nd), new_node_farg((yyvsp[0].nd)));
					}
#line 1697 "y.tab.c"
    break;

  case 41: /* if_stmt: KEY_IF condition then_stmts opt_else  */
#line 176 "parse.y"
                                        {
					(yyval.nd) = new_node_if( (yyvsp[-2].nd), (yyvsp[-1].nd), (yyvsp[0].nd) );
					}
#line 1705 "y.tab.c"
    break;

  case 42: /* condition: '(' expr ')' opt_termin  */
#line 180 "parse.y"
                                                        { (yyval.nd) = (yyvsp[-2].nd) ; }
#line 1711 "y.tab.c"
    break;

  case 43: /* then_stmts: stmt TERMIN  */
#line 182 "parse.y"
                                                                { (yyval.nd) = (yyvsp[-1].nd); }
#line 1717 "y.tab.c"
    break;

  case 44: /* then_stmts: '{' prgm '}'  */
#line 183 "parse.y"
                                                { (yyval.nd) = (yyvsp[-1].nd); }
#line 1723 "y.tab.c"
    break;

  case 45: /* opt_else: %empty  */
#line 186 "parse.y"
                  { (yyval.nd) = NULL; }
#line 1729 "y.tab.c"
    break;

  case 46: /* opt_else: KEY_ELSE stmt  */
#line 187 "parse.y"
                                                        { (yyval.nd) = (yyvsp[0].nd); }
#line 1735 "y.tab.c"
    break;

  case 47: /* opt_else: KEY_ELSE '{' prgm '}'  */
#line 188 "parse.y"
                                                { (yyval.nd) = (yyvsp[-1].nd); }
#line 1741 "y.tab.c"
    break;

  case 48: /* assign_stmt: lvar ASSIGN expr  */
#line 191 "parse.y"
                                        { (yyval.nd) = new_node_let((yyvsp[-2].nd), (yyvsp[0].nd)); }
#line 1747 "y.tab.c"
    break;

  case 49: /* lvar: IDENT  */
#line 194 "parse.y"
                                        {
					(yyval.nd) = new_node_ident( (yyvsp[0].id) );
					var_hash_add_name( &(p->vars) , (yyvsp[0].id) );
					var_hash_add_name( &(p->lhsvars) , (yyvsp[0].id) );
					(yyval.nd) = node_set_loc( (yyval.nd), &(yyloc) ); 
					}
#line 1758 "y.tab.c"
    break;


#line 1762 "y.tab.c"

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
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;
  *++yylsp = yyloc;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      yyerror (&yylloc, p, scanner, YY_("syntax error"));
    }

  yyerror_range[1] = yylloc;
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
                      yytoken, &yylval, &yylloc, p, scanner);
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
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
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
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;

      yyerror_range[1] = *yylsp;
      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp, yylsp, p, scanner);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  yyerror_range[2] = yylloc;
  ++yylsp;
  YYLLOC_DEFAULT (*yylsp, yyerror_range, 2);

  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (&yylloc, p, scanner, YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval, &yylloc, p, scanner);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp, yylsp, p, scanner);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif

  return yyresult;
}

#line 211 "parse.y"


void yyerror(YYLTYPE* loc , parser_state* p, void* scanner, char const* message)
{
  p->yynerrs++;
  fprintf(stderr, "%s (near line: %d column: %d )\n", message , loc->first_line, loc->first_column );
}

TreeNode*
node_set_loc( TreeNode* nd, YYLTYPE* loc)
{
  nd->loc.first_line   = loc->first_line;
  nd->loc.first_column = loc->first_column;
  nd->loc.last_line    = loc->last_line;
  nd->loc.last_column  = loc->last_column;
  return nd;
}

