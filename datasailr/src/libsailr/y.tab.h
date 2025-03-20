/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison interface for Yacc-like parsers in C

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

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

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

#line 135 "y.tab.h"

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
