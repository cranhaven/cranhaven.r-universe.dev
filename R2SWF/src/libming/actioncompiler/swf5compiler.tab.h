/* A Bison parser, made by GNU Bison 2.5.  */

/* Bison interface for Yacc-like parsers in C
   
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

extern YYSTYPE swf5lval;


