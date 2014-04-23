/*  FILE     sturm2texirules.l
**  PACKAGE  Maintenance
**  AUTHOR   Andreas Neubacher
**
**  CONTENT
**    (f)lex program that translates STURM function comments to texinfo
**    descriptions -- main rules.
**
**  REFERENCES
**    Based on Wolfgang Stoecher's previous implementation.
**
**  COPYRIGHT (C) 1995  STURM Project, RISC Linz
*/

/*
** Start Conditions
*/

%x CPLUSPLUS FILENAME SECTION SKIP_COMMENT
%x SYNOPSIS_NAME SYNOPSIS_ARGS SYNOPSIS_TXT SYNOPSIS_DARG
%x TXT_MAIN PROTO_RET PROTO_NAME PROTO_ARGS
%x TXT_TEXT TXT_VERBATIM TXT_BOLD TXT_CODE TXT_MATH TXT_LIST TXT_VAR

/*
** Pattern definitions
*/

CIDENT		([a-zA-Z_][a-zA-Z0-9_]*)
SPACE		[ \t\n]
TEMPL		("<"([^<>]*("<"([^<>]*("<"[^<>]*">")?)*">")?)*">")
CPPCIDENT	("::"?{CIDENT}{TEMPL}?)
CPPFIDENT	("::"?"~"?{CIDENT})
OPERATOR	("::"?"operator"{SPACE}*.[^ \t\n(]*)


/*
** Definitions, global variables, functions
*/

%{
#include "sturm2texidef.c"
%}

%%

 /*
 ** RULES
 */

  {
    filename = StrCpy(filename, "UNKNOWN");
    funcname = StrCpy(funcname, filename);
    SectionFileName = StrCpy(SectionFileName, filename);
    BEGIN(CPLUSPLUS);
  }

 /*
 ** C++ code
 */

<CPLUSPLUS>"/*"[ \t]*"FILE"  BEGIN(FILENAME);
<CPLUSPLUS>"/*S"	     BEGIN(SECTION);
<CPLUSPLUS>"/*F"	     BeginSYNOPSIS_NAME();
<CPLUSPLUS>"/*"		     BEGIN(SKIP_COMMENT);
<CPLUSPLUS>.|"\n"	     NOECHO();

 /*
 ** Filename
 */

<FILENAME>[A-Za-z0-9_,.\-]+  {
			     filename = StrCpy(filename, yytext);
			     BEGIN(SKIP_COMMENT);
			     }
<FILENAME>[ \t]+	     NOECHO();
<FILENAME>.|"\n"	     {
			     Error("Unknown token when reading filename!");
			     REPARSE();
			     BEGIN(CPLUSPLUS);
			     }

 /*
 ** Section
 */

<SECTION>[A-Za-z][A-Za-z0-9_ ,&()\[\]\-]*  {
			     NewSection(yytext);
			     BEGIN(SKIP_COMMENT);
			     }
<SECTION>[ \t]+		     NOECHO();
<SECTION>.|"\n"		     {
			     Error("Unknown token when reading section name!");
			     REPARSE();
			     BEGIN(CPLUSPLUS);
			     }

 /*
 ** C Comment Block
 */

<SKIP_COMMENT>"*/"	     BEGIN(CPLUSPLUS);
<SKIP_COMMENT>.|"\n"	     NOECHO();

 /*
 ** Return argument and/or function name of synopsis
 */

<SYNOPSIS_NAME>{CPPCIDENT}*{OPERATOR}  {
			     IsOperator = 1;
			     AddSynFunction(yytext);
			     BeginFunction();
			     }
<SYNOPSIS_NAME>{CPPCIDENT}*{CPPFIDENT}  {
			     IsOperator = 0;
			     AddSynFunction(yytext);
			     BeginFunction();
			     }
<SYNOPSIS_NAME>{CIDENT}[ \t]*"="  {
			     SetReturnVariable();
			     AddSynReturn(yytext);
			     }
<SYNOPSIS_NAME>"("	     {
			     /*
			     TXTOUT("(");
			     */
			     Synopsis = StrCat(Synopsis, "(");
			     BEGIN(SYNOPSIS_ARGS);
			     }
<SYNOPSIS_NAME>[ \t]+	     NOECHO();
<SYNOPSIS_NAME>.|"\n"	     {
			     Error("Illegal token when parsing function in \"/*F\" line!");
			     REPARSE();
			     BEGIN(SKIP_COMMENT);
			     }
 /*
 ** Synopsis arguments
 */

<SYNOPSIS_ARGS>{CIDENT}	     {
			     /*
			     VAROUT();
			     */
			     AddSynVar(yytext);
			     SynArgNum++;
			     }
<SYNOPSIS_ARGS>","	     {
			     /*
			     TXTOUT(", ");
			     */
			     Synopsis = StrCat(Synopsis, ", ");
			     }
<SYNOPSIS_ARGS>")"	     {
			     /*
			     TXTOUT(")\n");
			     */
			     TXTOUT("\n");
			     Synopsis = StrCat(Synopsis, ")");
			     BEGIN(SYNOPSIS_TXT);
			     }
<SYNOPSIS_ARGS>[* \t\n]	     NOECHO();
<SYNOPSIS_ARGS>.|"\n"	     {
			     Error("Illegal token in argument list!");
			     BEGIN(SKIP_COMMENT);
			     }

 /*
 ** Synopsis description text
 */

<SYNOPSIS_TXT>(" "+".")+     NOECHO();
<SYNOPSIS_TXT>"**"(" "+".")+ NOECHO();
<SYNOPSIS_TXT>[ \t]+	     TXTOUT(" ");
<SYNOPSIS_TXT>"\n"	     NOECHO();
<SYNOPSIS_TXT>"*"	     BeginBOLD(SYNOPSIS_TXT);
<SYNOPSIS_TXT>"\""	     BeginCODE(SYNOPSIS_TXT);
<SYNOPSIS_TXT>"$"	     BeginMATH(SYNOPSIS_TXT);
<SYNOPSIS_TXT>"`"	     BeginVAR(SYNOPSIS_TXT);
<SYNOPSIS_TXT>"\\\\"	     TXTOUT("\\");
<SYNOPSIS_TXT>"\\\""         TXTOUT("\"");
<SYNOPSIS_TXT>"\\$"          TXTOUT("$");
<SYNOPSIS_TXT>"\\`"	     TXTOUT("`");
<SYNOPSIS_TXT>"**"[ \t]*"\n"  NOECHO();
<SYNOPSIS_TXT>"**"[ \t]+"DESCRIPTION"[ \t]*"\n"  {
			     SetIndent();
			     PushIndent();
			     if (SynArgNum == 0 && !ReturnVarFlag) {
			       EndSYNOPSIS_TXT();
			       TXTOUT("\n\n");
			       BEGIN(TXT_MAIN);
			     }
			     else {
			       EndSYNOPSIS_TXT();
			       TXTOUT("\n");
			       SetExpectIndent();
			       BeginSYNOPSIS_DARG();
			     }
			     }
<SYNOPSIS_TXT>"*/"	     {
			     if (ReturnVarFlag || SynArgNum > 0) {
			       Error("Missing \"DESCRIPTION\"!");
			       EndSYNOPSIS_TXT();
			       TXTOUT("\n");
			       EndFunction();
			       BEGIN(CPLUSPLUS);
			     }
			     else {
			       EndSYNOPSIS_TXT();
			       TXTOUT("\n");
			       EndFunction();
			       BeginPROTO_RET();
			     }
			     }
<SYNOPSIS_TXT>":"[ \n]	     {
			     TXTOUT(yytext);
			     NoSynopsis = 1;
			     }
<SYNOPSIS_TXT>.		     TXTOUT(yytext);

 /*
 ** Synopsis argument description
 */

<SYNOPSIS_DARG>"**"[ \t]+    {
			     int i;
			     SetIndent();
			     i = CheckIndent();
			     if (i > 0) {
			       Error("Internal sturm2texi error after \"DESCRIPTION\"!");
			       BEGIN(SKIP_COMMENT);
			     }
			     else if (i == 0) {
			       if (DescArgNum == SynArgNum + ReturnVarFlag) {
			         PopIndent();
			         REPARSE();
				 EndSYNOPSIS_DARG();
			         BEGIN(TXT_MAIN);
			       }
			       else
			         TXTOUT("@item ");
			     }
			     else {
			       if (DescArgNum < SynArgNum + ReturnVarFlag)
			         Error("Argument description ended by indentation change with arguments missing!");
			       PopIndent();
			       REPARSE();
			       EndSYNOPSIS_DARG();
			       BEGIN(TXT_MAIN);
			     }
			     }
<SYNOPSIS_DARG>{CIDENT}*     {
			     VAROUT();
			     DescArgNum++;
			     if (DescArgNum > SynArgNum + ReturnVarFlag) {
			       Error("Too many arguments found in description!");
			       BEGIN(SKIP_COMMENT);
			     }
			     }
<SYNOPSIS_DARG>","	     TXTOUT(", ");
<SYNOPSIS_DARG>":"	     {
			     TXTOUT(": ");
			     TypeSubstOUT();
			     SetExpectIndent();
			     RecursiveBegin(SYNOPSIS_DARG, TXT_TEXT);
			     }
<SYNOPSIS_DARG>[ \t]	     NOECHO();
<SYNOPSIS_DARG>"*/"	     {
			     PopIndent();
			     REPARSE();
			     EndSYNOPSIS_DARG();
			     BEGIN(TXT_MAIN);
			     }
<SYNOPSIS_DARG>.	     {
			     Error("Syntax error in variable list after \"DESCRIPTION\"!");
			     EndSYNOPSIS_DARG();
			     EndFunction();
			     BEGIN(SKIP_COMMENT);
			     }

 /*
 ** Main description text
 */

<TXT_MAIN>"**"[ \t]*"\n"     TXTOUT("\n");
<TXT_MAIN>"**"[ \t]+	     {
			     int i;
			     SetIndent();
			     i = CheckIndent();
			     if (i > 0) {
			       PushIndent();
			       RecursiveBegin(TXT_MAIN, TXT_TEXT);
			     }
			     else if (i == 0)
			       NOECHO();
			     else {
			       Error("Illegal indentation change (to the left) in description text!");
			       EndFunction();
			       BEGIN(SKIP_COMMENT);
			     }
			     }
<TXT_MAIN>"**"[ \t]+"REMARKS"     HeaderOUT("Remarks");
<TXT_MAIN>"**"[ \t]+"COMPLEXITY"  HeaderOUT("Complexity");
<TXT_MAIN>"**"[ \t]+"REFERENCES"  HeaderOUT("References");
<TXT_MAIN>"**"[ \t]+"SEE ALSO"    HeaderOUT("See Also");
<TXT_MAIN>"**"[ \t]+"HEADERFILE"  HeaderOUT("Headerfile");
<TXT_MAIN>"*/"		     {
			     EndFunction();
			     BeginPROTO_RET();
			     }
<TXT_MAIN>.		     {
			     Error("Syntax error in function comment text after variable description!");
			     EndFunction();
			     BEGIN(SKIP_COMMENT);
			     }

 /*
 ** Prototype return type
 */

<PROTO_RET>"template"{SPACE}*  NOECHO();
<PROTO_RET>"static"{SPACE}*  NOECHO();
<PROTO_RET>"inline"{SPACE}*  NOECHO();
<PROTO_RET>{TEMPL}{SPACE}*   NOECHO();
<PROTO_RET>{SPACE}+	     NOECHO();
<PROTO_RET>(("const"{SPACE}*)?{CPPCIDENT}+({SPACE}*("const"{SPACE}*)?[*&])*) {
			     if (ReturnVarFlag)
			       AddReturnSubst();
			     else if (strstr(yytext, "void") == 0)
			       REPARSE();
			     ReturnType = StrCpy(ReturnType, yytext);
			     BEGIN(PROTO_NAME);
			     }
<PROTO_RET>.		     {
			     Error("Syntax error in parsing return type!");
			     BEGIN(CPLUSPLUS);
			     }

 /*
 ** Prototype function/operator name
 */

<PROTO_NAME>{SPACE}+/[^(]    NOECHO();
<PROTO_NAME>{OPERATOR}	     {
			     if (!IsOperator)
			       Error("Description of operator does not begin with \"operator\"!");
			     IsOperator = 1;
			     yymore();
			     }
<PROTO_NAME>"("		     {
			     if (StrLen(yytext) == 1)  /* Constructor */
			       ClassStr = GetClass(ClassStr, ReturnType, 1);
			     else
			       ClassStr = GetClass(ClassStr, yytext, 1);
			     if (StrLen(ClassStr) > 0)
			       AddClassSubst();
			     BeginPROTO_ARGS();
			     }
<PROTO_NAME>.		     yymore();

 /*
 ** Prototype argument list
 */

<PROTO_ARGS>{SPACE}*(("const"{SPACE}*)?{CPPCIDENT}+({SPACE}*("const"{SPACE}*)?[*&])*){SPACE}*{CIDENT}{SPACE}*"," {
                             TypeStr = StrCat(TypeStr, yytext);
			     GetType();
			     AddTypeSubst();
			     if (!ClassSubstDone && StrLen(TypeStr) > 0) {
			       ClassStr = GetClass(ClassStr, TypeStr, 0);
			       AddClassSubst();
			     }
			     StrClear(TypeStr);
			     }
<PROTO_ARGS>{SPACE}*(("const"{SPACE}*)?{CPPCIDENT}+({SPACE}*("const"{SPACE}*)?[*&])*){SPACE}*{CIDENT}{SPACE}*")" {
                             TypeStr = StrCat(TypeStr, yytext);
			     GetType();
			     AddTypeSubst();
			     if (!ClassSubstDone) {
			       if (StrLen(TypeStr) > 0)
			         ClassStr = GetClass(ClassStr, TypeStr, 0);
			       else
			         ClassStr = GetClass(ClassStr, ReturnType, 0);
			       AddClassSubst();
			     }
			     if (ProtoArgNum > DescArgNum-ReturnVarFlag) {
			       Error("More arguments in prototype than in description!");
			       ArgsCnt += ProtoArgNum;
			     }
			     else if (ProtoArgNum < DescArgNum-ReturnVarFlag) {
			       Error("Fewer arguments in prototype than in description!");
			       ArgsCnt += DescArgNum;
			     }
			     else
			       ArgsCnt += DescArgNum;
			     BEGIN(CPLUSPLUS);
			     }
<PROTO_ARGS>{SPACE}*")"	     {
			     if (!ClassSubstDone) {
			       if (StrLen(TypeStr) > 0)
			         ClassStr = GetClass(ClassStr, TypeStr, 0);
			       else
			         ClassStr = GetClass(ClassStr, ReturnType, 0);
			       AddClassSubst();
			     }
			     if (ProtoArgNum > DescArgNum-ReturnVarFlag) {
			       Error("More arguments in prototype than in description!");
			       ArgsCnt += ProtoArgNum;
			     }
			     else if (ProtoArgNum < DescArgNum-ReturnVarFlag) {
			       Error("Fewer arguments in prototype than in description!");
			       ArgsCnt += DescArgNum;
			     }
			     else
			       ArgsCnt += DescArgNum;
			     BEGIN(CPLUSPLUS);
			     }
<PROTO_ARGS>.		     {
			     Error("Warning: Illegal character in function arguments.");
			     }
 /*  FILE     textrules.l
 **  PACKAGE  Maintenance
 **  AUTHOR   Andreas Neubacher
 **
 **  CONTENT
 **    Rules for the parser for general comment text.
 **
 **    Requires indentation and string functions and
 **    * TXTOUT("string") ... writes "string" to the output file
 **
 **  REFERENCES
 **    Based on Wolfgang Stoecher's previous implementation.
 **
 **  COPYRIGHT (C) 1995  STURM Project, RISC Linz
 */

 /*
 ** Backslashed escapes.
 */

<TXT_TEXT,TXT_BOLD,TXT_CODE,TXT_MATH>"\\\\"    TXTOUT("\\");
<TXT_TEXT,TXT_BOLD>"\\\""                      TXTOUT("\"");
<TXT_TEXT,TXT_BOLD>"\\$"                       TXTOUT("$");
<TXT_TEXT,TXT_BOLD,TXT_CODE,TXT_MATH>"\\`"     TXTOUT("`");

 /*
 ** Text block
 */

<TXT_TEXT>"**"[ \t]*"\n"     TXTOUT("\n");
<TXT_TEXT>"**"[ \t]*"-"[ \t]* {
		 	     int i;
			     SetIndent();
			     i = CheckIndent();
			     if (i > 0)
			       BeginVERBATIM(TXT_TEXT);
			     else if (i == 0) {
			       REPARSE();
			       BeginLIST(TXT_TEXT);
			     }
			     else {
			       PopIndent();
			       REPARSE();
			       RecursiveReturn();
			     }
			     }
<TXT_TEXT>"**"[ \t]*	     {
		 	     int i;
			     SetIndent();
			     i = CheckIndent();
			     if (i > 0)
			       BeginVERBATIM(TXT_TEXT);
			     else if (i == 0)
			       NOECHO();
			     else {
			       PopIndent();
			       REPARSE();
			       RecursiveReturn();
			     }
			     }
<TXT_TEXT>"*"		     BeginBOLD(TXT_TEXT);
<TXT_TEXT>"\""		     BeginCODE(TXT_TEXT);
<TXT_TEXT>"$"		     BeginMATH(TXT_TEXT);
<TXT_TEXT>"`"		     BeginVAR(TXT_TEXT);
<TXT_TEXT>"*/"		     {
			     PopIndent();
			     REPARSE();
			     RecursiveReturn();
			     }
 /* otherwise		     ... output text */

 /*
 ** Bold text.
 */

<TXT_BOLD>"**"[ \t]*	     {
			     int i;
			     SetIndent();
			     i = CheckIndent();
			     if (i == 0)
			       NOECHO();
			     else {
			       Error("Indentation change in bold (*) text!");
			       REPARSE();
			       EndBOLD();
			     }
			     }
<TXT_BOLD>"*/"		     {
			     Error("\"*/\" in bold (*) text!");
			     REPARSE();
			     EndBOLD();
			     }
<TXT_BOLD>"*"		     EndBOLD();
<TXT_BOLD>"\""		     BeginCODE(TXT_BOLD);
<TXT_BOLD>"$"		     BeginMATH(TXT_BOLD);
<TXT_BOLD>"`"		     BeginVAR(TXT_BOLD);
 /* otherwise		     ... output text */

 /*
 ** Code
 */

<TXT_CODE>"**"[ \t]*	     {
			     int i;
			     SetIndent();
			     i = CheckIndent();
			     if (i == 0)
			       NOECHO();
			     else {
			       Error("Indentation change in quoted (\") text!");
			       REPARSE();
			       EndCODE();
			     }
			     }
<TXT_CODE>"*/"		     {
			     Error("\"*/\" in quoted (\") text!");
			     REPARSE();
			     EndCODE();
			     }
<TXT_CODE>"\""		     EndCODE();
<TXT_CODE>"`"		     BeginVAR(TXT_CODE);
 /* otherwise		     ... output text */

 /*
 ** List
 */

<TXT_LIST>"**"[ \t]*"\n"     TXTOUT("\n");
<TXT_LIST>"**"[ \t]*"-"[ \t]*  {
		 	     int i;
			     SetIndent();
			     i = CheckIndent();
			     if (i == 0) {
			       ITEMOUT();
			       SetExpectIndent();
			       RecursiveBegin(TXT_LIST, TXT_TEXT);
			     }
			     else {
			       REPARSE();
			       EndLIST(TXT_LIST);
			     }
			     }
<TXT_LIST>"**"[ \t]*	     {
			     REPARSE();
			     EndLIST();
			     }
<TXT_LIST>"*/"		     {
			     REPARSE();
			     EndLIST();
			     }
<TXT_LIST>.|"\n"	     {
			     Error("Illegal token in list!");
			     REPARSE();
			     EndLIST();
			     }

 /*
 ** Math
 */

<TXT_MATH>"**"[ \t]*	     {
			     int i;
			     SetIndent();
			     i = CheckIndent();
			     if (i == 0)
			       NOECHO();
			     else {
			       Error("Indentation change in math ($) text!");
			       REPARSE();
			       EndMATH();
			     }
			     }
<TXT_MATH>"*/"		     {
			     Error("\"*/\" in math ($) text!");
			     REPARSE();
			     EndMATH();
			     }
<TXT_MATH>"$"		     EndMATH();
<TXT_MATH>"`"		     BeginMATHVAR(TXT_MATH);
<TXT_MATH>"'"		     EndMATHVAR(TXT_MATH);
<TXT_MATH>"^"		     {
			     MathIText = StrCat(MathIText, "^");
			     MathTText = StrCat(MathTText, "@^{ }");
			     }
<TXT_MATH>"_"		     {
			     MathIText = StrCat(MathIText, "_");
			     MathTText = StrCat(MathTText, "@_{ }");
			     }
<TXT_MATH>"{"		     {
			     MathIText = StrCat(MathIText, "@{");
			     MathTText = StrCat(MathTText, "}");
			     }
<TXT_MATH>"}"		     {
			     MathIText = StrCat(MathIText, "@}");
			     MathTText = StrCat(MathTText, "}");
			     }
<TXT_MATH>.		     {
			     MathIText = StrCat(MathIText, yytext);
			     MathTText = StrCat(MathTText, yytext);
			     }

 /*
 ** Variable
 */

<TXT_VAR>"**"[ \t]*	     {
			     int i;
			     SetIndent();
			     i = CheckIndent();
			     if (i == 0)
			       NOECHO();
			     else {
			       Error("Indentation change in variable (`') text!");
			       REPARSE();
			       EndVAR();
			     }
			     }
<TXT_VAR>"*/"		     {
			     Error("\"*/\" in variable (`') text!");
			     REPARSE();
			     EndVAR();
			     }
<TXT_VAR>"'"		     EndVAR();
 /* otherwise		     ... output text */

 /*
 ** Verbatim text.
 */

<TXT_VERBATIM>"**"[ \t]*     {
			     int i;
			     SetIndent();
			     i = CheckIndent();
			     if (i >= 0)
			       NOECHO();
			     else {
			       REPARSE();
			       EndVERBATIM();
			     }
			     }
<TXT_VERBATIM>"*/"	     {
			     REPARSE();
			     EndVERBATIM();
			     }
 /* otherwise		     ... output text */
%%
/*  FILE     sturm2teximain.l
**  PACKAGE  Maintenance
**  AUTHOR   Andreas Neubacher
**
**  CONTENT
**    (f)lex program that translates STURM function comments to texinfo
**    descriptions -- main C function.
**
**  REFERENCES
**    Based on Wolfgang Stoecher's previous implementation.
**
**  COPYRIGHT (C) 1995  STURM Project, RISC Linz
*/

#include <stdio.h>
#include <string.h>
#include <time.h>


/*
** MAIN PROGRAM
*/

/*f usage(name) . . . . . . . . . . . . . . . .  print out options of program
**
**  DESCRIPTION
**    name: program name
**
*/
void
usage(const char *name)
{
  fprintf (stderr, "Usage: %s [options] [file]*

Options:
  [-h]     help (show this usage)
  [-s N]   set max. number of subst. per file to N (constraint of \"sed\")
           default: %d
  [-c]     complete file (inclusive header, file names)
  [-p]     pipe output to stdout, otherwise write to section output files
           (default name: \"%s\")

  If there are no [file] parameters, input is read from stdin.\n",
	   name, MAX_SUBST, OUTFILE_NAME);
}


/*F main (argc, argv) . . . . . . . . . . . . . . . . . . . . .  main routine
**
**  DESCRIPTION 
**    argc:
**    argv: standard arguments to "main()".
*/
int
main(int argc, char **argv)
{
  int   i, arg;
  int   complete_file = 0;
  long  now;
  char* today;

  now = time(0);
  today = ctime(&now);

  ProgramName = strrchr (argv[0], '/');
  if (ProgramName == NULL) {    /* no pathname */
    ProgramName = argv[0];
  } else {
    ProgramName++;
  }

  /* 1. handle options */
  for (arg=1; arg<argc; arg++) {
    if (argv[arg][0] != '-')
      break;
    switch (argv[arg][1]) {
    case 'h':
      usage(ProgramName);
      return 0;
    case 'c':
      complete_file = 1;
      break;
    case 'p':
      pipe_output = 1;
      break;
    case 's':
      arg++;
      if (arg < argc)
	Max_Subst = atoi(argv[arg]);
      else {
	usage(ProgramName);
	return 1;
      }
      break;
    default:
      usage(ProgramName);
      return 1;
    }
  }

  /* 2. output file */
  if (!pipe_output) {
    if (freopen(OUTFILE_NAME, "w", stdout) == NULL) {
      fprintf(stderr, "%s: Can't open file \"%s\" for writing!\n",
	      ProgramName, OUTFILE_NAME);
      return 1;
    }
  }

  /* 3. produce output */
  printf("
@c
@c This file was automatically produced at %.24s by
@c %s", today, ProgramName);
  for (i = 1; i < argc; i++) {
    printf(" %s", argv[i]);
  }
  puts("\n@c");
  if (complete_file) {
    puts("\\input texinfo   @c -*-texinfo-*-\n@setfilename sturm2texi.info");
  }

  yyin = stdin;
  do {
    if (arg < argc) {
      yyin = fopen(argv[arg], "r");
      if (yyin == NULL) {
	fprintf (stderr, "%s: Can't open file \"%s\" for reading\n",
		 ProgramName, argv[arg]);
	return 1;
      }
    }
    arg++;
  }
  while (yylex() == 0 && yyin != stdin && arg < argc);

  if (complete_file)  puts("\n@bye");

  return 0;
}
