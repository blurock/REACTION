/*  FILE     package2texirules.l
**  PACKAGE  Maintenance
**  AUTHOR   Andreas Neubacher
**
**  CONTENT
**    (f)lex program that translates STURM header and class comments to texinfo
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

%x CPLUSPLUS FILENAME FIND_OVERVIEW MAIN_COMMENT SKIP_COMMENT
%x CLASS_NAME CLASS_TXT TXT_MAIN
%x TXT_TEXT TXT_VERBATIM TXT_BOLD TXT_CODE TXT_MATH TXT_LIST TXT_VAR

/*
** Pattern definitions
*/

CIDENT		([a-zA-Z_][a-zA-Z0-9_]*)
SPACE		[ \t\n]
TEMPL		("<"([^<>]*("<"([^<>]*("<"[^<>]*">")?)*">")?)*">")
CPPCIDENT	("::"?{CIDENT}{TEMPL}?)


/*
** Definitions, global variables, functions
*/

%{
#include "package2texidef.c"
%}

%%

 /*
 ** RULES
 */

  {
    filename = StrCpy(filename, "NONE YET");
    classname = StrCpy(classname, filename);
    BEGIN(CPLUSPLUS);
  }

 /*
 ** C++ code
 */

<CPLUSPLUS>"/*"[ \t]*"FILE"  BEGIN(FILENAME);
<CPLUSPLUS>"/*C"	     BeginCLASS_NAME();
<CPLUSPLUS>"/*"		     BEGIN(SKIP_COMMENT);
<CPLUSPLUS>.|"\n"	     NOECHO();

 /*
 ** Filename
 */

<FILENAME>[A-Za-z0-9_,.\-]+  {
			     filename = StrCpy(filename, yytext);
			     BEGIN(FIND_OVERVIEW);
			     }
<FILENAME>[ \t]+	     NOECHO();
<FILENAME>.|"\n"	     {
			     Error("Unknown token when reading filename!");
			     REPARSE();
			     BEGIN(CPLUSPLUS);
			     }

 /*
 ** Find "OVERVIEW"
 */

<FIND_OVERVIEW>"**"[ \t]+"OVERVIEW"[ \t]*"\n"  {
			     SetIndent();
			     PushIndent();
			     REPARSE();
			     BeginMAIN_COMMENT();
			     }
<FIND_OVERVIEW>"*/"	     {
			     Error("No \"OVERVIEW\" found!");
			     BEGIN(CPLUSPLUS);
			     }
<FIND_OVERVIEW>.|"\n"	     NOECHO();

 /*
 ** MAIN_COMMENT
 */

<MAIN_COMMENT>"**"[ \t]+"CONTENT".*"\n" {
			     EndMAIN_COMMENT();
			     BEGIN(SKIP_COMMENT);
			     }
<MAIN_COMMENT>"**"[ \t]+"COPYRIGHT".*"\n" {
			     EndMAIN_COMMENT();
			     BEGIN(SKIP_COMMENT);
			     }
<MAIN_COMMENT>"**"([ \t]+[A-Z]+)+[ \t]*"\n" {
			     SetIndent();
			     if (CheckIndent() == 0) {
			       SetExpectIndent();
			       SectionOUT(yytext);
			       RecursiveBegin(MAIN_COMMENT, TXT_TEXT);
			     }
			     else {
			       Error("Illegal indentation change when looking for section header!");
			       EndMAIN_COMMENT();
			       BEGIN(SKIP_COMMENT);
			     }
			     }
<MAIN_COMMENT>"*/"	     {
			     EndMAIN_COMMENT();
			     BEGIN(CPLUSPLUS);
			     }

 /*
 ** C Comment Block
 */

<SKIP_COMMENT>"*/"	     BEGIN(CPLUSPLUS);
<SKIP_COMMENT>.|"\n"	     NOECHO();

 /*
 ** Class name
 */

<CLASS_NAME>{CPPCIDENT}+     {
			     ClassOUT();
			     BEGIN(CLASS_TXT);
			     }
<CLASS_NAME>[ \t]+	     NOECHO();
<CLASS_NAME>.|"\n"	     {
			     Error("Illegal token when parsing class name in \"/*C\" line!");
			     REPARSE();
			     BEGIN(SKIP_COMMENT);
			     }

 /*
 ** Class description text
 */

<CLASS_TXT>" "+"."	     NOECHO();
<CLASS_TXT>"**"		     NOECHO();
<CLASS_TXT>{SPACE}+	     TXTOUT(" ");
<CLASS_TXT>"*"		     BeginBOLD(CLASS_TXT);
<CLASS_TXT>"\""		     BeginCODE(CLASS_TXT);
<CLASS_TXT>"$"		     BeginMATH(CLASS_TXT);
<CLASS_TXT>"`"		     BeginVAR(CLASS_TXT);
<CLASS_TXT>"\\\\"	     TXTOUT("\\");
<CLASS_TXT>"\\\""	     TXTOUT("\"");
<CLASS_TXT>"\\$"	     TXTOUT("$");
<CLASS_TXT>"\\`"	     TXTOUT("`");
<CLASS_TXT>"**"[ \t]+"\n"    NOECHO();
<CLASS_TXT>"**"[ \t]+"DESCRIPTION"[ \t]*"\n"  {
			     SetIndent();
			     PushIndent();
			     TXTOUT("\n\n");
			     REPARSE();
			     BEGIN(TXT_MAIN);
			     }
<CLASS_TXT>"*/"		     {
			     EndCLASS();
			     BEGIN(CPLUSPLUS);
			     }
<CLASS_TXT>.		     TXTOUT(yytext);

 /*
 ** Main class description text
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
			       Error("Illegal indentation change (to the left) in class description text!");
			       EndCLASS();
			       BEGIN(SKIP_COMMENT);
			     }
			     }
<TXT_MAIN>"**"[ \t]+"DESCRIPTION" HeaderOUT("Description");
<TXT_MAIN>"**"[ \t]+"REMARKS"     HeaderOUT("Remarks");
<TXT_MAIN>"**"[ \t]+"IMPLEMENTATION"  HeaderOUT("Implementation");
<TXT_MAIN>"**"[ \t]+"REFERENCES"  HeaderOUT("References");
<TXT_MAIN>"**"[ \t]+"SEE ALSO"    HeaderOUT("See Also");
<TXT_MAIN>"*/"		     {
			     EndCLASS();
			     BEGIN(CPLUSPLUS);
			     }
<TXT_MAIN>.		     {
			     Error("Syntax error in class description!");
			     EndCLASS();
			     BEGIN(SKIP_COMMENT);
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
/*  FILE     package2teximain.l
**  PACKAGE  Maintenance
**  AUTHOR   Andreas Neubacher
**
**  CONTENT
**    (f)lex program that translates STURM package and class comments to texinfo
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
  [-c]     complete file (inclusive header, file names, use with \"-p\" only)
  [-p]     pipe output to stdout, otherwise write to overview and class output
           files (default names: \"%s\", \"%s\")

  If there are no [file] parameters, input is read from stdin.\n",
	   name, OFILE_NAME, CFILE_NAME);
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
  int   arg;
  int   complete_file = 0;
  long  now;

  now = time(0);
  today = ctime(&now);

  ProgramName = strrchr(argv[0], '/');
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
    default:
      usage(ProgramName);
      return 1;
    }
  }

  /* 2. produce output */
  if (pipe_output && complete_file) {
    puts("\\input texinfo   @c -*-texinfo-*-\n@setfilename package2texi.info");
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

  if (complete_file && pipe_output)  puts("\n@bye");

  return 0;
}
