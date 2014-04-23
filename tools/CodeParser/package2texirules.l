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
