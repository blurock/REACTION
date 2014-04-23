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
