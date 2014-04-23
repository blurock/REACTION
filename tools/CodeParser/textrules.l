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
