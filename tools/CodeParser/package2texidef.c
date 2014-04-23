/*  FILE     package2texidef.c
**  PACKAGE  Maintenance
**  AUTHOR   Andreas Neubacher
**
**  CONTENT
**    (f)lex program that translates STURM package and class comments to texinfo
**    descriptions -- definitions.
**
**  REFERENCES
**    Based on Wolfgang Stoecher's previous implementation.
**
**  COPYRIGHT (C) 1995  STURM Project, RISC Linz
*/

#include <stdio.h>
#include <string.h>

/*
** Definitions
*/

#define OFILE_NAME  "Overview.texi"
#define CFILE_NAME  "Classes.texi"

#define REPARSE()    yyless(0)
#define NOECHO()     /* do nothing */
#define TXTOUT(x)    fputs(x, stdout)

/*
** Global Variables
*/

static char* ProgramName = 0; /* "package2texi" */
static char* filename = 0;    /* Current source file */
static char* classname = 0;   /* Current class */
static int   pipe_output = 0; /* 1 ... pipe to stdout, 0 ... write to file */
static int   IsClassFile = 0; /* 1 ... stdout is classfile, 0 ... otherwise */
static char* today;           /* current date */

/*
** Misc. Functions
*/

void
Error(char* txt)
{
  fprintf(stderr,
	  "%s: %s\n\tat token \"%s\"\n\tclass \"%s\", file \"%s\"\n",
	  ProgramName, txt, yytext, classname, filename);
}

/*
** Start Condition Functions
*/

#include "scdef.c"

/*
** String and Text Functions
*/

#include "textdef.c"

/*
** Main package comment
*/

void
BeginMAIN_COMMENT(void)
{
  if (!pipe_output) {
    if (freopen(OFILE_NAME, "w", stdout) == 0) {
      Error("Can't open overview file for writing!");
      exit(1);
    }
  }
  printf("
@c
@c This file was automatically produced at %.24s by
@c %s
@c", today, ProgramName);
  BEGIN(MAIN_COMMENT);
}

void
EndMAIN_COMMENT(void)
{
}

void
SectionOUT(char* h)
{
  int i;
  for (i=0; i<strlen(h) && (h[i]=='*' || isspace(h[i])); i++)  /* skip space */
    ;
  h += i;
  i=0;
  do { /* capitalize section header */
    for (i++; i<strlen(h) && !isspace(h[i]); i++)
      h[i] = tolower(h[i]);
    while (i<strlen(h) && h[i] != '\n' && isspace(h[i]))
      i++;
  } while (i<strlen(h) && h[i] != '\n');
  if (i<strlen(h))  h[i] = 0;
  TXTOUT("\n@subsection ");
  TXTOUT(h);
  TXTOUT("\n\n");
}

void
BeginCLASS_NAME(void)
{
  ResetIndentStack();
  if (IsClassFile) {
    BEGIN(CLASS_NAME);
    return;
  }
  IsClassFile = 1;
  if (!pipe_output) {
    if (freopen(CFILE_NAME, "w", stdout) == 0) {
      Error("Can't open class file for writing!");
      exit(1);
    }
  }
  printf("
@c
@c This file was automatically produced at %.24s by
@c %s
@c", today, ProgramName);
  TXTOUT("\n@subsection Classes\n\n");
  BEGIN(CLASS_NAME);
}

void
ClassOUT()
{
  classname = StrCpy(classname, yytext);
  TXTOUT("@deftp Class {");
  TXTOUT(classname);
  TXTOUT("}\n");
}

void
HeaderOUT(char* h)
{
  TXTOUT("\n@strong{");
  TXTOUT(h);
  TXTOUT("}@*");
}

void
EndCLASS(void)
{
  TXTOUT("@end deftp\n\n");
}
