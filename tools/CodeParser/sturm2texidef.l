/*  FILE     sturm2texidef.l
**  PACKAGE  Maintenance
**  AUTHOR   Andreas Neubacher
**
**  CONTENT
**    (f)lex program that translates STURM function comments to texinfo
**    descriptions -- definitions.
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
%x TXT_MAIN PROTO_NAME PROTO_ARGS
%x TXT_TEXT TXT_VERBATIM TXT_BOLD TXT_CODE TXT_MATH TXT_LIST TXT_VAR

%{
#include <stdio.h>
#include <string.h>

/*
** Definitions
*/

#define OUTFILE_NAME    "sturm2texi.texi"
#define SUBSTFILE_NAME  "sturm2texi"

#define MAX_SUBST  150 /* Max. # of lines in SubstFile; constraint of "sed". */
#define CLASSSUBST "xxClass_"
#define TYPESUBST  "xxType_"

#define REPARSE()    yyless(0)
#define NOECHO()     /* do nothing */
#define TXTOUT(x)    fputs(x, stdout)

/*
** Global Variables
*/

static char* ProgramName = 0; /* "sturm2texi" */
static char* filename = 0;    /* Current source file */
static char* funcname = 0;    /* Current function */
static int   pipe_output = 1; /* 1 ... pipe to stdout, 0 ... write to file */
static int   Max_Subst = MAX_SUBST; /* Real max. # of lines in SubstFile */ 
static int   SubstCnt = 0;    /* # of lines in current SubstFile */
static int   SubstFileCnt = 0; /* # of SubstFiles */
static char* SubstFileName = 0; /* Current substitution file name */
static FILE* SubstFile;       /* Substitution file pointer */
static char* SectionFileName = 0; /* Current section */
static int   IsOperator = 0;  /* 1 ... function is operator, 0 ... otherwise */
static int   ReturnVarFlag = 0; /* 1 ... return variable, 0 ... void return */
static int   ClassSubstDone = 0; /* 1 ... class subst. done, 0 ... not yet */
static int   FuncCnt = 0;     /* Global # of functions seen */
static int   ArgsCnt = 0;     /* Global # of arguments seen */
static int   SynArgNum = 0;   /* Arguments in synopsis */
static int   DescArgNum = 0;  /* Arguments in description */
static int   ProtoArgNum = 0; /* Arguments in function prototype */
static char* ClassStr = 0;    /* Class substitution string */
static char* TypeStr = 0;     /* Type substitution string */
static char* SubstStr = 0;    /* Substitution string */
static char* ReturnType = 0;  /* Return type string */ 

/*
** Misc. Functions
*/

void
Error(char* txt)
{
  fprintf(stderr,
	  "%s: %s\n\tat token \"%s\", function \"%s\"\n\tsection\"%s\", file \"%s\"\n",
	  ProgramName, txt, yytext, funcname, SectionFileName, filename);
}


extern char* StrCpy(char*, char*);
extern char* StrCat(char*, char*);

void
NewSection(char* f)
{
  int l;
  if (pipe_output)  return;
  for (l = strlen(f); f[l]==' '; l--)
    f[l] = 0;
  SectionFileName = StrCpy(SectionFileName, f);
  SectionFileName = StrCat(SectionFileName, ".texi");
  if (freopen(SectionFileName, "a", stdout) == 0) {
    Error("Can't open section file for appending!");
    exit(1);
  }
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
** Function Synopsis
*/

void
BeginSYNOPSIS_NAME(void)
{
  IsOperator = 0;
  ReturnVarFlag = 0;
  SynArgNum = 0;
  BEGIN(SYNOPSIS_NAME);
}

void
SetReturnVariable(void)
{
  ReturnVarFlag = 1;
}

void
BeginFunction(void)
{
  int i;

  FuncCnt++;
  TXTOUT("@defop ");
  if (IsOperator) {
    TXTOUT("Operator ");
  }
  else if (ReturnVarFlag) {
    TXTOUT("Function ");
  }
  else {
    TXTOUT("Procedure ");
  }
  TXTOUT("{");
  ClassStr = StrCpy(ClassStr, CLASSSUBST);
  ClassStr = StrInt(ClassStr, FuncCnt);
  TXTOUT(ClassStr);
  TXTOUT("} {");
  for (i=yyleng; i>0 && yytext[i]!=':'; i--);
  if (yytext[i] == ':')  i++;
  TXTOUT(&(yytext[i]));
  TXTOUT("} ");
  funcname = StrCpy(funcname, yytext);
}

void
VAROUT(void)
{
  TXTOUT("@var{");
  TXTOUT(yytext);
  TXTOUT("}");
}

void
BeginSYNOPSIS_DARG(void)
{
  DescArgNum = 0;
  TXTOUT("\n@table @code\n");
  BEGIN(SYNOPSIS_DARG);
}

void
EndSYNOPSIS_DARG(void)
{
  TXTOUT("@end table\n");
  BEGIN(SYNOPSIS_DARG);
}

void
TypeSubstOUT()
{
  TypeStr = StrCpy(TypeStr, TYPESUBST);
  TypeStr = StrInt(TypeStr, ArgsCnt+DescArgNum);
  TXTOUT(TypeStr);
  TXTOUT("\n");
}

void
HeaderOUT(char* h)
{
  TXTOUT("\n@strong{");
  TXTOUT(h);
  TXTOUT("}\n");
}

void
BeginPROTO_NAME(void)
{
  ClassSubstDone = 0;
  StrClear(ReturnType);
  BEGIN(PROTO_NAME);
}

void
BeginPROTO_ARGS(void)
{
  ProtoArgNum = 0;
  StrClear(TypeStr);
  BEGIN(PROTO_ARGS);
}

char*
GetClass(char* s)
{
  int i, j;
  i = yyleng-2;  /* Skip trailing '(' */
  while (i>=0 && (isalnum(yytext[i]) || yytext[i] == '_'))  /* Skip identifier */
      i--;
  if (i<0)  i = -1;
  yytext[i+1] = 0;
  return StrCpy(s, yytext);
}

void
GetType(void)
{
  int i, j;
  i = StrLen(TypeStr)-1;
  while (i>=0 && (isalnum(TypeStr[i]) || TypeStr[i] == '_'))  /* Skip identifier */
      i--;
  if (i<0)  i = -1;
  TypeStr[i+1] = 0;
  for (j=0; TypeStr[j]!=0 && isspace(TypeStr[j]); j++)  /* Skip leading blanks */
    ;
  for (i=0; TypeStr[j]!=0; i++,j++) {
    if (isspace(TypeStr[j]))  TypeStr[i] = ' ';
    else                      TypeStr[i] = TypeStr[j];
  }
  TypeStr[i] = 0
}

/*
** Substitution file functions
*/

void
IncrementSubstCnt(void)
{
  if (SubstCnt == 0) {
    SubstFileCnt++;
    SubstFileName = StrCpy(SubstFileName, SUBSTFILE_NAME);
    SubstFileName = StrInt(SubstFileName, SubstFileCnt);
    SubstFileName = StrCat(SubstFileName, ".sub");
    SubstFile = fopen(SubstFileName, "w");
    if (SubstFile == 0) {
      fprintf(stderr, "%s: Can't open file \"%s\" for writing!\n",
	      ProgramName, SubstFileName);
      exit(1);
    }
    SubstCnt++;
    return;
  }
  SubstCnt++;
  if (SubstCnt > Max_Subst)  SubstCnt = 0;
}

void
AmpersandSubst(char* s)
{
  int i, j;
  for (i=0,j=0; s[i]!=0; i++)
    if (s[i] == '&')  j++;
  SubstStr = StrRealloc(SubstStr, StrLen(s)+j+1);
  for (i=0,j=0; s[i]!=0; i++,j++) {
    if (s[i] == '&') {  /* Replace '&' by '\&' */
      SubstStr[j] = '\\';
      j++;
    }
    SubstStr[j] = s[i];
  }
  SubstStr[j] = 0;
}

void
AddReturnSubst()
{
  IncrementSubstCnt();
  AmpersandSubst(yytext);
  fprintf(SubstFile, "s/%s%d/%s/1\n", TYPESUBST, ArgsCnt+DescArgNum, SubstStr);
}

void
AddClassSubst(char* s)
{
  if (s == 0)  return;
  IncrementSubstCnt();
  AmpersandSubst(s);
  fprintf(SubstFile, "s/%s%d/%s/1\n", CLASSSUBST, FuncCnt, SubstStr);
  ClassSubstDone = 1;
}


%}
