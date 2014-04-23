/*  FILE     sturm2texidef.c
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

#include <stdio.h>
#include <string.h>

/*
** Definitions
*/

#define OUTFILE_NAME    "sturm2texi.texi"
#define SUBSTFILE_NAME  "sturm2texi_"

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
static int   pipe_output = 0; /* 1 ... pipe to stdout, 0 ... write to file */
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
static int   NoSynopsis = 0;  /* 1 ... don't add synopsis, 0 ... add synopsis */
static char* Synopsis = 0;    /* Function synopsis text */

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
  DescArgNum = 0;
  NoSynopsis = 0;
  ResetIndentStack();
  Synopsis = StrCpy(Synopsis, "@code{");
  BEGIN(SYNOPSIS_NAME);
}

void
AddSynReturn(char* s)
{
  int i;
  for (i = strlen(s)-2; i >= 0 && isspace(s[i]); i--)
    ;
  s[i+1] = 0;
  Synopsis = StrCat(Synopsis, "@var{");
  Synopsis = StrCat(Synopsis, s);
  Synopsis = StrCat(Synopsis, "} = ");
}

void
AddSynVar(char* s)
{
  Synopsis = StrCat(Synopsis, "@var{");
  Synopsis = StrCat(Synopsis, s);
  Synopsis = StrCat(Synopsis, "}");
}

void
AddSynFunction(char* s)
{
  Synopsis = StrCat(Synopsis, s);
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
    TXTOUT("{Operator on} ");
  }
  else if (ReturnVarFlag) {
    TXTOUT("{Function on} ");
  }
  else {
    TXTOUT("{Procedure on} ");
  }
  TXTOUT("{");
  ClassStr = StrCpy(ClassStr, CLASSSUBST);
  ClassStr = StrInt(ClassStr, FuncCnt);
  ClassStr = StrCat(ClassStr, "xx");
  TXTOUT(ClassStr);
  TXTOUT("} {");
  for (i=yyleng; i>0 && yytext[i]!=':'; i--);
  if (i>0 && yytext[i] == ':')  i++;
  TXTOUT(&(yytext[i]));
  TXTOUT("} ");
  funcname = StrCpy(funcname, yytext);
}

void
EndFunction(void)
{
  TXTOUT("@end defop\n\n");
}

void
VAROUT(void)
{
  TXTOUT("@var{");
  TXTOUT(yytext);
  TXTOUT("}");
}

void
EndSYNOPSIS_TXT(void)
{
  if (NoSynopsis)  return;
  Synopsis = StrCat(Synopsis, "}");
  TXTOUT(": ");
  TXTOUT(Synopsis);
}

void
BeginSYNOPSIS_DARG(void)
{
  TXTOUT("\n@table @code\n");
  BEGIN(SYNOPSIS_DARG);
}

void
EndSYNOPSIS_DARG(void)
{
  TXTOUT("@end table\n");
}

void
TypeSubstOUT()
{
  TypeStr = StrCpy(TypeStr, TYPESUBST);
  TypeStr = StrInt(TypeStr, ArgsCnt+DescArgNum);
  TypeStr = StrCat(TypeStr, "xx");
  TXTOUT(TypeStr);
  TXTOUT("\n");
}

void
HeaderOUT(char* h)
{
  TXTOUT("\n@strong{");
  TXTOUT(h);
  TXTOUT("}@*");
}

void
BeginPROTO_RET(void)
{
  ClassSubstDone = 0;
  StrClear(ReturnType);
  BEGIN(PROTO_RET);
}

void
BeginPROTO_ARGS(void)
{
  ProtoArgNum = 0;
  StrClear(TypeStr);
  BEGIN(PROTO_ARGS);
}

char*
GetClass(char* c, char* s, int skipid)
{
  char* ss;
  char* sc;
  while (isspace(*s))  s++;
  if (!skipid)
    ss = s + strlen(s) - 1;
  else {
    if (IsOperator) { /* Skip identifier (operator name) */
      ss = strstr(s, "operator");
      if (ss == 0)  ss = s + StrLen(s) - 1;
      else          ss--;
    }
    else { /* Skip identifier (function name) */
      int i;
      i = StrLen(s) - 2;  /* Skip trailing '(' */
      while (i>=0 && (isalnum(s[i]) || s[i] == '_'))
	i--;
      if (i<0)  i = -1;
      ss = s+i;
    }
  }
  sc = strstr(s, "const");
  if (sc != 0 && (isalnum(*(sc+5)) || *(sc+5) == '_'))
    sc = 0;
  if (sc == s) {
    s += 5;
    while (isspace(*s))  s++;
    sc = strstr(s, "const");
    if (sc != 0 && (isalnum(*(sc+5)) || *(sc+5) == '_'))
      sc = 0;
  }
  if (sc != 0)  ss = sc - 1;
  while (ss >= s &&
	 (isspace(*ss) || *ss == '&' || *ss == '*' || *ss == ':' || *ss == '~'))
    ss--;
  ss++;
  *ss = 0;
  return StrCpy(c, s);
}

void
GetType(void)
{
  int i, j;
  i = StrLen(TypeStr)-2;
  while (i>=0 && isspace(TypeStr[i]))  /* Skip blanks */
      i--;
  while (i>=0 && (isalnum(TypeStr[i]) || TypeStr[i] == '_'))  /* Skip identifier */
      i--;
  while (i>=0 && isspace(TypeStr[i]))  /* Skip blanks */
      i--;
  if (i<0) {
    StrClear(TypeStr);
    return;
  }
  TypeStr[i+1] = 0;
  for (j=0; TypeStr[j]!=0 && isspace(TypeStr[j]); j++)  /* Skip leading blanks */
    ;
  for (i=0; TypeStr[j]!=0; i++,j++) {
    if (isspace(TypeStr[j]))  TypeStr[i] = ' ';
    else                      TypeStr[i] = TypeStr[j];
  }
  TypeStr[i] = 0;
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
  fprintf(SubstFile, "s/%s%dxx/%s/1\n", TYPESUBST, ArgsCnt+DescArgNum, SubstStr);
}

void
AddClassSubst()
{
  if (ClassStr == 0)  return;
  IncrementSubstCnt();
  AmpersandSubst(ClassStr);
  fprintf(SubstFile, "s/%s%dxx/%s/1\n", CLASSSUBST, FuncCnt, SubstStr);
  ClassSubstDone = 1;
}

void
AddTypeSubst(void)
{
  if (StrLen(TypeStr) == 0)  return;
  ProtoArgNum++;
  IncrementSubstCnt();
  AmpersandSubst(TypeStr);
  fprintf(SubstFile, "s/%s%dxx/%s/1\n", TYPESUBST, ArgsCnt+ProtoArgNum, SubstStr);
}
