/*  FILE     textdef.l
**  PACKAGE  Maintenance
**  AUTHOR   Andreas Neubacher
**
**  CONTENT
**    Definitions for the parser for general comment text.
**
**  REFERENCES
**    Based on Wolfgang Stoecher's previous implementation.
**
**  COPYRIGHT (C) 1995  STURM Project, RISC Linz
*/
 


%x TXT_TEXT TXT_VERBATIM TXT_BOLD TXT_CODE TXT_MATH TXT_VAR

%{
#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/*
** String handling functions.
*/
char *
StrAlloc(int size)
{
  int* s;
  s = (int*)malloc(size+sizeof(int));
  assert(s != 0);
  *s = size;
  s++;
  *(char*)s = 0;
  return (char*)s;
}

char *
StrRealloc(char* s, int size)
{
  int* ss;
  ss = (int*)realloc(((int*)s)-1, size+sizeof(int));
  assert(ss != 0);
  *ss = size;
  ss++;
  return (char*)ss;
}

void
StrFree(char* s)
{
  if (s != 0)  free(((int*)s)-1);
}

int
StrSize(char* s)
{
  return (s==0 ? 0 : *(((int*)s)-1));
}

int
StrLen(char* s)
{
  return (s==0 ? 0 : strlen(s));
}

void
StrClear(char* s)
{
  *s = 0;
}

char*
StrCpy(char* d, char* s)
{
  int i;
  if (StrLen(s) > StrSize(d)-1) {
    StrFree(d);
    d = StrAlloc(StrLen(s)+1);
  }
  for(i=0; s[i] != 0; i++)
    d[i] = s[i];
  d[i] = 0;
  return d;
}

char*
StrCat(char* d, char* s)
{
  int i, l;
  l = StrLen(d);
  if (StrLen(s)+l > StrSize(d)-1) {
    d = StrRealloc(d, StrLen(s)+l+1);
  }
  for(i=0; s[i] != 0; i++)
    d[l+i] = s[i];
  d[l+i] = 0;
  return d;
}

char*
StrInt(char* d, int i)
{
  int i, l;
  l = StrLen(d);
  if (l+2 > StrSize(d)-1) {
    d = StrRealloc(d, l+3);
  }
  sprintf(d+l, "%02d", i);
  return d;
}


/*
** Indentation stack functions.
*/
static int* IndentStack = 0;
static int  IndentStackSize = 0;
static int  IndentStackTop = 0;
static int  IndentLevel = 0;
static int  ExpectIndentFlag = 0;

void
PushIndent()
{
  if (IndentStack == 0) {
    IndentStack = (int*)malloc(10*sizeof(int));
    IndentStackSize = 10;
    assert(IndentStack != 0);
  }
  else if (IndentStackTop == IndentStackSize) {
    IndentStackSize = (IndentStackSize*3)/2;
    IndentStack = (int*)realloc(IndentStack, IndentStackSize*sizeof(int));
    assert(IndentStack != 0);
  }
  IndentStack[IndentStackTop] = IndentLevel;
  IndentStackTop++;
}

void
PopIndent()
{
  IndentStackTop--;
  assert(IndentStackTop >= 0);
}

void
SetIndentTop()
{
  if (IndentStackTop != 0)  PopIndent();
  PushIndent();
}

int
GetIndentTop()
{
  if (IndentStackTop != 0)  return IndentStack[IndentStackTop-1];
  else                      return 0;
}

void
SetIndent()
{
  int i;
  IndentLevel = 0;
  for (i = 0; i<yyleng && i<2; i++)  /* Skip leading "**". */
    if (yytext[i] == "*")  IndentLevel++;
  for (; i<yyleng && isspace(yytext[i]); i++) {
    if (yytext[i] == '\t')  IndentLevel = (IndentLevel/8+1)*8;
    else                    IndentLevel++;
  }
}

void
SetExpectIndent()
{
  ExpectIndentFlag = 1;
}

int
CheckIndent()
{
  int i = GetIndentTop();
  if (IndentLevel > i) {  /* Indent in */
    if (ExpectIndentFlag) {
      ExpectIndentFlag = 0;
      PushIndent();
      return 0;
    }
    else
      return 1;
  }
  else if (IndentLevel == i) {  /* Indent equal */
    if (ExpectIndentFlag) {
      ExpectIndentFlag = 0;
      return -1;
    }
    else
      return 0;
  }
  else {  /* Indent out */
    return -1;
  }
}


/*
** Text Output Functions
*/

static char* MathTText = 0;
static char* MathIText = 0;


void
BeginVERBATIM(int sc)
{
  TXTOUT("@example\n");
  RecursiveBegin(sc, TXT_VERBATIM);
}

void
EndVERBATIM()
{
  TXTOUT("@end example\n");
  RecursiveReturn();
}

void
BeginBOLD(int sc)
{
  TXTOUT("@strong{");
  RecursiveBegin(sc, TXT_BOLD);
}

void
EndBOLD()
{
  TXTOUT("}");
  RecursiveReturn();
}

void
BeginCODE(int sc)
{
  TXTOUT("@code{");
  RecursiveBegin(sc, TXT_CODE);
}

void
EndCODE()
{
  TXTOUT("}");
  RecursiveReturn();
}

void
BeginLIST(int sc)
{
  TXTOUT("@itemize @bullet\n@item");
  RecursiveBegin(sc, TXT_LIST);
}

void
ITEMOUT(void)
{
  TXTOUT("@item "
}

void
EndLIST()
{
  TXTOUT("@end itemize\n");
  RecursiveReturn();
}

void
BeginMATH(int sc)
{
  MathIText = StrCpy(MathIText, "$");
  MathTText = StrCpy(MathTText, "$");
  RecursiveBegin(sc, TXT_MATH);
}

void
EndMATH()
{
  MathIText = StrCpy(MathIText, "$");
  MathTText = StrCpy(MathTText, "$");
  TXTOUT("\n@tex\n");
  TXTOUT(MathTText);
  TXTOUT("\n@end tex\n@ifinfo\n");
  TXTOUT(MathIText);
  TXTOUT(\n@end ifinfo\n");
  RecursiveReturn();
}

void
BeginVAR(int sc)
{
  TXTOUT("@var{");
  RecursiveBegin(sc, TXT_VAR);
}

void
EndVAR()
{
  TXTOUT("}");
  RecursiveReturn();
}

%}
