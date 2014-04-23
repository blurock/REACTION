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
  int* ss = (int*)s;
  if (s == 0)
    return StrAlloc(size);
  if (*(ss-1) >= size)
    return s;
  ss = (int*)realloc(ss-1, size+sizeof(int));
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
  if (s != 0)  *s = 0;
}

char*
StrCpy(char* d, char* s)
{
  if (StrLen(s) > StrSize(d)-1) {
    StrFree(d);
    d = StrAlloc(StrLen(s)+1);
  }
  strcpy(d, s);
  return d;
}

char*
StrCat(char* d, char* s)
{
  int l;
  l = StrLen(d);
  if (StrLen(s)+l > StrSize(d)-1) {
    d = StrRealloc(d, StrLen(s)+l+1);
  }
  strcpy(&(d[l]), s);
  return d;
}

char*
StrInt(char* d, int i)
{
  int l;
  l = StrLen(d);
  if (l+8 > StrSize(d)-1) {
    d = StrRealloc(d, l+9);
  }
  sprintf(d+l, "%d", i);
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
ResetIndentStack()
{
  IndentStackTop = 0;
  ExpectIndentFlag = 0;
  IndentLevel = 0;
}

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
    if (yytext[i] == '*')  IndentLevel++;
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
      PushIndent();
      return -1;
    }
    else
      return 0;
  }
  else {  /* Indent out */
    if (ExpectIndentFlag) {
      ExpectIndentFlag = 0;
      PushIndent();
    }
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
  PushIndent();
  TXTOUT("@example\n");
  RecursiveBegin(sc, TXT_VERBATIM);
}

void
EndVERBATIM()
{
  PopIndent();
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
  TXTOUT("@itemize @bullet\n");
  RecursiveBegin(sc, TXT_LIST);
}

void
EndLIST()
{
  TXTOUT("@end itemize\n");
  RecursiveReturn();
}

void
ITEMOUT(void)
{
  TXTOUT("@item ");
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
  MathIText = StrCat(MathIText, "$");
  MathTText = StrCat(MathTText, "$");
  TXTOUT("\n@tex\n");
  TXTOUT(MathTText);
  TXTOUT("\n@end tex\n@ifinfo\n");
  TXTOUT(MathIText);
  TXTOUT("\n@end ifinfo\n");
  RecursiveReturn();
}

void
BeginMATHVAR(int sc)
{
  MathIText = StrCat(MathIText, "@var{");
  MathTText = StrCat(MathTText, "@var{");
}

void
EndMATHVAR()
{
  MathIText = StrCat(MathIText, "}");
  MathTText = StrCat(MathTText, "}");
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
