/*  FILE     scdef.l
**  PACKAGE  Maintenance
**  AUTHOR   Andreas Neubacher
**
**  CONTENT
**    Definitions for start condition handling.
**
**  REFERENCES
**    Based on Wolfgang Stoecher's previous implementation.
**
**  COPYRIGHT (C) 1995  STURM Project, RISC Linz
*/

%{
/*
** Start condition stack functions.
*/
static int* StartConditionStack = 0;
static int  StartConditionStackSize = 0;
static int  StartConditionStackTop = 0;

void
RecursiveBegin(int curr, int sc)
{
  if (StartConditionStack == 0) {
    StartConditionStack = (int*)malloc(10);
    StartConditionStackSize = 10;
    assert(StartConditionStack != 0);
  }
  else if (StartConditionStackTop == StartConditionStackSize) {
    StartConditionStackSize = (StartConditionStackSize*3)/2;
    StartConditionStack = (int*)realloc(StartConditionStack,
					StartConditionStackSize*sizeof(int));
    assert(StartConditionStack != 0);
  }
  StartConditionStack[StartConditionStackTop] = curr;
  StartConditionStack++;
  BEGIN(sc);
}

void
RecursiveReturn(void)
{
  assert(StartConditionTop > 0);
  StartConditionTop--;
  BEGIN(StartConditionStack[StartConditionTop]);
}

%}
